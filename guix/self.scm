;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix self)
  #:use-module (guix config)
  #:use-module (guix i18n)
  #:use-module (guix modules)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix discovery)
  #:use-module (guix packages)
  #:use-module (guix sets)
  #:use-module (guix modules)
  #:use-module ((guix build utils) #:select (find-files))
  #:use-module ((guix build compile) #:select (%lightweight-optimizations))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (make-config.scm
            whole-package                     ;for internal use in 'guix pull'
            compiled-guix
            guix-derivation))


;;;
;;; Dependency handling.
;;;

(define specification->package
  ;; Use our own variant of that procedure because that of (gnu packages)
  ;; would traverse all the .scm files, which is wasteful.
  (let ((ref (lambda (module variable)
               (module-ref (resolve-interface module) variable))))
    (match-lambda
      ("guile"      (ref '(gnu packages commencement) 'guile-final))
      ("guile-json" (ref '(gnu packages guile) 'guile-json))
      ("guile-ssh"  (ref '(gnu packages ssh)   'guile-ssh))
      ("guile-git"  (ref '(gnu packages guile) 'guile-git))
      ("guile-sqlite3" (ref '(gnu packages guile) 'guile-sqlite3))
      ("guile-gcrypt"  (ref '(gnu packages gnupg) 'guile-gcrypt))
      ("gnutls"     (ref '(gnu packages tls) 'gnutls))
      ("zlib"       (ref '(gnu packages compression) 'zlib))
      ("gzip"       (ref '(gnu packages compression) 'gzip))
      ("bzip2"      (ref '(gnu packages compression) 'bzip2))
      ("xz"         (ref '(gnu packages compression) 'xz))
      ("po4a"       (ref '(gnu packages gettext) 'po4a))
      ("gettext"       (ref '(gnu packages gettext) 'gettext-minimal))
      (_            #f))))                        ;no such package


;;;
;;; Derivations.
;;;

;; Node in a DAG of build tasks.  Each node maps to a derivation, but it's
;; easier to express things this way.
(define-record-type <node>
  (node name modules source dependencies compiled)
  node?
  (name          node-name)                       ;string
  (modules       node-modules)                    ;list of module names
  (source        node-source)                     ;list of source files
  (dependencies  node-dependencies)               ;list of nodes
  (compiled      node-compiled))                  ;node -> lowerable object

;; File mappings are essentially an alist as passed to 'imported-files'.
(define-record-type <file-mapping>
  (file-mapping name alist)
  file-mapping?
  (name  file-mapping-name)
  (alist file-mapping-alist))

(define-gexp-compiler (file-mapping-compiler (mapping <file-mapping>)
                                             system target)
  ;; Here we use 'imported-files', which can arrange to directly import all
  ;; the files instead of creating a derivation, when possible.
  (imported-files (map (match-lambda
                         ((destination (? local-file? file))
                          (cons destination
                                (local-file-absolute-file-name file)))
                         ((destination source)
                          (cons destination source))) ;silliness
                       (file-mapping-alist mapping))
                  #:name (file-mapping-name mapping)
                  #:system system))

(define (node-source+compiled node)
  "Return a \"bundle\" containing both the source code and object files for
NODE's modules, under their FHS directories: share/guile/site and lib/guile."
  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))

          (define source
            (string-append #$output "/share/guile/site/"
                           (effective-version)))

          (define object
            (string-append #$output "/lib/guile/" (effective-version)
                           "/site-ccache"))

          (mkdir-p (dirname source))
          (symlink #$(node-source node) source)
          (mkdir-p (dirname object))
          (symlink #$(node-compiled node) object))))

  (computed-file (string-append (node-name node) "-modules")
                 build))

(define (node-fold proc init nodes)
  (let loop ((nodes nodes)
             (visited (setq))
             (result init))
    (match nodes
      (() result)
      ((head tail ...)
       (if (set-contains? visited head)
           (loop tail visited result)
           (loop tail (set-insert head visited)
                 (proc head result)))))))

(define (node-modules/recursive nodes)
  (node-fold (lambda (node modules)
               (append (node-modules node) modules))
             '()
             nodes))

(define* (closure modules #:optional (except '()))
  (source-module-closure modules
                         #:select?
                         (match-lambda
                           (('guix 'config)
                            #f)
                           ((and module
                                 (or ('guix _ ...) ('gnu _ ...)))
                            (not (member module except)))
                           (rest #f))))

(define module->import
  ;; Return a file-name/file-like object pair for the specified module and
  ;; suitable for 'imported-files'.
  (match-lambda
    ((module '=> thing)
     (let ((file (module-name->file-name module)))
       (list file thing)))
    (module
        (let ((file (module-name->file-name module)))
          (list file
                (local-file (search-path %load-path file)))))))

(define* (scheme-node name modules #:optional (dependencies '())
                      #:key (extra-modules '()) (extra-files '())
                      (extensions '())
                      parallel? guile-for-build)
  "Return a node that builds the given Scheme MODULES, and depends on
DEPENDENCIES (a list of nodes).  EXTRA-MODULES is a list of additional modules
added to the source, and EXTRA-FILES is a list of additional files.
EXTENSIONS is a set of full-blown Guile packages (e.g., 'guile-json') that
must be present in the search path."
  (let* ((modules (append extra-modules
                          (closure modules
                                   (node-modules/recursive dependencies))))
         (module-files (map module->import modules))
         (source (file-mapping (string-append name "-source")
                               (append module-files extra-files))))
    (node name modules source dependencies
          (compiled-modules name source
                            (map car module-files)
                            (map node-source dependencies)
                            (map node-compiled dependencies)
                            #:extensions extensions
                            #:parallel? parallel?
                            #:guile-for-build guile-for-build))))

(define (file-imports directory sub-directory pred)
  "List all the files matching PRED under DIRECTORY/SUB-DIRECTORY.  Return a
list of file-name/file-like objects suitable as inputs to 'imported-files'."
  (map (lambda (file)
         (list (string-drop file (+ 1 (string-length directory)))
               (local-file file #:recursive? #t)))
       (find-files (string-append directory "/" sub-directory) pred)))

(define* (file-append* item file #:key (recursive? #t))
  "Return FILE within ITEM, which may be a file name or a file-like object.
When ITEM is a plain file name (a string), simply return a 'local-file'
record with the new file name."
  (match item
    ((? string?)
     ;; This is the optimal case: we return a new "source".  Thus, a
     ;; derivation that depends on this sub-directory does not depend on ITEM
     ;; itself.
     (local-file (string-append item "/" file)
                 #:recursive? recursive?))
    ;; TODO: Add 'local-file?' case.
    (_
     ;; In this case, anything that refers to the result also depends on ITEM,
     ;; which isn't great.
     (file-append item "/" file))))

(define* (locale-data source domain
                      #:optional (directory domain))
  "Return the locale data from 'po/DIRECTORY' in SOURCE, corresponding to
DOMAIN, a gettext domain."
  (define gettext
    (module-ref (resolve-interface '(gnu packages gettext))
                'gettext-minimal))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-26)
                       (ice-9 match) (ice-9 ftw))

          (define po-directory
            #+(file-append* source (string-append "po/" directory)))

          (define (compile language)
            (let ((gmo (string-append #$output "/" language "/LC_MESSAGES/"
                                      #$domain ".mo")))
              (mkdir-p (dirname gmo))
              (invoke #+(file-append gettext "/bin/msgfmt")
                      "-c" "--statistics" "--verbose"
                      "-o" gmo
                      (string-append po-directory "/" language ".po"))))

          (define (linguas)
            ;; Return the list of languages.  Note: don't read 'LINGUAS'
            ;; because it contains things like 'en@boldquot' that do not have
            ;; a corresponding .po file.
            (map (cut basename <> ".po")
                 (scandir po-directory
                          (cut string-suffix? ".po" <>))))

          (for-each compile (linguas)))))

  (computed-file (string-append "guix-locale-" domain)
                 build))

(define (translate-texi-manuals source)
  "Return the translated texinfo manuals built from SOURCE."
  (define po4a
    (specification->package "po4a"))
  
  (define gettext
    (specification->package "gettext"))

  (define glibc-utf8-locales
    (module-ref (resolve-interface '(gnu packages base))
                'glibc-utf8-locales))

  (define documentation
    (file-append* source "doc"))

  (define documentation-po
    (file-append* source "po/doc"))
  
  (define build
    (with-imported-modules '((guix build utils) (guix build po))
      #~(begin
          (use-modules (guix build utils) (guix build po)
                       (ice-9 match) (ice-9 regex) (ice-9 textual-ports)
                       (srfi srfi-1))

          (mkdir #$output)

          (copy-recursively #$documentation "."
                            #:log (%make-void-port "w"))

          (for-each
            (lambda (file)
              (copy-file file (basename file)))
            (find-files #$documentation-po ".*.po$"))

          (setenv "GUIX_LOCPATH"
                  #+(file-append glibc-utf8-locales "/lib/locale"))
          (setenv "PATH" #+(file-append gettext "/bin"))
          (setenv "LC_ALL" "en_US.UTF-8")
          (setlocale LC_ALL "en_US.UTF-8")

          (define (translate-tmp-texi po source output)
            "Translate Texinfo file SOURCE using messages from PO, and write
the result to OUTPUT."
            (invoke #+(file-append po4a "/bin/po4a-translate")
              "-M" "UTF-8" "-L" "UTF-8" "-k" "0" "-f" "texinfo"
              "-m" source "-p" po "-l" output))

          (define (make-ref-regex msgid end)
            (make-regexp (string-append
                           "ref\\{"
                           (string-join (string-split (regexp-quote msgid) #\ )
                                        "[ \n]+")
                           end)))

          (define (translate-cross-references content translations)
            "Take CONTENT, a string representing a .texi file and translate any
cross-reference in it (@ref, @xref and @pxref) that have a translation in
TRANSLATIONS, an alist of msgid and msgstr."
            (fold
              (lambda (elem content)
                (match elem
                  ((msgid . msgstr)
                   ;; Empty translations and strings containing some special characters
                   ;; cannot be the name of a section.
                   (if (or (equal? msgstr "")
                           (string-any (lambda (chr)
                                         (member chr '(#\{ #\} #\( #\) #\newline #\,)))
                                       msgid))
                       content
                       ;; Otherwise, they might be the name of a section, so we
                       ;; need to translate any occurence in @(p?x?)ref{...}.
                       (let ((regexp1 (make-ref-regex msgid ","))
                             (regexp2 (make-ref-regex msgid "\\}")))
                         (regexp-substitute/global
                           #f regexp2
                           (regexp-substitute/global
                             #f regexp1 content 'pre "ref{" msgstr "," 'post)
                           'pre "ref{" msgstr "}" 'post))))))
              content translations))
          
          (define (translate-texi po lang)
            "Translate the manual for one language LANG using the PO file."
            (let ((translations (call-with-input-file po read-po-file)))
              (translate-tmp-texi po "guix.texi"
                                  (string-append "guix." lang ".texi.tmp"))
              (translate-tmp-texi po "contributing.texi"
                                  (string-append "contributing." lang ".texi.tmp"))
              (let* ((texi-name (string-append "guix." lang ".texi"))
                     (tmp-name (string-append texi-name ".tmp")))
                (with-output-to-file texi-name
                  (lambda _
                    (format #t "~a"
                      (translate-cross-references
                        (call-with-input-file tmp-name get-string-all)
                        translations)))))
              (let* ((texi-name (string-append "contributing." lang ".texi"))
                     (tmp-name (string-append texi-name ".tmp")))
                (with-output-to-file texi-name
                  (lambda _
                    (format #t "~a"
                      (translate-cross-references
                        (call-with-input-file tmp-name get-string-all)
                        translations)))))))

          (for-each (lambda (po)
                      (match (reverse (string-split po #\.))
                        ((_ lang _ ...)
                         (translate-texi po lang))))
                    (find-files "." "^guix-manual\\.[a-z]{2}(_[A-Z]{2})?\\.po$"))

          (for-each
            (lambda (file)
              (copy-file file (string-append #$output "/" file)))
            (append
              (find-files "." "contributing\\..*\\.texi$")
              (find-files "." "guix\\..*\\.texi$"))))))

  (computed-file "guix-translated-texinfo" build))

(define (info-manual source)
  "Return the Info manual built from SOURCE."
  (define po4a
    (specification->package "po4a"))

  (define gettext
    (specification->package "gettext"))

  (define texinfo
    (module-ref (resolve-interface '(gnu packages texinfo))
                'texinfo))

  (define graphviz
    (module-ref (resolve-interface '(gnu packages graphviz))
                'graphviz))

  (define glibc-utf8-locales
    (module-ref (resolve-interface '(gnu packages base))
                'glibc-utf8-locales))

  (define documentation
    (file-append* source "doc"))

  (define examples
    (file-append* source "gnu/system/examples"))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))

          (mkdir #$output)

          ;; Create 'version.texi'.
          ;; XXX: Can we use a more meaningful version string yet one that
          ;; doesn't change at each commit?
          (call-with-output-file "version.texi"
            (lambda (port)
              (let ((version "0.0-git"))
                (format port "
@set UPDATED 1 January 1970
@set UPDATED-MONTH January 1970
@set EDITION ~a
@set VERSION ~a\n" version version))))

          ;; Copy configuration templates that the manual includes.
          (for-each (lambda (template)
                      (copy-file template
                                 (string-append
                                  "os-config-"
                                  (basename template ".tmpl")
                                  ".texi")))
                    (find-files #$examples "\\.tmpl$"))

          ;; Build graphs.
          (mkdir-p (string-append #$output "/images"))
          (for-each (lambda (dot-file)
                      (invoke #+(file-append graphviz "/bin/dot")
                              "-Tpng" "-Gratio=.9" "-Gnodesep=.005"
                              "-Granksep=.00005" "-Nfontsize=9"
                              "-Nheight=.1" "-Nwidth=.1"
                              "-o" (string-append #$output "/images/"
                                                  (basename dot-file ".dot")
                                                  ".png")
                              dot-file))
                    (find-files (string-append #$documentation "/images")
                                "\\.dot$"))

          ;; Copy other PNGs.
          (for-each (lambda (png-file)
                      (install-file png-file
                                    (string-append #$output "/images")))
                    (find-files (string-append #$documentation "/images")
                                "\\.png$"))

          ;; Finally build the manual.  Copy it the Texinfo files to $PWD and
          ;; add a symlink to the 'images' directory so that 'makeinfo' can
          ;; see those images and produce image references in the Info output.
          (copy-recursively #$documentation "."
                            #:log (%make-void-port "w"))
          (copy-recursively #+(translate-texi-manuals source) "."
                            #:log (%make-void-port "w"))
          (delete-file-recursively "images")
          (symlink (string-append #$output "/images") "images")

          ;; Provide UTF-8 locales needed by the 'xspara.c' code in makeinfo.
          (setenv "GUIX_LOCPATH"
                  #+(file-append glibc-utf8-locales "/lib/locale"))

          (for-each (lambda (texi)
                      (unless (string=? "guix.texi" texi)
                        ;; Create 'version-LL.texi'.
                        (let* ((base (basename texi ".texi"))
                               (dot  (string-index base #\.))
                               (tag  (string-drop base (+ 1 dot))))
                          (symlink "version.texi"
                                   (string-append "version-" tag ".texi"))))

                      (invoke #+(file-append texinfo "/bin/makeinfo")
                              texi "-I" #$documentation
                              "-I" "."
                              "-o" (string-append #$output "/"
                                                  (basename texi ".texi")
                                                  ".info")))
                    (cons "guix.texi"
                          (find-files "." "^guix\\.[a-z]{2}(_[A-Z]{2})?\\.texi$")))

          ;; Compress Info files.
          (setenv "PATH"
                  #+(file-append (specification->package "gzip") "/bin"))
          (for-each (lambda (file)
                      (invoke "gzip" "-9n" file))
                    (find-files #$output "\\.info(-[0-9]+)?$")))))

  (computed-file "guix-manual" build))

(define* (guile-module-union things #:key (name "guix-module-union"))
  "Return the union of the subset of THINGS (packages, computed files, etc.)
that provide Guile modules."
  (define build
    (with-imported-modules '((guix build union))
      #~(begin
          (use-modules (guix build union))

          (define (modules directory)
            (string-append directory "/share/guile/site"))

          (define (objects directory)
            (string-append directory "/lib/guile"))

          (union-build #$output
                       (filter (lambda (directory)
                                 (or (file-exists? (modules directory))
                                     (file-exists? (objects directory))))
                               '#$things)

                       #:log-port (%make-void-port "w")))))

  (computed-file name build))

(define* (guix-command modules
                       #:key source (dependencies '())
                       guile (guile-version (effective-version)))
  "Return the 'guix' command such that it adds MODULES and DEPENDENCIES in its
load path."
  (define glibc-utf8-locales
    (module-ref (resolve-interface '(gnu packages base))
                'glibc-utf8-locales))

  (define module-directory
    ;; To minimize the number of 'stat' calls needed to locate a module,
    ;; create the union of all the module directories.
    (guile-module-union (cons modules dependencies)))

  (program-file "guix-command"
                #~(begin
                    (set! %load-path
                      (cons (string-append #$module-directory
                                           "/share/guile/site/"
                                           (effective-version))
                            %load-path))

                    (set! %load-compiled-path
                      (cons (string-append #$module-directory
                                           "/lib/guile/"
                                           (effective-version)
                                           "/site-ccache")
                            %load-compiled-path))

                    ;; To maximize the chances that locales are set up right
                    ;; out-of-the-box, bundle "common" UTF-8 locales.
                    (let ((locpath (getenv "GUIX_LOCPATH")))
                      (setenv "GUIX_LOCPATH"
                              (string-append (if locpath
                                                 (string-append locpath ":")
                                                 "")
                                             #$(file-append glibc-utf8-locales
                                                            "/lib/locale"))))

                    (let ((guix-main (module-ref (resolve-interface '(guix ui))
                                                 'guix-main)))
                      #$(if source
                            #~(begin
                                (bindtextdomain "guix"
                                                #$(locale-data source "guix"))
                                (bindtextdomain "guix-packages"
                                                #$(locale-data source
                                                               "guix-packages"
                                                               "packages")))
                            #t)

                      ;; XXX: It would be more convenient to change it to:
                      ;;   (exit (apply guix-main (command-line)))
                      (apply guix-main (command-line))))
                #:guile guile))

(define (miscellaneous-files source)
  "Return data files taken from SOURCE."
  (file-mapping "guix-misc"
                `(("etc/bash_completion.d/guix"
                   ,(file-append* source "/etc/completion/bash/guix"))
                  ("etc/bash_completion.d/guix-daemon"
                   ,(file-append* source "/etc/completion/bash/guix-daemon"))
                  ("share/zsh/site-functions/_guix"
                   ,(file-append* source "/etc/completion/zsh/_guix"))
                  ("share/fish/vendor_completions.d/guix.fish"
                   ,(file-append* source "/etc/completion/fish/guix.fish"))
                  ("share/guix/hydra.gnu.org.pub"
                   ,(file-append* source
                                  "/etc/substitutes/hydra.gnu.org.pub"))
                  ("share/guix/berlin.guixsd.org.pub"
                   ,(file-append* source
                                  "/etc/substitutes/berlin.guixsd.org.pub"))
                  ("share/guix/ci.guix.info.pub"  ;alias
                   ,(file-append* source "/etc/substitutes/berlin.guixsd.org.pub")))))

(define* (whole-package name modules dependencies
                        #:key
                        (guile-version (effective-version))
                        info daemon miscellany
                        guile
                        (command (guix-command modules
                                               #:dependencies dependencies
                                               #:guile guile
                                               #:guile-version guile-version)))
  "Return the whole Guix package NAME that uses MODULES, a derivation of all
the modules (under share/guile/site and lib/guile), and DEPENDENCIES, a list
of packages depended on.  COMMAND is the 'guix' program to use; INFO is the
Info manual."
  (define (wrap daemon)
    (program-file "guix-daemon"
                  #~(begin
                      (setenv "GUIX" #$command)
                      (apply execl #$(file-append daemon "/bin/guix-daemon")
                             "guix-daemon" (cdr (command-line))))))

  (computed-file name
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (guix build utils))

                       (define daemon
                         #$(and daemon (wrap daemon)))

                       (mkdir-p (string-append #$output "/bin"))
                       (symlink #$command
                                (string-append #$output "/bin/guix"))

                       (when daemon
                         (symlink daemon
                                  (string-append #$output "/bin/guix-daemon")))

                       (let ((share (string-append #$output "/share"))
                             (lib   (string-append #$output "/lib"))
                             (info  #$info))
                         (mkdir-p share)
                         (symlink #$(file-append modules "/share/guile")
                                  (string-append share "/guile"))
                         (when info
                           (symlink #$info (string-append share "/info")))

                         (mkdir-p lib)
                         (symlink #$(file-append modules "/lib/guile")
                                  (string-append lib "/guile")))

                       (when #$miscellany
                         (copy-recursively #$miscellany #$output
                                           #:log (%make-void-port "w")))))))

(define* (compiled-guix source #:key (version %guix-version)
                        (pull-version 1)
                        (name (string-append "guix-" version))
                        (guile-version (effective-version))
                        (guile-for-build (default-guile))
                        (zlib (specification->package "zlib"))
                        (gzip (specification->package "gzip"))
                        (bzip2 (specification->package "bzip2"))
                        (xz (specification->package "xz"))
                        (guix (specification->package "guix")))
  "Return a file-like object that contains a compiled Guix."
  (define guile-json
    (specification->package "guile-json"))

  (define guile-ssh
    (specification->package "guile-ssh"))

  (define guile-git
    (specification->package "guile-git"))

  (define guile-sqlite3
    (specification->package "guile-sqlite3"))

  (define guile-gcrypt
    (specification->package "guile-gcrypt"))

  (define gnutls
    (specification->package "gnutls"))

  (define dependencies
    (match (append-map (lambda (package)
                         (cons (list "x" package)
                               (package-transitive-propagated-inputs package)))
                       (list guile-gcrypt gnutls guile-git guile-json
                             guile-ssh guile-sqlite3))
      (((labels packages _ ...) ...)
       packages)))

  (define *core-modules*
    (scheme-node "guix-core"
                 '((guix)
                   (guix monad-repl)
                   (guix packages)
                   (guix download)
                   (guix discovery)
                   (guix profiles)
                   (guix build-system gnu)
                   (guix build-system trivial)
                   (guix build profiles)
                   (guix build gnu-build-system))

                 ;; Provide a dummy (guix config) with the default version
                 ;; number, storedir, etc.  This is so that "guix-core" is the
                 ;; same across all installations and doesn't need to be
                 ;; rebuilt when the version changes, which in turn means we
                 ;; can have substitutes for it.
                 #:extra-modules
                 `(((guix config) => ,(make-config.scm)))

                 ;; (guix man-db) is needed at build-time by (guix profiles)
                 ;; but we don't need to compile it; not compiling it allows
                 ;; us to avoid an extra dependency on guile-gdbm-ffi.
                 #:extra-files
                 `(("guix/man-db.scm" ,(local-file "../guix/man-db.scm"))
                   ("guix/build/po.scm" ,(local-file "../guix/build/po.scm"))
                   ("guix/store/schema.sql"
                    ,(local-file "../guix/store/schema.sql")))

                 #:extensions (list guile-gcrypt)
                 #:guile-for-build guile-for-build))

  (define *extra-modules*
    (scheme-node "guix-extra"
                 (filter-map (match-lambda
                               (('guix 'scripts _ ..1) #f)
                               (('guix 'man-db) #f)
                               (name name))
                             (scheme-modules* source "guix"))
                 (list *core-modules*)
                 #:extensions dependencies
                 #:guile-for-build guile-for-build))

  (define *core-package-modules*
    (scheme-node "guix-packages-base"
                 `((gnu packages)
                   (gnu packages base))
                 (list *core-modules* *extra-modules*)
                 #:extensions dependencies

                 ;; Add all the non-Scheme files here.  We must do it here so
                 ;; that 'search-patches' & co. can find them.  Ideally we'd
                 ;; keep them next to the .scm files that use them but it's
                 ;; difficult to do (XXX).
                 #:extra-files
                 (file-imports source "gnu/packages"
                               (lambda (file stat)
                                 (and (eq? 'regular (stat:type stat))
                                      (not (string-suffix? ".scm" file))
                                      (not (string-suffix? ".go" file))
                                      (not (string-prefix? ".#" file))
                                      (not (string-suffix? "~" file)))))
                 #:guile-for-build guile-for-build))

  (define *package-modules*
    (scheme-node "guix-packages"
                 (scheme-modules* source "gnu/packages")
                 (list *core-modules* *extra-modules* *core-package-modules*)
                 #:extensions dependencies
                 #:guile-for-build guile-for-build))

  (define *system-modules*
    (scheme-node "guix-system"
                 `((gnu system)
                   (gnu services)
                   ,@(scheme-modules* source "gnu/bootloader")
                   ,@(scheme-modules* source "gnu/system")
                   ,@(scheme-modules* source "gnu/services"))
                 (list *core-package-modules* *package-modules*
                       *extra-modules* *core-modules*)
                 #:extensions dependencies
                 #:extra-files
                 (append (file-imports source "gnu/system/examples"
                                       (const #t))

                         ;; All the installer code is on the build-side.
                         (file-imports source "gnu/installer/"
                                       (const #t))
                         ;; Build-side code that we don't build.  Some of
                         ;; these depend on guile-rsvg, the Shepherd, etc.
                         (file-imports source "gnu/build" (const #t)))
                 #:guile-for-build
                 guile-for-build))

  (define *cli-modules*
    (scheme-node "guix-cli"
                 (append (scheme-modules* source "/guix/scripts")
                         `((gnu ci)))
                 (list *core-modules* *extra-modules*
                       *core-package-modules* *package-modules*
                       *system-modules*)
                 #:extensions dependencies
                 #:guile-for-build guile-for-build))

  (define *system-test-modules*
    ;; Ship these modules mostly so (gnu ci) can discover them.
    (scheme-node "guix-system-tests"
                 `((gnu tests)
                   ,@(scheme-modules* source "gnu/tests"))
                 (list *core-package-modules* *package-modules*
                       *extra-modules* *system-modules* *core-modules*
                       *cli-modules*)           ;for (guix scripts pack), etc.
                 #:extensions dependencies
                 #:guile-for-build guile-for-build))

  (define *config*
    (scheme-node "guix-config"
                 '()
                 #:extra-modules
                 `(((guix config)
                    => ,(make-config.scm #:zlib zlib
                                         #:gzip gzip
                                         #:bzip2 bzip2
                                         #:xz xz
                                         #:package-name
                                         %guix-package-name
                                         #:package-version
                                         version
                                         #:bug-report-address
                                         %guix-bug-report-address
                                         #:home-page-url
                                         %guix-home-page-url)))
                 #:guile-for-build guile-for-build))

  (define (built-modules node-subset)
    (directory-union (string-append name "-modules")
                     (append-map node-subset

                                 ;; Note: *CONFIG* comes first so that it
                                 ;; overrides the (guix config) module that
                                 ;; comes with *CORE-MODULES*.
                                 (list *config*
                                       *cli-modules*
                                       *system-test-modules*
                                       *system-modules*
                                       *package-modules*
                                       *core-package-modules*
                                       *extra-modules*
                                       *core-modules*))

                     ;; Silently choose the first entry upon collision so that
                     ;; we choose *CONFIG*.
                     #:resolve-collision 'first

                     ;; When we do (add-to-store "utils.scm"), "utils.scm" must
                     ;; be a regular file, not a symlink.  Thus, arrange so that
                     ;; regular files appear as regular files in the final
                     ;; output.
                     #:copy? #t
                     #:quiet? #t))

  ;; Version 0 of 'guix pull' meant we'd just return Scheme modules.
  ;; Version 1 is when we return the full package.
  (cond ((= 1 pull-version)
         ;; The whole package, with a standard file hierarchy.
         (let* ((modules  (built-modules (compose list node-source+compiled)))
                (command  (guix-command modules
                                        #:source source
                                        #:dependencies dependencies
                                        #:guile guile-for-build
                                        #:guile-version guile-version)))
           (whole-package name modules dependencies
                          #:command command
                          #:guile guile-for-build

                          ;; Include 'guix-daemon'.  XXX: Here we inject an
                          ;; older snapshot of guix-daemon, but that's a good
                          ;; enough approximation for now.
                          #:daemon (module-ref (resolve-interface
                                                '(gnu packages
                                                      package-management))
                                               'guix-daemon)

                          #:info (info-manual source)
                          #:miscellany (miscellaneous-files source)
                          #:guile-version guile-version)))
        ((= 0 pull-version)
         ;; Legacy 'guix pull': return the .scm and .go files as one
         ;; directory.
         (built-modules (lambda (node)
                          (list (node-source node)
                                (node-compiled node)))))
        (else
         ;; Unsupported 'guix pull' version.
         #f)))


;;;
;;; Generating (guix config).
;;;

(define %persona-variables
  ;; (guix config) variables that define Guix's persona.
  '(%guix-package-name
    %guix-version
    %guix-bug-report-address
    %guix-home-page-url))

(define %config-variables
  ;; (guix config) variables corresponding to Guix configuration.
  (letrec-syntax ((variables (syntax-rules ()
                               ((_)
                                '())
                               ((_ variable rest ...)
                                (cons `(variable . ,variable)
                                      (variables rest ...))))))
    (variables %localstatedir %storedir %sysconfdir)))

(define* (make-config.scm #:key zlib gzip xz bzip2
                          (package-name "GNU Guix")
                          (package-version "0")
                          (bug-report-address "bug-guix@gnu.org")
                          (home-page-url "https://gnu.org/s/guix"))

  ;; Hack so that Geiser is not confused.
  (define defmod 'define-module)

  (scheme-file "config.scm"
               #~(;; The following expressions get spliced.
                   (#$defmod (guix config)
                     #:export (%guix-package-name
                               %guix-version
                               %guix-bug-report-address
                               %guix-home-page-url
                               %system
                               %store-directory
                               %state-directory
                               %store-database-directory
                               %config-directory
                               %libz
                               %gzip
                               %bzip2
                               %xz))

                   (define %system
                     #$(%current-system))

                   #$@(map (match-lambda
                             ((name . value)
                              #~(define-public #$name #$value)))
                           %config-variables)

                   (define %store-directory
                     (or (and=> (getenv "NIX_STORE_DIR") canonicalize-path)
                         %storedir))

                   (define %state-directory
                     ;; This must match `NIX_STATE_DIR' as defined in
                     ;; `nix/local.mk'.
                     (or (getenv "GUIX_STATE_DIRECTORY")
                         (string-append %localstatedir "/guix")))

                   (define %store-database-directory
                     (or (getenv "GUIX_DATABASE_DIRECTORY")
                         (string-append %state-directory "/db")))

                   (define %config-directory
                     ;; This must match `GUIX_CONFIGURATION_DIRECTORY' as
                     ;; defined in `nix/local.mk'.
                     (or (getenv "GUIX_CONFIGURATION_DIRECTORY")
                         (string-append %sysconfdir "/guix")))

                   (define %guix-package-name #$package-name)
                   (define %guix-version #$package-version)
                   (define %guix-bug-report-address #$bug-report-address)
                   (define %guix-home-page-url #$home-page-url)

                   (define %gzip
                     #+(and gzip (file-append gzip "/bin/gzip")))
                   (define %bzip2
                     #+(and bzip2 (file-append bzip2 "/bin/bzip2")))
                   (define %xz
                     #+(and xz (file-append xz "/bin/xz")))

                   (define %libz
                     #+(and zlib
                            (file-append zlib "/lib/libz"))))

               ;; Guile 2.0 *requires* the 'define-module' to be at the
               ;; top-level or the 'toplevel-ref' in the resulting .go file are
               ;; made relative to a nonexistent anonymous module.
               #:splice? #t))


;;;
;;; Building.
;;;

(define* (compiled-modules name module-tree module-files
                           #:optional
                           (dependencies '())
                           (dependencies-compiled '())
                           #:key
                           (extensions '())       ;full-blown Guile packages
                           parallel?
                           guile-for-build)
  "Build all the MODULE-FILES from MODULE-TREE.  MODULE-FILES must be a list
like '(\"guix/foo.scm\" \"gnu/bar.scm\") and MODULE-TREE is the directory
containing MODULE-FILES and possibly other files as well."
  ;; This is a non-monadic, enhanced version of 'compiled-file' from (guix
  ;; gexp).
  (define build
    (with-imported-modules (source-module-closure
                            '((guix build compile)
                              (guix build utils)))
      #~(begin
          (use-modules (srfi srfi-26)
                       (ice-9 match)
                       (ice-9 format)
                       (ice-9 threads)
                       (guix build compile)
                       (guix build utils))

          (define (regular? file)
            (not (member file '("." ".."))))

          (define (report-load file total completed)
            (display #\cr)
            (format #t
                    "[~3@a/~3@a] loading...\t~5,1f% of ~d files"

                    ;; Note: Multiply TOTAL by two to account for the
                    ;; compilation phase that follows.
                    completed (* total 2)

                    (* 100. (/ completed total)) total)
            (force-output))

          (define (report-compilation file total completed)
            (display #\cr)
            (format #t "[~3@a/~3@a] compiling...\t~5,1f% of ~d files"

                    ;; Add TOTAL to account for the load phase that came
                    ;; before.
                    (+ total completed) (* total 2)

                    (* 100. (/ completed total)) total)
            (force-output))

          (define (process-directory directory files output)
            ;; Hide compilation warnings.
            (parameterize ((current-warning-port (%make-void-port "w")))
              (compile-files directory #$output files
                             #:workers (parallel-job-count)
                             #:report-load report-load
                             #:report-compilation report-compilation)))

          (setvbuf (current-output-port) 'line)
          (setvbuf (current-error-port) 'line)

          (set! %load-path (cons #+module-tree %load-path))
          (set! %load-path
            (append '#+dependencies
                    (map (lambda (extension)
                           (string-append extension "/share/guile/site/"
                                          (effective-version)))
                         '#+extensions)
                    %load-path))

          (set! %load-compiled-path
            (append '#+dependencies-compiled
                    (map (lambda (extension)
                           (string-append extension "/lib/guile/"
                                          (effective-version)
                                          "/site-ccache"))
                         '#+extensions)
                    %load-compiled-path))

          ;; Load the compiler modules upfront.
          (compile #f)

          (mkdir #$output)
          (chdir #+module-tree)
          (process-directory "." '#+module-files #$output)
          (newline))))

  (computed-file name build
                 #:guile guile-for-build
                 #:options
                 `(#:local-build? #f              ;allow substitutes

                   ;; Don't annoy people about _IONBF deprecation.
                   ;; Initialize 'terminal-width' in (system repl debug)
                   ;; to a large-enough value to make backtrace more
                   ;; verbose.
                   #:env-vars (("GUILE_WARN_DEPRECATED" . "no")
                               ("COLUMNS" . "200")))))


;;;
;;; Building.
;;;

(define* (guix-derivation source version
                          #:optional (guile-version (effective-version))
                          #:key (pull-version 0))
  "Return, as a monadic value, the derivation to build the Guix from SOURCE
for GUILE-VERSION.  Use VERSION as the version string.  PULL-VERSION specifies
the version of the 'guix pull' protocol.  Return #f if this PULL-VERSION value
is not supported."
  (define (shorten version)
    (if (and (string-every char-set:hex-digit version)
             (> (string-length version) 9))
        (string-take version 9)                   ;Git commit
        version))

  (define guile
    ;; When PULL-VERSION >= 1, produce a self-contained Guix and use Guile 2.2
    ;; unconditionally.
    (default-guile))

  (when (and (< pull-version 1)
             (not (string=? (package-version guile) guile-version)))
    ;; Guix < 0.15.0 has PULL-VERSION = 0, where the host Guile is reused and
    ;; can be any version.  When that happens and Guile is not current (e.g.,
    ;; it's Guile 2.0), just bail out.
    (raise (condition
            (&message
             (message "Guix is too old and cannot be upgraded")))))

  (mbegin %store-monad
    (set-guile-for-build guile)
    (let ((guix (compiled-guix source
                               #:version version
                               #:name (string-append "guix-"
                                                     (shorten version))
                               #:pull-version pull-version
                               #:guile-version (if (>= pull-version 1)
                                                   "2.2" guile-version)
                               #:guile-for-build guile)))
      (if guix
          (lower-object guix)
          (return #f)))))
