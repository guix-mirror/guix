;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
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

(define-module (guix scripts pack)
  #:use-module (guix scripts)
  #:use-module (guix ui)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (guix grafts)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system gnu)
  #:use-module (guix scripts build)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages base)
  #:autoload   (gnu packages package-management) (guix)
  #:autoload   (gnu packages gnupg) (libgcrypt)
  #:autoload   (gnu packages guile) (guile2.0-json guile-json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (compressor?
            lookup-compressor
            self-contained-tarball
            guix-pack))

;; Type of a compression tool.
(define-record-type <compressor>
  (compressor name extension command)
  compressor?
  (name       compressor-name)      ;string (e.g., "gzip")
  (extension  compressor-extension) ;string (e.g., ".lz")
  (command    compressor-command))  ;gexp (e.g., #~("/gnu/store/…/gzip" "-9n"))

(define %compressors
  ;; Available compression tools.
  (list (compressor "gzip"  ".gz"
                    #~(#+(file-append gzip "/bin/gzip") "-9n"))
        (compressor "lzip"  ".lz"
                    #~(#+(file-append lzip "/bin/lzip") "-9"))
        (compressor "xz"    ".xz"
                    #~(#+(file-append xz "/bin/xz") "-e -T0"))
        (compressor "bzip2" ".bz2"
                    #~(#+(file-append bzip2 "/bin/bzip2") "-9"))
        (compressor "none" "" #f)))

;; This one is only for use in this module, so don't put it in %compressors.
(define bootstrap-xz
  (compressor "bootstrap-xz" ".xz"
              #~(#+(file-append %bootstrap-coreutils&co "/bin/xz") "-e -T0")))

(define (lookup-compressor name)
  "Return the compressor object called NAME.  Error out if it could not be
found."
  (or (find (match-lambda
              (($ <compressor> name*)
               (string=? name* name)))
            %compressors)
      (leave (G_ "~a: compressor not found~%") name)))

(define* (self-contained-tarball name profile
                                 #:key target
                                 deduplicate?
                                 (compressor (first %compressors))
                                 localstatedir?
                                 (symlinks '())
                                 (tar tar))
  "Return a self-contained tarball containing a store initialized with the
closure of PROFILE, a derivation.  The tarball contains /gnu/store; if
LOCALSTATEDIR? is true, it also contains /var/guix, including /var/guix/db
with a properly initialized store database.

SYMLINKS must be a list of (SOURCE -> TARGET) tuples denoting symlinks to be
added to the pack."
  (define build
    (with-imported-modules (source-module-closure
                            '((guix build utils)
                              (guix build union)
                              (guix build store-copy)
                              (gnu build install)))
      #~(begin
          (use-modules (guix build utils)
                       ((guix build union) #:select (relative-file-name))
                       (gnu build install)
                       (srfi srfi-1)
                       (srfi srfi-26)
                       (ice-9 match))

          (define %root "root")

          (define symlink->directives
            ;; Return "populate directives" to make the given symlink and its
            ;; parent directories.
            (match-lambda
              ((source '-> target)
               (let ((target (string-append #$profile "/" target)))
                 `((directory ,(dirname source))
                   (,source
                    -> ,(relative-file-name (dirname source) target)))))))

          (define directives
            ;; Fully-qualified symlinks.
            (append-map symlink->directives '#$symlinks))

          ;; The --sort option was added to GNU tar in version 1.28, released
          ;; 2014-07-28.  For testing, we use the bootstrap tar, which is
          ;; older and doesn't support it.
          (define tar-supports-sort?
            (zero? (system* (string-append #+tar "/bin/tar")
                            "cf" "/dev/null" "--files-from=/dev/null"
                            "--sort=name")))

          ;; We need Guix here for 'guix-register'.
          (setenv "PATH"
                  (string-append #$(if localstatedir?
                                       (file-append guix "/sbin:")
                                       "")
                                 #$tar "/bin"))

          ;; Note: there is not much to gain here with deduplication and
          ;; there is the overhead of the '.links' directory, so turn it
          ;; off.
          (populate-single-profile-directory %root
                                             #:profile #$profile
                                             #:closure "profile"
                                             #:deduplicate? #f
                                             #:register? #$localstatedir?)

          ;; Create SYMLINKS.
          (for-each (cut evaluate-populate-directive <> %root)
                    directives)

          ;; Create the tarball.  Use GNU format so there's no file name
          ;; length limitation.
          (with-directory-excursion %root
            (exit
             (zero? (apply system* "tar"
                           "-I"
                           (string-join '#+(compressor-command compressor))
                           "--format=gnu"

                           ;; Avoid non-determinism in the archive.  Use
                           ;; mtime = 1, not zero, because that is what the
                           ;; daemon does for files in the store (see the
                           ;; 'mtimeStore' constant in local-store.cc.)
                           (if tar-supports-sort? "--sort=name" "--mtime=@1")
                           "--mtime=@1"           ;for files in /var/guix
                           "--owner=root:0"
                           "--group=root:0"

                           "--check-links"
                           "-cvf" #$output
                           ;; Avoid adding / and /var to the tarball, so
                           ;; that the ownership and permissions of those
                           ;; directories will not be overwritten when
                           ;; extracting the archive.  Do not include /root
                           ;; because the root account might have a
                           ;; different home directory.
                           #$@(if localstatedir?
                                  '("./var/guix")
                                  '())

                           (string-append "." (%store-directory))

                           (delete-duplicates
                            (filter-map (match-lambda
                                          (('directory directory)
                                           (string-append "." directory))
                                          (_ #f))
                                        directives)))))))))

  (gexp->derivation (string-append name ".tar"
                                   (compressor-extension compressor))
                    build
                    #:references-graphs `(("profile" ,profile))))

(define* (docker-image name profile
                       #:key target
                       deduplicate?
                       (compressor (first %compressors))
                       localstatedir?
                       (symlinks '())
                       (tar tar))
  "Return a derivation to construct a Docker image of PROFILE.  The
image is a tarball conforming to the Docker Image Specification, compressed
with COMPRESSOR.  It can be passed to 'docker load'.  If TARGET is true, it
must a be a GNU triplet and it is used to derive the architecture metadata in
the image."
  ;; FIXME: Honor LOCALSTATEDIR?.
  (define not-config?
    (match-lambda
      (('guix 'config) #f)
      (('guix rest ...) #t)
      (('gnu rest ...) #t)
      (rest #f)))

  (define defmod 'define-module)                  ;trick Geiser

  (define config
    ;; (guix config) module for consumption by (guix gcrypt).
    (scheme-file "gcrypt-config.scm"
                 #~(begin
                     (#$defmod (guix config)
                       #:export (%libgcrypt))

                     ;; XXX: Work around <http://bugs.gnu.org/15602>.
                     (eval-when (expand load eval)
                       (define %libgcrypt
                         #+(file-append libgcrypt "/lib/libgcrypt"))))))

  (define json
    ;; Pick the guile-json package that corresponds to the Guile used to build
    ;; derivations.
    (if (string-prefix? "2.0" (package-version (default-guile)))
        guile2.0-json
        guile-json))

  (define build
    (with-imported-modules `(,@(source-module-closure '((guix docker))
                                                      #:select? not-config?)
                             (guix build store-copy)
                             ((guix config) => ,config))
      #~(begin
          ;; Guile-JSON is required by (guix docker).
          (add-to-load-path
           (string-append #+json "/share/guile/site/"
                          (effective-version)))

          (use-modules (guix docker) (srfi srfi-19) (guix build store-copy))

          (setenv "PATH" (string-append #$tar "/bin"))

          (build-docker-image #$output
                              (call-with-input-file "profile"
                                read-reference-graph)
                              #$profile
                              #:system (or #$target (utsname:machine (uname)))
                              #:symlinks '#$symlinks
                              #:compressor '#$(compressor-command compressor)
                              #:creation-time (make-time time-utc 0 1)))))

  (gexp->derivation (string-append name ".tar"
                                   (compressor-extension compressor))
                    build
                    #:references-graphs `(("profile" ,profile))))


;;;
;;; Compiling C programs.
;;;

;; A C compiler.  That lowers to a single program that can be passed typical C
;; compiler flags, and it makes sure the whole toolchain is available.
(define-record-type <c-compiler>
  (%c-compiler toolchain guile)
  c-compiler?
  (toolchain c-compiler-toolchain)
  (guile     c-compiler-guile))

(define* (c-compiler #:optional inputs
                     #:key (guile (default-guile)))
  (%c-compiler inputs guile))

(define (bootstrap-c-compiler)
  "Return the C compiler that uses the bootstrap toolchain.  This is used only
by '--bootstrap', for testing purposes."
  (define bootstrap-toolchain
    (list (first (assoc-ref %bootstrap-inputs "gcc"))
          (first (assoc-ref %bootstrap-inputs "binutils"))
          (first (assoc-ref %bootstrap-inputs "libc"))))

  (c-compiler bootstrap-toolchain
              #:guile %bootstrap-guile))

(define-gexp-compiler (c-compiler-compiler (compiler <c-compiler>) system target)
  "Lower COMPILER to a single script that does the right thing."
  (define toolchain
    (or (c-compiler-toolchain compiler)
        (list (first (assoc-ref (standard-packages) "gcc"))
              (first (assoc-ref (standard-packages) "ld-wrapper"))
              (first (assoc-ref (standard-packages) "binutils"))
              (first (assoc-ref (standard-packages) "libc"))
              (gexp-input (first (assoc-ref (standard-packages) "libc"))
                          "static"))))

  (define inputs
    (match (append-map package-propagated-inputs
                       (filter package? toolchain))
      (((labels things . _) ...)
       (append toolchain things))))

  (define search-paths
    (cons $PATH
          (append-map package-native-search-paths
                      (filter package? inputs))))

  (define run
    (with-imported-modules (source-module-closure
                            '((guix build utils)
                              (guix search-paths)))
      #~(begin
          (use-modules (guix build utils) (guix search-paths)
                       (ice-9 match))

          (define (output-file args)
            (let loop ((args args))
              (match args
                (() "a.out")
                (("-o" file _ ...) file)
                ((head rest ...) (loop rest)))))

          (set-search-paths (map sexp->search-path-specification
                                 '#$(map search-path-specification->sexp
                                         search-paths))
                            '#$inputs)

          (let ((output (output-file (command-line))))
            (apply invoke "gcc" (cdr (command-line)))
            (invoke "strip" output)))))

  (when target
    ;; TODO: Yep, we'll have to do it someday!
    (leave (G_ "cross-compilation not implemented here;
please email '~a'~%")
           (@ (guix config) %guix-bug-report-address)))

  (gexp->script "c-compiler" run
                #:guile (c-compiler-guile compiler)))


;;;
;;; Wrapped package.
;;;

(define* (wrapped-package package
                          #:optional (compiler (c-compiler)))
  (define runner
    (local-file (search-auxiliary-file "run-in-namespace.c")))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 match))

          (define (strip-store-prefix file)
            ;; Given a file name like "/gnu/store/…-foo-1.2/bin/foo", return
            ;; "/bin/foo".
            (let* ((len  (string-length (%store-directory)))
                   (base (string-drop file (+ 1 len))))
              (match (string-index base #\/)
                (#f    base)
                (index (string-drop base index)))))

          (define (build-wrapper program)
            ;; Build a user-namespace wrapper for PROGRAM.
            (format #t "building wrapper for '~a'...~%" program)
            (copy-file #$runner "run.c")

            (substitute* "run.c"
              (("@WRAPPED_PROGRAM@") program)
              (("@STORE_DIRECTORY@") (%store-directory)))

            (let* ((base   (strip-store-prefix program))
                   (result (string-append #$output "/" base)))
              (mkdir-p (dirname result))
              (invoke #$compiler "-std=gnu99" "-static" "-Os" "-g0" "-Wall"
                      "run.c" "-o" result)
              (delete-file "run.c")))

          (setvbuf (current-output-port)
                   (cond-expand (guile-2.2 'line)
                                (else      _IOLBF)))
          (for-each build-wrapper
                    (append (find-files #$(file-append package "/bin"))
                            (find-files #$(file-append package "/sbin"))
                            (find-files #$(file-append package "/libexec")))))))

  (computed-file (string-append (package-full-name package "-") "R")
                 build))

(define (map-manifest-entries proc manifest)
  "Apply PROC to all the entries of MANIFEST and return a new manifest."
  (make-manifest
   (map (lambda (entry)
          (manifest-entry
            (inherit entry)
            (item (proc (manifest-entry-item entry)))))
        (manifest-entries manifest))))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((format . tarball)
    (system . ,(%current-system))
    (substitutes? . #t)
    (build-hook? . #t)
    (graft? . #t)
    (verbosity . 0)
    (symlinks . ())
    (compressor . ,(first %compressors))))

(define %formats
  ;; Supported pack formats.
  `((tarball . ,self-contained-tarball)
    (docker  . ,docker-image)))

(define %options
  ;; Specifications of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix pack")))

         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t (alist-cons 'graft? #f result))))
         (option '(#\f "format") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'format (string->symbol arg) result)))
         (option '(#\R "relocatable") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'relocatable? #t result)))
         (option '(#\e "expression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'expression arg result)))
         (option '(#\m "manifest") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'manifest arg result)))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         (option '("target") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'target arg
                               (alist-delete 'target result eq?))))
         (option '(#\C "compression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'compressor (lookup-compressor arg)
                               result)))
         (option '(#\S "symlink") #t #f
                 (lambda (opt name arg result)
                   ;; Note: Using 'string-split' allows us to handle empty
                   ;; TARGET (as in "/opt/guile=", meaning that /opt/guile is
                   ;; a symlink to the profile) correctly.
                   (match (string-split arg (char-set #\=))
                     ((source target)
                      (let ((symlinks (assoc-ref result 'symlinks)))
                        (alist-cons 'symlinks
                                    `((,source -> ,target) ,@symlinks)
                                    (alist-delete 'symlinks result eq?))))
                     (x
                      (leave (G_ "~a: invalid symlink specification~%")
                             arg)))))
         (option '("localstatedir") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'localstatedir? #t result)))
         (option '("bootstrap") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'bootstrap? #t result)))

         (append %transformation-options
                 %standard-build-options)))

(define (show-help)
  (display (G_ "Usage: guix pack [OPTION]... PACKAGE...
Create a bundle of PACKAGE.\n"))
  (show-build-options-help)
  (newline)
  (show-transformation-options-help)
  (newline)
  (display (G_ "
  -f, --format=FORMAT    build a pack in the given FORMAT"))
  (display (G_ "
  -R, --relocatable      produce relocatable executables"))
  (display (G_ "
  -e, --expression=EXPR  consider the package EXPR evaluates to"))
  (display (G_ "
  -s, --system=SYSTEM    attempt to build for SYSTEM--e.g., \"i686-linux\""))
  (display (G_ "
      --target=TRIPLET   cross-build for TRIPLET--e.g., \"armel-linux-gnu\""))
  (display (G_ "
  -C, --compression=TOOL compress using TOOL--e.g., \"lzip\""))
  (display (G_ "
  -S, --symlink=SPEC     create symlinks to the profile according to SPEC"))
  (display (G_ "
  -m, --manifest=FILE    create a pack with the manifest from FILE"))
  (display (G_ "
      --localstatedir    include /var/guix in the resulting pack"))
  (display (G_ "
      --bootstrap        use the bootstrap binaries to build the pack"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))


;;;
;;; Entry point.
;;;

(define (guix-pack . args)
  (define opts
    (parse-command-line args %options (list %default-options)))

  (define maybe-package-argument
    ;; Given an option pair, return a package, a package/output tuple, or #f.
    (match-lambda
      (('argument . spec)
       (call-with-values
           (lambda ()
             (specification->package+output spec))
         list))
      (('expression . exp)
       (read/eval-package-expression exp))
      (x #f)))

  (define (manifest-from-args store opts)
    (let* ((transform     (options->transformation opts))
           (packages      (map (match-lambda
                                 (((? package? package) output)
                                  (list (transform store package) output))
                                 ((? package? package)
                                  (list (transform store package) "out")))
                               (filter-map maybe-package-argument opts)))
           (manifest-file (assoc-ref opts 'manifest)))
      (cond
       ((and manifest-file (not (null? packages)))
        (leave (G_ "both a manifest and a package list were given~%")))
       (manifest-file
        (let ((user-module (make-user-module '((guix profiles) (gnu)))))
          (load* manifest-file user-module)))
       (else (packages->manifest packages)))))

  (with-error-handling
    (with-store store
      ;; Set the build options before we do anything else.
      (set-build-options-from-command-line store opts)

      (parameterize ((%graft? (assoc-ref opts 'graft?))
                     (%guile-for-build (package-derivation
                                        store
                                        (if (assoc-ref opts 'bootstrap?)
                                            %bootstrap-guile
                                            (canonical-package guile-2.2))
                                        #:graft? (assoc-ref opts 'graft?))))
        (let* ((dry-run?    (assoc-ref opts 'dry-run?))
               (relocatable? (assoc-ref opts 'relocatable?))
               (manifest    (let ((manifest (manifest-from-args store opts)))
                              ;; Note: We cannot honor '--bootstrap' here because
                              ;; 'glibc-bootstrap' lacks 'libc.a'.
                              (if relocatable?
                                  (map-manifest-entries wrapped-package manifest)
                                  manifest)))
               (pack-format (assoc-ref opts 'format))
               (name        (string-append (symbol->string pack-format)
                                           "-pack"))
               (target      (assoc-ref opts 'target))
               (bootstrap?  (assoc-ref opts 'bootstrap?))
               (compressor  (if bootstrap?
                                bootstrap-xz
                                (assoc-ref opts 'compressor)))
               (tar         (if bootstrap?
                                %bootstrap-coreutils&co
                                tar))
               (symlinks    (assoc-ref opts 'symlinks))
               (build-image (match (assq-ref %formats pack-format)
                              ((? procedure? proc) proc)
                              (#f
                               (leave (G_ "~a: unknown pack format")
                                      format))))
               (localstatedir? (assoc-ref opts 'localstatedir?)))
          (run-with-store store
            (mlet* %store-monad ((profile (profile-derivation
                                           manifest
                                           #:relative-symlinks? relocatable?
                                           #:hooks (if bootstrap?
                                                       '()
                                                       %default-profile-hooks)
                                           #:locales? (not bootstrap?)
                                           #:target target))
                                 (drv (build-image name profile
                                                   #:target
                                                   target
                                                   #:compressor
                                                   compressor
                                                   #:symlinks
                                                   symlinks
                                                   #:localstatedir?
                                                   localstatedir?
                                                   #:tar
                                                   tar)))
              (mbegin %store-monad
                (show-what-to-build* (list drv)
                                     #:use-substitutes?
                                     (assoc-ref opts 'substitutes?)
                                     #:dry-run? dry-run?)
                (munless dry-run?
                  (built-derivations (list drv))
                  (return (format #t "~a~%"
                                  (derivation->output-path drv))))))
            #:system (assoc-ref opts 'system)))))))
