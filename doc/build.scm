;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
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


;; This file contains machinery to build HTML and PDF copies of the manual
;; that can be readily published on the web site.  To do that, run:
;;
;;  guix build -f build.scm
;;
;; The result is a directory hierarchy that can be used as the manual/
;; sub-directory of the web site.

(use-modules (guix)
             (guix gexp)
             (guix git)
             (guix git-download)
             (guix utils)
             (git)
             (gnu packages base)
             (gnu packages compression)
             (gnu packages gawk)
             (gnu packages gettext)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages iso-codes)
             (gnu packages texinfo)
             (gnu packages tex)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (srfi srfi-71))

(define file-append*
  (@@ (guix self) file-append*))

(define translated-texi-manuals
  (@@ (guix self) translate-texi-manuals))

(define info-manual
  (@@ (guix self) info-manual))

(define %manual
  ;; The manual to build--i.e., the base name of a .texi file, such as "guix"
  ;; or "guix-cookbook".
  (or (getenv "GUIX_MANUAL")
      "guix"))

(define %languages
  ;; The cookbook is currently only translated into German.
  (if (string=? %manual "guix-cookbook")
      '("de" "en")
      '("de" "en" "es" "fr" "ru" "zh_CN")))

(define (texinfo-manual-images source)
  "Return a directory containing all the images used by the user manual, taken
from SOURCE, the root of the source tree."
  (define graphviz
    (module-ref (resolve-interface '(gnu packages graphviz))
                'graphviz))

  (define images
    (file-append* source "doc/images"))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-26))

          (define (dot->image dot-file format)
            (invoke #+(file-append graphviz "/bin/dot")
                    "-T" format "-Gratio=.9" "-Gnodesep=.005"
                    "-Granksep=.00005" "-Nfontsize=9"
                    "-Nheight=.1" "-Nwidth=.1"
                    "-o" (string-append #$output "/"
                                        (basename dot-file ".dot")
                                        "." format)
                    dot-file))

          ;; Build graphs.
          (mkdir-p #$output)
          (for-each (lambda (dot-file)
                      (for-each (cut dot->image dot-file <>)
                                '("png" "pdf")))
                    (find-files #$images "\\.dot$"))

          ;; Copy other PNGs.
          (for-each (lambda (png-file)
                      (install-file png-file #$output))
                    (find-files #$images "\\.png$")))))

  (computed-file "texinfo-manual-images" build))

(define* (texinfo-manual-source source #:key
                                (version "0.0")
                                (languages %languages)
                                (date 1))
  "Gather all the source files of the Texinfo manuals from SOURCE--.texi file
as well as images, OS examples, and translations."
  (define documentation
    (file-append* source "doc"))

  (define examples
    (file-append* source "gnu/system/examples"))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-19))

          (define (make-version-texi language)
            ;; Create the 'version.texi' file for LANGUAGE.
            (let ((file (if (string=? language "en")
                            "version.texi"
                            (string-append "version-" language ".texi"))))
              (call-with-output-file (string-append #$output "/" file)
                (lambda (port)
                  (let* ((version #$version)
                         (time    (make-time time-utc 0 #$date))
                         (date    (time-utc->date time)))
                    (format port "
@set UPDATED ~a
@set UPDATED-MONTH ~a
@set EDITION ~a
@set VERSION ~a\n"
                            (date->string date "~e ~B ~Y")
                            (date->string date "~B ~Y")
                            version version))))))

          (install-file #$(file-append documentation "/htmlxref.cnf")
                        #$output)

          (for-each (lambda (texi)
                      (install-file texi #$output))
                    (append (find-files #$documentation "\\.(texi|scm|json)$")
                            (find-files #$(translated-texi-manuals source)
                                        "\\.texi$")))

          ;; Create 'version.texi'.
          (for-each make-version-texi '#$languages)

          ;; Copy configuration templates that the manual includes.
          (for-each (lambda (template)
                      (copy-file template
                                 (string-append
                                  #$output "/os-config-"
                                  (basename template ".tmpl")
                                  ".texi")))
                    (find-files #$examples "\\.tmpl$"))

          (symlink #$(texinfo-manual-images source)
                   (string-append #$output "/images")))))

  (computed-file "texinfo-manual-source" build))

(define %web-site-url
  ;; URL of the web site home page.
  (or (getenv "GUIX_WEB_SITE_URL")
      "/software/guix/"))

(define %makeinfo-html-options
  ;; Options passed to 'makeinfo --html'.
  '("--css-ref=https://www.gnu.org/software/gnulib/manual.css"
    "-c" "EXTRA_HEAD=<meta name=\"viewport\" \
content=\"width=device-width, initial-scale=1\" />"))

(define (normalize-language-code language)        ;XXX: deduplicate
  ;; Normalize LANGUAGE.  For instance, "zh_CN" becomes "zh-cn".
  (string-map (match-lambda
                (#\_ #\-)
                (chr chr))
              (string-downcase language)))

(define* (html-manual-identifier-index manual base-url
                                       #:key
                                       (name "html-manual-identifier-index"))
  "Return an index of all the identifiers that appear in MANUAL, a
makeinfo-generated manual.  The index is a file that contains an alist; each
key is an identifier and the associated value is the URL reference pointing to
that identifier.  The URL is constructed by concatenating BASE-URL to the
actual file name."
  (define build
    (with-extensions (list guile-lib)
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils)
                         (htmlprag)
                         (srfi srfi-1)
                         (srfi srfi-26)
                         (ice-9 ftw)
                         (ice-9 match)
                         (ice-9 threads)
                         (ice-9 pretty-print))

            (%strict-tokenizer? #t)

            (define file-url
              (let ((prefix (string-append #$manual "/")))
                (lambda (file)
                  ;; Return the URL for FILE.
                  (let ((file (string-drop file (string-length prefix)))
                        (base #$base-url))
                    (if (string-null? base)
                        file
                        (string-append base "/" file))))))

            (define (underscore-decode str)
              ;; Decode STR, an "underscore-encoded" string as produced by
              ;; makeinfo for indexes, such as "_0025base_002dservices" for
              ;; "%base-services".
              (let loop ((str str)
                         (result '()))
                (match (string-index str #\_)
                  (#f
                   (string-concatenate-reverse (cons str result)))
                  (index
                   (let ((char (string->number
                                (substring str (+ index 1) (+ index 5))
                                16)))
                     (loop (string-drop str (+ index 5))
                           (append (list (string (integer->char char))
                                         (string-take str index))
                                   result)))))))

            (define (anchor-id->key id)
              ;; Convert ID, an anchor ID such as
              ;; "index-pam_002dlimits_002dservice" to the corresponding key,
              ;; "pam-limits-service" in this example.  Drop the suffix of
              ;; duplicate anchor IDs like "operating_002dsystem-1".
              (let ((id (if (any (cut string-suffix? <> id)
                                 '("-1" "-2" "-3" "-4" "-5"))
                            (string-drop-right id 2)
                            id)))
                (underscore-decode
                 (string-drop id (string-length "index-")))))

            (define* (collect-anchors file #:optional (anchors '()))
              ;; Collect the anchors that appear in FILE, a makeinfo-generated
              ;; file.  Grab those from <dt> tags, which corresponds to
              ;; Texinfo @deftp, @defvr, etc.  Return ANCHORS augmented with
              ;; more name/reference pairs.
              (define string-or-entity?
                (match-lambda
                  ((? string?) #t)
                  (('*ENTITY* _ ...) #t)
                  (_ #f)))

              (define (worthy-entry? lst)
                ;; Attempt to match:
                ;;   Scheme Variable: <strong>x</strong>
                ;; but not:
                ;;   <code>cups-configuration</code> parameter: …
                (let loop ((lst lst))
                  (match lst
                    (((? string-or-entity?) rest ...)
                     (loop rest))
                    ((('strong _ ...) _ ...)
                     #t)
                    ((('span ('@ ('class "symbol-definition-category"))
                             (? string-or-entity?) ...) rest ...)
                     #t)
                    (x
                     #f))))

              (let ((shtml (call-with-input-file file html->shtml)))
                (let loop ((shtml shtml)
                           (anchors anchors))
                  (match shtml
                    (('dt ('@ ('id id) _ ...) rest ...)
                     (if (and (string-prefix? "index-" id)
                              (worthy-entry? rest))
                         (alist-cons (anchor-id->key id)
                                     (string-append (file-url file)
                                                    "#" id)
                                     anchors)
                         anchors))
                    ((tag ('@ _ ...) body ...)
                     (fold loop anchors body))
                    ((tag body ...)
                     (fold loop anchors body))
                    (_ anchors)))))

            (define (html-files directory)
              ;; Return the list of HTML files under DIRECTORY.
              (map (cut string-append directory "/" <>)
                   (scandir #$manual (lambda (file)
                                       (string-suffix? ".html" file)))))

            (define anchors
              (sort (concatenate
                     (n-par-map (parallel-job-count)
                                (cut collect-anchors <>)
                                (html-files #$manual)))
                    (match-lambda*
                      (((key1 . url1) (key2 . url2))
                       (if (string=? key1 key2)
                           (string<? url1 url2)
                           (string<? key1 key2))))))

            (call-with-output-file #$output
              (lambda (port)
                (display ";; Identifier index for the manual.\n\n"
                         port)
                (pretty-print anchors port)))))))

  (computed-file name build))

(define* (html-identifier-indexes manual directory-suffix
                                  #:key (languages %languages)
                                  (manual-name %manual)
                                  (base-url (const "")))
  (map (lambda (language)
         (let ((language (normalize-language-code language)))
           (list language
                 (html-manual-identifier-index
                  (file-append manual "/" language directory-suffix)
                  (base-url language)
                  #:name (string-append manual-name "-html-index-"
                                        language)))))
       languages))

(define* (syntax-highlighted-html input
                                  #:key
                                  (name "highlighted-syntax")
                                  (languages %languages)
                                  (mono-node-indexes
                                   (html-identifier-indexes input ""
                                                            #:languages
                                                            languages))
                                  (split-node-indexes
                                   (html-identifier-indexes input
                                                            "/html_node"
                                                            #:languages
                                                            languages))
                                  (syntax-css-url
                                   "/static/base/css/code.css"))
  "Return a derivation called NAME that processes all the HTML files in INPUT
to (1) add them a link to SYNTAX-CSS-URL, and (2) highlight the syntax of all
its <pre class=\"lisp\"> blocks (as produced by 'makeinfo --html')."
  (define build
    (with-extensions (list guile-lib guile-syntax-highlight)
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (htmlprag)
                         (syntax-highlight)
                         (syntax-highlight scheme)
                         (syntax-highlight lexers)
                         (guix build utils)
                         (srfi srfi-1)
                         (srfi srfi-26)
                         (ice-9 match)
                         (ice-9 threads)
                         (ice-9 vlist))

            (%strict-tokenizer? #t)

            (define (pair-open/close lst)
              ;; Pair 'open' and 'close' tags produced by 'highlights' and
              ;; produce nested 'paren' tags instead.
              (let loop ((lst lst)
                         (level 0)
                         (result '()))
                (match lst
                  ((('open open) rest ...)
                   (call-with-values
                       (lambda ()
                         (loop rest (+ 1 level) '()))
                     (lambda (inner close rest)
                       (loop rest level
                             (cons `(paren ,level ,open ,inner ,close)
                                   result)))))
                  ((('close str) rest ...)
                   (if (> level 0)
                       (values (reverse result) str rest)
                       (begin
                         (format (current-error-port)
                                 "warning: extra closing paren; context:~% ~y~%"
                                 (reverse result))
                         (loop rest 0 (cons `(close ,str) result)))))
                  ((item rest ...)
                   (loop rest level (cons item result)))
                  (()
                   (when (> level 0)
                     (format (current-error-port)
                             "warning: missing ~a closing parens; context:~% ~y%"
                             level (reverse result)))
                   (values (reverse result) "" '())))))

            (define (highlights->sxml* highlights anchors)
              ;; Like 'highlights->sxml', but handle nested 'paren tags.  This
              ;; allows for paren matching highlights via appropriate CSS
              ;; "hover" properties.  When a symbol is encountered, look it up
              ;; in ANCHORS, a vhash, and emit the corresponding href, if any.
              (define (tag->class tag)
                (string-append "syntax-" (symbol->string tag)))

              (map (match-lambda
                     ((? string? str) str)
                     (('paren level open (body ...) close)
                      `(span (@ (class ,(string-append "syntax-paren"
                                                       (number->string level))))
                             ,open
                             (span (@ (class "syntax-symbol"))
                                   ,@(highlights->sxml* body anchors))
                             ,close))
                     (('symbol text)
                      ;; Check whether we can emit a hyperlink for TEXT.
                      (match (vhash-assoc text anchors)
                        (#f
                         `(span (@ (class ,(tag->class 'symbol))) ,text))
                        ((_ . target)
                         `(a (@ (class ,(tag->class 'symbol)) (href ,target))
                             ,text))))
                     ((tag text)
                      `(span (@ (class ,(tag->class tag))) ,text)))
                   highlights))

            (define entity->string
              (match-lambda
                ("rArr"   "⇒")
                ("rarr"   "→")
                ("hellip" "…")
                ("rsquo"  "’")
                (e (pk 'unknown-entity e) (primitive-exit 2))))

            (define (concatenate-snippets pieces)
              ;; Concatenate PIECES, which contains strings and entities,
              ;; replacing entities with their corresponding string.
              (let loop ((pieces pieces)
                         (strings '()))
                (match pieces
                  (()
                   (string-concatenate-reverse strings))
                  (((? string? str) . rest)
                   (loop rest (cons str strings)))
                  ((('*ENTITY* "additional" entity) . rest)
                   (loop rest (cons (entity->string entity) strings)))
                  ((('span _ lst ...) . rest)     ;for <span class="roman">
                   (loop (append lst rest) strings))
                  ((('var name) . rest)           ;for @var{name} within @lisp
                   (loop rest (cons name strings))) ;XXX: losing formatting
                  (something
                   (pk 'unsupported-code-snippet something)
                   (primitive-exit 1)))))

            (define (highlight-definition id category symbol args)
              ;; Produce stylable HTML for the given definition (an @deftp,
              ;; @deffn, or similar).
              `(dt (@ (id ,id) (class "symbol-definition"))
                   (span (@ (class "symbol-definition-category"))
                         ,@category)
                   (span (@ (class "symbol-definition-prototype"))
                         ,symbol " " ,@args)))

            (define (space? obj)
              (and (string? obj)
                   (string-every char-set:whitespace obj)))

            (define (syntax-highlight sxml anchors)
              ;; Recurse over SXML and syntax-highlight code snippets.
              (let loop ((sxml sxml))
                (match sxml
                  (('*TOP* decl body ...)
                   `(*TOP* ,decl ,@(map loop body)))
                  (('head things ...)
                   `(head ,@things
                          (link (@ (rel "stylesheet")
                                   (type "text/css")
                                   (href #$syntax-css-url)))))
                  (('pre ('@ ('class "lisp")) code-snippet ...)
                   `(pre (@ (class "lisp"))
                         ,@(highlights->sxml*
                            (pair-open/close
                             (highlight lex-scheme
                                        (concatenate-snippets code-snippet)))
                            anchors)))

                  ;; Replace the ugly <strong> used for @deffn etc., which
                  ;; translate to <dt>, with more stylable markup.
                  (('dt (@ ('id id)) category ... ('strong thing))
                   (highlight-definition id category thing '()))
                  (('dt (@ ('id id)) category ... ('strong thing)
                        (? space?) ('em args ...))
                   (highlight-definition id category thing args))

                  ((tag ('@ attributes ...) body ...)
                   `(,tag (@ ,@attributes) ,@(map loop body)))
                  ((tag body ...)
                   `(,tag ,@(map loop body)))
                  ((? string? str)
                   str))))

            (define (process-html file anchors)
              ;; Parse FILE and perform syntax highlighting for its Scheme
              ;; snippets.  Install the result to #$output.
              (format (current-error-port) "processing ~a...~%" file)
              (let* ((shtml        (call-with-input-file file html->shtml))
                     (highlighted  (syntax-highlight shtml anchors))
                     (base         (string-drop file (string-length #$input)))
                     (target       (string-append #$output base)))
                (mkdir-p (dirname target))
                (call-with-output-file target
                  (lambda (port)
                    (write-shtml-as-html highlighted port)))))

            (define (copy-as-is file)
              ;; Copy FILE as is to #$output.
              (let* ((base   (string-drop file (string-length #$input)))
                     (target (string-append #$output base)))
                (mkdir-p (dirname target))
                (catch 'system-error
                  (lambda ()
                    (if (eq? 'symlink (stat:type (lstat file)))
                        (symlink (readlink file) target)
                        (link file target)))
                  (lambda args
                    (let ((errno (system-error-errno args)))
                      (pk 'error-link file target (strerror errno))
                      (primitive-exit 3))))))

            (define (html? file stat)
              (string-suffix? ".html" file))

            (define language+node-anchors
              (match-lambda
                ((language files ...)
                 (cons language
                       (fold (lambda (file vhash)
                               (let ((alist (call-with-input-file file read)))
                                 ;; Use 'fold-right' so that the first entry
                                 ;; wins (e.g., "car" from "Pairs" rather than
                                 ;; from "rnrs base" in the Guile manual).
                                 (fold-right (match-lambda*
                                               (((key . value) vhash)
                                                (vhash-cons key value vhash)))
                                             vhash
                                             alist)))
                             vlist-null
                             files)))))

            (define mono-node-anchors
              ;; List of language/vhash pairs, where each vhash maps an
              ;; identifier to the corresponding URL in a single-page manual.
              (map language+node-anchors '#$mono-node-indexes))

            (define multi-node-anchors
              ;; Likewise for split-node manuals.
              (map language+node-anchors '#$split-node-indexes))

            ;; Install a UTF-8 locale so we can process UTF-8 files.
            (setenv "GUIX_LOCPATH"
                    #+(file-append glibc-utf8-locales "/lib/locale"))
            (setlocale LC_ALL "en_US.utf8")

            ;; First process the mono-node 'guix.html' files.
            (for-each (match-lambda
                        ((language . anchors)
                         (let ((files (find-files
                                       (string-append #$input "/" language)
                                       "^guix(-cookbook|)(\\.[a-zA-Z_-]+)?\\.html$")))
                           (n-par-for-each (parallel-job-count)
                                           (cut process-html <> anchors)
                                           files))))
                      mono-node-anchors)

            ;; Process the multi-node HTML files.
            (for-each (match-lambda
                        ((language . anchors)
                         (let ((files (find-files
                                       (string-append #$input "/" language
                                                      "/html_node")
                                       "\\.html$")))
                           (n-par-for-each (parallel-job-count)
                                           (cut process-html <> anchors)
                                           files))))
                      multi-node-anchors)

            ;; Last, copy non-HTML files as is.
            (for-each copy-as-is
                      (find-files #$input (negate html?)))))))

  (computed-file name build))

(define* (html-manual source #:key (languages %languages)
                      (version "0.0")
                      (manual %manual)
                      (mono-node-indexes (map list languages))
                      (split-node-indexes (map list languages))
                      (date 1)
                      (options %makeinfo-html-options))
  "Return the HTML manuals built from SOURCE for all LANGUAGES, with the given
makeinfo OPTIONS."
  (define manual-source
    (texinfo-manual-source source
                           #:version version
                           #:languages languages
                           #:date date))

  (define images
    (texinfo-manual-images source))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 match))

          (define (normalize language)
            ;; Normalize LANGUAGE.  For instance, "zh_CN" becomes "zh-cn".
            (string-map (match-lambda
                          (#\_ #\-)
                          (chr chr))
                        (string-downcase language)))

          (define (language->texi-file-name language)
            (if (string=? language "en")
                (string-append #$manual-source "/"
                               #$manual ".texi")
                (string-append #$manual-source "/"
                               #$manual "." language ".texi")))

          ;; Install a UTF-8 locale so that 'makeinfo' is at ease.
          (setenv "GUIX_LOCPATH"
                  #+(file-append glibc-utf8-locales "/lib/locale"))
          (setenv "LC_ALL" "en_US.utf8")

          (setvbuf (current-output-port) 'line)
          (setvbuf (current-error-port) 'line)

          ;; 'makeinfo' looks for "htmlxref.cnf" in the current directory, so
          ;; copy it right here.
          (copy-file (string-append #$manual-source "/htmlxref.cnf")
                     "htmlxref.cnf")

          (for-each (lambda (language)
                      (let* ((texi (language->texi-file-name language))
                             (opts `("--html"
                                     "-c" ,(string-append "TOP_NODE_UP_URL=/manual/"
                                                         language)
                                     #$@options
                                     ,texi)))
                        (format #t "building HTML manual for language '~a'...~%"
                                language)
                        (mkdir-p (string-append #$output "/"
                                                (normalize language)))
                        (setenv "LANGUAGE" language)
                        (apply invoke #$(file-append texinfo "/bin/makeinfo")
                               "-o" (string-append #$output "/"
                                                   (normalize language)
                                                   "/html_node")
                               opts)
                        (apply invoke #$(file-append texinfo "/bin/makeinfo")
                               "--no-split"
                               "-o"
                               (string-append #$output "/"
                                              (normalize language)
                                              "/" #$manual
                                              (if (string=? language "en")
                                                  ""
                                                  (string-append "." language))
                                              ".html")
                               opts)

                        ;; Make sure images are available.
                        (symlink #$images
                                 (string-append #$output "/" (normalize language)
                                                "/images"))
                        (symlink #$images
                                 (string-append #$output "/" (normalize language)
                                                "/html_node/images"))))
                    (filter (compose file-exists? language->texi-file-name)
                            '#$languages)))))

  (let* ((name   (string-append manual "-html-manual"))
         (manual (computed-file name build)))
    (syntax-highlighted-html manual
                             #:mono-node-indexes mono-node-indexes
                             #:split-node-indexes split-node-indexes
                             #:name (string-append name "-highlighted"))))

(define* (pdf-manual source #:key (languages %languages)
                     (version "0.0")
                     (manual %manual)
                     (date 1)
                     (options '()))
  "Return the HTML manuals built from SOURCE for all LANGUAGES, with the given
makeinfo OPTIONS."
  (define manual-source
    (texinfo-manual-source source
                           #:version version
                           #:languages languages
                           #:date date))

  ;; FIXME: This union works, except for the table of contents of non-English
  ;; manuals, which contains escape sequences like "^^ca^^fe" instead of
  ;; accented letters.
  ;;
  ;; (define texlive
  ;;   (texlive-union (list texlive-tex-texinfo
  ;;                        texlive-generic-epsf
  ;;                        texlive-fonts-ec)))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-34)
                       (ice-9 match))

          (define (normalize language)            ;XXX: deduplicate
            ;; Normalize LANGUAGE.  For instance, "zh_CN" becomes "zh-cn".
            (string-map (match-lambda
                          (#\_ #\-)
                          (chr chr))
                        (string-downcase language)))

          ;; Install a UTF-8 locale so that 'makeinfo' is at ease.
          (setenv "GUIX_LOCPATH"
                  #+(file-append glibc-utf8-locales "/lib/locale"))
          (setenv "LC_ALL" "en_US.utf8")
          (setenv "PATH"
                  (string-append #+(file-append texlive "/bin") ":"
                                 #+(file-append texinfo "/bin") ":"

                                 ;; Below are command-line tools needed by
                                 ;; 'texi2dvi' and friends.
                                 #+(file-append sed "/bin") ":"
                                 #+(file-append grep "/bin") ":"
                                 #+(file-append coreutils "/bin") ":"
                                 #+(file-append gawk "/bin") ":"
                                 #+(file-append tar "/bin") ":"
                                 #+(file-append diffutils "/bin")))

          (setvbuf (current-output-port) 'line)
          (setvbuf (current-error-port) 'line)

          (setenv "HOME" (getcwd))                ;for kpathsea/mktextfm

          ;; 'SOURCE_DATE_EPOCH' is honored by pdftex.
          (setenv "SOURCE_DATE_EPOCH" "1")

          (for-each (lambda (language)
                      (let ((opts `("--pdf"
                                    "-I" "."
                                    #$@options
                                    ,(if (string=? language "en")
                                         (string-append #$manual-source "/"
                                                        #$manual ".texi")
                                         (string-append #$manual-source "/"
                                                        #$manual "." language ".texi")))))
                        (format #t "building PDF manual for language '~a'...~%"
                                language)
                        (mkdir-p (string-append #$output "/"
                                                (normalize language)))
                        (setenv "LANGUAGE" language)


                        ;; FIXME: Unfortunately building PDFs for non-Latin
                        ;; alphabets doesn't work:
                        ;; <https://lists.gnu.org/archive/html/help-texinfo/2012-01/msg00014.html>.
                        (guard (c ((invoke-error? c)
                                   (format (current-error-port)
                                           "~%~%Failed to produce \
PDF for language '~a'!~%~%"
                                           language)))
                         (apply invoke #$(file-append texinfo "/bin/makeinfo")
                                "--pdf" "-o"
                                (string-append #$output "/"
                                               (normalize language)
                                               "/" #$manual
                                               (if (string=? language "en")
                                                   ""
                                                   (string-append "."
                                                                  language))
                                               ".pdf")
                                opts))))
                    '#$languages))))

  (computed-file (string-append manual "-pdf-manual") build))

(define (guix-manual-text-domain source languages)
  "Return the PO files for LANGUAGES of the 'guix-manual' text domain taken
from SOURCE."
  (define po-directory
    (file-append* source "/po/doc"))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))

          (mkdir-p #$output)
          (for-each (lambda (language)
                      (define directory
                        (string-append #$output "/" language
                                       "/LC_MESSAGES"))

                      (mkdir-p directory)
                      (invoke #+(file-append gnu-gettext "/bin/msgfmt")
                              "-c" "-o"
                              (string-append directory "/guix-manual.mo")
                              (string-append #$po-directory "/guix-manual."
                                             language ".po")))
                    '#$(delete "en" languages)))))

  (computed-file "guix-manual-po" build))

(define* (html-manual-indexes source
                              #:key (languages %languages)
                              (version "0.0")
                              (manual %manual)
                              (title (if (string=? "guix" manual)
                                         "GNU Guix Reference Manual"
                                         "GNU Guix Cookbook"))
                              (date 1))
  (define build
    (with-extensions (list guile-json-3)
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils)
                         (json)
                         (ice-9 match)
                         (ice-9 popen)
                         (sxml simple)
                         (srfi srfi-1)
                         (srfi srfi-19))

            (define (normalize language)          ;XXX: deduplicate
              ;; Normalize LANGUAGE.  For instance, "zh_CN" becomes "zh-cn".
              (string-map (match-lambda
                            (#\_ #\-)
                            (chr chr))
                          (string-downcase language)))

            (define-syntax-rule (with-language language exp ...)
              (let ((lang (getenv "LANGUAGE")))
                (dynamic-wind
                  (lambda ()
                    (setenv "LANGUAGE" language)
                    (setlocale LC_MESSAGES))
                  (lambda () exp ...)
                  (lambda ()
                    (if lang
                        (setenv "LANGUAGE" lang)
                        (unsetenv "LANGUAGE"))
                    (setlocale LC_MESSAGES)))))

            ;; (put 'with-language 'scheme-indent-function 1)
            (define* (translate str language
                                #:key (domain "guix-manual"))
              (define exp
                `(begin
                   (bindtextdomain "guix-manual"
                                   #+(guix-manual-text-domain
                                      source
                                      languages))
                   (bindtextdomain "iso_639-3"    ;language names
                                   #+(file-append iso-codes
                                                  "/share/locale"))
                   (write (gettext ,str ,domain))))

              (with-language language
                ;; Since the 'gettext' function caches msgid translations,
                ;; regardless of $LANGUAGE, we have to spawn a new process each
                ;; time we want to translate to a different language.  Bah!
                (let* ((pipe (open-pipe* OPEN_READ
                                         #+(file-append guile-2.2
                                                        "/bin/guile")
                                         "-c" (object->string exp)))
                       (str  (read pipe)))
                  (close-pipe pipe)
                  str)))

            (define (seconds->string seconds language)
              (let* ((time (make-time time-utc 0 seconds))
                     (date (time-utc->date time)))
                (with-language language (date->string date "~e ~B ~Y"))))

            (define (guix-url path)
              (string-append #$%web-site-url path))

            (define (sxml-index language title body)
              ;; FIXME: Avoid duplicating styling info from guix-artwork.git.
              `(html (@ (lang ,language))
                     (head
                      (title ,(string-append title " — GNU Guix"))
                      (meta (@ (charset "UTF-8")))
                      (meta (@ (name "viewport") (content "width=device-width, initial-scale=1.0")))
                      ;; Menu prefetch.
                      (link (@ (rel "prefetch") (href ,(guix-url "menu/index.html"))))
                      ;; Base CSS.
                      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/elements.css"))))
                      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/common.css"))))
                      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/messages.css"))))
                      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/navbar.css"))))
                      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/breadcrumbs.css"))))
                      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/buttons.css"))))
                      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/footer.css"))))

                      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/page.css"))))
                      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/post.css")))))
                     (body
                      (header (@ (class "navbar"))
                              (h1 (a (@ (class "branding")
                                        (href #$%web-site-url)))
                                  (span (@ (class "a11y-offset"))
                                        "Guix"))
                              (nav (@ (class "menu"))))
                      (nav (@ (class "breadcrumbs"))
                           (a (@ (class "crumb")
                                 (href #$%web-site-url))
                              "Home"))
                      ,body
                      (footer))))

            (define (language-index language)
              (define title
                (translate #$title language))

              (sxml-index
               language title
               `(main
                 (article
                  (@ (class "page centered-block limit-width"))
                  (h2 ,title)
                  (p (@ (class "post-metadata centered-text"))
                     #$version " — "
                     ,(seconds->string #$date language))

                  (div
                   (ul
                    (li (a (@ (href "html_node"))
                           "HTML, with one page per node"))
                    (li (a (@ (href
                               ,(string-append
                                 #$manual
                                 (if (string=? language
                                               "en")
                                     ""
                                     (string-append "."
                                                    language))
                                 ".html")))
                           "HTML, entirely on one page"))
                    ,@(if (member language '("ru" "zh_CN"))
                          '()
                          `((li (a (@ (href ,(string-append
                                              #$manual
                                              (if (string=? language "en")
                                                  ""
                                                  (string-append "."
                                                                 language))
                                              ".pdf"))))
                                "PDF")))))))))

            (define %iso639-languages
              (vector->list
               (assoc-ref (call-with-input-file
                              #+(file-append iso-codes
                                             "/share/iso-codes/json/iso_639-3.json")
                            json->scm)
                          "639-3")))

            (define (language-code->name code)
              "Return the full name of a language from its ISO-639-3 code."
              (let ((code (match (string-index code #\_)
                            (#f    code)
                            (index (string-take code index)))))
               (any (lambda (language)
                      (and (string=? (or (assoc-ref language "alpha_2")
                                         (assoc-ref language "alpha_3"))
                                     code)
                           (assoc-ref language "name")))
                    %iso639-languages)))

            (define (top-level-index languages)
              (define title #$title)
              (sxml-index
               "en" title
               `(main
                 (article
                  (@ (class "page centered-block limit-width"))
                  (h2 ,title)
                  (div
                   "This document is available in the following
languages:\n"
                   (ul
                    ,@(map (lambda (language)
                             `(li (a (@ (href ,(normalize language)))
                                     ,(translate
                                       (language-code->name language)
                                       language
                                       #:domain "iso_639-3"))))
                           languages)))))))

            (define (write-html file sxml)
              (call-with-output-file file
                (lambda (port)
                  (display "<!DOCTYPE html>\n" port)
                  (sxml->xml sxml port))))

            (setenv "GUIX_LOCPATH"
                    #+(file-append glibc-utf8-locales "/lib/locale"))
            (setenv "LC_ALL" "en_US.utf8")
            (setlocale LC_ALL "en_US.utf8")

            (for-each (lambda (language)
                        (define directory
                          (string-append #$output "/"
                                         (normalize language)))

                        (mkdir-p directory)
                        (write-html (string-append directory "/index.html")
                                    (language-index language)))
                      '#$languages)

            (write-html (string-append #$output "/index.html")
                        (top-level-index '#$languages))))))

  (computed-file "html-indexes" build))

(define* (pdf+html-manual source
                          #:key (languages %languages)
                          (version "0.0")
                          (date (time-second (current-time time-utc)))
                          (mono-node-indexes (map list %languages))
                          (split-node-indexes (map list %languages))
                          (manual %manual))
  "Return the union of the HTML and PDF manuals, as well as the indexes."
  (directory-union (string-append manual "-manual")
                   (map (lambda (proc)
                          (proc source
                                #:date date
                                #:languages languages
                                #:version version
                                #:manual manual))
                        (list html-manual-indexes
                              (lambda (source . args)
                                (apply html-manual source
                                       #:mono-node-indexes mono-node-indexes
                                       #:split-node-indexes split-node-indexes
                                       args))
                              pdf-manual))
                   #:copy? #t))

(define (latest-commit+date directory)
  "Return two values: the last commit ID (a hex string) for DIRECTORY, and its
commit date (an integer)."
  (let* ((repository (repository-open directory))
         (head       (repository-head repository))
         (oid        (reference-target head))
         (commit     (commit-lookup repository oid)))
    ;; TODO: Use (git describe) when it's widely available.
    (values (oid->string oid) (commit-time commit))))


;;;
;;; Guile manual.
;;;

(define guile-manual
  ;; The Guile manual as HTML, including both the mono-node "guile.html" and
  ;; the split-node "html_node" directory.
  (let ((guile guile-3.0-latest))
    (computed-file (string-append "guile-manual-" (package-version guile))
                   (with-imported-modules '((guix build utils))
                     #~(begin
                         (use-modules (guix build utils)
                                      (ice-9 match))

                         (setenv "PATH"
                                 (string-append #+tar "/bin:"
                                                #+xz "/bin:"
                                                #+texinfo "/bin"))
                         (invoke "tar" "xf" #$(package-source guile))
                         (mkdir-p (string-append #$output "/en/html_node"))

                         (let* ((texi (find-files "." "^guile\\.texi$"))
                                (documentation (match texi
                                                 ((file) (dirname file)))))
                           (with-directory-excursion documentation
                             (invoke "makeinfo" "--html" "--no-split"
                                     "-o" (string-append #$output
                                                         "/en/guile.html")
                                     "guile.texi")
                             (invoke "makeinfo" "--html" "-o" "split"
                                     "guile.texi")
                             (copy-recursively
                              "split"
                              (string-append #$output "/en/html_node")))))))))

(define %guile-manual-base-url
  "https://www.gnu.org/software/guile/manual")

(define (for-all-languages index)
  (map (lambda (language)
         (list language index))
       %languages))

(define guile-mono-node-indexes
  ;; The Guile manual is only available in English so use the same index in
  ;; all languages.
  (for-all-languages
   (html-manual-identifier-index (file-append guile-manual "/en")
                                 %guile-manual-base-url
                                 #:name "guile-html-index-en")))

(define guile-split-node-indexes
  (for-all-languages
   (html-manual-identifier-index (file-append guile-manual "/en/html_node")
                                 (string-append %guile-manual-base-url
                                                "/html_node")
                                 #:name "guile-html-index-en")))

(define (merge-index-alists alist1 alist2)
  "Merge ALIST1 and ALIST2, both of which are list of tuples like:

  (LANGUAGE INDEX1 INDEX2 ...)

where LANGUAGE is a string like \"en\" and INDEX1 etc. are indexes as returned
by 'html-identifier-indexes'."
  (let ((languages (delete-duplicates
                    (append (match alist1
                              (((languages . _) ...)
                               languages))
                            (match alist2
                              (((languages . _) ...)
                               languages))))))
    (map (lambda (language)
           (cons language
                 (append (or (assoc-ref alist1 language) '())
                         (or (assoc-ref alist2 language) '()))))
         languages)))


(let* ((root (canonicalize-path
              (string-append (current-source-directory) "/..")))
       (commit date (latest-commit+date root))
       (version (or (getenv "GUIX_MANUAL_VERSION")
                    (string-take commit 7)))
       (select? (let ((vcs? (git-predicate root)))
                  (lambda (file stat)
                    (and (vcs? file stat)
                         ;; Filter out this file.
                         (not (string=? (basename file) "build.scm"))))))
       (source (local-file root "guix" #:recursive? #t
                           #:select? select?)))

  (define guix-manual
    (html-manual source
                 #:manual "guix"
                 #:version version
                 #:date date))

  (define guix-mono-node-indexes
    ;; Alist of indexes for GUIX-MANUAL, where each key is a language code and
    ;; each value is a file-like object containing the identifier index.
    (html-identifier-indexes guix-manual ""
                             #:manual-name "guix"
                             #:base-url (if (string=? %manual "guix")
                                            (const "")
                                            (cut string-append
                                              "/manual/devel/" <>))
                             #:languages %languages))

  (define guix-split-node-indexes
    ;; Likewise for the split-node variant of GUIX-MANUAL.
    (html-identifier-indexes guix-manual "/html_node"
                             #:manual-name "guix"
                             #:base-url (if (string=? %manual "guix")
                                            (const "")
                                            (cut string-append
                                              "/manual/devel/" <>
                                              "/html_node"))
                             #:languages %languages))

  (define mono-node-indexes
    (merge-index-alists guix-mono-node-indexes guile-mono-node-indexes))

  (define split-node-indexes
    (merge-index-alists guix-split-node-indexes guile-split-node-indexes))

  (format (current-error-port)
          "building manual from work tree around commit ~a, ~a~%"
          commit
          (let* ((time (make-time time-utc 0 date))
                 (date (time-utc->date time)))
            (date->string date "~e ~B ~Y")))

  (pdf+html-manual source
                   ;; Always use the identifier indexes of GUIX-MANUAL and
                   ;; GUILE-MANUAL.  Both "guix" and "guix-cookbook" can
                   ;; contain links to definitions that appear in either of
                   ;; these two manuals.
                   #:mono-node-indexes mono-node-indexes
                   #:split-node-indexes split-node-indexes
                   #:version version
                   #:date date))
