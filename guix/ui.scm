;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (guix config)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix derivations)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module ((guix licenses) #:select (license? license-name))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:autoload   (ice-9 ftw)  (scandir)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:export (_
            N_
            P_
            leave
            show-version-and-exit
            show-bug-report-information
            string->number*
            size->number
            show-what-to-build
            call-with-error-handling
            with-error-handling
            read/eval
            read/eval-package-expression
            location->string
            switch-symlinks
            config-directory
            fill-paragraph
            string->recutils
            package->recutils
            package-specification->name+version+output
            string->generations
            string->duration
            args-fold*
            run-guix-command
            program-name
            guix-warning-port
            warning
            guix-main))

;;; Commentary:
;;;
;;; User interface facilities for command-line tools.
;;;
;;; Code:

(define %gettext-domain
  ;; Text domain for strings used in the tools.
  "guix")

(define %package-text-domain
  ;; Text domain for package synopses and descriptions.
  "guix-packages")

(define _ (cut gettext <> %gettext-domain))
(define N_ (cut ngettext <> <> <> %gettext-domain))
(define P_ (cut gettext <> %package-text-domain))

(define-syntax-rule (define-diagnostic name prefix)
  "Create a diagnostic macro (i.e., NAME), which will prepend PREFIX to all
messages."
  (define-syntax name
    (lambda (x)
      (define (augmented-format-string fmt)
        (string-append "~:[~*~;guix ~a: ~]~a" (syntax->datum fmt)))

      (syntax-case x ()
        ((name (underscore fmt) args (... ...))
         (and (string? (syntax->datum #'fmt))
              (free-identifier=? #'underscore #'_))
         (with-syntax ((fmt*   (augmented-format-string #'fmt))
                       (prefix (datum->syntax x prefix)))
           #'(format (guix-warning-port) (gettext fmt*)
                     (program-name) (program-name) prefix
                     args (... ...))))
        ((name (N-underscore singular plural n) args (... ...))
         (and (string? (syntax->datum #'singular))
              (string? (syntax->datum #'plural))
              (free-identifier=? #'N-underscore #'N_))
         (with-syntax ((s      (augmented-format-string #'singular))
                       (p      (augmented-format-string #'plural))
                       (prefix (datum->syntax x prefix)))
           #'(format (guix-warning-port)
                     (ngettext s p n %gettext-domain)
                     (program-name) (program-name) prefix
                     args (... ...))))))))

(define-diagnostic warning "warning: ") ; emit a warning

(define-diagnostic report-error "error: ")
(define-syntax-rule (leave args ...)
  "Emit an error message and exit."
  (begin
    (report-error args ...)
    (exit 1)))

(define (install-locale)
  "Install the current locale settings."
  (catch 'system-error
    (lambda _
      (setlocale LC_ALL ""))
    (lambda args
      (warning (_ "failed to install locale: ~a~%")
               (strerror (system-error-errno args))))))

(define (initialize-guix)
  "Perform the usual initialization for stand-alone Guix commands."
  (install-locale)
  (textdomain %gettext-domain)

  ;; Ignore SIGPIPE.  If the daemon closes the connection, we prefer to be
  ;; notified via an EPIPE later.
  (sigaction SIGPIPE SIG_IGN)

  (setvbuf (current-output-port) _IOLBF)
  (setvbuf (current-error-port) _IOLBF))

(define* (show-version-and-exit #:optional (command (car (command-line))))
  "Display version information for COMMAND and `(exit 0)'."
  (simple-format #t "~a (~a) ~a~%"
                 command %guix-package-name %guix-version)
  (display (_ "Copyright (C) 2014 the Guix authors
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
"))
  (exit 0))

(define (show-bug-report-information)
  (format #t (_ "
Report bugs to: ~a.") %guix-bug-report-address)
  (format #t (_ "
~a home page: <~a>") %guix-package-name %guix-home-page-url)
  (display (_ "
General help using GNU software: <http://www.gnu.org/gethelp/>"))
  (newline))

(define (string->number* str)
  "Like `string->number', but error out with an error message on failure."
  (or (string->number str)
      (leave (_ "~a: invalid number~%") str)))

(define (size->number str)
  "Convert STR, a storage measurement representation such as \"1024\" or
\"1MiB\", to a number of bytes.  Raise an error if STR could not be
interpreted."
  (define unit-pos
    (string-rindex str char-set:digit))

  (define unit
    (and unit-pos (substring str (+ 1 unit-pos))))

  (let* ((numstr (if unit-pos
                     (substring str 0 (+ 1 unit-pos))
                     str))
         (num    (string->number numstr)))
    (unless num
      (leave (_ "invalid number: ~a~%") numstr))

    ((compose inexact->exact round)
     (* num
        (match unit
          ("KiB" (expt 2 10))
          ("MiB" (expt 2 20))
          ("GiB" (expt 2 30))
          ("TiB" (expt 2 40))
          ("KB"  (expt 10 3))
          ("MB"  (expt 10 6))
          ("GB"  (expt 10 9))
          ("TB"  (expt 10 12))
          (""    1)
          (_
           (leave (_ "unknown unit: ~a~%") unit)))))))

(define (call-with-error-handling thunk)
  "Call THUNK within a user-friendly error handler."
  (guard (c ((package-input-error? c)
             (let* ((package  (package-error-package c))
                    (input    (package-error-invalid-input c))
                    (location (package-location package))
                    (file     (location-file location))
                    (line     (location-line location))
                    (column   (location-column location)))
               (leave (_ "~a:~a:~a: package `~a' has an invalid input: ~s~%")
                      file line column
                      (package-full-name package) input)))
            ((package-cross-build-system-error? c)
             (let* ((package (package-error-package c))
                    (loc     (package-location package))
                    (system  (package-build-system package)))
               (leave (_ "~a: ~a: build system `~a' does not support cross builds~%")
                      (location->string loc)
                      (package-full-name package)
                      (build-system-name system))))
            ((nix-connection-error? c)
             (leave (_ "failed to connect to `~a': ~a~%")
                    (nix-connection-error-file c)
                    (strerror (nix-connection-error-code c))))
            ((nix-protocol-error? c)
             ;; FIXME: Server-provided error messages aren't i18n'd.
             (leave (_ "build failed: ~a~%")
                    (nix-protocol-error-message c)))
            ((message-condition? c)
             ;; Normally '&message' error conditions have an i18n'd message.
             (leave (_ "~a~%") (gettext (condition-message c)))))
    ;; Catch EPIPE and the likes.
    (catch 'system-error
      thunk
      (lambda (key proc format-string format-args . rest)
        (leave (_ "~a: ~a~%") proc
               (apply format #f format-string format-args))))))

(define %guix-user-module
  ;; Module in which user expressions are evaluated.
  ;; Compute lazily to avoid circularity with (guix gexp).
  (delay
    (let ((module (make-module)))
      (beautify-user-module! module)
      ;; Use (guix gexp) so that one can use #~ & co.
      (module-use! module (resolve-interface '(guix gexp)))
      module)))

(define (read/eval str)
  "Read and evaluate STR, raising an error if something goes wrong."
  (let ((exp (catch #t
               (lambda ()
                 (call-with-input-string str read))
               (lambda args
                 (leave (_ "failed to read expression ~s: ~s~%")
                        str args)))))
    (catch #t
      (lambda ()
        (eval exp (force %guix-user-module)))
      (lambda args
        (leave (_ "failed to evaluate expression `~a': ~s~%")
               exp args)))))

(define (read/eval-package-expression str)
  "Read and evaluate STR and return the package it refers to, or exit an
error."
  (match (read/eval str)
    ((? package? p) p)
    (_
     (leave (_ "expression ~s does not evaluate to a package~%")
            str))))

(define* (show-what-to-build store drv
                             #:key dry-run? (use-substitutes? #t))
  "Show what will or would (depending on DRY-RUN?) be built in realizing the
derivations listed in DRV.  Return #t if there's something to build, #f
otherwise.  When USE-SUBSTITUTES?, check and report what is prerequisites are
available for download."
  (define (built-or-substitutable? drv)
    (let ((out (derivation->output-path drv)))
      ;; If DRV has zero outputs, OUT is #f.
      (or (not out)
          (or (valid-path? store out)
              (and use-substitutes?
                   (has-substitutes? store out))))))

  (let*-values (((build download)
                 (fold2 (lambda (drv build download)
                          (let-values (((b d)
                                        (derivation-prerequisites-to-build
                                         store drv
                                         #:use-substitutes?
                                         use-substitutes?)))
                            (values (append b build)
                                    (append d download))))
                        '() '()
                        drv))
                ((build)                          ; add the DRV themselves
                 (delete-duplicates
                  (append (map derivation-file-name
                               (remove built-or-substitutable? drv))
                          (map derivation-input-path build))))
                ((download)                   ; add the references of DOWNLOAD
                 (if use-substitutes?
                     (delete-duplicates
                      (append download
                              (remove (cut valid-path? store <>)
                                      (append-map
                                       substitutable-references
                                       (substitutable-path-info store
                                                                download)))))
                     download)))
    ;; TODO: Show the installed size of DOWNLOAD.
    (if dry-run?
        (begin
          (format (current-error-port)
                  (N_ "~:[The following derivation would be built:~%~{   ~a~%~}~;~]"
                      "~:[The following derivations would be built:~%~{   ~a~%~}~;~]"
                      (length build))
                  (null? build) build)
          (format (current-error-port)
                  (N_ "~:[The following file would be downloaded:~%~{   ~a~%~}~;~]"
                      "~:[The following files would be downloaded:~%~{   ~a~%~}~;~]"
                      (length download))
                  (null? download) download))
        (begin
          (format (current-error-port)
                  (N_ "~:[The following derivation will be built:~%~{   ~a~%~}~;~]"
                      "~:[The following derivations will be built:~%~{   ~a~%~}~;~]"
                      (length build))
                  (null? build) build)
          (format (current-error-port)
                  (N_ "~:[The following file will be downloaded:~%~{   ~a~%~}~;~]"
                      "~:[The following files will be downloaded:~%~{   ~a~%~}~;~]"
                      (length download))
                  (null? download) download)))
    (pair? build)))

(define-syntax with-error-handling
  (syntax-rules ()
    "Run BODY within a user-friendly error condition handler."
    ((_ body ...)
     (call-with-error-handling
      (lambda ()
        body ...)))))

(define (location->string loc)
  "Return a human-friendly, GNU-standard representation of LOC."
  (match loc
    (#f (_ "<unknown location>"))
    (($ <location> file line column)
     (format #f "~a:~a:~a" file line column))))

(define (switch-symlinks link target)
  "Atomically switch LINK, a symbolic link, to point to TARGET.  Works
both when LINK already exists and when it does not."
  (let ((pivot (string-append link ".new")))
    (symlink target pivot)
    (rename-file pivot link)))

(define (config-directory)
  "Return the name of the configuration directory, after making sure that it
exists.  Honor the XDG specs,
<http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html>."
  (let ((dir (and=> (or (getenv "XDG_CONFIG_HOME")
                        (and=> (getenv "HOME")
                               (cut string-append <> "/.config")))
                    (cut string-append <> "/guix"))))
    (catch 'system-error
      (lambda ()
        (mkdir-p dir)
        dir)
      (lambda args
        (let ((err (system-error-errno args)))
          ;; ERR is necessarily different from EEXIST.
          (leave (_ "failed to create configuration directory `~a': ~a~%")
                 dir (strerror err)))))))

(define* (fill-paragraph str width #:optional (column 0))
  "Fill STR such that each line contains at most WIDTH characters, assuming
that the first character is at COLUMN.

When STR contains a single line break surrounded by other characters, it is
converted to a space; sequences of more than one line break are preserved."
  (define (maybe-break chr result)
    (match result
      ((column newlines chars)
       (case chr
         ((#\newline)
          `(,column ,(+ 1 newlines) ,chars))
         (else
          (let* ((spaces (if (and (pair? chars) (eqv? (car chars) #\.)) 2 1))
                 (chars  (case newlines
                           ((0) chars)
                           ((1)
                            (append (make-list spaces #\space) chars))
                           (else
                            (append (make-list newlines #\newline) chars))))
                 (column (case newlines
                           ((0) column)
                           ((1) (+ spaces column))
                           (else 0))))
            (let ((chars  (cons chr chars))
                  (column (+ 1 column)))
              (if (> column width)
                  (let*-values (((before after)
                                 (break (cut eqv? #\space <>) chars))
                                ((len)
                                 (length before)))
                    (if (<= len width)
                        `(,len
                          0
                          ,(if (null? after)
                               before
                               (append before
                                       (cons #\newline
                                             (drop-while (cut eqv? #\space <>)
                                                         after)))))
                        `(,column 0 ,chars)))     ; unbreakable
                  `(,column 0 ,chars)))))))))

  (match (string-fold maybe-break
                      `(,column 0 ())
                      str)
    ((_ _ chars)
     (list->string (reverse chars)))))


;;;
;;; Packages.
;;;

(define (string->recutils str)
  "Return a version of STR where newlines have been replaced by newlines
followed by \"+ \", which makes for a valid multi-line field value in the
`recutils' syntax."
  (list->string
   (string-fold-right (lambda (chr result)
                        (if (eqv? chr #\newline)
                            (cons* chr #\+ #\space result)
                            (cons chr result)))
                      '()
                      str)))

(define* (package->recutils p port
                            #:optional (width (or (and=> (getenv "WIDTH")
                                                         string->number)
                                                  80)))
  "Write to PORT a `recutils' record of package P, arranging to fit within
WIDTH columns."
  (define (description->recutils str)
    (let ((str (P_ str)))
      (string->recutils
       (fill-paragraph str width
                       (string-length "description: ")))))

  (define (dependencies->recutils packages)
    (let ((list (string-join (map package-full-name
                                  (sort packages package<?)) " ")))
      (string->recutils
       (fill-paragraph list width
                       (string-length "dependencies: ")))))

  (define (package<? p1 p2)
    (string<? (package-full-name p1) (package-full-name p2)))

  ;; Note: Don't i18n field names so that people can post-process it.
  (format port "name: ~a~%" (package-name p))
  (format port "version: ~a~%" (package-version p))
  (format port "dependencies: ~a~%"
          (match (package-direct-inputs p)
            (((labels inputs . _) ...)
             (dependencies->recutils (filter package? inputs)))))
  (format port "location: ~a~%"
          (or (and=> (package-location p) location->string)
              (_ "unknown")))

  ;; Note: Starting from version 1.6 or recutils, hyphens are not allowed in
  ;; field identifiers.
  (format port "homepage: ~a~%" (package-home-page p))

  (format port "license: ~a~%"
          (match (package-license p)
            (((? license? licenses) ...)
             (string-join (map license-name licenses)
                          ", "))
            ((? license? license)
             (license-name license))
            (x
             (_ "unknown"))))
  (format port "synopsis: ~a~%"
          (string-map (match-lambda
                       (#\newline #\space)
                       (chr       chr))
                      (or (and=> (package-synopsis p) P_)
                          "")))
  (format port "description: ~a~%"
          (and=> (package-description p) description->recutils))
  (newline port))

(define (string->generations str)
  "Return the list of generations matching a pattern in STR.  This function
accepts the following patterns: \"1\", \"1,2,3\", \"1..9\", \"1..\", \"..9\"."
  (define (maybe-integer)
    (let ((x (string->number str)))
      (and (integer? x)
           x)))

  (define (maybe-comma-separated-integers)
    (let ((lst (delete-duplicates
                (map string->number
                     (string-split str #\,)))))
      (and (every integer? lst)
           lst)))

  (cond ((maybe-integer)
         =>
         list)
        ((maybe-comma-separated-integers)
         =>
         identity)
        ((string-match "^([0-9]+)\\.\\.([0-9]+)$" str)
         =>
         (lambda (match)
           (let ((s (string->number (match:substring match 1)))
                 (e (string->number (match:substring match 2))))
             (and (every integer? (list s e))
                  (<= s e)
                  (iota (1+ (- e s)) s)))))
        ((string-match "^([0-9]+)\\.\\.$" str)
         =>
         (lambda (match)
           (let ((s (string->number (match:substring match 1))))
             (and (integer? s)
                  `(>= ,s)))))
        ((string-match "^\\.\\.([0-9]+)$" str)
         =>
         (lambda (match)
           (let ((e (string->number (match:substring match 1))))
             (and (integer? e)
                  `(<= ,e)))))
        (else #f)))

(define (string->duration str)
  "Return the duration matching a pattern in STR.  This function accepts the
following patterns: \"1d\", \"1w\", \"1m\"."
  (define (hours->duration hours match)
    (make-time time-duration 0
               (* 3600 hours (string->number (match:substring match 1)))))

  (cond ((string-match "^([0-9]+)d$" str)
         =>
         (lambda (match)
           (hours->duration 24 match)))
        ((string-match "^([0-9]+)w$" str)
         =>
         (lambda (match)
           (hours->duration (* 24 7) match)))
        ((string-match "^([0-9]+)m$" str)
         =>
         (lambda (match)
           (hours->duration (* 24 30) match)))
        (else #f)))

(define* (package-specification->name+version+output spec
                                                     #:optional (output "out"))
  "Parse package specification SPEC and return three value: the specified
package name, version number (or #f), and output name (or OUTPUT).  SPEC may
optionally contain a version number and an output name, as in these examples:

  guile
  guile-2.0.9
  guile:debug
  guile-2.0.9:debug
"
  (let*-values (((name sub-drv)
                 (match (string-rindex spec #\:)
                   (#f    (values spec output))
                   (colon (values (substring spec 0 colon)
                                  (substring spec (+ 1 colon))))))
                ((name version)
                 (package-name->name+version name)))
    (values name version sub-drv)))


;;;
;;; Command-line option processing.
;;;

(define (args-fold* options unrecognized-option-proc operand-proc . seeds)
  "A wrapper on top of `args-fold' that does proper user-facing error
reporting."
  (catch 'misc-error
    (lambda ()
      (apply args-fold options unrecognized-option-proc
             operand-proc seeds))
    (lambda (key proc msg args . rest)
      ;; XXX: MSG is not i18n'd.
      (leave (_ "invalid argument: ~a~%")
             (apply format #f msg args)))))

(define (show-guix-usage)
  (format (current-error-port)
          (_ "Try `guix --help' for more information.~%"))
  (exit 1))

(define (command-files)
  "Return the list of source files that define Guix sub-commands."
  (define directory
    (and=> (search-path %load-path "guix.scm")
           (compose (cut string-append <> "/guix/scripts")
                    dirname)))

  (define dot-scm?
    (cut string-suffix? ".scm" <>))

  ;; In Guile 2.0.5 `scandir' would return "." and ".." regardless even though
  ;; they don't match `dot-scm?'.  Work around it by doing additional
  ;; filtering.
  (if directory
      (filter dot-scm? (scandir directory dot-scm?))
      '()))

(define (commands)
  "Return the list of Guix command names."
  (map (compose (cut string-drop-right <> 4)
                basename)
       (command-files)))

(define (show-guix-help)
  (define (internal? command)
    (member command '("substitute-binary" "authenticate" "offload")))

  (format #t (_ "Usage: guix COMMAND ARGS...
Run COMMAND with ARGS.\n"))
  (newline)
  (format #t (_ "COMMAND must be one of the sub-commands listed below:\n"))
  (newline)
  ;; TODO: Display a synopsis of each command.
  (format #t "~{   ~a~%~}" (sort (remove internal? (commands))
                                 string<?))
  (show-bug-report-information))

(define program-name
  ;; Name of the command-line program currently executing, or #f.
  (make-parameter #f))

(define (run-guix-command command . args)
  "Run COMMAND with the given ARGS.  Report an error when COMMAND is not
found."
  (define module
    (catch 'misc-error
      (lambda ()
        (resolve-interface `(guix scripts ,command)))
      (lambda -
        (format (current-error-port)
                (_ "guix: ~a: command not found~%") command)
        (show-guix-usage))))

  (let ((command-main (module-ref module
                                  (symbol-append 'guix- command))))
    (parameterize ((program-name command))
      (apply command-main args))))

(define guix-warning-port
  (make-parameter (current-warning-port)))

(define (guix-main arg0 . args)
  (initialize-guix)
  (let ()
    (define (option? str) (string-prefix? "-" str))
    (match args
      (()
       (format (current-error-port)
               (_ "guix: missing command name~%"))
       (show-guix-usage))
      ((or ("-h") ("--help"))
       (show-guix-help))
      (("--version")
       (show-version-and-exit "guix"))
      (((? option? o) args ...)
       (format (current-error-port)
               (_ "guix: unrecognized option '~a'~%") o)
       (show-guix-usage))
      ((command args ...)
       (apply run-guix-command
              (string->symbol command)
              args)))))

;;; ui.scm ends here
