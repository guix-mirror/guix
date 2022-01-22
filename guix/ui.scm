;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014, 2015, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Benz Schenk <benz.schenk@uzh.ch>
;;; Copyright © 2018 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018 Steve Sprang <scs@stevesprang.com>
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
  #:use-module (guix i18n)
  #:use-module (guix colors)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix sets)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (guix config)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:use-module (guix serialization)
  #:use-module ((guix licenses)
                #:select (license? license-name license-uri))
  #:use-module ((guix build syscalls)
                #:select (free-disk-space terminal-columns terminal-rows
                          with-file-lock/no-wait))
  #:use-module ((guix build utils)
                ;; XXX: All we need are the bindings related to
                ;; '&invoke-error'.  However, to work around the bug described
                ;; in 5d669883ecc104403c5d3ba7d172e9c02234577c, #:hide
                ;; unwanted bindings instead of #:select'ing the needed
                ;; bindings.
                #:hide (package-name->name+version
                        ;; Avoid "overrides core binding" warning.
                        delete))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-31)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:autoload   (ice-9 ftw)  (scandir)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:autoload   (ice-9 popen) (open-pipe* close-pipe)
  #:autoload   (system repl repl)  (start-repl)
  #:autoload   (system repl debug) (make-debug stack->vector)
  #:autoload   (web uri) (encode-and-join-uri-path)
  #:use-module (texinfo)
  #:use-module (texinfo plain-text)
  #:use-module (texinfo string-utils)

  ;; Re-exports for backward compatibility.
  #:re-export (G_ N_ P_                           ;now in (guix i18n)

               warning info report-error leave    ;now in (guix diagnostics)
               location->string
               guix-warning-port program-name)
  #:export (display-hint
            make-user-module
            load*
            warn-about-load-error
            show-version-and-exit
            show-bug-report-information
            make-regexp*
            string->number*
            size->number
            show-derivation-outputs
            build-notifier
            show-what-to-build
            show-what-to-build*
            show-manifest-transaction
            guard*
            call-with-error-handling
            with-error-handling
            with-unbound-variable-handling
            leave-on-EPIPE
            read/eval
            read/eval-package-expression
            check-available-space
            indented-string
            fill-paragraph
            %text-width
            texi->plain-text
            package-description-string
            package-synopsis-string
            string->recutils
            package->recutils
            package-specification->name+version+output

            supports-hyperlinks?
            hyperlink
            file-hyperlink
            location->hyperlink

            with-paginated-output-port
            relevance
            package-relevance
            display-search-results

            with-profile-lock
            string->generations
            string->duration
            matching-generations
            display-generation
            display-profile-content
            display-profile-content-diff
            roll-back*
            switch-to-generation*
            delete-generation*

            %default-message-language
            current-message-language

            run-guix-command
            run-guix
            guix-main))

;;; Commentary:
;;;
;;; User interface facilities for command-line tools.
;;;
;;; Code:

(define (print-unbound-variable-error port key args default-printer)
  ;; Print unbound variable errors more nicely, and in the right language.
  (match args
    ((proc message (variable) _ ...)
     ;; We can always omit PROC because when it's useful (i.e., different from
     ;; "module-lookup"), it gets displayed before.
     (format port (G_ "error: ~a: unbound variable") variable))
    (_
     (default-printer))))

(set-exception-printer! 'unbound-variable print-unbound-variable-error)

(define (make-user-module modules)
  "Return a new user module with the additional MODULES loaded."
  ;; Module in which the machine description file is loaded.
  (let ((module (make-fresh-user-module)))
    (for-each (lambda (iface)
                (module-use! module (resolve-interface iface)))
              modules)
    module))

(define (last-frame-with-source stack)
  "Walk stack upwards and return the last frame that has source location
information, or #f if it could not be found."
  (define (frame-with-source frame)
    ;; Walk from FRAME upwards until source location information is found.
    (let loop ((frame    frame)
               (previous frame))
      (if (not frame)
          previous

          ;; On Guile 3, the latest frame with source may be that of
          ;; 'raise-exception' in boot-9.scm.  Skip it.
          (if (and (frame-source frame)
                   (not (eq? 'raise-exception (frame-procedure-name frame))))
              frame
              (loop (frame-previous frame) frame)))))

  (let* ((depth (stack-length stack))
         (last  (and (> depth 0) (stack-ref stack 0))))
    (frame-with-source (if (> depth 1)
                           (stack-ref stack 1)    ;skip the 'throw' frame
                           last))))

(define-syntax-rule (without-compiler-optimizations exp)
  ;; Compile with the baseline compiler (-O1), which is much less expensive
  ;; than -O2.
  (parameterize (((@ (system base compile) default-optimization-level) 1))
    exp))

(define* (load* file user-module
                #:key (on-error 'nothing-special))
  "Load the user provided Scheme source code FILE."
  (define (error-string frame args)
    (call-with-output-string
      (lambda (port)
        (apply display-error frame port (cdr args)))))

  (define tag
    (make-prompt-tag "user-code"))

  (catch #t
    (lambda ()
      ;; XXX: Force a recompilation to avoid ABI issues.
      (set! %fresh-auto-compile #t)
      (set! %load-should-auto-compile #t)

      (save-module-excursion
       (lambda ()
         (set-current-module user-module)

         ;; Hide the "auto-compiling" messages.
         (parameterize ((current-warning-port (%make-void-port "w")))
           (call-with-prompt tag
             (lambda ()
               ;; Give 'load' an absolute file name so that it doesn't try to
               ;; search for FILE in %LOAD-PATH.  Note: use 'load', not
               ;; 'primitive-load', so that FILE is compiled, which then allows
               ;; us to provide better error reporting with source line numbers.
               (without-compiler-optimizations
                (load (canonicalize-path file))))
             (const #f))))))
    (lambda _
      ;; XXX: Errors are reported from the pre-unwind handler below, but
      ;; calling 'exit' from there has no effect, so we call it here.
      (exit 1))
    (rec (handle-error . args)
         ;; Capture the stack up to this procedure call, excluded, and pass
         ;; the faulty stack frame to 'report-load-error'.
         (let* ((stack (make-stack #t handle-error tag))
                (frame (last-frame-with-source stack)))

           (report-load-error file args frame)

           (case on-error
             ((debug)
              (newline)
              (display (G_ "entering debugger; type ',bt' for a backtrace\n"))
              (start-repl #:debug (make-debug (stack->vector stack) 0
                                              (error-string frame args)
                                              #f)))
             ((backtrace)
              (newline (current-error-port))
              (display-backtrace stack (current-error-port)))
             (else
              #t))))))

(define (known-variable-definition variable)
  "Search among the currently loaded modules one that defines a variable named
VARIABLE and return it, or #f if none was found."
  (define (module<? m1 m2)
    (match (module-name m2)
      (('gnu _ ...) #t)
      (('guix _ ...)
       (match (module-name m1)
         (('gnu _ ...) #f)
         (_ #t)))
      (_ #f)))

  (let loop ((modules     (list (resolve-module '() #f #f #:ensure #f)))
             (suggestions '())
             (visited     (setq)))
    (match modules
      (()
       ;; Pick the "best" suggestion.
       (match (sort suggestions module<?)
         (() #f)
         ((first _ ...) first)))
      ((head tail ...)
       (if (set-contains? visited head)
           (loop tail suggestions visited)
           (let ((visited (set-insert head visited))
                 (next    (append tail
                                  (hash-map->list (lambda (name module)
                                                    module)
                                                  (module-submodules head)))))
             (match (and=> (module-public-interface head)
                           (cut module-local-variable <> variable))
               (#f (loop next suggestions visited))
               (_
                (match (module-name head)
                  (('gnu _ ...) head)             ;must be that one
                  (_ (loop next (cons head suggestions) visited)))))))))))

(define %hint-color (color BOLD CYAN))

(define* (display-hint message #:optional (port (current-error-port)))
  "Display MESSAGE, a l10n message possibly containing Texinfo markup, to
PORT."
  (define colorize
    (if (color-output? port)
        (lambda (str)
          (colorize-string str %hint-color))
        identity))

  (display (colorize (G_ "hint: ")) port)
  (display
   ;; XXX: We should arrange so that the initial indent is wider.
   (parameterize ((%text-width (max 15 (- (terminal-columns) 5))))
     (texi->plain-text message))
   port))

(define* (report-unbound-variable-error args #:key frame)
  "Return the given unbound-variable error, where ARGS is the list of 'throw'
arguments."
  (match args
    ((key . args)
     (print-exception (current-error-port) frame key args)))
  (match args
    (('unbound-variable proc message (variable) _ ...)
     (match (known-variable-definition variable)
       (#f
        (display-hint (G_ "Did you forget a @code{use-modules} form?")))
       ((? module? module)
        (display-hint (format #f (G_ "Did you forget @code{(use-modules ~a)}?")
                              (module-name module))))))))

(define (check-module-matches-file module file)
  "Check whether FILE starts with 'define-module MODULE' and print a hint if
it doesn't."
  ;; This is a common mistake when people start writing their own package
  ;; definitions and try loading them with 'guix build -L …', so help them
  ;; diagnose the problem.
  (define (hint)
    (display-hint (format #f (G_ "File @file{~a} should probably start with:

@example\n(define-module ~a)\n@end example")
                          file module)))

  (catch 'system-error
    (lambda ()
      (let* ((sexp (call-with-input-file file read))
             (loc  (and (pair? sexp)
                        (source-properties->location (source-properties sexp)))))
        (match sexp
          (('define-module (names ...) _ ...)
           (unless (equal? module names)
             (warning loc
                      (G_ "module name ~a does not match file name '~a'~%")
                      names (module->source-file-name module))
             (hint)))
          ((? eof-object?)
           (warning (G_ "~a: file is empty~%") file))
          (else
           (hint)))))
    (const #f)))

(define* (report-load-error file args #:optional frame)
  "Report the failure to load FILE, a user-provided Scheme file.
ARGS is the list of arguments received by the 'throw' handler."
  (match args
    (('system-error . rest)
     (let ((err (system-error-errno args)))
       (report-error (G_ "failed to load '~a': ~a~%") file (strerror err))))
    (('read-error _ message args ...)
     ;; Guile's missing-paren messages are obscure so we make them more
     ;; intelligible here.
     (if (or (string-suffix? "end of file" message) ;Guile < 3.0.6
             (and (string-contains message "unexpected end of input")
                  (member '(#\)) args)))
         (let ((location (string-take message
                                      (+ 2 (string-contains message ": ")))))
           (format (current-error-port) (G_ "~amissing closing parenthesis~%")
                   location))
         (report-error (G_ "read error while loading '~a': ~a~%")
                       file (apply format #f message args))))
    (('syntax-error proc message properties form subform . rest)
     (let ((loc (source-properties->location properties)))
       (report-error loc (G_ "~s: ~a~%")
                     (or subform form) message)))
    (('unbound-variable _ ...)
     (report-unbound-variable-error args #:frame frame))
    (((or 'srfi-34 '%exception) obj)
     (cond ((message-condition? obj)
            (report-error (and (error-location? obj)
                               (error-location obj))
                          (G_ "~a~%")
                          (gettext (condition-message obj) %gettext-domain)))
           ((formatted-message? obj)
            (apply report-error
                   (and (error-location? obj) (error-location obj))
                   (gettext (formatted-message-string obj) %gettext-domain)
                   (formatted-message-arguments obj)))
           (else
            (report-error (G_ "exception thrown: ~s~%") obj)))
     (when (fix-hint? obj)
       (display-hint (condition-fix-hint obj))))
    ((key args ...)
     (report-error (G_ "failed to load '~a':~%") file)
     (match args
       (((? symbol? proc) (? string? message) (args ...) . rest)
        (display-error frame (current-error-port) proc message
                       args rest))
       (_
        ;; Some exceptions like 'git-error' do not follow Guile's convention
        ;; above and need to be printed with 'print-exception'.
        (print-exception (current-error-port) frame key args))))))

(define (warn-about-load-error file module args)  ;FIXME: factorize with ↑
  "Report the failure to load FILE, a user-provided Scheme file, without
exiting.  ARGS is the list of arguments received by the 'throw' handler."
  (match args
    (('system-error . rest)
     (let ((err (system-error-errno args)))
       (warning (G_ "failed to load '~a': ~a~%") module (strerror err))))
    (('syntax-error proc message properties form . rest)
     (let ((loc (source-properties->location properties)))
       (warning loc (G_ "~a~%") message)))
    (('unbound-variable _ ...)
     (report-unbound-variable-error args))
    (((or 'srfi-34 '%exception) obj)
     (cond ((message-condition? obj)
            (warning (G_ "failed to load '~a': ~a~%")
                     file
                     (gettext (condition-message obj) %gettext-domain)))
           ((formatted-message? obj)
            (warning (G_ "failed to load '~a': ~a~%")
                     file
                     (apply format #f
                            (gettext (formatted-message-string obj)
                                     %gettext-domain)
                            (formatted-message-arguments obj))))
           (else
            (warning (G_ "failed to load '~a': exception thrown: ~s~%")
                     file obj))))
    ((error args ...)
     (warning (G_ "failed to load '~a':~%") module)
     (apply display-error #f (current-error-port) args)
     (check-module-matches-file module file))))

(define (call-with-unbound-variable-handling thunk)
  (define tag
    (make-prompt-tag "user-code"))

  (catch 'unbound-variable
    (lambda ()
      (call-with-prompt tag
        thunk
        (const #f)))
    (const #t)
    (rec (handle-error . args)
         (let* ((stack (make-stack #t handle-error tag))
                (frame (and stack (last-frame-with-source stack))))
           (report-unbound-variable-error args #:frame frame)
           (exit 1)))))

(define-syntax-rule (with-unbound-variable-handling exp ...)
  "Capture 'unbound-variable' exceptions in the dynamic extent of EXP... and
report them in a user-friendly way."
  (call-with-unbound-variable-handling (lambda () exp ...)))

(define %default-message-language
  ;; Default language to use for messages.
  (make-parameter "en"))

(define (current-message-language)
  "Return the language used for messages according to the current locale.
Return %DEFAULT-MESSAGE-LANGUAGE if that information could not be obtained.  The
result is an ISO-639-2 language code such as \"ar\", without the territory
part."
  (let ((locale (setlocale LC_MESSAGES)))
    (match (string-index locale #\_)
      (#f    locale)
      (index (string-take locale index)))))

(define (install-locale)
  "Install the current locale settings."
  (catch 'system-error
    (lambda _
      (setlocale LC_ALL ""))
    (lambda args
      (display-hint (G_ "Consider installing the @code{glibc-locales} package
and defining @code{GUIX_LOCPATH}, along these lines:

@example
guix install glibc-locales
export GUIX_LOCPATH=\"$HOME/.guix-profile/lib/locale\"
@end example

See the \"Application Setup\" section in the manual, for more info.\n"))
      ;; We're now running in the "C" locale.  Try to install a UTF-8 locale
      ;; instead.  This one is guaranteed to be available in 'guix' from 'guix
      ;; pull'.
      (false-if-exception (setlocale LC_ALL "en_US.utf8")))))

(define (initialize-guix)
  "Perform the usual initialization for stand-alone Guix commands."
  ;; By default don't annoy users with deprecation warnings.  In practice,
  ;; 'define-deprecated' in (ice-9 deprecated) arranges so that those warnings
  ;; are emitted at expansion-time only, but there are cases where they could
  ;; slip through, for instance when interpreting code.
  (unless (getenv "GUILE_WARN_DEPRECATED")
    (debug-disable 'warn-deprecated))

  (install-locale)
  (textdomain %gettext-domain)

  ;; Ignore SIGPIPE.  If the daemon closes the connection, we prefer to be
  ;; notified via an EPIPE later.
  (sigaction SIGPIPE SIG_IGN)

  (setvbuf (current-output-port) 'line)
  (setvbuf (current-error-port) 'line))

(define* (show-version-and-exit #:optional (command (car (command-line))))
  "Display version information for COMMAND and `(exit 0)'."
  (simple-format #t "~a (~a) ~a~%"
                 command %guix-package-name %guix-version)
  (format #t "Copyright ~a 2022 ~a"
          ;; TRANSLATORS: Translate "(C)" to the copyright symbol
          ;; (C-in-a-circle), if this symbol is available in the user's
          ;; locale.  Otherwise, do not translate "(C)"; leave it as-is.  */
          (G_ "(C)")
          (G_ "the Guix authors\n"))
  (display (G_"\
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
"))
  (exit 0))

(define (show-bug-report-information)
  ;; TRANSLATORS: The placeholder indicates the bug-reporting address for this
  ;; package.  Please add another line saying "Report translation bugs to
  ;; ...\n" with the address for translation bugs (typically your translation
  ;; team's web or email address).
  (format #t (G_ "
Report bugs to: ~a.") %guix-bug-report-address)
  (format #t (G_ "
~a home page: <~a>") %guix-package-name %guix-home-page-url)
  (format #t (G_ "
General help using Guix and GNU software: <~a>")
          ;; TRANSLATORS: Change the "/en" bit of this URL appropriately if
          ;; the web site is translated in your language.
          (G_ "https://guix.gnu.org/en/help/"))
  (newline))

(define (augmented-system-error-handler file)
  "Return a 'system-error' handler that mentions FILE in its message."
  (lambda (key proc fmt args errno)
    ;; Augment the FMT and ARGS with information about TARGET (this
    ;; information is missing as of Guile 2.0.11, making the exception
    ;; uninformative.)
    (apply throw key proc "~A: ~S"
           (list (strerror (car errno)) file)
           (list errno))))

(define-syntax apply-formals
  (syntax-rules ()
    ((_ proc (args ...)) (proc args ...))
    ((_ proc (arg1 args ... . rest)) (apply proc arg1 args ... rest))))

(define-syntax-rule (error-reporting-wrapper proc formals file)
  "Wrap PROC such that its 'system-error' exceptions are augmented to mention
FILE."
  (let ((real-proc (@ (guile) proc)))
    (lambda formals
      (catch 'system-error
        (lambda ()
          (apply-formals real-proc formals))
        (augmented-system-error-handler file)))))

(set! symlink
  ;; We 'set!' the global binding because (gnu build ...) modules and similar
  ;; typically don't use (guix ui).
  (error-reporting-wrapper symlink (source target) target))

(set! copy-file
  ;; Note: here we use 'set!', not #:replace, because UIs typically use
  ;; 'copy-recursively', which doesn't use (guix ui).
  (error-reporting-wrapper copy-file (source target) target))

(set! canonicalize-path
  (error-reporting-wrapper canonicalize-path (file) file))

(set! delete-file
  (error-reporting-wrapper delete-file (file) file))

(set! execlp
  (error-reporting-wrapper execlp (filename . args) filename))

(define (make-regexp* regexp . flags)
  "Like 'make-regexp' but error out if REGEXP is invalid, reporting the error
nicely."
  (catch 'regular-expression-syntax
    (lambda ()
      (apply make-regexp regexp flags))
    (lambda (key proc message . rest)
      (leave (G_ "'~a' is not a valid regular expression: ~a~%")
             regexp message))))

(define (string->number* str)
  "Like `string->number', but error out with an error message on failure."
  (or (string->number str)
      (leave (G_ "~a: invalid number~%") str)))

(define (size->number str)
  "Convert STR, a storage measurement representation such as \"1024\" or
\"1MiB\", to a number of bytes.  Raise an error if STR could not be
interpreted."
  (define unit-pos
    (string-rindex str
                   (char-set-union (char-set #\.) char-set:digit)))

  (define unit
    (and unit-pos (substring str (+ 1 unit-pos))))

  (let* ((numstr (if unit-pos
                     (substring str 0 (+ 1 unit-pos))
                     str))
         (num    (string->number numstr)))
    (unless num
      (leave (G_ "invalid number: ~a~%") numstr))

    ((compose inexact->exact round)
     (* num
        (match unit
          ((or "KiB" "K" "k") (expt 2 10))
          ((or "MiB" "M")     (expt 2 20))
          ((or "GiB" "G")     (expt 2 30))
          ((or "TiB" "T")     (expt 2 40))
          ((or "PiB" "P")     (expt 2 50))
          ((or "EiB" "E")     (expt 2 60))
          ((or "ZiB" "Z")     (expt 2 70))
          ((or "YiB" "Y")     (expt 2 80))
          ("kB"  (expt 10 3))
          ("MB"  (expt 10 6))
          ("GB"  (expt 10 9))
          ("TB"  (expt 10 12))
          ("PB"  (expt 10 15))
          ("EB"  (expt 10 18))
          ("ZB"  (expt 10 21))
          ("YB"  (expt 10 24))
          (""    1)
          (x
           (leave (G_ "unknown unit: ~a~%") unit)))))))

(define (display-collision-resolution-hint collision)
  "Display hints on how to resolve COLLISION, a &profile-collistion-error."
  (define (top-most-entry entry)
    (let loop ((entry entry))
      (match (force (manifest-entry-parent entry))
        (#f entry)
        (parent (loop parent)))))

  (let* ((first  (profile-collision-error-entry collision))
         (second (profile-collision-error-conflict collision))
         (name1  (manifest-entry-name (top-most-entry first)))
         (name2  (manifest-entry-name (top-most-entry second))))
    (if (string=? name1 name2)
        (display-hint (format #f (G_ "You cannot have two different versions
or variants of @code{~a} in the same profile.")
                              name1))
        (display-hint (format #f (G_ "Try upgrading both @code{~a} and @code{~a},
or remove one of them from the profile.")
                              name1 name2)))))

;; On Guile 3.0, in 'call-with-error-handling' we need to re-raise.  To
;; preserve useful backtraces in case of unhandled errors, we want that to
;; happen before the stack has been unwound, hence 'guard*'.
(define-syntax-rule (guard* (var clauses ...) exp ...)
  "This variant of SRFI-34 'guard' does not unwind the stack before
evaluating the tests and bodies of CLAUSES."
  (with-exception-handler
      (lambda (var)
        (cond clauses ... (else (raise var))))
    (lambda () exp ...)
    #:unwind? #f))

(define (call-with-error-handling thunk)
  "Call THUNK within a user-friendly error handler."
  (define (port-filename* port)
    ;; 'port-filename' returns #f for non-file ports, but it raises an
    ;; exception for file ports that are closed.  Work around that.
    (and (not (port-closed? port))
         (port-filename port)))

  (guard* (c ((package-input-error? c)
              (let* ((package  (package-error-package c))
                     (input    (package-error-invalid-input c))
                     (location (package-location package))
                     (file     (location-file location))
                     (line     (location-line location))
                     (column   (location-column location)))
                (leave (G_ "~a:~a:~a: package `~a' has an invalid input: ~s~%")
                       file line column
                       (package-full-name package) input)))
             ((package-cross-build-system-error? c)
              (let* ((package (package-error-package c))
                     (loc     (package-location package))
                     (system  (package-build-system package)))
                (leave (G_ "~a: ~a: build system `~a' does not support cross builds~%")
                       (location->string loc)
                       (package-full-name package)
                       (build-system-name system))))
             ((gexp-input-error? c)
              (let ((input (gexp-error-invalid-input c)))
                (leave (G_ "~s: invalid G-expression input~%")
                       (gexp-error-invalid-input c))))
             ((profile-not-found-error? c)
              (leave (G_ "profile '~a' does not exist~%")
                     (profile-error-profile c)))
             ((missing-generation-error? c)
              (leave (G_ "generation ~a of profile '~a' does not exist~%")
                     (missing-generation-error-generation c)
                     (profile-error-profile c)))
             ((unmatched-pattern-error? c)
              (let ((pattern (unmatched-pattern-error-pattern c)))
                (leave (G_ "package '~a~@[@~a~]~@[:~a~]' not found in profile~%")
                       (manifest-pattern-name pattern)
                       (manifest-pattern-version pattern)
                       (match (manifest-pattern-output pattern)
                         ("out" #f)
                         (output output)))))
             ((profile-collision-error? c)
              (let ((entry    (profile-collision-error-entry c))
                    (conflict (profile-collision-error-conflict c)))
                (define (report-parent-entries entry)
                  (let ((parent (force (manifest-entry-parent entry))))
                    (when (manifest-entry? parent)
                      (report-error (G_ "   ... propagated from ~a@~a~%")
                                    (manifest-entry-name parent)
                                    (manifest-entry-version parent))
                      (report-parent-entries parent))))

                (define (manifest-entry-output* entry)
                  (match (manifest-entry-output entry)
                    ("out"   "")
                    (output (string-append ":" output))))

                (report-error (G_ "profile contains conflicting entries for ~a~a~%")
                              (manifest-entry-name entry)
                              (manifest-entry-output* entry))
                (report-error (G_ "  first entry: ~a@~a~a ~a~%")
                              (manifest-entry-name entry)
                              (manifest-entry-version entry)
                              (manifest-entry-output* entry)
                              (manifest-entry-item entry))
                (report-parent-entries entry)
                (report-error (G_ "  second entry: ~a@~a~a ~a~%")
                              (manifest-entry-name conflict)
                              (manifest-entry-version conflict)
                              (manifest-entry-output* conflict)
                              (manifest-entry-item conflict))
                (report-parent-entries conflict)
                (display-collision-resolution-hint c)
                (exit 1)))
             ((nar-error? c)
              (let ((file (nar-error-file c))
                    (port (nar-error-port c)))
                (if file
                    (leave (G_ "corrupt input while restoring '~a' from ~s~%")
                           file (or (port-filename* port) port))
                    (leave (G_ "corrupt input while restoring archive from ~s~%")
                           (or (port-filename* port) port)))))
             ((store-connection-error? c)
              (leave (G_ "failed to connect to `~a': ~a~%")
                     (store-connection-error-file c)
                     (strerror (store-connection-error-code c))))
             ((store-protocol-error? c)
              ;; FIXME: Server-provided error messages aren't i18n'd.
              (leave (G_ "~a~%")
                     (store-protocol-error-message c)))
             ((derivation-missing-output-error? c)
              (leave (G_ "reference to invalid output '~a' of derivation '~a'~%")
                     (derivation-missing-output c)
                     (derivation-file-name (derivation-error-derivation c))))
             ((file-search-error? c)
              (leave (G_ "file '~a' could not be found in these \
directories:~{ ~a~}~%")
                     (file-search-error-file-name c)
                     (file-search-error-search-path c)))
             ((invoke-error? c)
              (leave (G_ "program exited\
~@[ with non-zero exit status ~a~]\
~@[ terminated by signal ~a~]\
~@[ stopped by signal ~a~]: ~s~%")
                     (invoke-error-exit-status c)
                     (invoke-error-term-signal c)
                     (invoke-error-stop-signal c)
                     (cons (invoke-error-program c)
                           (invoke-error-arguments c))))

             ((formatted-message? c)
              (apply report-error
                     (and (error-location? c) (error-location c))
                     (gettext (formatted-message-string c) %gettext-domain)
                     (formatted-message-arguments c))
              (when (fix-hint? c)
                (display-hint (condition-fix-hint c)))
              (exit 1))

             ;; On Guile 3.0.0, exceptions such as 'unbound-variable' are
             ;; compound and include a '&message'.  However, that message only
             ;; contains the format string.  Thus, special-case it here to
             ;; avoid displaying a bare format string.
             ;;
             ;; Furthermore, use of 'guard*' ensures that the stack has not
             ;; been unwound when we re-raise, since that would otherwise show
             ;; useless backtraces.
             (((exception-predicate &exception-with-kind-and-args) c)
              (if (eq? 'system-error (exception-kind c)) ;EPIPE & co.
                  (match (exception-args c)
                    ((proc format-string format-args . _)
                     (leave (G_ "~a: ~a~%") proc
                            (apply format #f format-string format-args))))
                  (raise c)))

             ((message-condition? c)
              ;; Normally '&message' error conditions have an i18n'd message.
              (report-error (and (error-location? c) (error-location c))
                            (G_ "~a~%")
                            (gettext (condition-message c) %gettext-domain))
              (when (fix-hint? c)
                (display-hint (condition-fix-hint c)))
              (exit 1)))
      (thunk)))

(define-syntax-rule (leave-on-EPIPE exp ...)
  "Run EXP... in a context where EPIPE errors are caught and lead to 'exit'
with successful exit code.  This is useful when writing to the standard output
may lead to EPIPE, because the standard output is piped through 'head' or
similar."
  (catch 'system-error
    (lambda ()
      exp ...)
    (lambda args
      ;; We really have to exit this brutally, otherwise Guile eventually
      ;; attempts to flush all the ports, leading to an uncaught EPIPE down
      ;; the path.
      (if (= EPIPE (system-error-errno args))
          (primitive-_exit 0)
          (apply throw args)))))

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
                 (leave (G_ "failed to read expression ~s: ~s~%")
                        str args)))))
    (catch #t
      (lambda ()
        (eval exp (force %guix-user-module)))
      (lambda args
        (report-error (G_ "failed to evaluate expression '~a':~%") exp)
        (match args
          (('syntax-error proc message properties form . rest)
           (report-error (G_ "syntax error: ~a~%") message))
          (((or 'srfi-34 '%exception) obj)
           (cond ((message-condition? obj)
                  (report-error (G_ "~a~%")
                                (gettext (condition-message obj)
                                         %gettext-domain)))
                 ((formatted-message? obj)
                  (apply report-error #f
                         (gettext (formatted-message-string obj)
                                  %gettext-domain)
                         (formatted-message-arguments obj)))
                 (else
                  (report-error (G_ "exception thrown: ~s~%") obj))))
          ((error args ...)
           (apply display-error #f (current-error-port) args))
          (what? #f))
        (exit 1)))))

(define (read/eval-package-expression str)
  "Read and evaluate STR and return the package it refers to, or exit an
error."
  (match (read/eval str)
    ((? package? p) p)
    (x
     (leave (G_ "expression ~s does not evaluate to a package~%")
            str))))

(define (show-derivation-outputs derivation)
  "Show the output file names of DERIVATION, which can be a derivation or a
derivation input."
  (define (show-outputs derivation outputs)
    (format #t "~{~a~%~}"
            (map (cut derivation->output-path derivation <>)
                 outputs)))

  (match derivation
    ((? derivation?)
     (show-outputs derivation (derivation-output-names derivation)))
    ((? derivation-input? input)
     (show-outputs (derivation-input-derivation input)
                   (derivation-input-sub-derivations input)))))

(define* (check-available-space need
                                #:optional (directory (%store-prefix)))
  "Make sure at least NEED bytes are available in DIRECTORY.  Otherwise emit a
warning."
  (let ((free (catch 'system-error
                (lambda ()
                  (free-disk-space directory))
                (const #f))))
    (when (and free (>= need free))
      (warning (G_ "at least ~,1h MB needed but only ~,1h MB available in ~a~%")
               (/ need 1e6) (/ free 1e6) directory))))

(define (graft-derivation? drv)
  "Return true if DRV is definitely a graft derivation, false otherwise."
  (match (assq-ref (derivation-properties drv) 'type)
    ('graft #t)
    (_ #f)))

(define (profile-hook-derivation? drv)
  "Return true if DRV is definitely a profile hook derivation, false otherwise."
  (match (assq-ref (derivation-properties drv) 'type)
    ('profile-hook #t)
    (_ #f)))

(define (colorize-store-file-name file)
  "Colorize FILE, a store file name, such that the hash part is less prominent
than the rest."
  (let ((len    (string-length file))
        (prefix (+ (string-length (%store-prefix)) 32 2)))
    (if (< len prefix)
        file
        (string-append (colorize-string (string-take file prefix)
                                        (color DARK))
                       (string-drop file prefix)))))

(define %default-verbosity
  ;; Default verbosity level for 'show-what-to-build'.
  2)

(define* (show-what-to-build store drv
                             #:key dry-run? (use-substitutes? #t)
                             (verbosity %default-verbosity)
                             (mode (build-mode normal)))
  "Show what will or would (depending on DRY-RUN?) be built in realizing the
derivations listed in DRV using MODE, a 'build-mode' value.  The elements of
DRV can be either derivations or derivation inputs.

Return two values: a Boolean indicating whether there's something to build,
and a Boolean indicating whether there's something to download.

When USE-SUBSTITUTES?, check and report what is prerequisites are available
for download.  VERBOSITY is an integer indicating the level of details to be
shown: level 2 and higher provide all the details, level 1 shows a high-level
summary, and level 0 shows nothing."
  (define inputs
    (map (match-lambda
           ((? derivation? drv) (derivation-input drv))
           ((? derivation-input? input) input))
         drv))

  (define substitutable-info
    ;; Call 'substitution-oracle' upfront so we don't end up launching the
    ;; substituter many times.  This makes a big difference, especially when
    ;; DRV is a long list as is the case with 'guix environment'.
    (if use-substitutes?
        (substitution-oracle store inputs #:mode mode)
        (const #f)))

  (define colorized-store-item
    (if (color-output? (current-error-port))
        colorize-store-file-name
        identity))

  (let*-values (((build/full download)
                 (derivation-build-plan store inputs
                                        #:mode mode
                                        #:substitutable-info
                                        substitutable-info))
                ((graft hook build)
                 (match (fold (lambda (drv acc)
                                (let ((file (derivation-file-name drv)))
                                  (match acc
                                    ((#:graft graft #:hook hook #:build build)
                                     (cond
                                      ((graft-derivation? drv)
                                       `(#:graft ,(cons file graft)
                                         #:hook ,hook
                                         #:build ,build))
                                      ((profile-hook-derivation? drv)
                                       `(#:graft ,graft
                                         #:hook ,(cons file hook)
                                         #:build ,build))
                                      (else
                                       `(#:graft ,graft
                                         #:hook ,hook
                                         #:build ,(cons file build))))))))
                              '(#:graft () #:hook () #:build ())
                              build/full)
                   ((#:graft graft #:hook hook #:build build)
                    (values graft hook build)))))
    (define installed-size
      (reduce + 0 (map substitutable-nar-size download)))

    (define download-size
      (/ (reduce + 0 (map substitutable-download-size download))
         1e6))

    (define display-download-size?
      ;; Sometimes narinfos lack information about the download size.  Only
      ;; display when we have information for all of DOWNLOAD.
      (not (any (compose zero? substitutable-download-size) download)))

    ;; Combinatorial explosion ahead along two axes: DRY-RUN? and VERBOSITY.
    ;; Unfortunately, this is hardly avoidable for proper i18n.
    (if dry-run?
        (begin
          (unless (zero? verbosity)
            (format (current-error-port)
                    (N_ "~:[The following derivation would be built:~%~{   ~a~%~}~;~]"
                        "~:[The following derivations would be built:~%~{   ~a~%~}~;~]"
                        (length build))
                    (null? build) (map colorized-store-item build)))
          (cond ((>= verbosity 2)
                 (if display-download-size?
                     (format (current-error-port)
                             ;; TRANSLATORS: "MB" is for "megabyte"; it should be
                             ;; translated to the corresponding abbreviation.
                             (G_ "~:[~,1h MB would be downloaded:~%~{   ~a~%~}~;~]")
                             (null? download)
                             download-size
                             (map (compose colorized-store-item substitutable-path)
                                  download))
                     (format (current-error-port)
                             (N_ "~:[The following file would be downloaded:~%~{   ~a~%~}~;~]"
                                 "~:[The following files would be downloaded:~%~{   ~a~%~}~;~]"
                                 (length download))
                             (null? download)
                             (map (compose colorized-store-item substitutable-path)
                                  download)))
                 (format (current-error-port)
                         (N_ "~:[The following graft would be made:~%~{   ~a~%~}~;~]"
                             "~:[The following grafts would be made:~%~{   ~a~%~}~;~]"
                             (length graft))
                         (null? graft) (map colorized-store-item graft))
                 (format (current-error-port)
                         (N_ "~:[The following profile hook would be built:~%~{   ~a~%~}~;~]"
                             "~:[The following profile hooks would be built:~%~{   ~a~%~}~;~]"
                             (length hook))
                         (null? hook) (map colorized-store-item hook)))
                ((= verbosity 1)
                 ;; Display the bare minimum; don't mention grafts and hooks.
                 (unless (null? build)
                   (newline (current-error-port)))
                 (if display-download-size?
                     (format (current-error-port)
                             ;; TRANSLATORS: "MB" is for "megabyte"; it should be
                             ;; translated to the corresponding abbreviation.
                             (highlight (G_ "~:[~,1h MB would be downloaded~%~;~]"))
                             (null? download) download-size)
                     (format (current-error-port)
                             (highlight
                              (N_ "~:[~h item would be downloaded~%~;~]"
                                  "~:[~h items would be downloaded~%~;~]"
                                  (length download)))
                             (null? download) (length download))))))

        (begin
          (unless (zero? verbosity)
            (format (current-error-port)
                    (N_ "~:[The following derivation will be built:~%~{   ~a~%~}~;~]"
                        "~:[The following derivations will be built:~%~{   ~a~%~}~;~]"
                        (length build))
                    (null? build) (map colorized-store-item build)))
          (cond ((>= verbosity 2)
                 (if display-download-size?
                     (format (current-error-port)
                             ;; TRANSLATORS: "MB" is for "megabyte"; it should be
                             ;; translated to the corresponding abbreviation.
                             (G_ "~:[~,1h MB will be downloaded:~%~{   ~a~%~}~;~]")
                             (null? download)
                             download-size
                             (map (compose colorized-store-item substitutable-path)
                                  download))
                     (format (current-error-port)
                             (N_ "~:[The following file will be downloaded:~%~{   ~a~%~}~;~]"
                                 "~:[The following files will be downloaded:~%~{   ~a~%~}~;~]"
                                 (length download))
                             (null? download)
                             (map (compose colorized-store-item substitutable-path)
                                  download)))
                 (format (current-error-port)
                         (N_ "~:[The following graft will be made:~%~{   ~a~%~}~;~]"
                             "~:[The following grafts will be made:~%~{   ~a~%~}~;~]"
                             (length graft))
                         (null? graft) (map colorized-store-item graft))
                 (format (current-error-port)
                         (N_ "~:[The following profile hook will be built:~%~{   ~a~%~}~;~]"
                             "~:[The following profile hooks will be built:~%~{   ~a~%~}~;~]"
                             (length hook))
                         (null? hook) (map colorized-store-item hook)))
                ((= verbosity 1)
                 ;; Display the bare minimum; don't mention grafts and hooks.
                 (unless (null? build)
                   (newline (current-error-port)))
                 (if display-download-size?
                     (format (current-error-port)
                             ;; TRANSLATORS: "MB" is for "megabyte"; it should be
                             ;; translated to the corresponding abbreviation.
                             (highlight (G_ "~:[~,1h MB will be downloaded~%~;~]"))
                             (null? download) download-size)
                     (format (current-error-port)
                             (highlight
                              (N_ "~:[~h item will be downloaded~%~;~]"
                                  "~:[~h items will be downloaded~%~;~]"
                                  (length download)))
                             (null? download) (length download)))))))

    (check-available-space installed-size)

    (values (pair? build/full) (pair? download))))

(define show-what-to-build*
  (store-lift show-what-to-build))

(define* (build-notifier #:key (dry-run? #f) (use-substitutes? #t)
                         (verbosity %default-verbosity))
  "Return a procedure suitable for 'with-build-handler' that, when
'build-things' is called, invokes 'show-what-to-build' to display the build
plan.  When DRY-RUN? is true, the 'with-build-handler' form returns without
any build happening."
  (define not-comma
    (char-set-complement (char-set #\,)))

  (define (read-derivation-from-file* item)
    (catch 'system-error
      (lambda ()
        (read-derivation-from-file item))
      (const #f)))

  (lambda (continue store things mode)
    (define inputs
      ;; List of derivation inputs to build.  Filter out non-existent '.drv'
      ;; files because the daemon transparently tries to substitute them.
      (filter-map (match-lambda
                    (((? derivation-path? drv) . output)
                     (let ((drv     (read-derivation-from-file* drv))
                           (outputs (string-tokenize output not-comma)))
                       (and drv (derivation-input drv outputs))))
                    ((? derivation-path? drv)
                     (and=> (read-derivation-from-file* drv)
                            derivation-input))
                    (_
                     #f))
                  things))

    (let-values (((build? download?)
                  (show-what-to-build store inputs
                                      #:dry-run? dry-run?
                                      #:use-substitutes? use-substitutes?
                                      #:verbosity verbosity
                                      #:mode mode)))

      (unless (and (or build? download?)
                   dry-run?)
        (continue #t)))))

(define (right-arrow port)
  "Return either a string containing the 'RIGHT ARROW' character, or an ASCII
replacement if PORT is not Unicode-capable."
  (let ((encoding (port-encoding port))
        (arrow "→"))
    (catch 'encoding-error
      (lambda ()
        (call-with-output-string
          (lambda (port)
            (set-port-encoding! port encoding)
            (set-port-conversion-strategy! port 'error)
            (display arrow port))))
      (lambda (key . args)
        "->"))))

(define* (tabulate rows #:key (initial-indent 0) (max-width 25)
                   (inter-column " "))
  "Return a list of strings where each string is a tabulated representation of
an element of ROWS.  All the ROWS must be lists of the same number of cells.

Add INITIAL-INDENT white space at the beginning of each row.  Ensure that
columns are at most MAX-WIDTH characters wide.  Use INTER-COLUMN as a
separator between subsequent columns."
  (define column-widths
    ;; List of column widths.
    (let loop ((rows rows)
               (widths '()))
      (match rows
        (((? null?) ...)
         (reverse widths))
        (((column rest ...) ...)
         (loop rest
               (cons (min (apply max (map string-length column))
                          max-width)
                     widths))))))

  (define indent
    (make-string initial-indent #\space))

  (define (string-pad-right* str len)
    (if (> (string-length str) len)
        str
        (string-pad-right str len)))

  (map (lambda (row)
         (string-trim-right
          (string-append indent
                         (string-join
                          (map string-pad-right* row column-widths)
                          inter-column))))
       rows))

(define* (show-manifest-transaction store manifest transaction
                                    #:key dry-run?)
  "Display what will/would be installed/removed from MANIFEST by TRANSACTION."
  (define* (package-strings names versions outputs #:key old-versions)
    (tabulate (stable-sort
               (zip (map (lambda (name output)
                           (if (string=? output "out")
                               name
                               (string-append name ":" output)))
                         names outputs)
                    (if old-versions
                        (map (lambda (old new)
                               (if (string=? old new)
                                   (G_ "(dependencies or package changed)")
                                   (string-append old " " → " " new)))
                             old-versions versions)
                        versions))
               (lambda (x y)
                 (string<? (first x) (first y))))
              #:initial-indent 3))

  (define →                        ;an arrow that can be represented on stderr
    (right-arrow (current-error-port)))

  (let-values (((remove install upgrade downgrade)
                (manifest-transaction-effects manifest transaction)))
    (match remove
      ((($ <manifest-entry> name version output item) ..1)
       (let ((len    (length name))
             (remove (package-strings name version output)))
         (if dry-run?
             (format (current-error-port)
                     (N_ "The following package would be removed:~%~{~a~%~}~%"
                         "The following packages would be removed:~%~{~a~%~}~%"
                         len)
                     remove)
             (format (current-error-port)
                     (N_ "The following package will be removed:~%~{~a~%~}~%"
                         "The following packages will be removed:~%~{~a~%~}~%"
                         len)
                     remove))))
      (x #f))
    (match downgrade
      (((($ <manifest-entry> name old-version)
         . ($ <manifest-entry> _ new-version output item)) ..1)
       (let ((len       (length name))
             (downgrade (package-strings name new-version output
                                         #:old-versions old-version)))
         (if dry-run?
             (format (current-error-port)
                     (N_ "The following package would be downgraded:~%~{~a~%~}~%"
                         "The following packages would be downgraded:~%~{~a~%~}~%"
                         len)
                     downgrade)
             (format (current-error-port)
                     (N_ "The following package will be downgraded:~%~{~a~%~}~%"
                         "The following packages will be downgraded:~%~{~a~%~}~%"
                         len)
                     downgrade))))
      (x #f))
    (match upgrade
      (((($ <manifest-entry> name old-version)
         . ($ <manifest-entry> _ new-version output item)) ..1)
       (let ((len     (length name))
             (upgrade (package-strings name new-version output
                                       #:old-versions old-version)))
         (if dry-run?
             (format (current-error-port)
                     (N_ "The following package would be upgraded:~%~{~a~%~}~%"
                         "The following packages would be upgraded:~%~{~a~%~}~%"
                         len)
                     upgrade)
             (format (current-error-port)
                     (N_ "The following package will be upgraded:~%~{~a~%~}~%"
                         "The following packages will be upgraded:~%~{~a~%~}~%"
                         len)
                     upgrade))))
      (x #f))
    (match install
      ((($ <manifest-entry> name version output item _) ..1)
       (let ((len     (length name))
             (install (package-strings name version output)))
         (if dry-run?
             (format (current-error-port)
                     (N_ "The following package would be installed:~%~{~a~%~}~%"
                         "The following packages would be installed:~%~{~a~%~}~%"
                         len)
                     install)
             (format (current-error-port)
                     (N_ "The following package will be installed:~%~{~a~%~}~%"
                         "The following packages will be installed:~%~{~a~%~}~%"
                         len)
                     install))))
      (x #f))))

(define-syntax with-error-handling
  (syntax-rules ()
    "Run BODY within a user-friendly error condition handler."
    ((_ body ...)
     (call-with-error-handling
      (lambda ()
        body ...)))))

(define* (indented-string str indent
                          #:key (initial-indent? #t))
  "Return STR with each newline preceded by INDENT spaces.  When
INITIAL-INDENT? is true, the first line is also indented."
  (define indent-string
    (make-list indent #\space))

  (list->string
   (string-fold-right (lambda (chr result)
                        (if (eqv? chr #\newline)
                            (cons chr (append indent-string result))
                            (cons chr result)))
                      '()
                      (if initial-indent?
                          (string-append (list->string indent-string) str)
                          str))))

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
    ((column newlines chars)
     (list->string (reverse chars)))))


;;;
;;; Packages.
;;;

(define %text-width
  ;; '*line-width*' was introduced in Guile 2.2.7/3.0.1.  On older versions of
  ;; Guile, monkey-patch 'wrap*' below.
  (if (defined? '*line-width*)
      (let ((parameter (fluid->parameter *line-width*)))
        (parameter (terminal-columns))
        parameter)
      (make-parameter (terminal-columns))))

(unless (defined? '*line-width*)                  ;Guile < 2.2.7
  (set! (@@ (texinfo plain-text) wrap*)
    ;; XXX: Monkey patch this private procedure to let 'package->recutils'
    ;; parameterize the fill of description field correctly.
    (lambda strings
      (let ((indent (fluid-ref (@@ (texinfo plain-text) *indent*))))
        (fill-string (string-concatenate strings)
                     #:line-width (%text-width) #:initial-indent indent
                     #:subsequent-indent indent)))))

(define (texi->plain-text str)
  "Return a plain-text representation of texinfo fragment STR."
  ;; 'texi-fragment->stexi' uses a string port so make sure it's a
  ;; Unicode-capable one (see <http://bugs.gnu.org/11197>.)
  (with-fluids ((%default-port-encoding "UTF-8"))
    (stexi->plain-text (texi-fragment->stexi str))))

(define (texi->plain-text* package str)
  "Same as 'texi->plain-text', but gracefully handle Texinfo errors."
  (catch 'parser-error
    (lambda ()
      (texi->plain-text str))
    (lambda args
      (warning (package-location package)
               (G_ "~a: invalid Texinfo markup~%")
               (package-full-name package))
      str)))

(define (package-field-string package field-accessor)
  "Return a plain-text representation of PACKAGE field."
  (and=> (field-accessor package)
         (lambda (str)
           (texi->plain-text* package (P_ str)))))

(define (package-description-string package)
  "Return a plain-text representation of PACKAGE description field."
  (package-field-string package package-description))

(define (package-synopsis-string package)
  "Return a plain-text representation of PACKAGE synopsis field."
  (package-field-string package package-synopsis))

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

(define (hyperlink uri text)
  "Return a string that denotes a hyperlink using an OSC escape sequence as
documented at
<https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda>."
  (string-append "\x1b]8;;" uri "\x1b\\"
                 text "\x1b]8;;\x1b\\"))

(define* (supports-hyperlinks? #:optional (port (current-output-port)))
  "Return true if PORT is a terminal that supports hyperlink escapes."
  ;; Note that terminals are supposed to ignore OSC escapes they don't
  ;; understand (this is the case of xterm as of version 349, for instance.)
  ;; However, Emacs comint as of 26.3 does not ignore it and instead lets it
  ;; through, hence the 'INSIDE_EMACS' special case below.
  (and (isatty?* port)
       (not (getenv "INSIDE_EMACS"))))

(define* (file-hyperlink file #:optional (text file))
  "Return TEXT with escapes for a hyperlink to FILE."
  (hyperlink (string-append "file://" (gethostname)
                            (encode-and-join-uri-path
                             (string-split file #\/)))
             text))

(define (location->hyperlink location)
  "Return a string corresponding to LOCATION, with escapes for a hyperlink."
  (let ((str  (location->string location))
        (file (if (string-prefix? "/" (location-file location))
                  (location-file location)
                  (search-path %load-path (location-file location)))))
    (if file
        (file-hyperlink file str)
        str)))

(define* (package->recutils p port #:optional (width (%text-width))
                            #:key
                            (hyperlinks? (supports-hyperlinks? port))
                            (extra-fields '()))
  "Write to PORT a `recutils' record of package P, arranging to fit within
WIDTH columns.  EXTRA-FIELDS is a list of symbol/value pairs to emit.  When
HYPERLINKS? is true, emit hyperlink escape sequences when appropriate."
  (define width*
    ;; The available number of columns once we've taken into account space for
    ;; the initial "+ " prefix.
    (if (> width 2) (- width 2) width))

  (define (split-lines str indent)
    (string->recutils
     (fill-paragraph str width* indent)))

  (define (dependencies->recutils packages)
    (let ((list (string-join (delete-duplicates
                              (map package-full-name
                                   (sort packages package<?))) " ")))
      (split-lines list (string-length "dependencies: "))))

  (define (package<? p1 p2)
    (string<? (package-full-name p1) (package-full-name p2)))

  ;; Note: Don't i18n field names so that people can post-process it.
  (format port "name: ~a~%" (package-name p))
  (format port "version: ~a~%" (package-version p))
  (format port "outputs: ~a~%" (string-join (package-outputs p)))
  (format port "systems: ~a~%"
          (split-lines (string-join (package-transitive-supported-systems p))
                       (string-length "systems: ")))
  (format port "dependencies: ~a~%"
          (match (package-direct-inputs p)
            (((labels inputs . _) ...)
             (dependencies->recutils (filter package? inputs)))))
  (format port "location: ~a~%"
          (or (and=> (package-location p)
                     (if hyperlinks? location->hyperlink location->string))
              (G_ "unknown")))

  ;; Note: Starting from version 1.6 or recutils, hyphens are not allowed in
  ;; field identifiers.
  (format port "homepage: ~a~%" (package-home-page p))

  (format port "license: ~a~%"
          (match (package-license p)
            (((? license? licenses) ...)
             (string-join (map license-name licenses)
                          ", "))
            ((? license? license)
             (let ((text (license-name license))
                   (uri  (license-uri license)))
               (if (and hyperlinks? uri (string-prefix? "http" uri))
                   (hyperlink uri text)
                   text)))
            (x
             (G_ "unknown"))))
  (format port "synopsis: ~a~%"
          (string-map (match-lambda
                        (#\newline #\space)
                        (chr       chr))
                      (or (package-synopsis-string p) "")))
  (format port "~a~%"
          (string->recutils
           (string-trim-right
            (parameterize ((%text-width width*))
              ;; Call 'texi->plain-text' on the concatenated string to account
              ;; for the width of "description:" in paragraph filling.
              (texi->plain-text*
               p
               (string-append "description: "
                              (or (and=> (package-description p) P_)
                                  ""))))
            #\newline)))
  (for-each (match-lambda
              ((field . value)
               (let ((field (symbol->string field)))
                 (format port "~a: ~a~%"
                         field
                         (fill-paragraph (object->string value) width*
                                         (string-length field))))))
            extra-fields)
  (newline port))


;;;
;;; Searching.
;;;

(define (relevance obj regexps metrics)
  "Compute a \"relevance score\" for OBJ as a function of its number of
matches of REGEXPS and accordingly to METRICS.  METRICS is list of
field/weight pairs, where FIELD is a procedure that returns a string or list
of strings describing OBJ, and WEIGHT is a positive integer denoting the
weight of this field in the final score.

A score of zero means that OBJ does not match any of REGEXPS.  The higher the
score, the more relevant OBJ is to REGEXPS."
  (define (score regexp str)
    (fold-matches regexp str 0
                  (lambda (m score)
                    (+ score
                       (if (string=? (match:substring m) str)
                           5             ;exact match
                           1)))))

  (define (regexp->score regexp)
    (let ((score-regexp (lambda (str) (score regexp str))))
      (fold (lambda (metric relevance)
              (match metric
                ((field . weight)
                 (match (field obj)
                   (#f  relevance)
                   ((? string? str)
                    (+ relevance (* (score-regexp str) weight)))
                   ((lst ...)
                    (+ relevance (* weight (apply + (map score-regexp lst)))))))))
            0 metrics)))

  (let loop ((regexps regexps)
             (total-score 0))
    (match regexps
      ((head . tail)
       (let ((score (regexp->score head)))
         ;; Return zero if one of PATTERNS doesn't match.
         (if (zero? score)
             0
             (loop tail (+ total-score score)))))
      (() total-score))))

(define %package-metrics
  ;; Metrics used to compute the "relevance score" of a package against a set
  ;; of regexps.
  `((,package-name . 4)

    ;; Match against uncommon outputs.
    (,(lambda (package)
        (filter (lambda (output)
                  (not (member output
                               ;; Some common outputs shared by many packages.
                               '("out" "doc" "debug" "lib" "include" "bin"))))
                (package-outputs package)))
     . 1)

    ;; Match regexps on the raw Texinfo since formatting it is quite expensive
    ;; and doesn't have much of an effect on search results.
    (,(lambda (package)
        (and=> (package-synopsis package) P_)) . 3)
    (,(lambda (package)
        (and=> (package-description package) P_)) . 2)

    (,(lambda (type)
        (match (and=> (package-location type) location-file)
          ((? string? file) (basename file ".scm"))
          (#f "")))
     . 1)))

(define (package-relevance package regexps)
  "Return a score denoting the relevance of PACKAGE for REGEXPS.  A score of
zero means that PACKAGE does not match any of REGEXPS."
  (relevance package regexps %package-metrics))

(define* (call-with-paginated-output-port proc
                                          #:key (less-options "FrX"))
  (let ((pager-command-line (or (getenv "GUIX_PAGER")
                                (getenv "PAGER")
                                "less")))
    ;; Setting PAGER to the empty string conventionally disables paging.
    (if (and (not (string-null? pager-command-line))
             (isatty?* (current-output-port)))
        ;; Set 'LESS' so that 'less' exits if everything fits on the screen
        ;; (F), lets ANSI escapes through (r), does not send the termcap
        ;; initialization string (X).  Set it unconditionally because some
        ;; distros set it to something that doesn't work here.
        ;;
        ;; For things that produce long lines, such as 'guix processes', use
        ;; 'R' instead of 'r': this strips hyperlinks but allows 'less' to
        ;; make a good estimate of the line length.
        (let* ((pager (with-environment-variables `(("LESS" ,less-options))
                        (apply open-pipe* OPEN_WRITE
                               ;; Split into arguments.  Treat runs of multiple
                               ;; whitespace characters as one.  libpipeline-
                               ;; style "cmd one\ arg" escaping is unsupported.
                               (remove (lambda (s) (string-null? s))
                                       (string-split pager-command-line
                                                     char-set:whitespace))))))
          (dynamic-wind
            (const #t)
            (lambda () (proc pager))
            (lambda () (close-pipe pager))))
        (proc (current-output-port)))))

(define-syntax with-paginated-output-port
  (syntax-rules ()
    "Evaluate EXP... with PORT bound to a port that talks to the pager if
standard output is a tty, or with PORT set to the current output port."
    ((_ port exp ... #:less-options opts)
     (call-with-paginated-output-port (lambda (port) exp ...)
                                      #:less-options opts))
    ((_ port exp ...)
     (call-with-paginated-output-port (lambda (port) exp ...)))))

(define* (display-search-results matches port
                                 #:key
                                 (command "guix search")
                                 (print package->recutils))
  "Display MATCHES, a list of object/score pairs, by calling PRINT on each of
them.  If PORT is a terminal, print at most a full screen of results."
  (define first-line
    (port-line port))

  (define max-rows
    (and first-line (isatty? port)
         (terminal-rows port)))

  (define (line-count str)
    (string-count str #\newline))

  (with-paginated-output-port paginated
    (let loop ((matches matches))
      (match matches
        (((package . score) rest ...)
         (let* ((links? (supports-hyperlinks? port)))
           (print package paginated
                  #:hyperlinks? links?
                  #:extra-fields `((relevance . ,score)))
           (loop rest)))
        (()
         #t)))))


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

  (cond ((string-match "^([0-9]+)s$" str)
         =>
         (lambda (match)
           (make-time time-duration 0
                      (string->number (match:substring match 1)))))
        ((string-match "^([0-9]+)h$" str)
         =>
         (lambda (match)
           (hours->duration 1 match)))
        ((string-match "^([0-9]+)d$" str)
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

(define* (matching-generations str profile
                               #:key (duration-relation <=))
  "Return the list of available generations matching a pattern in STR.  See
'string->generations' and 'string->duration' for the list of valid patterns.
When STR is a duration pattern, return all the generations whose ctime has
DURATION-RELATION with the current time."
  (define (valid-generations lst)
    (define (valid-generation? n)
      (any (cut = n <>) (generation-numbers profile)))

    (fold-right (lambda (x acc)
                  (if (valid-generation? x)
                      (cons x acc)
                      acc))
                '()
                lst))

  (define (filter-generations generations)
    (match generations
      (() '())
      (('>= n)
       (drop-while (cut > n <>)
                   (generation-numbers profile)))
      (('<= n)
       (valid-generations (iota n 1)))
      ((lst ..1)
       (valid-generations lst))
      (x #f)))

  (define (filter-by-duration duration)
    (define (time-at-midnight time)
      ;; Return TIME at midnight by setting nanoseconds, seconds, minutes, and
      ;; hours to zeros.
      (let ((d (time-utc->date time)))
         (date->time-utc
          (make-date 0 0 0 0
                     (date-day d) (date-month d)
                     (date-year d) (date-zone-offset d)))))

    (define generation-ctime-alist
      (map (lambda (number)
             (cons number
                   (time-second
                    (time-at-midnight
                     (generation-time profile number)))))
           (generation-numbers profile)))

    (match duration
      (#f #f)
      (res
       (let ((s (time-second
                 (subtract-duration (time-at-midnight (current-time))
                                    duration))))
         (delete #f (map (lambda (x)
                           (and (duration-relation s (cdr x))
                                (first x)))
                         generation-ctime-alist))))))

  (cond ((string->generations str)
         =>
         filter-generations)
        ((string->duration str)
         =>
         filter-by-duration)
        (else
         (raise
          (formatted-message (G_ "invalid syntax: ~a~%") str)))))

(define (display-generation profile number)
  "Display a one-line summary of generation NUMBER of PROFILE."
  (unless (zero? number)
    (let* ((file   (generation-file-name profile number))
           (link   (if (supports-hyperlinks?)
                       (cut file-hyperlink file <>)
                       identity))
           (header (format #f (link (highlight (G_ "Generation ~a\t~a")))
                           number
                           (date->string
                            (time-utc->date
                             (generation-time profile number))
                            ;; TRANSLATORS: This is a format-string for date->string.
                            ;; Please choose a format that corresponds to the
                            ;; usual way of presenting dates in your locale.
                            ;; See https://www.gnu.org/software/guile/manual/html_node/SRFI_002d19-Date-to-string.html
                            ;; for details.
                            (G_ "~b ~d ~Y ~T"))))
           (current (generation-number profile)))
      (if (= number current)
          ;; TRANSLATORS: The word "current" here is an adjective for
          ;; "Generation", as in "current generation".  Use the appropriate
          ;; gender where applicable.
          (format #t (G_ "~a\t(current)~%") header)
          (format #t "~a~%" header)))))

(define (display-profile-content-diff profile gen1 gen2)
  "Display the changed packages in PROFILE GEN2 compared to generation GEN1."

  (define (equal-entry? first second)
    (string= (manifest-entry-item first) (manifest-entry-item second)))

  (define (make-row entry prefix)
    (match entry
      (($ <manifest-entry> name version output location _)
       (list (format #f " ~a ~a" prefix name) version output location))))

  (define (list-entries number)
    (manifest-entries (profile-manifest (generation-file-name profile number))))

  (define (display-diff profile old new)
    (display-generation profile new)
    (let ((added (lset-difference
                  equal-entry? (list-entries new) (list-entries old)))
          (removed (lset-difference
                    equal-entry? (list-entries old) (list-entries new))))
      (pretty-print-table (append (map (cut make-row <> "+") added)
                                  (map (cut make-row <> "-") removed)))
      (newline)))

  (display-diff profile gen1 gen2))

(define (profile-lock-handler profile errno . _)
  "Handle failure to acquire PROFILE's lock."
  ;; NFS mounts can return ENOLCK.  When that happens, there's not much that
  ;; can be done, so warn the user and keep going.
  (if (= errno ENOLCK)
      (warning (G_ "cannot lock profile ~a: ~a~%")
               profile (strerror errno))
      (leave (G_ "profile ~a is locked by another process~%")
             profile)))

(define profile-lock-file
  (cut string-append <> ".lock"))

(define-syntax-rule (with-profile-lock profile exp ...)
  "Grab PROFILE's lock and evaluate EXP...  Call 'leave' if the lock is
already taken."
  (with-file-lock/no-wait (profile-lock-file profile)
    (cut profile-lock-handler profile <...>)
    exp ...))

(define (display-profile-content profile number)
  "Display the packages in PROFILE, generation NUMBER, in a human-readable
way."

  (define entry->row
    (match-lambda
      (($ <manifest-entry> name version output location _)
       (list (string-append "  " name) version output location))))

  (let* ((manifest (profile-manifest (generation-file-name profile number)))
         (entries  (manifest-entries manifest))
         (rows     (map entry->row entries)))
    ;; Show most recently installed packages last.
    (pretty-print-table (reverse rows))))

(define (display-generation-change previous current)
  (format #t (G_ "switched from generation ~a to ~a~%") previous current))

(define (roll-back* store profile)
  "Like 'roll-back', but display what is happening."
  (call-with-values
      (lambda ()
        (roll-back store profile))
    display-generation-change))

(define (switch-to-generation* profile number)
  "Like 'switch-to-generation', but display what is happening."
  (let ((previous (switch-to-generation profile number)))
    (display-generation-change previous number)))

(define (delete-generation* store profile generation)
  "Like 'delete-generation', but display what is going on."
  (format #t (G_ "deleting ~a~%")
          (generation-file-name profile generation))
  (delete-generation store profile generation))

(define* (package-specification->name+version+output spec
                                                     #:optional (output "out"))
  "Parse package specification SPEC and return three value: the specified
package name, version number (or #f), and output name (or OUTPUT).  SPEC may
optionally contain a version number and an output name, as in these examples:

  guile
  guile@2.0.9
  guile:debug
  guile@2.0.9:debug
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

(define (show-guix-usage)
  (format (current-error-port)
          (G_ "Try `guix --help' for more information.~%"))
  (exit 1))

;; Representation of a 'guix' command.
(define-immutable-record-type <command>
  (command name synopsis category)
  command?
  (name     command-name)
  (synopsis command-synopsis)
  (category command-category))

(define (source-file-command file)
  "Read FILE, a Scheme source file, and return either a <command> object based
on the 'define-command' top-level form found therein, or #f if FILE does not
contain a 'define-command' form."
  (define command-name
    (match (filter (negate string-null?)
                   (string-split file #\/))
      ((_ ... "guix" (or "scripts" "extensions") name)
       (list (file-sans-extension name)))
      ((_ ... "guix" (or "scripts" "extensions") first second)
       (list first (file-sans-extension second)))))

  ;; The strategy here is to parse FILE.  This is much cheaper than a
  ;; technique based on run-time introspection where we'd load FILE and all
  ;; the modules it depends on.
  (call-with-input-file file
    (lambda (port)
      (let loop ()
        (match (read port)
          (('define-command _ ('synopsis synopsis)
             _ ...)
           (command command-name synopsis 'main))
          (('define-command _
             ('category category) ('synopsis synopsis)
             _ ...)
           (command command-name synopsis category))
          ((? eof-object?)
           #f)
          (_
           (loop)))))))

(define* (command-files #:optional directory)
  "Return the list of source files that define Guix sub-commands."
  (define directory*
    (or directory
        (and=> (search-path %load-path "guix.scm")
               (compose (cut string-append <> "/guix/scripts")
                        dirname))))

  (define dot-scm?
    (cut string-suffix? ".scm" <>))

  (if directory*
      (map (cut string-append directory* "/" <>)
           (scandir directory* dot-scm?))
      '()))

(define (extension-directories)
  "Return the list of directories containing Guix extensions."
  (filter file-exists?
          (parse-path
           (getenv "GUIX_EXTENSIONS_PATH"))))

(define (commands)
  "Return the list of commands, alphabetically sorted."
  (filter-map source-file-command
              (append (command-files)
                      (append-map command-files
                                  (extension-directories)))))

(define (show-guix-help)
  (define (internal? command)
    (member command '("substitute" "authenticate" "offload"
                      "perform-download")))

  (define (display-commands commands)
    (let* ((names     (map (lambda (command)
                             (string-join (command-name command)))
                           commands))
           (max-width (reduce max 0 (map string-length names))))
      (for-each (lambda (name command)
                  (format #t "    ~a  ~a~%"
                          (string-pad-right name max-width)
                          (G_ (command-synopsis command))))
                names
                commands)))

  (define (category-predicate category)
    (lambda (command)
      (eq? category (command-category command))))

  (display (G_ "Usage: guix OPTION | COMMAND ARGS...
Run COMMAND with ARGS, if given.\n"))

  (display (G_ "
  -h, --help             display this helpful text again and exit"))
  (display (G_ "
  -V, --version          display version and copyright information and exit"))
  (newline)

  (newline)
  (display (G_ "COMMAND must be one of the sub-commands listed below:\n"))

  (let ((commands   (commands))
        (categories (module-ref (resolve-interface '(guix scripts))
                                '%command-categories)))
    (for-each (match-lambda
                (('internal . _)
                 #t)                              ;hide internal commands
                ((category . synopsis)
                 (let ((relevant-commands (filter (category-predicate category)
                                                  commands)))
                   ;; Only print categories that contain commands.
                   (match relevant-commands
                     ((one . more)
                      (format #t "~%  ~a~%" (G_ synopsis))
                      (display-commands relevant-commands))
                     (_ #f)))))
              categories))
  (show-bug-report-information))

(define (run-guix-command command . args)
  "Run COMMAND with the given ARGS.  Report an error when COMMAND is not
found."
  (define (command-hint guess commands)
    (define command-names
      (map (lambda (command)
             (match (command-name command)
               ((head tail ...) head)))
           commands))
    (string-closest (symbol->string guess) command-names #:threshold 3))

  (define module
    ;; Check if there is a matching extension.
    (match (search-path (extension-directories)
                        (format #f "~a.scm" command))
      (#f
       (catch 'misc-error
         (lambda ()
           (resolve-interface `(guix scripts ,command)))
         (lambda _
           (let ((hint (command-hint command (commands))))
             (format (current-error-port)
                     (G_ "guix: ~a: command not found~%") command)
             (when hint
               (display-hint (format #f (G_ "Did you mean @code{~a}?")
                                     hint)))
             (show-guix-usage)))))
      (file
       (load file)
       (resolve-interface `(guix extensions ,command)))))

  (let ((command-main (module-ref module
                                  (symbol-append 'guix- command))))
    (parameterize ((program-name command))
      (dynamic-wind
        (const #f)
        (lambda ()
          (apply command-main args))
        (lambda ()
          ;; Abuse 'exit-hook' (which is normally meant to be used by the
          ;; REPL) to run things like profiling hooks upon completion.
          (run-hook exit-hook))))))

(define (run-guix . args)
  "Run the 'guix' command defined by command line ARGS.
Unlike 'guix-main', this procedure assumes that locale, i18n support,
and signal handling have already been set up."
  (define option? (cut string-prefix? "-" <>))

  ;; The default %LOAD-EXTENSIONS includes the empty string, which doubles the
  ;; number of 'stat' calls per entry in %LOAD-PATH.  Shamelessly remove it.
  (set! %load-extensions '(".scm"))

  ;; Disable canonicalization so we don't don't stat unreasonably.
  (with-fluids ((%file-port-name-canonicalization #f))
    (match args
      (()
       (format (current-error-port)
               (G_ "guix: missing command name~%"))
       (show-guix-usage))
      ((or ("-h") ("--help"))
       (leave-on-EPIPE (show-guix-help)))
      ((or ("-V") ("--version"))
       (show-version-and-exit "guix"))
      (((? option? o) args ...)
       (format (current-error-port)
               (G_ "guix: unrecognized option '~a'~%") o)
       (show-guix-usage))
      (("help" command)
       (apply run-guix-command (string->symbol command)
              '("--help")))
      (("help" args ...)
       (leave-on-EPIPE (show-guix-help)))
      ((command args ...)
       (apply run-guix-command
              (string->symbol command)
              args)))))

(define (guix-main arg0 . args)
  (initialize-guix)
  (apply run-guix args))

;;; Local Variables:
;;; eval: (put 'guard* 'scheme-indent-function 2)
;;; End:

;;; ui.scm ends here
