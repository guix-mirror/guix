;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts lint)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module (guix download)
  #:use-module (guix ftp-client)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix gnu-maintenance)
  #:use-module (guix monads)
  #:use-module (gnu packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (web uri)
  #:use-module ((guix build download)
                #:select (maybe-expand-mirrors
                          open-connection-for-uri))
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-6)                      ;Unicode string ports
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:export (guix-lint
            check-description-style
            check-inputs-should-be-native
            check-patch-file-names
            check-synopsis-style
            check-derivation
            check-home-page
            check-source))


;;;
;;; Helpers
;;;
(define* (emit-warning package message #:optional field)
  ;; Emit a warning about PACKAGE, printing the location of FIELD if it is
  ;; given, the location of PACKAGE otherwise, the full name of PACKAGE and the
  ;; provided MESSAGE.
  (let ((loc (or (package-field-location package field)
                 (package-location package))))
    (format (guix-warning-port) "~a: ~a: ~a~%"
            (location->string loc)
            (package-full-name package)
            message)))

(define (call-with-accumulated-warnings thunk)
  "Call THUNK, accumulating any warnings in the current state, using the state
monad."
  (let ((port (open-output-string)))
    (mlet %state-monad ((state      (current-state))
                        (result ->  (parameterize ((guix-warning-port port))
                                      (thunk)))
                        (warning -> (get-output-string port)))
      (mbegin %state-monad
        (munless (string=? "" warning)
          (set-current-state (cons warning state)))
        (return result)))))

(define-syntax-rule (with-accumulated-warnings exp ...)
  "Evaluate EXP and accumulate warnings in the state monad."
  (call-with-accumulated-warnings
   (lambda ()
     exp ...)))


;;;
;;; Checkers
;;;
(define-record-type* <lint-checker>
  lint-checker make-lint-checker
  lint-checker?
  ;; TODO: add a 'certainty' field that shows how confident we are in the
  ;; checker. Then allow users to only run checkers that have a certain
  ;; 'certainty' level.
  (name        lint-checker-name)
  (description lint-checker-description)
  (check       lint-checker-check))

(define (list-checkers-and-exit)
  ;; Print information about all available checkers and exit.
  (format #t (_ "Available checkers:~%"))
  (for-each (lambda (checker)
              (format #t "- ~a: ~a~%"
                      (lint-checker-name checker)
                      (_ (lint-checker-description checker))))
            %checkers)
  (exit 0))

(define (properly-starts-sentence? s)
  (string-match "^[(\"'[:upper:][:digit:]]" s))

(define (starts-with-abbreviation? s)
  "Return #t if S starts with what looks like an abbreviation or acronym."
  (string-match "^[A-Z][A-Z0-9]+\\>" s))

(define (check-description-style package)
  ;; Emit a warning if stylistic issues are found in the description of PACKAGE.
  (define (check-not-empty description)
    (when (string-null? description)
      (emit-warning package
                    (_ "description should not be empty")
                    'description)))

  (define (check-proper-start description)
    (unless (or (properly-starts-sentence? description)
                (string-prefix-ci? (package-name package) description))
      (emit-warning package
                    (_ "description should start with an upper-case letter or digit")
                    'description)))

  (define (check-end-of-sentence-space description)
    "Check that an end-of-sentence period is followed by two spaces."
    (let ((infractions
           (reverse (fold-matches
                     "\\. [A-Z]" description '()
                     (lambda (m r)
                       ;; Filter out matches of common abbreviations.
                       (if (find (lambda (s)
                                   (string-suffix-ci? s (match:prefix m)))
                                 '("i.e" "e.g" "a.k.a" "resp"))
                           r (cons (match:start m) r)))))))
      (unless (null? infractions)
        (emit-warning package
                      (format #f (_ "sentences in description should be followed ~
by two spaces; possible infraction~p at ~{~a~^, ~}")
                              (length infractions)
                              infractions)
                      'description))))

  (let ((description (package-description package)))
    (when (string? description)
      (check-not-empty description)
      (check-proper-start description)
      (check-end-of-sentence-space description))))

(define (check-inputs-should-be-native package)
  ;; Emit a warning if some inputs of PACKAGE are likely to belong to its
  ;; native inputs.
  (let ((inputs (package-inputs package)))
    (match inputs
      (((labels packages . _) ...)
       (when (member "pkg-config"
                     (map package-name (filter package? packages)))
        (emit-warning package
                      (_ "pkg-config should probably be a native input")
                      'inputs))))))

(define (package-name-regexp package)
  "Return a regexp that matches PACKAGE's name as a word at the beginning of a
line."
  (make-regexp (string-append "^" (regexp-quote (package-name package))
                              "\\>")
               regexp/icase))

(define (check-synopsis-style package)
  ;; Emit a warning if stylistic issues are found in the synopsis of PACKAGE.
  (define (check-not-empty synopsis)
    (when (string-null? synopsis)
      (emit-warning package
                    (_ "synopsis should not be empty")
                    'synopsis)))

  (define (check-final-period synopsis)
    ;; Synopsis should not end with a period, except for some special cases.
    (when (and (string-suffix? "." synopsis)
               (not (string-suffix? "etc." synopsis)))
      (emit-warning package
                    (_ "no period allowed at the end of the synopsis")
                    'synopsis)))

  (define check-start-article
    ;; Skip this check for GNU packages, as suggested by Karl Berry's reply to
    ;; <http://lists.gnu.org/archive/html/bug-womb/2014-11/msg00000.html>.
    (if (false-if-exception (gnu-package? package))
        (const #t)
        (lambda (synopsis)
          (when (or (string-prefix-ci? "A " synopsis)
                    (string-prefix-ci? "An " synopsis))
            (emit-warning package
                          (_ "no article allowed at the beginning of \
the synopsis")
                          'synopsis)))))

  (define (check-synopsis-length synopsis)
    (when (>= (string-length synopsis) 80)
      (emit-warning package
                    (_ "synopsis should be less than 80 characters long")
                    'synopsis)))

  (define (check-proper-start synopsis)
    (unless (properly-starts-sentence? synopsis)
      (emit-warning package
                    (_ "synopsis should start with an upper-case letter or digit")
                    'synopsis)))

  (define (check-start-with-package-name synopsis)
    (when (and (regexp-exec (package-name-regexp package) synopsis)
               (not (starts-with-abbreviation? synopsis)))
      (emit-warning package
                    (_ "synopsis should not start with the package name")
                    'synopsis)))

 (let ((synopsis (package-synopsis package)))
   (when (string? synopsis)
     (check-not-empty synopsis)
     (check-proper-start synopsis)
     (check-final-period synopsis)
     (check-start-article synopsis)
     (check-start-with-package-name synopsis)
     (check-synopsis-length synopsis))))

(define (probe-uri uri)
  "Probe URI, a URI object, and return two values: a symbol denoting the
probing status, such as 'http-response' when we managed to get an HTTP
response from URI, and additional details, such as the actual HTTP response."
  (define headers
    '((User-Agent . "GNU Guile")
      (Accept . "*/*")))

  (let loop ((uri     uri)
             (visited '()))
    (match (uri-scheme uri)
      ((or 'http 'https)
       (catch #t
         (lambda ()
           (let ((port    (open-connection-for-uri uri))
                 (request (build-request uri #:headers headers)))
             (define response
               (dynamic-wind
                 (const #f)
                 (lambda ()
                   (write-request request port)
                   (force-output port)
                   (read-response port))
                 (lambda ()
                   (close port))))

             (case (response-code response)
               ((301 302 307)
                (let ((location (response-location response)))
                  (if (or (not location) (member location visited))
                      (values 'http-response response)
                      (loop location (cons location visited))))) ;follow the redirect
               (else
                (values 'http-response response)))))
         (lambda (key . args)
           (case key
             ((bad-header bad-header-component)
              ;; This can happen if the server returns an invalid HTTP header,
              ;; as is the case with the 'Date' header at sqlite.org.
              (values 'invalid-http-response #f))
             ((getaddrinfo-error system-error gnutls-error)
              (values key args))
             (else
              (apply throw key args))))))
      ('ftp
       (catch #t
         (lambda ()
           (let ((conn (ftp-open (uri-host uri) 21)))
             (define response
               (dynamic-wind
                 (const #f)
                 (lambda ()
                   (ftp-chdir conn (dirname (uri-path uri)))
                   (ftp-size conn (basename (uri-path uri))))
                 (lambda ()
                   (ftp-close conn))))
             (values 'ftp-response '(ok))))
         (lambda (key . args)
           (case key
             ((ftp-error)
              (values 'ftp-response `(error ,@args)))
             ((getaddrinfo-error system-error gnutls-error)
              (values key args))
             (else
              (apply throw key args))))))
      (_
       (values 'unknown-protocol #f)))))

(define (validate-uri uri package field)
  "Return #t if the given URI can be reached, otherwise return #f and emit a
warning for PACKAGE mentionning the FIELD."
  (let-values (((status argument)
                (probe-uri uri)))
    (case status
      ((http-response)
       (or (= 200 (response-code argument))
           (begin
             (emit-warning package
                           (format #f
                                   (_ "URI ~a not reachable: ~a (~s)")
                                   (uri->string uri)
                                   (response-code argument)
                                   (response-reason-phrase argument))
                           field)
             #f)))
      ((ftp-response)
       (match argument
         (('ok) #t)
         (('error port command code message)
          (emit-warning package
                        (format #f
                                (_ "URI ~a not reachable: ~a (~s)")
                                (uri->string uri)
                                code (string-trim-both message)))
          #f)))
      ((getaddrinfo-error)
       (emit-warning package
                     (format #f
                             (_ "URI ~a domain not found: ~a")
                             (uri->string uri)
                             (gai-strerror (car argument)))
                     field)
       #f)
      ((system-error)
       (emit-warning package
                     (format #f
                             (_ "URI ~a unreachable: ~a")
                             (uri->string uri)
                             (strerror
                              (system-error-errno
                               (cons status argument))))
                     field)
       #f)
      ((invalid-http-response gnutls-error)
       ;; Probably a misbehaving server; ignore.
       #f)
      ((unknown-protocol)                             ;nothing we can do
       #f)
      (else
       (error "internal linter error" status)))))

(define (check-home-page package)
  "Emit a warning if PACKAGE has an invalid 'home-page' field, or if that
'home-page' is not reachable."
  (let ((uri (and=> (package-home-page package) string->uri)))
    (cond
     ((uri? uri)
      (validate-uri uri package 'home-page))
     ((not (package-home-page package))
      (unless (or (string-contains (package-name package) "bootstrap")
                  (string=? (package-name package) "ld-wrapper"))
        (emit-warning package
                      (_ "invalid value for home page")
                      'home-page)))
     (else
      (emit-warning package (format #f (_ "invalid home page URL: ~s")
                                    (package-home-page package))
                    'home-page)))))

(define (check-patch-file-names package)
  "Emit a warning if the patches requires by PACKAGE are badly named or if the
patch could not be found."
  (guard (c ((message-condition? c)               ;raised by 'search-patch'
             (emit-warning package (condition-message c)
                           'patch-file-names)))
    (let ((patches   (and=> (package-source package) origin-patches))
          (name      (package-name package))
          (full-name (package-full-name package)))
      (when (and patches
                 (any (match-lambda
                        ((? string? patch)
                         (let ((file (basename patch)))
                           (not (or (eq? (string-contains file name) 0)
                                    (eq? (string-contains file full-name)
                                         0)))))
                        (_
                         ;; This must be an <origin> or something like that.
                         #f))
                      patches))
        (emit-warning package
                      (_ "file names of patches should start with \
the package name")
                      'patch-file-names)))))

(define (escape-quotes str)
  "Replace any quote character in STR by an escaped quote character."
  (list->string
   (string-fold-right (lambda (chr result)
                        (match chr
                          (#\" (cons* #\\ #\"result))
                          (_   (cons chr result))))
                      '()
                      str)))

(define official-gnu-packages*
  (memoize
   (lambda ()
     "A memoizing version of 'official-gnu-packages' that returns the empty
list when something goes wrong, such as a networking issue."
     (let ((gnus (false-if-exception (official-gnu-packages))))
       (or gnus '())))))

(define (check-gnu-synopsis+description package)
  "Make sure that, if PACKAGE is a GNU package, it uses the synopsis and
descriptions maintained upstream."
  (match (find (lambda (descriptor)
                 (string=? (gnu-package-name descriptor)
                           (package-name package)))
               (official-gnu-packages*))
    (#f                                   ;not a GNU package, so nothing to do
     #t)
    (descriptor                           ;a genuine GNU package
     (let ((upstream   (gnu-package-doc-summary descriptor))
           (downstream (package-synopsis package))
           (loc        (or (package-field-location package 'synopsis)
                           (package-location package))))
       (unless (and upstream (string=? upstream downstream))
         (format (guix-warning-port)
                 (_ "~a: ~a: proposed synopsis: ~s~%")
                 (location->string loc) (package-full-name package)
                 upstream)))

     (let ((upstream   (gnu-package-doc-description descriptor))
           (downstream (package-description package))
           (loc        (or (package-field-location package 'description)
                           (package-location package))))
       (when (and upstream
                  (not (string=? (fill-paragraph upstream 100)
                                 (fill-paragraph downstream 100))))
         (format (guix-warning-port)
                 (_ "~a: ~a: proposed description:~%     \"~a\"~%")
                 (location->string loc) (package-full-name package)
                 (fill-paragraph (escape-quotes upstream) 77 7)))))))

(define (check-source package)
  "Emit a warning if PACKAGE has an invalid 'source' field, or if that
'source' is not reachable."
  (define (try-uris uris)
    (run-with-state
        (anym %state-monad
              (lambda (uri)
                (with-accumulated-warnings
                 (validate-uri uri package 'source)))
              (append-map (cut maybe-expand-mirrors <> %mirrors)
                          uris))
      '()))

  (let ((origin (package-source package)))
    (when (and origin
               (eqv? (origin-method origin) url-fetch))
      (let* ((strings (origin-uri origin))
             (uris (if (list? strings)
                       (map string->uri strings)
                       (list (string->uri strings)))))

        ;; Just make sure that at least one of the URIs is valid.
        (call-with-values
            (lambda () (try-uris uris))
          (lambda (success? warnings)
            ;; When everything fails, report all of WARNINGS, otherwise don't
            ;; report anything.
            ;;
            ;; XXX: Ideally we'd still allow warnings to be raised if *some*
            ;; URIs are unreachable, but distinguish that from the error case
            ;; where *all* the URIs are unreachable.
            (unless success?
              (emit-warning package
                            (_ "all the source URIs are unreachable:")
                            'source)
              (for-each (lambda (warning)
                          (display warning (guix-warning-port)))
                        (reverse warnings)))))))))

(define (check-derivation package)
  "Emit a warning if we fail to compile PACKAGE to a derivation."
  (catch #t
    (lambda ()
      (guard (c ((nix-protocol-error? c)
                 (emit-warning package
                               (format #f (_ "failed to create derivation: ~a")
                                       (nix-protocol-error-message c))))
                ((message-condition? c)
                 (emit-warning package
                               (format #f (_ "failed to create derivation: ~a")
                                       (condition-message c)))))
        (with-store store
          (package-derivation store package))))
    (lambda args
      (emit-warning package
                    (format #f (_ "failed to create derivation: ~s~%")
                            args)))))



;;;
;;; List of checkers.
;;;

(define %checkers
  (list
   (lint-checker
     (name        'description)
     (description "Validate package descriptions")
     (check       check-description-style))
   (lint-checker
     (name        'gnu-description)
     (description "Validate synopsis & description of GNU packages")
     (check       check-gnu-synopsis+description))
   (lint-checker
     (name        'inputs-should-be-native)
     (description "Identify inputs that should be native inputs")
     (check       check-inputs-should-be-native))
   (lint-checker
     (name        'patch-file-names)
     (description "Validate file names and availability of patches")
     (check       check-patch-file-names))
   (lint-checker
     (name        'home-page)
     (description "Validate home-page URLs")
     (check       check-home-page))
   (lint-checker
     (name        'source)
     (description "Validate source URLs")
     (check       check-source))
   (lint-checker
     (name        'derivation)
     (description "Report failure to compile a package to a derivation")
     (check       check-derivation))
   (lint-checker
     (name        'synopsis)
     (description "Validate package synopses")
     (check       check-synopsis-style))))

(define (run-checkers package checkers)
  ;; Run the given CHECKERS on PACKAGE.
  (let ((tty? (isatty? (current-error-port)))
        (name (package-full-name package)))
    (for-each (lambda (checker)
                (when tty?
                  (format (current-error-port) "checking ~a [~a]...\r"
                          name (lint-checker-name checker))
                  (force-output (current-error-port)))
                ((lint-checker-check checker) package))
              checkers)))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  '())

(define (show-help)
  (display (_ "Usage: guix lint [OPTION]... [PACKAGE]...
Run a set of checkers on the specified package; if none is specified,
run the checkers on all packages.\n"))
  (display (_ "
  -c, --checkers=CHECKER1,CHECKER2...
                         only run the specified checkers"))
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -l, --list-checkers    display the list of available lint checkers"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))


(define %options
  ;; Specification of the command-line options.
  ;; TODO: add some options:
  ;; * --certainty=[low,medium,high]: only run checkers that have at least this
  ;;                                  'certainty'.
  (list (option '(#\c "checkers") #t #f
                (lambda (opt name arg result)
                  (let ((names (map string->symbol (string-split arg #\,))))
                    (for-each (lambda (c)
                                (unless (memq c
                                              (map lint-checker-name
                                                   %checkers))
                                  (leave (_ "~a: invalid checker~%") c)))
                              names)
                    (alist-cons 'checkers
                                (filter (lambda (checker)
                                          (member (lint-checker-name checker)
                                                  names))
                                        %checkers)
                                result))))
        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\l "list-checkers") #f #f
                (lambda args
                   (list-checkers-and-exit)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix lint")))))


;;;
;;; Entry Point
;;;

(define (guix-lint . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (alist-cons 'argument arg result))
                %default-options))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                            (('argument . value)
                             value)
                            (_ #f))
                           (reverse opts)))
         (checkers (or (assoc-ref opts 'checkers) %checkers)))
     (if (null? args)
          (fold-packages (lambda (p r) (run-checkers p checkers)) '())
          (for-each (lambda (spec)
                      (run-checkers (specification->package spec) checkers))
                    args))))
