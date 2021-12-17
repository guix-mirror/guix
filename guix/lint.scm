;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2017, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2020 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
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

(define-module (guix lint)
  #:use-module (guix store)
  #:autoload   (guix base16) (bytevector->base16-string)
  #:use-module (guix base32)
  #:use-module (guix diagnostics)
  #:use-module (guix download)
  #:use-module (guix ftp-client)
  #:use-module (guix http-client)
  #:use-module (guix packages)
  #:use-module (guix i18n)
  #:use-module ((guix gexp)
                #:select (gexp? local-file? local-file-absolute-file-name
                                gexp->approximate-sexp))
  #:use-module (guix licenses)
  #:use-module (guix records)
  #:use-module (guix grafts)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (guix memoization)
  #:use-module (guix profiles)
  #:use-module (guix monads)
  #:use-module (guix scripts)
  #:use-module ((guix ui) #:select (texi->plain-text fill-paragraph))
  #:use-module (guix gnu-maintenance)
  #:use-module (guix cve)
  #:use-module ((guix swh) #:hide (origin?))
  #:autoload   (guix git-download) (git-reference?
                                    git-reference-url git-reference-commit)
  #:use-module (guix import stackage)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module ((guix build download)
                #:select (maybe-expand-mirrors
                          (open-connection-for-uri
                           . guix:open-connection-for-uri)))
  #:use-module (web request)
  #:use-module (web response)
  #:autoload   (gnutls) (error->string)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-6)                      ;Unicode string ports
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 rdelim)
  #:export (check-description-style
            check-inputs-should-be-native
            check-inputs-should-not-be-an-input-at-all
            check-input-labels
            check-wrapper-inputs
            check-patch-file-names
            check-patch-headers
            check-synopsis-style
            check-derivation
            check-home-page
            check-name
            check-source
            check-source-file-name
            check-source-unstable-tarball
            check-optional-tests
            check-mirror-url
            check-github-url
            check-license
            check-vulnerabilities
            check-for-updates
            check-formatting
            check-archival
            check-profile-collisions
            check-haskell-stackage
            check-tests-true

            lint-warning
            lint-warning?
            lint-warning-package
            lint-warning-message
            lint-warning-message-text
            lint-warning-message-data
            lint-warning-location

            %local-checkers
            %network-dependent-checkers
            %all-checkers

            lint-checker
            lint-checker?
            lint-checker-name
            lint-checker-description
            lint-checker-check
            lint-checker-requires-store?))


;;;
;;; Warnings
;;;

(define-record-type* <lint-warning>
  lint-warning make-lint-warning
  lint-warning?
  (package       lint-warning-package)
  (message-text  lint-warning-message-text)
  (message-data  lint-warning-message-data
                 (default '()))
  (location      lint-warning-location
                 (default #f)))

(define (lint-warning-message warning)
  (apply format #f
         (G_ (lint-warning-message-text warning))
         (lint-warning-message-data warning)))

(define (package-file package)
  (location-file
   (package-location package)))

(define* (%make-warning package message-text
                        #:optional (message-data '())
                        #:key field location)
  (make-lint-warning
   package
   message-text
   message-data
   (or location
       (and field (package-field-location package field))
       (package-location package))))

(define-syntax make-warning
  (syntax-rules (G_)
    ((_ package (G_ message) rest ...)
     (%make-warning package message rest ...))))


;;;
;;; Procedures for analysing Scheme code in package definitions
;;;

(define* (find-procedure-body expression found
                              #:key (not-found (const '())))
  "Try to find the body of the procedure defined inline by EXPRESSION.
If it was found, call FOUND with its body. If it wasn't, call
the thunk NOT-FOUND."
  (match expression
    (`(,(or 'let 'let*) . ,_)
     (find-procedure-body (car (last-pair expression)) found
                          #:not-found not-found))
    (`(,(or 'lambda 'lambda*) ,_ . ,code)
     (found code))
    (_ (not-found))))

(define* (report-bogus-phase-deltas package bogus-deltas)
  "Report a bogus invocation of ‘modify-phases’."
  (list (make-warning package
                      ;; TRANSLATORS: 'modify-phases' is a Scheme syntax
                      ;; and should not be translated.
                      (G_ "incorrect call to ‘modify-phases’")
                      #:field 'arguments)))

(define* (find-phase-deltas package found
                            #:key (not-found (const '()))
                            (bogus
                             (cut report-bogus-phase-deltas package <>)))
  "Try to find the clauses of the ‘modify-phases’ form in the phases
specification of PACKAGE.  If they were found, all FOUND with a list
of the clauses.  If they weren't (e.g. because ‘modify-phases’ wasn't
used at all), call the thunk NOT-FOUND instead.  If ‘modify-phases’
was used, but the clauses don't form a list, call BOGUS with the
not-a-list."
  (apply (lambda* (#:key phases #:allow-other-keys)
           (define phases/sexp
             (if (gexp? phases)
                 (gexp->approximate-sexp phases)
                 phases))
           (match phases/sexp
             (`(modify-phases ,_ . ,changes)
              ((if (list? changes) found bogus) changes))
             (_ (not-found))))
         (package-arguments package)))

(define (report-bogus-phase-procedure package)
  "Report a syntactically-invalid phase clause."
  (list (make-warning package
                      ;; TRANSLATORS: See ‘modify-phases’ in the manual.
                      (G_ "invalid phase clause")
                      #:field 'arguments)))

(define* (find-phase-procedure package expression found
                               #:key (not-found (const '()))
                               (bogus (cut report-bogus-phase-procedure
                                           package)))
  "Try to find the procedure in the phase clause EXPRESSION. If it was
found, call FOUND with the procedure expression. If EXPRESSION isn't
actually a phase clause, call the thunk BOGUS. If the phase form doesn't
have a procedure, call the thunk NOT-FOUND."
  (match expression
    (('add-after before after proc-expr)
     (found proc-expr))
    (('add-before after before proc-expr)
     (found proc-expr))
    (('replace _ proc-expr)
     (found proc-expr))
    (('delete _) (not-found))
    (_ (bogus))))


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
  (check       lint-checker-check)
  (requires-store? lint-checker-requires-store?
                   (default #f)))

(define (check-name package)
  "Check whether PACKAGE's name matches our guidelines."
  (let ((name (package-name package)))
    (cond
     ;; Currently checks only whether the name is too short.
     ((and (<= (string-length name) 1)
           (not (string=? name "r"))) ; common-sense exception
      (list
       (make-warning package
                     (G_ "name should be longer than a single character")
                     #:field 'name)))
     ((string-index name #\_)
      (list
       (make-warning package
                     (G_ "name should use hyphens instead of underscores")
                     #:field 'name)))
     (else '()))))

(define (check-tests-true package)
  "Check whether PACKAGE explicitly requests to run tests, which is
superfluous when building natively and incorrect when cross-compiling."
  (define (tests-explicitly-enabled?)
    (apply (lambda* (#:key tests? #:allow-other-keys)
             (eq? tests? #t))
           (package-arguments package)))
  (if (and (tests-explicitly-enabled?)
           ;; Some packages, e.g. gnutls, set #:tests?
           ;; differently depending on whether it is being
           ;; cross-compiled.
           (parameterize ((%current-target-system "aarch64-linux-gnu"))
             (tests-explicitly-enabled?)))
      (list (make-warning package
                          ;; TRANSLATORS: #:tests? and #t are Scheme constants
                          ;; and must not be translated.
                          (G_ "#:tests? must not be explicitly set to #t")
                          #:field 'arguments))
      '()))

(define (properly-starts-sentence? s)
  (string-match "^[(\"'`[:upper:][:digit:]]" s))

(define (starts-with-abbreviation? s)
  "Return #t if S starts with what looks like an abbreviation or acronym."
  (string-match "^[A-Z][A-Z0-9]+\\>" s))

(define %quoted-identifier-rx
  ;; A quoted identifier, like 'this'.
  (make-regexp "['`][[:graph:]]+'"))

(define (check-description-style package)
  ;; Emit a warning if stylistic issues are found in the description of PACKAGE.
  (define (check-not-empty description)
    (if (string-null? description)
        (list
         (make-warning package
                       (G_ "description should not be empty")
                       #:field 'description))
        '()))

  (define (check-texinfo-markup description)
    "Check that DESCRIPTION can be parsed as a Texinfo fragment.  If the
markup is valid return a plain-text version of DESCRIPTION, otherwise #f."
    (catch #t
      (lambda () (texi->plain-text description))
      (lambda (keys . args)
        (make-warning package
                      (G_ "Texinfo markup in description is invalid")
                      #:field 'description))))

  (define (check-description-typo description typo-corrections)
    "Check that DESCRIPTION does not contain typo, with optional correction"
    (append-map
     (match-lambda
      ((typo . correction)
       (if (string-contains description typo)
           (list
            (make-warning package
                          (G_
                           (format #false
                                   "description contains typo '~a'~@[, should be '~a'~]"
                                   typo correction))))
           '())))
     typo-corrections))

  (define (check-trademarks description)
    "Check that DESCRIPTION does not contain '™' or '®' characters.  See
http://www.gnu.org/prep/standards/html_node/Trademarks.html."
    (match (string-index description (char-set #\™ #\®))
      ((and (? number?) index)
       (list
        (make-warning package
                      (G_ "description should not contain ~
trademark sign '~a' at ~d")
                      (list (string-ref description index) index)
                      #:field 'description)))
      (else '())))

  (define (check-quotes description)
    "Check whether DESCRIPTION contains single quotes and suggest @code."
    (if (regexp-exec %quoted-identifier-rx description)
        (list
         (make-warning package
                       ;; TRANSLATORS: '@code' is Texinfo markup and must be kept
                       ;; as is.
                       (G_ "use @code or similar ornament instead of quotes")
                       #:field 'description))
        '()))

  (define (check-proper-start description)
    (if (or (string-null? description)
            (properly-starts-sentence? description)
            (string-prefix-ci? (package-name package) description))
        '()
        (list
         (make-warning
          package
          (G_ "description should start with an upper-case letter or digit")
          #:field 'description))))

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
      (if (null? infractions)
          '()
          (list
           (make-warning package
                         (G_ "sentences in description should be followed ~
by two spaces; possible infraction~p at ~{~a~^, ~}")
                         (list (length infractions)
                               infractions)
                         #:field 'description)))))

  (define (check-no-leading-whitespace description)
    "Check that DESCRIPTION doesn't have trailing whitespace."
    (if (string-prefix? " " description)
        (list
         (make-warning package
                       (G_ "description contains leading whitespace")
                       #:field 'description))
        '()))

  (define (check-no-trailing-whitespace description)
    "Check that DESCRIPTION doesn't have trailing whitespace."
    (if (string-suffix? " " description)
        (list
         (make-warning package
                       (G_ "description contains trailing whitespace")
                       #:field 'description))
        '()))

  (let ((description (package-description package)))
    (if (string? description)
        (append
         (check-not-empty description)
         (check-quotes description)
         (check-trademarks description)
         (check-description-typo description '(("This packages" . "This package")
                                               ("This modules" . "This module")
                                               ("allows to" . #f)
                                               ("permits to" . #f)))
         ;; Use raw description for this because Texinfo rendering
         ;; automatically fixes end of sentence space.
         (check-end-of-sentence-space description)
         (check-no-leading-whitespace description)
         (check-no-trailing-whitespace description)
         (match (check-texinfo-markup description)
           ((and warning (? lint-warning?)) (list warning))
           (plain-description
            (check-proper-start plain-description))))
        (list
         (make-warning package
                       (G_ "invalid description: ~s")
                       (list description)
                       #:field 'description)))))

(define (package-input-intersection inputs-to-check input-names)
  "Return the intersection between INPUTS-TO-CHECK, the list of input tuples
of a package, and INPUT-NAMES, a list of package specifications such as
\"glib:bin\"."
  (match inputs-to-check
    (((labels packages . outputs) ...)
     (filter-map (lambda (package output)
                   (and (package? package)
                        (let ((input (string-append
                                      (package-name package)
                                      (if (> (length output) 0)
                                          (string-append ":" (car output))
                                          ""))))
                          (and (member input input-names)
                               input))))
                 packages outputs))))

(define (check-inputs-should-be-native package)
  ;; Emit a warning if some inputs of PACKAGE are likely to belong to its
  ;; native inputs.
  (let ((inputs (append (package-inputs package)
                        (package-propagated-inputs package)))
        (input-names
         '("pkg-config"
            "autoconf"
            "automake"
            "bison"
            "cmake"
            "dejagnu"
            "desktop-file-utils"
            "doxygen"
            "extra-cmake-modules"
            "flex"
            "gettext"
            "glib:bin"
            "gobject-introspection"
            "googletest-source"
            "groff"
            "gtk-doc"
            "help2man"
            "intltool"
            "itstool"
            "kdoctools"
            "libtool"
            "m4"
            "qttools"
            "yasm" "nasm" "fasm"
            "python-coverage" "python2-coverage"
            "python-cython" "python2-cython"
            "python-docutils" "python2-docutils"
            "python-mock" "python2-mock"
            "python-nose" "python2-nose"
            "python-pbr" "python2-pbr"
            "python-pytest" "python2-pytest"
            "python-pytest-cov" "python2-pytest-cov"
            "python-setuptools-scm" "python2-setuptools-scm"
            "python-sphinx" "python2-sphinx"
            "scdoc"
            "swig"
            "qmake"
            "qttools"
            "texinfo"
            "xorg-server-for-tests"
            "yelp-tools")))
    (map (lambda (input)
           (make-warning
            package
            (G_ "'~a' should probably be a native input")
            (list input)
            #:field 'inputs))
         (package-input-intersection inputs input-names))))

(define (check-inputs-should-not-be-an-input-at-all package)
  ;; Emit a warning if some inputs of PACKAGE are likely to should not be
  ;; an input at all.
  (let ((input-names '("python-setuptools"
                       "python2-setuptools"
                       "python-pip"
                       "python2-pip")))
    (map (lambda (input)
           (make-warning
            package
            (G_ "'~a' should probably not be an input at all")
            (list input)
            #:field 'inputs))
         (package-input-intersection (package-direct-inputs package)
                                     input-names))))

(define (check-input-labels package)
  "Emit a warning for labels that differ from the corresponding package name."
  (define (check input-kind package-inputs)
    (define (warning label name)
      (make-warning package
                    (G_ "label '~a' does not match package name '~a'")
                    (list label name)
                    #:field input-kind))

    (append-map (match-lambda
                  (((? string? label) (? package? dependency))
                   (if (string=? label (package-name dependency))
                       '()
                       (list (warning label (package-name dependency)))))
                  (((? string? label) (? package? dependency) output)
                   (let ((expected (string-append (package-name dependency)
                                                  ":" output)))
                     (if (string=? label expected)
                         '()
                         (list (warning label expected)))))
                  (_
                   '()))
                (package-inputs package)))

  (append-map (match-lambda
                ((kind proc)
                 (check kind proc)))
              `((native-inputs ,package-native-inputs)
                (inputs ,package-inputs)
                (propagated-inputs ,package-propagated-inputs))))

(define (report-wrap-program-error package wrapper-name)
  "Warn that \"bash-minimal\" is missing from 'inputs', while WRAPPER-NAME
requires it."
  (make-warning package
                (G_ "\"bash-minimal\" should be in 'inputs' when '~a' is used")
                (list wrapper-name)))

(define (check-wrapper-inputs package)
  "Emit a warning if PACKAGE uses 'wrap-program' or similar, but \"bash\"
or \"bash-minimal\" is not in its inputs. 'wrap-script' is not supported."
  (define input-names '("bash" "bash-minimal"))
  (define has-bash-input?
    (pair? (package-input-intersection (package-inputs package)
                                       input-names)))
  (define (check-procedure-body body)
    (match body
      ;; Explicitely setting an interpreter is acceptable.
      (('wrap-program _ '#:sh . _) '())
      (('wrap-program _ . _)
       (list (report-wrap-program-error package 'wrap-program)))
      ;; Wrapper of 'wrap-program' for Qt programs.
      ;; TODO #:sh is not yet supported but probably will be.
      (('wrap-qt-program _ '#:sh . _) '())
      (('wrap-qt-program _ . _)
       (list (report-wrap-program-error package 'wrap-qt-program)))
      ((x . y)
       (append (check-procedure-body x) (check-procedure-body y)))
      (_ '())))
  (define (check-phase-procedure expression)
    (find-procedure-body expression check-procedure-body))
  (define (check-delta expression)
    (find-phase-procedure package expression check-phase-procedure))
  (define (check-deltas deltas)
    (append-map check-delta deltas))
  (if has-bash-input?
      ;; "bash" (or "bash-minimal") is in 'inputs', so everything seems ok.
      '()
      ;; "bash" is not in 'inputs'.  Verify 'wrap-program' and friends
      ;; are unused
      (find-phase-deltas package check-deltas)))

(define (package-name-regexp package)
  "Return a regexp that matches PACKAGE's name as a word at the beginning of a
line."
  (make-regexp (string-append "^" (regexp-quote (package-name package))
                              "\\>")
               regexp/icase))

(define (check-synopsis-style package)
  ;; Emit a warning if stylistic issues are found in the synopsis of PACKAGE.
  (define (check-final-period synopsis)
    ;; Synopsis should not end with a period, except for some special cases.
    (if (and (string-suffix? "." synopsis)
             (not (string-suffix? "etc." synopsis)))
        (list
         (make-warning package
                       (G_ "no period allowed at the end of the synopsis")
                       #:field 'synopsis))
        '()))

  (define check-start-article
    ;; Skip this check for GNU packages, as suggested by Karl Berry's reply to
    ;; <http://lists.gnu.org/archive/html/bug-womb/2014-11/msg00000.html>.
    (if (false-if-exception (gnu-package? package))
        (const '())
        (lambda (synopsis)
          (if (or (string-prefix-ci? "A " synopsis)
                  (string-prefix-ci? "An " synopsis))
              (list
               (make-warning package
                             (G_ "no article allowed at the beginning of \
the synopsis")
                             #:field 'synopsis))
              '()))))

  (define (check-synopsis-length synopsis)
    (if (>= (string-length synopsis) 80)
        (list
         (make-warning package
                       (G_ "synopsis should be less than 80 characters long")
                       #:field 'synopsis))
        '()))

  (define (check-proper-start synopsis)
    (if (properly-starts-sentence? synopsis)
        '()
        (list
         (make-warning package
                       (G_ "synopsis should start with an upper-case letter or digit")
                       #:field 'synopsis))))

  (define (check-start-with-package-name synopsis)
    (if (and (regexp-exec (package-name-regexp package) synopsis)
               (not (starts-with-abbreviation? synopsis)))
        (list
         (make-warning package
                       (G_ "synopsis should not start with the package name")
                       #:field 'synopsis))
        '()))

  (define (check-texinfo-markup synopsis)
    "Check that SYNOPSIS can be parsed as a Texinfo fragment.  If the
markup is valid return a plain-text version of SYNOPSIS, otherwise #f."
    (catch #t
      (lambda ()
        (texi->plain-text synopsis)
        '())
      (lambda (keys . args)
        (list
         (make-warning package
                       (G_ "Texinfo markup in synopsis is invalid")
                       #:field 'synopsis)))))

  (define (check-no-trailing-whitespace synopsis)
    "Check that SYNOPSIS doesn't have trailing whitespace."
    (if (string-suffix? " " synopsis)
        (list
         (make-warning package
                       (G_ "synopsis contains trailing whitespace")
                       #:field 'synopsis))
        '()))

  (define checks
    (list check-proper-start
          check-final-period
          check-start-article
          check-start-with-package-name
          check-synopsis-length
          check-texinfo-markup
          check-no-trailing-whitespace))

  (match (package-synopsis package)
    (""
     (list
      (make-warning package
                    (G_ "synopsis should not be empty")
                    #:field 'synopsis)))
    ((? string? synopsis)
     (append-map
      (lambda (proc)
        (proc synopsis))
      checks))
    (invalid
     (list
      (make-warning package
                    (G_ "invalid synopsis: ~s")
                    (list invalid)
                    #:field 'synopsis)))))

(define* (probe-uri uri #:key timeout)
  "Probe URI, a URI object, and return two values: a symbol denoting the
probing status, such as 'http-response' when we managed to get an HTTP
response from URI, and additional details, such as the actual HTTP response.

TIMEOUT is the maximum number of seconds (possibly an inexact number) to wait
for connections to complete; when TIMEOUT is #f, wait as long as needed."
  (define headers
    '((User-Agent . "GNU Guile")
      (Accept . "*/*")))

  (let loop ((uri     uri)
             (visited '()))
    (match (uri-scheme uri)
      ((or 'http 'https)
       (catch #t
         (lambda ()
           (let ((port    (guix:open-connection-for-uri
                           uri #:timeout timeout))
                 (request (build-request uri #:headers headers)))
             (define response
               (dynamic-wind
                 (const #f)
                 (lambda ()
                   (write-request request port)
                   (force-output port)
                   (read-response port))
                 (lambda ()
                   (close-port port))))

             (case (response-code response)
               ((302                    ; found (redirection)
                 303                    ; see other
                 307                    ; temporary redirection
                 308)                   ; permanent redirection
                (let ((location (response-location response)))
                  (if (or (not location) (member location visited))
                      (values 'http-response response)
                      (loop location (cons location visited))))) ;follow the redirect
               ((301)                   ; moved permanently
                (let ((location (response-location response)))
                  ;; Return RESPONSE, unless the final response as we follow
                  ;; redirects is not 200.
                  (if location
                      (let-values (((status response2)
                                    (loop location (cons location visited))))
                        (case status
                          ((http-response)
                           (values 'http-response
                                   (if (= 200 (response-code response2))
                                       response
                                       response2)))
                          (else
                           (values status response2))))
                      (values 'http-response response)))) ;invalid redirect
               (else
                (values 'http-response response)))))
         (lambda (key . args)
           (case key
             ((bad-header bad-header-component)
              ;; This can happen if the server returns an invalid HTTP header,
              ;; as is the case with the 'Date' header at sqlite.org.
              (values 'invalid-http-response #f))
             ((getaddrinfo-error system-error
               gnutls-error tls-certificate-error)
              (values key args))
             (else
              (apply throw key args))))))
      ('ftp
       (catch #t
         (lambda ()
           (let ((conn (ftp-open (uri-host uri) #:timeout timeout)))
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

(define (call-with-networking-fail-safe message error-value proc)
  "Call PROC catching any network-related errors.  Upon a networking error,
display a message including MESSAGE and return ERROR-VALUE."
  (guard (c ((http-get-error? c)
             (warning (G_ "~a: HTTP GET error for ~a: ~a (~s)~%")
                      message
                      (uri->string (http-get-error-uri c))
                      (http-get-error-code c)
                      (http-get-error-reason c))
             error-value))
    (catch #t
      proc
      (match-lambda*
        (('getaddrinfo-error errcode)
         (warning (G_ "~a: host lookup failure: ~a~%")
                  message
                  (gai-strerror errcode))
         error-value)
        (('tls-certificate-error args ...)
         (warning (G_ "~a: TLS certificate error: ~a")
                  message
                  (tls-certificate-error-string args))
         error-value)
        (('gnutls-error error function _ ...)
         (warning (G_ "~a: TLS error in '~a': ~a~%")
                  message
                  function (error->string error))
         error-value)
        ((and ('system-error _ ...) args)
         (let ((errno (system-error-errno args)))
           (if (member errno (list ECONNRESET ECONNABORTED ECONNREFUSED))
               (let ((details (call-with-output-string
                                (lambda (port)
                                  (print-exception port #f (car args)
                                                   (cdr args))))))
                 (warning (G_ "~a: ~a~%") message details)
                 error-value)
               (apply throw args))))
        (args
         (apply throw args))))))

(define-syntax-rule (with-networking-fail-safe message error-value exp ...)
  (call-with-networking-fail-safe message error-value
                                  (lambda () exp ...)))

(define (tls-certificate-error-string args)
  "Return a string explaining the 'tls-certificate-error' arguments ARGS."
  (call-with-output-string
    (lambda (port)
      (print-exception port #f
                       'tls-certificate-error args))))

(define (validate-uri uri package field)
  "Return #t if the given URI can be reached, otherwise return a warning for
PACKAGE mentioning the FIELD."
  (let-values (((status argument)
                (probe-uri uri #:timeout 3)))     ;wait at most 3 seconds
    (case status
      ((http-response)
       (cond ((= 200 (response-code argument))
              (match (response-content-length argument)
                ((? number? length)
                 ;; As of July 2016, SourceForge returns 200 (instead of 404)
                 ;; with a small HTML page upon failure.  Attempt to detect
                 ;; such malicious behavior.
                 (or (> length 1000)
                     (make-warning package
                                   (G_ "URI ~a returned \
suspiciously small file (~a bytes)")
                                   (list (uri->string uri)
                                         length)
                                   #:field field)))
                (_ #t)))
             ((= 301 (response-code argument))
              (if (response-location argument)
                  (make-warning package
                                (G_ "permanent redirect from ~a to ~a")
                                (list (uri->string uri)
                                      (uri->string
                                       (response-location argument)))
                                #:field field)
                  (make-warning package
                                (G_ "invalid permanent redirect \
from ~a")
                                (list (uri->string uri))
                                #:field field)))
             (else
              (make-warning package
                            (G_ "URI ~a not reachable: ~a (~s)")
                            (list (uri->string uri)
                                  (response-code argument)
                                  (response-reason-phrase argument))
                            #:field field))))
      ((ftp-response)
       (match argument
         (('ok) #t)
         (('error port command code message)
          (make-warning package
                        (G_ "URI ~a not reachable: ~a (~s)")
                        (list (uri->string uri)
                              code (string-trim-both message))
                        #:field field))))
      ((getaddrinfo-error)
       (make-warning package
                     (G_ "URI ~a domain not found: ~a")
                     (list (uri->string uri)
                           (gai-strerror (car argument)))
                     #:field field))
      ((system-error)
       (make-warning package
                     (G_ "URI ~a unreachable: ~a")
                     (list (uri->string uri)
                           (strerror
                            (system-error-errno
                             (cons status argument))))
                     #:field field))
      ((tls-certificate-error)
       (make-warning package
                     (G_ "TLS certificate error: ~a")
                     (list (tls-certificate-error-string argument))
                     #:field field))
      ((invalid-http-response gnutls-error)
       ;; Probably a misbehaving server; ignore.
       #f)
      ((unknown-protocol)                         ;nothing we can do
       #f)
      (else
       (error "internal linter error" status)))))

(define (check-home-page package)
  "Emit a warning if PACKAGE has an invalid 'home-page' field, or if that
'home-page' is not reachable."
  (let ((uri (and=> (package-home-page package) string->uri)))
    (cond
     ((uri? uri)
      (match (validate-uri uri package 'home-page)
        ((and (? lint-warning? warning) warning)
         (list warning))
        (_ '())))
     ((not (package-home-page package))
      (if (or (string-contains (package-name package) "bootstrap")
              (string=? (package-name package) "ld-wrapper"))
          '()
          (list
           (make-warning package
                         (G_ "invalid value for home page")
                         #:field 'home-page))))
     (else
      (list
       (make-warning package
                     (G_ "invalid home page URL: ~s")
                     (list (package-home-page package))
                     #:field 'home-page))))))

(define %distro-directory
  (mlambda ()
    (dirname (search-path %load-path "gnu.scm"))))

(define (check-patch-file-names package)
  "Emit a warning if the patches requires by PACKAGE are badly named or if the
patch could not be found."
  (guard (c ((formatted-message? c)               ;raised by 'search-patch'
             (list (%make-warning package
                                  (formatted-message-string c)
                                  (formatted-message-arguments c)
                                  #:field 'source))))
    (define patches
      (match (package-source package)
        ((? origin? origin) (origin-patches origin))
        (_ '())))

    (define (starts-with-package-name? file-name)
      (and=> (string-contains file-name (package-name package))
             zero?))

    (append
     (if (every (match-lambda        ;patch starts with package name?
                  ((? string? patch)
                   (starts-with-package-name? (basename patch)))
                  ((? origin? patch)
                   (starts-with-package-name? (origin-actual-file-name patch)))
                  (_  #f))     ;must be some other file-like object
                patches)
         '()
         (list
          (make-warning
           package
           (G_ "file names of patches should start with the package name")
           #:field 'patch-file-names)))

     ;; Check whether we're reaching tar's maximum file name length.
     (let ((prefix (string-length (%distro-directory)))
           ;; Margin approximating the largest path that "make dist" might
           ;; create, with a release candidate version, 123456 commits, and
           ;; git commit hash abcde0.
           (margin (string-length "guix-92.0.0rc3-123456-abcde0/"))
           ;; Tested maximum patch file length for ustar format.
           (max    151))
       (filter-map (match-lambda
                     ((? string? patch)
                      (if (> (+ margin (if (string-prefix? (%distro-directory)
                                                           patch)
                                           (- (string-length patch) prefix)
                                           (string-length patch)))
                             max)
                          (make-warning
                           package
                           (G_ "~a: file name is too long, which may break 'make dist'")
                           (list (basename patch))
                           #:field 'patch-file-names)
                          #f))
                     (_ #f))
                   patches)))))

(define (check-patch-headers package)
  "Check that PACKAGE's patches start with a comment.  Return a list of
warnings."
  (define (blank? str)
    (string-every char-set:blank str))

  (define (patch-header-warnings patch)
    (call-with-input-file patch
      (lambda (port)
        ;; Read from PORT until a non-blank line is found or EOF is reached.
        (let loop ()
          (let ((line (read-line port)))
            (cond ((eof-object? line)
                   (list (make-warning package
                                       (G_ "~a: empty patch")
                                       (list (basename patch))
                                       #:field 'source)))
                  ((blank? line)
                   (loop))
                  ((or (string-prefix? "--- " line)
                       (string-prefix? "+++ " line)
                       (string-prefix? "diff --git " line))
                   (list (make-warning package
                                       (G_ "~a: patch lacks comment and \
upstream status")
                                       (list (basename patch))
                                       #:field 'source)))
                  (else
                   '())))))))

  (guard (c ((formatted-message? c)               ;raised by 'search-patch'
             (list (%make-warning package
                                  (formatted-message-string c)
                                  (formatted-message-arguments c)
                                  #:field 'source))))
   (let ((patches (if (origin? (package-source package))
                      (origin-patches (package-source package))
                      '())))
     (append-map (lambda (patch)
                   ;; Dismiss PATCH if it's an origin or similar.
                   (cond ((string? patch)
                          (patch-header-warnings patch))
                         ((local-file? patch)
                          (patch-header-warnings
                           (local-file-absolute-file-name patch)))
                         (else
                          '())))
                 patches))))

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
  (mlambda ()
    "A memoizing version of 'official-gnu-packages' that returns the empty
list when something goes wrong, such as a networking issue."
    (let ((gnus (false-if-exception (official-gnu-packages))))
      (or gnus '()))))

(define (check-gnu-synopsis+description package)
  "Make sure that, if PACKAGE is a GNU package, it uses the synopsis and
descriptions maintained upstream."
  (match (find (lambda (descriptor)
                 (string=? (gnu-package-name descriptor)
                           (package-name package)))
               (official-gnu-packages*))
    (#f                                   ;not a GNU package, so nothing to do
     '())
    (descriptor                                   ;a genuine GNU package
     (append
      (let ((upstream   (gnu-package-doc-summary descriptor))
            (downstream (package-synopsis package)))
        (if (and upstream
                 (or (not (string? downstream))
                     (not (string=? upstream downstream))))
            (list
             (make-warning package
                           (G_ "proposed synopsis: ~s~%")
                           (list upstream)
                           #:field 'synopsis))
            '()))

      (let ((upstream   (gnu-package-doc-description descriptor))
            (downstream (package-description package)))
        (if (and upstream
                 (or (not (string? downstream))
                     (not (string=? (fill-paragraph upstream 100)
                                    (fill-paragraph downstream 100)))))
            (list
             (make-warning
              package
              (G_ "proposed description:~%     \"~a\"~%")
              (list (fill-paragraph (escape-quotes upstream) 77 7))
              #:field 'description))
            '()))))))

(define (origin-uris origin)
  "Return the list of URIs (strings) for ORIGIN."
  (match (origin-uri origin)
    ((? string? uri)
     (list uri))
    ((uris ...)
     uris)))

(define (check-source package)
  "Emit a warning if PACKAGE has an invalid 'source' field, or if that
'source' is not reachable."
  (define (warnings-for-uris uris)
    (let loop ((uris uris)
               (warnings '()))
      (match uris
        (()
         (reverse warnings))
        ((uri rest ...)
         (match (validate-uri uri package 'source)
           (#t
            ;; We found a working URL, so stop right away.
            '())
           (#f
            ;; Unsupported URL or other error, skip.
            (loop rest warnings))
           ((? lint-warning? warning)
            (loop rest (cons warning warnings))))))))

  (let ((origin (package-source package)))
    (if (origin? origin)
        (cond
         ((eq? (origin-method origin) url-fetch)
          (let* ((uris     (append-map (cut maybe-expand-mirrors <> %mirrors)
                                       (map string->uri (origin-uris origin))))
                 (warnings (warnings-for-uris uris)))

            ;; Just make sure that at least one of the URIs is valid.
            (if (= (length uris) (length warnings))
                ;; When everything fails, report all of WARNINGS, otherwise don't
                ;; report anything.
                ;;
                ;; XXX: Ideally we'd still allow warnings to be raised if *some*
                ;; URIs are unreachable, but distinguish that from the error case
                ;; where *all* the URIs are unreachable.
                (cons*
                 (make-warning package
                               (G_ "all the source URIs are unreachable:")
                               #:field 'source)
                 warnings)
                '())))
         ((git-reference? (origin-uri origin))
          (warnings-for-uris
           (list (string->uri (git-reference-url (origin-uri origin))))))
         (else
          '()))
        '())))

(define (check-source-file-name package)
  "Emit a warning if PACKAGE's origin has no meaningful file name."
  (define (origin-file-name-valid? origin)
    ;; Return #f if the source file name contains only a version or is #f;
    ;; indicates that the origin needs a 'file-name' field.
    (let ((file-name (origin-actual-file-name origin))
          (version (package-version package)))
      (and file-name
           ;; Common in many projects is for the filename to start
           ;; with a "v" followed by the version,
           ;; e.g. "v3.2.0.tar.gz".
           (not (string-match (string-append "^v?" version) file-name)))))

  (let ((origin (package-source package)))
    (if (or (not (origin? origin)) (origin-file-name-valid? origin))
        '()
        (list
         (make-warning package
                       (G_ "the source file name should contain the package name")
                       #:field 'source)))))

(define (check-source-unstable-tarball package)
  "Emit a warning if PACKAGE's source is an autogenerated tarball."
  (define (check-source-uri uri)
    (if (and (string=? (uri-host (string->uri uri)) "github.com")
             (match (split-and-decode-uri-path
                     (uri-path (string->uri uri)))
               ((_ _ "archive" _ ...) #t)
               (_ #f)))
        (make-warning package
                      (G_ "the source URI should not be an autogenerated tarball")
                      #:field 'source)
        #f))

  (let ((origin (package-source package)))
    (if (and (origin? origin)
             (eqv? (origin-method origin) url-fetch))
        (filter-map check-source-uri
                    (origin-uris origin))
        '())))

(define (check-mirror-url package)
  "Check whether PACKAGE uses source URLs that should be 'mirror://'."
  (define (check-mirror-uri uri)                  ;XXX: could be optimized
    (let loop ((mirrors %mirrors))
      (match mirrors
        (()
         #f)
        (((mirror-id mirror-urls ...) rest ...)
         (match (find (cut string-prefix? <> uri) mirror-urls)
           (#f
            (loop rest))
           (prefix
            (make-warning package
                          (G_ "URL should be \
'mirror://~a/~a'")
                          (list mirror-id
                                (string-drop uri (string-length prefix)))
                          #:field 'source)))))))

  (let ((origin (package-source package)))
    (if (and (origin? origin)
             (eqv? (origin-method origin) url-fetch))
        (let ((uris (origin-uris origin)))
          (filter-map check-mirror-uri uris))
        '())))

(define* (check-github-url package #:key (timeout 3))
  "Check whether PACKAGE uses source URLs that redirect to GitHub."
  (define (follow-redirect url)
    (let* ((uri      (string->uri url))
           (port     (guix:open-connection-for-uri uri #:timeout timeout))
           (response (http-head uri #:port port)))
      (close-port port)
      (case (response-code response)
        ((301 302)
         (uri->string (assoc-ref (response-headers response) 'location)))
        (else #f))))

  (define (follow-redirects-to-github uri)
    (cond
     ((string-prefix? "https://github.com/" uri) uri)
     ((string-prefix? "http" uri)
      (and=> (follow-redirect uri) follow-redirects-to-github))
     ;; Do not attempt to follow redirects on URIs other than http and https
     ;; (such as mirror, file)
     (else #f)))

  (let ((origin (package-source package)))
    (if (and (origin? origin)
             (eqv? (origin-method origin) url-fetch))
        (filter-map
         (lambda (uri)
           (and=> (with-networking-fail-safe
                   (format #f (G_ "while accessing '~a'") uri)
                   #f
                   (follow-redirects-to-github uri))
                  (lambda (github-uri)
                    (and (not (string=? github-uri uri))
                         (make-warning
                          package
                          (G_ "URL should be '~a'")
                          (list github-uri)
                          #:field 'source)))))
         (origin-uris origin))
        '())))

;; Guile 3.0.0 does not export this predicate.
(define exception-with-kind-and-args?
  (exception-predicate &exception-with-kind-and-args))

(define (check-optional-tests package)
  "Emit a warning if the test suite is run unconditionally."
  (define (sexp-contains-atom? sexp atom)
    "Test if SEXP contains ATOM."
    (if (pair? sexp)
        (or (sexp-contains-atom? (car sexp) atom)
            (sexp-contains-atom? (cdr sexp) atom))
        (eq? sexp atom)))
  (define (sexp-uses-tests?? sexp)
    "Test if SEXP contains the symbol 'tests?'."
    (sexp-contains-atom? sexp 'tests?))
  (define (check-procedure-body code)
    (if (sexp-uses-tests?? code)
        '()
        (list (make-warning package
                            ;; TRANSLATORS: check and #:tests? are a
                            ;; Scheme symbol and keyword respectively
                            ;; and should not be translated.
                            (G_ "the 'check' phase should respect #:tests?")
                            #:field 'arguments))))
  (define (check-check-procedure expression)
    (find-procedure-body expression check-procedure-body))
  (define (check-phases-delta delta)
    (match delta
      (`(replace 'check ,expression)
       (check-check-procedure expression))
      (_ '())))
  (define (check-phases-deltas deltas)
    (append-map check-phases-delta deltas))
  (find-phase-deltas package check-phases-deltas))

(define* (check-derivation package #:key store)
  "Emit a warning if we fail to compile PACKAGE to a derivation."
  (define (try store system)
    (guard (c ((store-protocol-error? c)
               (make-warning package
                             (G_ "failed to create ~a derivation: ~a")
                             (list system
                                   (store-protocol-error-message c))))
              ((exception-with-kind-and-args? c)
               (make-warning package
                             (G_ "failed to create ~a derivation: ~s")
                             (list system
                                   (cons (exception-kind c)
                                         (exception-args c)))))
              ((message-condition? c)
               (make-warning package
                             (G_ "failed to create ~a derivation: ~a")
                             (list system
                                   (condition-message c))))
              ((formatted-message? c)
               (let ((str (apply format #f
                                 (formatted-message-string c)
                                 (formatted-message-arguments c))))
                 (make-warning package
                               (G_ "failed to create ~a derivation: ~a")
                               (list system str)))))
      (parameterize ((%graft? #f))
        (package-derivation store package system #:graft? #f)

        ;; If there's a replacement, make sure we can compute its
        ;; derivation.
        (match (package-replacement package)
          (#f #t)
          (replacement
           (package-derivation store replacement system
                               #:graft? #f))))))

  (define (check-with-store store)
    (filter lint-warning?
            (map (cut try store <>) (package-supported-systems package))))

  ;; For backwards compatability, don't rely on store being set
  (or (and=> store check-with-store)
      (with-store store
        (check-with-store store))))

(define* (check-profile-collisions package #:key store)
  "Check for collisions that would occur when installing PACKAGE as a result
of the propagated inputs it pulls in."
  (define (do-check store)
    (guard (c ((profile-collision-error? c)
               (let ((first  (profile-collision-error-entry c))
                     (second (profile-collision-error-conflict c)))
                 (define format
                   (if (string=? (manifest-entry-version first)
                                 (manifest-entry-version second))
                       manifest-entry-item
                       (lambda (entry)
                         (string-append (manifest-entry-name entry) "@"
                                        (manifest-entry-version entry)))))

                 (list (make-warning package
                                     (G_ "propagated inputs ~a and ~a collide")
                                     (list (format first)
                                           (format second)))))))
      ;; Disable grafts to avoid building PACKAGE and its dependencies.
      (parameterize ((%graft? #f))
        (run-with-store store
          (mbegin %store-monad
            (check-for-collisions (packages->manifest (list package))
                                  (%current-system))
            (return '()))))))

  (if store
      (do-check store)
      (with-store store
        (do-check store))))

(define (check-license package)
  "Warn about type errors of the 'license' field of PACKAGE."
  (match (package-license package)
    ((or (? license?)
         ((? license?) ...))
     '())
    (x
     (list
      (make-warning package (G_ "invalid license field")
                    #:field 'license)))))

(define (current-vulnerabilities*)
  "Like 'current-vulnerabilities', but return the empty list upon networking
or HTTP errors.  This allows network-less operation and makes problems with
the NIST server non-fatal."
  (with-networking-fail-safe (G_ "while retrieving CVE vulnerabilities")
                             '()
                             (current-vulnerabilities #:timeout 4)))

(define package-vulnerabilities
  (let ((lookup (delay (vulnerabilities->lookup-proc
                        (current-vulnerabilities*)))))
    (lambda (package)
      "Return a list of vulnerabilities affecting PACKAGE."
      ;; First we retrieve the Common Platform Enumeration (CPE) name and
      ;; version for PACKAGE, then we can pass them to LOOKUP.
      (let ((name    (or (assoc-ref (package-properties package)
                                    'cpe-name)
                         (package-name package)))
            (version (or (assoc-ref (package-properties package)
                                    'cpe-version)
                         (package-version package))))
        ((force lookup) name version)))))

(define* (check-vulnerabilities package
                                #:optional (package-vulnerabilities
                                            package-vulnerabilities))
  "Check for known vulnerabilities for PACKAGE.  Obtain the list of
vulnerability records for PACKAGE by calling PACKAGE-VULNERABILITIES."
  (let ((package (or (package-replacement package) package)))
    (match (package-vulnerabilities package)
      (()
       '())
      ((vulnerabilities ...)
       (let* ((patched    (package-patched-vulnerabilities package))
              (known-safe (or (assq-ref (package-properties package)
                                        'lint-hidden-cve)
                              '()))
              (unpatched (remove (lambda (vuln)
                                   (let ((id (vulnerability-id vuln)))
                                     (or (member id patched)
                                         (member id known-safe))))
                                 vulnerabilities)))
         (if (null? unpatched)
             '()
             (list
              (make-warning
               package
               (G_ "probably vulnerable to ~a")
               (list (string-join (map vulnerability-id unpatched)
                                  ", "))))))))))

(define (check-for-updates package)
  "Check if there is an update available for PACKAGE."
  (match (lookup-updater package)
    (#f
     (list (make-warning package (G_ "no updater for ~a")
                         (list (package-name package))
                         #:field 'source)))
    ((? upstream-updater? updater)
     (match (with-networking-fail-safe
             (format #f (G_ "while retrieving upstream info for '~a'")
                     (package-name package))
             #f
             (package-latest-release package))
       ((? upstream-source? source)
        (if (version>? (upstream-source-version source)
                       (package-version package))
            (list
             (make-warning package
                           (G_ "can be upgraded to ~a")
                           (list (upstream-source-version source))
                           #:field 'version))
            '()))
       (#f                                       ;cannot find upstream release
        (list (make-warning package
                            (G_ "updater '~a' failed to find \
upstream releases")
                            (list (upstream-updater-name updater))
                            #:field 'source)))))))


(define (lookup-disarchive-spec hash)
  "If Disarchive mirrors have a spec for HASH, return the list of SWH
directory identifiers the spec refers to.  Otherwise return #f."
  (define (extract-swh-id spec)
    ;; Return the list of SWH directory identifiers SPEC refers to, where SPEC
    ;; is a Disarchive sexp.  Instead of attempting to parse it, traverse it
    ;; in a pretty unintelligent fashion.
    (let loop ((sexp spec)
               (ids '()))
      (match sexp
        ((? string? str)
         (let ((prefix "swh:1:dir:"))
           (if (string-prefix? prefix str)
               (cons (string-drop str (string-length prefix)) ids)
               ids)))
        ((head tail ...)
         (loop tail (loop head ids)))
        (_ ids))))

  (any (lambda (mirror)
         (with-networking-fail-safe
          (format #f (G_ "failed to access Disarchive database at ~a")
                  mirror)
          #f
          (guard (c ((http-get-error? c) #f))
            (let* ((url (string-append mirror
                                       (symbol->string
                                        (content-hash-algorithm hash))
                                       "/"
                                       (bytevector->base16-string
                                        (content-hash-value hash))))
                   (port (http-fetch (string->uri url) #:text? #t))
                   (spec (read port)))
              (close-port port)
              (extract-swh-id spec)))))
       %disarchive-mirrors))

(define (check-archival package)
  "Check whether PACKAGE's source code is archived on Software Heritage.  If
it's not, and if its source code is a VCS snapshot, then send a \"save\"
request to Software Heritage.

Software Heritage imposes limits on the request rate per client IP address.
This checker prints a notice and stops doing anything once that limit has been
reached."
  (define (response->warning url method response)
    (if (request-rate-limit-reached? url method)
        (list (make-warning package
                            (G_ "Software Heritage rate limit reached; \
try again later")
                            #:field 'source))
        (list (make-warning package
                            (G_ "'~a' returned ~a")
                            (list url (response-code response))
                            #:field 'source))))

  (define skip-key (gensym "skip-archival-check"))

  (define (skip-when-limit-reached url method)
    (or (not (request-rate-limit-reached? url method))
        (throw skip-key #t)))

  (parameterize ((%allow-request? skip-when-limit-reached))
    (catch #t
      (lambda ()
        (match (and (origin? (package-source package))
                    (package-source package))
          (#f                                     ;no source
           '())
          ((= origin-uri (? git-reference? reference))
           (define url
             (git-reference-url reference))
           (define commit
             (git-reference-commit reference))

           (match (if (commit-id? commit)
                      (or (lookup-revision commit)
                          (lookup-origin-revision url commit))
                      (lookup-origin-revision url commit))
             ((? revision? revision)
              '())
             (#f
              ;; Revision is missing from the archive, attempt to save it.
              (catch 'swh-error
                (lambda ()
                  (save-origin (git-reference-url reference) "git")
                  (list (make-warning
                         package
                         ;; TRANSLATORS: "Software Heritage" is a proper noun
                         ;; that must remain untranslated.  See
                         ;; <https://www.softwareheritage.org>.
                         (G_ "scheduled Software Heritage archival")
                         #:field 'source)))
                (lambda (key url method response . _)
                  (cond ((= 429 (response-code response))
                         (list (make-warning
                                package
                                (G_ "archival rate limit exceeded; \
try again later")
                                #:field 'source)))
                        (else
                         (response->warning url method response))))))))
          ((? origin? origin)
           ;; Since "save" origins are not supported for non-VCS source, all
           ;; we can do is tell whether a given tarball is available or not.
           (if (and=> (origin-hash origin)          ;XXX: for ungoogled-chromium
                      content-hash-value)           ;& icecat
               (let ((hash (origin-hash origin)))
                 (match (lookup-content (content-hash-value hash)
                                        (symbol->string
                                         (content-hash-algorithm hash)))
                   (#f
                    ;; If SWH doesn't have HASH as is, it may be because it's
                    ;; a hand-crafted tarball.  In that case, check whether
                    ;; the Disarchive database has an entry for that tarball.
                    (match (lookup-disarchive-spec hash)
                      (#f
                       (list (make-warning package
                                           (G_ "source not archived on Software \
Heritage and missing from the Disarchive database")
                                           #:field 'source)))
                      (directory-ids
                       (match (find (lambda (id)
                                      (not (lookup-directory id)))
                                    directory-ids)
                         (#f '())
                         (id
                          (list (make-warning package
                                              (G_ "\
Disarchive entry refers to non-existent SWH directory '~a'")
                                              (list id)
                                              #:field 'source)))))))
                   ((? content?)
                    '())))
               '()))
          (_
           (list (make-warning package
                               (G_ "unsupported source type")
                               #:field 'source)))))
      (match-lambda*
        (('swh-error url method response)
         (response->warning url method response))
        ((key . args)
         (if (eq? key skip-key)
             '()
             (with-networking-fail-safe
              (G_ "while connecting to Software Heritage")
              '()
              (apply throw key args))))))))

(define (check-haskell-stackage package)
  "Check whether PACKAGE is a Haskell package ahead of the current
Stackage LTS version."
  (match (with-networking-fail-safe
          (format #f (G_ "while retrieving upstream info for '~a'")
                  (package-name package))
          #f
          (package-latest-release package (list %stackage-updater)))
    ((? upstream-source? source)
     (if (version>? (package-version package)
                    (upstream-source-version source))
         (list
          (make-warning package
                        (G_ "ahead of Stackage LTS version ~a")
                        (list (upstream-source-version source))
                        #:field 'version))
         '()))
    (#f '())))


;;;
;;; Source code formatting.
;;;

(define (report-tabulations package line line-number)
  "Warn about tabulations found in LINE."
  (match (string-index line #\tab)
    (#f #f)
    (index
     (make-warning package
                   (G_ "tabulation on line ~a, column ~a")
                   (list line-number index)
                   #:location
                   (location (package-file package)
                             line-number
                             index)))))

(define (report-trailing-white-space package line line-number)
  "Warn about trailing white space in LINE."
  (and (not (or (string=? line (string-trim-right line))
                (string=? line (string #\page))))
       (make-warning package
                     (G_ "trailing white space on line ~a")
                     (list line-number)
                     #:location
                     (location (package-file package)
                               line-number
                               0))))

(define (report-long-line package line line-number)
  "Emit a warning if LINE is too long."
  ;; Note: We don't warn at 80 characters because sometimes hashes and URLs
  ;; make it hard to fit within that limit and we want to avoid making too
  ;; much noise.
  (and (> (string-length line) 90)
       (make-warning package
                     (G_ "line ~a is way too long (~a characters)")
                     (list line-number (string-length line))
                     #:location
                     (location (package-file package)
                               line-number
                               0))))

(define %hanging-paren-rx
  (make-regexp "^[[:blank:]]*[()]+[[:blank:]]*$"))

(define (report-lone-parentheses package line line-number)
  "Emit a warning if LINE contains hanging parentheses."
  (and (regexp-exec %hanging-paren-rx line)
       (make-warning package
                     (G_ "parentheses feel lonely, \
move to the previous or next line")
                     (list line-number)
                     #:location
                     (location (package-file package)
                               line-number
                               0))))

(define %formatting-reporters
  ;; List of procedures that report formatting issues.  These are not separate
  ;; checkers because they would need to re-read the file.
  (list report-tabulations
        report-trailing-white-space
        report-long-line
        report-lone-parentheses))

(define* (report-formatting-issues package file starting-line
                                   #:key (reporters %formatting-reporters))
  "Report white-space issues in FILE starting from STARTING-LINE, and report
them for PACKAGE."
  (define (sexp-last-line port)
    ;; Return the last line of the sexp read from PORT or an estimate thereof.
    (define &failure (list 'failure))

    (let ((start      (ftell port))
          (start-line (port-line port))
          (sexp       (catch 'read-error
                        (lambda () (read port))
                        (const &failure))))
      (let ((line (port-line port)))
        (seek port start SEEK_SET)
        (set-port-line! port start-line)
        (if (eq? sexp &failure)
            (+ start-line 60)                     ;conservative estimate
            line))))

  (call-with-input-file file
    (lambda (port)
      (let loop ((line-number 1)
                 (last-line #f)
                 (warnings '()))
        (let ((line (read-line port)))
          (if (or (eof-object? line)
                  (and last-line (> line-number last-line)))
              warnings
              (if (and (= line-number starting-line)
                       (not last-line))
                  (loop (+ 1 line-number)
                        (+ 1 (sexp-last-line port))
                        warnings)
                  (loop (+ 1 line-number)
                        last-line
                        (append
                         warnings
                         (if (< line-number starting-line)
                             '()
                             (filter-map (lambda (report)
                                           (report package line line-number))
                                         reporters)))))))))))

(define (check-formatting package)
  "Check the formatting of the source code of PACKAGE."
  (let ((location (package-location package)))
    (if location
        ;; Report issues starting from the line before the 'package'
        ;; form, which usually contains the 'define' form.
        (let ((line (- (location-line location) 1)))
          (match (search-path %load-path (location-file location))
            ((? string? file)
             (report-formatting-issues package file line))
            (#f
             ;; It could be that LOCATION lists a "true" relative file
             ;; name--i.e., not relative to an element of %LOAD-PATH.
             (let ((file (location-file location)))
               (if (file-exists? file)
                   (report-formatting-issues package file line)
                   (list (make-warning package
                                       (G_ "source file not found"))))))))
        '())))


;;;
;;; List of checkers.
;;;

(define %local-checkers
  (list
   (lint-checker
     (name        'name)
     (description "Validate package names")
     (check       check-name))
   (lint-checker
     (name        'tests-true)
     (description "Check if tests are explicitly enabled")
     (check       check-tests-true))
   (lint-checker
     (name        'description)
     (description "Validate package descriptions")
     (check       check-description-style))
   (lint-checker
     (name        'inputs-should-be-native)
     (description "Identify inputs that should be native inputs")
     (check       check-inputs-should-be-native))
   (lint-checker
     (name        'inputs-should-not-be-input)
     (description "Identify inputs that shouldn't be inputs at all")
     (check       check-inputs-should-not-be-an-input-at-all))
   (lint-checker
     (name        'input-labels)
     (description "Identify input labels that do not match package names")
     (check       check-input-labels))
   (lint-checker
     (name        'wrapper-inputs)
     (description "Make sure 'wrap-program' can finds its interpreter.")
     (check       check-wrapper-inputs))
   (lint-checker
     (name        'license)
     ;; TRANSLATORS: <license> is the name of a data type and must not be
     ;; translated.
     (description "Make sure the 'license' field is a <license> \
or a list thereof")
     (check       check-license))
   (lint-checker
     (name        'optional-tests)
     (description "Make sure tests are only run when requested")
     (check       check-optional-tests))
   (lint-checker
     (name        'mirror-url)
     (description "Suggest 'mirror://' URLs")
     (check       check-mirror-url))
   (lint-checker
     (name        'source-file-name)
     (description "Validate file names of sources")
     (check       check-source-file-name))
   (lint-checker
     (name        'source-unstable-tarball)
     (description "Check for autogenerated tarballs")
     (check       check-source-unstable-tarball))
   (lint-checker
     (name            'derivation)
     (description     "Report failure to compile a package to a derivation")
     (check           check-derivation)
     (requires-store? #t))
   (lint-checker
     (name            'profile-collisions)
     (description     "Report collisions that would occur due to propagated inputs")
     (check           check-profile-collisions)
     (requires-store? #t))
   (lint-checker
    (name        'patch-file-names)
    (description "Validate file names and availability of patches")
    (check       check-patch-file-names))
   (lint-checker
    (name        'patch-headers)
    (description "Validate patch headers")
    (check       check-patch-headers))
   (lint-checker
     (name        'formatting)
     (description "Look for formatting issues in the source")
     (check       check-formatting))))

(define %network-dependent-checkers
  (list
   (lint-checker
     (name        'synopsis)
     (description "Validate package synopses")
     (check       check-synopsis-style))
   (lint-checker
     (name        'gnu-description)
     (description "Validate synopsis & description of GNU packages")
     (check       check-gnu-synopsis+description))
   (lint-checker
     (name        'home-page)
     (description "Validate home-page URLs")
     (check       check-home-page))
   (lint-checker
     (name        'source)
     (description "Validate source URLs")
     (check       check-source))
   (lint-checker
     (name        'github-url)
     (description "Suggest GitHub URLs")
     (check       check-github-url))
   (lint-checker
     (name        'cve)
     (description "Check the Common Vulnerabilities and Exposures\
 (CVE) database")
     (check       check-vulnerabilities))
   (lint-checker
     (name        'refresh)
     (description "Check the package for new upstream releases")
     (check       check-for-updates))
   (lint-checker
     (name        'archival)
     (description "Ensure source code archival on Software Heritage")
     (check       check-archival))
   (lint-checker
     (name        'haskell-stackage)
     (description "Ensure Haskell packages use Stackage LTS versions")
     (check       check-haskell-stackage))))

(define %all-checkers
  (append %local-checkers
          %network-dependent-checkers))
