;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts shell)
  #:use-module (guix ui)
  #:use-module (guix scripts environment)
  #:autoload   (guix scripts build) (show-build-options-help)
  #:autoload   (guix transformations) (show-transformation-options-help)
  #:use-module (guix scripts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:export (guix-shell))

(define (show-help)
  (display (G_ "Usage: guix shell [OPTION] PACKAGES... [-- COMMAND...]
Build an environment that includes PACKAGES and execute COMMAND or an
interactive shell in that environment.\n"))
  (newline)

  ;; These two options differ from 'guix environment'.
  (display (G_ "
  -D, --development      include the development inputs of the next package"))
  (display (G_ "
  -f, --file=FILE        create environment for the package that the code within
                         FILE evaluates to"))

  (show-environment-options-help)
  (newline)
  (show-build-options-help)
  (newline)
  (show-transformation-options-help)
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define (tag-package-arg opts arg)
  "Return a two-element list with the form (TAG ARG) that tags ARG with either
'ad-hoc' in OPTS has the 'ad-hoc?' key set to #t, or 'inputs' otherwise."
  (if (assoc-ref opts 'ad-hoc?)
      `(ad-hoc-package ,arg)
      `(package ,arg)))

(define (ensure-ad-hoc alist)
  (if (assq-ref alist 'ad-hoc?)
      alist
      `((ad-hoc? . #t) ,@alist)))

(define (wrapped-option opt)
  "Wrap OPT, a SRFI-37 option, such that its processor always adds the
'ad-hoc?' flag to the resulting alist."
  (option (option-names opt)
          (option-required-arg? opt)
          (option-optional-arg? opt)
          (compose ensure-ad-hoc (option-processor opt))))

(define %options
  ;; Specification of the command-line options.
  (let ((to-remove '("ad-hoc" "inherit" "load" "help" "version")))
    (append
        (list (option '(#\h "help") #f #f
                      (lambda args
                        (show-help)
                        (exit 0)))
              (option '(#\V "version") #f #f
                      (lambda args
                        (show-version-and-exit "guix shell")))

              (option '(#\D "development") #f #f
                      (lambda (opt name arg result)
                        ;; Temporarily remove the 'ad-hoc?' flag from result.
                        ;; The next option will put it back thanks to
                        ;; 'wrapped-option'.
                        (alist-delete 'ad-hoc? result)))

              ;; For consistency with 'guix package', support '-f' rather than
              ;; '-l' like 'guix environment' does.
              (option '(#\f "file") #t #f
                      (lambda (opt name arg result)
                        (alist-cons 'load (tag-package-arg result arg)
                                    result))))
        (filter-map (lambda (opt)
                      (and (not (any (lambda (name)
                                       (member name to-remove))
                                     (option-names opt)))
                           (wrapped-option opt)))
                    %environment-options))))

(define %default-options
  `((ad-hoc? . #t)                                ;always true
    ,@%environment-default-options))

(define (parse-args args)
  "Parse the list of command line arguments ARGS."
  (define (handle-argument arg result)
    (alist-cons 'package (tag-package-arg result arg)
                (ensure-ad-hoc result)))

  ;; The '--' token is used to separate the command to run from the rest of
  ;; the operands.
  (let ((args command (break (cut string=? "--" <>) args)))
    (let ((opts (parse-command-line args %options (list %default-options)
                                    #:argument-handler handle-argument)))
      (match command
        (() opts)
        (("--") opts)
        (("--" command ...) (alist-cons 'exec command opts))))))


(define-command (guix-shell . args)
  (category development)
  (synopsis "spawn one-off software environments")

  (guix-environment* (parse-args args)))
