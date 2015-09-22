;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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


(define-module (test-scripts)
  #:use-module (guix scripts)
  #:use-module ((guix scripts build)
                #:select (%standard-build-options))
  #:use-module (srfi srfi-64))

;; Test the (guix scripts) module.

(define-syntax-rule (with-environment-variable variable value body ...)
  "Run BODY with VARIABLE set to VALUE."
  (let ((orig (getenv variable)))
    (dynamic-wind
      (lambda ()
        (setenv variable value))
      (lambda ()
        body ...)
      (lambda ()
        (if orig
            (setenv variable orig)
            (unsetenv variable))))))


(test-begin "scripts")

(test-equal "parse-command-line"
  '((argument . "bar") (argument . "foo")
    (cores . 10)                                  ;takes precedence
    (substitutes? . #f) (keep-failed? . #t)
    (max-jobs . 77) (cores . 42))

  (with-environment-variable "GUIX_BUILD_OPTIONS" "-c 42 -M 77"
    (parse-command-line '("--keep-failed" "--no-substitutes"
                          "--cores=10" "foo" "bar")
                        %standard-build-options
                        (list '()))))

(test-equal "parse-command-line and --no options"
  '((argument . "foo")
    (substitutes? . #f))                          ;takes precedence

  (with-environment-variable "GUIX_BUILD_OPTIONS" "--no-substitutes"
    (parse-command-line '("foo")
                        %standard-build-options
                        (list '((substitutes? . #t))))))

(test-end "scripts")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'with-environment-variable 'scheme-indent-function 2)
;;; End:
