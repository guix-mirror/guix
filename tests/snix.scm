;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-snix)
  #:use-module (guix import snix)
  #:use-module ((guix utils) #:select (%nixpkgs-directory))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define factorize-uri
  (@@ (guix import snix) factorize-uri))

(define-syntax-rule (every? proc lists ...)
  (not (not (every proc lists ...))))

(test-begin "snix")

(test-assert "factorize-uri"
  (every? (match-lambda
           ((uri version '-> expected)
            (equal? (factorize-uri uri version)
                    expected)))
          '(("http://example.com/foo.tgz" "1.0"
             -> "http://example.com/foo.tgz")
            ("http://example.com/foo-2.8.tgz" "2.8"
             -> ("http://example.com/foo-" version ".tgz"))
            ("http://example.com/2.8/foo-2.8.tgz" "2.8"
             -> ("http://example.com/" version "/foo-" version ".tgz")))))

(test-skip (if (and (%nixpkgs-directory)
                    (file-exists? (string-append (%nixpkgs-directory)
                                                 "/default.nix")))
               0
               1))

(test-assert "nixpkgs->guix-package"
  (match (nixpkgs->guix-package (%nixpkgs-directory) "guile")
    (('package
       ('name "guile")
       ('version (? string?))
       ('source ('origin _ ...))
       ('build-system _)
       ('inputs ('quasiquote (inputs ...)))
       ('propagated-inputs ('quasiquote (pinputs ...)))
       ('home-page (? string?))
       ('synopsis (? string?))
       ('description (? string?))
       ('license (? symbol?)))
     (and (member '("libffi" ,libffi) inputs)
          (member '("gmp" ,gmp) pinputs)
          #t))
    (x
     (pk 'fail x #f))))

(test-end "snix")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
