;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
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

(define-module (test-eggs)
  #:use-module (guix import egg)
  #:use-module (guix gexp)
  #:use-module (guix base32)
  #:use-module (gcrypt hash)
  #:use-module (guix tests)
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module ((guix build utils) #:select (delete-file-recursively mkdir-p which))
  #:use-module ((guix utils) #:select (call-with-temporary-output-file))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (web uri)
  #:use-module (ice-9 match))

(define test-egg-1
  '((synopsis "Example egg")
    (license "GPL-3/MIT")
    (version "1.0.0")
    (test-dependencies test srfi-1)
    (foreign-dependencies libgit2)
    (build-dependencies begin-syntax)
    (dependencies datatype)
    (author "John Doe")))

(define test-egg-2
  '((synopsis "Example egg")
    (license "GPL-3+")
    (version "0.3")
    (test-dependencies test)
    (foreign-dependencies libgit2)
    (build-dependencies begin-syntax)
    (dependencies datatype)
    (author "Alice Bobson")))

(define test-egg-1-file "/tmp/guix-egg-1")
(define test-egg-2-file "/tmp/guix-egg-2")

(test-begin "egg")

(test-equal "guix-package->egg-name"
  "bar"
  (guix-package->egg-name
   (dummy-package "dummy"
                  (name "chicken-bar"))))

;; Copied from tests/hackage.scm
(define-syntax-rule (define-package-matcher name pattern)
  (define* (name obj)
    (match obj
      (pattern #t)
      (x       (pk 'fail x #f)))))

(define (eval-test-with-egg-file egg-name egg-test egg-file matcher)
  (call-with-output-file egg-file
    (lambda (port)
      (write egg-test port)))
  (matcher (egg->guix-package egg-name #f
                              #:file egg-file
                              #:source (plain-file
                                        (string-append egg-name "-egg")
                                        "content"))))

(define-package-matcher match-chicken-foo
  ('package
    ('name "chicken-foo")
    ('version "1.0.0")
    ('source (? file-like? source))
    ('build-system 'chicken-build-system)
    ('arguments ('quasiquote ('#:egg-name "foo")))
    ('native-inputs
     ('list 'chicken-test 'chicken-srfi-1 'chicken-begin-syntax))
    ('inputs ('list 'libgit2))
    ('propagated-inputs ('list 'chicken-datatype))
    ('home-page "https://wiki.call-cc.org/egg/foo")
    ('synopsis "Example egg")
    ('description #f)
    ('license '(list license:gpl3 license:expat))))

(define-package-matcher match-chicken-bar
  ('package
    ('name "chicken-bar")
    ('version "0.3")
    ('source (? file-like? source))
    ('build-system 'chicken-build-system)
    ('arguments ('quasiquote ('#:egg-name "bar")))
    ('native-inputs ('list 'chicken-test 'chicken-begin-syntax))
    ('inputs ('list 'libgit2))
    ('propagated-inputs ('list 'chicken-datatype))
    ('home-page "https://wiki.call-cc.org/egg/bar")
    ('synopsis "Example egg")
    ('description #f)
    ('license 'license:gpl3+)))

(test-assert "egg->guix-package local file, multiple licenses"
  (eval-test-with-egg-file "foo" test-egg-1 test-egg-1-file match-chicken-foo))

(test-assert "egg->guix-package local file, single license"
  (eval-test-with-egg-file "bar" test-egg-2 test-egg-2-file match-chicken-bar))

(test-end "egg")
