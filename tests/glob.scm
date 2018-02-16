;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-glob)
  #:use-module (guix glob)
  #:use-module (srfi srfi-64))


(test-begin "glob")

(test-equal "compile-glob-pattern, no wildcards"
  "foo"
  (compile-glob-pattern "foo"))

(test-equal "compile-glob-pattern, Kleene star"
  '("foo" * "bar")
  (compile-glob-pattern "foo*bar"))

(test-equal "compile-glob-pattern, question mark"
  '(? "foo" *)
  (compile-glob-pattern "?foo*"))

(test-assert "literal match"
  (let ((pattern (compile-glob-pattern "foo")))
    (and (glob-match? pattern "foo")
         (not (glob-match? pattern "foobar"))
         (not (glob-match? pattern "barfoo")))))

(test-assert "trailing star"
  (let ((pattern (compile-glob-pattern "foo*")))
    (and (glob-match? pattern "foo")
         (glob-match? pattern "foobar")
         (not (glob-match? pattern "xfoo")))))

(test-assert "question marks"
  (let ((pattern (compile-glob-pattern "foo??bar")))
    (and (glob-match? pattern "fooxxbar")
         (glob-match? pattern "fooZZbar")
         (not (glob-match? pattern "foobar"))
         (not (glob-match? pattern "fooxxxbar"))
         (not (glob-match? pattern "fooxxbarzz")))))

(test-end "glob")
