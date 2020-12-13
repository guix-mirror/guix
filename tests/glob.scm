;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-syntax test-string->sglob
  (syntax-rules (=>)
    ((_ pattern => result rest ...)
     (begin
       (test-equal (format #f "string->sglob, ~s" pattern)
         result
         (string->sglob pattern))
       (test-string->sglob rest ...)))
    ((_)
     #t)))

(define-syntax test-glob-match
  (syntax-rules (matches and not)
    ((_ (pattern-string matches strings ... (and not others ...)) rest ...)
     (begin
       (test-assert (format #f "glob-match? ~s" pattern-string)
         (let ((pattern (string->compiled-sglob pattern-string)))
           (and (glob-match? pattern strings) ...
                (not (glob-match? pattern others)) ...)))
       (test-glob-match rest ...)))
    ((_)
     #t)))

(test-string->sglob
 "foo" => "foo"
 "?foo*" => '(? "foo" *)
 "foo[1-5]" => '("foo" (range #\1 #\5))
 "foo[abc]bar" => '("foo" (set #\a #\b #\c) "bar")
 "foo[a[b]c]bar" => '("foo" (set #\a #\[ #\b #\] #\c) "bar")
 "[123]x" => '((set #\1 #\2 #\3) "x")
 "[a-z]" => '((range #\a #\z))
 "**/*.scm" => '(**/ * ".scm"))

(test-glob-match
 ("foo" matches "foo" (and not "foobar" "barfoo"))
 ("foo*" matches "foo" "foobar" (and not "xfoo"))
 ("foo??bar" matches "fooxxbar" "fooZZbar"
  (and not "foobar" "fooxxxbar" "fooxxbarzz"))
 ("foo?" matches "foox" (and not "fooxx"))
 ("ab[0-9]c" matches "ab0c" "ab7c" "ab9c"
  (and not "ab-c" "ab00c" "ab3"))
 ("ab[cdefg]" matches "abc" "abd" "abg"
  (and not "abh" "abcd" "ab["))
 ("foo/**/*.scm" matches "foo/bar/baz.scm" "foo/bar.scm" "foo/bar/baz/zab.scm"
  (and not "foo/bar/baz.java" "foo/bar.smc")))

(test-end "glob")
