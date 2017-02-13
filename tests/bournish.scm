;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-bournish)
  #:use-module (guix build bournish)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (srfi srfi-64))


(test-begin "bournish")

(test-equal "single statement"
  '(chdir "/foo")
  (read-and-compile (open-input-string "cd /foo")
                    #:from %bournish-language #:to 'scheme))

(test-equal "multiple statements"
  '(begin
     (chdir "/foo")
     (getcwd)
     ((@@ (guix build bournish) ls-command-implementation)))
  (read-and-compile (open-input-string "cd /foo\npwd\nls")
                    #:from %bournish-language #:to 'scheme))

(test-equal "rm"
  '(for-each delete-file (list "foo" "bar"))
  (read-and-compile (open-input-string "rm foo bar\n")
                    #:from %bournish-language #:to 'scheme))

(test-equal "rm -r"
  '(for-each (@ (guix build utils) delete-file-recursively)
             (list "/foo" "/bar"))
  (read-and-compile (open-input-string "rm -r /foo /bar\n")
                    #:from %bournish-language #:to 'scheme))

(test-end "bournish")

