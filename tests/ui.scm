;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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


(define-module (test-ui)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;; Test the (guix ui) module.

(define %paragraph
  "GNU Guile is an implementation of the Scheme programming language, with
support for many SRFIs, packaged for use in a wide variety of environments.
In addition to implementing the R5RS Scheme standard and a large subset of
R6RS, Guile includes a module system, full access to POSIX system calls,
networking support, multiple threads, dynamic linking, a foreign function call
interface, and powerful string processing.")


(test-begin "ui")

(test-assert "fill-paragraph"
  (every (lambda (column)
           (every (lambda (width)
                    (every (lambda (line)
                             (<= (string-length line) width))
                           (string-split (fill-paragraph %paragraph
                                                         width column)
                                         #\newline)))
                  '(15 30 35 40 45 50 60 70 80 90 100)))
   '(0 5)))

(test-assert "fill-paragraph, consecutive newlines"
  (every (lambda (width)
           (any (lambda (line)
                  (string-prefix? "When STR" line))
                (string-split
                 (fill-paragraph (procedure-documentation fill-paragraph)
                                 width)
                 #\newline)))
         '(15 20 25 30 40 50 60)))

(test-equal "fill-paragraph, large unbreakable word"
  '("Here is a" "very-very-long-word"
    "and that's" "it.")
  (string-split
   (fill-paragraph "Here is a very-very-long-word and that's it."
                   10)
   #\newline))

(test-end "ui")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
