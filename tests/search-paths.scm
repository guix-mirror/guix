;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-search-paths)
  #:use-module (guix search-paths)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(define %top-srcdir
  (dirname (search-path %load-path "guix.scm")))


(test-begin "search-paths")

(test-equal "evaluate-search-paths, separator is #f"
  (string-append %top-srcdir
                 "/gnu/packages/aux-files/linux-libre")

  ;; The following search path spec should evaluate to a single item: the
  ;; first directory that matches the "-linux$" pattern in
  ;; gnu/packages/bootstrap.
  (let ((spec (search-path-specification
               (variable "CHBOUIB")
               (files '("gnu/packages/aux-files"))
               (file-type 'directory)
               (separator #f)
               (file-pattern "^linux"))))
    (match (evaluate-search-paths (list spec)
                                  (list %top-srcdir))
      (((spec* . value))
       (and (eq? spec* spec) value)))))

(test-end "search-paths")
