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

(define-module (test-sets)
  #:use-module (guix sets)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))


(test-begin "sets")

(test-assert "set-contains?"
  (let* ((lst (iota 123))
         (set (list->set lst)))
    (and (every (cut set-contains? set <>)
                lst)
         (not (set-contains? set -1)))))

(test-assert "set->list"
  (let* ((lst (iota 123))
         (set (list->set lst)))
    (lset= = lst (set->list set))))

(test-assert "set-union"
  (let* ((a  (list 'a))
         (b  (list 'b))
         (s1 (setq a))
         (s2 (setq b))
         (s3 (set-union s1 s2)))
    (and (set-contains? s3 a)
         (set-contains? s3 b))))

(test-end)
