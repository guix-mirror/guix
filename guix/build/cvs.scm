;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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

(define-module (guix build cvs)
  #:use-module (guix build utils)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:export (cvs-fetch))

;;; Commentary:
;;;
;;; This is the build-side support code of (guix cvs-download).  It allows a
;;; CVS repository to be checked out at a specific revision or date.
;;;
;;; Code:

(define (find-cvs-directories)
  (define (enter? path st result)
    (not (string-suffix? "/CVS" path)))
  (define (leaf path st result) result)
  (define (down path st result) result)
  (define (up   path st result) result)
  (define (skip path st result)
    (if (and (string-suffix? "/CVS" path)
             (eqv? 'directory (stat:type st)))
        (cons path result)
        result))
  (define (error path st errno result)
    (format (current-error-port) "cvs-fetch: ~a: ~a~%"
            path (strerror errno)))
  (sort (file-system-fold enter? leaf down up skip error '() "." lstat)
        string<?))

(define* (cvs-fetch cvs-root-directory module revision directory
                    #:key (cvs-command "cvs"))
  "Fetch REVISION from MODULE of CVS-ROOT-DIRECTORY into DIRECTORY.  REVISION
must either be a date in ISO-8601 format (e.g. \"2012-12-21\") or a CVS tag.
Return #t on success, #f otherwise."
  (and (zero? (system* cvs-command "-z3"
                       "-d" cvs-root-directory
                       "checkout"
                       (if (string-match "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" revision)
                           "-D" "-r")
                       revision
                       module))
       (rename-file module directory)
       (with-directory-excursion directory
         (for-each delete-file-recursively (find-cvs-directories)))
       #t))

;;; cvs.scm ends here
