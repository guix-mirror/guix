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

(define-module (guix build rpath)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (%patchelf
            file-rpath
            augment-rpath))

;;; Commentary:
;;;
;;; Tools to manipulate the RPATH and RUNPATH of ELF binaries.  Currently they
;;; rely on PatchELF.
;;;
;;; Code:

(define %patchelf
  ;; The `patchelf' command.
  (make-parameter "patchelf"))

(define %not-colon
  (char-set-complement (char-set #\:)))

(define (file-rpath file)
  "Return the RPATH (or RUNPATH) of FILE as a list of directory names, or #f
on failure."
  (let* ((p (open-pipe* OPEN_READ (%patchelf) "--print-rpath" file))
         (l (read-line p)))
    (and (zero? (close-pipe p))
         (string-tokenize l %not-colon))))

(define (augment-rpath file dir)
  "Add DIR to the front of the RPATH and RUNPATH of FILE.  Return the new
RPATH as a list, or #f on failure."
  (let* ((rpath  (or (file-rpath file) '()))
         (rpath* (cons dir rpath)))
    (format #t "~a: changing RPATH from ~s to ~s~%"
            file rpath rpath*)
    (and (zero? (system* (%patchelf) "--set-rpath"
                         (string-join rpath* ":") file))
         rpath*)))

;;; rpath.scm ends here
