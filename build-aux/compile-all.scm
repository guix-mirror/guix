;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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

(use-modules (ice-9 match)
             (ice-9 threads)
             (guix build compile)
             (guix build utils))

(define host (getenv "host"))
(define srcdir (getenv "srcdir"))

(define (relative-file file)
  (if (string-prefix? (string-append srcdir "/") file)
      (string-drop file (+ 1 (string-length srcdir)))
      file))

(define (file-mtime<? f1 f2)
  (< (stat:mtime (stat f1))
     (stat:mtime (stat f2))))

(define (scm->go file)
  (let* ((relative (relative-file file))
         (without-extension (string-drop-right relative 4)))
    (string-append without-extension ".go")))

(define (file-needs-compilation? file)
  (let ((go (scm->go file)))
    (or (not (file-exists? go))
        (file-mtime<? go file))))

;; Install a SIGINT handler to give unwind handlers in 'compile-file' an
;; opportunity to run upon SIGINT and to remove temporary output files.
(sigaction SIGINT
  (lambda args
    (exit 1)))

(match (command-line)
  ((_ . files)
   (compile-files srcdir (getcwd)
                  (filter file-needs-compilation? files)
                  #:host host
                  #:report-load (lambda (file total completed)
                                  (when file
                                    (format #t "  LOAD     ~a~%" file)))
                  #:report-compilation (lambda (file total completed)
                                         (when file
                                           (format #t "  GUILEC   ~a~%"
                                                   (scm->go file)))))))
