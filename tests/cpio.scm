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

(define-module (test-cpio)
  #:use-module (guix cpio)
  #:use-module (guix tests)
  #:use-module ((guix build utils) #:select (which))
  #:use-module ((guix utils) #:select (call-with-temporary-output-file))
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

(define %cpio-program
  (which "cpio"))


(test-begin "cpio")

(test-assert "file->cpio-header + write-cpio-header + read-cpio-header"
  (let* ((file   (search-path %load-path "guix.scm"))
         (header (file->cpio-header file)))
    (call-with-values
        (lambda ()
          (open-bytevector-output-port))
      (lambda (port get-bv)
        (write-cpio-header header port)
        (let ((port (open-bytevector-input-port (get-bv))))
          (equal? header (read-cpio-header port)))))))

(unless %cpio-program (test-skip 1))
(test-assert "bit-identical to GNU cpio's output"
  (call-with-temporary-output-file
   (lambda (link _)
     (delete-file link)
     (symlink "chbouib" link)

     (let ((files (cons* "/"
                         (canonicalize-path
                          (dirname (search-path %load-path "guix.scm")))
                         link
                         (map (compose canonicalize-path
                                       (cut search-path %load-path <>))
                              '("guix.scm" "guix/build/syscalls.scm"
                                "guix/packages.scm")))))
       (call-with-temporary-output-file
        (lambda (ref-file _)
          (let ((pipe (open-pipe* OPEN_WRITE %cpio-program "-o" "-O" ref-file
                                  "-H" "newc" "--null")))
            (for-each (lambda (file)
                        (format pipe "~a\0" file))
                      files)
            (and (zero? (close-pipe pipe))
                 (call-with-temporary-output-file
                  (lambda (file port)
                    (write-cpio-archive files port)
                    (close-port port)
                    (or (file=? ref-file file)
                        (throw 'cpio-archives-differ files
                               ref-file file
                               (stat:size (stat ref-file))
                               (stat:size (stat file))))))))))))))

(test-end "cpio")
