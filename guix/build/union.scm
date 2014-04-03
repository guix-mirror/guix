;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (guix build union)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:export (union-build))

;;; Commentary:
;;;
;;; Build a directory that is the union of a set of directories, using
;;; symbolic links.
;;;
;;; Code:

(define (files-in-directory dirname)
  (let ((dir (opendir dirname)))
    (let loop ((files '()))
      (match (readdir dir)
        ((or "." "..")
         (loop files))
        ((? eof-object?)
         (closedir dir)
         (sort files string<?))
        (file
         (loop (cons file files)))))))

(define (file-is-directory? file)
  (eq? 'directory (stat:type (stat file))))

(define (file=? file1 file2)
  "Return #t if FILE1 and FILE2 are regular files and their contents are
identical, #f otherwise."
  (let ((st1 (stat file1))
        (st2 (stat file2)))
    (and (eq? (stat:type st1) 'regular)
         (eq? (stat:type st2) 'regular)
         (= (stat:size st1) (stat:size st2))
         (call-with-input-file file1
           (lambda (port1)
             (call-with-input-file file2
               (lambda (port2)
                 (define len 8192)
                 (define buf1 (make-bytevector len))
                 (define buf2 (make-bytevector len))
                 (let loop ()
                   (let ((n1 (get-bytevector-n! port1 buf1 0 len))
                         (n2 (get-bytevector-n! port2 buf2 0 len)))
                     (and (equal? n1 n2)
                          (or (eof-object? n1)
                              (loop))))))))))))

(define* (union-build output inputs
                      #:key (log-port (current-error-port)))
  "Build in the OUTPUT directory a symlink tree that is the union of all
the INPUTS."

  (define (symlink* input output)
    (format log-port "`~a' ~~> `~a'~%" input output)
    (symlink input output))

  (define (resolve-collisions output dirs files)
    (cond ((null? dirs)
           ;; The inputs are all files.
           (format (current-error-port)
                   "warning: collision encountered: ~{~a ~}~%"
                   files)

           (let ((file (first files)))
             ;; TODO: Implement smarter strategies.
             (format (current-error-port)
                     "warning: arbitrarily choosing ~a~%"
                     file)

             (symlink* file output)))

          (else
           ;; The inputs are a mixture of files and directories
           (error "union-build: collision between file and directories"
                  `((files ,files) (dirs ,dirs))))))

  (define (union output inputs)
    (match inputs
      ((input)
       ;; There's only one input, so just make a link.
       (symlink* input output))
      (_
       (call-with-values (lambda () (partition file-is-directory? inputs))
         (match-lambda*
           ((dirs ())
            ;; All inputs are directories.
            (union-of-directories output dirs))

           ((() (file (? (cut file=? <> file)) ...))
            ;; There are no directories, and all files have the same contents,
            ;; so there's no conflict.
            (symlink* file output))

           ((dirs files)
            (resolve-collisions output dirs files)))))))

  (define (union-of-directories output dirs)
    ;; Create a new directory where we will merge the input directories.
    (mkdir output)

    ;; Build a hash table mapping each file to a list of input
    ;; directories containing that file.
    (let ((table (make-hash-table)))

      (define (add-to-table! file dir)
        (hash-set! table file (cons dir (hash-ref table file '()))))

      ;; Populate the table.
      (for-each (lambda (dir)
                  (for-each (cut add-to-table! <> dir)
                            (files-in-directory dir)))
                dirs)

      ;; Now iterate over the table and recursively
      ;; perform a union for each entry.
      (hash-for-each (lambda (file dirs-with-file)
                       (union (string-append output "/" file)
                              (map (cut string-append <> "/" file)
                                   (reverse dirs-with-file))))
                     table)))

  (setvbuf (current-output-port) _IOLBF)
  (setvbuf (current-error-port) _IOLBF)
  (when (file-port? log-port)
    (setvbuf log-port _IOLBF))

  (union-of-directories output (delete-duplicates inputs)))

;;; union.scm ends here
