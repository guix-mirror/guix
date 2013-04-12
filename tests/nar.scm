;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-nar)
  #:use-module (guix nar)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match))

;; Test the (guix nar) module.


;;;
;;; File system testing tools, initially contributed to Guile, then libchop.
;;;

(define (random-file-size)
  (define %average (* 1024 512))                  ; 512 KiB
  (define %stddev  (* 1024 64))                   ; 64 KiB
  (inexact->exact
   (max 0 (round (+ %average (* %stddev (random:normal)))))))

(define (make-file-tree dir tree)
  "Make file system TREE at DIR."
  (let loop ((dir  dir)
             (tree tree))
    (define (scope file)
      (string-append dir "/" file))

    (match tree
      (('directory name (body ...))
       (mkdir (scope name))
       (for-each (cute loop (scope name) <>) body))
      (('directory name (? integer? mode) (body ...))
       (mkdir (scope name))
       (for-each (cute loop (scope name) <>) body)
       (chmod (scope name) mode))
      ((file)
       (populate-file (scope file) (random-file-size)))
      ((file (? integer? mode))
       (populate-file (scope file) (random-file-size))
       (chmod (scope file) mode))
      ((from '-> to)
       (symlink to (scope from))))))

(define (delete-file-tree dir tree)
  "Delete file TREE from DIR."
  (let loop ((dir  dir)
             (tree tree))
    (define (scope file)
      (string-append dir "/" file))

    (match tree
      (('directory name (body ...))
       (for-each (cute loop (scope name) <>) body)
       (rmdir (scope name)))
      (('directory name (? integer? mode) (body ...))
       (chmod (scope name) #o755)          ; make sure it can be entered
       (for-each (cute loop (scope name) <>) body)
       (rmdir (scope name)))
      ((from '-> _)
       (delete-file (scope from)))
      ((file _ ...)
       (delete-file (scope file))))))

(define-syntax-rule (with-file-tree dir tree body ...)
  (dynamic-wind
    (lambda ()
      (make-file-tree dir 'tree))
    (lambda ()
      body ...)
    (lambda ()
      (delete-file-tree dir 'tree))))

(define (file-tree-equal? input output)
  "Return #t if the file trees at INPUT and OUTPUT are equal."
  (define strip
    (cute string-drop <> (string-length input)))
  (define sibling
    (compose (cut string-append output <>) strip))
  (define (file=? a b)
    (and (eq? (stat:type (lstat a)) (stat:type (lstat b)))
         (case (stat:type (lstat a))
           ((regular)
            (equal?
             (call-with-input-file a get-bytevector-all)
             (call-with-input-file b get-bytevector-all)))
           ((symlink)
            (string=? (readlink a) (readlink b)))
           (else
            (error "what?" (lstat a))))))

  (file-system-fold (const #t)
                    (lambda (name stat result)    ; leaf
                      (and result
                           (file=? name (sibling name))))
                    (lambda (name stat result)    ; down
                      result)
                    (lambda (name stat result)    ; up
                      result)
                    (const #f)                    ; skip
                    (lambda (name stat errno result)
                      (pk 'error name stat errno)
                      #f)
                    (> (stat:nlink (stat output)) 2)
                    input
                    lstat))

(define (make-random-bytevector n)
  (let ((bv (make-bytevector n)))
    (let loop ((i 0))
      (if (< i n)
          (begin
            (bytevector-u8-set! bv i (random 256))
            (loop (1+ i)))
          bv))))

(define (populate-file file size)
  (call-with-output-file file
    (lambda (p)
      (put-bytevector p (make-random-bytevector size)))))

(define (rm-rf dir)
  (file-system-fold (const #t)                    ; enter?
                    (lambda (file stat result)    ; leaf
                      (delete-file file))
                    (const #t)                    ; down
                    (lambda (dir stat result)     ; up
                      (rmdir dir))
                    (const #t)                    ; skip
                    (const #t)                    ; error
                    #t
                    dir
                    lstat))

(define %test-dir
  ;; An output directory under $top_builddir.
  (string-append (dirname (search-path %load-path "configure"))
                 "/test-nar-" (number->string (getpid))))


(test-begin "nar")

(test-assert "write-file + restore-file"
  (let* ((input  (string-append (dirname (search-path %load-path "guix.scm"))
                                "/guix"))
         (output %test-dir)
         (nar    (string-append output ".nar")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (call-with-output-file nar
          (cut write-file input <>))
        (call-with-input-file nar
          (cut restore-file <> output))
        (file-tree-equal? input output))
      (lambda ()
        (false-if-exception (delete-file nar))
        (false-if-exception (rm-rf output))))))

(test-assert "write-file + restore-file with symlinks"
  (let ((input (string-append %test-dir ".input")))
    (mkdir input)
    (dynamic-wind
      (const #t)
      (lambda ()
        (with-file-tree input
            (directory "root"
                       (("reg") ("exe" #o777) ("sym" -> "reg")))
          (let* ((output %test-dir)
                 (nar    (string-append output ".nar")))
            (dynamic-wind
              (lambda () #t)
              (lambda ()
                (call-with-output-file nar
                  (cut write-file input <>))
                (call-with-input-file nar
                  (cut restore-file <> output))
                (file-tree-equal? input output))
              (lambda ()
                (false-if-exception (delete-file nar))
                (false-if-exception (rm-rf output)))))))
      (lambda ()
        (rmdir input)))))

(test-end "nar")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'with-file-tree 'scheme-indent-function 2)
;;; End:
