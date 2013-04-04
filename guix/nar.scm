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

(define-module (guix nar)
  #:use-module (guix utils)
  #:use-module (guix serialization)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 ftw)
  #:export (write-file))

;;; Comment:
;;;
;;; Read and write Nix archives, aka. ‘nar’.
;;;
;;; Code:

(define (write-contents file p size)
  "Write SIZE bytes from FILE to output port P."
  (define (call-with-binary-input-file file proc)
    ;; Open FILE as a binary file.  This avoids scan-for-encoding, and thus
    ;; avoids any initial buffering.  Disable file name canonicalization to
    ;; avoid stat'ing like crazy.
    (with-fluids ((%file-port-name-canonicalization #f))
      (let ((port (open-file file "rb")))
        (catch #t (cut proc port)
          (lambda args
            (close-port port)
            (apply throw args))))))

  (define (dump in size)
    (define buf-size 65536)
    (define buf (make-bytevector buf-size))

    (let loop ((left size))
      (if (<= left 0)
          0
          (let ((read (get-bytevector-n! in buf 0 buf-size)))
            (if (eof-object? read)
                left
                (begin
                  (put-bytevector p buf 0 read)
                  (loop (- left read))))))))

  (write-string "contents" p)
  (write-long-long size p)
  (call-with-binary-input-file file
    ;; Use `sendfile' when available (Guile 2.0.8+).
    (if (compile-time-value (defined? 'sendfile))
        (cut sendfile p <> size 0)
        (cut dump <> size)))
  (write-padding size p))

(define (write-file file port)
  "Write the contents of FILE to PORT in Nar format, recursing into
sub-directories of FILE as needed."
  (define %archive-version-1 "nix-archive-1")
  (define p port)

  (write-string %archive-version-1 p)

  (let dump ((f file))
    (let ((s (lstat f)))
      (write-string "(" p)
      (case (stat:type s)
        ((regular)
         (write-string "type" p)
         (write-string "regular" p)
         (if (not (zero? (logand (stat:mode s) #o100)))
             (begin
               (write-string "executable" p)
               (write-string "" p)))
         (write-contents f p (stat:size s)))
        ((directory)
         (write-string "type" p)
         (write-string "directory" p)
         (let ((entries (remove (cut member <> '("." ".."))
                                (scandir f))))
           (for-each (lambda (e)
                       (let ((f (string-append f "/" e)))
                         (write-string "entry" p)
                         (write-string "(" p)
                         (write-string "name" p)
                         (write-string e p)
                         (write-string "node" p)
                         (dump f)
                         (write-string ")" p)))
                     entries)))
        (else
         (error "ENOSYS")))
      (write-string ")" p))))

;;; nar.scm ends here
