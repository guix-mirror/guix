;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix serialization)
  #:use-module (guix utils)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:export (write-int read-int
            write-long-long read-long-long
            write-padding
            write-string read-string read-latin1-string
            write-string-list read-string-list
            write-string-pairs
            write-store-path read-store-path
            write-store-path-list read-store-path-list

            &nar-error
            nar-error?
            nar-error-port
            nar-error-file

            &nar-read-error
            nar-read-error?
            nar-read-error-token

            write-file
            restore-file))

;;; Comment:
;;;
;;; Serialization procedures used by the RPCs and the Nar format.  This module
;;; is for internal consumption.
;;;
;;; Code:

;; Similar to serialize.cc in Nix.

(define (write-int n p)
  (let ((b (make-bytevector 8 0)))
    (bytevector-u32-set! b 0 n (endianness little))
    (put-bytevector p b)))

(define (read-int p)
  (let ((b (get-bytevector-n p 8)))
    (bytevector-u32-ref b 0 (endianness little))))

(define (write-long-long n p)
  (let ((b (make-bytevector 8 0)))
    (bytevector-u64-set! b 0 n (endianness little))
    (put-bytevector p b)))

(define (read-long-long p)
  (let ((b (get-bytevector-n p 8)))
    (bytevector-u64-ref b 0 (endianness little))))

(define write-padding
  (let ((zero (make-bytevector 8 0)))
    (lambda (n p)
      (let ((m (modulo n 8)))
        (or (zero? m)
            (put-bytevector p zero 0 (- 8 m)))))))

(define (write-string s p)
  (let* ((s (string->utf8 s))
         (l (bytevector-length s))
         (m (modulo l 8))
         (b (make-bytevector (+ 8 l (if (zero? m) 0 (- 8 m))))))
    (bytevector-u32-set! b 0 l (endianness little))
    (bytevector-copy! s 0 b 8 l)
    (put-bytevector p b)))

(define (read-string p)
  (let* ((len (read-int p))
         (m   (modulo len 8))
         (bv  (get-bytevector-n p len))
         (str (utf8->string bv)))
    (or (zero? m)
        (get-bytevector-n p (- 8 m)))
    str))

(define (read-latin1-string p)
  (let* ((len (read-int p))
         (m   (modulo len 8))
         ;; Note: do not use 'get-string-n' to work around Guile bug
         ;; <http://bugs.gnu.org/19621>.  See <http://bugs.gnu.org/19610> for
         ;; a discussion.
         (str (get-bytevector-n p len)))
    (or (zero? m)
        (get-bytevector-n p (- 8 m)))

    ;; XXX: Rewrite using (ice-9 iconv) when the minimum requirement is
    ;; upgraded to Guile >= 2.0.9.
    (list->string (map integer->char (bytevector->u8-list str)))))

(define (write-string-list l p)
  (write-int (length l) p)
  (for-each (cut write-string <> p) l))

(define (write-string-pairs l p)
  (write-int (length l) p)
  (for-each (match-lambda
             ((first . second)
              (write-string first p)
              (write-string second p)))
            l))

(define (read-string-list p)
  (let ((len (read-int p)))
    (unfold (cut >= <> len)
            (lambda (i)
              (read-string p))
            1+
            0)))

(define (write-store-path f p)
  (write-string f p))                             ; TODO: assert path

(define (read-store-path p)
  (read-string p))                                ; TODO: assert path

(define write-store-path-list write-string-list)
(define read-store-path-list read-string-list)


(define-condition-type &nar-error &error      ; XXX: inherit from &nix-error ?
  nar-error?
  (file  nar-error-file)                       ; file we were restoring, or #f
  (port  nar-error-port))                      ; port from which we read

(define-condition-type &nar-read-error &nar-error
  nar-read-error?
  (token nar-read-error-token))                 ; faulty token, or #f


(define (dump in out size)
  "Copy SIZE bytes from IN to OUT."
  (define buf-size 65536)
  (define buf (make-bytevector buf-size))

  (let loop ((left size))
    (if (<= left 0)
        0
        (let ((read (get-bytevector-n! in buf 0 (min left buf-size))))
          (if (eof-object? read)
              left
              (begin
                (put-bytevector out buf 0 read)
                (loop (- left read))))))))

(define (write-contents file p size)
  "Write SIZE bytes from FILE to output port P."
  (define (call-with-binary-input-file file proc)
    ;; Open FILE as a binary file.  This avoids scan-for-encoding, and thus
    ;; avoids any initial buffering.  Disable file name canonicalization to
    ;; avoid stat'ing like crazy.
    (with-fluids ((%file-port-name-canonicalization #f))
      (let ((port (open-file file "rb")))
        (dynamic-wind
          (const #t)
          (cut proc port)
          (lambda ()
            (close-port port))))))

  (write-string "contents" p)
  (write-long-long size p)
  (call-with-binary-input-file file
    ;; Use `sendfile' when available (Guile 2.0.8+).
    (if (and (compile-time-value (defined? 'sendfile))
             (file-port? p))
        (cut sendfile p <> size 0)
        (cut dump <> p size)))
  (write-padding size p))

(define (read-contents in out)
  "Read the contents of a file from the Nar at IN, write it to OUT, and return
the size in bytes."
  (define executable?
    (match (read-string in)
      ("contents"
       #f)
      ("executable"
       (match (list (read-string in) (read-string in))
         (("" "contents") #t)
         (x (raise
             (condition (&message
                         (message "unexpected executable file marker"))
                        (&nar-read-error (port in)
                                         (file #f)
                                         (token x))))))
       #t)
      (x
       (raise
        (condition (&message (message "unsupported nar file type"))
                   (&nar-read-error (port in) (file #f) (token x)))))))

  (let ((size (read-long-long in)))
    ;; Note: `sendfile' cannot be used here because of port buffering on IN.
    (dump in out size)

    (when executable?
      (chmod out #o755))
    (let ((m (modulo size 8)))
      (unless (zero? m)
        (get-bytevector-n in (- 8 m))))
    size))

(define %archive-version-1
  ;; Magic cookie for Nix archives.
  "nix-archive-1")

(define (write-file file port)
  "Write the contents of FILE to PORT in Nar format, recursing into
sub-directories of FILE as needed."
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
         (let ((entries
                ;; NOTE: Guile 2.0.5's 'scandir' returns all subdirectories
                ;; unconditionally, including "." and "..", regardless of the
                ;; 'select?' predicate passed to it, so we have to filter
                ;; those out externally.
                (filter (negate (cut member <> '("." "..")))
                        ;; 'scandir' defaults to 'string-locale<?' to sort
                        ;; files, but this happens to be case-insensitive (at
                        ;; least in 'en_US' locale on libc 2.18.)  Conversely,
                        ;; we want files to be sorted in a case-sensitive
                        ;; fashion.
                        (scandir f (const #t) string<?))))
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
        ((symlink)
         (write-string "type" p)
         (write-string "symlink" p)
         (write-string "target" p)
         (write-string (readlink f) p))
        (else
         (raise (condition (&message (message "unsupported file type"))
                           (&nar-error (file f) (port port))))))
      (write-string ")" p))))

(define (restore-file port file)
  "Read a file (possibly a directory structure) in Nar format from PORT.
Restore it as FILE."
  (let ((signature (read-string port)))
    (unless (equal? signature %archive-version-1)
      (raise
       (condition (&message (message "invalid nar signature"))
                  (&nar-read-error (port port)
                                   (token signature)
                                   (file #f))))))

  (let restore ((file file))
    (define (read-eof-marker)
      (match (read-string port)
        (")" #t)
        (x (raise
            (condition
             (&message (message "invalid nar end-of-file marker"))
             (&nar-read-error (port port) (file file) (token x)))))))

    (match (list (read-string port) (read-string port) (read-string port))
      (("(" "type" "regular")
       (call-with-output-file file (cut read-contents port <>))
       (read-eof-marker))
      (("(" "type" "symlink")
       (match (list (read-string port) (read-string port))
         (("target" target)
          (symlink target file)
          (read-eof-marker))
         (x (raise
             (condition
              (&message (message "invalid symlink tokens"))
              (&nar-read-error (port port) (file file) (token x)))))))
      (("(" "type" "directory")
       (let ((dir file))
         (mkdir dir)
         (let loop ((prefix (read-string port)))
           (match prefix
             ("entry"
              (match (list (read-string port)
                           (read-string port) (read-string port)
                           (read-string port))
                (("(" "name" file "node")
                 (restore (string-append dir "/" file))
                 (match (read-string port)
                   (")" #t)
                   (x
                    (raise
                     (condition
                      (&message
                       (message "unexpected directory entry termination"))
                      (&nar-read-error (port port)
                                       (file file)
                                       (token x))))))
                 (loop (read-string port)))))
             (")" #t)                             ; done with DIR
             (x
              (raise
               (condition
                (&message (message "unexpected directory inter-entry marker"))
                (&nar-read-error (port port) (file file) (token x)))))))))
      (x
       (raise
        (condition
         (&message (message "unsupported nar entry type"))
         (&nar-read-error (port port) (file file) (token x))))))))

;;; serialization.scm ends here
