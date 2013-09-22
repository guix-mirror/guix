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
  #:use-module ((guix build utils) #:select (with-directory-excursion))
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (nar-error?
            nar-read-error?
            nar-read-error-file
            nar-read-error-port
            nar-read-error-token

            write-file
            restore-file))

;;; Comment:
;;;
;;; Read and write Nix archives, aka. ‘nar’.
;;;
;;; Code:

(define-condition-type &nar-error &error      ; XXX: inherit from &nix-error ?
  nar-error?)

(define-condition-type &nar-read-error &nar-error
  nar-read-error?
  (port  nar-read-error-port)                   ; port from which we read
  (file  nar-read-error-file)                   ; file we were restoring, or #f
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
    (if (compile-time-value (defined? 'sendfile))
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
        ((symlink)
         (write-string "type" p)
         (write-string "symlink" p)
         (write-string "target" p)
         (write-string (readlink f) p))
        (else
         (raise (condition (&message (message "ENOSYS"))
                           (&nar-error)))))
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

;;; nar.scm ends here
