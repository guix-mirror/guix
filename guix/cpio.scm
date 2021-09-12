;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (guix cpio)
  #:use-module ((guix build syscalls) #:select (device-number
                                                device-number->major+minor))
  #:use-module ((guix build utils) #:select (dump-port))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:export (cpio-header?
            make-cpio-header
            file->cpio-header
            file->cpio-header*
            special-file->cpio-header*
            write-cpio-header
            read-cpio-header

            write-cpio-archive))

;;; Commentary:
;;;
;;; This module implements the cpio "new ASCII" format, bit-for-bit identical
;;; to GNU cpio with the '-H newc' option.
;;;
;;; Code:

;; Values for 'mode', OR'd together.

(define C_IRUSR #o000400)
(define C_IWUSR #o000200)
(define C_IXUSR #o000100)
(define C_IRGRP #o000040)
(define C_IWGRP #o000020)
(define C_IXGRP #o000010)
(define C_IROTH #o000004)
(define C_IWOTH #o000002)
(define C_IXOTH #o000001)

(define C_ISUID #o004000)
(define C_ISGID #o002000)
(define C_ISVTX #o001000)

(define C_FMT   #o170000)                         ;bit mask
(define C_ISBLK #o060000)
(define C_ISCHR #o020000)
(define C_ISDIR #o040000)
(define C_ISFIFO #o010000)
(define C_ISSOCK #o0140000)
(define C_ISLNK #o0120000)
(define C_ISCTG #o0110000)
(define C_ISREG #o0100000)


(define MAGIC
  ;; The "new" portable format with ASCII header, as produced by GNU cpio with
  ;; '-H newc'.
  (string->number "070701" 16))

(define (read-header-field size port)
  (string->number (get-string-n port size) 16))

(define (write-header-field value size port)
  (put-bytevector port
                  (string->utf8
                   (string-pad (string-upcase (number->string value 16))
                               size #\0))))

(define-syntax define-pack
  (syntax-rules ()
    ((_ type ctor pred write read (field-names field-sizes field-getters) ...)
     (begin
       (define-record-type type
         (ctor field-names ...)
         pred
         (field-names field-getters) ...)

       (define (read port)
         (set-port-encoding! port "ISO-8859-1")
         (ctor (read-header-field field-sizes port)
               ...))

       (define (write obj port)
         (let* ((size (+ field-sizes ...)))
           (match obj
             (($ type field-names ...)
              (write-header-field field-names field-sizes port)
              ...))))))))

;; cpio header in "new ASCII" format, without checksum.
(define-pack <cpio-header>
  %make-cpio-header cpio-header?
  write-cpio-header read-cpio-header
  (magic     6  cpio-header-magic)
  (ino       8  cpio-header-inode)
  (mode      8  cpio-header-mode)
  (uid       8  cpio-header-uid)
  (gid       8  cpio-header-gid)
  (nlink     8  cpio-header-nlink)
  (mtime     8  cpio-header-mtime)
  (file-size 8  cpio-header-file-size)
  (dev-maj   8  cpio-header-device-major)
  (dev-min   8  cpio-header-device-minor)
  (rdev-maj  8  cpio-header-rdevice-major)
  (rdev-min  8  cpio-header-rdevice-minor)
  (name-size 8  cpio-header-name-size)
  (checksum  8  cpio-header-checksum))            ;0 for "newc" format

(define* (make-cpio-header #:key
                           (inode 0)
                           (mode (logior C_ISREG C_IRUSR))
                           (uid 0) (gid 0)
                           (nlink 1) (mtime 0) (size 0)
                           (dev 0) (rdev 0) (name-size 0))
  "Return a new cpio file header."
  (let-values (((major minor)   (device-number->major+minor dev))
               ((rmajor rminor) (device-number->major+minor rdev)))
    (%make-cpio-header MAGIC
                       inode mode uid gid
                       nlink mtime
                       (if (or (= C_ISLNK (logand mode C_FMT))
                               (= C_ISREG (logand mode C_FMT)))
                           size
                           0)
                       major minor rmajor rminor
                       (+ name-size 1)              ;include trailing zero
                       0)))                          ;checksum

(define (mode->type mode)
  "Given the number MODE, return a symbol representing the kind of file MODE
denotes, similar to 'stat:type'."
  (let ((fmt (logand mode C_FMT)))
    (cond ((= C_ISREG fmt) 'regular)
          ((= C_ISDIR fmt) 'directory)
          ((= C_ISLNK fmt) 'symlink)
          ((= C_ISBLK fmt) 'block-special)
          ((= C_ISCHR fmt) 'char-special)
          (else
           (error "unsupported file type" mode)))))

(define* (file->cpio-header file #:optional (file-name file)
                            #:key (stat lstat))
  "Return a cpio header corresponding to the info returned by STAT for FILE,
using FILE-NAME as its file name."
  (let ((st (stat file)))
    (make-cpio-header #:inode (stat:ino st)
                      #:mode (stat:mode st)
                      #:uid (stat:uid st)
                      #:gid (stat:gid st)
                      #:nlink (stat:nlink st)
                      #:mtime (stat:mtime st)
                      #:size (stat:size st)
                      #:dev (stat:dev st)
                      #:rdev (stat:rdev st)
                      #:name-size (string-length file-name))))

(define* (file->cpio-header* file
                             #:optional (file-name file)
                             #:key (stat lstat))
  "Similar to 'file->cpio-header', but return a header with a zeroed
modification time, inode number, UID/GID, etc.  This allows archives to be
produced in a deterministic fashion."
  (let ((st (stat file)))
    (make-cpio-header #:mode (stat:mode st)
                      #:nlink (stat:nlink st)
                      #:size (stat:size st)
                      #:name-size (string-length file-name))))

(define* (special-file->cpio-header* file
                                     device-type
                                     device-major
                                     device-minor
                                     permission-bits
                                     #:optional (file-name file))
  "Create a character or block device header.

DEVICE-TYPE is either 'char-special or 'block-special.

The number of hard links is assumed to be 1."
  (make-cpio-header #:mode (logior (match device-type
                                    ('block-special C_ISBLK)
                                    ('char-special C_ISCHR))
                                    permission-bits)
                    #:nlink 1
                    #:rdev (device-number device-major device-minor)
                    #:name-size (string-length file-name)))

(define %trailer
  "TRAILER!!!")

(define %last-header
  ;; The header that marks the end of the archive.
  (make-cpio-header #:mode 0
                    #:name-size (string-length %trailer)))

(define* (write-cpio-archive files port
                             #:key (file->header file->cpio-header))
  "Write to PORT a cpio archive in \"new ASCII\" format containing all of FILES.

The archive written to PORT is intended to be bit-identical to what GNU cpio
produces with the '-H newc' option."
  (define (write-padding offset port)
    (let ((padding (modulo (- 4 (modulo offset 4)) 4)))
      (put-bytevector port (make-bytevector padding))))

  (define (pad-block port)
    ;; Write padding to PORT such that we finish with a 512-byte block.
    ;; XXX: We rely on PORT's internal state, assuming it's a file port.
    (let* ((offset  (seek port 0 SEEK_CUR))
           (padding (modulo (- 512 (modulo offset 512)) 512)))
      (put-bytevector port (make-bytevector padding))))

  (define (dump-file file)
    (let* ((header (file->header file))
           (size   (cpio-header-file-size header)))
      (write-cpio-header header port)
      (put-bytevector port (string->utf8 file))
      (put-u8 port 0)

      ;; We're padding the header + following file name + trailing zero, and
      ;; the header is 110 byte long.
      (write-padding (+ 110 1 (string-length file)) port)

      (case (mode->type (cpio-header-mode header))
        ((regular)
         (call-with-input-file file
           (lambda (input)
             (dump-port input port))))
        ((symlink)
         (let ((target (readlink file)))
           (put-string port target)))
        ((directory)
         #t)
        ((block-special)
         #t)
        ((char-special)
         #t)
        (else
         (error "file type not supported")))

      ;; Pad the file content.
      (write-padding size port)))

  (set-port-encoding! port "ISO-8859-1")

  (for-each dump-file files)

  (write-cpio-header %last-header port)
  (put-bytevector port (string->utf8 %trailer))
  (write-padding (string-length %trailer) port)

  ;; Pad so the last block is 512-byte long.
  (pad-block port))

;;; cpio.scm ends here
