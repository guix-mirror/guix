;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu build linux-initrd)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ftw)
  #:export (write-cpio-archive))

;;; Commentary:
;;;
;;; Tools to create Linux initial RAM disks ("initrds").  Initrds are
;;; essentially gzipped cpio archives, with a '/init' executable that the
;;; kernel runs at boot time.
;;;
;;; Code:

(define* (write-cpio-archive output directory
                             #:key
                             (compress? #t)
                             (cpio "cpio") (gzip "gzip"))
  "Write a cpio archive containing DIRECTORY to file OUTPUT, using CPIO.  When
COMPRESS? is true, compress it using GZIP.  On success, return OUTPUT."
  (let ((pipe (open-pipe* OPEN_WRITE cpio "-o" "-O" output
                          "-H" "newc" "--null"
                          "--no-absolute-filenames")))
    (define (print0 file)
      (format pipe "~a\0" file))

    ;; Note: as per `ramfs-rootfs-initramfs.txt', always add directory entries
    ;; before the files that are inside of it: "The Linux kernel cpio
    ;; extractor won't create files in a directory that doesn't exist, so the
    ;; directory entries must go before the files that go in those
    ;; directories."

    ;; XXX: Use a deterministic order.
    (file-system-fold (const #t)
                      (lambda (file stat result)   ; leaf
                        (print0 file))
                      (lambda (dir stat result)    ; down
                        (unless (string=? dir directory)
                          (print0 dir)))
                      (const #f)                   ; up
                      (const #f)                   ; skip
                      (const #f)
                      #f
                      directory)

    (and (zero? (close-pipe pipe))
         (or (not compress?)
             (and (zero? (system* gzip "--best" output))
                  (rename-file (string-append output ".gz")
                               output))
             output))))

;;; linux-initrd.scm ends here
