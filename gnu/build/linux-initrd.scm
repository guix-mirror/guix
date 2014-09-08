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
  #:use-module (guix build utils)
  #:use-module (guix build store-copy)
  #:use-module (system base compile)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:select (sizeof))
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ftw)
  #:export (write-cpio-archive
            build-initrd))

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

  ;; Note: don't use '--no-absolute-filenames' since that strips leading
  ;; slashes from symlink targets.
  (let ((pipe (open-pipe* OPEN_WRITE cpio "-o" "-O" output
                          "-H" "newc" "--null")))
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

(define (cache-compiled-file-name file)
  "Return the file name of the in-cache .go file for FILE, relative to the
current directory.

This is similar to what 'compiled-file-name' in (system base compile) does."
  (let loop ((file file))
    (let ((target (false-if-exception (readlink file))))
     (if target
         (loop target)
         (format #f ".cache/guile/ccache/~a-~a-~a-~a/~a"
                 (effective-version)
                 (if (eq? (native-endianness) (endianness little))
                     "LE"
                     "BE")
                 (sizeof '*)
                 (effective-version)
                 file)))))

(define (compile-to-cache file)
  "Compile FILE to the cache."
  (let ((compiled-file (cache-compiled-file-name file)))
    (mkdir-p (dirname compiled-file))
    (compile-file file
                  #:opts %auto-compilation-options
                  #:output-file compiled-file)))

(define* (build-initrd output
                       #:key
                       guile init
                       linux-module-directory
                       (references-graphs '())
                       (cpio "cpio")
                       (gzip "gzip"))
  "Write an initial RAM disk (initrd) to OUTPUT.  The initrd starts the script
at INIT, running GUILE.  It contains all the items referred to by
REFERENCES-GRAPHS, plus the Linux modules from LINUX-MODULE-DIRECTORY."
  (mkdir "contents")

  ;; Copy the closures of all the items referenced in REFERENCES-GRAPHS.
  (populate-store references-graphs "contents")

  (with-directory-excursion "contents"
    ;; Copy Linux modules.
    (mkdir "modules")
    (copy-recursively linux-module-directory "modules")

    ;; Make '/init'.
    (symlink init "init")

    ;; Compile it.
    (compile-to-cache "init")

    ;; Allow Guile to find out where it is (XXX).  See
    ;; 'guile-relocatable.patch'.
    (mkdir-p "proc/self")
    (symlink (string-append guile "/bin/guile") "proc/self/exe")
    (readlink "proc/self/exe")

    ;; Reset the timestamps of all the files that will make it in the initrd.
    (for-each (lambda (file)
                (unless (eq? 'symlink (stat:type (lstat file)))
                  (utime file 0 0 0 0)))
              (find-files "." ".*"))

    (write-cpio-archive output "."
                        #:cpio cpio #:gzip gzip))

  (delete-file-recursively "contents"))

;;; linux-initrd.scm ends here
