;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2018, 2020 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix cpio) #:prefix cpio:)
  #:use-module (guix build utils)
  #:use-module (guix build store-copy)
  #:use-module (system base compile)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:select (sizeof))
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
                             (gzip "gzip"))
  "Write a cpio archive containing DIRECTORY to file OUTPUT, with reset
timestamps in the archive.  When COMPRESS? is true, compress it using GZIP.
On success, return OUTPUT."

  ;; Note: as per `ramfs-rootfs-initramfs.txt', always add directory entries
  ;; before the files that are inside of it: "The Linux kernel cpio
  ;; extractor won't create files in a directory that doesn't exist, so the
  ;; directory entries must go before the files that go in those
  ;; directories."

  (define files
    ;; Use 'sort' so that (1) the order of files is deterministic, and (2)
    ;; directories appear before the files they contain.
    (sort (file-system-fold (const #t)                 ;enter?
                            (lambda (file stat result) ;leaf
                              (cons file result))
                            (lambda (dir stat result)  ;down
                              (if (string=? dir directory)
                                  result
                                  (cons dir result)))
                            (lambda (file stat result)
                              result)
                            (const #f)                 ;skip
                            (const #f)                 ;error
                            '()
                            directory)
          string<?))

  (call-with-output-file output
    (lambda (port)
      (cpio:write-cpio-archive files port
                               #:file->header cpio:file->cpio-header*)))

  (if compress?
      ;; Gzip insists on adding a '.gz' suffix and does nothing if the input
      ;; file already has that suffix.  Shuffle files around to placate it.
      (let* ((gz-suffix? (string-suffix? ".gz" output))
             (sans-gz    (if gz-suffix?
                             (string-drop-right output 3)
                             output)))
        (when gz-suffix?
          (rename-file output sans-gz))
        ;; Use '--no-name' so that gzip records neither a file name nor a time
        ;; stamp in its output.
        (and (zero? (system* gzip "--best" "--no-name" sans-gz))
             (begin
               (unless gz-suffix?
                 (rename-file (string-append output ".gz") output))
               output)))
      output))

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
                       (references-graphs '())
                       (gzip "gzip"))
  "Write an initial RAM disk (initrd) to OUTPUT.  The initrd starts the script
at INIT, running GUILE.  It contains all the items referred to by
REFERENCES-GRAPHS."
  (mkdir "contents")

  ;; Copy the closures of all the items referenced in REFERENCES-GRAPHS.
  (populate-store references-graphs "contents"
                  #:deduplicate? #f)

  (with-directory-excursion "contents"
    ;; Make '/init'.
    (symlink init "init")

    ;; Compile it.
    (compile-to-cache "init")

    ;; Allow Guile to find out where it is (XXX).  See
    ;; 'guile-relocatable.patch'.
    (mkdir-p "proc/self")
    (symlink (string-append guile "/bin/guile") "proc/self/exe")
    (readlink "proc/self/exe")

    (write-cpio-archive output "." #:gzip gzip))

  ;; Make sure directories are writable so we can delete files.
  (for-each make-file-writable
            (find-files "contents"
                        (lambda (file stat)
                          (eq? 'directory (stat:type stat)))
                        #:directories? #t))
  (delete-file-recursively "contents"))

;;; linux-initrd.scm ends here
