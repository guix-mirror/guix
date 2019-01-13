;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2015, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build make-bootstrap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (guix build utils)
  #:export (make-stripped-libc))

;; Commentary:
;;
;; This module provides facilities to build the bootstrap binaries.
;;
;; Code:

(define (make-stripped-libc output libc kernel-headers)
  "Copy to OUTPUT the subset of LIBC and KERNEL-HEADERS that is needed
when producing a bootstrap libc."

  (define (copy-mach-headers output kernel-headers)
    (let* ((incdir (string-append output "/include")))
      (copy-recursively (string-append libc "/include") incdir)

      (copy-recursively (string-append kernel-headers "/include/mach")
                        (string-append incdir "/mach"))
      #t))
  
  (define (copy-linux-headers output kernel-headers)
    (let* ((incdir (string-append output "/include")))
      (copy-recursively (string-append libc "/include") incdir)

      ;; Copy some of the Linux-Libre headers that glibc headers
      ;; refer to.
      (mkdir (string-append incdir "/linux"))
      (for-each (lambda (file)
                  (install-file (string-append kernel-headers "/include/linux/" file)
                                (string-append incdir "/linux")))
                '("limits.h" "errno.h" "socket.h" "kernel.h"
                  "sysctl.h" "param.h" "ioctl.h" "types.h"
                  "posix_types.h" "stddef.h" "falloc.h"))

      (copy-recursively (string-append kernel-headers "/include/asm")
                        (string-append incdir "/asm"))
      (copy-recursively (string-append kernel-headers "/include/asm-generic")
                        (string-append incdir "/asm-generic"))
      #t))

  (define %libc-object-files-rx "^(crt.*|ld.*|lib(c|m|dl|rt|pthread|nsl|\
util).*\\.so(\\..*)?|lib(machuser|hurduser).so.*|(libc(rt|)|libpthread)\
_nonshared\\.a)$")

  (setvbuf (current-output-port) 'line)
  (let* ((libdir (string-append output "/lib")))
    (mkdir-p libdir)
    (for-each (lambda (file)
                (let ((target (string-append libdir "/"
                                             (basename file))))
                  (copy-file file target)
                  (remove-store-references target)))
              (find-files (string-append libc "/lib") %libc-object-files-rx))
    #t)

    (if (directory-exists? (string-append kernel-headers "/include/mach"))
        (copy-mach-headers output kernel-headers)
        (copy-linux-headers output kernel-headers)))


