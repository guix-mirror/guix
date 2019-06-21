;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2015, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
  #:export (copy-linux-headers
            make-stripped-libc))

;; Commentary:
;;
;; This module provides facilities to build the bootstrap binaries.
;;
;; Code:

(define (copy-linux-headers output kernel-headers)
  "Copy to OUTPUT the subset of KERNEL-HEADERS that is needed when producing a
bootstrap libc."

  (let* ((incdir (string-append output "/include")))
    (mkdir-p incdir)

    ;; Copy some of the Linux-Libre headers that glibc headers
    ;; refer to.
    (mkdir (string-append incdir "/linux"))
    (for-each (lambda (file)
                (install-file (pk 'src (string-append kernel-headers "/include/linux/" file))
                              (pk 'dest (string-append incdir "/linux"))))
              '(
                "atalk.h"               ; for 2.2.5
                "errno.h"
                "falloc.h"
                "if_addr.h"             ; for 2.16.0
                "if_ether.h"            ; for 2.2.5
                "if_link.h"             ; for 2.16.0
                "ioctl.h"
                "kernel.h"
                "limits.h"
                "neighbour.h"           ; for 2.16.0
                "netlink.h"             ; for 2.16.0
                "param.h"
                "prctl.h"               ; for 2.16.0
                "posix_types.h"
                "rtnetlink.h"           ; for 2.16.0
                "socket.h"
                "stddef.h"
                "swab.h"                ; for 2.2.5
                "sysctl.h"
                "sysinfo.h"             ; for 2.2.5
                "types.h"
                "version.h"             ; for 2.2.5
                ))

    (copy-recursively (string-append kernel-headers "/include/asm")
                      (string-append incdir "/asm"))
    (copy-recursively (string-append kernel-headers "/include/asm-generic")
                      (string-append incdir "/asm-generic"))
    (copy-recursively (string-append kernel-headers "/include/linux/byteorder")
                      (string-append incdir "/linux/byteorder"))
    #t))

(define (make-stripped-libc output libc kernel-headers)
  "Copy to OUTPUT the subset of LIBC and KERNEL-HEADERS that is needed
when producing a bootstrap libc."

  (define (copy-mach-headers output kernel-headers)
    (let* ((incdir (string-append output "/include")))
      (copy-recursively (string-append libc "/include") incdir)

      (copy-recursively (string-append kernel-headers "/include/mach")
                        (string-append incdir "/mach"))
      #t))
  
  (define (copy-libc+linux-headers output kernel-headers)
    (let* ((incdir (string-append output "/include")))
      (copy-recursively (string-append libc "/include") incdir)
      (copy-linux-headers output kernel-headers)))

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
        (copy-libc+linux-headers output kernel-headers)))


