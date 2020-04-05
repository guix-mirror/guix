;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system hurd)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages hurd)
  #:use-module (gnu system vm)
  #:export (cross-hurd-image))

;;; Commentary:
;;;
;;; This module provides tools to (cross-)build GNU/Hurd virtual machine
;;; images.
;;;
;;; Code:

(define* (cross-hurd-image #:key (hurd hurd) (gnumach gnumach))
  "Return a cross-built GNU/Hurd image."
  (define hurd-os
    (let-syntax ((for-hurd (syntax-rules ()
                             ((_ things ...)
                              (list (with-parameters ((%current-target-system
                                                       "i586-pc-gnu"))
                                      things) ...)))))
      (directory-union "gnu+hurd"
                       (cons (with-parameters ((%current-system "i686-linux"))
                               gnumach)
                             (for-hurd hurd coreutils grep sed)))))

  (define grub.cfg
    (let ((hurd (with-parameters ((%current-target-system "i586-pc-gnu"))
                  hurd))
          (mach (with-parameters ((%current-system "i686-linux"))
                  gnumach))
          (libc (cross-libc "i586-pc-gnu")))
      (computed-file "grub.cfg"
                     #~(call-with-output-file #$output
                         (lambda (port)
                           (format port "
set timeout=2
search.file ~a/boot/gnumach

menuentry \"GNU\" {
  multiboot ~a/boot/gnumach root=device:hd0s1
  module ~a/hurd/ext2fs.static ext2fs \\
    --multiboot-command-line='${kernel-command-line}' \\
    --host-priv-port='${host-port}' \\
    --device-master-port='${device-port}' \\
    --exec-server-task='${exec-task}' -T typed '${root}' \\
    '$(task-create)' '$(task-resume)'
  module ~a/lib/ld.so.1 exec ~a/hurd/exec '$(exec-task=task-create)'
}\n"
                                   #+mach #+mach #+hurd
                                   #+libc #+hurd))))))

  (define fstab
    (plain-file "fstab"
"# This file was generated from your Guix configuration.  Any changes
# will be lost upon reboot or reconfiguration.

/dev/hd0s1	/	ext2	defaults
"))

  (define passwd
    (plain-file "passwd"
"root:x:0:0:root:/root:/bin/sh
"
))

  (define shadow
    (plain-file "shadow"
"root::0:0:0:0:::
"
))

  (define hurd-directives
    `((directory "/servers")
      ,@(map (lambda (server)
               `(file ,(string-append "/servers/" server)))
             '("startup" "exec" "proc" "password"
               "default-pager" "crash-dump-core"
               "kill" "suspend"))
      ("/servers/crash" -> "crash-dump-core")
      (directory "/servers/socket")
      (file "/servers/socket/1")
      (file "/servers/socket/2")
      (file "/servers/socket/16")
      ("/servers/socket/local" -> "1")
      ("/servers/socket/inet" -> "2")
      ("/servers/socket/inet6" -> "16")
      (file "/etc/resolv.conf"
            "nameserver 10.0.2.3\n")
      (directory "/boot")
      ("/boot/grub.cfg" -> ,grub.cfg)  ;XXX: not strictly needed
      ("/hurd" -> ,(file-append (with-parameters ((%current-target-system
                                                   "i586-pc-gnu"))
                                  hurd)
                                "/hurd"))
      ("/etc/fstab" -> ,fstab)
      ("/etc/passwd" -> ,passwd)
      ("/etc/shadow" -> ,shadow)
      ;; XXX can we instead, harmlessly set _PATH_TTYS (from glibc) in runttys.c?
      ("/etc/ttys" -> ,(file-append (with-parameters ((%current-target-system
                                                   "i586-pc-gnu"))
                                  hurd)
                                "/etc/ttys"))))

  (qemu-image #:file-system-type "ext2"
              #:file-system-options '("-o" "hurd")
              #:device-nodes 'hurd
              #:inputs `(("system" ,hurd-os)
                         ("grub.cfg" ,grub.cfg)
                         ("fstab" ,fstab)
                         ("passwd" ,passwd)
                         ("shadow" ,shadow))
              #:copy-inputs? #t
              #:os hurd-os
              #:bootcfg-drv grub.cfg
              #:bootloader grub-bootloader
              #:register-closures? #f
              #:extra-directives hurd-directives))

;; Return this thunk so one can type "guix build -f gnu/system/hurd.scm".
cross-hurd-image
