;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (guix profiles)
  #:use-module (guix utils)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages file)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages less)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services hurd)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:export (cross-hurd-image
            %base-packages/hurd
            %base-services/hurd
            %hurd-default-operating-system
            %hurd-default-operating-system-kernel))

;;; Commentary:
;;;
;;; This module provides tools to (cross-)build GNU/Hurd virtual machine
;;; images.
;;;
;;; Code:

(define %hurd-default-operating-system-kernel
  (if (hurd-system?)
      gnumach
      ;; A cross-built GNUmach does not work
      (with-parameters ((%current-system "i686-linux")
                        (%current-target-system #f))
        gnumach)))

(define %base-packages/hurd
  (list hurd bash coreutils file findutils grep sed
        guile-3.0 guile-colorized guile-readline
        net-base inetutils less shepherd which))

(define %base-services/hurd
  (list (service hurd-console-service-type
                 (hurd-console-configuration (hurd hurd)))
        (service hurd-getty-service-type (hurd-getty-configuration
                                          (tty "tty1")))
        (service hurd-getty-service-type (hurd-getty-configuration
                                          (tty "tty2")))
        (service static-networking-service-type
                 (list (static-networking (interface "lo")
                                          (ip "127.0.0.1")
                                          (requirement '())
                                          (provision '(loopback))
                                          (name-servers '("10.0.2.3")))))
        (syslog-service)
        (service guix-service-type
                 (guix-configuration
                  (extra-options '("--disable-chroot"
                                   "--disable-deduplication"))))))

(define %hurd-default-operating-system
  (operating-system
    (kernel %hurd-default-operating-system-kernel)
    (kernel-arguments '())
    (hurd hurd)
    (bootloader (bootloader-configuration
                 (bootloader grub-minimal-bootloader)
                 (target "/dev/vda")))
    (initrd (lambda _ '()))
    (initrd-modules (lambda _ '()))
    (firmware '())
    (host-name "guixygnu")
    (file-systems '())
    (packages %base-packages/hurd)
    (timezone "GNUrope")
    (name-service-switch #f)
    (essential-services (hurd-default-essential-services this-operating-system))
    (pam-services '())
    (setuid-programs '())
    (sudoers-file #f)))

(define* (cross-hurd-image #:key (hurd hurd) (gnumach gnumach))
  "Return a cross-built GNU/Hurd image."

  (define (cross-built thing)
    (with-parameters ((%current-target-system "i586-pc-gnu"))
      thing))

  (define (cross-built-entry entry)
    (manifest-entry
      (inherit entry)
      (item (cross-built (manifest-entry-item entry)))
      (dependencies (map cross-built-entry
                         (manifest-entry-dependencies entry)))))

  (define system-profile
    (profile
     (content
      (map-manifest-entries cross-built-entry
                            (packages->manifest %base-packages/hurd)))))

  (define grub.cfg
    (let ((hurd (cross-built hurd))
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
guixbuilder:x:1:1:guixbuilder:/var/empty:/bin/no-sh
"))

  (define group
    (plain-file "group"
                "guixbuild:x:1:guixbuilder
"))

  (define shadow
    (plain-file "shadow"
                "root::0:0:0:0:::
"))

  (define etc-profile
    (plain-file "profile"
                "\
export PS1='\\u@\\h\\$ '

GUIX_PROFILE=\"/run/current-system/profile\"
. \"$GUIX_PROFILE/etc/profile\"

GUIX_PROFILE=\"$HOME/.guix-profile\"
if [ -f \"$GUIX_PROFILE/etc/profile\" ]; then
  . \"$GUIX_PROFILE/etc/profile\"
fi\n"))

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
      (directory "/boot")
      ("/boot/grub.cfg" -> ,grub.cfg)   ;XXX: not strictly needed
      ("/hurd" -> ,(file-append (with-parameters ((%current-target-system
                                                   "i586-pc-gnu"))
                                  hurd)
                                "/hurd"))

      ;; TODO: Create those during activation, eventually.
      (directory "/root")
      (file "/root/.guile"
            ,(object->string
              '(begin
                 (use-modules (ice-9 readline) (ice-9 colorized))
                 (activate-readline) (activate-colorized))))
      (directory "/run")
      (directory "/run/current-system")
      ("/run/current-system/profile" -> ,system-profile)
      ("/etc/profile" -> ,etc-profile)
      ("/etc/fstab" -> ,fstab)
      ("/etc/group" -> ,group)
      ("/etc/passwd" -> ,passwd)
      ("/etc/shadow" -> ,shadow)
      (file "/etc/hostname" "guixygnu")
      (file "/etc/resolv.conf"
            "nameserver 10.0.2.3\n")
      ("/etc/services" -> ,(file-append (with-parameters ((%current-target-system
                                                           "i586-pc-gnu"))
                                          net-base)
                                        "/etc/services"))
      ("/etc/protocols" -> ,(file-append (with-parameters ((%current-target-system
                                                            "i586-pc-gnu"))
                                           net-base)
                                         "/etc/protocols"))
      ("/etc/motd" -> ,(file-append (with-parameters ((%current-target-system
                                                       "i586-pc-gnu"))
                                      hurd)
                                    "/etc/motd"))
      ("/etc/login" -> ,(file-append (with-parameters ((%current-target-system
                                                        "i586-pc-gnu"))
                                       hurd)
                                     "/etc/login"))


      ;; XXX can we instead, harmlessly set _PATH_TTYS (from glibc) in runttys.c?
      ("/etc/ttys" -> ,(file-append (with-parameters ((%current-target-system
                                                       "i586-pc-gnu"))
                                      hurd)
                                    "/etc/ttys"))
      ("/bin/sh" -> ,(file-append (with-parameters ((%current-target-system
                                                     "i586-pc-gnu"))
                                    bash)
                                  "/bin/sh"))))

  (qemu-image #:file-system-type "ext2"
              #:file-system-options '("-o" "hurd")
              #:device-nodes 'hurd
              #:inputs `(("system" ,system-profile)
                         ("grub.cfg" ,grub.cfg)
                         ("fstab" ,fstab)
                         ("passwd" ,passwd)
                         ("group" ,group)
                         ("etc-profile" ,etc-profile)
                         ("shadow" ,shadow))
              #:copy-inputs? #t
              #:os system-profile
              #:bootcfg-drv grub.cfg
              #:bootloader grub-bootloader
              #:register-closures? #f
              #:extra-directives hurd-directives))

;; Return this thunk so one can type "guix build -f gnu/system/hurd.scm".
cross-hurd-image
