;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system install)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages texinfo)
  #:export (installation-os))

;;; Commentary:
;;;
;;; This module provides an 'operating-system' definition for use on images
;;; for USB sticks etc., for the installation of the GNU system.
;;;
;;; Code:

(define (log-to-info)
  "Return a script that spawns the Info reader on the right section of the
manual."
  (gexp->script "log-to-info"
                #~(execl (string-append #$texinfo-4 "/bin/info")
                         "-d" "/run/current-system/profile/share/info"
                         "-f" (string-append #$guix "/share/info/guix.info")
                         "-n" "System Configuration")))

(define (installation-services)
  "Return the list services for the installation image."
  (let ((motd (text-file "motd" "
Welcome to the installation of the GNU operating system!

There is NO WARRANTY, to the extent permitted by law.  In particular, you may
LOSE ALL YOUR DATA as a side effect of the installation process.  Furthermore,
it is alpha software, so it may BREAK IN UNEXPECTED WAYS.

You have been warned.  Thanks for being so brave.
")))
    (define (normal-tty tty)
      (mingetty-service tty
                        #:motd motd
                        #:auto-login "root"
                        #:login-pause? #t))

    (list (mingetty-service "tty1"
                            #:motd motd
                            #:auto-login "root")

          ;; Documentation.
          (mingetty-service "tty2"
                            #:motd motd
                            #:auto-login "guest"
                            #:login-program (log-to-info))

          ;; A bunch of 'root' ttys.
          (normal-tty "tty3")
          (normal-tty "tty4")
          (normal-tty "tty5")
          (normal-tty "tty6")

          ;; The usual services.
          (syslog-service)
          (guix-service)
          (nscd-service))))

(define %issue
  ;; Greeting.
  "
This is an installation image of the GNU system.  Welcome.

Use Alt-F2 for documentation.
")

(define installation-os
  ;; The operating system used on installation images for USB sticks etc.
  (operating-system
    (host-name "gnu")
    (timezone "Europe/Paris")
    (locale "en_US.UTF-8")
    (bootloader (grub-configuration
                 (device "/dev/sda")))
    (file-systems
     ;; Note: the disk image build code overrides this root file system with
     ;; the appropriate one.
     (list (file-system
             (mount-point "/")
             (device "gnu-disk-image")
             (type "ext4"))))

    (users (list (user-account
                  (name "guest")
                  (group "wheel")
                  (password "")
                  (comment "Guest of GNU")
                  (home-directory "/home/guest"))))
    (groups (list (user-group (name "root") (id 0))
                  (user-group
                   (name "wheel")
                   (id 1)
                   (members '("guest")))          ; allow 'guest' to use sudo
                  (user-group
                   (name "users")
                   (id 100)
                   (members '("guest")))))

    (issue %issue)

    (services (installation-services))

    ;; We don't need setuid programs so pass the empty list so we don't pull
    ;; additional programs here.
    (setuid-programs '())

    (pam-services
     ;; Explicitly allow for empty passwords.
     (base-pam-services #:allow-empty-passwords? #t))

    (packages (cons* texinfo-4                ; for the standalone Info reader
                     parted fdisk ddrescue
                     %base-packages))))

;; Return it here so 'guix system' can consume it directly.
installation-os

;;; install.scm ends here
