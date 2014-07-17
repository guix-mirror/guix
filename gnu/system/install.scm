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
                #~(execl (string-append #$texinfo-4 "/bin/info") "info"
                         "-d" "/run/current-system/profile/share/info"
                         "-f" (string-append #$guix "/share/info/guix.info")
                         "-n" "System Installation")))

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

          ;; Documentation.  The manual is in UTF-8, but
          ;; 'console-font-service' sets up Unicode support and loads a font
          ;; with all the useful glyphs like em dash and quotation marks.
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

          ;; The build daemon.  Register the hydra.gnu.org key as trusted.
          ;; This allows the installation process to use substitutes by
          ;; default.
          (guix-service #:authorize-hydra-key? #t)

          ;; Start udev so that useful device nodes are available.
          (udev-service)

          ;; Install Unicode support and a suitable font.
          (console-font-service "tty1")
          (console-font-service "tty2")
          (console-font-service "tty3")
          (console-font-service "tty4")
          (console-font-service "tty5")
          (console-font-service "tty6")

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
                  (group "users")
                  (supplementary-groups '("wheel"))  ; allow use of sudo
                  (password "")
                  (comment "Guest of GNU")
                  (home-directory "/home/guest"))))

    (issue %issue)

    (services (installation-services))

    ;; We don't need setuid programs so pass the empty list so we don't pull
    ;; additional programs here.
    (setuid-programs '())

    (pam-services
     ;; Explicitly allow for empty passwords.
     (base-pam-services #:allow-empty-passwords? #t))

    (packages (cons* texinfo-4                ; for the standalone Info reader
                     parted ddrescue
                     ;; XXX: We used to have GNU fdisk here, but as of version
                     ;; 2.0.0a, that pulls Guile 1.8, which takes unreasonable
                     ;; space; furthermore util-linux's fdisk is already
                     ;; available here, so we keep that.
                     %base-packages))))

;; Return it here so 'guix system' can consume it directly.
installation-os

;;; install.scm ends here
