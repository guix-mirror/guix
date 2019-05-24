;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu system linux-container)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix config)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (gnu build linux-container)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:export (system-container
            containerized-operating-system
            container-script))

(define* (container-essential-services os #:key shared-network?)
  "Return a list of essential services corresponding to OS, a
non-containerized OS.  This procedure essentially strips essential services
from OS that are needed on the bare metal and not in a container."
  (define base
    (remove (lambda (service)
              (memq (service-kind service)
                    (list (service-kind %linux-bare-metal-service)
                          firmware-service-type
                          system-service-type)))
            (operating-system-default-essential-services os)))

  (cons (service system-service-type
                 (let ((locale (operating-system-locale-directory os)))
                   (with-monad %store-monad
                     (return `(("locale" ,locale))))))
        ;; If network is to be shared with the host, remove network
        ;; configuration files from etc-service.
        (if shared-network?
            (modify-services base
              (etc-service-type
               files => (remove
                         (match-lambda
                           ((filename _)
                            (member filename
                                    (map basename %network-configuration-files))))
                         files)))
            base)))

(define* (containerized-operating-system os mappings
                                         #:key
                                         shared-network?
                                         (extra-file-systems '()))
  "Return an operating system based on OS for use in a Linux container
environment.  MAPPINGS is a list of <file-system-mapping> to realize in the
containerized OS.  EXTRA-FILE-SYSTEMS is a list of file systems to add to OS."
  (define user-file-systems
    (remove (lambda (fs)
              (let ((target (file-system-mount-point fs))
                    (source (file-system-device fs)))
                (or (string=? target (%store-prefix))
                    (string=? target "/")
                    (and (string? source)
                         (string-prefix? "/dev/" source))
                    (string-prefix? "/dev/" target)
                    (string-prefix? "/sys/" target))))
            (operating-system-file-systems os)))

  (define (mapping->fs fs)
    (file-system (inherit (file-system-mapping->bind-mount fs))
      (needed-for-boot? #t)))

  (define useless-services
    ;; Services that make no sense in a container.  Those that attempt to
    ;; access /dev/tty[0-9] in particular cannot work in a container.
    (append (list console-font-service-type
                  mingetty-service-type
                  agetty-service-type)
            ;; Remove nscd service if network is shared with the host.
            (if shared-network?
                (list nscd-service-type)
                (list))))

  (operating-system
    (inherit os)
    (swap-devices '()) ; disable swap
    (essential-services (container-essential-services
                         this-operating-system
                         #:shared-network? shared-network?))
    (services (remove (lambda (service)
                        (memq (service-kind service)
                              useless-services))
                      (operating-system-user-services os)))
    (file-systems (append (map mapping->fs mappings)
                          extra-file-systems
                          user-file-systems

                          ;; Provide a dummy root file system so we can create
                          ;; a 'boot-parameters' file.
                          (list (file-system
                                  (mount-point "/")
                                  (device "nothing")
                                  (type "dummy")))))))

(define* (container-script os #:key (mappings '()) shared-network?)
  "Return a derivation of a script that runs OS as a Linux container.
MAPPINGS is a list of <file-system> objects that specify the files/directories
that will be shared with the host system."
  (define network-mappings
    ;; Files to map if network is to be shared with the host
    (append %network-file-mappings
            (let ((nscd-run-directory "/var/run/nscd"))
              (if (file-exists? nscd-run-directory)
                  (list (file-system-mapping
                         (source nscd-run-directory)
                         (target nscd-run-directory)))
                  '()))))

  (define (mountable-file-system? file-system)
    ;; Return #t if FILE-SYSTEM should be mounted in the container.
    (and (not (string=? "/" (file-system-mount-point file-system)))
         (file-system-needed-for-boot? file-system)))

  (let* ((os           (containerized-operating-system
                        os
                        (cons %store-mapping
                              (if shared-network?
                                  (append network-mappings mappings)
                                  mappings))
                        #:shared-network? shared-network?
                        #:extra-file-systems %container-file-systems))
         (file-systems (filter mountable-file-system?
                               (operating-system-file-systems os)))
         (specs        (map file-system->spec file-systems)))

    (define script
      (with-imported-modules (source-module-closure
                              '((guix build utils)
                                (gnu build linux-container)))
        #~(begin
            (use-modules (gnu build linux-container)
                         (gnu system file-systems) ;spec->file-system
                         (guix build utils))

            (call-with-container (map spec->file-system '#$specs)
              (lambda ()
                (setenv "HOME" "/root")
                (setenv "TMPDIR" "/tmp")
                (setenv "GUIX_NEW_SYSTEM" #$os)
                (for-each mkdir-p '("/run" "/bin" "/etc" "/home" "/var"))
                (primitive-load (string-append #$os "/boot")))
              ;; A range of 65536 uid/gids is used to cover 16 bits worth of
              ;; users and groups, which is sufficient for most cases.
              ;;
              ;; See: http://www.freedesktop.org/software/systemd/man/systemd-nspawn.html#--private-users=
              #:host-uids 65536
              #:namespaces (if #$shared-network?
                               (delq 'net %namespaces)
                               %namespaces)))))

    (gexp->script "run-container" script)))
