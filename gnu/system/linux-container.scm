;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:export (system-container
            containerized-operating-system
            container-script
            eval/container))

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
                 `(("locale" ,(operating-system-locale-directory os))))
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

(define dummy-networking-service-type
  (shepherd-service-type
   'dummy-networking
   (const (shepherd-service
           (documentation "Provide loopback and networking without actually
doing anything.")
           (provision '(loopback networking))
           (start #~(const #t))))
   #f))

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
                (list nscd-service-type
                      static-networking-service-type
                      dhcp-client-service-type
                      network-manager-service-type
                      connman-service-type
                      wicd-service-type)
                (list))))

  (operating-system
    (inherit os)
    (swap-devices '()) ; disable swap
    (essential-services (container-essential-services
                         this-operating-system
                         #:shared-network? shared-network?))
    (services (append (remove (lambda (service)
                                (memq (service-kind service)
                                      useless-services))
                              (operating-system-user-services os))
                      ;; Many Guix services depend on a 'networking' shepherd
                      ;; service, so make sure to provide a dummy 'networking'
                      ;; service when we are sure that networking is already set up
                      ;; in the host and can be used.  That prevents double setup.
                      (if shared-network?
                          (list (service dummy-networking-service-type))
                          '())))
    (file-systems (append (map mapping->fs
                               (if shared-network?
                                   (append %network-file-mappings mappings)
                                   mappings))
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
  (define (mountable-file-system? file-system)
    ;; Return #t if FILE-SYSTEM should be mounted in the container.
    (and (not (string=? "/" (file-system-mount-point file-system)))
         (file-system-needed-for-boot? file-system)))

  (define (os-file-system-specs os)
    (map file-system->spec
         (filter mountable-file-system?
                 (operating-system-file-systems os))))

  (let* ((os (containerized-operating-system
              os (cons %store-mapping mappings)
              #:shared-network? shared-network?
              #:extra-file-systems %container-file-systems))
         (specs (os-file-system-specs os)))

    (define script
      (with-imported-modules (source-module-closure
                              '((guix build utils)
                                (gnu build linux-container)
                                (guix i18n)
                                (guix diagnostics)))
        #~(begin
            (use-modules (gnu build linux-container)
                         (gnu system file-systems) ;spec->file-system
                         (guix build utils)
                         (guix i18n)
                         (guix diagnostics)
                         (srfi srfi-1))

            (define file-systems
              (filter-map (lambda (spec)
                            (let* ((fs    (spec->file-system spec))
                                   (flags (file-system-flags fs)))
                              (and (or (not (memq 'bind-mount flags))
                                       (file-exists? (file-system-device fs)))
                                   fs)))
                          '#$specs))

            (define (explain pid)
              ;; XXX: We can't quite call 'bindtextdomain' so there's actually
              ;; no i18n.
              ;; XXX: Should we really give both options? 'guix container exec'
              ;; is a more verbose command.  Hard to fail to enter the container
              ;; when we list two options.
              (info (G_ "system container is running as PID ~a~%") pid)
              (info (G_ "Run 'sudo guix container exec ~a /run/current-system/profile/bin/bash --login'\n")
                    pid)
              (info (G_ "or run 'sudo nsenter -a -t ~a' to get a shell into it.~%") pid)
              (newline (guix-warning-port)))

            (call-with-container file-systems
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
                               %namespaces)
              #:process-spawned-hook explain))))

    (gexp->script "run-container" script)))

(define* (eval/container exp
                         #:key
                         (mappings '())
                         (namespaces %namespaces))
  "Evaluate EXP, a gexp, in a new process executing in separate namespaces as
listed in NAMESPACES.  Add MAPPINGS, a list of <file-system-mapping>, to the
set of directories visible in the process's mount namespace.  Return the
process' exit status as a monadic value.

This is useful to implement processes that, unlike derivations, are not
entirely pure and need to access the outside world or to perform side
effects."
  (mlet %store-monad ((lowered (lower-gexp exp)))
    (define inputs
      (cons (lowered-gexp-guile lowered)
            (lowered-gexp-inputs lowered)))

    (define items
      (append (append-map derivation-input-output-paths inputs)
              (lowered-gexp-sources lowered)))

    (mbegin %store-monad
      (built-derivations inputs)
      (mlet %store-monad ((closure ((store-lift requisites) items)))
        (return (call-with-container (map file-system-mapping->bind-mount
                                          (append (map (lambda (item)
                                                         (file-system-mapping
                                                          (source item)
                                                          (target source)))
                                                       closure)
                                                  mappings))
                  (lambda ()
                    (apply execl
                           (string-append (derivation-input-output-path
                                           (lowered-gexp-guile lowered))
                                          "/bin/guile")
                           "guile"
                           (append (append-map (lambda (directory)
                                                 `("-L" ,directory))
                                               (lowered-gexp-load-path lowered))
                                   (append-map (lambda (directory)
                                                 `("-C" ,directory))
                                               (lowered-gexp-load-compiled-path
                                                lowered))
                                   (list "-c"
                                         (object->string
                                          (lowered-gexp-sexp lowered))))))))))))
