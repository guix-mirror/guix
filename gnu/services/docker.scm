;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Jesse Dowell <jessedowell@gmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
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

(define-module (gnu services docker)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system setuid)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages linux)               ;singularity
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:export (docker-configuration
            docker-service-type
            singularity-service-type))

(define-configuration docker-configuration
  (docker
   (file-like docker)
   "Docker daemon package.")
  (docker-cli
   (file-like docker-cli)
   "Docker client package.")
  (containerd
   (file-like containerd)
   "containerd package.")
  (proxy
   (file-like docker-libnetwork-cmd-proxy)
   "The proxy package to support inter-container and outside-container
loop-back communications.")
  (enable-proxy?
   (boolean #t)
   "Enable or disable the user-land proxy (enabled by default).")
  (debug?
   (boolean #f)
   "Enable or disable debug output.")
  (enable-iptables?
   (boolean #t)
   "Enable addition of iptables rules (enabled by default).")
  (environment-variables
   (list '())
   "Environment variables to set for dockerd")
  (no-serialization))

(define %docker-accounts
  (list (user-group (name "docker") (system? #t))))

(define (%containerd-activation config)
  (let ((state-dir "/var/lib/containerd"))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$state-dir))))

(define (%docker-activation config)
  (%containerd-activation config)
  (let ((state-dir "/var/lib/docker"))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$state-dir))))

(define (containerd-shepherd-service config)
  (let* ((package (docker-configuration-containerd config))
         (debug? (docker-configuration-debug? config))
         (containerd (docker-configuration-containerd config)))
    (shepherd-service
           (documentation "containerd daemon.")
           (provision '(containerd))
           (start #~(make-forkexec-constructor
                     (list (string-append #$package "/bin/containerd")
                           #$@(if debug?
                                  '("--log-level=debug")
                                  '()))
                     ;; For finding containerd-shim binary.
                     #:environment-variables
                     (list (string-append "PATH=" #$containerd "/bin"))
                     #:log-file "/var/log/containerd.log"))
           (stop #~(make-kill-destructor)))))

(define (docker-shepherd-service config)
  (let* ((docker (docker-configuration-docker config))
         (enable-proxy? (docker-configuration-enable-proxy? config))
         (enable-iptables? (docker-configuration-enable-iptables? config))
         (environment-variables (docker-configuration-environment-variables config))
         (proxy (docker-configuration-proxy config))
         (debug? (docker-configuration-debug? config)))
    (shepherd-service
           (documentation "Docker daemon.")
           (provision '(dockerd))
           (requirement '(containerd
                          dbus-system
                          elogind
                          file-system-/sys/fs/cgroup/blkio
                          file-system-/sys/fs/cgroup/cpu
                          file-system-/sys/fs/cgroup/cpuset
                          file-system-/sys/fs/cgroup/devices
                          file-system-/sys/fs/cgroup/memory
                          file-system-/sys/fs/cgroup/pids
                          networking
                          udev))
           (start #~(make-forkexec-constructor
                     (list (string-append #$docker "/bin/dockerd")
                           "-p" "/var/run/docker.pid"
                           #$@(if debug?
                                  '("--debug" "--log-level=debug")
                                  '())
                           #$@(if enable-proxy?
                                  (list "--userland-proxy=true"
                                        #~(string-append
                                           "--userland-proxy-path=" #$proxy "/bin/proxy"))
                                  '("--userland-proxy=false"))
                           (if #$enable-iptables?
                               "--iptables"
                               "--iptables=false"))
                     #:environment-variables
                     (list #$@environment-variables)
                     #:pid-file "/var/run/docker.pid"
                     #:log-file "/var/log/docker.log"))
           (stop #~(make-kill-destructor)))))

(define docker-service-type
  (service-type (name 'docker)
                (description "Provide capability to run Docker application
bundles in Docker containers.")
                (extensions
                 (list
                  ;; Make sure the 'docker' command is available.
                  (service-extension profile-service-type
                                     (compose list docker-configuration-docker-cli))
                  (service-extension activation-service-type
                                     %docker-activation)
                  (service-extension shepherd-root-service-type
                                     (lambda (config)
                                       (list (containerd-shepherd-service config)
                                             (docker-shepherd-service config))))
                  (service-extension account-service-type
                                     (const %docker-accounts))))
                (default-value (docker-configuration))))


;;;
;;; Singularity.
;;;

(define %singularity-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (define %mount-directory
          "/var/singularity/mnt/")

        ;; Create the directories that Singularity 2.6 expects to find.  Make
        ;; them #o755 like the 'install-data-hook' rule in 'Makefile.am' of
        ;; Singularity 2.6.1.
        (for-each (lambda (directory)
                    (let ((directory (string-append %mount-directory
                                                    directory)))
                      (mkdir-p directory)
                      (chmod directory #o755)))
                  '("container" "final" "overlay" "session"))
        (chmod %mount-directory #o755))))

(define (singularity-setuid-programs singularity)
  "Return the setuid-root programs that SINGULARITY needs."
  (define helpers
    ;; The helpers, under a meaningful name.
    (computed-file "singularity-setuid-helpers"
                   #~(begin
                       (mkdir #$output)
                       (for-each (lambda (program)
                                   (symlink (string-append #$singularity
                                                           "/libexec/singularity"
                                                           "/bin/"
                                                           program "-suid")
                                            (string-append #$output
                                                           "/singularity-"
                                                           program
                                                           "-helper")))
                                 '("action" "mount" "start")))))

  (map file-like->setuid-program
       (list (file-append helpers "/singularity-action-helper")
             (file-append helpers "/singularity-mount-helper")
             (file-append helpers "/singularity-start-helper"))))

(define singularity-service-type
  (service-type (name 'singularity)
                (description
                 "Install the Singularity application bundle tool.")
                (extensions
                 (list (service-extension setuid-program-service-type
                                          singularity-setuid-programs)
                       (service-extension activation-service-type
                                          (const %singularity-activation))))
                (default-value singularity)))
