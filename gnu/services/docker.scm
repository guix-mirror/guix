;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
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
  #:use-module (gnu system shadow)
  #:use-module (gnu packages docker)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:export (docker-configuration
            docker-service-type))

;;; We're not using serialize-configuration, but we must define this because
;;; the define-configuration macro validates it exists.
(define (serialize-boolean field-name val)
  "")

(define-configuration docker-configuration
  (docker
   (package docker)
   "Docker daemon package.")
  (containerd
   (package containerd)
   "containerd package.")
  (proxy
   (package docker-libnetwork-cmd-proxy)
   "The proxy package to support inter-container and outside-container
loop-back communications.")
  (enable-proxy?
   (boolean #t)
   "Enable or disable the user-land proxy (enabled by default)."))

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
  (let* ((package (docker-configuration-containerd config)))
    (shepherd-service
           (documentation "containerd daemon.")
           (provision '(containerd))
           (start #~(make-forkexec-constructor
                     (list (string-append #$package "/bin/containerd"))
                     #:log-file "/var/log/containerd.log"))
           (stop #~(make-kill-destructor)))))

(define (docker-shepherd-service config)
  (let* ((docker (docker-configuration-docker config))
         (enable-proxy? (docker-configuration-enable-proxy? config))
         (proxy (docker-configuration-proxy config)))
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
                          ; TODO: file-system-/sys/fs/cgroup/pids
                          networking
                          udev))
           (start #~(make-forkexec-constructor
                     (list (string-append #$docker "/bin/dockerd")
                           "-p" "/var/run/docker.pid"
                           (if #$enable-proxy? "--userland-proxy" "")
                           "--userland-proxy-path" (string-append #$proxy
                                                                  "/bin/proxy"))
                     #:pid-file "/var/run/docker.pid"
                     #:log-file "/var/log/docker.log"))
           (stop #~(make-kill-destructor)))))

(define docker-service-type
  (service-type (name 'docker)
                (description "Provide capability to run Docker application
bundles in Docker containers.")
                (extensions
                 (list
                  (service-extension activation-service-type
                                     %docker-activation)
                  (service-extension shepherd-root-service-type
                                     (lambda (config)
                                       (list (containerd-shepherd-service config)
                                             (docker-shepherd-service config))))
                  (service-extension account-service-type
                                     (const %docker-accounts))))
                (default-value (docker-configuration))))
