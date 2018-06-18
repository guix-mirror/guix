;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
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

(define-module (gnu services monitoring)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (darkstat-configuration
            prometheus-node-exporter-configuration
            darkstat-service-type
            prometheus-node-exporter-service-type))


;;;
;;; darkstat
;;;

(define-record-type* <darkstat-configuration>
  darkstat-configuration make-darkstat-configuration darkstat-configuration?
  (package      darkstat-configuration-package
                (default darkstat))
  (interface    darkstat-configuration-interface)
  (port         darkstat-configuration-port
                (default "667"))
  (bind-address darkstat-configuration-bind-address
                (default "127.0.0.1"))
  (base         darkstat-configuration-base
                (default "/")))

(define %darkstat-accounts
  (list (user-account
         (name "darkstat")
         (group "darkstat")
         (system? #t)
         (comment "darkstat daemon user")
         (home-directory "/var/lib/darkstat")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "darkstat")
         (system? #t))))

(define darkstat-shepherd-service
  (match-lambda
    (($ <darkstat-configuration>
        package interface port bind-address base)
     (shepherd-service
      (documentation "Network statistics gatherer.")
      (provision '(darkstat))
      (requirement '(networking))
      (start #~(make-forkexec-constructor
                (list #$(file-append package "/sbin/darkstat")
                      "-i" #$interface
                      "-p" #$port
                      "-b" #$bind-address
                      "--base" #$base
                      "--syslog" "--no-daemon"
                      "--chroot" "/var/lib/darkstat"
                      "--user" "darkstat"
                      "--import" "darkstat.db"
                      "--export" "darkstat.db")))
      (stop #~(make-kill-destructor))))))

(define darkstat-service-type
  (service-type
   (name 'darkstat)
   (description
    "Run @command{darkstat} to serve network traffic statictics reports over
HTTP.")
   (extensions
    (list (service-extension account-service-type
                             (const %darkstat-accounts))
          (service-extension shepherd-root-service-type
                             (compose list darkstat-shepherd-service))))))

(define-record-type* <prometheus-node-exporter-configuration>
  prometheus-node-exporter-configuration
  make-prometheus-node-exporter-configuration
  prometheus-node-exporter-configuration?
  (package prometheus-node-exporter-configuration-package
           (default go-github-com-prometheus-node-exporter))
  (web-listen-address prometheus-node-exporter-web-listen-address
                      (default ":9100")))

(define prometheus-node-exporter-shepherd-service
  (match-lambda
    (( $ <prometheus-node-exporter-configuration>
         package web-listen-address)
     (shepherd-service
      (documentation "Prometheus node exporter.")
      (provision '(prometheus-node-exporter))
      (requirement '(networking))
      (start #~(make-forkexec-constructor
                (list #$(file-append package "/bin/node_exporter")
                      "--web.listen-address" #$web-listen-address)))
      (stop #~(make-kill-destructor))))))

(define prometheus-node-exporter-service-type
  (service-type
   (name 'prometheus-node-exporter)
   (description
    "Run @command{node_exporter} to serve hardware and OS metrics to
prometheus.")
   (extensions
    (list (service-extension
           shepherd-root-service-type
           (compose list prometheus-node-exporter-shepherd-service))))))
