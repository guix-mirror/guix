;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu services avahi)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages admin)
  #:use-module (guix deprecation)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (avahi-configuration
            avahi-configuration?

            avahi-configuration-avahi
            avahi-configuration-debug?
            avahi-configuration-host-name
            avahi-configuration-publish?
            avahi-configuration-publish-workstation?
            avahi-configuration-ipv4?
            avahi-configuration-ipv6?
            avahi-configuration-wide-area?
            avahi-configuration-domains-to-browse

            avahi-service-type))

;;; Commentary:
;;;
;;; This module provides service definitions for the Avahi
;;; "zero-configuration" tool set.
;;;
;;; Code:

(define-record-type* <avahi-configuration>
  avahi-configuration make-avahi-configuration
  avahi-configuration?
  (avahi             avahi-configuration-avahi    ;file-like
                     (default avahi))
  (debug?            avahi-configuration-debug?   ;Boolean
                     (default #f))
  (host-name         avahi-configuration-host-name ;string | #f
                     (default #f))
  (publish?          avahi-configuration-publish? ;boolean
                     (default #t))

  ;; The default for this was #t in Avahi 0.6.31 and became #f in 0.7.  For
  ;; now we stick to the old default.
  (publish-workstation? avahi-configuration-publish-workstation? ;Boolean
                        (default #t))

  (ipv4?             avahi-configuration-ipv4?    ;Boolean
                     (default #t))
  (ipv6?             avahi-configuration-ipv6?    ;Boolean
                     (default #t))
  (wide-area?        avahi-configuration-wide-area? ;Boolean
                     (default #f))
  (domains-to-browse avahi-configuration-domains-to-browse ;list of strings
                     (default '())))

(define* (configuration-file config)
  "Return an avahi-daemon configuration file based on CONFIG, an
<avahi-configuration>."
  (define (bool value)
    (if value "yes\n" "no\n"))

  (define host-name (avahi-configuration-host-name config))

  (plain-file "avahi-daemon.conf"
              (string-append
               "[server]\n"
               (if host-name
                   (string-append "host-name=" host-name "\n")
                   "")

               "browse-domains=" (string-join
                                  (avahi-configuration-domains-to-browse
                                   config))
               "\n"
               "use-ipv4=" (bool (avahi-configuration-ipv4? config))
               "use-ipv6=" (bool (avahi-configuration-ipv6? config))
               "[wide-area]\n"
               "enable-wide-area=" (bool (avahi-configuration-wide-area? config))
               "[publish]\n"
               "disable-publishing="
               (bool (not (avahi-configuration-publish? config)))
               "publish-workstation="
               (bool (avahi-configuration-publish-workstation? config)))))

(define %avahi-accounts
  ;; Account and group for the Avahi daemon.
  (list (user-group (name "avahi") (system? #t))
        (user-account
         (name "avahi")
         (group "avahi")
         (system? #t)
         (comment "Avahi daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define %avahi-activation
  ;; Activation gexp.
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/run/avahi-daemon")))

(define (avahi-shepherd-service config)
  "Return a list of <shepherd-service> for CONFIG."
  (let ((config (configuration-file config))
        (debug? (avahi-configuration-debug? config))
        (avahi  (avahi-configuration-avahi config)))
    (list (shepherd-service
           (documentation "Run the Avahi mDNS/DNS-SD responder.")
           (provision '(avahi-daemon))
           (requirement '(user-processes dbus-system networking))

           (start #~(make-forkexec-constructor
                     (list #$(file-append avahi "/sbin/avahi-daemon")
                           "--daemonize"
                           #$@(if debug? #~("--debug") #~())
                           "-f" #$config)
                     #:pid-file "/run/avahi-daemon/pid"))
           (stop #~(make-kill-destructor))))))

(define avahi-service-type
  (let ((avahi-package (compose list avahi-configuration-avahi)))
    (service-type (name 'avahi)
                  (description
                   "Run @command{avahi-daemon}, a host and service discovery
daemon that implements the multicast DNS (mDNS) and DNS service
discovery (DNS-SD) protocols.  Additionally, extend the C library's name
service switch (NSS) with support for @code{.local} host name resolution.")
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            avahi-shepherd-service)
                         (service-extension dbus-root-service-type
                                            avahi-package)
                         (service-extension account-service-type
                                            (const %avahi-accounts))
                         (service-extension activation-service-type
                                            (const %avahi-activation))
                         (service-extension nscd-service-type
                                            (const (list nss-mdns)))

                         ;; Provide 'avahi-browse', 'avahi-resolve', etc. in
                         ;; the system profile.
                         (service-extension profile-service-type
                                            avahi-package)))
                  (default-value (avahi-configuration)))))

;;; avahi.scm ends here
