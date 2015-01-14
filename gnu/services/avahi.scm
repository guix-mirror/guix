;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (gnu system shadow)
  #:use-module (gnu packages avahi)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:export (avahi-service))

;;; Commentary:
;;;
;;; This module provides service definitions for the Avahi
;;; "zero-configuration" tool set.
;;;
;;; Code:

(define* (configuration-file #:key host-name publish?
                             ipv4? ipv6? wide-area? domains-to-browse)
  "Return an avahi-daemon configuration file."
  (define (bool value)
    (if value "yes\n" "no\n"))

  (text-file "avahi-daemon.conf"
             (string-append
              "[server]\n"
              (if host-name
                  (string-append "host-name=" host-name "\n")
                  "")

              "browse-domains=" (string-join domains-to-browse)
              "\n"
              "use-ipv4=" (bool ipv4?)
              "use-ipv6=" (bool ipv6?)
              "[wide-area]\n"
              "enable-wide-area=" (bool wide-area?)
              "[publish]\n"
              "disable-publishing=" (bool (not publish?)))))

(define* (avahi-service #:key (avahi avahi)
                        host-name
                        (publish? #t)
                        (ipv4? #t) (ipv6? #t)
                        wide-area?
                        (domains-to-browse '()))
  "Return a service that runs @command{avahi-daemon}, a system-wide
mDNS/DNS-SD responder that allows for service discovery and
\"zero-configuration\" host name lookups.

If @var{host-name} is different from @code{#f}, use that as the host name to
publish for this machine; otherwise, use the machine's actual host name.

When @var{publish?} is true, publishing of host names and services is allowed;
in particular, avahi-daemon will publish the machine's host name and IP
address via mDNS on the local network.

When @var{wide-area?} is true, DNS-SD over unicast DNS is enabled.

Boolean values @var{ipv4?} and @var{ipv6?} determine whether to use IPv4/IPv6
sockets."
  (mlet %store-monad ((config (configuration-file #:host-name host-name
                                                  #:publish? publish?
                                                  #:ipv4? ipv4?
                                                  #:ipv6? ipv6?
                                                  #:wide-area? wide-area?
                                                  #:domains-to-browse
                                                  domains-to-browse)))
    (return
     (service
      (documentation "Run the Avahi mDNS/DNS-SD responder.")
      (provision '(avahi-daemon))
      (requirement '(dbus-system networking))

      (start #~(make-forkexec-constructor
                (list (string-append #$avahi "/sbin/avahi-daemon")
                      "--syslog" "-f" #$config)))
      (stop #~(make-kill-destructor))
      (activate #~(begin
                    (use-modules (guix build utils))
                    (mkdir-p "/var/run/avahi-daemon")))

      (user-groups (list (user-group
                          (name "avahi")
                          (system? #t))))
      (user-accounts (list (user-account
                            (name "avahi")
                            (group "avahi")
                            (system? #t)
                            (comment "Avahi daemon user")
                            (home-directory "/var/empty")
                            (shell
                             "/run/current-system/profile/sbin/nologin"))))))))

;;; avahi.scm ends here
