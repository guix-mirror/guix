;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2021 Christopher Lemmer Webber <cwebber@dustycloud.org>
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

(define-module (gnu services networking)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services linux)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cluster)
  #:use-module (gnu packages connman)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages usb-modeswitch)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ntp)
  #:use-module (gnu packages wicd)
  #:use-module (gnu packages gnome)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (rnrs enums)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:re-export (static-networking-service
               static-networking-service-type)
  #:export (%facebook-host-aliases
            dhcp-client-service
            dhcp-client-service-type

            dhcpd-service-type
            dhcpd-configuration
            dhcpd-configuration?
            dhcpd-configuration-package
            dhcpd-configuration-config-file
            dhcpd-configuration-version
            dhcpd-configuration-run-directory
            dhcpd-configuration-lease-file
            dhcpd-configuration-pid-file
            dhcpd-configuration-interfaces

            ntp-configuration
            ntp-configuration?
            ntp-configuration-ntp
            ntp-configuration-servers
            ntp-allow-large-adjustment?

            %ntp-servers
            ntp-server
            ntp-server-type
            ntp-server-address
            ntp-server-options

            ntp-service
            ntp-service-type

            %openntpd-servers
            openntpd-configuration
            openntpd-configuration?
            openntpd-service-type

            inetd-configuration
            inetd-entry
            inetd-service-type

            tor-configuration
            tor-configuration?
            tor-hidden-service
            tor-service
            tor-service-type

            wicd-service-type
            wicd-service

            network-manager-configuration
            network-manager-configuration?
            network-manager-configuration-dns
            network-manager-configuration-vpn-plugins
            network-manager-service-type

            connman-configuration
            connman-configuration?
            connman-service-type

            modem-manager-configuration
            modem-manager-configuration?
            modem-manager-service-type

            usb-modeswitch-configuration
            usb-modeswitch-configuration?
            usb-modeswitch-configuration-usb-modeswitch
            usb-modeswitch-configuration-usb-modeswitch-data
            usb-modeswitch-service-type

            wpa-supplicant-configuration
            wpa-supplicant-configuration?
            wpa-supplicant-configuration-wpa-supplicant
            wpa-supplicant-configuration-requirement
            wpa-supplicant-configuration-pid-file
            wpa-supplicant-configuration-dbus?
            wpa-supplicant-configuration-interface
            wpa-supplicant-configuration-config-file
            wpa-supplicant-configuration-extra-options
            wpa-supplicant-service-type

            hostapd-configuration
            hostapd-configuration?
            hostapd-configuration-package
            hostapd-configuration-interface
            hostapd-configuration-ssid
            hostapd-configuration-broadcast-ssid?
            hostapd-configuration-channel
            hostapd-configuration-driver
            hostapd-service-type

            simulated-wifi-service-type

            openvswitch-service-type
            openvswitch-configuration

            iptables-configuration
            iptables-configuration?
            iptables-configuration-iptables
            iptables-configuration-ipv4-rules
            iptables-configuration-ipv6-rules
            iptables-service-type

            nftables-service-type
            nftables-configuration
            nftables-configuration?
            nftables-configuration-package
            nftables-configuration-ruleset
            %default-nftables-ruleset

            pagekite-service-type
            pagekite-configuration
            pagekite-configuration?
            pagekite-configuration-package
            pagekite-configuration-kitename
            pagekite-configuration-kitesecret
            pagekite-configuration-frontend
            pagekite-configuration-kites
            pagekite-configuration-extra-file

            yggdrasil-service-type
            yggdrasil-configuration
            yggdrasil-configuration?
            yggdrasil-configuration-autoconf?
            yggdrasil-configuration-config-file
            yggdrasil-configuration-log-level
            yggdrasil-configuration-log-to
            yggdrasil-configuration-json-config
            yggdrasil-configuration-package

            keepalived-configuration
            keepalived-configuration?
            keepalived-service-type))

;;; Commentary:
;;;
;;; Networking services.
;;;
;;; Code:

(define %facebook-host-aliases
  ;; This is the list of known Facebook hosts to be added to /etc/hosts if you
  ;; are to block it.
  "\
# Block Facebook IPv4.
127.0.0.1   www.facebook.com
127.0.0.1   facebook.com
127.0.0.1   login.facebook.com
127.0.0.1   www.login.facebook.com
127.0.0.1   fbcdn.net
127.0.0.1   www.fbcdn.net
127.0.0.1   fbcdn.com
127.0.0.1   www.fbcdn.com
127.0.0.1   static.ak.fbcdn.net
127.0.0.1   static.ak.connect.facebook.com
127.0.0.1   connect.facebook.net
127.0.0.1   www.connect.facebook.net
127.0.0.1   apps.facebook.com

# Block Facebook IPv6.
fe80::1%lo0 facebook.com
fe80::1%lo0 login.facebook.com
fe80::1%lo0 www.login.facebook.com
fe80::1%lo0 fbcdn.net
fe80::1%lo0 www.fbcdn.net
fe80::1%lo0 fbcdn.com
fe80::1%lo0 www.fbcdn.com
fe80::1%lo0 static.ak.fbcdn.net
fe80::1%lo0 static.ak.connect.facebook.com
fe80::1%lo0 connect.facebook.net
fe80::1%lo0 www.connect.facebook.net
fe80::1%lo0 apps.facebook.com\n")

(define dhcp-client-service-type
  (shepherd-service-type
   'dhcp-client
   (lambda (dhcp)
     (define dhclient
       (file-append dhcp "/sbin/dhclient"))

     (define pid-file
       "/var/run/dhclient.pid")

     (shepherd-service
      (documentation "Set up networking via DHCP.")
      (requirement '(user-processes udev))

      ;; XXX: Running with '-nw' ("no wait") avoids blocking for a minute when
      ;; networking is unavailable, but also means that the interface is not up
      ;; yet when 'start' completes.  To wait for the interface to be ready, one
      ;; should instead monitor udev events.
      (provision '(networking))

      (start #~(lambda _
                 ;; When invoked without any arguments, 'dhclient' discovers all
                 ;; non-loopback interfaces *that are up*.  However, the relevant
                 ;; interfaces are typically down at this point.  Thus we perform
                 ;; our own interface discovery here.
                 (define valid?
                   (lambda (interface)
                     (and (arp-network-interface? interface)
                          (not (loopback-network-interface? interface))
                          ;; XXX: Make sure the interfaces are up so that
                          ;; 'dhclient' can actually send/receive over them.
                          ;; Ignore those that cannot be activated.
                          (false-if-exception
                           (set-network-interface-up interface)))))
                 (define ifaces
                   (filter valid? (all-network-interface-names)))

                 (false-if-exception (delete-file #$pid-file))
                 (let ((pid (fork+exec-command
                             (cons* #$dhclient "-nw"
                                    "-pf" #$pid-file ifaces))))
                   (and (zero? (cdr (waitpid pid)))
                        (read-pid-file #$pid-file)))))
      (stop #~(make-kill-destructor))))
   isc-dhcp
   (description "Run @command{dhcp}, a Dynamic Host Configuration
Protocol (DHCP) client, on all the non-loopback network interfaces.")))

(define-deprecated (dhcp-client-service #:key (dhcp isc-dhcp))
  dhcp-client-service-type
  "Return a service that runs @var{dhcp}, a Dynamic Host Configuration
Protocol (DHCP) client, on all the non-loopback network interfaces."
  (service dhcp-client-service-type dhcp))

(define-record-type* <dhcpd-configuration>
  dhcpd-configuration make-dhcpd-configuration
  dhcpd-configuration?
  (package   dhcpd-configuration-package ;<package>
             (default isc-dhcp))
  (config-file   dhcpd-configuration-config-file ;file-like
                 (default #f))
  (version dhcpd-configuration-version ;"4", "6", or "4o6"
              (default "4"))
  (run-directory dhcpd-configuration-run-directory
                 (default "/run/dhcpd"))
  (lease-file dhcpd-configuration-lease-file
              (default "/var/db/dhcpd.leases"))
  (pid-file dhcpd-configuration-pid-file
            (default "/run/dhcpd/dhcpd.pid"))
  ;; list of strings, e.g. (list "enp0s25")
  (interfaces dhcpd-configuration-interfaces
              (default '())))

(define dhcpd-shepherd-service
  (match-lambda
    (($ <dhcpd-configuration> package config-file version run-directory
                              lease-file pid-file interfaces)
     (unless config-file
       (error "Must supply a config-file"))
     (list (shepherd-service
            ;; Allow users to easily run multiple versions simultaneously.
            (provision (list (string->symbol
                              (string-append "dhcpv" version "-daemon"))))
            (documentation (string-append "Run the DHCPv" version " daemon"))
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      '(#$(file-append package "/sbin/dhcpd")
                        #$(string-append "-" version)
                        "-lf" #$lease-file
                        "-pf" #$pid-file
                        "-cf" #$config-file
                        #$@interfaces)
                      #:pid-file #$pid-file))
            (stop #~(make-kill-destructor)))))))

(define dhcpd-activation
  (match-lambda
    (($ <dhcpd-configuration> package config-file version run-directory
                              lease-file pid-file interfaces)
     (with-imported-modules '((guix build utils))
       #~(begin
           (unless (file-exists? #$run-directory)
             (mkdir #$run-directory))
           ;; According to the DHCP manual (man dhcpd.leases), the lease
           ;; database must be present for dhcpd to start successfully.
           (unless (file-exists? #$lease-file)
             (with-output-to-file #$lease-file
               (lambda _ (display ""))))
           ;; Validate the config.
           (invoke/quiet
            #$(file-append package "/sbin/dhcpd") "-t" "-cf"
            #$config-file))))))

(define dhcpd-service-type
  (service-type
   (name 'dhcpd)
   (extensions
    (list (service-extension shepherd-root-service-type dhcpd-shepherd-service)
          (service-extension activation-service-type dhcpd-activation)))
   (description "Run a DHCP (Dynamic Host Configuration Protocol) daemon.  The
daemon is responsible for allocating IP addresses to its client.")))


;;;
;;; NTP.
;;;

(define ntp-server-types (make-enumeration
                          '(pool
                            server
                            peer
                            broadcast
                            manycastclient)))

(define-record-type* <ntp-server>
  ntp-server make-ntp-server
  ntp-server?
  ;; The type can be one of the symbols of the NTP-SERVER-TYPE? enumeration.
  (type ntp-server-type
        (default 'server))
  (address ntp-server-address)    ; a string
  ;; The list of options can contain single option names or tuples in the form
  ;; '(name value).
  (options ntp-server-options
           (default '())))

(define (ntp-server->string ntp-server)
  ;; Serialize the NTP server object as a string, ready to use in the NTP
  ;; configuration file.
  (define (flatten lst)
    (reverse
     (let loop ((x lst)
                (res '()))
       (if (list? x)
           (fold loop res x)
           (cons (format #f "~a" x) res)))))

  (match ntp-server
    (($ <ntp-server> type address options)
     ;; XXX: It'd be neater if fields were validated at the syntax level (for
     ;; static ones at least).  Perhaps the Guix record type could support a
     ;; predicate property on a field?
     (unless (enum-set-member? type ntp-server-types)
       (error "Invalid NTP server type" type))
     (string-join (cons* (symbol->string type)
                         address
                         (flatten options))))))

(define %ntp-servers
  ;; Default set of NTP servers. These URLs are managed by the NTP Pool project.
  ;; Within Guix, Leo Famulari <leo@famulari.name> is the administrative contact
  ;; for this NTP pool "zone".
  (list
   (ntp-server
    (type 'pool)
    (address "0.guix.pool.ntp.org")
    (options '("iburst")))))               ;as recommended in the ntpd manual

(define-record-type* <ntp-configuration>
  ntp-configuration make-ntp-configuration
  ntp-configuration?
  (ntp      ntp-configuration-ntp
            (default ntp))
  (servers  %ntp-configuration-servers   ;list of <ntp-server> objects
            (default %ntp-servers))
  (allow-large-adjustment? ntp-allow-large-adjustment?
                           (default #t))) ;as recommended in the ntpd manual

(define (ntp-configuration-servers ntp-configuration)
  ;; A wrapper to support the deprecated form of this field.
  (let ((ntp-servers (%ntp-configuration-servers ntp-configuration)))
    (match ntp-servers
      (((? string?) (? string?) ...)
       (format (current-error-port) "warning: Defining NTP servers as strings is \
deprecated.  Please use <ntp-server> records instead.\n")
       (map (lambda (addr)
              (ntp-server
               (type 'server)
               (address addr)
               (options '()))) ntp-servers))
      ((($ <ntp-server>) ($ <ntp-server>) ...)
       ntp-servers))))

(define ntp-shepherd-service
  (lambda (config)
    (match config
      (($ <ntp-configuration> ntp servers allow-large-adjustment?)
       (let ((servers (ntp-configuration-servers config)))
         ;; TODO: Add authentication support.
         (define config
           (string-append "driftfile /var/run/ntpd/ntp.drift\n"
                          (string-join (map ntp-server->string servers)
                                       "\n")
                          "
# Disable status queries as a workaround for CVE-2013-5211:
# <http://support.ntp.org/bin/view/Main/SecurityNotice#DRDoS_Amplification_Attack_using>.
restrict default kod nomodify notrap nopeer noquery limited
restrict -6 default kod nomodify notrap nopeer noquery limited

# Yet, allow use of the local 'ntpq'.
restrict 127.0.0.1
restrict -6 ::1

# This is required to use servers from a pool directive when using the 'nopeer'
# option by default, as documented in the 'ntp.conf' manual.
restrict source notrap nomodify noquery\n"))

         (define ntpd.conf
           (plain-file "ntpd.conf" config))

         (list (shepherd-service
                (provision '(ntpd))
                (documentation "Run the Network Time Protocol (NTP) daemon.")
                (requirement '(user-processes networking))
                (start #~(make-forkexec-constructor
                          (list (string-append #$ntp "/bin/ntpd") "-n"
                                "-c" #$ntpd.conf "-u" "ntpd"
                                #$@(if allow-large-adjustment?
                                       '("-g")
                                       '()))))
                (stop #~(make-kill-destructor)))))))))

(define %ntp-accounts
  (list (user-account
         (name "ntpd")
         (group "nogroup")
         (system? #t)
         (comment "NTP daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))


(define (ntp-service-activation config)
  "Return the activation gexp for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (define %user
          (getpw "ntpd"))

        (let ((directory "/var/run/ntpd"))
          (mkdir-p directory)
          (chown directory (passwd:uid %user) (passwd:gid %user))))))

(define ntp-service-type
  (service-type (name 'ntp)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          ntp-shepherd-service)
                       (service-extension account-service-type
                                          (const %ntp-accounts))
                       (service-extension activation-service-type
                                          ntp-service-activation)))
                (description
                 "Run the @command{ntpd}, the Network Time Protocol (NTP)
daemon of the @uref{http://www.ntp.org, Network Time Foundation}.  The daemon
will keep the system clock synchronized with that of the given servers.")
                (default-value (ntp-configuration))))

(define-deprecated (ntp-service #:key (ntp ntp)
                                (servers %ntp-servers)
                                allow-large-adjustment?)
  ntp-service-type
  "Return a service that runs the daemon from @var{ntp}, the
@uref{http://www.ntp.org, Network Time Protocol package}.  The daemon will
keep the system clock synchronized with that of @var{servers}.
@var{allow-large-adjustment?} determines whether @command{ntpd} is allowed to
make an initial adjustment of more than 1,000 seconds."
  (service ntp-service-type
           (ntp-configuration (ntp ntp)
                              (servers servers)
                              (allow-large-adjustment?
                               allow-large-adjustment?))))


;;;
;;; OpenNTPD.
;;;

(define %openntpd-servers
  (map ntp-server-address %ntp-servers))

(define-record-type* <openntpd-configuration>
  openntpd-configuration make-openntpd-configuration
  openntpd-configuration?
  (openntpd                openntpd-configuration-openntpd
                           (default openntpd))
  (listen-on               openntpd-listen-on
                           (default '("127.0.0.1"
                                      "::1")))
  (query-from              openntpd-query-from
                           (default '()))
  (sensor                  openntpd-sensor
                           (default '()))
  (server                  openntpd-server
                           (default '()))
  (servers                 openntpd-servers
                           (default %openntpd-servers))
  (constraint-from         openntpd-constraint-from
                           (default '()))
  (constraints-from        openntpd-constraints-from
                           (default '())))

(define (openntpd-configuration->string config)

  (define (quote-field? name)
    (member name '("constraints from")))

  (match-record config <openntpd-configuration>
    (listen-on query-from sensor server servers constraint-from
               constraints-from)
    (string-append
     (string-join
      (concatenate
       (filter-map (lambda (field values)
                     (match values
                       (() #f)          ;discard entry with filter-map
                       ((val ...)       ;validate value type
                        (map (lambda (value)
                               (if (quote-field? field)
                                   (format #f "~a \"~a\"" field value)
                                   (format #f "~a ~a" field value)))
                             values))))
                   ;; The entry names.
                   '("listen on" "query from" "sensor" "server" "servers"
                     "constraint from" "constraints from")
                   ;; The corresponding entry values.
                   (list listen-on query-from sensor server servers
                         constraint-from constraints-from)))
      "\n")
     "\n")))                              ;add a trailing newline

(define (openntpd-shepherd-service config)
  (let ((openntpd (openntpd-configuration-openntpd config)))

    (define ntpd.conf
      (plain-file "ntpd.conf" (openntpd-configuration->string config)))

    (list (shepherd-service
           (provision '(ntpd))
           (documentation "Run the Network Time Protocol (NTP) daemon.")
           (requirement '(user-processes networking))
           (start #~(make-forkexec-constructor
                     (list (string-append #$openntpd "/sbin/ntpd")
                           "-f" #$ntpd.conf
                           "-d") ;; don't daemonize
                     ;; When ntpd is daemonized it repeatedly tries to respawn
                     ;; while running, leading shepherd to disable it.  To
                     ;; prevent spamming stderr, redirect output to logfile.
                     #:log-file "/var/log/ntpd"))
           (stop #~(make-kill-destructor))))))

(define (openntpd-service-activation config)
  "Return the activation gexp for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (mkdir-p "/var/db")
        (mkdir-p "/var/run")
        (unless (file-exists? "/var/db/ntpd.drift")
          (with-output-to-file "/var/db/ntpd.drift"
                               (lambda _
                                 (format #t "0.0")))))))

(define openntpd-service-type
  (service-type (name 'openntpd)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          openntpd-shepherd-service)
                       (service-extension account-service-type
                                          (const %ntp-accounts))
                       (service-extension profile-service-type
                                          (compose list openntpd-configuration-openntpd))
                       (service-extension activation-service-type
                                          openntpd-service-activation)))
                (default-value (openntpd-configuration))
                (description
                 "Run the @command{ntpd}, the Network Time Protocol (NTP)
daemon, as implemented by @uref{http://www.openntpd.org, OpenNTPD}.  The
daemon will keep the system clock synchronized with that of the given servers.")))


;;;
;;; Inetd.
;;;

(define-record-type* <inetd-configuration> inetd-configuration
  make-inetd-configuration
  inetd-configuration?
  (program           inetd-configuration-program   ;file-like
                     (default (file-append inetutils "/libexec/inetd")))
  (entries           inetd-configuration-entries   ;list of <inetd-entry>
                     (default '())))

(define-record-type* <inetd-entry> inetd-entry make-inetd-entry
  inetd-entry?
  (node              inetd-entry-node         ;string or #f
                     (default #f))
  (name              inetd-entry-name)        ;string, from /etc/services

  (socket-type       inetd-entry-socket-type) ;stream | dgram | raw |
                                              ;rdm | seqpacket
  (protocol          inetd-entry-protocol)    ;string, from /etc/protocols

  (wait?             inetd-entry-wait?        ;Boolean
                     (default #t))
  (user              inetd-entry-user)        ;string

  (program           inetd-entry-program      ;string or file-like object
                     (default "internal"))
  (arguments         inetd-entry-arguments    ;list of strings or file-like objects
                     (default '())))

(define (inetd-config-file entries)
  (apply mixed-text-file "inetd.conf"
         (map
          (lambda (entry)
            (let* ((node (inetd-entry-node entry))
                   (name (inetd-entry-name entry))
                   (socket
                    (if node (string-append node ":" name) name))
                   (type
                    (match (inetd-entry-socket-type entry)
                      ((or 'stream 'dgram 'raw 'rdm 'seqpacket)
                       (symbol->string (inetd-entry-socket-type entry)))))
                   (protocol (inetd-entry-protocol entry))
                   (wait (if (inetd-entry-wait? entry) "wait" "nowait"))
                   (user (inetd-entry-user entry))
                   (program (inetd-entry-program entry))
                   (args (inetd-entry-arguments entry)))
              #~(string-append
                 (string-join
                  (list #$@(list socket type protocol wait user program) #$@args)
                  " ") "\n")))
          entries)))

(define inetd-shepherd-service
  (match-lambda
    (($ <inetd-configuration> program ()) '()) ; empty list of entries -> do nothing
    (($ <inetd-configuration> program entries)
     (list
      (shepherd-service
       (documentation "Run inetd.")
       (provision '(inetd))
       (requirement '(user-processes networking syslogd))
       (start #~(make-forkexec-constructor
                 (list #$program #$(inetd-config-file entries))
                 #:pid-file "/var/run/inetd.pid"))
       (stop #~(make-kill-destructor)))))))

(define-public inetd-service-type
  (service-type
   (name 'inetd)
   (extensions
    (list (service-extension shepherd-root-service-type
                             inetd-shepherd-service)))

   ;; The service can be extended with additional lists of entries.
   (compose concatenate)
   (extend (lambda (config entries)
             (inetd-configuration
              (inherit config)
              (entries (append (inetd-configuration-entries config)
                               entries)))))
   (description
    "Start @command{inetd}, the @dfn{Internet superserver}.  It is responsible
for listening on Internet sockets and spawning the corresponding services on
demand.")))


;;;
;;; Tor.
;;;

(define-record-type* <tor-configuration>
  tor-configuration make-tor-configuration
  tor-configuration?
  (tor              tor-configuration-tor
                    (default tor))
  (config-file      tor-configuration-config-file
                    (default (plain-file "empty" "")))
  (hidden-services  tor-configuration-hidden-services
                    (default '()))
  (socks-socket-type tor-configuration-socks-socket-type ; 'tcp or 'unix
                     (default 'tcp))
  (control-socket?  tor-control-socket-path
                    (default #f)))

(define %tor-accounts
  ;; User account and groups for Tor.
  (list (user-group (name "tor") (system? #t))
        (user-account
         (name "tor")
         (group "tor")
         (system? #t)
         (comment "Tor daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define-record-type <hidden-service>
  (hidden-service name mapping)
  hidden-service?
  (name    hidden-service-name)                   ;string
  (mapping hidden-service-mapping))               ;list of port/address tuples

(define (tor-configuration->torrc config)
  "Return a 'torrc' file for CONFIG."
  (match config
    (($ <tor-configuration> tor config-file services
                            socks-socket-type control-socket?)
     (computed-file
      "torrc"
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils)
                         (ice-9 match))

            (call-with-output-file #$output
              (lambda (port)
                (display "\
### These lines were generated from your system configuration:
User tor
DataDirectory /var/lib/tor
PidFile /var/run/tor/tor.pid
Log notice syslog\n" port)
                (when (eq? 'unix '#$socks-socket-type)
                  (display "\
SocksPort unix:/var/run/tor/socks-sock
UnixSocksGroupWritable 1\n" port))
                (when #$control-socket?
                  (display "\
ControlSocket unix:/var/run/tor/control-sock GroupWritable RelaxDirModeCheck
ControlSocketsGroupWritable 1\n"
                           port))

                (for-each (match-lambda
                            ((service (ports hosts) ...)
                             (format port "\
HiddenServiceDir /var/lib/tor/hidden-services/~a~%"
                                     service)
                             (for-each (lambda (tcp-port host)
                                         (format port "\
HiddenServicePort ~a ~a~%"
                                                 tcp-port host))
                                       ports hosts)))
                          '#$(map (match-lambda
                                    (($ <hidden-service> name mapping)
                                     (cons name mapping)))
                                  services))

                (display "\
### End of automatically generated lines.\n\n" port)

                ;; Append the user's config file.
                (call-with-input-file #$config-file
                  (lambda (input)
                    (dump-port input port)))
                #t))))))))

(define (tor-shepherd-service config)
  "Return a <shepherd-service> running Tor."
  (match config
    (($ <tor-configuration> tor)
     (let ((torrc (tor-configuration->torrc config)))
       (with-imported-modules (source-module-closure
                               '((gnu build shepherd)
                                 (gnu system file-systems)))
         (list (shepherd-service
                (provision '(tor))

                ;; Tor needs at least one network interface to be up, hence the
                ;; dependency on 'loopback'.
                (requirement '(user-processes loopback syslogd))

                (modules '((gnu build shepherd)
                           (gnu system file-systems)))

                (start #~(make-forkexec-constructor/container
                          (list #$(file-append tor "/bin/tor") "-f" #$torrc)

                          #:mappings (list (file-system-mapping
                                            (source "/var/lib/tor")
                                            (target source)
                                            (writable? #t))
                                           (file-system-mapping
                                            (source "/dev/log") ;for syslog
                                            (target source))
                                           (file-system-mapping
                                            (source "/var/run/tor")
                                            (target source)
                                            (writable? #t)))
                          #:pid-file "/var/run/tor/tor.pid"))
                (stop #~(make-kill-destructor))
                (documentation "Run the Tor anonymous network overlay."))))))))

(define (tor-activation config)
  "Set up directories for Tor and its hidden services, if any."
  #~(begin
      (use-modules (guix build utils))

      (define %user
        (getpw "tor"))

      (define (initialize service)
        (let ((directory (string-append "/var/lib/tor/hidden-services/"
                                        service)))
          (mkdir-p directory)
          (chown directory (passwd:uid %user) (passwd:gid %user))

          ;; The daemon bails out if we give wider permissions.
          (chmod directory #o700)))

      ;; Allow Tor to write its PID file.
      (mkdir-p "/var/run/tor")
      (chown "/var/run/tor" (passwd:uid %user) (passwd:gid %user))
      ;; Set the group permissions to rw so that if the system administrator
      ;; has specified UnixSocksGroupWritable=1 in their torrc file, members
      ;; of the "tor" group will be able to use the SOCKS socket.
      (chmod "/var/run/tor" #o750)

      ;; Allow Tor to access the hidden services' directories.
      (mkdir-p "/var/lib/tor")
      (chown "/var/lib/tor" (passwd:uid %user) (passwd:gid %user))
      (chmod "/var/lib/tor" #o700)

      ;; Make sure /var/lib is accessible to the 'tor' user.
      (chmod "/var/lib" #o755)

      (for-each initialize
                '#$(map hidden-service-name
                        (tor-configuration-hidden-services config)))))

(define tor-service-type
  (service-type (name 'tor)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          tor-shepherd-service)
                       (service-extension account-service-type
                                          (const %tor-accounts))
                       (service-extension activation-service-type
                                          tor-activation)))

                ;; This can be extended with hidden services.
                (compose concatenate)
                (extend (lambda (config services)
                          (tor-configuration
                           (inherit config)
                           (hidden-services
                            (append (tor-configuration-hidden-services config)
                                    services)))))
                (default-value (tor-configuration))
                (description
                 "Run the @uref{https://torproject.org, Tor} anonymous
networking daemon.")))

(define-deprecated (tor-service #:optional
                                (config-file (plain-file "empty" ""))
                                #:key (tor tor))
  tor-service-type
  "Return a service to run the @uref{https://torproject.org, Tor} anonymous
networking daemon.

The daemon runs as the @code{tor} unprivileged user.  It is passed
@var{config-file}, a file-like object, with an additional @code{User tor} line
and lines for hidden services added via @code{tor-hidden-service}.  Run
@command{man tor} for information about the configuration file."
  (service tor-service-type
           (tor-configuration (tor tor)
                              (config-file config-file))))

(define tor-hidden-service-type
  ;; A type that extends Tor with hidden services.
  (service-type (name 'tor-hidden-service)
                (extensions
                 (list (service-extension tor-service-type list)))
                (description
                 "Define a new Tor @dfn{hidden service}.")))

(define (tor-hidden-service name mapping)
  "Define a new Tor @dfn{hidden service} called @var{name} and implementing
@var{mapping}.  @var{mapping} is a list of port/host tuples, such as:

@example
 '((22 \"127.0.0.1:22\")
   (80 \"127.0.0.1:8080\"))
@end example

In this example, port 22 of the hidden service is mapped to local port 22, and
port 80 is mapped to local port 8080.

This creates a @file{/var/lib/tor/hidden-services/@var{name}} directory, where
the @file{hostname} file contains the @code{.onion} host name for the hidden
service.

See @uref{https://www.torproject.org/docs/tor-hidden-service.html.en, the Tor
project's documentation} for more information."
  (service tor-hidden-service-type
           (hidden-service name mapping)))


;;;
;;; Wicd.
;;;

(define %wicd-activation
  ;; Activation gexp for Wicd.
  #~(begin
      (use-modules (guix build utils))

      (mkdir-p "/etc/wicd")
      (let ((file-name "/etc/wicd/dhclient.conf.template.default"))
        (unless (file-exists? file-name)
          (copy-file (string-append #$wicd file-name)
                     file-name)))

      ;; Wicd invokes 'wpa_supplicant', which needs this directory for its
      ;; named socket files.
      (mkdir-p "/var/run/wpa_supplicant")
      (chmod "/var/run/wpa_supplicant" #o750)))

(define (wicd-shepherd-service wicd)
  "Return a shepherd service for WICD."
  (list (shepherd-service
         (documentation "Run the Wicd network manager.")
         (provision '(networking))
         (requirement '(user-processes dbus-system loopback))
         (start #~(make-forkexec-constructor
                   (list (string-append #$wicd "/sbin/wicd")
                         "--no-daemon")))
         (stop #~(make-kill-destructor)))))

(define wicd-service-type
  (service-type (name 'wicd)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          wicd-shepherd-service)
                       (service-extension dbus-root-service-type
                                          list)
                       (service-extension activation-service-type
                                          (const %wicd-activation))

                       ;; Add Wicd to the global profile.
                       (service-extension profile-service-type list)))
                (description
                 "Run @url{https://launchpad.net/wicd,Wicd}, a network
management daemon that aims to simplify wired and wireless networking.")))

(define* (wicd-service #:key (wicd wicd))
  "Return a service that runs @url{https://launchpad.net/wicd,Wicd}, a network
management daemon that aims to simplify wired and wireless networking.

This service adds the @var{wicd} package to the global profile, providing
several commands to interact with the daemon and configure networking:
@command{wicd-client}, a graphical user interface, and the @command{wicd-cli}
and @command{wicd-curses} user interfaces."
  (service wicd-service-type wicd))


;;;
;;; ModemManager
;;;

(define-record-type* <modem-manager-configuration>
  modem-manager-configuration make-modem-manager-configuration
  modem-manager-configuration?
  (modem-manager modem-manager-configuration-modem-manager
                   (default modem-manager)))


;;;
;;; NetworkManager
;;;

(define-record-type* <network-manager-configuration>
  network-manager-configuration make-network-manager-configuration
  network-manager-configuration?
  (network-manager network-manager-configuration-network-manager
                   (default network-manager))
  (dns network-manager-configuration-dns
       (default "default"))
  (vpn-plugins network-manager-configuration-vpn-plugins ;list of <package>
               (default '())))

(define network-manager-activation
  ;; Activation gexp for NetworkManager
  (match-lambda
    (($ <network-manager-configuration> network-manager dns vpn-plugins)
     #~(begin
         (use-modules (guix build utils))
         (mkdir-p "/etc/NetworkManager/system-connections")
         #$@(if (equal? dns "dnsmasq")
                ;; create directory to store dnsmasq lease file
                '((mkdir-p "/var/lib/misc"))
                '())))))

(define (vpn-plugin-directory plugins)
  "Return a directory containing PLUGINS, the NM VPN plugins."
  (directory-union "network-manager-vpn-plugins" plugins))

(define (network-manager-accounts config)
  "Return the list of <user-account> and <user-group> for CONFIG."
  (define nologin
    (file-append shadow "/sbin/nologin"))

  (define accounts
    (append-map (lambda (package)
                  (map (lambda (name)
                         (user-account (system? #t)
                                       (name name)
                                       (group "network-manager")
                                       (comment "NetworkManager helper")
                                       (home-directory "/var/empty")
                                       (create-home-directory? #f)
                                       (shell nologin)))
                       (or (assoc-ref (package-properties package)
                                      'user-accounts)
                           '())))
                (network-manager-configuration-vpn-plugins config)))

  (match accounts
    (()
     '())
    (_
     (cons (user-group (name "network-manager") (system? #t))
           accounts))))

(define network-manager-environment
  (match-lambda
    (($ <network-manager-configuration> network-manager dns vpn-plugins)
     ;; Define this variable in the global environment such that
     ;; "nmcli connection import type openvpn file foo.ovpn" works.
     `(("NM_VPN_PLUGIN_DIR"
        . ,(file-append (vpn-plugin-directory vpn-plugins)
                        "/lib/NetworkManager/VPN"))))))

(define network-manager-shepherd-service
  (match-lambda
    (($ <network-manager-configuration> network-manager dns vpn-plugins)
     (let ((conf (plain-file "NetworkManager.conf"
                             (string-append "[main]\ndns=" dns "\n")))
           (vpn  (vpn-plugin-directory vpn-plugins)))
       (list (shepherd-service
              (documentation "Run the NetworkManager.")
              (provision '(networking))
              (requirement '(user-processes dbus-system wpa-supplicant loopback))
              (start #~(make-forkexec-constructor
                        (list (string-append #$network-manager
                                             "/sbin/NetworkManager")
                              (string-append "--config=" #$conf)
                              "--no-daemon")
                        #:environment-variables
                        (list (string-append "NM_VPN_PLUGIN_DIR=" #$vpn
                                             "/lib/NetworkManager/VPN")
                              ;; Override non-existent default users
                              "NM_OPENVPN_USER="
                              "NM_OPENVPN_GROUP=")))
              (stop #~(make-kill-destructor))))))))

(define network-manager-service-type
  (let
      ((config->packages
        (match-lambda
         (($ <network-manager-configuration> network-manager _ vpn-plugins)
          `(,network-manager ,@vpn-plugins)))))

    (service-type
     (name 'network-manager)
     (extensions
      (list (service-extension shepherd-root-service-type
                               network-manager-shepherd-service)
            (service-extension dbus-root-service-type config->packages)
            (service-extension polkit-service-type
                               (compose
                                list
                                network-manager-configuration-network-manager))
            (service-extension account-service-type
                               network-manager-accounts)
            (service-extension activation-service-type
                               network-manager-activation)
            (service-extension session-environment-service-type
                               network-manager-environment)
            ;; Add network-manager to the system profile.
            (service-extension profile-service-type config->packages)))
     (default-value (network-manager-configuration))
     (description
      "Run @uref{https://wiki.gnome.org/Projects/NetworkManager,
NetworkManager}, a network management daemon that aims to simplify wired and
wireless networking."))))


;;;
;;; Connman
;;;

(define-record-type* <connman-configuration>
  connman-configuration make-connman-configuration
  connman-configuration?
  (connman      connman-configuration-connman
                (default connman))
  (disable-vpn? connman-configuration-disable-vpn?
                (default #f)))

(define (connman-activation config)
  (let ((disable-vpn? (connman-configuration-disable-vpn? config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (mkdir-p "/var/lib/connman/")
          (unless #$disable-vpn?
            (mkdir-p "/var/lib/connman-vpn/"))))))

(define (connman-shepherd-service config)
  "Return a shepherd service for Connman"
  (and
   (connman-configuration? config)
   (let ((connman      (connman-configuration-connman config))
         (disable-vpn? (connman-configuration-disable-vpn? config)))
     (list (shepherd-service
            (documentation "Run Connman")
            (provision '(networking))
            (requirement
             '(user-processes dbus-system loopback wpa-supplicant))
            (start #~(make-forkexec-constructor
                      (list (string-append #$connman
                                           "/sbin/connmand")
                            "--nodaemon"
                            "--nodnsproxy"
                            #$@(if disable-vpn? '("--noplugin=vpn") '()))

                      ;; As connman(8) notes, when passing '-n', connman
                      ;; "directs log output to the controlling terminal in
                      ;; addition to syslog."  Redirect stdout and stderr
                      ;; to avoid spamming the console (XXX: for some reason
                      ;; redirecting to /dev/null doesn't work.)
                      #:log-file "/var/log/connman.log"))
            (stop #~(make-kill-destructor)))))))

(define connman-service-type
  (let ((connman-package (compose list connman-configuration-connman)))
    (service-type (name 'connman)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            connman-shepherd-service)
                         (service-extension polkit-service-type
                                            connman-package)
                         (service-extension dbus-root-service-type
                                            connman-package)
                         (service-extension activation-service-type
                                            connman-activation)
                         ;; Add connman to the system profile.
                         (service-extension profile-service-type
                                            connman-package)))
                  (default-value (connman-configuration))
                  (description
                   "Run @url{https://01.org/connman,Connman},
a network connection manager."))))


;;;
;;; Modem manager
;;;

(define modem-manager-service-type
  (let ((config->package
         (match-lambda
          (($ <modem-manager-configuration> modem-manager)
           (list modem-manager)))))
    (service-type (name 'modem-manager)
                  (extensions
                   (list (service-extension dbus-root-service-type
                                            config->package)
                         (service-extension udev-service-type
                                            config->package)
                         (service-extension polkit-service-type
                                            config->package)))
                  (default-value (modem-manager-configuration))
                  (description
                   "Run @uref{https://wiki.gnome.org/Projects/ModemManager,
ModemManager}, a modem management daemon that aims to simplify dialup
networking."))))


;;;
;;; USB_ModeSwitch
;;;

(define-record-type* <usb-modeswitch-configuration>
  usb-modeswitch-configuration make-usb-modeswitch-configuration
  usb-modeswitch-configuration?
  (usb-modeswitch      usb-modeswitch-configuration-usb-modeswitch
                       (default usb-modeswitch))
  (usb-modeswitch-data usb-modeswitch-configuration-usb-modeswitch-data
                       (default usb-modeswitch-data))
  (config-file         usb-modeswitch-configuration-config-file
                       (default #~(string-append #$usb-modeswitch:dispatcher
                                                 "/etc/usb_modeswitch.conf"))))

(define (usb-modeswitch-sh usb-modeswitch config-file)
  "Build a copy of usb_modeswitch.sh located in package USB-MODESWITCH,
modified to pass the CONFIG-FILE in its calls to usb_modeswitch_dispatcher,
and wrap it to actually find the dispatcher in USB-MODESWITCH.  The script
will be run by USB_ModeSwitch’s udev rules file when a modeswitchable USB
device is detected."
  (computed-file
   "usb_modeswitch-sh"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let ((cfg-param
                #$(if config-file
                      #~(string-append " --config-file=" #$config-file)
                      "")))
           (mkdir #$output)
           (install-file (string-append #$usb-modeswitch:dispatcher
                                        "/lib/udev/usb_modeswitch")
                         #$output)

           ;; insert CFG-PARAM into usb_modeswitch_dispatcher command-lines
           (substitute* (string-append #$output "/usb_modeswitch")
             (("(exec usb_modeswitch_dispatcher .*)( 2>>)" _ left right)
              (string-append left cfg-param right))
             (("(exec usb_modeswitch_dispatcher .*)( &)" _ left right)
              (string-append left cfg-param right)))

           ;; wrap-program needs bash in PATH:
           (putenv (string-append "PATH=" #$bash "/bin"))
           (wrap-program (string-append #$output "/usb_modeswitch")
             `("PATH" ":" = (,(string-append #$coreutils "/bin")
                             ,(string-append
                               #$usb-modeswitch:dispatcher
                               "/bin")))))))))

(define (usb-modeswitch-configuration->udev-rules config)
  "Build a rules file for extending udev-service-type from the rules in the
usb-modeswitch package specified in CONFIG.  The rules file will invoke
usb_modeswitch.sh from the usb-modeswitch package, modified to pass the right
config file."
  (match config
    (($ <usb-modeswitch-configuration> usb-modeswitch data config-file)
     (computed-file
      "usb_modeswitch.rules"
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (let ((in (string-append #$data "/udev/40-usb_modeswitch.rules"))
                  (out (string-append #$output "/lib/udev/rules.d"))
                  (script #$(usb-modeswitch-sh usb-modeswitch config-file)))
              (mkdir-p out)
              (chdir out)
              (install-file in out)
              (substitute* "40-usb_modeswitch.rules"
                (("PROGRAM=\"usb_modeswitch")
                 (string-append "PROGRAM=\"" script "/usb_modeswitch"))
                (("RUN\\+=\"usb_modeswitch")
                 (string-append "RUN+=\"" script "/usb_modeswitch"))))))))))

(define usb-modeswitch-service-type
  (service-type
   (name 'usb-modeswitch)
   (extensions
    (list
     (service-extension
      udev-service-type
      (lambda (config)
        (let ((rules (usb-modeswitch-configuration->udev-rules config)))
          (list rules))))))
   (default-value (usb-modeswitch-configuration))
   (description "Run @uref{http://www.draisberghof.de/usb_modeswitch/,
USB_ModeSwitch}, a mode switching tool for controlling USB devices with
multiple @dfn{modes}.  When plugged in for the first time many USB
devices (primarily high-speed WAN modems) act like a flash storage containing
installers for Windows drivers.  USB_ModeSwitch replays the sequence the
Windows drivers would send to switch their mode from storage to modem (or
whatever the thing is supposed to do).")))


;;;
;;; WPA supplicant
;;;

(define-record-type* <wpa-supplicant-configuration>
  wpa-supplicant-configuration make-wpa-supplicant-configuration
  wpa-supplicant-configuration?
  (wpa-supplicant     wpa-supplicant-configuration-wpa-supplicant ;<package>
                      (default wpa-supplicant))
  (requirement        wpa-supplicant-configuration-requirement    ;list of symbols
                      (default '(user-processes loopback syslogd)))
  (pid-file           wpa-supplicant-configuration-pid-file       ;string
                      (default "/var/run/wpa_supplicant.pid"))
  (dbus?              wpa-supplicant-configuration-dbus?          ;Boolean
                      (default #t))
  (interface          wpa-supplicant-configuration-interface      ;#f | string
                      (default #f))
  (config-file        wpa-supplicant-configuration-config-file    ;#f | <file-like>
                      (default #f))
  (extra-options      wpa-supplicant-configuration-extra-options  ;list of strings
                      (default '())))

(define wpa-supplicant-shepherd-service
  (match-lambda
    (($ <wpa-supplicant-configuration> wpa-supplicant requirement pid-file dbus?
                                       interface config-file extra-options)
     (list (shepherd-service
            (documentation "Run the WPA supplicant daemon")
            (provision '(wpa-supplicant))
            (requirement (if dbus?
                             (cons 'dbus-system requirement)
                             requirement))
            (start #~(make-forkexec-constructor
                      (list (string-append #$wpa-supplicant
                                           "/sbin/wpa_supplicant")
                            (string-append "-P" #$pid-file)
                            "-B"        ;run in background
                            "-s"        ;log to syslogd
                            #$@(if dbus?
                                   #~("-u")
                                   #~())
                            #$@(if interface
                                   #~((string-append "-i" #$interface))
                                   #~())
                            #$@(if config-file
                                   #~((string-append "-c" #$config-file))
                                   #~())
                            #$@extra-options)
                      #:pid-file #$pid-file))
            (stop #~(make-kill-destructor)))))))

(define wpa-supplicant-service-type
  (let ((config->package
         (match-lambda
           (($ <wpa-supplicant-configuration> wpa-supplicant)
            (list wpa-supplicant)))))
    (service-type (name 'wpa-supplicant)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            wpa-supplicant-shepherd-service)
                         (service-extension dbus-root-service-type config->package)
                         (service-extension profile-service-type config->package)))
                  (description "Run the WPA Supplicant daemon, a service that
implements authentication, key negotiation and more for wireless networks.")
                  (default-value (wpa-supplicant-configuration)))))


;;;
;;; Hostapd.
;;;

(define-record-type* <hostapd-configuration>
  hostapd-configuration make-hostapd-configuration
  hostapd-configuration?
  (package           hostapd-configuration-package
                     (default hostapd))
  (interface         hostapd-configuration-interface ;string
                     (default "wlan0"))
  (ssid              hostapd-configuration-ssid)  ;string
  (broadcast-ssid?   hostapd-configuration-broadcast-ssid? ;Boolean
                     (default #t))
  (channel           hostapd-configuration-channel ;integer
                     (default 1))
  (driver            hostapd-configuration-driver ;string
                     (default "nl80211"))
  ;; See <https://w1.fi/cgit/hostap/plain/hostapd/hostapd.conf> for a list of
  ;; additional options we could add.
  (extra-settings    hostapd-configuration-extra-settings ;string
                     (default "")))

(define (hostapd-configuration-file config)
  "Return the configuration file for CONFIG, a <hostapd-configuration>."
  (match-record config <hostapd-configuration>
    (interface ssid broadcast-ssid? channel driver extra-settings)
    (plain-file "hostapd.conf"
                (string-append "\
# Generated from your Guix configuration.

interface=" interface "
ssid=" ssid "
ignore_broadcast_ssid=" (if broadcast-ssid? "0" "1") "
channel=" (number->string channel) "\n"
extra-settings "\n"))))

(define* (hostapd-shepherd-services config #:key (requirement '()))
  "Return Shepherd services for hostapd."
  (list (shepherd-service
         (provision '(hostapd))
         (requirement `(user-processes ,@requirement))
         (documentation "Run the hostapd WiFi access point daemon.")
         (start #~(make-forkexec-constructor
                   (list #$(file-append hostapd "/sbin/hostapd")
                         #$(hostapd-configuration-file config))
                   #:log-file "/var/log/hostapd.log"))
         (stop #~(make-kill-destructor)))))

(define hostapd-service-type
  (service-type
   (name 'hostapd)
   (extensions
    (list (service-extension shepherd-root-service-type
                             hostapd-shepherd-services)))
   (description
    "Run the @uref{https://w1.fi/hostapd/, hostapd} daemon for Wi-Fi access
points and authentication servers.")))

(define (simulated-wifi-shepherd-services config)
  "Return Shepherd services to run hostapd with CONFIG, a
<hostapd-configuration>, as well as services to set up WiFi hardware
simulation."
  (append (hostapd-shepherd-services config
                                     #:requirement
                                     '(unblocked-wifi
                                       kernel-module-loader))
          (list (shepherd-service
                 (provision '(unblocked-wifi))
                 (requirement '(file-systems kernel-module-loader))
                 (documentation
                  "Unblock WiFi devices for use by mac80211_hwsim.")
                 (start #~(lambda _
                            (invoke #$(file-append util-linux "/sbin/rfkill")
                                    "unblock" "0")
                            (invoke #$(file-append util-linux "/sbin/rfkill")
                                    "unblock" "1")))
                 (one-shot? #t)))))

(define simulated-wifi-service-type
  (service-type
   (name 'simulated-wifi)
   (extensions
    (list (service-extension shepherd-root-service-type
                             simulated-wifi-shepherd-services)
          (service-extension kernel-module-loader-service-type
                             (const '("mac80211_hwsim")))))
   (default-value (hostapd-configuration
                   (interface "wlan1")
                   (ssid "Test Network")))
   (description "Run hostapd to simulate WiFi connectivity.")))


;;;
;;; Open vSwitch
;;;

(define-record-type* <openvswitch-configuration>
  openvswitch-configuration make-openvswitch-configuration
  openvswitch-configuration?
  (package openvswitch-configuration-package
           (default openvswitch)))

(define openvswitch-activation
  (match-lambda
    (($ <openvswitch-configuration> package)
     (let ((ovsdb-tool (file-append package "/bin/ovsdb-tool")))
       (with-imported-modules '((guix build utils))
         #~(begin
             (use-modules (guix build utils))
             (mkdir-p "/var/run/openvswitch")
             (mkdir-p "/var/lib/openvswitch")
             (let ((conf.db "/var/lib/openvswitch/conf.db"))
               (unless (file-exists? conf.db)
                 (system* #$ovsdb-tool "create" conf.db)))))))))

(define openvswitch-shepherd-service
  (match-lambda
    (($ <openvswitch-configuration> package)
     (let ((ovsdb-server (file-append package "/sbin/ovsdb-server"))
           (ovs-vswitchd (file-append package "/sbin/ovs-vswitchd")))
       (list
        (shepherd-service
         (provision '(ovsdb))
         (documentation "Run the Open vSwitch database server.")
         (start #~(make-forkexec-constructor
                   (list #$ovsdb-server "--pidfile"
                         "--remote=punix:/var/run/openvswitch/db.sock")
                   #:pid-file "/var/run/openvswitch/ovsdb-server.pid"))
         (stop #~(make-kill-destructor)))
        (shepherd-service
         (provision '(vswitchd))
         (requirement '(ovsdb))
         (documentation "Run the Open vSwitch daemon.")
         (start #~(make-forkexec-constructor
                   (list #$ovs-vswitchd "--pidfile")
                   #:pid-file "/var/run/openvswitch/ovs-vswitchd.pid"))
         (stop #~(make-kill-destructor))))))))

(define openvswitch-service-type
  (service-type
   (name 'openvswitch)
   (extensions
    (list (service-extension activation-service-type
                             openvswitch-activation)
          (service-extension profile-service-type
                             (compose list openvswitch-configuration-package))
          (service-extension shepherd-root-service-type
                             openvswitch-shepherd-service)))
   (description
    "Run @uref{http://www.openvswitch.org, Open vSwitch}, a multilayer virtual
switch designed to enable massive network automation through programmatic
extension.")
   (default-value (openvswitch-configuration))))

;;;
;;; iptables
;;;

(define %iptables-accept-all-rules
  (plain-file "iptables-accept-all.rules"
              "*filter
:INPUT ACCEPT
:FORWARD ACCEPT
:OUTPUT ACCEPT
COMMIT
"))

(define-record-type* <iptables-configuration>
  iptables-configuration make-iptables-configuration iptables-configuration?
  (iptables iptables-configuration-iptables
            (default iptables))
  (ipv4-rules iptables-configuration-ipv4-rules
              (default %iptables-accept-all-rules))
  (ipv6-rules iptables-configuration-ipv6-rules
              (default %iptables-accept-all-rules)))

(define iptables-shepherd-service
  (match-lambda
    (($ <iptables-configuration> iptables ipv4-rules ipv6-rules)
     (let ((iptables-restore (file-append iptables "/sbin/iptables-restore"))
           (ip6tables-restore (file-append iptables "/sbin/ip6tables-restore")))
       (shepherd-service
        (documentation "Packet filtering framework")
        (provision '(iptables))
        (start #~(lambda _
                   (invoke #$iptables-restore #$ipv4-rules)
                   (invoke #$ip6tables-restore #$ipv6-rules)))
        (stop #~(lambda _
                  (invoke #$iptables-restore #$%iptables-accept-all-rules)
                  (invoke #$ip6tables-restore #$%iptables-accept-all-rules))))))))

(define iptables-service-type
  (service-type
   (name 'iptables)
   (description
    "Run @command{iptables-restore}, setting up the specified rules.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list iptables-shepherd-service))))))

;;;
;;; nftables
;;;

(define %default-nftables-ruleset
  (plain-file "nftables.conf"
              "# A simple and safe firewall
table inet filter {
  chain input {
    type filter hook input priority 0; policy drop;

    # early drop of invalid connections
    ct state invalid drop

    # allow established/related connections
    ct state { established, related } accept

    # allow from loopback
    iifname lo accept

    # allow icmp
    ip protocol icmp accept
    ip6 nexthdr icmpv6 accept

    # allow ssh
    tcp dport ssh accept

    # reject everything else
    reject with icmpx type port-unreachable
  }
  chain forward {
    type filter hook forward priority 0; policy drop;
  }
  chain output {
    type filter hook output priority 0; policy accept;
  }
}
"))

(define-record-type* <nftables-configuration>
  nftables-configuration
  make-nftables-configuration
  nftables-configuration?
  (package nftables-configuration-package
           (default nftables))
  (ruleset nftables-configuration-ruleset ; file-like object
           (default %default-nftables-ruleset)))

(define nftables-shepherd-service
  (match-lambda
    (($ <nftables-configuration> package ruleset)
     (let ((nft (file-append package "/sbin/nft")))
       (shepherd-service
        (documentation "Packet filtering and classification")
        (provision '(nftables))
        (start #~(lambda _
                   (invoke #$nft "--file" #$ruleset)))
        (stop #~(lambda _
                  (invoke #$nft "flush" "ruleset"))))))))

(define nftables-service-type
  (service-type
   (name 'nftables)
   (description
    "Run @command{nft}, setting up the specified ruleset.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list nftables-shepherd-service))
          (service-extension profile-service-type
                             (compose list nftables-configuration-package))))
   (default-value (nftables-configuration))))


;;;
;;; PageKite
;;;

(define-record-type* <pagekite-configuration>
  pagekite-configuration
  make-pagekite-configuration
  pagekite-configuration?
  (package pagekite-configuration-package
           (default pagekite))
  (kitename pagekite-configuration-kitename
            (default #f))
  (kitesecret pagekite-configuration-kitesecret
              (default #f))
  (frontend pagekite-configuration-frontend
            (default #f))
  (kites pagekite-configuration-kites
         (default '("http:@kitename:localhost:80:@kitesecret")))
  (extra-file pagekite-configuration-extra-file
              (default #f)))

(define (pagekite-configuration-file config)
  (match-record config <pagekite-configuration>
    (package kitename kitesecret frontend kites extra-file)
    (mixed-text-file "pagekite.rc"
                     (if extra-file
                         (string-append "optfile = " extra-file "\n")
                         "")
                     (if kitename
                         (string-append "kitename = " kitename "\n")
                         "")
                     (if kitesecret
                         (string-append "kitesecret = " kitesecret "\n")
                         "")
                     (if frontend
                         (string-append "frontend = " frontend "\n")
                         "defaults\n")
                     (string-join (map (lambda (kite)
                                         (string-append "service_on = " kite))
                                       kites)
                                  "\n"
                                  'suffix))))

(define (pagekite-shepherd-service config)
  (match-record config <pagekite-configuration>
    (package kitename kitesecret frontend kites extra-file)
    (with-imported-modules (source-module-closure
                            '((gnu build shepherd)
                              (gnu system file-systems)))
      (shepherd-service
       (documentation "Run the PageKite service.")
       (provision '(pagekite))
       (requirement '(networking))
       (modules '((gnu build shepherd)
                  (gnu system file-systems)))
       (start #~(make-forkexec-constructor/container
                 (list #$(file-append package "/bin/pagekite")
                       "--clean"
                       "--nullui"
                       "--nocrashreport"
                       "--runas=pagekite:pagekite"
                       (string-append "--optfile="
                                      #$(pagekite-configuration-file config)))
                 #:log-file "/var/log/pagekite.log"
                 #:mappings #$(if extra-file
                                  #~(list (file-system-mapping
                                           (source #$extra-file)
                                           (target source)))
                                  #~'())))
       ;; SIGTERM doesn't always work for some reason.
       (stop #~(make-kill-destructor SIGINT))))))

(define %pagekite-accounts
  (list (user-group (name "pagekite") (system? #t))
        (user-account
         (name "pagekite")
         (group "pagekite")
         (system? #t)
         (comment "PageKite user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define pagekite-service-type
  (service-type
   (name 'pagekite)
   (default-value (pagekite-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list pagekite-shepherd-service))
          (service-extension account-service-type
                             (const %pagekite-accounts))))
   (description
    "Run @url{https://pagekite.net/,PageKite}, a tunneling solution to make
local servers publicly accessible on the web, even behind NATs and firewalls.")))


;;;
;;; Yggdrasil
;;;

(define-record-type* <yggdrasil-configuration>
  yggdrasil-configuration
  make-yggdrasil-configuration
  yggdrasil-configuration?
  (package yggdrasil-configuration-package
           (default yggdrasil))
  (json-config yggdrasil-configuration-json-config
               (default '()))
  (config-file yggdrasil-config-file
               (default "/etc/yggdrasil-private.conf"))
  (autoconf? yggdrasil-configuration-autoconf?
             (default #f))
  (log-level yggdrasil-configuration-log-level
             (default 'info))
  (log-to yggdrasil-configuration-log-to
          (default 'stdout)))

(define (yggdrasil-configuration-file config)
  (define (scm->yggdrasil-json x)
    (define key-value?
      dotted-list?)
    (define (param->camel str)
      (string-concatenate
       (map
	string-capitalize
	(string-split str (cut eqv? <> #\-)))))
    (cond
     ((key-value? x)
      (let ((k (car x))
	    (v (cdr x)))
	(cons
	 (if (symbol? k)
	     (param->camel (symbol->string k))
	     k)
	 v)))
     ((list? x) (map scm->yggdrasil-json x))
     ((vector? x) (vector-map scm->yggdrasil-json x))
     (else x)))
  (computed-file
   "yggdrasil.conf"
   #~(call-with-output-file #$output
       (lambda (port)
         ;; it's HJSON, so comments are a-okay
         (display "# Generated by yggdrasil-service\n" port)
         (display #$(scm->json-string
                     (scm->yggdrasil-json
                      (yggdrasil-configuration-json-config config)))
                  port)))))

(define (yggdrasil-shepherd-service config)
  "Return a <shepherd-service> for yggdrasil with CONFIG."
  (define yggdrasil-command
    #~(append
       (list (string-append
              #$(yggdrasil-configuration-package config)
              "/bin/yggdrasil")
             "-useconffile"
             #$(yggdrasil-configuration-file config))
       (if #$(yggdrasil-configuration-autoconf? config)
           '("-autoconf")
           '())
       (let ((extraconf #$(yggdrasil-config-file config)))
         (if extraconf
             (list "-extraconffile" extraconf)
             '()))
       (list "-loglevel"
             #$(symbol->string
		(yggdrasil-configuration-log-level config))
             "-logto"
             #$(symbol->string
		(yggdrasil-configuration-log-to config)))))
  (list (shepherd-service
         (documentation "Connect to the Yggdrasil mesh network")
         (provision '(yggdrasil))
         (requirement '(networking))
         (start #~(make-forkexec-constructor
                   #$yggdrasil-command
                   #:log-file "/var/log/yggdrasil.log"
                   #:group "yggdrasil"))
         (stop #~(make-kill-destructor)))))

(define %yggdrasil-accounts
  (list (user-group (name "yggdrasil") (system? #t))))

(define yggdrasil-service-type
  (service-type
   (name 'yggdrasil)
   (description
    "Connect to the Yggdrasil mesh network.
See yggdrasil -genconf for config options.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             yggdrasil-shepherd-service)
          (service-extension account-service-type
                             (const %yggdrasil-accounts))
          (service-extension profile-service-type
                             (compose list yggdrasil-configuration-package))))))


;;;
;;; Keepalived
;;;

(define-record-type* <keepalived-configuration>
  keepalived-configuration make-keepalived-configuration
  keepalived-configuration?
  (keepalived  keepalived-configuration-keepalived  ;<package>
               (default keepalived))
  (config-file keepalived-configuration-config-file ;file-like
               (default #f)))

(define keepalived-shepherd-service
  (match-lambda
    (($ <keepalived-configuration> keepalived config-file)
     (list
      (shepherd-service
       (provision '(keepalived))
       (documentation "Run keepalived.")
       (requirement '(loopback))
       (start #~(make-forkexec-constructor
                 (list (string-append #$keepalived "/sbin/keepalived")
                       "--dont-fork" "--log-console" "--log-detail"
                       "--pid=/var/run/keepalived.pid"
                       (string-append "--use-file=" #$config-file))
                 #:pid-file "/var/run/keepalived.pid"
                 #:log-file "/var/log/keepalived.log"))
       (respawn? #f)
       (stop #~(make-kill-destructor)))))))

(define keepalived-service-type
  (service-type (name 'keepalived)
                (extensions (list (service-extension shepherd-root-service-type
                                                     keepalived-shepherd-service)))
                (description
                 "Run @uref{https://www.keepalived.org/, Keepalived}
routing software.")))

;;; networking.scm ends here
