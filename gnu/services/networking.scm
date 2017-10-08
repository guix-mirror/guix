;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
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
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages connman)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ntp)
  #:use-module (gnu packages wicd)
  #:use-module (gnu packages gnome)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (%facebook-host-aliases
            static-networking

            static-networking?
            static-networking-interface
            static-networking-ip
            static-networking-netmask
            static-networking-gateway

            static-networking-service
            static-networking-service-type
            dhcp-client-service
            %ntp-servers

            ntp-configuration
            ntp-configuration?
            ntp-service
            ntp-service-type

            inetd-configuration
            inetd-entry
            inetd-service-type

            tor-configuration
            tor-configuration?
            tor-hidden-service
            tor-service
            tor-service-type

            bitlbee-configuration
            bitlbee-configuration?
            bitlbee-service
            bitlbee-service-type

            wicd-service-type
            wicd-service

            network-manager-configuration
            network-manager-configuration?
            network-manager-configuration-dns
            network-manager-service-type

            connman-configuration
            connman-configuration?
            connman-service-type

            wpa-supplicant-service-type

            openvswitch-service-type
            openvswitch-configuration))

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


(define-record-type* <static-networking>
  static-networking make-static-networking
  static-networking?
  (interface static-networking-interface)
  (ip static-networking-ip)
  (netmask static-networking-netmask
           (default #f))
  (gateway static-networking-gateway              ;FIXME: doesn't belong here
           (default #f))
  (provision static-networking-provision
             (default #f))
  (name-servers static-networking-name-servers    ;FIXME: doesn't belong here
                (default '())))

(define static-networking-shepherd-service
  (match-lambda
    (($ <static-networking> interface ip netmask gateway provision
                            name-servers)
     (let ((loopback? (and provision (memq 'loopback provision))))
       (shepherd-service

        ;; Unless we're providing the loopback interface, wait for udev to be up
        ;; and running so that INTERFACE is actually usable.
        (requirement (if loopback? '() '(udev)))

        (documentation
         "Bring up the networking interface using a static IP address.")
        (provision (or provision
                       (list (symbol-append 'networking-
                                            (string->symbol interface)))))

        (start #~(lambda _
                   ;; Return #t if successfully started.
                   (let* ((addr     (inet-pton AF_INET #$ip))
                          (sockaddr (make-socket-address AF_INET addr 0))
                          (mask     (and #$netmask
                                         (inet-pton AF_INET #$netmask)))
                          (maskaddr (and mask
                                         (make-socket-address AF_INET
                                                              mask 0)))
                          (gateway  (and #$gateway
                                         (inet-pton AF_INET #$gateway)))
                          (gatewayaddr (and gateway
                                            (make-socket-address AF_INET
                                                                 gateway 0))))
                     (configure-network-interface #$interface sockaddr
                                                  (logior IFF_UP
                                                          #$(if loopback?
                                                                #~IFF_LOOPBACK
                                                                0))
                                                  #:netmask maskaddr)
                     (when gateway
                       (let ((sock (socket AF_INET SOCK_DGRAM 0)))
                         (add-network-route/gateway sock gatewayaddr)
                         (close-port sock))))))
        (stop #~(lambda _
                  ;; Return #f is successfully stopped.
                  (let ((sock (socket AF_INET SOCK_STREAM 0)))
                    (when #$gateway
                      (delete-network-route sock
                                            (make-socket-address
                                             AF_INET INADDR_ANY 0)))
                    (set-network-interface-flags sock #$interface 0)
                    (close-port sock)
                    #f)))
        (respawn? #f))))))

(define (static-networking-etc-files interfaces)
  "Return a /etc/resolv.conf entry for INTERFACES or the empty list."
  (match (delete-duplicates
          (append-map static-networking-name-servers
                      interfaces))
    (()
     '())
    ((name-servers ...)
     (let ((content (string-join
                     (map (cut string-append "nameserver " <>)
                          name-servers)
                     "\n" 'suffix)))
       `(("resolv.conf"
          ,(plain-file "resolv.conf"
                       (string-append "\
# Generated by 'static-networking-service'.\n"
                                      content))))))))

(define (static-networking-shepherd-services interfaces)
  "Return the list of Shepherd services to bring up INTERFACES, a list of
<static-networking> objects."
  (define (loopback? service)
    (memq 'loopback (shepherd-service-provision service)))

  (let ((services (map static-networking-shepherd-service interfaces)))
    (match (remove loopback? services)
      (()
       ;; There's no interface other than 'loopback', so we assume that the
       ;; 'networking' service will be provided by dhclient or similar.
       services)
      ((non-loopback ...)
       ;; Assume we're providing all the interfaces, and thus, provide a
       ;; 'networking' service.
       (cons (shepherd-service
              (provision '(networking))
              (requirement (append-map shepherd-service-provision
                                       services))
              (start #~(const #t))
              (stop #~(const #f))
              (documentation "Bring up all the networking interfaces."))
             services)))))

(define static-networking-service-type
  ;; The service type for statically-defined network interfaces.
  (service-type (name 'static-networking)
                (extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     static-networking-shepherd-services)
                  (service-extension etc-service-type
                                     static-networking-etc-files)))
                (compose concatenate)
                (extend append)
                (description
                 "Turn up the specified network interfaces upon startup,
with the given IP address, gateway, netmask, and so on.  The value for
services of this type is a list of @code{static-networking} objects, one per
network interface.")))

(define* (static-networking-service interface ip
                                    #:key
                                    netmask gateway provision
                                    (name-servers '()))
  "Return a service that starts @var{interface} with address @var{ip}.  If
@var{netmask} is true, use it as the network mask.  If @var{gateway} is true,
it must be a string specifying the default network gateway.

This procedure can be called several times, one for each network
interface of interest.  Behind the scenes what it does is extend
@code{static-networking-service-type} with additional network interfaces
to handle."
  (simple-service 'static-network-interface
                  static-networking-service-type
                  (list (static-networking (interface interface) (ip ip)
                                           (netmask netmask) (gateway gateway)
                                           (provision provision)
                                           (name-servers name-servers)))))

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
                   (negate loopback-network-interface?))
                 (define ifaces
                   (filter valid? (all-network-interface-names)))

                 ;; XXX: Make sure the interfaces are up so that 'dhclient' can
                 ;; actually send/receive over them.
                 (for-each set-network-interface-up ifaces)

                 (false-if-exception (delete-file #$pid-file))
                 (let ((pid (fork+exec-command
                             (cons* #$dhclient "-nw"
                                    "-pf" #$pid-file ifaces))))
                   (and (zero? (cdr (waitpid pid)))
                        (let loop ()
                          (catch 'system-error
                            (lambda ()
                              (call-with-input-file #$pid-file read))
                            (lambda args
                              ;; 'dhclient' returned before PID-FILE was created,
                              ;; so try again.
                              (let ((errno (system-error-errno args)))
                                (if (= ENOENT errno)
                                    (begin
                                      (sleep 1)
                                      (loop))
                                    (apply throw args))))))))))
      (stop #~(make-kill-destructor))))))

(define* (dhcp-client-service #:key (dhcp isc-dhcp))
  "Return a service that runs @var{dhcp}, a Dynamic Host Configuration
Protocol (DHCP) client, on all the non-loopback network interfaces."
  (service dhcp-client-service-type dhcp))

(define %ntp-servers
  ;; Default set of NTP servers. These URLs are managed by the NTP Pool project.
  ;; Within Guix, Leo Famulari <leo@famulari.name> is the administrative contact
  ;; for this NTP pool "zone".
  '("0.guix.pool.ntp.org"
    "1.guix.pool.ntp.org"
    "2.guix.pool.ntp.org"
    "3.guix.pool.ntp.org"))


;;;
;;; NTP.
;;;

;; TODO: Export.
(define-record-type* <ntp-configuration>
  ntp-configuration make-ntp-configuration
  ntp-configuration?
  (ntp      ntp-configuration-ntp
            (default ntp))
  (servers  ntp-configuration-servers)
  (allow-large-adjustment? ntp-allow-large-adjustment?
                           (default #f)))

(define ntp-shepherd-service
  (match-lambda
    (($ <ntp-configuration> ntp servers allow-large-adjustment?)
     (let ()
       ;; TODO: Add authentication support.
       (define config
         (string-append "driftfile /var/run/ntpd/ntp.drift\n"
                        (string-join (map (cut string-append "server " <>)
                                          servers)
                                     "\n")
                        "
# Disable status queries as a workaround for CVE-2013-5211:
# <http://support.ntp.org/bin/view/Main/SecurityNotice#DRDoS_Amplification_Attack_using>.
restrict default kod nomodify notrap nopeer noquery
restrict -6 default kod nomodify notrap nopeer noquery

# Yet, allow use of the local 'ntpq'.
restrict 127.0.0.1
restrict -6 ::1\n"))

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
              (stop #~(make-kill-destructor))))))))

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
will keep the system clock synchronized with that of the given servers.")))

(define* (ntp-service #:key (ntp ntp)
                      (servers %ntp-servers)
                      allow-large-adjustment?)
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
                    (default '())))

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
    (($ <tor-configuration> tor config-file services)
     (computed-file
      "torrc"
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils)
                         (ice-9 match))

            (call-with-output-file #$output
              (lambda (port)
                (display "\
# The beginning was automatically added.
User tor
DataDirectory /var/lib/tor
Log notice syslog\n" port)

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

                ;; Append the user's config file.
                (call-with-input-file #$config-file
                  (lambda (input)
                    (dump-port input port)))
                #t))))))))

(define (tor-shepherd-service config)
  "Return a <shepherd-service> running TOR."
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
                                            (target source)))))
                (stop #~(make-kill-destructor))
                (documentation "Run the Tor anonymous network overlay."))))))))

(define (tor-hidden-service-activation config)
  "Return the activation gexp for SERVICES, a list of hidden services."
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
                                          tor-hidden-service-activation)))

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

(define* (tor-service #:optional
                      (config-file (plain-file "empty" ""))
                      #:key (tor tor))
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
;;; BitlBee.
;;;

(define-record-type* <bitlbee-configuration>
  bitlbee-configuration make-bitlbee-configuration
  bitlbee-configuration?
  (bitlbee bitlbee-configuration-bitlbee
           (default bitlbee))
  (interface bitlbee-configuration-interface
             (default "127.0.0.1"))
  (port bitlbee-configuration-port
        (default 6667))
  (extra-settings bitlbee-configuration-extra-settings
                  (default "")))

(define bitlbee-shepherd-service
  (match-lambda
    (($ <bitlbee-configuration> bitlbee interface port extra-settings)
     (let ((conf (plain-file "bitlbee.conf"
                             (string-append "
  [settings]
  User = bitlbee
  ConfigDir = /var/lib/bitlbee
  DaemonInterface = " interface "
  DaemonPort = " (number->string port) "
" extra-settings))))

       (with-imported-modules (source-module-closure
                               '((gnu build shepherd)
                                 (gnu system file-systems)))
         (list (shepherd-service
                (provision '(bitlbee))

                ;; Note: If networking is not up, then /etc/resolv.conf
                ;; doesn't get mapped in the container, hence the dependency
                ;; on 'networking'.
                (requirement '(user-processes networking))

                (modules '((gnu build shepherd)
                           (gnu system file-systems)))
                (start #~(make-forkexec-constructor/container
                          (list #$(file-append bitlbee "/sbin/bitlbee")
                                "-n" "-F" "-u" "bitlbee" "-c" #$conf)

                          #:pid-file "/var/run/bitlbee.pid"
                          #:mappings (list (file-system-mapping
                                            (source "/var/lib/bitlbee")
                                            (target source)
                                            (writable? #t)))))
                (stop  #~(make-kill-destructor)))))))))

(define %bitlbee-accounts
  ;; User group and account to run BitlBee.
  (list (user-group (name "bitlbee") (system? #t))
        (user-account
         (name "bitlbee")
         (group "bitlbee")
         (system? #t)
         (comment "BitlBee daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define %bitlbee-activation
  ;; Activation gexp for BitlBee.
  #~(begin
      (use-modules (guix build utils))

      ;; This directory is used to store OTR data.
      (mkdir-p "/var/lib/bitlbee")
      (let ((user (getpwnam "bitlbee")))
        (chown "/var/lib/bitlbee"
               (passwd:uid user) (passwd:gid user)))))

(define bitlbee-service-type
  (service-type (name 'bitlbee)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          bitlbee-shepherd-service)
                       (service-extension account-service-type
                                          (const %bitlbee-accounts))
                       (service-extension activation-service-type
                                          (const %bitlbee-activation))))
                (default-value (bitlbee-configuration))
                (description
                 "Run @url{http://bitlbee.org,BitlBee}, a daemon that acts as
a gateway between IRC and chat networks.")))

(define* (bitlbee-service #:key (bitlbee bitlbee)
                          (interface "127.0.0.1") (port 6667)
                          (extra-settings ""))
  "Return a service that runs @url{http://bitlbee.org,BitlBee}, a daemon that
acts as a gateway between IRC and chat networks.

The daemon will listen to the interface corresponding to the IP address
specified in @var{interface}, on @var{port}.  @code{127.0.0.1} means that only
local clients can connect, whereas @code{0.0.0.0} means that connections can
come from any networking interface.

In addition, @var{extra-settings} specifies a string to append to the
configuration file."
  (service bitlbee-service-type
           (bitlbee-configuration
            (bitlbee bitlbee)
            (interface interface) (port port)
            (extra-settings extra-settings))))


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
;;; NetworkManager
;;;

(define-record-type* <network-manager-configuration>
  network-manager-configuration make-network-manager-configuration
  network-manager-configuration?
  (network-manager network-manager-configuration-network-manager
                   (default network-manager))
  (dns network-manager-configuration-dns
       (default "default"))
  (vpn-plugins network-manager-vpn-plugins        ;list of <package>
               (default '())))

(define %network-manager-activation
  ;; Activation gexp for NetworkManager.
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/etc/NetworkManager/system-connections")))

(define (vpn-plugin-directory plugins)
  "Return a directory containing PLUGINS, the NM VPN plugins."
  (directory-union "network-manager-vpn-plugins" plugins))

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
                                             "/lib/NetworkManager/VPN"))))
              (stop #~(make-kill-destructor))))))))

(define network-manager-service-type
  (let
      ((config->package
        (match-lambda
         (($ <network-manager-configuration> network-manager)
          (list network-manager)))))

    (service-type
     (name 'network-manager)
     (extensions
      (list (service-extension shepherd-root-service-type
                               network-manager-shepherd-service)
            (service-extension dbus-root-service-type config->package)
            (service-extension polkit-service-type config->package)
            (service-extension activation-service-type
                               (const %network-manager-activation))
            (service-extension session-environment-service-type
                               network-manager-environment)
            ;; Add network-manager to the system profile.
            (service-extension profile-service-type config->package)))
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
                            "-n" "-r"
                            #$@(if disable-vpn? '("--noplugin=vpn") '()))))
            (stop #~(make-kill-destructor)))))))

(define connman-service-type
  (let ((connman-package (compose list connman-configuration-connman)))
    (service-type (name 'connman)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            connman-shepherd-service)
                         (service-extension dbus-root-service-type
                                            connman-package)
                         (service-extension activation-service-type
                                            connman-activation)
                         ;; Add connman to the system profile.
                         (service-extension profile-service-type
                                            connman-package)))
                  (description
                   "Run @url{https://01.org/connman,Connman},
a network connection manager."))))


;;;
;;; WPA supplicant
;;;


(define (wpa-supplicant-shepherd-service wpa-supplicant)
  "Return a shepherd service for wpa_supplicant"
  (list (shepherd-service
         (documentation "Run WPA supplicant with dbus interface")
         (provision '(wpa-supplicant))
         (requirement '(user-processes dbus-system loopback))
         (start #~(make-forkexec-constructor
                   (list (string-append #$wpa-supplicant
                                        "/sbin/wpa_supplicant")
                         "-u" "-B" "-P/var/run/wpa_supplicant.pid")
                   #:pid-file "/var/run/wpa_supplicant.pid"))
         (stop #~(make-kill-destructor)))))

(define wpa-supplicant-service-type
  (service-type (name 'wpa-supplicant)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          wpa-supplicant-shepherd-service)
                       (service-extension dbus-root-service-type list)
                       (service-extension profile-service-type list)))
                (default-value wpa-supplicant)))


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
extension.")))

;;; networking.scm ends here
