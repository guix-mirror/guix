;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu services vpn)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages vpn)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (openvpn-client-service
            openvpn-server-service
            openvpn-client-service-type
            openvpn-server-service-type
            openvpn-client-configuration
            openvpn-server-configuration
            openvpn-remote-configuration
            openvpn-ccd-configuration
            generate-openvpn-client-documentation
            generate-openvpn-server-documentation

            wireguard-peer
            wireguard-peer?
            wireguard-peer-name
            wireguard-peer-endpoint
            wireguard-peer-allowed-ips

            wireguard-configuration
            wireguard-configuration?
            wireguard-configuration-wireguard
            wireguard-configuration-interface
            wireguard-configuration-addresses
            wireguard-configuration-port
            wireguard-configuration-private-key
            wireguard-configuration-peers

            wireguard-service-type))

;;;
;;; OpenVPN.
;;;

(define (uglify-field-name name)
  (match name
    ('verbosity "verb")
    (_ (let ((str (symbol->string name)))
         (if (string-suffix? "?" str)
             (substring str 0 (1- (string-length str)))
             str)))))

(define (serialize-field field-name val)
  (if (eq? field-name 'pid-file)
      (format #t "")
      (format #t "~a ~a\n" (uglify-field-name field-name) val)))
(define serialize-string serialize-field)
(define-maybe string)
(define (serialize-boolean field-name val)
  (if val
      (serialize-field field-name "")
      (format #t "")))

(define (ip-mask? val)
  (and (string? val)
       (if (string-match "^([0-9]+\\.){3}[0-9]+ ([0-9]+\\.){3}[0-9]+$" val)
           (let ((numbers (string-tokenize val char-set:digit)))
             (all-lte numbers (list 255 255 255 255 255 255 255 255)))
           #f)))
(define serialize-ip-mask serialize-string)

(define-syntax define-enumerated-field-type
  (lambda (x)
    (define (id-append ctx . parts)
      (datum->syntax ctx (apply symbol-append (map syntax->datum parts))))
    (syntax-case x ()
      ((_ name (option ...))
       #`(begin
           (define (#,(id-append #'name #'name #'?) x)
             (memq x '(option ...)))
           (define (#,(id-append #'name #'serialize- #'name) field-name val)
             (serialize-field field-name val)))))))

(define-enumerated-field-type proto
  (udp tcp udp6 tcp6))
(define-enumerated-field-type dev
  (tun tap))

(define key-usage? boolean?)
(define (serialize-key-usage field-name value)
  (if value
      (format #t "remote-cert-tls server\n")
      #f))

(define bind? boolean?)
(define (serialize-bind field-name value)
  (if value
      #f
      (format #t "nobind\n")))

(define resolv-retry? boolean?)
(define (serialize-resolv-retry field-name value)
  (if value
      (format #t "resolv-retry infinite\n")
      #f))

(define (serialize-tls-auth role location)
  (if location
      (serialize-field 'tls-auth
                       (string-append location " " (match role
                                                     ('server "0")
                                                     ('client "1"))))
      #f))
(define (tls-auth? val)
  (or (eq? val #f)
      (string? val)))
(define (serialize-tls-auth-server field-name val)
  (serialize-tls-auth 'server val))
(define (serialize-tls-auth-client field-name val)
  (serialize-tls-auth 'client val))
(define tls-auth-server? tls-auth?)
(define tls-auth-client? tls-auth?)

(define (serialize-number field-name val)
  (serialize-field field-name (number->string val)))

(define (all-lte left right)
  (if (eq? left '())
      (eq? right '())
      (and (<= (string->number (car left)) (car right))
           (all-lte (cdr left) (cdr right)))))

(define (cidr4? val)
  (if (string? val)
      (if (string-match "^([0-9]+\\.){3}[0-9]+/[0-9]+$" val)
          (let ((numbers (string-tokenize val char-set:digit)))
            (all-lte numbers (list 255 255 255 255 32)))
          #f)
      (eq? val #f)))

(define (cidr6? val)
  (if (string? val)
      (string-match "^([0-9a-f]{0,4}:){0,8}/[0-9]{1,3}$" val)
      (eq? val #f)))

(define (serialize-cidr4 field-name val)
  (if (eq? val #f) #f (serialize-field field-name val)))

(define (serialize-cidr6 field-name val)
  (if (eq? val #f) #f (serialize-field field-name val)))

(define (ip? val)
  (if (string? val)
      (if (string-match "^([0-9]+\\.){3}[0-9]+$" val)
          (let ((numbers (string-tokenize val char-set:digit)))
            (all-lte numbers (list 255 255 255 255)))
          #f)
      (eq? val #f)))
(define (serialize-ip field-name val)
  (if (eq? val #f) #f (serialize-field field-name val)))

(define (keepalive? val)
  (and (list? val)
       (and (number? (car val))
            (number? (car (cdr val))))))
(define (serialize-keepalive field-name val)
  (format #t "~a ~a ~a\n" (uglify-field-name field-name)
          (number->string (car val)) (number->string (car (cdr val)))))

(define gateway? boolean?)
(define (serialize-gateway field-name val)
  (and val
       (format #t "push \"redirect-gateway\"\n")))


(define-configuration openvpn-remote-configuration
  (name
   (string "my-server")
   "Server name.")
  (port
   (number 1194)
   "Port number the server listens to."))

(define-configuration openvpn-ccd-configuration
  (name
   (string "client")
   "Client name.")
  (iroute
   (ip-mask #f)
   "Client own network")
  (ifconfig-push
   (ip-mask #f)
   "Client VPN IP."))

(define (openvpn-remote-list? val)
  (and (list? val)
       (or (eq? val '())
           (and (openvpn-remote-configuration? (car val))
                (openvpn-remote-list? (cdr val))))))
(define (serialize-openvpn-remote-list field-name val)
  (for-each (lambda (remote)
              (format #t "remote ~a ~a\n" (openvpn-remote-configuration-name remote)
                      (number->string (openvpn-remote-configuration-port remote))))
            val))

(define (openvpn-ccd-list? val)
  (and (list? val)
       (or (eq? val '())
           (and (openvpn-ccd-configuration? (car val))
                (openvpn-ccd-list? (cdr val))))))
(define (serialize-openvpn-ccd-list field-name val)
  #f)

(define (create-ccd-directory val)
  "Create a ccd directory containing files for the ccd configuration option
of OpenVPN.  Each file in this directory represents particular settings for a
client.  Each file is named after the name of the client."
  (let ((files (map (lambda (ccd)
                      (list (openvpn-ccd-configuration-name ccd)
                            (with-output-to-string
                              (lambda ()
                                (serialize-configuration
                                 ccd openvpn-ccd-configuration-fields)))))
                    val)))
    (computed-file "ccd"
                   (with-imported-modules '((guix build utils))
                     #~(begin
                         (use-modules (guix build utils))
                         (use-modules (ice-9 match))
                         (mkdir-p #$output)
                         (for-each
                          (lambda (ccd)
                            (match ccd
                              ((name config-string)
                               (call-with-output-file
                                   (string-append #$output "/" name)
                                 (lambda (port) (display config-string port))))))
                          '#$files))))))

(define-syntax define-split-configuration
  (lambda (x)
    (syntax-case x ()
      ((_ name1 name2 (common-option ...) (first-option ...) (second-option ...))
       #`(begin
           (define-configuration #,#'name1
             common-option ...
             first-option ...)
           (define-configuration #,#'name2
             common-option ...
             second-option ...))))))

(define-split-configuration openvpn-client-configuration
  openvpn-server-configuration
  ((openvpn
    (package openvpn)
    "The OpenVPN package.")

   (pid-file
    (string "/var/run/openvpn/openvpn.pid")
    "The OpenVPN pid file.")

   (proto
    (proto 'udp)
    "The protocol (UDP or TCP) used to open a channel between clients and
servers.")

   (dev
    (dev 'tun)
    "The device type used to represent the VPN connection.")

   (ca
    (maybe-string "/etc/openvpn/ca.crt")
    "The certificate authority to check connections against.")

   (cert
    (maybe-string "/etc/openvpn/client.crt")
    "The certificate of the machine the daemon is running on. It should be signed
by the authority given in @code{ca}.")

   (key
    (maybe-string "/etc/openvpn/client.key")
    "The key of the machine the daemon is running on. It must be the key whose
certificate is @code{cert}.")

   (comp-lzo?
    (boolean #t)
    "Whether to use the lzo compression algorithm.")

   (persist-key?
    (boolean #t)
    "Don't re-read key files across SIGUSR1 or --ping-restart.")

   (persist-tun?
    (boolean #t)
    "Don't close and reopen TUN/TAP device or run up/down scripts across
SIGUSR1 or --ping-restart restarts.")

   (fast-io?
     (boolean #f)
     "(Experimental) Optimize TUN/TAP/UDP I/O writes by avoiding a call to
poll/epoll/select prior to the write operation.")

   (verbosity
    (number 3)
    "Verbosity level."))
  ;; client-specific configuration
  ((tls-auth
    (tls-auth-client #f)
    "Add an additional layer of HMAC authentication on top of the TLS control
channel to protect against DoS attacks.")

   (auth-user-pass
     (maybe-string 'disabled)
     "Authenticate with server using username/password.  The option is a file
containing username/password on 2 lines.  Do not use a file-like object as it
would be added to the store and readable by any user.")

   (verify-key-usage?
    (key-usage #t)
    "Whether to check the server certificate has server usage extension.")

   (bind?
    (bind #f)
    "Bind to a specific local port number.")

   (resolv-retry?
    (resolv-retry #t)
    "Retry resolving server address.")

   (remote
    (openvpn-remote-list '())
    "A list of remote servers to connect to."))
  ;; server-specific configuration
  ((tls-auth
    (tls-auth-server #f)
    "Add an additional layer of HMAC authentication on top of the TLS control
channel to protect against DoS attacks.")

   (port
    (number 1194)
    "Specifies the port number on which the server listens.")

   (server
    (ip-mask "10.8.0.0 255.255.255.0")
    "An ip and mask specifying the subnet inside the virtual network.")

   (server-ipv6
    (cidr6 #f)
    "A CIDR notation specifying the IPv6 subnet inside the virtual network.")

   (dh
    (string "/etc/openvpn/dh2048.pem")
    "The Diffie-Hellman parameters file.")

   (ifconfig-pool-persist
    (string "/etc/openvpn/ipp.txt")
    "The file that records client IPs.")

   (redirect-gateway?
    (gateway #f)
    "When true, the server will act as a gateway for its clients.")

   (client-to-client?
    (boolean #f)
    "When true, clients are allowed to talk to each other inside the VPN.")

   (keepalive
    (keepalive '(10 120))
    "Causes ping-like messages to be sent back and forth over the link so that
each side knows when the other side has gone down. @code{keepalive} requires
a pair. The first element is the period of the ping sending, and the second
element is the timeout before considering the other side down.")

   (max-clients
    (number 100)
    "The maximum number of clients.")

   (status
    (string "/var/run/openvpn/status")
    "The status file. This file shows a small report on current connection. It
is truncated and rewritten every minute.")

   (client-config-dir
    (openvpn-ccd-list '())
    "The list of configuration for some clients.")))

(define (openvpn-config-file role config)
  (let ((config-str
         (with-output-to-string
           (lambda ()
             (serialize-configuration config
                                      (match role
                                        ('server
                                         openvpn-server-configuration-fields)
                                        ('client
                                         openvpn-client-configuration-fields))))))
        (ccd-dir (match role
                   ('server (create-ccd-directory
                             (openvpn-server-configuration-client-config-dir
                              config)))
                   ('client #f))))
    (computed-file "openvpn.conf"
                   #~(begin
                       (use-modules (ice-9 match))
                       (call-with-output-file #$output
                         (lambda (port)
                           (match '#$role
                             ('server (display "" port))
                             ('client (display "client\n" port)))
                           (display #$config-str port)
                           (match '#$role
                             ('server (display
                                       (string-append "client-config-dir "
                                                      #$ccd-dir "\n") port))
                             ('client (display "" port)))))))))

(define (openvpn-shepherd-service role)
  (lambda (config)
    (let* ((config-file (openvpn-config-file role config))
           (pid-file ((match role
                        ('server openvpn-server-configuration-pid-file)
                        ('client openvpn-client-configuration-pid-file))
                      config))
           (openvpn ((match role
                       ('server openvpn-server-configuration-openvpn)
                       ('client openvpn-client-configuration-openvpn))
                     config))
           (log-file (match role
                       ('server "/var/log/openvpn-server.log")
                       ('client "/var/log/openvpn-client.log"))))
      (list (shepherd-service
             (documentation (string-append "Run the OpenVPN "
                                           (match role
                                             ('server "server")
                                             ('client "client"))
                                           " daemon."))
             (provision (match role
                          ('server '(vpn-server))
                          ('client '(vpn-client))))
             (requirement '(networking))
             (start #~(make-forkexec-constructor
                       (list (string-append #$openvpn "/sbin/openvpn")
                             "--writepid" #$pid-file "--config" #$config-file
                             "--daemon")
                       #:pid-file #$pid-file))
             (stop #~(make-kill-destructor)))))))

(define %openvpn-accounts
  (list (user-group (name "openvpn") (system? #t))
        (user-account
         (name "openvpn")
         (group "openvpn")
         (system? #t)
         (comment "Openvpn daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define %openvpn-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/run/openvpn")))

(define openvpn-server-service-type
  (service-type (name 'openvpn-server)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          (openvpn-shepherd-service 'server))
                       (service-extension account-service-type
                                          (const %openvpn-accounts))
                       (service-extension activation-service-type
                                          (const %openvpn-activation))))))

(define openvpn-client-service-type
  (service-type (name 'openvpn-client)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          (openvpn-shepherd-service 'client))
                       (service-extension account-service-type
                                          (const %openvpn-accounts))
                       (service-extension activation-service-type
                                          (const %openvpn-activation))))))

(define* (openvpn-client-service #:key (config (openvpn-client-configuration)))
  (validate-configuration config openvpn-client-configuration-fields)
  (service openvpn-client-service-type config))

(define* (openvpn-server-service #:key (config (openvpn-server-configuration)))
  (validate-configuration config openvpn-server-configuration-fields)
  (service openvpn-server-service-type config))

(define (generate-openvpn-server-documentation)
  (generate-documentation
   `((openvpn-server-configuration
      ,openvpn-server-configuration-fields
      (ccd openvpn-ccd-configuration))
     (openvpn-ccd-configuration ,openvpn-ccd-configuration-fields))
   'openvpn-server-configuration))

(define (generate-openvpn-client-documentation)
  (generate-documentation
   `((openvpn-client-configuration
      ,openvpn-client-configuration-fields
      (remote openvpn-remote-configuration))
     (openvpn-remote-configuration ,openvpn-remote-configuration-fields))
   'openvpn-client-configuration))


;;;
;;; Wireguard.
;;;

(define-record-type* <wireguard-peer>
  wireguard-peer make-wireguard-peer
  wireguard-peer?
  (name              wireguard-peer-name)
  (endpoint          wireguard-peer-endpoint
                     (default #f))     ;string
  (public-key        wireguard-peer-public-key)   ;string
  (allowed-ips       wireguard-peer-allowed-ips)) ;list of strings

(define-record-type* <wireguard-configuration>
  wireguard-configuration make-wireguard-configuration
  wireguard-configuration?
  (wireguard          wireguard-configuration-wireguard ;<package>
                      (default wireguard-tools))
  (interface          wireguard-configuration-interface ;string
                      (default "wg0"))
  (addresses          wireguard-configuration-addresses ;string
                      (default '("10.0.0.1/32")))
  (port               wireguard-configuration-port ;integer
                      (default 51820))
  (private-key        wireguard-configuration-private-key ;string
                      (default "/etc/wireguard/private.key"))
  (peers              wireguard-configuration-peers ;list of <wiregard-peer>
                      (default '())))

(define (wireguard-configuration-file config)
  (define (peer->config peer)
    (let ((name (wireguard-peer-name peer))
          (public-key (wireguard-peer-public-key peer))
          (endpoint (wireguard-peer-endpoint peer))
          (allowed-ips (wireguard-peer-allowed-ips peer)))
      (format #f "[Peer] #~a
PublicKey = ~a
AllowedIPs = ~a
~a"
              name
              public-key
              (string-join allowed-ips ",")
              (if endpoint
                  (format #f "Endpoint = ~a\n" endpoint)
                  "\n"))))

  (match-record config <wireguard-configuration>
    (wireguard interface addresses port private-key peers)
    (let* ((config-file (string-append interface ".conf"))
           (peers (map peer->config peers))
           (config
            (computed-file
             "wireguard-config"
             #~(begin
                 (mkdir #$output)
                 (chdir #$output)
                 (call-with-output-file #$config-file
                   (lambda (port)
                     (let ((format (@ (ice-9 format) format)))
                       (format port "[Interface]
Address = ~a
PostUp = ~a set %i private-key ~a
~a
~{~a~^~%~}"
                               #$(string-join addresses ",")
                               #$(file-append wireguard "/bin/wg")
                               #$private-key
                               #$(if port
                                     (format #f "ListenPort = ~a" port)
                                     "")
                               (list #$@peers)))))))))
      (file-append config "/" config-file))))

(define (wireguard-activation config)
  (match-record config <wireguard-configuration>
    (private-key)
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 popen)
                     (ice-9 rdelim))
        (mkdir-p (dirname #$private-key))
        (unless (file-exists? #$private-key)
          (let* ((pipe
                  (open-input-pipe (string-append
                                    #$(file-append wireguard-tools "/bin/wg")
                                    " genkey")))
                 (key (read-line pipe)))
            (call-with-output-file #$private-key
              (lambda (port)
                (display key port)))
            (chmod #$private-key #o400)
            (close-pipe pipe))))))

(define (wireguard-shepherd-service config)
  (match-record config <wireguard-configuration>
    (wireguard interface)
    (let ((wg-quick (file-append wireguard "/bin/wg-quick"))
          (config (wireguard-configuration-file config)))
      (list (shepherd-service
             (requirement '(networking))
             (provision (list
                         (symbol-append 'wireguard-
                                        (string->symbol interface))))
             (start #~(lambda _
                       (invoke #$wg-quick "up" #$config)))
             (stop #~(lambda _
                       (invoke #$wg-quick "down" #$config)))
             (documentation "Run the Wireguard VPN tunnel"))))))

(define wireguard-service-type
  (service-type
   (name 'wireguard)
   (extensions
    (list (service-extension shepherd-root-service-type
                             wireguard-shepherd-service)
          (service-extension activation-service-type
                             wireguard-activation)))))
