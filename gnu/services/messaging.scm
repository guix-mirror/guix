;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2015, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
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

(define-module (gnu services messaging)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages tls)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (prosody-service-type
            prosody-configuration
            opaque-prosody-configuration

            virtualhost-configuration
            int-component-configuration
            ext-component-configuration

            mod-muc-configuration
            ssl-configuration

            %default-modules-enabled
            prosody-configuration-pidfile

            bitlbee-configuration
            bitlbee-configuration?
            bitlbee-service
            bitlbee-service-type

            quassel-configuration
            quassel-service-type))

;;; Commentary:
;;;
;;; Messaging services.
;;;
;;; Code:

(define-syntax define-all-configurations
  (lambda (stx)
    (define-syntax-rule (id ctx parts ...)
      "Assemble PARTS into a raw (unhygienic) identifier."
      (datum->syntax ctx (symbol-append (syntax->datum parts) ...)))
    (define (make-pred arg)
      (lambda (field target)
        (and (memq (syntax->datum target) `(common ,arg)) field)))
    (syntax-case stx ()
      ((_ stem (field (field-type def) doc target) ...)
       (with-syntax (((new-field-type ...)
                      (map (lambda (field-type target)
                             (if (and (eq? 'common (syntax->datum target))
                                      (not (string-prefix?
                                            "maybe-"
                                            (symbol->string
                                             (syntax->datum field-type)))))
                                 (id #'stem #'maybe- field-type) field-type))
                           #'(field-type ...) #'(target ...)))
                     ((new-def ...)
                      (map (lambda (def target)
                             (if (eq? 'common (syntax->datum target))
                                 #''disabled def))
                           #'(def ...) #'(target ...)))
                     ((new-doc ...)
                      (map (lambda (doc target)
                             (if (eq? 'common (syntax->datum target))
                                 "" doc))
                           #'(doc ...) #'(target ...))))
         #`(begin
             (define #,(id #'stem #'common-fields)
               '(#,@(filter-map (make-pred #f) #'(field ...) #'(target ...))))
             (define-configuration #,(id #'stem #'prosody-configuration)
               #,@(filter-map (make-pred 'global)
                              #'((field (field-type def) doc) ...)
                              #'(target ...)))
             (define-configuration #,(id #'stem #'virtualhost-configuration)
               #,@(filter-map (make-pred 'virtualhost)
                              #'((field (new-field-type new-def) new-doc) ...)
                              #'(target ...)))
             (define-configuration #,(id #'stem #'int-component-configuration)
               #,@(filter-map (make-pred 'int-component)
                              #'((field (new-field-type new-def) new-doc) ...)
                              #'(target ...)))
             (define-configuration #,(id #'stem #'ext-component-configuration)
               #,@(filter-map (make-pred 'ext-component)
                              #'((field (new-field-type new-def) new-doc) ...)
                              #'(target ...)))))))))

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-join (string-split (if (string-suffix? "?" str)
                                   (substring str 0 (1- (string-length str)))
                                   str)
                               #\-)
                 "_")))

(define (serialize-field field-name val)
  #~(format #f "~a = ~a;\n" #$(uglify-field-name field-name) #$val))
(define (serialize-field-list field-name val)
  (serialize-field field-name #~(format #f "{\n~@{~a;\n~}}" #$@val)))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val "true" "false")))
(define-maybe boolean)

(define (string-or-boolean? val)
  (or (string? val) (boolean? val)))
(define (serialize-string-or-boolean field-name val)
  (if (string? val)
      (serialize-string field-name val)
      (serialize-boolean field-name val)))

(define (non-negative-integer? val)
  (and (exact-integer? val) (not (negative? val))))
(define (serialize-non-negative-integer field-name val)
  (serialize-field field-name (number->string val)))
(define-maybe non-negative-integer)

(define (non-negative-integer-list? val)
  (and (list? val) (and-map non-negative-integer? val)))
(define (serialize-non-negative-integer-list field-name val)
  (serialize-field-list field-name (map number->string val)))
(define-maybe non-negative-integer-list)

(define (enclose-quotes s)
  #~(string-append "\"" #$s "\""))
(define (serialize-string field-name val)
  (serialize-field field-name (enclose-quotes val)))
(define-maybe string)

(define (string-list? val)
  (and (list? val)
       (and-map (lambda (x)
                  (and (string? x) (not (string-index x #\,))))
                val)))
(define (serialize-string-list field-name val)
  (serialize-field-list field-name (map enclose-quotes val)))
(define-maybe string-list)

(define (module-list? val)
  (string-list? val))
(define (serialize-module-list field-name val)
  (serialize-string-list field-name val))
(define-maybe module-list)

(define (file-name? val)
  (and (string? val)
       (string-prefix? "/" val)))
(define (serialize-file-name field-name val)
  (serialize-string field-name val))
(define-maybe file-name)

(define (file-name-list? val)
  (and (list? val) (and-map file-name? val)))
(define (serialize-file-name-list field-name val)
  (serialize-string-list field-name val))
(define-maybe file-name)

(define (file-object? val)
  (or (file-like? val) (file-name? val)))
(define (serialize-file-object field-name val)
  (serialize-string field-name val))
(define-maybe file-object)

(define (file-object-list? val)
  (and (list? val) (and-map file-object? val)))
(define (serialize-file-object-list field-name val)
  (serialize-string-list field-name val))
(define-maybe file-object)

(define (raw-content? val)
  (not (eq? val 'disabled)))
(define (serialize-raw-content field-name val)
  val)
(define-maybe raw-content)

(define-configuration mod-muc-configuration
  (name
   (string "Prosody Chatrooms")
   "The name to return in service discovery responses.")

  (restrict-room-creation
   (string-or-boolean #f)
   "If @samp{#t}, this will only allow admins to create new chatrooms.
Otherwise anyone can create a room.  The value @samp{\"local\"} restricts room
creation to users on the service's parent domain.  E.g. @samp{user@@example.com}
can create rooms on @samp{rooms.example.com}.  The value @samp{\"admin\"}
restricts to service administrators only.")

  (max-history-messages
   (non-negative-integer 20)
   "Maximum number of history messages that will be sent to the member that has
just joined the room."))
(define (serialize-mod-muc-configuration field-name val)
  (serialize-configuration val mod-muc-configuration-fields))
(define-maybe mod-muc-configuration)

(define-configuration ssl-configuration
  (protocol
   (maybe-string 'disabled)
   "This determines what handshake to use.")

  (key
   (maybe-file-name 'disabled)
   "Path to your private key file.")

  (certificate
   (maybe-file-name 'disabled)
   "Path to your certificate file.")

  (capath
   (file-object "/etc/ssl/certs")
   "Path to directory containing root certificates that you wish Prosody to
trust when verifying the certificates of remote servers.")

  (cafile
   (maybe-file-object 'disabled)
   "Path to a file containing root certificates that you wish Prosody to trust.
Similar to @code{capath} but with all certificates concatenated together.")

  (verify
   (maybe-string-list 'disabled)
   "A list of verification options (these mostly map to OpenSSL's
@code{set_verify()} flags).")

  (options
   (maybe-string-list 'disabled)
   "A list of general options relating to SSL/TLS.  These map to OpenSSL's
@code{set_options()}.  For a full list of options available in LuaSec, see the
LuaSec source.")

  (depth
   (maybe-non-negative-integer 'disabled)
   "How long a chain of certificate authorities to check when looking for a
trusted root certificate.")

  (ciphers
   (maybe-string 'disabled)
   "An OpenSSL cipher string.  This selects what ciphers Prosody will offer to
clients, and in what order.")

  (dhparam
   (maybe-file-name 'disabled)
   "A path to a file containing parameters for Diffie-Hellman key exchange.  You
can create such a file with:
@code{openssl dhparam -out /etc/prosody/certs/dh-2048.pem 2048}")

  (curve
   (maybe-string 'disabled)
   "Curve for Elliptic curve Diffie-Hellman. Prosody's default is
@samp{\"secp384r1\"}.")

  (verifyext
   (maybe-string-list 'disabled)
   "A list of \"extra\" verification options.")

  (password
   (maybe-string 'disabled)
   "Password for encrypted private keys."))
(define (serialize-ssl-configuration field-name val)
  #~(format #f "ssl = {\n~a};\n"
            #$(serialize-configuration val ssl-configuration-fields)))
(define-maybe ssl-configuration)

(define %default-modules-enabled
  '("roster"
    "saslauth"
    "tls"
    "dialback"
    "disco"
    "carbons"
    "private"
    "blocklist"
    "vcard"
    "version"
    "uptime"
    "time"
    "ping"
    "pep"
    "register"
    "admin_adhoc"))

;; Guile bug.  Use begin wrapper, because otherwise virtualhost-configuration
;; is assumed to be a function.  See
;; https://www.gnu.org/software/guile/manual/html_node/R6RS-Incompatibilities.html
(begin
  (define (virtualhost-configuration-list? val)
    (and (list? val) (and-map virtualhost-configuration? val)))
  (define (serialize-virtualhost-configuration-list l)
    #~(string-append
       #$@(map (lambda (val)
                 (serialize-virtualhost-configuration val)) l)))

  (define (int-component-configuration-list? val)
    (and (list? val) (and-map int-component-configuration? val)))
  (define (serialize-int-component-configuration-list l)
    #~(string-append
       #$@(map (lambda (val)
                 (serialize-int-component-configuration val)) l)))

  (define (ext-component-configuration-list? val)
    (and (list? val) (and-map ext-component-configuration? val)))
  (define (serialize-ext-component-configuration-list l)
    #~(string-append
       #$@(map (lambda (val)
                 (serialize-ext-component-configuration val)) l)))

  (define-all-configurations prosody-configuration
    (prosody
     (package prosody)
     "The Prosody package."
     global)

    (data-path
     (file-name "/var/lib/prosody")
     "Location of the Prosody data storage directory.  See
@url{https://prosody.im/doc/configure}."
     global)

    (plugin-paths
     (file-object-list '())
     "Additional plugin directories.  They are searched in all the specified
paths in order.  See @url{https://prosody.im/doc/plugins_directory}."
     global)

    (certificates
     (file-name "/etc/prosody/certs")
     "Every virtual host and component needs a certificate so that clients and
servers can securely verify its identity.  Prosody will automatically load
certificates/keys from the directory specified here."
     global)

    (admins
     (string-list '())
     "This is a list of accounts that are admins for the server.  Note that you
must create the accounts separately.  See @url{https://prosody.im/doc/admins} and
@url{https://prosody.im/doc/creating_accounts}.
Example: @code{(admins '(\"user1@@example.com\" \"user2@@example.net\"))}"
     common)

    (use-libevent?
     (boolean #f)
     "Enable use of libevent for better performance under high load.  See
@url{https://prosody.im/doc/libevent}."
     common)

    (modules-enabled
     (module-list %default-modules-enabled)
     "This is the list of modules Prosody will load on startup.  It looks for
@code{mod_modulename.lua} in the plugins folder, so make sure that exists too.
Documentation on modules can be found at:
@url{https://prosody.im/doc/modules}."
     common)

    (modules-disabled
     (string-list '())
     "@samp{\"offline\"}, @samp{\"c2s\"} and @samp{\"s2s\"} are auto-loaded, but
should you want to disable them then add them to this list."
     common)

    (groups-file
     (file-object "/var/lib/prosody/sharedgroups.txt")
     "Path to a text file where the shared groups are defined.  If this path is
empty then @samp{mod_groups} does nothing.  See
@url{https://prosody.im/doc/modules/mod_groups}."
     common)

    (allow-registration?
     (boolean #f)
     "Disable account creation by default, for security.  See
@url{https://prosody.im/doc/creating_accounts}."
     common)

    (ssl
     (maybe-ssl-configuration (ssl-configuration))
     "These are the SSL/TLS-related settings.  Most of them are disabled so to
use Prosody's defaults.  If you do not completely understand these options, do
not add them to your config, it is easy to lower the security of your server
using them.  See @url{https://prosody.im/doc/advanced_ssl_config}."
     common)

    (c2s-require-encryption?
     (boolean #f)
     "Whether to force all client-to-server connections to be encrypted or not.
See @url{https://prosody.im/doc/modules/mod_tls}."
     common)

    (disable-sasl-mechanisms
     (string-list '("DIGEST-MD5"))
     "Set of mechanisms that will never be offered.  See
@url{https://prosody.im/doc/modules/mod_saslauth}."
     common)

    (s2s-require-encryption?
     (boolean #f)
     "Whether to force all server-to-server connections to be encrypted or not.
See @url{https://prosody.im/doc/modules/mod_tls}."
     common)

    (s2s-secure-auth?
     (boolean #f)
     "Whether to require encryption and certificate authentication.  This
provides ideal security, but requires servers you communicate with to support
encryption AND present valid, trusted certificates.  See
@url{https://prosody.im/doc/s2s#security}."
     common)

    (s2s-insecure-domains
     (string-list '())
     "Many servers don't support encryption or have invalid or self-signed
certificates.  You can list domains here that will not be required to
authenticate using certificates.  They will be authenticated using DNS.  See
@url{https://prosody.im/doc/s2s#security}."
     common)

    (s2s-secure-domains
     (string-list '())
     "Even if you leave @code{s2s-secure-auth?} disabled, you can still require
valid certificates for some domains by specifying a list here.  See
@url{https://prosody.im/doc/s2s#security}."
     common)

    (authentication
     (string "internal_plain")
     "Select the authentication backend to use.  The default provider stores
passwords in plaintext and uses Prosody's configured data storage to store the
authentication data.  If you do not trust your server please see
@url{https://prosody.im/doc/modules/mod_auth_internal_hashed} for information
about using the hashed backend.  See also
@url{https://prosody.im/doc/authentication}"
     common)

    ;; TODO: Handle more complicated log structures.
    (log
     (maybe-string "*syslog")
     "Set logging options.  Advanced logging configuration is not yet supported
by the Prosody service.  See @url{https://prosody.im/doc/logging}."
     common)

    (pidfile
     (file-name "/var/run/prosody/prosody.pid")
     "File to write pid in.  See @url{https://prosody.im/doc/modules/mod_posix}."
     global)

    (http-max-content-size
     (maybe-non-negative-integer 'disabled)
     "Maximum allowed size of the HTTP body (in bytes)."
     common)

    (http-external-url
     (maybe-string 'disabled)
     "Some modules expose their own URL in various ways.  This URL is built
from the protocol, host and port used.  If Prosody sits behind a proxy, the
public URL will be @code{http-external-url} instead.  See
@url{https://prosody.im/doc/http#external_url}."
     common)

    (virtualhosts
     (virtualhost-configuration-list
      (list (virtualhost-configuration
             (domain "localhost"))))
     "A host in Prosody is a domain on which user accounts can be created.  For
example if you want your users to have addresses like
@samp{\"john.smith@@example.com\"} then you need to add a host
@samp{\"example.com\"}.  All options in this list will apply only to this host.

Note: the name \"virtual\" host is used in configuration to avoid confusion with
the actual physical host that Prosody is installed on.  A single Prosody
instance can serve many domains, each one defined as a VirtualHost entry in
Prosody's configuration.  Conversely a server that hosts a single domain would
have just one VirtualHost entry.

See @url{https://prosody.im/doc/configure#virtual_host_settings}."
     global)

    (int-components
     (int-component-configuration-list '())
     "Components are extra services on a server which are available to clients,
usually on a subdomain of the main server (such as
@samp{\"mycomponent.example.com\"}).  Example components might be chatroom
servers, user directories, or gateways to other protocols.

Internal components are implemented with Prosody-specific plugins.  To add an
internal component, you simply fill the hostname field, and the plugin you wish
to use for the component.

See @url{https://prosody.im/doc/components}."
     global)

    (ext-components
     (ext-component-configuration-list '())
     "External components use XEP-0114, which most standalone components
support.  To add an external component, you simply fill the hostname field.  See
@url{https://prosody.im/doc/components}."
     global)

    (component-secret
     (string (configuration-missing-field 'ext-component 'component-secret))
     "Password which the component will use to log in."
     ext-component)

    (component-ports
     (non-negative-integer-list '(5347))
     "Port(s) Prosody listens on for component connections."
     global)

    (component-interface
     (string "127.0.0.1")
     "Interface Prosody listens on for component connections."
     global)

    (domain
     (string (configuration-missing-field 'virtualhost 'domain))
     "Domain you wish Prosody to serve."
     virtualhost)

    (hostname
     (string (configuration-missing-field 'int-component 'hostname))
     "Hostname of the component."
     int-component)

    (plugin
     (string (configuration-missing-field 'int-component 'plugin))
     "Plugin you wish to use for the component."
     int-component)

    (mod-muc
     (maybe-mod-muc-configuration 'disabled)
     "Multi-user chat (MUC) is Prosody's module for allowing you to create
hosted chatrooms/conferences for XMPP users.

General information on setting up and using multi-user chatrooms can be found
in the \"Chatrooms\" documentation (@url{https://prosody.im/doc/chatrooms}),
which you should read if you are new to XMPP chatrooms.

See also @url{https://prosody.im/doc/modules/mod_muc}."
     int-component)

    (hostname
     (string (configuration-missing-field 'ext-component 'hostname))
     "Hostname of the component."
     ext-component)

    (raw-content
     (maybe-raw-content 'disabled)
     "Raw content that will be added to the configuration file."
     common)))

;; Serialize Virtualhost line first.
(define (serialize-virtualhost-configuration config)
  (define (rest? field)
    (not (memq (configuration-field-name field)
               '(domain))))
  (let ((domain (virtualhost-configuration-domain config))
        (rest (filter rest? virtualhost-configuration-fields)))
    #~(string-append
       #$(format #f "VirtualHost \"~a\"\n" domain)
       #$(serialize-configuration config rest))))

;; Serialize Component line first.
(define (serialize-int-component-configuration config)
  (define (rest? field)
    (not (memq (configuration-field-name field)
               '(hostname plugin))))
  (let ((hostname (int-component-configuration-hostname config))
        (plugin (int-component-configuration-plugin config))
        (rest (filter rest? int-component-configuration-fields)))
    #~(string-append
       #$(format #f "Component \"~a\" \"~a\"\n" hostname plugin)
       #$(serialize-configuration config rest))))

;; Serialize Component line first.
(define (serialize-ext-component-configuration config)
  (define (rest? field)
    (not (memq (configuration-field-name field)
               '(hostname))))
  (let ((hostname (ext-component-configuration-hostname config))
        (rest (filter rest? ext-component-configuration-fields)))
    #~(string-append
       #$(format #f "Component \"~a\"\n" hostname)
       #$(serialize-configuration config rest))))

;; Serialize virtualhosts and components last.
(define (serialize-prosody-configuration config)
  (define (rest? field)
    (not (memq (configuration-field-name field)
               '(virtualhosts int-components ext-components))))
  #~(string-append
     #$(let ((rest (filter rest? prosody-configuration-fields)))
         (serialize-configuration config rest))
     #$(serialize-virtualhost-configuration-list
        (prosody-configuration-virtualhosts config))
     #$(serialize-int-component-configuration-list
        (prosody-configuration-int-components config))
     #$(serialize-ext-component-configuration-list
        (prosody-configuration-ext-components config))))

(define-configuration opaque-prosody-configuration
  (prosody
   (package prosody)
   "The prosody package.")

  (prosody.cfg.lua
   (string (configuration-missing-field 'opaque-prosody-configuration
                                        'prosody.cfg.lua))
   "The contents of the @code{prosody.cfg.lua} to use."))

(define (prosody-shepherd-service config)
  "Return a <shepherd-service> for Prosody with CONFIG."
  (let* ((prosody (if (opaque-prosody-configuration? config)
                      (opaque-prosody-configuration-prosody config)
                      (prosody-configuration-prosody config)))
         (prosodyctl-bin (file-append prosody "/bin/prosodyctl"))
         (pid-file (prosody-configuration-pidfile config))
         (prosodyctl-action (lambda args
                              #~(lambda _
                                  (invoke #$prosodyctl-bin #$@args)
                                  (match '#$args
                                    (("start")
                                     (call-with-input-file #$pid-file read))
                                    (_ #t))))))
    (list (shepherd-service
           (documentation "Run the Prosody XMPP server")
           (provision '(prosody xmpp-daemon))
           (requirement '(networking syslogd user-processes))
           (modules `((ice-9 match)
                      ,@%default-modules))
           (start (prosodyctl-action "start"))
           (stop (prosodyctl-action "stop"))))))

(define %prosody-accounts
  (list (user-group (name "prosody") (system? #t))
        (user-account
         (name "prosody")
         (group "prosody")
         (system? #t)
         (comment "Prosody daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (prosody-activation config)
  "Return the activation gexp for CONFIG."
  (let* ((config-dir "/etc/prosody")
         (default-certs-dir "/etc/prosody/certs")
         (data-path (prosody-configuration-data-path config))
         (pidfile-dir (dirname (prosody-configuration-pidfile config)))
         (config-str (if (opaque-prosody-configuration? config)
                         (opaque-prosody-configuration-prosody.cfg.lua config)
                         #~(begin
                             (use-modules (ice-9 format))
                             #$(serialize-prosody-configuration config))))
         (config-file (mixed-text-file "prosody.cfg.lua" config-str)))
    #~(begin
        (use-modules (guix build utils))
        (define %user (getpw "prosody"))

        (mkdir-p #$config-dir)
        (chown #$config-dir (passwd:uid %user) (passwd:gid %user))
        (copy-file #$config-file (string-append #$config-dir
                                                "/prosody.cfg.lua"))

        (mkdir-p #$default-certs-dir)
        (chown #$default-certs-dir (passwd:uid %user) (passwd:gid %user))
        (chmod #$default-certs-dir #o750)

        (mkdir-p #$data-path)
        (chown #$data-path (passwd:uid %user) (passwd:gid %user))
        (chmod #$data-path #o750)

        (mkdir-p #$pidfile-dir)
        (chown #$pidfile-dir (passwd:uid %user) (passwd:gid %user)))))

(define prosody-service-type
  (service-type (name 'prosody)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          prosody-shepherd-service)
                       (service-extension account-service-type
                                          (const %prosody-accounts))
                       (service-extension activation-service-type
                                          prosody-activation)))
                (default-value (prosody-configuration
                                (virtualhosts
                                 (list
                                  (virtualhost-configuration
                                   (domain "localhost"))))))
                (description
                 "Run Prosody, a modern XMPP communication server.")))

;; A little helper to make it easier to document all those fields.
(define (generate-documentation)
  (define documentation
    `((prosody-configuration
       ,prosody-configuration-fields
       (ssl ssl-configuration)
       (virtualhosts virtualhost-configuration)
       (int-components int-component-configuration)
       (ext-components ext-component-configuration))
      (ssl-configuration ,ssl-configuration-fields)
      (int-component-configuration ,int-component-configuration-fields
                                   (mod-muc mod-muc-configuration))
      (ext-component-configuration ,ext-component-configuration-fields)
      (mod-muc-configuration ,mod-muc-configuration-fields)
      (virtualhost-configuration ,virtualhost-configuration-fields)
      (opaque-prosody-configuration ,opaque-prosody-configuration-fields)))
  (define (generate configuration-name)
    (match (assq-ref documentation configuration-name)
      ((fields . sub-documentation)
       (format #t "\nAvailable @code{~a} fields are:\n\n" configuration-name)
       (when (memq configuration-name
                   '(virtualhost-configuration
                     int-component-configuration
                     ext-component-configuration))
         (format #t "all these @code{prosody-configuration} fields: ~a, plus:\n"
                 (string-join (map (lambda (s)
                                     (format #f "@code{~a}" s)) common-fields)
                              ", ")))
       (for-each
        (lambda (f)
          (let ((field-name (configuration-field-name f))
                (field-type (configuration-field-type f))
                (field-docs (string-trim-both
                             (configuration-field-documentation f)))
                (default (catch #t
                           (configuration-field-default-value-thunk f)
                           (lambda _ 'nope))))
            (define (escape-chars str chars escape)
              (with-output-to-string
                (lambda ()
                  (string-for-each (lambda (c)
                                     (when (char-set-contains? chars c)
                                       (display escape))
                                     (display c))
                                   str))))
            (define (show-default? val)
              (or (string? val) (number? val) (boolean? val)
                  (and (list? val) (and-map show-default? val))))
            (format #t "@deftypevr {@code{~a} parameter} ~a ~a\n~a\n"
                    configuration-name field-type field-name field-docs)
            (when (show-default? default)
              (format #t "Defaults to @samp{~a}.\n"
                      (escape-chars (format #f "~s" default)
                                    (char-set #\@ #\{ #\})
                                    #\@)))
            (for-each generate (or (assq-ref sub-documentation field-name) '()))
            (format #t "@end deftypevr\n\n")))
        (filter (lambda (f)
                  (not (string=? "" (configuration-field-documentation f))))
                fields)))))
  (generate 'prosody-configuration)
  (format #t "It could be that you just want to get a @code{prosody.cfg.lua}
up and running.  In that case, you can pass an
@code{opaque-prosody-configuration} record as the value of
@code{prosody-service-type}.  As its name indicates, an opaque configuration
does not have easy reflective capabilities.")
  (generate 'opaque-prosody-configuration)
  (format #t "For example, if your @code{prosody.cfg.lua} is just the empty
string, you could instantiate a prosody service like this:

@example
(service prosody-service-type
         (opaque-prosody-configuration
          (prosody.cfg.lua \"\")))
@end example"))


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
  (plugins bitlbee-plugins
           (default '()))
  (extra-settings bitlbee-configuration-extra-settings
                  (default "")))

(define bitlbee-shepherd-service
  (match-lambda
    (($ <bitlbee-configuration> bitlbee interface port
                                plugins extra-settings)
     (let ((conf (mixed-text-file "bitlbee.conf"
                                  "
  [settings]
  User = bitlbee
  ConfigDir = /var/lib/bitlbee
  DaemonInterface = " interface "
  DaemonPort = " (number->string port) "
  PluginDir = " (directory-union "bitlbee-plugins" plugins) "/lib/bitlbee
" extra-settings)))

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

(define-deprecated (bitlbee-service #:key (bitlbee bitlbee)
                                    (interface "127.0.0.1") (port 6667)
                                    (extra-settings ""))
  bitlbee-service-type
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
;;; Quassel.
;;;

(define-record-type* <quassel-configuration>
  quassel-configuration make-quassel-configuration
  quassel-configuration?
  (quassel quassel-configuration-quassel
           (default quassel))
  (interface quassel-configuration-interface
             (default "::,0.0.0.0"))
  (port quassel-configuration-port
        (default 4242))
  (loglevel quassel-configuration-loglevel
            (default "Info")))

(define quassel-shepherd-service
  (match-lambda
    (($ <quassel-configuration> quassel interface port loglevel)
     (with-imported-modules (source-module-closure
                              '((gnu build shepherd)
                                (gnu system file-systems)))
       (list (shepherd-service
               (provision '(quassel))
               (requirement '(user-processes networking))
               (modules '((gnu build shepherd)
                          (gnu system file-systems)))
               (start #~(make-forkexec-constructor/container
                          (list #$(file-append quassel "/bin/quasselcore")
                                "--configdir=/var/lib/quassel"
                                "--logfile=/var/log/quassel/core.log"
                                (string-append "--loglevel=" #$loglevel)
                                (string-append "--port=" (number->string #$port))
                                (string-append "--listen=" #$interface))
                          #:mappings (list (file-system-mapping
                                             (source "/var/lib/quassel")
                                             (target source)
                                             (writable? #t))
                                           (file-system-mapping
                                             (source "/var/log/quassel")
                                             (target source)
                                             (writable? #t)))))
               (stop  #~(make-kill-destructor))))))))

(define %quassel-account
  (list (user-group (name "quassel") (system? #t))
        (user-account
          (name "quasselcore")
          (group "quassel")
          (system? #t)
          (comment "Quassel daemon user")
          (home-directory "/var/lib/quassel")
          (shell (file-append shadow "/sbin/nologin")))))

(define %quassel-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/quassel")
      (mkdir-p "/var/log/quassel")
      (let ((cert "/var/lib/quassel/quasselCert.pem"))
        (unless (file-exists? cert)
          (invoke #$(file-append openssl "/bin/openssl")
                  "req" "-x509" "-nodes" "-batch" "-days" "680" "-newkey"
                  "rsa" "-keyout" cert "-out" cert)))))

(define quassel-service-type
  (service-type (name 'quassel)
                (extensions
                  (list (service-extension shepherd-root-service-type
                                           quassel-shepherd-service)
                        (service-extension profile-service-type
                                           (compose list quassel-configuration-quassel))
                        (service-extension account-service-type
                                           (const %quassel-account))
                        (service-extension activation-service-type
                                           (const %quassel-activation))))
                (default-value (quassel-configuration))
                (description
                 "Run @url{https://quassel-irc.org/,quasselcore}, the backend
for the distributed IRC client quassel, which allows you to connect from
multiple machines simultaneously.")))
