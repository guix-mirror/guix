;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ryan Moe <ryan.moe@gmail.com>
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

(define-module (gnu services virtualization)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages virtualization)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)

  #:export (libvirt-configuration
            libvirt-service-type
            virtlog-service-type))

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-join
     (string-split (string-delete #\? str) #\-)
     "_")))

(define (quote-val val)
  (string-append "\"" val "\""))

(define (serialize-field field-name val)
  (format #t "~a = ~a\n" (uglify-field-name field-name) val))

(define (serialize-string field-name val)
  (serialize-field field-name (quote-val val)))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val 1 0)))

(define (serialize-integer field-name val)
  (serialize-field field-name val))

(define (build-opt-list val)
  (string-append
   "["
   (string-join (map quote-val val) ",")
   "]"))

(define optional-list? list?)
(define optional-string? string?)

(define (serialize-list field-name val)
  (serialize-field field-name (build-opt-list val)))

(define (serialize-optional-list field-name val)
  (if (null? val)
      (format #t "# ~a = []\n" (uglify-field-name field-name))
      (serialize-list field-name val)))

(define (serialize-optional-string field-name val)
  (if (string-null? val)
      (format #t "# ~a = \"\"\n" (uglify-field-name field-name))
      (serialize-string field-name val)))

(define-configuration libvirt-configuration
  (libvirt
   (package libvirt)
   "Libvirt package.")
  (listen-tls?
   (boolean #t)
   "Flag listening for secure TLS connections on the public TCP/IP port.
must set @code{listen} for this to have any effect.

It is necessary to setup a CA and issue server certificates before
using this capability.")
  (listen-tcp?
   (boolean #f)
   "Listen for unencrypted TCP connections on the public TCP/IP port.
must set @code{listen} for this to have any effect.

Using the TCP socket requires SASL authentication by default. Only
SASL mechanisms which support data encryption are allowed. This is
DIGEST_MD5 and GSSAPI (Kerberos5)")
  (tls-port
   (string "16514")
   "Port for accepting secure TLS connections This can be a port number,
or service name")
  (tcp-port
   (string "16509")
   "Port for accepting insecure TCP connections This can be a port number,
or service name")
  (listen-addr
   (string "0.0.0.0")
   "IP address or hostname used for client connections.")
  (mdns-adv?
   (boolean #f)
   "Flag toggling mDNS advertisement of the libvirt service.

Alternatively can disable for all services on a host by
stopping the Avahi daemon.")
  (mdns-name
   (string (string-append "Virtualization Host " (gethostname)))
   "Default mDNS advertisement name. This must be unique on the
immediate broadcast network.")
  (unix-sock-group
   (string "root")
   "UNIX domain socket group ownership. This can be used to
allow a 'trusted' set of users access to management capabilities
without becoming root.")
  (unix-sock-ro-perms
   (string "0777")
   "UNIX socket permissions for the R/O socket. This is used
for monitoring VM status only.")
  (unix-sock-rw-perms
   (string "0770")
   "UNIX socket permissions for the R/W socket. Default allows
only root. If PolicyKit is enabled on the socket, the default
will change to allow everyone (eg, 0777)")
  (unix-sock-admin-perms
   (string "0777")
   "UNIX socket permissions for the admin socket. Default allows
only owner (root), do not change it unless you are sure to whom
you are exposing the access to.")
  (unix-sock-dir
   (string "/var/run/libvirt")
   "The directory in which sockets will be found/created.")
  (auth-unix-ro
   (string "polkit")
   "Authentication scheme for UNIX read-only sockets. By default
socket permissions allow anyone to connect")
  (auth-unix-rw
   (string "polkit")
   "Authentication scheme for UNIX read-write sockets. By default
socket permissions only allow root. If PolicyKit support was compiled
into libvirt, the default will be to use 'polkit' auth.")
  (auth-tcp
   (string "sasl")
   "Authentication scheme for TCP sockets. If you don't enable SASL,
then all TCP traffic is cleartext. Don't do this outside of a dev/test
scenario.")
  (auth-tls
   (string "none")
   "Authentication scheme for TLS sockets. TLS sockets already have
encryption provided by the TLS layer, and limited authentication is
done by certificates.

It is possible to make use of any SASL authentication mechanism as
well, by using 'sasl' for this option")
  (access-drivers
   (optional-list '())
   "API access control scheme.

By default an authenticated user is allowed access to all APIs. Access
drivers can place restrictions on this.")
  (key-file
   (string "")
   "Server key file path. If set to an empty string, then no private key
is loaded.")
  (cert-file
   (string "")
   "Server key file path. If set to an empty string, then no certificate
is loaded.")
  (ca-file
   (string "")
   "Server key file path. If set to an empty string, then no CA certificate
is loaded.")
  (crl-file
   (string "")
   "Certificate revocation list path. If set to an empty string, then no
CRL is loaded.")
  (tls-no-sanity-cert
   (boolean #f)
   "Disable verification of our own server certificates.

When libvirtd starts it performs some sanity checks against its own
certificates.")
  (tls-no-verify-cert
   (boolean #f)
   "Disable verification of client certificates.

Client certificate verification is the primary authentication mechanism.
Any client which does not present a certificate signed by the CA
will be rejected.")
  (tls-allowed-dn-list
   (optional-list '())
   "Whitelist of allowed x509 Distinguished Name.")
  (sasl-allowed-usernames
   (optional-list '())
   "Whitelist of allowed SASL usernames. The format for username
depends on the SASL authentication mechanism.")
  (tls-priority
   (string "NORMAL")
   "Override the compile time default TLS priority string. The
default is usually \"NORMAL\" unless overridden at build time.
Only set this is it is desired for libvirt to deviate from
the global default settings.")
  (max-clients
   (integer 5000)
   "Maximum number of concurrent client connections to allow
over all sockets combined.")
  (max-queued-clients
   (integer 1000)
   "Maximum length of queue of connections waiting to be
accepted by the daemon. Note, that some protocols supporting
retransmission may obey this so that a later reattempt at
connection succeeds.")
  (max-anonymous-clients
   (integer 20)
   "Maximum length of queue of accepted but not yet authenticated
clients. Set this to zero to turn this feature off")
  (min-workers
   (integer 5)
   "Number of workers to start up initially.")
  (max-workers
   (integer 20)
   "Maximum number of worker threads.

If the number of active clients exceeds @code{min-workers},
then more threads are spawned, up to max_workers limit.
Typically you'd want max_workers to equal maximum number
of clients allowed.")
  (prio-workers
   (integer 5)
   "Number of priority workers. If all workers from above
pool are stuck, some calls marked as high priority
(notably domainDestroy) can be executed in this pool.")
  (max-requests
    (integer 20)
    "Total global limit on concurrent RPC calls.")
  (max-client-requests
    (integer 5)
    "Limit on concurrent requests from a single client
connection. To avoid one client monopolizing the server
this should be a small fraction of the global max_requests
and max_workers parameter.")
  (admin-min-workers
    (integer 1)
    "Same as @code{min-workers} but for the admin interface.")
  (admin-max-workers
     (integer 5)
    "Same as @code{max-workers} but for the admin interface.")
  (admin-max-clients
    (integer 5)
    "Same as @code{max-clients} but for the admin interface.")
  (admin-max-queued-clients
    (integer 5)
    "Same as @code{max-queued-clients} but for the admin interface.")
  (admin-max-client-requests
    (integer 5)
    "Same as @code{max-client-requests} but for the admin interface.")
  (log-level
    (integer 3)
    "Logging level. 4 errors, 3 warnings, 2 information, 1 debug.")
  (log-filters
    (string "3:remote 4:event")
    "Logging filters.

A filter allows to select a different logging level for a given category
of logs
The format for a filter is one of:
@itemize
@item x:name

@item x:+name
@end itemize

where @code{name} is a string which is matched against the category
given in the @code{VIR_LOG_INIT()} at the top of each libvirt source
file, e.g., \"remote\", \"qemu\", or \"util.json\" (the name in the
filter can be a substring of the full category name, in order
to match multiple similar categories), the optional \"+\" prefix
tells libvirt to log stack trace for each message matching
name, and @code{x} is the minimal level where matching messages should
be logged:

@itemize
@item 1: DEBUG
@item 2: INFO
@item 3: WARNING
@item 4: ERROR
@end itemize

Multiple filters can be defined in a single filters statement, they just
need to be separated by spaces.")
  (log-outputs
    (string "3:stderr")
    "Logging outputs.

An output is one of the places to save logging information
The format for an output can be:

@table @code
@item x:stderr
output goes to stderr

@item x:syslog:name
use syslog for the output and use the given name as the ident

@item x:file:file_path
output to a file, with the given filepath

@item x:journald
output to journald logging system
@end table

In all case the x prefix is the minimal level, acting as a filter

@itemize
@item 1: DEBUG
@item 2: INFO
@item 3: WARNING
@item 4: ERROR
@end itemize

Multiple outputs can be defined, they just need to be separated by spaces.")
  (audit-level
    (integer 1)
    "Allows usage of the auditing subsystem to be altered

@itemize
@item 0: disable all auditing
@item 1: enable auditing, only if enabled on host
@item 2: enable auditing, and exit if disabled on host.
@end itemize
")
  (audit-logging
    (boolean #f)
    "Send audit messages via libvirt logging infrastructure.")
  (host-uuid
    (optional-string "")
    "Host UUID. UUID must not have all digits be the same.")
  (host-uuid-source
    (string "smbios")
    "Source to read host UUID.

@itemize

@item @code{smbios}: fetch the UUID from @code{dmidecode -s system-uuid}

@item @code{machine-id}: fetch the UUID from @code{/etc/machine-id}

@end itemize

If @code{dmidecode} does not provide a valid UUID a temporary UUID
will be generated.")
  (keepalive-interval
    (integer 5)
    "A keepalive message is sent to a client after
@code{keepalive_interval} seconds of inactivity to check if
the client is still responding. If set to -1, libvirtd will
never send keepalive requests; however clients can still send
them and the daemon will send responses.")
  (keepalive-count
    (integer 5)
    "Maximum number of keepalive messages that are allowed to be sent
to the client without getting any response before the connection is
considered broken.

In other words, the connection is automatically
closed approximately after
@code{keepalive_interval * (keepalive_count + 1)} seconds since the last
message received from the client. When @code{keepalive-count} is
set to 0, connections will be automatically closed after
@code{keepalive-interval} seconds of inactivity without sending any
keepalive messages.")
  (admin-keepalive-interval
    (integer 5)
    "Same as above but for admin interface.")
  (admin-keepalive-count
    (integer 5)
    "Same as above but for admin interface.")
  (ovs-timeout
    (integer 5)
    "Timeout for Open vSwitch calls.

The @code{ovs-vsctl} utility is used for the configuration and
its timeout option is set by default to 5 seconds to avoid
potential infinite waits blocking libvirt."))

(define* (libvirt-conf-file config)
  "Return a libvirtd config file."
  (plain-file "libvirtd.conf"
              (with-output-to-string
                (lambda ()
                  (serialize-configuration config libvirt-configuration-fields)))))

(define %libvirt-accounts
  (list (user-group (name "libvirt") (system? #t))))

(define (%libvirt-activation config)
  (let ((sock-dir (libvirt-configuration-unix-sock-dir config)))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$sock-dir))))


(define (libvirt-shepherd-service config)
  (let* ((config-file (libvirt-conf-file config))
         (libvirt (libvirt-configuration-libvirt config)))
    (list (shepherd-service
           (documentation "Run the libvirt daemon.")
           (provision '(libvirtd))
           (start #~(make-forkexec-constructor
                     (list (string-append #$libvirt "/sbin/libvirtd")
                           "-f" #$config-file)))
           (stop #~(make-kill-destructor))))))

(define libvirt-service-type
  (service-type (name 'libvirt)
		(extensions
                 (list
                  (service-extension polkit-service-type
                                     (compose list libvirt-configuration-libvirt))
                  (service-extension profile-service-type
                                     (compose list
                                              libvirt-configuration-libvirt))
                  (service-extension activation-service-type
                                     %libvirt-activation)
                  (service-extension shepherd-root-service-type
                                     libvirt-shepherd-service)
                  (service-extension account-service-type
                                     (const %libvirt-accounts))))
                (default-value (libvirt-configuration))))


(define-record-type* <virtlog-configuration>
  virtlog-configuration make-virtlog-configuration
  virtlog-configuration?
  (libvirt      virtlog-configuration-libvirt
                (default libvirt))
  (log-level    virtlog-configuration-log-level
                (default 3))
  (log-filters  virtlog-configuration-log-filters
                (default "3:remote 4:event"))
  (log-outputs  virtlog-configuration-log-outputs
                (default "3:syslog:virtlogd"))
  (max-clients  virtlog-configuration-max-clients
                (default 1024))
  (max-size     virtlog-configuration-max-size
                (default 2097152)) ;; 2MB
  (max-backups  virtlog-configuration-max-backups
                (default 3)))

(define* (virtlogd-conf-file config)
  "Return a virtlogd config file."
  (plain-file "virtlogd.conf"
              (string-append
               "log_level = " (number->string (virtlog-configuration-log-level config)) "\n"
               "log_filters = \"" (virtlog-configuration-log-filters config) "\"\n"
               "log_outputs = \"" (virtlog-configuration-log-outputs config) "\"\n"
               "max_clients = " (number->string (virtlog-configuration-max-clients config)) "\n"
               "max_size = " (number->string (virtlog-configuration-max-size config)) "\n"
               "max_backups = " (number->string (virtlog-configuration-max-backups config)) "\n")))

(define (virtlogd-shepherd-service config)
  (let* ((config-file (virtlogd-conf-file config))
         (libvirt (virtlog-configuration-libvirt config)))
    (list (shepherd-service
           (documentation "Run the virtlog daemon.")
           (provision '(virtlogd))
           (start #~(make-forkexec-constructor
                     (list (string-append #$libvirt "/sbin/virtlogd")
                           "-f" #$config-file)))
           (stop #~(make-kill-destructor))))))

(define virtlog-service-type
  (service-type (name 'virtlogd)
		(extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     virtlogd-shepherd-service)))
                (default-value (virtlog-configuration))))

(define (generate-libvirt-documentation)
  (generate-documentation
   `((libvirt-configuration ,libvirt-configuration-fields))
   'libvirt-configuration))
