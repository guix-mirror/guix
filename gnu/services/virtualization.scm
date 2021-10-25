;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ryan Moe <ryan.moe@gmail.com>
;;; Copyright © 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020,2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Timotej Lazar <timotej.lazar@araneo.si>
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
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu image)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu services)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system hurd)
  #:use-module (gnu system image)
  #:use-module (gnu system shadow)
  #:use-module (gnu system)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix utils)

  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)

  #:export (%hurd-vm-operating-system
            hurd-vm-configuration
            hurd-vm-configuration?
            hurd-vm-configuration-os
            hurd-vm-configuration-qemu
            hurd-vm-configuration-image
            hurd-vm-configuration-disk-size
            hurd-vm-configuration-memory-size
            hurd-vm-configuration-options
            hurd-vm-configuration-id
            hurd-vm-configuration-net-options
            hurd-vm-configuration-secrets

            hurd-vm-disk-image
            hurd-vm-port
            hurd-vm-net-options
            hurd-vm-service-type

            libvirt-configuration
            libvirt-service-type
            virtlog-configuration
            virtlog-service-type

            %qemu-platforms
            lookup-qemu-platforms
            qemu-platform?
            qemu-platform-name

            qemu-binfmt-configuration
            qemu-binfmt-configuration?
            qemu-binfmt-service-type

            qemu-guest-agent-configuration
            qemu-guest-agent-configuration?
            qemu-guest-agent-service-type))

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
   (file-like libvirt)
   "Libvirt package.")
  (qemu
   (file-like qemu)
   "Qemu package.")

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
   (string "libvirt")
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

A filter allows selecting a different logging level for a given category
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
    (string "3:syslog:libvirtd")
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
                           "-f" #$config-file)
                     ;; For finding qemu and ip binaries.
                     #:environment-variables
                     (list (string-append
                            "PATH=/run/current-system/profile/bin:"
                            "/run/current-system/profile/sbin"))))
           (stop #~(make-kill-destructor))))))

(define libvirt-service-type
  (service-type (name 'libvirt)
		(extensions
                 (list
                  (service-extension polkit-service-type
                                     (compose list libvirt-configuration-libvirt))
                  (service-extension profile-service-type
                                     (lambda (config)
                                       (list
                                        (libvirt-configuration-libvirt config)
                                        (libvirt-configuration-qemu config))))
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


;;;
;;; Transparent QEMU emulation via binfmt_misc.
;;;

;; Platforms that QEMU can emulate.
(define-record-type* <qemu-platform>
  qemu-platform make-qemu-platform
  qemu-platform?
  (name     qemu-platform-name)                   ;string
  (family   qemu-platform-family)                 ;string
  (magic    qemu-platform-magic)                  ;bytevector
  (mask     qemu-platform-mask)                   ;bytevector

  ;; Default flags:
  ;;
  ;;   "F": fix binary.  Open the qemu-user binary (statically linked) as soon
  ;;   as binfmt_misc interpretation is handled.
  ;;
  ;;   "P": preserve argv[0].  QEMU 6.0 detects whether it's started with this
  ;;   flag and automatically does the right thing.  Without this flag,
  ;;   argv[0] is replaced by the absolute file name of the executable, an
  ;;   observable difference that can cause discrepancies.
  (flags    qemu-platform-flags (default "FP")))  ;string

(define-syntax bv
  (lambda (s)
    "Expand the given string into a bytevector."
    (syntax-case s ()
      ((_ str)
       (string? (syntax->datum #'str))
       (let ((bv (u8-list->bytevector
                  (map char->integer
                       (string->list (syntax->datum #'str))))))
         bv)))))

;;; The platform descriptions below are taken from
;;; 'scripts/qemu-binfmt-conf.sh' in QEMU.

(define %i386
  (qemu-platform
   (name "i386")
   (family "i386")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x03\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xfe\xfe\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %alpha
  (qemu-platform
   (name "alpha")
   (family "alpha")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x26\x90"))
   (mask (bv "\xff\xff\xff\xff\xff\xfe\xfe\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %arm
  (qemu-platform
   (name "arm")
   (family "arm")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x28\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %armeb
  (qemu-platform
   (name "armeb")
   (family "arm")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x28"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %sparc
  (qemu-platform
   (name "sparc")
   (family "sparc")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %sparc32plus
  (qemu-platform
   (name "sparc32plus")
   (family "sparc")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x12"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %ppc
  (qemu-platform
   (name "ppc")
   (family "ppc")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x14"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %ppc64
  (qemu-platform
   (name "ppc64")
   (family "ppc")
   (magic (bv "\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x15"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %ppc64le
  (qemu-platform
   (name "ppc64le")
   (family "ppcle")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x15\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\x00"))))

(define %m68k
  (qemu-platform
   (name "m68k")
   (family "m68k")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x04"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xfe\xfe\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

;; XXX: We could use the other endianness on a MIPS host.
(define %mips
  (qemu-platform
   (name "mips")
   (family "mips")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %mipsel
  (qemu-platform
   (name "mipsel")
   (family "mips")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %mipsn32
  (qemu-platform
   (name "mipsn32")
   (family "mips")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %mipsn32el
  (qemu-platform
   (name "mipsn32el")
   (family "mips")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %mips64
  (qemu-platform
   (name "mips64")
   (family "mips")
   (magic (bv "\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %mips64el
  (qemu-platform
   (name "mips64el")
   (family "mips")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %riscv32
  (qemu-platform
   (name "riscv32")
   (family "riscv")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xf3\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %riscv64
  (qemu-platform
   (name "riscv64")
   (family "riscv")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xf3\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %sh4
  (qemu-platform
   (name "sh4")
   (family "sh4")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x2a\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %sh4eb
  (qemu-platform
   (name "sh4eb")
   (family "sh4")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x2a"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %s390x
  (qemu-platform
   (name "s390x")
   (family "s390x")
   (magic (bv "\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x16"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %aarch64
  (qemu-platform
   (name "aarch64")
   (family "arm")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xb7\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %hppa
  (qemu-platform
   (name "hppa")
   (family "hppa")
   (magic (bv "\x7f\x45\x4c\x46\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x0f"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %qemu-platforms
  (list %i386 %alpha %arm %sparc32plus %ppc %ppc64 %ppc64le %m68k
        %mips %mipsel %mipsn32 %mipsn32el %mips64 %mips64el
        %riscv32 %riscv64 %sh4 %sh4eb %s390x %aarch64 %hppa))

(define (lookup-qemu-platforms . names)
  "Return the list of QEMU platforms that match NAMES--a list of names such as
\"arm\", \"hppa\", etc."
  (filter (lambda (platform)
            (member (qemu-platform-name platform) names))
          %qemu-platforms))

(define-record-type* <qemu-binfmt-configuration>
  qemu-binfmt-configuration make-qemu-binfmt-configuration
  qemu-binfmt-configuration?
  (qemu        qemu-binfmt-configuration-qemu
               (default qemu))
  (platforms   qemu-binfmt-configuration-platforms
               (default '())))          ;safest default

(define (qemu-platform->binfmt qemu platform)
  "Return a gexp that evaluates to a binfmt string for PLATFORM, using the
given QEMU package."
  (define (bytevector->binfmt-string bv)
    ;; Return a binfmt-friendly string representing BV.  Hex-encode every
    ;; character, in particular because the doc notes "that you must escape
    ;; any NUL bytes; parsing halts at the first one".
    (string-concatenate
     (map (lambda (n)
            (string-append "\\x"
                           (string-pad (number->string n 16) 2 #\0)))
          (bytevector->u8-list bv))))

  (match platform
    (($ <qemu-platform> name family magic mask flags)
     ;; See 'Documentation/binfmt_misc.txt' in the kernel.
     #~(string-append ":qemu-" #$name ":M::"
                      #$(bytevector->binfmt-string magic)
                      ":" #$(bytevector->binfmt-string mask)
                      ":" #$qemu:static "/bin/qemu-" #$name
                      ":" #$flags))))

(define %binfmt-mount-point
  (file-system-mount-point %binary-format-file-system))

(define %binfmt-register-file
  (string-append %binfmt-mount-point "/register"))

(define qemu-binfmt-shepherd-services
  (match-lambda
    (($ <qemu-binfmt-configuration> qemu platforms)
     (list (shepherd-service
            (provision '(qemu-binfmt))
            (documentation "Install binfmt_misc handlers for QEMU.")
            (requirement '(file-system-/proc/sys/fs/binfmt_misc))
            (start #~(lambda ()
                       ;; Register the handlers for all of PLATFORMS.
                       (for-each (lambda (str)
                                   (call-with-output-file
                                       #$%binfmt-register-file
                                     (lambda (port)
                                       (display str port))))
                                 (list
                                  #$@(map (cut qemu-platform->binfmt qemu
                                               <>)
                                          platforms)))
                       #t))
            (stop #~(lambda (_)
                      ;; Unregister the handlers.
                      (for-each (lambda (name)
                                  (let ((file (string-append
                                               #$%binfmt-mount-point
                                               "/qemu-" name)))
                                    (call-with-output-file file
                                      (lambda (port)
                                        (display "-1" port)))))
                                '#$(map qemu-platform-name platforms))
                      #f)))))))

(define qemu-binfmt-service-type
  ;; TODO: Make a separate binfmt_misc service out of this?
  (service-type (name 'qemu-binfmt)
                (extensions
                 (list (service-extension file-system-service-type
                                          (const
                                           (list %binary-format-file-system)))
                       (service-extension shepherd-root-service-type
                                          qemu-binfmt-shepherd-services)))
                (default-value (qemu-binfmt-configuration))
                (description
                 "This service supports transparent emulation of binaries
compiled for other architectures using QEMU and the @code{binfmt_misc}
functionality of the kernel Linux.")))


;;;
;;; QEMU guest agent service.
;;;

(define-configuration qemu-guest-agent-configuration
  (qemu
   (file-like qemu-minimal)
   "QEMU package.")
  (device
   (string "")
   "Path to device or socket used to communicate with the host.  If not
specified, the QEMU default path is used."))

(define qemu-guest-agent-shepherd-service
  (match-lambda
    (($ <qemu-guest-agent-configuration> qemu device)
     (list
      (shepherd-service
       (provision '(qemu-guest-agent))
       (documentation "Run the QEMU guest agent.")
       (start #~(make-forkexec-constructor
                 `(,(string-append #$qemu "/bin/qemu-ga") "--daemon"
                   "--pidfile=/var/run/qemu-ga.pid"
                   "--statedir=/var/run"
                   ,@(if #$device
                         (list (string-append "--path=" #$device))
                         '()))
                 #:pid-file "/var/run/qemu-ga.pid"
                 #:log-file "/var/log/qemu-ga.log"))
       (stop #~(make-kill-destructor)))))))

(define qemu-guest-agent-service-type
  (service-type
   (name 'qemu-guest-agent)
   (extensions
    (list (service-extension shepherd-root-service-type
                             qemu-guest-agent-shepherd-service)))
   (default-value (qemu-guest-agent-configuration))
   (description "Run the QEMU guest agent.")))


;;;
;;; Secrets for guest VMs.
;;;

(define (secret-service-shepherd-services port)
  "Return a Shepherd service that fetches sensitive material at local PORT,
over TCP.  Reboot upon failure."
  ;; This is a Shepherd service, rather than an activation snippet, to make
  ;; sure it is started once 'networking' is up so it can accept incoming
  ;; connections.
  (list
   (shepherd-service
    (documentation "Fetch secrets from the host at startup time.")
    (provision '(secret-service-client))
    (requirement '(loopback networking))
    (modules '((gnu build secret-service)
               (guix build utils)))
    (start (with-imported-modules '((gnu build secret-service)
                                    (guix build utils))
             #~(lambda ()
                 ;; Since shepherd's output port goes to /dev/log, write this
                 ;; message to stderr so it's visible on the Mach console.
                 (format (current-error-port)
                         "receiving secrets from the host...~%")
                 (force-output (current-error-port))

                 (let ((sent (secret-service-receive-secrets #$port)))
                   (unless sent
                     (sleep 3)
                     (reboot))))))
    (stop #~(const #f)))))

(define secret-service-type
  (service-type
   (name 'secret-service)
   (extensions (list (service-extension shepherd-root-service-type
                                        secret-service-shepherd-services)

                     ;; Make every Shepherd service depend on
                     ;; 'secret-service-client'.
                     (service-extension user-processes-service-type
                                        (const '(secret-service-client)))))
   (description
    "This service fetches secret key and other sensitive material over TCP at
boot time.  This service is meant to be used by virtual machines (VMs) that
can only be accessed by their host.")))

(define (secret-service-operating-system os)
  "Return an operating system based on OS that includes the secret-service,
that will be listening to receive secret keys on port 1004, TCP."
  (operating-system
    (inherit os)
    ;; Arrange so that the secret service activation snippet shows up before
    ;; the OpenSSH and Guix activation snippets.  That way, we receive OpenSSH
    ;; and Guix keys before the activation snippets try to generate fresh keys
    ;; for nothing.
    (services (append (operating-system-user-services os)
                      (list (service secret-service-type 1004))))))


;;;
;;; The Hurd in VM service: a Childhurd.
;;;

(define %hurd-vm-operating-system
  (operating-system
    (inherit %hurd-default-operating-system)
    (host-name "childhurd")
    (timezone "Europe/Amsterdam")
    (bootloader (bootloader-configuration
                 (bootloader grub-minimal-bootloader)
                 (targets '("/dev/vda"))
                 (timeout 0)))
    (packages (cons* gdb-minimal
                     (operating-system-packages
                      %hurd-default-operating-system)))
    (services (cons*
               (service openssh-service-type
                        (openssh-configuration
                         (openssh openssh-sans-x)
                         (use-pam? #f)
                         (port-number 2222)
                         (permit-root-login #t)
                         (allow-empty-passwords? #t)
                         (password-authentication? #t)))

               ;; By default, the secret service introduces a pre-initialized
               ;; /etc/guix/acl file in the childhurd.  Thus, clear
               ;; 'authorize-key?' so that it's not overridden at activation
               ;; time.
               (modify-services %base-services/hurd
                 (guix-service-type config =>
                                    (guix-configuration
                                     (inherit config)
                                     (authorize-key? #f))))))))

(define-record-type* <hurd-vm-configuration>
  hurd-vm-configuration make-hurd-vm-configuration
  hurd-vm-configuration?
  (os          hurd-vm-configuration-os                 ;<operating-system>
               (default %hurd-vm-operating-system))
  (qemu        hurd-vm-configuration-qemu               ;file-like
               (default qemu-minimal))
  (image       hurd-vm-configuration-image              ;string
               (thunked)
               (default (hurd-vm-disk-image this-record)))
  (disk-size   hurd-vm-configuration-disk-size          ;number or 'guess
               (default 'guess))
  (memory-size hurd-vm-configuration-memory-size        ;number
               (default 512))
  (options     hurd-vm-configuration-options            ;list of string
               (default `("--snapshot")))
  (id          hurd-vm-configuration-id                 ;#f or integer [1..]
               (default #f))
  (net-options hurd-vm-configuration-net-options        ;list of string
               (thunked)
               (default (hurd-vm-net-options this-record)))
  (secret-root hurd-vm-configuration-secret-root        ;string
               (default "/etc/childhurd")))

(define (hurd-vm-disk-image config)
  "Return a disk-image for the Hurd according to CONFIG.  The secret-service
is added to the OS specified in CONFIG."
  (let* ((os        (secret-service-operating-system
                     (hurd-vm-configuration-os config)))
         (disk-size (hurd-vm-configuration-disk-size config))
         (type      (lookup-image-type-by-name 'hurd-qcow2))
         (os->image (image-type-constructor type)))
    (system-image
     (image (inherit (os->image os))
            (size disk-size)))))

(define (hurd-vm-port config base)
  "Return the forwarded vm port for this childhurd config."
  (let ((id (or (hurd-vm-configuration-id config) 0)))
    (+ base (* 1000 id))))
(define %hurd-vm-secrets-port 11004)
(define %hurd-vm-ssh-port 10022)
(define %hurd-vm-vnc-port 15900)

(define (hurd-vm-net-options config)
  `("--device" "rtl8139,netdev=net0"
    "--netdev"
    ,(string-append "user,id=net0"
                    ",hostfwd=tcp:127.0.0.1:"
                    (number->string (hurd-vm-port config %hurd-vm-secrets-port))
                    "-:1004"
                    ",hostfwd=tcp:127.0.0.1:"
                    (number->string (hurd-vm-port config %hurd-vm-ssh-port))
                    "-:2222"
                    ",hostfwd=tcp:127.0.0.1:"
                    (number->string (hurd-vm-port config %hurd-vm-vnc-port))
                    "-:5900")))

(define (hurd-vm-shepherd-service config)
  "Return a <shepherd-service> for a Hurd in a Virtual Machine with CONFIG."

  (let ((image       (hurd-vm-configuration-image config))
        (qemu        (hurd-vm-configuration-qemu config))
        (memory-size (hurd-vm-configuration-memory-size config))
        (options     (hurd-vm-configuration-options config))
        (id          (hurd-vm-configuration-id config))
        (net-options (hurd-vm-configuration-net-options config))
        (provisions  '(hurd-vm childhurd)))

    (define vm-command
      #~(append (list #$(file-append qemu "/bin/qemu-system-i386")
                      "-m" (number->string #$memory-size)
                      #$@net-options
                      #$@options
                      "--hda" #+image

                      ;; Cause the service to be respawned if the guest
                      ;; reboots (it can reboot for instance if it did not
                      ;; receive valid secrets, or if it crashed.)
                      "--no-reboot")
                (if (file-exists? "/dev/kvm")
                    '("--enable-kvm")
                    '())))

    (list
     (shepherd-service
      (documentation "Run the Hurd in a Virtual Machine: a Childhurd.")
      (provision (if id
                     (map
                      (cute symbol-append <>
                            (string->symbol (number->string id)))
                      provisions)
                     provisions))
      (requirement '(loopback networking user-processes))
      (start
       (with-imported-modules
           (source-module-closure '((gnu build secret-service)
                                    (guix build utils)))
         #~(lambda ()
             (let ((pid  (fork+exec-command #$vm-command
                                            #:user "childhurd"
                                            ;; XXX TODO: use "childhurd" after
                                            ;; updating Shepherd
                                            #:group "kvm"
                                            #:environment-variables
                                            ;; QEMU tries to write to /var/tmp
                                            ;; by default.
                                            '("TMPDIR=/tmp")))
                   (port #$(hurd-vm-port config %hurd-vm-secrets-port))
                   (root #$(hurd-vm-configuration-secret-root config)))
               (catch #t
                 (lambda _
                   ;; XXX: 'secret-service-send-secrets' won't complete until
                   ;; the guest has booted and its secret service server is
                   ;; running, which could take 20+ seconds during which PID 1
                   ;; is stuck waiting.
                   (if (secret-service-send-secrets port root)
                       pid
                       (begin
                         (kill (- pid) SIGTERM)
                         #f)))
                 (lambda (key . args)
                   (kill (- pid) SIGTERM)
                   (apply throw key args)))))))
      (modules `((gnu build secret-service)
                 (guix build utils)
                 ,@%default-modules))
      (stop  #~(make-kill-destructor))))))

(define %hurd-vm-accounts
  (list (user-group (name "childhurd") (system? #t))
        (user-account
         (name "childhurd")
         (group "childhurd")
         (supplementary-groups '("kvm"))
         (comment "Privilege separation user for the childhurd")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin"))
         (system? #t))))

(define (initialize-hurd-vm-substitutes)
  "Initialize the Hurd VM's key pair and ACL and store it on the host."
  (define run
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 match))

          (define host-key
            "/etc/guix/signing-key.pub")

          (define host-acl
            "/etc/guix/acl")

          (match (command-line)
            ((_ guest-config-directory)
             (setenv "GUIX_CONFIGURATION_DIRECTORY"
                     guest-config-directory)
             (invoke #+(file-append guix "/bin/guix") "archive"
                     "--generate-key")

             (when (file-exists? host-acl)
               ;; Copy the host ACL.
               (copy-file host-acl
                          (string-append guest-config-directory
                                         "/acl")))

             (when (file-exists? host-key)
               ;; Add the host key to the childhurd's ACL.
               (let ((key (open-fdes host-key O_RDONLY)))
                 (close-fdes 0)
                 (dup2 key 0)
                 (execl #+(file-append guix "/bin/guix")
                        "guix" "archive" "--authorize"))))))))

  (program-file "initialize-hurd-vm-substitutes" run))

(define (hurd-vm-activation config)
  "Return a gexp to activate the Hurd VM according to CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (define secret-directory
          #$(hurd-vm-configuration-secret-root config))

        (define ssh-directory
          (string-append secret-directory "/etc/ssh"))

        (define guix-directory
          (string-append secret-directory "/etc/guix"))

        (unless (file-exists? ssh-directory)
          ;; Generate SSH host keys under SSH-DIRECTORY.
          (mkdir-p ssh-directory)
          (invoke #$(file-append openssh "/bin/ssh-keygen")
                  "-A" "-f" secret-directory))

        (unless (file-exists? guix-directory)
          (invoke #$(initialize-hurd-vm-substitutes)
                  guix-directory)))))

(define hurd-vm-service-type
  (service-type
   (name 'hurd-vm)
   (extensions (list (service-extension shepherd-root-service-type
                                        hurd-vm-shepherd-service)
                     (service-extension account-service-type
                                        (const %hurd-vm-accounts))
                     (service-extension activation-service-type
                                        hurd-vm-activation)))
   (default-value (hurd-vm-configuration))
   (description
    "Provide a virtual machine (VM) running GNU/Hurd, also known as a
@dfn{childhurd}.")))
