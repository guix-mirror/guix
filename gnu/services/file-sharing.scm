;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Simon South <simon@simonsouth.net>
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

(define-module (gnu services file-sharing)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt random)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (transmission-daemon-configuration
            transmission-daemon-service-type
            transmission-password-hash
            transmission-random-salt))

;;;
;;; Transmission Daemon.
;;;

(define %transmission-daemon-user "transmission")
(define %transmission-daemon-group "transmission")

(define %transmission-daemon-configuration-directory
  "/var/lib/transmission-daemon")
(define %transmission-daemon-log-file
  "/var/log/transmission.log")

(define %transmission-salt-length 8)

(define (transmission-password-hash password salt)
  "Returns a string containing the result of hashing @var{password} together
with @var{salt}, in the format recognized by Transmission clients for their
@code{rpc-password} configuration setting.

@var{salt} must be an eight-character string.  The
@code{transmission-random-salt} procedure can be used to generate a suitable
salt value at random."
  (if (not (and (string? salt)
                (eq? (string-length salt) %transmission-salt-length)))
      (raise (formatted-message
              (G_ "salt value must be a string of ~d characters")
              %transmission-salt-length))
      (string-append "{"
                     (bytevector->base16-string
                      (sha1 (string->utf8 (string-append password salt))))
                     salt)))

(define (transmission-random-salt)
  "Returns a string containing a random, eight-character salt value of the
type generated and used by Transmission clients, suitable for passing to the
@code{transmission-password-hash} procedure."
  ;; This implementation matches a portion of Transmission's tr_ssha1
  ;; function.  See libtransmission/crypto-utils.c in the Transmission source
  ;; distribution.
  (let ((salter (string-append "0123456789"
                               "abcdefghijklmnopqrstuvwxyz"
                               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                               "./")))
    (list->string
     (map (lambda (u8)
            (string-ref salter (modulo u8 (string-length salter))))
          (bytevector->u8-list
           (gen-random-bv %transmission-salt-length %gcry-strong-random))))))

(define (uglify-field-name field-name)
  (string-delete #\? (symbol->string field-name)))

(define (serialize-field field-name val)
  ;; "Serialize" each configuration field as a G-expression containing a
  ;; name-value pair, the collection of which will subsequently be serialized
  ;; to disk as a JSON object.
  #~(#$(uglify-field-name field-name) . #$val))

(define serialize-boolean serialize-field)
(define serialize-integer serialize-field)
(define serialize-rational serialize-field)

(define serialize-string serialize-field)
(define-maybe string)
;; Override the definition of "serialize-maybe-string", as we need to output a
;; name-value pair for the JSON builder.
(set! serialize-maybe-string
  (lambda (field-name val)
    (serialize-string field-name
                      (if (and (symbol? val)
                               (eq? val 'disabled))
                          ""
                          val))))

(define (string-list? val)
  (and (list? val)
       (and-map (lambda (x)
                  (and (string? x)
                       (not (string-index x #\,))))
                val)))
(define (serialize-string-list field-name val)
  (serialize-field field-name (string-join val ",")))

(define days
  '((sunday    . #b0000001)
    (monday    . #b0000010)
    (tuesday   . #b0000100)
    (wednesday . #b0001000)
    (thursday  . #b0010000)
    (friday    . #b0100000)
    (saturday  . #b1000000)))
(define day-lists
  (list (cons 'weekdays '(monday tuesday wednesday thursday friday))
        (cons 'weekends '(saturday sunday))
        (cons 'all (map car days))))
(define (day-list? val)
  (or (and (symbol? val)
           (assq val day-lists))
      (and (list? val)
           (and-map (lambda (x)
                      (and (symbol? x)
                           (assq x days)))
                    val))))
(define (serialize-day-list field-name val)
  (serialize-integer field-name
                     (reduce logior
                             #b0000000
                             (map (lambda (day)
                                    (assq-ref days day))
                                  (if (symbol? val)
                                      (assq-ref day-lists val)
                                      val)))))

(define encryption-modes
  '((prefer-unencrypted-connections . 0)
    (prefer-encrypted-connections   . 1)
    (require-encrypted-connections  . 2)))
(define (encryption-mode? val)
  (and (symbol? val)
       (assq val encryption-modes)))
(define (serialize-encryption-mode field-name val)
  (serialize-integer field-name (assq-ref encryption-modes val)))

(define serialize-file-like serialize-field)

(define (file-object? val)
  (or (string? val)
      (file-like? val)))
(define (serialize-file-object field-name val)
  (if (file-like? val)
      (serialize-file-like field-name val)
      (serialize-string field-name val)))
(define-maybe file-object)
(set! serialize-maybe-file-object
  (lambda (field-name val)
    (if (and (symbol? val)
             (eq? val 'disabled))
        (serialize-string field-name "")
        (serialize-file-object field-name val))))

(define (file-object-list? val)
  (and (list? val)
       (and-map file-object? val)))
(define serialize-file-object-list serialize-field)

(define message-levels
  '((none  . 0)
    (error . 1)
    (info  . 2)
    (debug . 3)))
(define (message-level? val)
  (and (symbol? val)
       (assq val message-levels)))
(define (serialize-message-level field-name val)
  (serialize-integer field-name (assq-ref message-levels val)))

(define (non-negative-integer? val)
  (and (integer? val)
       (not (negative? val))))
(define serialize-non-negative-integer serialize-integer)

(define (non-negative-rational? val)
  (and (rational? val)
       (not (negative? val))))
(define serialize-non-negative-rational serialize-rational)

(define (port-number? val)
  (and (integer? val)
       (>= val 1)
       (<= val 65535)))
(define serialize-port-number serialize-integer)

(define preallocation-modes
  '((none   . 0)
    (fast   . 1)
    (sparse . 1)
    (full   . 2)))
(define (preallocation-mode? val)
  (and (symbol? val)
       (assq val preallocation-modes)))
(define (serialize-preallocation-mode field-name val)
  (serialize-integer field-name (assq-ref preallocation-modes val)))

(define tcp-types-of-service
  '((default     . "default")
    (low-cost    . "lowcost")
    (throughput  . "throughput")
    (low-delay   . "lowdelay")
    (reliability . "reliability")))
(define (tcp-type-of-service? val)
  (and (symbol? val)
       (assq val tcp-types-of-service)))
(define (serialize-tcp-type-of-service field-name val)
  (serialize-string field-name (assq-ref tcp-types-of-service val)))

(define (transmission-password-hash? val)
  (and (string? val)
       (= (string-length val) 49)
       (eqv? (string-ref val 0) #\{)
       (string-every char-set:hex-digit val 1 41)))
(define serialize-transmission-password-hash serialize-string)
(define-maybe transmission-password-hash)
(set! serialize-maybe-transmission-password-hash serialize-maybe-string)

(define (umask? val)
  (and (integer? val)
       (>= val #o000)
       (<= val #o777)))
(define serialize-umask serialize-integer) ; must use decimal representation

(define-configuration transmission-daemon-configuration
  ;; Settings internal to this service definition.
  (transmission
   (file-like transmission)
   "The Transmission package to use.")
  (stop-wait-period
   (non-negative-integer 10)
   "The period, in seconds, to wait when stopping the service for
@command{transmission-daemon} to exit before killing its process.  This allows
the daemon time to complete its housekeeping and send a final update to
trackers as it shuts down.  On slow hosts, or hosts with a slow network
connection, this value may need to be increased.")

  ;; Files and directories.
  (download-dir
   (string (string-append %transmission-daemon-configuration-directory
                          "/downloads"))
   "The directory to which torrent files are downloaded.")
  (incomplete-dir-enabled?
   (boolean #f)
   "If @code{#t}, files will be held in @code{incomplete-dir} while their
torrent is being downloaded, then moved to @code{download-dir} once the
torrent is complete.  Otherwise, files for all torrents (including those still
being downloaded) will be placed in @code{download-dir}.")
  (incomplete-dir
   (maybe-string 'disabled)
   "The directory in which files from incompletely downloaded torrents will be
held when @code{incomplete-dir-enabled?} is @code{#t}.")
  (umask
   (umask #o022)
   "The file mode creation mask used for downloaded files.  (See the
@command{umask} man page for more information.)")
  (rename-partial-files?
   (boolean #t)
   "When @code{#t}, ``.part'' is appended to the name of partially downloaded
files.")
  (preallocation
   (preallocation-mode 'fast)
   "The mode by which space should be preallocated for downloaded files, one
of @code{none}, @code{fast} (or @code{sparse}) and @code{full}.  Specifying
@code{full} will minimize disk fragmentation at a cost to file-creation
speed.")
  (watch-dir-enabled?
   (boolean #f)
   "If @code{#t}, the directory specified by @code{watch-dir} will be watched
for new @file{.torrent} files and the torrents they describe added
automatically (and the original files removed, if
@code{trash-original-torrent-files?} is @code{#t}).")
  (watch-dir
   (maybe-string 'disabled)
   "The directory to be watched for @file{.torrent} files indicating new
torrents to be added, when @code{watch-dir-enabled} is @code{#t}.")
  (trash-original-torrent-files?
   (boolean #f)
   "When @code{#t}, @file{.torrent} files will be deleted from the watch
directory once their torrent has been added (see
@code{watch-directory-enabled?}).")

  ;; Bandwidth limits.
  (speed-limit-down-enabled?
   (boolean #f)
   "When @code{#t}, the daemon's download speed will be limited to the rate
specified by @code{speed-limit-down}.")
  (speed-limit-down
   (non-negative-integer 100)
   "The default global-maximum download speed, in kilobytes per second.")
  (speed-limit-up-enabled?
   (boolean #f)
   "When @code{#t}, the daemon's upload speed will be limited to the rate
specified by @code{speed-limit-up}.")
  (speed-limit-up
   (non-negative-integer 100)
   "The default global-maximum upload speed, in kilobytes per second.")
  (alt-speed-enabled?
   (boolean #f)
   "When @code{#t}, the alternate speed limits @code{alt-speed-down} and
@code{alt-speed-up} are used (in place of @code{speed-limit-down} and
@code{speed-limit-up}, if they are enabled) to constrain the daemon's
bandwidth usage.  This can be scheduled to occur automatically at certain
times during the week; see @code{alt-speed-time-enabled?}.")
  (alt-speed-down
   (non-negative-integer 50)
   "The alternate global-maximum download speed, in kilobytes per second.")
  (alt-speed-up
   (non-negative-integer 50)
   "The alternate global-maximum upload speed, in kilobytes per second.")

  ;; Bandwidth-limit scheduling.
  (alt-speed-time-enabled?
   (boolean #f)
   "When @code{#t}, the alternate speed limits @code{alt-speed-down} and
@code{alt-speed-up} will be enabled automatically during the periods specified
by @code{alt-speed-time-day}, @code{alt-speed-time-begin} and
@code{alt-time-speed-end}.")
  (alt-speed-time-day
   (day-list 'all)
   "The days of the week on which the alternate-speed schedule should be used,
specified either as a list of days (@code{sunday}, @code{monday}, and so on)
or using one of the symbols @code{weekdays}, @code{weekends} or @code{all}.")
  (alt-speed-time-begin
   (non-negative-integer 540)
   "The time of day at which to enable the alternate speed limits,
expressed as a number of minutes since midnight.")
  (alt-speed-time-end
   (non-negative-integer 1020)
   "The time of day at which to disable the alternate speed limits,
expressed as a number of minutes since midnight.")

  ;; Peer networking.
  (bind-address-ipv4
   (string "0.0.0.0")
   "The IP address at which to listen for peer connections, or ``0.0.0.0'' to
listen at all available IP addresses.")
  (bind-address-ipv6
   (string "::")
   "The IPv6 address at which to listen for peer connections, or ``::'' to
listen at all available IPv6 addresses.")
  (peer-port-random-on-start?
   (boolean #f)
   "If @code{#t}, when the daemon starts it will select a port at random on
which to listen for peer connections, from the range specified (inclusively)
by @code{peer-port-random-low} and @code{peer-port-random-high}.  Otherwise,
it listens on the port specified by @code{peer-port}.")
  (peer-port-random-low
   (port-number 49152)
   "The lowest selectable port number when @code{peer-port-random-on-start?}
is @code{#t}.")
  (peer-port-random-high
   (port-number 65535)
   "The highest selectable port number when @code{peer-port-random-on-start}
is @code{#t}.")
  (peer-port
   (port-number 51413)
   "The port on which to listen for peer connections when
@code{peer-port-random-on-start?} is @code{#f}.")
  (port-forwarding-enabled?
   (boolean #t)
   "If @code{#t}, the daemon will attempt to configure port-forwarding on an
upstream gateway automatically using @acronym{UPnP} and @acronym{NAT-PMP}.")
  (encryption
   (encryption-mode 'prefer-encrypted-connections)
   "The encryption preference for peer connections, one of
@code{prefer-unencrypted-connections}, @code{prefer-encrypted-connections} or
@code{require-encrypted-connections}.")
  (peer-congestion-algorithm
   (maybe-string 'disabled)
   "The TCP congestion-control algorithm to use for peer connections,
specified using a string recognized by the operating system in calls to
@code{setsockopt} (or set to @code{disabled}, in which case the
operating-system default is used).

Note that on GNU/Linux systems, the kernel must be configured to allow
processes to use a congestion-control algorithm not in the default set;
otherwise, it will deny these requests with ``Operation not permitted''.  To
see which algorithms are available on your system and which are currently
permitted for use, look at the contents of the files
@file{tcp_available_congestion_control} and
@file{tcp_allowed_congestion_control} in the @file{/proc/sys/net/ipv4}
directory.

As an example, to have Transmission Daemon use
@uref{http://www-ece.rice.edu/networks/TCP-LP/, the TCP Low Priority
congestion-control algorithm}, you'll need to modify your kernel configuration
to build in support for the algorithm, then update your operating-system
configuration to allow its use by adding a @code{sysctl-service-type}
service (or updating the existing one's configuration) with lines like the
following:

@lisp
(service sysctl-service-type
         (sysctl-configuration
          (settings
           (\"net.ipv4.tcp_allowed_congestion_control\" .
            \"reno cubic lp\"))))
@end lisp

The Transmission Daemon configuration can then be updated with

@lisp
(peer-congestion-algorithm \"lp\")
@end lisp

and the system reconfigured to have the changes take effect.")
  (peer-socket-tos
   (tcp-type-of-service 'default)
   "The type of service to request in outgoing @acronym{TCP} packets,
one of @code{default}, @code{low-cost}, @code{throughput}, @code{low-delay}
and @code{reliability}.")
  (peer-limit-global
   (non-negative-integer 200)
   "The global limit on the number of connected peers.")
  (peer-limit-per-torrent
   (non-negative-integer 50)
   "The per-torrent limit on the number of connected peers.")
  (upload-slots-per-torrent
   (non-negative-integer 14)
   "The maximum number of peers to which the daemon will upload data
simultaneously for each torrent.")
  (peer-id-ttl-hours
   (non-negative-integer 6)
   "The maximum lifespan, in hours, of the peer ID associated with each public
torrent before it is regenerated.")

  ;; Peer blocklists.
  (blocklist-enabled?
   (boolean #f)
   "When @code{#t}, the daemon will ignore peers mentioned in the blocklist it
has most recently downloaded from @code{blocklist-url}.")
  (blocklist-url
   (maybe-string 'disabled)
   "The URL of a peer blocklist (in @acronym{P2P}-plaintext or eMule
@file{.dat} format) to be periodically downloaded and applied when
@code{blocklist-enabled?} is @code{#t}.")

  ;; Queueing.
  (download-queue-enabled?
   (boolean #t)
   "If @code{#t}, the daemon will be limited to downloading at most
@code{download-queue-size} non-stalled torrents simultaneously.")
  (download-queue-size
   (non-negative-integer 5)
   "The size of the daemon's download queue, which limits the number of
non-stalled torrents it will download at any one time when
@code{download-queue-enabled?} is @code{#t}.")
  (seed-queue-enabled?
   (boolean #f)
   "If @code{#t}, the daemon will be limited to seeding at most
@code{seed-queue-size} non-stalled torrents simultaneously.")
  (seed-queue-size
   (non-negative-integer 10)
   "The size of the daemon's seed queue, which limits the number of
non-stalled torrents it will seed at any one time when
@code{seed-queue-enabled?} is @code{#t}.")
  (queue-stalled-enabled?
   (boolean #t)
   "When @code{#t}, the daemon will consider torrents for which it has not
shared data in the past @code{queue-stalled-minutes} minutes to be stalled and
not count them against its @code{download-queue-size} and
@code{seed-queue-size} limits.")
  (queue-stalled-minutes
   (non-negative-integer 30)
   "The maximum period, in minutes, a torrent may be idle before it is
considered to be stalled, when @code{queue-stalled-enabled?} is @code{#t}.")

  ;; Seeding limits.
  (ratio-limit-enabled?
   (boolean #f)
   "When @code{#t}, a torrent being seeded will automatically be paused once
it reaches the ratio specified by @code{ratio-limit}.")
  (ratio-limit
   (non-negative-rational 2.0)
   "The ratio at which a torrent being seeded will be paused, when
@code{ratio-limit-enabled?} is @code{#t}.")
  (idle-seeding-limit-enabled?
   (boolean #f)
   "When @code{#t}, a torrent being seeded will automatically be paused once
it has been idle for @code{idle-seeding-limit} minutes.")
  (idle-seeding-limit
   (non-negative-integer 30)
   "The maximum period, in minutes, a torrent being seeded may be idle before
it is paused, when @code{idle-seeding-limit-enabled?} is @code{#t}.")

  ;; BitTorrent extensions.
  (dht-enabled?
   (boolean #t)
   "Enable @uref{http://bittorrent.org/beps/bep_0005.html, the distributed
hash table (@acronym{DHT}) protocol}, which supports the use of trackerless
torrents.")
  (lpd-enabled?
   (boolean #f)
   "Enable @url{https://en.wikipedia.org/wiki/Local_Peer_Discovery, local peer
discovery} (@acronym{LPD}), which allows the discovery of peers on the local
network and may reduce the amount of data sent over the public Internet.")
  (pex-enabled?
   (boolean #t)
   "Enable @url{https://en.wikipedia.org/wiki/Peer_exchange, peer
exchange} (@acronym{PEX}), which reduces the daemon's reliance on external
trackers and may improve its performance.")
  (utp-enabled?
   (boolean #t)
   "Enable @url{http://bittorrent.org/beps/bep_0029.html, the micro transport
protocol} (@acronym{uTP}), which aims to reduce the impact of BitTorrent
traffic on other users of the local network while maintaining full utilization
of the available bandwidth.")

  ;; Remote procedure call (RPC) interface.
  (rpc-enabled?
   (boolean #t)
   "If @code{#t}, enable the remote procedure call (@acronym{RPC}) interface,
which allows remote control of the daemon via its Web interface, the
@command{transmission-remote} command-line client, and similar tools.")
  (rpc-bind-address
   (string "0.0.0.0")
   "The IP address at which to listen for @acronym{RPC} connections, or
``0.0.0.0'' to listen at all available IP addresses.")
  (rpc-port
   (port-number 9091)
   "The port on which to listen for @acronym{RPC} connections.")
  (rpc-url
   (string "/transmission/")
   "The path prefix to use in the @acronym{RPC}-endpoint @acronym{URL}.")
  (rpc-authentication-required?
   (boolean #f)
   "When @code{#t}, clients must authenticate (see @code{rpc-username} and
@code{rpc-password}) when using the @acronym{RPC} interface.  Note this has
the side effect of disabling host-name whitelisting (see
@code{rpc-host-whitelist-enabled?}.")
  (rpc-username
   (maybe-string 'disabled)
   "The username required by clients to access the @acronym{RPC} interface
when @code{rpc-authentication-required?} is @code{#t}.")
  (rpc-password
   (maybe-transmission-password-hash 'disabled)
   "The password required by clients to access the @acronym{RPC} interface
when @code{rpc-authentication-required?} is @code{#t}.  This must be specified
using a password hash in the format recognized by Transmission clients, either
copied from an existing @file{settings.json} file or generated using the
@code{transmission-password-hash} procedure.")
  (rpc-whitelist-enabled?
   (boolean #t)
   "When @code{#t}, @acronym{RPC} requests will be accepted only when they
originate from an address specified in @code{rpc-whitelist}.")
  (rpc-whitelist
   (string-list '("127.0.0.1" "::1"))
   "The list of IP and IPv6 addresses from which @acronym{RPC} requests will
be accepted when @code{rpc-whitelist-enabled?} is @code{#t}.  Wildcards may be
specified using @samp{*}.")
  (rpc-host-whitelist-enabled?
   (boolean #t)
   "When @code{#t}, @acronym{RPC} requests will be accepted only when they are
addressed to a host named in @code{rpc-host-whitelist}.  Note that requests to
``localhost'' or ``localhost.'', or to a numeric address, are always accepted
regardless of these settings.

Note also this functionality is disabled when
@code{rpc-authentication-required?} is @code{#t}.")
  (rpc-host-whitelist
   (string-list '())
   "The list of host names recognized by the @acronym{RPC} server when
@code{rpc-host-whitelist-enabled?} is @code{#t}.")

  ;; Miscellaneous.
  (message-level
   (message-level 'info)
   "The minimum severity level of messages to be logged (to
@file{/var/log/transmission.log}) by the daemon, one of @code{none} (no
logging), @code{error}, @code{info} and @code{debug}.")
  (start-added-torrents?
   (boolean #t)
   "When @code{#t}, torrents are started as soon as they are added; otherwise,
they are added in ``paused'' state.")
  (script-torrent-done-enabled?
   (boolean #f)
   "When @code{#t}, the script specified by
@code{script-torrent-done-filename} will be invoked each time a torrent
completes.")
  (script-torrent-done-filename
   (maybe-file-object 'disabled)
   "A file name or file-like object specifying a script to run each time a
torrent completes, when @code{script-torrent-done-enabled?} is @code{#t}.")
  (scrape-paused-torrents-enabled?
   (boolean #t)
   "When @code{#t}, the daemon will scrape trackers for a torrent even when
the torrent is paused.")
  (cache-size-mb
   (non-negative-integer 4)
   "The amount of memory, in megabytes, to allocate for the daemon's in-memory
cache.  A larger value may increase performance by reducing the frequency of
disk I/O.")
  (prefetch-enabled?
   (boolean #t)
   "When @code{#t}, the daemon will try to improve I/O performance by hinting
to the operating system which data is likely to be read next from disk to
satisfy requests from peers."))

(define (transmission-daemon-shepherd-service config)
  "Return a <shepherd-service> for Transmission Daemon with CONFIG."
  (let ((transmission
         (transmission-daemon-configuration-transmission config))
        (stop-wait-period
         (transmission-daemon-configuration-stop-wait-period config)))
    (list
     (shepherd-service
      (provision '(transmission-daemon transmission bittorrent))
      (requirement '(networking))
      (documentation "Share files using the BitTorrent protocol.")
      (start #~(make-forkexec-constructor
                '(#$(file-append transmission "/bin/transmission-daemon")
                  "--config-dir"
                  #$%transmission-daemon-configuration-directory
                  "--foreground")
                #:user #$%transmission-daemon-user
                #:group #$%transmission-daemon-group
                #:directory #$%transmission-daemon-configuration-directory
                #:log-file #$%transmission-daemon-log-file
                #:environment-variables
                '("CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt")))
      (stop #~(lambda (pid)
                (kill pid SIGTERM)

                ;; Transmission Daemon normally needs some time to shut down,
                ;; as it will complete some housekeeping and send a final
                ;; update to trackers before it exits.
                ;;
                ;; Wait a reasonable period for it to stop before continuing.
                ;; If we don't do this, restarting the service can fail as the
                ;; new daemon process finds the old one still running and
                ;; attached to the port used for peer connections.
                (let wait-before-killing ((period #$stop-wait-period))
                  (if (zero? (car (waitpid pid WNOHANG)))
                      (if (positive? period)
                          (begin
                            (sleep 1)
                            (wait-before-killing (- period 1)))
                          (begin
                            (format #t
                                    #$(G_ "Wait period expired; killing \
transmission-daemon (pid ~a).~%")
                                    pid)
                            (display #$(G_ "(If you see this message \
regularly, you may need to increase the value
of 'stop-wait-period' in the service configuration.)\n"))
                            (kill pid SIGKILL)))))
                #f))
      (actions
       (list
        (shepherd-action
         (name 'reload)
         (documentation "Reload the settings file from disk.")
         (procedure #~(lambda (pid)
                        (if pid
                            (begin
                              (kill pid SIGHUP)
                              (display #$(G_ "Service transmission-daemon has \
been asked to reload its settings file.")))
                            (display #$(G_ "Service transmission-daemon is not \
running."))))))))))))

(define %transmission-daemon-accounts
  (list (user-group
         (name %transmission-daemon-group)
         (system? #t))
        (user-account
         (name %transmission-daemon-user)
         (group %transmission-daemon-group)
         (comment "Transmission Daemon service account")
         (home-directory %transmission-daemon-configuration-directory)
         (shell (file-append shadow "/sbin/nologin"))
         (system? #t))))

(define %transmission-daemon-log-rotations
  (list (log-rotation
         (files (list %transmission-daemon-log-file)))))

(define (transmission-daemon-computed-settings-file config)
  "Return a @code{computed-file} object that, when unquoted in a G-expression,
produces a Transmission settings file (@file{settings.json}) matching CONFIG."
  (let ((settings
         ;; "Serialize" the configuration settings as a list of G-expressions
         ;; containing a name-value pair, which will ultimately be sorted and
         ;; serialized to the settings file as a JSON object.
         (map
          (lambda (field)
            ((configuration-field-serializer field)
             (configuration-field-name field)
             ((configuration-field-getter field) config)))
          (filter
           (lambda (field)
             ;; Omit configuration fields that are used only internally by
             ;; this service definition.
             (not (memq (configuration-field-name field)
                        '(transmission stop-wait-period))))
           transmission-daemon-configuration-fields))))
    (computed-file
     "settings.json"
     (with-extensions (list guile-gcrypt guile-json-4)
       (with-imported-modules (source-module-closure '((json builder)))
         #~(begin
             (use-modules (json builder))

             (with-output-to-file #$output
               (lambda ()
                 (scm->json (sort-list '(#$@settings)
                                       (lambda (x y)
                                         (string<=? (car x) (car y))))
                            #:pretty #t)))))))))

(define (transmission-daemon-activation config)
  "Return the Transmission Daemon activation GEXP for CONFIG."
  (let ((config-dir %transmission-daemon-configuration-directory)
        (incomplete-dir-enabled
         (transmission-daemon-configuration-incomplete-dir-enabled? config))
        (incomplete-dir
         (transmission-daemon-configuration-incomplete-dir config))
        (watch-dir-enabled
         (transmission-daemon-configuration-watch-dir-enabled? config))
        (watch-dir
         (transmission-daemon-configuration-watch-dir config)))
    (with-imported-modules (source-module-closure '((guix build utils)))
      #~(begin
          (use-modules (guix build utils))

          (let ((owner (getpwnam #$%transmission-daemon-user)))
            (define (mkdir-p/perms directory perms)
              (mkdir-p directory)
              (chown directory (passwd:uid owner) (passwd:gid owner))
              (chmod directory perms))

            ;; Create the directories Transmission Daemon is configured to use
            ;; and assign them suitable permissions.
            (for-each (lambda (directory-specification)
                        (apply mkdir-p/perms directory-specification))
                      '(#$@(append
                            `((,config-dir #o750))
                            (if incomplete-dir-enabled
                                `((,incomplete-dir #o750))
                                '())
                            (if watch-dir-enabled
                                `((,watch-dir #o770))
                                '())))))

          ;; Generate and activate the daemon's settings file, settings.json.
          (activate-special-files
           '((#$(string-append config-dir "/settings.json")
              #$(transmission-daemon-computed-settings-file config))))))))

(define transmission-daemon-service-type
  (service-type
   (name 'transmission)
   (extensions
    (list (service-extension shepherd-root-service-type
                             transmission-daemon-shepherd-service)
          (service-extension account-service-type
                             (const %transmission-daemon-accounts))
          (service-extension rottlog-service-type
                             (const %transmission-daemon-log-rotations))
          (service-extension activation-service-type
                             transmission-daemon-activation)))
   (default-value (transmission-daemon-configuration))
   (description "Share files using the BitTorrent protocol.")))

(define (generate-transmission-daemon-documentation)
  (generate-documentation
   `((transmission-daemon-configuration
      ,transmission-daemon-configuration-fields))
   'transmission-daemon-configuration))
