;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build syscalls)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:export (errno
            MS_RDONLY
            MS_REMOUNT
            MS_BIND
            MS_MOVE
            mount
            umount
            mount-points
            swapon
            swapoff
            processes

            IFF_UP
            IFF_BROADCAST
            IFF_LOOPBACK
            all-network-interfaces
            network-interfaces
            network-interface-flags
            loopback-network-interface?
            network-interface-address
            set-network-interface-flags
            set-network-interface-address
            configure-network-interface))

;;; Commentary:
;;;
;;; This module provides bindings to libc's syscall wrappers.  It uses the
;;; FFI, and thus requires a dynamically-linked Guile.  (For statically-linked
;;; Guile, we instead apply 'guile-linux-syscalls.patch'.)
;;;
;;; Code:

(define %libc-errno-pointer
  ;; Glibc's 'errno' pointer.
  (let ((errno-loc (dynamic-func "__errno_location" (dynamic-link))))
    (and errno-loc
         (let ((proc (pointer->procedure '* errno-loc '())))
           (proc)))))

(define errno
  (if %libc-errno-pointer
      (let ((bv (pointer->bytevector %libc-errno-pointer (sizeof int))))
        (lambda ()
          "Return the current errno."
          ;; XXX: We assume that nothing changes 'errno' while we're doing all this.
          ;; In particular, that means that no async must be running here.

          ;; Use one of the fixed-size native-ref procedures because they are
          ;; optimized down to a single VM instruction, which reduces the risk
          ;; that we fiddle with 'errno' (needed on Guile 2.0.5, libc 2.11.)
          (let-syntax ((ref (lambda (s)
                              (syntax-case s ()
                                ((_ bv)
                                 (case (sizeof int)
                                   ((4)
                                    #'(bytevector-s32-native-ref bv 0))
                                   ((8)
                                    #'(bytevector-s64-native-ref bv 0))
                                   (else
                                    (error "unsupported 'int' size"
                                           (sizeof int)))))))))
            (ref bv))))
      (lambda () 0)))

(define (augment-mtab source target type options)
  "Augment /etc/mtab with information about the given mount point."
  (let ((port (open-file "/etc/mtab" "a")))
    (format port "~a ~a ~a ~a 0 0~%"
            source target type (or options "rw"))
    (close-port port)))

(define (read-mtab port)
  "Read an mtab-formatted file from PORT, returning a list of tuples."
  (let loop ((result '()))
    (let ((line (read-line port)))
      (if (eof-object? line)
          (reverse result)
          (loop (cons (string-tokenize line) result))))))

(define (remove-from-mtab target)
  "Remove mount point TARGET from /etc/mtab."
  (define entries
    (remove (match-lambda
             ((device mount-point type options freq passno)
              (string=? target mount-point))
             (_ #f))
            (call-with-input-file "/etc/mtab" read-mtab)))

  (call-with-output-file "/etc/mtab"
    (lambda (port)
      (for-each (match-lambda
                 ((device mount-point type options freq passno)
                  (format port "~a ~a ~a ~a ~a ~a~%"
                          device mount-point type options freq passno)))
                entries))))

;; Linux mount flags, from libc's <sys/mount.h>.
(define MS_RDONLY      1)
(define MS_REMOUNT    32)
(define MS_BIND     4096)
(define MS_MOVE     8192)

(define mount
  (let* ((ptr  (dynamic-func "mount" (dynamic-link)))
         (proc (pointer->procedure int ptr `(* * * ,unsigned-long *))))
    (lambda* (source target type #:optional (flags 0) options
                     #:key (update-mtab? #t))
      "Mount device SOURCE on TARGET as a file system TYPE.  Optionally, FLAGS
may be a bitwise-or of the MS_* <sys/mount.h> constants, and OPTIONS may be a
string.  When FLAGS contains MS_REMOUNT, SOURCE and TYPE are ignored.  When
UPDATE-MTAB? is true, update /etc/mtab.  Raise a 'system-error' exception on
error."
      (let ((ret (proc (if source
                           (string->pointer source)
                           %null-pointer)
                       (string->pointer target)
                       (if type
                           (string->pointer type)
                           %null-pointer)
                       flags
                       (if options
                           (string->pointer options)
                           %null-pointer)))
            (err (errno)))
        (unless (zero? ret)
          (throw 'system-error "mount" "mount ~S on ~S: ~A"
                 (list source target (strerror err))
                 (list err)))
        (when update-mtab?
          (augment-mtab source target type options))))))

(define umount
  (let* ((ptr  (dynamic-func "umount2" (dynamic-link)))
         (proc (pointer->procedure int ptr `(* ,int))))
    (lambda* (target #:optional (flags 0)
                     #:key (update-mtab? #t))
      "Unmount TARGET.  Optionally FLAGS may be one of the MNT_* or UMOUNT_*
constants from <sys/mount.h>."
      (let ((ret (proc (string->pointer target) flags))
            (err (errno)))
        (unless (zero? ret)
          (throw 'system-error "umount" "~S: ~A"
                 (list target (strerror err))
                 (list err)))
        (when update-mtab?
          (remove-from-mtab target))))))

(define (mount-points)
  "Return the mounts points for currently mounted file systems."
  (call-with-input-file "/proc/mounts"
    (lambda (port)
      (let loop ((result '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse result)
              (match (string-tokenize line)
                ((source mount-point _ ...)
                 (loop (cons mount-point result))))))))))

(define swapon
  (let* ((ptr  (dynamic-func "swapon" (dynamic-link)))
         (proc (pointer->procedure int ptr (list '* int))))
    (lambda* (device #:optional (flags 0))
      "Use the block special device at DEVICE for swapping."
      (let ((ret (proc (string->pointer device) flags))
            (err (errno)))
        (unless (zero? ret)
          (throw 'system-error "swapon" "~S: ~A"
                 (list device (strerror err))
                 (list err)))))))

(define swapoff
  (let* ((ptr  (dynamic-func "swapoff" (dynamic-link)))
         (proc (pointer->procedure int ptr '(*))))
    (lambda (device)
      "Stop using block special device DEVICE for swapping."
      (let ((ret (proc (string->pointer device)))
            (err (errno)))
        (unless (zero? ret)
          (throw 'system-error "swapff" "~S: ~A"
                 (list device (strerror err))
                 (list err)))))))

(define (kernel? pid)
  "Return #t if PID designates a \"kernel thread\" rather than a normal
user-land process."
  (let ((stat (call-with-input-file (format #f "/proc/~a/stat" pid)
                (compose string-tokenize read-string))))
    ;; See proc.txt in Linux's documentation for the list of fields.
    (match stat
      ((pid tcomm state ppid pgrp sid tty_nr tty_pgrp flags min_flt
            cmin_flt maj_flt cmaj_flt utime stime cutime cstime
            priority nice num_thread it_real_value start_time
            vsize rss rsslim
            (= string->number start_code) (= string->number end_code) _ ...)
       ;; Got this obscure trick from sysvinit's 'killall5' program.
       (and (zero? start_code) (zero? end_code))))))

(define (processes)
  "Return the list of live processes."
  (sort (filter-map (lambda (file)
                      (let ((pid (string->number file)))
                        (and pid
                             (not (kernel? pid))
                             pid)))
                    (scandir "/proc"))
        <))


;;;
;;; Packed structures.
;;;

(define-syntax sizeof*
  ;; XXX: This duplicates 'compile-time-value'.
  (syntax-rules (int128)
    ((_ int128)
     16)
    ((_ type)
     (let-syntax ((v (lambda (s)
                       (let ((val (sizeof type)))
                         (syntax-case s ()
                           (_ val))))))
       v))))

(define-syntax type-size
  (syntax-rules (~)
    ((_ (type ~ order))
     (sizeof* type))
    ((_ type)
     (sizeof* type))))

(define-syntax write-type
  (syntax-rules (~)
    ((_ bv offset (type ~ order) value)
     (bytevector-uint-set! bv offset value
                           (endianness order) (sizeof* type)))
    ((_ bv offset type value)
     (bytevector-uint-set! bv offset value
                           (native-endianness) (sizeof* type)))))

(define-syntax write-types
  (syntax-rules ()
    ((_ bv offset () ())
     #t)
    ((_ bv offset (type0 types ...) (field0 fields ...))
     (begin
       (write-type bv offset type0 field0)
       (write-types bv (+ offset (type-size type0))
                    (types ...) (fields ...))))))

(define-syntax read-type
  (syntax-rules (~)
    ((_ bv offset (type ~ order))
     (bytevector-uint-ref bv offset
                          (endianness order) (sizeof* type)))
    ((_ bv offset type)
     (bytevector-uint-ref bv offset
                          (native-endianness) (sizeof* type)))))

(define-syntax read-types
  (syntax-rules ()
    ((_ bv offset ())
     '())
    ((_ bv offset (type0 types ...))
     (cons (read-type bv offset type0)
           (read-types bv (+ offset (type-size type0)) (types ...))))))

(define-syntax define-c-struct
  (syntax-rules ()
    "Define READ as an optimized serializer and WRITE! as a deserializer for
the C structure with the given TYPES."
    ((_ name read write! (fields types) ...)
     (begin
       (define (write! bv offset fields ...)
         (write-types bv offset (types ...) (fields ...)))
       (define (read bv offset)
         (read-types bv offset (types ...)))))))


;;;
;;; Network interfaces.
;;;

(define SIOCGIFCONF                               ;from <bits/ioctls.h>
  (if (string-contains %host-type "linux")
      #x8912                                      ;GNU/Linux
      #xf00801a4))                                ;GNU/Hurd
(define SIOCGIFFLAGS
  (if (string-contains %host-type "linux")
      #x8913                                      ;GNU/Linux
      #xc4804191))                                ;GNU/Hurd
(define SIOCSIFFLAGS
  (if (string-contains %host-type "linux")
      #x8914                                      ;GNU/Linux
      -1))                                        ;FIXME: GNU/Hurd?
(define SIOCGIFADDR
  (if (string-contains %host-type "linux")
      #x8915                                      ;GNU/Linux
      -1))                                        ;FIXME: GNU/Hurd?
(define SIOCSIFADDR
  (if (string-contains %host-type "linux")
      #x8916                                      ;GNU/Linux
      -1))                                        ;FIXME: GNU/Hurd?

;; Flags and constants from <net/if.h>.

(define IFF_UP #x1)                               ;Interface is up
(define IFF_BROADCAST #x2)                        ;Broadcast address valid.
(define IFF_LOOPBACK #x8)                         ;Is a loopback net.

(define IF_NAMESIZE 16)                           ;maximum interface name size

(define ifconf-struct
  ;; 'struct ifconf', from <net/if.h>.
  (list int                                       ;int ifc_len
        '*))                                      ;struct ifreq *ifc_ifcu

(define ifreq-struct-size
  ;; 'struct ifreq' begins with an array of IF_NAMESIZE bytes containing the
  ;; interface name (nul-terminated), followed by a bunch of stuff.  This is
  ;; its size in bytes.
  (if (= 8 (sizeof '*))
      40
      32))

(define-c-struct sockaddr-in                      ;<linux/in.h>
  read-sockaddr-in
  write-sockaddr-in!
  (family    unsigned-short)
  (port      (int16 ~ big))
  (address   (int32 ~ big)))

(define-c-struct sockaddr-in6                     ;<linux/in6.h>
  read-sockaddr-in6
  write-sockaddr-in6!
  (family    unsigned-short)
  (port      (int16 ~ big))
  (flowinfo  (int32 ~ big))
  (address   (int128 ~ big))
  (scopeid   int32))

(define (write-socket-address! sockaddr bv index)
  "Write SOCKADDR, a socket address as returned by 'make-socket-address', to
bytevector BV at INDEX."
  (let ((family (sockaddr:fam sockaddr)))
    (cond ((= family AF_INET)
           (write-sockaddr-in! bv index
                               family
                               (sockaddr:port sockaddr)
                               (sockaddr:addr sockaddr)))
          ((= family AF_INET6)
           (write-sockaddr-in6! bv index
                                family
                                (sockaddr:port sockaddr)
                                (sockaddr:flowinfo sockaddr)
                                (sockaddr:addr sockaddr)
                                (sockaddr:scopeid sockaddr)))
          (else
           (error "unsupported socket address" sockaddr)))))

(define (read-socket-address bv index)
  "Read a socket address from bytevector BV at INDEX."
  (let ((family (bytevector-u16-native-ref bv index)))
    (cond ((= family AF_INET)
           (match (read-sockaddr-in bv index)
             ((family port address)
              (make-socket-address family address port))))
          ((= family AF_INET6)
           (match (read-sockaddr-in6 bv index)
             ((family port flowinfo address scopeid)
              (make-socket-address family address port
                                   flowinfo scopeid))))
          (else
           "unsupported socket address family" family))))

(define %ioctl
  ;; The most terrible interface, live from Scheme.
  (pointer->procedure int
                      (dynamic-func "ioctl" (dynamic-link))
                      (list int unsigned-long '*)))

(define (bytevector->string-list bv stride len)
  "Return the null-terminated strings found in BV every STRIDE bytes.  Read at
most LEN bytes from BV."
  (let loop ((bytes  (take (bytevector->u8-list bv)
                           (min len (bytevector-length bv))))
             (result '()))
    (match bytes
      (()
       (reverse result))
      (_
       (loop (drop bytes stride)
             (cons (list->string (map integer->char
                                      (take-while (negate zero?) bytes)))
                   result))))))

(define* (network-interfaces #:optional sock)
  "Return the list of existing network interfaces.  This is typically limited
to interfaces that are currently up."
  (let* ((close? (not sock))
         (sock   (or sock (socket SOCK_STREAM AF_INET 0)))
         (len    (* ifreq-struct-size 10))
         (reqs   (make-bytevector len))
         (conf   (make-c-struct ifconf-struct
                                (list len (bytevector->pointer reqs))))
         (ret    (%ioctl (fileno sock) SIOCGIFCONF conf))
         (err    (errno)))
    (when close?
      (close-port sock))
    (if (zero? ret)
        (bytevector->string-list reqs ifreq-struct-size
                                 (match (parse-c-struct conf ifconf-struct)
                                   ((len . _) len)))
        (throw 'system-error "network-interface-list"
               "network-interface-list: ~A"
               (list (strerror err))
               (list err)))))

(define %interface-line
  ;; Regexp matching an interface line in Linux's /proc/net/dev.
  (make-regexp "^[[:blank:]]*([[:alnum:]]+):.*$"))

(define (all-network-interfaces)
  "Return all the registered network interfaces, including those that are not
up."
  (call-with-input-file "/proc/net/dev"           ;XXX: Linux-specific
    (lambda (port)
      (let loop ((interfaces '()))
        (let ((line (read-line port)))
          (cond ((eof-object? line)
                 (reverse interfaces))
                ((regexp-exec %interface-line line)
                 =>
                 (lambda (match)
                   (loop (cons (match:substring match 1) interfaces))))
                (else
                 (loop interfaces))))))))

(define (network-interface-flags socket name)
  "Return a number that is the bit-wise or of 'IFF*' flags for network
interface NAME."
  (let ((req (make-bytevector ifreq-struct-size)))
    (bytevector-copy! (string->utf8 name) 0 req 0
                      (min (string-length name) (- IF_NAMESIZE 1)))
    (let* ((ret (%ioctl (fileno socket) SIOCGIFFLAGS
                        (bytevector->pointer req)))
           (err (errno)))
      (if (zero? ret)

          ;; The 'ifr_flags' field is IF_NAMESIZE bytes after the beginning of
          ;; 'struct ifreq', and it's a short int.
          (bytevector-sint-ref req IF_NAMESIZE (native-endianness)
                               (sizeof short))

          (throw 'system-error "network-interface-flags"
                 "network-interface-flags on ~A: ~A"
                 (list name (strerror err))
                 (list err))))))

(define (loopback-network-interface? name)
  "Return true if NAME designates a loopback network interface."
  (let* ((sock  (socket SOCK_STREAM AF_INET 0))
         (flags (network-interface-flags sock name)))
    (close-port sock)
    (not (zero? (logand flags IFF_LOOPBACK)))))

(define (set-network-interface-flags socket name flags)
  "Set the flag of network interface NAME to FLAGS."
  (let ((req (make-bytevector ifreq-struct-size)))
    (bytevector-copy! (string->utf8 name) 0 req 0
                      (min (string-length name) (- IF_NAMESIZE 1)))
    ;; Set the 'ifr_flags' field.
    (bytevector-uint-set! req IF_NAMESIZE flags (native-endianness)
                          (sizeof short))
    (let* ((ret (%ioctl (fileno socket) SIOCSIFFLAGS
                        (bytevector->pointer req)))
           (err (errno)))
      (unless (zero? ret)
        (throw 'system-error "set-network-interface-flags"
               "set-network-interface-flags on ~A: ~A"
               (list name (strerror err))
               (list err))))))

(define (set-network-interface-address socket name sockaddr)
  "Set the address of network interface NAME to SOCKADDR."
  (let ((req (make-bytevector ifreq-struct-size)))
    (bytevector-copy! (string->utf8 name) 0 req 0
                      (min (string-length name) (- IF_NAMESIZE 1)))
    ;; Set the 'ifr_addr' field.
    (write-socket-address! sockaddr req IF_NAMESIZE)
    (let* ((ret (%ioctl (fileno socket) SIOCSIFADDR
                        (bytevector->pointer req)))
           (err (errno)))
      (unless (zero? ret)
        (throw 'system-error "set-network-interface-address"
               "set-network-interface-address on ~A: ~A"
               (list name (strerror err))
               (list err))))))

(define (network-interface-address socket name)
  "Return the address of network interface NAME.  The result is an object of
the same type as that returned by 'make-socket-address'."
  (let ((req (make-bytevector ifreq-struct-size)))
    (bytevector-copy! (string->utf8 name) 0 req 0
                      (min (string-length name) (- IF_NAMESIZE 1)))
    (let* ((ret (%ioctl (fileno socket) SIOCGIFADDR
                        (bytevector->pointer req)))
           (err (errno)))
      (if (zero? ret)
          (read-socket-address req IF_NAMESIZE)
          (throw 'system-error "network-interface-address"
                 "network-interface-address on ~A: ~A"
                 (list name (strerror err))
                 (list err))))))

(define (configure-network-interface name sockaddr flags)
  "Configure network interface NAME to use SOCKADDR, an address as returned by
'make-socket-address', and FLAGS, a bitwise-or of IFF_* constants."
  (let ((sock (socket (sockaddr:fam sockaddr) SOCK_STREAM 0)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (set-network-interface-address sock name sockaddr)
        (set-network-interface-flags sock name flags))
      (lambda ()
        (close-port sock)))))

;;; syscalls.scm ends here
