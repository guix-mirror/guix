;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build linux-initrd)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system repl error-handling)
  #:autoload   (system repl repl) (start-repl)
  #:autoload   (system base compile) (compile-file)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (guix build utils)
  #:export (mount-essential-file-systems
            linux-command-line
            find-long-option
            make-essential-device-nodes
            configure-qemu-networking

            disk-partitions
            partition-label-predicate
            find-partition-by-label
            canonicalize-device-spec

            mount-flags->bit-mask
            check-file-system
            mount-file-system
            bind-mount

            load-linux-module*
            device-number
            boot-system))

;;; Commentary:
;;;
;;; Utility procedures useful in a Linux initial RAM disk (initrd).  Note that
;;; many of these use procedures not yet available in vanilla Guile (`mount',
;;; `load-linux-module', etc.); these are provided by a Guile patch used in
;;; the GNU distribution.
;;;
;;; Code:

(define* (mount-essential-file-systems #:key (root "/"))
  "Mount /proc and /sys under ROOT."
  (define (scope dir)
    (string-append root
                   (if (string-suffix? "/" root)
                       ""
                       "/")
                   dir))

  (unless (file-exists? (scope "proc"))
    (mkdir (scope "proc")))
  (mount "none" (scope "proc") "proc")

  (unless (file-exists? (scope "sys"))
    (mkdir (scope "sys")))
  (mount "none" (scope "sys") "sysfs"))

(define (move-essential-file-systems root)
  "Move currently mounted essential file systems to ROOT."
  (for-each (lambda (dir)
              (let ((target (string-append root dir)))
                (unless (file-exists? target)
                  (mkdir target))
                (mount dir target "" MS_MOVE)))
            '("/proc" "/sys")))

(define (linux-command-line)
  "Return the Linux kernel command line as a list of strings."
  (string-tokenize
   (call-with-input-file "/proc/cmdline"
     get-string-all)))

(define (find-long-option option arguments)
  "Find OPTION among ARGUMENTS, where OPTION is something like \"--load\".
Return the value associated with OPTION, or #f on failure."
  (let ((opt (string-append option "=")))
    (and=> (find (cut string-prefix? opt <>)
                 arguments)
           (lambda (arg)
             (substring arg (+ 1 (string-index arg #\=)))))))

(define-syntax %ext2-endianness
  ;; Endianness of ext2 file systems.
  (identifier-syntax (endianness little)))

;; Offset in bytes of interesting parts of an ext2 superblock.  See
;; <http://www.nongnu.org/ext2-doc/ext2.html#DEF-SUPERBLOCK>.
;; TODO: Use "packed structs" from Guile-OpenGL or similar.
(define-syntax %ext2-sblock-magic       (identifier-syntax 56))
(define-syntax %ext2-sblock-creator-os  (identifier-syntax 72))
(define-syntax %ext2-sblock-uuid        (identifier-syntax 104))
(define-syntax %ext2-sblock-volume-name (identifier-syntax 120))

(define (read-ext2-superblock device)
  "Return the raw contents of DEVICE's ext2 superblock as a bytevector, or #f
if DEVICE does not contain an ext2 file system."
  (define %ext2-magic
    ;; The magic bytes that identify an ext2 file system.
    #xef53)

  (define superblock-size
    ;; Size of the interesting part of an ext2 superblock.
    264)

  (define block
    ;; The superblock contents.
    (make-bytevector superblock-size))

  (call-with-input-file device
    (lambda (port)
      (seek port 1024 SEEK_SET)

      ;; Note: work around <http://bugs.gnu.org/17466>.
      (and (eqv? superblock-size (get-bytevector-n! port block 0
                                                    superblock-size))
           (let ((magic (bytevector-u16-ref block %ext2-sblock-magic
                                            %ext2-endianness)))
             (and (= magic %ext2-magic)
                  block))))))

(define (ext2-superblock-uuid sblock)
  "Return the UUID of ext2 superblock SBLOCK as a 16-byte bytevector."
  (let ((uuid (make-bytevector 16)))
    (bytevector-copy! sblock %ext2-sblock-uuid uuid 0 16)
    uuid))

(define (ext2-superblock-volume-name sblock)
  "Return the volume name of SBLOCK as a string of at most 16 characters, or
#f if SBLOCK has no volume name."
  (let ((bv (make-bytevector 16)))
    (bytevector-copy! sblock %ext2-sblock-volume-name bv 0 16)

    ;; This is a Latin-1, nul-terminated string.
    (let ((bytes (take-while (negate zero?) (bytevector->u8-list bv))))
      (if (null? bytes)
          #f
          (list->string (map integer->char bytes))))))

(define (disk-partitions)
  "Return the list of device names corresponding to valid disk partitions."
  (define (partition? major minor)
    (let ((marker (format #f "/sys/dev/block/~a:~a/partition" major minor)))
      (catch 'system-error
        (lambda ()
          (not (zero? (call-with-input-file marker read))))
        (lambda args
          (if (= ENOENT (system-error-errno args))
              #f
              (apply throw args))))))

  (call-with-input-file "/proc/partitions"
    (lambda (port)
      ;; Skip the two header lines.
      (read-line port)
      (read-line port)

      ;; Read each subsequent line, and extract the last space-separated
      ;; field.
      (let loop ((parts '()))
        (let ((line  (read-line port)))
          (if (eof-object? line)
              (reverse parts)
              (match (string-tokenize line)
                (((= string->number major) (= string->number minor)
                  blocks name)
                 (if (partition? major minor)
                     (loop (cons name parts))
                     (loop parts))))))))))

(define (partition-label-predicate label)
  "Return a procedure that, when applied to a partition name such as \"sda1\",
return #t if that partition's volume name is LABEL."
  (lambda (part)
    (let* ((device (string-append "/dev/" part))
           (sblock (catch 'system-error
                     (lambda ()
                       (read-ext2-superblock device))
                     (lambda args
                       ;; When running on the hand-made /dev,
                       ;; 'disk-partitions' could return partitions for which
                       ;; we have no /dev node.  Handle that gracefully.
                       (if (= ENOENT (system-error-errno args))
                           (begin
                             (format (current-error-port)
                                     "warning: device '~a' not found~%"
                                     device)
                             #f)
                           (apply throw args))))))
      (and sblock
           (let ((volume (ext2-superblock-volume-name sblock)))
             (and volume
                  (string=? volume label)))))))

(define (find-partition-by-label label)
  "Return the first partition found whose volume name is LABEL, or #f if none
were found."
  (and=> (find (partition-label-predicate label)
               (disk-partitions))
         (cut string-append "/dev/" <>)))

(define* (canonicalize-device-spec spec #:optional (title 'any))
  "Return the device name corresponding to SPEC.  TITLE is a symbol, one of
the following:

  • 'device', in which case SPEC is known to designate a device node--e.g.,
     \"/dev/sda1\";
  • 'label', in which case SPEC is known to designate a partition label--e.g.,
     \"my-root-part\";
  • 'any', in which case SPEC can be anything.
"
  (define max-trials
    ;; Number of times we retry partition label resolution.
    7)

  (define canonical-title
    ;; The realm of canonicalization.
    (if (eq? title 'any)
        (if (string-prefix? "/" spec)
            'device
            'label)
        title))

  (case canonical-title
    ((device)
     ;; Nothing to do.
     spec)
    ((label)
     ;; Resolve the label.
     (let loop ((count 0))
       (let ((device (find-partition-by-label spec)))
         (or device
             ;; Some devices take a bit of time to appear, most notably USB
             ;; storage devices.  Thus, wait for the device to appear.
             (if (> count max-trials)
                 (error "failed to resolve partition label" spec)
                 (begin
                   (sleep 1)
                   (loop (+ 1 count))))))))
    ;; TODO: Add support for UUIDs.
    (else
     (error "unknown device title" title))))

(define* (make-disk-device-nodes base major #:optional (minor 0))
  "Make the block device nodes around BASE (something like \"/root/dev/sda\")
with the given MAJOR number, starting with MINOR."
  (mknod base 'block-special #o644 (device-number major minor))
  (let loop ((i 1))
    (when (< i 6)
      (mknod (string-append base (number->string i))
             'block-special #o644 (device-number major (+ minor i)))
      (loop (+ i 1)))))

(define* (make-essential-device-nodes #:key (root "/"))
  "Make essential device nodes under ROOT/dev."
  ;; The hand-made udev!

  (define (scope dir)
    (string-append root
                   (if (string-suffix? "/" root)
                       ""
                       "/")
                   dir))

  (unless (file-exists? (scope "dev"))
    (mkdir (scope "dev")))

  ;; Make the device nodes for SCSI disks.
  (make-disk-device-nodes (scope "dev/sda") 8)
  (make-disk-device-nodes (scope "dev/sdb") 8 16)
  (make-disk-device-nodes (scope "dev/sdc") 8 32)
  (make-disk-device-nodes (scope "dev/sdd") 8 48)

  ;; SCSI CD-ROM devices (aka. "/dev/sr0" etc.).
  (mknod (scope "dev/scd0") 'block-special #o644 (device-number 11 0))
  (mknod (scope "dev/scd1") 'block-special #o644 (device-number 11 1))

  ;; The virtio (para-virtualized) block devices, as supported by QEMU/KVM.
  (make-disk-device-nodes (scope "dev/vda") 252)

  ;; Memory (used by Xorg's VESA driver.)
  (mknod (scope "dev/mem") 'char-special #o640 (device-number 1 1))
  (mknod (scope "dev/kmem") 'char-special #o640 (device-number 1 2))

  ;; Inputs (used by Xorg.)
  (unless (file-exists? (scope "dev/input"))
    (mkdir (scope "dev/input")))
  (mknod (scope "dev/input/mice") 'char-special #o640 (device-number 13 63))
  (mknod (scope "dev/input/mouse0") 'char-special #o640 (device-number 13 32))
  (mknod (scope "dev/input/event0") 'char-special #o640 (device-number 13 64))

  ;; System console.  This node is magically created by the kernel on the
  ;; initrd's root, so don't try to create it in that case.
  (unless (string=? root "/")
    (mknod (scope "dev/console") 'char-special #o600
           (device-number 5 1)))

  ;; TTYs.
  (mknod (scope "dev/tty") 'char-special #o600
         (device-number 5 0))
  (chmod (scope "dev/tty") #o666)
  (let loop ((n 0))
    (and (< n 50)
         (let ((name (format #f "dev/tty~a" n)))
           (mknod (scope name) 'char-special #o600
                  (device-number 4 n))
           (loop (+ 1 n)))))

  ;; Serial line.
  (mknod (scope "dev/ttyS0") 'char-special #o660
         (device-number 4 64))

  ;; Pseudo ttys.
  (mknod (scope "dev/ptmx") 'char-special #o666
         (device-number 5 2))
  (chmod (scope "dev/ptmx") #o666)

  ;; Create /dev/pts; it will be mounted later, at boot time.
  (unless (file-exists? (scope "dev/pts"))
    (mkdir (scope "dev/pts")))

  ;; Rendez-vous point for syslogd.
  (mknod (scope "dev/log") 'socket #o666 0)
  (mknod (scope "dev/kmsg") 'char-special #o600 (device-number 1 11))

  ;; Other useful nodes, notably relied on by guix-daemon.
  (for-each (match-lambda
             ((file major minor)
              (mknod (scope file) 'char-special #o666
                     (device-number major minor))
              (chmod (scope file) #o666)))
            '(("dev/null" 1 3)
              ("dev/zero" 1 5)
              ("dev/full" 1 7)
              ("dev/random" 1 8)
              ("dev/urandom" 1 9)))

  (symlink "/proc/self/fd" (scope "dev/fd"))
  (symlink "/proc/self/fd/0" (scope "dev/stdin"))
  (symlink "/proc/self/fd/1" (scope "dev/stdout"))
  (symlink "/proc/self/fd/2" (scope "dev/stderr"))

  ;; Loopback devices.
  (let loop ((i 0))
    (when (< i 8)
      (mknod (scope (string-append "dev/loop" (number->string i)))
             'block-special #o660
             (device-number 7 i))
      (loop (+ 1 i))))

  ;; File systems in user space (FUSE).
  (mknod (scope "dev/fuse") 'char-special #o666 (device-number 10 229)))

(define %host-qemu-ipv4-address
  (inet-pton AF_INET "10.0.2.10"))

(define* (configure-qemu-networking #:optional (interface "eth0"))
  "Setup the INTERFACE network interface and /etc/resolv.conf according to
QEMU's default networking settings (see net/slirp.c in QEMU for default
networking values.)  Return #t if INTERFACE is up, #f otherwise."
  (display "configuring QEMU networking...\n")
  (let* ((sock    (socket AF_INET SOCK_STREAM 0))
         (address (make-socket-address AF_INET %host-qemu-ipv4-address 0))
         (flags   (network-interface-flags sock interface)))
    (set-network-interface-address sock interface address)
    (set-network-interface-flags sock interface (logior flags IFF_UP))

    (unless (file-exists? "/etc")
      (mkdir "/etc"))
    (call-with-output-file "/etc/resolv.conf"
      (lambda (p)
        (display "nameserver 10.0.2.3\n" p)))

    (logand (network-interface-flags sock interface) IFF_UP)))

;; Linux mount flags, from libc's <sys/mount.h>.
(define MS_RDONLY 1)
(define MS_NOSUID 2)
(define MS_NODEV  4)
(define MS_NOEXEC 8)
(define MS_BIND 4096)
(define MS_MOVE 8192)

(define (bind-mount source target)
  "Bind-mount SOURCE at TARGET."
  (mount source target "" MS_BIND))

(define (load-linux-module* file)
  "Load Linux module from FILE, the name of a `.ko' file."
  (define (slurp module)
    (call-with-input-file file get-bytevector-all))

  (load-linux-module (slurp file)))

(define (device-number major minor)
  "Return the device number for the device with MAJOR and MINOR, for use as
the last argument of `mknod'."
  (+ (* major 256) minor))

(define (pidof program)
  "Return the PID of the first presumed instance of PROGRAM."
  (let ((program (basename program)))
    (find (lambda (pid)
            (let ((exe (format #f "/proc/~a/exe" pid)))
              (and=> (false-if-exception (readlink exe))
                     (compose (cut string=? program <>) basename))))
          (filter-map string->number (scandir "/proc")))))

(define* (mount-root-file-system root type
                                 #:key volatile-root? (unionfs "unionfs"))
  "Mount the root file system of type TYPE at device ROOT.  If VOLATILE-ROOT?
is true, mount ROOT read-only and make it a union with a writable tmpfs using
UNIONFS."
  (define (mark-as-not-killable pid)
    ;; Tell the 'user-processes' dmd service that PID must be kept alive when
    ;; shutting down.
    (mkdir-p "/root/etc/dmd")
    (let ((port (open-file "/root/etc/dmd/do-not-kill" "a")))
      (chmod port #o600)
      (write pid port)
      (newline port)
      (close-port port)))

  (catch #t
    (lambda ()
      (if volatile-root?
          (begin
            (mkdir-p "/real-root")
            (mount root "/real-root" type MS_RDONLY)
            (mkdir-p "/rw-root")
            (mount "none" "/rw-root" "tmpfs")

            ;; We want read-write /dev nodes.
            (make-essential-device-nodes #:root "/rw-root")

            ;; Make /root a union of the tmpfs and the actual root.  Use
            ;; 'max_files' to set a high RLIMIT_NOFILE for the unionfs process
            ;; itself.  Failing to do that, we quickly run out of file
            ;; descriptors; see <http://bugs.gnu.org/17827>.
            (unless (zero? (system* unionfs "-o"
                                    "cow,allow_other,use_ino,suid,dev,max_files=65536"
                                    "/rw-root=RW:/real-root=RO"
                                    "/root"))
              (error "unionfs failed"))

            ;; Make sure unionfs remains alive till the end.  Because
            ;; 'fuse_daemonize' doesn't tell the PID of the forked daemon, we
            ;; have to resort to 'pidof' here.
            (mark-as-not-killable (pidof unionfs)))
          (begin
            (check-file-system root type)
            (mount root "/root" type))))
    (lambda args
      (format (current-error-port) "exception while mounting '~a': ~s~%"
              root args)
      (start-repl)))

  (copy-file "/proc/mounts" "/root/etc/mtab"))

(define (check-file-system device type)
  "Run a file system check of TYPE on DEVICE."
  (define fsck
    (string-append "fsck." type))

  (let ((status (system* fsck "-v" "-p" device)))
    (match (status:exit-val status)
      (0
       #t)
      (1
       (format (current-error-port) "'~a' corrected errors on ~a; continuing~%"
               fsck device))
      (2
       (format (current-error-port) "'~a' corrected errors on ~a; rebooting~%"
               fsck device)
       (sleep 3)
       (reboot))
      (code
       (format (current-error-port) "'~a' exited with code ~a on ~a; spawning REPL~%"
               fsck code device)
       (start-repl)))))

(define (mount-flags->bit-mask flags)
  "Return the number suitable for the 'flags' argument of 'mount' that
corresponds to the symbols listed in FLAGS."
  (let loop ((flags flags))
    (match flags
      (('read-only rest ...)
       (logior MS_RDONLY (loop rest)))
      (('bind-mount rest ...)
       (logior MS_BIND (loop rest)))
      (('no-suid rest ...)
       (logior MS_NOSUID (loop rest)))
      (('no-dev rest ...)
       (logior MS_NODEV (loop rest)))
      (('no-exec rest ...)
       (logior MS_NOEXEC (loop rest)))
      (()
       0))))

(define* (mount-file-system spec #:key (root "/root"))
  "Mount the file system described by SPEC under ROOT.  SPEC must have the
form:

  (DEVICE TITLE MOUNT-POINT TYPE (FLAGS ...) OPTIONS CHECK?)

DEVICE, MOUNT-POINT, and TYPE must be strings; OPTIONS can be a string or #f;
FLAGS must be a list of symbols.  CHECK? is a Boolean indicating whether to
run a file system check."
  (match spec
    ((source title mount-point type (flags ...) options check?)
     (let ((source      (canonicalize-device-spec source title))
           (mount-point (string-append root "/" mount-point)))
       (when check?
         (check-file-system source type))
       (mkdir-p mount-point)
       (mount source mount-point type (mount-flags->bit-mask flags)
              (if options
                  (string->pointer options)
                  %null-pointer))

       ;; Update /etc/mtab.
       (mkdir-p (string-append root "/etc"))
       (let ((port (open-file (string-append root "/etc/mtab") "a")))
         (format port "~a ~a ~a ~a 0 0~%"
                 source mount-point type (or options ""))
         (close-port port))))))

(define (switch-root root)
  "Switch to ROOT as the root file system, in a way similar to what
util-linux' switch_root(8) does."
  (move-essential-file-systems root)
  (chdir root)

  ;; Since we're about to 'rm -rf /', try to make sure we're on an initrd.
  ;; TODO: Use 'statfs' to check the fs type, like klibc does.
  (when (or (not (file-exists? "/init")) (directory-exists? "/home"))
    (format (current-error-port)
            "The root file system is probably not an initrd; \
bailing out.~%root contents: ~s~%" (scandir "/"))
    (force-output (current-error-port))
    (exit 1))

  ;; Delete files from the old root, without crossing mount points (assuming
  ;; there are no mount points in sub-directories.)  That means we're leaving
  ;; the empty ROOT directory behind us, but that's OK.
  (let ((root-device (stat:dev (stat "/"))))
    (for-each (lambda (file)
                (unless (member file '("." ".."))
                  (let* ((file   (string-append "/" file))
                         (device (stat:dev (lstat file))))
                    (when (= device root-device)
                      (delete-file-recursively file)))))
              (scandir "/")))

  ;; Make ROOT the new root.
  (mount root "/" "" MS_MOVE)
  (chroot ".")
  (chdir "/")

  (when (file-exists? "/dev/console")
    ;; Close the standard file descriptors since they refer to the old
    ;; /dev/console, and reopen them.
    (let ((console (open-file "/dev/console" "r+b0")))
      (for-each close-fdes '(0 1 2))

      (dup2 (fileno console) 0)
      (dup2 (fileno console) 1)
      (dup2 (fileno console) 2)

      (close-port console))))


(define* (boot-system #:key
                      (linux-modules '())
                      qemu-guest-networking?
                      guile-modules-in-chroot?
                      volatile-root?
                      (mounts '()))
  "This procedure is meant to be called from an initrd.  Boot a system by
first loading LINUX-MODULES, then setting up QEMU guest networking if
QEMU-GUEST-NETWORKING? is true, mounting the file systems specified in MOUNTS,
and finally booting into the new root if any.  The initrd supports kernel
command-line options '--load', '--root', and '--repl'.

Mount the root file system, specified by the '--root' command-line argument,
if any.

MOUNTS must be a list suitable for 'mount-file-system'.

When GUILE-MODULES-IN-CHROOT? is true, make core Guile modules available in
the new root.

When VOLATILE-ROOT? is true, the root file system is writable but any changes
to it are lost."
  (define root-mount-point?
    (match-lambda
     ((device _ "/" _ ...) #t)
     (_ #f)))

  (define root-fs-type
    (or (any (match-lambda
              ((device _ "/" type _ ...) type)
              (_ #f))
             mounts)
        "ext4"))

  (display "Welcome, this is GNU's early boot Guile.\n")
  (display "Use '--repl' for an initrd REPL.\n\n")

  (call-with-error-handling
   (lambda ()
     (mount-essential-file-systems)
     (let* ((args    (linux-command-line))
            (to-load (find-long-option "--load" args))
            (root    (find-long-option "--root" args)))

       (when (member "--repl" args)
         (start-repl))

       (display "loading kernel modules...\n")
       (for-each (compose load-linux-module*
                          (cut string-append "/modules/" <>))
                 linux-modules)

       (when qemu-guest-networking?
         (unless (configure-qemu-networking)
           (display "network interface is DOWN\n")))

       ;; Make /dev nodes.
       (make-essential-device-nodes)

       ;; Prepare the real root file system under /root.
       (unless (file-exists? "/root")
         (mkdir "/root"))
       (if root
           (mount-root-file-system (canonicalize-device-spec root)
                                   root-fs-type
                                   #:volatile-root? volatile-root?)
           (mount "none" "/root" "tmpfs"))

       (unless (file-exists? "/root/dev")
         (mkdir "/root/dev")
         (make-essential-device-nodes #:root "/root"))

       ;; Mount the specified file systems.
       (for-each mount-file-system
                 (remove root-mount-point? mounts))

       (when guile-modules-in-chroot?
         ;; Copy the directories that contain .scm and .go files so that the
         ;; child process in the chroot can load modules (we would bind-mount
         ;; them but for some reason that fails with EINVAL -- XXX).
         (mkdir-p "/root/share")
         (mkdir-p "/root/lib")
         (mount "none" "/root/share" "tmpfs")
         (mount "none" "/root/lib" "tmpfs")
         (copy-recursively "/share" "/root/share"
                           #:log (%make-void-port "w"))
         (copy-recursively "/lib" "/root/lib"
                           #:log (%make-void-port "w")))

       (if to-load
           (begin
             (switch-root "/root")
             (format #t "loading '~a'...\n" to-load)

             ;; TODO: Remove /lib, /share, and /loader.go.
             (primitive-load to-load)

             (format (current-error-port)
                     "boot program '~a' terminated, rebooting~%"
                     to-load)
             (sleep 2)
             (reboot))
           (begin
             (display "no boot file passed via '--load'\n")
             (display "entering a warm and cozy REPL\n")
             (start-repl)))))))

;;; linux-initrd.scm ends here
