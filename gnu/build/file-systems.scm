;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
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

(define-module (gnu build file-systems)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (guix build utils)
  #:use-module (guix build bournish)
  #:use-module ((guix build syscalls)
                #:hide (file-system-type))
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (system foreign)
  #:autoload   (system repl repl) (start-repl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (disk-partitions
            partition-label-predicate
            partition-uuid-predicate
            partition-luks-uuid-predicate
            find-partition-by-label
            find-partition-by-uuid
            find-partition-by-luks-uuid
            canonicalize-device-spec

            read-partition-label
            read-partition-uuid
            read-luks-partition-uuid

            bind-mount

            mount-flags->bit-mask
            check-file-system
            mount-file-system))

;;; Commentary:
;;;
;;; This modules provides tools to deal with disk partitions, and to mount and
;;; check file systems.
;;;
;;; Code:

(define (bind-mount source target)
  "Bind-mount SOURCE at TARGET."
  (mount source target "" MS_BIND))

(define (seek* fd/port offset whence)
  "Like 'seek' but return -1 instead of throwing to 'system-error' upon
EINVAL.  This makes it easier to catch cases like OFFSET being too large for
FD/PORT."
  (catch 'system-error
    (lambda ()
      (seek fd/port offset whence))
    (lambda args
      (if (= EINVAL (system-error-errno args))
          -1
          (apply throw args)))))

(define (read-superblock device offset size magic?)
  "Read a superblock of SIZE from OFFSET and DEVICE.  Return the raw
superblock on success, and #f if no valid superblock was found.  MAGIC?
takes a bytevector and returns #t when it's a valid superblock."
  (call-with-input-file device
    (lambda (port)
      (and (= offset (seek* port offset SEEK_SET))
           (let ((block (make-bytevector size)))
             (match (get-bytevector-n! port block 0 (bytevector-length block))
               ((? eof-object?)
                #f)
               ((? number? len)
                (and (= len (bytevector-length block))
                     (and (magic? block)
                          block)))))))))

(define null-terminated-latin1->string
  (cut latin1->string <> zero?))


;;;
;;; Ext2 file systems.
;;;

;; <http://www.nongnu.org/ext2-doc/ext2.html#DEF-SUPERBLOCK>.
;; TODO: Use "packed structs" from Guile-OpenGL or similar.

(define-syntax %ext2-endianness
  ;; Endianness of ext2 file systems.
  (identifier-syntax (endianness little)))

(define (ext2-superblock? sblock)
  "Return #t when SBLOCK is an ext2 superblock."
  (let ((magic (bytevector-u16-ref sblock 56 %ext2-endianness)))
    (= magic #xef53)))

(define (read-ext2-superblock device)
  "Return the raw contents of DEVICE's ext2 superblock as a bytevector, or #f
if DEVICE does not contain an ext2 file system."
  (read-superblock device 1024 264 ext2-superblock?))

(define (ext2-superblock-uuid sblock)
  "Return the UUID of ext2 superblock SBLOCK as a 16-byte bytevector."
  (sub-bytevector sblock 104 16))

(define (ext2-superblock-volume-name sblock)
  "Return the volume name of SBLOCK as a string of at most 16 characters, or
#f if SBLOCK has no volume name."
  (null-terminated-latin1->string (sub-bytevector sblock 120 16)))

(define (check-ext2-file-system device)
  "Return the health of an ext2 file system on DEVICE."
  (match (status:exit-val
          (system* "e2fsck" "-v" "-p" "-C" "0" device))
    (0 'pass)
    (1 'errors-corrected)
    (2 'reboot-required)
    (_ 'fatal-error)))


;;;
;;; Btrfs file systems.
;;;

;; <https://btrfs.wiki.kernel.org/index.php/On-disk_Format#Superblock>.

(define-syntax %btrfs-endianness
  ;; Endianness of btrfs file systems.
  (identifier-syntax (endianness little)))

(define (btrfs-superblock? sblock)
  "Return #t when SBLOCK is a btrfs superblock."
  (bytevector=? (sub-bytevector sblock 64 8)
                (string->utf8 "_BHRfS_M")))

(define (read-btrfs-superblock device)
  "Return the raw contents of DEVICE's btrfs superblock as a bytevector, or #f
if DEVICE does not contain a btrfs file system."
  (read-superblock device 65536 4096 btrfs-superblock?))

(define (btrfs-superblock-uuid sblock)
  "Return the UUID of a btrfs superblock SBLOCK as a 16-byte bytevector."
  (sub-bytevector sblock 32 16))

(define (btrfs-superblock-volume-name sblock)
  "Return the volume name of SBLOCK as a string of at most 256 characters, or
#f if SBLOCK has no volume name."
  (null-terminated-latin1->string (sub-bytevector sblock 299 256)))

(define (check-btrfs-file-system device)
  "Return the health of a btrfs file system on DEVICE."
  (match (status:exit-val
          (system* "btrfs" "device" "scan"))
    (0 'pass)
    (_ 'fatal-error)))


;;;
;;; FAT32 file systems.
;;;

;; <http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-107.pdf>.

(define (fat32-superblock? sblock)
  "Return #t when SBLOCK is a fat32 superblock."
  (bytevector=? (sub-bytevector sblock 82 8)
                (string->utf8 "FAT32   ")))

(define (read-fat32-superblock device)
  "Return the raw contents of DEVICE's fat32 superblock as a bytevector, or
#f if DEVICE does not contain a fat32 file system."
  (read-superblock device 0 90 fat32-superblock?))

(define (fat32-superblock-uuid sblock)
  "Return the Volume ID of a fat superblock SBLOCK as a 4-byte bytevector."
  (sub-bytevector sblock 67 4))

(define (fat32-superblock-volume-name sblock)
  "Return the volume name of SBLOCK as a string of at most 11 characters, or
#f if SBLOCK has no volume name.  The volume name is a latin1 string.
Trailing spaces are trimmed."
  (string-trim-right (latin1->string (sub-bytevector sblock 71 11) (lambda (c) #f)) #\space))

(define (check-fat-file-system device)
  "Return the health of a fat file system on DEVICE."
  (match (status:exit-val
          (system* "fsck.vfat" "-v" "-a" device))
    (0 'pass)
    (1 'errors-corrected)
    (_ 'fatal-error)))


;;;
;;; FAT16 file systems.
;;;

(define (fat16-superblock? sblock)
  "Return #t when SBLOCK is a fat16 boot record."
  (bytevector=? (sub-bytevector sblock 54 8)
                (string->utf8 "FAT16   ")))

(define (read-fat16-superblock device)
  "Return the raw contents of DEVICE's fat16 superblock as a bytevector, or
#f if DEVICE does not contain a fat16 file system."
  (read-superblock device 0 62 fat16-superblock?))

(define (fat16-superblock-uuid sblock)
  "Return the Volume ID of a fat superblock SBLOCK as a 4-byte bytevector."
  (sub-bytevector sblock 39 4))

(define (fat16-superblock-volume-name sblock)
  "Return the volume name of SBLOCK as a string of at most 11 characters, or
#f if SBLOCK has no volume name.  The volume name is a latin1 string.
Trailing spaces are trimmed."
  (string-trim-right (latin1->string (sub-bytevector sblock 43 11)
                                     (lambda (c) #f))
                     #\space))


;;;
;;; ISO9660 file systems.
;;;

;; <http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-119.pdf>.

(define (iso9660-superblock? sblock)
  "Return #t when SBLOCK is an iso9660 volume descriptor."
  (bytevector=? (sub-bytevector sblock 1 6)
                ;; Note: "\x01" is the volume descriptor format version
                (string->utf8 "CD001\x01")))

(define (read-iso9660-primary-volume-descriptor device offset)
  "Find and read the first primary volume descriptor, starting at OFFSET.
   Return #f if not found."
  (let* ((sblock    (read-superblock device offset 2048 iso9660-superblock?))
         (type-code (if sblock
                        (bytevector-u8-ref sblock 0)
                        (error (format #f
                                       "Could not read ISO9660 primary
volume descriptor from ~s"
                                       device)))))
    (match type-code
      (255 #f) ; Volume Descriptor Set Terminator.
      (1 sblock) ; Primary Volume Descriptor
      (_ (read-iso9660-primary-volume-descriptor device (+ offset 2048))))))

(define (read-iso9660-superblock device)
  "Return the raw contents of DEVICE's iso9660 primary volume descriptor
as a bytevector, or #f if DEVICE does not contain an iso9660 file system."
  ;; Start reading at sector 16.
  ;; Since we are not sure that the device contains an ISO9660 file system,
  ;; we have to find that out first.
  (if (read-superblock device (* 2048 16) 2048 iso9660-superblock?)
      (read-iso9660-primary-volume-descriptor device (* 2048 16))
      #f)) ; Device does not contain an iso9660 file system.

(define (iso9660-superblock-uuid sblock)
  "Return the modification time of an iso9660 primary volume descriptor
SBLOCK as a bytevector.  If that's not set, returns the creation time."
  ;; Drops GMT offset for compatibility with Grub, blkid and /dev/disk/by-uuid.
  ;; Compare Grub: "2014-12-02-19-30-23-00".
  ;; Compare blkid result: "2014-12-02-19-30-23-00".
  ;; Compare /dev/disk/by-uuid entry: "2014-12-02-19-30-23-00".
  (let* ((creation-time (sub-bytevector sblock 813 17))
         (modification-time (sub-bytevector sblock 830 17))
         (unset-time (make-bytevector 17 0))
         (time (if (bytevector=? unset-time modification-time)
                   creation-time
                   modification-time)))
    (sub-bytevector time 0 16))) ; strips GMT offset.

(define (iso9660-superblock-volume-name sblock)
  "Return the volume name of SBLOCK as a string.  The volume name is an ASCII
string.  Trailing spaces are trimmed."
  ;; Note: Valid characters are of the set "[0-9][A-Z]_" (ECMA-119 Appendix A)
  (string-trim-right (latin1->string (sub-bytevector sblock 40 32)
                                     (lambda (c) #f)) #\space))


;;;
;;; LUKS encrypted devices.
;;;

;; The LUKS header format is described in "LUKS On-Disk Format Specification":
;; <https://gitlab.com/cryptsetup/cryptsetup/wikis/Specification>.  We follow
;; version 1.2.1 of this document.

(define-syntax %luks-endianness
  ;; Endianness of LUKS headers.
  (identifier-syntax (endianness big)))

(define (luks-superblock? sblock)
  "Return #t when SBLOCK is a luks superblock."
  (define %luks-magic
    ;; The 'LUKS_MAGIC' constant.
    (u8-list->bytevector (append (map char->integer (string->list "LUKS"))
                                 (list #xba #xbe))))
  (let ((magic   (sub-bytevector sblock 0 6))
        (version (bytevector-u16-ref sblock 6 %luks-endianness)))
    (and (bytevector=? magic %luks-magic)
         (= version 1))))

(define (read-luks-header file)
  "Read a LUKS header from FILE.  Return the raw header on success, and #f if
not valid header was found."
  ;; Size in bytes of the LUKS header, including key slots.
  (read-superblock file 0 592 luks-superblock?))

(define (luks-header-uuid header)
  "Return the LUKS UUID from HEADER, as a 16-byte bytevector."
  ;; 40 bytes are reserved for the UUID, but in practice, it contains the 36
  ;; bytes of its ASCII representation.
  (let ((uuid (sub-bytevector header 168 36)))
    (string->uuid (utf8->string uuid))))


;;;
;;; Partition lookup.
;;;

(define (disk-partitions)
  "Return the list of device names corresponding to valid disk partitions."
  (define (partition? name major minor)
    ;; grub-mkrescue does some funny things for EFI support which
    ;; makes it a lot more difficult than one would expect to support
    ;; booting an ISO-9660 image from an USB flash drive.
    ;; For example there's a buggy (too small) hidden partition in it
    ;; which Linux mounts and then proceeds to fail while trying to
    ;; fall off the edge.
    ;; In any case, partition tables are supposed to be optional so
    ;; here we allow checking entire disks for file systems, too.
    (> major 2))                      ;ignore RAM disks and floppy disks

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
                 (if (partition? name major minor)
                     (loop (cons name parts))
                     (loop parts))))))))))

(define (ENOENT-safe proc)
  "Wrap the one-argument PROC such that ENOENT errors are caught and lead to a
warning and #f as the result."
  (lambda (device)
    (catch 'system-error
      (lambda ()
        (proc device))
      (lambda args
        ;; When running on the hand-made /dev,
        ;; 'disk-partitions' could return partitions for which
        ;; we have no /dev node.  Handle that gracefully.
        (let ((errno (system-error-errno args)))
          (cond ((= ENOENT errno)
                 (format (current-error-port)
                         "warning: device '~a' not found~%" device)
                 #f)
                ((= ENOMEDIUM errno)              ;for removable media
                 #f)
                ((= EIO errno)                    ;unreadable hardware like audio CDs
                 (format (current-error-port)
                         "warning: failed to read from device '~a'~%" device)
                 #f)
                (else
                 (apply throw args))))))))

(define (partition-field-reader read field)
  "Return a procedure that takes a device and returns the value of a FIELD in
the partition superblock or #f."
  (let ((read (ENOENT-safe read)))
    (lambda (device)
      (let ((sblock (read device)))
        (and sblock
             (field sblock))))))

(define (read-partition-field device partition-field-readers)
  "Returns the value of a FIELD in the partition superblock of DEVICE or #f. It
takes a list of PARTITION-FIELD-READERS and returns the result of the first
partition field reader that returned a value."
  (match (filter-map (cut apply <> (list device)) partition-field-readers)
    ((field . _) field)
    (_ #f)))

(define %partition-label-readers
  (list (partition-field-reader read-iso9660-superblock
                                iso9660-superblock-volume-name)
        (partition-field-reader read-ext2-superblock
                                ext2-superblock-volume-name)
        (partition-field-reader read-btrfs-superblock
                                btrfs-superblock-volume-name)
        (partition-field-reader read-fat32-superblock
                                fat32-superblock-volume-name)
        (partition-field-reader read-fat16-superblock
                                fat16-superblock-volume-name)))

(define %partition-uuid-readers
  (list (partition-field-reader read-iso9660-superblock
                                iso9660-superblock-uuid)
        (partition-field-reader read-ext2-superblock
                                ext2-superblock-uuid)
        (partition-field-reader read-btrfs-superblock
                                btrfs-superblock-uuid)
        (partition-field-reader read-fat32-superblock
                                fat32-superblock-uuid)
        (partition-field-reader read-fat16-superblock
                                fat16-superblock-uuid)))

(define read-partition-label
  (cut read-partition-field <> %partition-label-readers))

(define read-partition-uuid
  (cut read-partition-field <> %partition-uuid-readers))

(define luks-partition-field-reader
  (partition-field-reader read-luks-header luks-header-uuid))

(define read-luks-partition-uuid
  (cut read-partition-field <> (list luks-partition-field-reader)))

(define (partition-predicate reader =)
  "Return a predicate that returns true if the FIELD of partition header that
was READ is = to the given value."
  (lambda (expected)
    (lambda (device)
      (let ((actual (reader device)))
        (and actual
             (= actual expected))))))

(define partition-label-predicate
  (partition-predicate read-partition-label string=?))

(define partition-uuid-predicate
  (partition-predicate read-partition-uuid uuid=?))

(define luks-partition-uuid-predicate
  (partition-predicate luks-partition-field-reader uuid=?))

(define (find-partition predicate)
  "Return the first partition found that matches PREDICATE, or #f if none
were found."
  (lambda (expected)
    (find (predicate expected)
          (map (cut string-append "/dev/" <>)
               (disk-partitions)))))

(define find-partition-by-label
  (find-partition partition-label-predicate))

(define find-partition-by-uuid
  (find-partition partition-uuid-predicate))

(define find-partition-by-luks-uuid
  (find-partition luks-partition-uuid-predicate))


(define (canonicalize-device-spec spec)
  "Return the device name corresponding to SPEC, which can be a <uuid>, a
<file-system-label>, or a string (typically a /dev file name)."
  (define max-trials
    ;; Number of times we retry partition label resolution, 1 second per
    ;; trial.  Note: somebody reported a delay of 16 seconds (!) before their
    ;; USB key would be detected by the kernel, so we must wait for at least
    ;; this long.
    20)

  (define (resolve find-partition spec fmt)
    (let loop ((count 0))
      (let ((device (find-partition spec)))
        (or device
            ;; Some devices take a bit of time to appear, most notably USB
            ;; storage devices.  Thus, wait for the device to appear.
            (if (> count max-trials)
                (error "failed to resolve partition" (fmt spec))
                (begin
                  (format #t "waiting for partition '~a' to appear...~%"
                          (fmt spec))
                  (sleep 1)
                  (loop (+ 1 count))))))))

  (match spec
    ((? string?)
     ;; Nothing to do, but wait until SPEC shows up.
     (resolve identity spec identity))
    ((? file-system-label?)
     ;; Resolve the label.
     (resolve find-partition-by-label
              (file-system-label->string spec)
              identity))
    ((? uuid?)
     (resolve find-partition-by-uuid
              (uuid-bytevector spec)
              uuid->string))))

(define (check-file-system device type)
  "Run a file system check of TYPE on DEVICE."
  (define check-procedure
    (cond
     ((string-prefix? "ext" type) check-ext2-file-system)
     ((string-prefix? "btrfs" type) check-btrfs-file-system)
     ((string-suffix? "fat" type) check-fat-file-system)
     (else #f)))

  (if check-procedure
      (match (check-procedure device)
        ('pass
         #t)
        ('errors-corrected
         (format (current-error-port)
                 "File system check corrected errors on ~a; continuing~%"
                 device))
        ('reboot-required
         (format (current-error-port)
                 "File system check corrected errors on ~a; rebooting~%"
                 device)
         (sleep 3)
         (reboot))
        ('fatal-error
         (format (current-error-port) "File system check on ~a failed~%"
                 device)

         ;; Spawn a REPL only if someone would be able to interact with it.
         (when (isatty? (current-input-port))
           (format (current-error-port) "Spawning Bourne-like REPL.~%")

           ;; 'current-output-port' is typically connected to /dev/klog (in
           ;; PID 1), but here we want to make sure we talk directly to the
           ;; user.
           (with-output-to-file "/dev/console"
             (lambda ()
               (start-repl %bournish-language))))))
      (format (current-error-port)
              "No file system check procedure for ~a; skipping~%"
              device)))

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
      (('no-atime rest ...)
       (logior MS_NOATIME (loop rest)))
      (()
       0))))

(define* (mount-file-system fs #:key (root "/root"))
  "Mount the file system described by FS, a <file-system> object, under ROOT.

DEVICE, MOUNT-POINT, and TYPE must be strings; OPTIONS can be a string or #f;
FLAGS must be a list of symbols.  CHECK? is a Boolean indicating whether to
run a file system check."

  (define (mount-nfs source mount-point type flags options)
    (let* ((idx (string-rindex source #\:))
           (host-part (string-take source idx))
           ;; Strip [] from around host if present
           (host (match (string-split host-part (string->char-set "[]"))
                 (("" h "") h)
                 ((h) h)))
           (aa (match (getaddrinfo host "nfs") ((x . _) x)))
           (sa (addrinfo:addr aa))
           (inet-addr (inet-ntop (sockaddr:fam sa)
                                 (sockaddr:addr sa))))

      ;; Mounting an NFS file system requires passing the address
      ;; of the server in the addr= option
      (mount source mount-point type flags
             (string-append "addr="
                            inet-addr
                            (if options
                                (string-append "," options)
                                "")))))
  (let ((type        (file-system-type fs))
        (options     (file-system-options fs))
        (source      (canonicalize-device-spec (file-system-device fs)))
        (mount-point (string-append root "/"
                                    (file-system-mount-point fs)))
        (flags       (mount-flags->bit-mask (file-system-flags fs))))
    (when (file-system-check? fs)
      (check-file-system source type))

    ;; Create the mount point.  Most of the time this is a directory, but
    ;; in the case of a bind mount, a regular file or socket may be needed.
    (if (and (= MS_BIND (logand flags MS_BIND))
             (not (file-is-directory? source)))
        (unless (file-exists? mount-point)
          (mkdir-p (dirname mount-point))
          (call-with-output-file mount-point (const #t)))
        (mkdir-p mount-point))

    (cond
     ((string-prefix? "nfs" type)
      (mount-nfs source mount-point type flags options))
     (else
      (mount source mount-point type flags options)))

    ;; For read-only bind mounts, an extra remount is needed, as per
    ;; <http://lwn.net/Articles/281157/>, which still applies to Linux 4.0.
    (when (and (= MS_BIND (logand flags MS_BIND))
               (= MS_RDONLY (logand flags MS_RDONLY)))
      (let ((flags (logior MS_BIND MS_REMOUNT MS_RDONLY)))
        (mount source mount-point type flags #f)))))

;;; file-systems.scm ends here
