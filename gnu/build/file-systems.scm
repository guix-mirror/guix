;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 David C. Trudgian <dave@trudgian.net>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
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
            mount-file-system

            swap-space->flags-bit-mask))

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

(define (bytevector-utf16-length bv)
  "Given a bytevector BV containing a NUL-terminated UTF16-encoded string,
determine where the NUL terminator is and return its index.  If there's no
NUL terminator, return the size of the bytevector."
  (let ((length (bytevector-length bv)))
    (let loop ((index 0))
      (if (< index length)
          (if (zero? (bytevector-u16-ref bv index 'little))
              index
              (loop (+ index 2)))
          length))))

(define* (bytevector->u16-list bv endianness #:optional (index 0))
  (if (< index (bytevector-length bv))
      (cons (bytevector-u16-ref bv index endianness)
            (bytevector->u16-list bv endianness (+ index 2)))
      '()))

;; The initrd doesn't have iconv data, so do the conversion ourselves.
(define (utf16->string bv endianness)
  (list->string
   (map integer->char
        (reverse
         (let loop ((remainder (bytevector->u16-list bv endianness))
                    (result '()))
             (match remainder
              (() result)
              ((a) (cons a result))
              ((a b x ...)
               (if (and (>= a #xD800) (< a #xDC00) ; high surrogate
                        (>= b #xDC00) (< b #xE000)) ; low surrogate
                   (loop x (cons (+ #x10000
                                    (* #x400 (- a #xD800))
                                    (- b #xDC00))
                                 result))
                   (loop (cons b x) (cons a result))))))))))

(define (null-terminated-utf16->string bv endianness)
  (utf16->string (sub-bytevector bv 0 (bytevector-utf16-length bv))
                 endianness))


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
  "Return the volume name of ext2 superblock SBLOCK as a string of at most 16
characters, or #f if SBLOCK has no volume name."
  (null-terminated-latin1->string (sub-bytevector sblock 120 16)))

(define (check-ext2-file-system device force? repair)
  "Return the health of an unmounted ext2 file system on DEVICE.  If FORCE? is
true, check the file system even if it's marked as clean.  If REPAIR is false,
do not write to the file system to fix errors.  If it's #t, fix all
errors.  Otherwise, fix only those considered safe to repair automatically."
  (match (status:exit-val
          (apply system* `("e2fsck" "-v" "-C" "0"
                           ,@(if force? '("-f") '())
                           ,@(match repair
                               (#f '("-n"))
                               (#t '("-y"))
                               (_  '("-p")))
                           ,device)))
    (0 'pass)
    (1 'errors-corrected)
    (2 'reboot-required)
    (_ 'fatal-error)))


;;;
;;; Linux swap.
;;;

;; Linux "swap space" is not a file system but it has a UUID and volume name,
;; like actual file systems, and we want to be able to look up swap partitions
;; by UUID and by label.

(define %linux-swap-magic
  (string->utf8 "SWAPSPACE2"))

;; Like 'PAGE_SIZE' in Linux, arch/x86/include/asm/page.h.
;; XXX: This is always 4K on x86_64, i386, and ARMv7.  However, on AArch64,
;; this is determined by 'CONFIG_ARM64_PAGE_SHIFT' in the kernel, which is 12
;; by default (4K) but can be 14 or 16.
(define %page-size 4096)

(define (linux-swap-superblock? sblock)
  "Return #t when SBLOCK is an linux-swap superblock."
  (and (= (bytevector-length sblock) %page-size)
       (bytevector=? (sub-bytevector sblock (- %page-size 10) 10)
                     %linux-swap-magic)))

(define (read-linux-swap-superblock device)
  "Return the raw contents of DEVICE's linux-swap superblock as a bytevector, or #f
if DEVICE does not contain an linux-swap file system."
  (read-superblock device 0 %page-size linux-swap-superblock?))

;; See 'union swap_header' in 'include/linux/swap.h'.

(define (linux-swap-superblock-uuid sblock)
  "Return the UUID of Linux-swap superblock SBLOCK as a 16-byte bytevector."
  (sub-bytevector sblock (+ 1024 4 4 4) 16))

(define (linux-swap-superblock-volume-name sblock)
  "Return the label of Linux-swap superblock SBLOCK as a string."
  (null-terminated-latin1->string
   (sub-bytevector sblock (+ 1024 4 4 4 16) 16)))

(define (swap-space->flags-bit-mask swap)
  "Return the number suitable for the 'flags' argument of 'mount'
that corresponds to the swap-space SWAP."
  (define prio-flag
    (let ((p (swap-space-priority swap))
          (max (ash SWAP_FLAG_PRIO_MASK (- SWAP_FLAG_PRIO_SHIFT))))
      (if p
          (logior SWAP_FLAG_PREFER
                  (ash (cond
                        ((< p 0)
                         (begin (warning
                                 (G_ "Given swap priority ~a is
negative, defaulting to 0.~%") p)
                                0))
                        ((> p max)
                         (begin (warning
                                 (G_ "Limiting swap priority ~a to
~a.~%")
                                 p max)
                                max))
                        (else p))
                       SWAP_FLAG_PRIO_SHIFT))
          0)))
  (define delayed-flag
    (if (swap-space-discard? swap)
        SWAP_FLAG_DISCARD
        0))
  (logior prio-flag delayed-flag))



;;;
;;; Bcachefs file systems.
;;;

;; <https://evilpiepirate.org/git/bcachefs-tools.git/tree/libbcachefs/bcachefs_format.h>

(define-syntax %bcachefs-endianness
  ;; Endianness of bcachefs file systems.
  (identifier-syntax (endianness little)))

(define (bcachefs-superblock? sblock)
  "Return #t when SBLOCK is an bcachefs superblock."
  (bytevector=? (sub-bytevector sblock 24 16)
                #vu8(#xc6 #x85 #x73 #xf6 #x4e #x1a #x45 #xca
                     #x82 #x65 #xf5 #x7f #x48 #xba #x6d #x81)))

(define (read-bcachefs-superblock device)
  "Return the raw contents of DEVICE's bcachefs superblock as a bytevector, or #f
if DEVICE does not contain a bcachefs file system."
  ;; Field offsets & lengths, in bytes.  There are more (and the superblock is
  ;; extensible) but we need only some basic information here:
  ;;  0 16 bch_csum
  ;; 16  8 version
  ;; 24 16 magic
  ;; 40 16 uuid               ← ‘internal’: you probably don't want this one
  ;; 56 16 user_uuid          ← ‘external’: user-visible one by which to mount
  ;; 72 32 label
  ;; Assume a sane file system: ignore the back-up superblock & checksums.
  (read-superblock device 4096 104 bcachefs-superblock?))

(define (bcachefs-superblock-external-uuid sblock)
  "Return the external UUID of bcachefs superblock SBLOCK as a 16-byte
bytevector."
  (sub-bytevector sblock 56 16))

(define (bcachefs-superblock-volume-name sblock)
  "Return the volume name of bcachefs superblock SBLOCK as a string of at most
32 characters, or #f if SBLOCK has no volume name."
  (null-terminated-latin1->string (sub-bytevector sblock 72 32)))

(define (check-bcachefs-file-system device force? repair)
  "Return the health of an unmounted bcachefs file system on DEVICE.  If FORCE?
is true, check the file system even if it's marked as clean.  If REPAIR is
false, do not write to the file system to fix errors.  If it's #t, fix all
errors. Otherwise, fix only those considered safe to repair automatically."
  (let ((ignored-bits (logior 2))       ; DEVICE was mounted read-only
        (status
         ;; A number, or #f on abnormal termination (e.g., assertion failure).
         (status:exit-val
          (apply system* `("bcachefs" "fsck" "-v"
                           ,@(if force? '("-f") '())
                           ,@(match repair
                               (#f '("-n"))
                               (#t '("-y"))
                               (_  '("-p")))
                           ;; Make each multi-device member a separate argument.
                           ,@(string-split device #\:))))))
    (match (and=> status (cut logand <> (lognot ignored-bits)))
      (0 'pass)
      (1 'errors-corrected)
      (_ 'fatal-error))))


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
  "Return the volume name of btrfs superblock SBLOCK as a string of at most 256
characters, or #f if SBLOCK has no volume name."
  (null-terminated-latin1->string (sub-bytevector sblock 299 256)))

(define (check-btrfs-file-system device force? repair)
  "Return the health of an unmounted btrfs file system on DEVICE.  If FORCE? is
false, return 'PASS unconditionally as btrfs claims no need for off-line checks.
When FORCE? is true, do perform a real check.  This is not recommended!  See
@uref{https://bugzilla.redhat.com/show_bug.cgi?id=625967#c8}.  If REPAIR is
false, do not write to DEVICE.  If it's #t, fix any errors found.  Otherwise,
fix only those considered safe to repair automatically."
  (if force?
      (match (status:exit-val
              (apply system* `("btrfs" "check" "--progress"
                               ;; Btrfs's ‘--force’ is not relevant to us here.
                               ,@(match repair
                                   ;; Upstream considers ALL repairs dangerous
                                   ;; and will warn the user at run time.
                                   (#t '("--repair"))
                                   (_  '("--readonly" ; a no-op for clarity
                                         ;; A 466G file system with 180G used is
                                         ;; enough to kill btrfs with 6G of RAM.
                                         "--mode" "lowmem")))
                               ,device)))
        (0 'pass)
        (_ 'fatal-error))
      'pass))


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
  "Return the volume name of fat superblock SBLOCK as a string of at most 11
characters, or #f if SBLOCK has no volume name.  The volume name is a latin1
string.  Trailing spaces are trimmed."
  (string-trim-right (latin1->string (sub-bytevector sblock 71 11) (lambda (c) #f)) #\space))

(define (check-fat-file-system device force? repair)
  "Return the health of an unmounted FAT file system on DEVICE.  FORCE? is
ignored: a full file system scan is always performed.  If REPAIR is false, do
not write to the file system to fix errors. Otherwise, automatically fix them
using the least destructive approach."
  (match (status:exit-val
          (apply system* `("fsck.vfat" "-v"
                           ,@(match repair
                               (#f '("-n"))
                               (_  '("-a"))) ; no 'safe/#t distinction
                           ,device)))
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
  "Return the volume name of fat superblock SBLOCK as a string of at most 11
characters, or #f if SBLOCK has no volume name.  The volume name is a latin1
string.  Trailing spaces are trimmed."
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
  "Return the volume name of iso9660 superblock SBLOCK as a string.  The volume
name is an ASCII string.  Trailing spaces are trimmed."
  ;; Note: Valid characters are of the set "[0-9][A-Z]_" (ECMA-119 Appendix A)
  (string-trim-right (latin1->string (sub-bytevector sblock 40 32)
                                     (lambda (c) #f)) #\space))


;;;
;;; JFS file systems.
;;;

;; Taken from <linux-libre>/fs/jfs/jfs_superblock.h.

(define-syntax %jfs-endianness
  ;; Endianness of JFS file systems.
  (identifier-syntax (endianness little)))

(define (jfs-superblock? sblock)
  "Return #t when SBLOCK is a JFS superblock."
  (bytevector=? (sub-bytevector sblock 0 4)
                (string->utf8 "JFS1")))

(define (read-jfs-superblock device)
  "Return the raw contents of DEVICE's JFS superblock as a bytevector, or #f
if DEVICE does not contain a JFS file system."
  (read-superblock device 32768 184 jfs-superblock?))

(define (jfs-superblock-uuid sblock)
  "Return the UUID of JFS superblock SBLOCK as a 16-byte bytevector."
  (sub-bytevector sblock 136 16))

(define (jfs-superblock-volume-name sblock)
  "Return the volume name of JFS superblock SBLOCK as a string of at most 16
characters, or #f if SBLOCK has no volume name."
  (null-terminated-latin1->string (sub-bytevector sblock 152 16)))

(define (check-jfs-file-system device force? repair)
  "Return the health of an unmounted JFS file system on DEVICE.  If FORCE? is
true, check the file system even if it's marked as clean.  If REPAIR is false,
do not write to the file system to fix errors, and replay the transaction log
only if FORCE?  is true. Otherwise, replay the transaction log before checking
and automatically fix found errors."
  (match (status:exit-val
          (apply system*
                 `("jfs_fsck" "-v"
                   ;; The ‘LEVEL’ logic is convoluted.  To quote fsck/xchkdsk.c
                   ;; (‘-p’, ‘-a’, and ‘-r’ are aliases in every way):
                   ;; “If -f was chosen, have it override [-p] by [forcing] a
                   ;;  check regardless of the outcome after the log is
                   ;;  replayed”.
                   ;; “If -n is specified by itself, don't replay the journal.
                   ;;  If -n is specified with [-p], replay the journal but
                   ;;  don't make any other changes”.
                   ,@(if force? '("-f") '())
                   ,@(match repair
                       (#f '("-n"))
                       (_  '("-p"))) ; no 'safe/#t distinction
                   ,device)))
    (0 'pass)
    (1 'errors-corrected)
    (2 'reboot-required)
    (_ 'fatal-error)))


;;;
;;; F2FS (Flash-Friendly File System)
;;;

;;; https://git.kernel.org/pub/scm/linux/kernel/git/jaegeuk/f2fs.git/tree/include/linux/f2fs_fs.h
;;; (but using xxd proved to be simpler)

(define-syntax %f2fs-endianness
  ;; Endianness of F2FS file systems
  (identifier-syntax (endianness little)))

;; F2FS actually stores two adjacent copies of the superblock.
;; should we read both?
(define (f2fs-superblock? sblock)
  "Return #t when SBLOCK is an F2FS superblock."
  (let ((magic (bytevector-u32-ref sblock 0 %f2fs-endianness)))
    (= magic #xF2F52010)))

(define (read-f2fs-superblock device)
  "Return the raw contents of DEVICE's F2FS superblock as a bytevector, or #f
if DEVICE does not contain an F2FS file system."
  (read-superblock device
                   ;; offset of magic in first copy
                   #x400
                   ;; difference between magic of second
                   ;; and first copies
                   (- #x1400 #x400)
                   f2fs-superblock?))

(define (f2fs-superblock-uuid sblock)
  "Return the UUID of F2FS superblock SBLOCK as a 16-byte bytevector."
  (sub-bytevector sblock
                  (- (+ #x460 12)
                     ;; subtract superblock offset
                     #x400)
                  16))

(define (f2fs-superblock-volume-name sblock)
  "Return the volume name of F2FS superblock SBLOCK as a string of at most 512
characters, or #f if SBLOCK has no volume name."
  (null-terminated-utf16->string
   (sub-bytevector sblock (- (+ #x470 12) #x400) 512)
   %f2fs-endianness))

(define (check-f2fs-file-system device force? repair)
  "Return the health of an unmuounted F2FS file system on DEVICE.  If FORCE? is
true, check the file system even if it's marked as clean.  If either FORCE? or
REPAIR are true, automatically fix found errors."
  ;; There's no ‘-n’ equivalent (‘--dry-run’ does not disable writes).
  ;; ’-y’ is an alias of ‘-f’.  The man page is bad: read main.c.
  (when (and force? (not repair))
    (format (current-error-port)
            "warning: forced check of F2FS ~a implies repairing any errors~%"
            device))
  (match (status:exit-val
          (apply system* `("fsck.f2fs"
                           ,@(if force? '("-f") '())
                           ,@(if repair '("-p") '("--dry-run"))
                           ,device)))
    ;; 0 and -1 are the only two possibilities according to the man page.
    (0 'pass)
    (_ 'fatal-error)))


;;;
;;; LUKS encrypted devices.
;;;

;; The LUKS header format is described in "LUKS On-Disk Format Specification":
;; <https://gitlab.com/cryptsetup/cryptsetup/wikis/Specification>.  We follow
;; version 1.2.1 of this document.

;; The LUKS2 header format is described in "LUKS2 On-Disk Format Specification":
;; <https://gitlab.com/cryptsetup/LUKS2-docs/blob/master/luks2_doc_wip.pdf>.
;; It is a WIP document.

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
         (or (= version 1) (= version 2)))))

(define (read-luks-header file)
  "Read a LUKS header from FILE.  Return the raw header on success, and #f if
not valid header was found."
  ;; Size in bytes of the LUKS binary header, which includes key slots in
  ;; LUKS1.  In LUKS2 the binary header is partially backward compatible, so
  ;; that UUID can be extracted as for LUKS1. Keyslots and other metadata are
  ;; not part of this header in LUKS2, but are included in the JSON metadata
  ;; area that follows.
  (read-superblock file 0 592 luks-superblock?))

(define (luks-header-uuid header)
  "Return the LUKS UUID from HEADER, as a 16-byte bytevector."
  ;; 40 bytes are reserved for the UUID, but in practice, it contains the 36
  ;; bytes of its ASCII representation.
  (let ((uuid (sub-bytevector header 168 36)))
    (string->uuid (utf8->string uuid))))


;;;
;;; NTFS file systems.
;;;

;; Taken from <linux-libre>/fs/ntfs/layout.h

(define-syntax %ntfs-endianness
  ;; Endianness of NTFS file systems.
  (identifier-syntax (endianness little)))

(define (ntfs-superblock? sblock)
  "Return #t when SBLOCK is a NTFS superblock."
  (bytevector=? (sub-bytevector sblock 3 8)
                (string->utf8 "NTFS    ")))

(define (read-ntfs-superblock device)
  "Return the raw contents of DEVICE's NTFS superblock as a bytevector, or #f
if DEVICE does not contain a NTFS file system."
  (read-superblock device 0 511 ntfs-superblock?))

(define (ntfs-superblock-uuid sblock)
  "Return the UUID of NTFS superblock SBLOCK as a 8-byte bytevector."
  (sub-bytevector sblock 72 8))

;; TODO: Add ntfs-superblock-volume-name.  The partition label is not stored
;; in the BOOT SECTOR like the UUID, but in the MASTER FILE TABLE, which seems
;; way harder to access.

(define (check-ntfs-file-system device force? repair)
  "Return the health of an unmounted NTFS file system on DEVICE.  FORCE? is
ignored: a full check is always performed.  Repair is not possible: if REPAIR is
true and the volume has been repaired by an external tool, clear the volume
dirty flag to indicate that it's now safe to mount."
  (match (status:exit-val
          (apply system* `("ntfsfix"
                           ,@(if repair '("--clear-dirty") '("--no-action"))
                           ,device)))
    (0 'pass)
    (_ 'fatal-error)))



;;;
;;; XFS file systems.
;;;

;; <https://git.kernel.org/pub/scm/fs/xfs/xfs-documentation.git/tree/design/XFS_Filesystem_Structure/allocation_groups.asciidoc>

(define-syntax %xfs-endianness
  ;; Endianness of XFS file systems.
  (identifier-syntax (endianness big)))

(define (xfs-superblock? sblock)
  "Return #t when SBLOCK is an XFS superblock."
  (bytevector=? (sub-bytevector sblock 0 4)
                (string->utf8 "XFSB")))

(define (read-xfs-superblock device)
  "Return the raw contents of DEVICE's XFS superblock as a bytevector, or #f
if DEVICE does not contain an XFS file system."
  (read-superblock device 0 120 xfs-superblock?))

(define (xfs-superblock-uuid sblock)
  "Return the UUID of XFS superblock SBLOCK as a 16-byte bytevector."
  (sub-bytevector sblock 32 16))

(define (xfs-superblock-volume-name sblock)
  "Return the volume name of XFS superblock SBLOCK as a string of at most 12
characters, or #f if SBLOCK has no volume name."
  (null-terminated-latin1->string (sub-bytevector sblock 108 12)))

(define (check-xfs-file-system device force? repair)
  "Return the health of an unmounted XFS file system on DEVICE.  If FORCE? is
false, return 'PASS unconditionally as XFS claims no need for off-line checks.
When FORCE? is true, do perform a thorough check.  If REPAIR is false, do not
write to DEVICE.  If it's #t, replay the log, check, and fix any errors found.
Otherwise, only replay the log, and check without attempting further repairs."
  (define (xfs_repair)
    (status:exit-val
     (apply system* `("xfs_repair" "-Pv"
                      ,@(match repair
                          (#t '("-e"))
                          (_  '("-n"))) ; will miss some errors
                      ,device))))
  (if force?
      ;; xfs_repair fails with exit status 2 if the log is dirty, which is
      ;; likely in situations where you're running xfs_repair.  Only the kernel
      ;; can replay the log by {,un}mounting it cleanly.
      (match (let ((status (xfs_repair)))
               (if (and repair (eq? 2 status))
                   (let ((target "/replay-XFS-log"))
                     ;; The kernel helpfully prints a ‘Mounting…’ notice for us.
                     (mkdir target)
                     (mount device target "xfs")
                     (umount target)
                     (rmdir target)
                     (xfs_repair))
                   status))
        (0 'pass)
        (4 'errors-corrected)
        (_ 'fatal-error))
      'pass))


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
  "Wrap the one-argument PROC such that ENOENT, EIO, and ENOMEDIUM errors are
caught and lead to a warning and #f as the result."
  (lambda (device)
    (catch 'system-error
      (lambda ()
        (proc device))
      (lambda args
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
  (lambda (device)
    (let ((sblock (read device)))
      (and sblock
           (field sblock)))))

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
        (partition-field-reader read-linux-swap-superblock
                                linux-swap-superblock-volume-name)
        (partition-field-reader read-bcachefs-superblock
                                bcachefs-superblock-volume-name)
        (partition-field-reader read-btrfs-superblock
                                btrfs-superblock-volume-name)
        (partition-field-reader read-fat32-superblock
                                fat32-superblock-volume-name)
        (partition-field-reader read-fat16-superblock
                                fat16-superblock-volume-name)
        (partition-field-reader read-jfs-superblock
                                jfs-superblock-volume-name)
        (partition-field-reader read-f2fs-superblock
                                f2fs-superblock-volume-name)
        (partition-field-reader read-xfs-superblock
                                xfs-superblock-volume-name)))

(define %partition-uuid-readers
  (list (partition-field-reader read-iso9660-superblock
                                iso9660-superblock-uuid)
        (partition-field-reader read-ext2-superblock
                                ext2-superblock-uuid)
        (partition-field-reader read-linux-swap-superblock
                                linux-swap-superblock-uuid)
        (partition-field-reader read-bcachefs-superblock
                                bcachefs-superblock-external-uuid)
        (partition-field-reader read-btrfs-superblock
                                btrfs-superblock-uuid)
        (partition-field-reader read-fat32-superblock
                                fat32-superblock-uuid)
        (partition-field-reader read-fat16-superblock
                                fat16-superblock-uuid)
        (partition-field-reader read-jfs-superblock
                                jfs-superblock-uuid)
        (partition-field-reader read-f2fs-superblock
                                f2fs-superblock-uuid)
        (partition-field-reader read-ntfs-superblock
                                ntfs-superblock-uuid)
        (partition-field-reader read-xfs-superblock
                                xfs-superblock-uuid)))

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
  ;; When running on the hand-made /dev, 'disk-partitions' could return
  ;; partitions for which we have no /dev node.  Handle that gracefully.
  (let ((reader (ENOENT-safe reader)))
    (lambda (expected)
      (lambda (device)
        (let ((actual (reader device)))
          (and actual
               (= actual expected)))))))

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
<file-system-label>, or a string (typically a /dev file name or an nfs-root
containing ':/')."
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
     (if (string-contains spec ":/")
         spec                  ; do not resolve NFS devices
         ;; Nothing to do, but wait until SPEC shows up.
         (resolve identity spec identity)))
    ((? file-system-label?)
     ;; Resolve the label.
     (resolve find-partition-by-label
              (file-system-label->string spec)
              identity))
    ((? uuid?)
     (resolve find-partition-by-uuid
              (uuid-bytevector spec)
              uuid->string))))

(define (check-file-system device type force? repair)
  "Check an unmounted TYPE file system on DEVICE.  Do nothing but warn if it is
mounted.  If FORCE? is true, check even when considered unnecessary.  If REPAIR
is false, try not to write to DEVICE at all.  If it's #t, try to fix all errors
found.  Otherwise, fix only those considered safe to repair automatically.  Not
all TYPEs support all values or combinations of FORCE? and REPAIR.  Don't throw
an exception in such cases but perform the nearest sane action."
  (define check-procedure
    (cond
     ((string-prefix? "ext" type) check-ext2-file-system)
     ((string-prefix? "bcachefs" type) check-bcachefs-file-system)
     ((string-prefix? "btrfs" type) check-btrfs-file-system)
     ((string-suffix? "fat" type) check-fat-file-system)
     ((string-prefix? "jfs" type) check-jfs-file-system)
     ((string-prefix? "f2fs" type) check-f2fs-file-system)
     ((string-prefix? "ntfs" type) check-ntfs-file-system)
     ((string-prefix? "nfs" type) (const 'pass))
     ((string-prefix? "xfs" type) check-xfs-file-system)
     (else #f)))

  (if check-procedure
      (let ((mount (find (lambda (mount)
                           (string=? device (mount-source mount)))
                         (mounts))))
        (if mount
            (format (current-error-port)
                    "Refusing to check ~a file system already mounted at ~a~%"
                    device (mount-point mount))
            (match (check-procedure device force? repair)
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

               ;; Spawn a REPL only if someone might interact with it.
               (when (isatty? (current-input-port))
                 (format (current-error-port) "Spawning Bourne-like REPL.~%")

                 ;; 'current-output-port' is typically connected to /dev/klog
                 ;; (in PID 1), but here we want to make sure we talk directly
                 ;; to the user.
                 (with-output-to-file "/dev/console"
                   (lambda ()
                     (start-repl %bournish-language))))))))
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
      (('strict-atime rest ...)
       (logior MS_STRICTATIME (loop rest)))
      (('lazy-time rest ...)
       (logior MS_LAZYTIME (loop rest)))
      (()
       0))))

(define* (mount-file-system fs #:key (root "/root")
                            (check? (file-system-check? fs))
                            (skip-check-if-clean?
                             (file-system-skip-check-if-clean? fs))
                            (repair (file-system-repair fs)))
  "Mount the file system described by FS, a <file-system> object, under ROOT."

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
  (let* ((type    (file-system-type fs))
         (source  (canonicalize-device-spec (file-system-device fs)))
         (target  (string-append root "/"
                                 (file-system-mount-point fs)))
         (flags   (logior (mount-flags->bit-mask (file-system-flags fs))

                          ;; For bind mounts, preserve the original flags such
                          ;; as MS_NOSUID, etc.  Failing to do that, the
                          ;; MS_REMOUNT call below fails with EPERM.
                          ;; See <https://bugs.gnu.org/46292>
                          (if (memq 'bind-mount (file-system-flags fs))
                              (statfs-flags->mount-flags
                               (file-system-mount-flags (statfs source)))
                              0)))
         (options (file-system-options fs)))
    (when check?
      (check-file-system source type (not skip-check-if-clean?) repair))

    (catch 'system-error
      (lambda ()
        ;; Create the mount point.  Most of the time this is a directory, but
        ;; in the case of a bind mount, a regular file or socket may be
        ;; needed.
        (if (and (= MS_BIND (logand flags MS_BIND))
                 (not (file-is-directory? source)))
            (unless (file-exists? target)
              (mkdir-p (dirname target))
              (call-with-output-file target (const #t)))
            (mkdir-p target))

        (cond
         ((string-prefix? "nfs" type)
          (mount-nfs source target type flags options))
         (else
          (mount source target type flags options)))

        ;; For read-only bind mounts, an extra remount is needed, as per
        ;; <http://lwn.net/Articles/281157/>, which still applies to Linux
        ;; 4.0.
        (when (and (= MS_BIND (logand flags MS_BIND))
                   (= MS_RDONLY (logand flags MS_RDONLY)))
          (let ((flags (logior MS_REMOUNT flags)))
            (mount source target type flags options))))
      (lambda args
        (or (file-system-mount-may-fail? fs)
            (apply throw args))))))

;;; file-systems.scm ends here
