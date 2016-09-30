;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix build utils)
  #:use-module (guix build bournish)
  #:use-module (guix build syscalls)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
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

            uuid->string
            string->uuid

            bind-mount

            mount-flags->bit-mask
            check-file-system
            mount-file-system)
  #:re-export (mount
               umount
               MS_BIND
               MS_MOVE
               MS_RDONLY))

;;; Commentary:
;;;
;;; This modules provides tools to deal with disk partitions, and to mount and
;;; check file systems.
;;;
;;; Code:

;; 'mount' is already defined in the statically linked Guile used for initial
;; RAM disks, in which case the bindings in (guix build syscalls) do not work
;; (the FFI bindings do not work there).  Override them in that case.
(when (module-defined? the-scm-module 'mount)
  (set! mount (@ (guile) mount))
  (set! umount (@ (guile) umount)))

(define (bind-mount source target)
  "Bind-mount SOURCE at TARGET."
  (mount source target "" MS_BIND))


;;;
;;; Ext2 file systems.
;;;

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


;;;
;;; LUKS encrypted devices.
;;;

;; The LUKS header format is described in "LUKS On-Disk Format Specification":
;; <http://wiki.cryptsetup.googlecode.com/git/LUKS-standard/>.  We follow
;; version 1.2.1 of this document.

(define-syntax %luks-endianness
  ;; Endianness of LUKS headers.
  (identifier-syntax (endianness big)))

(define-syntax %luks-header-size
  ;; Size in bytes of the LUKS header, including key slots.
  (identifier-syntax 592))

(define %luks-magic
  ;; The 'LUKS_MAGIC' constant.
  (u8-list->bytevector (append (map char->integer (string->list "LUKS"))
                               (list #xba #xbe))))

(define (sub-bytevector bv start size)
  "Return a copy of the SIZE bytes of BV starting from offset START."
  (let ((result (make-bytevector size)))
    (bytevector-copy! bv start result 0 size)
    result))

(define (read-luks-header file)
  "Read a LUKS header from FILE.  Return the raw header on success, and #f if
not valid header was found."
  (call-with-input-file file
    (lambda (port)
      (let ((header (make-bytevector %luks-header-size)))
        (match (get-bytevector-n! port header 0 (bytevector-length header))
          ((? eof-object?)
           #f)
          ((? number? len)
           (and (= len (bytevector-length header))
                (let ((magic   (sub-bytevector header 0 6)) ;XXX: inefficient
                      (version (bytevector-u16-ref header 6 %luks-endianness)))
                  (and (bytevector=? magic %luks-magic)
                       (= version 1)
                       header)))))))))

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
        (if (= ENOENT (system-error-errno args))
            (begin
              (format (current-error-port)
                      "warning: device '~a' not found~%" device)
              #f)
            (apply throw args))))))

(define (partition-predicate read field =)
  "Return a predicate that returns true if the FIELD of partition header that
was READ is = to the given value."
  (let ((read (ENOENT-safe read)))
    (lambda (expected)
      "Return a procedure that, when applied to a partition name such as \"sda1\",
returns #t if that partition's volume name is LABEL."
      (lambda (part)
        (let* ((device (string-append "/dev/" part))
               (sblock (read device)))
          (and sblock
               (let ((actual (field sblock)))
                 (and actual
                      (= actual expected)))))))))

(define partition-label-predicate
  (partition-predicate read-ext2-superblock
                       ext2-superblock-volume-name
                       string=?))

(define partition-uuid-predicate
  (partition-predicate read-ext2-superblock
                       ext2-superblock-uuid
                       bytevector=?))

(define partition-luks-uuid-predicate
  (partition-predicate read-luks-header
                       luks-header-uuid
                       bytevector=?))

(define (find-partition-by-label label)
  "Return the first partition found whose volume name is LABEL, or #f if none
were found."
  (and=> (find (partition-label-predicate label)
               (disk-partitions))
         (cut string-append "/dev/" <>)))

(define (find-partition-by-uuid uuid)
  "Return the first partition whose unique identifier is UUID (a bytevector),
or #f if none was found."
  (and=> (find (partition-uuid-predicate uuid)
               (disk-partitions))
         (cut string-append "/dev/" <>)))

(define (find-partition-by-luks-uuid uuid)
  "Return the first LUKS partition whose unique identifier is UUID (a bytevector),
or #f if none was found."
  (and=> (find (partition-luks-uuid-predicate uuid)
               (disk-partitions))
         (cut string-append "/dev/" <>)))


;;;
;;; UUIDs.
;;;

(define-syntax %network-byte-order
  (identifier-syntax (endianness big)))

(define (uuid->string uuid)
  "Convert UUID, a 16-byte bytevector, to its string representation, something
like \"6b700d61-5550-48a1-874c-a3d86998990e\"."
  ;; See <https://tools.ietf.org/html/rfc4122>.
  (let ((time-low  (bytevector-uint-ref uuid 0 %network-byte-order 4))
        (time-mid  (bytevector-uint-ref uuid 4 %network-byte-order 2))
        (time-hi   (bytevector-uint-ref uuid 6 %network-byte-order 2))
        (clock-seq (bytevector-uint-ref uuid 8 %network-byte-order 2))
        (node      (bytevector-uint-ref uuid 10 %network-byte-order 6)))
    (format #f "~8,'0x-~4,'0x-~4,'0x-~4,'0x-~12,'0x"
            time-low time-mid time-hi clock-seq node)))

(define %uuid-rx
  ;; The regexp of a UUID.
  (make-regexp "^([[:xdigit:]]{8})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{12})$"))

(define (string->uuid str)
  "Parse STR as a DCE UUID (see <https://tools.ietf.org/html/rfc4122>) and
return its contents as a 16-byte bytevector.  Return #f if STR is not a valid
UUID representation."
  (and=> (regexp-exec %uuid-rx str)
         (lambda (match)
           (letrec-syntax ((hex->number
                            (syntax-rules ()
                              ((_ index)
                               (string->number (match:substring match index)
                                               16))))
                           (put!
                            (syntax-rules ()
                              ((_ bv index (number len) rest ...)
                               (begin
                                 (bytevector-uint-set! bv index number
                                                       (endianness big) len)
                                 (put! bv (+ index len) rest ...)))
                              ((_ bv index)
                               bv))))
             (let ((time-low  (hex->number 1))
                   (time-mid  (hex->number 2))
                   (time-hi   (hex->number 3))
                   (clock-seq (hex->number 4))
                   (node      (hex->number 5))
                   (uuid      (make-bytevector 16)))
               (put! uuid 0
                     (time-low 4) (time-mid 2) (time-hi 2)
                     (clock-seq 2) (node 6)))))))


(define* (canonicalize-device-spec spec #:optional (title 'any))
  "Return the device name corresponding to SPEC.  TITLE is a symbol, one of
the following:

  • 'device', in which case SPEC is known to designate a device node--e.g.,
     \"/dev/sda1\";
  • 'label', in which case SPEC is known to designate a partition label--e.g.,
     \"my-root-part\";
  • 'uuid', in which case SPEC must be a UUID (a 16-byte bytevector)
     designating a partition;
  • 'any', in which case SPEC can be anything.
"
  (define max-trials
    ;; Number of times we retry partition label resolution, 1 second per
    ;; trial.  Note: somebody reported a delay of 16 seconds (!) before their
    ;; USB key would be detected by the kernel, so we must wait for at least
    ;; this long.
    20)

  (define canonical-title
    ;; The realm of canonicalization.
    (if (eq? title 'any)
        (if (string? spec)
            ;; The "--root=SPEC" kernel command-line option always provides a
            ;; string, but the string can represent a device, a UUID, or a
            ;; label.  So check for all three.
            (cond ((string-prefix? "/" spec) 'device)
                  ((string->uuid spec) 'uuid)
                  (else 'label))
            'uuid)
        title))

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

  (case canonical-title
    ((device)
     ;; Nothing to do.
     spec)
    ((label)
     ;; Resolve the label.
     (resolve find-partition-by-label spec identity))
    ((uuid)
     (resolve find-partition-by-uuid
              (if (string? spec)
                  (string->uuid spec)
                  spec)
              uuid->string))
    (else
     (error "unknown device title" title))))

(define (check-file-system device type)
  "Run a file system check of TYPE on DEVICE."
  (define fsck
    (string-append "fsck." type))

  (let ((status (system* fsck "-v" "-p" "-C" "0" device)))
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
       (format (current-error-port) "'~a' exited with code ~a on ~a; \
spawning Bourne-like REPL~%"
               fsck code device)
       (start-repl %bournish-language)))))

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

(define (regular-file? file-name)
  "Return #t if FILE-NAME is a regular file."
  (eq? (stat:type (stat file-name)) 'regular))

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
           (mount-point (string-append root "/" mount-point))
           (flags       (mount-flags->bit-mask flags)))
       (when check?
         (check-file-system source type))

       ;; Create the mount point.  Most of the time this is a directory, but
       ;; in the case of a bind mount, a regular file may be needed.
       (if (and (= MS_BIND (logand flags MS_BIND))
                (regular-file? source))
           (unless (file-exists? mount-point)
             (mkdir-p (dirname mount-point))
             (call-with-output-file mount-point (const #t)))
           (mkdir-p mount-point))

       (mount source mount-point type flags options)

       ;; For read-only bind mounts, an extra remount is needed, as per
       ;; <http://lwn.net/Articles/281157/>, which still applies to Linux 4.0.
       (when (and (= MS_BIND (logand flags MS_BIND))
                  (= MS_RDONLY (logand flags MS_RDONLY)))
         (let ((flags (logior MS_BIND MS_REMOUNT MS_RDONLY)))
           (mount source mount-point type flags #f)))))))

;;; file-systems.scm ends here
