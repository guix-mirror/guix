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

(define-module (gnu build file-systems)
  #:use-module (guix build utils)
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
            find-partition-by-label
            canonicalize-device-spec

            MS_RDONLY
            MS_NOSUID
            MS_NODEV
            MS_NOEXEC
            MS_BIND
            MS_MOVE
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
    ;; Number of times we retry partition label resolution, 1 second per
    ;; trial.  Note: somebody reported a delay of 16 seconds (!) before their
    ;; USB key would be detected by the kernel, so we must wait for at least
    ;; this long.
    20)

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
                   (format #t "waiting for partition '~a' to appear...~%"
                           spec)
                   (sleep 1)
                   (loop (+ 1 count))))))))
    ;; TODO: Add support for UUIDs.
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

;;; file-systems.scm ends here
