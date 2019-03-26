;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu installer parted)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt page)
  #:use-module (gnu system uuid)
  #:use-module ((gnu build file-systems)
                #:select (read-partition-uuid
                          read-luks-partition-uuid))
  #:use-module ((gnu build linux-modules)
                #:select (missing-modules))
  #:use-module ((gnu system linux-initrd)
                #:select (%base-initrd-modules))
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (guix i18n)
  #:use-module (parted)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (<user-partition>
            user-partition
            make-user-partition
            user-partition?
            user-partition-name
            user-partition-type
            user-partition-file-name
            user-partition-disk-file-name
            user-partition-crypt-label
            user-partition-crypt-password
            user-partition-fs-type
            user-partition-bootable?
            user-partition-esp?
            user-partition-bios-grub?
            user-partition-size
            user-partition-start
            user-partition-end
            user-partition-mount-point
            user-partition-need-formatting?
            user-partition-parted-object

            find-esp-partition
            data-partition?
            metadata-partition?
            freespace-partition?
            small-freespace-partition?
            normal-partition?
            extended-partition?
            logical-partition?
            esp-partition?
            boot-partition?
            default-esp-mount-point

            with-delay-device-in-use?
            force-device-sync
            non-install-devices
            partition-user-type
            user-fs-type-name
            partition-filesystem-user-type
            partition-get-flags
            partition->user-partition
            create-special-user-partitions
            find-user-partition-by-parted-object

            device-description
            partition-end-formatted
            partition-print-number
            partition-description
            partitions-descriptions
            user-partition-description

            &max-primary-exceeded
            max-primary-exceeded?
            &extended-creation-error
            extended-creation-error?
            &logical-creation-error
            logical-creation-error?

            can-create-partition?
            mklabel
            mkpart
            rmpart

            create-adjacent-partitions
            auto-partition

            &no-root-mount-point
            no-root-mount-point?

            check-user-partitions
            set-user-partitions-file-name
            format-user-partitions
            mount-user-partitions
            umount-user-partitions
            with-mounted-partitions
            user-partitions->file-systems
            user-partitions->configuration

            init-parted
            free-parted))


;;;
;;; Partition record.
;;;

(define-record-type* <user-partition>
  user-partition make-user-partition
  user-partition?
  (name                 user-partition-name ;string
                        (default #f))
  (type                 user-partition-type
                        (default 'normal)) ; 'normal | 'logical | 'extended
  (file-name            user-partition-file-name
                        (default #f))
  (disk-file-name       user-partition-disk-file-name
                        (default #f))
  (crypt-label          user-partition-crypt-label
                        (default #f))
  (crypt-password       user-partition-crypt-password
                        (default #f))
  (fs-type              user-partition-fs-type
                        (default 'ext4))
  (bootable?            user-partition-bootable?
                        (default #f))
  (esp?                 user-partition-esp?
                        (default #f))
  (bios-grub?           user-partition-bios-grub?
                        (default #f))
  (size                 user-partition-size
                        (default #f))
  (start                user-partition-start ;start as string (e.g. '11MB')
                        (default #f))
  (end                  user-partition-end ;same as start
                        (default #f))
  (mount-point          user-partition-mount-point ;string
                        (default #f))
  (need-formatting?     user-partition-need-formatting? ; boolean
                        (default #f))
  (parted-object        user-partition-parted-object ; <partition> from parted
                        (default #f)))


;;
;; Utilities.
;;

(define (find-esp-partition partitions)
  "Find and return the ESP partition among PARTITIONS."
  (find esp-partition? partitions))

(define (data-partition? partition)
  "Return #t if PARTITION is a partition dedicated to data (by opposition to
freespace, metadata and protected partition types), return #f otherwise."
  (let ((type (partition-type partition)))
    (not (any (lambda (flag)
                (member flag type))
              '(free-space metadata protected)))))

(define (metadata-partition? partition)
  "Return #t if PARTITION is a metadata partition, #f otherwise."
  (let ((type (partition-type partition)))
    (member 'metadata type)))

(define (freespace-partition? partition)
  "Return #t if PARTITION is a free-space partition, #f otherwise."
  (let ((type (partition-type partition)))
    (member 'free-space type)))

(define* (small-freespace-partition? device
                                     partition
                                     #:key (max-size MEBIBYTE-SIZE))
  "Return #t is PARTITION is a free-space partition with less a size strictly
inferior to MAX-SIZE, #f otherwise."
  (let ((size (partition-length partition))
        (max-sector-size (/ max-size
                            (device-sector-size device))))
    (< size max-sector-size)))

(define (normal-partition? partition)
  "return #t if partition is a normal partition, #f otherwise."
  (let ((type (partition-type partition)))
    (member 'normal type)))

(define (extended-partition? partition)
  "return #t if partition is an extended partition, #f otherwise."
  (let ((type (partition-type partition)))
    (member 'extended type)))

(define (logical-partition? partition)
  "Return #t if PARTITION is a logical partition, #f otherwise."
  (let ((type (partition-type partition)))
    (member 'logical type)))

(define (partition-user-type partition)
  "Return the type of PARTITION, to be stored in the TYPE field of
<user-partition> record. It can be 'normal, 'extended or 'logical."
  (cond ((normal-partition? partition)
         'normal)
        ((extended-partition? partition)
         'extended)
        ((logical-partition? partition)
         'logical)
        (else #f)))

(define (esp-partition? partition)
  "Return #t if partition has the ESP flag, return #f otherwise."
  (let* ((disk (partition-disk partition))
         (disk-type (disk-disk-type disk))
         (has-extended? (disk-type-check-feature
                         disk-type
                         DISK-TYPE-FEATURE-EXTENDED)))
    (and (data-partition? partition)
         (not has-extended?)
         (partition-is-flag-available? partition PARTITION-FLAG-ESP)
         (partition-get-flag partition PARTITION-FLAG-ESP))))

(define (boot-partition? partition)
  "Return #t if partition has the boot flag, return #f otherwise."
  (and (data-partition? partition)
       (partition-is-flag-available? partition PARTITION-FLAG-BOOT)
       (partition-get-flag partition PARTITION-FLAG-BOOT)))


;; The default mount point for ESP partitions.
(define default-esp-mount-point
  (make-parameter "/boot/efi"))

(define (efi-installation?)
  "Return #t if an EFI installation should be performed, #f otherwise."
  (file-exists? "/sys/firmware/efi"))

(define (user-fs-type-name fs-type)
  "Return the name of FS-TYPE as specified by libparted."
  (case fs-type
    ((ext4)  "ext4")
    ((btrfs) "btrfs")
    ((fat32) "fat32")
    ((swap)  "linux-swap")))

(define (user-fs-type->mount-type fs-type)
  "Return the mount type of FS-TYPE."
  (case fs-type
    ((ext4)  "ext4")
    ((btrfs) "btrfs")
    ((fat32) "vfat")))

(define (partition-filesystem-user-type partition)
  "Return the filesystem type of PARTITION, to be stored in the FS-TYPE field
of <user-partition> record."
  (let ((fs-type (partition-fs-type partition)))
    (and fs-type
         (let ((name (filesystem-type-name fs-type)))
           (cond
            ((string=? name "ext4") 'ext4)
            ((string=? name "btrfs") 'btrfs)
            ((string=? name "fat32") 'fat32)
            ((or (string=? name "swsusp")
                 (string=? name "linux-swap(v0)")
                 (string=? name "linux-swap(v1)"))
             'swap)
            (else
             (error (format #f "Unhandled ~a fs-type~%" name))))))))

(define (partition-get-flags partition)
  "Return the list of flags supported by the given PARTITION."
  (filter-map (lambda (flag)
                (and (partition-get-flag partition flag)
                     flag))
              (partition-flags partition)))

(define (partition->user-partition partition)
  "Convert PARTITION into a <user-partition> record and return it."
  (let* ((disk (partition-disk partition))
         (device (disk-device disk))
         (disk-type (disk-disk-type disk))
         (has-name? (disk-type-check-feature
                     disk-type
                     DISK-TYPE-FEATURE-PARTITION-NAME))
         (name (and has-name?
                    (data-partition? partition)
                    (partition-get-name partition))))
    (user-partition
     (name (and (and name
                     (not (string=? name "")))
                name))
     (type (or (partition-user-type partition)
               'normal))
     (file-name (partition-get-path partition))
     (disk-file-name (device-path device))
     (fs-type (or (partition-filesystem-user-type partition)
                  'ext4))
     (mount-point (and (esp-partition? partition)
                       (default-esp-mount-point)))
     (bootable? (boot-partition? partition))
     (esp? (esp-partition? partition))
     (parted-object partition))))

(define (create-special-user-partitions partitions)
  "Return a list with a <user-partition> record describing the ESP partition
found in PARTITIONS, if any."
  (filter-map (lambda (partition)
                (and (esp-partition? partition)
                     (partition->user-partition partition)))
              partitions))

(define (find-user-partition-by-parted-object user-partitions
                                              partition)
  "Find and return the <user-partition> record in USER-PARTITIONS list which
PARTED-OBJECT field equals PARTITION, return #f if not found."
  (find (lambda (user-partition)
          (equal? (user-partition-parted-object user-partition)
                  partition))
        user-partitions))


;;
;; Devices
;;

(define (with-delay-device-in-use? file-name)
  "Call DEVICE-IN-USE? with a few retries, as the first re-read will often
fail. See rereadpt function in wipefs.c of util-linux for an explanation."
  ;; Kernel always return EINVAL for BLKRRPART on loopdevices.
  (and (not (string-match "/dev/loop*" file-name))
       (let loop ((try 4))
         (usleep 250000)
         (let ((in-use? (device-in-use? file-name)))
           (if (and in-use? (> try 0))
               (loop (- try 1))
               in-use?)))))

(define* (force-device-sync device)
  "Force a flushing of the given DEVICE."
  (device-open device)
  (device-sync device)
  (device-close device))

(define (non-install-devices)
  "Return all the available devices, except the busy one, allegedly the
install device. DEVICE-IS-BUSY? is a parted call, checking if the device is
mounted. The install image uses an overlayfs so the install device does not
appear as mounted and won't be considered as busy. So use also DEVICE-IN-USE?
from (guix build syscalls) module, who will try to re-read the device's
partition table to determine whether or not it is already used (like sfdisk
from util-linux)."
  (remove (lambda (device)
            (let ((file-name (device-path device)))
              (or (device-is-busy? device)
                  (with-delay-device-in-use? file-name))))
          (devices)))


;;
;; Disk and partition printing.
;;

(define* (device-description device #:optional disk)
  "Return a string describing the given DEVICE."
  (let* ((type (device-type device))
         (file-name (device-path device))
         (model (device-model device))
         (type-str (device-type->string type))
         (disk-type (if disk
                        (disk-disk-type disk)
                        (disk-probe device)))
         (length (device-length device))
         (sector-size (device-sector-size device))
         (end (unit-format-custom-byte device
                                       (* length sector-size)
                                       UNIT-GIGABYTE)))
    (string-join
     `(,@(if (string=? model "")
             `(,type-str)
             `(,model ,(string-append "(" type-str ")")))
       ,file-name
       ,end
       ,@(if disk-type
             `(,(disk-type-name disk-type))
             '()))
     " ")))

(define (partition-end-formatted device partition)
  "Return as a string the end of PARTITION with the relevant unit."
  (unit-format-byte
   device
   (-
    (* (+ (partition-end partition) 1)
       (device-sector-size device))
    1)))

(define (partition-print-number partition)
  "Convert the given partition NUMBER to string."
  (let ((number (partition-number partition)))
    (number->string number)))

(define (partition-description partition user-partition)
  "Return a string describing the given PARTITION, located on the DISK of
DEVICE."

  (define (partition-print-type partition)
    "Return the type of PARTITION as a string."
    (if (freespace-partition? partition)
        (G_ "Free space")
        (let ((type (partition-type partition)))
          (match type
            ((type-symbol)
             (symbol->string type-symbol))))))

  (define (partition-print-flags partition)
    "Return the flags of PARTITION as a string of comma separated flags."
    (string-join
     (filter-map
      (lambda (flag)
        (and (partition-get-flag partition flag)
             (partition-flag-get-name flag)))
      (partition-flags partition))
     ","))

  (define (maybe-string-pad string length)
    "Returned a string formatted by padding STRING of LENGTH characters to the
right. If STRING is #f use an empty string."
    (if (and string (not (string=? string "")))
        (string-pad-right string length)
        ""))

  (let* ((disk (partition-disk partition))
         (device (disk-device disk))
         (disk-type (disk-disk-type disk))
         (has-name? (disk-type-check-feature
                     disk-type
                     DISK-TYPE-FEATURE-PARTITION-NAME))
         (has-extended? (disk-type-check-feature
                         disk-type
                         DISK-TYPE-FEATURE-EXTENDED))
         (part-type (partition-print-type partition))
         (number (and (not (freespace-partition? partition))
                      (partition-print-number partition)))
         (name (and has-name?
                    (if (freespace-partition? partition)
                        (G_ "Free space")
                        (partition-get-name partition))))
         (start (unit-format device
                             (partition-start partition)))
         (end (partition-end-formatted device partition))
         (size (unit-format device (partition-length partition)))
         (fs-type (partition-fs-type partition))
         (fs-type-name (and fs-type
                            (filesystem-type-name fs-type)))
         (crypt-label (and user-partition
                           (user-partition-crypt-label user-partition)))
         (flags (and (not (freespace-partition? partition))
                     (partition-print-flags partition)))
         (mount-point (and user-partition
                           (user-partition-mount-point user-partition))))
    `(,(or number "")
      ,@(if has-extended?
            (list part-type)
            '())
      ,size
      ,(or fs-type-name "")
      ,(or flags "")
      ,(or mount-point "")
      ,(or crypt-label "")
      ,(maybe-string-pad name 30))))

(define (partitions-descriptions partitions user-partitions)
  "Return a list of strings describing all the partitions found on
DEVICE. METADATA partitions are not described. The strings are padded to the
right so that they can be displayed as a table."

  (define (max-length-column lists column-index)
    "Return the maximum length of the string at position COLUMN-INDEX in the
list of string lists LISTS."
    (apply max
           (map (lambda (list)
                  (string-length
                   (list-ref list column-index)))
                lists)))

  (define (pad-descriptions descriptions)
    "Return a padded version of the list of string lists DESCRIPTIONS. The
strings are padded to the length of the longer string in a same column, as
determined by MAX-LENGTH-COLUMN procedure."
    (let* ((description-length (length (car descriptions)))
           (paddings (map (lambda (index)
                            (max-length-column descriptions index))
                          (iota description-length))))
      (map (lambda (description)
             (map string-pad-right description paddings))
           descriptions)))

  (let* ((descriptions
          (map
           (lambda (partition)
             (let ((user-partition
                    (find-user-partition-by-parted-object user-partitions
                                                          partition)))
               (partition-description partition user-partition)))
           partitions))
         (padded-descriptions (if (null? partitions)
                                  '()
                                  (pad-descriptions descriptions))))
    (map (cut string-join <> " ") padded-descriptions)))

(define (user-partition-description user-partition)
  "Return a string describing the given USER-PARTITION record."
  (let* ((partition (user-partition-parted-object user-partition))
         (disk (partition-disk partition))
         (disk-type (disk-disk-type disk))
         (device (disk-device disk))
         (has-name? (disk-type-check-feature
                     disk-type
                     DISK-TYPE-FEATURE-PARTITION-NAME))
         (has-extended? (disk-type-check-feature
                         disk-type
                         DISK-TYPE-FEATURE-EXTENDED))
         (name (user-partition-name user-partition))
         (type (user-partition-type user-partition))
         (type-name (symbol->string type))
         (fs-type (user-partition-fs-type user-partition))
         (fs-type-name (user-fs-type-name fs-type))
         (bootable? (user-partition-bootable? user-partition))
         (esp? (user-partition-esp? user-partition))
         (need-formatting? (user-partition-need-formatting? user-partition))
         (crypt-label (user-partition-crypt-label user-partition))
         (size (user-partition-size user-partition))
         (mount-point (user-partition-mount-point user-partition)))
    `(,@(if has-name?
            `((name . ,(string-append "Name: " (or name "None"))))
            '())
      ,@(if (and has-extended?
                 (freespace-partition? partition)
                 (not (eq? type 'logical)))
            `((type . ,(string-append "Type: " type-name)))
            '())
      ,@(if (eq? type 'extended)
            '()
            `((fs-type . ,(string-append "Filesystem type: " fs-type-name))))
      ,@(if (or (eq? type 'extended)
                (eq? fs-type 'swap)
                (not has-extended?))
            '()
            `((bootable . ,(string-append "Bootable flag: "
                                          (if bootable? "On" "Off")))))
      ,@(if (and (not has-extended?)
                 (not (eq? fs-type 'swap)))
            `((esp? . ,(string-append "ESP flag: "
                                      (if esp? "On" "Off"))))
            '())
      ,@(if (freespace-partition? partition)
            (let ((size-formatted
                   (or size (unit-format device
                                         (partition-length partition)))))
              `((size . ,(string-append "Size : " size-formatted))))
            '())
      ,@(if (or (eq? type 'extended)
                (eq? fs-type 'swap))
            '()
            `((crypt-label
               . ,(string-append
                   "Encryption: "
                   (if crypt-label
                       (format #f "Yes (label ~a)" crypt-label)
                       "No")))))
      ,@(if (or (freespace-partition? partition)
                (eq? fs-type 'swap))
            '()
            `((need-formatting?
               . ,(string-append "Format the partition? : "
                                 (if need-formatting? "Yes" "No")))))
      ,@(if (or (eq? type 'extended)
                (eq? fs-type 'swap))
            '()
            `((mount-point
               . ,(string-append "Mount point : "
                                 (or mount-point
                                     (and esp? (default-esp-mount-point))
                                     "None"))))))))


;;
;; Partition table creation.
;;

(define (mklabel device type-name)
  "Create a partition table on DEVICE. TYPE-NAME is the type of the partition
table, \"msdos\" or \"gpt\"."
  (let ((type (disk-type-get type-name)))
    (disk-new-fresh device type)))


;;
;; Partition creation.
;;

;; The maximum count of primary partitions is exceeded.
(define-condition-type &max-primary-exceeded &condition
  max-primary-exceeded?)

;; It is not possible to create an extended partition.
(define-condition-type &extended-creation-error &condition
  extended-creation-error?)

;; It is not possible to create a logical partition.
(define-condition-type &logical-creation-error &condition
  logical-creation-error?)

(define (can-create-primary? disk)
  "Return #t if it is possible to create a primary partition on DISK, return
#f otherwise."
  (let ((max-primary (disk-get-max-primary-partition-count disk)))
    (find (lambda (number)
            (not (disk-get-partition disk number)))
          (iota max-primary 1))))

(define (can-create-extended? disk)
  "Return #t if it is possible to create an extended partition on DISK, return
#f otherwise."
  (let* ((disk-type (disk-disk-type disk))
         (has-extended? (disk-type-check-feature
                         disk-type
                         DISK-TYPE-FEATURE-EXTENDED)))
    (and (can-create-primary? disk)
         has-extended?
         (not (disk-extended-partition disk)))))

(define (can-create-logical? disk)
  "Return #t is it is possible to create a logical partition on DISK, return
#f otherwise."
  (let* ((disk-type (disk-disk-type disk))
         (has-extended? (disk-type-check-feature
                         disk-type
                         DISK-TYPE-FEATURE-EXTENDED)))
    (and has-extended?
         (disk-extended-partition disk))))

(define (can-create-partition? user-part)
  "Return #t if it is possible to create the given USER-PART record, return #f
otherwise."
  (let* ((type (user-partition-type user-part))
         (partition (user-partition-parted-object user-part))
         (disk (partition-disk partition)))
    (case type
      ((normal)
       (or (can-create-primary? disk)
           (raise
            (condition (&max-primary-exceeded)))))
      ((extended)
       (or (can-create-extended? disk)
           (raise
            (condition (&extended-creation-error)))))
      ((logical)
       (or (can-create-logical? disk)
           (raise
            (condition (&logical-creation-error))))))))

(define* (mkpart disk user-partition
                 #:key (previous-partition #f))
  "Create the given USER-PARTITION on DISK. The PREVIOUS-PARTITION argument as
to be set to the partition preceding USER-PARTITION if any."

  (define (parse-start-end start end)
    "Parse start and end strings as positions on DEVICE expressed with a unit,
like '100GB' or '12.2KiB'. Return a list of 4 elements, the start sector, its
range (1 unit large area centered on start sector), the end sector and its
range."
    (let ((device (disk-device disk)))
      (call-with-values
          (lambda ()
            (unit-parse start device))
        (lambda (start-sector start-range)
          (call-with-values
              (lambda ()
                (unit-parse end device))
            (lambda (end-sector end-range)
              (list start-sector start-range
                    end-sector end-range)))))))

  (define* (extend-ranges! start-range end-range
                           #:key (offset 0))
    "Try to extend START-RANGE by 1 MEBIBYTE to the right and END-RANGE by 1
MEBIBYTE to the left. This way, if the disk is aligned on 2048 sectors of
512KB (like frequently), we will have a chance for the
'optimal-align-constraint' to succeed. Do not extend ranges if that would
cause them to cross."
    (let* ((device (disk-device disk))
           (start-range-end (geometry-end start-range))
           (end-range-start (geometry-start end-range))
           (mebibyte-sector-size (/ MEBIBYTE-SIZE
                                    (device-sector-size device)))
           (new-start-range-end
            (+ start-range-end mebibyte-sector-size offset))
           (new-end-range-start
            (- end-range-start mebibyte-sector-size offset)))
      (when (< new-start-range-end new-end-range-start)
        (geometry-set-end start-range new-start-range-end)
        (geometry-set-start end-range new-end-range-start))))

  (match (parse-start-end (user-partition-start user-partition)
                          (user-partition-end user-partition))
    ((start-sector start-range end-sector end-range)
     (let* ((prev-end (if previous-partition
                          (partition-end previous-partition)
                          0))
            (start-distance (- start-sector prev-end))
            (type (user-partition-type user-partition))
            ;; There should be at least 2 unallocated sectors in front of each
            ;; logical partition, otherwise parted will fail badly:
            ;; https://gparted.org/h2-fix-msdos-pt.php#apply-action-fail.
            (start-offset (if previous-partition
                              (- 3 start-distance)
                              0))
            (start-sector* (if (and (eq? type 'logical)
                                    (< start-distance 3))
                               (+ start-sector start-offset)
                               start-sector)))
       ;; This is a hack.  Parted almost always fails to create optimally
       ;; aligned partitions (unless specifying percentages) because the
       ;; default range of 1MB centered on the start sector is not enough when
       ;; the optimal alignment is 2048 sectors of 512KB.
       (extend-ranges! start-range end-range #:offset start-offset)

       (let* ((device (disk-device disk))
              (disk-type (disk-disk-type disk))
              (length (device-length device))
              (name (user-partition-name user-partition))
              (filesystem-type
               (filesystem-type-get
                (user-fs-type-name
                 (user-partition-fs-type user-partition))))
              (flags `(,@(if (user-partition-bootable? user-partition)
                             `(,PARTITION-FLAG-BOOT)
                             '())
                       ,@(if (user-partition-esp? user-partition)
                             `(,PARTITION-FLAG-ESP)
                             '())
                       ,@(if (user-partition-bios-grub? user-partition)
                             `(,PARTITION-FLAG-BIOS-GRUB)
                             '())))
              (has-name? (disk-type-check-feature
                          disk-type
                          DISK-TYPE-FEATURE-PARTITION-NAME))
              (partition-type (partition-type->int type))
              (partition (partition-new disk
                                        #:type partition-type
                                        #:filesystem-type filesystem-type
                                        #:start start-sector*
                                        #:end end-sector))
              (user-constraint (constraint-new
                                #:start-align 'any
                                #:end-align 'any
                                #:start-range start-range
                                #:end-range end-range
                                #:min-size 1
                                #:max-size length))
              (dev-constraint
               (device-get-optimal-aligned-constraint device))
              (final-constraint (constraint-intersect user-constraint
                                                      dev-constraint))
              (no-constraint (constraint-any device))
              ;; Try to create a partition with an optimal alignment
              ;; constraint. If it fails, fallback to creating a partition with
              ;; no specific constraint.
              (partition-ok?
               (or (disk-add-partition disk partition final-constraint)
                   (disk-add-partition disk partition no-constraint))))
         ;; Set the partition name if supported.
         (when (and partition-ok? has-name? name)
           (partition-set-name partition name))

         ;; Set flags is required.
         (for-each (lambda (flag)
                     (and (partition-is-flag-available? partition flag)
                          (partition-set-flag partition flag 1)))
                   flags)

         (and partition-ok?
              (partition-set-system partition filesystem-type)
              partition))))))


;;
;; Partition destruction.
;;

(define (rmpart disk number)
  "Remove the partition with the given NUMBER on DISK."
  (let ((partition (disk-get-partition disk number)))
    (disk-remove-partition disk partition)))


;;
;; Auto partitionning.
;;

(define* (create-adjacent-partitions disk partitions
                                     #:key (last-partition-end 0))
  "Create the given PARTITIONS on DISK. LAST-PARTITION-END is the sector from
which we want to start creating partitions. The START and END of each created
partition are computed from its SIZE value and the position of the last
partition."
  (let ((device (disk-device disk)))
    (let loop ((partitions partitions)
               (remaining-space (- (device-length device)
                                   last-partition-end))
               (start last-partition-end))
      (match partitions
        (() '())
        ((partition . rest)
         (let* ((size (user-partition-size partition))
                (percentage-size (and (string? size)
                                      (read-percentage size)))
                (sector-size (device-sector-size device))
                (partition-size (if percentage-size
                                    (exact->inexact
                                     (* (/ percentage-size 100)
                                        remaining-space))
                                    size))
                (end-partition (min (- (device-length device) 1)
                                    (nearest-exact-integer
                                     (+ start partition-size 1))))
                (name (user-partition-name partition))
                (type (user-partition-type partition))
                (fs-type (user-partition-fs-type partition))
                (start-formatted (unit-format-custom device
                                                     start
                                                     UNIT-SECTOR))
                (end-formatted (unit-format-custom device
                                                   end-partition
                                                   UNIT-SECTOR))
                (new-user-partition (user-partition
                                     (inherit partition)
                                     (start start-formatted)
                                     (end end-formatted)))
                (new-partition
                 (mkpart disk new-user-partition)))
           (if new-partition
               (cons (user-partition
                      (inherit new-user-partition)
                      (file-name (partition-get-path new-partition))
                      (disk-file-name (device-path device))
                      (parted-object new-partition))
                     (loop rest
                           (if (eq? type 'extended)
                               remaining-space
                               (- remaining-space
                                  (partition-length new-partition)))
                           (if (eq? type 'extended)
                               (+ start 1)
                               (+ (partition-end new-partition) 1))))
               (error
                (format #f "Unable to create partition ~a~%" name)))))))))

(define (force-user-partitions-formatting user-partitions)
  "Set the NEED-FORMATING? fields to #t on all <user-partition> records of
USER-PARTITIONS list and return the updated list."
  (map (lambda (p)
         (user-partition
          (inherit p)
          (need-formatting? #t)))
       user-partitions))

(define* (auto-partition disk
                         #:key
                         (scheme 'entire-root))
  "Automatically create partitions on DISK. All the previous
partitions (except the ESP on a GPT disk, if present) are wiped. SCHEME is the
desired partitioning scheme. It can be 'entire-root or
'entire-root-home. 'entire-root will create a swap partition and a root
partition occupying all the remaining space. 'entire-root-home will create a
swap partition, a root partition and a home partition."
  (let* ((device (disk-device disk))
         (disk-type (disk-disk-type disk))
         (has-extended? (disk-type-check-feature
                         disk-type
                         DISK-TYPE-FEATURE-EXTENDED))
         (partitions (filter data-partition? (disk-partitions disk)))
         (esp-partition (find-esp-partition partitions))
         ;; According to
         ;; https://wiki.archlinux.org/index.php/EFI_system_partition, the ESP
         ;; size should be at least 550MiB.
         (new-esp-size (nearest-exact-integer
                        (/ (* 550 MEBIBYTE-SIZE)
                           (device-sector-size device))))
         (end-esp-partition (and esp-partition
                                 (partition-end esp-partition)))
         (non-boot-partitions (remove esp-partition? partitions))
         (bios-grub-size (/ (* 3 MEBIBYTE-SIZE)
                            (device-sector-size device)))
         (five-percent-disk (nearest-exact-integer
                             (* 0.05 (device-length device))))
         (default-swap-size (nearest-exact-integer
                             (/ (* 4 GIGABYTE-SIZE)
                                (device-sector-size device))))
         ;; Use a 4GB size for the swap if it represents less than 5% of the
         ;; disk space. Otherwise, set the swap size to 5% of the disk space.
         (swap-size (min default-swap-size five-percent-disk)))

    (if has-extended?
        ;; msdos - remove everything.
        (disk-delete-all disk)
        ;; gpt - remove everything but esp if it exists.
        (for-each
         (lambda (partition)
           (and (data-partition? partition)
                (disk-remove-partition disk partition)))
         non-boot-partitions))

    (let* ((start-partition
            (and (not has-extended?)
                 (not esp-partition)
                 (if (efi-installation?)
                     (user-partition
                      (fs-type 'fat32)
                      (esp? #t)
                      (size new-esp-size)
                      (mount-point (default-esp-mount-point)))
                     (user-partition
                      (fs-type 'ext4)
                      (bootable? #t)
                      (bios-grub? #t)
                      (size bios-grub-size)))))
           (new-partitions
            (cond
             ((or (eq? scheme 'entire-root)
                  (eq? scheme 'entire-encrypted-root))
              (let ((encrypted? (eq? scheme 'entire-encrypted-root)))
                `(,@(if start-partition
                        `(,start-partition)
                        '())
                  ,@(if encrypted?
                        '()
                        `(,(user-partition
                            (fs-type 'swap)
                            (size swap-size))))
                  ,(user-partition
                    (fs-type 'ext4)
                    (bootable? has-extended?)
                    (crypt-label (and encrypted? "cryptroot"))
                    (size "100%")
                    (mount-point "/")))))
             ((or (eq? scheme 'entire-root-home)
                  (eq? scheme 'entire-encrypted-root-home))
              (let ((encrypted? (eq? scheme 'entire-encrypted-root-home)))
                `(,@(if start-partition
                        `(,start-partition)
                        '())
                  ,(user-partition
                    (fs-type 'ext4)
                    (bootable? has-extended?)
                    (crypt-label (and encrypted? "cryptroot"))
                    (size "33%")
                    (mount-point "/"))
                  ,@(if has-extended?
                        `(,(user-partition
                            (type 'extended)
                            (size "100%")))
                        '())
                  ,@(if encrypted?
                        '()
                        `(,(user-partition
                            (type (if has-extended?
                                      'logical
                                      'normal))
                            (fs-type 'swap)
                            (size swap-size))))
                  ,(user-partition
                    (type (if has-extended?
                              'logical
                              'normal))
                    (fs-type 'ext4)
                    (crypt-label (and encrypted? "crypthome"))
                    (size "100%")
                    (mount-point "/home")))))))
           (new-partitions* (force-user-partitions-formatting
                             new-partitions)))
      (create-adjacent-partitions disk
                                  new-partitions*
                                  #:last-partition-end
                                  (or end-esp-partition 0)))))


;;
;; Convert user-partitions.
;;

;; No root mount point found.
(define-condition-type &no-root-mount-point &condition
  no-root-mount-point?)

(define (check-user-partitions user-partitions)
  "Return #t if the USER-PARTITIONS lists contains one <user-partition> record
with a mount-point set to '/', raise &no-root-mount-point condition
otherwise."
  (let ((mount-points
         (map user-partition-mount-point user-partitions)))
    (or (member "/" mount-points)
        (raise
         (condition (&no-root-mount-point))))))

(define (set-user-partitions-file-name user-partitions)
  "Set the partition file-name of <user-partition> records in USER-PARTITIONS
list and return the updated list."
  (map (lambda (p)
         (let* ((partition (user-partition-parted-object p))
                (file-name (partition-get-path partition)))
           (user-partition
            (inherit p)
            (file-name file-name))))
       user-partitions))

(define-syntax-rule (with-null-output-ports exp ...)
  "Evaluate EXP with both the output port and the error port pointing to the
bit bucket."
  (with-output-to-port (%make-void-port "w")
    (lambda ()
      (with-error-to-port (%make-void-port "w")
        (lambda () exp ...)))))

(define (create-ext4-file-system partition)
  "Create an ext4 file-system for PARTITION file-name."
  (with-null-output-ports
   (invoke "mkfs.ext4" "-F" partition)))

(define (create-fat32-file-system partition)
  "Create an ext4 file-system for PARTITION file-name."
  (with-null-output-ports
   (invoke "mkfs.fat" "-F32" partition)))

(define (create-swap-partition partition)
  "Set up swap area on PARTITION file-name."
  (with-null-output-ports
   (invoke "mkswap" "-f" partition)))

(define (call-with-luks-key-file password proc)
  "Write PASSWORD in a temporary file and pass it to PROC as argument."
  (call-with-temporary-output-file
   (lambda (file port)
     (put-string port password)
     (close port)
     (proc file))))

(define (user-partition-upper-file-name user-partition)
  "Return the file-name of the virtual block device corresponding to
USER-PARTITION if it is encrypted, or the plain file-name otherwise."
  (let ((crypt-label (user-partition-crypt-label user-partition))
        (file-name (user-partition-file-name user-partition)))
    (if crypt-label
        (string-append "/dev/mapper/" crypt-label)
        file-name)))

(define (luks-format-and-open user-partition)
  "Format and open the encrypted partition pointed by USER-PARTITION."
  (let* ((file-name (user-partition-file-name user-partition))
         (label (user-partition-crypt-label user-partition))
         (password (user-partition-crypt-password user-partition)))
    (call-with-luks-key-file
     password
     (lambda (key-file)
       (system* "cryptsetup" "-q" "luksFormat" file-name key-file)
       (system* "cryptsetup" "open" "--type" "luks"
                "--key-file" key-file file-name label)))))

(define (luks-close user-partition)
  "Close the encrypted partition pointed by USER-PARTITION."
  (let ((label (user-partition-crypt-label user-partition)))
    (system* "cryptsetup" "close" label)))

(define (format-user-partitions user-partitions)
  "Format the <user-partition> records in USER-PARTITIONS list with
NEED-FORMATING? field set to #t."
  (for-each
   (lambda (user-partition)
     (let* ((need-formatting?
             (user-partition-need-formatting? user-partition))
            (type (user-partition-type user-partition))
            (crypt-label (user-partition-crypt-label user-partition))
            (file-name (user-partition-upper-file-name user-partition))
            (fs-type (user-partition-fs-type user-partition)))
       (when crypt-label
         (luks-format-and-open user-partition))

       (case fs-type
         ((ext4)
          (and need-formatting?
               (not (eq? type 'extended))
               (create-ext4-file-system file-name)))
         ((fat32)
          (and need-formatting?
               (not (eq? type 'extended))
               (create-fat32-file-system file-name)))
         ((swap)
          (create-swap-partition file-name))
         (else
          ;; TODO: Add support for other file-system types.
          #t))))
   user-partitions))

(define (sort-partitions user-partitions)
  "Sort USER-PARTITIONS by mount-points, so that the more nested mount-point
comes last. This is useful to mount/umount partitions in a coherent order."
  (sort user-partitions
        (lambda (a b)
          (let ((mount-point-a (user-partition-mount-point a))
                (mount-point-b (user-partition-mount-point b)))
            (string-prefix? mount-point-a mount-point-b)))))

(define (mount-user-partitions user-partitions)
  "Mount the <user-partition> records in USER-PARTITIONS list on their
respective mount-points."
  (let* ((mount-partitions (filter user-partition-mount-point user-partitions))
         (sorted-partitions (sort-partitions mount-partitions)))
    (for-each (lambda (user-partition)
                (let* ((mount-point
                        (user-partition-mount-point user-partition))
                       (target
                        (string-append (%installer-target-dir)
                                       mount-point))
                       (fs-type
                        (user-partition-fs-type user-partition))
                       (crypt-label
                        (user-partition-crypt-label user-partition))
                       (mount-type
                        (user-fs-type->mount-type fs-type))
                       (file-name
                        (user-partition-upper-file-name user-partition)))
                  (mkdir-p target)
                  (mount file-name target mount-type)))
              sorted-partitions)))

(define (umount-user-partitions user-partitions)
  "Unmount all the <user-partition> records in USER-PARTITIONS list."
  (let* ((mount-partitions (filter user-partition-mount-point user-partitions))
         (sorted-partitions (sort-partitions mount-partitions)))
    (for-each (lambda (user-partition)
                (let* ((mount-point
                        (user-partition-mount-point user-partition))
                       (crypt-label
                        (user-partition-crypt-label user-partition))
                       (target
                        (string-append (%installer-target-dir)
                                       mount-point)))
                  (umount target)
                  (when crypt-label
                    (luks-close user-partition))))
              (reverse sorted-partitions))))

(define (find-swap-user-partitions user-partitions)
  "Return the subset of <user-partition> records in USER-PARTITIONS list with
the FS-TYPE field set to 'swap, return the empty list if none found."
  (filter (lambda (user-partition)
            (let ((fs-type (user-partition-fs-type user-partition)))
              (eq? fs-type 'swap)))
          user-partitions))

(define (start-swapping user-partitions)
  "Start swaping on <user-partition> records with FS-TYPE equal to 'swap."
  (let* ((swap-user-partitions (find-swap-user-partitions user-partitions))
         (swap-devices (map user-partition-file-name swap-user-partitions)))
    (for-each swapon swap-devices)))

(define (stop-swapping user-partitions)
  "Stop swaping on <user-partition> records with FS-TYPE equal to 'swap."
  (let* ((swap-user-partitions (find-swap-user-partitions user-partitions))
         (swap-devices (map user-partition-file-name swap-user-partitions)))
    (for-each swapoff swap-devices)))

(define-syntax-rule (with-mounted-partitions user-partitions exp ...)
  "Mount USER-PARTITIONS and start swapping within the dynamic extent of EXP."
  (dynamic-wind
    (lambda ()
      (mount-user-partitions user-partitions)
      (start-swapping user-partitions))
    (lambda ()
      exp ...)
    (lambda ()
      (umount-user-partitions user-partitions)
      (stop-swapping user-partitions)
      #f)))

(define (user-partition->file-system user-partition)
  "Convert the given USER-PARTITION record in a FILE-SYSTEM record from
(gnu system file-systems) module and return it."
  (let* ((mount-point (user-partition-mount-point user-partition))
         (fs-type (user-partition-fs-type user-partition))
         (crypt-label (user-partition-crypt-label user-partition))
         (mount-type (user-fs-type->mount-type fs-type))
         (file-name (user-partition-file-name user-partition))
         (upper-file-name (user-partition-upper-file-name user-partition))
         ;; Only compute uuid if partition is not encrypted.
         (uuid (or crypt-label
                   (uuid->string (read-partition-uuid file-name) fs-type))))
    `(file-system
       (mount-point ,mount-point)
       (device ,@(if crypt-label
                     `(,upper-file-name)
                     `((uuid ,uuid (quote ,fs-type)))))
       (type ,mount-type)
       ,@(if crypt-label
             '((dependencies mapped-devices))
             '()))))

(define (user-partitions->file-systems user-partitions)
  "Convert the given USER-PARTITIONS list of <user-partition> records into a
list of <file-system> records."
  (filter-map
   (lambda (user-partition)
     (let ((mount-point
            (user-partition-mount-point user-partition)))
       (and mount-point
            (user-partition->file-system user-partition))))
   user-partitions))

(define (user-partition->mapped-device user-partition)
  "Convert the given USER-PARTITION record into a MAPPED-DEVICE record
from (gnu system mapped-devices) and return it."
  (let ((label (user-partition-crypt-label user-partition))
        (file-name (user-partition-file-name user-partition)))
    `(mapped-device
      (source (uuid ,(uuid->string
                      (read-luks-partition-uuid file-name)
                      'luks)))
      (target ,label)
      (type luks-device-mapping))))

(define (root-user-partition? partition)
  "Return true if PARTITION is the root partition."
  (let ((mount-point (user-partition-mount-point partition)))
    (and mount-point
         (string=? mount-point "/"))))

(define (bootloader-configuration user-partitions)
  "Return the bootloader configuration field for USER-PARTITIONS."
  (let* ((root-partition (find root-user-partition?
                               user-partitions))
         (root-partition-disk (user-partition-disk-file-name root-partition)))
    `((bootloader-configuration
       ,@(if (efi-installation?)
             `((bootloader grub-efi-bootloader)
               (target ,(default-esp-mount-point)))
             `((bootloader grub-bootloader)
               (target ,root-partition-disk)))

       ;; XXX: Assume we defined the 'keyboard-layout' field of
       ;; <operating-system> right above.
       (keyboard-layout keyboard-layout)))))

(define (user-partition-missing-modules user-partitions)
  "Return the list of kernel modules missing from the default set of kernel
modules to access USER-PARTITIONS."
  (let ((devices (filter user-partition-crypt-label user-partitions))
        (root    (find root-user-partition? user-partitions)))
    (delete-duplicates
     (append-map (lambda (device)
                   (catch 'system-error
                     (lambda ()
                       (missing-modules device %base-initrd-modules))
                     (const '())))
                 (delete-duplicates
                  (map user-partition-file-name
                       (cons root devices)))))))

(define (initrd-configuration user-partitions)
  "Return an 'initrd-modules' field with everything needed for
USER-PARTITIONS, or return nothing."
  (match (user-partition-missing-modules user-partitions)
    (()
     '())
    ((modules ...)
     `((initrd-modules ',modules)))))

(define (user-partitions->configuration user-partitions)
  "Return the configuration field for USER-PARTITIONS."
  (let* ((swap-user-partitions (find-swap-user-partitions user-partitions))
         (swap-devices (map user-partition-file-name swap-user-partitions))
         (encrypted-partitions
          (filter user-partition-crypt-label user-partitions)))
    `((bootloader ,@(bootloader-configuration user-partitions))
      ,@(initrd-configuration user-partitions)
      ,@(if (null? swap-devices)
            '()
            `((swap-devices (list ,@swap-devices))))
      ,@(if (null? encrypted-partitions)
            '()
            `((mapped-devices
               (list ,@(map user-partition->mapped-device
                            encrypted-partitions)))))
      (file-systems (cons*
                     ,@(user-partitions->file-systems user-partitions)
                     %base-file-systems)))))


;;
;; Initialization.
;;

(define (init-parted)
  "Initialize libparted support."
  (probe-all-devices)
  (exception-set-handler (lambda (exception)
                           EXCEPTION-OPTION-UNHANDLED)))

(define (free-parted devices)
  "Deallocate memory used for DEVICES in parted, force sync them and wait for
the devices not to be used before returning."
  ;; XXX: Formatting and further operations on disk partition table may fail
  ;; because the partition table changes are not synced, or because the device
  ;; is still in use, even if parted should have finished editing
  ;; partitions. This is not well understood, but syncing devices and waiting
  ;; them to stop returning EBUSY to BLKRRPART ioctl seems to be enough. The
  ;; same kind of issue is described here:
  ;; https://mail.gnome.org/archives/commits-list/2013-March/msg18423.html.
  (let ((device-file-names (map device-path devices)))
    (for-each force-device-sync devices)
    (free-all-devices)
    (for-each (lambda (file-name)
                (let ((in-use? (with-delay-device-in-use? file-name)))
                  (and in-use?
                       (error
                        (format #f (G_ "Device ~a is still in use.")
                                file-name)))))
              device-file-names)))
