;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer newt partition)
  #:use-module (gnu installer parted)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt utils)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (newt)
  #:use-module (parted)
  #:export (run-partioning-page))

(define (button-exit-action)
  "Raise the &installer-step-abort condition."
  (raise
   (condition
    (&installer-step-abort))))

(define (run-scheme-page)
  "Run a page asking the user for a partitioning scheme."
  (let* ((items
          `((root . ,(G_ "Everything is one partition"))
            (root-home . ,(G_ "Separate /home partition"))))
         (result (run-listbox-selection-page
                  #:info-text (G_ "Please select a partitioning scheme.")
                  #:title (G_ "Partition scheme")
                  #:listbox-items items
                  #:listbox-item->text cdr
                  #:sort-listbox-items? #f       ;keep the 'root' option first
                  #:button-text (G_ "Exit")
                  #:button-callback-procedure button-exit-action)))
    (car result)))

(define (draw-formatting-page)
  "Draw a page asking for confirmation, and then indicating that partitions
are being formatted."
  (run-confirmation-page (G_ "We are about to format your hard disk.  All \
its data will be lost.  Do you wish to continue?")
                         (G_ "Format disk?")
                         #:exit-button-procedure button-exit-action)
  (draw-info-page
   (format #f (G_ "Partition formatting is in progress, please wait."))
   (G_ "Preparing partitions")))

(define (run-device-page devices)
  "Run a page asking the user to select a device among those in the given
DEVICES list."
  (define (device-items)
    (map (lambda (device)
           `(,device . ,(device-description device)))
         devices))

  (let* ((result (run-listbox-selection-page
                  #:info-text (G_ "Please select a disk.")
                  #:title (G_ "Disk")
                  #:listbox-items (device-items)
                  #:listbox-item->text cdr
                  #:button-text (G_ "Exit")
                  #:button-callback-procedure button-exit-action))
         (device (car result)))
    device))

(define (run-label-page button-text button-callback)
  "Run a page asking the user to select a partition table label."
  (run-listbox-selection-page
   #:info-text (G_ "Select a new partition table type. \
Be careful, all data on the disk will be lost.")
   #:title (G_ "Partition table")
   #:listbox-items '("msdos" "gpt")
   #:listbox-item->text identity
   #:button-text button-text
   #:button-callback-procedure button-callback))

(define (run-type-page partition)
  "Run a page asking the user to select a partition type."
  (let* ((disk (partition-disk partition))
         (partitions (disk-partitions disk))
         (other-extended-partitions?
          (any extended-partition? partitions))
         (items
          `(normal ,@(if other-extended-partitions?
                         '()
                         '(extended)))))
    (run-listbox-selection-page
     #:info-text (G_ "Please select a partition type.")
     #:title (G_ "Partition type")
     #:listbox-items items
     #:listbox-item->text symbol->string
     #:sort-listbox-items? #f
     #:button-text (G_ "Exit")
     #:button-callback-procedure button-exit-action)))

(define (run-fs-type-page)
  "Run a page asking the user to select a file-system type."
  (run-listbox-selection-page
   #:info-text (G_ "Please select the file-system type for this partition.")
   #:title (G_ "File-system type")
   #:listbox-items '(ext4 btrfs fat32 swap)
   #:listbox-item->text user-fs-type-name
   #:sort-listbox-items? #f
   #:button-text (G_ "Exit")
   #:button-callback-procedure button-exit-action))

(define (inform-can-create-partition? user-partition)
  "Return #t if it is possible to create USER-PARTITION. This is determined by
calling CAN-CREATE-PARTITION? procedure. If an exception is raised, catch it
an inform the user with an appropriate error-page and return #f."
  (guard (c ((max-primary-exceeded? c)
            (run-error-page
             (G_ "Primary partitions count exceeded.")
             (G_ "Creation error"))
            #f)
           ((extended-creation-error? c)
            (run-error-page
             (G_ "Extended partition creation error.")
             (G_ "Creation error"))
            #f)
           ((logical-creation-error? c)
            (run-error-page
             (G_ "Logical partition creation error.")
             (G_ "Creation error"))
            #f))
    (can-create-partition? user-partition)))

(define (prompt-luks-passwords user-partitions)
  "Prompt for the luks passwords of the encrypted partitions in
USER-PARTITIONS list. Return this list with password fields filled-in."
  (map (lambda (user-part)
         (let* ((crypt-label (user-partition-crypt-label user-part))
                (file-name (user-partition-file-name user-part))
                (password-page
                 (lambda ()
                   ;; Note: Don't use FLAG-PASSWORD here because this is the
                   ;; first bit of text that the user types in, so it's
                   ;; probably safer if they can see that the keyboard layout
                   ;; they chose is in effect.
                   (run-input-page
                    (format #f (G_ "Please enter the password for the \
encryption of partition ~a (label: ~a).") file-name crypt-label)
                    (G_ "Password required"))))
                (password-confirm-page
                 (lambda ()
                   (run-input-page
                    (format #f (G_ "Please confirm the password for the \
encryption of partition ~a (label: ~a).") file-name crypt-label)
                    (G_ "Password confirmation required")
                    #:input-flags FLAG-PASSWORD))))
           (if crypt-label
               (let loop ()
                 (let ((password (password-page))
                       (confirmation (password-confirm-page)))
                   (if (string=? password confirmation)
                       (user-partition
                        (inherit user-part)
                        (crypt-password password))
                       (begin
                         (run-error-page
                          (G_ "Password mismatch, please try again.")
                          (G_ "Password error"))
                         (loop)))))
               user-part)))
       user-partitions))

(define* (run-partition-page target-user-partition
                             #:key
                             (default-item #f))
  "Run a page allowing the user to edit the given TARGET-USER-PARTITION
record. If the argument DEFAULT-ITEM is passed, use it to select the current
listbox item. This is used to avoid the focus to switch back to the first
listbox entry while calling this procedure recursively."

  (define (numeric-size device size)
    "Parse the given SIZE on DEVICE and return it."
    (call-with-values
        (lambda ()
          (unit-parse size device))
      (lambda (value range)
        value)))

  (define (numeric-size-range device size)
    "Parse the given SIZE on DEVICE and return the associated RANGE."
    (call-with-values
        (lambda ()
          (unit-parse size device))
      (lambda (value range)
        range)))

  (define* (fill-user-partition-geom user-part
                                     #:key
                                     device (size #f) start end)
    "Return the given USER-PART with the START, END and SIZE fields set to the
eponym arguments. Use UNIT-FORMAT-CUSTOM to format START and END arguments as
sectors on DEVICE."
    (user-partition
     (inherit user-part)
     (size size)
     (start (unit-format-custom device start UNIT-SECTOR))
     (end (unit-format-custom device end UNIT-SECTOR))))

  (define (apply-user-partition-changes user-part)
    "Set the name, file-system type and boot flag on the partition specified
by USER-PART, if it is applicable for the partition type."
    (let* ((partition (user-partition-parted-object user-part))
           (disk (partition-disk partition))
           (disk-type (disk-disk-type disk))
           (device (disk-device disk))
           (has-name? (disk-type-check-feature
                       disk-type
                       DISK-TYPE-FEATURE-PARTITION-NAME))
           (name (user-partition-name user-part))
           (fs-type (filesystem-type-get
                     (user-fs-type-name
                      (user-partition-fs-type user-part))))
           (bootable? (user-partition-bootable? user-part))
           (esp? (user-partition-esp? user-part))
           (flag-bootable?
            (partition-is-flag-available? partition PARTITION-FLAG-BOOT))
           (flag-esp?
            (partition-is-flag-available? partition PARTITION-FLAG-ESP)))
      (when (and has-name? name)
        (partition-set-name partition name))
      (partition-set-system partition fs-type)
      (when flag-bootable?
        (partition-set-flag partition
                            PARTITION-FLAG-BOOT
                            (if bootable? 1 0)))
      (when flag-esp?
        (partition-set-flag partition
                            PARTITION-FLAG-ESP
                            (if esp? 1 0)))
      #t))

  (define (listbox-action listbox-item)
    (let* ((item (car listbox-item))
           (partition (user-partition-parted-object
                       target-user-partition))
           (disk (partition-disk partition))
           (device (disk-device disk)))
      (list
       item
       (case item
         ((name)
          (let* ((old-name (user-partition-name target-user-partition))
                 (name
                  (run-input-page (G_ "Please enter the partition gpt name.")
                                  (G_ "Partition name")
                                  #:default-text old-name)))
            (user-partition
             (inherit target-user-partition)
             (name name))))
         ((type)
          (let ((new-type (run-type-page partition)))
            (user-partition
             (inherit target-user-partition)
             (type new-type))))
         ((bootable)
          (user-partition
           (inherit target-user-partition)
           (bootable? (not (user-partition-bootable?
                            target-user-partition)))))
         ((esp?)
          (let ((new-esp? (not (user-partition-esp?
                                target-user-partition))))
            (user-partition
             (inherit target-user-partition)
             (esp? new-esp?)
             (mount-point (if new-esp?
                              (default-esp-mount-point)
                              "")))))
         ((crypt-label)
          (let* ((label (user-partition-crypt-label
                         target-user-partition))
                 (new-label
                  (and (not label)
                       (run-input-page
                        (G_ "Please enter the encrypted label")
                        (G_ "Encryption label")))))
            (user-partition
             (inherit target-user-partition)
             (need-formatting? #t)
             (crypt-label new-label))))
         ((need-formatting?)
          (user-partition
           (inherit target-user-partition)
           (need-formatting?
            (not (user-partition-need-formatting?
                  target-user-partition)))))
         ((size)
          (let* ((old-size (user-partition-size target-user-partition))
                 (max-size-value (partition-length partition))
                 (max-size (unit-format device max-size-value))
                 (start (partition-start partition))
                 (size (run-input-page
                        (format #f (G_ "Please enter the size of the partition.\
 The maximum size is ~a.") max-size)
                        (G_ "Partition size")
                        #:default-text (or old-size max-size)))
                 (size-percentage (read-percentage size))
                 (size-value (if size-percentage
                                 (nearest-exact-integer
                                  (/ (* max-size-value size-percentage)
                                     100))
                                 (numeric-size device size)))
                 (end (and size-value
                           (+ start size-value)))
                 (size-range (numeric-size-range device size))
                 (size-range-ok? (and size-range
                                      (< (+ start
                                            (geometry-start size-range))
                                         (partition-end partition)))))
            (cond
             ((and size-percentage (> size-percentage 100))
              (run-error-page
               (G_ "The percentage can not be superior to 100.")
               (G_ "Size error"))
              target-user-partition)
             ((not size-value)
              (run-error-page
               (G_ "The requested size is incorrectly formatted, or too large.")
               (G_ "Size error"))
              target-user-partition)
             ((not (or size-percentage size-range-ok?))
              (run-error-page
               (G_ "The request size is superior to the maximum size.")
               (G_ "Size error"))
              target-user-partition)
             (else
              (fill-user-partition-geom target-user-partition
                                        #:device device
                                        #:size size
                                        #:start start
                                        #:end end)))))
         ((fs-type)
          (let ((fs-type (run-fs-type-page)))
            (user-partition
             (inherit target-user-partition)
             (fs-type fs-type))))
         ((mount-point)
          (let* ((old-mount (or (user-partition-mount-point
                                 target-user-partition)
                                ""))
                 (mount
                  (run-input-page
                   (G_ "Please enter the desired mounting point for this \
partition. Leave this field empty if you don't want to set a mounting point.")
                   (G_ "Mounting point")
                   #:default-text old-mount
                   #:allow-empty-input? #t)))
            (user-partition
             (inherit target-user-partition)
             (mount-point (and (not (string=? mount ""))
                               mount)))))))))

  (define (button-action)
    (let* ((partition (user-partition-parted-object
                       target-user-partition))
           (prev-part (partition-prev partition))
           (disk (partition-disk partition))
           (device (disk-device disk))
           (creation? (freespace-partition? partition))
           (start (partition-start partition))
           (end (partition-end partition))
           (new-user-partition
            (if (user-partition-start target-user-partition)
                target-user-partition
                (fill-user-partition-geom target-user-partition
                                          #:device device
                                          #:start start
                                          #:end end))))
      ;; It the backend PARTITION has free-space type, it means we are
      ;; creating a new partition, otherwise, we are editing an already
      ;; existing PARTITION.
      (if creation?
          (let* ((ok-create-partition?
                  (inform-can-create-partition? new-user-partition))
                 (new-partition
                  (and ok-create-partition?
                       (mkpart disk
                               new-user-partition
                               #:previous-partition prev-part))))
            (and new-partition
                 (user-partition
                  (inherit new-user-partition)
                  (need-formatting? #t)
                  (file-name (partition-get-path new-partition))
                  (disk-file-name (device-path device))
                  (parted-object new-partition))))
          (and (apply-user-partition-changes new-user-partition)
               new-user-partition))))

  (let* ((items (user-partition-description target-user-partition))
         (partition (user-partition-parted-object
                     target-user-partition))
         (disk (partition-disk partition))
         (device (disk-device disk))
         (file-name (device-path device))
         (number-str (partition-print-number partition))
         (type (user-partition-type target-user-partition))
         (type-str (symbol->string type))
         (start (unit-format device (partition-start partition)))
         (creation? (freespace-partition? partition))
         (default-item (and default-item
                            (find (lambda (item)
                                    (eq? (car item) default-item))
                                  items)))
         (result
          (run-listbox-selection-page
           #:info-text
           (if creation?
               (format #f (G_ "Creating ~a partition starting at ~a of ~a.")
                       type-str start file-name)
               (format #f (G_ "You are currently editing partition ~a.")
                       number-str))
           #:title (if creation?
                       (G_ "Partition creation")
                       (G_ "Partition edit"))
           #:listbox-items items
           #:listbox-item->text cdr
           #:sort-listbox-items? #f
           #:listbox-default-item default-item
           #:button-text (G_ "OK")
           #:listbox-callback-procedure listbox-action
           #:button-callback-procedure button-action)))
    (match result
      ((item new-user-partition)
       (run-partition-page new-user-partition
                           #:default-item item))
      (else result))))

(define* (run-disk-page disks
                        #:optional (user-partitions '())
                        #:key (guided? #f))
  "Run a page allowing to edit the partition tables of the given DISKS. If
specified, USER-PARTITIONS is a list of <user-partition> records associated to
the partitions on DISKS."

  (define (other-logical-partitions? partitions)
    "Return #t if at least one of the partition in PARTITIONS list is a
logical partition, return #f otherwise."
    (any logical-partition? partitions))

  (define (other-non-logical-partitions? partitions)
    "Return #t is at least one of the partitions in PARTITIONS list is not a
logical partition, return #f otherwise."
    (let ((non-logical-partitions
           (remove logical-partition? partitions)))
      (or (any normal-partition? non-logical-partitions)
          (any freespace-partition? non-logical-partitions))))

  (define (add-tree-symbols partitions descriptions)
    "Concatenate tree symbols to the given DESCRIPTIONS list and return
it. The PARTITIONS list is the list of partitions described in
DESCRIPTIONS. The tree symbols are used to indicate the partition's disk and
for logical partitions, the extended partition which includes them."
    (match descriptions
      (() '())
      ((description . rest-descriptions)
       (match partitions
         ((partition . rest-partitions)
          (if (null? rest-descriptions)
              (list (if (logical-partition? partition)
                        (string-append " ┗━ " description)
                        (string-append "┗━  " description)))
              (cons (cond
                     ((extended-partition? partition)
                      (if (other-non-logical-partitions? rest-partitions)
                          (string-append "┣┳  " description)
                          (string-append "┗┳  " description)))
                     ((logical-partition? partition)
                      (if (other-logical-partitions? rest-partitions)
                          (if (other-non-logical-partitions? rest-partitions)
                              (string-append "┃┣━ " description)
                              (string-append " ┣━ " description))
                          (if (other-non-logical-partitions? rest-partitions)
                              (string-append "┃┗━ " description)
                              (string-append " ┗━ " description))))
                     (else
                      (string-append "┣━  " description)))
                    (add-tree-symbols rest-partitions
                                      rest-descriptions))))))))

  (define (skip-item? item)
    (eq? (car item) 'skip))

  (define (disk-items)
    "Return the list of strings describing DISKS."
    (let loop ((disks disks))
      (match disks
        (() '())
        ((disk . rest)
         (let* ((device (disk-device disk))
                (partitions (disk-partitions disk))
                (partitions*
                 (filter-map
                  (lambda (partition)
                    (and (not (metadata-partition? partition))
                         (not (small-freespace-partition? device
                                                          partition))
                         partition))
                  partitions))
                (descriptions (add-tree-symbols
                               partitions*
                               (partitions-descriptions partitions*
                                                        user-partitions)))
                (partition-items (map cons partitions* descriptions)))
           (append
            `((,disk . ,(device-description device disk))
              ,@partition-items
              ,@(if (null? rest)
                    '()
                    '((skip . ""))))
            (loop rest)))))))

  (define (remove-user-partition-by-partition user-partitions partition)
    "Return the USER-PARTITIONS list with the record with the given PARTITION
object removed. If PARTITION is an extended partition, also remove all logical
partitions from USER-PARTITIONS."
    (remove (lambda (p)
              (let ((cur-partition (user-partition-parted-object p)))
                (or (equal? cur-partition partition)
                    (and (extended-partition? partition)
                         (logical-partition? cur-partition)))))
            user-partitions))

  (define (remove-user-partition-by-disk user-partitions disk)
    "Return the USER-PARTITIONS list with the <user-partition> records located
on given DISK removed."
    (remove (lambda (p)
              (let* ((partition (user-partition-parted-object p))
                     (cur-disk (partition-disk partition)))
                (equal? cur-disk disk)))
            user-partitions))

  (define (update-user-partitions user-partitions new-user-partition)
    "Update or insert NEW-USER-PARTITION record in USER-PARTITIONS list
depending if one of the <user-partition> record in USER-PARTITIONS has the
same PARTITION object as NEW-USER-PARTITION."
    (let* ((partition (user-partition-parted-object new-user-partition))
           (user-partitions*
            (remove-user-partition-by-partition user-partitions
                                                partition)))
      (cons new-user-partition user-partitions*)))

  (define (button-ok-action)
    "Commit the modifications to all DISKS and return #t."
    (for-each (lambda (disk)
                (disk-commit disk))
              disks)
    #t)

  (define (listbox-action listbox-item)
    "A disk or a partition has been selected. If it's a disk, ask for a label
to create a new partition table. If it is a partition, propose the user to
edit it."
    (let ((item (car listbox-item)))
      (cond
       ((disk? item)
        (let ((label (run-label-page (G_ "Back") (const #f))))
          (if label
              (let* ((device (disk-device item))
                     (new-disk (mklabel device label))
                     (commit-new-disk (disk-commit new-disk))
                     (other-disks (remove (lambda (disk)
                                            (equal? disk item))
                                          disks))
                     (new-user-partitions
                      (remove-user-partition-by-disk user-partitions item)))
                (disk-destroy item)
                `((disks . ,(cons new-disk other-disks))
                  (user-partitions . ,new-user-partitions)))
              `((disks . ,disks)
                (user-partitions . ,user-partitions)))))
       ((partition? item)
        (let* ((partition item)
               (disk (partition-disk partition))
               (device (disk-device disk))
               (existing-user-partition
                (find-user-partition-by-parted-object user-partitions
                                                      partition))
               (edit-user-partition
                (or existing-user-partition
                    (partition->user-partition partition))))
          `((disks . ,disks)
            (user-partitions . ,user-partitions)
            (edit-user-partition . ,edit-user-partition)))))))

  (define (hotkey-action key listbox-item)
    "The DELETE key has been pressed on a disk or a partition item."
    (let ((item (car listbox-item))
          (default-result
            `((disks . ,disks)
              (user-partitions . ,user-partitions))))
      (cond
       ((disk? item)
        (let* ((device (disk-device item))
               (file-name (device-path device))
               (info-text
                (format #f (G_ "Are you sure you want to delete everything on disk ~a?")
                        file-name))
               (result (choice-window (G_ "Delete disk")
                                      (G_ "OK")
                                      (G_ "Exit")
                                      info-text)))
          (case result
            ((1)
             (disk-delete-all item)
             `((disks . ,disks)
               (user-partitions
                . ,(remove-user-partition-by-disk user-partitions item))))
            (else
             default-result))))
       ((partition? item)
        (if (freespace-partition? item)
            (run-error-page (G_ "You cannot delete a free space area.")
                            (G_ "Delete partition"))
            (let* ((disk (partition-disk item))
                   (number-str (partition-print-number item))
                   (info-text
                    (format #f (G_ "Are you sure you want to delete partition ~a?")
                            number-str))
                   (result (choice-window (G_ "Delete partition")
                                          (G_ "OK")
                                          (G_ "Exit")
                                          info-text)))
              (case result
                ((1)
                 (let ((new-user-partitions
                        (remove-user-partition-by-partition user-partitions
                                                            item)))
                   (disk-delete-partition disk item)
                   `((disks . ,disks)
                     (user-partitions . ,new-user-partitions))))
                (else
                 default-result))))))))

  (let* ((info-text (G_ "You can change a disk's partition table by \
selecting it and pressing ENTER. You can also edit a partition by selecting it \
and pressing ENTER, or remove it by pressing DELETE. To create a new \
partition, select a free space area and press ENTER.

At least one partition must have its mounting point set to '/'."))
         (guided-info-text (format #f (G_ "This is the proposed \
partitioning. It is still possible to edit it or to go back to install menu \
by pressing the Exit button.~%~%")))
         (result
          (run-listbox-selection-page
           #:info-text (if guided?
                           (string-append guided-info-text info-text)
                           info-text)

          #:title (if guided?
                      (G_ "Guided partitioning")
                      (G_ "Manual partitioning"))
          #:info-textbox-width 76         ;we need a lot of room for INFO-TEXT
          #:listbox-height 12
          #:listbox-items (disk-items)
          #:listbox-item->text cdr
          #:sort-listbox-items? #f
          #:skip-item-procedure? skip-item?
          #:allow-delete? #t
          #:button-text (G_ "OK")
          #:button-callback-procedure button-ok-action
          #:button2-text (G_ "Exit")
          #:button2-callback-procedure button-exit-action
          #:listbox-callback-procedure listbox-action
          #:hotkey-callback-procedure hotkey-action)))
    (if (eq? result #t)
        (let ((user-partitions-ok?
               (guard
                   (c ((no-root-mount-point? c)
                       (run-error-page
                        (G_ "No root mount point found.")
                        (G_ "Missing mount point"))
                       #f))
                 (check-user-partitions user-partitions))))
          (if user-partitions-ok?
              (begin
                (for-each (cut disk-destroy <>) disks)
                user-partitions)
              (run-disk-page disks user-partitions
                             #:guided? guided?)))
        (let* ((result-disks (assoc-ref result 'disks))
               (result-user-partitions (assoc-ref result
                                                  'user-partitions))
               (edit-user-partition (assoc-ref result
                                               'edit-user-partition))
               (can-create-partition?
                (and edit-user-partition
                     (inform-can-create-partition? edit-user-partition)))
               (new-user-partition (and edit-user-partition
                                        can-create-partition?
                                        (run-partition-page
                                         edit-user-partition)))
               (new-user-partitions
                (if new-user-partition
                    (update-user-partitions result-user-partitions
                                            new-user-partition)
                    result-user-partitions)))
          (run-disk-page result-disks new-user-partitions
                         #:guided? guided?)))))

(define (run-partioning-page)
  "Run a page asking the user for a partitioning method."
  (define (run-page devices)
    (let* ((items
            `((entire . ,(G_ "Guided - using the entire disk"))
              (entire-encrypted . ,(G_ "Guided - using the entire disk with encryption"))
              (manual . ,(G_ "Manual"))))
           (result (run-listbox-selection-page
                    #:info-text (G_ "Please select a partitioning method.")
                    #:title (G_ "Partitioning method")
                    #:listbox-items items
                    #:listbox-item->text cdr
                    #:button-text (G_ "Exit")
                    #:button-callback-procedure button-exit-action))
           (method (car result)))
      (cond
       ((or (eq? method 'entire)
            (eq? method 'entire-encrypted))
         (let* ((device (run-device-page devices))
                (disk-type (disk-probe device))
                (disk (if disk-type
                          (disk-new device)
                          (let* ((label (run-label-page
                                         (G_ "Exit")
                                         button-exit-action))
                                 (disk (mklabel device label)))
                            (disk-commit disk)
                            disk)))
                (scheme (symbol-append method '- (run-scheme-page)))
                (user-partitions (append
                                  (auto-partition disk #:scheme scheme)
                                  (create-special-user-partitions
                                   (disk-partitions disk)))))
           (run-disk-page (list disk) user-partitions
                          #:guided? #t)))
       ((eq? method 'manual)
         (let* ((disks (filter-map disk-new devices))
                (user-partitions (append-map
                                  create-special-user-partitions
                                  (map disk-partitions disks)))
                (result-user-partitions (run-disk-page disks
                                                       user-partitions)))
           result-user-partitions)))))

  (init-parted)
  (let* ((non-install-devices (non-install-devices))
         (user-partitions (run-page non-install-devices))
         (user-partitions-with-pass (prompt-luks-passwords
                                     user-partitions))
         (form (draw-formatting-page)))
    ;; Make sure the disks are not in use before proceeding to formatting.
    (free-parted non-install-devices)
    (format-user-partitions user-partitions-with-pass)
    (destroy-form-and-pop form)
    user-partitions))
