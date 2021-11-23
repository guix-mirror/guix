;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Google LLC
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu system file-systems)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (guix records)
  #:use-module ((guix diagnostics)
                #:select (source-properties->location leave &fix-hint))
  #:use-module (guix i18n)
  #:use-module (gnu system uuid)
  #:re-export (uuid                               ;backward compatibility
               string->uuid
               uuid->string)
  #:export (file-system
            file-system?
            file-system-device
            file-system-device->string
            file-system-title                     ;deprecated
            file-system-mount-point
            file-system-type
            file-system-needed-for-boot?
            file-system-flags
            file-system-options
            file-system-options->alist
            alist->file-system-options

            file-system-mount?
            file-system-mount-may-fail?
            file-system-check?
            file-system-skip-check-if-clean?
            file-system-repair
            file-system-create-mount-point?
            file-system-dependencies
            file-system-location

            file-system-type-predicate
            btrfs-subvolume?
            btrfs-store-subvolume-file-name

            file-system-label
            file-system-label?
            file-system-label->string

            file-system->spec
            spec->file-system
            specification->file-system-mapping

            %pseudo-file-system-types
            %fuse-control-file-system
            %binary-format-file-system
            %debug-file-system
            %efivars-file-system
            %shared-memory-file-system
            %pseudo-terminal-file-system
            %tty-gid
            %immutable-store
            %control-groups
            %elogind-file-systems

            %base-file-systems
            %container-file-systems

            <file-system-mapping>
            file-system-mapping
            file-system-mapping?
            file-system-mapping-source
            file-system-mapping-target
            file-system-mapping-writable?

            file-system-mapping->bind-mount

            %store-mapping
            %network-configuration-files
            %network-file-mappings

            swap-space
            swap-space?
            swap-space-target
            swap-space-dependencies
            swap-space-priority
            swap-space-discard?))

;;; Commentary:
;;;
;;; Declaring file systems to be mounted.
;;;
;;; Note: this file system is used both in the Shepherd and on the "host
;;; side", so it must not include (gnu packages …) modules.
;;;
;;; Code:

(eval-when (expand load eval)
  (define invalid-file-system-flags
    ;; Note: Keep in sync with 'mount-flags->bit-mask'.
    (let ((known-flags '(read-only
                         bind-mount no-suid no-dev no-exec
                         no-atime strict-atime lazy-time)))
      (lambda (flags)
        "Return the subset of FLAGS that is invalid."
        (remove (cut memq <> known-flags) flags))))

  (define (%validate-file-system-flags flags location)
    "Raise an error if FLAGS contains invalid mount flags; otherwise return
FLAGS."
    (match (invalid-file-system-flags flags)
      (() flags)
      (invalid
       (leave (source-properties->location location)
              (N_ "invalid file system mount flag:~{ ~s~}~%"
                  "invalid file system mount flags:~{ ~s~}~%"
                  (length invalid))
              invalid)))))

(define-syntax validate-file-system-flags
  (lambda (s)
    "Validate the given file system mount flags, raising an error if invalid
flags are found."
    (syntax-case s (quote)
      ((_ (quote (symbols ...)))                  ;validate at expansion time
       (begin
         (%validate-file-system-flags (syntax->datum #'(symbols ...))
                                      (syntax-source s))
         #'(quote (symbols ...))))
      ((_ flags)
       #`(%validate-file-system-flags flags
                                      '#,(datum->syntax s (syntax-source s))))
      (id
       (identifier? #'id)
       #'%validate-file-system-flags))))

;; File system declaration.
(define-record-type* <file-system> %file-system
  make-file-system
  file-system?
  (device           file-system-device) ; string | <uuid> | <file-system-label>
  (mount-point      file-system-mount-point)      ; string
  (type             file-system-type)             ; string
  (flags            file-system-flags             ; list of symbols
                    (default '())
                    (sanitize validate-file-system-flags))
  (options          file-system-options           ; string or #f
                    (default #f))
  (mount?           file-system-mount?            ; Boolean
                    (default #t))
  (mount-may-fail?  file-system-mount-may-fail?   ; Boolean
                    (default #f))
  (needed-for-boot? %file-system-needed-for-boot? ; Boolean
                    (default #f))
  (check?           file-system-check?            ; Boolean
                    (default #t))
  (skip-check-if-clean? file-system-skip-check-if-clean? ; Boolean
                        (default #t))
  (repair           file-system-repair            ; symbol or #f
                    (default 'preen))
  (create-mount-point? file-system-create-mount-point? ; Boolean
                       (default #f))
  (dependencies     file-system-dependencies      ; list of <file-system>
                    (default '()))                ; or <mapped-device>
  (location         file-system-location
                    (default (current-source-location))
                    (innate)))

;; A file system label for use in the 'device' field.
(define-record-type <file-system-label>
  (file-system-label label)
  file-system-label?
  (label file-system-label->string))

(set-record-type-printer! <file-system-label>
                          (lambda (obj port)
                            (format port "#<file-system-label ~s>"
                                    (file-system-label->string obj))))

(define-syntax report-deprecation
  (lambda (s)
    "Report the use of the now-deprecated 'title' field."
    (syntax-case s ()
      ((_ field)
       (let* ((source (syntax-source #'field))
              (file   (and source (assq-ref source 'filename)))
              (line   (and source
                           (and=> (assq-ref source 'line) 1+)))
              (column (and source (assq-ref source 'column))))
         (format (current-error-port)
                 "~a:~a:~a: warning: 'title' field is deprecated~%"
                 file line column)
         #t)))))

;; Helper for 'process-file-system-declaration'.
(define-syntax device-expression
  (syntax-rules (quote label uuid device)
    ((_ (quote label) dev)
     (file-system-label dev))
    ((_ (quote uuid) dev)
     (if (uuid? dev) dev (uuid dev)))
    ((_ (quote device) dev)
     dev)
    ((_ title dev)
     (case title
       ((label) (file-system-label dev))
       ((uuid)  (uuid dev))
       (else    dev)))))

;; Helper to interpret the now-deprecated 'title' field.  Detect forms like
;; (title 'label), remove them, and adjust the 'device' field accordingly.
;; TODO: Remove this once 'title' has been deprecated long enough.
(define-syntax process-file-system-declaration
  (syntax-rules (device title)
    ((_ () (rest ...) #f #f)                 ;no 'title' and no 'device' field
     (%file-system rest ...))
    ((_ () (rest ...) dev #f)                     ;no 'title' field
     (%file-system rest ... (device dev)))
    ((_ () (rest ...) dev titl)                   ;got a 'title' field
     (%file-system rest ...
                   (device (device-expression titl dev))))
    ((_ ((title titl) rest ...) (previous ...) dev _)
     (begin
       (report-deprecation (title titl))
       (process-file-system-declaration (rest ...)
                                        (previous ...)
                                        dev titl)))
    ((_ ((device dev) rest ...) (previous ...) _ titl)
     (process-file-system-declaration (rest ...)
                                      (previous ...)
                                      dev titl))
    ((_ (field rest ...) (previous ...) dev titl)
     (process-file-system-declaration (rest ...)
                                      (previous ... field)
                                      dev titl))))

(define-syntax-rule (file-system fields ...)
  (process-file-system-declaration (fields ...) () #f #f))

(define (file-system-title fs)                    ;deprecated
  (match (file-system-device fs)
    ((? file-system-label?) 'label)
    ((? uuid?)              'uuid)
    ((? string?)            'device)))

;; Note: This module is used both on the build side and on the host side.
;; Arrange not to pull (guix store) and (guix config) because the latter
;; differs from user to user.
(define (%store-prefix)
  "Return the store prefix."
  ;; Note: If we have (guix store database) in the search path and we do *not*
  ;; have (guix store) proper, 'resolve-module' returns an empty (guix store)
  ;; with one sub-module.
  (cond ((and=> (resolve-module '(guix store) #:ensure #f)
                (lambda (store)
                  (module-variable store '%store-prefix)))
         =>
         (lambda (variable)
           ((variable-ref variable))))
        ((getenv "NIX_STORE")
         => identity)
        (else
         "/gnu/store")))

(define %not-slash
  (char-set-complement (char-set #\/)))

(define (file-prefix? file1 file2)
  "Return #t if FILE1 denotes the name of a file that is a parent of FILE2.
FILE1 and FILE2 must both be either absolute or relative file names, else #f
is returned.

For example:

  (file-prefix? \"/gnu\" \"/gnu/store\")
  => #t

  (file-prefix? \"/gn\" \"/gnu/store\")
  => #f
"
  (define (absolute? file)
    (string-prefix? "/" file))

  (if (or (every absolute? (list file1 file2))
          (every (negate absolute?) (list file1 file2)))
      (let loop ((file1 (string-tokenize file1 %not-slash))
                 (file2 (string-tokenize file2 %not-slash)))
        (match file1
          (()
           #t)
          ((head1 tail1 ...)
           (match file2
             ((head2 tail2 ...)
              (and (string=? head1 head2) (loop tail1 tail2)))
             (()
              #f)))))
      ;; FILE1 and FILE2 are a mix of absolute and relative file names.
      #f))

(define (file-name-depth file-name)
  (length (string-tokenize file-name %not-slash)))

(define* (file-system-device->string device #:key uuid-type)
  "Return the string representations of the DEVICE field of a <file-system>
record.  When the device is a UUID, its representation is chosen depending on
UUID-TYPE, a symbol such as 'dce or 'iso9660."
  (match device
    ((? file-system-label?)
     (file-system-label->string device))
    ((? uuid?)
     (if uuid-type
         (uuid->string (uuid-bytevector device) uuid-type)
         (uuid->string device)))
    ((? string?)
     device)))

(define (file-system-options->alist string)
  "Translate the option string format of a <file-system> record into an
association list of options or option/value pairs."
  (if string
      (let ((options (string-split string #\,)))
        (map (lambda (param)
               (let ((=index (string-index param #\=)))
                 (if =index
                     (cons (string-take param =index)
                           (string-drop param (1+ =index)))
                     param)))
             options))
      '()))

(define (alist->file-system-options options)
  "Return the string representation of OPTIONS, an association list.  The
string obtained can be used as the option field of a <file-system> record."
  (if (null? options)
      #f
      (string-join (map (match-lambda
                          ((key . value)
                           (string-append key "=" value))
                          (key
                           key))
                        options)
                   ",")))

(define (file-system-needed-for-boot? fs)
  "Return true if FS has the 'needed-for-boot?' flag set, or if it holds the
store--e.g., if FS is the root file system."
  (or (%file-system-needed-for-boot? fs)
      (and (file-prefix? (file-system-mount-point fs) (%store-prefix))
           (not (memq 'bind-mount (file-system-flags fs))))))

(define (file-system->spec fs)
  "Return a list corresponding to file-system FS that can be passed to the
initrd code."
  (match fs
    (($ <file-system> device mount-point type flags options mount?
                      mount-may-fail? needed-for-boot?
                      check? skip-check-if-clean? repair)
     ;; Note: Add new fields towards the end for compatibility.
     (list (cond ((uuid? device)
                  `(uuid ,(uuid-type device) ,(uuid-bytevector device)))
                 ((file-system-label? device)
                  `(file-system-label ,(file-system-label->string device)))
                 (else device))
           mount-point type flags options mount-may-fail?
           check? skip-check-if-clean? repair))))

(define (spec->file-system sexp)
  "Deserialize SEXP, a list, to the corresponding <file-system> object."
  (match sexp
    ((device mount-point type flags options mount-may-fail?
             check? skip-check-if-clean? repair
             _ ...)                               ;placeholder for new fields
     (file-system
       (device (match device
                 (('uuid (? symbol? type) (? bytevector? bv))
                  (bytevector->uuid bv type))
                 (('file-system-label (? string? label))
                  (file-system-label label))
                 (_
                  device)))
       (mount-point mount-point) (type type)
       (flags flags) (options options)
       (mount-may-fail? mount-may-fail?)
       (check? check?)
       (skip-check-if-clean? skip-check-if-clean?)
       (repair repair)))))

(define (specification->file-system-mapping spec writable?)
  "Read the SPEC and return the corresponding <file-system-mapping>.  SPEC is
a string of the form \"SOURCE\" or \"SOURCE=TARGET\".  The former specifies
that SOURCE from the host should be mounted at SOURCE in the other system.
The latter format specifies that SOURCE from the host should be mounted at
TARGET in the other system."
  (let ((index (string-index spec #\=)))
    (if index
        (file-system-mapping
         (source (substring spec 0 index))
         (target (substring spec (+ 1 index)))
         (writable? writable?))
        (file-system-mapping
         (source spec)
         (target spec)
         (writable? writable?)))))


;;;
;;; Common file systems.
;;;

(define %pseudo-file-system-types
  ;; List of know pseudo file system types.  This is used when validating file
  ;; system definitions.
  '("binfmt_misc" "cgroup" "debugfs" "devpts" "devtmpfs" "efivarfs" "fusectl"
    "hugetlbfs" "overlay" "proc" "securityfs" "sysfs" "tmpfs"))

(define %fuse-control-file-system
  ;; Control file system for Linux' file systems in user-space (FUSE).
  (file-system
    (device "fusectl")
    (mount-point "/sys/fs/fuse/connections")
    (type "fusectl")
    (check? #f)))

(define %binary-format-file-system
  ;; Support for arbitrary executable binary format.
  (file-system
    (device "binfmt_misc")
    (mount-point "/proc/sys/fs/binfmt_misc")
    (type "binfmt_misc")
    (check? #f)))

(define %debug-file-system
  (file-system
    (type "debugfs")
    (device "none")
    (mount-point "/sys/kernel/debug")
    (check? #f)
    (create-mount-point? #t)))

(define %efivars-file-system
  ;; Support for EFI variables file system.
  (file-system
    (device "efivarfs")
    (mount-point "/sys/firmware/efi/efivars")
    (type "efivarfs")
    (mount-may-fail? #t)
    (needed-for-boot? #f)
    (check? #f)))

(define %tty-gid
  ;; ID of the 'tty' group.  Allocate it statically to make it easy to refer
  ;; to it from here and from the 'tty' group definitions.
  996)

(define %pseudo-terminal-file-system
  ;; The pseudo-terminal file system.  It needs to be mounted so that
  ;; statfs(2) returns DEVPTS_SUPER_MAGIC like libc's getpt(3) expects (and
  ;; thus openpty(3) and its users, such as xterm.)
  (file-system
    (device "none")
    (mount-point "/dev/pts")
    (type "devpts")
    (check? #f)
    (needed-for-boot? #f)
    (create-mount-point? #t)
    (options (string-append "gid=" (number->string %tty-gid) ",mode=620"))))

(define %shared-memory-file-system
  ;; Shared memory.
  (file-system
    (device "tmpfs")
    (mount-point "/dev/shm")
    (type "tmpfs")
    (check? #f)
    (flags '(no-suid no-dev))
    (options "size=50%")                         ;TODO: make size configurable
    (create-mount-point? #t)))

(define %immutable-store
  ;; Read-only store to avoid users or daemons accidentally modifying it.
  ;; 'guix-daemon' has provisions to remount it read-write in its own name
  ;; space.
  (file-system
    (device (%store-prefix))
    (mount-point (%store-prefix))
    (type "none")
    (check? #f)
    (flags '(read-only bind-mount no-atime))))

(define %control-groups
  (let ((parent (file-system
                  (device "cgroup")
                  (mount-point "/sys/fs/cgroup")
                  (type "tmpfs")
                  (check? #f))))
    (cons parent
          (map (lambda (subsystem)
                 (file-system
                   (device "cgroup")
                   (mount-point (string-append "/sys/fs/cgroup/" subsystem))
                   (type "cgroup")
                   (check? #f)
                   (options subsystem)
                   (create-mount-point? #t)

                   ;; This must be mounted after, and unmounted before the
                   ;; parent directory.
                   (dependencies (list parent))))
               '("cpuset" "cpu" "cpuacct" "memory" "devices" "freezer"
                 "blkio" "perf_event" "pids")))))

(define %elogind-file-systems
  ;; We don't use systemd, but these file systems are needed for elogind,
  ;; which was extracted from systemd.
  (append
   (list (file-system
           (device "none")
           (mount-point "/run/systemd")
           (type "tmpfs")
           (check? #f)
           (flags '(no-suid no-dev no-exec))
           (options "mode=0755")
           (create-mount-point? #t))
         (file-system
           (device "none")
           (mount-point "/run/user")
           (type "tmpfs")
           (check? #f)
           (flags '(no-suid no-dev no-exec))
           (options "mode=0755")
           (create-mount-point? #t))
         ;; Elogind uses cgroups to organize processes, allowing it to map PIDs
         ;; to sessions.  Elogind's cgroup hierarchy isn't associated with any
         ;; resource controller ("subsystem").
         (file-system
           (device "cgroup")
           (mount-point "/sys/fs/cgroup/elogind")
           (type "cgroup")
           (check? #f)
           (options "none,name=elogind")
           (create-mount-point? #t)
           (dependencies (list (car %control-groups)))))
   %control-groups))

(define %base-file-systems
  ;; List of basic file systems to be mounted.  Note that /proc and /sys are
  ;; currently mounted by the initrd.
  (list %pseudo-terminal-file-system
        %debug-file-system
        %shared-memory-file-system
        %efivars-file-system
        %immutable-store))

;; File systems for Linux containers differ from %base-file-systems in that
;; they impose additional restrictions such as no-exec or need different
;; options to function properly.
;;
;; The file system flags and options conform to the libcontainer
;; specification:
;; https://github.com/docker/libcontainer/blob/master/SPEC.md#filesystem
(define %container-file-systems
  (list
   ;; Pseudo-terminal file system.
   (file-system
     (device "none")
     (mount-point "/dev/pts")
     (type "devpts")
     (flags '(no-exec no-suid))
     (needed-for-boot? #t)
     (create-mount-point? #t)
     (check? #f)
     (options "newinstance,ptmxmode=0666,mode=620"))
   ;; Shared memory file system.
   (file-system
     (device "tmpfs")
     (mount-point "/dev/shm")
     (type "tmpfs")
     (flags '(no-exec no-suid no-dev))
     (options "mode=1777,size=65536k")
     (needed-for-boot? #t)
     (create-mount-point? #t)
     (check? #f))
   ;; Message queue file system.
   (file-system
     (device "mqueue")
     (mount-point "/dev/mqueue")
     (type "mqueue")
     (flags '(no-exec no-suid no-dev))
     (needed-for-boot? #t)
     (create-mount-point? #t)
     (check? #f))))


;;;
;;; Shared file systems, for VMs/containers.
;;;

;; Mapping of host file system SOURCE to mount point TARGET in the guest.
(define-record-type* <file-system-mapping> file-system-mapping
  make-file-system-mapping
  file-system-mapping?
  (source    file-system-mapping-source)          ;string
  (target    file-system-mapping-target)          ;string
  (writable? file-system-mapping-writable?        ;Boolean
             (default #f)))

(define (file-system-mapping->bind-mount mapping)
  "Return a file system that realizes MAPPING, a <file-system-mapping>, using
a bind mount."
  (match mapping
    (($ <file-system-mapping> source target writable?)
     (file-system
       (mount-point target)
       (device source)
       (type "none")
       (flags (if writable?
                  '(bind-mount)
                  '(bind-mount read-only)))
       (check? #f)
       (create-mount-point? #t)))))

(define %store-mapping
  ;; Mapping of the host's store into the guest.
  (file-system-mapping
   (source (%store-prefix))
   (target (%store-prefix))
   (writable? #f)))

(define %network-configuration-files
  ;; List of essential network configuration files.
  '("/etc/resolv.conf"
    "/etc/nsswitch.conf"
    "/etc/services"
    "/etc/hosts"))

(define %network-file-mappings
  ;; List of file mappings for essential network files.
  (filter-map (lambda (file)
                (file-system-mapping
                 (source file)
                 (target file)
                 ;; XXX: On some GNU/Linux systems, /etc/resolv.conf is a
                 ;; symlink to a file in a tmpfs which, for an unknown reason,
                 ;; cannot be bind mounted read-only within the container.
                 (writable? (string=? file "/etc/resolv.conf"))))
              %network-configuration-files))

(define (file-system-type-predicate type)
  "Return a predicate that, when passed a file system, returns #t if that file
system has the given TYPE."
  (lambda (fs)
    (string=? (file-system-type fs) type)))


;;;
;;; Btrfs specific helpers.
;;;

(define (btrfs-subvolume? fs)
  "Predicate to check if FS, a file-system object, is a Btrfs subvolume."
  (and-let* ((btrfs-file-system? (string= "btrfs" (file-system-type fs)))
             (option-keys (map (match-lambda
                                 ((key . value) key)
                                 (key key))
                               (file-system-options->alist
                                (file-system-options fs)))))
    (find (cut string-prefix? "subvol" <>) option-keys)))

(define (btrfs-store-subvolume-file-name file-systems)
  "Return the subvolume file name within the Btrfs top level onto which the
store is located, else #f."

  (define (prepend-slash/maybe s)
    (if (string=? "/" (string-take s 1))
        s
        (string-append "/" s)))

  (and-let* ((btrfs-subvolume-fs (filter btrfs-subvolume? file-systems))
             (btrfs-subvolume-fs*
              (sort btrfs-subvolume-fs
                    (lambda (fs1 fs2)
                      (> (file-name-depth (file-system-mount-point fs1))
                         (file-name-depth (file-system-mount-point fs2))))))
             (store-subvolume-fs
              (find (lambda (fs) (file-prefix? (file-system-mount-point fs)
                                               (%store-prefix)))
                    btrfs-subvolume-fs*))
             (options (file-system-options->alist
                       (file-system-options store-subvolume-fs))))
    ;; XXX: Deriving the subvolume name based from a subvolume ID is not
    ;; supported, as we'd need to query the actual file system.
    (or (and=> (assoc-ref options "subvol") prepend-slash/maybe)
        (raise (condition
                (&message
                 (message "The store is on a Btrfs subvolume, but the \
subvolume name is unknown."))
                (&fix-hint
                 (hint
                  (G_ "Use the @code{subvol} Btrfs file system option."))))))))


;;;
;;; Swap space
;;;

(define-record-type* <swap-space> swap-space make-swap-space
  swap-space?
  this-swap-space
  (target swap-space-target)
  (dependencies swap-space-dependencies
                (default '()))
  (priority swap-space-priority
            (default #f))
  (discard? swap-space-discard?
           (default #f)))

;;; file-systems.scm ends here
