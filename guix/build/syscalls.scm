;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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

(define-module (guix build syscalls)
  #:use-module (system foreign)
  #:use-module (system base target)             ;for cross-compilation support
  #:use-module (rnrs bytevectors)
  #:autoload   (ice-9 binary-ports) (get-bytevector-n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:export (MS_RDONLY
            MS_NOSUID
            MS_NODEV
            MS_NOEXEC
            MS_REMOUNT
            MS_NOATIME
            MS_BIND
            MS_MOVE
            MS_STRICTATIME
            MNT_FORCE
            MNT_DETACH
            MNT_EXPIRE
            UMOUNT_NOFOLLOW

            AT_FDCWD
            AT_SYMLINK_NOFOLLOW
            AT_REMOVEDIR
            AT_SYMLINK_FOLLOW
            AT_NO_AUTOMOUNT
            AT_EMPTY_PATH

            restart-on-EINTR
            mount-points
            swapon
            swapoff

            file-system?
            file-system-type
            file-system-block-size
            file-system-block-count
            file-system-blocks-free
            file-system-blocks-available
            file-system-file-count
            file-system-free-file-nodes
            file-system-identifier
            file-system-maximum-name-length
            file-system-fragment-size
            file-system-mount-flags
            statfs
            free-disk-space
            device-in-use?

            processes
            mkdtemp!
            fdatasync
            pivot-root
            scandir*
            fcntl-flock

            set-thread-name
            thread-name

            CLONE_CHILD_CLEARTID
            CLONE_CHILD_SETTID
            CLONE_NEWNS
            CLONE_NEWUTS
            CLONE_NEWIPC
            CLONE_NEWUSER
            CLONE_NEWPID
            CLONE_NEWNET
            clone
            setns

            PF_PACKET
            AF_PACKET
            all-network-interface-names
            network-interface-names
            network-interface-netmask
            network-interface-running?
            loopback-network-interface?
            arp-network-interface?
            network-interface-address
            set-network-interface-netmask
            set-network-interface-up
            configure-network-interface
            add-network-route/gateway
            delete-network-route

            interface?
            interface-name
            interface-flags
            interface-address
            interface-netmask
            interface-broadcast-address
            network-interfaces

            termios?
            termios-input-flags
            termios-output-flags
            termios-control-flags
            termios-local-flags
            termios-line-discipline
            termios-control-chars
            termios-input-speed
            termios-output-speed
            local-flags
            input-flags
            tcsetattr-action
            tcgetattr
            tcsetattr

            window-size?
            window-size-rows
            window-size-columns
            window-size-x-pixels
            window-size-y-pixels
            terminal-window-size
            terminal-columns

            utmpx?
            utmpx-login-type
            utmpx-pid
            utmpx-line
            utmpx-id
            utmpx-user
            utmpx-host
            utmpx-termination-status
            utmpx-exit-status
            utmpx-session-id
            utmpx-time
            utmpx-address
            login-type
            utmpx-entries
            (read-utmpx-from-port . read-utmpx)))

;;; Commentary:
;;;
;;; This module provides bindings to libc's syscall wrappers.  It uses the
;;; FFI, and thus requires a dynamically-linked Guile.
;;;
;;; Some syscalls are already defined in statically-linked Guile by applying
;;; 'guile-linux-syscalls.patch'.
;;;
;;; Visibility of syscall's symbols shared between this module and static Guile
;;; is a bit delicate. It is handled by 'define-as-needed' macro.
;;;
;;; This macro is used to export symbols in dynamic Guile context, and to
;;; re-export them in static Guile context.
;;;
;;; This way, even if they don't appear in #:export list, it is safe to use
;;; syscalls from this module in static or dynamic Guile context.
;;;
;;; Code:


;;;
;;; Packed structures.
;;;

(define-syntax sizeof*
  ;; XXX: This duplicates 'compile-time-value'.
  (syntax-rules (int128 array)
    ((_ int128)
     16)
    ((_ (array type n))
     (* (sizeof* type) n))
    ((_ type)
     (let-syntax ((v (lambda (s)
                       (let ((val (sizeof type)))
                         (syntax-case s ()
                           (_ val))))))
       v))))

(define-syntax alignof*
  ;; XXX: This duplicates 'compile-time-value'.
  (syntax-rules (int128 array)
    ((_ int128)
     16)
    ((_ (array type n))
     (alignof* type))
    ((_ type)
     (let-syntax ((v (lambda (s)
                       (let ((val (alignof type)))
                         (syntax-case s ()
                           (_ val))))))
       v))))

(define-syntax align                             ;as found in (system foreign)
  (syntax-rules (~)
    "Add to OFFSET whatever it takes to get proper alignment for TYPE."
    ((_ offset (type ~ endianness))
     (align offset type))
    ((_ offset type)
     (1+ (logior (1- offset) (1- (alignof* type)))))))

(define-syntax type-size
  (syntax-rules (~)
    ((_ (type ~ order))
     (sizeof* type))
    ((_ type)
     (sizeof* type))))

(define-syntax struct-alignment
  (syntax-rules ()
    "Compute the alignment for the aggregate made of TYPES at OFFSET.  The
result is the alignment of the \"most strictly aligned component\"."
    ((_ offset types ...)
     (max (align offset types) ...))))

(define-syntax struct-size
  (syntax-rules ()
    "Return the size in bytes of the structure made of TYPES."
    ((_ offset (types-processed ...))
     ;; The SysV ABI P.S. says: "Aggregates (structures and arrays) and unions
     ;; assume the alignment of their most strictly aligned component."  As an
     ;; example, a struct such as "int32, int16" has size 8, not 6.
     (1+ (logior (1- offset)
                 (1- (struct-alignment offset types-processed ...)))))
    ((_ offset (types-processed ...) type0 types ...)
     (struct-size (+ (type-size type0) (align offset type0))
                  (type0 types-processed ...)
                  types ...))))

(define-syntax write-type
  (syntax-rules (~ array *)
    ((_ bv offset (type ~ order) value)
     (bytevector-uint-set! bv offset value
                           (endianness order) (sizeof* type)))
    ((_ bv offset (array type n) value)
     (let loop ((i 0)
                (value value)
                (o offset))
       (unless (= i n)
         (match value
           ((head . tail)
            (write-type bv o type head)
            (loop (+ 1 i) tail (+ o (sizeof* type))))))))
    ((_ bv offset '* value)
     (bytevector-uint-set! bv offset (pointer-address value)
                           (native-endianness) (sizeof* '*)))
    ((_ bv offset type value)
     (bytevector-uint-set! bv offset value
                           (native-endianness) (sizeof* type)))))

(define-syntax write-types
  (syntax-rules ()
    ((_ bv offset () ())
     #t)
    ((_ bv offset (type0 types ...) (field0 fields ...))
     (begin
       (write-type bv (align offset type0) type0 field0)
       (write-types bv
                    (+ (align offset type0) (type-size type0))
                    (types ...) (fields ...))))))

(define-syntax read-type
  (syntax-rules (~ array quote *)
    ((_ bv offset '*)
     (make-pointer (bytevector-uint-ref bv offset
                                        (native-endianness)
                                        (sizeof* '*))))
    ((_ bv offset (type ~ order))
     (bytevector-uint-ref bv offset
                          (endianness order) (sizeof* type)))
    ((_ bv offset (array type n))
     (unfold (lambda (i) (= i n))
             (lambda (i)
               (read-type bv (+ offset (* i (sizeof* type))) type))
             1+
             0))
    ((_ bv offset type)
     (bytevector-uint-ref bv offset
                          (native-endianness) (sizeof* type)))))

(define-syntax read-types
  (syntax-rules ()
    ((_ return bv offset () (values ...))
     (return values ...))
    ((_ return bv offset (type0 types ...) (values ...))
     (read-types return
                 bv
                 (+ (align offset type0) (type-size type0))
                 (types ...)
                 (values ... (read-type bv
                                        (align offset type0)
                                        type0))))))

(define-syntax define-c-struct-macro
  (syntax-rules ()
    "Define NAME as a macro that can be queried to get information about the C
struct it represents.  In particular:

  (NAME field-offset FIELD)

returns the offset in bytes of FIELD within the C struct represented by NAME."
    ((_ name ((fields types) ...))
     (define-c-struct-macro name
       (fields ...) 0 ()
       ((fields types) ...)))
    ((_ name (fields ...) offset (clauses ...) ((field type) rest ...))
     (define-c-struct-macro name
       (fields ...)
       (+ (align offset type) (type-size type))
       (clauses ... ((_ field-offset field) (align offset type)))
       (rest ...)))
    ((_ name (fields ...) offset (clauses ...) ())
     (define-syntax name
       (syntax-rules (field-offset fields ...)
         clauses ...)))))

(define-syntax define-c-struct
  (syntax-rules ()
    "Define SIZE as the size in bytes of the C structure made of FIELDS.  READ
as a deserializer and WRITE! as a serializer for the C structure with the
given TYPES.  READ uses WRAP-FIELDS to return its value."
    ((_ name size wrap-fields read write! (fields types) ...)
     (begin
       (define-c-struct-macro name
         ((fields types) ...))
       (define size
         (struct-size 0 () types ...))
       (define (write! bv offset fields ...)
         (write-types bv offset (types ...) (fields ...)))
       (define* (read bv #:optional (offset 0))
         (read-types wrap-fields bv offset (types ...) ()))))))

(define-syntax-rule (c-struct-field-offset type field)
  "Return the offset in BYTES of FIELD within TYPE, where TYPE is a C struct
defined with 'define-c-struct' and FIELD is a field identifier.  An
expansion-time error is raised if FIELD does not exist in TYPE."
  (type field-offset field))


;;;
;;; FFI.
;;;

(define (call-with-restart-on-EINTR thunk)
  (let loop ()
    (catch 'system-error
      thunk
      (lambda args
        (if (= (system-error-errno args) EINTR)
            (loop)
            (apply throw args))))))

(define-syntax-rule (restart-on-EINTR expr)
  "Evaluate EXPR and restart upon EINTR.  Return the value of EXPR."
  (call-with-restart-on-EINTR (lambda () expr)))

(define (syscall->procedure return-type name argument-types)
  "Return a procedure that wraps the C function NAME using the dynamic FFI,
and that returns two values: NAME's return value, and errno.

If an error occurs while creating the binding, defer the error report until
the returned procedure is called."
  (catch #t
    (lambda ()
      (let ((ptr (dynamic-func name (dynamic-link))))
        ;; The #:return-errno? facility was introduced in Guile 2.0.12.
        (pointer->procedure return-type ptr argument-types
                            #:return-errno? #t)))
    (lambda args
      (lambda _
        (throw 'system-error name  "~A" (list (strerror ENOSYS))
               (list ENOSYS))))))

(define-syntax define-as-needed
  (syntax-rules ()
    "Define VARIABLE.  If VARIABLE already exists in (guile) then re-export it,
  otherwise export the newly-defined VARIABLE."
    ((_ (proc args ...) body ...)
     (define-as-needed proc (lambda* (args ...) body ...)))
    ((_ variable value)
     (begin
       (when (module-defined? the-scm-module 'variable)
         (re-export variable))

       (define variable
         (if (module-defined? the-scm-module 'variable)
             (module-ref the-scm-module 'variable)
             value))

       (unless (module-defined? the-scm-module 'variable)
         (export variable))))))


;;;
;;; File systems.
;;;

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
(define MS_RDONLY             1)
(define MS_NOSUID             2)
(define MS_NODEV              4)
(define MS_NOEXEC             8)
(define MS_REMOUNT           32)
(define MS_NOATIME         1024)
(define MS_BIND            4096)
(define MS_MOVE            8192)
(define MS_STRICTATIME 16777216)

(define MNT_FORCE       1)
(define MNT_DETACH      2)
(define MNT_EXPIRE      4)
(define UMOUNT_NOFOLLOW 8)

(define-as-needed (mount source target type
                         #:optional (flags 0) options
                         #:key (update-mtab? #f))
  "Mount device SOURCE on TARGET as a file system TYPE.
Optionally, FLAGS may be a bitwise-or of the MS_* <sys/mount.h>
constants, and OPTIONS may be a string.  When FLAGS contains
MS_REMOUNT, SOURCE and TYPE are ignored.  When UPDATE-MTAB? is true,
update /etc/mtab.  Raise a 'system-error' exception on error."
  ;; XXX: '#:update-mtab?' is not implemented by core 'mount'.
  (let ((proc (syscall->procedure int "mount" `(* * * ,unsigned-long *))))
    (let-values (((ret err)
                  (proc (if source
                            (string->pointer source)
                            %null-pointer)
                        (string->pointer target)
                        (if type
                            (string->pointer type)
                            %null-pointer)
                        flags
                        (if options
                            (string->pointer options)
                            %null-pointer))))
      (unless (zero? ret)
        (throw 'system-error "mount" "mount ~S on ~S: ~A"
               (list source target (strerror err))
               (list err)))
      (when update-mtab?
        (augment-mtab source target type options)))))

(define-as-needed (umount target
                          #:optional (flags 0)
                          #:key (update-mtab? #f))
  "Unmount TARGET.  Optionally FLAGS may be one of the MNT_* or UMOUNT_*
constants from <sys/mount.h>."
  ;; XXX: '#:update-mtab?' is not implemented by core 'umount'.
  (let ((proc (syscall->procedure int "umount2" `(* ,int))))
    (let-values (((ret err)
                  (proc (string->pointer target) flags)))
      (unless (zero? ret)
        (throw 'system-error "umount" "~S: ~A"
               (list target (strerror err))
               (list err)))
      (when update-mtab?
        (remove-from-mtab target)))))

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
  (let ((proc (syscall->procedure int "swapon" (list '* int))))
    (lambda* (device #:optional (flags 0))
      "Use the block special device at DEVICE for swapping."
      (let-values (((ret err)
                    (proc (string->pointer device) flags)))
        (unless (zero? ret)
          (throw 'system-error "swapon" "~S: ~A"
                 (list device (strerror err))
                 (list err)))))))

(define swapoff
  (let ((proc (syscall->procedure int "swapoff" '(*))))
    (lambda (device)
      "Stop using block special device DEVICE for swapping."
      (let-values (((ret err) (proc (string->pointer device))))
        (unless (zero? ret)
          (throw 'system-error "swapoff" "~S: ~A"
                 (list device (strerror err))
                 (list err)))))))

(define-as-needed RB_AUTOBOOT    #x01234567)
(define-as-needed RB_HALT_SYSTEM #xcdef0123)
(define-as-needed RB_ENABLED_CAD #x89abcdef)
(define-as-needed RB_DISABLE_CAD 0)
(define-as-needed RB_POWER_OFF   #x4321fedc)
(define-as-needed RB_SW_SUSPEND  #xd000fce2)
(define-as-needed RB_KEXEC       #x45584543)

(define-as-needed (reboot #:optional (cmd RB_AUTOBOOT))
  (let ((proc (syscall->procedure int "reboot" (list int))))
    (let-values (((ret err) (proc cmd)))
      (unless (zero? ret)
        (throw 'system-error "reboot" "~S: ~A"
               (list cmd (strerror err))
               (list err))))))

(define-as-needed (load-linux-module data #:optional (options ""))
  (let ((proc (syscall->procedure int "init_module"
                                  (list '* unsigned-long '*))))
    (let-values (((ret err)
                  (proc (bytevector->pointer data)
                        (bytevector-length data)
                        (string->pointer options))))
      (unless (zero? ret)
        (throw 'system-error "load-linux-module" "~A"
               (list (strerror err))
               (list err))))))

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

(define mkdtemp!
  (let ((proc (syscall->procedure '* "mkdtemp" '(*))))
    (lambda (tmpl)
      "Create a new unique directory in the file system using the template
string TMPL and return its file name.  TMPL must end with 'XXXXXX'."
      (let-values (((result err) (proc (string->pointer tmpl))))
        (when (null-pointer? result)
          (throw 'system-error "mkdtemp!" "~S: ~A"
                 (list tmpl (strerror err))
                 (list err)))
        (pointer->string result)))))

(define fdatasync
  (let ((proc (syscall->procedure int "fdatasync" (list int))))
    (lambda (port)
      "Flush buffered output of PORT, an output file port, and then call
fdatasync(2) on the underlying file descriptor."
      (force-output port)
      (let*-values (((fd)      (fileno port))
                    ((ret err) (proc fd)))
        (unless (zero? ret)
          (throw 'system-error "fdatasync" "~S: ~A"
                 (list fd (strerror err))
                 (list err)))))))


(define-record-type <file-system>
  (file-system type block-size blocks blocks-free
               blocks-available files free-files identifier
               name-length fragment-size mount-flags spare)
  file-system?
  (type              file-system-type)
  (block-size        file-system-block-size)
  (blocks            file-system-block-count)
  (blocks-free       file-system-blocks-free)
  (blocks-available  file-system-blocks-available)
  (files             file-system-file-count)
  (free-files        file-system-free-file-nodes)
  (identifier        file-system-identifier)
  (name-length       file-system-maximum-name-length)
  (fragment-size     file-system-fragment-size)
  (mount-flags       file-system-mount-flags)
  (spare             file-system--spare))

(define-syntax fsword                             ;fsword_t
  (identifier-syntax long))

(define-c-struct %statfs                          ;<bits/statfs.h>
  sizeof-statfs                                   ;slightly overestimated
  file-system
  read-statfs
  write-statfs!
  (type             fsword)
  (block-size       fsword)
  (blocks           uint64)
  (blocks-free      uint64)
  (blocks-available uint64)
  (files            uint64)
  (free-files       uint64)
  (identifier       (array int 2))
  (name-length      fsword)
  (fragment-size    fsword)
  (mount-flags      fsword)
  (spare            (array fsword 4)))

(define statfs
  (let ((proc (syscall->procedure int "statfs64" '(* *))))
    (lambda (file)
      "Return a <file-system> data structure describing the file system
mounted at FILE."
      (let*-values (((stat)    (make-bytevector sizeof-statfs))
                    ((ret err) (proc (string->pointer file)
                                     (bytevector->pointer stat))))
        (if (zero? ret)
            (read-statfs stat)
            (throw 'system-error "statfs" "~A: ~A"
                   (list file (strerror err))
                   (list err)))))))

(define (free-disk-space file)
  "Return the free disk space, in bytes, on the file system that hosts FILE."
  (let ((fs (statfs file)))
    (* (file-system-block-size fs)
       (file-system-blocks-available fs))))

;; Flags for the *at command, notably the 'utime' procedure of libguile.
;; From <fcntl.h>.
(define AT_FDCWD             -100)
(define AT_SYMLINK_NOFOLLOW #x100)
(define AT_REMOVEDIR        #x200)
(define AT_SYMLINK_FOLLOW   #x400)
(define AT_NO_AUTOMOUNT     #x800)
(define AT_EMPTY_PATH       #x1000)

(define-syntax BLKRRPART                         ;<sys/mount.h>
  (identifier-syntax #x125F))

(define* (device-in-use? device)
  "Return #t if the block DEVICE is in use, #f otherwise. This is inspired
from fdisk_device_is_used function of util-linux. This is particularly useful
for devices that do not appear in /proc/self/mounts like overlayfs lowerdir
backend device."
  (let*-values (((fd)      (open-fdes device O_RDONLY))
                ((ret err) (%ioctl fd BLKRRPART %null-pointer)))
    (close-fdes fd)
    (cond
     ((= ret 0)
      #f)
     ((= err EBUSY)
      #t)
     ((= err EINVAL)
      ;; We get EINVAL for devices that have the GENHD_FL_NO_PART_SCAN flag
      ;; set in the kernel, in particular loopback devices, though we do seem
      ;; to get it for SCSI storage (/dev/sr0) on QEMU.
      #f)
     (else
      (throw 'system-error "ioctl" "~A"
             (list (strerror err))
             (list err))))))


;;;
;;; Containers.
;;;

;; Linux clone flags, from linux/sched.h
(define CLONE_CHILD_CLEARTID #x00200000)
(define CLONE_CHILD_SETTID   #x01000000)
(define CLONE_NEWNS          #x00020000)
(define CLONE_NEWUTS         #x04000000)
(define CLONE_NEWIPC         #x08000000)
(define CLONE_NEWUSER        #x10000000)
(define CLONE_NEWPID         #x20000000)
(define CLONE_NEWNET         #x40000000)

(define %set-automatic-finalization-enabled?!
  ;; When using a statically-linked Guile, for instance in the initrd, we
  ;; cannot resolve this symbol, but most of the time we don't need it
  ;; anyway.  Thus, delay it.
  (let ((proc (delay
                (pointer->procedure int
                                    (dynamic-func
                                     "scm_set_automatic_finalization_enabled"
                                     (dynamic-link))
                                    (list int)))))
    (lambda (enabled?)
      "Switch on or off automatic finalization in a separate thread.
Turning finalization off shuts down the finalization thread as a side effect."
      (->bool ((force proc) (if enabled? 1 0))))))

(define-syntax-rule (without-automatic-finalization exp)
  "Turn off automatic finalization within the dynamic extent of EXP."
  (let ((enabled? #t))
    (dynamic-wind
      (lambda ()
        (set! enabled? (%set-automatic-finalization-enabled?! #f)))
      (lambda ()
        exp)
      (lambda ()
        (%set-automatic-finalization-enabled?! enabled?)))))

;; The libc interface to sys_clone is not useful for Scheme programs, so the
;; low-level system call is wrapped instead.  The 'syscall' function is
;; declared in <unistd.h> as a variadic function; in practice, it expects 6
;; pointer-sized arguments, as shown in, e.g., x86_64/syscall.S.
(define clone
  (let* ((proc (syscall->procedure int "syscall"
                                   (list long                   ;sysno
                                         unsigned-long          ;flags
                                         '* '* '*
                                         '*)))
         ;; TODO: Don't do this.
         (syscall-id (match (utsname:machine (uname))
                       ("i686"   120)
                       ("x86_64" 56)
                       ("mips64" 5055)
                       ("armv7l" 120)
                       ("aarch64" 220)
                       (_ #f))))
    (lambda (flags)
      "Create a new child process by duplicating the current parent process.
Unlike the fork system call, clone accepts FLAGS that specify which resources
are shared between the parent and child processes."
      (let-values (((ret err)
                    ;; Guile 2.2 runs a finalization thread.  'primitive-fork'
                    ;; takes care of shutting it down before forking, and we
                    ;; must do the same here.  Failing to do that, if the
                    ;; child process calls 'primitive-fork', it will hang
                    ;; while trying to pthread_join the finalization thread
                    ;; since that thread does not exist.
                    (without-automatic-finalization
                     (proc syscall-id flags
                           %null-pointer              ;child stack
                           %null-pointer %null-pointer ;ptid & ctid
                           %null-pointer))))           ;unused
        (if (= ret -1)
            (throw 'system-error "clone" "~d: ~A"
                   (list flags (strerror err))
                   (list err))
            ret)))))

(define setns
  ;; Some systems may be using an old (pre-2.14) version of glibc where there
  ;; is no 'setns' function available.
  (false-if-exception
   (let ((proc (syscall->procedure int "setns" (list int int))))
     (lambda (fdes nstype)
       "Reassociate the current process with the namespace specified by FDES, a
file descriptor obtained by opening a /proc/PID/ns/* file.  NSTYPE specifies
which type of namespace the current process may be reassociated with, or 0 if
there is no such limitation."
       (let-values (((ret err) (proc fdes nstype)))
         (unless (zero? ret)
           (throw 'system-error "setns" "~d ~d: ~A"
                  (list fdes nstype (strerror err))
                  (list err))))))))

(define pivot-root
  (let ((proc (syscall->procedure int "pivot_root" (list '* '*))))
    (lambda (new-root put-old)
      "Change the root file system to NEW-ROOT and move the current root file
system to PUT-OLD."
      (let-values (((ret err)
                    (proc (string->pointer new-root)
                          (string->pointer put-old))))
        (unless (zero? ret)
          (throw 'system-error "pivot_root" "~S ~S: ~A"
                 (list new-root put-old (strerror err))
                 (list err)))))))


;;;
;;; Opendir & co.
;;;

(define (file-type->symbol type)
  ;; Convert TYPE to symbols like 'stat:type' does.
  (cond ((= type DT_REG)  'regular)
        ((= type DT_LNK)  'symlink)
        ((= type DT_DIR)  'directory)
        ((= type DT_FIFO) 'fifo)
        ((= type DT_CHR)  'char-special)
        ((= type DT_BLK)  'block-special)
        ((= type DT_SOCK) 'socket)
        (else             'unknown)))

;; 'struct dirent64' for GNU/Linux.
(define-c-struct %struct-dirent-header/linux
  sizeof-dirent-header/linux
  (lambda (inode offset length type name)
    `((type . ,(file-type->symbol type))
      (inode . ,inode)))
  read-dirent-header/linux
  write-dirent-header!/linux
  (inode  int64)
  (offset int64)
  (length unsigned-short)
  (type   uint8)
  (name   uint8))                                 ;first byte of 'd_name'

;; 'struct dirent64' for GNU/Hurd.
(define-c-struct %struct-dirent-header/hurd
  sizeof-dirent-header/hurd
  (lambda (inode length type name-length name)
    `((type . ,(file-type->symbol type))
      (inode . ,inode)))
  read-dirent-header/hurd
  write-dirent-header!/hurd
  (inode   int64)
  (length  unsigned-short)
  (type    uint8)
  (namelen uint8)
  (name    uint8))

(define-syntax define-generic-identifier
  (syntax-rules (gnu/linux gnu/hurd =>)
    "Define a generic identifier that adjust to the current GNU variant."
    ((_ id (gnu/linux => linux) (gnu/hurd => hurd))
     (define-syntax id
       (lambda (s)
         (syntax-case s ()
           ((_ args (... ...))
            (if (string-contains (or (target-type) %host-type)
                                 "linux")
                #'(linux args (... ...))
                #'(hurd args (... ...))))
           (_
            (if (string-contains (or (target-type) %host-type)
                                 "linux")
                #'linux
                #'hurd))))))))

(define-generic-identifier read-dirent-header
  (gnu/linux => read-dirent-header/linux)
  (gnu/hurd  => read-dirent-header/hurd))

(define-generic-identifier %struct-dirent-header
  (gnu/linux => %struct-dirent-header/linux)
  (gnu/hurd  => %struct-dirent-header/hurd))

(define-generic-identifier sizeof-dirent-header
  (gnu/linux => sizeof-dirent-header/linux)
  (gnu/hurd  => sizeof-dirent-header/hurd))

;; Constants for the 'type' field, from <dirent.h>.
(define DT_UNKNOWN 0)
(define DT_FIFO 1)
(define DT_CHR 2)
(define DT_DIR 4)
(define DT_BLK 6)
(define DT_REG 8)
(define DT_LNK 10)
(define DT_SOCK 12)
(define DT_WHT 14)

(define string->pointer/utf-8
  (cut string->pointer <> "UTF-8"))

(define pointer->string/utf-8
  (cut pointer->string <> <> "UTF-8"))

(define opendir*
  (let ((proc (syscall->procedure '* "opendir" '(*))))
    (lambda* (name #:optional (string->pointer string->pointer/utf-8))
      (let-values (((ptr err)
                    (proc (string->pointer name))))
        (if (null-pointer? ptr)
            (throw 'system-error "opendir*"
                   "~A: ~A" (list name (strerror err))
                   (list err))
            ptr)))))

(define closedir*
  (let ((proc (syscall->procedure int "closedir" '(*))))
    (lambda (directory)
      (let-values (((ret err)
                    (proc directory)))
        (unless (zero? ret)
          (throw 'system-error "closedir"
                 "closedir: ~A" (list (strerror err))
                 (list err)))))))

(define readdir*
  (let ((proc (syscall->procedure '* "readdir64" '(*))))
    (lambda* (directory #:optional (pointer->string pointer->string/utf-8))
      (let ((ptr (proc directory)))
        (and (not (null-pointer? ptr))
             (cons (pointer->string
                    (make-pointer (+ (pointer-address ptr)
                                     (c-struct-field-offset
                                      %struct-dirent-header name)))
                    -1)
                   (read-dirent-header
                    (pointer->bytevector ptr sizeof-dirent-header))))))))

(define* (scandir* name #:optional
                   (select? (const #t))
                   (entry<? (lambda (entry1 entry2)
                              (match entry1
                                ((name1 . _)
                                 (match entry2
                                   ((name2 . _)
                                    (string<? name1 name2)))))))
                   #:key
                   (string->pointer string->pointer/utf-8)
                   (pointer->string pointer->string/utf-8))
  "This procedure improves on Guile's 'scandir' procedure in several ways:

   1. Systematically encode decode file names using STRING->POINTER and
      POINTER->STRING (UTF-8 by default; this works around a defect in Guile 2.0/2.2
      where 'scandir' decodes file names according to the current locale, which is
      not always desirable.

   2. Each entry that is returned has the form (NAME . PROPERTIES).
      PROPERTIES is an alist showing additional properties about the entry, as
      found in 'struct dirent'.  An entry may look like this:

        (\"foo.scm\" (type . regular) (inode . 123456))

      Callers must be prepared to deal with the case where 'type' is 'unknown'
      since some file systems do not provide that information.

   3. Raise to 'system-error' when NAME cannot be opened."
  (let ((directory (opendir* name string->pointer)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (let loop ((result '()))
          (match (readdir* directory pointer->string)
            (#f
             (sort result entry<?))
            (entry
             (loop (if (select? entry)
                       (cons entry result)
                       result))))))
      (lambda ()
        (closedir* directory)))))


;;;
;;; Advisory file locking.
;;;

(define-c-struct %struct-flock                    ;<fcntl.h>
  sizeof-flock
  list
  read-flock
  write-flock!
  (type   short)
  (whence short)
  (start  size_t)
  (length size_t)
  (pid    int))

(define F_SETLKW
  ;; On Linux-based systems, this is usually 7, but not always
  ;; (exceptions include SPARC.)  On GNU/Hurd, it's 9.
  (cond ((string-contains %host-type "sparc") 9)  ; sparc-*-linux-gnu
        ((string-contains %host-type "linux") 7)  ; *-linux-gnu
        (else 9)))                                ; *-gnu*

(define F_SETLK
  ;; Likewise: GNU/Hurd and SPARC use 8, while the others typically use 6.
  (cond ((string-contains %host-type "sparc") 8)  ; sparc-*-linux-gnu
        ((string-contains %host-type "linux") 6)  ; *-linux-gnu
        (else 8)))                                ; *-gnu*

(define F_xxLCK
  ;; The F_RDLCK, F_WRLCK, and F_UNLCK constants.
  (cond ((string-contains %host-type "sparc") #(1 2 3))    ; sparc-*-linux-gnu
        ((string-contains %host-type "hppa")  #(1 2 3))    ; hppa-*-linux-gnu
        ((string-contains %host-type "linux") #(0 1 2))    ; *-linux-gnu
        (else                                 #(1 2 3))))  ; *-gnu*

(define fcntl-flock
  (let ((proc (syscall->procedure int "fcntl" `(,int ,int *))))
    (lambda* (fd-or-port operation #:key (wait? #t))
      "Perform locking OPERATION on the file beneath FD-OR-PORT.  OPERATION
must be a symbol, one of 'read-lock, 'write-lock, or 'unlock.  When WAIT? is
true, block until the lock is acquired; otherwise, thrown an 'flock-error'
exception if it's already taken."
      (define (operation->int op)
        (case op
          ((read-lock)  (vector-ref F_xxLCK 0))
          ((write-lock) (vector-ref F_xxLCK 1))
          ((unlock)     (vector-ref F_xxLCK 2))
          (else         (error "invalid fcntl-flock operation" op))))

      (define fd
        (if (port? fd-or-port)
            (fileno fd-or-port)
            fd-or-port))

      (define bv
        (make-bytevector sizeof-flock))

      (write-flock! bv 0
                    (operation->int operation) SEEK_SET
                    0 0                           ;whole file
                    0)

      ;; XXX: 'fcntl' is a vararg function, but here we happily use the
      ;; standard ABI; crossing fingers.
      (let-values (((ret err)
                    (proc fd
                          (if wait?
                              F_SETLKW            ;lock & wait
                              F_SETLK)            ;non-blocking attempt
                          (bytevector->pointer bv))))
        (unless (zero? ret)
          ;; Presumably we got EAGAIN or so.
          (throw 'flock-error err))))))


;;;
;;; Miscellaneous, aka. 'prctl'.
;;;

(define %prctl
  ;; Should it win the API contest against 'ioctl'?  You tell us!
  (syscall->procedure int "prctl"
                      (list int unsigned-long unsigned-long
                            unsigned-long unsigned-long)))

(define PR_SET_NAME 15)                           ;<linux/prctl.h>
(define PR_GET_NAME 16)

(define %max-thread-name-length
  ;; Maximum length in bytes of the process name, including the terminating
  ;; zero.
  16)

(define (set-thread-name name)
  "Set the name of the calling thread to NAME.  NAME is truncated to 15
bytes."
  (let ((ptr (string->pointer name)))
    (let-values (((ret err)
                  (%prctl PR_SET_NAME
                          (pointer-address ptr) 0 0 0)))
      (unless (zero? ret)
        (throw 'set-process-name "set-process-name"
               "set-process-name: ~A"
               (list (strerror err))
               (list err))))))

(define (thread-name)
  "Return the name of the calling thread as a string."
  (let ((buf (make-bytevector %max-thread-name-length)))
    (let-values (((ret err)
                  (%prctl PR_GET_NAME
                          (pointer-address (bytevector->pointer buf))
                          0 0 0)))
      (if (zero? ret)
          (bytes->string (bytevector->u8-list buf))
          (throw 'process-name "process-name"
                 "process-name: ~A"
                 (list (strerror err))
                 (list err))))))


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
(define SIOCGIFNETMASK
  (if (string-contains %host-type "linux")
      #x891b                                      ;GNU/Linux
      -1))                                        ;FIXME: GNU/Hurd?
(define SIOCSIFNETMASK
  (if (string-contains %host-type "linux")
      #x891c                                      ;GNU/Linux
      -1))                                        ;FIXME: GNU/Hurd?
(define SIOCADDRT
  (if (string-contains %host-type "linux")
      #x890B                                      ;GNU/Linux
      -1))                                        ;FIXME: GNU/Hurd?
(define SIOCDELRT
  (if (string-contains %host-type "linux")
      #x890C                                      ;GNU/Linux
      -1))                                        ;FIXME: GNU/Hurd?

;; Flags and constants from <net/if.h>.

(define-as-needed IFF_UP #x1)                     ;Interface is up
(define-as-needed IFF_BROADCAST #x2)              ;Broadcast address valid.
(define-as-needed IFF_LOOPBACK #x8)               ;Is a loopback net.
(define-as-needed IFF_RUNNING #x40)               ;interface RFC2863 OPER_UP
(define-as-needed IFF_NOARP #x80)                 ;ARP disabled or unsupported

(define IF_NAMESIZE 16)                           ;maximum interface name size

(define-c-struct %ifconf-struct
  sizeof-ifconf
  list
  read-ifconf
  write-ifconf!
  (length  int)                                   ;int ifc_len
  (request '*))                                   ;struct ifreq *ifc_ifcu

(define ifreq-struct-size
  ;; 'struct ifreq' begins with an array of IF_NAMESIZE bytes containing the
  ;; interface name (nul-terminated), followed by a bunch of stuff.  This is
  ;; its size in bytes.
  (if (= 8 (sizeof '*))
      40
      32))

(define-c-struct sockaddr-in                      ;<linux/in.h>
  sizeof-sockaddrin
  (lambda (family port address)
    (make-socket-address family address port))
  read-sockaddr-in
  write-sockaddr-in!
  (family    unsigned-short)
  (port      (int16 ~ big))
  (address   (int32 ~ big)))

(define-c-struct sockaddr-in6                     ;<linux/in6.h>
  sizeof-sockaddr-in6
  (lambda (family port flowinfo address scopeid)
    (make-socket-address family address port flowinfo scopeid))
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

(define PF_PACKET 17)                             ;<bits/socket.h>
(define AF_PACKET PF_PACKET)

(define* (read-socket-address bv #:optional (index 0))
  "Read a socket address from bytevector BV at INDEX."
  (let ((family (bytevector-u16-native-ref bv index)))
    (cond ((= family AF_INET)
           (read-sockaddr-in bv index))
          ((= family AF_INET6)
           (read-sockaddr-in6 bv index))
          (else
           ;; XXX: Unsupported address family, such as AF_PACKET.  Return a
           ;; vector such that the vector can at least call 'sockaddr:fam'.
           (vector family)))))

(define %ioctl
  ;; The most terrible interface, live from Scheme.
  (syscall->procedure int "ioctl" (list int unsigned-long '*)))

(define (bytes->string bytes)
  "Read BYTES, a list of bytes, and return the null-terminated string decoded
from there, or #f if that would be an empty string."
  (match (take-while (negate zero?) bytes)
    (()
     #f)
    (non-zero
     (list->string (map integer->char non-zero)))))

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
             (cons (bytes->string bytes) result))))))

(define* (network-interface-names #:optional sock)
  "Return the names of existing network interfaces.  This is typically limited
to interfaces that are currently up."
  (let* ((close? (not sock))
         (sock   (or sock (socket SOCK_STREAM AF_INET 0)))
         (len    (* ifreq-struct-size 10))
         (reqs   (make-bytevector len))
         (conf   (make-bytevector sizeof-ifconf)))
    (write-ifconf! conf 0
                   len (bytevector->pointer reqs))

    (let-values (((ret err)
                  (%ioctl (fileno sock) SIOCGIFCONF
                          (bytevector->pointer conf))))
      (when close?
        (close-port sock))
      (if (zero? ret)
          (bytevector->string-list reqs ifreq-struct-size
                                   (match (read-ifconf conf)
                                     ((len . _) len)))
          (throw 'system-error "network-interface-list"
                 "network-interface-list: ~A"
                 (list (strerror err))
                 (list err))))))

(define %interface-line
  ;; Regexp matching an interface line in Linux's /proc/net/dev.
  (make-regexp "^[[:blank:]]*([[:graph:]]+):.*$"))

(define (all-network-interface-names)
  "Return all the names of the registered network interfaces, including those
that are not up."
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

(define-as-needed (network-interface-flags socket name)
  "Return a number that is the bit-wise or of 'IFF*' flags for network
interface NAME."
  (let ((req (make-bytevector ifreq-struct-size)))
    (bytevector-copy! (string->utf8 name) 0 req 0
                      (min (string-length name) (- IF_NAMESIZE 1)))
    (let-values (((ret err)
                  (%ioctl (fileno socket) SIOCGIFFLAGS
                          (bytevector->pointer req))))
      (if (zero? ret)

          ;; The 'ifr_flags' field is IF_NAMESIZE bytes after the
          ;; beginning of 'struct ifreq', and it's a short int.
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

(define (network-interface-running? name)
  "Return true if NAME designates a running network interface."
  (let* ((sock  (socket SOCK_STREAM AF_INET 0))
         (flags (network-interface-flags sock name)))
    (close-port sock)
    (not (zero? (logand flags IFF_RUNNING)))))

(define (arp-network-interface? name)
  "Return true if NAME supports the Address Resolution Protocol."
  (let* ((sock  (socket SOCK_STREAM AF_INET 0))
         (flags (network-interface-flags sock name)))
    (close-port sock)
    (zero? (logand flags IFF_NOARP))))

(define-as-needed (set-network-interface-flags socket name flags)
  "Set the flag of network interface NAME to FLAGS."
  (let ((req (make-bytevector ifreq-struct-size)))
    (bytevector-copy! (string->utf8 name) 0 req 0
                      (min (string-length name) (- IF_NAMESIZE 1)))
    ;; Set the 'ifr_flags' field.
    (bytevector-uint-set! req IF_NAMESIZE flags (native-endianness)
                          (sizeof short))
    (let-values (((ret err)
                  (%ioctl (fileno socket) SIOCSIFFLAGS
                          (bytevector->pointer req))))
      (unless (zero? ret)
        (throw 'system-error "set-network-interface-flags"
               "set-network-interface-flags on ~A: ~A"
               (list name (strerror err))
               (list err))))))

(define-as-needed (set-network-interface-address socket name sockaddr)
  "Set the address of network interface NAME to SOCKADDR."
  (let ((req (make-bytevector ifreq-struct-size)))
    (bytevector-copy! (string->utf8 name) 0 req 0
                      (min (string-length name) (- IF_NAMESIZE 1)))
    ;; Set the 'ifr_addr' field.
    (write-socket-address! sockaddr req IF_NAMESIZE)
    (let-values (((ret err)
                  (%ioctl (fileno socket) SIOCSIFADDR
                          (bytevector->pointer req))))
      (unless (zero? ret)
        (throw 'system-error "set-network-interface-address"
               "set-network-interface-address on ~A: ~A"
               (list name (strerror err))
               (list err))))))

(define (set-network-interface-netmask socket name sockaddr)
  "Set the network mask of interface NAME to SOCKADDR."
  (let ((req (make-bytevector ifreq-struct-size)))
    (bytevector-copy! (string->utf8 name) 0 req 0
                      (min (string-length name) (- IF_NAMESIZE 1)))
    ;; Set the 'ifr_addr' field.
    (write-socket-address! sockaddr req IF_NAMESIZE)
    (let-values (((ret err)
                  (%ioctl (fileno socket) SIOCSIFNETMASK
                          (bytevector->pointer req))))
      (unless (zero? ret)
        (throw 'system-error "set-network-interface-netmask"
               "set-network-interface-netmask on ~A: ~A"
               (list name (strerror err))
               (list err))))))

(define (network-interface-address socket name)
  "Return the address of network interface NAME.  The result is an object of
the same type as that returned by 'make-socket-address'."
  (let ((req (make-bytevector ifreq-struct-size)))
    (bytevector-copy! (string->utf8 name) 0 req 0
                      (min (string-length name) (- IF_NAMESIZE 1)))
    (let-values (((ret err)
                  (%ioctl (fileno socket) SIOCGIFADDR
                          (bytevector->pointer req))))
      (if (zero? ret)
          (read-socket-address req IF_NAMESIZE)
          (throw 'system-error "network-interface-address"
                 "network-interface-address on ~A: ~A"
                 (list name (strerror err))
                 (list err))))))

(define (network-interface-netmask socket name)
  "Return the netmask of network interface NAME.  The result is an object of
the same type as that returned by 'make-socket-address'."
  (let ((req (make-bytevector ifreq-struct-size)))
    (bytevector-copy! (string->utf8 name) 0 req 0
                      (min (string-length name) (- IF_NAMESIZE 1)))
    (let-values (((ret err)
                  (%ioctl (fileno socket) SIOCGIFNETMASK
                          (bytevector->pointer req))))
      (if (zero? ret)
          (read-socket-address req IF_NAMESIZE)
          (throw 'system-error "network-interface-netmask"
                 "network-interface-netmask on ~A: ~A"
                 (list name (strerror err))
                 (list err))))))

(define* (configure-network-interface name sockaddr flags
                                      #:key netmask)
  "Configure network interface NAME to use SOCKADDR, an address as returned by
'make-socket-address', and FLAGS, a bitwise-or of IFF_* constants.  If NETMASK
is true, it must be a socket address to use as the network mask."
  (let ((sock (socket (sockaddr:fam sockaddr) SOCK_STREAM 0)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (set-network-interface-address sock name sockaddr)
        (set-network-interface-flags sock name flags)
        (when netmask
          (set-network-interface-netmask sock name netmask)))
      (lambda ()
        (close-port sock)))))

(define* (set-network-interface-up name
                                   #:key (family AF_INET))
  "Turn up the interface NAME."
  (let ((sock (socket family SOCK_STREAM 0)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (let ((flags (network-interface-flags sock name)))
          (set-network-interface-flags sock name
                                       (logior flags IFF_UP))))
      (lambda ()
        (close-port sock)))))


;;;
;;; Network routes.
;;;

(define-c-struct %rtentry                 ;'struct rtentry' from <net/route.h>
  sizeof-rtentry
  list
  read-rtentry
  write-rtentry!
  (pad1            unsigned-long)
  (destination     (array uint8 16))              ;struct sockaddr
  (gateway         (array uint8 16))              ;struct sockaddr
  (genmask         (array uint8 16))              ;struct sockaddr
  (flags           unsigned-short)
  (pad2            short)
  (pad3            long)
  (tos             uint8)
  (class           uint8)
  (pad4            (array uint8 (if (= 8 (sizeof* '*)) 3 1)))
  (metric          short)
  (device          '*)
  (mtu             unsigned-long)
  (window          unsigned-long)
  (initial-rtt     unsigned-short))

(define RTF_UP #x0001)                     ;'rtentry' flags from <net/route.h>
(define RTF_GATEWAY #x0002)

(define %sockaddr-any
  (make-socket-address AF_INET INADDR_ANY 0))

(define add-network-route/gateway
  ;; To allow field names to be matched as literals, we need to move them out
  ;; of the lambda's body since the parameters have the same name.  A lot of
  ;; fuss for very little.
  (let-syntax ((gateway-offset (identifier-syntax
                                (c-struct-field-offset %rtentry gateway)))
               (destination-offset (identifier-syntax
                                    (c-struct-field-offset %rtentry destination)))
               (genmask-offset (identifier-syntax
                                (c-struct-field-offset %rtentry genmask))))
    (lambda* (socket gateway
                     #:key (destination %sockaddr-any) (genmask %sockaddr-any))
      "Add a network route for DESTINATION (a socket address as returned by
'make-socket-address') that goes through GATEWAY (a socket address).  For
instance, the call:

  (add-network-route/gateway sock
                             (make-socket-address
                               AF_INET
                               (inet-pton AF_INET \"192.168.0.1\")
                               0))

is equivalent to this 'net-tools' command:

  route add -net default gw 192.168.0.1

because the default value of DESTINATION is \"0.0.0.0\"."
      (let ((route (make-bytevector sizeof-rtentry 0)))
        (write-socket-address! gateway route gateway-offset)
        (write-socket-address! destination route destination-offset)
        (write-socket-address! genmask route genmask-offset)
        (bytevector-u16-native-set! route
                                    (c-struct-field-offset %rtentry flags)
                                    (logior RTF_UP RTF_GATEWAY))
        (let-values (((ret err)
                      (%ioctl (fileno socket) SIOCADDRT
                              (bytevector->pointer route))))
          (unless (zero? ret)
            (throw 'system-error "add-network-route/gateway"
                   "add-network-route/gateway: ~A"
                   (list (strerror err))
                   (list err))))))))

(define delete-network-route
  (let-syntax ((destination-offset (identifier-syntax
                                    (c-struct-field-offset %rtentry destination))))
    (lambda* (socket destination)
      "Delete the network route for DESTINATION.  For instance, the call:

  (delete-network-route sock
                        (make-socket-address AF_INET INADDR_ANY 0))

is equivalent to the 'net-tools' command:

  route del -net default
"

      (let ((route (make-bytevector sizeof-rtentry 0)))
        (write-socket-address! destination route destination-offset)
        (let-values (((ret err)
                      (%ioctl (fileno socket) SIOCDELRT
                              (bytevector->pointer route))))
          (unless (zero? ret)
            (throw 'system-error "delete-network-route"
                   "delete-network-route: ~A"
                   (list (strerror err))
                   (list err))))))))


;;;
;;; Details about network interfaces---aka. 'getifaddrs'.
;;;

;; Network interfaces.  XXX: We would call it <network-interface> but that
;; would collide with the ioctl wrappers above.
(define-record-type <interface>
  (make-interface name flags address netmask broadcast-address)
  interface?
  (name              interface-name)               ;string
  (flags             interface-flags)              ;or'd IFF_* values
  (address           interface-address)            ;sockaddr | #f
  (netmask           interface-netmask)            ;sockaddr | #f
  (broadcast-address interface-broadcast-address)) ;sockaddr | #f

(define (write-interface interface port)
  (match interface
    (($ <interface> name flags address)
     (format port "#<interface ~s " name)
     (unless (zero? (logand IFF_UP flags))
       (display "up " port))

     ;; Check whether ADDRESS really is a sockaddr.
     (when address
       (if (member (sockaddr:fam address) (list AF_INET AF_INET6))
           (format port "~a " (inet-ntop (sockaddr:fam address)
                                         (sockaddr:addr address)))
           (format port "family:~a " (sockaddr:fam address))))

     (format port "~a>" (number->string (object-address interface) 16)))))

(set-record-type-printer! <interface> write-interface)

(define (values->interface next name flags address netmask
                           broadcast-address data)
  "Given the raw field values passed as arguments, return a pair whose car is
an <interface> object, and whose cdr is the pointer NEXT."
  (define (maybe-socket-address pointer)
    (if (null-pointer? pointer)
        #f
        (read-socket-address (pointer->bytevector pointer 50)))) ;XXX: size

  (cons (make-interface (if (null-pointer? name)
                            #f
                            (pointer->string name))
                        flags
                        (maybe-socket-address address)
                        (maybe-socket-address netmask)
                        (maybe-socket-address broadcast-address)
                        ;; Ignore DATA.
                        )
        next))

(define-c-struct ifaddrs                          ;<ifaddrs.h>
  %sizeof-ifaddrs
  values->interface
  read-ifaddrs
  write-ifaddrs!
  (next          '*)
  (name          '*)
  (flags         unsigned-int)
  (addr          '*)
  (netmask       '*)
  (broadcastaddr '*)
  (data          '*))

(define (unfold-interface-list ptr)
  "Call 'read-ifaddrs' on PTR and all its 'next' fields, recursively, and
return the list of resulting <interface> objects."
  (let loop ((ptr    ptr)
             (result '()))
    (if (null-pointer? ptr)
        (reverse result)
        (match (read-ifaddrs (pointer->bytevector ptr %sizeof-ifaddrs))
          ((ifaddr . ptr)
           (loop ptr (cons ifaddr result)))))))

(define network-interfaces
  (let ((proc (syscall->procedure int "getifaddrs" (list '*))))
    (lambda ()
      "Return a list of <interface> objects, each denoting a configured
network interface.  This is implemented using the 'getifaddrs' libc function."
      (let*-values (((ptr)
                     (bytevector->pointer (make-bytevector (sizeof* '*))))
                    ((ret err)
                     (proc ptr)))
        (if (zero? ret)
            (let* ((ptr    (dereference-pointer ptr))
                   (result (unfold-interface-list ptr)))
              (free-ifaddrs ptr)
              result)
            (throw 'system-error "network-interfaces" "~A"
                   (list (strerror err))
                   (list err)))))))

(define free-ifaddrs
  (syscall->procedure void "freeifaddrs" '(*)))


;;;
;;; Terminals.
;;;

(define-syntax bits->symbols-body
  (syntax-rules ()
    ((_ bits () ())
     '())
    ((_ bits (name names ...) (value values ...))
     (let ((result (bits->symbols-body bits (names ...) (values ...))))
       (if (zero? (logand bits value))
           result
           (cons 'name result))))))

(define-syntax define-bits
  (syntax-rules (define)
    "Define the given numerical constants under CONSTRUCTOR, such that
 (CONSTRUCTOR NAME) returns VALUE.  Define BITS->SYMBOLS as a procedure that,
given an integer, returns the list of names of the constants that are or'd."
    ((_ constructor bits->symbols (define names values) ...)
     (begin
       (define-syntax constructor
         (syntax-rules (names ...)
           ((_) 0)
           ((_ names) values) ...
           ((_ first rest (... ...))
            (logior (constructor first) rest (... ...)))))
       (define (bits->symbols bits)
         (bits->symbols-body bits (names ...) (values ...)))))))

;; 'local-flags' bits from <bits/termios.h>
(define-bits local-flags
  local-flags->symbols
 (define ISIG #o0000001)
 (define ICANON #o0000002)
 (define XCASE #o0000004)
 (define ECHO #o0000010)
 (define ECHOE #o0000020)
 (define ECHOK #o0000040)
 (define ECHONL #o0000100)
 (define NOFLSH #o0000200)
 (define TOSTOP #o0000400)
 (define ECHOCTL #o0001000)
 (define ECHOPRT #o0002000)
 (define ECHOKE #o0004000)
 (define FLUSHO #o0010000)
 (define PENDIN #o0040000)
 (define IEXTEN #o0100000)
 (define EXTPROC #o0200000))

(define-bits input-flags
  input-flags->symbols
  (define IGNBRK #o0000001)
  (define BRKINT #o0000002)
  (define IGNPAR #o0000004)
  (define PARMRK #o0000010)
  (define INPCK #o0000020)
  (define ISTRIP #o0000040)
  (define INLCR #o0000100)
  (define IGNCR #o0000200)
  (define ICRNL #o0000400)
  (define IUCLC #o0001000)
  (define IXON #o0002000)
  (define IXANY #o0004000)
  (define IXOFF #o0010000)
  (define IMAXBEL #o0020000)
  (define IUTF8 #o0040000))

;; "Actions" values for 'tcsetattr'.
(define-bits tcsetattr-action
  %unused-tcsetattr-action->symbols
  (define TCSANOW  0)
  (define TCSADRAIN 1)
  (define TCSAFLUSH 2))

(define-record-type <termios>
  (termios input-flags output-flags control-flags local-flags
           line-discipline control-chars
           input-speed output-speed)
  termios?
  (input-flags      termios-input-flags)
  (output-flags     termios-output-flags)
  (control-flags    termios-control-flags)
  (local-flags      termios-local-flags)
  (line-discipline  termios-line-discipline)
  (control-chars    termios-control-chars)
  (input-speed      termios-input-speed)
  (output-speed     termios-output-speed))

(define-c-struct %termios                         ;<bits/termios.h>
  sizeof-termios
  termios
  read-termios
  write-termios!
  (input-flags      unsigned-int)
  (output-flags     unsigned-int)
  (control-flags    unsigned-int)
  (local-flags      unsigned-int)
  (line-discipline  uint8)
  (control-chars    (array uint8 32))
  (input-speed      unsigned-int)
  (output-speed     unsigned-int))

(define tcgetattr
  (let ((proc (syscall->procedure int "tcgetattr" (list int '*))))
    (lambda (fd)
      "Return the <termios> structure for the tty at FD."
      (let*-values (((bv)      (make-bytevector sizeof-termios))
                    ((ret err) (proc fd (bytevector->pointer bv))))
        (if (zero? ret)
            (read-termios bv)
            (throw 'system-error "tcgetattr" "~A"
                   (list (strerror err))
                   (list err)))))))

(define tcsetattr
  (let ((proc (syscall->procedure int "tcsetattr" (list int int '*))))
    (lambda (fd actions termios)
      "Use TERMIOS for the tty at FD.  ACTIONS is one of of the values
produced by 'tcsetattr-action'; see tcsetattr(3) for details."
      (define bv
        (make-bytevector sizeof-termios))

      (let-syntax ((match/write (syntax-rules ()
                                  ((_ fields ...)
                                   (match termios
                                     (($ <termios> fields ...)
                                      (write-termios! bv 0 fields ...)))))))
        (match/write input-flags output-flags control-flags local-flags
                     line-discipline control-chars input-speed output-speed))

      (let-values (((ret err) (proc fd actions (bytevector->pointer bv))))
        (unless (zero? ret)
          (throw 'system-error "tcgetattr" "~A"
                 (list (strerror err))
                 (list err)))))))

(define-syntax TIOCGWINSZ                         ;<asm-generic/ioctls.h>
  (identifier-syntax #x5413))

(define-record-type <window-size>
  (window-size rows columns x-pixels y-pixels)
  window-size?
  (rows     window-size-rows)
  (columns  window-size-columns)
  (x-pixels window-size-x-pixels)
  (y-pixels window-size-y-pixels))

(define-c-struct winsize                          ;<bits/ioctl-types.h>
  sizeof-winsize
  window-size
  read-winsize
  write-winsize!
  (rows          unsigned-short)
  (columns       unsigned-short)
  (x-pixels      unsigned-short)
  (y-pixels      unsigned-short))

(define* (terminal-window-size #:optional (port (current-output-port)))
  "Return a <window-size> structure describing the terminal at PORT, or raise
a 'system-error' if PORT is not backed by a terminal.  This procedure
corresponds to the TIOCGWINSZ ioctl."
  (let*-values (((size)    (make-bytevector sizeof-winsize))
                ((ret err) (%ioctl (fileno port) TIOCGWINSZ
                                   (bytevector->pointer size))))
    (if (zero? ret)
        (read-winsize size)
        (throw 'system-error "terminal-window-size" "~A"
               (list (strerror err))
               (list err)))))

(define* (terminal-columns #:optional (port (current-output-port)))
  "Return the best approximation of the number of columns of the terminal at
PORT, trying to guess a reasonable value if all else fails.  The result is
always a positive integer."
  (define (fall-back)
    (match (and=> (getenv "COLUMNS") string->number)
      (#f 80)
      ((? number? columns)
       (if (> columns 0) columns 80))))

  (catch 'system-error
    (lambda ()
      (if (file-port? port)
          (match (window-size-columns (terminal-window-size port))
            ;; Things like Emacs shell-mode return 0, which is unreasonable.
            (0 (fall-back))
            ((? number? columns) columns))
          (fall-back)))
    (lambda args
      (let ((errno (system-error-errno args)))
        ;; ENOTTY is what we're after but 2012-and-earlier Linux versions
        ;; would return EINVAL instead in some cases:
        ;; <https://bugs.ruby-lang.org/issues/10494>.
        ;; Furthermore, some FUSE file systems like unionfs return ENOSYS for
        ;; that ioctl.
        (if (memv errno (list ENOTTY EINVAL ENOSYS))
            (fall-back)
            (apply throw args))))))


;;;
;;; utmpx.
;;;

(define-record-type <utmpx-entry>
  (utmpx type pid line id user host termination exit
         session time address)
  utmpx?
  (type           utmpx-login-type)               ;login-type
  (pid            utmpx-pid)
  (line           utmpx-line)                     ;device name
  (id             utmpx-id)
  (user           utmpx-user)                     ;user name
  (host           utmpx-host)                     ;host name | #f
  (termination    utmpx-termination-status)
  (exit           utmpx-exit-status)
  (session        utmpx-session-id)               ;session ID, for windowing
  (time           utmpx-time)                     ;entry time
  (address        utmpx-address))

(define-c-struct %utmpx                           ;<utmpx.h>
  sizeof-utmpx
  (lambda (type pid line id user host termination exit session
                seconds useconds address %reserved)
    (utmpx type pid
           (bytes->string line) id
           (bytes->string user)
           (bytes->string host) termination exit
           session
           (make-time time-utc (* 1000 useconds) seconds)
           address))
  read-utmpx
  write-utmpx!
  (type           short)
  (pid            int)
  (line           (array uint8 32))
  (id             (array uint8 4))
  (user           (array uint8 32))
  (host           (array uint8 256))
  (termination    short)
  (exit           short)
  (session        int32)
  (time-seconds   int32)
  (time-useconds  int32)
  (address-v6     (array int32 4))
  (%reserved      (array uint8 20)))

(define-bits login-type
  %unused-login-type->symbols
  (define EMPTY 0)                      ;No valid user accounting information.
  (define RUN_LVL 1)                    ;The system's runlevel.
  (define BOOT_TIME 2)                  ;Time of system boot.
  (define NEW_TIME 3)                   ;Time after system clock changed.
  (define OLD_TIME 4)                   ;Time when system clock changed.

  (define INIT_PROCESS 5)                ;Process spawned by the init process.
  (define LOGIN_PROCESS 6)               ;Session leader of a logged in user.
  (define USER_PROCESS 7)                ;Normal process.
  (define DEAD_PROCESS 8)                ;Terminated process.

  (define ACCOUNTING 9))                 ;System accounting.

(define setutxent
  (let ((proc (syscall->procedure void "setutxent" '())))
    (lambda ()
      "Open the user accounting database."
      (proc))))

(define endutxent
  (let ((proc (syscall->procedure void "endutxent" '())))
    (lambda ()
      "Close the user accounting database."
      (proc))))

(define getutxent
  (let ((proc (syscall->procedure '* "getutxent" '())))
    (lambda ()
      "Return the next entry from the user accounting database."
      (let ((ptr (proc)))
        (if (null-pointer? ptr)
            #f
            (read-utmpx (pointer->bytevector ptr sizeof-utmpx)))))))

(define (utmpx-entries)
  "Return the list of entries read from the user accounting database."
  (setutxent)
  (let loop ((entries '()))
    (match (getutxent)
      (#f
       (endutxent)
       (reverse entries))
      ((? utmpx? entry)
       (loop (cons entry entries))))))

(define (read-utmpx-from-port port)
  "Read a utmpx entry from PORT.  Return either the EOF object or a utmpx
entry."
  (match (get-bytevector-n port sizeof-utmpx)
    ((? eof-object? eof)
     eof)
    ((? bytevector? bv)
     (read-utmpx bv))))

;;; syscalls.scm ends here
