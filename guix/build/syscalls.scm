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
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:export (errno
            MS_RDONLY
            MS_REMOUNT
            MS_BIND
            MS_MOVE
            mount
            umount
            processes))

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
            (call-with-input-file "/etc/fstab" read-mtab)))

  (call-with-output-file "/etc/fstab"
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

;;; syscalls.scm ends here
