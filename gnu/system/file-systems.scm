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

(define-module (gnu system file-systems)
  #:use-module (guix records)
  #:export (<file-system>
            file-system
            file-system?
            file-system-device
            file-system-title
            file-system-mount-point
            file-system-type
            file-system-needed-for-boot?
            file-system-flags
            file-system-options
            file-system-check?
            file-system-create-mount-point?

            %fuse-control-file-system
            %binary-format-file-system
            %shared-memory-file-system
            %pseudo-terminal-file-system
            %devtmpfs-file-system

            %base-file-systems))

;;; Commentary:
;;;
;;; Declaring file systems to be mounted.
;;;
;;; Code:

;; File system declaration.
(define-record-type* <file-system> file-system
  make-file-system
  file-system?
  (device           file-system-device)           ; string
  (title            file-system-title             ; 'device | 'label | 'uuid
                    (default 'device))
  (mount-point      file-system-mount-point)      ; string
  (type             file-system-type)             ; string
  (flags            file-system-flags             ; list of symbols
                    (default '()))
  (options          file-system-options           ; string or #f
                    (default #f))
  (needed-for-boot? file-system-needed-for-boot?  ; Boolean
                    (default #f))
  (check?           file-system-check?            ; Boolean
                    (default #t))
  (create-mount-point? file-system-create-mount-point? ; Boolean
                       (default #f)))

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

(define %devtmpfs-file-system
  ;; /dev as a 'devtmpfs' file system, needed for udev.
  (file-system
    (device "none")
    (mount-point "/dev")
    (type "devtmpfs")
    (check? #f)

    ;; Mount it from the initrd so /dev/pts & co. can then be mounted over it.
    (needed-for-boot? #t)))

(define %tty-gid
  ;; ID of the 'tty' group.  Allocate it statically to make it easy to refer
  ;; to it from here and from the 'tty' group definitions.
  1004)

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

(define %base-file-systems
  ;; List of basic file systems to be mounted.  Note that /proc and /sys are
  ;; currently mounted by the initrd.
  (list %devtmpfs-file-system
        %pseudo-terminal-file-system
        %shared-memory-file-system))

;;; file-systems.scm ends here
