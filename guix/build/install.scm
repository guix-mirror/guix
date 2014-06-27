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

(define-module (guix build install)
  #:use-module (guix build utils)
  #:use-module (guix build install)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (install-grub
            populate-root-file-system
            reset-timestamps
            register-closure))

;;; Commentary:
;;;
;;; This module supports the installation of the GNU system on a hard disk.
;;; It is meant to be used both in a build environment (in derivations that
;;; build VM images), and on the bare metal (when really installing the
;;; system.)
;;;
;;; Code:

(define* (install-grub grub.cfg device mount-point)
  "Install GRUB with GRUB.CFG on DEVICE, which is assumed to be mounted on
MOUNT-POINT."
  (let* ((target (string-append mount-point "/boot/grub/grub.cfg"))
         (pivot  (string-append target ".new")))
    (mkdir-p (dirname target))

    ;; Copy GRUB.CFG instead of just symlinking it since it's not a GC root.
    ;; Do that atomically.
    (copy-file grub.cfg pivot)
    (rename-file pivot target)

    (unless (zero? (system* "grub-install" "--no-floppy"
                            "--boot-directory"
                            (string-append mount-point "/boot")
                            device))
      (error "failed to install GRUB"))))

(define (evaluate-populate-directive directive target)
  "Evaluate DIRECTIVE, an sexp describing a file or directory to create under
directory TARGET."
  (let loop ((directive directive))
    (match directive
      (('directory name)
       (mkdir-p (string-append target name)))
      (('directory name uid gid)
       (let ((dir (string-append target name)))
         (mkdir-p dir)
         (chown dir uid gid)))
      (('directory name uid gid mode)
       (loop `(directory ,name ,uid ,gid))
       (chmod (string-append target name) mode))
      ((new '-> old)
       (symlink old (string-append target new))))))

(define (directives store)
  "Return a list of directives to populate the root file system that will host
STORE."
  `(;; Note: the store's GID is fixed precisely so we can set it here rather
    ;; than at activation time.
    (directory ,store 0 30000)

    (directory "/etc")
    (directory "/var/log")                          ; for dmd
    (directory "/var/guix/gcroots")
    (directory "/var/empty")                        ; for no-login accounts
    (directory "/var/run")
    (directory "/run")
    (directory "/var/guix/profiles/per-user/root" 0 0)

    ;; Link to the initial system generation.
    ("/var/guix/profiles/system" -> "system-1-link")

    ("/var/guix/gcroots/booted-system" -> "/run/booted-system")
    ("/var/guix/gcroots/current-system" -> "/run/current-system")

    (directory "/bin")
    ("/bin/sh" -> "/run/current-system/profile/bin/bash")
    (directory "/tmp" 0 0 #o1777)                 ; sticky bit

    (directory "/root" 0 0)                       ; an exception
    (directory "/home" 0 0)))

(define (populate-root-file-system system target)
  "Make the essential non-store files and directories on TARGET.  This
includes /etc, /var, /run, /bin/sh, etc., and all the symlinks to SYSTEM."
  (for-each (cut evaluate-populate-directive <> target)
            (directives (%store-directory)))

  ;; Add system generation 1.
  (symlink system
           (string-append target "/var/guix/profiles/system-1-link")))

(define (reset-timestamps directory)
  "Reset the timestamps of all the files under DIRECTORY, so that they appear
as created and modified at the Epoch."
  (display "clearing file timestamps...\n")
  (for-each (lambda (file)
              (let ((s (lstat file)))
                ;; XXX: Guile uses libc's 'utime' function (not 'futime'), so
                ;; the timestamp of symlinks cannot be changed, and there are
                ;; symlinks here pointing to /gnu/store, which is the host,
                ;; read-only store.
                (unless (eq? (stat:type s) 'symlink)
                  (utime file 0 0 0 0))))
            (find-files directory "")))

(define (register-closure store closure)
  "Register CLOSURE in STORE, where STORE is the directory name of the target
store and CLOSURE is the name of a file containing a reference graph as used
by 'guix-register'.  As a side effect, this resets timestamps on store files."
  (let ((status (system* "guix-register" "--prefix" store
                         closure)))
    (unless (zero? status)
      (error "failed to register store items" closure))))

;;; install.scm ends here
