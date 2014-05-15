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
  #:use-module (ice-9 match)
  #:export (install-grub
            evaluate-populate-directive
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
MOUNT-POINT.  Return #t on success."
  (mkdir-p (string-append mount-point "/boot/grub"))
  (symlink grub.cfg (string-append mount-point "/boot/grub/grub.cfg"))
  (zero? (system* "grub-install" "--no-floppy"
                  "--boot-directory" (string-append mount-point "/boot")
                  device)))

(define (evaluate-populate-directive directive target)
  "Evaluate DIRECTIVE, an sexp describing a file or directory to create under
directory TARGET."
  (match directive
    (('directory name)
     (mkdir-p (string-append target name)))
    (('directory name uid gid)
     (let ((dir (string-append target name)))
       (mkdir-p dir)
       (chown dir uid gid)))
    ((new '-> old)
     (symlink old (string-append target new)))))

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
by 'guix-register'."
  (let ((status (system* "guix-register" "--prefix" store
                         closure)))
    (unless (zero? status)
      (error "failed to register store items" closure))))

;;; install.scm ends here
