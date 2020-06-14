;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (test-file-systems)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:use-module (gnu system file-systems)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

;; Test the (gnu system file-systems) module.

(test-begin "file-systems")

(test-assert "file-system-needed-for-boot?"
  (let-syntax ((dummy-fs (syntax-rules ()
                           ((_ directory)
                            (file-system
                              (device "foo")
                              (mount-point directory)
                              (type "ext4"))))))
    (parameterize ((%store-prefix "/gnu/guix/store"))
      (and (file-system-needed-for-boot? (dummy-fs "/"))
           (file-system-needed-for-boot? (dummy-fs "/gnu"))
           (file-system-needed-for-boot? (dummy-fs "/gnu/guix"))
           (file-system-needed-for-boot? (dummy-fs "/gnu/guix/store"))
           (not (file-system-needed-for-boot?
                 (dummy-fs "/gnu/guix/store/foo")))
           (not (file-system-needed-for-boot? (dummy-fs "/gn")))
           (not (file-system-needed-for-boot?
                 (file-system
                   (inherit (dummy-fs (%store-prefix)))
                   (device "/foo")
                   (flags '(bind-mount read-only)))))))))

(test-assert "does not pull (guix config)"
  ;; This module is meant both for the host side and "build side", so make
  ;; sure it doesn't pull in (guix config), which depends on the user's
  ;; config.
  (not (member '(guix config)
               (source-module-closure '((gnu system file-systems))))))

(test-equal "does not pull (gnu packages …)"
  ;; Same story: (gnu packages …) should not be pulled.
  #f
  (find (match-lambda
          (('gnu 'packages _ ..1) #t)
          (_ #f))
        (source-module-closure '((gnu system file-systems)))))

(test-equal "file-system-options->alist"
  '("autodefrag" ("subvol" . "home") ("compress" . "lzo"))
  (file-system-options->alist "autodefrag,subvol=home,compress=lzo"))

(test-equal "file-system-options->alist (#f)"
  '()
  (file-system-options->alist #f))

(test-equal "alist->file-system-options"
  "autodefrag,subvol=root,compress=lzo"
  (alist->file-system-options '("autodefrag"
                                ("subvol" . "root")
                                ("compress" . "lzo"))))

(test-equal "alist->file-system-options (null)"
  #f
  (alist->file-system-options '()))


;;;
;;; Btrfs related.
;;;

(define %btrfs-root-subvolume
  (file-system
    (device (file-system-label "btrfs-pool"))
    (mount-point "/")
    (type "btrfs")
    (options "subvol=rootfs,compress=zstd")))

(define %btrfs-store-subvolid
  (file-system
    (device (file-system-label "btrfs-pool"))
    (mount-point "/gnu/store")
    (type "btrfs")
    (options "subvolid=10,compress=zstd")
    (dependencies (list %btrfs-root-subvolume))))

(define %btrfs-store-subvolume
  (file-system
    (device (file-system-label "btrfs-pool"))
    (mount-point "/gnu/store")
    (type "btrfs")
    (options "subvol=/some/nested/file/name")
    (dependencies (list %btrfs-root-subvolume))))

(test-assert "btrfs-subvolume? (subvol)"
  (btrfs-subvolume? %btrfs-root-subvolume))

(test-assert "btrfs-subvolume? (subvolid)"
  (btrfs-subvolume? %btrfs-store-subvolid))

(test-equal "btrfs-store-subvolume-file-name"
  "/some/nested/file/name"
  (parameterize ((%store-prefix "/gnu/store"))
    (btrfs-store-subvolume-file-name (list %btrfs-root-subvolume
                                           %btrfs-store-subvolume))))

(test-error "btrfs-store-subvolume-file-name (subvolid)"
            (parameterize ((%store-prefix "/gnu/store"))
              (btrfs-store-subvolume-file-name (list %btrfs-root-subvolume
                                                     %btrfs-store-subvolid))))

(test-end)
