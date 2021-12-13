;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; This file returns a manifest containing release-critical bit, for all the
;;; supported architectures and cross-compilation targets.

(use-modules (gnu packages)
             (guix packages)
             (guix profiles)
             ((gnu ci) #:select (%cross-targets))
             ((gnu services xorg) #:select (%default-xorg-modules))
             (guix utils)
             (srfi srfi-1)
             (srfi srfi-26))

(define* (package->manifest-entry* package system
                                   #:key target)
  "Return a manifest entry for PACKAGE on SYSTEM, optionally cross-compiled to
TARGET."
  (manifest-entry
    (inherit (package->manifest-entry package))
    (name (string-append (package-name package) "." system
                         (if target
                             (string-append "." target)
                             "'")))
    (item (with-parameters ((%current-system system)
                            (%current-target-system target))
            package))))

(define %base-packages
  ;; Packages that must be substitutable on all the platforms Guix supports.
  (map specification->package
       '("bootstrap-tarballs" "gcc-toolchain" "nss-certs"
         "openssh" "emacs" "vim" "python" "guile" "guix")))

(define %base-packages/armhf
  ;; XXX: Relax requirements for armhf-linux for lack of enough build power.
  (map (lambda (package)
         (if (string=? (package-name package) "emacs")
             (specification->package "emacs-no-x")
             package))
       %base-packages))

(define %base-packages/hurd
  ;; XXX: For now we are less demanding of "i586-gnu".
  (map specification->package
       '("coreutils" "grep" "findutils" "gawk" "make"
         "gcc-toolchain" "tar" "xz")))

(define %system-packages
  ;; Key packages proposed by the Guix System installer.
  (append (map specification->package
               '("xorg-server" "xfce" "gnome" "mate" "enlightenment"
                 "openbox" "awesome" "i3-wm" "ratpoison"
                 "emacs" "emacs-exwm" "emacs-desktop-environment"
                 "xlockmore" "slock" "libreoffice"
                 "connman" "network-manager" "network-manager-applet"
                 "openssh" "ntp" "tor"
                 "linux-libre" "grub-hybrid"
                 ;; FIXME: Add IceCat when Rust is available on i686.
                 ;;"icecat"
                 ))
          %default-xorg-modules))

(define %packages-to-cross-build
  ;; Packages that must be cross-buildable from x86_64-linux.
  ;; FIXME: Add (@ (gnu packages gcc) gcc) when <https://bugs.gnu.org/40463>
  ;; is fixed.
  (append (list (@ (gnu packages guile) guile-3.0/fixed))
          (map specification->package
               '("coreutils" "grep" "sed" "findutils" "diffutils" "patch"
                 "gawk" "gettext" "gzip" "xz"
                 "hello" "zlib"))))

(define %packages-to-cross-build-for-mingw
  ;; Many things don't build for MinGW.  Restrict to what's known to work.
  (map specification->package '("hello")))

(define %cross-bootstrap-targets
  ;; Cross-compilation triplets for which 'bootstrap-tarballs' must be
  ;; buildable.
  '("i586-pc-gnu"
    "arm-linux-gnueabihf"
    "aarch64-linux-gnu"))


;;;
;;; Manifests.
;;;

(define %base-manifest
  (manifest
   (append-map (lambda (system)
                 (map (cut package->manifest-entry* <> system)
                      (cond ((string=? system "i586-gnu")
                             %base-packages/hurd)
                            ((string=? system "armhf-linux")
                             ;; FIXME: Drop special case when ci.guix.gnu.org
                             ;; has more ARMv7 build power.
                             %base-packages/armhf)
                            ((string=? system "powerpc64le-linux")
                             ;; FIXME: Drop 'bootstrap-tarballs' until
                             ;; <https://bugs.gnu.org/48055> is fixed.
                             (drop %base-packages 1))
                            (else
                             %base-packages))))
               %cuirass-supported-systems)))

(define %system-manifest
  (manifest
   (append-map (lambda (system)
                 ;; Some of %SYSTEM-PACKAGES are currently unsupported on some
                 ;; systems--e.g., GNOME on non-x86_64, due to Rust.  Filter
                 ;; them out.
                 (filter-map (lambda (package)
                               (and (supported-package? package system)
                                    (package->manifest-entry* package system)))
                             %system-packages))
               '("x86_64-linux" "i686-linux"))))  ;Guix System

(define %cross-manifest
  (manifest
   (append-map (lambda (target)
                 (map (cut package->manifest-entry* <> "x86_64-linux"
                           #:target target)
                      (if (target-mingw? target)
                          %packages-to-cross-build-for-mingw
                          %packages-to-cross-build)))
               ;; XXX: Important bits like libsigsegv and libffi don't support
               ;; RISCV at the moment, so don't require RISCV support.
               (delete "riscv64-linux-gnu" %cross-targets))))

(define %cross-bootstrap-manifest
  (manifest
   (map (lambda (target)
          (package->manifest-entry*
           (specification->package "bootstrap-tarballs")
           "x86_64-linux" #:target target))
        %cross-bootstrap-targets)))

;; Return the union of all three manifests.
(concatenate-manifests (list %base-manifest
                             %system-manifest
                             %cross-manifest
                             %cross-bootstrap-manifest))
