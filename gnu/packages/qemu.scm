;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages qemu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages linux))

(define-public qemu-kvm
  (package
    (name "qemu-kvm")
    (version "1.2.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/kvm/qemu-kvm/"
                                 version "/qemu-kvm-" version ".tar.gz"))
             (sha256
              (base32
               "018vb5nmk2fsm143bs2bl2wirhasd4b10d7jchl32zik4inbk2p9"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-replace
                 'configure
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; The `configure' script doesn't understand some of the
                   ;; GNU options.  Thus, add a new phase that's compatible.
                   (let ((out (assoc-ref outputs "out")))
                     (setenv "SHELL" (which "bash"))

                     ;; The binaries need to be linked against -lrt.
                     (setenv "LDFLAGS" "-lrt")
                     (zero?
                      (system* "./configure"
                               (string-append "--prefix=" out)))))
                 %standard-phases)))
    (inputs                                       ; TODO: Add optional inputs.
     `(;; ("mesa" ,mesa)
       ;; ("libaio" ,libaio)
       ("glib" ,glib)
       ("python" ,python)
       ("ncurses" ,ncurses)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg-8)
       ;; ("vde2" ,vde2)
       ("util-linux" ,util-linux)
       ;; ("pciutils" ,pciutils)
       ("pkg-config" ,pkg-config)
       ;; ("alsa-lib" ,alsa-lib)
       ;; ("SDL" ,SDL)
       ("zlib" ,zlib)
       ("attr" ,attr)))
    (home-page "http://www.linux-kvm.org/")
    (synopsis
     "A full virtualization solution for Linux on x86 hardware containing virtualization extensions")
    (description #f)
    (license #f)))
