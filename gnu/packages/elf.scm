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

(define-module (gnu packages elf)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (gpl3+ lgpl3+ lgpl2.0+))
  #:use-module (gnu packages)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages compression))

(define-public elfutils
  (package
    (name "elfutils")
    (version "0.157")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://fedorahosted.org/releases/e/l/elfutils/"
                    version "/elfutils-" version ".tar.bz2"))
              (sha256
               (base32
                "11ffbihw9zs2dhmlww9zilwvmv6v1pr6bvnz5dnzn0lxq70ckbag"))))
    (build-system gnu-build-system)

    ;; Separate programs because that's usually not what elfutils users want,
    ;; and because they duplicate what Binutils provides.
    (outputs '("out"                           ; libelf.so, elfutils/*.h, etc.
               "bin"))                         ; ld, nm, objdump, etc.

    (native-inputs `(("m4" ,m4)))
    (inputs `(("zlib" ,zlib)))
    (home-page "https://fedorahosted.org/elfutils/")
    (synopsis "Linker and ELF manipulation tools")
    (description
     "This package provides command-line tools to manipulate binaries in the
Executable and Linkable Format (ELF).  This includes ld, ar, objdump,
addr2line, and more.")

    ;; Libraries are dual-licensed LGPLv3.0+ | GPLv2, and programs are GPLv3+.
    (license lgpl3+)))

(define-public libelf
  (package
    (name "libelf")
    (version "0.8.13")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.mr511.de/software/libelf-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0vf7s9dwk2xkmhb79aigqm0x0yfbw1j0b9ksm51207qwr179n6jr"))))
    (build-system gnu-build-system)
    (arguments '(#:phases (alist-replace
                           'configure
                           (lambda* (#:key outputs #:allow-other-keys)
                             ;; This old `configure' script doesn't support
                             ;; variables passed as arguments.
                             (let ((out (assoc-ref outputs "out")))
                               (setenv "CONFIG_SHELL" (which "bash"))
                               (zero?
                                (system* "./configure"
                                         (string-append "--prefix=" out)))))
                           %standard-phases)))
    (home-page "http://www.mr511.de/software/english.html")
    (synopsis "ELF object file access library")
    (description "libelf is a C library to access ELF object files.")
    (license lgpl2.0+)))

(define-public patchelf
  (package
    (name "patchelf")
    (version "0.6")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://nixos.org/releases/patchelf/patchelf-"
                   version
                   "/patchelf-" version ".tar.bz2"))
             (sha256
              (base32
               "00bw29vdsscsili65wcb5ay0gvg1w0ljd00sb5xc6br8bylpyzpw"))
             (patches (list (search-patch "patchelf-page-size.patch")))))
    (build-system gnu-build-system)
    (home-page "http://nixos.org/patchelf.html")
    (synopsis "Modify the dynamic linker and RPATH of ELF executables")
    (description
     "PatchELF allows the ELF \"interpreter\" and RPATH of an ELF binary to be
changed.")
    (license gpl3+)))
