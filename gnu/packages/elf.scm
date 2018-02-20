;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
    (version "0.170")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceware.org/elfutils/ftp/"
                                  version "/elfutils-" version ".tar.bz2"))
              (sha256
               (base32
                "0rp0r54z44is49c594qy7hr211nhb00aa5y7z74vsybbaxslg10z"))
              (patches (search-patches "elfutils-tests-ptrace.patch"))))
    (build-system gnu-build-system)

    ;; Separate programs because that's usually not what elfutils users want,
    ;; and because they duplicate what Binutils provides.
    (outputs '("out"                           ; libelf.so, elfutils/*.h, etc.
               "bin"))                         ; ld, nm, objdump, etc.

    (arguments
     ;; Programs don't have libelf.so in their RUNPATH and libraries don't
     ;; know where to find each other.
     `(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))))

    (native-inputs `(("m4" ,m4)))
    (inputs `(("zlib" ,zlib)))
    (home-page "https://sourceware.org/elfutils/")
    (synopsis "Linker and ELF manipulation tools")
    (description
     "This package provides command-line tools to manipulate binaries in the
Executable and Linkable Format (@dfn{ELF}).  This includes @command{ld},
@command{ar}, @command{objdump}, @command{addr2line}, and more.")

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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; This old `configure' script doesn't support
             ;; variables passed as arguments.
             (let ((out (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (zero?
                (system* "./configure"
                         (string-append "--prefix=" out)
                       ,@(if (string=? "aarch64-linux"
                                       (%current-system))
                             '("--host=aarch64-unknown-linux-gnu")
                             '())))))))))
    (home-page "http://www.mr511.de/software/english.html")
    (synopsis "ELF object file access library")
    (description "Libelf is a C library to access ELF object files.")
    (license lgpl2.0+)))

(define-public patchelf
  (package
    (name "patchelf")
    (version "0.8")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://nixos.org/releases/patchelf/patchelf-"
                   version
                   "/patchelf-" version ".tar.bz2"))
             (sha256
              (base32
               "1rqpg84wrd3fa16wa9vqdvasnc05yz49w207cz1l0wrl4k8q97y9"))
             (patches (search-patches "patchelf-page-size.patch"))))
    (build-system gnu-build-system)

    ;; XXX: The upstream 'patchelf' doesn't support ARM.  The only available
    ;;      patch makes significant changes to the algorithm, possibly
    ;;      introducing bugs.  So, we apply the patch only on ARM systems.
    (inputs
     (if (string-prefix? "arm" (or (%current-target-system) (%current-system)))
         `(("patch/rework-for-arm" ,(search-patch
                                     "patchelf-rework-for-arm.patch")))
         '()))
    (arguments
     (if (string-prefix? "arm" (or (%current-target-system) (%current-system)))
         `(#:phases (modify-phases %standard-phases
                      (add-after 'unpack 'patch/rework-for-arm
                        (lambda* (#:key inputs #:allow-other-keys)
                          (let ((patch-file
                                 (assoc-ref inputs "patch/rework-for-arm")))
                            (invoke "patch" "--force" "-p1"
                                    "--input" patch-file)
                            #t)))))
         '()))

    (home-page "http://nixos.org/patchelf.html")
    (synopsis "Modify the dynamic linker and RPATH of ELF executables")
    (description
     "PatchELF allows the ELF \"interpreter\" and RPATH of an ELF binary to be
changed.")
    (license gpl3+)))
