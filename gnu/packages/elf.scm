;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (gpl3+ lgpl3+ lgpl2.0+))
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xml))

(define-public elfutils
  (package
    (name "elfutils")
    (version "0.173")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceware.org/elfutils/ftp/"
                                  version "/elfutils-" version ".tar.bz2"))
              (sha256
               (base32
                "1zq0l12k64hrbjmdjc4llrad96c25i427hpma1id9nk87w9qqvdp"))
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
                                              "/lib"))

       ;; Disable tests on MIPS (without changing
       ;; the arguments list on other systems).
       ,@(if (string-prefix? "mips" (or (%current-target-system)
                                        (%current-system)))
             '(#:tests? #f)
             '())

       #:phases
       (modify-phases %standard-phases
         ;; No reason has been found for this test to reliably fail on aarch64-linux.
         (add-after 'unpack 'disable-failing-aarch64-tests
           (lambda _
             (substitute* "tests/Makefile.in"
               (("run-backtrace-native.sh") ""))
             #t)))))

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

(define-public libabigail
  (package
    (name "libabigail")
    (home-page "https://sourceware.org/libabigail/")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceware.org/pub/libabigail/"
                                  "libabigail-" version ".tar.gz"))
              (sha256
               (base32
                "04j07lhvwbp6qp8pdwbf7iqnr7kgpabmqylsw4invpmzwnyp6g6g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static"
                           "--enable-bash-completion"
                           "--enable-manual")
       #:make-flags '("V=1")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-source
                    (lambda _
                      (substitute* "build-aux/ltmain.sh"
                        ;; Don't add -specs=/usr/lib/rpm/redhat/redhat-hardened-ld
                        ;; to the GCC command line.
                        (("compiler_flags=\"-specs=.*")
                         "compiler_flags=\n"))
                      #t))
                  (add-after 'build 'build-documentation
                    (lambda _
                      (invoke "make" "-C" "doc/manuals" "html-doc" "man" "info")))
                  (add-before 'check 'set-test-environment
                    (lambda _
                      (setenv "XDG_CACHE_HOME" "/tmp")
                      #t))
                  (add-after 'install 'install-documentation
                    (lambda _
                      (invoke "make" "-C" "doc/manuals"
                              "install-man-and-info-doc")))
                  (add-after 'install-documentation 'install-bash-completion
                    (lambda* (#:key outputs #:allow-other-keys)
                      (for-each (lambda (file)
                                  (install-file
                                   file (string-append (assoc-ref outputs "out")
                                                       "/share/bash-completion"
                                                       "/completions")))
                                (find-files "bash-completion" ".*abi.*"))
                      #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("makeinfo" ,texinfo)
       ("python-sphinx" ,python-sphinx)
       ("python" ,python)))             ;for tests
    (propagated-inputs
     `(("elfutils" ,elfutils)           ;libabigail.la says -lelf
       ("libxml2" ,libxml2)))           ;in Requires.private of libabigail.pc
    (synopsis "Analyze application binary interfaces (ABIs)")
    (description
     "@dfn{ABIGAIL} stands for the Application Binary Interface Generic
Analysis and Instrumentation Library.  It is a framework which aims at
helping developers and software distributors to spot ABI-related issues
like interface incompatibility in ELF shared libraries by performing a
static analysis of the ELF binaries at hand.")
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
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       ,@(if (string=? "aarch64-linux"
                                       (%current-system))
                             '("--host=aarch64-unknown-linux-gnu")
                             '()))))))))
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
                   "https://nixos.org/releases/patchelf/patchelf-"
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
     (if (target-arm32?)
         `(("patch/rework-for-arm" ,(search-patch
                                     "patchelf-rework-for-arm.patch")))
         '()))
    (arguments
     (if (target-arm32?)
         `(#:phases
           (modify-phases %standard-phases
             (add-after 'unpack 'patch/rework-for-arm
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((patch-file (assoc-ref inputs "patch/rework-for-arm")))
                   (invoke "patch" "--force" "-p1" "--input" patch-file))))))
         '()))

    (home-page "https://nixos.org/patchelf.html")
    (synopsis "Modify the dynamic linker and RPATH of ELF executables")
    (description
     "PatchELF allows the ELF \"interpreter\" and RPATH of an ELF binary to be
changed.")
    (license gpl3+)))
