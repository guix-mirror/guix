;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Vincent Legoll <vincent.legoll@gmail.com>
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

(define-module (gnu packages efi)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

(define-public gnu-efi
  (package
    (name "gnu-efi")
    (version "3.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/gnu-efi/"
                           "gnu-efi-" version ".tar.bz2"))
       (sha256
        (base32 "0z9v5pl5pmlw8pjpd66iyh9pml2hh6pqd4c5qilywilw4wazgk1g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; none exist
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (synopsis "EFI toolchain")
    (description "This package provides an @acronym{EFI, Extensible Firmware
Interface} toolchain for building programs that can run in the
environment presented by Intel's EFI.")
    (home-page "https://directory.fsf.org/wiki/GNU_EFI")
    ;; Distribution is allowed only when accepting all those licenses.
    (license (list license:bsd-2 license:bsd-3 license:bsd-4 license:expat))))

(define-public efi-analyzer
  (let ((commit "77c9e3a67cd7c2fca48a4292dad25a5429872f95")
        (revision "0"))
    (package
      (name "efi-analyzer")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/xypron/efi_analyzer")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1izdkzybqyvzpzqz6kx4j7y47j6aa2dsdrychzgs65466x1a4br1"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags
         (list (string-append "prefix=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'support-cross-compilation
             (lambda _
               (substitute* "Makefile"
                 (("gcc") ,(cc-for-target)))
               #t))
           (delete 'configure))))       ; no configure script
      (home-page "https://github.com/xypron/efi_analyzer")
      (synopsis "Analyze EFI binaries")
      (description
       "The EFI Analyzer checks EFI binaries and prints out header and section
information.")
      (license license:bsd-2))))

(define-public efi_analyzer
  ;; For a short while the package name contained an underscore.
  (deprecated-package "efi_analyzer" efi-analyzer))

(define-public sbsigntools
  (package
    (name "sbsigntools")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://git.kernel.org/pub/scm/linux/kernel/git/jejb/sbsigntools.git")
         (commit (string-append "v" version))
         (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y76wy65y6k10mjl2dm5hb5ms475alr4s080xzj8y833x01xvf3m"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-more-shebangs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/ccan.git/tools/create-ccan-tree"
              (("#!/bin/bash")
               (string-append "#!"
                              (assoc-ref inputs "bash")
                              "/bin/bash")))
             #t))
         (add-after 'unpack 'patch
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* '("configure.ac"
                            "tests/Makefile.am")
              (("/usr/include/efi")
               (string-append (assoc-ref inputs "gnu-efi")
                              "/include/efi"))
              (("/usr/lib/gnuefi")
               (string-append (assoc-ref inputs "gnu-efi")
                              "/lib")))
             #t))
         (add-after 'unpack 'setenv
           (lambda _
             (setenv "CC" "gcc")
             #t)))))
    (native-inputs
     (list autoconf
           automake
           bash
           help2man
           pkg-config
           util-linux)) ; getopt
    (inputs
     `(("gnu-efi" ,gnu-efi)
       ("libuuid" ,util-linux "lib")
       ("openssl" ,openssl)))
    (synopsis "EFI signing tools")
    (description "This package provides tools for signing EFI binaries.")
    (home-page "https://git.kernel.org/pub/scm/linux/kernel/git/jejb/sbsigntools.git/")
    (license license:gpl3+)))

(define-public efitools
  (package
    (name "efitools")
    (version "1.9.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://git.kernel.org/pub/scm/linux/kernel/git/jejb/efitools.git")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jabgl2pxvfl780yvghq131ylpf82k7banjz0ksjhlm66ik8gb1i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; No tests exist.
       #:make-flags
       '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "Make.rules"
              (("/usr/include/efi")
               (string-append (assoc-ref inputs "gnu-efi")
                              "/include/efi"))
              (("\\$\\(DESTDIR\\)/usr")
               (string-append (assoc-ref outputs "out")))
              (("/usr/lib/gnuefi")
               (string-append (assoc-ref inputs "gnu-efi")
                              "/lib")))
             #t))
         (add-after 'unpack 'patch-more-shebangs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "xxdi.pl"
              (("#!.*")
               (string-append "#!"
                              (assoc-ref inputs "perl")
                              "/bin/perl\n")))
             #t))
         (delete 'configure))))
    (native-inputs
     (list help2man perl perl-file-slurp sbsigntools))
    (inputs
     (list gnu-efi openssl))
    (synopsis "EFI tools (key management, variable management)")
    (description "This package provides EFI tools for EFI key management
and EFI variable management.")
    (home-page "https://blog.hansenpartnership.com/efitools-1-4-with-linux-key-manipulation-utilities-released/")
    ;; Programs are under GPL 2.
    ;; Library routines (in lib/) are under LGPL 2.1.
    ;; Compiling/linking/using OpenSSL is permitted.
    (license (list license:gpl2
                   license:lgpl2.1))))

(define-public efilinux
  (package
    (name "efilinux")
    (version "1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mfleming/efilinux")
                    (commit (string-append "efilinux-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b4nxzr3wl5v4b52r79iw1arfgasz26xb38r2blw9v2qz2s1q9w2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             ,@(match (%current-system)
                 ("aarch64-linux"
                  '("ARCH=aarch64"))
                 ("armhf-linux"
                  '("ARCH=arm"))
                 (_ '()))
             (string-append "INCDIR=" (assoc-ref %build-inputs "gnu-efi")
                            "/include")
             (string-append "LIBDIR=" (assoc-ref %build-inputs "gnu-efi")
                            "/lib"))
       #:tests? #f ; No tests exist.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "efilinux.efi"
                           (string-append (assoc-ref outputs "out")
                                          "/libexec"))
             #t)))))
    (inputs
     (list gnu-efi))
    (synopsis "Minimal Linux loader for UEFI")
    (description "This package provides a minimal Linux loader as an UEFI
program.")
    (home-page "https://github.com/mfleming/efilinux")
    (license license:bsd-2)))
