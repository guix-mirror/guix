;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 Raymond Nicholson <rain1@openmailbox.org>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016, 2018, 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2016 Carlos Sánchez de La Lama <csanchezdll@gmail.com>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.com>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 nee <nee-git@hidamari.blue>
;;; Copyright © 2017 Dave Love <fx@gnu.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018, 2020 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2018 Manuel Graf <graf@init.at>
;;; Copyright © 2018 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2019 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019, 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Stefan Stefanović <stefanx2ovic@gmail.com>
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Anders Thuné <asse.97@gmail.com>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020, 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2020 David Dashyan <mail@davie.li>
;;; Copyright © 2020 pukkamustard <pukkamustard@posteo.net>
;;; Copyright © 2021 B. Wilson <elaexuotee@wilsonb.com>
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

(define-module (gnu packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages file)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rrdtool)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages swig)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system linux-module)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix deprecation)    ;for libcap/next
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public (system->linux-architecture arch)
  "Return the Linux architecture name for ARCH, a Guix system name such as
\"x86_64-linux\" or a target triplet such as \"arm-linux-gnueabihf\"."
  (let ((arch (car (string-split arch #\-))))
    (cond ((string=? arch "i686") "i386")
          ((string-prefix? "mips" arch) "mips")
          ((string-prefix? "arm" arch) "arm")
          ((string-prefix? "aarch64" arch) "arm64")
          ((string-prefix? "alpha" arch) "alpha")
          ((string-prefix? "powerpc" arch) "powerpc") ;including "powerpc64le"
          ((string-prefix? "s390" arch) "s390")
          ((string-prefix? "riscv" arch) "riscv")
          (else arch))))

(define-public (system->defconfig system)
  "Some systems (notably powerpc-linux) require a special target for kernel
defconfig.  Return the appropriate make target if applicable, otherwise return
\"defconfig\"."
  (cond ((string-prefix? "powerpc-" system) "pmac32_defconfig")
        ((string-prefix? "powerpc64-" system) "ppc64_defconfig")
        ((string-prefix? "powerpc64le-" system) "ppc64_defconfig")
        (else "defconfig")))


;;;
;;; Kernel source code deblobbing.
;;;

(define (linux-libre-deblob-scripts version
                                    deblob-hash
                                    deblob-check-hash)
  (list (version-major+minor version)
        (origin
          (method url-fetch)
          (uri (string-append "https://linux-libre.fsfla.org"
                              "/pub/linux-libre/releases/" version "-gnu/"
                              "deblob-" (version-major+minor version)))
          (file-name (string-append "linux-libre-deblob-"
                                    version))
          (sha256 deblob-hash))
        (origin
          (method url-fetch)
          (uri (string-append "https://linux-libre.fsfla.org"
                              "/pub/linux-libre/releases/" version "-gnu/"
                              "deblob-check"))
          (file-name (string-append "linux-libre-deblob-check-" version))
          (sha256 deblob-check-hash))))

(define* (computed-origin-method gexp-promise hash-algo hash
                                 #:optional (name "source")
                                 #:key (system (%current-system))
                                 (guile (default-guile)))
  "Return a derivation that executes the G-expression that results
from forcing GEXP-PROMISE."
  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "computed-origin")
                      (force gexp-promise)
                      #:graft? #f       ;nothing to graft
                      #:system system
                      #:guile-for-build guile)))

(define (make-linux-libre-source version
                                 upstream-source
                                 deblob-scripts)
  "Return a 'computed' origin that generates a Linux-libre tarball from the
corresponding UPSTREAM-SOURCE (an origin), using the given DEBLOB-SCRIPTS."
  (match deblob-scripts
    ((deblob-version (? origin? deblob) (? origin? deblob-check))
     (unless (string=? deblob-version (version-major+minor version))
       ;; The deblob script cannot be expected to work properly on a
       ;; different version (major+minor) of Linux, even if no errors
       ;; are signaled during execution.
       (error "deblob major+minor version mismatch"))
     (origin
       (method computed-origin-method)
       (file-name (string-append "linux-libre-" version "-guix.tar.xz"))
       (sha256 #f)
       (uri
        (delay
          (with-imported-modules '((guix build utils))
            #~(begin
                (use-modules (guix build utils)
                             (srfi srfi-1)
                             (ice-9 match)
                             (ice-9 ftw))

                (setvbuf (current-output-port) 'line)

                (let ((dir (string-append "linux-" #$version)))

                  (mkdir "/tmp/bin")
                  (set-path-environment-variable
                   "PATH" '("bin")
                   (list "/tmp"
                         #+(canonical-package bash)
                         #+(canonical-package coreutils)
                         #+(canonical-package diffutils)
                         #+(canonical-package findutils)
                         #+(canonical-package patch)
                         #+(canonical-package xz)
                         #+(canonical-package sed)
                         #+(canonical-package grep)
                         #+(canonical-package bzip2)
                         #+(canonical-package gzip)
                         #+(canonical-package tar)
                         #+(canonical-package gawk)
                         #+python-wrapper))

                  (with-directory-excursion "/tmp/bin"

                    (copy-file #+deblob "deblob")
                    (chmod "deblob" #o755)
                    (substitute* "deblob"
                      (("/bin/sh") (which "sh")))

                    (copy-file #+deblob-check "deblob-check")
                    (chmod "deblob-check" #o755)
                    (substitute* "deblob-check"
                      (("/bin/sh") (which "sh"))
                      (("/bin/sed") (which "sed"))
                      (("/usr/bin/python") (which "python"))))

                  (if (file-is-directory? #+upstream-source)
                      (begin
                        (format #t "Copying upstream linux source...~%")
                        (invoke "cp" "--archive" #+upstream-source dir)
                        (invoke "chmod" "--recursive" "u+w" dir))
                      (begin
                        (format #t "Unpacking upstream linux tarball...~%")
                        (invoke "tar" "xf" #$upstream-source)
                        (match (scandir "."
                                        (lambda (name)
                                          (and (not (member name '("." "..")))
                                               (file-is-directory? name))))
                          ((unpacked-dir)
                           (unless (string=? dir unpacked-dir)
                             (rename-file unpacked-dir dir)))
                          (dirs
                           (error "multiple directories found" dirs)))))

                  (with-directory-excursion dir
                    (format #t "Running deblob script...~%")
                    (invoke "/tmp/bin/deblob"))

                  (format #t "~%Packing new Linux-libre tarball...~%")
                  (invoke "tar" "cvfa" #$output
                          ;; Avoid non-determinism in the archive.
                          "--mtime=@0"
                          "--owner=root:0"
                          "--group=root:0"
                          "--sort=name"
                          "--hard-dereference"
                          dir)

                  (format #t "~%Scanning the generated tarball for blobs...~%")
                  (invoke "/tmp/bin/deblob-check" "--use-awk" "--list-blobs"
                          #$output))))))))))


;;;
;;; Kernel sources.
;;;

(define (linux-libre-urls version)
  "Return a list of URLs for Linux-Libre VERSION."
  (list (string-append
         "https://linux-libre.fsfla.org/pub/linux-libre/releases/"
         version "-gnu/linux-libre-" version "-gnu.tar.xz")

        ;; XXX: Work around <http://bugs.gnu.org/14851>.
        (string-append
         "ftp://alpha.gnu.org/gnu/guix/mirror/linux-libre-"
         version "-gnu.tar.xz")

        ;; Maybe this URL will become valid eventually.
        (string-append
         "mirror://gnu/linux-libre/" version "-gnu/linux-libre-"
         version "-gnu.tar.xz")))

(define (%upstream-linux-source version hash)
  (origin
    (method url-fetch)
    (uri (string-append "mirror://kernel.org"
                        "/linux/kernel/v" (version-major version) ".x/"
                        "linux-" version ".tar.xz"))
    (sha256 hash)))

;; The current "stable" kernels. That is, the most recently released major
;; versions that are still supported upstream.
(define-public linux-libre-5.11-version "5.11.17")
(define deblob-scripts-5.11
  (linux-libre-deblob-scripts
   linux-libre-5.11-version
   (base32 "1kcvwbbzlii4smx6m4hj97va5bf3drbglb24jkky93a1g37a9ksj")
   (base32 "0yvr80g200hdryz54gdnzj4fl38pf7g4qbgj475rhcfwixhp1j7n")))
(define-public linux-libre-5.11-pristine-source
  (let ((version linux-libre-5.11-version)
        (hash (base32 "18yld85jymb3wf71kd425zzzvriz9wxlw90pklgf91q37r9rxpa1")))
   (make-linux-libre-source version
                            (%upstream-linux-source version hash)
                            deblob-scripts-5.11)))

;; The "longterm" kernels — the older releases with long-term upstream support.
;; Here are the support timelines:
;; <https://www.kernel.org/category/releases.html>
(define-public linux-libre-5.10-version "5.10.33")
(define deblob-scripts-5.10
  (linux-libre-deblob-scripts
   linux-libre-5.10-version
   (base32 "0i99adbfjnir8p8ihhac58dv8p7mnqg4z2jpgvhj35lksdskngf7")
   (base32 "0hh27ccqimagr3aij7ygwikxw66y63sqwd0xlf49bhpjd090r9a7")))
(define-public linux-libre-5.10-pristine-source
  (let ((version linux-libre-5.10-version)
        (hash (base32 "05a3bcc6ic9gyhp8bahfrk8xm8pwb6jmgad6nwqgih3icg1xngwk")))
   (make-linux-libre-source version
                            (%upstream-linux-source version hash)
                            deblob-scripts-5.10)))

(define-public linux-libre-5.4-version "5.4.115")
(define deblob-scripts-5.4
  (linux-libre-deblob-scripts
   linux-libre-5.4-version
   (base32 "0q3gwf3b404brjld7aj9krzv0wdpzvs8fgy088ag7q106cwgqg8i")
   (base32 "1xghbbnaisjd0k1klbyn1p7r6r4x5a1bpmkm56a3gh2zvw4s7mj8")))
(define-public linux-libre-5.4-pristine-source
  (let ((version linux-libre-5.4-version)
        (hash (base32 "1llxk04vlpi7a4ca2f5vlcxfn68n8yhmsar3hsl259i7hms28isv")))
   (make-linux-libre-source version
                            (%upstream-linux-source version hash)
                            deblob-scripts-5.4)))

(define-public linux-libre-4.19-version "4.19.189")
(define deblob-scripts-4.19
  (linux-libre-deblob-scripts
   linux-libre-4.19-version
   (base32 "02zs405awaxydbapka4nz8h6lmnc0dahgczqsrs5s2bmzjyyqkcy")
   (base32 "1jiaw0as1ippkrjdpd52657w5mz9qczg3y2hlra7m9k0xawwiqlf")))
(define-public linux-libre-4.19-pristine-source
  (let ((version linux-libre-4.19-version)
        (hash (base32 "12a3qn5s66rrbrq5p8dsf7kn0l74q0ahqkl3bmbw1fvyy1fhwyvi")))
    (make-linux-libre-source version
                             (%upstream-linux-source version hash)
                             deblob-scripts-4.19)))

(define-public linux-libre-4.14-version "4.14.232")
(define deblob-scripts-4.14
  (linux-libre-deblob-scripts
   linux-libre-4.14-version
   (base32 "091jk9jkn9jf39bxpc7395bhcb7p96nkg3a8047380ki06lnfxh6")
   (base32 "1qij18inijj6c3ma8hv98yjagnzxdxyn134da9fd23ky8q6hbvky")))
(define-public linux-libre-4.14-pristine-source
  (let ((version linux-libre-4.14-version)
        (hash (base32 "0sa3sz7bznlhijd0iwv37nyrrnw34iq6dq1bqr6cj2wpyrhr7h8x")))
    (make-linux-libre-source version
                             (%upstream-linux-source version hash)
                             deblob-scripts-4.14)))

(define-public linux-libre-4.9-version "4.9.268")
(define deblob-scripts-4.9
  (linux-libre-deblob-scripts
   linux-libre-4.9-version
   (base32 "1wvldzlv7q2xdbadas87dh593nxr4a8p5n0f8zpm72lja6w18hmg")
   (base32 "0fxajshb75siq39lj5h8xvhdj8lcmddkslwlyj65rhlwk6g2r4b2")))
(define-public linux-libre-4.9-pristine-source
  (let ((version linux-libre-4.9-version)
        (hash (base32 "0aknrlf5q0dsqib8c9klmf5c60dy7hg2zksb020qvyrp077gcrjv")))
    (make-linux-libre-source version
                             (%upstream-linux-source version hash)
                             deblob-scripts-4.9)))

(define-public linux-libre-4.4-version "4.4.268")
(define deblob-scripts-4.4
  (linux-libre-deblob-scripts
   linux-libre-4.4-version
   (base32 "0x2j1i88am54ih2mk7gyl79g25l9zz4r08xhl482l3fvjj2irwbw")
   (base32 "0hhin1jpfkd6nwrb6xqxjzl3hdxy4pn8a15hy2d3d83yw6pflbsf")))
(define-public linux-libre-4.4-pristine-source
  (let ((version linux-libre-4.4-version)
        (hash (base32 "1srk08kaxq5jjlqx804cgjffhcsrdkv3idh8ipagl6v2w4kas5v8")))
    (make-linux-libre-source version
                             (%upstream-linux-source version hash)
                             deblob-scripts-4.4)))

(define %boot-logo-patch
  ;; Linux-Libre boot logo featuring Freedo and a gnu.
  (origin
    (method url-fetch)
    (uri (string-append "http://www.fsfla.org/svn/fsfla/software/linux-libre/"
                        "lemote/gnewsense/branches/3.16/100gnu+freedo.patch"))
    (sha256
     (base32
      "1hk9swxxc80bmn2zd2qr5ccrjrk28xkypwhl4z0qx4hbivj7qm06"))))

(define %linux-libre-arm-export-__sync_icache_dcache-patch
  (origin
    (method url-fetch)
    (uri (string-append
          "https://salsa.debian.org/kernel-team/linux"
          "/raw/34a7d9011fcfcfa38b68282fd2b1a8797e6834f0"
          "/debian/patches/bugfix/arm/"
          "arm-mm-export-__sync_icache_dcache-for-xen-privcmd.patch"))
    (file-name "linux-libre-arm-export-__sync_icache_dcache.patch")
    (sha256
     (base32 "1ifnfhpakzffn4b8n7x7w5cps9mzjxlkcfz9zqak2vaw8nzvl39f"))))

(define (source-with-patches source patches)
  (origin
    (inherit source)
    (patches (append (origin-patches source)
                     patches))))

(define-public linux-libre-5.11-source
  (source-with-patches linux-libre-5.11-pristine-source
                       (list %boot-logo-patch
                             %linux-libre-arm-export-__sync_icache_dcache-patch
                             ;; Pinebook Pro patch to fix LCD display
                             (search-patch
                              "linux-libre-arm64-generic-pinebook-lcd.patch"))))

(define-public linux-libre-5.10-source
  (source-with-patches linux-libre-5.10-pristine-source
                       (list %boot-logo-patch
                             %linux-libre-arm-export-__sync_icache_dcache-patch
			     (search-patch
			      "linux-libre-arm64-generic-pinebook-lcd.patch"))))

(define-public linux-libre-5.4-source
  (source-with-patches linux-libre-5.4-pristine-source
                       (list %boot-logo-patch
                             %linux-libre-arm-export-__sync_icache_dcache-patch
                             ;; Pinebook Pro patch from linux-next,
                             ;; can be dropped for linux-libre 5.7
                             (search-patch
                              "linux-libre-support-for-Pinebook-Pro.patch"))))

(define-public linux-libre-4.19-source
  (source-with-patches linux-libre-4.19-pristine-source
                       (list %boot-logo-patch
                             %linux-libre-arm-export-__sync_icache_dcache-patch)))

(define-public linux-libre-4.14-source
  (source-with-patches linux-libre-4.14-pristine-source
                       (list %boot-logo-patch)))

(define-public linux-libre-4.9-source
  (source-with-patches linux-libre-4.9-pristine-source
                       (list %boot-logo-patch)))

(define-public linux-libre-4.4-source
  (source-with-patches linux-libre-4.4-pristine-source
                       (list %boot-logo-patch)))


;;;
;;; Kernel headers.
;;;

(define (make-linux-libre-headers version hash-string)
  (make-linux-libre-headers* version
                             (origin
                               (method url-fetch)
                               (uri (linux-libre-urls version))
                               (sha256 (base32 hash-string)))))

(define (make-linux-libre-headers* version source)
  (package
    (name "linux-libre-headers")
    (version version)
    (source source)
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)
                     ,@(if (version>=? version "4.16")
                           `(("flex" ,flex)
                             ("bison" ,bison))
                           '())
                     ,@(if (version>=? version "5.3")
                           `(("rsync" ,rsync))
                           '())))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (let ((arch ,(system->linux-architecture
                          (or (%current-target-system)
                              (%current-system))))
                   (defconfig ,(system->defconfig
                                (or (%current-target-system)
                                    (%current-system)))))
               (setenv "ARCH" arch)
               (format #t "`ARCH' set to `~a'~%" (getenv "ARCH"))
               (invoke "make" defconfig)
               (invoke "make" "mrproper" "headers_check"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make"
                       (string-append "INSTALL_HDR_PATH=" out)
                       "headers_install")

               (mkdir (string-append out "/include/config"))
               (call-with-output-file
                   (string-append out
                                  "/include/config/kernel.release")
                 (lambda (p)
                   (format p "~a-default~%" ,version)))

               ;; Remove the '.install' and '..install.cmd' files; the
               ;; latter contains store paths, which pulls in bootstrap
               ;; binaries in the build environment, and prevents bit
               ;; reproducibility for the bootstrap binaries.
               (for-each delete-file (find-files out "\\.install"))

               #t))))
       #:allowed-references ()
       #:tests? #f))
    (home-page "https://www.gnu.org/software/linux-libre/")
    (synopsis "GNU Linux-Libre kernel headers")
    (description "Headers of the Linux-Libre kernel.")
    (license license:gpl2)))

(define-public linux-libre-headers-5.11
  (make-linux-libre-headers* linux-libre-5.11-version
                             linux-libre-5.11-source))

(define-public linux-libre-headers-5.10
  (make-linux-libre-headers* linux-libre-5.10-version
                             linux-libre-5.10-source))

(define-public linux-libre-headers-5.4
  (make-linux-libre-headers* linux-libre-5.4-version
                             linux-libre-5.4-source))

(define-public linux-libre-headers-4.19
  (make-linux-libre-headers* linux-libre-4.19-version
                             linux-libre-4.19-source))

(define-public linux-libre-headers-4.14
  (make-linux-libre-headers* linux-libre-4.14-version
                             linux-libre-4.14-source))

(define-public linux-libre-headers-4.9
  (make-linux-libre-headers* linux-libre-4.9-version
                             linux-libre-4.9-source))

(define-public linux-libre-headers-4.4
  (make-linux-libre-headers* linux-libre-4.4-version
                             linux-libre-4.4-source))

;; The following package is used in the early bootstrap, and thus must be kept
;; stable and with minimal build requirements.
(define-public linux-libre-headers-5.4.20
  (make-linux-libre-headers "5.4.20"
                            "1qxhf6dmcwjblzx8fgn6vr10p38xw10iwh6d1y1v1mxb25y30b47"))

(define-public linux-libre-headers linux-libre-headers-5.4.20)


;;;
;;; Kernel configurations.
;;;

(define* (kernel-config arch #:key variant)
  "Return the absolute file name of the Linux-Libre build configuration file
for ARCH and optionally VARIANT, or #f if there is no such configuration."
  (let* ((name (string-append (if variant (string-append variant "-") "")
                              (if (string=? "i386" arch) "i686" arch) ".conf"))
         (file (string-append "linux-libre/" name)))
    (search-auxiliary-file file)))

(define %default-extra-linux-options
  `(;; Some very mild hardening.
    ("CONFIG_SECURITY_DMESG_RESTRICT" . #t)
    ;; All kernels should have NAMESPACES options enabled
    ("CONFIG_NAMESPACES" . #t)
    ("CONFIG_UTS_NS" . #t)
    ("CONFIG_IPC_NS" . #t)
    ("CONFIG_USER_NS" . #t)
    ("CONFIG_PID_NS" . #t)
    ("CONFIG_NET_NS" . #t)
    ;; Various options needed for elogind service:
    ;; https://issues.guix.gnu.org/43078
    ("CONFIG_CGROUP_FREEZER" . #t)
    ("CONFIG_BLK_CGROUP" . #t)
    ("CONFIG_CGROUP_WRITEBACK" . #t)
    ("CONFIG_CGROUP_SCHED" . #t)
    ("CONFIG_CGROUP_PIDS" . #t)
    ("CONFIG_CGROUP_FREEZER" . #t)
    ("CONFIG_CGROUP_DEVICE" . #t)
    ("CONFIG_CGROUP_CPUACCT" . #t)
    ("CONFIG_CGROUP_PERF" . #t)
    ("CONFIG_SOCK_CGROUP_DATA" . #t)
    ("CONFIG_BLK_CGROUP_IOCOST" . #t)
    ("CONFIG_CGROUP_NET_PRIO" . #t)
    ("CONFIG_CGROUP_NET_CLASSID" . #t)
    ("CONFIG_MEMCG" . #t)
    ("CONFIG_MEMCG_SWAP" . #t)
    ("CONFIG_MEMCG_KMEM" . #t)
    ("CONFIG_CPUSETS" . #t)
    ("CONFIG_PROC_PID_CPUSET" . #t)
    ;; Allow disk encryption by default
    ("CONFIG_DM_CRYPT" . m)
    ;; Modules required for initrd:
    ("CONFIG_NET_9P" . m)
    ("CONFIG_NET_9P_VIRTIO" . m)
    ("CONFIG_VIRTIO_BLK" . m)
    ("CONFIG_VIRTIO_NET" . m)
    ("CONFIG_VIRTIO_PCI" . m)
    ("CONFIG_VIRTIO_BALLOON" . m)
    ("CONFIG_VIRTIO_MMIO" . m)
    ("CONFIG_FUSE_FS" . m)
    ("CONFIG_CIFS" . m)
    ("CONFIG_9P_FS" . m)))

;; See https://github.com/iovisor/bcc/blob/master/INSTALL.md#kernel-configuration
(define %bpf-extra-linux-options
  `(;; Needed for probes
    ("CONFIG_UPROBE_EVENTS" . #t)
    ("CONFIG_KPROBE_EVENTS" . #t)
    ;; kheaders module also helpful for tracing
    ("CONFIG_IKHEADERS" . #t)
    ("CONFIG_BPF" . #t)
    ("CONFIG_BPF_SYSCALL" . #t)
    ("CONFIG_BPF_JIT_ALWAYS_ON" . #t)
    ;; optional, for tc filters
    ("CONFIG_NET_CLS_BPF" . m)
    ;; optional, for tc actions
    ("CONFIG_NET_ACT_BPF" . m)
    ("CONFIG_BPF_JIT" . #t)
    ;; for Linux kernel versions 4.1 through 4.6
    ;; ("CONFIG_HAVE_BPF_JIT" . y)
    ;; for Linux kernel versions 4.7 and later
    ("CONFIG_HAVE_EBPF_JIT" . #t)
    ;; optional, for kprobes
    ("CONFIG_BPF_EVENTS" . #t)
    ;; kheaders module
    ("CONFIG_IKHEADERS" . #t)))

(define (config->string options)
  (string-join (map (match-lambda
                      ((option . 'm)
                       (string-append option "=m"))
                      ((option . #t)
                       (string-append option "=y"))
                      ((option . #f)
                       (string-append option "=n")))
                    options)
               "\n"))


;;;
;;; Kernel package utilities.
;;;

(define* (make-linux-libre version hash-string supported-systems
                           #:key
                           ;; A function that takes an arch and a variant.
                           ;; See kernel-config for an example.
                           (extra-version #f)
                           (configuration-file #f)
                           (defconfig "defconfig")
                           (extra-options %default-extra-linux-options)
                           (patches (list %boot-logo-patch)))
  (make-linux-libre* version
                     (origin
                       (method url-fetch)
                       (uri (linux-libre-urls version))
                       (sha256 (base32 hash-string))
                       (patches patches))
                     supported-systems
                     #:extra-version extra-version
                     #:configuration-file configuration-file
                     #:defconfig defconfig
                     #:extra-options extra-options))

(define* (make-linux-libre* version source supported-systems
                            #:key
                            ;; A function that takes an arch and a variant.
                            ;; See kernel-config for an example.
                            (extra-version #f)
                            (configuration-file #f)
                            (defconfig "defconfig")
                            (extra-options %default-extra-linux-options))
  (package
    (name (if extra-version
              (string-append "linux-libre-" extra-version)
              "linux-libre"))
    (version version)
    (source source)
    (supported-systems supported-systems)
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("bc" ,bc)
       ("openssl" ,openssl)
       ("elfutils" ,elfutils)  ; Needed to enable CONFIG_STACK_VALIDATION
       ("flex" ,flex)
       ("bison" ,bison)

       ;; These are needed to compile the GCC plugins.
       ("gmp" ,gmp)
       ("mpfr" ,mpfr)
       ("mpc" ,mpc)

       ,@(match (and configuration-file
                     (configuration-file
                      (system->linux-architecture
                       (or (%current-target-system) (%current-system)))
                      #:variant (version-major+minor version)))
           (#f                                    ;no config for this platform
            '())
           ((? string? config)
            `(("kconfig" ,config))))))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 ftw)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/pwd
           (lambda _
             (substitute* (find-files "." "^Makefile(\\.include)?$")
               (("/bin/pwd") "pwd"))
             #t))
         (replace 'configure
           (lambda* (#:key inputs native-inputs target #:allow-other-keys)
             ;; Avoid introducing timestamps
             (setenv "KCONFIG_NOTIMESTAMP" "1")
             (setenv "KBUILD_BUILD_TIMESTAMP" (getenv "SOURCE_DATE_EPOCH"))

             ;; Set ARCH and CROSS_COMPILE
             (let ((arch ,(system->linux-architecture
                           (or (%current-target-system)
                               (%current-system)))))
               (setenv "ARCH" arch)
               (format #t "`ARCH' set to `~a'~%" (getenv "ARCH"))

               (when target
                 (setenv "CROSS_COMPILE" (string-append target "-"))
                 (format #t "`CROSS_COMPILE' set to `~a'~%"
                         (getenv "CROSS_COMPILE"))))

             (setenv "EXTRA_VERSION" ,extra-version)

             (let ((build  (assoc-ref %standard-phases 'build))
                   (config (assoc-ref (or native-inputs inputs) "kconfig")))

               ;; Use a custom kernel configuration file or a default
               ;; configuration file.
               (if config
                   (begin
                     (copy-file config ".config")
                     (chmod ".config" #o666))
                   (invoke "make" ,defconfig))

               ;; Appending works even when the option wasn't in the
               ;; file.  The last one prevails if duplicated.
               (let ((port (open-file ".config" "a"))
                     (extra-configuration ,(config->string extra-options)))
                 (display extra-configuration port)
                 (close-port port))

               (invoke "make" "oldconfig"))))
         (replace 'install
           (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (moddir (string-append out "/lib/modules"))
                    (dtbdir (string-append out "/lib/dtbs")))
               ;; Install kernel image, kernel configuration and link map.
               (for-each (lambda (file) (install-file file out))
                         (find-files "." "^(\\.config|bzImage|zImage|Image|vmlinuz|System\\.map|Module\\.symvers)$"))
               ;; Install device tree files
               (unless (null? (find-files "." "\\.dtb$"))
                 (mkdir-p dtbdir)
                 (invoke "make" (string-append "INSTALL_DTBS_PATH=" dtbdir)
                         "dtbs_install"))
               ;; Install kernel modules
               (mkdir-p moddir)
               (invoke "make"
                       ;; Disable depmod because the Guix system's module directory
                       ;; is an union of potentially multiple packages.  It is not
                       ;; possible to use depmod to usefully calculate a dependency
                       ;; graph while building only one of those packages.
                       "DEPMOD=true"
                       (string-append "MODULE_DIR=" moddir)
                       (string-append "INSTALL_PATH=" out)
                       (string-append "INSTALL_MOD_PATH=" out)
                       "INSTALL_MOD_STRIP=1"
                       "modules_install")
               (let* ((versions (filter (lambda (name)
                                          (not (string-prefix? "." name)))
                                        (scandir moddir)))
                      (version (match versions
                                ((x) x))))
                 ;; There are symlinks to the build and source directory,
                 ;; both of which will point to target /tmp/guix-build*
                 ;; and thus not be useful in a profile.  Delete the symlinks.
                 (false-if-file-not-found
                  (delete-file (string-append moddir "/" version "/build")))
                 (false-if-file-not-found
                  (delete-file (string-append moddir "/" version "/source"))))
               #t))))
       #:tests? #f))
    (home-page "https://www.gnu.org/software/linux-libre/")
    (synopsis "100% free redistribution of a cleaned Linux kernel")
    (description
     "GNU Linux-Libre is a free (as in freedom) variant of the Linux kernel.
It has been modified to remove all non-free binary blobs.")
    (license license:gpl2)
    (properties '((max-silent-time . 3600))))) ;don't timeout on blob scan.


;;;
;;; Generic kernel packages.
;;;

(define-public linux-libre-5.11
  (make-linux-libre* linux-libre-5.11-version
                     linux-libre-5.11-source
                     '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux" "riscv64-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-version         linux-libre-5.11-version)
(define-public linux-libre-pristine-source linux-libre-5.11-pristine-source)
(define-public linux-libre-source          linux-libre-5.11-source)
(define-public linux-libre                 linux-libre-5.11)

(define-public linux-libre-5.10
  (make-linux-libre* linux-libre-5.10-version
                     linux-libre-5.10-source
                     '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux" "riscv64-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-5.4
  (make-linux-libre* linux-libre-5.4-version
                     linux-libre-5.4-source
                     '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux" "riscv64-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-4.19
  (make-linux-libre* linux-libre-4.19-version
                     linux-libre-4.19-source
                     '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-4.14
  (make-linux-libre* linux-libre-4.14-version
                     linux-libre-4.14-source
                     '("x86_64-linux" "i686-linux" "armhf-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-4.9
  (make-linux-libre* linux-libre-4.9-version
                     linux-libre-4.9-source
                     '("x86_64-linux" "i686-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-4.4
  (make-linux-libre* linux-libre-4.4-version
                     linux-libre-4.4-source
                     '("x86_64-linux" "i686-linux")
                     #:configuration-file kernel-config
                     #:extra-options
                     (append
                      `(;; https://lists.gnu.org/archive/html/guix-devel/2014-04/msg00039.html
                        ;; This option was removed upstream in version 4.7.
                        ("CONFIG_DEVPTS_MULTIPLE_INSTANCES" . #t))
                      %default-extra-linux-options)))

;; Linux-Libre-LTS points to the *newest* released long-term support version of
;; Linux-Libre.
;; Reference: https://jxself.org/linux-libre/

(define-public linux-libre-lts-version         linux-libre-5.10-version)
(define-public linux-libre-lts-pristine-source linux-libre-5.10-pristine-source)
(define-public linux-libre-lts-source          linux-libre-5.10-source)
(define-public linux-libre-lts                 linux-libre-5.10)


;;;
;;; Specialized kernel variants.
;;;

(define-public linux-libre-arm-generic
  (make-linux-libre* linux-libre-version
                     linux-libre-source
                     '("armhf-linux")
                     #:defconfig "multi_v7_defconfig"
                     #:extra-version "arm-generic"
                     #:extra-options
                     (append
                      `(;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t))
                      %default-extra-linux-options)))

(define-public linux-libre-arm-veyron
  (deprecated-package "linux-libre-arm-veyron" linux-libre-arm-generic))

(define-public linux-libre-arm-generic-5.10
  (make-linux-libre* linux-libre-5.10-version
                     linux-libre-5.10-source
                     '("armhf-linux")
                     #:defconfig "multi_v7_defconfig"
                     #:extra-version "arm-generic"
                     #:extra-options
                     (append
                      `(;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t))
                      %default-extra-linux-options)))

(define-public linux-libre-arm-generic-5.4
  (make-linux-libre* linux-libre-5.4-version
                     linux-libre-5.4-source
                     '("armhf-linux")
                     #:defconfig "multi_v7_defconfig"
                     #:extra-version "arm-generic"
                     #:extra-options
                     (append
                      `(;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t))
                      %default-extra-linux-options)))

(define-public linux-libre-arm-generic-4.19
  (make-linux-libre* linux-libre-4.19-version
                     linux-libre-4.19-source
                     '("armhf-linux")
                     #:defconfig "multi_v7_defconfig"
                     #:extra-version "arm-generic"))

(define-public linux-libre-arm-generic-4.14
  (make-linux-libre* linux-libre-4.14-version
                     linux-libre-4.14-source
                     '("armhf-linux")
                     #:defconfig "multi_v7_defconfig"
                     #:extra-version "arm-generic"))

(define-public linux-libre-arm-omap2plus
  (make-linux-libre* linux-libre-version
                     linux-libre-source
                     '("armhf-linux")
                     #:defconfig "omap2plus_defconfig"
                     #:extra-version "arm-omap2plus"))

(define-public linux-libre-arm-omap2plus-4.19
  (make-linux-libre* linux-libre-4.19-version
                     linux-libre-4.19-source
                     '("armhf-linux")
                     #:defconfig "omap2plus_defconfig"
                     #:extra-version "arm-omap2plus"))

(define-public linux-libre-arm-omap2plus-4.14
  (make-linux-libre* linux-libre-4.14-version
                     linux-libre-4.14-source
                     '("armhf-linux")
                     #:defconfig "omap2plus_defconfig"
                     #:extra-version "arm-omap2plus"))

(define-public linux-libre-arm64-generic
  (make-linux-libre* linux-libre-version
                     linux-libre-source
                     '("aarch64-linux")
                     #:defconfig "defconfig"
                     #:extra-version "arm64-generic"
                     #:extra-options
                     (append
                      `(;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t)
                        ;; Pinebook display, battery, charger and usb
                        ("CONFIG_DRM_ANALOGIX_ANX6345" . m)
                        ("CONFIG_CHARGER_AXP20X" . m)
                        ("CONFIG_INPUT_AXP20X_PEK" . m)
                        ("CONFIG_CHARGER_AXP20X" . m)
                        ("CONFIG_BATTERY_AXP20X" . m)
                        ("CONFIG_PINCTRL_AXP209" . m)
                        ("CONFIG_AXP20X_POWER" . m)
                        ("CONFIG_AXP20X_ADC" . m)
                        ;; Pinebook PRO battery and sound support
                        ("CONFIG_BATTERY_CW2015" . m)
                        ("CONFIG_CHARGER_GPIO" . m)
                        ("CONFIG_SND_SOC_ES8316" . m))
                      %default-extra-linux-options)))

(define-public linux-libre-arm64-generic-5.10
  (make-linux-libre* linux-libre-5.10-version
                     linux-libre-5.10-source
                     '("aarch64-linux")
                     #:defconfig "defconfig"
                     #:extra-version "arm64-generic"
                     #:extra-options
                     (append
                      `(;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t)
                        ;; Pinebook display, battery, charger and usb
                        ("CONFIG_DRM_ANALOGIX_ANX6345" . m)
                        ("CONFIG_CHARGER_AXP20X" . m)
                        ("CONFIG_INPUT_AXP20X_PEK" . m)
                        ("CONFIG_CHARGER_AXP20X" . m)
                        ("CONFIG_BATTERY_AXP20X" . m)
                        ("CONFIG_PINCTRL_AXP209" . m)
                        ("CONFIG_AXP20X_POWER" . m)
                        ("CONFIG_AXP20X_ADC" . m)
                        ;; Pinebook PRO battery and sound support
                        ("CONFIG_BATTERY_CW2015" . m)
                        ("CONFIG_CHARGER_GPIO" . m)
                        ("CONFIG_SND_SOC_ES8316" . m))
                      %default-extra-linux-options)))

(define-public linux-libre-arm64-generic-5.4
  (make-linux-libre* linux-libre-5.4-version
                     linux-libre-5.4-source
                     '("aarch64-linux")
                     #:defconfig "defconfig"
                     #:extra-version "arm64-generic"
                     #:extra-options
                     (append
                      `(;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t))
                      %default-extra-linux-options)))

(define-public linux-libre-riscv64-generic
  (make-linux-libre* linux-libre-version
                     linux-libre-source
                     '("riscv64-linux")
                     #:extra-version "riscv64-generic"))

(define-public linux-libre-mips64el-fuloong2e
  (make-linux-libre* linux-libre-version
                     linux-libre-source
                     '("mips64el-linux")
                     #:defconfig "fuloong2e_defconfig"
                     #:extra-version "mips64el-fuloong2e"
                     #:extra-options
                     (append
                      `(("CONFIG_OVERLAY_FS" . m))
                      %default-extra-linux-options)))

(define-public linux-libre-with-bpf
  (let ((base-linux-libre
         (make-linux-libre*
          linux-libre-5.11-version
          linux-libre-5.11-source
          '("x86_64-linux" "i686-linux" "armhf-linux"
            "aarch64-linux" "riscv64-linux")
          #:extra-version "bpf"
          #:configuration-file kernel-config
          #:extra-options
          (append %bpf-extra-linux-options
                  %default-extra-linux-options))))
    (package
      (inherit base-linux-libre)
      (inputs `(("cpio" ,cpio) ,@(package-inputs base-linux-libre))))))



;;;
;;; Linux kernel modules.
;;;

(define-public acpi-call-linux-module
  (package
    (name "acpi-call-linux-module")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nix-community/acpi_call")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mr4rjbv6fj4phf038addrgv32940bphghw2v9n1z4awvw7wzkbg"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'patch-shebangs-harder
           ;; The (only) shebangs in examples/ don't justify a reference.
           ;; However, do substitute a more portable one than the original.
           (lambda _
             (substitute* (find-files "examples" ".")
               (("^(#! *)/[^ ]*/" _ shebang)
                (string-append shebang "/usr/bin/env ")))
             #t))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (for-each (lambda (file)
                           (let ((target (string-append doc "/" file)))
                             (mkdir-p (dirname target))
                             (copy-recursively file target)))
                         (list "README.md" "examples"))
               #t))))))
    (home-page "https://github.com/teleshoes/acpi_call")
    (synopsis "Linux kernel module to perform ACPI method calls")
    (description
     "This simple Linux kernel module allows calls from user space to any
@acronym{ACPI, Advanced Configuration and Power Interface} method provided by
your computer's firmware, by writing to @file{/proc/acpi/call}.  You can pass
any number of parameters of types @code{ACPI_INTEGER}, @code{ACPI_STRING},
and @code{ACPI_BUFFER}.

It grants direct and undocumented access to your hardware that may cause damage
and should be used with caution, especially on untested models.")
    (license license:gpl3+)))           ; see README.md (no licence headers)

(define-public rtl8812au-aircrack-ng-linux-module
  (let ((commit "059e06a51be025fde5b2bec6565540b3d9981b0b")
        (revision "4"))
    (package
      (name "rtl8812au-aircrack-ng-linux-module")
      (version (git-version "5.6.4.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aircrack-ng/rtl8812au")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0h6l2r3yj7j9zf11dw0zcdyn50ajnjw8yvv86dzlfj80dn75n98f"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Remove bundled tarballs, APKs, word lists, speadsheets,
             ;; and other unnecessary unlicenced things.
             (for-each delete-file-recursively (list "android"
                                                     "docs"
                                                     "tools"))
             #t))))
      (build-system linux-module-build-system)
      (arguments
       `(#:make-flags
         (list (string-append "KSRC="
                              (assoc-ref %build-inputs "linux-module-builder")
                              "/lib/modules/build"))
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key (make-flags '()) #:allow-other-keys)
               (apply invoke "make" make-flags))))
         #:tests? #f))                  ; no test suite
      (supported-systems '("x86_64-linux" "i686-linux"))
      (home-page "https://github.com/aircrack-ng/rtl8812au")
      (synopsis "Linux driver for Realtek USB wireless network adapters")
      (description
       "This is Realtek's rtl8812au Linux driver for USB 802.11n wireless
network adapters, modified by the aircrack-ng project to support monitor mode
and frame injection.  It provides a @code{88XXau} kernel module that supports
RTL8812AU, RTL8821AU, and RTL8814AU chips.")
      (license license:gpl2+))))

(define-public rtl8821ce-linux-module
  (let ((commit "14b536f0c9ad2d0abbdab8afc7ade684900ca9cf")
        (revision "2"))
    (package
      (name "rtl8821ce-linux-module")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tomaspinho/rtl8821ce")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0z7r7spsgn22gwv9pcmkdjn9ingi8jj7xkxasph8118h46fw8ip2"))))
      (build-system linux-module-build-system)
      (arguments
       `(#:make-flags
         (list "CC=gcc"
               (string-append "KSRC="
                              (assoc-ref %build-inputs "linux-module-builder")
                              "/lib/modules/build"))
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key (make-flags '()) #:allow-other-keys)
               (apply invoke "make" make-flags))))
         #:tests? #f))                  ; no test suite
      (home-page "https://github.com/tomaspinho/rtl8821ce")
      (synopsis "Linux driver for Realtek RTL8821CE wireless network adapters")
      (description "This is Realtek's RTL8821CE Linux driver for wireless
network adapters.")
      (license license:gpl2))))

(define-public vhba-module
  (package
    (name "vhba-module")
    (version "20200106")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://downloads.sourceforge.net/cdemu/vhba-module/vhba-module-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "10rlvsfj0fw6n0qmwcnvhimqnsnhi7n55lyl7fq1pkwggf5218sr"))))
    (build-system linux-module-build-system)
    (arguments
     ;; TODO: No tests?
     `(#:tests? #f))
    (home-page "https://cdemu.sourceforge.io/")
    (synopsis "Kernel module that emulates SCSI devices")
    (description "VHBA module provides a Virtual (SCSI) HBA, which is the link
between the CDemu userspace daemon and linux kernel.")
    (license license:gpl2+)))

(define-public bbswitch-module
  ;; Use "develop" branch since stable release does not build on Linux >= 5.6.
  ;; See https://github.com/Bumblebee-Project/bbswitch/issues/205.
  (let ((commit "ddbd243638c7bc2baecf43a78aff46cdc12e9b2e"))
    (package
      (name "bbswitch-module")
      (version (git-version "0.8" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Bumblebee-Project/bbswitch")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pgldnza7mzd0flrxg4q69dwbq1fhl58m5c62ary5drb0xyf3lqb"))))
      (build-system linux-module-build-system)
      (arguments
       ;; No tests.
       `(#:tests? #f))
      (home-page "https://github.com/Bumblebee-Project/bbswitch")
      (synopsis "Kernel module that disables discrete Nvidia graphics cards")
      (description "The bbswitch module provides a way to toggle the Nvidia
graphics card on Optimus laptops.")
      (license license:gpl2))))

(define-public ddcci-driver-linux
  (package
    (name "ddcci-driver-linux")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.com/ddcci-driver-linux/ddcci-driver-linux.git")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vkkja3ykjil783zjpwp0vz7jy2fp9ccazzi3afd4fjk8gldin7f"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:tests? #f                               ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda args
             (for-each
              (lambda (module)
                (with-directory-excursion module
                  (apply (assoc-ref %standard-phases 'build) args)))
              '("ddcci" "ddcci-backlight"))
             #t))
         (replace 'install
           (lambda args
             (for-each
              (lambda (module)
                (with-directory-excursion module
                  (apply (assoc-ref %standard-phases 'install) args)))
              '("ddcci" "ddcci-backlight"))
             #t)))))
    (home-page "https://gitlab.com/ddcci-driver-linux/ddcci-driver-linux")
    (synopsis "Pair of Linux kernel drivers for DDC/CI monitors")
    (description "This package provides two Linux kernel drivers, ddcci and
ddcci-backlight, that allows the control of DDC/CI monitors through the sysfs
interface.  The ddcci module creates a character device for each DDC/CI
monitors in @file{/dev/bus/ddcci/[I²C busnumber]}.  While the ddcci-backlight
module allows the control of the backlight level or luminance property when
supported under @file{/sys/class/backlight/}.")
    (license license:gpl2+)))

(define-public v4l2loopback-linux-module
  (package
    (name "v4l2loopback-linux-module")
    (version "0.12.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/umlaeute/v4l2loopback")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qi4l6yam8nrlmc3zwkrz9vph0xsj1cgmkqci4652mbpbzigg7vn"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:tests? #f))                    ; no test suite
    (home-page "https://github.com/umlaeute/v4l2loopback")
    (synopsis "Linux kernel module to create virtual V4L2 video devices")
    (description
     "This Linux module creates virtual video devices.  @acronym{V4L2, Video
for Linux 2} applications will treat these as ordinary video devices but read
video data generated by another application, instead of a hardware device such
as a capture card.

This lets you apply nifty effects to your Jitsi video, for example, but also
allows some more serious things like adding streaming capabilities to an
application by hooking GStreamer into the loopback device.")
    (license license:gpl2+)))


;;;
;;; Pluggable authentication modules (PAM).
;;;

(define-public linux-pam
  (package
    (name "linux-pam")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/linux-pam/linux-pam/releases/download/v"
             version "/Linux-PAM-" version ".tar.xz"))
       (sha256
        (base32
         "1z4jayf69qyyxln1gl6ch4qxfd66ib1g42garnrv2d8i1drl0790"))
       (patches (search-patches "linux-pam-no-setfsuid.patch"))))

    (build-system gnu-build-system)
    (native-inputs
     `(("flex" ,flex)

       ;; TODO: optional dependencies
       ;; ("libxcrypt" ,libxcrypt)
       ;; ("cracklib" ,cracklib)
       ))
    (arguments
     `(;; Most users, such as `shadow', expect the headers to be under
       ;; `security'.
       #:configure-flags (list (string-append "--includedir="
                                              (assoc-ref %outputs "out")
                                              "/include/security")

                               ;; XXX: <rpc/rpc.h> is missing from glibc when
                               ;; cross-compiling, so we have to disable NIS
                               ;; support altogether.
                               ,@(if (%current-target-system)
                                     '("--disable-nis")
                                     '()))

       ;; XXX: Tests won't run in chroot, presumably because /etc/pam.d
       ;; isn't available.
       #:tests? #f))
    (home-page "http://www.linux-pam.org/")
    (synopsis "Pluggable authentication modules for Linux")
    (description
     "A *Free* project to implement OSF's RFC 86.0.
Pluggable authentication modules are small shared object files that can
be used through the PAM API to perform tasks, like authenticating a user
at login.  Local and dynamic reconfiguration are its key features.")
    (license license:bsd-3)))

(define-public linux-pam-1.2
  (package
    (inherit linux-pam)
    (version "1.2.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "http://www.linux-pam.org/library/"
            "Linux-PAM-" version ".tar.bz2"))
      (sha256
       (base32
        "1n9lnf9gjs72kbj1g354v1xhi2j27aqaah15vykh7cnkq08i4arl"))
      (patches (search-patches "linux-pam-no-setfsuid.patch"))))))

(define-public python-pamela
  (package
    (name "python-pamela")
    (version "1.0.0")
    (source
      (origin
        ;; Tests not distributed in pypi release.
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/minrk/pamela")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0cg3w6np1fbjpvzhv54xg567hpf38szwp2d4gvzb9r736nxbv0vr"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; Test suite isn't designed to be run inside a container.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'hardcode-pam.so
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pam (assoc-ref inputs "linux-pam")))
               (substitute* "pamela.py"
                 (("find_library\\(\"pam\")")
                  (string-append "'" pam "/lib/libpam.so'")))
               #t)))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
               (if (file-exists? "test_pamela.py")
                 (invoke "py.test" "--assert=plain" "test_pamela.py")
                 (invoke "python" "-m" "pamela" "-a" "`whoami`"))
               #t))))))
    (inputs
     `(("linux-pam" ,linux-pam)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/minrk/pamela")
    (synopsis "PAM interface using ctypes")
    (description "This package provides a PAM interface using @code{ctypes}.")
    (license license:expat)))


;;;
;;; Miscellaneous.
;;;

(define-public powerstat
  (package
    (name "powerstat")
    (version "0.02.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://kernel.ubuntu.com/~cking/tarballs/"
                           "powerstat/powerstat-" version ".tar.gz"))
       (sha256
        (base32 "0dmixbxm4qd08ds26i0wvxwyy8nrjzfjj2q9ylx35564g4wh58qb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'respect-$prefix
           ;; https://bugs.launchpad.net/ubuntu/+source/powerstat/+bug/1877744
           (lambda _
             (substitute* "Makefile"
               (("DIR=/usr/") "DIR=$(prefix)/"))
             #t))
         (delete 'configure))))         ; no configure script
    (home-page "https://kernel.ubuntu.com/~cking/powerstat/")
    (synopsis "Measure system power consumption")
    (description
     "Powerstat measures and reports your computer's power consumption in real
time.  On mobile PCs, it uses ACPI battery information to measure the power
drain of the entire system.

Powerstat can also report @acronym{RAPL, Running Average Power Limit} power
domain measurements.  These are available only on some hardware such as Intel
Sandybridge and newer, and cover only part of the machine's components such as
CPU, DRAM, and graphics.  However, they provide accurate and immediate readings
and don't require a battery at all.

The output is like @command{vmstat} but also shows power consumption statistics:
at the end of a run, @command{powerstat} will calculate the average, standard
deviation, and minimum and maximum values.  It can show a nice histogram too.")
    (license license:gpl2)))

(define-public psmisc
  (package
    (name "psmisc")
    (version "23.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/psmisc/psmisc/psmisc-"
                          version ".tar.xz"))
      (sha256
       (base32 "0y8n1jd2dn4cvc5mh806d66qnq8xl0xmzczbycjwal10rvmcw33z"))))
    (build-system gnu-build-system)
    (arguments
     `(,@(if (%current-target-system)
             '(#:configure-flags
               (list
                "ac_cv_func_malloc_0_nonnull=yes"
                "ac_cv_func_realloc_0_nonnull=yes"))
             '())))
    (inputs `(("ncurses" ,ncurses)))
    (home-page "https://gitlab.com/psmisc/psmisc")
    (synopsis "Small utilities that use the proc file system")
    (description
     "This PSmisc package is a set of some small useful utilities that
use the proc file system.  We're not about changing the world, but
providing the system administrator with some help in common tasks.")
    (license license:gpl2+)))

(define-public util-linux
  (package
    (name "util-linux")
    (version "2.35.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/utils/"
                                  "util-linux/v" (version-major+minor version) "/"
                                  "util-linux-" version ".tar.xz"))
              (sha256
               (base32
                "1yfpy6bkab4jw61mpx48gfy24yrqp4a7arvpis8csrkk53fkxpnr"))
              (patches (search-patches "util-linux-tests.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; We take 'nologin' from Shadow, the 'logger' program from
               ;; GNU Inetutils and 'kill' from GNU Coreutils.
               '(begin
                  (substitute* "configure"
                    (("build_nologin=yes") "build_nologin=no")
                    (("build_logger=yes") "build_logger=no")
                    (("build_kill=yes") "build_kill=no"))
                  #t))))
    (build-system gnu-build-system)
    (outputs '("out"            ;6.4 MiB executables and documentation
               "lib"            ;8.8 MiB shared libraries, headers and locales
               "static"))       ;2.9 MiB static .a libraries
    (arguments
     `(#:configure-flags (list "--disable-use-tty-group"
                               "--enable-fs-paths-default=/run/current-system/profile/sbin"
                               ;; Don't try to chown root:root mount and umount
                               "--disable-makeinstall-chown"
                               "--localstatedir=/var"
                               (string-append "--localedir="
                                              (assoc-ref %outputs "lib")
                                              "/share/locale")
                               ;; Install completions where our
                               ;; bash-completion package expects them.
                               (string-append "--with-bashcompletiondir="
                                              (assoc-ref %outputs "out")
                                              "/etc/bash_completion.d"))

       ;; FIXME: For now we cannot reliably run tests on GNU/Hurd:
       ;; <https://bugs.gnu.org/47791>.
       #:tests? ,(and (not (%current-target-system))
                      (not (string-suffix? "-gnu" (%current-system))))

       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'patch-build-scripts
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "configure"
                        ;; The build system assumes that we want to install
                        ;; libraries below $exec_prefix when $libdir does not
                        ;; match any of the "usual" locations.  Fix that.
                        (("usrlib_execdir='\\$\\{exec_prefix\\}'\\$libdir")
                         "usrlib_execdir=$libdir"))
                      #t))
                  (add-before 'build 'set-umount-file-name
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Tell 'eject' the right file name of 'umount'.
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "sys-utils/eject.c"
                          (("\"/bin/umount\"")
                           (string-append "\"" out "/bin/umount\"")))
                        #t)))
                  (add-before 'check 'pre-check
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (net (assoc-ref inputs "net-base")))
                        ;; Change the test to refer to the right file.
                        (substitute* "tests/ts/misc/mcookie"
                          (("/etc/services")
                           (string-append net "/etc/services")))

                        ;; The C.UTF-8 locale does not exist in our libc.
                        (substitute* "tests/ts/column/invalid-multibyte"
                          (("C\\.UTF-8") "en_US.utf8"))
                        #t)))
                  ;; TODO: Remove the conditional on the next rebuild cycle.
                  ,@(if (string-prefix? "arm" (%current-system))
                        '((add-before 'check 'disable-setarch-test
                            (lambda _
                              ;; The setarch tests are unreliable in QEMU's
                              ;; user-mode emulation, which is our primary
                              ;; method of building ARMv7 packages.
                              ;; <https://github.com/karelzak/util-linux/issues/601>
                              (substitute* "tests/ts/misc/setarch"
                                (("ts_init_subtest.*" all)
                                 (string-append
                                  all "\n"
                                  "ts_skip \"setarch tests are unreliable under QEMU\"")))
                              #t)))
                        '())
                  (add-after 'install 'move-static-libraries
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((lib    (assoc-ref outputs "lib"))
                            (static (assoc-ref outputs "static")))

                        ;; Move static libraries to the "static" output.
                        (mkdir-p (string-append static "/lib"))
                        (with-directory-excursion lib
                          (for-each (lambda (file)
                                      (rename-file file
                                                   (string-append static "/"
                                                                  file)))
                                    (find-files "lib" "\\.a$"))

                          ;; Remove references to the static library from the '.la'
                          ;; files so that Libtool does the right thing when both
                          ;; the shared and static library is available.
                          (substitute* (find-files "lib" "\\.la$")
                            (("old_library=.*") "old_library=''\n")))

                        #t)))
                  (add-after 'install 'adjust-pkg-config-files
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((lib (assoc-ref outputs "lib")))
                        ;; Drop the unused "prefix=" and "exec_prefix=" variables from
                        ;; the pkg-config files to avoid a cyclic reference on "out".
                        (substitute* (find-files (string-append lib "/lib/pkgconfig")
                                                 "\\.pc$")
                          (("^(exec_)?prefix=.*") "")))
                        #t)))))
    (inputs `(("zlib" ,zlib)
              ("ncurses" ,ncurses)

              ;; XXX: This is so that the 'pre-check' phase can find it.
              ,@(if (%current-target-system)
                    `(("net-base" ,net-base))
                    '())))
    (native-inputs
     `(("perl" ,perl)
       ("net-base" ,net-base)))         ;for tests
    (home-page "https://www.kernel.org/pub/linux/utils/util-linux/")
    (synopsis "Collection of utilities for the Linux kernel")
    (description "Util-linux is a diverse collection of Linux kernel
utilities.  It provides dmesg and includes tools for working with file systems,
block devices, UUIDs, TTYs, and many other tools.")

    ;; Note that util-linux doesn't use the same license for all the
    ;; code.  GPLv2+ is the default license for a code without an
    ;; explicitly defined license.
    (license (list license:gpl3+ license:gpl2+ license:gpl2 license:lgpl2.0+
                   license:bsd-4 license:public-domain))))

;; util-linux optionally supports udev, which allows lsblk to read file system
;; metadata without special privileges.  Add it as a separate package to avoid
;; a circular dependency, and to keep the size small.
(define-public util-linux+udev
  (package/inherit
   util-linux
   (name "util-linux-with-udev")
   (inputs
    `(("udev" ,eudev)
      ,@(package-inputs util-linux)))))

(define-public ddate
  (package
    (name "ddate")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/bo0ts/ddate")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1qchxnxvghbma6gp1g78wnjxsri0b72ha9axyk31cplssl7yn73f"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/bo0ts/ddate")
    (synopsis "PERPETUAL DATE CONVERTER FROM GREGORIAN TO POEE CALENDAR")
    (description
     "ddate displays the Discordian date and holidays of a given date.
The Discordian calendar was made popular by the \"Illuminatus!\" trilogy
by Robert Shea and Robert Anton Wilson.")
    (license license:public-domain)))

(define-public fbset
  (package
    (name "fbset")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://users.telenet.be/geertu/Linux/fbdev/fbset-"
                           version ".tar.gz"))
       (sha256
        (base32 "080wnisi0jq7dp0jcwdp83rq8q8s3kw41vc712516xbv4jq4mzs0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'install 'pre-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("mknod ") "true ")
                 ;; The Makefile doesn't honour PREFIX or similar.
                 (("/usr") out))
               (mkdir out)
               (with-directory-excursion out
                 (for-each mkdir-p (list "sbin"
                                         "man/man5"
                                         "man/man8")))
               #t)))
         (add-after 'install 'install-fb.modes
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (etc (string-append out "/etc")))
               (for-each (cut install-file <> etc)
                         (find-files "etc" "^fb\\.modes"))
               (symlink "fb.modes.ATI"
                        (string-append etc "/fb.modes"))
               #t))))
       ;; Parallel building races to create modes.tab.c.
       #:parallel-build? #f
       #:tests? #f))                    ; no test suite
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (home-page "http://users.telenet.be/geertu/Linux/fbdev/")
    (synopsis "Show and modify Linux frame buffer settings")
    (description
     "The kernel Linux's @dfn{frame buffers} provide a simple interface to
different kinds of graphic displays.  The @command{fbset} utility can query and
change various device settings such as depth, virtual resolution, and timing
parameters.")
    (license license:gpl2)))

(define-public procps
  (package
    (name "procps")
    (version "3.3.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/procps-ng/Production/"
                                  "procps-ng-" version ".tar.xz"))
              (sha256
               (base32
                "1br0g93ysqhlv13i1k4lfbimsgxnpy5rgs4lxfc9rkzdbpbaqplj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (srfi srfi-1)
                  (srfi srfi-26))
       ,@(if (%current-target-system)
             '(#:configure-flags
               (list
                "ac_cv_func_malloc_0_nonnull=yes"
                "ac_cv_func_realloc_0_nonnull=yes"))
             '())
       #:phases
       (modify-phases %standard-phases
         (add-after
          'install 'post-install
          ;; Remove commands and man pages redudant with
          ;; Coreutils.
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (dup (append-map (cut find-files out <>)
                                    '("^kill" "^uptime"))))
              (for-each delete-file dup)
              #t))))))
    (inputs `(("ncurses" ,ncurses)))
    (home-page "https://gitlab.com/procps-ng/procps/")
    (synopsis "Utilities that give information about processes")
    (description
     "Procps is the package that has a bunch of small useful utilities
that give information about processes using the Linux /proc file system.
The package includes the programs free, pgrep, pidof, pkill, pmap, ps, pwdx,
slabtop, tload, top, vmstat, w, watch and sysctl.")
    (license license:gpl2)))

(define-public usbutils
  (package
    (name "usbutils")
    (version "013")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kernel.org/linux/utils/usb/usbutils/"
                          "usbutils-" version ".tar.xz"))
      (sha256
       (base32 "0f0klk6d3hmbpf6p4dcwa1qjzblmkhbxs1wsw87aidvqri7lj8wy"))))
    (build-system gnu-build-system)
    (outputs (list "out" "python"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'patch-bootstrap-scripts
           (lambda _
             (substitute* "usbhid-dump/bootstrap"
               (("/bin/sh") (which "sh")))

             ;; Don't let autogen.sh run configure with bogus options & CFLAGS.
             (substitute* "autogen.sh"
               (("^\\./configure.*") ""))
             #t))
         (add-after 'install 'separate-python-output
           ;; Separating one Python script shaves more than 106 MiB from :out.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out        (assoc-ref outputs "out"))
                   (out:python (assoc-ref outputs "python")))
               (for-each (lambda (file)
                           (let ((old (string-append out "/" file))
                                 (new (string-append out:python "/" file)))
                             (mkdir-p (dirname new))
                             (rename-file old new)))
                         (list "bin/lsusb.py"))
               #t))))))
    (inputs
     `(("eudev" ,eudev)
       ("libusb" ,libusb)
       ("python" ,python)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.linux-usb.org/")
    (synopsis
     "Tools for working with USB devices, such as lsusb")
    (description
     "Tools for working with USB devices, such as lsusb.")
    (license license:gpl2+)))

(define-public e2fsprogs
  (package
    (name "e2fsprogs")
    (version "1.45.6")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kernel.org/linux/kernel/people/tytso/"
                   "e2fsprogs/v" version "/"
                   "e2fsprogs-" version ".tar.xz"))
             (sha256
              (base32
               "0mj2yizwygs7xww8jfy5mxjn8ww4pvc0b1hg1p2vsnirailsx9zz"))))
    (build-system gnu-build-system)
    (inputs `(("util-linux" ,util-linux "lib")))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("texinfo" ,texinfo)       ;for the libext2fs Info manual

                     ;; For tests.
                     ("perl" ,perl)
                     ("procps" ,procps)))
    (arguments
     '(;; util-linux is the preferred source for some of the libraries and
       ;; commands, so disable them (see, e.g.,
       ;; <http://git.buildroot.net/buildroot/commit/?id=e1ffc2f791b33633>.)
       #:configure-flags (list "--disable-libblkid"
                               "--disable-libuuid" "--disable-uuidd"
                               "--disable-fsck"

                               ;; Use symlinks instead of hard links for
                               ;; 'fsck.extN' etc.  This makes the resulting nar
                               ;; smaller and is preserved across copies.
                               "--enable-symlink-install"

                               (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib")

                               ;; Install libext2fs et al.
                               "--enable-elf-shlibs")

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-shells
           (lambda _
             (substitute* "configure"
               (("/bin/sh (.*)parse-types.sh" _ dir)
                (string-append (which "sh") " " dir
                               "parse-types.sh")))
             (substitute* "MCONFIG.in"
               (("INSTALL_SYMLINK = /bin/sh")
                "INSTALL_SYMLINK = sh"))
             (substitute* (find-files "." "^Makefile.in$")
               (("#!/bin/sh")
                (string-append "#!" (which "sh"))))
             #t))
           (add-after 'install 'install-libs
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib")))
                 (invoke "make" "install-libs")

                 ;; Make the .a writable so that 'strip' works.
                 ;; Failing to do that, due to debug symbols, we
                 ;; retain a reference to the final
                 ;; linux-libre-headers, which refer to the
                 ;; bootstrap binaries.
                 (let ((archives (find-files lib "\\.a$")))
                   (for-each (lambda (file)
                               (chmod file #o666))
                             archives))
                 #t))))))
    (home-page "http://e2fsprogs.sourceforge.net/")
    (synopsis "Creating and checking ext2/ext3/ext4 file systems")
    (description
     "This package provides tools for manipulating ext2/ext3/ext4 file systems.")
    (license (list license:gpl2                   ;programs
                   license:lgpl2.0                ;libext2fs
                   license:x11))))                ;libuuid

(define e2fsprogs/static
  (static-package
   (package (inherit e2fsprogs)
            (arguments
             ;; Do not build shared libraries.
             (substitute-keyword-arguments (package-arguments e2fsprogs)
               ((#:configure-flags _)
                '(list "--disable-blkid"))
               ((#:make-flags _)
                '(list)))))))

(define-public e2fsck/static
  (package
    (name "e2fsck-static")
    (version (package-version e2fsprogs))
    (build-system trivial-build-system)
    (source #f)
    (inputs
     `(("e2fsprogs" ,e2fsprogs/static)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (srfi srfi-26))

         (let ((e2fsck (string-append (assoc-ref %build-inputs "e2fsprogs")
                                      "/sbin/e2fsck"))
               (bin    (string-append (assoc-ref %outputs "out") "/sbin")))
           (mkdir-p bin)
           (with-directory-excursion bin
             (copy-file e2fsck "e2fsck")
             (remove-store-references "e2fsck")
             (chmod "e2fsck" #o555))
           #t))))
    (home-page (package-home-page e2fsprogs))
    (synopsis "Statically-linked e2fsck command from e2fsprogs")
    (description "This package provides statically-linked e2fsck command taken
from the e2fsprogs package.  It is meant to be used in initrds.")
    (license (package-license e2fsprogs))))

(define-public extundelete
  (package
    (name "extundelete")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/extundelete/"
                           "extundelete/" version "/extundelete-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "1x0r7ylxlp9lbj3d7sqf6j2a222dwy2nfpff05jd6mkh4ihxvyd1"))
       (patches (search-patches "extundelete-e2fsprogs-1.44.patch"))))
    (build-system gnu-build-system)
    (inputs `(("e2fsprogs" ,e2fsprogs)))
    (home-page "http://extundelete.sourceforge.net/")
    (synopsis "Recover deleted files from ext2/3/4 partitions")
    (description
     "Extundelete is a set of tools that can recover deleted files from an
ext3 or ext4 partition.")
    (license license:gpl2)))

(define-public zerofree
  (package
    (name "zerofree")
    (version "1.1.1")
    (home-page "https://frippery.org/uml/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page name "-" version
                                  ".tgz"))
              (sha256
               (base32
                "0rrqfa5z103ws89vi8kfvbks1cfs74ix6n1wb6vs582vnmhwhswm"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'install
           ;; The Makefile lacks an ‘install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (chmod "zerofree" #o555)
               (install-file "zerofree" bin)
               #t))))
       #:tests? #f))                    ; no tests
    (inputs `(("libext2fs" ,e2fsprogs)))
    (synopsis "Zero non-allocated regions in ext2/ext3/ext4 file systems")
    (description
     "Zerofree finds the unallocated blocks with non-zero value content in an
ext2, ext3, or ext4 file system and fills them with zeroes (or another value).
This is a simple way to make disk images more compressible.
Zerofree requires the file system to be unmounted or mounted read-only.")
    (license license:gpl2)))

(define-public strace
  (package
    (name "strace")
    (version "5.8")
    (home-page "https://strace.io")
    (source (origin
             (method url-fetch)
             (uri (string-append home-page "/files/" version
                                 "/strace-" version ".tar.xz"))
             (sha256
              (base32
               "1abs3svkg9985f4jrxx34sj1dcpsf95vv1a0g01c777zgygncjnz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (substitute* "strace.c"
               (("/bin/sh") (which "sh")))
             #t))
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             ;; XXX These hang forever even if the test time-out is extended.
             (substitute* "tests/Makefile.in"
               (("^\tstrace-DD?D?\\.test \\\\.*") ""))
             #t)))
       ;; Don't fail if the architecture doesn't support different personalities.
       #:configure-flags '("--enable-mpers=check")
       ;; See <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32459>.
       #:parallel-tests? #f))           ; undeterministic failures
    (native-inputs `(("perl" ,perl)))
    (synopsis "System call tracer for Linux")
    (description
     "strace is a system call tracer, i.e. a debugging tool which prints out a
trace of all the system calls made by a another process/program.")
    (license license:lgpl2.1+)))

(define-public ltrace
  (package
    (name "ltrace")
    (version "0.7.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.ltrace.org/ltrace_" version
                                 ".orig.tar.bz2"))
             (sha256
              (base32
               "00wmbdghqbz6x95m1mcdd3wd46l6hgcr4wggdp049dbifh3qqvqf"))))
    (build-system gnu-build-system)
    (inputs `(("libelf" ,elfutils)))
    (arguments
     ;; Compilation uses -Werror by default, but it fails.
     '(#:configure-flags '("--disable-werror")))
    (home-page "https://www.ltrace.org/")
    (synopsis "Library call tracer for Linux")
    (description
     "ltrace intercepts and records dynamic library calls which are called by
an executed process and the signals received by that process.  It can also
intercept and print the system calls executed by the program.")
    (license license:gpl2+)))

(define-public alsa-lib
  (package
    (name "alsa-lib")
    (version "1.2.4")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "ftp://ftp.alsa-project.org/pub/lib/alsa-lib-"
                   version ".tar.bz2"))
             (sha256
              (base32
               "1xq8d48wfy59qw4x7383j32n8j5njndw5hcgnmlg9pvclphlnmgp"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))))
    (home-page "https://www.alsa-project.org/")
    (synopsis "The Advanced Linux Sound Architecture libraries")
    (description
     "The Advanced Linux Sound Architecture (ALSA) provides audio and
MIDI functionality to the Linux-based operating system.")
    (license license:lgpl2.1+)))

(define-public alsa-utils
  (package
    (name "alsa-utils")
    (version "1.2.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp.alsa-project.org/pub/utils/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "09m4dnn4kplawprd2bl15nwa0b4r1brab3x44ga7f1fyk7aw5zwq"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: Disable man page creation until we have DocBook.
     '(#:configure-flags (list "--disable-xmlto"

                               ;; The udev rule is responsible for restoring
                               ;; the volume.
                               (string-append "--with-udev-rules-dir="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-broken-test
           (lambda _
             ;; XXX: The 1.1.8 release tarball is missing a header that's
             ;; required for this test to work.  Fixed in 1.1.9.
             (substitute* "axfer/test/Makefile"
               ((".*container-test.*") ""))
             #t))
         (add-before
           'install 'pre-install
           (lambda _
             ;; Don't try to mkdir /var/lib/alsa.
             (substitute* "Makefile"
               (("\\$\\(MKDIR_P\\) .*ASOUND_STATE_DIR.*")
                "true\n"))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("libsamplerate" ,libsamplerate)
       ("ncurses" ,ncurses)
       ("alsa-lib" ,alsa-lib)
       ("xmlto" ,xmlto)))
    (home-page "http://www.alsa-project.org/")
    (synopsis "Utilities for the Advanced Linux Sound Architecture (ALSA)")
    (description
     "The Advanced Linux Sound Architecture (ALSA) provides audio and
MIDI functionality to the Linux-based operating system.")

    ;; This is mostly GPLv2+ but a few files such as 'alsactl.c' are
    ;; GPLv2-only.
    (license license:gpl2)))

(define-public alsa-plugins
  (package
    (name "alsa-plugins")
    (version "1.2.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp.alsa-project.org/pub/plugins/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "0z9k3ssbfk2ky2w13avgyf202j1drsz9sv3834bp33cj1i2hc3qw"))))
    (build-system gnu-build-system)
    ;; TODO: Split libavcodec and speex if possible. It looks like they can not
    ;; be split, there are references to both in files.
    ;; TODO: Remove OSS related plugins, they add support to run native
    ;; ALSA applications on OSS however we do not offer OSS and OSS is
    ;; obsolete.
    (outputs '("out" "pulseaudio" "jack"))
    (native-search-paths
      (list (search-path-specification
              (variable "ALSA_PLUGIN_DIR")
              (files '("lib/alsa-lib"))
              (separator #f))))
    (arguments
     `(#:configure-flags '(;; Do not install a "local" configuration targeted
                           ;; for /etc/alsa.  On Guix System plugins are loaded from
                           ;; the ALSA service, and other distributions likely
                           ;; won't use these files.
                           "--with-alsalconfdir=/tmp/noop")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'split
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Distribute the binaries to the various outputs.
             (let* ((out (assoc-ref outputs "out"))
                    (jack (assoc-ref outputs "jack"))
                    (jacklib (string-append jack "/lib/alsa-lib"))
                    (pua (assoc-ref outputs "pulseaudio"))
                    (pualib (string-append pua "/lib/alsa-lib")))
               ;; For jack.
               (mkdir-p jacklib)
               (for-each (lambda (file)
                           (rename-file file (string-append jacklib "/" (basename file))))
                         (find-files out ".*jack\\.(la|so)"))
               ;; For pulseaudio.
               (mkdir-p pualib)
               (for-each (lambda (file)
                           (rename-file file (string-append pualib "/" (basename file))))
                         (find-files out ".*pulse\\.(la|so)"))
               #t))))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("speex" ,speex) ; libspeexdsp resampling plugin
       ("libsamplerate" ,libsamplerate) ; libsamplerate resampling plugin
       ("ffmpeg" ,ffmpeg) ; libavcodec resampling plugin, a52 plugin
       ("pulseaudio" ,pulseaudio))) ; PulseAudio plugin
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.alsa-project.org/")
    (synopsis "Plugins for the Advanced Linux Sound Architecture (ALSA)")
    (description
     "The Advanced Linux Sound Architecture (ALSA) provides audio and
MIDI functionality to the Linux-based operating system.  This package enhances ALSA
by providing additional plugins which include: upmixing, downmixing, jackd and
pulseaudio support for native alsa applications, format conversion (s16 to a52), and
external rate conversion.")
    (license (list license:gpl2+
                   ;; `rate/rate_samplerate.c': LGPL v2.1 or later.
                   license:lgpl2.1+))))

(define-public iptables
  (package
    (name "iptables")
    (version "1.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://netfilter.org/iptables/iptables-"
                                 version ".tar.bz2")
                  (string-append "https://www.netfilter.org/projects/iptables/"
                                 "files/iptables-" version ".tar.bz2")))
       (sha256
        (base32 "1w6qx3sxzkv80shk21f63rq41c84irpx68k62m2cv629n1mwj2f1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("flex" ,flex)
       ("bison" ,bison)))
    (inputs
     `(("libmnl" ,libmnl)
       ("libnftnl" ,libnftnl)))
    (arguments
     '(#:tests? #f       ; no test suite
       #:configure-flags ; add $libdir to the RUNPATH of executables
       (list (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))))
    (home-page "https://www.netfilter.org/projects/iptables/index.html")
    (synopsis "Programs to configure Linux IP packet filtering rules")
    (description
     "@command{iptables} is the user-space command line program used to
configure the Linux 2.4.x and later IPv4 packet filtering ruleset
(@dfn{firewall}), including @dfn{NAT} (Network Address Translation).

This package also includes @command{ip6tables}, which is used to configure the
IPv6 packet filter.

Both commands are targeted at system administrators.")
    (license license:gpl2+)))

(define-public jitterentropy-rngd
  (package
    (name "jitterentropy-rngd")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smuellerDD/jitterentropy-rngd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fb8zfwhwkl1d8n4cdn7rdv5rwd75qgc00d36pmkl7wgnj3c9xda"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "UNITDIR=$(PREFIX)/lib/systemd/system")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no ./configure script
    (home-page "https://www.chronox.de/jent.html")
    (synopsis "CPU jitter random number generator daemon")
    (description
     "This simple daemon feeds entropy from the CPU Jitter @acronym{RNG, random
number generator} core to the kernel Linux's entropy estimator.  This prevents
the @file{/dev/random} device from blocking and should benefit users of the
preferred @file{/dev/urandom} and @code{getrandom()} interfaces too.

The CPU Jitter RNG itself is part of the kernel and claims to provide good
entropy by collecting and magnifying differences in CPU execution time as
measured by the high-resolution timer built into modern CPUs.  It requires no
additional hardware or external entropy source.

The random bit stream generated by @command{jitterentropy-rngd} is not processed
by a cryptographically secure whitening function.  Nonetheless, its authors
believe it to be a suitable source of cryptographically secure key material or
other cryptographically sensitive data.

If you agree with them, start this daemon as early as possible to provide
properly seeded random numbers to services like SSH or those using TLS during
early boot when entropy may be low, especially in virtualised environments.")
    (license (list license:bsd-3        ; or
                   license:gpl2+))))

(define-public lsscsi
  (package
    (name "lsscsi")
    (version "0.31")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://sg.danny.cz/scsi/lsscsi-" version ".tar.xz"))
             (sha256
              (base32
               "1ry2y34xmpgxdbfbyvs8cjmbx0fn222yjdab87wj21q60nab5p75"))))
    (build-system gnu-build-system)
    (synopsis "Lists information about SCSI or NVMe devices in Linux")
    (home-page "http://sg.danny.cz/scsi/lsscsi.html")
    (description
     "@command{lsscsi} lists SCSI logical units or SCSI targets.  It can
also list NVMe namespaces or controllers and show the relationship between a
device's primary node name, its SCSI generic (sg) node name and its kernel
name.")
    (license license:gpl2)))

(define-public ebtables
  (package
    (name "ebtables")
    (version "2.0.11")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://netfilter.org/ebtables/ebtables-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0apxgmkhsk3vxn9q3libxn3dgrdljrxyy4mli2gk49m7hi3na7xp"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl)
       ("iptables" ,iptables)))
    (synopsis "Ethernet bridge frame table administration")
    (home-page "https://ebtables.netfilter.org/")
    (description
     "ebtables is an application program used to set up and maintain the
tables of rules (inside the Linux kernel) that inspect Ethernet frames.  It is
analogous to the iptables application, but less complicated, due to the fact
that the Ethernet protocol is much simpler than the IP protocol.")
    (license license:gpl2+)))

(define-public iproute
  (package
    (name "iproute2")
    (version "5.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/utils/net/iproute2/iproute2-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0vrjbzhwzcvaxyyhkr2ii89w2vznzwp2pfgk7w72mviniawqs9lx"))))
    (build-system gnu-build-system)
    (arguments
     `( ;; There is a test suite, but it wants network namespaces and sudo.
       #:tests? #f
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "DESTDIR="
                            (string-append "CC=" ,(cc-for-target))
                            "HOSTCC=gcc"
                            (string-append "BASH_COMPDIR=" out
                                           "/etc/bash_completion.d")
                            (string-append "LIBDIR=" out "/lib")
                            (string-append "HDRDIR=" out "/include")
                            (string-append "SBINDIR=" out "/sbin")
                            (string-append "CONFDIR=" out "/etc")
                            (string-append "DOCDIR=" out "/share/doc/"
                                           ,name "-" ,version)
                            (string-append "MANDIR=" out "/share/man")))
       #:phases (modify-phases %standard-phases
                  (add-before 'install 'pre-install
                    (lambda _
                      ;; Don't attempt to create /var/lib/arpd.
                      (substitute* "Makefile"
                        (("^.*ARPDDIR.*$") ""))
                      #t))
                  (add-after 'unpack 'patch-configure
                    (lambda _
                      (let ((target ,(%current-target-system)))
                        (substitute* "configure"
                          (("pkg-config")
                            (if target
                              (string-append target "-pkg-config")
                              "pkg-config")))
                        #t))))))
    (inputs
     `(("db4" ,bdb)
       ("iptables" ,iptables)
       ("libmnl" ,libmnl)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)))
    ;; For tests.
    ;; ("libmnl" ,libmnl)
    ;; ("util-linux" ,util-linux)
    (home-page
     "https://wiki.linuxfoundation.org/networking/iproute2")
    (synopsis
     "Utilities for controlling TCP/IP networking and traffic in Linux")
    (description
     "Iproute2 is a collection of utilities for controlling TCP/IP networking
and traffic with the Linux kernel.  The most important of these are
@command{ip}, which configures IPv4 and IPv6, and @command{tc} for traffic
control.

Most network configuration manuals still refer to ifconfig and route as the
primary network configuration tools, but ifconfig is known to behave
inadequately in modern network environments, and both should be deprecated.")
    (license license:gpl2+)))

(define-public net-tools
  ;; XXX: This package is basically unmaintained, but it provides a few
  ;; commands not yet provided by Inetutils, such as 'route', so we have to
  ;; live with it.
  (let ((commit "479bb4a7e11a4084e2935c0a576388f92469225b")
        (revision "0"))
    (package
      (name "net-tools")
      (version (string-append "1.60-" revision "." (string-take commit 7)))
      (source (origin
               (method url-fetch)
               (uri (string-append "https://sourceforge.net/code-snapshots/git/"
                                   "n/ne/net-tools/code.git/net-tools-code-"
                                   commit ".zip"))
               (file-name (string-append name "-" version ".zip"))
               (sha256
                (base32
                 "0hz9fda9d78spp774b6rr5xaxav7cm4h0qcpxf70rvdbrf6qx7vy"))))
      (home-page "http://net-tools.sourceforge.net/")
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-1)
                    (srfi srfi-26))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (mkdir-p (string-append out "/bin"))
                 (mkdir-p (string-append out "/sbin"))

                 ;; Pretend we have everything...
                 (system "yes | make config")

                 ;; ... except for the things we don't have.
                 ;; HAVE_AFDECnet requires libdnet, which we don't have.
                 ;; HAVE_HWSTRIP and HAVE_HWTR require kernel headers
                 ;; that have been removed.
                 ;; XXX SELINUX and AFBLUETOOTH are removed for now, but we should
                 ;; think about adding them later.
                 (substitute* '("config.make" "config.h")
                   (("^.*HAVE_(AFDECnet|HWSTRIP|HWTR|SELINUX|AFBLUETOOTH)[ =]1.*$")
                    ""))
                 #t)))
           (add-after 'install 'remove-redundant-commands
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Remove commands and man pages redundant with Inetutils.
               (let* ((out (assoc-ref outputs "out"))
                      (dup (append-map (cut find-files out <>)
                                       '("^hostname"
                                         "^(yp|nis|dns)?domainname"))))
                 (for-each delete-file dup)
                 #t))))
         ;; Binaries that depend on libnet-tools.a don't declare that
         ;; dependency, making it parallel-unsafe.
         #:parallel-build? #f

         #:tests? #f                                ; no test suite
         #:make-flags (let ((out (assoc-ref %outputs "out")))
                        (list ,(string-append "CC=" (cc-for-target))
                              (string-append "BASEDIR=" out)
                              (string-append "INSTALLNLSDIR=" out "/share/locale")
                              (string-append "mandir=/share/man")))))
      (native-inputs `(("gettext" ,gettext-minimal)
                       ("unzip" ,unzip)))
      (supported-systems (delete "i586-gnu" %supported-systems))
      (synopsis "Tools for controlling the network subsystem in Linux")
      (description
       "This package includes the important tools for controlling the network
subsystem of the Linux kernel.  This includes arp, ifconfig, netstat, rarp and
route.  Additionally, this package contains utilities relating to particular
network hardware types (plipconfig, slattach) and advanced aspects of IP
configuration (iptunnel, ipmaddr).")
      (license license:gpl2+))))

(define-public libcap-2.31
  (package
    (name "libcap")
    (version "2.31")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kernel.org/linux/libs/security/linux-privs/"
                   "libcap2/libcap-" version ".tar.xz"))
             (sha256
              (base32
               "0ikwm0kngrqa4ci80lqnrkk17kg09q7dxrz28y0gm5qw3vj8s266"))))
    (build-system gnu-build-system)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (replace 'configure
                            ;; Add $libdir to the RUNPATH of executables.
                            (lambda _
                              (substitute* "Make.Rules"
                                (("LDFLAGS := #-g")
                                 (string-append "LDFLAGS := -Wl,-rpath="
                                                %output "/lib")))
                              #t)))
                 #:test-target "test"
                 #:make-flags (list "lib=lib"
                                    (string-append "prefix="
                                                   (assoc-ref %outputs "out"))
                                    "RAISE_SETFCAP=no")))
    (native-inputs `(("perl" ,perl)))
    (supported-systems (delete "i586-gnu" %supported-systems))
    (home-page "https://sites.google.com/site/fullycapable/")
    (synopsis "Library for working with POSIX capabilities")
    (description
     "Libcap2 provides a programming interface to POSIX capabilities on
Linux-based operating systems.")

    ;; License is BSD-3 or GPLv2, at the user's choice.
    (license license:gpl2)))

;; libcap 2.31 has problems with newer kernels, so provide this newer variant.
;; Keep the old libcap around to avoid rebuilding 'coreutils' and 'avahi'.
;; To be merged with libcap on the next rebuild cycle.
(define-public libcap
  (package
    (inherit libcap-2.31)
    (version "2.45")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/libs/security/linux-privs/"
                    "libcap2/libcap-" version ".tar.xz"))
              (sha256
               (base32
                "11ijmi7jik9iw6pdszc6bylhggghr8cza03bcrbhbqf0cpvkjrnn"))))
    (arguments
     (substitute-keyword-arguments (package-arguments libcap-2.31)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'configure
             (lambda _
               ;; Add $libdir to the RUNPATH of executables.
               (substitute* "Make.Rules"
                 (("LDFLAGS \\?= #-g")
                  (string-append "LDFLAGS ?= -Wl,-rpath="
                                 %output "/lib")))
               #t))))))))

(define-deprecated libcap/next libcap)
(export libcap/next)

(define-public bridge-utils
  (package
    (name "bridge-utils")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kernel.org/linux/kernel/people/shemminger/"
                           "bridge-utils/bridge-utils-" version ".tar.xz"))
       (sha256
        (base32 "0zlrigizac2nfwgvksm92v4wafrpgxlbci3gwimc795ib7k8g6ck"))))
    (build-system gnu-build-system)

    ;; The tarball lacks all the generated files.
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'patch-stuff
           (lambda _
             ;; Fix "field ‘ip6’ has incomplete type" errors.
             (substitute* "libbridge/libbridge.h"
               (("#include <linux/if_bridge.h>")
                "#include <linux/in6.h>\n#include <linux/if_bridge.h>"))

             ;; Ensure that the entire build fails if one of the
             ;; sub-Makefiles fails.
             (substitute* "Makefile.in"
               (("\\$\\(MAKE\\) \\$\\(MFLAGS\\) -C \\$\\$x ;")
                "$(MAKE) $(MFLAGS) -C $$x || exit 1;"))

             #t)))
       #:tests? #f))                              ; no 'check' target

    (home-page "https://wiki.linuxfoundation.org/networking/bridge")
    (synopsis "Manipulate Ethernet bridges")
    (description
     "Utilities for Linux's Ethernet bridging facilities.  A bridge is a way
to connect two Ethernet segments together in a protocol independent way.
Packets are forwarded based on Ethernet address, rather than IP address (like
a router).  Since forwarding is done at Layer 2, all protocols can go
transparently through a bridge.")
    (license license:gpl2+)))

(define-public libnl
  (package
    (name "libnl")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/thom311/libnl/releases/download/"
                    "libnl" (string-join (string-split version #\.) "_")
                    "/libnl-" version ".tar.gz"))
              (sha256
               (base32
                "1yh5bqmkivd78x378x34gzb28lvykn6b9k3hgvvpdnj5jpn3689m"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ("swig" ,swig)
       ("libnl3-doc"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://github.com/thom311/libnl/releases/download/libnl"
                 (string-join (string-split version #\.) "_")
                 "/libnl-doc-" version ".tar.gz"))
           (sha256
            (base32 "19p5y8q3cm5wqvamqc4s5syxnnkvzxy3gw8ivxk6fv9ybn8jm35h"))))))
    (outputs `("out" "doc"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
             (let ((dest (string-append (assoc-ref outputs "doc")
                                        "/share/doc/libnl")))
               (mkdir-p dest)
               (invoke "tar" "xf" (assoc-ref
                                   (or native-inputs inputs)
                                   "libnl3-doc")
                       "--strip-components=1" "-C" dest)))))))
    (home-page "https://www.infradead.org/~tgr/libnl/")
    (synopsis "NetLink protocol library suite")
    (description
     "The libnl suite is a collection of libraries providing APIs to netlink
protocol based Linux kernel interfaces.  Netlink is an IPC mechanism primarily
between the kernel and user space processes.  It was designed to be a more
flexible successor to ioctl to provide mainly networking related kernel
configuration and monitoring interfaces.")

    ;; Most files are LGPLv2.1-only, but some are GPLv2-only (like
    ;; 'nl-addr-add.c'), so the result is GPLv2-only.
    (license license:gpl2)))

;; libnl python extensions used to be outputs of libnl. However, as
;; cross-compiling python extensions is currently broken, create separate
;; packages for libnl python extensions.
(define (libnl-python-package python)
  (let ((name (string-append "libnl-" python)))
    (package
      (inherit libnl)
      (name name)
      (inputs `(,@(cond
                   ((string=? python "python2")
                    `(("python-2" ,python-2)))
                   ((string=? python "python3")
                    `(("python-3" ,python-3))))))
      (propagated-inputs `(("libnl" ,libnl)))
      (outputs '("out"))
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-1))
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (define (python-inst python)
                 (invoke python "setup.py" "build")
                 (invoke python "setup.py" "install"
                         (string-append "--prefix="
                                        (assoc-ref %outputs "out")))
                 (invoke python "setup.py" "clean"))
               (setenv "LDFLAGS" (format #f "-Wl,-rpath=~a/lib"
                                         (assoc-ref inputs "libnl")))
               (with-directory-excursion "./python" (python-inst ,python))
               #t))))))))

(define-public libnl-python2 (libnl-python-package "python2"))
(define-public libnl-python3 (libnl-python-package "python3"))

(define-public iw
  (package
    (name "iw")
    (version "4.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/software/network/iw/iw-"
                    version ".tar.xz"))
              (sha256
               (base32
                "12ddd6vh6vs97135bnlyr0szv7hvpbnmfh48584frzab0z0725ph"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libnl" ,libnl)))
    (arguments
     `(#:make-flags
       (let* ((target ,(%current-target-system))
              (pkg-config (if target
                              (string-append target "-pkg-config")
                              "pkg-config")))
         (list
          ,(string-append "CC=" (cc-for-target))
          (string-append "PKG_CONFIG="
                         (assoc-ref %build-inputs "pkg-config")
                         "/bin/" pkg-config)
          (string-append "PREFIX=" (assoc-ref %outputs "out"))))
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (home-page "https://wireless.wiki.kernel.org/")
    (synopsis "Tool for configuring wireless devices")
    (description
     "iw is a new nl80211 based CLI configuration utility for wireless
devices.  It replaces @code{iwconfig}, which is deprecated.")
    (license license:isc)))

(define-public powertop
  (package
    (name "powertop")
    (version "2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://01.org/sites/default/files/downloads/"
                           "powertop-" version ".tar.gz"))
       (sha256
        (base32 "0y1ixw8v17fdb1ima0zshrd0rh4zxdh10r93nrrvq6d4lhn9jpx6"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; TODO: Patch some hardcoded "wlan0" in calibrate/calibrate.cpp to
         ;; allow calibrating the network interface in Guix System.
         (add-after 'unpack 'patch-absolute-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((kmod (assoc-ref inputs "kmod")))
               (substitute* (find-files "src" "\\.cpp$")
                 ;; Give the right 'modprobe' file name so that essential
                 ;; modules such as msr.ko can be loaded.
                 (("/sbin/modprobe") (string-append kmod "/bin/modprobe"))
                 ;; These programs are only needed to calibrate, so using
                 ;; relative file names avoids adding extra inputs.  When they
                 ;; are missing powertop gracefully handles it.
                 (("/usr/bin/hcitool") "hcitool")
                 (("/usr/bin/xset") "xset")
                 (("/usr/sbin/hciconfig") "hciconfig"))
               #t))))))
    (inputs
     `(("kmod" ,kmod)
       ("libnl" ,libnl)
       ("ncurses" ,ncurses)
       ("pciutils" ,pciutils)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://01.org/powertop/")
    (synopsis "Analyze power consumption on Intel-based laptops")
    (description
     "PowerTOP is a Linux tool to diagnose issues with power consumption and
power management.  In addition to being a diagnostic tool, PowerTOP also has
an interactive mode where the user can experiment various power management
settings for cases where the operating system has not enabled these
settings.")
    (license license:gpl2)))

(define-public aumix
  (package
    (name "aumix")
    (version "2.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.jpj.net/~trevor/aumix/releases/aumix-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0a8fwyxnc5qdxff8sl2sfsbnvgh6pkij4yafiln0fxgg6bal7knj"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://www.jpj.net/~trevor/aumix.html")
    (synopsis "Audio mixer for X and the console")
    (description
     "Aumix adjusts an audio mixer from X, the console, a terminal,
the command line or a script.")
    (license license:gpl2+)))

(define-public iotop
  (package
    (name "iotop")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://guichaz.free.fr/iotop/files/iotop-"
                           version ".tar.gz"))
       (sha256 (base32
                "1kp8mqg2pbxq4xzpianypadfxcsyfgwcaqgqia6h9fsq6zyh4z0s"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build-with-python3
           (lambda _
             (substitute* "setup.py"
               (("itervalues") "values"))
             #t)))
       ;; There are currently no checks in the package.
       #:tests? #f))
    (native-inputs `(("python" ,python)))
    (home-page "http://guichaz.free.fr/iotop/")
    (synopsis
     "Displays the IO activity of running processes")
    (description
     "Iotop is a Python program with a top like user interface to show the
processes currently causing I/O.")
    (license license:gpl2+)))

(define-public fuse
  (package
    (name "fuse")
    (version "2.9.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libfuse/libfuse/releases/"
                                  "download/fuse-" version
                                  "/fuse-" version ".tar.gz"))
              (sha256
               (base32
                "1ddlq6kzxilccgbvxjfx80jx6kamgw4sv49phks2zhlcc1frvrnh"))
              (patches (search-patches "fuse-overlapping-headers.patch"))))
    (build-system gnu-build-system)
    (inputs `(("util-linux" ,util-linux)))
    (arguments
     '(#:configure-flags (list (string-append "MOUNT_FUSE_PATH="
                                              (assoc-ref %outputs "out")
                                              "/sbin")
                               (string-append "INIT_D_PATH="
                                              (assoc-ref %outputs "out")
                                              "/etc/init.d")

                               ;; The rule makes /dev/fuse 666.
                               (string-append "UDEV_RULES_PATH="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             ;; libfuse calls out to mount(8) and umount(8).  Make sure
             ;; it refers to the right ones.
             (substitute* '("lib/mount_util.c" "util/mount_util.c")
               (("/bin/(u?)mount" _ maybe-u)
                (string-append (assoc-ref inputs "util-linux")
                               "/bin/" maybe-u "mount")))
             (substitute* '("util/mount.fuse.c")
               (("/bin/sh")
                (which "sh")))

             ;; This hack leads libfuse to search for 'fusermount' in
             ;; $PATH, where it may find a setuid-root binary, instead of
             ;; trying solely $out/sbin/fusermount and failing because
             ;; it's not setuid.
             (substitute* "lib/Makefile"
               (("-DFUSERMOUNT_DIR=[[:graph:]]+")
                "-DFUSERMOUNT_DIR=\\\"/var/empty\\\""))
             #t)))))
    (supported-systems (delete "i586-gnu" %supported-systems))
    (home-page "https://github.com/libfuse/libfuse")
    (synopsis "Support file systems implemented in user space")
    (description
     "As a consequence of its monolithic design, file system code for Linux
normally goes into the kernel itself---which is not only a robustness issue,
but also an impediment to system extensibility.  FUSE, for \"file systems in
user space\", is a kernel module and user-space library that tries to address
part of this problem by allowing users to run file system implementations as
user-space processes.")
    (license (list license:lgpl2.1                ;library
                   license:gpl2+))))              ;command-line utilities

(define-public unionfs-fuse
  (package
    (name "unionfs-fuse")
    (version "2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/rpodgorny/unionfs-fuse")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bwx70x834qgqh53vqp18bhbxbsny80hz922rbgj8k9wj7cbfilm"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("python" ,python)))
    (inputs `(("fuse" ,fuse)))
    (arguments
     ;; The tests were never actually run ("collected 0 items"), but in recent
     ;; versions of pytest that causes an error.
     '(#:tests? #f))
    (home-page "https://github.com/rpodgorny/unionfs-fuse")
    (synopsis "User-space union file system")
    (description
     "UnionFS-FUSE is a flexible union file system implementation in user
space, using the FUSE library.  Mounting a union file system allows you to
\"aggregate\" the contents of several directories into a single mount point.
UnionFS-FUSE additionally supports copy-on-write.")
    (license license:bsd-3)))

(define fuse-static
  (package (inherit fuse)
    (name "fuse-static")
    (source (origin (inherit (package-source fuse))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Normally libfuse invokes mount(8) so that /etc/mtab is
                  ;; updated.  Change calls to 'mtab_needs_update' to 0 so
                  ;; that it doesn't do that, allowing us to remove the
                  ;; dependency on util-linux (something that is useful in
                  ;; initrds.)
                  (substitute* '("lib/mount_util.c"
                                 "util/mount_util.c")
                    (("mtab_needs_update[[:blank:]]*\\([a-z_]+\\)")
                     "0")
                    (("/bin/")
                     ""))
                  #t))))))

(define-public unionfs-fuse/static
  (package (inherit unionfs-fuse)
    (synopsis "User-space union file system (statically linked)")
    (name (string-append (package-name unionfs-fuse) "-static"))
    (source (origin (inherit (package-source unionfs-fuse))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Add -ldl to the libraries, because libfuse.a needs that.
                  (substitute* "src/CMakeLists.txt"
                    (("target_link_libraries(.*)\\)" _ libs)
                     (string-append "target_link_libraries"
                                    libs " dl)")))
                  #t))))
    (arguments
     '(#:tests? #f
       #:configure-flags '("-DCMAKE_EXE_LINKER_FLAGS=-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (exe (string-append out "/bin/unionfs")))
               ;; By default, 'unionfs' keeps references to
               ;; $glibc/share/locale and similar stuff.  Remove them.
               (remove-store-references exe)

               ;; 'unionfsctl' has references to glibc as well.  Since
               ;; we don't need it, remove it.
               (delete-file (string-append out "/bin/unionfsctl"))
               #t))))))
    (inputs `(("fuse" ,fuse-static)))))

(define-public sshfs
  (package
    (name "sshfs")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libfuse/sshfs/releases/"
                                  "download/sshfs-" version "/sshfs-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "00fir2iykdx11g8nv5gijg0zjrp2g3ldypnv0yi6lq3h5pg5v13h"))))
    (build-system gnu-build-system)
    (inputs
     `(("fuse" ,fuse)
       ("glib" ,glib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/libfuse/sshfs")
    (synopsis "Mount remote file systems over SSH")
    (description
     "This is a file system client based on the SSH File Transfer Protocol.
Since most SSH servers already support this protocol it is very easy to set
up: on the server side there's nothing to do; on the client side mounting the
file system is as easy as logging into the server with an SSH client.")
    (license license:gpl2+)))

(define-public sshfs-fuse
  (package (inherit sshfs)
    (name "sshfs-fuse")
    (properties `((superseded . ,sshfs)))))

(define-public archivemount
  (package
    (name "archivemount")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.cybernoia.de/software/archivemount/"
                           "archivemount-" version ".tar.gz"))
       (sha256
        (base32 "1cy5b6qril9c3ry6fv7ir87s8iyy5vxxmbyx90dm86fbra0vjaf5"))))
    (build-system gnu-build-system)
    (inputs `(("fuse" ,fuse)
              ("libarchive" ,libarchive)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.cybernoia.de/software/archivemount.html")
    (synopsis "Tool for mounting archive files with FUSE")
    (description "archivemount is a FUSE-based file system for Unix variants,
including Linux.  Its purpose is to mount archives (i.e. tar, tar.gz, etc.) to a
mount point where it can be read from or written to as with any other file
system.  This makes accessing the contents of the archive, which may be
compressed, transparent to other programs, without decompressing them.")
    (license license:lgpl2.0+)))

(define-public numactl
  (package
    (name "numactl")
    (version "2.0.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/numactl/numactl/releases/download/v"
                    version "/numactl-" version ".tar.gz"))
              (sha256
               (base32
                "1xngddsph43bxljywahi9d44fxr022slsap4hh91w8xnq54d2sw2"))))
    (build-system gnu-build-system)
    (arguments
     '(;; There's a 'test' target, but it requires NUMA support in the kernel
       ;; to run, which we can't assume to have.
       #:tests? #f))

    ;; NUMA is apparently not supported on armhf, see
    ;; http://www.spinics.net/lists/linux-numa/msg01157.html
    (supported-systems (delete "armhf-linux" %supported-systems))
    (home-page "https://github.com/numactl/numactl")
    (synopsis "Tools for non-uniform memory access (NUMA) machines")
    (description
     "NUMA stands for Non-Uniform Memory Access, in other words a system whose
memory is not all in one place.  The @command{numactl} program allows you to
run your application program on specific CPUs and memory nodes.  It does this
by supplying a NUMA memory policy to the operating system before running your
program.

The package contains other commands, such as @command{numastat},
@command{memhog}, and @command{numademo} which provides a quick overview of
NUMA performance on your system.")
    (license (list license:gpl2                   ;programs
                   license:lgpl2.1))))            ;library

(define-public kbd-neo
  (package
    (name "kbd-neo")
    (version "2486")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://svn.neo-layout.org/!svn/bc/"
                           version "/linux/console/neo.map"))
       (file-name (string-append name "-" version ".map"))
       (sha256
        (base32
         "19mfrd31vzpsjiwc7pshxm0b0sz5dd17xrz6k079cy4im1vf0r4g"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((out (string-append %output "/share/keymaps"))
                         (source (assoc-ref %build-inputs "source")))
                     (mkdir-p out)
                     (copy-file source (string-append out "/neo.map"))
                     #t))))
    (home-page "https://neo-layout.org")
    (synopsis "Neo2 console layout")
    (description
     "Kbd-neo provides the Neo2 keyboard layout for use with
@command{loadkeys(1)} from @code{kbd(4)}.")
    ;; The file is located in an svn directory, the entire content of
    ;; the directory is licensed as GPL3.
    (license license:gpl3+)))

(define-public kbd
  (package
    (name "kbd")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/utils/kbd/kbd-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "124swm93dm4ca0pifgkrand3r9gvj3019d4zkfxsj9djpvv0mnaz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "tests/Makefile.in"
                    ;; The '%: %.in' rule incorrectly uses @VERSION@.
                    (("@VERSION@")
                     "[@]VERSION[@]"))
                  (substitute* '("src/unicode_start" "src/unicode_stop")
                    ;; Assume the Coreutils are in $PATH.
                    (("/usr/bin/tty")
                     "tty"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gzip  (assoc-ref %build-inputs "gzip"))
                   (bzip2 (assoc-ref %build-inputs "bzip2")))
               (substitute* "src/libkeymap/findfile.c"
                 (("gzip")
                  (string-append gzip "/bin/gzip"))
                 (("bzip2")
                  (string-append bzip2 "/bin/bzip2")))
               #t)))
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure these programs find their comrades.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (prog)
                           (wrap-program (string-append bin "/" prog)
                             `("PATH" ":" prefix (,bin))))
                         '("unicode_start" "unicode_stop"))
               #t))))))
    (inputs `(("check" ,check)
              ("gzip" ,gzip)
              ("bzip2" ,bzip2)
              ("pam" ,linux-pam)))
    (native-search-paths
     (list (search-path-specification
            (variable "LOADKEYS_KEYMAP_PATH")
            ;; Append ‘/**’ to recursively search all directories.  One can then
            ;; run (for example) ‘loadkeys en-latin9’ instead of having to find
            ;; and type ‘i386/colemak/en-latin9’ on a mislabelled keyboard.
            (files (list "share/keymaps/**")))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://kbd-project.org/")
    (synopsis "Linux keyboard utilities and keyboard maps")
    (description
     "This package contains keytable files and keyboard utilities compatible
for systems using the Linux kernel.  This includes commands such as
@code{loadkeys}, @code{setfont}, @code{kbdinfo}, and @code{chvt}.")
    (license license:gpl2+)))

(define-public loadkeys-static
  (package
    (inherit kbd)
    (name "loadkeys-static")
    (arguments
     (substitute-keyword-arguments (package-arguments kbd)
       ((#:configure-flags flags ''())
        `(append '("LDFLAGS=-static" "--disable-shared" "--disable-nls"
                   "--disable-vlock"              ;so we don't need libpam
                   "--disable-libkeymap")
                 ,flags))
       ((#:make-flags flags ''())
        `(cons "LDFLAGS=-all-static" ,flags))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; The binary keeps references to gzip, among other things,
                 ;; which we don't need in the initrd, so strip references.
                 (remove-store-references "src/loadkeys")

                 (install-file "src/loadkeys"
                               (string-append out "/bin"))
                 #t)))
           (delete 'post-install)))
       ((#:strip-flags _ '())
        ''("--strip-all"))
       ((#:allowed-references _ '())
        '())))

    (synopsis "Statically-linked @command{loadkeys} program")

    ;; This package is meant to be used internally in the initrd so don't
    ;; expose it.
    (properties '((hidden? . #t)))))

(define-public inotify-tools
  (package
    (name "inotify-tools")
    (version "3.20.11.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rvoicilas/inotify-tools")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m8avqccrhm38krlhp88a7v949f3hrzx060bbrr5dp5qw2nmw9j2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (home-page "https://github.com/rvoicilas/inotify-tools/wiki")
    (synopsis "Monitor file accesses")
    (description
     "The inotify-tools packages provides a C library and command-line tools
to use Linux' inotify mechanism, which allows file accesses to be monitored.")
    (license license:gpl2+)))

(define-public kmod
  (package
    (name "kmod")
    (version "27")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kernel.org/linux/utils/kernel/kmod/"
                              "kmod-" version ".tar.xz"))
              (sha256
               (base32
                "035wzfzjx4nwidk747p8n085mgkvy531ppn16krrajx2dkqzply1"))
              (patches (search-patches "kmod-module-directory.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("xz" ,xz)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags '("--with-xz" "--with-zlib"
                           "--disable-test-modules")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-tests
           (lambda _
             ;; XXX: These tests need '--sysconfdir=/etc' to pass.
             (substitute* "Makefile.in"
               (("testsuite/test-modprobe") "")
               (("testsuite/test-depmod") "")
               (("testsuite/test-blacklist") ""))
             #t))
         (add-after 'install 'install-modprobe&co
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (tool)
                           (symlink "kmod"
                                    (string-append bin "/" tool)))
                         '("insmod" "rmmod" "lsmod" "modprobe"
                           "modinfo" "depmod"))
               #t))))))
    (supported-systems (delete "i586-gnu" %supported-systems))
    (home-page "https://www.kernel.org/")
    (synopsis "Kernel module tools")
    (description "Kmod is a set of tools to handle common tasks with Linux
kernel modules like insert, remove, list, check properties, resolve
dependencies and aliases.

These tools are designed on top of libkmod, a library that is shipped with
kmod.  The aim is to be compatible with tools, configurations and indices
from the module-init-tools project.")
    (license license:gpl2+))) ; library under lgpl2.1+

(define-public earlyoom
  (package
    (name "earlyoom")
    (version "1.6.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rfjakob/earlyoom")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16iyn51xlrsbshc7p5xl2338yyfzknaqc538sa7mamgccqwgyvvq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'check 'set-go-HOME
           (lambda _
             (setenv "HOME" (getcwd))
             #t))
         (add-before 'check 'disable-failing-test
           (lambda _
             ;; This test relies on writing to /proc/%d/oom_score_adj.
             (substitute* "testsuite_cli_test.go"
               (("TestI" match)
                (string-append "skipped" match)))
             #t)))
       #:make-flags (let* ((prefix (assoc-ref %outputs "out")))
                      (list ,(string-append "CC=" (cc-for-target))
                            (string-append "VERSION=v" ,version)
                            (string-append "PREFIX=" prefix)
                            (string-append "SYSCONFDIR=" prefix "/etc")))
       #:test-target "test"))
    (native-inputs `(("go" ,go)           ;for the test suite
                     ("pandoc" ,pandoc))) ;to generate the manpage
    (home-page "https://github.com/rfjakob/earlyoom")
    (synopsis "Simple out of memory (OOM) daemon for the Linux kernel")
    (description "Early OOM is a minimalist out of memory (OOM) daemon that
runs in user space and provides a more responsive and configurable alternative
to the in-kernel OOM killer.")
    (license license:expat)))

(define-public eudev
  ;; The post-systemd fork, maintained by Gentoo.
  (package
    (name "eudev")
    (version "3.2.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url "https://github.com/gentoo/eudev")
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g9z3d33m0i3hmbhm0wxpvkzf6ac7xj1drwcfrhzlfhhi63sg9h7"))
              (patches (search-patches "eudev-rules-directory.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-source-writable
           (lambda _
             ;; XXX: Git checkouts are read-only, but this package needs to
             ;; modify some of its files.
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'bootstrap 'patch-file-names
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
            (substitute* "man/make.sh"
              (("/usr/bin/xsltproc")
               (string-append (assoc-ref
                               (or native-inputs inputs) "xsltproc")
                               "/bin/xsltproc")))
            #t))
         (add-before 'configure 'patch-bindir-in-btrfs-rules
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The "@bindir@" substitution incorrectly expands to a literal
             ;; "${exec_prefix}" (see <https://bugs.gnu.org/39926>).  Work
             ;; around it.
             (let ((out (assoc-ref outputs "out")))
               (substitute* "rules/64-btrfs.rules.in"
                 (("@bindir@")
                  (string-append out "/bin")))
               #t)))
         (add-after 'install 'move-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (static (assoc-ref outputs "static"))
                    (source (string-append out "/lib/libudev.a"))
                    (target (string-append static "/lib/libudev.a")))
               (mkdir-p (dirname target))
               (link source target)
               (delete-file source)
               ;; Remove reference to the static library from the .la file
               ;; such that Libtool looks for it in the usual places.
               (substitute* (string-append out "/lib/libudev.la")
                 (("old_library=.*")
                  "old_library=''\n"))
               #t)))
         (add-after 'install 'build-hwdb
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Build OUT/etc/udev/hwdb.bin.  This allows 'lsusb' and
             ;; similar tools to display product names.
             ;;
             ;; XXX: This can't be done when cross-compiling. Find another way
             ;; to generate hwdb.bin for cross-built systems.
             (let ((out (assoc-ref outputs "out")))
               ,@(if (%current-target-system)
                     '(#t)
                     '((invoke (string-append out "/bin/udevadm")
                               "hwdb" "--update")))))))
       #:configure-flags (list "--enable-manpages")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gperf" ,gperf)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ;; For tests.
       ("perl" ,perl)
       ("python" ,python-wrapper)
       ;; For documentation.
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("libxml2" ,libxml2)             ;for $XML_CATALOG_FILES
       ("xsltproc" ,libxslt)))
    (inputs
     ;; When linked against libblkid, eudev can populate /dev/disk/by-label
     ;; and similar; it also installs the '60-persistent-storage.rules' file,
     ;; which contains the rules to do that.
     `(("util-linux" ,util-linux "lib")           ;for blkid
       ("kmod" ,kmod)))
    (outputs '("out" "static"))
    (home-page "https://wiki.gentoo.org/wiki/Project:Eudev")
    (synopsis "Userspace device management")
    (description "Udev is a daemon which dynamically creates and removes
device nodes from /dev/, handles hotplug events and loads drivers at boot
time.")
    (license license:gpl2+)))

(define-public python-evdev
  (package
    (name "python-evdev")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "evdev" version))
       (sha256
        (base32 "0kb3636yaw9l8xi8s184w0r0n9ic5dw3b8hx048jf9fpzss4kimi"))))
    (build-system python-build-system)
    (native-inputs
     `(("kernel-headers" ,linux-libre-headers)))
    (arguments
     `(#:tests? #f                      ;no rule for tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-hard-coded-directory
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               (("/usr/include/linux")
                (string-append
                 (assoc-ref inputs "kernel-headers") "/include/linux")))
             #t)))))
    (home-page "https://github.com/gvalkov/python-evdev")
    (synopsis "Bindings to the Linux input handling subsystem")
    (description
     "Python-evdev provides bindings to the generic input event interface in
Linux.  The @code{evdev} interface serves the purpose of passing events
generated in the kernel directly to userspace through character devices that
are typically located in @file{/dev/input/}.

This package also comes with bindings to @code{uinput}, the userspace input
subsystem.  @code{uinput} allows userspace programs to create and handle input
devices that can inject events directly into the input subsystem.")
    (license license:bsd-3)))

(define-public interception-tools
  (package
    (name "interception-tools")
    (version "0.6.4")
    (home-page "https://gitlab.com/interception/linux/tools")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "121jy40ynkbzlqnx7g0dqwvkb7dm2ahcy6vwrz6ylsyd0jmi6s5a"))))
    (build-system cmake-build-system)
    (inputs
     `(("boost" ,boost)
       ("libevdev" ,libevdev)
       ("libudev" ,eudev)
       ("yaml-cpp" ,yaml-cpp)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-libevdev-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libevdev (assoc-ref inputs "libevdev")))
               (substitute* "CMakeLists.txt"
                 (("/usr/include/libevdev-1.0")
                  (string-append libevdev "/include/libevdev-1.0")))
               #t))))
       ;; No tests are included.
       #:tests? #f))
    (synopsis "Utilities for operating on input events of evdev devices")
    (description
     "Interception Tools provides a composable infrastructure on top of
@code{libudev} and @code{libevdev}.  The following utilities are provided:

@itemize
@item @command{udevmon} --- monitor input devices for launching tasks
@item @command{intercept} --- redirect device input events to stdout
@item @command{uinput} --- redirect device input events from stding to virtual device
@item @command{mux} --- mux streams of input events
@end itemize")
    ;; Dual-licensed under GPLv3+ or "something else" on request, per
    ;; 'README.md'.
    (license license:gpl3+)))

(define-public interception-dual-function-keys
  (package
    (name "interception-dual-function-keys")
    (version "1.3.0")
    (home-page "https://gitlab.com/interception/linux/plugins/dual-function-keys")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gvhkmwzl5fyyc7k8rc4rf2b9mzh05wa8wcybf9hz2x1mqkc7lmz"))))
    (build-system gnu-build-system)
    (inputs
     `(("libevdev" ,libevdev)
       ("yaml-cpp" ,yaml-cpp)))
    (arguments
     `(#:make-flags (list "CC=gcc" "CXX=g++"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-libevdev-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libevdev (assoc-ref inputs "libevdev")))
               (substitute* "config.mk"
                 (("/usr/include/libevdev-1.0")
                  (string-append libevdev "/include/libevdev-1.0")))
               #t)))
         ;; No configure script
         (delete 'configure)
         ;; No target 'check'
         (delete 'check))))
    (synopsis "Tap for one key, hold for another")
    (description
     "Dual Function Keys is a plugin for @code{interception-tools} that allows
one to send arbitrary keycodes when a given key is tapped or held.")
    (license license:expat)))

(define-public lvm2
  (package
    (name "lvm2")
    (version "2.03.11")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://sourceware.org/ftp/lvm2/LVM2."
                                        version ".tgz")
                         (string-append "ftp://sources.redhat.com/pub/lvm2/releases/LVM2."
                                        version ".tgz")))
              (sha256
               (base32
                "1m4xpda8vbyd89ca0w8nacvnl4j34yzsa625gn990fb5sh84ab44"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (use-modules (guix build utils))

                  ;; Honor sysconfdir.
                  (substitute* "make.tmpl.in"
                    (("^confdir = .*$")
                     "confdir = @sysconfdir@\n")
                    (("DEFAULT_SYS_DIR = @DEFAULT_SYS_DIR@")
                     "DEFAULT_SYS_DIR = @sysconfdir@"))
                  #t))
              (patches (search-patches "lvm2-static-link.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("procps" ,procps)))                       ;tests use 'pgrep'
    (inputs
     `(("libaio" ,libaio)
       ("udev" ,eudev)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'configure 'set-makefile-shell
           (lambda _
             ;; Use 'sh', not 'bash', so that '. lib/utils.sh' works as
             ;; expected.
             (setenv "SHELL" (which "sh"))

             ;; Replace /bin/sh with the right file name.
             (patch-makefile-SHELL "make.tmpl")
             #t)))

       #:configure-flags (list (string-append "--sysconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc/lvm")
                               "--enable-udev_sync"
                               "--enable-udev_rules"
                               "--enable-pkgconfig"
                               "--enable-cmdlib"
                               "--enable-dmeventd" ; Requires '--enable-cmdlib'.

                               ;; Make sure programs such as 'dmsetup' can
                               ;; find libdevmapper.so.
                               (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib/device-mapper")
                               ;; This is needed when cross-compiling.
                               ,@(if (%current-target-system)
                                     '("ac_cv_func_malloc_0_nonnull=yes"
                                       "ac_cv_func_realloc_0_nonnull=yes")
                                     '()))

       ;; The tests use 'mknod', which requires root access.
       #:tests? #f))
    (supported-systems (delete "i586-gnu" %supported-systems))
    (home-page "https://sourceware.org/lvm2/")
    (synopsis "Logical volume management for Linux")
    (description
     "LVM2 is the logical volume management tool set for Linux-based systems.
This package includes the user-space libraries and tools, including the device
mapper.  Kernel components are part of Linux-libre.")

    ;; Libraries (liblvm2, libdevmapper) are LGPLv2.1.
    ;; Command-line tools are GPLv2.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public lvm2-static
  (package
    (inherit lvm2)
    (name "lvm2-static")

    ;; Propagate udev because libdevmapper.a depends on libudev.
    (propagated-inputs `(("udev:static" ,eudev "static")))

    (arguments
     (substitute-keyword-arguments (package-arguments lvm2)
       ((#:configure-flags flags '())
        ;; LVM2 doesn't use Libtool, hence the custom option.
        `(append '("--enable-static_link")
                 ;; Building dmeventd statically is complicated due to a
                 ;; requirement on libdevmapper.a, which is being phased out
                 ;; in favor of libdevice-mapper.a, which in turn is is not
                 ;; easily made available at dmeventd build time.  Just ignore
                 ;; it until the situation improves.
                 (delete "--enable-dmeventd" ,flags)))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'adjust-Makefile
             (lambda _
               ;; These fixes are related to the upstream libdm->device_mapper
               ;; migration and will hopefully be fixed upstream in due time.
               (substitute* "tools/Makefile.in"
                 ;; This variable is empty in a static configuration and causes
                 ;; an erroneous GCC command line.
                 (("-L\\$\\(interfacebuilddir\\)") "")
                 ;; Remove obsolete reference to libdevmapper.a.
                 (("-ldevmapper") ""))
               #t))))))
    (synopsis "Logical volume management for Linux (statically linked)")))

(define-public thin-provisioning-tools
  (package
    (name "thin-provisioning-tools")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jthornber/thin-provisioning-tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01wl8c0cjbx1smbhj8dx6av5bnw5775m58gasc3vqwvsj0s9hq19"))))
    (build-system gnu-build-system)
    (arguments
     ;; Doesn't build with --enable-testing due to a function name collision
     ;; with glibc. Fixed upstream. TODO: Enable tests when 0.9.0 is released.
     `(#:tests? #f))
    (native-inputs
     `(("automake" ,automake)
       ("autoreconf" ,autoconf)))
    (inputs
     `(("boost" ,boost)
       ("expat" ,expat)
       ("libaio" ,libaio)))
    (synopsis "Tools for manipulating the metadata of device-mapper targets")
    (description "A suite of tools for manipulating the metadata of the
dm-thin, dm-cache and dm-era device-mapper targets.")
    (home-page "https://github.com/jthornber/thin-provisioning-tools")
    (license license:gpl3+)))

(define-public wireless-tools
  (package
    (name "wireless-tools")
    (version "30.pre9")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.hpl.hp.com/personal/Jean_Tourrilhes/Linux/wireless_tools."
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qscyd44jmhs4k32ggp107hlym1pcyjzihiai48xs7xzib4wbndb"))
              (snippet
               '(begin
                  ;; Remove the older header files that are not free software.
                  (for-each (lambda (n)
                              (delete-file (format #f "wireless.~a.h" n)))
                            '(10 11 12 13 14 15 16 17 18 19 20))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" %output)
             (string-append "INSTALL_MAN=" %output "/share/man")
             (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib")
             "BUILD_STATIC=")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key target #:allow-other-keys)
             (when ,(%current-target-system)
               ;; Cross-compilation: use the cross tools.
               (substitute* (find-files "." "Makefile")
                 (("CC = .*$")
                  (string-append "CC = " target "-gcc\n"))
                 (("AR = .*$")
                  (string-append "AR = " target "-ar\n"))
                 (("RANLIB = .*$")
                  (string-append "RANLIB = " target "-ranlib\n"))))
             #t)))
       #:tests? #f))
    (synopsis "Tools for manipulating Linux Wireless Extensions")
    (description "Wireless Tools are used to manipulate the now-deprecated
Linux Wireless Extensions; consider using @code{iw} instead.  The Wireless
Extension was an interface allowing you to set Wireless LAN specific
parameters and get the specific stats.  It is deprecated in favor the nl80211
interface.")
    (home-page "https://www.hpl.hp.com/personal/Jean_Tourrilhes/Linux/Tools.html")
    ;; wireless.21.h and wireless.22.h are distributed under lgpl2.1+, the
    ;; other files are distributed under gpl2.
    (license (list license:gpl2 license:lgpl2.1+))))

(define-public crda
  (package
    (name "crda")
    (version "3.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/network/crda/"
                                  "crda-" version ".tar.xz"))
              (sha256
               (base32
                "1gydiqgb08d9gbx4l6gv98zg3pljc984m50hmn3ysxcbkxkvkz23"))
              (patches (search-patches "crda-optional-gcrypt.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'gzip-determinism
                    (lambda _
                      (substitute* "Makefile"
                        (("gzip") "gzip --no-name"))
                      #t))
                  ,@(if (%current-target-system)
                        '((add-after
                            'unpack 'fix-pkg-config
                            (lambda* (#:key target #:allow-other-keys)
                                     (substitute*
                                       "Makefile"
                                       (("pkg-config")
                                        (string-append target "-pkg-config")))
                                     #t)))
                        '())
                  (add-before
                   'build 'no-werror-no-ldconfig
                   (lambda _
                     (substitute* "Makefile"
                       (("-Werror")  "")
                       (("ldconfig") "true"))
                     #t))
                  (add-before
                   'build 'set-regulator-db-file-name
                   (lambda* (#:key native-inputs inputs #:allow-other-keys)
                     ;; Tell CRDA where to find our database.
                     (let ((regdb (assoc-ref (or native-inputs inputs)
                                             "wireless-regdb")))
                       (substitute* "crda.c"
                         (("\"/lib/crda/regulatory.bin\"")
                          (string-append "\"" regdb
                                         "/lib/crda/regulatory.bin\"")))
                       #t))))
       #:test-target "verify"
       #:make-flags (let ((out     (assoc-ref %outputs "out"))
                          (regdb   (assoc-ref %build-inputs "wireless-regdb")))
                      (list
                       (string-append "CC=" ,(cc-for-target))
                       "V=1"

                       ;; Disable signature-checking on 'regulatory.bin'.
                       ;; The reason is that this simplifies maintenance
                       ;; on our side (no need to manage a distro key
                       ;; pair), and we can guarantee integrity of
                       ;; 'regulatory.bin' by other means anyway, such as
                       ;; 'guix gc --verify'.  See
                       ;; <https://wireless.wiki.kernel.org/en/developers/regulatory/wireless-regdb>
                       ;; for a discssion.
                       "USE_OPENSSL=0"

                       (string-append "PREFIX=" out)
                       (string-append "SBINDIR=" out "/sbin/")
                       (string-append "UDEV_RULE_DIR="
                                      out "/lib/udev/rules.d")
                       (string-append "LDFLAGS=-Wl,-rpath="
                                      out "/lib -L.")
                       (string-append "REG_BIN=" regdb
                                      "/lib/crda/regulatory.bin")
                       "all_noverify"))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("wireless-regdb" ,wireless-regdb)))
    (inputs `(("libnl" ,libnl)))
    (home-page
     "https://wireless.wiki.kernel.org/en/developers/Regulatory/CRDA")
    (synopsis "Central regulatory domain agent (CRDA) for WiFi")
    (description
     "The Central Regulatory Domain Agent (CRDA) acts as the udev helper for
communication between the kernel Linux and user space for regulatory
compliance.")
    (license license:copyleft-next)))

(define-public wireless-regdb
  (package
    (name "wireless-regdb")
    (version "2020.11.20")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/software/network/wireless-regdb/"
                    "wireless-regdb-" version ".tar.xz"))
              (sha256
               (base32
                "0liagyi6ppf5w474qk9j4jz5gbvvn8mc8al1dq4b1xrgv28485ml"))

              ;; We're building 'regulatory.bin' by ourselves.
              (snippet '(begin
                          (delete-file "regulatory.bin")
                          #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'gzip-determinism
           (lambda _
             (substitute* "Makefile"
               (("gzip") "gzip --no-name"))
             #t))
         (add-after 'unpack 'omit-signature
           (lambda _
             (substitute* "Makefile"
               ;; Signing requires a REGDB_PUBCERT and REGDB_PRIVKEY which we
               ;; don't provide (see below).  Disable it.
               ((" regulatory\\.db\\.p7s") "")
               ;; regulatory.db is built as a dependency of regulatory.db.p7s,
               ;; but ‘make install’ depends only on the latter while installing
               ;; both (and failing).  Depend on it explicitly.
               (("^install: " all) (string-append all "regulatory.db ")))
             #t))
         (delete 'configure))  ; no configure script

       ;; The 'all' target of the makefile depends on $(REGDB_CHANGED), which
       ;; is computed and can be equal to 'maintainer-clean'; when that
       ;; happens, we can end up deleting the 'regulatory.bin' file that we
       ;; just built.  Thus, build things sequentially.
       #:parallel-build? #f

       #:tests? #f                      ; no tests
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)
               (string-append "FIRMWARE_PATH=$(PREFIX)/lib/firmware")

               ;; Leave this empty so that db2bin.py doesn't try to sign
               ;; ‘regulatory.bin’.  This allows us to avoid managing a key
               ;; pair for the whole distribution.
               (string-append "REGDB_PRIVKEY=")
               ;; Don't generate a public key for the same reason.  These are
               ;; used as Makefile targets and can't be the empty string.
               (string-append "REGDB_PUBCERT=/dev/null")
               (string-append "REGDB_PUBKEY=/dev/null")))))
    (native-inputs
     `(("python" ,python-wrapper)))
    (home-page
     "https://wireless.wiki.kernel.org/en/developers/regulatory/wireless-regdb")
    (synopsis "Wireless regulatory database")
    (description
     "This package contains the wireless regulatory database for the Central
Regulatory Database Agent (CRDA).  The database contains information on
country-specific regulations for the wireless spectrum.")
    (license license:isc)))

(define-public lm-sensors
  (package
    (name "lm-sensors")
    (version "3.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/groeck/lm-sensors")
             (commit (string-append "V" (string-join
                                         (string-split version #\.) "-")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ipf6wjx037sqyhy0r5jh4983h216anq9l68ckn2x5c3qc4wfmzn"))
       (patches (search-patches "lm-sensors-hwmon-attrs.patch"))))
    (build-system gnu-build-system)
    (inputs `(("rrdtool" ,rrdtool)
              ("perl" ,perl)
              ("kmod" ,kmod)
              ("gnuplot" ,gnuplot)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("flex" ,flex)
                     ("bison" ,bison)
                     ("which" ,which)))
    (outputs '("lib"                    ; avoid perl in closure
               "out"))
    (arguments
     `(#:tests? #f                      ; no 'check' target
       #:make-flags (list (string-append "PREFIX=" %output)
                          (string-append "ETCDIR=" (assoc-ref %outputs "lib") "/etc")
                          (string-append "INCLUDEDIR="
                                         (assoc-ref %outputs "lib") "/include")
                          (string-append "MANDIR=" %output "/share/man")
                          (string-append "LIBDIR=" (assoc-ref %outputs "lib") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-exec-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "prog/detect/sensors-detect"
               (("`uname")
                (string-append "`" (assoc-ref inputs "coreutils")
                               "/bin/uname"))
               (("(`|\")modprobe" all open-quote)
                (string-append open-quote
                               (assoc-ref inputs "kmod")
                               "/bin/modprobe")))
             (substitute* '("prog/pwm/pwmconfig"
                            "prog/pwm/fancontrol")
               (("gnuplot")
                (string-append (assoc-ref inputs "gnuplot")
                               "/bin/gnuplot"))
               (("cat ")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/cat "))
               (("e?grep " match)
                (string-append (assoc-ref inputs "grep")
                               "/bin/" match))
               (("sed -e")
                (string-append (assoc-ref inputs "sed")
                               "/bin/sed -e"))
               (("cut -d")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/cut -d"))
               (("sleep ")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/sleep "))
               (("readlink -f")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/readlink -f")))
             #t)))))
    (home-page "https://hwmon.wiki.kernel.org/lm_sensors")
    (synopsis "Utilities to read temperature/voltage/fan sensors")
    (description
     "Lm-sensors is a hardware health monitoring package for Linux.  It allows
you to access information from temperature, voltage, and fan speed sensors.
It works with most newer systems.")
    (license license:gpl2+)))

(define-public iucode-tool
  (package
    (name "iucode-tool")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.com/iucode-tool/releases"
                                  "/raw/latest/iucode-tool_" version ".tar.xz"))
              (sha256
               (base32
                "159gvf6ljgg3g4vlhyy6pyr0wz11rcyhp985vc4az58d9px8xf0j"))))
    (build-system gnu-build-system)
    (home-page "https://gitlab.com/iucode-tool/iucode-tool/wikis/home")
    (synopsis "Manipulate Intel microcode bundles")
    (description
     "@command{iucode_tool} is a utility to work with microcode packages for
Intel processors.  It can convert between formats, extract specific versions,
create a firmware image suitable for the Linux kernel, and more.")
    ;; cpuid.h is available for i686, x86_64, and ia64.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:gpl2+)))

(define-public i2c-tools
  (package
    (name "i2c-tools")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://jdelvare.nerim.net/mirror/i2c-tools/i2c-tools-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "000pvg995qy1b15ks59gd0klri55hb33kqpg5czy84hw1pbdgm0l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no 'check' target
       #:make-flags (list (string-append "prefix=" %output)
                          ,(string-append "CC=" (cc-for-target)))
       ;; No configure script.
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (inputs
     `(("perl" ,perl)))
    (home-page "http://jdelvare.nerim.net/devel.html#i2ctools")
    (synopsis "I2C tools for Linux")
    (description
     "The i2c-tools package contains a heterogeneous set of I2C tools for
Linux: a bus probing tool, a chip dumper, register-level SMBus access helpers,
EEPROM decoding scripts, EEPROM programming tools, and a python module for
SMBus access.")
    (license license:gpl2+)))

(define-public xsensors
  (package
    (name "xsensors")
    (version "0.70")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.linuxhardware.org/xsensors/xsensors-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1siplsfgvcxamyqf44h71jx6jdfmvhfm7mh0y1q8ps4zs6pj2zwh"))))
    (build-system gnu-build-system)
    (inputs `(("lm-sensors" ,lm-sensors "lib")
              ("gtk" ,gtk+-2)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enable-deprecated
           (lambda _
             (substitute* "src/Makefile.in"
               (("-DGDK_DISABLE_DEPRECATED") "")
               (("-DGTK_DISABLE_DEPRECATED") ""))
             #t))
         (add-before 'configure 'remove-Werror
           (lambda _
             (substitute* '("configure" "src/Makefile.in")
               (("-Werror") ""))
             #t)))))
    (home-page "http://www.linuxhardware.org/xsensors/")
    (synopsis "Hardware health information viewer")
    (description
     "Xsensors reads data from the libsensors library regarding hardware
health such as temperature, voltage and fan speed and displays the information
in a digital read-out.")
    (license license:gpl2+)))

(define-public perf
  (package
    (name "perf")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "SHELL_PATH" (which "bash"))
             (chdir "tools/perf")
             #t)))
       #:make-flags (list (string-append "prefix="
                                         (assoc-ref %outputs "out"))
                          "CC=gcc"
                          "WERROR=0"

                          ;; By default, 'config/Makefile' uses lib64 on
                          ;; x86_64.  Work around that.
                          "lib=lib")
       #:tests? #f))                              ;no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("bison" ,bison)
       ("flex" ,flex)

       ;; There are build scripts written in these languages.
       ("perl" ,perl)
       ("python2" ,python-2)
       ("python3" ,python-3)))
    (inputs
     `(("slang" ,slang)                        ;for the interactive TUI
       ;; ("newt" ,newt)
       ("python" ,python-2)                    ;'perf' links against libpython
       ("elfutils" ,elfutils)
       ("libiberty" ,libiberty)      ;used alongside BDF for symbol demangling
       ("libunwind" ,libunwind)      ;better stack walking
       ("numactl" ,numactl)          ;for 'perf bench numa mem'

       ;; Documentation.
       ("libxml2" ,libxml2)                       ;for $XML_CATALOG_FILES
       ("docbook-xsl" ,docbook-xsl)
       ("xmlto" ,xmlto)
       ("asciidoc" ,asciidoc)))
    (home-page "https://perf.wiki.kernel.org/")
    (synopsis "Linux profiling with performance counters")
    (description
     "perf is a tool suite for profiling using hardware performance counters,
with support in the Linux kernel.  perf can instrument CPU performance
counters, tracepoints, kprobes, and uprobes (dynamic tracing).  It is capable
of lightweight profiling.  This package contains the user-land tools and in
particular the @code{perf} command.")
    (license (package-license linux-libre))))

(define-public pflask
  (package
    (name "pflask")
    (version "0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ghedo/pflask")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jikjbhlxlqracnai3v9krzcgd2xwp0p4adw5n07yxc7b857damz"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (home-page "https://ghedo.github.io/pflask/")
    (synopsis "Simple tool for creating Linux namespace containers")
    (description "pflask is a simple tool for creating Linux namespace
containers.  It can be used for running a command or even booting an OS inside
an isolated container, created with the help of Linux namespaces.  It is
similar in functionality to chroot, although pflask provides better isolation
thanks to the use of namespaces.")
    (license license:bsd-2)))

(define-public singularity
  (package
    (name "singularity")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/singularityware/singularity/"
                                  "releases/download/" version
                                  "/singularity-" version ".tar.gz"))
              (sha256
               (base32
                "1whx0hqqi1326scgdxxxa1d94vn95mnq0drid6s8wdp84ni4d3gk"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Do not create directories in /var.
                  (substitute* "Makefile.in"
                    (("\\$\\(MAKE\\) .*install-data-hook") ""))

                  ;; The original source overrides PATH so that it points to
                  ;; /bin, /usr/local/bin, etc., which obviously doesn't work
                  ;; on Guix System.  Leave PATH unchanged so we refer to the
                  ;; installed Coreutils, grep, etc.
                  (substitute* "bin/singularity.in"
                    (("^PATH=.*" all)
                     (string-append "#" all "\n")))

                  (substitute* (find-files "libexec/cli" "\\.exec$")
                    (("\\$SINGULARITY_libexecdir/singularity/bin/([a-z]+)-suid"
                      _ program)
                     (string-append "/run/setuid-programs/singularity-"
                                    program "-helper")))

                  ;; These squashfs mount options are apparently no longer
                  ;; supported since Linux-libre 5.4.5.
                  (substitute* "src/lib/image/squashfs/mount.c"
                    (("\"errors=remount-ro\"")
                     "NULL"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-references
           (lambda _
             (substitute* "libexec/cli/build.exec.in"
               (("-mksquashfs") (string-append "-" (which "mksquashfs"))))
             (substitute* (append
                            (find-files "libexec" "functions")
                            (find-files "libexec/bootstrap-scripts" ".*sh$")
                            (find-files "libexec/cli" ".*exec$"))
               (("\\| grep ")
                (string-append "| " (which "grep") " "))
               (("egrep ")
                (string-append (which "egrep") " "))
               ((" sed ")
                (string-append " " (which "sed") " ")))
             #t))
         (add-after 'install 'set-PATH
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Have the 'singularity' and 'run-singularity' self-sufficient.
             (let ((out (assoc-ref outputs "out"))
                   (coreutils (assoc-ref inputs "coreutils")))
               (wrap-program (string-append out "/bin/singularity")
                 `("PATH" ":" = (,(string-append coreutils "/bin"))))
               (substitute* (string-append out "/bin/run-singularity")
                 (("/usr/bin/env singularity")
                  (string-append (which "env") " "
                                 out "/bin/singularity")))
               #t))))))
    (inputs
     `(("libarchive" ,libarchive)
       ("python" ,python-wrapper)
       ("zlib" ,zlib)
       ("squashfs-tools" ,squashfs-tools)))
    (home-page "https://singularity.lbl.gov/")
    (synopsis "Container platform")
    (description "Singularity is a container platform supporting a number of
container image formats.  It can build SquashFS container images or import
existing Docker images.  Singularity requires kernel support for container
isolation or root privileges.")
    (license license:bsd-3)))

(define-public hdparm
  (package
    (name "hdparm")
    (version "9.61")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/hdparm/hdparm/"
                                  "hdparm-" version ".tar.gz"))
              (sha256
               (base32
                "0hskvzsg58hw8abkkmxh5kky0hhilv516870x2bq62zihww1q6ns"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "binprefix=" out)
               (string-append "manprefix=" out)
               ,(string-append "CC=" (cc-for-target))
               ;; Let Guix strip the binaries and not break cross-compilation.
               "STRIP=true"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))           ; no configure script
       #:tests? #f))                    ; no test suite
    (home-page "https://sourceforge.net/projects/hdparm/")
    (synopsis "View and tune ATA disk drive parameters")
    (description
     "@command{hdparm} is a command-line utility to control ATA controllers and
disk drives.  It can increase performance and/or reliability by careful tuning
of hardware settings like power and acoustic management, DMA modes, and caching.
It can also display detailed device information, or be used as a simple
performance benchmarking tool.

@command{hdparm} provides a command line interface to various Linux kernel
interfaces provided by the SATA/ATA/SAS @code{libata} subsystem, and the older
IDE driver subsystem.  Many external USB drive enclosures with SCSI-ATA Command
Translation (@dfn{SAT}) are also supported.")
    (license (license:non-copyleft "file://LICENSE.TXT"))))

(define-public nvme-cli
  (package
    (name "nvme-cli")
    (version "1.14")
    (home-page "https://github.com/linux-nvme/nvme-cli")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32 "0dpadz945482srqpsbfx1bh7rc499fgpyzz1flhk9g9xjbpapkzc"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ; no ./configure script
                  (replace 'install
                    (lambda _
                      (invoke "make" "install-spec" "PREFIX="
                              (string-append "DESTDIR=" %output)))))
       ;; The tests require sysfs, which is not accessible from from the build
       ;; environment
       #:tests? #f))
    (synopsis "NVM-Express user space tooling for Linux")
    (description "Nvme-cli is a utility to provide standards compliant tooling
for NVM-Express drives.  It was made specifically for Linux as it relies on the
IOCTLs defined by the mainline kernel driver.")
    (license license:gpl2+)))

(define-public rfkill
  (package
    (name "rfkill")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/network/"
                                  name "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0snqj5h0y991lszbigbyyqb8swj0hxajc1vfqg2scfay44231bp0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:tests? #f))
    (home-page "https://wireless.wiki.kernel.org/en/users/Documentation/rfkill")
    (synopsis "Tool for enabling and disabling wireless devices")
    (description
     "rfkill is a simple tool for accessing the rfkill device interface,
which is used to enable and disable wireless networking devices, typically
WLAN, Bluetooth and mobile broadband.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))
    ;; rfkill is part of util-linux as of 2.31.
    (properties `((superseded . ,util-linux)))))

(define-public acpi
  (package
    (name "acpi")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/acpiclient/acpiclient/"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "01ahldvf0gc29dmbd5zi4rrnrw2i1ajnf30sx2vyaski3jv099fp"))))
    (build-system gnu-build-system)
    (home-page "http://acpiclient.sourceforge.net")
    (synopsis "Display information on ACPI devices")
    (description "@code{acpi} attempts to replicate the functionality of the
\"old\" @code{apm} command on ACPI systems, including battery and thermal
information.  It does not support ACPI suspending, only displays information
about ACPI devices.")
    (license license:gpl2+)))

(define-public acpid
  (package
    (name "acpid")
    (version "2.0.32")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/acpid2/acpid-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0zhmxnhnhg4v1viw82yjr22kram6k5k1ixznhayk8cnw7q5x7lpj"))))
    (build-system gnu-build-system)
    (home-page "https://sourceforge.net/projects/acpid2/")
    (synopsis "Daemon for delivering ACPI events to user-space programs")
    (description
     "acpid is designed to notify user-space programs of Advanced
Configuration and Power Interface (ACPI) events.  acpid should be started
during the system boot, and will run as a background process.  When an ACPI
event is received from the kernel, acpid will examine the list of rules
specified in /etc/acpi/events and execute the rules that match the event.")
    (license license:gpl2+)))

(define-public sysfsutils
  (package
    (name "sysfsutils")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://sourceforge/linux-diag/sysfsutils/" version "/sysfsutils-"
         version ".tar.gz"))
       (sha256
        (base32 "12i0ip11xbfcjzxz4r10cvz7mbzgq1hfcdn97w6zz7sm3wndwrg8"))))
    (build-system gnu-build-system)
    (home-page "http://linux-diag.sourceforge.net/Sysfsutils.html")
    (synopsis "System utilities based on Linux sysfs")
    (description
     "These are a set of utilities built upon sysfs, a virtual file system in
Linux kernel versions 2.5+ that exposes a system's device tree.  The package
also contains the libsysfs library.")
    ;; The library is under lgpl2.1+ (all files say "or any later version").
    ;; The rest is mostly gpl2, with a few files indicating gpl2+.
    (license (list license:gpl2 license:gpl2+ license:lgpl2.1+))))

(define-public sysfsutils-1
  (package
    (inherit sysfsutils)
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://sourceforge/linux-diag/sysfsutils/sysfsutils-" version
         "/sysfsutils-" version ".tar.gz"))
       (sha256
        (base32 "0kdhs07fm8263pxwd5blwn2x211cg4fk63fyf9ijcdkvzmwxrqq3"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "Makefile.in"
             (("includedir = /usr/include/sysfs")
              "includedir = @includedir@"))
           (substitute* "configure"
             (("includedir='(\\$\\{prefix\\}/include)'" all orig)
              (string-append "includedir='" orig "/sysfs'")))
           #t))))
    (synopsis "System utilities based on Linux sysfs (version 1.x)")))

(define-public cpufrequtils
  (package
    (name "cpufrequtils")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kernel.org/linux/utils/kernel/cpufreq/"
                           "cpufrequtils-" version ".tar.gz"))
       (sha256
        (base32 "0qfqv7nqmjfr3p0bwrdlxkiqwqr7vmx053cadaa548ybqbghxmvm"))
       (patches (search-patches "cpufrequtils-fix-aclocal.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("sysfsutils" ,sysfsutils-1)))
    (arguments
     '(#:make-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out") "/lib"))))
    (home-page "https://www.kernel.org/pub/linux/utils/kernel/cpufreq/")
    (synopsis "Utilities to get and set CPU frequency on Linux")
    (description
     "The cpufrequtils suite contains utilities to retrieve CPU frequency
information, and set the CPU frequency if supported, using the cpufreq
capabilities of the Linux kernel.")
    (license license:gpl2)))

(define-public libraw1394
  (package
    (name "libraw1394")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/libs/ieee1394/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0pm5b415j1qdzyw38wdv8h7ff4yx20831z1727mpsb6jc6bwdk03"))))
    (build-system gnu-build-system)
    (home-page "https://ieee1394.wiki.kernel.org/index.php/Main_Page")
    (synopsis "Interface library for the Linux IEEE1394 drivers")
    (description
     "Libraw1394 is the only supported interface to the kernel side raw1394 of
the Linux IEEE-1394 subsystem, which provides direct access to the connected
1394 buses to user space.  Through libraw1394/raw1394, applications can directly
send to and receive from other nodes without requiring a kernel driver for the
protocol in question.")
    (license license:lgpl2.1+)))

(define-public libavc1394
  (package
    (name "libavc1394")
    (version "0.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libavc1394/libavc1394/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lsv46jdqvdx5hx92v0z2cz3yh6212pz9gk0k3513sbaa04zzcbw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libraw1394" ,libraw1394))) ; required by libavc1394.pc
    (home-page "https://sourceforge.net/projects/libavc1394/")
    (synopsis "AV/C protocol library for IEEE 1394")
    (description
     "Libavc1394 is a programming interface to the AV/C specification from
the 1394 Trade Association.  AV/C stands for Audio/Video Control.")
    (license license:lgpl2.1+)))

(define-public libiec61883
  (package
    (name "libiec61883")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/libs/ieee1394/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "17ph458zya2l8dr2xwqnzy195qd9swrir31g78qkgb3g4xz2rq6i"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libraw1394" ,libraw1394))) ; required by libiec61883.pc
    (home-page "https://ieee1394.wiki.kernel.org/index.php/Main_Page")
    (synopsis "Isochronous streaming media library for IEEE 1394")
    (description
     "The libiec61883 library provides a higher level API for streaming DV,
MPEG-2 and audio over Linux IEEE 1394.")
    (license license:lgpl2.1+)))

(define-public mdadm
  (package
    (name "mdadm")
    (version "4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/utils/raid/mdadm/mdadm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0jjgjgqijpdp7ijh8slzzjjw690kydb1jjadf0x5ilq85628hxmb"))))
    (build-system gnu-build-system)
    (inputs
     `(("udev" ,eudev)))
    (arguments
     `(#:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list ,(string-append "CC=" (cc-for-target))
                            "INSTALL=install"
                            "CHECK_RUN_DIR=0"
                            ;; TODO: tell it where to find 'sendmail'
                            ;; (string-append "MAILCMD=" <???> "/sbin/sendmail")
                            (string-append "BINDIR=" out "/sbin")
                            (string-append "MANDIR=" out "/share/man")
                            (string-append "UDEVDIR=" out "/lib/udev")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-program-paths
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (let ((coreutils (assoc-ref (or native-inputs inputs)
                                         "coreutils")))
               (substitute* "udev-md-raid-arrays.rules"
                 (("/usr/bin/(readlink|basename)" all program)
                  (string-append coreutils "/bin/" program))))
             #t))
         (add-before 'build 'remove-W-error
           (lambda _
             ;; We cannot build with -Werror on i686 due to a
             ;; 'sign-compare' warning in util.c.
             (substitute* "Makefile"
               (("-Werror") ""))
             #t))
         (delete 'configure))
       ;;tests must be done as root
       #:tests? #f))
    (supported-systems (delete "i586-gnu" %supported-systems))
    (home-page "http://neil.brown.name/blog/mdadm")
    (synopsis "Tool for managing Linux Software RAID arrays")
    (description
     "mdadm is a tool for managing Linux Software RAID arrays.  It can create,
assemble, report on, and monitor arrays.  It can also move spares between raid
arrays when needed.")
    (license license:gpl2+)))

(define-public mdadm-static
  (package
    (inherit mdadm)
    (name "mdadm-static")
    (arguments
     (substitute-keyword-arguments (package-arguments mdadm)
       ((#:make-flags flags)
        `(cons "LDFLAGS = -static" ,flags))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'install 'remove-cruft
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out         (assoc-ref outputs "out"))
                      (precious?   (lambda (file)
                                     (member file '("." ".." "sbin"))))
                      (directories (scandir out (negate precious?))))
                 (with-directory-excursion out
                   (for-each delete-file-recursively directories)
                   (remove-store-references "sbin/mdadm")
                   (delete-file "sbin/mdmon")
                   #t))))))
       ((#:modules modules %gnu-build-system-modules)
        `((ice-9 ftw) ,@modules))
       ((#:strip-flags _ '())
        ''("--strip-all"))                        ;strip a few extra KiB
       ((#:allowed-references _ '("out"))
        '("out"))))                               ;refer only self
    (synopsis "Statically-linked 'mdadm' command for use in an initrd")))

(define-public multipath-tools
  (package
    (name "multipath-tools")
    (version "0.8.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.opensvc.com/multipath-tools/.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gipg0z79h76j0f449cx4wcrfsv69ravjlpphsac11h302g3nrvg"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Drop bundled valgrind headers.
                  (delete-file-recursively "third-party")
                  (substitute* '("multipathd/main.c"
                                 "libmultipath/debug.c")
                    (("#include \"../third-party/")
                     "#include \""))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:parallel-build? #f             ;XXX: broken in 0.8.4
       #:make-flags (list "CC=gcc"
                          (string-append "DESTDIR="
                                         (assoc-ref %outputs "out"))
                          ;; Install Udev rules below this directory, relative
                          ;; to the prefix.
                          "SYSTEMDPATH=lib"
                          (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out")
                                         "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((lvm2 (assoc-ref inputs "lvm2"))
                   (udev (assoc-ref inputs "udev")))
               (substitute* "Makefile.inc"
                 (("\\$\\(prefix\\)/usr") "$(prefix)")
                 ;; Do not save timestamp to avoid gzip "timestamp
                 ;; out-of-range" warnings.
                 (("gzip -9") "gzip -9n"))
               (substitute* '("kpartx/Makefile" "libmultipath/Makefile")
                 (("/usr/include/libdevmapper.h")
                  (string-append lvm2 "/include/libdevmapper.h"))
                 (("/usr/include/libudev.h")
                  (string-append udev "/include/libudev.h")))
               #t)))
         (add-after 'unpack 'fix-maybe-uninitialized-variable
           (lambda _
             ;; This variable gets initialized later if needed, but GCC 7
             ;; fails to notice.  Should be fixed for > 0.8.4.
             ;; https://www.redhat.com/archives/dm-devel/2020-March/msg00137.html
             (substitute* "libmultipath/structs_vec.c"
               (("bool is_queueing;")
                "bool is_queueing = false;"))
             #t))
         (add-after 'unpack 'fix-linking-tests
           (lambda _
             ;; Add missing linker flag for -lmpathcmd.  This should be fixed
             ;; for versions > 0.8.4.
             (substitute* "tests/Makefile"
               (("-lmultipath -lcmocka")
                "-lmultipath -L$(mpathcmddir) -lmpathcmd -lcmocka"))
             #t))
         (add-after 'unpack 'skip-failing-tests
           (lambda _
             ;; This test and the module's setup() test an arbitrary block
             ;; device node name, but the build environment has none.
             (substitute* "tests/devt.c"
               (("return get_one_devt.*") "return 0;\n")
               (("cmocka_unit_test\\(test_devt2devname_devt_good\\),") ""))
             ;; The above triggers -Werror=unused-function.  Ignore it.
             (substitute* "tests/Makefile"
               (("CFLAGS \\+= " match)
                (string-append match "-Wno-error=unused-function ")))
             #t))
         (delete 'configure))))         ; no configure script
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("valgrind" ,valgrind)

       ;; For tests.
       ("cmocka" ,cmocka)))
    (inputs
     `(("json-c" ,json-c)
       ("libaio" ,libaio)
       ("liburcu" ,liburcu)
       ("lvm2" ,lvm2)
       ("readline" ,readline)
       ("udev" ,eudev)))
    (home-page "http://christophe.varoqui.free.fr/")
    (synopsis "Access block devices through multiple paths")
    (description
     "This package provides the following binaries to drive the
Linux Device Mapper multipathing driver:
@enumerate
@item @command{multipath} - Device mapper target autoconfig.
@item @command{multipathd} - Multipath daemon.
@item @command{mpathpersist} - Manages SCSI persistent reservations on
@code{dm} multipath devices.
@item @command{kpartx} - Create device maps from partition tables.
@end enumerate")
    (license (list license:gpl2+             ; main distribution
                   license:lgpl2.0+))))      ; libmpathcmd/mpath_cmd.h

(define-public libaio
  (package
    (name "libaio")
    (version "0.3.112")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://releases.pagure.org/libaio/"
                                   name "-" version ".tar.gz")))
              (sha256
               (base32
                "14mlqdapjqq1dhpkdgy5z83mvsaz36fcxca7a4z6hinmr7r6415b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((target ,(%current-target-system)))
         (list (string-append "prefix=" %output)
               (string-append
                "CC=" (if target
                          (string-append (assoc-ref %build-inputs "cross-gcc")
                                         "/bin/" target "-gcc")
                          "gcc"))))
       #:test-target "partcheck" ; need root for a full 'check'
       #:phases
       (modify-phases %standard-phases (delete 'configure)))) ; no configure script
    (home-page "https://pagure.io/libaio")
    (synopsis "Linux-native asynchronous I/O access library")
    (description
     "This library enables userspace to use Linux kernel asynchronous I/O
system calls, important for the performance of databases and other advanced
applications.")
    (license license:lgpl2.1+)))

(define-public blktrace
  ;; Take a newer commit to get the fix for CVE-2018-10689.
  (let ((commit "db4f6340e04716285ea56fe26d76381c3adabe58")
        (revision "1"))
    (package
      (name "blktrace")
      (version (git-version "1.2.0" revision commit))
      (home-page
        "https://git.kernel.org/pub/scm/linux/kernel/git/axboe/blktrace.git")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32 "0ah7xn4qnx09k6bm39p69av7d0c8cl6863drv6a1nf914sq1kpgp"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags
         (list ,(string-append "CC=" (cc-for-target))
               (string-append "prefix=" %output))
         #:tests? #f                    ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (add-after 'unpack 'fix-gnuplot-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((gnuplot (assoc-ref inputs "gnuplot")))
                 (substitute* "btt/bno_plot.py"
                   (("gnuplot %s")
                    (string-append gnuplot "/bin/gnuplot %s")))
                 #t))))))
      (inputs
       `(("libaio" ,libaio)
         ("gnuplot" ,gnuplot)
         ("python" ,python-wrapper)))             ;for 'bno_plot.py'
      (synopsis "Block layer IO tracing mechanism")
      (description "Blktrace is a block layer IO tracing mechanism which provides
detailed information about request queue operations to user space.  It extracts
event traces from the kernel (via the relaying through the debug file system).")
      (license license:gpl2))))

(define-public sbc
  (package
    (name "sbc")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/bluetooth/sbc-"
                                  version ".tar.xz"))
              (patches (search-patches "sbc-fix-build-non-x86.patch"))
              (sha256
               (base32
                "1liig5856crb331dps18mp0s13zbkv7yh007zqhq97m94fcddfhc"))))
    (build-system gnu-build-system)
    (inputs
     `(("libsndfile" ,libsndfile)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.kernel.org/pub/linux/bluetooth/")
    (synopsis "Bluetooth subband audio codec")
    (description
     "The SBC is a digital audio encoder and decoder used to transfer data to
Bluetooth audio output devices like headphones or loudspeakers.")
    (license license:gpl2+)))

(define-public bluez
  (package
    (name "bluez")
    (version "5.55")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/bluetooth/bluez-"
                    version ".tar.xz"))
              (sha256
               (base32
                "124v9s4y1s7s6klx5vlmzpk1jlr4x84ch7r7scm7x2f42dqp2qw8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list "--sysconfdir=/etc"
               "--localstatedir=/var"
               "--enable-library"
               "--disable-systemd"
               ;; TODO: is this needed?  Not installed by default since 5.55.
               "--enable-hid2hci"
               ;; Install dbus/udev files to the correct location.
               (string-append "--with-dbusconfdir=" out "/etc")
               (string-append "--with-udevdir=" out "/lib/udev")))
       #:phases
       (modify-phases %standard-phases
         ;; Test unit/test-gatt fails unpredictably. Seems to be a timing
         ;; issue (discussion on upstream mailing list:
         ;; https://marc.info/?t=149578476300002&r=1&w=2)
         (add-before 'check 'skip-wonky-test
            (lambda _
              (substitute* "unit/test-gatt.c"
                (("tester_init\\(&argc, &argv\\);") "return 77;"))
              #t))
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out        (assoc-ref outputs "out"))
                    (servicedir (string-append out "/share/dbus-1/services"))
                    (service    "obexd/src/org.bluez.obex.service")
                    (rule       (string-append
                                 out "/lib/udev/rules.d/97-hid2hci.rules")))
               ;; Install the obex dbus service file.
               (substitute* service
                 (("/bin/false")
                  (string-append out "/libexec/bluetooth/obexd")))
               (install-file service servicedir)
               ;; Fix paths in the udev rule.
               (substitute* rule
                 (("hid2hci --method")
                  (string-append out "/lib/udev/hid2hci --method"))
                 (("/sbin/udevadm")
                  (string-append (assoc-ref inputs "eudev") "/bin/udevadm")))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("glib" ,glib)
       ("dbus" ,dbus)
       ("eudev" ,eudev)
       ("libical" ,libical)
       ("readline" ,readline)))
    (home-page "http://www.bluez.org/")
    (synopsis "Linux Bluetooth protocol stack")
    (description
     "BlueZ provides support for the core Bluetooth layers and protocols.  It
is flexible, efficient and uses a modular implementation.")
    (license license:gpl2+)))

(define-public fuse-exfat
  (package
    (name "fuse-exfat")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/relan/exfat/releases/download/v"
                    version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lz00q8g4590mrdqmf13ba1s9zrqq645ymgm5p9y99ad0qv22r87"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fuse" ,fuse)))
    (home-page "https://github.com/relan/exfat")
    (synopsis "Mount exFAT file systems")
    (description
     "This package provides a FUSE-based file system that provides read and
write access to exFAT devices.")
    (license license:gpl2+)))

(define-public fuseiso
  (package
    (name "fuseiso")
    (version "20070708")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/fuseiso/fuseiso/"
                                  version "/fuseiso-" version ".tar.bz2"))
              (sha256
               (base32
                "127xql52dcdhmh7s5m9xc6q39jdlj3zhbjar1j821kb6gl3jw94b"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fuse" ,fuse)
       ("glib" ,glib)
       ("zlib" ,zlib)))
    (home-page "https://sourceforge.net/projects/fuseiso/")
    (synopsis "Mount ISO file system images")
    (description
     "FuseISO is a FUSE module to mount ISO file system images (.iso, .nrg,
.bin, .mdf and .img files).  It supports plain ISO9660 Level 1 and 2, Rock
Ridge, Joliet, and zisofs.")
    (license license:gpl2)))

(define-public gpm
  (package
    (name "gpm")
    (version "1.20.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.nico.schottelius.org/software/gpm/archives/"
                    "gpm-" version ".tar.bz2"))
              (patches (search-patches "gpm-glibc-2.26.patch"))
              (sha256
               (base32
                "13d426a8h403ckpc8zyf7s2p5rql0lqbg2bv0454x0pvgbfbf4gh"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'bootstrap
                    (lambda _
                      ;; The tarball was not generated with 'make dist' so we
                      ;; need to bootstrap things ourselves.
                      (substitute* "autogen.sh"
                        (("/bin/sh") (which "sh")))
                      (invoke "./autogen.sh")
                      (patch-makefile-SHELL "Makefile.include.in")
                      #t)))

       ;; Make sure programs find libgpm.so.
       #:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))))
    (native-inputs
     `(("texinfo" ,texinfo)
       ("bison" ,bison)
       ("flex" ,flex)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://www.nico.schottelius.org/software/gpm/")
    (synopsis "Mouse support for the Linux console")
    (description
     "The GPM (general-purpose mouse) daemon is a mouse server for
applications running on the Linux console.  It allows users to select items
and copy/paste text in the console and in xterm.")
    (license license:gpl2+)))

(define-public btrfs-progs
  (package
    (name "btrfs-progs")
    (version "5.11.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/kernel/"
                                  "people/kdave/btrfs-progs/"
                                  "btrfs-progs-v" version ".tar.xz"))
              (sha256
               (base32
                "1zpbpmq8qndwls40yg2c9xj9ca6mcxdziadj5i8fbi3ffp2crap3"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "static"))      ; static versions of the binaries in "out"
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-makefile
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "Makefile"
                        (("\\$\\(DESTDIR\\)\\$\\(udevruledir\\)")
                         (string-append (assoc-ref outputs "out")
                                        "/lib/udev/rules.d")))
                      #t))
                 (add-after 'build 'build-static
                   (lambda _ (invoke "make" "static")))
                 (add-after 'install 'install-bash-completion
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (bashcomp (string-append out "/etc/bash_completion.d")))
                       (mkdir-p bashcomp)
                       (copy-file "btrfs-completion"
                                  (string-append bashcomp "/btrfs"))
                       #t)))
                 (add-after 'install 'install-static
                   (let ((staticbin (string-append (assoc-ref %outputs "static")
                                                  "/bin")))
                     (lambda _
                       (invoke "make"
                               (string-append "bindir=" staticbin)
                               "install-static")))))
       #:tests? #f            ; XXX: require the 'btrfs' kernel module.
       #:test-target "test"
       #:parallel-tests? #f)) ; tests fail when run in parallel
    (inputs `(("e2fsprogs" ,e2fsprogs)  ; for btrfs-convert
              ("lzo" ,lzo)
              ("util-linux:lib" ,util-linux "lib")       ;for libblkid and libuuid
              ("util-linux:static" ,util-linux "static") ;ditto
              ("zlib" ,zlib)
              ("zlib:static" ,zlib "static")
              ("zstd" ,zstd "lib")
              ("zstd:static" ,zstd "static")))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("asciidoc" ,asciidoc)
                     ("python" ,python)
                     ("xmlto" ,xmlto)
                     ;; For building documentation.
                     ("libxml2" ,libxml2)
                     ("docbook-xsl" ,docbook-xsl)
                     ;; For tests.
                     ("acl" ,acl)
                     ("which" ,which)
                     ("dmsetup" ,lvm2)
                     ("udevadm" ,eudev)
                     ;; The tests need 'grep' with perl regexp support.
                     ("grep" ,grep)))
    (home-page "https://btrfs.wiki.kernel.org/index.php/Main_Page")
    (synopsis "Create and manage btrfs copy-on-write file systems")
    (description "Btrfs is a @dfn{copy-on-write} (CoW) file system for Linux
aimed at implementing advanced features while focusing on fault tolerance,
repair and easy administration.")
    ;; GPL2+: crc32.c, radix-tree.c, raid6.c, rbtree.c.
    ;; GPL2: Everything else.
    (license (list license:gpl2 license:gpl2+))))

(define-public btrfs-progs/static
  (package
    (name "btrfs-progs-static")
    (version (package-version btrfs-progs))
    (source #f)
    (build-system trivial-build-system)
    (inputs
     `(("btrfs-progs:static" ,btrfs-progs "static")))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (srfi srfi-26))

         (let* ((btrfs  (assoc-ref %build-inputs "btrfs-progs:static"))
                (out    (assoc-ref %outputs "out"))
                (source (string-append btrfs "/bin/btrfs.static"))
                (target (string-append out "/bin/btrfs")))
           (mkdir-p (dirname target))
           (copy-file source target)
           (remove-store-references target)
           (chmod target #o555)
           #t))))
    (home-page (package-home-page btrfs-progs))
    (synopsis "Statically-linked btrfs command from btrfs-progs")
    (description "This package provides the statically-linked @command{btrfs}
from the btrfs-progs package.  It is meant to be used in initrds.")
    (license (package-license btrfs-progs))))

(define-public cramfs-tools
  (package
    (name "cramfs-tools")
    (home-page "https://github.com/npitre/cramfs-tools")
    (version "2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32 "183rfqqyzx52q0vxicdgf0p984idh3rqkvzfb93gjvyzfhc15c0p"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; No tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "mkcramfs" (string-append out "/sbin"))
               (install-file "cramfsck" (string-append out "/sbin")))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (synopsis "Tools to manage Cramfs file systems")
    (description "Cramfs is a Linux file system designed to be simple, small,
and to compress things well.  It is used on a number of embedded systems and
small devices.  This version has additional features such as uncompressed
blocks and random block placement.")
    (license license:gpl2+)))

(define-public compsize
  (package
    (name "compsize")
    (version "1.5")
    (home-page "https://github.com/kilobyte/compsize")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32 "0vqnrwgpv6pc1yjl0g4gl71xyl6v0xl3pyqjanjpwps73c53azir"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     `(("btrfs-progs" ,btrfs-progs)))
    (arguments
     `(#:tests? #f                      ; No tests.
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "compsize" (string-append out "/bin"))
               (install-file "compsize.8" (string-append out "/share/man/man8"))))))))
    (synopsis "Find compression type/ratio on Btrfs files")
    (description "@command{compsize} takes a list of files (given as
arguments) on a Btrfs file system and measures used compression types and
effective compression ratio, producing a report.

A directory has no extents but has a (recursive) list of files.  A non-regular
file is silently ignored.

As it makes no sense to talk about compression ratio of a partial extent,
every referenced extent is counted whole, exactly once -- no matter if you use
only a few bytes of a 1GB extent or reflink it a thousand times.  Thus, the
uncompressed size will not match the number given by @command{tar} or
@command{du}.  On the other hand, the space used should be accurate (although
obviously it can be shared with files outside our set).")
    (license license:gpl2+)))

(define-public f2fs-tools-1.7
  (package
    (name "f2fs-tools")
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://git.kernel.org/cgit/linux/kernel/git/jaegeuk"
                    "/f2fs-tools.git/snapshot/f2fs-tools-" version ".tar.gz"))
              (sha256
               (base32
                "1m6bn1ibq0p53m0n97il91xqgjgn2pzlz74lb5bfzassx7159m1k"))))

    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-include (string-append out "/include")))
               (install-file "include/f2fs_fs.h" out-include)
               (install-file "mkfs/f2fs_format_utils.h" out-include)
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libuuid" ,util-linux "lib")
       ("libselinux" ,libselinux)))
    (home-page "https://f2fs.wiki.kernel.org/")
    (synopsis "Userland tools for f2fs")
    (description
     "F2FS, the Flash-Friendly File System, is a modern file system
designed to be fast and durable on flash devices such as solid-state
disks and SD cards.  This package provides the userland utilities.")
    ;; The formatting utility, libf2fs and include/f2fs_fs.h is dual
    ;; GPL2/LGPL2.1, everything else is GPL2 only. See 'COPYING'.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public f2fs-tools
  (package
    (inherit f2fs-tools-1.7)
    (name "f2fs-tools")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://git.kernel.org/cgit/linux/kernel/git/jaegeuk"
                    "/f2fs-tools.git/snapshot/f2fs-tools-" version ".tar.gz"))
              (sha256
               (base32
                "1lab1446c78xsjwhpki7s85z4171m8p9279c8yhm4882wba674k1"))))
    (inputs
     `(("libuuid" ,util-linux "lib")))))

(define-public f2fs-tools/static
  (static-package
   (package
     (inherit f2fs-tools)
     (name "f2fs-tools-static")
     (arguments
     `(#:configure-flags
       (let ((libuuid-static (assoc-ref %build-inputs "libuuid:static"))
             (libuuid (assoc-ref %build-inputs "libuuid")))
         (list
          (string-append "libuuid_CFLAGS=-I" libuuid "/include/uuid")
          (string-append "libuuid_LIBS=-L" libuuid-static "/lib -luuid")
          (string-append "libblkid_CFLAGS=-I" libuuid "/include/uuid "
                         "-I" libuuid "/include/blkid")
          (string-append "libblkid_LIBS=-L" libuuid-static "/lib -lblkid")))
       #:disallowed-references (,util-linux)
       #:phases
       (modify-phases %standard-phases ; TODO: f2fs phases.
         (add-after 'unpack 'make-static
           (lambda _
             (define (append-to-file name body)
               (let ((file (open-file name "a")))
                 (display body file)
                 (close-port file)))
             (append-to-file "mkfs/Makefile.am" "\nmkfs_f2fs_LDFLAGS = -all-static\n")
             (append-to-file "fsck/Makefile.am" "\nfsck_f2fs_LDFLAGS = -all-static\n")
             (append-to-file "tools/Makefile.am" "\nf2fscrypt_LDFLAGS = -all-static -luuid\n")
             #t))
          (add-after 'install 'remove-store-references
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Work around bug in our util-linux.
              ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41019>.
              (remove-store-references (string-append (assoc-ref outputs "out")
                                                      "/sbin/mkfs.f2fs"))
              #t)))))
     (inputs
      `(("libuuid:static" ,util-linux "static")
        ("libuuid" ,util-linux "lib")))))) ; for include files

(define-public f2fs-fsck/static
  (package
    (name "f2fs-fsck-static")
    (version (package-version f2fs-tools/static))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (srfi srfi-26))
         (let* ((f2fs-tools (assoc-ref %build-inputs "f2fs-tools-static"))
                (fsck "fsck.f2fs")
                (out (assoc-ref %outputs "out"))
                (sbin (string-append out "/sbin")))
           (mkdir-p sbin)
           (with-directory-excursion sbin
             (install-file (string-append f2fs-tools "/sbin/" fsck)
                           ".")
             (remove-store-references fsck)
             (chmod fsck #o555))
           #t))))
    (inputs
     `(("f2fs-tools-static" ,f2fs-tools/static)))
    (home-page (package-home-page f2fs-tools/static))
    (synopsis "Statically-linked fsck.f2fs command from f2fs-tools")
    (description "This package provides statically-linked fsck.f2fs command taken
from the f2fs-tools package. It is meant to be used in initrds.")
    (license (package-license f2fs-tools/static))))

(define-public freefall
  (package
    (name "freefall")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'enter-subdirectory
                    (lambda _
                      (chdir "tools/laptop/freefall")
                      #t))
                  (delete 'configure)
                  (add-before 'build 'increase-timeout
                    (lambda _
                      ;; The default of 2 seconds is too low: it assumes an
                      ;; open lid and AC power without actually checking.
                      (substitute* "freefall.c"
                        (("alarm\\(2\\)") "alarm(5)"))
                      #t)))
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:tests? #f)) ;no tests
    (home-page (package-home-page linux-libre))
    (synopsis "Free-fall protection for spinning laptop hard drives")
    (description
     "Prevents shock damage to the internal spinning hard drive(s) of some
HP and Dell laptops.  When sudden movement is detected, all input/output
operations on the drive are suspended and its heads are parked on the ramp,
where they are less likely to cause damage to the spinning disc.  Requires a
drive that supports the ATA/ATAPI-7 IDLE IMMEDIATE command with unload
feature, and a laptop with an accelerometer.  It has no effect on SSDs.")
    (license license:gpl2)))

(define-public thinkfan
  (package
    (name "thinkfan")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vmatare/thinkfan")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18vgm5w5pjnpipa34j4x87q10695w2jnqwvc2f027afy7mnzw7kz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:tests? #f                      ; no test target
       #:configure-flags
       ;; Enable reading temperatures from hard disks via S.M.A.R.T.
       ;; Upstream ‘defaults to OFF because libatasmart seems to be horribly
       ;; inefficient’.
       `("-DUSE_ATASMART:BOOL=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-init-scripts
           ;; CMakeLists.txt relies on build-time symptoms of OpenRC and
           ;; systemd to patch and install their service files.  Fake their
           ;; presence rather than duplicating the build system below.  Leave
           ;; things like ‘/bin/kill’ because they're not worth a dependency.
           ;; The sysvinit needs manual patching, but since upstream doesn't
           ;; even provide the option to install it: don't.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share/" ,name)))
               (substitute* "CMakeLists.txt"
                 (("pkg_check_modules\\((OPENRC|SYSTEMD) .*" _ package)
                  (format #f "option(~a_FOUND \"Faked\" ON)\n" package))
                 ;; That was easy!  Now we just need to fix the destinations.
                 (("/etc" directory)
                  (string-append out directory)))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libatasmart" ,libatasmart)
       ("yaml-cpp" ,yaml-cpp)))
    (home-page "https://github.com/vmatare/thinkfan")
    (synopsis "Simple fan control program")
    (description
     "Thinkfan is a simple fan control program.  It reads temperatures,
checks them against configured limits and switches to appropriate (also
pre-configured) fan level.  It requires a working @code{thinkpad_acpi} or any
other @code{hwmon} driver that enables temperature reading and fan control
from userspace.")
    (license license:gpl3+)))

(define-public tpacpi-bat
  (package
    (name "tpacpi-bat")
    (version "3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/teleshoes/tpacpi-bat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wbaz34z99gqx721alh5vmpxpj2yxg3x9m8jqyivfi1wfpwc2nd5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'refer-to-inputs
           (lambda _
             (substitute* "tpacpi-bat"
               (("cat ")
                (format #f "~a " (which "cat")))
               ;; tpacpi-bat modprobes the acpi_call kernel module if it's not
               ;; loaded.  That's the administrator's prerogative; disable it.
               (("system \"(modprobe .*)\"" _ match)
                (format #f "die \"Please run ‘~a’ first.\\n\"" match)))
             #t))
         (delete 'configure)            ; nothing to configure
         (delete 'build)                ; nothing to build
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "tpacpi-bat" bin)

               ;; There's no man page.  Install other forms of documentation.
               (for-each (lambda (file)
                           (let ((target (string-append doc "/" file)))
                             (mkdir-p (dirname target))
                             (copy-recursively file target)))
                         (list "battery_asl" "examples" "README.md"))
               #t))))))
    (inputs
     `(("perl" ,perl)))
    (home-page "https://github.com/teleshoes/tpacpi-bat")
    (synopsis "ThinkPad battery charge controller")
    (description
     "Tpacpi-bat is a command-line interface to control battery charging on
@uref{https://github.com/teleshoes/tpacpi-bat/wiki/Supported-Hardware, Lenovo
ThinkPad models released after 2011}, starting with the xx20 series.  It can
query and set the thresholds at which one or both batteries will start and stop
charging, inhibit charging batteries for a set period of time, or force them to
discharge when they otherwise would not.

This tool merely exposes ACPI calls provided by the @code{acpi_call} Linux
kernel module provided by the @code{acpi-call-linux-module} package, which must
be installed and loaded separately.  Only the original vendor firmware is
supported.")
    (license license:gpl3+)))

(define-public tmon
  (package
    (name "tmon")
    ;; Tmon's ‘VERSION = 1.0’ hasn't been touched since 2013; the code has.
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "INSTALL_ROOT=" (assoc-ref %outputs "out"))
             "BINDIR=bin")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-subdirectory
           (lambda _
             (chdir "tools/thermal/tmon")
             #t))
         (add-after 'install 'install-man-page
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man8 (string-append out "/share/man/man8")))
               (install-file "tmon.8" man8)
               #t)))
         (delete 'configure))))         ; no configure script
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page (package-home-page linux-libre))
    (synopsis "Monitor and test the Linux thermal subsystem in real time")
    (description
     "Tmon is a tool to interact with the complex thermal subsystem of the
kernel Linux.  It helps visualize thermal relationships and real-time thermal
data, tune and test cooling devices and sensors, and collect thermal data for
further analysis.

As computers become smaller and more thermally constrained, more sensors are
added and new cooling capabilities introduced.  Thermal relationships can change
dynamically.  Their complexity grows exponentially among cooling devices, zones,
sensors, and trip points.

Linux exposes this relationship through @file{/sys/class/thermal} with a matrix
of symbolic links, trip point bindings, and device instances.  To traverse it
by hand is no trivial task: @command{tmon} aims to make it understandable.")
    (license (list license:gpl2         ; the man page
                   license:gpl2+))))    ; the actual rest

(define-public turbostat
  (package
    (name "turbostat")
    ;; XXX turbostat reports a version like ‘20.09.30’ but using it here would
    ;; make it harder to benefit from ‘free’ linux-libre package updates.
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-subdirectory
           (lambda _
             (chdir "tools/power/x86/turbostat")
             #t))
         (delete 'configure))))         ; no configure script
    (inputs
     `(("libcap" ,libcap)))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page (package-home-page linux-libre))
    (synopsis "Report x86 processor frequency and idle statistics")
    (description
     "Turbostat reports x86 processor topology, frequency, idle power state
statistics, temperature, and power consumption.  Some information is unavailable
on older processors.

It can be used to identify machines that are inefficient in terms of power usage
or idle time, report the rate of @acronym{SMI, system management interrupt}s
occurring on the system, or verify the effects of power management tuning.

@command{turbostat} reads hardware counters but doesn't write to them, so it
won't interfere with the OS or other running processes---including multiple
invocations of itself.")
    (license license:gpl2)))

(define-public ntfs-3g
  (package
    (name "ntfs-3g")
    (version "2017.3.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://tuxera.com/opensource/"
                                  "ntfs-3g_ntfsprogs-" version ".tgz"))
              (patches (search-patches "ntfs-3g-CVE-2019-9755.patch"))
              (sha256
               (base32
                "1mb228p80hv97pgk3myyvgp975r9mxq56c6bdn1n24kngcfh4niy"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Install under $prefix.
                          (substitute* '("src/Makefile.in" "ntfsprogs/Makefile.in")
                            (("/sbin")
                             "@sbindir@"))
                          #t))))
    (build-system gnu-build-system)
    (inputs `(("util-linux" ,util-linux)
              ("fuse" ,fuse)))                    ;libuuid
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     '(#:configure-flags (list "--disable-static"
                               "--exec-prefix=${prefix}"
                               "--with-fuse=external" ;use our own FUSE
                               "--enable-mount-helper"
                               "--enable-posix-acls"
                               "--enable-xattr-mappings")
       #:phases
       (modify-phases %standard-phases
         ;; If users install ntfs-3g, they probably want to make it the
         ;; default driver as well, so we opt for sensible defaults and link
         ;; mount.ntfs to mount.ntfs-3g.  (libmount tries to run mount.ntfs to
         ;; mount NTFS file systems.)
         (add-after 'install 'install-link
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin")))
               (symlink "mount.ntfs-3g"
                        (string-append sbin "/mount.ntfs")))
             #t)))))
    (home-page "https://www.tuxera.com/community/open-source-ntfs-3g/")
    (synopsis "Read-write access to NTFS file systems")
    (description
     "NTFS-3G provides read-write access to NTFS file systems, which are
commonly found on Microsoft Windows.  It is implemented as a FUSE file system.
The package provides additional NTFS tools.")
    (license license:gpl2+)))

(define-public rdma-core
  (package
    (name "rdma-core")
    (version "33.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/linux-rdma/rdma-core"
                                  "/releases/download/v" version "/rdma-core-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1rah0v9gq9rksqd2c17nmydsxcjz178n7m2y4ricwlf5pq1b2yfi"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no tests
       ;; Upstream uses the "ninja" build system and encourage distros
       ;; to do the same for consistency.
       #:configure-flags (list "-GNinja"

                               (string-append "-DRST2MAN_EXECUTABLE="
                                              (assoc-ref %build-inputs
                                                         "python-docutils")
                                              "/bin/rst2man.py"))
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "ninja"
                     "-j" (number->string (parallel-job-count)))))
         (replace 'install
           (lambda _
             (invoke "ninja" "install"))))))
    (native-inputs
     `(("ninja" ,ninja)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("python-docutils" ,python-docutils)))     ;for 'rst2man'
    (inputs
     `(("libnl" ,libnl)
       ("udev" ,eudev)))
    (home-page "https://github.com/linux-rdma/rdma-core")
    (synopsis "Utilities and libraries for working with RDMA devices")
    (description
     "This package provides userspace components for the InfiniBand
subsystem of the Linux kernel.  Specifically it contains userspace
libraries for the following device nodes:

@enumerate
@item @file{/dev/infiniband/uverbsX} (@code{libibverbs})
@item @file{/dev/infiniband/rdma_cm} (@code{librdmacm})
@item @file{/dev/infiniband/umadX} (@code{libibumad})
@end enumerate

The following service daemons are also provided:
@enumerate
@item @code{srp_daemon} (for the @code{ib_srp} kernel module)
@item @code{iwpmd} (for iWARP kernel providers)
@item @code{ibacm} (for InfiniBand communication management assistant)
@end enumerate")
    ;; All library code is dual licensed under GPL2 and a custom MIT
    ;; variant. The package also includes some components covered by
    ;; other licenses. Consult COPYING.md for full details.
    (license
     (list license:gpl2
           (license:x11-style "See COPYING.BSD_MIT in the distribution")
           license:bsd-2             ; Files referring to COPYING.BSD_FB
           license:cc0               ; most files in ccan/
           license:bsd-3))))         ; providers/hfi1verbs are dual GPL2/BSD-3

(define-public perftest
  (package
    (name "perftest")
    (version "4.4-0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/linux-rdma/perftest/releases/download/v"
                           version "/perftest-" version ".g0927198.tar.gz"))
       (sha256
        (base32 "11ix4h0rrmqqyi84y55a9xnkvwsmwq0sywr46hvxzm4rqz4ma8vq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-header-paths
           (lambda _
             (substitute* '("src/raw_ethernet_fs_rate.c"
                            "src/raw_ethernet_resources.c"
                            "src/raw_ethernet_resources.h"
                            "src/raw_ethernet_send_burst_lat.c"
                            "src/raw_ethernet_send_bw.c"
                            "src/raw_ethernet_send_lat.c")
               (("/usr/include/netinet/ip.h") "netinet/ip.h"))
             #t)))))
    (inputs `(("rdma-core" ,rdma-core)))
    (home-page "https://github.com/linux-rdma/perftest/")
    (synopsis "Open Fabrics Enterprise Distribution (OFED) Performance Tests")
    (description "This is a collection of tests written over uverbs intended for
use as a performance micro-benchmark. The tests may be used for hardware or
software tuning as well as for functional testing.

The collection contains a set of bandwidth and latency benchmark such as:
@enumerate
@item Send        - @code{ib_send_bw} and @code{ib_send_lat}
@item RDMA Read   - @code{ib_read_bw} and @code{ib_read_lat}
@item RDMA Write  - @code{ib_write_bw} and @code{ib_wriet_lat}
@item RDMA Atomic - @code{ib_atomic_bw} and @code{ib_atomic_lat}
@item Native Ethernet (when working with MOFED2) - @code{raw_ethernet_bw}, @code{raw_ethernet_lat}
@end enumerate")
    (license license:gpl2)))

(define-public rng-tools
  (package
    (name "rng-tools")
    (home-page "https://github.com/nhorman/rng-tools")
    (version "6.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z4j3kqh9k3zsrx6257hwh4fa51vqg79c6dnfrj6lhpcll0wh0hm"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Disable support for various hardware entropy sources as they need
       ;; dependencies that are not yet in Guix, and would significantly
       ;; increase closure size.
       #:configure-flags '("--without-nistbeacon"
                           "--without-pkcs11"
                           "--without-rtlsdr")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libsysfs" ,sysfsutils)
       ("openssl" ,openssl)))
    (synopsis "Random number generator daemon")
    (description
     "Monitor a hardware random number generator, and supply entropy
from that to the system kernel's @file{/dev/random} machinery.")
    ;; The source package is offered under the GPL2+, but the files
    ;; 'rngd_rdrand.c' and 'rdrand_asm.S' are only available under the GPL2.
    (license (list license:gpl2 license:gpl2+))))

(define-public cpupower
  (package
    (name "cpupower")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'enter-subdirectory
                    (lambda _
                      (chdir "tools/power/cpupower")
                      #t))
                  (delete 'configure)
                  (add-before 'build 'fix-makefiles
                    (lambda _
                      (substitute* "Makefile"
                        (("/usr/") "/")
                        (("/bin/(install|pwd)" _ command) command))
                      (substitute* "bench/Makefile"
                        (("\\$\\(CC\\) -o") "$(CC) $(LDFLAGS) -o"))
                      #t)))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "DESTDIR=" out)
                            (string-append "LDFLAGS=-Wl,-rpath=" out "/lib")
                            "libdir=/lib"
                            "docdir=/share/doc/cpupower"
                            "confdir=$(docdir)/examples"
                            ;; The Makefile recommends the following changes
                            "DEBUG=false"
                            "PACKAGE_BUGREPORT=bug-guix@gnu.org"))
       #:tests? #f)) ;no tests
    (native-inputs `(("gettext" ,gettext-minimal)))
    (inputs `(("pciutils" ,pciutils)))
    (home-page (package-home-page linux-libre))
    (synopsis "CPU frequency and voltage scaling tools for Linux")
    (description
     "cpupower is a set of user-space tools that use the cpufreq feature of the
Linux kernel to retrieve and control processor features related to power saving,
such as frequency and voltage scaling.")
    (license license:gpl2)))

(define-public x86-energy-perf-policy
  (package
    (name "x86-energy-perf-policy")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-subdirectory
           (lambda _
             (chdir "tools/power/x86/x86_energy_perf_policy")
             #t))
         (delete 'configure)
         (add-before 'build 'fix-makefile
           (lambda _
             (substitute* "Makefile" (("/usr") ""))
             #t)))
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "DESTDIR=" out)
               (string-append "LDFLAGS=-Wl,-rpath=" out "/lib")))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page (package-home-page linux-libre))
    (synopsis "Display and update Intel-CPU energy-performance policy")
    (description
     "@command{x86_energy_perf_policy} displays and updates energy-performance
policy settings specific to Intel Architecture Processors.  Settings are
accessed via Model Specific Register (MSR) updates, no matter if the Linux
cpufreq sub-system is enabled or not.")
    (license license:gpl2)))

(define-public haveged
  (package
    (name "haveged")
    (version "1.9.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jirka-h/haveged")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "050hmnh5s2s4mb83f4d1fk23mk27pprg840c1aagc6v1sz6x5dhq"))))
    (build-system gnu-build-system)
    (home-page "https://www.issihosts.com/haveged")
    (synopsis "Entropy source for the Linux random number generator")
    (description
     "haveged generates an unpredictable stream of random numbers for use by
Linux's @file{/dev/random} and @file{/dev/urandom} devices.  The kernel's
standard mechanisms for filling the entropy pool may not be sufficient for
systems with high needs or limited user interaction, such as headless servers.

@command{haveged} runs as a privileged daemon, harvesting randomness from the
indirect effects of hardware events on hidden processor state using the
@acronym{HAVEGE, HArdware Volatile Entropy Gathering and Expansion} algorithm.
It tunes itself to its environment and provides the same built-in test suite
for the output stream as used on certified hardware security devices.

The quality of the randomness produced by this algorithm has not been proven.
It is recommended to run it together with another entropy source like rngd, and
not as a replacement for it.")
    (license (list (license:non-copyleft "file://nist/mconf.h")
                   (license:non-copyleft "file://nist/packtest.c")
                   license:public-domain        ; nist/dfft.c
                   license:gpl3+))))            ; everything else

(define-public ecryptfs-utils
  (package
    (name "ecryptfs-utils")
    (version "111")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/ecryptfs/trunk/"
                           version "/+download/ecryptfs-utils_"
                           version ".orig.tar.gz"))
       (sha256
        (base32
         "0zwq19siiwf09h7lwa7n7mgmrr8cxifp45lmwgcfr8c1gviv6b0i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--disable-pywrap")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-hardcoded-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (utils-linux (assoc-ref inputs "utils-linux"))
                   (cryptsetup (assoc-ref inputs "cryptsetup"))
                   (linux-pam (assoc-ref inputs "linux-pam"))
                   (lvm2 (assoc-ref inputs "lvm2")))
               (substitute* '("src/utils/ecryptfs-mount-private"
                              "src/utils/ecryptfs-umount-private"
                              "src/utils/ecryptfs-setup-private"
                              "src/utils/ecryptfs-setup-swap"
                              "src/utils/mount.ecryptfs.c"
                              "src/utils/umount.ecryptfs.c"
                              "src/pam_ecryptfs/pam_ecryptfs.c"
                              "src/desktop/ecryptfs-mount-private.desktop.in"
                              "src/desktop/ecryptfs-setup-private.desktop.in")
                 (("/bin/mount")
                  (string-append utils-linux "/bin/mount"))
                 (("/bin/umount")
                  (string-append utils-linux "/bin/umount"))
                 (("/sbin/mount.ecryptfs_private")
                  (string-append out "/sbin/mount.ecryptfs_private"))
                 (("/sbin/umount.ecryptfs_private")
                  (string-append out "/sbin/umount.ecryptfs_private"))
                 (("/usr/bin/ecryptfs-mount-private")
                  (string-append out "/bin/ecryptfs-mount-private"))
                 (("/usr/bin/ecryptfs-rewrite-file")
                  (string-append out "/bin/ecryptfs-rewrite-file"))
                 (("/usr/bin/ecryptfs-setup-private")
                  (string-append out "/bin/ecryptfs-setup-private"))
                 (("/sbin/cryptsetup")
                  (string-append cryptsetup "/sbin/cryptsetup"))
                 (("/sbin/unix_chkpwd")
                  (string-append linux-pam "/sbin/unix_chkpwd"))
                 (("/sbin/dmsetup")
                  (string-append lvm2 "/sbin/dmsetup")))))))))
    (native-inputs
     `(("intltool" ,intltool)
       ("perl" ,perl)                   ; for pod2man
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("keyutils" ,keyutils)
       ("linux-pam" ,linux-pam)
       ("utils-linux" ,util-linux)
       ("cryptsetup" ,cryptsetup)
       ("lvm2" ,lvm2)
       ("nss" ,nss)))
    (home-page "https://ecryptfs.org/")
    (synopsis "eCryptfs cryptographic file system utilities")
    (description
     "eCryptfs is a POSIX-compliant stacked cryptographic file system for Linux.
Each file's cryptographic meta-data is stored inside the file itself, along
with the encrypted contents.  This allows individual encrypted files to be
copied between hosts and still be decrypted with the proper key.  eCryptfs is a
native Linux file system, and has been part of the Linux kernel since version
2.6.19.  This package contains the userland utilities to manage it.")
    ;; The files src/key_mod/ecryptfs_key_mod_{openssl,pkcs11_helper,tspi}.c
    ;; grant additional permission to link with OpenSSL.
    (license license:gpl2+)))

(define-public libnfsidmap
  (package
    (name "libnfsidmap")
    (version "0.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://fedorapeople.org/~steved/"
                           name "/" version "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "0bg2bcii424mf1bnp3fssr8jszbvhdxl7wvifm1yf6g596v8b8i5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list
                          (string-append "--with-pluginpath="
                                         (assoc-ref %outputs "out")
                                         "/lib/libnfsidmap"))))
    (native-inputs
     `(("autoconf" ,autoconf)))         ; 0.27 still needs autoheader
    (home-page
     "http://www.citi.umich.edu/projects/nfsv4/crossrealm/libnfsidmap_config.html")
    (synopsis "NFSv4 support library for name/ID mapping")
    (description "Libnfsidmap is a library holding mulitiple methods of
mapping names to ids and visa versa, mainly for NFSv4.  It provides an
extensible array of mapping functions, currently consisting of two choices:
the default @code{nsswitch} and the experimental @code{umich_ldap}.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))))

(define-public module-init-tools
  (package
    (name "module-init-tools")
    (version "3.16")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kernel.org/linux/utils/kernel/module-init-tools/"
                   "module-init-tools-" version ".tar.bz2"))
             (sha256
              (base32
               "0jxnz9ahfic79rp93l5wxcbgh4pkv85mwnjlbv1gz3jawv5cvwp1"))
             (patches (search-patches "module-init-tools-moduledir.patch"))))
    (build-system gnu-build-system)
    (arguments
     ;; FIXME: The upstream tarball lacks man pages, and building them would
     ;; require DocBook & co.  We used to use Gentoo's pre-built man pages,
     ;; but they vanished.  In the meantime, fake it.
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fake-docbook
           (lambda _
             (substitute* "Makefile.in"
               (("^DOCBOOKTOMAN.*$")
                "DOCBOOKTOMAN = true\n"))
             #t)))))
    (home-page "https://www.kernel.org/pub/linux/utils/kernel/module-init-tools/")
    (synopsis "Tools for loading and managing Linux kernel modules")
    (description
     "Tools for loading and managing Linux kernel modules, such as
@code{modprobe}, @code{insmod}, @code{lsmod}, and more.")
    (license license:gpl2+)))

(define-public mcelog
  (package
    (name "mcelog")
    (version "175")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/utils/cpu/mce/mcelog.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vvrnjkh1jp7f6295syydg7lplqmcm8msdls3xyk8xfiz69xqdjz"))
       (modules '((guix build utils)))
       (snippet
        `(begin
           ;; The checkout lack a .git directory, breaking ‘git describe’.
           (substitute* "Makefile"
             (("\"unknown\"") (string-append "\"v" ,version "\"")))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))  ; no configure script
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "CC=" ,(cc-for-target))
                            (string-append "prefix=" out)
                            (string-append "DOCDIR=" out "/share/doc/"
                                           ,name "-" ,version)
                            "etcprefix=$(DOCDIR)/examples"))
       ;; The tests will only run as root on certain supported CPU models.
       #:tests? #f))
    (supported-systems (list "i686-linux" "x86_64-linux"))
    (home-page "https://mcelog.org/")
    (synopsis "Machine check monitor for x86 Linux systems")
    (description
     "The mcelog daemon logs memory, I/O, CPU, and other hardware errors on x86
systems running the kernel Linux.  It can also perform user-defined tasks, such
as bringing bad pages off-line, when configurable error thresholds are
exceeded.")
    (license license:gpl2)))

(define-public mtd-utils
  (package
    (name "mtd-utils")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.infradead.org/pub/mtd-utils/"
                    "mtd-utils-" version ".tar.bz2"))
              (sha256
               (base32
                "1mp9fqgnz5r69s8ly98ry6k2blqnaqpllwi8m930dm0n8zrwbm4a"))))
    (arguments
     '(#:configure-flags '("--enable-unit-tests")))
    (native-inputs
     `(("cmocka" ,cmocka)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("acl" ,acl)                     ; extended attributes (xattr)
       ("libuuid" ,util-linux "lib")
       ("lzo" ,lzo)
       ("openssl" ,openssl)             ; optional crypto support
       ("zlib" ,zlib)
       ("zstd" ,zstd "lib")))
    (build-system gnu-build-system)
    (synopsis "MTD Flash Storage Utilities")
    (description "This package provides utilities for testing, partitioning, etc
of flash storage.")
    (home-page "http://www.linux-mtd.infradead.org/")
    (license
      (list license:gpl2 ; Almost everything is gpl2 or gpl2+
            license:mpl1.1 ; All ftl* files
            license:expat)))) ; libiniparser

(define-public libseccomp
  (package
    (name "libseccomp")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/seccomp/libseccomp/"
                                  "releases/download/v" version
                                  "/libseccomp-" version ".tar.gz"))
              (sha256
               (base32
                "0m8dlg1v7kflcxvajs4p76p275qwsm2abbf5mfapkakp7hw7wc7f"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'skip-load-test
                    (lambda _
                      ;; This test does a native system call and fails when
                      ;; run under QEMU user-mode emulation.  Just skip it.
                      (delete-file "tests/52-basic-load.tests")
                      #t)))))
    (native-inputs
     `(("gperf" ,gperf)
       ("which" ,which)))
    (synopsis "Interface to Linux's seccomp syscall filtering mechanism")
    (description "The libseccomp library provides an easy to use, platform
independent, interface to the Linux Kernel's syscall filtering mechanism.  The
libseccomp API is designed to abstract away the underlying BPF based syscall
filter language and present a more conventional function-call based filtering
interface that should be familiar to, and easily adopted by, application
developers.")
    (home-page "https://github.com/seccomp/libseccomp")
    (license license:lgpl2.1)))

(define-public radeontop
  (package
    (name "radeontop")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clbr/radeontop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ay6vl9zsz9b2scy0fnsy482pzizj52i27syxwny4z4i9wrk2wmn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  ;; getver.sh uses ‘git --describe’, isn't worth an extra git
                  ;; dependency, and doesn't even work on release(!) tarballs.
                  (add-after 'unpack 'report-correct-version
                    (lambda _
                      (substitute* "getver.sh"
                        (("ver=unknown")
                         (string-append "ver=" ,version)))
                      #t))
                  (delete 'configure))  ; no configure script
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:tests? #f))                    ; no tests
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libdrm" ,libdrm)
       ("libpciaccess" ,libpciaccess)
       ("libxcb" ,libxcb)
       ("ncurses" ,ncurses)))
    (home-page "https://github.com/clbr/radeontop/")
    (synopsis "Usage monitor for AMD Radeon graphics")
    (description "RadeonTop monitors resource consumption on supported AMD
Radeon Graphics Processing Units (GPUs), either in real time as bar graphs on
a terminal or saved to a file for further processing.  It measures both the
activity of the GPU as a whole, which is also accurate during OpenCL
computations, as well as separate component statistics that are only meaningful
under OpenGL graphics workloads.")
    (license license:gpl3)))

(define-public efivar
  (package
    (name "efivar")
    (version "37")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rhboot/" name
                                  "/releases/download/" version "/" name
                                  "-" version ".tar.bz2"))
              (sha256
               (base32
                "17vvfivhsrszh7q39b6npjsrhrhsjf1cmmcpp3xrh6wh7ywzwrrw"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Tests require a UEFI system and is not detected in the chroot.
       #:tests? #f
       #:make-flags (list (string-append "prefix=" %output)
                          (string-append "libdir=" %output "/lib")
                          "CC_FOR_BUILD=gcc"
                          (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("popt" ,popt)))
    (home-page "https://github.com/rhboot/efivar")
    (synopsis "Tool and library to manipulate EFI variables")
    (description "This package provides a library and a command line
interface to the variable facility of UEFI boot firmware.")
    (license license:lgpl2.1+)))

(define-public efibootmgr
  (package
    (name "efibootmgr")
    (version "17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rhinstaller/efibootmgr")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1niicijxg59rsmiw3rsjwy4bvi1n42dynvm01lnp9haixdzdpq03"))
       (patches (search-patches "efibootmgr-remove-extra-decl.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:make-flags (list (string-append "prefix=" %output)
                          (string-append "libdir=" %output "/lib")
                          ;; EFIDIR denotes a subdirectory relative to the
                          ;; EFI System Partition where the loader will be
                          ;; installed (known as OS_VENDOR in the code).
                          ;; GRUB overrides this, as such it's only used if
                          ;; nothing else is specified on the command line.
                          "EFIDIR=gnu")
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("efivar" ,efivar)
       ("popt" ,popt)))
    (home-page "https://github.com/rhinstaller/efibootmgr")
    (synopsis "Modify the Extensible Firmware Interface (EFI) boot manager")
    (description
     "@code{efibootmgr} is a user-space application to modify the Intel
Extensible Firmware Interface (EFI) Boot Manager.  This application can
create and destroy boot entries, change the boot order, change the next
running boot option, and more.")
    (license license:gpl2+)))

(define-public sysstat
  (package
    (name "sysstat")
    (version "12.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://pagesperso-orange.fr/sebastien.godard/"
                           "sysstat-" version ".tar.xz"))
       (sha256
        (base32 "1z8bdyj92q0capbrdscwzb51bqh54ng15gpvmjmvrb2syhqj8hxf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite.
       ;; Without this flag, it tries to install the man pages with group 'root'
       ;; and fails because /etc/passwd lacks an entry for the root user.
       #:configure-flags
       (list "--disable-file-attr"
             (string-append "conf_dir=" (assoc-ref %outputs "out") "/etc"))
       #:phases
       (modify-phases %standard-phases
         ;; The build process tries to create '/var/lib/sa', so we skip that
         ;; instruction.
         (add-after 'build 'skip-touching-var
           (lambda _
             (substitute* "Makefile"
               (("mkdir -p \\$\\(DESTDIR\\)\\$\\(SA_DIR\\)")
                ""))
             #t)))))
    (home-page "http://sebastien.godard.pagesperso-orange.fr/")
    (synopsis "Performance monitoring tools for Linux")
    (description "The sysstat utilities are a collection of performance
monitoring tools for Linux.  These include @code{mpstat}, @code{iostat},
@code{tapestat}, @code{cifsiostat}, @code{pidstat}, @code{sar}, @code{sadc},
@code{sadf} and @code{sa}.")
    (license license:gpl2+)))

(define-public light
  (package
    (name "light")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/haikarainen/light")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1a70zcf88ifsnwll486aicjnh48zisdf8f7vi34ihw61kdadsq9s"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-udev-rules-absolute-path-bins
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "90-backlight.rules"
               (("/bin/chgrp") (which "chgrp"))
               (("/bin/chmod") (which "chmod")))
             #t))
         (add-after 'install 'install-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file
                "90-backlight.rules" (string-append out "/lib/udev/rules.d"))
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (home-page "https://haikarainen.github.io/light/")
    (synopsis "GNU/Linux application to control backlights")
    (description
     "Light is a program to send commands to screen backlight controllers
under GNU/Linux.  Features include:

@itemize
@item It does not rely on X.
@item Light can automatically figure out the best controller to use, making
full use of underlying hardware.
@item It is possible to set a minimum brightness value, as some controllers
set the screen to be pitch black at a value of 0 (or higher).
@end itemize\n")
    (license license:gpl3+)))

(define-public brightnessctl
  (package
    (name "brightnessctl")
    (version "0.5.1")
    (home-page "https://github.com/Hummer12007/brightnessctl")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0immxc7almmpg80n3bdn834p3nrrz7bspl2syhb04s3lawa5y2lq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output)
                          (string-append "UDEVDIR=" %output "/lib/udev/rules.d/"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'adjust-udev-rules
           (lambda _
             (substitute* "90-brightnessctl.rules"
               (("/bin/") "/run/current-system/profile/bin/"))
             #t)))))
    (synopsis "Backlight and LED brightness control")
    (description
     "This program allows you read and control device brightness.  Devices
include backlight and LEDs.  It can also preserve current brightness before
applying the operation, such as on lid close.

The appropriate permissions must be set on the backlight or LED control
interface in sysfs, which can be accomplished with the included udev rules.")
    (license license:expat)))

(define-public tlp
  (package
    (name "tlp")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linrunner/TLP")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14fcnaz9pw534v4d8dddqq4wcvpf1kghr8zlrk62r5lrl46sp1p5"))))
    (native-inputs
     `(("shellcheck" ,shellcheck)))
    (inputs
     `(("bash" ,bash)
       ("dbus" ,dbus)
       ("ethtool" ,ethtool)
       ("eudev" ,eudev)
       ("grep" ,grep)
       ("hdparm" ,hdparm)
       ("inetutils" ,inetutils)
       ("iw" ,iw)
       ("kmod" ,kmod)
       ("pciutils" ,pciutils)
       ("perl" ,perl)
       ("rfkill" ,rfkill)
       ("sed" ,sed)
       ("usbutils" ,usbutils)
       ("util-linux" ,util-linux)
       ("wireless-tools" ,wireless-tools)
       ,@(if (let ((system (or (%current-target-system)
                               (%current-system))))
               (or (string-prefix? "i686-" system)
                   (string-prefix? "x86_64-" system)))
             `(("x86-energy-perf-policy" ,x86-energy-perf-policy))
             '())))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: The full test suite is run with "checkall" but it requires
     ;; "checkbashisms" and "perlcritic", not yet packaged in Guix.
     `(#:test-target "shellcheck"
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'build 'setenv
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "TLP_WITH_SYSTEMD" "0")
               (setenv "TLP_NO_INIT" "1")
               (setenv "TLP_NO_PMUTILS" "1")
               (setenv "TLP_SBIN" (string-append out "/bin"))
               (setenv "TLP_BIN" (string-append out "/bin"))
               (setenv "TLP_TLIB" (string-append out "/share/tlp"))
               (setenv "TLP_FLIB" (string-append out "/share/tlp/func.d"))
               (setenv "TLP_ULIB" (string-append out "/lib/udev"))
               (setenv "TLP_CONFDEF"
                       (string-append out "/share/tlp/defaults.conf"))
               (setenv "TLP_CONFDIR" (string-append out "/etc/tlp.d"))
               (setenv "TLP_ELOD"
                       (string-append out "/lib/elogind/system-sleep"))
               (setenv "TLP_SHCPL"
                       (string-append out "/share/bash-completion/completions"))
               (setenv "TLP_MAN" (string-append out "/share/man"))
               (setenv "TLP_META" (string-append out "/share/metainfo"))
               #t)))
         (add-before 'install 'fix-installation
           (lambda _
             ;; Stop the Makefile from trying to create system directories.
             (substitute* "Makefile"
               (("\\[ -f \\$\\(_CONFUSR\\) \\]") "#")
               (("install -d -m 755 \\$\\(_VAR\\)") "#"))
             #t))
         (replace 'install
           (lambda _ (invoke "make" "install-tlp" "install-man-tlp")))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
                    (bin-files (find-files bin ".*")))
               (define (bin-directory input-name)
                 (let ((p (assoc-ref inputs input-name)))
                   (and p (string-append p "/bin"))))
               (define (sbin-directory input-name)
                 (string-append (assoc-ref inputs input-name) "/sbin"))
               (for-each (lambda (program)
                           (wrap-program program
                             `("PATH" ":" prefix
                               ,(append
                                 (filter-map bin-directory
                                             '("bash"
                                               "coreutils"
                                               "dbus"
                                               "eudev"
                                               "grep"
                                               "inetutils"
                                               "kmod"
                                               "perl"
                                               "sed"
                                               "usbutils"
                                               "util-linux"
                                               "x86-energy-perf-policy"))
                                 (filter-map sbin-directory
                                             '("ethtool"
                                               "hdparm"
                                               "iw"
                                               "pciutils"
                                               "rfkill"
                                               "wireless-tools"))))))
                         bin-files)
               #t))))))
    (home-page "https://linrunner.de/en/tlp/tlp.html")
    (synopsis "Power management tool for Linux")
    (description "TLP is a power management tool for Linux.  It comes with
a default configuration already optimized for battery life.  Nevertheless,
TLP is customizable to fulfil system requirements.  TLP settings are applied
every time the power supply source is changed.")
    ;; 'COPYING' is a custom version that says that one file is GPLv3+ and the
    ;; rest is GPLv2+.
    (license (list license:gpl2+ license:gpl3+))))

(define-public lshw
  (package
    (name "lshw")
    (version "B.02.19.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.ezix.org/software/"
                                  "files/lshw-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "100gm1c6gb2hkfws22h0xhvv7nz38p49lxd1csikj8qlhyn4gcwv"))))
    (build-system gnu-build-system)
    (arguments
      `(#:phases (modify-phases %standard-phases (delete 'configure))
        #:tests? #f ; no tests
        #:make-flags
          (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (synopsis "List hardware information")
    (description
     "@command{lshw} (Hardware Lister) is a small tool to provide
detailed information on the hardware configuration of the machine.
It can report exact memory configuration, firmware version, mainboard
configuration, CPU version and speed, cache configuration, bus speed,
and more on DMI-capable x86 or EFI (IA-64) systems and on some PowerPC
machines (PowerMac G4 is known to work).")
    (home-page "https://www.ezix.org/project/wiki/HardwareLiSter")
    (license license:gpl2+)))

(define-public libmnl
  (package
    (name "libmnl")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://netfilter.org/libmnl/"
                            "libmnl-" version ".tar.bz2"))
        (sha256
         (base32
          "108zampspaalv44zn0ar9h386dlfixpd149bnxa5hsi8kxlqj7qp"))))
    (build-system gnu-build-system)
    (home-page "https://www.netfilter.org/projects/libmnl/")
    (synopsis "Netlink utility library")
    (description "Libmnl is a minimalistic user-space library oriented to
Netlink developers.  There are a lot of common tasks in parsing, validating,
constructing of both the Netlink header and TLVs that are repetitive and easy to
get wrong.  This library aims to provide simple helpers that allows you to
re-use code and to avoid re-inventing the wheel.")
    (license license:lgpl2.1+)))

(define-public libnftnl
  (package
    (name "libnftnl")
    (version "1.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://netfilter.org/libnftnl/"
                           "libnftnl-" version ".tar.bz2"))
       (sha256
        (base32 "16jbp4fs5dz2yf4c3bl1sb48x9x9wi1chv39zwmfgya1k9pimcp9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libmnl" ,libmnl)))
    (home-page "https://www.netfilter.org/projects/libnftnl/index.html")
    (synopsis "Netlink programming interface to the Linux nf_tables subsystem")
    (description "Libnftnl is a userspace library providing a low-level netlink
programming interface to the in-kernel nf_tables subsystem.  The library
libnftnl has been previously known as libnftables.  This library is currently
used by nftables.")
    (license license:gpl2+)))

(define-public nftables
  (package
    (name "nftables")
    (version "0.9.8")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://netfilter.org/nftables/nftables-"
                                 version ".tar.bz2")
                  (string-append "https://www.nftables.org/projects/nftables"
                                 "/files/nftables-" version ".tar.bz2")))
       (sha256
        (base32 "1r4g22grhd4s1918wws9vggb8821sv4kkj8197ygxr6sar301z30"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags
                 '("--disable-man-doc"))) ; FIXME: Needs docbook2x.
    (inputs `(("gmp" ,gmp)
              ("libmnl" ,libmnl)
              ("libnftnl" ,libnftnl)
              ("readline" ,readline)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("bison" ,bison)
                     ("flex" ,flex)))
    (home-page "https://www.nftables.org")
    (synopsis "Userspace utility for Linux packet filtering")
    (description "nftables is the project that aims to replace the existing
{ip,ip6,arp,eb}tables framework.  Basically, this project provides a new packet
filtering framework, a new userspace utility and also a compatibility layer for
{ip,ip6}tables.  nftables is built upon the building blocks of the Netfilter
infrastructure such as the existing hooks, the connection tracking system, the
userspace queueing component and the logging subsystem.")
    (license license:gpl2)))

(define-public proot
  (package
    (name "proot")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/proot-me/PRoot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0azsqis99gxldmbcg43girch85ysg4hwzf0h1b44bmapnsm89fbz"))
       (patches (search-patches "proot-test-fhs.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("-C" "src")

       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'set-shell-file-name
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* (find-files "src" "\\.[ch]$")
                        (("\"/bin/sh\"")
                         (string-append "\""
                                        (assoc-ref inputs "bash")
                                        "/bin/sh\"")))
                      #t))
                  (add-before 'check 'fix-fhs-assumptions-in-tests
                    (lambda _
                      (substitute* "tests/test-c6b77b77.mk"
                        (("/bin/bash") (which "bash"))
                        (("/usr/bin/test") (which "test")))
                      (substitute* '("tests/test-16573e73.c")
                        (("/bin/([a-z-]+)" _ program)
                         (which program)))

                      (substitute* (find-files "tests" "\\.sh$")
                        ;; Some of the tests try to "bind-mount" /bin/true.
                        (("-b /bin/true:")
                         (string-append "-b " (which "true") ":"))
                        ;; Likewise for /bin.
                        (("-b /bin:") "-b /gnu:")
                        ;; Others try to run /bin/sh.
                        (("/bin/sh") (which "sh"))
                        ;; Others assume /etc/fstab exists.
                        (("/etc/fstab") "/etc/passwd"))

                      (substitute* "tests/GNUmakefile"
                        (("-b /bin:") "-b /gnu:"))

                      ;; XXX: This test fails in an obscure corner case, just
                      ;; skip it.
                      (delete-file "tests/test-kkkkkkkk.c")

                      #t))
                  (replace 'check
                    (lambda _
                      (let ((n (parallel-job-count)))
                        ;; For some reason we get lots of segfaults with
                        ;; seccomp support (x86_64, Linux-libre 4.11.0).
                        (setenv "PROOT_NO_SECCOMP" "1")

                        ;; Most of the tests expect "/bin" to be in $PATH so
                        ;; they can run things that live in $ROOTFS/bin.
                        (setenv "PATH"
                                (string-append (getenv "PATH") ":/bin"))

                        (invoke "make" "check" "-C" "tests"
                                ;;"V=1"
                                "-j" (number->string n)))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; The 'install' rule does nearly nothing.
                      (let* ((out (assoc-ref outputs "out"))
                             (man1 (string-append out "/share/man/man1")))
                        ;; TODO: 'make install-care' (does not even
                        ;; build currently.)
                        (invoke "make" "-C" "src" "install"
                                (string-append "PREFIX=" out))

                        (mkdir-p man1)
                        (copy-file "doc/proot/man.1"
                                   (string-append man1 "/proot.1"))
                        #t))))))
    (native-inputs `(("which" ,which)

                     ;; For 'mcookie', used by some of the tests.
                     ("util-linux" ,util-linux)

                     ;; XXX: Choose the old coreutils because its 'stat'
                     ;; program does not use statx(2) when running 'stat -c
                     ;; %a' or similar, which PRoot doesn't properly support.
                     ("coreutils-old" ,coreutils-8.30)

                     ;; XXX: 'test-c6b77b77.sh' runs 'make' and that leads
                     ;; make 4.3 to segfault.
                     ("make-old" ,gnu-make-4.2)))
    (inputs `(("talloc" ,talloc)))
    (home-page "https://github.com/proot-me/PRoot")
    (synopsis "Unprivileged chroot, bind mount, and binfmt_misc")
    (description
     "PRoot is a user-space implementation of @code{chroot}, @code{mount --bind},
and @code{binfmt_misc}.  This means that users don't need any privileges or
setup to do things like using an arbitrary directory as the new root
file system, making files accessible somewhere else in the file system
hierarchy, or executing programs built for another CPU architecture
transparently through QEMU user-mode.  Also, developers can use PRoot as a
generic process instrumentation engine thanks to its extension mechanism.
Technically PRoot relies on @code{ptrace}, an unprivileged system-call
available in the kernel Linux.")
    (license license:gpl2+)))

(define-public proot-static
  (package
    (inherit proot)
    (name "proot-static")
    (synopsis
     "Unprivileged chroot, bind mount, and binfmt_misc (statically linked)")
    (inputs `(("talloc" ,talloc/static)))
    (arguments
     (substitute-keyword-arguments (package-arguments proot)
       ((#:make-flags flags)
        `(cons "LDFLAGS = -ltalloc -static -static-libgcc" ,flags))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'strip 'remove-store-references
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (with-directory-excursion out
                   (remove-store-references "bin/proot")
                   #t))))))
       ((#:allowed-references _ '("out"))
        '("out"))))))

(define-public cpuid
  (package
    (name "cpuid")
    (version "20201006")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.etallen.com/cpuid/cpuid-"
                                  version ".src.tar.gz"))
              (sha256
               (base32
                "19jnkh57f979b78ak5mpxmdvnkgc33r55cw9shgd2hc380b3zi8k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target)))
       #:tests? #f                      ; no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ; no configure script
                  (add-before 'install 'fix-makefile
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "Makefile"
                        (("\\$\\(BUILDROOT\\)/usr") (assoc-ref outputs "out")))
                      ;; Make the compressed manpages writable so that the
                      ;; reset-gzip-timestamps phase does not error out.
                      (substitute* "Makefile"
                        (("-m 444") "-m 644"))
                      #t)))))
    (inputs `(("perl" ,perl)))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "http://www.etallen.com/cpuid.html")
    (synopsis "Linux tool to dump x86 CPUID information about the CPU(s)")
    (description "cpuid dumps detailed information about the CPU(s) gathered
from the CPUID instruction, and also determines the exact model of CPU(s).  It
supports Intel, AMD, and VIA CPUs, as well as older Transmeta, Cyrix, UMC,
NexGen, Rise, and SiS CPUs.")
    (license license:gpl2+)))

(define-public jmtpfs
  (package
    (name "jmtpfs")
    (version "0.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JasonFerrara/jmtpfs")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1pm68agkhrwgrplrfrnbwdcvx5lrivdmqw8pb5gdmm3xppnryji1"))))
    (build-system gnu-build-system)
    (inputs
     `(("file" ,file)
       ("fuse" ,fuse)
       ("libmtp" ,libmtp)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/JasonFerrara/jmtpfs")
    (synopsis "Use a FUSE file system to access data over MTP")
    (description "jmtpfs uses FUSE (file system in userspace) to provide access
to data over the Media Transfer Protocol (MTP).  Unprivileged users can mount
the MTP device as a file system.")
    (license license:gpl3)))

(define-public procenv
  (package
   (name "procenv")
   (version "0.55")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
            (url "https://github.com/jamesodhunt/procenv")
            (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "13fnr4gfj2xlxghw93m5gsxzlwzv3s6jv9hja0w0pb23hlncbmhy"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("pkg-config" ,pkg-config)

      ;; For tests.
      ("check" ,check)
      ("groff" ,groff)))
   (inputs
    `(("expat" ,expat)
      ("libcap" ,libcap)
      ("libselinux" ,libselinux)))
   (synopsis "Utility to show process environment")
   (description "Procenv is a command-line tool that displays as much detail about
itself and its environment as possible.  It can be used as a test
tool, to understand the type of environment a process runs in, and for
comparing system environments.")
   (home-page "https://github.com/jamesodhunt/procenv/")
   (license license:gpl3+)))

(define-public libfabric
  (package
    (name "libfabric")
    (version "1.11.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ofiwg/libfabric/releases/download/v"
                       version "/libfabric-" version ".tar.bz2"))
       (sha256
        (base32 "1nnpfkwxhim2nqjkb1vwrb4wj4j3l6w6yvvy69fqam2snlhshazz"))))
    (build-system gnu-build-system)
    (inputs `(("rdma-core" ,rdma-core)
              ,@(match (%current-system)
                       ((member (package-supported-systems psm))
                        `(("psm" ,psm)))
                       (_ `()))
              ("libnl" ,libnl)))
    (home-page "https://ofiwg.github.io/libfabric/")
    (synopsis "Open Fabric Interfaces")
    (description
     "OpenFabrics Interfaces (OFI) is a framework focused on exporting fabric
communication services to applications.  OFI is best described as a collection
of libraries and applications used to export fabric services.  The key
components of OFI are: application interfaces, provider libraries, kernel
services, daemons, and test applications.

Libfabric is a core component of OFI.  It is the library that defines and
exports the user-space API of OFI, and is typically the only software that
applications deal with directly.  It works in conjunction with provider
libraries, which are often integrated directly into libfabric.")
    (license (list license:bsd-2 license:gpl2)))) ;dual

(define-public psm
  (package
    (name "psm")
    (version "3.3.20170428")
    (home-page "https://github.com/intel/psm")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page)
                           (commit "604758e76dc31e68d1de736ccf5ddf16cb22355b")))
       (file-name (string-append "psm-" version ".tar.gz"))
       (sha256
        (base32 "0nsb325dmhn5ia3d2cnksqr0gdvrrx2hmvlylfgvmaqdpq76zm85"))
       (patches (search-patches
                 "psm-arch.patch"     ; uname -p returns "unknown" on Debian 9
                 "psm-ldflags.patch"  ; build shared lib with LDFLAGS
                 "psm-repro.patch"    ; reproducibility
                 "psm-disable-memory-stats.patch"))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (inputs `(("libuuid" ,util-linux "lib")))
    (arguments
     '(#:make-flags `("PSM_USE_SYS_UUID=1" "CC=gcc" "WERROR="
                      ,(string-append "INSTALL_PREFIX=" %output)
                      ,(string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-/usr/include
                    (lambda _
                      (substitute* "Makefile"
                        (("\\$\\{DESTDIR}/usr/include")
                         (string-append %output "/include")))
                      (substitute* "Makefile"
                        (("/lib64") "/lib"))
                      #t))
                  (add-after 'unpack 'patch-sysmacros
                    (lambda _
                      (substitute* "ipath/ipath_proto.c"
                        (("#include <sys/poll.h>" m)
                         (string-append m "\n"
                                        "#include <sys/sysmacros.h>")))
                      #t)))))
    (synopsis "Intel Performance Scaled Messaging (PSM) Libraries")
    (description
     "The PSM Messaging API, or PSM API, is Intel's low-level user-level
communications interface for the True Scale family of products.  PSM users are
enabled with mechanisms necessary to implement higher level communications
interfaces in parallel environments.")
    ;; Only Intel-compatable processors are supported.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license (list license:bsd-2 license:gpl2)))) ;dual

(define-public snapscreenshot
  (package
    (name "snapscreenshot")
    (version "1.0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://bisqwit.iki.fi/src/arch/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32 "0gzvqsbf6a2sbd1mqvj1lbm57i2bm5k0cr6ncr821d1f32gw03mk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "BINDIR=" out "/bin")
               (string-append "MANDIR=" out "/share/man")))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; ./configure is a snarky no-op
         (add-before 'install 'fix-ownership
           ;; Install binaries owned by ‘root’ instead of the nonexistent ‘bin’.
           (lambda _
             (substitute* "depfun.mak"
               ((" -o bin -g bin ") " "))
             #t))
         (add-before 'install 'create-output-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share/man/man1"))
               #t))))))
    (home-page "https://bisqwit.iki.fi/source/snapscreenshot.html")
    (synopsis "Take screenshots of one or more Linux text consoles")
    (description
     "snapscreenshot saves a screenshot of one or more Linux text consoles as a
Targa (@dfn{.tga}) image.  It can be used by anyone with read access to the
relevant @file{/dev/vcs*} file(s).")
    (license license:gpl2)))

(define-public fbcat
  (package
    (name "fbcat")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/jwilk/fbcat/releases/download/"
                           version "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0pj9hxmwhbz6kmd7847yx2jh1scl9l25zgndyi8s9vlzdkq2q8d7"))))
    (build-system gnu-build-system)
    (inputs
     ;; The ‘fbgrab’ wrapper can use one of several PPM-to-PNG converters.  We
     ;; choose netpbm simply because it's the smallest.  It still adds ~94 MiB
     ;; to an otherwise tiny package, so we put ‘fbgrab’ in its own output.
     `(("pnmtopng" ,netpbm)))
    (outputs (list "out" "fbgrab"))
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'build 'qualify-references
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((pnmtopng (assoc-ref inputs "pnmtopng"))
                    (out (assoc-ref outputs "out")))
               (substitute* "fbgrab"
                 (("fbcat" all)
                  (string-append out "/bin/" all))
                 (("pnmtopng" all)
                  (string-append pnmtopng "/bin/" all)))
               #t)))
         (add-after 'install 'split-fbgrab-output
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out:fbgrab (assoc-ref outputs "fbgrab")))
               (for-each (lambda (file)
                           (let ((old (string-append out "/" file))
                                 (new (string-append out:fbgrab "/" file)))
                             (mkdir-p (dirname new))
                             (rename-file old new)))
                         (list "bin/fbgrab"
                               "share/man/man1/fbgrab.1"))
               #t))))))
    (home-page "https://jwilk.net/software/fbcat")
    (synopsis "Take a screenshot of the contents of the Linux framebuffer")
    (description
     "fbcat saves the contents of the Linux framebuffer (@file{/dev/fb*}), or
a dump therof.  It supports a wide range of drivers and pixel formats.
@command{fbcat} can take screenshots of virtually any application that can be
made to write its output to the framebuffer, including (but not limited to)
text-mode or graphical applications that don't use a display server.

Also included is @command{fbgrab}, a wrapper around @command{fbcat} that
emulates the behaviour of Gunnar Monell's older fbgrab utility.")
    (license license:gpl2)))

(define-public libcgroup
  (package
    (name "libcgroup")
    (version "0.41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/libcg/" name "/"
             version "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "0lgvyq37gq84sk30sg18admxaj0j0p5dq3bl6g74a1ppgvf8pqz4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("linux-pam" ,linux-pam)))
    (home-page "https://sourceforge.net/projects/libcg/")
    (synopsis "Control groups management tools")
    (description "Control groups is Linux kernel method for process resource
restriction, permission handling and more.  This package provides userspace
interface to this kernel feature.")
    (license license:lgpl2.1)))

(define-public mbpfan
  (package
    (name "mbpfan")
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgraziotin/mbpfan")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gc9ypxi55vxs77nx8ihhh9zk7fr9v0m0zfm76q7x0bi6jz11mbr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; tests ask to be run as root
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "DESTDIR=" out)
                            ,(string-append "CC=" (cc-for-target))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda _
             (substitute* "Makefile"
               (("/usr") ""))
             #t))
         (delete 'configure))))         ; there's no configure phase
    (home-page "https://github.com/dgraziotin/mbpfan")
    (synopsis "Control fan speed on Macbooks")
    (description
     "mbpfan is a fan control daemon for Apple Macbooks.  It uses input from
the @code{coretemp} module and sets the fan speed using the @code{applesmc}
module.  It can be executed as a daemon or in the foreground with root
privileges.")
    (license license:gpl3+)))

(define-public psm2
  (package
    (name "psm2")
    (version "11.2.185")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/intel/opa-psm2")
                    (commit (string-append "PSM2_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "062hg4r6gz7pla9df70nqs5i2a3mp1wszmp4l0g771fykhhrxsjg"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       `(,(string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-Makefiles
                    (lambda _
                      (substitute* "Makefile"
                        (("/lib64") "/lib")
                        (("/usr") ""))
                      (substitute* "compat/Makefile"
                        (("/lib64") "/lib")
                        (("/usr") ""))
                      #t))
                  (replace 'install
                    (lambda _
                      (setenv "DESTDIR" %output)
                      (invoke "make" "install")
                      #t)))))
    (inputs
     `(("rdma-core" ,rdma-core)
       ("numactl" ,numactl)))
    (synopsis "Intel Performance Scaled Messaging 2 (PSM2) library")
    (description
     "This package is low-level user-level Intel's communications interface.
The PSM2 API is a high-performance vendor-specific protocol that provides a
low-level communications interface for the Intel Omni-Path family of
high-speed networking devices.")
    (home-page "https://github.com/intel/opa-psm2")
    ;; Only the x86_64 architecure is supported.
    (supported-systems '("x86_64-linux"))
    (license (list license:bsd-3 license:gpl2)))) ; dual

(define-public libpfm4
  (package
    (name "libpfm4")
    (version "4.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/perfmon2/"
                                  name "/libpfm-" version ".tar.gz"))
              (sha256
               (base32
                "1qp4g4n6dw42p2w5rkwzdb7ynk8h7g5vg01ybpmvxncgwa7bw3yv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:modules ((guix build utils)
                  (guix build gnu-build-system))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'check)
                  (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (setenv "CC" "gcc")
                        (invoke "make")
                        #t)))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (invoke "make"
                                (string-append "PREFIX=" out)
                                "install")
                        #t))))))
    (synopsis "Performance event monitoring library")
    (description
     "This package provides a library called libpfm4, which is used to develop
monitoring tools exploiting the performance monitoring events such as those
provided by the Performance Monitoring Unit (PMU) of modern processors.

Libpfm4 helps convert from an event name, expressed as a string, to the event
encoding that is either the raw event as documented by the hardware vendor or
the OS-specific encoding.  In the latter case, the library is able to prepare
the OS-specific data structures needed by the kernel to setup the event.

libpfm4 provides support for the @code{perf_events} interface, which was
introduced in Linux 2.6.31.")
    (home-page "http://perfmon2.sourceforge.net/")
    (license license:expat)))

(define-public libnfnetlink
  (package
    (name "libnfnetlink")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.netfilter.org/projects/libnfnetlink/files/"
                    "libnfnetlink-" version ".tar.bz2"))
              (sha256
               (base32
                "06mm2x4b01k3m7wnrxblk9j0mybyr4pfz28ml7944xhjx6fy2w7j"))))
    (build-system gnu-build-system)
    (home-page "https://www.netfilter.org/projects/libnfnetlink/")
    (synopsis "Low-level netfilter netlink communication library")
    (description
     "@code{libnfnetlink} is the low-level library for netfilter related
kernel/userspace communication.  It provides a generic messaging
infrastructure for in-kernel netfilter subsystems (such as nfnetlink_log,
nfnetlink_queue, nfnetlink_conntrack) and their respective users and/or
management tools in userspace.")
    (license license:gpl2)))

(define-public go-netlink
  (package
    (name "go-netlink")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vishvananda/netlink")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hpzghf1a4cwawzhkiwdzin80h6hd09fskl77d5ppgc084yvj8x0"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/vishvananda/netlink"))
    (native-inputs
     `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-netns" ,go-netns)))
    (home-page "https://github.com/vishvananda/netlink")
    (synopsis "Simple netlink library for Go")
    (description "The netlink package provides a simple netlink library for
Go.  Netlink is the interface a user-space program in Linux uses to
communicate with the kernel.  It can be used to add and remove interfaces, set
IP addresses and routes, and configure IPsec.")
    (license license:asl2.0)))

(define-public libinih
  (package
    (name "libinih")
    (version "53")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/benhoyt/inih")
                    (commit (string-append "r" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dqf5j2sw4hq68rqvxbrsf44ygfzx9ypiyzipk4cvp9aimbvsbc6"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags '("-Ddistro_install=true" "-Ddefault_library=shared")))
    (home-page "https://github.com/benhoyt/inih")
    (synopsis "Simple .INI parser library for C")
    (description "The inih (INI Not Invented Here) library is a simple .INI file
parser written in C.  It's only a couple of pages of code, and it was designed to
be small and simple, so it's good for embedded systems.  It's also more or less
compatible with Python's ConfigParser style of .INI files, including RFC
822-style multi-line syntax and name: value entries.")
    (license license:bsd-3)))

(define-public xfsprogs
  (package
    (name "xfsprogs")
    (version "5.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/utils/fs/xfs/xfsprogs/"
                    "xfsprogs-" version ".tar.gz"))
              (sha256
               (base32
                "1byj53qdwsii35d0f11nz0dl618mpvwy5aa44pc9zg281g2r27ab"))))
    (build-system gnu-build-system)
    (outputs (list "out" "python"))
    (arguments
     `(#:tests? #f   ; kernel/user integration tests are in package "xfstests"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'separate-python-output
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out    (assoc-ref outputs "out"))
                   (python (assoc-ref outputs "python")))
               (for-each
                (lambda (script)
                  (mkdir-p (string-append python (dirname script)))
                  (rename-file (string-append out script)
                               (string-append python script)))
                (list "/sbin/xfs_scrub_all"))
               #t)))
         (add-after 'install 'install-headers
           (lambda _
             (invoke "make" "install-dev"))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("libinih" ,libinih)
       ("libuuid" ,util-linux "lib")
       ("python" ,python-wrapper)))
    (home-page "https://xfs.wiki.kernel.org/")
    (synopsis "XFS file system tools")
    (description "This package provides commands to create and check XFS
file systems.")
    ;; The library "libhandle" and the headers in "xfslibs-dev" are
    ;; licensed under lgpl2.1. the other stuff is licensed under gpl2.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public genext2fs
  (package
    (name "genext2fs")
    (version "1.4.1-4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jeremie-koenig/genext2fs")
                    ;; 1.4.1-3 had a VCS tag but 1.4.1-4 doesn't.
                    (commit "9ee43894634998b0b2b309d636f25c64314c9421")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0ib5icn78ciz00zhc1bgdlrwaxvsdz7wnplwblng0jirwi9ml7sq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'apply-debian-patches
           ;; Debian changes (the revision after ‘-’ in VERSION) are
           ;; maintained as separate patches.  Apply those relevant to us.
           (lambda _
             (for-each
              (lambda (file-name)
                (invoke "patch" "-p1" "-i"
                        (string-append "debian/patches/" file-name)))
              (list "blocksize+creator.diff" ; add -B/-o options
                    "byteswap_fix.diff"))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (home-page "https://github.com/jeremie-koenig/genext2fs")
    (synopsis "Generate ext2 file system as a normal user")
    (description "This package provides a program to generate an ext2
file system as a normal (non-root) user.  It does not require you to mount
the image file to copy files on it, nor does it require that you become
the superuser to make device nodes.")
    (license license:gpl2)))

(define-public fakeroot
  (package
    (name "fakeroot")
    (version "1.25.3")
    (source (origin
              ;; There are no tags in the repository, so take this snapshot.
              (method url-fetch)
              (uri (string-append "https://deb.debian.org/debian/pool/main/f/"
                                  "fakeroot/fakeroot_" version ".orig.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v4m3v1bdqvblwj3vqsb3mllgbci6dsgsydq6765nzvz6n1kd44f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             ;; The "preroll" script takes care of Autoconf and also
             ;; prepares the translated manuals.
             (invoke "sh" "./preroll")))
        (add-after 'configure 'patch-Makefile
          (lambda _
            ;; Note: The root of the problem is already in "Makefile.am".
            (substitute* "Makefile"
             (("/bin/sh") (which "sh")))
            #t))
        (add-after 'unpack 'patch-script
          (lambda*  (#:key inputs #:allow-other-keys)
            (substitute* "scripts/fakeroot.in"
             (("getopt")
              (string-append (assoc-ref inputs "util-linux")
                             "/bin/getopt"))
             (("sed")
              (string-append (assoc-ref inputs "sed")
                             "/bin/sed"))
             (("cut")
              (string-append (assoc-ref inputs "coreutils")
                             "/bin/cut")) )
            #t))
        (add-before 'configure 'setenv
          (lambda _
            (setenv "LIBS" "-lacl")
            #t))
        (add-before 'check 'prepare-check
          (lambda _
            (setenv "SHELL" (which "bash"))
            (setenv "VERBOSE" "1")
            (substitute* "test/t.touchinstall"
             ;; We don't have the name of the root user, so use ID=0.
             (("grep root") "grep \"\\<0\\>\""))
            (substitute* "test/tartest"
             ;; We don't have the name of the root group, so use ID=0.
             (("ROOTGROUP=root") "ROOTGROUP=0")
             ;; We don't have the name of the daemon user, so use IDs.
             (("daemon:sys") "1:3")
             (("daemon:") "1:"))
            ;; We don't have an /etc/passwd entry for "root" - use numeric IDs.
            (substitute* "test/compare-tar"
             (("tar -tvf") "tar --numeric-owner -tvf"))
            #t)))))
    (native-inputs
     `(;; For bootstrapping the package.
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("gettext" ,gettext-minimal)
       ("po4a" ,po4a)

       ;; For tests.
       ("sharutils" ,sharutils)
       ("xz" ,xz)))
    (inputs
     `(("acl" ,acl)
       ("libcap" ,libcap)
       ("util-linux" ,util-linux)
       ("sed" ,sed)
       ("coreutils" ,coreutils)))
    (synopsis "Provides a fake root environment")
    (description "@command{fakeroot} runs a command in an environment where
it appears to have root privileges for file manipulation. This is useful
for allowing users to create archives (tar, ar, .deb etc.) with files in
them with root permissions/ownership. Without fakeroot one would have to
have root privileges to create the constituent files of the archives with
the correct permissions and ownership, and then pack them up, or one would
have to construct the archives directly, without using the archiver.")
    (home-page "http://freshmeat.sourceforge.net/projects/fakeroot")
    (license license:gpl3+)))

(define-public fakechroot
  (package
    (name "fakechroot")
    (version "2.20.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/dex4er/fakechroot/releases/download/"
                    version "/fakechroot-" version ".tar.gz"))
              (sha256
               (base32
                "1aijkd0b45wav25v01qhw8zxwa3pl0nnp9fabmmy1nlx7hr09gas"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: The tests heavily assume they run on an FHS system so for now
     ;; skip them.
     '(#:tests? #f
       #:configure-flags '("--disable-static")))
    (synopsis "Emulate @code{chroot} by overriding file system calls")
    (description
     "@command{fakechroot} runs a command in an environment were is additional
possibility to use @code{chroot} command without root privileges.  This is
useful for allowing users to create own chrooted environment with possibility
to install another packages without need for root privileges.

It works by providing @file{libfakechroot.so}, a shared library meant to be
set as @code{LD_PRELOAD} to override the C library file system functions.")
    (home-page "https://github.com/dex4er/fakechroot/")
    (license license:lgpl2.1+)))

(define-public inputattach
  (package
    (name "inputattach")
    (version "0.42.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linuxwacom/input-wacom")
                    (commit (string-append "input-wacom-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "04lnn7v0rm4ppbya140im5d4igcl6c1nrqpgbsr0i8wkral0nv7j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inputattach"
               (invoke (string-append (assoc-ref inputs "gcc")
                                      "/bin/gcc")
                       "-O2" "-o" "inputattach" "inputattach.c"))
             #t))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target-dir (string-append
                                (assoc-ref outputs "out")
                                "/bin/")))
               (mkdir-p target-dir)
               (copy-file "inputattach/inputattach"
                          (string-append target-dir
                                         "inputattach"))
               #t))))))
    (home-page "https://linuxwacom.github.io/")
    (synopsis "Dispatch input peripherals events to a device file")
    (description "inputattach dispatches input events from several device
types and interfaces and translates so that the X server can use them.")
    (license license:gpl2+)))

(define-public pipewire
  (package
    (name "pipewire")
    (version "0.2.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PipeWire/pipewire")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q5wrqnhhs6r49p8yvkw1pl0cnsd4rndxy4h5lvdydwgf1civcwc"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags '("-Dsystemd=false")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("eudev" ,eudev)
       ("ffmpeg" ,ffmpeg)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libva" ,libva)
       ("sbc" ,sbc)
       ("sdl2" ,sdl2)))
    (home-page "https://pipewire.org/")
    (synopsis "Server and user space API to deal with multimedia pipelines")
    (description
     "PipeWire is a project that aims to greatly improve handling of audio and
video under Linux.  It aims to support the usecases currently handled by both
PulseAudio and Jack and at the same time provide same level of powerful handling
of Video input and output.  It also introduces a security model that makes
interacting with audio and video devices from containerized applications easy,
with supporting Flatpak applications being the primary goal.  Alongside Wayland
and Flatpak we expect PipeWire to provide a core building block for the future
of Linux application development.")
    (license license:lgpl2.0+)))

(define-public pipewire-0.3
  (package
    (inherit pipewire)
    (name "pipewire")
    (version "0.3.22")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PipeWire/pipewire")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ywna5f5v8s79ivrqfwwc8vy6sn3a2zvfwqyalf1fypj5d90w8g9"))))
    (arguments
     '(#:configure-flags
       (list (string-append "-Dudevrulesdir=" (assoc-ref %outputs "out")
                            "/lib/udev/rules.d")
             "-Dsystemd=false")
       #:phases
       (modify-phases %standard-phases
         ;; Skip shrink-runpath, otherwise validate-runpath fails.
         (delete 'shrink-runpath))))
    (inputs
     (append (package-inputs pipewire)
             `(("bluez" ,bluez)
               ("jack" ,jack-2)
               ("pulseaudio" ,pulseaudio)
               ("vulkan-loader" ,vulkan-loader)
               ("vulkan-headers" ,vulkan-headers))))))

(define-public ell
  (package
    (name "ell")
    (version "0.23")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.kernel.org/pub/scm/libs/ell/ell.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qhlcwhn0gj877yss2ymx1aczghlddzb5v9mm1dgp2zliii3jy10"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dbus-tests
           (lambda _
             (substitute* '("unit/test-dbus-message-fds.c"
                            "unit/test-dbus-properties.c"
                            "unit/test-dbus.c")
               (("/usr/bin/dbus-daemon") (which "dbus-daemon")))
             #t)))))
    (inputs
     `(("dbus" ,dbus)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("libtool" ,libtool)
       ("pkgconfig" ,pkg-config)
       ("automake" ,automake)))
    (home-page "https://01.org/ell")
    (synopsis "Embedded Linux Library")
    (description "The Embedded Linux* Library (ELL) provides core, low-level
functionality for system daemons.  It typically has no dependencies other than
the Linux kernel, C standard library, and libdl (for dynamic linking).  While
ELL is designed to be efficient and compact enough for use on embedded Linux
platforms, it is not limited to resource-constrained systems.")
    (license license:lgpl2.1+)))

(define-public lttng-ust
  (package
    (name "lttng-ust")
    (version "2.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://lttng.org/files/lttng-ust/"
                                  "lttng-ust-" version ".tar.bz2"))
              (sha256
               (base32
                "1n646yz7882svf5a4ay3vqiiz3qjn3pgkkij9kk22871wp7q0ck8"))))
    (build-system gnu-build-system)
    (inputs
     `(("liburcu" ,liburcu)
       ("numactl" ,numactl)))
    (native-inputs
     `(("python" ,python-3)))
    (home-page "https://lttng.org/")
    (synopsis "LTTng userspace tracer libraries")
    (description "The user space tracing library, liblttng-ust, is the LTTng
user space tracer.  It receives commands from a session daemon, for example to
enable and disable specific instrumentation points, and writes event records
to ring buffers shared with a consumer daemon.")
    (license license:lgpl2.1+)))

(define-public kexec-tools
  (package
    (name "kexec-tools")
    (version "2.0.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/utils/kernel"
                                  "/kexec/kexec-tools-" version ".tar.xz"))
              (sha256
               (base32
                "1hj9mj6x3qs9c8x8mrri9xd3wsg3gwzbab3jfa5ry146xm4pzjcn"))))
    (build-system gnu-build-system)
    (arguments
     ;; There are no automated tests.
     '(#:tests? #f))
    (home-page "https://projects.horms.net/projects/kexec/")
    (synopsis "Tools for booting directly into different kernels")
    (description "This package provides the @code{kexec} program and ancillary
utilities.  Using @code{kexec}, it is possible to boot directly into a new
kernel from the context of an already-running kernel, bypassing the normal
system boot process.")
    (license license:gpl2)))

(define-public cachefilesd
  (package
    (name "cachefilesd")
    (version "0.10.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://git.kernel.org/pub/scm/linux/kernel/git/dhowells"
                    "/cachefilesd.git/snapshot/cachefilesd-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0g40ljjnn3wzh9gp6il21c95f977298qrrkrxfnwfl3k3asfmnbi"))))
    (build-system gnu-build-system)
    (outputs '("out"))
    (arguments
     `(#:tests? #f ; there are no tests
       #:make-flags
       (let ((prefix-dir (lambda (var dir)
                           (string-append var "=" %output "/" dir))))
         (list (string-append "CC=" ,(cc-for-target))
               (prefix-dir "SBINDIR" "sbin/")
               (prefix-dir "ETCDIR" "etc/")
               (prefix-dir "MANDIR" "share/man/")))
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (home-page "https://people.redhat.com/~dhowells/cachefs/")
    (synopsis "CacheFiles userspace management daemon")
    (description "@code{cachefilesd} is a userspace daemon that manages the
cache data store that is used by network file systems such as @code{AFS} and
@code{NFS} to cache data locally on disk.  The content of the cache is
persistent over reboots.")
    (license license:gpl2+)))

(define-public libbpf
  (package
    (name "libbpf")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libbpf/libbpf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ilnnm4q22f8fagwp8kb37licy4ks861i2iqh2djsypqhnxvx3fv"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; In Requires.private of libbpf.pc.
     `(("libelf" ,libelf)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list
        (string-append "PREFIX=" (assoc-ref %outputs "out"))
        (string-append "LIBDIR=$(PREFIX)/lib")
        (string-append
         "CC=" (assoc-ref %build-inputs "gcc") "/bin/gcc"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'pre-build
           (lambda _
             (chdir "src")
             #t)))))
    (home-page "https://github.com/libbpf/libbpf")
    (synopsis "BPF CO-RE (Compile Once – Run Everywhere)")
    (description
     "Libbpf supports building BPF CO-RE-enabled applications, which, in
contrast to BCC, do not require the Clang/LLVM runtime or linux kernel
headers.")
    (license `(,license:lgpl2.1 ,license:bsd-2))))

(define-public bcc
  (package
    (name "bcc")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iovisor/bcc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1367c0bzrpclvjvmk0sxgi49rh7j2f9izqk5a7g3yvawh1fmvvjh"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("clang-toolchain" ,clang-toolchain)
       ("libbpf" ,(package-source libbpf))
       ;; LibElf required but libelf does not contain
       ;; archives, only object files.
       ;; https://github.com/iovisor/bcc/issues/504
       ("elfutils" ,elfutils)
       ("linux-libre-headers" ,linux-libre-headers)
       ("luajit" ,luajit)
       ("python-wrapper" ,python-wrapper)))
    (arguments
     `(;; Tests all require root permissions and a "standard" file hierarchy.
       #:tests? #f
       #:configure-flags
       (let ((revision ,version))
         `(,(string-append "-DREVISION=" revision)))
       #:phases
       (modify-phases %standard-phases
         ;; FIXME: Use "-DCMAKE_USE_LIBBPF_PACKAGE=ON".
         (add-after 'unpack 'copy-libbpf
           (lambda* (#:key inputs #:allow-other-keys)
             (delete-file-recursively "src/cc/libbpf")
             (copy-recursively
              (assoc-ref inputs "libbpf") "src/cc/libbpf")))
         (add-after 'copy-libbpf 'substitute-libbc
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/python/bcc/libbcc.py"
               (("(libbcc\\.so.*)\\b" _ libbcc)
                (string-append
                 (assoc-ref outputs "out") "/lib/" libbcc)))))
         (add-after 'install 'wrap-tools
           (lambda* (#:key outputs #:allow-other-keys)
             (use-modules (ice-9 textual-ports))
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (tools (string-append out "/share/bcc/tools"))
                    (python-executable?
                     (lambda (filename _)
                       (call-with-input-file filename
                         (lambda (port)
                           (string-contains (get-line port)
                                            "/bin/python"))))))
               (for-each
                (lambda (python-executable)
                  (format #t "Wrapping: ~A.~%" python-executable)
                  (wrap-program python-executable
                    `("PYTHONPATH" ":" prefix
                      (,(string-append lib
                                       "/python"
                                       ,(version-major+minor
                                         (package-version python))
                                       "/site-packages")))))
                (find-files tools python-executable?))
               #t))))))
    (home-page "https://github.com/iovisor/bcc")
    (synopsis "Tools for BPF on Linux")
    (description
     "BCC is a toolkit for creating efficient kernel tracing and manipulation
programs, and includes several useful tools and examples.  It makes use of
extended BPF (Berkeley Packet Filters), formally known as eBPF, a new feature
that was first added to Linux 3.15.  Much of what BCC uses requires Linux 4.1
and above.")
    (license license:asl2.0)))

(define-public bpftrace
  (package
    (name "bpftrace")
    (version "0.11.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iovisor/bpftrace")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y4qgm2cpccrsm20rnh92hqplddqsc5q5zhw9nqn2igm3h9i0z7h"))
       (patches (search-patches "bpftrace-disable-bfd-disasm.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("bcc" ,bcc)
       ("clang-toolchain" ,clang-toolchain)
       ("elfutils" ,elfutils)
       ("libbpf" ,libbpf)
       ("linux-libre-headers" ,linux-libre-headers)))
    (arguments
     `(#:tests? #f ;Tests require googletest sources.
       #:configure-flags
       '("-DBUILD_TESTING=OFF"
         ;; FIXME: libbfd misses some link dependencies, when fixed, remove
         ;; the associated patch.
         "-DHAVE_BFD_DISASM=OFF")))
    (home-page "https://github.com/iovisor/bpftrace")
    (synopsis "High-level tracing language for Linux eBPF")
    (description
     "bpftrace is a high-level tracing language for Linux enhanced Berkeley
Packet Filter (eBPF) available in recent Linux kernels (4.x).  bpftrace uses
LLVM as a backend to compile scripts to BPF-bytecode and makes use of BCC for
interacting with the Linux BPF system, as well as existing Linux tracing
capabilities: kernel dynamic tracing (kprobes), user-level dynamic
tracing (uprobes), and tracepoints.  The bpftrace language is inspired by awk
and C, and predecessor tracers such as DTrace and SystemTap.  bpftrace was
created by Alastair Robertson.")
    (license license:asl2.0)))

(define-public ttyebus-linux-module
  (let ((revision "0")
        (commit "fe4332a2281cf79804ef4d8516aa848ca1c58d1f"))
    (package
      (name "ttyebus-linux-module")
      (version (git-version "1.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eBUS/ttyebus")
               (commit "fe4332a2281cf79804ef4d8516aa848ca1c58d1f")))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1i66xjs9vln5cca6wx7aiiw7jihrlsk9hjdqyczp36fvm1b1bani"))))
      (supported-systems '("armhf-linux" "aarch64-linux"))
      (build-system linux-module-build-system)
      (arguments
       `(#:tests? #f))
      (home-page "https://github.com/eBUS/ttyebus")
      (synopsis "Low-latency Raspberry Pi UART driver")
      (description "This package provides a Linux kernel module that will
provide a serial device @code{/dev/ttyebus} with almost no latency upon
receiving.  It is dedicated to the PL011 UART of the Raspberry Pi.")
      (license license:gpl3+))))

(define-public ipset
  (package
    (name "ipset")
    (version "7.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://ipset.netfilter.org/"
                           "ipset-" version ".tar.bz2"))
       (sha256
        (base32 "0zdzp9fhpp6hmirzxy7w27fb9xx9lxd2ykxbn8by7ngi62nvll9i"))))
    (build-system gnu-build-system)
    (inputs
     `(("libmnl" ,libmnl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags '("--with-kmod=no")))
    (home-page "https://ipset.netfilter.org/")
    (synopsis "Administration tool for IP sets")
    (description "IP sets are a framework inside the Linux 2.4.x and 2.6.x kernel which
can be administered by the ipset utility.  Depending on the type,
currently an IP set may store IP addresses, (TCP/UDP) port numbers or
IP addresses with MAC addresses in a way which ensures lightning speed
when matching an entry against a set.

If you want to
@itemize @bullet
@item store multiple IP addresses or port numbers and match against the entire
collection using a single iptables rule.
@item dynamically update iptables rules against IP addresses or ports without
performance penalty.
@item express complex IP address and ports based rulesets with a single
iptables rule and benefit from the speed of IP sets.
@end itemize\n
then IP sets may be the proper tool for you.")
    (license license:gpl2+)))

(define-public liburing
  (package
    (name "liburing")
    (version "0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.kernel.dk/liburing")
                    (commit (string-append "liburing-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15z44l7y4c6s6dlf7v8lq4znlsjbja2r4ifbni0l8cdcnq0w3zh3"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Tests are dependent on kernel version and features
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; configure fails if it is followed by SHELL, CONFIG_SHELL,
           ;; --enable-fast-install, and --build
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))
               (invoke "./configure" (string-append "--prefix=" out))))))))
    (home-page "https://github.com/axboe/liburing")
    (synopsis "Interface to the Linux kernel @code{io_uring} interface")
    (description "This is the io_uring library, liburing. liburing provides
helpers to setup and teardown io_uring instances, and also a simplified
interface for applications that don't need (or want) to deal with the full
kernel side implementation.")
    (license license:expat)))

(define-public erofs-utils
  (package
    (name "erofs-utils")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/linux/kernel/git/xiang/erofs-utils.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vb4mxsb59g29x7l22cffsqa8x743sra4j5zbmx89hjwpwm9vvcg"))))
    (build-system gnu-build-system)
    (inputs
     `(("lz4" ,lz4)
       ("libuuid" ,util-linux "lib")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://git.kernel.org/pub/scm/linux/kernel/git/xiang/erofs-utils.git/")
    (synopsis "User-space tools for EROFS file system")
    (description "EROFS (Enhanced Read-Only File System) is a compressed,
read-only file system optimized for resource-scarce devices.  This package
provides user-space tools for creating EROFS file systems.")
    (license license:gpl2+)))

(define-public rasdaemon
  (package
    (name "rasdaemon")
    (version "0.6.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mchehab/rasdaemon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13g39x19lfjf9izdcb0nlyfjrgpliivhv4nw3ndgyzi59l3yqc0v"))))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool" ,libtool)))
    (inputs `(("sqlite" ,sqlite)))
    (arguments
     `(#:configure-flags '("--enable-all"
                           "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'munge-autotools
           (lambda _
             ;; For some reason upstream forces sysconfdir=/etc.  This results
             ;; in EPERM during the install phase.  Removing the offending
             ;; line lets sysconfdir correctly pick up DESTDIR.
             (substitute* "configure.ac"
               (("^test .* sysconfdir=/etc\n$") ""))
             ;; Upstream tries to create /var/lib/rasdaemon at install time.
             ;; This results in EPERM on guix.  Instead, the service should
             ;; create this at activation time.
             (substitute* "Makefile.am"
               (("^\\s*\\$\\(install_sh\\) -d .*@RASSTATEDIR@.*$") "")))))))
    (build-system gnu-build-system)
    (home-page "https://github.com/mchehab/rasdaemon")
    (synopsis "Platform Reliability, Availability and Serviceability tools")
    (description "The @code{rasdaemon} program is a daemon which monitors the
platform Reliablity, Availability and Serviceability (RAS) reports from the
Linux kernel trace events.  These trace events are logged in
/sys/kernel/debug/tracing, reporting them via syslog/journald.")
    (license license:gpl2)))
