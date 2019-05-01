;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages antivirus)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public clamav
  (package
    (name "clamav")
    (version "0.101.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.clamav.net/downloads/production/"
                                  "clamav-" version ".tar.gz"))
              (sha256
               (base32
                "0d3n4y8i5q594h4cjglmvpk4jd73r9ajpp1bvq5lr9zpdzgyn4ha"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                            '("win32"                  ; unnecessary
                              "libclamav/c++/llvm"     ; use system llvm
                              "libclamav/tomsfastmath" ; use system tomsfastmath
                              "libclamunrar"))))       ; non-free license
              (patches
               (search-patches "clamav-system-tomsfastmath.patch"
                               "clamav-config-llvm-libs.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("check" ,check)                 ; for tests
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bzip2" ,bzip2)
       ("libcurl" ,curl)
       ("libjson" ,json-c)
       ("libltdl" ,libltdl)
       ("libmspack" ,libmspack)
       ("llvm" ,llvm-3.6)               ; requires <3.7, for JIT/verifier
       ("ncurses" ,ncurses)
       ("openssl" ,libressl)
       ("pcre2" ,pcre2)
       ("sasl" ,cyrus-sasl)             ; for linking curl with libtool
       ("tomsfastmath" ,tomsfastmath)
       ("xml" ,libxml2)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags
       (let-syntax ((with (syntax-rules ()
                            ((_ name)
                             (string-append "--with-" name "="
                                            (assoc-ref %build-inputs name))))))
         (list "--disable-unrar"
               "--enable-llvm"
               "--with-system-llvm"
               "--with-system-libmspack"
               "--without-included-ltdl"
               (with "xml")
               (with "openssl")
               (with "libjson")
               (with "pcre2")
               (with "zlib")
               (with "libcurl")
               ;; For sanity, specifying --enable-* flags turns
               ;; "support unavailable" warnings into errors.
               "--enable-bzip2"
               "--enable-check"
               "--sysconfdir=/etc/clamav"
               ;; Default database directory needs to be writeable
               "--with-dbdir=/var/db/clamav"))
       ;; install sample .conf files to %output/etc rather than /etc/clamav
       #:make-flags (list (string-append "sysconfdir=" %output "/etc"))
       #:phases (modify-phases %standard-phases
                  ;; Regenerate configure script.  Without this we don't get
                  ;; the correct value for LLVM linker variables.
                  (add-after 'unpack 'reconf
                    (lambda _ (invoke "autoreconf" "-vfi")))
                  (add-before 'configure 'patch-llvm-config
                    (lambda _
                      (substitute* '("libclamav/c++/detect.cpp"
                                     "libclamav/c++/ClamBCRTChecks.cpp"
                                     "libclamav/c++/bytecode2llvm.cpp")
                        (("llvm/Config/config.h") "llvm/Config/llvm-config.h"))
                      ;; `llvm-config --libfiles` inappropriately lists lib*.a
                      ;; libraries, rather than the lib*.so's that our llvm
                      ;; contains.  They're used only for listing extra build
                      ;; dependencies, so ignore them until that's fixed.
                      (substitute* "libclamav/c++/Makefile.in"
                        (("@LLVMCONFIG_LIBFILES@") ""))
                      #t))
                  (add-before 'check 'skip-clamd-tests
                    ;; XXX: The check?_clamd tests fail inside the build
                    ;; chroot, but pass outside.
                    (lambda _
                      (substitute* "unit_tests/Makefile"
                        (("check2_clamd.sh.*check4_clamd.sh") ""))
                      #t)))))
    (home-page "https://www.clamav.net")
    (synopsis "Antivirus engine")
    (description
     "Clam AntiVirus is an anti-virus toolkit, designed especially for e-mail
scanning on mail gateways.  It provides a number of utilities including a
flexible and scalable multi-threaded daemon, a command line scanner, and
advanced tool for automatic database updates.  The core of the package is an
anti-virus engine available in the form of a shared library.")
    (license (list license:gpl2+        ;ClamAV itself
                   license:lgpl2.1      ;libclamav/mspack.[ch]
                   license:public-domain ;libclamav/7z/*, libclamav/rijndael.[ch], etc...
                   (package-license bzip2) ;modified bzip2 source in libclamav/nsis
                   license:bsd-2        ;several files in libclamav
                   license:bsd-3        ;libclamav/{regex,qsort.c,swf.[ch]
                   license:ncsa         ;libclamav/c++/PointerTracking.cpp
                   license:zlib         ;libclamav/inf*.h
                   license:x11          ;libclamav/lzw
                   (license:non-copyleft "libclamav/strlcat.c") ;"OpenBSD" license
                   license:asl2.0       ;libclamav/yara*
                   license:expat))))    ;shared/getopt.[ch]
