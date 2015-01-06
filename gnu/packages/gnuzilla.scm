;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
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

(define-module (gnu packages gnuzilla)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages yasm)
  #:use-module (gnu packages zip))

(define-public mozjs
  (package
    (name "mozjs")
    (version "17.0.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/js/"
                   name version ".tar.gz"))
             (sha256
              (base32
               "1fig2wf4f10v43mqx67y68z6h77sy900d1w0pz9qarrqx57rc7ij"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("perl", perl)
        ("python" ,python-2)))
    (arguments
      `(#:phases
          (alist-cons-before
           'configure 'chdir
           (lambda _
             (chdir "js/src"))
           (alist-replace
            'configure
            ;; configure fails if it is followed by SHELL and CONFIG_SHELL
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (setenv "SHELL" (which "sh"))
                (setenv "CONFIG_SHELL" (which "sh"))
                (zero? (system*
                        "./configure" (string-append "--prefix=" out)))))
            %standard-phases))))
    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey")
    (synopsis "Mozilla javascript engine")
    (description "SpiderMonkey is Mozilla's JavaScript engine written
in C/C++.")
    (license license:mpl2.0))) ; and others for some files

(define-public nspr
  (package
    (name "nspr")
    (version "4.10.7")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "0f1ri51yzjikigf6z31g03cdv6sgi9gw2c3vvv39psk3m37zb6iq"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("perl", perl)))
    (arguments
      `(#:tests? #f ; no check target
        #:configure-flags
        `("--enable-64bit")
        #:phases
          (alist-cons-before
           'configure 'chdir
           (lambda _
             (chdir "nspr"))
            %standard-phases)))
    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSPR")
    (synopsis "Netscape API for system level and libc-like functions")
    (description "Netscape Portable Runtime (NSPR) provides a
platform-neutral API for system level and libc-like functions.  It is used
in the Mozilla clients.")
    (license license:mpl2.0)))

(define-public nss
  (package
    (name "nss")
    (version "3.17.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.mozilla.org/pub/mozilla.org/security/nss/"
                    "releases/NSS_3_17_3_RTM/src/nss-3.17.3.tar.gz"))
              (sha256
               (base32
                "1m91z80x4zh1mxgf53bl33lp43gn1wxxx0y26mgz511gb81ykmgl"))
              ;; Create nss.pc and nss-config.
              (patches (list (search-patch "nss-pkgconfig.patch")))))
    (build-system gnu-build-system)
    (outputs '("out" "bin"))
    (arguments
     '(#:parallel-build? #f ; failed
       #:make-flags
       (let* ((out (assoc-ref %outputs "out"))
              (nspr (string-append (assoc-ref %build-inputs "nspr")))
              (rpath (string-append "-Wl,-rpath=" out "/lib/nss")))
         (list "-C" "nss" (string-append "PREFIX=" out)
               "NSDISTMODE=copy"
               "NSS_USE_SYSTEM_SQLITE=1"
               (string-append "NSPR_INCLUDE_DIR=" nspr "/include/nspr")
               ;; Add $out/lib/nss to RPATH.
               (string-append "RPATH=" rpath)
               (string-append "LDFLAGS=" rpath)))
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-26))
       #:imported-modules ((guix build gnu-build-system)
                           (guix build utils))
       #:phases
       (alist-replace
        'configure
        (lambda* (#:key system inputs #:allow-other-keys)
          ;; Tells NSS to build for the 64-bit ABI if we are 64-bit system.
          (when (string-prefix? "x86_64" system)
            (setenv "USE_64" "1"))
          #t)
        (alist-replace
         'check
         (lambda _
           ;; Use 127.0.0.1 instead of $HOST.$DOMSUF as HOSTADDR for testing.
           ;; The later requires a working DNS or /etc/hosts.
           (setenv "DOMSUF" "(none)")
           (setenv "USE_IP" "TRUE")
           (setenv "IP_ADDRESS" "127.0.0.1")
           (zero? (system* "./nss/tests/all.sh")))
         (alist-replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin (string-append (assoc-ref outputs "bin") "/bin"))
                   (inc (string-append out "/include/nss"))
                   (lib (string-append out "/lib/nss"))
                   (obj (match (scandir "dist" (cut string-suffix? "OBJ" <>))
                          ((obj) (string-append "dist/" obj)))))
              ;; Install nss-config to $out/bin.
              (mkdir-p (string-append out "/bin"))
              (copy-file (string-append obj "/bin/nss-config")
                         (string-append out "/bin/nss-config"))
              (delete-file (string-append obj "/bin/nss-config"))
              ;; Install nss.pc to $out/lib/pkgconfig.
              (mkdir-p (string-append out "/lib/pkgconfig"))
              (copy-file (string-append obj "/lib/pkgconfig/nss.pc")
                         (string-append out "/lib/pkgconfig/nss.pc"))
              (delete-file (string-append obj "/lib/pkgconfig/nss.pc"))
              (rmdir (string-append obj "/lib/pkgconfig"))
              ;; Install other files.
              (copy-recursively "dist/public/nss" inc)
              (copy-recursively (string-append obj "/bin") bin)
              (copy-recursively (string-append obj "/lib") lib)))
          %standard-phases)))))
    (inputs
     `(("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (propagated-inputs `(("nspr" ,nspr))) ; required by nss.pc.
    (native-inputs `(("perl" ,perl)))
    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSS")
    (synopsis "Network Security Services")
    (description
     "Network Security Services (NSS) is a set of libraries designed to support
cross-platform development of security-enabled client and server applications.
Applications built with NSS can support SSL v2 and v3, TLS, PKCS #5, PKCS #7,
PKCS #11, PKCS #12, S/MIME, X.509 v3 certificates, and other security
standards.")
    (license license:mpl2.0)))

(define-public icecat
  (package
    (name "icecat")
    (version "31.2.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnuzilla/"
                          version "/" name "-" version ".tar.xz"))
      (sha256
       (base32
        "02r9klfc0z26w270inq652249hq0wfzvwhzvwmk0n8v8nzkk5idh"))
      (patches (map search-patch
                    '("icecat-CVE-2014-1587-bug-1042567.patch"
                      "icecat-CVE-2014-1587-bug-1072847.patch"
                      "icecat-CVE-2014-1587-bug-1079729.patch"
                      "icecat-CVE-2014-1587-bug-1080312.patch"
                      "icecat-CVE-2014-1587-bug-1089207.patch"
                      "icecat-CVE-2014-1590.patch"
                      "icecat-CVE-2014-1592.patch"
                      "icecat-CVE-2014-1593.patch"
                      "icecat-CVE-2014-1594.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bzip2" ,bzip2)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer-0.10)
       ("gst-plugins-base" ,gst-plugins-base-0.10)
       ("gtk+" ,gtk+-2)
       ("pango" ,pango)
       ("freetype" ,freetype)
       ("libxft" ,libxft)
       ("libevent" ,libevent)
       ("libxt" ,libxt)
       ("libffi" ,libffi)
       ("pulseaudio" ,pulseaudio)
       ("mesa" ,mesa)
       ("unzip" ,unzip)
       ("yasm" ,yasm)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2) ; Python 3 not supported
       ("python2-pysqlite" ,python2-pysqlite)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:tests? #f          ; no check target
       #:out-of-source? #t  ; must be built outside of the source directory

       #:configure-flags '(;; Building with debugging symbols takes ~5GiB, so
                           ;; disable it.
                           "--disable-debug"
                           "--disable-debug-symbols"

                           "--enable-pulseaudio"
                           "--disable-webrtc"     ; webrtc fails to build

                           "--with-system-zlib"
                           "--with-system-bz2"    ; FIXME: not used
                           "--with-system-libevent"

                           ;; Fails with "--with-system-png won't work because
                           ;; the system's libpng doesn't have APNG support".
                           ;; According to
                           ;; http://sourceforge.net/projects/libpng-apng/ ,
                           ;; "the Animated Portable Network Graphics (APNG)
                           ;; is an unofficial extension of the Portable
                           ;; Network Graphics (PNG) format";
                           ;; we probably do not wish to support it.
                           ;; "--with-system-png"

                           ;; Fails with "libjpeg-turbo JCS_EXTENSIONS
                           ;; required".
                           ;; According to
                           ;; http://sourceforge.net/projects/libjpeg-turbo/ ,
                           ;; "libjpeg-turbo is a derivative of libjpeg that
                           ;; uses MMX, SSE, SSE2, and NEON SIMD instructions
                           ;; to accelerate baseline JPEG compression/
                           ;; decompression", so we had better not use it
                           ;; "--with-system-jpeg"

                           "--enable-system-ffi")

       #:phases
       (alist-replace
        'configure
        ;; configure does not work followed by both "SHELL=..." and
        ;; "CONFIG_SHELL=..."; set environment variables instead
        (lambda* (#:key outputs configure-flags #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (bash (which "bash"))
                 (abs-srcdir (getcwd))
                 (srcdir (string-append "../" (basename abs-srcdir)))
                 (flags `(,(string-append "--prefix=" out)
                          ,(string-append "--with-l10n-base="
                                          abs-srcdir "/l10n")
                          ,@configure-flags)))
            (setenv "SHELL" bash)
            (setenv "CONFIG_SHELL" bash)
            (mkdir "../build")
            (chdir "../build")
            (format #t "build directory: ~s~%" (getcwd))
            (format #t "configure flags: ~s~%" flags)
            (zero? (apply system* bash
                          (string-append srcdir "/configure")
                          flags))))
        %standard-phases)))
    (home-page "http://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons.  It also
features built-in privacy-protecting features.")
    (license license:mpl2.0))) ; and others, see toolkit/content/license.html
