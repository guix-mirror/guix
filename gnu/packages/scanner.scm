;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2019, 2020, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages scanner)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public sane-airscan
  (package
    (name "sane-airscan")
    (version "0.99.27")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alexpevzner/sane-airscan")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1syxsih1kdnz9slsg5a92bqnllagm4cybqk4n2y6mbkqn6h0zlnv"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "prefix=" #$output)
                   (string-append "libdir=$(prefix)/lib"))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure ))))  ; no configure script
    (native-inputs
     (list pkg-config))
    (inputs
     (list avahi
           gnutls
           libjpeg-turbo
           libpng
           libxml2
           sane-backends))
    (home-page "https://github.com/alexpevzner/sane-airscan")
    (synopsis "SANE backend for eSCL (AirScan) and WSD document scanners")
    (description ; no @acronym{eSCL} because the meaning isn't officially known
     "This SANE backend lets you scan documents and images from scanners and
multi-function printers that speak eSCL (marketed as ``AirScan'') or
@acronym{WSD, Web Services for Devices} (or ``WS-Scan'').

Both are vendor-neutral protocols that allow ``driverless'' scanning over IPv4
and IPv6 networks without the vendor-specific drivers that make up most of the
sane-backends collection.  This is similar to how most contemporary printers
speak the universal @acronym{IPP, Internet Printing Protocol}.

Only scanners that support eSCL will also work over USB.  This requires a
suitable IPP-over-USB daemon like ipp-usb to be installed and configured.

Any eSCL or WSD-capable scanner should just work.  sane-airscan automatically
discovers and configures devices, including which protocol to use.  It was
successfully tested with many devices from Brother, Canon, Dell, Kyocera,
Lexmark, Epson, HP, OKI, Panasonic, Pantum, Ricoh, Samsung, and Xerox, with both
WSD and eSCL.")
    (license (list license:gpl2+        ; the combined work
                   license:expat))))    ; http_parser.[ch]

(define-public sane-backends-minimal
  (package
    (name "sane-backends-minimal")
    (version "1.0.32")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://gitlab.com/sane-project/backends")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32 "13jlqdp7n7z2n78v6idl3ri5idk7ddk9j8wrmh73lba8l9y8xnsi"))
             (modules '((guix build utils)))
             (snippet
              ;; Generated HTML files and udev rules normally embed a
              ;; timestamp.  Work around that to build things reproducibly.
              '(begin
                 (substitute* "tools/sane-desc.c"
                   (("asctime \\(localtime \\(&current_time\\)\\)")
                    "\"1970-01-01\""))))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("autoconf-archive" ,autoconf-archive)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ;; For scripts/pixma_gen_options.py.
       ("python" ,python-wrapper)))
    (inputs
     (list libusb))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'zap-unnecessary-git-dependency
           (lambda _
             ;; This runs before default patch-shebangs phase.
             (substitute* "tools/git-version-gen"
               (("/bin/sh") (which "sh")))
             (with-output-to-file ".tarball-version"
               (lambda _ (format #t ,version)))))
         (add-before 'configure 'disable-backends
           (lambda _
             (setenv "BACKENDS" " ")

             ;; Disable tests that may require back ends to be built.
             (substitute* "testsuite/Makefile.in"
               ((" backend ") " "))))
         (add-before 'configure 'disable-failing-tests
           (lambda _
             ;; Disable unmaintained tests that that fail with errors resembling:
             ;;
             ;; < # by sane-desc 3.5 from sane-backends 1.0.24git on Jul 31 2013
             ;; ---
             ;; > # by sane-desc 3.5 from sane-backends 1.0.27 on 1970-01-01#
             ;; FAIL: sane-desc -m usermap -s ./data
             (for-each
              (lambda (pattern)
                (substitute* "testsuite/tools/Makefile.in"
                  (((string-append " " pattern " ")) " ")))
              (list "usermap" "db" "udev" "udev\\+acl" "udev\\+hwdb" "hwdb"))

             ;; Disable tests that try to connect to actual USB hardware & fail
             ;; with the following error when no USB access is allowed at all:
             ;;
             ;; sanei_usb_test: sanei_usb_test.c:849: main: Assertion
             ;; `test_init (1)' failed.
             (substitute* "testsuite/sanei/Makefile.in"
               (("sanei_usb_test\\$\\(EXEEXT\\) ") ""))))
         (add-before 'build 'build-pixma_sane_options.c
           ;; "No rule to make target '../backend/pixma/pixma_sane_options.c',
           ;; needed by 'sane-backends.pot-update'."
           (lambda _
             (invoke "make" "-C" "backend" "pixma/pixma_sane_options.c")))
         (add-after 'install 'install-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib/udev/rules.d"))
               (copy-file "tools/udev/libsane.rules"
                          (string-append out
                                         "/lib/udev/rules.d/"
                                         "60-libsane.rules")))))
         (add-after 'install 'make-reproducible
           ;; XXX Work around an old bug <https://issues.guix.gnu.org/26247>.
           ;; Then work around "Throw to key `decoding-error' ..." by using sed.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (locale (string-append out "/share/locale")))
               (with-directory-excursion locale
                 (for-each (lambda (file)
                             (invoke "sed" "-i" "/^PO-Revision-Date:/d" file))
                           (list "en@boldquot/LC_MESSAGES/sane-backends.mo"
                                 "en@quot/LC_MESSAGES/sane-backends.mo")))))))))
    (home-page "http://www.sane-project.org")
    (synopsis
     "Raster image scanner library and drivers, without scanner support")
    (description "SANE stands for \"Scanner Access Now Easy\" and is an API
proving access to any raster image scanner hardware (flatbed scanner,
hand-held scanner, video- and still-cameras, frame-grabbers, etc.).  The
package contains the library, but no drivers.")
    (license license:gpl2+))) ; plus linking exception

;; This variant links in the hpaio backend provided by hplip, which adds
;; support for HP scanners whose backends are not maintained by the SANE
;; project, and builds all of those backends.
(define-public sane-backends
  (package/inherit sane-backends-minimal
    (name "sane-backends")
    (inputs
     `(("hplip" ,(@ (gnu packages cups) hplip-minimal))
       ("libjpeg" ,libjpeg-turbo)       ; for pixma/epsonds/other back ends
       ("libpng" ,libpng)               ; support ‘scanimage --format=png’
       ("libxml2" ,libxml2)             ; for pixma back end
       ,@(package-inputs sane-backends-minimal)))
    (arguments
     (substitute-keyword-arguments (package-arguments sane-backends-minimal)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'disable-backends)
           (add-after 'disable-failing-tests 'disable-failing-backend-tests
             (lambda _
               ;; Disable test that fails on i686:
               ;;   <https://bugs.gnu.org/39449>
               (substitute* "testsuite/backend/genesys/Makefile.in"
                 ((" genesys_unit_tests\\$\\(EXEEXT\\)") ""))
               #t))
           (add-after 'unpack 'add-backends
             (lambda _
               (substitute* "backend/dll.conf.in"
                 (("hp5590" all) (format #f "~a~%~a" all "hpaio")))
               #t))
           (add-after 'install 'install-hpaio
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (define hplip (string-append (assoc-ref inputs "hplip")
                                            "/lib/sane"))
               (define out (string-append (assoc-ref outputs "out")
                                          "/lib/sane"))
               (for-each
                (lambda (file)
                  (symlink file (string-append out "/" (basename file))))
                (find-files hplip))
               #t))))))
    (synopsis
     "Raster image scanner library and drivers, with scanner support")
    (description "SANE stands for \"Scanner Access Now Easy\" and is an API
proving access to any raster image scanner hardware (flatbed scanner,
hand-held scanner, video- and still-cameras, frame-grabbers, etc.).  The
package contains the library and drivers.")))

(define-public scanbd
  (package
    (name "scanbd")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/scanbd/releases/"
                           "scanbd-" version ".tgz"))
       (sha256
        (base32 "0pvy4qirfjdfm8aj6x5rkbgl7hk3jfa2s21qkk8ic5dqfjjab75n"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--disable-debug"
                   "--sysconfdir=/etc"
                   "CFLAGS=-Wno-error") ; warnings should never be fatal
           #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda* (#:key make-flags #:allow-other-keys)
                   (let ((conf (string-append #$output "/etc/scanbd")))
                     (apply invoke "make" "install"
                            ;; Install example configuration to the store, not
                            ;; /etc.  These don't inherit from each other, so
                            ;; we need both.
                            (string-append "scanbdconfdir="  conf)
                            (string-append "scannerconfdir=" conf "/scanner.d")
                            make-flags))))
               (add-after 'install 'install-extra-documentation
                 ;; The README provides more detailed set-up instructions than
                 ;; the man page.
                 (lambda _
                   (let ((doc (string-append #$output "/share/doc/"
                                             #$name "-" #$version)))
                     (install-file "doc/README.txt" doc)))))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("dbus" ,dbus)
       ("libconfuse" ,libconfuse)
       ("sane-backends" ,sane-backends)
       ("udev" ,eudev)
       ("zlib" ,zlib)))
    (home-page "https://scanbd.sourceforge.io")
    (synopsis "Configurable scanner button monitor")
    (description "Scanbd stands for scanner button daemon.  It regularly polls
scanners for pressed buttons, function knob changes, or other events such
as (un)plugging the scanner or inserting and removing paper.  Then it performs
the desired action(s) such as saving, copying, or e-mailing the image.

Actions can be fully customized through scripts, based on any combination of
switch or knob settings.  Events are also signaled over D-Bus and scans can
even be triggered over D-Bus from foreign applications.

Scanbd talks to scanners through the @acronym{SANE, Scanner Access Now Easy}
back-end library.  This means that it supports almost all existing scanners,
provided the driver also exposes the buttons.")
    (license license:gpl2+)))

(define-public xsane
  (package
    (name "xsane")
    (version "0.999")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/sane-project/frontend/xsane.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08zvxj7i1s88ckrsqldmsrikc3g62a6p3s3i5b5x4krcfpi3vs50"))
       ;; Apply some important-looking fixes.  There are many more unreleased
       ;; commits upstream.  A 1.0 release is planned.
       (patches (search-patches "xsane-fix-memory-leak.patch"
                                "xsane-fix-pdf-floats.patch"
                                "xsane-fix-snprintf-buffer-length.patch"
                                "xsane-support-ipv6.patch"
                                "xsane-tighten-default-umask.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove ancient bundled lprng code under a non-free licence.  See
           ;; <https://trisquel.info/en/issues/10713>, which solves the problem
           ;; by replacing it with a newer (free) copy.  We let the build fall
           ;; back to the system version instead, which appears to work fine.
           (delete-file "lib/snprintf.c")
           (substitute* "lib/Makefile.in"
             (("snprintf\\.o ") ""))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "xsanedocdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))
       #:tests? #f                                ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-invalid-dereference
           ;; Fix the following compilation error with libpng:
           ;;   xsane-save.c: In function ‘xsane_save_png’:
           ;;   xsane-save.c:4913:21: error: dereferencing pointer to
           ;;   incomplete type ‘png_struct {aka struct png_struct_def}’
           ;;       if (setjmp(png_ptr->jmpbuf))
           ;;                         ^
           (lambda _
             (substitute* "src/xsane-save.c"
               (("png_ptr->jmpbuf") "png_jmpbuf(png_ptr)"))
             #t))
         (add-after 'unpack 'use-sane-help-browser
           (lambda _
             (substitute* "src/xsane.h"
               (("netscape") (which "xdg-open")))
             #t))
         (add-after 'install 'delete-empty-/sbin
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (rmdir (string-append out "/sbin"))
               #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("lcms" ,lcms)
       ("libjpeg" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("sane-backends" ,sane-backends)

       ;; To open the manual from the Help menu.
       ("xdg-utils" ,xdg-utils)))
    (home-page "https://gitlab.com/sane-project/frontend/xsane")
    (synopsis "Featureful graphical interface for document and image scanners")
    (description
     "XSane is a graphical interface for controlling a scanner and acquiring
images from it.  You can photocopy multi-page documents and save, fax, print,
or e-mail your scanned images.  It is highly configurable and exposes all
device settings, letting you fine-tune the final result.  It can also be used
as a GIMP plugin to acquire images directly from a scanner.

XSane talks to scanners through the @acronym{SANE, Scanner Access Now Easy}
back-end library, which supports almost all existing scanners.")
    (license license:gpl2+)))
