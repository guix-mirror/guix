;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Christopher Howard <christopher@librehacker.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (gnu packages libusb)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xiph))

(define-public libusb
  (package
    (name "libusb")
    (version "1.0.24")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/libusb/libusb/"
                          "releases/download/v" version
                          "/libusb-" version ".tar.bz2"))
      (sha256
       (base32 "0amilbi5qncdnrds3ji21vbiz1wvdm1fwp5qrxnk49xkyy2jdzby"))))
    (build-system gnu-build-system)

    ;; XXX: Enabling udev is now recommended, but eudev indirectly depends on
    ;; libusb.
    (arguments `(#:configure-flags '("--disable-udev")))
    ;; (inputs `(("eudev" ,eudev)))

    (home-page "https://libusb.info")
    (synopsis "User-space USB library")
    (description
     "Libusb is a library that gives applications easy access to USB
devices on various operating systems.")
    (license license:lgpl2.1+)))

(define-public libusb-compat
  (package
    (name "libusb-compat")
    (version "0.1.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libusb/"
                          "libusb-compat-" (version-major+minor version) "/"
                          "libusb-compat-" version "/"
                          "libusb-compat-" version ".tar.bz2"))
      (sha256
       (base32
        "0nn5icrfm9lkhzw1xjvaks9bq3w6mjg86ggv3fn7kgi4nfvg8kj0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libusb))
    (home-page "https://libusb.info")
    (synopsis "Compatibility shim for libusb")
    (description
     "Libusb-compat provides a shim allowing applications based on older
version of libusb to run with newer libusb.")
    (license license:lgpl2.1+)))

;; required by 0xffff, which compiles with libusb-compat, but executes only
;; with libusb-0.1
(define-public libusb-0.1
  (package (inherit libusb)
    (version "0.1.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libusb/libusb-0.1 (LEGACY)/"
                          version "/libusb-" version ".tar.gz"))
      (sha256
       (base32
        "0i4bacxkyr7xyqxbmb00ypkrv4swkgm0mghbzjsnw6blvvczgxip"))
      (patches (search-patches "libusb-0.1-disable-tests.patch"))))
    (arguments `(#:configure-flags (list "CFLAGS=-Wno-error")))))

(define-public libusb4java
  ;; There is no public release so we take the latest version from git.
  (let ((commit "396d642a57678a0d9663b062c980fe100cc0ea1e")
        (revision "1"))
    (package
      (name "libusb4java")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/usb4java/libusb4java")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0wqgapalhfh9v38ycbl6i2f5lh1wpr6fzwn5dwd0rdacypkd1gml"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ; there are no tests
         #:phases
         (modify-phases %standard-phases
           ;; FIXME: libusb 1.0.22 deprecated libusb_set_debug, so the build
           ;; fails because libusb4java uses a deprecated procedure.
           (add-after 'unpack 'disable-Werror
             (lambda _
               (substitute* "CMakeLists.txt"
                 (("-Werror") ""))
               #t))
           (add-before 'configure 'set-JAVA_HOME
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
               #t)))))
      (inputs
       (list libusb))
      (native-inputs
       `(("jdk" ,icedtea "jdk")))
      (home-page "https://github.com/usb4java/libusb4java/")
      (synopsis "JNI bindings to libusb")
      (description
       "This package provides Java JNI bindings to the libusb library for use
with usb4java.")
      (license license:expat))))

(define-public java-usb4java
  (package
    (name "java-usb4java")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/usb4java/usb4java")
                     (commit (string-append "usb4java-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0aip6k24czz5g58qwb963mpick0b6ks774drfpdd8gcdvj9iv87j"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "usb4java.jar"
       #:phases
       (modify-phases %standard-phases
         ;; Usually, native libusb4java libraries for all supported systems
         ;; would be included in the jar and extracted at runtime.  Since we
         ;; build everything from source we cannot just bundle pre-built
         ;; binaries for other systems.  Instead, we patch the loader to
         ;; directly return the appropriate library for this system.  The
         ;; downside is that the jar will only work on the same architecture
         ;; that it was built on.
         (add-after 'unpack 'copy-libusb4java
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/main/java/org/usb4java/Loader.java"
               (("private static String extractLibrary" line)
                (string-append
                 line "(final String a, final String b) {"
                 "return \""
                 (assoc-ref inputs "libusb4java") "/lib/libusb4java.so"
                 "\"; }\n"
                 "private static String _extractLibrary")))
             #t))
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             (with-directory-excursion "src/test/java/org/usb4java"
               ;; These tests should only be run when USB devices are present.
               (substitute* '("LibUsbGlobalTest.java"
                              "TransferTest.java")
                 (("this.context = new Context\\(\\);")
                  "this.context = null;")
                 (("LibUsb.init") "//"))
               (substitute* "DeviceListIteratorTest.java"
                 (("this.iterator.remove" line)
                  (string-append "assumeUsbTestsEnabled();" line))))
             #t)))))
    (inputs
     (list libusb4java java-commons-lang3 java-junit java-hamcrest-core))
    (home-page "http://usb4java.org/")
    (synopsis "USB library for Java")
    (description
     "This package provides a USB library for Java based on libusb and
implementing @code{javax.usb} (JSR-80).")
    (license license:expat)))

(define-public python-libusb1
  (package
    (name "python-libusb1")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "libusb1" version))
       (sha256
        (base32
         "0ikc7z7mdyr8pm0mk3ibab1hqrq4cpi9frmc3p94hsmbyzn85fnk"))))
    (build-system python-build-system)
    (arguments
     '(#:modules ((srfi srfi-1)
                  (guix build utils)
                  (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install-license-files 'remove-incorrect-license
           (lambda* (#:key out #:allow-other-keys)
             ;; Was relicensed to LGPL 2.1+, but old COPYING file still left
             ;; in source. Remove it so it does not get installed.
             (delete-file "COPYING")))
         (add-after 'unpack 'fix-libusb-reference
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "usb1/_libusb1.py"
               (("libusb_path = ctypes\\.util\\.find_library\\(base_name\\)")
                (string-append
                 "libusb_path = \""
                 (find (negate symbolic-link?)
                       (find-files (assoc-ref inputs "libusb")
                                   "^libusb.*\\.so\\..*"))
                 "\""))))))))
    (propagated-inputs (list libusb))
    (home-page "https://github.com/vpelletier/python-libusb1")
    (synopsis "Pure-python wrapper for libusb-1.0")
    (description "Libusb is a library that gives applications easy access to
USB devices on various operating systems.  This package provides a Python
wrapper for accessing libusb-1.0.")
    (license license:lgpl2.1+)))

(define-public python-pyusb
  (package
    (name "python-pyusb")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyusb" version))
       (sha256
        (base32
         "0qkk2jn270jwwl1x26hmdhb14m9kkbrzzwzizdjcl1a29b6756sf"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:modules ((srfi srfi-1)
                  (srfi srfi-26)
                  (guix build utils)
                  (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-libusb-reference
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "usb/libloader.py"
               (("lib = locate_library\\(candidates, find_library\\)")
                (string-append
                 "lib = \""
                 (find (negate symbolic-link?)
                       (find-files (assoc-ref inputs "libusb")
                                   "^libusb-.*\\.so\\..*"))
                 "\"")))
             #t)))))
    (inputs
     (list libusb))
    (home-page "https://pyusb.github.io/pyusb/")
    (synopsis "Python bindings to the libusb library")
    (description
     "PyUSB aims to be an easy to use Python module to access USB devices.")
    (license license:bsd-3)))

(define-public python2-pyusb
  (package-with-python2 python-pyusb))

(define-public python-capablerobot-usbhub
  (package
    (name "python-capablerobot-usbhub")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "capablerobot_usbhub" version))
       (sha256
        (base32
         "1priic4iq2vn1rc711kzxwhxrwa508rkxrr193qdz2lw26kdhvix"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib/udev/rules.d"))
               (copy-file "50-capablerobot-usbhub.rules"
                          (string-append out
                                         "/lib/udev/rules.d/"
                                         "50-capablerobot-usbhub.rules"))
               #t))))))
    (propagated-inputs
     (list python-click python-construct python-pyusb python-pyyaml))
    (home-page
     "https://github.com/CapableRobot/CapableRobot_USBHub_Driver")
    (synopsis
     "Host side driver for the Capable Robot Programmable USB Hub")
    (description
     "This package provides access to the internal state of the Capable Robot
USB Hub, allowing you to monitor and control the Hub from an upstream
computer.  It also creates a transparent CircuitPython Bridge, allowing
unmodified CircuitPython code to run on the host computer and interact with
I2C and SPI devices attached to the USB Hub.")
    (license license:expat)))

(define-public libplist
  (package
    (name "libplist")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libimobiledevice"
                           "/libplist/releases/download/" version
                           "/libplist-" version ".tar.bz2"))
       (sha256
        (base32 "16mxdwaw01x9a3adf0yj3bqjc7afpf2vm1n5hkgj3i3y6zjifmaa"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Tests fail randomly when run in parallel because several of them write
       ;; and read to/from the same file--e.g., "4.plist" is accessed by
       ;; 'large.test' and 'largecmp.test'.
       #:parallel-tests? #f))
    (inputs
     (list python))
    (native-inputs
     (list autoconf automake libtool pkg-config python-cython)) ; to build Python bindings
    (home-page "https://libimobiledevice.org/")
    (synopsis "C library to handle Apple Property List files")
    (description "This package provides a small portable C library to handle
Apple Property List files in binary or XML.")
    (license license:lgpl2.1+)))

(define-public libusbmuxd
  (package
    (name "libusbmuxd")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libimobiledevice"
                                  "/libusbmuxd/releases/download/" version
                                  "/libusbmuxd-" version ".tar.bz2"))
              (sha256
               (base32
                "084vg570g1qb506jd7axg6c080mfsmbf52v3lngzlknsaf2q0snc"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config libplist))
    (home-page "https://libimobiledevice.org/")
    (synopsis "Library to multiplex connections from and to iOS devices")
    (description "This package provides a client library to multiplex
connections from and to iOS devices by connecting to a socket provided by a
@code{usbmuxd} daemon.")
    (license license:lgpl2.1+)))

(define-public libimobiledevice
  (package
    (name "libimobiledevice")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libimobiledevice"
                                  "/libimobiledevice/releases/download/"
                                  version "/libimobiledevice-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "1xmhfnypg6j7shl73wfkrrn4mj9dh8qzaj3258q9zkb5cc669wjk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("PYTHON_VERSION=3")))
    (propagated-inputs
     (list openssl libplist libusbmuxd))
    (inputs
     (list python))
    (native-inputs
     (list pkg-config python-cython))
    (home-page "https://libimobiledevice.org/")
    (synopsis "Protocol library and tools to communicate with Apple devices")
    (description "libimobiledevice is a software library that talks the
protocols to support Apple devices.  It allows other software to easily access
the device's file system, retrieve information about the device and its
internals, backup/restore the device, manage installed applications, retrieve
address books, calendars, notes, and bookmarks, and (using libgpod) synchronize
music and video to the device.")
    (license license:lgpl2.1+)))

(define-public ifuse
  (package
    (name "ifuse")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libimobiledevice"
                                  "/ifuse/releases/download/" version
                                  "/ifuse-" version ".tar.bz2"))
              (sha256
               (base32
                "11wdv44qwia77sh38n36809ysaib52rwd4fvqwb5ybsbz4p70l1m"))))
    (inputs
     (list fuse libimobiledevice))
    (native-inputs
     (list pkg-config))
    (build-system gnu-build-system)
    (home-page "https://libimobiledevice.org/")
    (synopsis "Mount iOS devices")
    (description "This package provides @command{ifuse}, a command to mount
iOS devices and access their contents.")
    (license license:lgpl2.1+)))

(define-public usbmuxd
  (package
    (name "usbmuxd")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libimobiledevice"
                                  "/usbmuxd/releases/download/" version
                                  "/usbmuxd-" version ".tar.bz2"))
              (sha256
               (base32
                "17idzpxrvkbff0jpynf35df95lh7wsm8vndynp63bxib2w09gv60"))))
    (inputs
     (list libplist libusb libimobiledevice))
    (native-inputs
     (list pkg-config))
    (build-system gnu-build-system)
    (home-page "https://libimobiledevice.org/")
    (synopsis "Multiplex connections over USB to an iOS device")
    (description "This package provides the @code{usbmuxd} daemon
which multiplexes connections over USB to an iOS device.  To
users, it means you can sync your music, contacts, photos, etc.
over USB.")
    (license license:gpl2+)))

(define-public libmtp
  (package
    (name "libmtp")
    (version "1.1.19")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/libmtp/libmtp/" version
                                 "/libmtp-" version ".tar.gz"))
             (sha256
              (base32
               "0a1jlf5b2c2vylkvdd3cxjs20a96jlbrdyx7rwai5rzmcdpszd6y"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libgcrypt))
    (propagated-inputs
     ;; libmtp.pc refers to all these.
     (list libusb))
    (arguments
     `(#:configure-flags
       (list "--disable-static"
             (string-append "--with-udev="
                            (assoc-ref %outputs "out")
                            "/lib/udev"))))
    (home-page "http://libmtp.sourceforge.net/")
    (synopsis "Library implementing the Media Transfer Protocol")
    (description "Libmtp implements an MTP (Media Transfer Protocol)
initiator, which means that it initiates MTP sessions with devices.  The
devices responding are known as MTP responders.  Libmtp runs on devices
with a USB host controller interface.  It implements MTP Basic, which was
proposed for standardization.")
    ;; COPYING contains lgpl2.1, while files headers give
    ;; "GNU Lesser General Public License as published by the Free Software
    ;; Foundation; either version 2 of the License, or (at your option) any
    ;; later version."
    (license license:lgpl2.1+)))

(define-public gmtp
  (package
    (name "gmtp")
    (version "1.3.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gmtp/gMTP-" version
                                  "/gmtp-" version ".tar.gz"))
              (sha256
               (base32
                "04q6byyq002fhzkc2rkkahwh5b6272xakaj4m3vwm8la8jf0r0ss"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags
       (let ((libid3tag (assoc-ref %build-inputs "libid3tag")))
         (list
          "CFLAGS=-fcommon"
          ;; libid3tag provides no .pc file, so pkg-config fails to find them.
          (string-append "ID3TAG_CFLAGS=-I" libid3tag "/include")
          (string-append "ID3TAG_LIBS=-L" libid3tag "/lib -lid3tag -lz")))))
    (inputs
     (list gtk+ flac libvorbis libid3tag libmtp))
    (native-inputs
     (list pkg-config))
    (home-page "http://gmtp.sourceforge.net/")
    (synopsis "Simple graphical MTP client")
    (description "gMTP is a simple graphical client for the Media Transfer Protocol
  (MTP), which allows media files to be transferred to and from many portable
devices.")
    (license license:bsd-3)))

(define-public hidapi
  (package
    (name "hidapi")
    (version "0.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libusb/hidapi")
             (commit (string-append "hidapi-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nr4z4b10vpbh3ss525r7spz4i43zim2ba5qzfl15dgdxshxxivb"))))
    (build-system gnu-build-system)
    (inputs
     (list libusb eudev))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://github.com/libusb/hidapi")
    (synopsis "HID API library")
    (description
     "HIDAPI is a library which allows an application to interface with USB and Bluetooth
HID-Class devices.")
    ;; HIDAPI can be used under one of three licenses.
    (license (list license:gpl3
                   license:bsd-3
                   (license:non-copyleft "file://LICENSE-orig.txt")))))

(define-public python-hidapi
  (package
    (name "python-hidapi")
    (version "0.7.99.post21")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hidapi" version))
       (sha256
        (base32
         "15ws59zdrxahf3k7z5rcrwc4jgv1307anif8ixm2cyb9ask1mgp0"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libraries.
        '(begin
           (delete-file-recursively "hidapi")
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-configuration
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               (("'/usr/include/libusb-1.0'")
                (string-append "'" (assoc-ref inputs "libusb")
                               "/include/libusb-1.0'"))
               (("'/usr/include/hidapi'")
                (string-append "'" (assoc-ref inputs "hidapi")
                               "/include/hidapi'")))
             #t))
         ;; XXX Necessary because python-build-system drops the arguments.
         (replace 'build
           (lambda _
             (invoke "python" "setup.py" "build" "--with-system-hidapi")))
         (replace 'check
           (lambda _
             (invoke "python" "setup.py" "test" "--with-system-hidapi")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "python" "setup.py" "install" "--with-system-hidapi"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--single-version-externally-managed" "--root=/"))))))
    (inputs
     (list hidapi libusb eudev))
    (native-inputs
     (list python-cython))
    (home-page "https://github.com/trezor/cython-hidapi")
    (synopsis "Cython interface to hidapi")
    (description "This package provides a Cython interface to @code{hidapi}.")
    ;; The library can be used under either of these licenses.
    (license (list license:gpl3
                   license:bsd-3
                   (license:non-copyleft
                    "https://github.com/trezor/cython-hidapi/blob/master/LICENSE-orig.txt"
                    "You are free to use cython-hidapi code for any purpose.")))))

(define-public python2-hidapi
  (package-with-python2 python-hidapi))
