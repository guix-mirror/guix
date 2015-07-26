;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Andy Wingo <wingo@pobox.com>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
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

(define-module (gnu packages freedesktop)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages glib)                ;intltool
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages doxygen)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages polkit))

(define-public xdg-utils
  (package
    (name "xdg-utils")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
          (uri (string-append
                 "http://portland.freedesktop.org/download/xdg-utils-"
                 version ".tgz"))
          (sha256
            (base32
             "1b019d3r1379b60p33d6z44kx589xjgga62ijz9vha95dg8vgbi1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no check target
    (home-page "http://portland.freedesktop.org/")
    (synopsis "Freedesktop.org scripts for desktop integration")
    (description "The xdg-utils package is a set of simple scripts that
provide basic desktop integration functions in the framework of the
freedesktop.org project.")
    (license license:expat)))

(define-public libinput
  (package
    (name "libinput")
    (version "0.14.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://freedesktop.org/software/libinput/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0r0v5jqbnwgndq6ns3ss3kv1438ny302m7bg1najcl1dpqp21v9b"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libudev" ,eudev))) ; required by libinput.pc
    (inputs
     `(("libevdev" ,libevdev)
       ("mtdev" ,mtdev)))
    (home-page "http://www.freedesktop.org/wiki/Software/libinput/")
    (synopsis "Input devices handling library")
    (description
     "Libinput is a library to handle input devices for display servers and
other applications that need to directly deal with input devices.")
    (license license:x11)))

(define-public libxdg-basedir
  (package
    (name "libxdg-basedir")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/devnev/libxdg-basedir/archive/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s28c7sfwqimsmb3kn91mx7wi55fs3flhbmynl9k60rrllr00aqw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _
             ;; Run 'configure' in its own phase, not now.
             (substitute* "autogen.sh"
               (("^.*\\./configure.*") ""))
             (zero? (system* "sh" "autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://github.com/devnev/libxdg-basedir")
    (synopsis "Implementation of the XDG Base Directory specification")
    (description
     "libxdg-basedir is a C library providing some functions to use with
the freedesktop.org XDG Base Directory specification.")
    (license license:expat)))

(define-public elogind
  (let ((commit "14405a9"))
    (package
      (name "elogind")
      (version (string-append "219." commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://git.elephly.net/software/elogind.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1wz5lxj95qg64x2q5hf4zcb35hpxlw3wfswx6sb2srvsg50y3y72"))
                (file-name (string-append name "-checkout-" commit))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (use-modules (guix build utils))
                    (substitute* "Makefile.am"
                      ;; Avoid validation against DTD because the DTDs for
                      ;; both doctype 4.2 and 4.5 are needed.
                      (("XSLTPROC_FLAGS = ") "XSLTPROC_FLAGS = --novalid"))))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         (list
          ;; pam_elogind fails because of bus-error.c hackery
          "--disable-pam"
          (string-append "--with-rootprefix=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'autogen
                      (lambda _
                        (and (zero? (system* "intltoolize" "--force" "--automake"))
                             (zero? (system* "autoreconf" "-vif"))))))))
      (native-inputs
       `(("intltool" ,intltool)
         ("gettext" ,gnu-gettext)
         ("docbook-xsl" ,docbook-xsl)
         ("docbook-xml" ,docbook-xml)
         ("xsltproc" ,libxslt)
         ("libxml2" ,libxml2)                     ;for XML_CATALOG_FILES
         ("pkg-config", pkg-config)
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("gperf" ,gperf)))
      (inputs
       `(("linux-pam" ,linux-pam)
         ("linux-libre-headers" ,linux-libre-headers)
         ("libcap" ,libcap)
         ("dbus" ,dbus)
         ("eudev" ,eudev)))
      (home-page "https://github.com/andywingo/elogind")
      (synopsis "User, seat, and session management service")
      (description "Elogind is the systemd project's \"logind\" service,
extracted out as a separate project.  Elogind integrates with PAM to provide
the org.freedesktop.login1 interface over the system bus, allowing other parts
of a the system to know what users are logged in, and where.")
      (license license:lgpl2.1+))))

(define-public python-pyxdg
  (package
    (name "python-pyxdg")
    (version "0.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pyxdg/pyxdg-"
             version ".tar.gz"))
       (sha256
        (base32
         "179767h8m634ydlm4v8lnz01ba42gckfp684id764zaip7h87s41"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (alist-replace
        'check
        (lambda* (#:key inputs #:allow-other-keys)
          (setenv "XDG_DATA_DIRS"
                  (string-append (assoc-ref inputs "shared-mime-info")
                                 "/share/"))
          (substitute* "test/test-icon.py"
            (("/usr/share/icons/hicolor/index.theme")
             (string-append (assoc-ref inputs "hicolor-icon-theme")
                            "/share/icons/hicolor/index.theme")))

          ;; One test fails with:
          ;; AssertionError: 'x-apple-ios-png' != 'png'
          (substitute* "test/test-mime.py"
            (("self.check_mimetype\\(imgpng, 'image', 'png'\\)") "#"))
          (zero? (system* "nosetests" "-v")))
        %standard-phases)))
    (native-inputs
     `(("shared-mime-info" ,shared-mime-info) ;for tests
       ("hicolor-icon-theme" ,hicolor-icon-theme) ;for tests
       ("python-nose" ,python-nose)
       ("python-setuptools" ,python-setuptools)))
    (home-page "http://freedesktop.org/wiki/Software/pyxdg")
    (synopsis "Implementations of freedesktop.org standards in Python")
    (description
     "PyXDG is a collection of implementations of freedesktop.org standards in
Python.")
    (license license:lgpl2.0)))

(define-public python2-pyxdg
  (package-with-python2 python-pyxdg))

(define-public wayland
  (package
    (name "wayland")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://wayland.freedesktop.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1j3gfzn8i0xhk3j34mwb2srrscjxfyi279jhyq80mz943j6r6z7i"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)
       ("xmlto" ,xmlto)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("expat" ,expat)
       ("libffi" ,libffi)
       ("libxml2" ,libxml2))) ; for XML_CATALOG_FILES
    (home-page "http://wayland.freedesktop.org/")
    (synopsis "Display server protocol")
    (description
     "Wayland is a protocol for a compositor to talk to its clients as well as
a C library implementation of that protocol.  The compositor can be a standalone
display server running on Linux kernel modesetting and evdev input devices, an X
application, or a wayland client itself.  The clients can be traditional
applications, X servers (rootless or fullscreen) or other display servers.")
    (license license:x11)))

(define-public exempi
  (package
    (name "exempi")
    (version "2.2.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://libopenraw.freedesktop.org/download/"
                   name "-" version ".tar.bz2"))
             (sha256
              (base32
               "01vcd1mfn2s0iiq2cjyzgvnxx6kcq9cwra1iipijhs0vwvjx0yhf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--with-boost="
                               (assoc-ref %build-inputs "boost")))))
    (native-inputs
     `(("boost" ,boost))) ; tests
    (inputs
     `(("expat" ,expat)
       ("zlib" ,zlib)))
    (home-page "https://wiki.freedesktop.org/libopenraw/Exempi")
    (synopsis "XMP metadata handling library")
    (description "Exempi is an implementation of the Extensible Metadata
Platform (XMP), which enables embedding metadata in PDF and image formats.")
    (license license:bsd-3)))

(define-public libatasmart
  (package
    (name "libatasmart")
    (version "0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://0pointer.de/public/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "138gvgdwk6h4ljrjsr09pxk1nrki4b155hqdzyr8mlk3bwsfmw31"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("udev" ,eudev)))
    (home-page "http://0pointer.de/blog/projects/being-smart.html")
    (synopsis "ATA S.M.A.R.T. reading and parsing library")
    (description
     "This library supports a subset of the ATA S.M.A.R.T. (Self-Monitoring,
Analysis and Reporting Technology) functionality.")
    (license license:lgpl2.1+)))

(define-public udisks
  (package
    (name "udisks")
    (version "2.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://udisks.freedesktop.org/releases/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0spl155k0g2l2hvqf8xyjv08i68gfyhzpjva6cwlzxx0bz4gbify"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     `(("glib" ,glib))) ; required by udisks2.pc
    (inputs
     `(("acl" ,acl)
       ("libatasmart" ,libatasmart)
       ("libgudev" ,libgudev)
       ("polkit" ,polkit)))
    (arguments
     `(#:tests? #f ; requiring system message dbus
       #:configure-flags
       (list "--disable-man"
             "--localstatedir=/var"
             (string-append "--with-udevdir=" %output "/lib/udev"))
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'fix-girdir
          (lambda _
            ;; Install introspection data to its own output.
            (substitute* "udisks/Makefile.in"
              (("girdir = .*")
               "girdir = $(datadir)/gir-1.0\n")
              (("typelibsdir = .*")
               "typelibsdir = $(libdir)/girepository-1.0\n")))))))
    (home-page "http://www.freedesktop.org/wiki/Software/udisks/")
    (synopsis "Disk manager service")
    (description
     "UDisks provides interfaces to enumerate and perform operations on disks
and storage devices.  Any application (including unprivileged ones) can access
the udisksd(8) daemon via the name org.freedesktop.UDisks2 on the system
message bus.")
    ;; The dynamic library are under LGPLv2+, others are GPLv2+.
    (license (list license:gpl2+ license:lgpl2.0+))))
