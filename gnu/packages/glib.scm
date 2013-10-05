;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages glib)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages file)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages m4)

  ;; Export variables up-front to allow circular dependency with the 'xorg'
  ;; module.
  #:export (dbus
            glib
            gobject-introspection
            dbus-glib
            intltool
            itstool
            libsigc++
            glibmm))

(define dbus
  (package
    (name "dbus")
    (version "1.6.4")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "http://dbus.freedesktop.org/releases/dbus/dbus-"
                             version ".tar.gz"))
             (sha256
              (base32
               "1wacqyfkcpayg7f8rvx9awqg275n5pksxq5q7y21lxjx85x6pfjz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list ;; Install the system bus socket under /var.
                               "--localstatedir=/var"

                               ;; XXX: Fix the following to allow system-wide
                               ;; config.
                               ;; "--sysconfdir=/etc"

                               "--with-session-socket-dir=/tmp")
       #:patches (list (assoc-ref %build-inputs "patch/localstatedir"))))
    (inputs
     `(("expat" ,expat)
       ("pkg-config" ,pkg-config)
       ("patch/localstatedir"
        ,(search-patch "dbus-localstatedir.patch"))

       ;; Add a dependency on libx11 so that 'dbus-launch' has support for
       ;; '--autolaunch'.
       ("libx11" ,libx11)))

    (home-page "http://dbus.freedesktop.org/")
    (synopsis "Message bus for inter-process communication (IPC)")
    (description
     "D-Bus is a message bus system, a simple way for applications to
talk to one another. In addition to interprocess communication, D-Bus
helps coordinate process lifecycle; it makes it simple and reliable to
code a \"single instance\" application or daemon, and to launch
applications and daemons on demand when their services are needed.

D-Bus supplies both a system daemon (for events such as \"new hardware
device added\" or \"printer queue changed\") and a
per-user-login-session daemon (for general IPC needs among user
applications). Also, the message bus is built on top of a general
one-to-one message passing framework, which can be used by any two apps
to communicate directly (without going through the message bus
daemon). Currently the communicating applications are on one computer,
or through unencrypted TCP/IP suitable for use behind a firewall with
shared NFS home directories.")
    (license license:gpl2+)))                     ; or Academic Free License 2.1

(define glib
  (package
   (name "glib")
   (version "2.38.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/"
                                name "/" (string-take version 4) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32 "0cpzqadqk6z6bmb79p04pykxc8x57rvshh33414cnk41bvgaf4vm"))))
   (build-system gnu-build-system)
   (outputs '("out"                        ; everything
              "doc"))                      ; 20 MiB of GTK-Doc reference
   (inputs
    `(("coreutils" ,coreutils)
      ("gettext" ,guix:gettext)
      ("libffi" ,libffi)
      ("pkg-config" ,pkg-config)
      ("python" ,python-wrapper)
      ("zlib" ,zlib)
      ("perl" ,perl)                              ; needed by GIO tests
      ("dbus" ,dbus)                              ; for GDBus tests
      ("bash" ,bash)
      ("tzdata" ,tzdata)                          ; for tests/gdatetime.c

      ("patch/tests-homedir"
       ,(search-patch "glib-tests-homedir.patch"))
      ("patch/tests-desktop"
       ,(search-patch "glib-tests-desktop.patch"))
      ("patch/tests-prlimit"
       ,(search-patch "glib-tests-prlimit.patch"))))
   (arguments
    '(#:patches (list (assoc-ref %build-inputs "patch/tests-homedir")
                      (assoc-ref %build-inputs "patch/tests-desktop")
                      (assoc-ref %build-inputs "patch/tests-prlimit"))
      #:phases (alist-cons-before
                'build 'pre-build
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  ;; For tests/gdatetime.c.
                  (setenv "TZDIR"
                          (string-append (assoc-ref inputs "tzdata")
                                         "/share/zoneinfo"))

                  ;; Some tests want write access there.
                  (setenv "XDG_CACHE_HOME" (getcwd))

                  (substitute* '("glib/gspawn.c"
                                 "glib/tests/utils.c"
                                 "tests/spawn-test.c")
                    (("/bin/sh")
                     (string-append (assoc-ref inputs "bash") "/bin/sh"))))
                %standard-phases)

      ;; Note: `--docdir' and `--htmldir' are not honored, so work around it.
      #:configure-flags (list (string-append "--with-html-dir="
                                             (assoc-ref %outputs "doc")
                                             "/share/gtk-doc"))))
   (synopsis "Thread-safe general utility library; basis of GTK+ and GNOME")
   (description
    "GLib provides data structure handling for C, portability wrappers,
and interfaces for such runtime functionality as an event loop, threads,
dynamic loading, and an object system.")
   (home-page "http://developer.gnome.org/glib/")
   (license license:lgpl2.0+)))                        ; some files are under lgpl2.1+

(define gobject-introspection
  (package
    (name "gobject-introspection")
    (version "1.38.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://ftp.gnome.org/pub/GNOME/sources/"
                   "gobject-introspection/"
                   (substring version 0 (string-rindex version #\.))
                   "/gobject-introspection-"
                   version ".tar.xz"))
             (sha256
              (base32 "0wvxyvgajmms2bb6k3pf1rdpnd79xdxamykzvxzmcyn1ag9yax9m"))))
    (build-system gnu-build-system)
    (inputs
     `(("bison" ,bison)
       ("cairo" ,cairo)
       ("flex" ,flex)
       ("glib" ,glib)
       ("libffi" ,libffi)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (arguments
     `(#:phases
        (alist-replace
         'configure
         (lambda* (#:key #:allow-other-keys #:rest args)
          (let ((configure (assoc-ref %standard-phases 'configure)))
           ;; giscanner/sourcescanner.py looks for 'CC', let's set it here.
           (setenv "CC" "gcc")
           (apply configure args)))
         %standard-phases)))
    (home-page "https://wiki.gnome.org/GObjectIntrospection")
    (synopsis "Generate interface introspection data for GObject libraries")
    (description
     "GObject introspection is a middleware layer between C libraries (using
GObject) and language bindings.  The C library can be scanned at compile time
and generate a metadata file, in addition to the actual native C library.  Then
at runtime, language bindings can read this metadata and automatically provide
bindings to call into the C library.")
    ; Some bits are distributed under the LGPL2+, others under the GPL2+
    (license license:gpl2+)))

(define intltool
  (package
    (name "intltool")
    (version "0.50.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://launchpad.net/intltool/trunk/"
                                 version "/+download/intltool-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "01j4yd7i84n9nk4ccs6yifg84pp68nr9by57jdbhj7dpdxf5rwk7"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(;; Propagate gettext because users expect it to be there, and so does
       ;; the `intltool-update' script.
       ("gettext" ,guix:gettext)

       ;; `file' is used by `intltool-update' too.
       ("file" ,file)

       ("perl-xml-parser" ,perl-xml-parser)
       ("perl" ,perl)))
    (home-page "https://launchpad.net/intltool/+download")
    (synopsis "Tools to centralise translations of different file formats")
    (description
     "intltool is a set of tools to centralise translations of many different
file formats using GNU gettext-compatible PO files.

The intltool collection can be used to do these things:

    Extract translatable strings from various source files (.xml.in,
    glade, .desktop.in, .server.in, .oaf.in).

    Collect the extracted strings together with messages from traditional
    source files (.c, .h) in po/$(PACKAGE).pot.

    Merge back the translations from .po files into .xml, .desktop and
    oaf files. This merge step will happen at build resp. installation time.")
    (license license:gpl2+)))

(define itstool
  (package
    (name "itstool")
    (version "1.2.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://files.itstool.org/itstool/itstool-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1akq75aflihm3y7js8biy7b5mw2g11vl8yq90gydnwlwp0zxdzj6"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libxml2" ,libxml2)
       ("python-2" ,python-2)))
    (home-page "http://www.itstool.org")
    (synopsis "Tool to translate XML documents with PO files")
    (description
     "ITS Tool allows you to translate your XML documents with PO files, using
rules from the W3C Internationalization Tag Set (ITS) to determine what to
translate and how to separate it into PO file messages.

PO files are the standard translation format for GNU and other Unix-like
systems.  They present translatable information as discrete messages, allowing
each message to be translated independently.  In contrast to whole-page
translation, translating with a message-based format like PO means you can
easily track changes to the source document down to the paragraph.  When new
strings are added or existing strings are modified, you only need to update the
corresponding messages.

ITS Tool is designed to make XML documents translatable through PO files by
applying standard ITS rules, as well as extension rules specific to ITS Tool.
ITS also provides an industry standard way for authors to override translation
information in their documents, such as whether a particular element should be
translated.")
    (license license:gpl3+)))

(define dbus-glib
  (package
    (name "dbus-glib")
    (version "0.100.2")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "http://dbus.freedesktop.org/releases/dbus-glib/dbus-glib-"
                             version ".tar.gz"))
             (sha256
              (base32
               "1ibav91yg70f2l3l18cr0hf4mna1h9d4mrg0c60w4l8zjbd45fx5"))))
    (build-system gnu-build-system)
    (inputs
     `(("dbus" ,dbus)
       ("expat" ,expat)
       ("glib" ,glib)
       ("pkg-config" ,pkg-config)))
    (home-page "http://dbus.freedesktop.org/doc/dbus-glib/")
    (synopsis "D-Bus GLib bindings")
    (description
     "GLib bindings for D-Bus.  The package is obsolete and superseded
by GDBus included in Glib.")
    (license license:gpl2)))                     ; or Academic Free License 2.1

(define libsigc++
  (package
    (name "libsigc++")
    (version "2.3.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/libsigc++/2.3/libsigc++-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "14q3sq6d43f6wfcmwhw4v1aal4ba0h5x9v6wkxy2dnqznd95il37"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (native-inputs `(("m4" ,m4)))
    (home-page "http://libsigc.sourceforge.net/")
    (synopsis "Type-safe callback system for standard C++")
    (description
     "libsigc++ implements a type-safe callback system for standard C++.  It
allows you to define signals and to connect those signals to any callback
function, either global or a member function, regardless of whether it is
static or virtual.

It also contains adaptor classes for connection of dissimilar callbacks and
has an ease of use unmatched by other C++ callback libraries.")
    (license license:lgpl2.1+)))

(define glibmm
  (package
    (name "glibmm")
    (version "2.37.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/glibmm/2.37/glibmm-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0mms4yl5izsya1135772z4jkb184ss86x0wlg6dm7yvwxvb6bjlw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-cons-before
                 'build 'pre-build
                 (lambda _
                   ;; This test uses /etc/fstab as an example file to read
                   ;; from; choose a better example.
                   (substitute* "tests/giomm_simple/main.cc"
                     (("/etc/fstab")
                      (string-append (getcwd)
                                     "/tests/giomm_simple/main.cc")))

                   ;; This test does a DNS lookup, and then expects to be able
                   ;; to open a TLS session; just skip it.
                   (substitute* "tests/giomm_tls_client/main.cc"
                     (("Gio::init.*$")
                      "return 77;\n")))
                 %standard-phases)))
    (inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libsigc++" ,libsigc++)
       ("glib" ,glib)))
    (home-page "http://gtkmm.org/")
    (synopsis "C++ interface to the GLib library")
    (description
     "glibmm provides a C++ programming interface to the part of GLib that are
useful for C++.")
    (license license:lgpl2.1+)))
