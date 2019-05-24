;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)

  ;; Export variables up-front to allow circular dependency with the 'xorg'
  ;; module.
  #:export (dbus
            glib
            gobject-introspection
            dbus-glib
            intltool
            itstool
            libsigc++
            glibmm
            telepathy-glib
            perl-net-dbus
            perl-net-dbus-glib))

(define dbus
  (package
    (name "dbus")
    (version "1.12.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dbus.freedesktop.org/releases/dbus/dbus-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1y7mxhkw2shd9mi9s62k81lz8npjkrafapr4fyfms7hs04kg4ilm"))
              (patches (search-patches "dbus-helper-search-path.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list
        ;; Install the system bus socket under /var.
        "--localstatedir=/var"

        ;; Install the session bus socket under /tmp.
        "--with-session-socket-dir=/tmp"

        ;; Use /etc/dbus-1 for system-wide config.
        ;; Look for configuration file under
        ;; /etc/dbus-1.  This is notably required by
        ;; 'dbus-daemon-launch-helper', which looks for
        ;; the 'system.conf' file in that place,
        ;; regardless of what '--config-file' was
        ;; passed to 'dbus-daemon' on the command line;
        ;; see <https://bugs.freedesktop.org/show_bug.cgi?id=92458>.
        "--sysconfdir=/etc")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
                  (lambda _
                    ;; Don't try to create /var and /etc.
                    (invoke "make"
                            "localstatedir=/tmp/dummy"
                            "sysconfdir=/tmp/dummy"
                            "install"))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("expat" ,expat)

       ;; Add a dependency on libx11 so that 'dbus-launch' has support for
       ;; '--autolaunch'.
       ("libx11" ,libx11)))

    (home-page "https://www.freedesktop.org/wiki/Software/dbus/")
    (synopsis "Message bus for inter-process communication (IPC)")
    (description
     "D-Bus is a message bus system, a simple way for applications to
talk to one another.  In addition to interprocess communication, D-Bus
helps coordinate process lifecycle; it makes it simple and reliable to
code a \"single instance\" application or daemon, and to launch
applications and daemons on demand when their services are needed.

D-Bus supplies both a system daemon (for events such as \"new hardware
device added\" or \"printer queue changed\") and a
per-user-login-session daemon (for general IPC needs among user
applications).  Also, the message bus is built on top of a general
one-to-one message passing framework, which can be used by any two apps
to communicate directly (without going through the message bus
daemon).  Currently the communicating applications are on one computer,
or through unencrypted TCP/IP suitable for use behind a firewall with
shared NFS home directories.")
    (license license:gpl2+)))                     ; or Academic Free License 2.1

(define glib
  (package
   (name "glib")
   (version "2.56.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/"
                                name "/" (string-take version 4) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "1cjcqz77m62zrx7224vl3f2cxwqf28r5xpqb2jy7av0vr2scb959"))
            (patches (search-patches "glib-tests-timer.patch"))))
   (build-system gnu-build-system)
   (outputs '("out"           ; everything
              "bin"           ; glib-mkenums, gtester, etc.; depends on Python
              "doc"))         ; 20 MiB of GTK-Doc reference
   (propagated-inputs
    `(("pcre" ,pcre))) ; in the Requires.private field of glib-2.0.pc
   (inputs
    `(("coreutils" ,coreutils)
      ("util-linux" ,util-linux)  ; for libmount
      ("libffi" ,libffi)
      ("zlib" ,zlib)))
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("dbus" ,dbus)                              ; for GDBus tests
      ("pkg-config" ,pkg-config)
      ("python" ,python-wrapper)
      ("perl" ,perl)                              ; needed by GIO tests
      ("bash" ,bash)
      ("tzdata" ,tzdata-for-tests)))                  ; for tests/gdatetime.c
   (arguments
    `(#:disallowed-references (,tzdata-for-tests)
      #:phases
      (modify-phases %standard-phases
        (add-before 'build 'pre-build
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; For tests/gdatetime.c.
            (setenv "TZDIR"
                    (string-append (assoc-ref inputs "tzdata")
                                   "/share/zoneinfo"))

            ;; Some tests want write access there.
            (setenv "HOME" (getcwd))
            (setenv "XDG_CACHE_HOME" (getcwd))

            (substitute* '("glib/gspawn.c"
                           "glib/tests/utils.c"
                           "tests/spawn-test.c")
              (("/bin/sh")
               (string-append (assoc-ref inputs "bash") "/bin/sh")))
            #t))
        (add-before 'check 'disable-failing-tests
          (lambda _
            (let ((disable
                   (lambda (test-file test-paths)
                     (define pattern+procs
                       (map (lambda (test-path)
                              (cons
                               ;; XXX: only works for single line statements.
                               (format #f "g_test_add_func.*\"~a\".*" test-path)
                               (const "")))
                            test-paths))
                     (substitute test-file pattern+procs)))
                  (failing-tests
                   '(("glib/tests/thread.c"
                      (;; prlimit(2) returns ENOSYS on Linux 2.6.32-5-xen-amd64
                       ;; as found on hydra.gnu.org, and strace(1) doesn't
                       ;; recognize it.
                       "/thread/thread4"))

                     ("glib/tests/timer.c"
                      (;; fails if compiler optimizations are enabled, which they
                       ;; are by default.
                       "/timer/stop"))

                     ("gio/tests/gapplication.c"
                      (;; XXX: proven to be unreliable.  See:
                       ;;  <https://bugs.debian.org/756273>
                       ;;  <http://bugs.gnu.org/18445>
                       "/gapplication/quit"

                       ;; XXX: fails randomly for unknown reason. See:
                       ;;  <https://lists.gnu.org/archive/html/guix-devel/2016-04/msg00215.html>
                       "/gapplication/local-actions"))

                     ("gio/tests/contenttype.c"
                      (;; XXX: requires shared-mime-info.
                       "/contenttype/guess"
                       "/contenttype/guess_svg_from_data"
                       "/contenttype/subtype"
                       "/contenttype/list"
                       "/contenttype/icon"
                       "/contenttype/symbolic-icon"
                       "/contenttype/tree"))

                     ("gio/tests/appinfo.c"
                      (;; XXX: requires update-desktop-database.
                       "/appinfo/associations"))

                     ("gio/tests/desktop-app-info.c"
                      (;; XXX: requires update-desktop-database.
                       "/desktop-app-info/delete"
                       "/desktop-app-info/default"
                       "/desktop-app-info/fallback"
                       "/desktop-app-info/lastused"
                       "/desktop-app-info/search"))

                     ("gio/tests/gdbus-peer.c"
                      (;; Requires /etc/machine-id.
                       "/gdbus/codegen-peer-to-peer"))

                     ("gio/tests/gdbus-unix-addresses.c"
                      (;; Requires /etc/machine-id.
                       "/gdbus/x11-autolaunch")))))
              (for-each (lambda (x) (apply disable x)) failing-tests)
              #t))))

      ;; Note: `--docdir' and `--htmldir' are not honored, so work around it.
      #:configure-flags (list (string-append "--with-html-dir="
                                             (assoc-ref %outputs "doc")
                                             "/share/gtk-doc/html"))

      ;; In 'gio/tests', 'gdbus-test-codegen-generated.h' is #included in a
      ;; file that gets compiled possibly before it has been fully generated.
      #:parallel-tests? #f))

   (native-search-paths
    ;; This variable is not really "owned" by GLib, but several related
    ;; packages refer to it: gobject-introspection's tools use it as a search
    ;; path for .gir files, and it's also a search path for schemas produced
    ;; by 'glib-compile-schemas'.
    (list (search-path-specification
           (variable "XDG_DATA_DIRS")
           (files '("share")))
          ;; To load extra gio modules from glib-networking, etc.
          (search-path-specification
           (variable "GIO_EXTRA_MODULES")
           (files '("lib/gio/modules")))))
   (search-paths native-search-paths)

   (synopsis "Thread-safe general utility library; basis of GTK+ and GNOME")
   (description
    "GLib provides data structure handling for C, portability wrappers,
and interfaces for such runtime functionality as an event loop, threads,
dynamic loading, and an object system.")
   (home-page "https://developer.gnome.org/glib/")
   (license license:lgpl2.1+)))

(define gobject-introspection
  (package
    (name "gobject-introspection")
    (version "1.56.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/"
                   "gobject-introspection/" (version-major+minor version)
                   "/gobject-introspection-" version ".tar.xz"))
             (sha256
              (base32 "0jx2kryjd7l0vl5gb3qp1qjfy3cjiizvcd1snsm7pzwrzz67aa2v"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (substitute* "tools/g-ir-tool-template.in"
                   (("#!/usr/bin/env @PYTHON@") "#!@PYTHON@"))
                 #t))
             (patches (search-patches
                       "gobject-introspection-cc.patch"
                       "gobject-introspection-girepository.patch"
                       "gobject-introspection-absolute-shlib-path.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("glib" ,glib)
       ("python-2" ,python-2)))
    (native-inputs
     `(("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(;; In practice, GIR users will need libffi when using
       ;; gobject-introspection.
       ("libffi" ,libffi)))
    (native-search-paths
     (list (search-path-specification
            (variable "GI_TYPELIB_PATH")
            (files '("lib/girepository-1.0")))))
    (search-paths native-search-paths)
    (arguments
     `(;; The build system has at least one race condition involving Gio-2.0.gir
       ;; which causes intermittent failures, as of 1.56.0.
       #:parallel-build? #f
       ;; The patch 'gobject-introspection-absolute-shlib-path.patch' causes
       ;; some tests to fail.
       #:tests? #f))
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
    (version "0.51.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://launchpad.net/intltool/trunk/"
                                 version "/+download/intltool-"
                                 version ".tar.gz"))
             (patches (search-patches "intltool-perl-compatibility.patch"))
             (sha256
              (base32
               "1karx4sb7bnm2j67q0q74hspkfn6lqprpy5r99vkn5bb36a4viv7"))))
    (build-system gnu-build-system)
    (inputs
     `(("file" ,file)))
    (propagated-inputs
     `(;; Propagate gettext because users expect it to be there, and so does
       ;; the `intltool-update' script.
       ("gettext" ,gettext-minimal)

       ("perl-xml-parser" ,perl-xml-parser)
       ("perl" ,perl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-file-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((file (assoc-ref inputs "file")))
               (substitute* "intltool-update.in"
                 (("`file") (string-append "`" file "/bin/file")))
               #t))))))
    (home-page "https://launchpad.net/intltool/+download")
    (synopsis "Tools to centralise translations of different file formats")
    (description
     "Intltool is a set of tools to centralise translations of many different
file formats using GNU gettext-compatible PO files.

The intltool collection can be used to do these things:

    Extract translatable strings from various source files (.xml.in,
    glade, .desktop.in, .server.in, .oaf.in).

    Collect the extracted strings together with messages from traditional
    source files (.c, .h) in po/$(PACKAGE).pot.

    Merge back the translations from .po files into .xml, .desktop and
    oaf files.  This merge step will happen at build resp. installation time.")
    (license license:gpl2+)))

(define itstool
  (package
    (name "itstool")
    (version "2.0.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://files.itstool.org/itstool/itstool-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0fh34wi52i0qikgvlmrcpf1vx6gc1xqdad4539l4d9hikfsrz45z"))))
    (build-system gnu-build-system)
    (inputs
     `(("libxml2" ,libxml2)
       ("python2-libxml2" ,python2-libxml2)
       ("python-2" ,python-2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/itstool")))
               (wrap-program prog
                 `("PYTHONPATH" = (,(getenv "PYTHONPATH"))))
               #t))))))
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
    (version "0.108")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "https://dbus.freedesktop.org/releases/dbus-glib/dbus-glib-"
                             version ".tar.gz"))
             (sha256
              (base32
               "0b307hw9j41npzr6niw1bs6ryp87m5yafg492gqwvsaj4dz0qd4z"))))
    (build-system gnu-build-system)
    (propagated-inputs ; according to dbus-glib-1.pc
     `(("dbus" ,dbus)
       ("glib" ,glib)))
    (inputs
     `(("expat" ,expat)))
    (native-inputs
     `(("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (home-page "https://dbus.freedesktop.org/doc/dbus-glib/")
    (synopsis "D-Bus GLib bindings")
    (description
     "GLib bindings for D-Bus.  The package is obsolete and superseded
by GDBus included in Glib.")
    (license license:gpl2)))                     ; or Academic Free License 2.1

(define libsigc++
  (package
    (name "libsigc++")
    (version "2.10.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/libsigc++/"
                                 (version-major+minor version) "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "00v08km4wwzbh6vjxb21388wb9dm6g2xh14rgwabnv4c2wk5z8n9"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("m4" ,m4)))
    (home-page "http://libsigc.sourceforge.net/")
    (synopsis "Type-safe callback system for standard C++")
    (description
     "Libsigc++ implements a type-safe callback system for standard C++.  It
allows you to define signals and to connect those signals to any callback
function, either global or a member function, regardless of whether it is
static or virtual.

It also contains adaptor classes for connection of dissimilar callbacks and
has an ease of use unmatched by other C++ callback libraries.")
    (license license:lgpl2.1+)))

(define glibmm
  (package
    (name "glibmm")
    (version "2.56.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/glibmm/"
                                 (version-major+minor version)
                                 "/glibmm-" version ".tar.xz"))
             (sha256
              (base32
               "1abrkqhca5p8n6ly3vp1232rny03s7lrd8f8iz2m2m141nxgqx3f"))))
    (build-system gnu-build-system)
    (arguments
     `(;; XXX: Some tests uses C++14 features.  Remove this when the default
       ;; compiler is >= GCC6.
       #:configure-flags '("CXXFLAGS=-std=gnu++14")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
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
                "return 77;\n"))
             #t)))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("glib" ,glib "bin")))
    (propagated-inputs
     `(("libsigc++" ,libsigc++)
       ("glib" ,glib)))
    (home-page "https://gtkmm.org/")
    (synopsis "C++ interface to the GLib library")
    (description
     "Glibmm provides a C++ programming interface to the part of GLib that are
useful for C++.")
    (license license:lgpl2.1+)))

(define-public python2-pygobject-2
  (package
    (name "python2-pygobject")
    ;; This was the last version to declare the 2.0 platform number, i.e. its
    ;; pkg-config files were named pygobject-2.0.pc
    (version "2.28.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/pygobject/"
                           (version-major+minor version)
                           "/pygobject-" version ".tar.xz"))
       (sha256
        (base32
         "0nkam61rsn7y3wik3vw46wk5q2cjfh2iph57hl9m39rc8jijb7dv"))
       (patches (search-patches
                 "python2-pygobject-2-gi-info-type-error-domain.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)
       ("glib-bin" ,glib "bin")         ;for tests: glib-compile-schemas
       ("pkg-config" ,pkg-config)
       ("dbus" ,dbus)))                 ;for tests
    (inputs
     `(("python" ,python-2)
       ("glib"   ,glib)
       ("python2-pycairo" ,python2-pycairo)
       ("gobject-introspection" ,gobject-introspection)))
    (propagated-inputs
     `(("libffi" ,libffi)))             ;mentioned in pygobject-2.0.pc
    (arguments
     `(#:tests? #f                      ;segfaults during tests
       #:configure-flags '("LIBS=-lcairo-gobject")))
    (home-page "https://pypi.python.org/pypi/PyGObject")
    (synopsis "Python bindings for GObject")
    (description
     "Python bindings for GLib, GObject, and GIO.")
    (license license:lgpl2.1+)))

(define-public python-pygobject
  (package
    (name "python-pygobject")
    (version "3.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/pygobject/"
                           (version-major+minor version)
                           "/pygobject-" version ".tar.xz"))
       (sha256
        (base32
         "1c6h3brzlyvzbpdsammnd957azmp6cbzqrd65r400vnh2l8f5lrx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-broken-tests
           (lambda _
             ;; FIXME: this test freezes and times out.
             (delete-file "tests/test_mainloop.py")
             ;; FIXME: this test fails with this kind of error:
             ;; AssertionError: <Handlers.SIG_IGN: 1> != <built-in function default_int_handler
             (delete-file "tests/test_ossig.py")
             #t)))))
    (native-inputs
     `(("which" ,which)
       ;for tests: dbus-run-session and glib-compile-schemas
       ("dbus" ,dbus)
       ("glib-bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("python-pytest" ,python-pytest)))
    (inputs
     `(("python" ,python)
       ("python-pycairo" ,python-pycairo)
       ("gobject-introspection" ,gobject-introspection)))
    (propagated-inputs
     ;; pygobject-3.0.pc refers to all these.
     `(("glib" ,glib)
       ("libffi" ,libffi)))
    ;; For finding typelib files, since gobject-introscpetion isn't propagated.
    (native-search-paths (package-native-search-paths gobject-introspection))
    (home-page "https://live.gnome.org/PyGObject")
    (synopsis "Python bindings for GObject")
    (description
     "Python bindings for GLib, GObject, and GIO.")
    (license license:lgpl2.1+)
    (properties `((python2-variant . ,(delay python2-pygobject))))))

(define-public python2-pygobject
  (package (inherit (strip-python2-variant python-pygobject))
    (name "python2-pygobject")
    (inputs
     `(("python" ,python-2)
       ("python-pycairo" ,python2-pycairo)
       ("gobject-introspection" ,gobject-introspection)))
    (native-inputs
     `(("which" ,which)
       ;for tests: dbus-run-session and glib-compile-schemas
       ("dbus" ,dbus)
       ("glib-bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("python-pytest" ,python2-pytest)))))

(define-public perl-glib
  (package
    (name "perl-glib")
    (version "1.329")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/X/XA/XAOC/Glib-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0d9ak0zknz81lv3cqkzr2mxdic6g5rrbb87skqc4jj48rz4f2k3v"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-depends" ,perl-extutils-depends)
       ("perl-extutils-pkgconfig" ,perl-extutils-pkgconfig)))
    (inputs
     `(("glib" ,glib)))
    (home-page "https://metacpan.org/release/Glib")
    (synopsis "Perl wrappers for the GLib utility and Object libraries")
    (description "This module provides perl access to GLib and GLib's GObject
libraries.  GLib is a portability and utility library; GObject provides a
generic type system with inheritance and a powerful signal system.  Together
these libraries are used as the foundation for many of the libraries that make
up the Gnome environment, and are used in many unrelated projects.")
    (license license:lgpl2.1+)))

(define telepathy-glib
  (package
    (name "telepathy-glib")
    (version "0.24.1")
    (source
     (origin
      (method url-fetch)
       (uri
        (string-append
         "https://telepathy.freedesktop.org/releases/telepathy-glib/"
         "telepathy-glib-" version ".tar.gz"))
       (sha256
        (base32
         "1symyzbjmxvksn2ifdkk50lafjm2llf2sbmky062gq2pz3cg23cy"))
       (patches
        (list
         ;; Don't use the same test name for multiple tests.
         ;; <https://bugs.freedesktop.org/show_bug.cgi?id=92245>
         (origin
           (method url-fetch)
           (uri "https://bugs.freedesktop.org/attachment.cgi?id=118608")
           (file-name (string-append "telepathy-glib-duplicate-tests.patch"))
           (sha256
            (base32
             "0z261fwrszxb28ccg3hsg9rizig4s84zvwmx6y31a4pyv7bvs5w3")))))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-vala-bindings")

       ;; '../tools/glib-*.py' generate files but the target dependencies are
       ;; (presumably) not fully specified in the makefile, leading to
       ;; parallel build errors like:
       ;;
       ;;   EOFError: EOF read where object expected
       ;;   make[2]: *** [Makefile:1906: _gen/register-dbus-glib-marshallers-body.h] Error 1
       #:parallel-build? #f))
    (native-inputs
     `(("glib" ,glib "bin") ; uses glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     ;; There are all in the Requires.private field of telepathy-glib.pc.
     `(("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("glib" ,glib)))
    (home-page "https://telepathy.freedesktop.org/wiki/")
    (synopsis "GLib Real-time communications framework over D-Bus")
    (description "Telepathy is a flexible, modular communications framework
that enables real-time communication over D-Bus via pluggable protocol
backends.  Telepathy is a communications service that can be accessed by
many applications simultaneously.

This package provides the library for GLib applications.")
    (license license:lgpl2.1+)))

(define-public dbus-c++
  (package
    (name "dbus-c++")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "mirror://sourceforge/dbus-cplusplus/dbus-c%2B%2B/"
                version "/libdbus-c%2B%2B-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qafmy2i6dzx4n1dqp6pygyy6gjljnb7hwjcj2z11c1wgclsq4dw"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("dbus" ,dbus)))                      ;mentioned in the pkg-config file
    (inputs
     `(("efl" ,efl)
       ("expat" ,expat)
       ("glib" ,glib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(;; The 'configure' machinery fails to detect that it needs -lpthread.
       #:configure-flags (list "LDFLAGS=-lpthread")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'add-missing-header
           (lambda _
             (substitute* "include/dbus-c++/eventloop-integration.h"
               (("#include <errno.h>")
                "#include <errno.h>\n#include <unistd.h>")))))))
    (synopsis "D-Bus API for C++")
    (description "This package provides D-Bus client API bindings for the C++
programming language.  It also contains the utility
@command{dbuscxx-xml2cpp}.")
    (home-page "https://sourceforge.net/projects/dbus-cplusplus/")
    (license license:lgpl2.1+)))

(define-public appstream-glib
  (package
    (name "appstream-glib")
    (version "0.7.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://people.freedesktop.org/~hughsient/"
                                  "appstream-glib/releases/"
                                  "appstream-glib-" version ".tar.xz"))
              (sha256
               (base32
                "17ram4j3cvs78si0gi1labarr91pw68zn8kam41cqbvlwdyr89vy"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")         ; for glib-compile-resources
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("gcab" ,gcab) ; for .pc file
       ("gdk-pixbuf" ,gdk-pixbuf) ; for .pc file
       ("util-linux" ,util-linux))) ; for .pc file
    (inputs
     `(("glib" ,glib)
       ("gperf" ,gperf)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libarchive" ,libarchive)
       ("libsoup" ,libsoup)
       ("libyaml" ,libyaml)))
    (arguments
     `(#:configure-flags
       (list "-Ddep11=false"
             "-Dintrospection=false"    ; avoid g-ir-scanner dependency
             "-Drpm=false"
             "-Dstemmer=false")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "libappstream-glib/as-self-test.c"
               (("g_test_add_func.*as_test_store_local_appdata_func);") ""))
             #t)))))
    (home-page "https://github.com/hughsie/appstream-glib")
    (synopsis "Library for reading and writing AppStream metadata")
    (description "This library provides objects and helper methods to help
reading and writing @uref{https://www.freedesktop.org/wiki/Distributions/AppStream,AppStream}
metadata.")
    (license license:lgpl2.1+)))

(define perl-net-dbus
  (package
    (name "perl-net-dbus")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DANBERR/Net-DBus-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0sg2w147b9r9ykfzjs7y9qxry73xkjnhnk4qf95kfv79p5nnk4c3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (inputs
     `(("dbus" ,dbus)))
    (propagated-inputs
     `(("perl-xml-twig" ,perl-xml-twig)))
    (home-page "https://metacpan.org/release/Net-DBus")
    (synopsis "Extension for the DBus bindings")
    (description "@code{Net::DBus} provides a Perl XS API to the DBus
inter-application messaging system.  The Perl API covers the core base level
of the DBus APIs, not concerning itself yet with the GLib or QT wrappers.")
    (license license:perl-license)))

(define perl-net-dbus-glib
  (package
    (name "perl-net-dbus-glib")
    (version "0.33.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DANBERR/"
                           "Net-DBus-GLib-" version ".tar.gz"))
       (sha256
        (base32
         "1z4mbv8z0rad604xahijpg5szzi8qak07hbahh230z4jf96fkxvj"))))
    (build-system perl-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("dbus-glib" ,dbus-glib)))
    (home-page "https://metacpan.org/release/Net-DBus-GLib")
    (synopsis "Perl extension for the DBus GLib bindings")
    (description "This package provides an extension to the @code{Net::DBus}
module allowing integration with the GLib mainloop.  To integrate with the
main loop, simply get a connection to the bus via the methods in
@code{Net::DBus::GLib} rather than the usual @code{Net::DBus} module.  Every
other API remains the same.")
    (license license:gpl2+)))
