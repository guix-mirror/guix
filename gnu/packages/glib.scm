;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix licenses) #:select (lgpl2.0+ gpl2+ gpl2))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module ((gnu packages xml)
                #:renamer (symbol-prefix-proc 'xml:)))

(define-public dbus
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
    (inputs
     `(("expat" ,xml:expat)
       ("pkg-config" ,pkg-config)))
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
    (license gpl2+)))                     ; or Academic Free License 2.1

(define-public glib
  (package
   (name "glib")
   (version "2.34.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://ftp.gnome.org/pub/gnome/sources/"
                                name "/2.34/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32 "19sq4rhl2vr8ikjvl8qh51vr38yqfhbkb3imi2s6ac5rgkwcnpw5"))))
   (build-system gnu-build-system)
   (outputs '("out"                        ; everything
              "doc"))                      ; 20 MiB of GTK-Doc reference
   (inputs
    `(("coreutils" ,coreutils)
      ("gettext" ,guix:gettext)
      ("libffi" ,libffi)
      ("pkg-config" ,pkg-config)
      ("python" ,python)
      ("zlib" ,zlib)
      ("perl" ,perl)                              ; needed by GIO tests
      ("dbus" ,dbus)                              ; for GDBus tests

      ("patch/tests-tzdata"
       ,(search-patch "glib-tests-timezone.patch"))
      ("patch/tests-homedir"
       ,(search-patch "glib-tests-homedir.patch"))
      ("patch/tests-desktop"
       ,(search-patch "glib-tests-desktop.patch"))))
   (arguments
    '(#:patches (list (assoc-ref %build-inputs "patch/tests-tzdata")
                      (assoc-ref %build-inputs "patch/tests-homedir")
                      (assoc-ref %build-inputs "patch/tests-desktop"))
      #:phases (alist-cons-before
                'build 'pre-build
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  (substitute* '("glib/gspawn.c"
                                 "glib/tests/utils.c"
                                 "tests/spawn-test.c")
                    (("/bin/sh") (which "sh"))))
                %standard-phases)

      ;; Note: `--docdir' and `--htmldir' are not honored, so work around it.
      #:configure-flags (list (string-append "--with-html-dir="
                                             (assoc-ref %outputs "doc")
                                             "/share/gtk-doc"))))
   (synopsis "C library that provides core application building blocks")
   (description
    "GLib provides data structure handling for C, portability wrappers,
and interfaces for such runtime functionality as an event loop, threads,
dynamic loading, and an object system.")
   (home-page "http://developer.gnome.org/glib/")
   (license lgpl2.0+)))                        ; some files are under lgpl2.1+

(define-public intltool
  (package
    (name "intltool")
    (version "0.40.6")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://gnome/sources/intltool/0.40/intltool-"
                   version
                   ".tar.bz2"))
             (sha256
              (base32
               "0r1vkvy5xzqk01yl6a0xlrry39bra24alkrx6279b77hc62my7jd"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'set-perl-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; FIXME: Remove this phase when proper support for search
                   ;; paths is available.
                   (let ((xml-parser (assoc-ref inputs "perl-xml-parser")))
                     (setenv "PERL5LIB"
                             (string-append xml-parser
                                            "/lib/perl5/site_perl"))
                     #t))
                 %standard-phases)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("gettext" ,guix:gettext)
       ("perl-xml-parser" ,xml:perl-xml-parser)
       ("perl" ,perl)))
    (home-page "http://freedesktop.org/wiki/Software/intltool")
    (synopsis "Tools to centralize translation of many different file formats")
    (description
     "intltool is a set of tools to centralize translation of many different
file formats using GNU gettext-compatible PO files.

The intltool collection can be used to do these things:

    Extract translatable strings from various source files (.xml.in,
    glade, .desktop.in, .server.in, .oaf.in).

    Collect the extracted strings together with messages from traditional
    source files (.c, .h) in po/$(PACKAGE).pot.

    Merge back the translations from .po files into .xml, .desktop and
    oaf files. This merge step will happen at build resp. installation time.")
    (license gpl2)))
