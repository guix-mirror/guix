;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages gnome)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)  
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages image)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public brasero
  (package
    (name "brasero")
    (version "3.8.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/brasero/3.8/brasero-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1r5wjsrm47amdaf862ymkdlwlb636c45wg14x20hdr99c653d2nr"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")                       ; glib-compile-schemas, etc.
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gnome-doc-utils" ,gnome-doc-utils)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)
       ("itstool" ,itstool)
       ("libcanberra" ,libcanberra)
       ("libice" ,libice)
       ("libnotify" ,libnotify)
       ("libsm" ,libsm)
       ("libxml2" ,libxml2)))
    (home-page "https://projects.gnome.org/brasero/")
    (synopsis "CD/DVD burning tool for Gnome")
    (description "Brasero is an application to burn CD/DVD for the Gnome
Desktop.  It is designed to be as simple as possible and has some unique
features to enable users to create their discs easily and quickly.")
    (license license:gpl2+)))

(define-public gnome-desktop
  (package
    (name "gnome-desktop")
    (version "3.10.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/3.10/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "0p5p6wvmy5zvcdnmp5h2biz7rjrcw99chq5kkwcnb68flcmkb1ry"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("iso-codes" ,iso-codes)
       ("itstool" ,itstool)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxkbfile" ,libxkbfile)
       ("libxrandr" ,libxrandr)
       ("xkeyboard-config" ,xkeyboard-config)))
    (home-page "https://www.gnome.org/")
    (synopsis
     "libgnome-desktop, gnome-about, and desktop-wide documents")
    (description
     "The libgnome-desktop library provides API shared by several applications
on the desktop, but that cannot live in the platform for various reasons. There
is no API or ABI guarantee, although we are doing our best to provide
stability. Documentation for the API is available with gtk-doc.

The gnome-about program helps find which version of GNOME is installed.")
    ; Some bits under the LGPL.
    (license license:gpl2+)))

(define-public gnome-doc-utils
  (package
    (name "gnome-doc-utils")
    (version "0.20.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/0.20/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "19n4x25ndzngaciiyd8dd6s2mf9gv6nv3wv27ggns2smm7zkj1nb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-cons-before
        'check 'pre-check
        (lambda* (#:key inputs #:allow-other-keys #:rest args)
          ;; This is needed, because without it, xmlint etc tries
          ;; to download docbookx.dtd from the net
          (setenv "XML_CATALOG_FILES" 
                  (string-append (assoc-ref inputs "docbook-xml") 
                                 "/xml/dtd/docbook/catalog.xml")))
        %standard-phases)))
    (native-inputs
     `(("intltool" ,intltool)
       ("docbook-xml" ,docbook-xml-4.4)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (home-page "https://wiki.gnome.org/GnomeDocUtils")
    (synopsis
     "Documentation utilities for the Gnome project")
    (description
     "Gnome-doc-utils is a collection of documentation utilities for the
Gnome project.  It includes xml2po tool which makes it easier to translate
and keep up to date translations of documentation.")
    (license license:gpl2+))) ; xslt under lgpl

(define-public libgnome-keyring
  (package
    (name "libgnome-keyring")
    (version "3.6.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://gnome/sources/libgnome-keyring/3.6/libgnome-keyring-"
                   version
                   ".tar.xz"))
             (sha256
              (base32
               "0c4qrjpmv1hqga3xv6wsq2z10x2n78qgw7q3k3s01y1pggxkgjkd"))))
    (build-system gnu-build-system)
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("dbus" ,dbus)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     `(("glib" ,glib)))
    (home-page "http://www.gnome.org")
    (synopsis "Accessing passwords from the GNOME keyring")
    (description
     "Client library to access passwords from the GNOME keyring.")

    ;; Though a couple of files are LGPLv2.1+.
    (license license:lgpl2.0+)))

(define-public evince
  (package
    (name "evince")
    (version "3.6.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/evince/3.6/evince-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1da1pij030dh8mb0pr0jnyszgsbjnh8lc17rj5ii52j3kmbv51qv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-nautilus")

       ;; FIXME: Tests fail with:
       ;;   ImportError: No module named gi.repository
       ;; Where should that module come from?
       #:tests? #f

       #:phases (alist-cons-after
                 'install 'set-mime-search-path
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; Wrap 'evince' so that it knows where MIME info is.
                   (let ((out  (assoc-ref outputs "out"))
                         (mime (assoc-ref inputs "shared-mime-info")))
                     (wrap-program (string-append out "/bin/evince")
                                   `("XDG_DATA_DIRS" ":" prefix
                                     ,(list (string-append mime "/share")
                                            (string-append out "/share"))))))
                 %standard-phases)))
    (inputs
     `(("libspectre" ,libspectre)
       ;; ("djvulibre" ,djvulibre)
       ("ghostscript" ,ghostscript)
       ("poppler" ,poppler)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libgnome-keyring" ,libgnome-keyring)
       ("gnome-icon-theme" ,gnome-icon-theme)
       ("itstool" ,itstool)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("atk" ,atk)
       ("pango" ,pango)
       ("gtk+" ,gtk+)
       ("glib" ,glib)
       ("libxml2" ,libxml2)
       ("libsm" ,libsm)
       ("libice" ,libice)
       ("shared-mime-info" ,shared-mime-info)

       ;; For tests.
       ("dogtail" ,python2-dogtail)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (home-page
     "http://www.gnome.org/projects/evince/")
    (synopsis "GNOME's document viewer")
    (description
     "Evince is a document viewer for multiple document formats.  It
currently supports PDF, PostScript, DjVu, TIFF and DVI.  The goal
of Evince is to replace the multiple document viewers that exist
on the GNOME Desktop with a single simple application.")
    (license license:gpl2+)))

(define-public gsettings-desktop-schemas
  (package
    (name "gsettings-desktop-schemas")
    (version "3.10.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (string-copy version 0 (string-rindex version #\.)) "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "1km8qxwrzvravmg8j680qv64bwnwbdgrmy8bqmhs0dgxn2b1as6a"))))
    (build-system gnu-build-system)
    (inputs
     `(("glib" ,glib)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")                       ; glib-compile-schemas, etc.
       ("pkg-config" ,pkg-config)))
    (home-page "https://launchpad.net/gsettings-desktop-schemas")
    (synopsis
     "GNOME settings for various desktop components")
    (description
     "Gsettings-desktop-schemas contains a collection of GSettings schemas
for settings shared by various components of the GNOME desktop.")
    (license license:lgpl2.1+)))

(define-public icon-naming-utils
  (package
    (name "icon-naming-utils")
    (version "0.8.90")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://tango.freedesktop.org/releases/icon-naming-utils-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1mc3v28fdfqanx3lqx233vcr4glb4c2376k0kx2v91a4vxwqcdxi"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl)
       ("perl-xml-simple" ,perl-xml-simple)))
    (home-page "http://tango.freedesktop.org/Standard_Icon_Naming_Specification")
    (synopsis
     "Utility to implement the Freedesktop Icon Naming Specification")
    (description
     "To help with the transition to the Freedesktop Icon Naming
Specification, the icon naming utility maps the icon names used by the
GNOME and KDE desktops to the icon names proposed in the specification.")
    (license license:lgpl2.1+)))

(define-public gnome-icon-theme
  (package
    (name "gnome-icon-theme")
    (version "3.10.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (string-copy version 0 (string-rindex version #\.)) "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "1xinbgkkvlhazj887ajcl13i7kdc1wcca02jwxzvjrvchjsp4m66"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk+" ,gtk+)
       ("icon-naming-utils" ,icon-naming-utils)))
    (native-inputs
       `(("intltool" ,intltool)
         ("pkg-config" ,pkg-config)))
    (home-page "http://art.gnome.org/")
    (synopsis
     "GNOME icon theme")
    (description
     "Icons for the GNOME desktop.")
    (license license:lgpl3))) ; or Creative Commons BY-SA 3.0

(define-public shared-mime-info
  (package
    (name "shared-mime-info")
    (version "1.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://freedesktop.org/~hadess/shared-mime-info-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0y5vi0vr6rbhvfzcfg57cfskn362bpvcpca9cy598nmr87i6lld5"))))
    (build-system gnu-build-system)
    (arguments
     ;; The build system appears not to be parallel-safe.
     '(#:parallel-build? #f))
    (inputs
     `(("glib" ,glib)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://freedesktop.org/wiki/Software/shared-mime-info")
    (synopsis "Database of common MIME types")
    (description
     "The shared-mime-info package contains the core database of common types
and the update-mime-database command used to extend it.  It requires glib2 to
be installed for building the update command.  Additionally, it uses intltool
for translations, though this is only a dependency for the maintainers.  This
database is translated at Transifex.")
    (license license:gpl2+)))

(define-public hicolor-icon-theme
  (package
    (name "hicolor-icon-theme")
    (version "0.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://icon-theme.freedesktop.org/releases/hicolor-icon-theme-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0wzc7g4ldb2l8zc0x2785ck808c03i857jji942ikakyc68adp4y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no check target
    (home-page "http://icon-theme.freedesktop.org/releases/")
    (synopsis
     "Freedesktop icon theme")
    (description
     "Freedesktop icon theme.")
    (license license:gpl2)))

(define-public libnotify
  (package
    (name "libnotify")
    (version "0.7.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (string-copy version 0 (string-rindex version #\.)) "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "0dyq8zgjnnzcah31axnx6afb21kl7bks1gvrg4hjh3nk02j1rxhf"))))
    (build-system gnu-build-system)
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libpng" ,libpng)))
    (native-inputs
      `(("pkg-config" ,pkg-config)
        ("glib" ,glib "bin")))
    (home-page "https://developer-next.gnome.org/libnotify/")
    (synopsis
     "GNOME desktop notification library")
    (description
     "Libnotify is a library that sends desktop notifications to a
notification daemon, as defined in the Desktop Notifications spec. These
notifications can be used to inform the user about an event or display
some form of information without getting in the user's way.")
    (license license:lgpl2.1+)))

(define-public libpeas
  (package
    (name "libpeas")
    (version "1.9.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (substring version 0 (string-rindex version #\.)) "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "13fzyzv6c0cfdj83z1s16lv8k997wpnzyzr0wfwcfkcmvz64g1q0"))))
    (build-system gnu-build-system)
    (inputs
     `(("atk" ,atk)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("pango" ,pango)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)))
    (home-page "https://wiki.gnome.org/Libpeas")
    (synopsis "GObject plugin system")
    (description
     "libpeas is a gobject-based plugins engine, and is targetted at giving
every application the chance to assume its own extensibility.  It also has a
set of features including, but not limited to: multiple extension points; on
demand (lazy) programming language support for C, Python and JS; simplicity of
the API")

    (license license:lgpl2.0+)))

(define-public gtkglext
  (package
    (name "gtkglext")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/project/gtkglext/gtkglext/"
                                  version "/gtkglext-" version ".tar.gz"))
              (sha256
               (base32 "1ya4d2j2aacr9ii5zj4ac95fjpdvlm2rg79mgnk7yvl1dcy3y1z5"))
              (patches (list
                        (search-patch "gtkglext-disable-disable-deprecated.patch")))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("libx11" ,libx11)
              ("libxt" ,libxt)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("glib" ,glib "bin")))
    (propagated-inputs `(("pangox-compat" ,pangox-compat)))
    (home-page "https://projects.gnome.org/gtkglext")
    (synopsis "OpenGL extension to GTK+.")
    (description "GtkGLExt is an OpenGL extension to GTK+. It provides
additional GDK objects which support OpenGL rendering in GTK+ and GtkWidget
API add-ons to make GTK+ widgets OpenGL-capable.")
    (license license:lgpl2.1+)))

(define-public glade3
  (package
    (name "glade")
    (version "3.8.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (substring version 0 (string-rindex version #\.)) "/"
                          name "3-" version ".tar.xz"))
              (sha256
               (base32 "021xgq2l18w3rvwms9aq2idm0fk66vwb4f777gs0qh3ap5shgbn7"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk+" ,gtk+-2)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("python" ,python)
       ("pkg-config" ,pkg-config)))
    (home-page "https://glade.gnome.org")
    (synopsis "GTK+ rapid application development tool")
    (description "Glade is a rapid application development (RAD) tool to
enable quick & easy development of user interfaces for the GTK+ toolkit and
the GNOME desktop environment.")
    (license license:lgpl2.0+)))

(define-public libcroco
  (package
    (name "libcroco")
    (version "0.6.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/libcroco/0.6/libcroco-"
                    version
                    ".tar.xz"))
              (sha256
               (base32
                "0w453f3nnkbkrly7spx5lx5pf6mwynzmd5qhszprq8amij2invpa"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("libxml2" ,libxml2)
       ("zlib" ,zlib)))
    (home-page "https://github.com/GNOME/libcroco")
    (synopsis "CSS2 parsing and manipulation library")
    (description
     "Libcroco is a standalone CSS2 parsing and manipulation library.
The parser provides a low level event driven SAC-like API and a CSS object
model like API.  Libcroco provides a CSS2 selection engine and an experimental
XML/CSS rendering engine.")

    ;; LGPLv2.1-only.
    (license license:lgpl2.1)))

(define-public libgsf
  (package
    (name "libgsf")
    (version "1.14.30")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libgsf/1.14/libgsf-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0w2v1a9sxsymd1mcy4mwsz4r6za9iwq69rj86nb939p41d4c6j6b"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python)
       ("zlib" ,zlib)
       ("bzip2" ,bzip2)))
    (propagated-inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("libxml2" ,libxml2)))
    (home-page "http://www.gnome.org/projects/libgsf")
    (synopsis "GNOME's Structured File Library")
    (description
     "Libgsf aims to provide an efficient extensible I/O abstraction for
dealing with different structured file formats.")

    ;; LGPLv2.1-only.
    (license license:lgpl2.1)))

(define-public librsvg
  (package
    (name "librsvg")
    (version "2.40.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/librsvg/2.40/librsvg-"
                    version ".tar.xz"))
              (sha256
               (base32
                "071959yjb2i1bja7ciy4bmpnd6fn2is9jjqsvvvnsqwl69j9n128"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-cons-before
        'configure 'augment-gir-search-path
        (lambda* (#:key inputs #:allow-other-keys)
          (substitute* "gdk-pixbuf-loader/Makefile.in"
            ;; By default the gdk-pixbuf loader is installed under
            ;; gdk-pixbuf's prefix.  Work around that.
            (("gdk_pixbuf_moduledir = .*$")
             (string-append "gdk_pixbuf_moduledir = "
                            "$(prefix)/lib/gdk-pixbuf-2.0/2.0.10/"
                             "loaders\n"))
            ;; Likewise, create a separate 'loaders.cache' file.
            (("gdk_pixbuf_cache_file = .*$")
             "gdk_pixbuf_cache_file = $(gdk_pixbuf_moduledir).cache\n")))
        %standard-phases)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")                               ; glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection))) ; g-ir-compiler, etc.
    (inputs
     `(("pango" ,pango)
       ("libcroco" ,libcroco)
       ("bzip2" ,bzip2)
       ("libgsf" ,libgsf)
       ("libxml2" ,libxml2)))
    (propagated-inputs
     ;; librsvg-2.0.pc refers to all of that.
     `(("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)))
    (home-page "https://wiki.gnome.org/LibRsvg")
    (synopsis "Render SVG files using Cairo")
    (description
     "librsvg is a C library to render SVG files using the Cairo 2D graphics
library.")
    (license license:lgpl2.0+)))

(define-public libidl
  (package
    (name "libidl")
    (version "0.8.14")
    (source (origin
              (method url-fetch)
              (uri (let ((upstream-name "libIDL"))
		     (string-append
		      "mirror://gnome/sources/" upstream-name "/" (string-take version 3) "/" upstream-name "-"
		      version
		      ".tar.bz2")))
              (sha256
               (base32
                "08129my8s9fbrk0vqvnmx6ph4nid744g5vbwphzkaik51664vln5"))))
    (build-system gnu-build-system)
    (inputs `(("glib" ,glib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("flex", flex)
       ("bison" ,bison)))
    (home-page "http://freecode.com/projects/libidl")
    (synopsis "Create trees of CORBA Interface Definition Language files")
    (description  "libidl is a library for creating trees of CORBA Interface
Definition Language (idl) files, which is a specification for defining
portable interfaces. libidl was initially written for orbit (the orb from the
GNOME project, and the primary means of libidl distribution). However, the
functionality was designed to be as reusable and portable as possible.") 
    (license license:lgpl2.0+)))


(define-public orbit2
  (package
    (name "orbit2")
    (version "2.14.19")
    (source (origin
              (method url-fetch)
              (uri (let ((upstream-name "ORBit2")) 
		     (string-append
		      "mirror://gnome/sources/" upstream-name "/" (string-take version 4) "/" upstream-name "-"
		      version
		      ".tar.bz2")))
              (sha256
               (base32 "0l3mhpyym9m5iz09fz0rgiqxl2ym6kpkwpsp1xrr4aa80nlh1jam"))))
    (build-system gnu-build-system)
    (arguments
     ;; The programmer kindly gives us a hook to turn off deprecation warnings ...
     `(#:configure-flags '("DISABLE_DEPRECATED_CFLAGS=-DGLIB_DISABLE_DEPRECATION_WARNINGS")
                         ;; ... which they then completly ignore !!
                         #:phases
                         (alist-cons-before
                          'configure 'ignore-deprecations
                          (lambda _
                            (substitute* "linc2/src/Makefile.in"
                              (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS")))
                          %standard-phases)))
    (inputs `(("glib" ,glib)
              ("libidl" ,libidl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/orbit2/")
    (synopsis "CORBA 2.4-compliant Object Request Broker")
    (description  "orbit2 is a CORBA 2.4-compliant Object Request Broker (orb)
featuring mature C, C++ and Python bindings.") 
    ;; Licence notice is unclear.  The Web page simply say "GPL" without giving a version.
    ;; SOME of the code files have licence notices for GPLv2+
    ;; The tarball contains files of the text of GPLv2 and LGPLv2
    (license license:gpl2+))) 


(define-public libbonobo
  (package
    (name "libbonobo")
    (version "2.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (string-join (take (string-split version #\.) 2)
                                               ".")
                                  "/" name "-" version ".tar.bz2"))
              (sha256
               (base32 "0swp4kk6x7hy1rvd1f9jba31lvfc6qvafkvbpg9h0r34fzrd8q4i"))
              (patches (list (search-patch "libbonobo-activation-test-race.patch")))))
    (build-system gnu-build-system)
    (arguments
     ;; The programmer kindly gives us a hook to turn off deprecation warnings ...
     `(#:configure-flags
       '("DISABLE_DEPRECATED_CFLAGS=-DGLIB_DISABLE_DEPRECATION_WARNINGS")
       ;; ... which they then completly ignore !!
       #:phases
       (alist-cons-before
        'configure 'ignore-deprecations
        (lambda _
          (substitute* "activation-server/Makefile.in"
            (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS")))
        %standard-phases)))
    (inputs `(("popt" ,popt)
              ("libxml2" ,libxml2)))
    ;; The following are Required by the .pc file
    (propagated-inputs
     `(("glib" ,glib)
       ("orbit2" ,orbit2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("flex" ,flex)
       ("bison" ,bison)))
    (home-page "https://developer.gnome.org/libbonobo/")
    (synopsis "Framework for creating reusable components for use in GNOME applications")
    (description "Bonobo is a framework for creating reusable components for
use in GNOME applications, built on top of CORBA.") 
    ;; Licence not explicitly stated.  Source files contain no licence notices.
    ;; Tarball contains text of both GPLv2 and LGPLv2
    ;; GPLv2 covers both conditions
    (license license:gpl2+)))


(define-public gconf
  (package
    (name "gconf")
    (version "3.2.6")
    (source (origin
              (method url-fetch)
	      (uri 
	       (let ((upstream-name "GConf"))
		 (string-append
		  "mirror://gnome/sources/" upstream-name "/" (string-take version 3)  "/" upstream-name "-"
		  version
		  ".tar.xz")))
              (sha256
               (base32 "0k3q9nh53yhc9qxf1zaicz4sk8p3kzq4ndjdsgpaa2db0ccbj4hr"))))
    (build-system gnu-build-system)
    (inputs `(("glib" ,glib)
              ("dbus" ,dbus)
              ("dbus-glib" ,dbus-glib)
              ("libxml2" ,libxml2)))
    (propagated-inputs `(("orbit2" ,orbit2))) ; referred to in the .pc file
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/gconf/")
    (synopsis "store application preferences")
    (description  "gconf is a system for storing application preferences. It
is intended for user preferences; not arbitrary data storage.") 
    (license license:lgpl2.0+))) 


(define-public gnome-mime-data
  (package
    (name "gnome-mime-data")
    (version "2.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/" (string-take version 4)  "/" name "-"
                    version
                    ".tar.bz2"))
              (sha256
               (base32
                "1mvg8glb2a40yilmyabmb7fkbzlqd3i3d31kbkabqnq86xdnn69p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("intltool" ,intltool)))
    (home-page "http://www.gnome.org")
    (synopsis "base MIME and Application database for GNOME")
    (description  "GNOME Mime Data is a module which contains the base MIME
and Application database for GNOME.  The data stored by this module is
designed to be accessed through the MIME functions in GnomeVFS.")
    (license license:gpl2+)))


(define-public gnome-vfs
  (package
    (name "gnome-vfs")
    (version "2.24.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/" (string-take version 4)  "/" name "-"
                    version
                    ".tar.bz2"))
              (sha256
               (base32 "1ajg8jb8k3snxc7rrgczlh8daxkjidmcv3zr9w809sq4p2sn9pk2"))))
    (build-system gnu-build-system)
    (arguments
     ;; The programmer kindly gives us a hook to turn off deprecation warnings ...
     `(#:configure-flags '("DISABLE_DEPRECATED_CFLAGS=-DGLIB_DISABLE_DEPRECATION_WARNINGS")
                         ;; ... which they then completly ignore !!
                         #:phases
                         (alist-cons-before
                          'configure 'ignore-deprecations
                          (lambda _
                            (begin
                              (substitute* "libgnomevfs/Makefile.in"
                                (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))
                              (substitute* "daemon/Makefile.in"
                                (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))))
                          %standard-phases)))
    (inputs `(("glib" ,glib)
              ("libxml2" ,libxml2)
              ("dbus-glib" ,dbus-glib)
              ("dbus" ,dbus)
              ("gconf" ,gconf)
              ("gnome-mime-data" ,gnome-mime-data)
              ("zlib" ,zlib)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-mkenums, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/gnome-vfs/")
    (synopsis "access files and folders in GNOME applications")
    (description  "GnomeVFS is the core library used to access files and
folders in GNOME applications. It provides a file system abstraction which
allows applications to access local and remote files with a single consistent API.")
    (license license:lgpl2.0+)))



(define-public libgnome
  (package
    (name "libgnome")
    (version "2.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (string-take version 4)  "/" name "-"
                    version
                    ".tar.bz2"))
              (sha256
               (base32
                "197pnq8y0knqjhm2fg4j6hbqqm3qfzfnd0irhwxpk1b4hqb3kimj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-cons-before
        'configure 'enable-deprecated
        (lambda _ 
          (substitute* "libgnome/Makefile.in"
            (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS")))
        %standard-phases)))
    (inputs `(("popt" ,popt)
              ("libxml2" ,libxml2)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-mkenums, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    ;; The following are listed as Required in the .pc file
    ;; (except for libcanberra -- which seems to be oversight on the part
    ;; of the upstream developers -- anything that links against libgnome,
    ;; must also link against libcanberra
    (propagated-inputs
     `(("libcanberra" ,libcanberra)
       ("libbonobo" ,libbonobo)
       ("gconf" ,gconf)
       ("gnome-vfs" ,gnome-vfs)
       ("glib" ,glib)))
    (home-page "https://developer.gnome.org/libgnome/")
    (synopsis "Useful routines for building applications")
    (description  "The libgnome library provides a number of useful routines
for building modern applications, including session management, activation of
files and URIs, and displaying help.")
    (license license:lgpl2.0+)))


(define-public libart-lgpl
  (package
    (name "libart-lgpl")
    (version "2.3.9")
    (source (origin
              (method url-fetch)
              (uri (let ((upstream-name "libart_lgpl"))
                     (string-append
                      "mirror://gnome/sources/" upstream-name "/" 
                      (string-take version 3) "/" upstream-name "-" version
                      ".tar.bz2")))
              (sha256
               (base32
                "072r4svs4hjf2f4gxzx02n3f970kdv9fpx54r2m8bd42fjyyawrw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://people.gnome.org/~mathieu/libart")
    (synopsis "2D drawing library")
    (description  "Libart is a 2D drawing library intended as a 
high-quality vector-based 2D library with antialiasing and alpha composition.")
    (license license:lgpl2.0+)))



(define-public libgnomecanvas
  (package
    (name "libgnomecanvas")
    (version "2.30.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/" (string-take version 4)  "/" name "-"
                    version
                    ".tar.gz"))
              (sha256
               (base32
                "1nhnq4lfkk8ljkdafscwaggx0h95mq0rxnd7zgqyq0xb6kkqbjm8"))))
    (build-system gnu-build-system)
    ;; Mentioned as Required in the .pc file
    (propagated-inputs `(("libart-lgpl" ,libart-lgpl)
                         ("gtk+" ,gtk+-2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libgnomecanvas/")
    (synopsis "Flexible widget for creating interactive structured graphics")
    (description  "The GnomeCanvas widget provides a flexible widget for
creating interactive structured graphics.")
    (license license:lgpl2.0+)))

(define-public libgnomeui
  (package
    (name "libgnomeui")
    (version "2.24.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/" (string-take version 4)  "/" name "-"
                    version
                    ".tar.bz2"))
              (sha256
               (base32
                "03rwbli76crkjl6gp422wrc9lqpl174k56cp9i96b7l8jlj2yddf"))))
    (build-system gnu-build-system)
    ;; Mentioned as Required in the .pc file
    (propagated-inputs `(("libgnome" ,libgnome)
                         ("libgnome-keyring" ,libgnome-keyring)))
    (inputs `(("libgnomecanvas" ,libgnomecanvas)
              ("libbonoboui" ,libbonoboui)
              ("libjpeg" ,libjpeg)
              ("popt" ,popt)
              ("libbonobo" ,libbonobo)
              ("libxml2" ,libxml2)
              ("libglade" ,libglade)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-mkenums, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libgnomeui/")
    (synopsis "Additional widgets for applications")
    (description  "The libgnomeui library provides additional widgets for
applications. Many of the widgets from libgnomeui have already been ported to GTK+.")
    (license license:lgpl2.0+)))

(define-public libglade
  (package
    (name "libglade")
    (version "2.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/" (string-take version 3)  "/" name "-"
                    version
                    ".tar.bz2"))
              (sha256
               (base32
                "1v2x2s04jry4gpabws92i0wq2ghd47yr5n9nhgnkd7c38xv1wdk4"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk+-2" ,gtk+-2)
       ("libxml2" ,libxml2)
       ("python" ,python))) ;; needed for the optional libglade-convert program
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libglade")
    (synopsis "load glade interfaces and access the glade built widgets")
    (description  "libglade is a library that provides interfaces for loading
graphical interfaces described in glade files and for accessing the
widgets built in the loading process.")
    (license license:gpl2+))) ; This is correct.  GPL not LGPL

(define-public libgnomeprint
  (package
    (name "libgnomeprint")
    (version "2.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/" (string-take version 3)  "/" name "-"
                    version
                    ".tar.bz2"))
              (sha256
               (base32
                "129ka3nn8gx9dlfry17ib79azxk45wzfv5rgqzw6dwx2b5ns8phm"))))
    (build-system gnu-build-system)
    (inputs
     `(("popt" ,popt)
       ("libart-lgpl" ,libart-lgpl)
       ("gtk+" ,gtk+-2)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/gnome-print/home/faq.html")
    (synopsis "printing framework for GNOME")
    (description  "Gnome-print is a high-quality printing framework for GNOME.")
    (license license:lgpl2.0+)))


(define-public libgnomeprintui
  (package
    (name "libgnomeprintui")
    (version "2.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/" (string-take version 3)  "/" name "-"
                    version
                    ".tar.bz2"))
              (sha256
               (base32
                "1ivipk7r61rg90p9kp889j28xlyyj6466ypvwa4jvnrcllnaajsw"))))
    (build-system gnu-build-system)
    ;; Mentioned as Required in the .pc file
    (propagated-inputs `(("libgnomeprint" ,libgnomeprint)))
    (inputs `(("gtk+" ,gtk+-2)
              ("glib" ,glib)
              ("gnome-icon-theme" ,gnome-icon-theme)
              ("libgnomecanvas" ,libgnomecanvas)
              ("libxml2" ,libxml2))) 
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/gnome-print/home/faq.html")
    (synopsis "Printing framework for GNOME")
    (description  "Gnome-print is a high-quality printing framework for GNOME.")
    (license license:lgpl2.0+)))


(define-public libbonoboui
  (package
    (name "libbonoboui")
    (version "2.24.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (string-join (take (string-split version #\.) 2) ".")
                    "/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1kbgqh7bw0fdx4f1a1aqwpff7gp5mwhbaz60c6c98bc4djng5dgs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-cons-before
        'check 'start-xserver
        (lambda* (#:key inputs #:allow-other-keys)
          (let ((xorg-server (assoc-ref inputs "xorg-server"))
                (disp ":1"))
            
            (setenv "HOME" (getcwd))
            (setenv "DISPLAY" disp)
            ;; There must be a running X server and make check doesn't start one.
            ;; Therefore we must do it.
            (zero? (system (format #f "~a/bin/Xvfb ~a &" xorg-server disp)))))
        %standard-phases)))
    ;; Mentioned as Required by the .pc file
    (propagated-inputs `(("libxml2" ,libxml2)))
    (inputs
     `(("popt" ,popt)
       ("pangox-compat" ,pangox-compat)
       ("libgnome" ,libgnome)
       ("libgnomecanvas" ,libgnomecanvas)
       ("libglade" ,libglade)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("intltool" ,intltool)
       ("xorg-server" ,xorg-server) ; For running the tests
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libbonoboui/")
    (synopsis "Some user interface controls using Bonobo")
    (description  "The Bonobo UI library provides a number of user interface
controls using the Bonobo component framework.")
    (license license:lgpl2.0+)))


(define-public goffice
  (package
    (name "goffice")
    (version "0.10.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/" 
                    (string-take version 4) "/" name "-"
                    version    ".tar.xz"))
              (sha256
               (base32 "0kj0iwng6w4axm7yv2zy7myn5dhw5ilrlq2pzrjlm9i852ikqy60"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk+" ,gtk+)
       ("libgsf" ,libgsf)
       ("librsvg" ,librsvg)
       ("libxslt" ,libxslt)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/goffice/")
    (synopsis "Document-centric objects and utilities")
    (description "A GLib/GTK+ set of document-centric objects and utilities.")
    (license 
     ;; Dual licensed under GPLv2 or GPLv3 (both without "or later")
     ;; Note: NOT LGPL
     (list license:gpl2 license:gpl3))))

(define-public gnumeric
  (package
    (name "gnumeric")
    (version "1.12.17")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/" 
                    (string-take version 4) "/" name "-" version
                    ".tar.xz"))
              (sha256
               (base32
                "18bvc3phghr4p5440fp8hm6gvp53d3mqs9cyc637zpmk0b6bcp7c"))))
    (build-system gnu-build-system)
    (arguments
     `(;; The gnumeric developers don't worry much about failing tests.
       ;; See https://bugzilla.gnome.org/show_bug.cgi?id=732387
       #:tests? #f 
       #:phases
       (alist-cons-before
        'configure 'pre-conf
        (lambda* (#:key outputs #:allow-other-keys)
          ;; Make install tries to write into the directory of goffice
          ;; I am informed that this only affects the possibility to embed a
          ;; spreadsheet inside an Abiword document.   So presumably when we
          ;; package Abiword we'll have to refer it to this directory.
          (substitute* "configure" 
            (("^GOFFICE_PLUGINS_DIR=.*")
             (string-append "GOFFICE_PLUGINS_DIR=" 
                            (assoc-ref outputs "out") "/goffice/plugins"))))
        %standard-phases)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("goffice" ,goffice)
       ("libgsf" ,libgsf)
       ("libxml2" ,libxml2)
       ("zlib" ,zlib)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.gnumeric.org")
    (synopsis "Spreadsheet application")
    (description
     "GNUmeric is a GNU spreadsheet application, running under GNOME.  It is
interoperable with other spreadsheet applications.  It has a vast array of
features beyond typical spreadsheet functionality, such as support for linear
and non-linear solvers, statistical analysis, and telecommunication
engineering.")
    (license
    ;; Dual licensed under GPLv2 or GPLv3 (both without "or later")
     (list license:gpl2 license:gpl3))))
