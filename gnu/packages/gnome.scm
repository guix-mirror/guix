;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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
  #:use-module ((guix licenses) #:select (gpl2 gpl2+ lgpl2.0+ lgpl2.1+ lgpl3))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xorg))

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
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/brasero/")
    (synopsis "CD/DVD burning tool for Gnome")
    (description "Brasero is an application to burn CD/DVD for the Gnome
Desktop.  It is designed to be as simple as possible and has some unique
features to enable users to create their discs easily and quickly.")
    (license gpl2+)))

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
    (license gpl2+)))

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
    (native-inputs
     `(("intltool" ,intltool)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (arguments
     `(#:tests? #f)) ; tries to load http://www.oasis-open.org/docbook/xml/4.4/docbookx.dtd
    (home-page "https://wiki.gnome.org/GnomeDocUtils")
    (synopsis
     "Documentation utilities for the Gnome project")
    (description
     "Gnome-doc-utils is a collection of documentation utilities for the
Gnome project.  It includes xml2po tool which makes it easier to translate
and keep up to date translations of documentation.")
    (license gpl2+))) ; xslt under lgpl

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
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     `(("glib" ,glib)))
    (home-page "http://www.gnome.org")
    (synopsis "Accessing passwords from the GNOME keyring")
    (description
     "Client library to access passwords from the GNOME keyring.")

    ;; Though a couple of files are LGPLv2.1+.
    (license lgpl2.0+)))

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
       ("pkg-config" ,pkg-config)))
    (home-page
     "http://www.gnome.org/projects/evince/")
    (synopsis "GNOME's document viewer")
    (description
     "Evince is a document viewer for multiple document formats.  It
currently supports PDF, PostScript, DjVu, TIFF and DVI.  The goal
of Evince is to replace the multiple document viewers that exist
on the GNOME Desktop with a single simple application.")
    (license gpl2+)))

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
       ("pkg-config" ,pkg-config)))
    (home-page "https://launchpad.net/gsettings-desktop-schemas")
    (synopsis
     "GNOME settings for various desktop components")
    (description
     "Gsettings-desktop-schemas contains a collection of GSettings schemas
for settings shared by various components of the GNOME desktop.")
    (license lgpl2.1+)))

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
    (license lgpl2.1+)))

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
    (license lgpl3))) ; or Creative Commons BY-SA 3.0

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
    (license gpl2+)))

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
    (license gpl2)))

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
      `(("pkg-config" ,pkg-config)))
    (home-page "https://developer-next.gnome.org/libnotify/")
    (synopsis
     "GNOME desktop notification library")
    (description
     "Libnotify is a library that sends desktop notifications to a
notification daemon, as defined in the Desktop Notifications spec. These
notifications can be used to inform the user about an event or display
some form of information without getting in the user's way.")
    (license lgpl2.1+)))

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
    (arguments
     `(#:modules ((guix build gnome)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:imported-modules ((guix build gnome)
                           (guix build gnu-build-system)
                           (guix build utils))
       #:phases
        (alist-replace
         'configure
         (lambda* (#:key inputs #:allow-other-keys #:rest args)
          (let ((configure (assoc-ref %standard-phases 'configure)))
           (substitute* "libpeas-gtk/Makefile.in"
            (("--add-include-path")
             (string-append
               " --add-include-path=" (gir-directory inputs "atk")
               " --add-include-path=" (gir-directory inputs "gdk-pixbuf")
               " --add-include-path=" (gir-directory inputs "gtk+")
               " --add-include-path=" (gir-directory inputs "pango")
               " --add-include-path")))
           (substitute* "libpeas-gtk/Makefile.in"
            (("--includedir=\\$\\(top_builddir")
             (string-append
              " --includedir=" (gir-directory inputs "atk")
              " --includedir=" (gir-directory inputs "gdk-pixbuf")
              " --includedir=" (gir-directory inputs "gtk+")
              " --includedir=" (gir-directory inputs "pango")
              " --includedir=$(top_builddir")))
           (apply configure args)))
         %standard-phases)))
    (inputs
     `(("atk" ,atk)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("intltool" ,intltool)
       ("pango" ,pango)
       ("pkg-config" ,pkg-config)))
    (home-page "https://wiki.gnome.org/Libpeas")
    (synopsis "GObject plugin system")
    (description
     "libpeas is a gobject-based plugins engine, and is targetted at giving
every application the chance to assume its own extensibility.  It also has a
set of features including, but not limited to: multiple extension points; on
demand (lazy) programming language support for C, Python and JS; simplicity of
the API")

    (license lgpl2.0+)))

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
                        (search-patch "gtkglext-remove-pangox-dependency.patch")
                        (search-patch "gtkglext-disable-disable-deprecated.patch")))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("libx11" ,libx11)
              ("libxt" ,libxt)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/gtkglext")
    (synopsis "OpenGL extension to GTK+.")
    (description "GtkGLExt is an OpenGL extension to GTK+. It provides
additional GDK objects which support OpenGL rendering in GTK+ and GtkWidget
API add-ons to make GTK+ widgets OpenGL-capable.")
    (license lgpl2.1+)))

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
    (license lgpl2.0+)))
