;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
;;; Copyright © 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu packages abiword)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ots)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages wv)
  #:use-module (gnu packages xml))

(define-public abiword
  (package
    (name "abiword")
    (version "3.0.4")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "https://www.abisource.com/downloads/abiword/" version
                         "/source/abiword-" version ".tar.gz"))
        (sha256
         (base32 "1mx5l716n0z5788i19qmad30cck4v9ggr071cafw2nrf375rcc79"))
        (patches
         (search-patches "abiword-explictly-cast-bools.patch"))))

    (build-system glib-or-gtk-build-system)
    (arguments                   ;; NOTE: rsvg is disabled, since Abiword
      `(#:configure-flags        ;; supports it directly, and its BS is broken.
        (list                    ;; wmf was removed from Guix for security.
          "--enable-clipart"     ;; TODO: The following plugins have unresolved
          "--enable-templates"   ;; dependencies: aiksaurus, grammar, wpg, gda,
          (string-append         ;; wordperfect, psion, mathview.
            "--enable-plugins="
              "applix " "babelfish " "bmp " "clarisworks " "collab " "command "
              "docbook " "eml " "epub " "freetranslation " "garble " "gdict "
              "gimp " "goffice " "google " "hancom " "hrtext " "iscii " "kword "
              "latex " "loadbindings " "mht " "mif " "mswrite " "opendocument "
              "openwriter " "openxml " "opml " "ots " "paint " "passepartout "
              "pdb " "pdf " "presentation " "s5 " "sdw " "t602 " "urldict "
              "wikipedia " "wml " "xslfo"))
        ;; tests fail with: Gtk-CRITICAL **: gtk_settings_get_for_screen:
        ;;                  assertion 'GDK_IS_SCREEN (screen)' failed
        ;;                  GLib-GObject-CRITICAL **: g_object_get_qdata:
        ;;                  assertion 'G_IS_OBJECT (object)' failed
        ;; Manually starting the X server before the test phase did not help
        ;; the tests to pass.
        #:tests? #f
        #:make-flags
        (list "gtk_update_icon_cache=true")))
    (inputs
      `(("boost" ,boost)
        ("enchant" ,enchant)
        ("fontconfig" ,fontconfig)
        ("fribidi" ,fribidi)
        ("glib" ,glib)
        ("goffice" ,goffice)
        ("gtk+" ,gtk+)
        ("libchamplain" ,libchamplain)
        ("libglade" ,libglade)
        ("libgsf" ,libgsf)
        ("libjpeg" ,libjpeg-turbo)
        ("libpng" ,libpng)
        ("librsvg" ,librsvg)
        ("libxml2" ,libxml2)
        ("libxslt" ,libxslt)
        ("ots" ,ots)
        ("popt" ,popt)
        ("readline" ,readline)
        ("telepathy" ,telepathy-glib)
        ("wv" ,wv)
        ("zlib" ,zlib)))
    (native-inputs
      `(("intltool" ,intltool)
        ("glib:bin" ,glib "bin")
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
    (home-page "https://www.abisource.com/")
    (synopsis "Word processing program")

    ;; HACKERS: The comment below is here so that it shows up early in the
    ;; .pot file.

    ;; TRANSLATORS: Dear translator, We would like to inform you that package
    ;; descriptions may occasionally include Texinfo markup.  Texinfo markup
    ;; looks like "@code{rm -rf}", "@emph{important}", etc.  When translating,
    ;; please leave markup as is.
    (description "AbiWord is a word processing program.  It is rapidly
becoming a state of the art word processor, with lots of features useful for
your daily work, personal needs, or for just some good old typing fun.")
    (license license:gpl2+)))
