;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
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
  #:use-module (gnu packages)
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
    (version "2.8.6")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "http://abisource.org/downloads/" name "/" version
                         "/source/" name "-" version ".tar.gz"))
        (sha256
          (base32 "059sd2apxdmcacc4pll880i7vm18h0kyjsq299m1mz3c7ak8k46r"))
        (patches
          (list
            (search-patch "abiword-wmf-version-lookup-fix.patch")
            (search-patch "abiword-no-include-glib-internal-headers.patch")
            (search-patch "abiword-explictly-cast-bools.patch")
            (search-patch "abiword-use-proper-png-api.patch")
            (search-patch "abiword-pass-no-undefined-to-linker.patch")
            (search-patch "abiword-link-plugins-against-backend.patch")))))

    (build-system gnu-build-system)
    (arguments                   ;; NOTE: rsvg is disabled, since Abiword
      `(#:configure-flags        ;; supports it directly, and its BS is broken.
        (list
          "--enable-clipart"     ;; TODO: The following plugins have unresolved
          "--enable-templates"   ;; dependencies: aiksaurus, grammar, wpg, gda,
          (string-append         ;; wordperfect, psion, mathview, goffice.
            "--enable-plugins="
              "applix " "babelfish " "bmp " "clarisworks " "collab " "command "
              "docbook " "eml " "freetranslation " "garble " "gdict " "gimp "
              "google " "hancom " "hrtext " "iscii " "kword " "latex "
              "loadbindings " "mht " "mif " "mswrite " "opendocument "
              "openwriter " "openxml " "opml " "ots " "paint " "passepartout "
              "pdb " "pdf " "presentation " "s5 " "sdw " "t602 " "urldict "
              "wikipedia " "wmf " "wml " "xslfo"))))
    (inputs
      `(("boost" ,boost)
        ("enchant" ,enchant)
        ("fontconfig" ,fontconfig)
        ("fribidi" ,fribidi)
        ("glib" ,glib)
        ("gtk+" ,gtk+-2)
        ("libglade" ,libglade)
        ("libgsf" ,libgsf)
        ("libjpeg" ,libjpeg)
        ("libpng" ,libpng)
        ("librsvg" ,librsvg)
        ("libwmf" ,libwmf)
        ("libxml2" ,libxml2)
        ("ots" ,ots)
        ("popt" ,popt)
        ("readline" ,readline)
        ("wv" ,wv)
        ("zlib" ,zlib)))
    (native-inputs
      `(("intltool" ,intltool)
        ("glib:bin" ,glib "bin")
        ("pkg-config" ,pkg-config)))
    (home-page "http://abisource.org/")
    (synopsis "Word processing program")
    (description
      "AbiWord is a word processing program.  It is rapidly becoming a state
of the art word processor, with lots of features useful for your daily work,
personal needs, or for just some good old typing fun.")
    (license license:gpl2+)))
