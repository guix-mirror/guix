;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages image-viewers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages geeqie)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config))

(define-public viewnior
  (package
    (name "viewnior")
    (version "1.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/xsisqox/Viewnior/archive/"
                            name "-" version ".tar.gz"))
        (sha256
         (base32
          "18309qjgwak3kn228z3p3nx7yxasqgzx69v3rgc23hf161nky0c9"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _
             (zero? (system* "sh" "autogen.sh")))))))
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)
       ("intltool" ,intltool)
       ("glib" ,glib "bin") ; glib-genmarshal
       ("gnome-common" ,gnome-common)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("shared-mime-info" ,shared-mime-info)
       ("which" ,which)))
    (inputs
     `(("exiv2" ,exiv2)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gtk+-2" ,gtk+-2)))
    (home-page "http://siyanpanayotov.com/project/viewnior/")
    (synopsis "Simple, fast and elegant image viewer")
    (description "Viewnior is an image viewer program.  Created to be simple,
fast and elegant.  Its minimalistic interface provides more screenspace for
your images.  Among its features are:
@enumerate
@item Fullscreen & Slideshow
@item Rotate, flip, crop, save, delete images
@item Animation support
@item Browse only selected images
@item Navigation window
@item Set image as wallpaper (Gnome 2, Gnome 3, XFCE, LXDE, FluxBox, Nitrogen)
@item Simple interface
@item EXIF and IPTC metadata
@item Configurable mouse actions
@end enumerate\n")
    (license license:gpl3+)))
