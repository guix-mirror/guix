;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
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

(define-module (gnu packages image-viewers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))

(define-public feh
  (package
    (name "feh")
    (version "2.18")
    (home-page "https://feh.finalrewind.org/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "036lgv81d0vmrnmh1z1x360nr5avb2sk5jb1h3g5n6jijm8vzblx"))))
    (build-system gnu-build-system)
    (arguments
      '(#:phases (alist-delete 'configure %standard-phases)
        #:tests? #f
        #:make-flags
          (list "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (inputs `(("imlib2" ,imlib2)
              ("curl" ,curl)
              ("libpng" ,libpng)
              ("libxt" ,libxt)
              ("libx11" ,libx11)
              ("libxinerama" ,libxinerama)))
    (synopsis "Fast and light imlib2-based image viewer")
    (description
      "feh is an X11 image viewer aimed mostly at console users.
Unlike most other viewers, it does not have a fancy GUI, but simply
displays images.  It can also be used to set the desktop wallpaper.
It is controlled via commandline arguments and configurable key/mouse
actions.")

    ;; The license is really the Expat license, with additional wording in the
    ;; 2nd paragraph: "acknowledgment shall be given in the documentation and
    ;; software packages that this Software was used."
    (license (license:x11-style
              "file://COPYING"
              "See 'COPYING' in the distribution."))))

(define-public geeqie
  (package
    (name "geeqie")
    (version "1.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/BestImageViewer/geeqie/"
                                 "releases/download/v" version "/geeqie-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0gzc82sy66pbsmq7lnmq4y37zqad1zfwfls3ik3dmfm8s5nmcvsb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _
             (setenv "NOCONFIGURE" "true")
             (zero? (system* "sh" "autogen.sh")))))))
    (inputs
     `(;; ("libchamplain" ,libchamplain)
       ("lcms" ,lcms)
       ("exiv2" ,exiv2)
       ("libpng" ,libpng)
       ("gtk+" ,gtk+-2)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("glib" ,glib "bin") ; glib-gettextize
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.geeqie.org/")
    (synopsis "Lightweight GTK+ based image viewer")
    (description
     "Geeqie is a lightweight GTK+ based image viewer for Unix like operating
systems.  It features: EXIF, IPTC and XMP metadata browsing and editing
interoperability; easy integration with other software; geeqie works on files
and directories, there is no need to import images; fast preview for many raw
image formats; tools for image comparison, sorting and managing photo
collection.  Geeqie was initially based on GQview.")
    (license license:gpl2+)))

(define-public sxiv
  (package
    (name "sxiv")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/muennich/sxiv/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lxnd33gaw4drhdwbkk94wzrjyhh64d57jq2ps7ffmqgizg6hlwz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:make-flags (list (string-append "PREFIX=" %output)
                          "CC=gcc")
       #:phases (alist-delete
                 'configure             ; no configure phase
                 %standard-phases)))
    (inputs
     `(("libx11" ,libx11)
       ("imlib2" ,imlib2)
       ("giflib" ,giflib)
       ("libexif" ,libexif)))
    (home-page "https://github.com/muennich/sxiv")
    (synopsis "Simple X Image Viewer")
    (description
     "sxiv is an alternative to feh and qiv.  Its primary goal is to
provide the most basic features required for fast image viewing.  It has
vi key bindings and works nicely with tiling window managers.  Its code
base should be kept small and clean to make it easy for you to dig into
it and customize it for your needs.")
    (license license:gpl2+)))

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
