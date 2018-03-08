;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 ng0 <contact.ng0@cryptolab.net>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 nee <nee-git@hidamari.blue>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages))

(define-public feh
  (package
    (name "feh")
    (version "2.25.1")
    (home-page "https://feh.finalrewind.org/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "197sm78bm33dvahr5nxqkbmpmdn4b13ahc9mrgn1l7n104bg4phc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases (delete 'configure))
       #:test-target "test"
       #:make-flags
       (list "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (native-inputs
     `(("perl" ,perl)
       ("perl-test-command" ,perl-test-command)))
    (inputs `(("imlib2" ,imlib2)
              ("curl" ,curl)
              ("libpng" ,libpng)
              ("libxt" ,libxt)
              ("libx11" ,libx11)
              ("libxinerama" ,libxinerama)))
    (native-search-paths
     ;; Feh allows overriding the libcurl builtin CA path (unset in Guix)
     ;; with the same variable as the `curl` command line HTTP tool.
     (package-native-search-paths curl))
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
     `(;; Enable support for a "map" pane using GPS data.
       #:configure-flags '("--enable-map")

       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _
             (setenv "NOCONFIGURE" "true")
             (zero? (system* "sh" "autogen.sh")))))))
    (inputs
     `(("clutter" ,clutter)
       ("libchamplain" ,libchamplain)
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

(define-public gpicview
  (package
    (name "gpicview")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/lxde/"
                                  "GPicView%20%28image%20Viewer%29/0.2.x/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0hi9v0rdx47nys0wvm9xasdrafa34r5kq6crb074a0ipwmc60iiq"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)
              ("libjpeg" ,libjpeg)))
    (native-inputs `(("intltool"   ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "Simple and fast image viewer for X")
    (description "gpicview is a lightweight GTK+ 2.x based image viewer.
It is the default image viewer on LXDE desktop environment.")
    (home-page "http://lxde.sourceforge.net/gpicview/")
    (license license:gpl2+)))

(define-public sxiv
  (package
    (name "sxiv")
    (version "24")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/muennich/sxiv/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "044i077li6m4zsz2fswlcdi2m0sbr9mwws1h3k1zjaln29fw87ai"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags
       (list (string-append "PREFIX=" %output)
             "CC=gcc"
             ;; Xft.h #includes <ft2build.h> (without ‘freetype2/’).  The sxiv
             ;; Makefile works around this by hard-coding /usr/include instead.
             (string-append "DEF_CPPFLAGS=-I"
                            (assoc-ref %build-inputs "freetype")
                            "/include/freetype2")
             "V=1")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (inputs
     `(("freetype" ,freetype)
       ("giflib" ,giflib)
       ("imlib2" ,imlib2)
       ("libexif" ,libexif)
       ("libx11" ,libx11)
       ("libxft" ,libxft)))
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
             (zero? (system* "sh" "autogen.sh"))))
         (add-before 'install 'skip-gtk-update-icon-cache
           (lambda _
             ;; Don't create 'icon-theme.cache'
             (substitute* (find-files "data" "^Makefile$")
               (("gtk-update-icon-cache") (which "true")))
             #t)))))
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

(define-public catimg
  (package
    (name "catimg")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/posva/catimg/archive"
                           "/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rwgbq2imd5l4nql5hrz7rr5f4gz8aad1amlf0j3cxir8slpbd1y"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((magic (assoc-ref %build-inputs "imagemagick"))
                    (convert (string-append magic "/bin/convert")))
               (substitute* "catimg"
                 ;; By replacing "convert", we also replace the "convert"
                 ;; in the message 'The version of convert is too old, don't
                 ;; expect good results :('.  This should not happen, but in
                 ;; practice this error message should not affect us.
                 (("convert") convert))
               #t)))
         (replace 'build
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (man (string-append out "/share/man/man1")))
               (zero? (system* "cmake"
                               (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                               (string-append "-DMAN_OUTPUT_PATH=" man)
                               "."))
               (zero? (system* "make")))))
         (add-before 'install 'install-script
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The bash script lacks an file extension.  We have to rename
             ;; it so that the C program and the bash script can be happy
             ;; side by side.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "catimg" bin)
               (rename-file (string-append bin "/catimg")
                            (string-append bin "/catimg.sh"))
               #t))))))
    (inputs
     `(("imagemagick" ,imagemagick))) ; for the bash script version
    (home-page "https://github.com/posva/catimg")
    (synopsis "Render images in the terminal")
    (description
     "Catimg is a little program that prints images in the terminal.
It supports JPEG, PNG and GIF formats.")
    (license license:expat)))

(define-public luminance-hdr
  (package
    (name "luminance-hdr")
    (version "2.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/qtpfsgui/luminance/"
                    version "/luminance-hdr-" version ".tar.bz2"))
              (sha256
               (base32
                "00fldbcizrx8jcnjgq74n3zmbm27dxzl96fxa7q49689mfnlw08l"))
              (patches (search-patches "luminance-hdr-qt-printer.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebkit" ,qtwebkit)
       ("boost" ,boost)
       ;; ("gtest" ,gtest)
       ("libraw" ,libraw)
       ("zlib" ,zlib)
       ("exiv2" ,exiv2)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg)
       ("lcms" ,lcms)
       ("openexr" ,openexr)
       ("fftw" ,fftwf)
       ("gsl" ,gsl)
       ("libtiff" ,libtiff)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'add-ilmbase-include-path
           (lambda* (#:key inputs #:allow-other-keys)
             ;; 'OpenEXR.pc' has a -I for IlmBase but 'FindOpenEXR.cmake' does
             ;; not use 'OpenEXR.pc'.  Thus, we need to add
             ;; "$ilmbase/include/OpenEXR/" to the CPATH.
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "ilmbase")
                                    "/include/OpenEXR"
                                    ":" (or (getenv "CPATH") "")))
             #t)))))
    (home-page "http://qtpfsgui.sourceforge.net")
    (synopsis "High dynamic range (HDR) imaging application")
    (description
     "Luminance HDR (formerly QtPFSGui) is a graphical user interface
application that aims to provide a workflow for high dynamic range (HDR)
imaging.  It supports several HDR and LDR image formats, and it can:

@itemize
@item Create an HDR file from a set of images (formats: JPEG, TIFF 8bit and
16bit, RAW) of the same scene taken at different exposure setting;
@item Save load HDR images;
@item Rotate, resize and crop HDR images;
@item Tone-map HDR images;
@item Copy EXIF data between sets of images.
@end itemize\n")
    (license license:gpl2+)))

;; CBR and RAR are currently unsupported, due to non-free dependencies.
;; For optional PDF support, you can install the mupdf package.
(define-public mcomix
  (package
    (name "mcomix")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/mcomix/MComix-" version
                           "/mcomix-" version ".tar.bz2"))
       (sha256
        (base32
         "0fzsf9pklhfs1rzwzj64c0v30b74nk94p93h371rpg45qnfiahvy"))))
    (build-system python-build-system)
    (inputs
     `(("p7zip" ,p7zip)
       ("python2-pillow" ,python2-pillow)
       ("python2-pygtk" ,python2-pygtk)))
    (arguments
     ;; Python 2.5 or newer (Python 3 and up is not supported)
     `(#:python ,python-2
       #:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((p7zip (assoc-ref inputs "p7zip")))
               ;; insert absolute path to 7z executable
               (substitute* "mcomix/archive/sevenzip_external.py"
                 (("_7z_executable = -1")
                  (string-append "_7z_executable = u'" p7zip "/bin/7z'"))))
             #t)))))
    (home-page "https://sourceforge.net/p/mcomix/wiki/Home/")
    (synopsis "Image viewer for comics")
    (description "MComix is a customizable image viewer that specializes as
a comic and manga reader.  It supports a variety of container formats
including CBZ, CB7, CBT, LHA.")
    (license license:gpl2+)))
