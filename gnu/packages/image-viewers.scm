;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 nee <nee-git@hidamari.blue>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Guy Fleury Iteriteka <hoonandon@gmail.com>
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Peng Mei Yu <pengmeiyu@riseup.net>
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
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
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
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages))

(define-public feh
  (package
    (name "feh")
    (version "3.3")
    (home-page "https://feh.finalrewind.org/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "04c8cgwzkax481sz7lbzy23mk79bqmjy3qpvr7vxa4c14mc9k5gk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases (delete 'configure))
       #:test-target "test"
       #:make-flags
       (list "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "exif=1")))
    (native-inputs
     `(("perl" ,perl)
       ("perl-test-command" ,perl-test-command)))
    (inputs `(("imlib2" ,imlib2)
              ("curl" ,curl)
              ("libexif" ,libexif)
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
  ;; The latest release, 1.4, fails to build with Exiv2 0.27.1.  The upstream
  ;; repo has several fixes for that, so take a snapshot.
  (let ((commit "c220ddefb1b6b11b54f7598f0d44dd0723325ed4")
        (revision "1"))
    (package
      (name "geeqie")
      (version (git-version "1.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/BestImageViewer/geeqie")
                      (commit commit)))
                (sha256
                 (base32
                  "07424nzrnwbksgalgg1a6ig2snd986w79kca1cfnv1q9kc7x2h3x"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `( ;; Enable support for a "map" pane using GPS data.
         #:configure-flags '("--enable-map")

         ;; Parallel builds fail with something like:
         ;;   image-load.c:143:9: error: ‘gq_marshal_VOID__INT_INT_INT_INT’ undeclared
         ;; due to unexpressed makefile dependencies.
         #:parallel-build? #f

         #:phases
         (modify-phases %standard-phases
           (add-before 'bootstrap 'pre-bootstrap
             (lambda _
               (define (write-dummy-changelog port)
                 (display "See Git history for a change log.\n" port))
               ;; Create ChangeLog{,.html} to placate the makefile, which would
               ;; otherwise require access to the Git repo.
               (call-with-output-file "ChangeLog"
                 write-dummy-changelog)
               (call-with-output-file "ChangeLog.html"
                 write-dummy-changelog)

               ;; Don't try to run 'git' for the version number.
               (substitute* "configure.ac"
                 (("m4_esyscmd_s\\([^)]+\\)")
                  (string-append "[" ,version "]")))

               ;; Remove references to non-existent files.
               (substitute* "po/POTFILES.in"
                 (("^plugins/import/.*") ""))
               #t)))))
      (inputs
       `(("clutter" ,clutter)
         ("libchamplain" ,libchamplain)
         ("lcms" ,lcms)
         ("exiv2" ,exiv2)
         ("libpng" ,libpng)
         ("gtk+" ,gtk+)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("glib" ,glib "bin")                     ; glib-gettextize
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
      (license license:gpl2+))))

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
              ("libjpeg" ,libjpeg-turbo)))
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
    (version "25")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/muennich/sxiv.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13s1lfar142hq1j7xld0ri616p4bqs57b17yr4d0b9a9w7liz4hp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags
       (list (string-append "PREFIX=" %output)
             "CC=gcc"
             ;; Xft.h #includes <ft2build.h> (without ‘freetype2/’).  The sxiv
             ;; Makefile works around this by hard-coding /usr/include instead.
             (string-append "CPPFLAGS=-I"
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
    (version "1.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hellosiyan/Viewnior.git")
               (commit (string-append name "-" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0y4hk3vq8psba5k615w18qj0kbdfp5w0lm98nv5apy6hmcpwfyig"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;; Don't create 'icon-theme.cache'
             (substitute* "meson.build"
               (("meson.add_install_script*") ""))
             #t)))
       #:tests? #f)) ; no tests
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib "bin") ; glib-genmarshal
       ("pkg-config" ,pkg-config)
       ("shared-mime-info" ,shared-mime-info)))
    (inputs
     `(("exiv2" ,exiv2)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gtk+-2" ,gtk+-2)))
    (home-page "http://siyanpanayotov.com/project/viewnior")
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
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/posva/catimg.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g9ywbgy162wiam9hc3yqpq5q4gyxa8fj4jskr3fdz8z8jjaabzz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-convert
           (lambda _
             (substitute* "catimg"
               ;; By replacing "convert", we also replace the "convert"
               ;; in the message 'The version of convert is too old, don't
               ;; expect good results :('.  This should not happen, but in
               ;; practice this error message should not affect us.
               (("convert") (which "convert")))
             #t))
         (add-after 'install 'install-script
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The bash script lacks an file extension.  We have to rename
             ;; it so that the C program and the bash script can be happy
             ;; side by side.
             (copy-file "../source/catimg"
                        (string-append (assoc-ref outputs "out")
                                       "/bin/catimg.sh"))
             #t)))))
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
       ("libjpeg" ,libjpeg-turbo)
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

(define-public qview
  (package
    (name "qview")
    (version "3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jurplel/qView.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15a91bs3wcqhgf76wzigbn10hayg628j84pq4j2vaxar94ak0vk7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (invoke "qmake")))
         ;; Installation process hard-codes "/usr/bin", possibly
         ;; prefixed.
         (add-after 'configure 'fix-install-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("\\$\\(INSTALL_ROOT\\)/usr") out))
               #t)))
         ;; Don't phone home or show "Checking for updates..." in the
         ;; About menu.
         (add-before 'build 'disable-auto-update
           (lambda _
             (substitute* "src/qvaboutdialog.cpp"
               (("ui->updateLabel->setText\\(updateText\\);") "")
               (("requestUpdates\\(\\);") ""))
             #t)))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtimageformats" ,qtimageformats)))
    (home-page "https://interversehq.com/qview/")
    (synopsis "Convenient and minimal image viewer")
    (description "qView is a Qt image viewer designed with visually
minimalism and usability in mind.  Its features include animated GIF
controls, file history, rotation/mirroring, and multithreaded
preloading.")
    (license license:gpl3+)))

(define-public chafa
  (package
    (name "chafa")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hpjansson.org/chafa/releases/chafa-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1hj4vdyczby8h52ff23qxl8ng18p5jy549idngpiddwszf5s4drz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("imagemagick" ,imagemagick)))
    (synopsis "Convert images to ANSI/Unicode characters")
    (description
     "Chafa is a command-line utility that converts all kinds of images,
including animated GIFs, into ANSI/Unicode character output that can be
displayed in a terminal.")
    (home-page "https://hpjansson.org/chafa/")
    (license license:lgpl3+)))

(define-public imv
  (package
    (name "imv")
    (version "4.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/eXeC64/imv")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0gk8g178i961nn3bls75a8qpv6wvfvav6hd9lxca1skaikd33zdx"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'record-absolute-file-names
           (lambda* (#:key outputs #:allow-other-keys)
             ;; 'imv' is a script that execs 'imv-x11' or 'imv-wayland'.
             ;; Record their absolute file name.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (substitute* (string-append bin "/imv")
                 (("imv-")
                  (string-append bin "/imv-")))
               #t))))
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "CONFIGPREFIX="
                            (assoc-ref %outputs "out") "/etc"))))
    (inputs
     `(("asciidoc" ,asciidoc)
       ("freeimage" ,freeimage)
       ("glu" ,glu)
       ("librsvg" ,librsvg)
       ("libxkbcommon" ,libxkbcommon)
       ("pango" ,pango)
       ("wayland" ,wayland)))
    (native-inputs
     `(("cmocka" ,cmocka)
       ("pkg-config" ,pkg-config)))
    (synopsis "Image viewer for tiling window managers")
    (description "@code{imv} is a command line image viewer intended for use
with tiling window managers.  Features include:

@itemize
@item Native Wayland and X11 support.
@item Support for dozens of image formats including:
@itemize
@item PNG
@item JPEG
@item Animated GIFs
@item SVG
@item TIFF
@item Various RAW formats
@item Photoshop PSD files
@end itemize
@item Configurable key bindings and behavior.
@item Highly scriptable with IPC via imv-msg.
@end itemize\n")
    (home-page "https://github.com/eXeC64/imv")
    (license license:expat)))
