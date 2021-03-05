;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 nee <nee-git@hidamari.blue>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019, 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Peng Mei Yu <pengmeiyu@riseup.net>
;;; Copyright © 2020 R Veera Kumar <vkor@vkten.in>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2021 Rovanion Luckey <rovanion.luckey@gmail.com>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
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
  #:use-module (guix utils)
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
  #:use-module (gnu packages image-processing)
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
    (version "3.6.3")
    (home-page "https://feh.finalrewind.org/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1d13x8hmvpdc5f5rj4l29ha7iz7wvqxjlvh6il04wq8igzrj0x23"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases (delete 'configure))
       #:test-target "test"
       #:make-flags
       (list "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "exif=1"
             "inotify=1")))
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
  (package
    (name "geeqie")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/BestImageViewer/geeqie")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0nf45sh3pwsv98sppcrqj81b6mdi31n1sbc7gn88m8mhpfp1qq6k"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `( ;; Enable support for a "map" pane using GPS data.
       #:configure-flags '("--enable-map"
                           "--enable-gtk3")))
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
       ("glib" ,glib "bin")                       ; glib-gettextize
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
    (version "26")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/muennich/sxiv")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xaawlfdy7b277m38mgg4423kd7p1ffn0dq4hciqs6ivbb3q9c4f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags
       (list (string-append "PREFIX=" %output)
             (string-append "CC=" ,(cc-for-target))
             ;; Xft.h #includes <ft2build.h> without ‘freetype2/’.  The Makefile
             ;; works around this by hard-coding /usr/include & $PREFIX.
             (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "freetype")
                            "/include/freetype2")
             "V=1")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'install 'install-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "sxiv.desktop"
                           (string-append (assoc-ref outputs "out")
                                          "/share/applications"))
             #t))
         (add-after 'install 'install-icons
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "-C" "icon" "install" make-flags))))))
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
               (url "https://github.com/hellosiyan/Viewnior")
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
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/posva/catimg")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a2dswbv4xddb2l2d55hc43lzvjwrjs5z9am7v6i0p0mi2fmc89s"))))
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
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/qtpfsgui/luminance/"
                    version "/luminance-hdr-" version ".tar.bz2"))
              (sha256
               (base32
                "188q0l63nfasqfvwbq4mwx2vh7wsfi2bq9n5nksddspl1qz01lnp"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsvg" ,qtsvg)
       ("qtwebkit" ,qtwebkit)
       ("boost" ,boost)
       ("eigen" ,eigen)
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
     '(#:tests? #f  ;XXX: some tests fail to compile
       #:phases
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
(define-public mcomix
  ;; Official mcomix hasn't been updated since 2016, it's broken with
  ;; python-pillow 6+ and only supports Python 2.  We use fork instead.
  (let ((commit "fea55a7a9369569eefed72209eed830409c4af98"))
    (package
      (name "mcomix")
      (version (git-version "1.2.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/multiSnow/mcomix3")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "05zl0dkjwbdcm2zlk4nz9w33amlqj8pbf32a8ymshc2356fqhhi5"))))
      (build-system python-build-system)
      (inputs
       `(("p7zip" ,p7zip)
         ("python-pillow" ,python-pillow)
         ("python-pygobject" ,python-pygobject)
         ("python-pycairo" ,python-pycairo)))
      (arguments
       `(#:tests? #f                    ; FIXME: How do we run tests?
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'configure
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((p7zip (assoc-ref inputs "p7zip")))
                 ;; insert absolute path to 7z executable
                 (substitute* "mcomix/mcomix/archive/sevenzip_external.py"
                   (("_7z_executable = -1")
                    (string-append "_7z_executable = u'" p7zip "/bin/7z'"))))
               #t))
           (replace 'build
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (pyver ,(version-major+minor (package-version python)))
                      (lib (string-append out "/lib/python" pyver)))
                 (invoke (which "python") "installer.py" "--srcdir=mcomix"
                         (string-append "--target=" lib))
                 (rename-file (string-append lib "/mcomix")
                              (string-append lib "/site-packages"))
                 #t)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (share (string-append out "/share"))
                      (bin (string-append out "/bin"))
                      (pyver ,(version-major+minor (package-version python)))
                      (lib (string-append out "/lib/python" pyver "/site-packages")))
                 (mkdir-p bin)
                 (rename-file (string-append lib "/mcomixstarter.py")
                              (string-append bin "/mcomix"))
                 (rename-file (string-append lib "/comicthumb.py")
                              (string-append bin "/comicthumb"))
                 (install-file "mime/mcomix.desktop"
                               (string-append share "/applications"))
                 (install-file "mime/mcomix.appdata.xml"
                               (string-append share "/metainfo"))
                 (install-file "mime/mcomix.xml"
                               (string-append share "/mime/packages"))
                 (install-file "mime/comicthumb.thumbnailer"
                               (string-append share "/thumbnailers"))
                 (install-file "man/mcomix.1" (string-append share "/man/man1"))
                 (install-file "man/comicthumb.1" (string-append share "/man/man1"))
                 (for-each
                  (lambda (size)
                    (install-file
                     (format #f "mcomix/mcomix/images/~sx~s/mcomix.png" size size)
                     (format #f "~a/icons/hicolor/~sx~s/apps/" share size size))
                    (for-each
                     (lambda (ext)
                       (install-file
                        (format #f "mime/icons/~sx~s/application-x-~a.png" size size ext)
                        (format #f "~a/icons/hicolor/~sx~s/mimetypes/"
                                share size size)))
                     '("cb7" "cbr" "cbt" "cbz")))
                  '(16 22 24 32 48))
                 #t))))))
      (home-page "https://sourceforge.net/p/mcomix/wiki/Home/")
      (synopsis "Image viewer for comics")
      (description "MComix is a customizable image viewer that specializes as
a comic and manga reader.  It supports a variety of container formats
including CBZ, CB7, CBT, LHA.

For PDF support, install the @emph{mupdf} package.")
      (license license:gpl2+))))

(define-public qview
  (package
    (name "qview")
    (version "4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jurplel/qView")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15n9cq7w3ckinnx38hvncxrbkv4qm4k51sal41q4y0pkvhmafhnr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "qmake" (string-append "PREFIX=" out)))))
         ;; Don't phone home or show "Checking for updates..." in the About
         ;; menu.
         (add-before 'build 'disable-auto-update
           (lambda _
             (substitute* "src/qvaboutdialog.cpp"
               (("qvApp->checkUpdates\\(\\);") "")
               (("updateText\\(\\);") ""))
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
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hpjansson.org/chafa/releases/chafa-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "18rb82bfqj1sj2g4irazx4lwq9q4b4k7my1r0q714vf9yhs41ls6"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("freetype" ,freetype)
       ("libjpeg" ,libjpeg-turbo)
       ("glib" ,glib)
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

(define-public qiv
  (package
    (name "qiv")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://spiegl.de/qiv/download/qiv-"
                           version ".tgz"))
       (sha256
        (base32 "1rlf5h67vhj7n1y7jqkm9k115nfnzpwngj3kzqsi2lg676srclv7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ;; That is required for testing.
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("imlib2" ,imlib2)
       ("glib" ,glib)
       ("gtk+" ,gtk+-2)
       ("lcms" ,lcms)
       ("libjpeg" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("libexif" ,libexif)
       ("libx11" ,libx11)
       ("libxext" ,libxext)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'install 'patch-file-start-xserver
           (lambda* (#:key inputs #:allow-other-keys)
             ;; patch the file so that qiv runs and exits by itself
             (substitute* "Makefile"
               (("./qiv -f ./intro.jpg") "./qiv -f -C -s ./intro.jpg")
               ;; Fail the build when test fails.
               (("echo \"-- Test Failed --\"")
                "(echo \"-- Test Failed --\" ; false)"))
             ;; There must be a running X server and make install doesn't start one.
             ;; Therefore we must do it.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             #t)))
       #:tests? #f                      ; there is no check target
       #:make-flags
       (list
        (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (home-page "http://spiegl.de/qiv/")
    (synopsis "Graphical image viewer for X")
    (description
     "Quick Image Viewer is a small and fast GDK/Imlib2 image viewer.
Features include zoom, maxpect, scale down, fullscreen, slideshow, delete,
brightness/contrast/gamma correction, pan with keyboard and mouse, flip,
rotate left/right, jump/forward/backward images, filename filter and use it
to set X desktop background.")
    (license license:gpl2)))

(define-public nomacs
  (package
    (name "nomacs")
    (version "3.16.224")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nomacs/nomacs")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "05d4hqg0gl3g9s2xf1hr7mc7g4cqarcap4nzxxa51fsphw2b8x16"))))
    (build-system cmake-build-system)
    (arguments
     `(#:build-type "Release" ; fails to build with debug info
       #:configure-flags (list "-DENABLE_TRANSLATIONS=true"
                               "-DUSE_SYSTEM_QUAZIP=true"
                               "-DENABLE_OPENCV=true")
       #:tests? #f ; no rule for target 'test'
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'copy-plugins
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "plugins")
                               "ImageLounge/plugins")))
         (add-after 'copy-plugins 'cd-to-source-dir
           (lambda _ (chdir "ImageLounge") #t)))))
    (inputs
     `(("plugins"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/nomacs/nomacs-plugins")
                 (commit "3.16")))
           (sha256
            (base32
             "1cpdwhfvaxm970nwdc1hc13848a85pqqi176m9xpa3krla9qskml"))))
       ("exiv2" ,exiv2)
       ("libraw" ,libraw)
       ("libtiff" ,libtiff)
       ("opencv" ,opencv)
       ("python" ,python-wrapper)
       ("quazip" ,quazip)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qtlinguist" ,qttools)))
    (synopsis "Image viewer supporting all common formats")
    (description "Nomacs is a simple to use image lounge featuring
semi-transparent widgets that display additional information such as metadata,
thumbnails and histograms.  It is able to browse images compressed archives
and add notes to images.

Nomacs includes image manipulation methods for adjusting brightness, contrast,
saturation, hue, gamma, and exposure.  It has a pseudo color function which
allows creating false color images.  A unique feature of Nomacs is the
synchronization of multiple instances.")
    (home-page "https://nomacs.org/")
    (license license:gpl3+)))

(define-public xzgv
  (package
    (name "xzgv")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/xzgv/"
                           version "/xzgv-" version ".tar.gz"))
       (sha256
        (base32 "17l1xr9v07ggwga3vn0z1i4lnwjrr20rr8z1kjbw71aaijxl18i5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "config.mk"
               (("/usr/local") (assoc-ref outputs "out")))))
         (delete 'configure)            ; no configure script
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "src/xzgv" bin))))) ; just install the executable
       #:tests? #f))                             ; no rule for target 'test'
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("libexif" ,libexif)))
    (home-page "https://sourceforge.net/projects/xzgv/")
    (synopsis "Picture viewer for X with a thumbnail-based selector")
    (description
     "xzgv is a fast image viewer that provides extensive keyboard support.")
    (license license:gpl2+)))
