;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2017, 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2019 nee <nee@cock.li>
;;; Copyright © 2019 Yoshinori Arai <kumagusu08@gmail.com>
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

(define-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages anthy)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg))




;; packages without propagated input
;; (rationale for this separation: The packages in PROPAGATED_INPUTS need to
;; be defined first, the split makes book-keeping easier.)


;; compiles only on macos
;; (define-public applewmproto


(define xorg-cf-files
  ;; The xorg-cf-files package contains the data files for the imake utility,
  ;; defining the known settings for a wide variety of platforms (many of which
  ;; have not been verified or tested in over a decade), and for many of the
  ;; libraries formerly delivered in the X.Org monolithic releases.
  ;;
  ;; License: x11, see COPYING
  (origin
    (method url-fetch)
    (uri "mirror://xorg/individual/util/xorg-cf-files-1.0.5.tar.bz2")
    (sha256
     (base32
      "1m3ypq0xcy46ghxc0svl1rbhpy3zvgmy0aa2mn7w7v7d8d8bh8zd"))))

(define-public imake
  (package
    (name "imake")
    (version "1.0.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://xorg/individual/util/imake-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "0zpk8p044jh14bis838shbf4100bjg7mccd7bq54glpsq552q339"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (inputs
      `(("xorg-cf-files" ,xorg-cf-files)
        ("xorgproto" ,xorgproto)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((cf-files (assoc-ref inputs "xorg-cf-files"))
                   (out (assoc-ref outputs "out"))
                   (unpack (assoc-ref %standard-phases 'unpack))
                   (patch-source-shebangs
                    (assoc-ref %standard-phases 'patch-source-shebangs)))
               (mkdir "xorg-cf-files")
               (with-directory-excursion "xorg-cf-files"
                 (apply unpack (list #:source cf-files))
                 (apply patch-source-shebangs (list #:source cf-files))
                 (substitute* '("mingw.cf" "Imake.tmpl" "nto.cf" "os2.cf"
                                "linux.cf" "Amoeba.cf" "cygwin.cf")
                   (("/bin/sh") (which "bash")))
                 (invoke "./configure"
                         (string-append "SHELL=" (which "bash"))
                         (string-append "--prefix=" out))
                 (invoke "make" "install"))))))))
    (home-page "https://www.x.org/")
    (synopsis "Source code configuration and build system")
    (description
     "Imake is a deprecated source code configuration and build system which
has traditionally been supplied by and used to build the X Window System in
X11R6 and previous releases.  As of the X Window System X11R7 release, the X
Window system has switched to using GNU autotools as the primary build system,
and the Imake system is now deprecated, and should not be used by new software
projects.  Software developers are encouraged to migrate software to the GNU
autotools system.")
    (license license:x11)))

(define-public lndir
  (package
    (name "lndir")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/util/"
                    "lndir-" version ".tar.bz2"))
              (sha256
               (base32
                "0pdngiy8zdhsiqx2am75yfcl36l7kd7d7nl0rss8shcdvsqgmx29"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("xorgproto" ,xorgproto)))
    (home-page "https://www.x.org/")
    (synopsis "Symlink directory into tree")
    (description "Create a shadow directory of symbolic links to another
directory tree.")
    (license license:x11)))

(define-public bdftopcf
  (package
    (name "bdftopcf")
    (version "1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/bdftopcf-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "18hiscgljrz10zjcws25bis32nyrg3hzgmiq6scrh7izqmgz0kab"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxfont" ,libxfont2)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Convert X font from BDF to PCF")
    (description
     "BDFtoPCF is a font compiler for the X server and font server.  It
converts X font from Bitmap Distribution Format to Portable Compiled Format
which can be read by any architecture.")
    (license license:x11)))

(define-public xorgproto
  (package
    (name "xorgproto")
    (version "2018.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/proto/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "180mqkp70i44rkmj430pmn9idssvffrgv4y5h19fm698a7h8bs7y"))))
    (build-system gnu-build-system)
    (propagated-inputs
     ;; To get util-macros in (almost?) all package inputs.
     `(("util-macros" ,util-macros)))
    (home-page "https://cgit.freedesktop.org/xorg/proto/xorgproto")
    (synopsis "Xorg protocol headers")
    (description
     "This package provides the headers and specification documents defining
the core protocol and (many) extensions for the X Window System.")
    (license license:x11)))

(define-public bigreqsproto
  (package
    (name "bigreqsproto")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/bigreqsproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07hvfm84scz8zjw14riiln2v4w03jlhp756ypwhq27g48jmic8a6"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg BigReqsProto protocol headers")
    (description
     "Big Requests Extension defines a protocol to enable the use of
requests that exceed 262140 bytes in length.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public compositeproto
  (package
    (name "compositeproto")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/compositeproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1z0crmf669hirw4s7972mmp8xig80kfndja9h559haqbpvq5k4q4"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg CompositeProto protocol headers")
    (description
     "Composite Extension contains header files and documentation for
the damage protocol.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public damageproto
  (package
    (name "damageproto")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/damageproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0nzwr5pv9hg7c21n995pdiv0zqhs91yz3r8rn3aska4ykcp12z2w"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg DamageProto protocol headers")
    (description
     "Damage Extension contains header files and documentation for
the damage protocol.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public dmxproto
  (package
    (name "dmxproto")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/dmxproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "02b5x9dkgajizm8dqyx2w6hmqx3v25l67mgf35nj6sz0lgk52877"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg DMXProto protocol headers")
    (description
     "DMX (Distributed Multihead X) Extension defines a protocol for clients
to access a front-end proxy X server that controls multiple back-end X
servers making up a large display.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public dri2proto
  (package
    (name "dri2proto")
    (version "2.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/dri2proto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "015az1vfdqmil1yay5nlsmpf6cf7vcbpslxjb72cfkzlvrv59dgr"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg DRI2Proto protocol headers")
    (description
     "Direct Rendering Infrastructure 2 Extension defines a protocol to
securely allow user applications to access the video hardware without
requiring data to be passed through the X server.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public dri3proto
  (package
    (name "dri3proto")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/dri3proto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0x609xvnl8jky5m8jdklw4nymx3irkv32w99dfd8nl800bblkgh1"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg DRI3Proto protocol headers")
    (description
     "Direct Rendering Infrastructure 3 Extension provides mechanisms to
translate between direct rendered buffers and X pixmaps.  When combined with
the Present extension, a complete direct rendering solution for OpenGL is
provided.")
    (license (license:x11-style "file://dri3proto.h"
                                "See 'dri3proto.h' in the distribution."))
    (properties `((superseded . ,xorgproto)))))

(define-public %app-defaults-dir "/lib/X11/app-defaults")

(define-public editres
  (package
    (name "editres")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/" name "-"
               version
               ".tar.bz2"))
        (sha256
         (base32
          "04awfwmy3f9f0bchidc4ssbgrbicn5gzasg3jydpfnp5513d76h8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-appdefaultdir="
                            %output ,%app-defaults-dir))))
    (inputs
     `(("libxaw" ,libxaw)
       ("libxmu" ,libxmu)
       ("libxt" ,libxt)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Tool to browse and edit X Toolkit resource specifications")
    (description
     "Editres is a tool that allows users and application developers to view
the full widget hierarchy of any X Toolkit application that speaks the Editres
protocol.  In addition, editres will help the user construct resource
specifications, allow the user to apply the resource to the application and
view the results dynamically.  Once the user is happy with a resource
specification editres will append the resource string to the user's X
Resources file.")
    (license license:x11)))


(define-public encodings
  (package
    (name "encodings")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/font/encodings-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ffmaw80vmfwdgvdkp6495xgsqszb6s0iira5j0j6pd4i0lk3mnf"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontscale" ,mkfontscale)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg font encoding files")
    (description "Xorg font encoding files.")
    (license license:public-domain)))

(define (%xorg-font-origin font version hash)
  (origin
    (method url-fetch)
    (uri (string-append "mirror://xorg/individual/font/" font "-"
                        version ".tar.bz2"))
    (sha256 hash)
    (modules '((guix build utils)))
    (snippet
     ;; Do not include timestamps in '.pcf.gz' files.
     '(begin
        (substitute* "Makefile.in"
          (("^COMPRESS = (.*)$" _ rest)
           (string-append "COMPRESS = " (string-trim-right rest)
                          " --no-name\n")))
        #t))))

(define-syntax-rule (xorg-font-origin font version hash)
  "Expand to the 'origin' form for the given Xorg font package."
  (%xorg-font-origin font version (base32 hash)))

(define-public font-adobe100dpi
  (package
    (name "font-adobe100dpi")
    (version "1.0.3")
    (source (xorg-font-origin
             "font-adobe-100dpi" version
             "0m60f5bd0caambrk8ksknb5dks7wzsg7g7xaf0j21jxmx8rq9h5j"))
    (build-system gnu-build-system)
    (inputs
      `(("bdftopcf" ,bdftopcf)
        ("font-util" ,font-util)
        ("mkfontdir" ,mkfontdir)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (arguments
      `(#:configure-flags (list
        ;; install fonts into subdirectory of package output instead of
        ;; font-util-?.?.?/share/fonts/X11
        (string-append "--with-fontrootdir=" %output "/share/fonts/X11"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg adobe-100dpi fonts")
    (description "Xorg adobe-100dpi fonts.")
    (license license:x11)))


(define-public font-adobe75dpi
  (package
    (name "font-adobe75dpi")
    (version "1.0.3")
    (source (xorg-font-origin
             "font-adobe-75dpi" version
             "02advcv9lyxpvrjv8bjh1b797lzg6jvhipclz49z8r8y98g4l0n6"))
    (build-system gnu-build-system)
    (inputs
      `(("bdftopcf" ,bdftopcf)
        ("font-util" ,font-util)
        ("mkfontdir" ,mkfontdir)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (arguments
      `(#:configure-flags (list
        (string-append "--with-fontrootdir=" %output "/share/fonts/X11"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg adobe-75dpi fonts")
    (description "Xorg adobe-75dpi fonts.")
    (license license:x11)))


;; non-free license
;; (define-public font-adobe-utopia100dpi
;; (define-public font-adobe-utopia75dpi
;; (define-public font-adobe-utopia-type1


(define-public font-alias
  (package
    (name "font-alias")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "16ic8wfwwr3jicaml7b5a0sk6plcgc1kg84w02881yhwmqm3nicb"))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after
                   'install 'install-fonts-dir
                   ;; The X font server will not add directories to the font
                   ;; path unless they contain a "fonts.dir" file, so add some
                   ;; dummy files.
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out")))
                       (for-each (lambda (d)
                                   (call-with-output-file
                                       (string-append out "/share/fonts/X11"
                                                      "/" d "/fonts.dir")
                                     (lambda (p)
                                       (format p "0~%"))))
                                 '("75dpi" "100dpi" "misc" "cyrillic"))
                       #t))))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg font aliases")
    (description
     "This package provides some common aliases for Xorg fonts.
For example: '6x10', '9x15bold', etc.")
    (license license:x11)))


(define-public font-arabic-misc
  (package
    (name "font-arabic-misc")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "1x246dfnxnmflzf0qzy62k8jdpkb6jkgspcjgbk8jcq9lw99npah"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg arabic-misc font")
    (description "Xorg arabic-misc font.")
    (license license:x11)))


;; non-free license
;; (define-public font-bh100dpi
;; (define-public font-bh75dpi
;; (define-public font-bh-lucidatypewriter100dpi
;; (define-public font-bh-lucidatypewriter75dpi
;; (define-public font-bh-ttf
;; (define-public font-bh-type1
;; (define-public font-bitstream100dpi
;; (define-public font-bitstream75dpi


(define-public font-cronyx-cyrillic
  (package
    (name "font-cronyx-cyrillic")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "0ai1v4n61k8j9x2a1knvfbl2xjxk3xxmqaq3p9vpqrspc69k31kf"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg cronyx-cyrillic font")
    (description "Xorg cronyx-cyrillic font.")
    (license license:x11)))


;; no license
;; (define-public font-cursor-misc

;; non-free license
;; (define-public font-daewoo-misc


(define-public font-dec-misc
  (package
    (name "font-dec-misc")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "0yzza0l4zwyy7accr1s8ab7fjqkpwggqydbm2vc19scdby5xz7g1"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg dec-misc font")
    (description "Xorg dec-misc font.")
    (license license:x11)))


;; non-free license
;; (define-public font-ibm-type1

(define-public font-isas-misc
  (package
    (name "font-isas-misc")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "0rx8q02rkx673a7skkpnvfkg28i8gmqzgf25s9yi0lar915sn92q"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg isas-misc font")
    (description "Xorg isas-misc font.")
    (license license:x11)))


;; non-free license
;; (define-public font-jis-misc


(define-public font-micro-misc
  (package
    (name "font-micro-misc")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "1dldxlh54zq1yzfnrh83j5vm0k4ijprrs5yl18gm3n9j1z0q2cws"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg micro-misc font")
    (description "Xorg micro-misc font.")
    (license license:public-domain)))


(define-public font-misc-cyrillic
  (package
    (name "font-misc-cyrillic")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "0q2ybxs8wvylvw95j6x9i800rismsmx4b587alwbfqiw6biy63z4"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg misc-cyrillic fonts")
    (description "Xorg misc-cyrillic fonts.")
    (license license:x11)))


(define-public font-misc-ethiopic
  (package
    (name "font-misc-ethiopic")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "19cq7iq0pfad0nc2v28n681fdq3fcw1l1hzaq0wpkgpx7bc1zjsk"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("mkfontscale" ,mkfontscale)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg misc-ethiopic fonts")
    (description "Xorg misc-ethiopic fonts.")
    (license license:x11)))


;; non-free license
;; (define-public font-misc-meltho


(define-public font-misc-misc
  (package
    (name "font-misc-misc")
    (version "1.1.2")
    (source (xorg-font-origin
             name version
             "150pq6n8n984fah34n3k133kggn9v0c5k07igv29sxp1wi07krxq"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("font-util" ,font-util)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
      `(#:configure-flags (list
        (string-append "--with-fontrootdir=" %output "/share/fonts/X11"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg misc-misc fonts")
    (description "Xorg misc-misc fixed fonts.")
    (license license:public-domain)))


(define-public font-mutt-misc
  (package
    (name "font-mutt-misc")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "13qghgr1zzpv64m0p42195k1kc77pksiv059fdvijz1n6kdplpxx"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg mutt-misc fonts")
    (description "Xorg mutt-misc fonts.")
    (license license:x11)))


(define-public font-schumacher-misc
  (package
    (name "font-schumacher-misc")
    (version "1.1.2")
    (source (xorg-font-origin
             name version
             "0nkym3n48b4v36y4s927bbkjnsmicajarnf6vlp7wxp0as304i74"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("font-util" ,font-util)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (arguments
      `(#:configure-flags (list
        (string-append "--with-fontrootdir=" %output "/share/fonts/X11"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg schumacher-misc fonts")
    (description "Xorg schumacher-misc fonts.")
    (license license:x11)))


(define-public font-screen-cyrillic
  (package
    (name "font-screen-cyrillic")
    (version "1.0.4")
    (source (xorg-font-origin
             name version
             "0yayf1qlv7irf58nngddz2f1q04qkpr5jwp4aja2j5gyvzl32hl2"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg screen-cyrillic fonts")
    (description "Xorg screen-cyrillic fonts.")
    (license license:x11)))


(define-public font-sony-misc
  (package
    (name "font-sony-misc")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "1xfgcx4gsgik5mkgkca31fj3w72jw9iw76qyrajrsz1lp8ka6hr0"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg sony-misc fonts")
    (description "Xorg sony-misc fonts.")
    (license license:x11)))


(define-public font-sun-misc
  (package
    (name "font-sun-misc")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "1q6jcqrffg9q5f5raivzwx9ffvf7r11g6g0b125na1bhpz5ly7s8"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg sun-misc fonts")
    (description "Xorg sun-misc fonts.")
    (license license:x11)))


(define-public font-util
  (package
    (name "font-util")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/font/font-util-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "08drjb6cf84pf5ysghjpb4i7xkd2p86k3wl2a0jxs1jif6qbszma"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg font utilities")
    (description
     "Xorg font package creation/installation utilities.")
    (license license:x11)))


(define-public font-winitzki-cyrillic
  (package
    (name "font-winitzki-cyrillic")
    (version "1.0.3")
    (source (xorg-font-origin
             name version
             "181n1bgq8vxfxqicmy1jpm1hnr6gwn1kdhl6hr4frjigs1ikpldb"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg winitzki-cyrillic font")
    (description "Xorg winitzki-cyrillic font.")
    (license license:public-domain)))


(define-public font-xfree86-type1
  (package
    (name "font-xfree86-type1")
    (version "1.0.4")
    (source (xorg-font-origin
             name version
             "0jp3zc0qfdaqfkgzrb44vi9vi0a8ygb35wp082yz7rvvxhmg9sya"))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("mkfontscale" ,mkfontscale)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg xfree86-type1 font")
    (description "Xorg xfree86-type1 font.")
    (license license:x11)))


(define-public fontsproto
  (package
    (name "fontsproto")
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/fontsproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1f2sdsd74y34nnaf4m1zlcbhyv8xb6irnisc99f84c4ivnq4d415"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg FontsProto protocol headers")
    (description
     "Fonts Extension contains header files and documentation for
the fonts protocol.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public glproto
  (package
    (name "glproto")
    (version "1.4.17")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/glproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0h5ykmcddwid5qj6sbrszgkcypwn3mslvswxpgy2n2iixnyr9amd"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg GLProto protocol headers")
    (description
     "OpenGL Extension defines a protocol for the client to send 3D
rendering commands to the X server.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public iceauth
  (package
    (name "iceauth")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/iceauth-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1ik0mdidmyvy48hn8p2hwvf3535rf3m96hhf0mvcqrbj44x23vp6"))))
    (build-system gnu-build-system)
    (inputs
      `(("libice" ,libice)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "ICE authority file utility")
    (description
     "ICEAuth program is used to edit and display the authorization
information used in connecting with ICE (Inter-Client Exchange).  It
operates very much like the xauth program for X11 connection
authentication records.")
    (license license:x11)))


(define-public inputproto
  (package
    (name "inputproto")
    (version "2.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/inputproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07gk7v006zqn3dcfh16l06gnccy7xnqywf3vl9c209ikazsnlfl9"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg InputProto protocol headers")
    (description
     "Input Extension defines a protocol to provide additional input
devices management such as graphic tablets.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public kbproto
  (package
    (name "kbproto")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/kbproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0mxqj1pzhjpz9495vrjnpi10kv2n1s4vs7di0sh3yvipfq5j30pq"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg KBProto protocol headers")
    (description
     "X Keyboard (XKB) Extension defines a protocol to provide a number
of new capabilities and controls for text keyboards.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

;; requires applewmproto, which compiles only on macos
;; (define-public libapplewm


(define-public libdmx
  (package
    (name "libdmx")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libdmx-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0hvjfhrcym770cr0zpqajdy3cda30aiwbjzv16iafkqkbl090gr5"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg DMX library")
    (description
     "DMX (Distributed Multihead X) extension library.")
    (license license:x11)))


(define-public libxshmfence
  (package
    (name "libxshmfence")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/lib/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1ir0j92mnd1nk37mrv9bz5swnccqldicgszvfsh62jd14q6k115q"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("xorgproto" ,xorgproto)))
    (home-page "https://www.x.org/")
    (synopsis "Xorg shared memory fences library")
    (description
     "This library provides an interface to shared-memory fences for
synchronization between the X server and direct-rendering clients.")

    ;; Same license as libevdev.
    (license (license:x11-style "file://COPYING"))))


(define-public libfontenc
  (package
    (name "libfontenc")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libfontenc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0y90170dp8wsidr1dzza0grxr1lfh30ji3b5vkjz4j6x1n0wxz1c"))))
    (build-system gnu-build-system)
    (inputs
      `(("zlib" ,zlib)
        ("xorgproto" ,xorgproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://gitlab.freedesktop.org/xorg/lib/libfontenc")
    (synopsis "Xorg font encoding library")
    (description "Xorg font encoding library.")
    (license license:x11)))


(define-public libfs
  (package
    (name "libfs")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libFS-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1wy4km3qwwajbyl8y9pka0zwizn7d9pfiyjgzba02x3a083lr79f"))))
    (build-system gnu-build-system)
    (inputs
      `(("xtrans" ,xtrans)
        ("xorgproto" ,xorgproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Font Service client library")
    (description
     "Font Service client library is used by clients of X Font
Servers (xfs), such as xfsinfo, fslsfonts, and the X servers
themselves.")
    (license license:x11)))


(define-public libpciaccess
  (package
    (name "libpciaccess")
    (version "0.14")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libpciaccess-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "197jbcpvp4z4x6j705mq2y4fsnnypy6f85y8xalgwhgx5bhl7x9x"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Make sure libpciaccess can read compressed 'pci.ids' files as
       ;; provided by pciutils.
       #:configure-flags
       (list "--with-zlib"
             (string-append "--with-pciids-path="
                            (assoc-ref %build-inputs "pciutils")
                            "/share/hwdata"))

       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'add-L-zlib
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Provide '-LZLIB/lib' next to '-lz' in the .la file.
             (let ((zlib (assoc-ref inputs "zlib"))
                   (out  (assoc-ref outputs "out")))
               (substitute* (string-append out "/lib/libpciaccess.la")
                 (("-lz")
                  (string-append "-L" zlib "/lib -lz")))
               #t))))))
    (inputs
     `(("zlib" ,zlib)
       ("pciutils" ,pciutils)))                   ;for 'pci.ids.gz'
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg PCI access library")
    (description "Xorg Generic PCI access library.")
    (license license:x11)))


(define-public libpthread-stubs
  (package
    (name "libpthread-stubs")
    (version "0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/xcb/libpthread-stubs-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0cz7s9w8lqgzinicd4g36rjg08zhsbyngh0w68c3np8nlc8mkl74"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Library with pthread stubs")
    (description
     "This library provides weak aliases for pthread functions not
provided in libc or otherwise available by default.  Libraries like
libxcb rely on pthread stubs to use pthreads optionally, becoming
thread-safe when linked to libpthread, while avoiding any performance
hit when running single-threaded.")
    (license license:x11)))


(define-public libsm
  (package
    (name "libsm")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libSM-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1fwwfq9v3sqmpzpscymswxn76xhxnysa24pfim1mcpxhvjcl89id"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libice" ,libice))) ; SMlib.h includes ICElib.h
    (inputs
      `(("xtrans" ,xtrans)
        ("util-linux" ,util-linux)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Session Management library")
    (description "Xorg Session Management library.")
    (license license:x11)))


(define-public libwindowswm
  (package
    (name "libwindowswm")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libWindowsWM-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1p0flwb67xawyv6yhri9w17m1i4lji5qnd0gq8v1vsfb8zw7rw15"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg WindowsWM library")
    (description
     "Cygwin/X rootless window management extension.
WindowsWM is a simple library designed to interface with the Windows-WM
extension.  This extension allows X window managers to better interact
with the Cygwin XWin server when running X11 in a rootless mode.")
    (license license:x11)))


(define-public libxcomposite
  (package
    (name "libxcomposite")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXcomposite-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0y21nfpa5s8qmx0srdlilyndas3sgl0c6rc26d5fx2vx436m1qpd"))))
    (build-system gnu-build-system)
    (propagated-inputs
     ;; xcomposite.pc refers to all these.
      `(("xorgproto" ,xorgproto)
        ("libxfixes" ,libxfixes)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Composite library")
    (description
     "Client library for the Composite extension to the X11 protocol.")
    (license license:x11)))


(define-public libxdmcp
  (package
    (name "libxdmcp")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXdmcp-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1qp4yhxbfnpj34swa0fj635kkihdkwaiw7kf55cg5zqqg630kzl1"))))
    (build-system gnu-build-system)
    (inputs
      `(("libbsd" ,libbsd)
        ("xorgproto" ,xorgproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Display Manager Control Protocol library")
    (description "Xorg Display Manager Control Protocol library.")
    (license license:x11)))


(define-public libxft
  (package
    (name "libxft")
    (version "2.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXft-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0k6wzi5rzs0d0n338ms8n8lfyhq914hw4yl2j7553wqxfqjci8zm"))))
    (build-system gnu-build-system)
    (propagated-inputs
     ;; xft.pc refers to all these.
      `(("libxrender" ,libxrender)
        ("freetype" ,freetype)
        ("fontconfig" ,fontconfig)))
    (inputs
      `(("libx11" ,libx11)
        ("xorgproto" ,xorgproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg FreeType library")
    (description
     "Xorg FreeType library connects X applications with the FreeType font
rasterization library.  Xft uses fontconfig to locate fonts so it has no
configuration files.")
    (license license:x11)))


(define-public libxkbfile
  (package
    (name "libxkbfile")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libxkbfile-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0smimr14zvail7ar68n7spvpblpdnih3jxrva7cpa6cn602px0ai"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XKB file handling library")
    (description "Xorg XKB file handling library.")
    (license license:x11)))


(define-public libxmu
  (package
    (name "libxmu")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXmu-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "02wx6jw7i0q5qwx87yf94fsn3h0xpz1k7dz1nkwfwm1j71ydqvkm"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("xorgproto" ,xorgproto)
        ("libxext" ,libxext)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Xmu library")
    (description
     "Xmu library contains miscellaneous utilities and is not part of the
Xlib standard.  It is intended to support clients in the Xorg distribution;
vendors may choose not to distribute this library if they wish.  Therefore,
applications developers who depend on this library should be prepared to
treat it as part of their software base when porting.")
    (license license:x11)))


(define-public libxpm
  (package
    (name "libxpm")
    (version "3.5.12")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXpm-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1v5xaiw4zlhxspvx76y3hq4wpxv7mpj6parqnwdqvpj8vbinsspx"))))
    (build-system gnu-build-system)
    (inputs
      `(("gettext" ,gettext-minimal)
        ("libxt" ,libxt)
        ("xorgproto" ,xorgproto)
        ("libxext" ,libxext)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XPM library")
    (description "XPM (X Pixmap) image file format library.")
    (license license:x11)))


(define-public libxres
  (package
    (name "libxres")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXres-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1m0jr0lbz9ixpp9ihk68349q0i7ry2379lnfzdy4mrl86ijc2xgz"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Resource extension library")
    (description "X Resource extension library.")
    (license license:x11)))


(define-public libxscrnsaver
  (package
    (name "libxscrnsaver")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXScrnSaver-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1y4vx1vabg7j9hamp0vrfrax5b0lmgm3h0lbgbb3hnkv3dd0f5zr"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxext" ,libxext)
        ("libx11" ,libx11)))
    (propagated-inputs
      `(("xorgproto" ,xorgproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Screen Saver library")
    (description "X11 Screen Saver extension client library.")
    (license license:x11)))


(define-public libxxf86dga
  (package
    (name "libxxf86dga")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXxf86dga-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0zn7aqj8x0951d8zb2h2andldvwkzbsc4cs7q023g6nzq6vd9v4f"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xorgproto" ,xorgproto)))
    (inputs
      `(("libx11" ,libx11)
        ("libxext" ,libxext)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XFree86-DGA library")
    (description "Client library for the XFree86-DGA extension.")
    (license license:x11)))


(define-public luit
  (package
    (name "luit")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/luit-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0dn694mk56x6hdk6y9ylx4f128h5jcin278gnw2gb807rf3ygc1h"))
        ;; See https://bugs.freedesktop.org/show_bug.cgi?id=47792;
        ;; should become obsolete with the next release.
        (patches (search-patches "luit-posix.patch"))))
    (build-system gnu-build-system)
    (inputs
      `(("libfontenc" ,libfontenc)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Convert terminal I/O from legacy encodings to UTF-8")
    (description
     "Luit is a filter that can be run between an arbitrary application and
a UTF-8 terminal emulator such as xterm.  It will convert application
output from the locale's encoding into UTF-8, and convert terminal
input from UTF-8 into the locale's encoding.")
    (license license:x11)))


(define-public makedepend
  (package
    (name "makedepend")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/util/makedepend-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "09alw99r6y2bbd1dc786n3jfgv4j520apblyn7cw6jkjydshba7p"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg makedepend utility")
    (description
     "Makedepend is an utility for creating dependencies in makefiles.")
    (license license:x11)))


(define-public mkfontscale
  (package
    (name "mkfontscale")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/mkfontscale-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0siag28jpm8hj62bgjvw81sjfgrc7vcy2h7127bl4iazxrlxz60y"))))
    (build-system gnu-build-system)
    (inputs
      `(("zlib" ,zlib)
        ("xorgproto" ,xorgproto)
        ("freetype" ,freetype)
        ("libfontenc" ,libfontenc)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Create an index of scalable font files for X server")
    (description
     "MkFontScale creates the 'fonts.scale' and 'fonts.dir' index files used
by the legacy X11 font system.")
    (license license:x11)))


(define-public presentproto
  (package
    (name "presentproto")
    (version "1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/presentproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1f96dlgfwhsd0834z8ydjzjnb0cwha5r6lxgia4say4zhsl276zn"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg PresentProto protocol headers")
    (description
     "Present Extension provides a way for applications to update their
window contents from a pixmap in a well defined fashion, synchronizing
with the display refresh and potentially using a more efficient
mechanism than copying the contents of the source pixmap.")
    (license (license:x11-style "file://presentproto.h"
                                "See 'presentproto.h' in the distribution."))
    (properties `((superseded . ,xorgproto)))))

(define-public printproto
  (package
    (name "printproto")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/printproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "06liap8n4s25sgp27d371cc7yg9a08dxcr3pmdjp761vyin3360j"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg PrintProto protocol headers")
    (description
     "Print Extension defines a protocol for a portable,
network-transparent printing system.")
    (license license:x11)))


(define-public randrproto
  (package
    (name "randrproto")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/randrproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0s4496z61y5q45q20gldwpf788b9nsa8hb13gnck1mwwwwrmarsc"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg RandRProto protocol headers")
    (description
     "Resize and Rotate Extension defines a protocol for clients to
dynamically change X screens, so as to resize, rotate and reflect the root
window of a screen.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))


(define-public recordproto
  (package
    (name "recordproto")
    (version "1.14.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/recordproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0w3kgr1zabwf79bpc28dcnj0fpni6r53rpi82ngjbalj5s6m8xx7"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg RecordProto protocol headers")
    (description
     "Record Extension defines a protocol for the recording and playback
of user actions in the X Window System.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public renderproto
  (package
    (name "renderproto")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/renderproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0dr5xw6s0qmqg0q5pdkb4jkdhaja0vbfqla79qh5j1xjj9dmlwq6"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg RenderProto protocol headers")
    (description
     "Rendering Extension defines a protcol for a digital image composition
as the foundation of a new rendering model within the X Window System.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public resourceproto
  (package
    (name "resourceproto")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/resourceproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0638iyfiiyjw1hg3139pai0j6m65gkskrvd9684zgc6ydcx00riw"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg ResourceProto protocol headers")
    (description
     "Resource Extension defines a protocol that allows a client to
query the X server about its usage of various resources.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public scrnsaverproto
  (package
    (name "scrnsaverproto")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/scrnsaverproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0rfdbfwd35d761xkfifcscx56q0n56043ixlmv70r4v4l66hmdwb"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg ScrnSaverProto protocol headers")
    (description
     "Screen Saver Extension defines a protocol to control screensaver
features and to query screensaver info on specific windows.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public sessreg
  (package
    (name "sessreg")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/sessreg-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1qd66mg2bnppqz4xgdjzif2488zl82vx2c26ld3nb8pnyginm9vq"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Register X sessions in system utmp/utmpx databases")
    (description
     "SessReg is a simple program for managing utmp/wtmp entries for X
sessions.  It was originally written for use with xdm, but may also be
used with other display managers such as gdm or kdm.")
    (license license:x11)))


(define-public setxkbmap
  (package
    (name "setxkbmap")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/setxkbmap-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1qfk097vjysqb72pq89h0la3462kbb2dh1d11qzs2fr67ybb7pd9"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxkbfile" ,libxkbfile)
        ("xkeyboard-config" ,xkeyboard-config)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-xkb-config-root="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Set the keyboard using the X Keyboard Extension")
    (description
     "Setxkbmap is an X11 client to change the keymaps in the X server
for a specified keyboard to use the layout determined by the options
listed on the command line.")
    (license license:x11)))


(define-public smproxy
  (package
    (name "smproxy")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/smproxy-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0rkjyzmsdqmlrkx8gy2j4q6iksk58hcc92xzdprkf8kml9ar3wbc"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("libxmu" ,libxmu)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Session Manager Proxy")
    (description
     "SMProxy allows X applications that do not support X11R6 session
management to participate in an X11R6 session.")
    (license license:x11)))


(define-public util-macros
  (package
    (name "util-macros")
    (version "1.19.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/util/util-macros-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "04p7ydqxgq37jklnfj18b70zsifiz4h50wvrk94i2112mmv37r6p"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-makefile-in
           (lambda _
             (substitute* "Makefile.in"
               ;; Install xorg-macros.pc in PREFIX/lib/pkgconfig,
               ;; not PREFIX/share/pkgconfig.
               (("\\$\\(datadir\\)/pkgconfig") "$(libdir)/pkgconfig"))
             #t))
         (add-after 'install 'post-install-cleanup
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion out
                 (delete-file "share/util-macros/INSTALL")
                 (rmdir "share/util-macros"))
               #t))))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg autoconf macros")
    (description
     "This package provides a set of autoconf macros used by the
configure.ac scripts in other Xorg modular packages, and is needed to
generate new versions of their configure scripts with autoconf.")
    (license license:x11)))


(define-public videoproto
  (package
    (name "videoproto")
    (version "2.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/videoproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "00m7rh3pwmsld4d5fpii3xfk5ciqn17kkk38gfpzrrh8zn4ki067"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg VideoProto protocol headers")
    (description
     "Video Extension provides a protocol for a video output mechanism,
mainly to rescale video playback in the video controller hardware.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public windowswmproto
  (package
    (name "windowswmproto")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/windowswmproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0syjxgy4m8l94qrm03nvn5k6bkxc8knnlld1gbllym97nvnv0ny0"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg WindowsWMProto protocol headers")
    (description
     "WindowsWM Extension defines a protocol, used for coordination between
an X11 server and the Microsoft Windows native window manager.  WindowsWM
is only intended to be used on Cygwin when running a rootless XWin
server.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public x11perf
  (package
    (name "x11perf")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/x11perf-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0lb716yfdb8f11h4cz93d1bapqdxf1xplsb21kbp4xclq7g9hw78"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)
        ("libxft" ,libxft)
        ("libxmu" ,libxmu)
        ("libxrender" ,libxrender)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "X server performance benchmarker")
    (description
     "X11Perf is a simple performance benchmarker for the Xorg X server.")
    (license license:x11)))


(define-public xauth
  (package
    (name "xauth")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xauth-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0kgwz9rmxjfdvi2syf8g0ms5rr5cgyqx4n0n1m960kyz7k745zjs"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libxext" ,libxext)
        ("libxau" ,libxau)
        ("libx11" ,libx11)))
    (native-inputs
     `(("cmdtest" ,cmdtest)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "X authority file utility")
    (description
     "XAuth program is used to edit and display the authorization
information used in connecting to the X server.")
    (license license:x11)))


(define-public xbacklight
  (package
    (name "xbacklight")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xbacklight-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0pmzaz4kp38qv2lqiw5rnqhwzmwrq65m1x5j001mmv99wh9isnk1"))))
    (build-system gnu-build-system)
    (inputs
     `(("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Control display backlight")
    (description
     "Xbacklight is used to adjust the backlight brightness where
supported.  It uses the RandR extension to find all outputs on the X
server supporting backlight brightness control and changes them all in
the same way.")
    (license license:x11)))


(define-public xbitmaps
  (package
    (name "xbitmaps")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/data/xbitmaps-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1vh73sc13s7w5r6gnc6irca56s7998bja7wgdivkfn8jccawgw5r"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "X bitmaps")
    (description
     "xbitmaps provides basic bitmaps (little pictures) used by some
legacy X clients.")
    (license license:x11)))

(define-public xcalc
  (package
    (name "xcalc")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/"
                           "xcalc-" version ".tar.gz"))
       (sha256
        (base32 "1xgih1iq9498m7vk22qrdsck1rb905sn395azjn4a6pmhfai401b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after
                   'configure 'mutate-makefile
                   (lambda _
                     (substitute* "Makefile"
                       (("^appdefaultdir = .*$")
                        (string-append "appdefaultdir = " %output
                                       ,%app-defaults-dir "\n")))
                     #t)))))
    (inputs
     `(("libxaw" ,libxaw)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Hand calculator for the X Window system")
    (description "Xcalc is a scientific calculator desktop accessory that can
emulate a TI-30 or an HP-10C.")
    (license license:x11)))


(define-public xcb-proto
  (package
    (name "xcb-proto")
    (version "1.13")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://xcb.freedesktop.org/dist/xcb-proto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1qdxw9syhbvswiqj5dvj278lrmfhs81apzmvx6205s4vcqg7563v"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("pkg-config" ,pkg-config) ("python" ,python-minimal-wrapper)))
    (home-page "https://xcb.freedesktop.org/")
    (synopsis "XML-XCB protocol descriptions")
    (description
     "XCB-Proto provides the XML-XCB protocol descriptions that libxcb
uses to generate the majority of its code and API.  XCB-Proto is
separated from libxcb to allow reuse by other projects, such as
additional language bindings, protocol dissectors, or documentation
generators.

XCB-Proto also contains language-independent Python libraries that are
used to parse an XML description and create objects used by Python code
generators in individual language bindings.")
    (license license:x11)))


(define-public xcmiscproto
  (package
    (name "xcmiscproto")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xcmiscproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1pyjv45wivnwap2wvsbrzdvjc5ql8bakkbkrvcv6q9bjjf33ccmi"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XCMiscProto protocol headers")
    (description
     "XC-MISC Extension defines a protocol that provides Xlib two ways
to query the server for available resource IDs.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xcmsdb
  (package
    (name "xcmsdb")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xcmsdb-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1ik7gzlp2igz183x70883000ygp99r20x3aah6xhaslbpdhm6n75"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Device Color Characterization utility")
    (description
     "XCMSDB is used to load, query, or remove Device Color
Characterization data stored in properties on the root window of the
screen as specified in section 7, Device Color Characterization, of the
X11 Inter-Client Communication Conventions Manual (ICCCM).")
    (license license:x11)))


(define-public xcursor-themes
  (package
    (name "xcursor-themes")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/data/xcursor-themes-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "16a96li0s0ggg60v7f6ywxmsrmxdfizcw55ccv7sp4qjfisca7pf"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxcursor" ,libxcursor)
        ("xcursorgen" ,xcursorgen)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-cursordir="
                            (assoc-ref %outputs "out")
                            "/share/icons"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Default Xorg cursors")
    (description
     "This package provides a default set of cursor themes for the Xorg
X server: 'handhelds', 'redglass' and 'whiteglass'.")
    (license license:x11)))


(define-public xcursorgen
  (package
    (name "xcursorgen")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xcursorgen-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ggbv084cavp52hjgcz3vdj0g018axs0m23c03lpc5sgn92gidim"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxcursor" ,libxcursor)
        ("libpng" ,libpng)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Create an X cursor file from PNG images")
    (description
     "XCursorGen prepares X11 cursor sets for use with libXcursor.")
    (license license:x11)))


(define-public xdpyinfo
  (package
    (name "xdpyinfo")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xdpyinfo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ldgrj4w2fa8jng4b3f3biaj0wyn8zvya88pnk70d7k12pcqw8rh"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)
        ("libx11" ,libx11)
        ("libxxf86vm" ,libxxf86vm)
        ("libxxf86dga" ,libxxf86dga)
        ("libxtst" ,libxtst)
        ("libxrender" ,libxrender)
        ("libxinerama" ,libxinerama)
        ("libxi" ,libxi)
        ("libxcomposite" ,libxcomposite)
        ("libdmx" ,libdmx)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg display information utility")
    (description
     "Xdpyinfo is used to display information about an X server: the
capabilities of a server, the predefined values for various parameters
used in communicating between clients and the server, and the different
types of screens, visuals, and X11 protocol extensions that are
available.")
    (license license:x11)))


(define-public xdriinfo
  (package
    (name "xdriinfo")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xdriinfo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0lcx8h3zd11m4w8wf7dyp89826d437iz78cyrix436bqx31x5k6r"))))
    (build-system gnu-build-system)
    (inputs
      `(("mesa" ,mesa)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Query DRI configuration information")
    (description
     "XDRIInfo is used to query configuration information of X11
DRI (Direct Rendering Infrastructure) drivers.")
    (license license:x11)))


(define-public xev
  (package
    (name "xev")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xev-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "02ddsdx138g7szhwklpbzi0cxr34871iay3k28kdcihrz8f4zg36"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("libxrandr" ,libxrandr)
        ("xorgproto" ,xorgproto)
        ("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xev")
    (synopsis "Print contents of X events")
    (description
     "XEv creates a window and then asks the X server to send it X11
events whenever anything happens to the window (such as it being moved,
resized, typed in, clicked in, etc.).  You can also attach it to an
existing window.  It is useful for seeing what causes events to occur
and to display the information that they contain; it is essentially a
debugging and development tool, and should not be needed in normal
usage.")
    (license license:x11)))


(define-public xextproto
  (package
    (name "xextproto")
    (version "7.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xextproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1c2vma9gqgc2v06rfxdiqgwhxmzk2cbmknwf1ng3m76vr0xb5x7k"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XExtProto protocol headers")
    (description
     "XExtProto provides the following extensions: DOUBLE-BUFFER, DPMS,
Extended-Visual-Information, Generic Event Extension, LBX, MIT-SHM,
MIT-SUNDRY-NONSTANDARD, Multi-Buffering, SECURITY, SHAPE, SYNC, TOG-CUP,
XC-APPGROUP, XTEST.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public libevdev
  (package
    (name "libevdev")
    (version "1.5.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/" name "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0xca343ff12wh6nsq76r0nbsfrm8dypjrzm4fqz9vv9v8i8kfrp1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'pedantry
                    (lambda _
                      ;; XXX: libevdev includes kernel headers, which causes this
                      ;; compile test to fail with:
                      ;; ...-headers-4.14.67/include/asm-generic/posix_types.h:88:14:
                      ;;error: ISO C90 does not support ‘long long’ [-Werror=long-long]
                      (substitute* "test/Makefile.in"
                        (("-pedantic -Werror -std=c89")
                         "-pedantic -Werror -std=c99"))
                      #t)))))
    (native-inputs `(("python" ,python)))
    (home-page "https://www.freedesktop.org/wiki/Software/libevdev/")
    (synopsis "Wrapper library for evdev devices")
    (description
     "libevdev is a wrapper library for evdev devices. it moves the common
tasks when dealing with evdev devices into a library and provides a library
interface to the callers, thus avoiding erroneous ioctls, etc.

The eventual goal is that libevdev wraps all ioctls available to evdev
devices, thus making direct access unnecessary.")
    (license (license:x11-style "file://COPYING"))))


(define-public xf86-input-evdev
  (package
    (name "xf86-input-evdev")
    (version "2.10.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-evdev-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1h1y0fwnawlp4yc5llr1l7hwfcxxpln2fxhy6arcf6w6h4z0f9l7"))))
    (build-system gnu-build-system)
    (inputs
      `(("udev" ,eudev)
        ("libevdev" ,libevdev)
        ("mtdev" ,mtdev)
        ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Generic input driver for X server")
    (description
     "xf86-input-evdev is a generic input driver for the Xorg X server.
This driver supports all input devices that the kernel knows about,
including most mice, keyboards, tablets and touchscreens.")
    (license license:x11)))

(define-public xf86-input-libinput
  (package
    (name "xf86-input-libinput")
    (version "0.28.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/driver/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0818vr0yhk9j1y1wcbxzcd458vrvp06rrhi8k43bhqkb5jb4dcxq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-sdkdir="
                            %output "/include/xorg"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libinput" ,libinput)
       ("xorg-server" ,xorg-server)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Input driver for X server based on libinput")
    (description
     "xf86-input-libinput is an input driver for the Xorg X server based
on libinput.  It is a thin wrapper around libinput, so while it does
provide all features that libinput supports it does little beyond.")
    (license license:x11)))

(define-public xf86-input-joystick
  (package
    (name "xf86-input-joystick")
    (version "1.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-joystick-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1awfq496d082brgjbr60lhm6jvr9537rflwxqdfqwfzjy3n6jxly"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Joystick input driver for X server")
    (description
     "xf86-input-joystick is a joystick input driver for the Xorg X server.
It is used to cotrol the pointer with a joystick device.")
    (license license:x11)))


(define-public xf86-input-keyboard
  (package
    (name "xf86-input-keyboard")
    (version "1.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-keyboard-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "12032yg412kyvnmc5fha1in7mpi651d8sa1bk4138s2j2zr01jgp"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Keyboard input driver for X server")
    (description
     "xf86-input-keyboard is a keyboard input driver for the Xorg X server.")
    (license license:x11)))


(define-public xf86-input-mouse
  (package
    (name "xf86-input-mouse")
    (version "1.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-mouse-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1iawr1wyl2qch1mqszcs0s84i92mh4xxprflnycbw1adc18b7v4k"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Mouse input driver for X server")
    (description
     "xf86-input-mouse is a mouse input driver for the Xorg X server.
This driver supports four classes of mice: serial, bus and PS/2 mice,
and additional mouse types supported by specific operating systems, such
as USB mice.")
    (license license:x11)))


(define-public xf86-input-synaptics
  (package
    (name "xf86-input-synaptics")
    (version "1.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-synaptics-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0xhm03qywwfgkpfl904d08lx00y28m1b6lqmks5nxizixwk3by3s"))))
    (build-system gnu-build-system)
    (inputs `(("libx11" ,libx11)
              ("libxi" ,libxi)
              ("libevdev" ,libevdev)
              ("mtdev" ,mtdev)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg")
             (string-append "--with-xorg-conf-dir="
                            (assoc-ref %outputs "out")
                            "/share/X11/xorg.conf.d"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Touchpad input driver for X server")
    (description
     "xf86-input-synaptics is a touchpad driver for the Xorg X server.")
    (license license:x11)))


(define-public xf86-input-void
  (package
    (name "xf86-input-void")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-void-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "171k8b8s42s3w73l7ln9jqwk88w4l7r1km2blx1vy898c854yvpr"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Void (null) input driver for X server")
    (description
     "xf86-input-void is a null input driver for the Xorg X server.")
    (license license:x11)))

(define-public xf86-video-amdgpu
  (package
    (name "xf86-video-amdgpu")
    (version "18.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-amdgpu-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "0wlnb929l3yqj4hdkzyxyhbaph13ac4villajgmbh66pa6xja7z1"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "AMD Radeon video driver for X server")
    (description
     "xf86-video-amdgpu is an AMD Radeon video driver for the Xorg
X server.")
    (license license:x11)))

(define-public xf86-video-ark
  (package
    (name "xf86-video-ark")
    (version "0.7.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-ark-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "07p5vdsj2ckxb6wh02s61akcv4qfg6s1d5ld3jn3lfaayd3f1466"))
        (patches (search-patches "xf86-video-ark-remove-mibstore.patch"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Ark Logic video driver for X server")
    (description
     "xf86-video-ark is an Ark Logic video driver for the Xorg X server.")
    (license license:x11)))

(define-public xf86-video-ast
  (package
    (name "xf86-video-ast")
    (version "1.1.5")
    (home-page "https://cgit.freedesktop.org/xorg/driver/xf86-video-ast/")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/driver/xf86-video-ast-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1pm2cy81ma7ldsw0yfk28b33h9z2hcj5rccrxhfxfgvxsiavrnqy"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (synopsis "ASpeed Technologies video driver for X server")
    (description
     "xf86-video-ast is an ASpeed Technologies video driver for the Xorg
X server.")
    (license license:x11)))

(define-public xf86-video-ati
  (package
    (name "xf86-video-ati")
    (version "18.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-ati-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "180l2yw8c63cbcs3zk729vx439aig1d7yicpyxj0nmfl4y0kpskj"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments `(#:configure-flags `("--disable-glamor"))) ; TODO: Enable glamor
    (home-page "https://www.x.org/wiki/")
    (synopsis "ATI Radeon video driver for X server")
    (description
     "xf86-video-ati is an ATI Radeon video driver for the Xorg
X server.")
    (license license:x11)))


(define-public xf86-video-cirrus
  (package
    (name "xf86-video-cirrus")
    (version "1.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-cirrus-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1asifc6ld2g9kap15vfhvsvyl69lj7pw3d9ra9mi4najllh7pj7d"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Cirrus Logic video driver for X server")
    (description
     "xf86-video-cirrus is a Cirrus Logic video driver for the Xorg
X server.")
    (license license:x11)))


;; non-free license
;; (define-public xf86-video-dummy


(define-public xf86-video-fbdev
  (package
    (name "xf86-video-fbdev")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-fbdev-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "16a66zr0l1lmssa07i3rzy07djxnb45c17ks8c71h8l06xgxihyw"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Framebuffer device video driver for X server")
    (description
     "xf86-video-fbdev is a video driver for the Xorg X server for
framebuffer device.")
    (license license:x11)))


(define-public xf86-video-freedreno
  (let ((commit "ccba8f89995de7d5e1b216e580b789c4cda05035"))
    (package
      (name "xf86-video-freedreno")
      (version (string-append "1.4.0-1-" (string-take commit 7)))
      (source
       (origin
         ;; there's no current tarball
         (method git-fetch)
         (uri (git-reference
               (url (string-append "https://anongit.freedesktop.org/git/xorg/"
                                   "driver/xf86-video-freedreno.git"))
               (commit commit)))
         (sha256
          (base32
           "0bl9m1agi793lcddv94j8afzw1xc9w810q91mbq0n3dscbbcr9nh"))
         (file-name (string-append name "-" version))))
      (build-system gnu-build-system)
      (inputs
       `(("libdrm" ,libdrm)
         ("mesa" ,mesa)
         ("udev" ,eudev)
         ("xorg-server" ,xorg-server)
         ("zlib" ,zlib)))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)))
       ;; This driver is only supported on ARM systems.
      (supported-systems '("armhf-linux" "aarch64-linux"))
      (arguments
       `(#:configure-flags
         (list (string-append "--with-xorg-conf-dir="
                              (assoc-ref %outputs "out")
                              "/share/X11/xorg.conf.d"))))
      (home-page "https://www.x.org/wiki/")
      (synopsis "Adreno video driver for X server")
      (description
       "xf86-video-freedreno is a 2D graphics driver for the Xorg X server.
It supports a variety of Adreno graphics chipsets.")
      (license license:x11))))


(define-public xf86-video-geode
  (package
    (name "xf86-video-geode")
    (version "2.11.19")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-geode-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "0zn9gb49grds5mcs1dlrx241k2w1sgqmx4i5x7v6159xxqhlqsf6"))
        (patches (search-patches "xf86-video-geode-glibc-2.20.patch"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (supported-systems
     ;; This driver is only supported on i686 systems.
     (filter (lambda (system) (string-prefix? "i686-" system))
             %supported-systems))
    (home-page "https://www.x.org/wiki/")
    (synopsis "AMD Geode GX/LX video driver for X server")
    (description
     "xf86-video-geode is an Xorg X server video driver for the AMD
Geode GX and LX processors.  The GX component supports both XAA and EXA
for graphics acceleration.  The LX component supports EXA, including
compositing.  Both support Xv overlay and dynamic rotation with XRandR.")
    (license license:x11)))


;; Driver for obsolete graphics cards, depends on libglide:
;; http://sourceforge.net/projects/glide/ ,
;; last updated in 2003, and which does not compile out of the box any more.
;; (define-public xf86-video-glide
;;   (package
;;     (name "xf86-video-glide")
;;     (version "1.2.0")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri (string-append
;;                "mirror://xorg/X11R7.7/src/everything/xf86-video-glide-"
;;                version
;;                ".tar.bz2"))
;;         (sha256
;;           (base32
;;             "0byapm9mnpqk3wijfnnan3d22ii5cw6dmg4xn1625iiz89j5vs1l"))))
;;     (build-system gnu-build-system)
;;     (inputs `(("pkg-config" ,pkg-config)
;;               ("xorg-server" ,xorg-server)))
;;     (home-page "https://www.x.org/wiki/")
;;     (synopsis "Glide video driver for X server")
;;     (description
;;      "xf86-video-glide is a Glide video driver for the Xorg X server.")
;;     (license license:x11)))


(define-public xf86-video-glint
  (package
    (name "xf86-video-glint")
    (version "1.2.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-glint-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1lkpspvrvrp9s539bhfdjfh4andaqyk63l6zjn8m3km95smk6a45"))))
    (build-system gnu-build-system)
    (inputs `(("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "GLINT/Permedia video driver for X server")
    (description
     "xf86-video-glint is a GLINT/Permedia video driver for the Xorg
X server.")
    (license license:x11)))


(define-public xf86-video-i128
  (package
    (name "xf86-video-i128")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-i128-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1snhpv1igrhifcls3r498kjd14ml6x2xvih7zk9xlsd1ymmhlb4g"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "I128 video driver for X server")
    (description
     "xf86-video-i128 is an I128 (Imagine 128) video driver for the Xorg
X server.")
    (license license:x11)))


(define-public xf86-video-intel
  (let ((commit "33ee0c3b21ea279e08d0863fcb2e874f0974b00e")
        (revision "12"))
    (package
      (name "xf86-video-intel")
      (version (git-version "2.99.917" revision commit))
      (source
       (origin
         ;; there's no current tarball
         (method git-fetch)
         (uri (git-reference
               (url "https://anongit.freedesktop.org/git/xorg/driver/xf86-video-intel.git")
               (commit commit)))
         (sha256
          (base32
           "1ryjaj52nd2fbrjf1id5fr5vndzvv287rggbj3lzhbwzxv52r0gj"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (inputs `(("mesa" ,mesa)
                ("udev" ,eudev)
                ("libx11" ,libx11)
                ("libxfont" ,libxfont2)
                ("xorg-server" ,xorg-server)))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)))
      (supported-systems
       ;; This driver is only supported on Intel systems.
       (filter (lambda (system) (or (string-prefix? "i686-" system)
                                    (string-prefix? "x86_64-" system)))
               %supported-systems))
      (arguments
       '(#:configure-flags '("--with-default-accel=uxa")
         #:phases (modify-phases %standard-phases
                    (add-before 'bootstrap 'pre-bootstrap
                      (lambda _
                        ;; Inhibit autogen.sh from running configure.
                        (setenv "NOCONFIGURE" "1")
                        #t)))))
      (home-page "https://www.x.org/wiki/")
      (synopsis "Intel video driver for X server")
      (description
       "xf86-video-intel is a 2D graphics driver for the Xorg X server.
It supports a variety of Intel graphics chipsets.")
      (license license:x11))))


(define-public xf86-video-mach64
  (package
    (name "xf86-video-mach64")
    (version "6.9.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-mach64-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "171wg8r6py1l138s58rlapin3rlpwsg9spmvhc7l68mm3g3hf1vs"))
        (patches (search-patches "xf86-video-mach64-glibc-2.20.patch"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Mach64 video driver for X server")
    (description
     "xf86-video-mach64 is a video driver for the Xorg X server.
This driver is intended for all ATI video adapters based on the Mach64
series or older chipsets, providing maximum video function within
hardware limitations.  The driver is also intended to optionally provide
the same level of support for generic VGA or 8514/A adapters.")
    (license license:x11)))


(define-public xf86-video-mga
  (package
    (name "xf86-video-mga")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-mga-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "0yaxpgyyj9398nzzr5vnsfxcis76z46p9814yzj8179yl7hld296"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Matrox video driver for X server")
    (description
     "xf86-video-mga is a Matrox video driver for the Xorg X server.")
    (license license:x11)))

(define-public xf86-video-neomagic
  (package
    (name "xf86-video-neomagic")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-neomagic-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0r4h673kw8fl7afc30anwbjlbhp82mg15fvaxf470xg7z983k0wk"))))
    (build-system gnu-build-system)
    (inputs `(("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "NeoMagic video driver for X server")
    (description
     "xf86-video-neomagic is a NeoMagic video driver for the Xorg X server.")
    (license license:x11)))


;; This driver depends on XAA which has been removed from xorg-server.

;; (define-public xf86-video-newport
;;   (package
;;     (name "xf86-video-newport")
;;     (version "0.2.4")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri (string-append
;;                "mirror://xorg/individual/driver/xf86-video-newport-"
;;                version
;;                ".tar.bz2"))
;;         (sha256
;;           (base32
;;             "1yafmp23jrfdmc094i6a4dsizapsc9v0pl65cpc8w1kvn7343k4i"))))
;;     (build-system gnu-build-system)
;;     (inputs `(("xorg-server" ,xorg-server)))
;;     (native-inputs `(("pkg-config" ,pkg-config)))
;;     (home-page "https://www.x.org/wiki/")
;;     (synopsis "Newport video driver for X server")
;;     (description
;;      "xf86-video-newport is an Xorg X server video driver for the SGI
;; newport cards.")
;;     (license license:x11)))


(define-public xf86-video-nv
  (package
    (name "xf86-video-nv")
    (version "2.1.21")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-nv-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "0bdk3pc5y0n7p53q4gc2ff7bw16hy5hwdjjxkm5j3s7hdyg6960z"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "NVIDIA video driver for X server")
    (description
     "This package contains Xorg support for the NVIDIA GeForce 8 series of
graphics processors.

There are a few caveats of which to be aware: the XVideo extension is not
supported, and the RENDER extension is not accelerated by this driver.")
    (license license:x11)))

(define-public xf86-video-nouveau
  (package
    (name "xf86-video-nouveau")
    (version "1.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://xorg/individual/driver/xf86-video-nouveau-"
             version
             ".tar.bz2"))
       (sha256
        (base32
         "01mz8gnq7j6bvrqb2ljm3d1wpjhi9p2z2w8zbkdrqmqmcj060h1h"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://nouveau.freedesktop.org")
    (synopsis "NVIDIA video driver for X server")
    (description
     "This package provides modern, high-quality Xorg drivers for NVIDIA
graphics cards.")
    (license license:x11)))

(define-public xf86-video-openchrome
  (package
    (name "xf86-video-openchrome")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-openchrome-"
               version
               ".tar.bz2"))
        (sha256
         (base32
          "0x9gq3hw6k661k82ikd1y2kkk4dmgv310xr5q59dwn4k6z37aafs"))))
    (build-system gnu-build-system)
    (inputs `(("libx11" ,libx11)
              ("libxext" ,libxext)
              ("libxvmc" ,libxvmc)
              ("mesa" ,mesa)
              ("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Openchrome video driver for X server")
    (description
     "xf86-video-openchrome is a video driver for the Xorg X server.
This driver is intended for VIA chipsets featuring the VIA UniChrome,
UniChrome Pro and Chrome9 integrated graphics processors.")
    (license license:x11)))


(define-public xf86-video-qxl
  (package
    (name "xf86-video-qxl")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "mirror://xorg/individual/driver/"
                "xf86-video-qxl-" version ".tar.bz2"))
              (sha256
               (base32
                "14jc24znnahhmz4kqalafmllsg8awlz0y6gpgdpk5ih38ph851mi"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxfont" ,libxfont2)
        ("spice-protocol" ,spice-protocol)
        ("xorg-server" ,xorg-server)
        ("xorgproto" ,xorgproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (synopsis "Qxl video driver for X server")
    (description "xf86-video-qxl is a video driver for the Xorg X server.
This driver is intended for the spice qxl virtio device.")
    (home-page "http://www.spice-space.org")
    (license license:x11)))

(define-public xf86-video-r128
  (package
    (name "xf86-video-r128")
    (version "6.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/driver/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0mz0v5mqmmbncr2drd5zvia1fb7frz2xqwflhhqbnaxx5j48c740"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "ATI Rage 128 video driver for X server")
    (description
     "xf86-video-r128 is a video driver for the Xorg X server.
This driver is intended for ATI Rage 128 based cards.")
    (license license:x11)))

(define-public xf86-video-savage
  (package
    (name "xf86-video-savage")
    (version "2.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-savage-"
               version
               ".tar.bz2"))
        (patches (search-patches "xf86-video-savage-xorg-compat.patch"))
        (sha256
          (base32
           "11pcrsdpdrwk0mrgv83s5nsx8a9i4lhmivnal3fjbrvi3zdw94rc"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Savage video driver for X server")
    (description
     "xf86-video-savage is an S3 Savage video driver for the Xorg X server.")
    (license license:x11)))


(define-public xf86-video-siliconmotion
  (package
    (name "xf86-video-siliconmotion")
    (version "1.7.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-siliconmotion-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1g2r6gxqrmjdff95d42msxdw6vmkg2zn5sqv0rxd420iwy8wdwyh"))
        (patches (search-patches "xf86-video-siliconmotion-fix-ftbfs.patch"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Silicon Motion video driver for X server")
    (description
     "xf86-video-siliconmotion is a Silicon Motion video driver for the
Xorg X server.")
    (license license:x11)))


(define-public xf86-video-sis
  (package
    (name "xf86-video-sis")
    (version "0.10.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-sis-"
               version
               ".tar.bz2"))
        (patches (search-patches "xf86-video-sis-xorg-compat.patch"))
        (sha256
          (base32
           "03f1abjjf68y8y1iz768rn95va9d33wmbwfbsqrgl6k0gi0bf9jj"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Sis video driver for X server")
    (description
     "xf86-video-SiS is a SiS video driver for the Xorg X server.
This driver supports SiS chipsets of 300/315/330/340 series.")
    (license license:bsd-3)))


(define-public xf86-video-suncg6
  (package
    (name "xf86-video-suncg6")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-suncg6-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "04fgwgk02m4nimlv67rrg1wnyahgymrn6rb2cjj1l8bmzkii4glr"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "GX/TurboGX video driver for X server")
    (description
     "xf86-video-suncg6 is a GX/TurboGX video driver for the Xorg X server.")
    (license license:x11)))


(define-public xf86-video-sunffb
  (package
    (name "xf86-video-sunffb")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-sunffb-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07z3ngifwg2d4jgq8pms47n5lr2yn0ai72g86xxjnb3k20n5ym7s"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "SUNFFB video driver for X server")
    (description
     "xf86-video-sunffb is a SUNFFB video driver for the Xorg X server.")
    (license license:x11)))


(define-public xf86-video-tdfx
  (package
    (name "xf86-video-tdfx")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-tdfx-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "0qc5wzwf1n65si9rc37bh224pzahh7gp67vfimbxs0b9yvhq0i9g"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "3Dfx video driver for X server")
    (description
     "xf86-video-tdfx is a 3Dfx video driver for the Xorg X server.")
    (license license:x11)))


(define-public xf86-video-tga
  (package
    (name "xf86-video-tga")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-tga-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "0cb161lvdgi6qnf1sfz722qn38q7kgakcvj7b45ba3i0020828r0"))
        (patches (search-patches "xf86-video-tga-remove-mibstore.patch"))))
    (build-system gnu-build-system)
    (inputs `(("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "TGA video driver for X server")
    (description
     "xf86-video-tga is a TGA (DEC 21030) video driver for the Xorg
X server.")
    (license license:x11)))


(define-public xf86-video-trident
  (package
    (name "xf86-video-trident")
    (version "1.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-trident-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "0gxcar434kx813fxdpb93126lhmkl3ikabaljhcj5qn3fkcijlcy"))))
    (build-system gnu-build-system)
    (inputs `(("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Trident video driver for X server")
    (description
     "xf86-video-trident is a Trident video driver for the Xorg X server.")
    (license license:x11)))


;; no license
;; (define-public xf86-video-v4l


(define-public xf86-video-vesa
  (package
    (name "xf86-video-vesa")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-vesa-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1373vsxn6qh00na0s9c09kf09gj78rzi98zq93id8v5zsya3qi5z"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "VESA video driver for X server")
    (description
     "xf86-video-vesa is a generic VESA video driver for the Xorg
X server.")
    (license license:x11)))


(define-public xf86-video-vmware
  (package
    (name "xf86-video-vmware")
    (version "13.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-vmware-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "0v06qhm059klq40m2yx4wypzb7h53aaassbjfmm6clcyclj1k5s7"))))
    (build-system gnu-build-system)
    (inputs
     `(("libx11" ,libx11)
       ("libxext" ,libxext)
       ("llvm" ,llvm)
       ("mesa" ,mesa)                   ; for xatracker
       ("xorg-server" ,xorg-server)))
    (native-inputs
     `(("eudev" ,eudev)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "VMware SVGA video driver for X server")
    (description
     "xf86-video-vmware is a VMware SVGA video driver for the Xorg X server.")
    (license license:x11)))


(define-public xf86-video-voodoo
  (package
    (name "xf86-video-voodoo")
    (version "1.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-voodoo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1s6p7yxmi12q4y05va53rljwyzd6ry492r1pgi7wwq6cznivhgly"))
        (patches
         (list (origin
                 (method url-fetch)
                 (uri "https://cgit.freedesktop.org/xorg/driver/\
xf86-video-voodoo/patch/?id=9172ae566a0e85313fc80ab62b4455393eefe593")
                 (sha256
                  (base32
                   "0rndmxf5b8j3hjnhrwrnzsq5024fli134fj1mprhkcrvax2zq8db"))
                 (file-name "xf86-video-voodoo-pcitag.patch"))))))
    (build-system gnu-build-system)
    (inputs `(("xorgproto" ,xorgproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Voodoo/Voodoo2 video driver for X server")
    (description
     "xf86-video-voodoo is a Voodoo video driver for the Xorg X server.")
    (license license:x11)))


;; Only relevant for the frame buffer on BSD systems.
;; (define-public xf86-video-wsfb


(define-public xf86bigfontproto
  (package
    (name "xf86bigfontproto")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xf86bigfontproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0j0n7sj5xfjpmmgx6n5x556rw21hdd18fwmavp95wps7qki214ms"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XF86BigFontProto protocol headers")
    (description
     "XFree86 Bigfont Extension contains header files and documentation
for the XF86BigFontProto protocol.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xf86dgaproto
  (package
    (name "xf86dgaproto")
    (version "2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xf86dgaproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0l4hx48207mx0hp09026r6gy9nl3asbq0c75hri19wp1118zcpmc"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XF86DGAProto protocol headers")
    (description
     "XFree86 Direct Graphic Access Extension defines a protocol for
direct linear framebuffer access.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xf86driproto
  (package
    (name "xf86driproto")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xf86driproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07v69m0g2dfzb653jni4x656jlr7l84c1k39j8qc8vfb45r8sjww"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XF86DRIProto protocol headers")
    (description
     "XFree86 Direct Rendering Infrastructure Extension defines a
protocol to allow user applications to access the video hardware without
requiring data to be passed through the X server.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xf86vidmodeproto
  (package
    (name "xf86vidmodeproto")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xf86vidmodeproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0w47d7gfa8zizh2bshdr2rffvbr4jqjv019mdgyh6cmplyd4kna5"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XF86VidModeProto protocol headers")
    (description
     "XFree86 Video Mode Extension defines a protocol for dynamically
configuring modelines and gamma.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xgamma
  (package
    (name "xgamma")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xgamma-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1lr2nb1fhg5fk2fchqxdxyl739602ggwhmgl2wiv5c8qbidw7w8f"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxxf86vm" ,libxxf86vm)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Alter a monitor's gamma correction")
    (description
     "XGamma is used to query and alter the gamma correction of a
monitor via the X video mode extension.")
    (license license:x11)))


(define-public xhost
  (package
    (name "xhost")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xhost-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "15n3mnd4i5kh4z32qv11580qjgvnng0wry2y753ljrqkkrbkrp52"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libxau" ,libxau)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xhost")
    (synopsis "Xorg server access control utility")
    (description
     "XHost is used to manage the list of host names or user names
allowed to make connections to the X server.")
    (license license:x11)))


(define-public xineramaproto
  (package
    (name "xineramaproto")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xineramaproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ns8abd27x7gbp4r44z3wc5k9zqxxj8zjnazqpcyr4n17nxp8xcp"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XineramaProto protocol headers")
    (description
     "Xinerama Extension allows clients to query information about multiple
physical screens controlled by a single X server that appear as a single
screen to core X11 protocol operations.

This extension provides a common network protocol for querying information
which may be provided by different underlying screen combination
technologies in the X server, such as the original Xinerama multiplexer, or
alternative implementations like XRandR or TwinView.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xinput
  (package
    (name "xinput")
    (version "1.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xinput-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1i75mviz9dyqyf7qigzmxq8vn31i86aybm662fzjz5c086dx551n"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("libxrandr" ,libxrandr)
        ("libxinerama" ,libxinerama)
        ("libxext" ,libxext)
        ("libxi" ,libxi)
        ("libx11" ,libx11)
        ("xorgproto" ,xorgproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Configure input devices for X server")
    (description
     "XInput is used to configure and test XInput devices.")
    (license license:x11)))


(define xkbcomp-intermediate ; used as input for xkeyboard-config
  (package
    (name "xkbcomp-intermediate")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xkbcomp-"
               version
               ".tar.bz2"))
        (sha256
         (base32
          "0944rrkkf0dxp07vhh9yr4prslxhqyw63qmbjirbv1bypswvrn3d"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)
        ("libxkbfile" ,libxkbfile)
        ("libx11" ,libx11)))
    (native-inputs
        `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Compile XKB keyboard description")
    (description
     "xkbcomp keymap compiler converts a description of an XKB keymap
into one of several output formats.  The most common use for xkbcomp is
to create a compiled keymap file (.xkm extension) which can be read
directly by XKB- capable X servers or utilities.

X Keyboard (XKB) Extension essentially replaces the core protocol
definition of keyboard.  The extension makes possible to clearly and
explicitly specify most aspects of keyboard behaviour on per-key basis
and to more closely track the logical and physical state of the
keyboard.  It also includes a number of keyboard controls designed to
make keyboards more accessible to people with physical impairments.")
    (license license:x11)))

(define-public xkbcomp ; using xkeyboard-config as input
  (package (inherit xkbcomp-intermediate)
    (name "xkbcomp")
    (inputs
      `(,@(package-inputs xkbcomp-intermediate)
        ("xkeyboard-config" ,xkeyboard-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-xkb-config-root="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb"))))))


(define-public xkbevd
  (package
    (name "xkbevd")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xkbevd-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0sprjx8i86ljk0l7ldzbz2xlk8916z5zh78cafjv8k1a63js4c14"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxkbfile" ,libxkbfile)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "XKB event daemon demo")
    (description
     "XKB event daemon listens for the specified XKB events and executes
requested commands if they occur.")
    (license license:x11)))


(define-public xkbutils
  (package
    (name "xkbutils")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xkbutils-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0c412isxl65wplhl7nsk12vxlri29lk48g3p52hbrs3m0awqm8fj"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("libxaw" ,libxaw)
        ("xorgproto" ,xorgproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "XKB utilities")
    (description
     "XKBUtils is a collection of small utilities for X Keyboard (XKB)
extension to the X11 protocol.  It includes:

- xkbbell: generate XKB bell events;

- xkbvleds: display the state of LEDs on an XKB keyboard in a window;

- xkbwatch: reports changes in the XKB keyboard state.")
    (license license:x11)))


(define-public xkeyboard-config
  (package
    (name "xkeyboard-config")
    (version "2.26")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "mirror://xorg/individual/data/xkeyboard-config/xkeyboard-config-"
              version
              ".tar.bz2"))
        (sha256
          (base32
            "13h3381pfp4pv32189zkfsj2x0alr91xj6dqii76rl0c8v3ihdrr"))))
    (build-system gnu-build-system)
    (inputs
      `(("gettext" ,gettext-minimal)
        ("libx11" ,libx11)
        ("xkbcomp-intermediate" ,xkbcomp-intermediate)))
    (native-inputs
      `(("intltool" ,intltool)
        ("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XKB configuration files")
    (description
     "xkeyboard-config provides a database for X Keyboard (XKB) Extension.
There are five components that define a complete keyboard mapping:
symbols, geometry, keycodes, compat, and types; these five components
can be combined together using the 'rules' component of this database.")
    (license license:x11)))


(define-public xkill
  (package
    (name "xkill")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xkill-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0szzd9nzn0ybkhnfyizb876irwnjsnb78rcaxx6prb71jmmbpw65"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libx11" ,libx11)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Kill a client by its X resource")
    (description
     "XKill is used to force the X server to close connections to
clients.  This program is very dangerous, but is useful for aborting
programs that have displayed undesired windows on a user's screen.")
    (license license:x11)))


(define-public xlsatoms
  (package
    (name "xlsatoms")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xlsatoms-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "10m3a046jvaw5ywx4y65kl84lsxqan70gww1g1r7cf96ijaqz1jp"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxcb" ,libxcb)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xlsatoms")
    (synopsis "List interned X server atoms")
    (description
     "XLsAtoms is used to list the interned atoms defined on X server.")
    (license license:x11)))


(define-public xlsclients
  (package
    (name "xlsclients")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xlsclients-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1h8931sn34mcip6vpi4v7hdmr1r58gkbw4s2p97w98kykks2lgvp"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxcb" ,libxcb)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "List client applications running on a display")
    (description
     "XLsClients is used to list information about the client programs
running on X server.")
    (license license:x11)))


(define-public xlsfonts
  (package
    (name "xlsfonts")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://xorg/individual/app/xlsfonts-"
             version
             ".tar.bz2"))
       (sha256
        (base32
         "0s6kxgv78chkwsqmhw929f4pf91gq63f4yvixxnan1h00cx0pf49"))))
    (build-system gnu-build-system)
    (inputs
     `(("xorgproto" ,xorgproto)
       ("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "List fonts available from an X server")
    (description
     "xlsfonts lists fonts available from an X server via the X11 core
protocol.")
    (license license:x11)))

(define-public xfontsel
  (package
    (name "xfontsel")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/app/xfontsel-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0700lf6hx7dg88wq1yll7zjvf9gbwh06xff20yffkxb289y0pai5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-appdefaultdir="
                            %output ,%app-defaults-dir))))
    (inputs
     `(("libx11" ,libx11)
       ("libxaw" ,libxaw)
       ("libxmu" ,libxmu)
       ("libxt" ,libxt)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Browse and select X font names")
    (description
     "XFontSel provides a simple way to display the X11 core protocol fonts
known to your X server, examine samples of each, and retrieve the X Logical
Font Description (XLFD) full name for a font.")
    (license license:x11)))

(define-public xfd
  (package
    (name "xfd")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/app/xfd-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0n6r1v8sm0z0ycqch035xpm46nv5v4mav3kxh36883l3ln5r6bqr"))))
    (build-system gnu-build-system)
    (arguments
       `(#:configure-flags
         (list (string-append "--with-appdefaultdir="
                              %output ,%app-defaults-dir))))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("libx11" ,libx11)
       ("libxaw" ,libxaw)
       ("libxft" ,libxft)
       ("libxmu" ,libxmu)
       ("libxrender" ,libxrender)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Display all the characters in an X font")
    (description
     "XFD (X Font Display) package provides an utility that displays a
window containing the name of the font being displayed, a row of command
buttons, several lines of text for displaying character metrics, and a grid
containing one glyph per cell.")
    (license license:x11)))

(define-public xmodmap
  (package
    (name "xmodmap")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xmodmap-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0z28331i2pm16x671fa9qwsfqdmr6a43bzwmp0dm17a3sx0hjgs7"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xmodmap")
    (synopsis "Modify keymaps and button mappings on X server")
    (description
     "Xmodmap is used to display and edit the keyboard modifier map and
keymap table that are used by client programs running on X server to
convert event keycodes into keysyms.  It is usually run from the user's
session startup script to configure the keyboard according to personal
tastes.")
    (license license:x11)))


;; no license
;; (define-public xorg-docs


(define-public xorg-sgml-doctools
  (package
    (name "xorg-sgml-doctools")
    (version "1.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/doc/xorg-sgml-doctools-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0k5pffyi5bx8dmfn033cyhgd3gf6viqj3x769fqixifwhbgy2777"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg SGML documentation tools")
    (description
    "This package provides a common set of SGML entities and XML/CSS style
sheets used in building/formatting the documentation provided in other
Xorg packages.  It's typically only needed by people building from
source who want to produce formatted documentation from their builds, or
those who have installed the HTML version of the documentation, which
refers to the included common xorg.css stylesheet.")
    (license license:x11)))


(define-public xpr
  (package
    (name "xpr")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xpr-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07qy9lwjvxighcmg6qvjkgagad3wwvidrfx0jz85lgynz3qy0dmr"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)
        ("libxmu" ,libxmu)
        ("libx11" ,libx11)))
    (native-inputs
        `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Print an X window dump from xwd")
    (description
     "XPr takes as input a window dump file produced by xwd and formats
it for output on various types of printers.")
    (license license:x11)))


(define-public xprop
  (package
    (name "xprop")
    (version "1.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xprop-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0lzp7kyhpwd5hm83j2zm6j3w3z1z5i4ykgg2nwr01ij6dq4znxwc"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Display X server properties")
    (description
     "xprop is used to display and/or set window and font properties of
an X server.")
    (license license:x11)))


(define-public xrandr
  (package
    (name "xrandr")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xrandr-"
               version ".tar.bz2"))
        (sha256
          (base32
            "1kaih7rmzxr1vp5a5zzjhm5x7dn9mckya088sqqw026pskhx9ky1"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("libxrandr" ,libxrandr)
        ("xorgproto" ,xorgproto)
        ("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Command line interface to X RandR extension")
    (description
     "xrandr - primitive command line interface to X11 Resize, Rotate,
and Reflect (RandR) extension.")
    (license license:x11)))


(define-public xrdb
  (package
    (name "xrdb")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xrdb-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ik9gh6363c47pr0dp7q22nfs8vmavjg2v4bsr0604ppl77nafpj"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xrdb")
    (synopsis "X server resource database utility")
    (description
     "XRDB is used to get or set the contents of the RESOURCE_MANAGER
property on the root window of screen 0, or the SCREEN_RESOURCES
property on the root window of any or all screens, or everything
combined.  You would normally run this program from your X startup
file.")
    (license license:x11)))


(define-public xrefresh
  (package
    (name "xrefresh")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xrefresh-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0lv3rlshh7s0z3aqx5ahnnf8cl082m934bk7gv881mz8nydznz98"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Refresh all or part of an X screen")
    (description
     "Xrefresh is a simple X program that causes all or part of your
screen to be repainted.  This is useful when system messages have messed
up your screen.")
    (license license:x11)))


(define-public xset
  (package
    (name "xset")
    (version "1.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xset-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0my987wjvra7l92ry6q44ky383yg3phzxhdbn3lqhapm1ll9bzg4"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)
        ("libxmu" ,libxmu)
        ("libxext" ,libxext)
        ("libx11" ,libx11)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "User preference utility for X server")
    (description
     "XSet is used to set various user preference options of the display.")
    (license license:x11)))


(define-public xsetroot
  (package
    (name "xsetroot")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xsetroot-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0z21mqvmdl6rl63q77479wgkfygnll57liza1i3va7sr4fx45i0h"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libxcursor" ,libxcursor)
        ("xbitmaps" ,xbitmaps)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Root window parameter setting utility for X server")
    (description
     "XSetRoot allows you to tailor the appearance of the root window on
a display running X server.")
    (license license:x11)))


(define-public xtrans
  (package
    (name "xtrans")
    (version "1.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/xtrans-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "00c3ph17acnsch3gbdmx33b9ifjnl5w7vx8hrmic1r1cjcv3pgdd"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Network Transport layer library")
    (description
     "Xtrans is a library of code that is shared among various X packages to
handle network protocol transport in a modular fashion, allowing a single
place to add new transport types.  It is used by the X server, libX11,
libICE, the X font server, and related components.")
    (license license:x11)))


(define-public xvinfo
  (package
    (name "xvinfo")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xvinfo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0gz7fvxavqlrqynpfbrm2nc9yx8h0ksnbnv34fj7n1q6cq6j4lq3"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxext" ,libxext)
        ("libxv" ,libxv)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xvinfo")
    (synopsis "Print out X-Video extension adaptor information")
    (description
     "XVInfo is used to print out the capabilities of any video adaptors
associated with the display that are accessible through the X-Video
extension.")
    (license license:x11)))


(define-public xwd
  (package
    (name "xwd")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xwd-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1537i8q8pgf0sjklakzfvjwrq5b246qjywrx9ll8xfg0p6w1as6d"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("libxkbfile" ,libxkbfile)
        ("xorgproto" ,xorgproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Dump current contents of X window or screen to file")
    (description
     "Xwd is used to store window images in a specially formatted dump
file.  This file can then be read by various other X utilities for
redisplay, printing, editing, formatting, archiving, image processing,
etc.  The target window is selected by clicking the pointer in the
desired window.  The keyboard bell is rung once at the beginning of the
dump and twice whenthe dump is completed.")
    (license license:x11)))


(define-public xwininfo
  (package
    (name "xwininfo")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xwininfo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "00avrpw4h5mr1klp41lv2j4dmq465v6l5kb5bhm4k5ml8sm9i543"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)
        ("xorgproto" ,xorgproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Window information utility for X server")
    (description
     "XWinInfo is used to print out information about windows on an X server.
Various information is displayed depending on which options are selected.")
    (license license:x11)))


(define-public xwud
  (package
    (name "xwud")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xwud-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1a8hdgy40smvblnh3s9f0vkqckl68nmivx7d48zk34m8z18p16cr"))))
    (build-system gnu-build-system)
    (inputs
      `(("xorgproto" ,xorgproto)
        ("libx11" ,libx11)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Display an X window dump from xwd")
    (description
     "Xwud is used to display in a window an image saved in a specially
formatted dump file, such as produced by xwd.")
    (license license:x11)))

(define-public xorg-rgb
  (package
    (name "xorg-rgb")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/rgb-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1c76zcjs39ljil6f6jpx1x17c8fnvwazz7zvl3vbjfcrlmm7rjmv"))))
    (build-system gnu-build-system)
    (inputs
     `(("xorgproto" ,xorgproto)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "X color name database")
    (description
     "This package provides the X color name database.")
    (license license:x11)))

;; packages of height 1 in the propagated-inputs tree

(define-public fixesproto
  (package
    (name "fixesproto")
    (version "5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/fixesproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1ki4wiq2iivx5g4w5ckzbjbap759kfqd72yg18m3zpbb4hqkybxs"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xorgproto" ,xorgproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg FixesProto protocol headers")
    (description
     "Fixes Extension makes changes to many areas of the protocol to resolve
issues raised by application interaction with core protocol mechanisms that
cannot be adequately worked around on the client side of the wire.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public libxdamage
  (package
    (name "libxdamage")
    (version "1.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXdamage-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0igaw2akjf712y3rv7lx473jigxmcv9rs9y8sbrvbhya8f30cd5p"))))
    (build-system gnu-build-system)
    (propagated-inputs
      ;; These are all in the Requires or Requires.private field of xdamage.pc
      `(("libxfixes" ,libxfixes)
        ("xorgproto" ,xorgproto)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Damage Extension library")
    (description "Xorg library for the XDamage extension.")
    (license license:x11)))


(define-public libxext
  (package
    (name "libxext")
    (version "1.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXext-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0dbfn5bznnrhqzvkrcmw4c44yvvpwdcsrvzxf4rk27r36b9x865m"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xorgproto" ,xorgproto)))
    (inputs
      `(("libxau" ,libxau)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Common extensions library")
    (description
     "Library for common extensions to the X11 protocol.")
    (license license:x11)))


(define-public libxinerama
  (package
    (name "libxinerama")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXinerama-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "086p0axqj57nvkaqa6r00dnr9kyrn1m8blgf0zjy25zpxkbxn200"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xorgproto" ,xorgproto)))
    (inputs
      `(("libxext" ,libxext)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Xinerama protocol library")
    (description "API for Xinerama extension to X11 protocol.")
    (license license:x11)))


(define-public libxp
  (package
    (name "libxp")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXp-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0mwc2jwmq03b1m9ihax5c6gw2ln8rc70zz4fsj3kb7440nchqdkz"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("printproto" ,printproto)))
    (inputs
      `(("libx11" ,libx11)
        ("libxext" ,libxext)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Print Client library")
    (description "Xorg Print Client library.")
    (license license:x11)))


(define-public libxrender
  (package
    (name "libxrender")
    (version "0.9.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXrender-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0j89cnb06g8x79wmmnwzykgkkfdhin9j7hjpvsxwlr3fz1wmjvf0"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xorgproto" ,xorgproto)))
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Render Extension library")
    (description "Library for the Render Extension to the X11 protocol.")
    (license license:x11)))

(define-public libxtst
  (package
    (name "libxtst")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXtst-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "012jpyj7xfm653a9jcfqbzxyywdmwb2b5wr1dwylx14f3f54jma6"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libxi" ,libxi)
       ("xorgproto" ,xorgproto)))
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg library for Xtest and Record extensions")
    (description
     "libXtst provides the Xlib-based client API for the XTEST & RECORD
extensions.

The XTEST extension is a minimal set of client and server extensions
required to completely test the X11 server with no user intervention.  This
extension is not intended to support general journaling and playback of user
actions.

The RECORD extension supports the recording and reporting of all core X
protocol and arbitrary X extension protocol.")
    (license license:x11)))

(define-public libxv
  (package
    (name "libxv")
    (version "1.0.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXv-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "125hn06bd3d8y97hm2pbf5j55gg4r2hpd3ifad651i4sr7m16v6j"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("xorgproto" ,xorgproto)))
    (inputs
      `(("libxext" ,libxext)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XVideo Extension library")
    (description "Library for the X Video Extension to the X11 protocol.")
    (license license:x11)))

(define-public mkfontdir
  (package
    (name "mkfontdir")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/mkfontdir-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0c3563kw9fg15dpgx4dwvl12qz6sdqdns1pxa574hc7i5m42mman"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-mkfontdir
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/mkfontdir")
               `("PATH" ":" prefix
                 (,(string-append (assoc-ref inputs "mkfontscale")
                                  "/bin"))))
             #t)))))
    (inputs
      `(("mkfontscale" ,mkfontscale)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Create an index of X font files in a directory")
    (description
     "MkFontDir creates the 'fonts.dir' files needed by the legacy X
server core font system.  The current implementation is a simple wrapper
script around the mkfontscale program.")
    (license license:x11)))

(define-public xproto
  (package
    (name "xproto")
    (version "7.0.31")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ivpxz0rx2a7nahkpkhfgymz7j0pwzaqvyqpdgw9afmxl1yp9yf6"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("util-macros" ,util-macros))) ; to get util-macros in (almost?) all package inputs
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg X11Proto protocol headers")
    (description
     "XProto provides the headers and specification documents defining
the X Window System Core Protocol, Version 11.

It also includes a number of headers that aren't purely protocol related,
but are depended upon by many other X Window System packages to provide
common definitions and porting layer.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

;; packages of height 2 in the propagated-inputs tree

(define-public libice
  (package
    (name "libice")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libICE-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "00p2b6bsg6kcdbb39bv46339qcywxfl4hsrz8asm4hy6q7r34w4g"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xorgproto" ,xorgproto)))
    (inputs
      `(("libbsd" ,libbsd)
        ("xtrans" ,xtrans)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Inter-Client Exchange library")
    (description "Xorg Inter-Client Exchange library.")
    (license license:x11)))


(define-public libxau
  (package
    (name "libxau")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXau-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1v3krc6x0zliaa66qq1bf9j60x5nqfy68v8axaiglxpnvgqcpy6c"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xorgproto" ,xorgproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Authorization library")
    (description
     "libXau provides an authorization library for individual access to
an X Window System display.")
    (license license:x11)))

(define-public libxfixes
  (package
    (name "libxfixes")
    (version "5.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXfixes-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1miana3y4hwdqdparsccmygqr3ic3hs5jrqfzp70hvi2zwxd676y"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xorgproto" ,xorgproto)))
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Fixes Extension library")
    (description "Library for the XFixes Extension to the X11 protocol.")
    (license license:x11)))

(define-public libxfont
  (package
    (name "libxfont")
    (version "1.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXfont-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0hiji1bvpl78aj3a3141hkk353aich71wv8l5l2z51scfy878zqs"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("freetype" ,freetype)
        ("libfontenc" ,libfontenc)
        ("xorgproto" ,xorgproto)))
    (inputs
      `(("zlib" ,zlib)
        ("xtrans" ,xtrans)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Font handling library")
    (description
     "libXfont provides the core of the legacy X11 font system, handling the
index files (fonts.dir, fonts.alias, fonts.scale), the various font file
formats, and rasterizing them.  It is used by the X servers, the X Font
Server (xfs), and some font utilities (bdftopcf for instance), but should
not be used by normal X11 clients.  X11 clients access fonts via either the
new API's in libXft, or the legacy API's in libX11.")
    (license license:x11)))

(define-public libxfont2
  (package
    (inherit libxfont)
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/lib/libXfont2-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0klwmimmhm3axpj8pwn5l41lbggh47r5aazhw63zxkbwfgyvg2hf"))))))

(define-public libxi
  (package
    (name "libxi")
    (version "1.7.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXi-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0idg1wc01hndvaa820fvfs7phvd1ymf0lldmq6386i7rhkzvirn2"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xorgproto" ,xorgproto)
        ("libx11" ,libx11)
        ("libxext" ,libxext)
        ("libxfixes" ,libxfixes)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Input Extension library")
    (description "Library for the XInput Extension to the X11 protocol.")
    (license license:x11)))

(define-public libxrandr
  (package
    (name "libxrandr")
    (version "1.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXrandr-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "06pmphx8lp3iywqnh88fvbfb0d8xgkx0qpvan49akpja1vxfgy8z"))))
    (build-system gnu-build-system)
    (propagated-inputs
      ;; In accordance with xrandr.pc.
      `(("libx11" ,libx11)
        ("libxext" ,libxext)
        ("libxrender" ,libxrender)
        ("xorgproto" ,xorgproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Resize and Rotate Extension library")
    (description
     "Library for the Resize and Rotate Extension to the X11 protocol.")
    (license license:x11)))

(define-public libxvmc
  (package
    (name "libxvmc")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXvMC-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0bpffxr5dal90a8miv2w0rif61byqxq2f5angj4z1bnznmws00g5"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libxv" ,libxv)))
    (inputs
      `(("xorgproto" ,xorgproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XvMC library")
    (description "Xorg XvMC library.")
    (license license:x11)))

(define-public libxxf86vm
  (package
    (name "libxxf86vm")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXxf86vm-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0mydhlyn72i7brjwypsqrpkls3nm6vxw0li8b2nw0caz7kwjgvmg"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libxext" ,libxext)
        ("xorgproto" ,xorgproto)))
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XF86 Video Mode Extension library")
    (description
     "Library for the XFree86 Video Mode Extension Extension to the X11
protocol.")
    (license license:x11)))


;; packages of height 3 in the propagated-inputs tree

(define-public libxcb
  (package
    (name "libxcb")
    (version "1.13")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://xcb.freedesktop.org/dist/"
                            name "-" version ".tar.bz2"))
        (sha256
          (base32
           "1ahxhmdqp4bhb90zmc275rmf5wixqra4bnw9pqnzyl1w3598g30q"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libpthread-stubs" ,libpthread-stubs)
        ("libxau" ,libxau)
        ("libxdmcp" ,libxdmcp)))
    (inputs
      `(("xcb-proto" ,xcb-proto)
        ("libxslt" ,libxslt)))
    (native-inputs
      `(("pkg-config" ,pkg-config)
        ("python" ,python-minimal-wrapper)))
    (arguments
     `(#:configure-flags '("--enable-xkb")))
    (home-page "https://xcb.freedesktop.org/")
    (synopsis "The X C Binding (XCB) library")
    (description
     "libxcb provides an interface to the X Window System protocol,
which replaces the current Xlib interface.  It has several advantages
over Xlib, including:

- size: small, simple library, and lower memory footprint;

- latency hiding: batch several requests and wait for the replies later;

- direct protocol access: interface and protocol correspond exactly;

- proven thread support: transparently access XCB from multiple threads;

- easy extension implementation: interfaces auto-generated from XML-XCB.")
    (license license:x11)))


(define-public xorg-server
  (package
    (name "xorg-server")
    (version "1.20.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "mirror://xorg/individual/xserver/"
              "xorg-server-" version ".tar.bz2"))
        (sha256
         (base32
          "1vk6j7hmigfill9x8m7a6vvgb3s50ji2yf6yprbgqfz9xf9x83zy"))
        (patches
         (list
          ;; See:
          ;;   https://lists.fedoraproject.org/archives/list/devel@lists.
          ;;      fedoraproject.org/message/JU655YB7AM4OOEQ4MOMCRHJTYJ76VFOK/
          (origin
            (method url-fetch)
            (uri (string-append
                  "http://pkgs.fedoraproject.org/cgit/rpms/xorg-x11-server.git"
                  "/plain/06_use-intel-only-on-pre-gen4.diff"))
            (sha256
             (base32
              "0mm70y058r8s9y9jiv7q2myv0ycnaw3iqzm7d274410s0ik38w7q"))
            (file-name "xorg-server-use-intel-only-on-pre-gen4.diff"))))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libpciaccess" ,libpciaccess)
        ("mesa" ,mesa)
        ("pixman" ,pixman)
        ("xorgproto" ,xorgproto)))
    (inputs
      `(("udev" ,eudev)
        ("dbus" ,dbus)
        ("libdmx" ,libdmx)
        ("libepoxy" ,libepoxy)
        ("libgcrypt" ,libgcrypt)
        ("libxau" ,libxau)
        ("libxaw" ,libxaw)
        ("libxdmcp" ,libxdmcp)
        ("libxfixes" ,libxfixes)
        ("libxfont2" ,libxfont2)
        ("libxkbfile" ,libxkbfile)
        ("libxrender" ,libxrender)
        ("libxres" ,libxres)
        ("libxshmfence" ,libxshmfence)
        ("libxt" ,libxt)
        ("libxv" ,libxv)
        ("xkbcomp" ,xkbcomp)
        ("xkeyboard-config" ,xkeyboard-config)
        ("xtrans" ,xtrans)
        ("zlib" ,zlib)
        ;; Inputs for Xephyr
        ("xcb-util" ,xcb-util)
        ("xcb-util-image" ,xcb-util-image)
        ("xcb-util-keysyms" ,xcb-util-keysyms)
        ("xcb-util-renderutil" ,xcb-util-renderutil)
        ("xcb-util-wm" ,xcb-util-wm)))
    (native-inputs
     `(("python" ,python-wrapper)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:parallel-tests? #f
       #:configure-flags
       (list (string-append "--with-xkb-path="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb")
             (string-append "--with-xkb-output="
                            "/tmp") ; FIXME: This is a bit doubtful; where should
                                    ; the compiled keyboard maps go?
             (string-append "--with-xkb-bin-directory="
                            (assoc-ref %build-inputs "xkbcomp")
                            "/bin")
             ;; By default, it ends up with invalid '${prefix}/...', causes:
             ;;   _FontTransOpen: Unable to Parse address ${prefix}/share/...
             ;; It's not used anyway, so set it to empty.
             "--with-default-font-path="

             ;; The default is to use "uname -srm", which captures the kernel
             ;; version and makes builds non-reproducible.
             "--with-os-name=GNU"

             "--with-os-vendor=GuixSD"    ;not strictly needed, but looks nice


             ;; For the log file, etc.
             "--localstatedir=/var"
             ;; For sddm
             "--enable-kdrive"
             "--enable-xephyr")

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda _
             (substitute* (find-files "." "\\.c$")
               (("/bin/sh") (which "sh")))

             ;; Don't try to 'mkdir /var'.
             (substitute* "hw/xfree86/Makefile.in"
               (("\\$\\(MKDIR_P\\).*logdir.*")
                "true\n"))

             ;; Strip timestamps that would otherwise end up in the 'Xorg'
             ;; binary.
             (substitute* "configure"
               (("^BUILD_DATE=.*$")
                "BUILD_DATE=19700101\n")
               (("^BUILD_TIME=.*$")
                "BUILD_TIME=000001\n"))

             #t)))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description
     "This package provides the Xorg X server itself.
The X server accepts requests from client programs to create windows, which
are (normally rectangular) 'virtual screens' that the client program can
draw into.

Windows are then composed on the actual screen by the X server (or by a
separate composite manager) as directed by the window manager, which usually
communicates with the user via graphical controls such as buttons and
draggable titlebars and borders.")
    (license license:x11)))

;; This package is intended to be used when building GTK+.
;; Note: It's currently marked as "hidden" to avoid having two non-eq?
;; packages with the same name and version.
(define-public xorg-server-for-tests
  (hidden-package
   (package
     (inherit xorg-server))))

(define-public xorg-server-xwayland
  (package
    (inherit xorg-server)
    (name "xorg-server-xwayland")
    (inputs
     `(("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)
       ,@(package-inputs xorg-server)))
    (arguments
     (substitute-keyword-arguments (package-arguments xorg-server)
       ((#:configure-flags flags)
        `(cons* "--enable-xwayland" "--disable-xorg"
                "--disable-docs"    "--disable-devel-docs"
                "--disable-xvfb"    "--disable-xnest"
                "--disable-xquartz" "--disable-xwin"
                ,flags))))
    (synopsis "Xorg server with wayland backend")))


;; packages of height 4 in the propagated-inputs tree

(define-public libx11
  (package
    (name "libx11")
    (version "1.6.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libX11-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0j0k5bjz4kd7rx6z09n5ggxbzbi84wf78xx25ikx6jmsxwq9w3li"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;8 MiB of man pages + XML
    (arguments
     '(#:configure-flags (list (string-append "--mandir="
                                              (assoc-ref %outputs "doc")
                                              "/share/man"))))
    (propagated-inputs
      `(("xorgproto" ,xorgproto)
        ("libxcb" ,libxcb)))
    (inputs
      `(("xtrans" ,xtrans)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Core X11 protocol client library")
    (description "Xorg Core X11 protocol client library.")
    (license license:x11)))

;; packages of height 5 in the propagated-inputs tree

(define-public libxcursor
  (package
    (name "libxcursor")
    (version "1.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXcursor-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0syzlfvh29037p0vnlc8f3jxz8nl55k65blswsakklkwsc6nfki9"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libx11" ,libx11)
        ("libxrender" ,libxrender)
        ("libxfixes" ,libxfixes)
        ("xorgproto" ,xorgproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
;; TODO: add XCURSOR_PATH=.../share/icons to profile search paths, so
;; libXcursor finds cursors installed into a profile.  If we solve bugs
;; <http://bugs.gnu.org/20255> and <http://bugs.gnu.org/22138>, we can fix
;; this with a search-path as follows:
;;
;;    (native-search-paths
;;     (list (search-path-specification
;;            (variable "XCURSOR_PATH")
;;            (files '("share/icons")))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Cursor management library")
    (description "Xorg Cursor management library.")
    (license license:x11)))

(define-public libxt
  (package
    (name "libxt")
    (version "1.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXt-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "06lz6i7rbrp19kgikpaz4c97fw7n31k2h2aiikczs482g2zbdvj6"))
        (patches (search-patches "libxt-guix-search-paths.patch"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;2 MiB of man pages + XML
    (arguments
     '(#:configure-flags (list (string-append "--mandir="
                                              (assoc-ref %outputs "doc")
                                              "/share/man"))))
    (propagated-inputs
      `(("libx11" ,libx11)
        ("libice" ,libice)
        ("libsm" ,libsm)))
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XToolkit Intrinsics library")
    (description "Xorg XToolkit Intrinsics library.")
    (license license:x11)))


(define-public libxaw
  (package
    (name "libxaw")
    (version "1.0.13")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXaw-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1kdhxplwrn43d9jp3v54llp05kwx210lrsdvqb6944jp29rhdy4f"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libxext" ,libxext)
        ("libxmu" ,libxmu)
        ("libxpm" ,libxpm)
        ("libxt" ,libxt)))
    (inputs
      `(("xorgproto" ,xorgproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Xaw library")
    (description
     "Xaw is the X Athena Widget Set based on the X Toolkit
Intrinsics (Xt) Library.")
    (license license:x11)))

(define-public twm
  (package
    (name "twm")
    (version "1.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://xorg/individual/app/" name "-"
             version
             ".tar.bz2"))
       (sha256
        (base32
         "1ms5cj1w3g26zg6bxdv1j9hl0pxr4300qnv003cz1q3cl7ffljb4"))))
    (build-system gnu-build-system)
    (inputs
     `(("libxt" ,libxt)
       ("libxmu" ,libxmu)
       ("libxext" ,libxext)
       ("xorgproto" ,xorgproto)))
    (native-inputs
     `(("bison" ,bison)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Tab Window Manager for the X Window System")
    (description "Twm is a window manager for the X Window System.
It provides titlebars, shaped windows, several forms of icon management,
user-defined macro functions, click-to-type and pointer-driven
keyboard focus, and user-specified key and pointer button bindings.")
    (license license:x11)))

(define-public xcb-util
  (package
    (name "xcb-util")
    (version "0.4.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://xorg/individual/xcb/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1sahmrgbpyki4bb72hxym0zvxwnycmswsxiisgqlln9vrdlr9r26"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libxcb" ,libxcb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://cgit.freedesktop.org/xcb/util/")
    (synopsis "Core XCB utility functions")
    (description
     "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util module provides the following libraries:

- aux: Convenient access to connection setup and some core requests.

- atom: Standard core X atom constants and atom caching.

- event: Some utilities that have little to do with events any more.")
    (license license:x11)))


(define-public xcb-util-cursor
  (package
    (name "xcb-util-cursor")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://xcb.freedesktop.org/dist/"
                                  "xcb-util-cursor-" version ".tar.bz2"))
              (sha256
               (base32
                "0krr4rcw6r42cncinzvzzdqnmxk3nrgpnadyg2h8k9x10q3hm885"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("m4" ,m4)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxcb" ,libxcb)))
    (propagated-inputs
     `(("xcb-util-renderutil" ,xcb-util-renderutil)
       ("xcb-util-image" ,xcb-util-image)))
    (home-page "https://cgit.freedesktop.org/xcb/util-cursor/")
    (synopsis "Port of libxcursor")
    (description "XCB-util-cursor is a port of libxcursor.")
    (license
     ; expat license  with added clause regarding advertising
     (license:non-copyleft
      "file://COPYING"
      "See COPYING in the distribution."))))

(define-public xcb-util-errors
  (let ((commit "5d660ebe872cadcdc85de9d6f9afe05de629c030")
        (revision "1"))
    (package
      (name "xcb-util-errors")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://anongit.freedesktop.org/git/xcb/util-errors.git")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "12bah0iz5k6b9hwlc5zffyfg2gnrajll3gn5s8zmazgynvw72ahg"))))
     (build-system gnu-build-system)
     (outputs '("out"))
     (inputs
      `(("util-macros" ,util-macros)
        ("xcb-proto" ,xcb-proto)))
     (propagated-inputs
      `(("libxcb" ,libxcb)))
     (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("libtool" ,libtool)
        ("python-2" ,python-2)
        ("pkg-config" ,pkg-config)))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (replace 'bootstrap
            (lambda _
              ;; The default 'bootstrap' phase would run 'autogen.sh', which
              ;; would try to run ./configure and fail due to unpatched
              ;; shebangs.
              (invoke "autoreconf" "-v" "--install"))))))
     (home-page "https://cgit.freedesktop.org/xcb/util-errors/")
     (synopsis "XCB helper library for printing information about X11 errors")
     (description
      "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-errors module provides a utility library that gives human
readable names to error codes, event codes, and also to major and minor
numbers.")
     (license license:x11))))

(define-public xcb-util-image
  (package
    (name "xcb-util-image")
    (version "0.4.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://xorg/individual/xcb/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1z1gxacg7q4cw6jrd26gvi5y04npsyavblcdad1xccc8swvnmf9d"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libxcb" ,libxcb)))
    (inputs
     `(("xcb-util" ,xcb-util)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://cgit.freedesktop.org/xcb/util-image/")
    (synopsis "XCB port of Xlib's XImage and XShmImage")
    (description
     "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-image module provides the following library:

- image: Port of Xlib's XImage and XShmImage functions.")
    (license license:x11)))


(define-public xcb-util-keysyms
  (package
    (name "xcb-util-keysyms")
    (version "0.4.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://xorg/individual/xcb/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1nbd45pzc1wm6v5drr5338j4nicbgxa5hcakvsvm5pnyy47lky0f"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libxcb" ,libxcb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://cgit.freedesktop.org/xcb/util-keysyms/")
    (synopsis "Standard X constants and conversion to/from keycodes")
    (description
     "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-keysyms module provides the following library:

- keysyms: Standard X key constants and conversion to/from keycodes.")
    (license license:x11)))


(define-public xcb-util-renderutil
  (package
    (name "xcb-util-renderutil")
    (version "0.3.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://xorg/individual/xcb/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "0nza1csdvvxbmk8vgv8vpmq7q8h05xrw3cfx9lwxd1hjzd47xsf6"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libxcb" ,libxcb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://cgit.freedesktop.org/xcb/util-renderutil/")
    (synopsis "Convenience functions for the Render extension")
    (description
     "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-renderutil module provides the following library:

- renderutil: Convenience functions for the Render extension.")
    (license license:x11)))


(define-public xcb-util-wm
  (package
    (name "xcb-util-wm")
    (version "0.4.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://xorg/individual/xcb/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "0gra7hfyxajic4mjd63cpqvd20si53j1q3rbdlkqkahfciwq3gr8"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libxcb" ,libxcb)))
    (native-inputs
     `(("m4" ,m4)
       ("pkg-config" ,pkg-config)))
    (home-page "https://cgit.freedesktop.org/xcb/util-wm/")
    (synopsis "Client and window-manager helpers for ICCCM and EWMH")
    (description
     "The XCB util modules provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-wm module provides the following libraries:

- ewmh: Both client and window-manager helpers for EWMH.

- icccm: Both client and window-manager helpers for ICCCM.")
    (license license:x11)))

(define-public xinit
  (package
    (name "xinit")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/app/xinit-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1fdbakx59vyh474skjydj1bbglpby3y03nl7mxn0z9v8gdhqz6yy"))))
    (build-system gnu-build-system)
    (inputs
     `(("xorgproto" ,xorgproto)
       ("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("xauth" ,xauth)))
    (home-page "https://www.x.org/")
    (synopsis "Commands to start the X Window server")
    (description
     "The xinit program is used to start the X Window System server and a
first client program on systems that are not using a display manager such as
xdm.  This package also provides the 'startx' command, which provides a
user-friendly mechanism to start the X server.")
    (license license:x11)))

;; package outside the x.org system proper of height 5

(define-public libxaw3d
  (package
    (name "libxaw3d")
    (version "1.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXaw3d-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0i653s8g25cc0mimkwid9366bqkbyhdyjhckx7bw77j20hzrkfid"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libxext" ,libxext)
        ("libxmu" ,libxmu)
        ("libxt" ,libxt)))
    (inputs
     `(("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Xaw3d library")
    (description
     "Xaw is the X 3D Athena Widget Set based on the X Toolkit
Intrinsics (Xt) Library.")
    (license license:x11)))

(define-public xclock
  (package
    (name "xclock")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.x.org/releases/individual/app/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32 "0m92zhamh15my9f2rqa14q41d6k2cn468azm3g7g3w9n7942024k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-appdefaultdir="
                            %output ,%app-defaults-dir))))
    (inputs
     `(("libxmu" ,libxmu)
       ("libx11" ,libx11)
       ("libxaw" ,libxaw)
       ("libxrender" ,libxrender)
       ("libxft" ,libxft)
       ("libxkbfile" ,libxkbfile)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xclock")
    (synopsis "Analog / digital clock for X")
    (description "The xclock program displays the time in analog or digital
form.")
    (license (license:x11-style "file://COPYING" "See COPYING for details."))))

(define-public xmag
  (package
    (name "xmag")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://xorg/individual/app/" name "-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "19bsg5ykal458d52v0rvdx49v54vwxwqg8q36fdcsv9p2j8yri87"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-appdefaultdir="
                            %output ,%app-defaults-dir))))
    (inputs
     `(("libxaw" ,libxaw)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Display or capture a magnified part of a X11 screen")
    (description "Xmag displays and captures a magnified snapshot of a portion
of an X11 screen.")
    (license license:x11)))

(define-public xmessage
  (package
    (name "xmessage")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://xorg/individual/app/" name "-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0sw0b0cbrjl44brw7qi1jkm61xd7a1lpj04418c6iqk6mf83llwr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-appdefaultdir="
                            %output ,%app-defaults-dir))))
    (inputs
     `(("libxaw" ,libxaw)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Displays a message or query in a window")
    (description
     "Xmessage displays a message or query in a window.   The user can click
on a button to dismiss it or can select one of several buttons
to answer a question.  Xmessage can also exit after a specified time.")
    (license license:x11)))

(define-public xterm
  (package
    (name "xterm")
    (version "335")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "http://invisible-mirror.net/archives/xterm/"
                                   name "-" version ".tgz")
                    (string-append "ftp://ftp.invisible-island.net/xterm/"
                                   name "-" version ".tgz")))
              (sha256
               (base32
                "15nbgys4s2idhx6jzzc24g9bb1s6yps5fyg2bafvs0gkkcm1ggz0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-wide-chars" "--enable-load-vt-fonts"
                           "--enable-i18n" "--enable-doublechars"
                           "--enable-luit" "--enable-mini-luit")
       #:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("luit" ,luit)
       ("libXft" ,libxft)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("ncurses" ,ncurses)
       ("libICE" ,libice)
       ("libSM" ,libsm)
       ("libX11" ,libx11)
       ("libXext" ,libxext)
       ("libXt" ,libxt)
       ("xorgproto" ,xorgproto)
       ("libXaw" ,libxaw)))
    (home-page "https://invisible-island.net/xterm/")
    (synopsis "Terminal emulator for the X Window System")
    (description
     "The xterm program is a terminal emulator for the X Window System.  It
provides DEC VT102/VT220 (VTxxx) and Tektronix 4014 compatible terminals for
programs that cannot use the window system directly.")
    (license license:x11)))

(define-public perl-x11-xcb
  (package
    (name "perl-x11-xcb")
    (version "0.18")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/M/MS/MSTPLBG/"
                    "X11-XCB-" version ".tar.gz"))
              (sha256
               (base32
                "1cjpghw7cnackw20lbd7yzm222kz5bnrwz52f8ay24d1f4pwrnxf"))))
    (build-system perl-build-system)
    (arguments
     '(;; Disable parallel build to prevent a race condition.
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-perl-search-path
           (lambda _
             (setenv "PERL5LIB"
                     (string-append (getcwd) ":"
                                    (getenv "PERL5LIB")))
             #t))
         (add-before 'build 'patch-Makefile
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               ;; XXX: Without this hack, attempts at using XCB.so fails with
               ;; an error such as "XCB.so: undefined symbol: xcb_xinerama_id"
               (("^LDDLFLAGS = ")
                (string-append "LDDLFLAGS = "
                               "-lxcb -lxcb-util -lxcb-xinerama -lxcb-icccm ")))
             #t)))
       ;; Tests require a running X11 server.
       #:tests? #f))
    (native-inputs
     `(("perl-extutils-depends" ,perl-extutils-depends)
       ("perl-extutils-pkgconfig" ,perl-extutils-pkgconfig)
       ("perl-module-install" ,perl-module-install)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-data-dump" ,perl-data-dump)
       ("perl-mouse" ,perl-mouse)
       ("perl-mousex-nativetraits" ,perl-mousex-nativetraits)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-xml-descent" ,perl-xml-descent)
       ("perl-xml-simple" ,perl-xml-simple)
       ("perl-xs-object-magic" ,perl-xs-object-magic)))
    (inputs
     `(("libxcb" ,libxcb)
       ("xcb-proto" ,xcb-proto)
       ("xcb-util" ,xcb-util)
       ("xcb-util-wm" ,xcb-util-wm)))
    (home-page "https://metacpan.org/release/X11-XCB")
    (synopsis "Perl bindings for libxcb")
    (description
     "These bindings wrap @code{libxcb} (a C library to speak with X11,
in many cases better than @code{Xlib}), and provides an object oriented
interface to its methods (using @code{Mouse}).")
    (license license:perl-license)))

(define-public perl-x11-protocol
  (package
    (name "perl-x11-protocol")
    (version "0.56")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/S/SM/SMCCAM/X11-Protocol-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1dq89bh6fqv7l5mbffqcismcljpq5f869bx7g8lg698zgindv5ny"))))
    (build-system perl-build-system)
    (arguments '(#:tests? #f))          ;tests require a running x server
    (synopsis "Raw interface to X Window System servers")
    (description
     "X11::Protocol is a client-side interface to the X11 Protocol, allowing
perl programs to display windows and graphics on X11 servers.")
    (home-page "https://metacpan.org/release/X11-Protocol")
    ;; From the package README: "you can redistribute and/or modify it under
    ;; the same terms as Perl itself. (As an exception, the file
    ;; Keysyms.pm,which is derived from a file in the standard X11
    ;; distribution, has another, less restrictive copying policy, as do some
    ;; of the extension modules in the directory Protocol/Ext: see those files
    ;; for details)."
    (license license:perl-license)))

(define-public perl-x11-protocol-other
  (package
    (name "perl-x11-protocol-other")
    (version "30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/K/KR/KRYDE/X11-Protocol-Other-"
             version ".tar.gz"))
       (sha256
        (base32
         "1mambi57cdkj82wiw1l8y2f70a60qsamdas0165hlj10drryfgrj"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-encode-hanextra" ,perl-encode-hanextra)
       ("perl-module-util" ,perl-module-util)))
    (propagated-inputs
     `(("perl-x11-protocol" ,perl-x11-protocol)))
    (home-page "https://metacpan.org/release/X11-Protocol-Other")
    (synopsis "Miscellaneous helpers for @code{X11::Protocol} connections")
    (description
     "@code{X11::Protocol::Other} contains window manager related functions for
use by client programs, as per the @dfn{ICCCM} (Inter-Client Communication
Conventions Manual) and some of the @dfn{EWMH}
(Extended Window Manager Hints).")
    (license license:gpl3+)))

(define-public xcompmgr
  (package
    (name "xcompmgr")
    (version "1.1.7")
    (source
     (origin
       ;; there's no current tarball
       (method git-fetch)
       (uri (git-reference
             (url "https://anongit.freedesktop.org/git/xorg/app/xcompmgr.git")
             (commit (string-append name "-" version))))
       (sha256
        (base32
         "04swkrm3gk689wrjc418bd3n25w8r20kg1xfbn5j8d7mx1r5gf16"))
       (file-name (string-append name "-" version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'autogen
                              (lambda _
                                (setenv "NOCONFIGURE" "t")
                                (invoke "sh" "autogen.sh"))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("libX11" ,libx11)
       ("libXext" ,libxext)
       ("libXcomposite" ,libxcomposite)
       ("libXfixes" ,libxfixes)
       ("libXdamage" ,libxdamage)
       ("libXrender" ,libxrender)))
    (synopsis "X Compositing manager using RENDER")
    (description "xcompmgr is a sample compositing manager for X servers
supporting the XFIXES, DAMAGE, RENDER, and COMPOSITE extensions.  It enables
basic eye-candy effects.")
    (home-page "https://cgit.freedesktop.org/xorg/app/xcompmgr/")
    (license (license:x11-style
              "https://cgit.freedesktop.org/xorg/app/xcompmgr/tree/COPYING"))))

(define-public xpra
  (package
    (name "xpra")
    (version "2.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.xpra.org/src/xpra-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0pq2pzmv5fsafp50rzl9nb6ns08rl88fhgdqc2hh27dx7b8ka8n6"))))
    (build-system python-build-system)
    (inputs `(("ffmpeg" ,ffmpeg)
              ("flac" ,flac)
              ("gtk+-2" ,gtk+-2) ;; no full GTK3 support yet
              ("libjpeg" ,libjpeg)
              ("libpng" ,libpng)
              ("libvpx" ,libvpx)
              ("libx264" ,libx264)
              ("libxcomposite" ,libxcomposite)
              ("libxdamage" ,libxdamage)
              ("libxi" ,libxi)
              ("libxkbfile" ,libxkbfile)
              ("libxrandr" ,libxrandr)
              ("libxtst" ,libxtst)
              ("lzo" ,lzo)
              ("python2-cryptography" ,python2-cryptography)
              ("python2-dbus" ,python2-dbus)
              ("python2-lz4" ,python2-lz4)
              ("python2-lzo" ,python2-lzo)
              ("python2-netifaces" ,python2-netifaces)
              ("python2-numpy" ,python2-numpy)
              ("python2-pillow" ,python2-pillow)
              ("python2-pycairo" ,python2-pycairo)
              ("python2-pycrypto" ,python2-pycrypto)
              ("python2-pygobject" ,python2-pygobject)
              ("python2-pyopengl" ,python2-pyopengl)
              ("python2-pyopengl-accelerate" ,python2-pyopengl-accelerate)
              ("python2-pygtk" ,python2-pygtk)
              ("python2-rencode" ,python2-rencode)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("python2-cython" ,python2-cython)))
    (arguments
     `(#:python ,python-2 ;; no full Python 3 support yet
       #:configure-flags '("--with-tests"
                           "--with-bundle_tests"
                           "--without-Xdummy" ;; We use Xvfb instead.
                           "--without-Xdummy_wrapper"
                           "--without-strict")
       #:modules ((guix build python-build-system)
                  (guix build utils))

       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check) ;; There's no test suite at the moment.

         ;; Remove BUILD_CPU, BUILD_DATE, BUILD_TIME from build info to
         ;; prevent deterministic issues.  Also correct some directories and
         ;; use the xvfb binary instead of xorg-server (which doesn't seem to
         ;; work).
         (add-before 'install 'remove-timestamps&set-file-names
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "add_build_info.py"
               ((".*\"BUILD_CPU\", get_cpuinfo.*") ""))
             (substitute* "add_build_info.py"
               ((".*\"BUILD_DATE\", datetime.*") ""))
             (substitute* "add_build_info.py"
               ((".*\"BUILD_TIME\", datetime.*") ""))
             (substitute* "setup.py"
               (("/etc/init.d/")
                (string-append (assoc-ref outputs "out")
                               "/etc/init.d/")))
             (substitute* "setup.py"
               (("/usr/lib/")
                (string-append (assoc-ref outputs "out") "/lib/")))
             ;; Use Xvfb with '-nolisten local' to disable abstract X11 sockets.
             (substitute* "./xpra/scripts/config.py"
               ((":.*join.*xvfb.*")
                (string-append ": \"" (assoc-ref inputs "xorg-server")
                               "/bin/Xvfb +extension Composite"
                               " -screen 0 5760x2560x24+32 -dpi 96 -nolisten"
                               " tcp -nolisten local -noreset -auth"
                               " $XAUTHORITY\",\n")))
             (substitute* "./xpra/scripts/config.py"
               (("socket-dir.*: \"\",")
                "socket-dir\"        : \"~/.xpra\","))
             #t)))))
    (home-page "https://www.xpra.org/")
    (synopsis "Remote access to individual applications or full desktops")
    (description "Xpra is a persistent remote display server and client for
forwarding applications and desktop screens.  It gives you remote access to
individual applications or full desktops.  On X11, it is also known as
``@command{screen} for X11'': it allows you to run programs, usually on a
remote host, direct their display to your local machine, and then to
disconnect from these programs and reconnect from the same or another machine,
without losing any state.  It can also be used to forward full desktops from
X11 servers, Windows, or macOS.")
    (license license:gpl2+)))

(define-public uim
  (package
    (name "uim")
    (version "1.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/uim/uim/releases/download/"
                           version "/uim-" version ".tar.bz2"))
       (sha256
        (base32
         "1p7sl0js47ja4glmax93ci59h02ipqw3wxkh4f1qgaz5qjy9nn9l"))))
    (build-system gnu-build-system)
    (inputs
     `(("anthy" ,anthy)
       ("libedit" ,libedit)
       ("libxft" ,libxft)
       ("m17n-lib" ,m17n-lib)))
    (native-inputs
     `(("emacs" ,emacs-minimal)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))
       #:configure-flags
       (list "--with-anthy-utf8"
             (string-append "--with-lispdir=" %output
                            "/share/emacs/site-lisp/guix.d")
             ;; Set proper runpath
             (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:phases
       (modify-phases %standard-phases
         ;; Set path of uim-el-agent and uim-el-helper-agent executables
         (add-after 'configure 'configure-uim-el
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "emacs/uim-var.el"
               (("\"(uim-el-agent|uim-el-helper-agent)\"" _ executable)
                (string-append "\"" (assoc-ref outputs "out")
                               "/bin/" executable "\"")))
             #t))
         ;; Generate emacs autoloads for uim.el
         (add-after 'install 'make-autoloads
           (lambda* (#:key outputs #:allow-other-keys)
             (emacs-generate-autoloads
              ,name (string-append (assoc-ref outputs "out")
                                   "/share/emacs/site-lisp"))
             #t)))))
    (home-page "https://github.com/uim/uim")
    (synopsis "Multilingual input method framework")
    (description "Uim is a multilingual input method library and environment.
It provides a simple, easily extensible and high code-quality input method
development platform, and useful input method environment for users of desktop
and embedded platforms.")
    (license (list license:lgpl2.1+ ; scm/py.scm, pixmaps/*.{svg,png} (see pixmaps/README)
                   license:gpl2+ ; scm/pinyin-big5.scm
                   license:gpl3+ ; scm/elatin-rules.cm
                   license:public-domain ; scm/input-parse.scm, scm/match.scm
                   ;; gtk2/toolbar/eggtrayicon.{ch},
                   ;; qt3/chardict/kseparator.{cpp,h},
                   ;; qt3/pref/kseparator.{cpp,h}
                   license:lgpl2.0+
                   ;; pixmaps/*.{svg,png} (see pixmaps/README),
                   ;; all other files
                   license:bsd-3))))

(define-public uim-gtk
  (package
    (inherit uim)
    (name "uim-gtk")
    (inputs
     `(("gtk" ,gtk+)
       ("gtk" ,gtk+-2)
       ,@(package-inputs uim)))
    (synopsis "Multilingual input method framework (GTK+ support)")))

(define-public uim-qt
  (package
    (inherit uim)
    (name "uim-qt")
    (inputs
     `(("qt" ,qt-4)
       ,@(package-inputs uim)))
    (arguments
     (substitute-keyword-arguments (package-arguments uim)
       ((#:configure-flags configure-flags)
        (append configure-flags (list "--with-qt4-immodule"
                                      "--with-qt4")))))
    (synopsis "Multilingual input method framework (Qt support)")))

(define-public keynav
  (package
    (name "keynav")
    (version "0.20110708.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://http.debian.net/debian/pool/main/k/keynav/keynav_"
             version ".orig.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1gizjhji3yspxxxvb90js3z1bv18rbf5phxg8rciixpj3cccff8z"))))
    (build-system gnu-build-system)
    (inputs
     `(("cairo" ,cairo)
       ("glib" ,glib)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxinerama" ,libxinerama)
       ("libxtst" ,libxtst)
       ("xdotool" ,xdotool)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:tests? #f ;No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'setenv
           (lambda _
             (setenv "CC" (which "gcc"))
             #t))
         (add-after 'unpack 'patch-keynav
           (lambda _
             (substitute* "keynav.c"
               (("xdo_symbol_map") "xdo_get_symbol_map")
               (("xdo_window_setclass") "xdo_set_window_class")
               (("xdo_window_get_active") "xdo_get_window_at_mouse")
               (("xdo_click") "xdo_click_window")
               (("xdo_mouseup") "xdo_mouse_up")
               (("xdo_mousedown") "xdo_mouse_down")
               (("xdo_mousemove") "xdo_move_mouse")
               (("xdo_mousemove_relative") "xdo_move_mouse_relative")
               (("xdo_mouselocation") "xdo_get_mouse_location")
               (("xdo_mouse_wait_for_move_to") "xdo_wait_for_mouse_move_to")
               (("xdo_keysequence_up") "xdo_send_keysequence_window_up")
               (("xdo_keysequence_down") "xdo_send_keysequence_window_down"))
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "keynav" (string-append out "/bin"))
               (install-file "keynavrc" (string-append out "/etc")))
             #t)))))
    (home-page "https://www.semicomplete.com/projects/keynav/")
    (synopsis "Keyboard-driven mouse cursor mover")
    (description
     "Keynav makes your keyboard a fast mouse cursor mover.  You can move the
cursor to any point on the screen with a few key strokes.  It also simulates
mouse click.  You can do everything mouse can do with a keyboard.")
    (license license:bsd-3)))

(define-public transset-df
  (package
    (name "transset-df")
    (version "6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://forchheimer.se/" name "/" name "-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1vnykwwrv75miigbhmcwxniw8xnhsdyzhqydip2m9crxi2lwhqs5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'setenv
           (lambda _
             (setenv "CC" (which "gcc"))
             #t))
         (delete 'configure)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "transset-df" bin)
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs `(("libxcomposite" ,libxcomposite)
              ("libxdamager" ,libxdamage)
              ("libxrender" ,libxrender)))
    (synopsis "Set the transparency of X11 windows")
    (description "The @command{transset-df} command allows you to set the
opacity of X11 windows.  This patched version of X.Org's @command{transset}
adds functionality, including: selecting window by clicking (as transset),
selecting windows by pointing select actual focused X11 window, selecting by
window name or id, forcing toggle, increase or decrease opacity.")
    (home-page "http://forchheimer.se/transset-df/")
    (license license:x11)))

(define-public bdfresize
  (package
    (name "bdfresize")
    (version "1.5-11")
    (source (origin
              ;; Former upstream at
              ;; <http://openlab.ring.gr.jp/efont/dist/tools/bdfresize/>
              ;; vanished so use Debian, which in practice is the new
              ;; upstream.
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/debian/bdfresize.git")
                    (commit (string-append "debian/" version))))
              (sha256
               (base32
                "0n3i29wicak8n10vkkippym8yw4ir8f7a263a8rwb8q16wqrxx85"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each make-file-writable (find-files "."))

                  ;; Remove broken declaration.
                  (substitute* "charresize.c"
                    (("char\t\\*malloc\\(\\);")
                     ""))

                  ;; Remove old configury that doesn't support modern
                  ;; command-line options, new architectures, etc.
                  (for-each delete-file
                            '("configure" "install-sh"
                              "missing" "mkinstalldirs"))
                  #t))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)))
    (synopsis "Resize fonts in the BDF format")
    (description
     "This packages provides @command{bdfresize}, a command to magnify or
reduce fonts in the Glyph Bitmap Distribution Format (BDF).  It produces BDF
output.")
    (home-page "https://tracker.debian.org/pkg/bdfresize")
    (license license:gpl2+)) )

(define-public console-setup
  (package
    (name "console-setup")
    (version "1.189")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://salsa.debian.org/installer-team/console-setup.git")
             (commit version)))
       (sha256
        (base32 "09rfnnhwjm98im711v6jrgl49iy5n1b26x12zzk5rw6g1667mz86"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (let ((bash (assoc-ref %build-inputs "bash"))
             (out (assoc-ref %outputs "out")))
         (list (string-append "SHELL=" bash "/bin/bash")))
       #:tests? #f                                ;no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'make-doubled-bdfs
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "make" "-C" "Fonts"
                     "doubled_bdfs"
                     (string-append "SHELL="
                                    (assoc-ref inputs "bash")
                                    "/bin/bash"))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref %outputs "out")))
               (invoke "make" "install-linux"
                       (string-append "prefix=" out)
                       (string-append "SHELL="
                                      (assoc-ref inputs "bash")
                                      "/bin/bash"))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("bdftopcf" ,bdftopcf)
       ("bdfresize" ,bdfresize)
       ("sharutils" ,sharutils)))                 ;for 'uuencode'
    (inputs
     `(("perl" ,perl)))                           ;used by 'ckbcomp'
    (synopsis "Set up the Linux console font and keyboard")
    (description
     "console-setup provides the console with the same keyboard
configuration scheme that X Window System has.  In particular, the
@command{ckbcomp} program compiles an XKB keyboard description to a keymap
suitable for @command{loadkeys} or @command{kbdcontrol}.  As a result, there
is no need to duplicate or change the console keyboard files just to make
simple customizations.

Besides the keyboard, the package also configures the font on the console.  It
includes a rich collection of fonts and supports several languages that would
otherwise be unsupported on the console (such as Armenian, Georgian, Lao, and
Thai).")
    (home-page "https://salsa.debian.org/installer-team/console-setup/")

    ;; Most of the code is GPLv2+; the Expat license applies to 'setupcon' and
    ;; 'ckbcomp-mini'.  The installed precompiled keyboard files are covered
    ;; by simple permissive licenses.  See the 'COPYRIGHT' file.
    (license (list license:gpl2+
                   license:expat))))
