;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages ncurses)
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
        ("xproto" ,xproto)))
    (arguments
     `(#:phases
       (alist-cons-after
        'install 'install-data
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
              (and (zero? (system* "./configure"
                                   (string-append "SHELL=" (which "bash"))
                                   (string-append "--prefix=" out)))
                   (zero? (system* "make" "install"))))))
        %standard-phases)))
    (home-page "http://www.x.org")
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

(define-public bdftopcf
  (package
    (name "bdftopcf")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/bdftopcf-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1617zmgnx50n7vxlqyj84fl7vnk813jjqmi6jpigyz1xp9br1xga"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxfont" ,libxfont)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public bigreqsproto
  (package
    (name "bigreqsproto")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/bigreqsproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07hvfm84scz8zjw14riiln2v4w03jlhp756ypwhq27g48jmic8a6"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public compositeproto
  (package
    (name "compositeproto")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/compositeproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1z0crmf669hirw4s7972mmp8xig80kfndja9h559haqbpvq5k4q4"))))
    (build-system gnu-build-system)
    (inputs
      `(("fixesproto" ,fixesproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public damageproto
  (package
    (name "damageproto")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/damageproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0nzwr5pv9hg7c21n995pdiv0zqhs91yz3r8rn3aska4ykcp12z2w"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public dmxproto
  (package
    (name "dmxproto")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/dmxproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "02b5x9dkgajizm8dqyx2w6hmqx3v25l67mgf35nj6sz0lgk52877"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; not part of X11R7.7, required for newer versions of mesa
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license (license:x11-style "file://dri3proto.h"
                                "See 'dri3proto.h' in the distribution."))))


(define-public encodings
  (package
    (name "encodings")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/encodings-"
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:public-domain)))


(define-public font-adobe100dpi
  (package
    (name "font-adobe100dpi")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-adobe-100dpi-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0m60f5bd0caambrk8ksknb5dks7wzsg7g7xaf0j21jxmx8rq9h5j"))))
    (build-system gnu-build-system)
    (inputs
      `(("bdftopcf" ,bdftopcf)
        ("font-util", font-util)
        ("mkfontdir" ,mkfontdir)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (arguments
      `(#:configure-flags (list
        ;; install fonts into subdirectory of package output instead of
        ;; font-util-?.?.?/share/fonts/X11
        (string-append "--with-fontrootdir=" %output "/share/fonts/X11"))))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public font-adobe75dpi
  (package
    (name "font-adobe75dpi")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-adobe-75dpi-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "02advcv9lyxpvrjv8bjh1b797lzg6jvhipclz49z8r8y98g4l0n6"))))
    (build-system gnu-build-system)
    (inputs
      `(("bdftopcf" ,bdftopcf)
        ("font-util", font-util)
        ("mkfontdir" ,mkfontdir)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (arguments
      `(#:configure-flags (list
        (string-append "--with-fontrootdir=" %output "/share/fonts/X11"))))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; non-free license
;; (define-public font-adobe-utopia100dpi
;; (define-public font-adobe-utopia75dpi
;; (define-public font-adobe-utopia-type1


(define-public font-alias
  (package
    (name "font-alias")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-alias-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "16ic8wfwwr3jicaml7b5a0sk6plcgc1kg84w02881yhwmqm3nicb"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public font-arabic-misc
  (package
    (name "font-arabic-misc")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-arabic-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1x246dfnxnmflzf0qzy62k8jdpkb6jkgspcjgbk8jcq9lw99npah"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-cronyx-cyrillic-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ai1v4n61k8j9x2a1knvfbl2xjxk3xxmqaq3p9vpqrspc69k31kf"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; no license
;; (define-public font-cursor-misc

;; non-free license
;; (define-public font-daewoo-misc


(define-public font-dec-misc
  (package
    (name "font-dec-misc")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-dec-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0yzza0l4zwyy7accr1s8ab7fjqkpwggqydbm2vc19scdby5xz7g1"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; non-free license
;; (define-public font-ibm-type1

(define-public font-isas-misc
  (package
    (name "font-isas-misc")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-isas-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0rx8q02rkx673a7skkpnvfkg28i8gmqzgf25s9yi0lar915sn92q"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; non-free license
;; (define-public font-jis-misc


(define-public font-micro-misc
  (package
    (name "font-micro-misc")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-micro-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1dldxlh54zq1yzfnrh83j5vm0k4ijprrs5yl18gm3n9j1z0q2cws"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:public-domain)))


(define-public font-misc-cyrillic
  (package
    (name "font-misc-cyrillic")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-misc-cyrillic-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0q2ybxs8wvylvw95j6x9i800rismsmx4b587alwbfqiw6biy63z4"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public font-misc-ethiopic
  (package
    (name "font-misc-ethiopic")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-misc-ethiopic-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "19cq7iq0pfad0nc2v28n681fdq3fcw1l1hzaq0wpkgpx7bc1zjsk"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; non-free license
;; (define-public font-misc-meltho


(define-public font-misc-misc
  (package
    (name "font-misc-misc")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-misc-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "150pq6n8n984fah34n3k133kggn9v0c5k07igv29sxp1wi07krxq"))))
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:public-domain)))


(define-public font-mutt-misc
  (package
    (name "font-mutt-misc")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-mutt-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "13qghgr1zzpv64m0p42195k1kc77pksiv059fdvijz1n6kdplpxx"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public font-schumacher-misc
  (package
    (name "font-schumacher-misc")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-schumacher-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0nkym3n48b4v36y4s927bbkjnsmicajarnf6vlp7wxp0as304i74"))))
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public font-screen-cyrillic
  (package
    (name "font-screen-cyrillic")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-screen-cyrillic-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0yayf1qlv7irf58nngddz2f1q04qkpr5jwp4aja2j5gyvzl32hl2"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public font-sony-misc
  (package
    (name "font-sony-misc")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-sony-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1xfgcx4gsgik5mkgkca31fj3w72jw9iw76qyrajrsz1lp8ka6hr0"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public font-sun-misc
  (package
    (name "font-sun-misc")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-sun-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1q6jcqrffg9q5f5raivzwx9ffvf7r11g6g0b125na1bhpz5ly7s8"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public font-util
  (package
    (name "font-util")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-util-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "15cijajwhjzpy3ydc817zz8x5z4gbkyv3fps687jbq544mbfbafz"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public font-winitzki-cyrillic
  (package
    (name "font-winitzki-cyrillic")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-winitzki-cyrillic-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "181n1bgq8vxfxqicmy1jpm1hnr6gwn1kdhl6hr4frjigs1ikpldb"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:public-domain)))


(define-public font-xfree86-type1
  (package
    (name "font-xfree86-type1")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-xfree86-type1-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0jp3zc0qfdaqfkgzrb44vi9vi0a8ygb35wp082yz7rvvxhmg9sya"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public iceauth
  (package
    (name "iceauth")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/iceauth-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1x72y99dxf2fxnlyf0yrf9yzd8xzimxshy6l8mprwhrv6lvhi6dx"))))
    (build-system gnu-build-system)
    (inputs
      `(("libice" ,libice)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public inputproto
  (package
    (name "inputproto")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/inputproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1lf1jlxp0fc8h6fjdffhd084dqab94966l1zm3rwwsis0mifwiss"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public kbproto
  (package
    (name "kbproto")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/kbproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0yal11hhpiisy3w8wmacsdzzzcnc3xwnswxz8k7zri40xc5aqz03"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; requires applewmproto, which compiles only on macos
;; (define-public libapplewm


(define-public libdmx
  (package
    (name "libdmx")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libdmx-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "00djlxas38kbsrglcmwmxfbmxjdchlbj95pqwjvdg8jn5rns6zf9"))))
    (build-system gnu-build-system)
    (inputs
      `(("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("dmxproto" ,dmxproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxshmfence
  (package
    (name "libxshmfence")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/lib/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1gnfb1z8sbbdc3xpz1zmm94lv7yvfh4kvip9s5pj37ya4llxphnv"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("xproto" ,xproto)))
    (home-page "http://xorg.freedesktop.org")
    (synopsis "X shared memory fences")
    (description
     "This library provides an interface to shared-memory fences for
synchronization between the X server and direct-rendering clients.")
    (license license:x11-style)))


(define-public libfontenc
  (package
    (name "libfontenc")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libfontenc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0qign0ivqk166l9yfd51gw9lbhgs718bcrmvc40yicjr6gnyz959"))))
    (build-system gnu-build-system)
    (inputs
      `(("zlib" ,zlib)
        ("xproto" ,xproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libfs
  (package
    (name "libfs")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libFS-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1mxfsvj9m3pn8cdkcn4kg190zp665mf4pv0083g6xykvsgxzq1wh"))))
    (build-system gnu-build-system)
    (inputs
      `(("xtrans" ,xtrans)
        ("xproto" ,xproto)
        ("fontsproto" ,fontsproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libpciaccess
  (package
    (name "libpciaccess")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libpciaccess-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "06fy43n3c450h7xqpn3094bnfn7ca1mrq3i856y8kyqa0lmqraxb"))))
    (build-system gnu-build-system)
    (inputs
      `(("zlib" ,zlib)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libpthread-stubs
  (package
    (name "libpthread-stubs")
    (version "0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://xcb.freedesktop.org/dist/libpthread-stubs-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "16bjv3in19l84hbri41iayvvg4ls9gv1ma0x0qlbmwy67i7dbdim"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libsm
  (package
    (name "libsm")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libSM-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1gc7wavgs435g9qkp9jw4lhmaiq6ip9llv49f054ad6ryp4sib0b"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libice" ,libice))) ; SMlib.h includes ICElib.h
    (inputs
      `(("xtrans" ,xtrans)
        ("util-linux" ,util-linux)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libwindowswm
  (package
    (name "libwindowswm")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libWindowsWM-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1p0flwb67xawyv6yhri9w17m1i4lji5qnd0gq8v1vsfb8zw7rw15"))))
    (build-system gnu-build-system)
    (inputs
      `(("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("windowswmproto" ,windowswmproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
      `(("xproto" ,xproto)
        ("libxfixes" ,libxfixes)
        ("libx11" ,libx11)
        ("compositeproto" ,compositeproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxdmcp
  (package
    (name "libxdmcp")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libXdmcp-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "13highx4xpgkiwykpcl7z2laslrjc4pzi4h617ny9p7r6116vkls"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
        ("xproto" ,xproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxkbfile
  (package
    (name "libxkbfile")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxkbfile-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0flg5arw6n3njagmsi4i4l0zl5bfx866a1h9ydc3bi1pqlclxaca"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
        ("xproto" ,xproto)
        ("libxext" ,libxext)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxpm
  (package
    (name "libxpm")
    (version "3.5.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXpm-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07041q4k8m4nirzl7lrqn8by2zylx0xvh6n0za301qqs3njszgf5"))))
    (build-system gnu-build-system)
    (inputs
      `(("gettext" ,gnu-gettext)
        ("libxt" ,libxt)
        ("xproto" ,xproto)
        ("libxext" ,libxext)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxres
  (package
    (name "libxres")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXres-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1rd0bzn67cpb2qkc946gch2183r4bdjfhs6cpqbipy47m9a91296"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("resourceproto" ,resourceproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxscrnsaver
  (package
    (name "libxscrnsaver")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libXScrnSaver-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07ff4r20nkkrj7h08f9fwamds9b3imj8jz5iz6y38zqw6jkyzwcg"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxext" ,libxext)
        ("libx11" ,libx11)))
    (propagated-inputs
      `(("scrnsaverproto" ,scrnsaverproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
      `(("xf86dgaproto" ,xf86dgaproto)))
    (inputs
      `(("libx11" ,libx11)
        ("libxext" ,libxext)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public luit
  (package
    (name "luit")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/luit-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0dn694mk56x6hdk6y9ylx4f128h5jcin278gnw2gb807rf3ygc1h"))
        ;; See https://bugs.freedesktop.org/show_bug.cgi?id=47792;
        ;; should become obsolete with the next release.
        (patches (list (search-patch "luit-posix.patch")))))
    (build-system gnu-build-system)
    (inputs
      `(("libfontenc" ,libfontenc)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
      `(("xproto" ,xproto)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public mkfontscale
  (package
    (name "mkfontscale")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/mkfontscale-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0cdpn1ii2iw1vg2ga4w62acrh78gzgf0vza4g8wx5kkp4jcifh14"))))
    (build-system gnu-build-system)
    (inputs
      `(("zlib" ,zlib)
        ("xproto" ,xproto)
        ("freetype" ,freetype)
        ("libfontenc" ,libfontenc)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; not part of X11R7.7, required for newer versions of mesa
(define-public presentproto
  (package
    (name "presentproto")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/presentproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1kir51aqg9cwazs14ivcldcn3mzadqgykc9cg87rm40zf947sb41"))))
    (build-system gnu-build-system)
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license (license:x11-style "file://presentproto.h"
                                "See 'presentproto.h' in the distribution."))))

;; The package is missing from X11R7.7.
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public randrproto
  (package
    (name "randrproto")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/randrproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1kq9h93qdnniiivry8jmhlgwn9fbx9xp5r9cmzfihlx5cs62xi45"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public recordproto
  (package
    (name "recordproto")
    (version "1.14.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/recordproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0w3kgr1zabwf79bpc28dcnj0fpni6r53rpi82ngjbalj5s6m8xx7"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public renderproto
  (package
    (name "renderproto")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/renderproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0dr5xw6s0qmqg0q5pdkb4jkdhaja0vbfqla79qh5j1xjj9dmlwq6"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public resourceproto
  (package
    (name "resourceproto")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/resourceproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0638iyfiiyjw1hg3139pai0j6m65gkskrvd9684zgc6ydcx00riw"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public scrnsaverproto
  (package
    (name "scrnsaverproto")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/scrnsaverproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0rfdbfwd35d761xkfifcscx56q0n56043ixlmv70r4v4l66hmdwb"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public sessreg
  (package
    (name "sessreg")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/sessreg-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1hy4wvgawajf4qw2k51fkcjzxw0drx60ydzpmqhj7k1g4z3cqahf"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public setxkbmap
  (package
    (name "setxkbmap")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/setxkbmap-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1inygpvlgc6vr5h9laxw9lnvafnccl3fy0g5n9ll28iq3yfmqc1x"))))
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public smproxy
  (package
    (name "smproxy")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/smproxy-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "02fn5wa1gs2jap6sr9j9yk6zsvz82j8l61pf74iyqwa99q4wnb67"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("libxmu" ,libxmu)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public util-macros
  (package
    (name "util-macros")
    (version "1.19.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/util/util-macros-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1fnhpryf55l0yqajxn0cxan3kvsjzi67nlanz8clwqzf54cb2d98"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:phases (alist-cons-after
                 'unpack 'fix-makefile-in
                 (lambda _
                   (substitute* "Makefile.in"
                     ;; Install xorg-macros.pc in PREFIX/lib/pkgconfig,
                     ;; not PREFIX/share/pkgconfig.
                     (("\\$\\(datadir\\)/pkgconfig") "$(libdir)/pkgconfig")))
                 (alist-cons-after
                  'install 'post-install-cleanup
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out")))
                      (with-directory-excursion out
                        (delete-file "share/util-macros/INSTALL")
                        (rmdir "share/util-macros"))))
                  %standard-phases))))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public videoproto
  (package
    (name "videoproto")
    (version "2.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/videoproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1dnlkd9nb0m135lgd6hd61vc29sdyarsyya8aqpx7z10p261dbld"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public windowswmproto
  (package
    (name "windowswmproto")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/windowswmproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0syjxgy4m8l94qrm03nvn5k6bkxc8knnlld1gbllym97nvnv0ny0"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public x11perf
  (package
    (name "x11perf")
    (version "1.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/x11perf-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "111iwpxhnxjiq44w96zf0kszg5zpgv1g3ayx18v4nhdzl9bqivi4"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)
        ("libxft" ,libxft)
        ("libxmu" ,libxmu)
        ("libxrender" ,libxrender)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xauth
  (package
    (name "xauth")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xauth-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "13y2invb0894b1in03jbglximbz6v31y2kr4yjjgica8xciibkjn"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libxext" ,libxext)
        ("libxau" ,libxau)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))

    ;; FIXME: The test suite needs http://liw.fi/cmdtest/
    (arguments `(#:tests? #f))

    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xbacklight
  (package
    (name "xbacklight")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xbacklight-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0arnd1j8vzhzmw72mqhjjcb2qwcbs9qphsy3ps593ajyld8wzxhp"))))
    (build-system gnu-build-system)
    (inputs
     `(("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xbitmaps
  (package
    (name "xbitmaps")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xbitmaps-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "178ym90kwidia6nas4qr5n5yqh698vv8r02js0r4vg3b6lsb0w9n"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xcb-proto
  (package
    (name "xcb-proto")
    (version "1.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://xcb.freedesktop.org/dist/xcb-proto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0bp3f53l9fy5x3mn1rkj1g81aiyzl90wacwvqdgy831aa3kfxb5l"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("pkg-config" ,pkg-config) ("python" ,python-wrapper)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xcmiscproto
  (package
    (name "xcmiscproto")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xcmiscproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1pyjv45wivnwap2wvsbrzdvjc5ql8bakkbkrvcv6q9bjjf33ccmi"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xcmsdb
  (package
    (name "xcmsdb")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xcmsdb-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "03ms731l3kvaldq7ycbd30j6134b61i3gbll4b2gl022wyzbjq74"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xcursor-themes
  (package
    (name "xcursor-themes")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/data/xcursor-themes-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "11mv661nj1p22sqkv87ryj2lcx4m68a04b0rs6iqh3fzp42jrzg3"))))
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xcursorgen
  (package
    (name "xcursorgen")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xcursorgen-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0v7nncj3kaa8c0524j7ricdf4rvld5i7c3m6fj55l5zbah7r3j1i"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxcursor" ,libxcursor)
        ("libpng" ,libpng)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xdpyinfo
  (package
    (name "xdpyinfo")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xdpyinfo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "154b29zlrq33lmni883jgwyrb2kx7z8h52jx1s3ys5x5d582iydf"))))
    (build-system gnu-build-system)
    (inputs
      `(("inputproto" ,inputproto)
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xdriinfo
  (package
    (name "xdriinfo")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xdriinfo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "076bjix941znyjmh3j5jjsnhp2gv2iq53d0ks29mvvv87cyy9iim"))))
    (build-system gnu-build-system)
    (inputs
      `(("mesa" ,mesa)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xev
  (package
    (name "xev")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xev-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0hv296mysglcgkx6lj1wxc23kshb2kix1a8yqppxj5vz16mpzw8i"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("libxrandr" ,libxrandr)
        ("xproto" ,xproto)
        ("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libevdev
  (package
    (name "libevdev")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.freedesktop.org/software/" name "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0iil4pnla0kjdx52ay7igq65sx32sjbzn1wx9q3v74m5g7712m16"))))
    (build-system gnu-build-system)
    (native-inputs `(("python" ,python)))
    (home-page "http://www.freedesktop.org/wiki/Software/libevdev/")
    (synopsis "Wrapper library for evdev devices")
    (description
     "libevdev is a wrapper library for evdev devices. it moves the common
tasks when dealing with evdev devices into a library and provides a library
interface to the callers, thus avoiding erroneous ioctls, etc.

The eventual goal is that libevdev wraps all ioctls available to evdev
devices, thus making direct access unnecessary.")
    (license license:x11-style)))


(define-public xf86-input-evdev
  (package
    (name "xf86-input-evdev")
    (version "2.8.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-evdev-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "030haki1h0m85h91c91812gdnk6znfamw5kpr010zxwwbsgxxyl5"))))
    (build-system gnu-build-system)
    (inputs
      `(("udev" ,eudev)
        ("libevdev" ,libevdev)
        ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg"))))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-input-joystick
  (package
    (name "xf86-input-joystick")
    (version "1.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-joystick-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "038mfqairyyqvz02rk7v3i070sab1wr0k6fkxvyvxdgkfbnqcfzf"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg"))))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-input-keyboard
  (package
    (name "xf86-input-keyboard")
    (version "1.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-keyboard-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0nyb61w30z32djrllgr2s1i13di3vsl6hg4pqjhxdal71971ria1"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-input-mouse
  (package
    (name "xf86-input-mouse")
    (version "1.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-mouse-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1kn5kx3qyn9qqvd6s24a2l1wfgck2pgfvzl90xpl024wfxsx719l"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg"))))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-input-synaptics
  (package
    (name "xf86-input-synaptics")
    (version "1.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-synaptics-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "16phzd7yhl4wns957c35qz2nahmjvnlx05jf975s524qkvrdlkyp"))))
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Synaptics touchpad driver for X.Org")
    (description
     "This package provides a touchpad driver for the X.Org window system.")
    (license license:x11)))


(define-public xf86-input-void
  (package
    (name "xf86-input-void")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xf86-input-void-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "01bmk324fq48wydvy1qrnxbw6qz0fjd0i80g0n4cqr1c4mjmif9a"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
        (patches (list (search-patch "xf86-video-ark-remove-mibstore.patch")))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-ast
  (package
    (name "xf86-video-ast")
    (version "0.93.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-ast-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1q64z8qqa0ix3cymqiwk1s3sphd1fvvz30lvyxhgkgciygz6dm69"))
        (patches (list (search-patch "xf86-video-ast-remove-mibstore.patch")))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-ati
  (package
    (name "xf86-video-ati")
    (version "7.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-ati-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0dkrw036ikym8aacl43lnf04q0wbms5498xg5b3l16ngnq36ygpc"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xxf86driproto" ,xf86driproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments `(#:configure-flags `("--disable-glamor"))) ; TODO: Enable glamor
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-cirrus
  (package
    (name "xf86-video-cirrus")
    (version "1.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-cirrus-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1mycqgjp18b6adqj2h90vp324xh8ysyi5migfmjc914vbnkf2q9k"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; non-free license
;; (define-public xf86-video-dummy


(define-public xf86-video-fbdev
  (package
    (name "xf86-video-fbdev")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-fbdev-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "06ym7yy017lanj730hfkpfk4znx3dsj8jq3qvyzsn8w294kb7m4x"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-geode
  (package
    (name "xf86-video-geode")
    (version "2.11.16")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-geode-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "19y13xl7yfrgyis92rmxi0ld95ajgr5il0n9j1dridwzw9aizz1q"))
        (patches (list (search-patch "xf86-video-geode-glibc-2.20.patch")))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)
              ("xorg-server" ,xorg-server)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
;;     (home-page "http://www.x.org/wiki/")
;;     (synopsis "Xorg implementation of the X Window System")
;;     (description "X.org provides an implementation of the X Window System")
;;     (license license:x11)))


(define-public xf86-video-glint
  (package
    (name "xf86-video-glint")
    (version "1.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-glint-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "08a2aark2yn9irws9c78d9q44dichr03i9zbk61jgr54ncxqhzv5"))
        (patches (list
                  (search-patch "xf86-video-glint-remove-mibstore.patch")))))
    (build-system gnu-build-system)
    (inputs `(("xf86dgaproto" ,xf86dgaproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-i128
  (package
    (name "xf86-video-i128")
    (version "1.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-i128-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "171b8lbxr56w3isph947dnw7x87hc46v6m3mcxdcz44gk167x0pq"))
        (patches (list
                  (search-patch "xf86-video-i128-remove-mibstore.patch")))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-intel
  (package
    (name "xf86-video-intel")
    (version "2.21.15")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-intel-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1z6ncmpszmwqi9xr590c4kp4gjjf7mndcr56r35x2bx7h87i8nkx"))
        (patches (list (search-patch "xf86-video-intel-compat-api.patch")
                       (search-patch "xf86-video-intel-glibc-2.20.patch")))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("udev" ,eudev)
              ("libx11" ,libx11)
              ("xorg-server" ,xorg-server)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-mach64
  (package
    (name "xf86-video-mach64")
    (version "6.9.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-mach64-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "0pl582vnc6hdxqhf5c0qdyanjqxb4crnhqlmxxml5a60syw0iwcp"))
        (patches (list (search-patch "xf86-video-mach64-glibc-2.20.patch")))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xf86driproto" ,xf86driproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-mga
  (package
    (name "xf86-video-mga")
    (version "1.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-mga-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1my7y67sadjjmab1dyxckylrggi7p01yk4wwg9w6k1q96pmb213p"))
        (patches (list (search-patch "xf86-video-mga-glibc-2.20.patch")))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xf86driproto" ,xf86driproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))

(define-public xf86-video-modesetting
  (package
    (name "xf86-video-modesetting")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "mirror://xorg/individual/driver/xf86-video-modesetting-"
              version ".tar.bz2"))
        (sha256
         (base32
           "0p6pjn5bnd2wr3lmas4b12zcq12d9ilvssga93fzlg90fdahikwh"))))
    (build-system gnu-build-system)
    (inputs `(("libdrm" ,libdrm)
              ("xf86driproto" ,xf86driproto)
              ("libx11" ,libx11)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "\"Modesetting\" graphics driver for the Xorg server")
    (description
     "This is a generic \"modesetting\" video driver, that relies on the Linux
kernel mode setting (KMS).")
    (license license:x11)))

(define-public xf86-video-neomagic
  (package
    (name "xf86-video-neomagic")
    (version "1.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-neomagic-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0x48sxs1p3kmwk3pq1j7vl93y59gdmgkq1x5xbnh0yal0angdash"))))
    (build-system gnu-build-system)
    (inputs `(("xf86dgaproto" ,xf86dgaproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-newport
  (package
    (name "xf86-video-newport")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xf86-video-newport-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1yafmp23jrfdmc094i6a4dsizapsc9v0pl65cpc8w1kvn7343k4i"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-nv
  (package
    (name "xf86-video-nv")
    (version "2.1.20")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-nv-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1gqh1khc4zalip5hh2nksgs7i3piqq18nncgmsx9qvzi05azd5c3"))
        (patches (list (search-patch "xf86-video-nv-remove-mibstore.patch")))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-openchrome
  (package
    (name "xf86-video-openchrome")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-openchrome-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1v8j4i1r268n4fc5gq54zg1x50j0rhw71f3lba7411mcblg2z7p4"))
        (patches (list
                  (search-patch "xf86-video-openchrome-glibc-2.20.patch")))))
    (build-system gnu-build-system)
    (inputs `(("libx11" ,libx11)
              ("libxext" ,libxext)
              ("libxvmc" ,libxvmc)
              ("mesa" ,mesa)
              ("xf86driproto" ,xf86driproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-r128
  (package
    (name "xf86-video-r128")
    (version "6.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-r128-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1q3fsc603k2yinphx5rrcl5356qkpywwz8axlw277l2231gjjbcb"))
        (patches (list (search-patch "xf86-video-r128-glibc-2.20.patch")))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xf86driproto" ,xf86driproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-savage
  (package
    (name "xf86-video-savage")
    (version "2.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-savage-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0i2aqp68rfkrz9c1p6d7ny9x7bjrlnby7q56zf01fb12r42l4784"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xf86driproto" ,xf86driproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-siliconmotion
  (package
    (name "xf86-video-siliconmotion")
    (version "1.7.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-siliconmotion-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1an321kqvsxq0z35acwl99lc8hpdkayw0q180744ypcl8ffvbf47"))
        (patches
         (list
          (search-patch "xf86-video-siliconmotion-remove-mibstore.patch")))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-sis
  (package
    (name "xf86-video-sis")
    (version "0.10.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-sis-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "1l0w84x39gq4y9j81dny9r6rma1xkqvxpsavpkd8h7h8panbcbmy"))
        (patches (list (search-patch "xf86-video-sis-update-api.patch")))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xf86dgaproto" ,xf86dgaproto)
              ("xf86driproto" ,xf86driproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-tdfx
  (package
    (name "xf86-video-tdfx")
    (version "1.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xf86-video-tdfx-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "124gsi30rj547jjd7gvv7xykfnwlyrxw0gcacamby7pvl3g33fg0"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
              ("xf86driproto" ,xf86driproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-tga
  (package
    (name "xf86-video-tga")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xf86-video-tga-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0mdqrn02zzkdnmhg4vh9djaawg6b2p82g5qbj66z8b30yr77b93h"))))
    (build-system gnu-build-system)
    (inputs `(("xf86dgaproto" ,xf86dgaproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-trident
  (package
    (name "xf86-video-trident")
    (version "1.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xf86-video-trident-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "02y5pzdp0a1c12jr8gghbrzgbyfbgq67x7kd7n4f323pmf8x1csb"))))
    (build-system gnu-build-system)
    (inputs `(("xf86dgaproto" ,xf86dgaproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; no license
;; (define-public xf86-video-v4l


(define-public xf86-video-vesa
  (package
    (name "xf86-video-vesa")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xf86-video-vesa-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0wqms28kkz2fvghqhqwp6w8zj7k5cgwnhzvkc7j4v268bf6h78g4"))))
    (build-system gnu-build-system)
    (inputs `(("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-vmware
  (package
    (name "xf86-video-vmware")
    (version "12.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xf86-video-vmware-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0isiwx516gww8hfk3vy7js83yziyjym9mq2zjadyq1a8v5gqf9y8"))))
    (build-system gnu-build-system)
    (inputs `(("libx11" ,libx11)
              ("libxext" ,libxext)
              ("xorg-server" ,xorg-server)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86-video-voodoo
  (package
    (name "xf86-video-voodoo")
    (version "1.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xf86-video-voodoo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ha748yz92yzn6hp2rhin3il8f4j2rs4vkgdvqkagnv1ryxkh0ph"))))
    (build-system gnu-build-system)
    (inputs `(("xf86dgaproto" ,xf86dgaproto)
              ("xorg-server" ,xorg-server)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
               "mirror://xorg/X11R7.7/src/everything/xf86bigfontproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0j0n7sj5xfjpmmgx6n5x556rw21hdd18fwmavp95wps7qki214ms"))))
    (build-system gnu-build-system)
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86dgaproto
  (package
    (name "xf86dgaproto")
    (version "2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xf86dgaproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0l4hx48207mx0hp09026r6gy9nl3asbq0c75hri19wp1118zcpmc"))))
    (build-system gnu-build-system)
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86driproto
  (package
    (name "xf86driproto")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xf86driproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07v69m0g2dfzb653jni4x656jlr7l84c1k39j8qc8vfb45r8sjww"))))
    (build-system gnu-build-system)
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xf86vidmodeproto
  (package
    (name "xf86vidmodeproto")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xf86vidmodeproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0w47d7gfa8zizh2bshdr2rffvbr4jqjv019mdgyh6cmplyd4kna5"))))
    (build-system gnu-build-system)
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xgamma
  (package
    (name "xgamma")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xgamma-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0463sawps86jnxn121ramsz4sicy3az5wa5wsq4rqm8dm3za48p3"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxxf86vm" ,libxxf86vm)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xhost
  (package
    (name "xhost")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xhost-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1hlxm0is9nks1cx033s1733kkib9ivx2bxa3pb9yayqavwibkxd6"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libxau" ,libxau)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xineramaproto
  (package
    (name "xineramaproto")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xineramaproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ns8abd27x7gbp4r44z3wc5k9zqxxj8zjnazqpcyr4n17nxp8xcp"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xinput
  (package
    (name "xinput")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xinput-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07w7zlpdhpwzzshg8q0y152cy3wl2fj7x1897glnp2la487jsqxp"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("libxrandr" ,libxrandr)
        ("libxinerama" ,libxinerama)
        ("libxext" ,libxext)
        ("libxi" ,libxi)
        ("libx11" ,libx11)
        ("inputproto" ,inputproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define xkbcomp-intermediate ; used as input for xkeyboard-config
  (package
    (name "xkbcomp-intermediate")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xkbcomp-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0aibcbhhjlwcrxh943xg2dswwx5bz1x0pmhs28b55gzsg0vrgb6g"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxkbfile" ,libxkbfile)
        ("libx11" ,libx11)))
    (native-inputs
        `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xkbevd-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "05h1xcnbalndbrryyqs8wzy9h3wz655vc0ymhlk2q4aik17licjm"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxkbfile" ,libxkbfile)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
        ("xproto" ,xproto)
        ("libxaw" ,libxaw)
        ("inputproto" ,inputproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xkeyboard-config
  (package
    (name "xkeyboard-config")
    (version "2.13")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "mirror://xorg/individual/data/xkeyboard-config/xkeyboard-config-"
              version
              ".tar.bz2"))
        (sha256
          (base32
            "1klzbwabzd7bhbg23b14vy4l5xgpapn8lc0m5d8h40d3p7rfjnvv"))))
    (build-system gnu-build-system)
    (inputs
      `(("gettext" ,gnu-gettext)
        ("libx11" ,libx11)
        ("xkbcomp-intermediate" ,xkbcomp-intermediate)))
    (native-inputs
      `(("intltool" ,intltool)
        ("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xkill
  (package
    (name "xkill")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xkill-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0bl1ky8ps9jg842j4mnmf4zbx8nkvk0h77w7bqjlpwij9wq2mvw8"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libx11" ,libx11)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xlsatoms
  (package
    (name "xlsatoms")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xlsatoms-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1y9nfl8s7njxbnci8c20j986xixharasgg40vdw92y593j6dk2rv"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxcb" ,libxcb)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xlsclients
  (package
    (name "xlsclients")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xlsclients-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0g9x7rrggs741x9xwvv1k9qayma980d88nhdqw7j3pn3qvy6d5jx"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxcb" ,libxcb)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xmodmap
  (package
    (name "xmodmap")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xmodmap-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1hwzm54m4ng09ls9i4bq0x84zbyhamgzasgrvhxxp8jqk34f7qpg"))
        (patches (list (search-patch "xmodmap-asprintf.patch")))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
               "mirror://xorg/X11R7.7/src/everything/xorg-sgml-doctools-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0k5pffyi5bx8dmfn033cyhgd3gf6viqj3x769fqixifwhbgy2777"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xpr
  (package
    (name "xpr")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xpr-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1dbcv26w2yand2qy7b3h5rbvw1mdmdd57jw88v53sgdr3vrqvngy"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxmu" ,libxmu)
        ("libx11" ,libx11)))
    (native-inputs
        `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xprop
  (package
    (name "xprop")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xprop-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1ilvhqfjcg6f1hqahjkp8qaay9rhvmv2blvj3w9asraq0aqqivlv"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xrandr
  (package
    (name "xrandr")
    (version "1.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xrandr-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "06xy0kr6ih7ilrwl6b5g6ay75vm2j4lxnv1d5xlj6sdqhqsaqm3i"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("libxrandr" ,libxrandr)
        ("xproto" ,xproto)
        ("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xrdb
  (package
    (name "xrdb")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xrdb-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0nsnr90wazcdd50nc5dqswy0bmq6qcj14nnrhyi7rln9pxmpp0kk"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xrefresh
  (package
    (name "xrefresh")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xrefresh-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1mlinwgvql6s1rbf46yckbfr9j22d3c3z7jx3n6ix7ca18dnf4rj"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xset
  (package
    (name "xset")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xset-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0qw0iic27bz3yz2wynf1gxs70hhkcf9c4jrv7zhlg1mq57xz90j3"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxmu" ,libxmu)
        ("libxext" ,libxext)
        ("libx11" ,libx11)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xsetroot
  (package
    (name "xsetroot")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xsetroot-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1nf3ii31m1knimbidaaym8p61fq3blv8rrdr2775yhcclym5s8ds"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libxcursor" ,libxcursor)
        ("xbitmaps" ,xbitmaps)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xvinfo
  (package
    (name "xvinfo")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xvinfo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1qsh7fszi727l3vwlaf9pb7bpikdv15smrx5qhlgg3kqzl7xklzf"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxext" ,libxext)
        ("libxv" ,libxv)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xwd
  (package
    (name "xwd")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xwd-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ybx48agdvjp9lgwvcw79r1x6jbqbyl3fliy3i5xwy4d4si9dcrv"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("libxkbfile" ,libxkbfile)
        ("xproto" ,xproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xwininfo
  (package
    (name "xwininfo")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xwininfo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1y1zn8ijqslb5lfpbq4bb78kllhch8in98ps7n8fg3dxjpmb13i1"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)
        ("xproto" ,xproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xwud
  (package
    (name "xwud")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xwud-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1ggql6maivah58kwsh3z9x1hvzxm1a8888xx4s78cl77ryfa1cyn"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libx11" ,libx11)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
               "mirror://xorg/X11R7.7/src/everything/fixesproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1ki4wiq2iivx5g4w5ckzbjbap759kfqd72yg18m3zpbb4hqkybxs"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xextproto" ,xextproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxdamage
  (package
    (name "libxdamage")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXdamage-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1bamagq7g6s0d23l8rb3nppj8ifqj05f7z9bhbs4fdg8az3ffgvw"))))
    (build-system gnu-build-system)
    (propagated-inputs  
      ;; These are all in the Requires or Requires.private field of xdamage.pc
      `(("damageproto" ,damageproto)
        ("libxfixes" ,libxfixes)
        ("xproto" ,xproto)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
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
      `(("xextproto" ,xextproto)))
    (inputs
      `(("libxau" ,libxau)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxinerama
  (package
    (name "libxinerama")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXinerama-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1qlqfvzw45gdzk9xirgwlp2qgj0hbsyiqj8yh8zml2bk2ygnjibs"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xineramaproto" ,xineramaproto)))
    (inputs
      `(("libxext" ,libxext)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxp
  (package
    (name "libxp")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXp-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1dfh5w8sjz5b5fl6dl4y63ckq99snslz7bir8zq2rg8ax6syabwm"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("printproto" ,printproto)))
    (inputs
      `(("libx11" ,libx11)
        ("libxext" ,libxext)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxrender
  (package
    (name "libxrender")
    (version "0.9.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXrender-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0qpwyjhbpp734vnhca992pjh4w7ijslidkzx1pcwbbk000pv050x"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("renderproto" ,renderproto)))
    (inputs
      `(("xproto" ,xproto)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxtst
  (package
    (name "libxtst")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXtst-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1ngn161nq679ffmbwl81i2hn75jjg5b3ffv6n4jilpvyazypy2pg"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("recordproto" ,recordproto)))
    (inputs
      `(("libxi" ,libxi)
        ("libx11" ,libx11)
        ("inputproto" ,inputproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxv
  (package
    (name "libxv")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXv-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "09a5j6bisysiipd0nw6s352565bp0n6gbyhv5hp63s3cd3w95zjm"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("videoproto" ,videoproto)))
    (inputs
      `(("xproto" ,xproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public mkfontdir
  (package
    (name "mkfontdir")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/mkfontdir-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0c3563kw9fg15dpgx4dwvl12qz6sdqdns1pxa574hc7i5m42mman"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("mkfontscale" ,mkfontscale)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xproto
  (package
    (name "xproto")
    (version "7.0.26")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ksi8vhfd916bx2f3xlyhn6azf6cvvzrsdja26haa1cqfp0n4qb3"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("util-macros" ,util-macros))) ; to get util-macros in (almost?) all package inputs
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))



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
      `(("xproto" ,xproto)))
    (inputs
      `(("xtrans" ,xtrans)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxau
  (package
    (name "libxau")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXau-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1wm4pv12f36cwzhldpp7vy3lhm3xdcnp4f184xkxsp7b18r7gm7x"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("xproto" ,xproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))

(define-public libxfixes
  (package
    (name "libxfixes")
    (version "5.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXfixes-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0rs7qgzr6dpr62db7sd91c1b47hzhzfr010qwnpcm8sg122w1gk3"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("fixesproto" ,fixesproto)))
    (inputs
      `(("xproto" ,xproto)
        ("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxfont
  (package
    (name "libxfont")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXfont-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0py2c498lrq6wrj9al6nj57v2ypid9cz0zzhc0hjndgrmp254g1s"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("fontsproto" ,fontsproto)
        ("freetype" ,freetype)
        ("libfontenc" ,libfontenc)
        ("xproto" ,xproto)))
    (inputs
      `(("zlib" ,zlib)
        ("xtrans" ,xtrans)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxi
  (package
    (name "libxi")
    (version "1.7.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXi-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0i12lj973grlp9fa79v0vh9cahk3nf9csdjnf81iip0qcrlc5zrc"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("inputproto" ,inputproto)
        ("libx11" ,libx11)
        ("libxext" ,libxext)
        ("libxfixes" ,libxfixes)))
    (inputs
      `(("xproto" ,xproto)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxrandr
  (package
    (name "libxrandr")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXrandr-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1b95p3l84ppv6j7dbbmg0zrz6k8xdwvnag1l6ajm3gk9qwdb79ya"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libxext" ,libxext)
        ("randrproto" ,randrproto)))
    (inputs
      `(("libxrender" ,libxrender)
        ("xproto" ,xproto)
        ("libx11" ,libx11)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxvmc
  (package
    (name "libxvmc")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXvMC-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "015jk3bxfmj6zaw99x282f9npi8qqaw34yg186frags3z8g406jy"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libxv" ,libxv)))
    (inputs
      `(("xproto" ,xproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxxf86vm
  (package
    (name "libxxf86vm")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXxf86vm-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1f1pxj018nk7ybxv58jmn5y8gm2288p4q3l2dng9n1p25v1qcpns"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libxext" ,libxext)
        ("xf86vidmodeproto" ,xf86vidmodeproto)))
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; packages of height 3 in the propagated-inputs tree

(define-public libxcb
  (package
    (name "libxcb")
    (version "1.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://xcb.freedesktop.org/dist/libxcb-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1xqgc81krx14f2c8yl5chzg5g2l26mhm2rwffy8dx7jv0iq5sqq3"))))
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
        ("python" ,python-wrapper)))
    (arguments
     `(#:configure-flags '("--enable-xkb")))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xorg-server
  (package
    (name "xorg-server")
    (version "1.12.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xorg-server-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1xf57hcq6r17zxyfnx9r1wd0ir1bw13ff8bsiszwrw9jyhi9x7ya"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("dri2proto" ,dri2proto)
        ("fontsproto" ,fontsproto)
        ("inputproto" ,inputproto)
        ("kbproto" ,kbproto)
        ("libpciaccess" ,libpciaccess)
        ("pixman" ,pixman)
        ("randrproto" ,randrproto)
        ("renderproto" ,renderproto)
        ("videoproto" ,videoproto)
        ("xextproto" ,xextproto)
        ("xineramaproto" ,xineramaproto)
        ("xproto" ,xproto)))
    (inputs
      `(("bigreqsproto" ,bigreqsproto)
        ("compositeproto" ,compositeproto)
        ("damageproto" ,damageproto)
        ("dbus" ,dbus)
        ("dmxproto" ,dmxproto)
        ("libdmx" ,libdmx)
        ("libgcrypt" ,libgcrypt)
        ("libxau" ,libxau)
        ("libxaw" ,libxaw)
        ("libxdmcp" ,libxdmcp)
        ("libxfixes" ,libxfixes)
        ("libxfont" ,libxfont)
        ("libxkbfile" ,libxkbfile)
        ("libxrender" ,libxrender)
        ("libxres" ,libxres)
        ("libxt" ,libxt)
        ("libxv" ,libxv)
        ("mesa" ,mesa)
        ("recordproto" ,recordproto)
        ("resourceproto" ,resourceproto)
        ("scrnsaverproto" ,scrnsaverproto)
        ("xcmiscproto" ,xcmiscproto)
        ("xf86bigfontproto" ,xf86bigfontproto)
        ("xf86dgaproto" ,xf86dgaproto)
        ("xf86driproto" ,xf86driproto)
        ("xf86vidmodeproto" ,xf86vidmodeproto)
        ("xkbcomp" ,xkbcomp)
        ("xkeyboard-config" ,xkeyboard-config)
        ("xtrans" ,xtrans)
        ("zlib" ,zlib)))
    (native-inputs
       `(("python" ,python-wrapper)
         ("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-xkb-path="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb")
             (string-append "--with-xkb-output="
                            "/tmp") ; FIXME: This is a bit doubtful; where should
                                    ; the compiled keyboard maps go?
             (string-append "--with-xkb-bin-directory="
                            (assoc-ref %build-inputs "xkbcomp")
                            "/bin")

             ;; For the log file, etc.
             "--localstatedir=/var")
       #:phases
        (alist-replace
         'configure
         (lambda* (#:key outputs #:allow-other-keys #:rest args)
           (let ((configure (assoc-ref %standard-phases 'configure)))
             (substitute* (find-files "." "\\.c$")
               (("/bin/sh") (which "sh")))

             ;; Don't try to 'mkdir /var'.
             (substitute* "hw/xfree86/Makefile.in"
               (("mkdir(.*)logdir.*")
                "true\n"))

             (apply configure args)))
         %standard-phases)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))



;; packages of height 4 in the propagated-inputs tree

(define-public libx11
  (package
    (name "libx11")
    (version "1.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libX11-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "05mx0s0vqzds3qjc1gmjr2s6x2ll37z4lfhgm7p2w7936zl2g81a"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("kbproto" ,kbproto)
        ("libxcb" ,libxcb)))
    (inputs
      `(("inputproto" ,inputproto)
        ("xextproto" ,xextproto)
        ("xtrans" ,xtrans)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


;; packages of height 5 in the propagated-inputs tree

(define-public libxcursor
  (package
    (name "libxcursor")
    (version "1.1.14")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXcursor-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1prkdicl5y5yx32h1azh6gjfbijvjp415javv8dsakd13jrarilv"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libx11" ,libx11)
        ("libxrender" ,libxrender)
        ("libxfixes" ,libxfixes)
        ("xproto" ,xproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxt
  (package
    (name "libxt")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXt-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0myxwbx9ylam5x3ia5b5f4x8azcqdm420h9ad1r4hrgmi2lrffl4"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libx11" ,libx11)
        ("libice" ,libice)
        ("libsm" ,libsm)))
    (inputs
      `(("libx11" ,libx11)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public libxaw
  (package
    (name "libxaw")
    (version "1.0.12")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXaw-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1xnv7jy86j9vhmw74frkzcraynqbw1p1s79jasargsgwfi433z4n"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libxext" ,libxext)
        ("libxmu" ,libxmu)
        ("libxpm" ,libxpm)
        ("libxt" ,libxt)))
    (inputs
      `(("xproto" ,xproto)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))


(define-public xcb-util
  (package
    (name "xcb-util")
    (version "0.4.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://xcb.freedesktop.org/dist/" name "-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1sahmrgbpyki4bb72hxym0zvxwnycmswsxiisgqlln9vrdlr9r26"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libxcb" ,libxcb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://cgit.freedesktop.org/xcb/util/")
    (synopsis "Core XCB utility functions")
    (description "The XCB util module provides a number of libraries which
    sit on top of libxcb, the core X protocol library, and some of the
    extension libraries.  These experimental libraries provide convenience
functions and interfaces which make the raw X protocol more usable.  Some of
the libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util module provides the following libraries:
aux: Convenient access to connection setup and some core requests.
atom: Standard core X atom constants and atom caching.
event: Some utilities that have little to do with events any more.")
    (license license:x11)))


(define-public xcb-util-image
  (package
    (name "xcb-util-image")
    (version "0.4.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://xcb.freedesktop.org/dist/" name "-"
                                 version ".tar.bz2"))
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
    (home-page "http://cgit.freedesktop.org/xcb/util-image/")
    (synopsis "XCB port of Xlib's XImage and XShmImage")
    (description "The XCB util module provides a number of libraries which
    sit on top of libxcb, the core X protocol library, and some of the
    extension libraries.  These experimental libraries provide convenience
functions and interfaces which make the raw X protocol more usable.  Some of
the libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-image module provides the following library:
image: Port of Xlib's XImage and XShmImage functions.")
    (license license:x11)))


(define-public xcb-util-keysyms
  (package
    (name "xcb-util-keysyms")
    (version "0.4.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://xcb.freedesktop.org/dist/" name "-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1nbd45pzc1wm6v5drr5338j4nicbgxa5hcakvsvm5pnyy47lky0f"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libxcb" ,libxcb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://cgit.freedesktop.org/xcb/util-keysyms/")
    (synopsis "Standard X constants and conversion to/from keycodes")
    (description "The XCB util module provides a number of libraries which
    sit on top of libxcb, the core X protocol library, and some of the
    extension libraries.  These experimental libraries provide convenience
functions and interfaces which make the raw X protocol more usable.  Some of
the libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-keysyms module provides the following library:
keysyms: Standard X key constants and conversion to/from keycodes.")
    (license license:x11)))


(define-public xcb-util-renderutil
  (package
    (name "xcb-util-renderutil")
    (version "0.3.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://xcb.freedesktop.org/dist/" name "-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0nza1csdvvxbmk8vgv8vpmq7q8h05xrw3cfx9lwxd1hjzd47xsf6"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libxcb" ,libxcb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://cgit.freedesktop.org/xcb/util-renderutil/")
    (synopsis "SConvenience functions for the Render extension")
    (description "The XCB util module provides a number of libraries which
    sit on top of libxcb, the core X protocol library, and some of the
    extension libraries.  These experimental libraries provide convenience
functions and interfaces which make the raw X protocol more usable.  Some of
the libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-renderutil module provides the following library:
renderutil: Convenience functions for the Render extension.")
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
    (home-page "http://cgit.freedesktop.org/xcb/util-wm/")
    (synopsis "Client and window-manager helpers for ICCCM and EWMH")
    (description "The XCB util modules provides a number of libraries which
    sit on top of libxcb, the core X protocol library, and some of the
    extension libraries.  These experimental libraries provide convenience
functions and interfaces which make the raw X protocol more usable.  Some of
the libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-wm module provides the following libraries:
ewmh: Both client and window-manager helpers for EWMH.
icccm: Both client and window-manager helpers for ICCCM.")
    (license license:x11)))


;; package outside the x.org system proper of height 5

(define-public libxaw3d
  (package
    (name "libxaw3d")
    (version "1.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXaw3d-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0awplv1nf53ywv01yxphga3v6dcniwqnxgnb0cn4khb121l12kxp"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("libxext" ,libxext)
        ("libxmu" ,libxmu)
        ("libxt" ,libxt)))
    (inputs
     `(("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description "X.org provides an implementation of the X Window System")
    (license license:x11)))

(define-public xterm
  (package
    (name "xterm")
    (version "304")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.invisible-island.net/xterm/"
                                  "xterm-" version ".tgz"))
              (sha256
               (base32
                "19yp5phfzzgydc2yqka4p69ygvfzsd2aa98hbw086xyjlws3kbyk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-wide-chars" "--enable-256-color"
                           "--enable-load-vt-fonts" "--enable-i18n"
                           "--enable-doublechars" "--enable-luit"
                           "--enable-mini-luit")
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
       ("xproto" ,xproto)
       ("libXaw" ,libxaw)))
    (home-page "http://invisible-island.net/xterm")
    (synopsis "Terminal emulator for the X Window System")
    (description
     "The xterm program is a terminal emulator for the X Window System.  It
provides DEC VT102/VT220 (VTxxx) and Tektronix 4014 compatible terminals for
programs that cannot use the window system directly.")
    (license license:x11)))

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
    (home-page
     (string-append "http://search.cpan.org/~smccam/X11-Protocol-" version))
    ;; From the package README: "you can redistribute and/or modify it under
    ;; the same terms as Perl itself. (As an exception, the file
    ;; Keysyms.pm,which is derived from a file in the standard X11
    ;; distribution, has another, less restrictive copying policy, as do some
    ;; of the extension modules in the directory Protocol/Ext: see those files
    ;; for details)."
    (license (package-license perl))))
