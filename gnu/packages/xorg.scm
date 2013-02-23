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

(define-module (gnu packages xorg)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public applewmproto
  (package
    (name "applewmproto")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/applewmproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1zi4p07mp6jmk030p4gmglwxcwp0lzs5mi31y1b4rp8lsqxdxizw"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public bdftopcf
  (package
    (name "bdftopcf")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/bdftopcf-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "02hx981f7jfwylxj21s91yvv4h597nqqzz3vd6ar81zyn84b944w"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxfont" ,libxfont)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs
      `(("fixesproto" ,fixesproto)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs
      `(("xextproto" ,xextproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public font-bh-ttf
  (package
    (name "font-bh-ttf")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-bh-ttf-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0pyjmc0ha288d4i4j0si4dh3ncf3jiwwjljvddrb0k8v4xiyljqv"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontscale" ,mkfontscale)
        ("mkfontdir" ,mkfontdir)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public font-cursor-misc
  (package
    (name "font-cursor-misc")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-cursor-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0dd6vfiagjc4zmvlskrbjz85jfqhf060cpys8j0y1qpcbsrkwdhp"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontscale" ,mkfontscale)
        ("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public font-daewoo-misc
  (package
    (name "font-daewoo-misc")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-daewoo-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1s2bbhizzgbbbn5wqs3vw53n619cclxksljvm759h9p1prqdwrdw"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public font-jis-misc
  (package
    (name "font-jis-misc")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-jis-misc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0rdc3xdz12pnv951538q6wilx8mrdndpkphpbblszsv7nc8cw61b"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontdir" ,mkfontdir)
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
      `(("mkfontscale" ,mkfontscale)
        ("mkfontdir" ,mkfontdir)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public font-misc-meltho
  (package
    (name "font-misc-meltho")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/font-misc-meltho-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "148793fqwzrc3bmh2vlw5fdiwjc2n7vs25cic35gfp452czk489p"))))
    (build-system gnu-build-system)
    (inputs
      `(("mkfontscale" ,mkfontscale)
        ("mkfontdir" ,mkfontdir)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
      `(("mkfontscale" ,mkfontscale)
        ("mkfontdir" ,mkfontdir)
        ("font-util" ,font-util)
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
      `(("mkfontscale" ,mkfontscale)
        ("mkfontdir" ,mkfontdir)
        ("font-util" ,font-util)
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public fontsproto
  (package
    (name "fontsproto")
    (version "2.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/fontsproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1ab8mbqxdwvdz4k5x4xb9c4n5w7i1xw276cbpk4z7a1nlpjrg746"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("bdftopcf" ,bdftopcf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public gccmakedep
  (package
    (name "gccmakedep")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/util/gccmakedep-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "04dfamx3fvkvqfgs6xy2a6yqbxjrj4777ylxp38g60hhbdl4jg86"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public glproto
  (package
    (name "glproto")
    (version "1.4.16")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/glproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "13arnb4bz5pn89bxbh3shr8gihkhyznpjnq3zzr05msygwx6dpal"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public iceauth
  (package
    (name "iceauth")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/iceauth-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1aq6v671s2x5rc6zn0rgxb4wddg4vq94mckw3cpwl7ccrjjvd5hl"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libICE" ,libICE)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public imake
  (package
    (name "imake")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/util/imake-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1h8ww97aymm10l9qn21n1b9x5ypjrqr10qpf48jjcbc9fg77gklr"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto) ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public inputproto
  (package
    (name "inputproto")
    (version "2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/inputproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1c5wqamfsd8g5i8kya5pjfmcac8q5zq1l3vclh6p96f24nmicxfy"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libAppleWM
  (package
    (name "libAppleWM")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libAppleWM-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0r8x28n45q89x91mz8mv0zkkcxi8wazkac886fyvflhiv2y8ap2y"))))
    (build-system gnu-build-system)
    (inputs
      `(("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("applewmproto" ,applewmproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libFS
  (package
    (name "libFS")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libFS-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "05c3bqgn5m7j4kx8wvy0p36faq6f9jv5yq12m6033m4lflg7cwvh"))))
    (build-system gnu-build-system)
    (inputs
      `(("xtrans" ,xtrans)
        ("xproto" ,xproto)
        ("fontsproto" ,fontsproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libICE
  (package
    (name "libICE")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libICE-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07mp13pb3s73kj7y490gnx619znzwk91mlf8kdw0rzq29ll93a94"))))
    (build-system gnu-build-system)
    (inputs
      `(("xtrans" ,xtrans)
        ("xproto" ,xproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libSM
  (package
    (name "libSM")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libSM-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07bzi6xwlhq36f60qfspjbz0qjj7zcgayi1vp4ihgx34kib1vhck"))))
    (build-system gnu-build-system)
    (inputs
      `(("xtrans" ,xtrans)
        ("xproto" ,xproto)
        ("util-linux" ,util-linux)
        ("libICE" ,libICE)
        ("pkg-config" ,pkg-config)))
    (propagated-inputs
      `(("libICE" ,libICE)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libWindowsWM
  (package
    (name "libWindowsWM")
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
        ("windowswmproto" ,windowswmproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxScrnSaver
  (package
    (name "libxScrnSaver")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxScrnSaver-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07ff4r20nkkrj7h08f9fwamds9b3imj8jz5iz6y38zqw6jkyzwcg"))))
    (build-system gnu-build-system)
    (inputs
      `(("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("scrnsaverproto" ,scrnsaverproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxau
  (package
    (name "libxau")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxau-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "12d4f7sdv2pjxhk0lcay0pahccddszkw579dc59daqi37r8bllvi"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto) ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxaw
  (package
    (name "libxaw")
    (version "1.0.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxaw-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "14ll7ndf5njc30hz2w197qvwp7fqj7y14wq4p1cyxlbipfn79a47"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("xproto" ,xproto)
        ("libxpm" ,libxpm)
        ("libxmu" ,libxmu)
        ("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (propagated-inputs
      `(("libxmu" ,libxmu)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxcomposite
  (package
    (name "libxcomposite")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxcomposite-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1b8sniijb85v4my6v30ma9yqnwl4hkclci9l1hqxnipfyhl4sa9j"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxfixes" ,libxfixes)
        ("libx11" ,libx11)
        ("compositeproto" ,compositeproto)
        ("pkg-config" ,pkg-config)))
    (propagated-inputs
      `(("libxfixes" ,libxfixes)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxcursor
  (package
    (name "libxcursor")
    (version "1.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxcursor-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "13xd1dyb06gwdwb0bxb22fkgdlmis6wrljm2xk6fhz0v9bg2g27p"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("xproto" ,xproto)
        ("libxfixes" ,libxfixes)
        ("libx11" ,libx11)
        ("fixesproto" ,fixesproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxdamage
  (package
    (name "libxdamage")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxdamage-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1a678bwap74sqczbr2z4y4fvbr35km3inkm8bi1igjyk4v46jqdw"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxfixes" ,libxfixes)
        ("xextproto" ,xextproto)
        ("libx11" ,libx11)
        ("fixesproto" ,fixesproto)
        ("damageproto" ,damageproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxdmcp
  (package
    (name "libxdmcp")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxdmcp-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "13highx4xpgkiwykpcl7z2laslrjc4pzi4h617ny9p7r6116vkls"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto) ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxext
  (package
    (name "libxext")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxext-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ng8clhn7srbkadxjc7ih3z3v27v9ny0aa0dqkgddgxpgrhrq8jn"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxau" ,libxau)
        ("xproto" ,xproto)
        ("xextproto" ,xextproto)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (propagated-inputs
      `(("xproto" ,xproto)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxfixes
  (package
    (name "libxfixes")
    (version "5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxfixes-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1qx2rmwhmca2n7rgafy0arp15k5vwhdhhh6v6mx76hlj29328yjk"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("xextproto" ,xextproto)
        ("libx11" ,libx11)
        ("fixesproto" ,fixesproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxfont
  (package
    (name "libxfont")
    (version "1.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxfont-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0w3irg00k6b6mziddnacln9q2rkf5848b04nvjqwv5bb1fw6zydv"))))
    (build-system gnu-build-system)
    (inputs
      `(("zlib" ,zlib)
        ("xtrans" ,xtrans)
        ("xproto" ,xproto)
        ("freetype" ,freetype)
        ("fontsproto" ,fontsproto)
        ("libfontenc" ,libfontenc)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxft
  (package
    (name "libxft")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxft-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1gdv6559cdz1lfw73x7wsvax1fkvphmayrymprljhyyb5nwk5kkz"))))
    (build-system gnu-build-system)
    (inputs
      `(("renderproto" ,renderproto)
        ("libx11" ,libx11)
        ("xproto" ,xproto)
        ("libxrender" ,libxrender)
        ("xproto" ,xproto)
        ("libx11" ,libx11)
        ("freetype" ,freetype)
        ("fontconfig" ,fontconfig)
        ("pkg-config" ,pkg-config)))
    (propagated-inputs
      `(("fontconfig" ,fontconfig)
        ("freetype" ,freetype)
        ("libxrender" ,libxrender)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxi
  (package
    (name "libxi")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxi-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "029ihw4jq8mng8rx7a3jdvq64jm1zdkqidca93zmxv4jf9yn5qzj"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("inputproto" ,inputproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxinerama
  (package
    (name "libxinerama")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxinerama-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1b3aq1762hxzchd9ndavdjlksq93991s0g2z6spf8wl3v0pprrx4"))))
    (build-system gnu-build-system)
    (inputs
      `(("xineramaproto" ,xineramaproto)
        ("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxmu
  (package
    (name "libxmu")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxmu-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1pbym8rrznxqd60zwf7w4xpf27sa72bky2knginqcfnca32q343h"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("xproto" ,xproto)
        ("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxp
  (package
    (name "libxp")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libxp-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1lj3cjg9ygbmclxvayy5v88kkndpy9jq6y68p13dc5jn01hg5lbi"))))
    (build-system gnu-build-system)
    (inputs
      `(("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libxau" ,libxau)
        ("libx11" ,libx11)
        ("printproto" ,printproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxpm
  (package
    (name "libxpm")
    (version "3.5.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxpm-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0dd737ch4q9gr151wff1m3q2j7wf3pip4y81601xdrsh8wipxnx6"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("xproto" ,xproto)
        ("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxrandr
  (package
    (name "libxrandr")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libxrandr-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1hzm2ndra4nf8xxzm4lzd225zj05hzbzcq464q2znah15ynd0fh3"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("xproto" ,xproto)
        ("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("renderproto" ,renderproto)
        ("randrproto" ,randrproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxrender
  (package
    (name "libxrender")
    (version "0.9.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxrender-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1rmvja2gkf5v0k2n1bcghw8v98m2kfn3af0rbmsda5dwr69npd7r"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libx11" ,libx11)
        ("renderproto" ,renderproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxres
  (package
    (name "libxres")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxres-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1478pm70gdi6l70r4jpkyyg2am9wv6xh53z9ibwq5cg84p4n31pz"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("resourceproto" ,resourceproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxt
  (package
    (name "libxt")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxt-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1g85gwnhs7lg5f01gfi1cpb916xc3spm1fjlv2f4xz2zzk1r7dcd"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libx11" ,libx11)
        ("libSM" ,libSM)
        ("kbproto" ,kbproto)
        ("libICE" ,libICE)
        ("pkg-config" ,pkg-config)))
    (propagated-inputs
      `(("libSM" ,libSM)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxtst
  (package
    (name "libxtst")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxtst-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1q750hjplq1rfyxkr4545z1y2a1wfnc828ynvbws7b4jwdk3xsky"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxi" ,libxi)
        ("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("recordproto" ,recordproto)
        ("inputproto" ,inputproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxv
  (package
    (name "libxv")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxv-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "044hllz013afhzywwpxz007l4zjy99bv9im065rqd30zckmllrjx"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("videoproto" ,videoproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxvMC
  (package
    (name "libxvMC")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libxvMC-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "18yf6ysc01pqkbk9704914ghalq1sl2hfdjmwggxm8qqhpy8bw18"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxv" ,libxv)
        ("xproto" ,xproto)
        ("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("videoproto" ,videoproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libdmx
  (package
    (name "libdmx")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libdmx-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1fiq73sfxcbyjval118ialwrzxhzb08xsxmg69adcs47i9j0p1x7"))))
    (build-system gnu-build-system)
    (inputs
      `(("xextproto" ,xextproto)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("dmxproto" ,dmxproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libfontenc
  (package
    (name "libfontenc")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libfontenc-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0zq1483xy31sssq0h3xxf8y1v4q14cp8rv164ayn7fsn30pq2wny"))))
    (build-system gnu-build-system)
    (inputs
      `(("zlib" ,zlib)
        ("xproto" ,xproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libpciaccess
  (package
    (name "libpciaccess")
    (version "0.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libpciaccess-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "11509lkgd5j4g5wy0g13z4sf31h50hqx3jfwb2i4q6k98pv6iar7"))))
    (build-system gnu-build-system)
    (inputs
      `(("zlib" ,zlib) ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libpthread-stubs
  (package
    (name "libpthread-stubs")
    (version "0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/libpthread-stubs-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "16bjv3in19l84hbri41iayvvg4ls9gv1ma0x0qlbmwy67i7dbdim"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public libxcb
  (package
    (name "libxcb")
    (version "1.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/xcb/libxcb-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "15icn78x610dvvgnji6b3pyn8nd88lz68hq0w73pcadf78mycmw8"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxdmcp" ,libxdmcp)
        ("xcb-proto" ,xcb-proto)
        ("libxau" ,libxau)
        ("libpthread-stubs" ,libpthread-stubs)
        ("libxslt" ,libxslt)
        ("pkg-config" ,pkg-config)
        ("python" ,python)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
      `(("libx11" ,libx11)
        ("kbproto" ,kbproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public lndir
  (package
    (name "lndir")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/util/lndir-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0pdngiy8zdhsiqx2am75yfcl36l7kd7d7nl0rss8shcdvsqgmx29"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto) ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
            "0dn694mk56x6hdk6y9ylx4f128h5jcin278gnw2gb807rf3ygc1h"))))
    (build-system gnu-build-system)
    (inputs
      `(("libfontenc" ,libfontenc)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public makedepend
  (package
    (name "makedepend")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/makedepend-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1zpp2b9dfvlnfj2i1mzdyn785rpl7vih5lap7kcpiv80xspbhmmb"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto) ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public mkfontscale
  (package
    (name "mkfontscale")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/mkfontscale-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1539h3ws66vcql6sf2831bcs0r4d9b05lcgpswkw33lvcxighmff"))))
    (build-system gnu-build-system)
    (inputs
      `(("zlib" ,zlib)
        ("xproto" ,xproto)
        ("freetype" ,freetype)
        ("libfontenc" ,libfontenc)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public pixman
  (package
    (name "pixman")
    (version "0.26.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/pixman-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0z34jb75wpbyj3gxn34icd8j81fk5d6s6qnwp2ncz7m8icf6afqr"))))
    (build-system gnu-build-system)
    (inputs
      `(("pkg-config" ,pkg-config) ("perl" ,perl)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs
      `(("libxau" ,libxau) ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public sessreg
  (package
    (name "sessreg")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/sessreg-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0lifgjxdvc6lwyjk90slddnr12fsv88ldy6qhklr5av409cfwd47"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto) ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("libxmu" ,libxmu)
        ("libSM" ,libSM)
        ("libICE" ,libICE)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public twm
  (package
    (name "twm")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/twm-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0i6dbf5vafi5hm4bcmnj6r412cncjlv9hkkbr6bzlh15qvg56p8g"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("xproto" ,xproto)
        ("libxmu" ,libxmu)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("libSM" ,libSM)
        ("libICE" ,libICE)
        ("pkg-config" ,pkg-config)
        ("flex" ,flex)
        ("bison" ,bison)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public util-macros
  (package
    (name "util-macros")
    (version "1.17")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/util-macros-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1vbmrcn5n3wp4pyw0n4c3pyvzlc4yf7jzgngavfdq5zwfbgfsybx"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public videoproto
  (package
    (name "videoproto")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/videoproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0nk3i6gwkqq1w8zwn7bxz344pi1dwcjrmf6hr330h7hxjcj6viry"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xauth
  (package
    (name "xauth")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xauth-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1382wdfiakgckbw1xxavzh1nm34q21b1zzy96qp7ws66xc48rxw4"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libxext" ,libxext)
        ("libxau" ,libxau)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xbacklight
  (package
    (name "xbacklight")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xbacklight-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "199n9qszjiz82nbjz6ychh0xl15igm535mv0830wk4m59w9xclji"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("xcb-util" ,xcb-util)
        ("libxcb" ,libxcb)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xcb-proto
  (package
    (name "xcb-proto")
    (version "1.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/xcb/xcb-proto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1c11652h9sjynw3scm1pn5z3a6ci888pq7hij8q5n8qrl33icg93"))))
    (build-system gnu-build-system)
    (inputs
      `(("pkg-config" ,pkg-config) ("python" ,python)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xcb-util
  (package
    (name "xcb-util")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/xcb/xcb-util-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1i0qbhqkcdlbbsj7ifkyjsffl61whj24d3zlg5pxf3xj1af2a4f6"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxcb" ,libxcb)
        ("gperf" ,gperf)
        ("m4" ,m4)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xcb-util-image
  (package
    (name "xcb-util-image")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/xcb/xcb-util-image-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1pr1l1nkg197gyl9d0fpwmn72jqpxjfgn9y13q4gawg1m873qnnk"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("xcb-util" ,xcb-util)
        ("libxcb" ,libxcb)
        ("m4" ,m4)
        ("gperf" ,gperf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xcb-util-keysyms
  (package
    (name "xcb-util-keysyms")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/xcb/xcb-util-keysyms-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0vjwk7vrcfnlhiadv445c6skfxmdrg5v4qf81y8s2s5xagqarqbv"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxcb" ,libxcb)
        ("m4" ,m4)
        ("gperf" ,gperf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xcb-util-renderutil
  (package
    (name "xcb-util-renderutil")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/xcb/xcb-util-renderutil-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0lkl9ij9b447c0br2qc5qsynjn09c4fdz7sd6yp7pyi8az2sb2cp"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxcb" ,libxcb)
        ("m4" ,m4)
        ("gperf" ,gperf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xcb-util-wm
  (package
    (name "xcb-util-wm")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/xcb/xcb-util-wm-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0c30fj33gvwzwhyz1dhsfwni0ai16bxpvxb4l6c6s7vvj7drp3q3"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxcb" ,libxcb)
        ("m4" ,m4)
        ("gperf" ,gperf)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xclock
  (package
    (name "xclock")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xclock-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1l1zxr69p0734fnx9rdqw79ahr273hr050sm8xdc0n51n1bnzfr1"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("libxrender" ,libxrender)
        ("libxmu" ,libxmu)
        ("libxkbfile" ,libxkbfile)
        ("libxft" ,libxft)
        ("libxaw" ,libxaw)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
      `(("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xcursorgen
  (package
    (name "xcursorgen")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xcursorgen-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "10f5wk1326mm45gvgpf4m2p0j80fcd0i4c52zikahb91zah72wdw"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxcursor" ,libxcursor)
        ("libx11" ,libx11)
        ("libpng" ,libpng)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xcursor-themes
  (package
    (name "xcursor-themes")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xcursor-themes-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1is4bak0qkkhv63mfa5l7492r475586y52yzfxyv3psppn662ilr"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxcursor" ,libxcursor)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xdm
  (package
    (name "xdm")
    (version "1.1.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xdm-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0iqw11977lpr9nk1is4fca84d531vck0mq7jldwl44m0vrnl5nnl"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("libxpm" ,libxpm)
        ("libxmu" ,libxmu)
        ("libxinerama" ,libxinerama)
        ("libxft" ,libxft)
        ("libxext" ,libxext)
        ("libxdmcp" ,libxdmcp)
        ("libxaw" ,libxaw)
        ("libxau" ,libxau)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xdpyinfo
  (package
    (name "xdpyinfo")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xdpyinfo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0gypsvpmay3lsh3b1dg29pjxv95pkrr21d4w6ys02mrbld24kvi3"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxxf86vm" ,libxxf86vm)
        ("libxxf86misc" ,libxxf86misc)
        ("libxxf86dga" ,libxxf86dga)
        ("libxtst" ,libxtst)
        ("libxrender" ,libxrender)
        ("libxinerama" ,libxinerama)
        ("libxi" ,libxi)
        ("libxext" ,libxext)
        ("libxcomposite" ,libxcomposite)
        ("libxcb" ,libxcb)
        ("libx11" ,libx11)
        ("libdmx" ,libdmx)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("glproto" ,glproto)
        ("libx11" ,libx11)
        ("glproto" ,glproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xev
  (package
    (name "xev")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xev-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "13xk5z7vy87rnn4574z0jfzymdivyc7pl4axim81sx0pmdysg1ip"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("libxrandr" ,libxrandr)
        ("xproto" ,xproto)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xextproto
  (package
    (name "xextproto")
    (version "7.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xextproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "06kdanbnprxvgl56l5h0lqj4b0f1fbb1ndha33mv5wvy802v2lvw"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xeyes
  (package
    (name "xeyes")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xeyes-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "08d5x2kar5kg4yammw6hhk10iva6jmh8cqq176a1z7nm1il9hplp"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("libxrender" ,libxrender)
        ("libxmu" ,libxmu)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xfs
  (package
    (name "xfs")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xfs-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "17g34yq789grnic83cqj5khq0knda1w2rgabhjflsyw9wg663shd"))))
    (build-system gnu-build-system)
    (inputs
      `(("xtrans" ,xtrans)
        ("xproto" ,xproto)
        ("libxfont" ,libxfont)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xhost
  (package
    (name "xhost")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xhost-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0l483y6wfrjh37j16b41kpi2nc7ss5rvndafpbaylrs87ygx2w18"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libxau" ,libxau)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xinit
  (package
    (name "xinit")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xinit-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0d821rlqwyn2js7bkzicyp894n9gqv1hahxs285pas1zm3d7z1m1"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (propagated-inputs
      `(("xauth" ,xauth)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xinput
  (package
    (name "xinput")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xinput-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0zl4cdgnzh9shz20yn7hz889v4nkbyqwx0nb7dh6arn7abchgc2a"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("libxrandr" ,libxrandr)
        ("libxinerama" ,libxinerama)
        ("libxi" ,libxi)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("inputproto" ,inputproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xkbcomp
  (package
    (name "xkbcomp")
    (version "1.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xkbcomp-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0bas1d2wjiy5zy9d0g92d2p9pwv4aapfbfidi7hxy8ax8jmwkl4i"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxkbfile" ,libxkbfile)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xkbutils
  (package
    (name "xkbutils")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xkbutils-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1ga913pw6chssf2016kjyjl6ar2lj83pa497w97ak2kq603sy2g4"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("xproto" ,xproto)
        ("libxaw" ,libxaw)
        ("libx11" ,libx11)
        ("inputproto" ,inputproto)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xkeyboard-config
  (package
    (name "xkeyboard-config")
    (version "2.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xkeyboard-config-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1nmb7ma8rqryicc5xqrn2hm5pwp5lkf7nj28bwbf63mz2r0mk892"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xkill
  (package
    (name "xkill")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xkill-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1ac110qbb9a4x1dim3vaghvdk3jc708i2p3f4rmag33458khg0xx"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
      `(("libxcb" ,libxcb) ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xlsclients
  (package
    (name "xlsclients")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xlsclients-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1l97j15mg4wfzpm81wlpzagfjff7v4fwn7s2z2rpksk3gfcg7r8w"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxcb" ,libxcb) ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xmessage
  (package
    (name "xmessage")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xmessage-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0nrxidff0pcd1ampfzj91ai74j6mx613j5kqk3j0c4xdshx5v8yg"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("libxaw" ,libxaw)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xmodmap
  (package
    (name "xmodmap")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xmodmap-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1dg47lay4vhrl9mfq3cfc6741a0m2n8wd4ljagd21ix3qklys8pg"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xorg-cf-files
  (package
    (name "xorg-cf-files")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/util/xorg-cf-files-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0s86h66b3w4623m88fg2csp41cnr08qc8i3gkj85k3wpwj1wxs9n"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xorg-docs
  (package
    (name "xorg-docs")
    (version "1.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xorg-docs-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0prphdba6kgr1bxk7r07wxxx6x6pqjw6prr5qclypsb5sf5r3cdr"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xorg-server
  (package
    (name "xorg-server")
    (version "1.12.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/xserver/xorg-server-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1xscr6rf0q15hv3hmm51xhwk0c0rx7a2swkj14ygp8vb60sprh4a"))))
    (build-system gnu-build-system)
    (inputs
      `(("bigreqsproto" ,bigreqsproto)
        ("xtrans" ,xtrans)
        ("xcmiscproto" ,xcmiscproto)
        ("damageproto" ,damageproto)
        ("libxfont" ,libxfont)
        ("pixman" ,pixman)
        ("libxext" ,libxext)
        ("recordproto" ,recordproto)
        ("xf86vidmodeproto" ,xf86vidmodeproto)
        ("libdmx" ,libdmx)
        ("dmxproto" ,dmxproto)
        ("xf86dgaproto" ,xf86dgaproto)
        ("xineramaproto" ,xineramaproto)
        ("resourceproto" ,resourceproto)
        ("scrnsaverproto" ,scrnsaverproto)
        ("compositeproto" ,compositeproto)
        ("xf86driproto" ,xf86driproto)
        ("glproto" ,glproto)
        ("xf86bigfontproto" ,xf86bigfontproto)
        ("dbus" ,dbus)
        ("mesa" ,mesa)
        ("systemd" ,systemd)
        ("zlib" ,zlib)
        ("libxv" ,libxv)
        ("libxt" ,libxt)
        ("libxres" ,libxres)
        ("libxrender" ,libxrender)
        ("libxpm" ,libxpm)
        ("libxmu" ,libxmu)
        ("libxkbfile" ,libxkbfile)
        ("libxfixes" ,libxfixes)
        ("libxdmcp" ,libxdmcp)
        ("libxaw" ,libxaw)
        ("libxau" ,libxau)
        ("libx11" ,libx11)
        ("openssl" ,openssl)
        ("libdrm" ,libdrm)
        ("renderproto" ,renderproto)
        ("pkg-config" ,pkg-config)))
    (propagated-inputs
      `(("kbproto" ,kbproto)
        ("dri2proto" ,dri2proto)
        ("randrproto" ,randrproto)
        ("xextproto" ,xextproto)
        ("inputproto" ,inputproto)
        ("libpciaccess" ,libpciaccess)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xprop
  (package
    (name "xprop")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xprop-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "18zi2any13zlb7f34fzyw6lkiwkd6k2scp3b800a1f4rj0c7m407"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xproto
  (package
    (name "xproto")
    (version "7.0.23")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "17lkmi12f89qvg4jj5spqzwzc24fmsqq68dv6kpy7r7b944lmq5d"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xrandr
  (package
    (name "xrandr")
    (version "1.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xrandr-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "03lq1c1q4w5cf2ijs4b34v008lshibha9zv5lw08xpyhk9xgyn8h"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxrender" ,libxrender)
        ("libxrandr" ,libxrandr)
        ("xproto" ,xproto)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xrdb
  (package
    (name "xrdb")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xrdb-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1dza5a34nj68fzhlgwf18i5bk0n24ig28yihwpjy7vwn57hh2934"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xrefresh
  (package
    (name "xrefresh")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xrefresh-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ywxzwa4kmnnmf8idr8ssgcil9xvbhnk155zpsh2i8ay93mh5586"))))
    (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xset
  (package
    (name "xset")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xset-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1s61mvscd0h7y6anljarj7nkii6plhs8ndx1fm8b1f1h00a1qdv1"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxxf86misc" ,libxxf86misc)
        ("xproto" ,xproto)
        ("libxmu" ,libxmu)
        ("libxext" ,libxext)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xsetroot
  (package
    (name "xsetroot")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xsetroot-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1bazzsf9sy0q2bj4lxvh1kvyrhmpggzb7jg575i15sksksa3xwc8"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxmu" ,libxmu)
        ("libxcursor" ,libxcursor)
        ("xbitmaps" ,xbitmaps)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xtrans
  (package
    (name "xtrans")
    (version "1.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xtrans-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "19p1bw3qyn0ia1znx6q3gx92rr9rl88ylrfijjclm8vhpa8i30bz"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xvinfo
  (package
    (name "xvinfo")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xvinfo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "119rd93d7661ll1rfcdssn78l0b97326smziyr2f5wdwj2hlmiv0"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxext" ,libxext)
        ("libxv" ,libxv)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xwd
  (package
    (name "xwd")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xwd-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0fkg6msy2zg7rda2rpxb7j6vmrdmqmk72xsxnyhz97196ykjnx82"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxt" ,libxt)
        ("xproto" ,xproto)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
(define-public xwininfo
  (package
    (name "xwininfo")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/X11R7.7/src/everything/xwininfo-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0fmcr5yl03xw7m8p9h1rk67rrj7gp5x16a547xhmg8idw2f6r9lg"))))
    (build-system gnu-build-system)
    (inputs
      `(("xproto" ,xproto)
        ("libxcb" ,libxcb)
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  
  
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
        ("libx11" ,libx11)
        ("pkg-config" ,pkg-config)))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
  