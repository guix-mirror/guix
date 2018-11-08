;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016 Jookia <166291@gmail.com>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Dmitry Nikolaev <cameltheman@gmail.com>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Toni Reina <areina@riseup.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.com>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Brendan Tildesley <brendan.tildesley@openmailbox.org>
;;; Copyright © 2017, 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Mohammed Sadiq <sadiq@sadiqpk.org>
;;; Copyright © 2018 Charlie Ritter <chewzerita@posteo.net>
;;; Copyright © 2018 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019 Jens Mølgaard <jens@zete.tk>
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

(define-module (gnu packages fonts)
  #:use-module (ice-9 regex)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg))

(define-public font-ibm-plex
  (package
    (name "font-ibm-plex")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/IBM/plex/releases/download/"
                    "v" version "/OpenType.zip"))
              (sha256
               (base32
                "0nzxw9z6waixslam248yr26ci3fbk83c7jf6m90hncnaj6zxx795"))))
    (build-system font-build-system)
    (home-page "https://github.com/IBM/plex")
    (synopsis "IBM Plex typeface")
    (description "This package provides the Plex font family.  It comes in a
Sans, Serif, Mono and Sans Condensed, all with roman and true italics.  The
fonts have been designed to work well in user interface (UI) environments as
well as other mediums.")
    (license license:silofl1.1)))

(define-public font-inconsolata
  (package
    (name "font-inconsolata")
    (version "0.80")
    (source (origin
              (method url-fetch)
              (uri "http://www.levien.com/type/myfonts/Inconsolata.otf")
              (sha256
               (base32
                "06js6znbcf7swn8y3b8ki416bz96ay7d3yvddqnvi88lqhbfcq8m"))))
    (build-system font-build-system)
    (home-page "http://levien.com/type/myfonts/inconsolata.html")
    (synopsis "Monospace font")
    (description "A monospace font, designed for code listings and the like,
in print.  With attention to detail for high resolution rendering.")
    (license license:silofl1.1)))

(define-public font-ubuntu
  (package
    (name "font-ubuntu")
    (version "0.83")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/fonts-team/fonts-ubuntu")
                    (commit (string-append "upstream/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d2xrjpxy70f3nsgqiggwv6pj06qglf5vj2847pqx60w3ygi903g"))))
    (build-system font-build-system)
    (home-page "http://font.ubuntu.com/")
    (synopsis "The Ubuntu Font Family")
    (description "The Ubuntu Font Family is a unique, custom designed font
that has a very distinctive look and feel.  This package provides the
TrueType (TTF) files.")
    (license
     (license:non-copyleft
      "http://font.ubuntu.com/ufl/ubuntu-font-licence-1.0.txt"
      "Ubuntu Font License v1.0"))))

(define-public font-dejavu
  (package
    (name "font-dejavu")
    (version "2.37")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/dejavu/dejavu/"
                                 version "/dejavu-fonts-ttf-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1mqpds24wfs5cmfhj57fsfs07mji2z8812i5c4pi5pbi738s977s"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-conf
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((conf-dir (string-append (assoc-ref outputs "out")
                                            "/share/fontconfig/conf.avail")))
               (copy-recursively "fontconfig" conf-dir)
               #t))))))
    (home-page "https://dejavu-fonts.github.io/")
    (synopsis "Vera font family derivate with additional characters")
    (description "DejaVu provides an expanded version of the Vera font family
aiming for quality and broader Unicode coverage while retaining the original
Vera style.  DejaVu currently works towards conformance to the Multilingual
European Standards (MES-1 and MES-2) for Unicode coverage.  The DejaVu fonts
provide serif, sans and monospaced variants.")
    (license
     (license:x11-style
      "http://dejavu-fonts.org/"))))

(define-public font-bitstream-vera
  (package
    (name "font-bitstream-vera")
    (version "1.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/ttf-bitstream-vera/"
                                 version "/ttf-bitstream-vera-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1p3qs51x5327gnk71yq8cvmxc6wgx79sqxfvxcv80cdvgggjfnyv"))))
    (build-system font-build-system)
    (home-page "https://www.gnome.org/fonts/")
    (synopsis "Bitstream Vera sans-serif typeface")
    (description "Vera is a sans-serif typeface from Bitstream, Inc.  This
package provides the TrueType (TTF) files.")
    (license
     (license:fsdg-compatible
      "https://www.gnome.org/fonts/#Final_Bitstream_Vera_Fonts"
      "The Font Software may be sold as part of a larger software package but
no copy of one or more of the Font Software typefaces may be sold by
itself."))))

(define-public font-cantarell
  (package
    (name "font-abattis-cantarell")
    (version "0.0.25")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/cantarell-fonts/"
                                  (version-major+minor version)
                                  "/cantarell-fonts-" version ".tar.xz"))
              (sha256
               (base32
                "0zvkd8cm1cg2919v1js9qmzwa02sjl7qajj3gcvgqvai1fm2i8hl"))))
    (build-system gnu-build-system)
    (home-page "https://wiki.gnome.org/Projects/CantarellFonts")
    (synopsis "Cantarell sans-serif typeface")
    (description "The Cantarell font family is a contemporary Humanist
sans-serif designed for on-screen reading.  It is used by GNOME@tie{}3.")
    (license license:silofl1.1)))

(define-public font-lato
  (package
    (name "font-lato")
    (version "2.010")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "http://www.latofonts.com/download/Lato2OFL.zip"))
              (sha256
               (base32
                "1f5540g0ja1nx3ddd3ywn77xc81ssrxpq8n3gyb9sabyq2b4xda2"))))
    (build-system font-build-system)
    (home-page "http://www.latofonts.com/lato-free-fonts/")
    (synopsis "Lato sans-serif typeface")
    (description
     "Lato is a sanserif typeface family.  It covers over 3000 glyphs per style.
The Lato 2.010 family supports more than 100 Latin-based languages, over
50 Cyrillic-based languages as well as Greek and IPA phonetics.")
    (license license:silofl1.1)))

(define-public font-gnu-freefont-ttf
  (package
    (name "font-gnu-freefont-ttf")
    (version "20120503")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/freefont/freefont-src-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0yk58blhcd4hm7nyincmqq4jrzjjk82wif2zmk1l3y2m4vif4qhd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'install
                   (lambda _
                     (let ((doc-dir  (string-append %output "/share/doc/"
                                                    ,name "-" ,version))
                           (font-dir (string-append %output
                                                    "/share/fonts/truetype")))
                       (mkdir-p doc-dir)
                       (substitute* "Makefile"
                         (("\\$\\(TMPDIR\\)") doc-dir)
                         (("sfd/\\*.ttf") ""))
                       (system* "make" "ttftar")
                       (mkdir-p font-dir)
                       (for-each (lambda (file)
                                   (install-file file font-dir))
                                 (filter
                                   (lambda (file) (string-suffix? "ttf" file))
                                   (find-files "." "")))))))
       #:test-target "tests"))
    ;; replace python 3 with python 2
    ;; python 3 support commits aren't yet released in 20120503
    ;; so freefont needs python 2 support in fontforge
    (native-inputs `(("fontforge" ,(package (inherit fontforge)
                                     (inputs `(("python-2" ,python-2)
                                     ,@(package-inputs fontforge)))))))
    (home-page "https://www.gnu.org/software/freefont/")
    (synopsis "Unicode-encoded outline fonts")
    (description
     "The GNU Freefont project aims to provide a set of free outline
 (PostScript Type0, TrueType, OpenType...) fonts covering the ISO
10646/Unicode UCS (Universal Character Set).")
    (license license:gpl3+)
    (properties '((upstream-name . "freefont")
                  (ftp-directory . "/gnu/freefont")))))

(define-public font-liberation
  (package
    (name "font-liberation")
    (version "2.00.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.pagure.org/"
                                  "liberation-fonts/liberation-fonts-ttf-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "010m4zfqan4w04b6bs9pm3gapn9hsb18bmwwgp2p6y6idj52g43q"))))
    (build-system font-build-system)
    (home-page "https://pagure.io/liberation-fonts/")
    (synopsis
     "Fonts compatible with Arial, Times New Roman, and Courier New")
    (description
     "The Liberation font family aims at metric compatibility with
Arial, Times New Roman, and Courier New.
There are three sets:

@enumerate
@item Sans (a substitute for Arial, Albany, Helvetica, Nimbus Sans L, and
Bitstream Vera Sans);
@item Serif (a substitute for Times New Roman, Thorndale, Nimbus Roman, and
Bitstream Vera Serif);
@item Mono (a substitute for Courier New, Cumberland, Courier, Nimbus Mono L,
and Bitstream Vera Sans Mono).
@end enumerate

The Liberation Fonts are sponsored by Red Hat.")
    (license license:silofl1.1)))

(define-public font-linuxlibertine
  (package
    (name "font-linuxlibertine")
    (version "5.3.0")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "mirror://sourceforge/linuxlibertine/"
                                  "linuxlibertine/" version
                                  "/LinLibertineSRC_" version "_2012_07_02.tgz"))
              (sha256
               (base32
                "0x7cz6hvhpil1rh03rax9zsfzm54bh7r4bbrq8rz673gl9h47v0v"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'build
           (lambda _
             (let ((compile
                    (lambda (name ext)
                      (invoke
                       "fontforge" "-lang=ff"
                       "-c" (string-append "Open('" name "');"
                                           "Generate('"
                                           (basename name "sfd") ext
                                           "')")))))
               (for-each (lambda (name)
                           (and (compile name "ttf")
                                (compile name "otf")))
                         (find-files "." "\\.sfd$"))
               #t))))))
    (native-inputs
     `(("fontforge" ,fontforge)))
    (home-page "http://www.linuxlibertine.org/")
    (synopsis "Serif and sans serif typefaces")
    (description "The Linux Libertine fonts is a set of typefaces containing
both a Serif version (\"Linux Libertine\") and a Sans Serif (\"Linux
Biolinum\") designed to be used together as an alternative for Times/Times New
Roman and Helvetica/Arial.  The Serif typeface comes in two shapes and two
weights, and with a Small Capitals version of the regular typeface.  Linux
Biolinum is available in both Regular and Bold weights.")
    ;; The fonts are released under either of these licenses.
    (license (list license:gpl2+ license:silofl1.1))))

(define-public font-terminus
  (package
    (name "font-terminus")
    (version "4.40")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://sourceforge/terminus-font/terminus-font-"
               version
               "/terminus-font-"
               version
               ".tar.gz"))
        (sha256
         (base32
          "0487cyx5h1f0crbny5sg73a22gmym5vk1i7646gy7hgiscj2rxb4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ("bdftopcf" ,bdftopcf)
       ("font-util" ,font-util)
       ("mkfontdir" ,mkfontdir)))
    (arguments
     `(#:configure-flags (list
                          ;; install fonts into subdirectory of package output
                          ;; instead of font-util-?.?.?/share/fonts/X11
                          (string-append "--with-fontrootdir="
                                         %output "/share/fonts/X11"))
       #:tests? #f)) ;; No test target in tarball
    (home-page "http://terminus-font.sourceforge.net/")
    (synopsis "Simple bitmap programming font")
    (description "Terminus Font is a clean, fixed-width bitmap font, designed
for long periods of working with computers (8 or more hours per day).")
    (license license:silofl1.1)))

(define-public font-adobe-source-han-sans
  (package
    (name "font-adobe-source-han-sans")
    (version "1.004")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/adobe-fonts/source-han-sans/archive/"
                    version "R.tar.gz"))
              (file-name (string-append "source-han-sans-" version "R.tar.gz"))
              (sha256
               (base32
                "1ssx0fw90sy6mj8fv8fv4dgzszpqwbmwpjnlx16g4pvaqzdmybbz"))))
    (outputs '("out"                 ; OpenType/CFF Collection (OTC), 121 MiB.
               "cn" "jp" "kr" "tw")) ; Region-specific Subset OpenType/CFF.
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((tar  (string-append (assoc-ref %build-inputs
                                               "tar")
                                    "/bin/tar"))
               (PATH (string-append (assoc-ref %build-inputs
                                               "gzip")
                                    "/bin"))
               (install-opentype-fonts
                (lambda (fonts-dir out)
                  (copy-recursively fonts-dir
                                    (string-append (assoc-ref %outputs out)
                                                   "/share/fonts/opentype")))))
           (setenv "PATH" PATH)
           (invoke tar "xvf" (assoc-ref %build-inputs "source"))
           (chdir (string-append "source-han-sans-" ,version "R"))
           (install-opentype-fonts "OTC" "out")
           (install-opentype-fonts "SubsetOTF/CN" "cn")
           (install-opentype-fonts "SubsetOTF/JP" "jp")
           (install-opentype-fonts "SubsetOTF/KR" "kr")
           (install-opentype-fonts "SubsetOTF/TW" "tw")
           (for-each delete-file (find-files %output "\\.zip$"))
           #t))))
    (native-inputs
     `(("gzip" ,gzip)
       ("tar" ,tar)))
    (home-page "https://github.com/adobe-fonts/source-han-sans")
    (synopsis "Pan-CJK fonts")
    (description
     "Source Han Sans is a sans serif Pan-CJK font family that is offered in
seven weights: ExtraLight, Light, Normal, Regular, Medium, Bold, and Heavy.
And in several OpenType/CFF-based deployment configurations to accommodate
various system requirements or limitations.  As the name suggests, Pan-CJK
fonts are intended to support the characters necessary to render or display
text in Simplified Chinese, Traditional Chinese, Japanese, and Korean.")
    (license license:silofl1.1)))

(define-public font-cns11643
  ;; Since upstream doesn't provide any version numbers, the date of the last
  ;; edit is used, taken from https://data.gov.tw/dataset/5961
  ;; XXX: The source is also updated in-place, so it may be desirable to mirror
  ;; it elsewhere to avoid suddenly losing the current source file.
  (package
    (name "font-cns11643")
    (version "98.1.20180605")
    (source (origin
              (method url-fetch)
              (uri "http://www.cns11643.gov.tw/AIDB/Open_Data.zip")
              (sha256
               (base32
                "000a9whrjr1cd4pjc23pbl60zwkq3wcb5g61p9qi7fn3hwkp0kyw"))))
    (build-system font-build-system)
    (home-page "http://www.cns11643.gov.tw/AIDB/welcome.do")
    (synopsis "CJK TrueType fonts, TW-Kai and TW-Sung")
    (description
     "@code{CNS 11643} character set (Chinese National Standard, or Chinese
Standard Interchange Code) is the standard character set of the Republic of
China (Taiwan) for Chinese Characters and other Unicode symbols.  Contained
are six TrueType fonts based on two script styles, Regular script (Kai), and
Sung/Ming script, each with three variants:

@itemize
@item @code{CNS 11643} (@code{TW-Kai} and @code{TW-Sung}): Tens of thousands
of CJK characters from frequency tables published by the Taiwanese
Ministry of Education.  ISO 10646 and Unicode compatible encoding.
@item @code{Big-5 Plus}: Several thousand frequently used CJK characters
encoded in the user defined area of the Big-5 code.
@item @code{Big-5 Extended}: A Big-5 character set based on the
@code{Big-5 Plus} and @code{CNS 11643} character sets.
@end itemize\n")
    (license (license:non-copyleft
              "http://data.gov.tw/license")))) ; CC-BY 4.0 compatible

(define-public font-cns11643-swjz
  (package
    (name "font-cns11643-swjz")
    (version "1")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.moedict.tw/fonts/truetype/cns11643/ebas927.ttf")
       (sha256
        (base32
         "1qkljldbmb53zp1rcmpsb8rzy67rnsqcjxi549m9743ifk4isl78"))))
    (build-system font-build-system)
    (home-page
     (string-append "http://www.cns11643.gov.tw/AIDB/download.do"
                    "?name=%E5%AD%97%E5%9E%8B%E4%B8%8B%E8%BC%89"))
    (synopsis "TrueType seal script font")
    (description
     "@code{Shuowen Jiezi} is a TrueType seal script font based on the ancient
text of the same name published by the Executive Yuan of Taiwan.  6721 glyphs
are included, at Unicode compatible code points corresponding to their modern
variants.")
    ;; Original text only available in Chinese. More info at
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26703#11
    (license (license:non-copyleft
              "http://www.cns11643.gov.tw/AIDB/copyright.do"))))

(define-public font-wqy-zenhei
  (package
    (name "font-wqy-zenhei")
    (version "0.9.45")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/wqy/wqy-zenhei/" version
                    "%20%28Fighting-state%20RC1%29/wqy-zenhei-"
                    version ".tar.gz"))
              (file-name (string-append "wqy-zenhei-" version ".tar.gz"))
              (sha256
               (base32
                "1mkmxq8g2hjcglb3zajfqj20r4r88l78ymsp2xyl5yav8w3f7dz4"))))
    (build-system font-build-system)
    (home-page "http://wenq.org/wqy2/")
    (synopsis "CJK font")
    (description
     "WenQuanYi Zen Hei is a Hei-Ti style (sans-serif type) Chinese outline
font.  It is designed for general purpose text formatting and on-screen
display of Chinese characters and symbols from many other languages.
WenQuanYi Zen Hei provides a rather complete coverage of Chinese Hanzi glyphs,
including both simplified and traditional forms.  The total glyph number in
this font is over 35,000, including over 21,000 Chinese Hanzi.  This font has
full coverage of the GBK (CP936) charset, CJK Unified Ideographs, as well as
the code-points needed for zh_cn, zh_sg, zh_tw, zh_hk, zh_mo, ja (Japanese) and
ko (Korean) locales for @code{fontconfig}.")
    ;; GPLv2 with font embedding exception
    (license license:gpl2)))

(define-public font-wqy-microhei
  (package
    (name "font-wqy-microhei")
    (version "0.2.0-beta")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/wqy/wqy-microhei/"
                                  version "/wqy-microhei-" version ".tar.gz"))
              (sha256
               (base32
                "0gi1yxqph8xx869ichpzzxvx6y50wda5hi77lrpacdma4f0aq0i8"))))
    (build-system font-build-system)
    (home-page "http://wenq.org/wqy2/")
    (synopsis "CJK font")
    (description
     "WenQuanYi Micro Hei is a Sans-Serif style (also known as Hei, Gothic or
Dotum among the Chinese/Japanese/Korean users) high quality CJK outline font.
It was derived from \"Droid Sans Fallback\" and \"Droid Sans\" released by
Google Inc.  This font contains all the unified CJK Han glyphs in the range of
U+4E00-U+9FC3 defined in Unicode Standard 5.1, together with many other
languages unicode blocks, including Latins, Extended Latins, Hanguls and
Kanas.  The font file is extremely compact (~4M) compared with most known CJK
fonts.")
    ;; This font is licensed under Apache2.0 or GPLv3 with font embedding
    ;; exceptions.
    (license license:gpl3)))

(define-public font-rachana
  (package
    (name "font-rachana")
    (version "7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gitlab.com/smc/rachana/repository/archive.tar.gz?ref=Version"
             version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jc091gshna6p1dd6lf507jxkgk6rsja835fc9dm71mcplq53bm1"))))
    (build-system font-build-system)
    (home-page "https://smc.org.in")
    (synopsis "Malayalam font")
    (description
     "Rachana is a Malayalam font designed by Hussain K H.  The project was
part of Rachana Aksharavedi for the original script of Malayalam in computing.
Rachana has about 1,200+ glyphs for Malayalam and contains glyphs required for
printing old Malayalam books without compromising the writing style.")
    ;; This font is licensed under SIL 1.1 or GPLv3+ with font embedding
    ;; exceptions.
    (license (list license:silofl1.1 license:gpl3+))))

(define-public font-tex-gyre
  (package
    (name "font-tex-gyre")
    (version "2.005")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "http://www.gust.org.pl/projects/e-foundry/"
                           "tex-gyre/whole/tg-" version "otf.zip"))
       (sha256
        (base32
         "0kph9l3g7jb2bpmxdbdg5zl56wacmnvdvsdn7is1gc750sqvsn31"))))
    (build-system font-build-system)
    (home-page "http://www.gust.org.pl/projects/e-foundry/tex-gyre/")
    (synopsis "Remake of Ghostscript fonts")
    (description "The TeX Gyre collection of fonts is the result of an
extensive remake and extension of the freely available base PostScript fonts
distributed with Ghostscript version 4.00.  The collection contains the
following fonts in the OpenType format: Adventor, Bonum, Chorus, Cursor,
Heros, Pagella, Schola, Termes.")
    (license license:gfl1.0)))

(define-public font-anonymous-pro
  (package
    (name "font-anonymous-pro")
    (version "1.002")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.marksimonson.com/assets/content/fonts/"
                    "AnonymousPro-" version ".zip"))
              (sha256
               (base32
                "1asj6lykvxh46czbal7ymy2k861zlcdqpz8x3s5bbpqwlm3mhrl6"))))
    (build-system font-build-system)
    (home-page "http://www.marksimonson.com/fonts/view/anonymous-pro")
    (synopsis "Fixed-width fonts designed with coding in mind")
    (description "Anonymous Pro is a family of four fixed-width fonts designed
with coding in mind.  Anonymous Pro features an international, Unicode-based
character set, with support for most Western and Central European Latin-based
languages, plus Greek and Cyrillic.")
    (license license:silofl1.1)))

(define-public font-gnu-unifont
  (package
    (name "font-gnu-unifont")
    (version "11.0.03")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/unifont/unifont-" version "/unifont-"
                    version ".tar.gz"))
              (sha256
               (base32
                "171zhm52c4rzpn19qcq4n9nyfbcjl38v50zslhvgpjdcmc5jvhav"))))
    (build-system gnu-build-system)
    (outputs '("out"   ; TrueType version
               "pcf"   ; PCF (bitmap) version
               "psf"   ; PSF (console) version
               "bin")) ; Utilities to manipulate '.hex' format
    (arguments
     '(#:tests? #f          ; no check target
       #:phases
       (modify-phases %standard-phases
         (replace
          'configure
          (lambda _ (setenv "CC" "gcc") #t))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((ttf (string-append (assoc-ref outputs "out")
                                       "/share/fonts/truetype"))
                   (pcf (string-append (assoc-ref outputs "pcf")
                                       "/share/fonts/misc"))
                   (psf (string-append (assoc-ref outputs "psf")
                                       "/share/consolefonts"))
                   (bin (assoc-ref outputs "bin")))
              (invoke "make"
                      (string-append "PREFIX=" bin)
                      (string-append "TTFDEST=" ttf)
                      (string-append "PCFDEST=" pcf)
                      (string-append "CONSOLEDEST=" psf)
                      "install")
              ;; Move Texinfo file to the right place.
              (mkdir (string-append bin "/share/info"))
              (invoke "gzip" "-9n" "doc/unifont.info")
              (install-file "doc/unifont.info.gz"
                            (string-append bin "/share/info"))
              #t))))))
    (inputs
     `(("perl" ,perl))) ; for utilities
    (synopsis
     "Large bitmap font covering Unicode's Basic Multilingual Plane")
    (description
     "GNU Unifont is a bitmap font covering essentially all of
Unicode's Basic Multilingual Plane.  The package also includes
utilities to ease adding new glyphs to the font.")
    (home-page "http://unifoundry.com/unifont.html")
    (properties '((upstream-name . "unifont")))
    (license license:gpl2+)))

(define-public font-google-noto
  (package
    (name "font-google-noto")
    (version "20170403")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://noto-website.storage.googleapis.com/"
                                  "pkgs/Noto-hinted.zip"))
              (file-name (string-append name "-" version ".zip"))
              (sha256
               (base32
                "1p92a6dvs7wqwjfpp1ahr9z1wz35am0l8r78521383spd77bmrfm"))))
    (build-system font-build-system)
    (home-page "https://www.google.com/get/noto/")
    (synopsis "Fonts to cover all languages")
    (description "Google Noto Fonts is a family of fonts designed to support
all languages with a consistent look and aesthetic.  Its goal is to properly
display all Unicode symbols.")
    (license license:silofl1.1)))

(define-public font-google-roboto
  (package
    (name "font-google-roboto")
    (version "2.136")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/google/roboto/releases/download/"
                           "v" version "/roboto-hinted.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "0spscx08fad7i8qs7icns96iwcapniq8lwwqqvbf7bamvs8qfln4"))))
    (build-system font-build-system)
    (home-page "https://github.com/google/roboto")
    (synopsis "The Roboto family of fonts")
    (description
     "Roboto is Google’s signature family of fonts, the default font on Android
and Chrome OS, and the recommended font for the
visual language \"Material Design\".")
    (license license:asl2.0)))

(define-public font-un
  (package
    (name "font-un")
    (version "1.0.2-080608")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://kldp.net/unfonts/release/2607-"
                    "un-fonts-core-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13liaz2pmww3aqabm55la5npd08m1skh334ky7qfidxaz5s742iv"))))
    (build-system font-build-system)
    (home-page "https://kldp.net/projects/unfonts/")
    (synopsis "Collection of Korean fonts")
    (description
     "Un-fonts is a family of mainly Korean fonts.
It contains the following fonts and styles:

@enumerate
@item UnBatang, UnBatangBold: serif;
@item UnDotum, UnDotumBold: sans-serif;
@item UnGraphic, UnGraphicBold: sans-serif style;
@item UnDinaru, UnDinaruBold, UnDinaruLight;
@item UnPilgi, UnPilgiBold: script;
@item UnGungseo: cursive, brush-stroke.
@end enumerate\n")
    (license license:gpl2+)))

(define-public font-fantasque-sans
  (package
    (name "font-fantasque-sans")
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/belluzj/fantasque-sans.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1gjranq7qf20rfxnpxsckv1hl35nzsal0rjs475nhfbpqy5wmly6"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ttfautohint" ,ttfautohint)
       ("woff-tools" ,woff-tools)
       ("fontforge" ,fontforge)
       ("woff2" ,woff2)
       ("ttf2eot" ,ttf2eot)
       ("zip" ,zip)))
    (arguments
     `(#:tests? #f                 ;test target intended for visual inspection
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configuration
                  (add-before 'build 'xrange->range
                    ;; Rather than use a python2 fontforge, just replace the
                    ;; offending function.
                    (lambda _
                      (substitute* "Scripts/fontbuilder.py"
                        (("xrange") "range"))
                      #t))
                  (replace 'install
                    ;; 'make install' wants to install to ~/.fonts, install to
                    ;; output instead.  Install only the "Normal" variant.
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (font-dir (string-append out "/share/fonts"))
                             (truetype-dir (string-append font-dir "/truetype"))
                             (opentype-dir (string-append font-dir "/opentype"))
                             (webfonts-dir (string-append font-dir "/webfonts")))
                        (with-directory-excursion "Variants/Normal"
                          (copy-recursively "OTF" opentype-dir)
                          (for-each (lambda (f) (install-file f truetype-dir))
                                    (find-files "." "\\.ttf$"))
                          (copy-recursively "Webfonts" webfonts-dir)
                          #t)))))))
    (synopsis "Font family with a monospaced variant for programmers")
    (description
     "Fantasque Sans Mono is a programming font designed with functionality in
mind.  The font includes a bold version and a good italic version with new
glyph designs, not just an added slant.")
    (home-page "https://fontlibrary.org/en/font/fantasque-sans-mono")
    (license license:silofl1.1)))

(define-public font-hack
  (package
    (name "font-hack")
    (version "3.002")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://github.com/source-foundry/Hack/releases/download/v"
                    version "/Hack-v" version "-ttf.zip"))
              (sha256
               (base32
                "18fpaczj2rxfwgnrqpkxq0qn01parhmngglc4i1n3gchyzdsrh0x"))))
    (build-system font-build-system)
    (home-page "https://sourcefoundry.org/hack/")
    (synopsis "Typeface designed for source code")
    (description
     "Hack is designed to be a workhorse typeface for code.  It expands upon
the Bitstream Vera & DejaVu projects, provides 1561 glyphs, and includes
Powerline support.")
    (license
     ;; See https://github.com/source-foundry/Hack/issues/271 for details.
     (list license:expat                ; the Hack modifications to...
           license:public-domain        ; ...the DejaVu modifications to...
           (license:x11-style           ; ...the Bitstream Vera typeface
            "file://LICENSE.md" "Bitstream Vera License")))))

(define-public font-adobe-source-code-pro
  (package
    (name "font-adobe-source-code-pro")
    (version "2.030R-ro-1.050R-it")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/adobe-fonts/source-code-pro/archive/"
                    (regexp-substitute/global
                     ;; The upstream tag uses "/" between the roman and italic
                     ;; versions, so substitute our "-" separator here.
                     #f "R-ro-" version 'pre "R-ro/" 'post) ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0arhhsf3i7ss39ykn73d1j8k4n8vx7115xph6jwkd970p1cxvr54"))))
    (build-system font-build-system)
    (home-page "https://github.com/adobe-fonts/source-code-pro")
    (synopsis
     "Monospaced font family for user interface and coding environments")
    (description
     "Source Code Pro is a set of monospaced OpenType fonts that have been
designed to work well in user interface environments.")
    (license license:silofl1.1)))

(define-public font-adobe-source-sans-pro
  (package
    (name "font-adobe-source-sans-pro")
    (version "2.040R-ro-1.090R-it")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/adobe-fonts/source-sans-pro/archive/"
                    (regexp-substitute/global
                     ;; The upstream tag uses "/" between the roman and italic
                     ;; versions, so substitute our "-" separator here.
                     #f "R-ro-" version 'pre "R-ro/" 'post) ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1wpbhd2idps53ph8rg1mhr3vz4lsgbpjprcq10nliwcxdz9d8lv0"))))
    (build-system font-build-system)
    (home-page "https://github.com/adobe-fonts/source-sans-pro")
    (synopsis
     "Sans serif font family for user interface environments")
    (description
     "Source Sans Pro is a set of OpenType fonts that have been designed to
work well in user interface (UI) environments.")
    (license license:silofl1.1)))

(define-public font-fira-mono
  (package
    (name "font-fira-mono")
    (version "3.206")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://carrois.com/downloads/fira_mono_3_2/"
                                  "FiraMonoFonts"
                                  (string-replace-substring version "." "")
                                  ".zip"))
              (sha256
               (base32
                "1z65x0dw5dq6rs6p9wyfrir50rlh95vgzsxr8jcd40nqazw4jhpi"))))
    (build-system font-build-system)
    (home-page "https://mozilla.github.io/Fira/")
    (synopsis "Mozilla's monospace font")
    (description "This is the typeface used by Mozilla in Firefox OS.")
    (license license:silofl1.1)))

(define-public font-fira-sans
  (package
    (name "font-fira-sans")
    (version "4.202")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mozilla/Fira/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r6zdnqqp4bgq5nmgqbj0vvj7x1h9w912851ggbl9wc7fdjnjqnq"))))
    (build-system font-build-system)
    (home-page "https://mozilla.github.io/Fira/")
    (synopsis "Mozilla's Fira Sans Font")
    (description "This is the typeface used by Mozilla in Firefox OS.")
    (license license:silofl1.1)))

(define-public font-fira-code
  (package
    (name "font-fira-code")
    (version "1.205")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/tonsky/FiraCode/releases/"
                                  "download/" version
                                  "/FiraCode_" version ".zip"))
              (sha256
               (base32
                "13bxgf59g6fw5191xclcjzn22hj8jk9k5jjwf7vz07mpjbgadcl5"))))
    (build-system font-build-system)
    (home-page "https://mozilla.github.io/Fira/")
    (synopsis "Monospaced font with programming ligatures")
    (description
     "Fira Code is an extension of the Fira Mono font containing a set of ligatures
for common programming multi-character combinations.  This is just a font rendering
feature: underlying code remains ASCII-compatible.  This helps to read and understand
code faster.  For some frequent sequences like .. or //, ligatures allow us to
correct spacing.")
    (license license:silofl1.1)))

(define-public font-awesome
  (package
   (name "font-awesome")
   (version "4.7.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://fontawesome.io/assets/"
                                name "-" version ".zip"))
            (sha256
             (base32
              "1m1rfwm4sjkv10j3xd2dhwk286a5912b2zgvc692cmxi5gxs68jf"))))
   (build-system font-build-system)
   (home-page "http://fontawesome.io")
   (synopsis "Font that contains a rich iconset")
   (description
    "Font Awesome is a full suite of pictographic icons for easy scalable
vector graphics.")
   (license license:silofl1.1)))

(define-public font-tamzen
  (package
    (name "font-tamzen")
    (version "1.11.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sunaku/tamzen-font/archive/"
                           "Tamzen-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ryd7gp6qiwaqw73jqbmh4kwlriyd8xykh4j7z90z8xp9fm7lrys"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (let ((tar      (string-append (assoc-ref %build-inputs "tar")
                                        "/bin/tar"))
               (PATH     (string-append (assoc-ref %build-inputs "gzip")
                                        "/bin"))
               (font-dir (string-append %output "/share/fonts/misc"))
               (psf-dir (string-append %output "/share/kbd/consolefonts"))
               (src-pcf-dir (string-append "tamzen-font-Tamzen-"
                                            ,version "/pcf")))
           (setenv "PATH" PATH)
           (invoke tar "xvf" (assoc-ref %build-inputs "source"))
           (mkdir-p font-dir)
           (mkdir-p psf-dir)
           (chdir src-pcf-dir)
           (for-each (lambda (pcf)
                       (install-file pcf font-dir))
                     (find-files "." "\\.pcf$"))
           (chdir "../psf")
           (for-each (lambda (psf)
                       (install-file psf psf-dir))
                     (find-files "." "\\.psf$"))
           #t))))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "https://github.com/sunaku/tamzen-font")
    (synopsis "Monospaced bitmap font for console and X11")
    (description
     "Tamzen is a fork of the @code{Tamsyn} font.  It is programatically forked
from @code{Tamsyn} version 1.11, backporting glyphs from older versions while
deleting deliberately empty glyphs (which are marked as unimplemented) to
allow secondary/fallback fonts to provide real glyphs at those codepoints.

The @code{TamzenForPowerline} fonts provide additional @code{Powerline} symbols,
which are programatically injected with @code{bitmap-font-patcher} and
later hand-tweaked with the gbdfed(1) editor:

@enumerate
@item all icons are expanded to occupy the maximum available space
@item the branch of the fork icon ( U+E0A0) was made larger than the trunk
@item for the newline icon ( U+E0A1), the @emph{N} was made larger at the bottom
@item the keyhole in the padlock icon ( U+E0A2) was replaced with @emph{//} lines.
@end enumerate\n")
    (license (license:non-copyleft "file://LICENSE"))))

(define-public font-comic-neue
  (package
    (name "font-comic-neue")
    (version "2.3")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "http://www.comicneue.com/comic-neue-" version ".zip"))
              (sha256
               (base32
                "1695hkpd8kqnr2a88p8xs496slgzxjjkzpa9aa33ml3pnh7519zk"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Delete Mac OS X specific files. If not deleted, these cause
         ;; several hidden files to be installed.
         (add-before 'install 'delete-macosx-files
           (lambda _
             (delete-file-recursively "__MACOSX")
             #t))
         (add-after 'install 'install-conf
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((conf-dir (string-append (assoc-ref outputs "out")
                                            "/share/fontconfig/conf.avail")))
               (mkdir-p conf-dir)
               (call-with-output-file
                   (string-append conf-dir "/30-comic-neue.conf")
                 (lambda (port)
                   (format port "<?xml version=\"1.0\"?>
<!DOCTYPE fontconfig SYSTEM \"fonts.dtd\">
<fontconfig>
  <!-- If Comic Sans is missing, use Comic Neue instead. -->
  <alias>
    <family>Comic Sans MS</family>
    <prefer>
      <family>Comic Neue</family>
    </prefer>
  </alias>
</fontconfig>\n"))))
             #t)))))
    (home-page "http://www.comicneue.com/")
    (synopsis "Font that fixes the shortcomings of Comic Sans")
    (description
     "Comic Neue is a font that attempts to create a respectable casual
typeface, by mimicking Comic Sans while fixing its most obvious shortcomings.")
    (license license:silofl1.1)))

(define-public font-iosevka
  (package
    (name "font-iosevka")
    (version "1.12.5")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://github.com/be5invis/Iosevka/releases/download/v"
                    version "/iosevka-pack-" version ".zip"))
              (sha256
               (base32
                "0s3g6mk0ngwsrw9h9dqinb50cd9i8zhqdcmmh93fhyf4d87yfwyi"))))
    (build-system font-build-system)
    (home-page "https://be5invis.github.io/Iosevka/")
    (synopsis "Coders' typeface, built from code")
    (description
     "Iosevka is a slender monospace sans-serif or slab-serif typeface inspired
by Pragmata Pro, M+, and PF DIN Mono, designed to be the ideal font for
programming.  Iosevka is completely generated from its source code.")
    (license (list license:silofl1.1 ; build artifacts (i.e. the fonts)
                   license:bsd-3)))) ; supporting code

(define-public font-go
  (let ((commit "f03a046406d4d7fbfd4ed29f554da8f6114049fc")
        (revision "1"))
    (package
      (name "font-go")
      (version (string-append "20170330-" revision "." (string-take commit 7)))
      (source (origin
                (file-name (string-append "go-image-" version "-checkout"))
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/image")
                      (commit commit)))
                (sha256
                 (base32
                  "1aq6mnjayks55gd9ahavk6jfydlq5lm4xm0xk4pd5sqa74p5p74d"))))
      (build-system font-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'chdir
             (lambda _
               (chdir "font/gofont/ttfs")
               #t)))))
      (home-page "https://blog.golang.org/go-fonts")
      (synopsis "The Go font family")
      (description
       "The Go font family is a set of WGL4 TrueType fonts from the Bigelow &
Holmes type foundry, released under the same license as the Go programming
language.  It includes a set of proportional, sans-serif fonts, and a set of
monospace, slab-serif fonts.")
      (license license:bsd-3))))

(define-public font-google-material-design-icons
  (package
    (name "font-google-material-design-icons")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/google/material-design-icons/archive/"
                    version ".tar.gz"))
              (sha256
               (base32
                "018i3za9r6kf6svci33z09lc5pr5yz4164m8gzzwjzzqcrng0p5j"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system font-build-system)
    (home-page "http://google.github.io/material-design-icons")
    (synopsis "Icon font of Google Material Design icons")
    (description
     "Material design system icons are simple, modern, friendly, and sometimes
quirky.  Each icon is created using our design guidelines to depict in simple
and minimal forms the universal concepts used commonly throughout a UI.
Ensuring readability and clarity at both large and small sizes, these icons
have been optimized for beautiful display on all common platforms and display
resolutions.")
    (license license:asl2.0)))

(define-public font-open-dyslexic
  (package
    (name "font-open-dyslexic")
    (version "20160623")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/antijingoist/open-dyslexic/"
                           "archive/" version "-Stable.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0al0j9kb32kfavcpq1kigsd36yzvf5yhzqhds0jkh7ngbxyxwkx4"))))
    (build-system font-build-system)
    (home-page "https://opendyslexic.org")
    (synopsis "Font for dyslexics and high readability")
    (description "OpenDyslexic is a font designed to help readability for some
of the symptoms of dyslexia.  Letters have heavy weighted bottoms to provide
an indication of orientation to make it more difficult to confuse with other
similar letters.  Consistently weighted bottoms can also help reinforce the
line of text.  The unique shapes of each letter can help prevent flipping and
swapping.  The italic style for OpenDyslexic has been crafted to be used for
emphasis while still being readable.")
    (license
     (license:fsdg-compatible
      "https://www.gnome.org/fonts/#Final_Bitstream_Vera_Fonts"
      "The Font Software may be sold as part of a larger software package but
no copy of one or more of the Font Software typefaces may be sold by
itself."))))

(define-public font-dosis
  (package
    (name "font-dosis")
    (version "1.7")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "http://www.impallari.com/media/releases/dosis-"
                           "v" version ".zip"))
       (sha256
        (base32
         "1qhci68f68mf87jd69vjf9qjq3wydgw1q7ivn3amjb65ls1s0c4s"))))
    (build-system font-build-system)
    (home-page "http://www.impallari.com/dosis")
    (synopsis "Very simple, rounded, sans serif family")
    (description
     "Dosis is a very simple, rounded, sans serif family.
The lighter weights are minimalist.  The bolder weights have more personality.
The medium weight is nice and balanced.  The overall result is a family
that's clean and modern, and can express a wide range of voices & feelings.
It comes in 7 incremental weights:
ExtraLight, Light, Book, Medium, Semibold, Bold & ExtraBold")
    (license license:silofl1.1)))

(define-public font-culmus
  (package
    (name "font-culmus")
    (version "0.133")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/"
                           "culmus/files/culmus/" version "/culmus-src-"
                           version ".tar.gz"))
       (sha256
        (base32
         "02akysgsqhi15cck54xcacm16q5raf4l7shgb8fnj7xr3c1pbfyp"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'build
           (lambda _
             (let ((compile
                    (lambda (name ext)
                      (invoke
                       "fontforge" "-lang=ff"
                       "-c" (string-append "Open('" name "');"
                                           "Generate('"
                                           (basename name "sfd") ext
                                           "')")))))
               ;; This part based on the fonts shipped in the non-source package.
               (for-each (lambda (name)
                           (compile name "ttf"))
                         (find-files "." "^[^Nachlieli].*\\.sfd$"))
               (for-each (lambda (name)
                           (compile name "otf"))
                         (find-files "." "^Nachlieli.*\\.sfd$"))
               #t))))))
    (native-inputs
     `(("fontforge" ,fontforge)))
    (home-page "http://culmus.sourceforge.net/")
    (synopsis "TrueType Hebrew Fonts for X11")
    (description "14 Hebrew trivial families.  Contain ASCII glyphs from various
sources.  Those families provide a basic set of a serif (Frank Ruehl), sans
serif (Nachlieli) and monospaced (Miriam Mono) trivials.  Also included Miriam,
Drugulin, Aharoni, David, Hadasim etc.  Cantillation marks support is
available in Keter YG.")
    (license license:gpl2))) ; consult the LICENSE file included

(define-public font-lohit
  (package
    (name "font-lohit")
    (version "20140220")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.pagure.org/lohit/lohit-ttf-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1rmgr445hw1n851ywy28csfvswz1i6hnc8mzp88qw2xk9j4dn32d"))))
    (build-system font-build-system)
    (home-page "https://pagure.io/lohit")
    (synopsis "Lohit TrueType Indic fonts")
    (description "Lohit is a font family designed to cover Indic scripts.
Lohit supports the Assamese, Bengali, Devanagari (Hindi, Kashmiri, Konkani,
Maithili, Marathi, Nepali, Sindhi, Santali, Bodo, Dogri languages), Gujarati,
Kannada, Malayalam, Manipuri, Oriya, Punjabi, Tamil and Telugu scripts.")
    (license license:silofl1.1)))

(define-public font-blackfoundry-inria
  (package
    (name "font-blackfoundry-inria")
    (version "1.200")
    (home-page "https://github.com/BlackFoundry/InriaFonts")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "06775y99lyh6hj5hzvrx56iybdck8a8xfqkipqd5c4cldg0a9hh8"))
              (file-name (string-append name "-" version "-checkout"))))
    ;; XXX: There are .ufo directories (the "source") so in theory we should
    ;; be able to rebuild TTF and OTF files with FontForge.  Unfortunately a
    ;; command like:
    ;;
    ;;  fontforge -lang=ff -c "Open('InriaSans-Regular.ufo'); Generate('foo.ttf');"
    ;;
    ;; segfaults in '_UFOLoadGlyph', which calls out to libpython.  :-/
    ;; In the meantime we ship the precompiled OTF and TTF files.
    (build-system font-build-system)
    (synopsis "Inria Sans and Inria Serif type family")
    (description
     "Inria Sans and Inria Serif are the two members of a type family designed
for Inria, a public research institute in computer science and mathematics.")
    (license license:silofl1.1)))

(define-public font-sil-gentium
  (package
    (name "font-sil-gentium")
    (version "5.000")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://software.sil.org/downloads/r/gentium/GentiumPlus-"
                    version ".zip"))
              (sha256
               (base32
                "0m7189870hha217n1vgpmf89mwggrxkh679ffi1lxpnjggqi2n9k"))))
    ;; Note: The zip file provides TTF files only, but the developer release,
    ;; which contains additional files, has a 'SOURCES.txt' file that says
    ;; that "the primary source files for the fonts are the fonts themselves".
    ;; Thus it looks like the TTF can be considered source.
    (build-system font-build-system)
    (synopsis "Serif font for the Cyrillic, Greek, and Latin alphabets")
    (description
     "Gentium is a typeface family designed to enable the diverse ethnic
groups around the world who use the Latin, Cyrillic and Greek scripts to
produce readable, high-quality publications.  The font comes with regular and
italics shapes.  This package provides only TrueType files (TTF).")
    (home-page "https://software.sil.org/gentium/")
    (license license:silofl1.1)))

(define-public font-sil-charis
  (package
    (name "font-sil-charis")
    (version "5.000")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://software.sil.org/downloads/r/charis/CharisSIL-"
                    version ".zip"))
              (sha256
               (base32
                "1zcvw37f1a7gkml3yfm6hxh93844llm7xj4w52600qq3ndrm8gjy"))))
    ;; As for Gentium (see above), the TTF files are considered source.
    (build-system font-build-system)
    (synopsis "Serif font for the Cyrillic and Latin alphabets")
    (description
     "Charis SIL is a Unicode-based font family that supports the wide range
of languages that use the Latin and Cyrillic scripts.  It is specially
designed to make long texts pleasant and easy to read, even in less than ideal
reproduction and display environments.  This package provides only TrueType
files (TTF).")
    (home-page "https://software.sil.org/charis/")
    (license license:silofl1.1)))

(define-public font-mononoki
  (package
    (name "font-mononoki")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/madmalik/mononoki/")
                    (commit version)))
              (sha256
               (base32
                "1rkzyxn30rn8qv2h2xz324j7q15hzg2lci8790a7cdl1dfgic4xi"))
              (file-name (git-file-name name version))))
    (build-system font-build-system)
    (synopsis "Font for programming and code review")
    (description
     "Mononoki is a typeface by Matthias Tellen, created to enhance code
formatting.")
    (home-page "https://madmalik.github.io/mononoki/")
    (license license:silofl1.1)))
