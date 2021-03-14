;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2016 Jookia <166291@gmail.com>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Dmitry Nikolaev <cameltheman@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Toni Reina <areina@riseup.net>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.com>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2017, 2018, 2019, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Mohammed Sadiq <sadiq@sadiqpk.org>
;;; Copyright © 2018 Charlie Ritter <chewzerita@posteo.net>
;;; Copyright © 2018 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019, 2020 Jens Mølgaard <jens@zete.tk>
;;; Copyright © 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Baptiste Strazzulla <bstrazzull@hotmail.fr>
;;; Copyright © 2019 Alva <alva@skogen.is>
;;; Copyright © 2019 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2020 Damien Cassou <damien@cassou.me>
;;; Copyright © 2020 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020, 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2020 Simen Endsjø <simendsjo@gmail.com>
;;; Copyright © 2020 Tim Van den Langenbergh <tmt_vdl@gmx.com>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2021 Antoine Côté <antoine.cote@posteo.net>
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
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xorg))

(define-public font-ibm-plex
  (package
    (name "font-ibm-plex")
    (version "5.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/IBM/plex/releases/download/"
                    "v" version "/OpenType.zip"))
              (sha256
               (base32
                "0zlz8kxx54i4hpgaip9690bilvn5w14gp7jjkk6cz4h9p3xml231"))))
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
    (version "3.000")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/googlefonts/Inconsolata/"
                           "releases/download/v" version "/fonts_otf.zip"))
       (sha256
        (base32 "1wavvv86nwsqm5sbmnkv1bprj7l7zdrkxpvjy6w8yag93k6hrlx1"))))
    (build-system font-build-system)
    (home-page "https://levien.com/type/myfonts/inconsolata.html")
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

(define-public font-canada1500
  (package
    (name "font-canada1500")
    (version "1.101")
    (source (origin
              (method url-fetch)
              (uri "https://typodermicfonts.com/wp-content/uploads/2017/06/canada1500.zip")
              (sha256
               (base32
                "052rwhyfcz6q0g0nsr21bkbg2qb6sw7wzins5hv80qhdhi77sdaj"))))
    (build-system font-build-system)
    (home-page "https://typodermicfonts.com/canada1500/")
    (synopsis "Canadian typeface that supports English, French and Aboriginal languages")
    (description "Canada1500 is a display typeface originally created for the
Canadian sesquicentennial with four weights, italics and space symbols which
includes lining and old-style numerals, tabular and proportional.  Greek,
Cyrillic, Canadian Syllabics and most Latin based languages are supported.")
    (license license:cc0)))

(define-public font-cantarell
  (package
    (name "font-abattis-cantarell")
    (version "0.301")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/cantarell-fonts/"
                                  (version-major+minor version)
                                  "/cantarell-fonts-" version ".tar.xz"))
              (sha256
               (base32
                "10sycxscs9kzl451mhygyj2qj8qlny8pamskb86np7izq05dnd9x"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))   ; for msgfmt
    (home-page "https://wiki.gnome.org/Projects/CantarellFonts")
    (synopsis "Cantarell sans-serif typeface")
    (description "The Cantarell font family is a contemporary Humanist
sans-serif designed for on-screen reading.  It is used by GNOME@tie{}3.")
    (license license:silofl1.1)))

(define-public font-lato
  (package
    (name "font-lato")
    (version "2.015")                   ; also update description
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://www.latofonts.com/download/Lato2OFL.zip"))
              (sha256
               (base32
                "1f5540g0ja1nx3ddd3ywn77xc81ssrxpq8n3gyb9sabyq2b4xda2"))))
    (build-system font-build-system)
    (home-page "https://www.latofonts.com/lato-free-fonts/")
    (synopsis "Lato sans-serif typeface")
    (description
     "Lato is a sanserif typeface family.  It covers over 3000 glyphs per style.
The Lato 2.010 family supports more than 100 Latin-based languages, over
50 Cyrillic-based languages as well as Greek and IPA phonetics.")
    (license license:silofl1.1)))

(define-public font-gnu-freefont
  (package
    (name "font-gnu-freefont")
    ;; Note: Remove the special FontForge input and package once the 2020
    ;; release is out.
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
                           (ttf-font-dir (string-append %output
                                                        "/share/fonts/truetype"))
                           (otf-font-dir (string-append %output
                                                        "/share/fonts/opentype"))
                           (woff-font-dir (string-append %output
                                                         "/share/fonts/webfonts")))
                       (mkdir-p doc-dir)
                       (substitute* "Makefile"
                         (("\\$\\(TMPDIR\\)") doc-dir)
                         (("sfd/\\*.ttf") "")
                         (("sfd/\\*.otf") "")
                         (("sfd/\\*.woff") ""))
                       ;; XXX The FreeFont Makefile tries to use the current
                       ;; time and date as names for generated files, and fails
                       ;; silently. But the fonts are still installed, so we
                       ;; leave the issue alone for now.
                       ;; See <https://bugs.gnu.org/40783>
                       (system* "make" "ttftar" "otftar" "wofftar")
                       (mkdir-p ttf-font-dir)
                       (mkdir-p otf-font-dir)
                       (mkdir-p woff-font-dir)
                       (for-each (lambda (file)
                                   (install-file file ttf-font-dir))
                                 (filter
                                   (lambda (file) (string-suffix? "ttf" file))
                                   (find-files "." "")))
                       (for-each (lambda (file)
                                   (install-file file otf-font-dir))
                                 (filter
                                   (lambda (file) (string-suffix? "otf" file))
                                   (find-files "." "")))
                       (for-each (lambda (file)
                                   (install-file file woff-font-dir))
                                 (filter
                                   (lambda (file) (string-suffix? "woff" file))
                                   (find-files "." "")))))))
       #:test-target "tests"))
    ;; FreeFont anno 2012 requires a FontForge built with Python 2.
    (native-inputs `(("fontforge" ,fontforge-20190801)))
    (home-page "https://www.gnu.org/software/freefont/")
    (synopsis "Unicode-encoded outline fonts")
    (description
     "The GNU Freefont project aims to provide a set of free outline
 (PostScript Type0, TrueType, OpenType...) fonts covering the ISO
10646/Unicode UCS (Universal Character Set).")
    (license license:gpl3+)
    (properties '((upstream-name . "freefont")
                  (ftp-directory . "/gnu/freefont")))))

(define-public font-gnu-freefont-ttf
  (deprecated-package "font-gnu-freefont-ttf" font-gnu-freefont))

(define-public font-liberation
  (package
    (name "font-liberation")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/liberationfonts/liberation-fonts/"
             "files/6060976/liberation-fonts-ttf-" version ".tar.gz"))
       (sha256
        (base32 "0bv8i47iq2irxkkjlqwdli4zz01sb1qg2n6vbdqjrqqhx912zji2"))))
    (build-system font-build-system)
    (home-page "https://github.com/liberationfonts")
    (synopsis "Fonts compatible with Arial, Times New Roman, and Courier New")
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
@end enumerate\n")
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

(define-public font-libertinus
  (package
    (name "font-libertinus")
    (version "7.040")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/alerque/libertinus/releases"
                           "/download/v" version "/libertinus-" version
                           ".zip"))
       (sha256
        (base32 "1xkj993hwkr49q63dd2dnkvdkm9sckxm3zjwhdxsxn21fi80ikic"))))
    (build-system font-build-system)
    (home-page "https://github.com/alerque/libertinus")
    (synopsis "Font family based on Linux Libertine")
    (description
     "The Libertinus font family is a fork of Linux Libertine that addresses
many bugs in the unmaintained original and adds a new mathematical companion
font for use with OpenType math-capable applications like LuaTex or XeTeX.

The unified Libertinus family consists of:
@enumerate
@item Libertinus Serif, forked from Linux Libertine;
@item Libertinus Sans Serif, forked from Linux Biolinum;
@item Libertinus Mono, forked from Linux Libertine Mono; and
@item Libertinus Math, an original matching OpenType math font.
@end enumerate\n")
    (license license:silofl1.1)))

(define-public font-terminus
  (package
    (name "font-terminus")
    (version "4.48")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/terminus-font/terminus-font-"
                           version "/terminus-font-" version ".tar.gz"))
       (sha256
        (base32 "1bwlkj39rqbyq57v5yssayav6hzv1n11b9ml2s0dpiyfsn6rqy9l"))))
    (build-system gnu-build-system)
    (outputs (list "out" "pcf-8bit"))
    (arguments
     `(#:tests? #f                      ; no test target in tarball
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-more-bits
           ;; X11 8-bit code pages aren't installed by default (they were
           ;; until version 4.46).  Build and install them separately.
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "pcf-8bit" make-flags)))
         (add-after 'install 'install-more-bits
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let ((pcf-8bit (assoc-ref outputs "pcf-8bit")))
               (apply invoke "make" "install-pcf-8bit" (string-append "prefix="
                                                                      pcf-8bit)
                      make-flags)))))))
    (native-inputs
     `(("bdftopcf" ,bdftopcf)
       ("font-util" ,font-util)
       ("mkfontdir" ,mkfontdir)
       ("pkg-config" ,pkg-config)
       ("python" ,python)))
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
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/adobe-fonts/source-han-sans")
                     (commit (string-append version "R"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zm884d8fp5gvirq324050kqv7am9khyqhs9kk4r4rr3jzn61jpk"))))
    (outputs '("out"                 ; OpenType/CFF Collection (OTC), 121 MiB.
               "cn" "jp" "kr" "tw")) ; Region-specific Subset OpenType/CFF.
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((install-opentype-fonts
                (lambda (fonts-dir out)
                  (copy-recursively fonts-dir
                                    (string-append (assoc-ref %outputs out)
                                                   "/share/fonts/opentype")))))
           (chdir (assoc-ref %build-inputs "source"))
           (install-opentype-fonts "OTC" "out")
           (install-opentype-fonts "SubsetOTF/CN" "cn")
           (install-opentype-fonts "SubsetOTF/JP" "jp")
           (install-opentype-fonts "SubsetOTF/KR" "kr")
           (install-opentype-fonts "SubsetOTF/TW" "tw")
           (for-each delete-file (find-files %output "\\.zip$"))
           #t))))
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
    ;; GPLv2 with font embedding exception.
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
    (version "7.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/smc/fonts/rachana")
             (commit (string-append "Version" version))))
       (sha256
        (base32 "0r100pvk56y1s38nbv24d78s8nd7dkblgasbn8s887dzj6dps23d"))
       (file-name (git-file-name name version))))
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
                    "https://www.marksimonson.com/assets/content/fonts/"
                    "AnonymousPro-" version ".zip"))
              (sha256
               (base32
                "1asj6lykvxh46czbal7ymy2k861zlcdqpz8x3s5bbpqwlm3mhrl6"))))
    (build-system font-build-system)
    (home-page "https://www.marksimonson.com/fonts/view/anonymous-pro")
    (synopsis "Fixed-width fonts designed with coding in mind")
    (description "Anonymous Pro is a family of four fixed-width fonts designed
with coding in mind.  Anonymous Pro features an international, Unicode-based
character set, with support for most Western and Central European Latin-based
languages, plus Greek and Cyrillic.")
    (license license:silofl1.1)))

(define-public font-anonymous-pro-minus
  (package
    (inherit font-anonymous-pro)
    (name "font-anonymous-pro-minus")
    ;; The -Minus variant doesn't necessarily track the regular version above.
    (version "1.003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.marksimonson.com/assets/content/fonts/"
                           "AnonymousProMinus-" version ".zip"))
       (sha256
        (base32 "1p2n91jja37d2cshp5pjwld9lq0v7gnpk7ywwn2blq7k46q6vq38"))))
    (synopsis "Fixed-width fonts designed with coding in mind, without bitmaps")
    (description "Anonymous Pro is a family of four fixed-width fonts designed
with coding in mind.  Anonymous Pro features an international, Unicode-based
character set, with support for most Western and Central European Latin-based
languages, plus Greek and Cyrillic.

Anonymous Pro Minus is identical to Anonymous Pro, minus its embedded bitmaps
for use at smaller text sizes")))

(define-public font-gnu-unifont
  (package
    (name "font-gnu-unifont")
    (version "13.0.06")
    (source
     (origin
       (method url-fetch)
       (uri (list
             (string-append "https://unifoundry.com/pub/unifont/unifont-"
                            version "/unifont-" version ".tar.gz")
             (string-append "mirror://gnu/unifont/unifont-"
                            version "/unifont-" version ".tar.gz")))
       (sha256
        (base32 "09g91g0gv76sadslp70m5xwfk3jf8kh7rpk2pz3l2hpldnjggpk8"))))
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
    (home-page "http://unifoundry.com/unifont/index.html")
    (properties '((upstream-name . "unifont")))
    (license license:gpl2+)))

(define-public font-google-noto
  (package
    (name "font-google-noto")
    (version "20171025")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://noto-website-2.storage.googleapis.com/"
                           "pkgs/Noto-hinted.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "1bp42whyin7xcgmrbnfvz3rvd98xmxaz3ywqybbjmqzwaa9llyw3"))))
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
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/belluzj/fantasque-sans")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17l18488qyl9gdj80r8pcym3gp3jkgsdikwalnrp5rgvwidqx507"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ttfautohint" ,ttfautohint)
       ("woff-tools" ,woff-tools)
       ("fontforge" ,fontforge)
       ("woff2" ,woff2)
       ("woff2:bin" ,woff2 "bin")
       ("zip" ,zip)))
    (arguments
     `(#:tests? #f                 ;test target intended for visual inspection
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configuration
                  (add-before 'build 'support-python@3
                    ;; Rather than use a Python 2 fontforge, replace Python-2-
                    ;; specific code with a passable Python 3 equivalent.
                    (lambda _
                      (substitute* "Scripts/fontbuilder.py"
                        (("xrange") "range"))
                      (substitute* "Scripts/features.py"
                        (("f\\.write\\(fea_code\\)")
                         "f.write(str.encode(fea_code))"))
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
    (version "3.003")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://github.com/source-foundry/Hack/releases/download/v"
                    version "/Hack-v" version "-ttf.zip"))
              (sha256
               (base32
                "1b4hh8zkrx92m2v2vfkja1napb0192p0j3laqr0m018z3dih89hc"))))
    (build-system font-build-system)
    (home-page "https://sourcefoundry.org/hack/")
    (synopsis "Typeface designed for source code")
    (description
     "Hack is designed to be a workhorse typeface for code.  It expands upon
the Bitstream Vera & DejaVu projects, provides over 1,500 glyphs, and includes
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adobe-fonts/source-code-pro")
             (commit (regexp-substitute/global
                      ;; The upstream tag uses "/" between the roman and italic
                      ;; versions, so substitute our "-" separator here.
                      #f "R-ro-" version 'pre "R-ro/" 'post))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hc5kflr8xzqgdm0c3gbgb1paygznxmnivkylid69ipc7wnicx1n"))))
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
    (version "3.028R")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adobe-fonts/source-sans-pro")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lgjqi4d5p1q1z00ad807v5qy4z54gmp7jpdaypc0rxk8czv6zq7"))))
    (build-system font-build-system)
    (home-page "https://github.com/adobe-fonts/source-sans-pro")
    (synopsis
     "Sans serif font family for user interface environments")
    (description
     "Source Sans Pro is a set of OpenType fonts that have been designed to
work well in user interface (UI) environments.")
    (license license:silofl1.1)))

(define-public font-adobe-source-serif-pro
  (package
    (name "font-adobe-source-serif-pro")
    (version "3.001R")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adobe-fonts/source-serif-pro")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z0pjvx0jpjwb8vzvc6l5gzlg0mqax4v9pizqcxx82l0ydlfh5bj"))))
    (build-system font-build-system)
    (home-page "https://github.com/adobe-fonts/source-serif-pro")
    (synopsis
     "Serif typeface to complement Source Sans Pro for setting text")
    (description
     "Source Serif Pro is a set of OpenType fonts to complement the Source
Sans Pro family.")
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
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mozilla/Fira")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "116j26gdj5g1r124b4669372f7490vfjqw7apiwp2ggl0am5xd0w"))))
    (build-system font-build-system)
    (home-page "https://mozilla.github.io/Fira/")
    (synopsis "Mozilla's Fira Sans Font")
    (description "This is the typeface used by Mozilla in Firefox OS.")
    (license license:silofl1.1)))

(define-public font-fira-go
  (package
    (name "font-fira-go")
    (version "1.000")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/bBoxType/FiraGO")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10rcfg1fijv00yxv5n9l3lm0axhafa1irkg42zpmasd70flgg655"))))
    (build-system font-build-system)
    (home-page "https://github.com/bBoxType/FiraGO")
    (synopsis "Multilingual extension of the Fira Sans font family")
    (description "FiraGO is a multilingual extension of the Fira Sans font
family.  Based on the Fira Sans 4.3 glyph set, FiraGO adds support for the
Arabic, Devanagari, Georgian, Hebrew and Thai scripts.")
    (license license:silofl1.1)))

(define-public font-fira-code
  (package
    (name "font-fira-code")
    (version "5.2")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/tonsky/FiraCode/releases/"
                           "download/" version
                           "/Fira_Code_v" version ".zip"))
       (sha256
        (base32 "1zayrb6k0gd7xdvx7yx44dpip767q0bzhqwf4j6nf8nx02z746jj"))))
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
   ;; XXX The build scripts of version 5 are not freely licensed and
   ;; so we have to stick with version 4 for now:
   ;; <https://bugs.gnu.org/32916>
   (version "4.7.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                   (url "https://github.com/FortAwesome/Font-Awesome")
                   (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0w30y26jp8nvxa3iiw7ayl6rkza1rz62msl9xw3srvxya1c77grc"))))
   (build-system font-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (source (string-append (getcwd) "/fonts"))
                   (fonts (string-append out "/share/fonts")))
              (for-each (lambda (file)
                          (install-file file (string-append fonts "/truetype")))
                        (find-files source "\\.(ttf|ttc)$"))
              (for-each (lambda (file)
                          (install-file file (string-append fonts "/opentype")))
                        (find-files source "\\.(otf|otc)$"))
              #t))))))
   (home-page "https://fontawesome.com/")
   (synopsis "Font that contains a rich iconset")
   (description
    "Font Awesome is a full suite of pictographic icons for easy scalable
vector graphics.")
   (license license:silofl1.1)))

(define-public font-tamzen
  (package
    (name "font-tamzen")
    (version "1.11.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sunaku/tamzen-font")
              (commit (string-append "Tamzen-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00x5fipzqimglvshhqwycdhaqslbvn3rl06jnswhyxfvz16ymj7s"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (let* ((out      (assoc-ref %outputs "out"))
                (font-dir (string-append out "/share/fonts/misc"))
                (psf-dir  (string-append out "/share/kbd/consolefonts")))
           (chdir (assoc-ref %build-inputs "source"))
           (mkdir-p font-dir)
           (mkdir-p psf-dir)
           (for-each (lambda (pcf)
                       (install-file pcf font-dir))
                     (find-files "pcf" "\\.pcf$"))
           (for-each (lambda (psf)
                       (install-file psf psf-dir))
                     (find-files "psf" "\\.psf$"))
           #t))))
    (home-page "https://github.com/sunaku/tamzen-font")
    (synopsis "Monospaced bitmap font for console and X11")
    (description
     "Tamzen is a fork of the @code{Tamsyn} font.  It is programmatically forked
from @code{Tamsyn} version 1.11, backporting glyphs from older versions while
deleting deliberately empty glyphs (which are marked as unimplemented) to
allow secondary/fallback fonts to provide real glyphs at those codepoints.

The @code{TamzenForPowerline} fonts provide additional @code{Powerline} symbols,
which are programmatically injected with @code{bitmap-font-patcher} and
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
    (version "2.51")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "http://www.comicneue.com/comic-neue-" version ".zip"))
              (sha256
               (base32
                "0883542v915crz98v1ij6smgy40dg6gxwsid3j5nbmmqjf69kpal"))))
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
    ;; When updating, also update the hash of the Iosevka variant(s) below.
    (version "4.0.3")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttc-iosevka-" version ".zip"))
       (sha256
        (base32 "1xc45rs09aj899wz9ghyizq6ddbgxpkqq5bl1jc89hls5laf7qjb"))))
    (build-system font-build-system)
    (home-page "https://be5invis.github.io/Iosevka/")
    (synopsis "Coders' typeface, built from code")
    (description
     "Iosevka is a slender monospace sans-serif or slab-serif typeface inspired
by Pragmata Pro, M+, and PF DIN Mono, designed to be the ideal font for
programming.  Iosevka is completely generated from its source code.")
    (license (list license:silofl1.1 ; build artifacts (i.e. the fonts)
                   license:bsd-3)))) ; supporting code

(define-public font-iosevka-slab
  (package
    (inherit font-iosevka)
    (name "font-iosevka-slab")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttc-iosevka-slab-" version ".zip"))
       (sha256
        (base32 "0qpfzyi050zca0bwhb460nvcaarij4srhify0rb8sf9ygpzyvnjh"))))))

(define-public font-iosevka-term
  (package
    (inherit font-iosevka)
    (name "font-iosevka-term")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttf-iosevka-term-" version ".zip"))
       (sha256
        (base32 "092ygzv24wbi8cjjsmq0jkxdf4cm7wqlfj7jkn0cip7nlbyskl3h"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "." ".*"))
             #t)))))))

(define-public font-iosevka-term-slab
  (package
    (inherit font-iosevka)
    (name "font-iosevka-term-slab")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka/"
                           "releases/download/v" version "/"
                           "ttf-iosevka-term-slab-" version ".zip"))
       (sha256
        (base32 "157kdxrxz981ympd2iww66v50vm2cxd7z98vvb36rii94hf30k7x"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "." ".*"))
             #t)))))))

(define-public font-iosevka-aile
  (package
    (inherit font-iosevka)
    (name "font-iosevka-aile")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttc-iosevka-aile-" version ".zip"))
       (sha256
        (base32 "0n52lfn7awc28a4f5yh4my8q4ikzza705kp69chfw9jm2xx79npc"))))))

(define-public font-iosevka-curly
  (package
    (inherit font-iosevka)
    (name "font-iosevka-curly")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka/"
                           "releases/download/v" version  "/"
                           "ttc-iosevka-curly-" version ".zip"))
       (sha256
        (base32 "0m51r0bc25khllq8nsfgsldhwfs7kzl8kqasivjlm14mpv5080fn"))))))

(define-public font-iosevka-curly-slab
  (package
    (inherit font-iosevka)
    (name "font-iosevka-curly-slab")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka/"
                           "releases/download/v" version  "/"
                           "ttc-iosevka-curly-slab-" version ".zip"))
       (sha256
        (base32 "0ffiz2kg43kc2dxv48sjji33ra3kc6sy8vlard93c601fqjmrjws"))))))

(define-public font-iosevka-etoile
  (package
    (inherit font-iosevka)
    (name "font-iosevka-etoile")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttc-iosevka-etoile-" version ".zip"))
       (sha256
        (base32 "1fj8g7sjp9idjh14iqk4wsz3rdarlmq7amcdsr0dznwyivx8a9wx"))))))

(define-public font-iosevka-sparkle
  (package
    (inherit font-iosevka)
    (name "font-iosevka-sparkle")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttc-iosevka-sparkle-" version ".zip"))
       (sha256
        (base32 "00dw894930xdikai8bcaimp0a3720diwi0r7ii52jwl4d70w53dx"))))))

(define-public font-sarasa-gothic
  (package
    (name "font-sarasa-gothic")
    (version "0.12.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/be5invis/Sarasa-Gothic"
                           "/releases/download/v" version
                           "/sarasa-gothic-ttc-" version ".7z"))
       (sha256
        (base32 "09v65k00g8m953s6riz9xjzb2jgr6v5pdhcllalzzl7c1cn2xl3k"))))
    (build-system font-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (mkdir "source")
                      (chdir "source")
                      (invoke "7z" "x" source))))))
    (native-inputs `(("p7zip" ,p7zip)))
    (home-page "https://github.com/be5invis/Sarasa-Gothic")
    (license license:silofl1.1)
    (synopsis "Sarasa Gothic / 更纱黑体 / 更紗黑體 / 更紗ゴシック / 사라사 고딕")
    (description
     "Sarasa Gothic is a programming font based on Iosevka and Source Han Sans,
most CJK characters are same height, and double width as ASCII characters.")))

(define-public font-space-grotesk
  (package
    (name "font-space-grotesk")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/floriankarsten/space-grotesk")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aiivn0rl7ydiyqvsr0fa2hx82h3br3x48w3100fcly23n0fdcby"))))
    (build-system font-build-system)
    ;; TODO: Package fontmake and gftools and build from source.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'install-license-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "OFL.txt" doc)
               #t))))))
    (home-page "https://floriankarsten.github.io/space-grotesk/")
    (synopsis "Proportional variant of the fixed-width Space Mono family")
    (description
     "Space Grotesk is a proportional sans-serif typeface variant based on Colophon
Foundry's fixed-width Space Mono family.  It retains the monospace's idiosyncratic
details while optimizing for improved readability at non-display sizes.

Space Grotesk includes Latin Vietnamese, Pinyin, and all Western, Central, and
South-Eastern European language support, as well as several OpenType features:
old-style and tabular figures, superscript and subscript numerals, fractions,
and stylistic alternates.")
    (license license:silofl1.1)))

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
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/material-design-icons")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17q5brcqyyc8gbjdgpv38p89s60cwxjlwy2ljnrvas5cj0s62np0"))))
    (build-system font-build-system)
    (home-page "https://google.github.io/material-design-icons")
    (synopsis "Icon font of Google Material Design icons")
    (description
     "Material design system icons are simple, modern, friendly, and sometimes
quirky.  Each icon is created using our design guidelines to depict in simple
and minimal forms the universal concepts used commonly throughout a UI.
Ensuring readability and clarity at both large and small sizes, these icons
have been optimized for beautiful display on all common platforms and display
resolutions.")
    (license license:asl2.0)))

(define-public font-opendyslexic
  (package
    (name "font-opendyslexic")
    (version "0.91.12")
    (source
      (origin
        (method url-fetch/zipbomb)
        (uri (string-append "https://github.com/antijingoist/opendyslexic/"
                            "releases/download/v" version
                            "/opendyslexic-0.910.12-rc2-2019.10.17.zip"))
        (sha256
         (base32
          "11ml7v4iyf3hr0fbnkwz8afb8vi58wbcfnmn4gyvrwh9jk5pybdr"))))
    (build-system font-build-system)
    (native-inputs `(("unzip" ,unzip)))
    (home-page "https://opendyslexic.org/")
    (synopsis "Font for dyslexics and high readability")
    (description "OpenDyslexic is a font designed to help readability for some
of the symptoms of dyslexia.  Letters have heavy weighted bottoms to provide
an indication of orientation to make it more difficult to confuse with other
similar letters.  Consistently weighted bottoms can also help reinforce the
line of text.  The unique shapes of each letter can help prevent flipping and
swapping.  The italic style for OpenDyslexic has been crafted to be used for
emphasis while still being readable.")
    (license license:silofl1.1)))

(define-public font-open-dyslexic
  (deprecated-package "font-open-dyslexic" font-opendyslexic))

(define-public font-openmoji
  (package
    (name "font-openmoji")
    (version "12.4.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri
        (string-append "https://github.com/hfg-gmuend/openmoji/"
                       "releases/download/" version
                       "/openmoji-font.zip"))
       (sha256
        (base32
         "0wvvg5vnc950h8v23wfgjyi7rv89mgm5hqq6viqv0bxcc3azglxb"))))
    (build-system font-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://openmoji.org")
    (synopsis "Font for rendering emoji characters")
    (description
     "This package provides the OpenMoji font in both color and black
variants.")
    (license license:cc-by-sa4.0)))

(define-public font-dosis
  (package
    (name "font-dosis")
    (version "1.7")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://web.archive.org/web/20180228233737/"
                           "https://www.impallari.com/media/releases/dosis-"
                           "v" version ".zip"))
       (sha256
        (base32 "1qhci68f68mf87jd69vjf9qjq3wydgw1q7ivn3amjb65ls1s0c4s"))))
    (build-system font-build-system)
    (home-page (string-append "https://web.archive.org/web/20180228233737/"
                              "https://www.impallari.com/dosis"))
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

(define-public font-sil-andika
  (package
    (name "font-sil-andika")
    (version "5.000")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://software.sil.org/downloads/r/andika/Andika-"
                    version ".zip"))
              (sha256
               (base32
                "01zm7p32gxfwmv7h3cfj2vx59846w2y6rxqy67grn2dyjh8pljv0"))))
    ;; As for Gentium (see above), the TTF files are considered source.
    (build-system font-build-system)
    (synopsis "Sans serif font designed especially for literacy use")
    (description
     "Andika SIL is a sans serif, Unicode-compliant font designed especially
for literacy use, taking into account the needs of beginning readers.  The
focus is on clear, easy-to-perceive letterforms that will not be readily
confused with one another.  This package provides only TrueType files (TTF).")
    (home-page "https://software.sil.org/andika/")
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
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/madmalik/mononoki/")
                    (commit version)))
              (sha256
               (base32
                "07gh84iw84g8fha3gx4yjyl1rsksqhy2kk38f01m048sgjp6ww5j"))
              (file-name (git-file-name name version))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files ".")))))))
    (synopsis "Font for programming and code review")
    (description
     "Mononoki is a typeface by Matthias Tellen, created to enhance code
formatting.")
    (home-page "https://madmalik.github.io/mononoki/")
    (license license:silofl1.1)))

(define-public font-public-sans
  (package
    (name "font-public-sans")
    (version "1.008")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uswds/public-sans")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qhyxbjv1rnydfpqzd18fbiyz75p4sabphy8yj07hyq0hidp5xsf"))))
    (build-system font-build-system)
    (home-page "https://public-sans.digital.gov/")
    (synopsis "Neutral typeface for interfaces, text, and headings")
    (description
     "Public Sans is a strong, neutral, sans-serif typeface for text or
display based on Libre Franklin.")
    (license license:silofl1.1)))

(define-public font-hermit
  (package
    (name "font-hermit")
    (version "2.0")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://pcaro.es/d/otf-hermit-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "09rmy3sbf1j1hr8zidighjgqc8kp0wsra115y27vrnlf10ml6jy0"))))
    (build-system font-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://pcaro.es/p/hermit/")
    (synopsis "Monospace font")
    (description
     "Hermit is a monospace font designed to be clear, pragmatic and very
readable.  Its creation has been focused on programming.  Every glyph was
carefully planned and calculated, according to defined principles and rules.
For this reason, Hermit is coherent and regular.

Symbols stand out from common text.  Dots and commas are easily seen, and
operators are clear even when not surrounded by spaces.  Similar characters
have been designed to be very distinguishable from each other.")
    (license license:silofl1.1)))

(define-public font-dseg
  (package
    (name "font-dseg")
    (version "0.46")
    (source
      (origin
        (method url-fetch/zipbomb)
        (uri
          (string-append "https://github.com/keshikan/DSEG/"
                         "releases/download/v" version
                         "/fonts-DSEG_v"
                         (string-concatenate (string-split version #\.))
                         ".zip"))
        (sha256
          (base32 "13133kpa1ndsji9yq5ppkds5yq2y094qvrv2f83ah74p40sz9hm6"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (font-dir (string-append out "/share/fonts"))
                    (truetype-dir (string-append font-dir "/truetype")))
               (with-directory-excursion
                 (string-append "fonts-DSEG_v"
                                (apply string-append (string-split ,version
                                                                   #\.)))
                 (for-each (lambda (f) (install-file f truetype-dir))
                           (find-files "." "\\.ttf$"))
               #t)))))))
    (home-page "https://www.keshikan.net/fonts-e.html")
    (synopsis "DSEG: 7-segment and 14-segment fonts")
    (description
     "DSEG is a font family that imitates seven- and fourteen-segment LCD
displays (7SEG, 14SEG).  DSEG includes the roman alphabet and symbol glyphs.
This package provides the TrueType fonts.")
    (license license:silofl1.1)))

(define-public font-jetbrains-mono
  (package
    (name "font-jetbrains-mono")
    (version "2.221")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/JetBrains/JetBrainsMono/releases/"
                       "download/v" version "/JetBrainsMono-" version ".zip"))
       (sha256
        (base32 "1acrgv2q9vxviirpi01xy67pkkswyssw4dn5pgyvrnjxr85cgjrg"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install-license-files 'change-directory-to-archive-root
           ;; Find the license file outside of the default subdirectory.
           (lambda _
             (chdir "..")
             #t))
         (replace 'install-license-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "OFL.txt" doc)
               #t))))))
    (home-page "https://www.jetbrains.com/lp/mono/")
    (synopsis "Mono typeface for developers")
    (description
     "JetBrains Mono is a font family dedicated to developers.  JetBrains
Mono’s typeface forms are simple and free from unnecessary details.  Rendered
in small sizes, the text looks crisper.")
    (license license:asl2.0)))

(define-public font-juliamono
  (package
    (name "font-juliamono")
    (version "0.031")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cormullion/juliamono")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pcz2qaw0g0gak4plvhgg3m76h4gamffa373r52dzx0qwn1i1cf1"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-website-folder
           ;; This folder contains other unrelated fonts.
           (lambda _
             (delete-file-recursively "website")
             #t)))))
    (home-page "https://github.com/cormullion/juliamono")
    (synopsis "Monospaced font for programming")
    (description
     "JuliaMono is a monospaced font for scientific and technical computing,
designed to work for programming in the Julia Programming Language and other
text environments.")
    (license license:silofl1.1)))

(define-public font-vazir
  (package
    (name "font-vazir")
    (version "22.1.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri
        (string-append "https://github.com/rastikerdar/vazir-font/"
                       "releases/download/v" version
                       "/vazir-font-v" version ".zip"))
       (sha256
        (base32
         "0w3gwb5q33x5llw7cfs8qhaxr4ssg6rsx4b9day3993zn24xq031"))))
    (build-system font-build-system)
    (home-page "https://rastikerdar.github.io/vazir-font/")
    (synopsis "Vazir Persian typeface")
    (description
     "Vazir is a beautiful and elegant Persian typeface originally based on
DejaVu, and comes in six variants: Thin, Light, Normal, Medium, Bold, and
Black.  This package provides four versions of Vazir:

@itemize
@item @code{Vazir}: The main version; includes Latin glyphs from Roboto.
@item @code{Vazir-FD}: Like @code{Vazir}, but (always) uses Farsi digit glyphs
instead of Latin ones.
@item @code{Vazir-WOL}: Like @code{Vazir}, but without Roboto's Latin glyphs.
@item @code{Vazir-FD-WOL}: Combination of @code{Vazir-FD} and @code{Vazir-WOL}:
always uses Farsi digits, and does not include Latin glyphs from Roboto.
@end itemize\n")
    (license
     ;; See https://github.com/rastikerdar/vazir-font/blob/master/LICENSE for
     ;; details.
     (list license:public-domain        ; the Vazir modifications to DejaVu
                                        ; and the DejaVu modifications to...
           (license:x11-style           ; ...the Bitstream Vera typeface
            "file://LICENSE" "Bitstream Vera License")
           license:asl2.0))))           ; Latin glyphs from Roboto

(define-public font-meera-inimai
  (package
    (name "font-meera-inimai")
    (version "2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/smc/meera-inimai")
             (commit "0f39cdd7dbf1b6d1bed7df85834d33789dce20a7")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1x5mhrpx24imh0r4l83mkaiszxgwi1q4ppyyvq63h3ddwk20cwdg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("fontforge" ,fontforge)
       ("harfbuzz" ,harfbuzz "bin")
       ("python" ,python-minimal)
       ("python-fonttools" ,python-fonttools)
       ("python-google-brotli" ,python-google-brotli)))
    (arguments
     `(#:make-flags (list "PY=python3"
                          (string-append "DESTDIR=" %output)
                          "fontpath=/share/fonts/truetype")
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://gitlab.com/smc/meera-inimai")
    (synopsis "Meera Inimai Tamil font")
    (description "Meera Inimai is a Unicode font for the Tamil Script.  Meera
Inimai is a san-serif typeface.  It is best used as a screen font for body
text.  It is also useful for body text of printed pamphlets or single page
designs.  Meera Inimai can be thought of as similar to Helvetica and its
variation Arial.  Tamil characters are inherently vertically-elliptical.  The
orthography of Roman glyphs of Meera Inimai are also based on this
characteristic so that they sit smoothly with the Tamil glyphs.")
    (license license:silofl1.1)))

(define-public font-ipa-mj-mincho
  (package
    (name "font-ipa-mj-mincho")
    (version "006.01")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://mojikiban.ipa.go.jp/OSCDL/IPAmjMincho"
                                  "/ipamjm" (string-join (string-split version #\.) "")
                                  ".zip"))
              (sha256
               (base32
                "0s2vs9p7vd7ajnn6c2icli069sjwi4d45a39fczqpwwn507lwj9m"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc-dir (string-append (assoc-ref outputs "out")
                                           "/share/doc/font-ipa-mj-mincho")))
               (mkdir-p doc-dir)
               (copy-file "Readme.txt" (string-append doc-dir "/README"))
               (copy-file "IPA_Font_License_Agreement_v1.0.txt"
                          (string-append doc-dir "/LICENSE"))
               #t))))))
    (home-page "https://mojikiban.ipa.go.jp/1300.html")
    (synopsis "Japanese font from the Information-technology Promotion Agency")
    (description "MJM Mincho is a font that aims at, for example, allowing you
to write people's name, or for formal business situations where it is necessary
to have a detailed and proper character style.")
    (license license:ipa)))

(define-public font-fontna-yasashisa-antique
  (package
    (name "font-fontna-yasashisa-antique")
    (version "0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://flop.sakura.ne.jp/font/fontna-op/"
                                  "YasashisaAntiqueFont.zip"))
              (sha256
               (base32
                "1hl2qk3lzmh9h2vv5647vhlslkn3vqbq9rqgp4wzybajafx8c6nj"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; encoding issues cause many phases to fail
         (add-after 'unpack 'fix-encoding
           (lambda _
             ;; This directory, TrueType（サポート外）, is not properly encoded,
             ;; which makes rename-file fail. Instead, use shell globbing to
             ;; select and rename the directory.
             (invoke "sh" "-c" "mv TrueType* TrueType")
             #t)))))
    (native-inputs
     `(("bash" ,bash-minimal)
       ("coreutils" ,coreutils)))
    (home-page "http://www.fontna.com/blog/1122/")
    (synopsis "Mix font of gothic kanji and minchou kana")
    (description "Antique is a font that is popular to write manga bubbles,
dictionary headwords and picture books.  This font reduces the thickness
differences in characters compared to other antique fonts.")
    (license (list license:ipa
                   (license:non-copyleft "mplus-TESTFLIGHT-057/LICENSE_E")))))

(define-public font-mplus-testflight
  (package
    (name "font-mplus-testflight")
    (version "063a")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://osdn.net/frs/redir.php?"
                                  "m=gigenet&f=mplus-fonts%2F62344%2Fmplus-TESTFLIGHT-"
                                  version ".tar.xz"))
              (file-name (string-append name "-" version ".tar.xz"))
              (sha256
               (base32
                "0yfx9i77638yrgclnwwl4af79ri3vifi1nslwi6mgzva9cxrgss4"))))
    (build-system font-build-system)
    (home-page "https://mplus-fonts.osdn.jp/index.html")
    (synopsis "Japanese font collection")
    (description "M+ is a collection of Japanese fonts with all Latin glyph
sets, with Basic Latin, Latin-1 Supplement, Latin Extended-A, and IPA
Extensions.  In addition to European letters used in many Western European
languages, it contains Japanese characters, including Kana glyphs and more
than 5,300 Kanji glyphs, as well major international phonetic symbols,
operators and special symbols.")
    (license (license:non-copyleft "file:///LICENSE_E"))))

(define-public font-catamaran
  (let ((commit "7559b4906f9c9148fb22c6f89508c3053a78a296")
        (revision "1"))
    (package
      (name "font-catamaran")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/VanillaandCream/Catamaran-Tamil")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1wpp41l7j2kpgnyavhgxcc5wp852a4wqsnwravn39gp980s84yxw"))))
      (build-system font-build-system)
      (home-page "https://github.com/VanillaandCream/Catamaran-Tamil")
      (synopsis "9 weight Tamil and Latin type")
      (description "Catamaran is a 9 weight Tamil and Latin type.  Catamaran
is a stylish type with a polished yet relaxed feel.  Its versatility makes it
suitable for a wide range of uses.")
      (license license:silofl1.1))))

(define-public font-cozette
  (package
    (name "font-cozette")
    (version "1.9.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/slavfox/Cozette")
                     (commit (string-append "v." version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mb5ns6705piwgjw1g10czsakhyc1jnvxh342ixw8m5f1gf4595n"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'build
           (lambda _
             (invoke "python3" "build.py" "fonts"))))))
    (native-inputs
     `(("fontforge" ,fontforge)
       ("python" ,python)
       ("python-crayons" ,python-crayons)
       ("python-fonttools" ,python-fonttools)
       ("python-numpy" ,python-numpy)
       ("python-pillow" ,python-pillow)))
    (home-page "https://github.com/slavfox/Cozette")
    (synopsis "Bitmap programming font")
    (description "Cozette is a 6x13px (bounding box) bitmap font based on Dina
and heavily inspired by Creep.")
    (license license:expat)))
