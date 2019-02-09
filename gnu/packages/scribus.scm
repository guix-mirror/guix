;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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

(define-module (gnu packages scribus)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public scribus
  (package
    (name "scribus")
    (version "1.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/scribus/scribus-devel/"
                           version "/scribus-" version ".tar.xz"))
       (sha256
        (base32
         "00ys0p6h3iq77kh72dkl0qrf7qvznq18qdrgiq10gfxja1995034"))
       (patches (append
                 ;; Scribus relies heavily on Poppler internals, which have
                 ;; changed a lot since the latest Scribus release (2018-04).
                 ;; Thus, we require a bunch of patches to stay compatible.
                 (search-patches "scribus-poppler.patch")
                 (list (origin
                         (method url-fetch)
                         (uri (string-append
                               "https://github.com/scribusproject/scribus/commit/"
                               "7d4ceeb5cac32287769e3c0238699e0b3e56c24d.patch"))
                         (file-name "scribus-poppler-0.64.patch")
                         (sha256
                          (base32
                           "1kr27bfzkpabrh42nsrrvlqyycdg9isbavpaa5spgmrhidcg02xj")))
                       (origin
                         (method url-fetch)
                         (uri (string-append
                               "https://github.com/scribusproject/scribus/commit/"
                               "76561c1a55cd07c268f8f2b2fea888532933700b.patch"))
                         (file-name "scribus-poppler-config.patch")
                         (sha256
                          (base32
                           "01k18xjj82c3ndzp89dlpfhhdccc8z0acf8b04r592jyr5y9rc19")))
                       (origin
                         (method url-fetch)
                         (uri (string-append
                               "https://github.com/scribusproject/scribus/commit/"
                               "8e05d26c19097ac2ad5b4ebbf40a3771ee6faf9c.patch"))
                         (file-name "scribus-poppler-0.69.patch")
                         (sha256
                          (base32
                           "1avdmsj5l543j0irq18nxgiw99n395jj56ih5dsal59fn0wbqk42")))
                       (origin
                         (method url-fetch)
                         (uri (string-append "https://git.archlinux.org/svntogit/"
                                             "community.git/plain/trunk/scribus-"
                                             "poppler-0.70.patch?h=packages/scribus&id="
                                             "8ef43ee2fceb0753ed5a76bb0a11c84775898ffc"))
                         (file-name "scribus-poppler-0.70.patch")
                         (sha256
                          (base32
                           "0dw7ix3jaj0y1q97cmmqwb2qgdx760yhxx86wa8rnx0xhfi5x6qr"))))))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each (lambda (file)
                       (substitute* file
                         ;; These are required for compatibility with Poppler 0.71.
                         (("GBool") "bool") (("gTrue") "true") (("gFalse") "false")
                         ;; ...and this for Poppler 0.72.
                         (("getCString") "c_str")))
                     (find-files "scribus/plugins/import/pdf"))
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test target
       #:configure-flags
       '("-DWANT_GRAPHICSMAGICK=1")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Fix "ImportError: No module named _sysconfigdata_nd" where
             ;; Scribus checks PATH and eventually runs system's Python
             ;; instead of package's.
             (let* ((out (assoc-ref outputs "out"))
                    (py2 (assoc-ref inputs "python")))
               (wrap-program (string-append out "/bin/scribus")
                 `("PATH" ":" prefix (,(string-append py2 "/bin")))))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("cairo" ,cairo)
       ("cups" ,cups)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("graphicsmagick" ,graphicsmagick)
       ("harfbuzz" ,harfbuzz)
       ("hunspell" ,hunspell)
       ("icu4c" ,icu4c)
       ("lcms" ,lcms)
       ("libcdr" ,libcdr)
       ("libfreehand" ,libfreehand)
       ("libjpeg" ,libjpeg)
       ("libmspub" ,libmspub)
       ("libpagemaker" ,libpagemaker)
       ("librevenge" ,librevenge)
       ("libtiff" ,libtiff)
       ("libvisio" ,libvisio)
       ("libxml2" ,libxml2)
       ("libzmf" ,libzmf)
       ("openssl" ,openssl)
       ("podofo" ,podofo)
       ("poppler" ,poppler)
       ("python" ,python-2)             ;need Python library
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("util-linux" ,util-linux)))
    (home-page "https://www.scribus.net")
    (synopsis "Desktop publishing and page layout program")
    (description
     "Scribus is a @dfn{desktop publishing} (DTP) application and can be used
for many tasks; from brochure design to newspapers, magazines, newsletters and
posters to technical documentation.  Scribus supports professional DTP
features, such as CMYK color and a color management system to soft proof
images for high quality color printing, flexible PDF creation options,
Encapsulated PostScript import/export and creation of four color separations,
import of EPS/PS and SVG as native vector graphics, Unicode text including
right to left scripts such as Arabic and Hebrew via freetype.")
    (license license:gpl2+)))
