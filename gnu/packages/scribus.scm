;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/scribus/scribus-devel/"
                           version "/scribus-" version ".tar.xz"))
       (sha256
        (base32
         "0kyp45vidxa3v35ic9592db4zk3m8al26vck38q5v7z14x3hp8vk"))
       (patches
        (list
         (origin
           (method url-fetch)
           (uri (string-append "https://github.com/scribusproject/scribus/commit/"
                               "61186c7ef083046b7e0c908952e8a773e2787d82.patch"))
           (file-name "scribus-fix-poppler-0.58-breakage.patch")
           (sha256
            (base32 "189qw9xmgz01xz1w1bi9lzrp399zk1j1iz5qdhchdrhgnd69b7ly")))
         (origin
           (method url-fetch)
           (uri (string-append "https://github.com/scribusproject/scribus/commit/"
                               "d82b1c989bd0e79b5611521f671adbfb94996e5e.patch"))
           (file-name "scribus-fix-poppler-packaging.patch")
           (sha256
            (base32 "1p9s18jjvj2h0ba1xvk1zhmnn4f4n3ykrgb56mjd6in30h0vrykx")))))
       (modules '((guix build utils)))
       (snippet
        ;; Fix typo.  Equivalent to patch at
        ;; https://bugs.scribus.net/view.php?id=14850
        '(substitute* "cmake/modules/FindLIBPODOFO.cmake"
           (("find_package\\(OPENSSL\\)") "find_package(OpenSSL)")))))
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
