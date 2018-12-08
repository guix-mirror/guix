;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages inkscape)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config))

(define-public inkscape
  (package
    (name "inkscape")
    (version "0.92.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://media.inkscape.org/dl/"
                                  "resources/file/"
                                  "inkscape-" version ".tar.bz2"))
              (sha256
               (base32
                "1chng2yw8dsjxc9gf92aqv7plj11cav8ax321wmakmv5bb09cch6"))
              (patches
               (list (origin
                       (method url-fetch)
                       (uri (string-append "https://gitlab.com/inkscape/inkscape/commit/"
                                           "a600c6438fef2f4c06f9a4a7d933d99fb054a973.diff"))
                       (file-name "inkscape-poppler-compat.patch")
                       (sha256
                        (base32
                         "19dam5vsy571xszgjddl5g0958dmcsv0wvgxidp4bhj2lban222i")))
                     (origin
                       (method url-fetch)
                       (uri (string-append "https://gitlab.com/inkscape/inkscape/commit/"
                                           "fa1c469aa8c005e07bb8676d72af9f7c16fae3e0.diff"))
                       (file-name "inkscape-poppler-compat2.patch")
                       (sha256
                        (base32
                         "14k9yrfjz4nx3bz9dk91q74mc0i7rvl2qzkwhcy1br71yqjvngn5")))
                     (search-patch "inkscape-poppler-compat3.patch")
                     (origin
                       (method url-fetch)
                       (uri (string-append "https://gitlab.com/inkscape/inkscape/commit/"
                                           "d047859d90cef3784e2d13e40887a70d8d517897.diff"))
                       (file-name "inkscape-poppler-compat4.patch")
                       (sha256
                        (base32
                         "0xdfg3q4g4m15z7wna4brjn5j4kr15qiqc2f25vcw2nnr6x54qcp")))
                     (origin
                       (method url-fetch)
                       (uri (string-append "https://gitlab.com/inkscape/inkscape/commit/"
                                           "b3d59cc8106da3bf6020a6c47eeb3b8a7bbae1a9.diff"))
                       (file-name "inkscape-poppler-compat5.patch")
                       (sha256
                        (base32
                         "0haviy66q9szizmvb82msfj80bb3wgi1fnq3ml8fyfp8l90a1217")))))))
    (build-system cmake-build-system)
    (inputs
     `(("aspell" ,aspell)
       ("gtkmm" ,gtkmm-2)
       ("gtk" ,gtk+-2)
       ("gsl" ,gsl)
       ("poppler" ,poppler)
       ("libpng" ,libpng)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libgc" ,libgc)
       ("freetype" ,freetype)
       ("popt" ,popt)
       ("potrace" ,potrace)
       ("python" ,python-2)
       ("lcms" ,lcms)
       ("boost" ,boost)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    ;; FIXME: tests require gmock
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-icon-cache-generator
           (lambda _
             (substitute* "share/icons/application/CMakeLists.txt"
              (("gtk-update-icon-cache") "true"))
             #t)))))
    (home-page "https://inkscape.org/")
    (synopsis "Vector graphics editor")
    (description "Inkscape is a vector graphics editor.  What sets Inkscape
apart is its use of Scalable Vector Graphics (SVG), an XML-based W3C standard,
as the native format.")
    (license license:gpl2+)))
