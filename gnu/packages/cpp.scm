;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ethan R. Jones <doubleplusgood23@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
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

(define-module (gnu packages cpp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls))

(define-public libzen
  (package
    (name "libzen")
    (version "0.4.37")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://mediaarea.net/download/source/"
                                  name "/" version "/"
                                  name "_" version ".tar.bz2"))
              (sha256
               (base32
                "1hcsrmn85b0xp0mp33aazk7g071q1v3f163nnhv8b0mv9c4bgsfn"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       ;; build scripts not in root of archive
       (modify-phases %standard-phases
         (add-after 'unpack 'pre-configure
           (lambda _
             (chdir "Project/GNU/Library")))
         (add-after 'pre-configure 'autogen
           (lambda _
             (zero? (system* "sh" "autogen.sh")))))))
    (home-page "https://github.com/MediaArea/ZenLib")
    (synopsis "C++ utility library")
    (description "ZenLib is a C++ utility library.  It includes classes for handling
strings, configuration, bit streams, threading, translation, and cross-platform
operating system functions.")
    (license license:zlib)))

(define-public rct
  (let* ((commit "b3e6f41d9844ef64420e628e0c65ed98278a843a")
         (revision "2")
         (version (git-version "0.0.0" revision commit)))
    (package
      (name "rct")
      (version version)
      (home-page "https://github.com/Andersbakken/rct")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "1m2931jacka27ghnpgf1z1plkkr64z0pga4r4zdrfpp2d7xnrdvb"))
                (patches (search-patches "rct-add-missing-headers.patch"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags
         '("-DWITH_TESTS=ON"            ; To run the test suite
           "-DRCT_RTTI_ENABLED=ON")))
      (native-inputs
       `(("cppunit" ,cppunit)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("openssl" ,openssl)
         ("zlib" ,zlib)))
      (synopsis "C++ library providing Qt-like APIs on top of the STL")
      (description "Rct is a set of C++ tools that provide nicer (more Qt-like)
 APIs on top of Standard Template Library (@dfn{STL}) classes.")
      (license (list license:expat        ; cJSON
                     license:bsd-4)))))   ; everything else (LICENSE.txt)
