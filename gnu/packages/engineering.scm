;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages engineering)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (srfi srfi-1))

(define-public librecad
  (package
    (name "librecad")
    (version "2.0.6-rc")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/LibreCAD/LibreCAD/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1n1mh8asj6yrl5hi438dvizmrbqk1kni5xkizhi3pdmkg7z3hksm"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (alist-cons-after
        'unpack
        'patch-paths
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
            (substitute* "librecad/src/lib/engine/rs_system.cpp"
              (("/usr/share") (string-append out "/share")))))
        (alist-replace
         'configure
         (lambda* (#:key inputs #:allow-other-keys)
           (system* "qmake" (string-append "BOOST_DIR="
                                           (assoc-ref inputs "boost"))))
         (alist-replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (mkdir-p (string-append out "/bin"))
              (mkdir-p (string-append out "/share/librecad"))
              (copy-file "unix/librecad"
                         (string-append out "/bin/librecad"))
              (copy-recursively "unix/resources"
                                (string-append out "/share/librecad"))))
          %standard-phases)))))
    (inputs
     `(("boost" ,boost)
       ("muparser" ,muparser)
       ("freetype" ,freetype)
       ("qt" ,qt-4)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (home-page "http://librecad.org/")
    (synopsis "Computer-aided design (CAD) application")
    (description
     "LibreCAD is a 2D Computer-aided design (CAD) application for creating
plans and designs.")
    (license license:gpl2)))
