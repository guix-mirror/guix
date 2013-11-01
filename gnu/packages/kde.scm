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

(define-module (gnu packages kde)
  #:use-module ((guix licenses) #:select (bsd-2))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages qt))

(define-public automoc4
  (package
    (name "automoc4")
    (version "0.9.88")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.kde.org/stable/" name
                                "/" version "/" name "-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0jackvg0bdjg797qlbbyf9syylm0qjs55mllhn11vqjsq3s1ch93"))))
    (build-system cmake-build-system)
    (inputs
     `(("qt" ,qt-4)))
    (arguments
     `(#:tests? #f)) ; no check target
    (home-page "http://techbase.kde.org/Development/Tools/Automoc4")
    (synopsis "build tool for KDE")
    (description "KDE desktop environment")
    (license bsd-2)))
