;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages ebook)
  #:use-module ((guix licenses) #:select (gpl3))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public chmlib
  (package
    (name "chmlib")
    (version "0.40")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.jedrea.com/chmlib/chmlib-"
                                 version ".tar.bz2"))
             (sha256
               (base32
                "18zzb4x3z0d7fjh1x5439bs62dmgsi4c1pg3qyr7h5gp1i5xcj9l"))))
    (build-system gnu-build-system)
    (home-page "http://www.jedrea.com/chmlib/")
    (synopsis "Library for CHM files")
    (description "CHMLIB is a library for dealing with ITSS/CHM format files.")
    (license gpl3))) ; some files are under various other licenses, see COPYRIGHT
