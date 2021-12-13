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

(define-module (gnu packages lesstif)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xorg))


(define-public lesstif
  (package
    (name "lesstif")
    (version "0.95.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://sourceforge/lesstif/lesstif/" version
               "/lesstif-" version ".tar.bz2"))
        (sha256
          (base32
            "1qzpxjjf7ri1jzv71mvq5m9g8hfaj5yzwp30rwxlm6n2b24a6jpb"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list printproto))
    (inputs
      (list libxext libxt))
    (home-page "http://lesstif.sourceforge.net/")
    (synopsis "Clone of the Motif toolkit for the X window system")
    (description "Clone of the Motif toolkit for the X window system.")
    (license license:gpl2+))) ; some files are lgpl2.1+ or x11
