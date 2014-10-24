;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
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

(define-module (gnu packages feh)
  #:use-module (gnu packages)
  #:use-module (gnu packages image)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public feh
  (package
    (name "feh")
    (version "2.12")
    (home-page "https://feh.finalrewind.org/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0ckhidmsms2l5jycp0qf71jzmb3bpbhjq3bcgfpvfvszah7pmq30"))))
    (build-system gnu-build-system)
    (arguments
      '(#:phases (alist-delete 'configure %standard-phases)
        #:tests? #f
        #:make-flags
          (list "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (inputs `(("imlib2" ,imlib2)
              ("curl" ,curl)
              ("libpng" ,libpng)
              ("libxt" ,libxt)
              ("libx11" ,libx11)
              ("libxinerama" ,libxinerama)))
    (synopsis "Fast and light imlib2-based image viewer")
    (description
      "feh is an X11 image viewer aimed mostly at console users.
Unlike most other viewers, it does not have a fancy GUI, but simply
displays images.  It can also be used to set the desktop wallpaper.
It is controlled via commandline arguments and configurable key/mouse
actions.")

    ;; The license is really the Expat license, with additional wording in the
    ;; 2nd paragraph: "acknowledgment shall be given in the documentation and
    ;; software packages that this Software was used."
    (license (x11-style "file://COPYING"
                        "See 'COPYING' in the distribution."))))
