;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages ratpoison)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages fontutils))

(define-public ratpoison
  (package
    (name "ratpoison")
    (version "1.4.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/ratpoison/ratpoison-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0v4mh8d3vsh5xbbycfdl3g8zfygi1rkslh1x7k5hi1d05bfq3cdr"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("libXi" ,libxi)
       ("readline" ,readline)
       ("xextproto" ,xextproto)
       ("libXtst" ,libxtst)
       ("freetype" ,freetype)
       ("fontconfig" ,fontconfig)
       ("libXft" ,libxft)
       ("libXpm" ,libxpm)
       ("libXt" ,libxt)
       ("inputproto" ,inputproto)
       ("libX11" ,libx11)))
    (home-page "http://www.nongnu.org/ratpoison/")
    (synopsis "A simple mouse-free tiling window manager")
    (description
     "Ratpoison is a simple window manager with no fat library
dependencies, no fancy graphics, no window decorations, and no
rodent dependence.  It is largely modelled after GNU Screen which
has done wonders in the virtual terminal market.

The screen can be split into non-overlapping frames.  All windows
are kept maximized inside their frames to take full advantage of
your precious screen real estate.

All interaction with the window manager is done through keystrokes.
Ratpoison has a prefix map to minimize the key clobbering that
cripples Emacs and other quality pieces of software.")
    (license gpl2+)))
