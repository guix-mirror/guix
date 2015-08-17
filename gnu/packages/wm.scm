;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
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

(define-module (gnu packages wm)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages asciidoc)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages web)
  #:use-module (guix download)
  #:use-module (guix git-download))

(define-public i3-wm
  (package
    (name "i3-wm")
    (version "4.10.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://i3wm.org/downloads/i3-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1lq7h4w7m0hi31iva8g7yf1sc11ispnknxjdaj9agld4smxqb44j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc" (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:tests? #f)) ; no test suite
    (inputs
     `(("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)
       ("xcb-util-cursor" ,xcb-util-cursor)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-wm" ,xcb-util-wm)
       ("libxkbcommon" ,libxkbcommon)
       ("libev" ,libev)
       ("libyajl" ,libyajl)
       ("asciidoc" ,asciidoc)
       ("xmlto" ,xmlto)
       ("perl-pod-simple" ,perl-pod-simple)
       ("docbook-xml" ,docbook-xml)
       ("libx11" ,libx11)
       ("pcre" ,pcre)
       ("startup-notification" ,startup-notification)
       ("pango" ,pango)
       ("cairo" ,cairo)))
    (native-inputs
     `(("which" ,which)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (home-page "http://i3wm.org/")
    (synopsis "Improved tiling window manager")
    (description "i3 is a tiling window manager, completely written
from scratch.  i3 is primarily targeted at advanced users and
developers.")
    (license bsd-3)))
