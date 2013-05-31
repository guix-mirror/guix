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

(define-module (gnu packages xlockmore)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages linux))

(define-public xlockmore
  (package
    (name "xlockmore")
    (version "5.42")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.tux.org/~bagleyd/xlock/xlockmore-"
                                 version "/xlockmore-" version ".tar.bz2"))
             (sha256
              (base32
               "17xicps92ah9377zk65k9l1bmvzzj3bpxzzwxx21g9696l71gr0z"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))                 ; no such thing as a test suite
    (inputs
     `(("libX11" ,libx11)
       ("libXext" ,libxext)
       ("libXt" ,libxt)
       ("linux-pam" ,linux-pam)))
    (home-page "http://www.tux.org/~bagleyd/xlockmore.html")
    (synopsis "Screen locker for the X Window System")
    (description
     "XLockMore is a classic screen locker and screen saver for the
X Window System.")
    (license (bsd-style #f "See xlock.c.")
             ;; + GPLv2 in modes/glx/biof.c.
             )))
