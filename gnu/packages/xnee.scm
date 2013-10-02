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

(define-module (gnu packages xnee)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config))

(define-public xnee
  (package
    (name "xnee")
    (version "3.16")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/xnee/xnee-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1zkn66zpnbxryrb76cc3r2via6216868l1xsnl3vdymbsy23sf87"))))
    (build-system gnu-build-system)
    (inputs
     `(("recordproto" ,recordproto)
       ("pkg-config" ,pkg-config)
       ("inputproto" ,inputproto)
       ("libxi" ,libxi)
       ("libxtst" ,libxtst)
       ("xextproto" ,xextproto)
       ("libxext" ,libxext)
       ("xproto" ,xproto)
       ("libx11" ,libx11)))
    (home-page "http://www.gnu.org/software/xnee/")
    (synopsis "Record, replay and distribute user actions under X11")
    (description
     "GNU Xnee is a suite of programs that can record, replay and distribute
user actions under the X11 environment.  Think of it as a robot that can
imitate the job you just did.  Xnee can be used to automate tests, demonstrate
programs, distribute actions, record & replay \"macros\", retype a file.")
    (license gpl3+)))
