;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages xnee)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))

(define-public xnee
  (package
    (name "xnee")
    (version "3.19")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/xnee/xnee-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "04n2lac0vgpv8zsn7nmb50hf3qb56pmj90dmwnivg09gyrf1x92j"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("CFLAGS=-O2 -g -fcommon")))
    (inputs
     (list gtk+-2
           libx11
           libxext
           libxi
           libxtst
           xorgproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.gnu.org/software/xnee/")
    (synopsis "Record, replay and distribute user actions under X11")
    (description
     "GNU Xnee is a program that can record, replay and distribute user
actions in X11.  It can be used to automate user interactions for testing or
demonstration purposes.")
    (license gpl3+)))
