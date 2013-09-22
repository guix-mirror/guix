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

(define-module (gnu packages gstreamer)
  #:use-module ((guix licenses) #:select (lgpl2.0+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public gstreamer
  (package
    (name "gstreamer")
    (version "1.0.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://gstreamer.freedesktop.org/src/gstreamer/gstreamer-"
                          version ".tar.xz"))
      (sha256
       (base32
        "0c0irk85jd2cihm5pmf4zxhlpg08qpxjcqv1l9qn2n3h2gsaj2lf"))))
    (build-system gnu-build-system)
    (inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("glib" ,glib)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python)))
    (home-page "http://gstreamer.freedesktop.org/")
    (synopsis
     "Multimedia library")
    (description
     "GStreamer is a library for constructing graphs of media-handling
components.  The applications it supports range from simple Ogg/Vorbis
playback, audio/video streaming to complex audio (mixing) and video
(non-linear editing) processing.

Applications can take advantage of advances in codec and filter technology
transparently.  Developers can add new codecs and filters by writing a
simple plugin with a clean, generic interface.

This package provides the core library and elements.")
    (license lgpl2.0+)))
