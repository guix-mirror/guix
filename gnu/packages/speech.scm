;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages speech)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages textutils))

(define-public mitlm
  (package
    (name "mitlm")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mitlm/mitlm/releases/"
                                  "download/v" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "09fv4fcpmw9g1j0zml0k5kk1lgjw2spr8gn51llbkaaph6v8d62a"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (synopsis "The MIT Language Modeling toolkit")
    (description "The MIT Language Modeling (MITLM) toolkit is a set of
tools designed for the efficient estimation of statistical n-gram language
models involving iterative parameter estimation.  It achieves much of its
efficiency through the use of a compact vector representation of n-grams.")
    (home-page "https://github.com/mitlm/mitlm")
    (license license:expat)))

(define-public speech-dispatcher
  (package
    (name "speech-dispatcher")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/brailcom/speechd/releases"
                                  "/download/" version "/speech-dispatcher-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1yd2rb02565picga4gh2a0bvfxbhdyaj0cv9aaj5a8fc5zs29fbk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static"
                           ;; Disable support for proprietary TTS engines.
                           "--with-kali=no" "--with-baratinoo=no")))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dotconf" ,dotconf)
       ("espeak" ,espeak)
       ("glib" ,glib)
       ("libltdl" ,libltdl)
       ("libsndfile" ,libsndfile)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python)))
    (synopsis "Common interface to speech synthesizers")
    (description "The Speech Dispatcher project provides a high-level
device independent layer for access to speech synthesis through a simple,
stable and well documented interface.")
    (home-page "https://devel.freebsoft.org/speechd")
    ;; The software is distributed under GPL2+, but includes a number
    ;; of files covered by other licenses.
    (license (list license:gpl2+
                   license:fdl1.2+ ; Most files in doc/ are dual gpl2+/fdl1.2+.
                   license:lgpl2.1+
                   license:gpl2
                   (license:non-copyleft
                    ;; festival_client.{c,h} carries an expat-style license.
                    "See src/modules/festival_client.c in the distribution.")
                   license:gpl3+)))) ; doc/texinfo.tex -- with TeX exception.

(define-public sonic
  (package
    (name "sonic")
    (version "0.2.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/waywardgeek/sonic/archive/"
                                 "release-" version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "11a0q9wkgbb9ymf52v7dvybfhj8hprgr67zs1xcng143fvjpr0n7"))))
    (build-system gnu-build-system)
    (arguments
      `(#:tests? #f ; No test suite.
        #:make-flags
         (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
        #:phases
        (modify-phases %standard-phases
          (delete 'configure)))) ; No ./configure script.
    (synopsis "Speed up or slow down speech")
    (description "Sonic implements a simple algorithm for speeding up or slowing
down speech.  However, it's optimized for speed ups of over 2X, unlike previous
algorithms for changing speech rate.  Sonic is a C library designed to be easily
integrated into streaming voice applications such as text-to-speech (TTS) back
ends.

The primary motivation behind Sonic is to enable the blind and visually impaired
to improve their productivity with speech engines, like eSpeak.  Sonic can also
be used by the sighted.")
    (home-page "https://github.com/waywardgeek/sonic")
    (license license:asl2.0)))
