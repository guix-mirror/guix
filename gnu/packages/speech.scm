;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages textutils))

(define-public mitlm
  (package
    (name "mitlm")
    (version "0.4.1")
    ;; No official release tarballs, so for now we use the one from Debian
    ;; that is maintained by one of the project developers.
    ;;
    ;; See: https://github.com/mitlm/mitlm/issues/54
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/main/m/mitlm/mitlm_"
                                  version ".orig.tar.gz"))
              (sha256
               (base32
                "12m09xxx8jbir9cnzzaysvni5sfijpfj96z1x1520qqvmpc8lmn7"))))
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
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://devel.freebsoft.org/pub/"
                                  "projects/speechd/speech-dispatcher-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "18jlxnhlahyi6njc6l6576hfvmzivjjgfjyd2n7vvrvx9inphjrb"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dotconf" ,dotconf)
       ("glib" ,glib)
       ("libltdl" ,libltdl)
       ("libsndfile" ,libsndfile)))
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
