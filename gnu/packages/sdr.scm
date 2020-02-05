;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Christopher Howard <christopher@librehacker.com>
;;; Copyright © 2019, 2020 Evan Straw <evan.straw99@gmail.com>
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

(define-module (gnu packages sdr)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools))

(define-public liquid-dsp
  (package
    (name "liquid-dsp")
    (version "1.3.2")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/jgaeddert/liquid-dsp.git")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32 "1n6dbg13q8ga5qhg1yiszwly4jj0rxqr6f1xwm9waaly5z493xsd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)))
    (inputs
     `(("fftw" ,fftw)
       ("fftwf" ,fftwf)))
    (home-page "https://liquidsdr.org")
    (synopsis "Signal processing library for software-defined radios")
    (description
     "Liquid DSP is a @dfn{digital signal processing} (DSP) library designed
specifically for software-defined radios on embedded platforms.  The aim is to
provide a lightweight DSP library that does not rely on a myriad of external
dependencies or proprietary and otherwise cumbersome frameworks.  All signal
processing elements are designed to be flexible, scalable, and dynamic,
including filters, filter design, oscillators, modems, synchronizers, complex
mathematical operations, and much more.")
    (license license:expat)))
