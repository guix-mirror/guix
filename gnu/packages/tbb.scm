;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (gnu packages tbb)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages))

(define-public tbb
  (package
    (name "tbb")
    (version "2021.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/oneapi-src/oneTBB")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bz039my3ma87f24ngcsqs16f8jlpdgaqg01ab4g60nfqbrz1lkq"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DTBB_STRICT=OFF"))) ;; Don't fail on warnings
    (home-page "https://www.threadingbuildingblocks.org")
    (synopsis "C++ library for parallel programming")
    (description
     "Threading Building Blocks (TBB) is a C++ runtime library that abstracts
the low-level threading details necessary for optimal multi-core performance.
It uses common C++ templates and coding style to eliminate tedious threading
implementation work.  It provides parallel loop constructs, asynchronous
tasks, synchronization primitives, atomic operations, and more.")
    (license asl2.0)))
