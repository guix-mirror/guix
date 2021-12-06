;;; Copyright © 2021 Blake Shaw <blake@nonconstructivism.com>
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

(define-module (gnu packages notcurses)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages video)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libunistring))

(define-public notcurses
  (package
   (name "notcurses")
   (version "3.0.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/dankamongmen/notcurses")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
         (sha256
          (base32 "1y9s77m1pp6syfml559d8dvif61y6zjldrdx1zri18q9sr0zqm9m"))))
      (build-system cmake-build-system)
      (arguments
       `(#:make-flags
         (list (string-append "CC=" ,(cc-for-target)))
         ;; These flags are documented in 'INSTALL.md' in the source distribution.
         #:configure-flags
         '(;; Do not build "coverage"
           "-DUSE_COVERAGE=off"
           ;; Do not build HTML documentation
           "-DUSE_DOXYGEN=off"
           ;; Don't include mouse support
           "-DUSE_GPM=off"
           ;; Use FFmpeg for multimedia support
           "-DUSE_MULTIMEDIA=ffmpeg"
           ;; Follow the Debian Free Software Guidelines, omitting nonfree content.
           "-DFSG_BUILD=ON")))
      (native-inputs
       (list pkg-config
             pandoc
             doctest))
      (inputs
       (list ffmpeg
             libdeflate
             libunistring
             ncurses
             zlib))
      (synopsis "Textual user interfaces")
      (description "Notcurses is a library for building complex textual user
interfaces on modern terminals.")
      (home-page "https://notcurses.com")
      (license license:asl2.0)))
