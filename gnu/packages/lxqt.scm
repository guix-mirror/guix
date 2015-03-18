;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
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

(define-module (gnu packages lxqt)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:select (lgpl2.1+))
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt))

(define-public libqtxdg
  (package
    (name "libqtxdg")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
         (string-append "https://github.com/lxde/" name "/releases/download/"
                        version "/" name "-" version ".tar.xz"))
       (sha256
        (base32
         "00j0zzb8zn714lv77fawahlalxjznvh06nlhlz47qf0krngri42w"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; handled by configure flag instead
       #:configure-flags '(;; FIXME: Currently, the tests fail; this is a
                           ;; known issue, see
                           ;; https://github.com/lxde/libqtxdg/issues/42
                           ;; Enable in new release.
                           "-DBUILD_TESTS=OFF")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("qt" ,qt))) ; according to Qt5Xdg.pc
    (home-page "https://github.com/lxde/libqtxdg")
    (synopsis "Qt implementation of freedesktop.org xdg specifications")
    (description "Libqtxdg implements the freedesktop.org xdg specifications
in Qt.")
    (license lgpl2.1+)))
