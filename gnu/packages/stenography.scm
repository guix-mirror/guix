;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Christopher Baines <mail@cbaines.net>
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

(define-module (gnu packages stenography)
  #:use-module (guix build-system python)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages check)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages wxwidgets))

(define-public plover
  (package
    (name "plover")
    (version "4.0.0.dev8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openstenoproject/plover")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b2ys77bkjsdmyg97i7lq3lj45q56bycvsm06d4rs656kxhvc0a3"))))
    (build-system python-build-system)
    (native-inputs
     (list python-mock python-pytest python-setuptools-scm))
    (inputs
     (list python-appdirs
           python-pyqt
           python-babel
           python-dbus
           python-hidapi
           python-pyserial
           python-wxpython
           python-xlib))
    (home-page "https://www.openstenoproject.org/plover/")
    (synopsis "Stenography engine")
    (description
     "Plover (rhymes with @emph{lover}) is a desktop application that
allows anyone to use stenography to write on their computer, up to
speeds of 200WPM and beyond.")
    (license license:gpl2+)))

