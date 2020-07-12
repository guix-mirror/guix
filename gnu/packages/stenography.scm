;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages wxwidgets))

(define-public plover
  (package
    (name "plover")
    (version "3.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openstenoproject/plover")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "114rlxvq471fyifwcdcgdad79ak7q3w2lk8z9nqhz1i9fg05721c"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (native-inputs
     `(("python2-mock" ,python2-mock)
       ("python2-pytest" ,python2-pytest)
       ("python2-setuptools-scm" ,python2-setuptools-scm)))
    (inputs
     `(("python2-appdirs" ,python2-appdirs)
       ("python2-dbus" ,python2-dbus)
       ("python2-hidapi" ,python2-hidapi)
       ("python2-pyserial" ,python2-pyserial)
       ("python2-wxpython" ,python2-wxpython)
       ("python2-xlib" ,python2-xlib)))
    (home-page "https://www.openstenoproject.org/plover/")
    (synopsis "Stenography engine")
    (description
     "Plover (rhymes with @emph{lover}) is a desktop application that
allows anyone to use stenography to write on their computer, up to
speeds of 200WPM and beyond.")
    (license license:gpl2+)))

