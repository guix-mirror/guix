;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Quiliro <quiliro@fsfla.org>
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

(define-module (gnu packages medical)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt))

(define-public openmolar-1
  (package
   (name "openmolar")
   (version "1.0.15-gd81f9e5")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://static.openmolar.com/om1/releases/openmolar-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1cfdzfbi6wslw7k0dc6ad6xrgs75iwsl91cg73w4myswaqqkfk3z"))))
   (build-system python-build-system)
   (arguments
    `(#:use-setuptools? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-/usr
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "setup.py"
              (("/usr") (assoc-ref outputs "out")))
            #t)))))
   (inputs
    (list python-pyqt+qscintilla python-mysqlclient qscintilla))
   (home-page "https://openmolar.com/om1")
   (synopsis "Dental practice management software")
   (description "Openmolar is a dental practice management suite.  Its
functionality includes appointments, patient records, treatment planning,
billing etc.  It is a full featured, reliable and thoroughly tested
application and has been translated into many languages.")
   (license gpl3+)))
