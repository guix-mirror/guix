;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages python-science)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system python))

(define-public python-scikit-image
  (package
    (name "python-scikit-image")
    (version "0.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit-image" version))
       (sha256
        (base32 "07qchljkyxvg5nrm12fvszi7pmjk4m01qp0w0z8syxzxxs20pz8s"))))
    (build-system python-build-system)
    (arguments
     ;; TODO: Some tests require running X11 server. Disable them?
     '(#:tests? #f))
    ;; See DEPENDS.txt for the list of build and run time requiremnts
    (propagated-inputs
     `(("python-cloudpickle" ,python-cloudpickle)
       ("python-dask" ,python-dask)
       ("python-matplotlib" ,python-matplotlib)
       ("python-networkx" ,python-networkx)
       ("python-numpy" ,python-numpy)
       ("python-pillow" ,python-pillow)
       ("python-pywavelets" ,python-pywavelets)
       ("python-scipy" ,python-scipy)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-cython" ,python-cython)))
    (home-page "http://scikit-image.org/")
    (synopsis "Image processing in Python")
    (description
     "Scikit-image is a collection of algorithms for image processing.")
    (license license:bsd-3)))

(define-public python2-scikit-image
  (package-with-python2 python-scikit-image))
