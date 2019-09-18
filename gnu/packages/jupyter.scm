;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages jupyter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time))

(define-public python-jupyter-protocol
  (package
    (name "python-jupyter-protocol")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyter_protocol" version))
              (sha256
               (base32
                "1bk3as5yw9y5nmq6l15nr46aby34phmvsx9kxgqnm5pd5q2b5h57"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-pyzmq" ,python-pyzmq)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipython" ,python-ipython)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter protocol implementation")
    (description
     "This Python library is an experimental implementation of the
@uref{https://jupyter-client.readthedocs.io/en/latest/messaging.html, Jupyter
protocol} to be used by both clients and kernels.")
    (license license:bsd-3)
    (properties '((upstream-name . "jupyter_protocol")))))

(define-public python-jupyter-kernel-mgmt
  (package
    (name "python-jupyter-kernel-mgmt")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyter_kernel_mgmt" version))
              (sha256
               (base32
                "0i7a78dn89ca8h0a42giyxwcmk6y4wrdr7q8h2ax9vybb84c795q"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-entrypoints" ,python-entrypoints)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-jupyter-protocol" ,python-jupyter-protocol)
       ("python-pyzmq" ,python-pyzmq)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipython" ,python-ipython)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://jupyter.org")
    (synopsis "Discover, launch, and communicate with Jupyter kernels")
    (description
     "This package is an experimental refactoring of the machinery for
launching and using Jupyter kernels.")
    (license license:bsd-3)
    (properties '((upstream-name . "jupyter_kernel_mgmt")))))
