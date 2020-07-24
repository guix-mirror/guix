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
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls))

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

(define-public python-jupyter-kernel-test
  (package
    (name "python-jupyter-kernel-test")
    (version "0.3")
    (home-page "https://github.com/jupyter/jupyter_kernel_test")
    (source (origin
              ;; PyPI has a ".whl" file but not a proper source release.
              ;; Thus, fetch code from Git.
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00iy74i4i8is6axb9vlsm0b9wxkvyyxnbl8r0i4gaj3xd788jm83"))))
    (build-system python-build-system)
    (arguments
     ;; The repo doesn't contain a "setup.py" file so install files manually.
     '(#:phases (modify-phases %standard-phases
                  (delete 'build)
                  (delete 'check)
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (version (python-version (assoc-ref inputs "python")))
                             (pydir (string-append out "/lib/python"
                                                   version "/site-packages/"
                                                   "jupyter_kernel_test")))
                        (for-each (lambda (file)
                                    (install-file file pydir))
                                  (find-files "jupyter_kernel_test"
                                              "\\.py$"))
                        #t))))))
    (propagated-inputs
     `(("python-jupyter-kernel-mgmt" ,python-jupyter-kernel-mgmt)
       ("python-jupyter-protocol" ,python-jupyter-protocol)
       ("python-jsonschema" ,python-jsonschema)))
    (synopsis "Test Jupyter kernels")
    (description
     "@code{jupyter_kernel_test} is a tool for testing Jupyter kernels.  It
tests kernels for successful code execution and conformance with the
@uref{https://jupyter-client.readthedocs.io/en/latest/messaging.html, Jupyter
Messaging Protocol}.")
    (license license:bsd-3)))

(define-public xeus
  (package
    (name "xeus")
    (version "0.23.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/QuantStack/xeus")
                    (commit version)))
              (sha256
               (base32
                "1m1b6z1538r7mv2ggn7bdbd9570ja7cadplq64zl8rgl2c8vdi2a"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_STATIC_LIBS=OFF"
                           "-DDISABLE_ARCH_NATIVE=ON" ;no '-march=native'
                           "-DBUILD_TESTING=ON")))
    (native-inputs
     `(("pkg-config" ,pkg-config)

       ;; The following inputs are used by the test suite.
       ("googletest" ,googletest)
       ("python-pytest" ,python-pytest)
       ("python" ,python-3)
       ("python-jupyter-kernel-test" ,python-jupyter-kernel-test)
       ("python-jupyter-client" ,python-jupyter-client)))
    (inputs
     `(("xtl" ,xtl)
       ("json-modern-cxx" ,json-modern-cxx)
       ("cppzmq" ,cppzmq)
       ("zeromq" ,zeromq)
       ("openssl" ,openssl)
       ("util-linux" ,util-linux "lib")))         ;libuuid
    (home-page "https://quantstack.net/xeus")
    (synopsis "C++ implementation of the Jupyter Kernel protocol")
    (description
     "@code{xeus} is a library meant to facilitate the implementation of
kernels for Jupyter.  It takes the burden of implementing the Jupyter Kernel
protocol so developers can focus on implementing the interpreter part of the
kernel.

Several Jupyter kernels are built upon @code{xeus}, such as @code{xeus-cling},
a kernel for the C++ programming language, and @code{xeus-python}, an
alternative Python kernel for Jupyter.")
    (license license:bsd-3)))
