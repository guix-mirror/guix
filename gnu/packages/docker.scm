;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Thompson <davet@gnu.org>
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

(define-module (gnu packages docker)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages python))

(define-public python-docker-py
  (package
    (name "python-docker-py")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docker-py" version))
       (sha256
        (base32
         "16ba4xyd46hkj9nkfpz15r8kskl7ljx1afjzchyrhdsrklvzgzim"))))
    (build-system python-build-system)
    ;; TODO: Tests require a running Docker daemon.
    (arguments '(#:tests? #f))
    (inputs
     `(("python-requests" ,python-requests)
       ("python-setuptools" ,python-setuptools)
       ("python-six" ,python-six)
       ("python-websocket-client" ,python-websocket-client)))
    (home-page "https://github.com/docker/docker-py/")
    (synopsis "Python client for Docker")
    (description "Docker-Py is a Python client for the Docker container
management tool.")
    (license license:asl2.0)))
