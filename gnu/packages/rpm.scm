;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages rpm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages tls))

(define-public drpm
  (package
    (name "drpm")
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rpm-software-management/drpm")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m269nl8s31yjyz7kknv4jl9mx12xjpx2ly6wf66zs5613m4rj1z"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("cmocka" ,cmocka)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bzip2" ,bzip2)
       ("lzlib" ,lzlib)
       ("openssl" ,openssl)
       ("popt" ,popt)
       ("rpm" ,rpm)
       ("xz" ,xz)
       ("zlib" ,zlib)
       ("zstd" ,zstd "lib")))
    (home-page "https://github.com/rpm-software-management/drpm")
    (synopsis "Delta RPM library")
    (description "This package provides a library for making, reading and
applying deltarpms, compatible with the original deltarpm packages.")
    (license license:lgpl2.1+)))
