;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages rpc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls))

(define-public grpc
  (package
    (name "grpc")
    (version "1.16.1")
    (outputs '("out" "static"))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grpc/grpc.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jimqz3115f9pli5w6ik9wi7mjc7ix6y7yrq4a1ab9fc3dalj7p2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no test target
       #:configure-flags
       (list "-DgRPC_ZLIB_PROVIDER=package"
             "-DgRPC_CARES_PROVIDER=package"
             "-DgRPC_SSL_PROVIDER=package"
             "-DgRPC_PROTOBUF_PROVIDER=package"
             (string-append "-DCMAKE_INSTALL_PREFIX="
                            (assoc-ref %outputs "out"))
             "-DCMAKE_INSTALL_LIBDIR=lib"
             (string-append "-DCMAKE_INSTALL_RPATH="
                            (assoc-ref %outputs "out") "/lib")
             "-DCMAKE_VERBOSE_MAKEFILE=ON")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'configure-shared
           (lambda* (#:key (configure-flags '()) #:allow-other-keys)
             (mkdir "../build-shared")
             (with-directory-excursion "../build-shared"
               (apply invoke
                      "cmake" "../source"
                      "-DBUILD_SHARED_LIBS=ON"
                      configure-flags)
               (apply invoke "make"
                      `("-j" ,(number->string (parallel-job-count)))))))
         (add-after 'install 'install-shared-libraries
           (lambda _
             (with-directory-excursion "../build-shared"
               (invoke "make" "install"))))
         (add-before 'strip 'move-static-libs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (static (assoc-ref outputs "static")))
               (mkdir-p (string-append static "/lib"))
               (with-directory-excursion
                 (string-append out "/lib")
                 (for-each
                   (lambda (file)
                     (rename-file file
                                  (string-append static "/lib/" file)))
                   (find-files "." "\\.a$"))))
             #t)))))
    (inputs
     `(("c-ares" ,c-ares/cmake)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (native-inputs
     `(("protobuf" ,protobuf)
       ("python" ,python-wrapper)))
    (home-page "https://grpc.io")
    (synopsis "High performance universal RPC framework")
    (description "gRPC is a modern high performance @dfn{Remote Procedure Call}
(RPC) framework that can run in any environment.  It can efficiently connect
services in and across data centers with pluggable support for load balancing,
tracing, health checking and authentication.  It is also applicable in last
mile of distributed computing to connect devices, mobile applications and
browsers to backend services.")
    (license license:asl2.0)))

(define-public python-grpcio
  (package
    (name "python-grpcio")
    (version "1.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grpcio" version))
       (sha256
        (base32
         "0qb9y6j83nxa6d4kc60i8yfgdm7a8ms7b54kncjzf5y7nsxp8rzx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://grpc.io")
    (synopsis "HTTP/2-based RPC framework")
    (description "This package provides a Python library for communicating
with the HTTP/2-based RPC framework gRPC.")
    (license license:asl2.0)))
