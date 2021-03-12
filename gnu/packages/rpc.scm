;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages tls))

(define-public grpc
  (package
    (name "grpc")
    (version "1.34.0")
    (outputs '("out" "static"))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grpc/grpc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fs407hnlnm0b8sncjwys9rc7ia5nb7wxrpx39nq3pzzfs1lv3vq"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no test target
       #:configure-flags
       (list "-DgRPC_ZLIB_PROVIDER=package"
             "-DgRPC_ABSL_PROVIDER=package"
             "-DgRPC_CARES_PROVIDER=package"
             "-DgRPC_SSL_PROVIDER=package"
             "-DgRPC_PROTOBUF_PROVIDER=package"
             "-DgRPC_RE2_PROVIDER=package"
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
     `(("abseil-cpp" ,abseil-cpp)
       ("c-ares" ,c-ares/cmake)
       ("openssl" ,openssl)
       ("re2" ,re2)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("protobuf" ,protobuf)
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

;; Some packages require this older version.
(define-public grpc-1.16.1
  (package
    (inherit grpc)
    (version "1.16.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grpc/grpc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "grpc" version))
              (sha256
               (base32
                "1jimqz3115f9pli5w6ik9wi7mjc7ix6y7yrq4a1ab9fc3dalj7p2"))))
    (arguments
     (substitute-keyword-arguments (package-arguments grpc)
       ((#:phases phases)
        `(modify-phases ,phases
           ;; Note: This would be nicer as a snippet, but that creates a tarball
           ;; instead of a checkout and breaks assumptions made by the builder.
           (add-after 'unpack 'rename-gettid
             (lambda _
               ;; Rename custom gettid() syscall wrapper to avoid conflict
               ;; with gettid() from glibc 2.30.
               (substitute* '("src/core/lib/gpr/log_linux.cc"
                              "src/core/lib/gpr/log_posix.cc"
                              "src/core/lib/iomgr/ev_epollex_linux.cc")
                 (("gettid\\(")
                  "sys_gettid("))
               #t))))))))

(define-public python-grpcio
  (package
    (name "python-grpcio")
    (version "1.27.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grpcio" version))
       (sha256
        (base32
         "0zl89jwcff9hkd8mi4yf3qbhns9vbv1s4x4vahm5mkpr7jwk5ras"))
       (modules '((guix build utils) (ice-9 ftw)))
       (snippet
        '(begin
           (with-directory-excursion "third_party"
             ;; Delete the bundled source code of libraries that are possible
             ;; to provide as inputs.
             (for-each delete-file-recursively
                       (scandir "."
                                (lambda (file)
                                  (not (member file
                                               '("." ".."
                                                 "abseil-cpp"
                                                 "address_sorting"
                                                 "upb")))))))
           #t))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'use-system-libraries
                    (lambda _
                      (setenv "GRPC_PYTHON_BUILD_SYSTEM_CARES" "1")
                      (setenv "GRPC_PYTHON_BUILD_SYSTEM_OPENSSL" "1")
                      (setenv "GRPC_PYTHON_BUILD_SYSTEM_ZLIB" "1")
                      #t))
                  (add-before 'build 'configure-compiler
                    (lambda _
                      (substitute* '("setup.py" "src/python/grpcio/commands.py")
                        (("'cc'") "'gcc'"))
                      #t)))))
    (inputs
     `(("c-ares" ,c-ares)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://grpc.io")
    (synopsis "HTTP/2-based RPC framework")
    (description "This package provides a Python library for communicating
with the HTTP/2-based RPC framework gRPC.")
    (license license:asl2.0)))

(define-public apache-thrift
  (package
    (name "apache-thrift")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/thrift")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mj3c5jhjbg2sfkx9k9jqg88h1c5rswr9b606s02ny9ydf3akcny"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags
       (list (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost")))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("flex" ,flex)
       ("bison" ,bison)))
    (inputs
     `(("boost" ,boost)
       ("libressl" ,libressl)))
    (outputs '("out" "lib" "include"))
    (home-page "https://thrift.apache.org/")
    (synopsis
     "Lightweight, language-independent software stack for point-to-point
RPC")
    (description
     "Thrift provides clean abstractions and implementations for data
transport, data serialization, and application level processing.  The code
generation system takes a simple definition language as input and generates
code across programming languages that uses the abstracted stack to build
interoperable RPC clients and servers.")
    (license license:asl2.0)))
