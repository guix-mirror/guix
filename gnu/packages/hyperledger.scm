;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages hyperledger)
  #:use-module (ice-9 match)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system go)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web))

(define-public hyperledger-fabric
  (package
    (name "hyperledger-fabric")
    (version "1.4.0")
    ;; While the GitHub repository is supposed to be "just a mirror," the Go
    ;; imports refer to it explicitly.
    (home-page "https://github.com/hyperledger/fabric")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    ;; ‘release-…’ are branches, and move. ‘v…’ are the tags.
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0nmg24ishwddxm1i2vh5ah5ylmmcg0apnjbgv1hljvhl48k4pzxq"))
              (file-name (git-file-name name version))))
    (build-system go-build-system)
    (native-inputs
     (list which docker-cli git curl))
    (arguments
     `(#:import-path "github.com/hyperledger/fabric"
       #:unpack-path "github.com/hyperledger/fabric"
       ;; We don't need to install the source code for end-user applications.
       #:install-source? #f
       ;; TODO: Tests require a running Docker daemon.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             ;; Only linux-amd64 and linux-ppc64le seem to be supported at the moment.
             (invoke "make"
                     "-j" (number->string (parallel-job-count))
                     "-C" "src/github.com/hyperledger/fabric"
                     "release/linux-amd64")))
         (add-after 'install 'install-commands
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (src "src/github.com/hyperledger/fabric/"))
               (with-directory-excursion src
                 (copy-recursively
                  "release/linux-amd64/bin"
                  (string-append out "/bin"))
                 (install-file "LICENSE"
                               (string-append out "/share/licenses"))
                 (install-file "README.md"
                               (string-append out "/share/doc"))
                 (copy-recursively "sampleconfig"
                                   (string-append out "/etc/hyperledger/fabric"))))
             #t)))))
    (supported-systems '("x86_64-linux"))
    (synopsis "Platform for distributed ledger solutions")
    (description "Hyperledger Fabric is a platform for distributed ledger
solutions, underpinned by a modular architecture focusing on confidentiality
and resiliency.  It is designed to support pluggable implementations of
different components.")
    (license asl2.0)))

(define-public hyperledger-iroha-ed25519
  (package
    (name "hyperledger-iroha-ed25519")
    (version "2.0.2")
    (home-page "https://github.com/hyperledger/iroha-ed25519")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kr1zwah8mhnpfrpk3h6hdafyqdl3ixhs7czdfscqv6vxqfiabc4"))))
    (build-system cmake-build-system)
    (native-inputs
     (list googletest))
    (arguments
     `(#:tests? #f      ; Tests don't build because CMake cannot find GTest main.
       #:configure-flags '("-DHUNTER_ENABLED=OFF"
                           "-DBUILD=SHARED"
                           ;; TODO: x86_64 should use amd64-64-24k-pic but it
                           ;; fails to link when built as a shared library.
                           "-DEDIMPL=ref10"
                           "-DHASH=sha3_brainhub")))
    (synopsis "Ed25519 digital signature algorithm")
    (description "This repository aims to provide modularized implementation
of the Ed25519 digital signature algorithm which is is described in
RFC8032 (@url{https://tools.ietf.org/html/rfc8032}).

Originally Ed25519 consists of three modules:

@itemize
@item digital signature algorithm itself
@item SHA512 hash function
@item random number generator, to generate key pairs
@end itemize

This project offers at least two different C implementations for every
module.  Every implementation can be replaced with another one at
link-time.  New implementations can be added as well.")
    (license asl2.0)))

(define-public hyperledger-iroha
  (package
    (name "hyperledger-iroha")
    (version "1.1.1")
    (home-page "https://github.com/hyperledger/iroha")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url home-page)
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "014mbwq059yxwihw0mq8zgns53fsw8ckczi1lw8q9pz3pk86pa9b"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; https://github.com/hyperledger/iroha/commit/4dc710d2e9a067af866771318f673c7392797e48
                  ;; Backport unversioned fmt dependency, remove next update:
                  (substitute* "libs/logger/logger.hpp"
                    (("fmt::v5") "fmt"))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DTESTING=OFF"    ; The tests fail to link correctly to googletest.
         ;; Don't install the shared libraries of the dependencies:
         "-DENABLE_LIBS_PACKAGING=OFF")
       #:tests? #f
       ;; https://iroha.readthedocs.io/en/latest/build/index.html#running-tests-optional
       #:test-target "."))
    ;; https://github.com/hyperledger/iroha/blob/master/vcpkg/VCPKG_DEPS_LIST
    (native-inputs
     (list fmt googletest rapidjson rxcpp spdlog))
    (inputs
     (list boost
           gflags
           grpc-1.16.1
           hyperledger-iroha-ed25519
           postgresql
           protobuf
           soci
           tbb))
    (synopsis "Simple, decentralized ledger")
    (description "Iroha is a distributed ledger technology (DLT).  Iroha has
essential functionality for your asset, information and identity management
needs, at the same time being a crash fault-tolerant tool.

Iroha has the following features:

@itemize
@item Creation and management of custom fungible assets, such as currencies,
kilos of gold, etc.
@item Management of user accounts
@item Taxonomy of accounts based on domains in the system
@item The system of rights and verification of user permissions for the
execution of transactions and queries in the system
@item Validation of business rules for transactions and queries in the system
@item Multisignature transactions
@end itemize\n")
    (license asl2.0)))
