;;;
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages rocm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim))

;; The components are tightly integrated and can only be upgraded as a unit. If
;; you want to upgrade ROCm, bump this version number and update hashes below.
(define %rocm-version "4.3.0")

(define-public rocm-cmake
  (package
    (name "rocm-cmake")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/rocm-cmake.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sic2zxmzl2pb2865vvq55mbpcr8pby8v19pjdlm08pypqw5h6h6"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; Tests try to use git commit
    (native-inputs `(("git" ,git)))
    (home-page "https://github.com/RadeonOpenCompute/rocm-cmake")
    (synopsis "ROCm cmake modules")
    (description "ROCm cmake modules provides cmake modules for common build
tasks needed for the ROCM software stack.")
    (license license:ncsa)))

(define-public llvm-for-rocm
  (hidden-package
   (package
     ;; Actually based on LLVM 13 as of v4.3, but llvm-12 works just fine.
     (inherit llvm-12)
     (name "llvm-for-rocm")
     (version %rocm-version)
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/RadeonOpenCompute/llvm-project.git")
                     (commit (string-append "rocm-" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0p75nr1qpmy6crymdax5hm40wkimman4lnglz4x5cnbiqindya7s"))
               (patches
                (search-patches "llvm-roc-4.2.0-add_Object.patch"
                                "llvm-roc-3.0.0-add_libraries.patch"
                                "llvm-roc-4.0.0-remove-isystem-usr-include.patch"))))
     (arguments
      (substitute-keyword-arguments (package-arguments llvm-12)
        ((#:phases phases '%standard-phases)
         `(modify-phases ,phases
            (add-after 'unpack 'chdir
              (lambda _
                (chdir "llvm")))))
        ((#:configure-flags flags)
         ''("-DLLVM_ENABLE_PROJECTS=llvm;clang;lld"
            "-DLLVM_TARGETS_TO_BUILD=AMDGPU;X86"
            "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
            "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
            "-DBUILD_SHARED_LIBS:BOOL=TRUE"
            "-DLLVM_VERSION_SUFFIX=")))))))

(define-public rocm-device-libs
  (package
    (name "rocm-device-libs")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/ROCm-Device-Libs.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f8xsylfajpxqjk6ayjnrry53y8b0a6lh9d72pd41nffxfyzvw3w"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
             "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE")))
    (inputs `(("llvm" ,llvm-for-rocm)))
    (home-page "https://github.com/RadeonOpenCompute/ROCm-Device-Libs")
    (synopsis "ROCm Device libraries")
    (description "AMD-specific device-side language runtime libraries, namely
oclc, ocml, ockl, opencl, hip and hc.")
    (license license:ncsa)))

(define-public rocm-comgr
  (package
    (name "rocm-comgr")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/ROCm-CompilerSupport.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bakbm7shr0l67lph44b5cnc9psd6rivg1mp79qizaawkn380x60"))
              (patches
               (search-patches "rocm-comgr-3.1.0-dependencies.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "lib/comgr"))))))
    (inputs
     `(("rocm-device-libs" ,rocm-device-libs)
       ("llvm" ,llvm-for-rocm)
       ("lld" ,lld)))
    (home-page "https://github.com/RadeonOpenCompute/ROCm-CompilerSupport")
    (synopsis "ROCm Code Object Manager")
    (description "The Comgr library provides APIs for compiling and inspecting
AMDGPU code objects.")
    (license license:ncsa)))

(define-public roct-thunk-interface
  (package
    (name "roct-thunk-interface")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/ROCT-Thunk-Interface.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ffqhrrscmcydfqf61dk58d7nnxk6n2k68jhqfj7a4hvhlphb74f"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; Not sure how to run tests.
    (inputs `(("numactl" ,numactl)))
    (home-page "https://github.com/RadeonOpenCompute/ROCT-Thunk-Interface")
    (synopsis "Radeon Open Compute Thunk Interface")
    (description "User-mode API interfaces used to interact with the ROCk
driver.")
    (license license:ncsa)))
