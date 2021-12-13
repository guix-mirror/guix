;;; GNU Guix --- Functional package management for GNU
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
    (native-inputs (list git))
    (home-page "https://github.com/RadeonOpenCompute/rocm-cmake")
    (synopsis "ROCm cmake modules")
    (description "ROCm cmake modules provides cmake modules for common build
tasks needed for the ROCM software stack.")
    (license license:ncsa)))

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
    (inputs (list numactl))
    (home-page "https://github.com/RadeonOpenCompute/ROCT-Thunk-Interface")
    (synopsis "Radeon Open Compute Thunk Interface")
    (description "User-mode API interfaces used to interact with the ROCk
driver.")
    (license license:ncsa)))

(define-public rocr-runtime
  (package
    (name "rocr-runtime")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/ROCR-Runtime.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jqfqf5ymwlbpac065bhigmkgsk7mbyimdgvca7ymn38wpf80ka7"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       `(,(string-append
           "-DBITCODE_DIR="
           (assoc-ref %build-inputs "rocm-device-libs")
           "/amdgcn/bitcode/"))
       #:tests? #f ; No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "src"))))))
    (inputs
     `(("libelf" ,libelf)
       ("numactl" ,numactl)
       ("llvm" ,llvm-for-rocm)
       ("roct-thunk-interface" ,roct-thunk-interface)
       ("rocm-device-libs" ,rocm-device-libs))) ; For bitcode.
    (native-inputs (list xxd))
    (home-page "https://github.com/RadeonOpenCompute/ROCR-Runtime")
    (synopsis "ROCm Platform Runtime")
    (description "User-mode API interfaces and libraries necessary for host
applications to launch compute kernels to available HSA ROCm kernel agents.")
    (license license:ncsa)))

(define-public rocclr
  (package
    (name "rocclr")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ROCm-Developer-Tools/ROCclr.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pm1y020zriz7zmi95w0rcpka0jrsc7wwh81sssnysi8wxk3nnfy"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; No tests.
       #:configure-flags
       `(,(string-append
           "-DOPENCL_DIR="
           (assoc-ref %build-inputs "rocm-opencl-runtime-src")))))
    (inputs
     `(("mesa" ,mesa)
       ("rocm-comgr" ,rocm-comgr)
       ("llvm" ,llvm-for-rocm)
       ("rocm-device-libs" ,rocm-device-libs)
       ("rocr-runtime" ,rocr-runtime)
       ("rocm-cmake" ,rocm-cmake)
       ;; rocclr depends on a few headers provided by rocm-opencl-runtime.
       ("rocm-opencl-runtime-src"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/RadeonOpenCompute/ROCm-OpenCL-Runtime.git")
                 (commit (string-append "rocm-" version))))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "1cglpiaj3ny1z74ssmy6j63vj92sfy4q38ix6qsga0mg3b2wvqz3"))))))
    (home-page "https://github.com/ROCm-Developer-Tools/ROCclr")
    (synopsis "Radeon Open Compute Common Language Runtime")
    (description "ROCclr is a virtual device interface that compute runtimes
interact with to different backends such as ROCr or PAL.  This abstraction
allows runtimes to work on Windows as well as on Linux without much effort.")
    (license license:ncsa)))

(define-public rocm-opencl-runtime
  (package
    (name "rocm-opencl-runtime")
    (version %rocm-version)
    (home-page "https://github.com/RadeonOpenCompute/ROCm-OpenCL-Runtime")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cglpiaj3ny1z74ssmy6j63vj92sfy4q38ix6qsga0mg3b2wvqz3"))
              (patches
               (search-patches
                "rocm-opencl-runtime-3.10.0-includes.patch"
                ;; Do not install libOpenCL, which ocl-icd provides.
                "rocm-opencl-runtime-4.3-noopencl.patch"
                ;; Guix includes a program clinfo already.
                "rocm-opencl-runtime-4.3-noclinfo.patch"
                ;; cltrace linking fails, remove it.
                "rocm-opencl-runtime-4.3-nocltrace.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; Not sure how to run them.
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'create-icd
           ;; Manually install ICD, which simply consists of dumping
           ;; the path of the .so into the correct file.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (vendors (string-append out "/etc/OpenCL/vendors"))
                    (sopath (string-append out "/lib/libamdocl64.so")))
               (mkdir-p vendors)
               (with-output-to-file (string-append vendors "/amdocl64.icd")
                 (lambda _ (display sopath)))))))))
    (inputs
     (list mesa
           rocm-comgr
           rocr-runtime
           rocclr
           opencl-icd-loader
           glew))
    (native-inputs `())
    (synopsis "ROCm OpenCL Runtime")
    (description "OpenCL 2.0 compatible language runtime, supporting offline
and in-process/in-memory compilation.")
    (license license:ncsa)))

(define-public rocminfo
  (package
    (name "rocminfo")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/rocminfo.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pcm308vwkjrwnrk507iya20mkil8j0vx699w9jk2gas4n4jvkcz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-binary-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "rocminfo.cc"
               (("lsmod")
                (string-append (assoc-ref inputs "kmod") "/bin/lsmod"))
               (("grep") (which "grep"))))))))
    (inputs
     (list rocr-runtime kmod))
    (home-page "https://github.com/RadeonOpenCompute/rocminfo")
    (synopsis "ROCm Application for Reporting System Info")
    (description "List @acronym{HSA,Heterogeneous System Architecture} Agents
available to ROCm and show their properties.")
    (license license:ncsa)))

(define-public rocm-bandwidth-test
  (package
    (name "rocm-bandwidth-test")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/rocm_bandwidth_test.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a14kwkjpiyljgzxblh031qibn6xgbxp6m12zdy1pmwb2c44jjmm"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; No tests.
    (inputs (list rocr-runtime))
    (home-page "https://github.com/RadeonOpenCompute/rocm_bandwidth_test")
    (synopsis "Bandwidth test for ROCm")
    (description "RocBandwidthTest is designed to capture the performance
characteristics of buffer copying and kernel read/write operations. The help
screen of the benchmark shows various options one can use in initiating
cop/read/writer operations.  In addition one can also query the topology of the
system in terms of memory pools and their agents.")
    (license license:ncsa)))

