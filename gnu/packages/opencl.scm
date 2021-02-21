;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages opencl)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

;; This file adds OpenCL implementation related packages. Due to the fact that
;; OpenCL devices like GPU are not available during build (store environment),
;; tests that require such devices are all disabled.
;; Check https://lists.gnu.org/archive/html/guix-devel/2018-04/msg00293.html

(define-public opencl-headers
  (package
    (name "opencl-headers")
    (version "2020.12.18")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/KhronosGroup/OpenCL-Headers")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1zslxfzvbb1mrzmsik4pjzj88ds8y3p94ry5nnrnkhv6qm4n4lw9"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("CL" "include/")))) ; TODO: add phase for tests
    (synopsis "The Khronos OpenCL headers")
    (description
     "This package provides the C headers by Khronos for OpenCL programming.")
    (home-page "https://www.khronos.org/registry/OpenCL/")
    (license license:asl2.0)))

(define (make-opencl-headers major-version subversion)
  ;; The upstream OpenCL-Headers repository is no longer separating headers by
  ;; version; instead, you are supposed to #define CL_TARGET_OPENCL_VERSION.
  (deprecated-package (string-append "opencl-headers-"
                                     major-version "."
                                     subversion) opencl-headers))

(define-public opencl-headers-2.2
  (make-opencl-headers "2" "2"))
(define-public opencl-headers-2.1
  (make-opencl-headers "2" "1"))
(define-public opencl-headers-2.0
  (make-opencl-headers "2" "0"))
(define-public opencl-headers-1.2
  (make-opencl-headers "1" "2"))
(define-public opencl-headers-1.1
  (make-opencl-headers "1" "1"))
(define-public opencl-headers-1.0
  (make-opencl-headers "1" "0"))

(define-public opencl-clhpp
  (package
    (name "opencl-clhpp")
    (version "2.0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/OpenCL-CLHPP")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0h5kpg5cl8wzfnqmv6i26aig2apv06ffm9p3rh35938n9r8rladm"))
       (file-name (git-file-name name version))))
    (native-inputs
     `(("python" ,python-wrapper)))
    (propagated-inputs
     `(("opencl-headers" ,opencl-headers)))
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list
          "-DBUILD_EXAMPLES=OFF"
          "-DBUILD_TESTS=OFF"
          (string-append "-DCMAKE_INSTALL_PREFIX="
                         (assoc-ref %outputs "out")
                         "/include")))
       ;; The regression tests require a lot more dependencies.
       #:tests? #f))
    (build-system cmake-build-system)
    (home-page "https://github.khronos.org/OpenCL-CLHPP/")
    (synopsis "Khronos OpenCL-CLHPP")
    (description
     "This package provides the @dfn{host API} C++ headers for OpenCL.")
    (license license:expat)))

(define-public ocl-icd
  (package
    (name "ocl-icd")
    (version "2.2.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://forge.imag.fr/frs/download.php/836/ocl-icd-"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1x2dr8p4dkfds56r38av360i3nv1y3326jmshxvjngaf6mlg6rbn"))
              (modules '((guix build utils)))
              (snippet
               '(delete-file-recursively "khronos-headers"))))
    (native-inputs
     `(("opencl-headers" ,opencl-headers)
       ("ruby" ,ruby)))
    (inputs
     `(("libgcrypt" ,libgcrypt)))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("DEBUG_OCL_ICD=1")))
    (native-search-paths
     (list (search-path-specification
            (variable "OPENCL_VENDOR_PATH")
            (files '("etc/OpenCL/vendors")))))
    (search-paths native-search-paths)
    (home-page "https://forge.imag.fr/projects/ocl-icd/")
    (synopsis "OpenCL loader for Installable Client Drivers (ICDs)")
    (description
     "OpenCL implementations are provided as ICDs (Installable Client
Drivers).  An OpenCL program can use several ICDs thanks to the use of an ICD
Loader as provided by this package.")
    (license license:bsd-2)))

(define-public clinfo
  (package
    (name "clinfo")
    (version "2.2.18.04.06")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Oblomov/clinfo")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y2q0lz5yzxy970b7w7340vp4fl25vndahsyvvrywcrn51ipgplx"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("opencl-headers" ,opencl-headers)))
    (inputs
     `(("ocl-icd" ,ocl-icd)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (let ((cores (number->string (parallel-job-count))))
               (setenv "CC" "gcc")
               (invoke "make" "-j" cores))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "install" (string-append
                                       "PREFIX="
                                       (assoc-ref outputs "out"))))))
       #:tests? #f))
    (home-page "https://github.com/Oblomov/clinfo")
    (synopsis "Print information about OpenCL platforms and devices")
    ;; Only the implementation installed via Guix will be detected.
    (description
     "This package provides the @command{clinfo} command that enumerates all
possible (known) properties of the OpenCL platform and devices available on
the system.")
    (license license:cc0)))

(define-public beignet
  (package
    (name "beignet")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/beignet")
             (commit (string-append "Release_v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lpv3lvi2vrmzb8blflrpbd3jgin76zqmz6jcv17vn9mylqdrfnd"))
       (patches (search-patches "beignet-correct-file-names.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; There's a suspicious .isa binary file under kernels/.
        ;; Remove it.
        '(for-each delete-file (find-files "." "\\.isa$")))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("python" ,python)))
    (inputs `(("clang@3.7" ,clang-3.7)
              ("clang-runtime@3.7" ,clang-runtime-3.7)
              ("glu" ,glu)
              ("llvm@3.7" ,llvm-3.7)
              ("libdrm" ,libdrm)
              ("libedit" ,libedit)
              ("libpthread-stubs" ,libpthread-stubs)
              ("libsm" ,libsm)
              ("libva" ,libva)
              ("libxfixes" ,libxfixes)
              ("libxext" ,libxext)
              ("mesa-utils" ,mesa-utils)
              ("ncurses" ,ncurses)
              ("ocl-icd" ,ocl-icd)
              ("opencl-headers" ,opencl-headers)
              ("xextproto" ,xextproto)
              ("zlib" ,zlib)))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCLANG_LIBRARY_DIR="
                            (assoc-ref %build-inputs "clang@3.7") "/lib")
             "-DENABLE_GL_SHARING=ON"
             "-DEXPERIMENTAL_DOUBLE=ON")

       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (delete-file-recursively
                (string-append out "/include"))
               #t)))
         (add-after 'remove-headers 'install-kernels
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (builddir (getcwd))
                    (source-dir (string-append
                                 builddir
                                 "/../beignet-Release_v1.3.2/kernels")))
               (copy-recursively source-dir
                                 (string-append out "/lib/beignet/kernels"))
               #t))))
       ;; Beignet tries to find GPU when running tests, which is not available
       ;; during build.
       #:tests? #f))
    (home-page "https://wiki.freedesktop.org/www/Software/Beignet/")
    (synopsis "OpenCL framework for Intel GPUs")
    (description
     "Beignet is an implementation of the OpenCL specification.  This code
base contains the code to run OpenCL programs on Intel GPUs---IvyBridge,
Haswell, Skylake, Apollolake, etc.  It defines and implements the OpenCL host
functions required to initialize the device, create the command queues, the
kernels and the programs, and run them on the GPU.  The code also contains a
back-end for the LLVM compiler framework.")
    ;; Beignet only supports Intel processors.
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:lgpl2.1+)))

(define-public pocl
  (package
    (name "pocl")
    (version "1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pocl/pocl")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1c4y69zks6hkq5fqh9waxgb8g4ka7y6h3vacmsm720kba0h57g8a"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     `(("libltdl" ,libltdl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("clang" ,clang)
       ("hwloc" ,hwloc-2 "lib")
       ("llvm" ,llvm)
       ("ocl-icd" ,ocl-icd)))
    (arguments
     `(#:configure-flags
       (list "-DENABLE_ICD=ON"
             "-DENABLE_TESTSUITES=ON"
             ;; We are not developers, don't run conformance suite.
             "-DENABLE_CONFORMANCE=OFF"
             (string-append "-DEXTRA_HOST_LD_FLAGS=-L"
                            (assoc-ref %build-inputs "libc") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-HOME
           (lambda _
             (setenv "HOME" "/tmp")
             #t)))))
    (home-page "http://portablecl.org/")
    (synopsis "Portable Computing Language (pocl), an OpenCL implementation")
    (description
     "Pocl is a portable implementation of the OpenCL standard (1.2 with some
2.0 features supported).  This project seeks to improve performance
portability of OpenCL programs with the kernel compiler and the task run-time,
reducing the need for target-dependent manual optimizations.

pocl uses Clang as an OpenCL C frontend and LLVM for kernel compiler
implementation, and as a portability layer.  Thus, if your desired target has
an LLVM backend, it should be able to get OpenCL support easily by using
pocl.")
    (license license:expat)))

(define-public python-pytools
  (package
    (name "python-pytools")
    (version "2020.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytools" version))
       (sha256
        (base32 "0q7439iy365a874ckpavx6h1mhnnngfy8nl7xj5grdd127zkknrp"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-appdirs" ,python-appdirs)
       ("python-decorator" ,python-decorator)
       ("python-numpy" ,python-numpy)
       ("python-six" ,python-six)
       ("python-mpi4py" ,python-mpi4py)))
    (home-page "https://pypi.org/project/pytools/")
    (synopsis "Assorted tools for Python")
    (description
     "Pytools is a bag of things that are ``missing'' from the Python standard
library:

@itemize
@item
small helper functions such as @code{len_iterable}, @code{argmin},
tuple generation, permutation generation, ASCII table pretty printing,
GvR's @code{monkeypatch_xxx} hack, the elusive @code{flatten}, and much more.
@item
Michele Simionato's decorator module
@item
A time-series logging module, @code{pytools.log}.
@item
Batch job submission, @code{pytools.batchjob}.
@item
A lexer, @code{pytools.lex}.
@end itemize\n")
    (license license:expat)))

(define-public python-pyopencl
  (package
    (name "python-pyopencl")
    (version "2019.1.1")
    (source
     (origin
       ;; The tarball on PyPI lacks test programs such as
       ;; 'pygpu_language_opencl.cpp' so fetch it from Git.
       ;; XXX: The server at git.tiker.net is unreliable.
       (method git-fetch)
       (uri (git-reference
             (url "http://git.tiker.net/trees/pyopencl.git")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "12q0rs8yla571vcfpsh0mfrjbdiayv0hi8r1rq0f178m3i3qjz80"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'set-home
                    (lambda _
                      ;; Some of the Python build scripts expect 'HOME' to be
                      ;; set.
                      (setenv "HOME" (getcwd))
                      #t)))

       ;; Tests in 'compyte/ndarray/setup_opencl.py' appear to rely on
       ;; 'nvcc', which is not an option.
       #:tests? #f))
    (inputs
     `(("opencl-headers" ,opencl-headers-1.2)   ;POCL only supports OpenCL 1.2
       ("pybind11" ,pybind11)
       ("ocl-icd" ,ocl-icd)))                     ;libOpenCL
    (propagated-inputs
     `(("python-appdirs" ,python-appdirs)
       ("python-decorator" ,python-decorator)
       ("python-numpy" ,python-numpy)
       ("python-pytools" ,python-pytools)
       ("python-six" ,python-six)
       ("python-mako" ,python-mako)))
    (home-page "http://mathema.tician.de/software/pyopencl")
    (synopsis "Python wrapper for OpenCL")
    (description
     "PyOpenCL lets you access parallel computing devices such as GPUs from
Python @i{via} OpenCL.")
    (license license:expat)))
