;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
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

(define-module (gnu packages vulkan)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public spirv-headers
  (let ((commit "98b01515724c428d0f0a5d01deffcce0f5f5e61c")
        (revision "1"))
    (package
      (name "spirv-headers")
      (version (string-append "0.0-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/KhronosGroup/SPIRV-Headers")
               (commit commit)))
         (sha256
          (base32
           "15bknwkv3xwmjs3lmkp282a1wrp0da1b4lp45i4yiav04zmqygj2"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ;; No tests
         #:phases (modify-phases %standard-phases
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (zero? (system* "cmake" "-E" "copy_directory"
                                        "../source/include/spirv"
                                        (string-append (assoc-ref outputs "out")
                                                       "/include/spirv"))))))))
      (home-page "https://github.com/KhronosGroup/SPIRV-Headers")
      (synopsis "Machine-readable files from the SPIR-V Registry")
      (description
       "SPIRV-Headers is a repository containing machine-readable files from
the SPIR-V Registry.  This includes:
@itemize
@item Header files for various languages.
@item JSON files describing the grammar for the SPIR-V core instruction set,
and for the GLSL.std.450 extended instruction set.
@item The XML registry file.
@end itemize\n")
      (license (license:x11-style
                (string-append "https://github.com/KhronosGroup/SPIRV-Headers/blob/"
                               commit "/LICENSE"))))))

(define-public spirv-tools
  (package
    (name "spirv-tools")
    (version "2017.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/KhronosGroup/SPIRV-Tools/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32
         "009vflaa71a7xhvmm23f4sdbcgdkl1k4facqkwsg6djha2sdpsqq"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list (string-append "-DCMAKE_INSTALL_LIBDIR="
                                              (assoc-ref %outputs "out")
                                              "/lib")
                               (string-append "-DSPIRV-Headers_SOURCE_DIR="
                                              (assoc-ref %build-inputs
                                                         "spirv-headers")))))
    (inputs `(("spirv-headers" ,spirv-headers)))
    (native-inputs `(("pkg-config", pkg-config)
                     ("python" ,python)))
    (home-page "https://github.com/KhronosGroup/SPIRV-Tools")
    (synopsis "API and commands for processing SPIR-V modules")
    (description
     "The SPIR-V Tools project provides an API and commands for processing
SPIR-V modules.  The project includes an assembler, binary module parser,
disassembler, validator, and optimizer for SPIR-V.")
    (license license:asl2.0)))

(define-public glslang
  ;; Version 3.0 is too old for vulkan-icd-loader. Use a recent git commit
  ;; until the next stable version.
  (let ((commit "471bfed0621162a7513fc24a51e8a1ccc2e640ff")
        (revision "1"))
    (package
      (name "glslang")
      (version (string-append "3.0-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/KhronosGroup/glslang")
               (commit commit)))
         (sha256
          (base32
           "0m2vljmrqppp80ghbbwfnayqw2canxlcjhgy6jw9xjdssln0d3pd"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ;; No tests
         ;; glslang tries to set CMAKE_INSTALL_PREFIX manually. Remove the
         ;; offending line.
         #:phases (modify-phases %standard-phases
                    (add-after 'patch-source-shebangs 'fix-cmakelists
                      (lambda _
                        (substitute* "CMakeLists.txt"
                          (("set.*CMAKE_INSTALL_PREFIX.*") ""))
                        #t)))))
      (native-inputs `(("bison" ,bison)
                       ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/KhronosGroup/glslang")
      (synopsis "OpenGL and OpenGL ES shader front end and validator")
      (description
       "Glslang is the official reference compiler front end for the
OpenGL@tie{}ES and OpenGL shading languages.  It implements a strict
interpretation of the specifications for these languages.")
      ;; Modified BSD license. See "copyright" section of
      ;; https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/
      (license (list license:bsd-3
                     ;; include/SPIRV/{bitutils,hex_float}.h are Apache 2.0.
                     license:asl2.0)))))
