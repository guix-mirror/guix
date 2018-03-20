;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg))

(define-public spirv-headers
  ;; Keep updated in accordance with
  ;; https://github.com/google/shaderc/blob/known-good/known_good.json
  (let ((commit "061097878467b8e040fbf153a837d844ef9f9f96")
        (revision "2"))
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
           "0bf9h6xc5fkncxnadwmqvpsjh7sdgns6is8prv1gvlfzrkvpjj17"))
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
  ;; Keep updated in accordance with
  ;; https://github.com/google/shaderc/blob/known-good/known_good.json
  (let ((commit "90862fe4b1c6763b32ce683d2d32c2f281f577cf")
        (revision "1"))
    (package
     (name "spirv-tools")
     (version (string-append "0.0-" revision "." (string-take commit 9)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-Tools")
             (commit commit)))
       (sha256
        (base32
         "1yq8ab6f52wcxm2azzmx70nqz8l35izd45830aikjkk1jfzmzabb"))
       (file-name (string-append name "-" version "-checkout"))))
     (build-system cmake-build-system)
     (arguments
      `(#:configure-flags (list (string-append "-DCMAKE_INSTALL_LIBDIR="
                                               (assoc-ref %outputs "out")
                                               "/lib")
                                (string-append "-DSPIRV-Headers_SOURCE_DIR="
                                               (assoc-ref %build-inputs
                                                          "spirv-headers")))))
     (inputs `(("spirv-headers" ,spirv-headers)))
     (native-inputs `(("pkg-config" ,pkg-config)
                      ("python" ,python)))
     (home-page "https://github.com/KhronosGroup/SPIRV-Tools")
     (synopsis "API and commands for processing SPIR-V modules")
     (description
      "The SPIR-V Tools project provides an API and commands for processing
SPIR-V modules.  The project includes an assembler, binary module parser,
disassembler, validator, and optimizer for SPIR-V.")
     (license license:asl2.0))))

(define-public glslang
  ;; Keep updated in accordance with
  ;; https://github.com/google/shaderc/blob/known-good/known_good.json
  (let ((commit "b5b08462442239e6537315ea1405b6afcd53043e")
        (revision "2"))
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
           "08imby3hciisshzacrkx8s56lx4fxm7dad06xxaxxcapinmqrvwk"))
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

(define-public vulkan-icd-loader
  (package
    (name "vulkan-icd-loader")
    (version "1.1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/KhronosGroup/Vulkan-LoaderAndValidationLayers/"
             "archive/sdk-" version ".tar.gz"))
       (sha256
        (base32
         "15qkh77596v3xivnbb3l0q9zbmmsdglnaza2m1g7f8q7bbigyc5x"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;FIXME: 23/39 tests fail.  Try "tests/run_all_tests.sh".
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-spirv-tools-commit-id
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Remove lines trying to build in a git commit id.
             (substitute* "CMakeLists.txt" ((".*spirv_tools_commit_id.h.*") ""))
             #t)))
       #:configure-flags (list
                          "-DBUILD_LAYERS=OFF" ; FIXME: Fails to build.
                          (string-append "-DCMAKE_INSTALL_LIBDIR="
                                              (assoc-ref %outputs "out") "/lib"))))
    (inputs `(("glslang" ,glslang)
              ("libxcb" ,libxcb)
              ("libx11" ,libx11)
              ("libxrandr" ,libxrandr)
              ("mesa" ,mesa)
              ("spirv-tools" ,spirv-tools)
              ("wayland" ,wayland)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("python" ,python)))
    (home-page
     "https://github.com/KhronosGroup/Vulkan-LoaderAndValidationLayers")
    (synopsis "Khronos official ICD loader and validation layers for Vulkan")
    (description
     "Vulkan allows multiple @dfn{Installable Client Drivers} (ICDs) each
supporting one or more devices to be used collectively.  The loader is
responsible for discovering available Vulkan ICDs on the system and inserting
Vulkan layer libraries, including validation layers between the application
and the ICD.")
    ;; This software is mainly Apache 2.0 licensed, but contains some components
    ;; covered by other licenses.  See COPYRIGHT.txt for details.
    (license (list license:asl2.0       ;LICENSE.txt
                   (license:x11-style "file://COPYRIGHT.txt")
                   license:bsd-3))))

(define-public shaderc
  (let ((commit "773ec22d49f40b7161820f29d953be4a7e40190d")
        (revision "1"))
    (package
      (name "shaderc")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/shaderc")
               (commit commit)))
         (file-name (string-append name "-" version ".tar.gz"))
         (sha256
          (base32
           "0b41inb1czxv3mciip0lfdxv19ccx2ys31fivfywjn2q8va1gd1f"))))
      (build-system meson-build-system)
      (arguments
       `(#:tests? #f                    ; tests don't work yet.
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; Remove various lines and touch build-version.inc or
                 ;; configuring won't work.
                 (invoke "touch" "glslc/src/build-version.inc")
                 (substitute* "CMakeLists.txt" (("..PYTHON_EXE..*") ""))
                 (substitute* "CMakeLists.txt"
                   ((".*update_build_version.py..*") ""))
                 (substitute* "CMakeLists.txt"
                   ((".*add_custom_target.build-version.*") ""))
                 (substitute* "CMakeLists.txt"
                   ((".*spirv-tools_SOURCE_DIR.*glslang_SOURCE_DIR.*")
                    ""))
                 (substitute* "CMakeLists.txt"
                   ((".*Update build-version.inc.*") ""))
                 (substitute* "CMakeLists.txt" ((".*--check.*") ""))
                 (substitute* "glslc/src/main.cc" ((".*build-version.inc.*")
                                                   "\"1\""))
                 (invoke "cmake" "-GNinja" "-DCMAKE_BUILD_TYPE=Release"
                         "-DSHADERC_SKIP_TESTS=ON"
                         "-DCMAKE_INSTALL_LIBDIR=lib"
                         (string-append "-DCMAKE_INSTALL_PREFIX="
                                        out)))))
           (add-after 'unpack 'unpack-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((spirv-tools-source (assoc-ref inputs "spirv-tools-source"))
                     (spirv-headers-source (assoc-ref inputs "spirv-headers-source"))
                     (glslang-source (assoc-ref inputs "glslang-source")))
                 (copy-recursively spirv-tools-source "third_party/spirv-tools")
                 (copy-recursively spirv-headers-source
                                   (string-append "third_party/spirv-tools"
                                                  "/external/spirv-headers"))
                 (copy-recursively glslang-source "third_party/glslang")
                 #t))))))
      (inputs
       `(("python" ,python)))
      (native-inputs
       `(("cmake" ,cmake)
         ("glslang-source" ,(package-source glslang))
         ("pkg-config" ,pkg-config)
         ("spirv-headers-source" ,(package-source spirv-headers))
         ("spirv-tools-source" ,(package-source spirv-tools))))
      (home-page "https://github.com/google/shaderc")
      (synopsis "Tools for shader compilation")
      (description "Shaderc is a collection of tools, libraries, and tests for
shader compilation.")
      (license license:asl2.0))))
