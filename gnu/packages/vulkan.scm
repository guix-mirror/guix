;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages wine)
  #:use-module (gnu packages xorg))

(define-public spirv-headers
  ;; Keep updated in accordance with
  ;; https://github.com/google/shaderc/blob/known-good/known_good.json
  (let ((commit "8bea0a266ac9b718aa0818d9e3a47c0b77c2cb23")
        (revision "4"))
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
           "01qyjghjz42hmyw9111zz20a1paf37ps39p4xbj8abjba65d8lqx"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ;; No tests
         #:phases (modify-phases %standard-phases
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (invoke "cmake" "-E" "copy_directory"
                                        "../source/include/spirv"
                                        (string-append (assoc-ref outputs "out")
                                                       "/include/spirv")))))))
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
    (version "2019.1")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/KhronosGroup/SPIRV-Tools")
            (commit (string-append "v" version))))
      (sha256
       (base32
        "0vddjzhkrhrm3l3i57nxmq2smv3r1s0ka5ff2kziaahr4hqb479r"))
      (file-name (string-append name "-" version "-checkout"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; FIXME: Tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fixgcc7
           (lambda _
             (unsetenv "C_INCLUDE_PATH")
             (unsetenv "CPLUS_INCLUDE_PATH")
             #t)))
       #:configure-flags (list (string-append "-DSPIRV-Headers_SOURCE_DIR="
                               (assoc-ref %build-inputs "spirv-headers")))))
    (inputs `(("spirv-headers" ,spirv-headers)))
    (native-inputs `(("gcc" ,gcc-7)
                     ("pkg-config" ,pkg-config)
                     ("python" ,python)))
    (home-page "https://github.com/KhronosGroup/SPIRV-Tools")
    (synopsis "API and commands for processing SPIR-V modules")
    (description
     "The SPIR-V Tools project provides an API and commands for processing
SPIR-V modules.  The project includes an assembler, binary module
parser,disassembler, validator, and optimizer for SPIR-V.")
    (license license:asl2.0)))

(define-public glslang
  (package
    (name "glslang")
    (version "7.11.3113")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/glslang")
             (commit version)))
       (sha256
        (base32
         "1kzv2b4q1fddxd7c0hc754nd6rw6y9vijb9fsi13xzzq9dficgb6"))
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
                   license:asl2.0))))

(define-public vulkan-headers
  (package
    (name "vulkan-headers")
    (version "1.1.106")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Headers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0idw7q715ikj575qmspvgq2gzc6c1sj581b8z3xnv6wz9qbzrmsd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; No tests.
    (home-page
     "https://github.com/KhronosGroup/Vulkan-Headers")
    (synopsis "Vulkan Header files and API registry")
    (description
     "Vulkan-Headers contains header files and API registry for Vulkan.")
    (license (list license:asl2.0)))) ;LICENSE.txt

(define-public vulkan-loader
  (package
    (name "vulkan-loader")
    (version (package-version vulkan-headers))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Loader")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ypjd2gfxdwldnqrrqy6bnjln5mml62a9k5pfi451srcxznijjai"))))
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
                          "-DBUILD_TESTS=OFF" ; FIXME: Needs 'googletest' submodule.
                          (string-append "-DCMAKE_INSTALL_LIBDIR="
                                         (assoc-ref %outputs "out") "/lib"))))
    (native-inputs `(("libxrandr" ,libxrandr)
                     ("pkg-config" ,pkg-config)
                     ("python" ,python)
                     ("vulkan-headers" ,vulkan-headers)
                     ("wayland" ,wayland)))
    (home-page
     "https://github.com/KhronosGroup/Vulkan-Loader")
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

(define-public vulkan-icd-loader
  (deprecated-package "vulkan-icd-loader" vulkan-loader))

(define-public vulkan-tools
  (package
    (name "vulkan-tools")
    (version (package-version vulkan-headers))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0swqyk16mbkivyk79dpqbhpw05a7yrakqynywznr5zgqbc0z4gj8"))))
    (build-system cmake-build-system)
    (inputs
     `(("glslang" ,glslang)
       ("libxrandr" ,libxrandr)
       ("vulkan-loader" ,vulkan-loader)
       ("wayland" ,wayland)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)
       ("vulkan-headers" ,vulkan-headers)))
    (arguments
     `(#:tests? #f                      ; No tests.
       #:configure-flags (list (string-append "-DGLSLANG_INSTALL_DIR="
                               (assoc-ref %build-inputs "glslang")))))
    (home-page
     "https://github.com/KhronosGroup/Vulkan-Tools")
    (synopsis "Tools and utilities for Vulkan")
    (description
     "Vulkan-Tools provides tools and utilities that can assist development by
enabling developers to verify their applications correct use of the Vulkan
API.")
    (license (list license:asl2.0)))) ;LICENSE.txt

(define-public shaderc
  (package
    (name "shaderc")
    (version "2018.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/shaderc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0qigmj0riw43pgjn5f6kpvk72fajssz1lc2aiqib5qvmj9rqq3hl"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f ; FIXME: Tests fail.
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
     `(("googletest" ,googletest)
       ("python" ,python)))
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
    (license license:asl2.0)))

(define-public vkd3d
  (let ((commit "ecda316ef54d70bf1b3e860755241bb75873e53f")) ; Release 1.1.
    (package
     (name "vkd3d")
     (version "1.1")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://source.winehq.org/git/vkd3d.git")
             (commit commit)))
       (sha256
        (base32
         "05a28kspy8gzng181w28zjqdb3pj2ss83b0lwnppxbcdzsz7rvrf"))
       (file-name (string-append name "-" version "-checkout"))))
     (build-system gnu-build-system)
     (arguments
      `(#:configure-flags '("--with-spirv-tools")))
     (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("gettext" ,gnu-gettext)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
     (inputs
      `(("libx11" ,libx11)
        ("libxcb" ,libxcb)
        ("spirv-headers" ,spirv-headers)
        ("spirv-tools" ,spirv-tools)
        ("vulkan-headers" ,vulkan-headers)
        ("vulkan-loader" ,vulkan-loader)
        ("wine-minimal" ,wine-minimal) ; Needed for 'widl'.
        ("xcb-util" ,xcb-util)
        ("xcb-util-keysyms" ,xcb-util-keysyms)
        ("xcb-util-wm" ,xcb-util-wm)))
     (home-page "https://source.winehq.org/git/vkd3d.git/")
     (synopsis "Direct3D 12 to Vulkan translation library")
     (description "vkd3d is a library for translating Direct3D 12 to Vulkan.")
     (license license:lgpl2.1))))
