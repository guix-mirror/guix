;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
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
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages wine)
  #:use-module (gnu packages xorg))

(define-public spirv-headers
  (package
    (name "spirv-headers")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-Headers")
             (commit version)))
       (sha256
        (base32
         "069sivqajp7z4p44lmrz23lvf237xpkjxd4lzrg27836pwqcz9bj"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no tests
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
                             version "/LICENSE")))))

(define-public spirv-tools
  (package
    (name "spirv-tools")
    (version "2020.2")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/KhronosGroup/SPIRV-Tools")
            (commit (string-append "v" version))))
      (sha256
       (base32 "00b7xgyrcb2qq63pp3cnw5q1xqx2d9rfn65lai6n6r89s1vh3vg6"))
      (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON"
                               (string-append
                                "-DSPIRV-Headers_SOURCE_DIR="
                                (assoc-ref %build-inputs "spirv-headers")))))
    (inputs (list spirv-headers))
    (native-inputs (list pkg-config python))
    (home-page "https://github.com/KhronosGroup/SPIRV-Tools")
    (synopsis "API and commands for processing SPIR-V modules")
    (description
     "The SPIR-V Tools project provides an API and commands for processing
SPIR-V modules.  The project includes an assembler, binary module
parser,disassembler, validator, and optimizer for SPIR-V.")
    (license license:asl2.0)))

(define-public spirv-cross
  (package
    (name "spirv-cross")
    (version "2020-05-19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-Cross")
             (commit version)))
       (sha256
        (base32 "0zyijp9zx9wbd4i5lwjap7n793iz6yjkf27la60dsffxl75yy9pd"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DSPIRV_CROSS_SHARED=YES")
       ;; FIXME: The following tests fail:
       ;;   15 - spirv-cross-test-opt
       ;;   16 - spirv-cross-test-metal-opt
       ;;   17 - spirv-cross-test-hlsl-opt
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests-to-find-deps
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{CMAKE_(.*)_DIR\\}/external/glslang(.*)/bin")
                (string-append (assoc-ref inputs "glslang") "/bin")))
             (substitute* "CMakeLists.txt"
               (("\\$\\{CMAKE_(.*)_DIR\\}/external/spirv-tools(.*)/bin")
                (string-append (assoc-ref inputs "spirv-tools") "/bin")))
             #t)))))
    (inputs
     (list glslang spirv-headers spirv-tools))
    (native-inputs (list python))
    (home-page "https://github.com/KhronosGroup/SPIRV-Cross")
    (synopsis "Parser for and converter of SPIR-V to other shader languages")
    (description
     "SPIRV-Cross tries hard to emit readable and clean output from the
SPIR-V, aiming to emit GLSL or MSL that looks like human-written code.")
    (license license:asl2.0)))

(define-public glslang
  (package
    (name "glslang")
    (version "10-11.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/glslang")
             ;; Tag "10-11.0.0" was moved to "11.0.0".
             ;; FIXME: Use (commit version) on next update.
             (commit "11.0.0")))
       (sha256
        (base32
         "14mn2awswl022ls75mfpsnpsl0ai0jgfbqj3sxcsqawyj5f432py"))
       (file-name (string-append name "-" version "-checkout"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ;FIXME: requires bundled SPIRV-Tools
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
    (native-inputs
     (list pkg-config python))
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
    (version "1.2.164")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Headers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11wzxvwim4jna1yssbmprl211dhmz8vmrd498zww3bghzlj7bljv"))))
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
    (version "1.2.162")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Loader")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15gx9ab6w1sjq9hkpbas7z2f8f47j6mlln6p3w26qmydjj8gfjjv"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       ,#~(list
           (string-append "-DVULKAN_HEADERS_INSTALL_DIR="
                          #$(this-package-input "vulkan-headers"))
           (string-append "-DCMAKE_INSTALL_INCLUDEDIR="
                          #$(this-package-input "vulkan-headers")
                          "/include"))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'unpack-googletest
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((gtest (assoc-ref inputs "googletest:source")))
                        (when gtest
                          (copy-recursively gtest "external/googletest"))
                        #t)))
                  (add-after 'unpack 'disable-loader-tests
                    (lambda _
                      ;; Many tests require a Vulkan driver.  Skip those.
                      (substitute* "tests/loader_validation_tests.cpp"
                        ((".*= vkCreateInstance.*" all)
                         (string-append "GTEST_SKIP();\n" all))
                        (("TEST_F.*InstanceExtensionEnumerated.*" all)
                         (string-append all "\nGTEST_SKIP();\n")))
                      #t)))))
    (native-inputs
     `(("googletest:source" ,(package-source googletest))
       ("libxrandr" ,libxrandr)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("wayland" ,wayland)))
    (inputs
     (list vulkan-headers))
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

(define-public vulkan-tools
  (package
    (name "vulkan-tools")
    (version "1.2.162")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "129wzk7xj3vn3c8b4p7fzkd0npl58118s2i1d88gsfnlix54nagq"))))
    (build-system cmake-build-system)
    (inputs
     (list glslang libxrandr vulkan-loader wayland))
    (native-inputs
     (list pkg-config python vulkan-headers))
    (arguments
     `(#:tests? #f                      ;no tests
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
    (version "2020.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/shaderc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07h78nd964h2bdm4drzws8i1gvyal8a3wlhbcm5qxqk6vknv8hrk"))))
    (build-system cmake-build-system)
    (arguments
     `(;; FIXME: Skip most of the tests, because enabling system gtest breaks
       ;; the build: <https://github.com/google/shaderc/issues/470>.
       #:configure-flags
       (list "-DSHADERC_SKIP_TESTS=ON"
             ;; Note: despite the name, this just specifies the headers.
             (string-append "-Dglslang_SOURCE_DIR="
                            (assoc-ref %build-inputs "glslang") "/include/glslang"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-look-for-bundled-sources
           (lambda _
             (substitute* "CMakeLists.txt"
               (("add_subdirectory\\(third_party\\)")
                ""))

             (substitute* "glslc/test/CMakeLists.txt"
               (("\\$<TARGET_FILE:spirv-dis>")
                (which "spirv-dis")))

             ;; Do not attempt to use git to encode version information.
             (substitute* "glslc/CMakeLists.txt"
               (("add_dependencies\\(glslc_exe build-version\\)")
                ""))
             (call-with-output-file "glslc/src/build-version.inc"
               (lambda (port)
                 (format port "\"~a\"\n\"~a\"\n\"~a\"~%"
                         ,version
                         ,(package-version spirv-tools)
                         ,(package-version glslang))))
             #t)))))
    (inputs
     (list glslang python spirv-headers spirv-tools))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/google/shaderc")
    (synopsis "Tools for shader compilation")
    (description "Shaderc is a collection of tools, libraries, and tests for
shader compilation.")
    (license license:asl2.0)))

(define-public vkd3d
  (let ((commit "56cd4a94d541707959ce7677af6d1a34739e5579")) ; Release 1.2.
    (package
     (name "vkd3d")
     (version "1.2")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://source.winehq.org/git/vkd3d.git")
             (commit commit)))
       (sha256
        (base32
         "1n4a622drgnprvz5hjxzyzcsg2lp5rlf1sajki2vzf5gsx6fdpk8"))
       (file-name (string-append name "-" version "-checkout"))))
     (build-system gnu-build-system)
     (arguments
      `(#:configure-flags '("--with-spirv-tools")
        #:phases (modify-phases %standard-phases
                   (add-after 'unpack 'patch-for-new-vulkan
                     (lambda _
                       ;; Mimic upstream commit 8e7bf8a5c3e0047 for
                       ;; compatibility with newer vulkan-headers.
                       (substitute* "libs/vkd3d/vkd3d_private.h"
                         (("VK_PIPELINE_BIND_POINT_RANGE_SIZE")
                          "2u"))
                       #t)))))
     (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("gettext" ,gettext-minimal)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
     (inputs
      (list libx11
            libxcb
            spirv-headers
            spirv-tools
            vulkan-headers
            vulkan-loader
            wine-minimal ; Needed for 'widl'.
            xcb-util
            xcb-util-keysyms
            xcb-util-wm))
     (home-page "https://source.winehq.org/git/vkd3d.git/")
     (synopsis "Direct3D 12 to Vulkan translation library")
     (description "vkd3d is a library for translating Direct3D 12 to Vulkan.")
     (license license:lgpl2.1))))
