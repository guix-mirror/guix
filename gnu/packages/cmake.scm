;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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

(define-module (gnu packages cmake)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages file))

(define-public cmake
  (package
    (name "cmake")
    (version "2.8.10.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.cmake.org/files/v"
                   (substring version 0
                    (string-index version #\. (+ 1 (string-index version #\.))))
                   "/cmake-" version ".tar.gz"))
             (sha256
              (base32 "1c8fj6i2x9sb39wc9av2ighj415mw33cxfrlfpafcvm0knrlylnf"))
             (patches (list (search-patch "cmake-fix-tests.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     ;; Replace "/bin/sh" by the right path in... a lot of
                     ;; files.
                     (substitute*
                       '("Modules/CompilerId/Xcode-3.pbxproj.in"
                         "Modules/CompilerId/Xcode-1.pbxproj.in"
                         "Modules/CompilerId/Xcode-2.pbxproj.in"
                         "Modules/CPack.RuntimeScript.in"
                         "Source/cmakexbuild.cxx"
                         "Source/cmGlobalXCodeGenerator.cxx"
                         "Source/CTest/cmCTestBatchTestHandler.cxx"
                         "Source/cmLocalUnixMakefileGenerator3.cxx"
                         "Utilities/cmbzip2/Makefile-libbz2_so"
                         "Utilities/Release/release_cmake.cmake"
                         "Utilities/cmlibarchive/libarchive/archive_write_set_format_shar.c"
                         "Tests/CMakeLists.txt")
                       (("/bin/sh") (which "sh")))
                     (zero? (system* "./configure"
                             (string-append "--prefix=" out)))))
                 %standard-phases)))
    (inputs
     `(("file" ,file)))
    (home-page "http://www.cmake.org/")
    (synopsis "Cross-platform build system")
    (description
     "CMake is a family of tools designed to build, test and package software.
CMake is used to control the software compilation process using simple platform
and compiler independent configuration files. CMake generates native makefiles
and workspaces that can be used in the compiler environment of your choice.")
    (license bsd-3)))
