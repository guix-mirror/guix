;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module ((guix licenses) #:select (bsd-3))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages file)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1))

(define-public cmake
  (package
    (name "cmake")
    (version "2.8.12")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.cmake.org/files/v"
                   (string-join (take (string-split version #\.) 2)
                                ".")
                   "/cmake-" version ".tar.gz"))
             (sha256
              (base32 "11q21vyrr6c6smyjy81k2k07zmn96ggjia9im9cxwvj0n88bm1fq"))
             (patches (list (search-patch "cmake-fix-tests.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases (alist-cons-before
                 'configure 'patch-bin-sh
                 (lambda _
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
                     (("/bin/sh") (which "sh"))))
                 (alist-cons-before
                  'configure 'set-paths
                  (lambda _
                    ;; Help cmake's bootstrap process to find system libraries
                    (begin
                      (setenv "CMAKE_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
                      (setenv "CMAKE_INCLUDE_PATH" (getenv "CPATH"))))
                  (alist-replace
                   'configure
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out")))
                       (zero? (system*
                               "./configure"
                               (string-append "--prefix=" out)
                               "--system-libs"
                               ;; By default, the man pages and other docs land
                               ;; in PREFIX/man and PREFIX/doc, but we want them
                               ;; in share/{man,doc}.  Note that unlike
                               ;; autoconf-generated configure scripts, cmake's
                               ;; configure prepends "PREFIX/" to what we pass
                               ;; to --mandir and --docdir.
                               "--mandir=share/man"
                               ,(string-append
                                 "--docdir=share/doc/cmake-"
                                 (string-join (take (string-split version #\.) 2)
                                              "."))))))
                   %standard-phases)))))
    (inputs
     `(("file"       ,file)
       ("curl"       ,curl)
       ("zlib"       ,zlib)
       ("expat"      ,expat)
       ("bzip2"      ,bzip2)
       ("libarchive" ,libarchive)))
    (home-page "http://www.cmake.org/")
    (synopsis "Cross-platform build system")
    (description
     "CMake is a family of tools designed to build, test and package software.
CMake is used to control the software compilation process using simple platform
and compiler independent configuration files.  CMake generates native makefiles
and workspaces that can be used in the compiler environment of your choice.")
    (license bsd-3)))
