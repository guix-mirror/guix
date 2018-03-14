;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages file)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages xml))

(define-public cmake
  (package
    (name "cmake")
    (version "3.7.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://www.cmake.org/files/v"
                                 (version-major+minor version)
                                 "/cmake-" version ".tar.gz"))
             (sha256
              (base32
               "1q6a60695prpzzsmczm2xrgxdb61fyjznb04dr6yls6iwv24c4nw"))
             (patches (search-patches "cmake-fix-tests.patch"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; Drop bundled software.
                 (with-directory-excursion "Utilities"
                   (for-each delete-file-recursively
                             '("cmbzip2"
                               ;"cmcompress"
                               "cmcurl"
                               "cmexpat"
                               ;"cmjsoncpp"
                               ;"cmlibarchive"
                               "cmliblzma"
                               "cmlibuv"
                               "cmzlib"))
                   #t)))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-bin-sh
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
                 "Source/cmExecProgramCommand.cxx"
                 "Utilities/Release/release_cmake.cmake"
                 "Utilities/cmlibarchive/libarchive/archive_write_set_format_shar.c"
                 "Tests/CMakeLists.txt"
                 "Tests/RunCMake/File_Generate/RunCMakeTest.cmake")
               (("/bin/sh") (which "sh")))
           #t))
         (add-before 'configure 'set-paths
           (lambda _
             ;; Help cmake's bootstrap process to find system libraries
             (begin
               (setenv "CMAKE_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
               (setenv "CMAKE_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))
               #t)))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system*
                       "./configure"
                       (string-append "--prefix=" out)
                       "--system-libs"
                       "--no-system-jsoncpp" ; FIXME: Circular dependency.
                       ;; By default, the man pages and other docs land
                       ;; in PREFIX/man and PREFIX/doc, but we want them
                       ;; in share/{man,doc}.  Note that unlike
                       ;; autoconf-generated configure scripts, cmake's
                       ;; configure prepends "PREFIX/" to what we pass
                       ;; to --mandir and --docdir.
                       "--mandir=share/man"
                       ,(string-append
                         "--docdir=share/doc/cmake-"
                         (version-major+minor version)))))))
         (add-before 'check 'set-test-environment
           (lambda _
             ;; Get verbose output from failed tests.
             (setenv "CTEST_OUTPUT_ON_FAILURE" "TRUE")
             ;; Run tests in parallel.
             (setenv "CTEST_PARALLEL_LEVEL"
                     (number->string (parallel-job-count)))
             #t)))))
    (inputs
     `(("file"       ,file)
       ("curl"       ,curl)
       ("zlib"       ,zlib)
       ("expat"      ,expat)
       ("bzip2"      ,bzip2)
       ("ncurses"    ,ncurses) ; required for ccmake
       ("libuv"      ,libuv)
       ("libarchive" ,libarchive)))
    (native-search-paths
     (list (search-path-specification
             (variable "CMAKE_PREFIX_PATH")
             (files '("")))))
    (home-page "https://www.cmake.org/")
    (synopsis "Cross-platform build system")
    (description
     "CMake is a family of tools designed to build, test and package software.
CMake is used to control the software compilation process using simple platform
and compiler independent configuration files.  CMake generates native makefiles
and workspaces that can be used in the compiler environment of your choice.")
    (license (list license:bsd-3             ; cmake
                   license:bsd-4             ; cmcompress
                   license:bsd-2             ; cmlibarchive
                   license:expat             ; cmjsoncpp is dual MIT/public domain
                   license:public-domain)))) ; cmlibarchive/archive_getdate.c

;; Recent Ceph requires Boost 1.66, which in turn requires CMake 3.11 for
;; its updated "FindBoost.cmake" facility.
(define-public cmake-3.11
  (package
    (inherit cmake)
    (version "3.11.0-rc2")
    (source (origin
              (inherit (package-source cmake))
              (uri (string-append "https://www.cmake.org/files/v"
                                  (version-major+minor version)
                                  "/cmake-" version ".tar.gz"))
              (sha256
               (base32
                "14p6ais19nfcwl914n4n5rbzaqwafv3qkg6nd8jw54ykn6lz6mf3"))
              (snippet
               '(begin
                  ;; Drop bundled software.
                  (with-directory-excursion "Utilities"
                    (for-each delete-file-recursively
                              '("cmbzip2"
                                "cmcurl"
                                "cmexpat"
                                "cmliblzma"
                                "cmzlib"))
                    #t)))))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments cmake)
       ((#:make-flags flags ''()) `(cons (string-append
                                          "ARGS=-j "
                                          (number->string (parallel-job-count))
                                          " --output-on-failure")
                                         ,flags))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'patch-bin-sh
             (lambda _
               (substitute*
                   '("Modules/CompilerId/Xcode-3.pbxproj.in"
                     "Modules/CPack.RuntimeScript.in"
                     "Source/cmakexbuild.cxx"
                     "Source/cmGlobalXCodeGenerator.cxx"
                     "Source/cmLocalUnixMakefileGenerator3.cxx"
                     "Source/cmExecProgramCommand.cxx"
                     "Utilities/Release/release_cmake.cmake"
                     "Utilities/cmlibarchive/libarchive/archive_write_set_format_shar.c"
                     "Tests/CMakeLists.txt"
                     "Tests/RunCMake/File_Generate/RunCMakeTest.cmake")
                 (("/bin/sh") (which "sh")))
               #t))
           ;; This is now passed through #:make-flags.
           (delete 'set-test-environment)))))
    (inputs
     `(("rhash" ,rhash)
       ,@(package-inputs cmake)))))
