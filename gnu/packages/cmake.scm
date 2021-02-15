;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Pierre-Moana Levesque <pierre.moana.levesque@gmail.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix deprecation)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages file)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public cmake-shared
  (let ((commit "8122f2b96c8da38ea41b653cf69958e75fe2129d")
        (revision "32"))
    (package
      (name "cmake-shared")
      (version
       (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/lirios/cmake-shared.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "05avwzqcnliwx9h7qi1kl0iz4smqmxc4vkavyjbmlc6h2b97i58g"))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (srfi srfi-1)))
         (snippet
          `(begin
             (delete-file-recursively "3rdparty")))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f))                  ; No target
      (native-inputs
       `(("extra-cmake-modules" ,extra-cmake-modules)))
      (synopsis "Shared CMake functions and macros")
      (description "CMake-Shared are shared functions and macros for projects
using the CMake build system.")
      (home-page "https://github.com/lirios/cmake-shared/")
      (license license:bsd-3))))

;;; Build phases shared between 'cmake-bootstrap' and the later variants
;;; that use cmake-build-system.
(define %common-build-phases
  `((add-after 'unpack 'split-package
      ;; Remove files that have been packaged in other package recipes.
      (lambda _
        (delete-file "Auxiliary/cmake-mode.el")
        (substitute* "Auxiliary/CMakeLists.txt"
          ((".*cmake-mode.el.*") ""))
        #t))
    (add-before 'configure 'patch-bin-sh
      (lambda _
        ;; Replace "/bin/sh" by the right path in... a lot of
        ;; files.
        (substitute*
            '("Modules/CompilerId/Xcode-3.pbxproj.in"
              "Modules/Internal/CPack/CPack.RuntimeScript.in"
              "Source/cmGlobalXCodeGenerator.cxx"
              "Source/cmLocalUnixMakefileGenerator3.cxx"
              "Source/cmExecProgramCommand.cxx"
              "Utilities/Release/release_cmake.cmake"
              "Tests/CMakeLists.txt"
              "Tests/RunCMake/File_Generate/RunCMakeTest.cmake")
          (("/bin/sh") (which "sh")))
        #t))))

(define %common-disabled-tests
  '(;; This test copies libgcc_s.so.1 from GCC and tries to modify its RPATH,
    ;; but does not cope with the file being read-only.
    "BundleUtilities"
    ;; This test requires network access.
    "CTestTestUpload"
    ;; This test requires 'ldconfig' which is not available in Guix.
    "RunCMake.install"))

(define %preserved-third-party-files
  '(;; 'Source/cm_getdate.c' includes archive_getdate.c wholesale, so it must
    ;; be available along with the required headers.
    "Utilities/cmlibarchive/libarchive/archive_getdate.c"
    "Utilities/cmlibarchive/libarchive/archive_getdate.h"))

;;; The "bootstrap" CMake.  It is used to build 'cmake-minimal' below, as well
;;; as any dependencies that need cmake-build-system.
(define-public cmake-bootstrap
  (package
    (name "cmake-bootstrap")
    (version "3.16.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cmake.org/files/v"
                                  (version-major+minor version)
                                  "/cmake-" version ".tar.gz"))
              (sha256
               (base32
                "1z4bb8z6b4dvq5hrvajrf1hyybqay3xybyimf71w1jgcp180nxjz"))
              (modules '((guix build utils)
                         (ice-9 ftw)))
              (snippet
               `(begin
                  ;; CMake bundles its dependencies in the "Utilities" directory.
                  ;; Delete those to ensure the system libraries are used.
                  (define preserved-files
                    '(,@%preserved-third-party-files
                      ;; Use the bundled JsonCpp during bootstrap to work around
                      ;; a circular dependency.  TODO: JsonCpp can be built with
                      ;; Meson instead of CMake, but meson-build-system currently
                      ;; does not support cross-compilation.
                      "Utilities/cmjsoncpp"
                      ;; LibUV is required to bootstrap the initial build system.
                      "Utilities/cmlibuv"))

                  (file-system-fold (lambda (dir stat result)         ;enter?
                                      (or (string=? "Utilities" dir)  ;init
                                          ;; The bundled dependencies are
                                          ;; distinguished by having a "cm"
                                          ;; prefix to their upstream names.
                                          (and (string-prefix? "Utilities/cm" dir)
                                               (not (member dir preserved-files)))))
                                    (lambda (file stat result)        ;leaf
                                      (unless (or (member file preserved-files)
                                                  ;; Preserve top-level files.
                                                  (string=? "Utilities"
                                                            (dirname file)))
                                        (delete-file file)))
                                    (const #t)                        ;down
                                    (lambda (dir stat result)         ;up
                                      (when (equal? (scandir dir) '("." ".."))
                                        (rmdir dir)))
                                    (const #t)                        ;skip
                                    (lambda (file stat errno result)
                                      (format (current-error-port)
                                              "warning: failed to delete ~a: ~a~%"
                                              file (strerror errno)))
                                    #t
                                    "Utilities"
                                    lstat)
                  #t))
              (patches (search-patches "cmake-curl-certificates.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:configure-flags
       (let ((out (assoc-ref %outputs "out"))
             (parallel-job-count (number->string (parallel-job-count))))
         (list "--verbose"
               (string-append "--parallel=" parallel-job-count)
               (string-append "--prefix=" out)
               "--system-libs"
               "--no-system-jsoncpp"
               ;; By default, the man pages and other docs land
               ;; in PREFIX/man and PREFIX/doc, but we want them
               ;; in share/{man,doc}.  Note that unlike
               ;; autoconf-generated configure scripts, cmake's
               ;; configure prepends "PREFIX/" to what we pass
               ;; to --mandir and --docdir.
               "--mandir=share/man"
               ,(string-append "--docdir=share/doc/cmake-"
                               (version-major+minor version))

               ;; By default CMake is built without any optimizations.  Use
               ;; the recommended Release target for a ~2.5x speedup.
               "--" "-DCMAKE_BUILD_TYPE=Release"))
       #:make-flags
       (let ((skipped-tests
              (list ,@%common-disabled-tests
                    "CTestTestSubdir" ; This test fails to build 2 of the 3 tests.
                    ;; This test fails when ARGS (below) is in use, see
                    ;; <https://gitlab.kitware.com/cmake/cmake/issues/17165>.
                    "CTestCoverageCollectGCOV")))
         (list
          (string-append
           ;; These arguments apply for the tests only.
           "ARGS=-j " (number->string (parallel-job-count))
           " --output-on-failure"
           " --exclude-regex ^\\(" (string-join skipped-tests "\\|") "\\)$")))
       #:phases
       (modify-phases %standard-phases
         ,@%common-build-phases
         (add-before 'configure 'set-paths
           (lambda _
             ;; Help cmake's bootstrap process to find system libraries
             (begin
               (setenv "CMAKE_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
               (setenv "CMAKE_INCLUDE_PATH" (or (getenv "CPATH")
                                                (getenv "C_INCLUDE_PATH")))
               #t)))
         ;; CMake uses its own configure script.
         (replace 'configure
           (lambda* (#:key (configure-flags '()) #:allow-other-keys)
             (apply invoke "./configure" configure-flags))))))
    (inputs
     `(("bzip2" ,bzip2)
       ("curl" ,curl-minimal)
       ("expat" ,expat)
       ("file" ,file)
       ("libarchive" ,libarchive)
       ,@(if (hurd-target?)
             '()
             `(("libuv" ,libuv)))       ;not supported on the Hurd
       ("ncurses" ,ncurses)             ;required for ccmake
       ("rhash" ,rhash)
       ("zlib" ,zlib)))
    (native-search-paths
     (list (search-path-specification
            (variable "CMAKE_PREFIX_PATH")
            (files '("")))
           ;; "cmake-curl-certificates.patch" changes CMake to honor 'SSL_CERT_DIR'
           ;; and 'SSL_CERT_FILE', hence these search path entries.
           (search-path-specification
            (variable "SSL_CERT_DIR")
            (separator #f)              ;single entry
            (files '("etc/ssl/certs")))
           (search-path-specification
            (variable "SSL_CERT_FILE")
            (file-type 'regular)
            (separator #f)              ;single entry
            (files '("etc/ssl/certs/ca-certificates.crt")))))
    (home-page "https://cmake.org/")
    (synopsis "Cross-platform build system")
    (description
     "CMake is a family of tools designed to build, test and package software.
CMake is used to control the software compilation process using simple platform
and compiler independent configuration files.  CMake generates native makefiles
and workspaces that can be used in the compiler environment of your choice.")
    (properties '((hidden? . #t)))
    (license (list license:bsd-3        ; cmake
                   license:expat        ; cmjsoncpp is dual MIT/public domain
                   license:public-domain)))) ; cmlibarchive/archive_getdate.c

;;; This minimal variant of CMake does not include the documentation.  It is
;;; used by the cmake-build-system.
(define-public cmake-minimal
  (package
    (inherit cmake-bootstrap)
    (name "cmake-minimal")
    (source (origin
              (inherit (package-source cmake-bootstrap))
              (snippet
               (match (origin-snippet (package-source cmake-bootstrap))
                 ((_ _ exp ...)
                  ;; Now we can delete the remaining software bundles.
                  (append `(begin
                             (define preserved-files ',%preserved-third-party-files))
                          exp))))))
    (inputs
     `(("curl" ,curl)
       ("jsoncpp" ,jsoncpp)
       ,@(alist-delete "curl" (package-inputs cmake-bootstrap))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DCMAKE_USE_SYSTEM_LIBRARIES=ON"
             (string-append "-DCMAKE_DOC_DIR=share/doc/cmake-"
                            ,(version-major+minor (package-version
                                                   cmake-bootstrap))))

       ;; This is the CMake used in cmake-build-system.  Ensure compiler
       ;; optimizations are enabled to save size and CPU cycles.
       #:build-type "Release"
       #:phases
       (modify-phases %standard-phases
         ,@%common-build-phases
         (replace 'check
           (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
             (let ((skipped-tests (list ,@%common-disabled-tests
                                        ;; This test requires the bundled libuv.
                                        "BootstrapTest")))
               (if tests?
                   (begin
                     (invoke "ctest" "-j" (if parallel-tests?
                                              (number->string (parallel-job-count))
                                              "1")
                             "--exclude-regex"
                             (string-append "^(" (string-join skipped-tests "|") ")$")))
                   (format #t "test suite not run~%"))
               #t))))
        ,@(if (%current-target-system)
              '()
              `(#:cmake ,cmake-bootstrap))))))

;;; The "user-facing" CMake, now with manuals and HTML documentation.
(define-public cmake
  (package
    (inherit cmake-minimal)
    (name "cmake")
    (version "3.19.2")
    ;; TODO: Move the following source field to the cmake-bootstrap package in
    ;; the next rebuild cycle.
    (source (origin
              (inherit (package-source cmake-bootstrap))
              (uri (string-append "https://cmake.org/files/v"
                                  (version-major+minor version)
                                  "/cmake-" version ".tar.gz"))
              (sha256
               (base32
                "1w67w0ak6vf37501dlz9yhnzlvvpw1w10n2nm3hi7yxp4cxzvq73"))
              (snippet
               (match (origin-snippet (package-source cmake-bootstrap))
                 ((_ _ exp ...)
                  ;; Now we can delete the remaining software bundles.
                  (append `(begin
                             (define preserved-files
                               '(,@%preserved-third-party-files
                                 ;; TODO: Move this file to the
                                 ;; %preserved-third-party-files variable in
                                 ;; the next rebuild cycle.
                                 "Utilities/cm3p" ;CMake header wrappers
                                 ;; Use the bundled JsonCpp during bootstrap
                                 ;; to work around a circular dependency.
                                 ;; TODO: JsonCpp can be built with Meson
                                 ;; instead of CMake, but meson-build-system
                                 ;; currently does not support
                                 ;; cross-compilation.
                                 "Utilities/cmjsoncpp"
                                 ;; LibUV is required to bootstrap the initial
                                 ;; build system.
                                 "Utilities/cmlibuv")))
                          exp))))))
    (arguments
     (substitute-keyword-arguments (package-arguments cmake-minimal)
       ;; Use cmake-minimal this time.
       ((#:cmake _ #f)
        (if (%current-target-system)
            cmake-minimal-cross
            cmake-minimal))

       ;; Enable debugging information for convenience.
       ((#:build-type _ #f) "RelWithDebInfo")

       ((#:configure-flags flags ''())
        `(append (list "-DSPHINX_INFO=ON" "-DSPHINX_MAN=ON" "-DSPHINX_HTML=ON"
                       (string-append "-DCMAKE_DOC_DIR=share/doc/cmake-"
                                      ,(version-major+minor (package-version
                                                             cmake-minimal)))
                       "-DCMAKE_INFO_DIR=share/info"
                       "-DCMAKE_MAN_DIR=share/man")
                 ,flags))
       ((#:phases phases)
        `(modify-phases ,phases
           ;; TODO: Remove this override in the next rebuild cycle and adjust
           ;; the %common-build-phases variable instead: the
           ;; Utilities/Release/release_cmake.cmake file no longer exists in
           ;; version 3.19.0.
           (replace 'patch-bin-sh
             (lambda _
               ;; Replace "/bin/sh" by the right path in... a lot of
               ;; files.
               (substitute*
                   '("Modules/CompilerId/Xcode-3.pbxproj.in"
                     "Modules/Internal/CPack/CPack.RuntimeScript.in"
                     "Source/cmGlobalXCodeGenerator.cxx"
                     "Source/cmLocalUnixMakefileGenerator3.cxx"
                     "Source/cmExecProgramCommand.cxx"
                     "Tests/CMakeLists.txt"
                     "Tests/RunCMake/File_Generate/RunCMakeTest.cmake")
                 (("/bin/sh") (which "sh")))
               #t))
           ;; TODO: Remove this override in the next rebuild cycle and adjust
           ;; the %common-disabled-tests variable instead.
           (replace 'check
             (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
               (let ((skipped-tests (list ,@%common-disabled-tests
                                          ;; This test fails for unknown reason.
                                          "RunCMake.file-GET_RUNTIME_DEPENDENCIES"
                                          ;; This test requires the bundled libuv.
                                          "BootstrapTest")))
                 (if tests?
                     (begin
                       (invoke "ctest" "-j" (if parallel-tests?
                                                (number->string (parallel-job-count))
                                                "1")
                               "--exclude-regex"
                               (string-append "^(" (string-join skipped-tests "|") ")$")))
                     (format #t "test suite not run~%"))
                 #t)))
           (add-after 'install 'move-html-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (doc (assoc-ref outputs "doc"))
                     (html (string-append "/share/doc/cmake-"
                                          ,(version-major+minor
                                            (package-version cmake-minimal))
                                          "/html")))
                 (copy-recursively (string-append out html)
                                   (string-append doc html))
                 (delete-file-recursively (string-append out html))
                 #t)))))))
    ;; Extra inputs required to build the documentation.
    (native-inputs
     `(,@(package-native-inputs cmake-minimal)
       ("python-sphinx" ,python-sphinx)
       ("texinfo" ,texinfo)))
    (outputs '("out" "doc"))
    (properties (alist-delete 'hidden? (package-properties cmake-minimal)))))

(define-public cmake-minimal-cross
  (package
    (inherit cmake-minimal)
    (name "cmake-minimal-cross")
    (native-search-paths '())
    (search-paths
     (package-native-search-paths cmake-minimal))))

(define-public emacs-cmake-mode
  (package
    (inherit cmake)
    (name "emacs-cmake-mode")
    (native-inputs '())
    (inputs '())
    (outputs '("out"))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-elisp
           ;; Elisp directory is not in root of the source.
           (lambda _
             (chdir "Auxiliary")
             #t)))))
    (synopsis "Emacs major mode for editing Cmake expressions")
    (description "@code{cmakeos-mode} provides an Emacs major mode for editing
Cmake files.  It supports syntax highlighting, indenting and refilling of
comments.")))
