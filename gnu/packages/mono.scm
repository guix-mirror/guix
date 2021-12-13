;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (gnu packages mono)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public mono
  (package
    (name "mono")
    (version "4.4.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://download.mono-project.com/sources/mono/"
                    name "-" version
                    ".tar.bz2"))
              (sha256
               (base32
                "0jibyvyv2jy8dq5ij0j00iq3v74r0y90dcjc3dkspcfbnn37cphn"))
              (patches (search-patches "mono-mdoc-timestamping.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("libxslt" ,libxslt)
       ("perl" ,perl)
       ("python" ,python-2)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "mono/mini/Makefile.in"
              (("build_date = [^;]*;")
               "build_date = (void*) 0;"))
             #t))
         (add-after 'unpack 'set-env
           (lambda _ ;;* (#:key inputs #:allow-other-keys)
             ;; all tests under mcs/class fail trying to access $HOME
             (setenv "HOME" "/tmp")
             ;; ZIP files have "DOS time" which starts in Jan 1980.
             (setenv "SOURCE_DATE_EPOCH" "315532800")
             #t))
         (add-after 'unpack 'fix-includes
           (lambda _
             ;; makedev is in <sys/sysmacros.h> now.  Include it.
             (substitute* "mono/io-layer/processes.c"
              (("#ifdef HAVE_SYS_MKDEV_H") "#if 1")
              (("sys/mkdev.h") "sys/sysmacros.h"))
             #t))
         (add-after 'unpack 'patch-tests
           (lambda _  ;;* (#:key inputs #:allow-other-keys)
             (substitute* "mono/tests/Makefile.in"
               ;; does not build: no rule to make unhandled-exception-*
               (("@test-unhandled-exception-2:" all)
                (string-append all "#")))
             (substitute* "mcs/tools/mono-symbolicate/Makefile"
               ;; does not build: Source file `Test/StackTraceDumper.cs'
               ;; could not be found
               (("^check: test-local") "check:\ntest-local:")
               (("^test-local: all") "disabled-test-local:"))
             (substitute* "mono/unit-tests/Makefile.in"
               ;; test fails
               (("^test-sgen-qsort.log:")
                "disabled-test-sgen-qsort.log:\ntest-sgen-qsort.log:"))
             ;; tests fail, trying to access $HOME
             (substitute* "mcs/class/Makefile"
               (("^include ../build/rules.make" all)
                (string-append
                 all
                 "\nrun-test-recursive:\n\t@echo skipping tests\n")))
             ;; tests fail, trying to access $HOME
             (substitute* "mcs/class/Microsoft.Build.Tasks/Makefile"
               (("^include ../../build/rules.make" all)
                (string-append
                 all
                 "\nrun-test-recursive:\n\t@echo skipping tests\n")))
             (substitute* '("mcs/tools/mono-shlib-cop/Makefile"
                            "mcs/tools/mdoc/Makefile")
               (("^run-test-local:" all)
                (string-append "#" all)))
             (substitute* "mcs/tools/sqlmetal/Makefile"
               (("^include ../../build/rules.make" all)
                (string-append
                 "NO_TEST:=true\n"
                 all
                 "\nrun-test-lib:\n\t@echo skipping test\n"))))))
       ;; these 4 tests fail
       #:make-flags `(,(string-append "PLATFORM_DISABLED_TESTS="
                                      " appdomain-unload.exe"
                                      " delegate2.exe"
                                      " finally_guard.exe"
                                      " remoting4.exe"))
       ;; running tests in parallel fails
       #:parallel-tests? #f))
    (synopsis "Compiler and libraries for the C# programming language")
    (description "Mono is a compiler, vm, debugger and set of libraries for
C#, a C-style programming language from Microsoft that is very similar to
Java.")
    (home-page "https://www.mono-project.com/")
    (license license:x11)))

(define-public libgdiplus
  (package
    (name "libgdiplus")
    (version "6.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://download.mono-project.com/sources/libgdiplus/libgdiplus-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1vr5l09i5i91n9qzky7ab9wwvgdidvrbw26y8llip0z4qdf4w7mq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     `(("glib" ,glib)
       ("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("libtiff" ,libtiff)
       ("libjpeg" ,libjpeg-turbo)
       ("libexif" ,libexif)
       ("libungif" ,libungif)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; TODO: See with upstream why they fail.
         ;; https://github.com/mono/mono/issues/18934
         (add-before 'configure 'remove-buggy-tests
           (lambda _
             (substitute* "tests/Makefile.in"
               (("testicocodec\\$\\(EXEEXT\\) ") " ")
               (("testfont\\$\\(EXEEXT\\) ") " "))
             #t)))))
    (home-page "https://www.mono-project.com/docs/gui/libgdiplus/")
    (synopsis "Mono library that provides a GDI+-compatible API")
    (description "Libgdiplus is the Mono library that provides a
GDI+-compatible API on non-Windows operating systems.  The implementation uses
Cairo to do most of the heavy lifting.")
    (license license:gpl3+)))
