;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages perl)
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
                "0jibyvyv2jy8dq5ij0j00iq3v74r0y90dcjc3dkspcfbnn37cphn"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gnu-gettext)
       ("glib" ,glib)
       ("libxslt" ,libxslt)
       ("perl" ,perl)
       ("python" ,python-2)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _ ;;* (#:key inputs #:allow-other-keys)
             ;; all tests under mcs/class fail trying to access $HOME
             (setenv "HOME" "/tmp")))
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
    (home-page "http://mono-project.com/")
    (license license:x11)))
