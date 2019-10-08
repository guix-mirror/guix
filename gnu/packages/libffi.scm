;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2019 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages libffi)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sphinx)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby))

(define-public libffi
  (package
    (name "libffi")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "ftp://sourceware.org/pub/libffi/"
                              name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dya49bnhianl0r65m65xndz6ls2jn1xngyn72gd28ls3n7bnvnh"))
              (patches (search-patches "libffi-3.2.1-complex-alpha.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Prevent the build system from passing -march and -mtune to the
       ;; compiler.  See "ax_cc_maxopt.m4" and "ax_gcc_archflag.m4".
       #:configure-flags '("--enable-portable-binary" "--without-gcc-arch")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (define out (assoc-ref outputs "out"))
             (symlink (string-append out "/lib/libffi-3.2.1/include")
                      (string-append out "/include"))
             #t)))))
    (outputs '("out" "debug"))
    (synopsis "Foreign function call interface library")
    (description
     "The libffi library provides a portable, high level programming interface
to various calling conventions.  This allows a programmer to call any
function specified by a call interface description at run-time.

FFI stands for Foreign Function Interface.  A foreign function interface is
the popular name for the interface that allows code written in one language
to call code written in another language.  The libffi library really only
provides the lowest, machine dependent layer of a fully featured foreign
function interface.  A layer must exist above libffi that handles type
conversions for values passed between the two languages.")
    (home-page "http://sources.redhat.com/libffi/")

    ;; See <https://github.com/atgreen/libffi/blob/master/LICENSE>.
    (license expat)))

(define-public python-cffi
  (package
    (name "python-cffi")
    (version "1.11.5")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "cffi" version))
      (sha256
       (base32 "1x3lrj928dcxx1k8k9gf3s4s3jwvzv8mc3kkyg1g7c3a1sc1f3z9"))
      (patches (search-patches "python-cffi-x87-stack-clean.patch"))))
    (build-system python-build-system)
    (inputs
     `(("libffi" ,libffi)))
    (propagated-inputs ; required at run-time
     `(("python-pycparser" ,python-pycparser)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-pytest" ,python-pytest)))
    (arguments
     `(#:modules ((ice-9 ftw)
                  (srfi srfi-26)
                  (guix build utils)
                  (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append
                      (getenv "PYTHONPATH")
                      ":" (getcwd) "/build/"
                      (car (scandir "build" (cut string-prefix? "lib." <>)))))

             ;; XXX The "normal" approach of setting CC and friends does
             ;; not work here.  Is this the correct way of doing things?
             (substitute* "testing/embedding/test_basic.py"
               (("c = distutils\\.ccompiler\\.new_compiler\\(\\)")
                (string-append "c = distutils.ccompiler.new_compiler();"
                               "c.set_executables(compiler='gcc',"
                               "compiler_so='gcc',linker_exe='gcc',"
                               "linker_so='gcc -shared')")))
             (substitute* "testing/cffi0/test_ownlib.py"
               (("'cc testownlib") "'gcc testownlib"))
             (invoke "py.test" "-v" "c/" "testing/")
             #t))
         (add-before 'check 'patch-paths-of-dynamically-loaded-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Shared libraries should be referred by their absolute path as
             ;; using find_library or the like with their name fail when the
             ;; resolved .so object is a linker script rather than an ELF
             ;; binary (this is a limitation of the ctype library of Python).
             (let* ((glibc (assoc-ref inputs "libc"))
                    (libm (string-append glibc "/lib/libm.so.6"))
                    (libc (string-append glibc "/lib/libc.so.6")))
               (substitute* '("testing/cffi0/test_function.py"
                              "testing/cffi0/test_parsing.py"
                              "testing/cffi0/test_unicode_literals.py"
                              "testing/cffi0/test_zdistutils.py"
                              "testing/cffi1/test_recompiler.py")
                 (("lib_m = ['\"]{1}m['\"]{1}")
                  (format #f "lib_m = '~a'" libm)))
               (substitute* '("testing/cffi0/test_verify.py"
                              "testing/cffi1/test_verify1.py")
                 (("lib_m = \\[['\"]{1}m['\"]{1}\\]")
                  (format #f "lib_m = ['~a']" libm)))
               (substitute* "c/test_c.py"
                 (("find_and_load_library\\(['\"]{1}c['\"]{1}")
                  (format #f "find_and_load_library('~a'" libc)))
               #t)))
         (add-before 'check 'disable-failing-test
           ;; This is assumed to be a libffi issue:
           ;; https://bitbucket.org/cffi/cffi/issues/312/tests-failed-with-armv8
           (lambda _
             (substitute* "testing/cffi0/test_ownlib.py"
               (("ret.left") "ownlib.left"))
             #t)))))
    (home-page "https://cffi.readthedocs.io/")
    (synopsis "Foreign function interface for Python")
    (description "Foreign Function Interface for Python calling C code.")
    (license expat)))

(define-public python2-cffi
  (package-with-python2 python-cffi))

(define-public python-cffi-documentation
  (package
    (name "python-cffi-documentation")
    (version (package-version python-cffi))
    (source (package-source python-cffi))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'chdir
                    (lambda _ (chdir "doc") #t))
                  (delete 'configure)
                  (replace 'build
                    (lambda* (#:key (make-flags '()) #:allow-other-keys)
                      (apply invoke "make" "html" make-flags)))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (copy-recursively "build/html" (string-append out "/html"))
                        #t))))))
    (native-inputs
     `(("sphinx-build" ,python-sphinx)))
    (home-page (package-home-page python-cffi))
    (synopsis "Documentation for the Python CFFI interface")
    (description
     "This package contains HTML documentation for the @code{python-cffi}
project.")
    (license (package-license python-cffi))))

(define-public ruby-ffi
  (package
    (name "ruby-ffi")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ffi" version))
              (sha256
               (base32
                "0j8pzj8raxbir5w5k6s7a042sb5k02pg0f8s4na1r5lan901j00p"))))
    (build-system ruby-build-system)
    ;; FIXME: Before running tests the build system attempts to build libffi
    ;; from sources.
    (arguments `(#:tests? #f))
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubygems-tasks" ,ruby-rubygems-tasks)))
    (inputs
     `(("libffi" ,libffi)))
    (synopsis "Ruby foreign function interface library")
    (description "Ruby-FFI is a Ruby extension for programmatically loading
dynamic libraries, binding functions within them, and calling those functions
from Ruby code.  Moreover, a Ruby-FFI extension works without changes on Ruby
and JRuby.")
    (home-page "http://wiki.github.com/ffi/ffi")
    (license bsd-3)))
