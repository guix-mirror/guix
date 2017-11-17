;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module (gnu packages ruby)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby))

(define-public libffi
  (let ((post-install-phase
         ;; Keep headers where libffi.pc expects them, but also make them
         ;; available in $includedir where some users expect them.
         '(lambda* (#:key outputs #:allow-other-keys)
            (define out (assoc-ref outputs "out"))
            (symlink (string-append out "/lib/libffi-3.2.1/include")
                     (string-append out "/include")))))
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
    (arguments `(#:phases (alist-cons-after 'install 'post-install
                                            ,post-install-phase
                                            %standard-phases)))
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
    (license expat))))

(define-public python-cffi
  (package
    (name "python-cffi")
    (version "1.11.2")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "cffi" version))
      (sha256
       (base32 "19h0wwz9cww74gw8cyq0izj8zkhjyzjw2d3ks1c3f1y4q28xv1xb"))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (inputs
     `(("libffi" ,libffi)))
    (propagated-inputs ; required at run-time
     `(("python-pycparser" ,python-pycparser)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)
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
             (zero? (system* "py.test" "-v" "c/" "testing/"))))
         (add-before 'check 'disable-failing-test
           ;; This is assumed to be a libffi issue:
           ;; https://bitbucket.org/cffi/cffi/issues/312/tests-failed-with-armv8
           (lambda _
             (substitute* "testing/cffi0/test_ownlib.py"
               (("ret.left") "ownlib.left"))
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                    (doc (string-append data "/doc/" ,name "-" ,version))
                    (html (string-append doc "/html")))
               (with-directory-excursion "doc"
                 (system* "make" "html")
                 (mkdir-p html)
                 (copy-recursively "build/html" html))
               (copy-file "LICENSE" (string-append doc "/LICENSE"))
               #t))))))
    (home-page "https://cffi.readthedocs.org")
    (synopsis "Foreign function interface for Python")
    (description
     "Foreign Function Interface for Python calling C code.")
    (license expat)))

(define-public python2-cffi
  (package-with-python2 python-cffi))

(define-public ruby-ffi
  (package
    (name "ruby-ffi")
    (version "1.9.18")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ffi" version))
              (sha256
               (base32
                "034f52xf7zcqgbvwbl20jwdyjwznvqnwpbaps9nk18v9lgb1dpx0"))))
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
