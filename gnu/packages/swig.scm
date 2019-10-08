;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages swig)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl))

(define-public swig
  (package
    (name "swig")
    (version "3.0.12")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/" name "/" name "/"
                                 name "-" version "/"
                                 name "-" version ".tar.gz"))
             (patches (search-patches "swig-guile-gc.patch"))
             (sha256
              (base32
               "0kf99ygrjs5616gsqhz1l7bib3a12izmxi7g48bwblbymr3z9ybw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           ;; Required since Perl 5.26.0's removal of the current
           ;; working directory from @INC.
           ;; TODO Try removing this for later versions of SWIG.
           (lambda _ (setenv "PERL_USE_UNSAFE_INC" "1") #t))
         (add-before 'configure 'workaround-gcc-bug
           (lambda _
             ;; XXX: Don't add the -isystem flag, or GCCs #include_next
             ;; won't be able to find <stdlib.h>.
             (substitute* "configure"
               (("-isystem ") "-I"))
             #t)))))
    (native-inputs `(("boost" ,boost)
                     ("pcre" ,pcre "bin")))       ;for 'pcre-config'
    (inputs `(;; Provide these to run the corresponding tests.
              ("guile" ,guile-2.0)
              ("perl" ,perl)))
              ;; FIXME: reactivate input python as soon as the test failures
              ;;   fatal error: Python.h: No such file or directory
              ;;   # include <Python.h>
              ;; are fixed.
              ;; The python part probably never worked and does not seem to
              ;; be needed for currently dependent packages.
;;               ("python" ,python-wrapper)))
    (home-page "http://swig.org/")
    (synopsis
     "Interface compiler that connects C/C++ code to higher-level languages")
    (description
     "SWIG is an interface compiler that connects programs written in C and
C++ with languages such as Perl, Python, Ruby, Scheme, and Tcl.  It works by
taking the declarations found in C/C++ header files and using them to generate
the wrapper code that scripting languages need to access the underlying C/C++
code.  In addition, SWIG provides a variety of customization features that let
you tailor the wrapping process to suit your application.")

    ;; See http://www.swig.org/Release/LICENSE for details.
    (license gpl3+)))
