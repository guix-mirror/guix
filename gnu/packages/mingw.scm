;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Carl Dong <contact@carldong.me>
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

(define-module (gnu packages mingw)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages multiprecision)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:export (make-mingw-w64))

(define* (make-mingw-w64 machine
                         #:key
                         xgcc
                         xbinutils
                         with-winpthreads?)
  "Return a mingw-w64 for targeting MACHINE.  If XGCC or XBINUTILS is specified,
use that gcc or binutils when cross-compiling.  If WITH-WINPTHREADS? is
specified, recurse and return a mingw-w64 with support for winpthreads."
  (let* ((triplet (string-append machine "-" "w64-mingw32")))
    (package
      (name (string-append "mingw-w64" "-" machine
                           (if with-winpthreads? "-winpthreads" "")))
      (version "7.0.0")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://sourceforge.net/projects/mingw-w64/files/mingw-w64/"
                      "mingw-w64-release/mingw-w64-v" version ".tar.bz2"))
                (sha256
                 (base32 "0a5njsa2zw2ssdz10jkb10mhrf3cb8qp9avs89zqmw4n6pzxy85a"))
                (patches
                 (search-patches "mingw-w64-6.0.0-gcc.patch"
                                 "mingw-w64-dlltool-temp-prefix.patch"
                                 "mingw-w64-reproducible-gendef.patch"))))
      (native-inputs `(("xgcc-core" ,(if xgcc xgcc (cross-gcc triplet)))
                       ("xbinutils" ,(if xbinutils xbinutils (cross-binutils triplet)))
                       ,@(if with-winpthreads?
                             `(("xlibc" ,(make-mingw-w64 machine
                                                         #:xgcc xgcc
                                                         #:xbinutils xbinutils)))
                             '())))
      (build-system gnu-build-system)
      (search-paths
       (list (search-path-specification
              (variable "CROSS_C_INCLUDE_PATH")
              (files `("include" ,(string-append triplet "/include"))))
             (search-path-specification
              (variable "CROSS_LIBRARY_PATH")
              (files
               `("lib" "lib64"
                 ,(string-append triplet "/lib")
                 ,(string-append triplet "/lib64"))))))
      (arguments
       `(#:configure-flags '(,(string-append "--host=" triplet)
                             ,@(if with-winpthreads?
                                   '("--with-libraries=winpthreads")
                                   '()))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'setenv
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((xgcc-core (assoc-ref inputs "xgcc-core"))
                     (mingw-headers (string-append (getcwd) "/mingw-w64-headers")))
                 (setenv "CPP"
                         (string-append xgcc-core ,(string-append "/bin/" triplet "-cpp")))
                 (setenv "CROSS_C_INCLUDE_PATH"
                         (string-append
                          mingw-headers
                          ":" mingw-headers "/include"
                          ":" mingw-headers "/crt"
                          ":" mingw-headers "/defaults/include"
                          ":" mingw-headers "/direct-x/include"))
                 (when ,with-winpthreads?
                     (let ((xlibc (assoc-ref inputs "xlibc")))
                       (setenv "CROSS_LIBRARY_PATH"
                               (string-append
                                xlibc "/lib" ":"
                                xlibc "/" ,triplet "/lib"))))))))
         #:make-flags (list "DEFS=-DHAVE_CONFIG_H -D__MINGW_HAS_DXSDK=1")
         #:tests? #f ; compiles and includes glibc headers
         #:strip-binaries? #f))
      (home-page "https://mingw-w64.org")
      (synopsis "Minimalist GNU for Windows")
      (description
       "Minimalist GNU for Windows (@dfn{MinGW}) is a complete software
development environment for creating native Microsoft Windows applications.

It includes a set of Windows-specific header files and static import libraries
which enable the use of the Windows API.  It does not rely on any third-party C
runtime dynamic-link libraries (@dfn{DLL}s).

Mingw-w64 is an advancement of the original mingw.org project and provides
several new APIs such as DirectX and DDK, and 64-bit support.")
      (license license:fdl1.3+))))

(define-public mingw-w64-i686
  (make-mingw-w64 "i686"))

(define-public mingw-w64-x86_64
  (make-mingw-w64 "x86_64"))

(define-public mingw-w64-i686-winpthreads
  (make-mingw-w64 "i686"
                  #:with-winpthreads? #t))

(define-public mingw-w64-x86_64-winpthreads
  (make-mingw-w64 "x86_64"
                  #:with-winpthreads? #t))

(define-public mingw-w64 mingw-w64-i686)
