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
  #:use-module (ice-9 match))

(define %mingw-triplet
  "i686-w64-mingw32")

(define-public mingw-w64
  (package
    (name "mingw-w64")
    (version "5.0-rc2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://sourceforge.net/projects/mingw-w64/files/mingw-w64/"
                    "mingw-w64-release/mingw-w64-v" version ".tar.bz2"))
              (sha256
               (base32 "0imdary8j07if8ih73pfgxiclpf2ax8h3mz8mxln07i8sbbd30c9"))
              (patches (search-patches "mingw-w64-5.0rc2-gcc-4.9.3.patch"))))
    (native-inputs `(("xgcc-core" ,(cross-gcc %mingw-triplet))
                     ("xbinutils" ,(cross-binutils %mingw-triplet))))
    (build-system gnu-build-system)
    (search-paths
     (list (search-path-specification
            (variable "CROSS_C_INCLUDE_PATH")
            (files '("include" "i686-w64-mingw32/include")))
           (search-path-specification
            (variable "CROSS_LIBRARY_PATH")
            (files
             '("lib" "lib64" "i686-w64-mingw32/lib" "i686-w64-mingw32/lib64")))))
    (arguments
     `(#:configure-flags '("--host=i686-w64-mingw32")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'setenv
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xgcc-core (assoc-ref inputs "xgcc-core"))
                   (mingw-headers (string-append (getcwd) "/mingw-w64-headers")))
               (setenv "CPP"
                       (string-append xgcc-core "/bin/i686-w64-mingw32-cpp"))
               (setenv "CROSS_C_INCLUDE_PATH"
                       (string-append
                        mingw-headers
                        ":" mingw-headers "/include"
                        ":" mingw-headers "/crt"
                        ":" mingw-headers "/defaults/include"
                        ":" mingw-headers "/direct-x/include"))))))
       #:make-flags (list "DEFS=-DHAVE_CONFIG_H -D__MINGW_HAS_DXSDK=1")
       #:tests? #f ; compiles and includes glibc headers
       #:strip-binaries? #f))
    (home-page "http://mingw.org")
    (synopsis "Minimalist GNU for Windows")
    (description "MinGW provides a complete programming tool set which is
suitable for the development of native MS-Windows applications, and which does
not depend on any 3rd-party C-Runtime dlls.")
    (license license:fdl1.3+)))
