;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages slang)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pcre))

(define-public slang
  (package
    (name "slang")
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.jedsoft.org/releases/slang/slang-"
                    version
                    ".tar.gz"))
              (sha256
               (base32
                "0aqd2cjabj6nhd4r3dc4vhqif2bf3dmqnrn2gj0xm4gqyfd177jy"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "src/Makefile.in"
                    (("/bin/ln") "ln"))
                  (substitute* "configure"
                    (("-ltermcap") ""))))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-tests? #f))
    (inputs
     `(("readline" ,readline)
       ("zlib" ,zlib)
       ("libpng" ,libpng)
       ("pcre" ,pcre)
       ("ncurses" ,ncurses)))
    (home-page "http://www.jedsoft.org/slang/")
    (synopsis "Library for interactive applications and extensibility")
    (description
     "S-Lang is a multi-platform programmer's library designed to allow a
developer to create robust multi-platform software. It provides facilities
required by interactive applications such as display/screen management,
keyboard input, keymaps, and so on. The most exciting feature of the library
is the slang interpreter that may be easily embedded into a program to make it
extensible. While the emphasis has always been on the embedded nature of the
interpreter, it may also be used in a stand-alone fashion through the use of
slsh, which is part of the S-Lang distribution.")
    (license license:gpl2+)))
