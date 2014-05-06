;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages gdb)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages python)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public gdb
  (package
    (name "gdb")
    (version "7.7.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gdb/gdb-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "199sn1p0gzli6icp9dcvrphdvyi7hm4cc9zhziq0q6vg81h55g8d"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; FIXME "make check" fails on single-processor systems.
       #:phases (alist-cons-after
                 'configure 'post-configure
                 (lambda _
                   (for-each patch-makefile-SHELL
                             (find-files "." "Makefile\\.in")))
                 %standard-phases)))
    (inputs
     `(("expat" ,expat)
       ("mpfr" ,mpfr)
       ("gmp" ,gmp)
       ("readline" ,readline)
       ("ncurses" ,ncurses)
       ("python" ,python-wrapper)
       ("dejagnu" ,dejagnu)

       ;; Allow use of XML-formatted syscall information.  This enables 'catch
       ;; syscall' and similar commands.
       ("libxml2" ,libxml2)))
    (native-inputs
      `(("texinfo" ,texinfo)))
    (home-page "http://www.gnu.org/software/gdb/")
    (synopsis "The GNU debugger")
    (description
     "GDB is the GNU debugger.  With it, you can monitor what a program is
doing while it runs or what it was doing just before a crash.  It allows you
to specify the runtime conditions, to define breakpoints, and to change how
the program is running to try to fix bugs.  It can be used to debug programs
written in C, C++, Ada, Objective-C, Pascal and more.")
    (license gpl3+)))
