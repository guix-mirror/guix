;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
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
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1))

(define-public gdb-9.1
  (package
    (name "gdb")
    (version "9.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gdb/gdb-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0dqp1p7w836iwijg1zb4a784n0j4pyjiw5v6h8fg5lpx6b40x7k9"))))

    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; FIXME "make check" fails on single-processor systems.

       #:out-of-source? #t

       #:modules ((srfi srfi-1)
                  ,@%gnu-build-system-modules)

       #:phases (modify-phases %standard-phases
                  (add-after
                   'configure 'post-configure
                   (lambda _
                     (for-each patch-makefile-SHELL
                               (find-files "." "Makefile\\.in"))
                     #t))
                  (add-after
                   'install 'remove-libs-already-in-binutils
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     ;; Like Binutils, GDB installs libbfd, libopcodes, etc.
                     ;; However, this leads to collisions when both are
                     ;; installed, and really is none of its business,
                     ;; conceptually.  So remove them.
                     (let* ((binutils (assoc-ref inputs "binutils"))
                            (out      (assoc-ref outputs "out"))
                            (files1   (with-directory-excursion binutils
                                        (append (find-files "lib")
                                                (find-files "include"))))
                            (files2   (with-directory-excursion out
                                        (append (find-files "lib")
                                                (find-files "include"))))
                            (common   (lset-intersection string=?
                                                         files1 files2)))
                       (with-directory-excursion out
                         (for-each delete-file common)
                         #t)))))))
    (inputs
     `(("expat" ,expat)
       ("mpfr" ,mpfr)
       ("gmp" ,gmp)
       ("readline" ,readline)
       ("ncurses" ,ncurses)
       ("guile" ,guile-2.0)
       ("python" ,python)
       ("python-wrapper" ,python-wrapper)
       ("source-highlight" ,source-highlight)

       ;; Allow use of XML-formatted syscall information.  This enables 'catch
       ;; syscall' and similar commands.
       ("libxml2" ,libxml2)))
    (native-inputs
      `(("texinfo" ,texinfo)
        ("dejagnu" ,dejagnu)
        ("pkg-config" ,pkg-config)))
    (home-page "https://www.gnu.org/software/gdb/")
    (synopsis "The GNU debugger")
    (description
     "GDB is the GNU debugger.  With it, you can monitor what a program is
doing while it runs or what it was doing just before a crash.  It allows you
to specify the runtime conditions, to define breakpoints, and to change how
the program is running to try to fix bugs.  It can be used to debug programs
written in C, C++, Ada, Objective-C, Pascal and more.")
    (license gpl3+)))

;; This version of GDB is required by some of the Rust compilers, see
;; <https://bugs.gnu.org/37810>.
(define-public gdb-8.2
  (package/inherit
   gdb-9.1
   (version "8.2.1")
   (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gdb/gdb-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "00i27xqawjv282a07i73lp1l02n0a3ywzhykma75qg500wll6sha"))))))

(define-public gdb
  ;; This is the fixed version that packages depend on.  Update it rarely
  ;; enough to avoid massive rebuilds.
  gdb-9.1)
