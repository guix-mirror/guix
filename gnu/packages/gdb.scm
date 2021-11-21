;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (gnu packages bash)
  #:use-module (gnu packages hurd)
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
  #:use-module (guix download)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (srfi srfi-1))

(define-public gdb-11
  (package
    (name "gdb")
    (version "11.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gdb/gdb-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "151z6d0265hv9cgx9zqqa4bd6vbp20hrljhd6bxl7lr0gd0crkyc"))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:tests? #f                      ;FIXME: 217 unexpected failures
       #:out-of-source? #t
       #:modules ((srfi srfi-1)
                  ,@%gnu-build-system-modules)
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-paths
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((sh (string-append (assoc-ref inputs "bash")
                                               "/bin/sh")))
                        (substitute* '("gdb/ser-pipe.c"
                                       "gdbsupport/pathstuff.cc")
                          (("\"/bin/sh\"")
                           (format #f "~s" sh))))))
                  ,@(if (hurd-target?)
                        '((add-after 'unpack 'patch-gdb/hurd
                            (lambda* (#:key inputs #:allow-other-keys)
                              (let ((patch (assoc-ref inputs "hurd-build.patch")))
                                (invoke "patch" "-p1" "--force" "-i" patch)))))
                        '())
                  (add-after 'configure 'post-configure
                    (lambda _
                      (for-each patch-makefile-SHELL
                                (find-files "." "Makefile\\.in"))))
                  (add-after 'install 'remove-libs-already-in-binutils
                    (lambda* (#:key native-inputs inputs outputs
                              #:allow-other-keys)
                      ;; Like Binutils, GDB installs libbfd, libopcodes, etc.
                      ;; However, this leads to collisions when both are
                      ;; installed, and really is none of its business,
                      ;; conceptually.  So remove them.
                      (let* ((binutils (or (assoc-ref inputs "binutils")
                                           (assoc-ref native-inputs "binutils")))
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
                          (for-each delete-file common))))))))
    (inputs
     `(("bash" ,bash)
       ("expat" ,expat)
       ("mpfr" ,mpfr)
       ("gmp" ,gmp)
       ("readline" ,readline)
       ("ncurses" ,ncurses)
       ("guile" ,guile-3.0)
       ("python-wrapper" ,python-wrapper)
       ("source-highlight" ,source-highlight)

       ;; Allow use of XML-formatted syscall information.  This enables 'catch
       ;; syscall' and similar commands.
       ("libxml2" ,libxml2)

       ;; The Hurd needs -lshouldbeinlibc.
       ,@(if (hurd-target?)
             `(("hurd" ,hurd)
               ("hurd-build.patch"
                ,(search-patch "gdb-fix-gnu-nat-build.patch")))
             '())))
    (native-inputs
     `(("texinfo" ,texinfo)
       ("dejagnu" ,dejagnu)
       ("pkg-config" ,pkg-config)
       ,@(if (hurd-target?)
             ;; When cross-compiling from x86_64-linux, make sure to use a
             ;; 32-bit MiG because we assume target i586-pc-gnu.
             `(("mig" ,(if (%current-target-system)
                           mig/32-bit
                           mig)))
             '())))
    ;; TODO: Add support for the GDB_DEBUG_FILE_DIRECTORY environment
    ;; variable in GDB itself instead of relying on some glue code in
    ;; the Guix-provided .gdbinit file.
    (native-search-paths (list (search-path-specification
                                (variable "GDB_DEBUG_FILE_DIRECTORY")
                                (files '("lib/debug")))))
    (home-page "https://www.gnu.org/software/gdb/")
    (synopsis "The GNU debugger")
    (description
     "GDB is the GNU debugger.  With it, you can monitor what a program is
doing while it runs or what it was doing just before a crash.  It allows you
to specify the runtime conditions, to define breakpoints, and to change how
the program is running to try to fix bugs.  It can be used to debug programs
written in C, C++, Ada, Objective-C, Pascal and more.")
    (license gpl3+)))

(define-public gdb
  ;; This is the fixed version that packages depend on.  Update it rarely
  ;; enough to avoid massive rebuilds.
  gdb-11)

(define-public gdb-minimal
  (package/inherit gdb
    (name "gdb-minimal")
    (inputs (fold alist-delete (package-inputs gdb)
                  '("libxml2" "ncurses" "python-wrapper" "source-highlight")))))
