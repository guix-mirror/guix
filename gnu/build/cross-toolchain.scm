;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu build cross-toolchain)
  #:use-module (guix build utils)
  #:use-module (guix build gnu-build-system)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:export (cross-gcc-build-phases))

;;; Commentary:
;;;
;;; This module provides tools to build a cross-compiler.
;;;
;;; Code:

(define %gcc-include-paths
  ;; Environment variables for header search paths.
  ;; Note: See <http://bugs.gnu.org/30756> for why not 'C_INCLUDE_PATH' & co.
  '("CPATH"))

(define %gcc-cross-include-paths
  ;; Search path for target headers when cross-compiling.
  (map (cut string-append "CROSS_" <>) %gcc-include-paths))

(define* (make-cross-binutils-visible #:key outputs inputs target
                                      #:allow-other-keys)
  "Create symlinks for 'as', 'nm', and 'ld' in the \"out\" output, under
libexec/gcc, so that the cross-GCC can find them."
  (let* ((out      (assoc-ref outputs "out"))
         (libexec  (string-append out "/libexec/gcc/" target))
         (binutils (string-append (assoc-ref inputs "binutils-cross")
                                  "/bin/" target "-"))
         (wrapper  (string-append (assoc-ref inputs "ld-wrapper-cross")
                                  "/bin/" target "-ld")))
    (for-each (lambda (file)
                (symlink (string-append binutils file)
                         (string-append libexec "/" file)))
              '("as" "nm"))
    (symlink wrapper (string-append libexec "/ld"))
    #t))

(define* (set-cross-path #:key inputs #:allow-other-keys)
  "Add the cross kernel headers to CROSS_CPATH, and remove them from
C_INCLUDE_PATH et al."
  (match (assoc-ref inputs "libc")
    ((? string? libc)
     (let ((kernel (assoc-ref inputs "xkernel-headers")))
       (define (cross? x)
         ;; Return #t if X is a cross-libc or cross Linux.
         (or (string-prefix? libc x)
             (string-prefix? kernel x)))

       (let ((cpath (string-append libc "/include"
                                   ":" kernel "/include")))
         (for-each (cut setenv <> cpath)
                   %gcc-cross-include-paths))

       (setenv "CROSS_LIBRARY_PATH"
               (string-append libc "/lib:" kernel "/lib")) ;for Hurd's libihash

       (for-each (lambda (var)
                   (and=> (getenv var)
                          (lambda (value)
                            (let* ((path (search-path-as-string->list value))
                                   (native-path (list->search-path-as-string
                                                 (remove cross? path) ":")))
                              (setenv var native-path)))))
                 (cons "LIBRARY_PATH" %gcc-include-paths))
       #t))
    (#f
     ;; We're building the sans-libc cross-compiler, so nothing to do.
     #t)))

(define* (set-cross-path/mingw #:key inputs #:allow-other-keys)
  "Add the cross MinGW headers to CROSS_C_*_INCLUDE_PATH, and remove them from
C_*INCLUDE_PATH."
  (let ((libc (assoc-ref inputs "libc"))
        (gcc (assoc-ref inputs "gcc")))
    (define (cross? x)
      (and libc (string-prefix? libc x)))

    (define (unpacked-mingw-dir)
      (match (scandir "." (lambda (name)
                            (string-contains name "mingw-w64")))
        ((mingw-dir)
         (string-append
          (getcwd) "/" mingw-dir "/mingw-w64-headers"))))

    (if libc
        (let ((cpath (string-append libc "/include"
                                    ":" libc "/i686-w64-mingw32/include")))
          (for-each (cut setenv <> cpath)
                    %gcc-cross-include-paths))

        ;; libc is false, so we are building xgcc-sans-libc.
        ;; Add essential headers from mingw-w64.
        (let ((mingw-source (assoc-ref inputs "mingw-source")))
          (invoke "tar" "xvf" mingw-source)
          (let ((mingw-headers (unpacked-mingw-dir)))
            ;; We need _mingw.h which will gets built from _mingw.h.in by
            ;; mingw-w64's configure.  We cannot configure mingw-w64 until we
            ;; have xgcc-sans-libc; substitute to the rescue.
            (copy-file (string-append mingw-headers "/crt/_mingw.h.in")
                       (string-append mingw-headers "/crt/_mingw.h"))

            (substitute* (string-append mingw-headers "/crt/_mingw.h")
              (("@MINGW_HAS_SECURE_API@")
               "#define MINGW_HAS_SECURE_API 1"))

            (let ((cpath (string-append mingw-headers "/include"
                                        ":" mingw-headers "/crt"
                                        ":" mingw-headers
                                        "/defaults/include")))
              (for-each (cut setenv <> cpath)
                        (cons "CROSS_LIBRARY_PATH"
                              %gcc-cross-include-paths))))))

    (when libc
      (setenv "CROSS_LIBRARY_PATH"
              (string-append libc "/lib"
                             ":" libc "/i686-w64-mingw32/lib")))

    (setenv "CPP" (string-append gcc "/bin/cpp"))
    (for-each (lambda (var)
                (and=> (getenv var)
                       (lambda (value)
                         (let* ((path (search-path-as-string->list
                                       value))
                                (native-path (list->search-path-as-string
                                              (remove cross? path) ":")))
                           (setenv var native-path)))))
              (cons "LIBRARY_PATH" %gcc-include-paths))
    #t))

(define (install-strip . _)
  "Install a stripped GCC."
  ;; Unlike our 'strip' phase, this will do the right thing for
  ;; cross-compilers.
  (invoke "make" "install-strip"))

(define* (cross-gcc-build-phases target
                                 #:optional (phases %standard-phases))
  "Modify PHASES to include everything needed to build a cross-GCC for TARGET,
a target triplet."
  (modify-phases phases
    (add-before 'configure 'set-cross-path
      (if (string-contains target "mingw")
          set-cross-path/mingw
          set-cross-path))
    (add-after 'install 'make-cross-binutils-visible
      (cut make-cross-binutils-visible #:target target <...>))
    (replace 'install install-strip)))

;;; cross-toolchain.scm ends here
