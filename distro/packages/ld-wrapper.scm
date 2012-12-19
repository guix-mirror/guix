#!@BASH@
# -*- mode: scheme; coding: utf-8; -*-

# XXX: We have to go through Bash because there's no command-line switch to
# augment %load-compiled-path, and because of the silly 127-byte limit for
# the shebang line in Linux.
# Use `load-compiled' because `load' (and `-l') doesn't otherwise load our
# .go file (see <http://bugs.gnu.org/12519>).

main="(@ (gnu build-support ld-wrapper) ld-wrapper)"
exec @GUILE@ -c "(load-compiled \"$0.go\") (apply $main (cdr (command-line)))" "$@"
!#
;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu build-support ld-wrapper)
  #:use-module (srfi srfi-1)
  #:export (ld-wrapper))

;;; Commentary:
;;;
;;; This is a wrapper for the linker.  Its purpose is to inspect the -L and
;;; -l switches passed to the linker, add corresponding -rpath arguments, and
;;; invoke the actual linker with this new set of arguments.
;;;
;;; The alternatives to this hack would be:
;;;
;;;   1. Using $LD_RUN_PATH.  However, that would tend to include more than
;;;      needed in the RPATH; for instance, given a package with `libfoo' as
;;;      an input, all its binaries would have libfoo in their RPATH,
;;;      regardless of whether they actually NEED it.
;;;
;;;   2. Use a GCC "lib" spec string such as `%{L*:-rpath %*}', which adds a
;;;      `-rpath LIBDIR' argument for each occurrence of `-L LIBDIR'.
;;;      However, this doesn't work when $LIBRARY_PATH is used, because the
;;;      additional `-L' switches are not matched by the above rule, because
;;;      the rule only matches explicit user-provided switches.  See
;;;      <http://gcc.gnu.org/ml/gcc-help/2012-09/msg00110.html> for details.
;;;
;;; As a bonus, this wrapper checks for "impurities"--i.e., references to
;;; libraries outside the store.
;;;
;;; Code:

(define %real-ld
  ;; Name of the linker that we wrap.
  "@LD@")

(define %store-directory
  ;; File name of the store.
  (or (getenv "NIX_STORE") "/nix/store"))

(define %temporary-directory
  ;; Temporary directory.
  (or (getenv "TMPDIR") "/tmp"))

(define %build-directory
  ;; Top build directory when run from a builder.
  (getenv "NIX_BUILD_TOP"))

(define %allow-impurities?
  ;; Whether to allow references to libraries outside the store.
  (getenv "GUIX_LD_WRAPPER_ALLOW_IMPURITIES"))

(define %debug?
  ;; Whether to emit debugging output.
  (getenv "GUIX_LD_WRAPPER_DEBUG"))

(define (pure-file-name? file)
  ;; Return #t when FILE is the name of a file either within the store or
  ;; within the build directory.
  (or (not (string-prefix? "/" file))
      (string-prefix? %store-directory file)
      (string-prefix? %temporary-directory file)
      (and %build-directory
           (string-prefix? %build-directory file))))

(define (switch-arguments switch args)
  ;; Return the arguments passed for the occurrences of SWITCH--e.g.,
  ;; "-L"--in ARGS.
  (let ((prefix-len (string-length switch)))
    (fold-right (lambda (arg path)
                  (if (string-prefix? switch arg)
                      (cons (substring arg prefix-len) path)
                      path))
                '()
                args)))

(define (library-path args)
  ;; Return the library search path extracted from `-L' switches in ARGS.
  ;; Note: allow references to out-of-store directories.  When this leads to
  ;; actual impurities, this is caught later.
  (switch-arguments "-L" args))

(define (library-files-linked args)
  ;; Return the file names of shared libraries explicitly linked against via
  ;; `-l' in ARGS.
  (map (lambda (lib)
         (string-append "lib" lib ".so"))
       (switch-arguments "-l" args)))

(define (rpath-arguments lib-path library-files)
  ;; Return the `-rpath' argument list for each of LIBRARY-FILES found in
  ;; LIB-PATH.
  (fold-right (lambda (file args)
                (let ((absolute (search-path lib-path file)))
                  (if absolute
                      (if (or %allow-impurities?
                              (pure-file-name? absolute))
                          (cons* "-rpath" (dirname absolute)
                                 args)
                          (begin
                            (format (current-error-port)
                                    "ld-wrapper: error: attempt to use impure library ~s~%"
                                    absolute)
                            (exit 1)))
                      args)))
              '()
              library-files))

(define (ld-wrapper . args)
  ;; Invoke the real `ld' with ARGS, augmented with `-rpath' switches.
  (let* ((lib-path (library-path args))
         (libs     (library-files-linked args))
         (args     (append args (rpath-arguments lib-path libs))))
    (if %debug?
        (format (current-error-port)
                "ld-wrapper: invoking `~a' with ~s~%"
                %real-ld args))
    (apply execl %real-ld (basename %real-ld) args)))

;;; ld-wrapper.scm ends here
