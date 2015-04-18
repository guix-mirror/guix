#!@BASH@
# -*- mode: scheme; coding: utf-8; -*-

# XXX: We have to go through Bash because there's no command-line switch to
# augment %load-compiled-path, and because of the silly 127-byte limit for
# the shebang line in Linux.
# Use `load-compiled' because `load' (and `-l') doesn't otherwise load our
# .go file (see <http://bugs.gnu.org/12519>).

main="(@ (gnu build-support ld-wrapper) ld-wrapper)"
exec @GUILE@ -c "(load-compiled \"@SELF@.go\") (apply $main (cdr (command-line)))" "$@"
!#
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu build-support ld-wrapper)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
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
  (or (getenv "NIX_STORE") "/gnu/store"))

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

(define %disable-rpath?
  ;; Whether to disable automatic '-rpath' addition.
  (getenv "GUIX_LD_WRAPPER_DISABLE_RPATH"))

(define (readlink* file)
  ;; Call 'readlink' until the result is not a symlink.
  (define %max-symlink-depth 50)

  (let loop ((file  file)
             (depth 0))
    (catch 'system-error
      (lambda ()
        (if (>= depth %max-symlink-depth)
            file
            (loop (readlink file) (+ depth 1))))
      (lambda args
        (if (= EINVAL (system-error-errno args))
            file
            (apply throw args))))))

(define (dereference-symlinks file)
  ;; Same as 'readlink*' but return FILE if the symlink target is invalid or
  ;; FILE does not exist.
  (catch 'system-error
    (lambda ()
      ;; When used from a user environment, FILE may refer to
      ;; ~/.guix-profile/lib/libfoo.so, which is itself a symlink to the
      ;; store.  Check whether this is the case.
      (readlink* file))
    (lambda args
      (if (= ENOENT (system-error-errno args))
          file
          (apply throw args)))))

(define (pure-file-name? file)
  ;; Return #t when FILE is the name of a file either within the store
  ;; (possibly via a symlink) or within the build directory.
  (let ((file (dereference-symlinks file)))
    (or (not (string-prefix? "/" file))
        (string-prefix? %store-directory file)
        (string-prefix? %temporary-directory file)
        (and %build-directory
             (string-prefix? %build-directory file)))))

(define (store-file-name? file)
  ;; Return #t when FILE is a store file, possibly indirectly.
  (string-prefix? %store-directory (dereference-symlinks file)))

(define (shared-library? file)
  ;; Return #t when FILE denotes a shared library.
  (or (string-suffix? ".so" file)
      (let ((index (string-contains file ".so.")))
        ;; Since we cannot use regexps during bootstrap, roll our own.
        (and index
             (string-every (char-set-union (char-set #\.) char-set:digit)
                           (string-drop file (+ index 3)))))))

(define (library-files-linked args)
  ;; Return the file names of shared libraries explicitly linked against via
  ;; `-l' or with an absolute file name in ARGS.
  (define path+files
    (fold (lambda (argument result)
            (match result
              ((library-path . library-files)
               (cond ((string-prefix? "-L" argument) ;augment the search path
                      (cons (append library-path
                                    (list (string-drop argument 2)))
                            library-files))
                     ((string-prefix? "-l" argument) ;add library
                      (let* ((lib  (string-append "lib"
                                                  (string-drop argument 2)
                                                  ".so"))
                             (full (search-path library-path lib)))
                        (if full
                            (cons library-path
                                  (cons full library-files))
                            result)))
                     ((and (string-prefix? %store-directory argument)
                           (shared-library? argument)) ;add library
                      (cons library-path
                            (cons argument library-files)))
                     (else
                      result)))))
          (cons '() '())
          args))

  (match path+files
    ((path . files)
     (reverse files))))

(define (rpath-arguments library-files)
  ;; Return the `-rpath' argument list for each of LIBRARY-FILES, a list of
  ;; absolute file names.
  (fold-right (lambda (file args)
                ;; Add '-rpath' if and only if FILE is in the store; we don't
                ;; want to add '-rpath' for files under %BUILD-DIRECTORY or
                ;; %TEMPORARY-DIRECTORY because that could leak to installed
                ;; files.
                (cond ((and (not %disable-rpath?)
                            (store-file-name? file))
                       (cons* "-rpath" (dirname file) args))
                      ((or %allow-impurities?
                           (pure-file-name? file))
                       args)
                      (else
                       (begin
                         (format (current-error-port)
                                 "ld-wrapper: error: attempt to use \
impure library ~s~%"
                                 file)
                         (exit 1)))))
              '()
              library-files))

(define (ld-wrapper . args)
  ;; Invoke the real `ld' with ARGS, augmented with `-rpath' switches.
  (let* ((libs (library-files-linked args))
         (args (append args (rpath-arguments libs))))
    (when %debug?
      (format (current-error-port)
              "ld-wrapper: invoking `~a' with ~s~%"
              %real-ld args))
    (apply execl %real-ld (basename %real-ld) args)))

;;; ld-wrapper.scm ends here
