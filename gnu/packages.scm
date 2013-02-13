;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-39)
  #:export (search-patch
            search-bootstrap-binary
            %patch-directory
            %bootstrap-binaries-path
            fold-packages
            find-packages-by-name))

;;; Commentary:
;;;
;;; General utilities for the software distribution---i.e., the modules under
;;; (gnu packages ...).
;;;
;;; Code:

(define _ (cut gettext <> "guix"))

;; By default, we store patches and bootstrap binaries alongside Guile
;; modules.  This is so that these extra files can be found without
;; requiring a special setup, such as a specific installation directory
;; and an extra environment variable.  One advantage of this setup is
;; that everything just works in an auto-compilation setting.

(define %patch-path
  (make-parameter
   (map (cut string-append <>  "/gnu/packages/patches")
        %load-path)))

(define %bootstrap-binaries-path
  (make-parameter
   (map (cut string-append <> "/gnu/packages/bootstrap")
        %load-path)))

(define (search-patch file-name)
  "Search the patch FILE-NAME."
  (search-path (%patch-path) file-name))

(define (search-bootstrap-binary file-name system)
  "Search the bootstrap binary FILE-NAME for SYSTEM."
  (search-path (%bootstrap-binaries-path)
               (string-append system "/" file-name)))

(define %distro-module-directory
  ;; Absolute path of the (gnu packages ...) module root.
  (string-append (dirname (search-path %load-path "gnu/packages.scm"))
                 "/packages"))

(define (package-files)
  "Return the list of files that implement distro modules."
  (define prefix-len
    (string-length
     (dirname (dirname (search-path %load-path "gnu/packages.scm")))))

  (file-system-fold (const #t)                    ; enter?
                    (lambda (path stat result)    ; leaf
                      (if (string-suffix? ".scm" path)
                          (cons (substring path prefix-len) result)
                          result))
                    (lambda (path stat result)    ; down
                      result)
                    (lambda (path stat result)    ; up
                      result)
                    (const #f)                    ; skip
                    (lambda (path stat errno result)
                      (format (current-error-port)
                              (_ "warning: cannot access `~a': ~a~%")
                              path (strerror errno))
                      result)
                    '()
                    %distro-module-directory
                    stat))

(define (package-modules)
  "Return the list of modules that provide packages for the distribution."
  (define not-slash
    (char-set-complement (char-set #\/)))

  (filter-map (lambda (path)
                (let ((name (map string->symbol
                                 (string-tokenize (string-drop-right path 4)
                                                  not-slash))))
                  (false-if-exception (resolve-interface name))))
              (package-files)))

(define (fold2 f seed1 seed2 lst)
  (if (null? lst)
      (values seed1 seed2)
      (call-with-values
          (lambda () (f (car lst) seed1 seed2))
        (lambda (seed1 seed2)
          (fold2 f seed1 seed2 (cdr lst))))))

(define (fold-packages proc init)
  "Call (PROC PACKAGE RESULT) for each available package, using INIT as
the initial value of RESULT.  It is guaranteed to never traverse the
same package twice."
  (identity   ; discard second return value
   (fold2 (lambda (module result seen)
            (fold2 (lambda (var result seen)
                     (if (and (package? var)
                              (not (vhash-assq var seen)))
                         (values (proc var result)
                                 (vhash-consq var #t seen))
                         (values result seen)))
                   result
                   seen
                   (module-map (lambda (sym var)
                                 (false-if-exception (variable-ref var)))
                               module)))
          init
          vlist-null
          (package-modules))))

(define* (find-packages-by-name name #:optional version)
  "Return the list of packages with the given NAME.  If VERSION is not #f,
then only return packages whose version is equal to VERSION."
  (define right-package?
    (if version
        (lambda (p)
          (and (string=? (package-name p) name)
               (string=? (package-version p) version)))
        (lambda (p)
          (string=? (package-name p) name))))

  (fold-packages (lambda (package result)
                   (if (right-package? package)
                       (cons package result)
                       result))
                 '()))
