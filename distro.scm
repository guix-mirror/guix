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

(define-module (distro)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-39)
  #:export (search-patch
            search-bootstrap-binary
            %patch-directory
            fold-packages
            find-packages-by-name))

;;; Commentary:
;;;
;;; General utilities for the software distribution---i.e., the modules under
;;; (distro ...).
;;;
;;; Code:

(define _ (cut gettext <> "guix"))

(define not-colon
  ;; The char set that contains all the characters but `:'.
  (char-set-complement (char-set #\:)))

(define %patch-path
  (make-parameter
   (or (and=> (getenv "DISTRO_PATCH_PATH")
              (cut string-tokenize <> not-colon))
       (compile-time-value
        (list (getenv "DISTRO_INSTALLED_PATCH_DIRECTORY"))))))

(define %bootstrap-binaries-path
  (make-parameter
   (or (and=> (getenv "DISTRO_BOOTSTRAP_PATH")
              (cut string-tokenize <> not-colon))
       (compile-time-value
        (list (getenv "DISTRO_INSTALLED_BOOTSTRAP_DIRECTORY"))))))

(define (search-patch file-name)
  "Search the patch FILE-NAME."
  (search-path (%patch-path) file-name))

(define (search-bootstrap-binary file-name system)
  "Search the bootstrap binary FILE-NAME for SYSTEM."
  (search-path (%bootstrap-binaries-path)
               (string-append system "/" file-name)))

(define %distro-module-directory
  ;; Absolute path of the (distro ...) module root.
  (string-append (dirname (search-path %load-path "distro.scm"))
                 "/distro/packages"))

(define (package-files)
  "Return the list of files that implement distro modules."
  (define prefix-len
    (string-length (dirname (search-path %load-path "distro.scm"))))

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

(define (fold-packages proc init)
  "Call (PROC PACKAGE RESULT) for each available package, using INIT as
the initial value of RESULT."
  (fold (lambda (module result)
          (fold (lambda (var result)
                  (if (package? var)
                      (proc var result)
                      result))
                result
                (module-map (lambda (sym var)
                              (false-if-exception (variable-ref var)))
                            module)))
        init
        (package-modules)))

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
