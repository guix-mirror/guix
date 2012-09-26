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
            %patch-directory
            find-packages-by-name))

;;; Commentary:
;;;
;;; General utilities for the software distribution---i.e., the modules under
;;; (distro ...).
;;;
;;; Code:

(define _ (cut gettext <> "guix"))

(define %patch-directory
  (make-parameter
   (or (getenv "DISTRO_PATCH_DIRECTORY")
       (compile-time-value (getenv "DISTRO_INSTALLED_PATCH_DIRECTORY")))))

(define (search-patch file-name)
  "Search the patch FILE-NAME."
  (search-path (list (%patch-directory)) file-name))

(define %distro-module-directory
  ;; Absolute path of the (distro ...) module root.
  (string-append (dirname (search-path %load-path "distro.scm"))
                 "/distro/packages"))

(define (package-files)
  "Return the list of files that implement distro modules."
  (define prefix-len
    (string-length (dirname %distro-module-directory)))

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
                              (_ "warning: cannot access `~a': ~a")
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

(define* (find-packages-by-name name #:optional version)
  "Return the list of packages with the given NAME.  If VERSION is not #f,
then only return packages whose version is equal to VERSION."
  (define right-package?
    (if version
        (lambda (p)
          (and (package? p)
               (string=? (package-name p) name)
               (string=? (package-version p) version)))
        (lambda (p)
          (and (package? p)
               (string=? (package-name p) name)))))

  (append-map (lambda (module)
                (filter right-package?
                        (module-map (lambda (sym var)
                                      (variable-ref var))
                                    module)))
              (package-modules)))
