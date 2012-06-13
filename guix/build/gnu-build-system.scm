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

(define-module (guix build gnu-build-system)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:export (gnu-build))

;; Commentary:
;;
;; Standard build procedure for packages using the GNU Build System or
;; something compatible ("./configure && make && make install").  This is the
;; builder-side code.
;;
;; Code:

(define (first-subdirectory dir)
  "Return the path of the first sub-directory of DIR."
  (file-system-fold (lambda (path stat result)
                      (string=? path dir))
                    (lambda (path stat result) result) ; leaf
                    (lambda (path stat result) result) ; down
                    (lambda (path stat result) result) ; up
                    (lambda (path stat result)         ; skip
                      (or result path))
                    (lambda (path stat errno result)   ; error
                      (error "first-subdirectory" (strerror errno)))
                    #f
                    dir))

(define (unpack source)
  (system* "tar" "xvf" source)
  (chdir (first-subdirectory ".")))

(define (configure outputs flags)
  (let ((prefix     (assoc-ref outputs "out"))
        (libdir     (assoc-ref outputs "lib"))
        (includedir (assoc-ref outputs "include")))
   (apply system* "./configure"
          "--enable-fast-install"
          (string-append "--prefix=" prefix)
          `(,@(if libdir
                  (list (string-append "--libdir=" libdir))
                  '())
            ,@(if includedir
                  (list (string-append "--includedir=" includedir))
                  '())
            ,@flags))))

(define* (gnu-build source outputs inputs
                    #:key (configure-flags '()))
  "Build from SOURCE to OUTPUTS, using INPUTS."
  (let ((inputs (map cdr inputs)))
    (set-path-environment-variable "PATH" '("bin") inputs)
    (set-path-environment-variable "CPATH" '("include") inputs)
    (set-path-environment-variable "LIBRARY_PATH" '("lib" "lib64") inputs))
  (pk (getenv "PATH"))
  (pk 'inputs inputs)
  (system* "ls" "/nix/store")
  (unpack source)
  (configure outputs configure-flags)
  (system* "make")
  (system* "make" "check")
  (system* "make" "install"))
