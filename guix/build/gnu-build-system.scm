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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%standard-phases
            gnu-build))

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

(define* (set-paths #:key inputs #:allow-other-keys)
  (let ((inputs (map cdr inputs)))
    (set-path-environment-variable "PATH" '("bin") inputs)
    (set-path-environment-variable "CPATH" '("include") inputs)
    (set-path-environment-variable "LIBRARY_PATH" '("lib" "lib64") inputs)))

(define* (unpack #:key source #:allow-other-keys)
  (and (zero? (system* "tar" "xvf" source))
       (chdir (first-subdirectory "."))))

(define* (configure #:key outputs (configure-flags '()) #:allow-other-keys)
  (let* ((prefix     (assoc-ref outputs "out"))
         (libdir     (assoc-ref outputs "lib"))
         (includedir (assoc-ref outputs "include"))
         (flags      `(,(string-append "--prefix=" prefix)
                       "--enable-fast-install"    ; when using Libtool

                       ;; Produce multiple outputs when specific output names
                       ;; are recognized.
                       ,@(if libdir
                              (list (string-append "--libdir=" libdir "/lib"))
                              '())
                       ,@(if includedir
                             (list (string-append "--includedir="
                                                  includedir "/include"))
                             '())
                       ,@configure-flags)))
    (format #t "configure flags: ~s~%" flags)
    (zero? (apply system* "./configure" flags))))

(define* (build #:key (make-flags '()) #:allow-other-keys)
  (zero? (apply system* "make" make-flags)))

(define* (check #:key (make-flags '()) (tests? #t) (test-target "check")
                #:allow-other-keys)
  (if tests?
      (zero? (apply system* "make" test-target make-flags))
      (begin
        (format #t "test suite not run~%")
        #t)))

(define* (install #:key (make-flags '()) #:allow-other-keys)
  (zero? (apply system* "make" "install" make-flags)))

(define %standard-phases
  ;; Standard build phases, as a list of symbol/procedure pairs.
  (let-syntax ((phases (syntax-rules ()
                         ((_ p ...) `((p . ,p) ...)))))
    (phases set-paths unpack configure build check install)))


(define* (gnu-build #:key (source #f) (outputs #f) (inputs #f)
                    (phases %standard-phases)
                    #:allow-other-keys
                    #:rest args)
  "Build from SOURCE to OUTPUTS, using INPUTS, and by running all of PHASES
in order.  Return #t if all the PHASES succeeded, #f otherwise."
  (setvbuf (current-output-port) _IOLBF)

  ;; The trick is to #:allow-other-keys everywhere, so that each procedure in
  ;; PHASES can pick the keyword arguments it's interested in.
  (every (match-lambda
          ((name . proc)
           (format #t "starting phase `~a'~%" name)
           (apply proc args)))
         phases))
