;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (guix build-system minetest)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system)
  #:use-module (guix utils)
  #:export (minetest-mod-build-system))

;;
;; Build procedure for minetest mods.  This is implemented as an extension
;; of ‘copy-build-system’.
;;
;; Code:

;; Lazily resolve the bindings to avoid circular dependencies.
(define (default-optipng)
  ;; Lazily resolve the binding to avoid a circular dependency.
  (module-ref (resolve-interface '(gnu packages image)) 'optipng))

(define (default-minetest)
  (module-ref (resolve-interface '(gnu packages minetest)) 'minetest))

(define (default-xvfb-run)
  (module-ref (resolve-interface '(gnu packages xorg)) 'xvfb-run))

(define %minetest-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build minetest-build-system)
    ,@%copy-build-system-modules))

(define %default-modules
  ;; Modules in scope in the build-side environment.
  '((guix build gnu-build-system)
    (guix build minetest-build-system)
    (guix build utils)))

(define (standard-minetest-packages)
  "Return the list of (NAME PACKAGE OUTPUT) or (NAME PACKAGE) tuples of
standard packages used as implicit inputs of the Minetest build system."
  `(("xvfb-run" ,(default-xvfb-run))
    ("optipng" ,(default-optipng))
    ("minetest" ,(default-minetest))
    ,@(filter (lambda (input)
                (member (car input)
                        '("libc" "tar" "gzip" "bzip2" "xz" "locales")))
              (standard-packages))))

(define* (lower-mod name #:key (implicit-inputs? #t) #:allow-other-keys
                    #:rest arguments)
  (define lower (build-system-lower gnu-build-system))
  (apply lower
         name
         (substitute-keyword-arguments arguments
           ;; minetest-mod-build-system adds implicit inputs by itself,
           ;; so don't let gnu-build-system add its own implicit inputs
           ;; as well.
           ((#:implicit-inputs? implicit-inputs? #t)
            #f)
           ((#:implicit-cross-inputs? implicit-cross-inputs? #t)
            #f)
           ((#:imported-modules imported-modules %minetest-build-system-modules)
            imported-modules)
           ((#:modules modules %default-modules)
            modules)
           ((#:phases phases '%standard-phases)
            phases)
           ;; Ensure nothing sneaks into the closure.
           ((#:allowed-references allowed-references '())
            allowed-references)
           ;; Add the implicit inputs.
           ((#:native-inputs native-inputs '())
            (if implicit-inputs?
                (append native-inputs (standard-minetest-packages))
                native-inputs)))))

(define minetest-mod-build-system
  (build-system
    (name 'minetest-mod)
    (description "The build system for minetest mods")
    (lower lower-mod)))

;;; minetest.scm ends here
