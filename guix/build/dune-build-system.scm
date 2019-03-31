;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Gabriel Hondet <gabrielhondet@gmail.com>
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

(define-module (guix build dune-build-system)
  #:use-module ((guix build ocaml-build-system) #:prefix ocaml:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            dune-build))

;; Commentary:
;;
;; Builder-side code of the standard dune build procedure.
;;
;; Code:

(define* (build #:key (build-flags '()) (jbuild? #f)
                (use-make? #f) (package #f) #:allow-other-keys)
  "Build the given package."
  (let ((program (if jbuild? "jbuilder" "dune")))
    (apply invoke program "build" "@install"
           (append (if package (list "-p" package) '()) build-flags)))
  #t)

(define* (check #:key (test-flags '()) (test-target "test") tests?
                  (jbuild? #f) (package #f) #:allow-other-keys)
  "Test the given package."
  (when tests?
    (let ((program (if jbuild? "jbuilder" "dune")))
      (apply invoke program "runtest" test-target
             (append (if package (list "-p" package) '()) test-flags))))
  #t)

(define* (install #:key outputs (install-target "install") (jbuild? #f)
                  (package #f) #:allow-other-keys)
  "Install the given package."
  (let ((out (assoc-ref outputs "out"))
        (program (if jbuild? "jbuilder" "dune")))
    (apply invoke program install-target "--prefix" out "--libdir"
           (string-append out "/lib/ocaml/site-lib")
           (if package (list package) '())))
  #t)

(define %standard-phases
  ;; Everything is as with the GNU Build System except for the `configure'
  ;; , `build', `check' and `install' phases.
  (modify-phases ocaml:%standard-phases
    (delete 'configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (dune-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply ocaml:ocaml-build #:inputs inputs #:phases phases args))

;;; dune-build-system.scm ends here
