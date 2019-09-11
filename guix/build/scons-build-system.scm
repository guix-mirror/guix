;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
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

(define-module (guix build scons-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:export (%standard-phases
            scons-build))

;; Commentary:
;;
;; Builder-side code of the SCons build system.
;;
;; Code:

(define* (build #:key outputs (build-targets '()) (scons-flags '()) (parallel-build? #t) #:allow-other-keys)
  (let ((out (assoc-ref outputs "out")))
    (mkdir-p out)
    (apply invoke "scons"
           (append (if parallel-build?
                       (list "-j" (number->string
                                   (parallel-job-count)))
                       (list))
                   scons-flags
                   build-targets))))

(define* (check #:key tests? test-target (scons-flags '()) #:allow-other-keys)
  "Run the test suite of a given SCons application."
  (if tests?
      (apply invoke "scons" test-target scons-flags)
      (format #t "test suite not run~%"))
  #t)

(define* (install #:key outputs (install-targets '("install")) (scons-flags '()) #:allow-other-keys)
  "Install a given SCons application."
  (apply invoke "scons" (append scons-flags install-targets)))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (scons-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build a given SCons application, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; scons-build-system.scm ends here
