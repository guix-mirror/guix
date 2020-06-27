;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build gnu-dist)
  #:use-module (guix build utils)
  #:use-module (guix build gnu-build-system)
  #:use-module (srfi srfi-1)
  #:export (%dist-phases))

;;; Commentary:
;;;
;;; Build phases to build a source tarball with the GNU build system, as with
;;; "make distcheck".
;;;
;;; Code:

(define* (build #:key build-before-dist? make-flags (dist-target "distcheck")
                #:allow-other-keys
                #:rest args)
  (when build-before-dist?
    (let ((build (assq-ref %standard-phases 'build)))
      (apply build args)))
  (format #t "building target `~a'~%" dist-target)
  (apply invoke "make" dist-target make-flags))

(define* (install-dist #:key outputs #:allow-other-keys)
  (let ((out (assoc-ref outputs "out")))
    (for-each (lambda (tarball)
                (install-file tarball out))
              (find-files "." "\\.tar\\."))
    #t))

(define %dist-phases
  ;; Phases for building a source tarball.
  (modify-phases %standard-phases
    (delete 'strip)
    (replace 'install install-dist)
    (add-after 'build 'build-dist build)
    (delete 'build)))

;;; gnu-dist.scm ends here
