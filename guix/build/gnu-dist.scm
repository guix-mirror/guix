;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define* (copy-source #:key source #:allow-other-keys)
  (copy-recursively source "."))

(define* (autoreconf #:rest args)
  (letrec-syntax ((try-files (syntax-rules (else)
                               ((_ (else fallback ...))
                                (begin fallback ...))
                               ((_ file files ... (else fallback ...))
                                (if (file-exists? file)
                                    (begin
                                      (format #t "bootstrapping with `~a'...~%"
                                              file)
                                      (invoke (string-append "./" file)))
                                    (try-files files ...
                                               (else fallback ...)))))))
    (try-files "bootstrap" "bootstrap.sh" "autogen" "autogen.sh"
               (else
                (format #t "bootstrapping with `autoreconf'...~%")
                (invoke "autoreconf" "-vfi")))))

(define* (build #:key build-before-dist? make-flags (dist-target "distcheck")
                #:allow-other-keys
                #:rest args)
  (when build-before-dist?
    (let ((build (assq-ref %standard-phases 'build)))
      (apply build args)))
  (format #t "building target `~a'~%" dist-target)
  (apply invoke "make" dist-target make-flags))

(define* (install-dist #:key outputs #:allow-other-keys)
  (let* ((out      (assoc-ref outputs "out"))
         (meta     (string-append out "/nix-support")) ; Hydra meta-data
         (tarballs (find-files "." "\\.tar\\.")))
    (mkdir out)
    (for-each (lambda (tarball)
                (copy-file tarball (string-append out "/" tarball)))
              out)

    (mkdir meta)
    (call-with-output-file (string-append out "/hydra-build-products")
      (lambda (port)
        (for-each (lambda (tarball)
                    ;; This tells Hydra's what kind of build products we have,
                    ;; so it can represent them nicely.  See `product-list.tt'
                    ;; in Hydra for details.
                    (format port "file source-dist ~a/~a~%" out tarball))
                  tarballs)))
    #t))

(define %dist-phases
  ;; Phases for building a source tarball.
  (modify-phases %standard-phases
    (delete 'strip)
    (replace 'install install-dist)
    (replace 'build build)
    (add-before 'configure 'autoreconf autoreconf)
    (replace 'unpack copy-source)))

;;; gnu-dist.scm ends here
