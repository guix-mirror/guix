;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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


(define-module (test-union)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix build union)
  #:use-module ((guix build utils)
                #:select (with-directory-excursion directory-exists?))
  #:use-module (distro packages bootstrap)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

;; Exercise the (guix build union) module.

(define %store
  (false-if-exception (open-connection)))

(when %store
  ;; By default, use %BOOTSTRAP-GUILE for the current system.
  (let ((drv (package-derivation %store %bootstrap-guile)))
    (%guile-for-build drv)))


(test-begin "union")

(test-equal "tree-union, empty"
  '()
  (tree-union '()))

(test-equal "tree-union, leaves only"
  '(a b c d)
  (tree-union '(a b c d)))

(test-equal "tree-union, simple"
  '((bin ls touch make awk gawk))
  (tree-union '((bin ls touch)
                (bin make)
                (bin awk gawk))))

(test-equal "tree-union, several levels"
  '((share (doc (make README) (coreutils README)))
    (bin ls touch make))
  (tree-union '((bin ls touch)
                (share (doc (coreutils README)))
                (bin make)
                (share (doc (make README))))))

(test-skip (if (and %store
                    (false-if-exception
                     (getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)))
               0
               1))

(test-assert "union-build"
  (let* ((inputs  (map (match-lambda
                        ((name package)
                         `(,name ,(package-derivation %store package))))
                       %bootstrap-inputs))
         (builder `(begin
                     (use-modules (guix build union))
                     (union-build (assoc-ref %outputs "out")
                                  (map cdr %build-inputs))))
         (drv
          (build-expression->derivation %store "union-test"
                                        (%current-system)
                                        builder inputs
                                        #:modules '((guix build union)))))
    (and (build-derivations %store (list (pk 'drv drv)))
         (with-directory-excursion (derivation-path->output-path drv)
           (and (file-exists? "bin/touch")
                (file-exists? "bin/gcc")
                (file-exists? "bin/ld")
                (file-exists? "lib/libc.so")
                (directory-exists? "lib/gcc")
                (file-exists? "include/unistd.h"))))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; eval: (put 'test-equal 'scheme-indent-function 1)
;;; eval: (put 'call-with-input-string 'scheme-indent-function 1)
;;; End:
