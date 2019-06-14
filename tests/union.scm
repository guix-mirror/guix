;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (guix tests)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix build union)
  #:use-module ((guix build utils)
                #:select (with-directory-excursion directory-exists?))
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match))

;; Exercise the (guix build union) module.

(define %store
  (open-connection-for-tests))


(test-begin "union")

(test-assert "union-build with symlink to directory"
  ;; http://bugs.gnu.org/17083
  ;; Here both ONE and TWO provide an element called 'foo', but in ONE it's a
  ;; directory whereas in TWO it's a symlink to a directory.
  (let* ((one     (build-expression->derivation
                   %store "one"
                   '(begin
                      (use-modules (guix build utils) (srfi srfi-26))
                      (let ((foo (string-append %output "/foo")))
                        (mkdir-p foo)
                        (call-with-output-file (string-append foo "/one")
                          (cut display "one" <>))))
                   #:modules '((guix build utils))))
         (two     (build-expression->derivation
                   %store "two"
                   '(begin
                      (use-modules (guix build utils) (srfi srfi-26))
                      (let ((foo (string-append %output "/foo"))
                            (bar (string-append %output "/bar")))
                        (mkdir-p bar)
                        (call-with-output-file (string-append bar "/two")
                          (cut display "two" <>))
                        (symlink "bar" foo)))
                   #:modules '((guix build utils))))
         (builder '(begin
                     (use-modules (guix build union))

                     (union-build (assoc-ref %outputs "out")
                                  (list (assoc-ref %build-inputs "one")
                                        (assoc-ref %build-inputs "two")))))
         (drv
          (build-expression->derivation %store "union-collision-symlink"
                                        builder
                                        #:inputs `(("one" ,one) ("two" ,two))
                                        #:modules '((guix build union)))))
    (and (build-derivations %store (list drv))
         (with-directory-excursion (pk (derivation->output-path drv))
           (and (string=? "one"
                          (call-with-input-file "foo/one" get-string-all))
                (string=? "two"
                          (call-with-input-file "foo/two" get-string-all))
                (string=? "two"
                          (call-with-input-file "bar/two" get-string-all))
                (not (file-exists? "bar/one")))))))

(test-skip (if (and %store (network-reachable?))
               0
               1))

(test-assert "union-build"
  (let* ((inputs  (map (match-lambda
                        ((name package)
                         `(,name ,(package-derivation %store package))))

                       ;; Purposefully leave duplicate entries.
                       (filter (compose package? cadr)
                               (append %bootstrap-inputs-for-tests
                                       (take %bootstrap-inputs-for-tests 3)))))
         (builder `(begin
                     (use-modules (guix build union))
                     (union-build (assoc-ref %outputs "out")
                                  (map cdr %build-inputs))))
         (drv
          (build-expression->derivation %store "union-test"
                                        builder
                                        #:inputs inputs
                                        #:modules '((guix build union)))))
    (and (build-derivations %store (list (pk 'drv drv)))
         (with-directory-excursion (derivation->output-path drv)
           (and (file-exists? "bin/touch")
                (file-exists? "bin/gcc")
                (file-exists? "bin/ld")
                (file-exists? "lib/libc.so")
                (directory-exists? "lib/gcc")
                (file-exists? "include/unistd.h")

                ;; The 'include/c++' sub-directory is only found in
                ;; gcc-bootstrap, so it should be unified in a
                ;; straightforward way, without traversing it.
                (eq? 'symlink (stat:type (lstat "include/c++")))

                ;; Conversely, several inputs have a 'bin' sub-directory, so
                ;; unifying it requires traversing them all, and creating a
                ;; new 'bin' sub-directory in the profile.
                (eq? 'directory (stat:type (lstat "bin"))))))))

(test-assert "union-build collision first & last"
  (let* ((guile   (package-derivation %store %bootstrap-guile))
         (fake    (build-expression->derivation
                   %store "fake-guile"
                   '(begin
                      (use-modules (guix build utils))
                      (let ((out (assoc-ref %outputs "out")))
                        (mkdir-p (string-append out "/bin"))
                        (call-with-output-file (string-append out "/bin/guile")
                          (const #t))))
                   #:modules '((guix build utils))))
         (builder (lambda (policy)
                    `(begin
                       (use-modules (guix build union)
                                    (srfi srfi-1))
                       (union-build (assoc-ref %outputs "out")
                                    (map cdr %build-inputs)
                                    #:resolve-collision ,policy))))
         (drv1
          (build-expression->derivation %store "union-first"
                                        (builder 'first)
                                        #:inputs `(("guile" ,guile)
                                                   ("fake" ,fake))
                                        #:modules '((guix build union))))
         (drv2
          (build-expression->derivation %store "union-last"
                                        (builder 'last)
                                        #:inputs `(("guile" ,guile)
                                                   ("fake" ,fake))
                                        #:modules '((guix build union)))))
    (and (build-derivations %store (list drv1 drv2))
         (with-directory-excursion (derivation->output-path drv1)
           (string=? (readlink "bin/guile")
                     (string-append (derivation->output-path guile)
                                    "/bin/guile")))
         (with-directory-excursion (derivation->output-path drv2)
           (string=? (readlink "bin/guile")
                     (string-append (derivation->output-path fake)
                                    "/bin/guile"))))))

(test-assert "union-build #:create-all-directories? #t"
  (let* ((build  `(begin
                    (use-modules (guix build union))
                    (union-build (assoc-ref %outputs "out")
                                 (map cdr %build-inputs)
                                 #:create-all-directories? #t)))
         (input  (package-derivation %store %bootstrap-guile))
         (drv    (build-expression->derivation %store "union-test-all-dirs"
                                               build
                                               #:modules '((guix build union))
                                               #:inputs `(("g" ,input)))))
    (and (build-derivations %store (list drv))
         (with-directory-excursion (derivation->output-path drv)
           ;; Even though there's only one input to the union,
           ;; #:create-all-directories? #t must have created bin/ rather than
           ;; making it a symlink to Guile's bin/.
           (and (file-exists? "bin/guile")
                (file-is-directory? "bin")
                (eq? 'symlink (stat:type (lstat "bin/guile"))))))))

(letrec-syntax ((test-relative-file-name
                 (syntax-rules (=>)
                   ((_ (reference file => expected) rest ...)
                    (begin
                      (test-equal (string-append "relative-file-name "
                                                 reference " " file)
                        expected
                        (relative-file-name reference file))
                      (test-relative-file-name rest ...)))
                   ((_)
                    #t))))
  (test-relative-file-name
   ("/a/b" "/a/c/d"     => "../c/d")
   ("/a/b" "/a/b"       => "")
   ("/a/b" "/a"         => "..")
   ("/a/b" "/a/b/c/d"   => "c/d")
   ("/a/b/c" "/a/d/e/f" => "../../d/e/f")))

(test-end)
