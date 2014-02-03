;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-monads)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module ((guix packages)
                #:select (package-derivation %current-system))
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

;; Test the (guix store) module.

(define %store
  (open-connection))

;; Make sure we build everything by ourselves.
(set-build-options %store #:use-substitutes? #f)

(define %monads
  (list %identity-monad %store-monad))

(define %monad-run
  (list identity
        (cut run-with-store %store <>)))


(test-begin "monads")

(test-assert "monad?"
  (and (every monad? %monads)
       (every (compose procedure? monad-bind) %monads)
       (every (compose procedure? monad-return) %monads)))

;; The 3 "monad laws": <http://www.haskell.org/haskellwiki/Monad_laws>.

(test-assert "left identity"
  (every (lambda (monad run)
           (let ((number (random 777)))
             (with-monad monad
               (define (f x)
                 (return (* (1+ number) 2)))

               (= (run (>>= (return number) f))
                  (run (f number))))))
         %monads
         %monad-run))

(test-assert "right identity"
  (every (lambda (monad run)
           (with-monad monad
             (let ((number (return (random 777))))
               (= (run (>>= number return))
                  (run number)))))
         %monads
         %monad-run))

(test-assert "associativity"
  (every (lambda (monad run)
           (with-monad monad
             (define (f x)
               (return (+ 1 x)))
             (define (g x)
               (return (* 2 x)))

             (let ((number (return (random 777))))
               (= (run (>>= (>>= number f) g))
                  (run (>>= number (lambda (x) (>>= (f x) g))))))))
         %monads
         %monad-run))

(test-assert "lift"
  (every (lambda (monad run)
           (let ((f (lift1 1+ monad)))
             (with-monad monad
               (let ((number (random 777)))
                 (= (run (>>= (return number) f))
                    (1+ number))))))
         %monads
         %monad-run))

(test-assert "mlet* + text-file + package-file"
  (run-with-store %store
    (mlet* %store-monad ((guile (package-file %bootstrap-guile "bin/guile"))
                         (file  (text-file "monadic" guile)))
      (return (equal? (call-with-input-file file get-string-all)
                      guile)))
    #:guile-for-build (package-derivation %store %bootstrap-guile)))

(test-assert "mlet* + derivation-expression"
  (run-with-store %store
    (mlet* %store-monad ((guile  (package-file %bootstrap-guile "bin/guile"))
                         (gdrv   (package->derivation %bootstrap-guile))
                         (exp -> `(let ((out (assoc-ref %outputs "out")))
                                    (mkdir out)
                                    (symlink ,guile
                                             (string-append out "/guile-rocks"))))
                         (drv    (derivation-expression "rocks" exp
                                                        #:inputs
                                                        `(("g" ,gdrv))))
                         (out -> (derivation->output-path drv))
                         (built? (built-derivations (list drv))))
      (return (and built?
                   (equal? guile
                           (readlink (string-append out "/guile-rocks"))))))
    #:guile-for-build (package-derivation %store %bootstrap-guile)))

(test-assert "text-file*"
  (let ((references (store-lift references)))
    (run-with-store %store
      (mlet* %store-monad
          ((drv  (package->derivation %bootstrap-guile))
           (guile -> (derivation->output-path drv))
           (file (text-file "bar" "This is bar."))
           (text (text-file* "foo"
                             %bootstrap-guile "/bin/guile "
                             `(,%bootstrap-guile "out") "/bin/guile "
                             drv "/bin/guile "
                             file))
           (done (built-derivations (list text)))
           (out -> (derivation->output-path text))
           (refs (references out)))
        ;; Make sure we get the right references and the right content.
        (return (and (lset= string=? refs (list guile file))
                     (equal? (call-with-input-file out get-string-all)
                             (string-append guile "/bin/guile "
                                            guile "/bin/guile "
                                            guile "/bin/guile "
                                            file)))))
      #:guile-for-build (package-derivation %store %bootstrap-guile))))

(test-assert "mapm"
  (every (lambda (monad run)
           (with-monad monad
             (equal? (run (mapm monad (lift1 1+ monad) (map return (iota 10))))
                     (map 1+ (iota 10)))))
         %monads
         %monad-run))

(test-assert "sequence"
  (every (lambda (monad run)
           (let* ((input (iota 100))
                  (order '()))
             (define (frob i)
               ;; The side effect here is used to keep track of the order in
               ;; which monadic values are bound.
               (set! order (cons i order))
               i)

             (and (equal? input
                          (run (sequence monad
                                         (map (lift1 frob monad) input))))

                  ;; Make sure this is from left to right.
                  (equal? order (reverse input)))))
         %monads
         %monad-run))

(test-assert "listm"
  (every (lambda (monad run)
           (run (with-monad monad
                  (let ((lst (listm monad
                                    (return 1) (return 2) (return 3))))
                    (mlet monad ((lst lst))
                      (return (equal? '(1 2 3) lst)))))))
         %monads
         %monad-run))

(test-assert "anym"
  (every (lambda (monad run)
           (eq? (run (with-monad monad
                       (let ((lst (list (return 1) (return 2) (return 3))))
                         (anym monad
                               (lambda (x)
                                 (and (odd? x) 'odd!))
                               lst))))
                'odd!))
         %monads
         %monad-run))

(test-end "monads")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
