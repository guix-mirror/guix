;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix tests)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module ((gnu packages base) #:select (coreutils))
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

;; Test the (guix monads) module.

(define %store
  (open-connection-for-tests))

(define %monads
  (list %identity-monad %store-monad %state-monad))

(define %monad-run
  (list identity
        (cut run-with-store %store <>)
        (cut run-with-state <> '())))

(define-syntax-rule (values->list exp)
  (call-with-values (lambda () exp)
    list))


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

(test-assert "mbegin"
  (every (lambda (monad run)
           (with-monad monad
             (let* ((been-there? #f)
                    (number (mbegin monad
                              (return 1)
                              (begin
                                (set! been-there? #t)
                                (return 2))
                              (return 3))))
               (and (= (run number) 3)
                    been-there?))))
         %monads
         %monad-run))

(test-assert "mlet* + text-file + package-file"
  (run-with-store %store
    (mlet* %store-monad ((guile (package-file %bootstrap-guile "bin/guile"))
                         (file  (text-file "monadic" guile)))
      (return (equal? (call-with-input-file file get-string-all)
                      guile)))
    #:guile-for-build (package-derivation %store %bootstrap-guile)))

(test-assert "package-file, default system"
  ;; The default system should be the one at '>>=' time, not the one at
  ;; invocation time.  See <http://bugs.gnu.org/18002>.
  (run-with-store %store
    (mlet* %store-monad
        ((system -> (%current-system))
         (file   (parameterize ((%current-system "foobar64-linux"))
                   (package-file coreutils "bin/ls")))
         (cu     (package->derivation coreutils)))
      (return (string=? file
                        (string-append (derivation->output-path cu)
                                       "/bin/ls"))))
    #:guile-for-build (package-derivation %store %bootstrap-guile)))

(test-assert "package-file + package->cross-derivation"
  (run-with-store %store
    (mlet* %store-monad ((target -> "mips64el-linux-gnu")
                         (file (package-file coreutils "bin/ls"
                                             #:target target))
                         (xcu  (package->cross-derivation coreutils target)))
      (let ((output (derivation->output-path xcu)))
        (return (string=? file (string-append output "/bin/ls")))))
    #:guile-for-build (package-derivation %store %bootstrap-guile)))

(test-assert "interned-file"
  (run-with-store %store
    (mlet* %store-monad ((file -> (search-path %load-path "guix.scm"))
                         (a       (interned-file file))
                         (b       (interned-file file "b")))
      (return (equal? (call-with-input-file file get-string-all)
                      (call-with-input-file a get-string-all)
                      (call-with-input-file b get-string-all))))
    #:guile-for-build (package-derivation %store %bootstrap-guile)))

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
               (mlet monad ((foo (return 'foo)))
                 ;; The side effect here is used to keep track of the order in
                 ;; which monadic values are bound.  Perform the side effect
                 ;; within a '>>=' so that it is performed when the return
                 ;; value is actually bound.
                 (set! order (cons i order))
                 (return i)))

             (and (equal? input
                          (run (sequence monad (map frob input))))

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

(test-equal "set-current-state"
  (list '(a a d) 'd)
  (values->list
   (run-with-state
       (mlet* %state-monad ((init  (current-state))
                            (init2 (set-current-state 'b)))
         (mbegin %state-monad
           (set-current-state 'c)
           (set-current-state 'd)
           (mlet %state-monad ((last (current-state)))
             (return (list init init2 last)))))
     'a)))

(test-equal "state-push etc."
  (list '((z . 2) (p . (1)) (a . (1))) '(2 1))
  (values->list
   (run-with-state
       (mbegin %state-monad
         (state-push 1)    ;(1)
         (state-push 2)    ;(2 1)
         (mlet* %state-monad ((z (state-pop))        ;(1)
                              (p (current-state))
                              (a (state-push z)))    ;(2 1)
           (return `((z . ,z) (p . ,p) (a . ,a)))))
     '())))

(test-end "monads")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
