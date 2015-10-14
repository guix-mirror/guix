;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-services)
  #:use-module (gnu services)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

(test-begin "services")

(test-assert "service-back-edges"
  (let* ((t1 (service-type (name 't1) (extensions '())
                           (compose +) (extend *)))
         (t2 (service-type (name 't2)
                           (extensions
                            (list (service-extension t1 (const '()))))
                           (compose +) (extend *)))
         (t3 (service-type (name 't3)
                           (extensions
                            (list (service-extension t2 identity)
                                  (service-extension t1 list)))))
         (s1 (service t1 #t))
         (s2 (service t2 #t))
         (s3 (service t3 #t))
         (e  (service-back-edges (list s1 s2 s3))))
    (and (lset= eq? (e s1) (list s2 s3))
         (lset= eq? (e s2) (list s3))
         (null? (e s3)))))

(test-equal "fold-services"
  ;; Make sure 'fold-services' returns the right result.  The numbers come
  ;; from services of type T3; 'xyz 60' comes from the service of type T2,
  ;; where 60 = 15 × 4 = (1 + 2 + 3 + 4 + 5) × 4.
  '(initial-value 5 4 3 2 1 xyz 60)
  (let* ((t1 (service-type (name 't1) (extensions '())
                           (compose concatenate)
                           (extend cons)))
         (t2 (service-type (name 't2)
                           (extensions
                            (list (service-extension t1
                                                     (cut list 'xyz <>))))
                           (compose (cut reduce + 0 <>))
                           (extend *)))
         (t3 (service-type (name 't3)
                           (extensions
                            (list (service-extension t2 identity)
                                  (service-extension t1 list)))))
         (r  (fold-services (cons* (service t1 'initial-value)
                                   (service t2 4)
                                   (map (lambda (x)
                                          (service t3 x))
                                        (iota 5 1)))
                            #:target-type t1)))
    (and (eq? (service-kind r) t1)
         (service-parameters r))))

(test-assert "fold-services, ambiguity"
  (let* ((t1 (service-type (name 't1) (extensions '())
                           (compose concatenate)
                           (extend cons)))
         (t2 (service-type (name 't2)
                           (extensions
                            (list (service-extension t1 list)))))
         (s  (service t2 42)))
    (guard (c ((ambiguous-target-service-error? c)
               (and (eq? (ambiguous-target-service-error-target-type c)
                         t1)
                    (eq? (ambiguous-target-service-error-service c)
                         s))))
      (fold-services (list (service t1 'first)
                           (service t1 'second)
                           s)
                     #:target-type t1)
      #f)))

(test-assert "fold-services, missing target"
  (let* ((t1 (service-type (name 't1) (extensions '())))
         (t2 (service-type (name 't2)
                           (extensions
                            (list (service-extension t1 list)))))
         (s  (service t2 42)))
    (guard (c ((missing-target-service-error? c)
               (and (eq? (missing-target-service-error-target-type c)
                         t1)
                    (eq? (missing-target-service-error-service c)
                         s))))
      (fold-services (list s) #:target-type t1)
      #f)))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))
