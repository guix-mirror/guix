;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (gnu services herd)
  #:use-module (gnu services shepherd)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

(define live-service
  (@@ (gnu services herd) live-service))


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

(test-assert "shepherd-service-lookup-procedure"
  (let* ((s1 (shepherd-service (provision '(s1 s1b)) (start #f)))
         (s2 (shepherd-service (provision '(s2 s2b)) (start #f)))
         (s3 (shepherd-service (provision '(s3 s3b s3c)) (start #f)))
         (lookup (shepherd-service-lookup-procedure (list s1 s2 s3))))
    (and (eq? (lookup 's1) (lookup 's1b) s1)
         (eq? (lookup 's2) (lookup 's2b) s2)
         (eq? (lookup 's3) (lookup 's3b) s3))))

(test-assert "shepherd-service-back-edges"
  (let* ((s1 (shepherd-service (provision '(s1)) (start #f)))
         (s2 (shepherd-service (provision '(s2))
                               (requirement '(s1))
                               (start #f)))
         (s3 (shepherd-service (provision '(s3))
                               (requirement '(s1 s2))
                               (start #f)))
         (e  (shepherd-service-back-edges (list s1 s2 s3))))
    (and (lset= eq? (e s1) (list s2 s3))
         (lset= eq? (e s2) (list s3))
         (null? (e s3)))))

(test-equal "shepherd-service-upgrade: nothing to do"
  '(() ())
  (call-with-values
      (lambda ()
        (shepherd-service-upgrade '() '()))
    list))

(test-equal "shepherd-service-upgrade: one unchanged, one upgraded, one new"
  '(((bar))                                       ;unload
    ((bar) (baz)))                                ;load
  (call-with-values
      (lambda ()
        ;; Here 'foo' is not upgraded because it is still running, whereas
        ;; 'bar' is upgraded because it is not currently running.  'baz' is
        ;; loaded because it's a new service.
        (shepherd-service-upgrade
         (list (live-service '(foo) '() #t)
               (live-service '(bar) '() #f)
               (live-service '(root) '() #t))     ;essential!
         (list (shepherd-service (provision '(foo))
                                 (start #t))
               (shepherd-service (provision '(bar))
                                 (start #t))
               (shepherd-service (provision '(baz))
                                 (start #t)))))
    (lambda (unload load)
      (list (map live-service-provision unload)
            (map shepherd-service-provision load)))))

(test-equal "shepherd-service-upgrade: service depended on is not unloaded"
  '(((baz))                                       ;unload
    ())                                           ;load
  (call-with-values
      (lambda ()
        ;; Service 'bar' is not among the target services; yet, it must not be
        ;; unloaded because 'foo' depends on it.
        (shepherd-service-upgrade
         (list (live-service '(foo) '(bar) #t)
               (live-service '(bar) '() #t)       ;still used!
               (live-service '(baz) '() #t))
         (list (shepherd-service (provision '(foo))
                                 (start #t)))))
    (lambda (unload load)
      (list (map live-service-provision unload)
            (map shepherd-service-provision load)))))

(test-equal "shepherd-service-upgrade: obsolete services that depend on each other"
  '(((foo) (bar) (baz))                           ;unload
    ((qux)))                                      ;load
  (call-with-values
      (lambda ()
        ;; 'foo', 'bar', and 'baz' depend on each other, but all of them are
        ;; obsolete, and thus should be unloaded.
        (shepherd-service-upgrade
         (list (live-service '(foo) '(bar) #t)    ;obsolete
               (live-service '(bar) '(baz) #t)    ;obsolete
               (live-service '(baz) '() #t))      ;obsolete
         (list (shepherd-service (provision '(qux))
                                 (start #t)))))
    (lambda (unload load)
      (list (map live-service-provision unload)
            (map shepherd-service-provision load)))))

(test-end)
