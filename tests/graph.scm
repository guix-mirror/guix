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

(define-module (test-graph)
  #:use-module (guix tests)
  #:use-module (guix graph)
  #:use-module (guix scripts graph)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

(define %store
  (open-connection-for-tests))

(define (make-recording-backend)
  "Return a <graph-backend> and a thunk that returns the recorded nodes and
edges."
  (let ((nodes '())
        (edges '()))
    (define (record-node id label port)
      (set! nodes (cons (list id label) nodes)))
    (define (record-edge source target port)
      (set! edges (cons (list source target) edges)))
    (define (return)
      (values (reverse nodes) (reverse edges)))

    (values (graph-backend (const #t) (const #t)
                           record-node record-edge)
            return)))

(define (package->tuple package)
  "Return a tuple representing PACKAGE as produced by %PACKAGE-NODE-TYPE."
  (list (object-address package)
        (package-full-name package)))

(define (edge->tuple source target)
  "Likewise for an edge from SOURCE to TARGET."
  (list (object-address source)
        (object-address target)))


(test-begin "graph")

(test-assert "package DAG"
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (let* ((p1 (dummy-package "p1"))
           (p2 (dummy-package "p2" (inputs `(("p1" ,p1)))))
           (p3 (dummy-package "p3" (inputs `(("p2" ,p2) ("p1", p1))))))
      (run-with-store %store
        (export-graph (list p3) 'port
                      #:node-type %package-node-type
                      #:backend backend))
      ;; We should see nothing more than these 3 packages.
      (let-values (((nodes edges) (nodes+edges)))
        (and (equal? nodes (map package->tuple (list p3 p2 p1)))
             (equal? edges
                     (map edge->tuple
                          (list p3 p3 p2)
                          (list p2 p1 p1))))))))

(test-assert "bag-emerged DAG"
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (let ((p        (dummy-package "p"))
          (implicit (map (match-lambda
                           ((label package) package))
                         (standard-packages))))
      (run-with-store %store
        (export-graph (list p) 'port
                      #:node-type %bag-emerged-node-type
                      #:backend backend))
      ;; We should see exactly P and IMPLICIT, with one edge from P to each
      ;; element of IMPLICIT.
      (let-values (((nodes edges) (nodes+edges)))
        (and (equal? (match nodes
                       (((labels names) ...)
                        names))
                     (map package-full-name (cons p implicit)))
             (equal? (match edges
                       (((sources destinations) ...)
                        (zip (map store-path-package-name sources)
                             (map store-path-package-name destinations))))
                     (map (lambda (destination)
                            (list "p-0.drv"
                                  (string-append
                                   (package-full-name destination)
                                   ".drv")))
                          implicit)))))))

(test-assert "bag DAG"                            ;a big town in Iraq
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (let ((p (dummy-package "p")))
      (run-with-store %store
        (export-graph (list p) 'port
                      #:node-type %bag-node-type
                      #:backend backend))
      ;; We should see P, its implicit inputs as well as the whole DAG, which
      ;; should include bootstrap binaries.
      (let-values (((nodes edges) (nodes+edges)))
        (every (lambda (name)
                 (find (cut string=? name <>)
                       (match nodes
                         (((labels names) ...)
                          names))))
               (match %bootstrap-inputs
                 (((labels packages) ...)
                  (map package-full-name packages))))))))

(test-assert "derivation DAG"
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (run-with-store %store
      (mlet* %store-monad ((txt   (text-file "text-file" "Hello!"))
                           (guile (package->derivation %bootstrap-guile))
                           (drv   (gexp->derivation "output"
                                                    #~(symlink #$txt #$output)
                                                    #:guile-for-build
                                                    guile)))
        ;; We should get at least these 3 nodes and corresponding edges.
        (mbegin %store-monad
          (export-graph (list drv) 'port
                        #:node-type %derivation-node-type
                        #:backend backend)
          (let-values (((nodes edges) (nodes+edges)))
            ;; XXX: For some reason we need to throw in some 'basename'.
            (return (and (match nodes
                           (((ids labels) ...)
                            (let ((ids (map basename ids)))
                              (every (lambda (item)
                                       (member (basename item) ids))
                                     (list txt
                                           (derivation-file-name drv)
                                           (derivation-file-name guile))))))
                         (every (cut member <>
                                     (map (lambda (edge)
                                            (map basename edge))
                                          edges))
                                (list (map (compose basename derivation-file-name)
                                           (list drv guile))
                                      (list (basename (derivation-file-name drv))
                                            (basename txt))))))))))))

(test-assert "reference DAG"
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (run-with-store %store
      (mlet* %store-monad ((txt   (text-file "text-file" "Hello!"))
                           (guile (package->derivation %bootstrap-guile))
                           (drv   (gexp->derivation "output"
                                                    #~(symlink #$txt #$output)
                                                    #:guile-for-build
                                                    guile))
                           (out -> (derivation->output-path drv)))
        ;; We should see only OUT and TXT, with an edge from the former to the
        ;; latter.
        (mbegin %store-monad
          (built-derivations (list drv))
          (export-graph (list (derivation->output-path drv)) 'port
                        #:node-type %reference-node-type
                        #:backend backend)
          (let-values (((nodes edges) (nodes+edges)))
            (return
             (and (equal? (match nodes
                            (((ids labels) ...)
                             ids))
                          (list out txt))
                  (equal? edges `((,out ,txt)))))))))))

(test-assert "node-edges"
  (run-with-store %store
    (let ((packages (fold-packages cons '())))
      (mlet %store-monad ((edges (node-edges %package-node-type packages)))
        (return (and (null? (edges grep))
                     (lset= eq?
                            (edges guile-2.0)
                            (match (package-direct-inputs guile-2.0)
                              (((labels packages _ ...) ...)
                               packages)))))))))

(test-assert "node-transitive-edges + node-back-edges"
  (run-with-store %store
    (let ((packages   (fold-packages cons '()))
          (bootstrap? (lambda (package)
                        (string-contains
                         (location-file (package-location package))
                         "bootstrap.scm")))
          (trivial?   (lambda (package)
                        (eq? (package-build-system package)
                             trivial-build-system))))
      (mlet %store-monad ((edges (node-back-edges %bag-node-type packages)))
        (let* ((glibc      (canonical-package glibc))
               (dependents (node-transitive-edges (list glibc) edges))
               (diff       (lset-difference eq? packages dependents)))
          ;; All the packages depend on libc, except bootstrap packages and
          ;; some that use TRIVIAL-BUILD-SYSTEM.
          (return (null? (remove (lambda (package)
                                   (or (trivial? package)
                                       (bootstrap? package)))
                                 diff))))))))

(test-end "graph")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
