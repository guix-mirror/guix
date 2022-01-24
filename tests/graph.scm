;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix grafts)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (ice-9 sandbox)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

(define %store
  (open-connection-for-tests))

;; Globally disable grafts because they can trigger early builds.
(%graft? #f)

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

    (values (graph-backend "test" "This is the test backend."
                           (const #t) (const #t)
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

(test-assert "package DAG, limited depth"
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (let* ((p1 (dummy-package "p1"))
           (p2 (dummy-package "p2" (inputs `(("p1" ,p1)))))
           (p3 (dummy-package "p3" (inputs `(("p1" ,p1)))))
           (p4 (dummy-package "p4" (inputs `(("p2" ,p2) ("p3" ,p3))))))
      (run-with-store %store
        (export-graph (list p4) 'port
                      #:max-depth 1
                      #:node-type %package-node-type
                      #:backend backend))
      ;; We should see nothing more than these 3 packages.
      (let-values (((nodes edges) (nodes+edges)))
        (and (equal? nodes (map package->tuple (list p4 p2 p3)))
             (equal? edges
                     (map edge->tuple
                          (list p4 p4)
                          (list p2 p3))))))))

(test-assert "package DAG, oops it was a cycle"
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (letrec ((p1 (dummy-package "p1" (inputs `(("p3" ,p3)))))
             (p2 (dummy-package "p2" (inputs `(("p1" ,p1)))))
             (p3 (dummy-package "p3" (inputs `(("p2" ,p2) ("p1", p1))))))
      (call-with-time-limit
       600 ;; If ever this test should fail, we still want it to terminate
       (lambda ()
         (run-with-store %store
           (export-graph (list p3) 'port
                         #:node-type %package-node-type
                         #:backend backend)))
       (lambda ()
         (run-with-store %store
           (export-graph
            (list (dummy-package "timeout-reached"))
            'port
            #:node-type %package-node-type
            #:backend backend))))
      ;; We should see nothing more than these 3 packages.
      (let-values (((nodes edges) (nodes+edges)))
        (and (equal? nodes (map package->tuple (list p3 p2 p1)))
             (equal? edges
                     (map edge->tuple
                          (list p3 p3 p2 p1)
                          (list p2 p1 p1 p3))))))))

(test-assert "reverse package DAG"
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (run-with-store %store
      (export-graph (list libunistring) 'port
                    #:node-type %reverse-package-node-type
                    #:backend backend))
    ;; We should see nothing more than these 3 packages.
    (let-values (((nodes edges) (nodes+edges)))
      (and (member (package->tuple guile-2.0) nodes)
           (->bool (member (edge->tuple libunistring guile-2.0) edges))))))

(test-assert "bag-emerged DAG"
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (let* ((o        (dummy-origin (method (lambda _
                                             (text-file "foo" "bar")))))
           (p        (dummy-package "p" (source o)))
           (implicit (map (match-lambda
                            ((label package) package)
                            ((label package output) package))
                          (standard-packages))))
      (run-with-store %store
        (export-graph (list p) 'port
                      #:node-type %bag-emerged-node-type
                      #:backend backend))
      ;; We should see exactly P and IMPLICIT, with one edge from P to each
      ;; element of IMPLICIT.  O must not appear among NODES.  Note: IMPLICIT
      ;; contains "glibc" twice, once for "out" and a second time for
      ;; "static", hence the 'delete-duplicates' call below.
      (let-values (((nodes edges) (nodes+edges)))
        (and (equal? (match nodes
                       (((labels names) ...)
                        names))
                     (map package-full-name
                          (cons p (delete-duplicates implicit))))
             (equal? (match edges
                       (((sources destinations) ...)
                        (zip (map store-path-package-name sources)
                             (map store-path-package-name destinations))))
                     (map (lambda (destination)
                            (list "p-0.drv"
                                  (string-append
                                   (package-full-name destination "-")
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
               (match (%bootstrap-inputs)
                 (((labels packages) ...)
                  (map package-full-name (filter package? packages)))))))))

(test-assert "bag DAG, including origins"
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (let* ((m (lambda* (uri hash-type hash name #:key system)
                (text-file "foo-1.2.3.tar.gz" "This is a fake!")))
           (o (origin
                (method m) (uri "the-uri")
                (sha256
                 (base32
                  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
           (p (dummy-package "p" (source o))))
      (run-with-store %store
        (export-graph (list p) 'port
                      #:node-type %bag-with-origins-node-type
                      #:backend backend))
      ;; We should see O among the nodes, with an edge coming from P.
      (let-values (((nodes edges) (nodes+edges)))
        (run-with-store %store
          (mlet %store-monad ((o* (lower-object o))
                              (p* (lower-object p))
                              (g  (lower-object (default-guile))))
            (return
             (and (find (match-lambda
                          ((file "the-uri") #t)
                          (_                #f))
                        nodes)
                  (find (match-lambda
                          ((source target)
                           (and (string=? source (derivation-file-name p*))
                                (string=? target o*))))
                        edges)

                  ;; There must also be an edge from O to G.
                  (find (match-lambda
                          ((source target)
                           (and (string=? source o*)
                                (string=? target (derivation-file-name g)))))
                        edges)))))))))

(test-assert "reverse bag DAG"
  (let-values (((dune bap ocaml-base)
                (values (specification->package "ocaml4.07-dune")
                        (specification->package "bap")
                        (specification->package "ocaml4.07-base")))
               ((backend nodes+edges) (make-recording-backend)))
    (run-with-store %store
      (export-graph (list dune) 'port
                    #:node-type %reverse-bag-node-type
                    #:backend backend))

    (run-with-store %store
      (mlet %store-monad ((dune-drv       (package->derivation dune))
                          (bap-drv        (package->derivation bap))
                          (ocaml-base-drv (package->derivation ocaml-base)))
        ;; OCAML-BASE uses 'dune-build-system' so DUNE is a direct dependency.
        ;; BAP is much higher in the stack but it should be there.
        (let-values (((nodes edges) (nodes+edges)))
          (return
           (and (member `(,(derivation-file-name bap-drv)
                          ,(package-full-name bap))
                        nodes)
                (->bool (member (map derivation-file-name
                                     (list dune-drv ocaml-base-drv))
                                edges)))))))))

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

(test-assert "referrer DAG"
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (run-with-store %store
      (mlet* %store-monad ((txt   (text-file "referrer-node" (random-text)))
                           (drv   (gexp->derivation "referrer"
                                                    #~(symlink #$txt #$output)))
                           (out -> (derivation->output-path drv)))
        ;; We should see only TXT and OUT, with an edge from the former to the
        ;; latter.
        (mbegin %store-monad
          (built-derivations (list drv))
          (export-graph (list txt) 'port
                        #:node-type %referrer-node-type
                        #:backend backend)
          (let-values (((nodes edges) (nodes+edges)))
            (return
             (and (equal? (match nodes
                            (((ids labels) ...)
                             ids))
                          (list txt out))
                  (equal? edges `((,txt ,out)))))))))))

(test-assert "module graph"
  (let-values (((backend nodes+edges) (make-recording-backend)))
    (run-with-store %store
      (export-graph '((gnu packages guile)) 'port
                    #:node-type %module-node-type
                    #:backend backend))

    (let-values (((nodes edges) (nodes+edges)))
      (and (member '(gnu packages guile)
                   (match nodes
                     (((ids labels) ...) ids)))
           (->bool (and (member (list '(gnu packages guile)
                                      '(gnu packages libunistring))
                                edges)
                        (member (list '(gnu packages guile)
                                      '(gnu packages bdw-gc))
                                edges)))))))

(test-assert "node-edges"
  (run-with-store %store
    (let ((packages (fold-packages cons '())))
      (mlet %store-monad ((edges (node-edges %package-node-type packages)))
        (return (and (null? (edges hello))
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

(test-assert "node-transitive-edges, no duplicates"
  (run-with-store %store
    (let* ((p0  (dummy-package "p0"))
           (p1a (dummy-package "p1a" (inputs `(("p0" ,p0)))))
           (p1b (dummy-package "p1b" (inputs `(("p0" ,p0)))))
           (p2  (dummy-package "p2" (inputs `(("p1a" ,p1a) ("p1b" ,p1b))))))
      (mlet %store-monad ((edges (node-edges %package-node-type
                                             (list p2 p1a p1b p0))))
        (return (lset= eq? (node-transitive-edges (list p2) edges)
                       (list p1a p1b p0)))))))

(test-assert "node-transitive-edges, references"
  (run-with-store %store
    (mlet* %store-monad ((d0 (package->derivation %bootstrap-guile))
                         (d1 (gexp->derivation "d1"
                                               #~(begin
                                                   (mkdir #$output)
                                                   (symlink #$%bootstrap-guile
                                                            (string-append
                                                             #$output "/l")))))
                         (d2 (gexp->derivation "d2"
                                               #~(begin
                                                   (mkdir #$output)
                                                   (symlink #$d1
                                                            (string-append
                                                             #$output "/l")))))
                         (_  (built-derivations (list d2)))
                         (->node -> (node-type-convert %reference-node-type))
                         (o2      (->node (derivation->output-path d2)))
                         (o1      (->node (derivation->output-path d1)))
                         (o0      (->node (derivation->output-path d0)))
                         (edges   (node-edges %reference-node-type
                                              (append o0 o1 o2)))
                         (reqs    ((store-lift requisites) o2)))
      (return (lset= string=?
                     (append o2 (node-transitive-edges o2 edges)) reqs)))))

(test-equal "node-reachable-count"
  '(3 3)
  (run-with-store %store
    (let* ((p0  (dummy-package "p0"))
           (p1a (dummy-package "p1a" (inputs `(("p0" ,p0)))))
           (p1b (dummy-package "p1b" (inputs `(("p0" ,p0)))))
           (p2  (dummy-package "p2" (inputs `(("p1a" ,p1a) ("p1b" ,p1b))))))
      (mlet* %store-monad ((all -> (list p2 p1a p1b p0))
                           (edges  (node-edges %package-node-type all))
                           (back   (node-back-edges %package-node-type all)))
        (return (list (node-reachable-count (list p2) edges)
                      (node-reachable-count (list p0) back)))))))

(test-equal "shortest-path, packages + derivations"
  '(("p5" "p4" "p1" "p0")
    ("p3" "p2" "p1" "p0")
    #f
    ("p5-0.drv" "p4-0.drv" "p1-0.drv" "p0-0.drv"))
  (run-with-store %store
    (let* ((p0 (dummy-package "p0"))
           (p1 (dummy-package "p1" (inputs `(("p0" ,p0)))))
           (p2 (dummy-package "p2" (inputs `(("p1" ,p1)))))
           (p3 (dummy-package "p3" (inputs `(("p2" ,p2)))))
           (p4 (dummy-package "p4" (inputs `(("p1" ,p1)))))
           (p5 (dummy-package "p5" (inputs `(("p4" ,p4) ("p3" ,p3))))))
      (mlet* %store-monad ((path1 (shortest-path p5 p0 %package-node-type))
                           (path2 (shortest-path p3 p0 %package-node-type))
                           (nope  (shortest-path p3 p4 %package-node-type))
                           (drv5  (package->derivation p5))
                           (drv0  (package->derivation p0))
                           (path3 (shortest-path drv5 drv0
                                                 %derivation-node-type)))
        (return (append (map (lambda (path)
                               (and path (map package-name path)))
                             (list path1 path2 nope))
                        (list (map (node-type-label %derivation-node-type)
                                   path3))))))))

(test-equal "shortest-path, reverse packages"
  '("libffi" "guile" "guile-json")
  (run-with-store %store
    (mlet %store-monad ((path (shortest-path (specification->package "libffi")
                                             guile-json
                                             %reverse-package-node-type)))
      (return (map package-name path)))))

(test-equal "shortest-path, references"
  `(("d2" "d1" ,(package-full-name %bootstrap-guile "-"))
    (,(package-full-name %bootstrap-guile "-") "d1" "d2"))
  (run-with-store %store
    (mlet* %store-monad ((d0 (package->derivation %bootstrap-guile))
                         (d1 (gexp->derivation "d1"
                                               #~(begin
                                                   (mkdir #$output)
                                                   (symlink #$%bootstrap-guile
                                                            (string-append
                                                             #$output "/l")))))
                         (d2 (gexp->derivation "d2"
                                               #~(begin
                                                   (mkdir #$output)
                                                   (symlink #$d1
                                                            (string-append
                                                             #$output "/l")))))
                         (_  (built-derivations (list d2)))
                         (->node -> (node-type-convert %reference-node-type))
                         (o2   (->node (derivation->output-path d2)))
                         (o0   (->node (derivation->output-path d0)))
                         (path (shortest-path (first o2) (first o0)
                                              %reference-node-type))
                         (rpath (shortest-path (first o0) (first o2)
                                               %referrer-node-type)))
      (return (list (map (node-type-label %reference-node-type) path)
                    (map (node-type-label %referrer-node-type) rpath))))))

(test-end "graph")
