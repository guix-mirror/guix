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

(define-module (guix graph)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix sets)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (node-type
            node-type?
            node-type-identifier
            node-type-label
            node-type-edges
            node-type-convert
            node-type-name
            node-type-description

            node-edges
            node-back-edges
            node-transitive-edges

            %graphviz-backend
            graph-backend?
            graph-backend

            export-graph))

;;; Commentary:
;;;
;;; This module provides an abstract way to represent graphs and to manipulate
;;; them.  It comes with several such representations for packages,
;;; derivations, and store items.  It also provides a generic interface for
;;; exporting graphs in an external format, including a Graphviz
;;; implementation thereof.
;;;
;;; Code:


;;;
;;; Node types.
;;;

(define-record-type* <node-type> node-type make-node-type
  node-type?
  (identifier  node-type-identifier)              ;node -> M identifier
  (label       node-type-label)                   ;node -> string
  (edges       node-type-edges)                   ;node -> M list of nodes
  (convert     node-type-convert                  ;package -> M list of nodes
               (default (lift1 list %store-monad)))
  (name        node-type-name)                    ;string
  (description node-type-description))            ;string

(define (%node-edges type nodes cons-edge)
  (with-monad %store-monad
    (match type
      (($ <node-type> identifier label node-edges)
       (define (add-edge node edges)
         (>>= (node-edges node)
              (lambda (nodes)
                (return (fold (cut cons-edge node <> <>)
                              edges nodes)))))

       (mlet %store-monad ((edges (foldm %store-monad
                                         add-edge vlist-null nodes)))
         (return (lambda (node)
                   (reverse (vhash-foldq* cons '() node edges)))))))))

(define (node-edges type nodes)
  "Return, as a monadic value, a one-argument procedure that, given a node of TYPE,
returns its edges.  NODES is taken to be the sinks of the global graph."
  (%node-edges type nodes
               (lambda (source target edges)
                 (vhash-consq source target edges))))

(define (node-back-edges type nodes)
  "Return, as a monadic value, a one-argument procedure that, given a node of TYPE,
returns its back edges.  NODES is taken to be the sinks of the global graph."
  (%node-edges type nodes
               (lambda (source target edges)
                 (vhash-consq target source edges))))

(define (node-transitive-edges nodes node-edges)
  "Return the list of nodes directly or indirectly connected to NODES
according to the NODE-EDGES procedure.  NODE-EDGES must be a one-argument
procedure that, given a node, returns its list of direct dependents; it is
typically returned by 'node-edges' or 'node-back-edges'."
  (let loop ((nodes   (append-map node-edges nodes))
             (result  '())
             (visited (setq)))
    (match nodes
      (()
       result)
      ((head . tail)
       (if (set-contains? visited head)
           (loop tail result visited)
           (let ((edges (node-edges head)))
             (loop (append edges tail)
                   (cons head result)
                   (set-insert head visited))))))))


;;;
;;; Graphviz export.
;;;

(define-record-type <graph-backend>
  (graph-backend prologue epilogue node edge)
  graph-backend?
  (prologue graph-backend-prologue)
  (epilogue graph-backend-epilogue)
  (node     graph-backend-node)
  (edge     graph-backend-edge))

(define (emit-prologue name port)
  (format port "digraph \"Guix ~a\" {\n"
          name))
(define (emit-epilogue port)
  (display "\n}\n" port))
(define (emit-node id label port)
  (format port "  \"~a\" [label = \"~a\", shape = box, fontname = Helvetica];~%"
          id label))
(define (emit-edge id1 id2 port)
  (format port "  \"~a\" -> \"~a\" [color = red];~%"
          id1 id2))

(define %graphviz-backend
  (graph-backend emit-prologue emit-epilogue
                 emit-node emit-edge))

(define* (export-graph sinks port
                       #:key
                       reverse-edges? node-type
                       (backend %graphviz-backend))
  "Write to PORT the representation of the DAG with the given SINKS, using the
given BACKEND.  Use NODE-TYPE to traverse the DAG.  When REVERSE-EDGES? is
true, draw reverse arrows."
  (match backend
    (($ <graph-backend> emit-prologue emit-epilogue emit-node emit-edge)
     (emit-prologue (node-type-name node-type) port)

     (match node-type
       (($ <node-type> node-identifier node-label node-edges)
        (let loop ((nodes   sinks)
                   (visited (set)))
          (match nodes
            (()
             (with-monad %store-monad
               (emit-epilogue port)
               (store-return #t)))
            ((head . tail)
             (mlet %store-monad ((id (node-identifier head)))
               (if (set-contains? visited id)
                   (loop tail visited)
                   (mlet* %store-monad ((dependencies (node-edges head))
                                        (ids          (mapm %store-monad
                                                            node-identifier
                                                            dependencies)))
                     (emit-node id (node-label head) port)
                     (for-each (lambda (dependency dependency-id)
                                 (if reverse-edges?
                                     (emit-edge dependency-id id port)
                                     (emit-edge id dependency-id port)))
                               dependencies ids)
                     (loop (append dependencies tail)
                           (set-insert id visited)))))))))))))

;;; graph.scm ends here
