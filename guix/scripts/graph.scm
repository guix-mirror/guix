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

(define-module (guix scripts graph)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module ((guix build-system gnu) #:select (standard-packages))
  #:use-module (gnu packages)
  #:use-module (guix sets)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (%package-node-type
            %bag-node-type
            %bag-emerged-node-type
            %derivation-node-type
            %reference-node-type
            %node-types

            node-type
            node-type?
            node-type-identifier
            node-type-label
            node-type-edges
            node-type-convert
            node-type-name
            node-type-description

            %graphviz-backend
            graph-backend?
            graph-backend

            export-graph

            guix-graph))


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


;;;
;;; Package DAG.
;;;

(define (node-full-name thing)
  "Return a human-readable name to denote THING, a package, origin, or file
name."
  (cond ((package? thing)
         (package-full-name thing))
        ((origin? thing)
         (origin-actual-file-name thing))
        ((string? thing)                          ;file name
         (or (basename thing)
             (error "basename" thing)))
        (else
         (number->string (object-address thing) 16))))

(define (package-node-edges package)
  "Return the list of dependencies of PACKAGE."
  (match (package-direct-inputs package)
    (((labels packages . outputs) ...)
     ;; Filter out origins and other non-package dependencies.
     (filter package? packages))))

(define %package-node-type
  ;; Type for the traversal of package nodes.
  (node-type
   (name "package")
   (description "the DAG of packages, excluding implicit inputs")

   ;; We use package addresses as unique identifiers.  This generally works
   ;; well, but for generated package objects, we could end up with two
   ;; packages that are not 'eq?', yet map to the same derivation (XXX).
   (identifier (lift1 object-address %store-monad))
   (label node-full-name)
   (edges (lift1 package-node-edges %store-monad))))


;;;
;;; Package DAG using bags.
;;;

(define (bag-node-identifier thing)
  "Return a unique identifier for THING, which may be a package, origin, or a
file name."
  ;; If THING is a file name (a string), we just return it; if it's a package
  ;; or origin, we return its address.  That gives us the object graph, but
  ;; that may differ from the derivation graph (for instance,
  ;; 'package-with-bootstrap-guile' generates fresh package objects, and
  ;; several packages that are not 'eq?' may actually map to the same
  ;; derivation.)  Thus, we lower THING and use its derivation file name as a
  ;; unique identifier.
  (with-monad %store-monad
    (if (string? thing)
        (return thing)
        (mlet %store-monad ((low (lower-object thing)))
          (return (if (derivation? low)
                      (derivation-file-name low)
                      low))))))

(define (bag-node-edges thing)
  "Return the list of dependencies of THING, a package or origin, etc."
  (if (package? thing)
      (match (bag-direct-inputs (package->bag thing))
        (((labels things . outputs) ...)
         (filter-map (match-lambda
                       ((? package? p) p)
                       ;; XXX: Here we choose to filter out origins, files,
                       ;; etc.  Replace "#f" with "x" to reinstate them.
                       (x #f))
                     things)))
      '()))

(define %bag-node-type
  ;; Type for the traversal of package nodes via the "bag" representation,
  ;; which includes implicit inputs.
  (node-type
   (name "bag")
   (description "the DAG of packages, including implicit inputs")
   (identifier bag-node-identifier)
   (label node-full-name)
   (edges (lift1 bag-node-edges %store-monad))))

(define standard-package-set
  (memoize
   (lambda ()
     "Return the set of standard packages provided by GNU-BUILD-SYSTEM."
     (match (standard-packages)
       (((labels packages . output) ...)
        (list->setq packages))))))

(define (bag-node-edges-sans-bootstrap thing)
  "Like 'bag-node-edges', but pretend that the standard packages of
GNU-BUILD-SYSTEM have zero dependencies."
  (if (set-contains? (standard-package-set) thing)
      '()
      (bag-node-edges thing)))

(define %bag-emerged-node-type
  ;; Like %BAG-NODE-TYPE, but without the bootstrap subset of the DAG.
  (node-type
   (name "bag-emerged")
   (description "same as 'bag', but without the bootstrap nodes")
   (identifier bag-node-identifier)
   (label node-full-name)
   (edges (lift1 bag-node-edges-sans-bootstrap %store-monad))))


;;;
;;; Derivation DAG.
;;;

(define (file->derivation file)
  "Read the derivation from FILE and return it."
  (call-with-input-file file read-derivation))

(define (derivation-dependencies obj)
  "Return the <derivation> objects and store items corresponding to the
dependencies of OBJ, a <derivation> or store item."
  (if (derivation? obj)
      (append (map (compose file->derivation derivation-input-path)
                   (derivation-inputs obj))
              (derivation-sources obj))
      '()))

(define (derivation-node-identifier node)
  "Return a unique identifier for NODE, which may be either a <derivation> or
a plain store file."
  (if (derivation? node)
      (derivation-file-name node)
      node))

(define (derivation-node-label node)
  "Return a label for NODE, a <derivation> object or plain store item."
  (store-path-package-name (match node
                             ((? derivation? drv)
                              (derivation-file-name drv))
                             ((? string? file)
                              file))))

(define %derivation-node-type
  ;; DAG of derivations.  Very accurate, very detailed, but usually too much
  ;; detailed.
  (node-type
   (name "derivation")
   (description "the DAG of derivations")
   (convert (lambda (package)
              (with-monad %store-monad
                (>>= (package->derivation package)
                     (lift1 list %store-monad)))))
   (identifier (lift1 derivation-node-identifier %store-monad))
   (label derivation-node-label)
   (edges (lift1 derivation-dependencies %store-monad))))


;;;
;;; DAG of residual references (aka. run-time dependencies).
;;;

(define (references* item)
  "Return as a monadic value the references of ITEM, based either on the
information available in the local store or using information about
substitutes."
  (lambda (store)
    (guard (c ((nix-protocol-error? c)
               (match (substitutable-path-info store (list item))
                 ((info)
                  (values (substitutable-references info) store))
                 (()
                  (leave (_ "references for '~a' are not known~%")
                         item)))))
      (values (references store item) store))))

(define %reference-node-type
  (node-type
   (name "references")
   (description "the DAG of run-time dependencies (store references)")
   (convert (lambda (package)
              ;; Return the output file names of PACKAGE.
              (mlet %store-monad ((drv (package->derivation package)))
                (return (match (derivation->output-paths drv)
                          (((_ . file-names) ...)
                           file-names))))))
   (identifier (lift1 identity %store-monad))
   (label store-path-package-name)
   (edges references*)))


;;;
;;; List of node types.
;;;

(define %node-types
  ;; List of all the node types.
  (list %package-node-type
        %bag-node-type
        %bag-emerged-node-type
        %derivation-node-type
        %reference-node-type))

(define (lookup-node-type name)
  "Return the node type called NAME.  Raise an error if it is not found."
  (or (find (lambda (type)
              (string=? (node-type-name type) name))
            %node-types)
      (leave (_ "~a: unknown node type~%") name)))

(define (list-node-types)
  "Print the available node types along with their synopsis."
  (display (_ "The available node types are:\n"))
  (newline)
  (for-each (lambda (type)
              (format #t "  - ~a: ~a~%"
                      (node-type-name type)
                      (node-type-description type)))
            %node-types))


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
                       (node-type %package-node-type)
                       (backend %graphviz-backend))
  "Write to PORT the representation of the DAG with the given SINKS, using the
given BACKEND.  Use NODE-TYPE to traverse the DAG."
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
                                 (emit-edge id dependency-id port))
                               dependencies ids)
                     (loop (append dependencies tail)
                           (set-insert id visited)))))))))))))


;;;
;;; Command-line options.
;;;

(define %options
  (list (option '(#\t "type") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'node-type (lookup-node-type arg)
                              result)))
        (option '("list-types") #f #f
                (lambda (opt name arg result)
                  (list-node-types)
                  (exit 0)))
        (option '(#\e "expression") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'expression arg result)))
        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix edit")))))

(define (show-help)
  ;; TRANSLATORS: Here 'dot' is the name of a program; it must not be
  ;; translated.
  (display (_ "Usage: guix graph PACKAGE...
Emit a Graphviz (dot) representation of the dependencies of PACKAGE...\n"))
  (display (_ "
  -t, --type=TYPE        represent nodes of the given TYPE"))
  (display (_ "
      --list-types       list the available graph types"))
  (display (_ "
  -e, --expression=EXPR  consider the package EXPR evaluates to"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %default-options
  `((node-type . ,%package-node-type)))


;;;
;;; Entry point.
;;;

(define (guix-graph . args)
  (with-error-handling
    (let* ((opts     (parse-command-line args %options
                                         (list %default-options)))
           (type     (assoc-ref opts 'node-type))
           (packages (filter-map (match-lambda
                                   (('argument . spec)
                                    (specification->package spec))
                                   (('expression . exp)
                                    (read/eval-package-expression exp))
                                   (_ #f))
                                 opts)))
      (with-store store
        (run-with-store store
          (mlet %store-monad ((nodes (mapm %store-monad
                                           (node-type-convert type)
                                           packages)))
            (export-graph (concatenate nodes)
                          (current-output-port)
                          #:node-type type))))))
  #t)

;;; graph.scm ends here
