;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Julien Lepiller <julien@lepiller.eu>
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

(define-module (guix build maven plugin)
  #:use-module (guix build maven java)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (generate-mojo-from-files
	    default-convert-type
	    maven-convert-type))

(define-record-type mojo
  (make-mojo package name goal description requires-dependency-collection
             requires-dependency-resolution requires-direct-invocation?
             requires-project? requires-reports? aggregator? requires-online?
             inherited-by-default?  instantiation-strategy execution-strategy
             since thread-safe? phase parameters components)
  mojo?
  (package mojo-package)
  (name mojo-name)
  (goal mojo-goal)
  (description mojo-description)
  (requires-dependency-collection mojo-requires-dependency-collection)
  (requires-dependency-resolution mojo-requires-dependency-resolution)
  (requires-direct-invocation? mojo-requires-direct-invocation?)
  (requires-project? mojo-requires-project?)
  (requires-reports? mojo-requires-reports?)
  (aggregator? mojo-aggregator?)
  (requires-online? mojo-requires-online?)
  (inherited-by-default? mojo-inherited-by-default?)
  (instantiation-strategy mojo-instantiation-strategy)
  (execution-strategy mojo-execution-strategy)
  (since mojo-since)
  (thread-safe? mojo-thread-safe?)
  (phase mojo-phase)
  (parameters mojo-parameters)
  (components mojo-components))

(define* (update-mojo mojo
           #:key
           (package (mojo-package mojo))
           (name (mojo-name mojo))
           (goal (mojo-goal mojo))
           (description (mojo-description mojo))
           (requires-dependency-collection (mojo-requires-dependency-collection mojo))
           (requires-dependency-resolution (mojo-requires-dependency-resolution mojo))
           (requires-direct-invocation? (mojo-requires-direct-invocation? mojo))
           (requires-project? (mojo-requires-project? mojo))
           (requires-reports? (mojo-requires-reports? mojo))
           (aggregator? (mojo-aggregator? mojo))
           (requires-online? (mojo-requires-online? mojo))
           (inherited-by-default? (mojo-inherited-by-default? mojo))
           (instantiation-strategy (mojo-instantiation-strategy mojo))
           (execution-strategy (mojo-execution-strategy mojo))
           (since (mojo-since mojo))
           (thread-safe? (mojo-thread-safe? mojo))
           (phase (mojo-phase mojo))
           (parameters (mojo-parameters mojo))
           (components (mojo-components mojo)))
  (make-mojo package name goal description requires-dependency-collection
             requires-dependency-resolution requires-direct-invocation?
             requires-project? requires-reports? aggregator? requires-online?
             inherited-by-default? instantiation-strategy execution-strategy
             since thread-safe? phase parameters components))

(define-record-type mojo-parameter
  (make-mojo-parameter name type since required editable property description
                       configuration)
  mojo-parameter?
  (name          mojo-parameter-name)
  (type          mojo-parameter-type)
  (since         mojo-parameter-since)
  (required      mojo-parameter-required)
  (editable      mojo-parameter-editable)
  (property      mojo-parameter-property)
  (description   mojo-parameter-description)
  (configuration mojo-parameter-configuration))

(define* (update-mojo-parameter mojo-parameter
           #:key (name (mojo-parameter-name mojo-parameter))
                 (type (mojo-parameter-type mojo-parameter))
                 (since (mojo-parameter-since mojo-parameter))
                 (required (mojo-parameter-required mojo-parameter))
                 (editable (mojo-parameter-editable mojo-parameter))
                 (property (mojo-parameter-property mojo-parameter))
                 (description (mojo-parameter-description mojo-parameter))
                 (configuration (mojo-parameter-configuration mojo-parameter)))
  (make-mojo-parameter name type since required editable property description
                       configuration))

(define-record-type <mojo-component>
  (make-mojo-component field role hint)
  mojo-component?
  (field mojo-component-field)
  (role  mojo-component-role)
  (hint  mojo-component-hint))

(define* (update-mojo-component mojo-component
           #:key (field (mojo-component-field mojo-component))
                 (role (mojo-component-role mojo-component))
                 (hint (mojo-component-hint mojo-component)))
  (make-mojo-component field role hint))

(define (generate-mojo-parameter mojo-parameter)
  `(parameter (name ,(mojo-parameter-name mojo-parameter))
              (type ,(mojo-parameter-type mojo-parameter))
              ,@(if (mojo-parameter-since mojo-parameter)
                    `(since (mojo-parameter-since mojo-parameter))
                    '())
              (required ,(if (mojo-parameter-required mojo-parameter) "true" "false"))
              (editable ,(if (mojo-parameter-editable mojo-parameter) "true" "false"))
              (description ,(mojo-parameter-description mojo-parameter))))

(define (generate-mojo-configuration mojo-parameter)
  (let ((config (mojo-parameter-configuration mojo-parameter)))
    (if (or config (mojo-parameter-property mojo-parameter))
        `(,(string->symbol (mojo-parameter-name mojo-parameter))
           (@ ,@(cons (list 'implementation (mojo-parameter-type mojo-parameter))
                      (or config '())))
          ,@(if (mojo-parameter-property mojo-parameter)
                (list (string-append "${" (mojo-parameter-property mojo-parameter)
                                     "}"))
                '()))
        #f)))

(define (generate-mojo-component mojo-component)
  (let ((role (mojo-component-role mojo-component))
        (field (mojo-component-field mojo-component))
        (hint (mojo-component-hint mojo-component)))
    `(requirement
       (role ,role)
       ,@(if hint
          `((role-hint ,hint))
          '())
       (field-name ,field))))

(define (generate-mojo mojo)
  `(mojo
     (goal ,(mojo-goal mojo))
     (description ,(mojo-description mojo))
     ,@(let ((val (mojo-requires-dependency-collection mojo)))
         (if val
             `((requiresDependencyCollection ,val))
             '()))
     ,@(let ((val (mojo-requires-dependency-resolution mojo)))
         (if val
             `((requiresDependencyResolution ,val))
             '()))
     ,@(let ((val (mojo-requires-direct-invocation? mojo)))
         (if val
             `((requiresDirectInvocation ,val))
             '()))
     ,@(let ((val (mojo-requires-project? mojo)))
         (if val
             `((requiresProject ,val))
             '()))
     ,@(let ((val (mojo-requires-reports? mojo)))
         (if val
             `((requiresReports ,val))
             '()))
     ,@(let ((val (mojo-aggregator? mojo)))
         (if val
             `((aggregator ,val))
             '()))
     ,@(let ((val (mojo-requires-online? mojo)))
         (if val
             `((requiresOnline ,val))
             '()))
     ,@(let ((val (mojo-inherited-by-default? mojo)))
         (if val
             `((inheritedByDefault ,val))
             '()))
     ,@(let ((phase (mojo-phase mojo)))
             (if phase
                 `((phase ,phase))
                 '()))
     (implementation ,(string-append (mojo-package mojo) "." (mojo-name mojo)))
     (language "java")
     (instantiationStrategy ,(mojo-instantiation-strategy mojo))
     (executionStrategy ,(mojo-execution-strategy mojo))
     ,@(let ((since (mojo-since mojo)))
             (if since
                 `((since ,since))
                 '()))
     ,@(let ((val (mojo-thread-safe? mojo)))
         (if val
             `((threadSafe ,val))
             '()))
     (parameters
       ,(map generate-mojo-parameter (mojo-parameters mojo)))
     (configuration
       ,@(filter (lambda (a) a) (map generate-mojo-configuration (mojo-parameters mojo))))
     (requirements
       ,@(map generate-mojo-component (mojo-components mojo)))))


(define (default-convert-type type)
  (cond
    ((equal? type "String") "java.lang.String")
    ((equal? type "String[]") "java.lang.String[]")
    ((equal? type "File") "java.io.File")
    ((equal? type "File[]") "java.io.File[]")
    ((equal? type "List") "java.util.List")
    ((equal? type "Boolean") "java.lang.Boolean")
    ((equal? type "Properties") "java.util.Properties")
    ((and (> (string-length type) 5)
          (equal? (substring type 0 4) "Map<"))
     "java.util.Map")
    ((and (> (string-length type) 6)
          (equal? (substring type 0 5) "List<"))
     "java.util.List")
    ((and (> (string-length type) 15)
          (equal? (substring type 0 14) "LinkedHashSet<"))
     "java.util.LinkedHashSet")
    (else type)))

(define (maven-convert-type type)
  (cond
    ((equal? type "MavenProject")
     "org.apache.maven.project.MavenProject")
    (else (default-convert-type type))))

(define (update-mojo-from-file mojo file convert-type)
  (define parse-tree (parse-java-file file))

  (define (update-mojo-from-attrs mojo attrs)
    (let loop ((mojo mojo) (attrs attrs))
      (match attrs
        ('() mojo)
        ((attr attrs ...)
         (match attr
           (('annotation-attr ('attr-name name) ('attr-value value))
            (cond
              ((equal? name "name")
               (loop (update-mojo mojo #:goal value) attrs))
              ((equal? name "defaultPhase")
               (let* ((phase (car (reverse (string-split value #\.))))
                      (phase (string-downcase phase))
                      (phase (string-join (string-split phase #\_) "-")))
               (loop (update-mojo mojo #:phase phase) attrs)))
              ((equal? name "requiresProject")
               (loop (update-mojo mojo #:requires-project? value) attrs))
              ((equal? name "threadSafe")
               (loop (update-mojo mojo #:thread-safe? value) attrs))
              ((equal? name "aggregator")
               (loop (update-mojo mojo #:aggregator? value) attrs))
              ((equal? name "requiresDependencyCollection")
               (loop
                 (update-mojo mojo #:requires-dependency-collection
                              (match value
                                ("ResolutionScope.COMPILE" "compile")
                                ("ResolutionScope.COMPILE_PLUS_RUNTIME"
                                 "compile+runtime")
                                ("ResolutionScope.RUNTIME" "runtime")
                                ("ResolutionScope.RUNTIME_PLUS_SYSTEM"
                                 "runtime+system")
                                ("ResolutionScope.TEST" "test")
                                ("ResolutionScope.PROVIDED" "provided")
                                ("ResolutionScope.SYSTEM" "system")
                                ("ResolutionScope.IMPORT" "import")))
                 attrs))
              ((equal? name "requiresDependencyResolution")
               (loop
                 (update-mojo mojo #:requires-dependency-resolution
                              (match value
                                ("ResolutionScope.COMPILE" "compile")
                                ("ResolutionScope.COMPILE_PLUS_RUNTIME"
                                 "compile+runtime")
                                ("ResolutionScope.RUNTIME" "runtime")
                                ("ResolutionScope.RUNTIME_PLUS_SYSTEM"
                                 "runtime+system")
                                ("ResolutionScope.TEST" "test")
                                ("ResolutionScope.PROVIDED" "provided")
                                ("ResolutionScope.SYSTEM" "system")
                                ("ResolutionScope.IMPORT" "import")))
                 attrs))
              (else
                (throw 'not-found-attr name))))
           ((attrs ...) (loop mojo attrs))
           (_ (loop mojo attrs)))))))

  (define (string->attr name)
    (define (string-split-upper s)
      (let ((i (string-index s char-set:upper-case)))
        (if (and i (> i 0))
            (cons (substring s 0 i) (string-split-upper (substring s i)))
            (list s))))
    (string->symbol
      (string-join (map string-downcase (string-split-upper name)) "-")))

  (define (update-mojo-parameter-from-attrs mojo-parameter attrs)
    (match attrs
      ('() mojo-parameter)
      (('annotation-attr ('attr-name name) 'attr-value)
       mojo-parameter)
       ;(update-mojo-parameter-from-attrs mojo-parameter
       ;  `(annotation-attr (attr-name ,name) (attr-value ""))))
      (('annotation-attr ('attr-name name) ('attr-value value))
       (cond
         ((equal? name "editable")
          (update-mojo-parameter mojo-parameter #:editable value))
         ((equal? name "required")
          (update-mojo-parameter mojo-parameter #:required value))
         ((equal? name "property")
          (update-mojo-parameter mojo-parameter #:property value))
         (else
           (update-mojo-parameter mojo-parameter
                                  #:configuration
                                  (cons
                                    (list (string->attr name) value)
                                    (or
				      (mojo-parameter-configuration mojo-parameter)
				      '()))))))
      ((attr attrs ...)
       (update-mojo-parameter-from-attrs
         (update-mojo-parameter-from-attrs mojo-parameter attr)
         attrs))))

  (define (update-mojo-component-from-attrs mojo-component inverse-import attrs)
    (match attrs
      ('() mojo-component)
      ((attr attrs ...)
       (match attr
         (('annotation-attr ('attr-name name) ('attr-value value))
          (cond
            ((equal? name "role")
             (update-mojo-component-from-attrs
               (update-mojo-component mojo-component
                 #:role (select-import inverse-import value convert-type))
               inverse-import
               attrs))
            ((equal? name "hint")
             (update-mojo-component-from-attrs
               (update-mojo-component mojo-component #:hint value)
               inverse-import
               attrs))
            (else (throw 'not-found-attr name))))
         ((attrss ...)
          (update-mojo-component-from-attrs
            mojo-component inverse-import (append attrss attrs)))))))

  (define (add-mojo-parameter parameters name type last-comment attrs inverse-import)
    (let loop ((parameters parameters))
      (match parameters
        ('() (list (update-mojo-parameter-from-attrs
                     (make-mojo-parameter
                       ;; name convert since required editable property comment config
                       name (select-import inverse-import type convert-type)
                       #f #f #t #f last-comment #f)
                     attrs)))
        ((parameter parameters ...)
         (if (equal? (mojo-parameter-name parameter) name)
             (cons (update-mojo-parameter-from-attrs
                     (make-mojo-parameter
                       name (select-import inverse-import type convert-type)
                       #f #f #t #f last-comment #f)
                     attrs) parameters)
             (cons parameter (loop parameters)))))))

  (define (update-mojo-from-class-content mojo inverse-import content)
    (let loop ((content content)
               (mojo mojo)
               (last-comment #f))
      (match content
        ('() mojo)
        ((('comment ('annotation-pat _ ...) last-comment) content ...)
         (loop content mojo last-comment))
        ((('comment last-comment) content ...)
         (loop content mojo last-comment))
        ((('param-pat ('annotation-pat annot-name attrs ...) ('type-name type)
           ('param-name name)) content ...)
         (cond
           ((equal? annot-name "Parameter")
            (loop content
                  (update-mojo mojo
                               #:parameters
                               (add-mojo-parameter
                                 (mojo-parameters mojo) name type last-comment
                                 attrs inverse-import))
                  #f))
           ((equal? annot-name "Component")
            (loop content
                  (update-mojo mojo
                               #:components
                               (cons (update-mojo-component-from-attrs
                                       (make-mojo-component
                                         name
                                         (select-import inverse-import type
                                                        convert-type)
                                         #f)
                                       inverse-import
                                       attrs)
                                     (mojo-components mojo)))
                  #f))
           (else (throw 'not-found-annot annot-name))))
        ((('class-pat _ ...) content ...)
         (loop content mojo #f))
        ((('param-pat _ ...) content ...)
         (loop content mojo #f))
        ((('method-pat _ ...) content ...)
         (loop content mojo #f)))))

  (define (update-inverse-import inverse-import package)
    (let ((package-name (car (reverse (string-split package #\.)))))
      (cons (cons package-name package) inverse-import)))

  (define (select-import inverse-import package convert-type)
    (let* ((package (car (string-split package #\<)))
           (package (string-split package #\.))
           (rest (reverse (cdr package)))
           (rest (cond
                   ((null? rest) '())
                   ((equal? (car rest) "class") (cdr rest))
                   (else rest)))
           (base (or (assoc-ref inverse-import (car package)) (car package))))
      (convert-type (string-join (cons base rest) "."))))

  (let loop ((content parse-tree)
             (mojo mojo)
             (inverse-import '())
             (last-comment #f))
    (if (null? content)
        mojo
        (match content
          ((tls content ...)
           (match tls
             (('package package)
              (loop content (update-mojo mojo #:package package) inverse-import
                    last-comment))
             (('import-pat package)
              (loop content mojo (update-inverse-import inverse-import package)
                    last-comment))
             (('comment last-comment)
              (loop content mojo inverse-import last-comment))
             (('class-pat class-tls ...)
              (let loop2 ((class-tls class-tls) (mojo mojo))
                (match class-tls
                  ('() (loop content mojo inverse-import #f))
                  (((? string? name) class-tls ...)
                   (loop2 class-tls (update-mojo mojo #:name name)))
                  ((('annotation-pat annot-name (attrs ...)) class-tls ...)
                   (loop2
                     class-tls
                     (update-mojo-from-attrs mojo attrs)))
                  ((('class-body class-content ...) class-tls ...)
                   (loop2
                     class-tls
                     (update-mojo-from-class-content
                       mojo inverse-import class-content)))
                  ((_ class-tls ...)
                   (loop2 class-tls mojo)))))
             (_
              (loop content mojo inverse-import last-comment))))))))

(define (generate-mojo-from-files convert-type . files)
  (let ((mojo (make-mojo #f #f #f #f #f #f #f #f #f #f #f #f "per-lookup"
                         "once-per-session" #f #f #f '() '())))
    (let loop ((files files) (mojo mojo))
      (if (null? files)
          (generate-mojo mojo)
          (loop
            (cdr files)
            (update-mojo-from-file
              (update-mojo mojo
                #:package #f
                #:name #f
                #:goal #f
                #:description #f
                #:requires-dependency-resolution #f
                #:requires-direct-invocation? #f
                #:requires-project? #f
                #:requires-reports? #f
                #:aggregator? #f
                #:requires-online? #f
                #:inherited-by-default? #f
                #:instantiation-strategy "per-lookup"
                #:execution-strategy "once-per-session"
                #:since #f
                #:thread-safe? #f
                #:phase #f)
              (car files)
	      convert-type))))))
