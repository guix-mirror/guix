;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019-2021 Julien Lepiller <julien@lepiller.eu>
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

(define-module (guix build maven pom)
  #:use-module (sxml simple)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (add-local-package
            get-pom
            pom-ref
            pom-description
            pom-name
            pom-version
            pom-artifactid
            pom-groupid
            pom-dependencies
            group->dir
            pom-and-submodules
            pom-local-packages
            fix-pom-dependencies))

(define (add-local-package local-packages group artifact version)
  "Takes @var{local-packages}, a list of local packages, and adds a new one
for @var{group}:@var{artifact} at @var{version}."
  (define (alist-set lst key val)
    (match lst
      ('() (list (cons key val)))
      (((k . v) lst ...)
       (if (equal? k key)
         (cons (cons key val) lst)
         (cons (cons k v) (alist-set lst key val))))))
  (alist-set local-packages group
    (alist-set (or (assoc-ref local-packages group) '()) artifact
      version)))

(define (get-pom file)
  "Return the content of a @file{.pom} file."
  (let ((pom-content (call-with-input-file file xml->sxml)))
    (match pom-content
      (('*TOP* _ (_ ('@ _ ...) content ...))
        content)
      (('*TOP* (_ ('@ _ ...) content ...))
       content)
      (('*TOP* _ (_ content ...))
        content)
      (('*TOP* (_ content ...))
       content))))

(define (pom-ref content attr)
  "Gets a value associated to @var{attr} in @var{content}, an sxml value that
represents a @file{.pom} file content, or parts of it."
  (or
    (assoc-ref
      content
      (string->symbol
        (string-append "http://maven.apache.org/POM/4.0.0:" attr)))
    (assoc-ref content (string->symbol attr))))

(define (get-parent content)
  (pom-ref content "parent"))

(define* (find-parent content inputs #:optional local-packages)
  "Find the parent pom for the pom file with @var{content} in a package's
@var{inputs}.  When the parent pom cannot be found in @var{inputs}, but
@var{local-packages} is defined, the parent pom is looked up in it.

@var{local-packages} is an association list of groupID to an association list
of artifactID to version number.

The result is an sxml document that describes the content of the parent pom, or
of an hypothetical parent pom if it was generated from @var{local-packages}.
If no result is found, the result is @code{#f}."
  (let ((parent (pom-ref content "parent")))
    (if parent
        (let* ((groupid (car (pom-ref parent "groupId")))
               (artifactid (car (pom-ref parent "artifactId")))
               (version (car (pom-ref parent "version")))
               (pom-file (string-append "lib/m2/" (group->dir groupid)
                                        "/" artifactid "/" version "/"
                                        artifactid "-" version ".pom"))
               (java-inputs (filter
                              (lambda (input)
                                (file-exists? (string-append input "/" pom-file)))
                              inputs))
               (java-inputs (map (lambda (input) (string-append input "/" pom-file))
                                 java-inputs)))
          (if (null? java-inputs)
              (let ((version (assoc-ref (assoc-ref local-packages groupid) artifactid)))
                (if version
                    `((groupId ,groupid)
                      (artifactId ,artifactid)
                      (version ,version))
                    #f))
              (get-pom (car java-inputs))))
        #f)))

(define* (pom-groupid content)
  "Find the groupID of a pom file, potentially looking at its parent pom file.
See @code{find-parent} for the meaning of the arguments."
  (if content
    (let ((res (or (pom-ref content "groupId")
                   (pom-ref (pom-ref content "parent") "groupId"))))
      (cond
        ((string? res) res)
        ((null? res) #f)
        ((list? res) (car res))
        (else #f)))
    #f))

(define (pom-artifactid content)
  "Find the artifactID of a pom file, from its sxml @var{content}."
  (let ((res (pom-ref content "artifactId")))
    (if (and res (>= (length res) 1))
      (car res)
      #f)))

(define* (pom-version content)
  "Find the version of a pom file, potentially looking at its parent pom file.
See @code{find-parent} for the meaning of the arguments."
  (if content
    (let ((res (or (pom-ref content "version")
                   (pom-ref (pom-ref content "parent") "version"))))
      (cond
        ((string? res) res)
        ((null? res) #f)
        ((list? res) (car res))
        (else #f)))
    #f))

(define (pom-name content)
  "Return the name of the package as contained in the sxml @var{content} of the
pom file."
  (let ((res (pom-ref content "name")))
    (if (and res (>= (length res) 1))
      (car res)
      #f)))

(define (pom-description content)
  "Return the description of the package as contained in the sxml @var{content}
of the pom file."
  (let ((res (pom-ref content "description")))
    (if (and res (>= (length res) 1))
      (car res)
      #f)))

(define (pom-dependencies content)
  "Return the list of dependencies listed in the sxml @var{content} of the pom
file."
  (filter
    (lambda (a) a)
    (map
      (match-lambda
        ((? string? _) #f)
        (('http://maven.apache.org/POM/4.0.0:dependency content ...)
         (let loop ((content content) (groupid #f) (artifactid #f) (version #f) (scope #f))
           (match content
             ('()
              `(dependency
                 (groupId ,groupid)
                 (artifactId ,artifactid)
                 (version ,version)
                 ,@(if scope `((scope ,scope)) '())))
             (((? string? _) content ...)
              (loop content groupid artifactid version scope))
             ((('http://maven.apache.org/POM/4.0.0:scope scope) content ...)
              (loop content groupid artifactid version scope))
             ((('http://maven.apache.org/POM/4.0.0:groupId groupid) content ...)
              (loop content groupid artifactid version scope))
             ((('http://maven.apache.org/POM/4.0.0:artifactId artifactid) content ...)
              (loop content groupid artifactid version scope))
             ((('http://maven.apache.org/POM/4.0.0:version version) content ...)
              (loop content groupid artifactid version scope))
             ((_ content ...)
              (loop content groupid artifactid version scope))))))
      (pom-ref content "dependencies"))))

(define version-compare
  (let ((strverscmp
         (let ((sym (or (dynamic-func "strverscmp" (dynamic-link))
                        (error "could not find `strverscmp' (from GNU libc)"))))
           (pointer->procedure int sym (list '* '*)))))
    (lambda (a b)
      "Return '> when A denotes a newer version than B,
'< when A denotes a older version than B,
or '= when they denote equal versions."
      (let ((result (strverscmp (string->pointer a) (string->pointer b))))
        (cond ((positive? result) '>)
              ((negative? result) '<)
              (else '=))))))

(define (version>? a b)
  "Return #t when A denotes a version strictly newer than B."
  (eq? '> (version-compare a b)))

(define (fix-maven-xml sxml)
  "When writing an xml file from an sxml representation, it is not possible to
use namespaces in tag names.  This procedure takes an @var{sxml} representation
of a pom file and removes the namespace uses.  It also adds the required bits
to re-declare the namespaces in the top-level element."
  (define (fix-xml sxml)
    (match sxml
      ((tag ('@ opts ...) rest ...)
       (if (> (string-length (symbol->string tag))
              (string-length "http://maven.apache.org/POM/4.0.0:"))
         (let* ((tag (symbol->string tag))
                (tag (substring tag (string-length
                                      "http://maven.apache.org/POM/4.0.0:")))
                (tag (string->symbol tag)))
           `(,tag (@ ,@opts) ,@(map fix-xml rest)))
         `(,tag (@ ,@opts) ,@(map fix-xml rest))))
      ((tag (rest ...))
       (if (> (string-length (symbol->string tag))
              (string-length "http://maven.apache.org/POM/4.0.0:"))
         (let* ((tag (symbol->string tag))
                (tag (substring tag (string-length
                                      "http://maven.apache.org/POM/4.0.0:")))
                (tag (string->symbol tag)))
           `(,tag ,@(map fix-xml rest)))
         `(,tag ,@(map fix-xml rest))))
      ((tag rest ...)
       (if (> (string-length (symbol->string tag))
              (string-length "http://maven.apache.org/POM/4.0.0:"))
         (let* ((tag (symbol->string tag))
                (tag (substring tag (string-length
                                      "http://maven.apache.org/POM/4.0.0:")))
                (tag (string->symbol tag)))
           `(,tag ,@(map fix-xml rest)))
         `(,tag ,@(map fix-xml rest))))
      (_ sxml)))

  `((*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
     (project (@ (xmlns "http://maven.apache.org/POM/4.0.0")
                 (xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance")
                 (xmlns:schemaLocation "http://maven.apache.org/POM/4.0.0
                   http://maven.apache.org/xsd/maven-4.0.0.xsd"))
       ,(map fix-xml sxml)))))

(define (pom-and-submodules pom-file)
  "Given @var{pom-file}, the file name of a pom, return the list of pom file
names that correspond to itself and its submodules, recursively."
  (define (get-modules modules)
    (match modules
      (#f '())
      ('() '())
      (((? string? _) rest ...) (get-modules rest))
      ((('http://maven.apache.org/POM/4.0.0:module mod) rest ...)
       (let ((pom (string-append (dirname pom-file) "/" mod "/pom.xml")))
         (if (file-exists? pom)
             (cons pom (get-modules rest))
             (get-modules rest))))))

  (let* ((pom (get-pom pom-file))
         (modules (get-modules (pom-ref pom "modules"))))
    (cons pom-file
          (apply append (map pom-and-submodules modules)))))

(define* (pom-local-packages pom-file #:key (local-packages '()))
  "Given @var{pom-file}, a pom file name, return a list of local packages that
this repository contains."
  (let loop ((modules (pom-and-submodules pom-file))
             (local-packages local-packages))
    (match modules
      (() local-packages)
      ((module modules ...)
       (let* ((pom (get-pom module))
              (version (pom-version pom))
              (artifactid (pom-artifactid pom))
              (groupid (pom-groupid pom)))
         (loop modules
               (add-local-package local-packages groupid artifactid version)))))))

(define (group->dir group)
  "Convert a group ID to a directory path."
  (string-join (string-split group #\.) "/"))

(define* (fix-pom-dependencies pom-file inputs
                               #:key with-plugins? with-build-dependencies?
                                     with-modules? (excludes '())
                                     (local-packages '()))
  "Open @var{pom-file}, and override its content, rewriting its dependencies
to set their version to the latest version available in the @var{inputs}.

@var{#:with-plugins?} controls whether plugins are also overridden.
@var{#:with-build-dependencies?} controls whether build dependencies (whose
scope is not empty) are also overridden.  By default build dependencies and
plugins are not overridden.

@var{#:excludes} is an association list of groupID to a list of artifactIDs.
When a pair (groupID, artifactID) is present in the list, its entry is
removed instead of being overridden.  If the entry is ignored because of the
previous arguments, the entry is not removed.

@var{#:local-packages} is an association list that contains additional version
information for packages that are not in @var{inputs}.  If the package is
not found in @var{inputs}, information from this list is used instead to determine
the latest version of the package.  This is an association list of group IDs
to another association list of artifact IDs to a version number.

Returns nothing, but overrides the @var{pom-file} as a side-effect."
  (define pom (get-pom pom-file))

  (define (ls dir)
    (let ((dir (opendir dir)))
      (let loop ((res '()))
        (let ((entry (readdir dir)))
          (if (eof-object? entry)
              res
              (loop (cons entry res)))))))

  (define fix-pom
    (match-lambda
      ('() '())
      ((tag rest ...)
       (match tag
         (('http://maven.apache.org/POM/4.0.0:dependencies deps ...)
          `((http://maven.apache.org/POM/4.0.0:dependencies ,(fix-deps deps))
            ,@(fix-pom rest)))
         (('http://maven.apache.org/POM/4.0.0:dependencyManagement deps ...)
          `((http://maven.apache.org/POM/4.0.0:dependencyManagement
              ,(fix-dep-management deps))
            ,@(fix-pom rest)))
         (('http://maven.apache.org/POM/4.0.0:build build ...)
          (if with-plugins?
              `((http://maven.apache.org/POM/4.0.0:build ,(fix-build build))
                ,@(fix-pom rest))
              (cons tag (fix-pom rest))))
         (('http://maven.apache.org/POM/4.0.0:modules modules ...)
          (if with-modules?
              `((http://maven.apache.org/POM/4.0.0:modules ,(fix-modules modules))
                ,@(fix-pom rest))
              (cons tag (fix-pom rest))))
         (tag (cons tag (fix-pom rest)))))))

  (define fix-modules
    (match-lambda
      ('() '())
      ((tag rest ...)
       (match tag
        (('http://maven.apache.org/POM/4.0.0:module module)
         (if (file-exists? (string-append (dirname pom-file) "/" module "/pom.xml"))
             `((http://maven.apache.org/POM/4.0.0:module ,module) ,@(fix-modules rest))
             (fix-modules rest)))
        (tag (cons tag (fix-modules rest)))))))

  (define fix-dep-management
    (match-lambda
      ('() '())
      ((tag rest ...)
       (match tag
         (('http://maven.apache.org/POM/4.0.0:dependencies deps ...)
          `((http://maven.apache.org/POM/4.0.0:dependencies ,(fix-deps deps #t))
            ,@(fix-dep-management rest)))
         (tag (cons tag (fix-dep-management rest)))))))

  (define* (fix-deps deps #:optional optional?)
    (match deps
      ('() '())
      ((tag rest ...)
       (match tag
         (('http://maven.apache.org/POM/4.0.0:dependency dep ...)
          `((http://maven.apache.org/POM/4.0.0:dependency ,(fix-dep dep optional?))
            ,@(fix-deps rest optional?)))
         (tag (cons tag (fix-deps rest optional?)))))))

  (define fix-build
    (match-lambda
      ('() '())
      ((tag rest ...)
       (match tag
         (('http://maven.apache.org/POM/4.0.0:pluginManagement management ...)
          `((http://maven.apache.org/POM/4.0.0:pluginManagement
              ,(fix-management management))
            ,@(fix-build rest)))
         (('http://maven.apache.org/POM/4.0.0:plugins plugins ...)
          `((http://maven.apache.org/POM/4.0.0:plugins
              ,(fix-plugins plugins))
            ,@(fix-build rest)))
         (('http://maven.apache.org/POM/4.0.0:extensions extensions ...)
          `((http://maven.apache.org/POM/4.0.0:extensions
              ,(fix-extensions extensions))
            ,@(fix-build rest)))
         (tag (cons tag (fix-build rest)))))))

  (define* (fix-extensions extensions #:optional optional?)
    (match extensions
      ('() '())
      ((tag rest ...)
       (match tag
         (('http://maven.apache.org/POM/4.0.0:extension extension ...)
          (let ((group (or (pom-groupid extension) "org.apache.maven.plugins"))
                (artifact (pom-artifactid extension)))
            (if (member artifact (or (assoc-ref excludes group) '()))
              (fix-extensions rest optional?)
              `((http://maven.apache.org/POM/4.0.0:extension
                  ,(fix-plugin extension optional?)); extensions are similar to plugins
                ,@(fix-extensions rest optional?)))))
         (tag (cons tag (fix-extensions rest optional?)))))))

  (define fix-management
    (match-lambda
      ('() '())
      ((tag rest ...)
       (match tag
         (('http://maven.apache.org/POM/4.0.0:plugins plugins ...)
          `((http://maven.apache.org/POM/4.0.0:plugins
              ,(fix-plugins plugins #t))
            ,@(fix-management rest)))
         (tag (cons tag (fix-management rest)))))))

  (define* (fix-plugins plugins #:optional optional?)
    (match plugins
      ('() '())
      ((tag rest ...)
       (match tag
         (('http://maven.apache.org/POM/4.0.0:plugin plugin ...)
          (let ((group (or (pom-groupid plugin) "org.apache.maven.plugins"))
                (artifact (pom-artifactid plugin)))
            (if (member artifact (or (assoc-ref excludes group) '()))
              (fix-plugins rest optional?)
              `((http://maven.apache.org/POM/4.0.0:plugin
                  ,(fix-plugin plugin optional?))
                ,@(fix-plugins rest optional?)))))
         (tag (cons tag (fix-plugins rest optional?)))))))

  (define* (fix-plugin plugin #:optional optional?)
    (let* ((artifact (pom-artifactid plugin))
           (group (or (pom-groupid plugin) "org.apache.maven.plugins"))
           (version (or (assoc-ref (assoc-ref local-packages group) artifact)
                        (find-version inputs group artifact optional?)
                        (pom-version plugin))))
      (if (pom-version plugin)
        (map
          (lambda (tag)
            (match tag
              (('http://maven.apache.org/POM/4.0.0:version _)
               `(http://maven.apache.org/POM/4.0.0:version ,version))
              (('version _)
               `(http://maven.apache.org/POM/4.0.0:version ,version))
              (tag tag)))
          plugin)
        (cons `(http://maven.apache.org/POM/4.0.0:version ,version) plugin))))

  (define* (fix-dep dep #:optional optional?)
    (let* ((artifact (pom-artifactid dep))
           (group (or (pom-groupid dep) (pom-groupid pom)))
           (scope (pom-ref dep "scope"))
           (is-optional? (equal? (pom-ref dep "optional") '("true"))))
      (format (current-error-port) "maven: ~a:~a :: ~a (optional: ~a)~%"
              group artifact scope optional?)
      (if (or (and (not (equal? scope '("test"))) (not is-optional?))
              with-build-dependencies?)
          (let ((version (or (assoc-ref (assoc-ref local-packages group) artifact)
                             (find-version inputs group artifact optional?)
                             (pom-version dep))))
            (if (pom-version dep)
              (map
                (lambda (tag)
                  (match tag
                    (('http://maven.apache.org/POM/4.0.0:version _)
                     `(http://maven.apache.org/POM/4.0.0:version ,version))
                    (('version _)
                     `(http://maven.apache.org/POM/4.0.0:version ,version))
                    (_ tag)))
                dep)
              (cons `(http://maven.apache.org/POM/4.0.0:version ,version) dep)))
          dep)))

  (define (find-packaged-version inputs group artifact)
    (let* ((directory (string-append "lib/m2/" (group->dir group)
                                     "/" artifact))
           (java-inputs (filter
                          (lambda (input)
                            (file-exists? (string-append input "/" directory)))
                          inputs))
           (java-inputs (map (lambda (input) (string-append input "/" directory))
                             java-inputs))
           (versions (append-map ls java-inputs))
           (versions (sort versions version>?)))
      (if (null? versions)
          #f
          (car versions))))

  (define* (find-version inputs group artifact #:optional optional?)
    (let ((packaged-version (find-packaged-version inputs group artifact))
          (local-version (assoc-ref (assoc-ref local-packages group) artifact)))
      (or local-version packaged-version
          (if optional?
            #f
            (begin
              (format (current-error-port) "maven: ~a:~a is missing from inputs~%"
                      group artifact)
              (throw 'no-such-input group artifact))))))

  (let ((tmpfile (string-append pom-file ".tmp")))
    (with-output-to-file tmpfile
      (lambda _
        (sxml->xml (fix-maven-xml (fix-pom pom)))))
    (rename-file tmpfile pom-file)))
