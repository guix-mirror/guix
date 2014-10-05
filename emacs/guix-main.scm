;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
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

;;; Commentary:

;; Information about packages and generations is passed to the elisp
;; side in the form of alists of parameters (such as ‘name’ or
;; ‘version’) and their values.

;; ‘entries’ procedure is the “entry point” for the elisp side to get
;; information about packages and generations.

;; Since name/version pair is not necessarily unique, we use
;; `object-address' to identify a package (for ‘id’ parameter), if
;; possible.  However for the obsolete packages (that can be found in
;; installed manifest but not in a package directory), ‘id’ parameter is
;; still "name-version" string.  So ‘id’ package parameter in the code
;; below is either an object-address number or a full-name string.
;;
;; Important: as object addresses live only during guile session, elisp
;; part should take care about updating information after "Guix REPL" is
;; restarted (TODO!)

;; To speed-up the process of getting information, the following
;; auxiliary variables are used:
;;
;; - `%packages' - VHash of "package address"/"package" pairs.
;;
;; - `%package-table' - Hash table of
;;   "name+version key"/"list of packages" pairs.

;;; Code:

(use-modules
 (ice-9 vlist)
 (ice-9 match)
 (srfi srfi-1)
 (srfi srfi-11)
 (srfi srfi-19)
 (srfi srfi-26)
 (guix)
 (guix packages)
 (guix profiles)
 (guix licenses)
 (guix utils)
 (guix ui)
 (guix scripts package)
 (gnu packages))

(define-syntax-rule (first-or-false lst)
  (and (not (null? lst))
       (first lst)))

(define (list-maybe obj)
  (if (list? obj) obj (list obj)))

(define full-name->name+version package-name->name+version)
(define (name+version->full-name name version)
  (string-append name "-" version))

(define* (make-package-specification name #:optional version output)
  (let ((full-name (if version
                       (name+version->full-name name version)
                       name)))
    (if output
        (string-append full-name ":" output)
        full-name)))

(define name+version->key cons)
(define key->name+version car+cdr)

(define %packages
  (fold-packages (lambda (pkg res)
                   (vhash-consq (object-address pkg) pkg res))
                 vlist-null))

(define %package-table
  (let ((table (make-hash-table (vlist-length %packages))))
    (vlist-for-each
     (lambda (elem)
       (match elem
         ((address . pkg)
          (let* ((key (name+version->key (package-name pkg)
                                         (package-version pkg)))
                 (ref (hash-ref table key)))
            (hash-set! table key
                       (if ref (cons pkg ref) (list pkg)))))))
     %packages)
    table))

(define (manifest-entry->name+version+output entry)
  (values
   (manifest-entry-name    entry)
   (manifest-entry-version entry)
   (manifest-entry-output  entry)))

(define (manifest-entries->hash-table entries)
  "Return a hash table of name keys and lists of matching manifest ENTRIES."
  (let ((table (make-hash-table (length entries))))
    (for-each (lambda (entry)
                (let* ((key (manifest-entry-name entry))
                       (ref (hash-ref table key)))
                  (hash-set! table key
                             (if ref (cons entry ref) (list entry)))))
              entries)
    table))

(define (manifest=? m1 m2)
  (or (eq? m1 m2)
      (equal? m1 m2)))

(define manifest->hash-table
  (let ((current-manifest #f)
        (current-table #f))
    (lambda (manifest)
      "Return a hash table of name keys and matching MANIFEST entries."
      (unless (manifest=? manifest current-manifest)
        (set! current-manifest manifest)
        (set! current-table (manifest-entries->hash-table
                             (manifest-entries manifest))))
      current-table)))

(define* (manifest-entries-by-name manifest name #:optional version output)
  "Return a list of MANIFEST entries matching NAME, VERSION and OUTPUT."
  (let ((entries (or (hash-ref (manifest->hash-table manifest) name)
                     '())))
    (if (or version output)
        (filter (lambda (entry)
                  (and (or (not version)
                           (equal? version (manifest-entry-version entry)))
                       (or (not output)
                           (equal? output  (manifest-entry-output entry)))))
                entries)
        entries)))

(define (manifest-entry-by-output entries output)
  "Return a manifest entry from ENTRIES matching OUTPUT."
  (find (lambda (entry)
          (string= output (manifest-entry-output entry)))
        entries))

(define (fold-manifest-by-name manifest proc init)
  "Fold over MANIFEST entries.
Call (PROC NAME VERSION ENTRIES RESULT), using INIT as the initial value
of RESULT.  ENTRIES is a list of manifest entries with NAME/VERSION."
  (hash-fold (lambda (name entries res)
               (proc name (manifest-entry-version (car entries))
                     entries res))
             init
             (manifest->hash-table manifest)))

(define* (object-transformer param-alist #:optional (params '()))
  "Return procedure transforming objects into alist of parameter/value pairs.

PARAM-ALIST is alist of available parameters (symbols) and procedures
returning values of these parameters.  Each procedure is applied to
objects.

PARAMS is list of parameters from PARAM-ALIST that should be returned by
a resulting procedure.  If PARAMS is not specified or is an empty list,
use all available parameters.

Example:

  (let* ((alist `((plus1 . ,1+) (minus1 . ,1-) (mul2 . ,(cut * 2 <>))))
         (number->alist (object-transformer alist '(plus1 mul2))))
    (number->alist 8))
  =>
  ((plus1 . 9) (mul2 . 16))
"
  (let* ((use-all-params (null? params))
         (alist (filter-map (match-lambda
                             ((param . proc)
                              (and (or use-all-params
                                       (memq param params))
                                   (cons param proc)))
                             (_ #f))
                            param-alist)))
    (lambda objects
      (map (match-lambda
            ((param . proc)
             (cons param (apply proc objects))))
           alist))))

(define %manifest-entry-param-alist
  `((output       . ,manifest-entry-output)
    (path         . ,manifest-entry-item)
    (dependencies . ,manifest-entry-dependencies)))

(define manifest-entry->sexp
  (object-transformer %manifest-entry-param-alist))

(define (manifest-entries->sexps entries)
  (map manifest-entry->sexp entries))

(define (package-inputs-names inputs)
  "Return a list of full names of the packages from package INPUTS."
  (filter-map (match-lambda
               ((_ (? package? package))
                (package-full-name package))
               (_ #f))
              inputs))

(define (package-license-names package)
  "Return a list of license names of the PACKAGE."
  (filter-map (lambda (license)
                (and (license? license)
                     (license-name license)))
              (list-maybe (package-license package))))

(define (package-unique? package)
  "Return #t if PACKAGE is a single package with such name/version."
  (null? (cdr (packages-by-name (package-name package)
                                (package-version package)))))

(define %package-param-alist
  `((id                . ,object-address)
    (package-id        . ,object-address)
    (name              . ,package-name)
    (version           . ,package-version)
    (license           . ,package-license-names)
    (synopsis          . ,package-synopsis)
    (description       . ,package-description)
    (home-url          . ,package-home-page)
    (outputs           . ,package-outputs)
    (non-unique        . ,(negate package-unique?))
    (inputs            . ,(lambda (pkg)
                            (package-inputs-names
                             (package-inputs pkg))))
    (native-inputs     . ,(lambda (pkg)
                            (package-inputs-names
                             (package-native-inputs pkg))))
    (propagated-inputs . ,(lambda (pkg)
                            (package-inputs-names
                             (package-propagated-inputs pkg))))
    (location          . ,(lambda (pkg)
                            (location->string (package-location pkg))))))

(define (package-param package param)
  "Return a value of a PACKAGE PARAM."
  (and=> (assq-ref %package-param-alist param)
         (cut <> package)))


;;; Finding packages.

(define (package-by-address address)
  (and=> (vhash-assq address %packages)
         cdr))

(define (packages-by-name+version name version)
  (or (hash-ref %package-table
                (name+version->key name version))
      '()))

(define (packages-by-full-name full-name)
  (call-with-values
      (lambda () (full-name->name+version full-name))
    packages-by-name+version))

(define (packages-by-id id)
  (if (integer? id)
      (let ((pkg (package-by-address id)))
        (if pkg (list pkg) '()))
      (packages-by-full-name id)))

(define (id->name+version id)
  (if (integer? id)
      (and=> (package-by-address id)
             (lambda (pkg)
               (values (package-name pkg)
                       (package-version pkg))))
      (full-name->name+version id)))

(define (package-by-id id)
  (first-or-false (packages-by-id id)))

(define (newest-package-by-id id)
  (and=> (id->name+version id)
         (lambda (name)
           (first-or-false (find-best-packages-by-name name #f)))))

(define (matching-packages predicate)
  (fold-packages (lambda (pkg res)
                   (if (predicate pkg)
                       (cons pkg res)
                       res))
                 '()))

(define (filter-packages-by-output packages output)
  (filter (lambda (package)
            (member output (package-outputs package)))
          packages))

(define* (packages-by-name name #:optional version output)
  "Return a list of packages matching NAME, VERSION and OUTPUT."
  (let ((packages (if version
                      (packages-by-name+version name version)
                      (matching-packages
                       (lambda (pkg) (string=? name (package-name pkg)))))))
    (if output
        (filter-packages-by-output packages output)
        packages)))

(define (manifest-entry->packages entry)
  (call-with-values
      (lambda () (manifest-entry->name+version+output entry))
    packages-by-name))

(define (packages-by-regexp regexp match-params)
  "Return a list of packages matching REGEXP string.
MATCH-PARAMS is a list of parameters that REGEXP can match."
  (define (package-match? package regexp)
    (any (lambda (param)
           (let ((val (package-param package param)))
             (and (string? val) (regexp-exec regexp val))))
         match-params))

  (let ((re (make-regexp regexp regexp/icase)))
    (matching-packages (cut package-match? <> re))))

(define (all-available-packages)
  "Return a list of all available packages."
  (matching-packages (const #t)))

(define (newest-available-packages)
  "Return a list of the newest available packages."
  (vhash-fold (lambda (name elem res)
                (match elem
                  ((_ newest pkgs ...)
                   (cons newest res))))
              '()
              (find-newest-available-packages)))


;;; Making package/output patterns.

(define (specification->package-pattern specification)
  (call-with-values
      (lambda ()
        (full-name->name+version specification))
    list))

(define (specification->output-pattern specification)
  (call-with-values
      (lambda ()
        (package-specification->name+version+output specification #f))
    list))

(define (id->package-pattern id)
  (if (integer? id)
      (package-by-address id)
      (specification->package-pattern id)))

(define (id->output-pattern id)
  "Return an output pattern by output ID.
ID should be '<package-address>:<output>' or '<name>-<version>:<output>'."
  (let-values (((name version output)
                (package-specification->name+version+output id)))
    (if version
        (list name version output)
        (list (package-by-address (string->number name))
              output))))

(define (specifications->package-patterns . specifications)
  (map specification->package-pattern specifications))

(define (specifications->output-patterns . specifications)
  (map specification->output-pattern specifications))

(define (ids->package-patterns . ids)
  (map id->package-pattern ids))

(define (ids->output-patterns . ids)
  (map id->output-pattern ids))

(define* (manifest-patterns-result packages res obsolete-pattern
                                   #:optional installed-pattern)
  "Auxiliary procedure for 'manifest-package-patterns' and
'manifest-output-patterns'."
  (if (null? packages)
      (cons (obsolete-pattern) res)
      (if installed-pattern
          ;; We don't need duplicates for a list of installed packages,
          ;; so just take any (car) package.
          (cons (installed-pattern (car packages)) res)
          res)))

(define* (manifest-package-patterns manifest #:optional obsolete-only?)
  "Return a list of package patterns for MANIFEST entries.
If OBSOLETE-ONLY? is #f, use all entries, otherwise make patterns only
for obsolete packages."
  (fold-manifest-by-name
   manifest
   (lambda (name version entries res)
     (manifest-patterns-result (packages-by-name name version)
                               res
                               (lambda () (list name version entries))
                               (and (not obsolete-only?)
                                    (cut list <> entries))))
   '()))

(define* (manifest-output-patterns manifest #:optional obsolete-only?)
  "Return a list of output patterns for MANIFEST entries.
If OBSOLETE-ONLY? is #f, use all entries, otherwise make patterns only
for obsolete packages."
  (fold (lambda (entry res)
          (manifest-patterns-result (manifest-entry->packages entry)
                                    res
                                    (lambda () entry)
                                    (and (not obsolete-only?)
                                         (cut list <> entry))))
        '()
        (manifest-entries manifest)))

(define (obsolete-package-patterns manifest)
  (manifest-package-patterns manifest #t))

(define (obsolete-output-patterns manifest)
  (manifest-output-patterns manifest #t))


;;; Transforming package/output patterns into alists.

(define (obsolete-package-sexp name version entries)
  "Return an alist with information about obsolete package.
ENTRIES is a list of installed manifest entries."
  `((id        . ,(name+version->full-name name version))
    (name      . ,name)
    (version   . ,version)
    (outputs   . ,(map manifest-entry-output entries))
    (obsolete  . #t)
    (installed . ,(manifest-entries->sexps entries))))

(define (package-pattern-transformer manifest params)
  "Return 'package-pattern->package-sexps' procedure."
  (define package->sexp
    (object-transformer %package-param-alist params))

  (define* (sexp-by-package package #:optional
                            (entries (manifest-entries-by-name
                                      manifest
                                      (package-name package)
                                      (package-version package))))
    (cons (cons 'installed (manifest-entries->sexps entries))
          (package->sexp package)))

  (define (->sexps pattern)
    (match pattern
      ((? package? package)
       (list (sexp-by-package package)))
      (((? package? package) entries)
       (list (sexp-by-package package entries)))
      ((name version entries)
       (list (obsolete-package-sexp
              name version entries)))
      ((name version)
       (let ((packages (packages-by-name name version)))
         (if (null? packages)
             (let ((entries (manifest-entries-by-name
                             manifest name version)))
               (if (null? entries)
                   '()
                   (list (obsolete-package-sexp
                          name version entries))))
             (map sexp-by-package packages))))
      (_ '())))

  ->sexps)

(define (output-pattern-transformer manifest params)
  "Return 'output-pattern->output-sexps' procedure."
  (define package->sexp
    (object-transformer (alist-delete 'id %package-param-alist)
                        params))

  (define manifest-entry->sexp
    (object-transformer (alist-delete 'output %manifest-entry-param-alist)
                        params))

  (define* (output-sexp pkg-alist pkg-address output
                        #:optional entry)
    (let ((entry-alist (if entry
                           (manifest-entry->sexp entry)
                           '()))
          (base `((id        . ,(string-append
                                 (number->string pkg-address)
                                 ":" output))
                  (output    . ,output)
                  (installed . ,(->bool entry)))))
      (append entry-alist base pkg-alist)))

  (define (obsolete-output-sexp entry)
    (let-values (((name version output)
                  (manifest-entry->name+version+output entry)))
      (let ((base `((id         . ,(make-package-specification
                                    name version output))
                    (package-id . ,(name+version->full-name name version))
                    (name       . ,name)
                    (version    . ,version)
                    (output     . ,output)
                    (obsolete   . #t)
                    (installed  . #t))))
        (append (manifest-entry->sexp entry) base))))

  (define* (sexps-by-package package #:optional output
                             (entries (manifest-entries-by-name
                                       manifest
                                       (package-name package)
                                       (package-version package))))
    ;; Assuming that PACKAGE has this OUTPUT.
    (let ((pkg-alist (package->sexp package))
          (address (object-address package))
          (outputs (if output
                       (list output)
                       (package-outputs package))))
      (map (lambda (output)
             (output-sexp pkg-alist address output
                          (manifest-entry-by-output entries output)))
           outputs)))

  (define* (sexps-by-manifest-entry entry #:optional
                                    (packages (manifest-entry->packages
                                               entry)))
    (if (null? packages)
        (list (obsolete-output-sexp entry))
        (map (lambda (package)
               (output-sexp (package->sexp package)
                            (object-address package)
                            (manifest-entry-output entry)
                            entry))
             packages)))

  (define (->sexps pattern)
    (match pattern
      ((? package? package)
       (sexps-by-package package))
      ((package (? string? output))
       (sexps-by-package package output))
      ((? manifest-entry? entry)
       (list (obsolete-output-sexp entry)))
      ((package entry)
       (sexps-by-manifest-entry entry (list package)))
      ((name version output)
       (let ((packages (packages-by-name name version output)))
         (if (null? packages)
             (let ((entries (manifest-entries-by-name
                             manifest name version output)))
               (append-map (cut sexps-by-manifest-entry <>)
                           entries))
             (append-map (cut sexps-by-package <> output)
                         packages))))
      (_ '())))

  ->sexps)

(define (entry-type-error entry-type)
  (error (format #f "Wrong entry-type '~a'" entry-type)))

(define (search-type-error entry-type search-type)
  (error (format #f "Wrong search type '~a' for entry-type '~a'"
                 search-type entry-type)))

(define %pattern-transformers
  `((package . ,package-pattern-transformer)
    (output  . ,output-pattern-transformer)))

(define (pattern-transformer entry-type)
  (assq-ref %pattern-transformers entry-type))

;; All procedures from inner alists are called with (MANIFEST . SEARCH-VALS)
;; as arguments; see `package/output-sexps'.
(define %patterns-makers
  (let* ((apply-to-rest         (lambda (proc)
                                  (lambda (_ . rest) (apply proc rest))))
         (apply-to-first        (lambda (proc)
                                  (lambda (first . _) (proc first))))
         (manifest-package-proc (apply-to-first manifest-package-patterns))
         (manifest-output-proc  (apply-to-first manifest-output-patterns))
         (regexp-proc           (lambda (_ regexp params . __)
                                  (packages-by-regexp regexp params)))
         (all-proc              (lambda _ (all-available-packages)))
         (newest-proc           (lambda _ (newest-available-packages))))
    `((package
       (id               . ,(apply-to-rest ids->package-patterns))
       (name             . ,(apply-to-rest specifications->package-patterns))
       (installed        . ,manifest-package-proc)
       (generation       . ,manifest-package-proc)
       (obsolete         . ,(apply-to-first obsolete-package-patterns))
       (regexp           . ,regexp-proc)
       (all-available    . ,all-proc)
       (newest-available . ,newest-proc))
      (output
       (id               . ,(apply-to-rest ids->output-patterns))
       (name             . ,(apply-to-rest specifications->output-patterns))
       (installed        . ,manifest-output-proc)
       (generation       . ,manifest-output-proc)
       (obsolete         . ,(apply-to-first obsolete-output-patterns))
       (regexp           . ,regexp-proc)
       (all-available    . ,all-proc)
       (newest-available . ,newest-proc)))))

(define (patterns-maker entry-type search-type)
  (or (and=> (assq-ref %patterns-makers entry-type)
             (cut assq-ref <> search-type))
      (search-type-error entry-type search-type)))

(define (package/output-sexps profile params entry-type
                              search-type search-vals)
  "Return information about packages or package outputs.
See 'entry-sexps' for details."
  (let* ((profile (if (eq? search-type 'generation)
                      (generation-file-name profile (car search-vals))
                      profile))
         (manifest (profile-manifest profile))
         (patterns (apply (patterns-maker entry-type search-type)
                          manifest search-vals))
         (->sexps ((pattern-transformer entry-type) manifest params)))
    (append-map ->sexps patterns)))


;;; Getting information about generations.

(define (generation-param-alist profile)
  "Return an alist of generation parameters and procedures for PROFILE."
  (list
   (cons 'id          identity)
   (cons 'number      identity)
   (cons 'prev-number (cut previous-generation-number profile <>))
   (cons 'path        (cut generation-file-name profile <>))
   (cons 'time        (lambda (gen)
                        (time-second (generation-time profile gen))))))

(define (matching-generations profile predicate)
  "Return a list of PROFILE generations matching PREDICATE."
  (filter predicate (profile-generations profile)))

(define (last-generations profile number)
  "Return a list of last NUMBER generations.
If NUMBER is 0 or less, return all generations."
  (let ((generations (profile-generations profile))
        (number (if (<= number 0) +inf.0 number)))
    (if (> (length generations) number)
        (list-head  (reverse generations) number)
        generations)))

(define (find-generations profile search-type search-vals)
  "Find PROFILE's generations matching SEARCH-TYPE and SEARCH-VALS."
  (case search-type
    ((id)
     (matching-generations profile (cut memq <> search-vals)))
    ((last)
     (last-generations profile (car search-vals)))
    ((all)
     (last-generations profile +inf.0))
    (else (search-type-error "generation" search-type))))

(define (generation-sexps profile params search-type search-vals)
  "Return information about generations.
See 'entry-sexps' for details."
  (let ((generations (find-generations profile search-type search-vals))
        (->sexp (object-transformer (generation-param-alist profile)
                                    params)))
    (map ->sexp generations)))


;;; Getting package/output/generation entries (alists).

(define (entries profile params entry-type search-type search-vals)
  "Return information about entries.

ENTRY-TYPE is a symbol defining a type of returning information.  Should
be: 'package', 'output' or 'generation'.

SEARCH-TYPE and SEARCH-VALS define how to get the information.
SEARCH-TYPE should be one of the following symbols:

- If ENTRY-TYPE is 'package' or 'output':
  'id', 'name', 'regexp', 'all-available', 'newest-available',
  'installed', 'obsolete', 'generation'.

- If ENTRY-TYPE is 'generation':
  'id', 'last', 'all'.

PARAMS is a list of parameters for receiving.  If it is an empty list,
get information with all available parameters, which are:

- If ENTRY-TYPE is 'package':
  'id', 'name', 'version', 'outputs', 'license', 'synopsis',
  'description', 'home-url', 'inputs', 'native-inputs',
  'propagated-inputs', 'location', 'installed'.

- If ENTRY-TYPE is 'output':
  'id', 'package-id', 'name', 'version', 'output', 'license',
  'synopsis', 'description', 'home-url', 'inputs', 'native-inputs',
  'propagated-inputs', 'location', 'installed', 'path', 'dependencies'.

- If ENTRY-TYPE is 'generation':
  'id', 'number', 'prev-number', 'path', 'time'.

Returning value is a list of alists.  Each alist consists of
parameter/value pairs."
  (case entry-type
    ((package output)
     (package/output-sexps profile params entry-type
                           search-type search-vals))
    ((generation)
     (generation-sexps profile params
                       search-type search-vals))
    (else (entry-type-error entry-type))))


;;; Package actions.

(define* (package->manifest-entry* package #:optional output)
  (and package
       (begin
         (check-package-freshness package)
         (package->manifest-entry package output))))

(define* (make-install-manifest-entries id #:optional output)
  (package->manifest-entry* (package-by-id id) output))

(define* (make-upgrade-manifest-entries id #:optional output)
  (package->manifest-entry* (newest-package-by-id id) output))

(define* (make-manifest-pattern id #:optional output)
  "Make manifest pattern from a package ID and OUTPUT."
  (let-values (((name version)
                (id->name+version id)))
    (and name version
         (manifest-pattern
          (name name)
          (version version)
          (output output)))))

(define (convert-action-pattern pattern proc)
  "Convert action PATTERN into a list of objects returned by PROC.
PROC is called: (PROC ID) or (PROC ID OUTPUT)."
  (match pattern
    ((id . outputs)
     (if (null? outputs)
         (let ((obj (proc id)))
           (if obj (list obj) '()))
         (filter-map (cut proc id <>)
                     outputs)))
    (_ '())))

(define (convert-action-patterns patterns proc)
  (append-map (cut convert-action-pattern <> proc)
              patterns))

(define* (process-package-actions
          profile #:key (install '()) (upgrade '()) (remove '())
          (use-substitutes? #t) dry-run?)
  "Perform package actions.

INSTALL, UPGRADE, REMOVE are lists of 'package action patterns'.
Each pattern should have the following form:

  (ID . OUTPUTS)

ID is an object address or a full-name of a package.
OUTPUTS is a list of package outputs (may be an empty list)."
  (format #t "The process begins ...~%")
  (let* ((install (append
                   (convert-action-patterns
                    install make-install-manifest-entries)
                   (convert-action-patterns
                    upgrade make-upgrade-manifest-entries)))
         (remove (convert-action-patterns remove make-manifest-pattern))
         (transaction (manifest-transaction (install install)
                                            (remove remove)))
         (manifest (profile-manifest profile))
         (new-manifest (manifest-perform-transaction
                        manifest transaction)))
    (unless (and (null? install) (null? remove))
      (let* ((store (open-connection))
             (derivation (run-with-store
                          store (profile-derivation new-manifest)))
             (derivations (list derivation))
             (new-profile (derivation->output-path derivation)))
        (set-build-options store
                           #:use-substitutes? use-substitutes?)
        (manifest-show-transaction store manifest transaction
                                   #:dry-run? dry-run?)
        (show-what-to-build store derivations
                            #:use-substitutes? use-substitutes?
                            #:dry-run? dry-run?)
        (unless dry-run?
          (let ((name (generation-file-name
                       profile
                       (+ 1 (generation-number profile)))))
            (and (build-derivations store derivations)
                 (let* ((entries (manifest-entries new-manifest))
                        (count   (length entries)))
                   (switch-symlinks name new-profile)
                   (switch-symlinks profile name)
                   (format #t (N_ "~a package in profile~%"
                                  "~a packages in profile~%"
                                  count)
                           count)))))))))

(define (delete-generations* profile generations)
  "Delete GENERATIONS from PROFILE.
GENERATIONS is a list of generation numbers."
  (with-store store
    (delete-generations store profile generations)))
