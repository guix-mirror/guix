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
;; ‘version’) and their values.  These alists are called "entries" in
;; this code.  So to distinguish, just "package" in the name of a
;; function means a guile object ("package" record) while
;; "package entry" means alist of package parameters and values (see
;; ‘package-param-alist’).
;;
;; "Entry" is probably not the best name for such alists, because there
;; already exists "manifest-entry" which has nothing to do with the
;; "entry" described above.  Do not be confused :)

;; ‘get-entries’ function is the “entry point” for the elisp side to get
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
;;
;; ‘installed’ parameter of a package entry contains information about
;; installed outputs.  It is a list of "installed entries" (see
;; ‘package-installed-param-alist’).

;; To speed-up the process of getting information, the following
;; auxiliary variables are used:
;;
;; - `%packages' - VHash of "package address"/"package" pairs.
;;
;; - `%package-table' - Hash table of
;;   "name+version key"/"list of packages" pairs.
;;
;; - `%current-manifest-entries-table' - Hash table of
;;   "name+version key"/"list of manifest entries" pairs.  This variable
;;   is set by `set-current-manifest-maybe!' when it is needed.

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

(define %current-manifest #f)
(define %current-manifest-entries-table #f)

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

;; FIXME get rid of this function!
(define (set-current-manifest-maybe! profile)
  (define (manifest-entries->hash-table entries)
    (let ((entries-table (make-hash-table (length entries))))
      (for-each (lambda (entry)
                  (let* ((key (name+version->key
                               (manifest-entry-name entry)
                               (manifest-entry-version entry)))
                         (ref (hash-ref entries-table key)))
                    (hash-set! entries-table key
                               (if ref (cons entry ref) (list entry)))))
                entries)
      entries-table))

  (when profile
    (let ((manifest (profile-manifest profile)))
      (unless (and (manifest? %current-manifest)
                   (equal? manifest %current-manifest))
        (set! %current-manifest manifest)
        (set! %current-manifest-entries-table
              (manifest-entries->hash-table
               (manifest-entries manifest)))))))

(define (manifest-entries-by-name+version name version)
  (or (hash-ref %current-manifest-entries-table
                (name+version->key name version))
      '()))

(define (packages-by-name+version name version)
  (or (hash-ref %package-table
                (name+version->key name version))
      '()))

(define (packages-by-full-name full-name)
  (call-with-values
      (lambda () (full-name->name+version full-name))
    packages-by-name+version))

(define (package-by-address address)
  (and=> (vhash-assq address %packages)
         cdr))

(define (packages-by-id id)
  (if (integer? id)
      (let ((pkg (package-by-address id)))
        (if pkg (list pkg) '()))
      (packages-by-full-name id)))

(define (package-by-id id)
  (first-or-false (packages-by-id id)))

(define (newest-package-by-id id)
  (and=> (id->name+version id)
         (lambda (name)
           (first-or-false (find-best-packages-by-name name #f)))))

(define (id->name+version id)
  (if (integer? id)
      (and=> (package-by-address id)
             (lambda (pkg)
               (values (package-name pkg)
                       (package-version pkg))))
      (full-name->name+version id)))

(define (fold-manifest-entries proc init)
  "Fold over `%current-manifest-entries-table'.
Call (PROC NAME VERSION ENTRIES RESULT) for each element of the hash
table, using INIT as the initial value of RESULT."
  (hash-fold (lambda (key entries res)
               (let-values (((name version) (key->name+version key)))
                 (proc name version entries res)))
             init
             %current-manifest-entries-table))

(define (fold-object proc init obj)
  (fold proc init
        (if (list? obj) obj (list obj))))

(define* (object-transformer param-alist #:optional (params '()))
  "Return function for transforming an object into alist of parameters/values.

PARAM-ALIST is alist of available object parameters (symbols) and functions
returning values of these parameters.  Each function is called with object as
a single argument.

PARAMS is list of parameters from PARAM-ALIST that should be returned by a
resulting function.  If PARAMS is not specified or is an empty list, use all
available parameters.

Example:

  (let ((alist `((plus1 . ,1+) (minus1 . ,1-) (mul2 . ,(cut * 2 <>))))
        (number->alist (object-transformer alist '(plus1 mul2))))
    (number->alist 8))
  =>
  ((plus1 . 9) (mul2 . 16))
"
  (let ((alist (let ((use-all-params (null? params)))
                 (filter-map (match-lambda
                              ((param . fun)
                               (and (or use-all-params
                                        (memq param params))
                                    (cons param fun)))
                              (_ #f))
                             param-alist))))
    (lambda (object)
      (map (match-lambda
            ((param . fun)
             (cons param (fun object))))
           alist))))

(define package-installed-param-alist
  (list
   (cons 'output       manifest-entry-output)
   (cons 'path         manifest-entry-item)
   (cons 'dependencies manifest-entry-dependencies)))

(define manifest-entry->installed-entry
  (object-transformer package-installed-param-alist))

(define (manifest-entries->installed-entries entries)
  (map manifest-entry->installed-entry entries))

(define (installed-entries-by-name+version name version)
  (manifest-entries->installed-entries
   (manifest-entries-by-name+version name version)))

(define (installed-entries-by-package package)
  (installed-entries-by-name+version (package-name package)
                                     (package-version package)))

(define (package-inputs-names inputs)
  "Return list of full names of the packages from package INPUTS."
  (filter-map (match-lambda
               ((_ (? package? package))
                (package-full-name package))
               (_ #f))
              inputs))

(define (package-license-names package)
  "Return list of license names of the PACKAGE."
  (fold-object (lambda (license res)
                 (if (license? license)
                     (cons (license-name license) res)
                     res))
               '()
               (package-license package)))

(define (package-unique? package)
  "Return #t if PACKAGE is a single package with such name/version."
  (null? (cdr (packages-by-name+version (package-name package)
                                        (package-version package)))))

(define package-param-alist
  (list
   (cons 'id                object-address)
   (cons 'name              package-name)
   (cons 'version           package-version)
   (cons 'license           package-license-names)
   (cons 'synopsis          package-synopsis)
   (cons 'description       package-description)
   (cons 'home-url          package-home-page)
   (cons 'outputs           package-outputs)
   (cons 'non-unique        (negate package-unique?))
   (cons 'inputs            (lambda (pkg) (package-inputs-names
                                      (package-inputs pkg))))
   (cons 'native-inputs     (lambda (pkg) (package-inputs-names
                                      (package-native-inputs pkg))))
   (cons 'propagated-inputs (lambda (pkg) (package-inputs-names
                                      (package-propagated-inputs pkg))))
   (cons 'location          (lambda (pkg) (location->string
                                      (package-location pkg))))
   (cons 'installed         installed-entries-by-package)))

(define (package-param package param)
  "Return the value of a PACKAGE PARAM."
  (define (accessor param)
    (and=> (assq param package-param-alist)
           cdr))
  (and=> (accessor param)
         (cut <> package)))

(define (matching-package-entries ->entry predicate)
  "Return list of package entries for the matching packages.
PREDICATE is called on each package."
  (fold-packages (lambda (pkg res)
                   (if (predicate pkg)
                       (cons (->entry pkg) res)
                       res))
                 '()))

(define (make-obsolete-package-entry name version entries)
  "Return package entry for an obsolete package with NAME and VERSION.
ENTRIES is a list of manifest entries used to get installed info."
  `((id        . ,(name+version->full-name name version))
    (name      . ,name)
    (version   . ,version)
    (outputs   . ,(map manifest-entry-output entries))
    (obsolete  . #t)
    (installed . ,(manifest-entries->installed-entries entries))))

(define (package-entries-by-name+version ->entry name version)
  "Return list of package entries for packages with NAME and VERSION."
  (let ((packages (packages-by-name+version name version)))
    (if (null? packages)
        (let ((entries (manifest-entries-by-name+version name version)))
          (if (null? entries)
              '()
              (list (make-obsolete-package-entry name version entries))))
        (map ->entry packages))))

(define (package-entries-by-spec profile ->entry spec)
  "Return list of package entries for packages with name specification SPEC."
  (set-current-manifest-maybe! profile)
  (let-values (((name version)
                (full-name->name+version spec)))
    (if version
        (package-entries-by-name+version ->entry name version)
        (matching-package-entries
         ->entry
         (lambda (pkg) (string=? name (package-name pkg)))))))

(define (package-entries-by-regexp profile ->entry regexp match-params)
  "Return list of package entries for packages matching REGEXP string.
MATCH-PARAMS is a list of parameters that REGEXP can match."
  (define (package-match? package regexp)
    (any (lambda (param)
           (let ((val (package-param package param)))
             (and (string? val) (regexp-exec regexp val))))
         match-params))

  (set-current-manifest-maybe! profile)
  (let ((re (make-regexp regexp regexp/icase)))
    (matching-package-entries ->entry (cut package-match? <> re))))

(define (package-entries-by-ids profile ->entry ids)
  "Return list of package entries for packages matching KEYS.
IDS may be an object-address, a full-name or a list of such elements."
  (set-current-manifest-maybe! profile)
  (fold-object
   (lambda (id res)
     (if (integer? id)
         (let ((pkg (package-by-address id)))
           (if pkg
               (cons (->entry pkg) res)
               res))
         (let ((entries (package-entries-by-spec #f ->entry id)))
           (if (null? entries)
               res
               (append res entries)))))
   '()
   ids))

(define (newest-available-package-entries profile ->entry)
  "Return list of package entries for the newest available packages."
  (set-current-manifest-maybe! profile)
  (vhash-fold (lambda (name elem res)
                (match elem
                  ((version newest pkgs ...)
                   (cons (->entry newest) res))))
              '()
              (find-newest-available-packages)))

(define (all-available-package-entries profile ->entry)
  "Return list of package entries for all available packages."
  (set-current-manifest-maybe! profile)
  (matching-package-entries ->entry (const #t)))

(define (manifest-package-entries ->entry)
  "Return list of package entries for the current manifest."
  (fold-manifest-entries
   (lambda (name version entries res)
     ;; We don't care about duplicates for the list of
     ;; installed packages, so just take any package (car)
     ;; matching name+version
     (cons (car (package-entries-by-name+version ->entry name version))
           res))
   '()))

(define (installed-package-entries profile ->entry)
  "Return list of package entries for all installed packages."
  (set-current-manifest-maybe! profile)
  (manifest-package-entries ->entry))

(define (generation-package-entries profile ->entry generation)
  "Return list of package entries for packages from GENERATION."
  (set-current-manifest-maybe!
   (generation-file-name profile generation))
  (manifest-package-entries ->entry))

(define (obsolete-package-entries profile _)
  "Return list of package entries for obsolete packages."
  (set-current-manifest-maybe! profile)
  (fold-manifest-entries
   (lambda (name version entries res)
     (let ((packages (packages-by-name+version name version)))
       (if (null? packages)
           (cons (make-obsolete-package-entry name version entries) res)
           res)))
   '()))


;;; Generation entries

(define (profile-generations profile)
  "Return list of generations for PROFILE."
  (let ((generations (generation-numbers profile)))
    (if (equal? generations '(0))
        '()
        generations)))

(define (generation-param-alist profile)
  "Return alist of generation parameters and functions for PROFILE."
  (list
   (cons 'id          identity)
   (cons 'number      identity)
   (cons 'prev-number (cut previous-generation-number profile <>))
   (cons 'path        (cut generation-file-name profile <>))
   (cons 'time        (lambda (gen)
                        (time-second (generation-time profile gen))))))

(define (matching-generation-entries profile ->entry predicate)
  "Return list of generation entries for the matching generations.
PREDICATE is called on each generation."
  (filter-map (lambda (gen)
                (and (predicate gen) (->entry gen)))
              (profile-generations profile)))

(define (last-generation-entries profile ->entry number)
  "Return list of last NUMBER generation entries.
If NUMBER is 0 or less, return all generation entries."
  (let ((generations (profile-generations profile))
        (number (if (<= number 0) +inf.0 number)))
    (map ->entry
         (if (> (length generations) number)
             (list-head  (reverse generations) number)
             generations))))

(define (all-generation-entries profile ->entry)
  "Return list of all generation entries."
  (last-generation-entries profile ->entry +inf.0))

(define (generation-entries-by-ids profile ->entry ids)
  "Return list of generation entries for generations matching IDS.
IDS is a list of generation numbers."
  (matching-generation-entries profile ->entry (cut memq <> ids)))


;;; Getting package/generation entries

(define %package-entries-functions
  (alist->vhash
   `((id               . ,package-entries-by-ids)
     (name             . ,package-entries-by-spec)
     (regexp           . ,package-entries-by-regexp)
     (all-available    . ,all-available-package-entries)
     (newest-available . ,newest-available-package-entries)
     (installed        . ,installed-package-entries)
     (obsolete         . ,obsolete-package-entries)
     (generation       . ,generation-package-entries))
   hashq))

(define %generation-entries-functions
  (alist->vhash
   `((id   . ,generation-entries-by-ids)
     (last . ,last-generation-entries)
     (all  . ,all-generation-entries))
   hashq))

(define (get-entries profile params entry-type search-type search-vals)
  "Return list of entries.
ENTRY-TYPE and SEARCH-TYPE define a search function that should be
applied to PARAMS and VALS."
  (let-values (((vhash ->entry)
                (case entry-type
                  ((package)
                   (values %package-entries-functions
                           (object-transformer
                            package-param-alist params)))
                  ((generation)
                   (values %generation-entries-functions
                           (object-transformer
                            (generation-param-alist profile) params)))
                  (else (format (current-error-port)
                                "Wrong entry type '~a'" entry-type)))))
    (match (vhash-assq search-type vhash)
      ((key . fun)
       (apply fun profile ->entry search-vals))
      (_ '()))))


;;; Actions

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

