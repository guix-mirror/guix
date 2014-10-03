;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:re-export (%current-system
               %current-target-system)
  #:export (origin
            origin?
            origin-uri
            origin-method
            origin-sha256
            origin-file-name
            origin-patches
            origin-patch-flags
            origin-patch-inputs
            origin-patch-guile
            origin-snippet
            origin-modules
            origin-imported-modules
            base32

            <search-path-specification>
            search-path-specification
            search-path-specification?
            search-path-specification->sexp

            package
            package?
            package-name
            package-version
            package-full-name
            package-source
            package-build-system
            package-arguments
            package-inputs
            package-native-inputs
            package-propagated-inputs
            package-outputs
            package-native-search-paths
            package-search-paths
            package-synopsis
            package-description
            package-license
            package-home-page
            package-platforms
            package-maintainers
            package-properties
            package-location
            package-field-location

            package-direct-inputs
            package-transitive-inputs
            package-transitive-target-inputs
            package-transitive-native-inputs
            package-transitive-propagated-inputs
            package-source-derivation
            package-derivation
            package-cross-derivation
            package-output

            &package-error
            package-error?
            package-error-package
            &package-input-error
            package-input-error?
            package-error-invalid-input
            &package-cross-build-system-error
            package-cross-build-system-error?

            package->bag
            bag-transitive-inputs
            bag-transitive-host-inputs
            bag-transitive-build-inputs
            bag-transitive-target-inputs))

;;; Commentary:
;;;
;;; This module provides a high-level mechanism to define packages in a
;;; Guix-based distribution.
;;;
;;; Code:

;; The source of a package, such as a tarball URL and fetcher---called
;; "origin" to avoid name clash with `package-source', `source', etc.
(define-record-type* <origin>
  origin make-origin
  origin?
  (uri       origin-uri)                          ; string
  (method    origin-method)                       ; procedure
  (sha256    origin-sha256)                       ; bytevector
  (file-name origin-file-name (default #f))       ; optional file name
  (patches   origin-patches (default '()))        ; list of file names
  (snippet   origin-snippet (default #f))         ; sexp or #f
  (patch-flags  origin-patch-flags                ; list of strings
                (default '("-p1")))

  ;; Patching requires Guile, GNU Patch, and a few more.  These two fields are
  ;; used to specify these dependencies when needed.
  (patch-inputs origin-patch-inputs               ; input list or #f
                (default #f))
  (modules      origin-modules                    ; list of module names
                (default '()))
  (imported-modules origin-imported-modules       ; list of module names
                    (default '()))
  (patch-guile origin-patch-guile                 ; package or #f
               (default #f)))

(define (print-origin origin port)
  "Write a concise representation of ORIGIN to PORT."
  (match origin
    (($ <origin> uri method sha256 file-name patches)
     (simple-format port "#<origin ~s ~a ~s ~a>"
                    uri (bytevector->base32-string sha256)
                    patches
                    (number->string (object-address origin) 16)))))

(set-record-type-printer! <origin> print-origin)

(define-syntax base32
  (lambda (s)
    "Return the bytevector corresponding to the given Nix-base32
representation."
    (syntax-case s ()
      ((_ str)
       (string? (syntax->datum #'str))
       ;; A literal string: do the conversion at expansion time.
       (with-syntax ((bv (nix-base32-string->bytevector
                          (syntax->datum #'str))))
         #''bv))
      ((_ str)
       #'(nix-base32-string->bytevector str)))))

;; The specification of a search path.
(define-record-type* <search-path-specification>
  search-path-specification make-search-path-specification
  search-path-specification?
  (variable     search-path-specification-variable)
  (directories  search-path-specification-directories)
  (separator    search-path-specification-separator (default ":")))

(define (search-path-specification->sexp spec)
  "Return an sexp representing SPEC, a <search-path-specification>.  The sexp
corresponds to the arguments expected by `set-path-environment-variable'."
  (match spec
    (($ <search-path-specification> variable directories separator)
     `(,variable ,directories ,separator))))

;; A package.
(define-record-type* <package>
  package make-package
  package?
  (name   package-name)                   ; string
  (version package-version)               ; string
  (source package-source)                 ; <origin> instance
  (build-system package-build-system)     ; build system
  (arguments package-arguments            ; arguments for the build method
             (default '()) (thunked))

  (inputs package-inputs                  ; input packages or derivations
          (default '()) (thunked))
  (propagated-inputs package-propagated-inputs    ; same, but propagated
                     (default '()) (thunked))
  (native-inputs package-native-inputs    ; native input packages/derivations
                 (default '()) (thunked))
  (self-native-input? package-self-native-input?  ; whether to use itself as
                                                  ; a native input when cross-
                      (default #f))               ; compiling

  (outputs package-outputs                ; list of strings
           (default '("out")))

                                                  ; lists of
                                                  ; <search-path-specification>,
                                                  ; for native and cross
                                                  ; inputs
  (native-search-paths package-native-search-paths (default '()))
  (search-paths package-search-paths (default '()))

  (synopsis package-synopsis)                    ; one-line description
  (description package-description)              ; one or two paragraphs
  (license package-license)
  (home-page package-home-page)
  (platforms package-platforms (default '()))
  (maintainers package-maintainers (default '()))

  (properties package-properties (default '()))   ; alist for anything else

  (location package-location
            (default (and=> (current-source-location)
                            source-properties->location))))

(set-record-type-printer! <package>
                          (lambda (package port)
                            (let ((loc    (package-location package))
                                  (format simple-format))
                              (format port "#<package ~a-~a ~a:~a ~a>"
                                      (package-name package)
                                      (package-version package)
                                      (location-file loc)
                                      (location-line loc)
                                      (number->string (object-address
                                                       package)
                                                      16)))))

(define (package-field-location package field)
  "Return the source code location of the definition of FIELD for PACKAGE, or
#f if it could not be determined."
  (define (goto port line column)
    (unless (and (= (port-column port) (- column 1))
                 (= (port-line port) (- line 1)))
      (unless (eof-object? (read-char port))
        (goto port line column))))

  (match (package-location package)
    (($ <location> file line column)
     (catch 'system
       (lambda ()
         ;; In general we want to keep relative file names for modules.
         (with-fluids ((%file-port-name-canonicalization 'relative))
           (call-with-input-file (search-path %load-path file)
             (lambda (port)
               (goto port line column)
               (match (read port)
                 (('package inits ...)
                  (let ((field (assoc field inits)))
                    (match field
                      ((_ value)
                       ;; Put the `or' here, and not in the first argument of
                       ;; `and=>', to work around a compiler bug in 2.0.5.
                       (or (and=> (source-properties value)
                                  source-properties->location)
                           (and=> (source-properties field)
                                  source-properties->location)))
                      (_
                       #f))))
                 (_
                  #f))))))
       (lambda _
         #f)))
    (_ #f)))


;; Error conditions.

(define-condition-type &package-error &error
  package-error?
  (package package-error-package))

(define-condition-type &package-input-error &package-error
  package-input-error?
  (input package-error-invalid-input))

(define-condition-type &package-cross-build-system-error &package-error
  package-cross-build-system-error?)


(define (package-full-name package)
  "Return the full name of PACKAGE--i.e., `NAME-VERSION'."
  (string-append (package-name package) "-" (package-version package)))

(define (%standard-patch-inputs)
  (let ((ref (lambda (module var)
               (module-ref (resolve-interface module) var))))
    `(("tar"   ,(ref '(gnu packages base) 'tar))
      ("xz"    ,(ref '(gnu packages compression) 'xz))
      ("bzip2" ,(ref '(gnu packages compression) 'bzip2))
      ("gzip"  ,(ref '(gnu packages compression) 'gzip))
      ("lzip"  ,(ref '(gnu packages compression) 'lzip))
      ("patch" ,(ref '(gnu packages base) 'patch)))))

(define (default-guile)
  "Return the default Guile package for SYSTEM."
  (let ((distro (resolve-interface '(gnu packages commencement))))
    (module-ref distro 'guile-final)))

(define* (patch-and-repack store source patches
                           #:key
                           (inputs '())
                           (snippet #f)
                           (flags '("-p1"))
                           (modules '())
                           (imported-modules '())
                           (guile-for-build (%guile-for-build))
                           (system (%current-system)))
  "Unpack SOURCE (a derivation or store path), apply all of PATCHES, and
repack the tarball using the tools listed in INPUTS.  When SNIPPET is true,
it must be an s-expression that will run from within the directory where
SOURCE was unpacked, after all of PATCHES have been applied.  MODULES and
IMPORTED-MODULES specify modules to use/import for use by SNIPPET."
  (define source-file-name
    ;; SOURCE is usually a derivation, but it could be a store file.
    (if (derivation? source)
        (derivation->output-path source)
        source))

  (define decompression-type
    (cond ((string-suffix? "gz" source-file-name)  "gzip")
          ((string-suffix? "bz2" source-file-name) "bzip2")
          ((string-suffix? "lz" source-file-name)  "lzip")
          (else "xz")))

  (define original-file-name
    ;; Remove the store prefix plus the slash, hash, and hyphen.
    (let* ((sans (string-drop source-file-name
                              (+ (string-length (%store-prefix)) 1)))
           (dash (string-index sans #\-)))
      (string-drop sans (+ 1 dash))))

  (define (numeric-extension? file-name)
    ;; Return true if FILE-NAME ends with digits.
    (and=> (file-extension file-name)
           (cut string-every char-set:hex-digit <>)))

  (define (tarxz-name file-name)
    ;; Return a '.tar.xz' file name based on FILE-NAME.
    (let ((base (if (numeric-extension? file-name)
                    original-file-name
                    (file-sans-extension file-name))))
      (string-append base
                     (if (equal? (file-extension base) "tar")
                         ".xz"
                         ".tar.xz"))))

  (define patch-inputs
    (map (lambda (number patch)
           (list (string-append "patch" (number->string number))
                 (match patch
                   ((? string?)
                    (add-to-store store (basename patch) #t
                                  "sha256" patch))
                   ((? origin?)
                    (package-source-derivation store patch)))))
         (iota (length patches))

         patches))

  (define builder
    `(begin
       (use-modules (ice-9 ftw)
                    (srfi srfi-1)
                    (guix build utils))

       (let ((out     (assoc-ref %outputs "out"))
             (xz      (assoc-ref %build-inputs "xz"))
             (decomp  (assoc-ref %build-inputs ,decompression-type))
             (source  (assoc-ref %build-inputs "source"))
             (tar     (string-append (assoc-ref %build-inputs "tar")
                                     "/bin/tar"))
             (patch   (string-append (assoc-ref %build-inputs "patch")
                                     "/bin/patch")))
         (define (apply-patch input)
           (let ((patch* (assoc-ref %build-inputs input)))
             (format (current-error-port) "applying '~a'...~%" patch*)
             (zero? (system* patch "--batch" ,@flags "--input" patch*))))

         (define (first-file directory)
           ;; Return the name of the first file in DIRECTORY.
           (car (scandir directory
                         (lambda (name)
                           (not (member name '("." "..")))))))

         (setenv "PATH" (string-append xz "/bin" ":"
                                       decomp "/bin"))

         ;; SOURCE may be either a directory or a tarball.
         (and (if (file-is-directory? source)
                  (let* ((store     (or (getenv "NIX_STORE") "/gnu/store"))
                         (len       (+ 1 (string-length store)))
                         (base      (string-drop source len))
                         (dash      (string-index base #\-))
                         (directory (string-drop base (+ 1 dash))))
                    (mkdir directory)
                    (copy-recursively source directory)
                    #t)
                  (zero? (system* tar "xvf" source)))
              (let ((directory (first-file ".")))
                (format (current-error-port)
                        "source is under '~a'~%" directory)
                (chdir directory)

                (and (every apply-patch ',(map car patch-inputs))

                     ,@(if snippet
                           `((let ((module (make-fresh-user-module)))
                               (module-use-interfaces! module
                                                       (map resolve-interface
                                                            ',modules))
                               (module-define! module '%build-inputs
                                               %build-inputs)
                               (module-define! module '%outputs %outputs)
                               ((@ (system base compile) compile)
                                ',snippet
                                #:to 'value
                                #:opts %auto-compilation-options
                                #:env module)))
                           '())

                     (begin (chdir "..") #t)
                     (zero? (system* tar "cvfa" out directory))))))))


  (let ((name    (tarxz-name original-file-name))
        (inputs  (filter-map (match-lambda
                              ((name (? package? p))
                               (and (member name (cons decompression-type
                                                       '("tar" "xz" "patch")))
                                    (list name
                                          (package-derivation store p
                                                              system)))))
                             (or inputs (%standard-patch-inputs))))
        (modules (delete-duplicates (cons '(guix build utils) modules))))

    (build-expression->derivation store name builder
                                 #:inputs `(("source" ,source)
                                            ,@inputs
                                            ,@patch-inputs)
                                 #:system system
                                 #:modules modules
                                 #:guile-for-build guile-for-build)))

(define* (package-source-derivation store source
                                    #:optional (system (%current-system)))
  "Return the derivation path for SOURCE, a package source, for SYSTEM."
  (match source
    (($ <origin> uri method sha256 name () #f)
     ;; No patches, no snippet: this is a fixed-output derivation.
     (method store uri 'sha256 sha256 name
             #:system system))
    (($ <origin> uri method sha256 name (patches ...) snippet
        (flags ...) inputs (modules ...) (imported-modules ...)
        guile-for-build)
     ;; Patches and/or a snippet.
     (let ((source (method store uri 'sha256 sha256 name
                           #:system system))
           (guile  (match (or guile-for-build (%guile-for-build)
                              (default-guile))
                     ((? package? p)
                      (package-derivation store p system))
                     ((? derivation? drv)
                      drv))))
       (patch-and-repack store source patches
                         #:inputs inputs
                         #:snippet snippet
                         #:flags flags
                         #:system system
                         #:modules modules
                         #:imported-modules modules
                         #:guile-for-build guile)))
    ((and (? string?) (? direct-store-path?) file)
     file)
    ((? string? file)
     (add-to-store store (basename file) #t "sha256" file))))

(define (transitive-inputs inputs)
  (let loop ((inputs  inputs)
             (result '()))
    (match inputs
      (()
       (delete-duplicates (reverse result)))      ; XXX: efficiency
      (((and i (name (? package? p) sub ...)) rest ...)
       (let ((t (map (match-lambda
                      ((dep-name derivation ...)
                       (cons (string-append name "/" dep-name)
                             derivation)))
                     (package-propagated-inputs p))))
         (loop (append t rest)
               (append t (cons i result)))))
      ((input rest ...)
       (loop rest (cons input result))))))

(define (package-direct-inputs package)
  "Return all the direct inputs of PACKAGE---i.e, its direct inputs along
with their propagated inputs."
  (append (package-native-inputs package)
          (package-inputs package)
          (package-propagated-inputs package)))

(define (package-transitive-inputs package)
  "Return the transitive inputs of PACKAGE---i.e., its direct inputs along
with their propagated inputs, recursively."
  (transitive-inputs (package-direct-inputs package)))

(define (package-transitive-target-inputs package)
  "Return the transitive target inputs of PACKAGE---i.e., its direct inputs
along with their propagated inputs, recursively.  This only includes inputs
for the target system, and not native inputs."
  (transitive-inputs (append (package-inputs package)
                             (package-propagated-inputs package))))

(define (package-transitive-native-inputs package)
  "Return the transitive native inputs of PACKAGE---i.e., its direct inputs
along with their propagated inputs, recursively.  This only includes inputs
for the host system (\"native inputs\"), and not target inputs."
  (transitive-inputs (package-native-inputs package)))

(define (package-transitive-propagated-inputs package)
  "Return the propagated inputs of PACKAGE, and their propagated inputs,
recursively."
  (transitive-inputs (package-propagated-inputs package)))

(define (bag-transitive-inputs bag)
  "Same as 'package-transitive-inputs', but applied to a bag."
  (transitive-inputs (append (bag-build-inputs bag)
                             (bag-host-inputs bag)
                             (bag-target-inputs bag))))

(define (bag-transitive-build-inputs bag)
  "Same as 'package-transitive-native-inputs', but applied to a bag."
  (transitive-inputs (bag-build-inputs bag)))

(define (bag-transitive-host-inputs bag)
  "Same as 'package-transitive-target-inputs', but applied to a bag."
  (transitive-inputs (bag-host-inputs bag)))

(define (bag-transitive-target-inputs bag)
  "Return the \"target inputs\" of BAG, recursively."
  (transitive-inputs (bag-target-inputs bag)))


;;;
;;; Package derivations.
;;;

(define %derivation-cache
  ;; Package to derivation-path mapping.
  (make-weak-key-hash-table 100))

(define (cache package system thunk)
  "Memoize the return values of THUNK as the derivation of PACKAGE on
SYSTEM."
  ;; FIXME: This memoization should be associated with the open store, because
  ;; otherwise it breaks when switching to a different store.
  (let ((vals (call-with-values thunk list)))
    ;; Use `hashq-set!' instead of `hash-set!' because `hash' returns the
    ;; same value for all structs (as of Guile 2.0.6), and because pointer
    ;; equality is sufficient in practice.
    (hashq-set! %derivation-cache package `((,system ,@vals)))
    (apply values vals)))

(define-syntax-rule (cached package system body ...)
  "Memoize the result of BODY for the arguments PACKAGE and SYSTEM.
Return the cached result when available."
  (let ((thunk (lambda () body ...)))
    (match (hashq-ref %derivation-cache package)
      ((alist (... ...))
       (match (assoc-ref alist system)
         ((vals (... ...))
          (apply values vals))
         (#f
          (cache package system thunk))))
      (#f
       (cache package system thunk)))))

(define* (expand-input store package input system #:optional cross-system)
  "Expand INPUT, an input tuple, such that it contains only references to
derivation paths or store paths.  PACKAGE is only used to provide contextual
information in exceptions."
  (define (intern file)
    ;; Add FILE to the store.  Set the `recursive?' bit to #t, so that
    ;; file permissions are preserved.
    (add-to-store store (basename file) #t "sha256" file))

  (define derivation
    (if cross-system
        (cut package-cross-derivation store <> cross-system system)
        (cut package-derivation store <> system)))

  (match input
    (((? string? name) (? package? package))
     (list name (derivation package)))
    (((? string? name) (? package? package)
      (? string? sub-drv))
     (list name (derivation package)
           sub-drv))
    (((? string? name)
      (and (? string?) (? derivation-path?) drv))
     (list name drv))
    (((? string? name)
      (and (? string?) (? file-exists? file)))
     ;; Add FILE to the store.  When FILE is in the sub-directory of a
     ;; store path, it needs to be added anyway, so it can be used as a
     ;; source.
     (list name (intern file)))
    (((? string? name) (? origin? source))
     (list name (package-source-derivation store source system)))
    (x
     (raise (condition (&package-input-error
                        (package package)
                        (input   x)))))))

(define* (package->bag package #:optional
                       (system (%current-system))
                       (target (%current-target-system)))
  "Compile PACKAGE into a bag for SYSTEM, possibly cross-compiled to TARGET,
and return it."
  ;; Bind %CURRENT-SYSTEM and %CURRENT-TARGET-SYSTEM so that thunked field
  ;; values can refer to it.
  (parameterize ((%current-system system)
                 (%current-target-system target))
    (match package
      (($ <package> name version source build-system
                    args inputs propagated-inputs native-inputs self-native-input?
                    outputs)
       (or (make-bag build-system (package-full-name package)
                     #:target target
                     #:source source
                     #:inputs (append (inputs)
                                      (propagated-inputs))
                     #:outputs outputs
                     #:native-inputs `(,@(if (and target self-native-input?)
                                             `(("self" ,package))
                                             '())
                                       ,@(native-inputs))
                     #:arguments (args))
           (raise (if target
                      (condition
                       (&package-cross-build-system-error
                        (package package)))
                      (condition
                       (&package-error
                        (package package))))))))))

(define* (package-derivation store package
                             #:optional (system (%current-system)))
  "Return the <derivation> object of PACKAGE for SYSTEM."

  ;; Compute the derivation and cache the result.  Caching is important
  ;; because some derivations, such as the implicit inputs of the GNU build
  ;; system, will be queried many, many times in a row.
  (cached package system
          (let* ((bag        (package->bag package system #f))
                 (inputs     (bag-transitive-inputs bag))
                 (input-drvs (map (cut expand-input
                                       store package <> system)
                                  inputs))
                 (paths      (delete-duplicates
                              (append-map (match-lambda
                                           ((_ (? package? p) _ ...)
                                            (package-native-search-paths
                                             p))
                                           (_ '()))
                                          inputs))))

            (apply (bag-build bag)
                   store (bag-name bag)
                   input-drvs
                   #:search-paths paths
                   #:outputs (bag-outputs bag) #:system system
                   (bag-arguments bag)))))

(define* (package-cross-derivation store package target
                                   #:optional (system (%current-system)))
  "Cross-build PACKAGE for TARGET (a GNU triplet) from host SYSTEM (a Guix
system identifying string)."
  (cached package (cons system target)
          (let* ((bag         (package->bag package system target))
                 (host        (bag-transitive-host-inputs bag))
                 (host-drvs   (map (cut expand-input
                                        store package <>
                                        system target)
                                   host))
                 (target*     (bag-transitive-target-inputs bag))
                 (target-drvs (map (cut expand-input
                                        store package <> system)
                                   target*))
                 (build       (bag-transitive-build-inputs bag))
                 (build-drvs  (map (cut expand-input
                                        store package <> system)
                                   build))
                 (all         (append build target* host))
                 (paths       (delete-duplicates
                               (append-map (match-lambda
                                            ((_ (? package? p) _ ...)
                                             (package-search-paths p))
                                            (_ '()))
                                           all)))
                 (npaths      (delete-duplicates
                               (append-map (match-lambda
                                            ((_ (? package? p) _ ...)
                                             (package-native-search-paths
                                              p))
                                            (_ '()))
                                           all))))

            (apply (bag-build bag)
                   store (bag-name bag)
                   #:native-drvs build-drvs
                   #:target-drvs (append host-drvs target-drvs)
                   #:search-paths paths
                   #:native-search-paths npaths
                   #:outputs (bag-outputs bag)
                   #:system system #:target target
                   (bag-arguments bag)))))

(define* (package-output store package
                         #:optional (output "out") (system (%current-system)))
  "Return the output path of PACKAGE's OUTPUT for SYSTEM---where OUTPUT is the
symbolic output name, such as \"out\".  Note that this procedure calls
`package-derivation', which is costly."
  (let ((drv (package-derivation store package system)))
    (derivation->output-path drv output)))
