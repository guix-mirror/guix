;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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
            package-cross-build-system-error?))

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
  (method    origin-method)                       ; symbol
  (sha256    origin-sha256)                       ; bytevector
  (file-name origin-file-name (default #f))       ; optional file name
  (patches   origin-patches (default '()))        ; list of file names
  (patch-flags  origin-patch-flags                ; list of strings
                (default '("-p1")))

  ;; Patching requires Guile, GNU Patch, and a few more.  These two fields are
  ;; used to specify these dependencies when needed.
  (patch-inputs origin-patch-inputs               ; input list or #f
                (default #f))
  (patch-guile origin-patch-guile                 ; package or #f
               (default #f)))

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
                #f)))))
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
  (let ((distro (resolve-interface '(gnu packages base))))
    (module-ref distro 'guile-final)))

(define* (patch-and-repack store source patches inputs
                           #:key
                           (flags '("-p1"))
                           (guile-for-build (%guile-for-build))
                           (system (%current-system)))
  "Unpack SOURCE (a derivation), apply all of PATCHES, and repack the tarball
using the tools listed in INPUTS."
  (define decompression-type
    (let ((out (derivation->output-path source)))
      (cond ((string-suffix? "gz" out)  "gzip")
            ((string-suffix? "bz2" out) "bzip2")
            ((string-suffix? "lz" out)  "lzip")
            (else "xz"))))

  (define original-file-name
    (let ((out (derivation->output-path source)))
      ;; Remove the store prefix plus the slash, hash, and hyphen.
      (let* ((sans (string-drop out (+ (string-length (%store-prefix)) 1)))
             (dash (string-index sans #\-)))
        (string-drop sans (+ 1 dash)))))

  (define patch-inputs
    (map (lambda (number patch)
           (list (string-append "patch" (number->string number))
                 (add-to-store store (basename patch) #t
                               "sha256" patch)))
         (iota (length patches))

         patches))

  (define builder
    `(begin
       (use-modules (ice-9 ftw)
                    (srfi srfi-1))

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

         (setenv "PATH" (string-append xz "/bin" ":"
                                       decomp "/bin"))
         (and (zero? (system* tar "xvf" source))
              (let ((directory (car (scandir "."
                                             (lambda (name)
                                               (not
                                                (member name
                                                        '("." ".."))))))))
                (format (current-error-port)
                        "source is under '~a'~%" directory)
                (chdir directory)
                (and (every apply-patch ',(map car patch-inputs))
                     (begin (chdir "..") #t)
                     (zero? (system* tar "cvfa" out directory))))))))


  (let ((name   (string-append (file-sans-extension original-file-name)
                               ".xz"))
        (inputs (filter-map (match-lambda
                             ((name (? package? p))
                              (and (member name (cons decompression-type
                                                      '("tar" "xz" "patch")))
                                   (list name
                                         (package-derivation store p
                                                             system)))))
                            (or inputs (%standard-patch-inputs)))))

   (build-expression->derivation store name system builder
                                 `(("source" ,source)
                                   ,@inputs
                                   ,@patch-inputs)
                                 #:guile-for-build guile-for-build)))

(define* (package-source-derivation store source
                                    #:optional (system (%current-system)))
  "Return the derivation path for SOURCE, a package source, for SYSTEM."
  (match source
    (($ <origin> uri method sha256 name ())
     ;; No patches.
     (method store uri 'sha256 sha256 name
             #:system system))
    (($ <origin> uri method sha256 name (patches ...) (flags ...)
        inputs guile-for-build)
     ;; One or more patches.
     (let ((source (method store uri 'sha256 sha256 name
                           #:system system))
           (guile  (match (or guile-for-build (%guile-for-build)
                              (default-guile))
                     ((? package? p)
                      (package-derivation store p system))
                     ((? derivation? drv)
                      drv))))
       (patch-and-repack store source patches inputs
                         #:flags flags
                         #:system system
                         #:guile-for-build guile)))
    ((and (? string?) (? store-path?) file)
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

(define (package-transitive-inputs package)
  "Return the transitive inputs of PACKAGE---i.e., its direct inputs along
with their propagated inputs, recursively."
  (transitive-inputs (append (package-native-inputs package)
                             (package-inputs package)
                             (package-propagated-inputs package))))

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


;;;
;;; Package derivations.
;;;

(define %derivation-cache
  ;; Package to derivation-path mapping.
  (make-weak-key-hash-table 100))

(define (cache package system thunk)
  "Memoize the return values of THUNK as the derivation of PACKAGE on
SYSTEM."
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

(define* (package-derivation store package
                             #:optional (system (%current-system)))
  "Return the <derivation> object of PACKAGE for SYSTEM."

  ;; Compute the derivation and cache the result.  Caching is important
  ;; because some derivations, such as the implicit inputs of the GNU build
  ;; system, will be queried many, many times in a row.
  (cached package system

          ;; Bind %CURRENT-SYSTEM so that thunked field values can refer
          ;; to it.
          (parameterize ((%current-system system)
                         (%current-target-system #f))
            (match package
              (($ <package> name version source (= build-system-builder builder)
                  args inputs propagated-inputs native-inputs self-native-input?
                  outputs)
               (let* ((inputs     (package-transitive-inputs package))
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

                 (apply builder
                        store (package-full-name package)
                        (and source
                             (package-source-derivation store source system))
                        input-drvs
                        #:search-paths paths
                        #:outputs outputs #:system system
                        (args))))))))

(define* (package-cross-derivation store package target
                                   #:optional (system (%current-system)))
  "Cross-build PACKAGE for TARGET (a GNU triplet) from host SYSTEM (a Guix
system identifying string)."
  (cached package (cons system target)

          ;; Bind %CURRENT-SYSTEM so that thunked field values can refer
          ;; to it.
          (parameterize ((%current-system system)
                         (%current-target-system target))
            (match package
              (($ <package> name version source
                  (= build-system-cross-builder builder)
                  args inputs propagated-inputs native-inputs self-native-input?
                  outputs)
               (unless builder
                 (raise (condition
                         (&package-cross-build-system-error
                          (package package)))))

               (let* ((inputs     (package-transitive-target-inputs package))
                      (input-drvs (map (cut expand-input
                                            store package <>
                                            system target)
                                       inputs))
                      (host       (append (if self-native-input?
                                              `(("self" ,package))
                                              '())
                                          (package-transitive-native-inputs package)))
                      (host-drvs  (map (cut expand-input
                                            store package <> system)
                                       host))
                      (all        (append host inputs))
                      (paths      (delete-duplicates
                                   (append-map (match-lambda
                                                ((_ (? package? p) _ ...)
                                                 (package-search-paths p))
                                                (_ '()))
                                               all)))
                      (npaths     (delete-duplicates
                                   (append-map (match-lambda
                                                ((_ (? package? p) _ ...)
                                                 (package-native-search-paths
                                                  p))
                                                (_ '()))
                                               all))))

                 (apply builder
                        store (package-full-name package) target
                        (and source
                             (package-source-derivation store source system))
                        input-drvs host-drvs
                        #:search-paths paths
                        #:native-search-paths npaths
                        #:outputs outputs #:system system
                        (args))))))))

(define* (package-output store package
                         #:optional (output "out") (system (%current-system)))
  "Return the output path of PACKAGE's OUTPUT for SYSTEM---where OUTPUT is the
symbolic output name, such as \"out\".  Note that this procedure calls
`package-derivation', which is costly."
  (let ((drv (package-derivation store package system)))
    (derivation->output-path drv output)))
