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

(define-module (guix derivations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 vlist)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix records)
  #:export (<derivation>
            derivation?
            derivation-outputs
            derivation-inputs
            derivation-sources
            derivation-system
            derivation-builder-arguments
            derivation-builder-environment-vars
            derivation-file-name
            derivation-prerequisites
            derivation-prerequisites-to-build

            <derivation-output>
            derivation-output?
            derivation-output-path
            derivation-output-hash-algo
            derivation-output-hash
            derivation-output-recursive?

            <derivation-input>
            derivation-input?
            derivation-input-path
            derivation-input-sub-derivations
            derivation-input-output-paths

            fixed-output-derivation?
            offloadable-derivation?
            substitutable-derivation?
            derivation-hash

            read-derivation
            write-derivation
            derivation->output-path
            derivation->output-paths
            derivation-path->output-path
            derivation-path->output-paths
            derivation

            graft
            graft?
            graft-origin
            graft-replacement
            graft-origin-output
            graft-replacement-output
            graft-derivation

            map-derivation

            %guile-for-build
            imported-modules
            compiled-modules
            build-expression->derivation
            imported-files)
  #:replace (build-derivations))

;;;
;;; Nix derivations, as implemented in Nix's `derivations.cc'.
;;;

(define-record-type <derivation>
  (make-derivation outputs inputs sources system builder args env-vars
                   file-name)
  derivation?
  (outputs  derivation-outputs)      ; list of name/<derivation-output> pairs
  (inputs   derivation-inputs)       ; list of <derivation-input>
  (sources  derivation-sources)      ; list of store paths
  (system   derivation-system)       ; string
  (builder  derivation-builder)      ; store path
  (args     derivation-builder-arguments)         ; list of strings
  (env-vars derivation-builder-environment-vars)  ; list of name/value pairs
  (file-name derivation-file-name))               ; the .drv file name

(define-record-type <derivation-output>
  (make-derivation-output path hash-algo hash recursive?)
  derivation-output?
  (path       derivation-output-path)             ; store path
  (hash-algo  derivation-output-hash-algo)        ; symbol | #f
  (hash       derivation-output-hash)             ; bytevector | #f
  (recursive? derivation-output-recursive?))      ; Boolean

(define-record-type <derivation-input>
  (make-derivation-input path sub-derivations)
  derivation-input?
  (path            derivation-input-path)             ; store path
  (sub-derivations derivation-input-sub-derivations)) ; list of strings

(set-record-type-printer! <derivation>
                          (lambda (drv port)
                            (format port "#<derivation ~a => ~a ~a>"
                                    (derivation-file-name drv)
                                    (string-join
                                     (map (match-lambda
                                           ((_ . output)
                                            (derivation-output-path output)))
                                          (derivation-outputs drv)))
                                    (number->string (object-address drv) 16))))

(define (fixed-output-derivation? drv)
  "Return #t if DRV is a fixed-output derivation, such as the result of a
download with a fixed hash (aka. `fetchurl')."
  (match drv
    (($ <derivation>
        (("out" . ($ <derivation-output> _ (? symbol?) (? bytevector?)))))
     #t)
    (_ #f)))

(define (derivation-input-output-paths input)
  "Return the list of output paths corresponding to INPUT, a
<derivation-input>."
  (match input
    (($ <derivation-input> path sub-drvs)
     (map (cut derivation-path->output-path path <>)
          sub-drvs))))

(define (derivation-prerequisites drv)
  "Return the list of derivation-inputs required to build DRV, recursively."
  (let loop ((drv    drv)
             (result '()))
    (let ((inputs (remove (cut member <> result)  ; XXX: quadratic
                          (derivation-inputs drv))))
      (fold loop
            (append inputs result)
            (map (lambda (i)
                   (call-with-input-file (derivation-input-path i)
                     read-derivation))
                 inputs)))))

(define (offloadable-derivation? drv)
  "Return true if DRV can be offloaded, false otherwise."
  (match (assoc "preferLocalBuild"
                (derivation-builder-environment-vars drv))
    (("preferLocalBuild" . "1") #f)
    (_ #t)))

(define substitutable-derivation?
  ;; Return #t if the derivation can be substituted.  Currently the two are
  ;; synonymous, see <http://bugs.gnu.org/18747>.
  offloadable-derivation?)

(define* (derivation-prerequisites-to-build store drv
                                            #:key
                                            (outputs
                                             (map
                                              car
                                              (derivation-outputs drv)))
                                            (use-substitutes? #t))
  "Return two values: the list of derivation-inputs required to build the
OUTPUTS of DRV and not already available in STORE, recursively, and the list
of required store paths that can be substituted.  When USE-SUBSTITUTES? is #f,
that second value is the empty list."
  (define (derivation-output-paths drv sub-drvs)
    (match drv
      (($ <derivation> outputs)
       (map (lambda (sub-drv)
              (derivation-output-path (assoc-ref outputs sub-drv)))
            sub-drvs))))

  (define built?
    (cut valid-path? store <>))

  (define substitutable?
    ;; Return true if the given path is substitutable.  Call
    ;; `substitutable-paths' upfront, to benefit from parallelism in the
    ;; substituter.
    (if use-substitutes?
        (let ((s (substitutable-paths store
                                      (append
                                       (derivation-output-paths drv outputs)
                                       (append-map
                                        derivation-input-output-paths
                                        (derivation-prerequisites drv))))))
          (cut member <> s))
        (const #f)))

  (define input-built?
    (compose (cut any built? <>) derivation-input-output-paths))

  (define input-substitutable?
    ;; Return true if and only if all of SUB-DRVS are subsitutable.  If at
    ;; least one is missing, then everything must be rebuilt.
    (compose (cut every substitutable? <>) derivation-input-output-paths))

  (define (derivation-built? drv sub-drvs)
    (every built? (derivation-output-paths drv sub-drvs)))

  (define (derivation-substitutable? drv sub-drvs)
    (and (substitutable-derivation? drv)
         (every substitutable? (derivation-output-paths drv sub-drvs))))

  (let loop ((drv        drv)
             (sub-drvs   outputs)
             (build      '())
             (substitute '()))
    (cond ((derivation-built? drv sub-drvs)
           (values build substitute))
          ((derivation-substitutable? drv sub-drvs)
           (values build
                   (append (derivation-output-paths drv sub-drvs)
                           substitute)))
          (else
           (let ((build  (if (substitutable-derivation? drv)
                             build
                             (cons (make-derivation-input
                                    (derivation-file-name drv) sub-drvs)
                                   build)))
                 (inputs (remove (lambda (i)
                                   (or (member i build) ; XXX: quadratic
                                       (input-built? i)
                                       (input-substitutable? i)))
                                 (derivation-inputs drv))))
             (fold2 loop
                    (append inputs build)
                    (append (append-map (lambda (input)
                                          (if (and (not (input-built? input))
                                                   (input-substitutable? input))
                                              (derivation-input-output-paths
                                               input)
                                              '()))
                                        (derivation-inputs drv))
                            substitute)
                    (map (lambda (i)
                           (call-with-input-file (derivation-input-path i)
                             read-derivation))
                         inputs)
                    (map derivation-input-sub-derivations inputs)))))))

(define (%read-derivation drv-port)
  ;; Actually read derivation from DRV-PORT.

  (define comma (string->symbol ","))

  (define (ununquote x)
    (match x
      (('unquote x) (ununquote x))
      ((x ...)      (map ununquote x))
      (_            x)))

  (define (outputs->alist x)
    (fold-right (lambda (output result)
                  (match output
                    ((name path "" "")
                     (alist-cons name
                                 (make-derivation-output path #f #f #f)
                                 result))
                    ((name path hash-algo hash)
                     ;; fixed-output
                     (let* ((rec? (string-prefix? "r:" hash-algo))
                            (algo (string->symbol
                                   (if rec?
                                       (string-drop hash-algo 2)
                                       hash-algo)))
                            (hash (base16-string->bytevector hash)))
                       (alist-cons name
                                   (make-derivation-output path algo
                                                           hash rec?)
                                   result)))))
                '()
                x))

  (define (make-input-drvs x)
    (fold-right (lambda (input result)
                  (match input
                    ((path (sub-drvs ...))
                     (cons (make-derivation-input path sub-drvs)
                           result))))
                '()
                x))

  ;; The contents of a derivation are typically ASCII, but choosing
  ;; UTF-8 allows us to take the fast path for Guile's `scm_getc'.
  (set-port-encoding! drv-port "UTF-8")

  (let loop ((exp    (read drv-port))
             (result '()))
    (match exp
      ((? eof-object?)
       (let ((result (reverse result)))
         (match result
           (('Derive ((outputs ...) (input-drvs ...)
                      (input-srcs ...)
                      (? string? system)
                      (? string? builder)
                      ((? string? args) ...)
                      ((var value) ...)))
            (make-derivation (outputs->alist outputs)
                             (make-input-drvs input-drvs)
                             input-srcs
                             system builder args
                             (fold-right alist-cons '() var value)
                             (port-filename drv-port)))
           (_
            (error "failed to parse derivation" drv-port result)))))
      ((? (cut eq? <> comma))
       (loop (read drv-port) result))
      (_
       (loop (read drv-port)
             (cons (ununquote exp) result))))))

(define read-derivation
  (let ((cache (make-weak-value-hash-table 200)))
    (lambda (drv-port)
      "Read the derivation from DRV-PORT and return the corresponding
<derivation> object."
      ;; Memoize that operation because `%read-derivation' is quite expensive,
      ;; and because the same argument is read more than 15 times on average
      ;; during something like (package-derivation s gdb).
      (let ((file (and=> (port-filename drv-port) basename)))
        (or (and file (hash-ref cache file))
            (let ((drv (%read-derivation drv-port)))
              (hash-set! cache file drv)
              drv))))))

(define-inlinable (write-sequence lst write-item port)
  ;; Write each element of LST with WRITE-ITEM to PORT, separating them with a
  ;; comma.
  (match lst
    (()
     #t)
    ((prefix (... ...) last)
     (for-each (lambda (item)
                 (write-item item port)
                 (display "," port))
               prefix)
     (write-item last port))))

(define-inlinable (write-list lst write-item port)
  ;; Write LST as a derivation list to PORT, using WRITE-ITEM to write each
  ;; element.
  (display "[" port)
  (write-sequence lst write-item port)
  (display "]" port))

(define-inlinable (write-tuple lst write-item port)
  ;; Same, but write LST as a tuple.
  (display "(" port)
  (write-sequence lst write-item port)
  (display ")" port))

(define (write-derivation drv port)
  "Write the ATerm-like serialization of DRV to PORT.  See Section 2.4 of
Eelco Dolstra's PhD dissertation for an overview of a previous version of
that form."

  ;; Make sure we're using the faster implementation.
  (define format simple-format)

  (define (write-string-list lst)
    (write-list lst write port))

  (define (coalesce-duplicate-inputs inputs)
    ;; Return a list of inputs, such that when INPUTS contains the same DRV
    ;; twice, they are coalesced, with their sub-derivations merged.  This is
    ;; needed because Nix itself keeps only one of them.
    (fold (lambda (input result)
            (match input
              (($ <derivation-input> path sub-drvs)
               ;; XXX: quadratic
               (match (find (match-lambda
                             (($ <derivation-input> p s)
                              (string=? p path)))
                            result)
                 (#f
                  (cons input result))
                 ((and dup ($ <derivation-input> _ sub-drvs2))
                  ;; Merge DUP with INPUT.
                  (let ((sub-drvs (delete-duplicates
                                   (append sub-drvs sub-drvs2))))
                    (cons (make-derivation-input path sub-drvs)
                          (delq dup result))))))))
          '()
          inputs))

  (define (write-output output port)
    (match output
     ((name . ($ <derivation-output> path hash-algo hash recursive?))
      (write-tuple (list name path
                         (if hash-algo
                             (string-append (if recursive? "r:" "")
                                            (symbol->string hash-algo))
                             "")
                         (or (and=> hash bytevector->base16-string)
                             ""))
                   write
                   port))))

  (define (write-input input port)
    (match input
      (($ <derivation-input> path sub-drvs)
       (display "(" port)
       (write path port)
       (display "," port)
       (write-string-list (sort sub-drvs string<?))
       (display ")" port))))

  (define (write-env-var env-var port)
    (match env-var
      ((name . value)
       (display "(" port)
       (write name port)
       (display "," port)
       (write value port)
       (display ")" port))))

  ;; Note: lists are sorted alphabetically, to conform with the behavior of
  ;; C++ `std::map' in Nix itself.

  (match drv
    (($ <derivation> outputs inputs sources
        system builder args env-vars)
     (display "Derive(" port)
     (write-list (sort outputs
                       (lambda (o1 o2)
                         (string<? (car o1) (car o2))))
                 write-output
                 port)
     (display "," port)
     (write-list (sort (coalesce-duplicate-inputs inputs)
                       (lambda (i1 i2)
                         (string<? (derivation-input-path i1)
                                   (derivation-input-path i2))))
                 write-input
                 port)
     (display "," port)
     (write-string-list (sort sources string<?))
     (format port ",~s,~s," system builder)
     (write-string-list args)
     (display "," port)
     (write-list (sort env-vars
                       (lambda (e1 e2)
                         (string<? (car e1) (car e2))))
                 write-env-var
                 port)
     (display ")" port))))

(define derivation->string
  (memoize
   (lambda (drv)
     "Return the external representation of DRV as a string."
     (with-fluids ((%default-port-encoding "UTF-8"))
       (call-with-output-string
        (cut write-derivation drv <>))))))

(define* (derivation->output-path drv #:optional (output "out"))
  "Return the store path of its output OUTPUT."
  (let ((outputs (derivation-outputs drv)))
    (and=> (assoc-ref outputs output) derivation-output-path)))

(define (derivation->output-paths drv)
  "Return the list of name/path pairs of the outputs of DRV."
  (map (match-lambda
        ((name . output)
         (cons name (derivation-output-path output))))
       (derivation-outputs drv)))

(define derivation-path->output-path
  ;; This procedure is called frequently, so memoize it.
  (memoize
   (lambda* (path #:optional (output "out"))
     "Read the derivation from PATH (`/gnu/store/xxx.drv'), and return the store
path of its output OUTPUT."
     (derivation->output-path (call-with-input-file path read-derivation)
                              output))))

(define (derivation-path->output-paths path)
  "Read the derivation from PATH (`/gnu/store/xxx.drv'), and return the
list of name/path pairs of its outputs."
  (derivation->output-paths (call-with-input-file path read-derivation)))


;;;
;;; Derivation primitive.
;;;

(define (compressed-hash bv size)                 ; `compressHash'
  "Given the hash stored in BV, return a compressed version thereof that fits
in SIZE bytes."
  (define new (make-bytevector size 0))
  (define old-size (bytevector-length bv))
  (let loop ((i 0))
    (if (= i old-size)
        new
        (let* ((j (modulo i size))
               (o (bytevector-u8-ref new j)))
          (bytevector-u8-set! new j
                              (logxor o (bytevector-u8-ref bv i)))
          (loop (+ 1 i))))))

(define derivation-path->base16-hash
  (memoize
   (lambda (file)
     "Return a string containing the base16 representation of the hash of the
derivation at FILE."
     (call-with-input-file file
       (compose bytevector->base16-string
                derivation-hash
                read-derivation)))))

(define derivation-hash            ; `hashDerivationModulo' in derivations.cc
  (memoize
   (lambda (drv)
    "Return the hash of DRV, modulo its fixed-output inputs, as a bytevector."
    (match drv
      (($ <derivation> ((_ . ($ <derivation-output> path
                                (? symbol? hash-algo) (? bytevector? hash)
                                (? boolean? recursive?)))))
       ;; A fixed-output derivation.
       (sha256
        (string->utf8
         (string-append "fixed:out:"
                        (if recursive? "r:" "")
                        (symbol->string hash-algo)
                        ":" (bytevector->base16-string hash)
                        ":" path))))
      (($ <derivation> outputs inputs sources
          system builder args env-vars)
       ;; A regular derivation: replace the path of each input with that
       ;; input's hash; return the hash of serialization of the resulting
       ;; derivation.
       (let* ((inputs (map (match-lambda
                            (($ <derivation-input> path sub-drvs)
                             (let ((hash (derivation-path->base16-hash path)))
                               (make-derivation-input hash sub-drvs))))
                           inputs))
              (drv    (make-derivation outputs inputs sources
                                       system builder args env-vars
                                       #f)))

         ;; XXX: At this point this remains faster than `port-sha256', because
         ;; the SHA256 port's `write' method gets called for every single
         ;; character.
         (sha256
          (string->utf8 (derivation->string drv)))))))))

(define (store-path type hash name)               ; makeStorePath
  "Return the store path for NAME/HASH/TYPE."
  (let* ((s (string-append type ":sha256:"
                           (bytevector->base16-string hash) ":"
                           (%store-prefix) ":" name))
         (h (sha256 (string->utf8 s)))
         (c (compressed-hash h 20)))
    (string-append (%store-prefix) "/"
                   (bytevector->nix-base32-string c) "-"
                   name)))

(define (output-path output hash name)            ; makeOutputPath
  "Return an output path for OUTPUT (the name of the output as a string) of
the derivation called NAME with hash HASH."
  (store-path (string-append "output:" output) hash
              (if (string=? output "out")
                  name
                  (string-append name "-" output))))

(define (fixed-output-path output hash-algo hash recursive? name)
  "Return an output path for the fixed output OUTPUT defined by HASH of type
HASH-ALGO, of the derivation NAME.  RECURSIVE? has the same meaning as for
'add-to-store'."
  (if (and recursive? (eq? hash-algo 'sha256))
      (store-path "source" hash name)
      (let ((tag (string-append "fixed:" output ":"
                                (if recursive? "r:" "")
                                (symbol->string hash-algo) ":"
                                (bytevector->base16-string hash) ":")))
        (store-path (string-append "output:" output)
                    (sha256 (string->utf8 tag))
                    name))))

(define* (derivation store name builder args
                     #:key
                     (system (%current-system)) (env-vars '())
                     (inputs '()) (outputs '("out"))
                     hash hash-algo recursive?
                     references-graphs allowed-references
                     local-build?)
  "Build a derivation with the given arguments, and return the resulting
<derivation> object.  When HASH and HASH-ALGO are given, a
fixed-output derivation is created---i.e., one whose result is known in
advance, such as a file download.  If, in addition, RECURSIVE? is true, then
that fixed output may be an executable file or a directory and HASH must be
the hash of an archive containing this output.

When REFERENCES-GRAPHS is true, it must be a list of file name/store path
pairs.  In that case, the reference graph of each store path is exported in
the build environment in the corresponding file, in a simple text format.

When ALLOWED-REFERENCES is true, it must be a list of store items or outputs
that the derivation's output may refer to.

When LOCAL-BUILD? is true, declare that the derivation is not a good candidate
for offloading and should rather be built locally.  This is the case for small
derivations where the costs of data transfers would outweigh the benefits."
  (define (add-output-paths drv)
    ;; Return DRV with an actual store path for each of its output and the
    ;; corresponding environment variable.
    (match drv
      (($ <derivation> outputs inputs sources
          system builder args env-vars)
       (let* ((drv-hash (derivation-hash drv))
              (outputs  (map (match-lambda
                              ((output-name . ($ <derivation-output>
                                                 _ algo hash rec?))
                               (let ((path (if hash
                                               (fixed-output-path output-name
                                                                  algo hash
                                                                  rec? name)
                                               (output-path output-name
                                                            drv-hash name))))
                                 (cons output-name
                                       (make-derivation-output path algo
                                                               hash rec?)))))
                             outputs)))
         (make-derivation outputs inputs sources system builder args
                          (map (match-lambda
                                ((name . value)
                                 (cons name
                                       (or (and=> (assoc-ref outputs name)
                                                  derivation-output-path)
                                           value))))
                               env-vars)
                          #f)))))

  (define (user+system-env-vars)
    ;; Some options are passed to the build daemon via the env. vars of
    ;; derivations (urgh!).  We hide that from our API, but here is the place
    ;; where we kludgify those options.
    (let ((env-vars `(,@(if local-build?
                            `(("preferLocalBuild" . "1"))
                            '())
                      ,@(if allowed-references
                            `(("allowedReferences"
                               . ,(string-join allowed-references)))
                            '())
                      ,@env-vars)))
      (match references-graphs
        (((file . path) ...)
         (let ((value (map (cut string-append <> " " <>)
                           file path)))
           ;; XXX: This all breaks down if an element of FILE or PATH contains
           ;; white space.
           `(("exportReferencesGraph" . ,(string-join value " "))
             ,@env-vars)))
        (#f
         env-vars))))

  (define (env-vars-with-empty-outputs env-vars)
    ;; Return a variant of ENV-VARS where each OUTPUTS is associated with an
    ;; empty string, even outputs that do not appear in ENV-VARS.
    (let ((e (map (match-lambda
                   ((name . val)
                    (if (member name outputs)
                        (cons name "")
                        (cons name val))))
                  env-vars)))
      (fold (lambda (output-name env-vars)
              (if (assoc output-name env-vars)
                  env-vars
                  (append env-vars `((,output-name . "")))))
            e
            outputs)))

  (define (set-file-name drv file)
    ;; Set FILE as the 'file-name' field of DRV.
    (match drv
      (($ <derivation> outputs inputs sources system builder
          args env-vars)
       (make-derivation outputs inputs sources system builder
                        args env-vars file))))

  (let* ((outputs    (map (lambda (name)
                            ;; Return outputs with an empty path.
                            (cons name
                                  (make-derivation-output "" hash-algo
                                                          hash recursive?)))
                          outputs))
         (inputs     (map (match-lambda
                           (((? derivation? drv))
                            (make-derivation-input (derivation-file-name drv)
                                                   '("out")))
                           (((? derivation? drv) sub-drvs ...)
                            (make-derivation-input (derivation-file-name drv)
                                                   sub-drvs))
                           (((? direct-store-path? input))
                            (make-derivation-input input '("out")))
                           (((? direct-store-path? input) sub-drvs ...)
                            (make-derivation-input input sub-drvs))
                           ((input . _)
                            (let ((path (add-to-store store
                                                      (basename input)
                                                      #t "sha256" input)))
                              (make-derivation-input path '()))))
                          (delete-duplicates inputs)))
         (env-vars   (env-vars-with-empty-outputs (user+system-env-vars)))
         (drv-masked (make-derivation outputs
                                      (filter (compose derivation-path?
                                                       derivation-input-path)
                                              inputs)
                                      (filter-map (lambda (i)
                                                    (let ((p (derivation-input-path i)))
                                                      (and (not (derivation-path? p))
                                                           p)))
                                                  inputs)
                                      system builder args env-vars #f))
         (drv        (add-output-paths drv-masked)))

    (let ((file (add-text-to-store store (string-append name ".drv")
                                   (derivation->string drv)
                                   (map derivation-input-path
                                        inputs))))
      (set-file-name drv file))))

(define* (map-derivation store drv mapping
                         #:key (system (%current-system)))
  "Given MAPPING, a list of pairs of derivations, return a derivation based on
DRV where all the 'car's of MAPPING have been replaced by its 'cdr's,
recursively."
  (define (substitute str initial replacements)
    (fold (lambda (path replacement result)
            (string-replace-substring result path
                                      replacement))
          str
          initial replacements))

  (define (substitute-file file initial replacements)
    (define contents
      (with-fluids ((%default-port-encoding #f))
        (call-with-input-file file get-string-all)))

    (let ((updated (substitute contents initial replacements)))
      (if (string=? updated contents)
          file
          ;; XXX: permissions aren't preserved.
          (add-text-to-store store (store-path-package-name file)
                             updated))))

  (define input->output-paths
    (match-lambda
     (((? derivation? drv))
      (list (derivation->output-path drv)))
     (((? derivation? drv) sub-drvs ...)
      (map (cut derivation->output-path drv <>)
           sub-drvs))
     ((file)
      (list file))))

  (let ((mapping (fold (lambda (pair result)
                         (match pair
                           (((? derivation? orig) . replacement)
                            (vhash-cons (derivation-file-name orig)
                                        replacement result))
                           ((file . replacement)
                            (vhash-cons file replacement result))))
                       vlist-null
                       mapping)))
    (define rewritten-input
      ;; Rewrite the given input according to MAPPING, and return an input
      ;; in the format used in 'derivation' calls.
      (memoize
       (lambda (input loop)
         (match input
           (($ <derivation-input> path (sub-drvs ...))
            (match (vhash-assoc path mapping)
              ((_ . (? derivation? replacement))
               (cons replacement sub-drvs))
              ((_ . replacement)
               (list replacement))
              (#f
               (let* ((drv (loop (call-with-input-file path read-derivation))))
                 (cons drv sub-drvs)))))))))

    (let loop ((drv drv))
      (let* ((inputs       (map (cut rewritten-input <> loop)
                                (derivation-inputs drv)))
             (initial      (append-map derivation-input-output-paths
                                       (derivation-inputs drv)))
             (replacements (append-map input->output-paths inputs))

             ;; Sources typically refer to the output directories of the
             ;; original inputs, INITIAL.  Rewrite them by substituting
             ;; REPLACEMENTS.
             (sources      (map (lambda (source)
                                  (match (vhash-assoc source mapping)
                                    ((_ . replacement)
                                     replacement)
                                    (#f
                                     (substitute-file source
                                                      initial replacements))))
                                (derivation-sources drv)))

             ;; Now augment the lists of initials and replacements.
             (initial      (append (derivation-sources drv) initial))
             (replacements (append sources replacements))
             (name         (store-path-package-name
                            (string-drop-right (derivation-file-name drv)
                                               4))))
        (derivation store name
                    (substitute (derivation-builder drv)
                                initial replacements)
                    (map (cut substitute <> initial replacements)
                         (derivation-builder-arguments drv))
                    #:system system
                    #:env-vars (map (match-lambda
                                     ((var . value)
                                      `(,var
                                        . ,(substitute value initial
                                                       replacements))))
                                    (derivation-builder-environment-vars drv))
                    #:inputs (append (map list sources) inputs)
                    #:outputs (map car (derivation-outputs drv))
                    #:hash (match (derivation-outputs drv)
                             ((($ <derivation-output> _ algo hash))
                              hash)
                             (_ #f))
                    #:hash-algo (match (derivation-outputs drv)
                                  ((($ <derivation-output> _ algo hash))
                                   algo)
                                  (_ #f)))))))


;;;
;;; Store compatibility layer.
;;;

(define (build-derivations store derivations)
  "Build DERIVATIONS, a list of <derivation> objects or .drv file names."
  (let ((build (@ (guix store) build-derivations)))
    (build store (map (match-lambda
                       ((? string? file) file)
                       ((and drv ($ <derivation>))
                        (derivation-file-name drv)))
                      derivations))))


;;;
;;; Guile-based builders.
;;;

(define %guile-for-build
  ;; The derivation of the Guile to be used within the build environment,
  ;; when using `build-expression->derivation'.
  (make-parameter #f))

(define (parent-directories file-name)
  "Return the list of parent dirs of FILE-NAME, in the order in which an
`mkdir -p' implementation would make them."
  (let ((not-slash (char-set-complement (char-set #\/))))
    (reverse
     (fold (lambda (dir result)
             (match result
               (()
                (list dir))
               ((prev _ ...)
                (cons (string-append prev "/" dir)
                      result))))
           '()
           (remove (cut string=? <> ".")
                   (string-tokenize (dirname file-name) not-slash))))))

(define* (imported-files store files
                         #:key (name "file-import")
                         (system (%current-system))
                         (guile (%guile-for-build)))
  "Return a derivation that imports FILES into STORE.  FILES must be a list
of (FINAL-PATH . FILE-NAME) pairs; each FILE-NAME is read from the file
system, imported, and appears under FINAL-PATH in the resulting store path."
  (let* ((files   (map (match-lambda
                        ((final-path . file-name)
                         (list final-path
                               (add-to-store store (basename final-path) #f
                                             "sha256" file-name))))
                       files))
         (builder
          `(begin
             (mkdir %output) (chdir %output)
             ,@(append-map (match-lambda
                            ((final-path store-path)
                             (append (match (parent-directories final-path)
                                       (() '())
                                       ((head ... tail)
                                        (append (map (lambda (d)
                                                       `(false-if-exception
                                                         (mkdir ,d)))
                                                     head)
                                                `((or (file-exists? ,tail)
                                                      (mkdir ,tail))))))
                                     `((symlink ,store-path ,final-path)))))
                           files))))
    (build-expression->derivation store name builder
                                  #:system system
                                  #:inputs files
                                  #:guile-for-build guile
                                  #:local-build? #t)))

(define search-path*
  ;; A memoizing version of 'search-path' so 'imported-modules' does not end
  ;; up looking for the same files over and over again.
  (memoize search-path))

(define* (imported-modules store modules
                           #:key (name "module-import")
                           (system (%current-system))
                           (guile (%guile-for-build))
                           (module-path %load-path))
  "Return a derivation that contains the source files of MODULES, a list of
module names such as `(ice-9 q)'.  All of MODULES must be in the MODULE-PATH
search path."
  ;; TODO: Determine the closure of MODULES, build the `.go' files,
  ;; canonicalize the source files through read/write, etc.
  (let ((files (map (lambda (m)
                      (let ((f (string-append
                                (string-join (map symbol->string m) "/")
                                ".scm")))
                        (cons f (search-path* module-path f))))
                    modules)))
    (imported-files store files #:name name #:system system
                    #:guile guile)))

(define* (compiled-modules store modules
                           #:key (name "module-import-compiled")
                           (system (%current-system))
                           (guile (%guile-for-build))
                           (module-path %load-path))
  "Return a derivation that builds a tree containing the `.go' files
corresponding to MODULES.  All the MODULES are built in a context where
they can refer to each other."
  (let* ((module-drv (imported-modules store modules
                                       #:system system
                                       #:guile guile
                                       #:module-path module-path))
         (module-dir (derivation->output-path module-drv))
         (files      (map (lambda (m)
                            (let ((f (string-join (map symbol->string m)
                                                  "/")))
                              (cons (string-append f ".go")
                                    (string-append module-dir "/" f ".scm"))))
                      modules)))
    (define builder
      `(begin
         (use-modules (system base compile))
         (let ((out (assoc-ref %outputs "out")))
           (mkdir out)
           (chdir out))

         (set! %load-path
               (cons ,module-dir %load-path))

         ,@(map (match-lambda
                 ((output . input)
                  (let ((make-parent-dirs (map (lambda (dir)
                                                 `(unless (file-exists? ,dir)
                                                    (mkdir ,dir)))
                                               (parent-directories output))))
                   `(begin
                      ,@make-parent-dirs
                      (compile-file ,input
                                    #:output-file ,output
                                    #:opts %auto-compilation-options)))))
                files)))

    (build-expression->derivation store name builder
                                  #:inputs `(("modules" ,module-drv))
                                  #:system system
                                  #:guile-for-build guile
                                  #:local-build? #t)))

(define-record-type* <graft> graft make-graft
  graft?
  (origin             graft-origin)               ;derivation | store item
  (origin-output      graft-origin-output         ;string | #f
                      (default "out"))
  (replacement        graft-replacement)          ;derivation | store item
  (replacement-output graft-replacement-output    ;string | #f
                      (default "out")))

(define* (graft-derivation store name drv grafts
                           #:key (guile (%guile-for-build))
                           (system (%current-system)))
  "Return a derivation called NAME, based on DRV but with all the GRAFTS
applied."
  ;; XXX: Someday rewrite using gexps.
  (define mapping
    ;; List of store item pairs.
    (map (match-lambda
          (($ <graft> source source-output target target-output)
           (cons (if (derivation? source)
                     (derivation->output-path source source-output)
                     source)
                 (if (derivation? target)
                     (derivation->output-path target target-output)
                     target))))
         grafts))

  (define outputs
    (match (derivation-outputs drv)
      (((names . outputs) ...)
       (map derivation-output-path outputs))))

  (define output-names
    (match (derivation-outputs drv)
      (((names . outputs) ...)
       names)))

  (define build
    `(begin
       (use-modules (guix build graft)
                    (guix build utils)
                    (ice-9 match))

       (let ((mapping ',mapping))
         (for-each (lambda (input output)
                     (format #t "rewriting '~a' to '~a'...~%" input output)
                     (rewrite-directory input output
                                        `((,input . ,output)
                                          ,@mapping)))
                   ',outputs
                   (match %outputs
                     (((names . files) ...)
                      files))))))

  (define add-label
    (cut cons "x" <>))

  (match grafts
    ((($ <graft> sources source-outputs targets target-outputs) ...)
     (let ((sources (zip sources source-outputs))
           (targets (zip targets target-outputs)))
       (build-expression->derivation store name build
                                     #:system system
                                     #:guile-for-build guile
                                     #:modules '((guix build graft)
                                                 (guix build utils))
                                     #:inputs `(,@(map (lambda (out)
                                                         `("x" ,drv ,out))
                                                       output-names)
                                                ,@(append (map add-label sources)
                                                          (map add-label targets)))
                                     #:outputs output-names
                                     #:local-build? #t)))))

(define* (build-expression->derivation store name exp
                                       #:key
                                       (system (%current-system))
                                       (inputs '())
                                       (outputs '("out"))
                                       hash hash-algo recursive?
                                       (env-vars '())
                                       (modules '())
                                       guile-for-build
                                       references-graphs
                                       allowed-references
                                       local-build?)
  "Return a derivation that executes Scheme expression EXP as a builder
for derivation NAME.  INPUTS must be a list of (NAME DRV-PATH SUB-DRV)
tuples; when SUB-DRV is omitted, \"out\" is assumed.  MODULES is a list
of names of Guile modules from the current search path to be copied in
the store, compiled, and made available in the load path during the
execution of EXP.

EXP is evaluated in an environment where %OUTPUT is bound to the main
output path, %OUTPUTS is bound to a list of output/path pairs, and where
%BUILD-INPUTS is bound to an alist of string/output-path pairs made from
INPUTS.  Optionally, ENV-VARS is a list of string pairs specifying the
name and value of environment variables visible to the builder.  The
builder terminates by passing the result of EXP to `exit'; thus, when
EXP returns #f, the build is considered to have failed.

EXP is built using GUILE-FOR-BUILD (a derivation).  When GUILE-FOR-BUILD is
omitted or is #f, the value of the `%guile-for-build' fluid is used instead.

See the `derivation' procedure for the meaning of REFERENCES-GRAPHS,
ALLOWED-REFERENCES, and LOCAL-BUILD?."
  (define guile-drv
    (or guile-for-build (%guile-for-build)))

  (define guile
    (string-append (derivation->output-path guile-drv)
                   "/bin/guile"))

  (define module-form?
    (match-lambda
     (((or 'define-module 'use-modules) _ ...) #t)
     (_ #f)))

  (define source-path
    ;; When passed an input that is a source, return its path; otherwise
    ;; return #f.
    (match-lambda
     ((_ (? derivation?) _ ...)
      #f)
     ((_ path _ ...)
      (and (not (derivation-path? path))
           path))))

  (let* ((prologue `(begin
                      ,@(match exp
                          ((_ ...)
                           ;; Module forms must appear at the top-level so
                           ;; that any macros they export can be expanded.
                           (filter module-form? exp))
                          (_ `(,exp)))

                      (define %output (getenv "out"))
                      (define %outputs
                        (map (lambda (o)
                               (cons o (getenv o)))
                             ',outputs))
                      (define %build-inputs
                        ',(map (match-lambda
                                ((name drv . rest)
                                 (let ((sub (match rest
                                              (() "out")
                                              ((x) x))))
                                   (cons name
                                         (cond
                                          ((derivation? drv)
                                           (derivation->output-path drv sub))
                                          ((derivation-path? drv)
                                           (derivation-path->output-path drv
                                                                         sub))
                                          (else drv))))))
                               inputs))

                      ,@(if (null? modules)
                            '()
                            ;; Remove our own settings.
                            '((unsetenv "GUILE_LOAD_COMPILED_PATH")))

                      ;; Guile sets it, but remove it to avoid conflicts when
                      ;; building Guile-using packages.
                      (unsetenv "LD_LIBRARY_PATH")))
         (builder  (add-text-to-store store
                                      (string-append name "-guile-builder")

                                      ;; Explicitly use UTF-8 for determinism,
                                      ;; and also because UTF-8 output is faster.
                                      (with-fluids ((%default-port-encoding
                                                     "UTF-8"))
                                        (call-with-output-string
                                         (lambda (port)
                                           (write prologue port)
                                           (write
                                            `(exit
                                              ,(match exp
                                                 ((_ ...)
                                                  (remove module-form? exp))
                                                 (_ `(,exp))))
                                            port))))

                                      ;; The references don't really matter
                                      ;; since the builder is always used in
                                      ;; conjunction with the drv that needs
                                      ;; it.  For clarity, we add references
                                      ;; to the subset of INPUTS that are
                                      ;; sources, avoiding references to other
                                      ;; .drv; otherwise, BUILDER's hash would
                                      ;; depend on those, even if they are
                                      ;; fixed-output.
                                      (filter-map source-path inputs)))

         (mod-drv  (and (pair? modules)
                        (imported-modules store modules
                                          #:guile guile-drv
                                          #:system system)))
         (mod-dir  (and mod-drv
                        (derivation->output-path mod-drv)))
         (go-drv   (and (pair? modules)
                        (compiled-modules store modules
                                          #:guile guile-drv
                                          #:system system)))
         (go-dir   (and go-drv
                        (derivation->output-path go-drv))))
    (derivation store name guile
                `("--no-auto-compile"
                  ,@(if mod-dir `("-L" ,mod-dir) '())
                  ,builder)

                #:system system

                #:inputs `((,(or guile-for-build (%guile-for-build)))
                           (,builder)
                           ,@(map cdr inputs)
                           ,@(if mod-drv `((,mod-drv) (,go-drv)) '()))

                ;; When MODULES is non-empty, shamelessly clobber
                ;; $GUILE_LOAD_COMPILED_PATH.
                #:env-vars (if go-dir
                               `(("GUILE_LOAD_COMPILED_PATH" . ,go-dir)
                                 ,@(alist-delete "GUILE_LOAD_COMPILED_PATH"
                                                 env-vars))
                               env-vars)

                #:hash hash #:hash-algo hash-algo
                #:recursive? recursive?
                #:outputs outputs
                #:references-graphs references-graphs
                #:allowed-references allowed-references
                #:local-build? local-build?)))
