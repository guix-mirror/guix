;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
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
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 binary-ports)
  #:use-module ((ice-9 textual-ports) #:select (put-char put-string))
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 vlist)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix base16)
  #:use-module (guix memoization)
  #:use-module (guix combinators)
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix monads)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix records)
  #:use-module (guix sets)
  #:export (<derivation>
            derivation?
            derivation-outputs
            derivation-inputs
            derivation-sources
            derivation-system
            derivation-builder
            derivation-builder-arguments
            derivation-builder-environment-vars
            derivation-file-name
            derivation-prerequisites
            derivation-build-plan
            derivation-prerequisites-to-build     ;deprecated

            <derivation-output>
            derivation-output?
            derivation-output-path
            derivation-output-hash-algo
            derivation-output-hash
            derivation-output-recursive?

            <derivation-input>
            derivation-input?
            derivation-input
            derivation-input-path
            derivation-input-derivation
            derivation-input-sub-derivations
            derivation-input-output-paths
            derivation-input-output-path
            valid-derivation-input?

            &derivation-error
            derivation-error?
            derivation-error-derivation
            &derivation-missing-output-error
            derivation-missing-output-error?
            derivation-missing-output

            derivation-name
            derivation-output-names
            fixed-output-derivation?
            offloadable-derivation?
            substitutable-derivation?
            derivation-input-fold
            substitution-oracle
            derivation-hash
            derivation-properties

            read-derivation
            read-derivation-from-file
            write-derivation
            derivation->output-path
            derivation->output-paths
            derivation-path->output-path
            derivation-path->output-paths
            derivation
            raw-derivation
            invalidate-derivation-caches!

            map-derivation

            build-derivations
            built-derivations

            file-search-error?
            file-search-error-file-name
            file-search-error-search-path

            search-path*
            module->source-file-name
            build-expression->derivation)

  ;; Re-export it from here for backward compatibility.
  #:re-export (%guile-for-build))

;;;
;;; Error conditions.
;;;

(define-condition-type &derivation-error &store-error
  derivation-error?
  (derivation derivation-error-derivation))

(define-condition-type &derivation-missing-output-error &derivation-error
  derivation-missing-output-error?
  (output derivation-missing-output))

;;;
;;; Nix derivations, as implemented in Nix's `derivations.cc'.
;;;

(define-immutable-record-type <derivation>
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

(define-immutable-record-type <derivation-output>
  (make-derivation-output path hash-algo hash recursive?)
  derivation-output?
  (path       derivation-output-path)             ; store path
  (hash-algo  derivation-output-hash-algo)        ; symbol | #f
  (hash       derivation-output-hash)             ; bytevector | #f
  (recursive? derivation-output-recursive?))      ; Boolean

(define-immutable-record-type <derivation-input>
  (make-derivation-input drv sub-derivations)
  derivation-input?
  (drv             derivation-input-derivation)       ; <derivation>
  (sub-derivations derivation-input-sub-derivations)) ; list of strings


(define (derivation-input-path input)
  "Return the file name of the derivation INPUT refers to."
  (derivation-file-name (derivation-input-derivation input)))

(define* (derivation-input drv #:optional
                           (outputs (derivation-output-names drv)))
  "Return a <derivation-input> for the OUTPUTS of DRV."
  ;; This is a public interface meant to be more convenient than
  ;; 'make-derivation-input' and giving us more control.
  (make-derivation-input drv outputs))

(define (derivation-input-key input)
  "Return an object for which 'equal?' and 'hash' are constant-time, and which
can thus be used as a key for INPUT in lookup tables."
  (cons (derivation-input-path input)
        (derivation-input-sub-derivations input)))

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

(define (derivation-name drv)
  "Return the base name of DRV."
  (let ((base (store-path-package-name (derivation-file-name drv))))
    (string-drop-right base 4)))

(define (derivation-output-names drv)
  "Return the names of the outputs of DRV."
  (match (derivation-outputs drv)
    (((names . _) ...)
     names)))

(define (fixed-output-derivation? drv)
  "Return #t if DRV is a fixed-output derivation, such as the result of a
download with a fixed hash (aka. `fetchurl')."
  (match drv
    (($ <derivation>
        (("out" . ($ <derivation-output> _ (? symbol?) (? bytevector?)))))
     #t)
    (_ #f)))

(define (derivation-input<? input1 input2)
  "Compare INPUT1 and INPUT2, two <derivation-input>."
  (string<? (derivation-input-path input1)
            (derivation-input-path input2)))

(define (derivation-input-output-paths input)
  "Return the list of output paths corresponding to INPUT, a
<derivation-input>."
  (match input
    (($ <derivation-input> drv sub-drvs)
     (map (cut derivation->output-path drv <>)
          sub-drvs))))

(define (derivation-input-output-path input)
  "Return the output file name of INPUT.  If INPUT has more than one outputs,
an error is raised."
  (match input
    (($ <derivation-input> drv (output))
     (derivation->output-path drv output))))

(define (valid-derivation-input? store input)
  "Return true if INPUT is valid--i.e., if all the outputs it requests are in
the store."
  (every (cut valid-path? store <>)
         (derivation-input-output-paths input)))

(define (coalesce-duplicate-inputs inputs)
  "Return a list of inputs, such that when INPUTS contains the same DRV twice,
they are coalesced, with their sub-derivations merged.  This is needed because
Nix itself keeps only one of them."
  (define table
    (make-hash-table 25))

  (for-each (lambda (input)
              (let* ((drv (derivation-input-path input))
                     (sub-drvs (derivation-input-sub-derivations input)))
                (match (hash-get-handle table drv)
                  (#f
                   (hash-set! table drv input))
                  ((and handle (key . ($ <derivation-input> drv sub-drvs2)))
                   ;; Merge DUP with INPUT.
                   (let* ((sub-drvs (delete-duplicates
                                     (append sub-drvs sub-drvs2)))
                          (input
                           (make-derivation-input drv
                                                  (sort sub-drvs string<?))))
                     (set-cdr! handle input))))))
            inputs)

  (hash-fold (lambda (key input lst)
               (cons input lst))
             '()
             table))

(define* (derivation-prerequisites drv #:optional (cut? (const #f)))
  "Return the list of derivation-inputs required to build DRV, recursively.

CUT? is a predicate that is passed a derivation-input and returns true to
eliminate the given input and its dependencies from the search.  An example of
such a predicate is 'valid-derivation-input?'; when it is used as CUT?, the
result is the set of prerequisites of DRV not already in valid."
  (let loop ((drv       drv)
             (result    '())
             (input-set (set)))
    (let ((inputs (remove (lambda (input)
                            (or (set-contains? input-set
                                               (derivation-input-key input))
                                (cut? input)))
                          (derivation-inputs drv))))
      (fold2 loop
             (append inputs result)
             (fold set-insert input-set
                   (map derivation-input-key inputs))
             (map derivation-input-derivation inputs)))))

(define (offloadable-derivation? drv)
  "Return true if DRV can be offloaded, false otherwise."
  (match (assoc "preferLocalBuild"
                (derivation-builder-environment-vars drv))
    (("preferLocalBuild" . "1") #f)
    (_ #t)))

(define (substitutable-derivation? drv)
  "Return #t if DRV can be substituted."
  (match (assoc "allowSubstitutes"
                (derivation-builder-environment-vars drv))
    (("allowSubstitutes" . value)
     (string=? value "1"))
    (_ #t)))

(define (derivation-output-paths drv sub-drvs)
  "Return the output paths of outputs SUB-DRVS of DRV."
  (match drv
    (($ <derivation> outputs)
     (map (lambda (sub-drv)
            (derivation-output-path (assoc-ref outputs sub-drv)))
          sub-drvs))))

(define* (derivation-input-fold proc seed inputs
                                #:key (cut? (const #f)))
  "Perform a breadth-first traversal of INPUTS, calling PROC on each input
with the current result, starting from SEED.  Skip recursion on inputs that
match CUT?."
  (let loop ((inputs inputs)
             (result seed)
             (visited (set)))
    (match inputs
      (()
       result)
      ((input rest ...)
       (let ((key (derivation-input-key input)))
         (cond ((set-contains? visited key)
                (loop rest result visited))
               ((cut? input)
                (loop rest result (set-insert key visited)))
               (else
                (let ((drv (derivation-input-derivation input)))
                  (loop (append (derivation-inputs drv) rest)
                        (proc input result)
                        (set-insert key visited))))))))))

(define* (substitution-oracle store inputs-or-drv
                              #:key (mode (build-mode normal)))
  "Return a one-argument procedure that, when passed a store file name,
returns a 'substitutable?' if it's substitutable and #f otherwise.

The returned procedure knows about all substitutes for all the derivation
inputs or derivations listed in INPUTS-OR-DRV, *except* those that are already
valid (that is, it won't bother checking whether an item is substitutable if
it's already on disk); it also knows about their prerequisites, unless they
are themselves substitutable.

Creating a single oracle (thus making a single 'substitutable-path-info' call) and
reusing it is much more efficient than calling 'has-substitutes?' or similar
repeatedly, because it avoids the costs associated with launching the
substituter many times."
  (define valid-input?
    (cut valid-derivation-input? store <>))

  (define (closure inputs)
    (reverse
     (derivation-input-fold (lambda (input closure)
                              (let ((drv (derivation-input-derivation input)))
                                (if (substitutable-derivation? drv)
                                    (cons input closure)
                                    closure)))
                            '()
                            inputs
                            #:cut? valid-input?)))

  (let* ((inputs (closure (map (match-lambda
                                 ((? derivation-input? input)
                                  input)
                                 ((? derivation? drv)
                                  (derivation-input drv)))
                               inputs-or-drv)))
         (items  (append-map derivation-input-output-paths inputs))
         (subst  (fold (lambda (subst vhash)
                         (vhash-cons (substitutable-path subst) subst
                                     vhash))
                       vlist-null
                       (substitutable-path-info store items))))
    (lambda (item)
      (match (vhash-assoc item subst)
        (#f #f)
        ((key . value) value)))))

(define (dependencies-of-substitutables substitutables inputs)
  "Return the subset of INPUTS whose output file names is among the references
of SUBSTITUTABLES."
  (let ((items (fold set-insert (set)
                     (append-map substitutable-references substitutables))))
    (filter (lambda (input)
              (any (cut set-contains? items <>)
                   (derivation-input-output-paths input)))
            inputs)))

(define* (derivation-build-plan store inputs
                                #:key
                                (mode (build-mode normal))
                                (substitutable-info
                                 (substitution-oracle
                                  store inputs #:mode mode)))
  "Given INPUTS, a list of derivation-inputs, return two values: the list of
derivations to build, and the list of substitutable items that, together,
allow INPUTS to be realized.

SUBSTITUTABLE-INFO must be a one-argument procedure similar to that returned
by 'substitution-oracle'."
  (define (built? item)
    (valid-path? store item))

  (define (input-built? input)
    ;; In 'check' mode, assume that DRV is not built.
    (and (not (and (eqv? mode (build-mode check))
                   (member input inputs)))
         (every built? (derivation-input-output-paths input))))

  (define (input-substitutable-info input)
    (and (substitutable-derivation? (derivation-input-derivation input))
         (let* ((items (derivation-input-output-paths input))
                (info  (filter-map substitutable-info items)))
           (and (= (length info) (length items))
                info))))

  (let loop ((inputs     inputs)                  ;list of <derivation-input>
             (build      '())                     ;list of <derivation>
             (substitute '())                     ;list of <substitutable>
             (visited    (set)))                  ;set of <derivation-input>
    (match inputs
      (()
       (values build substitute))
      ((input rest ...)
       (let ((key  (derivation-input-key input))
             (deps (derivation-inputs
                    (derivation-input-derivation input))))
         (cond ((set-contains? visited key)
                (loop rest build substitute visited))
               ((input-built? input)
                (loop rest build substitute
                      (set-insert key visited)))
               ((input-substitutable-info input)
                =>
                (lambda (substitutables)
                  (loop (append (dependencies-of-substitutables substitutables
                                                                deps)
                                rest)
                        build
                        (append substitutables substitute)
                        (set-insert key visited))))
               (else
                (loop (append deps rest)
                      (cons (derivation-input-derivation input) build)
                      substitute
                      (set-insert key visited)))))))))

(define-deprecated (derivation-prerequisites-to-build store drv #:rest rest)
  derivation-build-plan
  (let-values (((build download)
                (apply derivation-build-plan store
                       (list (derivation-input drv)) rest)))
    (values (map derivation-input build) download)))

(define* (read-derivation drv-port
                          #:optional (read-derivation-from-file
                                      read-derivation-from-file))
  "Read the derivation from DRV-PORT and return the corresponding <derivation>
object.  Call READ-DERIVATION-FROM-FILE to read derivations declared as inputs
of the derivation being parsed.

Most of the time you'll want to use 'read-derivation-from-file', which caches
things as appropriate and is thus more efficient."

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
                     (let ((drv (read-derivation-from-file path)))
                       (cons (make-derivation-input drv sub-drvs)
                             result)))))
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

(define %derivation-cache
  ;; Maps derivation file names to <derivation> objects.
  ;; XXX: This is redundant with 'atts-cache' in the store.
  (make-weak-value-hash-table 200))

(define (read-derivation-from-file file)
  "Read the derivation in FILE, a '.drv' file, and return the corresponding
<derivation> object."
  ;; Memoize that operation because 'read-derivation' is quite expensive,
  ;; and because the same argument is read more than 15 times on average
  ;; during something like (package-derivation s gdb).
  (or (and file (hash-ref %derivation-cache file))
      (let ((drv (call-with-input-file file read-derivation)))
        (hash-set! %derivation-cache file drv)
        drv)))

(define-inlinable (write-sequence lst write-item port)
  ;; Write each element of LST with WRITE-ITEM to PORT, separating them with a
  ;; comma.
  (match lst
    (()
     #t)
    ((prefix (... ...) last)
     (for-each (lambda (item)
                 (write-item item port)
                 (put-char port #\,))
               prefix)
     (write-item last port))))

(define-inlinable (write-list lst write-item port)
  ;; Write LST as a derivation list to PORT, using WRITE-ITEM to write each
  ;; element.
  (put-char port #\[)
  (write-sequence lst write-item port)
  (put-char port #\]))

(define-inlinable (write-tuple lst write-item port)
  ;; Same, but write LST as a tuple.
  (put-char port #\()
  (write-sequence lst write-item port)
  (put-char port #\)))

(define %escape-char-set
  ;; Characters that need to be escaped.
  (char-set #\" #\\ #\newline #\return #\tab))

(define (escaped-string str)
  "Escape double quote characters found in STR, if any."
  (define escape
    (match-lambda
      (#\"       "\\\"")
      (#\\       "\\\\")
      (#\newline "\\n")
      (#\return  "\\r")
      (#\tab     "\\t")))

  (let loop ((str   str)
             (result '()))
    (let ((index (string-index str %escape-char-set)))
      (if index
          (let ((rest (string-drop str (+ 1 index))))
            (loop rest
                  (cons* (escape (string-ref str index))
                         (string-take str index)
                         result)))
          (if (null? result)
              str
              (string-concatenate-reverse (cons str result)))))))

(define (write-derivation drv port)
  "Write the ATerm-like serialization of DRV to PORT.  See Section 2.4 of
Eelco Dolstra's PhD dissertation for an overview of a previous version of
that form."

  ;; Use 'put-string', which does less work and is faster than 'display'.
  ;; Likewise, 'write-escaped-string' is faster than 'write'.

  (define (write-escaped-string str port)
    (put-char port #\")
    (put-string port (escaped-string str))
    (put-char port #\"))

  (define (write-string-list lst)
    (write-list lst write-escaped-string port))

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
                   write-escaped-string
                   port))))

  (define (write-input input port)
    (match input
      (($ <derivation-input> obj sub-drvs)
       (put-string port "(\"")

       ;; 'derivation/masked-inputs' produces objects that contain a string
       ;; instead of a <derivation>, so we need to account for that.
       (put-string port (if (derivation? obj)
                            (derivation-file-name obj)
                            obj))
       (put-string port "\",")
       (write-string-list sub-drvs)
       (put-char port #\)))))

  (define (write-env-var env-var port)
    (match env-var
      ((name . value)
       (put-char port #\()
       (write-escaped-string name port)
       (put-char port #\,)
       (write-escaped-string value port)
       (put-char port #\)))))

  ;; Assume all the lists we are writing are already sorted.
  (match drv
    (($ <derivation> outputs inputs sources
        system builder args env-vars)
     (put-string port "Derive(")
     (write-list outputs write-output port)
     (put-char port #\,)
     (write-list inputs write-input port)
     (put-char port #\,)
     (write-string-list sources)
     (simple-format port ",\"~a\",\"~a\"," system builder)
     (write-string-list args)
     (put-char port #\,)
     (write-list env-vars write-env-var port)
     (put-char port #\)))))

(define derivation->bytevector
  (lambda (drv)
    "Return the external representation of DRV as a UTF-8-encoded string."
    (with-fluids ((%default-port-encoding "UTF-8"))
      (call-with-values open-bytevector-output-port
        (lambda (port get-bytevector)
          (write-derivation drv port)
          (get-bytevector))))))

(define* (derivation->output-path drv #:optional (output "out"))
  "Return the store path of its output OUTPUT.  Raise a
'&derivation-missing-output-error' condition if OUTPUT is not an output of
DRV."
  (let ((output* (assoc-ref (derivation-outputs drv) output)))
    (if output*
        (derivation-output-path output*)
        (raise (condition (&derivation-missing-output-error
                           (derivation drv)
                           (output output)))))))

(define (derivation->output-paths drv)
  "Return the list of name/path pairs of the outputs of DRV."
  (map (match-lambda
        ((name . output)
         (cons name (derivation-output-path output))))
       (derivation-outputs drv)))

(define derivation-path->output-path
  ;; This procedure is called frequently, so memoize it.
  (let ((memoized (mlambda (path output)
                    (derivation->output-path (read-derivation-from-file path)
                                             output))))
    (lambda* (path #:optional (output "out"))
      "Read the derivation from PATH (`/gnu/store/xxx.drv'), and return the store
path of its output OUTPUT."
      (memoized path output))))

(define (derivation-path->output-paths path)
  "Read the derivation from PATH (`/gnu/store/xxx.drv'), and return the
list of name/path pairs of its outputs."
  (derivation->output-paths (read-derivation-from-file path)))


;;;
;;; Derivation primitive.
;;;

(define derivation-base16-hash
  (mlambdaq (drv)
    "Return a string containing the base16 representation of the hash of DRV."
    (bytevector->base16-string (derivation-hash drv))))

(define (derivation/masked-inputs drv)
  "Assuming DRV is a regular derivation (not fixed-output), replace the file
name of each input with that input's hash."
  (match drv
    (($ <derivation> outputs inputs sources
                     system builder args env-vars)
     (let ((inputs (map (match-lambda
                          (($ <derivation-input> drv sub-drvs)
                           (let ((hash (derivation-base16-hash drv)))
                             (make-derivation-input hash sub-drvs))))
                        inputs)))
       (make-derivation outputs
                        (sort (delete-duplicates inputs)
                              (lambda (drv1 drv2)
                                (string<? (derivation-input-derivation drv1)
                                          (derivation-input-derivation drv2))))
                        sources
                        system builder args env-vars
                        #f)))))

(define derivation-hash            ; `hashDerivationModulo' in derivations.cc
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
      (_

       ;; XXX: At this point this remains faster than `port-sha256', because
       ;; the SHA256 port's `write' method gets called for every single
       ;; character.
       (sha256 (derivation->bytevector (derivation/masked-inputs drv)))))))


(define (warn-about-derivation-deprecation name)
  ;; TRANSLATORS: 'derivation' must not be translated; it refers to the
  ;; 'derivation' procedure.
  (warning (G_ "in '~a': deprecated 'derivation' calling convention used~%")
           name))

(define* (derivation store name builder args
                     #:key
                     (system (%current-system)) (env-vars '())
                     (inputs '()) (sources '())
                     (outputs '("out"))
                     hash hash-algo recursive?
                     references-graphs
                     allowed-references disallowed-references
                     leaked-env-vars local-build?
                     (substitutable? #t)
                     (properties '())
                     (%deprecation-warning? #t))
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
that the derivation's outputs may refer to.  Likewise, DISALLOWED-REFERENCES,
if true, must be a list of things the outputs may not refer to.

When LEAKED-ENV-VARS is true, it must be a list of strings denoting
environment variables that are allowed to \"leak\" from the daemon's
environment to the build environment.  This is only applicable to fixed-output
derivations--i.e., when HASH is true.  The main use is to allow variables such
as \"http_proxy\" to be passed to derivations that download files.

When LOCAL-BUILD? is true, declare that the derivation is not a good candidate
for offloading and should rather be built locally.  This is the case for small
derivations where the costs of data transfers would outweigh the benefits.

When SUBSTITUTABLE? is false, declare that substitutes of the derivation's
output should not be used.

PROPERTIES must be an association list describing \"properties\" of the
derivation.  It is kept as-is, uninterpreted, in the derivation."
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
                               (let ((path
                                      (if hash
                                          (fixed-output-path name hash
                                                             #:hash-algo algo
                                                             #:output output-name
                                                             #:recursive? rec?)
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
                      ,@(if (not substitutable?)
                            `(("allowSubstitutes" . "0"))
                            '())
                      ,@(if allowed-references
                            `(("allowedReferences"
                               . ,(string-join allowed-references)))
                            '())
                      ,@(if disallowed-references
                            `(("disallowedReferences"
                               . ,(string-join disallowed-references)))
                            '())
                      ,@(if leaked-env-vars
                            `(("impureEnvVars"
                               . ,(string-join leaked-env-vars)))
                            '())
                      ,@(match properties
                          (() '())
                          (lst `(("guix properties"
                                  . ,(object->string properties)))))
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

  (define-syntax-rule (warn-deprecation name)
    (when %deprecation-warning?
      (warn-about-derivation-deprecation name)))

  (define input->derivation-input
    (match-lambda
      ((? derivation-input? input)
       input)
      (((? derivation? drv))
       (warn-deprecation name)
       (make-derivation-input drv '("out")))
      (((? derivation? drv) sub-drvs ...)
       (warn-deprecation name)
       (make-derivation-input drv sub-drvs))
      (_
       (warn-deprecation name)
       #f)))

  (define input->source
    (match-lambda
      (((? string? input) . _)
       (warn-deprecation name)
       (if (direct-store-path? input)
           input
           (add-to-store store (basename input)
                         #t "sha256" input)))
      (_ #f)))

  ;; Note: lists are sorted alphabetically, to conform with the behavior of
  ;; C++ `std::map' in Nix itself.

  (let* ((outputs    (map (lambda (name)
                            ;; Return outputs with an empty path.
                            (cons name
                                  (make-derivation-output "" hash-algo
                                                          hash recursive?)))
                          (sort outputs string<?)))
         (sources    (sort (delete-duplicates
                            (append (filter-map input->source inputs)
                                    sources))
                           string<?))
         (inputs     (sort (coalesce-duplicate-inputs
                            (filter-map input->derivation-input inputs))
                           derivation-input<?))
         (env-vars   (sort (env-vars-with-empty-outputs
                            (user+system-env-vars))
                           (lambda (e1 e2)
                             (string<? (car e1) (car e2)))))
         (drv-masked (make-derivation outputs inputs sources
                                      system builder args env-vars #f))
         (drv        (add-output-paths drv-masked)))

    (let* ((file (add-data-to-store store (string-append name ".drv")
                                    (derivation->bytevector drv)
                                    (append (map derivation-input-path inputs)
                                            sources)))
           (drv* (set-field drv (derivation-file-name) file)))
      ;; Preserve pointer equality.  This improves the performance of
      ;; 'eq?'-memoization on derivations.
      (or (hash-ref %derivation-cache file)
          (begin
            (hash-set! %derivation-cache file drv*)
            drv*)))))

(define (invalidate-derivation-caches!)
  "Invalidate internal derivation caches.  This is mostly useful for
long-running processes that know what they're doing.  Use with care!"
  ;; Typically this is meant to be used by Cuirass and Hydra, which can clear
  ;; caches when they start evaluating packages for another architecture.
  (invalidate-memoization! derivation-base16-hash)

  ;; FIXME: Comment out to work around <https://bugs.gnu.org/36487>.
  ;; (hash-clear! %derivation-cache)
  )

(define derivation-properties
  (mlambdaq (drv)
    "Return the property alist associated with DRV."
    (match (assoc "guix properties"
                  (derivation-builder-environment-vars drv))
      ((_ . str) (call-with-input-string str read))
      (#f        '()))))

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
        (call-with-input-file file read-string)))

    (let ((updated (substitute contents initial replacements)))
      (if (string=? updated contents)
          file
          ;; XXX: permissions aren't preserved.
          (add-text-to-store store (store-path-package-name file)
                             updated))))

  (define input->output-paths
    (match-lambda
      ((? derivation-input? input)
       (derivation-input-output-paths input))
      ((? string? file)
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
      (mlambda (input loop)
        (match input
          (($ <derivation-input> drv (sub-drvs ...))
           (match (vhash-assoc (derivation-file-name drv) mapping)
             ((_ . (? derivation? replacement))
              (derivation-input replacement sub-drvs))
             ((_ . (? string? source))
              source)
             (#f
              (derivation-input (loop drv) sub-drvs)))))))

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
                    #:inputs (filter derivation-input? inputs)
                    #:sources (append sources (filter string? inputs))
                    #:outputs (derivation-output-names drv)
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

(define* (build-derivations store derivations
                            #:optional (mode (build-mode normal)))
  "Build DERIVATIONS, a list of <derivation> or <derivation-input> objects,
.drv file names, or derivation/output pairs, using the specified MODE."
  (build-things store (map (match-lambda
                            ((? derivation? drv)
                             (derivation-file-name drv))
                            ((? derivation-input? input)
                             (cons (derivation-input-path input)
                                   (string-join
                                    (derivation-input-sub-derivations input)
                                    ",")))
                            ((? string? file) file)
                            (((? derivation? drv) . output)
                             (cons (derivation-file-name drv)
                                   output))
                            (((? string? file) . output)
                             (cons file output)))
                           derivations)
                mode))


;;;
;;; Guile-based builders.
;;;

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

(define* (imported-files store files              ;deprecated
                         #:key (name "file-import"))
  "Return a store item that contains FILES.  FILES must be a list
of (FINAL-PATH . FILE-NAME) pairs; each FILE-NAME is read from the file
system, imported, and appears under FINAL-PATH in the resulting store path."
  (add-file-tree-to-store store
                          `(,name directory
                                  ,@(file-mapping->tree files))))

;; The "file not found" error condition.
(define-condition-type &file-search-error &error
  file-search-error?
  (file     file-search-error-file-name)
  (path     file-search-error-search-path))

(define search-path*
  ;; A memoizing version of 'search-path' so 'imported-modules' does not end
  ;; up looking for the same files over and over again.
  (mlambda (path file)
    "Search for FILE in PATH and memoize the result.  Raise a
'&file-search-error' condition if it could not be found."
    (or (search-path path file)
        (raise (condition
                (&file-search-error (file file)
                                    (path path)))))))

(define (module->source-file-name module)
  "Return the file name corresponding to MODULE, a Guile module name (a list
of symbols.)"
  (string-append (string-join (map symbol->string module) "/")
                 ".scm"))

(define* (%imported-modules store modules         ;deprecated
                            #:key (name "module-import")
                            (module-path %load-path))
  "Return a store item that contains the source files of MODULES, a list of
module names such as `(ice-9 q)'.  All of MODULES must be in the MODULE-PATH
search path."
  ;; TODO: Determine the closure of MODULES, build the `.go' files,
  ;; canonicalize the source files through read/write, etc.
  (let ((files (map (lambda (m)
                      (let ((f (module->source-file-name m)))
                        (cons f (search-path* module-path f))))
                    modules)))
    (imported-files store files #:name name)))

(define* (%compiled-modules store modules         ;deprecated
                            #:key (name "module-import-compiled")
                            (system (%current-system))
                            (guile (%guile-for-build))
                            (module-path %load-path))
  "Return a derivation that builds a tree containing the `.go' files
corresponding to MODULES.  All the MODULES are built in a context where
they can refer to each other."
  (let* ((module-dir (%imported-modules store modules
                                        #:module-path module-path))
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
                                  #:inputs `(("modules" ,module-dir))
                                  #:system system
                                  #:guile-for-build guile
                                  #:local-build? #t)))

(define %module-cache
  ;; Map a list of modules to its 'imported+compiled-modules' result.
  (make-hash-table))

(define* (imported+compiled-modules store modules #:key
                                    (system (%current-system))
                                    (guile (%guile-for-build)))
  "Return a pair containing the derivation to import MODULES and that where
MODULES are compiled."
  (define key
    (list modules (derivation-file-name guile) system))

  (or (hash-ref %module-cache key)
      (let ((result (cons (%imported-modules store modules)
                          (%compiled-modules store modules
                                             #:system system #:guile guile))))
        (hash-set! %module-cache key result)
        result)))

(define-deprecated (build-expression->derivation store name exp
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
                                                 disallowed-references
                                                 local-build? (substitutable? #t)
                                                 (properties '()))
  gexp->derivation                                ;unbound, but that's okay
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
ALLOWED-REFERENCES, DISALLOWED-REFERENCES, LOCAL-BUILD?, SUBSTITUTABLE?,
and PROPERTIES."
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

         (mod+go-drv  (if (pair? modules)
                          (imported+compiled-modules store modules
                                                     #:guile guile-drv
                                                     #:system system)
                          '(#f . #f)))
         (mod-dir  (car mod+go-drv))
         (go-drv   (cdr mod+go-drv))
         (go-dir   (and go-drv
                        (derivation->output-path go-drv))))
    (derivation store name guile
                `("--no-auto-compile"
                  ,@(if mod-dir `("-L" ,mod-dir) '())
                  ,builder)

                ;; 'build-expression->derivation' is somewhat deprecated so
                ;; don't bother warning here.
                #:%deprecation-warning? #f

                #:system system

                #:inputs `((,(or guile-for-build (%guile-for-build)))
                           (,builder)
                           ,@(map cdr inputs)
                           ,@(if mod-dir `((,mod-dir) (,go-drv)) '()))

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
                #:disallowed-references disallowed-references
                #:local-build? local-build?
                #:substitutable? substitutable?
                #:properties properties)))


;;;
;;; Monadic interface.
;;;

(define built-derivations
  (store-lift build-derivations))

(define raw-derivation
  (store-lift derivation))
