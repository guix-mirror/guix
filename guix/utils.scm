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

(define-module (guix utils)
  #:use-module (guix config)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-39)
  #:use-module (srfi srfi-60)
  #:use-module (rnrs bytevectors)
  #:use-module ((rnrs io ports) #:select (put-bytevector))
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 format)
  #:autoload   (ice-9 popen)  (open-pipe*)
  #:autoload   (ice-9 rdelim) (read-line)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:autoload   (system foreign) (pointer->procedure)
  #:export (bytevector->base16-string
            base16-string->bytevector
            sha256

            %nixpkgs-directory
            nixpkgs-derivation
            nixpkgs-derivation*

            define-record-type*
            compile-time-value
            memoize
            default-keyword-arguments
            substitute-keyword-arguments

            <location>
            location
            location?
            location-file
            location-line
            location-column
            source-properties->location

            gnu-triplet->nix-system
            %current-system
            version-compare
            version>?
            package-name->name+version
            call-with-temporary-output-file
            fold2))


;;;
;;; Compile-time computations.
;;;

(define-syntax compile-time-value
  (syntax-rules ()
    "Evaluate the given expression at compile time.  The expression must
evaluate to a simple datum."
    ((_ exp)
     (let-syntax ((v (lambda (s)
                       (let ((val exp))
                         (syntax-case s ()
                           (_ #`'#,(datum->syntax s val)))))))
       v))))


;;;
;;; Base 16.
;;;

(define (bytevector->base16-string bv)
  "Return the hexadecimal representation of BV's contents."
  (define len
    (bytevector-length bv))

  (let-syntax ((base16-chars (lambda (s)
                               (syntax-case s ()
                                 (_
                                  (let ((v (list->vector
                                            (unfold (cut > <> 255)
                                                    (lambda (n)
                                                      (format #f "~2,'0x" n))
                                                    1+
                                                    0))))
                                    v))))))
    (define chars base16-chars)
    (let loop ((i 0)
               (r '()))
      (if (= i len)
          (string-concatenate-reverse r)
          (loop (+ 1 i)
                (cons (vector-ref chars (bytevector-u8-ref bv i)) r))))))

(define base16-string->bytevector
  (let ((chars->value (fold (lambda (i r)
                              (vhash-consv (string-ref (number->string i 16)
                                                       0)
                                           i r))
                            vlist-null
                            (iota 16))))
    (lambda (s)
      "Return the bytevector whose hexadecimal representation is string S."
      (define bv
        (make-bytevector (quotient (string-length s) 2) 0))

      (string-fold (lambda (chr i)
                     (let ((j (quotient i 2))
                           (v (and=> (vhash-assv chr chars->value) cdr)))
                       (if v
                           (if (zero? (logand i 1))
                               (bytevector-u8-set! bv j
                                                   (arithmetic-shift v 4))
                               (let ((w (bytevector-u8-ref bv j)))
                                 (bytevector-u8-set! bv j (logior v w))))
                           (error "invalid hexadecimal character" chr)))
                     (+ i 1))
                   0
                   s)
      bv)))


;;;
;;; Hash.
;;;

(define sha256
  (let ((hash   (pointer->procedure void
                                    (dynamic-func "gcry_md_hash_buffer"
                                                  (dynamic-link %libgcrypt))
                                    `(,int * * ,size_t)))
        (sha256 8))                        ; GCRY_MD_SHA256, as of 1.5.0
    (lambda (bv)
      "Return the SHA256 of BV as a bytevector."
      (let ((digest (make-bytevector (/ 256 8))))
        (hash sha256 (bytevector->pointer digest)
              (bytevector->pointer bv) (bytevector-length bv))
        digest))))


;;;
;;; Nixpkgs.
;;;

(define %nixpkgs-directory
  (make-parameter
   ;; Capture the build-time value of $NIXPKGS.
   (or %nixpkgs
       (and=> (getenv "NIXPKGS")
              (lambda (val)
                ;; Bail out when passed an empty string, otherwise
                ;; `nix-instantiate' will sit there and attempt to read
                ;; from its standard input.
                (if (string=? val "")
                    #f
                    val))))))

(define* (nixpkgs-derivation attribute #:optional (system (%current-system)))
  "Return the derivation path of ATTRIBUTE in Nixpkgs."
  (let* ((p (open-pipe* OPEN_READ (or (getenv "NIX_INSTANTIATE")
                                      %nix-instantiate)
                        "-A" attribute (%nixpkgs-directory)
                        "--argstr" "system" system))
         (l (read-line p))
         (s (close-pipe p)))
    (and (zero? (status:exit-val s))
         (not (eof-object? l))
         l)))

(define-syntax-rule (nixpkgs-derivation* attribute)
  "Evaluate the given Nixpkgs derivation at compile-time."
  (compile-time-value (nixpkgs-derivation attribute)))


;;;
;;; Miscellaneous.
;;;

(define-syntax define-record-type*
  (lambda (s)
    "Define the given record type such that an additional \"syntactic
constructor\" is defined, which allows instances to be constructed with named
field initializers, à la SRFI-35, as well as default values."
    (define (make-syntactic-constructor type name ctor fields thunked defaults)
      "Make the syntactic constructor NAME for TYPE, that calls CTOR, and
expects all of FIELDS to be initialized.  DEFAULTS is the list of
FIELD/DEFAULT-VALUE tuples, and THUNKED is the list of identifiers of
thunked fields."
      (with-syntax ((type     type)
                    (name     name)
                    (ctor     ctor)
                    (expected fields)
                    (defaults defaults))
        #`(define-syntax name
            (lambda (s)
              (define (record-inheritance orig-record field+value)
                ;; Produce code that returns a record identical to
                ;; ORIG-RECORD, except that values for the FIELD+VALUE alist
                ;; prevail.
                (define (field-inherited-value f)
                  (and=> (find (lambda (x)
                                 (eq? f (car (syntax->datum x))))
                               field+value)
                         car))

                #`(make-struct type 0
                               #,@(map (lambda (field index)
                                         (or (field-inherited-value field)
                                             #`(struct-ref #,orig-record
                                                           #,index)))
                                       'expected
                                       (iota (length 'expected)))))

              (define (thunked-field? f)
                (memq (syntax->datum f) '#,thunked))

              (define (field-bindings field+value)
                ;; Return field to value bindings, for use in `letrec*' below.
                (map (lambda (field+value)
                       (syntax-case field+value ()
                         ((field value)
                          #`(field
                             #,(if (thunked-field? #'field)
                                   #'(lambda () value)
                                   #'value)))))
                     field+value))

              (syntax-case s (inherit #,@fields)
                ((_ (inherit orig-record) (field value) (... ...))
                 #`(letrec* #,(field-bindings #'((field value) (... ...)))
                     #,(record-inheritance #'orig-record
                                           #'((field value) (... ...)))))
                ((_ (field value) (... ...))
                 (let ((fields (map syntax->datum #'(field (... ...))))
                       (dflt   (map (match-lambda
                                     ((f v)
                                      (list (syntax->datum f) v)))
                                    #'defaults)))

                   (define (field-value f)
                     (or (and=> (find (lambda (x)
                                        (eq? f (car (syntax->datum x))))
                                      #'((field value) (... ...)))
                                car)
                         (let ((value
                                (car (assoc-ref dflt
                                                (syntax->datum f)))))
                           (if (thunked-field? f)
                               #`(lambda () #,value)
                               value))))

                   (let-syntax ((error*
                                 (syntax-rules ()
                                   ((_ fmt args (... ...))
                                    (syntax-violation 'name
                                                      (format #f fmt args
                                                              (... ...))
                                                      s)))))
                     (let ((fields (append fields (map car dflt))))
                       (cond ((lset= eq? fields 'expected)
                              #`(letrec* #,(field-bindings
                                            #'((field value) (... ...)))
                                  (ctor #,@(map field-value 'expected))))
                             ((pair? (lset-difference eq? fields 'expected))
                              (error* "extraneous field initializers ~a"
                                      (lset-difference eq? fields 'expected)))
                             (else
                              (error* "missing field initializers ~a"
                                      (lset-difference eq? 'expected
                                                       fields)))))))))))))

    (define (field-default-value s)
      (syntax-case s (default)
        ((field (default val) _ ...)
         (list #'field #'val))
        ((field _ options ...)
         (field-default-value #'(field options ...)))
        (_ #f)))

    (define (thunked-field? s)
      ;; Return the field name if the field defined by S is thunked.
      (syntax-case s (thunked)
        ((field (thunked) _ ...)
         #'field)
        ((field _ options ...)
         (thunked-field? #'(field options ...)))
        (_ #f)))

    (define (thunked-field-accessor-name field)
      ;; Return the name (an unhygienic syntax object) of the "real"
      ;; getter for field, which is assumed to be a thunked field.
      (syntax-case field ()
        ((field get options ...)
         (let* ((getter      (syntax->datum #'get))
                (real-getter (symbol-append '% getter '-real)))
           (datum->syntax #'get real-getter)))))

    (define (field-spec->srfi-9 field)
      ;; Convert a field spec of our style to a SRFI-9 field spec of the
      ;; form (field get).
      (syntax-case field ()
        ((name get options ...)
         #`(name
            #,(if (thunked-field? field)
                  (thunked-field-accessor-name field)
                  #'get)))))

    (define (thunked-field-accessor-definition field)
      ;; Return the real accessor for FIELD, which is assumed to be a
      ;; thunked field.
      (syntax-case field ()
        ((name get _ ...)
         (with-syntax ((real-get (thunked-field-accessor-name field)))
           #'(define-inlinable (get x)
               ;; The real value of that field is a thunk, so call it.
               ((real-get x)))))))

    (syntax-case s ()
      ((_ type syntactic-ctor ctor pred
          (field get options ...) ...)
       (let* ((field-spec #'((field get options ...) ...)))
         (with-syntax (((field-spec* ...)
                        (map field-spec->srfi-9 field-spec))
                       ((thunked-field-accessor ...)
                        (filter-map (lambda (field)
                                      (and (thunked-field? field)
                                           (thunked-field-accessor-definition
                                            field)))
                                    field-spec)))
           #`(begin
               (define-record-type type
                 (ctor field ...)
                 pred
                 field-spec* ...)
               (begin thunked-field-accessor ...)
               #,(make-syntactic-constructor #'type #'syntactic-ctor #'ctor
                                             #'(field ...)
                                             (filter-map thunked-field? field-spec)
                                             (filter-map field-default-value
                                                         #'((field options ...)
                                                            ...))))))))))

(define (memoize proc)
  "Return a memoizing version of PROC."
  (let ((cache (make-hash-table)))
    (lambda args
      (let ((results (hash-ref cache args)))
        (if results
            (apply values results)
            (let ((results (call-with-values (lambda ()
                                               (apply proc args))
                             list)))
              (hash-set! cache args results)
              (apply values results)))))))

(define (default-keyword-arguments args defaults)
  "Return ARGS augmented with any keyword/value from DEFAULTS for
keywords not already present in ARGS."
  (let loop ((defaults defaults)
             (args     args))
    (match defaults
      ((kw value rest ...)
       (loop rest
             (if (assoc-ref kw args)
                 args
                 (cons* kw value args))))
      (()
       args))))

(define-syntax substitute-keyword-arguments
  (syntax-rules ()
    "Return a new list of arguments where the value for keyword arg KW is
replaced by EXP.  EXP is evaluated in a context where VAR is boud to the
previous value of the keyword argument."
    ((_ original-args ((kw var) exp) ...)
     (let loop ((args    original-args)
                (before '()))
       (match args
         ((kw var rest (... ...))
          (loop rest (cons* exp kw before)))
         ...
         ((x rest (... ...))
          (loop rest (cons x before)))
         (()
          (reverse before)))))))

(define (gnu-triplet->nix-system triplet)
  "Return the Nix system type corresponding to TRIPLET, a GNU triplet as
returned by `config.guess'."
  (let ((triplet (cond ((string-match "^i[345]86-(.*)$" triplet)
                        =>
                        (lambda (m)
                          (string-append "i686-" (match:substring m 1))))
                       (else triplet))))
    (cond ((string-match "^([^-]+)-([^-]+-)?linux-gnu.*" triplet)
           =>
           (lambda (m)
             ;; Nix omits `-gnu' for GNU/Linux.
             (string-append (match:substring m 1) "-linux")))
          ((string-match "^([^-]+)-([^-]+-)?([[:alpha:]]+)([0-9]+\\.?)*$" triplet)
           =>
           (lambda (m)
             ;; Nix strip the version number from names such as `gnu0.3',
             ;; `darwin10.2.0', etc., and always strips the vendor part.
             (string-append (match:substring m 1) "-"
                            (match:substring m 3))))
          (else triplet))))

(define %current-system
  ;; System type as expected by Nix, usually ARCHITECTURE-KERNEL.
  ;; By default, this is equal to (gnu-triplet->nix-system %host-type).
  (make-parameter %system))

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
  "Return #t when A denotes a newer version than B."
  (eq? '> (version-compare a b)))

(define (package-name->name+version name)
  "Given NAME, a package name like \"foo-0.9.1b\", return two values:
\"foo\" and \"0.9.1b\".  When the version part is unavailable, NAME and
#f are returned.  The first hyphen followed by a digit is considered to
introduce the version part."
  ;; See also `DrvName' in Nix.

  (define number?
    (cut char-set-contains? char-set:digit <>))

  (let loop ((chars   (string->list name))
             (prefix '()))
    (match chars
      (()
       (values name #f))
      ((#\- (? number? n) rest ...)
       (values (list->string (reverse prefix))
               (list->string (cons n rest))))
      ((head tail ...)
       (loop tail (cons head prefix))))))

(define (call-with-temporary-output-file proc)
  "Call PROC with a name of a temporary file and open output port to that
file; close the file and delete it when leaving the dynamic extent of this
call."
  (let* ((template (string-copy "guix-file.XXXXXX"))
         (out      (mkstemp! template)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (proc template out))
      (lambda ()
        (false-if-exception (close out))
        (false-if-exception (delete-file template))))))

(define fold2
  (case-lambda
    ((proc seed1 seed2 lst)
     "Like `fold', but with a single list and two seeds."
     (let loop ((result1 seed1)
                (result2 seed2)
                (lst     lst))
       (if (null? lst)
           (values result1 result2)
           (call-with-values
               (lambda () (proc (car lst) result1 result2))
             (lambda (result1 result2)
               (loop result1 result2 (cdr lst)))))))
    ((proc seed1 seed2 lst1 lst2)
     "Like `fold', but with a two lists and two seeds."
     (let loop ((result1 seed1)
                (result2 seed2)
                (lst1    lst1)
                (lst2    lst2))
       (if (or (null? lst1) (null? lst2))
           (values result1 result2)
           (call-with-values
               (lambda () (proc (car lst1) (car lst2) result1 result2))
             (lambda (result1 result2)
               (fold2 proc result1 result2 (cdr lst1) (cdr lst2)))))))))


;;;
;;; Source location.
;;;

;; A source location.
(define-record-type <location>
  (make-location file line column)
  location?
  (file          location-file)                   ; file name
  (line          location-line)                   ; 1-indexed line
  (column        location-column))                ; 0-indexed column

(define location
  (memoize
   (lambda (file line column)
     "Return the <location> object for the given FILE, LINE, and COLUMN."
     (and line column file
          (make-location file line column)))))

(define (source-properties->location loc)
  "Return a location object based on the info in LOC, an alist as returned
by Guile's `source-properties', `frame-source', `current-source-location',
etc."
  (let ((file (assq-ref loc 'filename))
        (line (assq-ref loc 'line))
        (col  (assq-ref loc 'column)))
    ;; In accordance with the GCS, start line and column numbers at 1.
    (location file (and line (+ line 1)) (and col (+ col 1)))))
