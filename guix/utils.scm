;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

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
            package-name->name+version))


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
   (or %nixpkgs (getenv "NIXPKGS"))))

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
    (define (make-syntactic-constructor type name ctor fields defaults)
      "Make the syntactic constructor NAME for TYPE, that calls CTOR, and
expects all of FIELDS to be initialized.  DEFAULTS is the list of
FIELD/DEFAULT-VALUE tuples."
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


              (syntax-case s (inherit #,@fields)
                ((_ (inherit orig-record) (field value) (... ...))
                 #`(letrec* ((field value) (... ...))
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
                         (car (assoc-ref dflt (syntax->datum f)))))

                   (let-syntax ((error*
                                 (syntax-rules ()
                                   ((_ fmt args (... ...))
                                    (syntax-violation 'name
                                                      (format #f fmt args
                                                              (... ...))
                                                      s)))))
                     (let ((fields (append fields (map car dflt))))
                       (cond ((lset= eq? fields 'expected)
                              #`(letrec* ((field value) (... ...))
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

    (syntax-case s ()
      ((_ type syntactic-ctor ctor pred
          (field get options ...) ...)
       #`(begin
           (define-record-type type
             (ctor field ...)
             pred
             (field get) ...)
           #,(make-syntactic-constructor #'type #'syntactic-ctor #'ctor
                                         #'(field ...)
                                         (filter-map field-default-value
                                                     #'((field options ...)
                                                        ...))))))))

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
  (make-parameter (gnu-triplet->nix-system %host-type)))

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
