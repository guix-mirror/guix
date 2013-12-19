;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix pk-crypto)
  #:use-module (guix config)
  #:use-module ((guix utils) #:select (bytevector->base16-string))
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:export (gcry-sexp?
            string->gcry-sexp
            gcry-sexp->string
            number->gcry-sexp
            bytevector->hash-data
            sign
            verify
            generate-key
            find-sexp-token))


;;; Commentary:
;;;
;;; Public key cryptographic routines from GNU Libgcrypt.
;;;;
;;; Libgcrypt uses s-expressions to represent key material, parameters, and
;;; data.  We keep it as an opaque object rather than attempting to map them
;;; to Scheme s-expressions because (1) Libgcrypt sexps are stored in secure
;;; memory, and (2) the read syntax is different.
;;;
;;; Code:

;; Libgcrypt "s-expressions".
(define-wrapped-pointer-type <gcry-sexp>
  gcry-sexp?
  naked-pointer->gcry-sexp
  gcry-sexp->pointer
  (lambda (obj port)
    ;; Don't print OBJ's external representation: we don't want key material
    ;; to leak in backtraces and such.
    (format port "#<gcry-sexp ~a | ~a>"
            (number->string (object-address obj) 16)
            (number->string (pointer-address (gcry-sexp->pointer obj))
                            16))))

(define libgcrypt-func
  (let ((lib (dynamic-link %libgcrypt)))
    (lambda (func)
      "Return a pointer to symbol FUNC in libgcrypt."
      (dynamic-func func lib))))

(define finalize-gcry-sexp!
  (libgcrypt-func "gcry_sexp_release"))

(define-inlinable (pointer->gcry-sexp ptr)
  "Return a <gcry-sexp> that wraps PTR."
  (let* ((sexp (naked-pointer->gcry-sexp ptr))
         (ptr* (gcry-sexp->pointer sexp)))
    ;; Did we already have a <gcry-sexp> object for PTR?
    (when (equal? ptr ptr*)
      ;; No, so we can safely add a finalizer (in Guile 2.0.9
      ;; 'set-pointer-finalizer!' *adds* a finalizer rather than replacing the
      ;; existing one.)
      (set-pointer-finalizer! ptr finalize-gcry-sexp!))
    sexp))

(define string->gcry-sexp
  (let* ((ptr  (libgcrypt-func "gcry_sexp_new"))
         (proc (pointer->procedure int ptr `(* * ,size_t ,int))))
    (lambda (str)
      "Parse STR and return the corresponding gcrypt s-expression."
      (let* ((sexp (bytevector->pointer (make-bytevector (sizeof '*))))
             (err  (proc sexp (string->pointer str) 0 1)))
        (if (= 0 err)
            (pointer->gcry-sexp (dereference-pointer sexp))
            (throw 'gcry-error err))))))

(define-syntax GCRYSEXP_FMT_ADVANCED
  (identifier-syntax 3))

(define gcry-sexp->string
  (let* ((ptr  (libgcrypt-func "gcry_sexp_sprint"))
         (proc (pointer->procedure size_t ptr `(* ,int * ,size_t))))
    (lambda (sexp)
      "Return a textual representation of SEXP."
      (let loop ((len 1024))
        (let* ((buf  (bytevector->pointer (make-bytevector len)))
               (size (proc (gcry-sexp->pointer sexp)
                           GCRYSEXP_FMT_ADVANCED buf len)))
          (if (zero? size)
              (loop (* len 2))
              (pointer->string buf size "ISO-8859-1")))))))

(define (number->gcry-sexp number)
  "Return an s-expression representing NUMBER."
  (string->gcry-sexp (string-append "#" (number->string number 16) "#")))

(define* (bytevector->hash-data bv #:optional (hash-algo "sha256"))
  "Given BV, a bytevector containing a hash, return an s-expression suitable
for use as the data for 'sign'."
  (string->gcry-sexp
   (format #f "(data (flags pkcs1) (hash \"~a\" #~a#))"
           hash-algo
           (bytevector->base16-string bv))))

(define sign
  (let* ((ptr  (libgcrypt-func "gcry_pk_sign"))
         (proc (pointer->procedure int ptr '(* * *))))
    (lambda (data secret-key)
      "Sign DATA (an s-expression) with SECRET-KEY (an s-expression whose car
is 'private-key'.)"
      (let* ((sig (bytevector->pointer (make-bytevector (sizeof '*))))
             (err (proc sig (gcry-sexp->pointer data)
                        (gcry-sexp->pointer secret-key))))
        (if (= 0 err)
            (pointer->gcry-sexp (dereference-pointer sig))
            (throw 'gry-error err))))))

(define verify
  (let* ((ptr  (libgcrypt-func "gcry_pk_verify"))
         (proc (pointer->procedure int ptr '(* * *))))
    (lambda (signature data public-key)
      "Verify that SIGNATURE is a signature of DATA with PUBLIC-KEY, all of
which are gcrypt s-expressions."
      (zero? (proc (gcry-sexp->pointer signature)
                   (gcry-sexp->pointer data)
                   (gcry-sexp->pointer public-key))))))

(define generate-key
  (let* ((ptr  (libgcrypt-func "gcry_pk_genkey"))
         (proc (pointer->procedure int ptr '(* *))))
    (lambda (params)
      "Return as an s-expression a new key pair for PARAMS.  PARAMS must be an
s-expression like: (genkey (rsa (nbits 4:2048)))."
      (let* ((key (bytevector->pointer (make-bytevector (sizeof '*))))
             (err (proc key (gcry-sexp->pointer params))))
        (if (zero? err)
            (pointer->gcry-sexp (dereference-pointer key))
            (throw 'gcry-error err))))))

(define find-sexp-token
  (let* ((ptr  (libgcrypt-func "gcry_sexp_find_token"))
         (proc (pointer->procedure '* ptr `(* * ,size_t))))
    (lambda (sexp token)
      "Find in SEXP the first element whose 'car' is TOKEN and return it;
return #f if not found."
      (let* ((token (string->pointer (symbol->string token)))
             (res   (proc (gcry-sexp->pointer sexp) token 0)))
        (if (null-pointer? res)
            #f
            (pointer->gcry-sexp res))))))

;;; pk-crypto.scm ends here
