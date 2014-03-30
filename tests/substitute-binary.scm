;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-substitute-binary)
  #:use-module (guix scripts substitute-binary)
  #:use-module (guix base64)
  #:use-module (guix hash)
  #:use-module (guix nar)
  #:use-module (guix pk-crypto)
  #:use-module (guix pki)
  #:use-module (guix config)
  #:use-module ((guix store) #:select (%store-prefix))
  #:use-module ((guix build utils) #:select (delete-file-recursively))
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (web uri)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module ((srfi srfi-64) #:hide (test-error)))

(define assert-valid-signature
  ;; (guix scripts substitute-binary) does not export this function in order to
  ;; avoid misuse.
  (@@ (guix scripts substitute-binary) assert-valid-signature))

;;; XXX: Replace with 'test-error' from SRFI-64 as soon as it allow us to
;;; catch specific exceptions.
(define-syntax-rule (test-error* name exp)
  (test-equal name
    1
    (catch 'quit
      (lambda ()
        exp
        #f)
      (lambda (key value)
        value))))

(define %public-key
  ;; This key is known to be in the ACL by default.
  (call-with-input-file (string-append %config-directory "/signing-key.pub")
    (compose string->canonical-sexp get-string-all)))

(define %private-key
  (call-with-input-file (string-append %config-directory "/signing-key.sec")
    (compose string->canonical-sexp get-string-all)))

(define* (signature-body str #:key (public-key %public-key))
  "Return the signature of STR as the base64-encoded body of a narinfo's
'Signature' field."
  (base64-encode
   (string->utf8
    (canonical-sexp->string
     (signature-sexp (bytevector->hash-data (sha256 (string->utf8 str))
                                            #:key-type 'rsa)
                     %private-key
                     public-key)))))

(define %signature-body
  ;; Body of the signature of the word "secret".
  (signature-body "secret"))

(define %wrong-public-key
  (string->canonical-sexp "(public-key
 (rsa
  (n #00E05873AC2B168760343145918E954EE9AB73C026355693B192E01EE835261AA689E9EF46642E895BCD65C648524059FC450E4BA77A68F4C52D0E39EF0CC9359709AB6AAB153B63782201871325B0FDA19CB401CD99FD0C31A91CA9000AA90A77E82B89E036FB63BC1D3961207469B3B12468977148D376F8012BB12A4B11A8F1#)
  (e #010001#)
  )
 )"))

(define %wrong-signature
  ;; 'Signature' field where the public key doesn't match the private key used
  ;; to make the signature.
  (let* ((body       (string->canonical-sexp
                      (utf8->string
                       (base64-decode %signature-body))))
         (data       (canonical-sexp->string (find-sexp-token body 'data)))
         (sig-val    (canonical-sexp->string (find-sexp-token body 'sig-val)))
         (public-key (canonical-sexp->string %wrong-public-key))
         (body*      (base64-encode
                      (string->utf8
                       (string-append "(signature \n" data sig-val
                                      public-key " )\n")))))
    (string-append "1;irrelevant;" body*)))

(define* (signature str #:optional (body %signature-body))
  "Return the 'Signature' field value with STR as the version part and BODY as
the actual base64-encoded signature part."
  (string-append str ";irrelevant;" body))

(define %signature
  ;; Signature computed over the word "secret".
  (signature "1" %signature-body))

(define %acl
  (public-keys->acl (list %public-key)))


(test-begin "substitute-binary")

(test-error* "not a number"
  (narinfo-signature->canonical-sexp (signature "not a number")))

(test-error* "wrong version number"
  (narinfo-signature->canonical-sexp (signature "2")))

(test-assert "valid narinfo-signature->canonical-sexp"
  (canonical-sexp? (narinfo-signature->canonical-sexp %signature)))

(define-syntax-rule (test-error-condition name pred exp)
  (test-assert name
    (guard (condition ((pred condition) #t)
                      (else #f))
      exp
      #f)))

;;; XXX: Do we need a better predicate hierarchy for these tests?
(test-error-condition "corrupt signature data"
  nar-signature-error?
  (assert-valid-signature (string->canonical-sexp "(foo bar baz)") "irrelevant"
                          (open-input-string "irrelevant")
                          %acl))

(test-error-condition "unauthorized public key"
  nar-signature-error?
  (assert-valid-signature (narinfo-signature->canonical-sexp %signature)
                          "irrelevant"
                          (open-input-string "irrelevant")
                          (public-keys->acl '())))

(test-error-condition "invalid signature"
  nar-signature-error?
  (assert-valid-signature (narinfo-signature->canonical-sexp
                           %wrong-signature)
                          (sha256 (string->utf8 "secret"))
                          (open-input-string "irrelevant")
                          (public-keys->acl (list %wrong-public-key))))


(define %narinfo
  (string-append "StorePath: " (%store-prefix)
                 "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo
URL: nar/foo
Compression: bzip2
NarHash: sha256:7
NarSize: 42
References: bar baz
Deriver: " (%store-prefix) "/foo.drv
System: mips64el-linux\n"))

(define (narinfo sig)
  "Return a narinfo with SIG as its 'Signature' field."
  (format #f "~aSignature: ~a~%" %narinfo sig))

(define %signed-narinfo
  ;; Narinfo with a valid signature.
  (narinfo (signature "1" (signature-body %narinfo))))

(define (call-with-narinfo narinfo thunk)
  "Call THUNK in a context where $GUIX_BINARY_SUBSTITUTE_URL is populated with
a file for NARINFO."
  (let ((narinfo-directory (and=> (string->uri (getenv
                                                "GUIX_BINARY_SUBSTITUTE_URL"))
                                  uri-path))
        (cache-directory   (string-append (getenv "XDG_CACHE_HOME")
                                          "/guix/substitute-binary/")))
    (dynamic-wind
      (lambda ()
        (when (file-exists? cache-directory)
          (delete-file-recursively cache-directory))
        (call-with-output-file (string-append narinfo-directory
                                              "/nix-cache-info")
          (lambda (port)
            (format port "StoreDir: ~a\nWantMassQuery: 0\n"
                    (%store-prefix))))
        (call-with-output-file (string-append narinfo-directory "/"
                                              "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                              ".narinfo")
          (cut display narinfo <>))

        (set! (@@ (guix scripts substitute-binary)
                  %allow-unauthenticated-substitutes?)
              #f))
      thunk
      (lambda ()
        (delete-file-recursively cache-directory)))))

(define-syntax-rule (with-narinfo narinfo body ...)
  (call-with-narinfo narinfo (lambda () body ...)))


(test-equal "query narinfo with invalid hash"
  ;; The hash of '%signature' is computed over the word "secret", not
  ;; '%narinfo'.
  ""

  (with-narinfo (narinfo %signature)
    (string-trim-both
     (with-output-to-string
       (lambda ()
         (with-input-from-string (string-append "have " (%store-prefix)
                                                "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
           (lambda ()
             (guix-substitute-binary "--query"))))))))

(test-equal "query narinfo signed with authorized key"
  (string-append (%store-prefix) "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")

  (with-narinfo %signed-narinfo
    (string-trim-both
     (with-output-to-string
       (lambda ()
         (with-input-from-string (string-append "have " (%store-prefix)
                                                "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
           (lambda ()
             (guix-substitute-binary "--query"))))))))

(test-equal "query narinfo signed with unauthorized key"
  ""                                              ; not substitutable

  (with-narinfo (narinfo (signature "1"
                                    (signature-body %narinfo
                                                    #:public-key %wrong-public-key)))
    (string-trim-both
     (with-output-to-string
       (lambda ()
         (with-input-from-string (string-append "have " (%store-prefix)
                                                "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
           (lambda ()
             (guix-substitute-binary "--query"))))))))

(test-error* "substitute, invalid hash"
  ;; The hash of '%signature' is computed over the word "secret", not
  ;; '%narinfo'.
  (with-narinfo (narinfo %signature)
    (guix-substitute-binary "--substitute"
                            (string-append (%store-prefix)
                                           "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
                            "foo")))

(test-error* "substitute, unauthorized key"
  (with-narinfo (narinfo (signature "1"
                                    (signature-body %narinfo
                                                    #:public-key %wrong-public-key)))
    (guix-substitute-binary "--substitute"
                            (string-append (%store-prefix)
                                           "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
                            "foo")))

(test-end "substitute-binary")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'with-narinfo 'scheme-indent-function 1)
;;; eval: (put 'test-error* 'scheme-indent-function 1)
;;; End:
