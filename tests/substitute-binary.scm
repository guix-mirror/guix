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
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-34)
  #:use-module ((srfi srfi-64) #:hide (test-error)))

(define assert-valid-signature
  ;; (guix scripts substitute-binary) does not export this function in order to
  ;; avoid misuse.
  (@@ (guix scripts substitute-binary) assert-valid-signature))

;;; XXX: Replace with 'test-error' from SRFI-64 as soon as it allow us to
;;; catch specific exceptions.
(define-syntax-rule (test-error* name exp)
  (test-assert name
    (catch 'quit
      (lambda ()
        exp
        #f)
      (const #t))))

(define %keypair
  ;; (display (canonical-sexp->string
  ;;           (generate-key "(genkey (rsa (nbits 4:1024)))")))
  (string->canonical-sexp
   "(key-data
 (public-key
  (rsa
   (n #00D74A00F16DD109A8E773291856A4EF9EE2C2D975E0BC207EA24245C9CFE39E32D8BA5442A2720A57E3A9D9E55E596A8B19CB2EF844E5E859362593914BD626433C887FB798AE87E1DA95D372DFC81E220B8802B04CEC818D9B6B4E2108817755AEBAC23D2FD2B0AB82A52FD785194F3C2D7B9327212588DB74D464EEE5DC9F5B#)
   (e #010001#)
   )
  )
 (private-key
  (rsa
   (n #00D74A00F16DD109A8E773291856A4EF9EE2C2D975E0BC207EA24245C9CFE39E32D8BA5442A2720A57E3A9D9E55E596A8B19CB2EF844E5E859362593914BD626433C887FB798AE87E1DA95D372DFC81E220B8802B04CEC818D9B6B4E2108817755AEBAC23D2FD2B0AB82A52FD785194F3C2D7B9327212588DB74D464EEE5DC9F5B#)
   (e #010001#)
   (d #40E6D963EF143E9241BC10DE7A785C988C89EB1EC33253A5796AFB38FCC804D015500EC8CBCA0F5E318EE9D660DC19E7774E2E89BFD38379297EA87EFBDAC24BA32EE5339215382B2C89F5A817FD9131CA8E8A0A70D58E26E847AD0C447053671A6B2D7746087DE058A02B17701752B8A36EB414435921615AE7CAA8AC48E451#)
   (p #00EA88C0C19FE83C09285EF49FF88A1159357FD870031C20F15EF5103FBEB10925299BCA197F7143D6792A1BA7044EDA572EC94FA6B00889F9857216CF5B984403#)
   (q #00EAFE541EE9E0531255A85CADBEF64D5F679766D7209F521ADD131CF4B7DA9DF5414901342A146EE84FAA1E35EE0D0F6CE3F5F25989C0D1E9FA5B678D78C113C9#)
   (u #59C80FA2C48181F6855691C9D443619BA46C7648056E081697C370D8096E8EF165122D5E55F8FD6A2DCC404FA8BDCDC1FD20B4D76A433F25E8FD6901EC2DBDAD#)
   )
  )
 )"))

(define %public-key
  (find-sexp-token %keypair 'public-key))

(define %private-key
  (find-sexp-token %keypair 'private-key))

(define (signature-body str)
  (base64-encode
   (string->utf8
    (canonical-sexp->string
     (signature-sexp (bytevector->hash-data (sha256 (string->utf8 str))
                                            #:key-type 'rsa)
                     %private-key
                     %public-key)))))

(define %signature-body
  (signature-body "secret"))

(define %wrong-public-key
  (string->canonical-sexp "(public-key
 (rsa
  (n #00E05873AC2B168760343145918E954EE9AB73C026355693B192E01EE835261AA689E9EF46642E895BCD65C648524059FC450E4BA77A68F4C52D0E39EF0CC9359709AB6AAB153B63782201871325B0FDA19CB401CD99FD0C31A91CA9000AA90A77E82B89E036FB63BC1D3961207469B3B12468977148D376F8012BB12A4B11A8F1#)
  (e #010001#)
  )
 )"))

(define %wrong-signature
  (let* ((body (string->canonical-sexp
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
  (string-append str ";irrelevant;" body))

(define %signature
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
    (guard (condition ((pred condition) (pk 'true condition #t))
                      (else #f))
      exp
      #f)))

;;; XXX: Do we need a better predicate hierarchy for these tests?
(test-error-condition "corrupt signature data"
  nar-signature-error?
  (assert-valid-signature "invalid sexp" "irrelevant"
                          (open-input-string "irrelevant")
                          %acl))

(test-error-condition "unauthorized public key"
  nar-signature-error?
  (assert-valid-signature (canonical-sexp->string
                           (narinfo-signature->canonical-sexp %signature))
                          "irrelevant"
                          (open-input-string "irrelevant")
                          (public-keys->acl '())))

(test-error-condition "invalid signature"
  nar-signature-error?
  (assert-valid-signature (canonical-sexp->string
                           (narinfo-signature->canonical-sexp
                            %wrong-signature))
                          (sha256 (string->utf8 "secret"))
                          (open-input-string "irrelevant")
                          (public-keys->acl (list %wrong-public-key))))

(define %narinfo
  "StorePath: /nix/store/foo
URL: nar/foo
Compression: bzip2
NarHash: sha256:7
NarSize: 42
References: bar baz
Deriver: foo.drv
System: mips64el-linux\n")

(define (narinfo sig)
  (format #f "~aSignature: ~a~%" %narinfo sig))

(define %signed-narinfo
  (narinfo (signature "1" (signature-body %narinfo))))

(test-error-condition "invalid hash"
  ;; The hash of '%signature' is computed over the word "secret", not
  ;; '%narinfo'.
  nar-invalid-hash-error?
  (read-narinfo (open-input-string (narinfo %signature))
                "https://example.com" %acl))

(test-assert "valid read-narinfo"
  (read-narinfo (open-input-string %signed-narinfo)
                "https://example.com" %acl))

(test-equal "valid write-narinfo"
  %signed-narinfo
  (call-with-output-string
   (lambda (port)
     (write-narinfo (read-narinfo (open-input-string %signed-narinfo)
                                  "https://example.com" %acl)
                    port))))

(test-end "substitute-binary")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
