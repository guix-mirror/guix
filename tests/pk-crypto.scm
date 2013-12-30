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

(define-module (test-pk-crypto)
  #:use-module (guix pk-crypto)
  #:use-module (guix utils)
  #:use-module (guix hash)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match))

;; Test the (guix pk-crypto) module.

(define %key-pair
  ;; Key pair that was generated with:
  ;;   (generate-key (string->canonical-sexp "(genkey (rsa (nbits 4:1024)))"))
  ;; which takes a bit of time.
  "(key-data
    (public-key
     (rsa
      (n #00C1F764069F54FFE93A126B02328903E984E4AE3AF6DF402B5B6B3907911B88C385F1BA76A002EC9DEA109A5228EF0E62EE31A06D1A5861CAB474F6C857AC66EB65A1905F25BBA1869579E73A3B7FED13AF5A1667326F88CDFC2FF24B03C14FD1384AA7E73CA89572880B606E3A974E15347963FC7B6378574936A47580DBCB45#)
      (e #010001#)))
    (private-key
     (rsa
      (n #00C1F764069F54FFE93A126B02328903E984E4AE3AF6DF402B5B6B3907911B88C385F1BA76A002EC9DEA109A5228EF0E62EE31A06D1A5861CAB474F6C857AC66EB65A1905F25BBA1869579E73A3B7FED13AF5A1667326F88CDFC2FF24B03C14FD1384AA7E73CA89572880B606E3A974E15347963FC7B6378574936A47580DBCB45#)
      (e #010001#)
      (d #58CAD84653D0046A8EC3F9AA82D9C829B145422109FC3F12DA01A694B92FA296E70D366FB166454D30E632CEE3A033B4C41781BA10325F69FCDC0250CA19C8EEB352FA085992494098DB133E682ED38A931701F0DED1A1E508F4341A4FB446A04F019427C7CB3C44F251EEA9D386100DA80F125E0FD5CE1B0DFEC6D21516EACD#)
      (p #00D47F185147EC39393CCDA4E7323FFC20FC8B8073E2A54DD63BA392A66975E4204CA48572496A9DFD7522436B852C07472A5AB25B7706F7C14E6F33FBC420FF3B#)
      (q #00E9AD22F158060BC9AE3601DA623AFC60FFF3058795802CA92371C00097335CF9A23D7782DE353C9DBA93D7BB99E6A24A411107605E722481C5C191F80D7EB77F#)
      (u #59B45B95AE01A7A7370FAFDB08FE73A4793CE37F228961B09B1B1E7DDAD9F8D3E28F5C5E8B4B067E6B8E0BBF3F690B42991A79E46108DDCDA2514323A66964DE#))))")

(test-begin "pk-crypto")

(let ((sexps '("(foo bar)"

               ;; In Libgcrypt 1.5.3 the following integer is rendered as
               ;; binary, whereas in 1.6.0 it's rendered as is (hexadecimal.)
               ;;"#C0FFEE#"

               "(genkey \n (rsa \n  (nbits \"1024\")\n  )\n )")))
  (test-equal "string->canonical-sexp->string"
    sexps
    (let ((sexps (map string->canonical-sexp sexps)))
      (and (every canonical-sexp? sexps)
           (map (compose string-trim-both canonical-sexp->string) sexps)))))

(gc)                                              ; stress test!

(let ((sexps `(("(foo bar)" foo -> "(foo bar)")
               ("(foo (bar (baz 3:123)))" baz -> "(baz \"123\")")
               ("(foo (bar 3:123))" baz -> #f))))
  (test-equal "find-sexp-token"
    (map (match-lambda
          ((_ _ '-> expected)
           expected))
         sexps)
    (map (match-lambda
          ((input token '-> _)
           (let ((sexp (find-sexp-token (string->canonical-sexp input) token)))
             (and sexp
                  (string-trim-both (canonical-sexp->string sexp))))))
         sexps)))

(gc)

(test-equal "canonical-sexp-length"
  '(0 1 2 4 0 0)
  (map (compose canonical-sexp-length string->canonical-sexp)
       '("()" "(a)" "(a b)" "(a #616263# b #C001#)" "a" "#123456#")))

(test-equal "canonical-sexp-list?"
  '(#t #f #t #f)
  (map (compose canonical-sexp-list? string->canonical-sexp)
       '("()" "\"abc\"" "(a b c)" "#123456#")))

(gc)

(test-equal "canonical-sexp-car + cdr"
  '("(b \n (c xyz)\n )")
  (let ((lst (string->canonical-sexp "(a (b (c xyz)))")))
    (map (lambda (sexp)
           (and sexp (string-trim-both (canonical-sexp->string sexp))))
         ;; Note: 'car' returns #f when the first element is an atom.
         (list (canonical-sexp-car (canonical-sexp-cdr lst))))))

(gc)

(test-equal "canonical-sexp-nth"
  '("(b pqr)" "(c \"456\")" "(d xyz)" #f #f)

  (let ((lst (string->canonical-sexp "(a (b 3:pqr) (c 3:456) (d 3:xyz))")))
    ;; XXX: In Libgcrypt 1.5.3, (canonical-sexp-nth lst 0) returns LST, whereas in
    ;; 1.6.0 it returns #f.
    (map (lambda (sexp)
           (and sexp (string-trim-both (canonical-sexp->string sexp))))
         (unfold (cut > <> 5)
                 (cut canonical-sexp-nth lst <>)
                 1+
                 1))))

(gc)

(test-equal "canonical-sexp-nth-data"
  `(Name Otto Meier #f ,(base16-string->bytevector "123456") #f)
  (let ((lst (string->canonical-sexp
              "(Name Otto Meier (address Burgplatz) #123456#)")))
    (unfold (cut > <> 5)
            (cut canonical-sexp-nth-data lst <>)
            1+
            0)))

(gc)

;; XXX: The test below is typically too long as it needs to gather enough entropy.

;; (test-assert "generate-key"
;;   (let ((key (generate-key (string->canonical-sexp
;;                             "(genkey (rsa (nbits 3:128)))"))))
;;     (and (canonical-sexp? key)
;;          (find-sexp-token key 'key-data)
;;          (find-sexp-token key 'public-key)
;;          (find-sexp-token key 'private-key))))

(test-assert "bytevector->hash-data->bytevector"
  (let* ((bv   (sha256 (string->utf8 "Hello, world.")))
         (data (bytevector->hash-data bv "sha256")))
    (and (canonical-sexp? data)
         (let-values (((value algo) (hash-data->bytevector data)))
           (and (string=? algo "sha256")
                (bytevector=? value bv))))))

(test-assert "sign + verify"
  (let* ((pair   (string->canonical-sexp %key-pair))
         (secret (find-sexp-token pair 'private-key))
         (public (find-sexp-token pair 'public-key))
         (data   (bytevector->hash-data
                  (sha256 (string->utf8 "Hello, world."))))
         (sig    (sign data secret)))
    (and (verify sig data public)
         (not (verify sig
                      (bytevector->hash-data
                       (sha256 (string->utf8 "Hi!")))
                      public)))))

(gc)

(test-equal "canonical-sexp->sexp"
  `((data
     (flags pkcs1)
     (hash sha256
           ,(base16-string->bytevector
             "2749f0ea9f26c6c7be746a9cff8fa4c2f2a02b000070dba78429e9a11f87c6eb")))

    (public-key
     (rsa
      (n ,(base16-string->bytevector
           (string-downcase
            "00C1F764069F54FFE93A126B02328903E984E4AE3AF6DF402B5B6B3907911B88C385F1BA76A002EC9DEA109A5228EF0E62EE31A06D1A5861CAB474F6C857AC66EB65A1905F25BBA1869579E73A3B7FED13AF5A1667326F88CDFC2FF24B03C14FD1384AA7E73CA89572880B606E3A974E15347963FC7B6378574936A47580DBCB45")))
      (e ,(base16-string->bytevector
           "010001")))))

  (list (canonical-sexp->sexp
         (string->canonical-sexp
          "(data
             (flags pkcs1)
             (hash \"sha256\"
                   #2749f0ea9f26c6c7be746a9cff8fa4c2f2a02b000070dba78429e9a11f87c6eb#))"))

        (canonical-sexp->sexp
         (find-sexp-token (string->canonical-sexp %key-pair)
                          'public-key))))


(let ((lst
       `((data
          (flags pkcs1)
          (hash sha256
                ,(base16-string->bytevector
                  "2749f0ea9f26c6c7be746a9cff8fa4c2f2a02b000070dba78429e9a11f87c6eb")))

         (public-key
          (rsa
           (n ,(base16-string->bytevector
                (string-downcase
                 "00C1F764069F54FFE93A126B02328903E984E4AE3AF6DF402B5B6B3907911B88C385F1BA76A002EC9DEA109A5228EF0E62EE31A06D1A5861CAB474F6C857AC66EB65A1905F25BBA1869579E73A3B7FED13AF5A1667326F88CDFC2FF24B03C14FD1384AA7E73CA89572880B606E3A974E15347963FC7B6378574936A47580DBCB45")))
           (e ,(base16-string->bytevector
                "010001")))))))
  (test-equal "sexp->canonical-sexp->sexp"
    lst
    (map (compose canonical-sexp->sexp sexp->canonical-sexp)
         lst)))

(let ((sexp `(signature
              (public-key
               (rsa
                (n ,(make-bytevector 1024 1))
                (e ,(base16-string->bytevector "010001")))))))
  (test-equal "https://bugs.g10code.com/gnupg/issue1594"
    ;; The gcrypt bug above was primarily affecting our uses in
    ;; 'canonical-sexp->sexp', typically when applied to a signature sexp (in
    ;; 'guix authenticate -verify') with a "big" RSA key, such as 4096 bits.
    sexp
    (canonical-sexp->sexp (sexp->canonical-sexp sexp))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))
