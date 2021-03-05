;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2012 Göran Weinholt <goran@weinholt.se>
;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; This code was originally written by Göran Weinholt for Industria and
;;; released under the Expat license shown above.  It was then modified by
;;; Ludovic Courtès for use in GNU Guix: turned into a native Guile module,
;;; ported to Guile-Gcrypt, and extended and simplified in other ways.

(define-module (guix openpgp)
  #:export (get-openpgp-detached-signature/ascii
            (get-packet . get-openpgp-packet)
            verify-openpgp-signature
            port-ascii-armored?

            openpgp-error?
            openpgp-unrecognized-packet-error?
            openpgp-unrecognized-packet-error-port
            openpgp-unrecognized-packet-error-type
            openpgp-invalid-signature-error?
            openpgp-invalid-signature-error-port

            openpgp-signature?
            openpgp-signature-issuer-key-id
            openpgp-signature-issuer-fingerprint
            openpgp-signature-public-key-algorithm
            openpgp-signature-hash-algorithm
            openpgp-signature-creation-time
            openpgp-signature-expiration-time

            openpgp-user-id?
            openpgp-user-id-value
            openpgp-user-attribute?

            openpgp-public-key?
            openpgp-public-key-subkey?
            openpgp-public-key-value
            openpgp-public-key-fingerprint openpgp-format-fingerprint
            openpgp-public-key-id

            openpgp-keyring?
            %empty-keyring
            lookup-key-by-id
            lookup-key-by-fingerprint
            get-openpgp-keyring

            read-radix-64
            string->openpgp-packet)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-60)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (ice-9 vlist)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt pk-crypto)
  #:use-module (gcrypt base64)
  #:use-module (gcrypt base16)
  #:use-module ((guix build utils) #:select (dump-port)))

;;; Commentary:
;;;
;;; This module contains code to read OpenPGP messages as described in
;;; <https://tools.ietf.org/html/rfc4880>, with extensions from
;;; <https://tools.ietf.org/html/draft-ietf-openpgp-rfc4880bis-06> (notably
;;; EdDSA support and extra signature sub-packets).
;;;
;;; Currently this module does enough to verify detached signatures of binary
;;; data.  It does _not_ perform sanity checks on self-signatures, subkey
;;; binding signatures, etc., among others.  Use only in a context where this
;;; limitations are acceptable!
;;;
;;; Code:

(define-syntax print
  (syntax-rules ()
    ;; ((_ args ...) (pk 'openpgp args))
    ((_ args ...) (values))))

(define-syntax-rule (define-alias new old)
  (define-syntax new (identifier-syntax old)))

(define-alias fx+ +)
(define-alias fx- -)
(define-alias fx* *)
(define-alias fx/ /)
(define-alias fxdiv quotient)
(define-alias fxand logand)
(define-inlinable (fxbit-set? n index) (bit-set? index n))
(define-alias fxbit-field bit-field)
(define-alias bitwise-bit-field bit-field)
(define-alias fxarithmetic-shift-left ash)
(define-inlinable (fxarithmetic-shift-right i n) (ash i (- n)))
(define-inlinable (port-eof? port) (eof-object? (lookahead-u8 port)))

(define (string-hex-pad str)
  (if (odd? (string-length str))
      (string-append "0" str)
      str))

(define (unixtime n)
  (time-monotonic->date (make-time 'time-monotonic 0 n)))

;; Root of the error hierarchy.
(define-condition-type &openpgp-error &error
  openpgp-error?)

;; Error raised when reading an unsupported or unrecognized packet tag.
(define-condition-type &openpgp-unrecognized-packet-error &openpgp-error
  openpgp-unrecognized-packet-error?
  (type openpgp-unrecognized-packet-error-type)
  (port openpgp-unrecognized-packet-error-port))

;; Error raised when reading an invalid signature packet.
(define-condition-type &openpgp-invalid-signature-error &openpgp-error
  openpgp-invalid-signature-error?
  (port openpgp-invalid-signature-error-port))


;;;
;;; Bitwise I/O.
;;;
;;; TODO: Use Bytestructures instead.
;;;

(define-syntax-rule (integer-read size)
  (lambda (port)
    "Read from PORT a big-endian integer of SIZE bytes.  Return the EOF object
on end-of-file."
    (let ((buf (make-bytevector size)))
      (match (get-bytevector-n! port buf 0 size)
        (size (bytevector-uint-ref buf 0 (endianness big) size))
        (_    (eof-object))))))

(define get-u16 (integer-read 2))
(define get-u32 (integer-read 4))
(define get-u64 (integer-read 8))

(define-syntax get-integers
  (syntax-rules ()
    "Read from PORT integers of the given TYPE, in big endian encoding.  Each
TYPE must be one of u8, u16, u32, u64, or _, as in this example:

  (get-integers port u8 _ _ _ u32 u16)

In the case of _ (wildcard), one byte is read and discarded.  Return as many
values as there are TYPEs."
    ((_ port type ...)
     (letrec-syntax ((get-integer (syntax-rules (u8 u16 u32 u64)
                                    ((x u8) (get-u8 port))
                                    ((x u16) (get-u16 port))
                                    ((x u32) (get-u32 port))
                                    ((x u64) (get-u64 port))))
                     (values*     (syntax-rules (_)
                                    ((x (result (... ...)))
                                     (values result (... ...)))
                                    ((x (result (... ...)) _ rest (... ...))
                                     (let ((x (get-u8 port)))
                                       (values* (result (... ...))
                                                rest (... ...))))
                                    ((x (result (... ...)) t rest (... ...))
                                     (let ((x (get-integer t)))
                                       (values* (result (... ...) x)
                                                rest (... ...)))))))
       (values* () type ...)))))

(define (bytevector->uint bv)
  (bytevector-uint-ref bv 0 (endianness big)
                       (bytevector-length bv)))

(define-syntax-rule (integer-write size)
  (lambda (port integer)
    "Write INTEGER to PORT as a SIZE-byte integer and as big endian."
    (let ((bv (make-bytevector size)))
      (bytevector-uint-set! bv 0 integer (endianness big) size)
      (put-bytevector port bv))))

(define put-u16 (integer-write 2))
(define put-u32 (integer-write 4))
(define put-u64 (integer-write 8))

(define-syntax put-integers
  (syntax-rules ()
    "Write the given integers as big endian to PORT.  For example:

  (put-integers port u8 42 u32 #x7777)

writes to PORT the value 42 as an 8-bit integer and the value #x7777 as a
32-bit integer."
    ((_ port)
     #t)
    ((_ port type value rest ...)
     (let-syntax ((put (syntax-rules (u8 u16 u32 u64)
                         ((_ u8 port integer)
                          (put-u8 port integer))
                         ((_ u16 port integer)
                          (put-u16 port integer))
                         ((_ u32 port integer)
                          (put-u32 port integer))
                         ((_ u64 port integer)
                          (put-u64 port integer)))))
       (begin
         (put type port value)
         (put-integers port rest ...))))))

(define-syntax-rule (integers->bytevector type value rest ...)
  "Return the the TYPE/VALUE integers representation as a bytevector."
  (let-values (((port get) (open-bytevector-output-port)))
    (put-integers port type value rest ...)
    (force-output port)
    (get)))


(define (bytevector->bitnames bv names)
  (define (bit-set? bv i)
    (let ((idx (fxarithmetic-shift-right i 3))
          (bit (fxand i #b111)))
      (and (< idx (bytevector-length bv))
           (fxbit-set? (bytevector-u8-ref bv idx) bit))))
  (do ((names names (cdr names))
       (i 0 (fx+ i 1))
       (bits '()
             (if (bit-set? bv i)
                 (cons (car names) bits)
                 bits)))
      ((null? names) (reverse bits))))

(define (openpgp-format-fingerprint bv)
  "Return a string representing BV, a bytevector, in the conventional OpenPGP
hexadecimal format for fingerprints."
  (define (h i)
    (string-pad (string-upcase
                 (number->string
                  (bytevector-u16-ref bv (* i 2) (endianness big))
                  16))
                4 #\0))
  (string-append (h 0) " " (h 1) " " (h 2) " " (h 3) " " (h 4)
                 "  "
                 (h 5) " " (h 6) " " (h 7) " " (h 8) " " (h 9)))

;;; Constants


(define PACKET-SESSION-KEY 1)
(define PACKET-SIGNATURE 2)
(define PACKET-SYMMETRIC-SESSION-KEY 3)
(define PACKET-ONE-PASS-SIGNATURE 4)
(define PACKET-SECRET-KEY 5)
(define PACKET-PUBLIC-KEY 6)
(define PACKET-SECRET-SUBKEY 7)
(define PACKET-COMPRESSED-DATA 8)
(define PACKET-SYMMETRIC-ENCRYPTED-DATA 9)
(define PACKET-MARKER 10)
(define PACKET-LITERAL-DATA 11)
(define PACKET-TRUST 12)
(define PACKET-USER-ID 13)
(define PACKET-PUBLIC-SUBKEY 14)
(define PACKET-USER-ATTRIBUTE 17)
(define PACKET-SYMMETRIC-ENCRYPTED/PROTECTED-DATA 18)
(define PACKET-MDC 19)

(define PUBLIC-KEY-RSA 1)
(define PUBLIC-KEY-RSA-ENCRYPT-ONLY 2)
(define PUBLIC-KEY-RSA-SIGN-ONLY 3)
(define PUBLIC-KEY-ELGAMAL-ENCRYPT-ONLY 16)
(define PUBLIC-KEY-DSA 17)
(define PUBLIC-KEY-ECDH 18)                       ;RFC-6637
(define PUBLIC-KEY-ECDSA 19)                      ;RFC-6639
(define PUBLIC-KEY-ELGAMAL 20)                    ;encrypt + sign (legacy)
(define PUBLIC-KEY-EDDSA 22)                      ;"not yet assigned" says GPG

(define (public-key-algorithm id)
  (cond ((= id PUBLIC-KEY-RSA) 'rsa)
        ((= id PUBLIC-KEY-DSA) 'dsa)
        ((= id PUBLIC-KEY-ELGAMAL-ENCRYPT-ONLY) 'elgamal)
        ((= id PUBLIC-KEY-EDDSA) 'eddsa)
        (else id)))

(define SYMMETRIC-KEY-PLAINTEXT 0)
(define SYMMETRIC-KEY-IDEA 1)
(define SYMMETRIC-KEY-TRIPLE-DES 2)
(define SYMMETRIC-KEY-CAST5-128 3)
(define SYMMETRIC-KEY-BLOWFISH-128 4)
(define SYMMETRIC-KEY-AES-128 7)
(define SYMMETRIC-KEY-AES-192 8)
(define SYMMETRIC-KEY-AES-256 9)
(define SYMMETRIC-KEY-TWOFISH-256 10)
(define SYMMETRIC-KEY-CAMELLIA-128 11)            ;RFC-5581
(define SYMMETRIC-KEY-CAMELLIA-192 12)
(define SYMMETRIC-KEY-CAMELLIA-256 13)

(define (symmetric-key-algorithm id)
  (cond ((= id SYMMETRIC-KEY-PLAINTEXT) 'plaintext)
        ((= id SYMMETRIC-KEY-IDEA) 'idea)
        ((= id SYMMETRIC-KEY-TRIPLE-DES) 'tdea)
        ((= id SYMMETRIC-KEY-CAST5-128) 'cast5-128)
        ((= id SYMMETRIC-KEY-BLOWFISH-128) 'blowfish-128)
        ((= id SYMMETRIC-KEY-AES-128) 'aes-128)
        ((= id SYMMETRIC-KEY-AES-192) 'aes-192)
        ((= id SYMMETRIC-KEY-AES-256) 'aes-256)
        ((= id SYMMETRIC-KEY-TWOFISH-256) 'twofish-256)
        (else id)))

(define HASH-MD5 1)
(define HASH-SHA-1 2)
(define HASH-RIPE-MD160 3)
(define HASH-SHA-256 8)
(define HASH-SHA-384 9)
(define HASH-SHA-512 10)
(define HASH-SHA-224 11)

(define (openpgp-hash-algorithm id signature-port)
  (cond ((= id HASH-MD5) 'md5)
        ((= id HASH-SHA-1) 'sha1)
        ((= id HASH-RIPE-MD160) 'rmd160)
        ((= id HASH-SHA-256) 'sha256)
        ((= id HASH-SHA-384) 'sha384)
        ((= id HASH-SHA-512) 'sha512)
        ((= id HASH-SHA-224) 'sha224)
        (else
         (raise (condition
                 (&openpgp-invalid-signature-error (port signature-port)))))))

(define COMPRESSION-UNCOMPRESSED 0)
(define COMPRESSION-ZIP 1)                      ;deflate

(define COMPRESSION-ZLIB 2)
(define COMPRESSION-BZIP2 3)

(define (compression-algorithm id)
  (cond ((= id COMPRESSION-UNCOMPRESSED) 'uncompressed)
        ((= id COMPRESSION-ZIP) 'deflate)
        ((= id COMPRESSION-ZLIB) 'zlib)
        ((= id COMPRESSION-BZIP2) 'bzip2)
        (else id)))

(define SUBPACKET-SIGNATURE-CTIME 2)
(define SUBPACKET-SIGNATURE-ETIME 3)
  ;;  4 = Exportable Certification

(define SUBPACKET-TRUST-SIGNATURE 5)
  ;;  6 = Regular Expression

(define SUBPACKET-REVOCABLE 7)
(define SUBPACKET-KEY-ETIME 9)
(define SUBPACKET-PREFERRED-SYMMETRIC-ALGORITHMS 11)
  ;; 12 = Revocation Key

(define SUBPACKET-ISSUER 16)
(define SUBPACKET-NOTATION-DATA 20)
(define SUBPACKET-PREFERRED-HASH-ALGORITHMS 21)
(define SUBPACKET-PREFERRED-COMPRESSION-ALGORITHMS 22)
(define SUBPACKET-KEY-SERVER-PREFERENCES 23)
(define SUBPACKET-PREFERRED-KEY-SERVER 24)
(define SUBPACKET-PRIMARY-USER-ID 25)
(define SUBPACKET-POLICY-URI 26)
(define SUBPACKET-KEY-FLAGS 27)
(define SUBPACKET-SIGNER-USER-ID 28)
(define SUBPACKET-REASON-FOR-REVOCATION 29)
(define SUBPACKET-FEATURES 30)
  ;; 31 = Signature Target
(define SUBPACKET-EMBEDDED-SIGNATURE 32)
(define SUBPACKET-ISSUER-FINGERPRINT 33)          ;defined in RFC4880bis

(define SIGNATURE-BINARY #x00)
(define SIGNATURE-TEXT #x01)
(define SIGNATURE-STANDALONE #x02)
(define SIGNATURE-GENERIC-CERT #x10)
(define SIGNATURE-PERSONA-CERT #x11)
(define SIGNATURE-CASUAL-CERT #x12)
(define SIGNATURE-POSITIVE-CERT #x13)
(define SIGNATURE-SUBKEY-BINDING #x18)
(define SIGNATURE-PRIMARY-KEY-BINDING #x19)
(define SIGNATURE-DIRECT #x1f)
(define SIGNATURE-KEY-REVOCATION #x20)
(define SIGNATURE-SUBKEY-REVOCATION #x28)
(define SIGNATURE-CERT-REVOCATION #x30)
(define SIGNATURE-TIMESTAMP #x40)
(define SIGNATURE-THIRD-PARTY #x50)

;;; Parsing

  ;; Look at the tag byte and see if it looks reasonable, if it does
  ;; then the file is likely not armored. Does not move the port
  ;; position.

(define (port-ascii-armored? p)
  (let ((tag (lookahead-u8 p)))
    (cond ((eof-object? tag) #f)
          ((not (fxbit-set? tag 7)) #t)
          (else
           (let ((type (if (fxbit-set? tag 6)
                           (fxbit-field tag 0 6)
                           (fxbit-field tag 2 6))))
             (not (<= PACKET-SESSION-KEY type PACKET-MDC)))))))

(define (get-mpi/bytevector p)
  (let* ((bitlen  (get-u16 p))
         (bytelen (fxdiv (fx+ bitlen 7) 8)))
    (get-bytevector-n p bytelen)))

(define (get-mpi p)
  (bytevector->uint (get-mpi/bytevector p)))

(define (get-v4-length p)
  ;; TODO: indeterminate length (only for data packets)
  (let ((o1 (get-u8 p)))
    (cond ((< o1 192) o1)
          ((< o1 255)
           (+ (fxarithmetic-shift-left (fx- o1 192) 8)
              (get-u8 p)
              192))
          ((= o1 255)
           (get-u32 p)))))

(define (get-packet p)
  (if (port-eof? p)
      (eof-object)
      (get-packet* p get-data)))

(define (get-packet* p get-data)
  (let ((tag (get-u8 p)))
    ;; (unless (fxbit-set? tag 7) (error 'get-packet "Invalid tag" tag))
    (cond ((fxbit-set? tag 6)                     ;New packet format
           (let ((tag (fxbit-field tag 0 6))
                 (len (get-v4-length p)))
             (get-data p tag len)))
          (else                                   ;Old packet format
           (let ((tag (fxbit-field tag 2 6))
                 (len (case (fxbit-field tag 0 2)
                        ((0) (get-u8 p))
                        ((1) (get-u16 p))
                        ((2) (get-u32 p))
                        ((3) #f))))
             (get-data p tag len))))))

(define (get-data p tag len)
  (let ((pp (if len
                (open-bytevector-input-port (get-bytevector-n p len))
                p)))                              ;indeterminate length
    (cond
     ((= tag PACKET-SIGNATURE)
      (get-signature pp))
     ((= tag PACKET-PUBLIC-KEY)
      (get-public-key pp #f))
     ((= tag PACKET-TRUST)
      'openpgp-trust)                             ;XXX: non-standard format?
     ((= tag PACKET-USER-ID)
      (get-user-id pp len))
     ((= tag PACKET-PUBLIC-SUBKEY)
      (get-public-key pp #t))
     ((= tag PACKET-USER-ATTRIBUTE)
      (get-user-attribute pp len))
     ((= tag PACKET-ONE-PASS-SIGNATURE)
      'one-pass-signature)                        ;TODO: implement
     (else
      (raise (condition (&openpgp-unrecognized-packet-error (type tag)
                                                            (port p))))))))

(define-record-type <openpgp-public-key>
  (make-openpgp-public-key version subkey? time value fingerprint)
  openpgp-public-key?
  (version      openpgp-public-key-version)
  (subkey?      openpgp-public-key-subkey?)
  (time         openpgp-public-key-time)
  (value        openpgp-public-key-value)
  (fingerprint  openpgp-public-key-fingerprint))

;;; Signatures

(define-record-type <openpgp-signature>
  (make-openpgp-signature version type pk-algorithm hash-algorithm hashl16
                          append-data hashed-subpackets unhashed-subpackets
                          value issuer issuer-fingerprint)
  openpgp-signature?
  (version               openpgp-signature-version)
  (type                  openpgp-signature-type)
  (pk-algorithm          openpgp-signature-public-key-algorithm)
  (hash-algorithm        openpgp-signature-hash-algorithm)
  (hashl16               openpgp-signature-hashl16) ;left 16 bits of signed hash
  (append-data           openpgp-signature-append-data) ;append to data when hashing
  (hashed-subpackets     openpgp-signature-hashed-subpackets)
  (unhashed-subpackets   openpgp-signature-unhashed-subpackets)
  (value                 openpgp-signature-value)
  (issuer                openpgp-signature-issuer-key-id)       ;integer | #f
  (issuer-fingerprint    openpgp-signature-issuer-fingerprint)) ;bytevector | #f

(define (openpgp-signature-creation-time sig)
  (cond ((assq 'signature-ctime (openpgp-signature-hashed-subpackets sig))
         => (lambda (x) (unixtime (cdr x))))
        ;; XXX: should be an error?
        (else #f)))

(define (openpgp-signature-expiration-time sig)
  (cond ((assq 'signature-etime (openpgp-signature-hashed-subpackets sig))
         => (lambda (x)
              (unixtime (+ (cdr x)
                           (openpgp-signature-creation-time sig)))))
        (else #f)))


(define (get-openpgp-detached-signature/ascii port)
  "Read from PORT an ASCII-armored detached signature.  Return an
<openpgp-signature> record or the end-of-file object.  Raise an error if the
data read from PORT does is invalid or does not correspond to a detached
signature."
  (let-values (((data type) (read-radix-64 port)))
    (cond ((eof-object? data) data)
          ((string=? type "PGP SIGNATURE")
           (get-packet (open-bytevector-input-port data)))
          (else
           (print "expected PGP SIGNATURE" type)
           (raise (condition
                   (&openpgp-invalid-signature-error (port port))))))))

(define (verify-openpgp-signature sig keyring dataport)
  "Verify that the data read from DATAPORT matches SIG, an
<openpgp-signature>.  Fetch the public key of the issuer of SIG from KEYRING,
a keyring as returned by 'get-openpgp-keyring'.  Return two values: a status
symbol, such as 'bad-signature or 'missing-key, and additional info, such as
the issuer's OpenPGP public key extracted from KEYRING."
  (define (check key sig)
    (let*-values (((hash-algorithm) (lookup-hash-algorithm
                                     (openpgp-signature-hash-algorithm sig)))
                  ((port get-hash) (open-hash-port hash-algorithm)))
      (dump-port dataport port)

      ;; As per RFC4880 Section 5.2.4 ("Computing Signatures"), hash some of
      ;; the fields from the signature packet.
      (for-each (cut put-bytevector port <>)
                (openpgp-signature-append-data sig))
      (close-port port)

      (let* ((signature  (openpgp-signature-value sig))
             (public-key (openpgp-public-key-value key))
             (hash       (get-hash))
             (key-type   (key-type public-key))
             (data
              ;; See "(gcrypt) Cryptographic Functions".
              (sexp->canonical-sexp
               (if (eq? key-type 'ecc)
                   `(data
                     (flags eddsa)
                     (hash-algo sha512)
                     (value ,hash))
                   `(data
                     (flags ,(match key-type
                               ('rsa 'pkcs1)
                               ('dsa 'rfc6979)))
                     (hash ,(hash-algorithm-name hash-algorithm)
                           ,hash))))))
        (values (if (verify signature data public-key)
                    'good-signature
                    'bad-signature)
                key))))

  ;; TODO: Support SIGNATURE-TEXT.
  (if (= (openpgp-signature-type sig) SIGNATURE-BINARY)
      (let* ((id          (openpgp-signature-issuer-key-id sig))
             (fingerprint (openpgp-signature-issuer-fingerprint sig))
             (key         (if fingerprint
                              (lookup-key-by-fingerprint keyring fingerprint)
                              (lookup-key-by-id keyring id))))
        (if key
            (check key sig)
            (values 'missing-key (or fingerprint id))))
      (values 'unsupported-signature sig)))

(define (key-id-matches-fingerprint? key-id fingerprint)
  "Return true if KEY-ID, a number, corresponds to the low 8 bytes of
FINGERPRINT, a bytevector."
  (let* ((len (bytevector-length fingerprint))
         (low (make-bytevector 8)))
    (bytevector-copy! fingerprint (- len 8) low 0 8)
    (= (bytevector->uint low) key-id)))

(define (get-signature p)
  (define (->hex n)
    (string-hex-pad (number->string n 16)))

  (define (get-sig p pkalg)
    (cond ((= pkalg PUBLIC-KEY-RSA)
           (print "RSA signature")
           (string->canonical-sexp
            (format #f "(sig-val (rsa (s #~a#)))"
                    (->hex (get-mpi p)))))
          ((= pkalg PUBLIC-KEY-DSA)
           (print "DSA signature")
           (let ((r (get-mpi p)) (s (get-mpi p)))
             (string->canonical-sexp
              (format #f "(sig-val (dsa (r #~a#) (s #~a#)))"
                      (->hex r) (->hex s)))))
          ((= pkalg PUBLIC-KEY-EDDSA)
           (print "EdDSA signature")
           (let ((r (get-mpi/bytevector p))
                 (s (get-mpi/bytevector p)))
             ;; XXX: 'verify' fails down the road with GPG_ERR_INV_LENGTH if
             ;; we provide a 31-byte R or S below, hence the second argument
             ;; to '->hex' ensuring the MPIs are represented as two-byte
             ;; multiples, with leading zeros.
             (define (bytevector->hex bv)
               (let ((str (bytevector->base16-string bv)))
                 (if (odd? (bytevector-length bv))
                     (string-append "00" str)
                     str)))

             (string->canonical-sexp
              (format #f "(sig-val (eddsa (r #~a#) (s #~a#)))"
                      (bytevector->hex r) (bytevector->hex s)))))
          (else
           (list 'unsupported-algorithm
                 (public-key-algorithm pkalg)
                 (get-bytevector-all p)))))
  (let ((version (get-u8 p)))
    (case version
      ((3)
       (let-values (((hmlen type ctime keyid pkalg halg hashl16)
                     (get-integers p u8 u8 u32 u64 u8 u8 u16)))
         (unless (= hmlen 5)
           (raise (condition
                   (&openpgp-invalid-signature-error (port p)))))

         (print "Signature type: " type " creation time: " (unixtime ctime))
         (print "Hash algorithm: " (openpgp-hash-algorithm halg p))
         (let ((value (get-sig p pkalg)))
           (unless (port-eof? p)
             (print "Trailing data in signature: " (get-bytevector-all p)))
           (make-openpgp-signature version type
                                   (public-key-algorithm pkalg)
                                   (openpgp-hash-algorithm halg p) hashl16
                                   (list (integers->bytevector u8 type
                                                               u32 ctime))
                                   ;; Emulate hashed subpackets
                                   (list (cons 'signature-ctime ctime))
                                   ;; Unhashed subpackets
                                   (list (cons 'issuer keyid))
                                   value
                                   keyid #f))))
      ((4)
       (let*-values (((type pkalg halg) (get-integers p u8 u8 u8))
                     ((hashed-subpackets)
                      (get-bytevector-n p (get-u16 p)))
                     ((unhashed-subpackets)
                      (get-bytevector-n p (get-u16 p)))
                     ((hashl16) (get-u16 p)))
         (print "Signature type: " type)
         (print "Hash algorithm: " (openpgp-hash-algorithm halg p))
         (let ((value (get-sig p pkalg)))
           (unless (port-eof? p)
             (print "Trailing data in signature: " (get-bytevector-all p)))
           (let* ((subpacket-len (bytevector-length hashed-subpackets))
                  (append-data
                   (list
                    (integers->bytevector u8 version
                                          u8 type
                                          u8 pkalg
                                          u8 halg
                                          u16 subpacket-len)
                    hashed-subpackets
                    ;; http://www.rfc-editor.org/errata_search.php?rfc=4880
                    ;; Errata ID: 2214.
                    (integers->bytevector u8 #x04
                                          u8 #xff
                                          u32 (+ 6 subpacket-len))))
                  (unhashed-subpackets
                   (parse-subpackets unhashed-subpackets p))
                  (hashed-subpackets (parse-subpackets hashed-subpackets p))
                  (subpackets        (append hashed-subpackets
                                             unhashed-subpackets))
                  (issuer-key-id     (assoc-ref subpackets 'issuer))
                  (issuer            (assoc-ref subpackets
                                                'issuer-fingerprint)))
             (unless (or (not issuer) (not issuer-key-id)
                         (key-id-matches-fingerprint? issuer-key-id issuer))
               (print "issuer key id does not match fingerprint"
                      issuer-key-id issuer)
               (raise (condition
                       (&openpgp-invalid-signature-error (port p)))))

             (make-openpgp-signature version type
                                     (public-key-algorithm pkalg)
                                     (openpgp-hash-algorithm halg p)
                                     hashl16
                                     append-data
                                     hashed-subpackets
                                     unhashed-subpackets
                                     value
                                     issuer-key-id issuer)))))
      (else
       (print "Unsupported signature version: " version)
       'unsupported-signature-version))))

(define (parse-subpackets bv signature-port)
  (define (parse tag data)
    (let ((type (fxbit-field tag 0 7))
          (critical? (fxbit-set? tag 7)))
      (cond
       ((= type SUBPACKET-SIGNATURE-CTIME)
        (cons 'signature-ctime
              (bytevector-u32-ref data 0 (endianness big))))
       ((= type SUBPACKET-SIGNATURE-ETIME)
        (cons 'signature-etime
              (bytevector-u32-ref data 0 (endianness big))))
       ((= type SUBPACKET-TRUST-SIGNATURE)
        (cons 'trust-signature
              (bytevector-u8-ref data 0)))
       ((= type SUBPACKET-REVOCABLE)
        (cons 'revocable
              (= (bytevector-u8-ref data 0) 1)))
       ((= type SUBPACKET-KEY-ETIME)
        (cons 'key-etime
              (bytevector-u32-ref data 0 (endianness big))))
       ((= type SUBPACKET-PREFERRED-SYMMETRIC-ALGORITHMS)
        (cons 'preferred-symmetric-algorithms
              (map symmetric-key-algorithm (bytevector->u8-list data))))
       ((= type SUBPACKET-ISSUER)
        (cons 'issuer
              (bytevector-u64-ref data 0 (endianness big))))
       ((= type SUBPACKET-ISSUER-FINGERPRINT)     ;v4+ only, RFC4880bis
        (cons 'issuer-fingerprint
              (let* ((version     (bytevector-u8-ref data 0))
                     (len         (match version (4 20) (5 32)) )
                     (fingerprint (make-bytevector len)))
                (bytevector-copy! data 1 fingerprint 0 len)
                fingerprint)))
       ((= type SUBPACKET-NOTATION-DATA)
        (let ((p (open-bytevector-input-port data)))
          (let-values (((f1 nlen vlen)
                        (get-integers p u8 _ _ _ u16 u16)))
            (let* ((name (get-bytevector-n p nlen))
                   (value (get-bytevector-n p vlen)))
              (cons 'notation-data
                    (list (utf8->string name)
                          (if (fxbit-set? f1 7)
                              (utf8->string value)
                              value)))))))
       ((= type SUBPACKET-PREFERRED-HASH-ALGORITHMS)
        (cons 'preferred-hash-algorithms
              (map (cut openpgp-hash-algorithm <> signature-port)
                   (bytevector->u8-list data))))
       ((= type SUBPACKET-PREFERRED-COMPRESSION-ALGORITHMS)
        (cons 'preferred-compression-algorithms
              (map compression-algorithm (bytevector->u8-list data))))
       ((= type SUBPACKET-KEY-SERVER-PREFERENCES)
        (cons 'key-server-preferences
              (if (and (>= (bytevector-length data) 1)
                       (fxbit-set? (bytevector-u8-ref data 0) 7))
                  (list 'no-modify)
                  (list))))
       ((= type SUBPACKET-PREFERRED-KEY-SERVER)
        (cons 'preferred-key-server (utf8->string data)))
       ((= type SUBPACKET-PRIMARY-USER-ID)
        (cons 'primary-user-id (not (zero? (bytevector-u8-ref data 0)))))
       ((= type SUBPACKET-POLICY-URI)
        (cons 'policy-uri (utf8->string data)))
       ((= type SUBPACKET-KEY-FLAGS)
        (cons 'key-flags (bytevector->bitnames
                          data
                          '(certification sign-data
                                          communications-encryption
                                          storage-encryption
                                          split-key authentication
                                          group-key))))
       ((= type SUBPACKET-SIGNER-USER-ID)
        (cons 'signer-user-id (utf8->string data)))
       ((= type SUBPACKET-REASON-FOR-REVOCATION)
        (let* ((p (open-bytevector-input-port data))
               (revocation-code (get-u8 p)))
          (cons 'reason-for-revocation
                (list revocation-code
                      (if (port-eof? p)
                          ""
                          (utf8->string (get-bytevector-all p)))))))
       ((= type SUBPACKET-FEATURES)
        (cons 'features (bytevector->bitnames
                         data '(modification-detection))))
       ((= type SUBPACKET-EMBEDDED-SIGNATURE)
        (cons 'embedded-signature
              (get-signature (open-bytevector-input-port data))))
       (else
        ;; Unknown subpacket type.  If it is critical, then the signature
        ;; should be considered invalid.
        (print "Unknown subpacket type: " type)
        (if critical?
            (raise (condition
                    (&openpgp-unrecognized-packet-error
                     (type type)
                     (port signature-port))))
            (list 'unsupported-subpacket type data))))))

  (let ((p (open-bytevector-input-port bv)))
    (let lp ((subpackets '()))
      ;; In case of multiple subpackets of the same type, the last
      ;; one should be used. Therefore the list is not reversed
      ;; here.
      (if (port-eof? p)
          (reverse subpackets)
          (let* ((len (- (get-v4-length p) 1))
                 (tag (get-u8 p))
                 (sp (parse tag (get-bytevector-n p len))))
            (print "#;Subpacket " sp)
            (lp (cons sp subpackets)))))))

;;; Public keys


(define (openpgp-public-key-id k)
  (let ((bv (openpgp-public-key-fingerprint k)))
    (bytevector-u64-ref bv
                        (- (bytevector-length bv) 8)
                        (endianness big))))

(define (get-public-key p subkey?)
  (define (fingerprint p)
    (let ((len (port-position p)))
      (set-port-position! p 0)
      (let-values (((sha1-port get)
                    (open-hash-port (hash-algorithm sha1))))
        (put-u8 sha1-port #x99)
        (put-u16 sha1-port len)
        (dump-port p sha1-port)
        (close-port sha1-port)
        (get))))
  (define (get-key p alg)
    (define (->hex n)
      (string-hex-pad (number->string n 16)))

    (cond ((= alg PUBLIC-KEY-RSA)
           (print "Public RSA key")
           (let* ((n (get-mpi p)) (e (get-mpi p)))
             (string->canonical-sexp
              (format #f "(public-key (rsa (n #~a#) (e #~a#)))"
                      (->hex n) (->hex e)))))
          ((= alg PUBLIC-KEY-DSA)
           (print "Public DSA key")
           (let* ((p* (get-mpi p)) (q (get-mpi p))
                  (g (get-mpi p)) (y (get-mpi p)))
             (string->canonical-sexp
              (format #f "(public-key (dsa (p #~a#)(q #~a#)(g #~a#)(y #~a#)))"
                      (->hex p*) (->hex q) (->hex g) (->hex y)))))
          #;
          ((= alg PUBLIC-KEY-ELGAMAL-ENCRYPT-ONLY) ; ; ; ;
          (print "Public El-Gamal Key")         ; ; ; ;
          (let* ((p* (get-mpi p)) (g (get-mpi p)) (y (get-mpi p))) ; ; ; ;
          (make-public-elgamal-key p* g y)))
          ((= alg PUBLIC-KEY-EDDSA)
           ;; See
           ;; <https://tools.ietf.org/html/draft-koch-eddsa-for-openpgp-04>
           ;; and openpgp-oid.c in GnuPG.
           (print "Public EdDSA key")
           (let* ((len     (get-u8 p))
                  (oid     (bytevector->uint (get-bytevector-n p len)))
                  (q (get-mpi p)))
             (define curve
               (match oid
                 (#x2b06010401da470f01   'Ed25519)
                 (#x2b060104019755010501 'Curve25519)))

             (string->canonical-sexp
              (format #f "(public-key (ecc (curve ~a)(flags ~a)(q #~a#)))"
                      curve
                      (if (eq? curve 'Curve25519) 'djb-tweak 'eddsa)
                      (->hex q)))))
          (else
           (list 'unsupported-algorithm           ;FIXME: throw
                 (public-key-algorithm alg)
                 (get-bytevector-all p)))))
  (let ((version (get-u8 p)))
    (case version
      ((4)
       (let-values (((ctime alg) (get-integers p u32 u8)))
         (print "Key creation time: " (unixtime ctime))
         (let ((key (get-key p alg)))
           (unless (port-eof? p)
             ;; Probably an error? Gonna cause trouble anyway.
             (print "Trailing data in public key: " (get-bytevector-all p)))
           (let ((digest (fingerprint p)))
             (make-openpgp-public-key version subkey? ctime key
                                      digest)))))
      (else
       (print "Unsupported public key version: " version)
       'unsupported-public-key-version))))

(define (openpgp-public-key-primary? key)
  (and (openpgp-public-key? key)
       (not (openpgp-public-key-subkey? key))))

;;; User IDs and User attributes


(define-record-type <openpgp-user-id>
  (make-openpgp-user-id value unparsed)
  openpgp-user-id?
  (value     openpgp-user-id-value)
  (unparsed  openpgp-user-id-unparsed))

(define (get-user-id p len)
  (let ((unparsed (get-bytevector-n p len)))
    (make-openpgp-user-id (utf8->string unparsed) unparsed)))

(define-record-type <openpgp-user-attribute>
  (make-openpgp-user-attribute unparsed)
  openpgp-user-attribute?
  (unparsed   openpgp-user-attribute-unparsed))

(define (get-user-attribute p len)
  (let ((bv (get-bytevector-n p len)))
    ;; TODO: bv contains subpackets. Type 1 is JFIF.
    (make-openpgp-user-attribute bv)))


;;; Keyring management

(define-record-type <openpgp-keyring>
  (openpgp-keyring ids fingerprints)
  openpgp-keyring?
  (ids            openpgp-keyring-ids)        ;vhash mapping key id to packets
  (fingerprints   openpgp-keyring-fingerprints)) ;mapping fingerprint to packets

(define* (keyring-insert key keyring #:optional (packets '()))
  "Insert the KEY/PACKETS association into KEYRING and return the resulting
keyring.  PACKETS typically contains KEY, an <openpgp-public-key>, alongside
with additional <openpgp-public-key> records for sub-keys, <openpgp-user-id>
records, and so on."
  (openpgp-keyring (vhash-consv (openpgp-public-key-id key)
                                (cons key packets)
                                (openpgp-keyring-ids keyring))
                   (vhash-cons (openpgp-public-key-fingerprint key)
                               (cons key packets)
                               (openpgp-keyring-fingerprints keyring))))

(define (lookup-key-by-id keyring id)
  "Return two values: the first key with ID in KEYRING, and a list of
associated packets (user IDs, signatures, etc.).  Return #f and the empty list
of ID was not found.  ID must be the 64-bit key ID of the key, an integer."
  (match (vhash-assv id (openpgp-keyring-ids keyring))
    ((_ key packets ...) (values key packets))
    (#f (values #f '()))))

(define (lookup-key-by-fingerprint keyring fingerprint)
  "Return two values: the key with FINGERPRINT in KEYRING, and a list of
associated packets (user IDs, signatures, etc.).  Return #f and the empty list
of FINGERPRINT was not found.  FINGERPRINT must be a bytevector."
  (match (vhash-assoc fingerprint (openpgp-keyring-fingerprints keyring))
    ((_ key packets ...) (values key packets))
    (#f (values #f '()))))

;; Reads a keyring from the binary input port p. It must not be
;; ASCII armored.

(define %empty-keyring
  ;; The empty keyring.
  (openpgp-keyring vlist-null vlist-null))

(define* (get-openpgp-keyring port
                              #:optional (keyring %empty-keyring)
                              #:key (limit -1))
  "Read from PORT an OpenPGP keyring in binary format; return a keyring based
on all the OpenPGP primary keys that were read.  The returned keyring
complements KEYRING.  LIMIT is the maximum number of keys to read, or -1 if
there is no limit."
  (let lp ((pkt (get-packet port))
           (limit limit)
           (keyring keyring))
    (print "#;key " pkt)
    (cond ((or (zero? limit) (eof-object? pkt))
           keyring)
          ((openpgp-public-key-primary? pkt)
           ;; Read signatures, user id's, subkeys
           (let lp* ((pkt  (get-packet port))
                     (pkts (list pkt))
                     (keys (list pkt)))
             (print "#;keydata " pkt)
             (cond ((or (eof-object? pkt)
                        (eq? pkt 'unsupported-public-key-version)
                        (openpgp-public-key-primary? pkt))
                    ;; KEYRING is indexed by key-id.  Key ids for both the
                    ;; primary key and subkeys all point to the list of
                    ;; packets.
                    (lp pkt
                        (- limit 1)
                        (fold (cute keyring-insert <> <> (reverse pkts))
                              keyring keys)))
                   ((openpgp-public-key? pkt)     ;subkey
                    (lp* (get-packet port) (cons pkt pkts)
                         (cons pkt keys)))
                   (else
                    (lp* (get-packet port) (cons pkt pkts) keys)))))
          (else
           ;; Skip until there's a primary key. Ignore errors...
           (lp (get-packet port) limit keyring)))))


;;;
;;; Radix-64 (RFC4880).
;;;

(define (crc24 bv)
  "Compute a CRC24 as described in RFC4880, Section 6.1."
  ;; We used to have it implemented in Scheme but the C version here makes
  ;; 'load-keyring-from-reference' 18% faster when loading the 72
  ;; ASCII-armored files of today's Guix keyring.
  (bytevector->uint (bytevector-hash bv (hash-algorithm crc24-rfc2440))))

(define %begin-block-prefix "-----BEGIN ")
(define %begin-block-suffix "-----")

(define %end-block-prefix "-----END ")
(define %end-block-suffix "-----")

(define (read-radix-64 port)
  "Read from PORT an ASCII-armored Radix-64 stream, decode it, and return the
result as a bytevector as well as the type, a string such as \"PGP MESSAGE\".
Return #f if PORT does not contain a valid Radix-64 stream, and the
end-of-file object if the Radix-64 sequence was truncated."
  ;; This is the same as 'get-delimited-base64', except that it implements the
  ;; CRC24 check.
  (define (skip-headers port)
    ;; Skip the Radix-64 "armor headers".
    (match (read-line port)
      ((? eof-object? eof) eof)
      ((= string-trim-both "") "")
      (_  (skip-headers port))))

  (let ((line (string-trim-right (read-line port))))
    (if (and (string-prefix? %begin-block-prefix line)
             (string-suffix? %begin-block-suffix line))
        (let* ((kind (string-drop-right
                      (string-drop line (string-length %begin-block-prefix))
                      (string-length %begin-block-suffix)))
               (end  (string-append %end-block-prefix kind
                                    %end-block-suffix)))
          (skip-headers port)
          (let loop ((lines '()))
            (let ((line (read-line port)))
              (match line
                ((? eof-object? eof)
                 (values eof kind))
                ((= string-trim-both "")
                 (loop lines))
                ((= string-trim-both str)
                 (if (string=? str end)
                     (match lines
                       ((crc lines ...)
                        ;; The last line should be the CRC, starting with an
                        ;; "=" sign.
                        (let ((crc  (and (string-prefix? "=" crc)
                                         (base64-decode (string-drop crc 1))))
                              (data (base64-decode
                                     (string-concatenate-reverse lines))))
                          (if (and crc (= (bytevector->uint crc) (crc24 data)))
                              (values data kind)
                              (values #f kind))))
                       (_
                        (values #f kind)))
                     (loop (cons str lines))))))))
        (values #f #f))))

(define (string->openpgp-packet str)
  "Read STR, an ASCII-armored OpenPGP packet, and return the corresponding
OpenPGP record."
  (get-packet
   (open-bytevector-input-port (call-with-input-string str read-radix-64))))
