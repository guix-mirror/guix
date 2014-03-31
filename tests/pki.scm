;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-pki)
  #:use-module (guix pki)
  #:use-module (guix pk-crypto)
  #:use-module (guix hash)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-64))

;; Test the (guix pki) module.

(define %public-key
  (call-with-input-file %public-key-file
    (compose string->canonical-sexp get-string-all)))

(define %secret-key
  (call-with-input-file %private-key-file
    (compose string->canonical-sexp get-string-all)))

(define %alternate-secret-key
  (string->canonical-sexp
   "
  (key-data
   (public-key
    (rsa
     (n #00FDBF170366AC43B7D95CF9085565C566FB1F21B17C0A36E68F35ABB500E7851E00B40D7B04C8CD25903371F38E4C298FACEFFC4C97E913B536A0672BAF99D04515AE98A1A56627CD7EB02502FCFBEEA21AF13CC1A853192AD6409B9EFBD9F549BDE32BD890AE01F9A221E81FEE1C407090550647790E0D60775B855E181C2FB5#)
     (e #010001#)))
   (private-key
    (rsa
     (n #00FDBF170366AC43B7D95CF9085565C566FB1F21B17C0A36E68F35ABB500E7851E00B40D7B04C8CD25903371F38E4C298FACEFFC4C97E913B536A0672BAF99D04515AE98A1A56627CD7EB02502FCFBEEA21AF13CC1A853192AD6409B9EFBD9F549BDE32BD890AE01F9A221E81FEE1C407090550647790E0D60775B855E181C2FB5#)
     (e #010001#)
     (d #2790250C2E74C2FD361A99288BBA19B878048F5A0F333F829CC71B3DD64582DB9DF3F4DB1EB0994DD7493225EDA4A1E1492F44D903617FA5643E47BFC7BA157EF48B492AB51229916B02DDBDA0E7DBC7B35A6B8332AB463DC61951CA694551A9760F5A836A375D39E3EA8F2C502A3B5D89CB8777A809B75D603BE7511CEB74E9#)
     (p #00FE15B1751E1C31125B724FF37462F9476239A2AFF4192FAB1550F76928C8D02407F4F5EFC83F7A0AF51BD93399DDC06A4B54DFA60A7079F160A9F618C0148AD9#)
     (q #00FFA8BE7005AAB7401B0926CD9D6AC30BC9BE7D12C8737C9438498A999F56BE9F5EA98B4D7F5364BEB6D550A5AEDDE34C1EC152C9DAF61A97FDE71740C73BAA3D#)
     (u #00FD4050EF4F31B41EC81C28E18D205DFFB3C188F15D8BBA300E30AD8B5C4D3E392EFE10269FC115A538B19F4025973AB09B6650A7FF97DA833FB726F3D8819319#))))"))

(test-begin "pki")

(test-assert "current-acl"
  (not (not (member (canonical-sexp->sexp %public-key)
                    (map canonical-sexp->sexp
                         (acl->public-keys (current-acl)))))))

(test-assert "authorized-key? public-key current-acl"
  (authorized-key? %public-key))

(test-assert "authorized-key? public-key empty-acl"
  (not (authorized-key? %public-key (public-keys->acl '()))))

(test-assert "authorized-key? public-key singleton"
  (authorized-key? %public-key (public-keys->acl (list %public-key))))

(test-assert "signature-case valid-signature"
  (let* ((hash (sha256 #vu8(1 2 3)))
         (data (bytevector->hash-data hash #:key-type (key-type %public-key)))
         (sig  (signature-sexp data %secret-key %public-key)))
   (signature-case (sig hash (public-keys->acl (list %public-key)))
     (valid-signature #t)
     (else #f))))

(test-eq "signature-case invalid-signature" 'i
  (let* ((hash (sha256 #vu8(1 2 3)))
         (data (bytevector->hash-data hash #:key-type (key-type %public-key)))
         (sig  (signature-sexp data %alternate-secret-key %public-key)))
    (signature-case (sig hash (public-keys->acl (list %public-key)))
      (valid-signature 'v)
      (invalid-signature 'i)
      (hash-mismatch 'm)
      (unauthorized-key 'u)
      (corrupt-signature 'c))))

(test-eq "signature-case hash-mismatch" 'm
  (let* ((hash (sha256 #vu8(1 2 3)))
         (data (bytevector->hash-data hash #:key-type (key-type %public-key)))
         (sig  (signature-sexp data %secret-key %public-key)))
    (signature-case (sig (sha256 #vu8())
                         (public-keys->acl (list %public-key)))
      (valid-signature 'v)
      (invalid-signature 'i)
      (hash-mismatch 'm)
      (unauthorized-key 'u)
      (corrupt-signature 'c))))

(test-eq "signature-case unauthorized-key" 'u
  (let* ((hash (sha256 #vu8(1 2 3)))
         (data (bytevector->hash-data hash #:key-type (key-type %public-key)))
         (sig  (signature-sexp data %secret-key %public-key)))
    (signature-case (sig hash (public-keys->acl '()))
      (valid-signature 'v)
      (invalid-signature 'i)
      (hash-mismatch 'm)
      (unauthorized-key 'u)
      (corrupt-signature 'c))))

(test-eq "signature-case corrupt-signature" 'c
  (let* ((hash (sha256 #vu8(1 2 3)))
         (sig  (string->canonical-sexp "(w tf)")))
    (signature-case (sig hash (public-keys->acl (list %public-key)))
      (valid-signature 'v)
      (invalid-signature 'i)
      (hash-mismatch 'm)
      (unauthorized-key 'u)
      (corrupt-signature 'c))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))
