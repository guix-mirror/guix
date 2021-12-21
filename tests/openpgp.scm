;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (tests-openpgp)
  #:use-module (guix openpgp)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt pk-crypto)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-71))

(define %radix-64-sample
  ;; Example of Radix-64 encoding from Section 6.6 of RFC4880.
  "\
-----BEGIN PGP MESSAGE-----
Version: OpenPrivacy 0.99

yDgBO22WxBHv7O8X7O/jygAEzol56iUKiXmV+XmpCtmpqQUKiQrFqclFqUDBovzS
vBSFjNSiVHsuAA==
=njUN
-----END PGP MESSAGE-----\n")

(define %radix-64-sample/crc-mismatch
  ;; This time with a wrong CRC24 value.
  "\
-----BEGIN PGP MESSAGE-----

yDgBO22WxBHv7O8X7O/jygAEzol56iUKiXmV+XmpCtmpqQUKiQrFqclFqUDBovzS
vBSFjNSiVHsuAA==
=AAAA
-----END PGP MESSAGE-----\n")

(define %binary-sample
  ;; Same message as %radix-64-sample, decoded into bytevector.
  (base16-string->bytevector
  "c838013b6d96c411efecef17ecefe3ca0004ce8979ea250a897995f979a9\
0ad9a9a9050a890ac5a9c945a940c1a2fcd2bc14858cd4a2547b2e00"))

(define %civodul-fingerprint
  "3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5")

(define %civodul-key-id #x090B11993D9AEBB5)       ;civodul.pub

#|
Test keys in ./tests/keys.  They were generated in a container along these lines:
  guix environment -CP --ad-hoc gnupg pinentry coreutils
then, within the container:
  mkdir ~/.gnupg && chmod -R og-rwx ~/.gnupg
  gpg --batch --passphrase '' --quick-gen-key '<example@example.com>' ed25519
  gpg --armor --export example@example.com
  gpg --armor --export-secret-key example@example.com
  # echo pinentry-program ~/.guix-profile/bin/pinentry-curses > ~/.gnupg/gpg-agent.conf
or similar.
|#
(define %rsa-key-id      #xAE25DA2A70DEED59)      ;rsa.pub
(define %dsa-key-id      #x587918047BE8BD2C)      ;dsa.pub
(define %ed25519-key-id  #x771F49CBFAAE072D)      ;ed25519.pub

(define %rsa-key-fingerprint
  (base16-string->bytevector
   (string-downcase "385F86CFC86B665A5C165E6BAE25DA2A70DEED59")))
(define %dsa-key-fingerprint
  (base16-string->bytevector
   (string-downcase "2884A980422330A4F33DD97F587918047BE8BD2C")))
(define %ed25519-key-fingerprint
  (base16-string->bytevector
   (string-downcase "44D31E21AF7138F9B632280A771F49CBFAAE072D")))


;;; The following are detached signatures created commands like:
;;;    echo 'Hello!' | gpg -sba --digest-algo sha512
;;; They are detached (no PACKET-ONE-PASS-SIGNATURE) and uncompressed.

(define %hello-signature/rsa
  ;; Signature of the ASCII string "Hello!\n".
  "\
-----BEGIN PGP SIGNATURE-----

iQEzBAABCAAdFiEEOF+Gz8hrZlpcFl5rriXaKnDe7VkFAl4SRF0ACgkQriXaKnDe
7VlIyQf/TU5rGUK42/C1ULoWvvm25Mjwh6xxoPPkuBxvos8bE6yKr/vJZePU3aSE
mjbVFcO7DioxHMqLd49j803bUtdllJVU18ex9MkKbKjapkgEGkJsuTTzqyONprgk
7xtZGBWuxkP1M6hJICJkA3Ys+sTdKalux/pzr5OWAe+gxytTF/vr/EyJzdmBxbJv
/fhd1SeVIXSw4c5gf2Wcvcgfy4N5CiLaUb7j4646KBTvDvmUMcDZ+vmKqC/XdQeQ
PrjArGKt40ErVd98fwvNHZnw7VQMx0A3nL3joL5g7/RckDOUb4mqKoqLsLd0wPHP
y32DiDUY9s3sy5OMzX4Y49em8vxvlg==
=ASEm
-----END PGP SIGNATURE-----")


(define %hello-signature/dsa
  "\
-----BEGIN PGP SIGNATURE-----

iHUEABEIAB0WIQQohKmAQiMwpPM92X9YeRgEe+i9LAUCXhJFpQAKCRBYeRgEe+i9
LDAaAQC0lXPQepvZBANAUtRLMZuOwL9NQPkfhIwUXtLEBBzyFQD/So8DcybXpRBi
JKOiyAQQjMs/GJ6qMEQpRAhyyJRAock=
=iAEc
-----END PGP SIGNATURE-----")


(define %hello-signature/ed25519/sha256           ;digest-algo: sha256
  "\
-----BEGIN PGP SIGNATURE-----

iHUEABYIAB0WIQRE0x4hr3E4+bYyKAp3H0nL+q4HLQUCXqRADAAKCRB3H0nL+q4H
LUImAP9/foaSjPFC/MSr52LNV5ROSL9haea4jPpUP+N6ViFGowEA+AE/xpXPIqsz
R6CdxMevURuqUpqQ7rHeiMmdUepeewU=
=tLXy
-----END PGP SIGNATURE-----")

(define %hello-signature/ed25519/sha512           ;digest-algo: sha512
  "\
-----BEGIN PGP SIGNATURE-----

iHUEABYKAB0WIQRE0x4hr3E4+bYyKAp3H0nL+q4HLQUCXqRAGgAKCRB3H0nL+q4H
LTeKAP0S8LiiosJXOARlYNdhfGw9j26lHrbwJh5CORGlaqqIJAEAoMYcmtNa2b6O
inlEwB/KQM88O9RwA8xH7X5a0rodOw4=
=68r/
-----END PGP SIGNATURE-----")

(define %hello-signature/ed25519/sha1             ;digest-algo: sha1
  "\
-----BEGIN PGP SIGNATURE-----

iHUEABYCAB0WIQRE0x4hr3E4+bYyKAp3H0nL+q4HLQUCXqRALQAKCRB3H0nL+q4H
LdhEAQCfkdYhIVRa43oTNw9EL/TDFGQjXSHNRFVU0ktjkWbkQwEAjIXhvj2sqy79
Pz7oopeN72xgggYUNT37ezqN3MeCqw0=
=AE4G
-----END PGP SIGNATURE-----")


(test-begin "openpgp")

(test-equal "read-radix-64"
  '(#t "PGP MESSAGE")
  (let-values (((data type)
                (call-with-input-string %radix-64-sample read-radix-64)))
    (list (bytevector? data) type)))

(test-equal "read-radix-64, CRC mismatch"
  '(#f "PGP MESSAGE")
  (call-with-values
      (lambda ()
        (call-with-input-string %radix-64-sample/crc-mismatch
          read-radix-64))
    list))

(test-assert "port-ascii-armored?, #t"
  (call-with-input-string %radix-64-sample port-ascii-armored?))

(test-assert "port-ascii-armored?, #f"
  (not (port-ascii-armored? (open-bytevector-input-port %binary-sample))))

(test-assert "get-openpgp-keyring"
  (let* ((key (search-path %load-path "tests/keys/civodul.pub"))
         (keyring (get-openpgp-keyring
                   (open-bytevector-input-port
                    (call-with-input-file key read-radix-64)))))
    (let-values (((primary packets)
                  (lookup-key-by-id keyring %civodul-key-id)))
      (let ((fingerprint (openpgp-public-key-fingerprint primary)))
        (and (= (openpgp-public-key-id primary) %civodul-key-id)
             (not (openpgp-public-key-subkey? primary))
             (string=? (openpgp-format-fingerprint fingerprint)
                       %civodul-fingerprint)
             (string=? (openpgp-user-id-value (find openpgp-user-id? packets))
                       "Ludovic Courtès <ludo@gnu.org>")
             (eq? (lookup-key-by-fingerprint keyring fingerprint)
                  primary))))))

(test-equal "get-openpgp-detached-signature/ascii"
  (list `(,%dsa-key-id ,%dsa-key-fingerprint dsa sha256)
        `(,%rsa-key-id ,%rsa-key-fingerprint rsa sha256)
        `(,%ed25519-key-id ,%ed25519-key-fingerprint eddsa sha256)
        `(,%ed25519-key-id ,%ed25519-key-fingerprint eddsa sha512)
        `(,%ed25519-key-id ,%ed25519-key-fingerprint eddsa sha1))
  (map (lambda (str)
         (let ((signature (get-openpgp-detached-signature/ascii
                           (open-input-string str))))
           (list (openpgp-signature-issuer-key-id signature)
                 (openpgp-signature-issuer-fingerprint signature)
                 (openpgp-signature-public-key-algorithm signature)
                 (openpgp-signature-hash-algorithm signature))))
       (list %hello-signature/dsa
             %hello-signature/rsa
             %hello-signature/ed25519/sha256
             %hello-signature/ed25519/sha512
             %hello-signature/ed25519/sha1)))

(test-equal "verify-openpgp-signature, missing key"
  `(missing-key ,%rsa-key-fingerprint)
  (let* ((keyring   (get-openpgp-keyring (%make-void-port "r")))
         (signature (string->openpgp-packet %hello-signature/rsa)))
    (let-values (((status key)
                  (verify-openpgp-signature signature keyring
                                            (open-input-string "Hello!\n"))))
      (list status key))))

(test-equal "verify-openpgp-signature, good signatures"
  `((good-signature ,%rsa-key-id)
    (good-signature ,%dsa-key-id)
    (good-signature ,%ed25519-key-id)
    (good-signature ,%ed25519-key-id)
    (good-signature ,%ed25519-key-id))
  (map (lambda (key signature)
         (let* ((key       (search-path %load-path key))
                (keyring   (get-openpgp-keyring
                            (open-bytevector-input-port
                             (call-with-input-file key read-radix-64))))
                (signature (string->openpgp-packet signature)))
           (let-values (((status key)
                         (verify-openpgp-signature signature keyring
                                                   (open-input-string "Hello!\n"))))
             (list status (openpgp-public-key-id key)))))
       (list "tests/keys/rsa.pub" "tests/keys/dsa.pub"
             "tests/keys/ed25519.pub"
             "tests/keys/ed25519.pub"
             "tests/keys/ed25519.pub")
       (list %hello-signature/rsa %hello-signature/dsa
             %hello-signature/ed25519/sha256
             %hello-signature/ed25519/sha512
             %hello-signature/ed25519/sha1)))

(test-equal "verify-openpgp-signature, bad signature"
  `((bad-signature ,%rsa-key-id)
    (bad-signature ,%dsa-key-id)
    (bad-signature ,%ed25519-key-id)
    (bad-signature ,%ed25519-key-id)
    (bad-signature ,%ed25519-key-id))
  (let ((keyring (fold (lambda (key keyring)
                         (let ((key (search-path %load-path key)))
                           (get-openpgp-keyring
                            (open-bytevector-input-port
                             (call-with-input-file key read-radix-64))
                            keyring)))
                       %empty-keyring
                       '("tests/keys/rsa.pub" "tests/keys/dsa.pub"
                         "tests/keys/ed25519.pub" "tests/keys/ed25519.pub"
                         "tests/keys/ed25519.pub"))))
    (map (lambda (signature)
           (let ((signature (string->openpgp-packet signature)))
             (let-values (((status key)
                           (verify-openpgp-signature signature keyring
                                                     (open-input-string "What?!"))))
               (list status (openpgp-public-key-id key)))))
         (list %hello-signature/rsa %hello-signature/dsa
               %hello-signature/ed25519/sha256
               %hello-signature/ed25519/sha512
               %hello-signature/ed25519/sha1))))

(test-end "openpgp")
