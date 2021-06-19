;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages certs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tls))

(define certdata2pem
  (let ((revision "1")
        (commit "4c576f350f44186d439179f63d5be19f710a73f5"))
    (package
      (name "certdata2pem")
      (version "0.0.0")                   ;no version
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://raw.githubusercontent.com/sabotage-linux/sabotage/"
                      commit "/KEEP/certdata2pem.c"))
                (sha256
                 (base32
                  "1rywp29q4l1cs2baplkbcravxqs4kw2cys4yifhfznbc210pskq6"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (delete 'configure)
                    (replace 'build
                      (lambda _
                        (invoke ,(cc-for-target) "certdata2pem.c"
                                "-o" "certdata2pem")))
                    (delete 'check)     ;no test suite
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let ((out (assoc-ref outputs "out")))
                          (install-file "certdata2pem"
                                        (string-append out "/bin"))))))))
      (home-page "https://github.com/sabotage-linux/")
      (synopsis "Utility to split TLS certificates data into multiple PEM files")
      (description "This is a C version of the certdata2pem Python utility
that was originally contributed to Debian.")
      (license license:isc))))

(define-public nss-certs
  (package
    (name "nss-certs")
    (version "3.67")
    (source (origin
              (method url-fetch)
              (uri (let ((version-with-underscores
                          (string-join (string-split version #\.) "_")))
                     (string-append
                      "https://ftp.mozilla.org/pub/mozilla.org/security/nss/"
                      "releases/NSS_" version-with-underscores "_RTM/src/"
                      "nss-" version ".tar.gz")))
              (sha256
               (base32
                "0zyfi27lbdz1bmk9dmsivcya4phx25rzlxqcnjab69yd928rlm7n"))))
    (build-system gnu-build-system)
    (outputs '("out"))
    (native-inputs
     `(("certdata2pem" ,certdata2pem)
       ("openssl" ,openssl)))
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (rnrs io ports)
                  (srfi srfi-26))
       #:phases
       (modify-phases
           (map (cut assq <> %standard-phases)
                '(set-paths install-locale unpack))
         (add-after 'unpack 'install
           (lambda _
             (let ((certsdir (string-append %output "/etc/ssl/certs/")))
               (with-directory-excursion "nss/lib/ckfw/builtins/"
                 (unless (file-exists? "blacklist.txt")
                   (call-with-output-file "blacklist.txt" (const #t)))
                 ;; Extract selected single certificates from blob.
                 (invoke "certdata2pem")
                 ;; Copy .crt files into the output.
                 (for-each (cut install-file <> certsdir)
                           (find-files "." ".*\\.crt$")))
               (invoke "openssl" "rehash" certsdir)))))))
    (synopsis "CA certificates from Mozilla")
    (description
     "This package provides certificates for Certification Authorities (CA)
taken from the NSS package and thus ultimately from the Mozilla project.")
    (home-page "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSS")
    (license license:mpl2.0)))

(define-public le-certs
  (package
    (name "le-certs")
    (version "1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((root-rsa (assoc-ref %build-inputs "isrgrootx1.pem"))
               (root-ecdsa (assoc-ref %build-inputs "isrgrootx2.pem"))
               (intermediate-rsa (assoc-ref %build-inputs "letsencryptauthorityr3.pem"))
               (intermediate-ecdsa (assoc-ref %build-inputs "letsencryptauthoritye1.pem"))
               (backup-rsa (assoc-ref %build-inputs "letsencryptauthorityr4.pem"))
               (backup-ecdsa (assoc-ref %build-inputs "letsencryptauthoritye2.pem"))
               (out (string-append (assoc-ref %outputs "out") "/etc/ssl/certs"))
               (openssl (assoc-ref %build-inputs "openssl"))
               (perl (assoc-ref %build-inputs "perl")))
           (mkdir-p out)
           (for-each
             (lambda (cert)
               (copy-file cert (string-append out "/"
                                              (strip-store-file-name cert))))
             (list root-rsa root-ecdsa
                   intermediate-rsa intermediate-ecdsa
                   backup-rsa backup-ecdsa))

           ;; Create hash symlinks suitable for OpenSSL ('SSL_CERT_DIR' and
           ;; similar.)
           (chdir (string-append %output "/etc/ssl/certs"))
           (invoke (string-append perl "/bin/perl")
                   (string-append openssl "/bin/c_rehash")
                   ".")))))
    (native-inputs
     `(("openssl" ,openssl)
       ("perl" ,perl)))                           ;for 'c_rehash'
    (inputs
     `(; The Let's Encrypt root certificate, "ISRG Root X1".
       ("isrgrootx1.pem"
        ,(origin
           (method url-fetch)
           (uri "https://letsencrypt.org/certs/isrgrootx1.pem")
           (sha256
            (base32
             "1la36n2f31j9s03v847ig6ny9lr875q3g7smnq33dcsmf2i5gd92"))))
      ; Upcoming ECDSA Let's Encrypt root certificate, "ISRG Root X2"
      ; Let's Encrypt describes it as "Active, limited availability"
      ("isrgrootx2.pem"
        ,(origin
           (method url-fetch)
           (uri "https://letsencrypt.org/certs/isrg-root-x2.pem")
           (sha256
            (base32
             "04xh8912nwkghqydbqvvmslpqbcafgxgjh9qnn0z2vgy24g8hgd1"))))
      ;; "Let’s Encrypt Authority R3", the active Let's Encrypt intermediate
      ;; RSA certificate.
      ("letsencryptauthorityr3.pem"
       ,(origin
          (method url-fetch)
          (uri "https://letsencrypt.org/certs/lets-encrypt-r3.pem")
          (sha256
           (base32
            "0clxry49rx6qd3pgbzknpgzywbg3j96zy0227wwjnwivqj7inzhp"))))
      ;; "Let’s Encrypt Authority E1", the active Let's Encrypt intermediate
      ;; ECDSA certificate.
      ("letsencryptauthoritye1.pem"
       ,(origin
          (method url-fetch)
          (uri "https://letsencrypt.org/certs/lets-encrypt-e1.pem")
          (sha256
           (base32
            "1zwrc6dlk1qig0z23x6x7fib14rrw41ccbf2ds0rw75zccc59xx0"))))
      ;; "Let’s Encrypt Authority R4", the backup Let's Encrypt intermediate
      ;; RSA certificate.  This will be used for disaster recovery and will only be
      ;; used should Let's Encrypt lose the ability to issue with "Let’s
      ;; Encrypt Authority R3".
      ("letsencryptauthorityr4.pem"
       ,(origin
          (method url-fetch)
          (uri "https://letsencrypt.org/certs/lets-encrypt-r4.pem")
          (sha256
           (base32
            "09bzxzbwb9x2xxan3p1fyj1pi2p5yks0879gwz5f28y9mzq8vmd8"))))
      ;; "Let’s Encrypt Authority E2", the backup Let's Encrypt intermediate
      ;; ECDSA certificate.  This will be used for disaster recovery and will
      ;; only be used should Let's Encrypt lose the ability to issue with "Let’s
      ;; Encrypt Authority E1".
      ("letsencryptauthoritye2.pem"
       ,(origin
          (method url-fetch)
          (uri "https://letsencrypt.org/certs/lets-encrypt-e2.pem")
          (sha256
           (base32
            "1wfmsa29lyi9dkh6xdcamb2rhkp5yl2ppnsgrzcrjl5c7gbqh9ml"))))))
    (home-page "https://letsencrypt.org/certificates/")
    (synopsis "Let's Encrypt root and intermediate certificates")
    (description "This package provides a certificate store containing only the
Let's Encrypt root and intermediate certificates.  It is intended to be used
within Guix.")
    (license license:public-domain)))
