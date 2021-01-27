;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages nss)
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
                      "https://github.com/sabotage-linux/sabotage/blob/"
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
                        (invoke "gcc" "certdata2pem.c" "-o" "certdata2pem")))
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
    (version (package-version nss))
    (source (package-source nss))
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
               (with-directory-excursion "lib/ckfw/builtins/"
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
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((root (assoc-ref %build-inputs "isrgrootx1.pem"))
               (intermediate (assoc-ref %build-inputs "letsencryptauthorityx3.pem"))
               (backup (assoc-ref %build-inputs "letsencryptauthorityx4.pem"))
               (out (string-append (assoc-ref %outputs "out") "/etc/ssl/certs"))
               (openssl (assoc-ref %build-inputs "openssl"))
               (perl (assoc-ref %build-inputs "perl")))
           (mkdir-p out)
           (for-each
             (lambda (cert)
               (copy-file cert (string-append out "/"
                                              (strip-store-file-name cert))))
             (list root intermediate backup))

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
       ;; "Let’s Encrypt Authority X3", the active Let's Encrypt intermediate
       ;; certificate.
       ("letsencryptauthorityx3.pem"
        ,(origin
           (method url-fetch)
           (uri "https://letsencrypt.org/certs/letsencryptauthorityx3.pem")
           (sha256
            (base32
             "100lxxvqv4fj563bm03zzk5r36hq5jx9nnrajzs38g825c5k0cg2"))))
       ;; "Let’s Encrypt Authority X4", the backup Let's Encrypt intermediate
       ;; certificate.  This will be used for disaster recovery and will only be
       ;; used should Let's Encrypt lose the ability to issue with "Let’s
       ;; Encrypt Authority X3".
       ("letsencryptauthorityx4.pem"
        ,(origin
           (method url-fetch)
           (uri "https://letsencrypt.org/certs/letsencryptauthorityx4.pem")
           (sha256
            (base32
             "0d5256gwf73drq6q6jala28rfzhrgbk5pjfq27vc40ly91pdyh8m"))))))
    (home-page "https://letsencrypt.org/certificates/")
    (synopsis "Let's Encrypt root and intermediate certificates")
    (description "This package provides a certificate store containing only the
Let's Encrypt root and intermediate certificates.  It is intended to be used
within Guix.")
    (license license:public-domain)))
