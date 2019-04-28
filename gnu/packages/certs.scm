;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tls))

(define certdata2pem
  (package
    (name "certdata2pem")
    (version "2013")
    (source
     (origin
      (method url-fetch)
        (uri
          "http://pkgs.fedoraproject.org/cgit/ca-certificates.git/plain/certdata2pem.py?id=053dde8a2f5901e97028a58bf54e7d0ef8095a54")
        (file-name "certdata2pem.py")
        (sha256
          (base32
            "0zscrm41gnsf14zvlkxhy00h3dmgidyz645ldpda3y3vabnwv8dx"))))
   (build-system trivial-build-system)
   (inputs
     `(("python" ,python-2)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
        (begin
          (use-modules (guix build utils))
          (let ((bin (string-append %output "/bin")))
            (copy-file (assoc-ref %build-inputs "source") "certdata2pem.py")
            (chmod "certdata2pem.py" #o555)
            (substitute* "certdata2pem.py"
              (("/usr/bin/python")
               (string-append (assoc-ref %build-inputs "python")
                              "/bin/python"))
              ;; Use the file extension .pem instead of .crt.
              (("crt") "pem"))
            (mkdir-p bin)
            (copy-file "certdata2pem.py"
                       (string-append bin "/certdata2pem.py"))
            #t))))
   (synopsis "Python script to extract .pem data from certificate collection")
   (description
    "certdata2pem.py is a Python script to transform X.509 certificate
\"source code\" as contained, for example, in the Mozilla sources, into
.pem formatted certificates.")
   (license license:gpl2+)
   (home-page "http://pkgs.fedoraproject.org/cgit/ca-certificates.git/")))

(define-public nss-certs
  (package
    (name "nss-certs")
    (version "3.43")
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
                "1jp27w4w9nj5pkzrbc1zqj6pa09h2yy7vhzyx5fvg1q86fvw22zk"))))
    (build-system gnu-build-system)
    (outputs '("out"))
    (native-inputs
     `(("certdata2pem" ,certdata2pem)
       ("openssl" ,openssl)
       ("perl" ,perl)))                           ;for OpenSSL's 'c_rehash'
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (rnrs io ports)
                  (srfi srfi-26)
                  (ice-9 regex))
       #:phases
       (modify-phases
           (map (cut assq <> %standard-phases)
                '(set-paths install-locale unpack))
         (add-after 'unpack 'install
           (lambda _
             (let ((certsdir (string-append %output "/etc/ssl/certs/"))
                   (trusted-rx (make-regexp "^# openssl-trust=[a-zA-Z]"
                                            regexp/newline)))

               (define (maybe-install-cert file)
                 (let ((cert (call-with-input-file file get-string-all)))
                   (when (regexp-exec trusted-rx cert)
                     (call-with-output-file
                         (string-append certsdir file)
                       (cut display cert <>)))))

               (mkdir-p certsdir)
               (with-directory-excursion "nss/lib/ckfw/builtins/"
                 ;; extract single certificates from blob
                 (invoke "certdata2pem.py" "certdata.txt")
                 ;; copy selected .pem files into the output
                 (for-each maybe-install-cert
                           (find-files "." ".*\\.pem")))

               (with-directory-excursion certsdir
                 ;; create symbolic links for and by openssl
                 ;; Strangely, the call (system* "c_rehash" certsdir)
                 ;; from inside the build dir fails with
                 ;; "Usage error; try -help."
                 ;; This looks like a bug in openssl-1.0.2, but we can also
                 ;; switch into the target directory.
                 (invoke "c_rehash" "."))
               #t))))))

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
             "0zhd1ps7sz4w1x52xk3v7ng6d0rcyi7y7rcrplwkmilnq5hzjv1y"))))
       ;; "Let’s Encrypt Authority X3", the active Let's Encrypt intermediate
       ;; certificate.
       ("letsencryptauthorityx3.pem"
        ,(origin
           (method url-fetch)
           (uri "https://letsencrypt.org/certs/letsencryptauthorityx3.pem")
           (sha256
            (base32
             "0zbamj6c7zqw1j9mbqygc8k1ykgj6xiisp9svmlif5lkbnyjhnkk"))))
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
             "003dc94c8qwj634h0dq743x7hqv9rdcfaisdksprkmi2jd107xq4"))))))
    (home-page "https://letsencrypt.org/certificates/")
    (synopsis "Let's Encrypt root and intermediate certificates")
    (description "This package provides a certificate store containing only the
Let's Encrypt root and intermediate certificates.  It is intended to be used
within Guix.")
    (license license:public-domain)))
