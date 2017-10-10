;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2012, 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2012, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages kerberos)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public mit-krb5
  (package
    (name "mit-krb5")
    (replacement mit-krb5-1.15.2)
    (version "1.15.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://web.mit.edu/kerberos/dist/krb5/"
                                  (version-major+minor version)
                                  "/krb5-" version ".tar.gz"))
              (sha256
               (base32
                "0igbi5d095c2hgpn2cixpc4q2ij8vgg2bx7yjfly5zfmvlqqhz23"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("perl" ,perl)))
    (arguments
     `(;; XXX: On 32-bit systems, 'kdb5_util' hangs on an fcntl/F_SETLKW call
       ;; while running the tests in 'src/tests'.
       #:tests? ,(string=? (%current-system) "x86_64-linux")

       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source-directory
           (lambda _
             (chdir "src")
             #t))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((perl (assoc-ref inputs "perl")))
               (substitute* "plugins/kdb/db2/libdb2/test/run.test"
                 (("/bin/cat") (string-append perl "/bin/perl"))
                 (("D/bin/sh") (string-append "D" (which "sh")))
                 (("bindir=/bin/.") (string-append "bindir=" perl "/bin"))))

             ;; avoid service names since /etc/services is unavailable
             (substitute* "tests/resolve/Makefile"
               (("-p telnet") "-p 23"))
             #t)))))
    (synopsis "MIT Kerberos 5")
    (description
     "Massachusetts Institute of Technology implementation of Kerberos.
Kerberos is a network authentication protocol designed to provide strong
authentication for client/server applications by using secret-key
cryptography.")
    (license (license:non-copyleft "file://NOTICE"
                                   "See NOTICE in the distribution."))
    (home-page "http://web.mit.edu/kerberos/")
    (properties '((cpe-name . "kerberos")))))

(define mit-krb5-1.15.2 ; CVE-2017-{11368,11462}
  (package
    (inherit mit-krb5)
    (version "1.15.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://web.mit.edu/kerberos/dist/krb5/"
                                  (version-major+minor version)
                                  "/krb5-" version ".tar.gz"))
              (sha256
               (base32
                "0zn8s7anb10hw3nzwjz7vg10fgmmgvwnibn2zrn3nppjxn9f6f8n"))))))

(define-public shishi
  (package
    (name "shishi")
    (version "1.0.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/shishi/shishi-"
                          version ".tar.gz"))
      (patches (search-patches "shishi-fix-libgcrypt-detection.patch"))
      (sha256
       (base32
        "032qf72cpjdfffq1yq54gz3ahgqf2ijca4vl31sfabmjzq9q370d"))))
    (build-system gnu-build-system)
    (arguments
     '(;; This is required since we patch some of the build scripts.
       ;; Remove for the next Shishi release after 1.0.2 or when
       ;; removing 'shishi-fix-libgcrypt-detection.patch'.
       #:configure-flags '("ac_cv_libgcrypt=yes")))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gnutls" ,gnutls)
       ("libidn" ,libidn)
       ("linux-pam" ,linux-pam-1.2)
       ("zlib" ,zlib)
       ("libgcrypt" ,libgcrypt)
       ("libtasn1" ,libtasn1)))
    (home-page "https://www.gnu.org/software/shishi/")
    (synopsis "Implementation of the Kerberos 5 network security system")
    (description
     "GNU Shishi is a free implementation of the Kerberos 5 network security
system.  It is used to allow non-secure network nodes to communicate in a
secure manner through client-server mutual authentication via tickets.")
    (license license:gpl3+)))

(define-public heimdal
  (package
    (name "heimdal")
    (version "1.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.h5l.org/dist/src/heimdal-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "19gypf9vzfrs2bw231qljfl4cqc1riyg0ai0xmm1nd1wngnpphma"))
              (patches (search-patches "heimdal-CVE-2017-6594.patch"
                                       "heimdal-CVE-2017-11103.patch"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("User=.*$") "User=Guix\n")
                  (("Date=.*$") "Date=2017\n")))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list
                          ;; Work around a linker error.
                          "CFLAGS=-pthread"

                          ;; Avoid 7 MiB of .a files.
                          "--disable-static"

                          ;; Do not build libedit.
                          (string-append
                           "--with-readline-lib="
                           (assoc-ref %build-inputs "readline") "/lib")
                          (string-append
                           "--with-readline-include="
                           (assoc-ref %build-inputs "readline") "/include"))

       #:phases (modify-phases %standard-phases
                  (add-before 'check 'skip-tests
                    (lambda _
                      ;; The test simply runs 'ftp --version && ftp --help'
                      ;; but that fails in the chroot because 'ftp' tries to
                      ;; do a service lookup before printing the help/version.
                      (substitute* "appl/ftp/ftp/Makefile.in"
                        (("^CHECK_LOCAL =.*")
                         "CHECK_LOCAL = no-check-local\n"))
                      #t)))))
    (native-inputs `(("e2fsprogs" ,e2fsprogs)))   ;for 'compile_et'
    (inputs `(("readline" ,readline)
              ("bdb" ,bdb)
              ("e2fsprogs" ,e2fsprogs)))          ;for libcom_err
    (home-page "http://www.h5l.org/")
    (synopsis "Kerberos 5 network authentication")
    (description
     "Heimdal is an implementation of Kerberos 5 network authentication
service.")
    (license license:bsd-3)))
