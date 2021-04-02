;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2015, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Dale Mellor <guix-devel-0brg6b@rdmp.org>
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

(define-module (gnu packages curl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (srfi srfi-1))

(define-public curl
  (package
   (name "curl")
   (version "7.74.0")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://curl.haxx.se/download/curl-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "12w7gskrglg6qrmp822j37fmbr0icrcxv7rib1fy5xiw80n5z7cr"))
             (patches (search-patches "curl-use-ssl-cert-env.patch"))))
   (replacement curl/fixed)
   (build-system gnu-build-system)
   (outputs '("out"
              "doc"))                             ;1.2 MiB of man3 pages
   (inputs `(("gnutls" ,gnutls)
             ("libidn" ,libidn)
             ("openldap" ,openldap)
             ("mit-krb5" ,mit-krb5)
             ("nghttp2" ,nghttp2 "lib")
             ("zlib" ,zlib)))
   (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
   (native-search-paths
    ;; These variables are introduced by curl-use-ssl-cert-env.patch.
    (list (search-path-specification
           (variable "SSL_CERT_DIR")
           (separator #f)                        ;single entry
           (files '("etc/ssl/certs")))
          (search-path-specification
           (variable "SSL_CERT_FILE")
           (file-type 'regular)
           (separator #f)                        ;single entry
           (files '("etc/ssl/certs/ca-certificates.crt")))
          ;; Note: This search path is respected by the `curl` command-line
          ;; tool only.  Patching libcurl to read it too would bring no
          ;; advantages and require maintaining a more complex patch.
          (search-path-specification
           (variable "CURL_CA_BUNDLE")
           (file-type 'regular)
           (separator #f)                         ;single entry
           (files '("etc/ssl/certs/ca-certificates.crt")))))
   (arguments
    `(#:disallowed-references ("doc")
      #:configure-flags (list "--with-gnutls"
                              (string-append "--with-gssapi="
                                             (assoc-ref %build-inputs "mit-krb5"))
                              "--disable-static")
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'do-not-record-configure-flags
          (lambda _
            ;; Do not save the configure options to avoid unnecessary references.
            (substitute* "curl-config.in"
              (("@CONFIGURE_OPTIONS@")
               "\"not available\""))
            #t))
        (add-after
         'install 'move-man3-pages
         (lambda* (#:key outputs #:allow-other-keys)
           ;; Move section 3 man pages to "doc".
           (let ((out (assoc-ref outputs "out"))
                 (doc (assoc-ref outputs "doc")))
             (mkdir-p (string-append doc "/share/man"))
             (rename-file (string-append out "/share/man/man3")
                          (string-append doc "/share/man/man3"))
             #t)))
        (replace
         'check
         (lambda _
           (substitute* "tests/runtests.pl"
             (("/bin/sh") (which "sh")))

           ;; The top-level "make check" does "make -C tests quiet-test", which
           ;; is too quiet.  Use the "test" target instead, which is more
           ;; verbose.
           (invoke "make" "-C" "tests" "test"))))))
   (synopsis "Command line tool for transferring data with URL syntax")
   (description
    "curl is a command line tool for transferring data with URL syntax,
supporting DICT, FILE, FTP, FTPS, Gopher, HTTP, HTTPS, IMAP, IMAPS, LDAP,
LDAPS, POP3, POP3S, RTMP, RTSP, SCP, SFTP, SMTP, SMTPS, Telnet and TFTP.
curl supports SSL certificates, HTTP POST, HTTP PUT, FTP uploading, HTTP
form based upload, proxies, cookies, file transfer resume, user+password
authentication (Basic, Digest, NTLM, Negotiate, kerberos...), proxy
tunneling, and so on.")
   (license (license:non-copyleft "file://COPYING"
                                  "See COPYING in the distribution."))
   (home-page "https://curl.haxx.se/")))

;; This package exists mainly to bootstrap CMake.  It must not depend on
;; anything that uses cmake-build-system.
(define-public curl-minimal
  (hidden-package
   (package/inherit
    curl
    (name "curl-minimal")
    (inputs (alist-delete "openldap" (package-inputs curl))))))

(define-public curl/fixed
  (package
    (inherit curl)
    (version "7.76.0")
    (source
     (origin
       (inherit (package-source curl))
       (uri (string-append "https://curl.haxx.se/download/curl-"
                           version ".tar.xz"))
       (patches (search-patches "curl-7.76-use-ssl-cert-env.patch"))
       (sha256
        (base32
         "1j2g04m6als6hmqzvddv84c31m0x90bfgyz3bjrwdkarbkby40k3"))))))

(define-public kurly
  (package
    (name "kurly")
    (version "1.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.com/davidjpeacock/kurly.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "003jv2k45hg2svhjpy5253ccd250vi2r17x2zhm51iw54kgwxipm"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "gitlab.com/davidjpeacock/kurly"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-documentation
           (lambda* (#:key import-path outputs #:allow-other-keys)
             (let* ((source (string-append "src/" import-path))
                    (out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version))
                    (man (string-append out "/share/man/man1")))
               (with-directory-excursion source
                 (install-file "README.md" doc)
                 (mkdir-p man)
                 (copy-file "doc/kurly.man"
                            (string-append man "/kurly.1")))
               #t))))))
    (inputs
     `(("go-github-com-alsm-ioprogress" ,go-github-com-alsm-ioprogress)
       ("go-github-com-aki237-nscjar" ,go-github-com-aki237-nscjar)
       ("go-github-com-urfave-cli" ,go-github-com-urfave-cli)))
    (synopsis "Command-line HTTP client")
    (description "kurly is an alternative to the @code{curl} program written in
Go.  kurly is designed to operate in a similar manner to curl, with select
features.  Notably, kurly is not aiming for feature parity, but common flags and
mechanisms particularly within the HTTP(S) realm are to be expected.  kurly does
not offer a replacement for libcurl.")
    (home-page "https://gitlab.com/davidjpeacock/kurly")
    (license license:asl2.0)))

(define-public guile-curl
  (package
   (name "guile-curl")
   (version "0.9")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.lonelycactus.com/tarball/"
                                "guile_curl-" version ".tar.gz"))
            (sha256
             (base32
              "0y7wfhilfm6vzs0wyifrrc2pj9nsxfas905c7qa5cw4i6s74ypmi"))))
   (build-system gnu-build-system)
   (arguments
    `(#:modules (((guix build guile-build-system)
                  #:select (target-guile-effective-version))
                 ,@%gnu-build-system-modules)
      #:imported-modules ((guix build guile-build-system)
                          ,@%gnu-build-system-modules)
      #:configure-flags (list (string-append
                               "--with-guilesitedir="
                               (assoc-ref %outputs "out")
                               "/share/guile/site/"
                               (target-guile-effective-version
                                (assoc-ref %build-inputs "guile")))
                              (string-append
                               "-with-guileextensiondir="
                               (assoc-ref %outputs "out")
                               "/lib/guile/"
                               (target-guile-effective-version
                                (assoc-ref %build-inputs "guile"))
                               "/extensions"))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-undefined-references
          (lambda* _
            (substitute* "module/curl.scm"
              ;; The following #defines are missing from our curl package
              ;; and therefore result in the evaluation of undefined symbols.
              ((",CURLOPT_HAPROXYPROTOCOL") "#f")
              ((",CURLOPT_DISALLOW_USERNAME_IN_URL") "#f")
              ((",CURLOPT_TIMEVALUE_LARGE") "#f")
              ((",CURLOPT_DNS_SHUFFLE_ADDRESSES") "#f")
              ((",CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS") "#f"))))
        (add-after 'install 'patch-extension-path
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out      (assoc-ref outputs "out"))
                   (curl.scm (string-append
                              out "/share/guile/site/"
                              (target-guile-effective-version)
                              "/curl.scm"))
                   (curl.go  (string-append
                              out "/lib/guile/"
                              (target-guile-effective-version)
                              "/site-ccache/curl.go"))
                   (ext      (string-append out "/lib/guile/"
                                            (target-guile-effective-version)
                                            "/extensions/libguile-curl")))
              (substitute* curl.scm (("libguile-curl") ext))
              ;; The build system does not actually compile the Scheme module.
              ;; So we can compile it and put it in the right place in one go.
              (invoke "guild" "compile" curl.scm "-o" curl.go)))))))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs
    `(("curl" ,curl)
      ("guile" ,guile-3.0)))
   (home-page "http://www.lonelycactus.com/guile-curl.html")
   (synopsis "Curl bindings for Guile")
   (description "@code{guile-curl} is a project that has procedures that allow
Guile to do client-side URL transfers, like requesting documents from HTTP or
FTP servers.  It is based on the curl library.")
   (license license:gpl3+)))

(define-public guile2.2-curl
  (package
    (inherit guile-curl)
    (name "guile2.2-curl")
    (inputs
     `(("curl" ,curl)
       ("guile" ,guile-2.2)))))

(define-public curlpp
  (package
    (name "curlpp")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jpbarrette/curlpp")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1b0ylnnrhdax4kwjq64r1fk0i24n5ss6zfzf4hxwgslny01xiwrk"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    ;; There are no build tests to be had.
    (arguments
     '(#:tests? #f))
    ;; The installed version needs the header files from the C library.
    (propagated-inputs
     `(("curl" ,curl)))
    (synopsis "C++ wrapper around libcURL")
    (description
     "This package provides a free and easy-to-use client-side C++ URL
transfer library, supporting FTP, FTPS, HTTP, HTTPS, GOPHER, TELNET, DICT,
FILE and LDAP; in particular it supports HTTPS certificates, HTTP POST, HTTP
PUT, FTP uploading, kerberos, HTTP form based upload, proxies, cookies,
user+password authentication, file transfer resume, http proxy tunneling and
more!")
    (home-page "http://www.curlpp.org")
    (license license:expat)))
