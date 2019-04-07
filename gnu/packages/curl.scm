;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public curl
  (package
   (name "curl")
   (version "7.64.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://curl.haxx.se/download/curl-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1cwg2skp953cqxbwn9ychy0lv4p3l38bsy3zz9xy6747gwm36llj"))))
   (build-system gnu-build-system)
   (outputs '("out"
              "doc"))                             ;1.2 MiB of man3 pages
   (inputs `(("gnutls" ,gnutls)
             ("gss" ,gss)
             ("libidn" ,libidn)
             ;; TODO XXX <https://bugs.gnu.org/34927>
             ;; Curl doesn't actually use or refer to libssh2 because the build
             ;; is not configured with '--with-libssh2'.  Remove this input when
             ;; a mass rebuild is appropriate (e.g. core-updates).
             ("libssh2" ,libssh2-1.8.0)
             ("openldap" ,openldap)
             ("nghttp2" ,nghttp2 "lib")
             ("zlib" ,zlib)))
   (native-inputs
     `(("perl" ,perl)
       ;; to enable the --manual option and make test 1026 pass
       ("groff" ,groff)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))
   (native-search-paths
    ;; Note: This search path is respected by the `curl` command-line tool only.
    ;; Ideally we would bake this into libcurl itself so other users can benefit,
    ;; but it's not supported upstream due to thread safety concerns.
    (list (search-path-specification
           (variable "CURL_CA_BUNDLE")
           (file-type 'regular)
           (separator #f)                         ;single entry
           (files '("etc/ssl/certs/ca-certificates.crt")))))
   (arguments
    `(#:configure-flags '("--with-gnutls" "--with-gssapi"
                          "--disable-static")
      ;; Add a phase to patch '/bin/sh' occurances in tests/runtests.pl
      #:phases
      (modify-phases %standard-phases
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

           ;; XXX FIXME: Test #1510 seems to work on some machines and not
           ;; others, possibly based on the kernel version.  It works on Guix System
           ;; on x86_64 with linux-libre-4.1, but fails on Hydra for both i686
           ;; and x86_64 with the following error:
           ;;
           ;; test 1510...[HTTP GET connection cache limit (CURLOPT_MAXCONNECTS)]
           ;;
           ;;  1510: output (log/stderr1510) FAILED:
           ;; --- log/check-expected    2015-06-27 07:45:53.166720834 +0000
           ;; +++ log/check-generated   2015-06-27 07:45:53.166720834 +0000
           ;; @@ -1,5 +1,5 @@
           ;;  * Connection #0 to host server1.example.com left intact[LF]
           ;;  * Connection #1 to host server2.example.com left intact[LF]
           ;;  * Connection #2 to host server3.example.com left intact[LF]
           ;; -* Closing connection 0[LF]
           ;; +* Closing connection 1[LF]
           ;;  * Connection #3 to host server4.example.com left intact[LF]
           (delete-file "tests/data/test1510")

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
   (version "0.6")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.lonelycactus.com/tarball/"
                                "guile_curl-" version ".tar.gz"))
            (sha256
             (base32
              "1pxdhnk288ky6gkpad8i60m0p6404rdvls43lr1b5d3csrklyc70"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags (list (string-append
                               "--with-guilesitedir="
                               (assoc-ref %outputs "out")
                               "/share/guile/site/2.2")
                              (string-append
                               "-with-guileextensiondir="
                               (assoc-ref %outputs "out")
                               "/lib/guile/2.2/extensions"))
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'patch-extension-path
          (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out      (assoc-ref outputs "out"))
                    (curl.scm (string-append
                               out "/share/guile/site/2.2/curl.scm"))
                    (curl.go  (string-append
                               out "/lib/guile/2.2/site-ccache/curl.go"))
                    (ext      (string-append out "/lib/guile/2.2/"
                                             "extensions/libguile-curl")))
               (substitute* curl.scm (("libguile-curl") ext))
               ;; The build system does not actually compile the Scheme module.
               ;; So we can compile it and put it in the right place in one go.
               (invoke "guild" "compile" curl.scm "-o" curl.go)))))))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs
    `(("curl" ,curl)
      ("guile" ,guile-2.2)))
   (home-page "http://www.lonelycactus.com/guile-curl.html")
   (synopsis "Curl bindings for Guile")
   (description "@code{guile-curl} is a project that has procedures that allow
Guile to do client-side URL transfers, like requesting documents from HTTP or
FTP servers.  It is based on the curl library.")
   (license license:gpl3+)))
