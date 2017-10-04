;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu packages ci)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu))

(define-public hydra
  (let ((commit "1ff48da3d3d4a425063f5b7dd0b89d35270f8932"))
    (package
      (name "hydra")
      (version (string-append "20151030." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/NixOS/hydra")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (patches (search-patches
                          ;; TODO: Remove once we have a darcs input
                          "hydra-disable-darcs-test.patch"))
                (sha256
                 (base32
                  "0ni8i8v1nxxfr51rz8m6znwpbm77vr7i05k506hmgmg32r938lap"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("unzip" ,unzip)
         ("pkg-config" ,pkg-config)
         ;; For documentation
         ("dblatex" ,dblatex)
         ("xsltproc" ,libxslt)
         ("docbook-xsl" ,docbook-xsl)
         ;; For bootstrap
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ;; For tests
         ("git" ,git)
         ("subversion" ,subversion)
         ("mercurial" ,mercurial)
         ("bazaar" ,bazaar)))
      (inputs
       `(("libpqxx" ,libpqxx)
         ("perl" ,perl)
         ("guile" ,guile-2.0)
         ("openssl" ,openssl)
         ("bzip2" ,bzip2)
         ("gzip" ,gzip)
         ("sed" ,sed)
         ("starman" ,starman)
         ("git" ,git)
         ("subversion" ,subversion)
         ("mercurial" ,mercurial)
         ("bazaar" ,bazaar)
         ("nix" ,nix)
         ;; Lots o' perl modules...
         ("perl-catalyst-action-rest" ,perl-catalyst-action-rest)
         ("perl-catalyst-authentication-store-dbix-class"
          ,perl-catalyst-authentication-store-dbix-class)
         ("perl-catalyst-devel" ,perl-catalyst-devel)
         ("perl-catalyst-dispatchtype-regex" ,perl-catalyst-dispatchtype-regex)
         ("perl-catalyst-plugin-accesslog" ,perl-catalyst-plugin-accesslog)
         ("perl-catalyst-plugin-authorization-roles"
          ,perl-catalyst-plugin-authorization-roles)
         ("perl-catalyst-plugin-captcha" ,perl-catalyst-plugin-captcha)
         ("perl-catalyst-plugin-session-state-cookie"
          ,perl-catalyst-plugin-session-state-cookie)
         ("perl-catalyst-plugin-session-store-fastmmap"
          ,perl-catalyst-plugin-session-store-fastmmap)
         ("perl-catalyst-plugin-stacktrace" ,perl-catalyst-plugin-stacktrace)
         ("perl-catalyst-traitfor-request-proxybase"
          ,perl-catalyst-traitfor-request-proxybase)
         ("perl-catalyst-view-download" ,perl-catalyst-view-download)
         ("perl-catalyst-view-json" ,perl-catalyst-view-json)
         ("perl-catalyst-view-tt" ,perl-catalyst-view-tt)
         ("perl-catalystx-roleapplicator" ,perl-catalystx-roleapplicator)
         ("perl-catalystx-script-server-starman"
          ,perl-catalystx-script-server-starman)
         ("perl-crypt-randpasswd" ,perl-crypt-randpasswd)
         ("perl-data-dump" ,perl-data-dump)
         ("perl-datetime" ,perl-datetime)
         ("perl-dbd-pg" ,perl-dbd-pg)
         ("perl-dbd-sqlite" ,perl-dbd-sqlite)
         ("perl-digest-sha1" ,perl-digest-sha1)
         ("perl-email-mime" ,perl-email-mime)
         ("perl-email-sender" ,perl-email-sender)
         ("perl-file-slurp" ,perl-file-slurp)
         ("perl-io-compress" ,perl-io-compress)
         ("perl-ipc-run" ,perl-ipc-run)
         ("perl-json-any" ,perl-json-any)
         ("perl-json-xs" ,perl-json-xs)
         ("perl-libwww" ,perl-libwww)
         ("perl-lwp-protocol-https" ,perl-lwp-protocol-https)
         ("perl-net-amazon-s3" ,perl-net-amazon-s3)
         ("perl-net-statsd" ,perl-net-statsd)
         ("perl-padwalker" ,perl-padwalker)
         ("perl-readonly" ,perl-readonly)
         ("perl-set-scalar" ,perl-set-scalar)
         ("perl-sql-splitstatement" ,perl-sql-splitstatement)
         ("perl-sys-hostname-long" ,perl-sys-hostname-long)
         ("perl-text-diff" ,perl-text-diff)
         ("perl-text-table" ,perl-text-table)
         ("perl-xml-simple" ,perl-xml-simple)))
      (arguments
       `(#:configure-flags
         (let ((docbook (assoc-ref %build-inputs "docbook-xsl")))
           (list (string-append "--with-docbook-xsl="
                                docbook "/xml/xsl/docbook-xsl-"
                                ,(package-version docbook-xsl))
                 (string-append "--docdir=" %output
                                "/doc/hydra-" ,version)))
         #:phases (modify-phases %standard-phases
                    (add-after
                     'unpack 'bootstrap
                     (lambda _ (zero? (system* "autoreconf" "-vfi"))))
                    (add-before
                     'check 'check-setup
                     (lambda _ (setenv "LOGNAME" "test.log")))
                    (add-after
                     'install 'wrap-program
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out")))
                         (for-each
                          (lambda (file)
                            (wrap-program file
                              `("PATH" ":" prefix
                                (,(string-append out "/bin")
                                 ,@(map (lambda (i)
                                          (string-append (assoc-ref inputs i)
                                                         "/bin"))
                                        '("subversion" "git" "bazaar"
                                          "mercurial" "coreutils" "gzip"
                                          "sed" "unzip" "nix"))))
                              `("PERL5LIB" ":" prefix
                                (,(string-append out "/libexec/hydra/lib")
                                 ,@(search-path-as-string->list
                                    (getenv "PERL5LIB"))))
                              `("HYDRA_RELEASE" = (,,version))
                              `("HYDRA_HOME" =
                                (,(string-append out "/libexec/hydra")))
                              `("NIX_RELEASE" = (,,(package-version nix)))))
                          (find-files (string-append out "/bin")
                                      ".*"))))))))
      (home-page "https://nixos.org/hydra")
      (synopsis "Continuous build system")
      (description
       "Hydra is a tool for continuous integration testing and software
release that uses a purely functional language to describe build jobs and
their dependencies.")
      (license l:gpl3+))))

(define-public cuirass
  (let ((commit "9cfea9fe2e3ca6a3d1b832a6ec217426ec973c93")
        (revision "10"))
    (package
      (name "cuirass")
      (version (string-append "0.0.1-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/guix/guix-cuirass.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "177klidmsw12kjk9dzawc0bqcwqlplgx45m87qpgjfx3cnk28i2b"))))
      (build-system gnu-build-system)
      (arguments
       '(#:modules ((guix build utils)
                    (guix build gnu-build-system)
                    (ice-9 rdelim)
                    (ice-9 popen))

         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'disable-repo-tests
             (λ _
               ;; Disable tests that use a connection to the Guix daemon.
               (substitute* "Makefile.am"
                 (("tests/repo.scm \\\\") "\\"))
               #t))
           (add-after 'disable-repo-tests 'patch-/bin/sh
             (lambda _
               (substitute* "build-aux/git-version-gen"
                 (("#!/bin/sh") (string-append "#!" (which "sh"))))
               #t))
           (add-after 'patch-/bin/sh 'bootstrap
             (lambda _ (zero? (system* "sh" "bootstrap"))))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Wrap the 'cuirass' command to refer to the right modules.
               (let* ((out    (assoc-ref outputs "out"))
                      (json   (assoc-ref inputs "guile-json"))
                      (sqlite (assoc-ref inputs "guile-sqlite3"))
                      (git    (assoc-ref inputs "guile-git"))
                      (bytes  (assoc-ref inputs "guile-bytestructures"))
                      (guix   (assoc-ref inputs "guix"))
                      (guile  (assoc-ref %build-inputs "guile"))
                      (effective (read-line
                                  (open-pipe* OPEN_READ
                                              (string-append guile "/bin/guile")
                                              "-c" "(display (effective-version))")))
                      (mods   (string-append json "/share/guile/site/"
                                             effective ":"
                                             git "/share/guile/site/"
                                             effective ":"
                                             bytes "/share/guile/site/"
                                             effective ":"
                                             sqlite "/share/guile/site/"
                                             effective ":"
                                             guix "/share/guile/site/"
                                             effective)))
                 ;; Make sure 'cuirass' can find the 'evaluate' command, as
                 ;; well as the relevant Guile modules.
                 (wrap-program (string-append out "/bin/cuirass")
                   `("PATH" ":" prefix (,(string-append out "/bin")))
                   `("GUILE_LOAD_PATH" ":" prefix (,mods))
                   `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,mods)))
                 #t))))))
      (inputs
       `(("guile" ,guile-2.2)
         ("guile-json" ,guile-json)
         ("guile-sqlite3" ,guile-sqlite3)
         ("guile-git" ,guile-git)
         ;; FIXME: this is propagated by "guile-git", but it needs to be among
         ;; the inputs to add it to GUILE_LOAD_PATH.
         ("guile-bytestructures" ,guile-bytestructures)
         ("guix" ,guix)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)
         ("texinfo" ,texinfo)))
      (native-search-paths
       ;; For HTTPS access, Cuirass itself honors these variables, with the
       ;; same semantics as Git and OpenSSL (respectively).
       (list (search-path-specification
              (variable "GIT_SSL_CAINFO")
              (file-type 'regular)
              (separator #f)                      ;single entry
              (files '("etc/ssl/certs/ca-certificates.crt")))
             (search-path-specification
              (variable "SSL_CERT_DIR")
              (files '("etc/ssl/certs")))))
      (synopsis "Continuous integration system")
      (description
       "Cuirass is a continuous integration tool using GNU Guix.  It is
intended as a replacement for Hydra.")
      (home-page "https://www.gnu.org/software/guix/")
      (license l:gpl3+))))
