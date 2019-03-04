;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Adonay "adfeno" Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@openmailbox.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages samba)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public cifs-utils
  (package
    (name "cifs-utils")
    (version "6.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.samba.org/pub/linux-cifs/"
                           name "/" name "-" version ".tar.bz2"))
       (sha256 (base32
                "0ygz3pagjpaj5ky11hzh4byyymb7fpmqiqkprn11zwj31h2zdlg7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("keytuils" ,keyutils)
       ("linux-pam" ,linux-pam)
       ("libcap-ng" ,libcap-ng)
       ("mit-krb5" ,mit-krb5)
       ("samba" ,samba)
       ("talloc" ,talloc)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-root-sbin
           (lambda _ ; Don't try to install in "/sbin".
             (setenv "ROOTSBINDIR"
                     (string-append (assoc-ref %outputs "out") "/sbin"))
             #t)))))
    (synopsis "User-space utilities for Linux CIFS (Samba) mounts")
    (description "@code{cifs-utils} is a set of user-space utilities for
mounting and managing @dfn{Common Internet File System} (CIFS) shares using
the Linux kernel CIFS client.")
    (home-page "https://wiki.samba.org/index.php/LinuxCIFS_utils")
    ;; cifs-utils is licensed as GPL3 or later, but 3 files contain LGPL code.
    (license gpl3+)))

(define-public iniparser
  (package
    (name "iniparser")
    (version "4.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/ndevilla/iniparser/archive/v"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1bpk8dj9d5cl64lg6jsk0qlzrpg848nymwxc3fx707fk1n0al3cn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("Makefile" "test/Makefile")
               (("/usr/lib")
                (string-append (assoc-ref outputs "out") "/lib")))
             #t))
         (replace 'build
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "libiniparser.so.1"
                    make-flags)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (lib  (string-append out "/lib"))
                    (inc  (string-append out "/include"))
                    (doc  (string-append out "/share/doc/" ,name))
                    (html (string-append doc "/html")))
               (define (install dir)
                 (lambda (file)
                   (install-file file dir)))
               (for-each (install lib)
                         (find-files "." "^lib.*\\.so"))
               (with-directory-excursion lib
                 (symlink "libiniparser.so.1" "libiniparser.so"))
               (for-each (install inc)
                         (find-files "src" "\\.h$"))
               (for-each (install html)
                         (find-files "html" ".*"))
               (for-each (install doc)
                         '("AUTHORS" "INSTALL" "LICENSE" "README.md"))
               #t))))))
    (home-page "https://github.com/ndevilla/iniparser")
    (synopsis "Standalone ini file parsing library")
    (description
     "iniparser is a free stand-alone `ini' file parsing library (Windows
configuration files).  It is written in portable ANSI C and should compile
anywhere.")
    (license x11)))

(define-public samba
  (package
    (name "samba")
    (version "4.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.samba.org/pub/samba/stable/"
                                 "samba-" version ".tar.gz"))
             (sha256
              (base32
               "0kqbzywlnh1skg6g78qilyn12qv7wri66h5v9f77igncpkcai63d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'locate-docbook-stylesheets
           (lambda* (#:key inputs #:allow-other-keys)
             ;; XXX for some reason XML_CATALOG_FILES is not respected.
             (substitute* '("buildtools/wafsamba/samba_conftests.py"
                            "buildtools/wafsamba/wafsamba.py"
                            "docs-xml/xslt/man.xsl")
               (("http://docbook.sourceforge.net/release/xsl/current/")
                (string-append (assoc-ref inputs "docbook-xsl")
                               "/xml/xsl/docbook-xsl-"
                               ,(package-version docbook-xsl) "/")))
             #t))
         (replace 'configure
           ;; samba uses a custom configuration script that runs waf.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (libdir (string-append out "/lib")))
               (invoke "./configure"
                       "--enable-fhs"
                       ;; XXX: heimdal not packaged.
                       "--bundled-libraries=com_err"
                       (string-append "--prefix=" out)
                       "--sysconfdir=/etc"
                       ;; Install public and private libraries into
                       ;; a single directory to avoid RPATH issues.
                       (string-append "--libdir=" libdir)
                       (string-append "--with-privatelibdir=" libdir)))))
         (add-before 'install 'disable-etc-samba-directory-creation
           (lambda _
             (substitute* "dynconfig/wscript"
               (("bld\\.INSTALL_DIR\\(\"\\$\\{CONFIGDIR\\}\"\\)")
                ""))
             #t)))
       ;; XXX: The test infrastructure attempts to set password with
       ;; smbpasswd, which fails with "smbpasswd -L can only be used by root."
       ;; So disable tests until there's a workaround.
       #:tests? #f))
    (inputs                                   ; TODO: Add missing dependencies
     `(("acl" ,acl)
       ("cups" ,cups)
       ;; ("gamin" ,gamin)
       ("gpgme" ,gpgme)
       ("gnutls" ,gnutls)
       ("iniparser" ,iniparser)
       ("jansson" ,jansson)
       ("libaio" ,libaio)
       ("libarchive" ,libarchive)
       ("linux-pam" ,linux-pam)
       ("lmdb" ,lmdb)
       ("openldap" ,openldap)
       ("popt" ,popt)
       ("readline" ,readline)
       ("tdb" ,tdb)))
    (propagated-inputs
     ;; In Requires or Requires.private of pkg-config files.
     `(("ldb" ,ldb)
       ("talloc" ,talloc)
       ("tevent" ,tevent)))
    (native-inputs
     `(("docbook-xsl" ,docbook-xsl)    ;for generating manpages
       ("xsltproc" ,libxslt)           ;ditto
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2))) ; incompatible with Python 3
    (home-page "https://www.samba.org/")
    (synopsis
     "The standard Windows interoperability suite of programs for GNU and Unix")
    (description
     "Since 1992, Samba has provided secure, stable and fast file and print
services for all clients using the SMB/CIFS protocol, such as all versions of
DOS and Windows, OS/2, GNU/Linux and many others.

Samba is an important component to seamlessly integrate Linux/Unix Servers and
Desktops into Active Directory environments using the winbind daemon.")
    (license gpl3+)))

(define-public talloc
  (package
    (name "talloc")
    (version "2.1.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.samba.org/ftp/talloc/talloc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1aajda08yf7njgvg6r21ccxlvkarb9bwvf4jqh8yn3871a1zcnqr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; talloc uses a custom configuration script that runs a Python
             ;; script called 'waf', and doesn't tolerate unknown options.
             (setenv "CONFIG_SHELL" (which "sh"))
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure"
                       (string-append "--prefix=" out))))))))
    (native-inputs
     `(("which" ,which)))
    (inputs
     `(("python" ,python)))
    (home-page "https://talloc.samba.org")
    (synopsis "Hierarchical, reference counted memory pool system")
    (description
     "Talloc is a hierarchical, reference counted memory pool system with
destructors.  It is the core memory allocator used in Samba.")
    (license gpl3+))) ;; The bundled "replace" library uses LGPL3.

(define-public talloc/static
  (package
    (inherit talloc)
    (name "talloc-static")
    (synopsis
     "Hierarchical, reference counted memory pool system (static library)")
    (arguments
     (substitute-keyword-arguments (package-arguments talloc)
       ((#:phases phases)
        ;; Since Waf, the build system talloc uses, apparently does not
        ;; support building static libraries from a ./configure flag, roll our
        ;; own build process.  No need to be ashamed, we're not the only ones
        ;; doing that:
        ;; <https://github.com/proot-me/proot-static-build/blob/master/GNUmakefile>.
        ;; :-)
        `(modify-phases ,phases
           (replace 'build
             (lambda _
               (invoke "gcc" "-c" "-Ibin/default" "-I" "lib/replace"
                       "-I." "-Wall" "-g" "-D__STDC_WANT_LIB_EXT1__=1"
                       "talloc.c")
               (invoke "ar" "rc" "libtalloc.a" "talloc.o")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out     (assoc-ref outputs "out"))
                      (lib     (string-append out "/lib"))
                      (include (string-append out "/include")))
                 (mkdir-p lib)
                 (install-file "libtalloc.a" lib)
                 (install-file "talloc.h" include)
                 #t)))
           (delete 'check)))))))            ;XXX: tests rely on Python modules

(define-public tevent
  (package
    (name "tevent")
    (version "0.9.39")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.samba.org/ftp/tevent/tevent-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1rnln76ngd2b8lgqvfa9iscy6jizwycj85nfp9zd46b1c760z3gn"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; tevent uses a custom configuration script that runs waf.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       "--bundled-libraries=NONE")))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)
       ("which" ,which)))
    (propagated-inputs
     `(("talloc" ,talloc))) ; required by tevent.pc
    (synopsis "Event system library")
    (home-page "https://tevent.samba.org/")
    (description
     "Tevent is an event system based on the talloc memory management library.
It is the core event system used in Samba.  The low level tevent has support for
many event types, including timers, signals, and the classic file descriptor events.")
    (license lgpl3+)))

(define-public ldb
  (package
    (name "ldb")
    (version "1.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.samba.org/ftp/ldb/ldb-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1kiwlra6nfkb5n870k2db41jrm59bq9lxqmil4v7ignblgsdfdwb"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each (lambda (file)
                              ;; Delete everything except the build tools.
                              (unless (or (string-prefix? "third_party/waf" file)
                                          (string-suffix? "wscript" file))
                                (delete-file file)))
                            (find-files "third_party"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(;; LMDB is only supported on 64-bit systems, yet the test suite
       ;; requires it.
       #:tests? (assoc-ref %build-inputs "lmdb")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; ldb use a custom configuration script that runs waf.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--with-modulesdir=" out
                                      "/lib/ldb/modules")
                       "--bundled-libraries=NONE")))))))
    (native-inputs
     `(("cmocka" ,cmocka)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("which" ,which)))
    (propagated-inputs
     ;; ldb.pc refers to all these.
     `(("talloc" ,talloc)
       ("tdb" ,tdb)))
    (inputs
     `(,@(if (target-64bit?)
             `(("lmdb" ,lmdb))
             '())
       ("popt" ,popt)
       ("tevent" ,tevent)))
    (synopsis "LDAP-like embedded database")
    (home-page "https://ldb.samba.org/")
    (description
     "Ldb is a LDAP-like embedded database built on top of TDB.  What ldb does
is provide a fast database with an LDAP-like API designed to be used within an
application.  In some ways it can be seen as a intermediate solution between
key-value pair databases and a real LDAP database.")
    (license lgpl3+)))

(define-public ppp
  (package
    (name "ppp")
    (version "2.4.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.samba.org/ftp/ppp/ppp-"
                                  version ".tar.gz"))
              (patches
               (list (origin
                       ;; Use OpenSSL for cryptography instead of the obsolete glibc
                       ;; crypto functions that were removed in glibc 2.28.
                       (method url-fetch)
                       (uri (string-append "https://github.com/paulusmack/ppp/commit/"
                                           "3c7b86229f7bd2600d74db14b1fe5b3896be3875"
                                           ".patch"))
                       (file-name "ppp-use-openssl-crypto.patch")
                       (sha256
                        (base32
                         "0qlbi247lx3injpy8a1gcij9yilik0vfaibkpvdp88k3sa1rs69z")))))
              (sha256
               (base32
                "0c7vrjxl52pdwi4ckrvfjr08b31lfpgwf3pp0cqy76a77vfs7q02"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no check target
       #:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-Makefile
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libc    (assoc-ref inputs "libc"))
                   (openssl (assoc-ref inputs "openssl"))
                   (libpcap (assoc-ref inputs "libpcap")))
               (substitute* "pppd/Makefile.linux"
                 (("/usr/include/crypt\\.h")
                  (string-append libc "/include/crypt.h"))
                 (("/usr/include/openssl")
                  (string-append openssl "/include/openssl"))
                 (("/usr/include/pcap-bpf.h")
                  (string-append libpcap "/include/pcap-bpf.h")))
               #t))))))
    (inputs
     `(("libpcap" ,libpcap)
       ("openssl" ,(@ (gnu packages tls) openssl))))
    (synopsis "Implementation of the Point-to-Point Protocol")
    (home-page "https://ppp.samba.org/")
    (description
     "The Point-to-Point Protocol (PPP) provides a standard way to establish
a network connection over a serial link.  At present, this package supports IP
and IPV6 and the protocols layered above them, such as TCP and UDP.")
    ;; pppd, pppstats and pppdump are under BSD-style notices.
    ;; some of the pppd plugins are GPL'd.
    ;; chat is public domain.
    (license (list bsd-3 bsd-4 gpl2+ public-domain))))
