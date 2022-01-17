;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2012, 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2012, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public mit-krb5
  (package
    (name "mit-krb5")
    (version "1.19.2")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://web.mit.edu/kerberos/dist/krb5/"
                                   (version-major+minor version)
                                   "/krb5-" version ".tar.gz")
                    (string-append "https://kerberos.org/dist/krb5/"
                                   (version-major+minor version)
                                   "/krb5-" version ".tar.gz")))
              (patches (search-patches "mit-krb5-hurd.patch"))
              (sha256
               (base32
                "0snz1jm2w4dkk65zcz953jmmv9mqa30fanch2bk8r3rs9vp3yi8h"))))
    (build-system gnu-build-system)
    (native-inputs
     (list bison perl tcl))                   ;required for some tests
    (inputs
     (list openssl))
    (arguments
     `(;; XXX: On 32-bit systems, 'kdb5_util' hangs on an fcntl/F_SETLKW call
       ;; while running the tests in 'src/tests'. Also disable tests when
       ;; cross-compiling.
       #:tests? ,(and (not (%current-target-system))
                      (string=? (%current-system) "x86_64-linux"))

       ,@(if (%current-target-system)
             '(#:configure-flags
               (list "--localstatedir=/var"
                     "krb5_cv_attr_constructor_destructor=yes"
                     "ac_cv_func_regcomp=yes"
                     "ac_cv_printf_positional=yes"
                     "ac_cv_file__etc_environment=yes"
                     "ac_cv_file__etc_TIMEZONE=no")
               #:make-flags
               (list "CFLAGS+=-DDESTRUCTOR_ATTR_WORKS=1"))
             '(#:configure-flags
               (list "--localstatedir=/var")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source-directory
           (lambda _
             (chdir "src")))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((perl (assoc-ref (or native-inputs inputs) "perl")))
               (substitute* "plugins/kdb/db2/libdb2/test/run.test"
                 (("/bin/cat") (string-append perl "/bin/perl"))
                 (("D/bin/sh") (string-append "D" (which "sh")))
                 (("bindir=/bin/.") (string-append "bindir=" perl "/bin")))))))))
    (synopsis "MIT Kerberos 5")
    (description
     "Massachusetts Institute of Technology implementation of Kerberos.
Kerberos is a network authentication protocol designed to provide strong
authentication for client/server applications by using secret-key
cryptography.")
    (license (license:non-copyleft "file://NOTICE"
                                   "See NOTICE in the distribution."))
    (home-page "https://web.mit.edu/kerberos/")
    (properties '((cpe-name . "kerberos")))))

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
       ;; Remove first two items for the next Shishi release after 1.0.2 or
       ;; when removing 'shishi-fix-libgcrypt-detection.patch'.
       #:configure-flags
       '("ac_cv_libgcrypt=yes" "--disable-static"
         "--with-key-dir=/etc/shishi" "--with-db-dir=/var/shishi")
       #:phases
       (modify-phases %standard-phases
        (add-after 'configure 'disable-automatic-key-generation
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "Makefile"
             (("^install-data-hook:")
              "install-data-hook:\nx:\n"))
            #t)))))
    (native-inputs (list pkg-config))
    (inputs
     (list gnutls
           libidn
           linux-pam-1.2
           zlib
           libgcrypt
           libtasn1))
    (home-page "https://www.gnu.org/software/shishi/")
    (synopsis "Implementation of the Kerberos 5 network security system")
    (description
     "GNU Shishi is a free implementation of the Kerberos 5 network security
system.  It is used to allow non-secure network nodes to communicate in a
secure manner through client-server mutual authentication via tickets.

After installation, the system administrator should generate keys using
@code{shisa -a /etc/shishi/shishi.keys}.")
    (license license:gpl3+)))

(define-public heimdal
  (package
    (name "heimdal")
    (version "7.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/heimdal/heimdal/releases/download/"
                    "heimdal-" version "/" "heimdal-" version ".tar.gz"))
              (sha256
               (base32
                "06vx3cb01s4lv3lpv0qzbbj97cln1np1wjphkkmmbk1lsqa36bgh"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "configure"
                    (("User=.*$") "User=Guix\n")
                    (("Host=.*$") "Host=GNU")
                    (("Date=.*$") "Date=2019\n"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ,#~(list
           ;; Avoid 7 MiB of .a files.
           "--disable-static"

           ;; Do not build libedit.
           (string-append
            "--with-readline-lib="
            (assoc-ref %build-inputs "readline") "/lib")
           (string-append
            "--with-readline-include="
            (assoc-ref %build-inputs "readline") "/include")

           ;; Do not build sqlite.
           (string-append
            "--with-sqlite3="
            (assoc-ref %build-inputs "sqlite"))

           #$@(if (%current-target-system)
                  ;; The configure script is too pessimistic.
                  ;; Setting this also resolves a linking error.
                  #~("ac_cv_func_getpwnam_r_posix=yes"
                     ;; Allow 'slc' and 'asn1_compile' to be found.
                     (string-append "--with-cross-tools="
                                    #+(file-append this-package
                                                   "/libexec/heimdal")))
                  #~()))
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'pre-configure
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "configure"
                        ;; The e2fsprogs input is included for libcom_err,
                        ;; let's use it even if cross-compiling.
                        (("test \"\\$\\{krb_cv_com_err\\}\" = \"yes\"")
                         ":")
                        ;; Our 'compile_et' is not in --with-cross-tools,
                        ;; which confuses heimdal.
                        (("ac_cv_prog_COMPILE_ET=\\$\\{with_cross_tools\\}compile_et")
                         "ac_cv_PROG_COMPILE_ET=compile_et"))
                      (substitute* '("appl/afsutil/pagsh.c" "appl/su/su.c")
                        (("/bin/sh")
                         (search-input-file inputs "bin/sh"))
                        ;; Use the cross-compiled bash instead of the
                        ;; native bash (XXX shouldn't _PATH_BSHELL point
                        ;; to a cross-compiled bash?).
                        (("_PATH_BSHELL")
                         (string-append
                          "\"" (search-input-file inputs "bin/sh") "\"")))
                      (substitute* '("tools/Makefile.in")
                        (("/bin/sh") (which "sh")))))
                  (add-before 'check 'pre-check
                    (lambda _
                      ;; For 'getxxyyy-test'.
                      (setenv "USER" (passwd:name (getpwuid (getuid))))

                      ;; Skip 'db' and 'kdc' tests for now.
                      ;; FIXME: figure out why 'kdc' tests fail.
                      (with-output-to-file "tests/db/have-db.in"
                        (lambda ()
                          (format #t "#!~a~%exit 1~%" (which "sh")))))))
       ;; Tests fail when run in parallel.
       #:parallel-tests? #f))
    (native-inputs (list e2fsprogs ;for 'compile_et'
                         texinfo
                         unzip ;for tests
                         perl))
    (inputs (list readline
                  bash-minimal
                  bdb
                  e2fsprogs ;for libcom_err
                  mit-krb5
                  sqlite))
    (home-page "http://www.h5l.org/")
    (synopsis "Kerberos 5 network authentication")
    (description
     "Heimdal is an implementation of Kerberos 5 network authentication
service.")
    (license license:bsd-3)))
