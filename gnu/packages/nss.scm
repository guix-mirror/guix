;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages nss)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages sqlite))

(define-public nspr
  (package
    (name "nspr")
    (version "4.31")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "1j5b2m8cjlhnnv8sq34587avaagkqvh521w4f95miwgvsn3xlaap"))))
    (build-system gnu-build-system)
    (inputs
     ;; For 'compile-et.pl' and 'nspr-config'.
     (list perl ;for 'compile-et.pl'
           bash-minimal)) ;for 'nspr-config'
    (native-inputs
     (list perl))
    (arguments
     `(;; Prevent the 'native' perl from sneaking into the closure.
       ;; XXX it would be nice to do the same for 'bash-minimal',
       ;; but using 'canonical-package' causes loops.
       ,@(if (%current-target-system)
             `(#:disallowed-references
               (,(gexp-input (this-package-native-input "perl") #:native? #t)))
             '())
       #:tests? #f ; no check target
       #:configure-flags
       (list "--disable-static"
             "--enable-64bit"
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib")
             ;; Mozilla deviates from Autotools conventions
             ;; due to historical reasons.  Adjust to Mozilla conventions,
             ;; otherwise the Makefile will try to use TARGET-gcc
             ;; as a ‘native’ compiler.
             ,@(if (%current-target-system)
                   `(,(string-append "--host="
                                     (nix-system->gnu-triplet (%current-system)))
                     ,(string-append "--target=" (%current-target-system)))
                   '()))
       ;; Use fixed timestamps for reproducibility.
       #:make-flags '("SH_DATE='1970-01-01 00:00:01'"
                      ;; This is epoch 1 in microseconds.
                      "SH_NOW=100000")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'chdir
                    (lambda _ (chdir "nspr") #t)))))
    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSPR")
    (synopsis "Netscape API for system level and libc-like functions")
    (description "Netscape Portable Runtime (@dfn{NSPR}) provides a
platform-neutral API for system level and libc-like functions.  It is used
in the Mozilla clients.")
    (license license:mpl2.0)))

(define-public nspr-4.32
  (package
    (inherit nspr)
    (version "4.32")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "0v3zds1id71j5a5si42a658fjz8nv2f6zp6w4gqrqmdr6ksz8sxv"))))))

(define-public nss
  (package
    (name "nss")
    ;; Also update and test the nss-certs package, which duplicates version and
    ;; source to avoid a top-level variable reference & module cycle.
    (version "3.72")
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
                "0bnh683nij6s0gvjcgwhyw5d3yx9fpm42pxj5bm97r0ky6ghm9kf"))
              ;; Create nss.pc and nss-config.
              (patches (search-patches "nss-3.56-pkgconfig.patch"
                                       "nss-getcwd-nonnull.patch"
                                       "nss-increase-test-timeout.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete the bundled copy of these libraries.
                  (delete-file-recursively "nss/lib/zlib")
                  (delete-file-recursively "nss/lib/sqlite")))))
    (build-system gnu-build-system)
    (outputs '("out" "bin"))
    (arguments
     `(#:make-flags
       (let* ((out (assoc-ref %outputs "out"))
              (nspr (string-append (assoc-ref %build-inputs "nspr")))
              (rpath (string-append "-Wl,-rpath=" out "/lib/nss")))
         (list "-C" "nss" (string-append "PREFIX=" out)
               "NSDISTMODE=copy"
               "NSS_USE_SYSTEM_SQLITE=1"
               (string-append "NSPR_INCLUDE_DIR=" nspr "/include/nspr")
               ;; Add $out/lib/nss to RPATH.
               (string-append "RPATH=" rpath)
               (string-append "LDFLAGS=" rpath)))
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (setenv "CC" ,(cc-for-target))
             ;; Tells NSS to build for the 64-bit ABI if we are 64-bit system.
             ,@(if (target-64bit?)
                   `((setenv "USE_64" "1"))
                   '())))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (begin
                   ;; Use 127.0.0.1 instead of $HOST.$DOMSUF as HOSTADDR for
                   ;; testing.  The latter requires a working DNS or /etc/hosts.
                   (setenv "DOMSUF" "localdomain")
                   (setenv "USE_IP" "TRUE")
                   (setenv "IP_ADDRESS" "127.0.0.1")

                   ;; The "PayPalEE.cert" certificate expires every six months,
                   ;; leading to test failures:
                   ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=609734>.  To
                   ;; work around that, set the time to roughly the release date.
                   (invoke "faketime" "2021-09-30" "./nss/tests/all.sh"))
                 (format #t "test suite not run~%"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append (assoc-ref outputs "bin") "/bin"))
                    (inc (string-append out "/include/nss"))
                    (lib (string-append out "/lib/nss"))
                    (obj (match (scandir "dist" (cut string-suffix? "OBJ" <>))
                           ((obj) (string-append "dist/" obj)))))
               ;; Install nss-config to $out/bin.
               (install-file (string-append obj "/bin/nss-config")
                             (string-append out "/bin"))
               (delete-file (string-append obj "/bin/nss-config"))
               ;; Install nss.pc to $out/lib/pkgconfig.
               (install-file (string-append obj "/lib/pkgconfig/nss.pc")
                             (string-append out "/lib/pkgconfig"))
               (delete-file (string-append obj "/lib/pkgconfig/nss.pc"))
               (rmdir (string-append obj "/lib/pkgconfig"))
               ;; Install other files.
               (copy-recursively "dist/public/nss" inc)
               (copy-recursively (string-append obj "/bin") bin)
               (copy-recursively (string-append obj "/lib") lib)))))))
    (inputs
     (list sqlite zlib))
    (propagated-inputs
     (list nspr))                 ;required by nss.pc.
    (native-inputs
     (list perl libfaketime))   ;for tests

    ;; The NSS test suite takes around 48 hours on Loongson 3A (MIPS) when
    ;; another build is happening concurrently on the same machine.
    (properties '((timeout . 216000)))  ;60 hours

    (home-page "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSS")
    (synopsis "Network Security Services")
    (description
     "Network Security Services (@dfn{NSS}) is a set of libraries designed to
support cross-platform development of security-enabled client and server
applications.  Applications built with NSS can support SSL v2 and v3, TLS,
PKCS #5, PKCS #7, PKCS #11, PKCS #12, S/MIME, X.509 v3 certificates, and other
security standards.")
    (license license:mpl2.0)))
