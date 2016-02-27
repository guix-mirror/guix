;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages mit-krb5)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages perl)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public mit-krb5
  (package
    (name "mit-krb5")
    (version "1.13.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://web.mit.edu/kerberos/dist/krb5/"
                                  (version-major+minor version)
                                  "/krb5-" version ".tar.gz"))
              (sha256
               (base32
                "1gpscn78lv48dxccxq9ncyj53w9l2a15xmngjfa1wylvmn7g0jjx"))
              (patches
               (map search-patch '("mit-krb5-init-context-null-spnego.patch"
                                   "mit-krb5-CVE-2015-8629.patch"
                                   "mit-krb5-CVE-2015-8630.patch"
                                   "mit-krb5-CVE-2015-8631.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("perl" ,perl)))
    (arguments
     `(#:phases
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
                 (("D/bin/sh") (string-append "D" (which "bash")))
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
    (license (non-copyleft "file://NOTICE"
                           "See NOTICE in the distribution."))
    (home-page "http://web.mit.edu/kerberos/")))
