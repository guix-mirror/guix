;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Andreas Enge <andreas@enge.fr>
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
  #:use-module (gnu packages gcc)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public mit-krb5
  (package
    (name "mit-krb5")
    (version "1.11.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://web.mit.edu/kerberos/www/dist/krb5/"
                                  (string-copy version 0 (string-rindex version #\.))
                                  "/krb5-" version "-signed.tar"))
              (sha256 (base32
                       "1daiaxgkxcryqs37w28v4x1vajqmay4l144d1zd9c2d7jjxr9gcs"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("patch/init-fix" ,(search-patch "mit-krb5-init-fix.patch"))
       ("bison" ,bison)
       ("perl" ,perl)

       ;; XXX: When built with GCC 4.8, the 'db_test' test program enters an
       ;; infinite loop.  As a stopgap measure, build with GCC 4.7.
       ("gcc" ,gcc-4.7)))
    (arguments
     '(#:phases
       (alist-replace
        'unpack
        (lambda* (#:key source #:allow-other-keys)
          (let ((inner
                 (substring source
                            (string-index-right source #\k)
                            (string-index-right source #\-))))
            (and (zero? (system* "tar" "xvf" source))
                 (zero? (system* "tar" "xvf" (string-append inner ".tar.gz")))
                 (chdir inner)
                 (chdir "src")
                 ;; XXX The current patch system does not support unusual
                 ;; source unpack methods, so we have to apply this patch in a
                 ;; non-standard way.
                 (zero? (system* "patch" "-p1" "--force" "-i"
                                 (assoc-ref %build-inputs "patch/init-fix"))))))
        (alist-replace
         'check
         (lambda* (#:key inputs #:allow-other-keys #:rest args)
           (let ((perl (assoc-ref inputs "perl"))
                 (check (assoc-ref %standard-phases 'check)))
             (substitute* "plugins/kdb/db2/libdb2/test/run.test"
               (("/bin/cat") (string-append perl "/bin/perl")))
             (substitute* "plugins/kdb/db2/libdb2/test/run.test"
               (("D/bin/sh") (string-append "D" (which "bash"))))
             (substitute* "plugins/kdb/db2/libdb2/test/run.test"
               (("bindir=/bin/.") (string-append "bindir=" perl "/bin")))
             ;; use existing files and directories in test
             (substitute* "tests/resolve/Makefile"
               (("-p telnet") "-p 23"))
             ;; avoid service names since /etc/services is unavailable
             (apply check args)))
         %standard-phases))))
    (synopsis "MIT Kerberos 5")
    (description
     "Massachusetts Institute of Technology implementation of Kerberos.
Kerberos is a network authentication protocol designed to provide strong
authentication for client/server applications by using secret-key cryptography.")
    (license (bsd-style "file://NOTICE"
                        "See NOTICE in the distribution."))
    (home-page "http://web.mit.edu/kerberos/")))
