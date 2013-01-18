;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Andreas Enge <andreas@enge.fr>
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
  #:use-module (guix build-system gnu))

(define-public mit-krb5
  (package
   (name "mit-krb5")
   (version "1.11")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://web.mit.edu/kerberos/www/dist/krb5/"
                                version
                                "/krb5-" version
                                "-signed.tar"))
            (sha256 (base32
                     "0lc6lxb98qzg4x01lppq700vkr1ax9rld09znahrinwqnf9zndzy"))))
   (build-system gnu-build-system)
   (inputs `(("bison" ,bison)
             ("perl" ,perl)
             ))
   (arguments
    (lambda (system)
      `(#:tests? #f
        #:phases
        (alist-replace
         'unpack
         (lambda* (#:key source #:allow-other-keys)
          (system* "echo" source)
          (let ((inner
                 (substring source
                            (string-index-right source #\k)
                            (string-index-right source #\-))))
           (system* "echo" inner)
           (and (zero? (system* "tar" "xvf" source))
                (zero? (system* "tar" "xvf" (string-append inner ".tar.gz")))
                (chdir inner)
                (chdir "src"))))
          %standard-phases))))
   (synopsis "MIT Kerberos 5")
   (description
    "Massachusetts Institute of Technology implementation of Kerberos.
Kerberos is a network authentication protocol designed to provide strong
authentication for client/server applications by using secret-key cryptography.")
   (license (bsd-style "file://NOTICE"
                       "See NOTICE in the distribution."))
   (home-page "http://web.mit.edu/kerberos/")))
