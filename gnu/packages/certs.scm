;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages certs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages python))

(define certdata2pem
  (package
    (name "certdata2pem")
    (version "2013")
    (source
    (origin
      (method url-fetch)
        (uri
          "http://pkgs.fedoraproject.org/cgit/ca-certificates.git/plain/certdata2pem.py?id=053dde8a2f5901e97028a58bf54e7d0ef8095a54")
        (sha256
          (base32
            "0zscrm41gnsf14zvlkxhy00h3dmgidyz645ldpda3y3vabnwv8dx"))))
   (build-system trivial-build-system)
   (inputs
     `(("python" ,python-2)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
        (begin
          (use-modules (guix build utils))
          (let ((bin (string-append %output "/bin")))
            (copy-file (assoc-ref %build-inputs "source") "certdata2pem.py")
            (chmod "certdata2pem.py" #o555)
            (substitute* "certdata2pem.py"
              (("/usr/bin/python")
               (string-append (assoc-ref %build-inputs "python")
                              "/bin/python"))
              ;; Use the file extension .pem instead of .crt.
              (("crt") "pem"))
            (mkdir-p bin)
            (copy-file "certdata2pem.py"
                       (string-append bin "/certdata2pem.py"))))))
   (synopsis "Python script to extract .pem data from certificate collection")
   (description
    "certdata2pem.py is a Python script to transform X.509 certificate
\"source code\" as contained, for example, in the Mozilla sources, into
.pem formatted certificates.")
   (license license:gpl2+)
   (home-page "http://pkgs.fedoraproject.org/cgit/ca-certificates.git/")))
