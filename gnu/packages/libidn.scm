;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages libidn)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libidn
  (package
   (name "libidn")
   (replacement libidn-1.33)
   (version "1.32")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libidn/libidn-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1xf4hphhahcjm2xwx147lfpsavjwv9l4c2gf6hx71zxywbz5lpds"))))
   (build-system gnu-build-system)
;; FIXME: No Java and C# libraries are currently built.
   (synopsis "Internationalized string processing library")
   (description
     "libidn is a library implementing of the Stringprep, Punycode and IDNA
specifications.  These are used to encode and decode internationalized domain
names.  It includes native C, C# and Java libraries.")
   ;; The C code is dual-licensed gpl2+ lgpl3+, the manual is fdl1.3+,
   ;; the command line tool is gpl3+.
   (license (list gpl2+ gpl3+ lgpl3+ fdl1.3+))
   (home-page "http://www.gnu.org/software/libidn/")))

(define libidn-1.33
  (package
    (inherit libidn)
    (source
      (let ((version "1.33"))
        (origin
          (method url-fetch)
          (uri (string-append "mirror://gnu/libidn/libidn-" version
                              ".tar.gz"))
          (sha256
           (base32
            "068fjg2arlppjqqpzd714n1lf6gxkpac9v5yyvp1qwmv6nvam9s4")))))))
