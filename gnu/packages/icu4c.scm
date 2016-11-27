;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages icu4c)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public icu4c
  (package
   (name "icu4c")
   (version "58.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://download.icu-project.org/files/icu4c/"
                  version
                  "/icu4c-"
                  (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                  "-src.tgz"))
            (sha256
             (base32 "0283lcgb39v7hsl16bbp8h8r9d51n4lpld5dr292173afjinpd0f"))))
   (build-system gnu-build-system)
   (inputs
    `(("perl" ,perl)))
   (arguments
    `(#:configure-flags
      '("--enable-rpath"
        ,@(if (let ((s (or (%current-target-system)
                           (%current-system))))
                (or (string-prefix? "arm" s)
                    (string-prefix? "mips" s)))
              '("--with-data-packaging=archive")
              '()))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'chdir-to-source
          (lambda _ (chdir "source") #t)))))
   (synopsis "International Components for Unicode")
   (description
    "ICU is a set of C/C++ and Java libraries providing Unicode and
globalisation support for software applications.  This package contains the
C/C++ part.")
   (license x11)
   (home-page "http://site.icu-project.org/")))
