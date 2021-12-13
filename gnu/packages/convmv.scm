;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (gnu packages convmv)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public convmv
  (package
    (name "convmv")
    (version "2.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.j3e.de/linux/convmv/convmv-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "19hwv197p7c23f43vvav5bs19z9b72jzca2npkjsxgprwj5ardjk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags `(,(string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (add-before 'check 'unpack-testsuite
           (lambda _
             (invoke "tar" "xf" "testsuite.tar")
             (patch-shebang "suite/dotests.sh")
             (patch-shebang "suite/parsable_tester.pl")
             #t)))))
    (inputs
     (list perl))
    (synopsis "Convert filenames between character sets")
    (description
     "convmv is a file renamer, that converts between different encodings,
e.g. from ISO-8859-1 to UTF-8.  It is particularly usefuls for files with
names, that display incorrectly.")
    (license (list gpl2 gpl3))
    (home-page "https://www.j3e.de/")))
