;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages datamash)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages which))

(define-public datamash
  (package
    (name "datamash")
    (version "1.0.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/datamash/datamash-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0f4rbbhl18fb851npza9cl8biynzr081f37ih2xsbvjlyrxhnz6b"))))
    (native-inputs
     `(("which" ,which)                 ;for tests
       ("perl" ,perl)))                 ;for help2man
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/datamash/")
    (synopsis "Scriptable statistics and data calculation")
    (description
     "Perform basic numeric, textual and statistical operations on plain text
files.  Designed to work within standard pipelines without additional code.")
    (license gpl3)))
