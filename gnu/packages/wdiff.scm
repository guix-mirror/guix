;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages wdiff)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages base))

(define-public wdiff
  (package
    (name "wdiff")
    (version "1.2.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/wdiff/wdiff-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0sxgg0ms5lhi4aqqvz1rj4s77yi9wymfm3l3gbjfd1qchy66kzrl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'fix-sh
                    (lambda _
                      (substitute* "tests/testsuite"
                        (("#! /bin/sh")
                         (string-append "#!" (which "sh")))))))))
    (native-inputs
     (list which
           ;; For some reason wdiff.info gets rebuilt.
           texinfo))
    (home-page "https://www.gnu.org/software/wdiff/")
    (synopsis "Word difference finder")
    (description
     "GNU Wdiff is a front-end to the diff program from Diffutils that
allows you to compare files on a word-by-word basis, where a word is anything
between whitespace.")
    (license gpl3+)))
