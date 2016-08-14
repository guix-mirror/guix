;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages regex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public re2
   (package
     (name "re2")
     (version "2016-08-01")
     (source (origin
               (method url-fetch)
               (uri
                (string-append
                 "https://github.com/google/re2/archive/"
                 version ".tar.gz"))
               (file-name (string-append name "-" version ".tar.gz"))
               (sha256
                (base32
                 "06pfm3xi5irrrij85m0c46rsn9jyg1rc2r431wi2knhjvbw9f0bx"))))
     (build-system gnu-build-system)
     (arguments
      `(#:test-target "test"
        ;; There is no configure step, but the Makefile respects a prefix.
        #:make-flags (list (string-append "prefix=" %output))
        #:phases
        (modify-phases %standard-phases
          (delete 'configure)
          (add-after 'install 'delete-static-library
            (lambda* (#:key outputs #:allow-other-keys)
              ;; No make target for shared-only; delete the static version.
              (delete-file (string-append (assoc-ref outputs "out")
                                          "/lib/libre2.a")))))))
     (home-page "https://github.com/google/re2")
     (synopsis "Fast, safe, thread-friendly regular expression engine")
     (description "RE2 is a fast, safe, thread-friendly alternative to
backtracking regular expression engines like those used in PCRE, Perl and
Python.  It is a C++ library.")
     (license license:bsd-3)))
