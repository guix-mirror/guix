;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils))

(define-public re2
   (package
     (name "re2")
     (version "2019-03-01")
     (home-page "https://github.com/google/re2")
     (source (origin
               (method git-fetch)
               (uri (git-reference (url home-page) (commit version)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "032lsrb57llq79icpvrrh1fmjzy2i4bdmydfww178nc45ya5wcl4"))))
     (build-system gnu-build-system)
     (arguments
      `(#:modules ((guix build gnu-build-system)
                   (guix build utils)
                   (srfi srfi-1))
        #:test-target "test"
        ;; There is no configure step, but the Makefile respects a prefix.
        ;; As ./configure does not know anything about the target CXX
        ;; we need to specify TARGET-g++ explicitly.
        #:make-flags (list (string-append "prefix=" %output)
                           (string-append
                             "CXX=" ,(string-append
                                       (if (%current-target-system)
                                           (string-append
                                             (%current-target-system) "-")
                                           "")
                                       "g++")))
        #:phases
        (modify-phases %standard-phases
          (delete 'configure)
          (add-after 'install 'delete-static-library
            (lambda* (#:key outputs #:allow-other-keys)
              ;; No make target for shared-only; delete the static version.
              (delete-file (string-append (assoc-ref outputs "out")
                                          "/lib/libre2.a"))
              #t)))))
     (synopsis "Fast, safe, thread-friendly regular expression engine")
     (description "RE2 is a fast, safe, thread-friendly alternative to
backtracking regular expression engines like those used in PCRE, Perl and
Python.  It is a C++ library.")
     (license license:bsd-3)))

(define-public tre
  (package
    (name "tre")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://laurikari.net/tre/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0n36cgqys59r2gmb7jzbqiwsy790v8nbxk82d2n2saz0rp145ild"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'install-locales
           (lambda _
             ;; The tests require the availability of the
             ;; 'en_US.ISO-8859-1' locale.
             (setenv "LOCPATH" (getcwd))
             (invoke "localedef" "--no-archive"
                     "--prefix" (getcwd) "-i" "en_US"
                     "-f" "ISO-8859-1" "./en_US.ISO-8859-1"))))))
    (synopsis "Approximate regex matching library and agrep utility")
    (description "Superset of the POSIX regex API, enabling approximate
matching.  Also ships a version of the agrep utility which behaves similar to
grep but features inexact matching.")
    (home-page "http://laurikari.net/tre")
    (license license:bsd-2)))
