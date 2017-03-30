;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages gperf)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public gperf
  (package
    (name "gperf")
    (version "3.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gperf/gperf-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1qispg6i508rq8pkajh26cznwimbnj06wq9sd85vg95v8nwld1aq"))))
    (build-system gnu-build-system)
    (arguments '(#:parallel-tests? #f))
    (home-page "https://www.gnu.org/software/gperf/")
    (synopsis "Perfect hash function generator")
    (description
     "gperf is a perfect hash function generator.  For a given list of
strings, it produces a hash function and hash table in C or C++ code.  That
the hash function is perfect means that no collisions can exist and that
look-ups can be made by single string comparisons.")
    (license gpl3+)))

(define-public gperf-3.0
  ;; This older version would use 'unsigned int' in its generated lookup
  ;; functions whereas 3.1 uses 'size_t', which causes breakage such as
  ;; <https://github.com/wingo/elogind/issues/8>.
  (package
    (inherit gperf)
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gperf/gperf-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0gnnm8iqcl52m8iha3sxrzrl9mcyhg7lfrhhqgdn4zj00ji14wbn"))))))
