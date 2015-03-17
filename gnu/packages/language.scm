;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages language)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (guix packages)
  #:use-module (guix build-system perl)
  #:use-module (guix download))

(define-public perl-lingua-en-findnumber
  (package
    (name "perl-lingua-en-findnumber")
    (version "1.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Lingua-EN-FindNumber-" version ".tar.gz"))
       (sha256
        (base32
         "0g5h6bwwggizwh3dd3xyx92537s4qy8nspshp1lrm9qlxh3prc28"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-lingua-en-words2nums" ,perl-lingua-en-words2nums)))
    (home-page "http://search.cpan.org/dist/Lingua-EN-FindNumber")
    (synopsis "Locate (written) numbers in English text ")
    (description "This module provides a regular expression for finding
numbers in English text.  It also provides functions for extracting and
manipulating such numbers.")
    (license (package-license perl))))

(define-public perl-lingua-en-inflect
  (package
    (name "perl-lingua-en-inflect")
    (version "1.895")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DC/DCONWAY/"
                           "Lingua-EN-Inflect-" version ".tar.gz"))
       (sha256
        (base32
         "0drzg9a2dkjxgf00n6jg0jzhd8972bh3j4wdnmdxpqi3zmfqhwcy"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Lingua-EN-Inflect")
    (synopsis "Convert singular to plural")
    (description "Lingua::EN::Inflect provides plural inflections,
\"a\"/\"an\" selection for English words, and manipulation of numbers as
words.  Plural forms of all nouns, most verbs, and some adjectives are
provided.  Where appropriate, \"classical\" variants (for example: \"brother\"
-> \"brethren\", \"dogma\" -> \"dogmata\", etc.) are also provided.")
    (license (package-license perl))))

(define-public perl-lingua-en-words2nums
  (package
    (name "perl-lingua-en-words2nums")
    (version "0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JO/JOEY/"
                           "Lingua-EN-Words2Nums-" version ".tar.gz"))
       (sha256
        (base32
         "118xx8qr1zbx30psv7ic55w65h15mc1vz6zicshfm96jgiwmcrb8"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Lingua-EN-Words2Nums")
    (synopsis "Convert English text to numbers")
    (description "This module converts English text into numbers.  It supports
both ordinal and cardinal numbers, negative numbers, and very large numbers.")
    (license (package-license perl))))
