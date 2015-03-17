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

(define-public perl-lingua-en-inflect-number
  (package
    (name "perl-lingua-en-inflect-number")
    (version "1.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Lingua-EN-Inflect-Number-" version ".tar.gz"))
       (sha256
        (base32
         "0rvgrff96ja7fqr79dszmiyv2wz4izw82znah0mx9szkir657gfz"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-lingua-en-inflect" ,perl-lingua-en-inflect)))
    (home-page "http://search.cpan.org/dist/Lingua-EN-Inflect-Number")
    (synopsis "Force number of words to singular or plural")
    (description "This module extends the functionality of Lingua::EN::Inflect
with three new functions for determining plurality of a word and forcefully
converting a word to singular or plural.")
    (license (package-license perl))))

(define-public perl-lingua-en-number-isordinal
  (package
    (name "perl-lingua-en-number-isordinal")
    (version "0.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RK/RKITOVER/"
                           "Lingua-EN-Number-IsOrdinal-" version ".tar.gz"))
       (sha256
        (base32
         "1321fm4pyqcamffd0qsjm1gb07ijqndlb29qkcqf22kaibngfm4i"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-try-tiny" ,perl-try-tiny)
       ("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-lingua-en-findnumber" ,perl-lingua-en-findnumber)))
    (home-page "http://search.cpan.org/dist/Lingua-EN-Number-IsOrdinal")
    (synopsis "Detect if English number is ordinal or cardinal")
    (description "This module will tell you if a number, either in words or as
digits, is a cardinal or ordinal number.")
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

(define-public perl-lingua-pt-stemmer
  (package
    (name "perl-lingua-pt-stemmer")
    (version "0.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/X/XE/XERN/"
                           "Lingua-PT-Stemmer-" version ".tar.gz"))
       (sha256
        (base32
         "11rqc5pqnkl9c13vy7sihiyas14ci0pj3k6chrgrgjv5sjv2m4a5"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Lingua-PT-Stemmer")
    (synopsis "Portuguese language stemming")
    (description "This module implements a Portuguese stemming algorithm
proposed in the paper A Stemming Algorithm for the Portuguese Language by
Moreira, V. and Huyck, C.")
    (license (package-license perl))))

(define-public perl-lingua-stem-fr
  (package
    (name "perl-lingua-stem-fr")
    (version "0.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SD/SDP/"
                           "Lingua-Stem-Fr-" version ".tar.gz"))
       (sha256
        (base32
         "0vyrspwzaqjxm5mqshf4wvwa3938mkajd1918d9ii2l9m2rn8kwx"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Lingua-Stem-Fr")
    (synopsis "Porter's stemming algorithm for French")
    (description "This module uses a modified version of the Porter Stemming
Algorithm to return a stemmed French word.")
    (license (package-license perl))))

(define-public perl-lingua-stem-it
  (package
    (name "perl-lingua-stem-it")
    (version "0.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AC/ACALPINI/"
                           "Lingua-Stem-It-" version ".tar.gz"))
       (sha256
        (base32
         "1207r183s5hlh4mfwa6p46vzm0dhvrs2dnss5s41a0gyfkxp7riq"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Lingua-Stem-It")
    (synopsis "Porter's stemming algorithm for Italian")
    (description "This module applies the Porter Stemming Algorithm to its
parameters, returning the stemmed Italian word.")
    (license (package-license perl))))

(define-public perl-lingua-stem-ru
  (package
    (name "perl-lingua-stem-ru")
    (version "0.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AL/ALGDR/"
                           "Lingua-Stem-Ru-" version ".tar.gz"))
       (sha256
        (base32
         "0pqgg442fkf12ayh9fgmpa8q9x0iqx6s96181r52yn7s7pcs61h6"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Lingua-Stem-Ru")
    (synopsis "Porter's stemming algorithm for Russian")
    (description "This module applies the Porter Stemming Algorithm to its
parameters, returning the stemmed Russian (KOI8-R only) word.")
    (license (package-license perl))))
