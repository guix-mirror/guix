;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Leo Prikler <leo.prikler@student.tugraz.at>
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

(define-module (gnu packages unicode)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial))

(define (unicode-emoji-file name version hash)
  (origin
    (method url-fetch)
    (uri (string-append "https://www.unicode.org/Public/emoji/"
                        version
                        "/emoji-" name ".txt"))
    (sha256 (base32 hash))))

(define-public unicode-emoji
  (package
    (name "unicode-emoji")
    (version "12.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let ((out (string-append %output "/share/unicode/emoji")))
         (use-modules (guix build utils))
         (mkdir-p out)
         (for-each
          (lambda (input)
            (copy-file
             (cdr input)
             (string-append out "/"
                            (substring (car input) 8) ; strip "unicode-"
                            ".txt")))
          %build-inputs)
         #t)))
    (inputs
     `(("unicode-emoji-data"
        ,(unicode-emoji-file
          "data" version
          "03sf7h1d6kb9m5s02lif88jsi5kjszpkfvcymaqxj8ds70ar9pgv"))
       ("unicode-emoji-sequences"
        ,(unicode-emoji-file
          "sequences" version
          "1hghki2rn3n7m4lwpwi2a5wrsf2nij4bxga9ldabx4g0g2k23svs"))
       ("unicode-emoji-test"
        ,(unicode-emoji-file
          "test" version
          "1dqd0fh999mh6naj816ni113m9dimfy3ih9nffjq2lrv9mmlgdck"))
       ("unicode-emoji-variation-sequences"
        ,(unicode-emoji-file
          "variation-sequences" version
          "1cccwx5bl79w4c19vi5dhjqxrph92s8hihp9y8s2cqvdzmgbln7a"))
       ("unicode-emoji-zwj-sequences"
        ,(unicode-emoji-file
          "zwj-sequences" version
          "1l791nbijmmhwa7kmvfn8gp26ban512l6mgqpz1mnbq3xm19181n"))))
    (home-page "https://www.unicode.org")
    (synopsis "Unicode Emoji data")
    (description
     "This package includes data files listing characters and sequences, that
Unicode emoji supporting fonts or keyboards should support according to the
Unicode Technological Standard #51.")
    (license unicode)))

(define-public unicode-cldr-common
  (package
    (name "unicode-cldr-common")
    (version "36.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://unicode.org/Public/cldr/"
                           (version-major version)
                           "/cldr-common-" version ".zip"))
       (sha256
        (base32
         "0hxsc3j5zb32hmiaj0r3ajchmklx6zng6zlh1ca6s9plq5b9w9q7"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let ((out (string-append %output "/share/unicode/cldr/common")))
         (use-modules (guix build utils))
         (mkdir-p out)
         (copy-recursively (string-append (assoc-ref %build-inputs "source")
                                          "/common")
                           out)
         #t)))
    (home-page "https://www.unicode.org")
    (synopsis "Locale data repository")
    (description
     "The Unicode Common Locale Data Repository (CLDR) is a large repository
of locale data, including among others

@itemize
@item patterns for formatting and parsing,
@item name translations,
@item and various informations on languages, scripts and country-specific
  conventions.
@end itemize\n")
    (license unicode)))
