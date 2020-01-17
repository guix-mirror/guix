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
