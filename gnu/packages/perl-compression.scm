;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

(define-module (gnu packages perl-compression)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check))

(define-public perl-compress-bzip2
  (package
    (name "perl-compress-bzip2")
    (version "2.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RU/RURBAN/"
                           "Compress-Bzip2-" version ".tar.gz"))
       (sha256
        (base32 "0vhi6nqayvg6wz55bynccv5xd6gzhbigk9mjv088i6aw7xf877w5"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-bzip2
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bzip2 (assoc-ref inputs "bzip2")))
               (setenv "BUILD_BZLIB" "0")
               (setenv "BZLIB_BIN" (string-append bzip2 "/bin"))
               (setenv "BZLIB_INCLUDE" (string-append bzip2 "/include"))
               (setenv "BZLIB_LIB" (string-append bzip2 "/lib"))
               #t))))))
    (inputs
     (list bzip2))
    (home-page "https://metacpan.org/release/Compress-Bzip2")
    (synopsis "Interface to Bzip2 compression library")
    (description
     "The Compress::Bzip2 module provides a Perl interface to the Bzip2
compression library.  A relevant subset of the functionality provided by Bzip2
is available in this module.")
    (license license:perl-license)))

(define-public perl-compress-raw-bzip2
  (package
    (name "perl-compress-raw-bzip2")
    (version "2.096")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PM/PMQS/"
                           "Compress-Raw-Bzip2-" version ".tar.gz"))
       (sha256
        (base32 "1glcjnbqksaviwyrprh9i4dybsb12kzfy0bx932l0xya9riyfr55"))))
    (build-system perl-build-system)
    ;; TODO: Use our bzip2 package.
    (home-page "https://metacpan.org/release/Compress-Raw-Bzip2")
    (synopsis "Low-level interface to bzip2 compression library")
    (description "This module provides a Perl interface to the bzip2
compression library.")
    (license license:perl-license)))

(define-public perl-compress-raw-zlib
  (package
    (name "perl-compress-raw-zlib")
    (version "2.096")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PM/PMQS/"
                           "Compress-Raw-Zlib-" version ".tar.gz"))
       (sha256
        (base32 "04jrqvqsa2c655idw7skv5rhb9vx9997h4n9if5p99srq4hblk6d"))))
    (build-system perl-build-system)
    (inputs
     (list zlib))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before
                   'configure 'configure-zlib
                   (lambda* (#:key inputs #:allow-other-keys)
                     (call-with-output-file "config.in"
                       (lambda (port)
                         (format port "
BUILD_ZLIB = False
INCLUDE = ~a/include
LIB = ~:*~a/lib
OLD_ZLIB = False
GZIP_OS_CODE = AUTO_DETECT"
                                 (assoc-ref inputs "zlib"))))
                     #t)))))
    (home-page "https://metacpan.org/release/Compress-Raw-Zlib")
    (synopsis "Low-level interface to zlib compression library")
    (description "This module provides a Perl interface to the zlib
compression library.")
    (license license:perl-license)))

(define-public perl-io-compress
  (package
    (name "perl-io-compress")
    (version "2.096")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PM/PMQS/"
                           "IO-Compress-" version ".tar.gz"))
       (sha256
        (base32 "0wm51dib74l9baqfkfm0ixsr4cqwrgij2yc45xfhnjabvzary8cx"))))
    (build-system perl-build-system)
    (propagated-inputs
     ;; These two packages should be updated to this one's version first.
     (list perl-compress-raw-zlib perl-compress-raw-bzip2))
    (home-page "https://metacpan.org/release/IO-Compress")
    (synopsis "IO Interface to compressed files/buffers")
    (description "IO-Compress provides a Perl interface to allow reading and
writing of compressed data created with the zlib and bzip2 libraries.")
    (license license:perl-license)))

(define-public perl-perlio-gzip
  (package
    (name "perl-perlio-gzip")
    (version "0.20")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/N/NW/NWCLARK/PerlIO-gzip-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1idxsdmpinsp6qm1lizs7y955bq2wqk9avsz1hxky7i07yd6fj28"))))
    (build-system perl-build-system)
    (arguments
     `(#:make-maker-flags
       ;; MakeMaker doesn't honor LIBRARY_PATH.
       (let ((zlib (assoc-ref %build-inputs "zlib")))
         (list (format #f "LIBS=-L~a/lib/ -lz" zlib)))))
    (inputs (list zlib))
    (home-page "https://metacpan.org/pod/PerlIO::gzip")
    (synopsis "Perl extension to provide a PerlIO layer to gzip/gunzip")
    (description "PerlIO::gzip provides a PerlIO layer that manipulates files
in the format used by the @command{gzip} program.")
    (license license:perl-license)))

(define-public perl-archive-extract
  (package
    (name "perl-archive-extract")
    (version "0.88")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BI/BINGOS/Archive-Extract-"
                           version ".tar.gz"))
       (sha256
        (base32 "0hpplmgf4j0rc9k3kl5fzi4m8j8ncxyly5827dyjh8h6rlsz3z6g"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Archive-Extract")
    (synopsis "Generic archive extracting mechanism")
    (description "It allows you to extract any archive file of the type .tar,
.tar.gz, .gz, .Z, tar.bz2, .tbz, .bz2, .zip, .xz,, .txz, .tar.xz or .lzma
without having to worry how it does so, or use different interfaces for each
type by using either Perl modules, or command-line tools on your system.")
    (license license:perl-license)))

(define-public perl-archive-zip
  (package
    (name "perl-archive-zip")
    (version "1.68")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/P/PH/PHRED/Archive-Zip-"
             version ".tar.gz"))
       (sha256
        (base32 "0l663s3a68p8r2qjy4pn1g05lx0i8js8wpz7qqln3bsvg1fihklq"))))
    (build-system perl-build-system)
    (native-inputs
     ;; For tests.
     (list perl-test-mockmodule))
    (synopsis  "Provides an interface to Zip archive files")
    (description "The @code{Archive::Zip} module allows a Perl program to
create, manipulate, read, and write Zip archive files.")
    (home-page "https://metacpan.org/release/Archive-Zip")
    (license license:perl-license)))
