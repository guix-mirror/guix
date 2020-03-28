;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-public perl-compress-raw-bzip2
  (package
    (name "perl-compress-raw-bzip2")
    (version "2.091")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PM/PMQS/"
                           "Compress-Raw-Bzip2-" version ".tar.gz"))
       (sha256
        (base32 "0c5lwaynkcprjd18d3c82m8cniyq3c8mc4jk42gb32lp7cyyzmr7"))))
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
    (version "2.091")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PM/PMQS/"
                           "Compress-Raw-Zlib-" version ".tar.gz"))
       (sha256
        (base32 "0vrzk1jzw2kwrsvnd190m7jl925cbd4skp6ipv5pndkn4y8gnzxn"))))
    (build-system perl-build-system)
    (inputs
     `(("zlib" ,zlib)))
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
    (version "2.091")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PM/PMQS/"
                           "IO-Compress-" version ".tar.gz"))
       (sha256
        (base32 "15ayljvmxszkd5bvzd52l7gv71ldj8bhdqdnj9f48dipn681238z"))))
    (build-system perl-build-system)
    (propagated-inputs
     ;; These two packages should be updated to this one's version first.
     `(("perl-compress-raw-zlib" ,perl-compress-raw-zlib)
       ("perl-compress-raw-bzip2" ,perl-compress-raw-bzip2)))
    (home-page "https://metacpan.org/release/IO-Compress")
    (synopsis "IO Interface to compressed files/buffers")
    (description "IO-Compress provides a Perl interface to allow reading and
writing of compressed data created with the zlib and bzip2 libraries.")
    (license license:perl-license)))

(define-public perl-archive-extract
  (package
    (name "perl-archive-extract")
    (version "0.82")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BI/BINGOS/Archive-Extract-"
                           version ".tar.gz"))
       (sha256
        (base32 "00zq2ishxk70192sjl1wciq14vhb9isz8rcafcy1j89va1srpvrn"))))
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
     `(("perl-test-mockmodule" ,perl-test-mockmodule)))
    (synopsis  "Provides an interface to Zip archive files")
    (description "The @code{Archive::Zip} module allows a Perl program to
create, manipulate, read, and write Zip archive files.")
    (home-page "https://metacpan.org/release/Archive-Zip")
    (license license:perl-license)))
