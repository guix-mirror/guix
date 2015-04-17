;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages textutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages python))

(define-public recode
  (package
    (name "recode")
    ;; Last beta release (3.7-beta2) is from 2008; last commit from Feb 2014.
    ;; So we use that commit instead.
    (version "3.7.0.201402")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pinard/Recode.git")
             (commit "2d7092a9999194fc0e9449717a8048c8d8e26c18")))
       (sha256
        (base32 "1wssv8z6g3ryrw33sksz4rjhlnhgvvdqszw1ggl4rcwks34n86zm"))))
    (build-system gnu-build-system)
    (native-inputs `(("python" ,python-2)))
    (arguments
     '(#:phases
       (alist-cons-before
        'check 'pre-check
        (lambda _
          (substitute* "tests/setup.py"
            (("([[:space:]]*)include_dirs=.*" all space)
             (string-append all space "library_dirs=['../src/.libs'],\n")))
          ;; The test extension 'Recode.so' lacks RUNPATH for 'librecode.so'.
          (setenv "LD_LIBRARY_PATH" (string-append (getcwd) "/src/.libs")))
        %standard-phases)))
    (home-page "https://github.com/pinard/Recode")
    (synopsis "Text encoding converter")
    (description "The Recode library converts files between character sets and
usages.  It recognises or produces over 200 different character sets (or about
300 if combined with an iconv library) and transliterates files between almost
any pair.  When exact transliteration are not possible, it gets rid of
offending characters or falls back on approximations.  The recode program is a
handy front-end to the library.")
    (license license:gpl2+)))

(define-public enca
  (package
    (name "enca")
    (version "1.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/nijel/enca/archive/" version ".tar.gz"))
       (sha256
        (base32 "1xik00x0yvhswsw2isnclabhv536xk1s42cf5z54gfbpbhc7ni8l"))))
    (build-system gnu-build-system)
    (inputs `(("recode" ,recode)))

    ;; Both 'test-convert-64.sh' and 'test-convert-filter.sh' manipulate a
    ;; 'test.tmp' file, so they have to run in sequence.
    (arguments '(#:parallel-tests? #f))

    (home-page "https://github.com/nijel/enca")
    (synopsis "Text encoding detection tool")
    (description "Enca (Extremely Naive Charset Analyser) consists of libenca,
an encoding detection library, and enca, a command line frontend, integrating
libenca and several charset conversion libraries and tools.")
    (license license:gpl2)))

(define-public utf8proc
  (package
    (name "utf8proc")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JuliaLang/utf8proc/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wmsi672knii0q70wh6a3ll0gv7qk33c50zbpzasrs3b16bqy659"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:make-flags '("CC=gcc")
       #:phases
       (alist-replace
        'install
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((lib (string-append (assoc-ref outputs "out") "/lib/"))
                (include (string-append (assoc-ref outputs "out") "/include/")))
            (mkdir-p lib)
            (mkdir-p include)
            (copy-file "utf8proc.h" (string-append include "utf8proc.h"))
            (for-each (lambda (file)
                        (copy-file file (string-append lib (basename file))))
                      '("libutf8proc.a" "libutf8proc.so"))))
        ;; no configure script
        (alist-delete 'configure %standard-phases))))
    (home-page "http://julialang.org/utf8proc/")
    (synopsis "C library for processing UTF-8 Unicode data")
    (description "utf8proc is a small C library that provides Unicode
normalization, case-folding, and other operations for data in the UTF-8
encoding, supporting Unicode version 7.0.")
    (license license:expat)))

(define-public libgtextutils
  (package
    (name "libgtextutils")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/agordon/libgtextutils/releases/download/"
             version "/libgtextutils-" version ".tar.gz"))
       (sha256
        (base32 "0jiybkb2z58wa2msvllnphr4js2hvjvh988pavb3mzkgr6ihwbkr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (alist-cons-after
        'unpack 'autoreconf
        (lambda _ (zero? (system* "autoreconf" "-vif")))
        %standard-phases)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://github.com/agordon/libgtextutils")
    (synopsis "Gordon's text utils library")
    (description
     "libgtextutils is a text utilities library used by the fastx toolkit from
the Hannon Lab.")
    (license license:agpl3+)))
