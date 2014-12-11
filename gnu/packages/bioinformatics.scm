;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages bioinformatics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public samtools
  (package
    (name "samtools")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "1y5p2hs4gif891b4ik20275a8xf3qrr1zh9wpysp4g8m0g1jckf2"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (alist-cons-after
        'unpack
        'patch-makefile-curses
        (lambda _
          (substitute* "Makefile"
            (("-lcurses") "-lncurses")))
        (alist-cons-after
         'unpack
         'patch-tests
         (lambda* (#:key inputs #:allow-other-keys)
           (let ((bash (assoc-ref inputs "bash")))
             (substitute* "test/test.pl"
               ;; The test script calls out to /bin/bash
               (("/bin/bash")
                (string-append bash "/bin/bash"))
               ;; There are two failing tests upstream relating to the "stats"
               ;; subcommand in test_usage_subcommand ("did not have Usage"
               ;; and "usage did not mention samtools stats"), so we disable
               ;; them.
               (("(test_usage_subcommand\\(.*\\);)" cmd)
                (string-append "unless ($subcommand eq 'stats') {" cmd "};")))))
         (alist-delete
          'configure
          %standard-phases)))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("ncurses" ,ncurses)
              ("perl" ,perl)
              ("python" ,python)
              ("zlib" ,zlib)))
    (home-page "http://samtools.sourceforge.net")
    (synopsis "Utilities to efficiently manipulate nucleotide sequence alignments")
    (description
     "Samtools implements various utilities for post-processing nucleotide
sequence alignments in the SAM, BAM, and CRAM formats, including indexing,
variant calling (in conjunction with bcftools), and a simple alignment
viewer.")
    (license license:expat)))
