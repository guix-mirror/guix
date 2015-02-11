;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages zip))

(define-public bedops
  (package
    (name "bedops")
    (version "2.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bedops/bedops/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0wmg6j0icimlrnsidaxrzf3hfgjvlkkcwvpdg7n4gg7hdv2m9ni5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:make-flags (list (string-append "BINDIR=" %output "/bin"))
       #:phases
       (alist-cons-after
         'unpack 'unpack-tarballs
         (lambda _
           ;; FIXME: Bedops includes tarballs of minimally patched upstream
           ;; libraries jansson, zlib, and bzip2.  We cannot just use stock
           ;; libraries because at least one of the libraries (zlib) is
           ;; patched to add a C++ function definition (deflateInit2cpp).
           ;; Until the Bedops developers offer a way to link against system
           ;; libraries we have to build the in-tree copies of these three
           ;; libraries.

           ;; See upstream discussion:
           ;; https://github.com/bedops/bedops/issues/124

           ;; Unpack the tarballs to benefit from shebang patching.
           (with-directory-excursion "third-party"
             (and (zero? (system* "tar" "xvf" "jansson-2.6.tar.bz2"))
                  (zero? (system* "tar" "xvf" "zlib-1.2.7.tar.bz2"))
                  (zero? (system* "tar" "xvf" "bzip2-1.0.6.tar.bz2"))))
           ;; Disable unpacking of tarballs in Makefile.
           (substitute* "system.mk/Makefile.linux"
             (("^\tbzcat .*") "\t@echo \"not unpacking\"\n")
             (("\\./configure") "CONFIG_SHELL=bash ./configure"))
           (substitute* "third-party/zlib-1.2.7/Makefile.in"
             (("^SHELL=.*$") "SHELL=bash\n")))
         (alist-delete 'configure %standard-phases))))
    (home-page "https://github.com/bedops/bedops")
    (synopsis "Tools for high-performance genomic feature operations")
    (description
     "BEDOPS is a suite of tools to address common questions raised in genomic
studies---mostly with regard to overlap and proximity relationships between
data sets.  It aims to be scalable and flexible, facilitating the efficient
and accurate analysis and management of large-scale genomic data.

BEDOPS provides tools that perform highly efficient and scalable Boolean and
other set operations, statistical calculations, archiving, conversion and
other management of genomic data of arbitrary scale.  Tasks can be easily
split by chromosome for distributing whole-genome analyses across a
computational cluster.")
    (license license:gpl2+)))

(define-public bedtools
  (package
    (name "bedtools")
    (version "2.22.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/arq5x/bedtools2/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "16aq0w3dmbd0853j32xk9jin4vb6v6fgakfyvrsmsjizzbn3fpfl"))))
    (build-system gnu-build-system)
    (native-inputs `(("python" ,python-2)))
    (inputs `(("samtools" ,samtools)
              ("zlib" ,zlib)))
    (arguments
     '(#:test-target "test"
       #:phases
       (alist-cons-after
        'unpack 'patch-makefile-SHELL-definition
        (lambda _
          ;; patch-makefile-SHELL cannot be used here as it does not
          ;; yet patch definitions with `:='.  Since changes to
          ;; patch-makefile-SHELL result in a full rebuild, features
          ;; of patch-makefile-SHELL are reimplemented here.
          (substitute* "Makefile"
            (("^SHELL := .*$") (string-append "SHELL := " (which "bash") " -e \n"))))
        (alist-delete
         'configure
         (alist-replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
              (mkdir-p bin)
              (for-each (lambda (file)
                          (copy-file file (string-append bin (basename file))))
                        (find-files "bin" ".*"))))
          %standard-phases)))))
    (home-page "https://github.com/arq5x/bedtools2")
    (synopsis "Tools for genome analysis and arithmetic")
    (description
     "Collectively, the bedtools utilities are a swiss-army knife of tools for
a wide-range of genomics analysis tasks.  The most widely-used tools enable
genome arithmetic: that is, set theory on the genome.  For example, bedtools
allows one to intersect, merge, count, complement, and shuffle genomic
intervals from multiple files in widely-used genomic file formats such as BAM,
BED, GFF/GTF, VCF.")
    (license license:gpl2)))

(define-public bowtie
  (package
    (name "bowtie")
    (version "2.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BenLangmead/bowtie2/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15dnbqippwvhyh9zqjhaxkabk7lm1xbh1nvar1x4b5kwm117zijn"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Makefile"
                  (("^CC = .*$") "CC = gcc")
                  (("^CPP = .*$") "CPP = g++")
                  ;; replace BUILD_HOST and BUILD_TIME for deterministic build
                  (("-DBUILD_HOST=.*") "-DBUILD_HOST=\"\\\"guix\\\"\"")
                  (("-DBUILD_TIME=.*") "-DBUILD_TIME=\"\\\"0\\\"\"")))
              (patches (list (search-patch "bowtie-fix-makefile.patch")))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)
              ("perl-clone" ,perl-clone)
              ("perl-test-deep" ,perl-test-deep)
              ("perl-test-simple" ,perl-test-simple)
              ("python" ,python-2)))
    (arguments
     '(#:make-flags '("allall")
       #:phases
       (alist-delete
        'configure
        (alist-replace
         'install
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
             (mkdir-p bin)
             (for-each (lambda (file)
                         (copy-file file (string-append bin file)))
                       (find-files "." "bowtie2.*"))))
         (alist-replace
          'check
          (lambda* (#:key outputs #:allow-other-keys)
            (system* "perl"
                     "scripts/test/simple_tests.pl"
                     "--bowtie2=./bowtie2"
                     "--bowtie2-build=./bowtie2-build"))
          %standard-phases)))))
    (home-page "http://bowtie-bio.sourceforge.net/bowtie2/index.shtml")
    (synopsis "Fast and sensitive nucleotide sequence read aligner")
    (description
     "Bowtie 2 is a fast and memory-efficient tool for aligning sequencing
reads to long reference sequences.  It is particularly good at aligning reads
of about 50 up to 100s or 1,000s of characters, and particularly good at
aligning to relatively long (e.g. mammalian) genomes.  Bowtie 2 indexes the
genome with an FM Index to keep its memory footprint small: for the human
genome, its memory footprint is typically around 3.2 GB.  Bowtie 2 supports
gapped, local, and paired-end alignment modes.")
    (supported-systems '("x86_64-linux"))
    (license license:gpl3+)))

(define-public flexbar
  (package
    (name "flexbar")
    (version "2.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://sourceforge/flexbar/"
                              version "/flexbar_v" version "_src.tgz"))
              (sha256
               (base32
                "13jaykc3y1x8y5nn9j8ljnb79s5y51kyxz46hdmvvjj6qhyympmf"))))
    (build-system cmake-build-system)
    (arguments
     `(;; There is no test target, although there is a directory containing
       ;; test data and scripts (launched by flexbar_validate.sh).
       #:tests? #f
       #:configure-flags (list
                          (string-append "-DFLEXBAR_BINARY_DIR="
                                         (assoc-ref %outputs "out")
                                         "/bin/"))
       #:phases
       (alist-delete 'install %standard-phases)))
    (inputs
     `(("tbb" ,tbb)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("seqan" ,seqan)))
    (home-page "http://flexbar.sourceforge.net")
    (synopsis "Barcode and adapter removal tool for sequencing platforms")
    (description
     "Flexbar preprocesses high-throughput nucleotide sequencing data
efficiently.  It demultiplexes barcoded runs and removes adapter sequences.
Moreover, trimming and filtering features are provided.  Flexbar increases
read mapping rates and improves genome and transcriptome assemblies.  It
supports next-generation sequencing data in fasta/q and csfasta/q format from
Illumina, Roche 454, and the SOLiD platform.")
    (license license:gpl3)))

(define-public hisat
  (package
    (name "hisat")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ccb.jhu.edu/software/hisat/downloads/hisat-"
                    version "-beta-source.zip"))
              (sha256
               (base32
                "1k381ydranqxp09yf2y7w1d0chz5d59vb6jchi89hbb0prq19lk5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no check target
       #:make-flags '("allall"
                      ;; Disable unsupported `popcnt' instructions on
                      ;; architectures other than x86_64
                      ,@(if (string-prefix? "x86_64"
                                            (or (%current-target-system)
                                                (%current-system)))
                            '()
                            '("POPCNT_CAPABILITY=0")))
       #:phases
       (alist-replace
        'unpack
        (lambda* (#:key source #:allow-other-keys)
          (and (zero? (system* "unzip" source))
               (chdir "hisat-0.1.4-beta")))
        (alist-cons-after
         'unpack 'patch-sources
         (lambda _
           ;; XXX Cannot use snippet because zip files are not supported
           (substitute* "Makefile"
             (("^CC = .*$") "CC = gcc")
             (("^CPP = .*$") "CPP = g++")
             ;; replace BUILD_HOST and BUILD_TIME for deterministic build
             (("-DBUILD_HOST=.*") "-DBUILD_HOST=\"\\\"guix\\\"\"")
             (("-DBUILD_TIME=.*") "-DBUILD_TIME=\"\\\"0\\\"\""))
           (substitute* '("hisat-build" "hisat-inspect")
             (("/usr/bin/env") (which "env"))))
         (alist-replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
              (mkdir-p bin)
              (for-each
               (lambda (file)
                 (copy-file file (string-append bin file)))
               (find-files
                "."
                "hisat(-(build|align|inspect)(-(s|l)(-debug)*)*)*$"))))
          (alist-delete 'configure %standard-phases))))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("perl" ,perl)
       ("python" ,python)
       ("zlib" ,zlib)))
    (home-page "http://ccb.jhu.edu/software/hisat/index.shtml")
    (synopsis "Hierarchical indexing for spliced alignment of transcripts")
    (description
     "HISAT is a fast and sensitive spliced alignment program for mapping
RNA-seq reads.  In addition to one global FM index that represents a whole
genome, HISAT uses a large set of small FM indexes that collectively cover the
whole genome.  These small indexes (called local indexes) combined with
several alignment strategies enable effective alignment of RNA-seq reads, in
particular, reads spanning multiple exons.")
    (license license:gpl3+)))

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
     `(;; There are 87 test failures when building on non-64-bit architectures
       ;; due to invalid test data.  This has since been fixed upstream (see
       ;; <https://github.com/samtools/samtools/pull/307>), but as there has
       ;; not been a new release we disable the tests for all non-64-bit
       ;; systems.
       #:tests? ,(string=? (or (%current-system) (%current-target-system))
                           "x86_64-linux")
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
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

(define-public seqan
  (package
    (name "seqan")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://packages.seqan.de/seqan-library/"
                                  "seqan-library-" version ".tar.bz2"))
              (sha256
               (base32
                "05s3wrrwn50f81aklfm65i4a749zag1vr8z03k21xm0pdxy47yvp"))))
    ;; The documentation is 7.8MB and the includes are 3.6MB heavy, so it
    ;; makes sense to split the outputs.
    (outputs '("out" "doc"))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((tar  (assoc-ref %build-inputs "tar"))
               (bzip (assoc-ref %build-inputs "bzip2"))
               (out  (assoc-ref %outputs "out"))
               (doc  (assoc-ref %outputs "doc")))
           (setenv "PATH" (string-append tar "/bin:" bzip "/bin"))
           (system* "tar" "xvf" (assoc-ref %build-inputs "source"))
           (chdir (string-append "seqan-library-" ,version))
           (copy-recursively "include" (string-append out "/include"))
           (copy-recursively "share"  (string-append doc "/share"))))))
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)
       ("bzip2" ,bzip2)))
    (home-page "http://www.seqan.de")
    (synopsis "Library for nucleotide sequence analysis")
    (description
     "SeqAn is a C++ library of efficient algorithms and data structures for
the analysis of sequences with the focus on biological data.  It contains
algorithms and data structures for string representation and their
manipulation, online and indexed string search, efficient I/O of
bioinformatics file formats, sequence alignment, and more.")
    (license license:bsd-3)))

(define-public star
  (package
    (name "star")
    (version "2.4.0j")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/alexdobin/STAR/archive/STAR_"
                    version ".tar.gz"))
              (sha256
               (base32
                "1y3bciych1aw6s7k8sy1saj23dcan9wk4d4f96an499slkxwz712"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "source/Makefile"
                  (("/bin/rm") "rm")))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no check target
       #:make-flags '("STAR")
       #:phases
       (alist-cons-after
        'unpack 'enter-source-dir (lambda _ (chdir "source"))
        (alist-replace
         'install
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
             (mkdir-p bin)
             (copy-file "STAR" (string-append bin "STAR"))))
         (alist-delete
          'configure %standard-phases)))))
    (native-inputs
     `(("vim" ,vim))) ; for xxd
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/alexdobin/STAR")
    (synopsis "Universal RNA-seq aligner")
    (description
     "The Spliced Transcripts Alignment to a Reference (STAR) software is
based on a previously undescribed RNA-seq alignment algorithm that uses
sequential maximum mappable seed search in uncompressed suffix arrays followed
by seed clustering and stitching procedure.  In addition to unbiased de novo
detection of canonical junctions, STAR can discover non-canonical splices and
chimeric (fusion) transcripts, and is also capable of mapping full-length RNA
sequences.")
    ;; STAR is licensed under GPLv3 or later; htslib is MIT-licensed.
    (license license:gpl3+)))
