;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015, 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2018 Raoul Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system ocaml)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages ldc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public aragorn
  (package
    (name "aragorn")
    (version "1.2.38")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://mbio-serv2.mbioekol.lu.se/ARAGORN/Downloads/aragorn"
                    version ".tgz"))
              (sha256
               (base32
                "09i1rg716smlbnixfm7q1ml2mfpaa2fpn3hwjg625ysmfwwy712b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
                  (lambda _
                    (zero? (system* "gcc"
                                    "-O3"
                                    "-ffast-math"
                                    "-finline-functions"
                                    "-o"
                                    "aragorn"
                                    (string-append "aragorn" ,version ".c")))))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin"))
                           (man (string-append out "/share/man/man1")))
                      (mkdir-p bin)
                      (install-file "aragorn" bin)
                      (mkdir-p man)
                      (install-file "aragorn.1" man))
                    #t)))))
    (home-page "http://mbio-serv2.mbioekol.lu.se/ARAGORN")
    (synopsis "Detect tRNA, mtRNA and tmRNA genes in nucleotide sequences")
    (description
     "Aragorn identifies transfer RNA, mitochondrial RNA and
transfer-messenger RNA from nucleotide sequences, based on homology to known
tRNA consensus sequences and RNA structure.  It also outputs the secondary
structure of the predicted RNA.")
    (license license:gpl2)))

(define-public bamm
  (package
    (name "bamm")
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              ;; BamM is not available on pypi.
              (uri (string-append
                    "https://github.com/Ecogenomics/BamM/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1f35yxp4pc8aadsvbpg6r4kg2jh4fkjci0iby4iyljm6980sac0s"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; Delete bundled htslib.
                  (delete-file-recursively "c/htslib-1.3.1")
                  #t))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; BamM is Python 2 only.
       ;; Do not use bundled libhts.  Do use the bundled libcfu because it has
       ;; been modified from its original form.
       #:configure-flags
       (let ((htslib (assoc-ref %build-inputs "htslib")))
         (list "--with-libhts-lib" (string-append htslib "/lib")
               "--with-libhts-inc" (string-append htslib "/include/htslib")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _
             (with-directory-excursion "c"
               (let ((sh (which "sh")))
                 ;; Use autogen so that 'configure' works.
                 (substitute* "autogen.sh" (("/bin/sh") sh))
                 (setenv "CONFIG_SHELL" sh)
                 (substitute* "configure" (("/bin/sh") sh))
                 (zero? (system* "./autogen.sh"))))))
         (delete 'build)
         ;; Run tests after installation so compilation only happens once.
         (delete 'check)
         (add-after 'install 'wrap-executable
           (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out  (assoc-ref outputs "out"))
                   (path (getenv "PATH")))
              (wrap-program (string-append out "/bin/bamm")
                `("PATH" ":" prefix (,path))))
            #t))
         (add-after 'wrap-executable 'post-install-check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "PATH"
                     (string-append (assoc-ref outputs "out")
                                    "/bin:"
                                    (getenv "PATH")))
             (setenv "PYTHONPATH"
                     (string-append
                      (assoc-ref outputs "out")
                      "/lib/python"
                      (string-take (string-take-right
                                    (assoc-ref inputs "python") 5) 3)
                      "/site-packages:"
                      (getenv "PYTHONPATH")))
             ;; There are 2 errors printed, but they are safe to ignore:
             ;; 1) [E::hts_open_format] fail to open file ...
             ;; 2) samtools view: failed to open ...
             (zero? (system* "nosetests")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("zlib" ,zlib)
       ("python-nose" ,python2-nose)
       ("python-pysam" ,python2-pysam)))
    (inputs
     `(("htslib" ,htslib-1.3) ; At least one test fails on htslib-1.4+.
       ("samtools" ,samtools)
       ("bwa" ,bwa)
       ("grep" ,grep)
       ("sed" ,sed)
       ("coreutils" ,coreutils)))
    (propagated-inputs
     `(("python-numpy" ,python2-numpy)))
    (home-page "http://ecogenomics.github.io/BamM/")
    (synopsis "Metagenomics-focused BAM file manipulator")
    (description
     "BamM is a C library, wrapped in python, to efficiently generate and
parse BAM files, specifically for the analysis of metagenomic data.  For
instance, it implements several methods to assess contig-wise read coverage.")
    (license license:lgpl3+)))

(define-public bamtools
  (package
    (name "bamtools")
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/pezmaster31/bamtools/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jr024kcrhjb82cm69i7p5fcg5375zlc1h3qh2n1v368hcd0qflk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'set-ldflags
          (lambda* (#:key outputs #:allow-other-keys)
            (setenv "LDFLAGS"
                    (string-append
                     "-Wl,-rpath="
                     (assoc-ref outputs "out") "/lib/bamtools")))))))
    (inputs `(("zlib" ,zlib)))
    (home-page "https://github.com/pezmaster31/bamtools")
    (synopsis "C++ API and command-line toolkit for working with BAM data")
    (description
     "BamTools provides both a C++ API and a command-line toolkit for handling
BAM files.")
    (license license:expat)))

(define-public bcftools
  (package
    (name "bcftools")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/bcftools/releases/download/"
                    version "/bcftools-" version ".tar.bz2"))
              (sha256
               (base32
                "0093hkkvxmbwfaa7905s6185jymynvg42kq6sxv7fili11l5mxwz"))
              (patches (search-patches "bcftools-regidx-unsigned-char.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled htslib.
               '(delete-file-recursively "htslib-1.5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:configure-flags (list "--with-htslib=system")
       #:make-flags
       (list
        "USE_GPL=1"
        "LIBS=-lgsl -lgslcblas"
        (string-append "prefix=" (assoc-ref %outputs "out"))
        (string-append "HTSDIR=" (assoc-ref %build-inputs "htslib") "/include")
        (string-append "HTSLIB=" (assoc-ref %build-inputs "htslib") "/lib/libhts.so")
        (string-append "BGZIP=" (assoc-ref %build-inputs "htslib") "/bin/bgzip")
        (string-append "TABIX=" (assoc-ref %build-inputs "htslib") "/bin/tabix")
        (string-append "PACKAGE_VERSION=" ,version))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-tests
           (lambda _
             (substitute* "test/test.pl"
               (("/bin/bash") (which "bash")))
             #t)))))
    (native-inputs
     `(("htslib" ,htslib)
       ("perl" ,perl)))
    (inputs
     `(("gsl" ,gsl)
       ("zlib" ,zlib)))
    (home-page "https://samtools.github.io/bcftools/")
    (synopsis "Utilities for variant calling and manipulating VCFs and BCFs")
    (description
     "BCFtools is a set of utilities that manipulate variant calls in the
Variant Call Format (VCF) and its binary counterpart BCF.  All commands work
transparently with both VCFs and BCFs, both uncompressed and BGZF-compressed.")
    ;; The sources are dual MIT/GPL, but becomes GPL-only when USE_GPL=1.
    (license (list license:gpl3+ license:expat))))

(define-public bedops
  (package
    (name "bedops")
    (version "2.4.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bedops/bedops/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kqbac547wyqma81cyky9n7mkgikjpsfd3nnmcm6hpqwanqgh10v"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:make-flags (list (string-append "BINDIR=" %output "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-tarballs
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
               (("^SHELL=.*$") "SHELL=bash\n"))
             #t))
         (delete 'configure))))
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
    (version "2.27.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/arq5x/bedtools2/releases/"
                                  "download/v" version "/"
                                  "bedtools-" version ".tar.gz"))
              (sha256
               (base32
                "1ndg5yknrxl4djx8ddzgk12rrbiidfpmkkg5z3f95jzryfxarhn8"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs `(("python" ,python-2)))
    (inputs
     `(("samtools" ,samtools)
       ("zlib" ,zlib)))
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

;; Later releases of bedtools produce files with more columns than
;; what Ribotaper expects.
(define-public bedtools-2.18
  (package (inherit bedtools)
    (name "bedtools")
    (version "2.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/arq5x/bedtools2/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05vrnr8yp7swfagshzpgqmzk1blnwnq8pq5pckzi1m26w98d63vf"))))
    (arguments
     '(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "bin" ".*")))
             #t)))))))

(define-public ribotaper
  (package
    (name "ribotaper")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ohlerlab.mdc-berlin.de/"
                                  "files/RiboTaper/RiboTaper_Version_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ykjbps1y3z3085q94npw8i9x5gldc6shy8vlc08v76zljsm07hv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-executables
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (script)
                  (wrap-program (string-append out "/bin/" script)
                    `("R_LIBS_SITE" ":" = (,(getenv "R_LIBS_SITE")))))
                '("create_annotations_files.bash"
                  "create_metaplots.bash"
                  "Ribotaper_ORF_find.sh"
                  "Ribotaper.sh"))))))))
    (inputs
     `(("bedtools" ,bedtools-2.18)
       ("samtools" ,samtools-0.1)
       ("r-minimal" ,r-minimal)
       ("r-foreach" ,r-foreach)
       ("r-xnomial" ,r-xnomial)
       ("r-domc" ,r-domc)
       ("r-multitaper" ,r-multitaper)
       ("r-seqinr" ,r-seqinr)))
    (home-page "https://ohlerlab.mdc-berlin.de/software/RiboTaper_126/")
    (synopsis "Define translated ORFs using ribosome profiling data")
    (description
     "Ribotaper is a method for defining translated @dfn{open reading
frames} (ORFs) using ribosome profiling (ribo-seq) data.  This package
provides the Ribotaper pipeline.")
    (license license:gpl3+)))

(define-public ribodiff
  (package
    (name "ribodiff")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ratschlab/RiboDiff/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0wpbwmfv05wdjxv7ikm664f7s7p7cqr8jnw99zrda0q67rl50aaj"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         ;; Generate an installable executable script wrapper.
         (add-after 'unpack 'patch-setup.py
           (lambda _
             (substitute* "setup.py"
               (("^(.*)packages=.*" line prefix)
                (string-append line "\n"
                               prefix "scripts=['scripts/TE.py'],\n")))
             #t)))))
    (inputs
     `(("python-numpy" ,python2-numpy)
       ("python-matplotlib" ,python2-matplotlib)
       ("python-scipy" ,python2-scipy)
       ("python-statsmodels" ,python2-statsmodels)))
    (native-inputs
     `(("python-mock" ,python2-mock)
       ("python-nose" ,python2-nose)))
    (home-page "http://public.bmi.inf.ethz.ch/user/zhongy/RiboDiff/")
    (synopsis "Detect translation efficiency changes from ribosome footprints")
    (description "RiboDiff is a statistical tool that detects the protein
translational efficiency change from Ribo-Seq (ribosome footprinting) and
RNA-Seq data.  It uses a generalized linear model to detect genes showing
difference in translational profile taking mRNA abundance into account.  It
facilitates us to decipher the translational regulation that behave
independently with transcriptional regulation.")
    (license license:gpl3+)))

(define-public bioawk
  (package
    (name "bioawk")
    (version "1.0")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/lh3/bioawk/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "1daizxsk17ahi9n58fj8vpgwyhzrzh54bzqhanjanp88kgrz7gjw"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)))
    (arguments
     `(#:tests? #f ; There are no tests to run.
       ;; Bison must generate files, before other targets can build.
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin  (string-append out "/bin"))
                   (man (string-append out "/share/man/man1")))
              (mkdir-p man)
              (copy-file "awk.1" (string-append man "/bioawk.1"))
              (install-file "bioawk" bin)))))))
    (home-page "https://github.com/lh3/bioawk")
    (synopsis "AWK with bioinformatics extensions")
    (description "Bioawk is an extension to Brian Kernighan's awk, adding the
support of several common biological data formats, including optionally gzip'ed
BED, GFF, SAM, VCF, FASTA/Q and TAB-delimited formats with column names.  It
also adds a few built-in functions and a command line option to use TAB as the
input/output delimiter.  When the new functionality is not used, bioawk is
intended to behave exactly the same as the original BWK awk.")
    (license license:x11)))

(define-public python2-pybedtools
  (package
    (name "python2-pybedtools")
    (version "0.6.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pypi.python.org/packages/source/p/pybedtools/pybedtools-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1ldzdxw1p4y3g2ignmggsdypvqkcwqwzhdha4rbgpih048z5p4an"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2)) ; no Python 3 support
    (inputs
     `(("python-matplotlib" ,python2-matplotlib)))
    (propagated-inputs
     `(("bedtools" ,bedtools)
       ("samtools" ,samtools)))
    (native-inputs
     `(("python-cython" ,python2-cython)
       ("python-pyyaml" ,python2-pyyaml)
       ("python-nose" ,python2-nose)))
    (home-page "https://pythonhosted.org/pybedtools/")
    (synopsis "Python wrapper for BEDtools programs")
    (description
     "pybedtools is a Python wrapper for Aaron Quinlan's BEDtools programs,
which are widely used for genomic interval manipulation or \"genome algebra\".
pybedtools extends BEDTools by offering feature-level manipulations from with
Python.")
    (license license:gpl2+)))

(define-public python-biom-format
  (package
   (name "python-biom-format")
   (version "2.1.6")
   (source
    (origin
     (method url-fetch)
     ;; Use GitHub as source because PyPI distribution does not contain
     ;; test data: https://github.com/biocore/biom-format/issues/693
     (uri (string-append "https://github.com/biocore/biom-format/archive/"
                         version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "08cr7wpahk6zb31h4bs7jmzpvxcqv9s13xz40h6y2h656jvdvnpj"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-numpy" ,python-numpy)
      ("python-scipy" ,python-scipy)
      ("python-future" ,python-future)
      ("python-click" ,python-click)
      ("python-h5py" ,python-h5py)
      ("python-pandas" ,python-pandas)))
   (native-inputs
    `(("python-nose" ,python-nose)))
   (home-page "http://www.biom-format.org")
   (synopsis "Biological Observation Matrix (BIOM) format utilities")
   (description
    "The BIOM file format is designed to be a general-use format for
representing counts of observations e.g. operational taxonomic units, KEGG
orthology groups or lipid types, in one or more biological samples
e.g. microbiome samples, genomes, metagenomes.")
   (license license:bsd-3)
   (properties `((python2-variant . ,(delay python2-biom-format))))))

(define-public python2-biom-format
  (let ((base (package-with-python2 (strip-python2-variant python-biom-format))))
    (package
      (inherit base)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; Do not require the unmaintained pyqi library.
           (add-after 'unpack 'remove-pyqi
             (lambda _
               (substitute* "setup.py"
                 (("install_requires.append\\(\"pyqi\"\\)") "pass"))
               #t)))
         ,@(package-arguments base))))))

(define-public bioperl-minimal
  (let* ((inputs `(("perl-module-build" ,perl-module-build)
                   ("perl-data-stag" ,perl-data-stag)
                   ("perl-libwww" ,perl-libwww)
                   ("perl-uri" ,perl-uri)))
         (transitive-inputs
          (map (compose package-name cadr)
               (delete-duplicates
                (concatenate
                 (map (compose package-transitive-target-inputs cadr) inputs))))))
    (package
      (name "bioperl-minimal")
      (version "1.7.0")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/bioperl/bioperl-live/"
                             "archive/release-"
                             (string-map (lambda (c)
                                           (if (char=? c #\.)
                                               #\- c)) version)
                             ".tar.gz"))
         (sha256
          (base32
           "12phgpxwgkqflkwfb9dcqg7a31dpjlfhar8wcgv0aj5ln4akfz06"))))
      (build-system perl-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after
            'install 'wrap-programs
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Make sure all executables in "bin" find the required Perl
              ;; modules at runtime.  As the PERL5LIB variable contains also
              ;; the paths of native inputs, we pick the transitive target
              ;; inputs from %build-inputs.
              (let* ((out  (assoc-ref outputs "out"))
                     (bin  (string-append out "/bin/"))
                     (path (string-join
                            (cons (string-append out "/lib/perl5/site_perl")
                                  (map (lambda (name)
                                         (assoc-ref %build-inputs name))
                                       ',transitive-inputs))
                            ":")))
                (for-each (lambda (file)
                            (wrap-program file
                              `("PERL5LIB" ":" prefix (,path))))
                          (find-files bin "\\.pl$"))
                #t))))))
      (inputs inputs)
      (native-inputs
       `(("perl-test-most" ,perl-test-most)))
      (home-page "http://search.cpan.org/dist/BioPerl")
      (synopsis "Bioinformatics toolkit")
      (description
       "BioPerl is the product of a community effort to produce Perl code which
is useful in biology.  Examples include Sequence objects, Alignment objects
and database searching objects.  These objects not only do what they are
advertised to do in the documentation, but they also interact - Alignment
objects are made from the Sequence objects, Sequence objects have access to
Annotation and SeqFeature objects and databases, Blast objects can be
converted to Alignment objects, and so on.  This means that the objects
provide a coordinated and extensible framework to do computational biology.")
      (license license:perl-license))))

(define-public python-biopython
  (package
    (name "python-biopython")
    (version "1.70")
    (source (origin
              (method url-fetch)
              ;; use PyPi rather than biopython.org to ease updating
              (uri (pypi-uri "biopython" version))
              (sha256
               (base32
                "0nz4n9d2y2dg849gn1z0vjlkwcpzzkzy3fij7x94a6ixy2c54z2a"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-home
           ;; Some tests require a home directory to be set.
           (lambda _ (setenv "HOME" "/tmp") #t)))))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
    (home-page "http://biopython.org/")
    (synopsis "Tools for biological computation in Python")
    (description
     "Biopython is a set of tools for biological computation including parsers
for bioinformatics files into Python data structures; interfaces to common
bioinformatics programs; a standard sequence class and tools for performing
common operations on them; code to perform data classification; code for
dealing with alignments; code making it easy to split up parallelizable tasks
into separate processes; and more.")
    (license (license:non-copyleft "http://www.biopython.org/DIST/LICENSE"))))

(define-public python2-biopython
  (package-with-python2 python-biopython))

(define-public bpp-core
  ;; The last release was in 2014 and the recommended way to install from source
  ;; is to clone the git repository, so we do this.
  ;; http://biopp.univ-montp2.fr/wiki/index.php/Main_Page
  (let ((commit "7d8bced0d1a87291ea8dd7046b7fb5ff9c35c582"))
    (package
      (name "bpp-core")
      (version (string-append "2.2.0-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://biopp.univ-montp2.fr/git/bpp-core")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "10djsq5vlnkilv436gnmh4irpk49v29pa69r6xiryg32xmvn909j"))))
      (build-system cmake-build-system)
      (arguments
       `(#:parallel-build? #f))
      (inputs
       `(("gcc" ,gcc-5))) ; Compilation of bpp-phyl fails with GCC 4.9 so we
                          ; compile all of the bpp packages with GCC 5.
      (home-page "http://biopp.univ-montp2.fr")
      (synopsis "C++ libraries for Bioinformatics")
      (description
       "Bio++ is a set of C++ libraries for Bioinformatics, including sequence
analysis, phylogenetics, molecular evolution and population genetics.  It is
Object Oriented and is designed to be both easy to use and computer efficient.
Bio++ intends to help programmers to write computer expensive programs, by
providing them a set of re-usable tools.")
      (license license:cecill-c))))

(define-public bpp-phyl
  ;; The last release was in 2014 and the recommended way to install from source
  ;; is to clone the git repository, so we do this.
  ;; http://biopp.univ-montp2.fr/wiki/index.php/Main_Page
  (let ((commit "0c07167b629f68b569bf274d1ad0c4af83276ae2"))
    (package
      (name "bpp-phyl")
      (version (string-append "2.2.0-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://biopp.univ-montp2.fr/git/bpp-phyl")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1ssjgchzwj3iai26kyly7gwkdv8sk59nqhkb1wpap3sf5m6kyllh"))))
      (build-system cmake-build-system)
      (arguments
       `(#:parallel-build? #f
         ;; If out-of-source, test data is not copied into the build directory
         ;; so the tests fail.
         #:out-of-source? #f))
      (inputs
       `(("bpp-core" ,bpp-core)
         ("bpp-seq" ,bpp-seq)
         ;; GCC 4.8 fails due to an 'internal compiler error', so we use a more
         ;; modern GCC.
         ("gcc" ,gcc-5)))
      (home-page "http://biopp.univ-montp2.fr")
      (synopsis "Bio++ phylogenetic Library")
      (description
       "Bio++ is a set of C++ libraries for Bioinformatics, including sequence
analysis, phylogenetics, molecular evolution and population genetics.  This
library provides phylogenetics-related modules.")
      (license license:cecill-c))))

(define-public bpp-popgen
  ;; The last release was in 2014 and the recommended way to install from source
  ;; is to clone the git repository, so we do this.
  ;; http://biopp.univ-montp2.fr/wiki/index.php/Main_Page
  (let ((commit "e472bac9b1a148803895d747cd6d0c5904f85d9f"))
    (package
      (name "bpp-popgen")
      (version (string-append "2.2.0-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://biopp.univ-montp2.fr/git/bpp-popgen")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0yn82dzn1n5629nzja68xfrhi655709rjanyryb36vzkmymy6dw5"))))
      (build-system cmake-build-system)
      (arguments
       `(#:parallel-build? #f
         #:tests? #f)) ; There are no tests.
      (inputs
       `(("bpp-core" ,bpp-core)
         ("bpp-seq" ,bpp-seq)
         ("gcc" ,gcc-5)))
      (home-page "http://biopp.univ-montp2.fr")
      (synopsis "Bio++ population genetics library")
      (description
       "Bio++ is a set of C++ libraries for Bioinformatics, including sequence
analysis, phylogenetics, molecular evolution and population genetics.  This
library provides population genetics-related modules.")
      (license license:cecill-c))))

(define-public bpp-seq
  ;; The last release was in 2014 and the recommended way to install from source
  ;; is to clone the git repository, so we do this.
  ;; http://biopp.univ-montp2.fr/wiki/index.php/Main_Page
  (let ((commit "6cfa07965ce152e5598a89df2fa80a75973bfa33"))
    (package
      (name "bpp-seq")
      (version (string-append "2.2.0-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://biopp.univ-montp2.fr/git/bpp-seq")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1nys5jq7jqvdg40d91wsmj3q2yzy4276cp7sp44n67p468f27zf2"))))
      (build-system cmake-build-system)
      (arguments
       `(#:parallel-build? #f
         ;; If out-of-source, test data is not copied into the build directory
         ;; so the tests fail.
         #:out-of-source? #f))
      (inputs
       `(("bpp-core" ,bpp-core)
         ("gcc" ,gcc-5))) ; Use GCC 5 as per 'bpp-core'.
      (home-page "http://biopp.univ-montp2.fr")
      (synopsis "Bio++ sequence library")
      (description
       "Bio++ is a set of C++ libraries for Bioinformatics, including sequence
analysis, phylogenetics, molecular evolution and population genetics.  This
library provides sequence-related modules.")
      (license license:cecill-c))))

(define-public bppsuite
  ;; The last release was in 2014 and the recommended way to install from source
  ;; is to clone the git repository, so we do this.
  ;; http://biopp.univ-montp2.fr/wiki/index.php/Main_Page
  (let ((commit "c516147f57aa50961121cd505bed52cd7603698b"))
    (package
      (name "bppsuite")
      (version (string-append "2.2.0-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://biopp.univ-montp2.fr/git/bppsuite")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1y87pxvw0jxjizhq2dr9g2r91md45k1p9ih2sl1yy1y3p934l2kb"))))
      (build-system cmake-build-system)
      (arguments
       `(#:parallel-build? #f
         #:tests? #f)) ; There are no tests.
      (native-inputs
       `(("groff" ,groff)
         ("man-db" ,man-db)
         ("texinfo" ,texinfo)))
      (inputs
       `(("bpp-core" ,bpp-core)
         ("bpp-seq" ,bpp-seq)
         ("bpp-phyl" ,bpp-phyl)
         ("bpp-phyl" ,bpp-popgen)
         ("gcc" ,gcc-5)))
      (home-page "http://biopp.univ-montp2.fr")
      (synopsis "Bioinformatics tools written with the Bio++ libraries")
      (description
       "Bio++ is a set of C++ libraries for Bioinformatics, including sequence
analysis, phylogenetics, molecular evolution and population genetics.  This
package provides command line tools using the Bio++ library.")
      (license license:cecill-c))))

(define-public blast+
  (package
    (name "blast+")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/"
                    version "/ncbi-blast-" version "+-src.tar.gz"))
              (sha256
               (base32
                "15n937pw5aqmyfjb6l387d18grqbb96l63d5xj4l7yyh0zbf2405"))
              (patches (search-patches "blast+-fix-makefile.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled bzip2, zlib and pcre.
                  (delete-file-recursively "c++/src/util/compress/bzip2")
                  (delete-file-recursively "c++/src/util/compress/zlib")
                  (delete-file-recursively "c++/src/util/regexp")
                  (substitute* "c++/src/util/compress/Makefile.in"
                    (("bzip2 zlib api") "api"))
                  ;; Remove useless msbuild directory
                  (delete-file-recursively
                   "c++/src/build-system/project_tree_builder/msbuild")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(;; There are two(!) tests for this massive library, and both fail with
       ;; "unparsable timing stats".
       ;; ERR [127] --  [serial/datatool] datatool.sh     (unparsable timing stats)
       ;; ERR [127] --  [serial/datatool] datatool_xml.sh     (unparsable timing stats)
       #:tests? #f
       #:out-of-source? #t
       #:parallel-build? #f ; not supported
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'set-HOME
          ;; $HOME needs to be set at some point during the configure phase
          (lambda _ (setenv "HOME" "/tmp") #t))
         (add-after
          'unpack 'enter-dir
          (lambda _ (chdir "c++") #t))
         (add-after
          'enter-dir 'fix-build-system
          (lambda _
            (define (which* cmd)
              (cond ((string=? cmd "date")
                     ;; make call to "date" deterministic
                     "date -d @0")
                    ((which cmd)
                     => identity)
                    (else
                     (format (current-error-port)
                             "WARNING: Unable to find absolute path for ~s~%"
                             cmd)
                     #f)))

            ;; Rewrite hardcoded paths to various tools
            (substitute* (append '("src/build-system/configure.ac"
                                   "src/build-system/configure"
                                   "src/build-system/helpers/run_with_lock.c"
                                   "scripts/common/impl/if_diff.sh"
                                   "scripts/common/impl/run_with_lock.sh"
                                   "src/build-system/Makefile.configurables.real"
                                   "src/build-system/Makefile.in.top"
                                   "src/build-system/Makefile.meta.gmake=no"
                                   "src/build-system/Makefile.meta.in"
                                   "src/build-system/Makefile.meta_l"
                                   "src/build-system/Makefile.meta_p"
                                   "src/build-system/Makefile.meta_r"
                                   "src/build-system/Makefile.mk.in"
                                   "src/build-system/Makefile.requirements"
                                   "src/build-system/Makefile.rules_with_autodep.in")
                                 (find-files "scripts/common/check" "\\.sh$"))
              (("(/usr/bin/|/bin/)([a-z][-_.a-z]*)" all dir cmd)
               (or (which* cmd) all)))

            (substitute* (find-files "src/build-system" "^config.*")
              (("LN_S=/bin/\\$LN_S") (string-append "LN_S=" (which "ln")))
              (("^PATH=.*") ""))

            ;; rewrite "/var/tmp" in check script
            (substitute* "scripts/common/check/check_make_unix.sh"
              (("/var/tmp") "/tmp"))

            ;; do not reset PATH
            (substitute* (find-files "scripts/common/impl/" "\\.sh$")
              (("^ *PATH=.*") "")
              (("action=/bin/") "action=")
              (("export PATH") ":"))
            #t))
         (replace
          'configure
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out     (assoc-ref outputs "out"))
                  (lib     (string-append (assoc-ref outputs "lib") "/lib"))
                  (include (string-append (assoc-ref outputs "include")
                                          "/include/ncbi-tools++")))
              ;; The 'configure' script doesn't recognize things like
              ;; '--enable-fast-install'.
              (zero? (system* "./configure.orig"
                              (string-append "--with-build-root=" (getcwd) "/build")
                              (string-append "--prefix=" out)
                              (string-append "--libdir=" lib)
                              (string-append "--includedir=" include)
                              (string-append "--with-bz2="
                                             (assoc-ref inputs "bzip2"))
                              (string-append "--with-z="
                                             (assoc-ref inputs "zlib"))
                              (string-append "--with-pcre="
                                             (assoc-ref inputs "pcre"))
                              ;; Each library is built twice by default, once
                              ;; with "-static" in its name, and again
                              ;; without.
                              "--without-static"
                              "--with-dll"))))))))
    (outputs '("out"       ;  21 MB
               "lib"       ; 226 MB
               "include")) ;  33 MB
    (inputs
     `(("bzip2" ,bzip2)
       ("zlib" ,zlib)
       ("pcre" ,pcre)
       ("perl" ,perl)
       ("python" ,python-wrapper)))
    (native-inputs
     `(("cpio" ,cpio)))
    (home-page "http://blast.ncbi.nlm.nih.gov")
    (synopsis "Basic local alignment search tool")
    (description
     "BLAST is a popular method of performing a DNA or protein sequence
similarity search, using heuristics to produce results quickly.  It also
calculates an “expect value” that estimates how many matches would have
occurred at a given score by chance, which can aid a user in judging how much
confidence to have in an alignment.")
    ;; Most of the sources are in the public domain, with the following
    ;; exceptions:
    ;;   * Expat:
    ;;     * ./c++/include/util/bitset/
    ;;     * ./c++/src/html/ncbi_menu*.js
    ;;   * Boost license:
    ;;     * ./c++/include/util/impl/floating_point_comparison.hpp
    ;;   * LGPL 2+:
    ;;     * ./c++/include/dbapi/driver/odbc/unix_odbc/
    ;;   * ASL 2.0:
    ;;     * ./c++/src/corelib/teamcity_*
    (license (list license:public-domain
                   license:expat
                   license:boost1.0
                   license:lgpl2.0+
                   license:asl2.0))))

(define-public bless
  (package
    (name "bless")
    (version "1p02")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bless-ec/bless.v"
                                  version ".tgz"))
              (sha256
               (base32
                "0rm0gw2s18dqwzzpl3c2x1z05ni2v0xz5dmfk3d33j6g4cgrlrdd"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; Remove bundled boost, pigz, zlib, and .git directory
                  ;; FIXME: also remove bundled sources for murmurhash3 and
                  ;; kmc once packaged.
                  (delete-file-recursively "boost")
                  (delete-file-recursively "pigz")
                  (delete-file-recursively "google-sparsehash")
                  (delete-file-recursively "zlib")
                  (delete-file-recursively ".git")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:make-flags
       (list (string-append "ZLIB="
                            (assoc-ref %build-inputs "zlib")
                            "/lib/libz.a")
             (string-append "LDFLAGS="
                            (string-join '("-lboost_filesystem"
                                           "-lboost_system"
                                           "-lboost_iostreams"
                                           "-lz"
                                           "-fopenmp"
                                           "-std=c++11"))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-build-bundled-pigz
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "Makefile"
              (("cd pigz/pigz-2.3.3; make") ""))
            #t))
         (add-after 'unpack 'patch-paths-to-executables
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "parse_args.cpp"
              (("kmc_binary = .*")
               (string-append "kmc_binary = \""
                              (assoc-ref outputs "out")
                              "/bin/kmc\";"))
              (("pigz_binary = .*")
               (string-append "pigz_binary = \""
                              (assoc-ref inputs "pigz")
                              "/bin/pigz\";")))
            #t))
         (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
              (for-each (lambda (file)
                          (install-file file bin))
                        '("bless" "kmc/bin/kmc"))
              #t)))
         (delete 'configure))))
    (native-inputs
     `(("perl" ,perl)))
    (inputs
     `(("openmpi" ,openmpi)
       ("boost" ,boost)
       ("sparsehash" ,sparsehash)
       ("pigz" ,pigz)
       ("zlib" ,zlib)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://sourceforge.net/p/bless-ec/wiki/Home/")
    (synopsis "Bloom-filter-based error correction tool for NGS reads")
    (description
     "@dfn{Bloom-filter-based error correction solution for high-throughput
sequencing reads} (BLESS) uses a single minimum-sized bloom filter is a
correction tool for genomic reads produced by @dfn{Next-generation
sequencing} (NGS).  BLESS produces accurate correction results with much less
memory compared with previous solutions and is also able to tolerate a higher
false-positive rate.  BLESS can extend reads like DNA assemblers to correct
errors at the end of reads.")
    (license license:gpl3+)))

(define-public bowtie
  (package
    (name "bowtie")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BenLangmead/bowtie2/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hwa5r9qbglppb7sz5z79rlmmddr3n51n468jb3wh8rwjgn3yr90"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Makefile"
                  ;; replace BUILD_HOST and BUILD_TIME for deterministic build
                  (("-DBUILD_HOST=.*") "-DBUILD_HOST=\"\\\"guix\\\"\"")
                  (("-DBUILD_TIME=.*") "-DBUILD_TIME=\"\\\"0\\\"\"")))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl)
       ("perl-clone" ,perl-clone)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-simple" ,perl-test-simple)
       ("python" ,python-2)
       ("tbb" ,tbb)
       ("zlib" ,zlib)))
    (arguments
     '(#:make-flags
       (list "allall"
             "WITH_TBB=1"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "perl"
                             "scripts/test/simple_tests.pl"
                             "--bowtie2=./bowtie2"
                             "--bowtie2-build=./bowtie2-build")))))))
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

(define-public tophat
  (package
    (name "tophat")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ccb.jhu.edu/software/tophat/downloads/tophat-"
                    version ".tar.gz"))
              (sha256
               (base32
                "168zlzykq622zbgkh90a90f1bdgsxkscq2zxzbj8brq80hbjpyp7"))
              (patches (search-patches "tophat-build-with-later-seqan.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled SeqAn and samtools
                  (delete-file-recursively "src/SeqAn-1.3")
                  (delete-file-recursively "src/samtools-0.1.18")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f ; not supported
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-samtools
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/Makefile.in"
               (("(noinst_LIBRARIES = )\\$\\(SAMLIB\\)" _ prefix) prefix)
               (("\\$\\(SAMPROG\\): \\$\\(SAMLIB\\)") "")
               (("SAMPROG = samtools_0\\.1\\.18") "")
               (("\\$\\(samtools_0_1_18_SOURCES\\)") "")
               (("am__EXEEXT_1 = samtools_0\\.1\\.18\\$\\(EXEEXT\\)") ""))
             (substitute* '("src/common.cpp"
                            "src/tophat.py")
               (("samtools_0.1.18") (which "samtools")))
             (substitute* '("src/common.h"
                            "src/bam2fastx.cpp")
               (("#include \"bam.h\"") "#include <samtools/bam.h>")
               (("#include \"sam.h\"") "#include <samtools/sam.h>"))
             (substitute* '("src/bwt_map.h"
                            "src/map2gtf.h"
                            "src/align_status.h")
               (("#include <bam.h>") "#include <samtools/bam.h>")
               (("#include <sam.h>") "#include <samtools/sam.h>"))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("bowtie" ,bowtie)
       ("samtools" ,samtools-0.1)
       ("ncurses" ,ncurses)
       ("python" ,python-2)
       ("perl" ,perl)
       ("zlib" ,zlib)
       ("seqan" ,seqan)))
    (home-page "http://ccb.jhu.edu/software/tophat/index.shtml")
    (synopsis "Spliced read mapper for RNA-Seq data")
    (description
     "TopHat is a fast splice junction mapper for nucleotide sequence
reads produced by the RNA-Seq method.  It aligns RNA-Seq reads to
mammalian-sized genomes using the ultra high-throughput short read
aligner Bowtie, and then analyzes the mapping results to identify
splice junctions between exons.")
    ;; TopHat is released under the Boost Software License, Version 1.0
    ;; See https://github.com/infphilo/tophat/issues/11#issuecomment-121589893
    (license license:boost1.0)))

(define-public bwa
  (package
    (name "bwa")
    (version "0.7.17")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/lh3/bwa/releases/download/v"
                    version "/bwa-" version ".tar.bz2"))
              (sha256
               (base32
                "1zfhv2zg9v1icdlq4p9ssc8k01mca5d1bd87w71py2swfi74s6yy"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append
                         (assoc-ref outputs "out") "/bin"))
                   (doc (string-append
                         (assoc-ref outputs "out") "/share/doc/bwa"))
                   (man (string-append
                         (assoc-ref outputs "out") "/share/man/man1")))
               (install-file "bwa" bin)
               (install-file "README.md" doc)
               (install-file "bwa.1" man))
             #t))
           ;; no "configure" script
          (delete 'configure))))
    (inputs `(("zlib" ,zlib)))
    ;; Non-portable SSE instructions are used so building fails on platforms
    ;; other than x86_64.
    (supported-systems '("x86_64-linux"))
    (home-page "http://bio-bwa.sourceforge.net/")
    (synopsis "Burrows-Wheeler sequence aligner")
    (description
     "BWA is a software package for mapping low-divergent sequences against a
large reference genome, such as the human genome.  It consists of three
algorithms: BWA-backtrack, BWA-SW and BWA-MEM.  The first algorithm is
designed for Illumina sequence reads up to 100bp, while the rest two for
longer sequences ranged from 70bp to 1Mbp.  BWA-MEM and BWA-SW share similar
features such as long-read support and split alignment, but BWA-MEM, which is
the latest, is generally recommended for high-quality queries as it is faster
and more accurate.  BWA-MEM also has better performance than BWA-backtrack for
70-100bp Illumina reads.")
    (license license:gpl3+)))

(define-public bwa-pssm
  (package (inherit bwa)
    (name "bwa-pssm")
    (version "0.5.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/pkerpedjiev/bwa-pssm/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02p7mpbs4mlxmn84g2x4ghak638vbj4lqix2ipx5g84pz9bhdavg"))))
    (build-system gnu-build-system)
    (inputs
     `(("gdsl" ,gdsl)
       ("zlib" ,zlib)
       ("perl" ,perl)))
    (home-page "http://bwa-pssm.binf.ku.dk/")
    (synopsis "Burrows-Wheeler transform-based probabilistic short read mapper")
    (description
     "BWA-PSSM is a probabilistic short genomic sequence read aligner based on
the use of @dfn{position specific scoring matrices} (PSSM).  Like many of the
existing aligners it is fast and sensitive.  Unlike most other aligners,
however, it is also adaptible in the sense that one can direct the alignment
based on known biases within the data set.  It is coded as a modification of
the original BWA alignment program and shares the genome index structure as
well as many of the command line options.")
    (license license:gpl3+)))

(define-public python2-bx-python
  (package
    (name "python2-bx-python")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "bx-python" version))
              (sha256
               (base32
                "15z2w3bvnc0n4qmb9bd6d8ylc2h2nj883x2w9iixf4x3vki9b22i"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "setup.py"
                  ;; remove dependency on outdated "distribute" module
                  (("^from distribute_setup import use_setuptools") "")
                  (("^use_setuptools\\(\\)") "")))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ;tests fail because test data are not included
       #:python ,python-2))
    (inputs
     `(("python-numpy" ,python2-numpy)
       ("zlib" ,zlib)))
    (native-inputs
     `(("python-nose" ,python2-nose)))
    (home-page "http://bitbucket.org/james_taylor/bx-python/")
    (synopsis "Tools for manipulating biological data")
    (description
     "bx-python provides tools for manipulating biological data, particularly
multiple sequence alignments.")
    (license license:expat)))

(define-public python-pysam
  (package
    (name "python-pysam")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              ;; Test data is missing on PyPi.
              (uri (string-append
                    "https://github.com/pysam-developers/pysam/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dzap2axin9cbbl0d825w294bpn00zagfm1sigamm4v2pm5bj9lp"))
              (modules '((guix build utils)))
              (snippet
               ;; Drop bundled htslib. TODO: Also remove samtools and bcftools.
               '(delete-file-recursively "htslib"))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((ice-9 ftw)
                  (srfi srfi-26)
                  (guix build python-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-flags
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "HTSLIB_MODE" "external")
             (setenv "HTSLIB_LIBRARY_DIR"
                     (string-append (assoc-ref inputs "htslib") "/lib"))
             (setenv "HTSLIB_INCLUDE_DIR"
                     (string-append (assoc-ref inputs "htslib") "/include"))
             (setenv "LDFLAGS" "-lncurses")
             (setenv "CFLAGS" "-D_CURSES_LIB=1")
             #t))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Add first subdirectory of "build" directory to PYTHONPATH.
             (setenv "PYTHONPATH"
                     (string-append
                      (getenv "PYTHONPATH")
                      ":" (getcwd) "/build/"
                      (car (scandir "build"
                                    (negate (cut string-prefix? "." <>))))))
             ;; Step out of source dir so python does not import from CWD.
             (with-directory-excursion "tests"
               (setenv "HOME" "/tmp")
               (and (zero? (system* "make" "-C" "pysam_data"))
                    (zero? (system* "make" "-C" "cbcf_data"))
                    ;; Running nosetests without explicitly asking for a
                    ;; single process leads to a crash.  Running with multiple
                    ;; processes fails because the tests are not designed to
                    ;; run in parallel.

                    ;; FIXME: tests keep timing out on some systems.
                    ;; (zero? (system* "nosetests" "-v"
                    ;;                 "--processes" "1"))
                    )))))))
    (propagated-inputs
     `(("htslib"            ,htslib))) ; Included from installed header files.
    (inputs
     `(("ncurses"           ,ncurses)
       ("zlib"              ,zlib)))
    (native-inputs
     `(("python-cython"     ,python-cython)
       ;; Dependencies below are are for tests only.
       ("samtools"          ,samtools)
       ("bcftools"          ,bcftools)
       ("python-nose"       ,python-nose)))
    (home-page "https://github.com/pysam-developers/pysam")
    (synopsis "Python bindings to the SAMtools C API")
    (description
     "Pysam is a Python module for reading and manipulating files in the
SAM/BAM format.  Pysam is a lightweight wrapper of the SAMtools C API.  It
also includes an interface for tabix.")
    (license license:expat)))

(define-public python2-pysam
  (package-with-python2 python-pysam))

(define-public python-twobitreader
  (package
    (name "python-twobitreader")
    (version "3.1.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "twobitreader" version))
              (sha256
               (base32
                "1q8wnj2kga9nz1lwc4w7qv52smfm536hp6mc8w6s53lhyj0mpi22"))))
    (build-system python-build-system)
    (arguments
     '(;; Tests are not distributed in the PyPi release.
       ;; TODO Try building from the Git repo or asking the upstream maintainer
       ;; to distribute the tests on PyPi.
       #:tests? #f))
    (native-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/benjschiller/twobitreader")
    (synopsis "Python library for reading .2bit files")
    (description
     "twobitreader is a Python library for reading .2bit files as used by the
UCSC genome browser.")
    (license license:artistic2.0)))

(define-public python2-twobitreader
  (package-with-python2 python-twobitreader))

(define-public python-plastid
  (package
    (name "python-plastid")
    (version "0.4.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "plastid" version))
              (sha256
               (base32
                "0l24dd3q66if8yj042m4s0g95n6acn7im1imqd3p6h8ns43kxhj8"))))
    (build-system python-build-system)
    (arguments
     ;; Some test files are not included.
     `(#:tests? #f))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-pandas" ,python-pandas)
       ("python-pysam" ,python-pysam)
       ("python-matplotlib" ,python-matplotlib)
       ("python-biopython" ,python-biopython)
       ("python-twobitreader" ,python-twobitreader)
       ("python-termcolor" ,python-termcolor)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/joshuagryphon/plastid")
    (synopsis "Python library for genomic analysis")
    (description
     "plastid is a Python library for genomic analysis – in particular,
high-throughput sequencing data – with an emphasis on simplicity.")
    (license license:bsd-3)))

(define-public python2-plastid
  (package-with-python2 python-plastid))

(define-public cd-hit
  (package
    (name "cd-hit")
    (version "4.6.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/weizhongli/cdhit"
                                  "/releases/download/V" version
                                  "/cd-hit-v" version
                                  "-2017-0621-source.tar.gz"))
              (sha256
               (base32
                "1b4mwm2520ixjbw57sil20f9iixzw4bkdqqwgg1fc3pzm6rz4zmn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:make-flags
       ;; Executables are copied directly to the PREFIX.
       (list (string-append "PREFIX=" (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         ;; No "configure" script
         (delete 'configure)
         ;; Remove sources of non-determinism
         (add-after 'unpack 'be-timeless
           (lambda _
             (substitute* "cdhit-utility.c++"
               ((" \\(built on \" __DATE__ \"\\)") ""))
             (substitute* "cdhit-common.c++"
               (("__DATE__") "\"0\"")
               (("\", %s, \" __TIME__ \"\\\\n\", date") ""))
             #t))
         ;; The "install" target does not create the target directory.
         (add-before 'install 'create-target-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
             #t)))))
    (inputs
     `(("perl" ,perl)))
    (home-page "http://weizhongli-lab.org/cd-hit/")
    (synopsis "Cluster and compare protein or nucleotide sequences")
    (description
     "CD-HIT is a program for clustering and comparing protein or nucleotide
sequences.  CD-HIT is designed to be fast and handle extremely large
databases.")
    ;; The manual says: "It can be copied under the GNU General Public License
    ;; version 2 (GPLv2)."
    (license license:gpl2)))

(define-public clipper
  (package
    (name "clipper")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/YeoLab/clipper/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pflmsvhbf8izbgwhbhj1i7349sw1f55qpqj8ljmapp16hb0p0qi"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; remove unnecessary setup dependency
                  (substitute* "setup.py"
                    (("setup_requires = .*") ""))
                  (for-each delete-file
                            '("clipper/src/peaks.so"
                              "clipper/src/readsToWiggle.so"))
                  (delete-file-recursively "dist/")
                  #t))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2)) ; only Python 2 is supported
    (inputs
     `(("htseq" ,python2-htseq)
       ("python-pybedtools" ,python2-pybedtools)
       ("python-cython" ,python2-cython)
       ("python-scikit-learn" ,python2-scikit-learn)
       ("python-matplotlib" ,python2-matplotlib)
       ("python-pandas" ,python2-pandas)
       ("python-pysam" ,python2-pysam)
       ("python-numpy" ,python2-numpy)
       ("python-scipy" ,python2-scipy)))
    (native-inputs
     `(("python-mock" ,python2-mock)   ; for tests
       ("python-nose" ,python2-nose)   ; for tests
       ("python-pytz" ,python2-pytz))) ; for tests
    (home-page "https://github.com/YeoLab/clipper")
    (synopsis "CLIP peak enrichment recognition")
    (description
     "CLIPper is a tool to define peaks in CLIP-seq datasets.")
    (license license:gpl2)))

(define-public codingquarry
  (package
    (name "codingquarry")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/codingquarry/CodingQuarry_v"
                    version ".tar.gz"))
              (sha256
               (base32
                "0115hkjflsnfzn36xppwf9h9avfxlavr43djqmshkkzbgjzsz60i"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/codingquarry")))
               (install-file "INSTRUCTIONS.pdf" doc)
               (copy-recursively "QuarryFiles"
                                 (string-append out "/QuarryFiles"))
               (install-file "CodingQuarry" bin)
               (install-file "CufflinksGTF_to_CodingQuarryGFF3.py" bin)))))))
    (inputs `(("openmpi" ,openmpi)))
    (native-search-paths
     (list (search-path-specification
            (variable "QUARRY_PATH")
            (files '("QuarryFiles")))))
    (native-inputs `(("python" ,python-2))) ; Only Python 2 is supported
    (synopsis "Fungal gene predictor")
    (description "CodingQuarry is a highly accurate, self-training GHMM fungal
gene predictor designed to work with assembled, aligned RNA-seq transcripts.")
    (home-page "https://sourceforge.net/projects/codingquarry/")
    (license license:gpl3+)))

(define-public couger
  (package
    (name "couger")
    (version "1.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://couger.oit.duke.edu/static/assets/COUGER"
                    version ".zip"))
              (sha256
               (base32
                "04p2b14nmhzxw5h72mpzdhalv21bx4w9b87z0wpw0xzxpysyncmq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin")))
              (copy-recursively "src" (string-append out "/src"))
              (mkdir bin)
              ;; Add "src" directory to module lookup path.
              (substitute* "couger"
                (("from argparse")
                 (string-append "import sys\nsys.path.append(\""
                                out "\")\nfrom argparse")))
              (install-file "couger" bin))
            #t))
         (add-after
          'install 'wrap-program
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; Make sure 'couger' runs with the correct PYTHONPATH.
            (let* ((out (assoc-ref outputs "out"))
                   (path (getenv "PYTHONPATH")))
              (wrap-program (string-append out "/bin/couger")
                `("PYTHONPATH" ":" prefix (,path))))
            #t)))))
    (inputs
     `(("python" ,python-2)
       ("python2-pillow" ,python2-pillow)
       ("python2-numpy" ,python2-numpy)
       ("python2-scipy" ,python2-scipy)
       ("python2-matplotlib" ,python2-matplotlib)))
    (propagated-inputs
     `(("r-minimal" ,r-minimal)
       ("libsvm" ,libsvm)
       ("randomjungle" ,randomjungle)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://couger.oit.duke.edu")
    (synopsis "Identify co-factors in sets of genomic regions")
    (description
     "COUGER can be applied to any two sets of genomic regions bound by
paralogous TFs (e.g., regions derived from ChIP-seq experiments) to identify
putative co-factors that provide specificity to each TF.  The framework
determines the genomic targets uniquely-bound by each TF, and identifies a
small set of co-factors that best explain the in vivo binding differences
between the two TFs.

COUGER uses classification algorithms (support vector machines and random
forests) with features that reflect the DNA binding specificities of putative
co-factors.  The features are generated either from high-throughput TF-DNA
binding data (from protein binding microarray experiments), or from large
collections of DNA motifs.")
    (license license:gpl3+)))

(define-public clustal-omega
  (package
    (name "clustal-omega")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.clustal.org/omega/clustal-omega-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1vm30mzncwdv881vrcwg11vzvrsmwy4wg80j5i0lcfk6dlld50w6"))))
    (build-system gnu-build-system)
    (inputs
     `(("argtable" ,argtable)))
    (home-page "http://www.clustal.org/omega/")
    (synopsis "Multiple sequence aligner for protein and DNA/RNA")
    (description
     "Clustal-Omega is a general purpose multiple sequence alignment (MSA)
program for protein and DNA/RNA.  It produces high quality MSAs and is capable
of handling data-sets of hundreds of thousands of sequences in reasonable
time.")
    (license license:gpl2+)))

(define-public crossmap
  (package
    (name "crossmap")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/crossmap/CrossMap-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "07y179f63d7qnzdvkqcziwk9bs3k4zhp81q392fp1hwszjdvy22f"))
              ;; This patch has been sent upstream already and is available
              ;; for download from Sourceforge, but it has not been merged.
              (patches (search-patches "crossmap-allow-system-pysam.patch"))
              (modules '((guix build utils)))
              ;; remove bundled copy of pysam
              (snippet
               '(delete-file-recursively "lib/pysam"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _ (setenv "CROSSMAP_USE_SYSTEM_PYSAM" "1") #t)))))
    (inputs
     `(("python-numpy" ,python2-numpy)
       ("python-pysam" ,python2-pysam)
       ("zlib" ,zlib)))
    (native-inputs
     `(("python-cython" ,python2-cython)
       ("python-nose" ,python2-nose)))
    (home-page "http://crossmap.sourceforge.net/")
    (synopsis "Convert genome coordinates between assemblies")
    (description
     "CrossMap is a program for conversion of genome coordinates or annotation
files between different genome assemblies.  It supports most commonly used
file formats including SAM/BAM, Wiggle/BigWig, BED, GFF/GTF, VCF.")
    (license license:gpl2+)))

(define-public cutadapt
  (package
    (name "cutadapt")
    (version "1.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/marcelm/cutadapt.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "09pr02067jiks19nc0aby4xp70hhgvb554i2y1c04rv1m401w7q8"))))
    (build-system python-build-system)
    (inputs
     `(("python-xopen" ,python-xopen)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-pytest" ,python-pytest)))
    (home-page "https://cutadapt.readthedocs.io/en/stable/")
    (synopsis "Remove adapter sequences from nucleotide sequencing reads")
    (description
     "Cutadapt finds and removes adapter sequences, primers, poly-A tails and
other types of unwanted sequence from high-throughput sequencing reads.")
    (license license:expat)))

(define-public libbigwig
  (package
    (name "libbigwig")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/dpryan79/libBigWig/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "098rjh35pi4a9q83n8wiwvyzykjqj6l8q189p1xgfw4ghywdlvw1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list "CC=gcc"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'check 'disable-curl-test
           (lambda _
             (substitute* "Makefile"
               (("./test/testRemote.*") ""))
             #t))
         ;; This has been fixed with the upstream commit 4ff6959cd8a0, but
         ;; there has not yet been a release containing this change.
         (add-before 'install 'create-target-dirs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib"))
               (mkdir-p (string-append out "/include"))
               #t))))))
    (inputs
     `(("zlib" ,zlib)
       ("curl" ,curl)))
    (native-inputs
     `(("doxygen" ,doxygen)))
    (home-page "https://github.com/dpryan79/libBigWig")
    (synopsis "C library for handling bigWig files")
    (description
     "This package provides a C library for parsing local and remote BigWig
files.")
    (license license:expat)))

(define-public python-pybigwig
  (package
    (name "python-pybigwig")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyBigWig" version))
              (sha256
               (base32
                "0yrpdxg3y0sny25x4w22lv1k47jzccqjmg7j4bp0hywklvp0hg7d"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled libBigWig sources
                  (delete-file-recursively "libBigWig")))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-with-libBigWig
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               (("libs=\\[") "libs=[\"BigWig\", "))
             #t)))))
    (inputs
     `(("libbigwig" ,libbigwig)
       ("zlib" ,zlib)
       ("curl" ,curl)))
    (home-page "https://github.com/dpryan79/pyBigWig")
    (synopsis "Access bigWig files in Python using libBigWig")
    (description
     "This package provides Python bindings to the libBigWig library for
accessing bigWig files.")
    (license license:expat)))

(define-public python2-pybigwig
  (package-with-python2 python-pybigwig))

(define-public python-dendropy
  (package
    (name "python-dendropy")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "DendroPy" version))
       (sha256
        (base32
         "15c7s3d5gf19ljsxvq5advaa752wfi7pwrdjyhzmg85hccyvp47p"))
       (patches (search-patches "python-dendropy-fix-tests.patch"))))
    (build-system python-build-system)
    (home-page "http://packages.python.org/DendroPy/")
    (synopsis "Library for phylogenetics and phylogenetic computing")
    (description
     "DendroPy is a library for phylogenetics and phylogenetic computing: reading,
writing, simulation, processing and manipulation of phylogenetic
trees (phylogenies) and characters.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-dendropy))))))

(define-public python2-dendropy
  (let ((base (package-with-python2 (strip-python2-variant python-dendropy))))
    (package
      (inherit base)
      (arguments
       `(#:python ,python-2
         #:phases
           (modify-phases %standard-phases
             (replace 'check
               ;; There is currently a test failure that only happens on some
               ;; systems, and only using "setup.py test"
               (lambda _ (zero? (system* "nosetests")))))))
      (native-inputs `(("python2-nose" ,python2-nose)
                       ,@(package-native-inputs base))))))

(define-public python-py2bit
  (package
    (name "python-py2bit")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py2bit" version))
       (sha256
        (base32
         "1cdf4qlmgwsh1f4k0wdv2sr8x9qn4366p0k3614vbd0fpqiarxrl"))))
    (build-system python-build-system)
    (home-page "https://github.com/dpryan79/py2bit")
    (synopsis "Access 2bit files using lib2bit")
    (description
     "This package provides Python bindings for lib2bit to access 2bit files
with Python.")
    (license license:expat)))

(define-public deeptools
  (package
    (name "deeptools")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/deeptools/deepTools/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q8i12l2gvk4n2s8lhyzwhh9g4qbc8lrk5l7maz00yvd5g6z5540"))))
    (build-system python-build-system)
    (inputs
     `(("python-scipy" ,python-scipy)
       ("python-numpy" ,python-numpy)
       ("python-numpydoc" ,python-numpydoc)
       ("python-matplotlib" ,python-matplotlib)
       ("python-pysam" ,python-pysam)
       ("python-py2bit" ,python-py2bit)
       ("python-pybigwig" ,python-pybigwig)))
    (native-inputs
     `(("python-mock" ,python-mock)   ;for tests
       ("python-nose" ,python-nose)   ;for tests
       ("python-pytz" ,python-pytz))) ;for tests
    (home-page "https://github.com/deeptools/deepTools")
    (synopsis "Tools for normalizing and visualizing deep-sequencing data")
    (description
     "DeepTools addresses the challenge of handling the large amounts of data
that are now routinely generated from DNA sequencing centers.  To do so,
deepTools contains useful modules to process the mapped reads data to create
coverage files in standard bedGraph and bigWig file formats.  By doing so,
deepTools allows the creation of normalized coverage files or the comparison
between two files (for example, treatment and control).  Finally, using such
normalized and standardized files, multiple visualizations can be created to
identify enrichments with functional annotations of the genome.")
    (license license:gpl3+)))

(define-public diamond
  (package
    (name "diamond")
    (version "0.9.18")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/bbuchfink/diamond/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vi2nddmy7knrv8gsprwqp6a40k63n3f2dfvx22ipjhrg9xir96f"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-native-compilation
           (lambda _
             (substitute* "CMakeLists.txt" (("-march=native") ""))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/bbuchfink/diamond")
    (synopsis "Accelerated BLAST compatible local sequence aligner")
    (description
     "DIAMOND is a BLAST-compatible local aligner for mapping protein and
translated DNA query sequences against a protein reference database (BLASTP
and BLASTX alignment mode).  The speedup over BLAST is up to 20,000 on short
reads at a typical sensitivity of 90-99% relative to BLAST depending on the
data and settings.")
    (license license:agpl3+)))

(define-public discrover
  (package
    (name "discrover")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/maaskola/discrover/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0rah9ja4m0rl5mldd6vag9rwrivw1zrqxssfq8qx64m7961fp68k"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-missing-includes
           (lambda _
             (substitute* "src/executioninformation.hpp"
               (("#define EXECUTIONINFORMATION_HPP" line)
                (string-append line "\n#include <random>")))
             (substitute* "src/plasma/fasta.hpp"
               (("#define FASTA_HPP" line)
                (string-append line "\n#include <random>")))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("cairo" ,cairo)))
    (native-inputs
     `(("texlive" ,texlive)
       ("imagemagick" ,imagemagick)))
    (home-page "http://dorina.mdc-berlin.de/public/rajewsky/discrover/")
    (synopsis "Discover discriminative nucleotide sequence motifs")
    (description "Discrover is a motif discovery method to find binding sites
of nucleic acid binding proteins.")
    (license license:gpl3+)))

(define-public eigensoft
  (let ((revision "1")
        (commit "b14d1e202e21e532536ff8004f0419cd5e259dc7"))
    (package
      (name "eigensoft")
      (version (string-append "6.1.2-"
                              revision "."
                              (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/DReichLab/EIG.git")
               (commit commit)))
         (file-name (string-append "eigensoft-" commit "-checkout"))
         (sha256
          (base32
           "0f5m6k2j5c16xc3xbywcs989xyc26ncy1zfzp9j9n55n9r4xcaiq"))
         (modules '((guix build utils)))
         ;; Remove pre-built binaries.
         (snippet '(begin
                     (delete-file-recursively "bin")
                     (mkdir "bin")
                     #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; There are no tests.
         #:make-flags '("CC=gcc")
         #:phases
         (modify-phases %standard-phases
           ;; There is no configure phase, but the Makefile is in a
           ;; sub-directory.
           (replace 'configure
             (lambda _
               (chdir "src")
               ;; The link flags are incomplete.
               (substitute* "Makefile"
                 (("-lgsl") "-lgsl -lm -llapack -llapacke -lpthread"))
               #t))
           ;; The provided install target only copies executables to
           ;; the "bin" directory in the build root.
           (add-after 'install 'actually-install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin  (string-append out "/bin")))
                 (for-each (lambda (file)
                             (install-file file bin))
                           (find-files "../bin" ".*"))
                 #t))))))
      (inputs
       `(("gsl" ,gsl)
         ("lapack" ,lapack)
         ("openblas" ,openblas)
         ("perl" ,perl)
         ("gfortran" ,gfortran "lib")))
      (home-page "https://github.com/DReichLab/EIG")
      (synopsis "Tools for population genetics")
      (description "The EIGENSOFT package provides tools for population
genetics and stratification correction.  EIGENSOFT implements methods commonly
used in population genetics analyses such as PCA, computation of Tracy-Widom
statistics, and finding related individuals in structured populations.  It
comes with a built-in plotting script and supports multiple file formats and
quantitative phenotypes.")
      ;; The license of the eigensoft tools is Expat, but since it's
      ;; linking with the GNU Scientific Library (GSL) the effective
      ;; license is the GPL.
      (license license:gpl3+))))

(define-public edirect
  (package
    (name "edirect")
    (version "4.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.ncbi.nlm.nih.gov/entrez/entrezdirect/"
                                  "versions/2016-05-03/edirect.tar.gz"))
              (sha256
               (base32
                "15zsprak5yh8c1yrz4r1knmb5s8qcmdid4xdhkh3lqcv64l60hli"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((target (string-append (assoc-ref outputs "out")
                                                 "/bin")))
                      (mkdir-p target)
                      (install-file "edirect.pl" target)
                      #t)))
         (add-after
          'install 'wrap-program
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; Make sure 'edirect.pl' finds all perl inputs at runtime.
            (let* ((out (assoc-ref outputs "out"))
                   (path (getenv "PERL5LIB")))
              (wrap-program (string-append out "/bin/edirect.pl")
                `("PERL5LIB" ":" prefix (,path)))))))))
    (inputs
     `(("perl-html-parser" ,perl-html-parser)
       ("perl-encode-locale" ,perl-encode-locale)
       ("perl-file-listing" ,perl-file-listing)
       ("perl-html-tagset" ,perl-html-tagset)
       ("perl-html-tree" ,perl-html-tree)
       ("perl-http-cookies" ,perl-http-cookies)
       ("perl-http-date" ,perl-http-date)
       ("perl-http-message" ,perl-http-message)
       ("perl-http-negotiate" ,perl-http-negotiate)
       ("perl-lwp-mediatypes" ,perl-lwp-mediatypes)
       ("perl-lwp-protocol-https" ,perl-lwp-protocol-https)
       ("perl-net-http" ,perl-net-http)
       ("perl-uri" ,perl-uri)
       ("perl-www-robotrules" ,perl-www-robotrules)
       ("perl" ,perl)))
    (home-page "http://www.ncbi.nlm.nih.gov/books/NBK179288/")
    (synopsis "Tools for accessing the NCBI's set of databases")
    (description
     "Entrez Direct (EDirect) is a method for accessing the National Center
for Biotechnology Information's (NCBI) set of interconnected
databases (publication, sequence, structure, gene, variation, expression,
etc.) from a terminal.  Functions take search terms from command-line
arguments.  Individual operations are combined to build multi-step queries.
Record retrieval and formatting normally complete the process.

EDirect also provides an argument-driven function that simplifies the
extraction of data from document summaries or other results that are returned
in structured XML format.  This can eliminate the need for writing custom
software to answer ad hoc questions.")
    (license license:public-domain)))

(define-public exonerate
  (package
    (name "exonerate")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "http://ftp.ebi.ac.uk/pub/software/vertebrategenomics/exonerate/"
         "exonerate-" version ".tar.gz"))
       (sha256
        (base32
         "0hj0m9xygiqsdxvbg79wq579kbrx1mdrabi2bzqz2zn9qwfjcjgq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f)) ; Building in parallel fails on some machines.
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)))
    (home-page
     "https://www.ebi.ac.uk/about/vertebrate-genomics/software/exonerate")
    (synopsis "Generic tool for biological sequence alignment")
    (description
     "Exonerate is a generic tool for pairwise sequence comparison.  It allows
the alignment of sequences using a many alignment models, either exhaustive
dynamic programming or a variety of heuristics.")
    (license license:gpl3)))

(define-public express
  (package
    (name "express")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "http://bio.math.berkeley.edu/eXpress/downloads/express-"
                version "/express-" version "-src.tgz"))
              (sha256
               (base32
                "03rczxd0gjp2l1jxcmjfmf5j94j77zqyxa6x063zsc585nj40n0c"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-shared-boost-libs-and-set-bamtools-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("set\\(Boost_USE_STATIC_LIBS ON\\)")
                "set(Boost_USE_STATIC_LIBS OFF)")
               (("\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}/bamtools/include")
                (string-append (assoc-ref inputs "bamtools") "/include/bamtools")))
             (substitute* "src/CMakeLists.txt"
               (("\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}/\\.\\./bamtools/lib")
                (string-append (assoc-ref inputs "bamtools") "/lib/bamtools")))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("bamtools" ,bamtools)
       ("protobuf" ,protobuf)
       ("zlib" ,zlib)))
    (home-page "http://bio.math.berkeley.edu/eXpress")
    (synopsis "Streaming quantification for high-throughput genomic sequencing")
    (description
     "eXpress is a streaming tool for quantifying the abundances of a set of
target sequences from sampled subsequences.  Example applications include
transcript-level RNA-Seq quantification, allele-specific/haplotype expression
analysis (from RNA-Seq), transcription factor binding quantification in
ChIP-Seq, and analysis of metagenomic data.")
    (license license:artistic2.0)))

(define-public express-beta-diversity
  (package
   (name "express-beta-diversity")
   (version "1.0.7")
   (source (origin
             (method url-fetch)
             (uri
              (string-append
               "https://github.com/dparks1134/ExpressBetaDiversity/archive/v"
               version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1djvdlmqvjf6h0zq7w36y8cl5cli6rgj86x65znl48agnwmzxfxr"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'enter-source (lambda _ (chdir "source") #t))
        (replace 'check
                 (lambda _ (zero? (system* "../bin/ExpressBetaDiversity"
                                           "-u"))))
        (add-after 'check 'exit-source (lambda _ (chdir "..") #t))
        (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((bin (string-append (assoc-ref outputs "out")
                                             "/bin")))
                     (mkdir-p bin)
                     (install-file "scripts/convertToEBD.py" bin)
                     (install-file "bin/ExpressBetaDiversity" bin)
                     #t))))))
   (inputs
    `(("python" ,python-2)))
   (home-page "http://kiwi.cs.dal.ca/Software/ExpressBetaDiversity")
   (synopsis "Taxon- and phylogenetic-based beta diversity measures")
   (description
    "Express Beta Diversity (EBD) calculates ecological beta diversity
(dissimilarity) measures between biological communities.  EBD implements a
variety of diversity measures including those that make use of phylogenetic
similarity of community members.")
   (license license:gpl3+)))

(define-public fasttree
  (package
   (name "fasttree")
   (version "2.1.10")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.microbesonline.org/fasttree/FastTree-"
                   version ".c"))
             (sha256
              (base32
               "0vcjdvy1j4m702vmak4svbfkrpcw63k7wymfksjp9a982zy8kjsl"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; no "check" target
      #:phases
      (modify-phases %standard-phases
        (delete 'unpack)
        (delete 'configure)
        (replace 'build
          (lambda* (#:key source #:allow-other-keys)
            (and (zero? (system* "gcc"
                                 "-O3"
                                 "-finline-functions"
                                 "-funroll-loops"
                                 "-Wall"
                                 "-o"
                                 "FastTree"
                                 source
                                 "-lm"))
                 (zero? (system* "gcc"
                                 "-DOPENMP"
                                 "-fopenmp"
                                 "-O3"
                                 "-finline-functions"
                                 "-funroll-loops"
                                 "-Wall"
                                 "-o"
                                 "FastTreeMP"
                                 source
                                 "-lm")))))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out")
                                      "/bin")))
              (mkdir-p bin)
              (install-file "FastTree" bin)
              (install-file "FastTreeMP" bin)
              #t))))))
   (home-page "http://www.microbesonline.org/fasttree")
   (synopsis "Infers approximately-maximum-likelihood phylogenetic trees")
   (description
    "FastTree can handle alignments with up to a million of sequences in a
reasonable amount of time and memory.  For large alignments, FastTree is
100-1,000 times faster than PhyML 3.0 or RAxML 7.")
   (license license:gpl2+)))

(define-public fastx-toolkit
  (package
    (name "fastx-toolkit")
    (version "0.0.14")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/agordon/fastx_toolkit/releases/download/"
                version "/fastx_toolkit-" version ".tar.bz2"))
              (sha256
               (base32
                "01jqzw386873sr0pjp1wr4rn8fsga2vxs1qfmicvx1pjr72007wy"))))
    (build-system gnu-build-system)
    (inputs
     `(("libgtextutils" ,libgtextutils)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://hannonlab.cshl.edu/fastx_toolkit/")
    (synopsis "Tools for FASTA/FASTQ file preprocessing")
    (description
     "The FASTX-Toolkit is a collection of command line tools for Short-Reads
FASTA/FASTQ files preprocessing.

Next-Generation sequencing machines usually produce FASTA or FASTQ files,
containing multiple short-reads sequences.  The main processing of such
FASTA/FASTQ files is mapping the sequences to reference genomes.  However, it
is sometimes more productive to preprocess the files before mapping the
sequences to the genome---manipulating the sequences to produce better mapping
results.  The FASTX-Toolkit tools perform some of these preprocessing tasks.")
    (license license:agpl3+)))

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
     `(#:configure-flags (list
                          (string-append "-DFLEXBAR_BINARY_DIR="
                                         (assoc-ref %outputs "out")
                                         "/bin/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "PATH" (string-append
                             (assoc-ref outputs "out") "/bin:"
                             (getenv "PATH")))
             (chdir "../flexbar_v2.5_src/test")
             (zero? (system* "bash" "flexbar_validate.sh"))))
         (delete 'install))))
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

(define-public fraggenescan
  (package
    (name "fraggenescan")
    (version "1.30")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/fraggenescan/"
                       "FragGeneScan" version ".tar.gz"))
       (sha256
        (base32 "158dcnwczgcyhwm4qlx19sanrwgdpzf6bn2y57mbpx55lkgz1mzj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (string-append (assoc-ref outputs "out")))
                    (share (string-append out "/share/fraggenescan/")))
               (substitute* "run_FragGeneScan.pl"
                 (("system\\(\"rm")
                  (string-append "system(\"" (which "rm")))
                 (("system\\(\"mv")
                  (string-append "system(\"" (which "mv")))
                 (("\\\"awk") (string-append "\"" (which "awk")))
                 ;; This script and other programs expect the training files
                 ;; to be in the non-standard location bin/train/XXX. Change
                 ;; this to be share/fraggenescan/train/XXX instead.
                 (("^\\$train.file = \\$dir.*")
                  (string-append "$train_file = \""
                                 share
                                 "train/\".$FGS_train_file;")))
               (substitute* "run_hmm.c"
                 (("^  strcat\\(train_dir, \\\"train/\\\"\\);")
                  (string-append "  strcpy(train_dir, \"" share "/train/\");"))))
             #t))
         (replace 'build
           (lambda _ (and (zero? (system* "make" "clean"))
                          (zero? (system* "make" "fgs")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (string-append (assoc-ref outputs "out")))
                    (bin (string-append out "/bin/"))
                    (share (string-append out "/share/fraggenescan/train")))
               (install-file "run_FragGeneScan.pl" bin)
               (install-file "FragGeneScan" bin)
               (copy-recursively "train" share))))
         (delete 'check)
         (add-after 'install 'post-install-check
           ;; In lieu of 'make check', run one of the examples and check the
           ;; output files gets created.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (string-append (assoc-ref outputs "out")))
                    (bin (string-append out "/bin/"))
                    (frag (string-append bin "run_FragGeneScan.pl")))
               (and (zero? (system* frag ; Test complete genome.
                             "-genome=./example/NC_000913.fna"
                             "-out=./test2"
                             "-complete=1"
                             "-train=complete"))
                    (file-exists? "test2.faa")
                    (file-exists? "test2.ffn")
                    (file-exists? "test2.gff")
                    (file-exists? "test2.out")
                    (zero? (system* ; Test incomplete sequences.
                            frag
                            "-genome=./example/NC_000913-fgs.ffn"
                            "-out=out"
                            "-complete=0"
                            "-train=454_30")))))))))
    (inputs
     `(("perl" ,perl)
       ("python" ,python-2))) ;not compatible with python 3.
    (home-page "https://sourceforge.net/projects/fraggenescan/")
    (synopsis "Finds potentially fragmented genes in short reads")
    (description
     "FragGeneScan is a program for predicting bacterial and archaeal genes in
short and error-prone DNA sequencing reads.  It can also be applied to predict
genes in incomplete assemblies or complete genomes.")
    ;; GPL3+ according to private correspondense with the authors.
    (license license:gpl3+)))

(define-public fxtract
  (let ((util-commit "776ca85a18a47492af3794745efcb4a905113115"))
    (package
      (name "fxtract")
      (version "2.3")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://github.com/ctSkennerton/fxtract/archive/"
               version ".tar.gz"))
         (file-name (string-append "ctstennerton-util-"
                                   (string-take util-commit 7)
                                   "-checkout"))
         (sha256
          (base32
           "0275cfdhis8517hm01is62062swmi06fxzifq7mr3knbbxjlaiwj"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list
                       (string-append "PREFIX=" (assoc-ref %outputs "out"))
                       "CC=gcc")
         #:test-target "fxtract_test"
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'copy-util
             (lambda* (#:key inputs #:allow-other-keys)
               (rmdir "util")
               (copy-recursively (assoc-ref inputs "ctskennerton-util") "util")
               #t))
           ;; Do not use make install as this requires additional dependencies.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out"/bin")))
                 (install-file "fxtract" bin)
                 #t))))))
      (inputs
       `(("pcre" ,pcre)
         ("zlib" ,zlib)))
      (native-inputs
       ;; ctskennerton-util is licensed under GPL2.
       `(("ctskennerton-util"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/ctSkennerton/util.git")
                   (commit util-commit)))
             (file-name (string-append
                         "ctstennerton-util-" util-commit "-checkout"))
             (sha256
              (base32
               "0cls1hd4vgj3f36fpzzg4xc77d6f3hpc60cbpfmn2gdr7ykzzad7"))))))
      (home-page "https://github.com/ctSkennerton/fxtract")
      (synopsis "Extract sequences from FASTA and FASTQ files")
      (description
       "Fxtract extracts sequences from a protein or nucleotide fastx (FASTA
or FASTQ) file given a subsequence.  It uses a simple substring search for
basic tasks but can change to using POSIX regular expressions, PCRE, hash
lookups or multi-pattern searching as required.  By default fxtract looks in
the sequence of each record but can also be told to look in the header,
comment or quality sections.")
      ;; 'util' requires SSE instructions.
      (supported-systems '("x86_64-linux"))
      (license license:expat))))

(define-public gemma
  (package
    (name "gemma")
    (version "0.96")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/xiangzhou/GEMMA/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "055ynn16gd12pf78n4vr2a9jlwsbwzajpdnf2y2yilg1krfff222"))
              (patches (search-patches "gemma-intel-compat.patch"))))
    (inputs
     `(("gsl" ,gsl)
       ("lapack" ,lapack)
       ("zlib" ,zlib)))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       '(,@(match (%current-system)
         ("x86_64-linux"
          '("FORCE_DYNAMIC=1"))
         ("i686-linux"
          '("FORCE_DYNAMIC=1" "FORCE_32BIT=1"))
         (_
          '("FORCE_DYNAMIC=1" "NO_INTEL_COMPAT=1"))))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'bin-mkdir
          (lambda _
            (mkdir-p "bin")
            #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "bin/gemma"
                             (string-append
                              out "/bin")))
             #t)))
       #:tests? #f)) ; no tests included yet
    (home-page "https://github.com/xiangzhou/GEMMA")
    (synopsis "Tool for genome-wide efficient mixed model association")
    (description
     "Genome-wide Efficient Mixed Model Association (GEMMA) provides a
standard linear mixed model resolver with application in genome-wide
association studies (GWAS).")
    (license license:gpl3)))

(define-public grit
  (package
    (name "grit")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/nboley/grit/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "157in84dj70wimbind3x7sy1whs3h57qfgcnj2s6lrd38fbrb7mj"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'generate-from-cython-sources
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Delete these C files to force fresh generation from pyx sources.
             (delete-file "grit/sparsify_support_fns.c")
             (delete-file "grit/call_peaks_support_fns.c")
             (substitute* "setup.py"
               (("Cython.Setup") "Cython.Build")
               ;; Add numpy include path to fix compilation
               (("pyx\", \\]")
                (string-append "pyx\", ], include_dirs = ['"
                               (assoc-ref inputs "python-numpy")
                               "/lib/python2.7/site-packages/numpy/core/include/"
                               "']")))
             #t)))))
    (inputs
     `(("python-scipy" ,python2-scipy)
       ("python-numpy" ,python2-numpy)
       ("python-pysam" ,python2-pysam)
       ("python-networkx" ,python2-networkx)))
    (native-inputs
     `(("python-cython" ,python2-cython)))
    (home-page "http://grit-bio.org")
    (synopsis "Tool for integrative analysis of RNA-seq type assays")
    (description
     "GRIT is designed to use RNA-seq, TES, and TSS data to build and quantify
full length transcript models.  When none of these data sources are available,
GRIT can be run by providing a candidate set of TES or TSS sites.  In
addition, GRIT can merge in reference junctions and gene boundaries.  GRIT can
also be run in quantification mode, where it uses a provided GTF file and just
estimates transcript expression.")
    (license license:gpl3+)))

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
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
           (lambda _
             ;; XXX Cannot use snippet because zip files are not supported
             (substitute* "Makefile"
               (("^CC = .*$") "CC = gcc")
               (("^CPP = .*$") "CPP = g++")
               ;; replace BUILD_HOST and BUILD_TIME for deterministic build
               (("-DBUILD_HOST=.*") "-DBUILD_HOST=\"\\\"guix\\\"\"")
               (("-DBUILD_TIME=.*") "-DBUILD_TIME=\"\\\"0\\\"\""))
             (substitute* '("hisat-build" "hisat-inspect")
               (("/usr/bin/env") (which "env")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files
                          "."
                          "hisat(-(build|align|inspect)(-(s|l)(-debug)*)*)*$")))
             #t))
         (delete 'configure))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("perl" ,perl)
       ("python" ,python)
       ("zlib" ,zlib)))
    ;; Non-portable SSE instructions are used so building fails on platforms
    ;; other than x86_64.
    (supported-systems '("x86_64-linux"))
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

(define-public hisat2
  (package
    (name "hisat2")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       ;; FIXME: a better source URL is
       ;; (string-append "ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2"
       ;;                "/downloads/hisat2-" version "-source.zip")
       ;; with hash "0lywnr8kijwsc2aw10dwxic0n0yvip6fl3rjlvc8zzwahamy4x7g"
       ;; but it is currently unavailable.
       (uri "https://github.com/infphilo/hisat2/archive/cba6e8cb.tar.gz")
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1mf2hdsyv7cd97xm9mp9a4qws02yrj95y6w6f6cdwnq0klp81r50"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags (list "CC=gcc" "CXX=g++" "allall")
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-deterministic
           (lambda _
             (substitute* "Makefile"
               (("`date`") "0"))
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (doc (string-append out "/share/doc/hisat2/")))
               (for-each
                (cut install-file <> bin)
                (find-files "."
                            "hisat2(-(build|align|inspect)(-(s|l)(-debug)*)*)*$"))
               (mkdir-p doc)
               (install-file "doc/manual.inc.html" doc))
             #t)))))
    (native-inputs
     `(("unzip" ,unzip)                 ; needed for archive from ftp
       ("perl" ,perl)
       ("pandoc" ,ghc-pandoc)))         ; for documentation
    (home-page "http://ccb.jhu.edu/software/hisat2/index.shtml")
    (synopsis "Graph-based alignment of genomic sequencing reads")
    (description "HISAT2 is a fast and sensitive alignment program for mapping
next-generation sequencing reads (both DNA and RNA) to a population of human
genomes (as well as to a single reference genome).  In addition to using one
global @dfn{graph FM} (GFM) index that represents a population of human
genomes, HISAT2 uses a large set of small GFM indexes that collectively cover
the whole genome.  These small indexes, combined with several alignment
strategies, enable rapid and accurate alignment of sequencing reads.  This new
indexing scheme is called a @dfn{Hierarchical Graph FM index} (HGFM).")
    ;; HISAT2 contains files from Bowtie2, which is released under
    ;; GPLv2 or later.  The HISAT2 source files are released under
    ;; GPLv3 or later.
    (license license:gpl3+)))

(define-public hmmer
  (package
    (name "hmmer")
    (version "3.1b2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://eddylab.org/software/hmmer"
             (version-major version) "/"
             version "/hmmer-" version ".tar.gz"))
       (sha256
        (base32
         "0djmgc0pfli0jilfx8hql1axhwhqxqb8rxg2r5rg07aw73sfs5nx"))
       (patches (search-patches "hmmer-remove-cpu-specificity.patch"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))
    (home-page "http://hmmer.org/")
    (synopsis "Biosequence analysis using profile hidden Markov models")
    (description
     "HMMER is used for searching sequence databases for homologs of protein
sequences, and for making protein sequence alignments.  It implements methods
using probabilistic models called profile hidden Markov models (profile
HMMs).")
    (license (list license:gpl3+
                   ;; The bundled library 'easel' is distributed
                   ;; under The Janelia Farm Software License.
                   (license:non-copyleft
                    "file://easel/LICENSE"
                    "See easel/LICENSE in the distribution.")))))

(define-public htseq
  (package
    (name "htseq")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "HTSeq" version))
              (sha256
               (base32
                "11flgb1381xdhk43bzbfm3vhnszkpqg6jk76rpa5xd1zbrvvlnxg"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-cython" ,python-cython)))
    ;; Numpy needs to be propagated when htseq is used as a Python library.
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
    (inputs
     `(("python-pysam" ,python-pysam)
       ("python-matplotlib" ,python-matplotlib)))
    (home-page "http://www-huber.embl.de/users/anders/HTSeq/")
    (synopsis "Analysing high-throughput sequencing data with Python")
    (description
     "HTSeq is a Python package that provides infrastructure to process data
from high-throughput sequencing assays.")
    (license license:gpl3+)))

(define-public python2-htseq
  (package-with-python2 htseq))

(define-public java-htsjdk
  (package
    (name "java-htsjdk")
    (version "2.3.0") ; last version without build dependency on gradle
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htsjdk/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ibhzzxsfc38nqyk9r8zqj6blfc1kh26iirypd4q6n90hs2m6nyq"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete pre-built binaries
               '(begin
                  (delete-file-recursively "lib")
                  (mkdir-p "lib")
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; test require Internet access
       #:jdk ,icedtea-8
       #:make-flags
       (list (string-append "-Ddist=" (assoc-ref %outputs "out")
                            "/share/java/htsjdk/"))
       #:build-target "all"
       #:phases
       (modify-phases %standard-phases
         ;; The build phase also installs the jars
         (delete 'install))))
    (inputs
     `(("java-ngs" ,java-ngs)
       ("java-snappy-1" ,java-snappy-1)
       ("java-commons-compress" ,java-commons-compress)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-jexl-2" ,java-commons-jexl-2)
       ("java-xz" ,java-xz)))
    (native-inputs
     `(("java-testng" ,java-testng)))
    (home-page "http://samtools.github.io/htsjdk/")
    (synopsis "Java API for high-throughput sequencing data (HTS) formats")
    (description
     "HTSJDK is an implementation of a unified Java library for accessing
common file formats, such as SAM and VCF, used for high-throughput
sequencing (HTS) data.  There are also an number of useful utilities for
manipulating HTS data.")
    (license license:expat)))

(define-public java-htsjdk-latest
  (package
    (name "java-htsjdk")
    (version "2.14.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/samtools/htsjdk.git")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1lmya1fdjy03mz6zmdmd86j9v9vfhqb3952mqq075navx1i6g4bc"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f                      ; test require Scala
       #:jdk ,icedtea-8
       #:jar-name "htsjdk.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-useless-build.xml
           (lambda _ (delete-file "build.xml") #t))
         ;; The tests require the scalatest package.
         (add-after 'unpack 'remove-tests
           (lambda _ (delete-file-recursively "src/test") #t)))))
    (inputs
     `(("java-ngs" ,java-ngs)
       ("java-snappy-1" ,java-snappy-1)
       ("java-commons-compress" ,java-commons-compress)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-jexl-2" ,java-commons-jexl-2)
       ("java-xz" ,java-xz)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "http://samtools.github.io/htsjdk/")
    (synopsis "Java API for high-throughput sequencing data (HTS) formats")
    (description
     "HTSJDK is an implementation of a unified Java library for accessing
common file formats, such as SAM and VCF, used for high-throughput
sequencing (HTS) data.  There are also an number of useful utilities for
manipulating HTS data.")
    (license license:expat)))

;; This is needed for picard 2.10.3
(define-public java-htsjdk-2.10.1
  (package (inherit java-htsjdk-latest)
    (name "java-htsjdk")
    (version "2.10.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/samtools/htsjdk.git")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1kxh7slm2pm3x9p6jxa1wqsq9a31dhiiflhxnxqcisan4k3rwia2"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f                      ; tests require Scala
       #:jdk ,icedtea-8
       #:jar-name "htsjdk.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-useless-build.xml
           (lambda _ (delete-file "build.xml") #t))
         ;; The tests require the scalatest package.
         (add-after 'unpack 'remove-tests
           (lambda _ (delete-file-recursively "src/test") #t)))))))

;; This version matches java-htsjdk 2.3.0.  Later versions also require a more
;; recent version of java-htsjdk, which depends on gradle.
(define-public java-picard
  (package
    (name "java-picard")
    (version "2.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/broadinstitute/picard.git")
                    (commit version)))
              (file-name (string-append "java-picard-" version "-checkout"))
              (sha256
               (base32
                "1ll7mf4r3by92w2nhlmpa591xd1f46xlkwh59mq6fvbb5pdwzvx6"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete pre-built binaries.
                  (delete-file-recursively "lib")
                  (mkdir-p "lib")
                  (substitute* "build.xml"
                    ;; Remove build-time dependency on git.
                    (("failifexecutionfails=\"true\"")
                     "failifexecutionfails=\"false\"")
                    ;; Use our htsjdk.
                    (("depends=\"compile-htsjdk, ")
                     "depends=\"")
                    (("depends=\"compile-htsjdk-tests, ")
                     "depends=\"")
                    ;; Build picard-lib.jar before building picard.jar
                    (("name=\"picard-jar\" depends=\"" line)
                     (string-append line "picard-lib-jar, ")))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "picard-jar"
       #:test-target "test"
       ;; Tests require jacoco:coverage.
       #:tests? #f
       #:make-flags
       (list (string-append "-Dhtsjdk_lib_dir="
                            (assoc-ref %build-inputs "java-htsjdk")
                            "/share/java/htsjdk/")
             "-Dhtsjdk-classes=dist/tmp"
             (string-append "-Dhtsjdk-version="
                            ,(package-version java-htsjdk)))
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-our-htsjdk
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("\\$\\{htsjdk\\}/lib")
                (string-append (assoc-ref inputs "java-htsjdk")
                               "/share/java/htsjdk/")))
             #t))
         (add-after 'unpack 'make-test-target-independent
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("name=\"test\" depends=\"compile, ")
                "name=\"test\" depends=\""))
             #t))
         (replace 'install (install-jars "dist")))))
    (inputs
     `(("java-htsjdk" ,java-htsjdk)
       ("java-guava" ,java-guava)))
    (native-inputs
     `(("java-testng" ,java-testng)))
    (home-page "http://broadinstitute.github.io/picard/")
    (synopsis "Tools for manipulating high-throughput sequencing data and formats")
    (description "Picard is a set of Java command line tools for manipulating
high-throughput sequencing (HTS) data and formats.  Picard is implemented
using the HTSJDK Java library to support accessing file formats that are
commonly used for high-throughput sequencing data such as SAM, BAM, CRAM and
VCF.")
    (license license:expat)))

;; This is needed for dropseq-tools
(define-public java-picard-2.10.3
  (package
    (name "java-picard")
    (version "2.10.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/broadinstitute/picard.git")
                    (commit version)))
              (file-name (string-append "java-picard-" version "-checkout"))
              (sha256
               (base32
                "1ajlx31l6i1k3y2rhnmgq07sz99g2czqfqgkr9mihmdjp3gwjhvi"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "picard.jar"
       ;; Tests require jacoco:coverage.
       #:tests? #f
       #:jdk ,icedtea-8
       #:main-class "picard.cmdline.PicardCommandLine"
       #:modules ((guix build ant-build-system)
                  (guix build utils)
                  (guix build java-utils)
                  (sxml simple)
                  (sxml transform)
                  (sxml xpath))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-useless-build.xml
           (lambda _ (delete-file "build.xml") #t))
         ;; This is necessary to ensure that htsjdk is found when using
         ;; picard.jar as an executable.
         (add-before 'build 'edit-classpath-in-manifest
           (lambda* (#:key inputs #:allow-other-keys)
             (chmod "build.xml" #o664)
             (call-with-output-file "build.xml.new"
               (lambda (port)
                 (sxml->xml
                  (pre-post-order
                   (with-input-from-file "build.xml"
                     (lambda _ (xml->sxml #:trim-whitespace? #t)))
                   `((target    . ,(lambda (tag . kids)
                                     (let ((name ((sxpath '(name *text*))
                                                  (car kids)))
                                           ;; FIXME: We're breaking the line
                                           ;; early with a dummy path to
                                           ;; ensure that the store reference
                                           ;; isn't broken apart and can still
                                           ;; be found by the reference
                                           ;; scanner.
                                           (msg (format #f
                                                        "\
Class-Path: /~a \
 ~a/share/java/htsjdk.jar${line.separator}"
                                                        ;; maximum line length is 70
                                                        (string-tabulate (const #\b) 57)
                                                        (assoc-ref inputs "java-htsjdk"))))
                                       (if (member "manifest" name)
                                           `(,tag ,@kids
                                                  (echo
                                                   (@ (message ,msg)
                                                      (file "${manifest.file}")
                                                      (append "true"))))
                                           `(,tag ,@kids)))))
                     (*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
                     (*text*    . ,(lambda (_ txt) txt))))
                  port)))
             (rename-file "build.xml.new" "build.xml")
             #t)))))
    (propagated-inputs
     `(("java-htsjdk" ,java-htsjdk-2.10.1)))
    (native-inputs
     `(("java-testng" ,java-testng)
       ("java-guava" ,java-guava)))
    (home-page "http://broadinstitute.github.io/picard/")
    (synopsis "Tools for manipulating high-throughput sequencing data and formats")
    (description "Picard is a set of Java command line tools for manipulating
high-throughput sequencing (HTS) data and formats.  Picard is implemented
using the HTSJDK Java library to support accessing file formats that are
commonly used for high-throughput sequencing data such as SAM, BAM, CRAM and
VCF.")
    (license license:expat)))

;; This is the last version of Picard to provide net.sf.samtools
(define-public java-picard-1.113
  (package (inherit java-picard)
    (name "java-picard")
    (version "1.113")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/broadinstitute/picard.git")
                    (commit version)))
              (file-name (string-append "java-picard-" version "-checkout"))
              (sha256
               (base32
                "0lkpvin2fz3hhly4l02kk56fqy8lmlgyzr9kmvljk6ry6l1hw973"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete pre-built binaries.
                  (delete-file-recursively "lib")
                  (mkdir-p "lib")
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "picard-jar"
       #:test-target "test"
       ;; FIXME: the class path at test time is wrong.
       ;; [testng] Error: A JNI error has occurred, please check your installation and try again
       ;; [testng] Exception in thread "main" java.lang.NoClassDefFoundError: com/beust/jcommander/ParameterException
       #:tests? #f
       #:jdk ,icedtea-8
       ;; This is only used for tests.
       #:make-flags
       (list "-Dsamjdk.intel_deflater_so_path=lib/jni/libIntelDeflater.so")
       #:phases
       (modify-phases %standard-phases
         ;; Do not use bundled ant bzip2.
         (add-after 'unpack 'use-ant-bzip
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("\\$\\{lib\\}/apache-ant-1.8.2-bzip2.jar")
                (string-append (assoc-ref inputs "ant")
                               "/lib/ant.jar")))
             #t))
         (add-after 'unpack 'make-test-target-independent
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("name=\"test\" depends=\"compile, ")
                "name=\"test\" depends=\"compile-tests, ")
               (("name=\"compile\" depends=\"compile-src, compile-tests\"")
                "name=\"compile\" depends=\"compile-src\""))
             #t))
         (add-after 'unpack 'fix-deflater-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/java/net/sf/samtools/Defaults.java"
               (("getStringProperty\\(\"intel_deflater_so_path\", null\\)")
                (string-append "getStringProperty(\"intel_deflater_so_path\", \""
                               (assoc-ref outputs "out")
                               "/lib/jni/libIntelDeflater.so"
                               "\")")))
             #t))
         ;; Build the deflater library, because we've previously deleted the
         ;; pre-built one.  This can only be built with access to the JDK
         ;; sources.
         (add-after 'build 'build-jni
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "lib/jni")
             (mkdir-p "jdk-src")
             (and (zero? (system* "tar" "--strip-components=1" "-C" "jdk-src"
                                  "-xf" (assoc-ref inputs "jdk-src")))
                  (zero? (system* "javah" "-jni"
                                  "-classpath" "classes"
                                  "-d" "lib/"
                                  "net.sf.samtools.util.zip.IntelDeflater"))
                  (with-directory-excursion "src/c/inteldeflater"
                    (zero? (system* "gcc" "-I../../../lib" "-I."
                                    (string-append "-I" (assoc-ref inputs "jdk")
                                                   "/include/linux")
                                    "-I../../../jdk-src/src/share/native/common/"
                                    "-I../../../jdk-src/src/solaris/native/common/"
                                    "-c" "-O3" "-fPIC" "IntelDeflater.c"))
                    (zero? (system* "gcc" "-shared"
                                    "-o" "../../../lib/jni/libIntelDeflater.so"
                                    "IntelDeflater.o" "-lz" "-lstdc++"))))))
         ;; We can only build everything else after building the JNI library.
         (add-after 'build-jni 'build-rest
           (lambda* (#:key make-flags #:allow-other-keys)
             (zero? (apply system* `("ant" "all" ,@make-flags)))))
         (add-before 'build 'set-JAVA6_HOME
           (lambda _
             (setenv "JAVA6_HOME" (getenv "JAVA_HOME"))
             #t))
         (replace 'install (install-jars "dist"))
         (add-after 'install 'install-jni-lib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((jni (string-append (assoc-ref outputs "out")
                                       "/lib/jni")))
               (mkdir-p jni)
               (install-file "lib/jni/libIntelDeflater.so" jni)
               #t))))))
    (inputs
     `(("java-snappy-1" ,java-snappy-1)
       ("java-commons-jexl-2" ,java-commons-jexl-2)
       ("java-cofoja" ,java-cofoja)
       ("ant" ,ant) ; for bzip2 support at runtime
       ("zlib" ,zlib)))
    (native-inputs
     `(("ant-apache-bcel" ,ant-apache-bcel)
       ("ant-junit" ,ant-junit)
       ("java-testng" ,java-testng)
       ("java-commons-bcel" ,java-commons-bcel)
       ("java-jcommander" ,java-jcommander)
       ("jdk" ,icedtea-8 "jdk")
       ("jdk-src" ,(car (assoc-ref (package-native-inputs icedtea-8) "jdk-drop")))))))

(define-public fastqc
  (package
    (name "fastqc")
    (version "0.11.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.bioinformatics.babraham.ac.uk/"
                           "projects/fastqc/fastqc_v"
                           version "_source.zip"))
       (sha256
        (base32
         "18rrlkhcrxvvvlapch4dpj6xc6mpayzys8qfppybi8jrpgx5cc5f"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:build-target "build"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dependencies
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("jbzip2-0.9.jar")
                (string-append (assoc-ref inputs "java-jbzip2")
                               "/share/java/jbzip2.jar"))
               (("sam-1.103.jar")
                (string-append (assoc-ref inputs "java-picard-1.113")
                               "/share/java/sam-1.112.jar"))
               (("cisd-jhdf5.jar")
                (string-append (assoc-ref inputs "java-cisd-jhdf5")
                               "/share/java/sis-jhdf5.jar")))
             #t))
         ;; There is no installation target
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share/fastqc/"))
                    (exe   (string-append share "/fastqc")))
               (for-each mkdir-p (list bin share))
               (copy-recursively "bin" share)
               (substitute* exe
                 (("my \\$java_bin = 'java';")
                  (string-append "my $java_bin = '"
                                 (assoc-ref inputs "java")
                                 "/bin/java';")))
               (chmod exe #o555)
               (symlink exe (string-append bin "/fastqc"))
               #t))))))
    (inputs
     `(("java" ,icedtea)
       ("perl" ,perl)                   ; needed for the wrapper script
       ("java-cisd-jhdf5" ,java-cisd-jhdf5)
       ("java-picard-1.113" ,java-picard-1.113)
       ("java-jbzip2" ,java-jbzip2)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.bioinformatics.babraham.ac.uk/projects/fastqc/")
    (synopsis "Quality control tool for high throughput sequence data")
    (description
     "FastQC aims to provide a simple way to do some quality control
checks on raw sequence data coming from high throughput sequencing
pipelines.  It provides a modular set of analyses which you can use to
give a quick impression of whether your data has any problems of which
you should be aware before doing any further analysis.

The main functions of FastQC are:

@itemize
@item Import of data from BAM, SAM or FastQ files (any variant);
@item Providing a quick overview to tell you in which areas there may
  be problems;
@item Summary graphs and tables to quickly assess your data;
@item Export of results to an HTML based permanent report;
@item Offline operation to allow automated generation of reports
  without running the interactive application.
@end itemize\n")
    (license license:gpl3+)))

(define-public htslib
  (package
    (name "htslib")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "1il6i2p84b0y9c93dhvzzki1ifw9bvapm2mvpr0xvb2nq8jlwgdy"))))
    (build-system gnu-build-system)
    (inputs
     `(("openssl" ,openssl)
       ("curl" ,curl)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "http://www.htslib.org")
    (synopsis "C library for reading/writing high-throughput sequencing data")
    (description
     "HTSlib is a C library for reading/writing high-throughput sequencing
data.  It also provides the @command{bgzip}, @command{htsfile}, and
@command{tabix} utilities.")
    ;; Files under cram/ are released under the modified BSD license;
    ;; the rest is released under the Expat license
    (license (list license:expat license:bsd-3))))

;; This package should be removed once no packages rely upon it.
(define htslib-1.3
  (package
    (inherit htslib)
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "1rja282fwdc25ql6izkhdyh8ppw8x2fs0w0js78zgkmqjlikmma9"))))))

(define-public idr
  (package
    (name "idr")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/nboley/idr/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rjdly6daslw66r43g9md8znizlscn1sphycqyldzsidkc4vxqv3"))
              ;; Delete generated C code.
              (snippet
               '(begin (delete-file "idr/inv_cdf.c") #t))))
    (build-system python-build-system)
    ;; There is only one test ("test_inv_cdf.py") and it tests features that
    ;; are no longer part of this package.  It also asserts False, which
    ;; causes the tests to always fail.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-scipy" ,python-scipy)
       ("python-sympy" ,python-sympy)
       ("python-numpy" ,python-numpy)
       ("python-matplotlib" ,python-matplotlib)))
    (native-inputs
     `(("python-cython" ,python-cython)))
    (home-page "https://github.com/nboley/idr")
    (synopsis "Tool to measure the irreproducible discovery rate (IDR)")
    (description
     "The IDR (Irreproducible Discovery Rate) framework is a unified approach
to measure the reproducibility of findings identified from replicate
experiments and provide highly stable thresholds based on reproducibility.")
    (license license:gpl2+)))

(define-public jellyfish
  (package
    (name "jellyfish")
    (version "2.2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gmarcais/Jellyfish/"
                                  "releases/download/v" version
                                  "/jellyfish-" version ".tar.gz"))
              (sha256
               (base32
                "1a1iwq9pq54k2m9ypvwl5s0bqfl64gwh9dx5af9i382ajas2016q"))))
    (build-system gnu-build-system)
    (outputs '("out"      ;for library
               "ruby"     ;for Ruby bindings
               "python")) ;for Python bindings
    (arguments
     `(#:configure-flags
       (list (string-append "--enable-ruby-binding="
                            (assoc-ref %outputs "ruby"))
             (string-append "--enable-python-binding="
                            (assoc-ref %outputs "python")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-SHELL-variable
           (lambda _
             ;; generator_manager.hpp either uses /bin/sh or $SHELL
             ;; to run tests.
             (setenv "SHELL" (which "bash"))
             #t)))))
    (native-inputs
     `(("bc" ,bc)
       ("time" ,time)
       ("ruby" ,ruby)
       ("python" ,python-2)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("htslib" ,htslib)))
    (synopsis "Tool for fast counting of k-mers in DNA")
    (description
     "Jellyfish is a tool for fast, memory-efficient counting of k-mers in
DNA.  A k-mer is a substring of length k, and counting the occurrences of all
such substrings is a central step in many analyses of DNA sequence.  Jellyfish
is a command-line program that reads FASTA and multi-FASTA files containing
DNA sequences.  It outputs its k-mer counts in a binary format, which can be
translated into a human-readable text format using the @code{jellyfish dump}
command, or queried for specific k-mers with @code{jellyfish query}.")
    (home-page "http://www.genome.umd.edu/jellyfish.html")
    ;; From their website: JELLYFISH runs on 64-bit Intel-compatible processors
    (supported-systems '("x86_64-linux"))
    ;; The combined work is published under the GPLv3 or later.  Individual
    ;; files such as lib/jsoncpp.cpp are released under the Expat license.
    (license (list license:gpl3+ license:expat))))

(define-public khmer
  (package
    (name "khmer")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "khmer" version))
       (sha256
        (base32
         "0wb05shqh77v00256qlm68vbbx3kl76fyzihszbz5nhanl4ni33a"))
       (patches (search-patches "khmer-use-libraries.patch"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Delete bundled libraries.
             (delete-file-recursively "third-party/zlib")
             (delete-file-recursively "third-party/bzip2")
             ;; Replace bundled seqan.
             (let* ((seqan-all "third-party/seqan")
                    (seqan-include (string-append
                                    seqan-all "/core/include")))
               (delete-file-recursively seqan-all)
               (copy-recursively (string-append (assoc-ref inputs "seqan")
                                                "/include/seqan")
                          (string-append seqan-include "/seqan")))
             ;; We do not replace the bundled MurmurHash as the canonical
             ;; repository for this code 'SMHasher' is unsuitable for
             ;; providing a library.  See
             ;; https://lists.gnu.org/archive/html/guix-devel/2016-06/msg00977.html
             #t))
         (add-after 'unpack 'set-cc
           (lambda _
             (setenv "CC" "gcc")
             #t))
         ;; It is simpler to test after installation.
         (delete 'check)
         (add-after 'install 'post-install-check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "PATH"
                       (string-append
                        (getenv "PATH")
                        ":"
                        (assoc-ref outputs "out")
                        "/bin"))
               (setenv "PYTHONPATH"
                       (string-append
                        (getenv "PYTHONPATH")
                        ":"
                        out
                        "/lib/python"
                        (string-take (string-take-right
                                      (assoc-ref inputs "python") 5) 3)
                        "/site-packages"))
               (with-directory-excursion "build"
                 (zero? (system* "nosetests" "khmer" "--attr"
                                 "!known_failing")))))))))
    (native-inputs
     `(("seqan" ,seqan)
       ("python-nose" ,python-nose)))
    (inputs
     `(("zlib" ,zlib)
       ("bzip2" ,bzip2)
       ("python-screed" ,python-screed)
       ("python-bz2file" ,python-bz2file)
       ;; Tests fail when gcc-5 is used for compilation.  Use gcc-4.9 at least
       ;; until the next version of khmer (likely 2.1) is released.
       ("gcc" ,gcc-4.9)))
    (home-page "https://khmer.readthedocs.org/")
    (synopsis "K-mer counting, filtering and graph traversal library")
    (description "The khmer software is a set of command-line tools for
working with DNA shotgun sequencing data from genomes, transcriptomes,
metagenomes and single cells.  Khmer can make de novo assemblies faster, and
sometimes better.  Khmer can also identify and fix problems with shotgun
data.")
    ;; When building on i686, armhf and mips64el, we get the following error:
    ;; error: ['khmer', 'khmer.tests', 'oxli'] require 64-bit operating system
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (license license:bsd-3)))

(define-public kaiju
  (package
    (name "kaiju")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/bioinformatics-centre/kaiju/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0afbfalfw9y39bkwnqjrh9bghs118ws1pzj5h8l0nblgn3mbjdks"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'move-to-src-dir
           (lambda _ (chdir "src") #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (chdir "..")
               (copy-recursively "bin" bin)
               (copy-recursively "util" bin))
             #t)))))
    (inputs
     `(("perl" ,perl)))
    (home-page "http://kaiju.binf.ku.dk/")
    (synopsis "Fast and sensitive taxonomic classification for metagenomics")
    (description "Kaiju is a program for sensitive taxonomic classification
of high-throughput sequencing reads from metagenomic whole genome sequencing
experiments.")
    (license license:gpl3+)))

(define-public macs
  (package
    (name "macs")
    (version "2.1.0.20151222")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "MACS2" version))
              (sha256
               (base32
                "1r2hcz6irhcq7lwbafjks98jbn34hv05avgbdjnp6w6mlfjkf8x5"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; only compatible with Python 2.7
       #:tests? #f)) ; no test target
    (inputs
     `(("python-numpy" ,python2-numpy)))
    (home-page "https://github.com/taoliu/MACS/")
    (synopsis "Model based analysis for ChIP-Seq data")
    (description
     "MACS is an implementation of a ChIP-Seq analysis algorithm for
identifying transcript factor binding sites named Model-based Analysis of
ChIP-Seq (MACS).  MACS captures the influence of genome complexity to evaluate
the significance of enriched ChIP regions and it improves the spatial
resolution of binding sites through combining the information of both
sequencing tag position and orientation.")
    (license license:bsd-3)))

(define-public mafft
  (package
    (name "mafft")
    (version "7.313")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://mafft.cbrc.jp/alignment/software/mafft-" version
                    "-without-extensions-src.tgz"))
              (file-name (string-append name "-" version ".tgz"))
              (sha256
               (base32
                "0r83qmg2if8mi6jyx3xdf8ar2gcxl7r9nmj98jr7lxym97v61a2k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no automated tests, though there are tests in the read me
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)
                            (string-append "BINDIR="
                                           (string-append out "/bin"))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "core") #t))
         (add-after 'enter-dir 'patch-makefile
           (lambda _
             ;; on advice from the MAFFT authors, there is no need to
             ;; distribute mafft-profile, mafft-distance, or
             ;; mafft-homologs.rb as they are too "specialised".
             (substitute* "Makefile"
               ;; remove mafft-homologs.rb from SCRIPTS
               (("^SCRIPTS = mafft mafft-homologs.rb")
                "SCRIPTS = mafft")
               ;; remove mafft-homologs from MANPAGES
               (("^MANPAGES = mafft.1 mafft-homologs.1")
                "MANPAGES = mafft.1")
               ;; remove mafft-distance from PROGS
               (("^PROGS = dvtditr dndfast7 dndblast sextet5 mafft-distance")
                "PROGS = dvtditr dndfast7 dndblast sextet5")
               ;; remove mafft-profile from PROGS
               (("splittbfast disttbfast tbfast mafft-profile 2cl mccaskillwrap")
                "splittbfast disttbfast tbfast f2cl mccaskillwrap")
               (("^rm -f mafft-profile mafft-profile.exe") "#")
               (("^rm -f mafft-distance mafft-distance.exe") ")#")
               ;; do not install MAN pages in libexec folder
               (("^\t\\$\\(INSTALL\\) -m 644 \\$\\(MANPAGES\\) \
\\$\\(DESTDIR\\)\\$\\(LIBDIR\\)") "#"))
             #t))
         (add-after 'enter-dir 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("pairash.c"
                            "mafft.tmpl")
               (("perl") (which "perl"))
               (("([\"`| ])awk" _ prefix)
                (string-append prefix (which "awk")))
               (("grep") (which "grep")))
             #t))
         (delete 'configure)
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (path (string-append
                           (assoc-ref %build-inputs "coreutils") "/bin:")))
               (for-each (lambda (file)
                           (wrap-program file
                             `("PATH" ":" prefix (,path))))
                         (find-files bin)))
             #t)))))
    (inputs
     `(("perl" ,perl)
       ("ruby" ,ruby)
       ("gawk" ,gawk)
       ("grep" ,grep)
       ("coreutils" ,coreutils)))
    (home-page "http://mafft.cbrc.jp/alignment/software/")
    (synopsis "Multiple sequence alignment program")
    (description
     "MAFFT offers a range of multiple alignment methods for nucleotide and
protein sequences.  For instance, it offers L-INS-i (accurate; for alignment
of <~200 sequences) and FFT-NS-2 (fast; for alignment of <~30,000
sequences).")
    (license (license:non-copyleft
              "http://mafft.cbrc.jp/alignment/software/license.txt"
              "BSD-3 with different formatting"))))

(define-public mash
  (package
    (name "mash")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/marbl/mash/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08znbvqq5xknfhmpp3wcj574zvi4p7i8zifi67c9qw9a6ikp42fj"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled kseq.
               ;; TODO: Also delete bundled murmurhash and open bloom filter.
               '(delete-file "src/mash/kseq.h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests.
       #:configure-flags
       (list
        (string-append "--with-capnp=" (assoc-ref %build-inputs "capnproto"))
        (string-append "--with-gsl=" (assoc-ref %build-inputs "gsl")))
       #:make-flags (list "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-includes
           (lambda _
             (substitute* '("src/mash/Sketch.cpp" "src/mash/CommandFind.cpp")
               (("^#include \"kseq\\.h\"")
                "#include \"htslib/kseq.h\""))
             #t))
         (add-after 'fix-includes 'autoconf
           (lambda _ (zero? (system* "autoconf")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ;; Capnproto and htslib are statically embedded in the final
       ;; application. Therefore we also list their licenses, below.
       ("capnproto" ,capnproto)
       ("htslib" ,htslib)))
    (inputs
     `(("gsl" ,gsl)
       ("zlib" ,zlib)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://mash.readthedocs.io")
    (synopsis "Fast genome and metagenome distance estimation using MinHash")
    (description "Mash is a fast sequence distance estimator that uses the
MinHash algorithm and is designed to work with genomes and metagenomes in the
form of assemblies or reads.")
    (license (list license:bsd-3          ; Mash
                   license:expat          ; HTSlib and capnproto
                   license:public-domain  ; MurmurHash 3
                   license:cpl1.0))))     ; Open Bloom Filter

(define-public metabat
  (package
    (name "metabat")
    (version "2.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bitbucket.org/berkeleylab/metabat/get/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1hmvdalz3zj5sqqklg0l4npjdv37cv2hsdi1al9iby2ndxjs1b73"))
       (patches (search-patches "metabat-fix-compilation.patch"))))
    (build-system scons-build-system)
    (arguments
     `(#:scons ,scons-python2
       #:scons-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "BOOST_ROOT=" (assoc-ref %build-inputs "boost")))
       #:tests? #f ;; Tests are run during the build phase.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-includes
           (lambda _
             (substitute* "src/BamUtils.h"
               (("^#include \"bam/bam\\.h\"")
                "#include \"samtools/bam.h\"")
               (("^#include \"bam/sam\\.h\"")
                "#include \"samtools/sam.h\""))
             (substitute* "src/KseqReader.h"
               (("^#include \"bam/kseq\\.h\"")
                "#include \"htslib/kseq.h\""))
             #t))
         (add-after 'unpack 'fix-scons
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "SConstruct"
               (("^htslib_dir += 'samtools'")
                (string-append "htslib_dir = '"
                               (assoc-ref inputs "htslib")
                               "'"))
               (("^samtools_dir = 'samtools'")
                (string-append "samtools_dir = '"
                               (assoc-ref inputs "samtools")
                               "'"))
               (("^findStaticOrShared\\('bam', hts_lib")
                (string-append "findStaticOrShared('bam', '"
                               (assoc-ref inputs "samtools")
                               "/lib'"))
               ;; Do not distribute README.
               (("^env\\.Install\\(idir_prefix, 'README\\.md'\\)") ""))
             #t)))))
    (inputs
     `(("zlib" ,zlib)
       ("perl" ,perl)
       ("samtools" ,samtools)
       ("htslib" ,htslib)
       ("boost" ,boost)))
    (home-page "https://bitbucket.org/berkeleylab/metabat")
    (synopsis
     "Reconstruction of single genomes from complex microbial communities")
    (description
     "Grouping large genomic fragments assembled from shotgun metagenomic
sequences to deconvolute complex microbial communities, or metagenome binning,
enables the study of individual organisms and their interactions.  MetaBAT is
an automated metagenome binning software, which integrates empirical
probabilistic distances of genome abundance and tetranucleotide frequency.")
    ;; The source code contains inline assembly.
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license (license:non-copyleft "file://license.txt"
                                   "See license.txt in the distribution."))))

(define-public minced
  (package
    (name "minced")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ctSkennerton/minced/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wxmlsapxfpxfd3ps9636h7i2xy6la8i42mwh0j2lsky63h63jp1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'check 'fix-test
           (lambda _
             ;; Fix test for latest version.
             (substitute* "t/Aquifex_aeolicus_VF5.expected"
               (("minced:0.1.6") "minced:0.2.0"))
             #t))
         (replace 'install ; No install target.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (wrapper (string-append bin "/minced")))
               ;; Minced comes with a wrapper script that tries to figure out where
               ;; it is located before running the JAR. Since these paths are known
               ;; to us, we build our own wrapper to avoid coreutils dependency.
               (install-file "minced.jar" bin)
               (with-output-to-file wrapper
                 (lambda _
                   (display
                    (string-append
                     "#!" (assoc-ref inputs "bash") "/bin/sh\n\n"
                     (assoc-ref inputs "jre") "/bin/java -jar "
                     bin "/minced.jar \"$@\"\n"))))
               (chmod wrapper #o555)))))))
    (native-inputs
     `(("jdk" ,icedtea "jdk")))
    (inputs
     `(("bash" ,bash)
       ("jre" ,icedtea "out")))
    (home-page "https://github.com/ctSkennerton/minced")
    (synopsis "Mining CRISPRs in Environmental Datasets")
    (description
     "MinCED is a program to find Clustered Regularly Interspaced Short
Palindromic Repeats (CRISPRs) in DNA sequences.  It can be used for
unassembled metagenomic reads, but is mainly designed for full genomes and
assembled metagenomic sequence.")
    (license license:gpl3+)))

(define-public miso
  (package
    (name "miso")
    (version "0.5.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "misopy" version))
              (sha256
               (base32
                "1z3x0vd8ma7pdrnywj7i3kgwl89sdkwrrn62zl7r5calqaq2hyip"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "setup.py"
                  ;; Use setuptools, or else the executables are not
                  ;; installed.
                  (("distutils.core") "setuptools")
                  ;; use "gcc" instead of "cc" for compilation
                  (("^defines")
                   "cc.set_executables(
compiler='gcc',
compiler_so='gcc',
linker_exe='gcc',
linker_so='gcc -shared'); defines")))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; only Python 2 is supported
       #:tests? #f)) ; no "test" target
    (inputs
     `(("samtools" ,samtools)
       ("python-numpy" ,python2-numpy)
       ("python-pysam" ,python2-pysam)
       ("python-scipy" ,python2-scipy)
       ("python-matplotlib" ,python2-matplotlib)))
    (native-inputs
     `(("python-mock" ,python2-mock)   ;for tests
       ("python-pytz" ,python2-pytz))) ;for tests
    (home-page "http://genes.mit.edu/burgelab/miso/index.html")
    (synopsis "Mixture of Isoforms model for RNA-Seq isoform quantitation")
    (description
     "MISO (Mixture-of-Isoforms) is a probabilistic framework that quantitates
the expression level of alternatively spliced genes from RNA-Seq data, and
identifies differentially regulated isoforms or exons across samples.  By
modeling the generative process by which reads are produced from isoforms in
RNA-Seq, the MISO model uses Bayesian inference to compute the probability
that a read originated from a particular isoform.")
    (license license:gpl2)))

(define-public muscle
  (package
    (name "muscle")
    (version "3.8.1551")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append
                    "http://www.drive5.com/muscle/muscle_src_"
                    version ".tar.gz"))
              (sha256
               (base32
                "0bj8kj7sdizy3987zx6w7axihk40fk8rn76mpbqqjcnd64i5a367"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "LDLIBS = -lm")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           ;; There are no tests, so just test if it runs.
           (lambda _ (zero? (system* "./muscle" "-version"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "muscle" bin)))))))
    (home-page "http://www.drive5.com/muscle")
    (synopsis "Multiple sequence alignment program")
    (description
     "MUSCLE aims to be a fast and accurate multiple sequence alignment
program for nucleotide and protein sequences.")
    ;; License information found in 'muscle -h' and usage.cpp.
    (license license:public-domain)))

(define-public newick-utils
  ;; There are no recent releases so we package from git.
  (let ((commit "da121155a977197cab9fbb15953ca1b40b11eb87"))
    (package
      (name "newick-utils")
      (version (string-append "1.6-1." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tjunier/newick_utils.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1hkw21rq1mwf7xp0rmbb2gqc0i6p11108m69i7mr7xcjl268pxnb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoconf
           (lambda _ (zero? (system* "autoreconf" "-vif")))))))
    (inputs
     ;; XXX: TODO: Enable Lua and Guile bindings.
     ;; https://github.com/tjunier/newick_utils/issues/13
     `(("libxml2" ,libxml2)
       ("flex" ,flex)
       ("bison" ,bison)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (synopsis "Programs for working with newick format phylogenetic trees")
    (description
     "Newick-utils is a suite of utilities for processing phylogenetic trees
in Newick format.  Functions include re-rooting, extracting subtrees,
trimming, pruning, condensing, drawing (ASCII graphics or SVG).")
    (home-page "https://github.com/tjunier/newick_utils")
    (license license:bsd-3))))

(define-public orfm
  (package
    (name "orfm")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/wwood/OrfM/releases/download/v"
                    version "/orfm-" version ".tar.gz"))
              (sha256
               (base32
                "16iigyr2gd8x0imzkk1dr3k5xsds9bpmwg31ayvjg0f4pir9rwqr"))))
    (build-system gnu-build-system)
    (inputs `(("zlib" ,zlib)))
    (native-inputs
     `(("ruby-bio-commandeer" ,ruby-bio-commandeer)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby" ,ruby)))
    (synopsis "Simple and not slow open reading frame (ORF) caller")
    (description
     "An ORF caller finds stretches of DNA that, when translated, are not
interrupted by stop codons.  OrfM finds and prints these ORFs.")
    (home-page "https://github.com/wwood/OrfM")
    (license license:lgpl3+)))

(define-public pplacer
  (let ((commit "g807f6f3"))
    (package
      (name "pplacer")
      ;; The commit should be updated with each version change.
      (version "1.1.alpha19")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/matsen/pplacer/archive/v"
                             version ".tar.gz"))
         (file-name (string-append name "-" version ".tar.gz"))
         (sha256
          (base32 "0z1lnd2s8sh6kpzg106wzbh2szw7h0hvq8syd5a6wv4rmyyz6x0f"))))
      (build-system ocaml-build-system)
      (arguments
       `(#:ocaml ,ocaml-4.01
         #:findlib ,ocaml4.01-findlib
         #:modules ((guix build ocaml-build-system)
                    (guix build utils)
                    (ice-9 ftw))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'replace-bundled-cddlib
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((cddlib-src (assoc-ref inputs "cddlib-src"))
                      (local-dir "cddlib_guix"))
                 (mkdir local-dir)
                 (with-directory-excursion local-dir
                   (system* "tar" "xvf" cddlib-src))
                 (let ((cddlib-src-folder
                        (string-append local-dir "/"
                                       (list-ref (scandir local-dir) 2)
                                       "/lib-src")))
                   (for-each
                    (lambda (file)
                      (copy-file file
                                 (string-append "cdd_src/" (basename file))))
                    (find-files cddlib-src-folder ".*[ch]$")))
                 #t)))
           (add-after 'unpack 'fix-makefile
             (lambda _
               ;; Remove system calls to 'git'.
               (substitute* "Makefile"
                 (("^DESCRIPT:=pplacer-.*")
                  (string-append
                   "DESCRIPT:=pplacer-$(shell uname)-v" ,version "\n")))
               (substitute* "myocamlbuild.ml"
                 (("git describe --tags --long .*\\\" with")
                  (string-append
                   "echo -n v" ,version "-" ,commit "\" with")))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (copy-recursively "bin" bin))
               #t)))))
      (native-inputs
       `(("zlib" ,zlib)
         ("gsl" ,gsl)
         ("ocaml-ounit" ,ocaml4.01-ounit)
         ("ocaml-batteries" ,ocaml4.01-batteries)
         ("ocaml-camlzip" ,ocaml4.01-camlzip)
         ("ocaml-csv" ,ocaml4.01-csv)
         ("ocaml-sqlite3" ,ocaml4.01-sqlite3)
         ("ocaml-xmlm" ,ocaml4.01-xmlm)
         ("ocaml-mcl" ,ocaml4.01-mcl)
         ("ocaml-gsl" ,ocaml4.01-gsl)
         ("cddlib-src" ,(package-source cddlib))))
      (propagated-inputs
       `(("pplacer-scripts" ,pplacer-scripts)))
      (synopsis "Phylogenetic placement of biological sequences")
      (description
       "Pplacer places query sequences on a fixed reference phylogenetic tree
to maximize phylogenetic likelihood or posterior probability according to a
reference alignment.  Pplacer is designed to be fast, to give useful
information about uncertainty, and to offer advanced visualization and
downstream analysis.")
      (home-page "http://matsen.fhcrc.org/pplacer")
      (license license:gpl3))))

;; This package is installed alongside 'pplacer'.  It is a separate package so
;; that it can use the python-build-system for the scripts that are
;; distributed alongside the main OCaml binaries.
(define pplacer-scripts
  (package
    (inherit pplacer)
    (name "pplacer-scripts")
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-scripts-dir
           (lambda _ (chdir "scripts")))
         (replace 'check
           (lambda _
             (zero? (system* "python" "-m" "unittest" "discover" "-v"))))
         (add-after 'install 'wrap-executables
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (let ((path (string-append
                            (assoc-ref inputs "hmmer") "/bin:"
                            (assoc-ref inputs "infernal") "/bin")))
                 (display path)
                 (wrap-program (string-append bin "/refpkg_align.py")
                   `("PATH" ":" prefix (,path))))
               (let ((path (string-append
                            (assoc-ref inputs "hmmer") "/bin")))
                 (wrap-program (string-append bin "/hrefpkg_query.py")
                   `("PATH" ":" prefix (,path)))))
             #t)))))
    (inputs
     `(("infernal" ,infernal)
       ("hmmer" ,hmmer)))
    (propagated-inputs
     `(("python-biopython" ,python2-biopython)
       ("taxtastic" ,taxtastic)))
    (synopsis "Pplacer Python scripts")))

(define-public python2-pbcore
  (package
    (name "python2-pbcore")
    (version "1.2.10")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pbcore" version))
              (sha256
               (base32
                "1kjmv891d6qbpp4shhhvkl02ff4q5xlpnls2513sm2cjcrs52f1i"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2)) ; pbcore requires Python 2.7
    (propagated-inputs
     `(("python-cython" ,python2-cython)
       ("python-numpy" ,python2-numpy)
       ("python-pysam" ,python2-pysam)
       ("python-h5py" ,python2-h5py)))
    (native-inputs
     `(("python-nose" ,python2-nose)
       ("python-sphinx" ,python2-sphinx)
       ("python-pyxb" ,python2-pyxb)))
    (home-page "http://pacificbiosciences.github.io/pbcore/")
    (synopsis "Library for reading and writing PacBio data files")
    (description
     "The pbcore package provides Python APIs for interacting with PacBio data
files and writing bioinformatics applications.")
    (license license:bsd-3)))

(define-public python2-warpedlmm
  (package
    (name "python2-warpedlmm")
    (version "0.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/W/WarpedLMM/WarpedLMM-"
             version ".zip"))
       (sha256
        (base32
         "1agfz6zqa8nc6cw47yh0s3y14gkpa9wqazwcj7mwwj3ffnw39p3j"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))  ; requires Python 2.7
    (propagated-inputs
     `(("python-scipy" ,python2-scipy)
       ("python-numpy" ,python2-numpy)
       ("python-matplotlib" ,python2-matplotlib)
       ("python-fastlmm" ,python2-fastlmm)
       ("python-pandas" ,python2-pandas)
       ("python-pysnptools" ,python2-pysnptools)))
    (native-inputs
     `(("python-mock" ,python2-mock)
       ("python-nose" ,python2-nose)
       ("unzip" ,unzip)))
    (home-page "https://github.com/PMBio/warpedLMM")
    (synopsis "Implementation of warped linear mixed models")
    (description
     "WarpedLMM is a Python implementation of the warped linear mixed model,
which automatically learns an optimal warping function (or transformation) for
the phenotype as it models the data.")
    (license license:asl2.0)))

(define-public pbtranscript-tofu
  (let ((commit "8f5467fe6a4472bcfb4226c8720993c8507adfe4"))
    (package
      (name "pbtranscript-tofu")
      (version (string-append "2.2.3." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/PacificBiosciences/cDNA_primer.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1lgnpi35ihay42qx0b6yl3kkgra723i413j33kvs0kvs61h82w0f"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; remove bundled Cython sources
                    (delete-file "pbtranscript-tofu/pbtranscript/Cython-0.20.1.tar.gz")
                    #t))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2
         ;; FIXME: Tests fail with "No such file or directory:
         ;; pbtools/pbtranscript/modified_bx_intervals/intersection_unique.so"
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-directory
            (lambda _
              (chdir "pbtranscript-tofu/pbtranscript/")
              #t))
           ;; With setuptools version 18.0 and later this setup.py hack causes
           ;; a build error, so we disable it.
           (add-after 'enter-directory 'patch-setuppy
            (lambda _
              (substitute* "setup.py"
                (("if 'setuptools.extension' in sys.modules:")
                 "if False:"))
              #t)))))
      (inputs
       `(("python-numpy" ,python2-numpy)
         ("python-bx-python" ,python2-bx-python)
         ("python-networkx" ,python2-networkx)
         ("python-scipy" ,python2-scipy)
         ("python-pbcore" ,python2-pbcore)
         ("python-h5py" ,python2-h5py)))
      (native-inputs
       `(("python-cython" ,python2-cython)
         ("python-nose" ,python2-nose)))
      (home-page "https://github.com/PacificBiosciences/cDNA_primer")
      (synopsis "Analyze transcriptome data generated with the Iso-Seq protocol")
      (description
       "pbtranscript-tofu contains scripts to analyze transcriptome data
generated using the PacBio Iso-Seq protocol.")
      (license license:bsd-3))))

(define-public prank
  (package
    (name "prank")
    (version "150803")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://wasabiapp.org/download/prank/prank.source."
                    version ".tgz"))
              (sha256
               (base32
                "0am4z94fs3w2n5xpfls9zda61vq7qqz4q2i7b9hlsxz5q4j3kfm4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-src-dir
            (lambda _
              (chdir "src")
              #t))
         (add-after 'unpack 'remove-m64-flag
           ;; Prank will build with the correct 'bit-ness' without this flag
           ;; and this allows building on 32-bit machines.
           (lambda _ (substitute* "src/Makefile"
                                  (("-m64") ""))
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1"))
                    (path (string-append
                           (assoc-ref %build-inputs "mafft") "/bin:"
                           (assoc-ref %build-inputs "exonerate") "/bin:"
                           (assoc-ref %build-inputs "bppsuite") "/bin")))
               (install-file "prank" bin)
               (wrap-program (string-append bin "/prank")
                 `("PATH" ":" prefix (,path)))
               (install-file "prank.1" man))
             #t)))))
    (inputs
     `(("mafft" ,mafft)
       ("exonerate" ,exonerate)
       ("bppsuite" ,bppsuite)))
    (home-page "http://wasabiapp.org/software/prank/")
    (synopsis "Probabilistic multiple sequence alignment program")
    (description
     "PRANK is a probabilistic multiple sequence alignment program for DNA,
codon and amino-acid sequences.  It is based on a novel algorithm that treats
insertions correctly and avoids over-estimation of the number of deletion
events.  In addition, PRANK borrows ideas from maximum likelihood methods used
in phylogenetics and correctly takes into account the evolutionary distances
between sequences.  Lastly, PRANK allows for defining a potential structure
for sequences to be aligned and then, simultaneously with the alignment,
predicts the locations of structural units in the sequences.")
    (license license:gpl2+)))

(define-public proteinortho
  (package
    (name "proteinortho")
    (version "5.16b")
    (source
     (origin
      (method url-fetch)
      (uri
       (string-append
        "http://www.bioinf.uni-leipzig.de/Software/proteinortho/proteinortho_v"
        version "_src.tar.gz"))
      (sha256
       (base32
        "1wl0dawpssqwfjvr651r4wlww8hhjin8nba6xh71ks7sbypx886j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; There is no configure script, so we modify the Makefile directly.
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("INSTALLDIR=.*")
                (string-append
                 "INSTALLDIR=" (assoc-ref outputs "out") "/bin\n")))
             #t))
         (add-before 'install 'make-install-directory
           ;; The install directory is not created during 'make install'.
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
             #t))
         (add-after 'install 'wrap-programs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((path (getenv "PATH"))
                    (out (assoc-ref outputs "out"))
                    (binary (string-append out "/bin/proteinortho5.pl")))
               (wrap-program binary `("PATH" ":" prefix (,path))))
             #t)))))
    (inputs
     `(("perl" ,perl)
       ("python" ,python-2)
       ("blast+" ,blast+)))
    (home-page "http://www.bioinf.uni-leipzig.de/Software/proteinortho")
    (synopsis "Detect orthologous genes across species")
    (description
     "Proteinortho is a tool to detect orthologous genes across different
species.  For doing so, it compares similarities of given gene sequences and
clusters them to find significant groups.  The algorithm was designed to handle
large-scale data and can be applied to hundreds of species at once.")
    (license license:gpl2+)))

(define-public pyicoteo
  (package
    (name "pyicoteo")
    (version "2.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bitbucket.org/regulatorygenomicsupf/"
                           "pyicoteo/get/v" version ".tar.bz2"))
       (file-name (string-append name "-" version ".tar.bz2"))
       (sha256
        (base32
         "0d6087f29xp8wxwlj111c3sylli98n0l8ry58c51ixzq0zfm50wa"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; does not work with Python 3
       #:tests? #f))      ; there are no tests
    (inputs
     `(("python2-matplotlib" ,python2-matplotlib)))
    (home-page "https://bitbucket.org/regulatorygenomicsupf/pyicoteo")
    (synopsis "Analyze high-throughput genetic sequencing data")
    (description
     "Pyicoteo is a suite of tools for the analysis of high-throughput genetic
sequencing data.  It works with genomic coordinates.  There are currently six
different command-line tools:

@enumerate
@item pyicoregion: for generating exploratory regions automatically;
@item pyicoenrich: for differential enrichment between two conditions;
@item pyicoclip: for calling CLIP-Seq peaks without a control;
@item pyicos: for genomic coordinates manipulation;
@item pyicoller: for peak calling on punctuated ChIP-Seq;
@item pyicount: to count how many reads from N experiment files overlap in a
  region file;
@item pyicotrocol: to combine operations from pyicoteo.
@end enumerate\n")
    (license license:gpl3+)))

(define-public prodigal
  (package
    (name "prodigal")
    (version "2.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/hyattpd/Prodigal/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17srxkqd3jc77xk15pfbgg1a9xahqg7337w95mrsia7mpza4l2c9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no check target
       #:make-flags (list (string-append "INSTALLDIR="
                                         (assoc-ref %outputs "out")
                                         "/bin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "http://prodigal.ornl.gov")
    (synopsis "Protein-coding gene prediction for Archaea and Bacteria")
    (description
     "Prodigal runs smoothly on finished genomes, draft genomes, and
metagenomes, providing gene predictions in GFF3, Genbank, or Sequin table
format.  It runs quickly, in an unsupervised fashion, handles gaps, handles
partial genes, and identifies translation initiation sites.")
    (license license:gpl3+)))

(define-public roary
  (package
    (name "roary")
    (version "3.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AJ/AJPAGE/Bio-Roary-"
             version ".tar.gz"))
       (sha256
        (base32
         "10lw78x1xzvn7xzvnmh4bm3cak3ah5cssapl0yidvhaj1f44h29i"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'check
           (lambda _
             ;; The tests are not run by default, so we run each test file
             ;; directly.
             (setenv "PATH" (string-append (getcwd) "/bin" ":"
                                           (getenv "PATH")))
             (setenv "PERL5LIB" (string-append (getcwd) "/lib" ":"
                                               (getenv "PERL5LIB")))
             (zero? (length (filter (lambda (file)
                                      (display file)(display "\n")
                                      (not (zero? (system* "perl" file))))
                                    (find-files "t" ".*\\.t$"))))))
         (replace 'install
           ;; There is no 'install' target in the Makefile.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (perl (string-append out "/lib/perl5/site_perl"))
                    (roary-plots "contrib/roary_plots"))
               (mkdir-p bin)
               (mkdir-p perl)
               (copy-recursively "bin" bin)
               (copy-recursively "lib" perl)
               #t)))
         (add-after 'install 'wrap-programs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (perl5lib (getenv "PERL5LIB"))
                    (path (getenv "PATH")))
               (for-each (lambda (prog)
                           (let ((binary (string-append out "/" prog)))
                             (wrap-program binary
                               `("PERL5LIB" ":" prefix
                                 (,(string-append perl5lib ":" out
                                                  "/lib/perl5/site_perl"))))
                             (wrap-program binary
                               `("PATH" ":" prefix
                                 (,(string-append path ":" out "/bin"))))))
                         (find-files "bin" ".*[^R]$"))
               (let ((file
                      (string-append out "/bin/roary-create_pan_genome_plots.R"))
                     (r-site-lib (getenv "R_LIBS_SITE"))
                     (coreutils-path
                      (string-append (assoc-ref inputs "coreutils") "/bin")))
                 (wrap-program file
                   `("R_LIBS_SITE" ":" prefix
                     (,(string-append r-site-lib ":" out "/site-library/"))))
                 (wrap-program file
                   `("PATH" ":" prefix
                     (,(string-append coreutils-path ":" out "/bin"))))))
             #t)))))
    (native-inputs
     `(("perl-env-path" ,perl-env-path)
       ("perl-test-files" ,perl-test-files)
       ("perl-test-most" ,perl-test-most)
       ("perl-test-output" ,perl-test-output)))
    (inputs
     `(("perl-array-utils" ,perl-array-utils)
       ("bioperl" ,bioperl-minimal)
       ("perl-digest-md5-file" ,perl-digest-md5-file)
       ("perl-exception-class" ,perl-exception-class)
       ("perl-file-find-rule" ,perl-file-find-rule)
       ("perl-file-grep" ,perl-file-grep)
       ("perl-file-slurper" ,perl-file-slurper)
       ("perl-file-which" ,perl-file-which)
       ("perl-graph" ,perl-graph)
       ("perl-graph-readwrite" ,perl-graph-readwrite)
       ("perl-log-log4perl" ,perl-log-log4perl)
       ("perl-moose" ,perl-moose)
       ("perl-perlio-utf8_strict" ,perl-perlio-utf8_strict)
       ("perl-text-csv" ,perl-text-csv)
       ("bedtools" ,bedtools)
       ("cd-hit" ,cd-hit)
       ("blast+" ,blast+)
       ("mcl" ,mcl)
       ("parallel" ,parallel)
       ("prank" ,prank)
       ("mafft" ,mafft)
       ("fasttree" ,fasttree)
       ("grep" ,grep)
       ("sed" ,sed)
       ("gawk" ,gawk)
       ("r-minimal" ,r-minimal)
       ("r-ggplot2" ,r-ggplot2)
       ("coreutils" ,coreutils)))
    (home-page "http://sanger-pathogens.github.io/Roary")
    (synopsis "High speed stand-alone pan genome pipeline")
    (description
     "Roary is a high speed stand alone pan genome pipeline, which takes
annotated assemblies in GFF3 format (produced by the Prokka program) and
calculates the pan genome.  Using a standard desktop PC, it can analyse
datasets with thousands of samples, without compromising the quality of the
results.  128 samples can be analysed in under 1 hour using 1 GB of RAM and a
single processor.  Roary is not intended for metagenomics or for comparing
extremely diverse sets of genomes.")
    (license license:gpl3)))

(define-public raxml
  (package
    (name "raxml")
    (version "8.2.10")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/stamatak/standard-RAxML/archive/v"
         version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13s7aspfdcfr6asynwdg1x6vznys6pzap5f8wsffbnnwpkkg9ya8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       ;; Use 'standard' Makefile rather than SSE or AVX ones.
       #:make-flags (list "-f" "Makefile.HYBRID.gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (executable "raxmlHPC-HYBRID"))
               (install-file executable bin)
               (symlink (string-append bin "/" executable) "raxml"))
             #t)))))
    (inputs
     `(("openmpi" ,openmpi)))
    (home-page "http://sco.h-its.org/exelixis/web/software/raxml/index.html")
    (synopsis "Randomized Axelerated Maximum Likelihood phylogenetic trees")
    (description
     "RAxML is a tool for phylogenetic analysis and post-analysis of large
phylogenies.")
    ;; The source includes x86 specific code
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:gpl2+)))

(define-public rsem
  (package
    (name "rsem")
    (version "1.2.20")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "http://deweylab.biostat.wisc.edu/rsem/src/rsem-"
                       version ".tar.gz"))
       (sha256
        (base32 "0nzdc0j0hjllhsd5f2xli95dafm3nawskigs140xzvjk67xh0r9q"))
       (patches (search-patches "rsem-makefile.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; remove bundled copy of boost
           (delete-file-recursively "boost")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; No "configure" script.
         ;; Do not build bundled samtools library.
         (replace 'configure
                  (lambda _
                    (substitute* "Makefile"
                      (("^all : sam/libbam.a") "all : "))
                    #t))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (string-append (assoc-ref outputs "out")))
                           (bin (string-append out "/bin/"))
                           (perl (string-append out "/lib/perl5/site_perl")))
                      (mkdir-p bin)
                      (mkdir-p perl)
                      (for-each (lambda (file)
                                  (install-file file bin))
                                (find-files "." "rsem-.*"))
                      (install-file "rsem_perl_utils.pm" perl))
                    #t))
         (add-after
          'install 'wrap-program
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (for-each (lambda (prog)
                          (wrap-program (string-append out "/bin/" prog)
                            `("PERL5LIB" ":" prefix
                              (,(string-append out "/lib/perl5/site_perl")))))
                        '("rsem-plot-transcript-wiggles"
                          "rsem-calculate-expression"
                          "rsem-generate-ngvector"
                          "rsem-run-ebseq"
                          "rsem-prepare-reference")))
            #t)))))
    (inputs
     `(("boost" ,boost)
       ("ncurses" ,ncurses)
       ("r-minimal" ,r-minimal)
       ("perl" ,perl)
       ("samtools" ,samtools-0.1)
       ("zlib" ,zlib)))
    (home-page "http://deweylab.biostat.wisc.edu/rsem/")
    (synopsis "Estimate gene expression levels from RNA-Seq data")
    (description
     "RSEM is a software package for estimating gene and isoform expression
levels from RNA-Seq data.  The RSEM package provides a user-friendly
interface, supports threads for parallel computation of the EM algorithm,
single-end and paired-end read data, quality scores, variable-length reads and
RSPD estimation.  In addition, it provides posterior mean and 95% credibility
interval estimates for expression levels.  For visualization, it can generate
BAM and Wiggle files in both transcript-coordinate and genomic-coordinate.")
    (license license:gpl3+)))

(define-public rseqc
  (package
    (name "rseqc")
    (version "2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/rseqc/"
                       "RSeQC-" version ".tar.gz"))
       (sha256
        (base32 "15ly0254yi032qzkdplg00q144qfdsd986gh62829rl5bkxhj330"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; remove bundled copy of pysam
           (delete-file-recursively "lib/pysam")
           (substitute* "setup.py"
             ;; remove dependency on outdated "distribute" module
             (("^from distribute_setup import use_setuptools") "")
             (("^use_setuptools\\(\\)") "")
             ;; do not use bundled copy of pysam
             (("^have_pysam = False") "have_pysam = True"))))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (inputs
     `(("python-cython" ,python2-cython)
       ("python-pysam" ,python2-pysam)
       ("python-numpy" ,python2-numpy)
       ("zlib" ,zlib)))
    (native-inputs
     `(("python-nose" ,python2-nose)))
    (home-page "http://rseqc.sourceforge.net/")
    (synopsis "RNA-seq quality control package")
    (description
     "RSeQC provides a number of modules that can comprehensively evaluate
high throughput sequence data, especially RNA-seq data.  Some basic modules
inspect sequence quality, nucleotide composition bias, PCR bias and GC bias,
while RNA-seq specific modules evaluate sequencing saturation, mapped reads
distribution, coverage uniformity, strand specificity, etc.")
    (license license:gpl3+)))

(define-public seek
  ;; There are no release tarballs.  According to the installation
  ;; instructions at http://seek.princeton.edu/installation.jsp, the latest
  ;; stable release is identified by this changeset ID.
  (let ((changeset "2329130")
        (revision "1"))
    (package
      (name "seek")
      (version (string-append "0-" revision "." changeset))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://bitbucket.org/libsleipnir/sleipnir")
                      (changeset changeset)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0qrvilwh18dpbhkf92qvxbmay0j75ra3jg2wrhz67gf538zzphsx"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((srfi srfi-1)
                    (guix build gnu-build-system)
                    (guix build utils))
         #:phases
         (let ((dirs '("SeekMiner"
                       "SeekEvaluator"
                       "SeekPrep"
                       "Distancer"
                       "Data2DB"
                       "PCL2Bin")))
           (modify-phases %standard-phases
             (add-before 'configure 'bootstrap
               (lambda _
                 (zero? (system* "bash" "gen_auto"))))
             (add-after 'build 'build-additional-tools
               (lambda* (#:key make-flags #:allow-other-keys)
                 (every (lambda (dir)
                          (with-directory-excursion (string-append "tools/" dir)
                            (zero? (apply system* "make" make-flags))))
                        dirs)))
             (add-after 'install 'install-additional-tools
               (lambda* (#:key make-flags #:allow-other-keys)
                 (fold (lambda (dir result)
                         (with-directory-excursion (string-append "tools/" dir)
                           (and result
                                (zero? (apply system*
                                              `("make" ,@make-flags "install"))))))
                       #t dirs)))))))
      (inputs
       `(("gsl" ,gsl)
         ("boost" ,boost)
         ("libsvm" ,libsvm)
         ("readline" ,readline)
         ("gengetopt" ,gengetopt)
         ("log4cpp" ,log4cpp)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("perl" ,perl)))
      (home-page "http://seek.princeton.edu")
      (synopsis "Gene co-expression search engine")
      (description
       "SEEK is a computational gene co-expression search engine.  SEEK provides
biologists with a way to navigate the massive human expression compendium that
now contains thousands of expression datasets.  SEEK returns a robust ranking
of co-expressed genes in the biological area of interest defined by the user's
query genes.  It also prioritizes thousands of expression datasets according
to the user's query of interest.")
      (license license:cc-by3.0))))

(define-public samtools
  (package
    (name "samtools")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "18acyqysbxpydlc44lqv2hpp57l06bs9a3yqmcvjk8va2xrrdc77"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((ice-9 ftw)
                  (ice-9 regex)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:configure-flags (list "--with-ncurses" "--with-htslib=system")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "test/test.pl"
               ;; The test script calls out to /bin/bash
               (("/bin/bash") (which "bash")))
             #t))
         (add-after 'install 'install-library
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
               (install-file "libbam.a" lib)
               #t)))
         (add-after 'install 'install-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((include (string-append (assoc-ref outputs "out")
                                           "/include/samtools/")))
               (for-each (lambda (file)
                           (install-file file include))
                         (scandir "." (lambda (name) (string-match "\\.h$" name))))
               #t))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("htslib" ,htslib)
       ("ncurses" ,ncurses)
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

(define-public samtools-0.1
  ;; This is the most recent version of the 0.1 line of samtools.  The input
  ;; and output formats differ greatly from that used and produced by samtools
  ;; 1.x and is still used in many bioinformatics pipelines.
  (package (inherit samtools)
    (version "0.1.19")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32 "1m33xsfwz0s8qi45lylagfllqg7fphf4dr0780rsvw75av9wk06h"))))
    (arguments
     `(#:tests? #f ;no "check" target
       ,@(substitute-keyword-arguments (package-arguments samtools)
           ((#:make-flags flags)
            `(cons "LIBCURSES=-lncurses" ,flags))
           ((#:phases phases)
            `(modify-phases ,phases
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((bin (string-append
                               (assoc-ref outputs "out") "/bin")))
                     (mkdir-p bin)
                     (install-file "samtools" bin)
                     #t)))
               (delete 'patch-tests)
               (delete 'configure))))))))

(define-public mosaik
  (let ((commit "5c25216d3522d6a33e53875cd76a6d65001e4e67"))
    (package
      (name "mosaik")
      (version "2.2.30")
      (source (origin
                ;; There are no release tarballs nor tags.
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wanpinglee/MOSAIK.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "17gj3s07cm77r41z92awh0bim7w7q7fbn0sf5nkqmcm1vw052qgw"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:make-flags (list "CC=gcc")
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
                    (lambda _ (chdir "src") #t))
           (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((bin (string-append (assoc-ref outputs "out")
                                                "/bin")))
                        (mkdir-p bin)
                        (copy-recursively "../bin" bin)
                        #t))))))
      (inputs
       `(("perl" ,perl)
         ("zlib" ,zlib)))
      (supported-systems '("x86_64-linux"))
      (home-page "https://github.com/wanpinglee/MOSAIK")
      (synopsis "Map nucleotide sequence reads to reference genomes")
      (description
       "MOSAIK is a program for mapping second and third-generation sequencing
reads to a reference genome.  MOSAIK can align reads generated by all the
major sequencing technologies, including Illumina, Applied Biosystems SOLiD,
Roche 454, Ion Torrent and Pacific BioSciences SMRT.")
      ;; MOSAIK is released under the GPLv2+ with the exception of third-party
      ;; code released into the public domain:
      ;; 1. fastlz by Ariya Hidayat - http://www.fastlz.org/
      ;; 2. MD5 implementation - RSA Data Security, RFC 1321
      (license (list license:gpl2+ license:public-domain)))))

(define-public ngs-sdk
  (package
    (name "ngs-sdk")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ncbi/ngs/archive/"
                       version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wiyf4c6nm2j87pv015cbi0qny5byf3pbvcw3likifz5dl56ag40"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f ; not supported
       #:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Allow 'konfigure.perl' to find 'package.prl'.
               (setenv "PERL5LIB"
                       (string-append ".:" (getenv "PERL5LIB")))

               ;; The 'configure' script doesn't recognize things like
               ;; '--enable-fast-install'.
               (zero? (system* "./configure"
                               (string-append "--build-prefix=" (getcwd) "/build")
                               (string-append "--prefix=" out))))))
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "ngs-sdk") #t)))))
    (native-inputs `(("perl" ,perl)))
    ;; According to the test
    ;;   unless ($MARCH =~ /x86_64/i || $MARCH =~ /i?86/i)
    ;; in ngs-sdk/setup/konfigure.perl
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "https://github.com/ncbi/ngs")
    (synopsis "API for accessing Next Generation Sequencing data")
    (description
     "NGS is a domain-specific API for accessing reads, alignments and pileups
produced from Next Generation Sequencing.  The API itself is independent from
any particular back-end implementation, and supports use of multiple back-ends
simultaneously.")
    (license license:public-domain)))

(define-public java-ngs
  (package (inherit ngs-sdk)
    (name "java-ngs")
    (arguments
     `(,@(substitute-keyword-arguments
             `(#:modules ((guix build gnu-build-system)
                          (guix build utils)
                          (srfi srfi-1)
                          (srfi srfi-26))
                         ,@(package-arguments ngs-sdk))
           ((#:phases phases)
            `(modify-phases ,phases
               (replace 'enter-dir (lambda _ (chdir "ngs-java") #t)))))))
    (inputs
     `(("jdk" ,icedtea "jdk")
       ("ngs-sdk" ,ngs-sdk)))
    (synopsis "Java bindings for NGS SDK")))

(define-public ncbi-vdb
  (package
    (name "ncbi-vdb")
    (version "2.8.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ncbi/ncbi-vdb/archive/"
                       version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1acn4bv81mfl137qnbn9995mjjhwd36pm0b7qli1iw5skrxa9j8m"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f ; not supported
       #:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-perl-search-path
           (lambda _
             ;; Work around "dotless @INC" build failure.
             (setenv "PERL5LIB"
                     (string-append (getcwd) "/setup:"
                                    (getenv "PERL5LIB")))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Override include path for libmagic
               (substitute* "setup/package.prl"
                 (("name => 'magic', Include => '/usr/include'")
                  (string-append "name=> 'magic', Include => '"
                                 (assoc-ref inputs "libmagic")
                                 "/include" "'")))

               ;; Install kdf5 library (needed by sra-tools)
               (substitute* "build/Makefile.install"
                 (("LIBRARIES_TO_INSTALL =")
                  "LIBRARIES_TO_INSTALL = kdf5.$(VERSION_LIBX) kdf5.$(VERSION_SHLX)"))

               (substitute* "build/Makefile.env"
                 (("CFLAGS	=" prefix)
                  (string-append prefix "-msse2 ")))

               ;; Override search path for ngs-java
               (substitute* "setup/package.prl"
                 (("/usr/local/ngs/ngs-java")
                  (assoc-ref inputs "java-ngs")))

               ;; The 'configure' script doesn't recognize things like
               ;; '--enable-fast-install'.
               (zero? (system*
                       "./configure"
                       (string-append "--build-prefix=" (getcwd) "/build")
                       (string-append "--prefix=" (assoc-ref outputs "out"))
                       (string-append "--debug")
                       (string-append "--with-xml2-prefix="
                                      (assoc-ref inputs "libxml2"))
                       (string-append "--with-ngs-sdk-prefix="
                                      (assoc-ref inputs "ngs-sdk"))
                       (string-append "--with-hdf5-prefix="
                                      (assoc-ref inputs "hdf5")))))))
         (add-after 'install 'install-interfaces
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Install interface libraries.  On i686 the interface libraries
             ;; are installed to "linux/gcc/i386", so we need to use the Linux
             ;; architecture name ("i386") instead of the target system prefix
             ;; ("i686").
             (mkdir (string-append (assoc-ref outputs "out") "/ilib"))
             (copy-recursively (string-append "build/ncbi-vdb/linux/gcc/"
                                              ,(system->linux-architecture
                                                (or (%current-target-system)
                                                    (%current-system)))
                                              "/rel/ilib")
                               (string-append (assoc-ref outputs "out")
                                              "/ilib"))
             ;; Install interface headers
             (copy-recursively "interfaces"
                               (string-append (assoc-ref outputs "out")
                                              "/include"))
             #t))
         ;; These files are needed by sra-tools.
         (add-after 'install 'install-configuration-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out") "/kfg")))
               (mkdir target)
               (install-file "libs/kfg/default.kfg" target)
               (install-file "libs/kfg/certs.kfg" target))
             #t)))))
    (inputs
     `(("libxml2" ,libxml2)
       ("ngs-sdk" ,ngs-sdk)
       ("java-ngs" ,java-ngs)
       ("libmagic" ,file)
       ("hdf5" ,hdf5)))
    (native-inputs `(("perl" ,perl)))
    ;; NCBI-VDB requires SSE capability.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "https://github.com/ncbi/ncbi-vdb")
    (synopsis "Database engine for genetic information")
    (description
     "The NCBI-VDB library implements a highly compressed columnar data
warehousing engine that is most often used to store genetic information.
Databases are stored in a portable image within the file system, and can be
accessed/downloaded on demand across HTTP.")
    (license license:public-domain)))

(define-public plink
  (package
    (name "plink")
    (version "1.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://pngu.mgh.harvard.edu/~purcell/plink/dist/plink-"
             version "-src.zip"))
       (sha256
        (base32 "0as8gxm4pjyc8dxmm1sl873rrd7wn5qs0l29nqfnl31x8i467xaa"))
       (patches (search-patches "plink-1.07-unclobber-i.patch"
                                "plink-endian-detection.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:make-flags (list (string-append "LIB_LAPACK="
                                         (assoc-ref %build-inputs "lapack")
                                         "/lib/liblapack.so")
                          "WITH_LAPACK=1"
                          "FORCE_DYNAMIC=1"
                          ;; disable phoning home
                          "WITH_WEBCHECK=")
       #:phases
       (modify-phases %standard-phases
         ;; no "configure" script
         (delete 'configure)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin/")))
                      (install-file "plink" bin)
                      #t))))))
    (inputs
     `(("zlib" ,zlib)
       ("lapack" ,lapack)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://pngu.mgh.harvard.edu/~purcell/plink/")
    (synopsis "Whole genome association analysis toolset")
    (description
     "PLINK is a whole genome association analysis toolset, designed to
perform a range of basic, large-scale analyses in a computationally efficient
manner.  The focus of PLINK is purely on analysis of genotype/phenotype data,
so there is no support for steps prior to this (e.g. study design and
planning, generating genotype or CNV calls from raw data).  Through
integration with gPLINK and Haploview, there is some support for the
subsequent visualization, annotation and storage of results.")
    ;; Code is released under GPLv2, except for fisher.h, which is under
    ;; LGPLv2.1+
    (license (list license:gpl2 license:lgpl2.1+))))

(define-public plink-ng
  (package (inherit plink)
    (name "plink-ng")
    (version "1.90b4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/chrchang/plink-ng/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09ixrds009aczjswxr2alcb774mksq5g0v78dgjjn1h4dky0kf9a"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:make-flags (list "BLASFLAGS=-llapack -lopenblas"
                          "CFLAGS=-Wall -O2 -DDYNAMIC_ZLIB=1"
                          "ZLIB=-lz"
                          "-f" "Makefile.std")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "1.9") #t))
         (delete 'configure) ; no "configure" script
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin/")))
                      (install-file "plink" bin)
                      #t))))))
    (inputs
     `(("zlib" ,zlib)
       ("lapack" ,lapack)
       ("openblas" ,openblas)))
    (home-page "https://www.cog-genomics.org/plink/")
    (license license:gpl3+)))

(define-public smithlab-cpp
  (let ((revision "1")
        (commit "728a097bec88c6f4b8528b685932049e660eff2e"))
    (package
      (name "smithlab-cpp")
      (version (string-append "0." revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/smithlabcode/smithlab_cpp.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0d476lmj312xk77kr9fzrv7z1bv96yfyx0w7y62ycmnfbx32ll74"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-26))
         #:tests? #f ;no "check" target
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'use-samtools-headers
            (lambda _
              (substitute* '("SAM.cpp"
                             "SAM.hpp")
                (("sam.h") "samtools/sam.h"))
              #t))
           (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (lib     (string-append out "/lib"))
                     (include (string-append out "/include/smithlab-cpp")))
                (mkdir-p lib)
                (mkdir-p include)
                (for-each (cut install-file <> lib)
                          (find-files "." "\\.o$"))
                (for-each (cut install-file <> include)
                          (find-files "." "\\.hpp$")))
              #t))
           (delete 'configure))))
      (inputs
       `(("samtools" ,samtools-0.1)
         ("zlib" ,zlib)))
      (home-page "https://github.com/smithlabcode/smithlab_cpp")
      (synopsis "C++ helper library for functions used in Smith lab projects")
      (description
       "Smithlab CPP is a C++ library that includes functions used in many of
the Smith lab bioinformatics projects, such as a wrapper around Samtools data
structures, classes for genomic regions, mapped sequencing reads, etc.")
      (license license:gpl3+))))

(define-public preseq
  (package
    (name "preseq")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/smithlabcode/"
                                  "preseq/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "08r684l50pnxjpvmhzjgqq56yv9rfw90k8vx0nsrnrzk8mf9hsdq"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove bundled samtools.
               '(delete-file-recursively "samtools"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:make-flags
       (list (string-append "PREFIX="
                            (assoc-ref %outputs "out"))
             (string-append "LIBBAM="
                            (assoc-ref %build-inputs "samtools")
                            "/lib/libbam.a")
             (string-append "SMITHLAB_CPP="
                            (assoc-ref %build-inputs "smithlab-cpp")
                            "/lib")
             "PROGS=preseq"
             "INCLUDEDIRS=$(SMITHLAB_CPP)/../include/smithlab-cpp $(SAMTOOLS_DIR)")))
    (inputs
     `(("gsl" ,gsl)
       ("samtools" ,samtools-0.1)
       ("smithlab-cpp" ,smithlab-cpp)
       ("zlib" ,zlib)))
    (home-page "http://smithlabresearch.org/software/preseq/")
    (synopsis "Program for analyzing library complexity")
    (description
     "The preseq package is aimed at predicting and estimating the complexity
of a genomic sequencing library, equivalent to predicting and estimating the
number of redundant reads from a given sequencing depth and how many will be
expected from additional sequencing using an initial sequencing experiment.
The estimates can then be used to examine the utility of further sequencing,
optimize the sequencing depth, or to screen multiple libraries to avoid low
complexity samples.")
    (license license:gpl3+)))

(define-public python-screed
  (package
    (name "python-screed")
    (version "0.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "screed" version))
       (sha256
        (base32
         "18czszp9fkx3j6jr7y5kp6dfialscgddk05mw1zkhh2zhn0jd8i0"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append (getenv "PYTHONPATH") ":."))
             (zero? (system* "nosetests" "--attr" "!known_failing")))))))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (inputs
     `(("python-bz2file" ,python-bz2file)))
    (home-page "https://github.com/dib-lab/screed/")
    (synopsis "Short read sequence database utilities")
    (description "Screed parses FASTA and FASTQ files and generates databases.
Values such as sequence name, sequence description, sequence quality and the
sequence itself can be retrieved from these databases.")
    (license license:bsd-3)))

(define-public python2-screed
  (package-with-python2 python-screed))

(define-public sra-tools
  (package
    (name "sra-tools")
    (version "2.8.2-1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ncbi/sra-tools/archive/"
                       version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1camsijmvv2s45mb4iyf44ghl4gkd4rl0viphpcgl3ccchy32a0g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f ; not supported
       #:tests? #f ; no "check" target
       #:make-flags
       (list (string-append "DEFAULT_CRT="
                            (assoc-ref %build-inputs "ncbi-vdb")
                            "/kfg/certs.kfg")
             (string-append "DEFAULT_KFG="
                            (assoc-ref %build-inputs "ncbi-vdb")
                            "/kfg/default.kfg")
             (string-append "VDB_LIBDIR="
                            (assoc-ref %build-inputs "ncbi-vdb")
                            ,(if (string-prefix? "x86_64"
                                                 (or (%current-target-system)
                                                     (%current-system)))
                                 "/lib64"
                                 "/lib32")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-perl-search-path
           (lambda _
             ;; Work around "dotless @INC" build failure.
             (setenv "PERL5LIB"
                     (string-append (getcwd) "/setup:"
                                    (getenv "PERL5LIB")))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; The build system expects a directory containing the sources and
             ;; raw build output of ncbi-vdb, including files that are not
             ;; installed.  Since we are building against an installed version of
             ;; ncbi-vdb, the following modifications are needed.
             (substitute* "setup/konfigure.perl"
               ;; Make the configure script look for the "ilib" directory of
               ;; "ncbi-vdb" without first checking for the existence of a
               ;; matching library in its "lib" directory.
               (("^            my \\$f = File::Spec->catdir\\(\\$libdir, \\$lib\\);")
                "my $f = File::Spec->catdir($ilibdir, $ilib);")
               ;; Look for interface libraries in ncbi-vdb's "ilib" directory.
               (("my \\$ilibdir = File::Spec->catdir\\(\\$builddir, 'ilib'\\);")
                "my $ilibdir = File::Spec->catdir($dir, 'ilib');"))

             ;; Dynamic linking
             (substitute* "tools/copycat/Makefile"
               (("smagic-static") "lmagic"))

             ;; The 'configure' script doesn't recognize things like
             ;; '--enable-fast-install'.
             (zero? (system*
                     "./configure"
                     (string-append "--build-prefix=" (getcwd) "/build")
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     (string-append "--debug")
                     (string-append "--with-fuse-prefix="
                                    (assoc-ref inputs "fuse"))
                     (string-append "--with-magic-prefix="
                                    (assoc-ref inputs "libmagic"))
                     ;; TODO: building with libxml2 fails with linker errors
                     ;; (string-append "--with-xml2-prefix="
                     ;;                (assoc-ref inputs "libxml2"))
                     (string-append "--with-ncbi-vdb-sources="
                                    (assoc-ref inputs "ncbi-vdb"))
                     (string-append "--with-ncbi-vdb-build="
                                    (assoc-ref inputs "ncbi-vdb"))
                     (string-append "--with-ngs-sdk-prefix="
                                    (assoc-ref inputs "ngs-sdk"))
                     (string-append "--with-hdf5-prefix="
                                    (assoc-ref inputs "hdf5"))))))
         ;; This version of sra-tools fails to build with glibc because of a
         ;; naming conflict.  glibc-2.25/include/bits/mathcalls.h already
         ;; contains a definition of "canonicalize", so we rename it.
         ;;
         ;; See upstream bug report:
         ;; https://github.com/ncbi/sra-tools/issues/67
         (add-after 'unpack 'patch-away-glibc-conflict
           (lambda _
             (substitute* "tools/bam-loader/bam.c"
               (("canonicalize\\(" line)
                (string-append "sra_tools_" line)))
             #t)))))
    (native-inputs `(("perl" ,perl)))
    (inputs
     `(("ngs-sdk" ,ngs-sdk)
       ("ncbi-vdb" ,ncbi-vdb)
       ("libmagic" ,file)
       ("fuse" ,fuse)
       ("hdf5" ,hdf5)
       ("zlib" ,zlib)))
    (home-page "http://www.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?view=software")
    (synopsis "Tools and libraries for reading and writing sequencing data")
    (description
     "The SRA Toolkit from NCBI is a collection of tools and libraries for
reading of sequencing files from the Sequence Read Archive (SRA) database and
writing files into the .sra format.")
    (license license:public-domain)))

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

(define-public seqmagick
  (package
    (name "seqmagick")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "seqmagick" version))
       (sha256
        (base32
         "12bfyp8nqi0hd36rmj450aygafp01qy3hkbvlwn3bk39pyjjkgg5"))))
    (build-system python-build-system)
    (inputs
     `(("python-biopython" ,python-biopython)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/fhcrc/seqmagick")
    (synopsis "Tools for converting and modifying sequence files")
    (description
     "Bioinformaticians often have to convert sequence files between formats
and do little manipulations on them, and it's not worth writing scripts for
that.  Seqmagick is a utility to expose the file format conversion in
BioPython in a convenient way.  Instead of having a big mess of scripts, there
is one that takes arguments.")
    (license license:gpl3)))

(define-public seqtk
  (package
    (name "seqtk")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/lh3/seqtk/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ywdyzpmfiz2wp6ampbzqg4y8bj450nfgqarpamg045b8mk32lxx"))
                            (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove extraneous header files, as is done in the seqtk
                  ;; master branch.
                  (for-each (lambda (file) (delete-file file))
                            (list "ksort.h" "kstring.h" "kvec.h"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           ;; There are no tests, so we just run a sanity check.
           (lambda _ (zero? (system* "./seqtk" "seq"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (install-file "seqtk" bin)))))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/lh3/seqtk")
    (synopsis "Toolkit for processing biological sequences in FASTA/Q format")
    (description
     "Seqtk is a fast and lightweight tool for processing sequences in the
FASTA or FASTQ format.  It parses both FASTA and FASTQ files which can be
optionally compressed by gzip.")
      (license license:expat)))

(define-public snap-aligner
  (package
    (name "snap-aligner")
    (version "1.0beta.18")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/amplab/snap/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vnsjwv007k1fl1q7d681kbwn6bc66cgw6h16hym6gvyy71qv2ly"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check (lambda _ (zero? (system* "./unit_tests"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "snap-aligner" bin)
               (install-file "SNAPCommand" bin)
               #t))))))
    (native-inputs
     `(("zlib" ,zlib)))
    (home-page "http://snap.cs.berkeley.edu/")
    (synopsis "Short read DNA sequence aligner")
    (description
     "SNAP is a fast and accurate aligner for short DNA reads.  It is
optimized for modern read lengths of 100 bases or higher, and takes advantage
of these reads to align data quickly through a hash-based indexing scheme.")
    ;; 32-bit systems are not supported by the unpatched code.
    ;; Following the bug reports https://github.com/amplab/snap/issues/68 and
    ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=812378 we see that
    ;; systems without a lot of memory cannot make good use of this program.
    (supported-systems '("x86_64-linux"))
    (license license:asl2.0)))

(define-public sortmerna
  (package
    (name "sortmerna")
    (version "2.1b")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/biocore/sortmerna/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ghaghvd82af9j5adavxh77g7hm247d1r69m3fbi6f1jdivj5ldk"))))
    (build-system gnu-build-system)
    (outputs '("out"      ;for binaries
               "db"))     ;for sequence databases
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (db    (assoc-ref outputs "db"))
                    (share
                     (string-append db "/share/sortmerna/rRNA_databases")))
               (install-file "sortmerna" bin)
               (install-file "indexdb_rna" bin)
               (for-each (lambda (file)
                           (install-file file share))
                         (find-files "rRNA_databases" ".*fasta"))
               #t))))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "http://bioinfo.lifl.fr/RNA/sortmerna")
    (synopsis "Biological sequence analysis tool for NGS reads")
    (description
     "SortMeRNA is a biological sequence analysis tool for filtering, mapping
and operational taxonomic unit (OTU) picking of next generation
sequencing (NGS) reads.  The core algorithm is based on approximate seeds and
allows for fast and sensitive analyses of nucleotide sequences.  The main
application of SortMeRNA is filtering rRNA from metatranscriptomic data.")
    ;; The source includes x86 specific code
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:lgpl3)))

(define-public star
  (package
    (name "star")
    (version "2.5.3a")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alexdobin/STAR/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "013wirlz8lllgjyagl48l75n1isxyabqb3sj7qlsl0x1rmvqw99a"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "source/Makefile"
                    (("/bin/rm") "rm"))
                  ;; Remove pre-built binaries and bundled htslib sources.
                  (delete-file-recursively "bin/MacOSX_x86_64")
                  (delete-file-recursively "bin/Linux_x86_64")
                  (delete-file-recursively "bin/Linux_x86_64_static")
                  (delete-file-recursively "source/htslib")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no check target
       #:make-flags '("STAR")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source-dir
           (lambda _ (chdir "source") #t))
         (add-after 'enter-source-dir 'make-reproducible
           (lambda _
             (substitute* "Makefile"
               (("(COMPILATION_TIME_PLACE=\")(.*)(\")" _ pre mid post)
                (string-append pre "Built with Guix" post)))))
         (add-after 'enter-source-dir 'do-not-use-bundled-htslib
           (lambda _
             (substitute* "Makefile"
               (("(Depend.list: \\$\\(SOURCES\\) parametersDefault\\.xxd) htslib"
                 _ prefix) prefix))
             (substitute* '("BAMfunctions.cpp"
                            "signalFromBAM.h"
                            "bam_cat.h"
                            "bam_cat.c"
                            "STAR.cpp"
                            "bamRemoveDuplicates.cpp")
               (("#include \"htslib/([^\"]+\\.h)\"" _ header)
                (string-append "#include <" header ">")))
             (substitute* "IncludeDefine.h"
               (("\"htslib/(htslib/[^\"]+.h)\"" _ header)
                (string-append "<" header ">")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (install-file "STAR" bin))
             #t))
         (delete 'configure))))
    (native-inputs
     `(("xxd" ,xxd)))
    (inputs
     `(("htslib" ,htslib)
       ("zlib" ,zlib)))
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
    ;; Only 64-bit systems are supported according to the README.
    (supported-systems '("x86_64-linux" "mips64el-linux"))
    ;; STAR is licensed under GPLv3 or later; htslib is MIT-licensed.
    (license license:gpl3+)))

(define-public subread
  (package
    (name "subread")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/subread/subread-"
                                  version "/subread-" version "-source.tar.gz"))
              (sha256
               (base32
                "0ah0n4jx6ksk2m2j7xk385x2qzmk1y4rfc6a4mfrdqrlq721w99i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
      ;; The CC and CCFLAGS variables are set to contain a lot of x86_64
      ;; optimizations by default, so we override these flags such that x86_64
      ;; flags are only added when the build target is an x86_64 system.
       #:make-flags
       (list (let ((system ,(or (%current-target-system)
                                (%current-system)))
                   (flags '("-ggdb" "-fomit-frame-pointer"
                            "-ffast-math" "-funroll-loops"
                            "-fmessage-length=0"
                            "-O9" "-Wall" "-DMAKE_FOR_EXON"
                            "-DMAKE_STANDALONE"
                            "-DSUBREAD_VERSION=\\\"${SUBREAD_VERSION}\\\""))
                   (flags64 '("-mmmx" "-msse" "-msse2" "-msse3")))
               (if (string-prefix? "x86_64" system)
                   (string-append "CCFLAGS=" (string-join (append flags flags64)))
                   (string-append "CCFLAGS=" (string-join flags))))
             "-f" "Makefile.Linux"
             "CC=gcc ${CCFLAGS}")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "src") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (mkdir-p bin)
               (copy-recursively "../bin" bin))))
         ;; no "configure" script
         (delete 'configure))))
    (inputs `(("zlib" ,zlib)))
    (home-page "http://bioinf.wehi.edu.au/subread-package/")
    (synopsis "Tool kit for processing next-gen sequencing data")
    (description
     "The subread package contains the following tools: subread aligner, a
general-purpose read aligner; subjunc aligner: detecting exon-exon junctions
and mapping RNA-seq reads; featureCounts: counting mapped reads for genomic
features; exactSNP: a SNP caller that discovers SNPs by testing signals
against local background noises.")
    (license license:gpl3+)))

(define-public stringtie
  (package
    (name "stringtie")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ccb.jhu.edu/software/stringtie/dl/"
                                  "stringtie-" version ".tar.gz"))
              (sha256
               (base32
                "1cqllsc1maq4kh92isi8yadgzbmnf042hlnalpk3y59aph1z3bfz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "samtools-0.1.18")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no test suite
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'build 'use-system-samtools
           (lambda _
             (substitute* "Makefile"
               (("stringtie: \\$\\{BAM\\}/libbam\\.a")
                "stringtie: "))
             (substitute* '("gclib/GBam.h"
                            "gclib/GBam.cpp")
               (("#include \"(bam|sam|kstring).h\"" _ header)
                (string-append "#include <samtools/" header ".h>")))
             #t))
         (add-after 'unpack 'remove-duplicate-typedef
           (lambda _
             ;; This typedef conflicts with the typedef in
             ;; glibc-2.25/include/bits/types.h
             (substitute* "gclib/GThreads.h"
               (("typedef long long __intmax_t;") ""))
             #t))
         (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
              (install-file "stringtie" bin)
              #t))))))
    (inputs
     `(("samtools" ,samtools-0.1)
       ("zlib" ,zlib)))
    (home-page "http://ccb.jhu.edu/software/stringtie/")
    (synopsis "Transcript assembly and quantification for RNA-Seq data")
    (description
     "StringTie is a fast and efficient assembler of RNA-Seq sequence
alignments into potential transcripts.  It uses a novel network flow algorithm
as well as an optional de novo assembly step to assemble and quantitate
full-length transcripts representing multiple splice variants for each gene
locus.  Its input can include not only the alignments of raw reads used by
other transcript assemblers, but also alignments of longer sequences that have
been assembled from those reads.  To identify differentially expressed genes
between experiments, StringTie's output can be processed either by the
Cuffdiff or Ballgown programs.")
    (license license:artistic2.0)))

(define-public taxtastic
  (package
    (name "taxtastic")
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "taxtastic" version))
              (sha256
               (base32
                "0s79z8kfl853x7l4h8ms05k31q87aw62nrchlk20w9n227j35929"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "python" "-m" "unittest" "discover" "-v")))))))
    (propagated-inputs
     `(("python-sqlalchemy" ,python2-sqlalchemy)
       ("python-decorator" ,python2-decorator)
       ("python-biopython" ,python2-biopython)
       ("python-pandas" ,python2-pandas)))
    (home-page "https://github.com/fhcrc/taxtastic")
    (synopsis "Tools for taxonomic naming and annotation")
    (description
     "Taxtastic is software written in python used to build and maintain
reference packages i.e. collections of reference trees, reference alignments,
profiles, and associated taxonomic information.")
    (license license:gpl3+)))

(define-public vcftools
  (package
    (name "vcftools")
    (version "0.1.15")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/vcftools/vcftools/releases/download/v"
                    version "/vcftools-" version ".tar.gz"))
              (sha256
               (base32
                "1qw30c45wihgy632rbz4rh3njnwj4msj46l1rsgdhyg6bgypmr1i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags (list
                     "CFLAGS=-O2" ; override "-m64" flag
                     (string-append "PREFIX=" (assoc-ref %outputs "out"))
                     (string-append "MANDIR=" (assoc-ref %outputs "out")
                                    "/share/man/man1"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("perl" ,perl)
       ("zlib" ,zlib)))
    (home-page "https://vcftools.github.io/")
    (synopsis "Tools for working with VCF files")
    (description
     "VCFtools is a program package designed for working with VCF files, such
as those generated by the 1000 Genomes Project.  The aim of VCFtools is to
provide easily accessible methods for working with complex genetic variation
data in the form of VCF files.")
    ;; The license is declared as LGPLv3 in the README and
    ;; at https://vcftools.github.io/license.html
    (license license:lgpl3)))

(define-public infernal
  (package
    (name "infernal")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://eddylab.org/software/infernal/"
                                  "infernal-" version ".tar.gz"))
              (sha256
               (base32
                "0sr2hiz3qxfwqpz3whxr6n82p3x27336v3f34iqznp10hks2935c"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl))) ; for tests
    (home-page "http://eddylab.org/infernal/")
    (synopsis "Inference of RNA alignments")
    (description "Infernal (\"INFERence of RNA ALignment\") is a tool for
searching DNA sequence databases for RNA structure and sequence similarities.
It is an implementation of a special case of profile stochastic context-free
grammars called @dfn{covariance models} (CMs).  A CM is like a sequence
profile, but it scores a combination of sequence consensus and RNA secondary
structure consensus, so in many cases, it is more capable of identifying RNA
homologs that conserve their secondary structure more than their primary
sequence.")
    ;; Infernal 1.1.2 requires VMX or SSE capability for parallel instructions.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:bsd-3)))

(define-public r-centipede
  (package
    (name "r-centipede")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.r-forge.r-project.org/"
                                  "src/contrib/CENTIPEDE_" version ".tar.gz"))
              (sha256
               (base32
                "1hsx6qgwr0i67fhy9257zj7s0ppncph2hjgbia5nn6nfmj0ax6l9"))))
    (build-system r-build-system)
    (home-page "http://centipede.uchicago.edu/")
    (synopsis "Predict transcription factor binding sites")
    (description
     "CENTIPEDE applies a hierarchical Bayesian mixture model to infer regions
of the genome that are bound by particular transcription factors.  It starts
by identifying a set of candidate binding sites, and then aims to classify the
sites according to whether each site is bound or not bound by a transcription
factor.  CENTIPEDE is an unsupervised learning algorithm that discriminates
between two different types of motif instances using as much relevant
information as possible.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-vegan
  (package
    (name "r-vegan")
    (version "2.4-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "vegan" version))
       (sha256
        (base32
         "175mqr42mmxy98gpf3rky8alnkw3w1k90ggdw3cispl36841p76w"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (propagated-inputs
     `(("r-cluster" ,r-cluster)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-mgcv" ,r-mgcv)
       ("r-permute" ,r-permute)))
    (home-page "https://cran.r-project.org/web/packages/vegan")
    (synopsis "Functions for community ecology")
    (description
     "The vegan package provides tools for descriptive community ecology.  It
has most basic functions of diversity analysis, community ordination and
dissimilarity analysis.  Most of its multivariate tools can be used for other
data types as well.")
    (license license:gpl2+)))

(define-public r-annotate
  (package
    (name "r-annotate")
    (version "1.56.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotate" version))
       (sha256
        (base32
         "14c5xd9kasvcwg5gbjys2c1vizxhlqlzxakqc2kml0kw97hmx0rq"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-dbi" ,r-dbi)
       ("r-rcurl" ,r-rcurl)
       ("r-xml" ,r-xml)
       ("r-xtable" ,r-xtable)))
    (home-page
     "https://bioconductor.org/packages/annotate")
    (synopsis "Annotation for microarrays")
    (description "This package provides R environments for the annotation of
microarrays.")
    (license license:artistic2.0)))

(define-public r-copynumber
  (package
    (name "r-copynumber")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "copynumber" version))
              (sha256
               (base32
                "01kcwzl485yjrkgyg8117b1il957ss0v6rq4bbxf4ksd5fzcjmyx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-s4vectors" ,r-s4vectors)
       ("r-iranges" ,r-iranges)
       ("r-genomicranges" ,r-genomicranges)
       ("r-biocgenerics" ,r-biocgenerics)))
    (home-page "https://bioconductor.org/packages/copynumber")
    (synopsis "Segmentation of single- and multi-track copy number data")
    (description
     "This package segments single- and multi-track copy number data by a
penalized least squares regression method.")
    (license license:artistic2.0)))

(define-public r-geneplotter
  (package
    (name "r-geneplotter")
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geneplotter" version))
       (sha256
        (base32
         "1z3g7frc1iviwrsv2dlm4nqvkc0685h4va0388yfxn102ln8wwma"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-lattice" ,r-lattice)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page "https://bioconductor.org/packages/geneplotter")
    (synopsis "Graphics functions for genomic data")
    (description
     "This package provides functions for plotting genomic data.")
    (license license:artistic2.0)))

(define-public r-genefilter
  (package
    (name "r-genefilter")
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "genefilter" version))
       (sha256
        (base32
         "173swlg6gj4kdllbqvyiw5dggbcxiwlwpqmllsv4dxzn7h25i3g7"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-s4vectors" ,r-s4vectors)
       ("r-survival" ,r-survival)))
    (home-page "https://bioconductor.org/packages/genefilter")
    (synopsis "Filter genes from high-throughput experiments")
    (description
     "This package provides basic functions for filtering genes from
high-throughput sequencing experiments.")
    (license license:artistic2.0)))

(define-public r-deseq2
  (package
    (name "r-deseq2")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq2" version))
       (sha256
        (base32
         "1iyimg1s0x5pdmvl8x08s8h0v019y0nhjzs50chagbpk2x91fsmv"))))
    (properties `((upstream-name . "DESeq2")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-genefilter" ,r-genefilter)
       ("r-geneplotter" ,r-geneplotter)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-hmisc" ,r-hmisc)
       ("r-iranges" ,r-iranges)
       ("r-locfit" ,r-locfit)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/DESeq2")
    (synopsis "Differential gene expression analysis")
    (description
     "This package provides functions to estimate variance-mean dependence in
count data from high-throughput nucleotide sequencing assays and test for
differential expression based on a model using the negative binomial
distribution.")
    (license license:lgpl3+)))

(define-public r-dexseq
  (package
    (name "r-dexseq")
    (version "1.24.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DEXSeq" version))
       (sha256
        (base32
         "0xip73hlbr3zry9d7ly9vvvsbb3xjcmfa09lr9fdy528dwjrf084"))))
    (properties `((upstream-name . "DEXSeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biomart" ,r-biomart)
       ("r-deseq2" ,r-deseq2)
       ("r-genefilter" ,r-genefilter)
       ("r-geneplotter" ,r-geneplotter)
       ("r-genomicranges" ,r-genomicranges)
       ("r-hwriter" ,r-hwriter)
       ("r-iranges" ,r-iranges)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-statmod" ,r-statmod)
       ("r-stringr" ,r-stringr)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/DEXSeq")
    (synopsis "Inference of differential exon usage in RNA-Seq")
    (description
     "This package is focused on finding differential exon usage using RNA-seq
exon counts between samples with different experimental designs.  It provides
functions that allows the user to make the necessary statistical tests based
on a model that uses the negative binomial distribution to estimate the
variance between biological replicates and generalized linear models for
testing.  The package also provides functions for the visualization and
exploration of the results.")
    (license license:gpl3+)))

(define-public r-annotationforge
  (package
    (name "r-annotationforge")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationForge" version))
       (sha256
        (base32
         "01vbrf76vqfvxh6vpfxkjwccxggnha3byqzj333glqz2b6kwx5q1"))))
    (properties
     `((upstream-name . "AnnotationForge")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-dbi" ,r-dbi)
       ("r-rcurl" ,r-rcurl)
       ("r-rsqlite" ,r-rsqlite)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xml" ,r-xml)))
    (home-page "https://bioconductor.org/packages/AnnotationForge")
    (synopsis "Code for building annotation database packages")
    (description
     "This package provides code for generating Annotation packages and their
databases.  Packages produced are intended to be used with AnnotationDbi.")
    (license license:artistic2.0)))

(define-public r-rbgl
  (package
    (name "r-rbgl")
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RBGL" version))
       (sha256
        (base32
         "18jad23i3899ypv4bg3l47cvvs3qnj1pqis2p9x0135yv5y6wnv7"))))
    (properties `((upstream-name . "RBGL")))
    (build-system r-build-system)
    (propagated-inputs `(("r-graph" ,r-graph)))
    (home-page "https://www.bioconductor.org/packages/RBGL")
    (synopsis "Interface to the Boost graph library")
    (description
     "This package provides a fairly extensive and comprehensive interface to
the graph algorithms contained in the Boost library.")
    (license license:artistic2.0)))

(define-public r-gseabase
  (package
    (name "r-gseabase")
    (version "1.40.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GSEABase" version))
       (sha256
        (base32
         "10cmjxahg2plwacfan6g0k8cwyzya96ypc7m1r79gwqkyykxw5fz"))))
    (properties `((upstream-name . "GSEABase")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-graph" ,r-graph)
       ("r-xml" ,r-xml)))
    (home-page "https://bioconductor.org/packages/GSEABase")
    (synopsis "Gene set enrichment data structures and methods")
    (description
     "This package provides classes and methods to support @dfn{Gene Set
Enrichment Analysis} (GSEA).")
    (license license:artistic2.0)))

(define-public r-category
  (package
    (name "r-category")
    (version "2.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Category" version))
       (sha256
        (base32
         "0mkav04vbla0xfa0dssxdd0rjs589sxi83xklf5iq5hj3dm8y0i8"))))
    (properties `((upstream-name . "Category")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-genefilter" ,r-genefilter)
       ("r-graph" ,r-graph)
       ("r-gseabase" ,r-gseabase)
       ("r-matrix" ,r-matrix)
       ("r-rbgl" ,r-rbgl)
       ("r-dbi" ,r-dbi)))
    (home-page "https://bioconductor.org/packages/Category")
    (synopsis "Category analysis")
    (description
     "This package provides a collection of tools for performing category
analysis.")
    (license license:artistic2.0)))

(define-public r-gostats
  (package
    (name "r-gostats")
    (version "2.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOstats" version))
       (sha256
        (base32
         "04gqfdlx9fxf97qf0l28x4aaqvl10n6v58qiz5fiaw05sbj1pf1i"))))
    (properties `((upstream-name . "GOstats")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationforge" ,r-annotationforge)
       ("r-biobase" ,r-biobase)
       ("r-category" ,r-category)
       ("r-go-db" ,r-go-db)
       ("r-graph" ,r-graph)
       ("r-rgraphviz" ,r-rgraphviz)
       ("r-rbgl" ,r-rbgl)))
    (home-page "https://bioconductor.org/packages/GOstats")
    (synopsis "Tools for manipulating GO and microarrays")
    (description
     "This package provides a set of tools for interacting with GO and
microarray data.  A variety of basic manipulation tools for graphs, hypothesis
testing and other simple calculations.")
    (license license:artistic2.0)))

(define-public r-shortread
  (package
    (name "r-shortread")
    (version "1.36.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ShortRead" version))
       (sha256
        (base32
         "1cyv47632m9ljkxfsvnvmd19sb607ys5kz8fwh6v39dnw16g0a6m"))))
    (properties `((upstream-name . "ShortRead")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-hwriter" ,r-hwriter)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-latticeextra" ,r-latticeextra)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)
       ("r-zlibbioc" ,r-zlibbioc)))
    (home-page "https://bioconductor.org/packages/ShortRead")
    (synopsis "FASTQ input and manipulation tools")
    (description
     "This package implements sampling, iteration, and input of FASTQ files.
It includes functions for filtering and trimming reads, and for generating a
quality assessment report.  Data are represented as
@code{DNAStringSet}-derived objects, and easily manipulated for a diversity of
purposes.  The package also contains legacy support for early single-end,
ungapped alignment formats.")
    (license license:artistic2.0)))

(define-public r-systempiper
  (package
    (name "r-systempiper")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "systemPipeR" version))
       (sha256
        (base32
         "11mj8pjq5vj25768vmagpzv74fvi3p3kdk5zdlznqyiaggri04cv"))))
    (properties `((upstream-name . "systemPipeR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-batchjobs" ,r-batchjobs)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-deseq2" ,r-deseq2)
       ("r-edger" ,r-edger)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-go-db" ,r-go-db)
       ("r-gostats" ,r-gostats)
       ("r-limma" ,r-limma)
       ("r-pheatmap" ,r-pheatmap)
       ("r-rjson" ,r-rjson)
       ("r-rsamtools" ,r-rsamtools)
       ("r-shortread" ,r-shortread)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://github.com/tgirke/systemPipeR")
    (synopsis "Next generation sequencing workflow and reporting environment")
    (description
     "This R package provides tools for building and running automated
end-to-end analysis workflows for a wide range of @dfn{next generation
sequence} (NGS) applications such as RNA-Seq, ChIP-Seq, VAR-Seq and Ribo-Seq.
Important features include a uniform workflow interface across different NGS
applications, automated report generation, and support for running both R and
command-line software, such as NGS aligners or peak/variant callers, on local
computers or compute clusters.  Efficient handling of complex sample sets and
experimental designs is facilitated by a consistently implemented sample
annotation infrastructure.")
    (license license:artistic2.0)))

(define-public r-grohmm
  (package
    (name "r-grohmm")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "groHMM" version))
       (sha256
        (base32
         "0cjkj0ypyc4dfi9s8dh88kh6q4xlpnc0wal7njg4b4gqj0l2hva7"))))
    (properties `((upstream-name . "groHMM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-mass" ,r-mass)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://github.com/Kraus-Lab/groHMM")
    (synopsis "GRO-seq analysis pipeline")
    (description
     "This package provides a pipeline for the analysis of GRO-seq data.")
    (license license:gpl3+)))

(define-public r-txdb-hsapiens-ucsc-hg19-knowngene
  (package
    (name "r-txdb-hsapiens-ucsc-hg19-knowngene")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib"
                                  "/TxDb.Hsapiens.UCSC.hg19.knownGene_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1sajhcqqwazgz2lqbik7rd935i7kpnh08zxbp2ra10j72yqy4g86"))))
    (properties
     `((upstream-name . "TxDb.Hsapiens.UCSC.hg19.knownGene")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-genomicfeatures" ,r-genomicfeatures)))
    (home-page
     "https://bioconductor.org/packages/TxDb.Hsapiens.UCSC.hg19.knownGene/")
    (synopsis "Annotation package for human genome in TxDb format")
    (description
     "This package provides an annotation database of Homo sapiens genome
data.  It is derived from the UCSC hg19 genome and based on the \"knownGene\"
track.  The database is exposed as a @code{TxDb} object.")
    (license license:artistic2.0)))

(define-public r-sparql
  (package
  (name "r-sparql")
  (version "1.16")
  (source (origin
           (method url-fetch)
           (uri (cran-uri "SPARQL" version))
           (sha256
            (base32
             "0gak1q06yyhdmcxb2n3v0h9gr1vqd0viqji52wpw211qp6r6dcrc"))))
  (properties `((upstream-name . "SPARQL")))
  (build-system r-build-system)
  (propagated-inputs
   `(("r-rcurl" ,r-rcurl)
     ("r-xml" ,r-xml)))
  (home-page "https://cran.r-project.org/web/packages/SPARQL")
  (synopsis "SPARQL client for R")
  (description "This package provides an interface to use SPARQL to pose
SELECT or UPDATE queries to an end-point.")
  ;; The only license indication is found in the DESCRIPTION file,
  ;; which states GPL-3.  So we cannot assume GPLv3+.
  (license license:gpl3)))

(define-public vsearch
  (package
    (name "vsearch")
    (version "2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/torognes/vsearch/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jlzfgh79fzmb4g7sngzdjjsjyc37icvs1k7vmc2ksxglj6x5i7f"))
       (patches (search-patches "vsearch-unbundle-cityhash.patch"))
       (snippet
        '(begin
           ;; Remove bundled cityhash sources.  The vsearch source is adjusted
           ;; for this in the patch.
           (delete-file "src/city.h")
           (delete-file "src/citycrc.h")
           (delete-file "src/city.cc")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _ (zero? (system* "autoreconf" "-vif")))))))
    (inputs
     `(("zlib" ,zlib)
       ("bzip2" ,bzip2)
       ("cityhash" ,cityhash)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (synopsis "Sequence search tools for metagenomics")
    (description
     "VSEARCH supports DNA sequence searching, clustering, chimera detection,
dereplication, pairwise alignment, shuffling, subsampling, sorting and
masking.  The tool takes advantage of parallelism in the form of SIMD
vectorization as well as multiple threads to perform accurate alignments at
high speed.  VSEARCH uses an optimal global aligner (full dynamic programming
Needleman-Wunsch).")
    (home-page "https://github.com/torognes/vsearch")
    ;; vsearch uses non-portable SSE intrinsics so building fails on other
    ;; platforms.
    (supported-systems '("x86_64-linux"))
    ;; Dual licensed; also includes public domain source.
    (license (list license:gpl3 license:bsd-2))))

(define-public pardre
  (package
    (name "pardre")
    ;; The source of 1.1.5 changed in place, so we append "-1" to the version.
    (version "1.1.5-1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pardre/ParDRe-rel"
                           "1.1.5" ".tar.gz"))
       (sha256
        (base32
         "17j73nc0viq4f6qj50nrndsrif5d6b71q8fl87m54psiv0ilns2b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (install-file "ParDRe" bin)
               #t))))))
    (inputs
     `(("openmpi" ,openmpi)
       ("zlib" ,zlib)))
    (synopsis "Parallel tool to remove duplicate DNA reads")
    (description
     "ParDRe is a parallel tool to remove duplicate genetic sequence reads.
Duplicate reads can be seen as identical or nearly identical sequences with
some mismatches.  This tool lets users avoid the analysis of unnecessary
reads, reducing the time of subsequent procedures with the
dataset (e.g. assemblies, mappings, etc.).  The tool is implemented with MPI
in order to exploit the parallel capabilities of multicore clusters.  It is
faster than multithreaded counterparts (end of 2015) for the same number of
cores and, thanks to the message-passing technology, it can be executed on
clusters.")
    (home-page "https://sourceforge.net/projects/pardre/")
    (license license:gpl3+)))

(define-public ruby-bio-kseq
  (package
    (name "ruby-bio-kseq")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-kseq" version))
       (sha256
        (base32
         "1xyaha46khb5jc6wzkbf7040jagac49jbimn0vcrzid0j8jdikrz"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rake-compiler" ,ruby-rake-compiler)))
    (inputs
     `(("zlib" ,zlib)))
    (synopsis "Ruby bindings for the kseq.h FASTA/Q parser")
    (description
     "@code{Bio::Kseq} provides ruby bindings to the @code{kseq.h} FASTA and
FASTQ parsing code.  It provides a fast iterator over sequences and their
quality scores.")
    (home-page "https://github.com/gusevfe/bio-kseq")
    (license license:expat)))

(define-public bio-locus
  (package
    (name "bio-locus")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-locus" version))
       (sha256
        (base32
         "02vmrxyimkj9sahsp4zhfhnmbvz6dbbqz1y01vglf8cbwvkajfl0"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis "Tool for fast querying of genome locations")
    (description
     "Bio-locus is a tabix-like tool for fast querying of genome
locations.  Many file formats in bioinformatics contain records that
start with a chromosome name and a position for a SNP, or a start-end
position for indels.  Bio-locus allows users to store this chr+pos or
chr+pos+alt information in a database.")
    (home-page "https://github.com/pjotrp/bio-locus")
    (license license:expat)))

(define-public bio-blastxmlparser
  (package
    (name "bio-blastxmlparser")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bio-blastxmlparser" version))
              (sha256
               (base32
                "1wf4qygcmdjgcqm6flmvsagfr1gs9lf63mj32qv3z1f481zc5692"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-bio-logger" ,ruby-bio-logger)
       ("ruby-nokogiri" ,ruby-nokogiri)))
    (inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis "Fast big data BLAST XML parser and library")
    (description
     "Very fast parallel big-data BLAST XML file parser which can be used as
command line utility.  Use blastxmlparser to: Parse BLAST XML; filter output;
generate FASTA, JSON, YAML, RDF, JSON-LD, HTML, CSV, tabular output etc.")
    (home-page "https://github.com/pjotrp/blastxmlparser")
    (license license:expat)))

(define-public bioruby
  (package
    (name "bioruby")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio" version))
       (sha256
        (base32
         "0hdl0789c9n4mprnx5pgd46bfwl8d000rqpamj5h6kkjgspijv49"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-libxml" ,ruby-libxml)))
    (native-inputs
     `(("which" ,which)))  ; required for test phase
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-test-command
          (lambda _
            (substitute* '("test/functional/bio/test_command.rb")
              (("/bin/sh") (which "sh")))
            (substitute* '("test/functional/bio/test_command.rb")
              (("/bin/ls") (which "ls")))
            (substitute* '("test/functional/bio/test_command.rb")
              (("which") (which "which")))
            (substitute* '("test/functional/bio/test_command.rb",
                           "test/data/command/echoarg2.sh")
              (("/bin/echo") (which "echo")))
            #t)))))
    (synopsis "Ruby library, shell and utilities for bioinformatics")
    (description "BioRuby comes with a comprehensive set of Ruby development
tools and libraries for bioinformatics and molecular biology.  BioRuby has
components for sequence analysis, pathway analysis, protein modelling and
phylogenetic analysis; it supports many widely used data formats and provides
easy access to databases, external programs and public web services, including
BLAST, KEGG, GenBank, MEDLINE and GO.")
    (home-page "http://bioruby.org/")
    ;; Code is released under Ruby license, except for setup
    ;; (LGPLv2.1+) and scripts in samples (which have GPL2 and GPL2+)
    (license (list license:ruby license:lgpl2.1+ license:gpl2+ ))))

(define-public r-acsnminer
  (package
    (name "r-acsnminer")
    (version "0.16.8.25")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ACSNMineR" version))
              (sha256
               (base32
                "0gh604s8qall6zfjlwcg2ilxjvz08dplf9k5g47idhv43scm748l"))))
    (properties `((upstream-name . "ACSNMineR")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-ggplot2" ,r-ggplot2)
        ("r-gridextra" ,r-gridextra)))
    (home-page "https://cran.r-project.org/web/packages/ACSNMineR")
    (synopsis "Gene enrichment analysis")
    (description
     "This package provides tools to compute and represent gene set enrichment
or depletion from your data based on pre-saved maps from the @dfn{Atlas of
Cancer Signalling Networks} (ACSN) or user imported maps.  The gene set
enrichment can be run with hypergeometric test or Fisher exact test, and can
use multiple corrections.  Visualization of data can be done either by
barplots or heatmaps.")
    (license license:gpl2+)))

(define-public r-biocgenerics
  (package
    (name "r-biocgenerics")
    (version "0.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocGenerics" version))
              (sha256
               (base32
                "03wxvhxyrhipbgcg83lqlfn7p9gbzzrnl48y0dq7303xgp232zai"))))
    (properties
     `((upstream-name . "BiocGenerics")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BiocGenerics")
    (synopsis "S4 generic functions for Bioconductor")
    (description
     "This package provides S4 generic functions needed by many Bioconductor
packages.")
    (license license:artistic2.0)))

(define-public r-biocinstaller
  (package
    (name "r-biocinstaller")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocInstaller" version))
              (sha256
               (base32
                "19fga27bv6q9v5mpil74y76lahmnwvpg2h33rdx1r79nvljkd19d"))))
    (properties
     `((upstream-name . "BiocInstaller")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BiocInstaller")
    (synopsis "Install Bioconductor packages")
    (description "This package is used to install and update R packages from
Bioconductor, CRAN, and Github.")
    (license license:artistic2.0)))

(define-public r-biocviews
  (package
    (name "r-biocviews")
    (version "1.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biocViews" version))
              (sha256
               (base32
                "09zyqj1kqc089lmh9sliy0acanx9zimcasvp71dsrg2bqm08r1md"))))
    (properties
     `((upstream-name . "biocViews")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-graph" ,r-graph)
       ("r-rbgl" ,r-rbgl)
       ("r-rcurl" ,r-rcurl)
       ("r-xml" ,r-xml)
       ("r-runit" ,r-runit)))
    (home-page "https://bioconductor.org/packages/biocViews")
    (synopsis "Bioconductor package categorization helper")
    (description "The purpose of biocViews is to create HTML pages that
categorize packages in a Bioconductor package repository according to keywords,
also known as views, in a controlled vocabulary.")
    (license license:artistic2.0)))

(define-public r-bookdown
  (package
    (name "r-bookdown")
    (version "0.7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "bookdown" version))
              (sha256
               (base32
                "1b3fw1f41zph5yw3kynb47aijq53vhaa6mnnvxly72zamyzdf95q"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-knitr" ,r-knitr)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-tinytex" ,r-tinytex)
       ("r-yaml" ,r-yaml)
       ("r-xfun" ,r-xfun)
       ("ghc-pandoc" ,ghc-pandoc)))
    (home-page "https://github.com/rstudio/bookdown")
    (synopsis "Authoring books and technical documents with R markdown")
    (description "This package provides output formats and utilities for
authoring books and technical documents with R Markdown.")
    (license license:gpl3)))

(define-public r-biocstyle
  (package
   (name "r-biocstyle")
   (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocStyle" version))
              (sha256
               (base32
                "03pp04pkcq99kdv2spzr995h2cxsza7l6w3d4gp4112m06prcybm"))))
    (properties
     `((upstream-name . "BiocStyle")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bookdown" ,r-bookdown)
       ("r-knitr" ,r-knitr)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-yaml" ,r-yaml)))
    (home-page "https://bioconductor.org/packages/BiocStyle")
    (synopsis "Bioconductor formatting styles")
    (description "This package provides standard formatting styles for
Bioconductor PDF and HTML documents.  Package vignettes illustrate use and
functionality.")
    (license license:artistic2.0)))

(define-public r-bioccheck
  (package
    (name "r-bioccheck")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocCheck" version))
              (sha256
               (base32
                "1nzp8kgw13z9pgf885rplj6k37jcldfhbz0adqclxr2gq0yalmyx"))))
    (properties
     `((upstream-name . "BiocCheck")))
    (build-system r-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; This package can be used by calling BiocCheck(<package>) from
         ;; within R, or by running R CMD BiocCheck <package>.  This phase
         ;; makes sure the latter works.  For this to work, the BiocCheck
         ;; script must be somewhere on the PATH (not the R bin directory).
         (add-after 'install 'install-bioccheck-subcommand
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dest-dir (string-append out "/bin"))
                    (script-dir
                     (string-append out "/site-library/BiocCheck/script/")))
               (mkdir-p dest-dir)
               (symlink (string-append script-dir "/checkBadDeps.R")
                        (string-append dest-dir "/checkBadDeps.R"))
               (symlink (string-append script-dir "/BiocCheck")
                        (string-append dest-dir "/BiocCheck")))
             #t)))))
    (propagated-inputs
     `(("r-codetools" ,r-codetools)
       ("r-graph" ,r-graph)
       ("r-httr" ,r-httr)
       ("r-optparse" ,r-optparse)
       ("r-biocinstaller" ,r-biocinstaller)
       ("r-biocviews" ,r-biocviews)
       ("r-stringdist" ,r-stringdist)))
    (home-page "https://bioconductor.org/packages/BiocCheck")
    (synopsis "Executes Bioconductor-specific package checks")
    (description "This package contains tools to perform additional quality
checks on R packages that are to be submitted to the Bioconductor repository.")
    (license license:artistic2.0)))

(define-public r-getopt
  (package
    (name "r-getopt")
    (version "1.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "getopt" version))
       (sha256
        (base32
         "13p35lbpy7i578752fa71sbfvcsqw5qfa9p6kf8b5m3c5p9i4v1x"))))
    (build-system r-build-system)
    (home-page "https://github.com/trevorld/getopt")
    (synopsis "Command-line option processor for R")
    (description
     "This package is designed to be used with Rscript to write shebang
scripts that accept short and long options.  Many users will prefer to
use the packages @code{optparse} or @code{argparse} which add extra
features like automatically generated help options and usage texts,
support for default values, positional argument support, etc.")
    (license license:gpl2+)))

(define-public r-optparse
  (package
    (name "r-optparse")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "optparse" version))
       (sha256
        (base32
         "1ff4wmsszrb3spwfp7ynfs8w11qpy1sdzfxm1wk8dqqvdwris7qb"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-getopt" ,r-getopt)))
    (home-page
     "https://github.com/trevorld/optparse")
    (synopsis "Command line option parser")
    (description
     "This package provides a command line parser inspired by Python's
@code{optparse} library to be used with Rscript to write shebang scripts
that accept short and long options.")
    (license license:gpl2+)))

(define-public r-dnacopy
  (package
    (name "r-dnacopy")
    (version "1.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DNAcopy" version))
              (sha256
               (base32
                "127il5rlg1hzjlhwhs64x3nm18p00q1pd9ckb2b9ifl0rax95wai"))))
    (properties
     `((upstream-name . "DNAcopy")))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://bioconductor.org/packages/DNAcopy")
    (synopsis "Implementation of a circular binary segmentation algorithm")
    (description "This package implements the circular binary segmentation (CBS)
algorithm to segment DNA copy number data and identify genomic regions with
abnormal copy number.")
    (license license:gpl2+)))

(define-public r-s4vectors
  (package
    (name "r-s4vectors")
    (version "0.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "S4Vectors" version))
              (sha256
               (base32
                "03s8vz33nl6mivjb7dbvj702dkypi340lji1sjban03fyyls0hw0"))))
    (properties
     `((upstream-name . "S4Vectors")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)))
    (home-page "https://bioconductor.org/packages/S4Vectors")
    (synopsis "S4 implementation of vectors and lists")
    (description
     "The S4Vectors package defines the @code{Vector} and @code{List} virtual
classes and a set of generic functions that extend the semantic of ordinary
vectors and lists in R.  Package developers can easily implement vector-like
or list-like objects as concrete subclasses of @code{Vector} or @code{List}.
In addition, a few low-level concrete subclasses of general interest (e.g.
@code{DataFrame}, @code{Rle}, and @code{Hits}) are implemented in the
S4Vectors package itself.")
    (license license:artistic2.0)))

(define-public r-seqinr
  (package
    (name "r-seqinr")
    (version "3.4-5")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "seqinr" version))
        (sha256
          (base32
            "17zv0n5cji17izwmwg0jcbxbjl3w5rls91w15svcnlpxjms38ahn"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ade4" ,r-ade4)
       ("r-segmented" ,r-segmented)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "http://seqinr.r-forge.r-project.org/")
    (synopsis "Biological sequences retrieval and analysis")
    (description
     "This package provides tools for exploratory data analysis and data
visualization of biological sequence (DNA and protein) data.  It also includes
utilities for sequence data management under the ACNUC system.")
    (license license:gpl2+)))

(define-public r-iranges
  (package
    (name "r-iranges")
    (version "2.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IRanges" version))
              (sha256
               (base32
                "1vqczb9wlxsmpwpqig6j1dmiblcfpq6mgnq8qwzcrvddm4cp47m5"))))
    (properties
     `((upstream-name . "IRanges")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/IRanges")
    (synopsis "Infrastructure for manipulating intervals on sequences")
    (description
     "This package provides efficient low-level and highly reusable S4 classes
for storing ranges of integers, RLE vectors (Run-Length Encoding), and, more
generally, data that can be organized sequentially (formally defined as
@code{Vector} objects), as well as views on these @code{Vector} objects.
Efficient list-like classes are also provided for storing big collections of
instances of the basic classes.  All classes in the package use consistent
naming and share the same rich and consistent \"Vector API\" as much as
possible.")
    (license license:artistic2.0)))

(define-public r-genomeinfodbdata
  (package
    (name "r-genomeinfodbdata")
    (version "0.99.1")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://bioconductor.org/packages/release/"
                                  "data/annotation/src/contrib/GenomeInfoDbData_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hipipvyvrh75n68hsjg35sxbcfzrghzxv547vnkk2f8ya99g01r"))))
    (properties
     `((upstream-name . "GenomeInfoDbData")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/GenomeInfoDbData")
    (synopsis "Species and taxonomy ID look up tables for GenomeInfoDb")
    (description "This package contains data for mapping between NCBI taxonomy
ID and species.  It is used by functions in the GenomeInfoDb package.")
    (license license:artistic2.0)))

(define-public r-genomeinfodb
  (package
    (name "r-genomeinfodb")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomeInfoDb" version))
              (sha256
               (base32
                "1jhm0imkac4gvchbjxj408aakk39xdv2fyh818d3lk295bz6bnyp"))))
    (properties
     `((upstream-name . "GenomeInfoDb")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomeinfodbdata" ,r-genomeinfodbdata)
       ("r-iranges" ,r-iranges)
       ("r-rcurl" ,r-rcurl)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/GenomeInfoDb")
    (synopsis "Utilities for manipulating chromosome identifiers")
    (description
     "This package contains data and functions that define and allow
translation between different chromosome sequence naming conventions (e.g.,
\"chr1\" versus \"1\"), including a function that attempts to place sequence
names in their natural, rather than lexicographic, order.")
    (license license:artistic2.0)))

(define-public r-edger
  (package
    (name "r-edger")
    (version "3.20.9")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "edgeR" version))
              (sha256
               (base32
                "0y52snwbz37xzdd7gihdkqczbndlfzmmypv6hri3ymjyfmlx6qaw"))))
    (properties `((upstream-name . "edgeR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-limma" ,r-limma)
       ("r-locfit" ,r-locfit)
       ("r-rcpp" ,r-rcpp)
       ("r-statmod" ,r-statmod))) ;for estimateDisp
    (home-page "http://bioinf.wehi.edu.au/edgeR")
    (synopsis "EdgeR does empirical analysis of digital gene expression data")
    (description "This package can do differential expression analysis of
RNA-seq expression profiles with biological replication.  It implements a range
of statistical methodology based on the negative binomial distributions,
including empirical Bayes estimation, exact tests, generalized linear models
and quasi-likelihood tests.  It be applied to differential signal analysis of
other types of genomic data that produce counts, including ChIP-seq, SAGE and
CAGE.")
    (license license:gpl2+)))

(define-public r-variantannotation
  (package
    (name "r-variantannotation")
    (version "1.24.5")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "VariantAnnotation" version))
              (sha256
               (base32
                "07ywn3c4w83l3sr76d0z3b1nv9icgdh3phsjlc6cfx7i6nfmvxw2"))))
    (properties
     `((upstream-name . "VariantAnnotation")))
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-dbi" ,r-dbi)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)
       ("r-zlibbioc" ,r-zlibbioc)))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/VariantAnnotation")
    (synopsis "Package for annotation of genetic variants")
    (description "This R package can annotate variants, compute amino acid
coding changes and predict coding outcomes.")
    (license license:artistic2.0)))

(define-public r-limma
  (package
    (name "r-limma")
    (version "3.34.9")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "limma" version))
              (sha256
               (base32
                "1y2fm61g5i0fn0j3l31xvwh9zww9bpkc4nwzb1d0yv1cag20jkdc"))))
    (build-system r-build-system)
    (home-page "http://bioinf.wehi.edu.au/limma")
    (synopsis "Package for linear models for microarray and RNA-seq data")
    (description "This package can be used for the analysis of gene expression
studies, especially the use of linear models for analysing designed experiments
and the assessment of differential expression.  The analysis methods apply to
different technologies, including microarrays, RNA-seq, and quantitative PCR.")
    (license license:gpl2+)))

(define-public r-xvector
  (package
    (name "r-xvector")
    (version "0.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "XVector" version))
              (sha256
               (base32
                "1i4i3kdxr78lr1kcxq657p11ybi7kq10c8kyaqyh6gfc8i9rhvmk"))))
    (properties
     `((upstream-name . "XVector")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-zlib
           (lambda _
             (substitute* "DESCRIPTION"
               (("zlibbioc, ") ""))
             (substitute* "NAMESPACE"
               (("import\\(zlibbioc\\)") ""))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/XVector")
    (synopsis "Representation and manpulation of external sequences")
    (description
     "This package provides memory efficient S4 classes for storing sequences
\"externally\" (behind an R external pointer, or on disk).")
    (license license:artistic2.0)))

(define-public r-genomicranges
  (package
    (name "r-genomicranges")
    (version "1.30.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicRanges" version))
              (sha256
               (base32
                "07cszc9ri94nzk4dffwnsj247ih6pchnrzrvnb0q4dkk33gwy8n1"))))
    (properties
     `((upstream-name . "GenomicRanges")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)))
    (home-page "https://bioconductor.org/packages/GenomicRanges")
    (synopsis "Representation and manipulation of genomic intervals")
    (description
     "This package provides tools to efficiently represent and manipulate
genomic annotations and alignments is playing a central role when it comes to
analyzing high-throughput sequencing data (a.k.a. NGS data).  The
GenomicRanges package defines general purpose containers for storing and
manipulating genomic intervals and variables defined along a genome.")
    (license license:artistic2.0)))

(define-public r-biobase
  (package
    (name "r-biobase")
    (version "2.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biobase" version))
              (sha256
               (base32
                "1cgm1ja1kp56zdlzyy9ggbkfn8r2vbsd4hncmz8g4hjd47fg18kg"))))
    (properties
     `((upstream-name . "Biobase")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)))
    (home-page "https://bioconductor.org/packages/Biobase")
    (synopsis "Base functions for Bioconductor")
    (description
     "This package provides functions that are needed by many other packages
on Bioconductor or which replace R functions.")
    (license license:artistic2.0)))

(define-public r-annotationdbi
  (package
    (name "r-annotationdbi")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationDbi" version))
              (sha256
               (base32
                "1dh4qs1a757n640gs34lf6z2glc96nan86x0sqaw5csadl2rhnlc"))))
    (properties
     `((upstream-name . "AnnotationDbi")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-dbi" ,r-dbi)
       ("r-iranges" ,r-iranges)
       ("r-rsqlite" ,r-rsqlite)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/AnnotationDbi")
    (synopsis "Annotation database interface")
    (description
     "This package provides user interface and database connection code for
annotation data packages using SQLite data storage.")
    (license license:artistic2.0)))

(define-public r-biomart
  (package
    (name "r-biomart")
    (version "2.34.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biomaRt" version))
              (sha256
               (base32
                "1zlgs2zg0lmnk572p55n7m34nkxka8w10x8f2ndssjkffl2csy79"))))
    (properties
     `((upstream-name . "biomaRt")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-httr" ,r-httr)
       ("r-progress" ,r-progress)
       ("r-rcurl" ,r-rcurl)
       ("r-stringr" ,r-stringr)
       ("r-xml" ,r-xml)))
    (home-page "https://bioconductor.org/packages/biomaRt")
    (synopsis "Interface to BioMart databases")
    (description
     "biomaRt provides an interface to a growing collection of databases
implementing the @url{BioMart software suite, http://www.biomart.org}.  The
package enables retrieval of large amounts of data in a uniform way without
the need to know the underlying database schemas or write complex SQL queries.
Examples of BioMart databases are Ensembl, COSMIC, Uniprot, HGNC, Gramene,
Wormbase and dbSNP mapped to Ensembl.  These major databases give biomaRt
users direct access to a diverse set of data and enable a wide range of
powerful online queries from gene annotation to database mining.")
    (license license:artistic2.0)))

(define-public r-biocparallel
  (package
    (name "r-biocparallel")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocParallel" version))
              (sha256
               (base32
                "13ng3n2wsgl3fh0v6jnz3vg51k5c1sh44pqdvblcrcd1qyjmmqhd"))))
    (properties
     `((upstream-name . "BiocParallel")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-futile-logger" ,r-futile-logger)
       ("r-snow" ,r-snow)
       ("r-bh" ,r-bh)))
    (home-page "https://bioconductor.org/packages/BiocParallel")
    (synopsis "Bioconductor facilities for parallel evaluation")
    (description
     "This package provides modified versions and novel implementation of
functions for parallel evaluation, tailored to use with Bioconductor
objects.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-biostrings
  (package
    (name "r-biostrings")
    (version "2.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biostrings" version))
              (sha256
               (base32
                "0vg50qdlxqcm2d6axjnzg8wh8pr4c5gz03l8bdl0llmwzp0zclzk"))))
    (properties
     `((upstream-name . "Biostrings")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)))
    (home-page "https://bioconductor.org/packages/Biostrings")
    (synopsis "String objects and algorithms for biological sequences")
    (description
     "This package provides memory efficient string containers, string
matching algorithms, and other utilities, for fast manipulation of large
biological sequences or sets of sequences.")
    (license license:artistic2.0)))

(define-public r-rsamtools
  (package
    (name "r-rsamtools")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Rsamtools" version))
              (sha256
               (base32
                "0pjny5fjvbnfdyhl3bwxin678sha2drvs00sivxh3l772cn6yams"))))
    (properties
     `((upstream-name . "Rsamtools")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-zlib
           (lambda _
             (substitute* "DESCRIPTION"
               (("zlibbioc, ") ""))
             (substitute* "NAMESPACE"
               (("import\\(zlibbioc\\)") ""))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-bitops" ,r-bitops)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)))
    (home-page "https://bioconductor.org/packages/release/bioc/html/Rsamtools.html")
    (synopsis "Interface to samtools, bcftools, and tabix")
    (description
     "This package provides an interface to the 'samtools', 'bcftools', and
'tabix' utilities for manipulating SAM (Sequence Alignment / Map), FASTA,
binary variant call (BCF) and compressed indexed tab-delimited (tabix)
files.")
    (license license:expat)))

(define-public r-delayedarray
  (package
    (name "r-delayedarray")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DelayedArray" version))
              (sha256
               (base32
                "0s7h2giyvz04cg6248kbbzpwhxdrpnsvl2s8k5c8ricisd9aaz4b"))))
    (properties
     `((upstream-name . "DelayedArray")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-s4vectors" ,r-s4vectors)
       ("r-iranges" ,r-iranges)
       ("r-matrixstats" ,r-matrixstats)))
    (home-page "https://bioconductor.org/packages/DelayedArray")
    (synopsis "Delayed operations on array-like objects")
    (description
     "Wrapping an array-like object (typically an on-disk object) in a
@code{DelayedArray} object allows one to perform common array operations on it
without loading the object in memory.  In order to reduce memory usage and
optimize performance, operations on the object are either delayed or executed
using a block processing mechanism.  Note that this also works on in-memory
array-like objects like @code{DataFrame} objects (typically with Rle columns),
@code{Matrix} objects, and ordinary arrays and data frames.")
    (license license:artistic2.0)))

(define-public r-summarizedexperiment
  (package
    (name "r-summarizedexperiment")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SummarizedExperiment" version))
              (sha256
               (base32
                "19vlwnby83fqjrilsxvnvgz0gvby7mrxvlmx18nb3p1w591ddfjh"))))
    (properties
     `((upstream-name . "SummarizedExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-delayedarray" ,r-delayedarray)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/SummarizedExperiment")
    (synopsis "Container for representing genomic ranges by sample")
    (description
     "The SummarizedExperiment container contains one or more assays, each
represented by a matrix-like object of numeric or other mode.  The rows
typically represent genomic ranges of interest and the columns represent
samples.")
    (license license:artistic2.0)))

(define-public r-genomicalignments
  (package
    (name "r-genomicalignments")
    (version "1.14.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicAlignments" version))
              (sha256
               (base32
                "033p6fw46sn7w2yyn14nb9qcnkf30cl0nv6zh014ixflm3iifz39"))))
    (properties
     `((upstream-name . "GenomicAlignments")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/GenomicAlignments")
    (synopsis "Representation and manipulation of short genomic alignments")
    (description
     "This package provides efficient containers for storing and manipulating
short genomic alignments (typically obtained by aligning short reads to a
reference genome).  This includes read counting, computing the coverage,
junction detection, and working with the nucleotide content of the
alignments.")
    (license license:artistic2.0)))

(define-public r-rtracklayer
  (package
    (name "r-rtracklayer")
    (version "1.38.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rtracklayer" version))
              (sha256
               (base32
                "1khzfczm35k5lq9h0jlqrq01192spzjyh8s6is89spj006flwn4k"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-zlib
           (lambda _
             (substitute* "DESCRIPTION"
               ((" zlibbioc,") ""))
             (substitute* "NAMESPACE"
               (("import\\(zlibbioc\\)") ""))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rcurl" ,r-rcurl)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xml" ,r-xml)
       ("r-xvector" ,r-xvector)))
    (home-page "https://bioconductor.org/packages/rtracklayer")
    (synopsis "R interface to genome browsers and their annotation tracks")
    (description
     "rtracklayer is an extensible framework for interacting with multiple
genome browsers (currently UCSC built-in) and manipulating annotation tracks
in various formats (currently GFF, BED, bedGraph, BED15, WIG, BigWig and 2bit
built-in).  The user may export/import tracks to/from the supported browsers,
as well as query and modify the browser state, such as the current viewport.")
    (license license:artistic2.0)))

(define-public r-genomicfeatures
  (package
    (name "r-genomicfeatures")
    (version "1.30.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicFeatures" version))
              (sha256
               (base32
                "010vn8hlwbnw12pd1d8pv6m12yp3xwx557gba5rbjq9p4qypnn3z"))))
    (properties
     `((upstream-name . "GenomicFeatures")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biomart" ,r-biomart)
       ("r-biostrings" ,r-biostrings)
       ("r-dbi" ,r-dbi)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rcurl" ,r-rcurl)
       ("r-rsqlite" ,r-rsqlite)
       ("r-rmysql" ,r-rmysql)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)))
    (home-page "https://bioconductor.org/packages/GenomicFeatures")
    (synopsis "Tools for working with transcript centric annotations")
    (description
     "This package provides a set of tools and methods for making and
manipulating transcript centric annotations.  With these tools the user can
easily download the genomic locations of the transcripts, exons and cds of a
given organism, from either the UCSC Genome Browser or a BioMart
database (more sources will be supported in the future).  This information is
then stored in a local database that keeps track of the relationship between
transcripts, exons, cds and genes.  Flexible methods are provided for
extracting the desired features in a convenient format.")
    (license license:artistic2.0)))

(define-public r-go-db
  (package
    (name "r-go-db")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/GO.db_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "02d1mn1al3q7qvhx1ylrr3ar4w4iw0qyi5d89v2336rzwk9maq35"))))
    (properties
     `((upstream-name . "GO.db")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://bioconductor.org/packages/GO.db")
    (synopsis "Annotation maps describing the entire Gene Ontology")
    (description
     "The purpose of this GO.db annotation package is to provide detailed
information about the latest version of the Gene Ontologies.")
    (license license:artistic2.0)))

(define-public r-graph
  (package
    (name "r-graph")
    (version "1.56.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "graph" version))
              (sha256
               (base32
                "15aajjp8h2z14p80c8hyd4rrmr9vqsm7bvwb989jxjl4k6g52an1"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)))
    (home-page "https://bioconductor.org/packages/graph")
    (synopsis "Handle graph data structures in R")
    (description
     "This package implements some simple graph handling capabilities for R.")
    (license license:artistic2.0)))

(define-public r-topgo
  (package
    (name "r-topgo")
    (version "2.30.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "topGO" version))
              (sha256
               (base32
                "1cgz4knxr328xfqlhl6ypxl6x86rfrlqz748kn94ainxjzz55i6x"))))
    (properties
     `((upstream-name . "topGO")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-dbi" ,r-dbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-go-db" ,r-go-db)
       ("r-graph" ,r-graph)
       ("r-lattice" ,r-lattice)
       ("r-matrixstats" ,r-matrixstats)
       ("r-sparsem" ,r-sparsem)))
    (home-page "https://bioconductor.org/packages/topGO")
    (synopsis "Enrichment analysis for gene ontology")
    (description
     "The topGO package provides tools for testing @dfn{gene ontology} (GO)
terms while accounting for the topology of the GO graph.  Different test
statistics and different methods for eliminating local similarities and
dependencies between GO terms can be implemented and applied.")
    ;; Any version of the LGPL applies.
    (license license:lgpl2.1+)))

(define-public r-bsgenome
  (package
    (name "r-bsgenome")
    (version "1.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome" version))
              (sha256
               (base32
                "1jbzq7lm2iajajn2bifxnkss0k9fdvgqr30mral17cbhp5f6w4lq"))))
    (properties
     `((upstream-name . "BSgenome")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)))
    (home-page "https://bioconductor.org/packages/BSgenome")
    (synopsis "Infrastructure for Biostrings-based genome data packages")
    (description
     "This package provides infrastructure shared by all Biostrings-based
genome data packages and support for efficient SNP representation.")
    (license license:artistic2.0)))

(define-public r-bsgenome-hsapiens-1000genomes-hs37d5
  (package
    (name "r-bsgenome-hsapiens-1000genomes-hs37d5")
    (version "0.99.1")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "BSgenome.Hsapiens.1000genomes.hs37d5_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1cg0g5fqmsvwyw2p9hp2yy4ilk21jkbbrnpgqvb5c36ihjwvc7sr"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.1000genomes.hs37d5")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Hsapiens.1000genomes.hs37d5/")
    (synopsis "Full genome sequences for Homo sapiens")
    (description
     "This package provides full genome sequences for Homo sapiens from
1000genomes phase2 reference genome sequence (hs37d5), based on NCBI GRCh37.")
    (license license:artistic2.0)))

(define-public r-impute
  (package
    (name "r-impute")
    (version "1.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "impute" version))
              (sha256
               (base32
                "0b8r4swvyx3cjcc2ky8yn0ncpzlbi1pgfsn3wpbjmhh7sqrffm2n"))))
    (inputs
     `(("gfortran" ,gfortran)))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/impute")
    (synopsis "Imputation for microarray data")
    (description
     "This package provides a function to impute missing gene expression
microarray data, using nearest neighbor averaging.")
    (license license:gpl2+)))

(define-public r-seqpattern
  (package
    (name "r-seqpattern")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "seqPattern" version))
              (sha256
               (base32
                "1kcm5w83q7w0v0vs7nyp4gq5z86c6n6pqy9zmyyhxcrns7f597pm"))))
    (properties
     `((upstream-name . "seqPattern")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-kernsmooth" ,r-kernsmooth)
       ("r-plotrix" ,r-plotrix)))
    (home-page "https://bioconductor.org/packages/seqPattern")
    (synopsis "Visualising oligonucleotide patterns and motif occurrences")
    (description
     "This package provides tools to visualize oligonucleotide patterns and
sequence motif occurrences across a large set of sequences centred at a common
reference point and sorted by a user defined feature.")
    (license license:gpl3+)))

(define-public r-genomation
  (package
    (name "r-genomation")
    (version "1.11.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "genomation" version))
              (sha256
               (base32
                "1d2g1v6xhrf3gm86pv8ln22df5g6v6k6i4i39v4j82zn4apany6v"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-data-table" ,r-data-table)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridbase" ,r-gridbase)
       ("r-impute" ,r-impute)
       ("r-iranges" ,r-iranges)
       ("r-matrixstats" ,r-matrixstats)
       ("r-plotrix" ,r-plotrix)
       ("r-plyr" ,r-plyr)
       ("r-rcpp" ,r-rcpp)
       ("r-readr" ,r-readr)
       ("r-reshape2" ,r-reshape2)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-runit" ,r-runit)
       ("r-s4vectors" ,r-s4vectors)
       ("r-seqpattern" ,r-seqpattern)))
    (home-page "http://bioinformatics.mdc-berlin.de/genomation/")
    (synopsis "Summary, annotation and visualization of genomic data")
    (description
     "This package provides a package for summary and annotation of genomic
intervals.  Users can visualize and quantify genomic intervals over
pre-defined functional regions, such as promoters, exons, introns, etc.  The
genomic intervals represent regions with a defined chromosome position, which
may be associated with a score, such as aligned reads from HT-seq experiments,
TF binding sites, methylation scores, etc.  The package can use any tabular
genomic feature data as long as it has minimal information on the locations of
genomic intervals.  In addition, it can use BAM or BigWig files as input.")
    (license license:artistic2.0)))

(define-public r-genomationdata
  (package
    (name "r-genomationdata")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://bioconductor.org/packages/"
                                  "release/data/experiment/src/contrib/"
                                  "genomationData_" version ".tar.gz"))
              (sha256
               (base32
                "0h7g5x3kyb50qlblz5hc85lfm6n6f5nb68i146way3ggs04sqvla"))))
    (build-system r-build-system)
    ;; As this package provides little more than large data files, it doesn't
    ;; make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "http://bioinformatics.mdc-berlin.de/genomation/")
    (synopsis "Experimental data for use with the genomation package")
    (description
     "This package contains experimental genetic data for use with the
genomation package.  Included are Chip Seq, Methylation and Cage data,
downloaded from Encode.")
    (license license:gpl3+)))

(define-public r-org-hs-eg-db
  (package
    (name "r-org-hs-eg-db")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "org.Hs.eg.db_" version ".tar.gz"))
              (sha256
               (base32
                "1v6wa5613cjq59xd7x1qz8lr9nb2abm9abl2cci1khrnrlpla927"))))
    (properties
     `((upstream-name . "org.Hs.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://www.bioconductor.org/packages/org.Hs.eg.db/")
    (synopsis "Genome wide annotation for Human")
    (description
     "This package contains genome-wide annotations for Human, primarily based
on mapping using Entrez Gene identifiers.")
    (license license:artistic2.0)))

(define-public r-org-ce-eg-db
  (package
    (name "r-org-ce-eg-db")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "org.Ce.eg.db_" version ".tar.gz"))
              (sha256
               (base32
                "02ggchixlmzywhsbr0h2ms4dravv7m5964cjxqcjxqs16vjwlbk9"))))
    (properties
     `((upstream-name . "org.Ce.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://www.bioconductor.org/packages/org.Ce.eg.db/")
    (synopsis "Genome wide annotation for Worm")
    (description
     "This package provides mappings from Entrez gene identifiers to various
annotations for the genome of the model worm Caenorhabditis elegans.")
    (license license:artistic2.0)))

(define-public r-org-dm-eg-db
  (package
    (name "r-org-dm-eg-db")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "org.Dm.eg.db_" version ".tar.gz"))
              (sha256
               (base32
                "033qak1d3wwz17va0bh8z8p8arx0aw2va6gm1qfwsvdkj9cd9d7d"))))
    (properties
     `((upstream-name . "org.Dm.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://www.bioconductor.org/packages/org.Dm.eg.db/")
    (synopsis "Genome wide annotation for Fly")
    (description
     "This package provides mappings from Entrez gene identifiers to various
annotations for the genome of the model fruit fly Drosophila melanogaster.")
    (license license:artistic2.0)))

(define-public r-org-mm-eg-db
  (package
    (name "r-org-mm-eg-db")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "org.Mm.eg.db_" version ".tar.gz"))
              (sha256
               (base32
                "11q21p3ki4bn4hb3aix0g775l45l66jmas6m94nfhqqnpjhv4d6g"))))
    (properties
     `((upstream-name . "org.Mm.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://www.bioconductor.org/packages/org.Mm.eg.db/")
    (synopsis "Genome wide annotation for Mouse")
    (description
     "This package provides mappings from Entrez gene identifiers to various
annotations for the genome of the model mouse Mus musculus.")
    (license license:artistic2.0)))

(define-public r-seqlogo
  (package
    (name "r-seqlogo")
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqLogo" version))
       (sha256
        (base32
         "1ql4q4vx0j61a893dqc3c8zxmgs8sqhy3j1qhyfdvbd01vw9w1kq"))))
    (properties `((upstream-name . "seqLogo")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/seqLogo")
    (synopsis "Sequence logos for DNA sequence alignments")
    (description
     "seqLogo takes the position weight matrix of a DNA sequence motif and
plots the corresponding sequence logo as introduced by Schneider and
Stephens (1990).")
    (license license:lgpl2.0+)))

(define-public r-bsgenome-hsapiens-ucsc-hg19
  (package
    (name "r-bsgenome-hsapiens-ucsc-hg19")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "BSgenome.Hsapiens.UCSC.hg19_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1y0nqpk8cw5a34sd9hmin3z4v7iqm6hf6l22cl81vlbxqbjibxc8"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.UCSC.hg19")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Hsapiens.UCSC.hg19/")
    (synopsis "Full genome sequences for Homo sapiens")
    (description
     "This package provides full genome sequences for Homo sapiens as provided
by UCSC (hg19, February 2009) and stored in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-mmusculus-ucsc-mm9
  (package
    (name "r-bsgenome-mmusculus-ucsc-mm9")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "BSgenome.Mmusculus.UCSC.mm9_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1birqw30g2azimxpnjfzmkphan7x131yy8b9h85lfz5fjdg7841i"))))
    (properties
     `((upstream-name . "BSgenome.Mmusculus.UCSC.mm9")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Mmusculus.UCSC.mm9/")
    (synopsis "Full genome sequences for Mouse")
    (description
     "This package provides full genome sequences for Mus musculus (Mouse) as
provided by UCSC (mm9, July 2007) and stored in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-mmusculus-ucsc-mm10
  (package
    (name "r-bsgenome-mmusculus-ucsc-mm10")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "BSgenome.Mmusculus.UCSC.mm10_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "12s0nm2na9brjad4rn9l7d3db2aj8qa1xvz0y1k7gk08wayb6bkf"))))
    (properties
     `((upstream-name . "BSgenome.Mmusculus.UCSC.mm10")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Mmusculus.UCSC.mm10/")
    (synopsis "Full genome sequences for Mouse")
    (description
     "This package provides full genome sequences for Mus
musculus (Mouse) as provided by UCSC (mm10, December 2011) and stored
in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-txdb-mmusculus-ucsc-mm10-knowngene
  (package
    (name "r-txdb-mmusculus-ucsc-mm10-knowngene")
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "TxDb.Mmusculus.UCSC.mm10.knownGene_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "08gava9wsvpcqz51k2sni3pj03n5155v32d9riqbf305nbirqbkb"))))
    (properties
     `((upstream-name . "TxDb.Mmusculus.UCSC.mm10.knownGene")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-annotationdbi" ,r-annotationdbi)))
    (home-page
     "https://bioconductor.org/packages/TxDb.Mmusculus.UCSC.mm10.knownGene/")
    (synopsis "Annotation package for TxDb knownGene object(s) for Mouse")
    (description
     "This package loads a TxDb object, which is an R interface to
prefabricated databases contained in this package.  This package provides
the TxDb object of Mouse data as provided by UCSC (mm10, December 2011)
based on the knownGene track.")
    (license license:artistic2.0)))

(define-public r-bsgenome-celegans-ucsc-ce6
  (package
    (name "r-bsgenome-celegans-ucsc-ce6")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "BSgenome.Celegans.UCSC.ce6_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0mqzb353xv2c3m3vkb315dkmnxkgczp7ndnknyhpgjlybyf715v9"))))
    (properties
     `((upstream-name . "BSgenome.Celegans.UCSC.ce6")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Celegans.UCSC.ce6/")
    (synopsis "Full genome sequences for Worm")
    (description
     "This package provides full genome sequences for Caenorhabditis
elegans (Worm) as provided by UCSC (ce6, May 2008) and stored in Biostrings
objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-celegans-ucsc-ce10
  (package
    (name "r-bsgenome-celegans-ucsc-ce10")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "BSgenome.Celegans.UCSC.ce10_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1zaym97jk4npxk14ifvwz2rvhm4zx9xgs33r9vvx9rlynp0gydrk"))))
    (properties
     `((upstream-name . "BSgenome.Celegans.UCSC.ce10")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Celegans.UCSC.ce10/")
    (synopsis "Full genome sequences for Worm")
    (description
     "This package provides full genome sequences for Caenorhabditis
elegans (Worm) as provided by UCSC (ce10, Oct 2010) and stored in Biostrings
objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-dmelanogaster-ucsc-dm3
  (package
    (name "r-bsgenome-dmelanogaster-ucsc-dm3")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "BSgenome.Dmelanogaster.UCSC.dm3_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "19bm3lkhhkag3gnwp419211fh0cnr0x6fa0r1lr0ycwrikxdxsv8"))))
    (properties
     `((upstream-name . "BSgenome.Dmelanogaster.UCSC.dm3")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Dmelanogaster.UCSC.dm3/")
    (synopsis "Full genome sequences for Fly")
    (description
     "This package provides full genome sequences for Drosophila
melanogaster (Fly) as provided by UCSC (dm3, April 2006) and stored in
Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-motifrg
  (package
    (name "r-motifrg")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifRG" version))
       (sha256
        (base32
         "193zl2rlzwxv9p9q5i7rilj3w05ndqfyp9bdpvagp5s5cin4hf44"))))
    (properties `((upstream-name . "motifRG")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-iranges" ,r-iranges)
       ("r-seqlogo" ,r-seqlogo)
       ("r-xvector" ,r-xvector)))
    (home-page "https://bioconductor.org/packages/motifRG")
    (synopsis "Discover motifs in high throughput sequencing data")
    (description
     "This package provides tools for discriminative motif discovery in high
throughput genetic sequencing data sets using regression methods.")
    (license license:artistic2.0)))

(define-public r-qtl
 (package
  (name "r-qtl")
  (version "1.42-8")
  (source
   (origin
    (method url-fetch)
    (uri (string-append "mirror://cran/src/contrib/qtl_"
                        version ".tar.gz"))
    (sha256
     (base32
      "1l528dwvfpdlr05imrrm4rq32axp6hld9nqm6mm43kn5n7z2f5k6"))))
  (build-system r-build-system)
  (home-page "http://rqtl.org/")
  (synopsis "R package for analyzing QTL experiments in genetics")
  (description "R/qtl is an extension library for the R statistics
system.  It is used to analyze experimental crosses for identifying
genes contributing to variation in quantitative traits (so-called
quantitative trait loci, QTLs).

Using a hidden Markov model, R/qtl allows to estimate genetic maps, to
identify genotyping errors, and to perform single-QTL and two-QTL,
two-dimensional genome scans.")
  (license license:gpl3)))

(define-public r-zlibbioc
  (package
    (name "r-zlibbioc")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "zlibbioc" version))
              (sha256
               (base32
                "1zr9hbh55hglfpy15cpxwmddxblhyb0an15953l3rbhmlh2vpy92"))))
    (properties
     `((upstream-name . "zlibbioc")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/zlibbioc")
    (synopsis "Provider for zlib-1.2.5 to R packages")
    (description "This package uses the source code of zlib-1.2.5 to create
libraries for systems that do not have these available via other means.")
    (license license:artistic2.0)))

(define-public r-r4rna
  (package
    (name "r-r4rna")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.e-rna.org/r-chie/files/R4RNA_"
                           version ".tar.gz"))
       (sha256
        (base32
         "1p0i78wh76jfgmn9jphbwwaz6yy6pipzfg08xs54cxavxg2j81p5"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-optparse" ,r-optparse)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page "http://www.e-rna.org/r-chie/index.cgi")
    (synopsis "Analysis framework for RNA secondary structure")
    (description
     "The R4RNA package aims to be a general framework for the analysis of RNA
secondary structure and comparative analysis in R.")
    (license license:gpl3+)))

(define-public r-rhtslib
  (package
    (name "r-rhtslib")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhtslib" version))
       (sha256
        (base32
         "1dw3p44bfr0m7w39ckc2k37sjcp1zz0b9g12mr8am15jaj6v0q2j"))))
    (properties `((upstream-name . "Rhtslib")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-zlibbioc" ,r-zlibbioc)))
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("autoconf" ,autoconf)))
    (home-page "https://github.com/nhayden/Rhtslib")
    (synopsis "High-throughput sequencing library as an R package")
    (description
     "This package provides the HTSlib C library for high-throughput
nucleotide sequence analysis.  The package is primarily useful to developers
of other R packages who wish to make use of HTSlib.")
    (license license:lgpl2.0+)))

(define-public r-bamsignals
  (package
    (name "r-bamsignals")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bamsignals" version))
       (sha256
        (base32
         "15id6mkj95skb4kfafvfs2j7ylydal60c3pspcl7llhwpq6vcqvl"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rcpp" ,r-rcpp)
       ("r-rhtslib" ,r-rhtslib)
       ("r-zlibbioc" ,r-zlibbioc)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://bioconductor.org/packages/bamsignals")
    (synopsis "Extract read count signals from bam files")
    (description
     "This package allows to efficiently obtain count vectors from indexed bam
files.  It counts the number of nucleotide sequence reads in given genomic
ranges and it computes reads profiles and coverage profiles.  It also handles
paired-end data.")
    (license license:gpl2+)))

(define-public r-rcas
  (package
    (name "r-rcas")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/RCAS/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qgc7vi6fpzl440yg7jhiycg5q336kd4pxqzx10yx2zcq3bq3msg"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)
       ("r-testthat" ,r-testthat)
       ;; During vignette building knitr checks that "pandoc-citeproc"
       ;; is in the PATH.
       ("ghc-pandoc-citeproc" ,ghc-pandoc-citeproc)))
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-biomart" ,r-biomart)
       ("r-org-hs-eg-db" ,r-org-hs-eg-db)
       ("r-org-ce-eg-db" ,r-org-ce-eg-db)
       ("r-org-dm-eg-db" ,r-org-dm-eg-db)
       ("r-org-mm-eg-db" ,r-org-mm-eg-db)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-bsgenome-mmusculus-ucsc-mm9" ,r-bsgenome-mmusculus-ucsc-mm9)
       ("r-bsgenome-celegans-ucsc-ce10" ,r-bsgenome-celegans-ucsc-ce10)
       ("r-bsgenome-dmelanogaster-ucsc-dm3" ,r-bsgenome-dmelanogaster-ucsc-dm3)
       ("r-topgo" ,r-topgo)
       ("r-dt" ,r-dt)
       ("r-pbapply" ,r-pbapply)
       ("r-plotly" ,r-plotly)
       ("r-plotrix" ,r-plotrix)
       ("r-motifrg" ,r-motifrg)
       ("r-genomation" ,r-genomation)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rmarkdown" ,r-rmarkdown)))
    (synopsis "RNA-centric annotation system")
    (description
     "RCAS aims to be a standalone RNA-centric annotation system that provides
intuitive reports and publication-ready graphics.  This package provides the R
library implementing most of the pipeline's features.")
    (home-page "https://github.com/BIMSBbioinfo/RCAS")
    (license license:artistic2.0)))

(define-public rcas-web
  (package
    (name "rcas-web")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/BIMSBbioinfo/rcas-web/"
                           "releases/download/v" version
                           "/rcas-web-" version ".tar.gz"))
       (sha256
        (base32
         "1p16frfys41a8yaa4gkm457nzkqhqs2pc3lkac0ds457w9w5j1gm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (json   (assoc-ref inputs "guile-json"))
                    (redis  (assoc-ref inputs "guile-redis"))
                    (path   (string-append
                             json  "/share/guile/site/2.2:"
                             redis "/share/guile/site/2.2")))
               (wrap-program (string-append out "/bin/rcas-web")
                 `("GUILE_LOAD_PATH" ":" = (,path))
                 `("GUILE_LOAD_COMPILED_PATH" ":" = (,path))
                 `("R_LIBS_SITE" ":" = (,(getenv "R_LIBS_SITE")))))
             #t)))))
    (inputs
     `(("r-minimal" ,r-minimal)
       ("r-rcas" ,r-rcas)
       ("guile-next" ,guile-2.2)
       ("guile-json" ,guile-json)
       ("guile-redis" ,guile2.2-redis)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/BIMSBbioinfo/rcas-web")
    (synopsis "Web interface for RNA-centric annotation system (RCAS)")
    (description "This package provides a simple web interface for the
@dfn{RNA-centric annotation system} (RCAS).")
    (license license:agpl3+)))

(define-public r-mutationalpatterns
  (package
    (name "r-mutationalpatterns")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MutationalPatterns" version))
       (sha256
        (base32
         "08ay9h5cqsi8ypb6r0g4rfa5l1g06jgfzl64wmhgz134yqbl7vfv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome-hsapiens-1000g" ,r-bsgenome-hsapiens-1000genomes-hs37d5)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-genomicranges" ,r-genomicranges)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-iranges" ,r-iranges)
       ("r-nmf" ,r-nmf)
       ("r-plyr" ,r-plyr)
       ("r-pracma" ,r-pracma)
       ("r-reshape2" ,r-reshape2)
       ("r-cowplot" ,r-cowplot)
       ("r-ggdendro" ,r-ggdendro)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://bioconductor.org/packages/MutationalPatterns/")
    (synopsis "Extract and visualize mutational patterns in genomic data")
    (description "This package provides an extensive toolset for the
characterization and visualization of a wide range of mutational patterns
in SNV base substitution data.")
    (license license:expat)))

(define-public r-wgcna
  (package
    (name "r-wgcna")
    (version "1.63")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "WGCNA" version))
       (sha256
        (base32
         "1225dqm68bynkmklnsxdqdd3zqrpzbvqwyly8ibxmk75z33xz309"))))
    (properties `((upstream-name . "WGCNA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-doparallel" ,r-doparallel)
       ("r-dynamictreecut" ,r-dynamictreecut)
       ("r-fastcluster" ,r-fastcluster)
       ("r-foreach" ,r-foreach)
       ("r-go-db" ,r-go-db)
       ("r-hmisc" ,r-hmisc)
       ("r-impute" ,r-impute)
       ("r-rcpp" ,r-rcpp)
       ("r-robust" ,r-robust)
       ("r-survival" ,r-survival)
       ("r-matrixstats" ,r-matrixstats)
       ("r-preprocesscore" ,r-preprocesscore)))
    (home-page
     "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/Rpackages/WGCNA/")
    (synopsis "Weighted correlation network analysis")
    (description
     "This package provides functions necessary to perform Weighted
Correlation Network Analysis on high-dimensional data.  It includes functions
for rudimentary data cleaning, construction and summarization of correlation
networks, module identification and functions for relating both variables and
modules to sample traits.  It also includes a number of utility functions for
data manipulation and visualization.")
    (license license:gpl2+)))

(define-public r-chipkernels
  (let ((commit "c9cfcacb626b1221094fb3490ea7bac0fd625372")
        (revision "1"))
    (package
      (name "r-chipkernels")
      (version (string-append "1.1-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ManuSetty/ChIPKernels.git")
               (commit commit)))
         (file-name (string-append name "-" version))
         (sha256
          (base32
           "14bj5qhjm1hsm9ay561nfbqi9wxsa7y487df2idsaaf6z10nw4v0"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-iranges" ,r-iranges)
         ("r-xvector" ,r-xvector)
         ("r-biostrings" ,r-biostrings)
         ("r-bsgenome" ,r-bsgenome)
         ("r-gtools" ,r-gtools)
         ("r-genomicranges" ,r-genomicranges)
         ("r-sfsmisc" ,r-sfsmisc)
         ("r-kernlab" ,r-kernlab)
         ("r-s4vectors" ,r-s4vectors)
         ("r-biocgenerics" ,r-biocgenerics)))
      (home-page "https://github.com/ManuSetty/ChIPKernels")
      (synopsis "Build string kernels for DNA Sequence analysis")
      (description "ChIPKernels is an R package for building different string
kernels used for DNA Sequence analysis.  A dictionary of the desired kernel
must be built and this dictionary can be used for determining kernels for DNA
Sequences.")
      (license license:gpl2+))))

(define-public r-seqgl
  (package
    (name "r-seqgl")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ManuSetty/SeqGL/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0pnk1p3sci5yipyc8xnb6jbmydpl80fld927xgnbcv104hy8h8yh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-chipkernels" ,r-chipkernels)
       ("r-genomicranges" ,r-genomicranges)
       ("r-spams" ,r-spams)
       ("r-wgcna" ,r-wgcna)
       ("r-fastcluster" ,r-fastcluster)))
    (home-page "https://github.com/ManuSetty/SeqGL")
    (synopsis "Group lasso for Dnase/ChIP-seq data")
    (description "SeqGL is a group lasso based algorithm to extract
transcription factor sequence signals from ChIP, DNase and ATAC-seq profiles.
This package presents a method which uses group lasso to discriminate between
bound and non bound genomic regions to accurately identify transcription
factors bound at the specific regions.")
    (license license:gpl2+)))

(define-public r-gkmsvm
  (package
    (name "r-gkmsvm")
    (version "0.79.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gkmSVM" version))
       (sha256
        (base32
         "04dakbgfvfalz4rm4fvvybp506dn5fbj5g86ybfhrc6wywjllsz3"))))
    (properties `((upstream-name . "gkmSVM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-kernlab" ,r-kernlab)
       ("r-rcpp" ,r-rcpp)
       ("r-rocr" ,r-rocr)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-seqinr" ,r-seqinr)))
    (home-page "https://cran.r-project.org/web/packages/gkmSVM")
    (synopsis "Gapped-kmer support vector machine")
    (description
     "This R package provides tools for training gapped-kmer SVM classifiers
for DNA and protein sequences.  This package supports several sequence
kernels, including: gkmSVM, kmer-SVM, mismatch kernel and wildcard kernel.")
    (license license:gpl2+)))

(define-public r-tximport
  (package
    (name "r-tximport")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tximport" version))
              (sha256
               (base32
                "1gyqcm91hxg1kgjqcz2qw1n56yp9pymjzs50rwcpb2893dr8sp2h"))))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/tximport")
    (synopsis "Import and summarize transcript-level estimates for gene-level analysis")
    (description
     "This package provides tools to import transcript-level abundance,
estimated counts and transcript lengths, and to summarize them into matrices
for use with downstream gene-level analysis packages.  Average transcript
length, weighted by sample-specific transcript abundance estimates, is
provided as a matrix which can be used as an offset for different expression
of gene-level counts.")
    (license license:gpl2+)))

(define-public r-rhdf5
  (package
    (name "r-rhdf5")
    (version "2.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rhdf5" version))
              (sha256
               (base32
                "145858qg1xan6imxcbprzq3yn3mdf532aahdr6cibvdjg47hs4c1"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-smallhdf5
           (lambda* (#:key outputs #:allow-other-keys)
             (system* "tar" "-xzvf"
                      "src/hdf5source/hdf5small.tgz" "-C" "src/" )
             (substitute* "src/hdf5/configure"
               (("/bin/mv") "mv"))
             ;; Remove timestamp and host system information to make
             ;; the build reproducible.
             (substitute* "src/hdf5/src/libhdf5.settings.in"
               (("Configured on: @CONFIG_DATE@")
                "Configured on: Guix")
               (("Uname information:.*")
                "Uname information: Linux\n")
               ;; Remove unnecessary store reference.
               (("C Compiler:.*")
                "C Compiler: GCC\n"))
             #t)))))
    (propagated-inputs
     `(("r-zlibbioc" ,r-zlibbioc)))
    (inputs
     `(("perl" ,perl)
       ("zlib" ,zlib)))
    (home-page "https://bioconductor.org/packages/rhdf5")
    (synopsis "HDF5 interface to R")
    (description
     "This R/Bioconductor package provides an interface between HDF5 and R.
HDF5's main features are the ability to store and access very large and/or
complex datasets and a wide variety of metadata on mass storage (disk) through
a completely portable file format.  The rhdf5 package is thus suited for the
exchange of large and/or complex datasets between R and other software
package, and for letting R applications work on datasets that are larger than
the available RAM.")
    (license license:artistic2.0)))

(define-public r-annotationfilter
  (package
    (name "r-annotationfilter")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationFilter" version))
              (sha256
               (base32
                "04zf864c1fvdlaay2r5cn30fc1n5i3czh31fs62qlrvs61wjiscs"))))
    (properties
     `((upstream-name . "AnnotationFilter")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomicranges" ,r-genomicranges)
       ("r-lazyeval" ,r-lazyeval)))
    (home-page "https://github.com/Bioconductor/AnnotationFilter")
    (synopsis "Facilities for filtering Bioconductor annotation resources")
    (description
     "This package provides classes and other infrastructure to implement
filters for manipulating Bioconductor annotation resources.  The filters are
used by @code{ensembldb}, @code{Organism.dplyr}, and other packages.")
    (license license:artistic2.0)))

(define-public emboss
  (package
    (name "emboss")
    (version "6.5.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://emboss.open-bio.org/pub/EMBOSS/old/"
                                  (version-major+minor version) ".0/"
                                  "EMBOSS-" version ".tar.gz"))
              (sha256
               (base32
                "0vsmz96gc411yj2iyzdrsmg4l2n1nhgmp7vrgzlxx3xixv9xbf0q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-hpdf="
                            (assoc-ref %build-inputs "libharu")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-checks
           (lambda _
             ;; The PNGDRIVER tests check for the presence of libgd, libpng
             ;; and zlib, but assume that they are all found at the same
             ;; prefix.
             (substitute* "configure.in"
               (("CHECK_PNGDRIVER")
                "LIBS=\"$LIBS -lgd -lpng -lz -lm\"
AC_DEFINE([PLD_png], [1], [Define to 1 if PNG support is available])
AM_CONDITIONAL(AMPNG, true)"))
             #t))
         (add-after 'fix-checks 'disable-update-check
           (lambda _
             ;; At build time there is no connection to the Internet, so
             ;; looking for updates will not work.
             (substitute* "Makefile.am"
               (("\\$\\(bindir\\)/embossupdate") ""))
             #t))
         (add-after 'disable-update-check 'autogen
           (lambda _ (zero? (system* "autoreconf" "-vif")))))))
    (inputs
     `(("perl" ,perl)
       ("libpng" ,libpng)
       ("gd" ,gd)
       ("libx11" ,libx11)
       ("libharu" ,libharu)
       ("zlib" ,zlib)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://emboss.sourceforge.net")
    (synopsis "Molecular biology analysis suite")
    (description "EMBOSS is the \"European Molecular Biology Open Software
Suite\".  EMBOSS is an analysis package specially developed for the needs of
the molecular biology (e.g. EMBnet) user community.  The software
automatically copes with data in a variety of formats and even allows
transparent retrieval of sequence data from the web.  It also provides a
number of libraries for the development of software in the field of molecular
biology.  EMBOSS also integrates a range of currently available packages and
tools for sequence analysis into a seamless whole.")
    (license license:gpl2+)))

(define-public bits
  (let ((revision "1")
        (commit "3cc4567896d9d6442923da944beb704750a08d2d"))
    (package
      (name "bits")
      ;; The version is 2.13.0 even though no release archives have been
      ;; published as yet.
      (version (string-append "2.13.0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/arq5x/bits.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "17n2kffk4kmhivd8c98g2vr6y1s23vbg4sxlxs689wni66797hbs"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ;no tests included
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'remove-cuda
             (lambda _
               (substitute* "Makefile"
                 ((".*_cuda") "")
                 (("(bits_test_intersections) \\\\" _ match) match))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively
                "bin" (string-append (assoc-ref outputs "out") "/bin"))
               #t)))))
      (inputs
       `(("gsl" ,gsl)
         ("zlib" ,zlib)))
      (home-page "https://github.com/arq5x/bits")
      (synopsis "Implementation of binary interval search algorithm")
      (description "This package provides an implementation of the
BITS (Binary Interval Search) algorithm, an approach to interval set
intersection.  It is especially suited for the comparison of diverse genomic
datasets and the exploration of large datasets of genome
intervals (e.g. genes, sequence alignments).")
      (license license:gpl2))))

(define-public piranha
  ;; There is no release tarball for the latest version.  The latest commit is
  ;; older than one year at the time of this writing.
  (let ((revision "1")
        (commit   "0466d364b71117d01e4471b74c514436cc281233"))
    (package
      (name "piranha")
      (version (string-append "1.2.1-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/smithlabcode/piranha.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "117dc0zf20c61jam69sk4abl57ah6yi6i7qra7d7y5zrbgk12q5n"))))
      (build-system gnu-build-system)
      (arguments
       `(#:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'copy-smithlab-cpp
             (lambda* (#:key inputs #:allow-other-keys)
               (for-each (lambda (file)
                           (install-file file "./src/smithlab_cpp/"))
                         (find-files (assoc-ref inputs "smithlab-cpp")))
               #t))
           (add-after 'install 'install-to-store
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (for-each (lambda (file)
                             (install-file file bin))
                           (find-files "bin" ".*")))
               #t)))
         #:configure-flags
         (list (string-append "--with-bam_tools_headers="
                              (assoc-ref %build-inputs "bamtools") "/include/bamtools")
               (string-append "--with-bam_tools_library="
                              (assoc-ref %build-inputs "bamtools") "/lib/bamtools"))))
      (inputs
       `(("bamtools" ,bamtools)
         ("samtools" ,samtools-0.1)
         ("gsl" ,gsl)
         ("smithlab-cpp"
          ,(let ((commit "3723e2db438c51501d0423429ff396c3035ba46a"))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/smithlabcode/smithlab_cpp.git")
                     (commit commit)))
               (file-name (string-append "smithlab_cpp-" commit "-checkout"))
               (sha256
                (base32
                 "0l4gvbwslw5ngziskja41c00x1r06l3yidv7y0xw9djibhykzy0g")))))))
      (native-inputs
       `(("python" ,python-2)))
      (home-page "https://github.com/smithlabcode/piranha")
      (synopsis "Peak-caller for CLIP-seq and RIP-seq data")
      (description
       "Piranha is a peak-caller for genomic data produced by CLIP-seq and
RIP-seq experiments.  It takes input in BED or BAM format and identifies
regions of statistically significant read enrichment.  Additional covariates
may optionally be provided to further inform the peak-calling process.")
      (license license:gpl3+))))

(define-public pepr
  (package
    (name "pepr")
    (version "1.0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pypi.python.org/packages/source/P"
                                  "/PePr/PePr-" version ".tar.gz"))
              (sha256
               (base32
                "0qxjfdpl1b1y53nccws2d85f6k74zwmx8y8sd9rszcqhfayx6gdx"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; python2 only
       #:tests? #f)) ; no tests included
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)
       ("python2-scipy" ,python2-scipy)
       ("python2-pysam" ,python2-pysam)))
    (home-page "https://github.com/shawnzhangyx/PePr")
    (synopsis "Peak-calling and prioritization pipeline for ChIP-Seq data")
    (description
     "PePr is a ChIP-Seq peak calling or differential binding analysis tool
that is primarily designed for data with biological replicates.  It uses a
negative binomial distribution to model the read counts among the samples in
the same group, and look for consistent differences between ChIP and control
group or two ChIP groups run under different conditions.")
    (license license:gpl3+)))

(define-public filevercmp
  (let ((commit "1a9b779b93d0b244040274794d402106907b71b7"))
    (package
      (name "filevercmp")
      (version (string-append "0-1." (string-take commit 7)))
      (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ekg/filevercmp/archive/"
                            commit ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0yp5jswf5j2pqc6517x277s4s6h1ss99v57kxw9gy0jkfl3yh450"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests to run.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; There is no configure phase.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "filevercmp" bin)))))))
      (home-page "https://github.com/ekg/filevercmp")
      (synopsis "This program compares version strings")
      (description "This program compares version strings.  It intends to be a
replacement for strverscmp.")
      (license license:gpl3+))))

(define-public multiqc
  (package
    (name "multiqc")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "multiqc" version))
       (sha256
        (base32
         "0ihx4rzmsfphv4byn05qv6f1y95g2dxs6viwziipl4wjk96acgm8"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-jinja2" ,python-jinja2)
       ("python-simplejson" ,python-simplejson)
       ("python-pyyaml" ,python-pyyaml)
       ("python-click" ,python-click)
       ("python-spectra" ,python-spectra)
       ("python-requests" ,python-requests)
       ("python-markdown" ,python-markdown)
       ("python-lzstring" ,python-lzstring)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ;; MultQC checks for the presence of nose at runtime.
       ("python-nose" ,python-nose)))
    (home-page "http://multiqc.info")
    (synopsis "Aggregate bioinformatics analysis reports")
    (description
     "MultiQC is a tool to aggregate bioinformatics results across many
samples into a single report.  It contains modules for a large number of
common bioinformatics tools.")
    (license license:gpl3+)))

(define-public r-chipseq
  (package
    (name "r-chipseq")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chipseq" version))
       (sha256
        (base32
         "1ymcq77krwjzrkzzcw7i9909cmkqa7c0675z9wzvrrk81hgdssfq"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shortread" ,r-shortread)))
    (home-page "https://bioconductor.org/packages/chipseq")
    (synopsis "Package for analyzing ChIPseq data")
    (description
     "This package provides tools for processing short read data from ChIPseq
experiments.")
    (license license:artistic2.0)))

(define-public r-copyhelper
  (package
    (name "r-copyhelper")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bioconductor.org/packages/release/"
                           "data/experiment/src/contrib/CopyhelpeR_"
                           version ".tar.gz"))
       (sha256
        (base32
         "0x7cyynjmxls9as2gg0iyp9x5fpalxmdjq914ss7i84i9zyk5bhq"))))
    (properties `((upstream-name . "CopyhelpeR")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/CopyhelpeR/")
    (synopsis "Helper files for CopywriteR")
    (description
     "This package contains the helper files that are required to run the
Bioconductor package CopywriteR.  It contains pre-assembled 1kb bin GC-content
and mappability files for the reference genomes hg18, hg19, hg38, mm9 and
mm10.  In addition, it contains a blacklist filter to remove regions that
display copy number variation.  Files are stored as GRanges objects from the
GenomicRanges Bioconductor package.")
    (license license:gpl2)))

(define-public r-copywriter
  (package
    (name "r-copywriter")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CopywriteR" version))
       (sha256
        (base32
         "17fy2lc5yf3nh6v077kv87h53n263hqz2540lzrl0vjiqrl2plca"))))
    (properties `((upstream-name . "CopywriteR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-chipseq" ,r-chipseq)
       ("r-copyhelper" ,r-copyhelper)
       ("r-data-table" ,r-data-table)
       ("r-dnacopy" ,r-dnacopy)
       ("r-futile-logger" ,r-futile-logger)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-gtools" ,r-gtools)
       ("r-iranges" ,r-iranges)
       ("r-matrixstats" ,r-matrixstats)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://github.com/PeeperLab/CopywriteR")
    (synopsis "Copy number information from targeted sequencing")
    (description
     "CopywriteR extracts DNA copy number information from targeted sequencing
by utilizing off-target reads.  It allows for extracting uniformly distributed
copy number information, can be used without reference, and can be applied to
sequencing data obtained from various techniques including chromatin
immunoprecipitation and target enrichment on small gene panels.  Thereby,
CopywriteR constitutes a widely applicable alternative to available copy
number detection tools.")
    (license license:gpl2)))

(define-public r-methylkit
  (package
    (name "r-methylkit")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "methylKit" version))
              (sha256
               (base32
                "1k0nfn9318sgwm4z963bhnbp4c3zv85v3f9886vc5hgaisr0yvai"))))
    (properties `((upstream-name . "methylKit")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-emdbook" ,r-emdbook)
       ("r-fastseg" ,r-fastseg)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-gtools" ,r-gtools)
       ("r-iranges" ,r-iranges)
       ("r-kernsmooth" ,r-kernsmooth)
       ("r-limma" ,r-limma)
       ("r-mclust" ,r-mclust)
       ("r-qvalue" ,r-qvalue)
       ("r-r-utils" ,r-r-utils)
       ("r-rcpp" ,r-rcpp)
       ("r-rhtslib" ,r-rhtslib)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-zlibbioc" ,r-zlibbioc)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/al2na/methylKit")
    (synopsis
     "DNA methylation analysis from high-throughput bisulfite sequencing results")
    (description
     "MethylKit is an R package for DNA methylation analysis and annotation
from high-throughput bisulfite sequencing.  The package is designed to deal
with sequencing data from @dfn{Reduced representation bisulfite
sequencing} (RRBS) and its variants, but also target-capture methods and whole
genome bisulfite sequencing.  It also has functions to analyze base-pair
resolution 5hmC data from experimental protocols such as oxBS-Seq and
TAB-Seq.")
    (license license:artistic2.0)))

(define-public r-sva
  (package
    (name "r-sva")
    (version "3.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "sva" version))
       (sha256
        (base32
         "0q5xb68wfcnchy8rkv5ma67pmz1i91lsnvmwmj8f1c3w4xan3pgw"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genefilter" ,r-genefilter)
       ("r-mgcv" ,r-mgcv)
       ("r-biocparallel" ,r-biocparallel)
       ("r-matrixstats" ,r-matrixstats)
       ("r-limma" ,r-limma)))
    (home-page "https://bioconductor.org/packages/sva")
    (synopsis "Surrogate variable analysis")
    (description
     "This package contains functions for removing batch effects and other
unwanted variation in high-throughput experiment.  It also contains functions
for identifying and building surrogate variables for high-dimensional data
sets.  Surrogate variables are covariates constructed directly from
high-dimensional data like gene expression/RNA sequencing/methylation/brain
imaging data that can be used in subsequent analyses to adjust for unknown,
unmodeled, or latent sources of noise.")
    (license license:artistic2.0)))

(define-public r-seqminer
  (package
    (name "r-seqminer")
    (version "6.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "seqminer" version))
       (sha256
        (base32
         "057j1l6dip35l1aivilapl2zv9db677b3di2pb3sfgq2sxg0ps3l"))))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (home-page "http://seqminer.genomic.codes")
    (synopsis "Read nucleotide sequence data (VCF, BCF, and METAL formats)")
    (description
     "This package provides tools to integrate nucleotide sequencing
data (variant call format, e.g. VCF or BCF) or meta-analysis results in R.")
    ;; Any version of the GPL is acceptable
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-raremetals2
  (package
    (name "r-raremetals2")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://genome.sph.umich.edu/w/images/"
                           "b/b7/RareMETALS2_" version ".tar.gz"))
       (sha256
        (base32
         "0z5ljcgvnm06ja9lm85a3cniq7slxcy37aqqkxrdidr79an5fs4s"))))
    (properties `((upstream-name . "RareMETALS2")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-seqminer" ,r-seqminer)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-mass" ,r-mass)
       ("r-compquadform" ,r-compquadform)
       ("r-getopt" ,r-getopt)))
    (home-page "http://genome.sph.umich.edu/wiki/RareMETALS2")
    (synopsis "Analyze gene-level association tests for binary trait")
    (description
     "The R package rareMETALS2 is an extension of the R package rareMETALS.
It was designed to meta-analyze gene-level association tests for binary trait.
While rareMETALS offers a near-complete solution for meta-analysis of
gene-level tests for quantitative trait, it does not offer the optimal
solution for binary trait.  The package rareMETALS2 offers improved features
for analyzing gene-level association tests in meta-analyses for binary
trait.")
    (license license:gpl3)))

(define-public r-maldiquant
  (package
    (name "r-maldiquant")
    (version "1.17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "MALDIquant" version))
       (sha256
        (base32
         "047s6007ydc38x8wm027mlb4mngz15n0d4238fr8h43wyll5zy0z"))))
    (properties `((upstream-name . "MALDIquant")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/MALDIquant")
    (synopsis "Quantitative analysis of mass spectrometry data")
    (description
     "This package provides a complete analysis pipeline for matrix-assisted
laser desorption/ionization-time-of-flight (MALDI-TOF) and other
two-dimensional mass spectrometry data.  In addition to commonly used plotting
and processing methods it includes distinctive features, namely baseline
subtraction methods such as morphological filters (TopHat) or the
statistics-sensitive non-linear iterative peak-clipping algorithm (SNIP), peak
alignment using warping functions, handling of replicated measurements as well
as allowing spectra with different resolutions.")
    (license license:gpl3+)))

(define-public r-protgenerics
  (package
    (name "r-protgenerics")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ProtGenerics" version))
       (sha256
        (base32
         "16ijp50448wnabp43klx943rhdvh7x45hvy7cnpq1s4dckxhhyni"))))
    (properties `((upstream-name . "ProtGenerics")))
    (build-system r-build-system)
    (home-page "https://github.com/lgatto/ProtGenerics")
    (synopsis "S4 generic functions for proteomics infrastructure")
    (description
     "This package provides S4 generic functions needed by Bioconductor
proteomics packages.")
    (license license:artistic2.0)))

(define-public r-mzr
  (package
    (name "r-mzr")
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzR" version))
       (sha256
        (base32
         "1x3gp30sfxz2v3k3swih9kff9b2rvk7hzhnlkp6ywlnn2wgb0q8c"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "src/boost")
           #t))))
    (properties `((upstream-name . "mzR")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-boost
           (lambda _
             (substitute* "src/Makevars"
               (("\\./boost/libs.*") "")
               (("ARCH_OBJS=" line)
                (string-append line
                               "\nARCH_LIBS=-lboost_system -lboost_regex \
-lboost_iostreams -lboost_thread -lboost_filesystem -lboost_chrono\n")))
             #t)))))
    (inputs
     `(("boost" ,boost) ; use this instead of the bundled boost sources
       ("netcdf" ,netcdf)))
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-protgenerics" ,r-protgenerics)
       ("r-rcpp" ,r-rcpp)
       ("r-zlibbioc" ,r-zlibbioc)))
    (home-page "https://github.com/sneumann/mzR/")
    (synopsis "Parser for mass spectrometry data files")
    (description
     "The mzR package provides a unified API to the common file formats and
parsers available for mass spectrometry data.  It comes with a wrapper for the
ISB random access parser for mass spectrometry mzXML, mzData and mzML files.
The package contains the original code written by the ISB, and a subset of the
proteowizard library for mzML and mzIdentML.  The netCDF reading code has
previously been used in XCMS.")
    (license license:artistic2.0)))

(define-public r-affyio
  (package
    (name "r-affyio")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyio" version))
       (sha256
        (base32
         "1pzzp3d3dbmyf34gvivfiprkpscn36rgvhrq853a1d3avcwr5ak9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-zlibbioc" ,r-zlibbioc)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/bmbolstad/affyio")
    (synopsis "Tools for parsing Affymetrix data files")
    (description
     "This package provides routines for parsing Affymetrix data files based
upon file format information.  The primary focus is on accessing the CEL and
CDF file formats.")
    (license license:lgpl2.0+)))

(define-public r-affy
  (package
    (name "r-affy")
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affy" version))
       (sha256
        (base32
         "0jmbkimma5ffsdkk3xp03g4lpz84gd95nkqakif2nqq6wmx0syrj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affyio" ,r-affyio)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocinstaller" ,r-biocinstaller)
       ("r-preprocesscore" ,r-preprocesscore)
       ("r-zlibbioc" ,r-zlibbioc)))
    (home-page "https://bioconductor.org/packages/affy")
    (synopsis "Methods for affymetrix oligonucleotide arrays")
    (description
     "This package contains functions for exploratory oligonucleotide array
analysis.")
    (license license:lgpl2.0+)))

(define-public r-vsn
  (package
    (name "r-vsn")
    (version "3.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "vsn" version))
       (sha256
        (base32
         "18y62phzirj75gg6v5l41jwybmk23ia6w7qhch0kxc4bl2rysw6j"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-biobase" ,r-biobase)
       ("r-ggplot2" ,r-ggplot2)
       ("r-lattice" ,r-lattice)
       ("r-limma" ,r-limma)))
    (home-page "https://bioconductor.org/packages/release/bioc/html/vsn.html")
    (synopsis "Variance stabilization and calibration for microarray data")
    (description
     "The package implements a method for normalising microarray intensities,
and works for single- and multiple-color arrays.  It can also be used for data
from other technologies, as long as they have similar format.  The method uses
a robust variant of the maximum-likelihood estimator for an
additive-multiplicative error model and affine calibration.  The model
incorporates data calibration step (a.k.a.  normalization), a model for the
dependence of the variance on the mean intensity and a variance stabilizing
data transformation.  Differences between transformed intensities are
analogous to \"normalized log-ratios\".  However, in contrast to the latter,
their variance is independent of the mean, and they are usually more sensitive
and specific in detecting differential transcription.")
    (license license:artistic2.0)))

(define-public r-mzid
  (package
    (name "r-mzid")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzID" version))
       (sha256
        (base32
         "0yk70dka56zd8w62f03ggx3mandj91gfa767h9ajj0sd3mjmfqb9"))))
    (properties `((upstream-name . "mzID")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-doparallel" ,r-doparallel)
       ("r-foreach" ,r-foreach)
       ("r-iterators" ,r-iterators)
       ("r-plyr" ,r-plyr)
       ("r-protgenerics" ,r-protgenerics)
       ("r-rcpp" ,r-rcpp)
       ("r-xml" ,r-xml)))
    (home-page "https://bioconductor.org/packages/mzID")
    (synopsis "Parser for mzIdentML files")
    (description
     "This package provides a parser for mzIdentML files implemented using the
XML package.  The parser tries to be general and able to handle all types of
mzIdentML files with the drawback of having less pretty output than a vendor
specific parser.")
    (license license:gpl2+)))

(define-public r-pcamethods
  (package
    (name "r-pcamethods")
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pcaMethods" version))
       (sha256
        (base32
         "0ii235g0x0492kh8cfrf28ni0b6vd6fh7kizkqmczzqggd6b1bk8"))))
    (properties `((upstream-name . "pcaMethods")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-mass" ,r-mass)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/hredestig/pcamethods")
    (synopsis "Collection of PCA methods")
    (description
     "This package provides Bayesian PCA, Probabilistic PCA, Nipals PCA,
Inverse Non-Linear PCA and the conventional SVD PCA.  A cluster based method
for missing value estimation is included for comparison.  BPCA, PPCA and
NipalsPCA may be used to perform PCA on incomplete data as well as for
accurate missing value estimation.  A set of methods for printing and plotting
the results is also provided.  All PCA methods make use of the same data
structure (pcaRes) to provide a common interface to the PCA results.")
    (license license:gpl3+)))

(define-public r-msnbase
  (package
    (name "r-msnbase")
    (version "2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnbase" version))
       (sha256
        (base32
         "1ig64bf881p118dwqfr0ry41m7yhnyv165smv8fdwfv7sb6sagif"))))
    (properties `((upstream-name . "MSnbase")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-digest" ,r-digest)
       ("r-ggplot2" ,r-ggplot2)
       ("r-impute" ,r-impute)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-maldiquant" ,r-maldiquant)
       ("r-mzid" ,r-mzid)
       ("r-mzr" ,r-mzr)
       ("r-pcamethods" ,r-pcamethods)
       ("r-plyr" ,r-plyr)
       ("r-preprocesscore" ,r-preprocesscore)
       ("r-protgenerics" ,r-protgenerics)
       ("r-rcpp" ,r-rcpp)
       ("r-s4vectors" ,r-s4vectors)
       ("r-vsn" ,r-vsn)
       ("r-xml" ,r-xml)))
    (home-page "https://github.com/lgatto/MSnbase")
    (synopsis "Base functions and classes for MS-based proteomics")
    (description
     "This package provides basic plotting, data manipulation and processing
of mass spectrometry based proteomics data.")
    (license license:artistic2.0)))

(define-public r-msnid
  (package
    (name "r-msnid")
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnID" version))
       (sha256
        (base32
         "1zw508kk4f8brg69674wp18gqkpx2kpya5f6x9cl3qng7v4h5pxx"))))
    (properties `((upstream-name . "MSnID")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-data-table" ,r-data-table)
       ("r-doparallel" ,r-doparallel)
       ("r-dplyr" ,r-dplyr)
       ("r-foreach" ,r-foreach)
       ("r-iterators" ,r-iterators)
       ("r-msnbase" ,r-msnbase)
       ("r-mzid" ,r-mzid)
       ("r-mzr" ,r-mzr)
       ("r-protgenerics" ,r-protgenerics)
       ("r-r-cache" ,r-r-cache)
       ("r-rcpp" ,r-rcpp)
       ("r-reshape2" ,r-reshape2)))
    (home-page "https://bioconductor.org/packages/MSnID")
    (synopsis "Utilities for LC-MSn proteomics identifications")
    (description
     "This package extracts @dfn{tandem mass spectrometry} (MS/MS) ID data
from mzIdentML (leveraging the mzID package) or text files.  After collating
the search results from multiple datasets it assesses their identification
quality and optimize filtering criteria to achieve the maximum number of
identifications while not exceeding a specified false discovery rate.  It also
contains a number of utilities to explore the MS/MS results and assess missed
and irregular enzymatic cleavages, mass measurement accuracy, etc.")
    (license license:artistic2.0)))

(define-public r-seurat
  (package
    (name "r-seurat")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "Seurat" version))
              (sha256
               (base32
                "1sr82nf38hq07avrfn8vlrzjq7dfm4pcr8l1nh6mnglcql2bk9z2"))
              ;; Delete pre-built jar.
              (snippet
               '(begin (delete-file "inst/java/ModularityOptimizer.jar")
                       #t))))
    (properties `((upstream-name . "Seurat")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'build-jar
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((classesdir "tmp-classes"))
               (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
               (mkdir classesdir)
               (with-output-to-file "manifest"
                 (lambda _
                   (display "Manifest-Version: 1.0
Main-Class: ModularityOptimizer\n")))
               (and (zero? (apply system* `("javac" "-d" ,classesdir
                                            ,@(find-files "java" "\\.java$"))))
                    (zero? (system* "jar"
                                    "-cmf" "manifest"
                                    "inst/java/ModularityOptimizer.jar"
                                    "-C" classesdir  ".")))))))))
    (native-inputs
     `(("jdk" ,icedtea "jdk")))
    (propagated-inputs
     `(("r-ape" ,r-ape)
       ("r-caret" ,r-caret)
       ("r-cowplot" ,r-cowplot)
       ("r-diffusionmap" ,r-diffusionmap)
       ("r-dplyr" ,r-dplyr)
       ("r-dtw" ,r-dtw)
       ("r-fnn" ,r-fnn)
       ("r-fpc" ,r-fpc)
       ("r-gdata" ,r-gdata)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggridges" ,r-ggridges)
       ("r-gplots" ,r-gplots)
       ("r-gridextra" ,r-gridextra)
       ("r-hmisc" ,r-hmisc)
       ("r-ica" ,r-ica)
       ("r-igraph" ,r-igraph)
       ("r-irlba" ,r-irlba)
       ("r-lars" ,r-lars)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-metap" ,r-metap)
       ("r-mixtools" ,r-mixtools)
       ("r-pbapply" ,r-pbapply)
       ("r-plotly" ,r-plotly)
       ("r-ranger" ,r-ranger)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-rcppprogress" ,r-rcppprogress)
       ("r-reshape2" ,r-reshape2)
       ("r-rocr" ,r-rocr)
       ("r-rtsne" ,r-rtsne)
       ("r-sdmtools" ,r-sdmtools)
       ("r-stringr" ,r-stringr)
       ("r-tclust" ,r-tclust)
       ("r-tidyr" ,r-tidyr)
       ("r-tsne" ,r-tsne)
       ("r-vgam" ,r-vgam)))
    (home-page "http://www.satijalab.org/seurat")
    (synopsis "Seurat is an R toolkit for single cell genomics")
    (description
     "This package is an R package designed for QC, analysis, and
exploration of single cell RNA-seq data.  It easily enables widely-used
analytical techniques, including the identification of highly variable genes,
dimensionality reduction; PCA, ICA, t-SNE, standard unsupervised clustering
algorithms; density clustering, hierarchical clustering, k-means, and the
discovery of differentially expressed genes and markers.")
    (license license:gpl3)))

(define-public r-aroma-light
  (package
    (name "r-aroma-light")
    (version "3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "aroma.light" version))
       (sha256
        (base32
         "0crnk6851jwypqr5l5jcbbay0vi5vvdjyisaf6z2d69c39wmr6sc"))))
    (properties `((upstream-name . "aroma.light")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrixstats" ,r-matrixstats)
       ("r-r-methodss3" ,r-r-methodss3)
       ("r-r-oo" ,r-r-oo)
       ("r-r-utils" ,r-r-utils)))
    (home-page "https://github.com/HenrikBengtsson/aroma.light")
    (synopsis "Methods for normalization and visualization of microarray data")
    (description
     "This package provides methods for microarray analysis that take basic
data types such as matrices and lists of vectors.  These methods can be used
standalone, be utilized in other packages, or be wrapped up in higher-level
classes.")
    (license license:gpl2+)))

(define-public r-deseq
  (package
    (name "r-deseq")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq" version))
       (sha256
        (base32
         "0mn5w3cy16iwwk8zxs7za6aa6cnrca75z0g45zd5zh1py5d7nfv9"))))
    (properties `((upstream-name . "DESeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-genefilter" ,r-genefilter)
       ("r-geneplotter" ,r-geneplotter)
       ("r-lattice" ,r-lattice)
       ("r-locfit" ,r-locfit)
       ("r-mass" ,r-mass)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page "http://www-huber.embl.de/users/anders/DESeq")
    (synopsis "Differential gene expression analysis")
    (description
     "This package provides tools for estimating variance-mean dependence in
count data from high-throughput genetic sequencing assays and for testing for
differential expression based on a model using the negative binomial
distribution.")
    (license license:gpl3+)))

(define-public r-edaseq
  (package
    (name "r-edaseq")
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EDASeq" version))
       (sha256
        (base32
         "07zm89zcivyn2261aq9grqmly8ji482kr9h9dyfknfdfrpv7jpwv"))))
    (properties `((upstream-name . "EDASeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-aroma-light" ,r-aroma-light)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biomart" ,r-biomart)
       ("r-biostrings" ,r-biostrings)
       ("r-deseq" ,r-deseq)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-shortread" ,r-shortread)))
    (home-page "https://github.com/drisso/EDASeq")
    (synopsis "Exploratory data analysis and normalization for RNA-Seq")
    (description
     "This package provides support for numerical and graphical summaries of
RNA-Seq genomic read data.  Provided within-lane normalization procedures to
adjust for GC-content effect (or other gene-level effects) on read counts:
loess robust local regression, global-scaling, and full-quantile
normalization.  Between-lane normalization procedures to adjust for
distributional differences between lanes (e.g., sequencing depth):
global-scaling and full-quantile normalization.")
    (license license:artistic2.0)))

(define-public r-interactivedisplaybase
  (package
    (name "r-interactivedisplaybase")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "interactiveDisplayBase" version))
       (sha256
        (base32
         "01yb945jqqimwjgriza6yy4dnp303cdirxrhl4hjyprfdlmnz5p5"))))
    (properties
     `((upstream-name . "interactiveDisplayBase")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-shiny" ,r-shiny)))
    (home-page "https://bioconductor.org/packages/interactiveDisplayBase")
    (synopsis "Base package for web displays of Bioconductor objects")
    (description
     "This package contains the basic methods needed to generate interactive
Shiny-based display methods for Bioconductor objects.")
    (license license:artistic2.0)))

(define-public r-annotationhub
  (package
    (name "r-annotationhub")
    (version "2.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationHub" version))
       (sha256
        (base32
         "14v8g44a6zg9j2rwn9x9y8509k0wr2cw8yccliz24glplb40wva4"))))
    (properties `((upstream-name . "AnnotationHub")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocinstaller" ,r-biocinstaller)
       ("r-curl" ,r-curl)
       ("r-httr" ,r-httr)
       ("r-interactivedisplaybase" ,r-interactivedisplaybase)
       ("r-rsqlite" ,r-rsqlite)
       ("r-s4vectors" ,r-s4vectors)
       ("r-yaml" ,r-yaml)))
    (home-page "https://bioconductor.org/packages/AnnotationHub")
    (synopsis "Client to access AnnotationHub resources")
    (description
     "This package provides a client for the Bioconductor AnnotationHub web
resource.  The AnnotationHub web resource provides a central location where
genomic files (e.g. VCF, bed, wig) and other resources from standard
locations (e.g. UCSC, Ensembl) can be discovered.  The resource includes
metadata about each resource, e.g., a textual description, tags, and date of
modification.  The client creates and manages a local cache of files retrieved
by the user, helping with quick and reproducible access.")
    (license license:artistic2.0)))

(define-public r-fastseg
  (package
    (name "r-fastseg")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fastseg" version))
       (sha256
        (base32
         "0dd7nr3klwz9ailwshnbynhd62lwb8zbbpj6jf3igpb94yi6x2jp"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "http://www.bioinf.jku.at/software/fastseg/index.html")
    (synopsis "Fast segmentation algorithm for genetic sequencing data")
    (description
     "Fastseg implements a very fast and efficient segmentation algorithm.
It can segment data from DNA microarrays and data from next generation
sequencing for example to detect copy number segments.  Further it can segment
data from RNA microarrays like tiling arrays to identify transcripts.  Most
generally, it can segment data given as a matrix or as a vector.  Various data
formats can be used as input to fastseg like expression set objects for
microarrays or GRanges for sequencing data.")
    (license license:lgpl2.0+)))

(define-public r-keggrest
  (package
    (name "r-keggrest")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGREST" version))
       (sha256
        (base32
         "02gwmm79djj55a90dzc80hlgwc6bafl7xd7fnx2q59pk945k3z9c"))))
    (properties `((upstream-name . "KEGGREST")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-httr" ,r-httr)
       ("r-png" ,r-png)))
    (home-page "https://bioconductor.org/packages/KEGGREST")
    (synopsis "Client-side REST access to KEGG")
    (description
     "This package provides a package that provides a client interface to the
@dfn{Kyoto Encyclopedia of Genes and Genomes} (KEGG) REST server.")
    (license license:artistic2.0)))

(define-public r-gage
  (package
    (name "r-gage")
    (version "2.28.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gage" version))
       (sha256
        (base32
         "0h0mlhns9j7cpfksvdlvx9jb7szm3r1dwqb3s4s8p8hmkb9byyii"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-graph" ,r-graph)
       ("r-keggrest" ,r-keggrest)))
    (home-page "http://www.biomedcentral.com/1471-2105/10/161")
    (synopsis "Generally applicable gene-set enrichment for pathway analysis")
    (description
     "GAGE is a published method for gene set (enrichment or GSEA) or pathway
analysis.  GAGE is generally applicable independent of microarray or RNA-Seq
data attributes including sample sizes, experimental designs, assay platforms,
and other types of heterogeneity.  The gage package provides functions for
basic GAGE analysis, result processing and presentation.  In addition, it
provides demo microarray data and commonly used gene set data based on KEGG
pathways and GO terms.  These funtions and data are also useful for gene set
analysis using other methods.")
    (license license:gpl2+)))

(define-public r-genomicfiles
  (package
    (name "r-genomicfiles")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicFiles" version))
       (sha256
        (base32
         "0r0wmrs5jycf1kckhnc2sgjmp336srlcjdkpbb1ymm7kazdd0s9n"))))
    (properties `((upstream-name . "GenomicFiles")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://bioconductor.org/packages/GenomicFiles")
    (synopsis "Distributed computing by file or by range")
    (description
     "This package provides infrastructure for parallel computations
distributed by file or by range.  User defined mapper and reducer functions
provide added flexibility for data combination and manipulation.")
    (license license:artistic2.0)))

(define-public r-complexheatmap
  (package
    (name "r-complexheatmap")
    (version "1.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ComplexHeatmap" version))
       (sha256
        (base32
         "1x6kp55iqqsd8bhdl3qch95nfiy2y46ldbbsx1sj1v8f0b0ywwcy"))))
    (properties
     `((upstream-name . "ComplexHeatmap")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-circlize" ,r-circlize)
       ("r-colorspace" ,r-colorspace)
       ("r-getoptlong" ,r-getoptlong)
       ("r-globaloptions" ,r-globaloptions)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page
     "https://github.com/jokergoo/ComplexHeatmap")
    (synopsis "Making Complex Heatmaps")
    (description
     "Complex heatmaps are efficient to visualize associations between
different sources of data sets and reveal potential structures.  This package
provides a highly flexible way to arrange multiple heatmaps and supports
self-defined annotation graphics.")
    (license license:gpl2+)))

(define-public r-dirichletmultinomial
  (package
    (name "r-dirichletmultinomial")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DirichletMultinomial" version))
       (sha256
        (base32
         "1c4s6x0qm20556grcd1xys9kkpnlzpasaai474malwcg6qvgi4x1"))))
    (properties
     `((upstream-name . "DirichletMultinomial")))
    (build-system r-build-system)
    (inputs
     `(("gsl" ,gsl)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/DirichletMultinomial")
    (synopsis "Dirichlet-Multinomial mixture models for microbiome data")
    (description
     "Dirichlet-multinomial mixture models can be used to describe variability
in microbial metagenomic data.  This package is an interface to code
originally made available by Holmes, Harris, and Quince, 2012, PLoS ONE 7(2):
1-15.")
    (license license:lgpl3)))

(define-public r-ensembldb
  (package
    (name "r-ensembldb")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ensembldb" version))
       (sha256
        (base32
         "1yngndkf3588z91z0a2fvkg423p26ajm6xv1p27x0l9mzhhaqq3k"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationfilter" ,r-annotationfilter)
       ("r-annotationhub" ,r-annotationhub)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-curl" ,r-curl)
       ("r-dbi" ,r-dbi)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-protgenerics" ,r-protgenerics)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rsqlite" ,r-rsqlite)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://github.com/jotsetung/ensembldb")
    (synopsis "Utilities to create and use Ensembl-based annotation databases")
    (description
     "The package provides functions to create and use transcript-centric
annotation databases/packages.  The annotation for the databases are directly
fetched from Ensembl using their Perl API.  The functionality and data is
similar to that of the TxDb packages from the @code{GenomicFeatures} package,
but, in addition to retrieve all gene/transcript models and annotations from
the database, the @code{ensembldb} package also provides a filter framework
allowing to retrieve annotations for specific entries like genes encoded on a
chromosome region or transcript models of lincRNA genes.")
    ;; No version specified
    (license license:lgpl3+)))

(define-public r-organismdbi
  (package
    (name "r-organismdbi")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "OrganismDbi" version))
       (sha256
        (base32
         "0yxvhwn0m53wfwp0zi81x96argdf7cf1lpymc2as51apvfcnjdl8"))))
    (properties `((upstream-name . "OrganismDbi")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocinstaller" ,r-biocinstaller)
       ("r-dbi" ,r-dbi)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-graph" ,r-graph)
       ("r-iranges" ,r-iranges)
       ("r-rbgl" ,r-rbgl)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/OrganismDbi")
    (synopsis "Software to enable the smooth interfacing of database packages")
    (description "The package enables a simple unified interface to several
annotation packages each of which has its own schema by taking advantage of
the fact that each of these packages implements a select methods.")
    (license license:artistic2.0)))

(define-public r-biovizbase
  (package
    (name "r-biovizbase")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biovizBase" version))
       (sha256
        (base32
         "14l4vhj0a4ssr9m9zdzz3qpd4qw1mhgq5bmxq7jhrq3j9kmd6i2f"))))
    (properties `((upstream-name . "biovizBase")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationfilter" ,r-annotationfilter)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-dichromat" ,r-dichromat)
       ("r-ensembldb" ,r-ensembldb)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-hmisc" ,r-hmisc)
       ("r-iranges" ,r-iranges)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scales" ,r-scales)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://bioconductor.org/packages/biovizBase")
    (synopsis "Basic graphic utilities for visualization of genomic data")
    (description
     "The biovizBase package is designed to provide a set of utilities, color
schemes and conventions for genomic data.  It serves as the base for various
high-level packages for biological data visualization.  This saves development
effort and encourages consistency.")
    (license license:artistic2.0)))

(define-public r-ggbio
  (package
    (name "r-ggbio")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggbio" version))
       (sha256
        (base32
         "1bqxfqy0hff87ax92z4lfbjz01ndrz7x8pzm6dlkdmi52p30krm9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationfilter" ,r-annotationfilter)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-biovizbase" ,r-biovizbase)
       ("r-bsgenome" ,r-bsgenome)
       ("r-ensembldb" ,r-ensembldb)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggally" ,r-ggally)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-gtable" ,r-gtable)
       ("r-hmisc" ,r-hmisc)
       ("r-iranges" ,r-iranges)
       ("r-organismdbi" ,r-organismdbi)
       ("r-reshape2" ,r-reshape2)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scales" ,r-scales)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "http://www.tengfei.name/ggbio/")
    (synopsis "Visualization tools for genomic data")
    (description
     "The ggbio package extends and specializes the grammar of graphics for
biological data.  The graphics are designed to answer common scientific
questions, in particular those often asked of high throughput genomics data.
All core Bioconductor data structures are supported, where appropriate.  The
package supports detailed views of particular genomic regions, as well as
genome-wide overviews.  Supported overviews include ideograms and grand linear
views.  High-level plots include sequence fragment length, edge-linked
interval to data view, mismatch pileup, and several splicing summaries.")
    (license license:artistic2.0)))

(define-public r-gprofiler
  (package
    (name "r-gprofiler")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gProfileR" version))
       (sha256
        (base32
         "1cka02zbz1rbppm782qpxk1xn9qxbrv2gp5rgf970j906hxm2y0b"))))
    (properties `((upstream-name . "gProfileR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-plyr" ,r-plyr)
       ("r-rcurl" ,r-rcurl)))
    (home-page "https://cran.r-project.org/web/packages/gProfileR/")
    (synopsis "Interface to the g:Profiler toolkit")
    (description
     "This package provides tools for functional enrichment analysis,
gene identifier conversion and mapping homologous genes across related
organisms via the @code{g:Profiler} toolkit.")
    (license license:gpl2+)))

(define-public r-gqtlbase
  (package
    (name "r-gqtlbase")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gQTLBase" version))
       (sha256
        (base32
         "1756vfcj2dkkgcmfkkg7qdaig36dv9gfvpypn9rbrky56wm1p035"))))
    (properties `((upstream-name . "gQTLBase")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-batchjobs" ,r-batchjobs)
       ("r-bbmisc" ,r-bbmisc)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-bit" ,r-bit)
       ("r-doparallel" ,r-doparallel)
       ("r-ff" ,r-ff)
       ("r-ffbase" ,r-ffbase)
       ("r-foreach" ,r-foreach)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/gQTLBase")
    (synopsis "Infrastructure for eQTL, mQTL and similar studies")
    (description
     "The purpose of this package is to simplify the storage and interrogation
of @dfn{quantitative trait loci} (QTL) archives, such as eQTL, mQTL, dsQTL,
and more.")
    (license license:artistic2.0)))

(define-public r-snpstats
  (package
    (name "r-snpstats")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "snpStats" version))
       (sha256
        (base32
         "1x9qwynh2hwl24vq02naf4mchpch7xi2pkdrlgw896k28kx0lvir"))))
    (properties `((upstream-name . "snpStats")))
    (build-system r-build-system)
    (inputs `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-matrix" ,r-matrix)
       ("r-survival" ,r-survival)
       ("r-zlibbioc" ,r-zlibbioc)))
    (home-page "https://bioconductor.org/packages/snpStats")
    (synopsis "Methods for SNP association studies")
    (description
     "This package provides classes and statistical methods for large
@dfn{single-nucleotide polymorphism} (SNP) association studies.  This extends
the earlier snpMatrix package, allowing for uncertainty in genotypes.")
    (license license:gpl3)))

(define-public r-homo-sapiens
  (package
    (name "r-homo-sapiens")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "http://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "Homo.sapiens_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "151vj7h5p1c8yd5swrchk46z469p135wk50hvkl0nhgndvy0jj01"))))
    (properties
     `((upstream-name . "Homo.sapiens")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomicfeatures" ,r-genomicfeatures)
       ("r-go-db" ,r-go-db)
       ("r-org-hs-eg-db" ,r-org-hs-eg-db)
       ("r-txdb-hsapiens-ucsc-hg19-knowngene" ,r-txdb-hsapiens-ucsc-hg19-knowngene)
       ("r-organismdbi" ,r-organismdbi)
       ("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://bioconductor.org/packages/Homo.sapiens/")
    (synopsis "Annotation package for the Homo.sapiens object")
    (description
     "This package contains the Homo.sapiens object to access data from
several related annotation packages.")
    (license license:artistic2.0)))

(define-public r-erma
  (package
    (name "r-erma")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "erma" version))
       (sha256
        (base32
         "0gcfs9g8vvdv5vmq9b21kd8sq5mizjj49nfzd4in9zvp4b9v7x1g"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-foreach" ,r-foreach)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-homo-sapiens" ,r-homo-sapiens)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shiny" ,r-shiny)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/erma")
    (synopsis "Epigenomic road map adventures")
    (description
     "The epigenomics road map describes locations of epigenetic marks in DNA
from a variety of cell types.  Of interest are locations of histone
modifications, sites of DNA methylation, and regions of accessible chromatin.
This package presents a selection of elements of the road map including
metadata and outputs of the ChromImpute procedure applied to ENCODE cell lines
by Ernst and Kellis.")
    (license license:artistic2.0)))

(define-public r-ldblock
  (package
    (name "r-ldblock")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ldblock" version))
       (sha256
        (base32
         "18nfsixh6d2wfrb9laqsgly5w1frzihhak683k0p8fdf51h4aqba"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-erma" ,r-erma)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-go-db" ,r-go-db)
       ("r-homo-sapiens" ,r-homo-sapiens)
       ("r-matrix" ,r-matrix)
       ("r-rsamtools" ,r-rsamtools)
       ("r-snpstats" ,r-snpstats)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://bioconductor.org/packages/ldblock")
    (synopsis "Data structures for linkage disequilibrium measures in populations")
    (description
     "This package defines data structures for @dfn{linkage
disequilibrium} (LD) measures in populations.  Its purpose is to simplify
handling of existing population-level data for the purpose of flexibly
defining LD blocks.")
    (license license:artistic2.0)))

(define-public r-gqtlstats
  (package
    (name "r-gqtlstats")
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gQTLstats" version))
       (sha256
        (base32
         "0gvq1sf2zjbkk431x40z6wql3c1rpclnnwa2f1hvykb8mmw70kmq"))))
    (properties `((upstream-name . "gQTLstats")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-batchjobs" ,r-batchjobs)
       ("r-bbmisc" ,r-bbmisc)
       ("r-beeswarm" ,r-beeswarm)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-doparallel" ,r-doparallel)
       ("r-dplyr" ,r-dplyr)
       ("r-erma" ,r-erma)
       ("r-ffbase" ,r-ffbase)
       ("r-foreach" ,r-foreach)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggbeeswarm" ,r-ggbeeswarm)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gqtlbase" ,r-gqtlbase)
       ("r-hardyweinberg" ,r-hardyweinberg)
       ("r-iranges" ,r-iranges)
       ("r-ldblock" ,r-ldblock)
       ("r-limma" ,r-limma)
       ("r-mgcv" ,r-mgcv)
       ("r-plotly" ,r-plotly)
       ("r-reshape2" ,r-reshape2)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shiny" ,r-shiny)
       ("r-snpstats" ,r-snpstats)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://bioconductor.org/packages/gQTLstats")
    (synopsis "Computationally efficient analysis for eQTL and allied studies")
    (description
     "This package provides tools for the computationally efficient analysis
of @dfn{quantitative trait loci} (QTL) data, including eQTL, mQTL, dsQTL, etc.
The software in this package aims to support refinements and functional
interpretation of members of a collection of association statistics on a
family of feature/genome hypotheses.")
    (license license:artistic2.0)))

(define-public r-gviz
  (package
    (name "r-gviz")
    (version "1.22.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Gviz" version))
       (sha256
        (base32
         "1grjzrjpzkw572pbvpsvdnfkfgwybl0cnjd7nnk2xdr26wnbsi9a"))))
    (properties `((upstream-name . "Gviz")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biomart" ,r-biomart)
       ("r-biostrings" ,r-biostrings)
       ("r-biovizbase" ,r-biovizbase)
       ("r-bsgenome" ,r-bsgenome)
       ("r-digest" ,r-digest)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-latticeextra" ,r-latticeextra)
       ("r-matrixstats" ,r-matrixstats)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)))
    (home-page "https://bioconductor.org/packages/Gviz")
    (synopsis "Plotting data and annotation information along genomic coordinates")
    (description
     "Genomic data analyses requires integrated visualization of known genomic
information and new experimental data.  Gviz uses the biomaRt and the
rtracklayer packages to perform live annotation queries to Ensembl and UCSC
and translates this to e.g. gene/transcript structures in viewports of the
grid graphics package.  This results in genomic information plotted together
with your data.")
    (license license:artistic2.0)))

(define-public r-gwascat
  (package
    (name "r-gwascat")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gwascat" version))
       (sha256
        (base32
         "0n5x5i5v6a8wpn5mxmlpkl34b4kyypmymiwww6g61zch7xqrgywi"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationhub" ,r-annotationhub)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggbio" ,r-ggbio)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gqtlstats" ,r-gqtlstats)
       ("r-graph" ,r-graph)
       ("r-gviz" ,r-gviz)
       ("r-homo-sapiens" ,r-homo-sapiens)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-snpstats" ,r-snpstats)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://bioconductor.org/packages/gwascat")
    (synopsis "Tools for data in the EMBL-EBI GWAS catalog")
    (description
     "This package provides tools for representing and modeling data in the
EMBL-EBI GWAS catalog.")
    (license license:artistic2.0)))

(define-public r-sushi
  (package
    (name "r-sushi")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Sushi" version))
              (sha256
               (base32
                "0axaqm480z8d0b2ldgxwm0swava1p4irc62bpl17p2k8k78g687g"))))
    (properties `((upstream-name . "Sushi")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biomart" ,r-biomart)
       ("r-zoo" ,r-zoo)))
    (home-page "https://bioconductor.org/packages/Sushi")
    (synopsis "Tools for visualizing genomics data")
    (description
     "This package provides flexible, quantitative, and integrative genomic
visualizations for publication-quality multi-panel figures.")
    (license license:gpl2+)))

(define-public r-fithic
  (package
    (name "r-fithic")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FitHiC" version))
              (sha256
               (base32
                "12ylhrppi051m7nqsgq95kzd9g9wmp34i0zzfi55cjqawlpx7c6n"))))
    (properties `((upstream-name . "FitHiC")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-fdrtool" ,r-fdrtool)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://bioconductor.org/packages/FitHiC")
    (synopsis "Confidence estimation for intra-chromosomal contact maps")
    (description
     "Fit-Hi-C is a tool for assigning statistical confidence estimates to
intra-chromosomal contact maps produced by genome-wide genome architecture
assays such as Hi-C.")
    (license license:gpl2+)))

(define-public r-hitc
  (package
    (name "r-hitc")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HiTC" version))
              (sha256
               (base32
                "0288xa1jy6nzvz2ha07csmp6dirjw5r7p9vy69q2wsbyzr02ymkp"))))
    (properties `((upstream-name . "HiTC")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rtracklayer" ,r-rtracklayer)))
    (home-page "https://bioconductor.org/packages/HiTC")
    (synopsis "High throughput chromosome conformation capture analysis")
    (description
     "The HiTC package was developed to explore high-throughput \"C\" data
such as 5C or Hi-C.  Dedicated R classes as well as standard methods for
quality controls, normalization, visualization, and further analysis are also
provided.")
    (license license:artistic2.0)))

(define-public r-qvalue
  (package
    (name "r-qvalue")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "qvalue" version))
       (sha256
        (base32
         "1rd9rnf16kh8wc076kahd9hsb9rfwsbzmz3kjmp0pj6rbiq0051i"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-reshape2" ,r-reshape2)))
    (home-page "http://github.com/jdstorey/qvalue")
    (synopsis "Q-value estimation for false discovery rate control")
    (description
     "This package takes a list of p-values resulting from the simultaneous
testing of many hypotheses and estimates their q-values and local @dfn{false
discovery rate} (FDR) values.  The q-value of a test measures the proportion
of false positives incurred when that particular test is called significant.
The local FDR measures the posterior probability the null hypothesis is true
given the test's p-value.  Various plots are automatically generated, allowing
one to make sensible significance cut-offs.  The software can be applied to
problems in genomics, brain imaging, astrophysics, and data mining.")
    ;; Any version of the LGPL.
    (license license:lgpl3+)))

(define-public r-hdf5array
  (package
    (name "r-hdf5array")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HDF5Array" version))
       (sha256
        (base32
         "0kcdza41saqv6vlpvqd841awbiwkg84lh0plx6c7fmfgbqv7a0jh"))))
    (properties `((upstream-name . "HDF5Array")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-delayedarray" ,r-delayedarray)
       ("r-iranges" ,r-iranges)
       ("r-rhdf5" ,r-rhdf5)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/HDF5Array")
    (synopsis "HDF5 back end for DelayedArray objects")
    (description "This package provides an array-like container for convenient
access and manipulation of HDF5 datasets.  It supports delayed operations and
block processing.")
    (license license:artistic2.0)))

(define-public r-rhdf5lib
  (package
    (name "r-rhdf5lib")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhdf5lib" version))
       (sha256
        (base32
         "0kkc4rprjbqn2wvbx4d49kk9l91vihccxbl4843qr1wqk6v33r1w"))))
    (properties `((upstream-name . "Rhdf5lib")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-use-bundled-hdf5
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each delete-file '("configure" "configure.ac"))
             ;; Do not make other packages link with the proprietary libsz.
             (substitute* "R/zzz.R"
               (("'%s/libhdf5_cpp.a %s/libhdf5.a %s/libsz.a'")
                "'%s/libhdf5_cpp.a %s/libhdf5.a %s/libhdf5.a'")
               (("'%s/libhdf5.a %s/libsz.a'")
                "'%s/libhdf5.a %s/libhdf5.a'"))
             (with-directory-excursion "src"
               (invoke "tar" "xvf" (assoc-ref inputs "hdf5-source"))
               (rename-file (string-append "hdf5-" ,(package-version hdf5))
                            "hdf5")
               ;; Remove timestamp and host system information to make
               ;; the build reproducible.
               (substitute* "hdf5/src/libhdf5.settings.in"
                 (("Configured on: @CONFIG_DATE@")
                  "Configured on: Guix")
                 (("Uname information:.*")
                  "Uname information: Linux\n")
                 ;; Remove unnecessary store reference.
                 (("C Compiler:.*")
                  "C Compiler: GCC\n"))
               (rename-file "Makevars.in" "Makevars")
               (substitute* "Makevars"
                 (("HDF5_CXX_LIB=.*")
                  (string-append "HDF5_CXX_LIB="
                                 (assoc-ref inputs "hdf5") "/lib/libhdf5_cpp.a\n"))
                 (("HDF5_LIB=.*")
                  (string-append "HDF5_LIB="
                                 (assoc-ref inputs "hdf5") "/lib/libhdf5.a\n"))
                 (("HDF5_CXX_INCLUDE=.*") "HDF5_CXX_INCLUDE=./hdf5/c++/src\n")
                 (("HDF5_INCLUDE=.*") "HDF5_INCLUDE=./hdf5/src\n")
                 ;; szip is non-free software
                 (("cp \\$\\{SZIP_LIB\\}.*") "")
                 (("PKG_LIBS = \\$\\{HDF5_LIB\\} \\$\\{SZIP_LIB\\}")
                  "PKG_LIBS = ${HDF5_LIB}\n")))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("hdf5" ,hdf5)))
    (native-inputs
     `(("hdf5-source" ,(package-source hdf5))))
    (home-page "https://bioconductor.org/packages/Rhdf5lib")
    (synopsis "HDF5 library as an R package")
    (description "This package provides C and C++ HDF5 libraries for use in R
packages.")
    (license license:artistic2.0)))

(define-public r-beachmat
  (package
    (name "r-beachmat")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "beachmat" version))
       (sha256
        (base32
         "0b6dzja5fbx4dawb7ixj67mlhw4fy62pfp20mfp918fy96zmdwqz"))))
    (build-system r-build-system)
    (inputs
     `(("hdf5" ,hdf5)))
    (propagated-inputs
     `(("r-delayedarray" ,r-delayedarray)
       ("r-hdf5array" ,r-hdf5array)
       ("r-rcpp" ,r-rcpp)
       ("r-rhdf5" ,r-rhdf5)
       ("r-rhdf5lib" ,r-rhdf5lib)))
    (home-page "https://bioconductor.org/packages/beachmat")
    (synopsis "Compiling Bioconductor to handle each matrix type")
    (description "This package provides a consistent C++ class interface for a
variety of commonly used matrix types, including sparse and HDF5-backed
matrices.")
    (license license:gpl3)))

(define-public r-singlecellexperiment
  (package
    (name "r-singlecellexperiment")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SingleCellExperiment" version))
       (sha256
        (base32
         "1r276i97w64a5vdlg6952gkj7bls909p42zl8fn8yz87cdwyaars"))))
    (properties
     `((upstream-name . "SingleCellExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/SingleCellExperiment")
    (synopsis "S4 classes for single cell data")
    (description "This package defines an S4 class for storing data from
single-cell experiments.  This includes specialized methods to store and
retrieve spike-in information, dimensionality reduction coordinates and size
factors for each cell, along with the usual metadata for genes and
libraries.")
    (license license:gpl3)))

(define-public r-scater
  (package
    (name "r-scater")
    (version "1.6.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scater" version))
              (sha256
               (base32
                "0q3s96gf8saa1dq2fvmpl0jyj7bx3wrdfck3hanb8pxkcir2p7dn"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biomart" ,r-biomart)
       ("r-data-table" ,r-data-table)
       ("r-dplyr" ,r-dplyr)
       ("r-edger" ,r-edger)
       ("r-ggbeeswarm" ,r-ggbeeswarm)
       ("r-ggplot2" ,r-ggplot2)
       ("r-limma" ,r-limma)
       ("r-matrix" ,r-matrix)
       ("r-matrixstats" ,r-matrixstats)
       ("r-plyr" ,r-plyr)
       ("r-rcpp" ,r-rcpp)
       ("r-reshape2" ,r-reshape2)
       ("r-rhdf5" ,r-rhdf5)
       ("r-rhdf5lib" ,r-rhdf5lib)
       ("r-rjson" ,r-rjson)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shiny" ,r-shiny)
       ("r-shinydashboard" ,r-shinydashboard)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-tximport" ,r-tximport)
       ("r-viridis" ,r-viridis)))
    (home-page "https://github.com/davismcc/scater")
    (synopsis "Single-cell analysis toolkit for gene expression data in R")
    (description "This package provides a collection of tools for doing
various analyses of single-cell RNA-seq gene expression data, with a focus on
quality control.")
    (license license:gpl2+)))

(define-public r-scran
  (package
    (name "r-scran")
    (version "1.6.8")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scran" version))
       (sha256
        (base32
         "07wniyrh2fhhkz28v0bfgpvpi1hkkn2cvhacrvvvck142j79944x"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-dt" ,r-dt)
       ("r-dynamictreecut" ,r-dynamictreecut)
       ("r-edger" ,r-edger)
       ("r-fnn" ,r-fnn)
       ("r-ggplot2" ,r-ggplot2)
       ("r-igraph" ,r-igraph)
       ("r-limma" ,r-limma)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rhdf5lib" ,r-rhdf5lib)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scater" ,r-scater)
       ("r-shiny" ,r-shiny)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-statmod" ,r-statmod)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-viridis" ,r-viridis)
       ("r-zoo" ,r-zoo)))
    (home-page "https://bioconductor.org/packages/scran")
    (synopsis "Methods for single-cell RNA-Seq data analysis")
    (description "This package implements a variety of low-level analyses of
single-cell RNA-seq data.  Methods are provided for normalization of
cell-specific biases, assignment of cell cycle phase, and detection of highly
variable and significantly correlated genes.")
    (license license:gpl3)))

(define-public r-delayedmatrixstats
  (package
    (name "r-delayedmatrixstats")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DelayedMatrixStats" version))
       (sha256
        (base32
         "1cxjbjdq9hg9cm95rci0al7a4pk2h73ym276ahw9q4977zbg6381"))))
    (properties
     `((upstream-name . "DelayedMatrixStats")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-delayedarray" ,r-delayedarray)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-matrixstats" ,r-matrixstats)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://github.com/PeteHaitch/DelayedMatrixStats")
    (synopsis "Functions that apply to rows and columns of DelayedMatrix objects")
    (description
     "This package provides a port of the @code{matrixStats} API for use with
@code{DelayedMatrix} objects from the @code{DelayedArray} package.  It
contains high-performing functions operating on rows and columns of
@code{DelayedMatrix} objects, e.g. @code{colMedians}, @code{rowMedians},
@code{colRanks}, @code{rowRanks}, @code{colSds}, and @code{rowSds}.  Functions
are optimized per data type and for subsetted calculations such that both
memory usage and processing time is minimized.")
    (license license:expat)))

(define-public r-phangorn
  (package
    (name "r-phangorn")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "phangorn" version))
       (sha256
        (base32
         "0xc8k552nxczy19jr0xjjagrzc8x6lafasgk2c099ls8bc1yml1i"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ape" ,r-ape)
       ("r-fastmatch" ,r-fastmatch)
       ("r-igraph" ,r-igraph)
       ("r-magrittr" ,r-magrittr)
       ("r-matrix" ,r-matrix)
       ("r-quadprog" ,r-quadprog)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/KlausVigo/phangorn")
    (synopsis "Phylogenetic analysis in R")
    (description
     "Phangorn is a package for phylogenetic analysis in R.  It supports
estimation of phylogenetic trees and networks using Maximum Likelihood,
Maximum Parsimony, distance methods and Hadamard conjugation.")
    (license license:gpl2+)))

(define-public r-dropbead
  (let ((commit "d746c6f3b32110428ea56d6a0001ce52a251c247")
        (revision "2"))
    (package
      (name "r-dropbead")
      (version (string-append "0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rajewsky-lab/dropbead.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0sbzma49aiiyw8b0jpr7fnhzys9nsqmp4hy4hdz1gzyg1lhnca26"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-ggplot2" ,r-ggplot2)
         ("r-rcolorbrewer" ,r-rcolorbrewer)
         ("r-gridextra" ,r-gridextra)
         ("r-gplots" ,r-gplots)
         ("r-plyr" ,r-plyr)))
      (home-page "https://github.com/rajewsky-lab/dropbead")
      (synopsis "Basic exploration and analysis of Drop-seq data")
      (description "This package offers a quick and straight-forward way to
explore and perform basic analysis of single cell sequencing data coming from
droplet sequencing.  It has been particularly tailored for Drop-seq.")
      (license license:gpl3))))

(define htslib-for-sambamba
  (let ((commit "2f3c3ea7b301f9b45737a793c0b2dcf0240e5ee5"))
    (package
      (inherit htslib)
      (name "htslib-for-sambamba")
      (version (string-append "1.3.1-1." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lomereiter/htslib.git")
               (commit commit)))
         (file-name (string-append "htslib-" version "-checkout"))
         (sha256
          (base32
           "0g38g8s3npr0gjm9fahlbhiskyfws9l5i0x1ml3rakzj7az5l9c9"))))
      (arguments
       (substitute-keyword-arguments (package-arguments htslib)
         ((#:phases phases)
          `(modify-phases  ,phases
             (add-after 'unpack 'bootstrap
               (lambda _
                 (zero? (system* "autoreconf" "-vif"))))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ,@(package-native-inputs htslib))))))

(define-public sambamba
  (package
    (name "sambamba")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lomereiter/sambamba/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17076gijd65a3f07zns2gvbgahiz5lriwsa6dq353ss3jl85d8vy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there is no test target
       #:make-flags
       '("D_COMPILER=ldc2"
         ;; Override "--compiler" flag only.
         "D_FLAGS=--compiler=ldc2 -IBioD -g -d"
         "sambamba-ldmd2-64")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'place-biod
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "biod") "BioD")
             #t))
         (add-after 'unpack 'unbundle-prerequisites
           (lambda _
             (substitute* "Makefile"
               ((" htslib-static lz4-static") ""))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "build/sambamba" bin)
               #t))))))
    (native-inputs
     `(("ldc" ,ldc)
       ("rdmd" ,rdmd)
       ("biod"
        ,(let ((commit "1248586b54af4bd4dfb28ebfebfc6bf012e7a587"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/biod/BioD.git")
                   (commit commit)))
             (file-name (string-append "biod-"
                                       (string-take commit 9)
                                       "-checkout"))
             (sha256
              (base32
               "1m8hi1n7x0ri4l6s9i0x6jg4z4v94xrfdzp7mbizdipfag0m17g3")))))))
    (inputs
     `(("lz4" ,lz4)
       ("htslib" ,htslib-for-sambamba)))
    (home-page "http://lomereiter.github.io/sambamba")
    (synopsis "Tools for working with SAM/BAM data")
    (description "Sambamba is a high performance modern robust and
fast tool (and library), written in the D programming language, for
working with SAM and BAM files.  Current parallelised functionality is
an important subset of samtools functionality, including view, index,
sort, markdup, and depth.")
    (license license:gpl2+)))

(define-public ritornello
  (package
    (name "ritornello")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/KlugerLab/"
                                  "Ritornello/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02nik86gq9ljjriv6pamwlmqnfky3ads1fpklx6mc3hx6k40pg38"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-samtools-references
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("src/SamStream.h"
                            "src/BufferedGenomeReader.h")
               (("<sam.h>") "<samtools/sam.h>"))
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (bin    (string-append out "/bin/")))
               (mkdir-p bin)
               (install-file "bin/Ritornello" bin)
               #t))))))
    (inputs
     `(("samtools" ,samtools-0.1)
       ("fftw" ,fftw)
       ("boost" ,boost)
       ("zlib" ,zlib)))
    (home-page "https://github.com/KlugerLab/Ritornello")
    (synopsis "Control-free peak caller for ChIP-seq data")
    (description "Ritornello is a ChIP-seq peak calling algorithm based on
signal processing that can accurately call binding events without the need to
do a pair total DNA input or IgG control sample.  It has been tested for use
with narrow binding events such as transcription factor ChIP-seq.")
    (license license:gpl3+)))

(define-public trim-galore
  (package
    (name "trim-galore")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.bioinformatics.babraham.ac.uk/"
                           "projects/trim_galore/trim_galore_v"
                           version ".zip"))
       (sha256
        (base32
         "0b9qdxi4521gsrjvbhgky8g7kry9b5nx3byzaxkgxz7p4k8bn1mn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         ;; The archive contains plain files.
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (zero? (system* "unzip" source))))
         (delete 'configure)
         (delete 'build)
         (add-after 'unpack 'hardcode-tool-references
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "trim_galore"
               (("\\$path_to_cutadapt = 'cutadapt'")
                (string-append "$path_to_cutadapt = '"
                               (assoc-ref inputs "cutadapt")
                               "/bin/cutadapt'"))
               (("\\| gzip")
                (string-append "| "
                               (assoc-ref inputs "gzip")
                               "/bin/gzip"))
               (("\"gunzip")
                (string-append "\""
                               (assoc-ref inputs "gzip")
                               "/bin/gunzip")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (mkdir-p bin)
               (install-file "trim_galore" bin)
               #t))))))
    (inputs
     `(("gzip" ,gzip)
       ("perl" ,perl)
       ("cutadapt" ,cutadapt)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.bioinformatics.babraham.ac.uk/projects/trim_galore/")
    (synopsis "Wrapper around Cutadapt and FastQC")
    (description "Trim Galore! is a wrapper script to automate quality and
adapter trimming as well as quality control, with some added functionality to
remove biased methylation positions for RRBS sequence files.")
    (license license:gpl3+)))

(define-public gess
  (package
    (name "gess")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://compbio.uthscsa.edu/"
                                  "GESS_Web/files/"
                                  "gess-" version ".src.tar.gz"))
              (sha256
               (base32
                "0hyk403kxscclzfs24pvdgiv0wm03kjcziqdrp5w46cb049gz0d7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((python (assoc-ref inputs "python"))
                    (out    (assoc-ref outputs "out"))
                    (bin    (string-append out "/bin/"))
                    (target (string-append
                             out "/lib/python2.7/site-packages/gess/")))
               (mkdir-p target)
               (copy-recursively "." target)
               ;; Make GESS.py executable
               (chmod (string-append target "GESS.py") #o555)
               ;; Add Python shebang to the top and make Matplotlib
               ;; usable.
               (substitute* (string-append target "GESS.py")
                 (("\"\"\"Description:" line)
                  (string-append "#!" (which "python") "
import matplotlib
matplotlib.use('Agg')
" line)))
               ;; Make sure GESS has all modules in its path
               (wrap-program (string-append target "GESS.py")
                 `("PYTHONPATH" ":" prefix (,target ,(getenv "PYTHONPATH"))))
               (mkdir-p bin)
               (symlink (string-append target "GESS.py")
                        (string-append bin "GESS.py"))
               #t))))))
    (inputs
     `(("python" ,python-2)
       ("python2-pysam" ,python2-pysam)
       ("python2-scipy" ,python2-scipy)
       ("python2-numpy" ,python2-numpy)
       ("python2-networkx" ,python2-networkx)
       ("python2-biopython" ,python2-biopython)))
    (home-page "http://compbio.uthscsa.edu/GESS_Web/")
    (synopsis "Detect exon-skipping events from raw RNA-seq data")
    (description
     "GESS is an implementation of a novel computational method to detect de
novo exon-skipping events directly from raw RNA-seq data without the prior
knowledge of gene annotation information.  GESS stands for the graph-based
exon-skipping scanner detection scheme.")
    (license license:bsd-3)))

(define-public phylip
  (package
    (name "phylip")
    (version "3.696")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://evolution.gs.washington.edu/phylip/"
                           "download/phylip-" version ".tar.gz"))
       (sha256
        (base32
         "01jar1rayhr2gba2pgbw49m56rc5z4p5wn3ds0m188hrlln4a2nd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags (list "-f" "Makefile.unx" "install")
       #:parallel-build? #f             ; not supported
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "src") #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/bin")))
               (mkdir-p target)
               (for-each (lambda (file)
                           (install-file file target))
                         (find-files "../exe" ".*")))
             #t)))))
    (home-page "http://evolution.genetics.washington.edu/phylip/")
    (synopsis "Tools for inferring phylogenies")
    (description "PHYLIP (the PHYLogeny Inference Package) is a package of
programs for inferring phylogenies (evolutionary trees).")
    (license license:bsd-2)))

(define-public imp
  (package
    (name "imp")
    (version "2.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://integrativemodeling.org/"
                           version "/download/imp-" version ".tar.gz"))
       (sha256
        (base32
         "0lxqx7vh79d771svr611dkilp6sn30qrbw8zvscbrm37v38d2j6h"))))
    (build-system cmake-build-system)
    (arguments
     `(;; FIXME: Some tests fail because they produce warnings, others fail
       ;; because the PYTHONPATH does not include the modeller's directory.
       #:tests? #f))
    (inputs
     `(("boost" ,boost)
       ("gsl" ,gsl)
       ("swig" ,swig)
       ("hdf5" ,hdf5)
       ("fftw" ,fftw)
       ("python" ,python-2)))
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)
       ("python2-scipy" ,python2-scipy)
       ("python2-pandas" ,python2-pandas)
       ("python2-scikit-learn" ,python2-scikit-learn)
       ("python2-networkx" ,python2-networkx)))
    (home-page "https://integrativemodeling.org")
    (synopsis "Integrative modeling platform")
    (description "IMP's broad goal is to contribute to a comprehensive
structural characterization of biomolecules ranging in size and complexity
from small peptides to large macromolecular assemblies, by integrating data
from diverse biochemical and biophysical experiments.  IMP provides a C++ and
Python toolbox for solving complex modeling problems, and a number of
applications for tackling some common problems in a user-friendly way.")
    ;; IMP is largely available under the GNU Lesser GPL; see the file
    ;; COPYING.LGPL for the full text of this license. Some IMP modules are
    ;; available under the GNU GPL (see the file COPYING.GPL).
    (license (list license:lgpl2.1+
                   license:gpl3+))))

(define-public tadbit
  (package
    (name "tadbit")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/3DGenomes/TADbit/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cnfqrl4685zar4nnw94j94nhvl2h29jm448nadqi1h05z6fdk4f"))))
    (build-system python-build-system)
    (arguments
     `(;; Tests are included and must be run after installation, but
       ;; they are incomplete and thus cannot be run.
       #:tests? #f
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-problems-with-setup.py
           (lambda* (#:key outputs #:allow-other-keys)
             ;; setup.py opens these files for writing
             (chmod "_pytadbit/_version.py" #o664)
             (chmod "README.rst" #o664)

             ;; Don't attempt to install the bash completions to
             ;; the home directory.
             (rename-file "extras/.bash_completion"
                          "extras/tadbit")
             (substitute* "setup.py"
               (("\\(path.expanduser\\('~'\\)")
                (string-append "(\""
                               (assoc-ref outputs "out")
                               "/etc/bash_completion.d\""))
               (("extras/\\.bash_completion")
                "extras/tadbit"))
             #t)))))
    (inputs
     ;; TODO: add Chimera for visualization
     `(("imp" ,imp)
       ("mcl" ,mcl)
       ("python2-scipy" ,python2-scipy)
       ("python2-numpy" ,python2-numpy)
       ("python2-matplotlib" ,python2-matplotlib)
       ("python2-pysam" ,python2-pysam)))
    (home-page "http://3dgenomes.github.io/TADbit/")
    (synopsis "Analyze, model, and explore 3C-based data")
    (description
     "TADbit is a complete Python library to deal with all steps to analyze,
model, and explore 3C-based data.  With TADbit the user can map FASTQ files to
obtain raw interaction binned matrices (Hi-C like matrices), normalize and
correct interaction matrices, identify and compare the so-called
@dfn{Topologically Associating Domains} (TADs), build 3D models from the
interaction matrices, and finally, extract structural properties from the
models.  TADbit is complemented by TADkit for visualizing 3D models.")
    (license license:gpl3+)))

(define-public kentutils
  (package
    (name "kentutils")
    ;; 302.1.0 is out, but the only difference is the inclusion of
    ;; pre-built binaries.
    (version "302.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ENCODE-DCC/kentUtils/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "134aja3k1cj32kbk1nnw0q9gxjb2krr15q6sga8qldzvc0585rmm"))
       (modules '((guix build utils)
                  (srfi srfi-26)
                  (ice-9 ftw)))
       (snippet
        '(begin
           ;; Only the contents of the specified directories are free
           ;; for all uses, so we remove the rest.  "hg/autoSql" and
           ;; "hg/autoXml" are nominally free, but they depend on a
           ;; library that is built from the sources in "hg/lib",
           ;; which is nonfree.
           (let ((free (list "." ".."
                             "utils" "lib" "inc" "tagStorm"
                             "parasol" "htslib"))
                 (directory? (lambda (file)
                               (eq? 'directory (stat:type (stat file))))))
             (for-each (lambda (file)
                         (and (directory? file)
                              (delete-file-recursively file)))
                       (map (cut string-append "src/" <>)
                            (scandir "src"
                                     (lambda (file)
                                       (not (member file free)))))))
           ;; Only make the utils target, not the userApps target,
           ;; because that requires libraries we won't build.
           (substitute* "Makefile"
             ((" userApps") " utils"))
           ;; Only build libraries that are free.
           (substitute* "src/makefile"
             (("DIRS =.*") "DIRS =\n")
             (("cd jkOwnLib.*") "")
             ((" hgLib") "")
             (("cd hg.*") ""))
           (substitute* "src/utils/makefile"
             ;; These tools depend on "jkhgap.a", which is part of the
             ;; nonfree "src/hg/lib" directory.
             (("raSqlQuery") "")
             (("pslLiftSubrangeBlat") "")

             ;; Do not build UCSC tools, which may require nonfree
             ;; components.
             (("ALL_APPS =.*") "ALL_APPS = $(UTILS_APPLIST)\n"))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `( ;; There is no global test target and the test target for
       ;; individual tools depends on input files that are not
       ;; included.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda _
             (substitute* "Makefile"
               (("/bin/echo") (which "echo")))
             #t))
         (add-after 'unpack 'prepare-samtabix
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "samtabix")
                               "samtabix")
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (copy-recursively "bin" bin))
             #t)))))
    (native-inputs
     `(("samtabix"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "http://genome-source.cse.ucsc.edu/samtabix.git")
                 (commit "10fd107909c1ac4d679299908be4262a012965ba")))
           (sha256
            (base32
             "0c1nj64l42v395sa84n7az43xiap4i6f9n9dfz4058aqiwkhkmma"))))))
    (inputs
     `(("zlib" ,zlib)
       ("tcsh" ,tcsh)
       ("perl" ,perl)
       ("libpng" ,libpng)
       ("mysql" ,mysql)
       ("openssl" ,openssl)))
    (home-page "http://genome.cse.ucsc.edu/index.html")
    (synopsis "Assorted bioinformatics utilities")
    (description "This package provides the kentUtils, a selection of
bioinformatics utilities used in combination with the UCSC genome
browser.")
    ;; Only a subset of the sources are released under a non-copyleft
    ;; free software license.  All other sources are removed in a
    ;; snippet.  See this bug report for an explanation of how the
    ;; license statements apply:
    ;; https://github.com/ENCODE-DCC/kentUtils/issues/12
    (license (license:non-copyleft
              "http://genome.ucsc.edu/license/"
              "The contents of this package are free for all uses."))))

(define-public f-seq
  (let ((commit "6ccded34cff38cf432deed8503648b4a66953f9b")
        (revision "1"))
    (package
      (name "f-seq")
      (version (string-append "1.1-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/aboyle/F-seq.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "1nk33k0yajg2id4g59bc4szr58r2q6pdq42vgcw054m8ip9wv26h"))
                (modules '((guix build utils)))
                ;; Remove bundled Java library archives.
                (snippet
                 '(begin
                    (for-each delete-file (find-files "lib" ".*"))
                    #t))))
      (build-system ant-build-system)
      (arguments
       `(#:tests? #f ; no tests included
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((target (assoc-ref outputs "out"))
                      (doc (string-append target "/share/doc/f-seq/")))
                 (mkdir-p target)
                 (mkdir-p doc)
                 (substitute* "bin/linux/fseq"
                   (("java") (which "java"))
                   (("\\$REALDIR/../lib/commons-cli-1.1.jar")
                    (string-append (assoc-ref inputs "java-commons-cli")
                                   "/share/java/commons-cli.jar"))
                   (("REALDIR=.*")
                    (string-append "REALDIR=" target "/bin\n")))
                 (install-file "README.txt" doc)
                 (install-file "bin/linux/fseq" (string-append target "/bin"))
                 (install-file "build~/fseq.jar" (string-append target "/lib"))
                 (copy-recursively "lib" (string-append target "/lib"))
                 #t))))))
      (inputs
       `(("perl" ,perl)
         ("java-commons-cli" ,java-commons-cli)))
      (home-page "http://fureylab.web.unc.edu/software/fseq/")
      (synopsis "Feature density estimator for high-throughput sequence tags")
      (description
       "F-Seq is a software package that generates a continuous tag sequence
density estimation allowing identification of biologically meaningful sites
such as transcription factor binding sites (ChIP-seq) or regions of open
chromatin (DNase-seq).  Output can be displayed directly in the UCSC Genome
Browser.")
      (license license:gpl3+))))

(define-public bismark
  (package
    (name "bismark")
    (version "0.16.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/FelixKrueger/Bismark/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1204i0pa02ll2jn5pnxypkclnskvv7a2nwh5nxhagmhxk9wfv9sq"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin"))
                   (docdir  (string-append (assoc-ref outputs "out")
                                           "/share/doc/bismark"))
                   (docs    '("Bismark_User_Guide.pdf"
                              "RELEASE_NOTES.txt"))
                   (scripts '("bismark"
                              "bismark_genome_preparation"
                              "bismark_methylation_extractor"
                              "bismark2bedGraph"
                              "bismark2report"
                              "coverage2cytosine"
                              "deduplicate_bismark"
                              "bismark_sitrep.tpl"
                              "bam2nuc"
                              "bismark2summary")))
               (mkdir-p docdir)
               (mkdir-p bin)
               (for-each (lambda (file) (install-file file bin))
                         scripts)
               (for-each (lambda (file) (install-file file docdir))
                         docs)
               #t))))))
    (home-page "http://www.bioinformatics.babraham.ac.uk/projects/bismark/")
    (synopsis "Map bisulfite treated sequence reads and analyze methylation")
    (description "Bismark is a program to map bisulfite treated sequencing
reads to a genome of interest and perform methylation calls in a single step.
The output can be easily imported into a genome viewer, such as SeqMonk, and
enables a researcher to analyse the methylation levels of their samples
straight away.  Its main features are:

@itemize
@item Bisulfite mapping and methylation calling in one single step
@item Supports single-end and paired-end read alignments
@item Supports ungapped and gapped alignments
@item Alignment seed length, number of mismatches etc are adjustable
@item Output discriminates between cytosine methylation in CpG, CHG
  and CHH context
@end itemize\n")
    (license license:gpl3+)))

(define-public paml
  (package
    (name "paml")
    (version "4.9e")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://abacus.gene.ucl.ac.uk/software/"
                                  "paml" version ".tgz"))
              (sha256
               (base32
                "13zf6h9fiqghwhch2h06x1zdr6s42plsnqahflp5g7myr3han3s6"))
              (modules '((guix build utils)))
              ;; Remove Windows binaries
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.exe$"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "src/BFdriver.c"
               (("/bin/bash") (which "bash")))
             (chdir "src")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((tools '("baseml" "basemlg" "codeml"
                            "pamp" "evolver" "yn00" "chi2"))
                   (bin    (string-append (assoc-ref outputs "out") "/bin"))
                   (docdir (string-append (assoc-ref outputs "out")
                                           "/share/doc/paml")))
               (mkdir-p bin)
               (for-each (lambda (file) (install-file file bin)) tools)
               (copy-recursively "../doc" docdir)
               #t))))))
    (home-page "http://abacus.gene.ucl.ac.uk/software/paml.html")
    (synopsis "Phylogentic analysis by maximum likelihood")
    (description "PAML (for Phylogentic Analysis by Maximum Likelihood)
contains a few programs for model fitting and phylogenetic tree reconstruction
using nucleotide or amino-acid sequence data.")
    ;; GPLv3 only
    (license license:gpl3)))

(define-public kallisto
  (package
    (name "kallisto")
    (version "0.43.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/pachterlab/"
                                  "kallisto/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03j3iqhvq7ya3c91gidly3k3jvgm97vjq4scihrlxh315j696r11"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ; no "check" target
    (inputs
     `(("hdf5" ,hdf5)
       ("zlib" ,zlib)))
    (home-page "http://pachterlab.github.io/kallisto/")
    (synopsis "Near-optimal RNA-Seq quantification")
    (description
     "Kallisto is a program for quantifying abundances of transcripts from
RNA-Seq data, or more generally of target sequences using high-throughput
sequencing reads.  It is based on the novel idea of pseudoalignment for
rapidly determining the compatibility of reads with targets, without the need
for alignment.  Pseudoalignment of reads preserves the key information needed
for quantification, and kallisto is therefore not only fast, but also as
accurate as existing quantification tools.")
    (license license:bsd-2)))

(define-public libgff
  (package
    (name "libgff")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Kingsford-Group/"
                    "libgff/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vc4nxyhlm6g9vvmx5l4lfs5pnvixsv1hiiy4kddf2y3p6jna8ls"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ; no tests included
    (home-page "https://github.com/Kingsford-Group/libgff")
    (synopsis "Parser library for reading/writing GFF files")
    (description "This is a simple \"libraryfication\" of the GFF/GTF parsing
code that is used in the Cufflinks codebase.  The goal of this library is to
provide this functionality without the necessity of drawing in a heavy-weight
dependency like SeqAn.")
    (license (license:x11-style "http://www.boost.org/LICENSE_1_0.txt"))))

(define-public libdivsufsort
  (package
    (name "libdivsufsort")
    (version "2.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/y-256/libdivsufsort.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fgdz9fzihlvjjrxy01md1bv9vh12rkgkwbm90b1hj5xpbaqp7z2"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; there are no tests
       #:configure-flags
       ;; Needed for rapmap and sailfish.
       '("-DBUILD_DIVSUFSORT64=ON")))
    (home-page "https://github.com/y-256/libdivsufsort")
    (synopsis "Lightweight suffix-sorting library")
    (description "libdivsufsort is a software library that implements a
lightweight suffix array construction algorithm.  This library provides a
simple and an efficient C API to construct a suffix array and a
Burrows-Wheeler transformed string from a given string over a constant-size
alphabet.  The algorithm runs in O(n log n) worst-case time using only 5n+O(1)
bytes of memory space, where n is the length of the string.")
    (license license:expat)))

(define-public sailfish
  (package
    (name "sailfish")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/kingsfordgroup/"
                              "sailfish/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1inn60dxiwsz8g9w7kvfhjxj4bwfb0r12dyhpzzhfbig712dkmm0"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled headers for eigen3.
                  (delete-file-recursively "include/eigen3/")
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DBOOST_INCLUDEDIR="
                            (assoc-ref %build-inputs "boost")
                            "/include/")
             (string-append "-DBOOST_LIBRARYDIR="
                            (assoc-ref %build-inputs "boost")
                            "/lib/")
             (string-append "-DBoost_LIBRARIES="
                            "-lboost_iostreams "
                            "-lboost_filesystem "
                            "-lboost_system "
                            "-lboost_thread "
                            "-lboost_timer "
                            "-lboost_chrono "
                            "-lboost_program_options")
             "-DBoost_FOUND=TRUE"
             ;; Don't download RapMap---we already have it!
             "-DFETCHED_RAPMAP=1")
       ;; Tests must be run after installation and the location of the test
       ;; data file must be overridden.  But the tests fail.  It looks like
       ;; they are not really meant to be run.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; Boost cannot be found, even though it's right there.
         (add-after 'unpack 'do-not-look-for-boost
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("find_package\\(Boost 1\\.53\\.0") "#"))))
         (add-after 'unpack 'do-not-assign-to-macro
           (lambda _
             (substitute* "include/spdlog/details/format.cc"
               (("const unsigned CHAR_WIDTH = 1;") ""))))
         (add-after 'unpack 'prepare-rapmap
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((src "external/install/src/rapmap/")
                   (include "external/install/include/rapmap/")
                   (rapmap (assoc-ref inputs "rapmap")))
               (mkdir-p "/tmp/rapmap")
               (system* "tar" "xf"
                        (assoc-ref inputs "rapmap")
                        "-C" "/tmp/rapmap"
                        "--strip-components=1")
               (mkdir-p src)
               (mkdir-p include)
               (for-each (lambda (file)
                           (install-file file src))
                         (find-files "/tmp/rapmap/src" "\\.(c|cpp)"))
               (copy-recursively "/tmp/rapmap/include" include))))
         (add-after 'unpack 'use-system-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("src/SailfishIndexer.cpp"
                            "src/SailfishUtils.cpp"
                            "src/SailfishQuantify.cpp"
                            "src/FASTAParser.cpp"
                            "include/PCA.hpp"
                            "include/SailfishUtils.hpp"
                            "include/SailfishIndex.hpp"
                            "include/CollapsedEMOptimizer.hpp"
                            "src/CollapsedEMOptimizer.cpp")
               (("#include \"jellyfish/config.h\"") ""))
             (substitute* "src/CMakeLists.txt"
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/include/jellyfish-2.2..")
                (string-append (assoc-ref inputs "jellyfish")
                               "/include/jellyfish-" ,(package-version jellyfish)))
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/lib/libjellyfish-2.0.a")
                (string-append (assoc-ref inputs "jellyfish")
                               "/lib/libjellyfish-2.0.a"))
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/lib/libdivsufsort.a")
                (string-append (assoc-ref inputs "libdivsufsort")
                               "/lib/libdivsufsort.so"))
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/lib/libdivsufsort64.a")
                (string-append (assoc-ref inputs "libdivsufsort")
                               "/lib/libdivsufsort64.so")))
             (substitute* "CMakeLists.txt"
               ;; Don't prefer static libs
               (("SET\\(CMAKE_FIND_LIBRARY_SUFFIXES.*") "")
               (("find_package\\(Jellyfish.*") "")
               (("ExternalProject_Add\\(libjellyfish") "message(")
               (("ExternalProject_Add\\(libgff") "message(")
               (("ExternalProject_Add\\(libsparsehash") "message(")
               (("ExternalProject_Add\\(libdivsufsort") "message("))

             ;; Ensure that Eigen headers can be found
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (getenv "CPLUS_INCLUDE_PATH")
                                    ":"
                                    (assoc-ref inputs "eigen")
                                    "/include/eigen3")))))))
    (inputs
     `(("boost" ,boost)
       ("eigen" ,eigen)
       ("jemalloc" ,jemalloc)
       ("jellyfish" ,jellyfish)
       ("sparsehash" ,sparsehash)
       ("rapmap" ,(origin
                    (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/COMBINE-lab/RapMap.git")
                          (commit (string-append "sf-v" version))))
                    (file-name (string-append "rapmap-sf-v" version "-checkout"))
                    (sha256
                     (base32
                      "1hv79l5i576ykv5a1srj2p0q36yvyl5966m0fcy2lbi169ipjakf"))
                    (modules '((guix build utils)))
                    ;; These files are expected to be excluded.
                    (snippet
                     '(begin (delete-file-recursively "include/spdlog")
                             (for-each delete-file '("include/xxhash.h"
                                                     "src/xxhash.c"))))))
       ("libdivsufsort" ,libdivsufsort)
       ("libgff" ,libgff)
       ("tbb" ,tbb)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.cs.cmu.edu/~ckingsf/software/sailfish")
    (synopsis "Mapping-based isoform quantification from RNA-Seq reads")
    (description "Sailfish is a tool for genomic transcript quantification
from RNA-seq data.  It requires a set of target transcripts (either from a
reference or de-novo assembly) to quantify.  All you need to run sailfish is a
fasta file containing your reference transcripts and a (set of) fasta/fastq
file(s) containing your reads.")
    (license license:gpl3+)))

(define libstadenio-for-salmon
  (package
    (name "libstadenio")
    (version "1.14.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/COMBINE-lab/staden-io_lib.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1x8kxxqxl892vwfbprlbyfwkkv7c34ggkc94892x9x0g37x5nbwx"))))
    (build-system gnu-build-system)
    (arguments '(#:parallel-tests? #f)) ; not supported
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)))                 ; for tests
    (home-page "https://github.com/COMBINE-lab/staden-io_lib")
    (synopsis "General purpose trace and experiment file library")
    (description "This package provides a library of file reading and writing
code to provide a general purpose Trace file (and Experiment File) reading
interface.

The following file formats are supported:

@enumerate
@item SCF trace files
@item ABI trace files
@item ALF trace files
@item ZTR trace files
@item SFF trace archives
@item SRF trace archives
@item Experiment files
@item Plain text files
@item SAM/BAM sequence files
@item CRAM sequence files
@end enumerate\n")
    (license license:bsd-3)))

(define spdlog-for-salmon
  (package
    (name "spdlog")
    (version "0.14.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/COMBINE-lab/spdlog.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "13730429gwlabi432ilpnja3sfvy0nn2719vnhhmii34xcdyc57q"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/COMBINE-lab/spdlog")
    (synopsis "Very fast C++ logging library")
    (description "Spdlog is a very fast header-only C++ logging library with
performance as its primary goal.")
    (license license:expat)))

;; This is a modified variant of bwa for use with Salmon. It installs a
;; library to avoid having to build this as part of Salmon.
(define bwa-for-salmon
  (package (inherit bwa)
    (name "bwa")
    (version "0.7.12.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/COMBINE-lab/bwa.git")
                    (commit (string-append "v" version))))
              (file-name (string-append "bwa-for-salmon-" version "-checkout"))
              (sha256
               (base32
                "1z2qa64y0c5hky10510x137mnzlhz6k8qf27csw4w9j6qihq95gb"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (doc (string-append out "/share/doc/bwa"))
                    (man (string-append out "/share/man/man1"))
                    (inc (string-append out "/include/bwa")))
               (install-file "bwa" bin)
               (install-file "README.md" doc)
               (install-file "bwa.1" man)
               (install-file "libbwa.a" lib)
               (mkdir-p lib)
               (mkdir-p inc)
               (for-each (lambda (file)
                           (install-file file inc))
                         (find-files "." "\\.h$")))
             #t))
         ;; no "configure" script
         (delete 'configure))))))

(define-public salmon
  (package
    (name "salmon")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/COMBINE-lab/salmon.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1zi1ff4i7y2ykk0vdzysgwzzzv166vg2x77pj1mf4baclavxj87a"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled headers for eigen3.
                  (delete-file-recursively "include/eigen3/")
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DBOOST_INCLUDEDIR="
                            (assoc-ref %build-inputs "boost")
                            "/include/")
             (string-append "-DBOOST_LIBRARYDIR="
                            (assoc-ref %build-inputs "boost")
                            "/lib/")
             (string-append "-DBoost_LIBRARIES="
                            "-lboost_iostreams "
                            "-lboost_filesystem "
                            "-lboost_system "
                            "-lboost_thread "
                            "-lboost_timer "
                            "-lboost_chrono "
                            "-lboost_program_options")
             "-DBoost_FOUND=TRUE"
             "-DTBB_LIBRARIES=tbb tbbmalloc"
             ;; Don't download RapMap---we already have it!
             "-DFETCHED_RAPMAP=1")
       #:phases
       (modify-phases %standard-phases
         ;; Boost cannot be found, even though it's right there.
         (add-after 'unpack 'do-not-look-for-boost
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("find_package\\(Boost 1\\.53\\.0") "#"))))
         (add-after 'unpack 'do-not-phone-home
           (lambda _
             (substitute* "src/Salmon.cpp"
               (("getVersionMessage\\(\\)") "\"\""))))
         (add-after 'unpack 'prepare-rapmap
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((src "external/install/src/rapmap/")
                   (include "external/install/include/rapmap/")
                   (rapmap (assoc-ref inputs "rapmap")))
               (mkdir-p src)
               (mkdir-p include)
               (for-each (lambda (file)
                           (install-file file src))
                         (find-files (string-append rapmap "/src") "\\.(c|cpp)"))
               (copy-recursively (string-append rapmap "/include") include)
               (for-each delete-file '("external/install/include/rapmap/xxhash.h"
                                       "external/install/include/rapmap/FastxParser.hpp"
                                       "external/install/include/rapmap/concurrentqueue.h"
                                       "external/install/include/rapmap/FastxParserThreadUtils.hpp"
                                       "external/install/src/rapmap/FastxParser.cpp"
                                       "external/install/src/rapmap/xxhash.c")))))
         (add-after 'unpack 'use-system-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/CMakeLists.txt"
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/include/jellyfish-2.2..")
                (string-append (assoc-ref inputs "jellyfish")
                               "/include/jellyfish-" ,(package-version jellyfish)))
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/lib/libjellyfish-2.0.a")
                (string-append (assoc-ref inputs "jellyfish")
                               "/lib/libjellyfish-2.0.a"))
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/lib/libdivsufsort.a")
                (string-append (assoc-ref inputs "libdivsufsort")
                               "/lib/libdivsufsort.so"))
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/lib/libstaden-read.a")
                (string-append (assoc-ref inputs "libstadenio-for-salmon")
                               "/lib/libstaden-read.a"))
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/lib/libbwa.a")
                (string-append (assoc-ref inputs "bwa") "/lib/libbwa.a"))
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/lib/libdivsufsort64.a")
                (string-append (assoc-ref inputs "libdivsufsort")
                               "/lib/libdivsufsort64.so")))
             (substitute* "CMakeLists.txt"
               ;; Don't prefer static libs
               (("SET\\(CMAKE_FIND_LIBRARY_SUFFIXES.*") "")
               (("set\\(TBB_LIBRARIES") "message(")
               (("find_package\\(Jellyfish.*") "")
               (("ExternalProject_Add\\(libcereal") "message(")
               (("ExternalProject_Add\\(libbwa") "message(")
               (("ExternalProject_Add\\(libjellyfish") "message(")
               (("ExternalProject_Add\\(libgff") "message(")
               (("ExternalProject_Add\\(libtbb") "message(")
               (("ExternalProject_Add\\(libspdlog") "message(")
               (("ExternalProject_Add\\(libdivsufsort") "message(")
               (("ExternalProject_Add\\(libstadenio") "message(")
               (("ExternalProject_Add_Step\\(") "message("))

             ;; Ensure that all headers can be found
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (getenv "CPLUS_INCLUDE_PATH")
                                    ":"
                                    (assoc-ref inputs "bwa")
                                    "/include/bwa"
                                    ":"
                                    (assoc-ref inputs "eigen")
                                    "/include/eigen3"))
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "bwa")
                                    "/include/bwa"
                                    ":"
                                    (assoc-ref inputs "eigen")
                                    "/include/eigen3"))
             #t))
         ;; CMAKE_INSTALL_PREFIX does not exist when the tests are
         ;; run.  It only exists after the install phase.
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "src/CMakeLists.txt"
               (("DTOPLEVEL_DIR=\\$\\{CMAKE_INSTALL_PREFIX")
                "DTOPLEVEL_DIR=${GAT_SOURCE_DIR"))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("bwa" ,bwa-for-salmon)
       ("bzip2" ,bzip2)
       ("cereal" ,cereal)
       ("eigen" ,eigen)
       ("rapmap" ,(origin
                    (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/COMBINE-lab/RapMap.git")
                          (commit (string-append "salmon-v" version))))
                    (file-name (string-append "rapmap-salmon-v" version "-checkout"))
                    (sha256
                     (base32
                      "1yc12yqsz6f0r8sg1qnk57xg34aqwc9jbqq6gd5ys28xw3plj98p"))))
       ("jemalloc" ,jemalloc)
       ("jellyfish" ,jellyfish)
       ("libgff" ,libgff)
       ("tbb" ,tbb)
       ("libdivsufsort" ,libdivsufsort)
       ("libstadenio-for-salmon" ,libstadenio-for-salmon)
       ("spdlog-for-salmon" ,spdlog-for-salmon)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (home-page "https://github.com/COMBINE-lab/salmon")
    (synopsis "Quantification from RNA-seq reads using lightweight alignments")
    (description "Salmon is a program to produce highly-accurate,
transcript-level quantification estimates from RNA-seq data.  Salmon achieves
its accuracy and speed via a number of different innovations, including the
use of lightweight alignments (accurate but fast-to-compute proxies for
traditional read alignments) and massively-parallel stochastic collapsed
variational inference.")
    (license license:gpl3+)))

(define-public python-loompy
  (package
    (name "python-loompy")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "loompy" version))
       (sha256
        (base32
         "1drgv8j1hxqzzpnfg272x9djb6j8qr798w1pc2x8ikmfgyd9gh51"))))
    (build-system python-build-system)
    ;; There are no tests
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-h5py" ,python-h5py)
       ("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-typing" ,python-typing)))
    (home-page "https://github.com/linnarsson-lab/loompy")
    (synopsis "Work with .loom files for single-cell RNA-seq data")
    (description "The loom file format is an efficient format for very large
omics datasets, consisting of a main matrix, optional additional layers, a
variable number of row and column annotations.  Loom also supports sparse
graphs.  This library makes it easy to work with @file{.loom} files for
single-cell RNA-seq data.")
    (license license:bsd-3)))

;; We cannot use the latest commit because it requires Java 9.
(define-public java-forester
  (let ((commit "86b07efe302d5094b42deed9260f719a4c4ac2e6")
        (revision "1"))
    (package
      (name "java-forester")
      (version (string-append "0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/cmzmasek/forester.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0vxavc1yrf84yrnf20dq26hi0lglidk8d382xrxsy4qmlbjd276z"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Delete bundled jars and pre-built classes
                    (delete-file-recursively "forester/java/resources")
                    (delete-file-recursively "forester/java/classes")
                    (for-each delete-file (find-files "forester/java/" "\\.jar$"))
                    ;; Delete bundled applications
                    (delete-file-recursively "forester_applications")
                    #t))))
      (build-system ant-build-system)
      (arguments
       `(#:tests? #f ; there are none
         #:jdk ,icedtea-8
         #:modules ((guix build ant-build-system)
                    (guix build utils)
                    (guix build java-utils)
                    (sxml simple)
                    (sxml transform))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "forester/java") #t))
           (add-after 'chdir 'fix-dependencies
             (lambda _
               (chmod "build.xml" #o664)
               (call-with-output-file "build.xml.new"
                 (lambda (port)
                   (sxml->xml
                    (pre-post-order
                     (with-input-from-file "build.xml"
                       (lambda _ (xml->sxml #:trim-whitespace? #t)))
                     `(;; Remove all unjar tags to avoid repacking classes.
                       (unjar     . ,(lambda _ '()))
                       (*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
                       (*text*    . ,(lambda (_ txt) txt))))
                    port)))
               (rename-file "build.xml.new" "build.xml")
               #t))
           ;; FIXME: itext is difficult to package as it depends on a few
           ;; unpackaged libraries.
           (add-after 'chdir 'remove-dependency-on-unpackaged-itext
             (lambda _
               (delete-file "src/org/forester/archaeopteryx/PdfExporter.java")
               (substitute* "src/org/forester/archaeopteryx/MainFrame.java"
                 (("pdf_written_to = PdfExporter.*")
                  "throw new IOException(\"PDF export is not available.\");"))
               #t))
           ;; There is no install target
           (replace 'install (install-jars ".")))))
      (propagated-inputs
       `(("java-commons-codec" ,java-commons-codec)
         ("java-openchart2" ,java-openchart2)))
      (home-page "https://sites.google.com/site/cmzmasek/home/software/forester")
      (synopsis "Phylogenomics libraries for Java")
      (description "Forester is a collection of Java libraries for
phylogenomics and evolutionary biology research.  It includes support for
reading, writing, and exporting phylogenetic trees.")
      (license license:lgpl2.1+))))

(define-public java-forester-1.005
  (package
    (name "java-forester")
    (version "1.005")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://search.maven.org/remotecontent?"
                                  "filepath=org/biojava/thirdparty/forester/"
                                  version "/forester-" version "-sources.jar"))
              (file-name (string-append name "-" version ".jar"))
              (sha256
               (base32
                "04r8qv4rk3p71z4ajrvp11py1z46qrx0047j3zzs79s6lnsm3lcv"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; there are none
       #:jdk ,icedtea-8
       #:modules ((guix build ant-build-system)
                  (guix build utils)
                  (guix build java-utils)
                  (sxml simple)
                  (sxml transform))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dependencies
           (lambda* (#:key inputs #:allow-other-keys)
             (call-with-output-file "build.xml"
               (lambda (port)
                 (sxml->xml
                  (pre-post-order
                   (with-input-from-file "src/build.xml"
                     (lambda _ (xml->sxml #:trim-whitespace? #t)))
                   `(;; Remove all unjar tags to avoid repacking classes.
                     (unjar     . ,(lambda _ '()))
                     (*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
                     (*text*    . ,(lambda (_ txt) txt))))
                  port)))
             (copy-file (assoc-ref inputs "synth_look_and_feel_1.xml")
                        "synth_look_and_feel_1.xml")
             (copy-file (assoc-ref inputs "phyloxml.xsd")
                        "phyloxml.xsd")
             (substitute* "build.xml"
               (("../resources/synth_laf/synth_look_and_feel_1.xml")
                "synth_look_and_feel_1.xml")
               (("../resources/phyloxml_schema/1.10/phyloxml.xsd")
                "phyloxml.xsd"))
             #t))
         ;; FIXME: itext is difficult to package as it depends on a few
         ;; unpackaged libraries.
         (add-after 'unpack 'remove-dependency-on-unpackaged-itext
           (lambda _
             (delete-file "src/org/forester/archaeopteryx/PdfExporter.java")
             (substitute* '("src/org/forester/archaeopteryx/MainFrame.java"
                            "src/org/forester/archaeopteryx/MainFrameApplication.java")
               (("pdf_written_to = PdfExporter.*")
                "throw new IOException(\"PDF export is not available.\"); /*")
               ((".getPrintSizeX\\(\\), getOptions\\(\\).getPrintSizeY\\(\\) \\);") "*/")
               (("getCurrentTreePanel\\(\\).getHeight\\(\\) \\);") "*/"))
             #t))
         (add-after 'unpack 'delete-pre-built-classes
           (lambda _ (delete-file-recursively "src/classes") #t))
         ;; There is no install target
         (replace 'install (install-jars ".")))))
    (propagated-inputs
     `(("java-commons-codec" ,java-commons-codec)
       ("java-openchart2" ,java-openchart2)))
    ;; The source archive does not contain the resources.
    (native-inputs
     `(("phyloxml.xsd"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://raw.githubusercontent.com/cmzmasek/forester/"
                               "b61cc2dcede0bede317db362472333115756b8c6/"
                               "forester/resources/phyloxml_schema/1.10/phyloxml.xsd"))
           (file-name (string-append name "-phyloxml-" version ".xsd"))
           (sha256
            (base32
             "1zxc4m8sn4n389nqdnpxa8d0k17qnr3pm2y5y6g6vh4k0zm52npv"))))
       ("synth_look_and_feel_1.xml"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://raw.githubusercontent.com/cmzmasek/forester/"
                               "29e04321615da6b35c1e15c60e52caf3f21d8e6a/"
                               "forester/java/classes/resources/synth_look_and_feel_1.xml"))
           (file-name (string-append name "-synth-look-and-feel-" version ".xml"))
           (sha256
            (base32
             "1gv5602gv4k7y7713y75a4jvj7i9s7nildsbdl7n9q10sc2ikg8h"))))))
    (home-page "https://sites.google.com/site/cmzmasek/home/software/forester")
    (synopsis "Phylogenomics libraries for Java")
    (description "Forester is a collection of Java libraries for
phylogenomics and evolutionary biology research.  It includes support for
reading, writing, and exporting phylogenetic trees.")
    (license license:lgpl2.1+)))

(define-public java-biojava-core
  (package
    (name "java-biojava-core")
    (version "4.2.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/biojava/biojava")
                    (commit (string-append "biojava-" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1bvryh2bpsvash8ln79cmc9sqm8qw72hz4xzwqxcrjm8ssxszhqk"))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:jar-name "biojava-core.jar"
       #:source-dir "biojava-core/src/main/java/"
       #:test-dir "biojava-core/src/test"
       ;; These tests seem to require internet access.
       #:test-exclude (list "**/SearchIOTest.java"
                            "**/BlastXMLParserTest.java"
                            "**/GenbankCookbookTest.java"
                            "**/GenbankProxySequenceReaderTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "biojava-core/src/main/resources"
                               "build/classes")
             #t))
         (add-before 'check 'copy-test-resources
           (lambda _
             (copy-recursively "biojava-core/src/test/resources"
                               "build/test-classes")
             #t)))))
    (propagated-inputs
     `(("java-log4j-api" ,java-log4j-api)
       ("java-log4j-core" ,java-log4j-core)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-simple" ,java-slf4j-simple)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://biojava.org")
    (synopsis "Core libraries of Java framework for processing biological data")
    (description "BioJava is a project dedicated to providing a Java framework
for processing biological data.  It provides analytical and statistical
routines, parsers for common file formats, reference implementations of
popular algorithms, and allows the manipulation of sequences and 3D
structures.  The goal of the biojava project is to facilitate rapid
application development for bioinformatics.

This package provides the core libraries.")
    (license license:lgpl2.1+)))

(define-public java-biojava-phylo
  (package (inherit java-biojava-core)
    (name "java-biojava-phylo")
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:jar-name "biojava-phylo.jar"
       #:source-dir "biojava-phylo/src/main/java/"
       #:test-dir "biojava-phylo/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "biojava-phylo/src/main/resources"
                               "build/classes")
             #t))
         (add-before 'check 'copy-test-resources
           (lambda _
             (copy-recursively "biojava-phylo/src/test/resources"
                               "build/test-classes")
             #t)))))
    (propagated-inputs
     `(("java-log4j-api" ,java-log4j-api)
       ("java-log4j-core" ,java-log4j-core)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-simple" ,java-slf4j-simple)
       ("java-biojava-core" ,java-biojava-core)
       ("java-forester" ,java-forester)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://biojava.org")
    (synopsis "Biojava interface to the forester phylogenomics library")
    (description "The phylo module provides a biojava interface layer to the
forester phylogenomics library for constructing phylogenetic trees.")))

(define-public java-biojava-alignment
  (package (inherit java-biojava-core)
    (name "java-biojava-alignment")
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:jar-name "biojava-alignment.jar"
       #:source-dir "biojava-alignment/src/main/java/"
       #:test-dir "biojava-alignment/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "biojava-alignment/src/main/resources"
                               "build/classes")
             #t))
         (add-before 'check 'copy-test-resources
           (lambda _
             (copy-recursively "biojava-alignment/src/test/resources"
                               "build/test-classes")
             #t)))))
    (propagated-inputs
     `(("java-log4j-api" ,java-log4j-api)
       ("java-log4j-core" ,java-log4j-core)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-simple" ,java-slf4j-simple)
       ("java-biojava-core" ,java-biojava-core)
       ("java-biojava-phylo" ,java-biojava-phylo)
       ("java-forester" ,java-forester)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://biojava.org")
    (synopsis "Biojava API for genetic sequence alignment")
    (description "The alignment module of BioJava provides an API that
contains

@itemize
@item implementations of dynamic programming algorithms for sequence
  alignment;
@item reading and writing of popular alignment file formats;
@item a single-, or multi- threaded multiple sequence alignment algorithm.
@end itemize\n")))

(define-public java-biojava-core-4.0
  (package (inherit java-biojava-core)
    (name "java-biojava-core")
    (version "4.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/biojava/biojava")
                    (commit (string-append "biojava-" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "13675f6y9aqi7bi2lk3s1z7a22ynccjiqwa8izh7p97xi9wsfmd8"))))))

(define-public java-biojava-phylo-4.0
  (package (inherit java-biojava-core-4.0)
    (name "java-biojava-phylo")
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:jar-name "biojava-phylo.jar"
       #:source-dir "biojava-phylo/src/main/java/"
       #:test-dir "biojava-phylo/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "biojava-phylo/src/main/resources"
                               "build/classes")
             #t))
         (add-before 'check 'copy-test-resources
           (lambda _
             (copy-recursively "biojava-phylo/src/test/resources"
                               "build/test-classes")
             #t)))))
    (propagated-inputs
     `(("java-log4j-api" ,java-log4j-api)
       ("java-log4j-core" ,java-log4j-core)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-simple" ,java-slf4j-simple)
       ("java-biojava-core" ,java-biojava-core-4.0)
       ("java-forester" ,java-forester-1.005)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://biojava.org")
    (synopsis "Biojava interface to the forester phylogenomics library")
    (description "The phylo module provides a biojava interface layer to the
forester phylogenomics library for constructing phylogenetic trees.")))

(define-public java-biojava-alignment-4.0
  (package (inherit java-biojava-core-4.0)
    (name "java-biojava-alignment")
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:jar-name "biojava-alignment.jar"
       #:source-dir "biojava-alignment/src/main/java/"
       #:test-dir "biojava-alignment/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "biojava-alignment/src/main/resources"
                               "build/classes")
             #t))
         (add-before 'check 'copy-test-resources
           (lambda _
             (copy-recursively "biojava-alignment/src/test/resources"
                               "build/test-classes")
             #t)))))
    (propagated-inputs
     `(("java-log4j-api" ,java-log4j-api)
       ("java-log4j-core" ,java-log4j-core)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-simple" ,java-slf4j-simple)
       ("java-biojava-core" ,java-biojava-core-4.0)
       ("java-biojava-phylo" ,java-biojava-phylo-4.0)
       ("java-forester" ,java-forester-1.005)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://biojava.org")
    (synopsis "Biojava API for genetic sequence alignment")
    (description "The alignment module of BioJava provides an API that
contains

@itemize
@item implementations of dynamic programming algorithms for sequence
  alignment;
@item reading and writing of popular alignment file formats;
@item a single-, or multi- threaded multiple sequence alignment algorithm.
@end itemize\n")))

(define-public dropseq-tools
  (package
    (name "dropseq-tools")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri "http://mccarrolllab.com/download/1276/")
       (file-name (string-append "dropseq-tools-" version ".zip"))
       (sha256
        (base32
         "0yrffckxqk5l8b5xb6z4laq157zd9mdypr2p4b4vq2bhjzi1sj0s"))
       ;; Delete bundled libraries
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file (find-files "jar/lib" "\\.jar$"))
           (delete-file-recursively "3rdParty")))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f                      ; test data are not included
       #:test-target "test"
       #:build-target "all"
       #:source-dir "public/src/"
       #:jdk ,icedtea-8
       #:make-flags
       (list (string-append "-Dpicard.executable.dir="
                            (assoc-ref %build-inputs "java-picard")
                            "/share/java/"))
       #:modules ((ice-9 match)
                  (srfi srfi-1)
                  (guix build utils)
                  (guix build java-utils)
                  (guix build ant-build-system))
       #:phases
       (modify-phases %standard-phases
         ;; All dependencies must be linked to "lib", because that's where
         ;; they will be searched for when the Class-Path property of the
         ;; manifest is computed.
         (add-after 'unpack 'record-references
	   (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "jar/lib")
             (let ((dirs (filter-map (match-lambda
                                       ((name . dir)
                                        (if (and (string-prefix? "java-" name)
                                                 (not (string=? name "java-testng")))
                                            dir #f)))
                                     inputs)))
               (for-each (lambda (jar)
                           (symlink jar (string-append "jar/lib/" (basename jar))))
                         (append-map (lambda (dir) (find-files dir "\\.jar$"))
                                     dirs)))
	     #t))
         ;; There is no installation target
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (bin     (string-append out "/bin"))
                    (share   (string-append out "/share/java/"))
                    (lib     (string-append share "/lib/"))
                    (scripts (list "BAMTagHistogram"
                                   "BAMTagofTagCounts"
                                   "BaseDistributionAtReadPosition"
                                   "CollapseBarcodesInPlace"
                                   "CollapseTagWithContext"
                                   "ConvertToRefFlat"
                                   "CreateIntervalsFiles"
                                   "DetectBeadSynthesisErrors"
                                   "DigitalExpression"
                                   "Drop-seq_alignment.sh"
                                   "FilterBAM"
                                   "FilterBAMByTag"
                                   "GatherGeneGCLength"
                                   "GatherMolecularBarcodeDistributionByGene"
                                   "GatherReadQualityMetrics"
                                   "PolyATrimmer"
                                   "ReduceGTF"
                                   "SelectCellsByNumTranscripts"
                                   "SingleCellRnaSeqMetricsCollector"
                                   "TagBamWithReadSequenceExtended"
                                   "TagReadWithGeneExon"
                                   "TagReadWithInterval"
                                   "TrimStartingSequence"
                                   "ValidateReference")))
               (for-each mkdir-p (list bin share lib))
               (install-file "dist/dropseq.jar" share)
               (for-each (lambda (script)
                           (chmod script #o555)
                           (install-file script bin))
                         scripts)
               (substitute* (map (lambda (script)
                                   (string-append bin "/" script))
                                 scripts)
                 (("^java") (which "java"))
                 (("jar_deploy_dir=.*")
                  (string-append "jar_deploy_dir=" share "\n"))))
             #t))
         ;; FIXME: We do this after stripping jars because we don't want it to
         ;; copy all these jars and strip them.  We only want to install
         ;; links.  Arguably, this is a problem with the ant-build-system.
         (add-after 'strip-jar-timestamps 'install-links
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (share   (string-append out "/share/java/"))
                    (lib     (string-append share "/lib/")))
               (for-each (lambda (jar)
                           (symlink (readlink jar)
                                    (string-append lib (basename jar))))
                         (find-files "jar/lib" "\\.jar$")))
             #t)))))
    (inputs
     `(("jdk" ,icedtea-8)
       ("java-picard" ,java-picard-2.10.3)
       ("java-log4j-1.2-api" ,java-log4j-1.2-api)
       ("java-commons-math3" ,java-commons-math3)
       ("java-commons-jexl2" ,java-commons-jexl-2)
       ("java-commons-collections4" ,java-commons-collections4)
       ("java-commons-lang2" ,java-commons-lang)
       ("java-commons-io" ,java-commons-io)
       ("java-snappy-1.0.3-rc3" ,java-snappy-1)
       ("java-guava" ,java-guava)
       ("java-la4j" ,java-la4j)
       ("java-biojava-core" ,java-biojava-core-4.0)
       ("java-biojava-alignment" ,java-biojava-alignment-4.0)
       ("java-jdistlib" ,java-jdistlib)
       ("java-simple-xml" ,java-simple-xml)
       ("java-snakeyaml" ,java-snakeyaml)))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-testng" ,java-testng)))
    (home-page "http://mccarrolllab.com/dropseq/")
    (synopsis "Tools for Drop-seq analyses")
    (description "Drop-seq is a technology to enable biologists to
analyze RNA expression genome-wide in thousands of individual cells at
once.  This package provides tools to perform Drop-seq analyses.")
    (license license:expat)))

(define-public pigx-rnaseq
  (package
    (name "pigx-rnaseq")
    (version "0.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_rnaseq/"
                                  "releases/download/v" version
                                  "/pigx_rnaseq-" version ".tar.gz"))
              (sha256
               (base32
                "168hx2ig3rarphx3l21ay9yyg8ipaakzixnrhpbdi0sknhyvrrk8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-tests? #f             ; not supported
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-executable
           ;; Make sure the executable finds all R modules.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/pigx-rnaseq")
                 `("R_LIBS_SITE" ":" = (,(getenv "R_LIBS_SITE")))
                 `("PYTHONPATH"  ":" = (,(getenv "PYTHONPATH")))))
             #t)))))
    (inputs
     `(("snakemake" ,snakemake)
       ("fastqc" ,fastqc)
       ("multiqc" ,multiqc)
       ("star" ,star)
       ("trim-galore" ,trim-galore)
       ("htseq" ,htseq)
       ("samtools" ,samtools)
       ("bedtools" ,bedtools)
       ("r-minimal" ,r-minimal)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-gprofiler" ,r-gprofiler)
       ("r-deseq2" ,r-deseq2)
       ("r-dt" ,r-dt)
       ("r-knitr" ,r-knitr)
       ("r-pheatmap" ,r-pheatmap)
       ("r-corrplot" ,r-corrplot)
       ("r-reshape2" ,r-reshape2)
       ("r-plotly" ,r-plotly)
       ("r-scales" ,r-scales)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-crosstalk" ,r-crosstalk)
       ("r-tximport" ,r-tximport)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rjson" ,r-rjson)
       ("salmon" ,salmon)
       ("ghc-pandoc" ,ghc-pandoc)
       ("ghc-pandoc-citeproc" ,ghc-pandoc-citeproc)
       ("python-wrapper" ,python-wrapper)
       ("python-pyyaml" ,python-pyyaml)))
    (home-page "http://bioinformatics.mdc-berlin.de/pigx/")
    (synopsis "Analysis pipeline for RNA sequencing experiments")
    (description "PiGX RNAseq is an analysis pipeline for preprocessing and
reporting for RNA sequencing experiments.  It is easy to use and produces high
quality reports.  The inputs are reads files from the sequencing experiment,
and a configuration file which describes the experiment.  In addition to
quality control of the experiment, the pipeline produces a differential
expression report comparing samples in an easily configurable manner.")
    (license license:gpl3+)))

(define-public pigx-chipseq
  (package
    (name "pigx-chipseq")
    (version "0.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_chipseq/"
                                  "releases/download/v" version
                                  "/pigx_chipseq-" version ".tar.gz"))
              (sha256
               (base32
                "1jliwhifnjgl9x0z730bzpxswi2s84fyg5y8cagbyzpw509452f5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-executable
           ;; Make sure the executable finds all R modules.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/pigx-chipseq")
                 `("R_LIBS_SITE" ":" = (,(getenv "R_LIBS_SITE")))
                 `("PYTHONPATH"  ":" = (,(getenv "PYTHONPATH")))))
             #t)))))
    (inputs
     `(("r-minimal" ,r-minimal)
       ("r-argparser" ,r-argparser)
       ("r-chipseq" ,r-chipseq)
       ("r-data-table" ,r-data-table)
       ("r-genomation" ,r-genomation)
       ("r-genomicranges" ,r-genomicranges)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rcas" ,r-rcas)
       ("r-stringr" ,r-stringr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-heatmaply" ,r-heatmaply)
       ("r-ggplot2" ,r-ggplot2)
       ("r-plotly" ,r-plotly)
       ("python-wrapper" ,python-wrapper)
       ("python-pyyaml" ,python-pyyaml)
       ("snakemake" ,snakemake)
       ("macs" ,macs)
       ("multiqc" ,multiqc)
       ("perl" ,perl)
       ("ghc-pandoc" ,ghc-pandoc)
       ("ghc-pandoc-citeproc" ,ghc-pandoc-citeproc)
       ("fastqc" ,fastqc)
       ("bowtie" ,bowtie)
       ("idr" ,idr)
       ("snakemake" ,snakemake)
       ("samtools" ,samtools)
       ("bedtools" ,bedtools)
       ("kentutils" ,kentutils)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "http://bioinformatics.mdc-berlin.de/pigx/")
    (synopsis "Analysis pipeline for ChIP sequencing experiments")
    (description "PiGX ChIPseq is an analysis pipeline for preprocessing, peak
calling and reporting for ChIP sequencing experiments.  It is easy to use and
produces high quality reports.  The inputs are reads files from the sequencing
experiment, and a configuration file which describes the experiment.  In
addition to quality control of the experiment, the pipeline enables to set up
multiple peak calling analysis and allows the generation of a UCSC track hub
in an easily configurable manner.")
    (license license:gpl3+)))

(define-public pigx-bsseq
  (package
    (name "pigx-bsseq")
    (version "0.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_bsseq/"
                                  "releases/download/v" version
                                  "/pigx_bsseq-" version ".tar.gz"))
              (sha256
               (base32
                "1h8ma99vi7hs83nafvjpq8jmaq9977j3n11c4zd95hai0cf7zxmp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-timezone
           ;; The readr package is picky about timezones.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "UTC+1")
             (setenv "TZDIR"
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             #t))
         (add-after 'install 'wrap-executable
           ;; Make sure the executable finds all R modules.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/pigx-bsseq")
                 `("R_LIBS_SITE" ":" = (,(getenv "R_LIBS_SITE")))
                 `("PYTHONPATH"  ":" = (,(getenv "PYTHONPATH")))))
             #t)))))
    (native-inputs
     `(("tzdata" ,tzdata)))
    (inputs
     `(("r-minimal" ,r-minimal)
       ("r-annotationhub" ,r-annotationhub)
       ("r-dt" ,r-dt)
       ("r-genomation" ,r-genomation)
       ("r-methylkit" ,r-methylkit)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-bookdown" ,r-bookdown)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggbio" ,r-ggbio)
       ("ghc-pandoc" ,ghc-pandoc)
       ("ghc-pandoc-citeproc" ,ghc-pandoc-citeproc)
       ("python-wrapper" ,python-wrapper)
       ("python-pyyaml" ,python-pyyaml)
       ("snakemake" ,snakemake)
       ("bismark" ,bismark)
       ("fastqc" ,fastqc)
       ("bowtie" ,bowtie)
       ("trim-galore" ,trim-galore)
       ("cutadapt" ,cutadapt)
       ("samtools" ,samtools)))
    (home-page "http://bioinformatics.mdc-berlin.de/pigx/")
    (synopsis "Bisulfite sequencing pipeline from fastq to methylation reports")
    (description "PiGx BSseq is a data processing pipeline for raw fastq read
data of bisulfite experiments; it produces reports on aggregate methylation
and coverage and can be used to produce information on differential
methylation and segmentation.")
    (license license:gpl3+)))
