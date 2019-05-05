;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015, 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2018 Raoul Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system meson)
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
  #:use-module (gnu packages bioconductor)
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
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
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
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
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
             (invoke "gcc"
                     "-O3"
                     "-ffast-math"
                     "-finline-functions"
                     "-o"
                     "aragorn"
                     (string-append "aragorn" ,version ".c"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "aragorn" bin)
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
              (method git-fetch)
              ;; BamM is not available on pypi.
              (uri (git-reference
                    (url "https://github.com/Ecogenomics/BamM.git")
                    (commit version)
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p83ahi984ipslxlg4yqy1gdnya9rkn1v71z8djgxkm9d2chw4c5"))
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
                 (for-each make-file-writable (find-files "." ".*"))
                 ;; Use autogen so that 'configure' works.
                 (substitute* "autogen.sh" (("/bin/sh") sh))
                 (setenv "CONFIG_SHELL" sh)
                 (invoke "./autogen.sh")))
             #t))
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
             (invoke "nosetests")
             #t)))))
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
    (version "2.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pezmaster31/bamtools.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nfb2ypcx9959xnbz6wxh6py3xfizgmg8nrknxl95c507m9hmq8b"))))
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
                     (assoc-ref outputs "out") "/lib/bamtools"))
            #t)))))
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
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/samtools/bcftools/"
                                  "releases/download/"
                                  version "/bcftools-" version ".tar.bz2"))
              (sha256
               (base32
                "1j3h638i8kgihzyrlnpj82xg1b23sijibys9hvwari3fy7kd0dkg"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Delete bundled htslib.
                          (delete-file-recursively "htslib-1.9")
                          #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-libgsl")
       #:test-target "test"
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
    (version "2.4.35")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bedops/bedops.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mmgsgwz5r9w76hzgxkxc9s9lkdhhaf7vr6i02b09vbswvs1fyqx"))))
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
               (invoke "tar" "xvf" "jansson-2.6.tar.bz2")
               (invoke "tar" "xvf" "zlib-1.2.7.tar.bz2")
               (invoke "tar" "xvf" "bzip2-1.0.6.tar.bz2"))
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
                                  "releases/download/v" version
                                  "/bedtools-" version ".tar.gz"))
              (sha256
               (base32
                "11rvca19ncg03kxd0wzlfx5ws7r3nisd0z8s9j9n182d8ksp2pxz"))))
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

;; Needed for pybedtools.
(define-public bedtools-2.26
  (package (inherit bedtools)
    (name "bedtools")
    (version "2.26.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/arq5x/bedtools2/releases/"
                                  "download/v" version "/"
                                  "bedtools-" version ".tar.gz"))
              (sha256
               (base32
                "0jhavwifnf7lmkb11h9y7dynr8d699h0rd2l52j1pfgircr2zwv5"))))))

(define-public pbbam
  (package
    (name "pbbam")
    (version "0.23.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PacificBiosciences/pbbam.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h9gkrpf2lrxklxp72xfl5bi3h5zcm5hprrya9gf0hr3xwlbpp0x"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-googletest
           (lambda* (#:key inputs #:allow-other-keys)
             ;; It doesn't find gtest_main because there's no pkg-config file
             ;; for it.  Find it another way.
             (substitute* "tests/meson.build"
               (("pbbam_gtest_dep = dependency\\('gtest_main'.*")
                (format #f "cpp = meson.get_compiler('cpp')
pbbam_gtest_dep = cpp.find_library('gtest_main', dirs : '~a')\n"
                        (assoc-ref inputs "googletest"))))
             #t)))
       ;; TODO: tests/pbbam_test cannot be linked
       ;; ld: tests/59830eb@@pbbam_test@exe/src_test_Accuracy.cpp.o:
       ;;   undefined reference to symbol '_ZTIN7testing4TestE'
       ;; ld: /gnu/store/...-googletest-1.8.0/lib/libgtest.so:
       ;;   error adding symbols: DSO missing from command line
       #:tests? #f
       #:configure-flags '("-Dtests=false")))
    ;; These libraries are listed as "Required" in the pkg-config file.
    (propagated-inputs
     `(("htslib" ,htslib)
       ("zlib" ,zlib)))
    (inputs
     `(("boost" ,boost)
       ("samtools" ,samtools)))
    (native-inputs
     `(("googletest" ,googletest)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper))) ; for tests
    (home-page "https://github.com/PacificBiosciences/pbbam")
    (synopsis "Work with PacBio BAM files")
    (description
     "The pbbam software package provides components to create, query, and
edit PacBio BAM files and associated indices.  These components include a core
C++ library, bindings for additional languages, and command-line utilities.
This library is not intended to be used as a general-purpose BAM utility - all
input and output BAMs must adhere to the PacBio BAM format specification.
Non-PacBio BAMs will cause exceptions to be thrown.")
    (license license:bsd-3)))

(define-public blasr-libcpp
  (package
    (name "blasr-libcpp")
    (version "5.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PacificBiosciences/blasr_libcpp.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cn5l42zyq67sj0g2imqkhayz2iqvv0a1pgpbmlq0qynjmsrbfd2"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-with-hdf5
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((hdf5 (assoc-ref inputs "hdf5")))
               (substitute* "meson.build"
                 (("libblasr_deps = \\[" m)
                  (string-append
                   m
                   (format #f "cpp.find_library('hdf5', dirs : '~a'), \
cpp.find_library('hdf5_cpp', dirs : '~a'), "
                           hdf5 hdf5)))))
             #t))
         (add-after 'unpack 'find-googletest
           (lambda* (#:key inputs #:allow-other-keys)
             ;; It doesn't find gtest_main because there's no pkg-config file
             ;; for it.  Find it another way.
             (substitute* "unittest/meson.build"
               (("libblasr_gtest_dep = dependency\\('gtest_main'.*")
                (format #f "cpp = meson.get_compiler('cpp')
libblasr_gtest_dep = cpp.find_library('gtest_main', dirs : '~a')\n"
                        (assoc-ref inputs "googletest"))))
             #t)))
       ;; TODO: unittest/libblasr_unittest cannot be linked
       ;; ld: ;; unittest/df08227@@libblasr_unittest@exe/alignment_utils_FileUtils_gtest.cpp.o:
       ;; undefined reference to symbol
       ;; '_ZN7testing8internal9DeathTest6CreateEPKcPKNS0_2REES3_iPPS1_'
       ;; ld: /gnu/store/...-googletest-1.8.0/lib/libgtest.so:
       ;;   error adding symbols: DSO missing from command line
       #:tests? #f
       #:configure-flags '("-Dtests=false")))
    (inputs
     `(("boost" ,boost)
       ("hdf5" ,hdf5)
       ("pbbam" ,pbbam)
       ("zlib" ,zlib)))
    (native-inputs
     `(("googletest" ,googletest)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/PacificBiosciences/blasr_libcpp")
    (synopsis "Library for analyzing PacBio genomic sequences")
    (description
     "This package provides three libraries used by applications for analyzing
PacBio genomic sequences.  This library contains three sub-libraries: pbdata,
hdf and alignment.")
    (license license:bsd-3)))

(define-public blasr
  (package
    (name "blasr")
    (version "5.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PacificBiosciences/blasr.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1skgy2mvz8gsgfh1gc2nfgwvpyzb1hpmp2cf2773h5wsj8nw22kl"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-with-hdf5
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((hdf5 (assoc-ref inputs "hdf5")))
               (substitute* "meson.build"
                 (("blasr_deps = \\[" m)
                  (string-append
                   m
                   (format #f "cpp.find_library('hdf5', dirs : '~a'), \
cpp.find_library('hdf5_cpp', dirs : '~a'), "
                           hdf5 hdf5)))))
             #t)))
       ;; Tests require "cram" executable, which is not packaged.
       #:tests? #f
       #:configure-flags '("-Dtests=false")))
    (inputs
     `(("boost" ,boost)
       ("blasr-libcpp" ,blasr-libcpp)
       ("hdf5" ,hdf5)
       ("pbbam" ,pbbam)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/PacificBiosciences/blasr")
    (synopsis "PacBio long read aligner")
    (description
     "Blasr is a genomic sequence aligner for processing PacBio long reads.")
    (license license:bsd-3)))

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
                  "Ribotaper.sh")))
             #t)))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ratschlab/RiboDiff.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0x75nlp7qnmm64jasbi6l21f2cy99r2cjyl6b4hr8zf2bq22drnz"))))
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
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lh3/bioawk.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pxc3zdnirxbf9a0az698hd8xdik7qkhypm7v6hn922x8y9qmspm"))))
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
               (install-file "bioawk" bin))
             #t)))))
    (home-page "https://github.com/lh3/bioawk")
    (synopsis "AWK with bioinformatics extensions")
    (description "Bioawk is an extension to Brian Kernighan's awk, adding the
support of several common biological data formats, including optionally gzip'ed
BED, GFF, SAM, VCF, FASTA/Q and TAB-delimited formats with column names.  It
also adds a few built-in functions and a command line option to use TAB as the
input/output delimiter.  When the new functionality is not used, bioawk is
intended to behave exactly the same as the original BWK awk.")
    (license license:x11)))

(define-public python-pybedtools
  (package
    (name "python-pybedtools")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pybedtools" version))
              (sha256
               (base32
                "1xl454ijvd4dzfvqgfahad49b49j7qy710fq9xh1rvk42z6x5ssf"))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((ice-9 ftw)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (guix build utils)
                  (guix build python-build-system))
       ;; See https://github.com/daler/pybedtools/issues/192
       #:phases
       (modify-phases %standard-phases
         ;; See https://github.com/daler/pybedtools/issues/261
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             ;; This test (pybedtools.test.test_scripts.test_venn_mpl) needs a
             ;; graphical environment.
             (substitute* "pybedtools/test/test_scripts.py"
               (("def test_venn_mpl")
                "def _do_not_test_venn_mpl"))
             (substitute* "pybedtools/test/test_helpers.py"
               ;; Requires internet access.
               (("def test_chromsizes")
                "def _do_not_test_chromsizes")
               ;; Broken as a result of the workaround used in the check phase
               ;; (see: https://github.com/daler/pybedtools/issues/192).
               (("def test_getting_example_beds")
                "def _do_not_test_getting_example_beds"))
             #t))
         ;; TODO: Remove phase after it's part of PYTHON-BUILD-SYSTEM.
         ;; build system.
         ;; Force the Cythonization of C++ files to guard against compilation
         ;; problems.
         (add-after 'unpack 'remove-cython-generated-files
           (lambda _
             (let ((cython-sources (map (cut string-drop-right <> 4)
                                        (find-files "." "\\.pyx$")))
                   (c/c++-files (find-files "." "\\.(c|cpp|cxx)$")))
               (define (strip-extension filename)
                 (string-take filename (string-index-right filename #\.)))
               (define (cythonized? c/c++-file)
                 (member (strip-extension c/c++-file) cython-sources))
               (for-each delete-file (filter cythonized? c/c++-files))
               #t)))
         (add-after 'remove-cython-generated-files 'generate-cython-extensions
           (lambda _
             (invoke "python" "setup.py" "cythonize")))
         (replace 'check
           (lambda _
             (let* ((cwd (getcwd))
                    (build-root-directory (string-append cwd "/build/"))
                    (build (string-append
                            build-root-directory
                            (find (cut string-prefix? "lib" <>)
                                  (scandir (string-append
                                            build-root-directory)))))
                    (scripts (string-append
                              build-root-directory
                              (find (cut string-prefix? "scripts" <>)
                                    (scandir build-root-directory)))))
               (setenv "PYTHONPATH"
                       (string-append build ":" (getenv "PYTHONPATH")))
               ;; Executable scripts such as 'intron_exon_reads.py' must be
               ;; available in the PATH.
               (setenv "PATH"
                       (string-append scripts ":" (getenv "PATH"))))
             ;; The tests need to be run from elsewhere...
             (mkdir-p "/tmp/test")
             (copy-recursively "pybedtools/test" "/tmp/test")
             (with-directory-excursion "/tmp/test"
               (invoke "pytest")))))))
    (propagated-inputs
     `(("bedtools" ,bedtools)
       ("samtools" ,samtools)
       ("python-matplotlib" ,python-matplotlib)
       ("python-pysam" ,python-pysam)
       ("python-pyyaml" ,python-pyyaml)))
    (native-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-cython" ,python-cython)
       ("kentutils" ,kentutils)         ; for bedGraphToBigWig
       ("python-six" ,python-six)
       ;; For the test suite.
       ("python-pytest" ,python-pytest)
       ("python-psutil" ,python-psutil)))
    (home-page "https://pythonhosted.org/pybedtools/")
    (synopsis "Python wrapper for BEDtools programs")
    (description
     "pybedtools is a Python wrapper for Aaron Quinlan's BEDtools programs,
which are widely used for genomic interval manipulation or \"genome algebra\".
pybedtools extends BEDTools by offering feature-level manipulations from with
Python.")
    (license license:gpl2+)))

(define-public python2-pybedtools
  (package-with-python2 python-pybedtools))

(define-public python-biom-format
  (package
    (name "python-biom-format")
    (version "2.1.7")
    (source
     (origin
       (method git-fetch)
       ;; Use GitHub as source because PyPI distribution does not contain
       ;; test data: https://github.com/biocore/biom-format/issues/693
       (uri (git-reference
             (url "https://github.com/biocore/biom-format.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1rna16lyk5aqhnv0dp77wwaplias93f1vw28ad3jmyw6hwkai05v"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Delete generated C files.
                   (for-each delete-file (find-files "." "\\.c"))
                   #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-cython
           (lambda _ (setenv "USE_CYTHON" "1") #t))
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             (substitute* "biom/tests/test_cli/test_validate_table.py"
               (("^(.+)def test_invalid_hdf5" m indent)
                (string-append indent
                               "@npt.dec.skipif(True, msg='Guix')\n"
                               m)))
             (substitute* "biom/tests/test_table.py"
               (("^(.+)def test_from_hdf5_issue_731" m indent)
                (string-append indent
                               "@npt.dec.skipif(True, msg='Guix')\n"
                               m)))
             #t))
         (add-before 'reset-gzip-timestamps 'make-files-writable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (file) (chmod file #o644))
                         (find-files out "\\.gz"))
               #t))))))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-flake8" ,python-flake8)
       ("python-future" ,python-future)
       ("python-click" ,python-click)
       ("python-h5py" ,python-h5py)
       ("python-pandas" ,python-pandas)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-nose" ,python-nose)))
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
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; Do not require the unmaintained pyqi library.
             (add-after 'unpack 'remove-pyqi
               (lambda _
                 (substitute* "setup.py"
                   (("install_requires.append\\(\"pyqi\"\\)") "pass"))
                 #t)))))))))

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
      (home-page "https://metacpan.org/release/BioPerl")
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

(define-public python-fastalite
  (package
    (name "python-fastalite")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fastalite" version))
       (sha256
        (base32
         "1qli6pxp77i9xn2wfciq2zaxhl82bdxb33cpzqzj1z25yd036wqj"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Test data is not distributed.
    (home-page "https://github.com/nhoffman/fastalite")
    (synopsis "Simplest possible FASTA parser")
    (description "This library implements a FASTA and a FASTQ parser without
relying on a complex dependency tree.")
    (license license:expat)))

(define-public python2-fastalite
  (package-with-python2 python-fastalite))

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
    (version "2.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/"
                    version "/ncbi-blast-" version "+-src.tar.gz"))
              (sha256
               (base32
                "1jlq0afxxgczpp35k6mxh8mn4jzq7vqcnaixk166sfj10wq8v9qh"))
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
         (add-before 'configure 'set-HOME
          ;; $HOME needs to be set at some point during the configure phase
          (lambda _ (setenv "HOME" "/tmp") #t))
         (add-after 'unpack 'enter-dir
          (lambda _ (chdir "c++") #t))
         (add-after 'enter-dir 'fix-build-system
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
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out     (assoc-ref outputs "out"))
                   (lib     (string-append (assoc-ref outputs "lib") "/lib"))
                   (include (string-append (assoc-ref outputs "include")
                                           "/include/ncbi-tools++")))
               ;; The 'configure' script doesn't recognize things like
               ;; '--enable-fast-install'.
               (invoke "./configure.orig"
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
                       "--with-dll")
               #t))))))
    (outputs '("out"       ;  21 MB
               "lib"       ; 226 MB
               "include")) ;  33 MB
    (inputs
     `(("bzip2" ,bzip2)
       ("lmdb" ,lmdb)
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
                            (assoc-ref %build-inputs "zlib:static")
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
       ("zlib:static" ,zlib "static")
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
    (version "2.3.4.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/BenLangmead/bowtie2.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zl3cf327y2p7p03cavymbh7b00djc7lncfaqih33n96iy9q8ibp"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Makefile"
                    ;; replace BUILD_HOST and BUILD_TIME for deterministic build
                    (("-DBUILD_HOST=.*") "-DBUILD_HOST=\"\\\"guix\\\"\"")
                    (("-DBUILD_TIME=.*") "-DBUILD_TIME=\"\\\"0\\\"\""))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list "allall"
             "WITH_TBB=1"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           (lambda _
             (invoke "perl"
                     "scripts/test/simple_tests.pl"
                     "--bowtie2=./bowtie2"
                     "--bowtie2-build=./bowtie2-build")
             #t)))))
    (inputs
     `(("tbb" ,tbb)
       ("zlib" ,zlib)
       ("python" ,python-wrapper)))
    (native-inputs
     `(("perl" ,perl)
       ("perl-clone" ,perl-clone)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-simple" ,perl-test-simple)))
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

(define-public bowtie1
  (package
    (name "bowtie1")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bowtie-bio/bowtie/"
                                  version "/bowtie-" version "-src.zip"))
              (sha256
               (base32
                "1jl2cj9bz8lwz8dwnxbycn8yp8g4kky62fkcxifyf1ri0y6n2vc0"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Makefile"
                  ;; replace BUILD_HOST and BUILD_TIME for deterministic build
                  (("-DBUILD_HOST=.*") "-DBUILD_HOST=\"\\\"guix\\\"\"")
                  (("-DBUILD_TIME=.*") "-DBUILD_TIME=\"\\\"0\\\"\"")))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no "check" target
       #:make-flags
       (list "all"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("tbb" ,tbb)
       ("zlib" ,zlib)))
    (supported-systems '("x86_64-linux"))
    (home-page "http://bowtie-bio.sourceforge.net/index.shtml")
    (synopsis "Fast aligner for short nucleotide sequence reads")
    (description
     "Bowtie is a fast, memory-efficient short read aligner.  It aligns short
DNA sequences (reads) to the human genome at a rate of over 25 million 35-bp
reads per hour.  Bowtie indexes the genome with a Burrows-Wheeler index to
keep its memory footprint small: typically about 2.2 GB for the human
genome (2.9 GB for paired-end).")
    (license license:artistic2.0)))

(define-public tophat
  (package
    (name "tophat")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ccb.jhu.edu/software/tophat/downloads/tophat-"
                    version ".tar.gz"))
              (sha256
               (base32
                "19add02kv2xhd6ihd779dr7x35ggym3jqr0m5c4315i1yfb0p11p"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled SeqAn and samtools
                  (delete-file-recursively "src/SeqAn-1.4.2")
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
       ("seqan" ,seqan-1)))
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
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pkerpedjiev/bwa-pssm.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "076c4q0cdqz8jgylb067y9zmvxglppnzi3qiscn0xiypgc6lgb5r"))))
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

(define-public bwa-meth
  (package
    (name "bwa-meth")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brentp/bwa-meth.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17j31i7zws5j7mhsq9x3qgkxly6mlmrgwhfq0qbflgxrmx04yaiz"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'keep-references-to-bwa
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "bwameth.py"
               (("bwa mem")
                (string-append (which "bwa") " mem"))
               ;; There's an ill-advised check for "samtools" on PATH.
               (("^checkX.*") ""))
             #t)))))
    (inputs
     `(("bwa" ,bwa)))
    (native-inputs
     `(("python-toolshed" ,python-toolshed)))
    (home-page "https://github.com/brentp/bwa-meth")
    (synopsis "Fast and accurante alignment of BS-Seq reads")
    (description
     "BWA-Meth works for single-end reads and for paired-end reads from the
directional protocol (most common).  It uses the method employed by
methylcoder and Bismark of in silico conversion of all C's to T's in both
reference and reads.  It recovers the original read (needed to tabulate
methylation) by attaching it as a comment which BWA appends as a tag to the
read.  It performs favorably to existing aligners gauged by number of on and
off-target reads for a capture method that targets CpG-rich region.")
    (license license:expat)))

(define-public python-bx-python
  (package
    (name "python-bx-python")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "bx-python" version))
              (sha256
               (base32
                "11kksg2rbzihpmcid823xvg42xi88m7sz58rzk29abybkxy0rszs"))))
    (build-system python-build-system)
    ;; Tests fail because test data are not included
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-six" ,python-six)))
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("python-lzo" ,python-lzo)
       ("python-nose" ,python-nose)
       ("python-cython" ,python-cython)))
    (home-page "https://github.com/bxlab/bx-python")
    (synopsis "Tools for manipulating biological data")
    (description
     "bx-python provides tools for manipulating biological data, particularly
multiple sequence alignments.")
    (license license:expat)))

(define-public python2-bx-python
  (package-with-python2 python-bx-python))

(define-public python-pysam
  (package
    (name "python-pysam")
    (version "0.15.1")
    (source (origin
              (method git-fetch)
              ;; Test data is missing on PyPi.
              (uri (git-reference
                    (url "https://github.com/pysam-developers/pysam.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vj367w6xbn9bpmksm162l1aipf7cj97h1q83y7jcpm33ihwpf7x"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Drop bundled htslib. TODO: Also remove samtools
                          ;; and bcftools.
                          (delete-file-recursively "htslib")
                          #t))))
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
             ;; This file contains tests that require a connection to the
             ;; internet.
             (delete-file "tests/tabix_test.py")
             ;; FIXME: This test fails
             (delete-file "tests/AlignmentFile_test.py")
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
               (invoke "make" "-C" "pysam_data")
               (invoke "make" "-C" "cbcf_data")
               ;; Running nosetests without explicitly asking for a single
               ;; process leads to a crash.  Running with multiple processes
               ;; fails because the tests are not designed to run in parallel.

               ;; FIXME: tests keep timing out on some systems.
               (invoke "nosetests" "-v" "--processes" "1")))))))
    (propagated-inputs
     `(("htslib" ,htslib))) ; Included from installed header files.
    (inputs
     `(("ncurses" ,ncurses)
       ("curl" ,curl)
       ("zlib" ,zlib)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ;; Dependencies below are are for tests only.
       ("samtools" ,samtools)
       ("bcftools" ,bcftools)
       ("python-nose" ,python-nose)))
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
    (version "3.1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/benjschiller/twobitreader")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qbxvv1h58cismbk1anpjrkpghsaiy64a11ir3lhy6qch6xf8n62"))))
    (build-system python-build-system)
    ;; Tests are not included
    (arguments '(#:tests? #f))
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

(define-public tetoolkit
  (package
    (name "tetoolkit")
    (version "2.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mhammell-laboratory/tetoolkit.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yzi0kfpzip8zpjb82x1ik6h22yzfyjiz2dv85v6as2awwqvk807"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2               ; not guaranteed to work with Python 3
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-after 'unpack 'patch-invocations
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("bin/TEtranscripts"
                            "bin/TEcount")
               (("'sort ")
                (string-append "'" (which "sort") " "))
               (("'rm -f ")
                (string-append "'" (which "rm") " -f "))
               (("'Rscript'") (string-append "'" (which "Rscript") "'")))
             (substitute* "TEToolkit/IO/ReadInputs.py"
               (("BamToBED") (which "bamToBed")))
             (substitute* "TEToolkit/Normalization.py"
               (("\"Rscript\"")
                (string-append "\"" (which "Rscript") "\"")))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure the executables find R packages.
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (script)
                  (wrap-program (string-append out "/bin/" script)
                    `("R_LIBS_SITE" ":" = (,(getenv "R_LIBS_SITE")))))
                '("TEtranscripts"
                  "TEcount")))
             #t)))))
    (inputs
     `(("coreutils" ,coreutils)
       ("bedtools" ,bedtools)
       ("python-argparse" ,python2-argparse)
       ("python-pysam" ,python2-pysam)
       ("r-minimal" ,r-minimal)
       ("r-deseq2" ,r-deseq2)))
    (home-page "https://github.com/mhammell-laboratory/tetoolkit")
    (synopsis "Transposable elements in differential enrichment analysis")
    (description
     "This is package for including transposable elements in differential
enrichment analysis of sequencing datasets.  TEtranscripts and TEcount take
RNA-seq (and similar data) and annotates reads to both genes and transposable
elements.  TEtranscripts then performs differential analysis using DESeq2.
Note that TEtranscripts and TEcount rely on specially curated GTF files, which
are not included due to their size.")
    (license license:gpl3+)))

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
       (list (string-append "PREFIX=" (assoc-ref %outputs "out") "/bin")
             ;; Support longer sequences (e.g. Pacbio sequences)
             "MAX_SEQ=60000000")
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
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/YeoLab/clipper.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fja1rj84wp9vpj8rxpj3n8zqzcqq454m904yp9as1w4phccirjb"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; remove unnecessary setup dependency
                  (substitute* "setup.py"
                    (("setup_requires = .*") ""))
                  #t))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; only Python 2 is supported
       #:phases
       (modify-phases %standard-phases
         ;; This is fixed in upstream commit
         ;; f6c2990198f906bf97730d95695b4bd5a6d01ddb.
         (add-after 'unpack 'fix-typo
           (lambda _
             (substitute* "clipper/src/readsToWiggle.pyx"
               (("^sc.*") ""))
             #t)))))
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
               (install-file "CufflinksGTF_to_CodingQuarryGFF3.py" bin))
             #t)))))
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
    (version "0.2.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "CrossMap" version))
              (sha256
               (base32
                "1byhclrqnqpvc1rqkfh4jwj6yhn0x9y7jk47i0qcjlhk0pjkw92p"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (inputs
     `(("python-bx-python" ,python2-bx-python)
       ("python-numpy" ,python2-numpy)
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

(define-public python-dnaio
  (package
    (name "python-dnaio")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dnaio" version))
       (sha256
        (base32
         "0f16m7hdlm0fz1n7y5asy0v9ghyrq17ni1p9iybq22ddzyd49r27"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-pytest" ,python-pytest)
       ("python-xopen" ,python-xopen)))
    (home-page "https://github.com/marcelm/dnaio/")
    (synopsis "Read FASTA and FASTQ files efficiently")
    (description
     "dnaio is a Python library for fast parsing of FASTQ and also FASTA
files.  The code was previously part of the cutadapt tool.")
    (license license:expat)))

(define-public cutadapt
  (package
    (name "cutadapt")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cutadapt" version))
              (sha256
               (base32
                "1vqmsfkm6llxzmsz9wcfcvzx9a9f8iabvwik2rbyn7nc4wm25z89"))))
    (build-system python-build-system)
    (inputs
     `(("python-dnaio" ,python-dnaio)
       ("python-xopen" ,python-xopen)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://cutadapt.readthedocs.io/en/stable/")
    (synopsis "Remove adapter sequences from nucleotide sequencing reads")
    (description
     "Cutadapt finds and removes adapter sequences, primers, poly-A tails and
other types of unwanted sequence from high-throughput sequencing reads.")
    (license license:expat)))

(define-public libbigwig
  (package
    (name "libbigwig")
    (version "0.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dpryan79/libBigWig.git")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0h2smg24v5srdcqzrmz2g23cmlp4va465mgx8r2z571sfz8pv454"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f ; tests require access to the web
       #:make-flags
       (list "CC=gcc"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("zlib" ,zlib)
       ("curl" ,curl)))
    (native-inputs
     `(("doxygen" ,doxygen)
       ;; Need for tests
       ("python" ,python-2)))
    (home-page "https://github.com/dpryan79/libBigWig")
    (synopsis "C library for handling bigWig files")
    (description
     "This package provides a C library for parsing local and remote BigWig
files.")
    (license license:expat)))

(define-public python-pybigwig
  (package
    (name "python-pybigwig")
    (version "0.3.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyBigWig" version))
              (sha256
               (base32
                "00w4kfnm2c5l7wdwr2nj1z5djv8kzgf7h1zhsgv6njff1rwr26g0"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled libBigWig sources
                  (delete-file-recursively "libBigWig")
                  #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-with-libBigWig
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               (("libs=\\[") "libs=[\"BigWig\", "))
             #t)))))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
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
    (version "4.4.0")
    (source
     (origin
       (method git-fetch)
       ;; Source from GitHub so that tests are included.
       (uri (git-reference
             (url "https://github.com/jeetsukumaran/DendroPy.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "097hfyv2kaf4x92i4rjx0paw2cncxap48qivv8zxng4z7nhid0x9"))))
    (build-system python-build-system)
    (home-page "http://packages.python.org/DendroPy/")
    (synopsis "Library for phylogenetics and phylogenetic computing")
    (description
     "DendroPy is a library for phylogenetics and phylogenetic computing: reading,
writing, simulation, processing and manipulation of phylogenetic
trees (phylogenies) and characters.")
    (license license:bsd-3)))

(define-public python2-dendropy
  (let ((base (package-with-python2 python-dendropy)))
    (package
      (inherit base)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'remove-failing-test
             (lambda _
               ;; This test fails when the full test suite is run, as documented
               ;; at https://github.com/jeetsukumaran/DendroPy/issues/74
               (substitute* "tests/test_dataio_nexml_reader_tree_list.py"
                 (("test_collection_comments_and_annotations")
                  "do_not_test_collection_comments_and_annotations"))
               #t)))
         ,@(package-arguments base))))))

(define-public python-py2bit
  (package
    (name "python-py2bit")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py2bit" version))
       (sha256
        (base32
         "1vw2nvw1yrl7ikkqsqs1pg239yr5nspvd969r1x9arms1k25a1a5"))))
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
    (version "3.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/deeptools/deepTools.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vggnf52g6q2vifdl4cyi7s2fnfqq0ky2zrkj5zv2qfzsc3p3siw"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This phase fails, but it's not needed.
         (delete 'reset-gzip-timestamps))))
    (inputs
     `(("python-plotly" ,python-plotly)
       ("python-scipy" ,python-scipy)
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

(define-public delly
  (package
    (name "delly")
    (version "0.7.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dellytools/delly.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "034jqsxswy9gqdh2zkgc1js99qkv75ks4xvzgmh0284sraagv61z"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "src/htslib")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests to run.
       #:make-flags
       (list "PARALLEL=1"  ; Allow parallel execution at run-time.
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (add-after 'install 'install-templates
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((templates (string-append (assoc-ref outputs "out")
                                             "/share/delly/templates")))
               (mkdir-p templates)
               (copy-recursively "excludeTemplates" templates)
               #t))))))
    (inputs
     `(("boost" ,boost)
       ("htslib" ,htslib)
       ("zlib" ,zlib)
       ("bzip2" ,bzip2)))
    (home-page "https://github.com/dellytools/delly")
    (synopsis "Integrated structural variant prediction method")
    (description "Delly is an integrated structural variant prediction method
that can discover and genotype deletions, tandem duplications, inversions and
translocations at single-nucleotide resolution in short-read massively parallel
sequencing data.  It uses paired-ends and split-reads to sensitively and
accurately delineate genomic rearrangements throughout the genome.")
    (license license:gpl3+)))

(define-public diamond
  (package
    (name "diamond")
    (version "0.9.22")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bbuchfink/diamond.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bky78v79g3wmdpsd706cscckgw1v09fg8vdd0z8z0d5b97aj9zl"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/maaskola/discrover.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "173fwi2vb6a5kp406hm3jj6j7v4whww796f2qcygp4rpvamh307y"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-latex-errors
           (lambda _
             (with-fluids ((%default-port-encoding #f))
               (substitute* "doc/references.bib"
                 (("\\{S\\}illanp[^,]+,")
                  "{S}illanp{\\\"a}{\\\"a},")))
             ;; XXX: I just can't get pdflatex to not complain about these
             ;; characters.  They end up in the manual via the generated
             ;; discrover-cli-help.txt.
             (substitute* "src/hmm/cli.cpp"
               (("µ") "mu")
               (("η") "eta")
               (("≤") "<="))
             ;; This seems to be a syntax error.
             (substitute* "doc/discrover-manual.tex"
               (("theverbbox\\[t\\]") "theverbbox"))
             #t))
         (add-after 'unpack 'add-missing-includes
           (lambda _
             (substitute* "src/executioninformation.hpp"
               (("#define EXECUTIONINFORMATION_HPP" line)
                (string-append line "\n#include <random>")))
             (substitute* "src/plasma/fasta.hpp"
               (("#define FASTA_HPP" line)
                (string-append line "\n#include <random>")))
             #t))
         ;; FIXME: this is needed because we're using texlive-union, which
         ;; doesn't handle fonts correctly.  It expects to be able to generate
         ;; fonts in the home directory.
         (add-before 'build 'setenv-HOME
           (lambda _ (setenv "HOME" "/tmp") #t)))))
    (inputs
     `(("boost" ,boost)
       ("cairo" ,cairo)
       ("rmath-standalone" ,rmath-standalone)))
    (native-inputs
     `(("texlive" ,(texlive-union (list texlive-fonts-cm
                                        texlive-fonts-amsfonts

                                        texlive-latex-doi
                                        texlive-latex-examplep
                                        texlive-latex-hyperref
                                        texlive-latex-ms
                                        texlive-latex-natbib
                                        texlive-bibtex         ; style files used by natbib
                                        texlive-latex-pgf      ; tikz
                                        texlive-latex-verbatimbox)))
       ("imagemagick" ,imagemagick)))
    (home-page "http://dorina.mdc-berlin.de/public/rajewsky/discrover/")
    (synopsis "Discover discriminative nucleotide sequence motifs")
    (description "Discrover is a motif discovery method to find binding sites
of nucleic acid binding proteins.")
    (license license:gpl3+)))

(define-public eigensoft
  (package
    (name "eigensoft")
    (version "7.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DReichLab/EIG.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1c141fqvhnzibmnf22sv23vbmzm20kjjyrib44cfh75wyndp2d9k"))
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
           (lambda _ (chdir "src") #t))
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
    (license license:gpl3+)))

(define-public edirect
  (package
    (name "edirect")
    (version "10.2.20181018")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.ncbi.nlm.nih.gov/entrez/entrezdirect"
                                  "/versions/" version
                                  "/edirect-" version ".tar.gz"))
              (sha256
               (base32
                "091f4aigzpbqih6h82nq566gkp3y07i72yqndmqskfgar1vwgci7"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)                ; simple check after install
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "edirect.pl"
                           (string-append (assoc-ref outputs "out") "/bin"))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure 'edirect.pl' finds all perl inputs at runtime.
             (let* ((out (assoc-ref outputs "out"))
                    (path (getenv "PERL5LIB")))
               (wrap-program (string-append out "/bin/edirect.pl")
                 `("PERL5LIB" ":" prefix (,path))))
             #t))
         (add-after 'wrap-program 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke (string-append (assoc-ref outputs "out")
                                    "/bin/edirect.pl")
                     "-filter" "-help")
             #t)))))
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
       ("perl-xml-simple" ,perl-xml-simple)
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
                (string-append (assoc-ref inputs "bamtools") "/lib"))
               (("libprotobuf.a") "libprotobuf.so"))
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
   (version "1.0.8")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/dparks1134/ExpressBetaDiversity.git")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0s0yzg5c21349rh7x4w9266jsvnp7j1hp9cf8sk32hz8nvrj745x"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'enter-source (lambda _ (chdir "source") #t))
        (replace 'check
          (lambda _ (invoke "../bin/ExpressBetaDiversity" "-u") #t))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "../scripts/convertToEBD.py" bin)
              (install-file "../bin/ExpressBetaDiversity" bin)
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
            (invoke "gcc"
                    "-O3"
                    "-finline-functions"
                    "-funroll-loops"
                    "-Wall"
                    "-o"
                    "FastTree"
                    source
                    "-lm")
            (invoke "gcc"
                    "-DOPENMP"
                    "-fopenmp"
                    "-O3"
                    "-finline-functions"
                    "-funroll-loops"
                    "-Wall"
                    "-o"
                    "FastTreeMP"
                    source
                    "-lm")
            #t))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
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
    (version "3.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/seqan/flexbar.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pq9sxvdnldl14libk234m72dqhwgzs3acgl943wchwdqlcsi5r2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-tune-to-CPU
           (lambda _
             (substitute* "src/CMakeLists.txt"
               ((" -march=native") ""))
             #t))
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "PATH" (string-append (getcwd) ":" (getenv "PATH")))
             (with-directory-excursion "../source/test"
               (invoke "bash" "flexbar_test.sh"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (string-append (assoc-ref outputs "out")))
                    (bin (string-append out "/bin/")))
               (install-file "flexbar" bin))
             #t)))))
    (inputs
     `(("tbb" ,tbb)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("seqan" ,seqan)))
    (home-page "https://github.com/seqan/flexbar")
    (synopsis "Barcode and adapter removal tool for sequencing platforms")
    (description
     "Flexbar preprocesses high-throughput nucleotide sequencing data
efficiently.  It demultiplexes barcoded runs and removes adapter sequences.
Moreover, trimming and filtering features are provided.  Flexbar increases
read mapping rates and improves genome and transcriptome assemblies.  It
supports next-generation sequencing data in fasta/q and csfasta/q format from
Illumina, Roche 454, and the SOLiD platform.")
    (license license:bsd-3)))

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
           (lambda _
             (invoke "make" "clean")
             (invoke "make" "fgs")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (string-append (assoc-ref outputs "out")))
                    (bin (string-append out "/bin/"))
                    (share (string-append out "/share/fraggenescan/train")))
               (install-file "run_FragGeneScan.pl" bin)
               (install-file "FragGeneScan" bin)
               (copy-recursively "train" share))
             #t))
         (delete 'check)
         (add-after 'install 'post-install-check
           ;; In lieu of 'make check', run one of the examples and check the
           ;; output files gets created.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (string-append (assoc-ref outputs "out")))
                    (bin (string-append out "/bin/"))
                    (frag (string-append bin "run_FragGeneScan.pl")))
               ;; Test complete genome.
               (invoke frag
                       "-genome=./example/NC_000913.fna"
                       "-out=./test2"
                       "-complete=1"
                       "-train=complete")
               (unless (and (file-exists? "test2.faa")
                            (file-exists? "test2.ffn")
                            (file-exists? "test2.gff")
                            (file-exists? "test2.out"))
                 (error "Expected files do not exist."))
               ;; Test incomplete sequences.
               (invoke frag
                       "-genome=./example/NC_000913-fgs.ffn"
                       "-out=out"
                       "-complete=0"
                       "-train=454_30")
               #t))))))
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
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ctSkennerton/fxtract.git")
               (commit version)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0hab3gpwf4w9s87qlbswq6ws1qqybh4dcqk79q1ahyldzai5fgp5"))))
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
    (version "0.98")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xiangzhou/GEMMA.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s3ncnbn45r2hh1cvrqky1kbqq6546biypr4f5mkw1kqlrgyh0yg"))))
    (inputs
     `(("eigen" ,eigen)
       ("gfortran" ,gfortran "lib")
       ("gsl" ,gsl)
       ("lapack" ,lapack)
       ("openblas" ,openblas)
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
         (add-after 'unpack 'find-eigen
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Ensure that Eigen headers can be found
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (getenv "CPLUS_INCLUDE_PATH")
                                    ":"
                                    (assoc-ref inputs "eigen")
                                    "/include/eigen3"))
             #t))
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
    (version "2.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nboley/grit.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l5v8vfvfbrpmgnrvbrbv40d0arhxcnmxgv2f1mlcqfa3q6bkqm9"))))
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
               (("Cython.Setup") "Cython.Build"))
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
       (uri (string-append "ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2"
                           "/downloads/hisat2-" version "-source.zip"))
       (sha256
        (base32
         "0lywnr8kijwsc2aw10dwxic0n0yvip6fl3rjlvc8zzwahamy4x7g"))))
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
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://eddylab.org/software/hmmer/hmmer-" version ".tar.gz"))
       (sha256
        (base32
         "171bivy6xhgjsz5nv53n81pc3frnwz29ylblawk2bv46szwjjqd5"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))
    (home-page "http://hmmer.org/")
    (synopsis "Biosequence analysis using profile hidden Markov models")
    (description
     "HMMER is used for searching sequence databases for homologs of protein
sequences, and for making protein sequence alignments.  It implements methods
using probabilistic models called profile hidden Markov models (profile
HMMs).")
    ;; hmmer uses non-portable SSE intrinsics so building fails on other
    ;; platforms.
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:bsd-3)))

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
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/samtools/htsjdk.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b178ixcabanm834ydjl3jiakpyxdmki32hqfv2abrzn3rcwa28i"))
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
         ;; FIXME: this phase fails with "duplicate entry: htsjdk/samtools/AbstractBAMFileIndex$1.class"
         (delete 'generate-jar-indices)
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
         ;; FIXME: this phase fails with "duplicate entry: htsjdk/samtools/AbstractBAMFileIndex$1.class"
         (delete 'generate-jar-indices)
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
 ~a/share/java/htsjdk.jar${line.separator}${line.separator}"
                                                        ;; maximum line length is 70
                                                        (string-tabulate (const #\b) 57)
                                                        (assoc-ref inputs "java-htsjdk"))))
                                       (if (member "manifest" name)
                                           `(,tag ,@kids
                                                  (replaceregexp
                                                   (@ (file "${manifest.file}")
                                                      (match "\\r\\n\\r\\n")
                                                      (replace "${line.separator}")))
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
         ;; FIXME: This phase fails.
         (delete 'generate-jar-indices)
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
             (invoke "tar" "--strip-components=1" "-C" "jdk-src"
                     "-xf" (assoc-ref inputs "jdk-src"))
             (invoke "javah" "-jni"
                     "-classpath" "classes"
                     "-d" "lib/"
                     "net.sf.samtools.util.zip.IntelDeflater")
             (with-directory-excursion "src/c/inteldeflater"
               (invoke "gcc" "-I../../../lib" "-I."
                       (string-append "-I" (assoc-ref inputs "jdk")
                                      "/include/linux")
                       "-I../../../jdk-src/src/share/native/common/"
                       "-I../../../jdk-src/src/solaris/native/common/"
                       "-c" "-O3" "-fPIC" "IntelDeflater.c")
               (invoke "gcc" "-shared"
                       "-o" "../../../lib/jni/libIntelDeflater.so"
                       "IntelDeflater.o" "-lz" "-lstdc++"))
             #t))
         ;; We can only build everything else after building the JNI library.
         (add-after 'build-jni 'build-rest
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke `("ant" "all" ,@make-flags))
             #t))
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

(define-public fastp
  (package
    (name "fastp")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenGene/fastp.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1r6ms5zbf5rps4rgp4z73nczadl00b5rqylw8f684isfz27dp0xh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are none
       #:make-flags
       (list (string-append "BINDIR=" (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'create-target-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/OpenGene/fastp/")
    (synopsis "All-in-one FastQ preprocessor")
    (description
     "Fastp is a tool designed to provide fast all-in-one preprocessing for
FastQ files.  This tool has multi-threading support to afford high
performance.")
    (license license:expat)))

(define-public htslib
  (package
    (name "htslib")
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "16ljv43sc3fxmv63w7b2ff8m1s7h89xhazwmbm1bicz8axq8fjz0"))))
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
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nboley/idr.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04j876h6z444v2q79drxx283d3k5snd72kj895wbalnl42206x9g"))
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
    (version "2.2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gmarcais/Jellyfish/"
                                  "releases/download/v" version
                                  "/jellyfish-" version ".tar.gz"))
              (sha256
               (base32
                "1k4pc3fvv6w1km2yph4m5sd78fbxp21d6xyzgmy0gjihzc6mb249"))))
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
    ;; JELLYFISH seems to be 64-bit only.
    (supported-systems '("x86_64-linux" "aarch64-linux" "mips64el-linux"))
    ;; The combined work is published under the GPLv3 or later.  Individual
    ;; files such as lib/jsoncpp.cpp are released under the Expat license.
    (license (list license:gpl3+ license:expat))))

(define-public khmer
  (package
    (name "khmer")
    (version "2.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dib-lab/khmer.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "02x38d9jw2r58y8dmnj4hffy9wxv1yc1jwbvdbhby9dxndv94r9m"))
       (patches (search-patches "khmer-use-libraries.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled libraries.  We do not replace the bundled seqan
           ;; as it is a modified subset of the old version 1.4.1.
           ;;
           ;; We do not replace the bundled MurmurHash as the canonical
           ;; repository for this code 'SMHasher' is unsuitable for providing
           ;; a library.  See
           ;; https://lists.gnu.org/archive/html/guix-devel/2016-06/msg00977.html
           (delete-file-recursively "third-party/zlib")
           (delete-file-recursively "third-party/bzip2")
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-cc
           (lambda _ (setenv "CC" "gcc") #t))
         ;; FIXME: This fails with "permission denied".
         (delete 'reset-gzip-timestamps))))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (inputs
     `(("zlib" ,zlib)
       ("bzip2" ,bzip2)
       ("python-screed" ,python-screed)
       ("python-bz2file" ,python-bz2file)))
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
    (version "1.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bioinformatics-centre/kaiju")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "119pzi0ddzv9mjg4wwa6han0cwr3k3ssn7kirvsjfcq05mi5ka0x"))))
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
               (copy-recursively "bin" bin))
             #t)))))
    (inputs
     `(("perl" ,perl)
       ("zlib" ,zlib)))
    (home-page "http://kaiju.binf.ku.dk/")
    (synopsis "Fast and sensitive taxonomic classification for metagenomics")
    (description "Kaiju is a program for sensitive taxonomic classification
of high-throughput sequencing reads from metagenomic whole genome sequencing
experiments.")
    (license license:gpl3+)))

(define-public macs
  (package
    (name "macs")
    (version "2.1.1.20160309")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "MACS2" version))
              (sha256
               (base32
                "09ixspd1vcqmz1c81ih70xs4m7qml2iy5vyx1y74zww3iy1vl210"))))
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
    (version "7.394")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://mafft.cbrc.jp/alignment/software/mafft-" version
                    "-without-extensions-src.tgz"))
              (file-name (string-append name "-" version ".tgz"))
              (sha256
               (base32
                "0bacjkxfg944p5khhyh5rd4y7wkjc9qk4v2jjj442sqlq0f8ar7b"))))
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
    (version "2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/marbl/mash.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "049hwcc059p2fd9vwndn63laifvvsi0wmv84i6y1fr79k15dxwy6"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled kseq.
                  ;; TODO: Also delete bundled murmurhash and open bloom filter.
                  (delete-file "src/mash/kseq.h")
                  #t))))
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
             (substitute* '("src/mash/Sketch.cpp"
                            "src/mash/CommandFind.cpp"
                            "src/mash/CommandScreen.cpp")
               (("^#include \"kseq\\.h\"")
                "#include \"htslib/kseq.h\""))
             #t))
         (add-after 'fix-includes 'use-c++14
           (lambda _
             ;; capnproto 0.7 requires c++14 to build
             (substitute* "configure.ac"
               (("c\\+\\+11") "c++14"))
             (substitute* "Makefile.in"
               (("c\\+\\+11") "c++14"))
             #t)))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://bitbucket.org/berkeleylab/metabat.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hyg2smw1nz69mfvjpk45xyyychmda92c80a0cv7baji84ri4iyn"))
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
    (version "0.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ctSkennerton/minced.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f5h9him0gd355cnx7p6pnxpknhckd4g0v62mg8zyhfbx9as25fv"))))
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
               (chmod wrapper #o555))
             #t)))))
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
              (snippet '(begin
                          (substitute* "setup.py"
                            ;; Use setuptools, or else the executables are not
                            ;; installed.
                            (("distutils.core") "setuptools")
                            ;; use "gcc" instead of "cc" for compilation
                            (("^defines")
                             "cc.set_executables(
compiler='gcc',
compiler_so='gcc',
linker_exe='gcc',
linker_so='gcc -shared'); defines"))
                          #t))))
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
           (lambda _ (invoke "./muscle" "-version") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "muscle" bin)
               #t))))))
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
       (uri (pypi-uri "WarpedLMM" version ".zip"))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://bitbucket.org/regulatorygenomicsupf/pyicoteo.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hz5g8d25lbjy1wpscr490l0lmyvaix893hhax4fxnh1h9w34w8p"))))
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
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyattpd/Prodigal.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fs1hqk83qjbjhrvhw6ni75zakx5ki1ayy3v6wwkn3xvahc9hi5s"))))
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
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AJ/AJPAGE/Bio-Roary-"
             version ".tar.gz"))
       (sha256
        (base32
         "0qxrds9wx7cfhlkihrp6697kx0flhhxymap9fwan0b3rbdhcnmff"))))
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
             (for-each (lambda (file)
                         (display file)(display "\n")
                         (invoke "perl" file))
                       (find-files "t" ".*\\.t$"))
             #t))
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
    (version "8.2.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stamatak/standard-RAxML.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1jqjzhch0rips0vp04prvb8vmc20c5pdmsqn8knadcf91yy859fh"))))
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
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deweylab/RSEM.git")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1jlq11d1p8qp64w75yj8cnbbd1a93viq10pzsbwal7vdn8fg13j1"))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; remove bundled copy of boost and samtools
           (delete-file-recursively "boost")
           (delete-file-recursively "samtools-1.3")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:make-flags
       (list (string-append "BOOST="
                            (assoc-ref %build-inputs "boost")
                            "/include/")
             (string-append "SAMHEADERS="
                            (assoc-ref %build-inputs "htslib")
                            "/include/htslib/sam.h")
             (string-append "SAMLIBS="
                            (assoc-ref %build-inputs "htslib")
                            "/lib/libhts.a"))
       #:phases
       (modify-phases %standard-phases
         ;; No "configure" script.
         ;; Do not build bundled samtools library.
         (replace 'configure
           (lambda _
             (substitute* "Makefile"
               (("^all : \\$\\(PROGRAMS\\).*") "all: $(PROGRAMS)\n")
               (("^\\$\\(SAMLIBS\\).*") ""))
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
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (prog)
                           (wrap-program (string-append out "/bin/" prog)
                             `("PERL5LIB" ":" prefix
                               (,(string-append out "/lib/perl5/site_perl")))))
                         '("rsem-calculate-expression"
                           "rsem-control-fdr"
                           "rsem-generate-data-matrix"
                           "rsem-generate-ngvector"
                           "rsem-plot-transcript-wiggles"
                           "rsem-prepare-reference"
                           "rsem-run-ebseq"
                           "rsem-run-prsem-testing-procedure")))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("r-minimal" ,r-minimal)
       ("perl" ,perl)
       ("htslib" ,htslib-1.3)
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
             (("^have_pysam = False") "have_pysam = True"))
           #t))))
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
             (replace 'bootstrap
               (lambda _
                 (substitute* "gen_tools_am"
                   (("/usr/bin/env.*") (which "perl")))
                 (invoke "bash" "gen_auto")
                 #t))
             (add-after 'build 'build-additional-tools
               (lambda* (#:key make-flags #:allow-other-keys)
                 (for-each (lambda (dir)
                             (with-directory-excursion (string-append "tools/" dir)
                               (apply invoke "make" make-flags)))
                           dirs)
                 #t))
             (add-after 'install 'install-additional-tools
               (lambda* (#:key make-flags #:allow-other-keys)
                 (for-each (lambda (dir)
                             (with-directory-excursion (string-append "tools/" dir)
                               (apply invoke `("make" ,@make-flags "install"))))
                           dirs)
                 #t))))))
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
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "10ilqbmm7ri8z431sn90lvbjwizd0hhkf9rcqw8j823hf26nhgq8"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Delete bundled htslib.
                   (delete-file-recursively "htslib-1.9")
                   #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((ice-9 ftw)
                  (ice-9 regex)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:configure-flags (list "--with-ncurses")
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
       #:make-flags
       (list "LIBCURSES=-lncurses")
       ,@(substitute-keyword-arguments (package-arguments samtools)
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
         ("zlib:static" ,zlib "static")
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
    (version "2.9.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ncbi/ngs.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17c0v1nah3g3d2ib5bbi0vhma1ghd6vb9xycavqsh64lhp840rk3"))))
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
               (invoke "./configure"
                       (string-append "--build-prefix=" (getcwd) "/build")
                       (string-append "--prefix=" out))
               #t)))
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
    (version "2.9.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ncbi/ncbi-vdb.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l4ny67nxwv1lagk9wwjbrgm7ln7adci6dnpc7k1yaln6shj0qpm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f ; not supported
       #:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _ (for-each make-file-writable (find-files "." ".*")) #t))
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
               (invoke "./configure"
                       (string-append "--build-prefix=" (getcwd) "/build")
                       (string-append "--prefix=" (assoc-ref outputs "out"))
                       (string-append "--debug")
                       (string-append "--with-xml2-prefix="
                                      (assoc-ref inputs "libxml2"))
                       (string-append "--with-ngs-sdk-prefix="
                                      (assoc-ref inputs "ngs-sdk"))
                       (string-append "--with-hdf5-prefix="
                                      (assoc-ref inputs "hdf5")))
               #t)))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chrchang/plink-ng.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02npdwgkpfkdnhw819rhj5kw02a5k5m90b14zq9zzya4hyg929c0"))))
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
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/smithlabcode/preseq/"
                                  "releases/download/v" version
                                  "/preseq_v" version ".tar.bz2"))
              (sha256
               (base32 "149x9xmk1wy1gff85325yfzqc0qk4sgp1w6gbyj9cnji4x1dszbl"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Remove bundled samtools.
                          (delete-file-recursively "samtools")
                          #t))))
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
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "screed" version))
       (sha256
        (base32
         "148vcb7w2wr6a4w6vs2bsxanbqibxfk490zbcbg4m61s8669zdjx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Tests must be run after installation, as the "screed" command does
         ;; not exist right after building.
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "PYTHONPATH"
                       (string-append out "/lib/python"
                                      (string-take (string-take-right
                                                    (assoc-ref inputs "python")
                                                    5) 3)
                                      "/site-packages:"
                                      (getenv "PYTHONPATH")))
               (setenv "PATH" (string-append out "/bin:" (getenv "PATH"))))
             (invoke "python" "setup.py" "test")
             #t)))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-runner" ,python-pytest-runner)))
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
    (version "2.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncbi/sra-tools.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0663gcdxkziwsmlznjxysb00621rllpbz6jwsfifq7z3dj3lwm8b"))))
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
             (invoke "./configure"
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
                                    (assoc-ref inputs "hdf5")))
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
    (version "2.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/seqan/seqan/releases/"
                                  "download/seqan-v" version
                                  "/seqan-library-" version ".tar.xz"))
              (sha256
               (base32
                "19a1rlxx03qy1i1iriicly68w64yjxbv24g9gdywnfmq998v35yx"))))
    ;; The documentation is 7.8MB and the includes are 3.6MB heavy, so it
    ;; makes sense to split the outputs.
    (outputs '("out" "doc"))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((tar (assoc-ref %build-inputs "tar"))
               (xz  (assoc-ref %build-inputs "xz"))
               (out (assoc-ref %outputs "out"))
               (doc (assoc-ref %outputs "doc")))
           (setenv "PATH" (string-append tar "/bin:" xz "/bin"))
           (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
           (chdir (string-append "seqan-library-" ,version))
           (copy-recursively "include" (string-append out "/include"))
           (copy-recursively "share"  (string-append doc "/share"))
           #t))))
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)
       ("xz" ,xz)))
    (home-page "http://www.seqan.de")
    (synopsis "Library for nucleotide sequence analysis")
    (description
     "SeqAn is a C++ library of efficient algorithms and data structures for
the analysis of sequences with the focus on biological data.  It contains
algorithms and data structures for string representation and their
manipulation, online and indexed string search, efficient I/O of
bioinformatics file formats, sequence alignment, and more.")
    (license license:bsd-3)))

(define-public seqan-1
  (package (inherit seqan)
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
           (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
           (chdir (string-append "seqan-library-" ,version))
           (copy-recursively "include" (string-append out "/include"))
           (copy-recursively "share"  (string-append doc "/share"))
           #t))))
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)
       ("bzip2" ,bzip2)))))

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
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lh3/seqtk.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bfzlqa84b5s1qi22blmmw2s8xdyp9h9ydcq22pfjhh5gab3yz6l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           ;; There are no tests, so we just run a sanity check.
           (lambda _ (invoke "./seqtk" "seq") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (install-file "seqtk" bin)
               #t))))))
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
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/amplab/snap.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01w3qq4wm07z73vky0cfwlmrbf50n3w722cxrlzxfi99mnb808d8"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check (lambda _ (invoke "./unit_tests") #t))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/biocore/sortmerna.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j3mbz4n25738yijmjbr5r4fyvkgm8v5vn3sshyfvmyqf5q9byqf"))))
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
    (version "2.7.0b")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alexdobin/STAR.git")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1lih6cbpvnvhyvvswdhy06mwyzvwax96m723378v4z6psqzsh11d"))
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
                (string-append pre "Built with Guix" post)))
             #t))
         ;; See https://github.com/alexdobin/STAR/pull/562
         (add-after 'enter-source-dir 'add-missing-header
           (lambda _
             (substitute* "SoloReadFeature_inputRecords.cpp"
               (("#include \"binarySearch2.h\"" h)
                (string-append h "\n#include <math.h>")))
             #t))
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

(define-public starlong
  (package (inherit star)
    (name "starlong")
    (arguments
     (substitute-keyword-arguments (package-arguments star)
       ((#:make-flags flags)
        `(list "STARlong"))
       ((#:phases phases)
        `(modify-phases ,phases
           ;; Allow extra long sequence reads.
           (add-after 'unpack 'make-extra-long
             (lambda _
               (substitute* "source/IncludeDefine.h"
                 (("(#define DEF_readNameLengthMax ).*" _ match)
                  (string-append match "900000\n")))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
                 (install-file "STARlong" bin))
               #t))))))))

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
               (copy-recursively "../bin" bin))
             #t))
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
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "taxtastic" version))
              (sha256
               (base32
                "03pysw79lsrvz4lwzis88j15067ffqbi4cid5pqhrlxmd6bh8rrk"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "python" "-m" "unittest" "discover" "-v") #t)))))
    (propagated-inputs
     `(("python-sqlalchemy" ,python2-sqlalchemy)
       ("python-decorator" ,python2-decorator)
       ("python-biopython" ,python2-biopython)
       ("python-pandas" ,python2-pandas)
       ("python-psycopg2" ,python2-psycopg2)
       ("python-fastalite" ,python2-fastalite)
       ("python-pyyaml" ,python2-pyyaml)
       ("python-six" ,python2-six)
       ("python-jinja2" ,python2-jinja2)
       ("python-dendropy" ,python2-dendropy)))
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

(define-public r-scde
  (package
    (name "r-scde")
    (version "1.99.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hms-dbmi/scde.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10na2gyka24mszdxf92wz9h2c13hdf1ww30c68gfsw53lvvhhhxb"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-mgcv" ,r-mgcv)
       ("r-rook" ,r-rook)
       ("r-rjson" ,r-rjson)
       ("r-cairo" ,r-cairo)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-edger" ,r-edger)
       ("r-quantreg" ,r-quantreg)
       ("r-nnet" ,r-nnet)
       ("r-rmtstat" ,r-rmtstat)
       ("r-extremes" ,r-extremes)
       ("r-pcamethods" ,r-pcamethods)
       ("r-biocparallel" ,r-biocparallel)
       ("r-flexmix" ,r-flexmix)))
    (home-page "https://hms-dbmi.github.io/scde/")
    (synopsis "R package for analyzing single-cell RNA-seq data")
    (description "The SCDE package implements a set of statistical methods for
analyzing single-cell RNA-seq data.  SCDE fits individual error models for
single-cell RNA-seq measurements.  These models can then be used for
assessment of differential expression between groups of cells, as well as
other types of analysis.  The SCDE package also contains the pagoda framework
which applies pathway and gene set overdispersion analysis to identify aspects
of transcriptional heterogeneity among single cells.")
    ;; See https://github.com/hms-dbmi/scde/issues/38
    (license license:gpl2)))

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

(define-public r-genefilter
  (package
    (name "r-genefilter")
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "genefilter" version))
       (sha256
        (base32
         "1jq7lam4dnbvz55lx93kcl9afl8xfjd6xs374d35m21bkay418kj"))))
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
    (version "1.22.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq2" version))
       (sha256
        (base32
         "0n5ah84mxn87p45drzy0wh2yknmzj1q5i6gv0v9vgg1lj7awb91r"))))
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
    (version "1.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DEXSeq" version))
       (sha256
        (base32
         "1wsj1kqfrakmjnlplxmrv17r2spzcdkmwdkhggyjbf8mdhqs3w16"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationForge" version))
       (sha256
        (base32
         "13yvhf3yskmvhs8szs6rkw93h81h5xqa3h19h91pp6nprhc8s3ll"))))
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
    (version "1.58.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RBGL" version))
       (sha256
        (base32
         "0vhnh47pswnp27c0zqcbnnsayfmq3cxcgrs9g860555ldqfl4cyl"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GSEABase" version))
       (sha256
        (base32
         "110al7x0ig8plzrprvhwc7xshi1jzpj2n8llhhg2fh6v6k0k6awr"))))
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
    (version "2.48.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Category" version))
       (sha256
        (base32
         "18rsxlwa1l06i635cnznb9b2zssqcgb71pihky29gl2gwp7a654b"))))
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
    (version "2.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOstats" version))
       (sha256
        (base32
         "0wlqqgfynwqnqhckhsfjwg9zkj6hkmzwd5y76dhqz720vy21rcln"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ShortRead" version))
       (sha256
        (base32
         "0iks123i1adkb9i2q4wvfqdmmj9dy867jvngj9757y8gj6xbcpy1"))))
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
    (version "1.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "systemPipeR" version))
       (sha256
        (base32
         "0qzydz87rld2nhwzbfgrw5jfgh8maa9y54mjx9c4285m11qj2shq"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "groHMM" version))
       (sha256
        (base32
         "1ph92fv44b90v7mk4b1mjvv0dlrhl8ba01klxbnd0vs4qn9zxplh"))))
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

(define-public vsearch
  (package
    (name "vsearch")
    (version "2.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/torognes/vsearch.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vhrpjfdf75ba04b24xknp41790cvcgwl0vgpy7qbzj5xh2521ss"))
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
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio" version))
       (sha256
        (base32
         "1d56amdsjv1mag7m6gv2w0xij8hqx1v5xbdjsix8sp3yp36m7938"))))
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

(define-public r-biocinstaller
  (package
    (name "r-biocinstaller")
    (version "1.32.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocInstaller" version))
              (sha256
               (base32
                "1s1f9qhyf3mc73ir25x2zlgi9hf45a37lg4z8fbva4i21hqisgsl"))))
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
    (version "1.50.10")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biocViews" version))
              (sha256
               (base32
                "06ms82pyc5rxbd9crfvqjxcwpafv0c627i83v80d12925mrc51h8"))))
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

(define-public r-biocstyle
  (package
    (name "r-biocstyle")
    (version "2.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocStyle" version))
              (sha256
               (base32
                "01lm8xljilj666fcl3wnw82dxkcxnlr294lddr553rm8xr5nwg31"))))
    (properties
     `((upstream-name . "BiocStyle")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocmanager" ,r-biocmanager)
       ("r-bookdown" ,r-bookdown)
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocCheck" version))
              (sha256
               (base32
                "0zamvs5jar38293ff27imvwy0ra25y64ls9z8w3q1y4jcp8p8pg7"))))
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
       ("r-knitr" ,r-knitr)
       ("r-optparse" ,r-optparse)
       ("r-biocmanager" ,r-biocmanager)
       ("r-biocviews" ,r-biocviews)
       ("r-stringdist" ,r-stringdist)))
    (home-page "https://bioconductor.org/packages/BiocCheck")
    (synopsis "Executes Bioconductor-specific package checks")
    (description "This package contains tools to perform additional quality
checks on R packages that are to be submitted to the Bioconductor repository.")
    (license license:artistic2.0)))

(define-public r-s4vectors
  (package
    (name "r-s4vectors")
    (version "0.20.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "S4Vectors" version))
              (sha256
               (base32
                "18whrw67nxn82xshckl2pjy7d14sa3c27h3n9naqyqwz88lr6dzg"))))
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

(define-public r-iranges
  (package
    (name "r-iranges")
    (version "2.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IRanges" version))
              (sha256
               (base32
                "0ljppsk611xi72gc8mbdx1311b63b1ijd401jz5xmxk5frla1nc1"))))
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
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://bioconductor.org/packages/release/"
                                  "data/annotation/src/contrib/GenomeInfoDbData_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0di6nlqpsyqf693k2na65ayqldih563x3zfrczpqc5q2hl5kg35c"))))
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
    (version "1.18.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomeInfoDb" version))
              (sha256
               (base32
                "07bm35jcczpyxap0b3gky4b28z38z423sngzsm19d9krjxr76b5p"))))
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
    (version "3.24.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "edgeR" version))
              (sha256
               (base32
                "15yimsbsxmxhlsfmgw5j7fd8qn08zz4xqxrir1c6n2dc103y22xg"))))
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
    (version "1.28.13")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "VariantAnnotation" version))
              (sha256
               (base32
                "1a7b0bg579ynpbfh5dk87fdgl62r9cwk4zmrl61m6zil7881p3gh"))))
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
    (version "3.38.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "limma" version))
              (sha256
               (base32
                "08va8jggmv61wym955mnb1n31mgikrmjys7dl1kp5hp3yia8jg7l"))))
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
    (version "0.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "XVector" version))
              (sha256
               (base32
                "01fph1ydd6g0rl5mcw54spx22glq2kqv7wyw8bqw0plmabzcwwdm"))))
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
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicRanges" version))
              (sha256
               (base32
                "0bgh14d15dpf2iy36qinw45r6n45rqkf0ghazrdl3jfva6vbrb29"))))
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
    (version "2.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biobase" version))
              (sha256
               (base32
                "10nr6nrkj5vlq8hsgbhbhv669z0dbpz4m3vz9k32rx1czbrrqwin"))))
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
    (version "1.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationDbi" version))
              (sha256
               (base32
                "1954vimkx5yb9irppq8vssq0f3yjkg36w38b9r0rqmijx1ps7x5d"))))
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
    (version "2.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biomaRt" version))
              (sha256
               (base32
                "1lshkknp7dmr3p6dd2zbv86cc71h53ggh9ji83jcjym8sgbbspl2"))))
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
    (version "1.16.6")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocParallel" version))
              (sha256
               (base32
                "1iv2xzm6lz371z0llhcxl8hmc5jfw0hjwnf1qc8d7jk9djgcaks2"))))
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
    (version "2.50.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biostrings" version))
              (sha256
               (base32
                "16cqqc8i6gb0jcz0lizfqqxsq7g0yb0ll2s9qzmb45brp07dg8f7"))))
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
    (version "1.34.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Rsamtools" version))
              (sha256
               (base32
                "02paq7klabdkaf1b8pmmbpmyqsj3yncnfsz62rgka6r4dpsilwk9"))))
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
     "This package provides an interface to the @code{samtools},
@code{bcftools}, and @code{tabix} utilities for manipulating SAM (Sequence
Alignment / Map), FASTA, binary variant call (BCF) and compressed indexed
tab-delimited (tabix) files.")
    (license license:expat)))

(define-public r-delayedarray
  (package
    (name "r-delayedarray")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DelayedArray" version))
              (sha256
               (base32
                "0cl5anqkjwvqx19snjhz0zj8cp8ibckiifl28h821h50g62nvb2f"))))
    (properties
     `((upstream-name . "DelayedArray")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
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
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SummarizedExperiment" version))
              (sha256
               (base32
                "07805572xhpj5mfwq6kw1ha21wgalqvhh4ydvafyl1bnf3r20vps"))))
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
    (version "1.18.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicAlignments" version))
              (sha256
               (base32
                "1maslav2r34wjyzh2nlwa862in1ir7i5xk57nw2nlfh5gqy112jd"))))
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
    (version "1.42.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rtracklayer" version))
              (sha256
               (base32
                "1c76g6h9lx2nm7dvb2zp9dmrpk3vanx3zaz6q9clggpj7yj5lmjd"))))
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
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (version "1.34.8")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicFeatures" version))
              (sha256
               (base32
                "1sxp86hdsg32l2c85jgic65gy92d8kxsm01264hrx6yikdhicjax"))))
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
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/GO.db_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0i3wcf5h3n0dawzc1hy0kv74f06j80c47n4p3g3fmrcxlhi3jpa5"))))
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

(define-public r-topgo
  (package
    (name "r-topgo")
    (version "2.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "topGO" version))
              (sha256
               (base32
                "1j1jcd16j564kr6qz28140fzmnh9xasi84v1c1fi98sqv30zq9bh"))))
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
    (version "1.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome" version))
              (sha256
               (base32
                "07z4zxx0khrc86qqvc7vxww8df9fh6pyks9ajxkc9gdqr5nn79j7"))))
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

(define-public r-impute
  (package
    (name "r-impute")
    (version "1.56.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "impute" version))
              (sha256
               (base32
                "08z0pj1dz5iq967nwj67qyka7ir7m5an2ggv7bsrlz3apzfsla33"))))
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
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "seqPattern" version))
              (sha256
               (base32
                "0di83qi83mrlw7i12khsq55d03hlazcywaa9m9pki1sfhafpq733"))))
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
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "genomation" version))
              (sha256
               (base32
                "0g0v4alfpqlinqinjnyzl3mrjnpbdx9ri34mcaiqbvbvg8ic8wvg"))))
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
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://bioconductor.org/packages/"
                                  "release/data/experiment/src/contrib/"
                                  "genomationData_" version ".tar.gz"))
              (sha256
               (base32
                "10xyb8akjrhmak2i0mnv1agny2ipy364q9nlibyplpzc7vdb6bw7"))))
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

(define-public r-seqlogo
  (package
    (name "r-seqlogo")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqLogo" version))
       (sha256
        (base32
         "022vr9ydwcivs7rw7kwj73gfk5gc7ckwa1q66vhd4kw9ylh70v68"))))
    (properties `((upstream-name . "seqLogo")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/seqLogo")
    (synopsis "Sequence logos for DNA sequence alignments")
    (description
     "seqLogo takes the position weight matrix of a DNA sequence motif and
plots the corresponding sequence logo as introduced by Schneider and
Stephens (1990).")
    (license license:lgpl2.0+)))

(define-public r-motifrg
  (package
    (name "r-motifrg")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifRG" version))
       (sha256
        (base32
         "1wxww6i0jgyapqclcwy0zzf9kqjvrvylr89z7yhg1izi7jnw2fka"))))
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
  (version "1.44-9")
  (source
   (origin
    (method url-fetch)
    (uri (string-append "mirror://cran/src/contrib/qtl_"
                        version ".tar.gz"))
    (sha256
     (base32
      "03lmvydln8b7666b6w46qbryhf83vsd11d4y2v95rfgvqgq66l1i"))))
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
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "zlibbioc" version))
              (sha256
               (base32
                "0bjvzy24kab7ank02cc1qk2ikcz4dllgf66wpsdl0d3zp4gn3l2h"))))
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
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhtslib" version))
       (sha256
        (base32
         "13fv78sk5g0gqfl3ks3rps3zc1k66a4lzxvgn36r7ix43yxk7hnr"))))
    (properties `((upstream-name . "Rhtslib")))
    (build-system r-build-system)
    ;; Without this a temporary directory ends up in the Rhtslib.so binary,
    ;; which makes R abort the build.
    (arguments '(#:configure-flags '("--no-staged-install")))
    (propagated-inputs
     `(("r-zlibbioc" ,r-zlibbioc)))
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bamsignals" version))
       (sha256
        (base32
         "19irfx1y1izf903vq59wxsdbf88g143zy9l89gxqawh7jfxds8w8"))))
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
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RCAS" version))
              (sha256
               (base32
                "0ss5hcg2m7gjji6dd23zxa5bd5a7knwcnada4qs5q2l4clgk39ad"))))
    (properties `((upstream-name . "RCAS")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biomart" ,r-biomart)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-cowplot" ,r-cowplot)
       ("r-data-table" ,r-data-table)
       ("r-dbi" ,r-dbi)
       ("r-dt" ,r-dt)
       ("r-genomation" ,r-genomation)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggseqlogo" ,r-ggseqlogo)
       ("r-knitr" ,r-knitr)
       ("r-motifrg" ,r-motifrg)
       ("r-org-hs-eg-db" ,r-org-hs-eg-db)
       ("r-pbapply" ,r-pbapply)
       ("r-pheatmap" ,r-pheatmap)
       ("r-plotly" ,r-plotly)
       ("r-plotrix" ,r-plotrix)
       ("r-proxy" ,r-proxy)
       ("r-rsqlite" ,r-rsqlite)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-s4vectors" ,r-s4vectors)
       ("r-topgo" ,r-topgo)))
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
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/BIMSBbioinfo/rcas-web/"
                           "releases/download/v" version
                           "/rcas-web-" version ".tar.gz"))
       (sha256
        (base32
         "0wq951aj45gqki1bickg876i993lmawkp8x24agg264br5x716db"))))
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
       ("guile-redis" ,guile-redis)))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MutationalPatterns" version))
       (sha256
        (base32
         "0w9lg1zs106h6rqvy8mhikq6q6q9syw6c1prcxr38ssh85rcih12"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ;; These two packages are suggested packages
       ("r-bsgenome-hsapiens-1000g" ,r-bsgenome-hsapiens-1000genomes-hs37d5)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-genomicranges" ,r-genomicranges)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-ggplot2" ,r-ggplot2)
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ManuSetty/SeqGL.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1r6ywvhxl3ffv48lgj7sbd582mcc6dha3ksgc2qjlvjrnkbj3799"))))
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

(define-public r-tximport
  (package
    (name "r-tximport")
    (version "1.10.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tximport" version))
              (sha256
               (base32
                "16wp09dm0cpb4mc00nmglfb8ica7qb4a55vm8ajgzyagbpfdd44l"))))
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
    (version "2.26.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rhdf5" version))
              (sha256
               (base32
                "10zkw3k13wmvyif417gplyf6rwp2gpkjasw97lhwv2f9i32rry9l"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rhdf5lib" ,r-rhdf5lib)))
    (inputs
     `(("zlib" ,zlib)))
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
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationFilter" version))
              (sha256
               (base32
                "0wrr10cxjzmxx46vjzq2nsf6xlqz1sqwx4xm0sk3d77ff8wmph4x"))))
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
           (lambda _ (invoke "autoreconf" "-vif") #t)))))
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
              (uri (pypi-uri "PePr" version))
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
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ekg/filevercmp.git")
                      (commit commit)))
                (file-name (git-file-name name commit))
                (sha256
                 (base32
                  "1j9vxsy0y050v59h0q1d6501fcw1kjvj0d18l1xk2zyg0jzj247c"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests to run.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; There is no configure phase.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "filevercmp" bin)
                 #t))))))
      (home-page "https://github.com/ekg/filevercmp")
      (synopsis "This program compares version strings")
      (description "This program compares version strings.  It intends to be a
replacement for strverscmp.")
      (license license:gpl3+))))

(define-public multiqc
  (package
    (name "multiqc")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "multiqc" version))
       (sha256
        (base32
         "02iihfl0w0hpnr4pa0sbd1y9qxrg3ycyhjp5lidkcrqh1lmzs3zy"))))
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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               ;; MultiQC 1.5 ‘requires’ a version of python-matplotlib older
               ;; than the one in Guix, but should work fine with 2.2.2.
               ;; See <https://github.com/ewels/MultiQC/issues/725> and
               ;; <https://github.com/ewels/MultiQC/issues/732> for details.
               (("['\"]matplotlib.*?['\"]")
                "'matplotlib'"))
             #t)))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chipseq" version))
       (sha256
        (base32
         "1pp1rm5fs3hlar5x4dl3a3b4gara7qwf81dbvka6r1n78hrf9x1b"))))
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
    (version "2.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CopywriteR" version))
       (sha256
        (base32
         "1hbiw0m9hmx4na9v502pxf8y5wvxzr68r4d3fqr2755gxx86qck6"))))
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
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "methylKit" version))
              (sha256
               (base32
                "1zcfwy7i10aqgnf7r0c41hakb5aai3s3n9y8pc6a98vimz51ly2z"))))
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
    (version "3.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "sva" version))
       (sha256
        (base32
         "0czja4c5jxa0g3fspi90nyajqmvzb29my4ykv2wi66h43f5dlwhq"))))
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
    (version "7.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "seqminer" version))
       (sha256
        (base32
         "1jydcpkw4rwfp983j83kipvsvr10as9pb49zzn3c2v09k1gh3ymy"))))
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
    (version "1.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "MALDIquant" version))
       (sha256
        (base32
         "11zbvm1vw8zn2vmymvydgdczvwj961s2knvrn1q4gbziwi5gqvlc"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ProtGenerics" version))
       (sha256
        (base32
         "053mmxhzncqgigl2iqjlq56qzimlw2zzw31wpzw19rf7rld1vi3b"))))
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
    (version "2.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzR" version))
       (sha256
        (base32
         "19fn58zl59kd0hsjjc6y975y9187nfls0028a4k3v0s9wfg5b3vn"))
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
     `(;; XXX Boost 1.69 will not work here.
       ("boost" ,boost-for-mysql) ; use this instead of the bundled boost sources
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-ncdf4" ,r-ncdf4)
       ("r-protgenerics" ,r-protgenerics)
       ("r-rcpp" ,r-rcpp)
       ("r-rhdf5lib" ,r-rhdf5lib)
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
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyio" version))
       (sha256
        (base32
         "1s4zp1211vf0krxzch9v3q3r6vs8hihqppq18i2fpvwlknfja7c1"))))
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
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affy" version))
       (sha256
        (base32
         "0x8h4fk2igv7vykqfvf6v9whmx3344v5rf3gyfajd431xkjldz6k"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affyio" ,r-affyio)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocmanager" ,r-biocmanager)
       ("r-preprocesscore" ,r-preprocesscore)
       ("r-zlibbioc" ,r-zlibbioc)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://bioconductor.org/packages/affy")
    (synopsis "Methods for affymetrix oligonucleotide arrays")
    (description
     "This package contains functions for exploratory oligonucleotide array
analysis.")
    (license license:lgpl2.0+)))

(define-public r-vsn
  (package
    (name "r-vsn")
    (version "3.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "vsn" version))
       (sha256
        (base32
         "1g6qkpykw99jm2wv2i61dg2ffwk0n8fm4s5pm2q4c024vw5c9b69"))))
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
    (version "1.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzID" version))
       (sha256
        (base32
         "15yd4bdxprw3kg7zj2k652y3yr3si781iw28jqvnkm0gsc23rd0c"))))
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
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pcaMethods" version))
       (sha256
        (base32
         "0ik82s9bsdj4a1mmv0a3k6yisa92mxx7maf3dvip1r8gqlm3dyng"))))
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
    (version "2.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnbase" version))
       (sha256
        (base32
         "1kl1d7byphnfpmbl5fzbgs68dxskhpsdyx7ka51bpfn0nv3pp492"))))
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
       ("r-mass" ,r-mass)
       ("r-mzid" ,r-mzid)
       ("r-mzr" ,r-mzr)
       ("r-pcamethods" ,r-pcamethods)
       ("r-plyr" ,r-plyr)
       ("r-preprocesscore" ,r-preprocesscore)
       ("r-protgenerics" ,r-protgenerics)
       ("r-rcpp" ,r-rcpp)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scales" ,r-scales)
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
    (version "1.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnID" version))
       (sha256
        (base32
         "077n6ljcnnl7q4w0qj8v46vm4sjk9vzzfqf7wsc6lz0wmyzqdng3"))))
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
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "Seurat" version))
              (sha256
               (base32
                "183lm2wk0i3g114jbdf7pb4ssizr48qzqv3cknbsiackr8kvpsvc"))))
    (properties `((upstream-name . "Seurat")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ape" ,r-ape)
       ("r-cluster" ,r-cluster)
       ("r-cowplot" ,r-cowplot)
       ("r-fitdistrplus" ,r-fitdistrplus)
       ("r-future" ,r-future)
       ("r-future-apply" ,r-future-apply)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-ggridges" ,r-ggridges)
       ("r-ica" ,r-ica)
       ("r-igraph" ,r-igraph)
       ("r-irlba" ,r-irlba)
       ("r-kernsmooth" ,r-kernsmooth)
       ("r-lmtest" ,r-lmtest)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-metap" ,r-metap)
       ("r-pbapply" ,r-pbapply)
       ("r-plotly" ,r-plotly)
       ("r-png" ,r-png)
       ("r-rann" ,r-rann)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-rcppprogress" ,r-rcppprogress)
       ("r-reticulate" ,r-reticulate)
       ("r-rlang" ,r-rlang)
       ("r-rocr" ,r-rocr)
       ("r-rsvd" ,r-rsvd)
       ("r-rtsne" ,r-rtsne)
       ("r-scales" ,r-scales)
       ("r-sctransform" ,r-sctransform)
       ("r-sdmtools" ,r-sdmtools)
       ("r-tsne" ,r-tsne)))
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
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "aroma.light" version))
       (sha256
        (base32
         "0vfifgpqxjjncbiv6gvlk9jmj14j90r9f30bqk3ks9v1csjnjhrb"))))
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
    (version "1.34.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq" version))
       (sha256
        (base32
         "0bpiixczbhlyaiinpbl6xrpmv72k2bq76bxnw06gl35m4pgs94p2"))))
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
    (version "2.16.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EDASeq" version))
       (sha256
        (base32
         "0559ph606ps2g9bwbl0a2knkcs5w581n9igngpjxvk5p56k24gb5"))))
    (properties `((upstream-name . "EDASeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-aroma-light" ,r-aroma-light)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocmanager" ,r-biocmanager)
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "interactiveDisplayBase" version))
       (sha256
        (base32
         "04xz3dkwan2s5ic1mwkdfnggm0l41mgqfagx160bcsrpkw6z7ark"))))
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
    (version "2.14.5")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationHub" version))
       (sha256
        (base32
         "0iyrxaijl4614iz5c1j53227xy2g756p3bx7hcwglcybh0k30nki"))))
    (properties `((upstream-name . "AnnotationHub")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocmanager" ,r-biocmanager)
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fastseg" version))
       (sha256
        (base32
         "1l8mdjpfpgwqdss2ywjkb8b4h55wf8v6kmyxdlvy04ds2hj16sb1"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGREST" version))
       (sha256
        (base32
         "0blpd5a7whd2sswfhqd17h58hg06ymaf80gapdr9ja43hnnlj309"))))
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
    (version "2.32.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gage" version))
       (sha256
        (base32
         "02g796sb1800ff0f1mq9f2m5wwzpf8pnfzajs49i68dhq2hm01a8"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicFiles" version))
       (sha256
        (base32
         "0qf2yj4lfnnk64fk125n8sqms01shfqiik04nasx2z3k129ykpxp"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ComplexHeatmap" version))
       (sha256
        (base32
         "0s01dzcfj1lmpqfpsbqw7r4858krfzy499lz4cwx4fq3mbyvy2aj"))))
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
    (version "1.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DirichletMultinomial" version))
       (sha256
        (base32
         "0vazfjzqy78p5g7dnv30lbqbj4bhq4zafd2wh6gdwy2il1fd78xa"))))
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
    (version "2.6.8")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ensembldb" version))
       (sha256
        (base32
         "0gijx2l2y00h6gfj3gfr7rd4vva6qf2vkfdfy5gdmvqlxy84ka38"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationfilter" ,r-annotationfilter)
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "OrganismDbi" version))
       (sha256
        (base32
         "11pyv56cy4iy095h40k6k0mpjdlh6gsb4ld3s57nfa9nd4ypx3yi"))))
    (properties `((upstream-name . "OrganismDbi")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocmanager" ,r-biocmanager)
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
    (version "1.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biovizBase" version))
       (sha256
        (base32
         "0v5gvcx180qn5487i1dph9abadw3ggqwp5yzy41jswzbdc8q6sbm"))))
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
       ("r-rlang" ,r-rlang)
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggbio" version))
       (sha256
        (base32
         "0wq49qqzkcn8s19xgaxf2s1j1a563d7pbhhvris6fhxfdjsz4934"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; See https://github.com/tengfei/ggbio/issues/117
         ;; This fix will be included in the next release.
         (add-after 'unpack 'fix-typo
           (lambda _
             (substitute* "R/GGbio-class.R"
               (("fechable") "fetchable"))
             #t)))))
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
       ("r-rlang" ,r-rlang)
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
    (version "0.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gProfileR" version))
       (sha256
        (base32
         "12nwidbnqmnfy5dnqga26byslvdnkrpz2fi19qfcby6xx0wbndk7"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gQTLBase" version))
       (sha256
        (base32
         "1lbk1m1mkvbk30flk5pf3pcrnm2s0sj5r48kbjgad39dsvd8zgqx"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "snpStats" version))
       (sha256
        (base32
         "1pplx4pf9bqi7v5v1l74yknc1s61carvbqkf327ky7vbvp0bck33"))))
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
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "erma" version))
       (sha256
        (base32
         "0hj9iz904rr1y66442lkxjywkw1ydyxxlhmjirawbf09ic5ad4g9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-homo-sapiens" ,r-homo-sapiens)
       ("r-iranges" ,r-iranges)
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
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ldblock" version))
       (sha256
        (base32
         "01lf74pby7si2g3kgc10qzr6lkcbigqcgqs2j3anc38vzxv0zhwv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-erma" ,r-erma)
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
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gQTLstats" version))
       (sha256
        (base32
         "1rkbnb3h02fdksc4nacqvmq4jgbj9fz4hm7j51yr2ggcgcykwraa"))))
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
       ("r-homo-sapiens" ,r-homo-sapiens)
       ("r-iranges" ,r-iranges)
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
    (version "1.26.5")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Gviz" version))
       (sha256
        (base32
         "1dpkcaar7qgzg3vgafvkplprhwmhzpb7ph009kr6ajm36hx4z81c"))))
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
    (version "2.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gwascat" version))
       (sha256
        (base32
         "1fnyjydhicq4ayrv0lqjv48h9bd72h40s6l82g1h2ng0icwz38g0"))))
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
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Sushi" version))
              (sha256
               (base32
                "0dv5di0hgbvk9cxnqhyf18mdjl50k6bk00a89r6zgp83rbxwr1r8"))))
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
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FitHiC" version))
              (sha256
               (base32
                "15xd8mz7660q4zr9p74mq1pqps4iz7pxp8f9ifn21gwg94aq1avn"))))
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
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HiTC" version))
              (sha256
               (base32
                "11f96k1707g6milpjgnrjf3b5r42hsrxhb5d8znkcr3y3mrskdbj"))))
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

(define-public r-hdf5array
  (package
    (name "r-hdf5array")
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HDF5Array" version))
       (sha256
        (base32
         "1qwdsygcadl58qj598hfyvs8hp0hqcl9ghnhknahrlhmb7k2bd2d"))))
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
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhdf5lib" version))
       (sha256
        (base32
         "0hjhjvg2kss71fkmxlbgnyyy1agwzgq57rxkgkm4riw82x2rvw7q"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled binaries
           (delete-file-recursively "src/winlib/")
           #t))))
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
               (("'%s/libhdf5_cpp.a %s/libhdf5.a %s/libsz.a -lz'")
                "'%s/libhdf5_cpp.a %s/libhdf5.a %s/libhdf5.a -lz'")
               (("'%s/libhdf5.a %s/libsz.a -lz'")
                "'%s/libhdf5.a %s/libhdf5.a -lz'"))
             (with-directory-excursion "src"
               (invoke "tar" "xvf" (assoc-ref inputs "hdf5-source"))
               (rename-file (string-append "hdf5-" ,(package-version hdf5-1.10))
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
                 (("\\$\\{USER_LIB_DIR\\}libsz.a") "")))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("hdf5" ,hdf5-1.10)))
    (native-inputs
     `(("hdf5-source" ,(package-source hdf5-1.10))))
    (home-page "https://bioconductor.org/packages/Rhdf5lib")
    (synopsis "HDF5 library as an R package")
    (description "This package provides C and C++ HDF5 libraries for use in R
packages.")
    (license license:artistic2.0)))

(define-public r-beachmat
  (package
    (name "r-beachmat")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "beachmat" version))
       (sha256
        (base32
         "07zgmms0qg8gw7x0js46965bbhpfj2aa1h5ixdz9r332bxv9cdmr"))))
    (build-system r-build-system)
    (inputs
     `(("hdf5" ,hdf5)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-delayedarray" ,r-delayedarray)
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
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SingleCellExperiment" version))
       (sha256
        (base32
         "12139kk9cqgzpm6f3cwdsq31gj5lxamz2q939dy9fa0fa54gdaq4"))))
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
    (version "1.10.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scater" version))
              (sha256
               (base32
                "0rijhy7g5qmcn927y1wyd63la1fhyar9fv1hccsqd23jd98yc55a"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-delayedarray" ,r-delayedarray)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-dplyr" ,r-dplyr)
       ("r-ggbeeswarm" ,r-ggbeeswarm)
       ("r-ggplot2" ,r-ggplot2)
       ("r-matrix" ,r-matrix)
       ("r-plyr" ,r-plyr)
       ("r-rcpp" ,r-rcpp)
       ("r-reshape2" ,r-reshape2)
       ("r-rhdf5lib" ,r-rhdf5lib)
       ("r-s4vectors" ,r-s4vectors)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
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
    (version "1.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scran" version))
       (sha256
        (base32
         "07mgilr3gq3lnrm1fjm9zhz4w7970bjhsykln1drqy9gkzj5sn7g"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocneighbors" ,r-biocneighbors)
       ("r-biocparallel" ,r-biocparallel)
       ("r-delayedarray" ,r-delayedarray)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-dynamictreecut" ,r-dynamictreecut)
       ("r-edger" ,r-edger)
       ("r-igraph" ,r-igraph)
       ("r-limma" ,r-limma)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rhdf5lib" ,r-rhdf5lib)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scater" ,r-scater)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-statmod" ,r-statmod)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DelayedMatrixStats" version))
       (sha256
        (base32
         "03fk2avl1vyjv2wslczkc82qr0zmp1ra8iimd47pbmnnm839ly4w"))))
    (properties
     `((upstream-name . "DelayedMatrixStats")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-delayedarray" ,r-delayedarray)
       ("r-hdf5array" ,r-hdf5array)
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
    (version "2.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "phangorn" version))
       (sha256
        (base32
         "1bv86yfk5r015s7ij6v4zz7bagwrw9m13yfs5853drxb19d5h1m3"))))
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
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ,@(package-native-inputs htslib))))))

(define-public sambamba
  (package
    (name "sambamba")
    (version "0.6.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lomereiter/sambamba.git")
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "0k0cz3qcv98p6cq09zlbgnjsggxcqbcmzxg5zikgcgbr2nfq4lry"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there is no test target
       #:parallel-build? #f             ; not supported
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-ldc-version
           (lambda _
             (substitute* "gen_ldc_version_info.py"
               (("/usr/bin/env.*") (which "python3")))
             (substitute* "Makefile"
               ;; We use ldc2 instead of ldmd2 to compile sambamba.
               (("\\$\\(shell which ldmd2\\)") (which "ldc2")))
             #t))
         (add-after 'unpack 'place-biod-and-undead
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "biod") "BioD")
             (copy-recursively (assoc-ref inputs "undead") "undeaD")
             #t))
         (add-after 'unpack 'unbundle-prerequisites
           (lambda _
             (substitute* "Makefile"
               (("htslib/libhts.a lz4/lib/liblz4.a")
                "-L-lhts -L-llz4")
               ((" lz4-static htslib-static") ""))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "bin/sambamba" bin)
               #t))))))
    (native-inputs
     `(("ldc" ,ldc)
       ("rdmd" ,rdmd)
       ("python" ,python)
       ("biod"
        ,(let ((commit "4f1a7d2fb7ef3dfe962aa357d672f354ebfbe42e"))
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
               "1k5pdjv1qvi0a3rwd1sfq6zbj37l86i7bf710m4c0y6737lxj426")))))
       ("undead"
        ,(let ((commit "9be93876982b5f14fcca60832563b3cd767dd84d"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/biod/undeaD.git")
                   (commit commit)))
             (file-name (string-append "undead-"
                                       (string-take commit 9)
                                       "-checkout"))
             (sha256
              (base32
               "1xfarj0nqlmi5jd1vmcmm7pabzaf9hxyvk6hp0d6jslb5k9r8r3d")))))))
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
    (version "2.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KlugerLab/Ritornello.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xahvq215qld7x1w8vpa5zbrsj6p9crb9shqa2x89sb0aaxa02jk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-samtools-references
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("src/SamStream.h"
                            "src/FLD.cpp")
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
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FelixKrueger/TrimGalore.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1y31wbxwkm9xqzr5zv1pk5q418whnmlmgmfyxxpnl12h83m2i9iv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             ;; Trim Galore tries to figure out what version of Python
             ;; cutadapt is using by looking at the shebang.  Of course that
             ;; doesn't work, because cutadapt is wrapped in a shell script.
             (substitute* "trim_galore"
               (("my \\$python_return.*")
                "my $python_return = \"Python 3.999\";\n"))
             #t))
         (delete 'build)
         (add-after 'unpack 'hardcode-tool-references
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "trim_galore"
               (("\\$path_to_cutadapt = 'cutadapt'")
                (string-append "$path_to_cutadapt = '"
                               (assoc-ref inputs "cutadapt")
                               "/bin/cutadapt'"))
               (("\\$compression_path = \"gzip\"")
                (string-append "$compression_path = \""
                               (assoc-ref inputs "gzip")
                               "/bin/gzip\""))
               (("\"gunzip")
                (string-append "\""
                               (assoc-ref inputs "gzip")
                               "/bin/gunzip"))
               (("\"pigz")
                (string-append "\""
                               (assoc-ref inputs "pigz")
                               "/bin/pigz")))
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
       ("pigz" ,pigz)
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
                             out "/lib/python"
                             ,(version-major+minor
                                (package-version python))
                             "/site-packages/gess/")))
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
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/3DGenomes/TADbit.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07g3aj648prmsvxp9caz5yl41k0y0647vxh0f5p3w8376mfiljd0"))))
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
    (home-page "https://3dgenomes.github.io/TADbit/")
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ENCODE-DCC/kentUtils.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0n1wbyjpzii2b9qhyp9r1q76j623cggpg3y8fmw78ld3z4y7ivha"))
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
         (add-after 'unpack 'fix-permissions
           (lambda _ (make-file-writable "src/inc/localEnvironment.mk") #t))
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
       ("mariadb" ,mariadb)
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
                      (bin (string-append target "/bin"))
                      (doc (string-append target "/share/doc/f-seq"))
                      (lib (string-append target "/lib")))
                 (mkdir-p target)
                 (mkdir-p doc)
                 (substitute* "bin/linux/fseq"
                   (("java") (which "java"))
                   (("\\$REALDIR/../lib/commons-cli-1.1.jar")
                    (string-append (assoc-ref inputs "java-commons-cli")
                                   "/share/java/commons-cli.jar"))
                   (("REALDIR=.*")
                    (string-append "REALDIR=" bin "\n")))
                 (install-file "README.txt" doc)
                 (install-file "bin/linux/fseq" bin)
                 (install-file "build~/fseq.jar" lib)
                 (copy-recursively "lib" lib)
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
    (version "0.20.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FelixKrueger/Bismark.git")
             (commit version)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "0xchm3rgilj6vfjnyzfzzymfd7djr64sbrmrvs3njbwi66jqbzw9"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:modules ((guix build utils)
                  (ice-9 popen)
                  (srfi srfi-26)
                  (guix build perl-build-system))
       #:phases
       (modify-phases %standard-phases
         ;; The bundled plotly.js is minified.
         (add-after 'unpack 'replace-plotly.js
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((file (assoc-ref inputs "plotly.js"))
                    (installed "plotly/plotly.js"))
               (let ((minified (open-pipe* OPEN_READ "uglify-js" file)))
                 (call-with-output-file installed
                   (cut dump-port minified <>))))
             #t))
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share   (string-append out "/share/bismark"))
                    (docdir  (string-append out "/share/doc/bismark"))
                    (docs    '("Docs/Bismark_User_Guide.html"))
                    (scripts '("bismark"
                               "bismark_genome_preparation"
                               "bismark_methylation_extractor"
                               "bismark2bedGraph"
                               "bismark2report"
                               "coverage2cytosine"
                               "deduplicate_bismark"
                               "filter_non_conversion"
                               "bam2nuc"
                               "bismark2summary"
                               "NOMe_filtering")))
               (substitute* "bismark2report"
                 (("\\$RealBin/plotly")
                  (string-append share "/plotly")))
               (mkdir-p share)
               (mkdir-p docdir)
               (mkdir-p bin)
               (for-each (lambda (file) (install-file file bin))
                         scripts)
               (for-each (lambda (file) (install-file file docdir))
                         docs)
               (copy-recursively "Docs/Images" (string-append docdir "/Images"))
               (copy-recursively "plotly"
                                 (string-append share "/plotly"))

               ;; Fix references to gunzip
               (substitute* (map (lambda (file)
                                   (string-append bin "/" file))
                                 scripts)
                 (("\"gunzip -c")
                  (string-append "\"" (assoc-ref inputs "gzip")
                                 "/bin/gunzip -c")))
               #t))))))
    (inputs
     `(("gzip" ,gzip)
       ("perl-carp" ,perl-carp)
       ("perl-getopt-long" ,perl-getopt-long)))
    (native-inputs
     `(("plotly.js"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://raw.githubusercontent.com/plotly/plotly.js/"
                               "v1.39.4/dist/plotly.js"))
           (sha256
            (base32 "138mwsr4nf5qif4mrxx286mpnagxd1xwl6k8aidrjgknaqg88zyr"))))
       ("uglify-js" ,uglify-js)))
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
    (version "0.44.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pachterlab/kallisto.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nj382jiywqnpgvyhichajpkkh5r0bapn43f4dx40zdaq5v4m40m"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f          ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-use-bundled-htslib
           (lambda _
             (substitute* "CMakeLists.txt"
               (("^ExternalProject_Add" m)
                (string-append "if (NEVER)\n" m))
               (("^\\)")
                (string-append ")\nendif(NEVER)"))
               (("include_directories\\(\\$\\{htslib_PREFIX.*" m)
                (string-append "# " m)))
             (substitute* "src/CMakeLists.txt"
               (("target_link_libraries\\(kallisto kallisto_core pthread \
\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}/../ext/htslib/libhts.a\\)")
                "target_link_libraries(kallisto kallisto_core pthread hts)")
               (("include_directories\\(\\.\\./ext/htslib\\)") ""))
             #t)))))
    (inputs
     `(("hdf5" ,hdf5)
       ("htslib" ,htslib)
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
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Kingsford-Group/libgff.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n6vfjnq7a2mianipscbshrvbncss8z4zkgkbjw754p9043nfkps"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ; no tests included
    (home-page "https://github.com/Kingsford-Group/libgff")
    (synopsis "Parser library for reading/writing GFF files")
    (description "This is a simple \"libraryfication\" of the GFF/GTF parsing
code that is used in the Cufflinks codebase.  The goal of this library is to
provide this functionality without the necessity of drawing in a heavy-weight
dependency like SeqAn.")
    (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt"))))

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
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kingsfordgroup/sailfish.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1amcc5hqvsl42hg4x19bi9vy47cl874s0lw1fmi0hwsdk9i8c03v"))
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
               (("find_package\\(Boost 1\\.53\\.0") "#"))
             #t))
         (add-after 'unpack 'do-not-assign-to-macro
           (lambda _
             (substitute* "include/spdlog/details/format.cc"
               (("const unsigned CHAR_WIDTH = 1;") ""))
             #t))
         (add-after 'unpack 'prepare-rapmap
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((src "external/install/src/rapmap/")
                   (include "external/install/include/rapmap/")
                   (rapmap (assoc-ref inputs "rapmap")))
               (mkdir-p "/tmp/rapmap")
               (invoke "tar" "xf"
                       (assoc-ref inputs "rapmap")
                       "-C" "/tmp/rapmap"
                       "--strip-components=1")
               (mkdir-p src)
               (mkdir-p include)
               (for-each (lambda (file)
                           (install-file file src))
                         (find-files "/tmp/rapmap/src" "\\.(c|cpp)"))
               (copy-recursively "/tmp/rapmap/include" include))
             #t))
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
                                    "/include/eigen3"))
             #t)))))
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
                                                     "src/xxhash.c"))
                             #t))))
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
               (("find_package\\(Boost 1\\.53\\.0") "#"))
             #t))
         (add-after 'unpack 'do-not-phone-home
           (lambda _
             (substitute* "src/Salmon.cpp"
               (("getVersionMessage\\(\\)") "\"\""))
             #t))
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
                                       "external/install/src/rapmap/xxhash.c")))
             #t))
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
    (version "2.0.17")
    ;; The tarball on Pypi does not include the tests.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linnarsson-lab/loompy.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12a5kjgiikapv93wahfw0frszx1lblnppyz3vs5gy8fgmgngra07"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append (getcwd) ":"
                                    (getenv "PYTHONPATH")))
             (invoke "pytest" "tests")
             #t)))))
    (propagated-inputs
     `(("python-h5py" ,python-h5py)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-scipy" ,python-scipy)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/linnarsson-lab/loompy")
    (synopsis "Work with .loom files for single-cell RNA-seq data")
    (description "The loom file format is an efficient format for very large
omics datasets, consisting of a main matrix, optional additional layers, a
variable number of row and column annotations.  Loom also supports sparse
graphs.  This library makes it easy to work with @file{.loom} files for
single-cell RNA-seq data.")
    (license license:bsd-3)))

;; pigx-scrnaseq does not work with the latest version of loompy.
(define-public python-loompy-for-pigx-scrnaseq
  (package (inherit python-loompy)
    (name "python-loompy")
    (version "2.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linnarsson-lab/loompy.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pjyl532pl8sbv71yci6h0agchn0naw2qjcwj50n6afrsahbsag3"))))
    ;; There are none.
    (arguments '(#:tests? #f))))

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
           (delete-file-recursively "3rdParty")
           #t))))
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
         ;; FIXME: fails with "java.io.FileNotFoundException:
         ;; /gnu/store/…-dropseq-tools-1.13/share/java/lib/biojava-alignment.jar"
         (delete 'generate-jar-indices)
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
    (version "0.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_rnaseq/"
                                  "releases/download/v" version
                                  "/pigx_rnaseq-" version ".tar.gz"))
              (sha256
               (base32
                "05gn658zpj9xki5dbs728z9zxq1mcm25hkwr5vzwqxsfi15l5f2l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-tests? #f             ; not supported
       #:phases
       (modify-phases %standard-phases
         ;; "test.sh" runs STAR, which requires excessive amounts of memory.
         (add-after 'unpack 'disable-resource-intensive-test
           (lambda _
             (substitute* "Makefile.in"
               (("(^  tests/test_trim_galore/test.sh).*" _ m) m)
               (("^  tests/test_multiqc/test.sh") "")
               (("^  test.sh") ""))
             #t)))))
    (inputs
     `(("gzip" ,gzip)
       ("snakemake" ,snakemake)
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
    (version "0.0.40")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_chipseq/"
                                  "releases/download/v" version
                                  "/pigx_chipseq-" version ".tar.gz"))
              (sha256
               (base32
                "0y9x62cfwzhsp82imnawyamxp58bcb00yjxdy44spylqnjdlsaj8"))))
    (build-system gnu-build-system)
    ;; parts of the tests rely on access to the network
    (arguments '(#:tests? #f))
    (inputs
     `(("grep" ,grep)
       ("coreutils" ,coreutils)
       ("r-minimal" ,r-minimal)
       ("r-argparser" ,r-argparser)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-chipseq" ,r-chipseq)
       ("r-data-table" ,r-data-table)
       ("r-dplyr" ,r-dplyr)
       ("r-genomation" ,r-genomation)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-heatmaply" ,r-heatmaply)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-ggplot2" ,r-ggplot2)
       ("r-plotly" ,r-plotly)
       ("r-rmarkdown" ,r-rmarkdown)
       ("python-wrapper" ,python-wrapper)
       ("python-pyyaml" ,python-pyyaml)
       ("python-magic" ,python-magic)
       ("python-xlrd" ,python-xlrd)
       ("trim-galore" ,trim-galore)
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
    (version "0.0.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_bsseq/"
                                  "releases/download/v" version
                                  "/pigx_bsseq-" version ".tar.gz"))
              (sha256
               (base32
                "0l97wvkq4diq8lcarraj33bby1zzf0w804jwi8mlc5qddp8idwhy"))))
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
             #t)))))
    (native-inputs
     `(("tzdata" ,tzdata)))
    (inputs
     `(("coreutils" ,coreutils)
       ("sed" ,sed)
       ("grep" ,grep)
       ("r-minimal" ,r-minimal)
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

(define-public pigx-scrnaseq
  (package
    (name "pigx-scrnaseq")
    (version "0.0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_scrnaseq/"
                                  "releases/download/v" version
                                  "/pigx_scrnaseq-" version ".tar.gz"))
              (sha256
               (base32
                "131zarirv16w8653m0d66jgjnwqfsxqc0hix0rypssz4d83bl51j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "PICARDJAR=" (assoc-ref %build-inputs "java-picard")
			    "/share/java/picard.jar")
	     (string-append "DROPSEQJAR=" (assoc-ref %build-inputs "dropseq-tools")
			    "/share/java/dropseq.jar"))))
    (inputs
     `(("coreutils" ,coreutils)
       ("perl" ,perl)
       ("dropseq-tools" ,dropseq-tools)
       ("fastqc" ,fastqc)
       ("java-picard" ,java-picard-2.10.3) ; same as for dropseq
       ("java" ,icedtea-8)
       ("python-wrapper" ,python-wrapper)
       ("python-pyyaml" ,python-pyyaml)
       ("python-pandas" ,python-pandas)
       ("python-magic" ,python-magic)
       ("python-numpy" ,python-numpy)
       ("python-loompy" ,python-loompy-for-pigx-scrnaseq)
       ("ghc-pandoc" ,ghc-pandoc)
       ("ghc-pandoc-citeproc" ,ghc-pandoc-citeproc)
       ("samtools" ,samtools)
       ("snakemake" ,snakemake)
       ("star" ,star)
       ("r-minimal" ,r-minimal)
       ("r-argparser" ,r-argparser)
       ("r-cowplot" ,r-cowplot)
       ("r-data-table" ,r-data-table)
       ("r-delayedarray" ,r-delayedarray)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-dplyr" ,r-dplyr)
       ("r-dropbead" ,r-dropbead)
       ("r-dt" ,r-dt)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-hdf5array" ,r-hdf5array)
       ("r-pheatmap" ,r-pheatmap)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rtsne" ,r-rtsne)
       ("r-scater" ,r-scater)
       ("r-scran" ,r-scran)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-stringr" ,r-stringr)
       ("r-yaml" ,r-yaml)))
    (home-page "http://bioinformatics.mdc-berlin.de/pigx/")
    (synopsis "Analysis pipeline for single-cell RNA sequencing experiments")
    (description "PiGX scRNAseq is an analysis pipeline for preprocessing and
quality control for single cell RNA sequencing experiments.  The inputs are
read files from the sequencing experiment, and a configuration file which
describes the experiment.  It produces processed files for downstream analysis
and interactive quality reports.  The pipeline is designed to work with UMI
based methods.")
    (license license:gpl3+)))

(define-public pigx
  (package
    (name "pigx")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx/"
                                  "releases/download/v" version
                                  "/pigx-" version ".tar.gz"))
              (sha256
               (base32
                "1i5njdy1clj5ncw45d16p7mwmqvb1ilikl9n797pxklc3f4s7mq7"))))
    (build-system gnu-build-system)
    (inputs
     `(("python" ,python)
       ("pigx-bsseq" ,pigx-bsseq)
       ("pigx-chipseq" ,pigx-chipseq)
       ("pigx-rnaseq" ,pigx-rnaseq)
       ("pigx-scrnaseq" ,pigx-scrnaseq)))
    (home-page "http://bioinformatics.mdc-berlin.de/pigx/")
    (synopsis "Analysis pipelines for genomics")
    (description "PiGx is a collection of genomics pipelines.  It includes the
following pipelines:

@itemize
@item PiGx BSseq for raw fastq read data of bisulfite experiments
@item PiGx RNAseq for RNAseq samples
@item PiGx scRNAseq for single cell dropseq analysis
@item PiGx ChIPseq for reads from ChIPseq experiments
@end itemize

All pipelines are easily configured with a simple sample sheet and a
descriptive settings file.  The result is a set of comprehensive, interactive
HTML reports with interesting findings about your samples.")
    (license license:gpl3+)))

(define-public genrich
  (package
    (name "genrich")
    (version "0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jsh58/Genrich.git")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0x0q6z0208n3cxzqjla4rgjqpyqgwpmz27852lcvzkzaigymq4zp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "Genrich" (string-append (assoc-ref outputs "out") "/bin"))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/jsh58/Genrich")
    (synopsis "Detecting sites of genomic enrichment")
    (description "Genrich is a peak-caller for genomic enrichment
assays (e.g. ChIP-seq, ATAC-seq).  It analyzes alignment files generated
following the assay and produces a file detailing peaks of significant
enrichment.")
    (license license:expat)))

(define-public mantis
  (let ((commit "4ffd171632c2cb0056a86d709dfd2bf21bc69b84")
        (revision "1"))
    (package
      (name "mantis")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/splatlab/mantis.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0iqbr0dhmlc8mzpirmm2s4pkzkwdgrcx50yx6cv3wlr2qi064p55"))))
      (build-system cmake-build-system)
      (arguments '(#:tests? #f)) ; there are none
      (inputs
       `(("sdsl-lite" ,sdsl-lite)
         ("openssl" ,openssl)
         ("zlib" ,zlib)))
      (home-page "https://github.com/splatlab/mantis")
      (synopsis "Large-scale sequence-search index data structure")
      (description "Mantis is a space-efficient data structure that can be
used to index thousands of raw-read genomics experiments and facilitate
large-scale sequence searches on those experiments.  Mantis uses counting
quotient filters instead of Bloom filters, enabling rapid index builds and
queries, small indexes, and exact results, i.e., no false positives or
negatives.  Furthermore, Mantis is also a colored de Bruijn graph
representation, so it supports fast graph traversal and other topological
analyses in addition to large-scale sequence-level searches.")
      ;; uses __uint128_t and inline assembly
      (supported-systems '("x86_64-linux"))
      (license license:bsd-3))))

(define-public r-diversitree
  (package
    (name "r-diversitree")
    (version "0.9-11")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "diversitree" version))
        (sha256
         (base32
          "1jqfjmmaigq581l4zxysmkhld0xv6izlbr1hihf9zplkix36majc"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (inputs `(("fftw" ,fftw) ("gsl" ,gsl)))
    (propagated-inputs
     `(("r-ape" ,r-ape)
       ("r-desolve" ,r-desolve)
       ("r-rcpp" ,r-rcpp)
       ("r-subplex" ,r-subplex)))
    (home-page "https://www.zoology.ubc.ca/prog/diversitree")
    (synopsis "Comparative 'phylogenetic' analyses of diversification")
    (description "This package contains a number of comparative \"phylogenetic\"
methods, mostly focusing on analysing diversification and character evolution.
Contains implementations of \"BiSSE\" (Binary State Speciation and Extinction)
and its unresolved tree extensions, \"MuSSE\" (Multiple State Speciation and
Extinction), \"QuaSSE\", \"GeoSSE\", and \"BiSSE-ness\" Other included methods
include Markov models of discrete and continuous trait evolution and constant
rate speciation and extinction.")
    (license license:gpl2+)))

(define-public sjcount
  ;; There is no tag for version 3.2, nor is there a release archive.
  (let ((commit "292d3917cadb3f6834c81e509c30e61cd7ead6e5")
        (revision "1"))
    (package
      (name "sjcount")
      (version (git-version "3.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pervouchine/sjcount-full.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0gdgj35j249f04rqgq8ymcc1xg1vi9kzbajnjqpaq2wpbh8bl234"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; requires a 1.4G test file
         #:make-flags
         (list (string-append "SAMTOOLS_DIR="
                              (assoc-ref %build-inputs "samtools")
                              "/lib/"))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "makefile"
                 (("-I \\$\\{SAMTOOLS_DIR\\}")
                  (string-append "-I" (assoc-ref inputs "samtools")
                                 "/include/samtools"))
                 (("-lz ") "-lz -lpthread "))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (for-each (lambda (tool)
                           (install-file tool
                                         (string-append (assoc-ref outputs "out")
                                                        "/bin")))
                         '("j_count" "b_count" "sjcount"))
               #t)))))
      (inputs
       `(("samtools" ,samtools-0.1)
         ("zlib" ,zlib)))
      (home-page "https://github.com/pervouchine/sjcount-full/")
      (synopsis "Annotation-agnostic splice junction counting pipeline")
      (description "Sjcount is a utility for fast quantification of splice
junctions in RNA-seq data.  It is annotation-agnostic and offset-aware.  This
version does count multisplits.")
      (license license:gpl3+))))

(define-public minimap2
  (package
    (name "minimap2")
    (version "2.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lh3/minimap2/"
                           "releases/download/v" version "/"
                           "minimap2-" version ".tar.bz2"))
       (sha256
        (base32
         "080w9066irkbhbyr4nmf19pzkdd2s4v31hpzlajgq2y0drr6zcsj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are none
       #:make-flags
       (list "CC=gcc"
             (let ((system ,(or (%current-target-system)
                                (%current-system))))
               (cond
                ((string-prefix? "x86_64" system)
                 "all")
                ((or (string-prefix? "armhf" system)
                     (string-prefix? "aarch64" system))
                 "arm_neon=1")
                (_ "sse2only=1"))))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "minimap2" bin)
               (mkdir-p man)
               (install-file "minimap2.1" man))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://lh3.github.io/minimap2/")
    (synopsis "Pairwise aligner for genomic and spliced nucleotide sequences")
    (description "Minimap2 is a versatile sequence alignment program that
aligns DNA or mRNA sequences against a large reference database.  Typical use
cases include:

@enumerate
@item mapping PacBio or Oxford Nanopore genomic reads to the human genome;
@item finding overlaps between long reads with error rate up to ~15%;
@item splice-aware alignment of PacBio Iso-Seq or Nanopore cDNA or Direct RNA
  reads against a reference genome;
@item aligning Illumina single- or paired-end reads;
@item assembly-to-assembly alignment;
@item full-genome alignment between two closely related species with
  divergence below ~15%.
@end enumerate\n")
    (license license:expat)))

(define-public r-circus
  (package
    (name "r-circus")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BIMSBbioinfo/ciRcus.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jhjn3ilb057hbf6yzrihj13ifxxs32y7nkby8l3lkm28dg4p97h"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationhub" ,r-annotationhub)
       ("r-biomart" ,r-biomart)
       ("r-data-table" ,r-data-table)
       ("r-dbi" ,r-dbi)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-hash" ,r-hash)
       ("r-iranges" ,r-iranges)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rmysql" ,r-rmysql)
       ("r-s4vectors" ,r-s4vectors)
       ("r-stringr" ,r-stringr)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/BIMSBbioinfo/ciRcus")
    (synopsis "Annotation, analysis and visualization of circRNA data")
    (description "Circus is an R package for annotation, analysis and
visualization of circRNA data.  Users can annotate their circRNA candidates
with host genes, gene featrues they are spliced from, and discriminate between
known and yet unknown splice junctions.  Circular-to-linear ratios of circRNAs
can be calculated, and a number of descriptive plots easily generated.")
    (license license:artistic2.0)))

(define-public r-loomr
  (let ((commit "df0144bd2bbceca6fadef9edc1bbc5ca672d4739")
        (revision "1"))
    (package
      (name "r-loomr")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mojaveazure/loomR.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1b1g4dlmfdyhn56bz1mkh9ymirri43wiz7rjhs7py3y7bdw1s3yr"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-r6" ,r-r6)
         ("r-hdf5r" ,r-hdf5r)
         ("r-iterators" ,r-iterators)
         ("r-itertools" ,r-itertools)
         ("r-matrix" ,r-matrix)))
      (home-page "https://github.com/mojaveazure/loomR")
      (synopsis "R interface for loom files")
      (description "This package provides an R interface to access, create,
and modify loom files.  loomR aims to be completely compatible with loompy.")
      (license license:gpl3))))

(define-public gffread
  ;; We cannot use the tagged release because it is not in sync with gclib.
  ;; See https://github.com/gpertea/gffread/issues/26
  (let ((commit "ba7535fcb3cea55a6e5a491d916e93b454e87fd0")
        (revision "1"))
    (package
      (name "gffread")
      (version (git-version "0.9.12" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gpertea/gffread.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1dl2nbcg96lxpd0drg48ssa8343nf7pw9s9mkrc4mjjmfwsin3ki"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no check target
         #:make-flags
         (list "GCLDIR=gclib")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'copy-gclib-source
             (lambda* (#:key inputs #:allow-other-keys)
               (mkdir-p "gclib")
               (copy-recursively (assoc-ref inputs "gclib-source") "gclib")
               #t))
           ;; There is no install target
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "gffread" bin))
               #t)))))
      (native-inputs
       `(("gclib-source"
          ,(let ((version "0.10.3")
                 (commit "54917d0849c1e83cfb057b5f712e5cb6a35d948f")
                 (revision "1"))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/gpertea/gclib.git")
                     (commit commit)))
               (file-name (git-file-name "gclib" version))
               (sha256
                (base32
                 "0b51lc0b8syrv7186fd7n8f15rwnf264qgfmm2palrwks1px24mr")))))))
      (home-page "https://github.com/gpertea/gffread/")
      (synopsis "Parse and convert GFF/GTF files")
      (description
       "This package provides a GFF/GTF file parsing utility providing format
conversions, region filtering, FASTA sequence extraction and more.")
      ;; gffread is under Expat, but gclib is under Artistic 2.0
      (license (list license:expat
                     license:artistic2.0)))))

(define-public find-circ
  ;; The last release was in 2015.  The license was clarified in 2017, so we
  ;; take the latest commit.
  (let ((commit "8655dca54970fcf7e92e22fbf57e1188724dda7d")
        (revision "1"))
    (package
      (name "find-circ")
      (version (git-version "1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/marvin-jens/find_circ.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0p77pbqbclqr4srms34y1b9b4njybfpjiknc11ki84f3p8skb3cg"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; there are none
         #:phases
         ;; There is no actual build system.
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (path (getenv "PYTHONPATH")))
                 (for-each (lambda (script)
                             (install-file script bin)
                             (wrap-program (string-append bin "/" script)
                               `("PYTHONPATH" ":" prefix (,path))))
                           '("cmp_bed.py"
                             "find_circ.py"
                             "maxlength.py"
                             "merge_bed.py"
                             "unmapped2anchors.py")))
               #t)))))
      (inputs
       `(("python2" ,python-2)
         ("python2-pysam" ,python2-pysam)
         ("python2-numpy" ,python2-numpy)))
      (home-page "https://github.com/marvin-jens/find_circ")
      (synopsis "circRNA detection from RNA-seq reads")
      (description "This package provides tools to detect head-to-tail
spliced (back-spliced) sequencing reads, indicative of circular RNA (circRNA)
in RNA-seq data.")
      (license license:gpl3))))

(define-public python-scanpy
  (package
    (name "python-scanpy")
    (version "1.4")
    ;; Fetch from git because the pypi tarball does not include tests.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/theislab/scanpy.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0zn6x6c0cnm1a20i6isigwb51g3pr9zpjk8r1minjqnxi5yc9pm4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; These tests require Internet access.
             (delete-file-recursively "scanpy/tests/notebooks")
             (delete-file "scanpy/tests/test_clustering.py")

             ;; TODO: I can't get the plotting tests to work, even with Xvfb.
             (delete-file "scanpy/tests/test_plotting.py")
             (delete-file "scanpy/tests/test_preprocessing.py")
             (delete-file "scanpy/tests/test_read_10x.py")

             (setenv "PYTHONPATH"
                     (string-append (getcwd) ":"
                                    (getenv "PYTHONPATH")))
             (invoke "pytest")
             #t)))))
    (propagated-inputs
     `(("python-anndata" ,python-anndata)
       ("python-h5py" ,python-h5py)
       ("python-igraph" ,python-igraph)
       ("python-joblib" ,python-joblib)
       ("python-louvain" ,python-louvain)
       ("python-matplotlib" ,python-matplotlib)
       ("python-natsort" ,python-natsort)
       ("python-networkx" ,python-networkx)
       ("python-numba" ,python-numba)
       ("python-pandas" ,python-pandas)
       ("python-scikit-learn" ,python-scikit-learn)
       ("python-scipy" ,python-scipy)
       ("python-seaborn" ,python-seaborn)
       ("python-statsmodels" ,python-statsmodels)
       ("python-tables" ,python-tables)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/theislab/scanpy")
    (synopsis "Single-Cell Analysis in Python.")
    (description "Scanpy is a scalable toolkit for analyzing single-cell gene
expression data.  It includes preprocessing, visualization, clustering,
pseudotime and trajectory inference and differential expression testing.  The
Python-based implementation efficiently deals with datasets of more than one
million cells.")
    (license license:bsd-3)))

(define-public python-bbknn
  (package
    (name "python-bbknn")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bbknn" version))
       (sha256
        (base32
         "1qgdganvj3lyxj84v7alm23b9vqhwpn8z0115qndpnpy90qxynwz"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-annoy" ,python-annoy)
       ("python-cython" ,python-cython)
       ("python-faiss" ,python-faiss)
       ("python-numpy" ,python-numpy)
       ("python-scanpy" ,python-scanpy)))
    (home-page "https://github.com/Teichlab/bbknn")
    (synopsis "Batch balanced KNN")
    (description "BBKNN is a batch effect removal tool that can be directly
used in the Scanpy workflow.  It serves as an alternative to
@code{scanpy.api.pp.neighbors()}, with both functions creating a neighbour
graph for subsequent use in clustering, pseudotime and UMAP visualisation.  If
technical artifacts are present in the data, they will make it challenging to
link corresponding cell types across different batches.  BBKNN actively
combats this effect by splitting your data into batches and finding a smaller
number of neighbours for each cell within each of the groups.  This helps
create connections between analogous cells in different batches without
altering the counts or PCA space.")
    (license license:expat)))

(define-public gffcompare
  (let ((commit "be56ef4349ea3966c12c6397f85e49e047361c41")
        (revision "1"))
    (package
      (name "gffcompare")
      (version (git-version "0.10.15" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gpertea/gffcompare/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0cp5qpxdhw4mxpya5dld8wi3jk00zyklm6rcri426wydinrnfmkg"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no check target
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'copy-gclib-source
             (lambda* (#:key inputs #:allow-other-keys)
               (mkdir "../gclib")
               (copy-recursively
                (assoc-ref inputs "gclib-source") "../gclib")
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "gffcompare" bin)
                 #t))))))
      (native-inputs
       `(("gclib-source" ; see 'README.md' of gffcompare
          ,(let ((commit "54917d0849c1e83cfb057b5f712e5cb6a35d948f")
                 (revision "1")
                 (name "gclib")
                 (version (git-version "0.10.3" revision commit)))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/gpertea/gclib/")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32 "0b51lc0b8syrv7186fd7n8f15rwnf264qgfmm2palrwks1px24mr")))))))
      (home-page "https://github.com/gpertea/gffcompare/")
      (synopsis "Tool for comparing or classifing transcripts of RNA-Seq")
      (description
       "@code{gffcompare} is a tool that can:
@enumerate
@item compare and evaluate the accuracy of RNA-Seq transcript assemblers
(Cufflinks, Stringtie);
@item collapse (merge) duplicate transcripts from multiple GTF/GFF3 files (e.g.
resulted from assembly of different samples);
@item classify transcripts from one or multiple GTF/GFF3 files as they relate to
reference transcripts provided in a annotation file (also in GTF/GFF3 format).
@end enumerate")
      (license
       (list
        license:expat                   ;license for gffcompare
        license:artistic2.0)))))        ;license for gclib

(define-public python-intervaltree
  (package
    (name "python-intervaltree")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "intervaltree" version))
       (sha256
        (base32
         "02w191m9zxkcjqr1kv2slxvhymwhj3jnsyy3a28b837pi15q19dc"))))
    (build-system python-build-system)
    ;; FIXME: error when collecting tests
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-sortedcontainers" ,python-sortedcontainers)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/chaimleib/intervaltree")
    (synopsis "Editable interval tree data structure")
    (description
     "This package provides a mutable, self-balancing interval tree
implementation for Python.  Queries may be by point, by range overlap, or by
range envelopment.  This library was designed to allow tagging text and time
intervals, where the intervals include the lower bound but not the upper
bound.")
    (license license:asl2.0)))

(define-public python-pypairix
  (package
    (name "python-pypairix")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pypairix" version))
       (sha256
        (base32
         "0zs92b74s5v4xy2h16s15f3z6l4nnbw8x8zyif7xx5xpafjn0xss"))))
    (build-system python-build-system)
    ;; FIXME: the tests fail because test.support cannot be loaded:
    ;; ImportError: cannot import name 'support'
    (arguments '(#:tests? #f))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/4dn-dcic/pairix")
    (synopsis "Support for querying pairix-indexed bgzipped text files")
    (description
     "Pypairix is a Python module for fast querying on a pairix-indexed
bgzipped text file that contains a pair of genomic coordinates per line.")
    (license license:expat)))

(define-public python-pyfaidx
  (package
    (name "python-pyfaidx")
    (version "0.5.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyfaidx" version))
       (sha256
        (base32
         "0y5zyjksj1rdglj601xd2bbni5abhdh622y3ck76chyzxz9z4rx8"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-six" ,python-six)))
    (home-page "http://mattshirley.com")
    (synopsis "Random access to fasta subsequences")
    (description
     "This package provides procedures for efficient pythonic random access to
fasta subsequences.")
    (license license:bsd-3)))

(define-public python-cooler
  (package
    (name "python-cooler")
    (version "0.7.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cooler" version))
       (sha256
        (base32
         "08k5nxnxa6qsbk15z5z0q01n28042k87wi4905hh95rzqib15mhx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-biopython" ,python-biopython)
       ("python-click" ,python-click)
       ("python-cytoolz" ,python-cytoolz)
       ("python-dask" ,python-dask)
       ("python-h5py" ,python-h5py)
       ("python-multiprocess" ,python-multiprocess)
       ("python-pandas" ,python-pandas)
       ("python-pyfaidx" ,python-pyfaidx)
       ("python-pypairix" ,python-pypairix)
       ("python-pysam" ,python-pysam)
       ("python-scipy" ,python-scipy)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nose" ,python-nose)
       ("python-numpydoc" ,python-numpydoc)
       ("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/mirnylab/cooler")
    (synopsis "Sparse binary format for genomic interaction matrices")
    (description
     "Cooler is a support library for a sparse, compressed, binary persistent
storage format, called @code{cool}, used to store genomic interaction data,
such as Hi-C contact matrices.")
    (license license:bsd-3)))

(define-public python-hicexplorer
  (package
    (name "python-hicexplorer")
    (version "2.1.4")
    (source
     (origin
       ;; The latest version is not available on Pypi.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deeptools/HiCExplorer.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0q5gpbzmrkvygqgw524q36b4nrivcmyi5v194vsx0qw7b3gcmq08"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-up-requirements
           (lambda _
             (substitute* "setup.py"
               (("==") ">="))
             #t)))))
    (propagated-inputs
     `(("python-biopython" ,python-biopython)
       ("python-configparser" ,python-configparser)
       ("python-cooler" ,python-cooler)
       ("python-future" ,python-future)
       ("python-intervaltree" ,python-intervaltree)
       ("python-jinja2" ,python-jinja2)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-pybigwig" ,python-pybigwig)
       ("python-pysam" ,python-pysam)
       ("python-scipy" ,python-scipy)
       ("python-six" ,python-six)
       ("python-tables" ,python-tables)
       ("python-unidecode" ,python-unidecode)))
    (home-page "http://hicexplorer.readthedocs.io")
    (synopsis "Process, analyze and visualize Hi-C data")
    (description
     "HiCExplorer is a powerful and easy to use set of tools to process,
normalize and visualize Hi-C data.  HiCExplorer facilitates the creation of
contact matrices, correction of contacts, TAD detection, A/B compartments,
merging, reordering or chromosomes, conversion from different formats
including cooler and detection of long-range contacts.  Moreover, it allows
the visualization of multiple contact matrices along with other types of data
like genes, compartments, ChIP-seq coverage tracks (and in general any type of
genomic scores), long range contacts and the visualization of viewpoints.")
    (license license:gpl3)))

(define-public python-pygenometracks
  (package
    (name "python-pygenometracks")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyGenomeTracks" version))
       (sha256
        (base32
         "1fws6bqsyy9kj3qiabhkqx4wd4i775gsxnhszqd3zg7w67sc1ic5"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-configparser" ,python-configparser)
       ("python-future" ,python-future)
       ("python-hicexplorer" ,python-hicexplorer)
       ("python-intervaltree" ,python-intervaltree)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-pybigwig" ,python-pybigwig)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://pygenometracks.readthedocs.io")
    (synopsis "Program and library to plot beautiful genome browser tracks")
    (description
     "This package aims to produce high-quality genome browser tracks that
are highly customizable.  Currently, it is possible to plot: bigwig, bed (many
options), bedgraph, links (represented as arcs), and Hi-C matrices.
pyGenomeTracks can make plots with or without Hi-C data.")
    (license license:gpl3+)))

(define-public python-hic2cool
  (package
    (name "python-hic2cool")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hic2cool" version))
       (sha256
        (base32
         "0xy6mhfns2lzib1kcr6419jjp6pmh0qx8z8na55lmiwn0ds8q9cl"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; no tests included
    (propagated-inputs
     `(("python-cooler" ,python-cooler)))
    (home-page "https://github.com/4dn-dcic/hic2cool")
    (synopsis "Converter for .hic and .cool files")
    (description
     "This package provides a converter between @code{.hic} files (from
juicer) and single-resolution or multi-resolution @code{.cool} files (for
cooler).  Both @code{hic} and @code{cool} files describe Hi-C contact
matrices.")
    (license license:expat)))

(define-public r-pore
  (package
    (name "r-pore")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/rpore/" version
                       "/poRe_" version ".tar.gz"))
       (sha256
        (base32 "0pih9nljbv8g4x8rkk29i7aqq681b782r5s5ynp4nw9yzqnmmksv"))))
    (properties `((upstream-name . "poRe")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bit64" ,r-bit64)
       ("r-data-table" ,r-data-table)
       ("r-rhdf5" ,r-rhdf5)
       ("r-shiny" ,r-shiny)
       ("r-svdialogs" ,r-svdialogs)))
    (home-page "https://sourceforge.net/projects/rpore/")
    (synopsis "Visualize Nanopore sequencing data")
    (description
     "This package provides graphical user interfaces to organize and visualize Nanopore
sequencing data.")
    ;; This is free software but the license variant is unclear:
    ;; <https://github.com/mw55309/poRe_docs/issues/10>.
    (license license:bsd-3)))

(define-public r-xbioc
  (let ((revision "1")
        (commit "f798c187e376fd1ba27abd559f47bbae7e3e466b"))
    (package
      (name "r-xbioc")
      (version (git-version "0.1.15" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/renozao/xbioc.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "03hffh2f6z71y6l6dqpa5cql3hdaw7zigdi8sm2dzgx379k9rgrr"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-annotationdbi" ,r-annotationdbi)
         ("r-assertthat" ,r-assertthat)
         ("r-biobase" ,r-biobase)
         ("r-biocinstaller" ,r-biocinstaller)
         ("r-digest" ,r-digest)
         ("r-pkgmaker" ,r-pkgmaker)
         ("r-plyr" ,r-plyr)
         ("r-reshape2" ,r-reshape2)
         ("r-stringr" ,r-stringr)))
      (home-page "https://github.com/renozao/xbioc/")
      (synopsis "Extra base functions for Bioconductor")
      (description "This package provides extra utility functions to perform
common tasks in the analysis of omics data, leveraging and enhancing features
provided by Bioconductor packages.")
      (license license:gpl3+))))

(define-public r-cssam
  (let ((revision "1")
        (commit "9ec58c982fa551af0d80b1a266890d92954833f2"))
    (package
      (name "r-cssam")
      (version (git-version "1.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/shenorrLab/csSAM.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "128syf9v39gk0z3ip000qpsjbg6l1siyq6c8b0hz41dzg5achyb3"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-formula" ,r-formula)
         ("r-ggplot2" ,r-ggplot2)
         ("r-pkgmaker" ,r-pkgmaker)
         ("r-plyr" ,r-plyr)
         ("r-rngtools" ,r-rngtools)
         ("r-scales" ,r-scales)))
      (home-page "https://github.com/shenorrLab/csSAM/")
      (synopsis "Cell type-specific statistical analysis of microarray")
      (description "This package implements the method csSAM that computes
cell-specific differential expression from measured cell proportions using
SAM.")
      ;; Any version
      (license license:lgpl2.1+))))

(define-public r-bseqsc
  (let ((revision "1")
        (commit "fef3f3e38dcf3df37103348b5780937982b43b98"))
    (package
      (name "r-bseqsc")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/shenorrLab/bseqsc.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1prw13wa20f7wlc3gkkls66n1kxz8d28qrb8icfqdwdnnv8w5qg8"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-abind" ,r-abind)
         ("r-annotationdbi" ,r-annotationdbi)
         ("r-biobase" ,r-biobase)
         ("r-cssam" ,r-cssam)
         ("r-dplyr" ,r-dplyr)
         ("r-e1071" ,r-e1071)
         ("r-edger" ,r-edger)
         ("r-ggplot2" ,r-ggplot2)
         ("r-nmf" ,r-nmf)
         ("r-openxlsx" ,r-openxlsx)
         ("r-pkgmaker" ,r-pkgmaker)
         ("r-plyr" ,r-plyr)
         ("r-preprocesscore" ,r-preprocesscore)
         ("r-rngtools" ,r-rngtools)
         ("r-scales" ,r-scales)
         ("r-stringr" ,r-stringr)
         ("r-xbioc" ,r-xbioc)))
      (home-page "https://github.com/shenorrLab/bseqsc")
      (synopsis "Deconvolution of bulk sequencing experiments using single cell data")
      (description "BSeq-sc is a bioinformatics analysis pipeline that
leverages single-cell sequencing data to estimate cell type proportion and
cell type-specific gene expression differences from RNA-seq data from bulk
tissue samples.  This is a companion package to the publication \"A
single-cell transcriptomic map of the human and mouse pancreas reveals inter-
and intra-cell population structure.\" Baron et al. Cell Systems (2016)
@url{https://www.ncbi.nlm.nih.gov/pubmed/27667365}.")
      (license license:gpl2+))))

(define-public porechop
  ;; The recommended way to install is to clone the git repository
  ;; https://github.com/rrwick/Porechop#installation
  (let ((commit "289d5dca4a5fc327f97b3f8cecb68ecaf1014861")
        (revision "1"))
    (package
      (name "porechop")
      (version (git-version "0.2.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rrwick/Porechop.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "05ps43gig0d3ia9x5lj84lb00hbsl6ba9n7y7jz927npxbr2ym23"))))
      (build-system python-build-system)
      (home-page "https://github.com/rrwick/porechop")
      (synopsis "Finding, trimming or splitting adapters, in Oxford Nanopore reads")
      (description
       "The porechop package is a tool for finding and removing adapters from Oxford
Nanopore reads.  Adapters on the ends of reads are trimmed off, and when a read
has an adapter in its middle, it is treated as chimeric and chopped into
separate reads.  Porechop performs thorough alignments to effectively find
adapters, even at low sequence identity.  Porechop also supports demultiplexing
of Nanopore reads that were barcoded with the Native Barcoding Kit, PCR
Barcoding Kit or Rapid Barcoding Kit.")
      (license license:gpl3+))))

(define-public poretools
  ;; The latest release was in 2016 and the latest commit is from 2017
  ;; the recommended way to install is to clone the git repository
  ;; https://poretools.readthedocs.io/en/latest/content/installation.html
  (let ((commit "e426b1f09e86ac259a00c261c79df91510777407")
        (revision "1"))
    (package
      (name "poretools")
      (version (git-version "0.6.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arq5x/poretools.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bglj833wxpp3cq430p1d3xp085ls221js2y90w7ir2x5ay8l7am"))))
      (build-system python-build-system)
      ;; requires python >=2.7, <3.0, and the same for python dependencies
      (arguments `(#:python ,python-2))
      (inputs
       `(("hdf5" ,hdf5)))
      (propagated-inputs
       `(("python-dateutil" ,python2-dateutil)
         ("python-h5py" ,python2-h5py)
         ("python-matplotlib" ,python2-matplotlib)
         ("python-pandas" ,python2-pandas)
         ("python-seaborn" ,python2-seaborn)))
      (home-page "https://poretools.readthedocs.io")
      (synopsis "Toolkit for working with nanopore sequencing data")
      (description
       "The MinION from Oxford Nanopore Technologies is a nanopore sequencer.
This @code{poretools} package is a flexible toolkit for exploring datasets
generated by nanopore sequencing devices for the purposes of quality control and
downstream analysis.  Poretools operates directly on the native FAST5, a variant
of the Hierarchical Data Format (HDF5) standard.")
      (license license:expat))))

(define-public r-absfiltergsea
  (package
    (name "r-absfiltergsea")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "AbsFilterGSEA" version))
       (sha256
        (base32 "15srxkxsvn38kd5frdrwfdf0ad8gskrd0h01wmdf9hglq8fjrp7w"))))
    (properties `((upstream-name . "AbsFilterGSEA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-deseq" ,r-deseq)
       ("r-limma" ,r-limma)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)))
    (home-page "https://cran.r-project.org/web/packages/AbsFilterGSEA/")
    (synopsis "Improved false positive control of gene-permuting with absolute filtering")
    (description
     "This package provides a function that performs gene-permuting of a gene-set
enrichment analysis (GSEA) calculation with or without the absolute filtering.
  Without filtering, users can perform (original) two-tailed or one-tailed
absolute GSEA.")
    (license license:gpl2)))

(define-public jamm
  (package
    (name "jamm")
    (version "1.0.7.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mahmoudibrahim/JAMM.git")
             (commit (string-append "JAMMv" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ls889jcma1ch9h21jjhnkadgszgqj41842hhcjh6cg88f85qf3i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libexec (string-append out "/libexec/jamm"))
                    (bin (string-append out "/bin")))
               (substitute* '("JAMM.sh"
                              "SignalGenerator.sh")
                 (("^sPath=.*")
                  (string-append "sPath=\"" libexec "\"\n")))
               (for-each (lambda (file)
                           (install-file file libexec))
                         (list "bincalculator.r"
                               "peakfinder.r"
                               "peakhelper.r"
                               "signalmaker.r"
                               "xcorr.r"
                               "xcorrhelper.r"
                               ;; Perl scripts
                               "peakfilter.pl"
                               "readshifter.pl"))

               (for-each
                (lambda (script)
                  (chmod script #o555)
                  (install-file script bin)
                  (wrap-program (string-append bin "/" script)
                    `("PATH" ":" prefix
                      (,(string-append (assoc-ref inputs "coreutils") "/bin")
                       ,(string-append (assoc-ref inputs "gawk") "/bin")
                       ,(string-append (assoc-ref inputs "perl") "/bin")
                       ,(string-append (assoc-ref inputs "r-minimal") "/bin")))
                    `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB")))
                    `("R_LIBS_SITE" ":" prefix (,(getenv "R_LIBS_SITE")))))
                (list "JAMM.sh" "SignalGenerator.sh")))
             #t)))))
    (inputs
     `(("bash" ,bash)
       ("coreutils" ,coreutils)
       ("gawk" ,gawk)
       ("perl" ,perl)
       ("r-minimal" ,r-minimal)
       ;;("r-parallel" ,r-parallel)
       ("r-signal" ,r-signal)
       ("r-mclust" ,r-mclust)))
    (home-page "https://github.com/mahmoudibrahim/JAMM")
    (synopsis "Peak finder for NGS datasets")
    (description
     "JAMM is a peak finder for next generation sequencing datasets (ChIP-Seq,
ATAC-Seq, DNase-Seq, etc.) that can integrate replicates and assign peak
boundaries accurately.  JAMM is applicable to both broad and narrow
datasets.")
    (license license:gpl3+)))

(define-public ngless
  (package
    (name "ngless")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/ngless/ngless.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mc2gi7h4lx74zylvyp76mvc0w6706j858ii9vlgzqsw6acpr117"))))
    (build-system haskell-build-system)
    (arguments
     `(#:haddock? #f ; The haddock phase fails with: NGLess/CmdArgs.hs:20:1:
                     ; error: parse error on input import
                     ; import Options.Applicative
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-cabal-file
           (lambda _ (invoke "hpack") #t))
         ;; These tools are expected to be installed alongside ngless.
         (add-after 'install 'link-tools
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (symlink (string-append (assoc-ref inputs "prodigal")
                                       "/bin/prodigal")
                        (string-append bin "ngless-" ,version "-prodigal"))
               (symlink (string-append (assoc-ref inputs "minimap2")
                                       "/bin/minimap2")
                        (string-append bin "ngless-" ,version "-minimap2"))
               (symlink (string-append (assoc-ref inputs "samtools")
                                       "/bin/samtools")
                        (string-append bin "ngless-" ,version "-samtools"))
               (symlink (string-append (assoc-ref inputs "bwa")
                                       "/bin/bwa")
                        (string-append bin "ngless-" ,version "-bwa"))
               #t))))))
    (inputs
     `(("prodigal" ,prodigal)
       ("bwa" ,bwa)
       ("samtools" ,samtools)
       ("minimap2" ,minimap2)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-async" ,ghc-async)
       ("ghc-atomic-write" ,ghc-atomic-write)
       ("ghc-bytestring-lexing" ,ghc-bytestring-lexing)
       ("ghc-chart" ,ghc-chart)
       ("ghc-chart-cairo" ,ghc-chart-cairo)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-algorithms" ,ghc-conduit-algorithms)
       ("ghc-conduit-combinators" ,ghc-conduit-combinators)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-configurator" ,ghc-configurator)
       ("ghc-convertible" ,ghc-convertible)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-double-conversion" ,ghc-double-conversion)
       ("ghc-edit-distance" ,ghc-edit-distance)
       ("ghc-either" ,ghc-either)
       ("ghc-errors" ,ghc-errors)
       ("ghc-extra" ,ghc-extra)
       ("ghc-filemanip" ,ghc-filemanip)
       ("ghc-file-embed" ,ghc-file-embed)
       ("ghc-gitrev" ,ghc-gitrev)
       ("ghc-hashtables" ,ghc-hashtables)
       ("ghc-http-conduit" ,ghc-http-conduit)
       ("ghc-inline-c" ,ghc-inline-c)
       ("ghc-inline-c-cpp" ,ghc-inline-c-cpp)
       ("ghc-intervalmap" ,ghc-intervalmap)
       ("ghc-missingh" ,ghc-missingh)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-regex" ,ghc-regex)
       ("ghc-safe" ,ghc-safe)
       ("ghc-safeio" ,ghc-safeio)
       ("ghc-strict" ,ghc-strict)
       ("ghc-tar" ,ghc-tar)
       ("ghc-text" ,ghc-text)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-unliftio-core" ,ghc-unliftio-core)
       ("ghc-vector" ,ghc-vector)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-zlib" ,ghc-zlib)))
    (propagated-inputs
     `(("r-r6" ,r-r6)
       ("r-hdf5r" ,r-hdf5r)
       ("r-iterators" ,r-iterators)
       ("r-itertools" ,r-itertools)
       ("r-matrix" ,r-matrix)))
    (native-inputs
     `(("ghc-hpack" ,ghc-hpack)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit",ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-test-framework-th" ,ghc-test-framework-th)))
    (home-page "https://gitlab.com/ngless/ngless")
    (synopsis "DSL for processing next-generation sequencing data")
    (description "Ngless is a domain-specific language for
@dfn{next-generation sequencing} (NGS) data processing.")
    (license license:expat)))

(define-public filtlong
  ;; The recommended way to install is to clone the git repository
  ;; https://github.com/rrwick/Filtlong#installation
  ;; and the lastest release is more than nine months old
  (let ((commit "d1bb46dfe8bc7efe6257b5ce222c04bfe8aedaab")
        (revision "1"))
    (package
      (name "filtlong")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rrwick/Filtlong.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1xr92r820x8qlkcr3b57iw223yq8vjgyi42jr79w2xgw47qzr575"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no check target
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (scripts (string-append out "/share/filtlong/scripts")))
                 (install-file "bin/filtlong" bin)
                 (install-file "scripts/histogram.py" scripts)
                 (install-file "scripts/read_info_histograms.sh" scripts))
               #t))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (path (getenv "PYTHONPATH")))
                 (wrap-program (string-append out
                                              "/share/filtlong/scripts/histogram.py")
                   `("PYTHONPATH" ":" prefix (,path))))
               #t))
           (add-before 'check 'patch-tests
             (lambda _
               (substitute* "scripts/read_info_histograms.sh"
                 (("awk") (which "gawk")))
               #t)))))
      (inputs
       `(("gawk" ,gawk)                 ;for read_info_histograms.sh
         ("python" ,python-2)           ;required for histogram.py
         ("zlib" ,zlib)))
      (home-page "https://github.com/rrwick/Filtlong/")
      (synopsis "Tool for quality filtering of Nanopore and PacBio data")
      (description
       "The Filtlong package is a tool for filtering long reads by quality.
It can take a set of long reads and produce a smaller, better subset.  It uses
both read length (longer is better) and read identity (higher is better) when
choosing which reads pass the filter.")
      (license (list license:gpl3       ;filtlong
                     license:asl2.0))))) ;histogram.py

(define-public nanopolish
  ;; The recommended way to install is to clone the git repository
  ;; <https://github.com/jts/nanopolish#installing-a-particular-release>.
  ;; Also, the differences between release and current version seem to be
  ;; significant.
  (let ((commit "50e8b5cc62f9b46f5445f5c5e8c5ab7263ea6d9d")
        (revision "1"))
    (package
      (name "nanopolish")
      (version (git-version "0.10.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jts/nanopolish.git")
               (commit commit)
               (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "09j5gz57yr9i34a27vbl72i4g8syv2zzgmsfyjq02yshmnrvkjs6"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (delete-file-recursively "htslib")
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags
         `("HDF5=noinstall" "EIGEN=noinstall" "HTS=noinstall" "CC=gcc")
         #:tests? #f                    ; no check target
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'find-eigen
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (string-append (assoc-ref inputs "eigen")
                                      "/include/eigen3"))
               #t))
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (scripts (string-append out "/share/nanopolish/scripts")))

                 (install-file "nanopolish" bin)
                 (for-each (lambda (file) (install-file file scripts))
                           (find-files "scripts" ".*"))
                 #t)))
           (add-after 'install 'wrap-programs
             (lambda* (#:key outputs #:allow-other-keys)
               (for-each (lambda (file)
                           (wrap-program file `("PYTHONPATH" ":" prefix (,path))))
                         (find-files "/share/nanopolish/scripts" "\\.py"))
               (for-each (lambda (file)
                           (wrap-program file `("PERL5LIB" ":" prefix (,path))))
                         (find-files  "/share/nanopolish/scripts" "\\.pl"))
               #t)))))
      (inputs
       `(("eigen" ,eigen)
         ("hdf5" ,hdf5)
         ("htslib" ,htslib)
         ("perl" ,perl)
         ("python" ,python-wrapper)
         ("python-biopython" ,python-biopython)
         ("python-numpy" ,python-numpy)
         ("python-pysam" ,python-pysam)
         ("python-scikit-learn" , python-scikit-learn)
         ("python-scipy" ,python-scipy)
         ("zlib" ,zlib)))
      (home-page "https://github.com/jts/nanopolish")
      (synopsis "Signal-level analysis of Oxford Nanopore sequencing data")
      (description
       "This package analyses the Oxford Nanopore sequencing data at signal-level.
Nanopolish can calculate an improved consensus sequence for a draft genome
assembly, detect base modifications, call SNPs (Single nucleotide
polymorphisms) and indels with respect to a reference genome and more.")
      (license license:expat))))

(define-public cnvkit
  (package
    (name "cnvkit")
    (version "0.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/etal/cnvkit.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g2f78k68yglmj4fsfmgs8idqv3di9aj53fg0ld0hqljg8chhh82"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-biopython" ,python-biopython)
       ("python-future" ,python-future)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-reportlab" ,python-reportlab)
       ("python-pandas" ,python-pandas)
       ("python-pysam" ,python-pysam)
       ("python-pyfaidx" ,python-pyfaidx)
       ("python-scipy" ,python-scipy)
       ;; R packages
       ("r-dnacopy" ,r-dnacopy)))
    (home-page "https://cnvkit.readthedocs.org/")
    (synopsis "Copy number variant detection from targeted DNA sequencing")
    (description
     "CNVkit is a Python library and command-line software toolkit to infer
and visualize copy number from high-throughput DNA sequencing data.  It is
designed for use with hybrid capture, including both whole-exome and custom
target panels, and short-read sequencing platforms such as Illumina and Ion
Torrent.")
    (license license:asl2.0)))

(define-public python-pyfit-sne
  (package
    (name "python-pyfit-sne")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KlugerLab/pyFIt-SNE.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13wh3qkzs56azmmgnxib6xfr29g7xh09sxylzjpni5j0pp0rc5qw"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
    (inputs
     `(("fftw" ,fftw)))
    (native-inputs
     `(("python-cython" ,python-cython)))
    (home-page "https://github.com/KlugerLab/pyFIt-SNE")
    (synopsis "FFT-accelerated Interpolation-based t-SNE")
    (description
     "t-Stochastic Neighborhood Embedding (t-SNE) is a highly successful
method for dimensionality reduction and visualization of high dimensional
datasets.  A popular implementation of t-SNE uses the Barnes-Hut algorithm to
approximate the gradient at each iteration of gradient descent.  This package
is a Cython wrapper for FIt-SNE.")
    (license license:bsd-4)))

(define-public bbmap
  (package
    (name "bbmap")
    (version "35.82")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/bbmap/BBMap_" version ".tar.gz"))
              (sha256
               (base32
                "1q4rfhxcb6z3gm8zg2davjz98w22lkf4hm9ikxz9kdl93pil3wkd"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "dist"
       #:tests? #f ; there are none
       #:make-flags
       (list (string-append "-Dmpijar="
                            (assoc-ref %build-inputs "java-openmpi")
                            "/lib/mpi.jar"))
       #:modules ((guix build ant-build-system)
                  (guix build utils)
                  (guix build java-utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-jni-library
           (lambda _
             (with-directory-excursion "jni"
               (invoke "make" "-f" "makefile.linux"))))
         ;; There is no install target
         (replace 'install (install-jars "dist"))
         (add-after 'install 'install-scripts-and-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "calcmem.sh"
               (("\\| awk ") (string-append "| " (which "awk") " ")))
             (let* ((scripts (find-files "." "\\.sh$"))
                    (out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/bbmap"))
                    (jni (string-append out "/lib/jni")))
               (substitute* scripts
                 (("\\$DIR\"\"docs") doc)
                 (("^CP=.*")
                  (string-append "CP=" out "/share/java/BBTools.jar\n"))
                 (("^NATIVELIBDIR.*")
                  (string-append "NATIVELIBDIR=" jni "\n"))
                 (("CMD=\"java")
                  (string-append "CMD=\"" (which "java"))))
               (for-each (lambda (script) (install-file script bin)) scripts)

               ;; Install JNI library
               (install-file "jni/libbbtoolsjni.so" jni)

               ;; Install documentation
               (install-file "docs/readme.txt" doc)
               (copy-recursively "docs/guides" doc))
             #t)))
       #:jdk ,openjdk11))
    (inputs
     `(("gawk" ,gawk)
       ("java-eclipse-jdt-core" ,java-eclipse-jdt-core)
       ("java-eclipse-jdt-compiler-apt" ,java-eclipse-jdt-compiler-apt)
       ("java-openmpi" ,java-openmpi)))
    (home-page "http://sourceforge.net/projects/bbmap/")
    (synopsis "Aligner and other tools for short sequencing reads")
    (description
     "This package provides bioinformatic tools to align, deduplicate,
reformat, filter and normalize DNA and RNA-seq data.  It includes the
following tools: BBMap, a short read aligner for DNA and RNA-seq data; BBNorm,
a kmer-based error-correction and normalization tool; Dedupe, a tool to
simplify assemblies by removing duplicate or contained subsequences that share
a target percent identity; Reformat, to convert reads between
fasta/fastq/scarf/fasta+qual/sam, interleaved/paired, and ASCII-33/64, at over
500 MB/s; and BBDuk, a tool to filter, trim, or mask reads with kmer matches
to an artifact/contaminant file.")
    (license license:bsd-3)))

(define-public velvet
  (package
    (name "velvet")
    (version "1.2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.ebi.ac.uk/~zerbino/velvet/"
                                  "velvet_" version ".tgz"))
              (sha256
               (base32
                "0h3njwy66p6bx14r3ar1byb0ccaxmxka4c65rn4iybyiqa4d8kc8"))
              ;; Delete bundled libraries
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file "Manual.pdf")
                  (delete-file-recursively "third-party")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("OPENMP=t")
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-zlib-include
           (lambda _
             (substitute* "src/binarySequences.c"
               (("../third-party/zlib-1.2.3/zlib.h") "zlib.h"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/velvet")))
               (mkdir-p bin)
               (mkdir-p doc)
               (install-file "velveth" bin)
               (install-file "velvetg" bin)
               (install-file "Manual.pdf" doc)
               (install-file "Columbus_manual.pdf" doc)
               #t))))))
    (inputs
     `(("openmpi" ,openmpi)
       ("zlib" ,zlib)))
    (native-inputs
     `(("texlive" ,(texlive-union (list texlive-latex-graphics
                                        texlive-latex-hyperref)))))
    (home-page "https://www.ebi.ac.uk/~zerbino/velvet/")
    (synopsis "Nucleic acid sequence assembler for very short reads")
    (description
     "Velvet is a de novo genomic assembler specially designed for short read
sequencing technologies, such as Solexa or 454.  Velvet currently takes in
short read sequences, removes errors then produces high quality unique
contigs.  It then uses paired read information, if available, to retrieve the
repeated areas between contigs.")
    (license license:gpl2+)))

(define-public python-velocyto
  (package
    (name "python-velocyto")
    (version "0.17.17")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "velocyto" version))
       (sha256
        (base32
         "0fgygyzqgrq32dv6a00biq1p1cwi6kbl5iqblxq1kklj6b2mzmhs"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-cython" ,python-cython)
       ("python-h5py" ,python-h5py)
       ("python-loompy" ,python-loompy)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numba" ,python-numba)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-pysam" ,python-pysam)
       ("python-scikit-learn" ,python-scikit-learn)
       ("python-scipy" ,python-scipy)))
    (home-page "https://github.com/velocyto-team/velocyto.py")
    (synopsis "RNA velocity analysis for single cell RNA-seq data")
    (description
     "Velocyto is a library for the analysis of RNA velocity.  Velocyto
includes a command line tool and an analysis pipeline.")
    (license license:bsd-2)))

(define-public arriba
  (package
    (name "arriba")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/suhrig/arriba/releases/"
                           "download/v" version "/arriba_v" version ".tar.gz"))
       (sha256
        (base32
         "0jx9656ry766vb8z08m1c3im87b0c82qpnjby9wz4kcz8vn87dx2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are none
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((htslib (assoc-ref inputs "htslib")))
               (substitute* "Makefile"
                 (("-I\\$\\(HTSLIB\\)/htslib")
                  (string-append "-I" htslib "/include/htslib"))
                 ((" \\$\\(HTSLIB\\)/libhts.a")
                  (string-append " " htslib "/lib/libhts.so"))))
             (substitute* "run_arriba.sh"
               (("^STAR ") (string-append (which "STAR") " "))
               (("samtools --version-only")
                (string-append (which "samtools") " --version-only"))
               (("samtools index")
                (string-append (which "samtools") " index"))
               (("samtools sort")
                (string-append (which "samtools") " sort")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (install-file "arriba" bin)
               (install-file "run_arriba.sh" bin)
               (install-file "draw_fusions.R" bin)
               (wrap-program (string-append bin "/draw_fusions.R")
                 `("R_LIBS_SITE" ":" prefix (,(getenv "R_LIBS_SITE")))))
             #t)))))
    (inputs
     `(("htslib" ,htslib)
       ("r-minimal" ,r-minimal)
       ("r-circlize" ,r-circlize)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("samtools" ,samtools)
       ("star" ,star)
       ("zlib" ,zlib)))
    (home-page "https://github.com/suhrig/arriba")
    (synopsis "Gene fusion detection from RNA-Seq data ")
    (description
     "Arriba is a command-line tool for the detection of gene fusions from
RNA-Seq data.  It was developed for the use in a clinical research setting.
Therefore, short runtimes and high sensitivity were important design criteria.
It is based on the fast STAR aligner and the post-alignment runtime is
typically just around two minutes.  In contrast to many other fusion detection
tools which build on STAR, Arriba does not require to reduce the
@code{alignIntronMax} parameter of STAR to detect small deletions.")
    ;; All code is under the Expat license with the exception of
    ;; "draw_fusions.R", which is under GPLv3.
    (license (list license:expat license:gpl3))))

(define-public adapterremoval
  (package
    (name "adapterremoval")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MikkelSchubert/adapterremoval.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nf3ki5pfzalhrx2fr1y6pfqfi133yj2m7q4fj9irf5fb94bapwr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "COLOR_BUILD=no"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://adapterremoval.readthedocs.io/")
    (synopsis "Rapid sequence adapter trimming, identification, and read merging")
    (description
     "This program searches for and removes remnant adapter sequences from
@dfn{High-Throughput Sequencing} (HTS) data and (optionally) trims low quality
bases from the 3' end of reads following adapter removal.  AdapterRemoval can
analyze both single end and paired end data, and can be used to merge
overlapping paired-ended reads into (longer) consensus sequences.
Additionally, the AdapterRemoval may be used to recover a consensus adapter
sequence for paired-ended data, for which this information is not available.")
    (license license:gpl3+)))

(define-public pplacer
  (let ((commit "807f6f3"))
    (package
      (name "pplacer")
      ;; The commit should be updated with each version change.
      (version "1.1.alpha19")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/matsen/pplacer.git")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11ppbbbx20p2g9wj3ff64dhnarb12q79v7qh4rk0gj6lkbz4n7cn"))))
      (build-system ocaml-build-system)
      (arguments
       `(#:modules ((guix build ocaml-build-system)
                    (guix build utils)
                    (ice-9 ftw))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'fix-build-with-latest-ocaml
             (lambda _
               (substitute* "myocamlbuild.ml"
                 (("dep \\[\"c_pam\"\\]" m)
                  (string-append "flag [\"ocaml\"; \"compile\"] (A \"-unsafe-string\");\n"
                                 m))
                 (("let run_and_read" m)
                  (string-append "
let split s ch =
  let x = ref [] in
  let rec go s =
    let pos = String.index s ch in
    x := (String.before s pos)::!x;
    go (String.after s (pos + 1))
  in
  try go s
  with Not_found -> !x
let split_nl s = split s '\\n'
let before_space s =
  try String.before s (String.index s ' ')
  with Not_found -> s

" m))
                 (("run_and_read \"ocamlfind list \\| cut -d' ' -f1\"" m)
                  (string-append "List.map before_space (split_nl & " m ")"))
                 (("    blank_sep_strings &") "")
                 (("      Lexing.from_string &") ""))
               #t))
           (add-after 'unpack 'replace-bundled-cddlib
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((cddlib-src (assoc-ref inputs "cddlib-src"))
                      (local-dir "cddlib_guix"))
                 (mkdir local-dir)
                 (with-directory-excursion local-dir
                   (invoke "tar" "xvf" cddlib-src))
                 (let ((cddlib-src-folder
                        (string-append local-dir "/"
                                       (list-ref (scandir local-dir) 2)
                                       "/lib-src")))
                   (for-each make-file-writable (find-files "cdd_src" ".*"))
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
      (inputs
       `(("zlib" ,zlib "static")
         ("gsl" ,gsl)
         ("ocaml-ounit" ,ocaml-ounit)
         ("ocaml-batteries" ,ocaml-batteries)
         ("ocaml-camlzip" ,camlzip)
         ("ocaml-csv" ,ocaml-csv)
         ("ocaml-sqlite3" ,ocaml-sqlite3)
         ("ocaml-xmlm" ,ocaml-xmlm)
         ("ocaml-mcl" ,ocaml-mcl)
         ("ocaml-gsl" ,ocaml-gsl-1)))
      (native-inputs
       `(("cddlib-src" ,(package-source cddlib))
         ("ocamlbuild" ,ocamlbuild)
         ("pkg-config" ,pkg-config)))
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
           (lambda _ (chdir "scripts") #t))
         (replace 'check
           (lambda _ (invoke "python" "-m" "unittest" "discover" "-v") #t))
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

(define-public python2-checkm-genome
  (package
    (name "python2-checkm-genome")
    (version "1.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "checkm-genome" version))
       (sha256
        (base32
         "0bm8gpxjmzxsxxl8lzwqhgx8g1dlnmp6znz7wv3hgb0gdjbf9dzz"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f)) ; some tests are interactive
    (propagated-inputs
     `(("python-dendropy" ,python2-dendropy)
       ("python-matplotlib" ,python2-matplotlib)
       ("python-numpy" ,python2-numpy)
       ("python-pysam" ,python2-pysam)
       ("python-scipy" ,python2-scipy)))
    (home-page "http://pypi.python.org/pypi/checkm/")
    (synopsis "Assess the quality of putative genome bins")
    (description
     "CheckM provides a set of tools for assessing the quality of genomes
recovered from isolates, single cells, or metagenomes.  It provides robust
estimates of genome completeness and contamination by using collocated sets of
genes that are ubiquitous and single-copy within a phylogenetic lineage.
Assessment of genome quality can also be examined using plots depicting key
genomic characteristics (e.g., GC, coding density) which highlight sequences
outside the expected distributions of a typical genome.  CheckM also provides
tools for identifying genome bins that are likely candidates for merging based
on marker set compatibility, similarity in genomic characteristics, and
proximity within a reference genome.")
    (license license:gpl3+)))

(define-public umi-tools
  (package
    (name "umi-tools")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "umi_tools" version))
       (sha256
        (base32
         "08y3vz1vcx09whmbsn722lcs6jl9wyrh9i4p3k8j4cb1i32bij4a"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-pandas" ,python-pandas)
       ("python-future" ,python-future)
       ("python-scipy" ,python-scipy)
       ("python-matplotlib" ,python-matplotlib)
       ("python-regex" ,python-regex)
       ("python-pysam" ,python-pysam)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-cython" ,python-cython)))
    (home-page "https://github.com/CGATOxford/UMI-tools")
    (synopsis "Tools for analyzing unique modular identifiers")
    (description "This package provides tools for dealing with @dfn{Unique
Molecular Identifiers} (UMIs) and @dfn{Random Molecular Tags} (RMTs) in
genetic sequences.  There are six tools: the @code{extract} and
@code{whitelist} commands are used to prepare a fastq containg UMIs @code{+/-}
cell barcodes for alignment.  The remaining commands, @code{group},
@code{dedup}, and @{count}/@code{count_tab}, are used to identify PCR
duplicates using the UMIs and perform different levels of analysis depending
on the needs of the user.")
    (license license:expat)))
