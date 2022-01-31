;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015, 2016, 2018, 2019, 2020 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2020, 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2018 Raoul Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018, 2019, 2020, 2021 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2019, 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Bonface Munyoki Kilyungi <bonfacemunyoki@gmail.com>
;;; Copyright © 2021 Tim Howes <timhowes@lavabit.com>
;;; Copyright © 2021 Hong Li <hli@mdc-berlin.de>
;;; Copyright © 2021, 2022 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
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
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system go)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system ocaml)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system r)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system trivial)
  #:use-module (guix deprecation)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages node)
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
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages skribilo)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages uglifyjs)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
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
                    (url "https://github.com/Ecogenomics/BamM")
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
                  (delete-file-recursively "c/htslib-1.3.1")))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; BamM is Python 2 only.
       ;; Do not use bundled libhts.  Do use the bundled libcfu because it has
       ;; been modified from its original form.
       #:configure-flags
       ,#~(let ((htslib #$(this-package-input "htslib")))
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
                 (invoke "./autogen.sh")))))
         (delete 'build)                ;the build loops otherwise
         (replace 'check
           (lambda _
             ;; There are 2 errors printed, but they are safe to ignore:
             ;; 1) [E::hts_open_format] fail to open file ...
             ;; 2) samtools view: failed to open ...
             (invoke "nosetests")))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (path (getenv "PATH"))
                    (pythonpath (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/bamm")
                 `("PATH" ":" prefix (,path))
                 `("GUIX_PYTHONPATH" ":" prefix (,pythonpath)))))))))
    (native-inputs
     (list autoconf
           automake
           libtool
           zlib
           python2-nose
           python2-pysam))
    (inputs
     (list htslib-1.3 ; At least one test fails on htslib-1.4+.
           samtools
           bwa
           grep
           sed
           coreutils))
    (propagated-inputs
     (list python2-numpy))
    (home-page "https://ecogenomics.github.io/BamM/")
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
                    (url "https://github.com/pezmaster31/bamtools")
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
    (inputs (list zlib))
    (home-page "https://github.com/pezmaster31/bamtools")
    (synopsis "C++ API and command-line toolkit for working with BAM data")
    (description
     "BamTools provides both a C++ API and a command-line toolkit for handling
BAM files.")
    (license license:expat)))

(define-public bamutils
  (package
    (name "bamutils")
    (version "1.0.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/statgen/bamUtil")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i2r332k1kz0jysyg89d858wqq59n16lw6dv5qmilcwshb77r9v7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; Unclear how to run tests
       #:make-flags
       ,#~(list "USER_WARNINGS=-std=gnu++98" ;
                (string-append "INSTALLDIR=" #$output "/bin"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/Makefile" ;
               (("^DATE=.*") "DATE=\"1970-01-01\"\n"))
             (copy-recursively (assoc-ref inputs "libstatgen")
                               "../libStatGen"))))))
    (inputs
     (list zlib))
    (native-inputs
     `(("libstatgen"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/statgen/libStatGen/")
                 (commit (string-append "v" version))))
           (file-name (git-file-name "libstatgen" version))
           (sha256
            (base32
             "0q9iyk046r4m7qnav8c3f28zsar25lj9nydiklwaswmzdijhi4p1"))))))
    (home-page "https://genome.sph.umich.edu/wiki/BamUtil")
    (synopsis "Programs for working on SAM/BAM files")
    (description "This package provides several programs that perform
operations on SAM/BAM files.  All of these programs are built into a
single executable called @code{bam}.")
    (license license:gpl3+)))

(define-public bcftools
  (package
    (name "bcftools")
    (version "1.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/samtools/bcftools/"
                                  "releases/download/"
                                  version "/bcftools-" version ".tar.bz2"))
              (sha256
               (base32
                "1jqrma16fx8kpvb3c0462dg0asvmiv5yi8myqmc5ddgwi6p8ivxp"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Delete bundled htslib.
                          (delete-file-recursively "htslib-1.14")))))
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
               (("/bin/bash") (which "bash"))))))))
    (native-inputs
     (list htslib perl))
    (inputs
     (list gsl zlib))
    (home-page "https://samtools.github.io/bcftools/")
    (synopsis "Utilities for variant calling and manipulating VCFs and BCFs")
    (description
     "BCFtools is a set of utilities that manipulate variant calls in the
Variant Call Format (VCF) and its binary counterpart BCF.  All commands work
transparently with both VCFs and BCFs, both uncompressed and BGZF-compressed.")
    ;; The sources are dual MIT/GPL, but becomes GPL-only when USE_GPL=1.
    (license (list license:gpl3+ license:expat))))

(define-public bcftools-1.12
  (package/inherit bcftools
    (version "1.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/samtools/bcftools/"
                                  "releases/download/"
                                  version "/bcftools-" version ".tar.bz2"))
              (sha256
               (base32
                "1x94l1hy2pi3lbz0sxlbw0g6q5z5apcrhrlcwda94ns9n4r6a3ks"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Delete bundled htslib.
                          (delete-file-recursively "htslib-1.12")))))
    (native-inputs (list htslib-1.12 perl))))

(define-public bcftools-1.10
  (package/inherit bcftools
    (version "1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/samtools/bcftools/"
                                  "releases/download/"
                                  version "/bcftools-" version ".tar.bz2"))
              (sha256
               (base32
                "10xgwfdgqb6dsmr3ndnpb77mc3a38dy8kh2c6czn6wj7jhdp4dra"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Delete bundled htslib.
                          (delete-file-recursively "htslib-1.10")))))
    (native-inputs (list htslib-1.10 perl))))

(define-public bedops
  (package
    (name "bedops")
    (version "2.4.35")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bedops/bedops")
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
    (version "2.30.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/arq5x/bedtools2/releases/"
                                  "download/v" version "/"
                                  "bedtools-" version ".tar.gz"))
              (sha256
               (base32
                "1f2hh79l7dn147c2xyfgf5wfjvlqfw32kjfnnh2n1qy6rpzx2fik"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       ,#~(list (string-append "prefix=" #$output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("python" ,python-wrapper)))
    (inputs
     (list samtools zlib))
    (home-page "https://github.com/arq5x/bedtools2")
    (synopsis "Tools for genome analysis and arithmetic")
    (description
     "Collectively, the bedtools utilities are a swiss-army knife of tools for
a wide-range of genomics analysis tasks.  The most widely-used tools enable
genome arithmetic: that is, set theory on the genome.  For example, bedtools
allows one to intersect, merge, count, complement, and shuffle genomic
intervals from multiple files in widely-used genomic file formats such as BAM,
BED, GFF/GTF, VCF.")
    (license license:expat)))

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
         (add-after 'unpack 'compatibility
           (lambda _
             (substitute* "src/utils/fileType/FileRecordTypeChecker.h"
               (("static const float PERCENTAGE")
                "static constexpr float PERCENTAGE"))
             (substitute* "src/utils/general/DualQueue.h"
               (("template <class T, template<class T> class CompareFunc>")
                "template <class T, template<class U> class CompareFunc>"))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "bin" ".*"))))))))
    (native-inputs
     `(("python" ,python-wrapper)))
    (inputs
     (list samtools zlib))))

(define-public pbcopper
  ;; This is the latest commit at the time of this writing.
  (let ((commit "ad4143afd25a0bd6adc977c544865c992a515841")
        (revision "1"))
    (package
      (name "pbcopper")
      (version (git-version "1.9.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/PacificBiosciences/pbcopper")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1qxkbpdkamfisnk36lpi1vdvf3p1lg2hdqna3xgd94pz52bwbmp7"))))
      (build-system meson-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-meson-files
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "meson.build"
                 (("-msse4.1") "")))))))
      (inputs
       (list boost))
      (native-inputs
       (list googletest pkg-config))
      (home-page "https://github.com/PacificBiosciences/pbcopper")
      (synopsis "Data structures, algorithms, and utilities for PacBio C++ applications")
      (description
       "The pbcopper library provides a suite of data structures, algorithms,
and utilities for PacBio C++ applications.")
      (license license:bsd-3))))

(define-public pbbam
  (package
    (name "pbbam")
    (version "1.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PacificBiosciences/pbbam")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1avdm5hwhr5ls79017blyalx1npzbf1aa6dgb6j6lg8sq4nk9yyg"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Disable this test.  I tried fixing it by including
             ;; optional_io.hpp, but there's a type error.
             (substitute* "tests/src/meson.build"
               (("'test_ReadGroupInfo.cpp',") ""))
             #;
             (substitute* "include/pbbam/ReadGroupInfo.h"
               (("#include <boost/optional.hpp>" m)
                (string-append m "\n#include <boost/optional/optional_io.hpp>")))
             (substitute* '("tests/scripts/cram/_test.py"
                            "tests/scripts/cram/_main.py")
               (("'/bin/sh'")
                (string-append "'" (which "sh") "'"))))))))
    ;; These libraries are listed as "Required" in the pkg-config file.
    (propagated-inputs
     (list htslib pbcopper zlib))
    (inputs
     (list boost samtools))
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

(define-public pbgzip
  (let ((commit "2b09f97b5f20b6d83c63a5c6b408d152e3982974"))
    (package
      (name "pbgzip")
      (version (git-version "0.0.0" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nh13/pbgzip")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1mlmq0v96irbz71bgw5zcc43g1x32zwnxx21a5p1f1ch4cikw1yd"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake))
      (inputs
       (list zlib))
      (home-page "https://github.com/nh13/pbgzip")
      (synopsis "Parallel Block GZIP")
      (description "This package implements parallel block gzip.  For many
formats, in particular genomics data formats, data are compressed in
fixed-length blocks such that they can be easily indexed based on a (genomic)
coordinate order, since typically each block is sorted according to this order.
This allows for each block to be individually compressed (deflated), or more
importantly, decompressed (inflated), with the latter enabling random retrieval
of data in large files (gigabytes to terabytes).  @code{pbgzip} is not limited
to any particular format, but certain features are tailored to genomics data
formats when enabled.  Parallel decompression is somewhat faster, but the true
speedup comes during compression.")
      (license license:expat))))

(define-public blasr-libcpp
  (package
    (name "blasr-libcpp")
    (version "5.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PacificBiosciences/blasr_libcpp")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07cdfnfl29zf2j7fpaaqaxghq3p0wnc109razs0icwm2q6l3gycb"))))
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
     (list boost hdf5 htslib pbbam zlib))
    (native-inputs
     (list googletest pkg-config))
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
    (version "5.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PacificBiosciences/blasr")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0axyd06gn2xa0p0k76fihsbxpfxvhlb18jn6bf97c0ii58r1wc0k"))))
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
                           hdf5 hdf5))))))))
       ;; Tests require "cram" executable, which is not packaged.
       #:tests? #f
       #:configure-flags '("-Dtests=false")))
    (inputs
     (list boost blasr-libcpp hdf5 pbbam zlib))
    (native-inputs
     (list pkg-config))
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
     (list bedtools-2.18
           samtools-0.1
           r-minimal
           r-foreach
           r-xnomial
           r-domc
           r-multitaper
           r-seqinr))
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
             (url "https://github.com/ratschlab/RiboDiff")
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
         ;; This test fails because of the matplotlib plotting backend.
         (add-after 'unpack 'disable-plot-test
           (lambda _
             (substitute* "src/ribodiff/functional_test_te.py"
               (("pl\\.make_plots\\(data, opts\\)") "#"))))
         ;; Generate an installable executable script wrapper.
         (add-after 'unpack 'patch-setup.py
           (lambda _
             (substitute* "setup.py"
               (("^(.*)packages=.*" line prefix)
                (string-append line "\n"
                               prefix "scripts=['scripts/TE.py'],\n"))))))))
    (inputs
     (list python2-numpy python2-matplotlib python2-scipy
           python2-statsmodels))
    (native-inputs
     (list python2-mock python2-nose))
    (home-page "https://public.bmi.inf.ethz.ch/user/zhongy/RiboDiff/")
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
                    (url "https://github.com/lh3/bioawk")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pxc3zdnirxbf9a0az698hd8xdik7qkhypm7v6hn922x8y9qmspm"))))
    (build-system gnu-build-system)
    (inputs
     (list zlib))
    (native-inputs
     (list bison))
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

(define-public python-htsget
  (package
   (name "python-htsget")
   (version "0.2.5")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "htsget" version))
            (sha256
             (base32
              "0ic07q85vhw9djf23k57b21my7i5xp400m8gfqgr5gcryqvdr0yk"))))
   (build-system python-build-system)
   (native-inputs
    (list python-setuptools-scm))
   (propagated-inputs
    (list python-humanize python-requests python-six))
   (home-page "https://pypi.org/project/htsget/")
   (synopsis "Python API and command line interface for the GA4GH htsget API")
   (description "This package is a client implementation of the GA4GH htsget
protocol.  It provides a simple and reliable way to retrieve genomic data from
servers supporting the protocol.")
   (license license:asl2.0)))

(define-public python-pybedtools
  (package
    (name "python-pybedtools")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pybedtools" version))
              (sha256
               (base32
                "0wc7z8g8prgdx7n5chjva2fdq03wiwhqisjjxzkjg1j5k5ha7151"))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((srfi srfi-26)
                  (guix build utils)
                  (guix build python-build-system))
       ;; See https://github.com/daler/pybedtools/issues/192
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             (substitute* "pybedtools/test/test_helpers.py"
               ;; Requires internet access.
               (("def test_chromsizes")
                "def _do_not_test_chromsizes")
               ;; Broken as a result of the workaround used in the check phase
               ;; (see: https://github.com/daler/pybedtools/issues/192).
               (("def test_getting_example_beds")
                "def _do_not_test_getting_example_beds"))
             ;; This issue still occurs on python2
             (substitute* "pybedtools/test/test_issues.py"
               (("def test_issue_303")
                "def _test_issue_303"))))
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
               (for-each delete-file (filter cythonized? c/c++-files)))))
         (add-after 'remove-cython-generated-files 'generate-cython-extensions
           (lambda _
             (invoke "python" "setup.py" "cythonize")))
         (replace 'check
           (lambda _
             ;; The tests need to be run from elsewhere...
             (mkdir-p "/tmp/test")
             (copy-recursively "pybedtools/test" "/tmp/test")
             (with-directory-excursion "/tmp/test"
               (invoke "pytest" "-v" "--doctest-modules")))))))
    (propagated-inputs
     (list bedtools samtools python-matplotlib python-pysam
           python-pyyaml))
    (native-inputs
     (list python-numpy
           python-pandas
           python-cython
           kentutils ; for bedGraphToBigWig
           python-six
           ;; For the test suite.
           python-pytest
           python-psutil))
    (home-page "https://pythonhosted.org/pybedtools/")
    (synopsis "Python wrapper for BEDtools programs")
    (description
     "pybedtools is a Python wrapper for Aaron Quinlan's BEDtools programs,
which are widely used for genomic interval manipulation or \"genome algebra\".
pybedtools extends BEDTools by offering feature-level manipulations from with
Python.")
    (license license:gpl2+)))

(define-public python2-pybedtools
  (let ((pybedtools (package-with-python2 python-pybedtools)))
    (package
      (inherit pybedtools)
      (native-inputs
       (modify-inputs (package-native-inputs pybedtools)
         (prepend python2-pathlib))))))

(define-public python-biom-format
  (package
    (name "python-biom-format")
    (version "2.1.10")
    (source
     (origin
       (method git-fetch)
       ;; Use GitHub as source because PyPI distribution does not contain
       ;; test data: https://github.com/biocore/biom-format/issues/693
       (uri (git-reference
             (url "https://github.com/biocore/biom-format")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0i62j6ksmp78ap2dnl969gq6vprc3q87zc8ksj9if8g2603iq6i8"))
       (modules '((guix build utils)))
       ;; Delete generated C files.
       (snippet
        '(for-each delete-file (find-files "." "\\.c")))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-cython
           (lambda _ (setenv "USE_CYTHON" "1")))
         (add-after 'unpack 'relax
           (lambda _
             (substitute* "setup.py"
               (("pytest < 5.3.4") "pytest"))))
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
                               m))))))))
    (propagated-inputs
     (list python-anndata
           python-numpy
           python-scipy
           python-flake8
           python-future
           python-click
           python-h5py
           python-pandas))
    (native-inputs
     (list python-cython python-pytest python-pytest-cov python-nose))
    (home-page "http://www.biom-format.org")
    (synopsis "Biological Observation Matrix (BIOM) format utilities")
    (description
     "The BIOM file format is designed to be a general-use format for
representing counts of observations e.g. operational taxonomic units, KEGG
orthology groups or lipid types, in one or more biological samples
e.g. microbiome samples, genomes, metagenomes.")
    (license license:bsd-3)))

(define-public python-pairtools
  (package
    (name "python-pairtools")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirnylab/pairtools")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gr8y13q7sd6yai6df4aavl2470n1f9s3cib6r473z4hr8hcbwmc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-references
           (lambda _
             (substitute* '("pairtools/pairtools_merge.py"
                            "pairtools/pairtools_sort.py")
               (("/bin/bash") (which "bash")))
             #t))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (with-directory-excursion "/tmp"
               (invoke "pytest" "-v")))))))
    (native-inputs
     (list python-cython python-nose python-pytest))
    (inputs
     `(("python" ,python-wrapper)))
    (propagated-inputs
     (list htslib ; for bgzip, looked up in PATH
           samtools ; looked up in PATH
           lz4 ; for lz4c
           python-click
           python-numpy))
    (home-page "https://github.com/mirnylab/pairtools")
    (synopsis "Process mapped Hi-C data")
    (description "Pairtools is a simple and fast command-line framework to
process sequencing data from a Hi-C experiment.  Process pair-end sequence
alignments and perform the following operations:

@itemize
@item detect ligation junctions (a.k.a. Hi-C pairs) in aligned paired-end
  sequences of Hi-C DNA molecules
@item sort @code{.pairs} files for downstream analyses
@item detect, tag and remove PCR/optical duplicates
@item generate extensive statistics of Hi-C datasets
@item select Hi-C pairs given flexibly defined criteria
@item restore @code{.sam} alignments from Hi-C pairs.
@end itemize
")
    (license license:expat)))

(define-public bioperl-minimal
  (package
    (name "bioperl-minimal")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bioperl/bioperl-live")
             (commit (string-append "release-"
                                    (string-map (lambda (c)
                                                  (if (char=? c #\.)
                                                      #\- c)) version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wl8yvzcls59pwwk6m8ahy87pwg6nnibzy5cldbvmcwg2x2w7783"))))
    (build-system perl-build-system)
    (arguments
     (let ((transitive-inputs
            (map (compose package-name cadr)
                 (delete-duplicates
                  (concatenate
                   (map (compose package-transitive-target-inputs cadr)
                        (package-inputs this-package)))))))
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
                 #t)))))))
    (inputs
     (list perl-module-build perl-data-stag perl-libwww perl-uri))
    (native-inputs
     (list perl-test-most))
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
    (license license:perl-license)))

(define-public perl-bio-db-hts
  (package
    (name "perl-bio-db-hts")
    (version "3.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AV/AVULLO/Bio-DB-HTS-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0hjg0igfkpvh27zdkdr6pa7cqm9n6r7cwz0np74cl4wmawgvr9hj"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build pkg-config))
    (propagated-inputs
     (list bioperl-minimal htslib-1.9))
    (home-page "https://metacpan.org/release/Bio-DB-HTS")
    (synopsis "Perl interface to HTS library for DNA sequencing")
    (description "This is a Perl interface to the HTS library for DNA
sequencing.")
    (license license:asl2.0)))

(define-public python-biopython
  (package
    (name "python-biopython")
    (version "1.76")
    (source (origin
              (method url-fetch)
              ;; use PyPi rather than biopython.org to ease updating
              (uri (pypi-uri "biopython" version))
              (sha256
               (base32
                "0wlch9xpa0fpgjzyxi6jsfca6iakaq9a05927xg8vqnmvaccnwrq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-home
           ;; Some tests require a home directory to be set.
           (lambda _ (setenv "HOME" "/tmp") #t)))))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://biopython.org/")
    (synopsis "Tools for biological computation in Python")
    (description
     "Biopython is a set of tools for biological computation including parsers
for bioinformatics files into Python data structures; interfaces to common
bioinformatics programs; a standard sequence class and tools for performing
common operations on them; code to perform data classification; code for
dealing with alignments; code making it easy to split up parallelizable tasks
into separate processes; and more.")
    (license (license:non-copyleft "http://www.biopython.org/DIST/LICENSE"))))

(define-public python-biopython-1.73
  (package
    (inherit python-biopython)
    (version "1.73")
    (source (origin
              (method url-fetch)
              ;; use PyPi rather than biopython.org to ease updating
              (uri (pypi-uri "biopython" version))
              (sha256
               (base32
                "1q55jhf76z3k6is3psis0ckbki7df26x7dikpcc3vhk1vhkwribh"))))))

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

(define-public biosoup
  (package
    (name "biosoup")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rvaser/biosoup")
             ;; Corresponds to version 0.10.0
             (commit "38181f09854ff42cbd9632200a2ec9fb37a4b7b6")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "02hvyka703zagx0nvv2yx3dkc748zc8g6qbrpya7r8kfkcl7y8hw"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./bin/biosoup_test")))))))
    (native-inputs
     (list googletest))
    (home-page "https://github.com/rvaser/biosoup")
    (synopsis "C++ support library for bioinformatics tools")
    (description "Biosoup is a C++ collection of header-only data structures
used for storage and logging in bioinformatics tools.")
    (license license:expat)))

(define-public bioparser
  (package
    (name "bioparser")
    (version "3.0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rvaser/bioparser")
             ;; Corresponds to tag 3.0.13
             (commit "13341e6e0855c6b358ffcea6dad216e1009e1287")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0c5p2dl8jb12ci9f427jzrmmm9cgvc1k4fxsn2ggkfsin6r1r82i"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./bin/bioparser_test")))))))
    (inputs
     (list biosoup))
    (propagated-inputs
     (list zlib))
    (native-inputs
     (list googletest))
    (home-page "https://github.com/rvaser/bioparser")
    (synopsis "C++ library for parsing several formats in bioinformatics")
    (description "Bioparser is a C++ header only parsing library for several
bioinformatics formats (FASTA/Q, MHAP/PAF/SAM), with support for zlib
compressed files.")
    (license license:expat)))

(define-public circtools
  (package
    (name "circtools")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Kevinzjy/circtools")
             ;; Corresponds to tag v1.0.0
             (commit "79380de59013601021ca3b1352d6f64d2fb89646")
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wg1s927g32k25j967kfr8l30nmr4c0p4zvy5igvy7cs6chd60lh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _
             (for-each make-file-writable (find-files "."))))
         (add-after 'unpack 'prepare-spoa-dependencies
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "vendor/spoa/CMakeLists.txt"
               (("find_package\\(bioparser 3.0.13 QUIET\\)")
                "find_package(bioparser 3.0.13 CONFIG)")
               (("find_package\\(biosoup 0.10.0 QUIET\\)")
                "find_package(biosoup 0.10.0 CONFIG)")
               (("GTest_FOUND") "TRUE")))))
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-bio" ,rust-bio-0.33)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-docopt" ,rust-docopt-1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-indicatif" ,rust-indicatif-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-seq-io" ,rust-seq-io-0.3))))
    (inputs
     (list bioparser biosoup))
    (native-inputs
     (list cmake pkg-config googletest))
    (home-page "https://github.com/Kevinzjy/circtools")
    (synopsis "Accelerating functions in CIRI toolkit")
    (description "This package provides accelerated functions for the CIRI
toolkit.  It also provides the @code{ccs} executable to scan for circular
consensus sequences.")
    (license license:expat)))

(define-public ciri-long
  (package
    (name "ciri-long")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bioinfo-biols/CIRI-long")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10k88i1fcqchrrjv82rmylwvbwqfba0n51palhig9hsg71xs0dbi"))
       ;; Delete bundled binary
       (snippet '(delete-file "libs/ccs"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               (("'argparse[^']*',") "") ; only for python2
               (("==") ">="))))
         (add-before 'build 'build-libssw
           (lambda _
             (with-directory-excursion "libs/striped_smith_waterman"
               (invoke "make" "libssw.so"))))
         (add-before 'build 'fix-reference-to-ccs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CIRI_long/pipeline.py"
               (("'ccs -i")
                (string-append "'"
                               (assoc-ref inputs "circtools") "/bin/ccs"
                               " -i")))
             ;; yuck!
             (substitute* "CIRI_long/main.py"
               (("os.chmod\\(lib_path.*") "")))))))
    (inputs
     (list circtools
           python-biopython
           python-bwapy
           python-levenshtein
           python-mappy
           python-numpy
           python-pandas
           python-pysam
           python-pyspoa
           python-scikit-learn
           python-scipy))
    (native-inputs
     (list python-cython python-nose python-setuptools))
    (home-page "https://ciri-cookbook.readthedocs.io/")
    (synopsis "Circular RNA identification for Nanopore sequencing")
    (description "CIRI-long is a package for circular RNA identification using
long-read sequencing data.")
    (license license:expat)))

(define-public qtltools
  (package
    (name "qtltools")
    (version "1.3.1")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://qtltools.github.io/qtltools/"
                                  "binaries/QTLtools_" version
                                  "_source.tar.gz"))
              (sha256
               (base32
                "13gdry5l43abn3464fmk8qzrxgxnxah2612r66p9dzhhl92j30cd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests included
       #:make-flags
       ,#~(list (string-append "BOOST_INC="
                               #$(this-package-input "boost") "/include")
                (string-append "BOOST_LIB="
                               #$(this-package-input "boost") "/lib")
                (string-append "HTSLD_INC="
                               #$(this-package-input "htslib") "/include")
                (string-append "HTSLD_LIB="
                               #$(this-package-input "htslib") "/lib")
                (string-append "RMATH_INC="
                               #$(this-package-input "rmath-standalone")
                               "/include")
                (string-append "RMATH_LIB="
                               #$(this-package-input "rmath-standalone")
                               "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-linkage
           (lambda _
             (substitute* "qtltools/Makefile"
               (("libboost_iostreams.a")
                "libboost_iostreams.so")
               (("libboost_program_options.a")
                "libboost_program_options.so")
               (("-lblas") "-lopenblas"))))
         (add-before 'build 'chdir
           (lambda _ (chdir "qtltools")))
         (replace 'configure
           (lambda _
             (substitute* "qtltools/Makefile"
               (("LIB_FLAGS=-lz")
                "LIB_FLAGS=-lz -lcrypto -lssl")
               (("LIB_FILES=\\$\\(RMATH_LIB\\)/libRmath.a \
\\$\\(HTSLD_LIB\\)/libhts.a \
\\$\\(BOOST_LIB\\)/libboost_iostreams.a \
\\$\\(BOOST_LIB\\)/libboost_program_options.a")
                "LIB_FILES=$(RMATH_LIB)/libRmath.so \
$(HTSLD_LIB)/libhts.so \
$(BOOST_LIB)/libboost_iostreams.so \
$(BOOST_LIB)/libboost_program_options.so"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "bin/QTLtools" bin)))))))
    (inputs
     (list curl
           gsl
           boost
           rmath-standalone
           htslib-1.3
           openssl
           openblas
           zlib))
    (home-page "https://qtltools.github.io/qtltools/")
    (synopsis "Tool set for molecular QTL discovery and analysis")
    (description "QTLtools is a tool set for molecular QTL discovery
and analysis.  It allows going from the raw genetic sequence data to
collection of molecular @dfn{Quantitative Trait Loci} (QTLs) in few
easy-to-perform steps.")
    (license license:gpl3+)))

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
       (list bpp-core bpp-seq))
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
       (list bpp-core bpp-seq))
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
       (list bpp-core))
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
       (list groff man-db texinfo))
      (inputs
       `(("bpp-core" ,bpp-core)
         ("bpp-seq" ,bpp-seq)
         ("bpp-phyl" ,bpp-phyl)
         ("bpp-phyl" ,bpp-popgen)))
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
    (version "2.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/"
                    version "/ncbi-blast-" version "+-src.tar.gz"))
              (sha256
               (base32
                "0m0r9vkw631ky1za1wilsfk9k9spwqh22nkrb9a57rbwmrc1i3nq"))
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
     (list cpio))
    (home-page "https://blast.ncbi.nlm.nih.gov")
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
                  (delete-file-recursively ".git")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:make-flags
       ,#~(list (string-append "ZLIB="
                               #$(this-package-input "zlib")
                               "/lib/libz.so")
                (string-append "LDFLAGS="
                               (string-join '("-lboost_filesystem"
                                              "-lboost_system"
                                              "-lboost_iostreams"
                                              "-lz"
                                              "-fopenmp"))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-build-bundled-pigz
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "Makefile"
              (("cd pigz/pigz-2.3.3; make") ""))))
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
                              "/bin/pigz\";")))))
         (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
              (for-each (lambda (file)
                          (install-file file bin))
                        '("bless" "kmc/bin/kmc")))))
         (delete 'configure))))
    (native-inputs
     (list perl))
    (inputs
     (list openmpi boost sparsehash pigz zlib))
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
                    (url "https://github.com/BenLangmead/bowtie2")
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
                    (("-DBUILD_TIME=.*") "-DBUILD_TIME=\"\\\"0\\\"\""))))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       ,#~(list "allall"
                "WITH_TBB=1"
                (string-append "prefix=" #$output))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             ;; This "extended character" is not considered valid.
             (substitute* "processor_support.h"
               (("“") "\"")
               (("”") "\""))))
         (replace 'check
           (lambda _
             (invoke "perl"
                     "scripts/test/simple_tests.pl"
                     "--bowtie2=./bowtie2"
                     "--bowtie2-build=./bowtie2-build"))))))
    (inputs
     `(("tbb" ,tbb-2020)
       ("zlib" ,zlib)
       ("python" ,python-wrapper)))
    (native-inputs
     (list perl perl-clone perl-test-deep perl-test-simple))
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
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bowtie-bio/bowtie/"
                                  version "/bowtie-" version "-src.zip"))
              (sha256
               (base32
                "11dbihdnrizc6qhx9xsw77w3q5ssx642alaqzvhxx32ak9glvq04"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Makefile"
                  ;; replace BUILD_HOST and BUILD_TIME for deterministic build
                  (("-DBUILD_HOST=.*") "-DBUILD_HOST=\"\\\"guix\\\"\"")
                  (("-DBUILD_TIME=.*") "-DBUILD_TIME=\"\\\"0\\\"\"")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:make-flags
       ,#~(list "CC=gcc" "all"
                (string-append "prefix=" #$output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list python-wrapper tbb-2020 zlib))
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
     '(#:parallel-build? #f             ; not supported
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'hide-default-gcc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               ;; Remove the default GCC from CPLUS_INCLUDE_PATH to prevent
               ;; conflicts with the GCC 5 input.
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gcc "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                        ":"))
               #t)))
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
    (native-inputs
     `(("gcc@5" ,gcc-5))) ;; doesn't build with later versions
    (inputs
     `(("boost" ,boost)
       ("bowtie" ,bowtie)
       ("ncurses" ,ncurses)
       ("perl" ,perl)
       ("python" ,python-2)
       ("samtools" ,samtools-0.1)
       ("seqan" ,seqan-1)
       ("zlib" ,zlib)))
    (home-page "https://ccb.jhu.edu/software/tophat/index.shtml")
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
       #:make-flags '("CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (doc (string-append out "/share/doc/bwa"))
                    (man (string-append out "/share/man/man1")))
               (install-file "bwa" bin)
               (install-file "libbwa.a" lib)
               (install-file "README.md" doc)
               (install-file "bwa.1" man))))
           ;; no "configure" script
          (delete 'configure))))
    (inputs (list zlib))
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
                    (url "https://github.com/pkerpedjiev/bwa-pssm")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "076c4q0cdqz8jgylb067y9zmvxglppnzi3qiscn0xiypgc6lgb5r"))))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments bwa)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-C-error
             (lambda _
               (substitute* "pssm.c"
                 (("inline int map") "int map"))))))))
    (inputs
     (list gdsl zlib perl))
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
    (version "0.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brentp/bwa-meth")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c695lkrr0996zwkibl7324wg2vxmn6522sz30xv4a9gaf0lnbh3"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'keep-references-to-bwa
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "bwameth.py"
               (("bwa (mem|index)" _ command)
                (string-append (which "bwa") " " command))
               ;; There's an ill-advised check for "samtools" on PATH.
               (("^checkX.*") "")))))))
    (inputs
     (list bwa))
    (native-inputs
     (list python-toolshed))
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
    (version "0.8.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bxlab/bx-python")
                    (commit "f4e6a5c93e719db69b5798b6fdd9b167da358316")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mclahslz34vq9x424jmzsxk0nmpm1j716fa8h3zwr9ssvch7skc"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy))
    (inputs
     (list zlib))
    (native-inputs
     (list python-lzo python-nose python-cython))
    (home-page "https://github.com/bxlab/bx-python")
    (synopsis "Tools for manipulating biological data")
    (description
     "bx-python provides tools for manipulating biological data, particularly
multiple sequence alignments.")
    (license license:expat)))

(define-public python-pyega3
  (package
    (name "python-pyega3")
    (version "3.4.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyega3" version))
              (sha256
               (base32
                "1k736in8g27rarx65ym9xk50x53zjg75h37bb8ljynxv04rypx2q"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; The tests require network access.
    (native-inputs
     (list python-psutil python-htsget))
    (propagated-inputs
     (list python-requests python-tqdm python-urllib3 python-responses))
    (home-page "https://github.com/EGA-archive/ega-download-client")
    (synopsis "Python client for EGA")
    (description "This package is a python-based tool for viewing and
downloading files from authorized EGA datasets.  It uses the EGA data API and
has several key features:
@itemize
@item Files are transferred over secure https connections and received
  unencrypted, so no need for decryption after download.
@item Downloads resume from where they left off in the event that the
  connection is interrupted.
@item Supports file segmenting and parallelized download of segments,
  improving overall performance.
@item After download completes, file integrity is verified using checksums.
@item Implements the GA4GH-compliant htsget protocol for download of genomic
  ranges for data files with accompanying index files.
@end itemize\n")
    (license license:asl2.0)))

(define-public python-pysam
  (package
    (name "python-pysam")
    (version "0.18.0")
    (source (origin
              (method git-fetch)
              ;; Test data is missing on PyPi.
              (uri (git-reference
                    (url "https://github.com/pysam-developers/pysam")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "042ca27r6634xg2ixgvq1079cp714wmm6ml7bwc1snn0wxxzywfg"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; FIXME: Unbundle samtools and bcftools.
                          (delete-file-recursively "htslib")))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-flags
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "HTSLIB_MODE" "external")
             (setenv "HTSLIB_LIBRARY_DIR"
                     (string-append (assoc-ref inputs "htslib") "/lib"))
             (setenv "HTSLIB_INCLUDE_DIR"
                     (string-append (assoc-ref inputs "htslib") "/include"))
             (setenv "LDFLAGS" "-lncurses")
             (setenv "CFLAGS" "-D_CURSES_LIB=1")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Step out of source dir so python does not import from CWD.
               (with-directory-excursion "tests"
                 (setenv "HOME" "/tmp")
                 (invoke "make" "-C" "pysam_data")
                 (invoke "make" "-C" "cbcf_data")
                 ;; The FileHTTP test requires network access.
                 (invoke "pytest" "-k" "not FileHTTP"))))))))
    (propagated-inputs
     (list htslib))                    ; Included from installed header files.
    (inputs
     (list ncurses curl zlib))
    (native-inputs
     (list python-cython
           python-pytest
           ;; Dependencies below are are for tests only.
           samtools
           bcftools))
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
     (list python-sphinx))
    (home-page "https://github.com/benjschiller/twobitreader")
    (synopsis "Python library for reading .2bit files")
    (description
     "twobitreader is a Python library for reading .2bit files as used by the
UCSC genome browser.")
    (license license:artistic2.0)))

(define-public python-plastid
  (package
    (name "python-plastid")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "plastid" version))
              (sha256
               (base32
                "1a7mdky2xw02y88l51f58pqk8039ahdp6sblj3zx58zarmy2pqyl"))))
    (build-system python-build-system)
    (arguments
     ;; Some test files are not included.
     `(#:tests? #f))
    (propagated-inputs
     (list python-numpy
           python-scipy
           python-pandas
           python-pysam
           python-matplotlib
           python-biopython
           python-twobitreader
           python-termcolor))
    (native-inputs
     (list python-cython python-nose))
    (home-page "https://github.com/joshuagryphon/plastid")
    (synopsis "Python library for genomic analysis")
    (description
     "plastid is a Python library for genomic analysis – in particular,
high-throughput sequencing data – with an emphasis on simplicity.")
    (license license:bsd-3)))

(define-public tetoolkit
  (package
    (name "tetoolkit")
    (version "2.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mhammell-laboratory/tetoolkit")
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
     (list coreutils
           bedtools
           python2-argparse
           python2-pysam
           r-minimal
           r-deseq2))
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
       ,#~(list (string-append "PREFIX=" #$output "/bin")
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
     (list perl))
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
    (version "2.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/YeoLab/clipper")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0508rgnfjk5ar5d1mjbjyrnarv4kw9ksq0m3jw2bmgabmb5v6ikk"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete pre-compiled files.
                  (delete-file "clipper/src/peaks.so")))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #false
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-python3-for-cython
           (lambda _
             (substitute* "setup.py"
               (("^setup")
                "\
peaks.cython_directives = {'language_level': '3'}
readsToWiggle.cython_directives = {'language_level': '3'}
setup"))))
         (add-after 'unpack 'disable-nondeterministic-test
           (lambda _
             ;; This test fails/succeeds non-deterministically.
             (substitute* "clipper/test/test_call_peak.py"
               (("test_get_FDR_cutoff_mean") "_test_get_FDR_cutoff_mean"))))
         ;; This doesn't work because "usage" is executed, and that calls
         ;; exit(8).
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (with-directory-excursion "clipper/test"
                 (invoke "python" "-m" "unittest")))))
         ;; This is not a library
         (delete 'sanity-check))))
    (inputs
     (list htseq
           python-pybedtools
           python-cython
           python-scikit-learn
           python-matplotlib
           python-pandas
           python-pysam
           python-numpy
           python-scipy))
    (native-inputs
     (list python-setuptools-git
           python-mock ; for tests
           python-nose ; for tests
           python-pytz)) ; for tests
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
    (inputs (list openmpi))
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
     (list argtable))
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
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "CrossMap" version))
              (sha256
               (base32
                "0hqminh5wn1p3x481jbyc7gmncp5xc196hpvki7k25vzbryhwcix"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete compiled Python files.
                  (for-each delete-file (find-files "." "\\.pyc$"))
                  (delete-file-recursively ".eggs")))))
    (build-system python-build-system)
    (inputs
     (list python-bx-python python-numpy python-pybigwig python-pysam
           zlib))
    (native-inputs
     (list python-cython python-nose))
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
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dnaio" version))
       (sha256
        (base32
         "14v5yyasq2bz34j38wi3xfcp06jj7l35ppibjcn95l2n73hz3zwi"))))
    (build-system python-build-system)
    (native-inputs
     (list python-cython python-pytest python-xopen))
    (home-page "https://github.com/marcelm/dnaio/")
    (synopsis "Read FASTA and FASTQ files efficiently")
    (description
     "dnaio is a Python library for fast parsing of FASTQ and also FASTA
files.  The code was previously part of the cutadapt tool.")
    (license license:expat)))

(define-public python-deeptoolsintervals
  (package
    (name "python-deeptoolsintervals")
    (version "0.1.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "deeptoolsintervals" version))
              (sha256
               (base32
                "1xnl80nblysj6dylj4683wgrfa425rkx4dp5k65hvwdns9pw753x"))))
    (build-system python-build-system)
    (inputs
     (list zlib))
    (home-page "https://github.com/deeptools/deeptools_intervals")
    (synopsis "Create GTF-based interval trees with associated meta-data")
    (description
     "This package provides a Python module creating/accessing GTF-based
interval trees with associated meta-data.  It is primarily used by the
@code{deeptools} package.")
    (license license:expat)))

(define-public python-deeptools
  (package
    (name "python-deeptools")
    (version "3.4.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/deeptools/deepTools")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0l09vyynz6s6w7fnyd94rpys4a6aja6kp4gli64pngdxdz3md1nl"))))
    (build-system python-build-system)
    (native-inputs
     (list python-mock python-nose))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-numpydoc
           python-py2bit
           python-pybigwig
           python-pysam
           python-scipy
           python-deeptoolsintervals
           python-plotly-2.4.1))
    (home-page "https://pypi.org/project/deepTools/")
    (synopsis "Useful tools for exploring deep sequencing data")
    (description "This package addresses the challenge of handling large amounts
of data that are now routinely generated from DNA sequencing centers.
@code{deepTools} contains useful modules to process the mapped reads data for
multiple quality checks, creating normalized coverage files in standard bedGraph
and bigWig file formats, that allow comparison between different files.  Finally,
using such normalized and standardized files, deepTools can create many
publication-ready visualizations to identify enrichments and for functional
annotations of the genome.")
    ;; The file deeptools/cm.py is licensed under the BSD license.  The
    ;; remainder of the code is licensed under the MIT license.
    (license (list license:bsd-3 license:expat))))

(define-deprecated deeptools python-deeptools)

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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'always-cythonize
           (lambda _
             (delete-file "src/cutadapt/_align.c")
             ;; If PKG-INFO exists, setup.py decides not to run Cython.
             (substitute* "setup.py"
               (("os.path.exists\\('PKG-INFO'\\):")
                "os.path.exists('totally-does-not-exist'):")))))))
    (inputs
     (list python-dnaio python-xopen))
    (native-inputs
     (list python-cython python-pytest python-setuptools-scm))
    (home-page "https://cutadapt.readthedocs.io/en/stable/")
    (synopsis "Remove adapter sequences from nucleotide sequencing reads")
    (description
     "Cutadapt finds and removes adapter sequences, primers, poly-A tails and
other types of unwanted sequence from high-throughput sequencing reads.")
    (license license:expat)))

(define-public libbigwig
  (package
    (name "libbigwig")
    (version "0.4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dpryan79/libBigWig")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09693dmf1scdac5pyq6qyn8b4mcipvnmc370k9a5z41z81m3dcsj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f ; tests require access to the web
       #:make-flags
       ,#~(list "CC=gcc"
                (string-append "prefix=" #$output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list zlib curl))
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
    (version "0.3.17")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyBigWig" version))
              (sha256
               (base32
                "157x6v48y299zm382krf1dw08fdxg95im8lnabhp5vc94s04zxj1"))
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
     (list python-numpy))
    (inputs
     (list libbigwig zlib curl))
    (home-page "https://github.com/dpryan79/pyBigWig")
    (synopsis "Access bigWig files in Python using libBigWig")
    (description
     "This package provides Python bindings to the libBigWig library for
accessing bigWig files.")
    (license license:expat)))

(define-public python2-pybigwig
  (package-with-python2 python-pybigwig))

(define-public python-schema-salad
  (package
    (name "python-schema-salad")
    (version "8.2.20211116214159")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "schema-salad" version))
        (sha256
         (base32
          "005dh2y45x92zl8sf2sqjmfvcqr4hrz8dfckgkckv87003v7lwqc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'skip-failing-tests
           (lambda _
             ;; Skip tests that require network access.
             (substitute* "schema_salad/tests/test_cwl11.py"
               (("^def test_(secondaryFiles|outputBinding)" all)
                (string-append "@pytest.mark.skip(reason="
                               "\"test requires network access\")\n"
                               all))))))))
    (propagated-inputs
     (list python-cachecontrol
           python-lockfile
           python-mistune
           python-rdflib
           python-rdflib-jsonld
           python-requests
           python-ruamel.yaml
           python-typing-extensions))
    (native-inputs
     (list python-black python-pytest python-pytest-runner))
    (home-page "https://github.com/common-workflow-language/schema_salad")
    (synopsis "Schema Annotations for Linked Avro Data (SALAD)")
    (description
     "Salad is a schema language for describing JSON or YAML structured linked
data documents.  Salad schema describes rules for preprocessing, structural
validation, and hyperlink checking for documents described by a Salad schema.
Salad supports rich data modeling with inheritance, template specialization,
object identifiers, object references, documentation generation, code
generation, and transformation to RDF.  Salad provides a bridge between document
and record oriented data modeling and the Semantic Web.")
    (license license:asl2.0)))

(define-public cwltool
  (package
    (name "cwltool")
    (version "3.1.20220119140128")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/common-workflow-language/cwltool")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jmrm0qrqgka79avc1kq63fgh20gx6g07fc8p3iih4k85vhdyl3f"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-version-restrictions
           (lambda _
             (substitute* "setup.py"
               (("== 1.5.1") ">=1.5.1")))) ; prov
         (add-after 'unpack 'dont-use-git
           (lambda _
             (substitute* "gittaggers.py"
               (("self.git_timestamp_tag\\(\\)")
                (string-append "time.strftime('.%Y%m%d%H%M%S', time.gmtime(int("
                               (string-drop ,version 4) ")))")))))
         (add-after 'unpack 'modify-tests
           (lambda _
             ;; Tries to connect to the internet.
             (delete-file "tests/test_content_type.py")
             (delete-file "tests/test_udocker.py")
             (delete-file "tests/test_http_input.py")
             (substitute* "tests/test_load_tool.py"
               (("def test_load_graph_fragment_from_packed")
                (string-append "@pytest.mark.skip(reason=\"Disabled by Guix\")\n"
                               "def test_load_graph_fragment_from_packed")))
             (substitute* "tests/test_examples.py"
               (("def test_env_filtering")
                (string-append "@pytest.mark.skip(reason=\"Disabled by Guix\")\n"
                               "def test_env_filtering")))
             ;; Tries to use cwl-runners.
             (substitute* "tests/test_examples.py"
               (("def test_v1_0_arg_empty_prefix_separate_false")
                (string-append "@pytest.mark.skip(reason=\"Disabled by Guix\")\n"
                               "def test_v1_0_arg_empty_prefix_separate_false")))

             (substitute* '("cwltool/schemas/v1.1/tests/env-tool1.cwl"
                            "cwltool/schemas/v1.1/tests/env-tool2.cwl"
                            "cwltool/schemas/v1.1/tests/imported-hint.cwl"
                            "tests/subgraph/env-tool2.cwl"
                            "tests/subgraph/env-tool2_req.cwl"
                            "tests/subgraph/env-wf2_subwf-packed.cwl"
                            "tests/subgraph/env-tool2_no_env.cwl")
               (("\"/bin/sh\"") (string-append "\"" (which "sh") "\"")))
             ;; Pytest doesn't know what to do with "-n auto"
             (substitute* "tox.ini"
               (("-n auto") "")))))))
    (propagated-inputs
     (list python-argcomplete
           python-bagit
           python-coloredlogs
           python-mypy-extensions
           python-prov
           python-pydot
           python-psutil
           python-rdflib
           python-requests
           python-ruamel.yaml
           python-schema-salad
           python-shellescape
           python-typing-extensions
           ;; Not listed as needed but still necessary:
           node))
    (native-inputs
     (list python-arcp
           python-humanfriendly
           python-mock
           python-pytest
           python-pytest-cov
           python-pytest-mock
           python-pytest-runner))
    (home-page
     "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis "Common Workflow Language reference implementation")
    (description
     "This is the reference implementation of the @acronym{CWL, Common Workflow
Language} standards.  The CWL open standards are for describing analysis
workflows and tools in a way that makes them portable and scalable across a
variety of software and hardware environments, from workstations to cluster,
cloud, and high performance computing (HPC) environments.  CWL is designed to
meet the needs of data-intensive science, such as Bioinformatics, Medical
Imaging, Astronomy, Physics, and Chemistry.  The @acronym{cwltool, CWL reference
implementation} is intended to be feature complete and to provide comprehensive
validation of CWL files as well as provide other tools related to working with
CWL descriptions.")
    (license license:asl2.0)))

(define-public python-dendropy
  (package
    (name "python-dendropy")
    (version "4.5.1")
    (source
     (origin
       (method git-fetch)
       ;; Source from GitHub so that tests are included.
       (uri (git-reference
             (url "https://github.com/jeetsukumaran/DendroPy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lrfzjqzbpk1rrra9vd7z2j7q09jy9w1ss7wn2rd85i4k5y3xz8l"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-broken-tests
           (lambda _
             ;; These tests fail because we have no "paup" executable.
             (substitute* "tests/test_datamodel_split_bitmasks.py"
               (((format #false "(~{~a~^|~})"
                         '("test_group1"
                           "test_basic_split_counting_under_different_rootings"
                           "test_basic_split_count_with_incorrect_weight_treatment_raises_error"
                           "test_basic_split_count_with_incorrect_rootings_raises_error")) m)
                (string-append "_skip_" m)))
             (delete-file "tests/test_paup.py")
             (delete-file "tests/test_dataio_nexml_reader_tree_list.py")
             ;; Assert error for unknown reasons
             (substitute* "tests/test_protractedspeciation.py"
               (("test_by_num_lineages" m)
                (string-append "_skip_" m))))))))
    (home-page "https://dendropy.org/")
    (synopsis "Library for phylogenetics and phylogenetic computing")
    (description
     "DendroPy is a library for phylogenetics and phylogenetic computing: reading,
writing, simulation, processing and manipulation of phylogenetic
trees (phylogenies) and characters.")
    (license license:bsd-3)))

(define-public python2-dendropy
  (let ((base (package-with-python2 python-dendropy)))
    (package/inherit base
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

(define-public delly
  (package
    (name "delly")
    (version "0.8.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dellytools/delly")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1ibnplgfzj96w8glkx17v7sld3pm402fr5ybmf3h0rlcryabxrqy"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "src/htslib")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests to run.
       #:make-flags
       ,#~(list "PARALLEL=1"           ; Allow parallel execution at run-time.
                (string-append "prefix=" #$output))
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
     (list boost bzip2 htslib zlib))
    (home-page "https://github.com/dellytools/delly")
    (synopsis "Integrated structural variant prediction method")
    (description "Delly is an integrated structural variant prediction method
that can discover and genotype deletions, tandem duplications, inversions and
translocations at single-nucleotide resolution in short-read massively parallel
sequencing data.  It uses paired-ends and split-reads to sensitively and
accurately delineate genomic rearrangements throughout the genome.")
    (license license:gpl3+)))

(define-public trf
  (package
    (name "trf")
    (version "4.09.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Benson-Genomics-Lab/TRF")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0fhwr4s1mf8nw8fr5imwjvjr42b59p97zr961ifm8xl1bajz4wpg"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/Benson-Genomics-Lab/TRF")
    (synopsis "Tandem Repeats Finder: a program to analyze DNA sequences")
    (description "A tandem repeat in DNA is two or more adjacent, approximate
copies of a pattern of nucleotides.  Tandem Repeats Finder is a program to
locate and display tandem repeats in DNA sequences.  In order to use the
program, the user submits a sequence in FASTA format.  The output consists of
two files: a repeat table file and an alignment file.  Submitted sequences may
be of arbitrary length. Repeats with pattern size in the range from 1 to 2000
bases are detected.")
    (license license:agpl3+)))

(define-public repeat-masker
  (package
    (name "repeat-masker")
    (version "4.1.2-p1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.repeatmasker.org/"
                                  "RepeatMasker/RepeatMasker-"
                                  version ".tar.gz"))
              (sha256
               (base32 "15hfdfpzmdjcx7ng7rjfid69bmvgn3z9g9r43qhjnhjhq3v4prab"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #false ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out")
                                         "/share/RepeatMasker")))
               (mkdir-p share)
               (copy-recursively "." share)
               (with-directory-excursion share
                 (invoke "perl" "configure"
                         "--trf_prgm" (which "trf")
                         "--hmmer_dir"
                         (string-append (assoc-ref inputs "hmmer")
                                        "/bin"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share/RepeatMasker"))
                    (bin   (string-append out "/bin"))
                    (path  (getenv "PERL5LIB")))
               (install-file (string-append share "/RepeatMasker") bin)
               (wrap-program (string-append bin "/RepeatMasker")
                 `("PERL5LIB" ":" prefix (,path ,share)))))))))
    (inputs
     (list perl
           perl-text-soundex
           python
           python-h5py
           hmmer
           trf))
    (home-page "https://github.com/Benson-Genomics-Lab/TRF")
    (synopsis "Tandem Repeats Finder: a program to analyze DNA sequences")
    (description "A tandem repeat in DNA is two or more adjacent, approximate
copies of a pattern of nucleotides.  Tandem Repeats Finder is a program to
locate and display tandem repeats in DNA sequences.  In order to use the
program, the user submits a sequence in FASTA format.  The output consists of
two files: a repeat table file and an alignment file.  Submitted sequences may
be of arbitrary length. Repeats with pattern size in the range from 1 to 2000
bases are detected.")
    (license license:osl2.1)))

(define-public diamond
  (package
    (name "diamond")
    (version "0.9.30")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bbuchfink/diamond")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0k6f3kb6cniw11xw6763kkbs1sl0yack7xsy7q5fl5v170ssphq4"))))
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
     (list zlib))
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
             (url "https://github.com/maaskola/discrover")
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
               (("theverbbox\\[t\\]") "theverbbox"))))
         (add-after 'unpack 'add-missing-includes
           (lambda _
             (substitute* "src/executioninformation.hpp"
               (("#define EXECUTIONINFORMATION_HPP" line)
                (string-append line "\n#include <random>")))
             (substitute* "src/plasma/fasta.hpp"
               (("#define FASTA_HPP" line)
                (string-append line "\n#include <random>"))))))))
    (inputs
     (list boost cairo rmath-standalone))
    (native-inputs
     `(("texlive" ,(texlive-updmap.cfg (list texlive-cm
                                             texlive-amsfonts
                                             texlive-doi
                                             texlive-fonts-ec
                                             texlive-latex-examplep
                                             texlive-hyperref
                                             texlive-latex-ms
                                             texlive-latex-natbib
                                             texlive-bibtex ; style files used by natbib
                                             texlive-latex-pgf ; tikz
                                             texlive-latex-verbatimbox)))
       ("imagemagick" ,imagemagick)))
    (home-page "https://dorina.mdc-berlin.de/public/rajewsky/discrover/")
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
             (url "https://github.com/DReichLab/EIG")
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
     (list gsl lapack openblas perl
           `(,gfortran "lib")))
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
    (version "13.3.20200128")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.ncbi.nlm.nih.gov/entrez/entrezdirect"
                                  "/versions/" version
                                  "/edirect-" version ".tar.gz"))
              (sha256
               (base32
                "093zp7klv81ph0y8mm8d78a9hnpfxbv2kdym70gzdf3vz176rw33"))
              (modules '((guix build utils)))
              (snippet
               '(begin (delete-file "Mozilla-CA.tar.gz")
                       (substitute* "rchive.go"
                         ;; This go library does not have any license.
                         (("github.com/fiam/gounidecode/unidecode")
                          "golang.org/rainycape/unidecode"))
                       #t))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)                ; simple check after install
         (add-after 'unpack 'patch-programs
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Ignore errors about missing xtract.Linux and rchive.Linux.
              (substitute* "pm-refresh"
                (("cat \\\"\\$target")
                 "grep ^[[:digit:]] \"$target"))
              #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (edirect-go (assoc-ref inputs "edirect-go-programs")))
               (for-each
                 (lambda (file)
                   (install-file file bin))
                 '("archive-pubmed" "asp-cp" "asp-ls" "download-ncbi-data"
                   "download-pubmed" "edirect.pl" "efetch" "epost" "esearch"
                   "fetch-pubmed" "ftp-cp" "ftp-ls" "has-asp" "index-pubmed"
                   "pm-prepare" "pm-refresh" "pm-stash" "pm-collect"
                   "pm-index" "pm-invert" "pm-merge" "pm-promote"))
               (symlink (string-append edirect-go "/bin/xtract.Linux")
                        (string-append bin "/xtract"))
               (symlink (string-append edirect-go "/bin/rchive.Linux")
                        (string-append bin "/rchive")))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
              ;; Make sure everything can run in a pure environment.
              (let ((out (assoc-ref outputs "out"))
                    (path (getenv "PERL5LIB")))
                (for-each
                  (lambda (file)
                    (wrap-program file
                      `("PERL5LIB" ":" prefix (,path)))
                    (wrap-program file
                      `("PATH" ":" prefix (,(string-append out "/bin")
                                           ,(dirname (which "sed"))
                                           ,(dirname (which "gzip"))
                                           ,(dirname (which "grep"))
                                           ,(dirname (which "perl"))
                                           ,(dirname (which "uname"))))))
                  (find-files out ".")))
              #t))
         (add-after 'wrap-program 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke (string-append (assoc-ref outputs "out")
                                    "/bin/edirect.pl")
                     "-filter" "-help")
             #t)))))
    (inputs
     (list edirect-go-programs
           perl-html-parser
           perl-encode-locale
           perl-file-listing
           perl-html-tagset
           perl-html-tree
           perl-http-cookies
           perl-http-date
           perl-http-message
           perl-http-negotiate
           perl-lwp-mediatypes
           perl-lwp-protocol-https
           perl-net-http
           perl-uri
           perl-www-robotrules
           perl-xml-simple
           perl))
    (home-page "https://www.ncbi.nlm.nih.gov/books/NBK179288/")
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
    (native-search-paths
     ;; Ideally this should be set for LWP somewhere.
     (list (search-path-specification
            (variable "PERL_LWP_SSL_CA_FILE")
            (file-type 'regular)
            (separator #f)
            (files '("/etc/ssl/certs/ca-certificates.crt")))))
    (license license:public-domain)))

(define-public edirect-go-programs
  (package
    (inherit edirect)
    (name "edirect-go-programs")
    (build-system go-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f      ; No tests.
       #:import-path "ncbi.nlm.nih.gov/entrez/edirect"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (invoke "go" "build" "-v" "-x" "j2x.go")
               (invoke "go" "build" "-v" "-x" "t2x.go")
               (invoke "go" "build" "-v" "-x" "-o"
                       "xtract.Linux" "xtract.go" "common.go")
               (invoke "go" "build" "-v" "-x" "-o"
                       "rchive.Linux" "rchive.go" "common.go")
               (invoke "go" "build" "-v" "-x" "-o" "symbols.Linux" "s2p.go"))))
         (replace 'install
           (lambda* (#:key outputs import-path #:allow-other-keys)
             (let ((dest    (string-append (assoc-ref outputs "out") "/bin"))
                   (source  (string-append "src/" import-path "/")))
               (for-each (lambda (file)
                           (format #t "installing ~a~%" file)
                           (install-file (string-append source file) dest))
                         '("j2x" "t2x" "symbols.Linux" "xtract.Linux" "rchive.Linux"))
               #t))))))
    (native-inputs '())
    (propagated-inputs '())
    (inputs
     (list go-github-com-fatih-color
           go-github-com-fogleman-gg
           go-github-com-gedex-inflector
           go-github-com-golang-freetype
           go-github-com-klauspost-cpuid
           go-github-com-pbnjay-memory
           go-github-com-surgebase-porter2
           go-golang-org-rainycape-unidecode
           go-golang-org-x-image
           go-golang-org-x-text))))

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
     (list pkg-config))
    (inputs
     (list glib))
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
    (version "1.5.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/adarob/eXpress")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18nb22n7x820fzjngf4qgyb3mspqkw7xyk7v7s5ps6wfrd8qwscb"))))
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
             #t))
         (add-after 'unpack 'remove-update-check
           (lambda _
             (substitute* "src/main.cpp"
               (("#include \"update_check.h\"") "")
               (("check_version\\(PACKAGE_VERSION\\);") ""))
             #t)))))
    (inputs
     (list boost bamtools protobuf zlib))
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
                   (url "https://github.com/dparks1134/ExpressBetaDiversity")
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
   (home-page "https://github.com/dparks1134/ExpressBetaDiversity")
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
     (list libgtextutils))
    (native-inputs
     (list gcc-6 ;; doesn't build with later versions
           pkg-config))
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
                    (url "https://github.com/seqan/flexbar")
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
               ((" -march=native") ""))))
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "PATH" (string-append (getcwd) ":" (getenv "PATH")))
             (with-directory-excursion "../source/test"
               (invoke "bash" "flexbar_test.sh"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (string-append (assoc-ref outputs "out")))
                    (bin (string-append out "/bin/")))
               (install-file "flexbar" bin)))))))
    (inputs
     (list tbb-2020 zlib))
    (native-inputs
     (list pkg-config seqan-2))
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
               (url "https://github.com/ctSkennerton/fxtract")
               (commit version)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0hab3gpwf4w9s87qlbswq6ws1qqybh4dcqk79q1ahyldzai5fgp5"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags ,#~(list
                          (string-append "PREFIX=" #$output)
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
       (list pcre zlib))
      (native-inputs
       ;; ctskennerton-util is licensed under GPL2.
       `(("ctskennerton-util"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/ctSkennerton/util")
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
    (version "0.98.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/genetics-statistics/GEMMA")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p8a7kkfn1mmrg017aziy544aha8i9h6wd1x2dk3w2794wl33qb7"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "contrib")
                  #t))))
    (build-system gnu-build-system)
    (inputs
     (list gsl openblas zlib))
    (native-inputs
     `(("catch" ,catch-framework2-1)
       ("perl" ,perl)
       ("shunit2" ,shunit2)
       ("which" ,which)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'prepare-build
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "bin")
             (substitute* "Makefile"
               (("/usr/local/opt/openblas")
                (assoc-ref inputs "openblas")))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; 'make slow-check' expects shunit2-2.0.3.
               (with-directory-excursion "test"
                 (invoke "./test_suite.sh"))
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "bin/gemma"
                           (string-append (assoc-ref outputs "out") "/bin"))
             #t)))))
    (home-page "https://github.com/genetics-statistics/GEMMA")
    (synopsis "Tool for genome-wide efficient mixed model association")
    (description
     "@acronym{GEMMA, Genome-wide Efficient Mixed Model Association} provides a
standard linear mixed model resolver with application in @acronym{GWAS,
genome-wide association studies}.")
    (license license:gpl3)))

(define-public grit
  (package
    (name "grit")
    (version "2.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nboley/grit")
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
     (list python2-scipy python2-numpy python2-pysam python2-networkx))
    (native-inputs
     (list python2-cython))
    ;; The canonical <http://grit-bio.org> home page times out as of 2020-01-21.
    (home-page "https://github.com/nboley/grit")
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
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ccb.jhu.edu/software/hisat/downloads/hisat-"
                    version "-beta-source.zip"))
              (sha256
               (base32
                "177z85vqp9b30vgxl5py5hz4mm37ila37nzhfam23ci9iyfxgyv9"))))
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
             ;; This "extended character" is not considered valid.
             (substitute* "processor_support.h"
               (("“") "\"")
               (("”") "\""))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files
                          "."
                          "hisat(-(build|align|inspect)(-(s|l)(-debug)*)*)*$")))))
         (delete 'configure))))
    (native-inputs
     (list unzip))
    (inputs
     (list perl python zlib))
    ;; Non-portable SSE instructions are used so building fails on platforms
    ;; other than x86_64.
    (supported-systems '("x86_64-linux"))
    (home-page "https://ccb.jhu.edu/software/hisat/index.shtml")
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
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DaehwanKimLab/hisat2/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lmzdhzjkvxw7n5w40pbv5fgzd4cz0f9pxczswn3d4cr0k10k754"))))
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
               (("`date`") "0"))))
         (delete 'configure)
         (add-before 'build 'build-manual
           (lambda _
             (mkdir-p "doc")
             (invoke "make" "doc")))
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
               (install-file "doc/manual.inc.html" doc)))))))
    (native-inputs
     (list perl pandoc))             ; for documentation
    (inputs
     `(("python" ,python-wrapper)))
    (home-page "https://daehwankimlab.github.io/hisat2/")
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
    (version "3.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://eddylab.org/software/hmmer/hmmer-" version ".tar.gz"))
       (sha256
        (base32
         "0s9wf6n0qanbx8qs6igfl3vyjikwbrvh4d9d6mv54yp3xysykzlj"))))
    (build-system gnu-build-system)
    (native-inputs (list perl python)) ; for tests
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
    (version "0.12.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "HTSeq" version))
              (sha256
               (base32
                "0pk41vkzxsbb5nv644325mh8akmz4zdply9r2s80dgg5b21pgp0b"))))
    (build-system python-build-system)
    (native-inputs
     (list python-cython))
    ;; Numpy needs to be propagated when htseq is used as a Python library.
    (propagated-inputs
     (list python-numpy))
    (inputs
     (list python-pysam python-matplotlib))
    (home-page "https://htseq.readthedocs.io/")
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
                    (url "https://github.com/samtools/htsjdk")
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
       ,#~(list (string-append "-Ddist=" #$output "/share/java/htsjdk/"))
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
     (list java-testng))
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
                    (url "https://github.com/samtools/htsjdk")
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
     (list java-junit))
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
                    (url "https://github.com/samtools/htsjdk")
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
                    (url "https://github.com/broadinstitute/picard")
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
       ,#~(list (string-append "-Dhtsjdk_lib_dir="
                               #$(this-package-input "java-htsjdk")
                               "/share/java/htsjdk/")
                "-Dhtsjdk-classes=dist/tmp"
                (string-append "-Dhtsjdk-version="
                               #$(package-version java-htsjdk)))
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         ;; FIXME: this phase fails with "duplicate entry: htsjdk/samtools/AbstractBAMFileIndex$1.class"
         (delete 'generate-jar-indices)
         (add-after 'unpack 'use-our-htsjdk
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("\\$\\{htsjdk\\}/lib")
                (search-input-directory inputs
                                        "share/java/htsjdk")))))
         (add-after 'unpack 'make-test-target-independent
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("name=\"test\" depends=\"compile, ")
                "name=\"test\" depends=\""))
             #t))
         (replace 'install (install-jars "dist")))))
    (inputs
     (list java-htsjdk java-guava))
    (native-inputs
     (list java-testng))
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
                    (url "https://github.com/broadinstitute/picard")
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
     (list java-htsjdk-2.10.1))
    (native-inputs
     (list java-testng java-guava))
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
                    (url "https://github.com/broadinstitute/picard")
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
       #:ant ,ant/java8
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
                (search-input-file inputs "/lib/ant.jar")))))
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
       ("ant" ,ant/java8) ; for bzip2 support at runtime
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
                (search-input-file inputs "/share/java/jbzip2.jar"))
               (("sam-1.103.jar")
                (search-input-file inputs
                                   "/share/java/sam-1.112.jar"))
               (("cisd-jhdf5.jar")
                (search-input-file inputs
                                   "/share/java/sis-jhdf5.jar")))))
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
     (list unzip))
    (home-page "https://www.bioinformatics.babraham.ac.uk/projects/fastqc/")
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
    (version "0.20.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenGene/fastp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ly8mxdvrcy23jwxyppysx3dhb1lwsqhfbgpyvargxhfk6k700x4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are none
       #:make-flags
       ,#~(list (string-append "PREFIX=" #$output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'create-target-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/bin")))))))
    (inputs
     (list zlib))
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
    (version "1.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "0pwk8yhhvb85mi1d2qhwsb4samc3rmbcrq7b1s0jz0glaa7in8pd"))))
    (build-system gnu-build-system)
    ;; Let htslib translate "gs://" and "s3://" to regular https links with
    ;; "--enable-gcs" and "--enable-s3". For these options to work, we also
    ;; need to set "--enable-libcurl".
    (arguments
     `(#:configure-flags '("--enable-gcs"
                           "--enable-libcurl"
                           "--enable-s3")))
    (inputs
     (list curl openssl))
    ;; This is referred to in the pkg-config file as a required library.
    (propagated-inputs
     (list zlib))
    (native-inputs
     (list perl))
    (home-page "https://www.htslib.org")
    (synopsis "C library for reading/writing high-throughput sequencing data")
    (description
     "HTSlib is a C library for reading/writing high-throughput sequencing
data.  It also provides the @command{bgzip}, @command{htsfile}, and
@command{tabix} utilities.")
    ;; Files under cram/ are released under the modified BSD license;
    ;; the rest is released under the Expat license
    (license (list license:expat license:bsd-3))))

(define-public htslib-1.12
  (package/inherit htslib
    (version "1.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "1jplnvizgr0fyyvvmkfmnsywrrpqhid3760vw15bllz98qdi9012"))))))

(define-public htslib-1.10
  (package/inherit htslib
    (version "1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "0wm9ay7qgypj3mwx9zl1mrpnr36298b1aj5vx69l4k7bzbclvr3s"))))))

(define-public htslib-1.9
  (package/inherit htslib
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "16ljv43sc3fxmv63w7b2ff8m1s7h89xhazwmbm1bicz8axq8fjz0"))))))

;; This package should be removed once no packages rely upon it.
(define htslib-1.3
  (package/inherit htslib
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "1rja282fwdc25ql6izkhdyh8ppw8x2fs0w0js78zgkmqjlikmma9"))))))

(define htslib-for-samtools-1.2
  (package/inherit htslib
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "1c32ssscbnjwfw3dra140fq7riarp2x990qxybh34nr1p5r17nxx"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "test/test.pl"
               (("/bin/bash") (which "bash"))))))))
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)))))

(define htslib-for-stringtie
  (package
    (inherit htslib)
    (source (origin
              (inherit (package-source htslib))
              (patches
               (search-patches "htslib-for-stringtie.patch"))))
    (arguments
     `(#:configure-flags '("--with-libdeflate")))
    (inputs
     (list bzip2 libdeflate openssl))))

(define-public idr
  (package
    (name "idr")
    (version "2.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nboley/idr")
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
     (list python-scipy python-sympy python-numpy python-matplotlib))
    (native-inputs
     (list python-cython))
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
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gmarcais/Jellyfish/"
                                  "releases/download/v" version
                                  "/jellyfish-" version ".tar.gz"))
              (sha256
               (base32
                "0npa62wzasdibas5zp3n8j3armsci4kyvh0jw7jr0am4gg7vg5g1"))))
    (build-system gnu-build-system)
    (outputs '("out"      ;for library
               "python")) ;for Python bindings
    (arguments
     `(#:configure-flags
       ,#~(list "--without-sse" ; configure script probes for CPU features when SSE is enabled.
                (string-append "--enable-python-binding=" #$output:python))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-SHELL-variable
           (lambda _
             ;; generator_manager.hpp either uses /bin/sh or $SHELL
             ;; to run tests.
             (setenv "SHELL" (which "bash")))))))
    (native-inputs
     `(("bc" ,bc)
       ("time" ,time)
       ("python" ,python-wrapper)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list htslib))
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
    ;; One of these licenses may be picked
    (license (list license:gpl3+ license:bsd-3))))

(define-public khmer
  (package
    (name "khmer")
    (version "3.0.0a3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dib-lab/khmer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01l4jczglkl7yfhgvzx8j0df7k54bk1r8sli9ll16i1mis0d8f37"))
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
           (delete-file-recursively "third-party/seqan")
           (substitute* "setup.cfg"
             (("# libraries = z,bz2")
              "libraries = z,bz2")
             (("include:third-party/zlib:third-party/bzip2")
              "include:"))
           ;; Delete generated Cython CPP files.
           (for-each delete-file (find-files "khmer/_oxli/" "\\.cpp$"))))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-cc
           (lambda _ (setenv "CC" "gcc")))
         (add-after 'unpack 'python-3.8-compatibility
           (lambda _
             ;; Python 3.8 removed time.clock().
             (substitute* "sandbox/sweep-reads.py"
               (("time\\.clock")
                "time.process_time"))))
         (add-after 'unpack 'do-use-cython
           (lambda _
             (substitute* "setup.py"
               (("from setuptools import Extension as CyExtension")
                "from Cython.Distutils import Extension as CyExtension")
               (("from setuptools.command.build_ext import build_ext as _build_ext")
                "from Cython.Distutils import build_ext as _build_ext")
               (("HAS_CYTHON = False") "HAS_CYTHON = True")
               (("cy_ext = 'cpp'") "cy_ext = 'pyx'"))))
         (add-before 'build 'build-extensions
           (lambda _
             ;; Cython extensions have to be built before running the tests.
             (invoke "python" "setup.py" "build_ext" "--inplace")))
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-v")))))))
    (native-inputs
     (list python-cython python-pytest python-pytest-runner))
    (inputs
     (list zlib bzip2 seqan-1 python-screed python-bz2file))
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
     (list perl zlib))
    (home-page "http://kaiju.binf.ku.dk/")
    (synopsis "Fast and sensitive taxonomic classification for metagenomics")
    (description "Kaiju is a program for sensitive taxonomic classification
of high-throughput sequencing reads from metagenomic whole genome sequencing
experiments.")
    (license license:gpl3+)))

(define-public macs
  (package
    (name "macs")
    (version "2.2.7.1")
    (source (origin
              ;; The PyPi tarball does not contain tests.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/macs3-project/MACS")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08zsgh65xbpv1md2s3wqmrk9g2mz6izmn59ryw5lbac54120p291"))
        (modules '((guix build utils)))
        ;; Remove files generated by Cython
        (snippet
         '(begin
            (for-each (lambda (file)
                        (let ((generated-file
                                (string-append (string-drop-right file 3) "c")))
                          (when (file-exists? generated-file)
                            (delete-file generated-file))))
                      (find-files "." "\\.pyx$"))
            (delete-file "MACS2/IO/CallPeakUnitPrecompiled.c")))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-HOME
           (lambda _ (setenv "HOME" "/tmp")))
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-v")))))))
    (inputs
     (list python-numpy))
    (native-inputs
     (list python-cython python-pytest))
    (home-page "https://github.com/macs3-project/MACS")
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
    (version "7.475")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://mafft.cbrc.jp/alignment/software/mafft-" version
                    "-without-extensions-src.tgz"))
              (file-name (string-append name "-" version ".tgz"))
              (sha256
               (base32
                "0i2i2m3blh2xkbkdk48hxfssks30ny0v381gdl7zwhcvp0axs26r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no automated tests, though there are tests in the read me
       #:make-flags ,#~(list (string-append "PREFIX=" #$output)
                             (string-append "BINDIR="
                                            (string-append #$output "/bin")))
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
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (path (string-append
                           (assoc-ref inputs "coreutils") "/bin:")))
               (for-each (lambda (file)
                           (wrap-program file
                             `("PATH" ":" prefix (,path))))
                         (find-files bin))))))))
    (inputs
     (list perl ruby gawk grep coreutils))
    (home-page "https://mafft.cbrc.jp/alignment/software/")
    (synopsis "Multiple sequence alignment program")
    (description
     "MAFFT offers a range of multiple alignment methods for nucleotide and
protein sequences.  For instance, it offers L-INS-i (accurate; for alignment
of <~200 sequences) and FFT-NS-2 (fast; for alignment of <~30,000
sequences).")
    (license (license:non-copyleft
              "https://mafft.cbrc.jp/alignment/software/license.txt"
              "BSD-3 with different formatting"))))

(define-public mash
  (package
    (name "mash")
    (version "2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/marbl/mash")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "049hwcc059p2fd9vwndn63laifvvsi0wmv84i6y1fr79k15dxwy6"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled kseq.
               ;; TODO: Also delete bundled murmurhash and open bloom filter.
               '(delete-file "src/mash/kseq.h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests.
       #:configure-flags
       ,#~(list
           (string-append "--with-capnp=" #$(this-package-input "capnproto"))
           (string-append "--with-gsl=" #$(this-package-input "gsl")))
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
     (list autoconf))
    (inputs
     (list ;; Capnproto and htslib are statically embedded in the final
           ;; application. Therefore we also list their licenses, below.
           capnproto
           htslib
           gsl
           zlib))
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
    (version "2.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://bitbucket.org/berkeleylab/metabat.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0v3gsps0ypani14102z2y1a2wignhpf7s1h45mxmj5f783rkhqd9"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       ,#~(list (string-append "-Dzlib_LIB=" #$(this-package-input "zlib")
                               "/lib/libz.so")
                (string-append "-Dhtslib_LIB=" #$(this-package-input "htslib")
                               "/lib/libhts.so")
                (string-append "-DBOOST_ROOT=" #$(this-package-input "boost")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure-version-file
           (lambda _
             (copy-file "metabat_version.h.in" "metabat_version.h")
             (substitute* "metabat_version.h"
               (("@_time_stamp@") "19700101")
               (("@GIT_IS_DIRTY@") "0")
               (("@GIT_RETRIEVED_STATE@") "0")
               (("@GIT_HEAD_SHA1@") (string-append "v" ,version)))))
         (add-after 'unpack 'do-not-use-bundled-libraries
           (lambda _
             (substitute* "CMakeLists.txt"
               (("include\\(cmake.*") ""))
             (substitute* "src/CMakeLists.txt"
               (("set\\(Boost.*") "")
               (("add_dependencies.*") "")))))))
    (inputs
     (list zlib perl samtools htslib boost))
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
                    (url "https://github.com/ctSkennerton/minced")
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
  (let ((commit "b71402188000465e3430736a11ea118fd5639a4a")
        (revision "1"))
    (package
      (name "miso")
      (version (git-version "0.5.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yarden/MISO/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0x37ipwwvpxbkrg17gmq3hp92c9cphch8acd6cj7fqgnrjwd47g5"))
                (modules '((guix build utils)))
                (snippet
                 '(substitute* "setup.py"
                    ;; Use "gcc" instead of "cc" for compilation.
                    (("^defines")
                     "cc.set_executables(
compiler='gcc',
compiler_so='gcc',
linker_exe='gcc',
linker_so='gcc -shared'); defines")))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2               ; only Python 2 is supported
         #:tests? #f))                    ; no "test" target
      (inputs
       ;; Samtools must not be newer than 1.2.  See
       ;; https://github.com/yarden/MISO/issues/135
       (list samtools-1.2 python2-numpy python2-pysam python2-scipy
             python2-matplotlib))
      (native-inputs
       (list python2-mock ; for tests
             python2-pytz))  ; for tests
      (home-page "https://miso.readthedocs.io/en/fastmiso/")
      (synopsis "Mixture of Isoforms model for RNA-Seq isoform quantitation")
      (description
       "MISO (Mixture-of-Isoforms) is a probabilistic framework that quantitates
the expression level of alternatively spliced genes from RNA-Seq data, and
identifies differentially regulated isoforms or exons across samples.  By
modeling the generative process by which reads are produced from isoforms in
RNA-Seq, the MISO model uses Bayesian inference to compute the probability
that a read originated from a particular isoform.")
      (license license:gpl2))))

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
                      (url "https://github.com/tjunier/newick_utils")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1hkw21rq1mwf7xp0rmbb2gqc0i6p11108m69i7mr7xcjl268pxnb"))))
      (build-system gnu-build-system)
      (arguments
       '(#:make-flags (list "CFLAGS=-O2 -g -fcommon")))
      (inputs
       ;; XXX: TODO: Enable Lua and Guile bindings.
       ;; https://github.com/tjunier/newick_utils/issues/13
       (list libxml2 flex bison))
      (native-inputs
       (list autoconf automake libtool))
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
    (inputs (list zlib))
    (native-inputs
     (list ruby-bio-commandeer ruby-rspec ruby))
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
    (arguments
     `(#:python ,python-2               ;pbcore < 2.0 requires Python 2.7
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'remove-sphinx-dependency
                    (lambda _
                      ;; Sphinx is only required for documentation tests, which
                      ;; we do not run; furthermore it depends on python2-sphinx
                      ;; which is no longer maintained.
                      (substitute* "requirements-dev.txt"
                        (("^sphinx") ""))
                      #t)))))
    (propagated-inputs
     (list python2-cython python2-numpy python2-pysam python2-h5py))
    (native-inputs
     (list python2-nose python2-pyxb))
    (home-page "https://pacificbiosciences.github.io/pbcore/")
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
     `(#:python ,python-2  ; requires Python 2.7
       #:tests? #f ; test data are not included
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-weave
           (lambda _
             (substitute* "warpedlmm/util/linalg.py"
               (("from scipy import linalg, weave")
                "from scipy import linalg\nimport weave"))
             #t)))))
    (propagated-inputs
     (list python2-scipy
           python2-numpy
           python2-matplotlib
           python2-fastlmm
           python2-pandas
           python2-pysnptools
           python2-weave))
    (native-inputs
     (list python2-mock python2-nose unzip))
    (home-page "https://github.com/PMBio/warpedLMM")
    (synopsis "Implementation of warped linear mixed models")
    (description
     "WarpedLMM is a Python implementation of the warped linear mixed model,
which automatically learns an optimal warping function (or transformation) for
the phenotype as it models the data.")
    (license license:asl2.0)))

(define-public prank
  (package
    (name "prank")
    (version "170427")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://wasabiapp.org/download/prank/prank.source."
                    version ".tgz"))
              (sha256
               (base32
                "0nc8g9c5rkdxcir46s0in9ci1sxwzbjibxrvkksf22ybnplvagk2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-src-dir
           (lambda _ (chdir "src")))
         (add-after 'unpack 'remove-m64-flag
           ;; Prank will build with the correct 'bit-ness' without this flag
           ;; and this allows building on 32-bit machines.
           (lambda _
             (substitute* "src/Makefile"
               (("-m64") ""))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1"))
                    (path (string-append
                           (assoc-ref inputs "mafft") "/bin:"
                           (assoc-ref inputs "exonerate") "/bin:"
                           (assoc-ref inputs "bppsuite") "/bin")))
               (install-file "prank" bin)
               (wrap-program (string-append bin "/prank")
                 `("PATH" ":" prefix (,path)))
               (install-file "prank.1" man)))))))
    (inputs
     (list mafft exonerate bppsuite))
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
    (version "6.0.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/paulklemm_PHD/proteinortho.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pmy617zy2z2w6hjqxjhf3rzikf5n3mpia80ysq8233vfr7wrzff"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; remove pre-built scripts
                  (delete-file-recursively "src/BUILD/")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags '("CC=gcc")
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
             (let ((path (getenv "PATH"))
                   (out (assoc-ref outputs "out"))
                   (guile (search-input-file inputs "bin/guile")))
               (for-each (lambda (script)
                           (wrap-script script #:guile guile
                                        `("PATH" ":" prefix (,path))))
                         (cons (string-append out "/bin/proteinortho")
                               (find-files out "\\.(pl|py)$"))))
             #t)))))
    (inputs
     `(("guile" ,guile-3.0) ; for wrap-script
       ("diamond" ,diamond)
       ("perl" ,perl)
       ("python" ,python-wrapper)
       ("blast+" ,blast+)
       ("lapack" ,lapack)
       ("openblas" ,openblas)))
    (native-inputs
     (list which))
    (home-page "http://www.bioinf.uni-leipzig.de/Software/proteinortho")
    (synopsis "Detect orthologous genes across species")
    (description
     "Proteinortho is a tool to detect orthologous genes across different
species.  For doing so, it compares similarities of given gene sequences and
clusters them to find significant groups.  The algorithm was designed to handle
large-scale data and can be applied to hundreds of species at once.")
    (license license:gpl3+)))

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
     (list python2-matplotlib))
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
    ;; Check for a new home page when updating this package:
    ;; https://github.com/hyattpd/Prodigal/issues/36#issuecomment-536617588
    (version "2.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyattpd/Prodigal")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fs1hqk83qjbjhrvhw6ni75zakx5ki1ayy3v6wwkn3xvahc9hi5s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags
       ,#~(list (string-append "INSTALLDIR=" #$output "/bin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/hyattpd/Prodigal")
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
                      (dirname (search-input-file inputs "bin/chmod"))))
                 (wrap-program file
                   `("R_LIBS_SITE" ":" prefix
                     (,(string-append r-site-lib ":" out "/site-library/"))))
                 (wrap-program file
                   `("PATH" ":" prefix
                     (,(string-append coreutils-path ":" out "/bin"))))))
             #t)))))
    (native-inputs
     (list perl-env-path perl-test-files perl-test-most perl-test-output))
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
    (home-page "https://sanger-pathogens.github.io/Roary/")
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
             (url "https://github.com/stamatak/standard-RAxML")
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
     (list openmpi))
    (home-page "https://cme.h-its.org/exelixis/web/software/raxml/index.html")
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
    (version "1.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deweylab/RSEM")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1yl4i7z20n2p84j1lmk15aiak3yqc6fiw0q5a4pndw7pxfiq3rzp"))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; remove bundled copy of boost and samtools
           (delete-file-recursively "boost")
           (delete-file-recursively "samtools-1.3")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:make-flags
       ,#~(list (string-append "BOOST="
                               #$(this-package-input "boost")
                               "/include/")
                (string-append "SAMHEADERS="
                               #$(this-package-input "htslib")
                               "/include/htslib/sam.h")
                (string-append "SAMLIBS="
                               #$(this-package-input "htslib")
                               "/lib/libhts.so"))
       #:phases
       (modify-phases %standard-phases
         ;; No "configure" script.
         (replace 'configure
           (lambda _
             (substitute* "Makefile"
               (("^all : \\$\\(PROGRAMS\\).*") "all: $(PROGRAMS)\n")
               ;; Do not build bundled samtools library.
               (("^\\$\\(SAMLIBS\\).*") "")
               ;; Needed for Boost
               (("gnu\\+\\+98") "gnu++11"))
             ;; C++11 compatibility
             (substitute* "buildReadIndex.cpp"
               (("success = \\(getline")
                "success = (bool)(getline"))
             (substitute* '("PairedEndHit.h"
                            "SingleHit.h")
               (("return \\(in>>sid>>pos")
                "return (bool)(in>>sid>>pos"))))
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
               (install-file "rsem_perl_utils.pm" perl))))
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
                           "rsem-run-prsem-testing-procedure"))))))))
    (inputs
     (list boost r-minimal perl htslib-1.3 zlib))
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
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/rseqc/"
                       "RSeQC-" version ".tar.gz"))
       (sha256
        (base32
         "0gbb9iyb7swiv5455fm5rg98r7l6qn27v564yllqjd574hncpx6m"))))
    (build-system python-build-system)
    (inputs
     (list python-cython
           python-bx-python
           python-pybigwig
           python-pysam
           python-numpy
           zlib))
    (native-inputs
     (list python-nose))
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
  ;; There are no release tarballs.  And the installation instructions at
  ;; http://seek.princeton.edu/installation.jsp only mention a mercurial
  ;; changeset ID.  This is a git repository, though.  So we just take the
  ;; most recent commit.
  (let ((commit "196ed4c7633246e9c628e4330d77577ccfd7f1e5")
        (revision "1"))
    (package
      (name "seek")
      (version (git-version "1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/FunctionLab/sleipnir.git")
                      (commit commit)
                      (recursive? #true)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0c658n8nz563a96dsi4gl2685vxph0yfmmqq5yjc6i4xin1jy1ab"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags
         ,#~(list (string-append "-DSVM_LIBRARY="
                                 #$(this-package-input "libsvm")
                                 "/lib/libsvm.so.2")
                  (string-append "-DSVM_INCLUDE="
                                 #$(this-package-input "libsvm")
                                 "/include"))
         #:tests? #false ; tests only fail in the build container
         #:phases
         (modify-phases %standard-phases
           ;; The check phase expects to find the unit_tests executable in the
           ;; "build/bin" directory, but it is actually in "build/tests".
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "tests/unit_tests")))))))
      (inputs
       `(("apache-thrift:include" ,apache-thrift "include")
         ("apache-thrift:lib" ,apache-thrift "lib")
         ("gsl" ,gsl)
         ("boost" ,boost)
         ("gengetopt" ,gengetopt)
         ("libsvm" ,libsvm)
         ("log4cpp" ,log4cpp)
         ("python" ,python)
         ("readline" ,readline)))
      (native-inputs
       (list pkg-config))
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
    (version "1.14")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "0x3xdda78ac5vx66b3jdsv9sfhyz4npl4znl1zbaf3lbm6xdlhck"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Delete bundled htslib.
                   (delete-file-recursively "htslib-1.14")))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--with-ncurses")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "test/test.pl"
               ;; The test script calls out to /bin/bash
               (("/bin/bash") (which "bash"))))))))
    (native-inputs (list pkg-config))
    (inputs
     (list htslib ncurses perl python zlib))
    (home-page "http://samtools.sourceforge.net")
    (synopsis "Utilities to efficiently manipulate nucleotide sequence alignments")
    (description
     "Samtools implements various utilities for post-processing nucleotide
sequence alignments in the SAM, BAM, and CRAM formats, including indexing,
variant calling (in conjunction with bcftools), and a simple alignment
viewer.")
    (license license:expat)))

(define-public samtools-1.12
  (package/inherit samtools
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "1jrdj2idpma5ja9cg0rr73b565vdbr9wyy6zig54bidicc2pg8vd"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Delete bundled htslib.
                   (delete-file-recursively "htslib-1.12")))))
    (arguments
     (substitute-keyword-arguments (package-arguments samtools)
       ((#:modules _ #f)
        '((ice-9 ftw)
          (ice-9 regex)
          (guix build gnu-build-system)
          (guix build utils)))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'install 'install-library
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
                 (install-file "libbam.a" lib))))
           (add-after 'install 'install-headers
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((include (string-append (assoc-ref outputs "out")
                                             "/include/samtools/")))
                 (for-each (lambda (file)
                             (install-file file include))
                           (scandir "." (lambda (name)
                                          (string-match "\\.h$" name)))))))))))
    (native-inputs (list pkg-config))
    (inputs
     (list htslib-1.12 ncurses perl python zlib))))

(define-public samtools-1.10
  (package (inherit samtools)
    (name "samtools")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "119ms0dpydw8dkh3zc4yyw9zhdzgv12px4l2kayigv31bpqcb7kv"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Delete bundled htslib.
                   (delete-file-recursively "htslib-1.10")
                   #t))))
    (inputs
     (list htslib-1.10 ncurses perl python zlib))))

(define-public samtools-1.2
  (package (inherit samtools)
    (name "samtools")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "1akdqb685pk9xk1nb6sa9aq8xssjjhvvc06kp4cpdqvz2157l3j2"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete bundled htslib and Windows binaries
        '(for-each delete-file-recursively (list "win32" "htslib-1.2.1")))))
    (arguments
     `(#:make-flags
       ,#~(list (string-append "prefix=" #$output)
                (string-append "BGZIP="
                               #$(this-package-input "htslib")
                               "/bin/bgzip")
                (string-append "HTSLIB="
                               #$(this-package-input "htslib")
                               "/lib/libhts.so")
                (string-append "HTSDIR="
                               #$(this-package-input "htslib")
                               "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile-curses
           (lambda _
             (substitute* "Makefile"
               (("-lcurses") "-lncurses")
               (("include \\$\\(HTSDIR.*") ""))))
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "test/test.pl"
               ;; The test script calls out to /bin/bash
               (("/bin/bash") (which "bash"))
               ;; There are two failing tests upstream relating to the "stats"
               ;; subcommand in test_usage_subcommand ("did not have Usage"
               ;; and "usage did not mention samtools stats"), so we disable
               ;; them.
               (("(test_usage_subcommand\\(.*\\);)" cmd)
                (string-append "unless ($subcommand eq 'stats') {" cmd "};")))
             ;; This test fails because the grep output doesn't look as
             ;; expected; it is correct, though.
             (substitute* "test/mpileup/mpileup.reg"
               (("P 52.out.*") ""))))
         (delete 'configure))))
    (native-inputs
     (list grep gawk pkg-config))
    (inputs
     (list htslib-for-samtools-1.2 ncurses perl python zlib))))

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
           ((#:modules _ #f)
            '((ice-9 ftw)
              (ice-9 regex)
              (guix build gnu-build-system)
              (guix build utils)))
           ((#:phases phases)
            `(modify-phases ,phases
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((bin (string-append
                               (assoc-ref outputs "out") "/bin")))
                     (mkdir-p bin)
                     (install-file "samtools" bin)
                     #t)))
               (add-after 'install 'install-library
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
                     (install-file "libbam.a" lib))))
               (add-after 'install 'install-headers
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((include (string-append (assoc-ref outputs "out")
                                                 "/include/samtools/")))
                     (for-each (lambda (file)
                                 (install-file file include))
                               (scandir "." (lambda (name)
                                              (string-match "\\.h$" name)))))))
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
                      (url "https://github.com/wanpinglee/MOSAIK")
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

(define-public mosaicatcher
  (package
    (name "mosaicatcher")
    (version "0.3.1")
    (source (origin
              ;; There are no release tarballs nor tags.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/friendsofstrandseq/mosaicatcher")
                    (commit (string-append version "-dev"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1n2s5wvvj2y0vfgjkg1q11xahpbagxz7h2vf5q7qyy25s12kbzbd"))
              (patches (search-patches "mosaicatcher-unbundle-htslib.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #false ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "src")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((target (assoc-ref outputs "out"))
                    (bin (string-append target "/bin"))
                    (share (string-append target "/share/mosaicatcher")))
               (install-file "mosaic" bin)
               (mkdir-p share)
               (copy-recursively "../R" share)))))))
    (inputs
     (list boost htslib))
    (home-page "https://github.com/friendsofstrandseq/mosaicatcher")
    (synopsis "Count and classify Strand-seq reads")
    (description
     "Mosaicatcher counts Strand-seq reads and classifies strand states of
each chromosome in each cell using a Hidden Markov Model.")
    (license license:expat)))

(define-public ngs-sdk
  (package
    (name "ngs-sdk")
    (version "2.10.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ncbi/ngs")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ix51c25hjn57w93qmwzw80xh2i34wx8j2hn7szh8p6w8i3az5qa"))))
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
    (native-inputs (list perl))
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
    (version "2.10.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ncbi/ncbi-vdb")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m8hlxscidsfqm9x9fyi62q6lpf1dv5115kgjjgnrkl49q9c27m6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f ; not supported
       #:tests? #f ; no "check" target
       #:make-flags '("HAVE_HDF5=1")
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
         ;; See https://github.com/ncbi/ncbi-vdb/issues/14
         (add-after 'unpack 'patch-krypto-flags
           (lambda _
             (substitute* "libs/krypto/Makefile"
               (("-Wa,-march=generic64\\+aes") "")
               (("-Wa,-march=generic64\\+sse4") ""))
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
    (native-inputs (list perl))
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
     `(#:tests? #f ;no "check" target
       #:make-flags ,#~(list (string-append "LIB_LAPACK="
                                            #$(this-package-input "lapack")
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
               (install-file "plink" bin)))))))
    (inputs
     (list zlib lapack))
    (native-inputs
     (list unzip))
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
    (version "2.00a2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chrchang/plink-ng")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p88lz9agzjlspjhciz61qjc36cfniv4nkxszyy0njqyc5rzc0cd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       ,#~(list "BLASFLAGS=-llapack -lopenblas"
                "CFLAGS=-Wall -O2 -DDYNAMIC_ZLIB=1"
                "ZLIB=-lz"
                "BIN=plink prettify"
                (string-append "CC=" #$(cc-for-target))
                (string-append "PREFIX=" #$output)
                "DESTDIR=")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "1.9")))
         (delete 'configure)  ; no "configure" script
         (replace 'check
           (lambda* (#:key tests? inputs #:allow-other-keys)
             (when tests?
               (symlink "plink" "plink19")
               (symlink (search-input-file inputs "/bin/plink") "plink107")
               (setenv "PATH" (string-append (getcwd) ":" (getenv "PATH")))
               (with-directory-excursion "tests"
                 ;; The model test fails because of a 0.0001 difference.
                 (substitute* "tests.py"
                   (("diff -q test1.model test2.model")
                    "echo yes"))
                 (invoke "bash" "test_setup.sh")
                 (invoke "python3" "tests.py"))))))))
    (inputs
     (list lapack openblas zlib))
    (native-inputs
     (list diffutils plink python)) ; for tests
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
                      (url "https://github.com/smithlabcode/smithlab_cpp")
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
       (list samtools-0.1 zlib))
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
              ;; Remove bundled samtools.
              (snippet '(delete-file-recursively "samtools"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:make-flags
       ,#~(list (string-append "PREFIX=" #$output)
                (string-append "LIBBAM="
                               #$(this-package-input "samtools")
                               "/lib/libbam.a")
                (string-append "SMITHLAB_CPP="
                               #$(this-package-input "smithlab-cpp")
                               "/lib")
                "PROGS=preseq"
                "INCLUDEDIRS=$(SMITHLAB_CPP)/../include/smithlab-cpp $(SAMTOOLS_DIR)")))
    (inputs
     (list gsl samtools-0.1 smithlab-cpp zlib))
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
    (native-inputs
     (list python-pytest python-pytest-cov python-pytest-runner))
    (inputs
     (list python-bz2file))
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
    (version "2.10.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncbi/sra-tools")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1cr2mijkfs5sm35ffjs6861qsd1qkgnhnbavdv65zg5d655abbjf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f             ; not supported
       #:tests? #f                      ; no "check" target
       #:make-flags
       ,#~(list (string-append "DEFAULT_CRT="
                               #$(this-package-input "ncbi-vdb")
                               "/kfg/certs.kfg")
                (string-append "DEFAULT_KFG="
                               #$(this-package-input "ncbi-vdb")
                               "/kfg/default.kfg")
                (string-append "VDB_LIBDIR="
                               #$(this-package-input "ncbi-vdb")
                               #$(if (string-prefix? "x86_64"
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
             (substitute* "tools/driver-tool/utf8proc/Makefile"
               (("CC\\?=gcc") "myCC=gcc")
               (("\\(CC\\)") "(myCC)"))

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
                     #;
                     (string-append "--with-xml2-prefix="
                                    (assoc-ref inputs "libxml2"))
                     (string-append "--with-ncbi-vdb-sources="
                                    (assoc-ref inputs "ncbi-vdb"))
                     (string-append "--with-ncbi-vdb-build="
                                    (assoc-ref inputs "ncbi-vdb"))
                     (string-append "--with-ngs-sdk-prefix="
                                    (assoc-ref inputs "ngs-sdk"))
                     (string-append "--with-hdf5-prefix="
                                    (assoc-ref inputs "hdf5")))
             #t)))))
    (native-inputs (list perl))
    (inputs
     `(("ngs-sdk" ,ngs-sdk)
       ("ncbi-vdb" ,ncbi-vdb)
       ("libmagic" ,file)
       ("fuse" ,fuse)
       ("hdf5" ,hdf5-1.10)
       ("zlib" ,zlib)
       ("python" ,python-wrapper)))
    (home-page
     "https://trace.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?view=software")
    (synopsis "Tools and libraries for reading and writing sequencing data")
    (description
     "The SRA Toolkit from NCBI is a collection of tools and libraries for
reading of sequencing files from the Sequence Read Archive (SRA) database and
writing files into the .sra format.")
    (license license:public-domain)))

(define-public seqan
  (package
    (name "seqan")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/seqan/seqan3/releases/"
                                  "download/" version "/seqan3-"
                                  version "-Source.tar.xz"))
              (sha256
               (base32
                "1h2z0cvgidhkmh5xsbw75waqbrqbbv6kkrvb0b92xfh3gqpaiz22"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "ctest" "test" "--output-on-failure"))))))
    (native-inputs
     (list bzip2 cereal zlib))
    (home-page "https://www.seqan.de")
    (synopsis "Library for nucleotide sequence analysis")
    (description
     "SeqAn is a C++ library of efficient algorithms and data structures for
the analysis of sequences with the focus on biological data.  It contains
algorithms and data structures for string representation and their
manipulation, online and indexed string search, efficient I/O of
bioinformatics file formats, sequence alignment, and more.")
    (license license:bsd-3)))

(define-public seqan-2
  (package
    (inherit seqan)
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
       ,#~(begin
            (use-modules (guix build utils))
            (let ((tar #$(this-package-native-input "tar"))
                  (xz  #$(this-package-native-input "xz"))
                  (out #$output)
                  (doc #$output:doc))
              (setenv "PATH" (string-append tar "/bin:" xz "/bin"))
              (invoke "tar" "xvf" #$(this-package-native-input "source"))
              (chdir (string-append "seqan-library-" #$version))
              (copy-recursively "include" (string-append out "/include"))
              (copy-recursively "share"  (string-append doc "/share"))))))
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)
       ("xz" ,xz)))))

(define-public seqan-1
  (package (inherit seqan)
    (name "seqan")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://packages.seqan.de/seqan-library/"
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
       ,#~(begin
            (use-modules (guix build utils))
            (let ((tar  #$(this-package-native-input "tar"))
                  (bzip #$(this-package-native-input "bzip2"))
                  (out #$output)
                  (doc #$output:doc))
              (setenv "PATH" (string-append tar "/bin:" bzip "/bin"))
              (invoke "tar" "xvf" #$(this-package-native-input "source"))
              (chdir (string-append "seqan-library-" #$version))
              (copy-recursively "include" (string-append out "/include"))
              (copy-recursively "share"  (string-append doc "/share"))))))
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)
       ("bzip2" ,bzip2)))))

(define-public seqmagick
  (package
    (name "seqmagick")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "seqmagick" version))
       (sha256
        (base32
         "0pf98da7i59q47gwrbx0wjk6xlvbybiwphw80w7h4ydjj0579a2b"))))
    (build-system python-build-system)
    (inputs
     (list python-biopython))
    (native-inputs
     (list python-nose))
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
                    (url "https://github.com/lh3/seqtk")
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
     (list zlib))
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
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/amplab/snap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yag3jcazdqfxgmw0vbi91i98kj9sr0aqx83qqj6m5b45wxs7jms"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests? (invoke "./unit_tests"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "snap-aligner" bin)
               (install-file "SNAPCommand" bin)))))))
    (native-inputs
     (list zlib))
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
    (version "4.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/biocore/sortmerna")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0f8jfc8vsq6llhbb92p9yv7nbp566yqwfcmq3g2hw0n7d8hyl3a8"))))
    (build-system cmake-build-system)
    (outputs '("out"      ;for binaries
               "db"))     ;for sequence databases
    (arguments
     (list
      #:tests? #false ;unclear how to run them
      #:configure-flags
      #~(list "-DWITH_TESTS=ON"
              "-DCMAKE_CXX_FLAGS=-pthread"
              "-DZLIB_STATIC=OFF"
              "-DROCKSDB_STATIC=OFF"
              "-DPORTABLE=OFF" ;do not use static linking
              (string-append "-DROCKSDB_HOME="
                             #$(this-package-input "rocksdb"))
              (string-append "-DRAPIDJSON_HOME="
                             #$(this-package-input "rapidjson"))
              (string-append "-DRapidJson_DIR="
                             #$(this-package-input "rapidjson")
                             "/lib/cmake/RapidJSON")
              (string-append "-DRapidJSON_INCLUDE_DIR="
                             #$(this-package-input "rapidjson")
                             "/include"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'find-concurrentqueue-headers
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Ensure that headers can be found
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (search-input-directory
                                     inputs "/include/concurrentqueue")
                                    ":"
                                    (or (getenv "CPLUS_INCLUDE_PATH") "")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (db    (assoc-ref outputs "db"))
                    (share
                     (string-append db "/share/sortmerna/rRNA_databases")))
               (install-file "src/sortmerna" bin)
               (for-each (lambda (file)
                           (install-file file share))
                         (find-files "../source/data/rRNA_databases" ".*fasta"))))))))
    (inputs
     (list concurrentqueue
           gflags ; because of rocksdb
           rapidjson rocksdb zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://bioinfo.lifl.fr/RNA/sortmerna/")
    (synopsis "Biological sequence analysis tool for NGS reads")
    (description
     "SortMeRNA is a biological sequence analysis tool for filtering, mapping
and @acronym{OTU, operational taxonomic unit} picking of @acronym{NGS, next
generation sequencing} reads.  The core algorithm is based on approximate seeds
and allows for fast and sensitive analyses of nucleotide sequences.  The main
application of SortMeRNA is filtering rRNA from metatranscriptomic data.")
    ;; The source includes x86 specific code
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:lgpl3)))

(define-public star
  (package
    (name "star")
    (version "2.7.8a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alexdobin/STAR")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zc5biymja9zml9yizcj1h68fq9c6sxfcav8a0lbgvgsm44rvans"))
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
     (list xxd))
    (inputs
     (list htslib zlib))
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

(define-public star-for-pigx
  (package
    (inherit star)
    (name "star")
    (version "2.7.3a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alexdobin/STAR")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hgiqw5qhs0pc1xazzihcfd92na02xyq2kb469z04y1v51kpvvjq"))
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
                  #t))))))

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
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/subread/subread-"
                                  version "/subread-" version "-source.tar.gz"))
              (sha256
               (base32
                "0szmllia7jl0annk5568xjhw6cc8yj1c5mb961qk5m0lz6ig7kjn"))))
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
                            "-fmessage-length=0" "-fcommon"
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
           (lambda _ (chdir "src")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (mkdir-p bin)
               (copy-recursively "../bin" bin))))
         ;; no "configure" script
         (delete 'configure))))
    (inputs (list zlib))
    (home-page "http://subread.sourceforge.net/")
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
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ccb.jhu.edu/software/stringtie/dl/"
                                  "stringtie-" version ".tar.gz"))
              (sha256
               (base32
                "08w3ish4y9kf9acp7k38iwi8ixa6j51m6qyf0vvfj7yz78a3ai3x"))
              ;; This package bundles an annoying amount of third party source
              ;; code.
              (modules '((guix build utils)))
              (snippet
               '(delete-file-recursively "htslib"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no test suite
       #:make-flags '("LIBDEFLATE=-ldeflate"
                      "LIBBZ2=-lbz2"
                      "LIBLZMA=-llzma")
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'build 'use-system-samtools
           (lambda _
             (substitute* "Makefile"
               ((" -lm") " -lm -lhts")
               ((" \\$\\{HTSLIB\\}/libhts\\.a") " "))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (install-file "stringtie" bin)))))))
    (inputs
     (list bzip2 htslib-for-stringtie libdeflate zlib))
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
    (license license:expat)))

(define-public taxtastic
  (package
    (name "taxtastic")
    (version "0.9.2")
    (source (origin
              ;; The Pypi version does not include tests.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fhcrc/taxtastic")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k6wg9ych1j3srnhdny1y4470qlhfg730rb3rm3pq7l7gw62vmgb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-directory
           (lambda _
             ;; This test fails, but the error is not caught by the test
             ;; framework, so the tests fail...
             (substitute* "tests/test_taxit.py"
               (("self.cmd_fails\\(''\\)")
                "self.cmd_fails('nothing')"))
             ;; This version file is expected to be created with git describe.
             (mkdir-p "taxtastic/data")
             (with-output-to-file "taxtastic/data/ver"
               (lambda () (display ,version)))))
         (replace 'check
           ;; Note, this fails to run with "-v" as it tries to write to a
           ;; closed output stream.
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "unittest"))))
         ;; This fails because it cannot find psycopg2 even though it is
         ;; available.
         (delete 'sanity-check))))
    (propagated-inputs
     (list python-sqlalchemy
           python-decorator
           python-biopython
           python-pandas
           python-psycopg2
           python-fastalite
           python-pyyaml
           python-six
           python-jinja2
           python-dendropy))
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
    (version "0.1.16")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/vcftools/vcftools/releases/download/v"
                    version "/vcftools-" version ".tar.gz"))
              (sha256
               (base32
                "1qqlx7flfv7axrjwkaz6njkscsl1d0jw98ns8d8bh1n1hd1pgz6v"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags
       ,#~(list
           "CFLAGS=-O2"                 ; override "-m64" flag
           (string-append "PREFIX=" #$output)
           (string-append "MANDIR=" #$output "/share/man/man1"))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list perl zlib))
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
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://eddylab.org/software/infernal/"
                                  "infernal-" version ".tar.gz"))
              (sha256
               (base32
                "1z4mgwqg1j4n5ika08ai8mg9yjyjhf4821jp83v2bgwzxrykqjgr"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl python)) ; for tests
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
    ;; Infernal 1.1.3 requires VMX or SSE capability for parallel instructions.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:bsd-3)))

(define-public r-presto
  (let ((commit "052085db9c88aa70a28d11cc58ebc807999bf0ad")
        (revision "0"))
    (package
      (name "r-presto")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/immunogenomics/presto")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1c3fmag4r4p2lvbvxlxyck9dvfw1prbwcl9665mmlx4a35750hk8"))))
      (properties `((upstream . "presto")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-data-table
             r-deseq2
             r-dplyr
             r-matrix
             r-rcpp
             r-rcpparmadillo
             r-reshape2
             r-rlang
             r-tidyr))
      (home-page "https://github.com/immunogenomics/presto")
      (synopsis "Fast Functions for Differential Expression using Wilcox and AUC")
      (description "This package performs a fast Wilcoxon rank sum test and
auROC analysis.")
      (license license:gpl3))))

(define-public r-snapatac
  (package
    (name "r-snapatac")
    (version "2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/r3fang/SnapATAC")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "037jzlbl436fi7lkpq7d83i2vd1crnrik3vac2x6xj75dbikb2av"))))
    (properties `((upstream-name . "SnapATAC")))
    (build-system r-build-system)
    (propagated-inputs
      (list r-bigmemory
            r-doparallel
            r-dosnow
            r-edger
            r-foreach
            r-genomicranges
            r-igraph
            r-iranges
            r-irlba
            r-matrix
            r-plyr
            r-plot3d
            r-rann
            r-raster
            r-rcolorbrewer
            r-rhdf5
            r-rtsne
            r-scales
            r-viridis))
    (home-page "https://github.com/r3fang/SnapATAC")
    (synopsis "Single nucleus analysis package for ATAC-Seq")
    (description
      "This package provides a fast and accurate analysis toolkit for single
cell ATAC-seq (Assay for transposase-accessible chromatin using sequencing).
Single cell ATAC-seq can resolve the heterogeneity of a complex tissue and
reveal cell-type specific regulatory landscapes.  However, the exceeding data
sparsity has posed unique challenges for the data analysis.  This package
@code{r-snapatac} is an end-to-end bioinformatics pipeline for analyzing large-
scale single cell ATAC-seq data which includes quality control, normalization,
clustering analysis, differential analysis, motif inference and exploration of
single cell ATAC-seq sequencing data.")
    (license license:gpl3)))

(define-public r-umi4cpackage
  (let ((commit "88b07d896a137418ba6c31c2474b9dbe1d86fc20")
        (revision "1"))
    (package
      (name "r-umi4cpackage")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tanaylab/umi4cpackage")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bjzamdw2lcfhlbzc0vdva87c3wwnij8jsvnrpx4wyyxvpcz13m5"))))
      (properties `((upstream-name . "umi4cPackage")))
      (build-system r-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-references
             (lambda _
               (substitute* "inst/conf/paths.conf"
                 (("TG3C\\.bowtie2_bin=.*")
                  (string-append "TG3C.bowtie2_bin="
                                 (which "bowtie2") "\n")))
               (substitute* "inst/perl/map3c/TG3C/import3C.pl"
                 (("\"perl")
                  (string-append "\"" (which "perl")))))))))
      (inputs
       (list perl bowtie))
      (propagated-inputs
       (list r-misha r-zoo))
      (native-inputs (list r-knitr))
      (home-page "https://github.com/tanaylab/umi4cpackage")
      (synopsis "Processing and analysis of UMI-4C contact profiles")
      (description "This is a package that lets you process UMI-4C data from
scratch to produce nice plots.")
      (license license:expat))))

(define-public r-shinycell
  (let ((commit
         "aecbd56e66802f28e397f5ae1f19403aadd12163")
        (revision "1"))
    (package
      (name "r-shinycell")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/SGDDNB/ShinyCell")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "13jn2ikmvljnzayk485g1mmq5abcp9m1b8n1djdb1agmn83zaki5"))))
      (properties `((upstream-name . "ShinyCell")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-data-table
             r-ggplot2
             r-glue
             r-gridextra
             r-hdf5r
             r-matrix
             r-r-utils
             r-rcolorbrewer
             r-readr
             r-reticulate))
      (home-page "https://github.com/SGDDNB/ShinyCell")
      (synopsis "Shiny interactive web apps for single-cell data")
      (description
       "This package provides Shiny apps for interactive exploration of
single-cell data.")
      (license license:gpl3))))

(define-public r-archr
  (let ((commit "92ab814f86be0cea75c661f9827a9549c2cf47f5")
        (revision "1"))
    (package
      (name "r-archr")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/GreenleafLab/ArchR")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1m1vp3kkpvd0fcviv5vb3gcbm3w91ih6gm9ivg48swnbqny44kqb"))))
      (properties `((upstream-name . "ArchR")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-biocgenerics
             r-biostrings
             r-chromvar
             r-complexheatmap
             r-data-table
             r-genomicranges
             r-ggplot2
             r-ggrepel
             r-gridextra
             r-gtable
             r-gtools
             r-magrittr
             r-matrix
             r-matrixstats
             r-motifmatchr
             r-nabor
             r-plyr
             r-rcpp
             r-rhdf5
             r-rsamtools
             r-s4vectors
             r-stringr
             r-summarizedexperiment
             r-uwot))
      (home-page "https://github.com/GreenleafLab/ArchR")
      (synopsis "Analyze single-cell regulatory chromatin in R")
      (description
       "This package is designed to streamline scATAC analyses in R.")
      (license license:gpl2+))))

(define-public r-icellnet
  ;; v1.0 tagged in 2020, last commit contains many fixes.
  ;; DESCRIPTION says Version: 0.0.0.9000.
  (let ((commit "b9c05488fb8b5ea69bd560018966eaf4e25f82a")
        (revision "0"))
    (package
      (name "r-icellnet")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/soumelis-lab/ICELLNET")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0cld7d6xqnvd0zpcpg3sx73an6vdc9divzywgnn6zxnqcd987cnw"))))
      (build-system r-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-dir
             (lambda _ (chdir "icellnet"))))))
      (propagated-inputs
       (list r-annotationdbi
             r-data-table
             r-dplyr
             r-ggplot2
             r-hgu133plus2-db
             r-jetset
             r-psych
             r-reshape2
             r-rlist))
      (home-page "https://github.com/soumelis-lab/ICELLNET")
      (synopsis "Transcriptomic-based framework to dissect cell communication")
      (description "This packages provides a a transcriptomic-based framework
to dissect cell communication in a global manner.  It integrates an original
expert-curated database of ligand-receptor interactions taking into account
multiple subunits expression.  Based on transcriptomic profiles (gene
expression), this package allows to compute communication scores between cells
and provides several visualization modes that can be helpful to dig into
cell-cell interaction mechanism and extend biological knowledge.")
      (license license:gpl3))))

(define-public r-scde
  (package
    (name "r-scde")
    (version "1.99.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hms-dbmi/scde")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10na2gyka24mszdxf92wz9h2c13hdf1ww30c68gfsw53lvvhhhxb"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcpp
           r-rcpparmadillo
           r-mgcv
           r-rook
           r-rjson
           r-cairo
           r-rcolorbrewer
           r-edger
           r-quantreg
           r-nnet
           r-rmtstat
           r-extremes
           r-pcamethods
           r-biocparallel
           r-flexmix))
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

(define-public r-misha
  (package
    (name "r-misha")
    (version "4.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tanaylab/misha")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0bgivx3lzjh3173jsfrhb5kvhjsn53br0n4hmyx7i3dwy2cnnp2p"))
       ;; Delete bundled executable.
       (snippet
        '(delete-file "exec/bigWigToWig"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-use-bundled-bigWigToWig
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "R/misha.R"
               (("get\\(\".GLIBDIR\"\\), \"/exec/bigWigToWig")
                (string-append "\""
                               (assoc-ref inputs "kentutils")
                               "/bin/bigWigToWig"))))))))
    (inputs
     (list kentutils))
    (home-page "https://github.com/tanaylab/misha")
    (synopsis "Toolkit for analysis of genomic data")
    (description "This package is intended to help users to efficiently
analyze genomic data resulting from various experiments.")
    (license license:gpl2)))

(define-public r-scseqcomm
  (let ((commit "01076e703999f1a5aa76419d821b50aebe2b777a")
        (revision "0"))
    (package
      (name "r-scseqcomm")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/sysbiobig/scseqcomm")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1fw5si47d6agnz5fibmp2b1sv08pbpwv1j71w57xbav9044i032q"))
         ;; Delete bundled dependency.
         (modules '((guix build utils)))
         (snippet
          '(delete-file-recursively "other_deps"))))
      (build-system r-build-system)
      (inputs
       (list r-add2ggplot
             r-chorddiag
             r-doparallel
             r-dplyr
             r-foreach
             ;;r-grid ;; listed in DESCRIPTION
             r-gridextra
             r-ggplot2
             r-gtable
             r-htmlwidgets
             r-igraph
             r-matrix
             ;;r-methods ;; listed in DESCRIPTION
             r-org-hs-eg-db
             r-psych
             r-rcolorbrewer
             r-rlang
             r-scico
             r-tidygraph
             r-topgo))
      (native-inputs
       (list r-knitr))
      (home-page "https://gitlab.com/sysbiobig/scseqcomm")
      (synopsis "Inter- and intra- cellular signaling from single cell RNA-seq")
      (description "This package is tools for analysing intercellular and
intracellular signaling from single cell RNA-seq (scRNA-seq) data.")
      (license license:gpl3))))

(define-public r-shaman
  (let ((commit "d6944e8ac7bd1dbd5c6cec646eafc1d19d0ca96f")
        (release "2.0")
        (revision "2"))
    (package
      (name "r-shaman")
      (version (git-version release revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tanaylab/shaman")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "03sx138dzpfiq23j49z0m0s4j79855mrg64hpj9c83408wzphxi6"))
                (snippet
                 ;; This file will be generated.
                 '(delete-file "inst/doc/shaman-package.R"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-data-table
             r-domc
             r-ggplot2
             r-gviz
             r-misha
             r-plyr
             r-rann
             r-rcpp
             r-reshape2
             ;; For vignettes
             r-rmarkdown
             r-knitr))
      (home-page "https://github.com/tanaylab/shaman")
      (synopsis "Sampling HiC contact matrices for a-parametric normalization")
      (description "The Shaman package implements functions for
resampling Hi-C matrices in order to generate expected contact
distributions given constraints on marginal coverage and
contact-distance probability distributions.  The package also provides
support for visualizing normalized matrices and statistical analysis
of contact distributions around selected landmarks.")
      ;; Any version of the GPL
      (license license:gpl3+))))

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

(define-public r-demultiplex
  (let ((commit "6e2a1422c8e6f418cfb271997eebc91f9195f299")
        (revision "1"))
    (package
      (name "r-demultiplex")
      (version (git-version "1.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/chris-mcginnis-ucsf/MULTI-seq")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "01kv88wp8vdaq07sjk0d3d1cb553mq1xqg0war81pgmg63bgi38w"))))
      (properties `((upstream-name . "deMULTIplex")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-kernsmooth r-reshape2 r-rtsne r-shortread r-stringdist))
      (home-page "https://github.com/chris-mcginnis-ucsf/MULTI-seq")
      (synopsis "MULTI-seq pre-processing and classification tools")
      (description
       "deMULTIplex is an R package for analyzing single-cell RNA sequencing
data generated with the MULTI-seq sample multiplexing method.  The package
includes software to

@enumerate
@item Convert raw MULTI-seq sample barcode library FASTQs into a sample
  barcode UMI count matrix, and
@item Classify cell barcodes into sample barcode groups.
@end enumerate
")
      (license license:cc0))))

(define-public gdc-client
  (package
    (name "gdc-client")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NCI-GDC/gdc-client.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0cagawlzjwj3wam10lv64xgbfx4zcnzxi5sjpsdhq7rn4z24mzc2"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "requirements.txt"
               (("==") ">=")))))))
    (inputs
     (list python-cryptography
           python-intervaltree
           python-jsonschema
           python-lxml
           python-ndg-httpsclient
           python-progressbar2
           python-pyasn1
           python-pyopenssl
           python-pyyaml
           python-requests
           python-termcolor))
    (home-page "https://gdc.nci.nih.gov/access-data/gdc-data-transfer-tool")
    (synopsis "GDC data transfer tool")
    (description "The gdc-client provides several convenience functions over
the GDC API which provides general download/upload via HTTPS.")
    (license license:asl2.0)))

(define-public vsearch
  (package
    (name "vsearch")
    (version "2.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/torognes/vsearch")
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
     (list zlib bzip2 cityhash))
    (native-inputs
     (list autoconf automake))
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
    (version "2.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pardre/ParDRe-rel"
                           version ".tar.gz"))
       (sha256
        (base32
         "105s4f8zs8hh0sc32r9p725n7idza9cj5jvp5z1m5pljjhgk3if5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; tests require "prove"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (install-file "ParDRe" bin)))))))
    (inputs
     (list openmpi-c++ zlib))
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

(define-public ngshmmalign
  (package
    (name "ngshmmalign")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cbg-ethz/ngshmmalign/"
                           "releases/download/" version
                           "/ngshmmalign-" version ".tar.bz2"))
       (sha256
        (base32
         "0jryvlssi2r2ii1dxnx39yk6bh4yqgq010fnxrgfgbaj3ykijlzv"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #false))      ; there are none
    (inputs
     (list boost))
    (home-page "https://github.com/cbg-ethz/ngshmmalign/")
    (synopsis "Profile HMM aligner for NGS reads")
    (description
     "ngshmmalign is a profile HMM aligner for NGS reads designed particularly
for small genomes (such as those of RNA viruses like HIV-1 and HCV) that
experience substantial biological insertions and deletions.")
    (license license:gpl2+)))

(define-public prinseq
  (package
    (name "prinseq")
    (version "0.20.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/prinseq/standalone/"
                           "prinseq-lite-" version ".tar.gz"))
       (sha256
        (base32
         "0vxmzvmm67whxrqdaaamwgjk7cf0fzfs5s673jgg00kz7g70splv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #false                  ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (scripts (find-files "." "prinseq.*.pl"))
                    (guile (search-input-file inputs "bin/guile")))
               (substitute* scripts
                 (("\"perl -pe")
                  (string-append "\"" (which "perl") " -pe")))
               (for-each (lambda (file)
                           (chmod file #o555)
                           (install-file file bin)
                           (wrap-script (string-append bin "/" (basename file))
                                        #:guile guile
                                        `("PERL5LIB" ":" prefix
                                          (,(getenv "PERL5LIB")))))
                         scripts)))))))
    (inputs
     (list guile-3.0 ; for wrapper scripts
           perl
           perl-cairo
           perl-data-dumper
           perl-digest-md5
           perl-getopt-long
           perl-json
           perl-statistics-pca))
    (home-page "http://prinseq.sourceforge.net/")
    (synopsis "Preprocess sequence data in FASTA or FASTQ formats")
    (description
     "PRINSEQ is a bioinformatics tool to help you preprocess your genomic or
metagenomic sequence data in FASTA or FASTQ formats.  The tool is written in
Perl and can be helpful if you want to filter, reformat, or trim your sequence
data.  It also generates basic statistics for your sequences.")
    (license license:gpl3+)))

(define-public shorah
  (package
    (name "shorah")
    (version "1.99.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cbg-ethz/shorah"
                           "/releases/download/v" version
                           "/shorah-" version ".tar.xz"))
       (sha256
        (base32
         "158dir9qcqspknlnyfr9zwk41x48nrh5wcg10k2grh9cidp9daiq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test-wrapper
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (substitute* "examples/run_end2end_test"
                 (("\\$\\{interpreter\\} ../\\$\\{testscript\\}")
                  (string-append bin "/${testscript}"))))))
         (delete 'check)
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site (string-append
                           out "/lib/python"
                           ,(version-major+minor
                             (package-version python))
                           "/site-packages"))
                    (pythonpath (getenv "GUIX_PYTHONPATH"))
                    (script (string-append out "/bin/shorah")))
               (chmod script #o555)
               (wrap-program script `("GUIX_PYTHONPATH" ":" prefix (,site ,pythonpath))))))
         (add-after 'wrap-programs 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "make" "check")))))))
    (inputs
     (list boost
           htslib
           python
           python-biopython
           python-numpy
           zlib))
    (native-inputs
     (list pkg-config))
    (home-page "")
    (synopsis "Short reads assembly into haplotypes")
    (description
     "ShoRAH is a project for the analysis of next generation sequencing data.
It is designed to analyse genetically heterogeneous samples.  Its tools
provide error correction, haplotype reconstruction and estimation of the
frequency of the different genetic variants present in a mixed sample.")
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
     (list bundler ruby-rspec ruby-rake-compiler))
    (inputs
     (list zlib))
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
     (list ruby-rspec))
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
     (list ruby-bio-logger ruby-nokogiri))
    (inputs
     (list ruby-rspec))
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
     (list ruby-libxml))
    (native-inputs
     (list which))  ; required for test phase
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

(define-public bio-vcf
  (package
    (name "bio-vcf")
    (version "0.9.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "bio-vcf" version))
        (sha256
         (base32
          "1glw5pn9s8z13spxk6yyfqaz80n9lga67f33w35nkpq9dwi2vg6g"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-cucumber))
    (synopsis "Smart VCF parser DSL")
    (description
     "Bio-vcf provides a @acronym{DSL, domain specific language} for processing
the VCF format.  Record named fields can be queried with regular expressions.
Bio-vcf is a new generation VCF parser, filter and converter.  Bio-vcf is not
only very fast for genome-wide (WGS) data, it also comes with a filtering,
evaluation and rewrite language and can output any type of textual data,
including VCF header and contents in RDF and JSON.")
    (home-page "https://github.com/vcflib/bio-vcf")
    (license license:expat)))

(define-public r-phantompeakqualtools
  (let ((commit "8d2b2d18c686d894ef5908b37da7adf72a07ef42")
        (revision "1"))
    (package
      (name "r-phantompeakqualtools")
      (version (git-version "1.2.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kundajelab/phantompeakqualtools")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00anrvcwsp02d98qhj1xpj85644h2pp4kfzq6dgbmwmdr6jvy7p4"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((script (string-append (assoc-ref outputs "out")
                                            "/share/scripts")))
                 (install-file "run_spp.R" script)))))))
      (inputs
       `(("r" ,r-minimal)))
      (propagated-inputs
       (list r-catools
             r-snow
             r-snowfall
             r-bitops
             r-rsamtools
             r-spp
             gawk
             samtools
             boost
             gzip))
      (home-page "https://github.com/kundajelab/phantompeakqualtools")
      (synopsis "Informative enrichment for ChIP-seq data")
      (description "This package computes informative enrichment and quality
measures for ChIP-seq/DNase-seq/FAIRE-seq/MNase-seq data.  It can also be
used to obtain robust estimates of the predominant fragment length or
characteristic tag shift values in these assays.")
      (license license:bsd-3))))

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
     (list r-optparse r-rcolorbrewer))
    (home-page "https://www.e-rna.org/r-chie/index.cgi")
    (synopsis "Analysis framework for RNA secondary structure")
    (description
     "The R4RNA package aims to be a general framework for the analysis of RNA
secondary structure and comparative analysis in R.")
    (license license:gpl3+)))

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
         (add-before 'configure 'find-RCAS
           ;; The configure script can't find non-1.3.x versions of RCAS because
           ;; its R expression ‘1.10.1 >= 1.3.4’ evaluates to false.
           (lambda _
             (substitute* "configure"
               (("1\\.3\\.4") "0.0.0"))
             #t))
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
       ("guile" ,guile-2.2)
       ("guile-json" ,guile-json-1)
       ("guile-redis" ,guile2.2-redis)))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/BIMSBbioinfo/rcas-web")
    (synopsis "Web interface for RNA-centric annotation system (RCAS)")
    (description "This package provides a simple web interface for the
@dfn{RNA-centric annotation system} (RCAS).")
    (license license:agpl3+)))

(define-public r-chipkernels
  (let ((commit "c9cfcacb626b1221094fb3490ea7bac0fd625372")
        (revision "1"))
    (package
      (name "r-chipkernels")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ManuSetty/ChIPKernels")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "14bj5qhjm1hsm9ay561nfbqi9wxsa7y487df2idsaaf6z10nw4v0"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-iranges
             r-xvector
             r-biostrings
             r-bsgenome
             r-gtools
             r-genomicranges
             r-sfsmisc
             r-kernlab
             r-s4vectors
             r-biocgenerics))
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
             (url "https://github.com/ManuSetty/SeqGL")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1r6ywvhxl3ffv48lgj7sbd582mcc6dha3ksgc2qjlvjrnkbj3799"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-chipkernels
           r-genomicranges
           r-spams
           r-wgcna
           r-fastcluster))
    (home-page "https://github.com/ManuSetty/SeqGL")
    (synopsis "Group lasso for Dnase/ChIP-seq data")
    (description "SeqGL is a group lasso based algorithm to extract
transcription factor sequence signals from ChIP, DNase and ATAC-seq profiles.
This package presents a method which uses group lasso to discriminate between
bound and non bound genomic regions to accurately identify transcription
factors bound at the specific regions.")
    (license license:gpl2+)))

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
       ,#~(list (string-append "--with-hpdf="
                               #$(this-package-input "libharu")))
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
     (list perl
           libpng
           gd
           libx11
           libharu
           zlib))
    (native-inputs
     (list autoconf automake libtool pkg-config))
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
      (version (git-version "2.13.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/arq5x/bits")
                      (commit commit)))
                (file-name (git-file-name name version))
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
       (list gsl zlib))
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
      (version (git-version "1.2.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/smithlabcode/piranha")
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
                         (find-files (assoc-ref inputs "smithlab-cpp")))))
           (add-after 'install 'install-to-store
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (for-each (lambda (file)
                             (install-file file bin))
                           (find-files "bin" ".*"))))))
         #:configure-flags
         ,#~(list (string-append "--with-bam_tools_headers="
                                 #$(this-package-input "bamtools") "/include/bamtools")
                  (string-append "--with-bam_tools_library="
                                 #$(this-package-input "bamtools") "/lib/bamtools"))))
      (inputs
       `(("bamtools" ,bamtools)
         ("samtools" ,samtools-0.1)
         ("gsl" ,gsl)
         ("smithlab-cpp"
          ,(let ((commit "3723e2db438c51501d0423429ff396c3035ba46a"))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/smithlabcode/smithlab_cpp")
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
     (list python2-numpy python2-scipy python2-pysam))
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
  (let ((commit "1a9b779b93d0b244040274794d402106907b71b7")
        (revision "1"))
    (package
      (name "filevercmp")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ekg/filevercmp")
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
               (let ((out (assoc-ref outputs "out")))
                 (install-file "filevercmp" (string-append out "/bin"))
                 (install-file "filevercmp.h" (string-append out "/include"))
                 #t))))))
      (home-page "https://github.com/ekg/filevercmp")
      (synopsis "This program compares version strings")
      (description "This program compares version strings.  It intends to be a
replacement for strverscmp.")
      (license license:gpl3+))))

(define-public multiqc
  (package
    (name "multiqc")
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "multiqc" version))
       (sha256
        (base32
         "0y9sgjca3bp0kk3ngry4zf4q2diyzp5bvzsx5l23nsysfbfkigm4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" "/tmp")
               (let ((here (getcwd)))
                 (copy-recursively (assoc-ref inputs "tests") "/tmp/tests")
                 ;; ModuleNotFoundError: No module named 'multiqc.modules.ccs'
                 (delete-file "/tmp/tests/unit_tests/test_ccs.py")
                 (with-directory-excursion "/tmp/tests"
                   (setenv "GUIX_PYTHONPATH"
                           (string-append here ":" (getenv "GUIX_PYTHONPATH")))
                   (invoke "python" "-munittest" "discover"))))))
         ;; TODO: importing the picard and gatk modules fails for unknown
         ;; reasons.
         (delete 'sanity-check))))
    (propagated-inputs
     (list python-click
           python-coloredlogs
           python-future
           python-jinja2
           python-lzstring
           python-markdown
           python-matplotlib
           python-networkx
           python-numpy
           python-pyyaml
           python-requests
           python-rich
           python-simplejson
           python-spectra))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("tests"
        ,(let ((commit "02272d48a382beb27489fcf9e6308a0407dc3c2e"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/ewels/MultiQC_TestData")
                   (commit commit)))
             (file-name (git-file-name "multiqc-test-data" commit))
             (sha256
              (base32
               "1bha64wanrigczw4yn81din56396n61j5gqdrkslhslmskcafi91")))))))
    (home-page "https://multiqc.info")
    (synopsis "Aggregate bioinformatics analysis reports")
    (description
     "MultiQC is a tool to aggregate bioinformatics results across many
samples into a single report.  It contains modules for a large number of
common bioinformatics tools.")
    (license license:gpl3+)))

(define-public variant-tools
  (package
    (name "variant-tools")
    (version "3.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vatlab/varianttools")
             ;; There is no tag corresponding to version 3.1.2
             (commit "813ae4a90d25b69abc8a40f4f70441fe09015249")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "12ibdmksj7icyqhks4xyvd61bygk4pjmxn618kp6vgk1af01y34g"))))
    (build-system python-build-system)
    (inputs
     (list boost
           c-blosc
           gsl
           hdf5
           hdf5-blosc
           python-cython
           zlib))
    (propagated-inputs
     (list python-numpy python-pycurl python-pyzmq python-scipy
           python-tables))
    (home-page "https://vatlab.github.io/vat-docs/")
    (synopsis "Analyze genetic variants from Next-Gen sequencing studies")
    (description
     "Variant tools is a tool for the manipulation, annotation,
selection, simulation, and analysis of variants in the context of next-gen
sequencing analysis.  Unlike some other tools used for next-gen sequencing
analysis, variant tools is project based and provides a whole set of tools to
manipulate and analyze genetic variants.")
    (license license:gpl3+)))

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
     (list r-seqminer r-mvtnorm r-mass r-compquadform r-getopt))
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
               (url "https://github.com/rajewsky-lab/dropbead")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0sbzma49aiiyw8b0jpr7fnhzys9nsqmp4hy4hdz1gzyg1lhnca26"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-ggplot2 r-rcolorbrewer r-gridextra r-gplots r-plyr))
      (home-page "https://github.com/rajewsky-lab/dropbead")
      (synopsis "Basic exploration and analysis of Drop-seq data")
      (description "This package offers a quick and straight-forward way to
explore and perform basic analysis of single cell sequencing data coming from
droplet sequencing.  It has been particularly tailored for Drop-seq.")
      (license license:gpl3))))

(define-public r-cellchat
  (let ((commit
         "21edd226ca408e4c413408f98562d71ee0b54e5d")
        (revision "1"))
    (package
      (name "r-cellchat")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sqjin/CellChat")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0cvzl9mi8jjznpql2gv67swnk1dndn3a2h22z5l84h7lwpwjmh53"))
         (snippet
          '(for-each delete-file '("src/CellChat.so"
                                   "src/CellChat_Rcpp.o"
                                   "src/RcppExports.o")))))
      (properties `((upstream-name . "CellChat")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-biocgenerics
             r-circlize
             r-colorspace
             r-complexheatmap
             r-cowplot
             r-dplyr
             r-expm
             r-fnn
             r-forcats
             r-future
             r-future-apply
             r-gg-gap
             r-ggalluvial
             r-ggplot2
             r-ggrepel
             r-igraph
             r-irlba
             r-magrittr
             r-matrix
             r-nmf
             r-patchwork
             r-pbapply
             r-rcolorbrewer
             r-rcpp
             r-rcppeigen
             r-reshape2
             r-reticulate
             r-rspectra
             r-rtsne
             r-scales
             r-shape
             r-sna
             r-stringr
             r-svglite))
      (native-inputs (list r-knitr))
      (home-page "https://github.com/sqjin/CellChat")
      (synopsis "Analysis of cell-cell communication from single-cell transcriptomics data")
      (description
       "This package infers, visualizes and analyzes the cell-cell
communication networks from scRNA-seq data.")
      (license license:gpl3))))

(define-public sambamba
  (package
    (name "sambamba")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/biod/sambamba")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zdkd1md5wk4la71p82pbclqqcm55abk23fk087da6186i1bsihl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there is no test target
       #:parallel-build? #f             ; not supported
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'prepare-build-tools
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("\\$\\(shell which ldmd2\\)") (which "ldmd2")))
             (setenv "CC" "gcc")
             (setenv "D_LD" (which "ld.gold"))))
         (add-after 'unpack 'unbundle-prerequisites
           (lambda _
             (substitute* "Makefile"
               (("= lz4/lib/liblz4.a") "= -L-llz4")
               (("ldc_version_info lz4-static") "ldc_version_info"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (copy-file (string-append "bin/sambamba-" ,version)
                          (string-append bin "/sambamba"))))))))
    (native-inputs
     `(("ld-gold-wrapper"
        ;; Importing (gnu packages commencement) would introduce a cycle.
        ,(module-ref (resolve-interface
                      '(gnu packages commencement))
                     'ld-gold-wrapper))
       ("binutils-gold" ,binutils-gold)
       ("python" ,python)))
    (inputs
     (list ldc lz4 zlib))
    (home-page "https://github.com/biod/sambamba")
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
                    (url "https://github.com/KlugerLab/Ritornello")
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
     (list samtools-0.1 fftw boost zlib))
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
    (version "0.6.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FelixKrueger/TrimGalore")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0yrwg6325j4sb9vnplvl3jplzab0qdhp92wl480qjinpfq88j4rs"))))
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
     (list gzip perl pigz cutadapt))
    (native-inputs
     (list unzip))
    (home-page "https://www.bioinformatics.babraham.ac.uk/projects/trim_galore/")
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
               (wrap-script (string-append target "GESS.py")
                 #:guile (search-input-file inputs "bin/guile")
                 `("GUIX_PYTHONPATH" ":" = (,target ,(getenv "GUIX_PYTHONPATH"))))
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
       ("python2-biopython" ,python2-biopython)
       ("guile" ,guile-3.0))) ; for the script wrapper
    (home-page "https://compbio.uthscsa.edu/GESS_Web/")
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
    (version "3.697")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://evolution.gs.washington.edu/phylip/"
                           "download/phylip-" version ".tar.gz"))
       (sha256
        (base32
         "1h8h0nafnlbqryswxgplx80k2044yhfz97jh13vsgzlaifqdh9ls"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags (list "-f" "Makefile.unx" "CFLAGS=-fcommon" "install")
       #:parallel-build? #f             ; not supported
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "src")))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/bin")))
               (mkdir-p target)
               (for-each (lambda (file)
                           (install-file file target))
                         (find-files "../exe" ".*"))))))))
    (home-page "http://evolution.genetics.washington.edu/phylip/")
    (synopsis "Tools for inferring phylogenies")
    (description "PHYLIP (the PHYLogeny Inference Package) is a package of
programs for inferring phylogenies (evolutionary trees).")
    (license license:bsd-2)))

(define-public imp
  (package
    (name "imp")
    (version "2.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://integrativemodeling.org/"
                           version "/download/imp-" version ".tar.gz"))
       (sha256
        (base32
         "05hsrnkpkajppa3f45x4qsarnkj616hlby749zxg4is3bv4i6b5y"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #false ; The test suite is notoriously fickle
       #:configure-flags
       (let ((disabled-tests
              '("expensive"                 ;exclude expensive tests
                "IMP.modeller"              ;fail to import its own modules
                "IMP.parallel-test_sge.py"  ;fail in build container
                ;; The following test fails non-reproducibly on
                ;; an inexact numbers assertion.
                "IMP.em-medium_test_local_fitting.py"
                ;; The following test fails for unknown reasons
                "IMP.foxs-add-missing-residues.py")))
         (list
          (string-append
           "-DCMAKE_CTEST_ARGUMENTS="
           (string-join
            (list "-L" "-tests?-"       ;select only tests
                  "-E" (format #f "'(~a)'" (string-join disabled-tests "|")))
            ";"))))))
    (native-inputs
     `(("python" ,python-wrapper)
       ("swig" ,swig)))
    (inputs
     (list boost
           cgal
           gsl
           hdf5
           fftw
           eigen
           ;; Enabling MPI causes the build to use all the available memory and
           ;; fail (tested on a machine with 32 GiB of RAM).
           ;;("mpi" ,openmpi)
           opencv))
    (propagated-inputs
     (list python-numpy python-scipy python-pandas python-scikit-learn
           python-networkx))
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

;; We use this seemingly arbitrary commit because of
;; https://github.com/3DGenomes/TADbit/issues/371
(define-public tadbit
  (let ((commit "5c4c1ddaadfbaf7e6edc58173e46d801093bdc9b")
        (revision "1"))
    (package
      (name "tadbit")
      (version (git-version "1.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/3DGenomes/TADbit")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "17nwlvjgqpa7x6jgh56m3di61ynaz34kl1jamyv7r2a5rhfcbkla"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-problems-with-setup.py
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "src/test/Makefile"
                 (("^CFLAGS=") "CFLAGS= -fcommon"))
               
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
                  "extras/tadbit"))))
           (replace 'check
             (lambda* (#:key tests? inputs outputs #:allow-other-keys)
               (when tests?
                 (add-installed-pythonpath inputs outputs)
                 (invoke "python3" "test/test_all.py")))))))
      (native-inputs
       (list `(,glib "bin") ;for gtester
             pkg-config))
      (inputs
       ;; TODO: add Chimera for visualization
       (list imp
             mcl
             python-future
             python-h5py
             python-scipy
             python-numpy
             python-matplotlib
             python-pysam))
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
      (license license:gpl3+))))

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
             (url "https://github.com/ENCODE-DCC/kentUtils")
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
        ,(let ((commit "10fd107909c1ac4d679299908be4262a012965ba"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "http://genome-source.cse.ucsc.edu/samtabix.git")
                   (commit commit)))
             (file-name (git-file-name "samtabix" (string-take commit 7)))
             (sha256
              (base32
               "0c1nj64l42v395sa84n7az43xiap4i6f9n9dfz4058aqiwkhkmma")))))))
    (inputs
     `(("zlib" ,zlib)
       ("tcsh" ,tcsh)
       ("perl" ,perl)
       ("libpng" ,libpng)
       ("mariadb-dev" ,mariadb "dev")
       ("openssl" ,openssl)))
    (home-page "https://genome.cse.ucsc.edu/index.html")
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
      (version (git-version "1.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/aboyle/F-seq")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1nk33k0yajg2id4g59bc4szr58r2q6pdq42vgcw054m8ip9wv26h"))
                (modules '((guix build utils)))
                ;; Remove bundled Java library archives.
                (snippet
                 '(for-each delete-file (find-files "lib" ".*")))))
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
                    (search-input-file inputs
                                       (string-append "/lib/m2/commons-cli/commons-cli/"
                                                      ,(package-version java-commons-cli)
                                                      "/commons-cli-"
                                                      ,(package-version java-commons-cli)
                                                      ".jar")))
                   (("REALDIR=.*")
                    (string-append "REALDIR=" bin "\n")))
                 (install-file "README.txt" doc)
                 (install-file "bin/linux/fseq" bin)
                 (install-file "build~/fseq.jar" lib)
                 (copy-recursively "lib" lib)))))))
      (inputs
       (list perl java-commons-cli))
      (home-page "https://fureylab.web.unc.edu/software/fseq/")
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
             (url "https://github.com/FelixKrueger/Bismark")
             (commit version)))
       (file-name (git-file-name name version))
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
               (let ((minified (open-pipe* OPEN_READ "uglifyjs" file)))
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
     (list gzip perl-carp perl-getopt-long))
    (native-inputs
     `(("plotly.js"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://raw.githubusercontent.com/plotly/plotly.js/"
                               "v1.39.4/dist/plotly.js"))
           (sha256
            (base32 "138mwsr4nf5qif4mrxx286mpnagxd1xwl6k8aidrjgknaqg88zyr"))))
       ("uglifyjs" ,node-uglify-js)))
    (home-page "https://www.bioinformatics.babraham.ac.uk/projects/bismark/")
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
                  ;; Some files in the original tarball have restrictive
                  ;; permissions, which makes repackaging fail
                  (for-each (lambda (file) (chmod file #o644)) (find-files "."))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:make-flags '("CC=gcc" "CFLAGS=-fcommon -O3")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "src/BFdriver.c"
               (("/bin/bash") (which "bash")))
             (chdir "src")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((tools '("baseml" "basemlg" "codeml"
                            "pamp" "evolver" "yn00" "chi2"))
                   (bin    (string-append (assoc-ref outputs "out") "/bin"))
                   (docdir (string-append (assoc-ref outputs "out")
                                           "/share/doc/paml")))
               (mkdir-p bin)
               (for-each (lambda (file) (install-file file bin)) tools)
               (copy-recursively "../doc" docdir)))))))
    (home-page "http://abacus.gene.ucl.ac.uk/software/paml.html")
    (synopsis "Phylogentic analysis by maximum likelihood")
    (description "PAML (for Phylogentic Analysis by Maximum Likelihood)
contains a few programs for model fitting and phylogenetic tree reconstruction
using nucleotide or amino-acid sequence data.")
    ;; GPLv3 only
    (license license:gpl3)))

(define-public segemehl
  (package
    (name "segemehl")
    (version "0.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.bioinf.uni-leipzig.de/Software"
                                  "/segemehl/downloads/segemehl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0lbzbb7i8zadsn9b99plairhq6s2h1z8qdn6n7djclfis01nycz4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             "all")
       #:tests? #false ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; There is no installation target
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (exes (list "segemehl.x" "haarz.x")))
               (mkdir-p bin)
               (for-each (lambda (exe)
                           (install-file exe bin))
                         exes)))))))
    (inputs
     (list htslib ncurses zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.bioinf.uni-leipzig.de/Software/segemehl")
    (synopsis "Map short sequencer reads to reference genomes")
    (description "Segemehl is software to map short sequencer reads to
reference genomes.  Segemehl implements a matching strategy based on enhanced
suffix arrays (ESA).  It accepts fasta and fastq queries (gzip'ed and
bgzip'ed).  In addition to the alignment of reads from standard DNA- and
RNA-seq protocols, it also allows the mapping of bisulfite converted
reads (Lister and Cokus) and implements a split read mapping strategy.  The
output of segemehl is a SAM or BAM formatted alignment file.")
    (license license:gpl3+)))

(define-public kallisto
  (package
    (name "kallisto")
    (version "0.46.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pachterlab/kallisto")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ij5n7v3m90jdfi7sn8nvglfyf58abp1f5xq42r4k73l0lfds6xi"))
              (modules '((guix build utils)))
              (snippet
               '(delete-file-recursively "ext/htslib/"))))
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
     (list hdf5 htslib-1.9 zlib))
    (home-page "https://pachterlab.github.io/kallisto/")
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
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/COMBINE-lab/libgff")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ds9r22y8bl1rj7bhl0003kgmm6aam7g8l41mnjfrzw15d9zf9k4"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ; no tests included
    (home-page "https://github.com/COMBINE-lab/libgff")
    (synopsis "Parser library for reading/writing GFF files")
    (description "This is a simple \"libraryfication\" of the GFF/GTF parsing
code that is used in the Cufflinks codebase.  The goal of this library is to
provide this functionality without the necessity of drawing in a heavy-weight
dependency like SeqAn.")
    (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt"))))

(define-public sailfish
  (package
    (name "sailfish")
    (version "0.10.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kingsfordgroup/sailfish")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1amcc5hqvsl42hg4x19bi9vy47cl874s0lw1fmi0hwsdk9i8c03v"))
              (modules '((guix build utils)))
              ;; Delete bundled headers for eigen3.
              (snippet
               '(delete-file-recursively "include/eigen3/"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       ,#~(list (string-append "-DBOOST_INCLUDEDIR="
                               #$(this-package-input "boost")
                               "/include/")
                (string-append "-DBOOST_LIBRARYDIR="
                               #$(this-package-input "boost")
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
               (mkdir-p src)
               (mkdir-p include)
               (for-each (lambda (file)
                           (install-file file src))
                         (find-files (string-append rapmap "/src") "\\.(c|cpp)"))
               (copy-recursively (string-append rapmap "/include") include))))
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
                (search-input-directory
                 inputs
                 (string-append "/include/jellyfish-" ,(package-version jellyfish))))
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/lib/libjellyfish-2.0.a")
                (search-input-file inputs
                                   "/lib/libjellyfish-2.0.a"))
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/lib/libdivsufsort.a")
                (search-input-file inputs
                                   "/lib/libdivsufsort.so"))
               (("\\$\\{GAT_SOURCE_DIR\\}/external/install/lib/libdivsufsort64.a")
                (search-input-file inputs
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
                     (string-append (search-input-directory
                                     inputs "/include/eigen3")
                                    ":"
                                    (or (getenv "CPLUS_INCLUDE_PATH") ""))))))))
    (inputs
     `(("boost" ,boost)
       ("eigen" ,eigen)
       ("jemalloc" ,jemalloc)
       ("jellyfish" ,jellyfish)
       ("sparsehash" ,sparsehash)
       ("rapmap" ,(origin
                    (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/COMBINE-lab/RapMap")
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
       ("tbb" ,tbb-2020)
       ("zlib" ,zlib)))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.cs.cmu.edu/~ckingsf/software/sailfish/")
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
                    (url "https://github.com/COMBINE-lab/staden-io_lib")
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

(define-public salmon
  (package
    (name "salmon")
    (version "1.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/COMBINE-lab/salmon")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wb5wl0rc77svbwq6zvak5h7pf9acw3di0vz5i3gqyhg5l6qd736"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled headers for eigen3.
               '(delete-file-recursively "include/eigen3/"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       ,#~(list (string-append "-Dlibgff_DIR="
                               #$(this-package-input "libgff") "/lib")
                "-DCMAKE_CXX_FLAGS=\"-DHAVE_NUMERIC_LIMITS128=1\""
                "-Dlibgff_FOUND=TRUE"
                "-DTBB_FOUND=TRUE"
                #$(string-append "-DTBB_VERSION=" (package-version tbb-2020))
                "-DTBB_LIBRARIES=tbb -ltbbmalloc"
                "-DFETCHED_PUFFERFISH=TRUE"
                "-DUSE_SHARED_LIBS=TRUE")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-pufferfish
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "pufferfish")
                               "external/pufferfish")
             ;; This test isn't working correctly, so compilation aborts.
             (substitute* "external/pufferfish/include/string_view.hpp"
               (("#if __has_include\\(<string_view>\\)")
                "#if 0"))
             (let ((headers "external/install/pufferfish/include/pufferfish")
                   (source "external/install/src/pufferfish"))
               (mkdir-p headers)
               (mkdir-p source)
               (for-each (lambda (file)
                           (install-file (string-append "external/pufferfish/include/" file)
                                         headers))
                         (list "ProgOpts.hpp" "BooPHF.hpp" "SpinLock.hpp"
                               "Kmer.hpp" "CanonicalKmer.hpp" "string_view.hpp"
                               "CanonicalKmerIterator.hpp"
                               "PufferfishBaseIndex.hpp"
                               "PufferfishIndex.hpp"
                               "PufferfishSparseIndex.hpp"
                               "PufferfishLossyIndex.hpp"
                               "PufferfishTypes.hpp"
                               "rank9b.hpp" "rank9sel.hpp" "macros.hpp"
                               "select.hpp" "Util.hpp"
                               "PairedAlignmentFormatter.hpp"
                               "SelectiveAlignmentUtils.hpp"
                               "PuffAligner.hpp" "MemCollector.hpp"
                               "MemChainer.hpp" "CommonTypes.hpp"
                               "SAMWriter.hpp" "PufferfishConfig.hpp"
                               "BulkChunk.hpp" "BinWriter.hpp"))
               (for-each (lambda (dir)
                           (copy-recursively
                            (string-append "external/pufferfish/include/" dir)
                            (string-append headers "/" dir)))
                         (list "libdivide"
                               "ksw2pp"
                               "compact_vector"
                               "metro"
                               "chobo"
                               "sparsepp"
                               "simde"
                               "tsl"))
               (copy-recursively
                (string-append "external/pufferfish/src/metro/")
                (string-append source "/metro"))
               (install-file
                (string-append "external/pufferfish/src/rank9b.cpp")
                source)

               ;; Do not complain about not having built libtbb
               (substitute* "external/pufferfish/external/twopaco/CMakeLists.txt"
                 (("add_dependencies.*") "")))))
         (add-after 'unpack 'do-not-phone-home
           (lambda _
             (substitute* "src/Salmon.cpp"
               (("getVersionMessage\\(\\)") "\"\""))))
         (add-after 'unpack 'use-system-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Ensure that all headers can be found
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (or (getenv "CPLUS_INCLUDE_PATH") "")
                                    ":"
                                    (getcwd) "/external/install/pufferfish/include:"
                                    (assoc-ref inputs "eigen")
                                    "/include/eigen3"))))
         (add-after 'unpack 'fix-error-message-in-tests
           (lambda _
             (substitute* "cmake/TestSalmonQuasi.cmake"
               (("SALMON_QUASI_INDEX_COMMAND")
                "SALMON_QUASI_INDEX_CMD")))))))
    (inputs
     `(("boost" ,boost)
       ("bzip2" ,bzip2)
       ("cereal" ,cereal)
       ("curl" ,curl)
       ("eigen" ,eigen)
       ("jemalloc" ,jemalloc)
       ("libgff" ,libgff)
       ("pufferfish" ,(origin
                        (method git-fetch)
                        (uri (git-reference
                              (url "https://github.com/COMBINE-lab/pufferfish")
                              (commit (string-append "salmon-v" version))))
                        (file-name (git-file-name "pufferfish" version))
                        (sha256
                         (base32
                          "0jakgpbanl6cs23x3g26iab54p7zylcf9v8vc32ps57smp8wql52"))))
       ("tbb" ,tbb-2020)
       ("libstadenio-for-salmon" ,libstadenio-for-salmon)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (native-inputs
     (list pkg-config))
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
                    (url "https://github.com/linnarsson-lab/loompy")
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
             (invoke "pytest" "tests"))))))
    (propagated-inputs
     (list python-h5py python-numpy python-pandas python-scipy))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/linnarsson-lab/loompy")
    (synopsis "Work with .loom files for single-cell RNA-seq data")
    (description "The loom file format is an efficient format for very large
omics datasets, consisting of a main matrix, optional additional layers, a
variable number of row and column annotations.  Loom also supports sparse
graphs.  This library makes it easy to work with @file{.loom} files for
single-cell RNA-seq data.")
    (license license:bsd-3)))

(define-public python-biothings-client
  (package
    (name "python-biothings-client")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "biothings_client" version))
       (sha256
        (base32 "0bccs37d5saxn5xsd2rfpkrnc5a120xs3ibizai66fgvp1vxbnc4"))))
    (build-system python-build-system)
    (arguments `(#:tests? #false)) ; require internet access
    (propagated-inputs (list python-requests))
    (home-page "https://github.com/biothings/biothings_client.py")
    (synopsis "Python client for BioThings API services")
    (description "This package provides a Python client for BioThings
API services.")
    (license license:bsd-3)))

(define-public python-mygene
  (package
    (name "python-mygene")
    (version "3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mygene" version))
       (sha256
        (base32 "1snszwdgfygchxshcbry3b5pbcw3g1isp8dw46razxccqaxwlag7"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-biothings-client))
    (home-page "https://github.com/biothings/mygene.py")
    (synopsis "Python Client for MyGene.Info services")
    (description "MyGene.Info provides simple-to-use REST web services
to query/retrieve gene annotation data.  It's designed with simplicity
and performance emphasized.  Mygene is a Python wrapper to access
MyGene.Info services.")
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
                      (url "https://github.com/cmzmasek/forester")
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
       (list java-commons-codec java-openchart2))
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
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/biojava/thirdparty/forester/"
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
     (list java-commons-codec java-openchart2))
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
                               "forester/java/classes/resources/"
                               "synth_look_and_feel_1.xml"))
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
     (list java-log4j-api java-log4j-core java-slf4j-api
           java-slf4j-simple))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://biojava.org")
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
     (list java-log4j-api
           java-log4j-core
           java-slf4j-api
           java-slf4j-simple
           java-biojava-core
           java-forester))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://biojava.org")
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
     (list java-log4j-api
           java-log4j-core
           java-slf4j-api
           java-slf4j-simple
           java-biojava-core
           java-biojava-phylo
           java-forester))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://biojava.org")
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
     (list java-log4j-api
           java-log4j-core
           java-slf4j-api
           java-slf4j-simple
           java-biojava-core-4.0
           java-forester-1.005))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://biojava.org")
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
     (list java-log4j-api
           java-log4j-core
           java-slf4j-api
           java-slf4j-simple
           java-biojava-core-4.0
           java-biojava-phylo-4.0
           java-forester-1.005))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://biojava.org")
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
       (list ,#~(string-append "-Dpicard.executable.dir="
                               #$(this-package-input "java-picard")
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
                                     dirs)))))
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
                  (string-append "jar_deploy_dir=" share "\n"))))))
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
                         (find-files "jar/lib" "\\.jar$"))))))))
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
     (list unzip java-testng))
    (home-page "http://mccarrolllab.com/dropseq/")
    (synopsis "Tools for Drop-seq analyses")
    (description "Drop-seq is a technology to enable biologists to
analyze RNA expression genome-wide in thousands of individual cells at
once.  This package provides tools to perform Drop-seq analyses.")
    (license license:expat)))

(define-public pigx-rnaseq
  (package
    (name "pigx-rnaseq")
    (version "0.0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_rnaseq/"
                                  "releases/download/v" version
                                  "/pigx_rnaseq-" version ".tar.gz"))
              (sha256
               (base32
                "1ja3bda1appxrzbfy7wp7khy30mm7lic8xbq3gkbpc5bld3as9cm"))
              (patches (search-patches "pigx-rnaseq-no-citeproc.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-tests? #f             ; not supported
       #:phases
       (modify-phases %standard-phases
         ;; "test.sh" runs the whole pipeline, which takes a long time and
         ;; might fail due to OOM.  The MultiQC is also resource intensive.
         (add-after 'unpack 'disable-resource-intensive-test
           (lambda _
             (substitute* "Makefile.in"
               (("^  tests/test_multiqc/test.sh") "")
               (("^  test.sh") ""))))
         (add-before 'bootstrap 'autoreconf
           (lambda _
             (invoke "autoreconf" "-vif")))
         (add-before 'configure 'set-PYTHONPATH
           (lambda _
             (setenv "PYTHONPATH" (getenv "GUIX_PYTHONPATH"))))
         (add-before 'check 'set-timezone
           ;; The readr package is picky about timezones.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "UTC+1")
             (setenv "TZDIR"
                     (search-input-directory inputs
                                             "share/zoneinfo")))))))
    (inputs
     (list coreutils
           sed
           gzip
           snakemake
           multiqc
           star-for-pigx
           hisat2
           fastp
           htseq
           samtools
           r-minimal
           r-rmarkdown
           r-ggplot2
           r-ggpubr
           r-ggrepel
           r-gprofiler2
           r-deseq2
           r-dt
           r-knitr
           r-pheatmap
           r-corrplot
           r-reshape2
           r-plotly
           r-scales
           r-summarizedexperiment
           r-crosstalk
           r-tximport
           r-rtracklayer
           r-rjson
           salmon
           pandoc
           python-wrapper
           python-deeptools
           python-pyyaml))
    (native-inputs
     (list tzdata automake autoconf))
    (home-page "https://bioinformatics.mdc-berlin.de/pigx/")
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
    (version "0.0.53")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_chipseq/"
                                  "releases/download/v" version
                                  "/pigx_chipseq-" version ".tar.gz"))
              (sha256
               (base32
                "0c6npx35sszycf059w1x1k4k9hq1qqxny0i4p57q1188czr4561h"))
              (patches (search-patches "pigx-chipseq-no-citeproc.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; parts of the tests rely on access to the network
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'autoreconf
           (lambda _
             (invoke "autoreconf" "-vif")))
         (add-before 'configure 'set-PYTHONPATH
           (lambda _
             (setenv "PYTHONPATH" (getenv "GUIX_PYTHONPATH")))))))
    (inputs
     (list grep
           coreutils
           r-minimal
           r-argparser
           r-biocparallel
           r-biostrings
           r-chipseq
           r-corrplot
           r-data-table
           r-deseq2
           r-dplyr
           r-dt
           r-genomation
           r-genomicalignments
           r-genomicranges
           r-ggplot2
           r-ggrepel
           r-gprofiler2
           r-heatmaply
           r-hexbin
           r-htmlwidgets
           r-jsonlite
           r-pheatmap
           r-plotly
           r-rmarkdown
           r-rsamtools
           r-rsubread
           r-rtracklayer
           r-s4vectors
           r-stringr
           r-tibble
           r-tidyr
           python-wrapper
           python-pyyaml
           python-magic
           python-xlrd
           trim-galore
           macs
           multiqc
           perl
           pandoc
           fastqc
           bowtie
           idr
           snakemake
           samtools
           bedtools
           kentutils))
    (native-inputs
     (list autoconf automake python-pytest))
    (home-page "https://bioinformatics.mdc-berlin.de/pigx/")
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
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_bsseq/"
                                  "releases/download/v" version
                                  "/pigx_bsseq-" version ".tar.gz"))
              (sha256
               (base32
                "1dipikph0xdr8fp0h1flpafcrg60y4aabljg8fl1v92j3gxdggmw"))
              (patches (search-patches "pigx-bsseq-no-citeproc.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(;; TODO: tests currently require 12+GB of RAM.  See
       ;; https://github.com/BIMSBbioinfo/pigx_bsseq/issues/164
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'autoreconf
           (lambda _
             (invoke "autoreconf" "-vif")))
         (add-before 'configure 'set-PYTHONPATH
           (lambda _
             (setenv "PYTHONPATH" (getenv "GUIX_PYTHONPATH"))))
         (add-before 'check 'set-timezone
           ;; The readr package is picky about timezones.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "UTC+1")
             (setenv "TZDIR"
                     (search-input-directory inputs
                                             "share/zoneinfo")))))))
    (native-inputs
     (list tzdata automake autoconf))
    (inputs
     (list coreutils
           sed
           grep
           r-minimal
           r-annotationhub
           r-dt
           r-genomation
           r-ggbio
           r-ggrepel
           r-matrixstats
           r-methylkit
           r-reshape2
           r-rtracklayer
           r-rmarkdown
           r-bookdown
           r-ggplot2
           r-ggbio
           pandoc
           python-wrapper
           python-pyyaml
           snakemake
           bismark
           bowtie
           bwa-meth
           fastqc
           methyldackel
           multiqc
           trim-galore
           cutadapt
           samblaster
           samtools))
    (home-page "https://bioinformatics.mdc-berlin.de/pigx/")
    (synopsis "Bisulfite sequencing pipeline from fastq to methylation reports")
    (description "PiGx BSseq is a data processing pipeline for raw fastq read
data of bisulfite experiments; it produces reports on aggregate methylation
and coverage and can be used to produce information on differential
methylation and segmentation.")
    (license license:gpl3+)))

(define-public pigx-scrnaseq
  (package
    (name "pigx-scrnaseq")
    (version "1.1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_scrnaseq/"
                                  "releases/download/v" version
                                  "/pigx_scrnaseq-" version ".tar.gz"))
              (sha256
               (base32
                "1h5mcxzwj3cidlkvy9ly5wmi48vwfsjf8dxjfirknqxr9a92hwlx"))
              (patches (search-patches "pigx-scrnaseq-no-citeproc.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'autoreconf
           (lambda _
             (invoke "autoreconf" "-vif")))
         (add-before 'configure 'set-PYTHONPATH
           (lambda _
             (setenv "PYTHONPATH" (getenv "GUIX_PYTHONPATH")))))))
    (native-inputs
     (list automake autoconf))
    (inputs
     `(("coreutils" ,coreutils)
       ("perl" ,perl)
       ("fastqc" ,fastqc)
       ("flexbar" ,flexbar)
       ("java" ,icedtea-8)
       ("jellyfish" ,jellyfish)
       ("python-wrapper" ,python-wrapper)
       ("python-pyyaml" ,python-pyyaml)
       ("python-pandas" ,python-pandas)
       ("python-magic" ,python-magic)
       ("python-numpy" ,python-numpy)
       ("python-loompy" ,python-loompy)
       ("pandoc" ,pandoc)
       ("samtools" ,samtools)
       ("snakemake" ,snakemake)
       ("star" ,star-for-pigx)
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
       ("r-seurat" ,r-seurat)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-stringr" ,r-stringr)
       ("r-yaml" ,r-yaml)))
    (home-page "https://bioinformatics.mdc-berlin.de/pigx/")
    (synopsis "Analysis pipeline for single-cell RNA sequencing experiments")
    (description "PiGX scRNAseq is an analysis pipeline for preprocessing and
quality control for single cell RNA sequencing experiments.  The inputs are
read files from the sequencing experiment, and a configuration file which
describes the experiment.  It produces processed files for downstream analysis
and interactive quality reports.  The pipeline is designed to work with UMI
based methods.")
    (license license:gpl3+)))

(define-public pigx-sars-cov2-ww
  (package
    (name "pigx-sars-cov2-ww")
    (version "0.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/pigx_sarscov2_ww/"
                                  "releases/download/v" version
                                  "/pigx_sars-cov2-ww-" version ".tar.gz"))
              (sha256
               (base32
                "0axnmz4d8zgir888mc0cilcq4m3v41xmjmpp3w3444lciwnxydvs"))
              (patches (search-patches "pigx-sars-cov2-ww-no-citeproc.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'autoreconf
           (lambda _
             (invoke "autoreconf" "-vif")))
         (add-before 'configure 'set-PYTHONPATH
           (lambda _
             (setenv "PYTHONPATH" (getenv "GUIX_PYTHONPATH")))))))
    (native-inputs
     (list automake autoconf))
    (inputs
     (list bash-minimal
           bbmap
           bedtools
           bwa
           ensembl-vep
           fastp
           fastqc
           ivar
           kraken2
           krona-tools
           lofreq
           multiqc
           prinseq
           python-pyyaml
           python-wrapper
           r-base64url
           r-dplyr
           r-dt
           r-ggplot2
           r-magrittr
           r-minimal
           r-plotly
           r-qpcr
           r-r-utils
           r-reshape2
           r-rmarkdown
           r-stringr
           r-tidyr
           samtools
           snakemake
           wget))
    (home-page "https://bioinformatics.mdc-berlin.de/pigx/")
    (synopsis "Analysis pipeline for wastewater sequencing")
    (description "PiGx SARS-CoV-2 is a pipeline for analysing data from
sequenced wastewater samples and identifying given variants-of-concern of
SARS-CoV-2.  The pipeline can be used for continuous sampling.  The output
report will provide an intuitive visual overview about the development of
variant abundance over time and location.")
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
     (list python pigx-bsseq pigx-chipseq pigx-rnaseq pigx-scrnaseq))
    (home-page "https://bioinformatics.mdc-berlin.de/pigx/")
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
                    (url "https://github.com/jsh58/Genrich")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
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
     (list zlib))
    (home-page "https://github.com/jsh58/Genrich")
    (synopsis "Detecting sites of genomic enrichment")
    (description "Genrich is a peak-caller for genomic enrichment
assays (e.g. ChIP-seq, ATAC-seq).  It analyzes alignment files generated
following the assay and produces a file detailing peaks of significant
enrichment.")
    (license license:expat)))

(define-public mantis
  ;; This is an arbitrary commit as a year has passed since 0.1 was tagged.
  (let ((commit "b6979a269172a45201c8366680d8b889f889432b")
        (revision "2"))
    (package
      (name "mantis")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/splatlab/mantis")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0dq8a785hnaxx5kq757m5czs8xpcjpcph1inq2nm8h6zfvqyj8xs"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f ; there are none
         #:configure-flags (list "-DNH=ON"))) ; do not use SSE4.2 instructions
      (inputs
       (list sdsl-lite openssl zlib))
      (native-inputs
       (list gcc-7))
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
                      (url "https://github.com/pervouchine/sjcount-full")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0gdgj35j249f04rqgq8ymcc1xg1vi9kzbajnjqpaq2wpbh8bl234"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; requires a 1.4G test file
         #:make-flags
         ,#~(list (string-append "SAMTOOLS_DIR="
                                 #$(this-package-input "samtools")
                                 "/lib/"))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "makefile"
                 (("-I \\$\\{SAMTOOLS_DIR\\}")
                  (string-append "-I" (assoc-ref inputs "samtools")
                                 "/include/samtools"))
                 (("-lz ") "-lz -lpthread "))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (for-each (lambda (tool)
                           (install-file tool
                                         (string-append (assoc-ref outputs "out")
                                                        "/bin")))
                         '("j_count" "b_count" "sjcount")))))))
      (inputs
       (list samtools-0.1 zlib))
      (home-page "https://github.com/pervouchine/sjcount-full/")
      (synopsis "Annotation-agnostic splice junction counting pipeline")
      (description "Sjcount is a utility for fast quantification of splice
junctions in RNA-seq data.  It is annotation-agnostic and offset-aware.  This
version does count multisplits.")
      (license license:gpl3+))))

(define-public minimap2
  (package
    (name "minimap2")
    (version "2.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lh3/minimap2/"
                           "releases/download/v" version "/"
                           "minimap2-" version ".tar.bz2"))
       (sha256
        (base32
         "00ngbz1swcgxk5apx9dz5xkh1z8abdpysx5lc7w8fbrfxp41w0j0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are none
       #:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (srfi srfi-26))
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (let ((system ,(or (%current-target-system)
                                (%current-system))))
               (cond
                 ((string-prefix? "x86_64" system)
                  "all")
                 ((or (string-prefix? "i586" system)
                      (string-prefix? "i686" system))
                  "sse2only=1")
                 ((string-prefix? "armhf" system)
                  "arm_neon=1")
                 ((string-prefix? "aarch64" system)
                  "aarch64=1")
                 (else ""))))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (inc (string-append out "/include"))
                    (man (string-append out "/share/man/man1")))
               (install-file "minimap2" bin)
               (install-file "libminimap2.a" lib)
               (install-file "minimap2.1" man)
               (map (cut install-file <> inc)
                    (find-files "." "\\.h$"))
               ;; Not this file.
               (delete-file (string-append inc "/emmintrin.h"))
               (mkdir-p (string-append lib "/pkgconfig"))
               (with-output-to-file (string-append lib "/pkgconfig/minimap2.pc")
                (lambda _
                  (format #t "prefix=~a~@
                          exec_prefix=${prefix}~@
                          libdir=${exec_prefix}/lib~@
                          includedir=${prefix}/include~@
                          ~@
                          Name: libminimap2~@
                          Version: ~a~@
                          Description: A versatile pairwise aligner for genomic and spliced nucleotide sequence~@
                          Libs: -L${libdir} -lminimap2~@
                          Cflags: -I${includedir}~%"
                          out ,version))))
             #t)))))
    (inputs
     (list zlib))
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

(define-public python-mappy
  (package
   (name "python-mappy")
   (version "2.18")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "mappy" version))
            (sha256
             (base32
              "1a05p7rkmxa6qhm108na8flzj2v45jab06drk59kzk1ip2sgvzqq"))))
   (build-system python-build-system)
   (native-inputs
    (list python-cython))
   (inputs
    (list zlib))
   (home-page "https://github.com/lh3/minimap2")
   (synopsis "Python binding for minimap2")
   (description "This package provides a convenient interface to minimap2,
a fast and accurate C program to align genomic and transcribe nucleotide
sequences.")
   (license license:expat)))

(define-public miniasm
  (package
   (name "miniasm")
   (version "0.3")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                   (url "https://github.com/lh3/miniasm")
                   (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "04dv5wv8bhsw1imxwyd438bnn9kby7svp44nbcz8lsadzjjci5gs"))))
   (build-system gnu-build-system)
   (inputs
    (list zlib))
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "miniasm" bin)
              (install-file "minidot" bin)
              #t))))))
   (home-page "https://github.com/lh3/miniasm")
   (synopsis "Ultrafast de novo assembly for long noisy reads")
   (description "Miniasm is a very fast OLC-based de novo assembler for noisy
long reads.  It takes all-vs-all read self-mappings (typically by minimap) as
input and outputs an assembly graph in the GFA format.  Different from
mainstream assemblers, miniasm does not have a consensus step.  It simply
concatenates pieces of read sequences to generate the final unitig sequences.
Thus the per-base error rate is similar to the raw input reads.")
   (license license:expat)))

(define-public bandage
  (package
    (name "bandage")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rrwick/Bandage")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bbsn5f5x8wlspg4pbibqz6m5vin8c19nl224f3z3km0pkc97rwv"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (invoke "qmake" "Bandage.pro")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (substitute* "tests/bandage_command_line_tests.sh"
                 (("^bandagepath=.*")
                  (string-append "bandagepath=" (getcwd) "/Bandage\n")))
               (with-directory-excursion "tests"
                 (setenv "XDG_RUNTIME_DIR" (getcwd))
                 (invoke "./bandage_command_line_tests.sh")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "Bandage" (string-append out "/bin"))
               #t))))))
    (inputs
     (list qtbase-5 qtsvg))
    (native-inputs
     (list imagemagick))
    (home-page "https://rrwick.github.io/Bandage/")
    (synopsis
     "Bioinformatics Application for Navigating De novo Assembly Graphs Easily")
    (description "Bandage is a program for visualising de novo assembly graphs.
It allows users to interact with the assembly graphs made by de novo assemblers
such as Velvet, SPAdes, MEGAHIT and others.  De novo assembly graphs contain not
only assembled contigs but also the connections between those contigs, which
were previously not easily accessible.  Bandage visualises assembly graphs, with
connections, using graph layout algorithms.  Nodes in the drawn graph, which
represent contigs, can be automatically labelled with their ID, length or depth.
Users can interact with the graph by moving, labelling and colouring nodes.
Sequence information can also be extracted directly from the graph viewer.  By
displaying connections between contigs, Bandage opens up new possibilities for
analysing and improving de novo assemblies that are not possible by looking at
contigs alone.")
    (license (list license:gpl2+        ; bundled ogdf
                   license:gpl3+))))

(define-public libmaus2
  (package
    (name "libmaus2")
    (version "2.0.786")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/german.tischler/libmaus2")
                    (commit (string-append version "-release-20210531143054"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rxakmwjcx2yq5sjh3v849f7dfw4xzc2fyzf6s28s3p95z84w564"))))
    (build-system gnu-build-system)
    ;; The test suite attempts to execute ../test-driver, which does not exist.
    (arguments '(#:tests? #false))
    (propagated-inputs
     (list zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://gitlab.com/german.tischler/libmaus2")
    (synopsis "Collection of data structures and algorithms useful for bioinformatics")
    (description "libmaus2 is a collection of data structures and
algorithms.  It contains:

@itemize
@item I/O classes (single byte and UTF-8);
@item @code{bitio} classes (input, output and various forms of bit level
  manipulation);
@item text indexing classes (suffix and LCP array, fulltext and minute (FM),
  etc.);
@item BAM sequence alignment files input/output (simple and collating);
and many lower level support classes.
@end itemize\n")
    ;; The code is explicitly available under the terms of either GPLv2 or
    ;; GPLv3 according to the AUTHORS file, though most files have a GPLv3+
    ;; license header.
    (license (list license:gpl2+ license:gpl3+))))

(define-public biobambam2
  (package
    (name "biobambam2")
    (version "2.0.182")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/german.tischler/biobambam2")
                    (commit (string-append version "-release-20210412001032"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b7w7a2a7hpkgrdn0n7hy4pilzrj82zqrh7q4bg1l0cd6bqr60m5"))))
    (build-system gnu-build-system)
    (arguments
     ;; The test suite attempts to execute ../test-driver, which does not exist.
     `(#:tests? #false
       #:configure-flags
       ,#~(list (string-append "--with-libmaus2="
                               #$(this-package-input "libmaus2")))))
    (inputs
     (list libmaus2 xerces-c))
    (native-inputs
     (list pkg-config))
    (home-page "https://gitlab.com/german.tischler/biobambam2")
    (synopsis "Tools for processing BAM files")
    (description "This package contains some tools for processing BAM files
including:

@itemize
@item bamsormadup: parallel sorting and duplicate marking
@item bamcollate2: reads BAM and writes BAM reordered such that alignment or
  collated by query name
@item bammarkduplicates: reads BAM and writes BAM with duplicate alignments
  marked using the BAM flags field
@item bammaskflags: reads BAM and writes BAM while masking (removing) bits
  from the flags column
@item bamrecompress: reads BAM and writes BAM with a defined compression
  setting.  This tool is capable of multi-threading.
@item bamsort: reads BAM and writes BAM resorted by coordinates or query name
@item bamtofastq: reads BAM and writes FastQ; output can be collated or
  uncollated by query name.
@end itemize
")
    ;; The COPYING file states that the code is distributed under version 3 of
    ;; the GPL, but the license headers include the "or later" clause.
    (license license:gpl3+)))

(define-public r-circus
  (package
    (name "r-circus")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BIMSBbioinfo/ciRcus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jhjn3ilb057hbf6yzrihj13ifxxs32y7nkby8l3lkm28dg4p97h"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-annotationhub
           r-biomart
           r-data-table
           r-dbi
           r-genomicfeatures
           r-genomicranges
           r-ggplot2
           r-hash
           r-iranges
           r-rcolorbrewer
           r-rmysql
           r-s4vectors
           r-stringr
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/BIMSBbioinfo/ciRcus")
    (synopsis "Annotation, analysis and visualization of circRNA data")
    (description "Circus is an R package for annotation, analysis and
visualization of circRNA data.  Users can annotate their circRNA candidates
with host genes, gene featrues they are spliced from, and discriminate between
known and yet unknown splice junctions.  Circular-to-linear ratios of circRNAs
can be calculated, and a number of descriptive plots easily generated.")
    (license license:artistic2.0)))

(define-public r-doubletfinder
  (let ((commit "554097ba4e2c0ed7c28dc7f0b5b75277f3a50551")
        (revision "1"))
    (package
      (name "r-doubletfinder")
      (version (git-version "2.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/chris-mcginnis-ucsf/DoubletFinder")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1q1pnqw7ry4syp04wjmvz5bws6z4vg4c340ky07lk0vp577x2773"))))
      (properties `((upstream-name . "DoubletFinder")))
      (build-system r-build-system)
      (propagated-inputs (list r-fields r-kernsmooth r-rocr))
      (home-page "https://github.com/chris-mcginnis-ucsf/DoubletFinder")
      (synopsis "Identify doublets in single-cell RNA sequencing data")
      (description
       "DoubletFinder identifies doublets by generating artificial doublets
from existing scRNA-seq data and defining which real cells preferentially
co-localize with artificial doublets in gene expression space.  Other
DoubletFinder package functions are used for fitting DoubletFinder to
different scRNA-seq datasets.  For example, ideal DoubletFinder performance in
real-world contexts requires optimal pK selection and homotypic doublet
proportion estimation.  pK selection is achieved using pN-pK parameter sweeps
and maxima identification in mean-variance-normalized bimodality coefficient
distributions.  Homotypic doublet proportion estimation is achieved by finding
the sum of squared cell annotation frequencies.")
      (license license:cc0))))

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
               (url "https://github.com/gpertea/gffread")
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
                     (url "https://github.com/gpertea/gclib")
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
               (url "https://github.com/marvin-jens/find_circ")
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
                      (path (getenv "GUIX_PYTHONPATH")))
                 (for-each (lambda (script)
                             (install-file script bin)
                             (wrap-program (string-append bin "/" script)
                               `("GUIX_PYTHONPATH" ":" prefix (,path))))
                           '("cmp_bed.py"
                             "find_circ.py"
                             "maxlength.py"
                             "merge_bed.py"
                             "unmapped2anchors.py")))
               #t)))))
      (inputs
       (list python-2 python2-pysam python2-numpy))
      (home-page "https://github.com/marvin-jens/find_circ")
      (synopsis "circRNA detection from RNA-seq reads")
      (description "This package provides tools to detect head-to-tail
spliced (back-spliced) sequencing reads, indicative of circular RNA (circRNA)
in RNA-seq data.")
      (license license:gpl3))))

(define-public fit-sne
  (package
    (name "fit-sne")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KlugerLab/FIt-SNE")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1imq4577awc226wvygf94kpz156qdfw8xl0w0f7ss4w10lhmpmf5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #false ; there are none
       #:phases
       ;; There is no build system.
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "g++" "-std=c++11" "-O3"
                     "src/sptree.cpp"
                     "src/tsne.cpp"
                     "src/nbodyfft.cpp"
                     "-o" "bin/fast_tsne"
                     "-pthread" "-lfftw3" "-lm"
                     "-Wno-address-of-packed-member")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share/fit-sne")))
               (for-each (lambda (file) (install-file file bin))
                         (find-files "bin"))

               (substitute* "fast_tsne.R"
                 (("^FAST_TSNE_SCRIPT_DIR.*")
                  (string-append "FAST_TSNE_SCRIPT_DIR = \"" out "\"\n")))
               (install-file "fast_tsne.R" share)))))))
    (inputs
     (list fftw))
    (home-page "https://github.com/KlugerLab/FIt-SNE")
    (synopsis "Fast Fourier Transform-accelerated interpolation-based t-SNE")
    (description "@dfn{t-Stochastic Neighborhood Embedding} (t-SNE) is a
method for dimensionality reduction and visualization of high dimensional
datasets.  A popular implementation of t-SNE uses the Barnes-Hut algorithm to
approximate the gradient at each iteration of gradient descent.  This
implementation differs in these ways:

@itemize
@item Instead of approximating the N-body simulation using Barnes-Hut, we
  interpolate onto an equispaced grid and use FFT to perform the convolution.
@item Instead of computing nearest neighbors using vantage-point trees, we
  approximate nearest neighbors using the Annoy library.  The neighbor lookups
  are multithreaded to take advantage of machines with multiple cores.
@end itemize
")
    ;; See LICENSE.txt for details on what license applies to what files.
    (license (list license:bsd-4 license:expat license:asl2.0))))

(define-public python-scanpy
  (package
    (name "python-scanpy")
    (version "1.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/theislab/scanpy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "14zax23lqinv7xyv3491vpl3ydi38naiwaxg5mkfs5zk2406cqdr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" ,version)
             ;; ZIP does not support timestamps before 1980.
             (setenv "SOURCE_DATE_EPOCH" "315532800")
             (invoke "flit" "build")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (wheel)
                           (format #true wheel)
                           (invoke "python" "-m" "pip" "install"
                                   wheel (string-append "--prefix=" out)))
                         (find-files "dist" "\\.whl$")))))
         (replace 'check
           (lambda* (#:key tests? inputs #:allow-other-keys)
             (when tests?
               ;; These tests require Internet access.
               (delete-file-recursively "scanpy/tests/notebooks")
               (delete-file "scanpy/tests/test_clustering.py")
               (delete-file "scanpy/tests/test_datasets.py")
               (delete-file "scanpy/tests/test_score_genes.py")
               (delete-file "scanpy/tests/test_highly_variable_genes.py")

               ;; TODO: I can't get the plotting tests to work, even with Xvfb.
               (delete-file "scanpy/tests/test_embedding_plots.py")
               (delete-file "scanpy/tests/test_preprocessing.py")
               (delete-file "scanpy/tests/test_read_10x.py")

               ;; TODO: these fail with TypingError and "Use of unsupported
               ;; NumPy function 'numpy.split'".
               (delete-file "scanpy/tests/test_metrics.py")

               ;; The following tests requires 'scanorama', which isn't
               ;; packaged yet.
               (delete-file "scanpy/tests/external/test_scanorama_integrate.py")

               (setenv "PYTHONPATH"
                       (string-append (getcwd) ":"
                                      (assoc-ref inputs "python-anndata:source") ":"
                                      (getenv "GUIX_PYTHONPATH")))
               (invoke "pytest" "-vv"
                       "-k"
                       ;; Plot tests that fail.
                       (string-append "not test_dotplot_matrixplot_stacked_violin"
                                      " and not test_violin_without_raw"
                                      " and not test_correlation"
                                      " and not test_scatterplots"
                                      " and not test_scatter_embedding_add_outline_vmin_vmax_norm"
                                      " and not test_paga"
                                      " and not test_paga_compare"
                                      " and not test_clustermap"

                                      ;; These try to connect to the network
                                      " and not test_plot_rank_genes_groups_gene_symbols"
                                      " and not test_pca_chunked"
                                      " and not test_pca_sparse"
                                      " and not test_pca_reproducible"))))))))
    (propagated-inputs
     (list python-anndata
           python-h5py
           python-igraph
           python-joblib
           python-legacy-api-wrap
           python-louvain-0.7
           python-matplotlib
           python-natsort
           python-networkx
           python-numba
           python-packaging
           python-pandas
           python-patsy
           python-scikit-learn
           python-scipy
           python-seaborn
           python-sinfo
           python-statsmodels
           python-tables
           python-pytoml
           python-tqdm
           python-umap-learn))
    (native-inputs
     `(;; This package needs anndata.tests, which is not installed.
       ("python-anndata:source" ,(package-source python-anndata))
       ("python-flit" ,python-flit)
       ("python-leidenalg" ,python-leidenalg)
       ("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/theislab/scanpy")
    (synopsis "Single-Cell Analysis in Python")
    (description "Scanpy is a scalable toolkit for analyzing single-cell gene
expression data.  It includes preprocessing, visualization, clustering,
pseudotime and trajectory inference and differential expression testing.  The
Python-based implementation efficiently deals with datasets of more than one
million cells.")
    (license license:bsd-3)))

(define-public python-bbknn
  (package
    (name "python-bbknn")
    (version "1.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bbknn" version))
       (sha256
        (base32
         "1jbsh01f57zj4bhvjr3jh4532zznqd6nccmgrl3qi9gnhkf7c4y0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no tests are included
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-fail-to-find-sklearn
           (lambda _
             ;; XXX: I have no idea why it cannot seem to find sklearn.
             (substitute* "setup.py"
               (("'sklearn'") "")))))))
    (propagated-inputs
     (list python-annoy
           python-cython
           python-numpy
           python-scikit-learn
           python-scipy
           python-umap-learn))
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

(define-public python-drep
  (package
    (name "python-drep")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "drep" version))
       (sha256
        (base32
         "08vk0x6v5c5n7afgd5pcjhsvb424absypxy22hw1cm1n9kirbi77"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-biopython
           python-matplotlib
           python-numpy
           python-pandas
           python-pytest
           python-scikit-learn
           python-seaborn
           python-tqdm))
    (home-page "https://github.com/MrOlm/drep")
    (synopsis "De-replication of microbial genomes assembled from multiple samples")
    (description
     "dRep is a Python program for rapidly comparing large numbers of genomes.
dRep can also \"de-replicate\" a genome set by identifying groups of highly
similar genomes and choosing the best representative genome for each genome
set.")
    (license license:expat)))

(define-public instrain
  (package
    (name "instrain")
    (version "1.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "inStrain" version))
       (sha256
        (base32
         "05w1lw75x4lwkzg4qpi055g7hdjp9rnc4ksbxg2hfgksq9djk0hx"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-relative-imports
           (lambda _
             (substitute* "docker/run_instrain.py"
               (("from s3_utils")
                "from .s3_utils")
               (("from job_utils")
                "from .job_utils")))))))
    (inputs
     (list python-biopython-1.73
           python-boto3
           python-h5py
           python-lmfit
           python-matplotlib
           python-networkx
           python-numba
           python-numpy
           python-pandas
           python-psutil
           python-pysam
           python-scikit-learn
           python-seaborn
           python-tqdm
           ;; drep is needed for deprecated plot utilities
           python-drep))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/MrOlm/inStrain")
    (synopsis "Calculation of strain-level metrics")
    (description
     "inStrain is a Python program for analysis of co-occurring genome
populations from metagenomes that allows highly accurate genome comparisons,
analysis of coverage, microdiversity, and linkage, and sensitive SNP detection
with gene localization and synonymous non-synonymous identification.")
    ;; The tool itself says that the license is "MIT", but the repository
    ;; contains a LICENSE file with the GPLv3.
    ;; See https://github.com/MrOlm/inStrain/issues/51
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

(define-public intervaltree
  (let ((commit "b90527f9e6d51cd36ecbb50429e4524d3a418ea5"))
    (package
      (name "intervaltree")
      (version (git-version "0.0.0" "1" commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ekg/intervaltree/")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0rgv6q5fl4x5d74n6p5wvdna6zmbdbqpb4jqqh6vq3670gn08xad"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; No tests.
         #:make-flags
         ,#~(list (string-append "PREFIX=" #$output) "DESTDIR=\"\"")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)))) ; There is no configure phase.
      (home-page "https://github.com/ekg/intervaltree")
      (synopsis "Minimal C++ interval tree implementation")
      (description "An interval tree can be used to efficiently find a set of
numeric intervals overlapping or containing another interval.  This library
provides a basic implementation of an interval tree using C++ templates,
allowing the insertion of arbitrary types into the tree.")
      (license license:expat))))

(define-public python-intervaltree
  (package
    (name "python-intervaltree")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "intervaltree" version))
       (sha256
        (base32
         "0wz234g6irlm4hivs2qzmnywk0ss06ckagwh15nflkyb3p462kyb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; pytest seems to have a check to make sure the user is testing
         ;; their checked-out code and not an installed, potentially
         ;; out-of-date copy. This is harmless here, since we just installed
         ;; the package, so we disable the check to avoid skipping tests
         ;; entirely.
         (add-before 'check 'import-mismatch-error-workaround
           (lambda _
             (setenv "PY_IGNORE_IMPORTMISMATCH" "1")
             #t)))))
    (propagated-inputs
     (list python-sortedcontainers))
    (native-inputs
     (list python-pytest))
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
    (version "0.3.7")
    ;; The tarball on pypi does not include the makefile to build the
    ;; programs.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/4dn-dcic/pairix")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1snr3lrmsld8sy77ng6ba6wcmd33xjccf1l2f3m6pi29xis9nd6p"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-programs
           (lambda _ (invoke "make")))
         (add-after 'install 'install-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "bin" (string-append
                                      (assoc-ref outputs "out")
                                      "/bin"))
             #t)))))
    (inputs
     (list zlib))
    (home-page "https://github.com/4dn-dcic/pairix")
    (synopsis "Support for querying pairix-indexed bgzipped text files")
    (description
     "Pypairix is a Python module for fast querying on a pairix-indexed
bgzipped text file that contains a pair of genomic coordinates per line.")
    (license license:expat)))

(define-public python-pyfaidx
  (package
    (name "python-pyfaidx")
    (version "0.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyfaidx" version))
       (sha256
        (base32
         "038xi3a6zvrxbyyfpp64ka8pcjgsdq4fgw9cl5lpxbvmm1bzzw2q"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six))
    (home-page "http://mattshirley.com")
    (synopsis "Random access to fasta subsequences")
    (description
     "This package provides procedures for efficient pythonic random access to
fasta subsequences.")
    (license license:bsd-3)))

(define-public python2-pyfaidx
  (package-with-python2 python-pyfaidx))

(define-public python-cooler
  (package
    (name "python-cooler")
    (version "0.8.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cooler" version))
       (sha256
        (base32
         "1i96fmpsimj4wrx51rxn8lw2gqxf5a2pvrj5rwdd6ivnm3pmhyrn"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "tests/test_create.py"
              (("def test_roundtrip")
                 (string-append "@pytest.mark.skip(reason=\"requires network "
                                "access to genome.ucsc.edu\")\n"
                                "def test_roundtrip")))
             (substitute* "tests/test_util.py"
              (("def test_fetch_chromsizes")
                 (string-append "@pytest.mark.skip(reason=\"requires network "
                                "access to genome.ucsc.edu\")\n"
                                "def test_fetch_chromsizes")))
             ;; This test depends on ipytree, which contains a lot of minified
             ;; JavaScript.
             (substitute* "tests/test_fileops.py"
               (("def test_print_trees")
                "def _test_print_trees"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" "-v")))))))
    (propagated-inputs
     (list python-asciitree
           python-biopython
           python-click
           python-cytoolz-for-cooler
           python-dask
           python-h5py
           python-multiprocess
           python-numpy
           python-pandas
           python-pyfaidx
           python-pypairix
           python-pysam
           python-pyyaml
           python-scipy
           python-simplejson
           python-six
           python-sparse))
    (native-inputs
     (list python-codecov python-mock python-pytest python-pytest-cov
           python-pytest-flake8))
    ;; Almost all the projects of the Mirnylab are moved under Open2C umbrella
    (home-page "https://github.com/open2c/cooler")
    (synopsis "Sparse binary format for genomic interaction matrices")
    (description
     "Cooler is a support library for a sparse, compressed, binary persistent
storage format, called @code{cool}, used to store genomic interaction data,
such as Hi-C contact matrices.")
    (license license:bsd-3)))

(define-public python-hicmatrix
  (package
    (name "python-hicmatrix")
    (version "15")
      (source
        (origin
          ;;Pypi sources do not contain any test
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/deeptools/HiCMatrix")
                 (commit version)))
          (file-name (git-file-name name version))
          (sha256
            (base32
             "1dshjxgb16sdfg9k1bhw2yhyngac04k4ca7aqy8g3i3pprr068r5"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" "-v")))))))
    (propagated-inputs
     (list python-cooler
           python-intervaltree
           python-numpy
           python-pandas
           python-scipy
           python-tables))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/deeptools/HiCMatrix/")
    (synopsis "HiCMatrix class for HiCExplorer and pyGenomeTracks")
    (description
     "This helper package implements the @code{HiCMatrix} class for
the HiCExplorer and pyGenomeTracks packages.")
    (license license:gpl3+)))

(define-public python-hicexplorer
  (package
    (name "python-hicexplorer")
    (version "2.1.4")
    (source
     (origin
       ;; The latest version is not available on Pypi.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deeptools/HiCExplorer")
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
     (list python-biopython
           python-configparser
           python-cooler
           python-future
           python-intervaltree
           python-jinja2
           python-matplotlib
           python-numpy
           python-pandas
           python-pybigwig
           python-pysam
           python-scipy
           python-six
           python-tables
           python-unidecode))
    (home-page "https://hicexplorer.readthedocs.io")
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
    (version "3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyGenomeTracks" version))
       (sha256
        (base32
         "16laa0wnf4qn9fb9ych4w1vqhqwjss70v0y0f6wp4gwqfrlgac0f"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; there are none
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               (("matplotlib ==3.1.1")
                "matplotlib >=3.1.1"))
             #t)))))
    (propagated-inputs
     (list python-future
           python-gffutils
           python-hicmatrix
           python-intervaltree
           python-matplotlib
           python-numpy
           python-pybigwig
           python-pysam
           python-tqdm))
    (native-inputs
     (list python-pytest))
    (home-page "https://pygenometracks.readthedocs.io")
    (synopsis "Program and library to plot beautiful genome browser tracks")
    (description
     "This package aims to produce high-quality genome browser tracks that
are highly customizable.  Currently, it is possible to plot: bigwig, bed (many
options), bedgraph, links (represented as arcs), and Hi-C matrices.
pyGenomeTracks can make plots with or without Hi-C data.")
    (license license:gpl3+)))

(define-public python-iced
  (package
    (name "python-iced")
    (version "0.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iced" version))
       (sha256
        (base32
         "1avcjmpyyvhgbj5qca4l70ipiz7j3xmcw9p6rd9c06j99faa0r71"))))
    (build-system python-build-system)
    (arguments `(#:tests? #false)) ; there are none
    (propagated-inputs
     (list python-numpy python-pandas python-scipy python-scikit-learn))
    (home-page "https://github.com/hiclib/iced")
    (synopsis "ICE normalization")
    (description "This is a package for normalizing Hi-C contact counts
efficiently.")
    (license license:bsd-3)))

(define-public python-hic2cool
  (package
    (name "python-hic2cool")
    (version "0.8.3")
    ;; pypi sources do not contain the test_data directory and no test can be
    ;; run
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/4dn-dcic/hic2cool")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dlnf0qfcp4jrc1nyya32a035c13xicyq16bwfnwhbb9s47mz7gl"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Two of the test-data files need to be writable.
         (add-after 'unpack 'make-test-data-writable
           (lambda _
             (for-each make-file-writable
                       (list "test_data/hic2cool_0.4.2_single_res.cool"
                             "test_data/hic2cool_0.7.0_multi_res.mcool")))))))
    (propagated-inputs
     (list python-cooler python-h5py python-numpy python-pandas
           python-scipy))
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
     (list r-bit64 r-data-table r-rhdf5 r-shiny r-svdialogs))
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
        (commit "6ff0670a37ab3036aaf1d94aa4b208310946b0b5"))
    (package
      (name "r-xbioc")
      (version (git-version "0.1.16" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/renozao/xbioc")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0w8bsq5myiwkfhh83nm6is5ichiyvwa1axx2szvxnzq39x6knf66"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-annotationdbi
             r-assertthat
             r-biobase
             r-biocmanager
             r-digest
             r-pkgmaker
             r-plyr
             r-reshape2
             r-stringr))
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
                      (url "https://github.com/shenorrLab/csSAM")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "128syf9v39gk0z3ip000qpsjbg6l1siyq6c8b0hz41dzg5achyb3"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-formula
             r-ggplot2
             r-pkgmaker
             r-plyr
             r-rngtools
             r-scales))
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
                      (url "https://github.com/shenorrLab/bseqsc")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1prw13wa20f7wlc3gkkls66n1kxz8d28qrb8icfqdwdnnv8w5qg8"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-abind
             r-annotationdbi
             r-biobase
             r-cssam
             r-dplyr
             r-e1071
             r-edger
             r-ggplot2
             r-nmf
             r-openxlsx
             r-pkgmaker
             r-plyr
             r-preprocesscore
             r-rngtools
             r-scales
             r-stringr
             r-xbioc))
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
               (url "https://github.com/rrwick/Porechop")
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
               (url "https://github.com/arq5x/poretools")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bglj833wxpp3cq430p1d3xp085ls221js2y90w7ir2x5ay8l7am"))))
      (build-system python-build-system)
      ;; requires python >=2.7, <3.0, and the same for python dependencies
      (arguments `(#:python ,python-2))
      (inputs
       (list hdf5))
      (propagated-inputs
       (list python2-dateutil python2-h5py python2-matplotlib
             python2-pandas python2-seaborn))
      (home-page "https://poretools.readthedocs.io")
      (synopsis "Toolkit for working with nanopore sequencing data")
      (description
       "The MinION from Oxford Nanopore Technologies is a nanopore sequencer.
This @code{poretools} package is a flexible toolkit for exploring datasets
generated by nanopore sequencing devices for the purposes of quality control and
downstream analysis.  Poretools operates directly on the native FAST5, a variant
of the Hierarchical Data Format (HDF5) standard.")
      (license license:expat))))

(define-public jamm
  (package
    (name "jamm")
    (version "1.0.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mahmoudibrahim/JAMM")
             (commit (string-append "JAMMv" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0bsa5mf9n9q5jz7mmacrra41l7r8rac5vgsn6wv1fb52ya58b970"))))
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
     (list bash
           coreutils
           gawk
           perl
           r-minimal
           ;;("r-parallel" ,r-parallel)
           r-signal
           r-mclust))
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
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ngless-toolkit/ngless.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0pb9f6b0yk9p4cdwiym8r190q1bcdiwvc7i2s6rw54qgi8r3g6pj"))
       (patches (search-patches "ngless-unliftio.patch"))))
    (build-system haskell-build-system)
    (arguments
     `(#:haddock? #f ; The haddock phase fails with: NGLess/CmdArgs.hs:20:1:
                     ; error: parse error on input import
                     ; import Options.Applicative
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-Versions.hs
           (lambda _
             (substitute* "Makefile"
               (("BWA_VERSION = .*")
                (string-append "BWA_VERSION = "
                               ,(package-version bwa) "\n"))
               (("SAM_VERSION = .*")
                (string-append "SAM_VERSION = "
                               ,(package-version samtools) "\n"))
               (("PRODIGAL_VERSION = .*")
                (string-append "PRODIGAL_VERSION = "
                               ,(package-version prodigal) "\n"))
               (("MINIMAP2_VERSION = .*")
                (string-append "MINIMAP2_VERSION = "
                               ,(package-version minimap2) "\n")))
             (invoke "make" "NGLess/Dependencies/Versions.hs")
             #t))
         (add-after 'create-Versions.hs 'create-cabal-file
           (lambda _ (invoke "hpack") #t))
         ;; These tools are expected to be installed alongside ngless.
         (add-after 'install 'link-tools
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (symlink (search-input-file inputs "/bin/prodigal")
                        (string-append bin "ngless-" ,version "-prodigal"))
               (symlink (search-input-file inputs "/bin/minimap2")
                        (string-append bin "ngless-" ,version "-minimap2"))
               (symlink (search-input-file inputs "/bin/samtools")
                        (string-append bin "ngless-" ,version "-samtools"))
               (symlink (search-input-file inputs "/bin/bwa")
                        (string-append bin "ngless-" ,version "-bwa"))
               #t))))))
    (inputs
     (list prodigal
           bwa
           samtools
           minimap2
           ghc-aeson
           ghc-ansi-terminal
           ghc-async
           ghc-atomic-write
           ghc-bytestring-lexing
           ghc-conduit
           ghc-conduit-algorithms
           ghc-conduit-extra
           ghc-configurator
           ghc-convertible
           ghc-data-default
           ghc-diagrams-core
           ghc-diagrams-lib
           ghc-diagrams-svg
           ghc-double-conversion
           ghc-edit-distance
           ghc-either
           ghc-errors
           ghc-extra
           ghc-filemanip
           ghc-file-embed
           ghc-gitrev
           ghc-hashtables
           ghc-http-conduit
           ghc-inline-c
           ghc-inline-c-cpp
           ghc-int-interval-map
           ghc-missingh
           ghc-optparse-applicative
           ghc-regex
           ghc-safe
           ghc-safeio
           ghc-strict
           ghc-tar
           ghc-tar-conduit
           ghc-unliftio
           ghc-unliftio-core
           ghc-vector
           ghc-yaml
           ghc-zlib))
    (propagated-inputs
     (list r-r6 r-hdf5r r-iterators r-itertools r-matrix))
    (native-inputs
     (list ghc-hpack
           ghc-quickcheck
           ghc-test-framework
           ghc-test-framework-hunit
           ghc-test-framework-quickcheck2
           ghc-test-framework-th))
    (home-page "https://ngless.embl.de/")
    (synopsis "DSL for processing next-generation sequencing data")
    (description "Ngless is a domain-specific language for
@dfn{next-generation sequencing} (NGS) data processing.")
    (license license:expat)))

(define-public ghc-int-interval-map
  (let ((commit "678763de7fe6d7fa3f1c44b32d18ce58670270f4")
        (revision "1"))
    (package
      (name "ghc-int-interval-map")
      (version "0.0.0.0")
      (source
        (origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/ngless-toolkit/interval-to-int.git")
                 (commit commit)))
           (file-name (git-file-name name version))
          (sha256 (base32 "0fd728b5if89vj5j4f9y7k0b2xv2ycz5a21iy15wbdcf5bhim7i8"))))
      (build-system haskell-build-system)
      (inputs
        (list ghc-either ghc-primitive ghc-vector ghc-vector-algorithms))
      (native-inputs
        (list ghc-hedgehog
              ghc-tasty
              ghc-tasty-hedgehog
              ghc-tasty-hunit
              ghc-tasty-quickcheck
              ghc-tasty-th))
      (home-page "https://github.com/luispedro/interval-to-int#readme")
      (synopsis "Interval map structure in Haskell")
      (description "An interval map structure that is optimized for low
memory (each interval is represented by about 3 words + whatever the
cargo is) and has semantics that are appropriate for genomic intervals
(namely, intervals can overlap and queries will return all matches
together). It also designed to be used in two phases: a construction
phase + query phase).")
      (license license:expat))))

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
               (url "https://github.com/rrwick/Filtlong")
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
                      (path (getenv "GUIX_PYTHONPATH")))
                 (wrap-program (string-append out
                                              "/share/filtlong/scripts/histogram.py")
                   `("GUIX_PYTHONPATH" ":" prefix (,path))))
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
  (let ((commit "6331dc4f15b9dfabb954ba3fae9d76b6c3ca6377")
        (revision "1"))
    (package
      (name "nanopolish")
      (version (git-version "0.11.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jts/nanopolish")
               (commit commit)
               (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "15ikl3d37y49pwd7vx36xksgsqajhf24q7qqsnpl15dqqyy5qgbc"))
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
                       (string-append
                        (search-input-directory inputs "/include/eigen3")
                        ":" (or (getenv "CPATH") "")))))
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
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((pythonpath (getenv "GUIX_PYTHONPATH"))
                     (perl5lib (getenv "PERL5LIB"))
                     (scripts (string-append (assoc-ref outputs "out")
                                             "/share/nanopolish/scripts"))
                     (guile (search-input-file inputs "bin/guile")))
                 (for-each (lambda (file)
                             (wrap-program file `("GUIX_PYTHONPATH" ":" prefix (,pythonpath))))
                           (find-files scripts "\\.py"))
                 (for-each (lambda (file)
                             (wrap-script file #:guile guile
                                          `("PERL5LIB" ":" prefix (,perl5lib))))
                           (find-files scripts "\\.pl"))))))))
      (inputs
       `(("guile" ,guile-3.0) ; for wrappers
         ("eigen" ,eigen)
         ("hdf5" ,hdf5)
         ("htslib" ,htslib)
         ("perl" ,perl)
         ("bioperl" ,bioperl-minimal)
         ("perl-getopt-long" ,perl-getopt-long)
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
             (url "https://github.com/etal/cnvkit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g2f78k68yglmj4fsfmgs8idqv3di9aj53fg0ld0hqljg8chhh82"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-biopython
           python-future
           python-matplotlib
           python-numpy
           python-reportlab
           python-pandas
           python-pysam
           python-pyfaidx
           python-scipy
           ;; R packages
           r-dnacopy))
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
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KlugerLab/pyFIt-SNE")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f3n7wcmxbnqiisgimhpa6p5chqpb1hj69i6rpg2hv2671i8nn68"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ; there are none
    (propagated-inputs
     (list python-numpy))
    (inputs
     (list fftw))
    (native-inputs
     (list python-cython))
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
    (version "38.90")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/bbmap/BBMap_" version ".tar.gz"))
              (sha256
               (base32
                "1wb94bcc006qq86x77z2rz0lc8m9f1kpnw6gdhjfg9bdaqf56rm3"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "dist"
       #:tests? #f ; there are none
       #:make-flags
       ,#~(list (string-append "-Dmpijar="
                               #$(this-package-input "java-openmpi")
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
     (list gawk java-eclipse-jdt-core java-eclipse-jdt-compiler-apt
           java-openmpi))
    (home-page "https://sourceforge.net/projects/bbmap/")
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
                  (delete-file-recursively "third-party")))))
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
               (("../third-party/zlib-1.2.3/zlib.h") "zlib.h"))))
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
               (install-file "Columbus_manual.pdf" doc)))))))
    (inputs
     (list openmpi zlib))
    (native-inputs
     `(("texlive" ,(texlive-updmap.cfg (list texlive-latex-graphics
                                             texlive-fonts-ec
                                             texlive-hyperref)))))
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
         "0fgygyzqgrq32dv6a00biq1p1cwi6kbl5iqblxq1kklj6b2mzmhs"))
       (modules '((guix build utils)))
       ;; Delete generated C files.
       (snippet
        '(for-each delete-file (find-files "." "\\.c")))))
    (build-system python-build-system)
    (native-inputs
     (list python-joblib))
    (propagated-inputs
     (list python-click
           python-cython
           python-h5py
           python-loompy
           python-matplotlib
           python-numba
           python-numpy
           python-pandas
           python-pysam
           python-scikit-learn
           python-scipy))
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
     (list htslib
           r-minimal
           r-circlize
           r-genomicalignments
           r-genomicranges
           samtools
           star
           zlib))
    (home-page "https://github.com/suhrig/arriba")
    (synopsis "Gene fusion detection from RNA-Seq data")
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
             (url "https://github.com/MikkelSchubert/adapterremoval")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nf3ki5pfzalhrx2fr1y6pfqfi133yj2m7q4fj9irf5fb94bapwr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       ,#~(list "COLOR_BUILD=no"
                (string-append "PREFIX=" #$output))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list zlib))
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
               (url "https://github.com/matsen/pplacer")
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
               #t)))
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib))
      (inputs
       `(("zlib" ,zlib "static")
         ("gsl" ,gsl-static)
         ("ocaml-ounit" ,(package-with-ocaml4.07 ocaml-ounit))
         ("ocaml-batteries" ,(package-with-ocaml4.07 ocaml-batteries))
         ("ocaml-camlzip" ,(package-with-ocaml4.07 camlzip))
         ("ocaml-csv" ,(package-with-ocaml4.07 ocaml-csv))
         ("ocaml-sqlite3" ,(package-with-ocaml4.07 ocaml-sqlite3))
         ("ocaml-xmlm" ,(package-with-ocaml4.07 ocaml-xmlm))
         ("ocaml-mcl" ,(package-with-ocaml4.07 ocaml-mcl))
         ("ocaml-gsl" ,ocaml4.07-gsl-1)
         ("sqlite:static" ,sqlite "static")))
      (native-inputs
       `(("cddlib-src" ,(package-source cddlib))
         ("ocamlbuild" ,(package-with-ocaml4.07 ocamlbuild))
         ("pkg-config" ,pkg-config)))
      (propagated-inputs
       (list pplacer-scripts))
      (synopsis "Phylogenetic placement of biological sequences")
      (description
       "Pplacer places query sequences on a fixed reference phylogenetic tree
to maximize phylogenetic likelihood or posterior probability according to a
reference alignment.  Pplacer is designed to be fast, to give useful
information about uncertainty, and to offer advanced visualization and
downstream analysis.")
      (home-page "https://matsen.fhcrc.org/pplacer/")
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

(define-public checkm
  (package
    (name "checkm")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "checkm-genome" version))
       (sha256
        (base32
         "0i2nnki639hgjag17wlva2x0ymn37b4krqsf6akxddykhfbkdnkz"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Some tests fail for unknown reasons.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-HOME
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (inputs
     (list python-dendropy python-matplotlib python-numpy python-pysam
           python-scipy))
    (home-page "https://ecogenomics.github.io/CheckM/")
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

(define-public python2-checkm-genome
  (deprecated-package "python2-checkm-genome" checkm))

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
     (list python-pandas
           python-future
           python-scipy
           python-matplotlib
           python-regex
           python-pysam))
    (native-inputs
     (list python-cython))
    (home-page "https://github.com/CGATOxford/UMI-tools")
    (synopsis "Tools for analyzing unique modular identifiers")
    (description "This package provides tools for dealing with @dfn{Unique
Molecular Identifiers} (UMIs) and @dfn{Random Molecular Tags} (RMTs) in
genetic sequences.  There are six tools: the @code{extract} and
@code{whitelist} commands are used to prepare a fastq containing UMIs @code{+/-}
cell barcodes for alignment.  The remaining commands, @code{group},
@code{dedup}, and @{count}/@code{count_tab}, are used to identify PCR
duplicates using the UMIs and perform different levels of analysis depending
on the needs of the user.")
    (license license:expat)))

(define-public ataqv
  (package
    (name "ataqv")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ParkerLab/ataqv")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "031xr6jx1aprh26y5b1lv3gzrlmzg4alfl73vvshymx8cq8asrqi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       ,#~(list (string-append "prefix=" #$output)
                (string-append "BOOST_ROOT="
                               #$(this-package-input "boost"))
                (string-append "HTSLIB_ROOT="
                               #$(this-package-input "htslib")))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list boost htslib ncurses zlib))
    (native-inputs
     (list lcov))
    (home-page "https://github.com/ParkerLab/ataqv")
    (synopsis "Toolkit for quality control and visualization of ATAC-seq data")
    (description "This package provides a toolkit for measuring and comparing
ATAC-seq results.  It was written to make it easier to spot differences that
might be caused by ATAC-seq library prep or sequencing.  The main program,
@code{ataqv}, examines aligned reads and reports some basic metrics.")
    (license license:gpl3+)))

(define-public r-psiplot
  (package
    (name "r-psiplot")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kcha/psiplot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08438h16cfry5kqh3y9hs8q1b1a8bxhblsm75knviz5r6q0n1jxh"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-mass
           r-dplyr
           r-tidyr
           r-purrr
           r-readr
           r-magrittr
           r-ggplot2))
    (home-page "https://github.com/kcha/psiplot")
    (synopsis "Plot percent spliced-in values of alternatively-spliced exons")
    (description
     "PSIplot is an R package for generating plots of @dfn{percent
spliced-in} (PSI) values of alternatively-spliced exons that were computed by
vast-tools, an RNA-Seq pipeline for alternative splicing analysis.  The plots
are generated using @code{ggplot2}.")
    (license license:expat)))

(define-public vbz-compression
  (package
    (name "vbz-compression")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nanoporetech/vbz_compression/")
             (commit (string-append "v" version))
             ;; We include the streamvbyte sources
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1c6wsrnw03vsc5cfp2rdakly5xy55m9chjmy6v685yapdwirdky0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DENABLE_CONAN=OFF"
         ;; Python things aren't even installed, so we might as well
         ;; disable building them.
         "-DENABLE_PYTHON=OFF")))
    (inputs
     (list ;("hdf5" ,hdf5-1.10)
           `(,zstd "lib")))
    (native-inputs
     (list googlebenchmark))
    (home-page "https://github.com/nanoporetech/vbz_compression/")
    (synopsis "VBZ compression plugin for nanopore signal data")
    (description
     "VBZ Compression uses variable byte integer encoding to compress
nanopore signal data.  The performance of VBZ is achieved by taking
advantage of the properties of the raw signal and therefore is most
effective when applied to the signal dataset.")
    (license license:mpl2.0)))

(define-public python-ont-fast5-api
  (package
    (name "python-ont-fast5-api")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nanoporetech/ont_fast5_api")
             (commit (string-append "release_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01hj4751j424lzic2sc4bz1f8w7i7fpkjpy3rgghdyl5lyfyb4s4"))
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "ont_fast5_api/vbz_plugin"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'copy-plugin
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "ont_fast5_api/vbz_plugin/")
             (install-file (string-append
                            (assoc-ref inputs "vbz-compression")
                            "/hdf5/lib/plugin/libvbz_hdf_plugin.so")
                           "ont_fast5_api/vbz_plugin/"))))))
    (inputs
     (list vbz-compression))
    (propagated-inputs
     (list python-numpy python-h5py python-packaging python-progressbar33))
    (home-page "https://github.com/nanoporetech/ont_fast5_api")
    (synopsis "Interface to HDF5 files of the Oxford Nanopore fast5 file format")
    (description
     "This package provides a concrete implementation of the fast5 file schema
using the generic @code{h5py} library, plain-named methods to interact with
and reflect the fast5 file schema, and tools to convert between
@code{multi_read} and @code{single_read} formats.")
    (license license:mpl2.0)))

(define-public tbsp
  (let ((commit "dc30c03868233c5504299c9cb0d7b2064ba9cb41")
        (revision "2"))
    (package
      (name "tbsp")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/phoenixding/tbsp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1im0bik2hxkcb7jzkcxp5nqb30hd8lfraxml6i5ik52j6z3qqln1"))))
      (build-system python-build-system)
      (arguments
       '(#:tests? #f         ; no tests included
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'relax-requirements
             (lambda _
               (substitute* "setup.py"
                 ((", <3.0") ""))))))) ; matplotlib
      (inputs
       (list python-matplotlib
             python-networkx
             python-numpy
             python-pybigwig
             python-biopython-1.73
             python-scikit-learn
             python-scipy))
      (home-page "https://github.com/phoenixding/tbsp/")
      (synopsis "SNP-based trajectory inference")
      (description
       "Several studies focus on the inference of developmental and response
trajectories from single cell RNA-Seq (scRNA-Seq) data.  A number of
computational methods, often referred to as pseudo-time ordering, have been
developed for this task.  CRISPR has also been used to reconstruct lineage
trees by inserting random mutations.  The tbsp package implements an
alternative method to detect significant, cell type specific sequence
mutations from scRNA-Seq data.")
      (license license:expat))))

(define-public tabixpp
  (package
   (name "tabixpp")
   (version "1.1.0")
   (source (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/ekg/tabixpp")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1k2a3vbq96ic4lw72iwp5s3mwwc4xhdffjj584yn6l9637q9j1yd"))
     (modules '((guix build utils)))
     (snippet
      `(begin
         (delete-file-recursively "htslib") #t))))
   (build-system gnu-build-system)
   (inputs
    (list htslib zlib))
   (arguments
    `(#:tests? #f ; There are no tests to run.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; There is no configure phase.
        ;; The build phase needs overriding the location of htslib.
        (replace 'build
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((htslib-ref (assoc-ref inputs "htslib")))
              (invoke "make"
                      (string-append "HTS_LIB=" htslib-ref "/lib/libhts.a")
                      (string-append "INCLUDES= -I" htslib-ref "/include/htslib")
                      "HTS_HEADERS="    ; No need to check for headers here.
                      (string-append "LIBPATH=-L. -L" htslib-ref "/include"))
              (invoke "g++" "-shared" "-o" "libtabixpp.so" "tabix.o" "-lhts")
              (invoke "ar" "rcs" "libtabixpp.a" "tabix.o"))))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (lib (string-append out "/lib"))
                   (bin (string-append out "/bin")))
              (install-file "tabix++" bin)
              (install-file "libtabixpp.so" lib)
              (install-file "libtabixpp.a" lib)
              (install-file "tabix.hpp" (string-append out "/include"))
              (mkdir-p (string-append lib "/pkgconfig"))
              (with-output-to-file (string-append lib "/pkgconfig/tabixpp.pc")
                (lambda _
                  (format #t "prefix=~a~@
                          exec_prefix=${prefix}~@
                          libdir=${exec_prefix}/lib~@
                          includedir=${prefix}/include~@
                          ~@
                          ~@
                          Name: libtabixpp~@
                          Version: ~a~@
                          Description: C++ wrapper around tabix project~@
                          Libs: -L${libdir} -ltabixpp~@
                          Cflags: -I${includedir}~%"
                          out ,version)))
              #t))))))
   (home-page "https://github.com/ekg/tabixpp")
   (synopsis "C++ wrapper around tabix project")
   (description "This is a C++ wrapper around the Tabix project which abstracts
some of the details of opening and jumping in tabix-indexed files.")
   (license license:expat)))

(define-public smithwaterman
  (let ((commit "2610e259611ae4cde8f03c72499d28f03f6d38a7"))
    (package
      (name "smithwaterman")
      (version (git-version "0.0.0" "2" commit))
      (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/ekg/smithwaterman/")
              (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0i9d8zrxpiracw3mxzd9siybpy62p06rqz9mc2w93arajgbk45bs"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests to run.
         #:make-flags '("libsw.a" "all")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; There is no configure phase.
           (add-after 'unpack 'patch-source
             (lambda _
               (substitute* "Makefile"
                 (("-c ") "-c -fPIC "))
               #t))
           (add-after 'build 'build-dynamic
             (lambda _
               (invoke "g++"
                       "-shared" "-o" "libsmithwaterman.so"
                       "smithwaterman.o" "SmithWatermanGotoh.o"
                       "disorder.o" "BandedSmithWaterman.o"
                       "LeftAlign.o" "Repeats.o" "IndelAllele.o")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (lib (string-append out "/lib")))
                 (install-file "smithwaterman" bin)
                 (for-each
                   (lambda (file)
                     (install-file file (string-append out "/include/smithwaterman")))
                   (find-files "." "\\.h$"))
                 (install-file "libsmithwaterman.so" lib)
                 (install-file "libsw.a" lib)
                 (mkdir-p (string-append lib "/pkgconfig"))
                 (with-output-to-file (string-append lib "/pkgconfig/smithwaterman.pc")
                   (lambda _
                     (format #t "prefix=~a~@
                             exec_prefix=${prefix}~@
                             libdir=${exec_prefix}/lib~@
                             includedir=${prefix}/include/smithwaterman~@
                             ~@
                             ~@
                             Name: smithwaterman~@
                             Version: ~a~@
                             Description: smith-waterman-gotoh alignment algorithm~@
                             Libs: -L${libdir} -lsmithwaterman~@
                             Cflags: -I${includedir}~%"
                             out ,version))))
               #t)))))
      (home-page "https://github.com/ekg/smithwaterman")
      (synopsis "Implementation of the Smith-Waterman algorithm")
      (description "Implementation of the Smith-Waterman algorithm.")
      ;; The licensing terms are unclear: https://github.com/ekg/smithwaterman/issues/9.
      (license (list license:gpl2 license:expat)))))

(define-public multichoose
  (package
    (name "multichoose")
    (version "1.0.3")
    (source (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/ekg/multichoose/")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0ci5fqvmpamwgxvmyd79ygj6n3bnbl3vc7b6h1sxz58186sm3pfs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; Tests require node.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (include (string-append out "/include")))
               ;; TODO: There are Python modules for these programs too.
               (install-file "multichoose" bin)
               (install-file "multipermute" bin)
               (install-file "multichoose.h" include)
               (install-file "multipermute.h" include))
             #t)))))
    (home-page "https://github.com/ekg/multichoose")
    (synopsis "Efficient loopless multiset combination generation algorithm")
    (description "This library implements an efficient loopless multiset
combination generation algorithm which is (approximately) described in
\"Loopless algorithms for generating permutations, combinations, and other
combinatorial configurations.\", G. Ehrlich - Journal of the ACM (JACM),
1973. (Algorithm 7.)")
    (license license:expat)))

(define-public fsom
  (let ((commit "a6ef318fbd347c53189384aef7f670c0e6ce89a3"))
    (package
      (name "fsom")
      (version (git-version "0.0.0" "1" commit))
      (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/ekg/fsom/")
              (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0gw1lpvr812pywg9y546x0h1hhj261xwls41r6kqhddjlrcjc0pi"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests to run.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; There is no configure phase.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "fsom" bin)))))))
      (native-inputs
       (list gcc-6))
      (home-page "https://github.com/ekg/fsom")
      (synopsis "Manage SOM (Self-Organizing Maps) neural networks")
      (description "A tiny C library for managing SOM (Self-Organizing Maps)
neural networks.")
      (license license:gpl3))))

(define-public fastahack
  (package
    (name "fastahack")
    (version "1.0.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/ekg/fastahack/")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0rp1blskhzxf7vbh253ibpxbgl9wwgyzf1wbkxndi08d3j4vcss9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; Unclear how to run tests: https://github.com/ekg/fastahack/issues/15
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
           (add-after 'unpack 'patch-source
             (lambda _
               (substitute* "Makefile"
                 (("-c ") "-c -fPIC "))
               #t))
         (add-after 'build 'build-dynamic
           (lambda _
             (invoke "g++"
                     "-shared" "-o" "libfastahack.so"
                     "Fasta.o" "FastaHack.o" "split.o" "disorder.o")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (bin (string-append out "/bin")))
               (mkdir-p (string-append out "/include/fastahack"))
               (for-each
                 (lambda (file)
                   (install-file file (string-append out "/include/fastahack")))
                 (find-files "." "\\.h$"))
               (install-file "fastahack" bin)
               (install-file "libfastahack.so" lib)
               (mkdir-p (string-append lib "/pkgconfig"))
               (with-output-to-file (string-append lib "/pkgconfig/fastahack.pc")
                 (lambda _
                   (format #t "prefix=~a~@
                           exec_prefix=${prefix}~@
                           libdir=${exec_prefix}/lib~@
                           includedir=${prefix}/include/fastahack~@
                           ~@
                           ~@
                           Name: fastahack~@
                           Version: ~a~@
                           Description: Indexing and sequence extraction from FASTA files~@
                           Libs: -L${libdir} -lfastahack~@
                           Cflags: -I${includedir}~%"
                           out ,version))))
             #t)))))
    (home-page "https://github.com/ekg/fastahack")
    (synopsis "Indexing and sequence extraction from FASTA files")
    (description "Fastahack is a small application for indexing and
extracting sequences and subsequences from FASTA files.  The included library
provides a FASTA reader and indexer that can be embedded into applications
which would benefit from directly reading subsequences from FASTA files.  The
library automatically handles index file generation and use.")
    (license (list license:expat license:gpl2))))

(define-public vcflib
  (package
    (name "vcflib")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/vcflib/vcflib")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k1z3876kbzifj1sqfzsf3lgb4rw779hvkg6ryxbyq5bc2paj9kh"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "CMakeLists.txt"
             ((".*fastahack.*") "")
             ((".*smithwaterman.*") "")
             (("(pkg_check_modules\\(TABIXPP)" text)
              (string-append
                "pkg_check_modules(FASTAHACK REQUIRED fastahack)\n"
                "pkg_check_modules(SMITHWATERMAN REQUIRED smithwaterman)\n"
                text))
             (("\\$\\{TABIXPP_LIBRARIES\\}" text)
              (string-append "${FASTAHACK_LIBRARIES} "
                             "${SMITHWATERMAN_LIBRARIES} "
                             text)))
           (substitute* (find-files "." "\\.(h|c)(pp)?$")
             (("\"SmithWatermanGotoh.h\"") "<smithwaterman/SmithWatermanGotoh.h>")
             (("\"convert.h\"") "<smithwaterman/convert.h>")
             (("\"disorder.h\"") "<smithwaterman/disorder.h>")
             (("Fasta.h") "fastahack/Fasta.h"))
           (for-each delete-file-recursively
                     '("fastahack" "filevercmp" "fsom" "googletest" "intervaltree"
                       "libVCFH" "multichoose" "smithwaterman"))
           #t))))
    (build-system cmake-build-system)
    (inputs
     (list bzip2
           htslib
           fastahack
           perl
           python
           smithwaterman
           tabixpp
           xz
           zlib))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ;; Submodules.
       ;; This package builds against the .o files so we need to extract the source.
       ("filevercmp-src" ,(package-source filevercmp))
       ("fsom-src" ,(package-source fsom))
       ("intervaltree-src" ,(package-source intervaltree))
       ("multichoose-src" ,(package-source multichoose))))
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'build-shared-library
           (lambda _
             (substitute* "CMakeLists.txt"
               (("vcflib STATIC") "vcflib SHARED"))
             (substitute* "test/Makefile"
               (("libvcflib.a") "libvcflib.so"))
             #t))
         (add-after 'unpack 'unpack-submodule-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((unpack (lambda (source target)
                             (mkdir target)
                             (with-directory-excursion target
                               (if (file-is-directory? (assoc-ref inputs source))
                                   (copy-recursively (assoc-ref inputs source) ".")
                                   (invoke "tar" "xvf"
                                           (assoc-ref inputs source)
                                           "--strip-components=1"))))))
               (and
                (unpack "filevercmp-src" "filevercmp")
                (unpack "fsom-src" "fsom")
                (unpack "intervaltree-src" "intervaltree")
                (unpack "multichoose-src" "multichoose"))
               #t)))
         ;; This pkg-config file is provided by other distributions.
         (add-after 'install 'install-pkg-config-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pkgconfig (string-append out "/lib/pkgconfig")))
               (mkdir-p pkgconfig)
               (with-output-to-file (string-append pkgconfig "/vcflib.pc")
                 (lambda _
                   (format #t "prefix=~a~@
                           exec_prefix=${prefix}~@
                           libdir=${exec_prefix}/lib~@
                           includedir=${prefix}/include~@
                           ~@
                           Name: vcflib~@
                           Version: ~a~@
                           Requires: smithwaterman, fastahack, tabixpp~@
                           Description: C++ library for parsing and manipulating VCF files~@
                           Libs: -L${libdir} -lvcflib~@
                           Cflags: -I${includedir}~%"
                           out ,version)))
                 #t))))))
    (home-page "https://github.com/vcflib/vcflib/")
    (synopsis "Library for parsing and manipulating VCF files")
    (description "Vcflib provides methods to manipulate and interpret
sequence variation as it can be described by VCF.  It is both an API for parsing
and operating on records of genomic variation as it can be described by the VCF
format, and a collection of command-line utilities for executing complex
manipulations on VCF files.")
    (license license:expat)))

(define-public freebayes
  (package
    (name "freebayes")
    (version "1.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/freebayes/freebayes")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1l0z88gq57kva677a6xri5g9k2d9h9lk5yk1q2xmq64wqhv7dvc3"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "contrib/htslib")
                  #t))))
    (build-system meson-build-system)
    (inputs
     (list fastahack htslib smithwaterman tabixpp vcflib))
    (native-inputs
     `(("bash-tap" ,bash-tap)
       ("bc" ,bc)
       ("grep" ,grep)   ; Built with perl support.
       ("parallel" ,parallel)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("samtools" ,samtools)
       ("simde" ,simde)
       ;; This submodule is needed to run the tests.
       ("test-simple-bash-src"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/ingydotnet/test-simple-bash/")
                 (commit "124673ff204b01c8e96b7fc9f9b32ee35d898acc")))
           (file-name "test-simple-bash-src-checkout")
           (sha256
            (base32 "043plp6z0x9yf7mdpky1fw7zcpwn1p47px95w9mh16603zqqqpga"))))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bash-tap (assoc-ref inputs "bash-tap")))
               (substitute* (find-files "test/t")
                 (("BASH_TAP_ROOT=bash-tap")
                  (string-append "BASH_TAP_ROOT=" bash-tap "/bin"))
                 (("bash-tap/bash-tap-bootstrap")
                  (string-append bash-tap "/bin/bash-tap-bootstrap"))
                 (("source.*bash-tap-bootstrap")
                  (string-append "source " bash-tap "/bin/bash-tap-bootstrap")))
               (substitute* '("src/BedReader.cpp"
                              "src/BedReader.h")
                 (("../intervaltree/IntervalTree.h") "IntervalTree.h"))
               (substitute* "meson.build"
                 ;; Our pkg-config file is vcflib.pc
                 (("libvcflib") "vcflib")
                 (("vcflib_inc,") ""))
               #t)))
         (add-after 'unpack 'unpack-submodule-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "test/test-simple-bash")
             (copy-recursively (assoc-ref inputs "test-simple-bash-src")
                               "test/test-simple-bash")
             #t))
        ;; The slow tests take longer than the specified timeout.
        ,@(if (any (cute string=? <> (%current-system))
                   '("armhf-linux" "aarch64-linux"))
            '((replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "meson" "test" "--timeout-multiplier" "5"))
                #t)))
            '()))))
    (home-page "https://github.com/freebayes/freebayes")
    (synopsis "Haplotype-based variant detector")
    (description "FreeBayes is a Bayesian genetic variant detector designed to
find small polymorphisms, specifically SNPs (single-nucleotide polymorphisms),
indels (insertions and deletions), MNPs (multi-nucleotide polymorphisms), and
complex events (composite insertion and substitution events) smaller than the
length of a short-read sequencing alignment.")
    (license license:expat)))

(define-public samblaster
  (package
    (name "samblaster")
    (version "0.1.24")
    (source (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/GregoryFaust/samblaster")
            (commit (string-append "v." version))))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "0iv2ddfw8363vb2x8gr3p8g88whb6mb9m0pf71i2cqsbv6jghap7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "samblaster"
                           (string-append (assoc-ref outputs "out") "/bin"))
             #t)))))
    (home-page "https://github.com/GregoryFaust/samblaster")
    (synopsis "Mark duplicates in paired-end SAM files")
    (description "Samblaster is a fast and flexible program for marking
duplicates in read-id grouped paired-end SAM files.  It can also optionally
output discordant read pairs and/or split read mappings to separate SAM files,
and/or unmapped/clipped reads to a separate FASTQ file. When marking
duplicates, samblaster will require approximately 20MB of memory per 1M read
pairs.")
    (license license:expat)))

(define-public r-velocyto
  (let ((commit "d7790346cb99f49ab9c2b23ba70dcf9d2c9fc350")
        (revision "1"))
    (package
      (name "r-velocyto")
      (version (git-version "0.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/velocyto-team/velocyto.R")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "16wqf70j7rd7pay2q513iyz12i8n9vrpg1bisah4lddbcpx5dz1n"))))
      (build-system r-build-system)
      (inputs
       (list boost))
      (propagated-inputs
       (list r-hdf5r
             r-mass
             r-mgcv
             r-pcamethods
             r-rcpp
             r-rcpparmadillo
             ;; Suggested packages
             r-rtsne
             r-cluster
             r-abind
             r-h5
             r-biocgenerics
             r-genomicalignments
             r-rsamtools
             r-edger
             r-igraph))
      (home-page "https://velocyto.org")
      (synopsis "RNA velocity estimation in R")
      (description
       "This package provides basic routines for estimation of gene-specific
transcriptional derivatives and visualization of the resulting velocity
patterns.")
      (license license:gpl3))))

(define-public methyldackel
  (package
    (name "methyldackel")
    (version "0.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dpryan79/MethylDackel")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sfhf2ap75qxpnmy1ifgmxqs18rq8mah9mcgkby73vc6h0sw99ws"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       ,#~(list "CC=gcc"
                "CFLAGS=-fcommon"
                (string-append "prefix=" #$output "/bin/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("-lhts ") "-lhts -lBigWig ")
               (("install MethylDackel \\$\\(prefix\\)" match)
                (string-append "install -d $(prefix); " match))))))))
    (inputs
     (list curl ; XXX: needed by libbigwig
           htslib-1.9 libbigwig zlib))
    ;; Needed for tests
    (native-inputs
     `(("python" ,python-wrapper)))
    (home-page "https://github.com/dpryan79/MethylDackel")
    (synopsis "Universal methylation extractor for BS-seq experiments")
    (description
     "MethylDackel will process a coordinate-sorted and indexed BAM or CRAM
file containing some form of BS-seq alignments and extract per-base
methylation metrics from them.  MethylDackel requires an indexed fasta file
containing the reference genome as well.")
    ;; See https://github.com/dpryan79/MethylDackel/issues/85
    (license license:expat)))

;; This package bundles PCRE 8.02 and cannot be built with the current
;; version.
(define-public phast
  (package
    (name "phast")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/CshlSiepelLab/phast")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10lpbllvny923jjbbyrpxahhd1m5h7sbj9gx7rd123rg10mlidki"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       ,#~(list "CC=gcc"
                (string-append "DESTDIR=" #$output))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Fix syntax
             (substitute* "test/Makefile"
               (("        ") "	"))
             (substitute* "Makefile"
               (("CLAPACKPATH=/usr/lib")
                (string-append "CLAPACKPATH="
                               (assoc-ref inputs "clapack") "/lib")))
             ;; Renaming the libraries is not necessary with our version of
             ;; CLAPACK.
             (substitute* "src/lib/Makefile"
               (("ifdef CLAPACKPATH") "ifdef UNNECESSARY"))
             (substitute* "src/make-include.mk"
               (("-lblaswr") "-lblas")
               (("-ltmg") "-ltmglib")
               (("liblapack.a") "liblapack.so")
               (("libblas.a") "libblas.so")
               (("libf2c.a") "libf2c.so"))
             (substitute* "src/Makefile"
               (("/opt") "/share")
               (("/usr/") "/"))
             #t))
         (replace 'check
           (lambda _
             (setenv "PATH"
                     (string-append (getcwd) "/bin:" (getenv "PATH")))
             ;; Disable broken test
             (substitute* "test/Makefile"
               ((".*if.*hmrc_summary" m) (string-append "#" m)))
             ;; Only run the msa_view tests because the others fail for
             ;; unknown reasons.
             (invoke "make" "-C" "test" "msa_view"))))))
    (inputs
     (list clapack))
    (native-inputs
     (list perl))
    (home-page "http://compgen.cshl.edu/phast/")
    (synopsis "Phylogenetic analysis with space/time models")
    (description
     "Phylogenetic Analysis with Space/Time models (PHAST) is a collection of
command-line programs and supporting libraries for comparative and
evolutionary genomics.  Best known as the search engine behind the
Conservation tracks in the University of California, Santa Cruz (UCSC) Genome
Browser, PHAST also includes several tools for phylogenetic modeling,
functional element identification, as well as utilities for manipulating
alignments, trees and genomic annotations.")
    (license license:bsd-3)))

(define-public python-gffutils
  ;; The latest release is older more than a year than the latest commit
  (let ((commit "4034c54600813b1402945e12faa91b3a53162cf1")
        (revision "1"))
    (package
      (name "python-gffutils")
      (version (git-version "0.9" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/daler/gffutils")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rwafjdnbir5wnk0ap06ww4lra3p5frhy7mfs03rlldgfnwxymsn"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda _
               ;; Tests need to access the HOME directory
               (setenv "HOME" "/tmp")
               (invoke "nosetests" "-a" "!slow")))
           (add-after 'unpack 'make-gz-files-writable
             (lambda _
               (for-each make-file-writable
                         (find-files "." "\\.gz"))
               #t)))))
      (propagated-inputs
       (list python-argcomplete
             python-argh
             python-biopython
             python-pybedtools
             python-pyfaidx
             python-simplejson
             python-six))
      (native-inputs
       (list python-nose))
      (home-page "https://github.com/daler/gffutils")
      (synopsis "Tool for manipulation of GFF and GTF files")
      (description
       "python-gffutils is a Python package for working with and manipulating
the GFF and GTF format files typically used for genomic annotations.  The
files are loaded into a SQLite database, allowing much more complex
manipulation of hierarchical features (e.g., genes, transcripts, and exons)
than is possible with plain-text methods alone.")
      (license license:expat))))

(define-public indelfixer
  (package
    (name "indelfixer")
    (version "1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cbg-ethz/InDelFixer/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10ak05x8i1bx2p7rriv2rglqg1wr7c8wrhjrqlq1wm7ka99w8i79"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "InDelFixer.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"))
    (inputs
     `(("java-commons-lang2" ,java-commons-lang)
       ("java-args4j" ,java-args4j)))
    (native-inputs
     (list java-junit))
    (home-page "https://github.com/cbg-ethz/InDelFixer/")
    (synopsis "Iterative and sensitive NGS sequence aligner")
    (description "InDelFixer is a sensitive aligner for 454, Illumina and
PacBio data, employing a full Smith-Waterman alignment against a reference.
This Java command line application aligns Next-Generation Sequencing (NGS) and
third-generation reads to a set of reference sequences, by a prior fast k-mer
matching and removes indels, causing frame shifts.  In addition, only a
specific region can be considered.  An iterative refinement of the alignment
can be performed, by alignment against the consensus sequence with wobbles.
The output is in SAM format.")
    (license license:gpl3+)))

(define-public libsbml
  (package
    (name "libsbml")
    (version "5.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/sbml/libsbml/"
                                  version "/stable/libSBML-"
                                  version "-core-src.tar.gz"))
              (sha256
               (base32
                "0slkagrk3nfi2qsksv6b1brj6zhx4bj4bkib2sdycvrcd10ql2lh"))))
    (build-system cmake-build-system)
    (arguments
     `(#:test-target "test"
       #:configure-flags
       ,#~(list "-DWITH_CHECK=ON"
                (string-append "-DLIBXML_LIBRARY="
                               #$(this-package-input "libxml2")
                               "/lib/libxml2.so")
                (string-append "-DLIBXML_INCLUDE_DIR="
                               #$(this-package-input "libxml2")
                               "/include/libxml2"))))
    (propagated-inputs
     (list libxml2))
    (native-inputs
     (list check-0.14 swig))
    (home-page "http://sbml.org/Software/libSBML")
    (synopsis "Process SBML files and data streams")
    (description "LibSBML is a library to help you read, write, manipulate,
translate, and validate SBML files and data streams.  The @dfn{Systems Biology
Markup Language} (SBML) is an interchange format for computer models of
biological processes.  SBML is useful for models of metabolism, cell
signaling, and more.  It continues to be evolved and expanded by an
international community.")
    (license license:lgpl2.1+)))

(define-public kraken2
  (package
    (name "kraken2")
    (version "2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DerrickWood/kraken2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h7a7vygd7y5isbrnc6srwq6xj1rmyd33pm8mmcgfkmlxlg5vkg3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #false                  ; there are none
       #:make-flags
       ,#~(list "-C" "src"
                (string-append "KRAKEN2_DIR=" #$output "/bin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'install-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
                    (replacements `(("KRAKEN2_DIR" . ,bin)
                                    ("VERSION" . ,,version))))
               (mkdir-p bin)

               (with-directory-excursion "scripts"
                 (let ((scripts (find-files "." ".*")))
                   (substitute* scripts
                     (("#####=([^=]+)=#####" _ key)
                      (or (assoc-ref replacements key)
                          (error (format #false "unknown key: ~a~%" key)))))
                   (substitute* "kraken2"
                     (("compression_program = \"bzip2\"")
                      (string-append "compression_program = \""
                                     (which "bzip2")
                                     "\""))
                     (("compression_program = \"gzip\"")
                      (string-append "compression_program = \""
                                     (which "gzip")
                                     "\"")))
                   (substitute* '("download_genomic_library.sh"
                                  "download_taxonomy.sh"
                                  "16S_gg_installation.sh"
                                  "16S_silva_installation.sh"
                                  "16S_rdp_installation.sh")
                     (("wget") (which "wget")))
                   (substitute* '("download_taxonomy.sh"
			          "download_genomic_library.sh"
			          "rsync_from_ncbi.pl")
		     (("rsync -")
                      (string-append (which "rsync") " -")))
                   (substitute* "mask_low_complexity.sh"
                     (("which") (which "which")))
                   (substitute* '("mask_low_complexity.sh"
                                  "download_genomic_library.sh"
                                  "16S_silva_installation.sh")
                     (("sed -e ")
                      (string-append (which "sed") " -e ")))
                   (substitute* '("rsync_from_ncbi.pl"
                                  "16S_rdp_installation.sh"
                                  "16S_silva_installation.sh"
                                  "16S_gg_installation.sh"
                                  "download_taxonomy.sh"
                                  "download_genomic_library.sh")
                     (("gunzip") (which "gunzip")))
                   (for-each (lambda (script)
                               (chmod script #o555)
                               (install-file script bin))
                             scripts)))))))))
    (inputs
     (list gzip
           perl
           rsync
           sed
           wget
           which))
  (home-page "https://github.com/DerrickWood/kraken2")
  (synopsis "Taxonomic sequence classification system")
  (description "Kraken is a taxonomic sequence classifier that assigns
taxonomic labels to DNA sequences.  Kraken examines the k-mers within a query
sequence and uses the information within those k-mers to query a
database. That database maps k-mers to the lowest common ancestor (LCA) of all
genomes known to contain a given k-mer.")
  (license license:expat)))

(define-public lofreq
  (package
    (name "lofreq")
    (version "2.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/CSB5/lofreq")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qssrn3mgjak7df6iqc1rljqd3g3a5syvg0lsv4vds43s3fq23bl"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "bug-tests"
       #:tests? #false)) ; test data are not included
    (inputs
     `(("htslib" ,htslib)
       ("python" ,python-wrapper)
       ("zlib" ,zlib)))
    (native-inputs
     (list autoconf automake which))
    (home-page "https://csb5.github.io/lofreq/")
    (synopsis "Sensitive variant calling from sequencing data")
    (description "LoFreq is a fast and sensitive variant-caller for inferring
SNVs and indels from next-generation sequencing data.  It makes full use of
base-call qualities and other sources of errors inherent in
sequencing (e.g. mapping or base/indel alignment uncertainty), which are
usually ignored by other methods or only used for filtering.")
    (license license:expat)))

(define-public ivar
  (package
    (name "ivar")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/andersen-lab/ivar")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "044xa0hm3b8fga64csrdx05ih8w7kwmvcdrdrhkg8j11ml4bi4xv"))))
    (build-system gnu-build-system)
    (arguments `(#:parallel-tests? #false)) ; not supported
    (inputs
     (list htslib zlib))
    (native-inputs
     (list autoconf automake))
    (home-page "https://andersen-lab.github.io/ivar/html/")
    (synopsis "Tools for amplicon-based sequencing")
    (description "iVar is a computational package that contains functions
broadly useful for viral amplicon-based sequencing.")
    (license license:gpl3+)))

(define-public python-pyliftover
  (package
    (name "python-pyliftover")
    (version "0.4")
    ;; The version of pypi does not include test data.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/konstantint/pyliftover")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j8jp9iynv2l3jv5pr0pn0p3azlama1bqg233piglzm6bqh3m2m3"))))
    (build-system python-build-system)
    (arguments `(#:tests? #false)) ; the tests access the web
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/konstantint/pyliftover")
    (synopsis "Python implementation of UCSC liftOver genome coordinate conversion")
    (description
     "PyLiftover is a library for quick and easy conversion of genomic (point)
coordinates between different assemblies.")
    (license license:expat)))

(define-public python-cgatcore
  (package
    (name "python-cgatcore")
    (version "0.6.7")
    ;; The version of pypi does not include test data.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cgat-developers/cgat-core")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17vk88v1bx7x02ibzkc9i7ir4b5p1hcjr38jpsfzyzxr68352d5k"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-references
           (lambda _
             (substitute* "cgatcore/pipeline/execution.py"
               (("#!/bin/bash") (string-append "#!" (which "bash")))
               (("executable=\"/bin/bash\"")
                (string-append "executable=\"" (which "bash") "\""))
               (("\\\\time") (which "time")))))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               ;; Requires network access
               (delete-file "tests/test_pipeline_execution.py")
               (invoke "python" "-m" "pytest" "-v")))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("lsof" ,lsof)
       ("hostname" ,inetutils)
       ("openssl" ,openssl)))
    (inputs
     (list time))
    (propagated-inputs
     (list python-apsw
           python-gevent
           python-pandas
           python-paramiko
           python-pyyaml
           python-ruffus
           python-sqlalchemy))
    (home-page "https://github.com/cgat-developers/cgat-core")
    (synopsis "Computational genomics analysis toolkit")
    (description
     "CGAT-core is a set of libraries and helper functions used to enable
researchers to design and build computational workflows for the analysis of
large-scale data-analysis.")
    (license license:expat)))

(define-public perl-cworld-dekker
  (package
    (name "perl-cworld-dekker")
    (version "1.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dekkerlab/cworld-dekker.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dvh23fx52m59y6304xi2j2pl2hiqadlqg8jyv2pm14j1hy71ych"))))
    (build-system perl-build-system)
    (arguments
     `(#:modules ((guix build perl-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'hardcode-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bedtools (assoc-ref inputs "bedtools"))
                   (r (assoc-ref inputs "r-minimal")))
               (substitute* '("scripts/python/getEigenVectors.py"
                              "scripts/python/matrix2EigenVectors.py")
                 (("bedtools intersect")
                  (string-append bedtools "/bin/bedtools intersect")))
               (substitute* "lib/cworld/dekker.pm"
                 (("bedtools --version")
                  (string-append bedtools "/bin/bedtools --version")))
               (substitute* '("scripts/perl/correlateMatrices.pl"
                              "scripts/perl/matrix2scaling.pl"
                              "scripts/perl/matrix2distance.pl"
                              "scripts/perl/coverageCorrect.pl"
                              "scripts/perl/matrix2anchorPlot.pl"
                              "scripts/python/matrix2EigenVectors.py"
                              "scripts/python/matrix2insulation-lite.py"
                              "scripts/perl/matrix2compartment.pl"
                              "scripts/perl/anchorPurge.pl"
                              "scripts/perl/applyCorrection.pl"
                              "scripts/perl/compareInsulation.pl"
                              "scripts/perl/fillMissingData.pl"
                              "scripts/perl/matrix2loess.pl"
                              "scripts/python/getEigenVectors.py"
                              "scripts/perl/aggregateBED.pl"
                              "scripts/perl/collapseMatrix.pl"
                              "scripts/perl/matrix2direction.pl"
                              "scripts/perl/singletonRemoval.pl"
                              "lib/cworld/dekker.pm"
                              "scripts/perl/matrix2insulation.pl")
                 (("(`|\")Rscript" _ pre)
                  (string-append pre r "/bin/Rscript"))))))
         (add-after 'install 'install-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share/cworld-dekker")))
               (mkdir-p share)
               (copy-recursively "scripts" share)

               ;; Make all scripts executable and wrap them.
               (let ((r     (find-files share "\\.R$"))
                     (py    (find-files share "\\.py$"))
                     (pl    (find-files share "\\.pl$"))
                     (wrap  (lambda* (script var #:optional (extra ""))
                              (let ((path (string-append (getenv var)
                                                         extra)))
                                (wrap-program script
                                  `(,var ":" prefix (,path)))))))
                 (for-each (cut chmod <> #o555) (append r py pl))
                 (for-each (cut wrap <> "PERL5LIB"
                                (string-append ":" out
                                               "/lib/perl5/site_perl"))
                           pl)
                 (for-each (cut wrap <> "GUIX_PYTHONPATH") py))))))))
    (inputs
     `(("libgd" ,gd)
       ("perl-gd" ,perl-gd)
       ("bedtools" ,bedtools)
       ("python" ,python-wrapper)
       ("python-scipy" ,python-scipy)
       ("python-numpy" ,python-numpy)
       ("python-matplotlib" ,python-matplotlib)
       ("python-h5py" ,python-h5py)
       ("python-scikit-learn" ,python-scikit-learn)
       ("r-minimal" ,r-minimal)))
    (native-inputs
     (list perl-module-build))
    (home-page "https://github.com/dekkerlab/cworld-dekker")
    (synopsis "Utility and analysis scripts for 3C, 4C, 5C, and Hi-C data")
    (description "This package is a collection of Perl, Python, and R
scripts for manipulating 3C/4C/5C/Hi-C data.")
    (license license:asl2.0)))

(define-public ensembl-vep
  (let* ((api-version "103")
         (api-module
          (lambda (name hash)
            (origin (method git-fetch)
                    (uri (git-reference
                          (url (string-append "https://github.com/Ensembl/"
                                              name ".git"))
                          (commit (string-append "release/" api-version))))
                    (file-name (string-append name "-" api-version "-checkout"))
                    (sha256 (base32 hash))))))
    (package
      (name "ensembl-vep")
      (version (string-append api-version ".1"))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Ensembl/ensembl-vep.git")
               (commit (string-append "release/" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1iq7p72cv9b38jz2v8a4slzy2n8y0md487943180ym9xc8qvw09c"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 match))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           ;; Tests need to run after installation
           (delete 'check)
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((modules '(("ensembl" "/")
                                 ("ensembl-variation" "/Variation")
                                 ("ensembl-funcgen"   "/Funcgen")
                                 ("ensembl-io"        "/")))
                      (scripts '(("convert_cache.pl" "vep_convert_cache.pl")
                                 ("INSTALL.pl"       "vep_install.pl")
                                 ("haplo"            #f)
                                 ("variant_recoder"  #f)
                                 ("filter_vep"       #f)
                                 ("vep"              #f)))
                      (out  (assoc-ref outputs "out"))
                      (bin  (string-append out "/bin"))
                      (perl (string-append out "/lib/perl5/site_perl")))
                 (for-each
                  (match-lambda
                    ((name path)
                     (let ((dir (string-append perl "/Bio/EnsEMBL" path)))
                       (mkdir-p dir)
                       (copy-recursively
                        (string-append (assoc-ref inputs (string-append "api-module-" name))
                                       "/modules/Bio/EnsEMBL" path)
                        dir))))
                  modules)
                 (copy-recursively "modules/" perl)
                 (mkdir-p bin)
                 (for-each
                  (match-lambda
                    ((script new-name)
                     (let ((location (string-append bin "/"
                                                    (or new-name (basename script)))))
                       (copy-file script location)
                       (chmod location #o555)
                       (wrap-program location
                         `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB")
                                                  ,perl))))))
                  scripts)

                 ;; Fix path to tools
                 (with-directory-excursion (string-append perl "/Bio/EnsEMBL")
                   (substitute* '("Funcgen/RunnableDB/ProbeMapping/PrePipelineChecks.pm"
                                  "VEP/BaseRunner.pm"
                                  "VEP/Utils.pm"
                                  "VEP/AnnotationSource/Cache/VariationTabix.pm"
                                  "VEP/AnnotationSource/Cache/BaseSerialized.pm"
                                  "Variation/Utils/BaseVepTabixPlugin.pm"
                                  "Variation/Utils/VEP.pm"
                                  "Variation/Pipeline/ReleaseDataDumps/PreRunChecks.pm")
                     (("`which")
                      (string-append "`"
                                     (assoc-ref inputs "which")
                                     "/bin/which")))))))
           (add-after 'install 'check
             (lambda* (#:key tests? inputs outputs #:allow-other-keys)
               (when tests?
                 (setenv "PERL5LIB"
                         (string-append (getenv "PERL5LIB")
                                        ":"
                                        (assoc-ref outputs "out")
                                        "/lib/perl5/site_perl"))
                 (copy-recursively (string-append (assoc-ref inputs "source") "/t")
                                   "/tmp/t")
                 (for-each make-file-writable (find-files "/tmp/t"))
                 ;; TODO: haplo needs Set/IntervalTree.pm
                 (invoke "perl" "-e" (string-append "
use Test::Harness; use Test::Exception;
my $dirname = \"/tmp\";
opendir TEST, \"$dirname\\/t\";
my @test_files = map {\"$dirname\\/t\\/\".$_} grep {!/^\\./ && /\\.t$/} readdir TEST; closedir TEST;
@test_files = grep {!/Haplo/} @test_files;
runtests(@test_files);
"))))))))
      (inputs
       (list bioperl-minimal
             perl-bio-db-hts
             perl-dbi
             perl-dbd-mysql
             perl-libwww
             perl-http-tiny
             perl-json
             which))
      (propagated-inputs
       (list kentutils))
      (native-inputs
       `(("unzip" ,unzip)
         ("perl" ,perl)
         ("api-module-ensembl"
          ,(api-module "ensembl"
                       "0s59rj905g72hljzfpvnx5nxwz925b917y4jp912i23f5gwxh14v"))
         ("api-module-ensembl-variation"
          ,(api-module "ensembl-variation"
                       "1dvwdzzfjhzymq02b6n4p6j3a9q4jgq0g89hs7hj1apd7zhirgkq"))
         ("api-module-ensembl-funcgen"
          ,(api-module "ensembl-funcgen"
                       "1x23pv38dmv0w0gby6rv3wds50qghb4v3v1mf43vk55msfxzry8n"))
         ("api-module-ensembl-io"
          ,(api-module "ensembl-io"
                       "14adb2x934lzsq20035mazdkhrkcw0qzb0xhz6zps9vk4wixwaix"))
         ("perl-test-harness" ,perl-test-harness)
         ("perl-test-exception" ,perl-test-exception)))
      (home-page "http://www.ensembl.org/vep")
      (synopsis "Predict functional effects of genomic variants")
      (description
       "This package provides a Variant Effect Predictor, which predicts
the functional effects of genomic variants.  It also provides
Haplosaurus, which uses phased genotype data to predict
whole-transcript haplotype sequences, and Variant Recoder, which
translates between different variant encodings.")
      (license license:asl2.0))))

(define-public r-signac
  (let ((commit "e0512d348adeda4a3f23a2e8f56d1fe09840e03c")
        (revision "1"))
    (package
      (name "r-signac")
      (version (git-version "1.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/timoast/signac/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1yihhrv7zs87ax61la1nb4y12lg3knraw4b20k5digbcwm8488lb"))))
      (properties `((upstream-name . "Signac")))
      (build-system r-build-system)
      (inputs (list zlib))
      (propagated-inputs
       (list r-annotationfilter
             r-biocgenerics
             r-biostrings
             r-biovizbase
             r-data-table
             r-dplyr
             r-fastmatch
             r-future
             r-future-apply
             r-genomeinfodb
             r-genomicranges
             r-ggbio
             r-ggforce
             r-ggplot2
             r-ggrepel
             r-ggseqlogo
             r-iranges
             r-irlba
             r-lsa
             r-matrix
             r-patchwork
             r-pbapply
             r-rcpp
             r-rcpproll
             r-rsamtools
             r-s4vectors
             r-scales
             r-seurat
             r-seuratobject
             r-stringi
             r-tidyr))
      (home-page "https://github.com/timoast/signac/")
      (synopsis "Analysis of single-cell chromatin data")
      (description
       "This package provides a framework for the analysis and exploration of
single-cell chromatin data.  The Signac package contains functions for
quantifying single-cell chromatin data, computing per-cell quality control
metrics, dimension reduction and normalization, visualization, and DNA
sequence motif analysis.")
      (license license:expat))))

(define-public tombo
  (package
    (name "tombo")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ont-tombo" version))
       (sha256
        (base32
         "1023hadgcsgi53kz53ql45207hfizf9sw57z0qij3ay1bx68zbpm"))))
    (build-system python-build-system)
    (native-inputs
     (list python-cython python-nose2))
    ;; The package mainly consists of a command-line tool, but also has a
    ;; Python-API. Thus these must be propagated.
    (propagated-inputs
     (list python-future
           python-h5py
           python-mappy
           python-numpy
           python-scipy
           python-tqdm
           python-rpy2))
    (home-page "https://github.com/nanoporetech/tombo")
    (synopsis "Analysis of raw nanopore sequencing data")
    (description "Tombo is a suite of tools primarily for the identification of
modified nucleotides from nanopore sequencing data.  Tombo also provides tools
for the analysis and visualization of raw nanopore signal.")
    ;; Some parts may be BSD-3-licensed.
    (license license:mpl2.0)))

(define-public python-pyvcf
  (package
    (name "python-pyvcf")
    (version "0.6.8")
    ;; Use git, because the PyPI tarballs lack test data.
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jamescasbon/PyVCF.git")
               ;; Latest release is not tagged.
               (commit "bfcedb9bad1a14074ac4526ffdb610611e073810")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0c7lsssns3zp8fh2ibllzzra003srg9vbxqzmq6654akbzdb7lrf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'patch-sample-script
            (lambda _
              ;; Add Python 3 compatibility to this sample script.
              (substitute* "scripts/vcf_sample_filter.py"
                (("print (.*)\n" _ arg)
                 (string-append "print(" arg ")\n")))))
          (add-after 'install 'remove-installed-tests
            ;; Do not install test files.
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (delete-file-recursively (string-append
                                         (site-packages inputs outputs)
                                         "/vcf/test")))))))
    (native-inputs
     ;; Older setuptools is needed for use_2to3.
     (list python-cython python-setuptools))
    (propagated-inputs
     (list python-pysam python-rpy2))
    (home-page "https://github.com/jamescasbon/PyVCF")
    (synopsis "Variant Call Format parser for Python")
    (description "This package provides a @acronym{VCF,Variant Call Format}
parser for Python.")
    (license license:expat)))

(define-public nanosv
  (package
   (name "nanosv")
   (version "1.2.4")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "NanoSV" version))
            (sha256
             (base32
              "1wl2daj0bwrl8fx5xi8j8hfs3mp3vg3qycy66538n032v1qkc6xg"))))
   (build-system python-build-system)
   (inputs
    (list python-configparser python-pysam python-pyvcf))
   (home-page "https://github.com/mroosmalen/nanosv")
   (synopsis "Structural variation detection tool for Oxford Nanopore data")
   (description "NanoSV is a software package that can be used to identify
structural genomic variations in long-read sequencing data, such as data
produced by Oxford Nanopore Technologies’ MinION, GridION or PromethION
instruments, or Pacific Biosciences RSII or Sequel sequencers.")
   (license license:expat)))

(define-public python-strawc
  (package
    (name "python-strawc")
    (version "0.0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "strawC" version))
       (sha256
        (base32
         "1z1gy8n56lhriy6hdkh9r82ndikndipq2cy2wh8q185qig4rimr6"))))
    (build-system python-build-system)
    (inputs
     (list curl zlib))
    (propagated-inputs
     (list pybind11))
    (home-page "https://github.com/aidenlab/straw")
    (synopsis "Stream data from .hic files")
    (description "Straw is library which allows rapid streaming of contact
data from @file{.hic} files.  This package provides Python bindings.")
    (license license:expat)))

(define-public python-pybbi
  (package
    (name "python-pybbi")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pybbi" version))
       (sha256
        (base32
         "1hvy2f28i2b41l1pq15vciqbj538n0lichp8yr6413jmgg06xdsk"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #false ; tests require network access
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-cc
           (lambda _ (setenv "CC" "gcc")))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (copy-recursively "tests" "/tmp/tests")
               (with-directory-excursion "/tmp/tests"
                 (invoke "python" "-m" "pytest" "-v"))))))))
    (native-inputs
     (list pkg-config python-pkgconfig python-pytest))
    (inputs
     (list libpng openssl zlib))
    (propagated-inputs
     (list python-cython python-numpy python-pandas python-six))
    (home-page "https://github.com/nvictus/pybbi")
    (synopsis "Python bindings to UCSC Big Binary file library")
    (description
     "This package provides Python bindings to the UCSC Big
Binary (bigWig/bigBed) file library.  This provides read-level access to local
and remote bigWig and bigBed files but no write capabilitites.  The main
feature is fast retrieval of range queries into numpy arrays.")
    (license license:expat)))

(define-public python-dna-features-viewer
  (package
    (name "python-dna-features-viewer")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dna_features_viewer" version))
       (sha256
        (base32
         "0vci6kg2id6r6rh3cifq7ccnh7j0mb8iqg3hji6rva0ayrdqzafc"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ; there are none
    (propagated-inputs
     (list python-biopython python-matplotlib))
    (home-page
     "https://github.com/Edinburgh-Genome-Foundry/DnaFeaturesViewer")
    (synopsis "Plot features from DNA sequences")
    (description
     "DNA Features Viewer is a Python library to visualize DNA features,
e.g. from GenBank or Gff files, or Biopython SeqRecords.")
    (license license:expat)))

(define-public python-coolbox
  (package
    (name "python-coolbox")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "coolbox" version))
       (sha256
        (base32
         "0gqp76285w9klswr47y6kxbzwhv033b26jfa179kccfhiaq5p2xa"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ; there are none
    (inputs
     (list pybind11))
    (propagated-inputs
     (list python-cooler
           python-dna-features-viewer
           python-fire
           python-h5py
           python-intervaltree
           python-ipywidgets
           jupyter
           python-matplotlib
           python-nbformat
           python-numpy
           python-numpydoc
           python-pandas
           python-pybbi
           python-pytest
           python-scipy
           python-statsmodels
           python-strawc
           python-svgutils
           python-termcolor
           python-voila))
    (home-page "https://github.com/GangCaoLab/CoolBox")
    (synopsis "Genomic data visualization toolkit")
    (description
     "CoolBox is a toolkit for visual analysis of genomics data.  It aims to
be highly compatible with the Python ecosystem, easy to use and highly
customizable with a well-designed user interface.  It can be used in various
visualization situations, for example, to produce high-quality genome track
plots or fetch common used genomic data files with a Python script or command
line, interactively explore genomic data within Jupyter environment or web
browser.")
    (license license:gpl3+)))

(define-public python-pyspoa
  (package
    (name "python-pyspoa")
    (version "0.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nanoporetech/pyspoa")
             (commit (string-append "v" version))
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1lgf2shzhxkcsircd6vy46h27pjljd5q95fyz1cm3lkk702qbnzx"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-libspoa
           (lambda _
             (mkdir-p "src/build")
             (with-directory-excursion "src/build"
               (invoke "cmake"
                       "-Dspoa_optimize_for_portability=ON"
                       "-DCMAKE_BUILD_TYPE=Release"
                       "-DCMAKE_CXX_FLAGS=\"-I ../vendor/cereal/include/\" -fPIC"
                       "..")
               (invoke "make"))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "tests/test_pyspoa.py")))))))
    (propagated-inputs
     (list pybind11))
    (native-inputs
     `(("cmake" ,cmake-minimal)))
    (home-page "https://github.com/nanoporetech/pyspoa")
    (synopsis "Python bindings for the SIMD partial order alignment library")
    (description
     "This package provides Python bindings for spoa, a C++ implementation of
the @dfn{partial order alignment} (POA) algorithm (as described in
10.1093/bioinformatics/18.3.452) which is used to generate consensus
sequences")
    (license license:expat)))

(define-public python-bwapy
  (package
    (name "python-bwapy")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bwapy" version))
       (sha256
        (base32 "090qwx3vl729zn3a7sksbviyg04kc71gpbm3nd8dalqp673x1npw"))
       (modules '((guix build utils)))
       (snippet
        '(for-each delete-file (find-files "." "\\.o$")))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               (("wheel>=0.34") "wheel>=0.30"))))
         ;; TODO: it's possible that the import error points to a real
         ;; problem with the C sources.
         (delete 'sanity-check))))
    (propagated-inputs
     (list python-cffi python-setuptools python-wheel))
    (inputs
     (list zlib))
    (home-page "https://github.com/ACEnglish/bwapy")
    (synopsis "Python bindings to bwa alinger")
    (description "This package provides Python bindings to the bwa mem
aligner.")
    ;; These Python bindings are licensed under Mozilla Public License 2.0,
    ;; bwa itself is licenced under GNU General Public License v3.0.
    (license license:mpl2.0)))

(define-public scvelo
  (package
    (name "scvelo")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scvelo" version))
       (sha256
        (base32 "0h5ha1459ljs0qgpnlfsw592i8dxqn6p9bl08l1ikpwk36baxb7z"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Numba needs a writable dir to cache functions.
         (add-before 'check 'set-numba-cache-dir
           (lambda _
             (setenv "NUMBA_CACHE_DIR" "/tmp")))
         (replace 'check
           (lambda* (#:key outputs tests? #:allow-other-keys)
             (when tests?
               ;; The discovered test file names must match the names of the
               ;; compiled files, so we cannot run the tests from
               ;; /tmp/guix-build-*.
               (with-directory-excursion
                   (string-append (assoc-ref outputs "out")
                                  "/lib/python3.9/site-packages/scvelo/core/tests/")
                 (invoke "pytest" "-v"))))))))
    (propagated-inputs
     (list python-anndata
           python-hnswlib
           python-isort
           python-igraph
           python-loompy
           python-louvain
           python-matplotlib
           python-numba
           python-numpy
           python-pandas
           python-scanpy
           python-scikit-learn
           python-scipy
           python-umap-learn
           pybind11))
    (native-inputs
     (list python-black
           python-flake8
           python-hypothesis
           python-pre-commit
           python-pytest
           python-setuptools-scm
           python-wheel))
    (home-page "https://scvelo.org")
    (synopsis "RNA velocity generalized through dynamical modeling")
    (description "ScVelo is a scalable toolkit for RNA velocity analysis in
single cells.  RNA velocity enables the recovery of directed dynamic
information by leveraging splicing kinetics. scVelo generalizes the concept of
RNA velocity by relaxing previously made assumptions with a stochastic and a
dynamical model that solves the full transcriptional dynamics.  It thereby
adapts RNA velocity to widely varying specifications such as non-stationary
populations.")
    (license license:bsd-3)))

(define-public scregseg
  (package
    (name "scregseg")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/BIMSBbioinfo/scregseg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k8hllr5if6k2mm2zj391fv40sfc008cjm04l9vgfsdppb80i112"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #false                  ; tests require network access
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-fail-to-find-sklearn
           (lambda _
             ;; XXX: I have no idea why it cannot seem to find sklearn.
             (substitute* "setup.py"
               (("'sklearn',") "")))))))
    (native-inputs
     (list python-cython))
    (propagated-inputs
     (list python-scikit-learn
           python-scipy
           python-numpy
           python-hmmlearn
           python-pandas
           python-numba
           python-anndata
           python-scanpy
           python-pybedtools
           python-pysam
           python-matplotlib
           python-seaborn
           python-coolbox))
    (home-page "https://github.com/BIMSBbioinfo/scregseg")
    (synopsis "Single-cell regulatory landscape segmentation")
    (description "Scregseg (Single-Cell REGulatory landscape SEGmentation) is a
tool that facilitates the analysis of single cell ATAC-seq data by an
HMM-based segmentation algorithm.  Scregseg uses an HMM with
Dirichlet-Multinomial emission probabilities to segment the genome either
according to distinct relative cross-cell accessibility profiles or (after
collapsing the single-cell tracks to pseudo-bulk tracks) to capture distinct
cross-cluster accessibility profiles.")
    (license license:gpl3+)))

(define-public megadepth
  (package
    (name "megadepth")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ChristopherWilks/megadepth")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hj69d2dgmk2zwgazik7xzc04fxxlk93p888kpgc52fmhd95qph7"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #false ; some tests seem to require connection to
                       ; www.ebi.ac.uk; this may be caused by htslib.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-CMakeLists.txt
           (lambda _
             (rename-file "CMakeLists.txt.ci" "CMakeLists.txt")
             (substitute* "CMakeLists.txt"
               (("`cat ../VERSION`") ,version)
               (("target_link_libraries\\(megadepth_static") "#")
               (("target_link_libraries\\(megadepth_statlib") "#")
               (("add_executable\\(megadepth_static") "#")
               (("add_executable\\(megadepth_statlib") "#"))

             (substitute* "tests/test.sh"
               ;; Disable remote test
               (("./megadepth http://stingray.cs.jhu.edu/data/temp/test.bam") "#")
               ;; Prior to installation the binary's name differs from what
               ;; the test script assumes.
               (("./megadepth") "../build/megadepth_dynamic"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "../source"
                 (invoke "bash" "tests/test.sh" "use-local-test-data")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (copy-file "megadepth_dynamic"
                          (string-append bin "/megadepth"))))))))
    (native-inputs
     (list diffutils perl grep))
    (inputs
     (list curl htslib libdeflate libbigwig zlib))
    (home-page "https://github.com/ChristopherWilks/megadepth")
    (synopsis "BigWig and BAM/CRAM related utilities")
    (description "Megadepth is an efficient tool for extracting coverage
related information from RNA and DNA-seq BAM and BigWig files.  It supports
reading whole-genome coverage from BAM files and writing either indexed TSV or
BigWig files, as well as efficient region coverage summary over intervals from
both types of files.")
    (license license:expat)))

(define-public r-ascat
  (package
   (name "r-ascat")
   (version "2.5.2")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/Crick-CancerGenomics/ascat.git")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0cc0y3as6cb64iwnx0pgbajiig7m4z723mns9d5i4j09ccid3ccm"))))
   (build-system r-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
       (add-after 'unpack 'move-to-ascat-dir
         (lambda _
           (chdir "ASCAT"))))))
   (propagated-inputs
    (list r-rcolorbrewer))
   (home-page "https://github.com/VanLoo-lab/ascat/")
   (synopsis "Allele-Specific Copy Number Analysis of Tumors in R")
   (description "This package provides the @acronym{ASCAT,Allele-Specific Copy
Number Analysis of Tumors} R package that can be used to infer tumour purity,
ploidy and allele-specific copy number profiles.")
   (license license:gpl3)))

(define-public r-battenberg
  (package
   (name "r-battenberg")
   (version "2.2.9")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/Wedge-lab/battenberg.git")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0nmcq4c7y5g8h8lxsq9vadz9bj4qgqn118alip520ny6czaxki4h"))))
   (build-system r-build-system)
   (propagated-inputs
    (list r-devtools
          r-readr
          r-doparallel
          r-ggplot2
          r-rcolorbrewer
          r-gridextra
          r-gtools
          r-ascat))
   (home-page "https://github.com/Wedge-lab/battenberg")
   (synopsis "Subclonal copy number estimation in R")
   (description "This package contains the Battenberg R package for subclonal
copy number estimation, as described by
@url{doi:10.1016/j.cell.2012.04.023,Nik-Zainal et al.}")
   (license license:gpl3)))

(define-public r-catch
  (let ((commit "196ddd5a51b1a5f5daa01de53fdaad9b7505e084")
        (revision "1"))
    (package
      (name "r-catch")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/zhanyinx/CaTCH")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "11c7f1fc8f57wnwk1hrgr5y814m80zj8gkz5021vxyxy2v02cqgd"))))
      (build-system r-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "CaTCH"))))))
      (home-page "https://github.com/zhanyinx/CaTCH_R")
      (synopsis "Call a hierarchy of domains based on Hi-C data")
      (description "This package allows building the hierarchy of domains
starting from Hi-C data.  Each hierarchical level is identified by a minimum
value of physical insulation between neighboring domains.")
      (license license:gpl2+))))

(define-public r-spectre
  (let ((commit "f6648ab3eb9499300d86502b5d60ec370ae9b61a")
        (revision "1"))
    (package
      (name "r-spectre")
      (version (git-version "0.5.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ImmuneDynamics/Spectre")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0g38grrhbqqa4bmcilvdyawbkcnax6k4vffx2giywp18mbirmj0x"))))
      (properties `((upstream-name . "Spectre")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-biobase
             r-biocmanager
             r-caret
             r-class
             r-colorramps
             r-data-table
             r-devtools
             r-dplyr
             r-exactextractr
             r-factoextra
             r-flowcore
             r-flowsom
             r-flowviz
             r-fnn
             r-ggplot2
             r-ggpointdensity
             r-ggpubr
             r-ggraph
             r-ggthemes
             r-gridextra
             r-gridextra
             r-gtools
             r-hdf5array
             r-irlba
             r-pheatmap
             r-plyr
             r-qs
             r-raster
             r-rcolorbrewer
             r-rgeos
             r-rhdf5
             r-rstudioapi
             r-rsvd
             r-rtsne
             r-s2
             r-scales
             r-sf
             r-sp
             r-stars
             r-stringr
             r-tidygraph
             r-tidyr
             r-tidyr
             r-tiff
             r-umap))
      (home-page "https://github.com/ImmuneDynamics/Spectre")
      (synopsis "High-dimensional cytometry and imaging analysis")
      (description
       "This package provides a computational toolkit in R for the
integration, exploration, and analysis of high-dimensional single-cell
cytometry and imaging data.")
      (license license:expat))))

(define-public r-cytonorm
  (let ((commit "e4b9d343ee65db3c422800f1db3e77c25abde987")
        (revision "1"))
    (package
      (name "r-cytonorm")
      (version (git-version "0.0.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/saeyslab/CytoNorm")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0h2rdy15i4zymd4dv60n5w0frbsdbmzpv99dgm0l2dn041qv7fah"))))
      (properties `((upstream-name . "CytoNorm")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-cytoml
             r-dplyr
             r-emdist
             r-flowcore
             r-flowsom
             r-flowworkspace
             r-ggplot2
             r-gridextra
             r-pheatmap
             r-stringr))
      (home-page "https://github.com/saeyslab/CytoNorm")
      (synopsis "Normalize cytometry data measured across multiple batches")
      (description
       "This package can be used to normalize cytometry samples when a control
sample is taken along in each of the batches.  This is done by first
identifying multiple clusters/cell types, learning the batch effects from the
control samples and applying quantile normalization on all markers of
interest.")
      (license license:gpl2+))))

(define-public ccwl
  (package
    (name "ccwl")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://ccwl.systemreboot.net/releases/ccwl-"
                           version ".tar.lz"))
       (sha256
        (base32
         "1ar8rfz3zrksgygrv67zv77y8gfvvz54zcs546jn6j28y20basla"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0") ; to prevent guild warnings
       #:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%gnu-build-system-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (effective-version (target-guile-effective-version)))
               (wrap-program (string-append out "/bin/ccwl")
                 `("GUILE_LOAD_PATH" prefix
                   (,(string-append out "/share/guile/site/" effective-version)
                    ,(getenv "GUILE_LOAD_PATH")))
                 `("GUILE_LOAD_COMPILED_PATH" prefix
                   (,(string-append out "/lib/guile/" effective-version "/site-ccache")
                    ,(getenv "GUILE_LOAD_COMPILED_PATH"))))))))))
    (inputs
     `(("bash" ,bash-minimal)
       ("guile" ,guile-3.0)
       ("guile-libyaml" ,guile-libyaml)))
    (native-inputs
     (list pkg-config
           lzip
           ;; To build documentation
           cwltool
           graphviz
           skribilo))
    (home-page "https://ccwl.systemreboot.net")
    (synopsis "Concise common workflow language")
    (description "The @acronym{ccwl, Concise Common Workflow Language} is a
concise syntax to express CWL workflows.  ccwl is a compiler to generate CWL
workflows from concise descriptions in ccwl.  It is implemented as an
@acronym{EDSL, Embedded Domain Specific Language} in the Scheme programming
language.")
    (license license:gpl3+)))
