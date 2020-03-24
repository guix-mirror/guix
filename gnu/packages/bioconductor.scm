;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (gnu packages bioconductor)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1))


;;; Annotations

(define-public r-reactome-db
  (package
    (name "r-reactome-db")
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "reactome.db" version 'annotation))
       (sha256
        (base32
         "05wc4fp0faq6h3kq5rwafnips043as31yq11mrjngfxvf5i10srg"))))
    (properties `((upstream-name . "reactome.db")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://bioconductor.org/packages/reactome.db/")
    (synopsis "Annotation maps for reactome")
    (description
     "This package provides a set of annotation maps for the REACTOME
database, assembled using data from REACTOME.")
    (license license:cc-by4.0)))

(define-public r-bsgenome-celegans-ucsc-ce6
  (package
    (name "r-bsgenome-celegans-ucsc-ce6")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Celegans.UCSC.ce6"
                                     version 'annotation))
              (sha256
               (base32
                "0mqzb353xv2c3m3vkb315dkmnxkgczp7ndnknyhpgjlybyf715v9"))))
    (properties
     `((upstream-name . "BSgenome.Celegans.UCSC.ce6")))
    (build-system r-build-system)
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
              (uri (bioconductor-uri "BSgenome.Celegans.UCSC.ce10"
                                     version 'annotation))
              (sha256
               (base32
                "1zaym97jk4npxk14ifvwz2rvhm4zx9xgs33r9vvx9rlynp0gydrk"))))
    (properties
     `((upstream-name . "BSgenome.Celegans.UCSC.ce10")))
    (build-system r-build-system)
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

(define-public r-bsgenome-dmelanogaster-ucsc-dm6
  (package
    (name "r-bsgenome-dmelanogaster-ucsc-dm6")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Dmelanogaster.UCSC.dm6"
                                     version 'annotation))
              (sha256
               (base32
                "1bhj0rdgf7lspw4xby9y9mf7v7jxxz8001bc8vw8kf04rjsx6060"))))
    (properties
     `((upstream-name . "BSgenome.Dmelanogaster.UCSC.dm6")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Dmelanogaster.UCSC.dm6/")
    (synopsis "Full genome sequences for Fly")
    (description
     "This package provides full genome sequences for Drosophila
melanogaster (Fly) as provided by UCSC (dm6) and stored in Biostrings
objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-dmelanogaster-ucsc-dm3
  (package
    (name "r-bsgenome-dmelanogaster-ucsc-dm3")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Dmelanogaster.UCSC.dm3"
                                     version 'annotation))
              (sha256
               (base32
                "19bm3lkhhkag3gnwp419211fh0cnr0x6fa0r1lr0ycwrikxdxsv8"))))
    (properties
     `((upstream-name . "BSgenome.Dmelanogaster.UCSC.dm3")))
    (build-system r-build-system)
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

(define-public r-bsgenome-dmelanogaster-ucsc-dm3-masked
  (package
    (name "r-bsgenome-dmelanogaster-ucsc-dm3-masked")
    (version "1.3.99")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Dmelanogaster.UCSC.dm3.masked"
                                     version 'annotation))
              (sha256
               (base32
                "1756csb09f1br9rj1l3f08qyh4hlymdbd0cfn8x3fq39dn45m5ap"))))
    (properties
     `((upstream-name . "BSgenome.Dmelanogaster.UCSC.dm3.masked")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-dmelanogaster-ucsc-dm3"
        ,r-bsgenome-dmelanogaster-ucsc-dm3)))
    (home-page "https://www.bioconductor.org/packages/BSgenome.Dmelanogaster.UCSC.dm3.masked/")
    (synopsis "Full masked genome sequences for Fly")
    (description
     "This package provides full masked genome sequences for Drosophila
melanogaster (Fly) as provided by UCSC (dm3, April 2006) and stored in
Biostrings objects.  The sequences are the same as in
BSgenome.Dmelanogaster.UCSC.dm3, except that each of them has the 4 following
masks on top: (1) the mask of assembly gaps (AGAPS mask), (2) the mask of
intra-contig ambiguities (AMB mask), (3) the mask of repeats from
RepeatMasker (RM mask), and (4) the mask of repeats from Tandem Repeats
Finder (TRF mask).  Only the AGAPS and AMB masks are \"active\" by default.")
    (license license:artistic2.0)))

(define-public r-bsgenome-hsapiens-1000genomes-hs37d5
  (package
    (name "r-bsgenome-hsapiens-1000genomes-hs37d5")
    (version "0.99.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Hsapiens.1000genomes.hs37d5"
                                     version 'annotation))
              (sha256
               (base32
                "1cg0g5fqmsvwyw2p9hp2yy4ilk21jkbbrnpgqvb5c36ihjwvc7sr"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.1000genomes.hs37d5")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Hsapiens.1000genomes.hs37d5/")
    (synopsis "Full genome sequences for Homo sapiens")
    (description
     "This package provides full genome sequences for Homo sapiens from
1000genomes phase2 reference genome sequence (hs37d5), based on NCBI GRCh37.")
    (license license:artistic2.0)))

(define-public r-bsgenome-hsapiens-ncbi-grch38
  (package
    (name "r-bsgenome-hsapiens-ncbi-grch38")
    (version "1.3.1000")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BSgenome.Hsapiens.NCBI.GRCh38"
                              version 'annotation))
       (sha256
        (base32
         "0y75qdq578fh6420vbvsbwmdw8jvr3g06qli2h3vj3pxmjykh9c1"))))
    (properties `((upstream-name . "BSgenome.Hsapiens.NCBI.GRCh38")))
    (build-system r-build-system)
    (propagated-inputs `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://bioconductor.org/packages/release/data/annotation/html/\
BSgenome.Hsapiens.NCBI.GRCh38.html")
    (synopsis "Full genome sequences for Homo sapiens (GRCh38)")
    (description
     "This package provides full genome sequences for Homo sapiens (Human) as
provided by NCBI (GRCh38, 2013-12-17) and stored in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-hsapiens-ucsc-hg19-masked
  (package
    (name "r-bsgenome-hsapiens-ucsc-hg19-masked")
    (version "1.3.99")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Hsapiens.UCSC.hg19.masked"
                                     version 'annotation))
              (sha256
               (base32
                "0452pyah0kv1vsrsjbrqw4k2rm8lc2vc771dzib45gnnfz86qxrr"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.UCSC.hg19.masked")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-hsapiens-ucsc-hg19"
        ,r-bsgenome-hsapiens-ucsc-hg19)))
    (home-page "https://bioconductor.org/packages/BSgenome.Hsapiens.UCSC.hg19.masked/")
    (synopsis "Full masked genome sequences for Homo sapiens")
    (description
     "This package provides full genome sequences for Homo sapiens (Human) as
provided by UCSC (hg19, Feb. 2009) and stored in Biostrings objects.  The
sequences are the same as in BSgenome.Hsapiens.UCSC.hg19, except that each of
them has the 4 following masks on top: (1) the mask of assembly gaps (AGAPS
mask), (2) the mask of intra-contig ambiguities (AMB mask), (3) the mask of
repeats from RepeatMasker (RM mask), and (4) the mask of repeats from Tandem
Repeats Finder (TRF mask).  Only the AGAPS and AMB masks are \"active\" by
default.")
    (license license:artistic2.0)))

(define-public r-bsgenome-mmusculus-ucsc-mm9
  (package
    (name "r-bsgenome-mmusculus-ucsc-mm9")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Mmusculus.UCSC.mm9"
                                     version 'annotation))
              (sha256
               (base32
                "1birqw30g2azimxpnjfzmkphan7x131yy8b9h85lfz5fjdg7841i"))))
    (properties
     `((upstream-name . "BSgenome.Mmusculus.UCSC.mm9")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Mmusculus.UCSC.mm9/")
    (synopsis "Full genome sequences for Mouse")
    (description
     "This package provides full genome sequences for Mus musculus (Mouse) as
provided by UCSC (mm9, July 2007) and stored in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-mmusculus-ucsc-mm9-masked
  (package
    (name "r-bsgenome-mmusculus-ucsc-mm9-masked")
    (version "1.3.99")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Mmusculus.UCSC.mm9.masked"
                                     version 'annotation))
              (sha256
               (base32
                "00bpbm3havqcxr4g63zhllsbpd9q6svgihks7qp7x73nm4gvq7fn"))))
    (properties
     `((upstream-name . "BSgenome.Mmusculus.UCSC.mm9.masked")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-mmusculus-ucsc-mm9"
        ,r-bsgenome-mmusculus-ucsc-mm9)))
    (home-page "https://bioconductor.org/packages/BSgenome.Mmusculus.UCSC.mm9.masked/")
    (synopsis "Full masked genome sequences for Mouse")
    (description
     "This package provides full genome sequences for Mus musculus (Mouse) as
provided by UCSC (mm9, Jul. 2007) and stored in Biostrings objects.  The
sequences are the same as in BSgenome.Mmusculus.UCSC.mm9, except that each of
them has the 4 following masks on top: (1) the mask of assembly gaps (AGAPS
mask), (2) the mask of intra-contig ambiguities (AMB mask), (3) the mask of
repeats from RepeatMasker (RM mask), and (4) the mask of repeats from Tandem
Repeats Finder (TRF mask).  Only the AGAPS and AMB masks are \"active\" by
default."  )
    (license license:artistic2.0)))

(define-public r-bsgenome-mmusculus-ucsc-mm10
  (package
    (name "r-bsgenome-mmusculus-ucsc-mm10")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Mmusculus.UCSC.mm10"
                                     version 'annotation))
              (sha256
               (base32
                "12s0nm2na9brjad4rn9l7d3db2aj8qa1xvz0y1k7gk08wayb6bkf"))))
    (properties
     `((upstream-name . "BSgenome.Mmusculus.UCSC.mm10")))
    (build-system r-build-system)
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

(define-public r-org-ce-eg-db
  (package
    (name "r-org-ce-eg-db")
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Ce.eg.db" version 'annotation))
              (sha256
               (base32
                "1w5br1ss4ha8wv4v2saj7cmbjc2jw0dyj2f2y269l078z31wcnaz"))))
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
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Dm.eg.db" version 'annotation))
              (sha256
               (base32
                "1pqjrzlyg72bjpy8zsxvaglc7jsv176bnyi87xdajmkvsgxpm7b3"))))
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

(define-public r-org-dr-eg-db
  (package
    (name "r-org-dr-eg-db")
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Dr.eg.db" version 'annotation))
              (sha256
               (base32
                "1xs5wsbcpy0iwbjyiv7fax57djqc529ai5fk1qfsdcvlja3cpglx"))))
    (properties
     `((upstream-name . "org.Dr.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://www.bioconductor.org/packages/org.Dr.eg.db/")
    (synopsis "Annotation for Zebrafish")
    (description
     "This package provides genome wide annotations for Zebrafish, primarily
based on mapping using Entrez Gene identifiers.")
    (license license:artistic2.0)))

(define-public r-org-hs-eg-db
  (package
    (name "r-org-hs-eg-db")
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Hs.eg.db" version 'annotation))
              (sha256
               (base32
                "1qxz9l80yg3qdqszs6dsscp7lrpfi1bgd0pxh9j7q34vprzwhdim"))))
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

(define-public r-org-mm-eg-db
  (package
    (name "r-org-mm-eg-db")
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Mm.eg.db" version 'annotation))
              (sha256
               (base32
                "1i3nvrd3wjigf1rmgxq1p5xxc3p8v02h5gwi62s30rkrsyjjfjxx"))))
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

(define-public r-bsgenome-hsapiens-ucsc-hg19
  (package
    (name "r-bsgenome-hsapiens-ucsc-hg19")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Hsapiens.UCSC.hg19"
                                     version 'annotation))
              (sha256
               (base32
                "1y0nqpk8cw5a34sd9hmin3z4v7iqm6hf6l22cl81vlbxqbjibxc8"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.UCSC.hg19")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Hsapiens.UCSC.hg19/")
    (synopsis "Full genome sequences for Homo sapiens")
    (description
     "This package provides full genome sequences for Homo sapiens as provided
by UCSC (hg19, February 2009) and stored in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-ensdb-hsapiens-v75
  (package
    (name "r-ensdb-hsapiens-v75")
    (version "2.99.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EnsDb.Hsapiens.v75" version 'annotation))
       (sha256
        (base32
         "0jx6rf6v0j8yr07q3c1h7s121901dc400nm6xaiv4i7kb5czjn9c"))))
    (properties
     `((upstream-name . "EnsDb.Hsapiens.v75")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ensembldb" ,r-ensembldb)))
    (home-page "https://bioconductor.org/packages/EnsDb.Hsapiens.v75")
    (synopsis "Ensembl based annotation package")
    (description
     "This package exposes an annotation database generated from Ensembl.")
    (license license:artistic2.0)))

(define-public r-genelendatabase
  (package
    (name "r-genelendatabase")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geneLenDataBase" version 'experiment))
       (sha256
        (base32
         "03gm4pvsfascx7kjg0jycpf4f572mja68wwmwigs390vbmawyb4a"))))
    (properties
     `((upstream-name . "geneLenDataBase")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rtracklayer" ,r-rtracklayer)
       ("r-genomicfeatures" ,r-genomicfeatures)))
    (home-page "https://bioconductor.org/packages/geneLenDataBase/")
    (synopsis "Lengths of mRNA transcripts for a number of genomes")
    (description
     "This package provides the lengths of mRNA transcripts for a number of
genomes and gene ID formats, largely based on the UCSC table browser.")
    (license license:lgpl2.0+)))

(define-public r-txdb-hsapiens-ucsc-hg19-knowngene
  (package
    (name "r-txdb-hsapiens-ucsc-hg19-knowngene")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TxDb.Hsapiens.UCSC.hg19.knownGene"
                                     version 'annotation))
              (sha256
               (base32
                "1sajhcqqwazgz2lqbik7rd935i7kpnh08zxbp2ra10j72yqy4g86"))))
    (properties
     `((upstream-name . "TxDb.Hsapiens.UCSC.hg19.knownGene")))
    (build-system r-build-system)
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

(define-public r-txdb-hsapiens-ucsc-hg38-knowngene
  (package
    (name "r-txdb-hsapiens-ucsc-hg38-knowngene")
    (version "3.4.6")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TxDb.Hsapiens.UCSC.hg38.knownGene"
                                     version 'annotation))
              (sha256
               (base32
                "12j7rri9r129v9w1yiqadg952dx462dh092sxif3r5kk8l7bxkn9"))))
    (properties
     `((upstream-name . "TxDb.Hsapiens.UCSC.hg38.knownGene")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomicfeatures" ,r-genomicfeatures)))
    (home-page
     "https://bioconductor.org/packages/TxDb.Hsapiens.UCSC.hg38.knownGene/")
    (synopsis "Annotation package for human genome in TxDb format")
    (description
     "This package provides an annotation database of Homo sapiens genome
data.  It is derived from the UCSC hg38 genome and based on the \"knownGene\"
track.  The database is exposed as a @code{TxDb} object.")
    (license license:artistic2.0)))

(define-public r-txdb-mmusculus-ucsc-mm9-knowngene
  (package
    (name "r-txdb-mmusculus-ucsc-mm9-knowngene")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TxDb.Mmusculus.UCSC.mm9.knownGene"
                                     version 'annotation))
              (sha256
               (base32
                "16bjxy00363hf91ik2mqlqls86i07gia72qh92xc3l1ncch61mx2"))))
    (properties
     `((upstream-name . "TxDb.Mmusculus.UCSC.mm9.knownGene")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomicfeatures" ,r-genomicfeatures)
       ("r-annotationdbi" ,r-annotationdbi)))
    (home-page
     "https://bioconductor.org/packages/TxDb.Mmusculus.UCSC.mm9.knownGene/")
    (synopsis "Annotation package for mouse genome in TxDb format")
    (description
     "This package provides an annotation database of Mouse genome data.  It
is derived from the UCSC mm9 genome and based on the \"knownGene\" track.  The
database is exposed as a @code{TxDb} object.")
    (license license:artistic2.0)))

(define-public r-txdb-mmusculus-ucsc-mm10-knowngene
  (package
    (name "r-txdb-mmusculus-ucsc-mm10-knowngene")
    (version "3.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TxDb.Mmusculus.UCSC.mm10.knownGene"
                                     version 'annotation))
              (sha256
               (base32
                "0xs9npnhbwll7p62hibs02y4ac23jchdcr25i6a7qwq1kms82qk9"))))
    (properties
     `((upstream-name . "TxDb.Mmusculus.UCSC.mm10.knownGene")))
    (build-system r-build-system)
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

(define-public r-txdb-celegans-ucsc-ce6-ensgene
  (package
    (name "r-txdb-celegans-ucsc-ce6-ensgene")
    (version "3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TxDb.Celegans.UCSC.ce6.ensGene"
                              version 'annotation))
       (sha256
        (base32
         "1sgppva33cdy4isj2is8mfalj5gmmkpbkq9w1d83a4agcq31mi90"))))
    (properties
     `((upstream-name . "TxDb.Celegans.UCSC.ce6.ensGene")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-genomicfeatures" ,r-genomicfeatures)))
    (home-page "https://bioconductor.org/packages/TxDb.Celegans.UCSC.ce6.ensGene/")
    (synopsis "Annotation package for C elegans TxDb objects")
    (description
     "This package exposes a C elegans annotation database generated from UCSC
by exposing these as TxDb objects.")
    (license license:artistic2.0)))

(define-public r-fdb-infiniummethylation-hg19
  (package
    (name "r-fdb-infiniummethylation-hg19")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FDb.InfiniumMethylation.hg19"
                                     version 'annotation))
              (sha256
               (base32
                "0gq90fvph6kgrpjb89nvzq6hl1k24swn19rgjh5g98l86mja6nk0"))))
    (properties
     `((upstream-name . "FDb.InfiniumMethylation.hg19")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-org-hs-eg-db" ,r-org-hs-eg-db)
       ("r-txdb-hsapiens-ucsc-hg19-knowngene" ,r-txdb-hsapiens-ucsc-hg19-knowngene)))
    (home-page "https://bioconductor.org/packages/FDb.InfiniumMethylation.hg19/")
    (synopsis "Compiled HumanMethylation27 and HumanMethylation450 annotations")
    (description
     "This is an annotation package for Illumina Infinium DNA methylation
probes.  It contains the compiled HumanMethylation27 and HumanMethylation450
annotations.")
    (license license:artistic2.0)))

(define-public r-illuminahumanmethylationepicmanifest
  (package
    (name "r-illuminahumanmethylationepicmanifest")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IlluminaHumanMethylationEPICmanifest"
                                     version 'annotation))
              (sha256
               (base32
                "0alhjda5g186z8b1nsmnpfswrlj7prdz8mkwx60wkkl6hkcnk6p3"))))
    (properties
     `((upstream-name . "IlluminaHumanMethylationEPICmanifest")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-minfi" ,r-minfi)))
    (home-page "https://bioconductor.org/packages/IlluminaHumanMethylationEPICmanifest/")
    (synopsis "Manifest for Illumina's EPIC methylation arrays")
    (description
     "This is a manifest package for Illumina's EPIC methylation arrays.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on r-bsgenome-hsapiens-ucsc-hg19
;; from Bioconductor.
(define-public r-deconstructsigs
  (package
    (name "r-deconstructsigs")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "deconstructSigs" version))
              (sha256
               (base32
                "014x0nb23jb98666kaav2phkvmkr38pi38jv0dqd4jv7zp0gdf1a"))))
    (properties
     `((upstream-name . "deconstructSigs")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-reshape2" ,r-reshape2)))
    (home-page "https://github.com/raerose01/deconstructSigs")
    (synopsis "Identifies signatures present in a tumor sample")
    (description "This package takes sample information in the form of the
fraction of mutations in each of 96 trinucleotide contexts and identifies
the weighted combination of published signatures that, when summed, most
closely reconstructs the mutational profile.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on Bioconductor packages.
(define-public r-nmf
  (package
    (name "r-nmf")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "NMF" version))
       (sha256
        (base32
         "0b2ls3x1nkrnam45hagpys624nzxj3v7kxnp0q216yakvx5h57cq"))))
    (properties `((upstream-name . "NMF")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cluster" ,r-cluster)
       ("r-biobase" ,r-biobase)
       ("r-biocmanager" ,r-biocmanager)
       ("r-bigmemory" ,r-bigmemory) ; suggested
       ("r-synchronicity" ,r-synchronicity) ; suggested
       ("r-colorspace" ,r-colorspace)
       ("r-digest" ,r-digest)
       ("r-doparallel" ,r-doparallel)
       ("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridbase" ,r-gridbase)
       ("r-pkgmaker" ,r-pkgmaker)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-registry" ,r-registry)
       ("r-reshape2" ,r-reshape2)
       ("r-rngtools" ,r-rngtools)
       ("r-stringr" ,r-stringr)))
    (home-page "http://renozao.github.io/NMF")
    (synopsis "Algorithms and framework for nonnegative matrix factorization")
    (description
     "This package provides a framework to perform Non-negative Matrix
Factorization (NMF).  The package implements a set of already published
algorithms and seeding methods, and provides a framework to test, develop and
plug new or custom algorithms.  Most of the built-in algorithms have been
optimized in C++, and the main interface function provides an easy way of
performing parallel computations on multicore machines.")
    (license license:gpl2+)))

(define-public r-do-db
  (package
    (name "r-do-db")
    (version "2.9")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DO.db" version 'annotation))
              (sha256
               (base32
                "10bqqa124l61ivzy4mdd3z3ar9a6537qbxw23pc4y9w8a6dwnavn"))))
    (properties
     `((upstream-name . "DO.db")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://www.bioconductor.org/packages/DO.db/")
    (synopsis "Annotation maps describing the entire Disease Ontology")
    (description
     "This package provides a set of annotation maps describing the entire
Disease Ontology.")
    (license license:artistic2.0)))

(define-public r-pfam-db
  (package
    (name "r-pfam-db")
    (version "3.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "PFAM.db" version 'annotation))
       (sha256
        (base32
         "0rn1arzzcniy3yyc4yc44vn40g0cqss37dhwnvsgxpfayqq1k59s"))))
    (properties `((upstream-name . "PFAM.db")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://bioconductor.org/packages/PFAM.db")
    (synopsis "Set of protein ID mappings for PFAM")
    (description
     "This package provides a set of protein ID mappings for PFAM, assembled
using data from public repositories.")
    (license license:artistic2.0)))

(define-public r-phastcons100way-ucsc-hg19
  (package
    (name "r-phastcons100way-ucsc-hg19")
    (version "3.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "phastCons100way.UCSC.hg19"
                              version 'annotation))
       (sha256
        (base32
         "1jmc4k4zgkx5vr2plnidnd9bidlwlb0kr7mjg60cqjw7dq7jl1fa"))))
    (properties
     `((upstream-name . "phastCons100way.UCSC.hg19")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-genomicscores" ,r-genomicscores)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/phastCons100way.UCSC.hg19")
    (synopsis "UCSC phastCons conservation scores for hg19")
    (description
     "This package provides UCSC phastCons conservation scores for the human
genome (hg19) calculated from multiple alignments with other 99 vertebrate
species.")
    (license license:artistic2.0)))


;;; Experiment data

(define-public r-abadata
  (package
    (name "r-abadata")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ABAData" version 'experiment))
              (sha256
               (base32
                "1bmj341xcymlrk02gss5vvrqc4ddas0rdw39lnpsj98hq6n11p5z"))))
    (properties
     `((upstream-name . "ABAData")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://www.bioconductor.org/packages/ABAData/")
    (synopsis "Gene expression in human brain regions from Allen Brain Atlas")
    (description
     "This package provides the data for the gene expression enrichment
analysis conducted in the package ABAEnrichment.  The package includes three
datasets which are derived from the Allen Brain Atlas:

@enumerate
@item Gene expression data from Human Brain (adults) averaged across donors,
@item Gene expression data from the Developing Human Brain pooled into five
  age categories and averaged across donors, and
@item a developmental effect score based on the Developing Human Brain
  expression data.
@end enumerate

All datasets are restricted to protein coding genes.")
    (license license:gpl2+)))

(define-public r-arrmdata
  (package
    (name "r-arrmdata")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ARRmData" version 'experiment))
              (sha256
               (base32
                "0r1y3zn7ly4k3ngx55vfavn9s6aidbddlv2fbmj7hj3hvpslmyly"))))
    (properties
     `((upstream-name . "ARRmData")))
    (build-system r-build-system)
    (home-page "https://www.bioconductor.org/packages/ARRmData/")
    (synopsis "Example dataset for normalization of Illumina 450k methylation data")
    (description
     "This package provides raw beta values from 36 samples across 3 groups
from Illumina 450k methylation arrays.")
    (license license:artistic2.0)))

(define-public r-hsmmsinglecell
  (package
    (name "r-hsmmsinglecell")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HSMMSingleCell" version 'experiment))
              (sha256
               (base32
                "1vxnr8gr6md85g39csy7g2sqqajiqgyvznys2qa9yixd2b01yph9"))))
    (properties
     `((upstream-name . "HSMMSingleCell")))
    (build-system r-build-system)
    (home-page "https://www.bioconductor.org/packages/HSMMSingleCell/")
    (synopsis "Single-cell RNA-Seq for differentiating human skeletal muscle myoblasts (HSMM)")
    (description
     "Skeletal myoblasts undergo a well-characterized sequence of
morphological and transcriptional changes during differentiation.  In this
experiment, primary @dfn{human skeletal muscle myoblasts} (HSMM) were expanded
under high mitogen conditions (GM) and then differentiated by switching to
low-mitogen media (DM).  RNA-Seq libraries were sequenced from each of several
hundred cells taken over a time-course of serum-induced differentiation.
Between 49 and 77 cells were captured at each of four time points (0, 24, 48,
72 hours) following serum switch using the Fluidigm C1 microfluidic system.
RNA from each cell was isolated and used to construct mRNA-Seq libraries,
which were then sequenced to a depth of ~4 million reads per library,
resulting in a complete gene expression profile for each cell.")
    (license license:artistic2.0)))

(define-public r-all
  (package
    (name "r-all")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ALL" version 'experiment))
              (sha256
               (base32
                "1z7kpjw4ndj6fkxwvhqf3gawhrn26ksrlns7j2c78qzxqmjndik9"))))
    (properties `((upstream-name . "ALL")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)))
    (home-page "https://bioconductor.org/packages/ALL")
    (synopsis "Acute Lymphoblastic Leukemia data from the Ritz laboratory")
    (description
     "The data consist of microarrays from 128 different individuals with
@dfn{acute lymphoblastic leukemia} (ALL).  A number of additional covariates
are available.  The data have been normalized (using rma) and it is the
jointly normalized data that are available here.  The data are presented in
the form of an @code{exprSet} object.")
    (license license:artistic2.0)))

(define-public r-affydata
  (package
    (name "r-affydata")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affydata" version 'experiment))
       (sha256
        (base32
         "1l9qhmjqgbrdl9cmd74rlnvmvr6mslbmckb83n0211whp2i0b7h5"))))
    (properties `((upstream-name . "affydata")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)))
    (home-page "https://bioconductor.org/packages/affydata/")
    (synopsis "Affymetrix data for demonstration purposes")
    (description
     "This package provides example datasets that represent 'real world
examples' of Affymetrix data, unlike the artificial examples included in the
package @code{affy}.")
    (license license:gpl2+)))

(define-public r-curatedtcgadata
  (package
    (name "r-curatedtcgadata")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "curatedTCGAData" version 'experiment))
       (sha256
        (base32
         "02y6cgihmsl9b4a9mmcdjjgjp06lpz04biyvxd3n5lk5gnqd9r3y"))))
    (properties
     `((upstream-name . "curatedTCGAData")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationhub" ,r-annotationhub)
       ("r-experimenthub" ,r-experimenthub)
       ("r-hdf5array" ,r-hdf5array)
       ("r-multiassayexperiment" ,r-multiassayexperiment)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/curatedTCGAData/")
    (synopsis "Curated data from The Cancer Genome Atlas")
    (description
     "This package provides publicly available data from The Cancer Genome
Atlas (TCGA) as @code{MultiAssayExperiment} objects.
@code{MultiAssayExperiment} integrates multiple assays (e.g., RNA-seq, copy
number, mutation, microRNA, protein, and others) with clinical / pathological
data.  It also links assay barcodes with patient identifiers, enabling
harmonized subsetting of rows (features) and columns (patients / samples)
across the entire multi-'omics experiment.")
    (license license:artistic2.0)))


;;; Packages

(define-public r-biocversion
  (package
    (name "r-biocversion")
    (version "3.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocVersion" version))
       (sha256
        (base32
         "0mfqjqfvrwwglldq3g7nbic5hf3nwzv02nbfxnl2cfvf9gznlh5f"))))
    (properties `((upstream-name . "BiocVersion")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BiocVersion/")
    (synopsis "Set the appropriate version of Bioconductor packages")
    (description
     "This package provides repository information for the appropriate version
of Bioconductor.")
    (license license:artistic2.0)))

(define-public r-biocgenerics
  (package
    (name "r-biocgenerics")
    (version "0.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocGenerics" version))
              (sha256
               (base32
                "1np8y442zyakm4axpinbw1qsgc6wd3zjsnirbhc8lcii4ky9j0rn"))))
    (properties
     `((upstream-name . "BiocGenerics")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BiocGenerics")
    (synopsis "S4 generic functions for Bioconductor")
    (description
     "This package provides S4 generic functions needed by many Bioconductor
packages.")
    (license license:artistic2.0)))

(define-public r-affycomp
  (package
    (name "r-affycomp")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycomp" version))
       (sha256
        (base32
         "0cl7c3m2lz2w8g2k7z7wjd0dyj0dkssvms99qpg8a1v3hx1xs6js"))))
    (properties `((upstream-name . "affycomp")))
    (build-system r-build-system)
    (propagated-inputs `(("r-biobase" ,r-biobase)))
    (home-page "https://bioconductor.org/packages/affycomp/")
    (synopsis "Graphics toolbox for assessment of Affymetrix expression measures")
    (description
     "The package contains functions that can be used to compare expression
measures for Affymetrix Oligonucleotide Arrays.")
    (license license:gpl2+)))

(define-public r-affycompatible
  (package
    (name "r-affycompatible")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AffyCompatible" version))
       (sha256
        (base32
         "10ahrdlifp1i7rd58zb10w75y5bnigs7xp5gv4fhb5y7p7dvb0ks"))))
    (properties
     `((upstream-name . "AffyCompatible")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-rcurl" ,r-rcurl)
       ("r-xml" ,r-xml)))
    (home-page "https://bioconductor.org/packages/AffyCompatible/")
    (synopsis "Work with Affymetrix GeneChip files")
    (description
     "This package provides an interface to Affymetrix chip annotation and
sample attribute files.  The package allows an easy way for users to download
and manage local data bases of Affynmetrix NetAffx annotation files.  It also
provides access to @dfn{GeneChip Operating System} (GCOS) and @dfn{GeneChip
Command Console} (AGCC)-compatible sample annotation files.")
    (license license:artistic2.0)))

(define-public r-affycontam
  (package
    (name "r-affycontam")
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyContam" version))
       (sha256
        (base32
         "0yd1prgv5zfkg22ski73mvg96qknwz8v6ji6s4qy8p4wrqyj7b7l"))))
    (properties `((upstream-name . "affyContam")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-affydata" ,r-affydata)
       ("r-biobase" ,r-biobase)))
    (home-page "https://bioconductor.org/packages/affyContam/")
    (synopsis "Structured corruption of Affymetrix CEL file data")
    (description
     "Microarray quality assessment is a major concern of microarray analysts.
This package provides some simple approaches to in silico creation of quality
problems in CEL-level data to help evaluate performance of quality metrics.")
    (license license:artistic2.0)))

(define-public r-affycoretools
  (package
    (name "r-affycoretools")
    (version "1.58.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycoretools" version))
       (sha256
        (base32
         "1p283ysib04qzaayxmrpsmk5bq0jdq2rlky180jrlskpyg6risfw"))))
    (properties `((upstream-name . "affycoretools")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-dbi" ,r-dbi)
       ("r-edger" ,r-edger)
       ("r-gcrma" ,r-gcrma)
       ("r-glimma" ,r-glimma)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gostats" ,r-gostats)
       ("r-gplots" ,r-gplots)
       ("r-hwriter" ,r-hwriter)
       ("r-lattice" ,r-lattice)
       ("r-limma" ,r-limma)
       ("r-oligoclasses" ,r-oligoclasses)
       ("r-reportingtools" ,r-reportingtools)
       ("r-rsqlite" ,r-rsqlite)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xtable" ,r-xtable)))
    (home-page "https://bioconductor.org/packages/affycoretools/")
    (synopsis "Functions for analyses with Affymetrix GeneChips")
    (description
     "This package provides various wrapper functions that have been written
to streamline the more common analyses that a Biostatistician might see.")
    (license license:artistic2.0)))

(define-public r-affxparser
  (package
    (name "r-affxparser")
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affxparser" version))
       (sha256
        (base32
         "03h4lxr48p84f6i7zb2rm10ma3k4d1nmvdw5yhxcmzqbmd12lk40"))))
    (properties `((upstream-name . "affxparser")))
    (build-system r-build-system)
    (home-page "https://github.com/HenrikBengtsson/affxparser")
    (synopsis "Affymetrix File Parsing SDK")
    (description
     "This is a package for parsing Affymetrix files (CDF, CEL, CHP, BPMAP,
BAR).  It provides methods for fast and memory efficient parsing of Affymetrix
files using the Affymetrix' Fusion SDK.  Both ASCII- and binary-based files
are supported.  Currently, there are methods for reading @dfn{chip definition
file} (CDF) and a @dfn{cell intensity file} (CEL).  These files can be read
either in full or in part.  For example, probe signals from a few probesets
can be extracted very quickly from a set of CEL files into a convenient list
structure.")
    ;; The Fusion SDK contains files under GPLv2 and LGPLv2.1.  The R code is
    ;; under LGPLv2+.
    (license (list license:lgpl2.0+ license:lgpl2.1 license:gpl2))))

(define-public r-annotate
  (package
    (name "r-annotate")
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotate" version))
       (sha256
        (base32
         "0rcmdy6hs6m4d6wxgi52c0bhdsbf2sm9f155qbcb05sn0nh8pxwy"))))
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

(define-public r-hpar
  (package
    (name "r-hpar")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hpar" version))
       (sha256
        (base32
         "1yhay1ryrgj9cqa1x136cw40ca93afyvg0sarm30jsbj8nc1rm5m"))))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/hpar/")
    (synopsis "Human Protein Atlas in R")
    (description "This package provides a simple interface to and data from
the Human Protein Atlas project.")
    (license license:artistic2.0)))

(define-public r-regioner
  (package
    (name "r-regioner")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "regioneR" version))
       (sha256
        (base32
         "0if7r6njz3ahm545383z5mzmzw8fdvw80a9lfz160j5pcgpx2dq9"))))
    (properties `((upstream-name . "regioneR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-memoise" ,r-memoise)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/regioneR/")
    (synopsis "Association analysis of genomic regions")
    (description "This package offers a statistical framework based on
customizable permutation tests to assess the association between genomic
region sets and other genomic features.")
    (license license:artistic2.0)))

(define-public r-reportingtools
  (package
    (name "r-reportingtools")
    (version "2.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReportingTools" version))
       (sha256
        (base32
         "0wmi2219wydyzc07rz3azsrksa7wiacfh9pr5x2fsmj9f0w3n15w"))))
    (properties
     `((upstream-name . "ReportingTools")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-category" ,r-category)
       ("r-deseq2" ,r-deseq2)
       ("r-edger" ,r-edger)
       ("r-ggbio" ,r-ggbio)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gostats" ,r-gostats)
       ("r-gseabase" ,r-gseabase)
       ("r-hwriter" ,r-hwriter)
       ("r-iranges" ,r-iranges)
       ("r-knitr" ,r-knitr)
       ("r-lattice" ,r-lattice)
       ("r-limma" ,r-limma)
       ("r-pfam-db" ,r-pfam-db)
       ("r-r-utils" ,r-r-utils)
       ("r-xml" ,r-xml)))
    (home-page "https://bioconductor.org/packages/ReportingTools/")
    (synopsis "Tools for making reports in various formats")
    (description
     "The ReportingTools package enables users to easily display reports of
analysis results generated from sources such as microarray and sequencing
data.  The package allows users to create HTML pages that may be viewed on a
web browser, or in other formats.  Users can generate tables with sortable and
filterable columns, make and display plots, and link table entries to other
data sources such as NCBI or larger plots within the HTML page.  Using the
package, users can also produce a table of contents page to link various
reports together for a particular project that can be viewed in a web
browser.")
    (license license:artistic2.0)))

(define-public r-geneplotter
  (package
    (name "r-geneplotter")
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geneplotter" version))
       (sha256
        (base32
         "1k6780fn1kkghpm1prhhsyw621441a3bmnqfl9ns0zbc1zdq39nx"))))
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

(define-public r-oligoclasses
  (package
    (name "r-oligoclasses")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligoClasses" version))
       (sha256
        (base32
         "02m1m3dkiyywalphw3i5n6y3bs8zp24xh59v9cz6jgjpah811skf"))))
    (properties `((upstream-name . "oligoClasses")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affyio" ,r-affyio)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocmanager" ,r-biocmanager)
       ("r-biostrings" ,r-biostrings)
       ("r-dbi" ,r-dbi)
       ("r-ff" ,r-ff)
       ("r-foreach" ,r-foreach)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rsqlite" ,r-rsqlite)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/oligoClasses/")
    (synopsis "Classes for high-throughput arrays")
    (description
     "This package contains class definitions, validity checks, and
initialization methods for classes used by the @code{oligo} and @code{crlmm}
packages.")
    (license license:gpl2+)))

(define-public r-oligo
  (package
    (name "r-oligo")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligo" version))
       (sha256
        (base32
         "01icfyy82f9k0m7ngrppz1ckq3wpq7zp6kgf8ppc55j6582c5jh3"))))
    (properties `((upstream-name . "oligo")))
    (build-system r-build-system)
    (inputs `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-affxparser" ,r-affxparser)
       ("r-affyio" ,r-affyio)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-dbi" ,r-dbi)
       ("r-ff" ,r-ff)
       ("r-oligoclasses" ,r-oligoclasses)
       ("r-preprocesscore" ,r-preprocesscore)
       ("r-rsqlite" ,r-rsqlite)
       ("r-zlibbioc" ,r-zlibbioc)))
    (home-page "https://bioconductor.org/packages/oligo/")
    (synopsis "Preprocessing tools for oligonucleotide arrays")
    (description
     "This package provides a package to analyze oligonucleotide
arrays (expression/SNP/tiling/exon) at probe-level.  It currently supports
Affymetrix (CEL files) and NimbleGen arrays (XYS files).")
    (license license:lgpl2.0+)))

(define-public r-qvalue
  (package
    (name "r-qvalue")
    (version "2.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "qvalue" version))
       (sha256
        (base32
         "0njnidyncm3g3712mnp77cs4kghn596ss1pz6fhp1cr0wxcayp6j"))))
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

(define-public r-diffbind
  (package
    (name "r-diffbind")
    (version "2.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DiffBind" version))
       (sha256
        (base32
         "1729wyi2l4480yrkp5wg3ryirrmlk0j3njqs1qyckq3c8bjk12h2"))))
    (properties `((upstream-name . "DiffBind")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-amap" ,r-amap)
       ("r-biocparallel" ,r-biocparallel)
       ("r-deseq2" ,r-deseq2)
       ("r-dplyr" ,r-dplyr)
       ("r-edger" ,r-edger)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-gplots" ,r-gplots)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-limma" ,r-limma)
       ("r-locfit" ,r-locfit)
       ("r-rcolorbrewer" , r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ("r-rhtslib" ,r-rhtslib)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-systempiper" ,r-systempiper)))
    (home-page "https://bioconductor.org/packages/DiffBind")
    (synopsis "Differential binding analysis of ChIP-Seq peak data")
    (description
     "This package computes differentially bound sites from multiple
ChIP-seq experiments using affinity (quantitative) data.  Also enables
occupancy (overlap) analysis and plotting functions.")
    (license license:artistic2.0)))

(define-public r-ripseeker
  (package
    (name "r-ripseeker")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RIPSeeker" version))
       (sha256
        (base32
         "1wyv9mfrbxzklysfjcnwb8yils71janyyxa982jn0zxx4p9cl3vs"))))
    (properties `((upstream-name . "RIPSeeker")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-s4vectors" ,r-s4vectors)
       ("r-iranges" ,r-iranges)
       ("r-genomicranges" ,r-genomicranges)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-rsamtools" ,r-rsamtools)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-rtracklayer" ,r-rtracklayer)))
    (home-page "https://bioconductor.org/packages/RIPSeeker")
    (synopsis
     "Identifying protein-associated transcripts from RIP-seq experiments")
    (description
     "This package infers and discriminates RIP peaks from RIP-seq alignments
using two-state HMM with negative binomial emission probability.  While
RIPSeeker is specifically tailored for RIP-seq data analysis, it also provides
a suite of bioinformatics tools integrated within this self-contained software
package comprehensively addressing issues ranging from post-alignments
processing to visualization and annotation.")
    (license license:gpl2)))

(define-public r-multtest
  (package
    (name "r-multtest")
    (version "2.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "multtest" version))
       (sha256
        (base32
         "0qna9lx76ldsfy8qf5xmhl4ymqfkj29m1gdqhph06s470c8mwari"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-survival" ,r-survival)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biobase" ,r-biobase)
       ("r-mass" ,r-mass)))
    (home-page "https://bioconductor.org/packages/multtest")
    (synopsis "Resampling-based multiple hypothesis testing")
    (description
     "This package can do non-parametric bootstrap and permutation
resampling-based multiple testing procedures (including empirical Bayes
methods) for controlling the family-wise error rate (FWER), generalized
family-wise error rate (gFWER), tail probability of the proportion of
false positives (TPPFP), and false discovery rate (FDR).  Several choices
of bootstrap-based null distribution are implemented (centered, centered
and scaled, quantile-transformed).  Single-step and step-wise methods are
available.  Tests based on a variety of T- and F-statistics (including
T-statistics based on regression parameters from linear and survival models
as well as those based on correlation parameters) are included.  When probing
hypotheses with T-statistics, users may also select a potentially faster null
distribution which is multivariate normal with mean zero and variance
covariance matrix derived from the vector influence function.  Results are
reported in terms of adjusted P-values, confidence regions and test statistic
cutoffs.  The procedures are directly applicable to identifying differentially
expressed genes in DNA microarray experiments.")
    (license license:lgpl3)))

(define-public r-graph
  (package
    (name "r-graph")
    (version "1.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "graph" version))
              (sha256
               (base32
                "1ivf59k7k552m7zd8g3wwazd71dq3xmgmhcq435738i02h0fqnyc"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)))
    (home-page "https://bioconductor.org/packages/graph")
    (synopsis "Handle graph data structures in R")
    (description
     "This package implements some simple graph handling capabilities for R.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on a Bioconductor package.
(define-public r-ggm
  (package
    (name "r-ggm")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggm" version))
       (sha256
        (base32
         "11wc6k2kj2ydy0dyks5mbvbhxm1r43id87anl1jg6dn0yv4m78di"))))
    (properties `((upstream-name . "ggm")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-graph" ,r-graph)
       ("r-igraph" ,r-igraph)))
    (home-page "https://cran.r-project.org/package=ggm")
    (synopsis "Functions for graphical Markov models")
    (description
     "This package provides functions and datasets for maximum likelihood
fitting of some classes of graphical Markov models.")
    (license license:gpl2+)))

(define-public r-codedepends
  (package
    (name "r-codedepends")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "CodeDepends" version))
       (sha256
        (base32
         "0l7kiv3awx50glf5cs841b4zzsff1ml90f0zr868ygvwsr4ps1hq"))))
    (properties `((upstream-name . "CodeDepends")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-codetools" ,r-codetools)
       ("r-graph" ,r-graph)
       ("r-xml" ,r-xml)))
    (home-page "https://cran.r-project.org/web/packages/CodeDepends")
    (synopsis "Analysis of R code for reproducible research and code comprehension")
    (description
     "This package provides tools for analyzing R expressions or blocks of
code and determining the dependencies between them.  It focuses on R scripts,
but can be used on the bodies of functions.  There are many facilities
including the ability to summarize or get a high-level view of code,
determining dependencies between variables, code improvement suggestions.")
    ;; Any version of the GPL
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-chippeakanno
  (package
    (name "r-chippeakanno")
    (version "3.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPpeakAnno" version))
       (sha256
        (base32
         "0kmfha4vprbi0z6n7v9w28xfrqcx5qad7yfr0b316j5aj8v9f4hc"))))
    (properties `((upstream-name . "ChIPpeakAnno")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocmanager" ,r-biocmanager)
       ("r-biomart" ,r-biomart)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-dbi" ,r-dbi)
       ("r-delayedarray" ,r-delayedarray)
       ("r-ensembldb" ,r-ensembldb)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-go-db" ,r-go-db)
       ("r-graph" ,r-graph)
       ("r-idr" ,r-idr)
       ("r-iranges" ,r-iranges)
       ("r-limma" ,r-limma)
       ("r-matrixstats" ,r-matrixstats)
       ("r-multtest" ,r-multtest)
       ("r-rbgl" ,r-rbgl)
       ("r-regioner" ,r-regioner)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-seqinr" ,r-seqinr)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-venndiagram" ,r-venndiagram)))
    (home-page "https://bioconductor.org/packages/ChIPpeakAnno")
    (synopsis "Peaks annotation from ChIP-seq and ChIP-chip experiments")
    (description
     "The package includes functions to retrieve the sequences around the peak,
obtain enriched Gene Ontology (GO) terms, find the nearest gene, exon, miRNA or
custom features such as most conserved elements and other transcription factor
binding sites supplied by users.  Starting 2.0.5, new functions have been added
for finding the peaks with bi-directional promoters with summary statistics
(peaksNearBDP), for summarizing the occurrence of motifs in peaks
(summarizePatternInPeaks) and for adding other IDs to annotated peaks or
enrichedGO (addGeneIDs).")
    (license license:gpl2+)))

(define-public r-marray
  (package
    (name "r-marray")
    (version "1.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "marray" version))
              (sha256
               (base32 "1mbs9rk279hnm9yz34za3xz3hb88ll1d0abw4m2pgjgbh4kkhdrl"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-limma" ,r-limma)))
    (home-page "https://bioconductor.org/packages/marray")
    (synopsis "Exploratory analysis for two-color spotted microarray data")
    (description "This package contains class definitions for two-color spotted
microarray data.  It also includes functions for data input, diagnostic plots,
normalization and quality checking.")
    (license license:lgpl2.0+)))

(define-public r-cghbase
  (package
   (name "r-cghbase")
   (version "1.46.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHbase" version))
            (sha256
             (base32 "0136pk6pfwpiiy9vca4pgg4wh74jfb5ssglpdszzhamljpvg765x"))))
   (properties `((upstream-name . "CGHbase")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biobase" ,r-biobase)
      ("r-marray" ,r-marray)))
   (home-page "https://bioconductor.org/packages/CGHbase")
   (synopsis "Base functions and classes for arrayCGH data analysis")
   (description "This package contains functions and classes that are needed by
the @code{arrayCGH} packages.")
   (license license:gpl2+)))

(define-public r-cghcall
  (package
   (name "r-cghcall")
   (version "2.48.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHcall" version))
            (sha256
             (base32 "1x8pz7zhw2nabik1vgdhlivndqvil3s7vnl5070k493v6gza0p3s"))))
   (properties `((upstream-name . "CGHcall")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biobase" ,r-biobase)
      ("r-cghbase" ,r-cghbase)
      ("r-impute" ,r-impute)
      ("r-dnacopy" ,r-dnacopy)
      ("r-snowfall" ,r-snowfall)))
   (home-page "https://bioconductor.org/packages/CGHcall")
   (synopsis "Base functions and classes for arrayCGH data analysis")
   (description "This package contains functions and classes that are needed by
@code{arrayCGH} packages.")
   (license license:gpl2+)))

(define-public r-qdnaseq
  (package
    (name "r-qdnaseq")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "QDNAseq" version))
              (sha256
               (base32 "0xcqdpv9a47zpxx0q9sif5y4s1yzx3pig0kywy961kh5xgl5bcrq"))))
    (properties `((upstream-name . "QDNAseq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-cghbase" ,r-cghbase)
       ("r-cghcall" ,r-cghcall)
       ("r-dnacopy" ,r-dnacopy)
       ("r-future" ,r-future)
       ("r-future-apply" ,r-future-apply)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-matrixstats" ,r-matrixstats)
       ("r-r-utils" ,r-r-utils)
       ("r-rsamtools" ,r-rsamtools)))
    (home-page "https://bioconductor.org/packages/QDNAseq")
    (synopsis "Quantitative DNA sequencing for chromosomal aberrations")
    (description "The genome is divided into non-overlapping fixed-sized bins,
number of sequence reads in each counted, adjusted with a simultaneous
two-dimensional loess correction for sequence mappability and GC content, and
filtered to remove spurious regions in the genome.  Downstream steps of
segmentation and calling are also implemented via packages DNAcopy and CGHcall,
respectively.")
    (license license:gpl2+)))

(define-public r-bayseq
  (package
    (name "r-bayseq")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "baySeq" version))
       (sha256
        (base32
         "040s1d3756spyzblkyx2vcy7bn3vf87mhsp3is35yxkj55n3myjk"))))
    (properties `((upstream-name . "baySeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)
       ("r-edger" ,r-edger)
       ("r-genomicranges" ,r-genomicranges)))
    (home-page "https://bioconductor.org/packages/baySeq/")
    (synopsis "Bayesian analysis of differential expression patterns in count data")
    (description
     "This package identifies differential expression in high-throughput count
data, such as that derived from next-generation sequencing machines,
calculating estimated posterior likelihoods of differential expression (or
more complex hypotheses) via empirical Bayesian methods.")
    (license license:gpl3)))

(define-public r-chipcomp
  (package
    (name "r-chipcomp")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPComp" version))
       (sha256
        (base32
         "0wk0vvg6dk9wk60lzbadrnqar75dppvyr4hiwrhv9rhhah2mg2mg"))))
    (properties `((upstream-name . "ChIPComp")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-bsgenome-mmusculus-ucsc-mm9" ,r-bsgenome-mmusculus-ucsc-mm9)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-limma" ,r-limma)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/ChIPComp")
    (synopsis "Quantitative comparison of multiple ChIP-seq datasets")
    (description
     "ChIPComp implements a statistical method for quantitative comparison of
multiple ChIP-seq datasets.  It detects differentially bound sharp binding
sites across multiple conditions considering matching control in ChIP-seq
datasets.")
    ;; Any version of the GPL.
    (license license:gpl3+)))

(define-public r-riboprofiling
  (package
    (name "r-riboprofiling")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RiboProfiling" version))
       (sha256
        (base32
         "0nfzyiq7cd6fs5agzl1zyfg8s631wi7kjngiyvd3vxlhi8wsjicx"))))
    (properties `((upstream-name . "RiboProfiling")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-data-table" ,r-data-table)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggbio" ,r-ggbio)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iranges" ,r-iranges)
       ("r-plyr" ,r-plyr)
       ("r-reshape2" ,r-reshape2)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-sqldf" ,r-sqldf)))
    (home-page "https://bioconductor.org/packages/RiboProfiling/")
    (synopsis "Ribosome profiling data analysis")
    (description "Starting with a BAM file, this package provides the
necessary functions for quality assessment, read start position recalibration,
the counting of genomic sequence reads on CDS, 3'UTR, and 5'UTR, and plotting
of count data: pairs, log fold-change, codon frequency and coverage
assessment, principal component analysis on codon coverage.")
    (license license:gpl3)))

(define-public r-riboseqr
  (package
    (name "r-riboseqr")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "riboSeqR" version))
       (sha256
        (base32
         "1jr7h64hyhyf9gf15lah6iqwyljfc8mraf9kya4lql6lcjjkjiqm"))))
    (properties `((upstream-name . "riboSeqR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)
       ("r-bayseq" ,r-bayseq)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-seqlogo" ,r-seqlogo)))
    (home-page "https://bioconductor.org/packages/riboSeqR/")
    (synopsis "Analysis of sequencing data from ribosome profiling experiments")
    (description
     "This package provides plotting functions, frameshift detection and
parsing of genetic sequencing data from ribosome profiling experiments.")
    (license license:gpl3)))

(define-public r-interactionset
  (package
    (name "r-interactionset")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "InteractionSet" version))
       (sha256
        (base32
         "0n2l95h56x5g68p10cap8p4x3a6vaph2hjlk09vmi3j48lrzb2kh"))))
    (properties
     `((upstream-name . "InteractionSet")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/InteractionSet")
    (synopsis "Base classes for storing genomic interaction data")
    (description
     "This package provides the @code{GInteractions},
@code{InteractionSet} and @code{ContactMatrix} objects and associated methods
for storing and manipulating genomic interaction data from Hi-C and ChIA-PET
experiments.")
    (license license:gpl3)))

(define-public r-genomicinteractions
  (package
    (name "r-genomicinteractions")
    (version "1.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicInteractions" version))
       (sha256
        (base32
         "140bp7c8p079xh5wwxmrq3a73pm6mdc9czq5w2gzjml7dgrca38a"))))
    (properties
     `((upstream-name . "GenomicInteractions")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-data-table" ,r-data-table)
       ("r-dplyr" ,r-dplyr)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-gviz" ,r-gviz)
       ("r-igraph" ,r-igraph)
       ("r-interactionset" ,r-interactionset)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-stringr" ,r-stringr)))
    (home-page "https://github.com/ComputationalRegulatoryGenomicsICL/GenomicInteractions/")
    (synopsis "R package for handling genomic interaction data")
    (description
     "This R package provides tools for handling genomic interaction data,
such as ChIA-PET/Hi-C, annotating genomic features with interaction
information and producing various plots and statistics.")
    (license license:gpl3)))

(define-public r-ctc
  (package
    (name "r-ctc")
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ctc" version))
       (sha256
        (base32
         "0wh27izkyr1j26nznisw654mb5c94xpwjjkx7r6bhwg9ihxxcl6d"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-amap" ,r-amap)))
    (home-page "https://bioconductor.org/packages/ctc/")
    (synopsis "Cluster and tree conversion")
    (description
     "This package provides tools for exporting and importing classification
trees and clusters to other programs.")
    (license license:gpl2)))

(define-public r-goseq
  (package
    (name "r-goseq")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "goseq" version))
       (sha256
        (base32
         "11ypa41qv1nx3cncxlwlbhdxqlwq95rb9byv2z3crrf9nfp24byv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biasedurn" ,r-biasedurn)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-genelendatabase" ,r-genelendatabase)
       ("r-go-db" ,r-go-db)
       ("r-mgcv" ,r-mgcv)))
    (home-page "https://bioconductor.org/packages/goseq/")
    (synopsis "Gene Ontology analyser for RNA-seq and other length biased data")
    (description
     "This package provides tools to detect Gene Ontology and/or other user
defined categories which are over/under represented in RNA-seq data.")
    (license license:lgpl2.0+)))

(define-public r-glimma
  (package
    (name "r-glimma")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Glimma" version))
       (sha256
        (base32
         "1rjrqgl96iz4b3xqpc174wgz7bqmc8gbm9ljag0y27kz29fwng8r"))))
    (properties `((upstream-name . "Glimma")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-edger" ,r-edger)
       ("r-jsonlite" ,r-jsonlite)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://github.com/Shians/Glimma")
    (synopsis "Interactive HTML graphics")
    (description
     "This package generates interactive visualisations for analysis of
RNA-sequencing data using output from limma, edgeR or DESeq2 packages in an
HTML page.  The interactions are built on top of the popular static
representations of analysis results in order to provide additional
information.")
    (license license:lgpl3)))

(define-public r-rots
  (package
    (name "r-rots")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROTS" version))
       (sha256
        (base32
         "08mwlb0lpprys2b7vif8aj5bnprmn09mm79zz158gbhrv9j9d1qm"))))
    (properties `((upstream-name . "ROTS")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://bioconductor.org/packages/ROTS/")
    (synopsis "Reproducibility-Optimized Test Statistic")
    (description
     "This package provides tools for calculating the
@dfn{Reproducibility-Optimized Test Statistic} (ROTS) for differential testing
in omics data.")
    (license license:gpl2+)))

(define-public r-plgem
  (package
    (name "r-plgem")
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "plgem" version))
       (sha256
        (base32
         "0fkyvcw2qxp4g527s9rzia45yapi0r6gbij7svisil8rbgfdp45v"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-mass" ,r-mass)))
    (home-page "http://www.genopolis.it")
    (synopsis "Detect differential expression in microarray and proteomics datasets")
    (description
     "The Power Law Global Error Model (PLGEM) has been shown to faithfully
model the variance-versus-mean dependence that exists in a variety of
genome-wide datasets, including microarray and proteomics data.  The use of
PLGEM has been shown to improve the detection of differentially expressed
genes or proteins in these datasets.")
    (license license:gpl2)))

(define-public r-inspect
  (package
    (name "r-inspect")
    (version "1.16.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "INSPEcT" version))
       (sha256
        (base32
         "0y4gpvrbmcipdnyz9lz9ywn0317xnpcriiisqvxygin05vgcpilp"))))
    (properties `((upstream-name . "INSPEcT")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-deseq2" ,r-deseq2)
       ("r-desolve" ,r-desolve)
       ("r-gdata" ,r-gdata)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-kernsmooth" ,r-kernsmooth)
       ("r-plgem" ,r-plgem)
       ("r-proc" ,r-proc)
       ("r-rootsolve" ,r-rootsolve)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shiny" ,r-shiny)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-txdb-mmusculus-ucsc-mm9-knowngene"
        ,r-txdb-mmusculus-ucsc-mm9-knowngene)))
    (home-page "https://bioconductor.org/packages/INSPEcT")
    (synopsis "Analysis of 4sU-seq and RNA-seq time-course data")
    (description
     "INSPEcT (INference of Synthesis, Processing and dEgradation rates in
Time-Course experiments) analyses 4sU-seq and RNA-seq time-course data in
order to evaluate synthesis, processing and degradation rates and assess via
modeling the rates that determines changes in mature mRNA levels.")
    (license license:gpl2)))

(define-public r-dnabarcodes
  (package
    (name "r-dnabarcodes")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNABarcodes" version))
       (sha256
        (base32
         "0r2r9qc2qvf7rfl1h5ynvv3xd7n444zbc697s85qxqdpr4sxqmfd"))))
    (properties `((upstream-name . "DNABarcodes")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://bioconductor.org/packages/DNABarcodes")
    (synopsis "Create and analyze DNA barcodes")
    (description
     "This package offers tools to create DNA barcode sets capable of
correcting insertion, deletion, and substitution errors.  Existing barcodes
can be analyzed regarding their minimal, maximal and average distances between
barcodes.  Finally, reads that start with a (possibly mutated) barcode can be
demultiplexed, i.e. assigned to their original reference barcode.")
    (license license:gpl2)))

(define-public r-ruvseq
  (package
    (name "r-ruvseq")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RUVSeq" version))
       (sha256
        (base32
         "1ipbbzpngx988lsmwqv7vbmqm65m43xvsmipayfppkrr6jipzxrj"))))
    (properties `((upstream-name . "RUVSeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-edaseq" ,r-edaseq)
       ("r-edger" ,r-edger)
       ("r-mass" ,r-mass)))
    (home-page "https://github.com/drisso/RUVSeq")
    (synopsis "Remove unwanted variation from RNA-Seq data")
    (description
     "This package implements methods to @dfn{remove unwanted variation} (RUV)
of Risso et al. (2014) for the normalization of RNA-Seq read counts between
samples.")
    (license license:artistic2.0)))

(define-public r-biocneighbors
  (package
    (name "r-biocneighbors")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocNeighbors" version))
       (sha256
        (base32
         "1bx7i5pifj8w89fnhfgcfgcar2ik2ad8wqs2rix7yks90vz185i6"))))
    (properties `((upstream-name . "BiocNeighbors")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppannoy" ,r-rcppannoy)
       ("r-rcpphnsw" ,r-rcpphnsw)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/BiocNeighbors")
    (synopsis "Nearest Neighbor Detection for Bioconductor packages")
    (description
     "This package implements exact and approximate methods for nearest
neighbor detection, in a framework that allows them to be easily switched
within Bioconductor packages or workflows.  The exact algorithm is implemented
using pre-clustering with the k-means algorithm.  Functions are also provided
to search for all neighbors within a given distance.  Parallelization is
achieved for all methods using the BiocParallel framework.")
    (license license:gpl3)))

(define-public r-biocsingular
  (package
    (name "r-biocsingular")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSingular" version))
       (sha256
        (base32
         "1282fdwxrpga87y8xflr0sl8ajwawgn2i9i65my8mbg5vrixs407"))))
    (properties `((upstream-name . "BiocSingular")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-delayedarray" ,r-delayedarray)
       ("r-irlba" ,r-irlba)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rsvd" ,r-rsvd)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://github.com/LTLA/BiocSingular")
    (synopsis "Singular value decomposition for Bioconductor packages")
    (description
     "This package implements exact and approximate methods for singular value
decomposition and principal components analysis, in a framework that allows
them to be easily switched within Bioconductor packages or workflows.  Where
possible, parallelization is achieved using the BiocParallel framework.")
    (license license:gpl3)))

(define-public r-destiny
  (package
    (name "r-destiny")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "destiny" version))
       (sha256
        (base32
         "1hzg53p1cz21bvnfyyz40bpvjhg89zi3rahlqf0c4w85iwc1i4vi"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-ggplot-multistats" ,r-ggplot-multistats)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggthemes" ,r-ggthemes)
       ("r-irlba" ,r-irlba)
       ("r-knn-covertree" ,r-knn-covertree)
       ("r-matrix" ,r-matrix)
       ("r-pcamethods" ,r-pcamethods)
       ("r-proxy" ,r-proxy)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-rcpphnsw" ,r-rcpphnsw)
       ("r-rspectra" ,r-rspectra)
       ("r-scales" ,r-scales)
       ("r-scatterplot3d" ,r-scatterplot3d)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-smoother" ,r-smoother)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-tidyr" ,r-tidyr)
       ("r-tidyselect" ,r-tidyselect)
       ("r-vim" ,r-vim)))
    (home-page "https://bioconductor.org/packages/destiny/")
    (synopsis "Create and plot diffusion maps")
    (description "This package provides tools to create and plot diffusion
maps.")
    ;; Any version of the GPL
    (license license:gpl3+)))

(define-public r-savr
  (package
    (name "r-savr")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "savR" version))
       (sha256
        (base32
         "1lsnqjl6qxbj0wai05qb2wrxrhxq5iarv2livmvwiwv70iigqygf"))))
    (properties `((upstream-name . "savR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-reshape2" ,r-reshape2)
       ("r-scales" ,r-scales)
       ("r-xml" ,r-xml)))
    (home-page "https://github.com/bcalder/savR")
    (synopsis "Parse and analyze Illumina SAV files")
    (description
     "This package provides tools to parse Illumina Sequence Analysis
Viewer (SAV) files, access data, and generate QC plots.")
    (license license:agpl3+)))

(define-public r-chipexoqual
  (package
    (name "r-chipexoqual")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPexoQual" version))
       (sha256
        (base32
         "044n6kn16qczpdhp0w0z5x8xpr0rfs9s8q70rafgnvj7a2q1xdd0"))))
    (properties `((upstream-name . "ChIPexoQual")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-biovizbase" ,r-biovizbase)
       ("r-broom" ,r-broom)
       ("r-data-table" ,r-data-table)
       ("r-dplyr" ,r-dplyr)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-hexbin" ,r-hexbin)
       ("r-iranges" ,r-iranges)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scales" ,r-scales)
       ("r-viridis" ,r-viridis)))
    (home-page "https://github.com/keleslab/ChIPexoQual")
    (synopsis "Quality control pipeline for ChIP-exo/nexus data")
    (description
     "This package provides a quality control pipeline for ChIP-exo/nexus
sequencing data.")
    (license license:gpl2+)))

(define-public r-copynumber
  (package
    (name "r-copynumber")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "copynumber" version))
              (sha256
               (base32
                "0r6r1adj20x3ckm0dlipxlf1rzngr92xsxxpy81mqxf4jpmyr8gj"))))
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

(define-public r-dnacopy
  (package
    (name "r-dnacopy")
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNAcopy" version))
       (sha256
        (base32
         "119z5lqhhw9ppg6s4dvbxk1kxf3wc55ibpm9b88c91s04yd7m9yw"))))
    (properties `((upstream-name . "DNAcopy")))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://bioconductor.org/packages/DNAcopy")
    (synopsis "DNA copy number data analysis")
    (description
     "This package implements the @dfn{circular binary segmentation} (CBS)
algorithm to segment DNA copy number data and identify genomic regions with
abnormal copy number.")
    (license license:gpl2+)))

;; This is a CRAN package, but it uncharacteristically depends on a
;; Bioconductor package.
(define-public r-htscluster
  (package
    (name "r-htscluster")
    (version "2.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "HTSCluster" version))
       (sha256
        (base32
         "0wnbfh6hdx8692jilgmv8sys1zm6fqc6mim7vvjhyqlmpm8gm0kg"))))
    (properties `((upstream-name . "HTSCluster")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-capushe" ,r-capushe)
       ("r-edger" ,r-edger)
       ("r-plotrix" ,r-plotrix)))
    (home-page "https://cran.r-project.org/web/packages/HTSCluster")
    (synopsis "Clustering high-throughput transcriptome sequencing (HTS) data")
    (description
     "This package provides a Poisson mixture model is implemented to cluster
genes from high-throughput transcriptome sequencing (RNA-seq) data.  Parameter
estimation is performed using either the EM or CEM algorithm, and the slope
heuristics are used for model selection (i.e., to choose the number of
clusters).")
    (license license:gpl3+)))

(define-public r-deds
  (package
    (name "r-deds")
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DEDS" version))
       (sha256
        (base32
         "0vzsmah2lhxf8k6n4d0i4j609sbvygmb6ii2ridg9z3nskwkrhp8"))))
    (properties `((upstream-name . "DEDS")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/DEDS/")
    (synopsis "Differential expression via distance summary for microarray data")
    (description
     "This library contains functions that calculate various statistics of
differential expression for microarray data, including t statistics, fold
change, F statistics, SAM, moderated t and F statistics and B statistics.  It
also implements a new methodology called DEDS (Differential Expression via
Distance Summary), which selects differentially expressed genes by integrating
and summarizing a set of statistics using a weighted distance approach.")
    ;; Any version of the LGPL.
    (license license:lgpl3+)))

;; This is a CRAN package, but since it depends on a Bioconductor package we
;; put it here.
(define-public r-nbpseq
  (package
    (name "r-nbpseq")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "NBPSeq" version))
       (sha256
        (base32
         "0l4ylxhs2k9ww21jjqs67fygk92avdchhx2y1ixzl7yr2yh1y9by"))))
    (properties `((upstream-name . "NBPSeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-qvalue" ,r-qvalue)))
    (home-page "https://cran.r-project.org/web/packages/NBPSeq")
    (synopsis "Negative binomial models for RNA-Seq data")
    (description
     "This package provides negative binomial models for two-group comparisons
and regression inferences from RNA-sequencing data.")
    (license license:gpl2)))

(define-public r-ebseq
  (package
    (name "r-ebseq")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBSeq" version))
       (sha256
        (base32
         "0ay1vcccpc29r3vinlnwp9256345bfb346kg2y11kib4bnrabjb6"))))
    (properties `((upstream-name . "EBSeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-blockmodeling" ,r-blockmodeling)
       ("r-gplots" ,r-gplots)
       ("r-testthat" ,r-testthat)))
    (home-page "https://bioconductor.org/packages/EBSeq")
    (synopsis "Differential expression analysis of RNA-seq data")
    (description
     "This package provides tools for differential expression analysis at both
gene and isoform level using RNA-seq data")
    (license license:artistic2.0)))

(define-public r-lpsymphony
  (package
    (name "r-lpsymphony")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lpsymphony" version))
       (sha256
        (base32
         "0j5j9kggh3l61vp6hpnqf45d5kzifksaj0sqhvs1zahmx2c1gfdv"))))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://r-forge.r-project.org/projects/rsymphony")
    (synopsis "Symphony integer linear programming solver in R")
    (description
     "This package was derived from Rsymphony.  The package provides an R
interface to SYMPHONY, a linear programming solver written in C++.  The main
difference between this package and Rsymphony is that it includes the solver
source code, while Rsymphony expects to find header and library files on the
users' system.  Thus the intention of @code{lpsymphony} is to provide an easy
to install interface to SYMPHONY.")
    ;; Symphony 5.4 or later is distributed under the terms of the EPL 1.0.
    ;; lpsimphony is released under the same terms.
    (license license:epl1.0)))

(define-public r-ihw
  (package
    (name "r-ihw")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IHW" version))
       (sha256
        (base32
         "0rnw7r9pylpj3a5graavcpiqv2v67rv2a4dlcynkf4ihpxs4bg8x"))))
    (properties `((upstream-name . "IHW")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-fdrtool" ,r-fdrtool)
       ("r-lpsymphony" ,r-lpsymphony)
       ("r-slam" ,r-slam)))
    (home-page "https://bioconductor.org/packages/IHW")
    (synopsis "Independent hypothesis weighting")
    (description
     "@dfn{Independent hypothesis weighting} (IHW) is a multiple testing
procedure that increases power compared to the method of Benjamini and
Hochberg by assigning data-driven weights to each hypothesis.  The input to
IHW is a two-column table of p-values and covariates.  The covariate can be
any continuous-valued or categorical variable that is thought to be
informative on the statistical properties of each hypothesis test, while it is
independent of the p-value under the null hypothesis.")
    (license license:artistic2.0)))

(define-public r-icobra
  (package
    (name "r-icobra")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iCOBRA" version))
       (sha256
        (base32
         "1l0yr2grpwmr3pc5h50p1j4nxyb1ddmc55l7rhxpp4igh4fq1avz"))))
    (properties `((upstream-name . "iCOBRA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-dt" ,r-dt)
       ("r-ggplot2" ,r-ggplot2)
       ("r-limma" ,r-limma)
       ("r-reshape2" ,r-reshape2)
       ("r-rocr" ,r-rocr)
       ("r-scales" ,r-scales)
       ("r-shiny" ,r-shiny)
       ("r-shinybs" ,r-shinybs)
       ("r-shinydashboard" ,r-shinydashboard)
       ("r-upsetr" ,r-upsetr)))
    (home-page "https://bioconductor.org/packages/iCOBRA")
    (synopsis "Comparison and visualization of ranking and assignment methods")
    (description
     "This package provides functions for calculation and visualization of
performance metrics for evaluation of ranking and binary
classification (assignment) methods.  It also contains a Shiny application for
interactive exploration of results.")
    (license license:gpl2+)))

(define-public r-mast
  (package
    (name "r-mast")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MAST" version))
       (sha256
        (base32
         "1l78rrwkzyswpj6pgc9z8290fqrk4akba76invkkazpyh91r8gga"))))
    (properties `((upstream-name . "MAST")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-data-table" ,r-data-table)
       ("r-ggplot2" ,r-ggplot2)
       ("r-plyr" ,r-plyr)
       ("r-progress" ,r-progress)
       ("r-reshape2" ,r-reshape2)
       ("r-s4vectors" ,r-s4vectors)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-stringr" ,r-stringr)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://github.com/RGLab/MAST/")
    (synopsis "Model-based analysis of single cell transcriptomics")
    (description
     "This package provides methods and models for handling zero-inflated
single cell assay data.")
    (license license:gpl2+)))

(define-public r-monocle
  (package
    (name "r-monocle")
    (version "2.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "monocle" version))
       (sha256
        (base32
         "18b9y592q48mbcp5w095qs1kaklq64v6dcnlpqhv3rwxvywygsz2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocviews" ,r-biocviews)
       ("r-cluster" ,r-cluster)
       ("r-combinat" ,r-combinat)
       ("r-ddrtree" ,r-ddrtree)
       ("r-densityclust" ,r-densityclust)
       ("r-dplyr" ,r-dplyr)
       ("r-fastica" ,r-fastica)
       ("r-ggplot2" ,r-ggplot2)
       ("r-hsmmsinglecell" ,r-hsmmsinglecell)
       ("r-igraph" ,r-igraph)
       ("r-irlba" ,r-irlba)
       ("r-limma" ,r-limma)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-matrixstats" ,r-matrixstats)
       ("r-pheatmap" ,r-pheatmap)
       ("r-plyr" ,r-plyr)
       ("r-proxy" ,r-proxy)
       ("r-qlcmatrix" ,r-qlcmatrix)
       ("r-rann" ,r-rann)
       ("r-rcpp" ,r-rcpp)
       ("r-reshape2" ,r-reshape2)
       ("r-rtsne" ,r-rtsne)
       ("r-slam" ,r-slam)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-vgam" ,r-vgam)
       ("r-viridis" ,r-viridis)))
    (home-page "https://bioconductor.org/packages/monocle")
    (synopsis "Clustering, differential expression, and trajectory analysis for single-cell RNA-Seq")
    (description
     "Monocle performs differential expression and time-series analysis for
single-cell expression experiments.  It orders individual cells according to
progress through a biological process, without knowing ahead of time which
genes define progress through that process.  Monocle also performs
differential expression analysis, clustering, visualization, and other useful
tasks on single cell expression data.  It is designed to work with RNA-Seq and
qPCR data, but could be used with other types as well.")
    (license license:artistic2.0)))

(define-public r-monocle3
  (package
    (name "r-monocle3")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cole-trapnell-lab/monocle3.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1cjxqfw3qvy269hsf5v80d4kshl932wrl949iayas02saj6f70ls"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-grr" ,r-grr)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-igraph" ,r-igraph)
       ("r-irlba" ,r-irlba)
       ("r-limma" ,r-limma)
       ("r-lmtest" ,r-lmtest)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-matrix-utils" ,r-matrix-utils)
       ("r-pbapply" ,r-pbapply)
       ("r-pbmcapply" ,r-pbmcapply)
       ("r-pheatmap" ,r-pheatmap)
       ("r-plotly" ,r-plotly)
       ("r-pryr" ,r-pryr)
       ("r-proxy" ,r-proxy)
       ("r-pscl" ,r-pscl)
       ("r-purrr" ,r-purrr)
       ("r-rann" ,r-rann)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppparallel" ,r-rcppparallel)
       ("r-reshape2" ,r-reshape2)
       ("r-reticulate" ,r-reticulate)
       ("r-rhpcblasctl" ,r-rhpcblasctl)
       ("r-rtsne" ,r-rtsne)
       ("r-shiny" ,r-shiny)
       ("r-slam" ,r-slam)
       ("r-spdep" ,r-spdep)
       ("r-speedglm" ,r-speedglm)
       ("r-stringr" ,r-stringr)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-uwot" ,r-uwot)
       ("r-viridis" ,r-viridis)))
    (home-page "https://github.com/cole-trapnell-lab/monocle3")
    (synopsis "Analysis toolkit for single-cell RNA-Seq data")
    (description
     "Monocle 3 is an analysis toolkit for single-cell RNA-Seq experiments.")
    (license license:expat)))

(define-public r-noiseq
  (package
    (name "r-noiseq")
    (version "2.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "NOISeq" version))
       (sha256
        (base32
         "1a8p66hhnwmay0rjabwq7356wr93yn33nqgr9rr7whpp9nls1hg4"))))
    (properties `((upstream-name . "NOISeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-matrix" ,r-matrix)))
    (home-page "https://bioconductor.org/packages/NOISeq")
    (synopsis "Exploratory analysis and differential expression for RNA-seq data")
    (description
     "This package provides tools to support the analysis of RNA-seq
expression data or other similar kind of data.  It provides exploratory plots
to evaluate saturation, count distribution, expression per chromosome, type of
detected features, features length, etc.  It also supports the analysis of
differential expression between two experimental conditions with no parametric
assumptions.")
    (license license:artistic2.0)))

(define-public r-scdd
  (package
    (name "r-scdd")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scDD" version))
       (sha256
        (base32
         "0nk0qq664zs7dnlcamdkrrhvll01p7f96jj3igbxxlzj3dvr58w4"))))
    (properties `((upstream-name . "scDD")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-arm" ,r-arm)
       ("r-biocparallel" ,r-biocparallel)
       ("r-ebseq" ,r-ebseq)
       ("r-fields" ,r-fields)
       ("r-ggplot2" ,r-ggplot2)
       ("r-mclust" ,r-mclust)
       ("r-outliers" ,r-outliers)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scran" ,r-scran)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://github.com/kdkorthauer/scDD")
    (synopsis "Mixture modeling of single-cell RNA-seq data")
    (description
     "This package implements a method to analyze single-cell RNA-seq data
utilizing flexible Dirichlet Process mixture models.  Genes with differential
distributions of expression are classified into several interesting patterns
of differences between two conditions.  The package also includes functions
for simulating data with these patterns from negative binomial
distributions.")
    (license license:gpl2)))

(define-public r-scone
  (package
    (name "r-scone")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scone" version))
       (sha256
        (base32
         "1hfsbnix0y9ad6bj0d8q2aw13pb3vjcaa0dfwxxapzl90zwbsjhp"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-aroma-light" ,r-aroma-light)
       ("r-biocparallel" ,r-biocparallel)
       ("r-boot" ,r-boot)
       ("r-class" ,r-class)
       ("r-cluster" ,r-cluster)
       ("r-compositions" ,r-compositions)
       ("r-diptest" ,r-diptest)
       ("r-edger" ,r-edger)
       ("r-fpc" ,r-fpc)
       ("r-gplots" ,r-gplots)
       ("r-hexbin" ,r-hexbin)
       ("r-limma" ,r-limma)
       ("r-matrixstats" ,r-matrixstats)
       ("r-mixtools" ,r-mixtools)
       ("r-rarpack" ,r-rarpack)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rhdf5" ,r-rhdf5)
       ("r-ruvseq" ,r-ruvseq)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/scone")
    (synopsis "Single cell overview of normalized expression data")
    (description
     "SCONE is an R package for comparing and ranking the performance of
different normalization schemes for single-cell RNA-seq and other
high-throughput analyses.")
    (license license:artistic2.0)))

(define-public r-geoquery
  (package
    (name "r-geoquery")
    (version "2.54.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GEOquery" version))
       (sha256
        (base32
         "0j8mlldy7dc38fx5zwj6z6l4b9bhvrn8sil3z8asnd4ic7w9ifx5"))))
    (properties `((upstream-name . "GEOquery")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-dplyr" ,r-dplyr)
       ("r-httr" ,r-httr)
       ("r-limma" ,r-limma)
       ("r-magrittr" ,r-magrittr)
       ("r-readr" ,r-readr)
       ("r-tidyr" ,r-tidyr)
       ("r-xml2" ,r-xml2)))
    (home-page "https://github.com/seandavi/GEOquery/")
    (synopsis "Get data from NCBI Gene Expression Omnibus (GEO)")
    (description
     "The NCBI Gene Expression Omnibus (GEO) is a public repository of
microarray data.  Given the rich and varied nature of this resource, it is
only natural to want to apply BioConductor tools to these data.  GEOquery is
the bridge between GEO and BioConductor.")
    (license license:gpl2)))

(define-public r-illuminaio
  (package
    (name "r-illuminaio")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "illuminaio" version))
       (sha256
        (base32
         "1psza8jq6h8fv2rm91ah67dgjlnj1l80yracwgdl1agd0ycv90sh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64" ,r-base64)))
    (home-page "https://github.com/HenrikBengtsson/illuminaio/")
    (synopsis "Parse Illumina microarray output files")
    (description
     "This package provides tools for parsing Illumina's microarray output
files, including IDAT.")
    (license license:gpl2)))

(define-public r-siggenes
  (package
    (name "r-siggenes")
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "siggenes" version))
       (sha256
        (base32
         "03lmq3hqprwps4miynl2vhqi3v4als5vqmz4lb19lk5a4zja72b4"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-multtest" ,r-multtest)
       ("r-scrime" ,r-scrime)))
    (home-page "https://bioconductor.org/packages/siggenes/")
    (synopsis
     "Multiple testing using SAM and Efron's empirical Bayes approaches")
    (description
     "This package provides tools for the identification of differentially
expressed genes and estimation of the @dfn{False Discovery Rate} (FDR) using
both the Significance Analysis of Microarrays (SAM) and the @dfn{Empirical
Bayes Analyses of Microarrays} (EBAM).")
    (license license:lgpl2.0+)))

(define-public r-bumphunter
  (package
    (name "r-bumphunter")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bumphunter" version))
       (sha256
        (base32
         "1p3gwb954zns61d1pwgkplxnvgk2lixrl93kkkf439wa3vlgsnjv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-dorng" ,r-dorng)
       ("r-foreach" ,r-foreach)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-iterators" ,r-iterators)
       ("r-limma" ,r-limma)
       ("r-locfit" ,r-locfit)
       ("r-matrixstats" ,r-matrixstats)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://github.com/ririzarr/bumphunter")
    (synopsis "Find bumps in genomic data")
    (description
     "This package provides tools for finding bumps in genomic data in order
to identify differentially methylated regions in epigenetic epidemiology
studies.")
    (license license:artistic2.0)))

(define-public r-minfi
  (package
    (name "r-minfi")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "minfi" version))
       (sha256
        (base32
         "177isdvcq2476xybvfbh7x11qsj5nzckgh3b6p156wlx9ap9dvl3"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beanplot" ,r-beanplot)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-bumphunter" ,r-bumphunter)
       ("r-data-table" ,r-data-table)
       ("r-delayedarray" ,r-delayedarray)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-genefilter" ,r-genefilter)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-geoquery" ,r-geoquery)
       ("r-hdf5array" ,r-hdf5array)
       ("r-illuminaio" ,r-illuminaio)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-limma" ,r-limma)
       ("r-mass" ,r-mass)
       ("r-mclust" ,r-mclust)
       ("r-nlme" ,r-nlme)
       ("r-nor1mix" ,r-nor1mix)
       ("r-preprocesscore" ,r-preprocesscore)
       ("r-quadprog" ,r-quadprog)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-reshape" ,r-reshape)
       ("r-s4vectors" ,r-s4vectors)
       ("r-siggenes" ,r-siggenes)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://github.com/hansenlab/minfi")
    (synopsis "Analyze Illumina Infinium DNA methylation arrays")
    (description
     "This package provides tools to analyze and visualize Illumina Infinium
methylation arrays.")
    (license license:artistic2.0)))

(define-public r-methylumi
  (package
    (name "r-methylumi")
    (version "2.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "methylumi" version))
       (sha256
        (base32
         "0zd4h6n37v3z0mas69z2xsf5s0naih828987m8v0g9hhq4f6hf5w"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-fdb-infiniummethylation-hg19" ,r-fdb-infiniummethylation-hg19)
       ("r-genefilter" ,r-genefilter)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-illuminaio" ,r-illuminaio)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-matrixstats" ,r-matrixstats)
       ("r-minfi" ,r-minfi)
       ("r-reshape2" ,r-reshape2)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scales" ,r-scales)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/methylumi")
    (synopsis "Handle Illumina methylation data")
    (description
     "This package provides classes for holding and manipulating Illumina
methylation data.  Based on eSet, it can contain MIAME information, sample
information, feature information, and multiple matrices of data.  An
\"intelligent\" import function, methylumiR can read the Illumina text files
and create a MethyLumiSet.  methylumIDAT can directly read raw IDAT files from
HumanMethylation27 and HumanMethylation450 microarrays.  Normalization,
background correction, and quality control features for GoldenGate, Infinium,
and Infinium HD arrays are also included.")
    (license license:gpl2)))

(define-public r-lumi
  (package
    (name "r-lumi")
    (version "2.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lumi" version))
       (sha256
        (base32
         "0lgrqbdvp7ykcafc0bpnbcsf396na3jj5c35x9ysf5bpcas23nmm"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-dbi" ,r-dbi)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-kernsmooth" ,r-kernsmooth)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-methylumi" ,r-methylumi)
       ("r-mgcv" ,r-mgcv)
       ("r-nleqslv" ,r-nleqslv)
       ("r-preprocesscore" ,r-preprocesscore)
       ("r-rsqlite" ,r-rsqlite)))
    (home-page "https://bioconductor.org/packages/lumi")
    (synopsis "BeadArray-specific methods for Illumina methylation and expression microarrays")
    (description
     "The lumi package provides an integrated solution for the Illumina
microarray data analysis.  It includes functions of Illumina
BeadStudio (GenomeStudio) data input, quality control, BeadArray-specific
variance stabilization, normalization and gene annotation at the probe level.
It also includes the functions of processing Illumina methylation microarrays,
especially Illumina Infinium methylation microarrays.")
    (license license:lgpl2.0+)))

(define-public r-linnorm
  (package
    (name "r-linnorm")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Linnorm" version))
       (sha256
        (base32
         "15mhwiqmp9m65zvrdbr2hhy3x81lf4jbwgjsf75g41if2v2g8x67"))))
    (properties `((upstream-name . "Linnorm")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-amap" ,r-amap)
       ("r-apcluster" ,r-apcluster)
       ("r-ellipse" ,r-ellipse)
       ("r-fastcluster" ,r-fastcluster)
       ("r-fpc" ,r-fpc)
       ("r-ggdendro" ,r-ggdendro)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gmodels" ,r-gmodels)
       ("r-igraph" ,r-igraph)
       ("r-limma" ,r-limma)
       ("r-mass" ,r-mass)
       ("r-mclust" ,r-mclust)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rtsne" ,r-rtsne)
       ("r-statmod" ,r-statmod)
       ("r-vegan" ,r-vegan)
       ("r-zoo" ,r-zoo)))
    (home-page "http://www.jjwanglab.org/Linnorm/")
    (synopsis "Linear model and normality based transformation method")
    (description
     "Linnorm is an R package for the analysis of RNA-seq, scRNA-seq, ChIP-seq
count data or any large scale count data.  It transforms such datasets for
parametric tests.  In addition to the transformtion function (@code{Linnorm}),
the following pipelines are implemented:

@enumerate
@item Library size/batch effect normalization (@code{Linnorm.Norm})
@item Cell subpopluation analysis and visualization using t-SNE or PCA K-means
  clustering or hierarchical clustering (@code{Linnorm.tSNE},
  @code{Linnorm.PCA}, @code{Linnorm.HClust})
@item Differential expression analysis or differential peak detection using
  limma (@code{Linnorm.limma})
@item Highly variable gene discovery and visualization (@code{Linnorm.HVar})
@item Gene correlation network analysis and visualization (@code{Linnorm.Cor})
@item Stable gene selection for scRNA-seq data; for users without or who do
  not want to rely on spike-in genes (@code{Linnorm.SGenes})
@item Data imputation (@code{Linnorm.DataImput}).
@end enumerate

Linnorm can work with raw count, CPM, RPKM, FPKM and TPM.  Additionally, the
@code{RnaXSim} function is included for simulating RNA-seq data for the
evaluation of DEG analysis methods.")
    (license license:expat)))

(define-public r-ioniser
  (package
    (name "r-ioniser")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IONiseR" version))
       (sha256
        (base32
         "1c265bzh923i9mw83mjpj0bzbkixbs6sg1h1z51y9xjkakdgg90f"))))
    (properties `((upstream-name . "IONiseR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-bit64" ,r-bit64)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-magrittr" ,r-magrittr)
       ("r-rhdf5" ,r-rhdf5)
       ("r-shortread" ,r-shortread)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-xvector" ,r-xvector)))
    (home-page "https://bioconductor.org/packages/IONiseR/")
    (synopsis "Quality assessment tools for Oxford Nanopore MinION data")
    (description
     "IONiseR provides tools for the quality assessment of Oxford Nanopore
MinION data.  It extracts summary statistics from a set of fast5 files and can
be used either before or after base calling.  In addition to standard
summaries of the read-types produced, it provides a number of plots for
visualising metrics relative to experiment run time or spatially over the
surface of a flowcell.")
    (license license:expat)))

;; This is a CRAN package, but it depends on packages from Bioconductor.
(define-public r-gkmsvm
  (package
    (name "r-gkmsvm")
    (version "0.80.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gkmSVM" version))
       (sha256
        (base32
         "0ljcga246ad0ql8x3drvrdsyp0f20mgp3p6lnl79xb76qgfdnm0p"))))
    (properties `((upstream-name . "gkmSVM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-kernlab" ,r-kernlab)
       ("r-rcpp" ,r-rcpp)
       ("r-rocr" ,r-rocr)
       ("r-seqinr" ,r-seqinr)))
    (home-page "https://cran.r-project.org/web/packages/gkmSVM")
    (synopsis "Gapped-kmer support vector machine")
    (description
     "This R package provides tools for training gapped-kmer SVM classifiers
for DNA and protein sequences.  This package supports several sequence
kernels, including: gkmSVM, kmer-SVM, mismatch kernel and wildcard kernel.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on multtest from Bioconductor.
(define-public r-mutoss
  (package
    (name "r-mutoss")
    (version "0.1-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mutoss" version))
       (sha256
        (base32
         "1yk7p7pb2xm38d3j19ysgwmix48lvimbhkhjjwk5jmr1a0ysx298"))))
    (properties `((upstream-name . "mutoss")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-multcomp" ,r-multcomp)
       ("r-multtest" ,r-multtest)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-plotrix" ,r-plotrix)))
    (home-page "https://github.com/kornl/mutoss/")
    (synopsis "Unified multiple testing procedures")
    (description
     "This package is designed to ease the application and comparison of
multiple hypothesis testing procedures for FWER, gFWER, FDR and FDX.  Methods
are standardized and usable by the accompanying mutossGUI package.")
    ;; Any version of the GPL.
    (license (list license:gpl2+ license:gpl3+))))

;; This is a CRAN package, but it depends on mutoss, which depends on multtest
;; from Bioconductor, so we put it here.
(define-public r-metap
  (package
    (name "r-metap")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "metap" version))
       (sha256
        (base32
         "1jmmmmjiklaxfl604hwqil193ydaghvd5jv8xsr4bx3pzn5i9kvz"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-mutoss" ,r-mutoss)
       ("r-rdpack" ,r-rdpack)
       ("r-tfisher" ,r-tfisher)))
    (home-page "http://www.dewey.myzen.co.uk/meta/meta.html")
    (synopsis "Meta-analysis of significance values")
    (description
     "The canonical way to perform meta-analysis involves using effect sizes.
When they are not available this package provides a number of methods for
meta-analysis of significance values including the methods of Edgington,
Fisher, Stouffer, Tippett, and Wilkinson; a number of data-sets to replicate
published results; and a routine for graphical display.")
    (license license:gpl2)))

(define-public r-triform
  (package
    (name "r-triform")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "triform" version))
       (sha256
        (base32
         "12qdyrcw2mcawqdw65v0hjaghzlwa10xl6j8458izcrm5k6dlvz9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-iranges" ,r-iranges)
       ("r-yaml" ,r-yaml)))
    (home-page "https://bioconductor.org/packages/triform/")
    (synopsis "Find enriched regions in transcription factor ChIP-sequencing data")
    (description
     "The Triform algorithm uses model-free statistics to identify peak-like
distributions of TF ChIP sequencing reads, taking advantage of an improved
peak definition in combination with known profile characteristics.")
    (license license:gpl2)))

(define-public r-varianttools
  (package
    (name "r-varianttools")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantTools" version))
       (sha256
        (base32
         "0aafcprsqbn1xl8jqnxfl8r2d0lmzhssqpr26lam2cprh3yk0xiv"))))
    (properties `((upstream-name . "VariantTools")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://bioconductor.org/packages/VariantTools/")
    (synopsis "Tools for exploratory analysis of variant calls")
    (description
     "Explore, diagnose, and compare variant calls using filters.  The
VariantTools package supports a workflow for loading data, calling single
sample variants and tumor-specific somatic mutations or other sample-specific
variant types (e.g., RNA editing).  Most of the functions operate on
alignments (BAM files) or datasets of called variants.  The user is expected
to have already aligned the reads with a separate tool, e.g., GSNAP via
gmapR.")
    (license license:artistic2.0)))

(define-public r-heatplus
  (package
    (name "r-heatplus")
    (version "2.32.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Heatplus" version))
       (sha256
        (base32
         "1ldxj2hasg6ysh70sc73mz2v4h3n8gf3lii09c3sqc4wmz5h7rp1"))))
    (properties `((upstream-name . "Heatplus")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page "https://github.com/alexploner/Heatplus")
    (synopsis "Heatmaps with row and/or column covariates and colored clusters")
    (description
     "This package provides tools to display a rectangular heatmap (intensity
plot) of a data matrix.  By default, both samples (columns) and features (row)
of the matrix are sorted according to a hierarchical clustering, and the
corresponding dendrogram is plotted.  Optionally, panels with additional
information about samples and features can be added to the plot.")
    (license license:gpl2+)))

(define-public r-gosemsim
  (package
    (name "r-gosemsim")
    (version "2.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOSemSim" version))
       (sha256
        (base32
         "1r8yx6qw6d6602cp8aspzl3shi1l1zqcrc8fm9d5wg01sw1whs05"))))
    (properties `((upstream-name . "GOSemSim")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-go-db" ,r-go-db)
       ("r-rcpp" ,r-rcpp)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://guangchuangyu.github.io/software/GOSemSim")
    (synopsis "GO-terms semantic similarity measures")
    (description
     "The semantic comparisons of @dfn{Gene Ontology} (GO) annotations provide
quantitative ways to compute similarities between genes and gene groups, and
have became important basis for many bioinformatics analysis approaches.
GOSemSim is an R package for semantic similarity computation among GO terms,
sets of GO terms, gene products and gene clusters.")
    (license license:artistic2.0)))

(define-public r-anota
  (package
    (name "r-anota")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "anota" version))
       (sha256
        (base32
         "1bkavzrxy1bjz0bwapwa9i3ysln7gljgziwd8c05fmg2f46j1z6m"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-multtest" ,r-multtest)
       ("r-qvalue" ,r-qvalue)))
    (home-page "https://bioconductor.org/packages/anota/")
    (synopsis "Analysis of translational activity")
    (description
     "Genome wide studies of translational control is emerging as a tool to
study various biological conditions.  The output from such analysis is both
the mRNA level (e.g. cytosolic mRNA level) and the levl of mRNA actively
involved in translation (the actively translating mRNA level) for each mRNA.
The standard analysis of such data strives towards identifying differential
translational between two or more sample classes - i.e.  differences in
actively translated mRNA levels that are independent of underlying differences
in cytosolic mRNA levels.  This package allows for such analysis using partial
variances and the random variance model.  As 10s of thousands of mRNAs are
analyzed in parallel the library performs a number of tests to assure that
the data set is suitable for such analysis.")
    (license license:gpl3)))

(define-public r-sigpathway
  (package
    (name "r-sigpathway")
    (version "1.54.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "sigPathway" version))
        (sha256
          (base32
            "057qdkbfldpy6hsysk9mrs1vvsqgyl9yx2s6c2f26jz1pardkbqb"))))
    (properties `((upstream-name . "sigPathway")))
    (build-system r-build-system)
    (home-page "https://www.pnas.org/cgi/doi/10.1073/pnas.0506577102")
    (synopsis "Pathway analysis")
    (description
     "This package is used to conduct pathway analysis by calculating the NT_k
and NE_k statistics in a statistical framework for determining whether a
specified group of genes for a pathway has a coordinated association with a
phenotype of interest.")
    (license license:gpl2)))

(define-public r-fgsea
  (package
    (name "r-fgsea")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fgsea" version))
       (sha256
        (base32
         "0pbq3g515kcbi4wvfx8m09p01h2f8vwsi1qqsyahxz4r1fasz4c1"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-biocparallel" ,r-biocparallel)
       ("r-data-table" ,r-data-table)
       ("r-fastmatch" ,r-fastmatch)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/ctlab/fgsea/")
    (synopsis "Fast gene set enrichment analysis")
    (description
     "The package implements an algorithm for fast gene set enrichment
analysis.  Using the fast algorithm allows to make more permutations and get
more fine grained p-values, which allows to use accurate stantard approaches
to multiple hypothesis correction.")
    (license license:expat)))

(define-public r-dose
  (package
    (name "r-dose")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DOSE" version))
       (sha256
        (base32
         "0ss8mr28q0vswxjmhcas0aqag5nl099jbyn5w3ypbbxqwfvf9jj5"))))
    (properties `((upstream-name . "DOSE")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocparallel" ,r-biocparallel)
       ("r-do-db" ,r-do-db)
       ("r-fgsea" ,r-fgsea)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gosemsim" ,r-gosemsim)
       ("r-qvalue" ,r-qvalue)
       ("r-reshape2" ,r-reshape2)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://guangchuangyu.github.io/software/DOSE/")
    (synopsis "Disease ontology semantic and enrichment analysis")
    (description
     "This package implements five methods proposed by Resnik, Schlicker,
Jiang, Lin and Wang, respectively, for measuring semantic similarities among
@dfn{Disease ontology} (DO) terms and gene products.  Enrichment analyses
including hypergeometric model and gene set enrichment analysis are also
implemented for discovering disease associations of high-throughput biological
data.")
    (license license:artistic2.0)))

(define-public r-enrichplot
  (package
    (name "r-enrichplot")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "enrichplot" version))
       (sha256
        (base32
         "0707f5ll58psh7pr001cmmk5di7dprnbry1cy2mw20vn8p24nf3x"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-cowplot" ,r-cowplot)
       ("r-dose" ,r-dose)
       ("r-europepmc" ,r-europepmc)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggplotify" ,r-ggplotify)
       ("r-ggraph" ,r-ggraph)
       ("r-ggridges" ,r-ggridges)
       ("r-gosemsim" ,r-gosemsim)
       ("r-gridextra" ,r-gridextra)
       ("r-igraph" ,r-igraph)
       ("r-purrr" ,r-purrr)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-reshape2" ,r-reshape2)))
    (home-page "https://github.com/GuangchuangYu/enrichplot")
    (synopsis "Visualization of functional enrichment result")
    (description
     "The enrichplot package implements several visualization methods for
interpreting functional enrichment results obtained from ORA or GSEA analyses.
All the visualization methods are developed based on ggplot2 graphics.")
    (license license:artistic2.0)))

(define-public r-clusterprofiler
  (package
    (name "r-clusterprofiler")
    (version "3.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "clusterProfiler" version))
       (sha256
        (base32
         "08pd7bmqmyxncj09ilz8yb9sf1pv9ni98y8b93pz2giy7pl407hg"))))
    (properties
     `((upstream-name . "clusterProfiler")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-dose" ,r-dose)
       ("r-enrichplot" ,r-enrichplot)
       ("r-ggplot2" ,r-ggplot2)
       ("r-go-db" ,r-go-db)
       ("r-gosemsim" ,r-gosemsim)
       ("r-magrittr" ,r-magrittr)
       ("r-plyr" ,r-plyr)
       ("r-qvalue" ,r-qvalue)
       ("r-rvcheck" ,r-rvcheck)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://guangchuangyu.github.io/software/clusterProfiler/")
    (synopsis "Analysis and visualization of functional profiles for gene clusters")
    (description
     "This package implements methods to analyze and visualize functional
profiles (GO and KEGG) of gene and gene clusters.")
    (license license:artistic2.0)))

(define-public r-mlinterfaces
  (package
    (name "r-mlinterfaces")
    (version "1.66.5")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MLInterfaces" version))
       (sha256
        (base32
         "05bg7qcrv485m03rkyq3qg5hrr1m3y7zx49bipwaivzqm3s1mbw5"))))
    (properties `((upstream-name . "MLInterfaces")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-cluster" ,r-cluster)
       ("r-fpc" ,r-fpc)
       ("r-gbm" ,r-gbm)
       ("r-gdata" ,r-gdata)
       ("r-genefilter" ,r-genefilter)
       ("r-ggvis" ,r-ggvis)
       ("r-hwriter" ,r-hwriter)
       ("r-mass" ,r-mass)
       ("r-mlbench" ,r-mlbench)
       ("r-pls" ,r-pls)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rpart" ,r-rpart)
       ("r-sfsmisc" ,r-sfsmisc)
       ("r-shiny" ,r-shiny)
       ("r-threejs" ,r-threejs)))
    (home-page "https://bioconductor.org/packages/MLInterfaces/")
    (synopsis "Interfaces to R machine learning procedures")
    (description
     "This package provides uniform interfaces to machine learning code for
data in R and Bioconductor containers.")
    ;; Any version of the LGPL.
    (license license:lgpl2.1+)))

(define-public r-annaffy
  (package
    (name "r-annaffy")
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annaffy" version))
       (sha256
        (base32
         "1jrf4bq2wky4ay1jrcy60si6hxdcn4j05w5vgs38yfb92gq77i16"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-reference-to-non-free-data
           (lambda _
             (substitute* "DESCRIPTION"
               ((", KEGG.db") ""))
             #t)))))
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-dbi" ,r-dbi)
       ("r-go-db" ,r-go-db)))
    (home-page "https://bioconductor.org/packages/annaffy/")
    (synopsis "Annotation tools for Affymetrix biological metadata")
    (description
     "This package provides functions for handling data from Bioconductor
Affymetrix annotation data packages.  It produces compact HTML and text
reports including experimental data and URL links to many online databases.
It allows searching of biological metadata using various criteria.")
    ;; Any version of the LGPL according to the DESCRIPTION file.  A copy of
    ;; the LGPL 2.1 is included.
    (license license:lgpl2.1+)))

(define-public r-a4core
  (package
    (name "r-a4core")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Core" version))
       (sha256
        (base32
         "0xcs9wl2yrprl4yc0z5s4zrkil3752k9xc1fi8fcx7zab1mm80df"))))
    (properties `((upstream-name . "a4Core")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-glmnet" ,r-glmnet)))
    (home-page "https://bioconductor.org/packages/a4Core")
    (synopsis "Automated Affymetrix array analysis core package")
    (description
     "This is the core package for the automated analysis of Affymetrix
arrays.")
    (license license:gpl3)))

(define-public r-a4classif
  (package
    (name "r-a4classif")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Classif" version))
       (sha256
        (base32
         "0gj3hdflfs1ybc2kshl9w1dzy0rfzppfj08hx3wa2chjsm0m9brn"))))
    (properties `((upstream-name . "a4Classif")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-a4core" ,r-a4core)
       ("r-a4preproc" ,r-a4preproc)
       ("r-glmnet" ,r-glmnet)
       ("r-mlinterfaces" ,r-mlinterfaces)
       ("r-pamr" ,r-pamr)
       ("r-rocr" ,r-rocr)
       ("r-varselrf" ,r-varselrf)))
    (home-page "https://bioconductor.org/packages/a4Classif/")
    (synopsis "Automated Affymetrix array analysis classification package")
    (description
     "This is the classification package for the automated analysis of
Affymetrix arrays.")
    (license license:gpl3)))

(define-public r-a4preproc
  (package
    (name "r-a4preproc")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Preproc" version))
       (sha256
        (base32
         "11j5nc33gd6yis1fyagr0y39g21bzkc59kq8b8sd6b3pfc84zrjd"))))
    (properties `((upstream-name . "a4Preproc")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://bioconductor.org/packages/a4Preproc/")
    (synopsis "Automated Affymetrix array analysis preprocessing package")
    (description
     "This is a package for the automated analysis of Affymetrix arrays.  It
is used for preprocessing the arrays.")
    (license license:gpl3)))

(define-public r-a4reporting
  (package
    (name "r-a4reporting")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Reporting" version))
       (sha256
        (base32
         "15nd4pa5hkdzkhinvqw5ijdqb7k5gk37v2hmk3jsg2d6m0jqphi5"))))
    (properties `((upstream-name . "a4Reporting")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annaffy" ,r-annaffy)
       ("r-xtable" ,r-xtable)))
    (home-page "https://bioconductor.org/packages/a4Reporting/")
    (synopsis "Automated Affymetrix array analysis reporting package")
    (description
     "This is a package for the automated analysis of Affymetrix arrays.  It
provides reporting features.")
    (license license:gpl3)))

(define-public r-a4base
  (package
    (name "r-a4base")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Base" version))
       (sha256
        (base32
         "0dgqyq4dnlcik5qqygnhxlhfr98sh6kmdcj2qllhrwyk0lmsfk01"))))
    (properties `((upstream-name . "a4Base")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-a4core" ,r-a4core)
       ("r-a4preproc" ,r-a4preproc)
       ("r-annaffy" ,r-annaffy)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-genefilter" ,r-genefilter)
       ("r-glmnet" ,r-glmnet)
       ("r-gplots" ,r-gplots)
       ("r-limma" ,r-limma)
       ("r-mpm" ,r-mpm)
       ("r-multtest" ,r-multtest)))
    (home-page "https://bioconductor.org/packages/a4Base/")
    (synopsis "Automated Affymetrix array analysis base package")
    (description
     "This package provides basic features for the automated analysis of
Affymetrix arrays.")
    (license license:gpl3)))

(define-public r-a4
  (package
    (name "r-a4")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4" version))
       (sha256
        (base32
         "14yipy6s2wqr9q0yp09x1mm17npknrhs6yd34i3wrb5id64ywnq4"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-a4base" ,r-a4base)
       ("r-a4classif" ,r-a4classif)
       ("r-a4core" ,r-a4core)
       ("r-a4preproc" ,r-a4preproc)
       ("r-a4reporting" ,r-a4reporting)))
    (home-page "https://bioconductor.org/packages/a4/")
    (synopsis "Automated Affymetrix array analysis umbrella package")
    (description
     "This package provides a software suite for the automated analysis of
Affymetrix arrays.")
    (license license:gpl3)))

(define-public r-abseqr
  (package
    (name "r-abseqr")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "abseqR" version))
       (sha256
        (base32
         "1n9h5qkj4njr1f8fvhg9sj9wxcd7hljnnk8m80zwswzs2n9ivppa"))))
    (properties `((upstream-name . "abseqR")))
    (build-system r-build-system)
    (inputs
     `(("pandoc" ,ghc-pandoc)))
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-biocstyle" ,r-biocstyle)
       ("r-circlize" ,r-circlize)
       ("r-flexdashboard" ,r-flexdashboard)
       ("r-ggcorrplot" ,r-ggcorrplot)
       ("r-ggdendro" ,r-ggdendro)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-knitr" ,r-knitr)
       ("r-plotly" ,r-plotly)
       ("r-plyr" ,r-plyr)
       ("r-png" ,r-png)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-reshape2" ,r-reshape2)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-stringr" ,r-stringr)
       ("r-vegan" ,r-vegan)
       ("r-venndiagram" ,r-venndiagram)))
    (home-page "https://github.com/malhamdoosh/abseqR")
    (synopsis "Reporting and data analysis for Rep-Seq datasets of antibody libraries")
    (description
     "AbSeq is a comprehensive bioinformatic pipeline for the analysis of
sequencing datasets generated from antibody libraries and abseqR is one of its
packages.  AbseqR empowers the users of abseqPy with plotting and reporting
capabilities and allows them to generate interactive HTML reports for the
convenience of viewing and sharing with other researchers.  Additionally,
abseqR extends abseqPy to compare multiple repertoire analyses and perform
further downstream analysis on its output.")
    (license license:gpl3)))

(define-public r-bacon
  (package
    (name "r-bacon")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bacon" version))
       (sha256
        (base32
         "1q18vm4znl47v56cnvx9y5ygrial2mdjpl8x1043jq00kyygrc86"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-ellipse" ,r-ellipse)
       ("r-ggplot2" ,r-ggplot2)))
    (home-page "https://bioconductor.org/packages/bacon/")
    (synopsis "Controlling bias and inflation in association studies")
    (description
     "Bacon can be used to remove inflation and bias often observed in
epigenome- and transcriptome-wide association studies.  To this end bacon
constructs an empirical null distribution using a Gibbs Sampling algorithm by
fitting a three-component normal mixture on z-scores.")
    (license license:gpl2+)))

(define-public r-rgadem
  (package
    (name "r-rgadem")
    (version "2.34.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rGADEM" version))
       (sha256
        (base32
         "12xm4p4qsczf57kip8bvi6pr8sb5gvn11dnbz7lbh6sc03sx3q2h"))))
    (properties `((upstream-name . "rGADEM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-seqlogo" ,r-seqlogo)))
    (home-page "https://bioconductor.org/packages/rGADEM/")
    (synopsis "De novo sequence motif discovery")
    (description
     "rGADEM is an efficient de novo motif discovery tool for large-scale
genomic sequence data.")
    (license license:artistic2.0)))

(define-public r-motiv
  (package
    (name "r-motiv")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MotIV" version))
       (sha256
        (base32
         "07k4rw4nhcn4sg43psv1h7qr064gws22m2yyr7x8sy3f1i1c954k"))))
    (properties `((upstream-name . "MotIV")))
    (build-system r-build-system)
    (inputs
     `(("gsl" ,gsl)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-rgadem" ,r-rgadem)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/MotIV/")
    (synopsis "Motif identification and validation")
    (description
     "This package is used for the identification and validation of sequence
motifs.  It makes use of STAMP for comparing a set of motifs to a given
database (e.g. JASPAR).  It can also be used to visualize motifs, motif
distributions, modules and filter motifs.")
    (license license:gpl2)))

(define-public r-motifstack
  (package
    (name "r-motifstack")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifStack" version))
       (sha256
        (base32
         "00rafqs1gqlcxlbsdn9qnq9xb7wjphiksb3hsx76viqjbjzi14wg"))))
    (properties `((upstream-name . "motifStack")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ade4" ,r-ade4)
       ("r-biostrings" ,r-biostrings)
       ("r-ggplot2" ,r-ggplot2)
       ("r-grimport2" ,r-grimport2)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-motiv" ,r-motiv)
       ("r-scales" ,r-scales)
       ("r-xml" ,r-xml)))
    (home-page "https://bioconductor.org/packages/motifStack/")
    (synopsis "Plot stacked logos for DNA, RNA and amino acid sequences")
    (description
     "The motifStack package is designed for graphic representation of
multiple motifs with different similarity scores.  It works with both DNA/RNA
sequence motifs and amino acid sequence motifs.  In addition, it provides the
flexibility for users to customize the graphic parameters such as the font
type and symbol colors.")
    (license license:gpl2+)))

(define-public r-genomicscores
  (package
    (name "r-genomicscores")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicScores" version))
       (sha256
        (base32
         "175iaqv7npa11yw48vmqpgx0qqs3g44c3dsya7ccwd1lg97fznkj"))))
    (properties `((upstream-name . "GenomicScores")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationhub" ,r-annotationhub)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xml" ,r-xml)))
    (home-page "https://github.com/rcastelo/GenomicScores/")
    (synopsis "Work with genome-wide position-specific scores")
    (description
     "This package provides infrastructure to store and access genome-wide
position-specific scores within R and Bioconductor.")
    (license license:artistic2.0)))

(define-public r-atacseqqc
  (package
    (name "r-atacseqqc")
    (version "1.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ATACseqQC" version))
       (sha256
        (base32
         "18zf90iksglbs13cwr4jjwsv332a19lf4bpdmy69jz8bpwrklv22"))))
    (properties `((upstream-name . "ATACseqQC")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-chippeakanno" ,r-chippeakanno)
       ("r-edger" ,r-edger)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-genomicscores" ,r-genomicscores)
       ("r-iranges" ,r-iranges)
       ("r-kernsmooth" ,r-kernsmooth)
       ("r-limma" ,r-limma)
       ("r-motifstack" ,r-motifstack)
       ("r-preseqr" ,r-preseqr)
       ("r-randomforest" ,r-randomforest)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/ATACseqQC/")
    (synopsis "ATAC-seq quality control")
    (description
     "ATAC-seq, an assay for Transposase-Accessible Chromatin using
sequencing, is a rapid and sensitive method for chromatin accessibility
analysis.  It was developed as an alternative method to MNase-seq, FAIRE-seq
and DNAse-seq.  The ATACseqQC package was developed to help users to quickly
assess whether their ATAC-seq experiment is successful.  It includes
diagnostic plots of fragment size distribution, proportion of mitochondria
reads, nucleosome positioning pattern, and CTCF or other Transcript Factor
footprints.")
    (license license:gpl2+)))

(define-public r-gofuncr
  (package
    (name "r-gofuncr")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOfuncR" version))
       (sha256
        (base32
         "0hmi13pz923fm95asys615rih63b1i2nvynfczr1zcsc9fzn4h35"))))
    (properties `((upstream-name . "GOfuncR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-genomicranges" ,r-genomicranges)
       ("r-gtools" ,r-gtools)
       ("r-iranges" ,r-iranges)
       ("r-mapplots" ,r-mapplots)
       ("r-rcpp" ,r-rcpp)
       ("r-vioplot" ,r-vioplot)))
    (home-page "https://bioconductor.org/packages/GOfuncR/")
    (synopsis "Gene ontology enrichment using FUNC")
    (description
     "GOfuncR performs a gene ontology enrichment analysis based on the
ontology enrichment software FUNC.  GO-annotations are obtained from
OrganismDb or OrgDb packages (@code{Homo.sapiens} by default); the GO-graph is
included in the package and updated regularly.  GOfuncR provides the standard
candidate vs background enrichment analysis using the hypergeometric test, as
well as three additional tests:

@enumerate
@item the Wilcoxon rank-sum test that is used when genes are ranked,
@item a binomial test that is used when genes are associated with two counts,
  and
@item a Chi-square or Fisher's exact test that is used in cases when genes are
associated with four counts.
@end enumerate

To correct for multiple testing and interdependency of the tests, family-wise
error rates are computed based on random permutations of the gene-associated
variables.  GOfuncR also provides tools for exploring the ontology graph and
the annotations, and options to take gene-length or spatial clustering of
genes into account.  It is also possible to provide custom gene coordinates,
annotations and ontologies.")
    (license license:gpl2+)))

(define-public r-abaenrichment
  (package
    (name "r-abaenrichment")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ABAEnrichment" version))
       (sha256
        (base32
         "128ik28j8kmdkycffhxajv5h174zdq9sfn2gz6ai90wgkdadbzwp"))))
    (properties `((upstream-name . "ABAEnrichment")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abadata" ,r-abadata)
       ("r-data-table" ,r-data-table)
       ("r-gofuncr" ,r-gofuncr)
       ("r-gplots" ,r-gplots)
       ("r-gtools" ,r-gtools)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://bioconductor.org/packages/ABAEnrichment/")
    (synopsis "Gene expression enrichment in human brain regions")
    (description
     "The package ABAEnrichment is designed to test for enrichment of user
defined candidate genes in the set of expressed genes in different human brain
regions.  The core function @code{aba_enrich} integrates the expression of the
candidate gene set (averaged across donors) and the structural information of
the brain using an ontology, both provided by the Allen Brain Atlas project.")
    (license license:gpl2+)))

(define-public r-annotationfuncs
  (package
    (name "r-annotationfuncs")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationFuncs" version))
       (sha256
        (base32
         "0ws14b1ibqml7w8kj0gi7wdp6wd8dcdpnrmcxldjzsyawf17q0yq"))))
    (properties
     `((upstream-name . "AnnotationFuncs")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-dbi" ,r-dbi)))
    (home-page "https://www.iysik.com/r/annotationfuncs")
    (synopsis "Annotation translation functions")
    (description
     "This package provides functions for handling translating between
different identifieres using the Biocore Data Team data-packages (e.g.
@code{org.Bt.eg.db}).")
    (license license:gpl2)))

(define-public r-annotationtools
  (package
    (name "r-annotationtools")
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotationTools" version))
       (sha256
        (base32
         "1cq7ayq2swp2ahlphz74nh5mb869rgyyr0kkqy1mxyilk8k2g44i"))))
    (properties
     `((upstream-name . "annotationTools")))
    (build-system r-build-system)
    (propagated-inputs `(("r-biobase" ,r-biobase)))
    (home-page "https://bioconductor.org/packages/annotationTools/")
    (synopsis "Annotate microarrays and perform gene expression analyses")
    (description
     "This package provides functions to annotate microarrays, find orthologs,
and integrate heterogeneous gene expression profiles using annotation and
other molecular biology information available as flat file database (plain
text files).")
    ;; Any version of the GPL.
    (license (list license:gpl2+))))

(define-public r-allelicimbalance
  (package
    (name "r-allelicimbalance")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AllelicImbalance" version))
       (sha256
        (base32
         "0vy8w7ii2qljsmq8lr21lygkcrsshc7syyqhadlbxyj3scgi7kyc"))))
    (properties
     `((upstream-name . "AllelicImbalance")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-gridextra" ,r-gridextra)
       ("r-gviz" ,r-gviz)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-latticeextra" ,r-latticeextra)
       ("r-nlme" ,r-nlme)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-seqinr" ,r-seqinr)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://github.com/pappewaio/AllelicImbalance")
    (synopsis "Investigate allele-specific expression")
    (description
     "This package provides a framework for allele-specific expression
investigation using RNA-seq data.")
    (license license:gpl3)))

(define-public r-aucell
  (package
    (name "r-aucell")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AUCell" version))
       (sha256
        (base32
         "1g4mdq8z29fjxrfjng0fb3cvbph49mwds4ijsa2bn2k6f75dnzky"))))
    (properties `((upstream-name . "AUCell")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-data-table" ,r-data-table)
       ("r-gseabase" ,r-gseabase)
       ("r-mixtools" ,r-mixtools)
       ("r-r-utils" ,r-r-utils)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shiny" ,r-shiny)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/AUCell/")
    (synopsis "Analysis of gene set activity in single-cell RNA-seq data")
    (description
     "AUCell allows to identify cells with active gene sets (e.g. signatures,
gene modules, etc) in single-cell RNA-seq data.  AUCell uses the @dfn{Area
Under the Curve} (AUC) to calculate whether a critical subset of the input
gene set is enriched within the expressed genes for each cell.  The
distribution of AUC scores across all the cells allows exploring the relative
expression of the signature.  Since the scoring method is ranking-based,
AUCell is independent of the gene expression units and the normalization
procedure.  In addition, since the cells are evaluated individually, it can
easily be applied to bigger datasets, subsetting the expression matrix if
needed.")
    (license license:gpl3)))

(define-public r-ebimage
  (package
    (name "r-ebimage")
    (version "4.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBImage" version))
       (sha256
        (base32
         "0q54q2nw6p1s4nrj1l8qz78m67xcysj2x82zs43my8iv2g9iifgs"))))
    (properties `((upstream-name . "EBImage")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-fftwtools" ,r-fftwtools)
       ("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-jpeg" ,r-jpeg)
       ("r-locfit" ,r-locfit)
       ("r-png" ,r-png)
       ("r-rcurl" ,r-rcurl)
       ("r-tiff" ,r-tiff)))
    (native-inputs
     `(("r-knitr" ,r-knitr))) ; for vignettes
    (home-page "https://github.com/aoles/EBImage")
    (synopsis "Image processing and analysis toolbox for R")
    (description
     "EBImage provides general purpose functionality for image processing and
analysis.  In the context of (high-throughput) microscopy-based cellular
assays, EBImage offers tools to segment cells and extract quantitative
cellular descriptors.  This allows the automation of such tasks using the R
programming language and facilitates the use of other tools in the R
environment for signal processing, statistical modeling, machine learning and
visualization with image data.")
    ;; Any version of the LGPL.
    (license license:lgpl2.1+)))

(define-public r-yamss
  (package
    (name "r-yamss")
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yamss" version))
       (sha256
        (base32
         "12jr7hbrwhb1gfjadj1024hv80ra22miy46dn40nmsrbklkfn3rw"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-data-table" ,r-data-table)
       ("r-ebimage" ,r-ebimage)
       ("r-iranges" ,r-iranges)
       ("r-limma" ,r-limma)
       ("r-matrix" ,r-matrix)
       ("r-mzr" ,r-mzr)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment"
        ,r-summarizedexperiment)))
    (home-page "https://github.com/hansenlab/yamss")
    (synopsis "Tools for high-throughput metabolomics")
    (description
     "This package provides tools to analyze and visualize high-throughput
metabolomics data acquired using chromatography-mass spectrometry.  These tools
preprocess data in a way that enables reliable and powerful differential
analysis.")
    (license license:artistic2.0)))

(define-public r-gtrellis
  (package
    (name "r-gtrellis")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gtrellis" version))
       (sha256
        (base32
         "0mgspmv6p1a2k98jyy2dfl0wpa2vh7bhnjfm2xaqmcsxzmbjhh9z"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-circlize" ,r-circlize)
       ("r-genomicranges" ,r-genomicranges)
       ("r-getoptlong" ,r-getoptlong)
       ("r-iranges" ,r-iranges)))
    (home-page "https://github.com/jokergoo/gtrellis")
    (synopsis "Genome level Trellis layout")
    (description
     "Genome level Trellis graph visualizes genomic data conditioned by
genomic categories (e.g. chromosomes).  For each genomic category, multiple
dimensional data which are represented as tracks describe different features
from different aspects.  This package provides high flexibility to arrange
genomic categories and to add self-defined graphics in the plot.")
    (license license:expat)))

(define-public r-somaticsignatures
  (package
    (name "r-somaticsignatures")
    (version "2.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SomaticSignatures" version))
       (sha256
        (base32
         "1gvrkahllwz38g3hn9pjlikhfqz19a4qglcqmyrxk7h9ybx5zy5z"))))
    (properties
     `((upstream-name . "SomaticSignatures")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggbio" ,r-ggbio)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iranges" ,r-iranges)
       ("r-nmf" ,r-nmf)
       ("r-pcamethods" ,r-pcamethods)
       ("r-proxy" ,r-proxy)
       ("r-reshape2" ,r-reshape2)
       ("r-s4vectors" ,r-s4vectors)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://github.com/juliangehring/SomaticSignatures")
    (synopsis "Somatic signatures")
    (description
     "This package identifies mutational signatures of @dfn{single nucleotide
variants} (SNVs).  It provides a infrastructure related to the methodology
described in Nik-Zainal (2012, Cell), with flexibility in the matrix
decomposition algorithms.")
    (license license:expat)))

(define-public r-yapsa
  (package
    (name "r-yapsa")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "YAPSA" version))
       (sha256
        (base32
         "132x51f8k8zyx6j8jk05x4lr9q1hlblgvr69wkhn0q3f8mhaj926"))))
    (properties `((upstream-name . "YAPSA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-circlize" ,r-circlize)
       ("r-complexheatmap" ,r-complexheatmap)
       ("r-corrplot" ,r-corrplot)
       ("r-dendextend" ,r-dendextend)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-getoptlong" ,r-getoptlong)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-gtrellis" ,r-gtrellis)
       ("r-keggrest" ,r-keggrest)
       ("r-lsei" ,r-lsei)
       ("r-pmcmr" ,r-pmcmr)
       ("r-reshape2" ,r-reshape2)
       ("r-somaticsignatures" ,r-somaticsignatures)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://bioconductor.org/packages/YAPSA/")
    (synopsis "Yet another package for signature analysis")
    (description
     "This package provides functions and routines useful in the analysis of
somatic signatures (cf. L. Alexandrov et al., Nature 2013).  In particular,
functions to perform a signature analysis with known signatures and a
signature analysis on @dfn{stratified mutational catalogue} (SMC) are
provided.")
    (license license:gpl3)))

(define-public r-gcrma
  (package
    (name "r-gcrma")
    (version "2.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gcrma" version))
       (sha256
        (base32
         "0c9sa9ldlcpdcjdan8m4ndnyaisr6wbarq486sl44ikh7wf1csfx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-affyio" ,r-affyio)
       ("r-biobase" ,r-biobase)
       ("r-biocmanager" ,r-biocmanager)
       ("r-biostrings" ,r-biostrings)
       ("r-xvector" ,r-xvector)))
    (home-page "https://bioconductor.org/packages/gcrma/")
    (synopsis "Background adjustment using sequence information")
    (description
     "Gcrma adjusts for background intensities in Affymetrix array data which
include optical noise and @dfn{non-specific binding} (NSB).  The main function
@code{gcrma} converts background adjusted probe intensities to expression
measures using the same normalization and summarization methods as a
@dfn{Robust Multiarray Average} (RMA).  Gcrma uses probe sequence information
to estimate probe affinity to NSB.  The sequence information is summarized in
a more complex way than the simple GC content.  Instead, the base types (A, T,
G or C) at each position along the probe determine the affinity of each probe.
The parameters of the position-specific base contributions to the probe
affinity is estimated in an NSB experiment in which only NSB but no
gene-specific bidning is expected.")
    ;; Any version of the LGPL
    (license license:lgpl2.1+)))

(define-public r-simpleaffy
  (package
    (name "r-simpleaffy")
    (version "2.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "simpleaffy" version))
       (sha256
        (base32
         "18bz3pfgp0f7906flrljx97ann8s71pnb8gpw7nah46n8vqc0xcs"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-gcrma" ,r-gcrma)
       ("r-genefilter" ,r-genefilter)))
    (home-page "https://bioconductor.org/packages/simpleaffy/")
    (synopsis "Very simple high level analysis of Affymetrix data")
    (description
     "This package provides high level functions for reading Affy @file{.CEL}
files, phenotypic data, and then computing simple things with it, such as
t-tests, fold changes and the like.  It makes heavy use of the @code{affy}
library.  It also has some basic scatter plot functions and mechanisms for
generating high resolution journal figures.")
    (license license:gpl2+)))

(define-public r-yaqcaffy
  (package
    (name "r-yaqcaffy")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yaqcaffy" version))
       (sha256
        (base32
         "0kzzqsf1lfbcmy95w2z0c9qrvp7mbwm50k2l9wvz3xa5wz6xa7gz"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-simpleaffy" ,r-simpleaffy)))
    (home-page "https://bioconductor.org/packages/yaqcaffy/")
    (synopsis "Affymetrix quality control and reproducibility analysis")
    (description
     "This is a package that can be used for quality control of Affymetrix
GeneChip expression data and reproducibility analysis of human whole genome
chips with the MAQC reference datasets.")
    (license license:artistic2.0)))

(define-public r-quantro
  (package
    (name "r-quantro")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "quantro" version))
       (sha256
        (base32
         "09f3x1j50ll5hhn3qwys5x06mii3fqsrk6dkvsxai0kdxv9cjz9m"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-doparallel" ,r-doparallel)
       ("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iterators" ,r-iterators)
       ("r-minfi" ,r-minfi)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page "https://bioconductor.org/packages/quantro/")
    (synopsis "Test for when to use quantile normalization")
    (description
     "This package provides a data-driven test for the assumptions of quantile
normalization using raw data such as objects that inherit eSets (e.g.
ExpressionSet, MethylSet).  Group level information about each sample (such as
Tumor / Normal status) must also be provided because the test assesses if
there are global differences in the distributions between the user-defined
groups.")
    (license license:gpl3+)))

(define-public r-yarn
  (package
    (name "r-yarn")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yarn" version))
       (sha256
        (base32
         "0891a10adkhm1zpm7fpcxc2xfxjf9yrpckaz87b2wdjdiwivc4cp"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biomart" ,r-biomart)
       ("r-downloader" ,r-downloader)
       ("r-edger" ,r-edger)
       ("r-gplots" ,r-gplots)
       ("r-limma" ,r-limma)
       ("r-matrixstats" ,r-matrixstats)
       ("r-preprocesscore" ,r-preprocesscore)
       ("r-quantro" ,r-quantro)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-readr" ,r-readr)))
    (home-page "https://bioconductor.org/packages/yarn/")
    (synopsis "Robust multi-condition RNA-Seq preprocessing and normalization")
    (description
     "Expedite large RNA-Seq analyses using a combination of previously
developed tools.  YARN is meant to make it easier for the user in performing
basic mis-annotation quality control, filtering, and condition-aware
normalization.  YARN leverages many Bioconductor tools and statistical
techniques to account for the large heterogeneity and sparsity found in very
large RNA-seq experiments.")
    (license license:artistic2.0)))

(define-public r-roar
  (package
    (name "r-roar")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "roar" version))
       (sha256
        (base32
         "1nqw0agx9x8ycdf0gw17fdlnmzpw9x3zig1wcy10xpyhyjdbbi06"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://github.com/vodkatad/roar/")
    (synopsis "Identify differential APA usage from RNA-seq alignments")
    (description
     "This package provides tools for identifying preferential usage of APA
sites, comparing two biological conditions, starting from known alternative
sites and alignments obtained from standard RNA-seq experiments.")
    (license license:gpl3)))

(define-public r-xbseq
  (package
    (name "r-xbseq")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "XBSeq" version))
       (sha256
        (base32
         "1qr5gvf8jcx6r0ac7d2wmnikswmp3k71lirnw7dyr6fndzrdz9lp"))))
    (properties `((upstream-name . "XBSeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-deseq2" ,r-deseq2)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-locfit" ,r-locfit)
       ("r-magrittr" ,r-magrittr)
       ("r-matrixstats" ,r-matrixstats)
       ("r-pracma" ,r-pracma)
       ("r-roar" ,r-roar)))
    (home-page "https://github.com/Liuy12/XBSeq")
    (synopsis "Test for differential expression for RNA-seq data")
    (description
     "XBSeq is a novel algorithm for testing RNA-seq @dfn{differential
expression} (DE), where a statistical model was established based on the
assumption that observed signals are the convolution of true expression
signals and sequencing noises.  The mapped reads in non-exonic regions are
considered as sequencing noises, which follows a Poisson distribution.  Given
measurable observed signal and background noise from RNA-seq data, true
expression signals, assuming governed by the negative binomial distribution,
can be delineated and thus the accurate detection of differential expressed
genes.")
    (license license:gpl3+)))

(define-public r-massspecwavelet
  (package
    (name "r-massspecwavelet")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MassSpecWavelet" version))
       (sha256
        (base32
         "0xnj3ncrwvr2b8msi3g77mgzj0zaksn3lgqdn1abh0ww5wgk83v7"))))
    (properties
     `((upstream-name . "MassSpecWavelet")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-waveslim" ,r-waveslim)))
    (home-page "https://bioconductor.org/packages/MassSpecWavelet/")
    (synopsis "Mass spectrum processing by wavelet-based algorithms")
    (description
     "The MassSpecWavelet package aims to process @dfn{Mass Spectrometry} (MS)
data mainly through the use of wavelet transforms.  It supports peak detection
based on @dfn{Continuous Wavelet Transform} (CWT).")
    (license license:lgpl2.0+)))

(define-public r-xcms
  (package
    (name "r-xcms")
    (version "3.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "xcms" version))
       (sha256
        (base32
         "0bfl56v3l6k31i11l09nx1yqfjy6z5yragm6k83z4w0mpgk18y7g"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-massspecwavelet" ,r-massspecwavelet)
       ("r-msnbase" ,r-msnbase)
       ("r-multtest" ,r-multtest)
       ("r-mzr" ,r-mzr)
       ("r-plyr" ,r-plyr)
       ("r-protgenerics" ,r-protgenerics)
       ("r-rann" ,r-rann)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-robustbase" ,r-robustbase)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/xcms/")
    (synopsis "LC/MS and GC/MS mass spectrometry data analysis")
    (description
     "This package provides a framework for processing and visualization of
chromatographically separated and single-spectra mass spectral data.  It
imports from AIA/ANDI NetCDF, mzXML, mzData and mzML files.  It preprocesses
data for high-throughput, untargeted analyte profiling.")
    (license license:gpl2+)))

(define-public r-wrench
  (package
    (name "r-wrench")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Wrench" version))
       (sha256
        (base32
         "1s8d7jn0dk5zk2fhvsd4sgicypz4c41nzf19nqzcwb9bd6asgrwm"))))
    (properties `((upstream-name . "Wrench")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-limma" ,r-limma)
       ("r-locfit" ,r-locfit)
       ("r-matrixstats" ,r-matrixstats)))
    (home-page "https://github.com/HCBravoLab/Wrench")
    (synopsis "Wrench normalization for sparse count data")
    (description
     "Wrench is a package for normalization sparse genomic count data, like
that arising from 16s metagenomic surveys.")
    (license license:artistic2.0)))

(define-public r-wiggleplotr
  (package
    (name "r-wiggleplotr")
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wiggleplotr" version))
       (sha256
        (base32
         "1pj2bsn5azs18mp5hr4g7c6rnds2y2rjjnak2785kaj7xi8jly4m"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-cowplot" ,r-cowplot)
       ("r-dplyr" ,r-dplyr)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iranges" ,r-iranges)
       ("r-purrr" ,r-purrr)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/wiggleplotr/")
    (synopsis "Make read coverage plots from BigWig files")
    (description
     "This package provides tools to visualize read coverage from sequencing
experiments together with genomic annotations (genes, transcripts, peaks).
Introns of long transcripts can be rescaled to a fixed length for better
visualization of exonic read coverage.")
    (license license:asl2.0)))

(define-public r-widgettools
  (package
    (name "r-widgettools")
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "widgetTools" version))
       (sha256
        (base32
         "1nqy3icayacv5mlv5s5xgfli0dqzancs6zpffrl5p237c994nyr5"))))
    (properties `((upstream-name . "widgetTools")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/widgetTools/")
    (synopsis "Tools for creating interactive tcltk widgets")
    (description
     "This package contains tools to support the construction of tcltk
widgets in R.")
    ;; Any version of the LGPL.
    (license license:lgpl3+)))

(define-public r-webbioc
  (package
    (name "r-webbioc")
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "webbioc" version))
       (sha256
        (base32
         "1cwrmvh1l603k6j1r425c4vrqp0zf0x7bgx7y1wnbq4r7yc5sp62"))))
    (build-system r-build-system)
    (inputs
     `(("netpbm" ,netpbm)
       ("perl" ,perl)))
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-annaffy" ,r-annaffy)
       ("r-biobase" ,r-biobase)
       ("r-biocmanager" ,r-biocmanager)
       ("r-gcrma" ,r-gcrma)
       ("r-multtest" ,r-multtest)
       ("r-qvalue" ,r-qvalue)
       ("r-vsn" ,r-vsn)))
    (home-page "https://www.bioconductor.org/")
    (synopsis "Bioconductor web interface")
    (description
     "This package provides an integrated web interface for doing microarray
analysis using several of the Bioconductor packages.  It is intended to be
deployed as a centralized bioinformatics resource for use by many users.
Currently only Affymetrix oligonucleotide analysis is supported.")
    (license license:gpl2+)))

(define-public r-zfpkm
  (package
    (name "r-zfpkm")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zFPKM" version))
       (sha256
        (base32
         "1hg0vc0ns8d4jpddn1v5a5m13a033b0wf1al01pb1lvmx5mzzr2n"))))
    (properties `((upstream-name . "zFPKM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-checkmate" ,r-checkmate)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://github.com/ronammar/zFPKM/")
    (synopsis "Functions to facilitate zFPKM transformations")
    (description
     "This is a package to perform the zFPKM transform on RNA-seq FPKM data.
This algorithm is based on the publication by Hart et al., 2013 (Pubmed ID
24215113).")
    (license license:gpl3)))

(define-public r-rbowtie2
  (package
    (name "r-rbowtie2")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie2" version))
       (sha256
        (base32
         "1b7x42n9zisi2w2wwfc0c39j8s9868imyr0ysqgswf6l5mv9ivc5"))))
    (properties `((upstream-name . "Rbowtie2")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://bioconductor.org/packages/Rbowtie2/")
    (synopsis "R wrapper for Bowtie2 and AdapterRemoval")
    (description
     "This package provides an R wrapper of the popular @code{bowtie2}
sequencing reads aligner and @code{AdapterRemoval}, a convenient tool for
rapid adapter trimming, identification, and read merging.")
    (license license:gpl3+)))

(define-public r-progeny
  (package
    (name "r-progeny")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "progeny" version))
       (sha256
        (base32
         "0j3kxjj7xmmwb29p87n3bw7bl38gcb94jrpw32126a578rf1pfrq"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-biobase" ,r-biobase)))
    (home-page "https://github.com/saezlab/progeny")
    (synopsis "Pathway responsive gene activity inference")
    (description
     "This package provides a function to infer pathway activity from gene
expression.  It contains the linear model inferred in the publication
\"Perturbation-response genes reveal signaling footprints in cancer gene
expression\".")
    (license license:asl2.0)))

(define-public r-arrmnormalization
  (package
    (name "r-arrmnormalization")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ARRmNormalization" version))
       (sha256
        (base32
         "0jj81q454nyh4hr5c56q1hv7i4ynx3rwnvpv08w34a6m77122bi1"))))
    (properties
     `((upstream-name . "ARRmNormalization")))
    (build-system r-build-system)
    (propagated-inputs `(("r-arrmdata" ,r-arrmdata)))
    (home-page "https://bioconductor.org/packages/ARRmNormalization/")
    (synopsis "Adaptive robust regression normalization for methylation data")
    (description
     "This is a package to perform the @dfn{Adaptive Robust Regression
method} (ARRm) for the normalization of methylation data from the Illumina
Infinium HumanMethylation 450k assay.")
    (license license:artistic2.0)))

(define-public r-biocfilecache
  (package
    (name "r-biocfilecache")
    (version "1.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocFileCache" version))
       (sha256
        (base32
         "0jivhn95y6zv5ryamqk6nyd4l8nlskq64j362l9ml9qw746v0ja6"))))
    (properties `((upstream-name . "BiocFileCache")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-curl" ,r-curl)
       ("r-dbi" ,r-dbi)
       ("r-dbplyr" ,r-dbplyr)
       ("r-dplyr" ,r-dplyr)
       ("r-httr" ,r-httr)
       ("r-rappdirs" ,r-rappdirs)
       ("r-rsqlite" ,r-rsqlite)))
    (home-page "https://bioconductor.org/packages/BiocFileCache/")
    (synopsis "Manage files across sessions")
    (description
     "This package creates a persistent on-disk cache of files that the user
can add, update, and retrieve.  It is useful for managing resources (such as
custom Txdb objects) that are costly or difficult to create, web resources,
and data files used across sessions.")
    (license license:artistic2.0)))

(define-public r-iclusterplus
  (package
    (name "r-iclusterplus")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iClusterPlus" version))
       (sha256
        (base32
         "1dzgfzf2x8m62hssvsn1zzag7m444kyxj2vpdxl9nk859dr5pf37"))))
    (properties `((upstream-name . "iClusterPlus")))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://bioconductor.org/packages/iClusterPlus/")
    (synopsis "Integrative clustering of multi-type genomic data")
    (description
     "iClusterPlus is developed for integrative clustering analysis of
multi-type genomic data and is an enhanced version of iCluster proposed and
developed by Shen, Olshen and Ladanyi (2009).  Multi-type genomic data arise
from the experiments where biological samples (e.g. tumor samples) are
analyzed by multiple techniques, for instance, @dfn{array comparative genomic
hybridization} (aCGH), gene expression microarray, RNA-seq and DNA-seq, and so
on.  In the iClusterPlus model, binary observations such as somatic mutation
are modeled as Binomial processes; categorical observations such as copy
number states are realizations of Multinomial random variables; counts are
modeled as Poisson random processes; and continuous measures are modeled by
Gaussian distributions.")
    (license license:gpl2+)))

(define-public r-rbowtie
  (package
    (name "r-rbowtie")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie" version))
       (sha256
        (base32
         "0yy31xhdmf6xb21wlhmxxwfqpm0nil39pb2cs3pq5ia758lb88z4"))))
    (properties `((upstream-name . "Rbowtie")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://bioconductor.org/packages/Rbowtie/")
    (synopsis "R bowtie wrapper")
    (description
     "This package provides an R wrapper around the popular bowtie short read
aligner and around SpliceMap, a de novo splice junction discovery and
alignment tool.")
    (license license:artistic2.0)))

(define-public r-sgseq
  (package
    (name "r-sgseq")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SGSeq" version))
       (sha256
        (base32
         "0950iv08wd0kjaw55rjn7m6syklvrabwr2zqq74wzyc5afyk2mrw"))))
    (properties `((upstream-name . "SGSeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-igraph" ,r-igraph)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-runit" ,r-runit)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/SGSeq/")
    (synopsis "Splice event prediction and quantification from RNA-seq data")
    (description
     "SGSeq is a package for analyzing splice events from RNA-seq data.  Input
data are RNA-seq reads mapped to a reference genome in BAM format.  Genes are
represented as a splice graph, which can be obtained from existing annotation
or predicted from the mapped sequence reads.  Splice events are identified
from the graph and are quantified locally using structurally compatible reads
at the start or end of each splice variant.  The software includes functions
for splice event prediction, quantification, visualization and
interpretation.")
    (license license:artistic2.0)))

(define-public r-rhisat2
  (package
    (name "r-rhisat2")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhisat2" version))
       (sha256
        (base32
         "02fn5cm8sj2s9x00505y3iyipn1r3lpvpwpjy2pdxdbpmhb5hy49"))))
    (properties `((upstream-name . "Rhisat2")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "src/Makefile"
               (("`hostname`") "guix")
               (("`date`") "0")
               ;; Avoid shelling out to "which".
               (("^CC =.*") (which "gcc"))
               (("^CPP =.*") (which "g++")))
             #t)))))
    (propagated-inputs
     `(("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-sgseq" ,r-sgseq)))
    (home-page "https://github.com/fmicompbio/Rhisat2")
    (synopsis "R Wrapper for HISAT2 sequence aligner")
    (description
     "This package provides an R interface to the HISAT2 spliced short-read
aligner by Kim et al. (2015).  The package contains wrapper functions to
create a genome index and to perform the read alignment to the generated
index.")
    (license license:gpl3)))

(define-public r-quasr
  (package
    (name "r-quasr")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "QuasR" version))
       (sha256
        (base32
         "07m5q7wasapj2984kwy897510flbhvz7x0fgs7q2vm5wjhfzdrgv"))))
    (properties `((upstream-name . "QuasR")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocmanager" ,r-biocmanager)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rbowtie" ,r-rbowtie)
       ("r-rhisat2" ,r-rhisat2)
       ("r-rhtslib" ,r-rhtslib)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shortread" ,r-shortread)))
    (home-page "https://bioconductor.org/packages/QuasR/")
    (synopsis "Quantify and annotate short reads in R")
    (description
     "This package provides a framework for the quantification and analysis of
short genomic reads.  It covers a complete workflow starting from raw sequence
reads, over creation of alignments and quality control plots, to the
quantification of genomic regions of interest.")
    (license license:gpl2)))

(define-public r-rqc
  (package
    (name "r-rqc")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rqc" version))
       (sha256
        (base32
         "1nxkrb9kx41g050yz935yrl9pjkakhr8v6whxcvr72gg4r9m2x3m"))))
    (properties `((upstream-name . "Rqc")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biocstyle" ,r-biocstyle)
       ("r-biostrings" ,r-biostrings)
       ("r-biovizbase" ,r-biovizbase)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iranges" ,r-iranges)
       ("r-knitr" ,r-knitr)
       ("r-markdown" ,r-markdown)
       ("r-plyr" ,r-plyr)
       ("r-rcpp" ,r-rcpp)
       ("r-reshape2" ,r-reshape2)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shiny" ,r-shiny)
       ("r-shortread" ,r-shortread)))
    (home-page "https://github.com/labbcb/Rqc")
    (synopsis "Quality control tool for high-throughput sequencing data")
    (description
     "Rqc is an optimized tool designed for quality control and assessment of
high-throughput sequencing data.  It performs parallel processing of entire
files and produces a report which contains a set of high-resolution
graphics.")
    (license license:gpl2+)))

(define-public r-birewire
  (package
    (name "r-birewire")
    (version "3.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiRewire" version))
       (sha256
        (base32
         "1074cp422ail72yajn8p0bg26h6zzz07nzypnwfyv86qrpvpdw9q"))))
    (properties `((upstream-name . "BiRewire")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-igraph" ,r-igraph)
       ("r-matrix" ,r-matrix)
       ("r-slam" ,r-slam)
       ("r-tsne" ,r-tsne)))
    (home-page "https://bioconductor.org/packages/release/bioc/html/BiRewire.html")
    (synopsis "Tools for randomization of bipartite graphs")
    (description
     "This package provides functions for bipartite network rewiring through N
consecutive switching steps and for the computation of the minimal number of
switching steps to be performed in order to maximise the dissimilarity with
respect to the original network.  It includes functions for the analysis of
the introduced randomness across the switching steps and several other
routines to analyse the resulting networks and their natural projections.")
    (license license:gpl3)))

(define-public r-birta
  (package
    (name "r-birta")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "birta" version))
       (sha256
        (base32
         "1zhlwapdgkz0fpv5bqfxh9aw6ymnmxnnm1r0n0kfzn5izyjavslg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-limma" ,r-limma)
       ("r-mass" ,r-mass)))
    (home-page "https://bioconductor.org/packages/birta")
    (synopsis "Bayesian inference of regulation of transcriptional activity")
    (description
     "Expression levels of mRNA molecules are regulated by different
processes, comprising inhibition or activation by transcription factors and
post-transcriptional degradation by microRNAs.  @dfn{birta} (Bayesian
Inference of Regulation of Transcriptional Activity) uses the regulatory
networks of transcription factors and miRNAs together with mRNA and miRNA
expression data to predict switches in regulatory activity between two
conditions.  A Bayesian network is used to model the regulatory structure and
Markov-Chain-Monte-Carlo is applied to sample the activity states.")
    (license license:gpl2+)))

(define-public r-multidataset
  (package
    (name "r-multidataset")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiDataSet" version))
       (sha256
        (base32
         "0iw99ymrv3ls023mpmgyagmb5simgsgadpj0k4bnssfaqnklywlj"))))
    (properties `((upstream-name . "MultiDataSet")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-iranges" ,r-iranges)
       ("r-limma" ,r-limma)
       ("r-qqman" ,r-qqman)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/MultiDataSet/")
    (synopsis "Implementation of MultiDataSet and ResultSet")
    (description
     "This package provides an implementation of the BRGE's (Bioinformatic
Research Group in Epidemiology from Center for Research in Environmental
Epidemiology) MultiDataSet and ResultSet.  MultiDataSet is designed for
integrating multi omics data sets and ResultSet is a container for omics
results.  This package contains base classes for MEAL and rexposome
packages.")
    (license license:expat)))

(define-public r-ropls
  (package
    (name "r-ropls")
    (version "1.18.8")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ropls" version))
       (sha256
        (base32
         "033i39r4037nd54jnp5zdn1vpzh61r671vmq0sf8dqrfblhm4w7a"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-multidataset" ,r-multidataset)))
    (native-inputs
     `(("r-knitr" ,r-knitr))) ; for vignettes
    (home-page "https://dx.doi.org/10.1021/acs.jproteome.5b00354")
    (synopsis "Multivariate analysis and feature selection of omics data")
    (description
     "Latent variable modeling with @dfn{Principal Component Analysis} (PCA)
and @dfn{Partial Least Squares} (PLS) are powerful methods for visualization,
regression, classification, and feature selection of omics data where the
number of variables exceeds the number of samples and with multicollinearity
among variables.  @dfn{Orthogonal Partial Least Squares} (OPLS) enables to
separately model the variation correlated (predictive) to the factor of
interest and the uncorrelated (orthogonal) variation.  While performing
similarly to PLS, OPLS facilitates interpretation.

This package provides imlementations of PCA, PLS, and OPLS for multivariate
analysis and feature selection of omics data.  In addition to scores, loadings
and weights plots, the package provides metrics and graphics to determine the
optimal number of components (e.g. with the R2 and Q2 coefficients), check the
validity of the model by permutation testing, detect outliers, and perform
feature selection (e.g. with Variable Importance in Projection or regression
coefficients).")
    (license license:cecill)))

(define-public r-biosigner
  (package
    (name "r-biosigner")
    (version "1.14.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biosigner" version))
       (sha256
        (base32
         "0hypk784xcax99mp673md6kvx45chk2nxbqniww7zm9q2hj983hl"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-e1071" ,r-e1071)
       ("r-multidataset" ,r-multidataset)
       ("r-randomforest" ,r-randomforest)
       ("r-ropls" ,r-ropls)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/biosigner/")
    (synopsis "Signature discovery from omics data")
    (description
     "Feature selection is critical in omics data analysis to extract
restricted and meaningful molecular signatures from complex and high-dimension
data, and to build robust classifiers.  This package implements a method to
assess the relevance of the variables for the prediction performances of the
classifier.  The approach can be run in parallel with the PLS-DA, Random
Forest, and SVM binary classifiers.  The signatures and the corresponding
'restricted' models are returned, enabling future predictions on new
datasets.")
    (license license:cecill)))

(define-public r-annotatr
  (package
    (name "r-annotatr")
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotatr" version))
       (sha256
        (base32
         "1bibk3p1q4cavqy11xs6rqqhqdjsq2dd7lf7blwcr27s5ajcd6dj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationhub" ,r-annotationhub)
       ("r-dplyr" ,r-dplyr)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iranges" ,r-iranges)
       ("r-readr" ,r-readr)
       ("r-regioner" ,r-regioner)
       ("r-reshape2" ,r-reshape2)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/annotatr/")
    (synopsis "Annotation of genomic regions to genomic annotations")
    (description
     "Given a set of genomic sites/regions (e.g. ChIP-seq peaks, CpGs,
differentially methylated CpGs or regions, SNPs, etc.) it is often of interest
to investigate the intersecting genomic annotations.  Such annotations include
those relating to gene models (promoters, 5'UTRs, exons, introns, and 3'UTRs),
CpGs (CpG islands, CpG shores, CpG shelves), or regulatory sequences such as
enhancers.  The annotatr package provides an easy way to summarize and
visualize the intersection of genomic sites/regions with genomic
annotations.")
    (license license:gpl3)))

(define-public r-rsubread
  (package
    (name "r-rsubread")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rsubread" version))
       (sha256
        (base32
         "0pdkx9mhvzw3a4ff725qvda9bxvs9vh2ppb29cc5jrivgjndv5cy"))))
    (properties `((upstream-name . "Rsubread")))
    (build-system r-build-system)
    (inputs `(("zlib" ,zlib)))
    (home-page "https://bioconductor.org/packages/Rsubread/")
    (synopsis "Subread sequence alignment and counting for R")
    (description
     "This package provides tools for alignment, quantification and analysis
of second and third generation sequencing data.  It includes functionality for
read mapping, read counting, SNP calling, structural variant detection and
gene fusion discovery.  It can be applied to all major sequencing techologies
and to both short and long sequence reads.")
    (license license:gpl3)))

(define-public r-flowutils
  (package
    (name "r-flowutils")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowUtils" version))
       (sha256
        (base32
         "1xcs19j9p3izvksyy5wzsbicwby0dsa9g2w7gjzwynzgj5dpfr81"))))
    (properties `((upstream-name . "flowUtils")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-corpcor" ,r-corpcor)
       ("r-flowcore" ,r-flowcore)
       ("r-graph" ,r-graph)
       ("r-runit" ,r-runit)
       ("r-xml" ,r-xml)))
    (home-page "https://github.com/jspidlen/flowUtils")
    (synopsis "Utilities for flow cytometry")
    (description
     "This package provides utilities for flow cytometry data.")
    (license license:artistic2.0)))

(define-public r-consensusclusterplus
  (package
    (name "r-consensusclusterplus")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ConsensusClusterPlus" version))
       (sha256
        (base32
         "0pnh5zy6d3c364xxjdn5zp3kf5afhsxv6yzkdn1qspny0pmqlrp4"))))
    (properties
     `((upstream-name . "ConsensusClusterPlus")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-all" ,r-all)
       ("r-biobase" ,r-biobase)
       ("r-cluster" ,r-cluster)))
    (home-page "https://bioconductor.org/packages/ConsensusClusterPlus")
    (synopsis "Clustering algorithm")
    (description
     "This package provides an implementation of an algorithm for determining
cluster count and membership by stability evidence in unsupervised analysis.")
    (license license:gpl2)))

(define-public r-cytolib
  (package
    (name "r-cytolib")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cytolib" version))
       (sha256
        (base32
         "1zk5i09k782lw6297gl3rh0g4fxswr2ang8dgqj7p0814l7h15k9"))))
    (properties `((upstream-name . "cytolib")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/cytolib/")
    (synopsis "C++ infrastructure for working with gated cytometry")
    (description
     "This package provides the core data structure and API to represent and
interact with gated cytometry data.")
    (license license:artistic2.0)))

(define-public r-flowcore
  (package
    (name "r-flowcore")
    (version "1.52.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowCore" version))
       (sha256
        (base32
         "08kvxc187iwlixibx1860jcp5g9bsw8abkv06x2qv1w83fas4pp2"))))
    (properties `((upstream-name . "flowCore")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-cytolib" ,r-cytolib)
       ("r-matrixstats" ,r-matrixstats)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://bioconductor.org/packages/flowCore")
    (synopsis "Basic structures for flow cytometry data")
    (description
     "This package provides S4 data structures and basic functions to deal
with flow cytometry data.")
    (license license:artistic2.0)))

(define-public r-flowmeans
  (package
    (name "r-flowmeans")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowMeans" version))
       (sha256
        (base32
         "1yisrikaafmpb4sig2c5l0wcz4idrs4as7i9x90v6z2v94iq0m8h"))))
    (properties `((upstream-name . "flowMeans")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-feature" ,r-feature)
       ("r-flowcore" ,r-flowcore)
       ("r-rrcov" ,r-rrcov)))
    (home-page "https://bioconductor.org/packages/flowMeans")
    (synopsis "Non-parametric flow cytometry data gating")
    (description
     "This package provides tools to identify cell populations in Flow
Cytometry data using non-parametric clustering and segmented-regression-based
change point detection.")
    (license license:artistic2.0)))

(define-public r-ncdfflow
  (package
    (name "r-ncdfflow")
    (version "2.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ncdfFlow" version))
       (sha256
        (base32
         "06lscx6h4rg80ifi90rj7z2497b8w1fjipm3l8s3230rkizhh02i"))))
    (properties `((upstream-name . "ncdfFlow")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-flowcore" ,r-flowcore)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rhdf5lib" ,r-rhdf5lib)
       ("r-zlibbioc" ,r-zlibbioc)))
    (home-page "https://bioconductor.org/packages/ncdfFlow/")
    (synopsis "HDF5 based storage for flow cytometry data")
    (description
     "This package provides HDF5 storage based methods and functions for
manipulation of flow cytometry data.")
    (license license:artistic2.0)))

(define-public r-ggcyto
  (package
    (name "r-ggcyto")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggcyto" version))
       (sha256
        (base32
         "16jwdslhmj1nsa28wmaircy15cq7qn8nsyiawinjv711qiqhgw50"))))
    (properties `((upstream-name . "ggcyto")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-flowcore" ,r-flowcore)
       ("r-flowworkspace" ,r-flowworkspace)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-ncdfflow" ,r-ncdfflow)
       ("r-plyr" ,r-plyr)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rlang" ,r-rlang)
       ("r-scales" ,r-scales)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/RGLab/ggcyto/issues")
    (synopsis "Visualize Cytometry data with ggplot")
    (description
     "With the dedicated fortify method implemented for @code{flowSet},
@code{ncdfFlowSet} and @code{GatingSet} classes, both raw and gated flow
cytometry data can be plotted directly with ggplot.  The @code{ggcyto} wrapper
and some custom layers also make it easy to add gates and population
statistics to the plot.")
    (license license:artistic2.0)))

(define-public r-flowviz
  (package
    (name "r-flowviz")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowViz" version))
       (sha256
        (base32
         "0ik16bxcfcg3q26ra3055718kskid64aaazcbqsxalca9ppdm4k7"))))
    (properties `((upstream-name . "flowViz")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-flowcore" ,r-flowcore)
       ("r-hexbin" ,r-hexbin)
       ("r-idpmisc" ,r-idpmisc)
       ("r-kernsmooth" ,r-kernsmooth)
       ("r-lattice" ,r-lattice)
       ("r-latticeextra" ,r-latticeextra)
       ("r-mass" ,r-mass)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page "https://bioconductor.org/packages/flowViz/")
    (synopsis "Visualization for flow cytometry")
    (description
     "This package provides visualization tools for flow cytometry data.")
    (license license:artistic2.0)))

(define-public r-flowclust
  (package
    (name "r-flowclust")
    (version "3.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowClust" version))
       (sha256
        (base32
         "0k4bgc4mf512njfdfg5ld9l7slgfxyfh766jab87j96zrrgcnj8s"))))
    (properties `((upstream-name . "flowClust")))
    (build-system r-build-system)
    (arguments
     `(#:configure-flags
       (list "--configure-args=--enable-bundled-gsl=no")))
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-clue" ,r-clue)
       ("r-corpcor" ,r-corpcor)
       ("r-ellipse" ,r-ellipse)
       ("r-flowcore" ,r-flowcore)
       ("r-flowviz" ,r-flowviz)
       ("r-graph" ,r-graph)
       ("r-mnormt" ,r-mnormt)))
    (inputs
     `(("gsl" ,gsl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://bioconductor.org/packages/flowClust")
    (synopsis "Clustering for flow cytometry")
    (description
     "This package provides robust model-based clustering using a t-mixture
model with Box-Cox transformation.")
    (license license:artistic2.0)))

;; TODO: this package bundles an old version of protobuf.  It's not easy to
;; make it use our protobuf package instead.
(define-public r-rprotobuflib
  (package
    (name "r-rprotobuflib")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RProtoBufLib" version))
       (sha256
        (base32
         "0dlgki21a37bxqh3cf83vl5zqxm86472g8a9plvhrjzzsn3mwnrm"))))
    (properties `((upstream-name . "RProtoBufLib")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-bundled-sources
           (lambda _
             (with-directory-excursion "src"
               (invoke "tar" "xf" "protobuf-2.6.0.tgz"))
             #t)))))
    (home-page "https://bioconductor.org/packages/RProtoBufLib/")
    (synopsis "C++ headers and static libraries of Protocol buffers")
    (description
     "This package provides the headers and static library of Protocol buffers
for other R packages to compile and link against.")
    (license license:bsd-3)))

(define-public r-flowworkspace
  (package
    (name "r-flowworkspace")
    (version "3.34.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowWorkspace" version))
       (sha256
        (base32
         "1ijbc6z9ljhrw3cqr02smgplhrfg44gzrb1dq4gbrpq3nj4khhpn"))))
    (properties `((upstream-name . "flowWorkspace")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-cytolib" ,r-cytolib)
       ("r-data-table" ,r-data-table)
       ("r-digest" ,r-digest)
       ("r-dplyr" ,r-dplyr)
       ("r-flowcore" ,r-flowcore)
       ("r-flowviz" ,r-flowviz)
       ("r-graph" ,r-graph)
       ("r-gridextra" ,r-gridextra)
       ("r-lattice" ,r-lattice)
       ("r-latticeextra" ,r-latticeextra)
       ("r-matrixstats" ,r-matrixstats)
       ("r-ncdfflow" ,r-ncdfflow)
       ("r-rbgl" ,r-rbgl)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppparallel" ,r-rcppparallel)
       ("r-rgraphviz" ,r-rgraphviz)
       ("r-rprotobuflib" ,r-rprotobuflib)
       ("r-scales" ,r-scales)
       ("r-stringr" ,r-stringr)))
    (home-page "https://bioconductor.org/packages/flowWorkspace/")
    (synopsis "Infrastructure for working with cytometry data")
    (description
     "This package is designed to facilitate comparison of automated gating
methods against manual gating done in flowJo.  This package allows you to
import basic flowJo workspaces into BioConductor and replicate the gating from
flowJo using the @code{flowCore} functionality.  Gating hierarchies, groups of
samples, compensation, and transformation are performed so that the output
matches the flowJo analysis.")
    (license license:artistic2.0)))

(define-public r-flowstats
  (package
    (name "r-flowstats")
    (version "3.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowStats" version))
       (sha256
        (base32
         "0pql0lpf90nra7w6z6nd8l9cgjlsg2pxysfravnbzfhl3pjvd96w"))))
    (properties `((upstream-name . "flowStats")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-cluster" ,r-cluster)
       ("r-fda" ,r-fda)
       ("r-flowcore" ,r-flowcore)
       ("r-flowviz" ,r-flowviz)
       ("r-flowworkspace" ,r-flowworkspace)
       ("r-kernsmooth" ,r-kernsmooth)
       ("r-ks" ,r-ks)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-ncdfflow" ,r-ncdfflow)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rrcov" ,r-rrcov)))
    (home-page "http://www.github.com/RGLab/flowStats")
    (synopsis "Statistical methods for the analysis of flow cytometry data")
    (description
     "This package provides methods and functionality to analyze flow data
that is beyond the basic infrastructure provided by the @code{flowCore}
package.")
    (license license:artistic2.0)))

(define-public r-opencyto
  (package
    (name "r-opencyto")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "openCyto" version))
       (sha256
        (base32
         "0h25nhvq1zq624wsgb55wjcgri9rcd2fnqkb31h9jdakr01dw2sb"))))
    (properties `((upstream-name . "openCyto")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-clue" ,r-clue)
       ("r-data-table" ,r-data-table)
       ("r-flowclust" ,r-flowclust)
       ("r-flowcore" ,r-flowcore)
       ("r-flowstats" ,r-flowstats)
       ("r-flowviz" ,r-flowviz)
       ("r-flowworkspace" ,r-flowworkspace)
       ("r-graph" ,r-graph)
       ("r-gtools" ,r-gtools)
       ("r-ks" ,r-ks)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-ncdfflow" ,r-ncdfflow)
       ("r-plyr" ,r-plyr)
       ("r-r-utils" ,r-r-utils)
       ("r-rbgl" ,r-rbgl)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ("r-rrcov" ,r-rrcov)))
    (home-page "https://bioconductor.org/packages/openCyto")
    (synopsis "Hierarchical gating pipeline for flow cytometry data")
    (description
     "This package is designed to facilitate the automated gating methods in a
sequential way to mimic the manual gating strategy.")
    (license license:artistic2.0)))

(define-public r-cytoml
  (package
    (name "r-cytoml")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CytoML" version))
       (sha256
        (base32
         "0m8x18wkvis85cawv7j07pk59w76wnzy93ia99gd24j82z4h97p1"))))
    (properties `((upstream-name . "CytoML")))
    (build-system r-build-system)
    (inputs
     `(("libxml2" ,libxml2)))
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-bh" ,r-bh)
       ("r-biobase" ,r-biobase)
       ("r-corpcor" ,r-corpcor)
       ("r-cytolib" ,r-cytolib)
       ("r-data-table" ,r-data-table)
       ("r-dplyr" ,r-dplyr)
       ("r-flowcore" ,r-flowcore)
       ("r-flowworkspace" ,r-flowworkspace)
       ("r-ggcyto" ,r-ggcyto)
       ("r-graph" ,r-graph)
       ("r-jsonlite" ,r-jsonlite)
       ("r-lattice" ,r-lattice)
       ("r-ncdfflow" ,r-ncdfflow)
       ("r-opencyto" ,r-opencyto)
       ("r-plyr" ,r-plyr)
       ("r-rbgl" ,r-rbgl)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppparallel" ,r-rcppparallel)
       ("r-rgraphviz" ,r-rgraphviz)
       ("r-rprotobuflib" ,r-rprotobuflib)
       ("r-runit" ,r-runit)
       ("r-xml" ,r-xml)
       ("r-yaml" ,r-yaml)))
    (home-page "https://github.com/RGLab/CytoML")
    (synopsis "GatingML interface for cross platform cytometry data sharing")
    (description
     "This package provides an interface to implementations of the GatingML2.0
standard to exchange gated cytometry data with other software platforms.")
    (license license:artistic2.0)))

(define-public r-flowsom
  (package
    (name "r-flowsom")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "FlowSOM" version))
       (sha256
        (base32
         "0265sq4zvj6d6h5ghqj9xzm4b0z9v65kgyl88cgdcpdkzfnfcvd5"))))
    (properties `((upstream-name . "FlowSOM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-consensusclusterplus" ,r-consensusclusterplus)
       ("r-cytoml" ,r-cytoml)
       ("r-flowcore" ,r-flowcore)
       ("r-flowworkspace" ,r-flowworkspace)
       ("r-igraph" ,r-igraph)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-tsne" ,r-tsne)
       ("r-xml" ,r-xml)))
    (home-page "https://bioconductor.org/packages/FlowSOM/")
    (synopsis "Visualize and interpret cytometry data")
    (description
     "FlowSOM offers visualization options for cytometry data, by using
self-organizing map clustering and minimal spanning trees.")
    (license license:gpl2+)))

(define-public r-mixomics
  (package
    (name "r-mixomics")
    (version "6.10.8")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mixOmics" version))
       (sha256
        (base32
         "0307vhx9ck24rxqbvq15815ssxcc226sl2la060n204b51wi9jaa"))))
    (properties `((upstream-name . "mixOmics")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-corpcor" ,r-corpcor)
       ("r-dplyr" ,r-dplyr)
       ("r-ellipse" ,r-ellipse)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-igraph" ,r-igraph)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-matrixstats" ,r-matrixstats)
       ("r-rarpack" ,r-rarpack)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-reshape2" ,r-reshape2)
       ("r-tidyr" ,r-tidyr)))
    (home-page "http://www.mixOmics.org")
    (synopsis "Multivariate methods for exploration of biological datasets")
    (description
     "mixOmics offers a wide range of multivariate methods for the exploration
and integration of biological datasets with a particular focus on variable
selection.  The package proposes several sparse multivariate models we have
developed to identify the key variables that are highly correlated, and/or
explain the biological outcome of interest.  The data that can be analysed
with mixOmics may come from high throughput sequencing technologies, such as
omics data (transcriptomics, metabolomics, proteomics, metagenomics etc) but
also beyond the realm of omics (e.g.  spectral imaging).  The methods
implemented in mixOmics can also handle missing values without having to
delete entire rows with missing data.")
    (license license:gpl2+)))

(define-public r-depecher
  (package
    (name "r-depecher")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DepecheR" version))
       (sha256
        (base32
         "199j2kw0xnw7y4v1gakm2jgyc7zzlj8xh0570f2yjq55gp1kggbm"))))
    (properties `((upstream-name . "DepecheR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beanplot" ,r-beanplot)
       ("r-dosnow" ,r-dosnow)
       ("r-dplyr" ,r-dplyr)
       ("r-fnn" ,r-fnn)
       ("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-mass" ,r-mass)
       ("r-matrixstats" ,r-matrixstats)
       ("r-mixomics" ,r-mixomics)
       ("r-moments" ,r-moments)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-reshape2" ,r-reshape2)
       ("r-robustbase" ,r-robustbase)
       ("r-viridis" ,r-viridis)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/DepecheR/")
    (synopsis "Identify traits of clusters in high-dimensional entities")
    (description
     "The purpose of this package is to identify traits in a dataset that can
separate groups.  This is done on two levels.  First, clustering is performed,
using an implementation of sparse K-means.  Secondly, the generated clusters
are used to predict outcomes of groups of individuals based on their
distribution of observations in the different clusters.  As certain clusters
with separating information will be identified, and these clusters are defined
by a sparse number of variables, this method can reduce the complexity of
data, to only emphasize the data that actually matters.")
    (license license:expat)))

(define-public r-rcistarget
  (package
    (name "r-rcistarget")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RcisTarget" version))
       (sha256
        (base32
         "1nnah7s0jd24fpfyxsf76jas8dm23c3266aps124wdlqsp9q5qjw"))))
    (properties `((upstream-name . "RcisTarget")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-aucell" ,r-aucell)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-data-table" ,r-data-table)
       ("r-feather" ,r-feather)
       ("r-gseabase" ,r-gseabase)
       ("r-r-utils" ,r-r-utils)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://aertslab.org/#scenic")
    (synopsis "Identify transcription factor binding motifs enriched on a gene list")
    (description
     "RcisTarget identifies @dfn{transcription factor binding motifs} (TFBS)
over-represented on a gene list.  In a first step, RcisTarget selects DNA
motifs that are significantly over-represented in the surroundings of the
@dfn{transcription start site} (TSS) of the genes in the gene-set.  This is
achieved by using a database that contains genome-wide cross-species rankings
for each motif.  The motifs that are then annotated to TFs and those that have
a high @dfn{Normalized Enrichment Score} (NES) are retained.  Finally, for
each motif and gene-set, RcisTarget predicts the candidate target genes (i.e.
genes in the gene-set that are ranked above the leading edge).")
    (license license:gpl3)))

(define-public r-cicero
  (package
    (name "r-cicero")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cicero" version))
       (sha256
        (base32
         "1ay1g2r0la4grcp1y8vcp211lfwzjf7j819ajzdirsh5dab8whld"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-data-table" ,r-data-table)
       ("r-dplyr" ,r-dplyr)
       ("r-fnn" ,r-fnn)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-glasso" ,r-glasso)
       ("r-gviz" ,r-gviz)
       ("r-igraph" ,r-igraph)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-monocle" ,r-monocle)
       ("r-plyr" ,r-plyr)
       ("r-reshape2" ,r-reshape2)
       ("r-s4vectors" ,r-s4vectors)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-vgam" ,r-vgam)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/cicero/")
    (synopsis "Predict cis-co-accessibility from single-cell data")
    (description
     "Cicero computes putative cis-regulatory maps from single-cell chromatin
accessibility data.  It also extends the monocle package for use in chromatin
accessibility data.")
    (license license:expat)))

;; This is the latest commit on the "monocle3" branch.
(define-public r-cicero-monocle3
  (let ((commit "fa2fb6515857a8cfc88bc9af044f34de1bcd2b7b")
        (revision "1"))
    (package (inherit r-cicero)
      (name "r-cicero-monocle3")
      (version (git-version "1.3.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cole-trapnell-lab/cicero-release.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "077yza93wdhi08n40md20jwk55k9lw1f3y0063qkk90cpz60wi0c"))))
      (propagated-inputs
       `(("r-monocle3" ,r-monocle3)
         ,@(alist-delete "r-monocle"
                         (package-propagated-inputs r-cicero)))))))

(define-public r-cistopic
  (let ((commit "29abd8df9afb60ff27ac3f0a590930debe926950")
        (revision "0"))
    (package
      (name "r-cistopic")
      (version (git-version "0.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aertslab/cisTopic.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0s8irpsv5d2zcv4ihanvsf1vrpignzliscxnvs4519af3jmx78h8"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-aucell" ,r-aucell)
         ("r-data-table" ,r-data-table)
         ("r-dplyr" ,r-dplyr)
         ("r-dosnow" ,r-dosnow)
         ("r-dt" ,r-dt)
         ("r-feather" ,r-feather)
         ("r-fitdistrplus" ,r-fitdistrplus)
         ("r-genomicranges" ,r-genomicranges)
         ("r-ggplot2" ,r-ggplot2)
         ("r-lda" ,r-lda)
         ("r-matrix" ,r-matrix)
         ("r-plyr" ,r-plyr)
         ("r-rcistarget" ,r-rcistarget)
         ("r-rtracklayer" ,r-rtracklayer)
         ("r-s4vectors" ,r-s4vectors)))
      (home-page "https://github.com/aertslab/cisTopic")
      (synopsis "Modelling of cis-regulatory topics from single cell epigenomics data")
      (description
       "The sparse nature of single cell epigenomics data can be overruled using
probabilistic modelling methods such as @dfn{Latent Dirichlet
Allocation} (LDA).  This package allows the probabilistic modelling of
cis-regulatory topics (cisTopics) from single cell epigenomics data, and
includes functionalities to identify cell states based on the contribution of
cisTopics and explore the nature and regulatory proteins driving them.")
      (license license:gpl3))))

(define-public r-genie3
  (package
    (name "r-genie3")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GENIE3" version))
       (sha256
        (base32
         "0p67lhgy3lb4nc958s51hx7rvgwhzwfic9xhpsrask40k43spv7l"))))
    (properties `((upstream-name . "GENIE3")))
    (build-system r-build-system)
    (propagated-inputs `(("r-reshape2" ,r-reshape2)))
    (home-page "https://bioconductor.org/packages/GENIE3")
    (synopsis "Gene network inference with ensemble of trees")
    (description
     "This package implements the GENIE3 algorithm for inferring gene
regulatory networks from expression data.")
    (license license:gpl2+)))

(define-public r-roc
  (package
    (name "r-roc")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROC" version))
       (sha256
        (base32
         "1aqpyc28czagg3nbybh55vf152nbar61jjw79w04326d97mc3j3y"))))
    (properties `((upstream-name . "ROC")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://www.bioconductor.org/packages/ROC/")
    (synopsis "Utilities for ROC curves")
    (description
     "This package provides utilities for @dfn{Receiver Operating
Characteristic} (ROC) curves, with a focus on micro arrays.")
    (license license:artistic2.0)))

(define-public r-illuminahumanmethylation450kanno-ilmn12-hg19
  (package
    (name "r-illuminahumanmethylation450kanno-ilmn12-hg19")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri
             "IlluminaHumanMethylation450kanno.ilmn12.hg19"
             version 'annotation))
       (sha256
        (base32
         "059vlxsx3p3fcnywwirahsc6mlk813zpqnbv0jsrag6x5bb8z6r4"))))
    (properties
     `((upstream-name
        . "IlluminaHumanMethylation450kanno.ilmn12.hg19")))
    (build-system r-build-system)
    (propagated-inputs `(("r-minfi" ,r-minfi)))
    (home-page
     "https://bioconductor.org/packages/IlluminaHumanMethylation450kanno.ilmn12.hg19/")
    (synopsis "Annotation for Illumina's 450k methylation arrays")
    (description
     "This package provides manifests and annotation for Illumina's 450k array
data.")
    (license license:artistic2.0)))

(define-public r-watermelon
  (package
    (name "r-watermelon")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wateRmelon" version))
       (sha256
        (base32
         "0a66fq04yph9dis91lzjk9kh134zy6fj98yklrwf24r1080qngx0"))))
    (properties `((upstream-name . "wateRmelon")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-illuminahumanmethylation450kanno-ilmn12-hg19"
        ,r-illuminahumanmethylation450kanno-ilmn12-hg19)
       ("r-illuminaio" ,r-illuminaio)
       ("r-limma" ,r-limma)
       ("r-lumi" ,r-lumi)
       ("r-matrixstats" ,r-matrixstats)
       ("r-methylumi" ,r-methylumi)
       ("r-roc" ,r-roc)))
    (home-page "https://bioconductor.org/packages/wateRmelon/")
    (synopsis "Illumina 450 methylation array normalization and metrics")
    (description
     "The standard index of DNA methylation (beta) is computed from methylated
and unmethylated signal intensities.  Betas calculated from raw signal
intensities perform well, but using 11 methylomic datasets we demonstrate that
quantile normalization methods produce marked improvement.  The commonly used
procedure of normalizing betas is inferior to the separate normalization of M
and U, and it is also advantageous to normalize Type I and Type II assays
separately.  This package provides 15 flavours of betas and three performance
metrics, with methods for objects produced by the @code{methylumi} and
@code{minfi} packages.")
    (license license:gpl3)))

(define-public r-gdsfmt
  (package
    (name "r-gdsfmt")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gdsfmt" version))
       (sha256
        (base32
         "0zc9v62imd0ykz4h30pxa64q0y45qijmkwdk2pd4ncsg8fc2jlz9"))
       (modules '((guix build utils)))
       ;; Remove bundled sources of zlib, lz4, and xz.  Don't attempt to build
       ;; them and link with system libraries instead.
       (snippet
        '(begin
           (for-each delete-file-recursively
                     '("src/LZ4"
                       "src/XZ"
                       "src/ZLIB"))
           (substitute* "src/Makevars"
             (("all: \\$\\(SHLIB\\)") "all:")
             (("\\$\\(SHLIB\\): liblzma.a") "")
             (("(ZLIB|LZ4)/.*") "")
             (("CoreArray/dVLIntGDS.cpp.*")
              "CoreArray/dVLIntGDS.cpp")
             (("CoreArray/dVLIntGDS.o.*")
              "CoreArray/dVLIntGDS.o")
             (("PKG_LIBS = ./liblzma.a")
              "PKG_LIBS = -llz4"))
           (substitute* "src/CoreArray/dStream.h"
             (("include \"../(ZLIB|LZ4|XZ/api)/(.*)\"" _ _ header)
              (string-append "include <" header ">")))
           #t))))
    (properties `((upstream-name . "gdsfmt")))
    (build-system r-build-system)
    (inputs
     `(("lz4" ,lz4)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (home-page "http://corearray.sourceforge.net/")
    (synopsis
     "R Interface to CoreArray Genomic Data Structure (GDS) Files")
    (description
     "This package provides a high-level R interface to CoreArray @dfn{Genomic
Data Structure} (GDS) data files, which are portable across platforms with
hierarchical structure to store multiple scalable array-oriented data sets
with metadata information.  It is suited for large-scale datasets, especially
for data which are much larger than the available random-access memory.  The
@code{gdsfmt} package offers efficient operations specifically designed for
integers of less than 8 bits, since a diploid genotype, like
@dfn{single-nucleotide polymorphism} (SNP), usually occupies fewer bits than a
byte.  Data compression and decompression are available with relatively
efficient random access.  It is also allowed to read a GDS file in parallel
with multiple R processes supported by the package @code{parallel}.")
    (license license:lgpl3)))

(define-public r-bigmelon
  (package
    (name "r-bigmelon")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigmelon" version))
       (sha256
        (base32
         "0sw7rp4p03m1s72b4j06jfb7as3v1n2w2z4ppk8s4f80fb05bcls"))))
    (properties `((upstream-name . "bigmelon")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-gdsfmt" ,r-gdsfmt)
       ("r-geoquery" ,r-geoquery)
       ("r-methylumi" ,r-methylumi)
       ("r-minfi" ,r-minfi)
       ("r-watermelon" ,r-watermelon)))
    (home-page "https://bioconductor.org/packages/bigmelon/")
    (synopsis "Illumina methylation array analysis for large experiments")
    (description
     "This package provides methods for working with Illumina arrays using the
@code{gdsfmt} package.")
    (license license:gpl3)))

(define-public r-seqbias
  (package
    (name "r-seqbias")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqbias" version))
       (sha256
        (base32
         "19vbdyjblij2533ibmrw1n0rkqfrbflma6cg5b79ghks0mg7z8hq"))))
    (properties `((upstream-name . "seqbias")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-genomicranges" ,r-genomicranges)
       ("r-rhtslib" ,r-rhtslib)))
    (inputs
     `(("zlib" ,zlib))) ; This comes from rhtslib.
    (home-page "https://bioconductor.org/packages/seqbias/")
    (synopsis "Estimation of per-position bias in high-throughput sequencing data")
    (description
     "This package implements a model of per-position sequencing bias in
high-throughput sequencing data using a simple Bayesian network, the structure
and parameters of which are trained on a set of aligned reads and a reference
genome sequence.")
    (license license:lgpl3)))

(define-public r-reqon
  (package
    (name "r-reqon")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReQON" version))
       (sha256
        (base32
         "10p6l2zxijqyypdm970jyfqyrnfhaq3nf7cg2q6mgd1srggfa0cx"))))
    (properties `((upstream-name . "ReQON")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rjava" ,r-rjava)
       ("r-rsamtools" ,r-rsamtools)
       ("r-seqbias" ,r-seqbias)))
    (home-page "https://bioconductor.org/packages/ReQON/")
    (synopsis "Recalibrating quality of nucleotides")
    (description
     "This package provides an implementation of an algorithm for
recalibrating the base quality scores for aligned sequencing data in BAM
format.")
    (license license:gpl2)))

(define-public r-wavcluster
  (package
    (name "r-wavcluster")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wavClusteR" version))
       (sha256
        (base32
         "0vq3xxsvwq86rlya7xc92sc4i6b48sib0pcina3xivg3ky2j3z7y"))))
    (properties `((upstream-name . "wavClusteR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-foreach" ,r-foreach)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-hmisc" ,r-hmisc)
       ("r-iranges" ,r-iranges)
       ("r-mclust" ,r-mclust)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-seqinr" ,r-seqinr)
       ("r-stringr" ,r-stringr)
       ("r-wmtsa" ,r-wmtsa)))
    (home-page "https://bioconductor.org/packages/wavClusteR/")
    (synopsis "Identification of RNA-protein interaction sites in PAR-CLIP data")
    (description
     "This package provides an integrated pipeline for the analysis of
PAR-CLIP data.  PAR-CLIP-induced transitions are first discriminated from
sequencing errors, SNPs and additional non-experimental sources by a non-
parametric mixture model.  The protein binding sites (clusters) are then
resolved at high resolution and cluster statistics are estimated using a
rigorous Bayesian framework.  Post-processing of the results, data export for
UCSC genome browser visualization and motif search analysis are provided.  In
addition, the package allows to integrate RNA-Seq data to estimate the False
Discovery Rate of cluster detection.  Key functions support parallel multicore
computing.  While wavClusteR was designed for PAR-CLIP data analysis, it can
be applied to the analysis of other NGS data obtained from experimental
procedures that induce nucleotide substitutions (e.g. BisSeq).")
    (license license:gpl2)))

(define-public r-timeseriesexperiment
  (package
    (name "r-timeseriesexperiment")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TimeSeriesExperiment" version))
       (sha256
        (base32
         "0xqa6hzknnci20zx2f6mw5cpqx8pq0v6fzf053hh51p1l2ikvgqm"))))
    (properties
     `((upstream-name . "TimeSeriesExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-deseq2" ,r-deseq2)
       ("r-dplyr" ,r-dplyr)
       ("r-dynamictreecut" ,r-dynamictreecut)
       ("r-edger" ,r-edger)
       ("r-ggplot2" ,r-ggplot2)
       ("r-hmisc" ,r-hmisc)
       ("r-limma" ,r-limma)
       ("r-magrittr" ,r-magrittr)
       ("r-proxy" ,r-proxy)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-vegan" ,r-vegan)
       ("r-viridis" ,r-viridis)))
    (home-page "https://github.com/nlhuong/TimeSeriesExperiment/")
    (synopsis "Analysis for short time-series data")
    (description
     "This package is a visualization and analysis toolbox for short time
course data which includes dimensionality reduction, clustering, two-sample
differential expression testing and gene ranking techniques.  The package also
provides methods for retrieving enriched pathways.")
    (license license:lgpl3+)))

(define-public r-variantfiltering
  (package
    (name "r-variantfiltering")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantFiltering" version))
       (sha256
        (base32
         "13pgfk2mbffd9smmxnwz7g0jrwng78711053wfzadr107zbyn4r8"))))
    (properties
     `((upstream-name . "VariantFiltering")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-dt" ,r-dt)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-genomicscores" ,r-genomicscores)
       ("r-graph" ,r-graph)
       ("r-gviz" ,r-gviz)
       ("r-iranges" ,r-iranges)
       ("r-rbgl" ,r-rbgl)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shiny" ,r-shiny)
       ("r-shinyjs" ,r-shinyjs)
       ("r-shinythemes" ,r-shinythemes)
       ("r-shinytree" ,r-shinytree)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)
       ("r-xvector" ,r-xvector)))
    (home-page "https://github.com/rcastelo/VariantFiltering")
    (synopsis "Filtering of coding and non-coding genetic variants")
    (description
     "Filter genetic variants using different criteria such as inheritance
model, amino acid change consequence, minor allele frequencies across human
populations, splice site strength, conservation, etc.")
    (license license:artistic2.0)))

(define-public r-genomegraphs
  (package
    (name "r-genomegraphs")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomeGraphs" version))
       (sha256
        (base32
         "05vavhz936v7cknig2f2mn3fd9fiy54r3swlvifpawramblp1ags"))))
    (properties `((upstream-name . "GenomeGraphs")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biomart" ,r-biomart)))
    (home-page "https://bioconductor.org/packages/GenomeGraphs/")
    (synopsis "Plotting genomic information from Ensembl")
    (description
     "Genomic data analyses requires integrated visualization of known genomic
information and new experimental data.  GenomeGraphs uses the biomaRt package
to perform live annotation queries to Ensembl and translates this to e.g.
gene/transcript structures in viewports of the grid graphics package.  This
results in genomic information plotted together with your data.  Another
strength of GenomeGraphs is to plot different data types such as array CGH,
gene expression, sequencing and other data, together in one plot using the
same genome coordinate system.")
    (license license:artistic2.0)))

(define-public r-wavetiling
  (package
    (name "r-wavetiling")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "waveTiling" version))
       (sha256
        (base32
         "0d7l559zlmly8mncmh1zhkqmsml0bwwfpm7ccp8l26y852vwf7hf"))))
    (properties `((upstream-name . "waveTiling")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-biobase" ,r-biobase)
       ("r-biostrings" ,r-biostrings)
       ("r-genomegraphs" ,r-genomegraphs)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-oligo" ,r-oligo)
       ("r-oligoclasses" ,r-oligoclasses)
       ("r-preprocesscore" ,r-preprocesscore)
       ("r-waveslim" ,r-waveslim)))
    (home-page "https://r-forge.r-project.org/projects/wavetiling/")
    (synopsis "Wavelet-based models for tiling array transcriptome analysis")
    (description
     "This package is designed to conduct transcriptome analysis for tiling
arrays based on fast wavelet-based functional models.")
    (license license:gpl2+)))

(define-public r-variancepartition
  (package
    (name "r-variancepartition")
    (version "1.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "variancePartition" version))
       (sha256
        (base32
         "02pzsff14j4am2d949mh8xgi0c7k44g09q4lr6nqm08vf92brb6g"))))
    (properties
     `((upstream-name . "variancePartition")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocparallel" ,r-biocparallel)
       ("r-colorramps" ,r-colorramps)
       ("r-doparallel" ,r-doparallel)
       ("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-iterators" ,r-iterators)
       ("r-limma" ,r-limma)
       ("r-lme4" ,r-lme4)
       ("r-lmertest" ,r-lmertest)
       ("r-mass" ,r-mass)
       ("r-pbkrtest" ,r-pbkrtest)
       ("r-progress" ,r-progress)
       ("r-reshape2" ,r-reshape2)
       ("r-scales" ,r-scales)))
    (home-page "https://bioconductor.org/packages/variancePartition/")
    (synopsis "Analyze variation in gene expression experiments")
    (description
     "This is a package providing tools to quantify and interpret multiple
sources of biological and technical variation in gene expression experiments.
It uses a linear mixed model to quantify variation in gene expression
attributable to individual, tissue, time point, or technical variables.  The
package includes dream differential expression analysis for repeated
measures.")
    (license license:gpl2+)))

(define-public r-htqpcr
  (package
    (name "r-htqpcr")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HTqPCR" version))
       (sha256
        (base32
         "008iczqaa0wn5nw144vfg3qylg7qa1q963nq9mqhgj3sxlg4rmjx"))))
    (properties `((upstream-name . "HTqPCR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-biobase" ,r-biobase)
       ("r-gplots" ,r-gplots)
       ("r-limma" ,r-limma)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page (string-append "https://www.ebi.ac.uk/sites/ebi.ac.uk/files/"
                              "groups/bertone/software/HTqPCR.pdf"))
    (synopsis "Automated analysis of high-throughput qPCR data")
    (description
     "Analysis of Ct values from high throughput quantitative real-time
PCR (qPCR) assays across multiple conditions or replicates.  The input data
can be from spatially-defined formats such ABI TaqMan Low Density Arrays or
OpenArray; LightCycler from Roche Applied Science; the CFX plates from Bio-Rad
Laboratories; conventional 96- or 384-well plates; or microfluidic devices
such as the Dynamic Arrays from Fluidigm Corporation.  HTqPCR handles data
loading, quality assessment, normalization, visualization and parametric or
non-parametric testing for statistical significance in Ct values between
features (e.g.  genes, microRNAs).")
    (license license:artistic2.0)))

(define-public r-unifiedwmwqpcr
  (package
    (name "r-unifiedwmwqpcr")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "unifiedWMWqPCR" version))
       (sha256
        (base32
         "1l6rf7scxxyz4x0m4li54y6905sqj4jrx481zb9h3vqhcfcmn8lj"))))
    (properties
     `((upstream-name . "unifiedWMWqPCR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-htqpcr" ,r-htqpcr)))
    (home-page "https://bioconductor.org/packages/unifiedWMWqPCR")
    (synopsis "Unified Wilcoxon-Mann Whitney Test for differential expression in qPCR data")
    (description
     "This package implements the unified Wilcoxon-Mann-Whitney Test for qPCR
data.  This modified test allows for testing differential expression in qPCR
data.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on Bioconductor packages, so we put
;; it here.
(define-public r-activedriverwgs
  (package
    (name "r-activedriverwgs")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActiveDriverWGS" version))
       (sha256
        (base32
         "08l9dj8d3cd74z1dqn8n4yqykwvqjxsfa067wnxyh7xnfvvnm5v1"))))
    (properties
     `((upstream-name . "ActiveDriverWGS")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-plyr" ,r-plyr)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://cran.r-project.org/web/packages/ActiveDriverWGS/")
    (synopsis "Driver discovery tool for cancer whole genomes")
    (description
     "This package provides a method for finding an enrichment of cancer
simple somatic mutations (SNVs and Indels) in functional elements across the
human genome.  ActiveDriverWGS detects coding and noncoding driver elements
using whole genome sequencing data.")
    (license license:gpl3)))

;; This is a CRAN package, but it depends on Bioconductor packages, so we put
;; it here.
(define-public r-activepathways
  (package
    (name "r-activepathways")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActivePathways" version))
       (sha256
        (base32
         "1xb0d1svmzw404pv8ja6wr4773k7x2lxkrzrayilvzqbfzj1wx20"))))
    (properties
     `((upstream-name . "ActivePathways")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-ggplot2" ,r-ggplot2)
       ("r-metap" ,r-metap)))
    (home-page "https://cran.r-project.org/web/packages/ActivePathways/")
    (synopsis "Multivariate pathway enrichment analysis")
    (description
     "This package represents an integrative method of analyzing multi omics
data that conducts enrichment analysis of annotated gene sets.  ActivePathways
uses a statistical data fusion approach, rationalizes contributing evidence
and highlights associated genes, improving systems-level understanding of
cellular organization in health and disease.")
    (license license:gpl3)))

(define-public r-bgmix
  (package
    (name "r-bgmix")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BGmix" version))
       (sha256
        (base32
         "1bwqqhkh4m3hhpd71grwjrg7n07lzvys4y7aghmw2gw5ibnk5683"))))
    (properties `((upstream-name . "BGmix")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-kernsmooth" ,r-kernsmooth)))
    (home-page "https://bioconductor.org/packages/BGmix/")
    (synopsis "Bayesian models for differential gene expression")
    (description
     "This package provides fully Bayesian mixture models for differential
gene expression.")
    (license license:gpl2)))

(define-public r-bgx
  (package
    (name "r-bgx")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bgx" version))
       (sha256
        (base32
         "0fiqqv6pin0zhxaw67hzfjccq2qkl9qfqjf10nx2zmpxm2licavm"))))
    (properties `((upstream-name . "bgx")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-biobase" ,r-biobase)
       ("r-gcrma" ,r-gcrma)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://bioconductor.org/packages/bgx/")
    (synopsis "Bayesian gene expression")
    (description
     "This package provides tools for Bayesian integrated analysis of
Affymetrix GeneChips.")
    (license license:gpl2)))

(define-public r-bhc
  (package
    (name "r-bhc")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BHC" version))
       (sha256
        (base32
         "1bxx3jak8mgvay3j1xd59bb9j86pzl6hh5abxww9x1b7rswmy1jh"))))
    (properties `((upstream-name . "BHC")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BHC/")
    (synopsis "Bayesian hierarchical clustering")
    (description
     "The method implemented in this package performs bottom-up hierarchical
clustering, using a Dirichlet Process (infinite mixture) to model uncertainty
in the data and Bayesian model selection to decide at each step which clusters
to merge.  This avoids several limitations of traditional methods, for example
how many clusters there should be and how to choose a principled distance
metric.  This implementation accepts multinomial (i.e. discrete, with 2+
categories) or time-series data.  This version also includes a randomised
algorithm which is more efficient for larger data sets.")
    (license license:gpl3)))

(define-public r-bicare
  (package
    (name "r-bicare")
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BicARE" version))
       (sha256
        (base32
         "1gia5vzmvbk4k1vx3bh9nld1ws9s3c0y11qfbzqhfnfjbd7n8qcs"))))
    (properties `((upstream-name . "BicARE")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-gseabase" ,r-gseabase)
       ("r-multtest" ,r-multtest)))
    (home-page "http://bioinfo.curie.fr")
    (synopsis "Biclustering analysis and results exploration")
    (description
     "This is a package for biclustering analysis and exploration of
results.")
    (license license:gpl2)))

(define-public r-bifet
  (package
    (name "r-bifet")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiFET" version))
       (sha256
        (base32
         "0ck1d6hxd4f40hfz8p2z5xmjbz79yhrf6fisjka2xzk5v9fm4p4k"))))
    (properties `((upstream-name . "BiFET")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomicranges" ,r-genomicranges)
       ("r-poibin" ,r-poibin)))
    (home-page "https://bioconductor.org/packages/BiFET")
    (synopsis "Bias-free footprint enrichment test")
    (description
     "BiFET identifies @dfn{transcription factors} (TFs) whose footprints are
over-represented in target regions compared to background regions after
correcting for the bias arising from the imbalance in read counts and GC
contents between the target and background regions.  For a given TF k, BiFET
tests the null hypothesis that the target regions have the same probability of
having footprints for the TF k as the background regions while correcting for
the read count and GC content bias.")
    (license license:gpl3)))

(define-public r-rsbml
  (package
    (name "r-rsbml")
    (version "2.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rsbml" version))
       (sha256
        (base32
         "1dbp0aaijxn3na26b68ws0v9qzvml61ifb9z4i8pz7q6h48n7lxa"))))
    (properties `((upstream-name . "rsbml")))
    (build-system r-build-system)
    (inputs
     `(("libsbml" ,libsbml)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-graph" ,r-graph)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.sbml.org")
    (synopsis "R support for SBML")
    (description
     "This package provides an R interface to libsbml for SBML parsing,
validating output, provides an S4 SBML DOM, converts SBML to R graph objects.")
    (license license:artistic2.0)))

(define-public r-hypergraph
  (package
    (name "r-hypergraph")
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hypergraph" version))
       (sha256
        (base32
         "1bixmslxy7r987zw1vf4dg72hfi04lf4vj03n7ygym2g8nfhbh7m"))))
    (properties `((upstream-name . "hypergraph")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-graph" ,r-graph)))
    (home-page "https://bioconductor.org/packages/hypergraph")
    (synopsis "Hypergraph data structures")
    (description
     "This package implements some simple capabilities for representing and
manipulating hypergraphs.")
    (license license:artistic2.0)))

(define-public r-hyperdraw
  (package
    (name "r-hyperdraw")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hyperdraw" version))
       (sha256
        (base32
         "0a8h3pb7196qi49ady8ni92m5wqb1hvxw6khk9j63mwj3h7jinbj"))))
    (properties `((upstream-name . "hyperdraw")))
    (build-system r-build-system)
    (inputs `(("graphviz" ,graphviz)))
    (propagated-inputs
     `(("r-graph" ,r-graph)
       ("r-hypergraph" ,r-hypergraph)
       ("r-rgraphviz" ,r-rgraphviz)))
    (home-page "https://bioconductor.org/packages/hyperdraw")
    (synopsis "Visualizing hypergraphs")
    (description
     "This package provides functions for visualizing hypergraphs.")
    (license license:gpl2+)))

(define-public r-biggr
  (package
    (name "r-biggr")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiGGR" version))
       (sha256
        (base32
         "1n2ypc84abmhn6br0yi87k7lvjc11k7abzhgvzdabc2ai1qgcqif"))))
    (properties `((upstream-name . "BiGGR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-hyperdraw" ,r-hyperdraw)
       ("r-hypergraph" ,r-hypergraph)
       ("r-lim" ,r-lim)
       ("r-limsolve" ,r-limsolve)
       ("r-rsbml" ,r-rsbml)
       ("r-stringr" ,r-stringr)))
    (home-page "https://bioconductor.org/packages/BiGGR/")
    (synopsis "Constraint based modeling using metabolic reconstruction databases")
    (description
     "This package provides an interface to simulate metabolic reconstruction
from the @url{http://bigg.ucsd.edu/, BiGG database} and other metabolic
reconstruction databases.  The package facilitates @dfn{flux balance
analysis} (FBA) and the sampling of feasible flux distributions.  Metabolic
networks and estimated fluxes can be visualized with hypergraphs.")
    (license license:gpl3+)))

(define-public r-bigmemoryextras
  (package
    (name "r-bigmemoryextras")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigmemoryExtras" version))
       (sha256
        (base32
         "17dk7c44ikphcrpi8hnxyvlmj30qmj098kc0ihfi69bp9rw1cibq"))))
    (properties
     `((upstream-name . "bigmemoryExtras")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bigmemory" ,r-bigmemory)))
    (home-page "https://github.com/phaverty/bigmemoryExtras")
    (synopsis "Extension of the bigmemory package")
    (description
     "This package defines a @code{BigMatrix} @code{ReferenceClass} which adds
safety and convenience features to the @code{filebacked.big.matrix} class from
the @code{bigmemory} package.  @code{BigMatrix} protects against segfaults by
monitoring and gracefully restoring the connection to on-disk data and it also
protects against accidental data modification with a filesystem-based
permissions system.  Utilities are provided for using @code{BigMatrix}-derived
classes as @code{assayData} matrices within the @code{Biobase} package's
@code{eSet} family of classes.  @code{BigMatrix} provides some optimizations
related to attaching to, and indexing into, file-backed matrices with
dimnames.  Additionally, the package provides a @code{BigMatrixFactor} class,
a file-backed matrix with factor properties.")
    (license license:artistic2.0)))

(define-public r-bigpint
  (package
    (name "r-bigpint")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigPint" version))
       (sha256
        (base32
         "10vs0lzfyxp6sm4r9pxfwipjvzmmaqnvwn1hc5q37s5qz44fg0hk"))))
    (properties `((upstream-name . "bigPint")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-ggally" ,r-ggally)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-hexbin" ,r-hexbin)
       ("r-hmisc" ,r-hmisc)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-plotly" ,r-plotly)
       ("r-plyr" ,r-plyr)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-reshape" ,r-reshape)
       ("r-shiny" ,r-shiny)
       ("r-shinycssloaders" ,r-shinycssloaders)
       ("r-shinydashboard" ,r-shinydashboard)
       ("r-stringr" ,r-stringr)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://github.com/lindsayrutter/bigPint")
    (synopsis "Big multivariate data plotted interactively")
    (description
     "This package provides methods for visualizing large multivariate
datasets using static and interactive scatterplot matrices, parallel
coordinate plots, volcano plots, and litre plots.  It includes examples for
visualizing RNA-sequencing datasets and differentially expressed genes.")
    (license license:gpl3)))

(define-public r-chemminer
  (package
    (name "r-chemminer")
    (version "3.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChemmineR" version))
       (sha256
        (base32
         "1j6vmkhc03dmmkm5wgbcv62pw5dclp49f906xkx1pwg27bdldbga"))))
    (properties `((upstream-name . "ChemmineR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-bh" ,r-bh)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-dbi" ,r-dbi)
       ("r-digest" ,r-digest)
       ("r-dt" ,r-dt)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-png" ,r-png)
       ("r-rcpp" ,r-rcpp)
       ("r-rcurl" ,r-rcurl)
       ("r-rjson" ,r-rjson)
       ("r-rsvg" ,r-rsvg)))
    (home-page "https://github.com/girke-lab/ChemmineR")
    (synopsis "Cheminformatics toolkit for R")
    (description
     "ChemmineR is a cheminformatics package for analyzing drug-like small
molecule data in R.  It contains functions for efficient processing of large
numbers of molecules, physicochemical/structural property predictions,
structural similarity searching, classification and clustering of compound
libraries with a wide spectrum of algorithms.  In addition, it offers
visualization functions for compound clustering results and chemical
structures.")
    (license license:artistic2.0)))

(define-public r-bioassayr
  (package
    (name "r-bioassayr")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioassayR" version))
       (sha256
        (base32
         "08vxkvxhqnryfbj4dwk3ifb9pn544www9zk2pj9fjbh5xfpwi7zw"))))
    (properties `((upstream-name . "bioassayR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-chemminer" ,r-chemminer)
       ("r-dbi" ,r-dbi)
       ("r-matrix" ,r-matrix)
       ("r-rjson" ,r-rjson)
       ("r-rsqlite" ,r-rsqlite)
       ("r-xml" ,r-xml)))
    (home-page "https://github.com/TylerBackman/bioassayR")
    (synopsis "Cross-target analysis of small molecule bioactivity")
    (description
     "bioassayR is a computational tool that enables simultaneous analysis of
thousands of bioassay experiments performed over a diverse set of compounds
and biological targets.  Unique features include support for large-scale
cross-target analyses of both public and custom bioassays, generation of
@dfn{high throughput screening fingerprints} (HTSFPs), and an optional
preloaded database that provides access to a substantial portion of publicly
available bioactivity data.")
    (license license:artistic2.0)))

(define-public r-biobroom
  (package
    (name "r-biobroom")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biobroom" version))
       (sha256
        (base32
         "1480ycdsh9xdhbpr47vdw5g6m8arqsnp8hc19wwhzm8npxh4qqlb"))))
    (properties `((upstream-name . "biobroom")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-broom" ,r-broom)
       ("r-dplyr" ,r-dplyr)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://github.com/StoreyLab/biobroom")
    (synopsis "Turn Bioconductor objects into tidy data frames")
    (description
     "This package contains methods for converting standard objects
constructed by bioinformatics packages, especially those in Bioconductor, and
converting them to @code{tidy} data.  It thus serves as a complement to the
@code{broom} package, and follows the same tidy, augment, glance division of
tidying methods.  Tidying data makes it easy to recombine, reshape and
visualize bioinformatics analyses.")
    ;; Any version of the LGPL.
    (license license:lgpl3+)))

(define-public r-graphite
  (package
    (name "r-graphite")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "graphite" version))
       (sha256
        (base32
         "1r9fk0cpdwm7012pa85dkjcpkml2j89zcznpf4hfdz66anfyyycd"))))
    (properties `((upstream-name . "graphite")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-checkmate" ,r-checkmate)
       ("r-graph" ,r-graph)
       ("r-httr" ,r-httr)
       ("r-rappdirs" ,r-rappdirs)))
    (home-page "https://bioconductor.org/packages/graphite/")
    (synopsis "Networks from pathway databases")
    (description
     "Graphite provides networks derived from eight public pathway databases,
and automates the conversion of node identifiers (e.g. from Entrez IDs to gene
symbols).")
    (license license:agpl3+)))

(define-public r-reactomepa
  (package
    (name "r-reactomepa")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReactomePA" version))
       (sha256
        (base32
         "1vwc9kj1l4yi7c4f4lnq0i3wl2nrmmhcxyakz8qak122fi92z3j1"))))
    (properties `((upstream-name . "ReactomePA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-dose" ,r-dose)
       ("r-enrichplot" ,r-enrichplot)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggraph" ,r-ggraph)
       ("r-graphite" ,r-graphite)
       ("r-igraph" ,r-igraph)
       ("r-reactome-db" ,r-reactome-db)))
    (home-page "https://guangchuangyu.github.io/software/ReactomePA")
    (synopsis "Reactome pathway analysis")
    (description
     "This package provides functions for pathway analysis based on the
REACTOME pathway database.  It implements enrichment analysis, gene set
enrichment analysis and several functions for visualization.")
    (license license:gpl2)))

(define-public r-ebarrays
  (package
    (name "r-ebarrays")
    (version "2.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBarrays" version))
       (sha256
        (base32
         "063rhsdp8x0f881kslq06zxfp6b2qabrz4vmfrn8a4v3pd3n7s13"))))
    (properties `((upstream-name . "EBarrays")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-cluster" ,r-cluster)
       ("r-lattice" ,r-lattice)))
    (home-page "https://bioconductor.org/packages/EBarrays/")
    (synopsis "Gene clustering and differential expression identification")
    (description
     "EBarrays provides tools for the analysis of replicated/unreplicated
microarray data.")
    (license license:gpl2+)))

(define-public r-bioccasestudies
  (package
    (name "r-bioccasestudies")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocCaseStudies" version))
       (sha256
        (base32
         "1sg9vxs24zfz3dg9y0qlrdsq43y0pbahbvcfxzlxjzjw80xzxpbd"))))
    (properties
     `((upstream-name . "BiocCaseStudies")))
    (build-system r-build-system)
    (propagated-inputs `(("r-biobase" ,r-biobase)))
    (home-page "https://bioconductor.org/packages/BiocCaseStudies")
    (synopsis "Support for the case studies monograph")
    (description
     "This package provides software and data to support the case studies
monograph.")
    (license license:artistic2.0)))

(define-public r-biocgraph
  (package
    (name "r-biocgraph")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biocGraph" version))
       (sha256
        (base32
         "1rv2lwiqwg7h7za23n896fs4dpla3xhw6kzwghb6iw5nlm2m61yw"))))
    (properties `((upstream-name . "biocGraph")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-geneplotter" ,r-geneplotter)
       ("r-graph" ,r-graph)
       ("r-rgraphviz" ,r-rgraphviz)))
    (home-page "https://bioconductor.org/packages/biocGraph/")
    (synopsis "Graph examples and use cases in Bioinformatics")
    (description
     "This package provides examples and code that make use of the
different graph related packages produced by Bioconductor.")
    (license license:artistic2.0)))

(define-public r-experimenthub
  (package
    (name "r-experimenthub")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ExperimentHub" version))
       (sha256
        (base32
         "054w2lkyirbmhgia0rp4nk9zzw3zphz6jxg6fc9zlarp90g64z24"))))
    (properties `((upstream-name . "ExperimentHub")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationhub" ,r-annotationhub)
       ("r-biocfilecache" ,r-biocfilecache)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocmanager" ,r-biocmanager)
       ("r-curl" ,r-curl)
       ("r-rappdirs" ,r-rappdirs)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/ExperimentHub/")
    (synopsis "Client to access ExperimentHub resources")
    (description
     "This package provides a client for the Bioconductor ExperimentHub web
resource.  ExperimentHub provides a central location where curated data from
experiments, publications or training courses can be accessed.  Each resource
has associated metadata, tags and date of modification.  The client creates
and manages a local cache of files retrieved enabling quick and reproducible
access.")
    (license license:artistic2.0)))

(define-public r-multiassayexperiment
  (package
    (name "r-multiassayexperiment")
    (version "1.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiAssayExperiment" version))
       (sha256
        (base32
         "01cnp00y5bk551c8gqgqp5468dvccg72i0rvh5cxgbx1c42zy6xn"))))
    (properties
     `((upstream-name . "MultiAssayExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://waldronlab.io/MultiAssayExperiment/")
    (synopsis "Integration of multi-omics experiments in Bioconductor")
    (description
     "MultiAssayExperiment harmonizes data management of multiple assays
performed on an overlapping set of specimens.  It provides a familiar
Bioconductor user experience by extending concepts from
@code{SummarizedExperiment}, supporting an open-ended mix of standard data
classes for individual assays, and allowing subsetting by genomic ranges or
rownames.")
    (license license:artistic2.0)))

(define-public r-bioconcotk
  (package
    (name "r-bioconcotk")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocOncoTK" version))
       (sha256
        (base32
         "0rnah6c01a33yb9663jim9iclan61rpcwprb56mykgn1pf5hywbj"))))
    (properties `((upstream-name . "BiocOncoTK")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bigrquery" ,r-bigrquery)
       ("r-car" ,r-car)
       ("r-complexheatmap" ,r-complexheatmap)
       ("r-curatedtcgadata" ,r-curatedtcgadata)
       ("r-dbi" ,r-dbi)
       ("r-dplyr" ,r-dplyr)
       ("r-dt" ,r-dt)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggpubr" ,r-ggpubr)
       ("r-graph" ,r-graph)
       ("r-httr" ,r-httr)
       ("r-iranges" ,r-iranges)
       ("r-magrittr" ,r-magrittr)
       ("r-plyr" ,r-plyr)
       ("r-rgraphviz" ,r-rgraphviz)
       ("r-rjson" ,r-rjson)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scales" ,r-scales)
       ("r-shiny" ,r-shiny)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/BiocOncoTK")
    (synopsis "Bioconductor components for general cancer genomics")
    (description
     "The purpose of this package is to provide a central interface to various
tools for genome-scale analysis of cancer studies.")
    (license license:artistic2.0)))

(define-public r-biocor
  (package
    (name "r-biocor")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioCor" version))
       (sha256
        (base32
         "1bjw02rwmz2d715sgpfp08njb15200ch7cmipsf9hd5835ppg1jl"))))
    (properties `((upstream-name . "BioCor")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-gseabase" ,r-gseabase)
       ("r-matrix" ,r-matrix)))
    (home-page "https://llrs.github.io/BioCor/")
    (synopsis "Functional similarities")
    (description
     "This package provides tools to calculate functional similarities based
on the pathways described on KEGG and REACTOME or in gene sets.  These
similarities can be calculated for pathways or gene sets, genes, or clusters
and combined with other similarities.  They can be used to improve networks,
gene selection, testing relationships, and so on.")
    (license license:expat)))

(define-public r-biocpkgtools
  (package
    (name "r-biocpkgtools")
    (version "1.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocPkgTools" version))
       (sha256
        (base32
         "0grwnmncmpqcplbfw3j210m1a8f7mmdizklh4zksg4ic21dpjj1a"))))
    (properties `((upstream-name . "BiocPkgTools")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocfilecache" ,r-biocfilecache)
       ("r-biocmanager" ,r-biocmanager)
       ("r-biocviews" ,r-biocviews)
       ("r-dplyr" ,r-dplyr)
       ("r-dt" ,r-dt)
       ("r-gh" ,r-gh)
       ("r-graph" ,r-graph)
       ("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-httr" ,r-httr)
       ("r-igraph" ,r-igraph)
       ("r-jsonlite" ,r-jsonlite)
       ("r-magrittr" ,r-magrittr)
       ("r-rappdirs" ,r-rappdirs)
       ("r-rbgl" ,r-rbgl)
       ("r-readr" ,r-readr)
       ("r-rex" ,r-rex)
       ("r-rlang" ,r-rlang)
       ("r-rvest" ,r-rvest)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-tidyselect" ,r-tidyselect)
       ("r-xml2" ,r-xml2)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/seandavi/BiocPkgTools")
    (synopsis "Collection of tools for learning about Bioconductor packages")
    (description
     "Bioconductor has a rich ecosystem of metadata around packages, usage,
and build status.  This package is a simple collection of functions to access
that metadata from R.  The goal is to expose metadata for data mining and
value-added functionality such as package searching, text mining, and
analytics on packages.")
    (license license:expat)))

(define-public r-biocset
  (package
    (name "r-biocset")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSet" version))
       (sha256
        (base32
         "1xcksnvjflrdarn8xqmgf0n6wbsjkq9jazqwp35i52vqcq4ic1j9"))))
    (properties `((upstream-name . "BiocSet")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-dplyr" ,r-dplyr)
       ("r-keggrest" ,r-keggrest)
       ("r-plyr" ,r-plyr)
       ("r-rlang" ,r-rlang)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-tibble" ,r-tibble)))
    (home-page
     "https://bioconductor.org/packages/BiocSet")
    (synopsis
     "Representing Different Biological Sets")
    (description
     "BiocSet displays different biological sets in a triple tibble format.
These three tibbles are @code{element}, @code{set}, and @code{elementset}.
The user has the ability to activate one of these three tibbles to perform
common functions from the @code{dplyr} package.  Mapping functionality and
accessing web references for elements/sets are also available in BiocSet.")
    (license license:artistic2.0)))

(define-public r-biocworkflowtools
  (package
    (name "r-biocworkflowtools")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocWorkflowTools" version))
       (sha256
        (base32
         "1v4bhnpdkmllm7aghms9b7369hkrgz7mn69wbrqg1x42pgkf30ad"))))
    (properties
     `((upstream-name . "BiocWorkflowTools")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocstyle" ,r-biocstyle)
       ("r-bookdown" ,r-bookdown)
       ("r-git2r" ,r-git2r)
       ("r-httr" ,r-httr)
       ("r-knitr" ,r-knitr)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-stringr" ,r-stringr)
       ("r-usethis" ,r-usethis)))
    (home-page "https://bioconductor.org/packages/BiocWorkflowTools/")
    (synopsis "Tools to aid the development of Bioconductor Workflow packages")
    (description
     "This package provides functions to ease the transition between
Rmarkdown and LaTeX documents when authoring a Bioconductor Workflow.")
    (license license:expat)))

(define-public r-biodist
  (package
    (name "r-biodist")
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioDist" version))
       (sha256
        (base32
         "0iabw07px3ybdgbbab0vv350051cm4aq8w47rz9dnmzx4kil9h5q"))))
    (properties `((upstream-name . "bioDist")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-kernsmooth" ,r-kernsmooth)))
    (home-page "https://bioconductor.org/packages/bioDist/")
    (synopsis "Different distance measures")
    (description
     "This package provides a collection of software tools for calculating
distance measures.")
    (license license:artistic2.0)))
