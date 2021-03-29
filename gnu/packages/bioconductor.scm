;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018, 2020 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 Peter Lo <peterloleungyau@gmail.com>
;;; Copyright © 2020, 2021 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Hong Li <hli@mdc-berlin.de>
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

(define-public r-org-eck12-eg-db
  (package
    (name "r-org-eck12-eg-db")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "org.EcK12.eg.db" version 'annotation))
       (sha256
        (base32 "0c4p6jr83k0gm6pvn760yr8xf33wggrfcr6fg7a42a96bcf817gs"))))
    (properties
     `((upstream-name . "org.EcK12.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://bioconductor.org/packages/org.EcK12.eg.db")
    (synopsis "Genome wide annotation for E coli strain K12")
    (description
     "This package provides genome wide annotation for E coli strain K12,
primarily based on mapping using Entrez Gene identifiers.  Entrez Gene is
National Center for Biotechnology Information (NCBI)’s database for
gene-specific information.  Entrez Gene maintains records from genomes which
have been completely sequenced, which have an active research community to
submit gene-specific information, or which are scheduled for intense sequence
analysis.")
    (license license:artistic2.0)))

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

(define-public r-genomeinfodbdata
  (package
    (name "r-genomeinfodbdata")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomeInfoDbData" version 'annotation))
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

(define-public r-homo-sapiens
  (package
    (name "r-homo-sapiens")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Homo.sapiens" version 'annotation))
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

(define-public r-bsgenome-hsapiens-ucsc-hg38
  (package
    (name "r-bsgenome-hsapiens-ucsc-hg38")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Hsapiens.UCSC.hg38"
                                     version 'annotation))
              (sha256
               (base32
                "1ql08pvi4vv0ynvg4qs9kysw1c7s3crkgin6zxvgzqk6fray9mvi"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.UCSC.hg38")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Hsapiens.UCSC.hg38/")
    (synopsis "Full genome sequences for Homo sapiens")
    (description
     "This package provides full genome sequences for Homo sapiens (Human)
as provided by UCSC (hg38, Dec. 2013) and stored in Biostrings objects.")
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

(define-public r-txdb-dmelanogaster-ucsc-dm6-ensgene
  (package
    (name "r-txdb-dmelanogaster-ucsc-dm6-ensgene")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TxDb.Dmelanogaster.UCSC.dm6.ensGene"
                              version 'annotation))
       (sha256
        (base32
         "0yij7zyqkmmr13389rs2gfa5anvvw648nnl1kjbsgvyxkggif8q4"))))
    (properties
     `((upstream-name . "TxDb.Dmelanogaster.UCSC.dm6.ensGene")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-genomicfeatures" ,r-genomicfeatures)))
    (home-page
     "https://bioconductor.org/packages/TxDb.Dmelanogaster.UCSC.dm6.ensGene")
    (synopsis "Annotation package for TxDb object(s)")
    (description
     "This package exposes an annotation databases generated from UCSC by
exposing these as TxDb objects.")
    (license license:artistic2.0)))

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

(define-public r-chromstardata
  (package
    (name "r-chromstardata")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromstaRData" version 'experiment))
       (sha256
        (base32
         "0ph80d53598635bb8g61acg5rqwnj8644a0gh297r4hgbvwlflab"))))
    (properties `((upstream-name . "chromstaRData")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/chromstaRData/")
    (synopsis "ChIP-seq data for demonstration purposes")
    (description
     "This package provides ChIP-seq data for demonstration purposes in the
chromstaR package.")
    (license license:gpl3)))

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

(define-public r-pasilla
  (package
    (name "r-pasilla")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://bioconductor.org/packages/release/data/experiment"
                    "/src/contrib/pasilla_" version ".tar.gz"))
              (sha256
               (base32
                "0h124i2fb2lbj2k48zzf1n7ldqa471bs26fbd9vw50299aqx28x0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocstyle" ,r-biocstyle)
       ("r-dexseq" ,r-dexseq)
       ("r-knitr" ,r-knitr)
       ("r-rmarkdown" ,r-rmarkdown)))
    (home-page "https://www.bioconductor.org/packages/pasilla/")
    (synopsis "Data package with per-exon and per-gene read counts")
    (description "This package provides per-exon and per-gene read counts
computed for selected genes from RNA-seq data that were presented in the
article 'Conservation of an RNA regulatory map between Drosophila and mammals'
by Brooks et al., Genome Research 2011.")
    (license license:lgpl2.1+)))

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

(define-public r-gagedata
  (package
    (name "r-gagedata")
    (version "2.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gageData" version 'experiment))
       (sha256
        (base32 "16lmnvmbykvbdgwyx7r2jc217gb9nidn81860v5kri99g97j4jdn"))))
    (properties `((upstream-name . "gageData")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/gageData")
    (synopsis "Auxiliary data for the gage package")
    (description
     "This is a supportive data package for the software package @code{gage}.
However, the data supplied here are also useful for gene set or pathway
analysis or microarray data analysis in general.  In this package, we provide
two demo microarray dataset: GSE16873 (a breast cancer dataset from GEO) and
BMP6 (originally published as an demo dataset for GAGE, also registered as
GSE13604 in GEO).  This package also includes commonly used gene set data based
on KEGG pathways and GO terms for major research species, including human,
mouse, rat and budding yeast.  Mapping data between common gene IDs for budding
yeast are also included.")
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
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocVersion" version))
       (sha256
        (base32
         "1cj9swyf4zbkdq45bhw0kymh2aghkwimxjlfj5r2j7kdiyh6n3rk"))))
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
    (version "0.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocGenerics" version))
              (sha256
               (base32
                "1y9pgangz2f0n9v3zj8brz8bv7a1yjsncdnd5h1l7zv0c0j9fh9q"))))
    (properties
     `((upstream-name . "BiocGenerics")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BiocGenerics")
    (synopsis "S4 generic functions for Bioconductor")
    (description
     "This package provides S4 generic functions needed by many Bioconductor
packages.")
    (license license:artistic2.0)))

(define-public r-coverageview
  (package
    (name "r-coverageview")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "CoverageView" version))
              (sha256
               (base32
                "1k89gzqhd8ca8s9gk5bfzringnc5nayqbwzwwy35fls1cg96qmsj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-s4vectors" ,r-s4vectors)
       ("r-iranges" ,r-iranges)
       ("r-genomicranges" ,r-genomicranges)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rsamtools" ,r-rsamtools)))
    (home-page "https://bioconductor.org/packages/CoverageView/")
    (synopsis "Coverage visualization package for R")
    (description "This package provides a framework for the visualization of
genome coverage profiles.  It can be used for ChIP-seq experiments, but it can
be also used for genome-wide nucleosome positioning experiments or other
experiment types where it is important to have a framework in order to inspect
how the coverage distributed across the genome.")
    (license license:artistic2.0)))

(define-public r-cummerbund
  (package
   (name "r-cummerbund")
   (version "2.32.0")
   (source (origin
             (method url-fetch)
             (uri (bioconductor-uri "cummeRbund" version))
             (sha256
              (base32
               "1x7rby50api1c66al6a0i92q82ydjmh3h8l2k7hj0ffpn8c5pdgj"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biobase" ,r-biobase)
      ("r-biocgenerics" ,r-biocgenerics)
      ("r-fastcluster" ,r-fastcluster)
      ("r-ggplot2" ,r-ggplot2)
      ("r-gviz" ,r-gviz)
      ("r-plyr" ,r-plyr)
      ("r-reshape2" ,r-reshape2)
      ("r-rsqlite" ,r-rsqlite)
      ("r-rtracklayer" ,r-rtracklayer)
      ("r-s4vectors" ,r-s4vectors)))
   (home-page "https://bioconductor.org/packages/cummeRbund/")
   (synopsis "Analyze Cufflinks high-throughput sequencing data")
   (description "This package allows for persistent storage, access,
exploration, and manipulation of Cufflinks high-throughput sequencing
data.  In addition, provides numerous plotting functions for commonly
used visualizations.")
   (license license:artistic2.0)))

(define-public r-delayedarray
  (package
    (name "r-delayedarray")
    (version "0.16.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DelayedArray" version))
              (sha256
               (base32
                "0w1wppy6m2iv41852dscg3y19sq84ahdx3m7c2p2pxjcznmv6hys"))))
    (properties
     `((upstream-name . "DelayedArray")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-s4vectors" ,r-s4vectors)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-matrixgenerics" ,r-matrixgenerics)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-bluster
  (package
   (name "r-bluster")
   (version "1.0.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "bluster" version))
            (sha256
             (base32
              "0izrf82m5znyrgai5y5jss4k2brabh4ajxdvnlwwc92l5bw7jp61"))))
   (properties `((upstream-name . "bluster")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biocneighbors" ,r-biocneighbors)
      ("r-biocparallel" ,r-biocparallel)
      ("r-igraph" ,r-igraph)
      ("r-matrix" ,r-matrix)
      ("r-rcpp" ,r-rcpp)
      ("r-s4vectors" ,r-s4vectors)))
   (native-inputs
    `(("r-knitr" ,r-knitr)))
   (home-page "https://bioconductor.org/packages/bluster")
   (synopsis "Clustering algorithms for Bioconductor")
   (description"This package wraps common clustering algorithms in an easily
extended S4 framework.  Backends are implemented for hierarchical, k-means
and graph-based clustering.  Several utilities are also provided to compare
and evaluate clustering results.")
   (license license:gpl3)))

(define-public r-ideoviz
  (package
    (name "r-ideoviz")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IdeoViz" version))
              (sha256
               (base32
                "1k5c0skr6pvpcvkak9f0a088w5wsx4fl3jb9a76gyyni4nkj7djq"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-iranges" ,r-iranges)
       ("r-genomicranges" ,r-genomicranges)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-genomeinfodb" ,r-genomeinfodb)))
    (home-page "https://bioconductor.org/packages/IdeoViz/")
    (synopsis "Plots data along a chromosomal ideogram")
    (description "This package provides functions to plot data associated with
arbitrary genomic intervals along chromosomal ideogram.")
    (license license:gpl2)))

(define-public r-iranges
  (package
    (name "r-iranges")
    (version "2.24.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IRanges" version))
              (sha256
               (base32
                "01mx46a82vd3gz705pj0kk4wpxg683s8jqxchzjia3gz00b4qw52"))))
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

;; This is a CRAN package, but it depends on r-biobase and r-limma from Bioconductor.
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

;; This is a CRAN package, but it depends on r-biobase from Bioconductor.
(define-public r-bisquerna
  (package
   (name "r-bisquerna")
   (version "1.0.4")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "BisqueRNA" version))
            (sha256
             (base32
              "01g34n87ml7n3pck77497ddgbv3rr5p4153ac8ninpgjijlm3jw2"))))
   (properties `((upstream-name . "BisqueRNA")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biobase" ,r-biobase)
      ("r-limsolve" ,r-limsolve)))
   (home-page "https://www.biorxiv.org/content/10.1101/669911v1")
   (synopsis "Decomposition of bulk expression with single-cell sequencing")
   (description "This package provides tools to accurately estimate cell type
abundances from heterogeneous bulk expression.  A reference-based method
utilizes single-cell information to generate a signature matrix and
transformation of bulk expression for accurate regression based estimates.
A marker-based method utilizes known cell-specific marker genes to measure
relative abundances across samples.")
   (license license:gpl3)))

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
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "NMF" version))
       (sha256
        (base32
         "0ls7q9yc9l1z10jphq5a11wkfgcxc3gm3sfjj376zx3vnc0wl30g"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-affycomp
  (package
    (name "r-affycomp")
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycomp" version))
       (sha256
        (base32
         "106gz4rami04r0ffc7rxkrv92s3yrcnlqnyykd5s8lrkndbihrpk"))))
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
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AffyCompatible" version))
       (sha256
        (base32
         "0pcs51miy45bky26i1d4iarbjh569gssb5g4fr26bzgjmq19yl7x"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyContam" version))
       (sha256
        (base32
         "0pi5fll5868sb80vb9kbymz6gkjv58f0abk6zwn407cnyjhr342b"))))
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
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycoretools" version))
       (sha256
        (base32
         "0jacgwylg7wjw3xk3ga2sb1wkdklm5glamhbmqgvzm5kdjnl0rv0"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/affycoretools/")
    (synopsis "Functions for analyses with Affymetrix GeneChips")
    (description
     "This package provides various wrapper functions that have been written
to streamline the more common analyses that a Biostatistician might see.")
    (license license:artistic2.0)))

(define-public r-affxparser
  (package
    (name "r-affxparser")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affxparser" version))
       (sha256
        (base32
         "13h4iwskvgwgxid9f60gzb1zndgbhdlbn9ixv5waihy1jkcbn24p"))))
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
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotate" version))
       (sha256
        (base32
         "1rql591x56532m8n4axdkfkhkbcsz5hfrf7271s0lmkvy84i7z6l"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-dbi" ,r-dbi)
       ("r-httr" ,r-httr)
       ("r-xml" ,r-xml)
       ("r-xtable" ,r-xtable)))
    (home-page
     "https://bioconductor.org/packages/annotate")
    (synopsis "Annotation for microarrays")
    (description "This package provides R environments for the annotation of
microarrays.")
    (license license:artistic2.0)))

(define-public r-annotationdbi
  (package
    (name "r-annotationdbi")
    (version "1.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationDbi" version))
              (sha256
               (base32
                "0zqxgh3nx6y8ry12s2vss2f4axz5vpqxha1y4ifhhcx4zhpzsglr"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/AnnotationDbi")
    (synopsis "Annotation database interface")
    (description
     "This package provides user interface and database connection code for
annotation data packages using SQLite data storage.")
    (license license:artistic2.0)))

(define-public r-annotationforge
  (package
    (name "r-annotationforge")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationForge" version))
       (sha256
        (base32
         "0y3820dkvwz09wlmz9drx6gqpsr9cwppaiz40zafwfxbz65y8px7"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/AnnotationForge")
    (synopsis "Code for building annotation database packages")
    (description
     "This package provides code for generating Annotation packages and their
databases.  Packages produced are intended to be used with AnnotationDbi.")
    (license license:artistic2.0)))

(define-public r-biobase
  (package
    (name "r-biobase")
    (version "2.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biobase" version))
              (sha256
               (base32
                "11kgc4flywlm3i18603558l8ksv91c24vkc5fnnbcd375i2dhhd4"))))
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

(define-public r-biomart
  (package
    (name "r-biomart")
    (version "2.46.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biomaRt" version))
              (sha256
               (base32
                "0gwmd0ykpv0gyh34c56g5m12lil20fvig49f3ih1jxrxf3q4wmq7"))))
    (properties
     `((upstream-name . "biomaRt")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocfilecache" ,r-biocfilecache)
       ("r-httr" ,r-httr)
       ("r-openssl" ,r-openssl)
       ("r-progress" ,r-progress)
       ("r-rappdirs" ,r-rappdirs)
       ("r-stringr" ,r-stringr)
       ("r-xml" ,r-xml)
       ("r-xml2" ,r-xml2)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.24.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocParallel" version))
              (sha256
               (base32
                "1iryicvmcagcrj29kp49mqhiq2kn72j4idj380hi9illmdrg9ism"))))
    (properties
     `((upstream-name . "BiocParallel")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             ;; Remove generated documentation.
             (for-each delete-file
                       '("inst/doc/BiocParallel_BatchtoolsParam.pdf"
                         "inst/doc/Introduction_To_BiocParallel.pdf"
                         "inst/doc/Errors_Logs_And_Debugging.pdf"
                         "inst/doc/BiocParallel_BatchtoolsParam.R"
                         "inst/doc/Introduction_To_BiocParallel.R"
                         "inst/doc/Errors_Logs_And_Debugging.R"))

             ;; Remove time-dependent macro
             (substitute* '("inst/doc/BiocParallel_BatchtoolsParam.Rnw"
                            "inst/doc/Introduction_To_BiocParallel.Rnw"
                            "inst/doc/Errors_Logs_And_Debugging.Rnw"
                            "vignettes/BiocParallel_BatchtoolsParam.Rnw"
                            "vignettes/Introduction_To_BiocParallel.Rnw"
                            "vignettes/Errors_Logs_And_Debugging.Rnw")
               (("\\today") "later"))

             ;; Initialize the random number generator seed when building.
             (substitute* "R/internal_rng_stream.R"
               (("\"L'Ecuyer-CMRG\"\\)" m)
                (string-append
                 m "; if (!is.na(Sys.getenv(\"SOURCE_DATE_EPOCH\"))) {set.seed(100)}\n"))))))))
    (propagated-inputs
     `(("r-futile-logger" ,r-futile-logger)
       ("r-snow" ,r-snow)
       ("r-bh" ,r-bh)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.58.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biostrings" version))
              (sha256
               (base32
                "1rbqhs73mhfr1gi0rx28jiyan7i3hb45ai3jpl1656fnrhgjfxq5"))))
    (properties
     `((upstream-name . "Biostrings")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-crayon" ,r-crayon)
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

(define-public r-category
  (package
    (name "r-category")
    (version "2.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Category" version))
       (sha256
        (base32
         "0m77wpnica0h2ia9ajdaiga4plgz1s9wls6pdnxzk7kwl8a68wkr"))))
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

(define-public r-deseq2
  (package
    (name "r-deseq2")
    (version "1.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq2" version))
       (sha256
        (base32
         "1i0jpzsm1vl7q6qdmplj45w13lsaycxrx5pazlanjba2khn79k19"))))
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
       ("r-iranges" ,r-iranges)
       ("r-locfit" ,r-locfit)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DEXSeq" version))
       (sha256
        (base32
         "0wfjb42xcr4wjy8a654b74411dky8hp6sp8xdwf0sxqgsxy106qi"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-edger
  (package
    (name "r-edger")
    (version "3.32.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "edgeR" version))
              (sha256
               (base32
                "1gaic8qf6a6sy0bmydh1xzf52w0wnq31aanpvw3a30pfsi218bcp"))))
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

(define-public r-genefilter
  (package
    (name "r-genefilter")
    (version "1.72.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "genefilter" version))
       (sha256
        (base32
         "1c6h3qnjvphs977qhv5vafvsb108r0q7xhaayly6qv6adqfn94rn"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)
       ("r-knitr" ,r-knitr)))
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-survival" ,r-survival)))
    (home-page "https://bioconductor.org/packages/genefilter")
    (synopsis "Filter genes from high-throughput experiments")
    (description
     "This package provides basic functions for filtering genes from
high-throughput sequencing experiments.")
    (license license:artistic2.0)))

(define-public r-genomeinfodb
  (package
    (name "r-genomeinfodb")
    (version "1.26.6")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomeInfoDb" version))
              (sha256
               (base32
                "1wy4dwiv0pgim975var802z565py4a0nakx6zdvbhry4c0dfczd1"))))
    (properties
     `((upstream-name . "GenomeInfoDb")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomeinfodbdata" ,r-genomeinfodbdata)
       ("r-iranges" ,r-iranges)
       ("r-rcurl" ,r-rcurl)
       ("r-s4vectors" ,r-s4vectors)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/GenomeInfoDb")
    (synopsis "Utilities for manipulating chromosome identifiers")
    (description
     "This package contains data and functions that define and allow
translation between different chromosome sequence naming conventions (e.g.,
\"chr1\" versus \"1\"), including a function that attempts to place sequence
names in their natural, rather than lexicographic, order.")
    (license license:artistic2.0)))

(define-public r-genomicranges
  (package
    (name "r-genomicranges")
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicRanges" version))
              (sha256
               (base32
                "0j4py5g6pdj35xhlaqhxxhg55j9l4mcdk3yck4dgyavv5f2dh24i"))))
    (properties
     `((upstream-name . "GenomicRanges")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/GenomicRanges")
    (synopsis "Representation and manipulation of genomic intervals")
    (description
     "This package provides tools to efficiently represent and manipulate
genomic annotations and alignments is playing a central role when it comes to
analyzing high-throughput sequencing data (a.k.a. NGS data).  The
GenomicRanges package defines general purpose containers for storing and
manipulating genomic intervals and variables defined along a genome.")
    (license license:artistic2.0)))

(define-public r-gostats
  (package
    (name "r-gostats")
    (version "2.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOstats" version))
       (sha256
        (base32
         "18q8p0fv9fl2r6zjxknfjwqxr69dlyxy6c8amzn6c6dwjq1cxk6j"))))
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

(define-public r-gseabase
  (package
    (name "r-gseabase")
    (version "1.52.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GSEABase" version))
       (sha256
        (base32
         "0dawh1kjmf6921jm77j2s2phrq5237pjc4sdh8fkln89gf48zx6i"))))
    (properties `((upstream-name . "GSEABase")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-graph" ,r-graph)
       ("r-xml" ,r-xml)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/GSEABase")
    (synopsis "Gene set enrichment data structures and methods")
    (description
     "This package provides classes and methods to support @dfn{Gene Set
Enrichment Analysis} (GSEA).")
    (license license:artistic2.0)))

(define-public r-hpar
  (package
    (name "r-hpar")
    (version "1.32.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hpar" version))
       (sha256
        (base32
         "0h10b0fyblpsnxj60rpbk99z7snrkkb5jssmf0v27s6d445jq2zr"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/hpar/")
    (synopsis "Human Protein Atlas in R")
    (description "This package provides a simple interface to and data from
the Human Protein Atlas project.")
    (license license:artistic2.0)))

(define-public r-limma
  (package
    (name "r-limma")
    (version "3.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "limma" version))
              (sha256
               (base32
                "1xxv493q1kip9bjfv7v7k5dnq7hz7gvl80i983v4mvwavhgnbxfz"))))
    (build-system r-build-system)
    (home-page "http://bioinf.wehi.edu.au/limma")
    (synopsis "Package for linear models for microarray and RNA-seq data")
    (description "This package can be used for the analysis of gene expression
studies, especially the use of linear models for analysing designed experiments
and the assessment of differential expression.  The analysis methods apply to
different technologies, including microarrays, RNA-seq, and quantitative PCR.")
    (license license:gpl2+)))

(define-public r-rbgl
  (package
    (name "r-rbgl")
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RBGL" version))
       (sha256
        (base32
         "016vyzgixb3gjpzi21rbs6ngnnqcxr77krwjjf1ldnzzj8vqrqsz"))))
    (properties `((upstream-name . "RBGL")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-graph" ,r-graph)))
    (home-page "https://www.bioconductor.org/packages/RBGL")
    (synopsis "Interface to the Boost graph library")
    (description
     "This package provides a fairly extensive and comprehensive interface to
the graph algorithms contained in the Boost library.")
    (license license:artistic2.0)))

(define-public r-regioner
  (package
    (name "r-regioner")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "regioneR" version))
       (sha256
        (base32
         "0c2khfyrgy3y68cj6b07s91hplabbj70xwdgwrf2bsd9h6gknjdi"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/regioneR/")
    (synopsis "Association analysis of genomic regions")
    (description "This package offers a statistical framework based on
customizable permutation tests to assess the association between genomic
region sets and other genomic features.")
    (license license:artistic2.0)))

(define-public r-reportingtools
  (package
    (name "r-reportingtools")
    (version "2.30.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReportingTools" version))
       (sha256
        (base32
         "1vvra7l29s7lnq996nwlpzbkrbdkr3ivkgmfp4kndfykxsl9q4vb"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-rsamtools
  (package
    (name "r-rsamtools")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Rsamtools" version))
              (sha256
               (base32
                "040pggkwglc6wy90qnc7xcdnaj0v3iqlykvvsl74241409qly554"))))
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
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-bitops" ,r-bitops)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rhtslib" ,r-rhtslib)
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

(define-public r-shortread
  (package
    (name "r-shortread")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ShortRead" version))
       (sha256
        (base32
         "0w4m8d3h660mmr2ymp206r1n4aqssxmkv8yxkbr5y1swrahxzfk9"))))
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
       ("r-rhtslib" ,r-rhtslib)
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
    (version "1.24.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "systemPipeR" version))
       (sha256
        (base32
         "0ffazyl2q9plbhwlxi04s3fvnli6qj95n7bkjc21535bbi08xfki"))))
    (properties `((upstream-name . "systemPipeR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-assertthat" ,r-assertthat)
       ("r-batchtools" ,r-batchtools)
       ("r-biostrings" ,r-biostrings)
       ("r-deseq2" ,r-deseq2)
       ("r-dot" ,r-dot)
       ("r-edger" ,r-edger)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-go-db" ,r-go-db)
       ("r-gostats" ,r-gostats)
       ("r-iranges" ,r-iranges)
       ("r-limma" ,r-limma)
       ("r-magrittr" ,r-magrittr)
       ("r-pheatmap" ,r-pheatmap)
       ("r-rjson" ,r-rjson)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rsvg" ,r-rsvg)
       ("r-shortread" ,r-shortread)
       ("r-stringr" ,r-stringr)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-yaml" ,r-yaml)
       ("r-variantannotation" ,r-variantannotation)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-variantannotation
  (package
    (name "r-variantannotation")
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "VariantAnnotation" version))
              (sha256
               (base32
                "1sl0l6v05lfglj281nszma0h5k234md7rn2pdah8vs2d4iq3kimw"))))
    (properties
     `((upstream-name . "VariantAnnotation")))
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
       ("r-matrixgenerics" ,r-matrixgenerics)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-rhtslib" ,r-rhtslib)
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

(define-public r-xvector
  (package
    (name "r-xvector")
    (version "0.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "XVector" version))
              (sha256
               (base32
                "1pqljikg4f6jb7wgm5537zwgq5b013nyz1agjrwfq2cljb0ym6lq"))))
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

(define-public r-geneplotter
  (package
    (name "r-geneplotter")
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geneplotter" version))
       (sha256
        (base32
         "1f8nr60n1nig1gdy85wqdhpzxvp9r4chygxm8xpy882mdvfv6rqx"))))
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
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligoClasses" version))
       (sha256
        (base32
         "19p6h0cgnma5md5mm9bn6rxfhr0a9znljgdbvsqybms6asvh18gy"))))
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
    (version "1.54.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligo" version))
       (sha256
        (base32
         "0cpfkvxpni7an361li0k0qlfcraj7z9zv71r25dbly5kp3dql7k3"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "qvalue" version))
       (sha256
        (base32
         "0xbww16lz0k2p4mmq1aqd7iz7d8rvzgw1gm55jy6xbx19ymj64i5"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-reshape2" ,r-reshape2)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/StoreyLab/qvalue")
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

(define r-rcppnumerical
  (package
   (name "r-rcppnumerical")
   (version "0.4-0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "RcppNumerical" version))
            (sha256
             (base32
              "1a92fql6mijhnr1kxkcxwivf95pk9lhgmhzkshs51h0ybfv5krik"))))
   (properties `((upstream-name . "RcppNumerical")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-rcpp" ,r-rcpp)
      ("r-rcppeigen" ,r-rcppeigen)))
   (native-inputs
    `(("r-knitr" ,r-knitr)))
   (home-page "https://github.com/yixuan/RcppNumerical")
   (synopsis "Rcpp integration for numerical computing libraries")
   (description "This package provides a collection of open source libraries
for numerical computing (numerical integration, optimization, etc.) and their
integration with @code{Rcpp}.")
   (license license:gpl2+)))

(define-public r-apeglm
  (package
   (name "r-apeglm")
   (version "1.12.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "apeglm" version))
            (sha256
             (base32
              "0pix1fhxk2q89p2745fgsmxwics9rf10l392qhw3rw6v6ynhims2"))))
   (properties `((upstream-name . "apeglm")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-emdbook" ,r-emdbook)
      ("r-genomicranges" ,r-genomicranges)
      ("r-rcpp" ,r-rcpp)
      ("r-rcppeigen" ,r-rcppeigen)
      ("r-rcppnumerical" ,r-rcppnumerical)
      ("r-summarizedexperiment" ,r-summarizedexperiment)))
   (native-inputs `(("r-knitr" ,r-knitr)))
   (home-page "https://bioconductor.org/packages/apeglm")
   (synopsis "Approximate posterior estimation for GLM coefficients")
   (description "This package provides Bayesian shrinkage estimators for
effect sizes for a variety of GLM models, using approximation of the
posterior for individual coefficients.")
   (license license:gpl2)))

(define-public r-greylistchip
  (package
   (name "r-greylistchip")
   (version "1.22.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "GreyListChIP" version))
            (sha256
             (base32
              "1d1yvza1aw3vs3di6mrra5l52ig0p9bpzprrqvknjaz5i4yb8ma6"))))
   (properties `((upstream-name . "GreyListChIP")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-bsgenome" ,r-bsgenome)
      ("r-genomeinfodb" ,r-genomeinfodb)
      ("r-genomicalignments" ,r-genomicalignments)
      ("r-genomicranges" ,r-genomicranges)
      ("r-mass" ,r-mass)
      ("r-rsamtools" ,r-rsamtools)
      ("r-rtracklayer" ,r-rtracklayer)
      ("r-summarizedexperiment" ,r-summarizedexperiment)))
   (home-page "https://bioconductor.org/packages/GreyListChIP")
   (synopsis "Greylist artefact regions based on ChIP inputs")
   (description "This package identifies regions of ChIP experiments with high
signal in the input, that lead to spurious peaks during peak calling.")
   (license license:artistic2.0)))

(define-public r-diffbind
  (package
    (name "r-diffbind")
    (version "3.0.15")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DiffBind" version))
       (sha256
        (base32
         "06f613s8d9z51njyf839g22gwybx9zs5n6xghwr5j1ad2n4m6qwi"))))
    (properties `((upstream-name . "DiffBind")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-amap" ,r-amap)
       ("r-apeglm" ,r-apeglm)
       ("r-ashr" ,r-ashr)
       ("r-biocparallel" ,r-biocparallel)
       ("r-deseq2" ,r-deseq2)
       ("r-dplyr" ,r-dplyr)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-gplots" ,r-gplots)
       ("r-greylistchip" ,r-greylistchip)
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
    (version "2.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "multtest" version))
       (sha256
        (base32
         "06vixd81nh3nxrc6km73p7c4bwln1zm39fa9gp7gj272vsxkx53q"))))
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
    (version "1.68.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "graph" version))
              (sha256
               (base32
                "0wr7j2pasvi3srvg9z3n034ljk8mldcixny6b3kmqbqm8dqy9py4"))))
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

;; This is a CRAN package, but it depends on a Bioconductor package, r-graph.
(define-public r-perfmeas
  (package
    (name "r-perfmeas")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "PerfMeas" version))
       (sha256
        (base32
         "1x7ancmb41zd1js24rx94plgbssyc71z2bvpic6mg34xjkwdjw93"))))
    (properties `((upstream-name . "PerfMeas")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-graph" ,r-graph)
       ("r-limma" ,r-limma)
       ("r-rbgl" ,r-rbgl)))
    (home-page "https://cran.r-project.org/web/packages/PerfMeas/")
    (synopsis "Performance measures for ranking and classification tasks")
    (description
     "This package implements different performance measures for
classification and ranking tasks.  @dfn{Area under curve} (AUC), precision at
a given recall, F-score for single and multiple classes are available.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on a Bioconductor package.
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
    (version "3.24.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPpeakAnno" version))
       (sha256
        (base32
         "0l417aygs89wf1j9fjpfjhahzskbpbgcrm8xpx3qm4s0307vfzkw"))))
    (properties `((upstream-name . "ChIPpeakAnno")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biomart" ,r-biomart)
       ("r-biostrings" ,r-biostrings)
       ("r-dbi" ,r-dbi)
       ("r-dplyr" ,r-dplyr)
       ("r-ensembldb" ,r-ensembldb)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-graph" ,r-graph)
       ("r-iranges" ,r-iranges)
       ("r-keggrest" ,r-keggrest)
       ("r-matrixstats" ,r-matrixstats)
       ("r-multtest" ,r-multtest)
       ("r-rbgl" ,r-rbgl)
       ("r-regioner" ,r-regioner)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-venndiagram" ,r-venndiagram)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-matrixgenerics
  (package
   (name "r-matrixgenerics")
   (version "1.2.1")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MatrixGenerics" version))
            (sha256
             (base32
              "163f0z33cv6038gcjdxn1hadcg9b09qgvm6zc5zn97y4rc8grkrb"))))
   (properties
    `((upstream-name . "MatrixGenerics")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-matrixstats" ,r-matrixstats)))
   (home-page "https://bioconductor.org/packages/MatrixGenerics")
   (synopsis "S4 generic summary statistic functions for matrix-like objects")
   (description
    "This package provides S4 generic functions modeled after the
@code{matrixStats} API for alternative matrix implementations.  Packages with
alternative matrix implementation can depend on this package and implement the
generic functions that are defined here for a useful set of row and column
summary statistics.  Other package developers can import this package and
handle a different matrix implementations without worrying about
incompatibilities.")
   (license license:artistic2.0)))

(define-public r-marray
  (package
    (name "r-marray")
    (version "1.68.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "marray" version))
              (sha256
               (base32 "1kkgv166gzvlj8p58vzam3hcaz8mypi3hhpdsjhaszwg6nav4ray"))))
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
   (version "1.50.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHbase" version))
            (sha256
             (base32 "10zhjmls3f63cj0bnywjb97zhrj7x3xsq6yjhvf5cclxc4kcrcx4"))))
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
   (version "2.52.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHcall" version))
            (sha256
             (base32 "1a6k87xfm79wnsc30k5aziakv51h4dd9zqw81q8bd72hc3fpz8ba"))))
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
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "QDNAseq" version))
              (sha256
               (base32 "1njka1ldaj12id3m2z8ghlrm2lg0n5pxsxyv5gpjnsiabnnaw6ph"))))
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
    (version "2.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "baySeq" version))
       (sha256
        (base32
         "1496inlw0x4mfy3g2v7j9ips96sf7576ydnfn6hvn2m6rz2ls215"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPComp" version))
       (sha256
        (base32
         "0dbypfgys74snmyf982183ilzg6vamfw1d5y0lp5p8zxbffh2xl7"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RiboProfiling" version))
       (sha256
        (base32
         "112071w7aw7cwckipq0dll1lssl7pwafma4v9jj9sx12rjcj57xg"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "riboSeqR" version))
       (sha256
        (base32
         "07i64gch14rsbjlfv17s689wzlqbi7hcqhcw21pp6cw8bvhvd5xr"))))
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
  (package                              ;BROKEN
    (name "r-interactionset")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "InteractionSet" version))
       (sha256
        (base32
         "14lp23b298wr3r7ggcfvas0xlf1866cpla0rv7dz589f50z6bj31"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicInteractions" version))
       (sha256
        (base32
         "0ad0a5cadchx1rkqj4cc8k0y1zf34jgp1406hvik5zabr7xijkbd"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ctc" version))
       (sha256
        (base32
         "1nwlphbfba3w8ixck02k5c84qm4flnp9fd68li0jn5a08qi9gmyp"))))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "goseq" version))
       (sha256
        (base32
         "18fs3m4kl3zahn42j20rjvxy83irscgqx0dvid7va4majvsib509"))))
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
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Glimma" version))
       (sha256
        (base32
         "0gy30v30lw27frhmw39pzacqzrv2vwj5rsp6gb3yifllrahdiffv"))))
    (properties `((upstream-name . "Glimma")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-deseq2" ,r-deseq2)
       ("r-edger" ,r-edger)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-jsonlite" ,r-jsonlite)
       ("r-limma" ,r-limma)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROTS" version))
       (sha256
        (base32
         "0qk0gfhgr14g13zlfyf5101b5s8cma7j3r8a92q93h0axy8ka23n"))))
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
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "plgem" version))
       (sha256
        (base32
         "039gqwsm1v6q8v8b248nm8g9gnsk379mfx65rbgdmh3chsd8pm8a"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "INSPEcT" version))
       (sha256
        (base32
         "1jymvi5mf7vhs58zfh290pacfswgvkw09rmbirmr24kxcgl30483"))))
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
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shiny" ,r-shiny)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-txdb-mmusculus-ucsc-mm9-knowngene"
        ,r-txdb-mmusculus-ucsc-mm9-knowngene)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNABarcodes" version))
       (sha256
        (base32
         "0zzf6xgg6k1gdig8zvpawck2bgmamsc0k43j4pl4xsz9an6dmzbg"))))
    (properties `((upstream-name . "DNABarcodes")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RUVSeq" version))
       (sha256
        (base32
         "1anrybyrzpajr5434svyfbaypjai6x0ifsmqvjgimmxq3xqhv0jh"))))
    (properties `((upstream-name . "RUVSeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-edaseq" ,r-edaseq)
       ("r-edger" ,r-edger)
       ("r-mass" ,r-mass)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocNeighbors" version))
       (sha256
        (base32
         "19gyl917lf5ydy5hgj0hnc388rw5sbj83awav9js2yr2zmbgn4d7"))))
    (properties `((upstream-name . "BiocNeighbors")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpphnsw" ,r-rcpphnsw)
       ("r-s4vectors" ,r-s4vectors)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSingular" version))
       (sha256
        (base32
         "1hczix1h14d19hzcsngqkqqnqkprs41phzlcird8haxnw9bs03ni"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "destiny" version))
       (sha256
        (base32
         "1i7f5q02zvpfaxhd76fbw0h1wfgjphyn5hrmrjpvlnv4ardzsir2"))))
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
    (native-inputs
     `(("r-nbconvertr" ,r-nbconvertr))) ; for vignettes
    (home-page "https://bioconductor.org/packages/destiny/")
    (synopsis "Create and plot diffusion maps")
    (description "This package provides tools to create and plot diffusion
maps.")
    ;; Any version of the GPL
    (license license:gpl3+)))

(define-public r-savr
  (package
    (name "r-savr")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "savR" version))
       (sha256
        (base32
         "1vha9b7gndwjzvrzr1hdhv3wc6a1s2n9grxwfd78yb2lkysf4hic"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPexoQual" version))
       (sha256
        (base32
         "15r5jgkfwwfqpw4v4q2ddmglm3bfw002nnbnzn1s0v2b1w3bgiag"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/keleslab/ChIPexoQual")
    (synopsis "Quality control pipeline for ChIP-exo/nexus data")
    (description
     "This package provides a quality control pipeline for ChIP-exo/nexus
sequencing data.")
    (license license:gpl2+)))

(define-public r-copynumber
  (package
    (name "r-copynumber")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "copynumber" version))
              (sha256
               (base32
                "00fyfy3kpz33v1hqisd5m5xdazwjmjrfj8ssbf6p9m3am2ar23gm"))))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNAcopy" version))
       (sha256
        (base32
         "0km5af4iw8a0m6by933lgdi5246jafyfxk6fsqdiwg07v9wxw5hc"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBSeq" version))
       (sha256
        (base32
         "1x2489xaqg85v7n3yhqs0nh9ha6dn4m167dkc6akzig4xivwjjny"))))
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

(define-public r-karyoploter
  (package
    (name "r-karyoploter")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "karyoploteR" version))
              (sha256
               (base32
                "1agw49mckm3g33igqdp9lr8a4ky8nhivaxrs7d00dvzk0diqwdb2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-bamsignals" ,r-bamsignals)
       ("r-bezier" ,r-bezier)
       ("r-biovizbase" ,r-biovizbase)
       ("r-digest" ,r-digest)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-memoise" ,r-memoise)
       ("r-regioner" ,r-regioner)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-variantannotation" ,r-variantannotation)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/karyoploteR/")
    (synopsis "Plot customizable linear genomes displaying arbitrary data")
    (description "This package creates karyotype plots of arbitrary genomes and
offers a complete set of functions to plot arbitrary data on them.  It mimics
many R base graphics functions coupling them with a coordinate change function
automatically mapping the chromosome and data coordinates into the plot
coordinates.")
    (license license:artistic2.0)))

(define-public r-lpsymphony
  (package
    (name "r-lpsymphony")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lpsymphony" version))
       (sha256
        (base32
         "0f9qjfv7rp1y3mwscnjz3pph7m40zgz55xcdhyii6k1iw2vyaxx9"))))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("r-knitr" ,r-knitr)))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IHW" version))
       (sha256
        (base32
         "04szg3bj5cjixxcp8j3inmj0fzk2mg8gp2w2b33x0im8ik24qiw0"))))
    (properties `((upstream-name . "IHW")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-fdrtool" ,r-fdrtool)
       ("r-lpsymphony" ,r-lpsymphony)
       ("r-slam" ,r-slam)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iCOBRA" version))
       (sha256
        (base32
         "0knqvvfi5y53jk8s2g2bqgxnh2pbdf38676fk7pkdp1r2j6cbi3s"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MAST" version))
       (sha256
        (base32
         "11qr7n9i4masqz0yzikddchyn223m8dy6zv461dly07fd43qi9mn"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/RGLab/MAST/")
    (synopsis "Model-based analysis of single cell transcriptomics")
    (description
     "This package provides methods and models for handling zero-inflated
single cell assay data.")
    (license license:gpl2+)))

(define-public r-monocle
  (package
    (name "r-monocle")
    (version "2.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "monocle" version))
       (sha256
        (base32
         "1k3hwi9aspjy75arigg7i1w7ygf112y12cndibf2bhpz2phzwslx"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
             (url "https://github.com/cole-trapnell-lab/monocle3")
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
    (version "2.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "NOISeq" version))
       (sha256
        (base32
         "08qlavakclgzk345bliam4cfjhsy39n4s6m1biqpq94n9qp00x8f"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scDD" version))
       (sha256
        (base32
         "07l07fq5633ccq5d3l35dm34pwvaqfa3b3qwpn5v5xn99f5hfz0g"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scone" version))
       (sha256
        (base32
         "1lnyxcrw3kn5gi3n59dwdhkqps58cjxfknsjsj53qz5rv8iiqz73"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GEOquery" version))
       (sha256
        (base32
         "1jzhgnd404wkz978vbqzwbgixr7yk98c7s9q1fzlyax4f8l0cpi4"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "0.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "illuminaio" version))
       (sha256
        (base32
         "1yqm2fqw5ka7qywbal3p7axlwm1r0wibsr33n5xjma1dl9pi8fay"))))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "siggenes" version))
       (sha256
        (base32
         "08wi2i6pqx06v13533y3mpli5fb637h0xfwcwy67ya9j2ygypv7w"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bumphunter" version))
       (sha256
        (base32
         "0hfl820kfxydv5kpgyly7sibv2sp6dqsmc78qm33n81w4z4j0mkk"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "minfi" version))
       (sha256
        (base32
         "1x3ksp6syl54hds7wgm4p9yj4mznhhhhk20ijn3i2jc3k8xqcqfi"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/hansenlab/minfi")
    (synopsis "Analyze Illumina Infinium DNA methylation arrays")
    (description
     "This package provides tools to analyze and visualize Illumina Infinium
methylation arrays.")
    (license license:artistic2.0)))

(define-public r-methylumi
  (package
    (name "r-methylumi")
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "methylumi" version))
       (sha256
        (base32
         "00w5affxzirf6ffiznk33papwwvwsk2zgy6xvsx7iaf5kvnak2nh"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lumi" version))
       (sha256
        (base32
         "19asap8vhm3g8hyvpr8l7mw071dsa1d95wx46lh8m6achffngqv3"))))
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
    (version "2.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Linnorm" version))
       (sha256
        (base32
         "1is1kp5av01kqqph16xl7w1dqbyd0q85pgqfv9gqkk8m53635cz3"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IONiseR" version))
       (sha256
        (base32
         "0cfa64d3qv881sa9d665rfki91jaz2spg0zfrb24m37948qzk1lx"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "triform" version))
       (sha256
        (base32
         "089b7f6dwpi9abj0ncswbi4s30k45996zb99sh43avw6jcb6qj60"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantTools" version))
       (sha256
        (base32
         "1im4g9p419mikkh4v585yf5f23d13chy67znk4g2mii2i1cd1c89"))))
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
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Heatplus" version))
       (sha256
        (base32
         "0vp8y0242k6q07yjk4sg2w7mlk5pgzhjgqkxa79c5ypkyp095a8n"))))
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
    (version "2.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOSemSim" version))
       (sha256
        (base32
         "1hk1626172scja2gr6axy98czblz0zljiqgqaknsv2xj6frhxcgs"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "anota" version))
       (sha256
        (base32
         "02s061q6dfw1czppqiklb0fz6q0mjyqgxg6926b2dpqpz8hv690x"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-multtest" ,r-multtest)
       ("r-qvalue" ,r-qvalue)))
    (home-page "https://bioconductor.org/packages/anota/")
    (synopsis "Analysis of translational activity")
    (description
     "Genome wide studies of translational control is emerging as a tool to
study various biological conditions.  The output from such analysis is both
the mRNA level (e.g. cytosolic mRNA level) and the level of mRNA actively
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
    (version "1.58.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "sigPathway" version))
        (sha256
          (base32
            "1fkw0ss471pllqxyjyif5lr35cr8sqpx31x0ccjp85lm3blws72l"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fgsea" version))
       (sha256
        (base32
         "0jmkkayabx3m0lyyc2mxd4vdvv7gv7fbk1r884gplnf2zgsx068n"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/ctlab/fgsea/")
    (synopsis "Fast gene set enrichment analysis")
    (description
     "The package implements an algorithm for fast gene set enrichment
analysis.  Using the fast algorithm makes more permutations and gets
more fine grained p-values, which allows using accurate standard approaches
to multiple hypothesis correction.")
    (license license:expat)))

(define-public r-dose
  (package
    (name "r-dose")
    (version "3.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DOSE" version))
       (sha256
        (base32
         "149hpf690jls5r5g84sh2hqs10qbqi94syhxfv8n2f800fk7lgy4"))))
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
       ("r-reshape2" ,r-reshape2)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "enrichplot" version))
       (sha256
        (base32
         "0lm5yapd567jxcnz9m4a623aymf3q00svjrxp3rf0r9j77dgyisv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cowplot" ,r-cowplot)
       ("r-dose" ,r-dose)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggraph" ,r-ggraph)
       ("r-gosemsim" ,r-gosemsim)
       ("r-igraph" ,r-igraph)
       ("r-magrittr" ,r-magrittr)
       ("r-plyr" ,r-plyr)
       ("r-purrr" ,r-purrr)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-reshape2" ,r-reshape2)
       ("r-scatterpie" ,r-scatterpie)
       ("r-shadowtext" ,r-shadowtext)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "3.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "clusterProfiler" version))
       (sha256
        (base32
         "04v1xsxfxxy8rdjfswv4crpzkx9592r2sh3cjh1kb54sd4lyb6si"))))
    (properties
     `((upstream-name . "clusterProfiler")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-dose" ,r-dose)
       ("r-downloader" ,r-downloader)
       ("r-dplyr" ,r-dplyr)
       ("r-enrichplot" ,r-enrichplot)
       ("r-go-db" ,r-go-db)
       ("r-gosemsim" ,r-gosemsim)
       ("r-magrittr" ,r-magrittr)
       ("r-plyr" ,r-plyr)
       ("r-qvalue" ,r-qvalue)
       ("r-rlang" ,r-rlang)
       ("r-rvcheck" ,r-rvcheck)
       ("r-tidyr" ,r-tidyr)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://guangchuangyu.github.io/software/clusterProfiler/")
    (synopsis "Analysis and visualization of functional profiles for gene clusters")
    (description
     "This package implements methods to analyze and visualize functional
profiles (GO and KEGG) of gene and gene clusters.")
    (license license:artistic2.0)))

(define-public r-mlinterfaces
  (package
    (name "r-mlinterfaces")
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MLInterfaces" version))
       (sha256
        (base32
         "1j920h1657rc5agd1vrkzk126npfhw7pzr7p7gwg4i0h0wv25q3r"))))
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
       ("r-rcpp" ,r-rcpp)
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
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annaffy" version))
       (sha256
        (base32
         "1szlr33lq98pd3kx6n9l07lhr93swbk6vjpvb2n9f7716k39mi4i"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Core" version))
       (sha256
        (base32
         "13mzhn92kqpbn58zmh96f6frkm85sv9137mldfzaljf6snk0spg2"))))
    (properties `((upstream-name . "a4Core")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-glmnet" ,r-glmnet)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/a4Core")
    (synopsis "Automated Affymetrix array analysis core package")
    (description
     "This is the core package for the automated analysis of Affymetrix
arrays.")
    (license license:gpl3)))

(define-public r-a4classif
  (package
    (name "r-a4classif")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Classif" version))
       (sha256
        (base32
         "03fln0x1am5fqhj4fpkx1yq58paqha086bhhr8az8j0vsq1r7wcz"))))
    (properties `((upstream-name . "a4Classif")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-a4core" ,r-a4core)
       ("r-a4preproc" ,r-a4preproc)
       ("r-biobase" ,r-biobase)
       ("r-glmnet" ,r-glmnet)
       ("r-pamr" ,r-pamr)
       ("r-rocr" ,r-rocr)
       ("r-varselrf" ,r-varselrf)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/a4Classif/")
    (synopsis "Automated Affymetrix array analysis classification package")
    (description
     "This is the classification package for the automated analysis of
Affymetrix arrays.")
    (license license:gpl3)))

(define-public r-a4preproc
  (package
    (name "r-a4preproc")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Preproc" version))
       (sha256
        (base32
         "1j8jhal83x1xpmsaw8iwv2r32i1ghzm6n0ipjk06yqa9f6zb7f7i"))))
    (properties `((upstream-name . "a4Preproc")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/a4Preproc/")
    (synopsis "Automated Affymetrix array analysis preprocessing package")
    (description
     "This is a package for the automated analysis of Affymetrix arrays.  It
is used for preprocessing the arrays.")
    (license license:gpl3)))

(define-public r-a4reporting
  (package
    (name "r-a4reporting")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Reporting" version))
       (sha256
        (base32
         "1jx4ym3hyix8gwr8d2r38w1wj7siv6ynzhwanczcjf1naws3dqpy"))))
    (properties `((upstream-name . "a4Reporting")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-xtable" ,r-xtable)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/a4Reporting/")
    (synopsis "Automated Affymetrix array analysis reporting package")
    (description
     "This is a package for the automated analysis of Affymetrix arrays.  It
provides reporting features.")
    (license license:gpl3)))

(define-public r-a4base
  (package
    (name "r-a4base")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Base" version))
       (sha256
        (base32
         "0bqagjmg3yjmdzxv4j7685jjhgb261pq60b5qkfffr1lfnz27lsp"))))
    (properties `((upstream-name . "a4Base")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-a4core" ,r-a4core)
       ("r-a4preproc" ,r-a4preproc)
       ("r-annaffy" ,r-annaffy)
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4" version))
       (sha256
        (base32
         "12q09dhxjl7yrd5m2y7a03kv5614dp144ajmskp5q9x2gvz30f79"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "abseqR" version))
       (sha256
        (base32
         "0lh7kcsp3yb3s8s8j6w9k1by8i16q7r2a49z8y1xjmkcb2klsi3f"))))
    (properties `((upstream-name . "abseqR")))
    (build-system r-build-system)
    (inputs
     `(("pandoc" ,pandoc)))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bacon" version))
       (sha256
        (base32
         "0cv4zhs075mz8c5gdwhr45v14fb1lyi3rlwjfqyz15dmmnzlxw47"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-ellipse" ,r-ellipse)
       ("r-ggplot2" ,r-ggplot2)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rGADEM" version))
       (sha256
        (base32
         "0x13glgkcnjg4qsn0v0qgzy6bgmghqpvgwfww2ha641p0c5i9qzw"))))
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
    (version "1.43.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MotIV" version))
       (sha256
        (base32
         "1yqqymcrnwlpv6h3w80yliv19922g32xqlqszaqjk6zp853qilh6"))))
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

(define-public r-motifdb
  (package
   (name "r-motifdb")
   (version "1.32.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MotifDb" version))
            (sha256
             (base32 "0gfk1dgw7gd2y4cnmfdzpzjqkvvikcwx20h0fp7aiq8f0zfwlav5"))))
   (properties `((upstream-name . "MotifDb")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biocgenerics" ,r-biocgenerics)
      ("r-biostrings" ,r-biostrings)
      ("r-genomicranges" ,r-genomicranges)
      ("r-iranges" ,r-iranges)
      ("r-rtracklayer" ,r-rtracklayer)
      ("r-s4vectors" ,r-s4vectors)
      ("r-splitstackshape" ,r-splitstackshape)))
   (native-inputs
     `(("r-knitr" ,r-knitr)))
   (home-page "https://www.bioconductor.org/packages/MotifDb/")
   (synopsis "Annotated collection of protein-DNA binding sequence motifs")
   (description "This package provides more than 2000 annotated position
frequency matrices from nine public sources, for multiple organisms.")
   (license license:artistic2.0)))

(define-public r-motifbreakr
  (package
   (name "r-motifbreakr")
   (version "2.4.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "motifbreakR" version))
            (sha256
             (base32 "0nni6i7h51kz0ch8ls9c9jzd7fjmc9wsavp11hx6w6bmhnh3k4n7"))))
   (properties `((upstream-name . "motifbreakR")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biocgenerics" ,r-biocgenerics)
      ("r-biocparallel" ,r-biocparallel)
      ("r-biostrings" ,r-biostrings)
      ("r-bsgenome" ,r-bsgenome)
      ("r-genomeinfodb" ,r-genomeinfodb)
      ("r-genomicranges" ,r-genomicranges)
      ("r-grimport" ,r-grimport)
      ("r-gviz" ,r-gviz)
      ("r-iranges" ,r-iranges)
      ("r-matrixstats" ,r-matrixstats)
      ("r-motifdb" ,r-motifdb)
      ("r-motifstack" ,r-motifstack)
      ("r-rtracklayer" ,r-rtracklayer)
      ("r-s4vectors" ,r-s4vectors)
      ("r-stringr" ,r-stringr)
      ("r-summarizedexperiment" ,r-summarizedexperiment)
      ("r-tfmpvalue" ,r-tfmpvalue)
      ("r-variantannotation" ,r-variantannotation)))
   (native-inputs
     `(("r-knitr" ,r-knitr)))
   (home-page "https://www.bioconductor.org/packages/motifbreakR/")
   (synopsis "Predicting disruptiveness of single nucleotide polymorphisms")
   (description "This package allows biologists to judge in the first place
whether the sequence surrounding the polymorphism is a good match, and in
the second place how much information is gained or lost in one allele of
the polymorphism relative to another.  This package gives a choice of
algorithms for interrogation of genomes with motifs from public sources:
@enumerate
@item a weighted-sum probability matrix;
@item log-probabilities;
@item weighted by relative entropy.
@end enumerate

This package can predict effects for novel or previously described variants in
public databases, making it suitable for tasks beyond the scope of its original
design.  Lastly, it can be used to interrogate any genome curated within
Bioconductor.")
   (license license:gpl2+)))

(define-public r-motifstack
  (package
    (name "r-motifstack")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifStack" version))
       (sha256
        (base32
         "1psqpdbgbad31bd8hg5bl62qi5s9rl75nzm85igfpxar3zwwxjlb"))))
    (properties `((upstream-name . "motifStack")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ade4" ,r-ade4)
       ("r-biostrings" ,r-biostrings)
       ("r-ggplot2" ,r-ggplot2)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-xml" ,r-xml)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicScores" version))
       (sha256
        (base32
         "1492xirsgag2dsr6ys9wm3a65sq826p9hcdg3b4dm1wbxgdfx6jr"))))
    (properties `((upstream-name . "GenomicScores")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationhub" ,r-annotationhub)
       ("r-biobase" ,r-biobase)
       ("r-biocfilecache" ,r-biocfilecache)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocmanager" ,r-biocmanager)
       ("r-biostrings" ,r-biostrings)
       ("r-delayedarray" ,r-delayedarray)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-hdf5array" ,r-hdf5array)
       ("r-iranges" ,r-iranges)
       ("r-rhdf5" ,r-rhdf5)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xml" ,r-xml)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/rcastelo/GenomicScores/")
    (synopsis "Work with genome-wide position-specific scores")
    (description
     "This package provides infrastructure to store and access genome-wide
position-specific scores within R and Bioconductor.")
    (license license:artistic2.0)))

(define-public r-atacseqqc
  (package
    (name "r-atacseqqc")
    (version "1.14.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ATACseqQC" version))
       (sha256
        (base32
         "04sn0zl4m60i5jvqz5rmhc4qwcgrhk6rhznrygmm93k9v363mbn9"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOfuncR" version))
       (sha256
        (base32
         "1ah4v2jj508wjsmrncw58wjq2cyris7bnzfw6kr7jp9n4dvn33mq"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ABAEnrichment" version))
       (sha256
        (base32
         "0i0214ap9f6lnyawdgcdsds6g3g9qqji3wbn6ln6rs698gjs9w9c"))))
    (properties `((upstream-name . "ABAEnrichment")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abadata" ,r-abadata)
       ("r-data-table" ,r-data-table)
       ("r-gofuncr" ,r-gofuncr)
       ("r-gplots" ,r-gplots)
       ("r-gtools" ,r-gtools)
       ("r-rcpp" ,r-rcpp)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationFuncs" version))
       (sha256
        (base32
         "0xsm7741zm81bi4c9hy0zaacnk8a6bahdpc6srqzrbsz0pfzdyhr"))))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotationTools" version))
       (sha256
        (base32
         "1q3c30hqxjgar3gm8d7h4rw3m7cgc11cgv9q0fwv5abj075cj224"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AllelicImbalance" version))
       (sha256
        (base32
         "1hk08kwxjlg2jb59bwv9fbc446pyf6knkscfj757nl6yjf11akbl"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/pappewaio/AllelicImbalance")
    (synopsis "Investigate allele-specific expression")
    (description
     "This package provides a framework for allele-specific expression
investigation using RNA-seq data.")
    (license license:gpl3)))

(define-public r-aucell
  (package
    (name "r-aucell")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AUCell" version))
       (sha256
        (base32
         "0ibsf3nid27hipr03z7phh0yzwfj8bqza6n6g7wfghpls4l12ipx"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/AUCell/")
    (synopsis "Analysis of gene set activity in single-cell RNA-seq data")
    (description
     "AUCell identifies cells with active gene sets (e.g. signatures,
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
    (version "4.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBImage" version))
       (sha256
        (base32
         "0qi8bbix5bjahs73ljhfvidlbj8hz5m5j0sb9cjxlngnnldbh4ww"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yamss" version))
       (sha256
        (base32
         "0cxzn7j9apjcabbvvii16kn4whwd9khcyz867w5ag3zdxwvg7l7w"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gtrellis" version))
       (sha256
        (base32
         "14mpavkxlp9d1kccwi4b9hi7x8md5j4s1g17ivqsj38lxqjvg5gw"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-circlize" ,r-circlize)
       ("r-genomicranges" ,r-genomicranges)
       ("r-getoptlong" ,r-getoptlong)
       ("r-iranges" ,r-iranges)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SomaticSignatures" version))
       (sha256
        (base32
         "1pwf9ws0klcij27w22p0nh924yp5h2jsidp54ppp7mnx08iv0801"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "YAPSA" version))
       (sha256
        (base32
         "1vwccrp01p8i42axbaz1bqq173la18ldrzmrjawr5nkjjkvddbpb"))))
    (properties `((upstream-name . "YAPSA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-circlize" ,r-circlize)
       ("r-complexheatmap" ,r-complexheatmap)
       ("r-corrplot" ,r-corrplot)
       ("r-dendextend" ,r-dendextend)
       ("r-doparallel" ,r-doparallel)
       ("r-dplyr" ,r-dplyr)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-getoptlong" ,r-getoptlong)
       ("r-ggbeeswarm" ,r-ggbeeswarm)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-gtrellis" ,r-gtrellis)
       ("r-keggrest" ,r-keggrest)
       ("r-limsolve" ,r-limsolve)
       ("r-magrittr" ,r-magrittr)
       ("r-pmcmr" ,r-pmcmr)
       ("r-pracma" ,r-pracma)
       ("r-reshape2" ,r-reshape2)
       ("r-somaticsignatures" ,r-somaticsignatures)
       ("r-variantannotation" ,r-variantannotation)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gcrma" version))
       (sha256
        (base32
         "1v1x13iwcv6c9x7r1iz2598rwlyzic67jbqcajg24ib6lcfn9f00"))))
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
gene-specific binding is expected.")
    ;; Any version of the LGPL
    (license license:lgpl2.1+)))

(define-public r-simpleaffy
  (package
    (name "r-simpleaffy")
    (version "2.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "simpleaffy" version))
       (sha256
        (base32
         "04a11dsqd5y4b39nny94acnh0qhdazjc6d1803izza4vrgmw2csb"))))
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
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yaqcaffy" version))
       (sha256
        (base32
         "18gphcjj15iivrahp52186bvdg07yd2dvrykfjdd4r1vyf33im96"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "quantro" version))
       (sha256
        (base32
         "1mq4hda73idkq0lkfrhcmiz4kkalfn47dh3i75br5fi33mdgc0k2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-doparallel" ,r-doparallel)
       ("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iterators" ,r-iterators)
       ("r-minfi" ,r-minfi)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yarn" version))
       (sha256
        (base32
         "0p8wz5jn601vxbbxkm73ps3fx0j1y56nr2qf6y8k80vgrk7bv5gp"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "roar" version))
       (sha256
        (base32
         "0spidrcjnrcli0whkf6h8pa1i9dg9arjbm7b1skxbs6dn2k4yyqw"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "XBSeq" version))
       (sha256
        (base32
         "1dvk2jpsdynqw5071z54yd5j0ddprhc1ppk834cz9liibd72d7vz"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MassSpecWavelet" version))
       (sha256
        (base32
         "1vvxbxc538raqdsy0x9ln41vjhp2aw1nrh4k35y3s9mhb1jlzzv3"))))
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
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "xcms" version))
       (sha256
        (base32
         "17kyybj093mj0g2sbfmjp19mmkww4w025n6zc0hbznqb94vkc8fv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-massspecwavelet" ,r-massspecwavelet)
       ("r-mscoreutils" ,r-mscoreutils)
       ("r-msnbase" ,r-msnbase)
       ("r-mzr" ,r-mzr)
       ("r-plyr" ,r-plyr)
       ("r-protgenerics" ,r-protgenerics)
       ("r-rann" ,r-rann)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-robustbase" ,r-robustbase)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Wrench" version))
       (sha256
        (base32
         "01z7rd9fn6cpab3dxgwfpfjlq6vsqb8jhbzvhcqn9v2vqc2pridx"))))
    (properties `((upstream-name . "Wrench")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-limma" ,r-limma)
       ("r-locfit" ,r-locfit)
       ("r-matrixstats" ,r-matrixstats)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/HCBravoLab/Wrench")
    (synopsis "Wrench normalization for sparse count data")
    (description
     "Wrench is a package for normalization sparse genomic count data, like
that arising from 16s metagenomic surveys.")
    (license license:artistic2.0)))

(define-public r-wiggleplotr
  (package
    (name "r-wiggleplotr")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wiggleplotr" version))
       (sha256
        (base32
         "1k4wlh5ayb1w4dr6dydqfgm3415qhsfmshmi6zjyyhhkd2626vad"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "widgetTools" version))
       (sha256
        (base32
         "172f0pmsspd9lss557cmxzjfsbansimjyhwdiahg8pqrayhwvf2w"))))
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
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "webbioc" version))
       (sha256
        (base32
         "1nnmr4ddi07x7sy89fgdn7iwz5k4l8n5ca3xjnlbpwxycza793vj"))))
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zFPKM" version))
       (sha256
        (base32
         "1sa7m7mzzr92c9ickial5701414rab233lq1il1sm9yfdkf8s9fm"))))
    (properties `((upstream-name . "zFPKM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-checkmate" ,r-checkmate)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-tidyr" ,r-tidyr)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie2" version))
       (sha256
        (base32
         "1pcdcqn82ray73bajjnx5zgs98m56acviq3adbzga0cfqf6wiqx5"))))
    (properties `((upstream-name . "Rbowtie2")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "progeny" version))
       (sha256
        (base32
         "00lhzz4plmx5128khs298n6zv9204mhqv548lxxdhaw18b16vwm7"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-gridextra" ,r-gridextra)
       ("r-tidyr" ,r-tidyr)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ARRmNormalization" version))
       (sha256
        (base32
         "1ximvi0jbwmymx6iy70qfyr9j26x5arlarra9fzs5hq05jif6q95"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocFileCache" version))
       (sha256
        (base32
         "0r032a033636bxap0vvb02jvjqiynzj9npqd8603qnwmhvvfi5z1"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iClusterPlus" version))
       (sha256
        (base32
         "02ji84dsbn4wir8sim4qy8h57524mnrsq51wxc7n8ybp5x7n9k9q"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie" version))
       (sha256
        (base32
         "0rgxqc3sbq7phnrn9a6z361725h4zi2mi985i43n7fi3csif7507"))))
    (properties `((upstream-name . "Rbowtie")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SGSeq" version))
       (sha256
        (base32
         "1nfhy5kgyz56b6pyxcq8kflqwnhl9nlffszwpqb5fdh5ibz8xbjx"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhisat2" version))
       (sha256
        (base32
         "0f9x2qcazml0zjcgyy0kdphnww4d1m62rn0ijcqlhy1bng6ihwwb"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "QuasR" version))
       (sha256
        (base32
         "032m01q34nnmvbhcb2g3pz2fqmgcw5464m74m1m0h7x9bl04a5k8"))))
    (properties `((upstream-name . "QuasR")))
    (build-system r-build-system)
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rqc" version))
       (sha256
        (base32
         "083c3ql0gndb6y7m9d3rpbkimyw8cj8jyv77mwwjhq49lzwsg6n9"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "3.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiRewire" version))
       (sha256
        (base32
         "1h9zjjd5krsjpbxlmsbzwx7kbishn0z6mpm8zmrcpmbfrprp38qw"))))
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
    (version "1.31.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "birta" version))
       (sha256
        (base32
         "00a1kcfmcgdbx6wpnhk45wm45bynhry5m93l9hm75j2rwyc4lnca"))))
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
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiDataSet" version))
       (sha256
        (base32
         "0v3ljpkggrpc7zp72z417jkzjq17abwsvsxh33fb8s3i2s4ycaa4"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ropls" version))
       (sha256
        (base32
         "1h76s89hsafrkshpkx7vjinfni9lzfpnbfyg3fhkkrwpp1fnwyj5"))))
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
    (version "1.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biosigner" version))
       (sha256
        (base32
         "0i18j4fk91x5017yk1l35c58k5aby22yh81zkp59irphpv9akvjn"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotatr" version))
       (sha256
        (base32
         "0dq67snpqxl9mifljm6zlnkdb0ghjwday0fvcn3i7zmrfszgzyf9"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rsubread" version))
       (sha256
        (base32
         "0c4akc89p5467n5rzq9bi7h0h15rbpqpvh7fw42qcj7g2vc41wba"))))
    (properties `((upstream-name . "Rsubread")))
    (build-system r-build-system)
    (inputs `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-matrix" ,r-matrix)))
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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowUtils" version))
       (sha256
        (base32
         "1q4g666nd51j24hcp2wxla1bdi77kbfd4i0pxgp7rs2jf7200k09"))))
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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ConsensusClusterPlus" version))
       (sha256
        (base32
         "06h85l1mg2kpjprylzz44nhxp64k211plhch5qhg39wp0fk34lfp"))))
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
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cytolib" version))
       (sha256
        (base32
         "0y8mxrg3jh9bahsf9rblgyja37m1x1ncxfnrli91xjyg0582kh7r"))))
    (properties `((upstream-name . "cytolib")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-linking
           (lambda _
             (substitute* "src/Makevars.in"
               ;; This is to avoid having a plain directory on the list of
               ;; libraries to link.
               (("\\(RHDF5_LIBS\\)" match)
                (string-append match "/libhdf5.a")))
             #t)))))
    (native-inputs
     `(("r-knitr" ,r-knitr)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rcppparallel" ,r-rcppparallel)
       ("r-rhdf5lib" ,r-rhdf5lib)
       ("r-rprotobuflib" ,r-rprotobuflib)))
    (home-page "https://bioconductor.org/packages/cytolib/")
    (synopsis "C++ infrastructure for working with gated cytometry")
    (description
     "This package provides the core data structure and API to represent and
interact with gated cytometry data.")
    (license license:artistic2.0)))

(define-public r-flowcore
  (package
    (name "r-flowcore")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowCore" version))
       (sha256
        (base32
         "001ickrl2asdl0zwpdjqkl1w7137nzxbryamxihgya394jw73xr8"))))
    (properties `((upstream-name . "flowCore")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-cytolib" ,r-cytolib)
       ("r-matrixstats" ,r-matrixstats)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rprotobuflib" ,r-rprotobuflib)
       ("r-s4vectors" ,r-s4vectors)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/flowCore")
    (synopsis "Basic structures for flow cytometry data")
    (description
     "This package provides S4 data structures and basic functions to deal
with flow cytometry data.")
    (license license:artistic2.0)))

(define-public r-flowmeans
  (package
    (name "r-flowmeans")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowMeans" version))
       (sha256
        (base32
         "02y5b3iqjlqjlxrqq0l24dr68sjaniz26jqf14cnnwz1xg5hz734"))))
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
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ncdfFlow" version))
       (sha256
        (base32
         "1knqc3ic2vpck7n7m7adxjz3ac70ra89d5gvlgp9r2q3kgaciwac"))))
    (properties `((upstream-name . "ncdfFlow")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-linking
           (lambda _
             (substitute* "src/Makevars"
               ;; This is to avoid having a plain directory on the list of
               ;; libraries to link.
               (("\\(RHDF5_LIBS\\)" match)
                (string-append match "/libhdf5.a")))
             #t)))))
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-flowcore" ,r-flowcore)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rhdf5lib" ,r-rhdf5lib)
       ("r-zlibbioc" ,r-zlibbioc)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/ncdfFlow/")
    (synopsis "HDF5 based storage for flow cytometry data")
    (description
     "This package provides HDF5 storage based methods and functions for
manipulation of flow cytometry data.")
    (license license:artistic2.0)))

(define-public r-ggcyto
  (package
    (name "r-ggcyto")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggcyto" version))
       (sha256
        (base32
         "0myjvhm9jjb9cih5nlka3f9zg5ncy8gy3krpdpa0618jdglvgr1m"))))
    (properties `((upstream-name . "ggcyto")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-flowcore" ,r-flowcore)
       ("r-flowworkspace" ,r-flowworkspace)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-hexbin" ,r-hexbin)
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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowViz" version))
       (sha256
        (base32
         "1s6jrn2a7hv984xvm6gyn8k3hnma8qidrw9kgj9z5128hv330z7k"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/flowViz/")
    (synopsis "Visualization for flow cytometry")
    (description
     "This package provides visualization tools for flow cytometry data.")
    (license license:artistic2.0)))

(define-public r-flowclust
  (package
    (name "r-flowclust")
    (version "3.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowClust" version))
       (sha256
        (base32
         "1ml3y5wq68jbyr7l5j4zs79bj5bbwzmn5gx41yi88hq78iwkscrq"))))
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
     `(("pkg-config" ,pkg-config)
       ("r-knitr" ,r-knitr)))
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
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RProtoBufLib" version))
       (sha256
        (base32
         "09n4ny3ymfkja2br4rrr2n9dzw3hs7qijhcq4mj0avr86i27llqz"))))
    (properties `((upstream-name . "RProtoBufLib")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-bundled-sources
           (lambda _
             (with-directory-excursion "src"
               (invoke "tar" "xf" "protobuf-3.10.0.tar.gz"))
             #t)))))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/RProtoBufLib/")
    (synopsis "C++ headers and static libraries of Protocol buffers")
    (description
     "This package provides the headers and static library of Protocol buffers
for other R packages to compile and link against.")
    (license license:bsd-3)))

(define-public r-flowworkspace
  (package
    (name "r-flowworkspace")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowWorkspace" version))
       (sha256
        (base32
         "19svh32jq1dpq3ayhpd5r8bw0iax8d9kdvpvc23gx2pf16g1j5ag"))))
    (properties `((upstream-name . "flowWorkspace")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-linking
           (lambda _
             (substitute* "src/Makevars"
               ;; This is to avoid having a plain directory on the list of
               ;; libraries to link.
               (("\\{h5lib\\}" match)
                (string-append match "/libhdf5.a")))
             #t)))))
    (propagated-inputs
     `(("r-aws-s3" ,r-aws-s3)
       ("r-aws-signature" ,r-aws-signature)
       ("r-bh" ,r-bh)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-cytolib" ,r-cytolib)
       ("r-data-table" ,r-data-table)
       ("r-digest" ,r-digest)
       ("r-dplyr" ,r-dplyr)
       ("r-flowcore" ,r-flowcore)
       ("r-ggplot2" ,r-ggplot2)
       ("r-graph" ,r-graph)
       ("r-lattice" ,r-lattice)
       ("r-latticeextra" ,r-latticeextra)
       ("r-matrixstats" ,r-matrixstats)
       ("r-ncdfflow" ,r-ncdfflow)
       ("r-rbgl" ,r-rbgl)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rcppparallel" ,r-rcppparallel)
       ("r-rgraphviz" ,r-rgraphviz)
       ("r-rhdf5lib" ,r-rhdf5lib)
       ("r-rprotobuflib" ,r-rprotobuflib)
       ("r-scales" ,r-scales)
       ("r-xml" ,r-xml)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowStats" version))
       (sha256
        (base32
         "1i6nrwc55k4bn4qprgs6npy7wf8537m429yncqsygsv47z21ix6x"))))
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
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "openCyto" version))
       (sha256
        (base32
         "02dymy5fa0wjd4pql3jdv1d65mak7ra4il96va7c0km8s87rn40v"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/openCyto")
    (synopsis "Hierarchical gating pipeline for flow cytometry data")
    (description
     "This package is designed to facilitate the automated gating methods in a
sequential way to mimic the manual gating strategy.")
    (license license:artistic2.0)))

(define-public r-cytoml
  (package
    (name "r-cytoml")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CytoML" version))
       (sha256
        (base32
         "0ckjb7bkz0cy46scrv4vl9w37g54c0yihvzmbkzilip1ikpvhxd1"))))
    (properties `((upstream-name . "CytoML")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-linking
           (lambda _
             (substitute* "src/Makevars.in"
               ;; This is to avoid having a plain directory on the list of
               ;; libraries to link.
               (("\\{h5lib\\}" match)
                (string-append match "/libhdf5.a")))
             #t)))))
    (inputs
     `(("libxml2" ,libxml2)
       ("zlib" ,zlib)))
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
       ("r-opencyto" ,r-opencyto)
       ("r-plyr" ,r-plyr)
       ("r-rbgl" ,r-rbgl)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rcppparallel" ,r-rcppparallel)
       ("r-rgraphviz" ,r-rgraphviz)
       ("r-rhdf5lib" ,r-rhdf5lib)
       ("r-rprotobuflib" ,r-rprotobuflib)
       ("r-runit" ,r-runit)
       ("r-tibble" ,r-tibble)
       ("r-xml" ,r-xml)
       ("r-xml2" ,r-xml2)
       ("r-yaml" ,r-yaml)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/RGLab/CytoML")
    (synopsis "GatingML interface for cross platform cytometry data sharing")
    (description
     "This package provides an interface to implementations of the GatingML2.0
standard to exchange gated cytometry data with other software platforms.")
    (license license:artistic2.0)))

(define-public r-flowsom
  (package
    (name "r-flowsom")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "FlowSOM" version))
       (sha256
        (base32
         "0gydp6q6zqkadw356k9br646zfynz8gk9ckbx9d297x503j5sgwf"))))
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
    (version "6.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mixOmics" version))
       (sha256
        (base32
         "0q43ay5r0qsx0zjjnrq24fk6pq5cimviky5lm4w2mbjclqf0gv0q"))))
    (properties `((upstream-name . "mixOmics")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-corpcor" ,r-corpcor)
       ("r-dplyr" ,r-dplyr)
       ("r-ellipse" ,r-ellipse)
       ("r-ggrepel" ,r-ggrepel)
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
  (package                              ;Source/Weave error
    (name "r-depecher")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DepecheR" version))
       (sha256
        (base32
         "0c7yv3a7k22nhhw3601n8jdl61cjmlny9b3nfrzsp78mkxi0h469"))))
    (properties `((upstream-name . "DepecheR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beanplot" ,r-beanplot)
       ("r-dosnow" ,r-dosnow)
       ("r-dplyr" ,r-dplyr)
       ("r-fnn" ,r-fnn)
       ("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gmodels" ,r-gmodels)
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RcisTarget" version))
       (sha256
        (base32
         "0a711jzxl1kggpk3ln68xzc5y30my4nbs1mxx8951pyi3jvzjfyf"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cicero" version))
       (sha256
        (base32
         "12y857p4p0m7k72bimli8wjn9cd0gxjwcs3n7ri9pn9l9d42krqr"))))
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
       ("r-stringi" ,r-stringi)
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
               (url "https://github.com/cole-trapnell-lab/cicero-release")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "077yza93wdhi08n40md20jwk55k9lw1f3y0063qkk90cpz60wi0c"))))
      (propagated-inputs
       `(("r-monocle3" ,r-monocle3)
         ,@(alist-delete "r-monocle"
                         (package-propagated-inputs r-cicero)))))))

(define-public r-circrnaprofiler
  (package
    (name "r-circrnaprofiler")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "circRNAprofiler" version))
       (sha256
        (base32
         "0r1hfm3pc7c71irzmxmdwc27ns9hkymz4vhb4pqbli4xn37q7cg8"))))
    (properties
     `((upstream-name . "circRNAprofiler")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationhub" ,r-annotationhub)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-deseq2" ,r-deseq2)
       ("r-dplyr" ,r-dplyr)
       ("r-edger" ,r-edger)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gwascat" ,r-gwascat)
       ("r-iranges" ,r-iranges)
       ("r-magrittr" ,r-magrittr)
       ("r-r-utils" ,r-r-utils)
       ("r-readr" ,r-readr)
       ("r-reshape2" ,r-reshape2)
       ("r-rlang" ,r-rlang)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-seqinr" ,r-seqinr)
       ("r-stringi" ,r-stringi)
       ("r-stringr" ,r-stringr)
       ("r-universalmotif" ,r-universalmotif)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page
     "https://github.com/Aufiero/circRNAprofiler")
    (synopsis
     "Computational framework for the downstream analysis of circular RNA's")
    (description
     "@code{r-circrnaprofiler} is a computational framework for a comprehensive
in silico analysis of @dfn{circular RNA} (circRNAs).  This computational
framework allows to combine and analyze circRNAs previously detected by
multiple publicly available annotation-based circRNA detection tools.  It
covers different aspects of circRNAs analysis from differential expression
analysis, evolutionary conservation, biogenesis to functional analysis.")
    (license license:gpl3)))

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
               (url "https://github.com/aertslab/cisTopic")
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GENIE3" version))
       (sha256
        (base32
         "1z7qkv0cgdx2plhc7xdz6s7vwdjhzcdadi35wg3fl6xpids5njf5"))))
    (properties `((upstream-name . "GENIE3")))
    (build-system r-build-system)
    (propagated-inputs `(("r-reshape2" ,r-reshape2)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/GENIE3")
    (synopsis "Gene network inference with ensemble of trees")
    (description
     "This package implements the GENIE3 algorithm for inferring gene
regulatory networks from expression data.")
    (license license:gpl2+)))

(define-public r-roc
  (package
    (name "r-roc")
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROC" version))
       (sha256
        (base32
         "02b9n42z3kjrfxpdf3glqvimd79nhnycq00mb09fzhkpp5zl43c9"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wateRmelon" version))
       (sha256
        (base32
         "1sc2nxg9bafpvlwqhky2p5b6fkimkk9v3xcab9kvwnj6szrb6p3f"))))
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
    (version "1.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gdsfmt" version))
       (sha256
        (base32
         "0f5vn8h5fzzazcv92sgrf85hc4xkkizk2wwml9mzjd8ya2fkwg8n"))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigmelon" version))
       (sha256
        (base32
         "0hj5njwx7n681vigkq4560f9dm7mdjgvcwbgp5nbdn1fb2z24bk7"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqbias" version))
       (sha256
        (base32
         "1m634sidmk6c603k2gflyiddkns9vr6ij591nmab523xk5r2f4b2"))))
    (properties `((upstream-name . "seqbias")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-genomicranges" ,r-genomicranges)
       ("r-rhtslib" ,r-rhtslib)))
    (home-page "https://bioconductor.org/packages/seqbias/")
    (synopsis "Estimation of per-position bias in high-throughput sequencing data")
    (description
     "This package implements a model of per-position sequencing bias in
high-throughput sequencing data using a simple Bayesian network, the structure
and parameters of which are trained on a set of aligned reads and a reference
genome sequence.")
    (license license:lgpl3)))

(define-public r-snplocs-hsapiens-dbsnp144-grch37
  (package
    (name "r-snplocs-hsapiens-dbsnp144-grch37")
    (version "0.99.20")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SNPlocs.Hsapiens.dbSNP144.GRCh37"
                                     version 'annotation))
              (sha256
               (base32
                "1z8kx43ki1jvj7ms7pcybakcdimfwr6zpjvspkjmma97bdz093iz"))))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-s4vectors" ,r-s4vectors)
       ("r-iranges" ,r-iranges)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-bsgenome" ,r-bsgenome)
       ("r-biostrings" ,r-biostrings)))
    (home-page
     "https://bioconductor.org/packages/SNPlocs.Hsapiens.dbSNP144.GRCh37/")
    (synopsis "SNP locations for Homo sapiens (dbSNP Build 144)")
    (description "This package provides SNP locations and alleles for Homo
sapiens extracted from NCBI dbSNP Build 144.  The source data files used for
this package were created by NCBI on May 29-30, 2015, and contain SNPs mapped
to reference genome GRCh37.p13.  Note that the GRCh37.p13 genome is a
patched version of GRCh37.  However the patch doesn't alter chromosomes 1-22,
X, Y, MT.  GRCh37 itself is the same as the hg19 genome from UCSC *except* for
the mitochondrion chromosome.  Therefore, the SNPs in this package can be
injected in @code{BSgenome.Hsapiens.UCSC.hg19} and they will land at the
correct position but this injection will exclude chrM (i.e. nothing will be
injected in that sequence).")
    (license license:artistic2.0)))

(define-public r-reqon
  (package
    (name "r-reqon")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReQON" version))
       (sha256
        (base32
         "1yz0r0rrk4n6dnqbdq41lvs5z8l6vkx729m0a7f29svw4dbc6mdq"))))
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
    (version "2.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wavClusteR" version))
       (sha256
        (base32
         "18cg0jbr3rjyx31wwyag1n5gams55pbd2rvb99i3g80q9hvswawi"))))
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
       ("r-stringr" ,r-stringr)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
addition, the package integrates RNA-Seq data to estimate the False
Discovery Rate of cluster detection.  Key functions support parallel multicore
computing.  While wavClusteR was designed for PAR-CLIP data analysis, it can
be applied to the analysis of other NGS data obtained from experimental
procedures that induce nucleotide substitutions (e.g. BisSeq).")
    (license license:gpl2)))

(define-public r-timeseriesexperiment
  (package
    (name "r-timeseriesexperiment")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TimeSeriesExperiment" version))
       (sha256
        (base32
         "1jx0rk660mfzk7rfhamnp0disx3bv456cqi9hyaz2wcbcdrlvcjn"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantFiltering" version))
       (sha256
        (base32
         "0zy53knvyk8wy3hmnsxc0w9qkhvx6xhviskvx7rwmrsi7pz531l5"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "variancePartition" version))
       (sha256
        (base32
         "0fisaqd5v8xy9rz9l1zs62k1n2h4k1irzgwj46ci947l52x1qhah"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HTqPCR" version))
       (sha256
        (base32
         "1fzjx6psr41naq9ycpnv3lxlgkiyrpn7r2wl1i4gz45f3lax0yji"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "unifiedWMWqPCR" version))
       (sha256
        (base32
         "1ad5a7gy43l8x1rf5lgqiy2bv6fgah7cbnp4lrqwshphlnr30ndv"))))
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

(define-public r-universalmotif
  (package
    (name "r-universalmotif")
    (version "1.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "universalmotif" version))
       (sha256
        (base32
         "0pmi5mp5v0srr482vlkfmkp28bywq969fvv9g5kjl5rxki963zmr"))))
    (properties
     `((upstream-name . "universalmotif")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-reference-to-strip
           (lambda _
             (substitute* "src/Makevars"
               (("/usr/bin/strip") (which "strip"))))))))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggseqlogo" ,r-ggseqlogo)
       ("r-iranges" ,r-iranges)
       ("r-mass" ,r-mass)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppthread" ,r-rcppthread)
       ("r-rdpack" ,r-rdpack)
       ("r-rlang" ,r-rlang)
       ("r-s4vectors" ,r-s4vectors)
       ("r-yaml" ,r-yaml)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page
     "https://bioconductor.org/packages/universalmotif/")
    (synopsis
     "Specific structures importer, modifier, and exporter for R")
    (description
     "This package allows importing most common @dfn{specific structure}
(motif) types into R for use by functions provided by other Bioconductor
motif-related packages.  Motifs can be exported into most major motif formats
from various classes as defined by other Bioconductor packages.  A suite of
motif and sequence manipulation and analysis functions are included, including
enrichment, comparison, P-value calculation, shuffling, trimming, higher-order
motifs, and others.")
    (license license:gpl3)))

;; This is a CRAN package, but it depends on Bioconductor packages, so we put
;; it here.
(define-public r-activedriverwgs
  (package
    (name "r-activedriverwgs")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActiveDriverWGS" version))
       (sha256
        (base32
         "06mvakdc8d2pn91p0sr4ixc561s4ia5h1cvd1p7pqd6s50dy4say"))))
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
       ("r-s4vectors" ,r-s4vectors)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActivePathways" version))
       (sha256
        (base32
         "1hxy760x141ykrpqdbfldq4ggj1svj3lsrpwi4rb2x7r4lna937l"))))
    (properties
     `((upstream-name . "ActivePathways")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-ggplot2" ,r-ggplot2)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BGmix" version))
       (sha256
        (base32
         "0r9gjqajarg5mivxhqdzn8m8hmfarmzbplp3zqyyznccri03pv50"))))
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
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bgx" version))
       (sha256
        (base32
         "0gm4wv9drqvg9r4f0id1ivrfgv0nvh0hb6hp63b3jd14092777im"))))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BHC" version))
       (sha256
        (base32
         "1n2rkbj8j10f38d40wvi6mwjxnrlfx71a48ab07bp2s0hwhxd7yn"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BicARE" version))
       (sha256
        (base32
         "1np3967rjx54hbjsynvya75lcsqa6zic6frw5fjwqybwv2pzzw2k"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiFET" version))
       (sha256
        (base32
         "191m1xhsj4l64rrh67hqiz1rdkfhw0gfd5aymf3x0xm710l3rh4a"))))
    (properties `((upstream-name . "BiFET")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomicranges" ,r-genomicranges)
       ("r-poibin" ,r-poibin)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rsbml" version))
       (sha256
        (base32
         "0vrjfhwcpc699sq78pkm022fam3ahar8h3lx3fr879dd21k02g61"))))
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
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hypergraph" version))
       (sha256
        (base32
         "1p5mzr2aiqqc1j2inh45abjvqhid9jqv6wiym1xxnz16mpaa7q97"))))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hyperdraw" version))
       (sha256
        (base32
         "0z3a3lpz7s0fw023fxldkgxvl2dl1wn8agnyj09sky11ainxdayz"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiGGR" version))
       (sha256
        (base32
         "1hlsnss6071ck4ky1akxp1dnv3vmd8f85drdziqmm4nc2dfrr14y"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigmemoryExtras" version))
       (sha256
        (base32
         "1k31h746j8r3f92vj62s38fw12qjkv5814ipcqfbswnxgaan17zj"))))
    (properties
     `((upstream-name . "bigmemoryExtras")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bigmemory" ,r-bigmemory)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/phaverty/bigmemoryExtras")
    (synopsis "Extension of the bigmemory package")
    (description
     "This package defines a @code{BigMatrix} @code{ReferenceClass} which adds
safety and convenience features to the @code{filebacked.big.matrix} class from
the @code{bigmemory} package.  @code{BigMatrix} protects against segfaults by
monitoring and gracefully restoring the connection to on-disk data and it also
protects against accidental data modification with a file-system-based
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
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigPint" version))
       (sha256
        (base32
         "1axgapy4iyx059777m9faczwwj03h3i5dyrs0rlc84axzhzd2kis"))))
    (properties `((upstream-name . "bigPint")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-delayedarray" ,r-delayedarray)
       ("r-dplyr" ,r-dplyr)
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
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-tidyr" ,r-tidyr)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "3.42.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChemmineR" version))
       (sha256
        (base32
         "10d8h6w24h4s7l02zzv6q46w3yiqsjizip7mf11cvkmd6p7qxfl9"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.28.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioassayR" version))
       (sha256
        (base32
         "0ylnnm31jkmi8zz78kngqv36yn6i5lvjp1i27v59svw13m4r03g5"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/girke-lab/bioassayR")
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biobroom" version))
       (sha256
        (base32
         "07wzamwl07p20s932aym2jkf6c1zz7d9h7kyby5ka4pw4abynlrv"))))
    (properties `((upstream-name . "biobroom")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-broom" ,r-broom)
       ("r-dplyr" ,r-dplyr)
       ("r-tidyr" ,r-tidyr)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "graphite" version))
       (sha256
        (base32
         "1ihza8m397qfvr79fhghs2knmw862hwz2akysy39r8hndv6lc7wk"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReactomePA" version))
       (sha256
        (base32
         "04b2ng9jp2bsfbg3wnbg6m6a5c3szcmbypk1lx34i63228g8z98m"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBarrays" version))
       (sha256
        (base32
         "1r2dl19my1hqkq01fqk48pk3agb98vgrplj56kb4srhz2xm0w9pd"))))
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
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocCaseStudies" version))
       (sha256
        (base32
         "03n49b6fvyyzmvdy4yif3cl7yv21c09c8xdx4cvvax5zz4v4sab1"))))
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

(define-public r-bioccheck
  (package
    (name "r-bioccheck")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocCheck" version))
              (sha256
               (base32
                "1hyncn9zqj432da95k86rm5b28nbwrvzm52jbhisifkxj1j43cib"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/BiocCheck")
    (synopsis "Executes Bioconductor-specific package checks")
    (description "This package contains tools to perform additional quality
checks on R packages that are to be submitted to the Bioconductor repository.")
    (license license:artistic2.0)))

(define-public r-biocgraph
  (package
    (name "r-biocgraph")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biocGraph" version))
       (sha256
        (base32
         "02y7vizc6jv8y9r8chsda4yysim0ch45i3rasqvv7m85zqsskf75"))))
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

(define-public r-biocstyle
  (package
    (name "r-biocstyle")
    (version "2.18.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocStyle" version))
              (sha256
               (base32
                "0rsxyna4dd99x42vc82mlkxx774vb9375llpakg53max1hhwkrqp"))))
    (properties
     `((upstream-name . "BiocStyle")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocmanager" ,r-biocmanager)
       ("r-bookdown" ,r-bookdown)
       ("r-knitr" ,r-knitr)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-yaml" ,r-yaml)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/BiocStyle")
    (synopsis "Bioconductor formatting styles")
    (description "This package provides standard formatting styles for
Bioconductor PDF and HTML documents.  Package vignettes illustrate use and
functionality.")
    (license license:artistic2.0)))

(define-public r-biocviews
  (package
    (name "r-biocviews")
    (version "1.58.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biocViews" version))
              (sha256
               (base32
                "1by2639z7n62z84dr8rj9jz12gsd1k8q42zsnxacxbwfwp6h0cl4"))))
    (properties
     `((upstream-name . "biocViews")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocmanager" ,r-biocmanager)
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

(define-public r-experimenthub
  (package
    (name "r-experimenthub")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ExperimentHub" version))
       (sha256
        (base32
         "1zi7vsrhf1hj27rlzrxl4jd81fqh1yhn0svp2d9w71fizsi71akg"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-grohmm
  (package
    (name "r-grohmm")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "groHMM" version))
       (sha256
        (base32
         "08pap9wsaxl4jjlc1py0rc019gmi6daa0f9cr3ih1d97wybncanx"))))
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

(define-public r-multiassayexperiment
  (package
    (name "r-multiassayexperiment")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiAssayExperiment" version))
       (sha256
        (base32
         "1nx3gikl8vr54862h6wl0q30arnpynla085219lhh61ziswdffrs"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocOncoTK" version))
       (sha256
        (base32
         "1jwp0gww2xyx8qfv5h4y0v3g66mmlyrd1v64xn91si4fsymk4108"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/BiocOncoTK")
    (synopsis "Bioconductor components for general cancer genomics")
    (description
     "The purpose of this package is to provide a central interface to various
tools for genome-scale analysis of cancer studies.")
    (license license:artistic2.0)))

(define-public r-biocor
  (package
    (name "r-biocor")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioCor" version))
       (sha256
        (base32
         "12slrdn98h43j3y7klk3chrwa2ycwm4krhz3l3kfzbxr834mhy19"))))
    (properties `((upstream-name . "BioCor")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-gseabase" ,r-gseabase)
       ("r-matrix" ,r-matrix)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocPkgTools" version))
       (sha256
        (base32
         "12j4vag40zdgrxfzaqvf3ly7776qyziryz04c3jqzgsqfvzvzz8m"))))
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSet" version))
       (sha256
        (base32
         "16pjg09i0j5qk9s9qzm6fq5q0bgwb4wgqvp6scs06ajgrr07qjqg"))))
    (properties `((upstream-name . "BiocSet")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocio" ,r-biocio)
       ("r-dplyr" ,r-dplyr)
       ("r-keggrest" ,r-keggrest)
       ("r-ontologyindex" ,r-ontologyindex)
       ("r-plyr" ,r-plyr)
       ("r-rlang" ,r-rlang)
       ("r-s4vectors" ,r-s4vectors)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocWorkflowTools" version))
       (sha256
        (base32
         "0lvckdy20bhgyhqbccp0rdfi2p6vvip694r27qwpyi5092nfmqh6"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/BiocWorkflowTools/")
    (synopsis "Tools to aid the development of Bioconductor Workflow packages")
    (description
     "This package provides functions to ease the transition between
Rmarkdown and LaTeX documents when authoring a Bioconductor Workflow.")
    (license license:expat)))

(define-public r-biodist
  (package
    (name "r-biodist")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioDist" version))
       (sha256
        (base32
         "10p4iajpyqgqb35743jm1a33lwbsmax2g4vz9fbbhn2cpiq3chap"))))
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

(define-public r-pcatools
  (package
    (name "r-pcatools")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "PCAtools" version))
       (sha256
        (base32
         "1fz9h99yyn49b5rcnkg2kjdfmczfwnc44fpwbia0izj6gx192phb"))))
    (properties `((upstream-name . "PCAtools")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-bh" ,r-bh)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biocsingular" ,r-biocsingular)
       ("r-cowplot" ,r-cowplot)
       ("r-delayedarray" ,r-delayedarray)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-dqrng" ,r-dqrng)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-lattice" ,r-lattice)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-reshape2" ,r-reshape2)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/kevinblighe/PCAtools")
    (synopsis "PCAtools: everything Principal Components Analysis")
    (description
     "@dfn{Principal Component Analysis} (PCA) extracts the fundamental
structure of the data without the need to build any model to represent it.
This \"summary\" of the data is arrived at through a process of reduction that
can transform the large number of variables into a lesser number that are
uncorrelated (i.e. the 'principal components'), while at the same time being
capable of easy interpretation on the original data.  PCAtools provides
functions for data exploration via PCA, and allows the user to generate
publication-ready figures.  PCA is performed via @code{BiocSingular}; users
can also identify an optimal number of principal components via different
metrics, such as the elbow method and Horn's parallel analysis, which has
relevance for data reduction in single-cell RNA-seq (scRNA-seq) and high
dimensional mass cytometry data.")
    (license license:gpl3)))

(define-public r-rgreat
  (package
    (name "r-rgreat")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rGREAT" version))
       (sha256
        (base32
         "0p2b8cqxibxxmsh687y7yvlvr2a5ipiz53jb4wsr8ddypynb1asj"))))
    (properties `((upstream-name . "rGREAT")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomicranges" ,r-genomicranges)
       ("r-getoptlong" ,r-getoptlong)
       ("r-iranges" ,r-iranges)
       ("r-rcurl" ,r-rcurl)
       ("r-rjson" ,r-rjson)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/jokergoo/rGREAT")
    (synopsis "Client for GREAT analysis")
    (description
     "This package makes GREAT (Genomic Regions Enrichment of Annotations
Tool) analysis automatic by constructing a HTTP POST request according to
user's input and automatically retrieving results from GREAT web server.")
    (license license:expat)))

(define-public r-m3c
  (package
    (name "r-m3c")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "M3C" version))
       (sha256
        (base32
         "05ygi4fd85hh17mlww5wcww8d5z5zvkn2r46s4n6g18mcbv8snv5"))))
    (properties `((upstream-name . "M3C")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cluster" ,r-cluster)
       ("r-corpcor" ,r-corpcor)
       ("r-doparallel" ,r-doparallel)
       ("r-dosnow" ,r-dosnow)
       ("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-matrix" ,r-matrix)
       ("r-matrixcalc" ,r-matrixcalc)
       ("r-rtsne" ,r-rtsne)
       ("r-umap" ,r-umap)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/M3C")
    (synopsis "Monte Carlo reference-based consensus clustering")
    (description
     "M3C is a consensus clustering algorithm that uses a Monte Carlo
simulation to eliminate overestimation of @code{K} and can reject the null
hypothesis @code{K=1}.")
    (license license:agpl3+)))

(define-public r-icens
  (package
    (name "r-icens")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Icens" version))
       (sha256
        (base32
         "1w94mvh8pai77h4fcaiyacmzs58n4kbiq6bm4z0hk24j1ywph3dr"))))
    (properties `((upstream-name . "Icens")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-survival" ,r-survival)))
    (home-page "https://bioconductor.org/packages/Icens")
    (synopsis "NPMLE for censored and truncated data")
    (description
     "This package provides many functions for computing the
@dfn{nonparametric maximum likelihood estimator} (NPMLE) for censored and
truncated data.")
    (license license:artistic2.0)))

;; This is a CRAN package but it depends on r-icens, which is published on
;; Bioconductor.
(define-public r-interval
  (package
    (name "r-interval")
    (version "1.1-0.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "interval" version))
       (sha256
        (base32
         "1b31lh0sv7lzy76230djipahxa10lblbr37kdiigr6hp3dd1xmz9"))))
    (properties `((upstream-name . "interval")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-icens" ,r-icens)
       ("r-mlecens" ,r-mlecens)
       ("r-perm" ,r-perm)
       ("r-survival" ,r-survival)))
    (home-page "https://cran.r-project.org/web/packages/interval/")
    (synopsis "Weighted Logrank tests and NPMLE for interval censored data")
    (description
     "This package provides functions to fit nonparametric survival curves,
plot them, and perform logrank or Wilcoxon type tests.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on r-interval, which depends on a
;; Bioconductor package.
(define-public r-fhtest
  (package
    (name "r-fhtest")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "FHtest" version))
       (sha256
        (base32
         "00mql2r4f5hxhdqf27q3x9s5rz2zzakx2myym97b1w1s7c5znl4q"))))
    (properties `((upstream-name . "FHtest")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-interval" ,r-interval)
       ("r-kmsurv" ,r-kmsurv)
       ("r-mass" ,r-mass)
       ("r-perm" ,r-perm)
       ("r-survival" ,r-survival)))
    (home-page "https://cran.r-project.org/web/packages/FHtest/")
    (synopsis "Tests for survival data based on the Fleming-Harrington class")
    (description
     "This package provides functions to compare two or more survival curves
with:

@itemize
@item The Fleming-Harrington test for right-censored data based on
  permutations and on counting processes.
@item An extension of the Fleming-Harrington test for interval-censored data
  based on a permutation distribution and on a score vector distribution.
@end itemize
")
    (license license:gpl2+)))

(define-public r-fourcseq
  (package
    (name "r-fourcseq")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "FourCSeq" version))
       (sha256
        (base32 "1rwdphcj26xis47n8j1fiyc3k3qbsgn0bhf5bhgy5vm11yqyvicb"))))
    (properties `((upstream-name . "FourCSeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biostrings" ,r-biostrings)
       ("r-deseq2" ,r-deseq2)
       ("r-fda" ,r-fda)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggbio" ,r-ggbio)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gtools" ,r-gtools)
       ("r-lsd" ,r-lsd)
       ("r-matrix" ,r-matrix)
       ("r-reshape2" ,r-reshape2)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page
     "https://bioconductor.org/packages/release/bioc/html/FourCSeq.html")
    (synopsis "Analysis of multiplexed 4C sequencing data")
    (description
     "This package is an R package dedicated to the analysis of (multiplexed)
4C sequencing data.  @code{r-fourcseq} provides a pipeline to detect specific
interactions between DNA elements and identify differential interactions
between conditions.  The statistical analysis in R starts with individual bam
files for each sample as inputs.  To obtain these files, the package contains
a Python script to demultiplex libraries and trim off primer sequences.  With
a standard alignment software the required bam files can be then be
generated.")
    (license license:gpl3+)))

(define-public r-preprocesscore
  (package
    (name "r-preprocesscore")
    (version "1.52.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "preprocessCore" version))
       (sha256
        (base32
         "1hz7rlpscaczvvcalky2f5bmr70aii455549m7f6wk10aklp3nll"))))
    (properties
     `((upstream-name . "preprocessCore")))
    (build-system r-build-system)
    (home-page "https://github.com/bmbolstad/preprocessCore")
    (synopsis "Collection of pre-processing functions")
    (description
     "This package provides a library of core pre-processing and normalization
routines.")
    (license license:lgpl2.0+)))

(define-public r-s4vectors
  (package
    (name "r-s4vectors")
    (version "0.28.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "S4Vectors" version))
              (sha256
               (base32
                "0fhf4lsfxrim7glazh6ng46ykzaly5ggwpg170vcz4cc24prv0rh"))))
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

;; This is a CRAN package, but it depends on preprocessorcore, which is a
;; Bioconductor package.
(define-public r-wgcna
  (package
    (name "r-wgcna")
    (version "1.70-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "WGCNA" version))
       (sha256
        (base32
         "1m6b4a2xpb02c1ajndhk8qlqnhwxa7lkkwj6nzv3l618jy1kp15r"))))
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

(define-public r-rgraphviz
  (package
    (name "r-rgraphviz")
    (version "2.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rgraphviz" version))
       (sha256
        (base32
         "1k0nrskak2v5xv7za226r3wypja3zxxmmc0cxz2imjhlgnkbha77"))))
    (properties `((upstream-name . "Rgraphviz")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             ;; The replacement value is taken from src/graphviz/builddate.h
             (substitute* "src/graphviz/configure"
               (("VERSION_DATE=.*")
                "VERSION_DATE=20200427.2341\n"))
             #t)))))
    ;; FIXME: Rgraphviz bundles the sources of an older variant of
    ;; graphviz.  It does not build with the latest version of graphviz, so
    ;; we do not add graphviz to the inputs.
    (inputs `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-graph" ,r-graph)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://bioconductor.org/packages/Rgraphviz")
    (synopsis "Plotting capabilities for R graph objects")
    (description
     "This package interfaces R with the graphviz library for plotting R graph
objects from the @code{graph} package.")
    (license license:epl1.0)))

(define-public r-fithic
  (package
    (name "r-fithic")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FitHiC" version))
              (sha256
               (base32
                "1sdfkqc6s7m9whkzr0mllzzfjzhj2g54ncjwxj8q0azjgszrfwd2"))))
    (properties `((upstream-name . "FitHiC")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-fdrtool" ,r-fdrtool)
       ("r-rcpp" ,r-rcpp)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HiTC" version))
              (sha256
               (base32
                "1xbh36qgmzl8b6xq0hnl41li2x18yma50fq0v4dglh2ddn7as9iy"))))
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
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HDF5Array" version))
       (sha256
        (base32
         "14v2adhwi0yac834g23kvfid740raclhmribzd28k10gsjk9cj7g"))))
    (properties `((upstream-name . "HDF5Array")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-linking
           (lambda _
             (substitute* "src/Makevars"
               ;; This is to avoid having a plain directory on the list of
               ;; libraries to link.
               (("\\(RHDF5LIB_LIBS\\)" match)
                (string-append match "/libhdf5.a")))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-delayedarray" ,r-delayedarray)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-rhdf5" ,r-rhdf5)
       ("r-rhdf5lib" ,r-rhdf5lib)
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
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhdf5lib" version))
       (sha256
        (base32
         "14fnq4gijxp2l7985pksfk52i6klvy81r3892lnna73c6hh1dj28"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled binaries
           (delete-file-recursively "src/wininclude/")
           (delete-file-recursively "src/winlib-4.9.3/")
           (delete-file-recursively "src/winlib-8.3.0/")
           (delete-file "src/hdf5small_cxx_hl_1.10.6.tar.gz")
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
               ((" \"%s/libsz.a\"") ""))
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
               (rename-file "hdf5/src/libhdf5.settings.in"
                            "hdf5/src/libhdf5.settings")
               (rename-file "Makevars.in" "Makevars")
               (substitute* "Makevars"
                 (("@ZLIB_LIB@") "-lz")
                 (("@ZLIB_INCLUDE@") "")
                 (("HDF5_CXX_LIB=.*")
                  (string-append "HDF5_CXX_LIB="
                                 (assoc-ref inputs "hdf5") "/lib/libhdf5_cpp.a\n"))
                 (("HDF5_LIB=.*")
                  (string-append "HDF5_LIB="
                                 (assoc-ref inputs "hdf5") "/lib/libhdf5.a\n"))
                 (("HDF5_CXX_INCLUDE=.*") "HDF5_CXX_INCLUDE=./hdf5/c++/src\n")
                 (("HDF5_INCLUDE=.*") "HDF5_INCLUDE=./hdf5/src\n")
                 (("HDF5_HL_INCLUDE=.*") "HDF5_HL_INCLUDE=./hdf5/hl/src\n")
                 (("HDF5_HL_CXX_INCLUDE=.*") "HDF5_HL_CXX_INCLUDE=./hdf5/hl/c++/src\n")
                 (("HDF5_HL_LIB=.*")
                  (string-append "HDF5_HL_LIB="
                                 (assoc-ref inputs "hdf5") "/lib/libhdf5_hl.a\n"))
                 (("HDF5_HL_CXX_LIB=.*")
                  (string-append "HDF5_HL_CXX_LIB="
                                 (assoc-ref inputs "hdf5") "/lib/libhdf5_hl_cpp.a\n"))
                 ;; szip is non-free software
                 (("cp \"\\$\\{SZIP_LIB\\}.*") "")
                 (("PKG_LIBS =.*") "PKG_LIBS = -lz -lhdf5\n")))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("hdf5" ,hdf5-1.10)))
    (native-inputs
     `(("hdf5-source" ,(package-source hdf5-1.10))
       ("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/Rhdf5lib")
    (synopsis "HDF5 library as an R package")
    (description "This package provides C and C++ HDF5 libraries for use in R
packages.")
    (license license:artistic2.0)))

(define-public r-beachmat
  (package
    (name "r-beachmat")
    (version "2.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "beachmat" version))
       (sha256
        (base32
         "0vbqdkc52j2v1ghygmhy2cbgqm4l99vmv8930wkzkq1pm73pmjji"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-delayedarray" ,r-delayedarray)
       ("r-matrix" ,r-matrix)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/beachmat")
    (synopsis "Compiling Bioconductor to handle each matrix type")
    (description "This package provides a consistent C++ class interface for a
variety of commonly used matrix types, including sparse and HDF5-backed
matrices.")
    (license license:gpl3)))

;; This package includes files that have been taken from kentutils.  Some
;; parts of kentutils are not released under a free license, but this package
;; only uses files that are also found in the free parts of kentutils.
(define-public r-cner
  (package
    (name "r-cner")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CNEr" version))
       (sha256
        (base32 "0qy4pm23vyy23zwsjkf0mpf2c0p54nq26w9lq7j0ld4bx9l3jc6c"))))
    (properties `((upstream-name . "CNEr")))
    (build-system r-build-system)
    (inputs `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-dbi" ,r-dbi)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-go-db" ,r-go-db)
       ("r-iranges" ,r-iranges)
       ("r-keggrest" ,r-keggrest)
       ("r-powerlaw" ,r-powerlaw)
       ("r-r-utils" ,r-r-utils)
       ("r-readr" ,r-readr)
       ("r-reshape2" ,r-reshape2)
       ("r-rsqlite" ,r-rsqlite)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/ge11232002/CNEr")
    (synopsis "CNE Detection and Visualization")
    (description
     "This package provides tools for large-scale identification and
advanced visualization of sets of conserved noncoding elements.")
    ;; For all files in src/ucsc "license is hereby granted for all use -
    ;; public, private or commercial"; this includes those files that don't
    ;; have a license header, because they are included in the free parts of
    ;; the kentutils package.
    (license (list license:gpl2
                   (license:non-copyleft
                    "https://raw.githubusercontent.com/ucscGenomeBrowser/kent/v410_base/src/lib/LICENSE")))))

(define-public r-tfbstools
  (package
    (name "r-tfbstools")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TFBSTools" version))
       (sha256
        (base32
         "0p42hnwhipmcvrsqk3s8qfiian1fvh6izfd9m961bsx99r2clnha"))))
    (properties `((upstream-name . "TFBSTools")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-catools" ,r-catools)
       ("r-cner" ,r-cner)
       ("r-dbi" ,r-dbi)
       ("r-dirichletmultinomial" ,r-dirichletmultinomial)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-gtools" ,r-gtools)
       ("r-iranges" ,r-iranges)
       ("r-rsqlite" ,r-rsqlite)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-seqlogo" ,r-seqlogo)
       ("r-tfmpvalue" ,r-tfmpvalue)
       ("r-xml" ,r-xml)
       ("r-xvector" ,r-xvector)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/ge11232002/TFBSTools")
    (synopsis "Transcription factor binding site (TFBS) analysis")
    (description
     "TFBSTools is a package for the analysis and manipulation of
transcription factor binding sites.  It includes matrices conversion
between @dfn{Position Frequency Matrix} (PFM), @dfn{Position Weight
Matrix} (PWM) and @dfn{Information Content Matrix} (ICM).  It can also
scan putative TFBS from sequence/alignment, query JASPAR database and
provides a wrapper of de novo motif discovery software.")
    (license license:gpl2)))

(define-public r-motifmatchr
  (package
    (name "r-motifmatchr")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifmatchr" version))
       (sha256
        (base32
         "0zrpn0hqdg0hm80ydkfpiqncwyb8y0xp6mlin7g955p8zcpcm67z"))))
    (properties `((upstream-name . "motifmatchr")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-tfbstools" ,r-tfbstools)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/motifmatchr")
    (synopsis "Fast motif matching in R")
    (description
     "Quickly find motif matches for many motifs and many sequences.
This package wraps C++ code from the MOODS motif calling library.")
    (license license:gpl3)))

(define-public r-chromvar
  (package
    (name "r-chromvar")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromVAR" version))
       (sha256
        (base32 "0dn04ijgq8fncn2bkvnd0lnabjg2s4mpb91b3kwvv3nkgjgfx819"))))
    (properties `((upstream-name . "chromVAR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-dt" ,r-dt)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-miniui" ,r-miniui)
       ("r-nabor" ,r-nabor)
       ("r-plotly" ,r-plotly)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtsne" ,r-rtsne)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shiny" ,r-shiny)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-tfbstools" ,r-tfbstools)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/release/bioc/html/chromVAR.html")
    (synopsis "Determine chromatin variation across regions")
    (description
     "This package @code{r-chromvar} determines variation in chromatin
accessibility across sets of annotations or peaks.  @code{r-chromvar} is
designed primarily for single-cell or sparse chromatin accessibility data like
single cell assay for transposase-accessible chromatin using
sequencing (@code{scATAC-seq} or sparse bulk ATAC or deoxyribonuclease
sequence (@code{DNAse-seq}) experiments.")
    (license license:expat)))

(define-public r-singlecellexperiment
  (package
    (name "r-singlecellexperiment")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SingleCellExperiment" version))
       (sha256
        (base32
         "0wpgb2rhxxlclpmwl08iyfy204f7gpj8ijd0qdy4j41c58bl4qm2"))))
    (properties
     `((upstream-name . "SingleCellExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/SingleCellExperiment")
    (synopsis "S4 classes for single cell data")
    (description "This package defines an S4 class for storing data from
single-cell experiments.  This includes specialized methods to store and
retrieve spike-in information, dimensionality reduction coordinates and size
factors for each cell, along with the usual metadata for genes and
libraries.")
    (license license:gpl3)))

(define-public r-scuttle
  (package
    (name "r-scuttle")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scuttle" version))
       (sha256
        (base32
         "0vfhxyv81y525qgk0s3bxy1yypj16h1bl7sc1a1jdqx11fxxv2l8"))))
    (properties `((upstream-name . "scuttle")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-delayedarray" ,r-delayedarray)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-genomicranges" ,r-genomicranges)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-s4vectors" ,r-s4vectors)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/scuttle")
    (synopsis "Single-cell RNA-Seq analysis utilities")
    (description
     "This package provides basic utility functions for performing single-cell
analyses, focusing on simple normalization, quality control and data
transformations.  It also provides some helper functions to assist development
of other packages.")
    (license license:gpl3)))

(define-public r-scater
  (package
    (name "r-scater")
    (version "1.18.6")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scater" version))
              (sha256
               (base32
                "0k1ls5gqv1zsn1w2kszhmbhwfccfjw8khk36s5zbf90rbbkw5609"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocneighbors" ,r-biocneighbors)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biocsingular" ,r-biocsingular)
       ("r-delayedarray" ,r-delayedarray)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-ggbeeswarm" ,r-ggbeeswarm)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-matrix" ,r-matrix)
       ("r-rlang" ,r-rlang)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scuttle" ,r-scuttle)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-viridis" ,r-viridis)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/davismcc/scater")
    (synopsis "Single-cell analysis toolkit for gene expression data in R")
    (description "This package provides a collection of tools for doing
various analyses of single-cell RNA-seq gene expression data, with a focus on
quality control.")
    (license license:gpl2+)))

(define-public r-scran
  (package
    (name "r-scran")
    (version "1.18.5")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scran" version))
       (sha256
        (base32
         "0mk4bs7pkzbaiaaap75nzsrlwr883h45xnbpn94fy91i8d9w1xy1"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-bh" ,r-bh)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocneighbors" ,r-biocneighbors)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biocsingular" ,r-biocsingular)
       ("r-bluster" ,r-bluster)
       ("r-delayedarray" ,r-delayedarray)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-dqrng" ,r-dqrng)
       ("r-edger" ,r-edger)
       ("r-igraph" ,r-igraph)
       ("r-limma" ,r-limma)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scuttle" ,r-scuttle)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-statmod" ,r-statmod)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/scran")
    (synopsis "Methods for single-cell RNA-Seq data analysis")
    (description "This package implements a variety of low-level analyses of
single-cell RNA-seq data.  Methods are provided for normalization of
cell-specific biases, assignment of cell cycle phase, and detection of highly
variable and significantly correlated genes.")
    (license license:gpl3)))

(define-public r-sparsematrixstats
  (package
    (name "r-sparsematrixstats")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "sparseMatrixStats" version))
       (sha256
        (base32
         "01gnmy9zqd0ygm40vqkmqmiwfqmdawj4m81dysmmcdm7z80rn9ii"))))
    (properties
     `((upstream-name . "sparseMatrixStats")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)
       ("r-matrixgenerics" ,r-matrixgenerics)
       ("r-matrixstats" ,r-matrixstats)
       ("r-rcpp" ,r-rcpp)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/sparseMatrixStats/")
    (synopsis "Summary statistics for rows and columns of sparse matrices")
    (description
     "This package provides high performance functions for row and column
operations on sparse matrices.  Currently, the optimizations are limited to
data in the column sparse format.")
    (license license:expat)))

(define-public r-delayedmatrixstats
  (package
    (name "r-delayedmatrixstats")
    (version "1.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DelayedMatrixStats" version))
       (sha256
        (base32
         "1hb8jv5dy3svf7xan6rym7amwdqm5mvl9qx5xhmj1vkb81bizn7h"))))
    (properties
     `((upstream-name . "DelayedMatrixStats")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-delayedarray" ,r-delayedarray)
       ("r-hdf5array" ,r-hdf5array)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-matrixgenerics" ,r-matrixgenerics)
       ("r-matrixstats" ,r-matrixstats)
       ("r-s4vectors" ,r-s4vectors)
       ("r-sparsematrixstats" ,r-sparsematrixstats)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-mscoreutils
  (package
    (name "r-mscoreutils")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MsCoreUtils" version))
       (sha256
        (base32
         "0fa3bcf2cmzf5y8wjs7pnzs26qwgqnrrl4hj4sa4fp9kv8z80630"))))
    (properties `((upstream-name . "MsCoreUtils")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-rcpp" ,r-rcpp)
       ("r-s4vectors" ,r-s4vectors)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/RforMassSpectrometry/MsCoreUtils")
    (synopsis "Core utils for mass spectrometry data")
    (description
     "This package defines low-level functions for mass spectrometry data and
is independent of any high-level data structures.  These functions include
mass spectra processing functions (noise estimation, smoothing, binning),
quantitative aggregation functions (median polish, robust summarisation,
etc.), missing data imputation, data normalisation (quantiles, vsn, etc.) as
well as misc helper functions, that are used across high-level data structure
within the R for Mass Spectrometry packages.")
    (license license:artistic2.0)))

(define-public r-biocio
  (package
    (name "r-biocio")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "BiocIO" version))
        (sha256
          (base32
            "06gg5ra3r7q4b6mz14c2s9d453cnh1lxh517ffl9f8dr8vwv5s18"))))
    (properties `((upstream-name . "BiocIO")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-biocgenerics" ,r-biocgenerics)
        ("r-genomicranges" ,r-genomicranges)
        ("r-rcurl" ,r-rcurl)
        ("r-s4vectors" ,r-s4vectors)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/BiocIO")
    (synopsis "Standard input and output for Bioconductor packages")
    (description
      "This package implements `import()` and `export()` standard generics for
importing and exporting biological data formats. `import()` supports
whole-file as well as chunk-wise iterative import.  The `import()` interface
optionally provides a standard mechanism for 'lazy' access via `filter()` (on
row or element-like components of the file resource), `select()` (on
column-like components of the file resource) and `collect()`.  The `import()`
interface optionally provides transparent access to remote (e.g.  via https)
as well as local access.  Developers can register a file extension, e.g.,
`.loom` for dispatch from character-based URIs to specific `import()` /
`export()` methods based on classes representing file types, e.g.,
`LoomFile()`.")
    (license license:artistic2.0)))

(define-public r-msmseda
  (package
    (name "r-msmseda")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msmsEDA" version))
       (sha256
        (base32
         "1llmy8msxmrqik3s3439wffma1662vwvvcaz8q0a4g5ridkmdbrx"))))
    (properties `((upstream-name . "msmsEDA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gplots" ,r-gplots)
       ("r-mass" ,r-mass)
       ("r-msnbase" ,r-msnbase)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page
     "https://bioconductor.org/packages/msmsEDA")
    (synopsis "Exploratory data analysis of LC-MS/MS data by spectral counts")
    (description
     "Exploratory data analysis to assess the quality of a set of LC-MS/MS
experiments, and visualize de influence of the involved factors.")
    (license license:gpl2)))

(define-public r-msmstests
  (package
    (name "r-msmstests")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msmsTests" version))
       (sha256
        (base32
         "1zsnmzj1qvjdwz7mwg9wrsk5iskpqs0f6jccqn8mxy9dgkskmp0j"))))
    (properties `((upstream-name . "msmsTests")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-edger" ,r-edger)
       ("r-msmseda" ,r-msmseda)
       ("r-msnbase" ,r-msnbase)
       ("r-qvalue" ,r-qvalue)))
    (home-page
     "https://bioconductor.org/packages/msmsTests")
    (synopsis "Differential LC-MS/MS expression tests")
    (description
     "This packages provides statistical tests for label-free LC-MS/MS data
by spectral counts, to discover differentially expressed proteins between two
biological conditions.  Three tests are available: Poisson GLM regression,
quasi-likelihood GLM regression, and the negative binomial of the edgeR
package.  The three models admit blocking factors to control for nuisance
variables.  To assure a good level of reproducibility a post-test filter is
available, where we may set the minimum effect size considered biologicaly
relevant, and the minimum expression of the most abundant condition.")
    (license license:gpl2)))

(define-public r-catalyst
  (package
    (name "r-catalyst")
    (version "1.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "CATALYST" version))
        (sha256
          (base32
            "13af7c4irx1f5yqi32k7kj661vzg32wn3dnps7r9pjijfl4drhrh"))))
    (properties `((upstream-name . "CATALYST")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-circlize" ,r-circlize)
        ("r-complexheatmap" ,r-complexheatmap)
        ("r-consensusclusterplus" ,r-consensusclusterplus)
        ("r-cowplot" ,r-cowplot)
        ("r-data-table" ,r-data-table)
        ("r-dplyr" ,r-dplyr)
        ("r-drc" ,r-drc)
        ("r-flowcore" ,r-flowcore)
        ("r-flowsom" ,r-flowsom)
        ("r-ggplot2" ,r-ggplot2)
        ("r-ggrepel" ,r-ggrepel)
        ("r-ggridges" ,r-ggridges)
        ("r-gridextra" ,r-gridextra)
        ("r-magrittr" ,r-magrittr)
        ("r-matrix" ,r-matrix)
        ("r-matrixstats" ,r-matrixstats)
        ("r-nnls" ,r-nnls)
        ("r-purrr" ,r-purrr)
        ("r-rcolorbrewer" ,r-rcolorbrewer)
        ("r-reshape2" ,r-reshape2)
        ("r-rtsne" ,r-rtsne)
        ("r-s4vectors" ,r-s4vectors)
        ("r-scales" ,r-scales)
        ("r-scater" ,r-scater)
        ("r-singlecellexperiment" ,r-singlecellexperiment)
        ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page
      "https://github.com/HelenaLC/CATALYST")
    (synopsis "Cytometry data analysis tools")
    (description
      "This package is Cytometry dATa anALYSis Tools (CATALYST).  Mass
cytometry (CyTOF) uses heavy metal isotopes rather than fluorescent tags as
reporters to label antibodies, thereby substantially decreasing spectral
overlap and allowing for examination of over 50 parameters at the single cell
level.  While spectral overlap is significantly less pronounced in CyTOF than
flow cytometry, spillover due to detection sensitivity, isotopic impurities,
and oxide formation can impede data interpretability.  We designed
CATALYST (Cytometry dATa anALYSis Tools) to provide a pipeline for
preprocessing of cytometry data, including i) normalization using bead
standards, ii) single-cell deconvolution, and iii) bead-based compensation.")
    (license license:gpl2+)))

(define-public r-erma
  (package
    (name "r-erma")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "erma" version))
       (sha256
        (base32
         "1k2j1xhv0vwn45xmh8ds0gz812px5hnpgzvp37ngsdn4j5ai1l0k"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-ggbio
  (package
    (name "r-ggbio")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggbio" version))
       (sha256
        (base32
         "0vabil4jzrlv01aibqjhdkvrv2bf2kkpsidrkjj06isqr5fz54lw"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-gqtlbase
  (package
    (name "r-gqtlbase")
    (version "1.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gQTLBase" version))
       (sha256
        (base32
         "0nipibm1bk9k70ajbw1f6vjmz0dh7gk21l17q3m54bnawxsggrfh"))))
    (properties `((upstream-name . "gQTLBase")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This is an upstream bug.
         (add-after 'unpack 'fix-imports
           (lambda _
             (substitute* "NAMESPACE"
               ((".*maxffmode.*") "")
               (("importFrom\\(ff,.*") "import(ff)\n"))
             #t)))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/gQTLBase")
    (synopsis "Infrastructure for eQTL, mQTL and similar studies")
    (description
     "The purpose of this package is to simplify the storage and interrogation
of @dfn{quantitative trait loci} (QTL) archives, such as eQTL, mQTL, dsQTL,
and more.")
    (license license:artistic2.0)))

(define-public r-gqtlstats
  (package
    (name "r-gqtlstats")
    (version "1.21.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gQTLstats" version))
       (sha256
        (base32
         "1h78l23idf867djmdk97b02jxgmz4vfr2dai01fp648d0lsx5mkl"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.34.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Gviz" version))
       (sha256
        (base32
         "0bmlfz9ri1gkwyl605a2hqi5b8jdpvynrxwghwmrsd657ip6c7n1"))))
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
       ("r-ensembldb" ,r-ensembldb)
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gwascat" version))
       (sha256
        (base32
         "1aqi1ny93virnzsxkh9ccx3mws70bgv0r8nwgla09vffb7f16nna"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocfilecache" ,r-biocfilecache)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-readr" ,r-readr)
       ("r-s4vectors" ,r-s4vectors)
       ("r-snpstats" ,r-snpstats)
       ("r-variantannotation" ,r-variantannotation)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/gwascat")
    (synopsis "Tools for data in the EMBL-EBI GWAS catalog")
    (description
     "This package provides tools for representing and modeling data in the
EMBL-EBI GWAS catalog.")
    (license license:artistic2.0)))

(define-public r-kegggraph
  (package
    (name "r-kegggraph")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGgraph" version))
       (sha256
        (base32 "1h293hn02ysm923bh9gxk87xv663xiqchqcvpaxpla9c3yrgkx2v"))))
    (properties `((upstream-name . "KEGGgraph")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-graph" ,r-graph)
       ("r-rcurl" ,r-rcurl)
       ("r-xml" ,r-xml)))
    (home-page "https://bioconductor.org/packages/KEGGgraph")
    (synopsis "Graph approach to Kegg Pathway database in R and Bioconductor")
    (description
     "@code{r-kegggraph} is an interface between Kegg Pathway database and graph
object as well as a collection of tools to analyze, dissect and visualize these
graphs.  It parses the regularly updated kgml (Kegg XML) files into graph models
maintaining all essential pathway attributes.  The package offers
functionalities including parsing, graph operation, visualization and etc.")
    (license license:gpl2+)))

(define-public r-ldblock
  (package
    (name "r-ldblock")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ldblock" version))
       (sha256
        (base32
         "09i3ikv0axks9g842z1pjsc8x0fba51zyyc218h0bylbi1n9cdkm"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-ensdb-hsapiens-v75" ,r-ensdb-hsapiens-v75)
       ("r-ensembldb" ,r-ensembldb)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-httr" ,r-httr)
       ("r-matrix" ,r-matrix)
       ("r-rsamtools" ,r-rsamtools)
       ("r-snpstats" ,r-snpstats)
       ("r-variantannotation" ,r-variantannotation)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/ldblock")
    (synopsis "Data structures for linkage disequilibrium measures in populations")
    (description
     "This package defines data structures for @dfn{linkage
disequilibrium} (LD) measures in populations.  Its purpose is to simplify
handling of existing population-level data for the purpose of flexibly
defining LD blocks.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on r-snpstats, which is a
;; Bioconductor package.
(define-public r-ldheatmap
  (package
    (name "r-ldheatmap")
    (version "1.0-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "LDheatmap" version))
       (sha256
        (base32
         "1jp578cf29qcgx95w10lpymlwx2pgjsf0nypwkl9b8g635gkisq7"))))
    (properties `((upstream-name . "LDheatmap")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genetics" ,r-genetics)
       ("r-rcpp" ,r-rcpp)
       ("r-snpstats" ,r-snpstats)))
    (home-page "https://stat.sfu.ca/statgen/research/ldheatmap.html")
    (synopsis "Graphical display of pairwise linkage disequilibria between SNPs")
    (description
     "This package provides tools to produce a graphical display, as a heat
map, of measures of pairwise linkage disequilibria between SNPs.  Users may
optionally include the physical locations or genetic map distances of each SNP
on the plot.")
    (license license:gpl3)))

(define-public r-pathview
  (package
    (name "r-pathview")
    (version "1.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pathview" version))
       (sha256
        (base32 "11fisiksw1y64ii9q8p2znyp9w8mlqzgiaacmycw59rngkjlmbs4"))))
    (properties `((upstream-name . "pathview")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-graph" ,r-graph)
       ("r-kegggraph" ,r-kegggraph)
       ("r-keggrest" ,r-keggrest)
       ("r-org-hs-eg-db" ,r-org-hs-eg-db)
       ("r-png" ,r-png)
       ("r-rgraphviz" ,r-rgraphviz)
       ("r-xml" ,r-xml)))
    (home-page "https://pathview.uncc.edu/")
    (synopsis "Tool set for pathway based data integration and visualization")
    (description
     "@code{r-pathview} is a tool set for pathway based data integration and
visualization.  It maps and renders a wide variety of biological data on
relevant pathway graphs.  All users need is to supply their data and specify
the target pathway.  This package automatically downloads the pathway graph
data, parses the data file, maps user data to the pathway, and render pathway
graph with the mapped data.  In addition, @code{r-pathview} also seamlessly
integrates with pathway and gene set (enrichment) analysis tools for
large-scale and fully automated analysis.")
    (license license:gpl3+)))

(define-public r-snpstats
  (package
    (name "r-snpstats")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "snpStats" version))
       (sha256
        (base32
         "1298a71swwav53yf9kfqkdpach3818plqcbw0lgb6sibs8y8ff24"))))
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

(define-public r-chromstar
  (package
    (name "r-chromstar")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromstaR" version))
       (sha256
        (base32
         "0vgpb7g2cncdn82hia2yzzachyns2zbd7906662g990qjnp2xlm1"))))
    (properties `((upstream-name . "chromstaR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bamsignals" ,r-bamsignals)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-chromstardata" ,r-chromstardata)
       ("r-doparallel" ,r-doparallel)
       ("r-foreach" ,r-foreach)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iranges" ,r-iranges)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-reshape2" ,r-reshape2)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/ataudt/chromstaR")
    (synopsis "Chromatin state analysis for ChIP-Seq data")
    (description
     "This package implements functions for combinatorial and differential
analysis of ChIP-seq data.  It includes uni- and multivariate peak-calling,
export to genome browser viewable files, and functions for enrichment
analyses.")
    (license license:artistic2.0)))

(define-public r-sushi
  (package
    (name "r-sushi")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Sushi" version))
              (sha256
               (base32
                "0ksj4f9z14mjsv6ssg5dwhpldw4r7wpdsln2if5g486mm1c56r8p"))))
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
