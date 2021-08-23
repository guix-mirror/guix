;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018, 2020, 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 Peter Lo <peterloleungyau@gmail.com>
;;; Copyright © 2020, 2021 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Hong Li <hli@mdc-berlin.de>
;;; Copyright © 2021 Tim Howes <timhowes@lavabit.com>
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
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
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

(define-public r-bsgenome-btaurus-ucsc-bostau8
  (package
    (name "r-bsgenome-btaurus-ucsc-bostau8")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Btaurus.UCSC.bosTau8"
                                     version 'annotation))
              (sha256
               (base32
                "16wjy1aw9nvx03r7w8yh5w7sw3pn8i9nczd0n0728l6nnyqxlsz6"))))
    (properties
     `((upstream-name . "BSgenome.Btaurus.UCSC.bosTau8")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Btaurus.UCSC.bosTau8/")
    (synopsis "Full genome sequences for Bos taurus (UCSC version bosTau8)")
    (description "This package provides the full genome sequences for Bos
taurus (UCSC version bosTau8).")
    (license license:artistic2.0)))

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

(define-public r-go-db
  (package
    (name "r-go-db")
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GO.db" version 'annotation))
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

(define-public r-aneufinderdata
  (package
   (name "r-aneufinderdata")
   (version "1.18.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "AneuFinderData" version 'experiment))
            (sha256
             (base32
              "02vb3kmza5hv8bc424fdmfif608xvpdb759w8882kac8izpv29ks"))))
   (build-system r-build-system)
   (home-page "https://bioconductor.org/packages/AneuFinderData/")
   (synopsis "Data package for @code{AneuFinder}")
   (description "This package contains data used by @code{AneuFinder}.")
   (license license:artistic2.0)))

(define-public r-aneufinder
  (package
    (name "r-aneufinder")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AneuFinder" version))
              (sha256
               (base32
                "0m6wphlriq7y21ih1p2kzml5jzcic79jc52kkk59dkjj8j88yllk"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (propagated-inputs
     `(("r-genomicranges" ,r-genomicranges)
       ("r-aneufinderdata" ,r-aneufinderdata)
       ("r-ecp" ,r-ecp)
       ("r-foreach" ,r-foreach)
       ("r-doparallel" ,r-doparallel)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-s4vectors" ,r-s4vectors)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-bamsignals" ,r-bamsignals)
       ("r-dnacopy" ,r-dnacopy)
       ("r-biostrings" ,r-biostrings)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-ggplot2" ,r-ggplot2)
       ("r-reshape2" ,r-reshape2)
       ("r-ggdendro" ,r-ggdendro)
       ("r-ggrepel" ,r-ggrepel)
       ("r-reordercluster" ,r-reordercluster)
       ("r-mclust" ,r-mclust)
       ("r-cowplot" ,r-cowplot)))
    (home-page "https://bioconductor.org/packages/AneuFinder/")
    (synopsis "Copy number variation analysis in single-cell-sequencing data")
    (description "This package implements functions for copy number variant
calling, plotting, export and analysis from whole-genome single cell
sequencing data.")
    (license license:artistic2.0)))

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

(define-public r-copyhelper
  (package
    (name "r-copyhelper")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CopyhelpeR" version 'experiment))
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

(define-public r-genomationdata
  (package
    (name "r-genomationdata")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "genomationData" version 'experiment))
              (sha256
               (base32
                "0igjsvfnws3498j65ifniw0kbxfqpfr59rcjddqvq4zsj453fx1g"))))
    (properties
     `((upstream-name . "genomationData")))
    (build-system r-build-system)
    ;; As this package provides little more than large data files, it doesn't
    ;; make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioinformatics.mdc-berlin.de/genomation/")
    (synopsis "Experimental data for use with the genomation package")
    (description
     "This package contains experimental genetic data for use with the
genomation package.  Included are Chip Seq, Methylation and Cage data,
downloaded from Encode.")
    (license license:gpl3+)))

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
    (version "3.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocVersion" version))
       (sha256
        (base32
         "0rsw8g4g1pcjw1zbx8x17yd3drhxqk4sx3cy3ddzy5731hl6mbfi"))))
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
    (version "0.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocGenerics" version))
              (sha256
               (base32
                "1kv7lzmyki3hi771v01nml1v1hzz8pyhqqv0xcdzqy354mlgx4m6"))))
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
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "CoverageView" version))
              (sha256
               (base32
                "1xhirbjdw09cqm4xvysxqicvqjbahavwvs7shg4cb05gwyd2ha8g"))))
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
   (version "2.34.0")
   (source (origin
             (method url-fetch)
             (uri (bioconductor-uri "cummeRbund" version))
             (sha256
              (base32
               "1avvmvrmldbscc7xd6a6k22xjykbzafvqf87wh5z9rx3qlzswsjx"))))
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

(define-public r-decipher
  (package
    (name "r-decipher")
    (version "2.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DECIPHER" version))
              (sha256
               (base32
                "0mr7glkx2d37l9nszs52m0kycpm14vxl5gdp3z7i5j7yig1sw2nk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-dbi" ,r-dbi)
       ("r-iranges" ,r-iranges)
       ("r-rsqlite" ,r-rsqlite)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)))
    (home-page "https://www.bioconductor.org/packages/DECIPHER/")
    (synopsis "Tools for deciphering and managing biological sequences")
    (description "This package provides a toolset for deciphering and managing
biological sequences.")
    (license license:gpl3)))

(define-public r-deepsnv
  (package
    (name "r-deepsnv")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "deepSNV" version))
              (sha256
               (base32
                "0zz56hf417m7bgg2g2wpbaik30pi6h2nam1n5bviqgdn4mv8n0bs"))))
    (properties `((upstream-name . "deepSNV")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rhtslib" ,r-rhtslib)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)
       ("r-vgam" ,r-vgam)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/gerstung-lab/deepSNV/")
    (synopsis "Detection of subclonal SNVs in deep sequencing data")
    (description
     "This package provides quantitative variant callers for detecting
subclonal mutations in ultra-deep (>=100x coverage) sequencing experiments.
The deepSNV algorithm is used for a comparative setup with a control experiment
of the same loci and uses a beta-binomial model and a likelihood ratio test to
discriminate sequencing errors and subclonal SNVs.  The shearwater algorithm
computes a Bayes classifier based on a beta-binomial model for variant calling
with multiple samples for precisely estimating model parameters - such as local
error rates and dispersion - and prior knowledge, e.g.  from variation data
bases such as COSMIC.")
    (license license:gpl3)))

(define-public r-delayedarray
  (package
    (name "r-delayedarray")
    (version "0.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DelayedArray" version))
              (sha256
               (base32
                "0w632262dqzcnvq9s6jvc7naz93dayx51fsv05s0zb6cjwygbqjr"))))
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
   (version "1.2.1")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "bluster" version))
            (sha256
             (base32
              "128zd5vwlhmrkq0bpp1dxnkrcqfz1mjmdjlla4wqbjv7v6yyn6lg"))))
   (properties `((upstream-name . "bluster")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biocneighbors" ,r-biocneighbors)
      ("r-biocparallel" ,r-biocparallel)
      ("r-cluster" ,r-cluster)
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
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IdeoViz" version))
              (sha256
               (base32
                "17sx0v5lq2zmg098ps4ksj1h0yla3vlh6s2w1ahqsq0nvm193scm"))))
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
    (version "2.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IRanges" version))
              (sha256
               (base32
                "0dzj8wqbjzilh2dsaj3ylx958xqrv0c688ayfq2r1z7rs75qy7kx"))))
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
   (version "1.0.5")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "BisqueRNA" version))
            (sha256
             (base32
              "0p3p5lp69gri7vs6qfpm7br4ksbs4l7clm4nj8ki99wpqiqni23n"))))
   (properties `((upstream-name . "BisqueRNA")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biobase" ,r-biobase)
      ("r-limsolve" ,r-limsolve)))
   (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-affy
  (package
    (name "r-affy")
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affy" version))
       (sha256
        (base32
         "0vz5y92gqcm0qk57qlba85bk683lzdr7vkmxv109rq6i17vdkkrm"))))
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

(define-public r-affycomp
  (package
    (name "r-affycomp")
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycomp" version))
       (sha256
        (base32
         "07my5scdh6h0y1bx1h9y5m4fa8bnmw389f83gkb7cf19w4vp36b2"))))
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
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AffyCompatible" version))
       (sha256
        (base32
         "1xwz22wf2smsvmd0d5lfadbsgscnl3fl9msypy2mml38k048p6vj"))))
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
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyContam" version))
       (sha256
        (base32
         "0242ga68wrdk1kcmxfdbq73a0d3zhrzdlzkzphfg3q0zjbvg49jj"))))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycoretools" version))
       (sha256
        (base32
         "1dx861spr3kn1dxwhf43s5l1r7bmrq0h6538l3q64iiwzzc6krdh"))))
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

(define-public r-affyio
  (package
    (name "r-affyio")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyio" version))
       (sha256
        (base32
         "07kibqsm888g06qqqa4648ph877kriy74kprbri8nyx83y8aspjr"))))
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

(define-public r-affxparser
  (package
    (name "r-affxparser")
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affxparser" version))
       (sha256
        (base32
         "0n3yyrglzqzw0wqxl9igqvkj8qslw6yjkym3vcq0c93kkg7vk01l"))))
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
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotate" version))
       (sha256
        (base32
         "13ny596wh65sw7dbw0zd4h84d6k1w99g91c4sqy83mdzgpv73sz7"))))
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
    (version "1.54.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationDbi" version))
              (sha256
               (base32
                "0fcammls431pgmp47r85k0zh3bz42bajbqdmafd31kpqncc1ijal"))))
    (properties
     `((upstream-name . "AnnotationDbi")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-dbi" ,r-dbi)
       ("r-keggrest" ,r-keggrest)
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

(define-public r-annotationfilter
  (package
    (name "r-annotationfilter")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationFilter" version))
              (sha256
               (base32
                "0799jja6v7l3jsi26wbjalkr5lriaxid05xb5g36iq93myhaj0sa"))))
    (properties
     `((upstream-name . "AnnotationFilter")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomicranges" ,r-genomicranges)
       ("r-lazyeval" ,r-lazyeval)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/Bioconductor/AnnotationFilter")
    (synopsis "Facilities for filtering Bioconductor annotation resources")
    (description
     "This package provides classes and other infrastructure to implement
filters for manipulating Bioconductor annotation resources.  The filters are
used by @code{ensembldb}, @code{Organism.dplyr}, and other packages.")
    (license license:artistic2.0)))

(define-public r-annotationforge
  (package
    (name "r-annotationforge")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationForge" version))
       (sha256
        (base32
         "1by3diy0y4809k97cw97mp4j177gn1dzhqil8myij1r7b8sk7hax"))))
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

(define-public r-annotationhub
  (package
    (name "r-annotationhub")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationHub" version))
       (sha256
        (base32
         "12i8lafy1z97gs4knqi7r5l1hd7dr6j8a23qj4fkdpqsdpyz21z7"))))
    (properties `((upstream-name . "AnnotationHub")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocfilecache" ,r-biocfilecache)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocmanager" ,r-biocmanager)
       ("r-biocversion" ,r-biocversion)
       ("r-curl" ,r-curl)
       ("r-dplyr" ,r-dplyr)
       ("r-httr" ,r-httr)
       ("r-interactivedisplaybase" ,r-interactivedisplaybase)
       ("r-rappdirs" ,r-rappdirs)
       ("r-rsqlite" ,r-rsqlite)
       ("r-s4vectors" ,r-s4vectors)
       ("r-yaml" ,r-yaml)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-aroma-light
  (package
    (name "r-aroma-light")
    (version "3.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "aroma.light" version))
       (sha256
        (base32
         "1yvq6l1p8cpijvlib4fn9y88ihn0gaalrmgx82jgrfmnszkqn3y5"))))
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

(define-public r-bamsignals
  (package
    (name "r-bamsignals")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bamsignals" version))
       (sha256
        (base32
         "1ljrip0jlxjcljqac7vzvwqbcqil5v4f4s4bhpq4akvdkshas6mn"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rcpp" ,r-rcpp)
       ("r-rhtslib" ,r-rhtslib)
       ("r-zlibbioc" ,r-zlibbioc)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/bamsignals")
    (synopsis "Extract read count signals from bam files")
    (description
     "This package efficiently obtains count vectors from indexed bam
files.  It counts the number of nucleotide sequence reads in given genomic
ranges and it computes reads profiles and coverage profiles.  It also handles
paired-end data.")
    (license license:gpl2+)))

(define-public r-biobase
  (package
    (name "r-biobase")
    (version "2.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biobase" version))
              (sha256
               (base32
                "1sg8w8860zhlz9s1pf75xa8asd2hyqsj13fh5xc37hf5yqdfkavr"))))
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
    (version "2.48.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biomaRt" version))
              (sha256
               (base32
                "096s243yzbhhz3wsm7azml5sznqczmcpi5g0gnb02mah1przczfx"))))
    (properties
     `((upstream-name . "biomaRt")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocfilecache" ,r-biocfilecache)
       ("r-digest" ,r-digest)
       ("r-httr" ,r-httr)
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
    (version "1.26.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocParallel" version))
              (sha256
               (base32
                "1jx1wm47s64ywfddrg8kqzz4xpcmfjwrzbxhvlmys7pf2hzj4gbh"))))
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
    (version "2.60.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biostrings" version))
              (sha256
               (base32
                "0ai0fg0w4l0a7avbafdbqjgjpg91myxalwrg2i3ixm1l2lyyfyah"))))
    (properties
     `((upstream-name . "Biostrings")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-crayon" ,r-crayon)
       ("r-genomeinfodb" ,r-genomeinfodb)
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

(define-public r-biovizbase
  (package
    (name "r-biovizbase")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biovizBase" version))
       (sha256
        (base32
         "14jyyg3ggdhaqhp0j4qf6dapykh76fygbaa4lr7czqbc5mr0iw23"))))
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

(define-public r-bsgenome
  (package
    (name "r-bsgenome")
    (version "1.60.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome" version))
              (sha256
               (base32
                "1jhissp9ad1rn2p0bzr3yslbn84yqbaqgnn5p9hyacwr7mr091cn"))))
    (properties
     `((upstream-name . "BSgenome")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-matrixstats" ,r-matrixstats)
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

(define-public r-category
  (package
    (name "r-category")
    (version "2.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Category" version))
       (sha256
        (base32
         "0f76pb7h8qc51mca5pq00m9p02sbkcj6ywfzli20qai2ykpfr71x"))))
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

(define-public r-chipseeker
  (package
    (name "r-chipseeker")
    (version "1.28.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ChIPseeker" version))
              (sha256
               (base32
                "18hdgml80770c0xgd06zrl8px1ql9fa65rirfkq07z7rzpnd23rw"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-boot" ,r-boot)
       ("r-enrichplot" ,r-enrichplot)
       ("r-iranges" ,r-iranges)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-gtools" ,r-gtools)
       ("r-dplyr" ,r-dplyr)
       ("r-plotrix" ,r-plotrix)
       ("r-dplyr" ,r-dplyr)
       ("r-magrittr" ,r-magrittr)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-txdb-hsapiens-ucsc-hg19-knowngene"
        ,r-txdb-hsapiens-ucsc-hg19-knowngene)))
    (home-page "https://www.bioconductor.org/packages/ChIPseeker/")
    (synopsis "ChIPseeker for ChIP peak annotation, comparison, and visualization")
    (description "This package implements functions to retrieve the nearest
genes around the peak, annotate genomic region of the peak, statstical methods
for estimate the significance of overlap among ChIP peak data sets, and
incorporate GEO database for user to compare the own dataset with those
deposited in database.  The comparison can be used to infer cooperative
regulation and thus can be used to generate hypotheses.  Several visualization
functions are implemented to summarize the coverage of the peak experiment,
average profile and heatmap of peaks binding to TSS regions, genomic
annotation, distance to TSS, and overlap of peaks or genes.")
    (license license:artistic2.0)))

(define-public r-chipseq
  (package
    (name "r-chipseq")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chipseq" version))
       (sha256
        (base32
         "078p9h0zghlhpd6cr54nww1mk7q97imx8yqfayw5m2yq4097ivbi"))))
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

(define-public r-complexheatmap
  (package
    (name "r-complexheatmap")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ComplexHeatmap" version))
       (sha256
        (base32
         "0jl96msj1njdrvngg68s50vmphvhi2lfwlv34x07pcdzgkjjs41f"))))
    (properties
     `((upstream-name . "ComplexHeatmap")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cairo" ,r-cairo)
       ("r-circlize" ,r-circlize)
       ("r-clue" ,r-clue)
       ("r-colorspace" ,r-colorspace)
       ("r-digest" ,r-digest)
       ("r-doparallel" ,r-doparallel)
       ("r-foreach" ,r-foreach)
       ("r-getoptlong" ,r-getoptlong)
       ("r-globaloptions" ,r-globaloptions)
       ("r-iranges" ,r-iranges)
       ("r-matrixstats" ,r-matrixstats)
       ("r-png" ,r-png)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page
     "https://github.com/jokergoo/ComplexHeatmap")
    (synopsis "Making Complex Heatmaps")
    (description
     "Complex heatmaps are efficient to visualize associations between
different sources of data sets and reveal potential structures.  This package
provides a highly flexible way to arrange multiple heatmaps and supports
self-defined annotation graphics.")
    (license license:gpl2+)))

(define-public r-copywriter
  (package
    (name "r-copywriter")
    (version "2.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CopywriteR" version))
       (sha256
        (base32
         "0pacs714d9b1fdz68pp9ca0x77d376s19lxb82np4l9fgx0rgkxp"))))
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

(define-public r-deseq
  (package
    (name "r-deseq")
    (version "1.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq" version))
       (sha256
        (base32
         "047hph5aqmjnz1aqprziw0smdn5lf96hmwpnvqrxv1j2yfvcf3h1"))))
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
    (home-page "https://www-huber.embl.de/users/anders/DESeq/")
    (synopsis "Differential gene expression analysis")
    (description
     "This package provides tools for estimating variance-mean dependence in
count data from high-throughput genetic sequencing assays and for testing for
differential expression based on a model using the negative binomial
distribution.")
    (license license:gpl3+)))

(define-public r-deseq2
  (package
    (name "r-deseq2")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq2" version))
       (sha256
        (base32
         "0r1brwmj7av0bj72jajn27vx3zs1bgg8qfbhf02fln6kf7im4kaz"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DEXSeq" version))
       (sha256
        (base32
         "1zywh30f4j4rj0f9w6yk5xr9mvdbg8gicy3wsb8yxdnamadyr7x4"))))
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

(define-public r-dirichletmultinomial
  (package
    (name "r-dirichletmultinomial")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DirichletMultinomial" version))
       (sha256
        (base32
         "0ikmj0300lfzj6q1vyahfyx5kwi5h59mds7ym4f2j1bbxqzy6ssl"))))
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

(define-public r-edaseq
  (package
    (name "r-edaseq")
    (version "2.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EDASeq" version))
       (sha256
        (base32
         "0pakcbkalhhqz3d9lpfx3hscf53k24mlmrywxxzfg43yq57srkql"))))
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
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-shortread" ,r-shortread)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-edger
  (package
    (name "r-edger")
    (version "3.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "edgeR" version))
              (sha256
               (base32
                "1ikl9y6hj2p92nrb1ydxy2410b3wrax83rfy2imaj0vgfmhsgx6g"))))
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

(define-public r-ensembldb
  (package
    (name "r-ensembldb")
    (version "2.16.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ensembldb" version))
       (sha256
        (base32
         "15yllkxr6sj5pfvkvv285nk3q5374nzq1iz8ywmnrq910k3xagd8"))))
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-fastseg
  (package
    (name "r-fastseg")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fastseg" version))
       (sha256
        (base32
         "006v7qs707xmnr35b7rw135pyvacrmhv55a3c53birkpsrjgkps0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://www.bioinf.jku.at/software/fastseg/index.html")
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

(define-public r-gage
  (package
    (name "r-gage")
    (version "2.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gage" version))
       (sha256
        (base32
         "0z7hrwdm6my6p7z04bcpfhqk72pd0s1bdzvsiiym59qj79fbvb83"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-go-db" ,r-go-db)
       ("r-graph" ,r-graph)
       ("r-keggrest" ,r-keggrest)))
    (home-page (string-append "https://bmcbioinformatics.biomedcentral.com/"
                              "articles/10.1186/1471-2105-10-161"))
    (synopsis "Generally applicable gene-set enrichment for pathway analysis")
    (description
     "GAGE is a published method for gene set (enrichment or GSEA) or pathway
analysis.  GAGE is generally applicable independent of microarray or RNA-Seq
data attributes including sample sizes, experimental designs, assay platforms,
and other types of heterogeneity.  The gage package provides functions for
basic GAGE analysis, result processing and presentation.  In addition, it
provides demo microarray data and commonly used gene set data based on KEGG
pathways and GO terms.  These functions and data are also useful for gene set
analysis using other methods.")
    (license license:gpl2+)))

(define-public r-genefilter
  (package
    (name "r-genefilter")
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "genefilter" version))
       (sha256
        (base32
         "0dy9pmlb0pc9b4ks5fb9zgnmhc9f2mkqmsdlb7f5z88xmj68y4qk"))))
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

(define-public r-geneoverlap
  (package
    (name "r-geneoverlap")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GeneOverlap" version))
              (sha256
               (base32
                "1kfw3h68rvbafhklds6sfmviwv91nms8wk0ywzkjg5h3mmgxbsv9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-gplots" ,r-gplots)))
    (home-page "https://www.bioconductor.org/packages/GeneOverlap/")
    (synopsis "Test and visualize gene overlaps")
    (description "This package can be used to test two sets of gene lists
and visualize the results.")
    (license license:gpl3)))

(define-public r-genomation
  (package
    (name "r-genomation")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "genomation" version))
              (sha256
               (base32
                "0qa3b4mfd7l1sy4pw64zr2d90y5apah900krxjl4x39acygg0i2r"))))
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
       ("r-s4vectors" ,r-s4vectors)
       ("r-seqpattern" ,r-seqpattern)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioinformatics.mdc-berlin.de/genomation/")
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

(define-public r-genomeinfodb
  (package
    (name "r-genomeinfodb")
    (version "1.28.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomeInfoDb" version))
              (sha256
               (base32
                "1ga8yrn7j1wn9wdsvf4ws6n2987yk1yxz22v2jzaszfikhjh1sp8"))))
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

(define-public r-genomicalignments
  (package
    (name "r-genomicalignments")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicAlignments" version))
              (sha256
               (base32
                "00rq110jkh89nxgk05zh8kssxk8mb4dq0wjg3n7ivfmmm9wdwhp2"))))
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

(define-public r-genomicfeatures
  (package
    (name "r-genomicfeatures")
    (version "1.44.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicFeatures" version))
              (sha256
               (base32
                "0byizkq18kkyq3n604f38z4mikhi3szsrfrlz22wdq2ldq3nzkis"))))
    (properties
     `((upstream-name . "GenomicFeatures")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocio" ,r-biocio)
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-genomicfiles
  (package
    (name "r-genomicfiles")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicFiles" version))
       (sha256
        (base32
         "1plh14m7w6calw5yxcxp7g4bg8q00ax85m517wap78bni975k13y"))))
    (properties `((upstream-name . "GenomicFiles")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-matrixgenerics" ,r-matrixgenerics)
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

(define-public r-genomicranges
  (package
    (name "r-genomicranges")
    (version "1.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicRanges" version))
              (sha256
               (base32
                "1qxc6dcwdlkan3hg0j9yfhz7gyi9qg671yj6zizsk6mzl7qqva0x"))))
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
    (version "2.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOstats" version))
       (sha256
        (base32
         "0jjswy6qmfgr2f6vk3y9pdvs9x91gn31h55qllgh0qb2cb26g9wa"))))
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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GSEABase" version))
       (sha256
        (base32
         "0946kkykms79mqnx262q20xzrrhv7cv723xh378335ff41qyf63n"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hpar" version))
       (sha256
        (base32
         "0q5pp04rq2gsh1kibvp8bvjkqc1kb46qpnj6agqp2vyqhrrfrm99"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/hpar/")
    (synopsis "Human Protein Atlas in R")
    (description "This package provides a simple interface to and data from
the Human Protein Atlas project.")
    (license license:artistic2.0)))

(define-public r-rhtslib
  (package
    (name "r-rhtslib")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhtslib" version))
       (sha256
        (base32
         "0acpgv98529ic2i2k03knz05akb6h51wbz7sr5zgi8gk4nmawrlf"))))
    (properties `((upstream-name . "Rhtslib")))
    (build-system r-build-system)
    ;; Without this a temporary directory ends up in the Rhtslib.so binary,
    ;; which makes R abort the build.
    (arguments '(#:configure-flags '("--no-staged-install")))
    (propagated-inputs
     `(("curl" ,curl)
       ("zlib" ,zlib) ; packages using rhtslib need to link with zlib
       ("r-zlibbioc" ,r-zlibbioc)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("r-knitr" ,r-knitr)))
    (home-page "https://github.com/nhayden/Rhtslib")
    (synopsis "High-throughput sequencing library as an R package")
    (description
     "This package provides the HTSlib C library for high-throughput
nucleotide sequence analysis.  The package is primarily useful to developers
of other R packages who wish to make use of HTSlib.")
    (license license:lgpl2.0+)))

(define-public r-impute
  (package
    (name "r-impute")
    (version "1.66.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "impute" version))
              (sha256
               (base32
                "0pprkv25avxhllddcssvqvy3nibmqkfwaq4xnlhka7858gyiyd1k"))))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/impute")
    (synopsis "Imputation for microarray data")
    (description
     "This package provides a function to impute missing gene expression
microarray data, using nearest neighbor averaging.")
    (license license:gpl2+)))

(define-public r-interactivedisplaybase
  (package
    (name "r-interactivedisplaybase")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "interactiveDisplayBase" version))
       (sha256
        (base32
         "00rgrrmglslgb6j7whp0m5dlyl4436r647br05rrpv8cxrmbs2iv"))))
    (properties
     `((upstream-name . "interactiveDisplayBase")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-dt" ,r-dt)
       ("r-shiny" ,r-shiny)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/interactiveDisplayBase")
    (synopsis "Base package for web displays of Bioconductor objects")
    (description
     "This package contains the basic methods needed to generate interactive
Shiny-based display methods for Bioconductor objects.")
    (license license:artistic2.0)))

(define-public r-keggrest
  (package
    (name "r-keggrest")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGREST" version))
       (sha256
        (base32
         "15313d20v3ziyn52fhc6fbzcy1kxjkdx18124bxhdfd14f4aypcd"))))
    (properties `((upstream-name . "KEGGREST")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-httr" ,r-httr)
       ("r-png" ,r-png)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/KEGGREST")
    (synopsis "Client-side REST access to KEGG")
    (description
     "This package provides a package that provides a client interface to the
@dfn{Kyoto Encyclopedia of Genes and Genomes} (KEGG) REST server.")
    (license license:artistic2.0)))

(define-public r-limma
  (package
    (name "r-limma")
    (version "3.48.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "limma" version))
              (sha256
               (base32
                "0385ac0hvvml99krcgcpp6q7layjhzgp9xcxkcjrpfg5mjg1z4sx"))))
    (build-system r-build-system)
    (home-page "http://bioinf.wehi.edu.au/limma")
    (synopsis "Package for linear models for microarray and RNA-seq data")
    (description "This package can be used for the analysis of gene expression
studies, especially the use of linear models for analysing designed experiments
and the assessment of differential expression.  The analysis methods apply to
different technologies, including microarrays, RNA-seq, and quantitative PCR.")
    (license license:gpl2+)))

(define-public r-methylkit
  (package
    (name "r-methylkit")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "methylKit" version))
              (sha256
               (base32
                "147nag4rz2xpjkkf8rmhja9k4ixjj1hsb0lq3lw7mw6q67zxsvf3"))))
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
       ("r-mgcv" ,r-mgcv)
       ("r-qvalue" ,r-qvalue)
       ("r-r-utils" ,r-r-utils)
       ("r-rcpp" ,r-rcpp)
       ("r-rhtslib" ,r-rhtslib)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-zlibbioc" ,r-zlibbioc)))
    (native-inputs
     `(("r-knitr" ,r-knitr))) ; for vignettes
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

(define-public r-motifrg
  (package
    (name "r-motifrg")
    (version "1.31.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifRG" version))
       (sha256
        (base32
         "1ml6zyzlk8yjbnfhga2qnw8nl43rankvka0kc1yljxr2b66aqbhn"))))
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

(define-public r-mutationalpatterns
  (package
    (name "r-mutationalpatterns")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MutationalPatterns" version))
       (sha256
        (base32
         "0433i4xbz8hrfaj8fxgzps3x8dqrl5vgwzg7qmp4cy5sb1lw5wvs"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ;; These two packages are suggested packages
       ("r-bsgenome-hsapiens-1000g" ,r-bsgenome-hsapiens-1000genomes-hs37d5)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-cowplot" ,r-cowplot)
       ("r-dplyr" ,r-dplyr)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggalluvial" ,r-ggalluvial)
       ("r-ggdendro" ,r-ggdendro)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iranges" ,r-iranges)
       ("r-magrittr" ,r-magrittr)
       ("r-nmf" ,r-nmf)
       ("r-pracma" ,r-pracma)
       ("r-purrr" ,r-purrr)
       ("r-s4vectors" ,r-s4vectors)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://bioconductor.org/packages/MutationalPatterns/")
    (synopsis "Extract and visualize mutational patterns in genomic data")
    (description "This package provides an extensive toolset for the
characterization and visualization of a wide range of mutational patterns
in SNV base substitution data.")
    (license license:expat)))

(define-public r-msnbase
  (package
    (name "r-msnbase")
    (version "2.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnbase" version))
       (sha256
        (base32
         "1z7s17j6zgb70m0khyf9icqlnbnzlivca7vw7j0vxyw417ld9lkr"))))
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
       ("r-mscoreutils" ,r-mscoreutils)
       ("r-mzid" ,r-mzid)
       ("r-mzr" ,r-mzr)
       ("r-pcamethods" ,r-pcamethods)
       ("r-plyr" ,r-plyr)
       ("r-protgenerics" ,r-protgenerics)
       ("r-rcpp" ,r-rcpp)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scales" ,r-scales)
       ("r-vsn" ,r-vsn)
       ("r-xml" ,r-xml)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/lgatto/MSnbase")
    (synopsis "Base functions and classes for MS-based proteomics")
    (description
     "This package provides basic plotting, data manipulation and processing
of mass spectrometry based proteomics data.")
    (license license:artistic2.0)))

(define-public r-msnid
  (package
    (name "r-msnid")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnID" version))
       (sha256
        (base32
         "0r3vgigf4fk0xzfg8gcvad01jdh0fysh6x22m9qy77x6glyrxcj2"))))
    (properties `((upstream-name . "MSnID")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationhub" ,r-annotationhub)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocstyle" ,r-biocstyle)
       ("r-biostrings" ,r-biostrings)
       ("r-data-table" ,r-data-table)
       ("r-doparallel" ,r-doparallel)
       ("r-dplyr" ,r-dplyr)
       ("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iterators" ,r-iterators)
       ("r-msnbase" ,r-msnbase)
       ("r-msmstests" ,r-msmstests)
       ("r-mzid" ,r-mzid)
       ("r-mzr" ,r-mzr)
       ("r-protgenerics" ,r-protgenerics)
       ("r-purrr" ,r-purrr)
       ("r-r-cache" ,r-r-cache)
       ("r-rcpp" ,r-rcpp)
       ("r-reshape2" ,r-reshape2)
       ("r-rlang" ,r-rlang)
       ("r-runit" ,r-runit)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-xtable" ,r-xtable)))
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

(define-public r-mzid
  (package
    (name "r-mzid")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzID" version))
       (sha256
        (base32
         "1wsy6frxa5602jqb1nlqv39mzgpid8wfyvb9m2jb6srv7p59rgys"))))
    (properties `((upstream-name . "mzID")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-doparallel" ,r-doparallel)
       ("r-foreach" ,r-foreach)
       ("r-iterators" ,r-iterators)
       ("r-plyr" ,r-plyr)
       ("r-protgenerics" ,r-protgenerics)
       ("r-xml" ,r-xml)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/mzID")
    (synopsis "Parser for mzIdentML files")
    (description
     "This package provides a parser for mzIdentML files implemented using the
XML package.  The parser tries to be general and able to handle all types of
mzIdentML files with the drawback of having less pretty output than a vendor
specific parser.")
    (license license:gpl2+)))

(define-public r-mzr
  (package
    (name "r-mzr")
    (version "2.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzR" version))
       (sha256
        (base32
         "0z4cz6lir9gwzy0hxwv03wv36fkkfdb97p9wv4af020k0zkp3ipr"))
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
               (("PKG_LIBS=") "PKG_LIBS=$(BOOST_LIBS) ")
               (("\\ARCH_OBJS=" line)
                (string-append line
                               "\nBOOST_LIBS=-lboost_system -lboost_regex \
-lboost_iostreams -lboost_thread -lboost_filesystem -lboost_chrono\n")))
             #t)))))
    (inputs
     `(;; Our default boost package won't work here, unfortunately, even with
       ;; mzR version 2.26.1.
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
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-organismdbi
  (package
    (name "r-organismdbi")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "OrganismDbi" version))
       (sha256
        (base32
         "0p8ccpdchdwci4kv9y07wdadzgms8nipvg6rm1rll35jcflnnkxi"))))
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

(define-public r-pcamethods
  (package
    (name "r-pcamethods")
    (version "1.84.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pcaMethods" version))
       (sha256
        (base32
         "1ccqsxn487dy92c2d1iffh9917z4zq5ia92zv2h6pi00jjc6ymb5"))))
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

(define-public r-protgenerics
  (package
    (name "r-protgenerics")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ProtGenerics" version))
       (sha256
        (base32
         "1c79k8nc28782w5gxi7pkm8jmddq1hdw6khs9kgsp8dyk60ak6fq"))))
    (properties `((upstream-name . "ProtGenerics")))
    (build-system r-build-system)
    (home-page "https://github.com/lgatto/ProtGenerics")
    (synopsis "S4 generic functions for proteomics infrastructure")
    (description
     "This package provides S4 generic functions needed by Bioconductor
proteomics packages.")
    (license license:artistic2.0)))

(define-public r-rbgl
  (package
    (name "r-rbgl")
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RBGL" version))
       (sha256
        (base32
         "0l40ibf8g5s4ay83s92l198jjqc5l09hcmxqcjrpifvp5pjf9yy5"))))
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

(define-public r-rcas
  (package
    (name "r-rcas")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RCAS" version))
              (sha256
               (base32
                "0l92v870ndna8zjqwzf22fb9vyhkh6942v4gaiqr1yc4qr521p5p"))))
    (properties `((upstream-name . "RCAS")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-cowplot" ,r-cowplot)
       ("r-data-table" ,r-data-table)
       ("r-dt" ,r-dt)
       ("r-genomation" ,r-genomation)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggseqlogo" ,r-ggseqlogo)
       ("r-gprofiler2" ,r-gprofiler2)
       ("r-iranges" ,r-iranges)
       ("r-knitr" ,r-knitr)
       ("r-pbapply" ,r-pbapply)
       ("r-pheatmap" ,r-pheatmap)
       ("r-plotly" ,r-plotly)
       ("r-plotrix" ,r-plotrix)
       ("r-proxy" ,r-proxy)
       ("r-ranger" ,r-ranger)
       ("r-rsqlite" ,r-rsqlite)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-s4vectors" ,r-s4vectors)
       ("pandoc" ,pandoc)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (synopsis "RNA-centric annotation system")
    (description
     "RCAS aims to be a standalone RNA-centric annotation system that provides
intuitive reports and publication-ready graphics.  This package provides the R
library implementing most of the pipeline's features.")
    (home-page "https://github.com/BIMSBbioinfo/RCAS")
    (license license:artistic2.0)))

(define-public r-regioner
  (package
    (name "r-regioner")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "regioneR" version))
       (sha256
        (base32
         "0xzzaz3cl6pyxfsg0d931v8k15wbd05s5mnsb7igxldc5qqg3nsl"))))
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
    (version "2.32.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReportingTools" version))
       (sha256
        (base32
         "0wq9y649dh1am6djzz0xlz42428xsgw2bdx1dknhdw2xbydmmx47"))))
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

(define-public r-rhdf5
  (package
    (name "r-rhdf5")
    (version "2.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rhdf5" version))
              (sha256
               (base32
                "1a5kw9ry9cr258al0x3q0ss5xn1ymscdypx51vzzgzamhy7dqakz"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rhdf5filters" ,r-rhdf5filters)
       ("r-rhdf5lib" ,r-rhdf5lib)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-rhdf5filters
  (package
    (name "r-rhdf5filters")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rhdf5filters" version))
       (sha256
        (base32
         "133v0s452acspi4dbf6gsa2xrr0qza86jdjjbpwhdv6zfd1djbgc"))))
    (properties `((upstream-name . "rhdf5filters")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rhdf5lib" ,r-rhdf5lib)))
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/grimbough/rhdf5filters")
    (synopsis "HDF5 compression filters")
    (description
     "This package provides a collection of compression filters for use with
HDF5 datasets.")
    (license license:bsd-2)))

(define-public r-rsamtools
  (package
    (name "r-rsamtools")
    (version "2.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Rsamtools" version))
              (sha256
               (base32
                "0arhh5bbx3pmxmkh5sjgczcswqy83d35r7cjhd2knpczdvrixaq5"))))
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
               (("import\\(zlibbioc\\)") "")))))))
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

;; This is a CRAN package, but it depends on a Bioconductor package:
;; s4vectors.
(define-public r-restfulr
  (package
    (name "r-restfulr")
    (version "0.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "restfulr" version))
       (sha256
        (base32
         "1dk45mzrr6ka92yjz7hfhkj12kpx1wg4szv1h1mg80mgga4ganbv"))))
    (properties `((upstream-name . "restfulr")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcurl" ,r-rcurl)
       ("r-rjson" ,r-rjson)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xml" ,r-xml)
       ("r-yaml" ,r-yaml)))
    (home-page "https://cran.r-project.org/package=restfulr")
    (synopsis "R interface to RESTful web services")
    (description
     "This package models a RESTful service as if it were a nested R list.")
    (license license:artistic2.0)))

(define-public r-rtracklayer
  (package
    (name "r-rtracklayer")
    (version "1.52.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rtracklayer" version))
              (sha256
               (base32
                "11w6dx09pb49lin1gr9q88xn7ixh9jd5z6m9z27djchm0nw10lx9"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-zlib
           (lambda _
             (substitute* "DESCRIPTION"
               ((" zlibbioc,") ""))
             (substitute* "NAMESPACE"
               (("import\\(zlibbioc\\)") "")))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocio" ,r-biocio)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rcurl" ,r-rcurl)
       ("r-restfulr" ,r-restfulr)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xml" ,r-xml)
       ("r-xvector" ,r-xvector)
       ("r-zlibbioc" ,r-zlibbioc)))
    (home-page "https://bioconductor.org/packages/rtracklayer")
    (synopsis "R interface to genome browsers and their annotation tracks")
    (description
     "rtracklayer is an extensible framework for interacting with multiple
genome browsers (currently UCSC built-in) and manipulating annotation tracks
in various formats (currently GFF, BED, bedGraph, BED15, WIG, BigWig and 2bit
built-in).  The user may export/import tracks to/from the supported browsers,
as well as query and modify the browser state, such as the current viewport.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on a Bioconductor package.
(define-public r-samr
  (package
    (name "r-samr")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "samr" version))
       (sha256
        (base32
         "01km0f7qgm73x19vbvsxl083hs1dq4dj8qm5h64cxbf20b08my15"))))
    (properties `((upstream-name . "samr")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gsa" ,r-gsa)
       ("r-impute" ,r-impute)
       ("r-matrixstats" ,r-matrixstats)
       ("r-openxlsx" ,r-openxlsx)
       ("r-shiny" ,r-shiny)
       ("r-shinyfiles" ,r-shinyfiles)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://statweb.stanford.edu/~tibs/SAM/")
    (synopsis "Significance analysis of Microarrays")
    (description
     "This is a package for significance analysis of Microarrays for
differential expression analysis, RNAseq data and related problems.")
    ;; Any version of the LGPL
    (license license:lgpl3+)))

(define-public r-seqlogo
  (package
    (name "r-seqlogo")
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqLogo" version))
       (sha256
        (base32
         "1253sj1hc6bbrff0iv5xa3v9znqvisll0fy6fdjka9c778fn4mcp"))))
    (properties `((upstream-name . "seqLogo")))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/seqLogo")
    (synopsis "Sequence logos for DNA sequence alignments")
    (description
     "seqLogo takes the position weight matrix of a DNA sequence motif and
plots the corresponding sequence logo as introduced by Schneider and
Stephens (1990).")
    (license license:lgpl2.0+)))

(define-public r-seqpattern
  (package
    (name "r-seqpattern")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "seqPattern" version))
              (sha256
               (base32
                "0h74z84zyvfzclsm0g13b95hirn99185wc6lp53jkzah9yyi59ay"))))
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

(define-public r-shortread
  (package
    (name "r-shortread")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ShortRead" version))
       (sha256
        (base32
         "0zqinw3c6h5v1c5nhzkiziirws16nbviccgw8nj2d22r33dbqwp3"))))
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

(define-public r-trajectoryutils
  (package
    (name "r-trajectoryutils")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TrajectoryUtils" version))
       (sha256
        (base32
         "1b7mg3ypp1ay98cav47h9vn692lx0n9b5b0hpansgnkr5prb823b"))))
    (properties
     `((upstream-name . "TrajectoryUtils")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-igraph" ,r-igraph)
       ("r-matrix" ,r-matrix)
       ("r-s4vectors" ,r-s4vectors)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/TrajectoryUtils")
    (synopsis "Single-cell trajectory analysis utilities")
    (description
     "This package implements low-level utilities for single-cell trajectory
analysis, primarily intended for re-use inside higher-level packages.  It
includes a function to create a cluster-level minimum spanning tree and data
structures to hold pseudotime inference results.")
    (license license:gpl3)))

(define-public r-slingshot
  (package
   (name "r-slingshot")
   (version "2.0.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "slingshot" version))
            (sha256
             (base32
              "1aqsz2kxwax8d8d5iv3zk5hlyk5aya1wbxs1wky2rgccw4d35whx"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-igraph" ,r-igraph)
      ("r-matrixstats" ,r-matrixstats)
      ("r-princurve" ,r-princurve)
      ("r-s4vectors" ,r-s4vectors)
      ("r-singlecellexperiment" ,r-singlecellexperiment)
      ("r-summarizedexperiment" ,r-summarizedexperiment)
      ("r-trajectoryutils" ,r-trajectoryutils)))
   (native-inputs
    `(("r-knitr" ,r-knitr)))
   (home-page "https://bioconductor.org/packages/slingshot")
   (synopsis "Tools for ordering single-cell sequencing")
   (description "This package provides functions for inferring continuous,
branching lineage structures in low-dimensional data.  Slingshot was designed
to model developmental trajectories in single-cell RNA sequencing data and
serve as a component in an analysis pipeline after dimensionality reduction
and clustering.  It is flexible enough to handle arbitrarily many branching
events and allows for the incorporation of prior knowledge through supervised
graph construction.")
   (license license:artistic2.0)))

(define-public r-structuralvariantannotation
  (package
    (name "r-structuralvariantannotation")
    (version "1.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "StructuralVariantAnnotation" version))
       (sha256
        (base32 "04ac4mjh3pgdlws0aiacqg0vd7bhg890w44r7b90p947c3rk1mfw"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-dplyr" ,r-dplyr)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rlang" ,r-rlang)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-stringr" ,r-stringr)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/StructuralVariantAnnotation/")
    (synopsis "R package designed to simplify structural variant analysis")
    (description
     "This package contains useful helper functions for dealing with structural
variants in VCF format.  The packages contains functions for parsing VCFs from
a number of popular callers as well as functions for dealing with breakpoints
involving two separate genomic loci encoded as GRanges objects.")
    (license license:gpl3)))

(define-public r-summarizedexperiment
  (package
    (name "r-summarizedexperiment")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SummarizedExperiment" version))
              (sha256
               (base32
                "16np5ik6jgbi68mhlib6yskywwbaa50mlr7m3sh1hqk889whfn1g"))))
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
       ("r-matrixgenerics" ,r-matrixgenerics)
       ("r-s4vectors" ,r-s4vectors)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/SummarizedExperiment")
    (synopsis "Container for representing genomic ranges by sample")
    (description
     "The SummarizedExperiment container contains one or more assays, each
represented by a matrix-like object of numeric or other mode.  The rows
typically represent genomic ranges of interest and the columns represent
samples.")
    (license license:artistic2.0)))

(define-public r-sva
  (package
    (name "r-sva")
    (version "3.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "sva" version))
       (sha256
        (base32
         "12jkcybdfspabh7x124d44l9fj1hwwg3gvcqxvz5wpkiflc2vkji"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-edger" ,r-edger)
       ("r-genefilter" ,r-genefilter)
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

(define-public r-systempiper
  (package
    (name "r-systempiper")
    (version "1.26.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "systemPipeR" version))
       (sha256
        (base32
         "01l35l5zj87qkarrbal9la6kshk3j7k8hy3iimv3gdnnz4axmvs7"))))
    (properties `((upstream-name . "systemPipeR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-assertthat" ,r-assertthat)
       ("r-batchtools" ,r-batchtools)
       ("r-biostrings" ,r-biostrings)
       ("r-crayon" ,r-crayon)
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
       ("r-s4vectors" ,r-s4vectors)
       ("r-shortread" ,r-shortread)
       ("r-stringr" ,r-stringr)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-testthat" ,r-testthat)
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

(define-public r-topgo
  (package
    (name "r-topgo")
    (version "2.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "topGO" version))
              (sha256
               (base32
                "1ggi7yrhkyi85p3sfj3yd95n9mzq1xpff28ixa4dl9yzasks1v5a"))))
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

(define-public r-tximport
  (package
    (name "r-tximport")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tximport" version))
              (sha256
               (base32
                "0ip2yr0zspf2aagskxl4dwncr48dw5qb90an3sswnnh2dqdb82if"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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

(define-public r-variantannotation
  (package
    (name "r-variantannotation")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "VariantAnnotation" version))
              (sha256
               (base32
                "0c9r00j8a3bs6n0dv4wi17jc1ljzvr3r2bi4h9axhcsf2ip906rh"))))
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

(define-public r-vsn
  (package
    (name "r-vsn")
    (version "3.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "vsn" version))
       (sha256
        (base32
         "0nppph3kv8z83368snb8s3n4vcqj829yyi1kh4q09qvq18rhvssv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-affy" ,r-affy)
       ("r-biobase" ,r-biobase)
       ("r-ggplot2" ,r-ggplot2)
       ("r-lattice" ,r-lattice)
       ("r-limma" ,r-limma)))
    (native-inputs
     `(("r-knitr" ,r-knitr))) ; for vignettes
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

(define-public r-xvector
  (package
    (name "r-xvector")
    (version "0.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "XVector" version))
              (sha256
               (base32
                "1cw34gd9iaspl0v737xl7rngq63zrj03a5ngai15ggrnv1sq2aqr"))))
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

(define-public r-zlibbioc
  (package
    (name "r-zlibbioc")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "zlibbioc" version))
              (sha256
               (base32
                "1dv5bf12kzk1yzyfs3g5syim16sbi44kalvzj2i2xcnxnl6x60ip"))))
    (properties
     `((upstream-name . "zlibbioc")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/zlibbioc")
    (synopsis "Provider for zlib-1.2.5 to R packages")
    (description "This package uses the source code of zlib-1.2.5 to create
libraries for systems that do not have these available via other means.")
    (license license:artistic2.0)))

(define-public r-geneplotter
  (package
    (name "r-geneplotter")
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geneplotter" version))
       (sha256
        (base32
         "1d085lfa3yif5wkys1fb0zzgg0cqwd1y18vasgqqdr6rva075d4z"))))
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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligoClasses" version))
       (sha256
        (base32
         "1nsfyfpj264h6y322pzz0i001b0m862j3skbq5rjwlrj1p8j2gi7"))))
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
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligo" version))
       (sha256
        (base32
         "0fyq77im6s79havjwbchhqhnllxs134jsi98g6msxz66h16lj3al"))))
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
    (version "2.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "qvalue" version))
       (sha256
        (base32
         "0xssanffh1hr7f48mnazcpwi25rdp7mxlyb9nbf4q2mp7m40jnpm"))))
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
   (version "1.14.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "apeglm" version))
            (sha256
             (base32
              "1xld6ar440achik4dbd7vhiw6jfj0sb96jm52n7hav5bv4gag3mh"))))
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
   (version "1.24.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "GreyListChIP" version))
            (sha256
             (base32
              "1g9ja8p90czx83ar0l9ran7m6aggvszdbqm714fq7z4rxq9b4hs3"))))
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
    (version "3.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DiffBind" version))
       (sha256
        (base32
         "1rp4sgx58g1lq5brpx07wffllhvsqq1097vrjiaksbih08338nih"))))
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

(define-public r-mbkmeans
  (package
    (name "r-mbkmeans")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "mbkmeans" version))
              (sha256
               (base32
                "1k7ngpx4s50jplrsv19zzjr7izpdl9wwppb218ih5cp0ki1gcc2n"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-benchmarkme" ,r-benchmarkme)
       ("r-biocparallel" ,r-biocparallel)
       ("r-clusterr" ,r-clusterr)
       ("r-delayedarray" ,r-delayedarray)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rhdf5lib" ,r-rhdf5lib)
       ("r-s4vectors" ,r-s4vectors)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/mbkmeans")
    (synopsis "Mini-batch k-means clustering for single-cell RNA-seq")
    (description "This package implements the mini-batch k-means algorithm for
large datasets, including support for on-disk data representation.")
    (license license:expat)))

(define-public r-multtest
  (package
    (name "r-multtest")
    (version "2.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "multtest" version))
       (sha256
        (base32
         "1wi15v4llgv11hpb2j9h4a35nrnawnmvbz5d5dvgy8lhqrlq8w9a"))))
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
    (version "1.70.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "graph" version))
              (sha256
               (base32
                "1i7s198d5kw4gk6nqqsd3vqaknj4493p822f2za8q95gv6x02rxa"))))
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
    (version "3.26.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPpeakAnno" version))
       (sha256
        (base32
         "07dvg3r4kghkqhh1a8rw149hgfswmzdh9cvnam8c82006cpmi74s"))))
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
       ("r-interactionset" ,r-interactionset)
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
   (version "1.4.2")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MatrixGenerics" version))
            (sha256
             (base32
              "0kjsc5ghcplay4a74ffpwsf3kbp51x6rl5265gvlfchdwrawkzd2"))))
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
    (version "1.70.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "marray" version))
              (sha256
               (base32 "0wpmrhqpyv27h1hn31hzz21r74r7yqx79ljv8i8gn6ix8lf5ca56"))))
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
   (version "1.52.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHbase" version))
            (sha256
             (base32 "1p87k4vw981k97d9bckmprrfg55jwd91658rgxzjj8hnschf28a4"))))
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
   (version "2.54.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHcall" version))
            (sha256
             (base32 "1zik04y2q0anzy85l0b4ryzkxpfx2fnmpwp5s7akyk1jfl2r8gw7"))))
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
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "QDNAseq" version))
              (sha256
               (base32 "1p4544xlarkbgs9kybrrawq3v7qr6ix62nrc6l7dcksh2ka69yzf"))))
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
    (version "2.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "baySeq" version))
       (sha256
        (base32
         "0hyc0sl2nf18bz0jxbzim0v41zwh2rnnr7l3p6zkk9wnw5gn7bbc"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPComp" version))
       (sha256
        (base32
         "1kfxjh1mynrgqsy2q6byf03cbymqc8w7l7672iyd0wj5qzlif4h5"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RiboProfiling" version))
       (sha256
        (base32
         "1jmd8yrv7p7hn4gdibg3svmhqxjyrnfp7cgsqg8zv862lgd75zsl"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "riboSeqR" version))
       (sha256
        (base32
         "1m5w2j35wr0lzwir4s58z757vrcsj5mglsqrkvs241k0hlmn06qa"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "InteractionSet" version))
       (sha256
        (base32
         "034xidjmn67n1471fnpdmz7gjm6p05cj8sp9nssc3gxdzn54a6xb"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicInteractions" version))
       (sha256
        (base32
         "0zjl7rp5fk14kqsx0bkbpq6hqahbkiyvwa9aggp4kfb8hnmz9qal"))))
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
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ctc" version))
       (sha256
        (base32
         "1v8aysvy6a3r1iafc3xvk885c128kb3pb9zpcdhdjcn0by96k8hh"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "goseq" version))
       (sha256
        (base32
         "07qrxssx4rb8r958r1smx8xfpzdxpp55hci3201hcmz3mxz77i0s"))))
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
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Glimma" version))
       (sha256
        (base32
         "0dsk8qmwimzmd1x4k4jwg9q11jm1ahn8cw0gzd6s2gmigfls4hsa"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROTS" version))
       (sha256
        (base32
         "18wyi73l95df182vg3m014sxwdbpggr61vsbazhyw4vyx2fnzmpl"))))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "plgem" version))
       (sha256
        (base32
         "1rz5jk5bgpk7gjknx79jyslahjg46q2f4bx6dgd0vwmarc29a45z"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "INSPEcT" version))
       (sha256
        (base32
         "0jh5db9dv5pb6b50sg22x8q55m3h0h0nkmb9mivvvp22dhyrd82z"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNABarcodes" version))
       (sha256
        (base32
         "1wiqmzjcb7flp7ldcgbx91asxxrmm1rg9pcfljniab9xcsldhksp"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RUVSeq" version))
       (sha256
        (base32
         "1fy0k1p0m209lzjpd5jhfnifa22lrn63qq3a3kn5g0xhbbmijzac"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocNeighbors" version))
       (sha256
        (base32
         "0cjidi18wjip9xzx83890wjk40vvjq06prf1ag4m2kac47w01r7v"))))
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

(define-public r-scaledmatrix
  (package
    (name "r-scaledmatrix")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ScaledMatrix" version))
       (sha256
        (base32
         "1j96fvw1waqxbv5c8myfmhsidq370z03yz13yqmrs4nn1rpn1a06"))))
    (properties `((upstream-name . "ScaledMatrix")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-delayedarray" ,r-delayedarray)
       ("r-matrix" ,r-matrix)
       ("r-s4vectors" ,r-s4vectors)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/LTLA/ScaledMatrix")
    (synopsis "Create a DelayedMatrix of scaled and centered values")
    (description
     "This package provides delayed computation of a matrix of scaled and
centered values.  The result is equivalent to using the @code{scale} function
but avoids explicit realization of a dense matrix during block processing.
This permits greater efficiency in common operations, most notably matrix
multiplication.")
    (license license:gpl3)))

(define-public r-treeio
  (package
    (name "r-treeio")
    (version "1.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "treeio" version))
       (sha256
        (base32
         "1y60yvg1rl21cab9xrkns0209pg44mwr90qj099dvk97wsjkx67g"))))
    (properties `((upstream-name . "treeio")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ape" ,r-ape)
       ("r-dplyr" ,r-dplyr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-magrittr" ,r-magrittr)
       ("r-rlang" ,r-rlang)
       ("r-tibble" ,r-tibble)
       ("r-tidytree" ,r-tidytree)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/YuLab-SMU/treeio")
    (synopsis "Base classes and functions for Phylogenetic tree input and output")
    (description
     "This is an R package to make it easier to import and store phylogenetic
trees with associated data; and to link external data from different sources
to phylogeny.  It also supports exporting phylogenetic trees with
heterogeneous associated data to a single tree file and can be served as a
platform for merging tree with associated data and converting file formats.")
    (license license:artistic2.0)))

(define-public r-ggtree
  (package
    (name "r-ggtree")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggtree" version))
       (sha256
        (base32
         "02ydi5iyxwrvwfjmv8pbanmzpi1r99mc4gxl17fpq2jf1d1mk6g0"))))
    (properties `((upstream-name . "ggtree")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ape" ,r-ape)
       ("r-aplot" ,r-aplot)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-magrittr" ,r-magrittr)
       ("r-purrr" ,r-purrr)
       ("r-rlang" ,r-rlang)
       ("r-rvcheck" ,r-rvcheck)
       ("r-scales" ,r-scales)
       ("r-tidyr" ,r-tidyr)
       ("r-tidytree" ,r-tidytree)
       ("r-treeio" ,r-treeio)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://yulab-smu.top/treedata-book/")
    (synopsis "R package for visualization of trees and annotation data")
    (description
     "This package extends the ggplot2 plotting system which implements a
grammar of graphics.  ggtree is designed for visualization and annotation of
phylogenetic trees and other tree-like structures with their annotation
data.")
    (license license:artistic2.0)))

(define-public r-metapod
  (package
    (name "r-metapod")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "metapod" version))
       (sha256
        (base32
         "1hbcwr6d8gyrf4azh0gi588xkrg6gz7gsb5hbvzqkhplbsp6shlv"))))
    (properties `((upstream-name . "metapod")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/metapod")
    (synopsis "Meta-analyses on p-values of differential analyses")
    (description
     "This package implements a variety of methods for combining p-values in
differential analyses of genome-scale datasets.  Functions can combine
p-values across different tests in the same analysis (e.g., genomic windows in
ChIP-seq, exons in RNA-seq) or for corresponding tests across separate
analyses (e.g., replicated comparisons, effect of different treatment
conditions).  Support is provided for handling log-transformed input p-values,
missing values and weighting where appropriate.")
    (license license:gpl3)))

(define-public r-biocsingular
  (package
    (name "r-biocsingular")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSingular" version))
       (sha256
        (base32
         "16gkwq8fb8jdchpnlzq2hz3i74a6pzbnc1bf93282h11mp7qr58l"))))
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
       ("r-s4vectors" ,r-s4vectors)
       ("r-scaledmatrix" ,r-scaledmatrix)))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "savR" version))
       (sha256
        (base32
         "1ynp334hm76zf05j4f6vha6r16s5f2ncxx9yviq4rxidk9r723jq"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPexoQual" version))
       (sha256
        (base32
         "0fbrf5s6pz115djm7xw95k1d0p7svi40aacbb3d52wmx5azwj424"))))
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
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "copynumber" version))
              (sha256
               (base32
                "03pvjzjrcsbjfw3855s3whfzin45vaipy7cahnj6fywdysvh8hps"))))
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
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNAcopy" version))
       (sha256
        (base32
         "0mgq814f6c2271d2lxg763bsnv3ma4ari5xa4x1rbksv8yvcjc4d"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBSeq" version))
       (sha256
        (base32
         "0h5v3vrb90zim80bdnr3aw58g3h7zjqa4l9i0jwx5j19ywf54fdz"))))
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "karyoploteR" version))
              (sha256
               (base32
                "11sgxz2xz685pgm8mf0hzm6aryx4fj5g3dlffjzpqxh3awfqa19p"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lpsymphony" version))
       (sha256
        (base32
         "0aw4b3p5z8ys7zlwy8s3bsqk03xwx42311yxr7q14w3f7sn3shzn"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IHW" version))
       (sha256
        (base32
         "0b393a8ayzbnrgkk562w1dj7avacpb3wc7yq7awiki24wi5g2lfw"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iCOBRA" version))
       (sha256
        (base32
         "190rkx3sivj68in36hhin5v535yd6fvlvm7l90w1bl38zpb7p6jn"))))
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

(define-public r-residualmatrix
  (package
    (name "r-residualmatrix")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ResidualMatrix" version))
       (sha256
        (base32
         "0p7va19aw0j6anx7ck879kbi5cn7dy712h5ia94adr38ssismv1v"))))
    (properties
     `((upstream-name . "ResidualMatrix")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-delayedarray" ,r-delayedarray)
       ("r-matrix" ,r-matrix)
       ("r-s4vectors" ,r-s4vectors)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/LTLA/ResidualMatrix")
    (synopsis "Create a DelayedMatrix of regression residuals")
    (description
     "This package implements tools for delayed computation of a matrix of
residuals after fitting a linear model to each column of an input matrix.  It
also supports partial computation of residuals where selected factors are to
be preserved in the output matrix.  It implements a number of efficient
methods for operating on the delayed matrix of residuals, most notably matrix
multiplication and calculation of row/column sums or means.")
    (license license:gpl3)))

(define-public r-batchelor
  (package
    (name "r-batchelor")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "batchelor" version))
       (sha256
        (base32
         "1bkk69b5llkmvmpsnhymwjm2szmzypgszfsw8mak1b5ms5zf8lr0"))))
    (properties `((upstream-name . "batchelor")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocneighbors" ,r-biocneighbors)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biocsingular" ,r-biocsingular)
       ("r-delayedarray" ,r-delayedarray)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-igraph" ,r-igraph)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-residualmatrix" ,r-residualmatrix)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scaledmatrix" ,r-scaledmatrix)
       ("r-scuttle" ,r-scuttle)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/batchelor")
    (synopsis "Single-Cell Batch Correction Methods")
    (description
     "This package implements a variety of methods for batch correction of
single-cell (RNA sequencing) data.  This includes methods based on detecting
mutually nearest neighbors, as well as several efficient variants of linear
regression of the log-expression values.  Functions are also provided to
perform global rescaling to remove differences in depth between batches, and
to perform a principal components analysis that is robust to differences in
the numbers of cells across batches.")
    (license license:gpl3)))

(define-public r-mast
  (package
    (name "r-mast")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MAST" version))
       (sha256
        (base32
         "1gkpagam5rap36viyr3n4psa658x9vckrxap1h67jasiiyrcfz2d"))))
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
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "monocle" version))
       (sha256
        (base32
         "05j1vc51f39xalggdq27y7218gkr3zq7fh7jhzsb4jj7fpn837ry"))))
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

(define-public r-leidenbase
  (let ((commit "430f22af6982cc7d2e6e77f6b0df47bc970dcbce")
        (revision "1"))
    (package
      (name "r-leidenbase")
      (version (git-version "0.1.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cole-trapnell-lab/leidenbase")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0ylqsmdpv4xq6f6ldxvwkhk33a9i1jcgk60zvksk3iplykfzd6c0"))))
      (properties `((upstream-name . "leidenbase")))
      (build-system r-build-system)
      (inputs
       `(("zlib" ,zlib)))
      (native-inputs
       `(("gfortran" ,gfortran)))
      (propagated-inputs
       `(("r-igraph" ,r-igraph)))
      (home-page "https://github.com/cole-trapnell-lab/leidenbase")
      (synopsis "R and C wrappers to run the Leiden find_partition function")
      (description
       "This package provides an R to C interface that runs the Leiden
community detection algorithm to find a basic partition.  It runs the
equivalent of the @code{find_partition} function.  This package includes the
required source code files from the official Leidenalg distribution and
several functions from the R igraph package.")
      (license license:gpl3+))))

(define-public r-monocle3
  (package
    (name "r-monocle3")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cole-trapnell-lab/monocle3")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "16vpvlbms8fdvpfwzcig0rkg2mxnsq1h80d2l7q3953wm91qc9x4"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-batchelor" ,r-batchelor)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-grr" ,r-grr)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-igraph" ,r-igraph)
       ("r-irlba" ,r-irlba)
       ("r-leidenbase" ,r-leidenbase)
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
       ("r-rsample" ,r-rsample)
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
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "NOISeq" version))
       (sha256
        (base32
         "18d51dv2ygsm7kkwal341f1wrwrazyns0045j00vld367kic8jiz"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scDD" version))
       (sha256
        (base32
         "1dw9m3m99apmbs32461c6lnmy81n5hxbhz3p8jk419gajkh4v1ji"))))
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
    (version "1.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scone" version))
       (sha256
        (base32
         "0zw8g4mql7b02xqwhc1i0bnhm20c1q9xqc7yz84j98pqbi996vi5"))))
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
       ("r-matrixgenerics" ,r-matrixgenerics)
       ("r-matrixstats" ,r-matrixstats)
       ("r-mixtools" ,r-mixtools)
       ("r-rarpack" ,r-rarpack)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rhdf5" ,r-rhdf5)
       ("r-ruvseq" ,r-ruvseq)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
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
    (version "2.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GEOquery" version))
       (sha256
        (base32
         "0jhkdbcd03d5n8vn3xkad6f21xjkawyxc9rdwcj8vwc8alx730am"))))
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
    (version "0.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "illuminaio" version))
       (sha256
        (base32
         "1sy0i3nbzsw4ymdxaiwpyx1vcg9yp3i8xfjcymqwhv95j3kyglv9"))))
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
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "siggenes" version))
       (sha256
        (base32
         "0lva0f255fcpy625frvij4n14q7nw4jcx8n2hlkxid4mgkfqwlhf"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bumphunter" version))
       (sha256
        (base32
         "0wi38vwfi8qr10x4xifhylxx7vfc6fqvqs649iq7lf0y7islwq2v"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "minfi" version))
       (sha256
        (base32
         "189lzppvrz6bw2kpppawgsfjyh2ahyy8bi9z8srpas67qf2r8jmj"))))
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
    (version "2.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "methylumi" version))
       (sha256
        (base32
         "1941rc524ssx8gwhmwk40mgfrhddfs6hgldvs7bi22r29gm4y7qj"))))
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
    (version "2.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lumi" version))
       (sha256
        (base32
         "0qjdxjdzfnnxcm07bf51v38388s5qf1i03l1sdb9jf3gxdh8yh02"))))
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
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Linnorm" version))
       (sha256
        (base32
         "035hrniycqadmkwg8rmzw8szv0amhy31390izy91rfrld31v2yy7"))))
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
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IONiseR" version))
       (sha256
        (base32
         "0fknlwdfd49v09zspg0337b0zzc8hqza3563yrw51viw3g35d6q3"))))
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

(define-public r-tradeseq
  (package
   (name "r-tradeseq")
   (version "1.6.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "tradeSeq" version))
            (sha256
             (base32
              "0fgmb309pvqf3chdhqgbd4lzhahcj5g71sica33nzn98qhipldx7"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biobase" ,r-biobase)
      ("r-biocparallel" ,r-biocparallel)
      ("r-edger" ,r-edger)
      ("r-ggplot2" ,r-ggplot2)
      ("r-igraph" ,r-igraph)
      ("r-magrittr" ,r-magrittr)
      ("r-matrix" ,r-matrix)
      ("r-matrixstats" ,r-matrixstats)
      ("r-mgcv" ,r-mgcv)
      ("r-monocle" ,r-monocle)
      ("r-pbapply" ,r-pbapply)
      ("r-princurve" ,r-princurve)
      ("r-rcolorbrewer" ,r-rcolorbrewer)
      ("r-s4vectors" ,r-s4vectors)
      ("r-singlecellexperiment" ,r-singlecellexperiment)
      ("r-slingshot" ,r-slingshot)
      ("r-summarizedexperiment" ,r-summarizedexperiment)
      ("r-tibble" ,r-tibble)
      ("r-viridis" ,r-viridis)))
   (native-inputs
    `(("r-knitr" ,r-knitr)))
   (home-page "https://statomics.github.io/tradeSeq/index.html")
   (synopsis "Trajectory-based differential expression analysis")
   (description
    "This package provides a flexible method for fitting regression models that
can be used to find genes that are differentially expressed along one or
multiple lineages in a trajectory.  Based on the fitted models, it uses a
variety of tests suited to answer different questions of interest, e.g.  the
discovery of genes for which expression is associated with pseudotime, or which
are differentially expressed (in a specific region) along the trajectory.  It
fits a negative binomial generalized additive model (GAM) for each gene, and
performs inference on the parameters of the GAM.")
   (license license:expat)))

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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantTools" version))
       (sha256
        (base32
         "1gpzrln2clfrja8rzxhsis6bi1xqglh3h2lhvqlnrx4lqxhbkv9c"))))
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
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Heatplus" version))
       (sha256
        (base32
         "0xwg3sxmihg3p6v3nxgrqy0nrqxi6razg3b3rjh2gcb2vv8gcqng"))))
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
    (version "2.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOSemSim" version))
       (sha256
        (base32
         "00dbgkiv9x7g2i0anzcxpycwqqqry0y7jl3ad93lhvi31qnqq1sm"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "anota" version))
       (sha256
        (base32
         "039bmcv5l44gszb6xapbihp3mfqdaaa8mfc05y702p78i7x93g5y"))))
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
    (version "1.60.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "sigPathway" version))
        (sha256
          (base32
            "1xz5nbw5dzyah8az7mpwj8m27fsvpi2jjhfg3n27dsv8rvdncqi8"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fgsea" version))
       (sha256
        (base32
         "1mhdgy46nxgv7v54bk9bqfy0vgjzl1law7zy718swdd762xn6g9d"))))
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
    (version "3.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DOSE" version))
       (sha256
        (base32
         "0kp6j42mpxrpd02cjrzqmrx3rvvpi90xiy4gc5km6ny3vxbhlaqw"))))
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
    (version "1.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "enrichplot" version))
       (sha256
        (base32
         "194sfmcnjfi3fvvfpljg1f80f44vvvxiij336b8z1dgzki6bqa3r"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cowplot" ,r-cowplot)
       ("r-dose" ,r-dose)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggraph" ,r-ggraph)
       ("r-ggtree" ,r-ggtree)
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
    (version "4.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "clusterProfiler" version))
       (sha256
        (base32
         "161w9mn2plmymvzf1hkk9fwi3d9c26kbcpndyyrfcl6bg2nxr1s8"))))
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

(define-public r-clusterexperiment
  (package
    (name "r-clusterexperiment")
    (version "2.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "clusterExperiment" version))
              (sha256
               (base32
                "1fhmayciwn1m124b4dcs3gzbghgk9f7a7qmjnvvxc958cywcwkx5"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (propagated-inputs
     `(("r-ape" ,r-ape)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocsingular" ,r-biocsingular)
       ("r-cluster" ,r-cluster)
       ("r-delayedarray" ,r-delayedarray)
       ("r-edger" ,r-edger)
       ("r-hdf5array" ,r-hdf5array)
       ("r-howmany" ,r-howmany)
       ("r-kernlab" ,r-kernlab)
       ("r-limma" ,r-limma)
       ("r-locfdr" ,r-locfdr)
       ("r-matrix" ,r-matrix)
       ("r-matrixstats" ,r-matrixstats)
       ("r-mbkmeans" ,r-mbkmeans)
       ("r-nmf" ,r-nmf)
       ("r-phylobase" ,r-phylobase)
       ("r-pracma" ,r-pracma)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scales" ,r-scales)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-stringr" ,r-stringr)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-zinbwave" ,r-zinbwave)))
    (home-page "https://bioconductor.org/packages/clusterExperiment/")
    (synopsis "Compare clusterings for single-cell sequencing")
    (description "This package provides functionality for running and comparing
many different clusterings of single-cell sequencing data or other large mRNA
expression data sets.")
    (license license:artistic2.0)))

(define-public r-mlinterfaces
  (package
    (name "r-mlinterfaces")
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MLInterfaces" version))
       (sha256
        (base32
         "0ipzv7wdvfqhdyjiak956qq201igsdxm6dr6rh3dj6cssgsrnrpb"))))
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
       ("r-magrittr" ,r-magrittr)
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
    (version "1.64.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annaffy" version))
       (sha256
        (base32
         "03y633vgxprd2abhanj4sanmb4ymz7az5aiasxn6wjzawiqjdcb1"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-reference-to-non-free-data
           (lambda _
             (substitute* "DESCRIPTION"
               ((", KEGG.db") "")))))))
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocmanager" ,r-biocmanager)
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Core" version))
       (sha256
        (base32
         "00fi753nsayv0xspavw8r9ni1sim8ng33hp3d3kj2b8ihygd1s10"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Classif" version))
       (sha256
        (base32
         "1lz85bys5dp5d1ir9c9c4wy85wkk62s14niyzzxaqrxpsji2p2iw"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Preproc" version))
       (sha256
        (base32
         "1jibm2f5glzsrvl64hxc5sf59d4w6ry0f663p619hfr44mi1mpri"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Reporting" version))
       (sha256
        (base32
         "09jjfby5znmg2fdkhpbinx2v21zrfa44qq7cylcn9ipffqx3pk86"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Base" version))
       (sha256
        (base32
         "101w4i0w8n7qhki22qr8l8wk8w6zalzmcywqm2g4238qv7xbnr8p"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4" version))
       (sha256
        (base32
         "0bwn185admy5k99fkd2dhhvy7x1f75r0mqn5k24dbbg0paw6nnr8"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "abseqR" version))
       (sha256
        (base32
         "1xvk84gd1lfq1icrfwwd6y79zywrisfnc3knprigzzax31aks32c"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bacon" version))
       (sha256
        (base32
         "1yp0675n1g8rxdjdd7w8al5c9jq96h0kfm8218mc50z0p2fasgbj"))))
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
    (version "2.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rGADEM" version))
       (sha256
        (base32
         "05drbhjqn5kik6k4h03vr3d2b6pv5rm65lsnkyx4caxxcdii4jzm"))))
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
   (version "1.34.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MotifDb" version))
            (sha256
             (base32 "04cmgg5mw1cqbg95zyfc2imykmdxyff16w26rq97xghcxwiq2b3z"))))
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
   (version "2.6.1")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "motifbreakR" version))
            (sha256
             (base32 "1n6v8a7c27aswmbrlcwcpghf27aplkc6nn923imglc3c0sh2lkrz"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifStack" version))
       (sha256
        (base32
         "0yn348kmw7v46iym913ncx4yh5nxzmba8bdys52s12ldgjja53gp"))))
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicScores" version))
       (sha256
        (base32
         "1b8982fj0r7igj749wljsdfn3c985w8n3d5gbhr5rw73llfb8x6w"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ATACseqQC" version))
       (sha256
        (base32
         "168g3xkhjzrfjpa1ynifdyfhsxx3rpyrbybsarlzr9kslw1cdkxl"))))
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOfuncR" version))
       (sha256
        (base32
         "0lp2gmjlsk1yqxim5pi26i27iijw11lrcxmji7ynlag359yfnynd"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ABAEnrichment" version))
       (sha256
        (base32
         "15v5n6d3mnj9d86swkh9agfvrsd065dldaywm1adwmkhhk525wmd"))))
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
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotationTools" version))
       (sha256
        (base32
         "18dd8saqx17fplvv5sbfz4p9fy7ksy3n9348rd3qlczihcrh9i3w"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AllelicImbalance" version))
       (sha256
        (base32
         "1mn6975npncmfjlpjs3s2pgsrm32xgvnyl2vh1922l6vra97dkrc"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AUCell" version))
       (sha256
        (base32
         "0qdac3qalcki20r90k40cc8d0lfywzn9pffg9d719yvs7nrsfdjr"))))
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
    (version "4.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBImage" version))
       (sha256
        (base32
         "1z3cxxg593d66nczz5hh2hdj1d87wc0lxrzc5sn6bp43n351q8h3"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yamss" version))
       (sha256
        (base32
         "1al615x778h17jqiyhiyf6djaq9iygs1hlbrna6y4xc0f2kvgxld"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gtrellis" version))
       (sha256
        (base32
         "0l5271cpzjlm4m0v6xcdg2vxlbhn53x1fd59ynb9jwll93av5h1f"))))
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
    (version "2.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SomaticSignatures" version))
       (sha256
        (base32
         "0jr11c9hz7m49xc8pi6xrr5fhbv68vafvqpzhr0pmm51vvr1vfs9"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "YAPSA" version))
       (sha256
        (base32
         "1xfkgjlm0rxz82qcaqzx95cwirxifd9dsswjg3zcqmz03v7a0gz2"))))
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
    (version "2.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gcrma" version))
       (sha256
        (base32
         "1z4abw9s2hs3csnx25nli7fpvb3rh3l0swzl0wfqp087fcs78pxi"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "quantro" version))
       (sha256
        (base32
         "1fkma2ic448h2lrlza8ipg65gpfsz4fhlxcnjpmzhhmzp2xi2p4a"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yarn" version))
       (sha256
        (base32
         "1x07l255x52z6cgdc2j8285shqszhr034xm5686rp6d35vah55ji"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "roar" version))
       (sha256
        (base32
         "0vp0n90rvjm8rzwkbrxa3fasb9val56bz2srz72xwsl3jzb5yk6w"))))
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
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MassSpecWavelet" version))
       (sha256
        (base32
         "1cs32sig1yvqn9xs0cvhfpmkh2lbllx7aab80sz58x03wnx8v60z"))))
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
    (version "3.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "xcms" version))
       (sha256
        (base32
         "1g8k90p0sbcb3rdrbldj5bcjp2piy5ldni4jplyr78vjpmrmvqk7"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Wrench" version))
       (sha256
        (base32
         "0yssmqayaryrc0asjjqxdipqdfg309llrzldx38jrfgdsza6bvs0"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wiggleplotr" version))
       (sha256
        (base32
         "13f3g6fcc29k0g21mhnypm3nc6qqjw867vamvfkdzynspsfb32ga"))))
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
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "widgetTools" version))
       (sha256
        (base32
         "186xgpgyfyi4angxhz6558lfpzvvszzlpyfr8xnbccs3gshk3db2"))))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "webbioc" version))
       (sha256
        (base32
         "1mpdw477j78s2nvlf2lzm6mdjcpamyyazjn060h9q3apawn6zajx"))))
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

(define-public r-zinbwave
  (package
    (name "r-zinbwave")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zinbwave" version))
       (sha256
        (base32
         "0xgjbk35wl1vjqyq4y5c7hna8hkgmf56xjaxcph9bs2q7mbdnqwf"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-edger" ,r-edger)
       ("r-genefilter" ,r-genefilter)
       ("r-matrix" ,r-matrix)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-softimpute" ,r-softimpute)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/zinbwave")
    (synopsis "Zero-inflated negative binomial model for RNA-seq data")
    (description "This package implements a general and flexible zero-inflated
negative binomial model that can be used to provide a low-dimensional
representations of single-cell RNA-seq data.  The model accounts for zero
inflation (dropouts), over-dispersion, and the count nature of the data.
The model also accounts for the difference in library sizes and optionally
for batch effects and/or other covariates, avoiding the need for pre-normalize
the data.")
    (license license:artistic2.0)))

(define-public r-zfpkm
  (package
    (name "r-zfpkm")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zFPKM" version))
       (sha256
        (base32
         "1k7xaxr2gn26y8bps5l32g2axfhdn07nbk4q3qcx32d5jm75qkx2"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie2" version))
       (sha256
        (base32
         "0r5yqjal48xlcv5cidi7p3zwygvsllmv2zzkwkc9kfq083l2i4ih"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "progeny" version))
       (sha256
        (base32
         "10vpjvl4wps857xiy8rpzr82jzdfbc5rgwh3ir3my26lfws4hfz8"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ARRmNormalization" version))
       (sha256
        (base32
         "0ni3vblcadhwxaq1pf1n9jn66cp3bqch68ww4c8zh19zagil2y7r"))))
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
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocFileCache" version))
       (sha256
        (base32
         "0ymg4hmabk233qgassld62achjylm12rnidxbakfkx4dvvlbhxxv"))))
    (properties `((upstream-name . "BiocFileCache")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-curl" ,r-curl)
       ("r-dbi" ,r-dbi)
       ("r-dbplyr" ,r-dbplyr)
       ("r-dplyr" ,r-dplyr)
       ("r-filelock" ,r-filelock)
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iClusterPlus" version))
       (sha256
        (base32
         "1haj25c4cmmjjvh181b41a9pvkh92f7k2w4ljn17iqg31vm45za1"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie" version))
       (sha256
        (base32
         "0mfikbrs28q7r3lnsq0jma5x6nkrnm3q46242jh35w9c969jk5yy"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SGSeq" version))
       (sha256
        (base32
         "0i1yaw8h8gibakvaf1xd6nnjx2bsb2s9c0q74rbq9lm7haihivp6"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhisat2" version))
       (sha256
        (base32
         "1hqahh5h22mj2crqp6r9xnm111xmfgk39c100rcaziqrpdy5npk8"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "QuasR" version))
       (sha256
        (base32
         "1635ff11ahzjrh3cdcxrq5bgd100n444k7mc0maz0jx21vj8qqb1"))))
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
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rbowtie" ,r-rbowtie)
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rqc" version))
       (sha256
        (base32
         "02hwj2vd003x0zf273ndnwh7kxy6wc3sz14d3kryp2w2aqjj826f"))))
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
    (version "3.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiRewire" version))
       (sha256
        (base32
         "0p6mr67mkw54490sv4dvkyh8l0xkpjfbqy532vi8l41i40qg3gry"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiDataSet" version))
       (sha256
        (base32
         "1hyk40xgmy50rqxwdvc64d3pgz5vsg8vmlj5cp5m0n5m0adxcdfj"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ropls" version))
       (sha256
        (base32
         "1j99kdywyljqzdcns5ysh590w4w8iiwzpddpfk5c8d4whax7vk5b"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biosigner" version))
       (sha256
        (base32
         "159fbkymn92li7dinsm56nsacjp8wnhsljv7airgs9m82p8wd5sl"))))
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
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotatr" version))
       (sha256
        (base32
         "1ls1qdfppcxysggi3bghrnspb5a3s40pm2mj4x0whc7c40cf90mg"))))
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
    (version "2.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rsubread" version))
       (sha256
        (base32
         "043m4512q73x6q529hqfgrap0fq5anvny4va085nafia06b805pi"))))
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
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowUtils" version))
       (sha256
        (base32
         "1sdwgyvrsz0pp60zdfrcgb7bs8s87j7257p8ck813ydizc324x9w"))))
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
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ConsensusClusterPlus" version))
       (sha256
        (base32
         "163nr50nyvwrsajmm0cgxp70pqk61mgw0k7ams694hcb42162j8b"))))
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cytolib" version))
       (sha256
        (base32
         "0wl7zqwv0i38dfzqfsz40n3mcbxi38ffn1rbd5pm9s7hq16zr4nv"))))
    (properties `((upstream-name . "cytolib")))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowCore" version))
       (sha256
        (base32
         "13xpbkxi53rxmhvpdiy6bydmhicmxd2gi96d9c1qx4lkss2f14nc"))))
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
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowMeans" version))
       (sha256
        (base32
         "1awskkq48qhv4v9glxgfqi0kgwqd62fbj641k4vvxfcwsf2c7bfg"))))
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
    (version "2.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ncdfFlow" version))
       (sha256
        (base32
         "1lm88qnfv6rnnr7wmgbvwyj272imjjjn7h3agxqqzsbmn8vyrnf0"))))
    (properties `((upstream-name . "ncdfFlow")))
    (build-system r-build-system)
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggcyto" version))
       (sha256
        (base32
         "0jd6m84m4znnpix4bcgdby7mnflsn206f5x2vw9n8rxnwzx77wpg"))))
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
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowViz" version))
       (sha256
        (base32
         "17x04xwyw2pp5zkhgvrmxkb8cbrv9wql6xhjsfpq0n6yd9dxqc1v"))))
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
    (version "3.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowClust" version))
       (sha256
        (base32
         "03xsmprrkxwkaksjlaxwp54mqfb3zlg6dbqhp87w78fwscisv76b"))))
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RProtoBufLib" version))
       (sha256
        (base32
         "1hyds97ay4mn7nl830yh9v8mlasgsljsx5wsrhz2zsmbbyx6wbnb"))))
    (properties `((upstream-name . "RProtoBufLib")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-bundled-sources
           (lambda _
             (with-directory-excursion "src"
               (invoke "tar" "xf" "protobuf-3.13.0.tar.gz")))))))
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
    (version "4.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowWorkspace" version))
       (sha256
        (base32
         "1a9qb2dcvwgb3z0vdbbzn1rzy77d3da72kirs272344hdx9b2cx9"))))
    (properties `((upstream-name . "flowWorkspace")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-aws-s3" ,r-aws-s3)
       ("r-aws-signature" ,r-aws-signature)
       ("r-bh" ,r-bh)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-cytolib" ,r-cytolib)
       ("r-data-table" ,r-data-table)
       ("r-delayedarray" ,r-delayedarray)
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
       ("r-s4vectors" ,r-s4vectors)
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
    (version "4.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowStats" version))
       (sha256
        (base32
         "1yr5m7qmhmm52c70z3d0zy4zgf0hja7r2ig9yljv5w86bzm962x6"))))
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "openCyto" version))
       (sha256
        (base32
         "1par1d5pk1rjg15q7i5z5wqma7xg6fycb826a823wk8wr52a885x"))))
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CytoML" version))
       (sha256
        (base32
         "0ixy7mmnipk8wy61wz6qy7jfbc5zhs6p5iqaii8hdprjnb841ri7"))))
    (properties `((upstream-name . "CytoML")))
    (build-system r-build-system)
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
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "FlowSOM" version))
       (sha256
        (base32
         "18h7p7g3w9imyd1c93jllgp4kd74z96cs85wcqfhmd26nx18hl82"))))
    (properties `((upstream-name . "FlowSOM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-colorramps" ,r-colorramps)
       ("r-consensusclusterplus" ,r-consensusclusterplus)
       ("r-cytoml" ,r-cytoml)
       ("r-dplyr" ,r-dplyr)
       ("r-flowcore" ,r-flowcore)
       ("r-flowworkspace" ,r-flowworkspace)
       ("r-ggforce" ,r-ggforce)
       ("r-ggnewscale" ,r-ggnewscale)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggpointdensity" ,r-ggpointdensity)
       ("r-ggpubr" ,r-ggpubr)
       ("r-ggrepel" ,r-ggrepel)
       ("r-igraph" ,r-igraph)
       ("r-magrittr" ,r-magrittr)
       ("r-pheatmap" ,r-pheatmap)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rlang" ,r-rlang)
       ("r-rtsne" ,r-rtsne)
       ("r-scattermore" ,r-scattermore)
       ("r-tidyr" ,r-tidyr)
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
    (version "6.16.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mixOmics" version))
       (sha256
        (base32
         "1x6dbw4q6p9vngm256fr96r9fjxk5nik5ivkhbl5a9zqyq8wagpa"))))
    (properties `((upstream-name . "mixOmics")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocparallel" ,r-biocparallel)
       ("r-corpcor" ,r-corpcor)
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DepecheR" version))
       (sha256
        (base32
         "04vxc43p3kpsx0vksk2nwmy9p56h35z2mc8j9p2wm29zaz1y8j3p"))))
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RcisTarget" version))
       (sha256
        (base32
         "1yh0l11vnslgr6zsbpgc8mc4aa32zy34f5yrz98hkcdl53iw5y7f"))))
    (properties `((upstream-name . "RcisTarget")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-arrow" ,r-arrow)
       ("r-aucell" ,r-aucell)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-data-table" ,r-data-table)
       ("r-dplyr" ,r-dplyr)
       ("r-feather" ,r-feather)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-gseabase" ,r-gseabase)
       ("r-r-utils" ,r-r-utils)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-tibble" ,r-tibble)))
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

(define-public r-chicago
  (package
    (name "r-chicago")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Chicago" version))
       (sha256
        (base32
         "0dkwy6pfvzd7g4qmhjl24ypn92l78w1zy0ajhcxgg39f7zsq883x"))))
    (properties `((upstream-name . "Chicago")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-delaporte" ,r-delaporte)
       ("r-hmisc" ,r-hmisc)
       ("r-mass" ,r-mass)
       ("r-matrixstats" ,r-matrixstats)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/Chicago")
    (synopsis "Capture Hi-C analysis of genomic organization")
    (description
     "This package provides a pipeline for analysing Capture Hi-C data.")
    (license license:artistic2.0)))

(define-public r-cicero
  (package
    (name "r-cicero")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cicero" version))
       (sha256
        (base32
         "0kw16zf9004d1zlwsswhbcb7p77nabpd1fjagznff3zyan9fpdxf"))))
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
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "circRNAprofiler" version))
       (sha256
        (base32
         "1hif40vfg2lkbyf6abbkxbdm3b1biw6gxnh6ca2sydvi3y5l2ys2"))))
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
framework allows combining and analyzing circRNAs previously detected by
multiple publicly available annotation-based circRNA detection tools.  It
covers different aspects of circRNAs analysis from differential expression
analysis, evolutionary conservation, biogenesis to functional analysis.")
    (license license:gpl3)))

(define-public r-cistopic
  (package
    (name "r-cistopic")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aertslab/cisTopic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0c4553rnxq7b1w451kcc3iwvak4qa5h2b43xmfw6ii8096zd1gbf"))))
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
    (license license:gpl3)))

(define-public r-cistopic-next
  (let ((commit "04cecbb9d1112fcc1a6edc28b5a506bcb49f2803")
        (revision "1"))
    (package
      (inherit r-cistopic)
      (name "r-cistopic-next")
      ;; The DESCRIPTION file says this is version 0.3.0, which is a bit odd
      ;; since the previous release is 2.1.0.  Oh well.
      (version (git-version "0.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aertslab/cisTopic")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11cg9szlysnsjiaahda4k5v2vh4rxx27zhz53hafgaq9mdz0kgi2"))))
      (properties `((upstream-name . "cisTopic")))
      (propagated-inputs
       `(("r-aucell" ,r-aucell)
         ("r-data-table" ,r-data-table)
         ("r-dosnow" ,r-dosnow)
         ("r-dplyr" ,r-dplyr)
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
         ("r-s4vectors" ,r-s4vectors)
         ("r-text2vec" ,r-text2vec)))
      (native-inputs
       `(("r-knitr" ,r-knitr))))))

(define-public r-genie3
  (package
    (name "r-genie3")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GENIE3" version))
       (sha256
        (base32
         "1v54dzcz654wfm3npbp8gb55v49im0fm547cz3hvsidq4zhi3l1b"))))
    (properties `((upstream-name . "GENIE3")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-reshape2" ,r-reshape2)))
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
    (version "1.68.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROC" version))
       (sha256
        (base32
         "1rmsrvn6hrg9ay7xfb05mfkxknnig78p3kbk9ghsd11lhx2fjm3s"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wateRmelon" version))
       (sha256
        (base32
         "1qar8z0nf33bqr488swig0bfq8lnvcdjcxvw3q3b0hkkvybn27zw"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gdsfmt" version))
       (sha256
        (base32
         "119qdivd7vaaqkjb0nrwidi6g26hh8znhif3g4prqn7x5pl2clvy"))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigmelon" version))
       (sha256
        (base32
         "061f0hc4m4nvdr6298pg3inpx1z2bpsm9nlxqs3v7n58q15xyzri"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqbias" version))
       (sha256
        (base32
         "06w43plv4x1pafybq633n7adqp9yj3bvaaamq7vylmkfbcx3nl8k"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReQON" version))
       (sha256
        (base32
         "1z13avbxwvvhh03arjfnaipznynifsi8k2hzw4kappz24f7lwmza"))))
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
    (version "2.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wavClusteR" version))
       (sha256
        (base32
         "1sydzrqydfv1ik2h08xkxlx6xrv866bf0if6v5wch9l3krh0sych"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TimeSeriesExperiment" version))
       (sha256
        (base32
         "10xgihjssnc6i03819p9gnzwfc7znanic514ar3yxzl3fhxy3yyy"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantFiltering" version))
       (sha256
        (base32
         "0abhrk53csd9jz9sv4q1qr74jax9mbj2icbz1iilf1123nvnjypd"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "variancePartition" version))
       (sha256
        (base32
         "1pqy2g9pg8pswmkrs2fzlkwwliw2r7f33h05bci5bz41b8ribpzj"))))
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
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HTqPCR" version))
       (sha256
        (base32
         "1y3ik5a9w66jby6682jfm8mn2883s8yfv4xw8a8v1f6q2d1j938l"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "unifiedWMWqPCR" version))
       (sha256
        (base32
         "1clcz610sl3s0mjf84j21xgrmjhkxcc4h292ljwq5yzbkk68g896"))))
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
    (version "1.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "universalmotif" version))
       (sha256
        (base32
         "0hv2v2zgif5ihr5hxmdz32rln43jc1j0rslp44kd3rijjl45zysn"))))
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
       ("r-iranges" ,r-iranges)
       ("r-mass" ,r-mass)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppthread" ,r-rcppthread)
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
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BGmix" version))
       (sha256
        (base32
         "1gp40ddspblpszzm4k4r7ysgx883iwdfqc5ds23p2q1ml9idwgvv"))))
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
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bgx" version))
       (sha256
        (base32
         "18n6j2ihv85rhai5sf5k3mwf9nkc2nl2sinx3rrs6sbl529g4mw4"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BHC" version))
       (sha256
        (base32
         "0aaawm7h3ppyyhd7hi14rpynagnxx4730f5vxizj5bpzwbclp6h9"))))
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
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BicARE" version))
       (sha256
        (base32
         "0j3gs4xcdgaca2c62jf2h86skbbxm1c9g2khs5bsa8fmkskr6vl9"))))
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiFET" version))
       (sha256
        (base32
         "1fwy7ws0bn67557s0kcw8pbln2jg834n6kfbs8297ps07nxr0lpj"))))
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
    (version "2.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rsbml" version))
       (sha256
        (base32
         "017xwra6ms7kx6sg3ksw1vr9zn23imc2qjgpmjikx7mgbak125xh"))))
    (properties `((upstream-name . "rsbml")))
    (build-system r-build-system)
    (inputs
     `(("libsbml" ,libsbml)))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hypergraph" version))
       (sha256
        (base32
         "01knpd964m2g9vwd7c72qnc8g2p2pzhvk7lin4mhvcmb3pvsdlh7"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hyperdraw" version))
       (sha256
        (base32
         "1qkxixkgvvfha0ii8rwwcbrbjmbbxsy8afv5ymcq01k3hbykx44r"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiGGR" version))
       (sha256
        (base32
         "1y9659pxm65w51zvrz36girb3qvfc64zijjkxmg6xn4pbc8vv1wf"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigPint" version))
       (sha256
        (base32
         "0yrg9x1a92zmz7j5hk77cph1jg82mkpr7k7qi9fdr6z5nqq6fgz6"))))
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
    (version "3.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChemmineR" version))
       (sha256
        (base32
         "1z59npqk7hnqzhjdnry6lfqlyxfzwqprp7bmbdzs4rp22pzcv1v8"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioassayR" version))
       (sha256
        (base32
         "1mlv80w8a7l8cii3dfqvanvh0qdqvcg8c1iiq4xlyvkjxfs64ka1"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biobroom" version))
       (sha256
        (base32
         "1ybyhmrcvj6k1laxw9bc8jbn533frkzh9k8kl1ibd5pi368rfqzn"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "graphite" version))
       (sha256
        (base32
         "11bgz6951nfygxp3fm0190gf2bb5zplis1bc0am4757liw4qybhf"))))
    (properties `((upstream-name . "graphite")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-checkmate" ,r-checkmate)
       ("r-graph" ,r-graph)
       ("r-httr" ,r-httr)
       ("r-rappdirs" ,r-rappdirs)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReactomePA" version))
       (sha256
        (base32
         "1293z89ai766c559axgr7mz5x4564gyl9xqzf6s8s0aj1xb35gqf"))))
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
    (version "2.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBarrays" version))
       (sha256
        (base32
         "1k1kl0m7wzaqpv7i20pfav2a6jf93bhri4w7qdikmvkf011n9422"))))
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
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocCheck" version))
              (sha256
               (base32
                "1h0l5w33c9jpc20pynq634qmx8jbfa802d7jslmf4haljmrxm4a1"))))
    (properties
     `((upstream-name . "BiocCheck")))
    (build-system r-build-system)
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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biocGraph" version))
       (sha256
        (base32
         "12bmj9kdlylp02cfwviak7y323ndccl2694rvi4cdg4vsx7v3ya3"))))
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
    (version "2.20.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocStyle" version))
              (sha256
               (base32
                "0p2wdq5vrx63ndghl9ww428z2lwnv5y88xmcr51by2g6vcj3brcf"))))
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
    (version "1.60.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biocViews" version))
              (sha256
               (base32
                "0pc5ll59vm8a9s1nrdc7p9wk11a52rrz669fsrrqd8qapa8p6wfd"))))
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
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ExperimentHub" version))
       (sha256
        (base32
         "1mzmw3100lf33yhz27nbxncrjk5bprlackrjcwf8xdhcaidg40p4"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "groHMM" version))
       (sha256
        (base32
         "1h63vg1iskw79ijg5h1b9097ams8pp3kvnlawlfci6xfli07xpkj"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiAssayExperiment" version))
       (sha256
        (base32
         "0l0arf3q1f6zy6pdgsy3h5n523sg1hlilv7lj7snr5814idgdj51"))))
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
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocOncoTK" version))
       (sha256
        (base32
         "1ix09a39z7y2cj0y8qsd66ka8a8y8q79w08l4jv1yhhn9h4va89s"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioCor" version))
       (sha256
        (base32
         "135rga7mwpzy8ypvriqpbmlh9l5yf61s9s1sa9615qfab14jh06b"))))
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
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocPkgTools" version))
       (sha256
        (base32
         "18a9mbzfmkipnv1cc9h3rhn9jxdp7nzywp0bz7hvmsaffv4p4skc"))))
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
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSet" version))
       (sha256
        (base32
         "0nmrg9cgq8l9w467y364jghnvym05abpj8pyj90grmrib2xc5sj2"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocWorkflowTools" version))
       (sha256
        (base32
         "1a32bwgnxaw0gv2gij2p4rm0a6l06jjhidvfz2v4k900pz1w79av"))))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioDist" version))
       (sha256
        (base32
         "1y1x9q9aa76gbhsyfn638rxp5icjvq30fv3a4205xh7g5jwlf6yw"))))
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "PCAtools" version))
       (sha256
        (base32
         "11idi9fwvyhkakbm63qxcdhkany8gbskis04z0p5a39lppq8ks31"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rGREAT" version))
       (sha256
        (base32
         "0ads9c9i8b39wvjih057zlyivj8zpsqjxf6r97yflz4sbi1jmcji"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "M3C" version))
       (sha256
        (base32
         "17gj4haa4ywc6bmppm342jzppl3inqa94235yspikij6c098vrmc"))))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Icens" version))
       (sha256
        (base32
         "1rzgwxx4w2bqsaz0xmkhi4w9zsxgms80xf59zg1xp1camyhaihlz"))))
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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "preprocessCore" version))
       (sha256
        (base32
         "17a00blz4kvhmsk92bp5alj9kdpy7h1id7nk4vqxakhkb2jabr20"))))
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
    (version "0.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "S4Vectors" version))
              (sha256
               (base32
                "0v5vxmg0a27ivgymmzfl595rcb1m3dz27r2wzbk1j97rlpwy1p4q"))))
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
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rgraphviz" version))
       (sha256
        (base32
         "0d0xq1vgr4b165cn4wg7zmfjyc1d9ir4amgs196j4cgrhpmrnl8s"))))
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FitHiC" version))
              (sha256
               (base32
                "1p8a6lis5yswaj647bjw89harlarnsxmvjzz63fn1l6gxrwhh8bx"))))
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
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HiTC" version))
              (sha256
               (base32
                "0s3kfqs3zlmq61hxdmxpmi9gfs9w3byckw7xzp1hrhdbsl46yvgq"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HDF5Array" version))
       (sha256
        (base32
         "1718hplz5qlbwxwb87509bl7lir9kilvn1s7p2haz551pg48zvrc"))))
    (properties `((upstream-name . "HDF5Array")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-delayedarray" ,r-delayedarray)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-rhdf5" ,r-rhdf5)
       ("r-rhdf5filters" ,r-rhdf5filters)
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
    (version "1.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhdf5lib" version))
       (sha256
        (base32
         "1cwynbcaaxmbh45fc0d264liqdj0wbjlj7k2bsq3qfjbnh6kkam5"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled binaries
           (delete-file-recursively "src/wininclude/")
           (delete-file-recursively "src/winlib-8.3.0/")
           (delete-file "src/hdf5small_cxx_hl_1.10.7.tar.gz")
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
               ((" \"%s/libsz.a\"") "")
               (("patharch, .getDynamicLinks")
                ".getDynamicLinks"))
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
                 (("PKG_LIBS =.*") "PKG_LIBS = -lz -lhdf5\n"))))))))
    (propagated-inputs
     `(("hdf5" ,hdf5-1.10)
       ("zlib" ,zlib)))
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
    (version "2.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "beachmat" version))
       (sha256
        (base32
         "06bpfpddsl49csxrs8hlx5pv0absc2wdcparchf8qqaqydazwci2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-delayedarray" ,r-delayedarray)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CNEr" version))
       (sha256
        (base32 "13w0gsfm7k29dp5nb0c9sb2ix506ph6dixsjis6xxcz6acq7lg2k"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TFBSTools" version))
       (sha256
        (base32
         "1avgjv2nyr28cla0z9dvh3v0hr1f561sz2as1k53a42irbjk0var"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifmatchr" version))
       (sha256
        (base32
         "1vif3dp4lv4jz1pxsq0ig791ir8q65jmrlqmkyfxpfxfj5alqnbm"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromVAR" version))
       (sha256
        (base32 "1birmwvcvl2hb5ygbsb3wvbdfayqqs53j1bks46cbkq7ybigfyar"))))
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
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SingleCellExperiment" version))
       (sha256
        (base32
         "1bwdhx0ss0s8f4xdgwd7x48apn849x4dyb5hbyglcz31r5vh1qgz"))))
    (properties
     `((upstream-name . "SingleCellExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-delayedarray" ,r-delayedarray)
       ("r-genomicranges" ,r-genomicranges)
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
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scuttle" version))
       (sha256
        (base32
         "015k879gg2r39lp6mz26jwmysa56k0ial6v74zzmbi8hnz9zic3i"))))
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
    (version "1.20.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scater" version))
              (sha256
               (base32
                "1046fgf93dda3y2z5ilrjdczz7klj67ag63p4p1h03965wpj9vfn"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocneighbors" ,r-biocneighbors)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biocsingular" ,r-biocsingular)
       ("r-delayedarray" ,r-delayedarray)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-ggbeeswarm" ,r-ggbeeswarm)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-matrix" ,r-matrix)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rlang" ,r-rlang)
       ("r-rtsne" ,r-rtsne)
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
    (version "1.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scran" version))
       (sha256
        (base32
         "0ilzp6ngw9pq88gk79iic2zxfh5jaic6lnq3xfwc9a269bjylff0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beachmat" ,r-beachmat)
       ("r-bh" ,r-bh)
       ("r-biocgenerics" ,r-biocgenerics)
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
       ("r-metapod" ,r-metapod)
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "sparseMatrixStats" version))
       (sha256
        (base32
         "0ilspddfkqpnf2lng9jjs2ahv6vlc6sap69fzkcw314rha59kncr"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DelayedMatrixStats" version))
       (sha256
        (base32
         "02r68rzmc54m353fpw5ampyv26i5622bc7iihfqlpy6p3033lpqk"))))
    (properties
     `((upstream-name . "DelayedMatrixStats")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-delayedarray" ,r-delayedarray)
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MsCoreUtils" version))
       (sha256
        (base32
         "13g8a726vsyjs6m1kd42vf8avdnmhykxhl6z3j2njzkp2jg17pd8"))))
    (properties `((upstream-name . "MsCoreUtils")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-clue" ,r-clue)
       ("r-mass" ,r-mass)
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
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "BiocIO" version))
        (sha256
          (base32
            "0skwnpxl6fkqihidpqrfzzh7b05x11j6jwkiinmhggmv0ggjb5ph"))))
    (properties `((upstream-name . "BiocIO")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msmsEDA" version))
       (sha256
        (base32
         "0555a3riyp781mlffmnf93igfq8vl8wjk51pa9qb48qkdn6y8pfc"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msmsTests" version))
       (sha256
        (base32
         "11vhy1l6za73dpdj85q4ksaghgd8vb2h8v9iirmsn2vpajqrvgjh"))))
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
     "This package provides statistical tests for label-free LC-MS/MS data
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
    (version "1.16.2")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "CATALYST" version))
        (sha256
          (base32
            "1vw8x54hwgcyn29k81zcvawawy2iy2qgp49gjpb830q04gx6yc6h"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "erma" version))
       (sha256
        (base32
         "02a9702sasgighwb0f9ii8n30ngfx1hjnppaay1f5zsigr8vqalz"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggbio" version))
       (sha256
        (base32
         "0k3dxacrwgyrkvm7ggc2s1s1pbfs6c0a7ngykkj7jnc73czvnilx"))))
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
    (version "1.36.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Gviz" version))
       (sha256
        (base32
         "0lp0k8jd4dfsfn10706124graaqnzcyv1siblvm8dn2ykw2rc6vl"))))
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
    (version "2.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gwascat" version))
       (sha256
        (base32
         "0109cxsviq1sk5hfwkjzd0y1kpm934287asjjd0j8jhh4b0ah2b2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationhub" ,r-annotationhub)
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
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGgraph" version))
       (sha256
        (base32 "0zxdph5hzr3kzj2g1mjqpiviwa189a5sq4bw7wiry6r79fdnklqs"))))
    (properties `((upstream-name . "KEGGgraph")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-graph" ,r-graph)
       ("r-rcurl" ,r-rcurl)
       ("r-rgraphviz" ,r-rgraphviz)
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
    (version "1.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ldblock" version))
       (sha256
        (base32
         "16vp5psmigxdkkd6fbivb6s8mvd7rsnm771wsbjvayl2y7ig7kq4"))))
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

;; This is a CRAN package, but it depends on r-rgraphviz, which is a
;; Bioconductor package.
(define-public r-abn
  (package
    (name "r-abn")
    (version "2.5-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abn" version))
       (sha256
        (base32
         "1fqmhw0mhdl6az1gpg0byvx5snhz1pl3fqikhyfjcjrc9xbsq8yw"))))
    (build-system r-build-system)
    (inputs
     `(("gsl" ,gsl)))
    (propagated-inputs
     `(("r-lme4" ,r-lme4)
       ("r-nnet" ,r-nnet)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rgraphviz" ,r-rgraphviz)
       ("r-rjags" ,r-rjags)))
    (home-page "https://r-bayesian-networks.org/")
    (synopsis "Modelling multivariate data with additive bayesian networks")
    (description
     "Bayesian network analysis is a form of probabilistic graphical models
which derives from empirical data a directed acyclic graph, DAG, describing
the dependency structure between random variables.  An additive Bayesian
network model consists of a form of a DAG where each node comprises a
@dfn{generalized linear model} (GLM).  Additive Bayesian network models are
equivalent to Bayesian multivariate regression using graphical modelling, they
generalises the usual multivariable regression, GLM, to multiple dependent
variables.  This package provides routines to help determine optimal Bayesian
network models for a given data set, where these models are used to identify
statistical dependencies in messy, complex data.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on r-rsamtools, which is a
;; Bioconductor package.
(define-public r-spp
  (package
    (name "r-spp")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "spp" version))
              (sha256
               (base32
                "08zxxgyp0h6733b08jmml7k4rhfd3mi5dda3jrzid0s184y0z29w"))))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-catools" ,r-catools)
       ("r-rcpp" ,r-rcpp)
       ("r-rsamtools" ,r-rsamtools)))
    (home-page "https://cran.r-project.org/web/packages/spp/")
    (synopsis "ChIP-Seq processing pipeline")
    (description "This package provides tools for analysis of ChIP-seq and
other functional sequencing data.")
    (license license:gpl2)))

(define-public r-pathview
  (package
    (name "r-pathview")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pathview" version))
       (sha256
        (base32 "1fank0qavv9ikmzxvms8mky2wbzny02rfqkvsqzma26r3vl4r1g1"))))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "snpStats" version))
       (sha256
        (base32
         "11vvih5y9kvyjfp2navkfpp4xiyfgwlv4r0x3p52hkj92pc2pg3g"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromstaR" version))
       (sha256
        (base32
         "09cqzylci4x4i7wn4ckcqhdlljbzfrp08wdvkkc5vrdldnj9pq5h"))))
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

(define-public r-guitar
  (package
    (name "r-guitar")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Guitar" version))
       (sha256
        (base32
         "1q4m6c5181dw12lvdp324jlk78c9jgcsg6b9hk87zjadp6j0gfr6"))))
    (properties `((upstream-name . "Guitar")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-dplyr" ,r-dplyr)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-knitr" ,r-knitr)
       ("r-magrittr" ,r-magrittr)
       ("r-rtracklayer" ,r-rtracklayer)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/Guitar")
    (synopsis "Visualize genomic features")
    (description
     "This package is designed for visualization of RNA-related genomic
features with respect to the landmarks of RNA transcripts, i.e., transcription
starting site, start codon, stop codon and transcription ending site.")
    (license license:gpl2)))

(define-public r-sushi
  (package
    (name "r-sushi")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Sushi" version))
              (sha256
               (base32
                "1m5l0nflhcynb3gz7b8qzvknb0s6xhds8z1yl3mbv8ic9qn2knv4"))))
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

(define-public r-tximeta
  (package
    (name "r-tximeta")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "tximeta" version))
       (sha256
        (base32
         "0ipgpcl93cac4qff6lp9x2l3gav5kb1x1d56g32h09hlm797rvvh"))))
    (properties `((upstream-name . "tximeta")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationhub" ,r-annotationhub)
       ("r-biocfilecache" ,r-biocfilecache)
       ("r-biostrings" ,r-biostrings)
       ("r-ensembldb" ,r-ensembldb)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-jsonlite" ,r-jsonlite)
       ("r-matrix" ,r-matrix)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-tibble" ,r-tibble)
       ("r-tximport" ,r-tximport)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/mikelove/tximeta")
    (synopsis "Transcript quantification import with automatic metadata")
    (description
     "This package implements transcript quantification import from Salmon and
alevin with automatic attachment of transcript ranges and release information,
and other associated metadata.  De novo transcriptomes can be linked to the
appropriate sources with linkedTxomes and shared for computational
reproducibility.")
    (license license:gpl2)))
