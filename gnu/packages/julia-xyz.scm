;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2021, 2022 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 jgart <jgart@dismail.de>
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

(define-module (gnu packages julia-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system julia)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages julia-jll)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control))

(define-public julia-abstractffts
  (package
    (name "julia-abstractffts")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/AbstractFFTS.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0083pwdyxjb04i330ir9pc8kmp4bwk59lx1jgc9qi05y8j7xzbp0"))))
    (build-system julia-build-system)
    (inputs                             ;required for tests
     (list julia-unitful))
    (home-page "https://github.com/JuliaGPU/Adapt.jl")
    (synopsis "General framework for fast Fourier transforms (FFTs)")
    (description "This package allows multiple FFT packages to co-exist with
the same underlying @code{fft(x)} and @code{plan_fft(x)} interface.  It is
mainly not intended to be used directly.  Instead, developers of packages that
implement FFTs (such as @code{FFTW.jl} or @code{FastTransforms.jl}) extend the
types/functions defined in AbstractFFTs.")
    (license license:expat)))

(define-public julia-abstracttrees
  (package
    (name "julia-abstracttrees")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaCollections/AbstractTrees.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16is5n2qa69cci34vfazxsa7ik6q0hbnnqrbrhkq8frh142f1xs8"))))
    (build-system julia-build-system)
    (home-page "https://juliacollections.github.io/AbstractTrees.jl/stable/")
    (synopsis "Abstract Julia interfaces for working with trees")
    (description "This Julia package provides several utilities for working
with tree-like data structures.  Most importantly, it defines the
@code{children} method that any package that contains such a data structure
may import and extend in order to take advantage of any generic tree algorithm
in this package.")
    (license license:expat)))

(define-public julia-adapt
  (package
    (name "julia-adapt")
    (version "3.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaGPU/Adapt.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "009fj59fzhvfsyw35kakllsh36k3xlwyzq8qa5f5k598i3pq14i7"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaGPU/Adapt.jl")
    (synopsis "Package providing the @code{adapt} function, similar to @code{convert}")
    (description "This Julia package provides the @code{adapt(T, x)} function
acts like @code{convert(T, x)}, but without the restriction of returning a
@code{T}.  This allows you to \"convert\" wrapper types like @code{Adjoint} to
be GPU compatible without throwing away the wrapper.")
    (license license:expat)))

(define-public julia-ansicoloredprinters
  (package
    (name "julia-ansicoloredprinters")
    (version "0.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/ANSIColoredPrinters.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0dp5agljr0g50s5gn0pr70wrz01ggck6pb40ay3l4szhswq7mqzf"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaDocs/ANSIColoredPrinters.jl")
    (synopsis "ANSI escape code translator")
    (description "@code{ANSIColoredPrinters.jl} converts a text qualified by
ANSI escape codes to another format.")
    (license license:expat)))

(define-public julia-aqua
  (package
    (name "julia-aqua")
    (version "0.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaTesting/Aqua.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1g0kyzcdykgs247j72jpc2qqall696jwgb3hnn4cxmbi8bkf7wpk"))))
    (build-system julia-build-system)
    (arguments
     `(#:parallel-tests? #f))
    (home-page "https://github.com/JuliaTesting/Aqua.jl")
    (synopsis "Automated quality assurance for Julia packages")
    (description "@acronym{Aqua.jl, Auto QUality Assurance for Julia packages},
provides functions to run a few automatable checks for Julia packages.")
    (license license:expat)))

(define-public julia-arrayinterface
  (package
    (name "julia-arrayinterface")
    (version "3.1.19")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/ArrayInterface.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0cmldnzvdgmfnrnrzgj6v1mfr2rvk5096392rwmhd3iyx7v0pq33"))))
    (build-system julia-build-system)
    (arguments
     ;; XXXX: Unexpected failures for i686, e.g.,
     ;; Expression: @inferred(ArrayInterface.size(Rnr)) === (StaticInt(4),)
     ;; Evaluated: (static(2),) === (static(4),)
     ;; Disable as stopgap.
     `(#:tests? ,(not (target-x86-32?))))
    (propagated-inputs
     (list julia-ifelse
           julia-requires
           julia-static))
    (native-inputs
     (list julia-aqua
           julia-bandedmatrices
           julia-blockbandedmatrices
           julia-ifelse
           julia-offsetarrays
           julia-staticarrays))
    (home-page "https://github.com/JuliaArrays/ArrayInterface.jl")
    (synopsis "Base array interface primitives")
    (description "The purpose of this library is to solidify extensions to the
current @code{AbstractArray} interface, which are put to use in package
ecosystems like @code{DifferentialEquations.jl}.  Since these libraries are
live, this package will serve as a staging ground for ideas before they are
merged into Base Julia.  For this reason, no functionality is exported so that
if such functions are added and exported in a future Base Julia, there will be
no issues with the upgrade.")
    (license license:expat)))

(define-public julia-arraylayouts
  (package
    (name "julia-arraylayouts")
    (version "0.7.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/ArrayLayouts.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05q62pks8j23pgbrfny072rrwzrz6q19l68srnjxxv39ncmdmrvg"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-fillarrays))
    (home-page "https://github.com/JuliaMatrices/ArrayLayouts.jl")
    (synopsis "Array layouts and general fast linear algebra")
    (description "This package implements a trait-based framework for describing
array layouts such as column major, row major, etc. that can be dispatched to
appropriate BLAS or optimised Julia linear algebra routines.  This supports a
much wider class of matrix types than Julia's in-built @code{StridedArray}.")
    (license license:expat)))

(define-public julia-automa
  (package
    (name "julia-automa")
    (version "0.8.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/BioJulia/Automa.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0hmwvk3qw54p7f63a2dnzlmvkynfs62x9n8x952bcmczp35csgq0"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-scanbyte
           julia-transcodingstreams))
    (home-page "https://github.com/BioJulia/Automa.jl")
    (synopsis "Validation, parsing, and tokenizing based on state machine compiler")
    (description "This package compiles regular expressions into Julia code,
which is then compiled into low-level machine code by the Julia compiler.  The
package is designed to generate very efficient code to scan large text data,
which is often much faster than handcrafted code.  @code{Automa.jl} can insert
arbitrary Julia code that will be executed in state transitions.  This makes
it possible, for example, to extract substrings that match a part of a regular
expression.")
    (license license:expat)))

(define-public julia-axisalgorithms
  (package
    (name "julia-axisalgorithms")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/timholy/AxisAlgorithms.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00x85lnfln7xkfnirpplzyi8r6q92nfqwya8il156bf7b1pa20gk"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-woodburymatrices))
    (home-page "https://github.com/timholy/AxisAlgorithms.jl")
    (synopsis "Filtering and linear algebra routines for multidimensional arrays")
    (description "@code{AxisAlgorithms} is a collection of filtering and linear
algebra algorithms for multidimensional arrays.  For algorithms that would
typically apply along the columns of a matrix, you can instead pick an arbitrary
axis (dimension).")
    (license license:expat)))

(define-public julia-axisarrays
  (package
    (name "julia-axisarrays")
    (version "0.4.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/AxisArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "03kzan1lm4fxfhzv1xjg3ysf6y7nagcc61vfz15kvdrp1dqxlynk"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-rangearrays
           julia-intervalsets
           julia-itertools))
    (native-inputs
     (list julia-offsetarrays
           julia-unitful))
    (home-page "http://juliaarrays.github.io/AxisArrays.jl/latest/")
    (synopsis "Arrays where each dimension can have a named axis with values")
    (description "This package for the Julia language provides an array type
(the AxisArray) that knows about its dimension names and axis values.  This
allows for indexing by name without incurring any runtime overhead.  This
permits one to implement algorithms that are oblivious to the storage order of
the underlying arrays.  AxisArrays can also be indexed by the values along their
axes, allowing column names or interval selections.")
    (license license:expat)))

(define-public julia-bandedmatrices
  (package
    (name "julia-bandedmatrices")
    (version "0.16.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/BandedMatrices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0rlfj9gr9ss621v5kw5b06206yaak21s2vq9vk7r8a7p2ylncism"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-arraylayouts
           julia-fillarrays))
    (native-inputs
     (list julia-genericlinearalgebra))
    (home-page "https://github.com/JuliaMatrices/BandedMatrices.jl")
    (synopsis "Julia package for representing banded matrices")
    (description "This package supports representing banded matrices by only
the entries on the bands.")
    (license license:expat)))

(define-public julia-benchmarktools
  (package
    (name "julia-benchmarktools")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaCI/BenchmarkTools.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xz3kdrphp4b158pg7dwkiry49phs2fjjpdvk1hjpww5ykxacks8"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       ,@(if (target-x86-32?)
           '((modify-phases %standard-phases
               (add-after 'unpack 'remove-failing-tests-i686
                 (lambda _
                   (substitute* "test/GroupsTests.jl"
                     (("@test sprint\\(show, g1\\)")
                      "@test_broken sprint(show, g1)")
                     (("@test sprint\\(show, g1; context = :boundto => 1\\)")
                      "@test_broken sprint(show, g1; context = :boundto => 1)")
                     (("@test sprint\\(show, g1; context = :limit => false\\)")
                      "@test_broken sprint(show, g1; context = :limit => false)")
                     (("@test @test_deprecated") "@test_broken"))
                   (substitute* "test/ExecutionTests.jl"
                     ;; Evaluated: 12 == 8
                     (("@test @ballocated\\(Ref\\(1\\)\\)")
                      "@test_broken @ballocated(Ref(1))"))))))
           '(%standard-phases))))
    (propagated-inputs
     (list julia-json))
    (home-page "https://github.com/JuliaCI/BenchmarkTools.jl")
    (synopsis "Benchmarking framework for the Julia language")
    (description "@code{BenchmarkTools.jl} makes performance tracking of Julia
code easy by supplying a framework for writing and running groups of
benchmarks as well as comparing benchmark results.")
    (license license:expat)))

(define-public julia-bioalignments
  (package
    (name "julia-bioalignments")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BioJulia/BioAlignments.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wf6qgsada59r2fykxfj9hcr635wl8maqxbd3w8qpa01k9glxa0k"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-test
           (lambda _
             ;; Test fails because an unexpected type representation from
             ;; BioSequences.  The aligned value is correct though.
             (substitute* "test/runtests.jl"
               (("@test sprint\\(show, aln\\)")
                "@test_broken sprint(show, aln)")))))))
    (propagated-inputs
     (list julia-biogenerics
           julia-biosequences
           julia-biosymbols
           julia-intervaltrees))
    (home-page "https://github.com/BioJulia/BioAlignments.jl")
    (synopsis "Sequence alignment algorithm and data structures")
    (description "This package provides alignment algorithms and data
structures for sequence of DNA, RNA, and amino acid sequences.")
    (license license:expat)))

(define-public julia-biogenerics
  ;; No upstream release
  (let ((commit "a75abaf459250e2b5e22b4d9adf25fd36d2acab6")
        (revision "1"))
    (package
      (name "julia-biogenerics")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BioJulia/BioGenerics.jl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17d222vi9nssjwr5l349fss7jnglnjimp1z62kmfmxa4fsn8lk8l"))))
      (build-system julia-build-system)
      (inputs
       (list julia-transcodingstreams))
      (home-page "https://github.com/BioJulia/BioGenerics.jl")
      (synopsis "Generic methods used by BioJulia packages")
      (description "This package provides generic methods and modules used in
many of the other BioJulia packages.  This package defines IO, exceptions, and
other types or methods used by other BioJulia packages.")
      (license license:expat))))

(define-public julia-biosequences
  (package
    (name "julia-biosequences")
    (version "2.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BioJulia/BioSequences.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ns6zk0zvnsf4hlsys9ck2xrn20qck0b0aghh484vc6n458zq2gw"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-biogenerics
           julia-biosymbols
           julia-combinatorics
           julia-indexablebitvectors
           julia-stablerngs
           julia-twiddle))
    (native-inputs
     (list julia-statsbase
           julia-yaml))
    (home-page "https://biojulia.net/BioSequences.jl/stable/")
    (synopsis "Data types and methods for common operations with biological sequences")
    (description "This package provides Data types and methods for common
operations with biological sequences, including DNA, RNA, and amino acid
sequences.")
    (license license:expat)))

(define-public julia-biosymbols
  (package
    (name "julia-biosymbols")
    ;; Older release for compatibility with julia-biosequences.
    (version "4.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BioJulia/BioSymbols.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1222rwdndi777lai8a6dwrh35i5rgmj75kcrhn8si72sxgz0syjm"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-tests
           (lambda _
             (substitute* "test/runtests.jl"
               (("\\@testset \\\"Range.*" all)
                (string-append all " return\n"))))))))
    (propagated-inputs
     (list julia-automa))
    (home-page "https://github.com/BioJulia/BioSymbols.jl")
    (synopsis "Primitive types for nucleic acids and amino acids")
    (description "This package defines the primitive types for nucleic acids
and amino acids that are used ny otherBioJulia packages.")
    (license license:expat)))

(define-public julia-blockarrays
  (package
    (name "julia-blockarrays")
    (version "0.16.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/BlockArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1by26036fk9mawmcgqxpwizgbs398v9p6vrbsgg7h6llqn3q9iw1"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       ,@(if (target-64bit?)
           '(%standard-phases)
           '((modify-phases %standard-phases
               (add-after 'unpack 'fix-tests-int32-i686
                 (lambda _
                   (substitute* "test/test_blockarrays.jl"
                     (("Int64") "Int32")))))))))
    (propagated-inputs
     (list julia-arraylayouts
           julia-fillarrays))
    (native-inputs
     (list julia-lazyarrays
           julia-offsetarrays
           julia-staticarrays))
    (home-page "https://github.com/JuliaArrays/BlockArrays.jl")
    (synopsis "BlockArrays for Julia")
    (description "A block array is a partition of an array into blocks or
subarrays.  This package has two purposes.  Firstly, it defines an interface for
an @code{AbstractBlockArray} block arrays that can be shared among types
representing different types of block arrays.  The advantage to this is that it
provides a consistent API for block arrays.
Secondly, it also implements two different type of block arrays that follow the
@code{AbstractBlockArray} interface.  The type @code{BlockArray} stores each
block contiguously while the type @code{PseudoBlockArray} stores the full matrix
contiguously.  This means that @code{BlockArray} supports fast non copying
extraction and insertion of blocks while @code{PseudoBlockArray} supports fast
access to the full matrix to use in in for example a linear solver.")
    (license license:expat)))

(define-public julia-blockbandedmatrices
  (package
    (name "julia-blockbandedmatrices")
    (version "0.10.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/BlockBandedMatrices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "10n1r6kmmv2wa307jfg9y2m6p16j8hngjp3fjavpbdy1r5haasm9"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-arraylayouts
           julia-bandedmatrices
           julia-blockarrays
           julia-fillarrays
           julia-matrixfactorizations))
    (home-page "https://github.com/JuliaMatrices/BlockBandedMatrices.jl")
    (synopsis "Block-banded matrices and banded-block-banded matrices")
    (description "This package supports representing block-banded and
banded-block-banded matrices by only storing the entries in the non-zero bands.
A @code{BlockBandedMatrix} is a subtype of @code{BlockMatrix} of
@code{BlockArrays.jl} whose layout of non-zero blocks is banded.")
    (license license:expat)))

(define-public julia-bson
  (package
    (name "julia-bson")
    (version "0.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/BSON.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1l5608ma2ys7v2gpcqbiv9mwfc6yrlqkihrfx1pf7fgv5llhd4fn"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-dataframes))
    (home-page "https://github.com/JuliaIO/BSON.jl")
    (synopsis "Binary JSON serialisation format")
    (description "@code{BSON.jl} is a Julia package for working with the Binary
JSON serialisation format.  It can be used as a general store for Julia data
structures.")
    (license license:expat)))

(define-public julia-bufferedstreams
  (package
    (name "julia-bufferedstreams")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BioJulia/BufferedStreams.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sf4sxbq55mg2pwxyxf0c839z1lk0yxg8nmb7617bfbvw31cp88z"))))
    (build-system julia-build-system)
    ;; The package is old and tests are using undefined functions.  They also
    ;; freeze, see
    ;; https://travis-ci.org/BioJulia/BufferedStreams.jl/jobs/491050182
    (arguments
     '(#:tests? #f
       #:julia-package-name "BufferedStreams"
       #:julia-package-uuid "e1450e63-4bb3-523b-b2a4-4ffa8c0fd77d"))
    (propagated-inputs
     (list julia-compat))
    (home-page "https://github.com/BioJulia/BufferedStreams.jl")
    (synopsis "Fast composable IO streams")
    (description "@code{BufferedStreams.jl} provides buffering for IO
operations.  It can wrap any @code{IO} type automatically making incremental
reading and writing faster.")
    (license license:expat)))

(define-public julia-calculus
  (package
    (name "julia-calculus")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/Calculus.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xh0ak2ycsjw2h86ja24ch3kn2d18zx3frrds78aimwdnqb1gdc2"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaMath/Calculus.jl")
    (synopsis "Common utilities for automatic differentiation")
    (description "This package provides tools for working with the basic
calculus operations of differentiation and integration.  The @code{Calculus}
package produces approximate derivatives by several forms of finite
differencing or produces exact derivative using symbolic differentiation.  It
can also be used to compute definite integrals by different numerical
methods.")
    (license license:expat)))

(define-public julia-categoricalarrays
  (package
    (name "julia-categoricalarrays")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaData/CategoricalArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bcfylxdaizgasnmlkjjkf4dgfvy2y9ycnphw2d0z6mm9vx3n04x"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-pooledarrays))
    (propagated-inputs
     (list julia-dataapi
           julia-json
           julia-json3
           julia-missings
           julia-recipesbase
           julia-structtypes))
    (home-page "https://github.com/JuliaData/CategoricalArrays.jl")
    (synopsis "Arrays for working with categorical data")
    (description "This package provides tools for working with categorical
variables, both with unordered (nominal variables) and ordered categories
(ordinal variables), optionally with missing values.")
    (license license:expat)))

(define-public julia-chainrules
  (package
    (name "julia-chainrules")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/ChainRules.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0if93pd3b3scg2x3gmk1cbwjk0ax1n792vy8c38y3xl7jpd5cb70"))))
    (build-system julia-build-system)
    (inputs                             ;required for test
     (list julia-chainrulestestutils
           julia-finitedifferences
           julia-nanmath
           julia-specialfunctions))
    (propagated-inputs
     (list julia-chainrulescore
           julia-compat
           julia-reexport
           julia-requires))
    (home-page "https://github.com/JuliaDiff/ChainRules.jl")
    (synopsis "Common utilities for automatic differentiation")
    (description "The is package provides a variety of common utilities that
can be used by downstream automatic differentiation (AD) tools to define and
execute forward-, reverse-, and mixed-mode primitives.")
    (license license:expat)))

(define-public julia-chainrulescore
  (package
    (name "julia-chainrulescore")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/ChainRulesCore.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1866xv30h1bi7f2m993nljzf58wwmv8zlgn6ffn9j3wckch1nfpb"))))
    (build-system julia-build-system)
    (inputs                             ;required for tests
     (list julia-benchmarktools
           julia-staticarrays))
    (propagated-inputs
     (list julia-compat))
    (home-page "https://github.com/JuliaDiff/ChainRulesCore.jl")
    (synopsis "Common utilities used by downstream automatic differentiation tools")
    (description "The package provides a light-weight dependency for defining
sensitivities for functions without the need to depend on ChainRules itself.")
    (license license:expat)))

(define-public julia-chainrulestestutils
  (package
    (name "julia-chainrulestestutils")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/ChainRulesTestUtils.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vlkyp72d514gyb4k3yhjl1g7f24ncmz61j56p4sdi9f76rk9fx9"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-chainrulescore
           julia-compat
           julia-finitedifferences))
    (home-page "https://github.com/JuliaDiff/ChainRulesTestUtils.jl")
    (synopsis "Common utilities used by downstream automatic differentiation tools")
    (description "This package is designed to help in testing
@code{ChainRulesCore.frule} and @code{ChainRulesCore.rrule} methods.  The main
entry points are @code{ChainRulesTestUtils.frule_test},
@code{ChainRulesTestUtils.rrule_test}, and
@code{ChainRulesTestUtils.test_scalar}. Currently this is done via testing the
rules against numerical differentiation (using @code{FiniteDifferences.jl}).

@code{ChainRulesTestUtils.jl} is separated from @code{ChainRulesCore.jl} so that it
can be a test-only dependency, allowing it to have potentially heavy
dependencies, while keeping @code{ChainRulesCore.jl} as light-weight as possible.")
    (license license:expat)))

(define-public julia-codeczlib
  (package
    (name "julia-codeczlib")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaIO/CodecZlib.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xm603nylkwk4bzx66zv1g3syzrvn3jh9spdx7kvcvgszzyrrgh4"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'reset-gzip-timestamps 'make-files-writable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each make-file-writable
                         (find-files out "\\.gz$"))
               #t))))))
    (propagated-inputs
     (list julia-transcodingstreams
           julia-zlib-jll))
    (home-page "https://github.com/JuliaIO/CodecZlib.jl")
    (synopsis "Zlib codecs for @code{TranscodingStreams.jl}")
    (description "This package provides zlib codecs for
@code{TranscodingStreams.jl}.")
    (license license:expat)))

(define-public julia-colors
  (package
    (name "julia-colors")
    (version "0.12.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaGraphics/Colors.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kx3hq7rf8p5zx6ly9k5j90zijmc7yrwmy96cgkl2ibdfbnhmya3"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-colortypes
           julia-fixedpointnumbers
           julia-reexport))
    (home-page "https://github.com/JuliaGraphics/Colors.jl")
    (synopsis "Tools for dealing with color")
    (description "This package provides a wide array of functions for dealing
with color.  This includes conversion between colorspaces, measuring distance
between colors, simulating color blindness, parsing colors, and generating
color scales for graphics.")
    (license license:expat)))

(define-public julia-colorschemes
  (package
    (name "julia-colorschemes")
    (version "3.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/ColorSchemes.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0kpjhmqd5cj8dh8bmh9b5g6bscyp7h23hzpr2s93pnrp57q1wvhq"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-colors
           julia-colortypes
           julia-fixedpointnumbers
           julia-staticarrays))
    (home-page "https://github.com/JuliaGraphics/ColorSchemes.jl")
    (synopsis "Colorschemes, colormaps, gradients, and palettes")
    (description "This package provides a collection of colorschemes.")
    (license license:expat)))

(define-public julia-colortypes
  (package
    (name "julia-colortypes")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaGraphics/ColorTypes.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n7h70caqv7yd0khjhn90iax62r73mcif8qzkwj5b4q46li1r8ih"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-fixedpointnumbers))
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/JuliaGraphics/ColorTypes.jl")
    (synopsis "Basic color types and constructor")
    (description "This minimalistic package serves as the foundation for
working with colors in Julia.  It defines basic color types and their
constructors, and sets up traits and show methods to make them easier to work
with.")
    (license license:expat)))

(define-public julia-colorvectorspace
  (package
    (name "julia-colorvectorspace")
    (version "0.9.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/ColorVectorSpace.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02gk7f5g5wjxdasbjf8bvv1m7clksh7mw1xmygjdirjz1q0d6dwi"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-colortypes
           julia-specialfunctions
           julia-tensorcore))
    (native-inputs
     (list julia-colors))
    (home-page "https://github.com/JuliaGraphics/ColorVectorSpace.jl")
    (synopsis "Treat colors as n-vectors for the purposes of arithmetic")
    (description "This package is an add-on to @code{ColorTypes.jl} and provides
fast mathematical operations for objects with types such as RGB and Gray.
Specifically, with this package both grayscale and RGB colors are treated as if
they are points in a normed vector space.")
    (license license:expat)))

(define-public julia-combinatorics
  (package
    (name "julia-combinatorics")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Combinatorics.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0gafqkqi874zfm9h99akw9q95lk3ih5gip2h8p12fj9h7rvyf4j5"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaMath/Combinatorics.jl")
    (synopsis "Combinatorics library for Julia")
    (description "This package provides a combinatorics library for Julia,
focusing mostly (as of now) on enumerative combinatorics and permutations.")
    (license license:expat)))

(define-public julia-commonsubexpressions
  (package
    (name "julia-commonsubexpressions")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rdeits/CommonSubexpressions.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mgy90kk8ksv3l720kkk04gnhn4aqhh2dj4sp3x8yy3limngfjay"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-macrotools))
    (home-page "https://github.com/rdeits/CommonSubexpressions.jl")
    (synopsis "@code{@@cse} macro for Julia")
    (description "This package provides the @code{@@cse} macro, which performs
common subexpression elimination.")
    (license license:expat)))

(define-public julia-compat
  (package
    (name "julia-compat")
    (version "3.39.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaLang/Compat.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qzvaqi5gqgc747fnajbvvf5vqbh6cwykwky00c7glvmvdsgk3z0"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaLang/Compat.jl")
    (synopsis "Compatibility across Julia versions")
    (description "The Compat package is designed to ease interoperability
between older and newer versions of the Julia language.  The Compat package
provides a macro that lets you use the latest syntax in a backwards-compatible
way.")
    (license license:expat)))

(define-public julia-configurations
  (package
    (name "julia-configurations")
    (version "0.16.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Roger-luo/Configurations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1b23p0zk8dx2sf01cnw177mqci7qd81b9s32ixz9clsh0r0icl1b"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'fix-tests
           (lambda _
             (substitute* "test/runtests.jl"
               (("option.toml") "test/option.toml"))))
         (add-after 'link-depot 'dont-use-exproniconlite
           (lambda _
             (substitute* '("Project.toml"
                            "src/Configurations.jl"
                            "test/runtests.jl")
               (("ExproniconLite") "Expronicon"))
             (substitute* "Project.toml"
               (("55351af7-c7e9-48d6-89ff-24e801d99491")
                "6b7a57c9-7cc1-4fdf-b7f5-e857abae3636"))))
         ,@(if (target-64bit?)
             '()
             '((add-after 'unpack 'fix-tests-int32-i686
                 (lambda _
                   (substitute* "test/runtests.jl"
                     (("Int64") "Int32")))))))))
    (propagated-inputs
     (list julia-crayons
           julia-expronicon
           julia-orderedcollections))
    (home-page "https://configurations.rogerluo.dev/stable/")
    (synopsis "Tools for options and configurations in Julia")
    (description "@code{Configurations.jl} provides a macro @code{@@option} to
let you define @code{structs} to represent options/configurations, and serialize
between different option/configuration file formats such as @code{TOML}.")
    (license license:expat)))

(define-public julia-constructionbase
  (package
    (name "julia-constructionbase")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaObjects/ConstructionBase.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jk3h446vkv4yaavgm1hf1az7cwhppvhklvr08s49hhg02cm750q"))))
    (build-system julia-build-system)
    (home-page "https://juliaobjects.github.io/ConstructionBase.jl/dev/")
    (synopsis "Primitive functions for construction of objects")
    (description "This very lightweight package provides primitive functions
for construction of objects.")
    (license license:expat)))

(define-public julia-coordinatetransformations
  (package
    (name "julia-coordinatetransformations")
    (version "0.6.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGeometry/CoordinateTransformations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15zbkn32v7xlz7559s0r5a0vkwmjwsswxaqpzijly4lky4jnp33d"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-staticarrays))
    (native-inputs
    (list julia-documenter
          julia-forwarddiff
          julia-unitful))
    (home-page "https://github.com/JuliaGeometry/CoordinateTransformations.jl")
    (synopsis "Coordinate transformations in Julia")
    (description "@code{CoordinateTransformations} is a Julia package to manage
simple or complex networks of coordinate system transformations.
Transformations can be easily applied, inverted, composed, and differentiated
(both with respect to the input coordinates and with respect to transformation
parameters such as rotation angle).  Transformations are designed to be
light-weight and efficient enough for, e.g., real-time graphical applications,
while support for both explicit and automatic differentiation makes it easy to
perform optimization and therefore ideal for computer vision applications such
as SLAM (simultaneous localization and mapping).")
    (license license:expat)))

(define-public julia-crayons
  (package
    (name "julia-crayons")
    (version "4.0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/KristofferC/Crayons.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0v3zhjlnb2914bxcj4myl8pgb7m31p77aj2k1bckmqs96jdph10z"))))
    (build-system julia-build-system)
    (home-page "https://github.com/KristofferC/Crayons.jl")
    (synopsis "Colored and styled strings for terminals")
    (description "Crayons is a package that makes it simple to write strings in
different colors and styles to terminals.  It supports the 16 system colors,
both the 256 color and 24 bit true color extensions, and the different text
styles available to terminals.")
    (license license:expat)))

(define-public julia-dataapi
  (package
    (name "julia-dataapi")
    (version "1.7.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/DataAPI.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0swk31p83fycz5cdj4fg6b0wfqj473lnx94q2fl7ybxkkc6afba7"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaData/DataAPI.jl")
    (synopsis "Data-focused namespace for packages to share functions")
    (description "This package provides a namespace for data-related generic
function definitions to solve the optional dependency problem; packages wishing
to share and/or extend functions can avoid depending directly on each other by
moving the function definition to DataAPI.jl and each package taking a
dependency on it.")
    (license license:expat)))

(define-public julia-dataframes
  (package
    (name "julia-dataframes")
    (version "1.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/DataFrames.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1bk0amrghgjrkyn1mm4ac23swwbgszl1d0qyl9137qj5zvv9dasp"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'skip-failing-test
           (lambda _
             ;; Tests with non-standard colors.
             (substitute* "test/show.jl"
               (("test (sprint\\(show, df, context=:color=>true)" _ test)
                (string-append "test_nowarn " test)))
             (substitute* "test/io.jl"
               (("testset \\\"improved.*" all)
                (string-append all "return\n")))
             (substitute* "test/join.jl"
               (("test (levels\\(outerjoin\\(B)" _ test)
                (string-append "test_nowarn " test)))
             #t)))))
    (propagated-inputs
     (list julia-dataapi
           julia-invertedindices
           julia-iteratorinterfaceextensions
           julia-missings
           julia-pooledarrays
           julia-prettytables
           julia-reexport
           julia-sortingalgorithms
           julia-tables
           julia-tabletraits))
    (native-inputs
     (list julia-categoricalarrays
           julia-combinatorics
           julia-datastructures
           julia-datavalues
           julia-offsetarrays
           julia-unitful))
    (home-page "https://dataframes.juliadata.org/stable/")
    (synopsis "In-memory tabular data")
    (description "This package provides a set of tools for working with tabular
data in Julia.  Its design and functionality are similar to those of Pandas from
Python or @code{data.frame}, @code{data.table} and @code{dplyr} from R, making
it a great general purpose data science tool, especially for those coming to
Julia from R or Python.")
    (license license:expat)))

(define-public julia-datastructures
  (package
    (name "julia-datastructures")
    (version "0.18.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaCollections/DataStructures.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hdqp8ipsqdw5bqqkdvz4j6n67x80sj5azr9vzyxwjfsgkfbnk2l"))))
    (propagated-inputs
     (list julia-compat
           julia-orderedcollections))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       ,@(if (target-x86-32?)
           '((modify-phases %standard-phases
               (add-after 'unpack 'remove-failing-test-i686
                 (lambda _
                   ;; The evaluation returns the correct value,
                   ;; Evaluated: "Accumulator(1 => 3, 3 => 4)"
                   ;; but, for some reasons, is considered as failed.
                   (substitute* "test/test_accumulator.jl"
                     (("@test sprint\\(show,Accumulator\\(1 => 3, 3 => 4\\)\\)")
                      "@test_broken sprint(show, Accumulator(1 => 3, 3 => 4))"))))))
           '(%standard-phases))))
    (home-page "https://github.com/JuliaCollections/DataStructures.jl")
    (synopsis "Julia module providing different data structures")
    (description "This package implements a variety of data structures,
including, @code{CircularBuffer}, @code{Queue}, @code{Stack},
@code{Accumulators}, @code{LinkedLists}, @code{SortedDicts} and many others.")
    (license license:expat)))

(define-public julia-datavalueinterfaces
  (package
    (name "julia-datavalueinterfaces")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/DataValueInterfaces.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0g2wj6q7jj956nx6g7dk8x7w1c4l2xcmnr1kq5x8s8fild9kslg8"))))
    (build-system julia-build-system)
    (home-page "https://github.com/queryverse/DataValueInterfaces.jl")
    (synopsis "Interface for DataValues.jl")
    (description "This package allows a few \"forward\" definitions for the
@code{DataValues.jl} package that other packages can utilize for integration
without having to take direct dependencies.")
    (license license:expat)))

(define-public julia-datavalues
  (package
    (name "julia-datavalues")
    (version "0.4.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/DataValues.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15j3hrqq6nazn533bfsvg32xznacbzsl303j1qs48av59ppnvhhv"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f      ; Tests need upgrading with newer Julia version.
       #:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'skip-known-failing-tests
           (lambda _
             ;; See upstream report:
             ;; https://github.com/queryverse/DataValues.jl/issues/83
             (substitute* "test/array/test_reduce.jl"
               ((".*DataValue\\(mapreduce.*") "")
               ((".*DataValue\\(method\\(f.*") ""))
             #t)))))
    (propagated-inputs
     (list julia-datavalueinterfaces))
    (home-page "https://github.com/queryverse/DataValues.jl")
    (synopsis "Missing values for Julia")
    (description "This package provides the type @code{DataValue} that is used
to represent missing data.")
    (license license:expat)))

(define-public julia-deepdiffs
  (package
    (name "julia-deepdiffs")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ssfrr/DeepDiffs.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1gsbxb1d67g05h5bvzz3swdfih6404jrydy724a8dvbdgqvm3sds"))))
    (build-system julia-build-system)
    (home-page "https://github.com/ssfrr/DeepDiffs.jl")
    (synopsis "Compute and pretty-print diffs for data structures")
    (description "@code{DeepDiffs.jl} provides the @code{deepdiff} function,
which finds and displays differences (diffs) between Julia data structures.  It
supports @code{Vectors}, @code{Dicts}, and @code{String}s.  When diffing
dictionaries where values associated with a particular key may change,
@code{deepdiff} will recurse into value to provide a more detailed diff.")
    (license license:expat)))

(define-public julia-dictionaries
  (package
    (name "julia-dictionaries")
    (version "0.3.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/andyferris/Dictionaries.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1mm43hm8hd6sgmkkpqhbqhvap7mpkjwzmz5algxi6manp580gkr5"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-indexing))
    (home-page "https://github.com/andyferris/Dictionaries.jl")
    (synopsis "Alternative interface for dictionaries in Julia")
    (description "This package provides an alternative interface for
dictionaries in Julia, for improved productivity and performance.")
    (license license:expat)))

(define-public julia-distances
  (package
    (name "julia-distances")
    (version "0.10.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/Distances.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1yqd9wg4z15k42mrp4y14j2x0sq7yrjhm5zpqklrw6w6j1c367ig"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'skip-flakey-tests
           (lambda _
             ;; Some combination of these tests fail nondeterministically
             ;; each of the times this package is built.
             (substitute* "test/test_dists.jl"
               (("test dyz ≥") "test_nowarn dyz ≥")
               (("test dist\\(y, x") "test_nowarn dist(y, x")
               (("test dist\\(z, x") "test_nowarn dist(z, x")
               (("test dist\\(z, y") "test_nowarn dist(z, y")))))))
    (propagated-inputs
     (list julia-statsapi))
    (native-inputs
     (list julia-offsetarrays
           julia-unitful))
    (home-page "https://github.com/JuliaStats/Distances.jl")
    (synopsis "Julia package for evaluating distances (metrics) between vectors")
    (description "A Julia package for evaluating distances(metrics) between
vectors.  This package also provides optimized functions to compute column-wise
and pairwise distances, which are often substantially faster than a
straightforward loop implementation.")
    (license license:expat)))

(define-public julia-docstringextensions
  (package
    (name "julia-docstringextensions")
    (version "0.8.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/DocStringExtensions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0fy4kfnfacyfmlly6nqxn77dk2gqw80b69zb4m1i0i39zv3cpqfb"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))        ; Tests try to read SSL certificates.
    (home-page "https://juliadocs.github.io/DocStringExtensions.jl/latest/")
    (synopsis "Extensions for Julia's docsystem")
    (description "This package provides a collection of useful extensions for
Julia's built-in docsystem.  These are features that are not yet mature enough
to be considered for inclusion in Base, or that have sufficiently niche use
cases that including them with the default Julia installation is not seen as
valuable enough at this time.")
    (license license:expat)))

;; By removing all the javascript and css downloads any HTML documentation
;; produced by this package will not be very useful.
(define-public julia-documenter
  (package
    (name "julia-documenter")
    (version "0.27.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/Documenter.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00ai3c24i3fkn5plmavampcxm0ijhwk0v5cn9xwm7rvbjnnvaaam"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/Deps.jl"
               (("pip install")
                (string-append (search-input-file inputs "bin/pip")
                               " install")))))
         (add-after 'link-depot 'remove-javascript-downloads
           (lambda _
             (substitute* "src/Writers/HTMLWriter.jl"
               (("cdnjs.cloudflare.com") "example.com"))
             ;; Removing the javascript downloads causes these tests fail.
             (substitute* "test/examples/tests.jl"
               ((".*Main\\.examples_html_doc.*") "")
               ((".*Main\\.examples_html_mathjax3_doc.*") "")))))))
    (propagated-inputs
     (list julia-ansicoloredprinters
           julia-docstringextensions
           julia-iocapture
           julia-json))
    (inputs
     (list python-wrapper))
    (native-inputs
     (list git-minimal
           julia-documentermarkdown
           julia-documentertools))
    (home-page "https://juliadocs.github.io/Documenter.jl")
    (synopsis "Documentation generator for Julia")
    (description "This package provides a documentation generator for Julia.")
    (license license:expat)))

(define julia-documenter-bootstrap
  (package
    (inherit julia-documenter)
    (name "julia-documenter-bootstrap")
    (arguments
     (substitute-keyword-arguments (package-arguments julia-documenter)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'patch-source)))
       ;; Not all dependencies available in bootstrap version.
       ((#:tests? _ #f) #f)))
    (inputs `())
    (native-inputs `())))

(define-public julia-documentermarkdown
  (package
    (name "julia-documentermarkdown")
    (version "0.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/DocumenterMarkdown.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0sx89hi5p2f8zi2rp5qrv06m270d90pxj5d2y5cxls1spax7wqx8"))))
    (build-system julia-build-system)
    (inputs
     ;; We don't want to propagate the bootstrap version.
     ;; Cycle with Documenter.jl in later versions.
     (list julia-documenter-bootstrap))
    (home-page "https://github.com/JuliaDocs/DocumenterMarkdown.jl")
    (synopsis "Documenter's Markdown")
    (description "This package enables the Markdown / MkDocs backend of
@code{Documenter.jl}.")
    (license license:expat)))

(define-public julia-documentertools
  (package
    (name "julia-documentertools")
    (version "0.1.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/DocumenterTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05p57p8xlkn42m1lv9gq4hl96vp7hpj19d51p828ai1rbpcpi3a6"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Tests require network.
    (inputs
     ;; We don't want to propagate the bootstrap version.
     ;; Cycle with Documenter.jl in later versions.
     (list julia-documenter-bootstrap))
    (propagated-inputs
     (list julia-docstringextensions
           julia-gumbo
           julia-sass))
    (native-inputs
     (list julia-example))
    (home-page "https://github.com/JuliaDocs/DocumenterTools.jl")
    (synopsis "Extra tools for setting up Documenter.jl")
    (description "This package contains utilities for setting up documentation
generation with @code{Documenter.jl}.")
    (license license:expat)))

(define-public julia-diffresults
  (package
    (name "julia-diffresults")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/DiffResults.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w6p3yxajvclax5b9g7cr2jmbc7lvr5nk4gq0aljxdycdq1d2y3v"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-staticarrays))
    (home-page "https://github.com/JuliaDiff/DiffResults.jl")
    (synopsis "In-place differentiation methods of primal values at multi-order")
    (description "This package provides the @code{DiffResult} type, which can
be passed to in-place differentiation methods instead of an output buffer.")
    (license license:expat)))

(define-public julia-diffrules
  (package
    (name "julia-diffrules")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/DiffRules.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cwjvj4gma7924fm3yas0nf0jlnwwx4v7fi79ii3s290lkdldzfl"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-nanmath
           julia-specialfunctions))
    (home-page "https://github.com/JuliaDiff/DiffRules.jl")
    (synopsis "Primitive differentiation rules")
    (description "This package provides primitive differentiation rules that
can be composed via various formulations of the chain rule.  Using
@code{DiffRules}, new differentiation rules can defined, query whether or not
a given rule exists, and symbolically apply rules to simple Julia expressions.")
    (license license:expat)))

(define-public julia-difftests
  (package
    (name "julia-difftests")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/DiffTests.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rxpnd5zi3pxgdd38l5jm2sxc3q6p7g57fqgll2dsiin07y3my57"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaDiff/DiffTests.jl")
    (synopsis "Common test functions for differentiation tools")
    (description "This package contains a common suite of test functions for
stressing the robustness of differentiation tools.")
    (license license:expat)))

(define-public julia-dualnumbers
  (package
    (name "julia-dualnumbers")
    (version "0.6.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDiff/DualNumbers.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05vr5wbzqpchnb96b3pmn67x196mbfnkv7r9bdlz3gm56if4awk5"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'adjust-test-suite
           (lambda _
             (substitute* "test/runtests.jl"
               ;; Seems to not play nicely with SpecialFunctions
               ((".*isempty.*") "")))))))
    (propagated-inputs
     (list julia-calculus
           julia-nanmath
           julia-specialfunctions))
    (home-page "https://github.com/JuliaDiff/DualNumbers.jl")
    (synopsis "Represent dual numbers and for perform dual algebra")
    (description "The @code{DualNumbers} Julia package defines the @code{Dual}
type to represent dual numbers, and supports standard mathematical operations on
them.  Conversions and promotions are defined to allow performing operations on
combinations of dual numbers with predefined Julia numeric types.")
    (license license:expat)))

(define-public julia-ellipsisnotation
  (package
    (name "julia-ellipsisnotation")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ChrisRackauckas/EllipsisNotation.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0py46kxl702r8pw3v7x4cqllf7yc91b0dr7vb60xh2qi7d6y3jc7"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'adjust-test-suite
           (lambda _
             (substitute* "test/runtests.jl"
               ;; Seems to not play nicely with Julia-1.6.
               ((".*basic.jl.*") "")))))))
    (propagated-inputs
     (list julia-arrayinterface))
    (home-page "https://github.com/ChrisRackauckas/EllipsisNotation.jl")
    (synopsis "Elipsis notation implementation")
    (description "This implements the notation @code{..} for indexing arrays.
It's similar to the Python @code{...} in that it means \"all of the columns
before (or after)\".")
    (license license:expat)))

(define-public julia-example
  (let ((commit "f968c69dea24f851d0c7e686db23fa55826b5388"))
    (package
      (name "julia-example")
      (version "0.5.4")                   ;tag not created upstream
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/JuliaLang/Example.jl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1v3z0d6gh6wfbypffy9m9rhh36px6fm5wjzq0y6rbmc95r0qpqlx"))))
      (build-system julia-build-system)
      (home-page "https://github.com/JuliaLang/Example.jl")
      (synopsis "Module providing examples")
      (description "This package provides various examples.")
      (license license:expat))))

;; ExproniconLite.jl is autogenerated from this package.
(define-public julia-expronicon
  (package
    (name "julia-expronicon")
    (version "0.6.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Roger-luo/Expronicon.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0h8aaynqlxrkn8575k5vqmhzil4vvxchhf0bcxa6zwawp558gj2y"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'skip-network-tests
           (lambda _
             (substitute* "test/runtests.jl"
               ;; This test tries to access the Julia package registry.
               ((".*expand\\.jl.*") "")))))))
    (propagated-inputs
     (list julia-mlstyle))
    (native-inputs
     (list julia-documenter))
    (home-page "https://expronicon.rogerluo.dev/dev/")
    (synopsis "Collective tools for metaprogramming on Julia Expr")
    (description "This package provides a collection of tools for
metaprogramming on Julia Expr, the meta programming standard library for
@code{MLStyle}.")
    (license license:expat)))

(define-public julia-exprtools
  (package
    (name "julia-exprtools")
    (version "0.1.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/invenia/ExprTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "058ax5d96jpym5w3g37ah1c4xq3fskwpjdhchakzax15vqzy7ab4"))))
    (build-system julia-build-system)
    (home-page "https://github.com/invenia/ExprTools.jl")
    (synopsis "Light-weight expression manipulation tools")
    (description "@code{ExprTools} provides tooling for working with Julia
expressions during metaprogramming.  This package aims to provide light-weight
performant tooling without requiring additional package dependencies.")
    (license license:expat)))

(define-public julia-ffmpeg
  (package
    (name "julia-ffmpeg")
    (version "0.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/FFMPEG.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ldxbp0kq3ip67x7sp82dz56aq4p5i0chspbgx2zgskr6jcbjj1b"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-ffmpeg-jll
           julia-x264-jll))
    (home-page "https://github.com/JuliaIO/FFMPEG.jl")
    (synopsis "Julia Package for ffmpeg")
    (description "This package is made to be included into packages that just
need the ffmpeg binaries + executables, and don't want the overhead of
@code{VideoIO.jl}.")
    (license license:expat)))

(define-public julia-fileio
  (package
    (name "julia-fileio")
    (version "1.9.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/FileIO.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1b18x43i737g5q41n9818xbnc2pgd98q1m6yw3h29yri0clg4gfx"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'reset-gzip-timestamps)
         (add-after 'link-depot 'skip-network-tests
           (lambda _
             ;; These tests try to download audio/video files.
             (substitute* "test/query.jl"
               (("testset.*(MP4|OGG|MATROSKA).*" all)
                (string-append all "return\n")))
             (substitute* "test/loadsave.jl"
               (("testset.*CSVFiles.*" all)
                (string-append all "return\n")))
             ;; This test tries to download a Julia package.
             (substitute* "test/error_handling.jl"
               (("testset.*Not installed.*" all)
                (string-append all "return\n")))
             ;; This test tries to write to the store.
             ;; (Error says can't find User 0)
             (substitute* "test/runtests.jl"
               ((".*test_mimesave.*") "")))))))
    (propagated-inputs
     (list julia-requires))
    (native-inputs
     (list julia-colortypes
           julia-filepathsbase
           julia-http))
    (home-page "https://github.com/JuliaIO/FileIO.jl")
    (synopsis "Main Package for IO, loading all different kind of files")
    (description "@code{FileIO} aims to provide a common framework for detecting
file formats and dispatching to appropriate readers/writers.  The two core
functions in this package are called @code{load} and @code{save}, and offer
high-level support for formatted files (in contrast with Julia's low-level
@code{read} and @code{write}).")
    (license license:expat)))

(define-public julia-filepathsbase
  (package
    (name "julia-filepathsbase")
    (version "0.9.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rofinn/FilePathsBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "136wm4ik6isrdanmpi4gdr1qw0qhr15i925qzjxbawk5hnyzwng9"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with JLSO.jl
    (home-page "https://github.com/rofinn/FilePathsBase.jl")
    (synopsis "Filesystem path types in Julia")
    (description "@code{FilePathsBase.jl} provides a type based approach to
working with filesystem paths in Julia.")
    (license license:expat)))

(define-public julia-fillarrays
  (package
    (name "julia-fillarrays")
    (version "0.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaArrays/FillArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sx96pzrgyh8k7x2w8vmyi6cazlmp7rg1n7wbl47qfzqjggka6kz"))))
    (build-system julia-build-system)
    (inputs                             ;required by tests
     (list julia-staticarrays))
    (home-page "https://github.com/JuliaArrays/FillArrays.jl")
    (synopsis "Lazy matrix representation")
    (description "This package lazily represents matrices filled with
a single entry, as well as identity matrices.  This package exports the
following types: @code{Eye}, @code{Fill}, @code{Ones}, @code{Zeros},
@code{Trues} and @code{Falses}.")
    (license license:expat)))

(define-public julia-finitediff
  (package
    (name "julia-finitediff")
    (version "2.8.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDiff/FiniteDiff.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "105f6r0hq97n9mxf1nacmz94dpca66vzqj5p3zh4h0brshmggqnq"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; We don't want to run all the tests; the Downstream tests
             ;; try to download the package registry.
             (setenv "GROUP" "Core")
             #t)))))
    (propagated-inputs
     (list julia-arrayinterface
           julia-requires
           julia-staticarrays))
    (native-inputs
     (list julia-bandedmatrices
           julia-blockbandedmatrices
           julia-safetestsets))
    (home-page "https://github.com/JuliaDiff/FiniteDiff.jl")
    (synopsis "Calculations of gradients, Jacobians, and Hessians")
    (description "This package is for calculating derivatives, gradients,
Jacobians, Hessians, etc. numerically.  This library is for maximizing speed
while giving a usable interface to end users in a way that specializes on array
types and sparsity.")
    (license license:expat)))

(define-public julia-finitedifferences
  (package
    (name "julia-finitedifferences")
    (version "0.12.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/FiniteDifferences.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09nsf9cgk49yrvprflnhd9h5rrgs280rgj8sad3csghxdx6jqk5c"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       ,@(if (target-x86-32?)
           '((modify-phases %standard-phases
               (add-after 'unpack 'remove-failing-test-i686
                 (lambda _
                   ;; Machine Precision incorrectly handled
                   (substitute* "test/methods.jl"
                     (("@test central_fdm\\(15, 5, adapt=2\\)\\(exp, 1.0\\)")
                      "@test_broken central_fdm(15, 5, adapt=2)(exp, 1.0)"))))))
           '(%standard-phases))))
    (inputs
     (list julia-benchmarktools))
    (propagated-inputs
     (list julia-chainrulescore
           julia-richardson
           julia-staticarrays))
    (home-page "https://github.com/JuliaDiff/FiniteDifferences.jl")
    (synopsis "Estimates derivatives with finite differences")
    (description "This package calculates approximate derivatives numerically
using finite difference.")
    (license license:expat)))

(define-public julia-fixedpointnumbers
  (package
    (name "julia-fixedpointnumbers")
    (version "0.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/FixedPointNumbers.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j0n40n04q9sk68wh9jq90m6c67k4ws02k41djjzkrqmpzv4rcdi"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'disable-failing-test
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "test/fixed.jl"
               ;; A deprecation warning is not thrown
               (("@test_logs.*:warn" all) (string-append "# " all)))
             #t)))))
    (propagated-inputs
     (list julia-compat))
    (home-page "https://github.com/JuliaMath/FixedPointNumbers.jl")
    (synopsis "Fixed point types for Julia")
    (description "@code{FixedPointNumbers.jl} implements fixed-point number
types for Julia.  A fixed-point number represents a fractional, or
non-integral, number.  In contrast with the more widely known floating-point
numbers, with fixed-point numbers the decimal point doesn't \"float\":
fixed-point numbers are effectively integers that are interpreted as being
scaled by a constant factor.  Consequently, they have a fixed number of
digits (bits) after the decimal (radix) point.")
    (license license:expat)))

(define-public julia-formatting
  (package
    (name "julia-formatting")
    (version "0.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/Formatting.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ma3q9my51rr38bb5712xkc4h3rq0wsfjb4ac6mdh9ywn8rqvrmh"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaIO/Formatting.jl")
    (synopsis "Julia package to provide Python-like formatting support")
    (description "This package offers Python-style general formatting and
c-style numerical formatting.")
    (license license:expat)))

(define-public julia-forwarddiff
  (package
    (name "julia-forwarddiff")
    (version "0.10.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/ForwardDiff.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vb46x8mcn61g1l14qrk22c043khg2ml4q1ci7h4k2v34f2ak5fs"))))
    (build-system julia-build-system)
    (arguments
     ;; XXXX: Unexpected and non-deterministic failures for i686, e.g.,
     ;; Expression: dual_isapprox(FDNUM ^ PRIMAL, exp(PRIMAL * log(FDNUM)))
     ;; ERROR: LoadError: LoadError: There was an error during testing
     ;; Disable as stopgap.
     `(#:tests? ,(not (target-x86-32?))))
    (inputs                             ;required for tests
     (list julia-calculus
           julia-difftests))
    (propagated-inputs
     (list julia-commonsubexpressions
           julia-diffresults
           julia-diffrules
           julia-nanmath
           julia-specialfunctions
           julia-staticarrays))
    (home-page "https://github.com/JuliaDiff/ForwardDiff.jl")
    (synopsis "Methods to take multidimensional derivatives")
    (description "This package implements methods to take derivatives,
gradients, Jacobians, Hessians, and higher-order derivatives of native Julia
functions (or any callable object, really) using forward mode automatic
differentiation (AD).")
    (license license:expat)))

(define-public julia-functionwrappers
  (package
    (name "julia-functionwrappers")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yuyichao/FunctionWrappers.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02jilpjr7px6138dx2w7ixricvfgsxqdk84d9dgviranibhnjcxa"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'adjust-tests
           (lambda _
             (substitute* "test/runtests.jl"
               (("testset \\\"Abstract.*" all)
                (string-append all "return\n"))))))))
    (home-page "https://github.com/yuyichao/FunctionWrappers.jl")
    (synopsis "Type stable and efficient wrapper of arbitrary functions")
    (description "This package provides a type stable and efficient wrapper of
arbitrary functions.")
    (license license:expat)))

(define-public julia-functors
  (package
    (name "julia-functors")
    (version "0.2.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/FluxML/Functors.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "03ry1wn1y2jssq65l29bi6q4ki041aa6gl1nd2w6cgl00k2mrxf3"))))
    (build-system julia-build-system)
    (home-page "https://fluxml.ai/Functors.jl/stable/")
    (synopsis "Design pattern for structures as in machine learning")
    (description "This package provides tools to express a design pattern for
dealing with large/ nested structures, as in machine learning and
optimisation.  For large machine learning models it can be cumbersome or
inefficient to work with parameters as one big, flat vector, and structs help
in managing complexity; but it is also desirable to easily operate over all
parameters at once, e.g. for changing precision or applying an optimiser
update step.")
    (license license:expat)))

(define-public julia-fuzzycompletions
  (package
    (name "julia-fuzzycompletions")
    (version "0.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JunoLab/FuzzyCompletions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "07sv88c472n6w4x7diy952igbcfm1s104ysnnvprld83312siw06"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'skip-failing-test
           (lambda _
             (substitute* "test/runtests.jl"
               ((".*RPLE.*") "")))))))
    (home-page "https://github.com/JunoLab/FuzzyCompletions.jl")
    (synopsis "Fuzzy completion provider for Julia")
    (description
     "FuzzyCompletions provides fuzzy completions for a Julia runtime session.")
    (license license:expat)))

(define-public julia-genericlinearalgebra
  (package
    (name "julia-genericlinearalgebra")
    (version "0.2.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLinearAlgebra/GenericLinearAlgebra.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ndwypa397z3pwzdgc3s9plaqlqf63g3d4px5pvym5psgr6lnm3l"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'adjust-test-suite
           (lambda _
             (substitute* "test/runtests.jl"
               ((".*lapack.*") "")))))))
    (native-inputs
     (list julia-quaternions))
    (home-page "https://github.com/JuliaLinearAlgebra/GenericLinearAlgebra.jl")
    (synopsis "Generic numerical linear algebra")
    (description "The purpose of this package is partly to extend linear algebra
functionality in base to cover generic element types, e.g. @code{BigFloat} and
@code{Quaternion}, and partly to be a place to experiment with fast linear
algebra routines written in Julia (except for optimized BLAS).")
    (license license:expat)))

(define-public julia-genericschur
  (package
    (name "julia-genericschur")
    (version "0.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/RalphAS/GenericSchur.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "12x6lxzxm91y3k6s9dam46dq5hrby5sr0gy0fdfnp0xhjzdy2j0d"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'adjust-test-suite
           (lambda _
             (substitute* "test/complex.jl"
               ;; expected Array{Int32,1}, got a value of type Array{Int64,1}
               (("A = _example") "#A = _example")
               (("schurtest\\(A,20\\)") ""))
             (substitute* "test/runtests.jl"
               ;; Test errors relating to liblapack.so
               ((".*complex\\.jl.*") "")
               ((".*real\\.jl.*") "")
               ;; GenericSVD is deprecated upstream
               ((".*gordschur\\.jl.*") "")))))))
    (home-page "https://github.com/RalphAS/GenericSchur.jl")
    (synopsis "Schur decomposition of matrices with generic element types")
    (description "The Schur decomposition is the workhorse for eigensystem
analysis of dense matrices.  The diagonal eigen-decomposition of normal
(especially Hermitian) matrices is an important special case, but for non-normal
matrices the Schur form is often more useful.")
    (license license:expat)))

(define-public julia-geometrybasics
  (package
    (name "julia-geometrybasics")
    (version "0.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGeometry/GeometryBasics.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "057j3hjpli3q5b98cqkpi4p10x2k9pyksrz62hjmv1kb5qzdvhsj"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'remove-earcut
           (lambda _
             (substitute* '("Project.toml"
                            "src/GeometryBasics.jl")
               ((".*EarCut.*") ""))))
         (add-after 'link-depot 'skip-incompatible-test
           (lambda _
             (substitute* "test/runtests.jl"
               (("@testset.*MetaT and heterogeneous data.*" all)
                (string-append all "return\n")))))
         ,@(if (target-64bit?)
             '()
             '((add-after 'unpack 'fix-tests-int32-i686
                 (lambda _
                   (substitute* "test/runtests.jl"
                     (("Int64") "Int32")))))))))
    (propagated-inputs
     (list julia-itertools
           julia-staticarrays
           julia-structarrays
           julia-tables))
    (native-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/JuliaGeometry/GeometryBasics.jl")
    (synopsis "Basic Geometry Types")
    (description "This package aims to offer a standard set of Geometry types,
which easily work with metadata, query frameworks on geometries and different
memory layouts.  The aim is to create a solid basis for Graphics/Plotting,
finite elements analysis, Geo applications, and general geometry manipulations
- while offering a Julian API, that still allows performant C-interop.")
    (license license:expat)))

(define-public julia-gr
  (package
    (name "julia-gr")
    (version "0.58.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jheinen/GR.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "18zxa1w2wmrf44c5l10qbh99zjdp7h94gxlymh47cf5kj5fc4xmx"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-gr-jll))
    (home-page "https://github.com/jheinen/GR.jl")
    (synopsis "Plotting for Julia based on GR")
    (description "This module provides a Julia interface to GR, a framework for
visualisation applications.")
    (license license:expat)))

(define-public julia-graphics
  (package
    (name "julia-graphics")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/Graphics.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "10h1s09v7qkvrjr6l678zamb1p248n8jv4rrwkf8g7d2bpfz9amn"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-colors
           julia-nanmath))
    (home-page "https://github.com/JuliaGraphics/Graphics.jl")
    (synopsis "Base graphics in Julia")
    (description "@code{Graphics.jl} is an abstraction layer for graphical
operations in Julia.")
    (license license:expat)))

(define-public julia-gumbo
  (package
    (name "julia-gumbo")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaWeb/Gumbo.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g22dv3v7caakspv3pdahnqn937fzzsg9y87rj72hid9g8lxl1gm"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-abstracttrees
           julia-gumbo-jll))
    (home-page "https://github.com/JuliaWeb/Gumbo.jl")
    (synopsis "Julia wrapper around Google's gumbo C library for parsing HTML")
    (description "@code{Gumbo.jl} is a Julia wrapper around Google's gumbo
library for parsing HTML.")
    (license license:expat)))

(define-public julia-http
  (package
    (name "julia-http")
    (version "0.9.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaWeb/HTTP.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jsyk3mhnwj4h19cxclx26igdqdrw51fd3k1hgav0nm67dy4cxyk"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'disable-network-tests
           (lambda _
             (substitute* "test/runtests.jl"
               (("\"async.jl") "# \"async.jl")
               (("\"client.jl") "# \"client.jl"))
             (substitute* "test/aws4.jl"
               (("@testset.*HTTP.request with AWS authentication.*" all)
                (string-append all "return\n")))
             (substitute* "test/insert_layers.jl"
               (("@testset.*Inserted final layer runs handler.*" all)
                (string-append all "return\n")))
             (substitute* "test/multipart.jl"
               (("@testset \"Setting of Content-Type.*" all)
                (string-append all "return\n"))
               (("@testset \"Deprecation of .*" all)
                (string-append all "return\n")))
             (substitute* "test/websockets.jl"
               (("@testset.*External Host.*" all)
                (string-append all "return\n")))
             (substitute* "test/messages.jl"
               (("@testset.*Read methods.*" all)
                (string-append all "return\n"))
               (("@testset.*Body - .*" all)
                (string-append all "return\n"))
               (("@testset.*Write to file.*" all)
                (string-append all "return\n")))
             #t)))))
    (propagated-inputs
     (list julia-inifile
           julia-mbedtls
           julia-uris))
    ;; required for tests
    (inputs
     (list julia-json
           julia-bufferedstreams))
    (home-page "https://juliaweb.github.io/HTTP.jl/")
    (synopsis "HTTP support for Julia")
    (description "@code{HTTP.jl} is a Julia library for HTTP Messages,
implementing both a client and a server.")
    (license license:expat)))

(define-public julia-identityranges
  (package
    (name "julia-identityranges")
    (version "0.3.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/IdentityRanges.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0jvl4xn8f8k70sn473li5q62wbiycl5qi25b5k456h3a0j1lbiml"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/JuliaArrays/IdentityRanges.jl")
    (synopsis "Ranges that preserve indices of views")
    (description "@code{IdentityRanges} are Julia-language a helper type for
creating \"views\" of arrays.  They are a custom type of AbstractUnitRange that
makes it easy to preserve the indices of array views.  The key property of an
@code{IdentityRange r} is that @code{r[i] == i} (hence the name of the
type/package), and that they support arbitrary start/stop indices (i.e., not
just starting at 1).")
    (license license:expat)))

(define-public julia-ifelse
  (package
    (name "julia-ifelse")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sciml/ifelse.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wrw842r8708fryf2ihp9mkmdrg27saa9nix2c31vs995k2fgr9w"))))
    (build-system julia-build-system)
    (home-page "https://github.com/sciml/ifelse.jl")
    (synopsis "Function form of the if-else conditional statement")
    (description "This package provides a convenient function form of the
conditional ifelse.  It is similar to @code{Core.ifelse} but it is extendable.")
    (license license:expat)))

(define-public julia-imageaxes
  (package
    (name "julia-imageaxes")
    (version "0.6.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageAxes.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15zqxani1jjh8849s7rdps6b6prqdwv8yxx893y536vkpk7i07qd"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-axisarrays
           julia-imagecore
           julia-reexport
           julia-simpletraits))
    (native-inputs
     (list julia-unitful))
    (home-page "https://github.com/JuliaImages/ImageAxes.jl")
    (synopsis "Julia package for giving \"meaning\" to the axes of an image")
    (description "This small package supports the representation of images as
@code{AxisArrays} to endow the axes with \"meaning,\" and makes programming with
such arrays easy via traits.")
    (license license:expat)))

(define-public julia-imagebase
  (package
    (name "julia-imagebase")
    (version "0.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1n63f2zs6ail9pcl7rzgv3l0z8v1idjsaza3zgvgy7iacxsdpcj2"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with ImageMagick.jl.
    (propagated-inputs
     (list julia-imagecore
           julia-reexport))
    ;(native-inputs
    ; `(("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-offsetarrays" ,julia-offsetarrays)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://github.com/JuliaImages/ImageBase.jl")
    (synopsis "Wrapper package around ImageCore")
    (description "This is a twin package to @code{ImageCore} with functions that
are used among many of the packages in JuliaImages.  The main purpose of this
package is to reduce unnecessary compilation overhead from external
dependencies.")
    (license license:expat)))

(define-public julia-imagecore
  (package
    (name "julia-imagecore")
    (version "0.9.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageCore.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0h9m3pl3wic1jrgaqkdifz24cya5vxd3m6qdmm37pxg2y2ii2vcq"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with ImageMagick.jl.
    (propagated-inputs
     (list julia-abstractffts
           julia-colors
           julia-colorvectorspace
           julia-fixedpointnumbers
           julia-graphics
           julia-mappedarrays
           julia-mosaicviews
           julia-offsetarrays
           julia-paddedviews
           julia-reexport))
    ;(native-inputs
    ; `(("julia-aqua" ,julia-aqua)
    ;   ("julia-colorvectorspace" ,julia-colorvectorspace)
    ;   ("julia-documenter" ,julia-documenter)
    ;   ("julia-fftw" ,julia-fftw)
    ;   ("julia-imageinterminal" ,julia-imageinterminal)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-referencetests" ,julia-referencetests)
    ;   ("julia-statistics" ,julia-statistics)))
    (home-page "https://github.com/JuliaImages/ImageCore.jl")
    (synopsis "Julia types for representing images")
    (description "@code{ImageCore} is the lowest-level component of the system
of packages designed to support image processing and computer vision.")
    (license license:expat)))

(define-public julia-imageinterminal
  (package
    (name "julia-imageinterminal")
    (version "0.4.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageInTerminal.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0bbpzi7bv8jdiggq1wmcn67vnf96qagvwg0fk95s125wy5980xsl"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with ReferenceTests.jl.
    (propagated-inputs
     (list julia-crayons
           julia-imagebase
           julia-imagecore
           julia-requires))
    ;(native-inputs
    ; `(("julia-coordinatetransformations" ,julia-coordinatetransformations)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-imagetransformations" ,julia-imagetransformations)
    ;   ("julia-offsetarrays" ,julia-offsetarrays)
    ;   ("julia-referencetests" ,julia-referencetests)
    ;   ("julia-rotations" ,julia-rotations)
    ;   ("julia-sparsearrays" ,julia-sparsearrays)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://github.com/JuliaImages/ImageInTerminal.jl")
    (synopsis "Julia package for displaying images in the terminal")
    (description "@code{ImageInTerminal.jl} is a drop-in package that once
imported changes a how a single @code{Colorant} and whole @code{Colorant} arrays
(i.e. Images) are displayed in the interactive REPL.  The displayed images will
be downscaled to fit into the size of your active terminal session.")
    (license license:expat)))

(define-public julia-imagemagick
  (package
    (name "julia-imagemagick")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/ImageMagick.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05vzv4jsj3l9pv6yrix28hlw7wnag0mqdfjwv8shn4x71hcfxl1p"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'skip-failing-test
           (lambda _
             ;; These tests try to download from the imagemagick.org
             (substitute* "test/runtests.jl"
               ((".*readremote\\.jl.*") ""))
             ;; Tests with the color gray are hard.
             (substitute* "test/constructed_images.jl"
               (("test (b == aa)" _ test) (string-append "test_nowarn " test))
               (("test (B == map)" _ test) (string-append "test_nowarn " test)))
             #t)))))
    (propagated-inputs
     (list julia-fileio
           julia-imagecore
           julia-imagemagick-jll))
    (native-inputs
     (list julia-colors
           julia-colorvectorspace
           julia-imagemetadata
           julia-imageshow
           julia-imagetransformations
           julia-indirectarrays
           julia-offsetarrays
           julia-zipfile))
    (home-page "https://github.com/JuliaIO/ImageMagick.jl")
    (synopsis "Thin wrapper for ImageMagick")
    (description "This package provides a wrapper around ImageMagick version 6.
It was split off from @code{Images.jl} to make image I/O more modular.")
    (license license:expat)))

(define-public julia-imagemetadata
  (package
    (name "julia-imagemetadata")
    (version "0.9.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageMetadata.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0iv154ms370xgcr56bwsjl13iwmy671cbxjl9ld5yfj85pclcwi1"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-axisarrays
           julia-imageaxes
           julia-imagecore
           julia-indirectarrays))
    (native-inputs
     (list julia-offsetarrays
           julia-simpletraits
           julia-unitful))
    (home-page "https://github.com/JuliaImages/ImageMetadata.jl")
    (synopsis "Julia package for images having metadata")
    (description "@code{ImageMetadata} is a simple package providing utilities
for working with images that have metadata attached.  For example, you might
want to associate an image with the date on which the picture was taken, or an
MRI scan with patient data, or an astronomical image with sky coordinates and
information about the detector used to acquire the image.")
    (license license:expat)))

(define-public julia-imageshow
  (package
    (name "julia-imageshow")
    (version "0.3.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageShow.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00wq3ab8y6nyhxwc5lpz9dnslsmcr1vg3cjdkh7wb7k6a8bw98mh"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; cycle with ImageMagick.jl.
    (propagated-inputs
     (list julia-fileio
           julia-imagebase
           julia-imagecore
           julia-offsetarrays
           julia-stackviews))
    ;(native-inputs
    ; `(("julia-imagedistances" ,julia-imagedistances)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-suppressor" ,julia-suppressor)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://github.com/JuliaImages/ImageShow.jl")
    (synopsis
     "Inline graphical display of images in Julia graphical environments")
    (description "This package implements image @code{show} methods suitable
for graphical platforms such as IJulia.  It is intended to provide convenient
inline presentation of greyscale or color images.")
    (license license:expat)))

(define-public julia-imagetransformations
  (package
    (name "julia-imagetransformations")
    (version "0.8.12")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageTransformations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0i8gw68hljshsy9wdl5mrpbb31irhmayqyglsxi7jwm88iy9pxhm"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with ImageMagick.jl.
    (propagated-inputs
     (list julia-axisalgorithms
           julia-colorvectorspace
           julia-coordinatetransformations
           julia-identityranges
           julia-imagecore
           julia-interpolations
           julia-offsetarrays
           julia-rotations
           julia-staticarrays))
    ;(native-inputs
    ; `(("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-referencetests" ,julia-referencetests)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://github.com/JuliaImages/ImageTransformations.jl")
    (synopsis "Geometric transformations on images for Julia")
    (description "This package provides support for image resizing, image
rotation, and other spatial transformations of arrays.")
    (license license:expat)))

(define-public julia-indexablebitvectors
  (package
    (name "julia-indexablebitvectors")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/BioJulia/IndexableBitVectors.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1khaycydwa31sxwvrrvvlylpzdb77kkxfmb8cax3i22ix0c2nmlc"))))
    (build-system julia-build-system)
    ;; Package without Project.toml
    (arguments
     '(#:julia-package-name "IndexableBitVectors"
       #:julia-package-uuid "1cb3b9ac-1ffd-5777-9e6b-a3d42300664d"))
    (home-page "https://github.com/BioJulia/IndexableBitVectors.jl")
    (synopsis "Bit vectors operations with extremely fast speed")
    (description "This package exports following operations over bit vectors
with extremely fast speed while keeping extra memory usage small:
@itemize
@item @code{getindex(bv::IndexableBitVectors, i::Integer)}: @code{i}-th
element of @code{bv}
@item @code{rank(b::Bool, bv::AbstractIndexableBitVector, i::Integer)}: the
number of occurrences of bit @code{b} in @code{bv[1:i]}
@item @code{select(b::Bool, bv::AbstractIndexableBitVector, i::Integer)}: the
index of i-th occurrence of @code{b} in @code{bv}.
@end itemize
and other shortcuts or types.")
    (license license:expat)))

(define-public julia-indexing
  (package
    (name "julia-indexing")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/andyferris/Indexing.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1s7bz5aaj9sx753pcaixq83jgbk33adxgybpinjgzb9lzdv1ddgx"))))
    (build-system julia-build-system)
    (home-page "https://github.com/andyferris/Indexing.jl")
    (synopsis "Generalized indexing for Julia")
    (description "This package defines functions for getting multiple indices
out of dictionaries, tuples, etc, extending this ability beyond
@code{AbstractArray}.")
    (license license:expat)))

(define-public julia-indirectarrays
  (package
    (name "julia-indirectarrays")
    (version "0.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/IndirectArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0l0jq0jnr9z3k431ni82xycq7mqapgxrbrx4yyk6lycvi41ipm4s"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-colors
           julia-fixedpointnumbers
           julia-mappedarrays))
    (home-page "https://github.com/JuliaArrays/IndirectArrays.jl")
    (synopsis "Julia implementation of indexed arrays")
    (description "An @code{IndirectArray} is one that encodes data using a
combination of an @code{index} and a @code{value} table.  Each element is
assigned its own index, which is used to retrieve the value from the
@code{value} table.  Among other uses, @code{IndirectArrays} can represent
indexed images, sometimes called \"colormap images\" or \"paletted images.\"")
    (license license:expat)))

(define-public julia-infinity
  (package
    (name "julia-infinity")
    (version "0.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cjdoris/Infinity.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1941lwvrdjnrynigzixxin3chpg1ba6xplvcwc89x0f6z658hwmm"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'remove-timezones.jl
           (lambda _
             (substitute* "test/runtests.jl"
               (("using TimeZones.*") "")
               ((".*infextendedtime.*") ""))
             #t)))))
    (propagated-inputs
     (list julia-requires))
    (native-inputs
     (list julia-compat))
    (home-page "https://docs.juliahub.com/Infinity/")
    (synopsis "Representation of infinity in Julia")
    (description "This package provides representations for infinity and
negative infinity in Julia.")
    (license license:expat)))

(define-public julia-inifile
  (package
    (name "julia-inifile")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaIO/IniFile.jl")
             (commit "8ba59958495fa276d6489d2c3903e765d75e0bc0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11h6f99jpbg729lplw841m68jprka7q3n8yw390bndlmcdsjabpd"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaIO/IniFile.jl")
    (synopsis "Reading Windows-style INI files")
    (description "This is a Julia package that defines an IniFile type that
interfaces with @file{.ini} files.")
    (license license:expat)))

(define-public julia-interpolations
  (package
    (name "julia-interpolations")
    (version "0.13.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Interpolations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1236c20k388qlh7k74mhf7hkbn0vf7ss8b1rgh1a6aj0234ayfnc"))))
    (build-system julia-build-system)
    (arguments
     `(#:parallel-tests? #f
       ;; XXXX: Unexpected failures for i686, e.g.,
       ;; Got exception outside of a @test
       ;; OverflowError: 96908232 * 106943408 overflowed for type Int32
       ;; Disable as stopgap.
       #:tests? ,(not (target-x86-32?))))
    (propagated-inputs
     (list julia-axisalgorithms
           julia-offsetarrays
           julia-ratios
           julia-requires
           julia-staticarrays
           julia-woodburymatrices))
    (native-inputs
     (list julia-dualnumbers
           julia-forwarddiff
           julia-offsetarrays
           julia-unitful julia-zygote))
    (home-page "https://github.com/JuliaMath/Interpolations.jl")
    (synopsis "Continuous interpolation of discrete datasets")
    (description "This package implements a variety of interpolation schemes for
the Julia language.  It has the goals of ease-of-use, broad algorithmic support,
and exceptional performance.")
    (license license:expat)))

(define-public julia-intervalsets
  (package
    (name "julia-intervalsets")
    (version "0.5.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/IntervalSets.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0gsz89cd3iygbl5qr389k9vwpg7w1nk0s90g25nsmk34y9hifxag"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       ,@(if (target-x86-32?)
           '((modify-phases %standard-phases
               (add-after 'unpack 'remove-failing-test-i686
                 (lambda _
                   (substitute* "test/runtests.jl"
                     ;; For some reason, the output is correct but the test
                     ;; is considered as failed:
                     ;; Expression: duration(ClosedInterval(A, B)) ≡ 60
                     ;; Evaluated: 60 ≡ 60
                     (("@test duration\\(ClosedInterval")
                      "@test_broken duration(ClosedInterval"))))))
           '(%standard-phases))))
    (propagated-inputs
     (list julia-ellipsisnotation))
    (native-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/JuliaMath/IntervalSets.jl")
    (synopsis "Interval Sets for Julia")
    (description "This package is intended to implement a \"minimal\" foundation
for intervals upon which other packages might build.  In particular, we
encourage type-piracy for the reason that only one interval package can
unambiguously define the @code{..} and @code{±} operators.")
    (license license:expat)))

(define-public julia-intervaltrees
  ;; Last upstream release on May 2020 and this last release does not contain
  ;; the file Project.toml.
  (let ((commit "e37edab61568d08141a3e9c25ec55caac21e5aa5")
        (revision "1"))
    (package
      (name "julia-intervaltrees")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BioJulia/IntervalTrees.jl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "01x48a5zrx0833s1kjhf0ml4x9xz8xja4ymran770akmf6968yl9"))))
      (build-system julia-build-system)
      (home-page "https://github.com/BioJulia/IntervalTrees.jl")
      (synopsis "Interval Trees for Julia")
      (description "This package provides an implementation of an associative
container mapping @code{(K,V)} pairs via the type @code{IntervalTree{K, V}}.
The type @code{K} may be any ordered type.")
      (license license:expat))))

(define-public julia-invertedindices
  (package
    (name "julia-invertedindices")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mbauman/InvertedIndices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1179z20yxnkyziip7gn26wr1g3k3ssl1ci7pig3khc900f62di46"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/mbauman/InvertedIndices.jl")
    (synopsis "Index type that allows for inverted selections")
    (description "This package just exports one type: the @code{InvertedIndex},
or @code{Not} for short.  It can wrap any supported index type and may be used
as an index into any @code{AbstractArray} subtype, including OffsetArrays.")
    (license license:expat)))

(define-public julia-iocapture
  (package
    (name "julia-iocapture")
    (version "0.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/IOCapture.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0v76wbvg80g9nx0rjbcna82zk61krly1y9yhyfrjv2pf7mcr4idb"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaDocs/IOCapture.jl")
    (synopsis "Capture standard output and error streams")
    (description "This package provides the @code{IOCapture.capture(f)}
function, which captures the standard output and standard error, and returns it
as a string together with the return value.")
    (license license:expat)))

(define-public julia-irtools
  (package
    (name "julia-irtools")
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FluxML/IRTools.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11334fcg2slpwcj0raxf457brhf7pxglgxc6cy8q58ggrpxqfqql"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-macrotools))
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/FluxML/IRTools.jl")
    (synopsis "Simple and flexible IR format")
    (description "This package provides a simple and flexible IR format,
expressive enough to work with both lowered and typed Julia code, as well as
external IRs.  It can be used with Julia metaprogramming tools such as
Cassette.")
    (license license:expat)))

(define-public julia-iteratorinterfaceextensions
  (package
    (name "julia-iteratorinterfaceextensions")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/IteratorInterfaceExtensions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1slpay1dhja8f9gy6z7b3psgvgcknn963dvfqqakvg1grk9ppa09"))))
    (build-system julia-build-system)
    (home-page "https://github.com/queryverse/IteratorInterfaceExtensions.jl")
    (synopsis "Traits for Julia iterators")
    (description "IteratorInterfaceExtensions defines a small number of
extensions to the iterator interface.")
    (license license:expat)))

(define-public julia-itertools
  (package
    (name "julia-itertools")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaCollections/IterTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0haf974kcqj6arv4if97ahs4w3dmvslh6ab3hl57r9s41ic36xdq"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaCollections/IterTools.jl")
    (synopsis "Common functional iterator patterns")
    (description
     "Common functional iterator patterns (formerly @code{Iterators.jl}).")
    (license license:expat)))

(define-public julia-jive
  (package
    (name "julia-jive")
    (version "0.2.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wookay/Jive.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0khwsdh8472jxcfi9lqw14l49sqlbsixql1jb4irnyajxkdjrcsf"))))
    (build-system julia-build-system)
    (home-page "https://github.com/wookay/Jive.jl")
    (synopsis "Julia package to help with writing tests")
    (description "@code{Jive.jl} is a Julia package to help with writing tests.")
    (license license:expat)))

(define-public julia-json
  (package
    (name "julia-json")
    (version "0.21.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaIO/JSON.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f9k613kbknmp4fgjxvjaw4d5sfbx8a5hmcszmp1w9rqfqngjx9m"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-datastructures
           julia-fixedpointnumbers
           julia-parsers
           julia-offsetarrays))
    (home-page "https://github.com/JuliaIO/JSON.jl")
    (synopsis "JSON parsing and printing library for Julia")
    (description "@code{JSON.jl} is a pure Julia module which supports parsing
and printing JSON documents.")
    (license license:expat)))

(define-public julia-json3
  (package
    (name "julia-json3")
    (version "1.9.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/quinnj/JSON3.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "11z5maz7v50wd99id8z7838higza0cllh2amkdkrlskbri3v2f17"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-parsers
           julia-structtypes))
    (home-page "https://github.com/quinnj/JSON3.jl")
    (synopsis "JSON package for Julia")
    (description "This package provides another JSON package for Julia, with a
focus on speed and slick struct mapping.")
    (license license:expat)))

(define-public julia-latexstrings
  (package
    (name "julia-latexstrings")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/stevengj/LaTeXStrings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "117z27krcf8fydgp6mb0pgn75r4gng9qs7v90qb4bqzsry3faadp"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/stevengj/LaTeXStrings.jl")
    (synopsis "Input and display of LaTeX equation strings")
    (description "This is a small package to make it easier to type LaTeX
equations in string literals in the Julia language.")
    (license license:expat)))

(define-public julia-lazyarrays
  (package
    (name "julia-lazyarrays")
    (version "0.22.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/LazyArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "17rhlrmgfvdw8w62pg32ikr9j4xy2ylr7mx7ar0hnpzryv929rp5"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       ,@(if (target-64bit?)
           '(%standard-phases)
           '((modify-phases %standard-phases
               (add-after 'unpack 'fix-tests-int32-i686
                 (lambda _
                   (substitute* "test/multests.jl"
                     (("Int64") "Int32")))))))))
    (propagated-inputs
     (list julia-arraylayouts
           julia-fillarrays
           julia-macrotools
           julia-matrixfactorizations
           julia-staticarrays))
    (native-inputs
     (list julia-tracker))
    (home-page "https://github.com/JuliaArrays/LazyArrays.jl")
    (synopsis "Lazy arrays and linear algebra")
    (description "This package supports lazy analogues of array operations like
@code{vcat}, @code{hcat}, and multiplication.  This helps with the
implementation of matrix-free methods for iterative solvers.")
    (license license:expat)))

(define-public julia-logexpfunctions
  (package
    (name "julia-logexpfunctions")
    (version "0.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/LogExpFunctions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0rvms3mmq8a1viqlyzdgs2ccddcy6j0c677dlb8m5nk6hkiwr16n"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-docstringextensions))
    (native-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/JuliaStats/LogExpFunctions.jl")
    (synopsis "Special functions based on @code{log} and @code{exp}")
    (description "Various special functions based on log and exp moved from
@code{StatsFuns.jl} into a separate package, to minimize dependencies.  These
functions only use native Julia code, so there is no need to depend on
@code{librmath} or similar libraries.")
    (license license:expat)))

(define-public julia-macrotools
  (package
    (name "julia-macrotools")
    (version "0.5.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FluxML/MacroTools.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k4z2hyasd9cwxf4l61zk3w4ajs44k69wx6z1ghdn8f5p8xy217f"))))
    (build-system julia-build-system)
    (home-page "https://fluxml.ai/MacroTools.jl")
    (synopsis "Tools for working with Julia code and expressions")
    (description "This library provides tools for working with Julia code and
expressions.  This includes a template-matching system and code-walking tools
that let you do deep transformations of code.")
    (license license:expat)))

(define-public julia-mappedarrays
  (package
    (name "julia-mappedarrays")
    (version "0.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/MappedArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0l5adird8m1cmnsxwhzi5hcr7q9bm1rf7a6018zc7kcn2yxdshy3"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       ,@(if (target-64bit?)
           '(%standard-phases)
           '((modify-phases %standard-phases
               (add-after 'unpack 'fix-tests-int32-i686
                 (lambda _
                   (substitute* "test/runtests.jl"
                     (("Int64") "Int32")))))))))
    (propagated-inputs
     (list julia-fixedpointnumbers))
    (native-inputs
     (list julia-colortypes
           julia-fixedpointnumbers
           julia-offsetarrays))
    (home-page "https://github.com/JuliaArrays/MappedArrays.jl")
    (synopsis "Lazy in-place transformations of arrays")
    (description "This package implements \"lazy\" in-place elementwise
transformations of arrays for the Julia programming language.  Explicitly, it
provides a \"view\" M of an array A so that @code{M[i] = f(A[i])} for a
specified (but arbitrary) function f, without ever having to compute M
explicitly (in the sense of allocating storage for M).  The name of the package
comes from the fact that @code{M == map(f, A)}.")
    (license license:expat)))

(define-public julia-matrixfactorizations
  (package
    (name "julia-matrixfactorizations")
    (version "0.8.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/MatrixFactorizations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15zvcv2l4iqmjpnqjyx2kry7a85p652nbjy9pj3wq0piksqcz4jb"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'skip-failing-test
           (lambda _
             ;; Tests with math functions are hard.
             (substitute* "test/test_ul.jl"
               (("@test @inferred\\(logdet") "@test @test_nowarn(logdet")
               ;; Also skip the REPL test.
               (("test String") "test_nowarn String"))
             #t)))))
    (propagated-inputs
     (list julia-arraylayouts))
    (home-page "https://github.com/JuliaMatrices/MatrixFactorizations.jl")
    (synopsis "Julia package to contain non-standard matrix factorizations")
    (description "A Julia package to contain non-standard matrix factorizations.
At the moment it implements the QL, RQ, and UL factorizations, a combined
Cholesky factorization with inverse, and polar decompositions.  In the future it
may include other factorizations such as the LQ factorization.")
    (license license:expat)))

(define-public julia-mbedtls
  (package
    (name "julia-mbedtls")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaLang/MbedTLS.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zjzf2r57l24n3k0gcqkvx3izwn5827iv9ak0lqix0aa5967wvfb"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'disable-network-tests
           ;; Tries to connect to httpbin.org
           (lambda _
             (substitute* "test/runtests.jl"
               (("testhost =") "return #"))
             #t)))))
    (propagated-inputs
     (list julia-mbedtls-jll))
    (home-page "https://github.com/JuliaLang/MbedTLS.jl")
    (synopsis "Apache's mbed TLS library wrapper")
    (description "@code{MbedTLS.jl} provides a wrapper around the @code{mbed
TLS} and cryptography C library for Julia.")
    (license license:expat)))

(define-public julia-measurements
  (package
    (name "julia-measurements")
    (version "2.6.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPhysics/Measurements.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05p3f0gr4sv4maq8cix5fi8ldq0zagswqsd43xn6fhy046f936mz"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-calculus
           julia-recipesbase
           julia-requires))
    (native-inputs
     (list julia-quadgk
           julia-specialfunctions
           julia-unitful))
    (home-page "https://juliaphysics.github.io/Measurements.jl/stable/")
    (synopsis "Error propagation calculator and library")
    (description "@code{Measurements.jl} is an error propagation calculator and
library for physical measurements.  It supports real and complex numbers with
uncertainty, arbitrary precision calculations, operations with arrays, and
numerical integration.  The linear error propagation theory is employed to
propagate the errors.")
    (license license:expat)))

(define-public julia-measures
  (package
    (name "julia-measures")
    (version "0.3.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/Measures.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0j34psrdijnqqn9zv0r2sknr1p9q0mmbjvjhmjra37bb5fh2gk8l"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaGraphics/Measures.jl")
    (synopsis "Unified measure and coordinates types")
    (description "This library generalizes and unifies the notion of measures
used in Compose, Compose3D, and Escher.  It allows building up and representing
expressions involving differing types of units that are then evaluated,
resolving them into absolute units.")
    (license license:expat)))

(define-public julia-media
  (package
    (name "julia-media")
    (version "0.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JunoLab/Media.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05jq9j3vs8yfj2lwz3sh1vk5rha06xdcikp9s2q3dn316vryy6di"))))
    (build-system julia-build-system)
    ;; Package without Project.toml
    (arguments
     '(#:julia-package-name "Media"
       #:julia-package-uuid "e89f7d12-3494-54d1-8411-f7d8b9ae1f27"))
    (propagated-inputs
     (list julia-macrotools))
    (home-page "https://github.com/JunoLab/Media.jl")
    (synopsis "Unified measure and coordinates types")
    (description "This package provides a display system which enables the
user handle multiple input/output devices and decide what media types get
displayed where.")
    (license license:expat)))

(define-public julia-millboard
  (package
    (name "julia-millboard")
    (version "0.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wookay/Millboard.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k9jqgp285qhckldvvsmfk6s69dcr8s74m2fijgm2vxjj2gqjs1n"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-jive))
    (home-page "https://github.com/wookay/Millboard.jl")
    (synopsis "Displaying data in tables for Julia")
    (description
     "@code{Millboard.jl} provides a library for getting data in a tablized
format to arrange into rows and columns of cells.")
    (license license:expat)))

(define-public julia-missings
  (package
    (name "julia-missings")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/Missings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1k481rm5lahmjyh34j177d4n10svgr0wm7ps5m3ar3xx6nr26ad5"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dataapi))
    (home-page "https://github.com/JuliaData/Missings.jl")
    (synopsis "Additional missing value support for Julia")
    (description "This package provides additional functionality for working
with @code{missing} values in Julia.")
    (license license:expat)))

(define-public julia-mlstyle
  (package
    (name "julia-mlstyle")
    (version "0.4.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/thautwarm/MLStyle.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0h1cd7cr4c4cnpqyj3180113gdbvcc047lqphp8a8gq5smp3c059"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-datastructures))
    (home-page "https://thautwarm.github.io/MLStyle.jl/latest/")
    (synopsis "Julia functional programming infrastructures")
    (description "This package provides consistent and extensible functional
programming infrastructures, and metaprogramming facilities.")
    (license license:expat)))

(define-public julia-mocking
  (package
    (name "julia-mocking")
    (version "0.7.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/invenia/Mocking.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1cg2is83bjmrchmmxcgx57k8c9b9vlamrw38v4fdhbb6d4six5cg"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-compat
           julia-exprtools))
    (home-page "https://github.com/invenia/Mocking.jl")
    (synopsis "Overload Julia function calls")
    (description "The purpose of this package is to allow Julia function calls
to be temporarily overloaded for the purpose of testing.")
    (license license:expat)))

(define-public julia-mosaicviews
  (package
    (name "julia-mosaicviews")
    (version "0.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/MosaicViews.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "04fgxghyb7n2ji76xkb1r1fjhzsdbgmp5wsfyyn3yjcsdqbyp8pz"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with ImageCore.jl
    (propagated-inputs
     (list julia-mappedarrays
           julia-paddedviews
           julia-stackviews))
    ;(native-inputs
    ; `(("julia-colorvectorspace" ,julia-colorvectorspace)
    ;   ("julia-imagecore" ,julia-imagecore)))
    (home-page "https://github.com/JuliaArrays/MosaicViews.jl")
    (synopsis
     "Lazily view a 3D or 4D array as an expanded 2D array as a matrix of slices")
    (description "When visualizing images, it is not uncommon to provide a 2D
view of different image sources.  For example, comparing multiple images of
different sizes, getting a preview of machine learning dataset.  This package
aims to provide easy-to-use tools for such tasks.")
    (license license:expat)))

(define-public julia-msgpack
  (package
    (name "julia-msgpack")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/MsgPack.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1layiqjf9si38pfdcszppgcy4zbfqgld7jlw8x645sm9b17b19fg"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaIO/MsgPack.jl")
    (synopsis "Julia MsgPack implementation")
    (description "@code{MsgPack.jl} is a MessagePack implementation in pure
Julia, with type-driven, overloadable packing/unpacking functionality.")
    (license license:expat)))

(define-public julia-mutablearithmetics
  (package
    (name "julia-mutablearithmetics")
    (version "0.2.20")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jump-dev/MutableArithmetics.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1isyj8h4nx96cr6892d154v8pw1nhr7mjyz5bd6ffr2mkzb2bq4f"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       ,@(if (target-x86-32?)
           '((modify-phases %standard-phases
               (add-after 'unpack 'remove-failing-test-i686
                 (lambda _
                   (substitute* "test/utilities.jl"
                     ;; Non-deterministic returned value, e.g.,
                     ;;    Expression: n == @allocated(f())
                     ;;    Evaluated: 240 == 120
                     ;; and for some other values:
                     ;;    Got correct result, please change to @test
                     ;; so @test_broken is not enough.
                     (("@test n == @allocated f\\(\\)")
                      " "))))))
           '(%standard-phases))))
    (propagated-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/jump-dev/MutableArithmetics.jl")
    (synopsis "Interface for arithmetics on mutable types in Julia")
    (description "MutableArithmetics is a Julia package which allows:
@itemize
@item mutable types to implement mutable arithmetics
@item algorithms that could exploit mutable arithmetics to exploit them while
still being completely generic
@end itemize")
    (license license:mpl2.0)))

(define-public julia-nanmath
  (package
    (name "julia-nanmath")
    (version "0.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mlubin/NaNMath.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hczhz00qj99w63vp627kwk02l2sr2qmzc2rkwwkdwvzy670p25q"))))
    (build-system julia-build-system)
    (home-page "https://github.com/mlubin/NaNMath.jl")
    (synopsis "Implementations of basic math functions")
    (description "Implementations of basic math functions which return
@code{NaN} instead of throwing a @code{DomainError}.")
    (license license:expat)))

(define-public julia-nlsolversbase
  (package
    (name "julia-nlsolversbase")
    (version "7.8.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaNLSolvers/NLSolversBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0n8qh5a2ghjx1j70zxn0hmh8gzpa46kmjg8di879y9974bfk0f98"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-diffresults
           julia-finitediff
           julia-forwarddiff))
    (native-inputs
     (list julia-optimtestproblems
           julia-recursivearraytools))
    (home-page "https://github.com/JuliaNLSolvers/NLSolversBase.jl")
    (synopsis "Optimization and equation solver software in JuliaNLSolvers")
    (description "This package aims at establishing common ground for Optim.jl,
LineSearches.jl, and NLsolve.jl.  The common ground is mainly the types used to
hold objective related callables, information about the objectives, and an
interface to interact with these types.")
    (license license:expat)))

(define-public julia-nnlib
  (package
    (name "julia-nnlib")
    (version "0.7.29")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/FluxML/NNlib.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "16vn5w5274kcywh1xp0zqjk5q10xrk125aznz5av6wifwrvghk8s"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'skip-cuda-tests
           (lambda _
             (substitute* "test/runtests.jl"
               (("using CUDA") "")
               (("&& CUDA\\.functional\\(\\)") ""))
             (setenv "NNLIB_TEST_CUDA" "false"))))))
    (propagated-inputs
     (list julia-adapt
           julia-chainrulescore
           julia-requires))
    (native-inputs
     (list julia-chainrulestestutils
           julia-stablerngs
           julia-zygote))
    (home-page "https://github.com/FluxML/NNlib.jl")
    (synopsis "Neural Network primitives with multiple backends")
    (description "This package will provide a library of functions useful for
machine learning, such as softmax, sigmoid, convolutions and pooling.  It
doesn't provide any other \"high-level\" functionality like layers or AD.")
    (license license:expat)))

(define-public julia-optimtestproblems
  (package
    (name "julia-optimtestproblems")
    (version "2.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaNLSolvers/OptimTestProblems.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "10h47x5ws42pkqjccimaz0yxfvz41w0yazq6inamfk4lg5g2g3d9"))))
    (build-system julia-build-system)
    (arguments
     `(#:julia-package-name "OptimTestProblems"
       #:julia-package-uuid "cec144fc-5a64-5bc6-99fb-dde8f63e154c"))
    (home-page "https://github.com/JuliaNLSolvers/OptimTestProblems.jl")
    (synopsis "Collection of optimization test problems")
    (description "The purpose of this package is to provide test problems for
JuliaNLSolvers packages.")
    (license license:expat)))

(define-public julia-orderedcollections
  (package
    (name "julia-orderedcollections")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaCollections/OrderedCollections.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jaxcmvkp8zpqrz101yikdigz90s70i7in5wn8kybwzf0na3lhwf"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaCollections/OrderedCollections.jl")
    (synopsis "Associative containers that preserve insertion order")
    (description "This package implements @code{OrderedDicts} and
@code{OrderedSets}, which are similar to containers in base Julia.  However,
during iteration the @code{Ordered*} containers return items in the order in
which they were added to the collection.")
    (license license:expat)))

(define-public julia-offsetarrays
  (package
    (name "julia-offsetarrays")
    (version "1.10.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaArrays/OffsetArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j5a8ar8yc0j9h87gwfyrcqm23wpyv5yv5gn8vzilpg4vr0fiasc"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-adapt))
    ;; CatIndices depends on OffsetArrays, introducing a recursive dependency
    (arguments '(#:tests? #f))
    (home-page "https://juliaarrays.github.io/OffsetArrays.jl/stable/")
    (synopsis "Fortran-like arrays with arbitrary, zero or negative indices")
    (description "@code{OffsetArrays.jl} provides Julia users with arrays that
have arbitrary indices, similar to those found in some other programming
languages like Fortran.")
    (license license:expat)))

(define-public julia-paddedviews
  (package
    (name "julia-paddedviews")
    (version "0.5.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/PaddedViews.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ran2vj6ahlzib0g77y7g0jhavy3k9s2mqq23ybpgp9z677wf26h"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-offsetarrays))
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/JuliaArrays/PaddedViews.jl")
    (synopsis "Add virtual padding to the edges of an array")
    (description "@code{PaddedViews} provides a simple wrapper type,
@code{PaddedView}, to add \"virtual\" padding to any array without copying data.
Edge values not specified by the array are assigned a @code{fillvalue}.
Multiple arrays may be \"promoted\" to have common indices using the
@code{paddedviews} function.")
    (license license:expat)))

(define-public julia-parameters
  (package
    (name "julia-parameters")
    (version "0.12.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mauro3/Parameters.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0b8lawi7kcws4axfsdf023gyxca15irl648ciyi1kw3wghz3pfi2"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-orderedcollections
           julia-unpack))
    (home-page "https://github.com/mauro3/Parameters.jl")
    (synopsis "Numerical-model parameter helpers")
    (description "This package contains types with default field values, keyword
constructors and (un-)pack macros.  Keyword functions can be slow in Julia,
however, the normal positional constructor is also provided and could be used in
performance critical code.")
    (license license:expat)))

(define-public julia-parsers
  (package
    (name "julia-parsers")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaData/Parsers.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gz3drd5334xrbx2ms33hiifkd0q1in4ywc92xvrkq3xgzdjqjdk"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaData/Parsers.jl")
    (synopsis "Fast parsing machinery for basic types in Julia")
    (description "@code{Parsers.jl} is a collection of type parsers and
utilities for Julia.")
    (license license:expat)))

(define-public julia-pdmats
  (package
    (name "julia-pdmats")
    (version "0.11.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/PDMats.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0bc2gmpd30rkclvxyfnssjllp0pk63h0vvgr8862phm5ia83r8j0"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaStats/PDMats.jl")
    (synopsis
     "Uniform Interface for positive definite matrices of various structures")
    (description "PDMats.jl supports efficient computation on positive definite
matrices of various structures.  In particular, it provides uniform interfaces
to use positive definite matrices of various structures for writing generic
algorithms, while ensuring that the most efficient implementation is used in
actual computation.")
    (license license:expat)))

(define-public julia-plotthemes
  (package
    (name "julia-plotthemes")
    (version "2.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/PlotThemes.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1fd27w9z1vhz0d1bzrs5vcavpb5r5jviyh27d9c4ka37phz4xvmh"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-plotutils
           julia-requires))
    (home-page "https://github.com/JuliaPlots/PlotThemes.jl")
    (synopsis "Themes for the Julia plotting package Plots.jl")
    (description
     "PlotThemes is a package to spice up the plots made with @code{Plots.jl}.")
    (license license:expat)))

(define-public julia-plotutils
  (package
    (name "julia-plotutils")
    (version "1.0.15")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/PlotUtils.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "12aw5gkkcfhpczv2w510k65w1j0hjnh825ihimi223v8plsi5105"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-colors
           julia-colorschemes
           julia-reexport))
    (native-inputs
     (list julia-stablerngs))
    (home-page "https://github.com/JuliaPlots/PlotUtils.jl")
    (synopsis "Helper algorithms for building plotting components")
    (description "This package contains generic helper algorithms for building
plotting components.")
    (license license:expat)))

(define-public julia-pooledarrays
  (package
    (name "julia-pooledarrays")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/PooledArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0qdwvf1p5z6z0q4s4hn85ysd8wq47zy6hlzddc5ijvhk86ccqlrr"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dataapi))
    (home-page "https://github.com/JuliaData/PooledArrays.jl")
    (synopsis "Pooled representation of arrays in Julia")
    (description "This package provides a pooled representation of arrays for
purposes of compression when there are few unique elements.")
    (license license:expat)))

(define-public julia-positivefactorizations
  (package
    (name "julia-positivefactorizations")
    (version "0.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/timholy/PositiveFactorizations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wxy6ak7f3hvibcgc8q88cgkf9zvi649mmjy1zlkx1qk80hgvz23"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-forwarddiff
           julia-reversediff))
    (home-page "https://github.com/timholy/PositiveFactorizations.jl")
    (synopsis "Positive-definite \"approximations\" to matrices")
    (description "@code{PositiveFactorizations} is a package for computing a
positive definite matrix decomposition (factorization) from an arbitrary
symmetric input.  The motivating application is optimization (Newton or
quasi-Newton methods), in which the canonical search direction -H/g (H being the
Hessian and g the gradient) may not be a descent direction if H is not positive
definite.")
    (license license:expat)))

(define-public julia-preferences
  (package
    (name "julia-preferences")
    (version "1.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPackaging/Preferences.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1cail43iqzbi6m9v6981rhz47zf2lcvhs5ds5gdqvc9nx5frghxq"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))        ; Tests try to mkdir /.julia
    (home-page "https://github.com/JuliaPackaging/Preferences.jl")
    (synopsis "Store configuration switches to TOML files")
    (description "The @code{Preferences} package provides an integrated way for
packages to store configuration switches to persistent TOML files, and use those
pieces of information at both run time and compile time in Julia.  This enables
the user to modify the behavior of a package, and have that choice reflected in
everything from run time algorithm choice to code generation at compile time.")
    (license license:expat)))

(define-public julia-prettytables
  (package
    (name "julia-prettytables")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ronisbr/PrettyTables.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1d1sd87kkwbar3l608h0adzws42cwdrmp1idxx7an6mfqcsdrijw"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'skip-color-tests
           (lambda _
             (substitute* "test/text_backend.jl"
               ((".*colors\\.jl.*") ""))
             #t)))))
    (propagated-inputs
     (list julia-crayons
           julia-formatting
           julia-reexport
           julia-tables))
    (home-page "https://github.com/ronisbr/PrettyTables.jl")
    (synopsis "Print data in formatted tables")
    (description "This package has the purpose to print data in matrices in a
human-readable format.")
    (license license:expat)))

(define-public julia-pycall
  (package
    (name "julia-pycall")
    (version "1.92.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPy/PyCall.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1fj5d1ihnhnm0pl4hbx6hcd2bpdyhm8jiaqah2axsbd069j70saf"))))
    (build-system julia-build-system)
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%julia-build-system-modules)
       #:modules ((guix build julia-build-system)
                  (guix build utils)
                  ((guix build python-build-system) #:prefix python:))
       #:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'remove-conda
           (lambda _
             (substitute* "Project.toml"
               ((".*Conda.*") ""))
             (substitute* (list "src/PyCall.jl"
                                "test/runtests.jl")
               (("import Conda") ""))
             (substitute* "deps/depsutils.jl"
               (("Conda.PYTHONDIR") "\"/\""))
             #t))
         (add-after 'link-depot 'set-python
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((python (assoc-ref inputs "python")))
               (setenv "PYCALL_JL_RUNTIME_PYTHON"
                       (string-append python "/bin/python3"))
               (with-output-to-file "deps/deps.jl"
                 (lambda _
                   (format #t
                           "const python = \"~a/bin/python3\"~@
                           const pyprogramname = \"~a/bin/python3\"~@
                           const libpython = \"~a/lib/libpython~a.so.1.0\"~@
                           const PYTHONHOME = \"~a\"~@
                           const pyversion_build = v\"~a\"~@
                           const conda = false~%"
                           python
                           python
                           python
                           (python:python-version python)
                           python
                           ,(package-version python))))
               #t)))
         (add-before 'check 'pre-check
           (lambda _
             (setenv "CI" "true")
             (setenv "JULIA_PKGEVAL" "true")
             #t)))))
    (propagated-inputs
     (list julia-macrotools
           julia-versionparsing))
    (inputs
     (list python))
    (native-inputs
     (list python-numpy))
    (home-page "https://github.com/JuliaPy/PyCall.jl")
    (synopsis "Call Python functions from the Julia language")
    (description "This package provides the ability to directly call and fully
interoperate with Python from the Julia language.  You can import arbitrary
Python modules from Julia, call Python functions (with automatic conversion of
types between Julia and Python), define Python classes from Julia methods, and
share large data structures between Julia and Python without copying them.")
    (license license:expat)))

(define-public julia-pyplot
  (package
    (name "julia-pyplot")
    (version "2.10.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPy/PyPlot.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "007zs0imfgs69f30pp2a3rc93kl0qiq7qjx6ig35z4wzkmps4skd"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-colors
           julia-latexstrings
           julia-pycall
           julia-versionparsing
           ;; python-matplotlib is expected to be available at runtime.
           python-matplotlib))
    (home-page "https://github.com/JuliaPy/PyPlot.jl")
    (synopsis "Plotting for Julia based on matplotlib.pyplot")
    (description "This package provides a Julia interface to the Matplotlib
plotting library from Python, and specifically to the @code{matplotlib.pyplot}
module.  PyPlot uses the Julia PyCall package to call Matplotlib directly from
Julia with little or no overhead (arrays are passed without making a copy).")
    (license license:expat)))

(define-public julia-quadgk
  (package
    (name "julia-quadgk")
    (version "2.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/QuadGK.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1hy0629yai6xflgxaflk9764lzr1lzhlghimxk1aqi212q9c6n33"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-datastructures))
    (home-page "https://github.com/JuliaMath/QuadGK.jl")
    (synopsis "Adaptive 1d numerical Gauss–Kronrod integration")
    (description "This package provides support for one-dimensional numerical
integration in Julia using adaptive Gauss-Kronrod quadrature.  The code was
originally part of Base Julia.  It supports integration of arbitrary numeric
types, including arbitrary precision (@code{BigFloat}), and even integration of
arbitrary normed vector spaces (e.g. matrix-valued integrands).")
    (license license:expat)))

(define-public julia-quadmath
  (package
    (name "julia-quadmath")
    (version "0.5.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Quadmath.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "051biw4b9zni7cmh2f1yzifp1v8wazlfxrdz4p44lyd1wba6379w"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'hardcode-libmath-location
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcclib (assoc-ref inputs "gcc:lib")))
               (substitute* "src/Quadmath.jl"
                 (("libgcc_s.so.1" lib) (string-append gcclib "/lib/" lib))
                 (("libquadmath.so.0" lib) (string-append gcclib "/lib/" lib)))
               #t))))))
    (propagated-inputs
     (list julia-requires))
    (inputs
     `(("gcc:lib" ,gcc "lib")))
    (native-inputs
     (list julia-specialfunctions))
    (home-page "https://github.com/JuliaMath/Quadmath.jl")
    (synopsis "Float128 and libquadmath for the Julia language")
    (description "This is a Julia interface to @code{libquadmath}, providing a
@code{Float128} type corresponding to the IEEE754 binary128 floating point
format.")
    (license license:expat)))

(define-public julia-quaternions
  (package
    (name "julia-quaternions")
    (version "0.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGeometry/Quaternions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1zhynyvchc50hywws2jznpkwydr3njh8cv84d2ylyabhcwwmil9s"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dualnumbers))
    (home-page "https://github.com/JuliaGeometry/Quaternions.jl")
    (synopsis "Quaternion and dual-quaternion functionality")
    (description "Quaternions are best known for their suitability as
representations of 3D rotational orientation.  They can also be viewed as an
extension of complex numbers.")
    (license license:expat)))

(define-public julia-queryoperators
  (package
    (name "julia-queryoperators")
    (version "0.9.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/QueryOperators.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "06zm4cbn3x49lbpgshhdfvvmgz066qkc8q0d57igm5p8bcp6js22"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-datastructures
           julia-iteratorinterfaceextensions
           julia-tableshowutils))
    (home-page "https://github.com/queryverse/QueryOperators.jl")
    (synopsis "Query operators for Julia")
    (description "This package contains the underlying query operators that are
exposed to users in @code{Query.jl}.")
    (license license:expat)))

(define-public julia-rangearrays
  (package
    (name "julia-rangearrays")
    (version "0.3.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/RangeArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ihzfyfq1xihkjcvn7xmzfbn6igzidb4fkzdcxwfr5qkvi52gnmg"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaArrays/RangeArrays.jl")
    (synopsis "Array data structures with autogenerated columns")
    (description "The goal of RangeArrays is to provide efficient and convenient
array data structures where the columns of the arrays are generated (on the fly)
by Ranges.")
    (license license:expat)))

(define-public julia-ratios
  (package
    (name "julia-ratios")
    (version "0.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/timholy/Ratios.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1a4fd4jq4qjply29rkwg3m1clfndjsbckj1b1dab1bc35h2c6yxh"))))
    (build-system julia-build-system)
    (home-page "https://github.com/timholy/Ratios.jl")
    (synopsis "Faster Rational-like types for Julia")
    (description "This package provides types similar to Julia's @code{Rational}
type, which make some sacrifices but have better computational performance.")
    (license license:expat)))

(define-public julia-recipesbase
  (package
    (name "julia-recipesbase")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/RecipesBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1b6m5rz6wprj30rwvlxz4r1jv5gl0ay0f52kfmy2w7lqly7zhap5"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaPlots/RecipesBase.jl")
    (synopsis "Define transformation recipes on user types")
    (description "This package implements handy macros @code{@@recipe} and
@code{@@series} which will define a custom transformation and attach attributes
for user types.  Its design is an attempt to simplify and generalize the summary
and display of types and data from external packages.  With this package it is
possible to describe visualization routines that can be used as components in
more complex visualizations.")
    (license license:expat)))

(define-public julia-recipespipeline
  (package
    (name "julia-recipespipeline")
    (version "0.3.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/RecipesPipeline.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0zq4bzxvq36zr0va6iip3x97mgq5b4fwza8avszx1ryfqm3lg1f7"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with Plots.jl.
    (propagated-inputs
     (list julia-nanmath
           julia-plotutils
           julia-recipesbase))
    (home-page "http://juliaplots.org/RecipesPipeline.jl/dev/")
    (synopsis "Utilities for processing recipes")
    (description "This package was factored out of @code{Plots.jl} to allow any
other plotting package to use the recipe pipeline.  In short, the extremely
lightweight @code{RecipesBase.jl} package can be depended on by any package to
define \"recipes\": plot specifications of user-defined types, as well as custom
plot types.  @code{RecipePipeline.jl} contains the machinery to translate these
recipes to full specifications for a plot.")
    (license license:expat)))

(define-public julia-recursivearraytools
  (package
    (name "julia-recursivearraytools")
    (version "2.16.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SciML/RecursiveArrayTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0vx8ndxrii53na7jsc2lki47wfpi77rn3y2r6xhiysx1qwr14msf"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with OrdinaryDiffEq.jl.
    (propagated-inputs
     (list julia-arrayinterface
           julia-chainrulescore
           julia-docstringextensions
           julia-recipesbase
           julia-requires
           julia-staticarrays
           julia-zygoterules))
    ;(native-inputs
    ; `(("julia-forwarddiff" ,julia-forwarddiff)
    ;   ("julia-nlsolve" ,julia-nlsolve)
    ;   ("julia-ordinarydiffeq" ,julia-ordinarydiffeq)
    ;   ("julia-structarrays" ,julia-structarrays)
    ;   ("julia-zygote" ,julia-zygote)))
    (home-page "https://github.com/SciML/RecursiveArrayTools.jl")
    (synopsis "Tools for handling objects like arrays of arrays and deeper nestings")
    (description "@code{RecursiveArrayTools.jl} is a set of tools for dealing with
recursive arrays like arrays of arrays.")
    (license license:expat)))

(define-public julia-reexport
  (package
    (name "julia-reexport")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/simonster/Reexport.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0inf5q6f01ncd5c5wm8mwzv2hs627ng6xlh8dhrlflp321hbamwf"))))
    (build-system julia-build-system)
    (home-page "https://github.com/simonster/Reexport.jl")
    (synopsis "Re-export modules and symbols")
    (description "This package provides tools to re-export modules and symbols.")
    (license license:expat)))

(define-public julia-referencetests
  (package
    (name "julia-referencetests")
    (version "0.9.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaTesting/ReferenceTests.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0mm6bjhs8a21pippww6b08b5frmnb9m6k8xrszrwq9zhc879zpc9"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with ImageCore.jl through ImageMagick.jl.
    (propagated-inputs
     (list julia-deepdiffs
           julia-distances
           julia-fileio
           julia-imagecore
           julia-imageinterminal))
    ;(native-inputs
    ; `(("julia-csvfiles" ,julia-csvfiles)
    ;   ("julia-dataframes" ,julia-dataframes)
    ;   ("julia-gr" ,julia-gr)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-imagetransformations" ,julia-imagetransformations)
    ;   ("julia-plots" ,julia-plots)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://juliatesting.github.io/ReferenceTests.jl/latest/")
    (synopsis "Utility package for comparing data against reference files")
    (description "@code{ReferenceTests.jl} is a Julia package that adds a couple
of additional macros to your testing toolbox.  In particular, it focuses on
functionality for testing values against reference files, which in turn the
package can help create and update if need be.")
    (license license:expat)))

(define-public julia-requires
  (package
    (name "julia-requires")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaPackaging/Requires.jl/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03hyfy7c0ma45b0y756j76awi3az2ii4bz4s8cxm3xw9yy1z7b01"))))
    (build-system julia-build-system)
    (arguments
     `(#:parallel-tests? #f))
    (inputs                             ;required for test
     (list julia-example))
    (propagated-inputs
     (list julia-colors))
    (home-page "https://github.com/JuliaPackaging/Requires.jl/")
    (synopsis "Faster package loader")
    (description "This package make loading packages faster, maybe.  It
supports specifying glue code in packages which will load automatically when
another package is loaded, so that explicit dependencies (and long load times)
can be avoided.")
    (license license:expat)))

(define-public julia-reversediff
  (package
    (name "julia-reversediff")
    (version "1.9.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDiff/ReverseDiff.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wrr6sqj2xl9grkvdp88rw3manxy9vbx28zq2wssya5ns1xabsnl"))))
    (build-system julia-build-system)
    (arguments
     ;; XXXX: Test suite failing for i686, e.g.,
     ;; Expression: hash(tr_float, hash(1)) === hash(v_float, hash(1))
     ;; MethodError: no method matching decompose(::ReverseDiff.TrackedReal{Float64, Float64, Nothing})
     ;; Disable as stopgap.
     `(#:tests? ,(not (target-x86-32?))))
    (propagated-inputs
     (list julia-diffresults
           julia-diffrules
           julia-forwarddiff
           julia-functionwrappers
           julia-macrotools
           julia-nanmath
           julia-specialfunctions
           julia-staticarrays))
    (native-inputs
     (list julia-difftests
           julia-fillarrays))
    (home-page "https://github.com/JuliaDiff/ReverseDiff.jl")
    (synopsis "Reverse Mode Automatic Differentiation for Julia")
    (description "@code{ReverseDiff.jl} is a fast and compile-able tape-based
reverse mode @acronym{AD, automatic differentiation}, that implements methods to
take gradients, Jacobians, Hessians, and higher-order derivatives of native
Julia functions (or any callable object, really).")
    (license license:expat)))

(define-public julia-richardson
  (package
    (name "julia-richardson")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/Richardson.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06v9ii3d7hh41fsrfklaa8ap55z5s017f888mrd1c18y4fx9i4nx"))))
    (build-system julia-build-system)
    (home-page "https://juliapackages.com/p/richardson")
    (synopsis "Extrapolate function using Richardson method")
    (description "This package provides a function extrapolate that
extrapolates a given function @code{f(x)} to @code{f(x0)}, evaluating @code{f}
only at a geometric sequence of points @code{> x0} (or optionally @code{<
x0}).  The key algorithm is Richardson extrapolation using a Neville–Aitken
tableau, which adaptively increases the degree of an extrapolation polynomial
until convergence is achieved to a desired tolerance (or convergence stalls
due to e.g. floating-point errors).  This allows one to obtain @code{f(x0)} to
high-order accuracy, assuming that @code{f(x0+h)} has a Taylor series or some
other power series in @code{h}.")
    (license license:expat)))

(define-public julia-rotations
  ;; This is the first commit with support for julia-1.6.
  (let ((commit "b599102535bc3534252c76f3fd4cf521f4741788")
        (revision "1"))
    (package
      (name "julia-rotations")
      (version (git-version "1.0.2" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/JuliaGeometry/Rotations.jl")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "01wwqbdmj61yiz7rkmams4xg7gb9hbmg325173846ky4b9x1wb19"))))
      (build-system julia-build-system)
      (propagated-inputs
       (list julia-staticarrays))
      (native-inputs
       (list julia-benchmarktools
             julia-forwarddiff
             julia-unitful))
      (home-page "https://github.com/JuliaGeometry/Rotations.jl")
      (synopsis "Julia implementations for different rotation parameterisations")
      (description "This package implements various 3D rotation parameterizations
and defines conversions between them.  At their heart, each rotation
parameterization is a 3×3 unitary (orthogonal) matrix (based on the
@code{StaticArrays.jl} package), and acts to rotate a 3-vector about the origin
through matrix-vector multiplication.")
      (license license:expat))))

(define-public julia-safetestsets
  ;; The only release tag is the first commit in the repository.
  (let ((commit "e553edc4c753344d38349304b9ff5483c3b8ff21")
        (revision "1"))
    (package
      (name "julia-safetestsets")
      (version (git-version "0.0.1" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/YingboMa/SafeTestsets.jl")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "1fb1dfdmiw2ggx60hf70954xlps0r48fcb3k3dvxynlz7ylphp96"))))
      (build-system julia-build-system)
      (arguments
       `(#:julia-package-name "SafeTestsets"
         #:julia-package-uuid "1bc83da4-3b8d-516f-aca4-4fe02f6d838f"))
      (native-inputs
       (list julia-staticarrays))
      (home-page "https://github.com/YingboMa/SafeTestsets.jl")
      (synopsis "Julia's testset in a module")
      (description "This package contains the testset from Julia, packaged into
a loadable module.")
      (license license:expat))))

(define-public julia-sass
  (package
    (name "julia-sass")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/piever/Sass.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0y7kkkj717h5cj659ssry89i5r64symr6pvhr6vv4qmaxrnjxj92"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-libsass-jll))
    (home-page "https://github.com/piever/Sass.jl")
    (synopsis "Compile scss and sass file to css")
    (description "This package provides a simple Julian API to use the
@code{libsass} library to compile scss and sass files to css.")
    (license license:expat)))

(define-public julia-scanbyte
  (package
    (name "julia-scanbyte")
    (version "0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jakobnissen/ScanByte.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0dqqa3d7c87358144pji6ik8xlki2hj0hkvjs72j5aypfms8rwn3"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-simd))
    (home-page "https://github.com/jakobnissen/ScanByte.jl")
    (synopsis "Find the first occurrence of set of bytes in a chunk of memory")
    (description "This package finds the first occurrence of a byte or set of
bytes in a chunk of memory.  Think of it like a much faster version of
@code{findfirst} that only iterates over bytes in memory.")
    (license license:expat)))

(define-public julia-scratch
  (package
    (name "julia-scratch")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPackaging/Scratch.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "09xni9rrrax17fxjz04j1b48mk9ffww5rcbagh66jklr89mrkqhx"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Test suite tries to access the Julia package registry.
    (home-page "https://github.com/JuliaPackaging/Scratch.jl")
    (synopsis "Scratch spaces for all your persistent mutable data needs")
    (description "This repository implements the scratch spaces API for
package-specific mutable containers of data.  These spaces can contain datasets,
text, binaries, or any other kind of data that would be convenient to store in
a location specific to your package.  As compared to Artifacts, these containers
of data are mutable.  Because the scratch space location on disk is not very
user-friendly, scratch spaces should, in general, not be used for a storing
files that the user must interact with through a file browser.")
    (license license:expat)))

(define-public julia-showoff
  (package
    (name "julia-showoff")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/Showoff.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1g4hqvjjpwbrs7fnllyl5w66yj6qlvpvzpygym2nvf01m1ps6m53"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaGraphics/Showoff.jl")
    (synopsis "Nicely format an array of n things for tables and plots")
    (description "@code{Showoff} provides an interface for consistently
formatting an array of n things, e.g. numbers, dates, unitful values.  It's used
in @code{Gadfly}, @code{Plots} and @code{Makie} to label axes and keys.")
    (license license:expat)))

(define-public julia-simd
  (package
    (name "julia-simd")
    (version "3.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/eschnett/SIMD.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0z7m5fykc6r4bxz4hfm6d3v1h7gg0c322l4zv8r3vrb8hrd6h263"))))
    (build-system julia-build-system)
    (home-page "https://github.com/eschnett/SIMD.jl")
    (synopsis "Explicit SIMD vectorization")
    (description "This package allows programmers to explicitly SIMD-vectorize
their Julia code.  By exposing SIMD vector types and corresponding operations,
the programmer can explicitly vectorize their code.  While this does not
guarantee that the generated machine code is efficient, it relieves the
compiler from determining whether it is legal to vectorize the code, deciding
whether it is beneficial to do so, and rearranging the code to synthesize
vector instructions.")
    (license license:expat)))

(define-public julia-simpletraits
  (package
    (name "julia-simpletraits")
    (version "0.9.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mauro3/SimpleTraits.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qvmkqcvhc2nilvkk36szccxdlcv9ls2i0ksxgl2yfjr3b3qlr05"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-macrotools))
    (home-page "https://github.com/mauro3/SimpleTraits.jl")
    (synopsis "Simple Traits for Julia")
    (description "This package provides a macro-based implementation of traits.
The main idea behind traits is to group types outside the type-hierarchy and to
make dispatch work with that grouping.  The difference to Union-types is that
types can be added to a trait after the creation of the trait, whereas Union
types are fixed after creation.")
    (license license:expat)))

(define-public julia-softglobalscope
  (package
    (name "julia-softglobalscope")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stevengj/SoftGlobalScope.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n3l0al1vw5jpb4k9a29a71666cdb617nmiqg34wcmyfzrxpvv39"))))
    (build-system julia-build-system)
    (home-page "https://github.com/stevengj/SoftGlobalScope.jl")
    (synopsis "Utilities for soft global scope in interactive Julia environments")
    (description
     "SoftGlobalScope is a package for the Julia language that simplifies the
variable scoping rules for code in global scope.  It is intended for interactive
shells to make it easier to work interactively with Julia, especially for
beginners.")
    (license license:expat)))

(define-public julia-sortingalgorithms
  (package
    (name "julia-sortingalgorithms")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaCollections/SortingAlgorithms.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "173x77a80xnh99viqa3r7rgdaksvxaw8xyfqw09gwvp4p2zrxivb"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; cycle with StatsBase.jl
    (propagated-inputs
     (list julia-datastructures))
    ;(native-inputs
    ; `(("julia-statsbase" ,julia-statsbase)))
    (home-page "https://github.com/JuliaCollections/SortingAlgorithms.jl")
    (synopsis "Extra sorting algorithms extending Julia's sorting API")
    (description "The SortingAlgorithms package provides three sorting
algorithms that can be used with Julia's standard sorting API: heapsort,
timsort and radixsort.")
    (license license:expat)))

(define-public julia-specialfunctions
  (package
    (name "julia-specialfunctions")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/SpecialFunctions.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nfpnglx7zl9g20w2mgfkg17hcw9ri0shaq2rwplj0ij5pwz8yf0"))))
    (build-system julia-build-system)
    (inputs
     (list julia-chainrulestestutils))
    (propagated-inputs
     (list julia-chainrulescore
           julia-logexpfunctions
           julia-openspecfun-jll))
    (home-page "https://github.com/JuliaMath/SpecialFunctions.jl")
    (synopsis "Special mathematical functions")
    (description "This package provides special mathematical functions,
including Bessel, Hankel, Airy, error, Dawson, exponential (or sine and
cosine) integrals, eta, zeta, digamma, inverse digamma, trigamma, and
polygamma functions.")
    (license license:expat)))

(define-public julia-splitapplycombine
  (package
    (name "julia-splitapplycombine")
    (version "1.1.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/SplitApplyCombine.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qzaqvk57b0s5krzn8bxkzmr5kz6hi9dm3jbf2sl7z4vznsgbn9x"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dictionaries
           julia-indexing))
    (home-page "https://github.com/JuliaData/SplitApplyCombine.jl")
    (synopsis "Split-apply-combine strategies for Julia")
    (description "@code{SplitApplyCombine.jl} provides high-level, generic tools
for manipulating data - particularly focussing on data in nested containers.  An
emphasis is placed on ensuring split-apply-combine strategies are easy to apply,
and work reliably for arbitrary iterables and in an optimized way with the data
structures included in Julia's standard library.")
    (license license:expat)))

(define-public julia-stablerngs
  (package
    (name "julia-stablerngs")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaRandom/StableRNGs.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1cw4wc38qbgmrrx0jjwjhynnarrzjkh0yyz242zj272brbci7p1r"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaRandom/StableRNGs.jl")
    (synopsis "Julia RNG with stable streams")
    (description "This package intends to provide a simple RNG with stable
streams, suitable for tests in packages which need reproducible streams of
random numbers across Julia versions.  Indeed, the Julia RNGs provided by
default are documented to have non-stable streams (which for example enables
some performance improvements).")
    (license license:expat)))

(define-public julia-stackviews
  (package
    (name "julia-stackviews")
    (version "0.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/StackViews.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1fwiaxdpx1z9dli3jr8kyraych0jbdiny3qklynf0r13px25r6i7"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'skip-doctest
           (lambda _
             (substitute* "test/runtests.jl"
               ((".*doctest.*") ""))
             #t)))))
    (propagated-inputs
     (list julia-offsetarrays))
    (native-inputs
    (list julia-aqua
          julia-documenter))
    (home-page "https://github.com/JuliaArrays/StackViews.jl")
    (synopsis "No more catcat")
    (description "StackViews provides only one array type: @code{StackView}.
There are multiple ways to understand @code{StackView}:
@itemize
@item inverse of @code{eachslice}
@item @code{cat} variant
@item view object
@item lazy version of @code{repeat} special case
@end itemize")
    (license license:expat)))

(define-public julia-static
  (package
    (name "julia-static")
    (version "0.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SciML/Static.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "19k7h47zhz3zz28i0l4b3cc0r07pzp8kf35z0yammpy361b644l2"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-ifelse))
    (native-inputs
     (list julia-aqua))
    (home-page "https://github.com/SciML/Static.jl")
    (synopsis "Static types useful for dispatch and generated functions")
    (description "Static.jl defines a limited set of statically parameterized
types and a common interface that is shared between them.")
    (license license:expat)))

(define-public julia-staticarrays
  (package
    (name "julia-staticarrays")
    (version "1.2.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaArrays/StaticArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "041mijzgzm8r61a3gwspr2wcxjikqksvj2rbnl4gmcy41alqmx79"))))
    (build-system julia-build-system)
    (inputs
     (list julia-benchmarktools))
    (home-page "https://github.com/JuliaArrays/StaticArrays.jl")
    (synopsis "Statically sized arrays")
    (description "This package provides a framework for implementing
statically sized arrays in Julia, using the abstract type
@code{StaticArray{Size,T,N} <: AbstractArray{T,N}}.  Subtypes of
@code{StaticArray} will provide fast implementations of common array and
linear algebra operations.")
    (license license:expat)))

(define-public julia-statsapi
  (package
    (name "julia-statsapi")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/StatsAPI.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1k1c3s7i5wzkz4r9fyy4gd7wb97p0qgbc7bmaajm16zqipfmy2bv"))))
    (build-system julia-build-system)
    (home-page "https://juliastats.org/")
    (synopsis "Statistics-focused namespace for packages to share functions")
    (description "This package provides a namespace for data-related generic
function definitions to solve the optional dependency problem; packages wishing
to share and/or extend functions can avoid depending directly on each other by
moving the function definition to @code{StatsAPI.jl} and each package taking a
dependency on it.")
    (license license:expat)))

(define-public julia-statsbase
  (package
    (name "julia-statsbase")
    (version "0.33.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/StatsBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02y4pm5yvg713a2pn970bbcfkrn2h133rxbxk1da18svhqw3czhi"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dataapi
           julia-datastructures
           julia-missings
           julia-sortingalgorithms
           julia-statsapi))
    (native-inputs
     (list julia-stablerngs))
    (home-page "https://github.com/JuliaStats/StatsBase.jl")
    (synopsis "Basic statistics for Julia")
    (description "StatsBase.jl is a Julia package that provides basic support
for statistics.  Particularly, it implements a variety of statistics-related
functions, such as scalar statistics, high-order moment computation, counting,
ranking, covariances, sampling, and empirical density estimation.")
    (license license:expat)))

(define-public julia-stringdistances
  (package
    (name "julia-stringdistances")
    (version "0.10.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/matthieugomez/StringDistances.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0n5707wplbibzhhv1xmshvc025d7xk6mnikclc3hvq5cdc0gy9f7"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-distances))
    (home-page "https://github.com/matthieugomez/StringDistances.jl")
    (synopsis "String Distances in Julia")
    (description "This package provides string distances in Julia.  Distances
are defined for @code{AbstractStrings}, and any iterator that define
@code{length()}.  The package also defines Distance \"modifiers\" that can be
applied to any distance.")
    (license license:expat)))

(define-public julia-stringencodings
  (package
    (name "julia-stringencodings")
    (version "0.3.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStrings/StringEncodings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qwc5ll68ng80b5921ww6fvifxbsmiylakfgsbsjbzg7lzyb5i67"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-failing-test
           (lambda _
             ;; https://github.com/JuliaStrings/StringEncodings.jl/issues/49
             (substitute* "test/runtests.jl"
               (("\"SHIFT_JIS\", \"SHIFT_JISX0213\"")
                " ")))))))
    (propagated-inputs
     (list julia-libiconv-jll))
    (home-page "https://github.com/JuliaStrings/StringEncodings.jl")
    (synopsis "Support for decoding and encoding texts")
    (description "This package provides support for decoding and encoding
texts between multiple character encodings.  It is currently based on the
@code{iconv} interface, and supports all major platforms using GNU libiconv.")
    (license license:expat)))

(define-public julia-structarrays
  (package
    (name "julia-structarrays")
    (version "0.6.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/StructArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0rjcpyjwzg70n87q5r9c5i1qzigavncslxssm3rk5a3y549py56v"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       ,@(if (target-64bit?)
           '(%standard-phases)
           '((modify-phases %standard-phases
               (add-after 'unpack 'fix-tests-int32-i686
                 (lambda _
                   (substitute* '("src/utils.jl"
                                  "test/runtests.jl")
                     (("Int64") "Int32")))))))))
    (propagated-inputs
     (list julia-dataapi
           julia-staticarrays
           julia-tables))
    (native-inputs
     (list julia-documenter
           julia-offsetarrays
           julia-pooledarrays
           julia-typedtables
           julia-weakrefstrings))
    (home-page "https://github.com/JuliaArrays/StructArrays.jl")
    (synopsis "Efficient implementation of struct arrays in Julia")
    (description "This package introduces the type @code{StructArray} which is
an @code{AbstractArray} whose elements are @code{struct} (for example
@code{NamedTuples}, or @code{ComplexF64}, or a custom user defined
@code{struct}).  While a @code{StructArray} iterates @code{structs}, the layout
is column based (meaning each field of the @code{struct} is stored in a separate
@code{Array}).")
    (license license:expat)))

(define-public julia-structtypes
  (package
    (name "julia-structtypes")
    (version "1.7.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/StructTypes.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02mn4kkhn3927dk7945c9bjwlldihydxgn5ilmqqvs8dknvbw8p1"))))
    (build-system julia-build-system)
    (home-page "https://juliadata.github.io/StructTypes.jl/stable/")
    (synopsis "Abstract definitions and convenience methods for Julia objects")
    (description "This package provides the @code{StructTypes.StructType} trait
for Julia types to declare the kind of \"struct\" they are, providing
serialization/deserialization packages patterns and strategies to automatically
construct objects.")
    (license license:expat)))

(define-public julia-suppressor
  (package
    (name "julia-suppressor")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/Suppressor.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0v6pxvf8lzrqjc676snvlszh14ridl442g2h6syfjiy75pk7mdyc"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaIO/Suppressor.jl")
    (synopsis "Capture stdout and sterr")
    (description "Julia macros for suppressing and/or capturing output (stdout),
warnings (stderr) or both streams at the same time.")
    (license license:expat)))

(define-public julia-tableiointerface
  (package
    (name "julia-tableiointerface")
    (version "0.1.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lungben/TableIOInterface.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0p2fi9jbyfg2j6rysv4if7dx8qw2mssb04i75j1zq607j8707kvn"))))
    (build-system julia-build-system)
    (home-page "https://github.com/lungben/TableIOInterface.jl")
    (synopsis "File formats based on file extensions")
    (description "This package determines tabular file formats based on file
extensions.  It is intended to be the base both for @code{TableIO.jl} and for
the @code{Pluto.jl} tabular data import functionality.")
    (license license:expat)))

(define-public julia-tables
  (package
    (name "julia-tables")
    (version "1.4.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/Tables.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1sqqagzqvav8b1rv5ywwbgy9ixvlmipq95fkwfwn0m8769i8jwzb"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dataapi
           julia-datavalueinterfaces
           julia-iteratorinterfaceextensions
           julia-tabletraits))
    (native-inputs
     (list julia-datavalues
           julia-queryoperators))
    (home-page "https://github.com/JuliaData/Tables.jl")
    (synopsis "Interface for tables in Julia")
    (description "The @code{Tables.jl} package provides simple, yet powerful
interface functions for working with all kinds tabular data.")
    (license license:expat)))

(define-public julia-tableshowutils
  ;; The 0.2.5 release is not fully compatable with newer versions of Julia.
  (let ((commit "c4e02d8b9bbb31fc81ed6618955e9b1c7cb04460")
        (revision "1"))
    (package
      (name "julia-tableshowutils")
      (version "0.2.5")
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/queryverse/TableShowUtils.jl")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0gp3hpj3jvzfhkp9r345vfic2j2n2s60729wv38hwn75csp74cg5"))))
      (build-system julia-build-system)
      (propagated-inputs
       (list julia-datavalues
             julia-json))
      (home-page "https://github.com/queryverse/TableShowUtils.jl")
      (synopsis "Implement show for TableTraits.jl types")
      (description "This package provides some common helper functions that make
it easier to implement various @code{Base.show} functions for types that
participate in the @code{TableTraits.jl} ecosystem.")
      (license license:expat))))

(define-public julia-tabletraits
  (package
    (name "julia-tabletraits")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/TableTraits.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "08ssb2630wm6j8f2qa985mn2vfibfm5kjcn4ayl2qkhfcyp8daw4"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-iteratorinterfaceextensions))
    (home-page "https://github.com/queryverse/TableTraits.jl")
    (synopsis "Traits for Julia tables")
    (description "TableTraits defines a generic interface for tabular data.")
    (license license:expat)))

(define-public julia-tensorcore
  (package
    (name "julia-tensorcore")
    (version "0.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/TensorCore.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1sy3in4a1rl3l2vk0cm9mzg2nkva7syhr7i35si0kbzhkdwpbqjy"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaMath/TensorCore.jl")
    (synopsis "Tensor-algebra definitions")
    (description "This package is intended as a lightweight foundation for
tensor operations across the Julia ecosystem.  Currently it exports three
operations: @acronym{hadamard, elementwise multiplication}, @acronym{tensor,
product preserves all dimensions}, and @acronym{boxdot, contracts neighboring
dimensions}.")
    (license license:expat)))

(define-public julia-testimages
  (package
    (name "julia-testimages")
    (version "1.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/TestImages.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1lnfsmx33qspyvxw0cykwh7il8xykjpcw1080sisn95ngz2qhdmy"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; cycle with ImageMagick.jl
    (propagated-inputs
     (list julia-axisarrays
           julia-colortypes
           julia-fileio
           julia-offsetarrays
           julia-stringdistances))
    ;(native-inputs
    ; `(("julia-colors" ,julia-colors)
    ;   ("julia-fixedpointnumbers" ,julia-fixedpointnumbers)
    ;   ("julia-imagecontrastadjustment" ,julia-imagecontrastadjustment)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-ometiff" ,julia-ometiff)
    ;   ("julia-referencetests" ,julia-referencetests)))
    (home-page "https://testimages.juliaimages.org/")
    (synopsis "Standard test images for Julia")
    (description "This package provides a convenient Julia interface for loading
standard named test images and example images for the internal usage in
@code{JuliaImages}.  This can be used in conjunction with the @code{Images}
package.")
    (license license:expat)))

(define-public julia-tracker
  (package
    (name "julia-tracker")
    (version "0.2.12")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/FluxML/Tracker.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1s4mdywbp7nli7z985fqaj1rs4i6d92b1jx3lhg0qhk1s5wc0v8j"))
        (patches (search-patches "julia-tracker-16-compat.patch"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-adapt
           julia-diffrules
           julia-forwarddiff
           julia-macrotools
           julia-nanmath
           julia-nnlib
           julia-requires
           julia-specialfunctions))
    (native-inputs
     (list julia-pdmats))
    (home-page "https://github.com/FluxML/Tracker.jl")
    (synopsis "Operator overloading reverse-mode automatic differentiator")
    (description "@code{Tracker.jl} previously provided @code{Flux.jl} with
automatic differentiation for its machine learning platform.")
    (license license:expat)))

(define-public julia-transcodingstreams
  (package
    (name "julia-transcodingstreams")
    (version "0.9.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/TranscodingStreams.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1w3klii293caqiclfh28jggv7f53xclm9fr6xmw38brwrn1hjb48"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))                ; Circular dependency with various codecs.
    (home-page "https://github.com/JuliaIO/TranscodingStreams.jl")
    (synopsis "Fast I/O transcoding data streams")
    (description "This package provides tools for transcoding data streams
which are:
@itemize
@item fast: small overhead and specialized methods
@item consistent: basic I/O operations work as expected
@item generic: support any I/O objects like files, buffers, pipes, etc.
@item extensible: easy definition for new codec to transcode data
@end itemize")
    (license license:expat)))

(define-public julia-twiddle
  (package
    (name "julia-twiddle")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SabrinaJaye/Twiddle.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1c2gdv7sy4n1d8687w2q0yzwmwmanf4p6mvzvkz5gm4baxyzmbh2"))))
    (build-system julia-build-system)
    (home-page "https://ben-ward.science/Twiddle.jl/stable")
    (synopsis "Ready to use bit-twiddling tricks")
    (description "This package provides a collection of useful bit-twiddling
tricks, ready to use as functions, with detailed documentation and example
real-world use cases.")
    (license license:expat)))

(define-public julia-typedtables
  (package
    (name "julia-typedtables")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/TypedTables.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0nk6zhqvl2r8yhjdhb59kxq0srd3vy4ysg4d8rszj9a43dnn3w3i"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-adapt
           julia-splitapplycombine
           julia-tables))
    (home-page "https://github.com/JuliaData/TypedTables.jl")
    (synopsis "Column-based storage for data analysis in Julia")
    (description "@code{TypedTables.jl} provides two column-based storage
containers: @code{Table} and @code{FlexTable}, both of which represent an array
of @code{NamedTuples}.  This package is designed to be lightweight, easy-to-use
and fast, and presents a very minimal new interface to learn.")
    (license license:expat)))

(define-public julia-unpack
  (package
    (name "julia-unpack")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mauro3/UnPack.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "066v1px72zidnvhl0rczhh07rcfwvli0jx5nprrgyi1dvj3mps2a"))))
    (build-system julia-build-system)
    (home-page "https://github.com/mauro3/UnPack.jl")
    (synopsis "Pack and Unpack macros for Julia")
    (description "The @code{@@unpack} and @code{@@pack!} macros work to unpack
types, modules, and dictionaries.")
    (license license:expat)))

(define-public julia-uris
  (package
    (name "julia-uris")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaWeb/URIs.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kp4hg3kknkm2smlcizqfd33l9x4vkahc2714gnbjp39fj285b92"))))
    (build-system julia-build-system)
    (arguments
     '(#:julia-package-name "URIs"      ;required to run tests
       #:julia-package-uuid "5c2747f8-b7ea-4ff2-ba2e-563bfd36b1d4"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'change-dir
           ;; Tests must be run from the testdir
           (lambda* (#:key source outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (chdir
                (string-append out "/share/julia/loadpath/URIs/test")))
             #t)))))
    ;; required for tests
    (inputs (list julia-json))
    (home-page "https://github.com/JuliaWeb/URIs.jl")
    (synopsis "URI parsing in Julia")
    (description "@code{URIs.jl} is a Julia package that allows parsing and
working with @acronym{URIs,Uniform Resource Identifiers}, as defined in RFC
3986.")
    (license license:expat)))

(define-public julia-unitful
  (package
    (name "julia-unitful")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PainterQubits/Unitful.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10qwscd15dnmvx116dwvg99m7kmwgmj5ahdkq7psiq48lcc554gq"))))
    (build-system julia-build-system)
    (arguments
     `(#:parallel-tests? #f))
    (propagated-inputs
     (list julia-constructionbase))
    (home-page "https://painterqubits.github.io/Unitful.jl/stable/")
    (synopsis "Physical units in Julia")
    (description "This package supports SI units and also many other unit
system.")
    (license license:expat)))

(define-public julia-versionparsing
  (package
    (name "julia-versionparsing")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaInterop/VersionParsing.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "060s72dsnpavgilf7f7315lw2sn4npk8lkndmj6bg7i23hppiwva"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaInterop/VersionParsing.jl")
    (synopsis "Flexible VersionNumber parsing in Julia")
    (description "The @code{VersionParsing} package implements flexible parsing
of version-number strings into Julia's built-in @code{VersionNumber} type, via
the @code{vparse(string)} function.  Unlike the @code{VersionNumber(string)}
constructor, @code{vparse(string)} can handle version-number strings in a much
wider range of formats than are encompassed by the semver standard.  This is
useful in order to support @code{VersionNumber} comparisons applied to
\"foreign\" version numbers from external packages.")
    (license license:expat)))

(define-public julia-weakrefstrings
  (package
    (name "julia-weakrefstrings")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/WeakRefStrings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "14h1vdnc3rx87w6v2rr59lgb4kai2hd1wzqpxhmzsi8karg2z219"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dataapi
           julia-parsers))
    (home-page "https://github.com/JuliaData/WeakRefStrings.jl")
    (synopsis "Efficient string representation and transfer in Julia")
    (description "This package provides a minimal String type for Julia that
allows for efficient string representation and transfer")
    (license license:expat)))

(define-public julia-woodburymatrices
  (package
    (name "julia-woodburymatrices")
    (version "0.5.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/timholy/WoodburyMatrices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "04yykivi8zrbryxlmb0p5xa6lma8iq22r5s863117dnnqj5gaffd"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       ,@(if (target-x86-32?)
           '((modify-phases %standard-phases
               (add-after 'unpack 'remove-failing-test-i686
                 (lambda _
                   (substitute* "test/woodbury.jl"
                     (("@test logdet\\(W\\)")
                      "@test_broken logdet(W)"))))))
           '(%standard-phases))))
    (home-page "https://github.com/timholy/WoodburyMatrices.jl")
    (synopsis "Support for the Woodbury matrix identity for Julia")
    (description "This package provides support for the Woodbury matrix identity
for the Julia programming language.  This is a generalization of the
Sherman-Morrison formula.  Note that the Woodbury matrix identity is notorious
for floating-point roundoff errors, so be prepared for a certain amount of
inaccuracy in the result.")
    (license license:expat)))

(define-public julia-yaml
  (package
    (name "julia-yaml")
    (version "0.4.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/YAML.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00k8456ffldbf75k2q5yxim7cgz3p0pbshsvmpm1331g8qy6liin"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-stringencodings))
    (native-inputs
     (list julia-datastructures
           julia-orderedcollections))
    (home-page "https://github.com/JuliaData/YAML.jl")
    (synopsis "Parses YAML documents into native Julia types")
    (description "This package parses YAML documents into native Julia types
and dumps them back into YAML documents.")
    (license license:expat)))

(define-public julia-zipfile
  (package
    (name "julia-zipfile")
    (version "0.9.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fhs/ZipFile.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "15bm3ki5mb9nvqs2byznrryq0bilnjcvsfy3k05hxhk9vapilw7k"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-zlib-jll))
    (home-page "https://github.com/fhs/ZipFile.jl")
    (synopsis "Read/Write ZIP archives in Julia")
    (description "This module provides support for reading and writing ZIP
archives in Julia.")
    (license license:expat)))

(define-public julia-zygoterules
  (package
    (name "julia-zygoterules")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FluxML/ZygoteRules.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07i2mf6rr5b8i6l82qgwl5arsb5pwyyzyfasgnszhdqllk9501bs"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-macrotools))
    (home-page "https://github.com/FluxML/ZygoteRules.jl")
    (synopsis "Add minimal custom gradients to Zygote")
    (description "Minimal package which enables to add custom gradients to
Zygote, without depending on Zygote itself.")
    (license license:expat)))

(define-public julia-zygote
  (package
    (name "julia-zygote")
    (version "0.6.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FluxML/Zygote.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cx66sp30s34ln6p0fpqk1ggjxfxg2gp8791zz3cl85dmk4dl14b"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))                    ;require CUDA, not packaged yet
    (propagated-inputs
     (list julia-abstractffts
           julia-chainrules
           julia-diffrules
           julia-fillarrays
           julia-forwarddiff
           julia-irtools
           julia-macrotools
           julia-nanmath
           julia-requires
           julia-specialfunctions
           julia-zygoterules))
    (home-page "https://fluxml.ai/Zygote.jl")
    (synopsis "Automatic differentiation in Julia")
    (description "Zygote provides source-to-source automatic
differentiation (AD) in Julia, and is the next-generation AD system for the
Flux differentiable programming framework.")
    (license license:expat)))
