;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system julia)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages julia-jll))

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
     `(("julia-unitful" ,julia-unitful)))
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
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaGPU/Adapt.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zs5mjnql77jvrsm8lrlfkq5524wnrfxqxyqyjk8ka2xpxf9lp7n"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaGPU/Adapt.jl")
    (synopsis "Package providing the @code{adapt} function, similar to @code{convert}")
    (description "This Julia package provides the @code{adapt(T, x)} function
acts like @code{convert(T, x)}, but without the restriction of returning a
@code{T}.  This allows you to \"convert\" wrapper types like @code{Adjoint} to
be GPU compatible without throwing away the wrapper.")
    (license license:expat)))

(define-public julia-aqua
  (package
    (name "julia-aqua")
    (version "0.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaTesting/Aqua.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0zcvrwnyhh2kr4d2xv7ps8dh7byw78dx6yb1m9m4dblgscn5kypb"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaTesting/Aqua.jl")
    (synopsis "Automated quality assurance for Julia packages")
    (description "@acronym{Aqua.jl, Auto QUality Assurance for Julia packages},
provides functions to run a few automatable checks for Julia packages.")
    (license license:expat)))

(define-public julia-arraylayouts
  (package
    (name "julia-arraylayouts")
    (version "0.7.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/ArrayLayouts.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "01725v4jp8h8zwn85splw907r206h1hnp205pchmzjin7h4659xz"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-fillarrays" ,julia-fillarrays)))
    (home-page "https://github.com/JuliaMatrices/ArrayLayouts.jl")
    (synopsis "Array layouts and general fast linear algebra")
    (description "This package implements a trait-based framework for describing
array layouts such as column major, row major, etc. that can be dispatched to
appropriate BLAS or optimised Julia linear algebra routines.  This supports a
much wider class of matrix types than Julia's in-built @code{StridedArray}.")
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
     `(("julia-woodburymatrices" ,julia-woodburymatrices)))
    (home-page "https://github.com/timholy/AxisAlgorithms.jl")
    (synopsis "Filtering and linear algebra routines for multidimensional arrays")
    (description "@code{AxisAlgorithms} is a collection of filtering and linear
algebra algorithms for multidimensional arrays.  For algorithms that would
typically apply along the columns of a matrix, you can instead pick an arbitrary
axis (dimension).")
    (license license:expat)))

(define-public julia-bandedmatrices
  (package
    (name "julia-bandedmatrices")
    (version "0.16.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/BandedMatrices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "105y5d3208s0byk3p0469sfy79lhjpdblk6karbwj8x7hl26na00"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-arraylayouts" ,julia-arraylayouts)
       ("julia-fillarrays" ,julia-fillarrays)))
    (native-inputs
     `(("julia-genericlinearalgebra" ,julia-genericlinearalgebra)))
    (home-page "https://github.com/JuliaMatrices/BandedMatrices.jl")
    (synopsis "Julia package for representing banded matrices")
    (description "This package supports representing banded matrices by only
the entries on the bands.")
    (license license:expat)))

(define-public julia-benchmarktools
  (package
    (name "julia-benchmarktools")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaCI/BenchmarkTools.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "000izw9pj7cbh9r35nnwg2ijkb9dpfd5nkl2889b8b2dpsh4fi63"))))
    (build-system julia-build-system)
    (propagated-inputs `(("julia-json" ,julia-json)))
    (home-page "https://github.com/JuliaCI/BenchmarkTools.jl")
    (synopsis "Benchmarking framework for the Julia language")
    (description "@code{BenchmarkTools.jl} makes performance tracking of Julia
code easy by supplying a framework for writing and running groups of
benchmarks as well as comparing benchmark results.")
    (license license:expat)))

(define-public julia-blockarrays
  (package
    (name "julia-blockarrays")
    (version "0.15.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/BlockArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15nd493bfkx92ihnr8dj8mb155dj44iqw266igv0qr5q0wad2bfr"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-arraylayouts" ,julia-arraylayouts)
       ("julia-fillarrays" ,julia-fillarrays)))
    (native-inputs
     `(("julia-lazyarrays" ,julia-lazyarrays)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-staticarrays" ,julia-staticarrays)))
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
    (version "0.10.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/BlockBandedMatrices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0q9ni4pgdkb00jb42fdzlhx745852xx2666vr96k0c4l0cn5mi0y"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-arraylayouts" ,julia-arraylayouts)
       ("julia-bandedmatrices" ,julia-bandedmatrices)
       ("julia-blockarrays" ,julia-blockarrays)
       ("julia-fillarrays" ,julia-fillarrays)
       ("julia-matrixfactorizations" ,julia-matrixfactorizations)))
    (home-page "https://github.com/JuliaMatrices/BlockBandedMatrices.jl")
    (synopsis "Block-banded matrices and banded-block-banded matrices")
    (description "This package supports representing block-banded and
banded-block-banded matrices by only storing the entries in the non-zero bands.
A @code{BlockBandedMatrix} is a subtype of @code{BlockMatrix} of
@code{BlockArrays.jl} whose layout of non-zero blocks is banded.")
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
       #:julia-package-name "BufferedStreams"))
    (propagated-inputs `(("julia-compat" ,julia-compat)))
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

(define-public julia-chainrules
  (package
    (name "julia-chainrules")
    (version "0.7.65")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/ChainRules.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0697m5y5ryqnhw6cbk90rlydrxan2b308pzbl86qz4mzhypyk7yi"))))
    (build-system julia-build-system)
    (inputs                             ;required for test
     `(("julia-chainrulestestutils" ,julia-chainrulestestutils)
       ("julia-finitedifferences" ,julia-finitedifferences)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-specialfunctions" ,julia-specialfunctions)))
    (propagated-inputs
     `(("julia-chainrulescore" ,julia-chainrulescore)
       ("julia-compat" ,julia-compat)
       ("julia-reexport" ,julia-reexport)
       ("julia-requires" ,julia-requires)))
    (home-page "https://github.com/JuliaDiff/ChainRules.jl")
    (synopsis "Common utilities for automatic differentiation")
    (description "The is package provides a variety of common utilities that
can be used by downstream automatic differentiation (AD) tools to define and
execute forward-, reverse-, and mixed-mode primitives.")
    (license license:expat)))

(define-public julia-chainrulescore
  (package
    (name "julia-chainrulescore")
    (version "0.9.43")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/ChainRulesCore.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12by6zcxv0ivpf0f22gc9i067360syda9m2lxk0rhypxq4smj8w4"))))
    (build-system julia-build-system)
    (inputs                             ;required for tests
     `(("julia-benchmarktools" ,julia-benchmarktools)
       ("julia-staticarrays" ,julia-staticarrays)))
    (propagated-inputs
     `(("julia-compat" ,julia-compat)))
    (home-page "https://github.com/JuliaDiff/ChainRulesCore.jl")
    (synopsis "Common utilities used by downstream automatic differentiation tools")
    (description "The package provides a light-weight dependency for defining
sensitivities for functions without the need to depend on ChainRules itself.")
    (license license:expat)))

(define-public julia-chainrulestestutils
  (package
    (name "julia-chainrulestestutils")
    (version "0.6.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/ChainRulesTestUtils.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nx2fzxhh3q8znnjfjbgf7776scszixmigwna3hvmdfixsk58x0i"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-chainrulescore" ,julia-chainrulescore)
       ("julia-compat" ,julia-compat)
       ("julia-finitedifference" ,julia-finitedifferences)))
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
     `(("julia-colortypes" ,julia-colortypes)
       ("julia-fixedpointnumbers" ,julia-fixedpointnumbers)
       ("julia-reexport" ,julia-reexport)))
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
    (version "3.12.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/ColorSchemes.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "08k39hbdf3jn0001f7qxa99xvagrnh9764911hs6cmxkvp061sa4"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-colors" ,julia-colors)
       ("julia-colortypes" ,julia-colortypes)
       ("julia-fixedpointnumbers" ,julia-fixedpointnumbers)
       ("julia-staticarrays" ,julia-staticarrays)))
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
    (arguments
     '(#:tests? #f))                    ;require Documenter, not packaged yet
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-fixedpointnumbers" ,julia-fixedpointnumbers)))
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
    (version "0.9.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/ColorVectorSpace.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1gx4k1bvf6lkzxphnvpkw857ihrkifhb68yvsj889k9pf1vda3fq"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-colortypes" ,julia-colortypes)
       ("julia-specialfunctions" ,julia-specialfunctions)
       ("julia-tensorcore" ,julia-tensorcore)))
    (native-inputs
     `(("julia-colors" ,julia-colors)))
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
     `(("julia-macrotools" ,julia-macrotools)))
    (home-page "https://github.com/rdeits/CommonSubexpressions.jl")
    (synopsis "@code{@@cse} macro for Julia")
    (description "This package provides the @code{@@cse} macro, which performs
common subexpression elimination.")
    (license license:expat)))

(define-public julia-compat
  (package
    (name "julia-compat")
    (version "3.29.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaLang/Compat.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00wn28kmzn61fpj3i8f6p987927h9315j9pzzvjpfk5c0ppd1p6q"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaLang/Compat.jl")
    (synopsis "Compatibility across Julia versions")
    (description "The Compat package is designed to ease interoperability
between older and newer versions of the Julia language.  The Compat package
provides a macro that lets you use the latest syntax in a backwards-compatible
way.")
    (license license:expat)))

(define-public julia-constructionbase
  (package
    (name "julia-constructionbase")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaObjects/ConstructionBase.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bmx5c5z9jxmyf2xjwwl5lhs9czmwq4isl0bkr78fak4j8brqr4n"))))
    (build-system julia-build-system)
    (home-page "https://juliaobjects.github.io/ConstructionBase.jl/dev/")
    (synopsis "Primitive functions for construction of objects")
    (description "This very lightweight package provides primitive functions
for construction of objects.")
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
    (version "1.6.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/DataAPI.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "14sfvkz169zcbap3gdwpj16qsap783h86fd07flfxk822abam11w"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaData/DataAPI.jl")
    (synopsis "Data-focused namespace for packages to share functions")
    (description "This package provides a namespace for data-related generic
function definitions to solve the optional dependency problem; packages wishing
to share and/or extend functions can avoid depending directly on each other by
moving the function definition to DataAPI.jl and each package taking a
dependency on it.")
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
     `(("julia-compat" ,julia-compat)
       ("julia-orderedcollections" ,julia-orderedcollections)))
    (build-system julia-build-system)
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
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-known-failing-tests
           (lambda _
             ;; See upstream report:
             ;; https://github.com/queryverse/DataValues.jl/issues/83
             (substitute* "test/array/test_reduce.jl"
               ((".*DataValue\\(mapreduce.*") "")
               ((".*DataValue\\(method\\(f.*") ""))
             #t)))))
    (propagated-inputs
     `(("julia-datavalueinterfaces" ,julia-datavalueinterfaces)))
    (home-page "https://github.com/queryverse/DataValues.jl")
    (synopsis "Missing values for Julia")
    (description "This package provides the type @code{DataValue} that is used
to represent missing data.")
    (license license:expat)))

(define-public julia-dictionaries
  (package
    (name "julia-dictionaries")
    (version "0.3.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/andyferris/Dictionaries.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1j88f6qa5hqm64n5q3jy08a02gwp7by401s03n5x7575p58iqqh2"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-indexing" ,julia-indexing)))
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
         (add-after 'unpack 'skip-flakey-tests
           (lambda _
             (substitute* "test/test_dists.jl"
               (("test dyz ≥") "test_nowarn dyz ≥")
               (("test dist\\(y, x") "test_nowarn dist(y, x")
               (("test dist\\(z, x") "test_nowarn dist(z, x")))))))
    (propagated-inputs
     `(("julia-statsapi" ,julia-statsapi)))
    (native-inputs
     `(("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-unitful" ,julia-unitful)))
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
    (version "0.8.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/DocStringExtensions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1fazv87f0j6hw03frx0gqgq9qpjbddqgccm9998a3329wrrs6gwd"))))
    (build-system julia-build-system)
    (home-page "https://juliadocs.github.io/DocStringExtensions.jl/latest/")
    (synopsis "Extensions for Julia's docsystem")
    (description "This package provides a collection of useful extensions for
Julia's built-in docsystem.  These are features that are not yet mature enough
to be considered for inclusion in Base, or that have sufficiently niche use
cases that including them with the default Julia installation is not seen as
valuable enough at this time.")
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
    (propagated-inputs
     `(("julia-staticarrays" ,julia-staticarrays)))
    (build-system julia-build-system)
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
    (propagated-inputs
     `(("julia-nanmath" ,julia-nanmath)
       ("julia-specialfunctions" ,julia-specialfunctions)))
    (build-system julia-build-system)
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
         (add-after 'unpack 'adjust-test-suite
           (lambda _
             (substitute* "test/runtests.jl"
               ;; Seems to not play nicely with SpecialFunctions
               ((".*isempty.*") "")))))))
    (propagated-inputs
     `(("julia-calculus" ,julia-calculus)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-specialfunctions" ,julia-specialfunctions)))
    (home-page "https://github.com/JuliaDiff/DualNumbers.jl")
    (synopsis "Represent dual numbers and for perform dual algebra")
    (description "The @code{DualNumbers} Julia package defines the @code{Dual}
type to represent dual numbers, and supports standard mathematical operations on
them.  Conversions and promotions are defined to allow performing operations on
combinations of dual numbers with predefined Julia numeric types.")
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

(define-public julia-exprtools
  (package
    (name "julia-exprtools")
    (version "0.1.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/invenia/ExprTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1lwxi9fx9farf1jdv42gv43xs3f3i3js2xnvr5gf6d0xfx0g6b6a"))))
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
    (version "0.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/FFMPEG.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1kwqixwhnnxs59xsw2k44xxnkx5fn4y49g58l5snfbszycxq7lls"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-ffmpeg-jll" ,julia-ffmpeg-jll)
       ("julia-x264-jll" ,julia-x264-jll)))
    (home-page "https://github.com/JuliaIO/FFMPEG.jl")
    (synopsis "Julia Package for ffmpeg")
    (description "This package is made to be included into packages that just
need the ffmpeg binaries + executables, and don't want the overhead of
@code{VideoIO.jl}.")
    (license license:expat)))

(define-public julia-fillarrays
  (package
    (name "julia-fillarrays")
    (version "0.11.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaArrays/FillArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q1qn9pb5dmppddnmf8gggdqyvivqh3ffvbblhn37smcf9r5sy7d"))))
    (build-system julia-build-system)
    (inputs                             ;required by tests
     `(("julia-staticarrays" ,julia-staticarrays)))
    (home-page "https://github.com/JuliaArrays/FillArrays.jl")
    (synopsis "Lazy matrix representation")
    (description "This package lazily represents matrices filled with
a single entry, as well as identity matrices.  This package exports the
following types: @code{Eye}, @code{Fill}, @code{Ones}, @code{Zeros},
@code{Trues} and @code{Falses}.")
    (license license:expat)))

(define-public julia-finitedifferences
  (package
    (name "julia-finitedifferences")
    (version "0.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/FiniteDifferences.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kg8snnspn48i2rr99mwy0an3hzlgrnic7xnh3whj2ws2znw46hr"))))
    (build-system julia-build-system)
    (inputs
     `(("julia-benchmarktools" ,julia-benchmarktools)))
    (propagated-inputs
     `(("julia-chainrulescore" ,julia-chainrulescore)
       ("julia-richardson" ,julia-richardson)
       ("julia-staticarrays" ,julia-staticarrays)))
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
         (add-after 'unpack 'disable-failing-test
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "test/fixed.jl"
               ;; A deprecation warning is not thrown
               (("@test_logs.*:warn" all) (string-append "# " all)))
             #t)))))
    (propagated-inputs `(("julia-compat" ,julia-compat)))
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
    (inputs                             ;required for tests
     `(("julia-calculus" ,julia-calculus)
       ("julia-difftests" ,julia-difftests)))
    (propagated-inputs
     `(("julia-commonsubexpressions" ,julia-commonsubexpressions)
       ("julia-diffresults" ,julia-diffresults)
       ("julia-diffrules" ,julia-diffrules)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-specialfunctions" ,julia-specialfunctions)
       ("julia-staticarrays" ,julia-staticarrays)))
    (home-page "https://github.com/JuliaDiff/ForwardDiff.jl")
    (synopsis "Methods to take multidimensional derivatives")
    (description "This package implements methods to take derivatives,
gradients, Jacobians, Hessians, and higher-order derivatives of native Julia
functions (or any callable object, really) using forward mode automatic
differentiation (AD).")
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
         (add-after 'unpack 'adjust-test-suite
           (lambda _
             (substitute* "test/runtests.jl"
               ((".*lapack.*") "")))))))
    (native-inputs
     `(("julia-quaternions" ,julia-quaternions)))
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
    (version "0.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/RalphAS/GenericSchur.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0kklc2niylvynhq0v49kdmy58m9jmr5jxjf287k1wr9r81fya3sz"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-test-suite
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
     `(("julia-colors" ,julia-colors)
       ("julia-nanmath" ,julia-nanmath)))
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
     `(("julia-abstracttrees" ,julia-abstracttrees)
       ("julia-gumbo-jll" ,julia-gumbo-jll)))
    (home-page "https://github.com/JuliaWeb/Gumbo.jl")
    (synopsis "Julia wrapper around Google's gumbo C library for parsing HTML")
    (description "@code{Gumbo.jl} is a Julia wrapper around Google's gumbo
library for parsing HTML.")
    (license license:expat)))

(define-public julia-http
  (package
    (name "julia-http")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaWeb/HTTP.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ij0yci13c46p92m4zywvcs02nn8pm0abyfffiyhxvva6hq48lyl"))))
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
     `(("julia-inifile" ,julia-inifile)
       ("julia-mbedtls" ,julia-mbedtls)
       ("julia-uris" ,julia-uris)))
    ;; required for tests
    (inputs
     `(("julia-json" ,julia-json)
       ("julia-bufferedstreams" ,julia-bufferedstreams)))
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
     `(("julia-offsetarrays" ,julia-offsetarrays)))
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
     `(("julia-colors" ,julia-colors)
       ("julia-fixedpointnumbers" ,julia-fixedpointnumbers)
       ("julia-mappedarrays" ,julia-mappedarrays)))
    (home-page "https://github.com/JuliaArrays/IndirectArrays.jl")
    (synopsis "Julia implementation of indexed arrays")
    (description "An @code{IndirectArray} is one that encodes data using a
combination of an @code{index} and a @code{value} table.  Each element is
assigned its own index, which is used to retrieve the value from the
@code{value} table.  Among other uses, @code{IndirectArrays} can represent
indexed images, sometimes called \"colormap images\" or \"paletted images.\"")
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
     `(("julia-offsetarrays" ,julia-offsetarrays)))
    (home-page "https://github.com/mbauman/InvertedIndices.jl")
    (synopsis "Index type that allows for inverted selections")
    (description "This package just exports one type: the @code{InvertedIndex},
or @code{Not} for short.  It can wrap any supported index type and may be used
as an index into any @code{AbstractArray} subtype, including OffsetArrays.")
    (license license:expat)))

(define-public julia-iocapture
  (package
    (name "julia-iocapture")
    (version "0.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/IOCapture.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ajlfh8f1g23bx5f8h70nrgr0zfwxaqnpxlka8l4qhjmnfqxl43a"))))
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
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FluxML/IRTools.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wwzy77jcdnffnd5fr6xan7162g4wydz67igrq82wflwnrhlcx5y"))))
    (arguments
     '(#:tests? #f))                    ;require Documenter, not packaged yet
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-macrotools" ,julia-macrotools)))
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
     `(("julia-datastructures" ,julia-datastructures)
       ("julia-fixedpointnumbers" ,julia-fixedpointnumbers)
       ("julia-parsers" ,julia-parsers)
       ("julia-offsetarrays" ,julia-offsetarrays)))
    (home-page "https://github.com/JuliaIO/JSON.jl")
    (synopsis "JSON parsing and printing library for Julia")
    (description "@code{JSON.jl} is a pure Julia module which supports parsing
and printing JSON documents.")
    (license license:expat)))

(define-public julia-lazyarrays
  (package
    (name "julia-lazyarrays")
    (version "0.21.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/LazyArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0zm1ihvi8v2823ap59ajqcs46s5z0nai8l70n51f6j9dzp8lc3cy"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-arraylayouts" ,julia-arraylayouts)
       ("julia-fillarrays" ,julia-fillarrays)
       ("julia-macrotools" ,julia-macrotools)
       ("julia-matrixfactorizations" ,julia-matrixfactorizations)
       ("julia-staticarrays" ,julia-staticarrays)))
    (native-inputs
     `(("julia-tracker" ,julia-tracker)))
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
     `(("julia-docstringextensions" ,julia-docstringextensions)))
    (native-inputs
     `(("julia-offsetarrays" ,julia-offsetarrays)))
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
    (propagated-inputs
     `(("julia-fixedpointnumbers" ,julia-fixedpointnumbers)))
    (native-inputs
     `(("julia-colortypes" ,julia-colortypes)
       ("julia-fixedpointnumbers" ,julia-fixedpointnumbers)
       ("julia-offsetarrays" ,julia-offsetarrays)))
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
    (version "0.8.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/MatrixFactorizations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "14c6w1vhyf4pi4454pdp6ryczsxn9pgjg99fg9bkdj03xg5fsxb8"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-failing-test
           (lambda _
             ;; Tests with math functions are hard.
             (substitute* "test/test_ul.jl"
               (("@test @inferred\\(logdet") "@test @test_nowarn(logdet")
               ;; Also skip the REPL test.
               (("test String") "test_nowarn String"))
             #t)))))
    (propagated-inputs
     `(("julia-arraylayouts" ,julia-arraylayouts)))
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
    (propagated-inputs `(("julia-mbedtls-jll" ,julia-mbedtls-jll)))
    (home-page "https://github.com/JuliaLang/MbedTLS.jl")
    (synopsis "Apache's mbed TLS library wrapper")
    (description "@code{MbedTLS.jl} provides a wrapper around the @code{mbed
TLS} and cryptography C library for Julia.")
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

(define-public julia-missings
  (package
    (name "julia-missings")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/Missings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "131ma44yvswvj85jdjhm37frzfz46cc60lwj65a9jcmgc77dshsm"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-dataapi" ,julia-dataapi)))
    (home-page "https://github.com/JuliaData/Missings.jl")
    (synopsis "Additional missing value support for Julia")
    (description "This package provides additional functionality for working
with @code{missing} values in Julia.")
    (license license:expat)))

(define-public julia-mocking
  (package
    (name "julia-mocking")
    (version "0.7.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/invenia/Mocking.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "10jz716v6i3gpd403rmcrip6cncjl9lqr12cdl321x1994a5g8ck"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-exprtools" ,julia-exprtools)))
    (home-page "https://github.com/invenia/Mocking.jl")
    (synopsis "Overload Julia function calls")
    (description "The purpose of this package is to allow Julia function calls
to be temporarily overloaded for the purpose of testing.")
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
    (version "0.2.19")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jump-dev/MutableArithmetics.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1zjfq5sh0rc941pbc9kvnr6a2lpr4yd276mw62vbncbz9jg52rrg"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-offsetarrays" ,julia-offsetarrays)))
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

(define-public julia-nnlib
  (package
    (name "julia-nnlib")
    (version "0.7.19")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/FluxML/NNlib.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "163v7hkmbxxgnq7qigmqjdqcdywi2njxbh54w8v0hf4bddnalbba"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'drop-cuda-support
           (lambda _
             (substitute* "test/runtests.jl"
               (("using CUDA") ""))
             (setenv "NNLIB_TEST_CUDA" "false"))))))
    (propagated-inputs
     `(("julia-adapt" ,julia-adapt)
       ("julia-chainrulescore" ,julia-chainrulescore)
       ("julia-requires" ,julia-requires)))
    (native-inputs
     `(("julia-chainrulestestutils" ,julia-chainrulestestutils)
       ("julia-stablerngs" ,julia-stablerngs)
       ("julia-zygote" ,julia-zygote)))
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
     `(#:julia-package-name "OptimTestProblems"))
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
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaArrays/OffsetArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s02175pb2pkwg87g7vva2hsrh2ksj9ariw9ccd7axbdm2vd2zcs"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-adapt" ,julia-adapt)))
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
    (arguments
     '(#:tests? #f))                    ;require Documenter, not packaged yet
    (propagated-inputs
     `(("julia-offsetarrays" ,julia-offsetarrays)))
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
     `(("julia-orderedcollections" ,julia-orderedcollections)
       ("julia-unpack" ,julia-unpack)))
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
    (version "0.11.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/PDMats.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1gyhfjmb0qlqgx2398b356cph25bnpjagcslckv41bzyf8pg3ybl"))))
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
     `(("julia-plotutils" ,julia-plotutils)
       ("julia-requires" ,julia-requires)))
    (home-page "https://github.com/JuliaPlots/PlotThemes.jl")
    (synopsis "Themes for the Julia plotting package Plots.jl")
    (description
     "PlotThemes is a package to spice up the plots made with @code{Plots.jl}.")
    (license license:expat)))

(define-public julia-plotutils
  (package
    (name "julia-plotutils")
    (version "1.0.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/PlotUtils.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1jimdkp590g7s33w7i431nn7mp1phjy9gdjs88zyqsmq5hxldacg"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-colors" ,julia-colors)
       ("julia-colorschemes" ,julia-colorschemes)
       ("julia-reexport" ,julia-reexport)))
    (native-inputs
     `(("julia-stablerngs" ,julia-stablerngs)))
    (home-page "https://github.com/JuliaPlots/PlotUtils.jl")
    (synopsis "Helper algorithms for building plotting components")
    (description "This package contains generic helper algorithms for building
plotting components.")
    (license license:expat)))

(define-public julia-pooledarrays
  (package
    (name "julia-pooledarrays")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/PooledArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ihvhzkxdw4qf0i6sbrickhdcwkmlin9zyixxn9xvgzm8nc0iwqy"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-dataapi" ,julia-dataapi)))
    (home-page "https://github.com/JuliaData/PooledArrays.jl")
    (synopsis "Pooled representation of arrays in Julia")
    (description "This package provides a pooled representation of arrays for
purposes of compression when there are few unique elements.")
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
         (add-after 'unpack 'skip-color-tests
           (lambda _
             (substitute* "test/text_backend.jl"
               ((".*colors\\.jl.*") ""))
             #t)))))
    (propagated-inputs
     `(("julia-crayons" ,julia-crayons)
       ("julia-formatting" ,julia-formatting)
       ("julia-reexport" ,julia-reexport)
       ("julia-tables" ,julia-tables)))
    (home-page "https://github.com/ronisbr/PrettyTables.jl")
    (synopsis "Print data in formatted tables")
    (description "This package has the purpose to print data in matrices in a
human-readable format.")
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
         (add-after 'unpack 'hardcode-libmath-location
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcclib (assoc-ref inputs "gcc:lib")))
               (substitute* "src/Quadmath.jl"
                 (("libgcc_s.so.1" lib) (string-append gcclib "/lib/" lib))
                 (("libquadmath.so.0" lib) (string-append gcclib "/lib/" lib)))
               #t))))))
    (propagated-inputs
     `(("julia-requires" ,julia-requires)))
    (inputs
     `(("gcc:lib" ,gcc "lib")))
    (native-inputs
     `(("julia-specialfunctions" ,julia-specialfunctions)))
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
     `(("julia-dualnumbers" ,julia-dualnumbers)))
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
     `(("julia-datastructures" ,julia-datastructures)
       ("julia-iteratorinterfaceextensions" ,julia-iteratorinterfaceextensions)
       ("julia-tableshowutils" ,julia-tableshowutils)))
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

(define-public julia-reexport
  (package
    (name "julia-reexport")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/simonster/Reexport.jl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yhhja1zz6dy5f4fd19bdwd6jwgj7q4w3avzgyg1hjhmdl8jrh0s"))))
    (build-system julia-build-system)
    (home-page "https://github.com/simonster/Reexport.jl")
    (synopsis "Re-export modules and symbols")
    (description "This package provides tools to re-export modules and symbols.")
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
    (inputs                             ;required for test
     `(("julia-example" ,julia-example)))
    (propagated-inputs
     `(("julia-colors" ,julia-colors)))
    (home-page "https://github.com/JuliaPackaging/Requires.jl/")
    (synopsis "Faster package loader")
    (description "This package make loading packages faster, maybe.  It
supports specifying glue code in packages which will load automatically when
another package is loaded, so that explicit dependencies (and long load times)
can be avoided.")
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
  (package
    (name "julia-rotations")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGeometry/Rotations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1l1a3bnrpv9qdksnk3c8j82ik3yrnpsmnyxyiy3gc1yjya5pajqj"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-staticarrays" ,julia-staticarrays)))
    (native-inputs
     `(("julia-benchmarktools" ,julia-benchmarktools)
       ("julia-forwarddiff" ,julia-forwarddiff)
       ("julia-unitful" ,julia-unitful)))
    (home-page "https://github.com/JuliaGeometry/Rotations.jl")
    (synopsis "Julia implementations for different rotation parameterisations")
    (description "This package implements various 3D rotation parameterizations
and defines conversions between them.  At their heart, each rotation
parameterization is a 3×3 unitary (orthogonal) matrix (based on the
@code{StaticArrays.jl} package), and acts to rotate a 3-vector about the origin
through matrix-vector multiplication.")
    (license license:expat)))

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
       `(#:julia-package-name "SafeTestsets"))
      (native-inputs
       `(("julia-staticarrays" ,julia-staticarrays)))
      (home-page "https://github.com/YingboMa/SafeTestsets.jl")
      (synopsis "Julia's testset in a module")
      (description "This package contains the testset from Julia, packaged into
a loadable module.")
      (license license:expat))))

(define-public julia-scratch
  (package
    (name "julia-scratch")
    (version "1.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPackaging/Scratch.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "06n0rc7grlg9igkdlrql83q0zpc97bh2hfzj5mw4spfik8ahw2aa"))))
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

(define-public julia-simpletraits
  (package
    (name "julia-simpletraits")
    (version "0.9.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mauro3/SimpleTraits.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1aaq91klwxb0r3ckwihf7rd7jvg70757nkhp5ip4cc3lspnnip23"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-macrotools" ,julia-macrotools)))
    (home-page "https://github.com/mauro3/SimpleTraits.jl")
    (synopsis "Simple Traits for Julia")
    (description "This package provides a macro-based implementation of traits.
The main idea behind traits is to group types outside the type-hierarchy and to
make dispatch work with that grouping.  The difference to Union-types is that
types can be added to a trait after the creation of the trait, whereas Union
types are fixed after creation.")
    (license license:expat)))

(define-public julia-sortingalgorithms
  (package
    (name "julia-sortingalgorithms")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaCollections/SortingAlgorithms.jl")
               ;; Tagging releases is hard:
               ;; https://github.com/JuliaCollections/SortingAlgorithms.jl/issues/41#issuecomment-840587380
               (commit "aa2b98d384ddd132aae0219e68fb63b92513cb35")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "13zbx18psxrg4fvkqgp0m7g484vrama2xm6902bbls30801hgljg"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; cycle with StatsBase.jl
    (propagated-inputs
     `(("julia-datastructures" ,julia-datastructures)))
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
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/SpecialFunctions.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rfhrrkzi3ils7fklbn35ki1yp5x88fi71qknfwqyw4pk8cf8p80"))))
    (build-system julia-build-system)
    (inputs
     `(("julia-chainrulestestutils" ,julia-chainrulestestutils)))
    (propagated-inputs
     `(("julia-chainrulescore" ,julia-chainrulescore)
       ("julia-openspecfun-jll" ,julia-openspecfun-jll)))
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
     `(("julia-dictionaries" ,julia-dictionaries)
       ("julia-indexing" ,julia-indexing)))
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

(define-public julia-static
  (package
    (name "julia-static")
    (version "0.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SciML/Static.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "01rbiysrkaca03gh55rc5zykkp63bhzaqgrxxj88lnisrbzmf0d2"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-ifelse" ,julia-ifelse)))
    (native-inputs
     `(("julia-aqua" ,julia-aqua)))
    (home-page "https://github.com/SciML/Static.jl")
    (synopsis "Static types useful for dispatch and generated functions")
    (description "Static.jl defines a limited set of statically parameterized
types and a common interface that is shared between them.")
    (license license:expat)))

(define-public julia-staticarrays
  (package
    (name "julia-staticarrays")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaArrays/StaticArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z4g1kk6gy514dyafb559gcp4264ffx6h28ffczdvkyk8gm9j0m7"))))
    (build-system julia-build-system)
    (inputs
     `(("julia-benchmarktools" ,julia-benchmarktools)))
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
     `(("julia-dataapi" ,julia-dataapi)
       ("julia-datastructures" ,julia-datastructures)
       ("julia-missings" ,julia-missings)
       ("julia-sortingalgorithms" ,julia-sortingalgorithms)
       ("julia-statsapi" ,julia-statsapi)))
    (native-inputs
     `(("julia-stablerngs" ,julia-stablerngs)))
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
     `(("julia-distances" ,julia-distances)))
    (home-page "https://github.com/matthieugomez/StringDistances.jl")
    (synopsis "String Distances in Julia")
    (description "This package provides string distances in Julia.  Distances
are defined for @code{AbstractStrings}, and any iterator that define
@code{length()}.  The package also defines Distance \"modifiers\" that can be
applied to any distance.")
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
    (version "1.4.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/Tables.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0yfjl4v1vglsk9wr7gbqgya4kk3a0q0i6zhi9xdgvnqsqzqrsc7c"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-dataapi" ,julia-dataapi)
       ("julia-datavalueinterfaces" ,julia-datavalueinterfaces)
       ("julia-iteratorinterfaceextensions" ,julia-iteratorinterfaceextensions)
       ("julia-tabletraits" ,julia-tabletraits)))
    (native-inputs
     `(("julia-datavalues" ,julia-datavalues)
       ("julia-queryoperators" ,julia-queryoperators)))
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
       `(("julia-datavalues" ,julia-datavalues)
         ("julia-json" ,julia-json)))
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
     `(("julia-iteratorinterfaceextensions" ,julia-iteratorinterfaceextensions)))
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
         (base32 "1s4mdywbp7nli7z985fqaj1rs4i6d92b1jx3lhg0qhk1s5wc0v8j"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-adapt" ,julia-adapt)
       ("julia-diffrules" ,julia-diffrules)
       ("julia-forwarddiff" ,julia-forwarddiff)
       ("julia-macrotools" ,julia-macrotools)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-nnlib" ,julia-nnlib)
       ("julia-requires" ,julia-requires)
       ("julia-specialfunctions" ,julia-specialfunctions)))
    (native-inputs
     `(("julia-pdmats" ,julia-pdmats)))
    (home-page "https://github.com/FluxML/Tracker.jl")
    (synopsis "Operator overloading reverse-mode automatic differentiator")
    (description "@code{Tracker.jl} previously provided @code{Flux.jl} with
automatic differentiation for its machine learning platform.")
    (license license:expat)))

(define-public julia-typedtables
  (package
    (name "julia-typedtables")
    (version "1.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/TypedTables.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "06k5h7ybsh29b6kiy0p62rp9b2q3xi9jk8p9wf0kq907p5kvfnfy"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-adapt" ,julia-adapt)
       ("julia-splitapplycombine" ,julia-splitapplycombine)
       ("julia-tables" ,julia-tables)))
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
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'change-dir
           ;; Tests must be run from the testdir
           (lambda* (#:key source outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (chdir
                (string-append out "/share/julia/packages/URIs/test")))
             #t)))))
    ;; required for tests
    (inputs `(("julia-json" ,julia-json)))
    (home-page "https://github.com/JuliaWeb/URIs.jl")
    (synopsis "URI parsing in Julia")
    (description "@code{URIs.jl} is a Julia package that allows parsing and
working with @acronym{URIs,Uniform Resource Identifiers}, as defined in RFC
3986.")
    (license license:expat)))

(define-public julia-unitful
  (package
    (name "julia-unitful")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PainterQubits/Unitful.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03nq2nc2mwiqg3z1gksfsnyd7dmsjsya5c2v1n5h0ab22vm59f0m"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-constructionbase" ,julia-constructionbase)))
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
    (version "0.6.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/WeakRefStrings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0129mf1axxycb1ans3idlvw8ch0hmdrl80q98jw63f99zz3ddipr"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-dataapi" ,julia-dataapi)))
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
    (home-page "https://github.com/timholy/WoodburyMatrices.jl")
    (synopsis "Support for the Woodbury matrix identity for Julia")
    (description "This package provides support for the Woodbury matrix identity
for the Julia programming language.  This is a generalization of the
Sherman-Morrison formula.  Note that the Woodbury matrix identity is notorious
for floating-point roundoff errors, so be prepared for a certain amount of
inaccuracy in the result.")
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
     `(("julia-zlib-jll" ,julia-zlib-jll)))
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
     `(("julia-macrotools" ,julia-macrotools)))
    (home-page "https://github.com/FluxML/ZygoteRules.jl")
    (synopsis "Add minimal custom gradients to Zygote")
    (description "Minimal package which enables to add custom gradients to
Zygote, without depending on Zygote itself.")
    (license license:expat)))

(define-public julia-zygote
  (package
    (name "julia-zygote")
    (version "0.6.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FluxML/Zygote.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qqjzhiqghj07mab24b0n0h4xfzq8r1s9hccrmx1lwjkkskhc5f9"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))                    ;require CUDA, not packaged yet
    (propagated-inputs
     `(("julia-abstractffs" ,julia-abstractffts)
       ("julia-chainrules" ,julia-chainrules)
       ("julia-diffrules" ,julia-diffrules)
       ("julia-fillarrays" ,julia-fillarrays)
       ("julia-forwarddiff" ,julia-forwarddiff)
       ("julia-irtools" ,julia-irtools)
       ("julia-macrotools" ,julia-macrotools)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-requires" ,julia-requires)
       ("julia-specialfunctions" ,julia-specialfunctions)
       ("julia-zygote-rules" ,julia-zygoterules)))
    (home-page "https://fluxml.ai/Zygote.jl")
    (synopsis "Automatic differentiation in Julia")
    (description "Zygote provides source-to-source automatic
differentiation (AD) in Julia, and is the next-generation AD system for the
Flux differentiable programming framework.")
    (license license:expat)))
