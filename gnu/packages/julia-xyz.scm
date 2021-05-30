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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
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

(define-public julia-imagemagick-jll
  (package
    (name "julia-imagemagick-jll")
    (version "6.9.10-12+3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/ImageMagick_jll.jl")
               (commit (string-append "ImageMagick-v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1a0vnnjl52yqi7jgpr0dmx5ah5nlpylv5hc8aw2l89b9f36ff8jg"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f ; no runtests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
               (lambda (wrapper)
                 (substitute* wrapper
                   ;; Make sure we match the current library.
                   (("libMagickWand-6.Q16.so.6") "libMagickWand-6.Q16.so.7")
                   (("artifact\"ImageMagick\"")
                    (string-append "\"" (assoc-ref inputs "imagemagick") "\""))))
               ;; There's a Julia file for each platform, override them all
               (find-files "src/wrappers/" "\\.jl$")))))))
    (propagated-inputs
     `(("julia-jllwrappers" ,julia-jllwrappers)
       ("julia-jpegturbo-jll" ,julia-jpegturbo-jll)
       ("julia-libpng-jll" ,julia-libpng-jll)
       ("julia-libtiff-jll" ,julia-libtiff-jll)
       ("julia-zlib-jll" ,julia-zlib-jll)))
    (inputs
     `(("imagemagick" ,imagemagick)))
    (home-page "https://github.com/JuliaBinaryWrappers/ImageMagick_jll.jl")
    (synopsis "Imagemagick library wrappers")
    (description "This package provides a wrapper for Imagemagick.")
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

(define-public julia-libpng-jll
  (package
    (name "julia-libpng-jll")
    (version "1.6.37+5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/LibPNG_jll.jl")
               (commit (string-append "libpng-v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "10azxv26j8r193dg9i9x7ajm3frhwbhj8zzi40fj59fxm81xz6dx"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f  ; no runtests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
               (lambda (wrapper)
                 (substitute* wrapper
                   (("artifact\"libpng\"")
                    (string-append "\"" (assoc-ref inputs "libpng") "\""))))
               ;; There's a Julia file for each platform, override them all
               (find-files "src/wrappers/" "\\.jl$")))))))
    (inputs
     `(("libpng" ,libpng)))
    (propagated-inputs
     `(("julia-jllwrappers" ,julia-jllwrappers)
       ("julia-zlib-jll" ,julia-zlib-jll)))
    (home-page "https://github.com/JuliaBinaryWrappers/LibPNG_jll.jl")
    (synopsis "Libpng library wrappers")
    (description "This package provides a wrapper for the libpng library.")
    (license license:expat)))

(define-public julia-libtiff-jll
  (package
    (name "julia-libtiff-jll")
    (version "4.1.0+1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/Libtiff_jll.jl")
               (commit (string-append "Libtiff-v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "07zzhmwmh2g4645ghv76z40hza2ghlb7sw15b1pii7f9kfcsgf45"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f  ; no runtests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
               (lambda (wrapper)
                 (substitute* wrapper
                   (("artifact\"Libtiff\"")
                    (string-append "\"" (assoc-ref inputs "libtiff") "\""))))
               ;; There's a Julia file for each platform, override them all
               (find-files "src/wrappers/" "\\.jl$")))))))
    (inputs
     `(("libtiff" ,libtiff)))
    (propagated-inputs
     `(("julia-jllwrappers" ,julia-jllwrappers)
       ("julia-jpegturbo-jll" ,julia-jpegturbo-jll)
       ("julia-zlib-jll" ,julia-zlib-jll)
       ("julia-zstd-jll" ,julia-zstd-jll)))
    (home-page "https://github.com/JuliaBinaryWrappers/Libtiff_jll.jl")
    (synopsis "Libtiff library wrappers")
    (description "This package provides a wrapper for libtiff")
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
    (description "This package containts generic helper algorithms for building
plotting components.")
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

(define-public julia-zstd-jll
  (package
    (name "julia-zstd-jll")
    (version "1.5.0+0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/Zstd_jll.jl")
               (commit (string-append "Zstd-v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "15g5wsvga4p9bjmx97xqwqdnfzfbwfl6c4a9iaphcncwgcrnw6y6"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f  ; no runtests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
               (lambda (wrapper)
                 (substitute* wrapper
                   (("generate_wrapper_header.*")
                    (string-append
                      "generate_wrapper_header(\"Zstd\", \""
                      (assoc-ref inputs "zstd:lib") "\")\n"))))
               ;; There's a Julia file for each platform, override them all
               (find-files "src/wrappers/" "\\.jl$")))))))
    (inputs
     `(("zstd:lib" ,zstd "lib")))
    (propagated-inputs
     `(("julia-jllwrappers" ,julia-jllwrappers)))
    (home-page "https://github.com/JuliaBinaryWrappers/Zstd_jll.jl")
    (synopsis "Zstd library wrappers")
    (description "This package provides a wrapper for the zstd library.")
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
