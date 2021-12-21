;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018-2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Dan Frumin <dfrumin@cs.ru.nl>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
;;; Copyright © 2020 Robin Green <greenrd@greenrd.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (gnu packages coq)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system dune)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((srfi srfi-1) #:hide (zip)))

(define-public coq-core
  (package
    (name "coq-core")
    (version "8.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coq/coq")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0q634fmrh5ivpxy7ammdiqdh9xp72ji2726vcz1bd55ln0d08z40"))
       (patches (search-patches "coq-fix-envvars.patch"))))
    (native-search-paths
     (list (search-path-specification
            (variable "COQPATH")
            (files (list "lib/ocaml/site-lib/coq/user-contrib"
                         "lib/coq/user-contrib")))
           (search-path-specification
            (variable "COQLIBPATH")
            (files (list "lib/ocaml/site-lib/coq")))
           (search-path-specification
            (variable "COQCORELIB")
            (files (list "lib/ocaml/site-lib/coq-core"))
            (separator #f))))
    (build-system dune-build-system)
    (inputs
     (list gmp ocaml-zarith))
    (native-inputs
     (list ocaml-ounit2 which))
    (arguments
     `(#:package "coq-core"
       #:test-target "."))
    (properties '((upstream-name . "coq"))) ; also for inherited packages
    (home-page "https://coq.inria.fr")
    (synopsis "Proof assistant for higher-order logic")
    (description
     "Coq is a proof assistant for higher-order logic, which allows the
development of computer programs consistent with their formal specification.
It is developed using Objective Caml and Camlp5.")
    ;; The source code is distributed under lgpl2.1.
    ;; Some of the documentation is distributed under opl1.0+.
    (license (list license:lgpl2.1 license:opl1.0+))))

(define-public coq-stdlib
  (package
    (inherit coq-core)
    (name "coq-stdlib")
    (arguments
     `(#:package "coq-stdlib"
       #:test-target "."))
    (inputs
     (list coq-core gmp ocaml-zarith))
    (native-inputs '())))

(define-public coq
  (package
    (inherit coq-core)
    (name "coq")
    (arguments
     `(#:package "coq"
       #:test-target "."))
    (propagated-inputs
     (list coq-core coq-stdlib))
    (native-inputs '())))

(define-public coq-ide-server
  (package
    (inherit coq)
    (name "coq-ide-server")
    (arguments
     `(#:tests? #f
       #:package "coqide-server"))
    (inputs
     (list coq gmp ocaml-zarith))))

(define-public coq-ide
  (package
    (inherit coq)
    (name "coq-ide")
    (arguments
     `(#:tests? #f
       #:package "coqide"))
    (propagated-inputs
     (list coq coq-ide-server))
    (inputs
     `(("lablgtk3" ,lablgtk3)))))

(define-public proof-general
  ;; The latest release is from 2016 and there has been more than 450 commits
  ;; since then.
  ;; Commit from 2021-11-25.
  (let ((commit "1b1083e86e0cddc20ff2f1a6b25c7a7eee2edf02")
        (revision "1"))
    (package
      (name "proof-general")
      (version (git-version "4.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ProofGeneral/PG")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pnysczhscapgwmvf6ix7f31lf3hnh8h977bfll1m7jlxl9b9c0j"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("emacs" ,emacs-minimal)
         ("texinfo" ,texinfo)))
      (inputs
       (list perl))
      (arguments
       (let ((base-directory "/share/emacs/site-lisp/ProofGeneral"))
         `(#:tests? #f                  ; no check target
           #:make-flags (list (string-append "PREFIX=" %output)
                              (string-append "EMACS=" (assoc-ref %build-inputs "emacs")
                                             "/bin/emacs")
                              (string-append "DEST_PREFIX=" %output)
                              (string-append "ELISP=" %output ,base-directory)
                              (string-append "DEST_ELISP=" %output ,base-directory)
                              (string-append "ELISP_START=" %output ,base-directory))
           #:phases
           (modify-phases %standard-phases
             (delete 'configure)
             (add-after 'unpack 'disable-byte-compile-error-on-warn
               (lambda _
                 (substitute* "Makefile"
                   (("\\(setq byte-compile-error-on-warn t\\)")
                    "(setq byte-compile-error-on-warn nil)"))))
             (add-after 'unpack 'patch-hardcoded-paths
               (lambda _
                 (substitute* "Makefile"
                   (("/sbin/install-info") "install-info"))))
             (add-after 'unpack 'remove-which
               (lambda _
                 (substitute* "Makefile"
                   (("`which perl`") "perl")
                   (("`which bash`") "bash"))))
             (add-after 'unpack 'clean
               (lambda _
                 ;; Delete the pre-compiled elc files for Emacs 23.
                 (invoke "make" "clean")))
             (add-after 'install 'install-doc
               (lambda* (#:key make-flags #:allow-other-keys)
                 ;; XXX FIXME avoid building/installing pdf files,
                 ;; due to unresolved errors building them.
                 (substitute* "Makefile"
                   ((" [^ ]*\\.pdf") ""))
                 (apply invoke "make" "install-doc" make-flags)))
             (add-after 'install 'allow-subfolders-autoloads
               ;; Autoload cookies are present in sub-directories.  A friendly
               ;; wrapper proof-general.el around generic/proof-site.el is
               ;; provided for execution on Emacs start-up.  It serves two
               ;; purposes:
               ;;
               ;; * Setting up the load path when byte-compiling pg.
               ;; * Loading a minimal PG setup on startup (not all of Proof
               ;; General, of course; mostly mode hooks and autoloads).
               ;;
               ;; The renaming to proof-general-autoloads.el is Guix
               ;; specific.
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (copy-file "proof-general.el"
                              (string-append out ,base-directory
                                             "/proof-general-autoloads.el")))))))))
      (home-page "https://proofgeneral.github.io/")
      (synopsis "Generic front-end for proof assistants based on Emacs")
      (description
       "Proof General is a major mode to turn Emacs into an interactive proof
assistant to write formal mathematical proofs using a variety of theorem
provers.")
      (license license:gpl3+))))

(define-public coq-flocq
  (package
    (name "coq-flocq")
    (version "3.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/flocq/flocq.git")
             (commit (string-append "flocq-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j7vq7ifqcdaj2x881aha2rl51l2p72y1cn7r2xya0fjgsssfigy"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake ocaml which coq))
    (arguments
     `(#:configure-flags
       (list (string-append "COQUSERCONTRIB=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-remake
           (lambda _
             (substitute* "remake.cpp"
               (("/bin/sh") (which "sh")))
             #t))
         (replace 'build
           (lambda _
             (invoke "./remake")))
         (replace 'check
           (lambda _
             (invoke "./remake" "check")))
             ;; TODO: requires coq-gappa and coq-interval.
             ;(invoke "./remake" "check-more")
         (replace 'install
           (lambda _
             (invoke "./remake" "install"))))))
    (home-page "https://flocq.gitlabpages.inria.fr")
    (synopsis "Floating-point formalization for the Coq system")
    (description "Flocq (Floats for Coq) is a floating-point formalization for
the Coq system.  It provides a comprehensive library of theorems on a multi-radix
multi-precision arithmetic.  It also supports efficient numerical computations
inside Coq.")
    (license license:lgpl3+)))

(define-public coq-gappa
  (package
    (name "coq-gappa")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/gappa/coq.git")
             (commit (string-append "gappalib-coq-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ivh8xm1c8191rm4riamjzya2x6ls96qax5byir1fywf9hbxr1vg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           ocaml
           which
           coq
           camlp5
           bison
           flex))
    (inputs
     (list gmp mpfr ocaml-zarith boost))
    (propagated-inputs
     (list coq-flocq))
    (arguments
     `(#:configure-flags
       (list (string-append "COQUSERCONTRIB=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-remake
           (lambda _
             (substitute* "remake.cpp"
               (("/bin/sh") (which "sh")))
             #t))
         (replace 'build
           (lambda _ (invoke "./remake")))
         ;; FIXME: Figure out why failures occur, and re-enable check phase.
         (delete 'check)
         ;; (replace 'check
         ;;   (lambda _ (invoke "./remake" "check")))
         (replace 'install
           (lambda _ (invoke "./remake" "install"))))))
    (home-page "https://gappa.gitlabpages.inria.fr/")
    (synopsis "Verify and formally prove properties on numerical programs")
    (description "Gappa is a tool intended to help verifying and formally proving
properties on numerical programs dealing with floating-point or fixed-point
arithmetic.  It has been used to write robust floating-point filters for CGAL
and it is used to certify elementary functions in CRlibm.  While Gappa is
intended to be used directly, it can also act as a backend prover for the Why3
software verification plateform or as an automatic tactic for the Coq proof
assistant.")
    (license (list license:gpl2+ license:cecill))));either gpl2+ or cecill

(define-public coq-mathcomp
  (package
    (name "coq-mathcomp")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/math-comp/math-comp")
             (commit (string-append "mathcomp-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0aj8hsdzzds5w0p1858s2b6k9zssjcxa6kgpi0q1nvaml4zfpkcc"))))
    (build-system gnu-build-system)
    (native-inputs
     (list ocaml which coq))
    (arguments
     `(#:tests? #f ; No tests.
       #:make-flags (list (string-append "COQLIBINSTALL="
                                         (assoc-ref %outputs "out")
                                         "/lib/coq/user-contrib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'chdir
           (lambda _ (chdir "mathcomp") #t)))))
    (home-page "https://math-comp.github.io/")
    (synopsis "Mathematical Components for Coq")
    (description "Mathematical Components for Coq has its origins in the formal
proof of the Four Colour Theorem.  Since then it has grown to cover many areas
of mathematics and has been used for large scale projects like the formal proof
of the Odd Order Theorem.

The library is written using the Ssreflect proof language that is an integral
part of the distribution.")
    (license license:cecill-b)))

(define-public coq-coquelicot
  (package
    (name "coq-coquelicot")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/coquelicot/coquelicot.git")
             (commit (string-append "coquelicot-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "146s5y2xsc7wb43m1pq1n4p14hw99gqbzx0ic3a4naxq16v7cv4w"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake ocaml which coq))
    (propagated-inputs
     `(("mathcomp" ,coq-mathcomp)))
    (arguments
     `(#:configure-flags
       (list (string-append "COQUSERCONTRIB=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-remake
           (lambda _
             (substitute* "remake.cpp"
               (("/bin/sh") (which "sh")))
             #t))
         (replace 'build
           (lambda _ (invoke "./remake")))
         (replace 'check
           (lambda _ (invoke "./remake" "check")))
         (replace 'install
           (lambda _ (invoke "./remake" "install"))))))
    (home-page "http://coquelicot.saclay.inria.fr")
    (synopsis "Coq library for Reals")
    (description "Coquelicot is an easier way of writing formulas and theorem
statements, achieved by relying on total functions in place of dependent types
for limits, derivatives, integrals, power series, and so on.  To help with the
proof process, the library comes with a comprehensive set of theorems that cover
not only these notions, but also some extensions such as parametric integrals,
two-dimensional differentiability, asymptotic behaviors.  It also offers some
automations for performing differentiability proofs.  Moreover, Coquelicot is a
conservative extension of Coq's standard library and provides correspondence
theorems between the two libraries.")
    (license license:lgpl3+)))

(define-public coq-bignums
  (package
    (name "coq-bignums")
    (version "8.14.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/coq/bignums")
                     (commit (string-append "V" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jsgdvj0ddhkls32krprp34r64y1rb5mwxl34fgaxk2k4664yq06"))))
    (build-system gnu-build-system)
    (native-inputs
     (list ocaml coq))
    (inputs
     (list camlp5 ocaml-zarith))
    (arguments
     `(#:tests? #f ; No test target.
       #:make-flags
       (list (string-append "COQLIBINSTALL=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/coq/bignums")
    (synopsis "Coq library for arbitrary large numbers")
    (description "Bignums is a coq library of arbitrary large numbers.  It
provides BigN, BigZ, BigQ that used to be part of Coq standard library.")
    (license license:lgpl2.1+)))

(define-public coq-interval
  (package
    (name "coq-interval")
    (version "4.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/coqinterval/interval.git")
             (commit (string-append "interval-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0sr9psildc0sda07r2r47rfgyry49yklk38bg04yyvry5j5pryb6"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake ocaml which coq))
    (propagated-inputs
     `(("flocq" ,coq-flocq)
       ("bignums" ,coq-bignums)
       ("coquelicot" ,coq-coquelicot)
       ("mathcomp" ,coq-mathcomp)
       ("ocaml-zarith" ,ocaml-zarith)))
    (arguments
     `(#:configure-flags
       (list (string-append "COQUSERCONTRIB=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-remake
           (lambda _
             (substitute* "remake.cpp"
               (("/bin/sh") (which "sh")))
             #t))
         (replace 'build
           (lambda _ (invoke "./remake")))
         (replace 'check
           (lambda _ (invoke "./remake" "check")))
         (replace 'install
           (lambda _ (invoke "./remake" "install"))))))
    (home-page "https://coqinterval.gitlabpages.inria.fr/")
    (synopsis "Coq tactics to simplify inequality proofs")
    (description "Interval provides vernacular files containing tactics for
simplifying the proofs of inequalities on expressions of real numbers for the
Coq proof assistant.")
    (license license:cecill-c)))

(define-public coq-autosubst
  ;; Latest commit on that branch, where work on supporting coq 8.6 and
  ;; more recent versions of coq happen.
  (let ((branch "coq86-devel")
        (commit "fa6ef30664511ffa659cbcf3c962715cbee03572"))
    (package
      (name "coq-autosubst")
      (version (git-version "1" branch commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://github.com/uds-psl/autosubst")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1cl0bp96bk6lplbl7n5c703vd3gvbs5mvf2qrf8q333kkqd7jqq4"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
       #:make-flags (list (string-append "COQLIBINSTALL="
                                         (assoc-ref %outputs "out")
                                         "/lib/coq/user-contrib"))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))
      (native-inputs
       (list coq))
      (home-page "https://www.ps.uni-saarland.de/autosubst/")
      (synopsis "Coq library for parallel de Bruijn substitutions")
      (description "Formalizing syntactic theories with variable binders is
not easy.  Autosubst is a library for the Coq proof assistant to
automate this process.  Given an inductive definition of syntactic objects in
de Bruijn representation augmented with binding annotations, Autosubst
synthesizes the parallel substitution operation and automatically proves the
basic lemmas about substitutions.  This library contains an automation
tactic that solves equations involving terms and substitutions.  This makes the
usage of substitution lemmas unnecessary.  The tactic is based on our current
work on a decision procedure for the equational theory of an extension of the
sigma-calculus by Abadi et al.  The library is completely written in Coq and
uses Ltac to synthesize the substitution operation.")
      (license license:bsd-3))))

(define-public coq-equations
  (package
    (name "coq-equations")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mattam82/Coq-Equations")
                    (commit (string-append "v" version "-8.14"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19bj9nncd1r9g4273h5qx35gs3i4bw5z9bhjni24b413hyj55hkv"))))
    (build-system gnu-build-system)
    (native-inputs
     (list ocaml coq camlp5))
    (inputs
     (list ocaml-zarith))
    (arguments
     `(#:test-target "test-suite"
       #:make-flags (list (string-append "COQLIBINSTALL="
                                         (assoc-ref %outputs "out")
                                         "/lib/coq/user-contrib"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "sh" "./configure.sh"))))))
    (home-page "https://mattam82.github.io/Coq-Equations/")
    (synopsis "Function definition plugin for Coq")
    (description "Equations provides a notation for writing programs
by dependent pattern-matching and (well-founded) recursion in Coq.  It
compiles everything down to eliminators for inductive types, equality
and accessibility, providing a definitional extension to the Coq
kernel.")
    (license license:lgpl2.1)))

(define-public coq-semantics
  (package
    (name "coq-semantics")
    (version "8.14.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/coq-community/semantics")
              (commit (string-append "v" version))))
        (modules '((guix build utils)))
        (snippet
         '(substitute* "Makefile.coq.local"
            ;; Num was part of OCaml and now external
            (("-libs nums") "-use-ocamlfind -pkg num -libs num")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0ldrp86bfcjpzsb08p45sgs3aczjzr1gksy5dsf7pxapg05pc7ac"))))
    (build-system gnu-build-system)
    (native-inputs
     (list coq ocaml ocamlbuild ocaml-findlib))
    (inputs
     (list ocaml-num))
    (arguments
     `(#:tests? #f                      ;included in Makefile
       #:make-flags (list (string-append "COQLIBINSTALL="
                                         (assoc-ref %outputs "out")
                                         "/lib/coq/user-contrib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/coq-community/semantics")
    (synopsis "Survey of semantics styles")
    (description
     "This package provides a survey of programming language semantics styles,
from natural semantics through structural operational, axiomatic, and
denotational semantics, for a miniature example of an imperative programming
language.  Their encoding, the proofs of equivalence of different styles,
abstract interpretation, and the proof of soundess obtained from axiomatic
semantics or abstract interpretation is done in Coq.  The tools can be run
inside Coq, thus making them available for proof by reflection.  Code can also
be extracted and connected to a yacc-based parser, thanks to the use of a
functor parameterized by a module type of strings.  A hand-written parser is
also provided in Coq, without associated proofs.")
    (license license:expat)))

(define-public coq-stdpp
  (package
    (name "coq-stdpp")
    (version "1.6.0")
    (synopsis "Alternative Coq standard library std++")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.mpi-sws.org/iris/stdpp.git")
                    (commit (string-append "coq-stdpp-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l1w6srzydjg0h3f4krrfgvz455h56shyy2lbcnwdbzjkahibl7v"))))
    (build-system gnu-build-system)
    (inputs
     (list coq))
    (arguments
     `(#:tests? #f ; Tests are executed during build phase.
       #:make-flags (list (string-append "COQLIBINSTALL="
                                         (assoc-ref %outputs "out")
                                         "/lib/coq/user-contrib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (description "This project contains an extended \"Standard Library\" for
Coq called coq-std++.  The key features are:
@itemize
@item Great number of definitions and lemmas for common data structures such
as lists, finite maps, finite sets, and finite multisets.

@item Type classes for common notations (like ∅, ∪, and Haskell-style monad
notations) so that these can be overloaded for different data structures.

@item It uses type classes to keep track of common properties of types, like
it having decidable equality or being countable or finite.

@item Most data structures are represented in canonical ways so that Leibniz
equality can be used as much as possible (for example, for maps we have m1 =
m2 iff ∀ i, m1 !! i = m2 !! i).  On top of that, the library provides setoid
instances for most types and operations.

@item Various tactics for common tasks, like an ssreflect inspired done tactic
for finishing trivial goals, a simple breadth-first solver naive_solver, an
equality simplifier simplify_eq, a solver solve_proper for proving
compatibility of functions with respect to relations, and a solver set_solver
for goals involving set operations.

@item The library is dependency- and axiom-free.
@end itemize")
    (home-page "https://gitlab.mpi-sws.org/iris/stdpp")
    (license license:bsd-3)))
