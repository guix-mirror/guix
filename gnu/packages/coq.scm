;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Dan Frumin <dfrumin@cs.ru.nl>
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
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((srfi srfi-1) #:hide (zip)))

(define-public coq
  (package
    (name "coq")
    (version "8.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coq/coq.git")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01ad7az6f95w16xya7979lk32agy22lf4bqgqf5qpnarpkpxhbw8"))))
    (native-search-paths
     (list (search-path-specification
            (variable "COQPATH")
            (files (list "lib/coq/user-contrib")))))
    (build-system ocaml-build-system)
    (inputs
     `(("lablgtk" ,lablgtk)
       ("python" ,python-2)
       ("camlp5" ,camlp5)
       ("ocaml-num" ,ocaml-num)))
    (native-inputs
     `(("ocaml-ounit" ,ocaml-ounit)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (mandir (string-append out "/share/man"))
                    (browser "icecat -remote \"OpenURL(%s,new-tab)\""))
               (invoke "./configure"
                       "-prefix" out
                       "-mandir" mandir
                       "-browser" browser
                       "-coqide" "opt"))))
         (replace 'build
           (lambda _
             (invoke "make"
                     "-j" (number->string (parallel-job-count))
                     "world")))
         (delete 'check)
         (add-after 'install 'check
           (lambda _
             (with-directory-excursion "test-suite"
               ;; These two tests fail.
               ;; This one fails because the output is not formatted as expected.
               (delete-file-recursively "coq-makefile/timing")
               ;; This one fails because we didn't build coqtop.byte.
               (delete-file-recursively "coq-makefile/findlib-package")
               (invoke "make")))))))
    (home-page "https://coq.inria.fr")
    (synopsis "Proof assistant for higher-order logic")
    (description
     "Coq is a proof assistant for higher-order logic, which allows the
development of computer programs consistent with their formal specification.
It is developed using Objective Caml and Camlp5.")
    ;; The code is distributed under lgpl2.1.
    ;; Some of the documentation is distributed under opl1.0+.
    (license (list license:lgpl2.1 license:opl1.0+))))

(define-public proof-general
  (package
    (name "proof-general")
    (version "4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://proofgeneral.inf.ed.ac.uk/releases/"
                    "ProofGeneral-" version ".tgz"))
              (sha256
               (base32
                "09qb0myq66fw17v4ziz401ilsb5xlxz1nl2wsp69d0vrfy0bcrrm"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)
       ("emacs" ,emacs-minimal)
       ("texinfo" ,texinfo)))
    (inputs
     `(("host-emacs" ,emacs)
       ("perl" ,perl)
       ("coq" ,coq)))
    (arguments
     `(#:tests? #f  ; no check target
       #:make-flags (list (string-append "PREFIX=" %output)
                          (string-append "DEST_PREFIX=" %output))
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'disable-byte-compile-error-on-warn
                    (lambda _
                      (substitute* "Makefile"
                        (("\\(setq byte-compile-error-on-warn t\\)")
                         "(setq byte-compile-error-on-warn nil)"))
                      #t))
         (add-after 'unpack 'patch-hardcoded-paths
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out   (assoc-ref outputs "out"))
                            (coq   (assoc-ref inputs "coq"))
                            (emacs (assoc-ref inputs "host-emacs")))
                        (define (coq-prog name)
                          (string-append coq "/bin/" name))
                        (emacs-substitute-variables "coq/coq.el"
                          ("coq-prog-name"           (coq-prog "coqtop"))
                          ("coq-compiler"            (coq-prog "coqc"))
                          ("coq-dependency-analyzer" (coq-prog "coqdep")))
                        (substitute* "Makefile"
                          (("/sbin/install-info") "install-info"))
                        (substitute* "bin/proofgeneral"
                          (("^PGHOMEDEFAULT=.*" all)
                           (string-append all
                                          "PGHOME=$PGHOMEDEFAULT\n"
                                          "EMACS=" emacs "/bin/emacs")))
                        #t)))
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
             (apply invoke "make" "install-doc" make-flags))))))
    (home-page "http://proofgeneral.inf.ed.ac.uk/")
    (synopsis "Generic front-end for proof assistants based on Emacs")
    (description
     "Proof General is a major mode to turn Emacs into an interactive proof
assistant to write formal mathematical proofs using a variety of theorem
provers.")
    (license license:gpl2+)))

(define-public coq-flocq
  (package
    (name "coq-flocq")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              ;; Use the ‘Latest version’ link for a stable URI across releases.
              (uri (string-append "https://gforge.inria.fr/frs/download.php/"
                                  "file/37901/flocq-" version ".tar.gz"))
              (sha256
               (base32
                "02szrgz9m0ac51la1lqpiv6i2g0zbgx9gz5rp0q1g00ajldyna5c"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq)))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib/Flocq"))
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
    (home-page "http://flocq.gforge.inria.fr/")
    (synopsis "Floating-point formalization for the Coq system")
    (description "Flocq (Floats for Coq) is a floating-point formalization for
the Coq system.  It provides a comprehensive library of theorems on a multi-radix
multi-precision arithmetic.  It also supports efficient numerical computations
inside Coq.")
    (license license:lgpl3+)))

(define-public coq-gappa
  (package
    (name "coq-gappa")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/file/37918/gappa-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1wdg07dk4lbq7dr80ywzna0lclwgi8bddzc6yfx19z1zn9yljzxh"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq)
       ("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("gmp" ,gmp)
       ("mpfr" ,mpfr)
       ("boost" ,boost)))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib/Gappa")
             "CXXFLAGS=-std=c++11")
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
    (home-page "http://gappa.gforge.inria.fr/")
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
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/math-comp/math-comp.git")
             (commit (string-append "mathcomp-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sdrw3b6lc8crz02lp90a863rvyzhc9vcfsrdvc9m311yiaad4xv"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq)))
    (arguments
     `(#:tests? #f             ; no need to test formally-verified programs :)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'chdir
           (lambda _ (chdir "mathcomp") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "COQLIB" (string-append (assoc-ref outputs "out") "/lib/coq/"))
             (invoke "make" "-f" "Makefile.coq"
                     (string-append "COQLIB=" (assoc-ref outputs "out")
                                    "/lib/coq/")
                     "install"))))))
    (home-page "https://math-comp.github.io/math-comp/")
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
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/"
                                  "file/37523/coquelicot-" version ".tar.gz"))
              (sha256
               (base32
                "1biia7nfqf7vaqq5gmykl4rwjyvrcwss6r2jdf0in5pvp2rnrj2w"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq)))
    (propagated-inputs
     `(("mathcomp" ,coq-mathcomp)))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib/Coquelicot"))
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
    (home-page "http://coquelicot.saclay.inria.fr/index.html")
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
    (version "8.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/coq/bignums/archive/V"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pmk9smw7a14wrfkvjlvmpxim4bsv6xnm5xkrlld2faqy74a044g"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("coq" ,coq)))
    (inputs
     `(("camlp5" ,camlp5)))
    (arguments
     `(#:tests? #f; No test target
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
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/"
                                  "file/37524/interval-" version ".tar.gz"))
              (sha256
               (base32
                "023j9sd64brqvjdidqkn5m8d7a93zd9r86ggh573z9nkjm2m7vvg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq)))
    (propagated-inputs
     `(("flocq" ,coq-flocq)
       ("bignums" ,coq-bignums)
       ("coquelicot" ,coq-coquelicot)
       ("mathcomp" ,coq-mathcomp)))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib/Gappa"))
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
    (home-page "http://coq-interval.gforge.inria.fr/")
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
                      (url "git://github.com/uds-psl/autosubst.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1cl0bp96bk6lplbl7n5c703vd3gvbs5mvf2qrf8q333kkqd7jqq4"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (setenv "COQLIB" (string-append (assoc-ref outputs "out") "/lib/coq/"))
               (invoke "make"
                       (string-append "COQLIB=" (assoc-ref outputs "out")
                                      "/lib/coq/")
                       "install"))))))
      (native-inputs
       `(("coq" ,coq)))
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
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mattam82/Coq-Equations.git")
                    (commit (string-append "v" version "-8.9"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q3wvicr43bgy7xn1diwh4j43mnrhprrc2xd22qlbz9cl6bhf8bj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml"  ,ocaml)
       ("coq"    ,coq)
       ("camlp5" ,camlp5)))
    (arguments
     `(#:test-target "test-suite"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "coq_makefile" "-f" "_CoqProject" "-o" "Makefile")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "COQLIB" (string-append (assoc-ref outputs "out") "/lib/coq/"))
             (invoke "make"
                     (string-append "COQLIB=" (assoc-ref outputs "out")
                                    "/lib/coq/")
                     "install"))))))
    (home-page "https://mattam82.github.io/Coq-Equations/")
    (synopsis "Function definition plugin for Coq")
    (description "Equations provides a notation for writing programs
by dependent pattern-matching and (well-founded) recursion in Coq.  It
compiles everything down to eliminators for inductive types, equality
and accessibility, providing a definitional extension to the Coq
kernel.")
    (license license:lgpl2.1)))

(define-public coq-stdpp
  (package
    (name "coq-stdpp")
    (version "1.2.0")
    (synopsis "Alternative Coq standard library std++")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.mpi-sws.org/iris/stdpp.git")
                    (commit (string-append "coq-stdpp-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "11m7kqxsbxygk41v2wsi3npdzwin9fcnzc1gn0gq0rd57wnqk83i"))))
    (build-system gnu-build-system)
    (inputs
     `(("coq" ,coq)))
    (arguments
     `(#:tests? #f ;; the tests are being run automaticlly as part of `make all`
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "COQLIB" (string-append (assoc-ref outputs "out") "/lib/coq/"))
             (invoke "make"
                     (string-append "COQLIB=" (assoc-ref outputs "out")
                                    "/lib/coq/")
                     "install"))))))
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
