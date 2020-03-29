;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Dan Frumin <dfrumin@cs.ru.nl>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
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
    (version "8.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coq/coq.git")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ji2rzd70b3hcwfw97qk7rv3m2977ylqnq82l1555dp3njr8nm3q"))))
    (native-search-paths
     (list (search-path-specification
            (variable "COQPATH")
            (files (list "lib/coq/user-contrib")))))
    (build-system ocaml-build-system)
    (outputs '("out" "ide"))
    (inputs
     `(("lablgtk" ,lablgtk3)
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
         (add-after 'install 'remove-duplicate
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               ;; These files are exact copies without `.opt` extension.
               ;; Removing these saves 35 MiB in the resulting package.
               (delete-file (string-append bin "/coqtop.opt"))
               (delete-file (string-append bin "/coqidetop.opt")))
             #t))
         (add-after 'install 'install-ide
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (ide (assoc-ref outputs "ide")))
               (mkdir-p (string-append ide "/bin"))
               (rename-file (string-append out "/bin/coqide")
                            (string-append ide "/bin/coqide")))
             #t))
         (add-after 'install 'check
           (lambda _
             (with-directory-excursion "test-suite"
               ;; These two tests fail.
               ;; Fails because the output is not formatted as expected.
               (delete-file-recursively "coq-makefile/timing")
               ;; Fails because we didn't build coqtop.byte.
               (delete-file-recursively "coq-makefile/findlib-package")
               (invoke "make")))))))
    (home-page "https://coq.inria.fr")
    (synopsis "Proof assistant for higher-order logic")
    (description
     "Coq is a proof assistant for higher-order logic, which allows the
development of computer programs consistent with their formal specification.
It is developed using Objective Caml and Camlp5.")
    ;; The source code is distributed under lgpl2.1.
    ;; Some of the documentation is distributed under opl1.0+.
    (license (list license:lgpl2.1 license:opl1.0+))))

(define-public proof-general
  (package
    (name "proof-general")
    (version "4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append
                          "https://github.com/ProofGeneral/PG"))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bdfk91wf71z80mdfnl8hpinripndcjgdkz854zil6521r84nqk8"))))
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
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/flocq/flocq.git")
             (commit (string-append "flocq-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15bi36x7zj0glsb3s2gwqd4wswhfzh36rbp7imbyff53a7nna95l"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq)))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib/Flocq"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-failing-examples
           (lambda _
             (substitute* "Remakefile.in"
               ;; Fails on a union error.
               (("Double_rounding_odd_radix.v") ""))
             #t))
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
    (home-page "https://flocq.gforge.inria.fr/")
    (synopsis "Floating-point formalization for the Coq system")
    (description "Flocq (Floats for Coq) is a floating-point formalization for
the Coq system.  It provides a comprehensive library of theorems on a multi-radix
multi-precision arithmetic.  It also supports efficient numerical computations
inside Coq.")
    (license license:lgpl3+)))

(define-public coq-gappa
  (package
    (name "coq-gappa")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/gappa/coq.git")
             (commit (string-append "gappalib-coq-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r7jwp5xssdfzivs2flp7mzrscqhgl63mryhhf1cvndpgzqwfk2f"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq)
       ("camlp5" ,camlp5)
       ("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("gmp" ,gmp)
       ("mpfr" ,mpfr)
       ("boost" ,boost)))
    (propagated-inputs
     `(("coq-flocq" ,coq-flocq)))
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
         ;; FIXME: Figure out why failures occur, and re-enable check phase.
         (delete 'check)
         ;; (replace 'check
         ;;   (lambda _ (invoke "./remake" "check")))
         (replace 'install
           (lambda _ (invoke "./remake" "install"))))))
    (home-page "https://gappa.gforge.inria.fr/")
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
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/math-comp/math-comp.git")
             (commit (string-append "mathcomp-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h5h1c2025r1ms5qryvwy6pikxmpmmjav6yl127xpzmqdi6w732d"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq)))
    (arguments
     `(#:tests? #f ; No tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'chdir
           (lambda _ (chdir "mathcomp") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "-f" "Makefile.coq"
                     (string-append "COQLIB=" (assoc-ref outputs "out")
                                    "/lib/coq/")
                     "install"))))))
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
    (version "3.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/coquelicot/coquelicot.git")
             (commit (string-append "coquelicot-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0m5wbr2s8lnf8b7cfwv15hyzsmbcaz6hgdn7aazcrkxnwr87vgkp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("ocaml" ,ocaml)
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
    (version "8.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/coq/bignums.git")
                     (commit (string-append "V" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bpb4flckn4nqxbs3wjiznyx1k7r8k93qdigp3qwmikp2lxvcbw5"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("coq" ,coq)))
    (inputs
     `(("camlp5" ,camlp5)))
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
    (version "3.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/coqinterval/interval.git")
             (commit (string-append "interval-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03q3dfqi3r3f7aji5s06ig4aav9ajcwswwdzi5lrgr69z0m487k4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("ocaml" ,ocaml)
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
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mattam82/Coq-Equations.git")
                    (commit (string-append "v" version "-8.10"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "023q5dww3drw35dm9bi9p9d0wrj9k7vax7hfdsprf8l340pb4s0k"))))
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
             (invoke "sh" "./configure.sh")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
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
    (version "1.2.1")
    (synopsis "Alternative Coq standard library std++")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.mpi-sws.org/iris/stdpp.git")
                    (commit (string-append "coq-stdpp-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lczybg1jq9drbi8nzrlb0k199x4n07aawjwfzrl3qqc0w8kmvdz"))))
    (build-system gnu-build-system)
    (inputs
     `(("coq" ,coq)))
    (arguments
     `(#:tests? #f ; Tests are executed during build phase.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
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
