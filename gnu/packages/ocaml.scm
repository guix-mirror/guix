;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages ocaml)
  #:use-module ((guix licenses) #:hide (zlib))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages curl))

(define-public ocaml
  (package
    (name "ocaml")
    (version "4.02.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://caml.inria.fr/pub/distrib/ocaml-"
                    (version-major+minor version)
                    "/ocaml-" version ".tar.xz"))
              (sha256
               (base32
                "1p7lqvh64xpykh99014mz21q8fs3qyjym2qazhhbq8scwldv1i38"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libx11" ,libx11)
       ;; For libiberty, needed for objdump support.
       ("gcc:lib" ,(canonical-package gcc-4.9) "lib")
       ("zlib" ,zlib)))                       ;also needed for objdump support
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (web server))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh-references
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((sh (string-append (assoc-ref inputs "bash")
                                                "/bin/sh"))
                             (quoted-sh (string-append "\"" sh "\"")))
                        (with-fluids ((%default-port-encoding #f))
                          (for-each (lambda (file)
                                      (substitute* file
                                        (("\"/bin/sh\"")
                                         (begin
                                           (format (current-error-port) "\
patch-/bin/sh-references: ~a: changing `\"/bin/sh\"' to `~a'~%"
                                                   file quoted-sh)
                                           quoted-sh))))
                                    (find-files "." "\\.ml$"))
                          #t))))
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (mandir (string-append out "/share/man")))
                      ;; Custom configure script doesn't recognize
                      ;; --prefix=<PREFIX> syntax (with equals sign).
                      (zero? (system* "./configure"
                                      "--prefix" out
                                      "--mandir" mandir)))))
         (replace 'build
                  (lambda _
                    (zero? (system* "make" "-j" (number->string
                                                 (parallel-job-count))
                                    "world.opt"))))
         (delete 'check)
         (add-after 'install 'check
                    (lambda _
                      (with-directory-excursion "testsuite"
                        (zero? (system* "make" "all")))))
         (add-before 'check 'prepare-socket-test
                     (lambda _
                       (format (current-error-port)
                               "Spawning local test web server on port 8080~%")
                       (when (zero? (primitive-fork))
                         (run-server (lambda (request request-body)
                                       (values '((content-type . (text/plain)))
                                               "Hello!"))
                                     'http '(#:port 8080)))
                       (let ((file "testsuite/tests/lib-threads/testsocket.ml"))
                         (format (current-error-port)
                                 "Patching ~a to use localhost port 8080~%"
                                 file)
                         (substitute* file
                           (("caml.inria.fr") "localhost")
                           (("80") "8080")
                           (("HTTP1.0") "HTTP/1.0"))
                         #t))))))
    (home-page "https://ocaml.org/")
    (synopsis "The OCaml programming language")
    (description
     "OCaml is a general purpose industrial-strength programming language with
an emphasis on expressiveness and safety.  Developed for more than 20 years at
Inria it benefits from one of the most advanced type systems and supports
functional, imperative and object-oriented styles of programming.")
    ;; The compiler is distributed under qpl1.0 with a change to choice of
    ;; law: the license is governed by the laws of France.  The library is
    ;; distributed under lgpl2.0.
    (license (list qpl lgpl2.0))))

(define-public opam
  (package
    (name "opam")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              ;; Use the '-full' version, which includes all the dependencies.
              (uri (string-append
                    "https://github.com/ocaml/opam/releases/download/"
                    version "/opam-full-" version ".tar.gz")
               ;; (string-append "https://github.com/ocaml/opam/archive/"
               ;;                    version ".tar.gz")
               )
              (sha256
               (base32
                "1frzqkx6yn1pnyd9qz3bv3rbwv74bmc1xji8kl41r1dkqzfl3xqv"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Sometimes, 'make -jX' would fail right after ./configure with
       ;; "Fatal error: exception End_of_file".
       #:parallel-build? #f

       ;; For some reason, 'ocp-build' needs $TERM to be set.
       #:make-flags '("TERM=screen")
       #:test-target "tests"

       ;; FIXME: There's an obscure test failure:
       ;;   …/_obuild/opam/opam.asm install P1' failed.
       #:tests? #f

       #:phases (alist-cons-before
                 'build 'pre-build
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((bash (assoc-ref inputs "bash")))
                     (substitute* "src/core/opamSystem.ml"
                       (("\"/bin/sh\"")
                        (string-append "\"" bash "/bin/sh\"")))))
                 (alist-cons-before
                  'check 'pre-check
                  (lambda _
                    (setenv "HOME" (getcwd))
                    (and (system "git config --global user.email guix@gnu.org")
                         (system "git config --global user.name Guix")))
                  %standard-phases))))
    (native-inputs
     `(("git" ,git)                               ;for the tests
       ("python" ,python)))                       ;for the tests
    (inputs
     `(("ocaml" ,ocaml)
       ("ncurses" ,ncurses)
       ("curl" ,curl)))
    (home-page "http://opam.ocamlpro.com/")
    (synopsis "Package manager for OCaml")
    (description
     "OPAM is a tool to manage OCaml packages.  It supports multiple
simultaneous compiler installations, flexible package constraints, and a
Git-friendly development workflow.")

    ;; The 'LICENSE' file waives some requirements compared to LGPLv3.
    (license lgpl3)))

(define-public camlp5
  (package
    (name "camlp5")
    (version "6.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://camlp5.gforge.inria.fr/distrib/src/"
                                  name "-" version ".tgz"))
              (sha256
               (base32
                "00jwgp6w4g64lfqjx77xziy532091fy00c42fsy0b4i892rch5mp"))))
    (build-system gnu-build-system)
    (inputs
     `(("ocaml" ,ocaml)))
    (arguments
     `(#:tests? #f  ; XXX TODO figure out how to run the tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (mandir (string-append out "/share/man")))
                      ;; Custom configure script doesn't recognize
                      ;; --prefix=<PREFIX> syntax (with equals sign).
                      (zero? (system* "./configure"
                                      "--prefix" out
                                      "--mandir" mandir)))))
         (replace 'build
                  (lambda _
                    (zero? (system* "make" "-j" (number->string
                                                 (parallel-job-count))
                                    "world.opt")))))))
    (home-page "http://camlp5.gforge.inria.fr/")
    (synopsis "Pre-processor Pretty Printer for OCaml")
    (description
     "Camlp5 is a Pre-Processor-Pretty-Printer for Objective Caml.  It offers
tools for syntax (Stream Parsers and Grammars) and the ability to modify the
concrete syntax of the language (Quotations, Syntax Extensions).")
    ;; Most files are distributed under bsd-3, but ocaml_stuff/* is under qpl.
    (license (list bsd-3 qpl))))

(define-public hevea
  (package
    (name "hevea")
    (version "2.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://hevea.inria.fr/distri/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1f9pj48518ixhjxbviv2zx27v4anp92zgg3x704g1s5cki2w33nv"))))
    (build-system gnu-build-system)
    (inputs
     `(("ocaml" ,ocaml)))
    (arguments
     `(#:tests? #f  ; no test suite
       #:make-flags (list (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "http://hevea.inria.fr/")
    (synopsis "LaTeX to HTML translator")
    (description
     "HeVeA is a LaTeX to HTML translator that generates modern HTML 5.  It is
written in Objective Caml.")
    (license qpl)))

(define-public coq
  (package
    (name "coq")
    (version "8.4pl6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://coq.inria.fr/distrib/V" version
                                  "/files/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mpbj4yf36kpjg2v2sln12i8dzqn8rag6fd07hslj2lpm4qs4h55"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("texlive" ,texlive)
       ("hevea" ,hevea)))
    (inputs
     `(("ocaml" ,ocaml)
       ("camlp5" ,camlp5)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (mandir (string-append out "/share/man"))
                           (browser "icecat -remote \"OpenURL(%s,new-tab)\""))
                      (zero? (system* "./configure"
                                      "--prefix" out
                                      "--mandir" mandir
                                      "--browser" browser)))))
         (replace 'build
                  (lambda _
                    (zero? (system* "make" "-j" (number->string
                                                 (parallel-job-count))
                                    "world"))))
         (delete 'check)
         (add-after 'install 'check
                    (lambda _
                      (with-directory-excursion "test-suite"
                        (zero? (system* "make"))))))))
    (home-page "https://coq.inria.fr")
    (synopsis "Proof assistant for higher-order logic")
    (description
     "Coq is a proof assistant for higher-order logic, which allows the
development of computer programs consistent with their formal specification.
It is developed using Objective Caml and Camlp5.")
    ;; The code is distributed under lgpl2.1.
    ;; Some of the documentation is distributed under opl1.0+.
    (license (list lgpl2.1 opl1.0+))))

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
       ("emacs" ,emacs-no-x)
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
                      (zero? (system* "make" "clean"))))
         (add-after 'install 'install-doc
                    (lambda* (#:key make-flags #:allow-other-keys)
                      ;; XXX FIXME avoid building/installing pdf files,
                      ;; due to unresolved errors building them.
                      (substitute* "Makefile"
                        ((" [^ ]*\\.pdf") ""))
                      (zero? (apply system* "make" "install-doc"
                                    make-flags)))))))
    (home-page "http://proofgeneral.inf.ed.ac.uk/")
    (synopsis "Generic front-end for proof assistants based on Emacs")
    (description
     "Proof General is a major mode to turn Emacs into an interactive proof
assistant to write formal mathematical proofs using a variety of theorem
provers.")
    (license gpl2+)))
