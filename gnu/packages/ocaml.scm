;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module ((srfi srfi-1) #:hide (zip)))

;; A shortcut for files from ocaml forge. Downloaded files are computed from
;; their number, not their name.
(define (ocaml-forge-uri name version file-number)
  (string-append "https://forge.ocamlcore.org/frs/download.php/"
                 (number->string file-number) "/" name "-" version
                 ".tar.gz"))

;; Janestreet packages are found in a similar way and all need the same patch.
(define (janestreet-origin name version hash)
  (origin (method url-fetch)
          (uri (string-append "https://ocaml.janestreet.com/ocaml-core/"
                              (version-major+minor version) "/files/"
                              name "-" version ".tar.gz"))
          (sha256 (base32 hash))
          (modules '((guix build utils)))
          (snippet
           (let ((pattern (string-append "lib/" name)))
             `(begin
                ;; install.ml contains an invalid reference to the ppx file and
                ;; propagates this error to the generated META file.  It
                ;; looks for it in the "lib" directory, but it is installed in
                ;; "lib/ocaml/site-lib/package".  This substitute does not change
                ;; this file for non ppx packages.
                (substitute* "install.ml"
                  ((,pattern) (string-append "lib/ocaml/site-lib/" ,name)))
                ;; The standard Makefile would try to install janestreet modules
                ;; in OCaml's directory in the store, which is read-only.
                (substitute* "Makefile"
                  (("--prefix")
                   "--libdir $(LIBDIR) --prefix")))))))

;; They also require almost the same set of arguments
(define janestreet-arguments
  `(#:use-make? #t
    #:make-flags
    (list (string-append "CONFIGUREFLAGS=--prefix "
                         (assoc-ref %outputs "out")
                         " --enable-tests")
          (string-append "LIBDIR="
                         (assoc-ref %outputs "out")
                         "/lib/ocaml/site-lib")
          ;; for ocaml-bin-prot, otherwise ignored
          (string-append "OCAML_TOPLEVEL_PATH="
                         (assoc-ref %build-inputs "findlib")
                         "/lib/ocaml/site-lib"))
    #:phases (modify-phases %standard-phases (delete 'configure))))

(define-public ocaml
  (package
    (name "ocaml")
    (version "4.02.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://caml.inria.fr/pub/distrib/ocaml-"
                    (version-major+minor version)
                    "/ocaml-" version ".tar.xz"))
              (sha256
               (base32
                "1qwwvy8nzd87hk8rd9sm667nppakiapnx4ypdwcrlnav2dz6kil3"))
              (patches
               (search-patches
                "ocaml-CVE-2015-8869.patch"
                "ocaml-Add-a-.file-directive.patch"))))
    (build-system gnu-build-system)
    (native-search-paths
     (list (search-path-specification
            (variable "OCAMLPATH")
            (files (list "lib/ocaml" "lib/ocaml/site-lib")))
           (search-path-specification
            (variable "CAML_LD_LIBRARY_PATH")
            (files (list "lib/ocaml/site-lib/stubslibs")))))
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
                 (for-each
                  (lambda (file)
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
    (license (list license:qpl license:lgpl2.0))))

(define-public ocaml-4.01
  (package
    (inherit ocaml)
    (version "4.01.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://caml.inria.fr/pub/distrib/ocaml-"
                    (version-major+minor version)
                    "/ocaml-" version ".tar.xz"))
              (sha256
               (base32
                "03d7ida94s1gpr3gadf4jyhmh5rrszd5s4m4z59daaib25rvfyv7"))))
    (arguments
     (substitute-keyword-arguments (package-arguments ocaml)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build
             (lambda _
               ;; Specifying '-j' at all causes the build to fail.
               (zero? (system* "make" "world.opt"))))
           (replace 'check
             (lambda _
               (with-directory-excursion "testsuite"
                 (zero? (system*
                         "make"
                         "all"
                         (string-append
                          "TOPDIR=" (getcwd) "/.."))))))))))))

(define-public opam
  (package
    (name "opam")
    (version "1.2.2")
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
                "004gwn6rbpcb53y3rpb3v23vk39rp2xmf0liyd5iy12ij8bigrhm"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Sometimes, 'make -jX' would fail right after ./configure with
       ;; "Fatal error: exception End_of_file".
       #:parallel-build? #f

       ;; For some reason, 'ocp-build' needs $TERM to be set.
       #:make-flags `("TERM=screen"
                      ,(string-append "SHELL="
                                      (assoc-ref %build-inputs "bash")
                                      "/bin/sh"))
       #:test-target "tests"

       ;; FIXME: There's an obscure test failure:
       ;;   …/_obuild/opam/opam.asm install P1' failed.
       #:tests? #f

       #:phases (modify-phases %standard-phases
                 (add-before 'build 'pre-build
                   (lambda* (#:key inputs make-flags #:allow-other-keys)
                     (let ((bash (assoc-ref inputs "bash")))
                       (substitute* "src/core/opamSystem.ml"
                         (("\"/bin/sh\"")
                          (string-append "\"" bash "/bin/sh\"")))
                       ;; Build dependencies
                       (zero? (apply system* "make" "lib-ext" make-flags)))))
                 (add-before 'check 'pre-check
                   (lambda _
                     (setenv "HOME" (getcwd))
                     (and (system "git config --global user.email guix@gnu.org")
                          (system "git config --global user.name Guix")))))))
    (native-inputs
     `(("git" ,git)                               ;for the tests
       ("python" ,python)                         ;for the tests
       ("camlp4" ,camlp4)))
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
    (license license:lgpl3)))

(define-public camlp4
  (package
    (name "camlp4")
    (version "4.02+6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/camlp4/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0icdfzhsbgf89925gc8gl3fm8z2xzszzlib0v9dj5wyzkyv3a342"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (native-inputs `(("ocaml" ,ocaml)
                     ("which" ,which)))
    (inputs `(("ocaml" ,ocaml)))
    (arguments
     '(#:tests? #f                                ;no documented test target
       ;; a race-condition will lead byte and native targets to  mkdir _build
       ;; which  fails on the second attempt.
       #:parallel-build? #f
       #:make-flags '("all")
       #:phases (modify-phases %standard-phases
                  (replace
                   'configure
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; This is a home-made 'configure' script.
                     (let ((out (assoc-ref outputs "out")))
                       (zero? (system* "./configure"
                                       (string-append "--libdir=" out
                                                      "/lib/ocaml/site-lib")
                                       (string-append "--bindir=" out "/bin")
                                       (string-append "--pkgdir=" out
                                                      "/lib/ocaml/site-lib"))))))
                  (add-after 'install 'install-meta
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "camlp4/META.in"
                          (("directory = .*")
                            (string-append "directory = \"" out
                                           "/lib/ocaml/site-lib/camlp4\"\n")))
                        (zero? (system* "make" "install-META"))))))))
    (home-page "https://github.com/ocaml/camlp4")
    (synopsis "Write parsers in OCaml")
    (description
     "Camlp4 is a software system for writing extensible parsers for
programming languages.  It provides a set of OCaml libraries that are used to
define grammars as well as loadable syntax extensions of such grammars.
Camlp4 stands for Caml Preprocessor and Pretty-Printer and one of its most
important applications is the definition of domain-specific extensions of the
syntax of OCaml.")

    ;; This is LGPLv2 with an exception that allows packages statically-linked
    ;; against the library to be released under any terms.
    (license license:lgpl2.0)))

(define-public camlp5
  (package
    (name "camlp5")
    (version "6.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://camlp5.gforge.inria.fr/distrib/src/"
                                  name "-" version ".tgz"))
              (sha256
               (base32
                "1ql04iyvclpyy9805kpddc4ndjb5d0qg4shhi2fc6bixi49fvy89"))))
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
                                    "world.opt"))))
         ;; Required for findlib to find camlp5's libraries
         (add-after 'install 'install-meta
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "etc/META" (string-append (assoc-ref outputs "out")
                                                     "/lib/ocaml/camlp5/")))))))
    (home-page "http://camlp5.gforge.inria.fr/")
    (synopsis "Pre-processor Pretty Printer for OCaml")
    (description
     "Camlp5 is a Pre-Processor-Pretty-Printer for Objective Caml.  It offers
tools for syntax (Stream Parsers and Grammars) and the ability to modify the
concrete syntax of the language (Quotations, Syntax Extensions).")
    ;; Most files are distributed under bsd-3, but ocaml_stuff/* is under qpl.
    (license (list license:bsd-3 license:qpl))))

(define-public hevea
  (package
    (name "hevea")
    (version "2.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://hevea.inria.fr/old/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "14fns13wlnpiv9i05841kvi3cq4b9v2sw5x3ff6ziws28q701qnd"))))
    (build-system gnu-build-system)
    (inputs
     `(("ocaml" ,ocaml)))
    (arguments
     `(#:tests? #f  ; no test suite
       #:make-flags (list (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'patch-/bin/sh
                    (lambda _
                      (substitute* "_tags"
                        (("/bin/sh") (which "sh")))
                      #t)))))
    (home-page "http://hevea.inria.fr/")
    (synopsis "LaTeX to HTML translator")
    (description
     "HeVeA is a LaTeX to HTML translator that generates modern HTML 5.  It is
written in Objective Caml.")
    (license license:qpl)))

(define-public coq
  (package
    (name "coq")
    (version "8.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://coq.inria.fr/distrib/V" version
                                  "/files/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "15wjngjd5pyfqdl5yw92rvdxvy15xcjlpx0rqlkzvcsis1z20xpk"))))
    (native-search-paths
     (list (search-path-specification
            (variable "COQPATH")
            (files (list "lib/coq/user-contrib")))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("texlive" ,texlive)
       ("hevea" ,hevea)))
    (inputs
     `(("lablgtk" ,lablgtk)
       ("python" ,python-2)
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
                               "-prefix" out
                               "-mandir" mandir
                               "-browser" browser
                               "-coqide" "opt")))))
         (replace 'build
           (lambda _
             (zero? (system* "make" "-j" (number->string
                                          (parallel-job-count))
                             "world"))))
         (delete 'check)
         (add-after 'install 'check
           (lambda _
             (with-directory-excursion "test-suite"
               ;; These two tests fail.
               ;; This one fails because the output is not formatted as expected.
               (delete-file-recursively "coq-makefile/timing")
               ;; This one fails because we didn't build coqtop.byte.
               (delete-file-recursively "coq-makefile/findlib-package")
               (zero? (system* "make"))))))))
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
    (license license:gpl2+)))

(define-public emacs-tuareg
  (package
    (name "emacs-tuareg")
    (version "2.0.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/tuareg/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r2smclcs63n74lcyckbp90j09wyjdngn816cqzfkw54iwh3hd7q"))))
    (build-system gnu-build-system)
    (native-inputs `(("emacs" ,emacs-minimal)
                     ("opam" ,opam)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'fix-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/emacs/site-lisp")
                (string-append (assoc-ref %outputs "out")
                               "/share/emacs/site-lisp/")))
             #t))
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (symlink "tuareg.el"
                      (string-append (assoc-ref outputs "out")
                                     "/share/emacs/site-lisp/"
                                     "tuareg-autoloads.el"))
             #t)))))
    (home-page "https://github.com/ocaml/tuareg")
    (synopsis "OCaml programming mode, REPL, debugger for Emacs")
    (description "Tuareg helps editing OCaml code, to highlight important
parts of the code, to run an OCaml REPL, and to run the OCaml debugger within
Emacs.")
    (license license:gpl2+)))

(define-public ocaml-menhir
  (package
    (name "ocaml-menhir")
    (version "20161115")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://gallium.inria.fr/~fpottier/menhir/"
                    "menhir-" version ".tar.gz"))
              (sha256
               (base32
                "1j8nmcj2gq6hyyi16z27amiahplgrnk4ppchpm0v4qy80kwkf47k"))))
    (build-system gnu-build-system)
    (inputs
     `(("ocaml" ,ocaml)))
    (arguments
     `(#:parallel-build? #f ; Parallel build causes failure
       #:tests? #f ; No check target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "PREFIX" out))
             #t)))))
    (home-page "http://gallium.inria.fr/~fpottier/menhir")
    (synopsis "Parser generator")
    (description "Menhir is a parser generator.  It turns high-level grammar
specifications, decorated with semantic actions expressed in the OCaml
programming language into parsers, again expressed in OCaml. It is based on
Knuth’s LR(1) parser construction technique.")
    ;; The file src/standard.mly and all files listed in src/mnehirLib.mlpack
    ;; that have an *.ml or *.mli extension are GPL licensed. All other files
    ;; are QPL licensed.
    (license (list license:gpl2+ license:qpl))))

(define-public lablgtk
  (package
    (name "lablgtk")
    (version "2.18.5")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1627))
              (sha256
               (base32
                "0cyj6sfdvzx8hw7553lhgwc0krlgvlza0ph3dk9gsxy047dm3wib"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("camlp4" ,camlp4)
       ("ocaml" ,ocaml)
       ("findlib" ,ocaml-findlib)
       ("pkg-config" ,pkg-config)))
    ;; FIXME: Add inputs gtkgl-2.0, libpanelapplet-2.0, gtkspell-2.0,
    ;; and gtk+-quartz-2.0 once available.
    (inputs
     `(("gtk+" ,gtk+-2)
       ("gtksourceview" ,gtksourceview-2)
       ("libgnomecanvas" ,libgnomecanvas)
       ("libgnomeui" ,libgnomeui)
       ("libglade" ,libglade)
       ("librsvg" ,librsvg)))
    (arguments
     `(#:tests? #f ; no check target

       ;; opt: also install cmxa files
       #:make-flags (list "all" "opt"
                          (string-append "FINDLIBDIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/ocaml"))
       ;; Occasionally we would get "Error: Unbound module GtkThread" when
       ;; compiling 'gtkThInit.ml', with 'make -j'.  So build sequentially.
       #:parallel-build? #f

       #:phases
         (modify-phases %standard-phases
           (add-before 'install 'prepare-install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (ocaml (assoc-ref inputs "ocaml")))
                 ;; Install into the output and not the ocaml directory.
                 (mkdir-p (string-append out "/lib/ocaml"))
                 (substitute* "config.make"
                   ((ocaml) out))
                 #t))))))
    (home-page "http://lablgtk.forge.ocamlcore.org/")
    (synopsis "GTK+ bindings for OCaml")
    (description
     "LablGtk is an OCaml interface to GTK+ 1.2 and 2.x.  It provides
a strongly-typed object-oriented interface that is compatible with the
dynamic typing of GTK+.  Most widgets and methods are available.  LablGtk
also provides bindings to
gdk-pixbuf, the GLArea widget (in combination with LablGL), gnomecanvas,
gnomeui, gtksourceview, gtkspell,
libglade (and it an generate OCaml code from .glade files),
libpanel, librsvg and quartz.")
    (license license:lgpl2.1)))

(define-public unison
  (package
    (name "unison")
    (version "2.48.3")
    (source
      (origin
        (method svn-fetch)
        (uri (svn-reference
              (url (string-append "https://webdav.seas.upenn.edu/svn/"
                                  "unison/branches/"
                                  (version-major+minor version)))
              (revision 535)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "0486s53wyayicj9f2raj2dvwvk4xyzar219rccc1iczdwixm4x05"))
        (modules '((guix build utils)
                   (ice-9 rdelim)
                   (ice-9 regex)
                   (srfi srfi-1)))
        (snippet
         `(begin
            ;; The svn revision in the release tarball appears to be
            ;; artificially manipulated in order to set the desired point
            ;; version number.  Because the point version is calculated during
            ;; the build, we can offset pointVersionOrigin by the desired
            ;; point version and write that into "Rev: %d".  We do this rather
            ;; than hardcoding the necessary revision number, for
            ;; maintainability.
            (with-atomic-file-replacement "src/mkProjectInfo.ml"
              (lambda (in out)
                (let ((pt-ver (string->number (third (string-split ,version #\.))))
                      (pt-rx  (make-regexp "^let pointVersionOrigin = ([0-9]+)"))
                      (rev-rx (make-regexp "Rev: [0-9]+")))
                  (let loop ((pt-origin #f))
                    (let ((line (read-line in 'concat)))
                      (cond
                       ((regexp-exec pt-rx line)
                        => (lambda (m)
                             (display line out)
                             (loop (string->number (match:substring m 1)))))
                       ((regexp-exec rev-rx line)
                        => (lambda (m)
                             (format out "~aRev: ~d~a"
                                     (match:prefix m)
                                     (+ pt-origin pt-ver)
                                     (match:suffix m))
                             (dump-port in out))) ;done
                       (else
                        (display line out)
                        (loop pt-origin))))))))
            ;; Without the '-fix' argument, the html file produced does not
            ;; have functioning internal hyperlinks.
            (substitute* "doc/Makefile"
              (("hevea unison") "hevea -fix unison"))))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                  ; 1.9 MiB of documentation
    (native-inputs
     `(("ocaml" ,ocaml)
       ;; For documentation
       ("ghostscript" ,ghostscript)
       ("texlive" ,texlive)
       ("hevea" ,hevea)
       ("lynx" ,lynx)))
    (arguments
     `(#:parallel-build? #f
       #:parallel-tests? #f
       #:test-target "selftest"
       #:tests? #f ; Tests require writing to $HOME.
                   ; If some $HOME is provided, they fail with the message
                   ; "Fatal error: Skipping some tests -- remove me!"
       #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'install 'prepare-install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (mkdir-p bin)
                 (setenv "HOME" out) ; forces correct INSTALLDIR in Makefile
                 #t)))
           (add-after 'install 'install-fsmonitor
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 ;; 'unison-fsmonitor' is used in "unison -repeat watch" mode.
                 (install-file "src/unison-fsmonitor" bin))))
           (add-after 'install 'install-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((doc (string-append (assoc-ref outputs "doc")
                                         "/share/doc/unison")))
                 (mkdir-p doc)
                 ;; This file needs write-permissions, because it's
                 ;; overwritten by 'docs' during documentation generation.
                 (chmod "src/strings.ml" #o600)
                 (and (zero? (system* "make" "docs"
                                      "TEXDIRECTIVES=\\\\draftfalse"))
                      (begin
                        (for-each (lambda (f)
                                    (install-file f doc))
                                  (map (lambda (ext)
                                         (string-append
                                          "doc/unison-manual." ext))
                                       ;; Install only html documentation,
                                       ;; since the build is currently
                                       ;; non-reproducible with the ps, pdf,
                                       ;; and dvi docs.
                                       '(;;"ps" "pdf" "dvi"
                                         "html")))
                        #t))))))))
    (home-page "https://www.cis.upenn.edu/~bcpierce/unison/")
    (synopsis "File synchronizer")
    (description
     "Unison is a file-synchronization tool.  It allows two replicas of
a collection of files and directories to be stored on different hosts
(or different disks on the same host), modified separately, and then
brought up to date by propagating the changes in each replica
to the other.")
    (license license:gpl3+)))

(define-public ocaml-findlib
  (package
    (name "ocaml-findlib")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.camlcity.org/download/"
                                  "findlib" "-" version ".tar.gz"))
              (sha256
               (base32
                "02abg1lsnwvjg3igdyb8qjgr5kv1nbwl4gaf8mdinzfii5p82721"))
              (patches (search-patches "ocaml-findlib-make-install.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("camlp4" ,camlp4)
       ("m4" ,m4)
       ("ocaml" ,ocaml)))
    (arguments
     `(#:tests? #f  ; no test suite
       #:parallel-build? #f
       #:make-flags (list "all" "opt")
       #:phases (modify-phases %standard-phases
                  (replace
                   'configure
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out")))
                       (system*
                        "./configure"
                        "-bindir" (string-append out "/bin")
                        "-config" (string-append out "/etc/ocamfind.conf")
                        "-mandir" (string-append out "/share/man")
                        "-sitelib" (string-append out "/lib/ocaml/site-lib")
                        "-with-toolbox"))))
                  (add-after 'install 'remove-camlp4
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (delete-file-recursively
                          (string-append out "/lib/ocaml/site-lib/camlp4"))))))))
    (home-page "http://projects.camlcity.org/projects/findlib.html")
    (synopsis "Management tool for OCaml libraries")
    (description
     "The \"findlib\" library provides a scheme to manage reusable software
components (packages), and includes tools that support this scheme.  Packages
are collections of OCaml modules for which metainformation can be stored.  The
packages are kept in the file system hierarchy, but with strict directory
structure.  The library contains functions to look the directory up that
stores a package, to query metainformation about a package, and to retrieve
dependency information about multiple packages.  There is also a tool that
allows the user to enter queries on the command-line.  In order to simplify
compilation and linkage, there are new frontends of the various OCaml
compilers that can directly deal with packages.")
    (license license:x11)))

(define-public ocaml-findlib-1.7.3
  (package
    (inherit ocaml-findlib)
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.camlcity.org/download/"
                                  "findlib" "-" version ".tar.gz"))
              (sha256
               (base32
                "12xx8si1qv3xz90qsrpazjjk4lc1989fzm97rsmc4diwla7n15ni"))))
    (arguments
     (substitute-keyword-arguments (package-arguments ocaml-findlib)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (zero? (system* "make" "install"
                                 (string-append "OCAML_CORE_STDLIB="
                                                out))))))))))))

(define-public ocaml4.01-findlib
  (package
    (inherit ocaml-findlib)
    (name "ocaml4.01-findlib")
    (native-inputs
     `(("m4" ,m4)
       ("ocaml" ,ocaml-4.01)))))

;; note that some tests may hang for no obvious reason.
(define-public ocaml-ounit
  (package
    (name "ocaml-ounit")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri "ounit" version 1258))
              (sha256
               (base32
                "118xsadrx84pif9vaq13hv4yh22w9kmr0ypvhrs0viir1jr0ajjd"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("libxml2" ,libxml2))) ; for xmllint
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Tests are done during build.
         (delete 'check))))
    (home-page "http://ounit.forge.ocamlcore.org")
    (synopsis "Unit testing framework for OCaml")
    (description "Unit testing framework for OCaml.  It is similar to JUnit and
other XUnit testing frameworks.")
    (license license:expat)))

(define-public ocaml4.01-ounit
  (package-with-ocaml4.01 ocaml-ounit))

(define-public camlzip
  (package
    (name "camlzip")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1616))
              (sha256
               (base32
                "0m6gyjw46w3qnhxfsyqyag42znl5lwargks7w7rfchr9jzwpff68"))))
    (build-system ocaml-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'install-camlzip
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dir (string-append out "/lib/ocaml/site-lib/camlzip")))
               (mkdir-p dir)
               (call-with-output-file (string-append dir "/META")
                 (lambda (port)
                   (format port "version=\"1.06\"\n")
                   (format port "requires=\"unix\"\n")
                   (format port "archive(byte)=\"zip.cma\"\n")
                   (format port "archive(native)=\"zip.cmxa\"\n")
                   (format port "archive(native,plugin)=\"zip.cmxs\"\n")
                   (format port "directory=\"../zip\"\n")))))))
       #:install-target "install-findlib"
       #:make-flags
       (list "all" "allopt"
             (string-append "INSTALLDIR=" (assoc-ref %outputs "out")
                            "/lib/ocaml"))))
    (home-page "http://forge.ocamlcore.org/projects/camlzip")
    (synopsis "Provides easy access to compressed files")
    (description "Provides easy access to compressed files in ZIP, GZIP and
JAR format.  It provides functions for reading from and writing to compressed
files in these formats.")
    (license license:lgpl2.1+)))

(define-public ocaml4.01-camlzip
  (let ((base (package-with-ocaml4.01 camlzip)))
    (package
      (inherit base)
      (name "ocaml4.01-camlzip")
      ;; Version 1.05 is the last version to support OCaml 4.01.0.
      (version "1.05")
      (source
       (origin
         (method url-fetch)
         (uri
          (string-append
           "http://forge.ocamlcore.org/frs/download.php/1037/camlzip-"
           version ".tar.gz"))
         (sha256
          (base32
           "0syh72jk9s0qwjmmfrkqchaj98m020ii082jn38pwnmb6v3p02wk")))))))

(define-public ocamlmod
  (package
    (name "ocamlmod")
    (version "0.0.8")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1544))
              (sha256
               (base32
                "1w0w8lfyymvk300dv13gvhrddpcyknvyp4g2yvq2vaw7khkhjs9g"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ounit" ,ocaml-ounit)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Tests are done during build.
         (delete 'check))))
    (home-page "https://forge.ocamlcore.org/projects/ocamlmod")
    (synopsis "Generate modules from OCaml source files")
    (description "Generate modules from OCaml source files.")
    (license license:lgpl2.1+))) ; with an exception

(define-public ocaml-zarith
  (package
    (name "ocaml-zarith")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri "zarith" version 1574))
              (sha256
               (base32
                "0l36hzmfbvdai2kcgynh13vfdim5x2grnaw61fxqalyjm90c3di3"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("perl" ,perl)))
    (inputs
     `(("gmp" ,gmp)))
    (arguments
     `(#:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key #:allow-other-keys)
             (zero? (system* "./configure")))))))
    (home-page "https://forge.ocamlcore.org/projects/zarith/")
    (synopsis "Implements arbitrary-precision integers")
    (description "Implements arithmetic and logical operations over
arbitrary-precision integers.  It uses GMP to efficiently implement arithmetic
over big integers. Small integers are represented as Caml unboxed integers,
for speed and space economy.")
    (license license:lgpl2.1+))) ; with an exception

(define-public ocaml-frontc
  (package
    (name "ocaml-frontc")
    (version "3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.irit.fr/recherches/ARCHI/MARCH/"
                                  "frontc/Frontc-" version ".tgz"))
              (sha256
               (base32
                "16dz153s92dgbw1rrfwbhscy73did87kfmjwyh3qpvs748h1sc4g"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'install-meta
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-output-to-file
                   (string-append out "/lib/ocaml/frontc/META")
                 (lambda _
                   (display
                    (string-append
                     "description = \"Parser for the C language\"
version = \"" ,version "\"
requires = \"unix\"
archive(byte) = \"frontc.cma\"
archive(native) = \"frontc.cmxa\""))))
               (symlink (string-append out "/lib/ocaml/frontc")
                        (string-append out "/lib/ocaml/FrontC"))))))
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          "OCAML_SITE=$(LIB_DIR)/ocaml/")))
    (home-page "https://www.irit.fr/FrontC")
    (synopsis "C parser and lexer library")
    (description "FrontC is an OCAML library providing a C parser and lexer.
The result is a syntactic tree easy to process with usual OCAML tree management.
It provides support for ANSI C syntax, old-C K&R style syntax and the standard
GNU CC attributes.  It provides also a C pretty printer as an example of use.")
    (license license:lgpl2.1)))

(define-public ocaml-qcheck
  (package
    (name "ocaml-qcheck")
    (version "0.5.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/c-cube/qcheck/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zs1pg5cb1iry554v3cdmmiglsrwmsqa9x8zxmzb118fnk5d3ha6"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ounit" ,ocaml-ounit)))
    (home-page "https://github.com/c-cube/qcheck")
    (synopsis "QuickCheck inspired property-based testing for OCaml")
    (description "QuickCheck inspired property-based testing for OCaml. This
module allows to check invariants (properties of some types) over randomly
generated instances of the type. It provides combinators for generating
instances and printing them.")
    (license license:lgpl3+)))

(define-public ocaml-qtest
  (package
    (name "ocaml-qtest")
    (version "2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/vincent-hugot/iTeML/"
                                  "archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1hw3jqir7w79payy4knc38fa3nxcvl7ap6y6hnqavrhpi8zqb59j"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("findlib" ,ocaml-findlib)))
    (propagated-inputs
     `(("ounit" ,ocaml-ounit)
       ("qcheck" ,ocaml-qcheck)))
    (arguments
     `(#:tests? #f ; No test target.
       #:make-flags
       (list (string-append "BIN=" (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/vincent-hugot/iTeML")
    (synopsis "Inline (Unit) Tests for OCaml")
    (description "Qtest extracts inline unit tests written using a special
syntax in comments.  Those tests are then run using the oUnit framework and the
qcheck library.  The possibilities range from trivial tests -- extremely simple
to use -- to sophisticated random generation of test cases.")
    (license license:lgpl3+)))

(define-public ocaml4.01-qtest
  (package-with-ocaml4.01 ocaml-qtest))

(define-public ocaml-stringext
  (package
    (name "ocaml-stringext")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rgrinberg/stringext"
                                  "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "19g6lfn03iki9f8h91hi0yiqn0b3wkxyq08b3y23wgv6jw6mssfh"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("qtest" ,ocaml-qtest)))
    (home-page "https://github.com/rgrinberg/stringext")
    (synopsis "Extra string functions for OCaml")
    (description "Provides a single module named Stringext that provides a grab
bag of often used but missing string functions from the stdlib.  E.g, split,
full_split, cut, rcut, etc..")
    ;; the only mention of a license in this project is in its `opam' file
    ;; where it says `mit'.
    (license license:expat)))


(define-public ocaml-bisect
  (package
    (name "ocaml-bisect")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri "bisect" version 1051))
              (sha256
               (base32
                "0kcg2rh0qlkfpbv3nhcb75n62b04gbrz0zasq15ynln91zd5qrg0"))
              (patches
               (search-patches
                "ocaml-bisect-fix-camlp4-in-another-directory.patch"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("camlp4" ,camlp4)
       ("libxml2" ,libxml2)
       ("which" ,which)))
    (propagated-inputs
     `(("camlp4" ,camlp4)))
    (arguments
     `(#:test-target "tests"
       #:make-flags
       (list "all" (string-append "CAMLP4_LIBDIR="
                                  (assoc-ref %build-inputs "camlp4")
                                  "/lib/ocaml/site-lib/camlp4"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "./configure" "-prefix"
                             (assoc-ref outputs "out"))))))))
    (home-page "http://bisect.x9c.fr")
    (synopsis "Code coverage tool for the OCaml language")
    (description "Bisect is a code coverage tool for the OCaml language.  It is
a camlp4-based tool that allows to instrument your application before running
tests.  After application execution, it is possible to generate a report in HTML
format that is the replica of the application source code annotated with code
coverage information.")
    (properties `((ocaml4.01-variant . ,(delay ocaml4.01-bisect))))
    (license license:gpl3+)))

(define-public ocaml4.01-bisect
  (let ((base (package-with-ocaml4.01 (strip-ocaml4.01-variant ocaml-bisect))))
    (package
      (inherit base)
      (arguments
       `(#:ocaml ,ocaml-4.01
         ;; Camlp4 is included with OCaml 4.01, so do not include it as a
         ;; separate input.
         ,@(strip-keyword-arguments '(#:make-flags) (package-arguments base))))
      (native-inputs `(,@(alist-delete "camlp4" (package-native-inputs base))))
      (propagated-inputs
       `(,@(alist-delete "camlp4" (package-propagated-inputs base)))))))

(define-public ocaml-bitstring
  (package
    (name "ocaml-bitstring")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/xguerin/bitstring"
                                  "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "0vy8ibrxccii1jbsk5q6yh1kxjigqvi7lhhcmizvd5gfhf7mfyc8"))
              (patches (search-patches "ocaml-bitstring-fix-configure.patch"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("camlp4" ,camlp4)
       ("time" ,time)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bisect" ,ocaml-bisect)))
    (propagated-inputs
     `(("camlp4" ,camlp4)))
    (arguments
     `(#:configure-flags
       (list "CAMLP4OF=camlp4of" "--enable-coverage")
       #:make-flags
       (list (string-append "BISECTLIB="
                            (assoc-ref %build-inputs "bisect")
                            "/lib/ocaml/site-lib")
             (string-append "OCAMLCFLAGS=-g -I "
                            (assoc-ref %build-inputs "camlp4")
                            "/lib/ocaml/site-lib/camlp4 -I "
                            "$(BISECTLIB)/bisect")
             (string-append "OCAMLOPTFLAGS=-g -I "
                            (assoc-ref %build-inputs "camlp4")
                            "/lib/ocaml/site-lib/camlp4 -I "
                            "$(BISECTLIB)/bisect"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-configure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("@abs_top_builddir@")
                (string-append "@abs_top_builddir@:" (getenv "LIBRARY_PATH"))))
             (substitute* "configure"
               (("-/bin/sh") (string-append "-" (assoc-ref inputs "bash")
                                            "/bin/sh")))))
         (add-after 'install 'link-lib
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (stubs (string-append out
                                          "/lib/ocaml/site-lib/stubslibs"))
                    (lib (string-append out
                                        "/lib/ocaml/site-lib/bitstring")))
               (mkdir-p stubs)
               (symlink (string-append lib "/dllbitstring.so")
                        (string-append stubs "/dllbitstring.so"))))))))
    (home-page "https://github.com/xguerin/bitstring")
    (synopsis "Bitstrings and bitstring matching for OCaml")
    (description "Adds Erlang-style bitstrings and matching over bitstrings as
a syntax extension and library for OCaml.  You can use this module to both parse
and generate binary formats, files and protocols.  Bitstring handling is added
as primitives to the language, making it exceptionally simple to use and very
powerful.")
    (license license:isc)))

(define-public ocaml-result
  (package
    (name "ocaml-result")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/janestreet/result"
                                  "/archive/" version ".tar.gz"))
              (sha256
               (base32
                "1pgpfsgvhxnh0i37fkvp9j8nadns9hz9iqgabj4dr519j2gr1xvw"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/janestreet/result")
    (synopsis "Compatibility Result module")
    (description "Uses the new result type defined in OCaml >= 4.03 while
staying compatible with older version of OCaml should use the Result module
defined in this library.")
    (license license:bsd-3)))

(define-public ocaml-topkg
  (package
    (name "ocaml-topkg")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/topkg/releases/"
                                  "topkg-" version ".tbz"))
              (sha256
               (base32
                "18rrh6fmf708z7dd30amljmcgaypj3kk49jrmrj68r4wnw8004j8"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("opam" ,opam)))
    (propagated-inputs
     `(("result" ,ocaml-result)))
    (arguments
     `(#:tests? #f
       #:build-flags '("build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "http://erratique.ch/software/topkg")
    (synopsis "Transitory OCaml software packager")
    (description "Topkg is a packager for distributing OCaml software. It
provides an API to describe the files a package installs in a given build
configuration and to specify information about the package's distribution,
creation and publication procedures.")
    (license license:isc)))

(define-public ocaml-rresult
  (package
    (name "ocaml-rresult")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/rresult/releases/"
                                  "rresult-" version ".tbz"))
              (sha256
               (base32
                "1xxycxhdhaq8p9vhwi93s2mlxjwgm44fcxybx5vghzgbankz9yhm"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("opam" ,opam)))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)))
    (arguments
     `(#:tests? #f
       #:build-flags '("build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "http://erratique.ch/software/rresult")
    (synopsis "Result value combinators for OCaml")
    (description "Handle computation results and errors in an explicit and
declarative manner, without resorting to exceptions.  It defines combinators
to operate on the result type available from OCaml 4.03 in the standard
library.")
    (license license:isc)))

(define-public ocaml-sqlite3
  (package
    (name "ocaml-sqlite3")
    (version "4.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mmottl/sqlite3-ocaml/releases/download/v"
             version "/sqlite3-ocaml-" version ".tar.gz"))
       (sha256
        (base32
         "14c1nir7c6bivajg0vyx853y7la7r5d25g1v5hjb2wfi73r15p1m"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("sqlite" ,sqlite)))
    (home-page "https://mmottl.github.io/sqlite3-ocaml")
    (synopsis "SQLite3 Bindings for OCaml")
    (description
     "SQLite3-OCaml is an OCaml library with bindings to the SQLite3 client
API.  Sqlite3 is a self-contained, serverless, zero-configuration,
transactional SQL database engine with outstanding performance for many use
cases.  These bindings are written in a way that enables a friendly
coexistence with the old (version 2) SQLite and its OCaml wrapper
@code{ocaml-sqlite}.")
    (license license:expat)))

(define-public ocaml4.01-sqlite3
  (package-with-ocaml4.01 ocaml-sqlite3))

(define-public ocaml-csv
  (package
    (name "ocaml-csv")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/Chris00/ocaml-csv/releases/download/"
         version "/csv-" version ".tar.gz"))
       (sha256
        (base32
         "0rv7x843vn6scxj87hzl01yqrl26rc27lr8s7z6rp9vs745g05zj"))))
    (build-system ocaml-build-system)
    (home-page "https://github.com/Chris00/ocaml-csv")
    (synopsis "Pure OCaml functions to read and write CSV")
    (description
     "@dfn{Comma separated values} (CSV) is a simple tabular format supported
by all major spreadsheets.  This library implements pure OCaml functions to
read and write files in this format as well as some convenience functions to
manipulate such data.")
    (license (package-license camlp4))))

(define-public ocaml4.01-csv
  (package-with-ocaml4.01 ocaml-csv))

(define-public ocaml-mtime
  (package
    (name "ocaml-mtime")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/mtime/releases/"
                                  "mtime-" version ".tbz"))
              (sha256
               (base32
                "1hfx4ny2dkw6jf3jppz0640dafl5xgn8r2si9kpwzhmibal8qrah"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("opam" ,opam)))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)))
    (arguments
     `(#:tests? #f
       #:build-flags
       '("native=true" "native-dynlink=true" "jsoo=false")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "http://erratique.ch/software/mtime")
    (synopsis "Monotonic wall-clock time for OCaml")
    (description "Access monotonic wall-clock time.  It allows to measure time
spans without being subject to operating system calendar time adjustments.")
    (license license:isc)))

(define-public ocaml-cmdliner
  (package
    (name "ocaml-cmdliner")
    (version "0.9.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/cmdliner/releases/"
                                  "cmdliner-" version ".tbz"))
              (sha256
               (base32
                "0hdxlkgiwjml9dpaa80282a8350if7mc1m6yz2mrd7gci3fszykx"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("opam" ,opam)))
    (arguments
     `(#:tests? #f
       #:build-flags '("native=true" "native-dynlink=true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "http://erratique.ch/software/cmdliner")
    (synopsis "Declarative definition of command line interfaces for OCaml")
    (description "Cmdliner is a module for the declarative definition of command
line interfaces.  It provides a simple and compositional mechanism to convert
command line arguments to OCaml values and pass them to your functions.  The
module automatically handles syntax errors, help messages and UNIX man page
generation. It supports programs with single or multiple commands and respects
most of the POSIX and GNU conventions.")
    (license license:bsd-3)))

(define-public ocaml-fmt
  (package
    (name "ocaml-fmt")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://erratique.ch/software/fmt/releases/fmt-"
                            version ".tbz"))
        (sha256 (base32
                  "16y7ibndnairb53j8a6qgipyqwjxncn4pl9jiw5bxjfjm59108px"))))
    (build-system ocaml-build-system)
    (native-inputs `(("opam" ,opam)
                     ("topkg" ,ocaml-topkg)))
    (propagated-inputs `(("result" ,ocaml-result)
                         ("cmdliner" ,ocaml-cmdliner)))
    (arguments `(#:tests? #f
                 #:build-flags (list "build" "--with-base-unix" "true"
                                     "--with-cmdliner" "true")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (home-page "http://erratique.ch/software/fmt")
    (synopsis "OCaml Format pretty-printer combinators")
    (description "Fmt exposes combinators to devise Format pretty-printing
functions.")
    (license license:isc)))

(define-public ocaml-astring
  (package
    (name "ocaml-astring")
    (version "0.8.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://erratique.ch/software/astring/releases/astring-"
                            version ".tbz"))
        (sha256 (base32
                  "0ixjwc3plrljvj24za3l9gy0w30lsbggp8yh02lwrzw61ls4cri0"))))
    (build-system ocaml-build-system)
    (native-inputs `(("opam" ,opam)
                     ("topkg" ,ocaml-topkg)))
    (arguments `(#:tests? #f
                 #:build-flags (list "build")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (home-page "http://erratique.ch/software/astring")
    (synopsis "Alternative String module for OCaml")
    (description "Astring exposes an alternative String module for OCaml.  This
module balances minimality and expressiveness for basic, index-free, string
processing and provides types and functions for substrings, string sets and
string maps.  The String module exposed by Astring has exception safe functions,
removes deprecated and rarely used functions, alters some signatures and names,
adds a few missing functions and fully exploits OCaml's newfound string
immutability.")
    (license license:isc)))

(define-public ocaml-alcotest
  (package
    (name "ocaml-alcotest")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mirage/alcotest/releases/"
                                  "download/" version "/alcotest-" version ".tbz"))
              (sha256
                (base32
                  "0g5lzk0gpfx4q8hyhr460gr4lab5wakfxsmhfwvb3yinxwzs95gc"))))
    (build-system ocaml-build-system)
    (arguments `(#:tests? #f
                 #:build-flags (list "build")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs `(("opam" ,opam)
                     ("topkg" ,ocaml-topkg)))
    (propagated-inputs `(("fmt" ,ocaml-fmt)
                         ("astring" ,ocaml-astring)))
    (home-page "https://github.com/mirage/alcotest")
    (synopsis "Lightweight OCaml test framework")
    (description "Alcotest exposes simple interface to perform unit tests.  It
exposes a simple TESTABLE module type, a check function to assert test
predicates and a run function to perform a list of unit -> unit test callbacks.
Alcotest provides a quiet and colorful output where only faulty runs are fully
displayed at the end of the run (with the full logs ready to inspect), with a
simple (yet expressive) query language to select the tests to run.")
    (license license:isc)))

(define-public ocaml-ppx-tools
  (package
    (name "ocaml-ppx-tools")
    (version "5.0+4.02.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/alainfrisch/ppx_tools/archive/"
                            version ".tar.gz"))
        (sha256 (base32
                  "0rjg4rngi8k9873z4zq95zn9hj8qyw1vcrf11y15aqasfpqq16rc"))))
    (build-system ocaml-build-system)
    (arguments `(#:phases (modify-phases %standard-phases (delete 'configure))
                 #:tests? #f))
    (home-page "https://github.com/alainfrisch/ppx_tools")
    (synopsis "Tools for authors of ppx rewriters and other syntactic tools")
    (description "Tools for authors of ppx rewriters and other syntactic tools.")
    (license license:expat)))

(define-public ocaml-react
  (package
    (name "ocaml-react")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://erratique.ch/software/react/releases/react-"
                            version ".tbz"))
        (sha256 (base32
                  "0knhgbngphv5sp1yskfd97crf169qhpc0igr6w7vqw0q36lswyl8"))))
    (build-system ocaml-build-system)
    (native-inputs `(("opam" ,opam)))
    (arguments `(#:tests? #f
                 #:build-flags (list "native=true" "native-dynlink=true")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (home-page "http://erratique.ch/software/react")
    (synopsis "Declarative events and signals for OCaml")
    (description "React is an OCaml module for functional reactive programming
(FRP).  It provides support to program with time varying values: declarative
events and signals.  React doesn't define any primitive event or signal, it
lets the client choose the concrete timeline.")
    (license license:bsd-3)))

(define-public ocaml-ssl
  (package
    (name "ocaml-ssl")
    (version "0.5.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/savonet/ocaml-ssl/archive/"
                            version ".tar.gz"))
        (sha256 (base32
                  "15p7652cvzdrlqxc1af11mg07wasxr1fsaj44gcmmh6bmav7wfzq"))))
    (build-system ocaml-build-system)
    (arguments `(#:tests? #f
                 #:make-flags (list "OCAMLFIND_LDCONF=ignore")
                 #:phases
                 (modify-phases %standard-phases
                   (add-after 'unpack 'bootstrap
                     (lambda* (#:key #:allow-other-keys)
                       (system* "sh" "bootstrap")
                       (substitute* "src/OCamlMakefile"
                         (("/bin/sh") (which "bash")))
                       (substitute* "configure"
                         (("/bin/sh") (which "bash"))))))))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("which" ,which)))
    (propagated-inputs `(("openssl" ,openssl)))
    (home-page "https://github.com/savonet/ocaml-ssl/")
    (synopsis "OCaml bindings for OpenSSL")
    (description
     "OCaml-SSL is a set of bindings for OpenSSL, a library for communicating
through Transport Layer Security (@dfn{TLS}) encrypted connections.")
    (license license:lgpl2.1)))

(define-public ocaml-lwt
  (package
    (name "ocaml-lwt")
    (version "2.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ocsigen/lwt/archive/" version
                            ".tar.gz"))
        (sha256 (base32
                  "1gbw0g8a5a4b16diqrmlhc8ilnikrm4w3jjm1zq310maqg8z0zxz"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-ssl" "--enable-glib" "--enable-react"
             "--enable-ppx")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-some-checks
           (lambda* (#:key #:allow-other-keys)
             (substitute* "tests/unix/main.ml"
               (("Test_mcast.suite;") ""))))
         (add-after 'install 'link-stubs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
                    (lib (string-append out "/lib/ocaml/site-lib/lwt")))
               (mkdir-p stubs)
               (symlink (string-append lib "/dlllwt-glib_stubs.so")
                        (string-append stubs "/dlllwt-glib_stubs.so"))
               (symlink (string-append lib "/dlllwt-unix_stubs.so")
                        (string-append stubs "/dlllwt-unix_stubs.so"))))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("ppx-tools" ,ocaml-ppx-tools)))
    (inputs `(("libev" ,libev)
              ("glib" ,glib)))
    (propagated-inputs `(("result" ,ocaml-result)
                         ("ocaml-ssl" ,ocaml-ssl)
                         ("ocaml-react" ,ocaml-react)))
    (home-page "https://github.com/ocsigen/lwt")
    (synopsis "Cooperative threads and I/O in monadic style")
    (description "Lwt provides typed, composable cooperative threads.  These
make it easy to run normally-blocking I/O operations concurrently in a single
process.  Also, in many cases, Lwt threads can interact without the need for
locks or other synchronization primitives.")
    (license license:lgpl2.1)))

(define-public ocaml-logs
  (package
    (name "ocaml-logs")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/logs/releases/"
                                  "logs-" version ".tbz"))
              (sha256
                (base32
                  "1khbn7jqpid83zn8rvyh1x1sirls7zc878zj4fz985m5xlsfy853"))))
    (build-system ocaml-build-system)
    (arguments `(#:tests? #f
                 #:build-flags (list "build" "--with-js_of_ocaml" "false")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs `(("opam" ,opam)))
    (propagated-inputs `(("fmt" ,ocaml-fmt)
                         ("lwt" ,ocaml-lwt)
                         ("mtime" ,ocaml-mtime)
                         ("result" ,ocaml-result)
                         ("cmdliner" ,ocaml-cmdliner)
                         ("topkg" ,ocaml-topkg)))
    (home-page "http://erratique.ch/software/logs")
    (synopsis "Logging infrastructure for OCaml")
    (description "Logs provides a logging infrastructure for OCaml.  Logging is
performed on sources whose reporting level can be set independently.  Log
message report is decoupled from logging and is handled by a reporter.")
    (license license:isc)))

(define-public ocaml-fpath
  (package
    (name "ocaml-fpath")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/fpath/releases/"
                                  "fpath-" version ".tbz"))
              (sha256
                (base32
                  "05134ij27xjl6gaqsc65yl19vfj6cjxq3mbm9bf4mija8grdpn6g"))))
    (build-system ocaml-build-system)
    (arguments `(#:tests? #f
                 #:build-flags (list "build")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs `(("opam" ,opam)))
    (propagated-inputs `(("topkg" ,ocaml-topkg)
                         ("astring" ,ocaml-astring)))
    (home-page "http://erratique.ch/software/fpath")
    (synopsis "File system paths for OCaml")
    (description "Fpath is an OCaml module for handling file system paths with
POSIX or Windows conventions.  Fpath processes paths without accessing the
file system and is independent from any system library.")
    (license license:isc)))

(define-public ocaml-bos
  (package
    (name "ocaml-bos")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/bos/releases/"
                                  "bos-" version ".tbz"))
              (sha256
                (base32
                  "1ly66lysk4w6mdy4k1n3ynlpfpq7lw4wshcpzgx58v6x613w5s7q"))))
    (build-system ocaml-build-system)
    (arguments `(#:tests? #f
                 #:build-flags (list "build")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs `(("opam" ,opam)))
    (propagated-inputs `(("topkg" ,ocaml-topkg)
                         ("astring" ,ocaml-astring)
                         ("fmt" ,ocaml-fmt)
                         ("fpath" ,ocaml-fpath)
                         ("logs" ,ocaml-logs)
                         ("rresult" ,ocaml-rresult)))
    (home-page "http://erratique.ch/software/bos")
    (synopsis "Basic OS interaction for OCaml")
    (description "Bos provides support for basic and robust interaction with
the operating system in OCaml.  It has functions to access the process
environment, parse command line arguments, interact with the file system and
run command line programs.")
    (license license:isc)))

(define-public ocaml-xmlm
  (package
    (name "ocaml-xmlm")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/xmlm/releases/"
                                  "xmlm-" version ".tbz"))
              (sha256
                (base32
                  "1jywcrwn5z3gkgvicr004cxmdaqfmq8wh72f81jqz56iyn5024nh"))))
    (build-system ocaml-build-system)
    (arguments `(#:tests? #f
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure)
                   (replace 'build
                     (lambda* (#:key #:allow-other-keys)
                       (zero? (system* "pkg/build" "true")))))))
    (native-inputs `(("opam" ,opam)))
    (home-page "http://erratique.ch/software/xmlm")
    (synopsis "Streaming XML codec for OCaml")
    (description "Xmlm is a streaming codec to decode and encode the XML data
format.  It can process XML documents without a complete in-memory
representation of the data.")
    (license license:isc)))

(define-public ocaml4.01-xmlm
  (package-with-ocaml4.01 ocaml-xmlm))

(define-public ocaml-ulex
  (package
    (name "ocaml-ulex")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.cduce.org/download/ulex-"
                                  version ".tar.gz"))
              (sha256
                (base32
                  "0fjlkwps14adfgxdrbb4yg65fhyimplvjjs1xqj5np197cig67x0"))))
    (build-system ocaml-build-system)
    (arguments `(#:phases (modify-phases %standard-phases (delete 'configure))
                 #:tests? #f
                 #:make-flags
                 (list "all.opt"
                       (string-append "OCAMLBUILD=ocamlbuild -byte-plugin "
                                      "-cflags -I,"
                                      (assoc-ref %build-inputs "camlp4")
                                      "/lib/ocaml/site-lib/camlp4"))))
    (native-inputs `(("camlp4" ,camlp4)))
    (home-page "http://www.cduce.org/download.html#side")
    (synopsis "Lexer generator for Unicode and OCaml")
    (description "Lexer generator for Unicode and OCaml.")
    (license license:expat)))

(define-public ocaml-uchar
  (package
    (name "ocaml-uchar")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ocaml/uchar/releases/download/v"
                            version "/uchar-" version ".tbz"))
        (sha256 (base32
                  "0ficw1x7ymbd6m8hqw3w1aycwm1hbwd6bad3c5pspwnzh3qlikhi"))))
    (build-system ocaml-build-system)
    (arguments `(#:tests? #f
                 #:build-flags (list "native=true" "native-dynlink=true")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs `(("opam" ,opam)))
    (home-page "https://github.com/ocaml/uchar")
    (synopsis "Compatibility library for OCaml's Uchar module")
    (description "The uchar package provides a compatibility library for the
`Uchar` module introduced in OCaml 4.03.")
    (license license:lgpl2.1)))

(define-public ocaml-uutf
  (package
    (name "ocaml-uutf")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/uutf/releases/"
                                  "uutf-" version ".tbz"))
              (sha256
                (base32
                  "08i0cw02cxw4mi2rs01v9xi307qshs6fnd1dlqyb52kcxzblpp37"))))
    (build-system ocaml-build-system)
    (arguments `(#:tests? #f
                 #:build-flags (list "build")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs `(("opam" ,opam)
                     ("topkg" ,ocaml-topkg)))
    (propagated-inputs `(("uchar" ,ocaml-uchar)
                         ("cmdliner" ,ocaml-cmdliner)))
    (home-page "http://erratique.ch/software/uutf")
    (synopsis "Non-blocking streaming Unicode codec for OCaml")
    (description "Uutf is a non-blocking streaming codec to decode and encode
the UTF-8, UTF-16, UTF-16LE and UTF-16BE encoding schemes.  It can efficiently
work character by character without blocking on IO.  Decoders perform character
position tracking and support newline normalization.

Functions are also provided to fold over the characters of UTF encoded OCaml
string values and to directly encode characters in OCaml Buffer.t values.")
    (license license:isc)))

(define-public ocaml-jsonm
  (package
    (name "ocaml-jsonm")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/jsonm/releases/"
                                  "jsonm-" version ".tbz"))
              (sha256
                (base32
                  "1v3ln6d965lplj28snjdqdqablpp1kx8bw2cfx0m6i157mqyln62"))))
    (build-system ocaml-build-system)
    (arguments `(#:tests? #f
                 #:build-flags (list "build")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs `(("opam" ,opam)
                     ("topkg" ,ocaml-topkg)))
    (propagated-inputs `(("uutf" ,ocaml-uutf)
                         ("cmdliner" ,ocaml-cmdliner)))
    (home-page "http://erratique.ch/software/jsonm")
    (synopsis "Non-blocking streaming JSON codec for OCaml")
    (description "Jsonm is a non-blocking streaming codec to decode and encode
the JSON data format.  It can process JSON text without blocking on IO and
without a complete in-memory representation of the data.")
    (license license:isc)))

(define-public ocaml-ocurl
  (package
    (name "ocaml-ocurl")
    (version "0.7.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ygrek.org.ua/p/release/ocurl/ocurl-"
                                  version ".tar.gz"))
              (sha256
                (base32
                  "0pm6nm33wi0p9h765k6zb94ljpknryam4qd1hmb2rsk2y0y1181n"))))
    (build-system ocaml-build-system)
    (arguments `(#:phases
                 (modify-phases %standard-phases
                   (add-before 'configure 'fix-/bin/sh
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "configure"
                         (("-/bin/sh") (string-append "-" (which "bash")))))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("curl" ,curl)))
    (home-page "http://ocurl.forge.ocamlcore.org/")
    (synopsis "OCaml bindings for libcurl")
    (description "Client-side URL transfer library, supporting HTTP and a
multitude of other network protocols (FTP/SMTP/RTSP/etc).")
    (license license:isc)))

(define-public ocaml-base64
  (package
    (name "ocaml-base64")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mirage/ocaml-base64/"
                                  "releases/download/v" version "/base64-"
                                   version ".tbz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                 "1p45sawchmrkr22gkmydjc4ary23pisp58zsnb7iq7d82nxs1lfq"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags (list "build" "--tests" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("topkg" ,ocaml-topkg)
       ("opam" ,opam)
       ("rresult" ,ocaml-rresult)
       ("bos" ,ocaml-bos)
       ("alcotest" ,ocaml-alcotest)))
    (home-page "https://github.com/mirage/ocaml-base64")
    (synopsis "Base64 encoding for OCaml")
    (description "Base64 is a group of similar binary-to-text encoding schemes
that represent binary data in an ASCII string format by translating it into a
radix-64 representation.  It is specified in RFC 4648.")
    (license license:isc)))

(define-public ocamlify
  (package
    (name "ocamlify")
    (version "0.0.2")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1209))
              (sha256
               (base32
                "1f0fghvlbfryf5h3j4as7vcqrgfjb4c8abl5y0y5h069vs4kp5ii"))))
    (build-system ocaml-build-system)
    ; tests are done during build
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (home-page "https://forge.ocamlcore.org/projects/ocamlify")
    (synopsis "Include files in OCaml code")
    (description "OCamlify allows to create OCaml source code by including
whole files into OCaml string or string list.  The code generated can be
compiled as a standard OCaml file.  It allows embedding external resources as
OCaml code.")
    (license license:lgpl2.1+))); with the OCaml static compilation exception

(define-public omake
  (package
    (name "omake")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.camlcity.org/download/"
                                  "omake-" version ".tar.gz"))
              (sha256
               (base32
                "093ansbppms90hiqvzar2a46fj8gm9iwnf8gn38s6piyp70lrbsj"))
              (patches (search-patches "omake-fix-non-determinism.patch"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-makefile
                     (lambda* (#:key outputs #:allow-other-keys)
                       (substitute* "mk/osconfig_unix.mk"
                                    (("CC = cc") "CC = gcc")))))))
    (native-inputs `(("hevea" ,hevea)))
    (home-page "http://projects.camlcity.org/projects/omake.html")
    (synopsis "Build system designed for scalability and portability")
    (description "Similar to make utilities you may have used, but it features
many additional enhancements, including:

@enumerate
@item Support for projects spanning several directories or directory hierarchies.
@item Fast, reliable, automated, scriptable dependency analysis using MD5 digests,
      with full support for incremental builds.
@item Dependency analysis takes the command lines into account — whenever the
      command line used to build a target changes, the target is considered
      out-of-date.
@item Fully scriptable, includes a library that providing support for standard
      tasks in C, C++, OCaml, and LaTeX projects, or a mixture thereof.
@end enumerate")
    (license (list license:lgpl2.1 ; libmojave
                   license:expat ; OMake scripts
                   license:gpl2)))) ; OMake itself, with ocaml linking exception
                                    ; see LICENSE.OMake

(define-public ocaml-batteries
  (package
    (name "ocaml-batteries")
    (version "2.5.3")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri "batteries" version 1650))
              (sha256
               (base32
                "1a97w3x2l1jr5x9kj5gqm1x6b0q9fjqdcsvls7arnl3bvzgsia0n"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("qtest" ,ocaml-qtest)
       ("bisect" ,ocaml-bisect)
       ("ounit" ,ocaml-ounit)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check) ; tests are run by the build phase
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "ocaml" "setup.ml" "-build")))))))
    (home-page "http://batteries.forge.ocamlcore.org/")
    (synopsis "Development platform for the OCaml programming language")
    (description "Define a standard set of libraries which may be expected on
every compliant installation of OCaml and organize these libraries into a
hierarchy of modules.")
    (license license:lgpl2.1+)))

(define-public ocaml4.01-batteries
  (package-with-ocaml4.01 ocaml-batteries))

(define-public ocaml-pcre
  (package
    (name "ocaml-pcre")
    (version "7.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mmottl/pcre-ocaml/archive"
                                  "/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rj6dw79px4sj2kq0iss2nzq3rnsn9wivvc0f44wa1mppr6njfb3"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'link-lib
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
                    (lib (string-append out "/lib/ocaml/site-lib/pcre")))
               (mkdir-p stubs)
               (symlink (string-append lib "/dllpcre_stubs.so")
                        (string-append stubs "/dllpcre_stubs.so"))))))))
    (native-inputs
     `(("batteries" ,ocaml-batteries)
       ("pcre:bin" ,pcre "bin")))
    (propagated-inputs `(("pcre" ,pcre)))
    (home-page "https://mmottl.github.io/pcre-ocaml")
    (synopsis "Bindings to the Perl Compatibility Regular Expressions library")
    (description "Pcre-ocaml offers library functions for string pattern
matching and substitution, similar to the functionality offered by the Perl
language.")
    (license license:lgpl2.1+))); with the OCaml link exception

(define-public ocaml-expect
  (package
    (name "ocaml-expect")
    (version "0.0.5")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1372))
              (sha256
               (base32
                "07xq8w2x2vffc32z7vk6y14jwbfb1cw0m2lm1jzi60hnr1dvg8by"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ocaml-pcre" ,ocaml-pcre)
       ("ounit" ,ocaml-ounit)))
    (propagated-inputs `(("batteries" ,ocaml-batteries)))
    (home-page "https://forge.ocamlcore.org/projects/ocaml-expect/")
    (synopsis "Simple implementation of expect")
    (description "Help building unitary testing of interactive program.  You
can match the question using a regular expression or a timeout.")
    (license license:lgpl2.1+))) ; with the OCaml static compilation exception

(define-public ocaml-fileutils
  (package
    (name "ocaml-fileutils")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1651))
              (sha256
               (base32
                "0g6zx2rcvacklxyli19ixcf6ich9ipxsps4k3jz98f5zlaab0a7g"))))
    (build-system ocaml-build-system)
    (native-inputs `(("ounit" ,ocaml-ounit)))
    (home-page "http://ocaml-fileutils.forge.ocamlcore.org")
    (synopsis "Pure OCaml functions to manipulate real file and filename")
    (description "Library to provide pure OCaml functions to manipulate real
file (POSIX like) and filename.")
    (license license:lgpl2.1+))) ; with the OCaml static compilation exception

(define-public ocaml-oasis
  (package
    (name "ocaml-oasis")
    (version "0.4.8")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1669))
              (sha256
               (base32
                "1ln7vc7ip6s5xbi20mhnn087xi4a2m5vqawx0703qqnfkzhmslqy"))
            (modules '((guix build utils)))
            (snippet
             '(substitute* "test/test-main/Test.ml"
                ;; most of these tests fail because ld cannot find crti.o, but according
                ;; to the log file, the environment variables {LD_,}LIBRARY_PATH
                ;; are set correctly whene LD_LIBRARY_PATH is defined beforhand.
                (("TestBaseCompat.tests;") "")
                (("TestExamples.tests;") "")
                (("TestFull.tests;") "")
                (("TestPluginDevFiles.tests;") "")
                (("TestPluginInternal.tests;") "")
                (("TestPluginOCamlbuild.tests;") "")
                (("TestPluginOMake.tests;") "")))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ocamlify" ,ocamlify)
       ("ocamlmod" ,ocamlmod)
       ("ounit" ,ocaml-ounit)
       ("omake" ,omake)
       ("ocaml-expect" ,ocaml-expect)
       ("ocaml-pcre" ,ocaml-pcre)
       ("ocaml-fileutils" ,ocaml-fileutils)
       ("camlp4" ,camlp4)
       ("texlive" ,texlive)
       ("pkg-config" ,pkg-config)))
    (home-page "https://oasis.forge.ocamlcore.org")
    (synopsis "Integrates a configure, build, install system in OCaml projects")
    (description "OASIS is a tool to integrate a configure, build and install
system in your OCaml projects.  It helps to create standard entry points in your
build system and allows external tools to analyse your project easily.")
    (license license:lgpl2.1+))) ; with ocaml static compilation exception

(define-public ocaml-js-build-tools
  (package
    (name "ocaml-js-build-tools")
    (version "113.33.06")
    (source (janestreet-origin "js-build-tools" version
              "0r8z4fz8iy5y6hkdlkpwf6rk4qigcr3dzyv35585xgg2ahf12zy6"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("opam" ,opam)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/js-build-tools")
    (synopsis "Collection of tools to help building Jane Street Packages")
    (description "This package contains tools to help building Jane Street
packages, but can be used for other purposes.  It contains:
@enumerate
@item an @command{oasis2opam-install} tool to produce a @file{.install} file
from the oasis build log
@item a @code{js_build_tools} ocamlbuild plugin with various goodies.
@end enumerate")
    (license license:asl2.0)))

(define-public ocaml-bin-prot
  (package
    (name "ocaml-bin-prot")
    (version "113.33.03")
    (source (janestreet-origin "bin_prot" version
               "1ws8c017z8nbj3vw92ndvjk9011f71rmp3llncbv8r5fc76wqv3l"))
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/bin_prot/")
    (synopsis "Binary protocol generator")
    (description "This library contains functionality for reading and writing
OCaml-values in a type-safe binary protocol.  It is extremely efficient,
typically supporting type-safe marshalling and unmarshalling of even highly
structured values at speeds sufficient to saturate a gigabit connection.  The
protocol is also heavily optimized for size, making it ideal for long-term
storage of large amounts of data.")
    (license license:asl2.0)))

(define-public ocaml-fieldslib
  (package
    (name "ocaml-fieldslib")
    (version "113.33.03")
    (source (janestreet-origin "fieldslib" version
               "1rm3bn54bzk2hlq8f3w067ak8s772w4a8f78i3yh79vxfzq8ncvv"))
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/fieldslib/")
    (synopsis "Syntax extension to record fields")
    (description "Syntax extension to define first class values representing
record fields, to get and set record fields, iterate and fold over all fields
of a record and create new record values.")
    (license license:asl2.0)))

(define-public ocaml-ppx-core
  (package
    (name "ocaml-ppx-core")
    (version "113.33.03")
    (source (janestreet-origin "ppx_core" version
               "0f69l4czhgssnhb5ds2j9dbqjyz8dp1y3i3x0i4h6pxnms20zbxa"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (inputs `(("ppx-tools" ,ocaml-ppx-tools)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_core/")
    (synopsis "Standard library for ppx rewriters")
    (description "Ppx_core is a standard library for OCaml AST transformers.
It contains:
@enumerate
@item various auto-generated AST traversal using an open recursion scheme
@item helpers for building AST fragments
@item helpers for matching AST fragments
@item a framework for dealing with attributes and extension points.
@end enumerate")
    (license license:asl2.0)))

(define-public ocaml-ppx-optcomp
  (package
    (name "ocaml-ppx-optcomp")
    (version "113.33.03")
    (source (janestreet-origin "ppx_optcomp" version
               "13an8p2r7sd0d5lv54mlzrxdni47408bwqi3bjcx4m6005170q30"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_optcomp/")
    (synopsis "Optional compilation for OCaml")
    (description "Ppx_optcomp stands for Optional Compilation.  It is a tool
used to handle optional compilations of pieces of code depending of the word
size, the version of the compiler, ...")
    (license license:asl2.0)))

(define-public ocaml-ppx-driver
  (package
    (name "ocaml-ppx-driver")
    (version "113.33.03")
    (source (janestreet-origin "ppx_driver" version
              "011zzr45897j49b7iiybb29k7pspcx36mlnp7nh6pxb8b0ga76fh"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)
       ("ppx-optcomp" ,ocaml-ppx-optcomp)))
    (propagated-inputs
     `(("ppx-optcomp" ,ocaml-ppx-optcomp)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_driver/")
    (synopsis "Feature-full driver for OCaml AST transformers")
    (description "A driver is an executable created from a set of OCaml AST
transformers linked together with a command line frontend.  The aim is to
provide a tool that can be used to:
@enumerate
@item easily view the pre-processed version of a file, no need to construct a
      complex command line: @command{ppx file.ml} will do
@item use a single executable to run several transformations: no need to fork
      many times just for pre-processing
@item improved errors for misspelled/misplaced attributes and extension points.
@end enumerate")
    (license license:asl2.0)))

(define-public ocaml-cppo
  (package
    (name "ocaml-cppo")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/mjambon/cppo/archive/v" version
                            ".tar.gz"))
        (sha256 (base32
                  "02gma6nw09vfnd6h7bl3n70lwz7m9z2svplxyfh6h5bf4lqgqzjv"))
        (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases (delete 'configure))
       #:make-flags (list (string-append "BINDIR="
                                         (assoc-ref %outputs "out") "/bin"))
       #:tests? #f))
    (home-page "https://github.com/mjambon/cppo")
    (synopsis "Equivalent of the C preprocessor for OCaml programs")
    (description "Cppo is an equivalent of the C preprocessor for OCaml
programs.  It allows the definition of simple macros and file inclusion.  Cpp ois:
@enumerate
@item more OCaml-friendly than cpp
@item easy to learn without consulting a manual
@item reasonably fast
@item simple to install and to maintain.
@end enumerate")
    (license license:bsd-3)))

;; this package is not reproducible. This is related to temporary filenames
;; such as findlib_initxxxxx where xxxxx is random.
(define-public ocaml-ppx-deriving
  (package
    (name "ocaml-ppx-deriving")
    (version "4.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/whitequark/ppx_deriving//archive/v"
                            version ".tar.gz"))
        (sha256 (base32
                  "1fr16g121j6zinwcprzlhx2py4271n9jzs2m9hq2f3qli2b1p0vl"))
        (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("cppo" ,ocaml-cppo)
       ("ounit" ,ocaml-ounit)
       ("opam" ,opam)))
    (propagated-inputs
     `(("result" ,ocaml-result)
       ("ppx-tools" ,ocaml-ppx-tools)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
           (add-before 'install 'fix-environment
             (lambda* (#:key outputs #:allow-other-keys)
               ;; the installation procedures looks for the installed module
               (setenv "OCAMLPATH"
                       (string-append (getenv "OCAMLPATH") ":"
                                      (getenv "OCAMLFIND_DESTDIR"))))))))
    (home-page "https://github.com/whitequark/ppx_deriving/")
    (synopsis "Type-driven code generation for OCaml >=4.02")
    (description "Ppx_deriving provides common infrastructure for generating
code based on type definitions, and a set of useful plugins for common tasks.")
    (license license:expat)))

(define-public ocaml-ppx-type-conv
  (package
    (name "ocaml-ppx-type-conv")
    (version "113.33.03")
    (source
      (janestreet-origin "ppx_type_conv" version
        "1sp602ads2f250nv4d4rgw54d14k7flyhb4w8ff084f657hhmxv2"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-deriving" ,ocaml-ppx-deriving)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("result" ,ocaml-result)
       ("ppx-core" ,ocaml-ppx-core)
       ("ppx-driver" ,ocaml-ppx-driver)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_type_conv/")
    (synopsis "Support Library for type-driven code generators")
    (description "The type_conv library factors out functionality needed by
different preprocessors that generate code from type specifications.")
    (license license:asl2.0)))

(define-public ocaml-ppx-inline-test
  (package
    (name "ocaml-ppx-inline-test")
    (version "113.33.03")
    (source (janestreet-origin "ppx_inline_test" version
              "0859ni43fl39dd5g6cxfhq0prxmx59bd2bfq8jafp593ba4q0icq"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)
       ("ppx-core" ,ocaml-ppx-core)))
    (propagated-inputs
      `(("ppx-driver" ,ocaml-ppx-driver)
        ("ppx-tools" ,ocaml-ppx-tools)
        ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_inline_test/")
    (synopsis "Syntax extension for writing in-line tests in ocaml code")
    (description "Syntax extension for writing in-line tests in ocaml code.")
    (license license:asl2.0)))

(define-public ocaml-ppx-bench
  (package
    (name "ocaml-ppx-bench")
    (version "113.33.03")
    (source (janestreet-origin "ppx_bench" version
                   "1hky3y17qpb925rymf97wv54di9gaqdmkix7wpkjw14qzl512b68"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)
       ("ppx-core" ,ocaml-ppx-core)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-inline-test" ,ocaml-ppx-inline-test)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_bench/")
    (synopsis "Syntax extension for writing in-line benchmarks in ocaml code")
    (description "Syntax extension for writing in-line benchmarks in ocaml code.")
    (license license:asl2.0)))

(define-public ocaml-ppx-compare
  (package
    (name "ocaml-ppx-compare")
    (version "113.33.03")
    (source (janestreet-origin "ppx_compare" version
              "0bfhi33kq9l4q6zzc6svki2csracz5j4731c3npcy6cs73jynn0z"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)
       ("ppx-core" ,ocaml-ppx-core)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)
       ("ppx-type-conv" ,ocaml-ppx-type-conv)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_compare/")
    (synopsis "Generation of comparison functions from types")
    (description "Generation of fast comparison functions from type expressions
and definitions.  Ppx_compare is a ppx rewriter that derives comparison functions
from type representations.  The scaffolded functions are usually much faster
than ocaml's Pervasives.compare.  Scaffolding functions also gives you more
flexibility by allowing you to override them for a specific type and more safety
by making sure that you only compare comparable values.")
    (license license:asl2.0)))

(define-public ocaml-sexplib
  (package
    (name "ocaml-sexplib")
    (version "113.33.03")
    (source (janestreet-origin "sexplib" version
              "1ffjmj8if9lyv965cgn2ld1xv7g52qsr8mqflbm515ck1i8l2ima"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/sexplib/")
    (synopsis "Library for serializing OCaml values to and from S-expressions")
    (description "Sexplib contains functionality for parsing and pretty-printing
s-expressions.")
    (license license:asl2.0)))

(define-public ocaml-typerep
  (package
    (name "ocaml-typerep")
    (version "113.33.03")
    (source (janestreet-origin "typerep" version
              "1b9v5bmi824a9d4sx0f40ixq0yfcbiqxafg4a1jx95xg9199zafy"))
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/typerep/")
    (synopsis "Typerep is a library for runtime types")
    (description "Typerep is a library for runtime types.")
    (license license:asl2.0)))

(define-public ocaml-variantslib
  (package
    (name "ocaml-variantslib")
    (version "113.33.03")
    (source (janestreet-origin "variantslib" version
              "05vp799vl38fvl98ga5miwbzh09cnnpapi6q6gdvwyqi6w7s919n"))
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/variantslib")
    (synopsis "OCaml variants as first class values")
    (description "OCaml variants as first class values.")
    (license license:asl2.0)))

(define-public ocaml-ppx-sexp-conv
  (package
    (name "ocaml-ppx-sexp-conv")
    (version "113.33.03")
    (source (janestreet-origin "ppx_sexp_conv" version
              "1rbj6d5dl625gdxih34xcrdvikci6h8i2dl9x3wraa4qrgishiw7"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)
       ("ppx-core" ,ocaml-ppx-core)))
    (propagated-inputs
     `(("sexplib" ,ocaml-sexplib)
       ("ppx-core" ,ocaml-ppx-core)
       ("ppx-type-conv" ,ocaml-ppx-type-conv)
       ("ppx-tools" ,ocaml-ppx-tools)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_sexp_conv")
    (synopsis "Generation of S-expression conversion functions from type definitions")
    (description "Generation of S-expression conversion functions from type
definitions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-variants-conv
  (package
    (name "ocaml-ppx-variants-conv")
    (version "113.33.03")
    (source (janestreet-origin "ppx_variants_conv" version
              "0vnn2l1118cj72413d3f7frlw6yc09l8f64jlzkzbgb9bxpalx34"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-core" ,ocaml-ppx-core)
       ("variantslib" ,ocaml-variantslib)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-type-conv" ,ocaml-ppx-type-conv)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_variants_conv")
    (synopsis "Generation of accessor and iteration functions for ocaml variant
types")
    (description "Generation of accessor and iteration functions for ocaml
variant types.")
    (license license:asl2.0)))

(define-public ocaml-ppx-here
  (package
    (name "ocaml-ppx-here")
    (version "113.33.03")
    (source (janestreet-origin "ppx_here" version
              "1ay8lfxi0qg3ib2zkwh4h0vqk3gjmxaz572gzab0bbxyqn3z86v7"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_here")
    (synopsis "Expands [%here] into its location")
    (description "Expands [%here] into its location.")
    (license license:asl2.0)))

(define-public ocaml-ppx-assert
  (package
    (name "ocaml-ppx-assert")
    (version "113.33.03")
    (source (janestreet-origin "ppx_assert" version
              "1k5kxmqkibp5fk25pgz81f3c1r4mgvb5byzf6bnmxd24y60wn46p"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-compare" ,ocaml-ppx-compare)
       ("ppx-core" ,ocaml-ppx-core)
       ("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-type-conv" ,ocaml-ppx-type-conv)
       ("ppx-sexplib" ,ocaml-sexplib)
       ("ppx-here" ,ocaml-ppx-here)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_assert")
    (synopsis "Assert-like extension nodes that raise useful errors on failure")
    (description "Assert-like extension nodes that raise useful errors on failure.")
    (license license:asl2.0)))

(define-public ocaml-ppx-enumerate
  (package
    (name "ocaml-ppx-enumerate")
    (version "113.33.03")
    (source (janestreet-origin "ppx_enumerate" version
              "15g7yfv9wg2h9r6k6q1zrhygmsl4xrfn25mrb0i4czjjivzmxjh4"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-type-conv" ,ocaml-ppx-type-conv)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_enumerate")
    (synopsis "Generate a list containing all values of a finite type")
    (description "Ppx_enumerate is a ppx rewriter which generates a definition
for the list of all values of a type (for a type which only has finitely
many values).")
    (license license:asl2.0)))

(define-public ocaml-ppx-let
  (package
    (name "ocaml-ppx-let")
    (version "113.33.03")
    (source (janestreet-origin "ppx_let" version
              "0gd6d3gdaqfwjcs7gaw1qxc30i584q6a86ndaj1bx1q63xqd6yx9"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_let")
    (synopsis "Monadic let-bindings")
    (description "A ppx rewriter for monadic and applicative let bindings,
match expressions, and if expressions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-typerep-conv
  (package
    (name "ocaml-ppx-typerep-conv")
    (version "113.33.03")
    (source (janestreet-origin "ppx_typerep_conv" version
              "0g0xqm9s1b2jjvxb8yp69281q2s3bwz6sibn10fvgcdawpa0rmrg"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-type-conv" ,ocaml-ppx-type-conv)
       ("ppx-core" ,ocaml-ppx-core)
       ("typerep" ,ocaml-typerep)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_typerep_conv")
    (synopsis "Generation of runtime types from type declarations")
    (description "Automatic generation of runtime types from type definitions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-sexp-value
  (package
    (name "ocaml-ppx-sexp-value")
    (version "113.33.03")
    (source (janestreet-origin "ppx_sexp_value" version
              "0m3ag23mbqm0i2pv1dzilfks15ipa5q60mf57a0cd3p0pvarq10g"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-here" ,ocaml-ppx-here)
       ("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_sexp_value/")
    (synopsis "Simplify building s-expressions from ocaml values")
    (description "A ppx rewriter that simplifies building s-expressions from
ocaml values.")
    (license license:asl2.0)))

(define-public ocaml-ppx-pipebang
  (package
    (name "ocaml-ppx-pipebang")
    (version "113.33.03")
    (source (janestreet-origin "ppx_pipebang" version
              "1965c7hymp26ncmjs0pfxi2s5jlj60z2c9b194lgcwxqiav56pcw"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_pipebang/")
    (synopsis "Inline reverse application operators `|>` and `|!`")
    (description "A ppx rewriter that inlines reverse application operators
@code{|>} and @code{|!}.")
    (license license:asl2.0)))

(define-public ocaml-ppx-bin-prot
  (package
    (name "ocaml-ppx-bin-prot")
    (version "113.33.03")
    (source (janestreet-origin "ppx_bin_prot" version
              "173kjv36giik11zgfvsbzwfbpr66dm2pcha9vf990jgzh8hqz39h"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("bin-prot" ,ocaml-bin-prot)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-type-conv" ,ocaml-ppx-type-conv)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_bin_prot/")
    (synopsis "Generation of bin_prot readers and writers from types")
    (description "Generation of binary serialization and deserialization
functions from type definitions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-fail
  (package
    (name "ocaml-ppx-fail")
    (version "113.33.03")
    (source (janestreet-origin "ppx_fail" version
              "1dwgad0f05gqp5rnwf9dcasidpfi7q3mrpazsw3a2vijjblbhjgn"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-here" ,ocaml-ppx-here)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_fail/")
    (synopsis "Add location to calls to failwiths")
    (description "Syntax extension that makes [failwiths] always include a
position.")
    (license license:asl2.0)))

(define-public ocaml-ppx-custom-printf
  (package
    (name "ocaml-ppx-custom-printf")
    (version "113.33.03")
    (source (janestreet-origin "ppx_custom_printf" version
              "11jlx0n87g2j1vyyp343dibx7lvvwig5j5q0nq0b80kbsq0k6yr8"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)
       ("ppx-driver" ,ocaml-ppx-driver)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_custom_printf/")
    (synopsis "Printf-style format-strings for user-defined string conversion")
    (description "Extensions to printf-style format-strings for user-defined
string conversion.")
    (license license:asl2.0)))

(define-public ocaml-ppx-sexp-message
  (package
    (name "ocaml-ppx-sexp-message")
    (version "113.33.03")
    (source (janestreet-origin "ppx_sexp_message" version
              "084w1l3gnyw4ri9vbn7bv9b2xkw1520qczfxpxdarfivdrz8xr68"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-here" ,ocaml-ppx-here)
       ("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_sexp_message/")
    (synopsis "A ppx rewriter for easy construction of s-expressions")
    (description "Ppx_sexp_message aims to ease the creation of s-expressions
in OCaml.  This is mainly motivated by writing error and debugging messages,
where one needs to construct a s-expression based on various element of the
context such as function arguments.")
    (license license:asl2.0)))

(define-public ocaml-ppx-fields-conv
  (package
    (name "ocaml-ppx-fields-conv")
    (version "113.33.03")
    (source (janestreet-origin "ppx_fields_conv" version
              "1vzbdz27g5qhhfs7wx6rjf979q4xyssxqbmp6sc1sxknbghslbdv"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)
       ("ppx-core" ,ocaml-ppx-core)))
    (propagated-inputs
     `(("fieldslib" ,ocaml-fieldslib)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)
       ("ppx-type-conv" ,ocaml-ppx-type-conv)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_fields_conv/")
    (synopsis "Generation of accessor and iteration functions for ocaml records")
    (description "Ppx_fields_conv is a ppx rewriter that can be used to define
first class values representing record fields, and additional routines, to get
and set record fields, iterate and fold over all fields of a record and create
new record values.")
    (license license:asl2.0)))

(define-public ocaml-re
  (package
    (name "ocaml-re")
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/ocaml-re//archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s3rcr76cgm4p1xmaazc58arkg2lz3zfcp1icm00m6s5ccnkh67b"))))
    (build-system ocaml-build-system)
    (native-inputs `(("ounit" ,ocaml-ounit)))
    (home-page "https://github.com/ocaml/ocaml-re/")
    (synopsis "Regular expression library for OCaml")
    (description "Pure OCaml regular expressions with:
@enumerate
@item Perl-style regular expressions (module Re_perl)
@item Posix extended regular expressions (module Re_posix)
@item Emacs-style regular expressions (module Re_emacs)
@item Shell-style file globbing (module Re_glob)
@item Compatibility layer for OCaml's built-in Str module (module Re_str)
@end enumerate")
    (license license:expat)))

(define-public ocaml-ppx-expect
  (package
    (name "ocaml-ppx-expect")
    (version "113.33.03")
    (source (janestreet-origin "ppx_expect" version
              "03sbs4s5i8l9syr45v25f5hzy7msd2b47k2a9wsq9m43d4imgkrc"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("fieldslib" ,ocaml-fieldslib)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-assert" ,ocaml-ppx-assert)
       ("ppx-compare" ,ocaml-ppx-compare)
       ("ppx-core" ,ocaml-ppx-core)
       ("ppx-custom-printf" ,ocaml-ppx-custom-printf)
       ("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-fields-conv" ,ocaml-ppx-fields-conv)
       ("ppx-inline-test" ,ocaml-ppx-inline-test)
       ("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ppx-variants-conv" ,ocaml-ppx-variants-conv)
       ("re" ,ocaml-re)
       ("sexplib" ,ocaml-sexplib)
       ("variantslib" ,ocaml-variantslib)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_expect/")
    (synopsis "Cram like framework for OCaml")
    (description "Expect-test is a framework for writing tests in OCaml, similar
to Cram.  Expect-tests mimic the existing inline tests framework with the
let%expect_test construct.  The body of an expect-test can contain
output-generating code, interleaved with %expect extension expressions to denote
the expected output.")
    (license license:asl2.0)))

(define-public ocaml-ppx-jane
  (package
    (name "ocaml-ppx-jane")
    (version "113.33.03")
    (source (janestreet-origin "ppx_jane" version
              "0bjxkhmzgm6x9dcvjwybbccn34khbvyyjimcbaja30fp6qcqk5yl"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-assert" ,ocaml-ppx-assert)
       ("ppx-bench" ,ocaml-ppx-bench)
       ("ppx-bin-prot" ,ocaml-ppx-bin-prot)
       ("ppx-compare" ,ocaml-ppx-compare)
       ("ppx-custom-printf" ,ocaml-ppx-custom-printf)
       ("ppx-deriving" ,ocaml-ppx-deriving)
       ("ppx-enumerate" ,ocaml-ppx-enumerate)
       ("ppx-expect" ,ocaml-ppx-expect)
       ("ppx-fail" ,ocaml-ppx-fail)
       ("ppx-fields-conv" ,ocaml-ppx-fields-conv)
       ("ppx-here" ,ocaml-ppx-here)
       ("ppx-inline-test" ,ocaml-ppx-inline-test)
       ("ppx-let" ,ocaml-ppx-let)
       ("ppx-pipebang" ,ocaml-ppx-pipebang)
       ("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ppx-sexp-message" ,ocaml-ppx-sexp-message)
       ("ppx-sexp-value" ,ocaml-ppx-sexp-value)
       ("ppx-typerep-conv" ,ocaml-ppx-typerep-conv)
       ("ppx-variants-conv" ,ocaml-ppx-variants-conv)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_jane/")
    (synopsis "Standard Jane Street ppx rewriters")
    (description "Ppx_jane is a ppx_driver including all standard ppx rewriters.")
    (license license:asl2.0)))

(define-public ocaml-core-kernel
  (package
    (name "ocaml-core-kernel")
    (version "113.33.03")
    (source (janestreet-origin "core_kernel" version
               "0fl23jrwivixawhxinbwaw9cabqnzn7fini7dxpxjjvkxdc8ip5y"))
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("bin_prot" ,ocaml-bin-prot)
       ("ppx-assert" ,ocaml-ppx-assert)
       ("ppx-bench" ,ocaml-ppx-bench)
       ("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-expect" ,ocaml-ppx-expect)
       ("ppx-inline-test" ,ocaml-ppx-inline-test)
       ("typerep" ,ocaml-typerep)
       ("sexplib" ,ocaml-sexplib)
       ("variantslib" ,ocaml-variantslib)
       ("result" ,ocaml-result)
       ("fieldslib" ,ocaml-fieldslib)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/core_kernel/")
    (synopsis "Portable standard library for OCaml")
    (description "Core is an alternative to the OCaml standard library.

Core_kernel is the system-independent part of Core.  It is aimed for cases when
the full Core is not available, such as in Javascript.")
    (license license:asl2.0)))

(define-public ocaml-async-kernel
  (package
    (name "ocaml-async-kernel")
    (version "113.33.03")
    (source (janestreet-origin "async_kernel" version
              "04bjsaa23j831r09r38x6xx9nhryvp0z5ihickvhxqa4fb2snyvd"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("core-kernel" ,ocaml-core-kernel)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/async_kernel/")
    (synopsis "Monadic concurrency library")
    (description "Async-kernel is a library for concurrent programming in OCaml.")
    (license license:asl2.0)))

(define-public ocaml-async-rpc-kernel
  (package
    (name "ocaml-async-rpc-kernel")
    (version "113.33.03")
    (source (janestreet-origin "async_rpc_kernel" version
             "0y97h9pkb00v7jpf87m8cbb0ffkclj9g26ph6sq97q8dpisnkjwh"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-kernel" ,ocaml-async-kernel)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/async_rpc_kernel/")
    (synopsis "Platform-independent core of the Async RPC library")
    (description "Async_rpc_kernel is the platform-independent core of
the Async RPC library.")
    (license license:asl2.0)))

(define-public ocaml-core
  (package
    (name "ocaml-core")
    (version "113.33.03")
    (source (janestreet-origin "core" version
              "1znll157qg56g9d3247fjibv1hxv3r9wxgr4nhy19j2vzdh6a268"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("core-kernel" ,ocaml-core-kernel)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/core/")
    (synopsis "Alternative to OCaml's standard library")
    (description "The Core suite of libraries is an alternative to OCaml's
standard library that was developed by Jane Street.")
    (license license:asl2.0)))

(define-public ocaml-async-unix
  (package
    (name "ocaml-async-unix")
    (version "113.33.03")
    (source (janestreet-origin "async_unix" version
              "1fwl0lfrizllcfjk8hk8m7lsz9ha2jg6qgk4gssfyz377qvpcq4h"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-kernel" ,ocaml-async-kernel)
       ("core" ,ocaml-core)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/async_unix")
    (synopsis "Asynchronous execution library for Unix")
    (description "Async_unix is an asynchronous execution library for Unix.")
    (license license:asl2.0)))

(define-public ocaml-async-extra
  (package
    (name "ocaml-async-extra")
    (version "113.33.03")
    (source (janestreet-origin "async_extra" version
              "1si8jgiq5xh5sl9f2b7f9p17p7zx5h1pg557x2cxywi2x7pxqg4f"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-rpc-kernel" ,ocaml-async-rpc-kernel)
       ("async-unix" ,ocaml-async-unix)
       ("core" ,ocaml-core)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/async_extra")
    (synopsis "Extra functionnalities for the async library")
    (description "Async_extra provides additional functionnalities for the
async library.")
    (license license:asl2.0)))

(define-public ocaml-async
  (package
    (name "ocaml-async")
    (version "113.33.03")
    (source (janestreet-origin "async" version
              "0210fyhcs12kpmmd26015bgivkfd2wqkyn3c5wd7688d0f872y25"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-extra" ,ocaml-async-extra)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/async")
    (synopsis "Monadic concurrency library")
    (description "Async is a library for concurrent programming in OCaml.")
    (license license:asl2.0)))

(define-public ocaml-ocplib-endian
  (package
    (name "ocaml-ocplib-endian")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/OCamlPro/ocplib-endian/"
                                  "archive/" version ".tar.gz"))
              (sha256
               (base32
                "0hwj09rnzjs0m0kazz5h2mgs6p95j0zlga8cda5srnzqmzhniwkn"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (native-inputs `(("cppo" ,ocaml-cppo)))
    (home-page "https://github.com/OCamlPro/ocplib-endian")
    (synopsis "Optimised functions to read and write int16/32/64 from strings
and bigarrays")
    (description "Optimised functions to read and write int16/32/64 from strings
and bigarrays, based on new primitives added in version 4.01.  It works on
strings, bytes and bigstring (Bigarrys of chars), and provides submodules for
big- and little-endian, with their unsafe counter-parts.")
    (license license:lgpl2.1)))

(define-public ocaml-cstruct
  (package
    (name "ocaml-cstruct")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mirage/ocaml-cstruct/"
                                  "archive/v" version ".tar.gz"))
              (sha256
               (base32
                "15qpdc8421shq4pprdas9jznpva45229wkfqbwcxw9khaiiz7949"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-lwt" "--enable-async")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'link-stubs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
                    (lib (string-append out "/lib/ocaml/site-lib/cstruct")))
               (mkdir-p stubs)
               (symlink (string-append lib "/dllcstruct_stubs.so")
                        (string-append stubs "/dllcstruct_stubs.so"))))))))
    (native-inputs
     `(("ounit" ,ocaml-ounit)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("camlp4" ,camlp4)))
    (propagated-inputs
     `(("ocplib-endian" ,ocaml-ocplib-endian)
       ("lwt" ,ocaml-lwt)
       ("async" ,ocaml-async)
       ("sexplib" ,ocaml-sexplib)))
    (home-page "https://github.com/mirage/ocaml-cstruct")
    (synopsis "Access C structures via a camlp4 extension")
    (description "Cstruct is a library and syntax extension to make it easier
to access C-like structures directly from OCaml.  It supports both reading and
writing to these structures, and they are accessed via the Bigarray module.")
    (license license:isc)))

(define-public ocaml-hex
  (package
    (name "ocaml-hex")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mirage/ocaml-hex/"
                                  "archive/" version ".tar.gz"))
              (sha256
               (base32
                "0s63g0b8gfv2xm6fv6xg7bva8h76b5pcjb0zw3f8cygs0lq9072v"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (propagated-inputs `(("cstruct" ,ocaml-cstruct)))
    (home-page "https://github.com/mirage/ocaml-hex/")
    (synopsis "Minimal library providing hexadecimal converters")
    (description "Hex is a minimal library providing hexadecimal converters.")
    (license license:isc)))

(define-public ocaml-ezjsonm
  (package
    (name "ocaml-ezjsonm")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mirage/ezjsonm/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1kag0z2xlk4rw73a240dmkxh9rj6psxxcxkm7d7z0rrj6hzjajgq"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("alcotest" ,ocaml-alcotest)))
    (propagated-inputs
     `(("hex" ,ocaml-hex)
       ("jsonm" ,ocaml-jsonm)
       ("lwt" ,ocaml-lwt)
       ("sexplib" ,ocaml-sexplib)))
    (arguments
     `(#:configure-flags (list "--enable-lwt")))
    (home-page "https://github.com/mirage/ezjsonm/")
    (synopsis "Read and write JSON data")
    (description "Ezjsonm provides more convenient (but far less flexible) input
and output functions that go to and from [string] values than jsonm.  This avoids
the need to write signal code, which is useful for quick scripts that manipulate
JSON.")
    (license license:isc)))

(define-public ocaml-uri
  (package
    (name "ocaml-uri")
    (version "1.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mirage/ocaml-uri/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "02bzrag79prx261rxf9mlak749pwf4flpfl8p012x1xznv9m0clc"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ounit" ,ocaml-ounit)))
    (propagated-inputs
     `(("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("re" ,ocaml-re)
       ("ppx-deriving" ,ocaml-ppx-deriving)
       ("sexplib" ,ocaml-sexplib)
       ("stringext" ,ocaml-stringext)))
    (home-page "https://github.com/mirage/ocaml-uri")
    (synopsis "RFC3986 URI/URL parsing library")
    (description "OCaml-uri is a library for parsing URI/URL in the RFC3986 format.")
    (license license:isc)))

(define-public ocaml-easy-format
  (package
    (name "ocaml-easy-format")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mjambon/easy-format/"
                                  "archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1zcz682y9figa84k7lgdjcab5qbzk3yy14ygfqp2dhhrvjygm252"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/mjambon/easy-format")
    (synopsis "Interface to the Format module")
    (description "Easy-format is a high-level and functional interface to the
Format module of the OCaml standard library.")
    (license license:bsd-3)))

(define-public optcomp
  (package
    (name "optcomp")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/diml/optcomp/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hhhb2gisah1h22zlg5iszbgqxdd7x85cwd57bd4mfkx9l7dh8jh"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:use-make? #t
       #:make-flags
       (list (string-append "BUILDFLAGS=\"-cflags -I,"
                            (assoc-ref %build-inputs "camlp4")
                            "/lib/ocaml/site-lib/camlp4/Camlp4Parsers\""))))
    (native-inputs `(("camlp4" ,camlp4)))
    (propagated-inputs `(("camlp4" ,camlp4)))
    (home-page "https://github.com/diml/optcomp")
    (synopsis "Optional compilation for OCaml")
    (description "Optcomp provides an optional compilation facility with
cpp-like directives.")
    (license license:bsd-3)))

(define-public ocaml-piqilib
  (package
    (name "ocaml-piqilib")
    (version "0.6.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alavrik/piqi/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1whqr2bb3gds2zmrzqnv8vqka9928w4lx6mi6g244kmbwb2h8d8l"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-ocamlpath
           (lambda _
             (substitute* '("Makefile" "make/Makefile.ocaml")
               (("OCAMLPATH := ") "OCAMLPATH := $(OCAMLPATH):"))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "make/OCamlMakefile"
                 (("/bin/sh") (which "bash")))
               (zero? (system* "./configure" "--prefix" out "--ocaml-libdir"
                               (string-append out "/lib/ocaml/site-lib"))))))
       (add-after 'build 'build-ocaml
         (lambda* (#:key outputs #:allow-other-keys)
           (zero? (system* "make" "ocaml")))) 
       (add-after 'install 'install-ocaml
         (lambda* (#:key outputs #:allow-other-keys)
           (zero? (system* "make" "ocaml-install"))))
       (add-after 'install-ocaml 'link-stubs
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
                  (lib (string-append out "/lib/ocaml/site-lib/piqilib")))
             (mkdir-p stubs)
             (symlink (string-append lib "/dllpiqilib_stubs.so")
                      (string-append stubs "/dllpiqilib_stubs.so"))
             #t))))))
    (native-inputs
     `(("which" ,which)
       ("camlp4" ,camlp4)))
    (propagated-inputs
     `(("xmlm" ,ocaml-xmlm)
       ("ulex" ,ocaml-ulex)
       ("optcomp" ,optcomp)
       ("easy-format" ,ocaml-easy-format)
       ("base64" ,ocaml-base64)))
    (home-page "http://piqi.org")
    (synopsis "Data serialization and conversion library")
    (description "Piqilib is the common library used by the piqi command-line
tool and piqi-ocaml.")
    (license license:asl2.0)))

(define-public ocaml-uuidm
  (package
    (name "ocaml-uuidm")
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/uuidm/"
                                  "releases/uuidm-" version ".tbz"))
              (sha256
               (base32
                "0hz4fdx0x16k0pw9995vkz5d1hmzz6b16wck9li399rcbfnv5jlc"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags
       (list "build" "--tests" "true" "--with-cmdliner" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("opam" ,opam)))
    (propagated-inputs
     `(("cmdliner" ,ocaml-cmdliner)
       ("topkg" ,ocaml-topkg)))
    (home-page "http://erratique.ch/software/uuidm")
    (synopsis "Universally unique identifiers for OCaml")
    (description "Uuidm is an OCaml module implementing 128 bits universally
unique identifiers (UUIDs) version 3, 5 (named based with MD5, SHA-1 hashing)
and 4 (random based) according to RFC 4122.")
    (license license:isc)))

(define-public ocaml-graph
  (package
    (name "ocaml-graph")
    (version "1.8.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ocamlgraph.lri.fr/download/"
                                  "ocamlgraph-" version ".tar.gz"))
              (sha256
               (base32
                "1845r537swjil2fcj7lgbibc2zybfwqqasrd2s7bncajs83cl1nz"))
              (patches (search-patches "ocaml-graph-honor-source-date-epoch.patch"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:install-target "install-findlib"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-shell
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CONFIG_SHELL" (string-append (assoc-ref inputs "bash")
                                                   "/bin/sh")))))))
    (inputs `(("lablgtk" ,lablgtk)))
    (home-page "http://ocamlgraph.lri.fr/")
    (synopsis "Graph library for OCaml")
    (description "OCamlgraph is a generic graph library for OCaml.")
    (license license:lgpl2.1)))

(define-public ocaml-piqi
  (package
    (name "ocaml-piqi")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alavrik/piqi-ocaml/"
                                  "archive/v" version ".tar.gz"))
              (sha256
               (base32
                "0ngz6y8i98i5v2ma8nk6mc83pdsmf2z0ks7m3xi6clfg3zqbddrv"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "DESTDIR=" (assoc-ref %outputs "out"))
             (string-append "SHELL=" (assoc-ref %build-inputs "bash")
                            "/bin/sh"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("which" ,which)
       ("protobuf" ,protobuf))) ; for tests
    (propagated-inputs
     `(("piqilib" ,ocaml-piqilib)))
    (home-page "https://github.com/alavrik/piqi-ocaml")
    (synopsis "Protocol serialization system for OCaml")
    (description "Piqi is a multi-format data serialization system for OCaml.
It provides a uniform interface for serializing OCaml data structures to JSON,
XML and Protocol Buffers formats.")
    (license license:asl2.0)))

(define-public bap
  (package
    (name "bap")
    (version "1.1.0")
    (home-page "https://github.com/BinaryAnalysisPlatform/bap")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1ms95m4j1qrmy7zqmsn2izh7gq68lnmssl7chyhk977kd3sxj66m"))
              (file-name (string-append name "-" version ".tar.gz"))))
   (build-system ocaml-build-system)
   (native-inputs
    `(("oasis" ,ocaml-oasis)
      ("clang" ,clang)
      ("ounit" ,ocaml-ounit)))
   (propagated-inputs
    `(("core-kernel" ,ocaml-core-kernel)
      ("ppx-driver" ,ocaml-ppx-driver)
      ("uri" ,ocaml-uri)
      ("llvm" ,llvm)
      ("gmp" ,gmp)
      ("clang-runtime" ,clang-runtime)
      ("fileutils" ,ocaml-fileutils)
      ("cmdliner" ,ocaml-cmdliner)
      ("zarith" ,ocaml-zarith)
      ("uuidm" ,ocaml-uuidm)
      ("camlzip" ,camlzip)
      ("frontc" ,ocaml-frontc)
      ("ezjsonm" ,ocaml-ezjsonm)
      ("ocurl" ,ocaml-ocurl)
      ("piqi" ,ocaml-piqi)
      ("ocamlgraph" ,ocaml-graph)
      ("bitstring" ,ocaml-bitstring)
      ("ppx-jane" ,ocaml-ppx-jane)
      ("re" ,ocaml-re)))
   (inputs `(("llvm" ,llvm)))
   (arguments
    `(#:use-make? #t
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (zero? (system* "./configure" "--prefix"
                            (assoc-ref outputs "out")
                            "--libdir"
                            (string-append
                              (assoc-ref outputs "out")
                              "/lib/ocaml/site-lib")
                            "--with-llvm-version=3.8"
                            "--with-llvm-config=llvm-config"
                            "--enable-everything"))))
        (add-after 'install 'link-stubs
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
                   (lib (string-append out "/lib/ocaml/site-lib/bap-plugin-llvm")))
              (mkdir-p stubs)
              (symlink (string-append lib "/dllllvm_plugin_stubs.so")
                       (string-append stubs "/dllllvm_plugin_stubs.so"))))))))
   (synopsis "Binary Analysis Platform")
   (description "Binary Analysis Platform is a framework for writing program
analysis tools, that target binary files.  The framework consists of a plethora
of libraries, plugins, and frontends.  The libraries provide code reusability,
the plugins facilitate extensibility, and the frontends serve as entry points.")
   (license license:expat)))

(define-public ocaml-camomile
  (package
    (name "ocaml-camomile")
    (version "0.8.5")
    (home-page "https://github.com/yoriyuki/Camomile")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/releases/download/rel-" version
                                  "/camomile-" version ".tar.bz2"))
              (sha256
               (base32
                "003ikpvpaliy5hblhckfmln34zqz0mk3y2m1fqvbjngh3h2np045"))))
    (build-system ocaml-build-system)
    (native-inputs `(("camlp4" ,camlp4)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-bin/sh
           (lambda _
             (setenv "CONFIG_SHELL" (which "bash")))))))
    (synopsis "Comprehensive Unicode library")
    (description "Camomile is a Unicode library for OCaml.  Camomile provides
Unicode character type, UTF-8, UTF-16, UTF-32 strings, conversion to/from about
200 encodings, collation and locale-sensitive case mappings, and more.  The
library is currently designed for Unicode Standard 3.2.")
    ;; with an exception for linked libraries to use a different license
    (license license:lgpl2.0+)))

(define-public ocaml-jbuilder
  (package
    (name "ocaml-jbuilder")
    (version "1.0+beta16")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/janestreet/jbuilder/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gcy52y0mkg450yxwszp3lww303a1154566r8jb4hh5l61dh4dwj"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "PREFIX" out))
             #t)))))
    (native-inputs
     `(("menhir" ,ocaml-menhir)))
    (propagated-inputs
     `(("opam" ,opam)))
    (home-page "https://github.com/janestreet/jbuilder")
    (synopsis "Composable build system for OCaml")
    (description "Jbuilder is a build system designed for OCaml/Reason projects
only.  It focuses on providing the user with a consistent experience and takes
care of most of the low-level details of OCaml compilation.  All you have to do
is provide a description of your project and Jbuilder will do the rest.")
    (license license:asl2.0)))

(define-public ocaml-zed
  (package
    (name "ocaml-zed")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/diml/zed/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q281slzwgdrrxalayll75bxgghadswlh2zcvzy08nrywqnlq5y8"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key #:allow-other-keys)
             (zero? (system* "jbuilder" "build"))))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "jbuilder" "install" "--prefix" out))))))))
    (native-inputs
     `(("jbuilder" ,ocaml-jbuilder)))
    (propagated-inputs
     `(("camomile" ,ocaml-camomile)
       ("react" ,ocaml-react)))
    (home-page "https://github.com/diml/zed")
    (synopsis "Abstract engine for text editing in OCaml")
    (description "Zed is an abstract engine for text edition.  It can be used
to write text editors, edition widgets, readlines, etc.  You just have to
connect an engine to your inputs and rendering functions to get an editor.")
    (license license:bsd-3)))

(define-public ocaml-lambda-term
  (package
    (name "ocaml-lambda-term")
    (version "1.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/diml/lambda-term/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10lx1jqgmmfwwlv64di4a8nia9l53v7179z70n9fx6aq5l7r8nba"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; currently, ocaml-lwt is an old version of lwt from before lwt.react
         ;; was split into a separate module called lwt_react
         (add-before 'build 'use-old-lwt-react-name
           (lambda _
             (substitute* "src/jbuild" (("lwt_react") "lwt.react"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "jbuilder" "install" "--prefix" out))))))))
    (native-inputs
     `(("jbuilder" ,ocaml-jbuilder)))
    (propagated-inputs
     `(("lwt" ,ocaml-lwt)
       ("zed" ,ocaml-zed)))
    (home-page "https://github.com/diml/lambda-term")
    (synopsis "Terminal manipulation library for OCaml")
    (description "Lambda-Term is a cross-platform library for manipulating the
terminal.  It provides an abstraction for keys, mouse events, colors, as well as
a set of widgets to write curses-like applications.  The main objective of
Lambda-Term is to provide a higher level functional interface to terminal
manipulation than, for example, ncurses, by providing a native OCaml interface
instead of bindings to a C library.")
    (license license:bsd-3)))

(define-public ocaml-utop
  (package
    (name "ocaml-utop")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/diml/utop/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rglznh4prcix8spi3f060jz2gngk7x8vkd291fxs10b88aqcpxf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libdir (string-append out "/lib/ocaml/site-lib")))
               (mkdir-p libdir)
               (zero? (system* "jbuilder" "install"
                               "--prefix" out
                               "--libdir" libdir))))))))
    (native-inputs
     `(("ocaml" ,ocaml)
       ("cppo" ,ocaml-cppo)
       ("jbuilder" ,ocaml-jbuilder)))
    (propagated-inputs
     `(("findlib" ,ocaml-findlib-1.7.3)
       ("lambda-term" ,ocaml-lambda-term)
       ("lwt" ,ocaml-lwt)
       ("react" ,ocaml-react)
       ("camomile" ,ocaml-camomile)
       ("zed" ,ocaml-zed)))
    (home-page "https://github.com/diml/utop")
    (synopsis "Improved interface to the OCaml toplevel")
    (description "UTop is an improved toplevel for OCaml.  It can run in a
terminal or in Emacs.  It supports line editing, history, real-time and context
sensitive completion, colors, and more.")
    (license license:bsd-3)))

(define-public coq-flocq
  (package
    (name "coq-flocq")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/file"
                                  "/37054/flocq-" version ".tar.gz"))
              (sha256
               (base32
                "13fv150dcwnjrk00d7zj2c5x9jwmxgrq0ay440gkr730l8mvk3l3"))))
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
               (("/bin/sh") (which "sh")))))
         (replace 'build
           (lambda _
             (zero? (system* "./remake"))))
         (replace 'check
           (lambda _
             (zero? (system* "./remake" "check"))))
             ;; TODO: requires coq-gappa and coq-interval.
             ;(zero? (system* "./remake" "check-more"))))
         (replace 'install
           (lambda _
             (zero? (system* "./remake" "install")))))))
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
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/file/36351/gappa-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0924jr6f15fx22qfsvim5vc0qxqg30ivg9zxj34lf6slbgdl3j39"))))
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
                            "/lib/coq/user-contrib/Gappa"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-remake
           (lambda _
             (substitute* "remake.cpp"
               (("/bin/sh") (which "sh")))))
         (replace 'build
           (lambda _
             (zero? (system* "./remake"))))
         (replace 'check
           (lambda _
             (zero? (system* "./remake" "check"))))
         (replace 'install
           (lambda _
             (zero? (system* "./remake" "install")))))))
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
    (version "1.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/math-comp/math-comp/archive/mathcomp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0lg5ncr7p4y8qqq6pfw6brqc6a9xzlfa0drprwfdn0rnyaq5nca6"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq)))
    (arguments
     `(#:tests? #f; No need to test formally-verified programs :)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'chdir
           (lambda _
             (chdir "mathcomp")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "COQLIB" (string-append (assoc-ref outputs "out") "/lib/coq/"))
             (zero? (system* "make" "-f" "Makefile.coq"
                             (string-append "COQLIB=" (assoc-ref outputs "out")
                                            "/lib/coq/")
                             "install")))))))
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
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/"
                                  "file/37045/coquelicot-" version ".tar.gz"))
              (sha256
               (base32
                "0hsyhsy2lwqxxx2r8xgi5csmirss42lp9bkb9yy35mnya0w78c8r"))))
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
               (("/bin/sh") (which "sh")))))
         (replace 'build
           (lambda _
             (zero? (system* "./remake"))))
         (replace 'check
           (lambda _
             (zero? (system* "./remake" "check"))))
         (replace 'install
           (lambda _
             (zero? (system* "./remake" "install")))))))
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
    (version "8.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/coq/bignums/archive/V"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03iw9jiwq9jx45gsvp315y3lxr8m9ksppmcjvxs5c23qnky6zqjx"))))
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
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/"
                                  "file/37077/interval-" version ".tar.gz"))
              (sha256
               (base32
                "08fdcf3hbwqphglvwprvqzgkg0qbimpyhnqsgv3gac4y1ap0f903"))))
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
               (("/bin/sh") (which "sh")))))
         (replace 'build
           (lambda _
             (zero? (system* "./remake"))))
         (replace 'check
           (lambda _
             (zero? (system* "./remake" "check"))))
         (replace 'install
           (lambda _
             (zero? (system* "./remake" "install")))))))
    (home-page "http://coq-interval.gforge.inria.fr/")
    (synopsis "Coq tactics to simplify inequality proofs")
    (description "Interval provides vernacular files containing tactics for
simplifying the proofs of inequalities on expressions of real numbers for the
Coq proof assistant.")
    (license license:cecill-c)))
