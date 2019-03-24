;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Peter Kreye <kreyepr@gmail.com>
;;; Copyright © 2018, 2019 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system dune)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module (guix download)
  #:use-module (guix git-download)
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
                   "--libdir $(LIBDIR) --prefix"))
                #t)))))

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

(define-public ocaml-4.02
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
                "ocaml-Add-a-.file-directive.patch"
                "ocaml-enable-ocamldoc-reproducibility.patch"))))
    (build-system gnu-build-system)
    (native-search-paths
     (list (search-path-specification
            (variable "OCAMLPATH")
            (files (list "lib/ocaml" "lib/ocaml/site-lib")))
           (search-path-specification
            (variable "CAML_LD_LIBRARY_PATH")
            (files (list "lib/ocaml/site-lib/stubslibs"
                         "lib/ocaml/site-lib/stublibs")))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libx11" ,libx11)
       ;; For libiberty, needed for objdump support.
       ("gcc:lib" ,(canonical-package gcc) "lib")
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
               (invoke "./configure"
                       "--prefix" out
                       "--mandir" mandir))))
         (replace 'build
           (lambda _
             (invoke "make" "-j" (number->string (parallel-job-count))
                     "world.opt")))
         (delete 'check)
         (add-after 'install 'check
           (lambda _
             (with-directory-excursion "testsuite"
               (invoke "make" "all"))))
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

(define-public ocaml-4.07
  (package
    (inherit ocaml-4.02)
    (version "4.07.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://caml.inria.fr/pub/distrib/ocaml-"
                    (version-major+minor version)
                    "/ocaml-" version ".tar.xz"))
              (sha256
               (base32
                "1f07hgj5k45cylj1q3k5mk8yi02cwzx849b1fwnwia8xlcfqpr6z"))))
    (arguments
     (substitute-keyword-arguments (package-arguments ocaml-4.02)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'prepare-socket-test)
           (replace 'check
             (lambda _
               (with-directory-excursion "testsuite"
                 (invoke "make" "all"))
               #t))))))))

(define-public ocaml ocaml-4.07)

(define-public ocamlbuild
  (package
    (name "ocamlbuild")
    (version "0.13.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/ocamlbuild/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1320cfkixs1xlng5av04pa5qjb3ynvi2kl3k1ngqzg5fpi29b0vr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f; tests require findlib
       #:make-flags
       (list (string-append "OCAMLBUILD_PREFIX=" (assoc-ref %outputs "out"))
             (string-append "OCAMLBUILD_BINDIR=" (assoc-ref %outputs "out")
                        "/bin")
             (string-append "OCAMLBUILD_LIBDIR=" (assoc-ref %outputs "out")
                        "/lib/ocaml/site-lib")
             (string-append "OCAMLBUILD_MANDIR=" (assoc-ref %outputs "out")
                        "/share/man"))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (add-before 'build 'findlib-environment
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (setenv "OCAMLFIND_DESTDIR" (string-append out "/lib/ocaml/site-lib"))
               (setenv "OCAMLFIND_LDCONF" "ignore")
               #t))))))
    (native-inputs
     `(("ocaml" ,ocaml)))
    (home-page "https://github.com/ocaml/ocamlbuild")
    (synopsis "OCaml build tool")
    (description "OCamlbuild is a generic build tool, that has built-in rules
for building OCaml library and programs.")
    (license license:lgpl2.1+)))

(define-public opam
  (package
    (name "opam")
    (version "2.0.3")
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
                "1qphm1grxx5j8li7f9qfpih4ylrnjl08b4ym8ma4ln44l56xm285"))))
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
                     (let ((bash (assoc-ref inputs "bash"))
                           (bwrap (string-append (assoc-ref inputs "bubblewrap")
                                                 "/bin/bwrap")))
                       (substitute* "src/core/opamSystem.ml"
                         (("\"/bin/sh\"")
                          (string-append "\"" bash "/bin/sh\""))
                         (("getconf")
                          (which "getconf")))
                       ;; Use bwrap from the store directly.
                       (substitute* "src/state/shellscripts/bwrap.sh"
                         (("-v bwrap") (string-append "-v " bwrap))
                         (("exec bwrap") (string-append "exec " bwrap))
                         ;; Mount /gnu and /run/current-system in the
                         ;; isolated environment when building with opam.
                         ;; This is necessary for packages to find external
                         ;; dependencies, such as a C compiler, make, etc...
                         (("^add_mounts ro /usr")
                          "add_mounts ro /gnu /run/current-system /usr"))
                       (substitute* "src/client/opamInitDefaults.ml"
                         (("\"bwrap\"") (string-append "\"" bwrap "\"")))
                       ;; Build dependencies
                       (apply invoke "make" "lib-ext" make-flags)
                       #t)))
                 (add-before 'check 'pre-check
                   (lambda _
                     (setenv "HOME" (getcwd))
                     (invoke "git" "config" "--global" "user.email" "guix@gnu.org")
                     (invoke "git" "config" "--global" "user.name" "Guix")
                     #t)))))
    (native-inputs
     `(("git" ,git)                               ;for the tests
       ("python" ,python)                         ;for the tests
       ("camlp4" ,camlp4)))
    (inputs
     `(("ocaml" ,ocaml)
       ("ncurses" ,ncurses)
       ("curl" ,curl)
       ("bubblewrap" ,bubblewrap)))
    (home-page "http://opam.ocamlpro.com/")
    (synopsis "Package manager for OCaml")
    (description
     "OPAM is a tool to manage OCaml packages.  It supports multiple
simultaneous compiler installations, flexible package constraints, and a
Git-friendly development workflow.")

    ;; The 'LICENSE' file waives some requirements compared to LGPLv3.
    (license license:lgpl3)))

(define-public camlp4-4.02
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
    (native-inputs `(("ocaml" ,ocaml-4.02)
                     ("which" ,which)))
    (inputs `(("ocaml" ,ocaml-4.02)))
    (arguments
     '(#:tests? #f                                ;no documented test target
       ;; a race-condition will lead byte and native targets to  mkdir _build
       ;; which  fails on the second attempt.
       #:parallel-build? #f
       #:make-flags '("all")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; This is a home-made 'configure' script.
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure"
                       (string-append "--libdir=" out
                                      "/lib/ocaml/site-lib")
                       (string-append "--bindir=" out "/bin")
                       (string-append "--pkgdir=" out
                                      "/lib/ocaml/site-lib")))))
         (add-after 'install 'install-meta
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "camlp4/META.in"
                 (("directory = .*")
                  (string-append "directory = \"" out
                                 "/lib/ocaml/site-lib/camlp4\"\n")))
               (invoke "make" "install-META")))))))
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

(define-public camlp4-4.07
  (package
    (inherit camlp4-4.02)
    (name "camlp4")
    (version "4.07+1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/camlp4/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "143hhxv1i6aq413z0i1pynrjcfl2g5gnh5r3863v6h9z0riqknzc"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (properties
     `((ocaml4.02-variant . ,(delay camlp4-4.02))))
    (native-inputs
     `(("ocaml" ,ocaml-4.07)
       ("ocamlbuild" ,ocamlbuild)
       ("which" ,which)))
    (inputs
     `(("ocaml" ,ocaml-4.07)))))

(define-public camlp4 camlp4-4.07)

(define-public camlp5
  (package
    (name "camlp5")
    (version "7.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/camlp5/camlp5/archive/rel"
                                  (string-delete #\. version) ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "148r6p93xlxi6v7kbsqv8i70r6av04cyn0109pwss5xj6fw97i52"))))
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
                      (invoke "./configure"
                              "--prefix" out
                              "--mandir" mandir))))
         (add-before 'build 'fix-/bin-references
           (lambda _
             (substitute* "config/Makefile"
               (("/bin/rm") "rm"))
             #t))
         (replace 'build
                  (lambda _
                    (invoke "make" "-j" (number->string
                                         (parallel-job-count))
                            "world.opt")))
         ;; Required for findlib to find camlp5's libraries
         (add-after 'install 'install-meta
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "etc/META" (string-append (assoc-ref outputs "out")
                                                     "/lib/ocaml/camlp5/"))
             #t)))))
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
    (version "2.32")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://hevea.inria.fr/old/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s4yqphfcr1pf5mcj5c84mvmd107k525iiym5jdwsxz0ka0ccmfy"))))
    (build-system gnu-build-system)
    (inputs
     `(("ocaml" ,ocaml)))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)))
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

(define-public ocaml-num
  (package
    (name "ocaml-num")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/num/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xlkd0svc0mgq5s7nrm2rjrsvg15i9wxqkc1kvwjp6sv8vv8bb04"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-makefile
           (lambda* (#:key outputs #:allow-other-keys)
             ;; This package supposes we install to the same directory as
             ;; the ocaml package.
             (substitute* "src/META"
               (("\"\\^\"") (string-append "\"" (assoc-ref outputs "out")
                                           "/lib/ocaml/site-lib\"")))
             (substitute* "src/Makefile"
               (("\\) \\$\\(STDLIBDIR\\)")
                (string-append ") " (assoc-ref outputs "out")
                               "/lib/ocaml/site-lib")))
             #t))
         (add-after 'install 'fix-stubslib
           (lambda* (#:key outputs #:allow-other-keys)
             (format #t "~a~%" (find-files "." ".*.so"))
             (let ((stubdir (string-append (assoc-ref outputs "out")
                                           "/lib/ocaml/site-lib")))
               (mkdir-p stubdir)
               (install-file "src/dllnums.so" stubdir))
             #t)))))
    (home-page "https://github.com/ocaml/num")
    (synopsis "Arbitrary-precision integer and rational arithmetic")
    (description "OCaml-Num contains the legacy Num library for
arbitrary-precision integer and rational arithmetic that used to be part of
the OCaml core distribution.")
    (license license:lgpl2.1+))); with linking exception

(define-public emacs-tuareg
  (package
    (name "emacs-tuareg")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/tuareg/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ynpfc170f9jqx49biji9npfkvfpflbm29xf24wc7fnxxayr49ig"))))
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
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)))
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
    (properties
     `((ocaml4.02-variant . ,(delay ocaml4.02-menhir))))
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

(define-public ocaml4.02-menhir
  (package
    (inherit ocaml-menhir)
    (name "ocaml4.02-menhir")
    (inputs `(("ocaml" ,ocaml-4.02)))
    (native-inputs '())))

(define-public lablgtk
  (package
    (name "lablgtk")
    (version "2.18.6")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1726))
              (sha256
               (base32
                "1y38fdvswy6hmppm65qvgdk4pb3ghhnvz7n4ialf46340r1s5p2d"))))
    (build-system gnu-build-system)
    (properties
     `((ocaml4.02-variant . ,(delay ocaml4.02-lablgtk))))
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

(define-public ocaml4.02-lablgtk
  (package
    (inherit lablgtk)
    (name "ocaml4.02-lablgtk")
    (version "2.18.5")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1627))
              (sha256
               (base32
                "0cyj6sfdvzx8hw7553lhgwc0krlgvlza0ph3dk9gsxy047dm3wib"))))
    (native-inputs
     `(("camlp4" ,camlp4-4.02)
       ("ocaml" ,ocaml-4.02)
       ("findlib" ,ocaml4.02-findlib)
       ("pkg-config" ,pkg-config)))))

(define-public unison
  (package
    (name "unison")
    (version "2.51.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bcpierce00/unison.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bykiyc0dc5pkw8x370qkg2kygq9pq7yqzsgczd3y13b6ivm4sdq"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                  ; 1.9 MiB of documentation
    (native-inputs
     `(("ocaml" ,ocaml)
       ;; For documentation
       ("ghostscript" ,ghostscript)
       ("texlive" ,texlive-tiny)
       ("hevea" ,hevea)
       ("lynx" ,lynx)
       ("which" ,which)))
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
                 (install-file "src/unison-fsmonitor" bin)
                 #t)))
           (add-after 'install 'install-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((doc (string-append (assoc-ref outputs "doc")
                                         "/share/doc/unison")))
                 (mkdir-p doc)
                 ;; Remove an '\n' that prevents the doc to be generated
                 ;; correctly with newer hevea.
                 (substitute* "doc/local.tex"
                   (("----SNIP----.*") "----SNIP----"))
                 ;; This file needs write-permissions, because it's
                 ;; overwritten by 'docs' during documentation generation.
                 (chmod "src/strings.ml" #o600)
                 (invoke "make" "docs"
                         "TEXDIRECTIVES=\\\\draftfalse")
                 (for-each (lambda (f)
                             (install-file f doc))
                           (map (lambda (ext)
                                  (string-append "doc/unison-manual." ext))
                                ;; Install only html documentation,
                                ;; since the build is currently
                                ;; non-reproducible with the ps, pdf,
                                ;; and dvi docs.
                                '(;; "ps" "pdf" "dvi"
                                  "html")))
                 #t))))))
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
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.camlcity.org/download/"
                                  "findlib" "-" version ".tar.gz"))
              (sha256
               (base32
                "1b97zqjdriqd2ikgh4rmqajgxwdwn013riji5j53y3xvcmnpsyrb"))))
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
                       (invoke
                        "./configure"
                        "-bindir" (string-append out "/bin")
                        "-config" (string-append out "/etc/ocamfind.conf")
                        "-mandir" (string-append out "/share/man")
                        "-sitelib" (string-append out "/lib/ocaml/site-lib")
                        "-with-toolbox"))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (invoke "make" "install"
                                (string-append "OCAML_CORE_STDLIB="
                                               out "/lib/ocaml/site-lib")))))
                  (add-after 'install 'remove-camlp4
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (delete-file-recursively
                         (string-append out "/lib/ocaml/site-lib/camlp4"))
                        #t))))))
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

(define-public ocaml4.02-findlib
  (package
    (inherit ocaml-findlib)
    (name "ocaml4.02-findlib")
    (native-inputs
     `(("camlp4" ,camlp4-4.02)
       ("m4" ,m4)
       ("ocaml" ,ocaml-4.02)))))

;; note that some tests may hang for no obvious reason.
(define-public ocaml-ounit
  (package
    (name "ocaml-ounit")
    (version "2.0.8")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri "ounit" version 1749))
              (sha256
               (base32
                "03ifp9hjcxg4m5j190iy373jcn4039d3vy10kmd8p4lfciwzwc1f"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("libxml2" ,libxml2)           ; for xmllint
       ("ocamlbuild" ,ocamlbuild)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))             ; tests are run during build
    (home-page "http://ounit.forge.ocamlcore.org")
    (synopsis "Unit testing framework for OCaml")
    (description "Unit testing framework for OCaml.  It is similar to JUnit and
other XUnit testing frameworks.")
    (license license:expat)))

(define-public ocaml4.02-ounit
  (package-with-ocaml4.02 ocaml-ounit))

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

(define-public ocaml4.02-camlzip
  (package-with-ocaml4.02 camlzip))

(define-public ocamlmod
  (package
    (name "ocamlmod")
    (version "0.0.9")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1702))
              (sha256
               (base32
                "0cgp9qqrq7ayyhddrmqmq1affvfqcn722qiakjq4dkywvp67h4aa"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ounit" ,ocaml-ounit)
       ("ocamlbuild" ,ocamlbuild)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Tests are done during build.
         (delete 'check))))
    (home-page "https://forge.ocamlcore.org/projects/ocamlmod")
    (synopsis "Generate modules from OCaml source files")
    (description "Generate modules from OCaml source files.")
    (license license:lgpl2.1+))) ; with an exception

(define-public ocaml4.02-ocamlmod
  (package-with-ocaml4.02 ocamlmod))

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
           (lambda _ (invoke "./configure"))))))
    (home-page "https://forge.ocamlcore.org/projects/zarith/")
    (synopsis "Implements arbitrary-precision integers")
    (description "Implements arithmetic and logical operations over
arbitrary-precision integers.  It uses GMP to efficiently implement arithmetic
over big integers. Small integers are represented as Caml unboxed integers,
for speed and space economy.")
    (license license:lgpl2.1+))) ; with an exception

(define-public ocaml4.02-zarith
  (package-with-ocaml4.02 ocaml-zarith))

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
     `(#:ocaml ,ocaml-4.02
       #:findlib ,ocaml4.02-findlib
       #:phases
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
     `(("ounit" ,ocaml-ounit)
       ("ocamlbuild" ,ocamlbuild)))
    (home-page "https://github.com/c-cube/qcheck")
    (synopsis "QuickCheck inspired property-based testing for OCaml")
    (description "QuickCheck inspired property-based testing for OCaml. This
module allows to check invariants (properties of some types) over randomly
generated instances of the type. It provides combinators for generating
instances and printing them.")
    (license license:lgpl3+)))

(define-public ocaml4.02-qcheck
  (package-with-ocaml4.02 ocaml-qcheck))

(define-public ocaml-qtest
  (package
    (name "ocaml-qtest")
    (version "2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/vincent-hugot/qtest/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ff4if64mc9c7wmhjdgnlnh6k6a713piqzr4043zzj4s5pw7smxk"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)))
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
    (home-page "https://github.com/vincent-hugot/qtest")
    (synopsis "Inline (Unit) Tests for OCaml")
    (description "Qtest extracts inline unit tests written using a special
syntax in comments.  Those tests are then run using the oUnit framework and the
qcheck library.  The possibilities range from trivial tests -- extremely simple
to use -- to sophisticated random generation of test cases.")
    (license license:lgpl3+)))

(define-public ocaml4.02-qtest
  (package-with-ocaml4.02 ocaml-qtest))

(define-public ocaml-stringext
  (package
    (name "ocaml-stringext")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rgrinberg/stringext"
                                  "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19g6lfn03iki9f8h91hi0yiqn0b3wkxyq08b3y23wgv6jw6mssfh"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("qtest" ,ocaml-qtest)))
    (home-page "https://github.com/rgrinberg/stringext")
    (synopsis "Extra string functions for OCaml")
    (description "Provides a single module named Stringext that provides a grab
bag of often used but missing string functions from the stdlib.  E.g, split,
full_split, cut, rcut, etc..")
    ;; the only mention of a license in this project is in its `opam' file
    ;; where it says `mit'.
    (license license:expat)))

(define-public ocaml4.02-stringext
  (package-with-ocaml4.02 ocaml-stringext))

(define-public ocaml-bisect
  (package
    (name "ocaml-bisect")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gasche/bisect/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0p67fppk5ifb63b00kxwrb1xg75hrqhknng3bsdyw3gxxqyjlpmx"))
              (patches
               (search-patches
                "ocaml-bisect-fix-camlp4-in-another-directory.patch"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("camlp4" ,camlp4)
       ("libxml2" ,libxml2)
       ("ocamlbuild" ,ocamlbuild)
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
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "./configure" "-prefix" (assoc-ref outputs "out")
                     "-ocaml-prefix" (assoc-ref inputs "ocaml")
                     "-camlp4-prefix" (assoc-ref inputs "camlp4"))
             #t)))))
    (home-page "http://bisect.x9c.fr")
    (synopsis "Code coverage tool for the OCaml language")
    (description "Bisect is a code coverage tool for the OCaml language.  It is
a camlp4-based tool that allows to instrument your application before running
tests.  After application execution, it is possible to generate a report in HTML
format that is the replica of the application source code annotated with code
coverage information.")
    (properties
     `((ocaml4.02-variant . ,(delay ocaml4.02-bisect))))
    (license license:gpl3+)))

(define-public ocaml4.02-bisect
  (let ((base (package-with-ocaml4.02 (strip-ocaml4.02-variant ocaml-bisect))))
    (package
      (inherit base)
      (native-inputs
       `(("camlp4" ,camlp4-4.02)
         ("libxml2" ,libxml2)
         ("which" ,which)))
      (propagated-inputs
       `(("camlp4" ,camlp4-4.02))))))

(define-public dune
  (package
    (name "dune")
    (version "1.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/dune/releases/"
                                  "download/" version "/dune-" version ".tbz"))
              (sha256
               (base32
                "0dmf0wbfmgdy5plz1bjiisc2hjgblvxsnrqjmw2c8y45v1h23mdz"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; require odoc
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "./configure")
             #t)))))
    (home-page "https://github.com/ocaml/dune")
    (synopsis "OCaml build system")
    (description "Dune is a build system that was designed to simplify the
release of Jane Street packages.  It reads metadata from @file{dune} files
following a very simple s-expression syntax.")
    (license license:expat)))

(define-public ocaml-migrate-parsetree
  (package
    (name "ocaml-migrate-parsetree")
    (version "1.2.0")
    (home-page "https://github.com/ocaml-ppx/ocaml-migrate-parsetree")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit (string-append "v" version))))
       (sha256
        (base32
         "16kas19iwm4afijv3yxd250s08absabmdcb4yj57wc8r4fmzv5dm"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("ocaml-ppx-derivers" ,ocaml-ppx-derivers)
       ("ocamlbuild" ,ocamlbuild)
       ("ocaml-result" ,ocaml-result)))
    (synopsis "OCaml parsetree convertor")
    (description "This library converts between parsetrees of different OCaml
versions.  For each version, there is a snapshot of the parsetree and conversion
functions to the next and/or previous version.")
    (license license:lgpl2.1+)))
 
(define-public ocaml-ppx-tools-versioned
  (package
    (name "ocaml-ppx-tools-versioned")
    (version "5.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml-ppx/"
                                  "ppx_tools_versioned/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1x2xfjpkzbcz4rza1d7gh3ipliw6jqfcklbsln82v3561qgkqgmh"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
     `(("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)))
    (home-page "https://github.com/let-def/ppx_tools_versioned")
    (synopsis "Variant of ppx_tools")
    (description "This package is a variant of ppx_tools based on
ocaml-migrate-parsetree")
    (license license:expat)))

(define-public ocaml-bitstring
  (package
    (name "ocaml-bitstring")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/thanatonauts/bitstring/"
                                  "get/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15jjk2pq1vx311gl49s5ag6x5y0654x35w75z07g7kr2q334hqps"))))
    (build-system dune-build-system)
    (native-inputs
     `(("camlp4" ,camlp4)
       ("time" ,time)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bisect" ,ocaml-bisect)))
    (propagated-inputs
     `(("camlp4" ,camlp4)
       ("ocaml-ppx-tools-versioned" ,ocaml-ppx-tools-versioned)))
    (arguments
     `(#:tests? #f; Tests fail to build
       #:jbuild? #t))
    (properties
      `((ocaml4.02-variant . ,(delay ocaml4.02-bitstring))))
    (home-page "https://github.com/xguerin/bitstring")
    (synopsis "Bitstrings and bitstring matching for OCaml")
    (description "Adds Erlang-style bitstrings and matching over bitstrings as
a syntax extension and library for OCaml.  You can use this module to both parse
and generate binary formats, files and protocols.  Bitstring handling is added
as primitives to the language, making it exceptionally simple to use and very
powerful.")
    (license license:isc)))
 
(define-public ocaml4.02-bitstring
  (let ((base (package-with-ocaml4.02 (strip-ocaml4.02-variant ocaml-bitstring))))
    (package
      (inherit base)
      (version "2.1.1")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/xguerin/bitstring"
                                    "/archive/v" version ".tar.gz"))
                (file-name (string-append "ocaml-bitstring-" version ".tar.gz"))
                (sha256
                 (base32
                  "0vy8ibrxccii1jbsk5q6yh1kxjigqvi7lhhcmizvd5gfhf7mfyc8"))
                (patches (search-patches "ocaml-bitstring-fix-configure.patch"))))
      (build-system ocaml-build-system)
      (arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib
         #:configure-flags
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
           (add-after 'install 'link-lib
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (stubs (string-append out
                                            "/lib/ocaml/site-lib/stubslibs"))
                      (lib (string-append out
                                          "/lib/ocaml/site-lib/bitstring")))
                 (mkdir-p stubs)
                 (symlink (string-append lib "/dllbitstring.so")
                          (string-append stubs "/dllbitstring.so")))
               #t))
           (add-before 'configure 'fix-configure
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "Makefile.in"
                 (("@abs_top_builddir@")
                  (string-append "@abs_top_builddir@:" (getenv "LIBRARY_PATH"))))
               (substitute* "configure"
                 (("-/bin/sh") (string-append "-" (assoc-ref inputs "bash")
                                              "/bin/sh"))))))))
      (native-inputs
       `(("camlp4" ,camlp4-4.02)
         ("time" ,time)
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("bisect" ,ocaml4.02-bisect)))
      (propagated-inputs
       `(("camlp4" ,camlp4-4.02))))))

(define-public ocaml-result
  (package
    (name "ocaml-result")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/janestreet/result"
                                  "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
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
 
(define-public ocaml4.02-result
  (package-with-ocaml4.02 ocaml-result))

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
     `(("opam" ,opam)
       ("ocamlbuild" ,ocamlbuild)))
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
 
(define-public ocaml4.02-topkg
  (package-with-ocaml4.02 ocaml-topkg))

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
     `(("opam" ,opam)
       ("ocamlbuild" ,ocamlbuild)))
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

(define-public ocaml4.02-rresult
  (package-with-ocaml4.02 ocaml-rresult))

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
     `(("ocamlbuild" ,ocamlbuild)
       ("pkg-config" ,pkg-config)))
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

(define-public ocaml4.02-sqlite3
  (package-with-ocaml4.02 ocaml-sqlite3))

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
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)))
    (home-page "https://github.com/Chris00/ocaml-csv")
    (synopsis "Pure OCaml functions to read and write CSV")
    (description
     "@dfn{Comma separated values} (CSV) is a simple tabular format supported
by all major spreadsheets.  This library implements pure OCaml functions to
read and write files in this format as well as some convenience functions to
manipulate such data.")
    (license (package-license camlp4))))

(define-public ocaml4.02-csv
  (package-with-ocaml4.02 ocaml-csv))

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
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)))
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

(define-public ocaml4.02-mtime
  (package-with-ocaml4.02 ocaml-mtime))

(define-public ocaml-cmdliner
  (package
    (name "ocaml-cmdliner")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/cmdliner/releases/"
                                  "cmdliner-" version ".tbz"))
              (sha256
               (base32
                "18jqphjiifljlh9jg8zpl6310p3iwyaqphdkmf89acyaix0s4kj1"))))
    (build-system ocaml-build-system)
    (inputs
     `(("ocaml-result" ,ocaml-result)))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)))
    (arguments
     `(#:tests? #f
       #:build-flags '("native=true" "native-dynlink=true")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           ;; The makefile says 'adjust on cli invocation'
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make" "install" (string-append "PREFIX=" out))
               #t)))
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

(define-public ocaml4.02-cmdliner
  (package-with-ocaml4.02 ocaml-cmdliner))

(define-public ocaml-fmt
  (package
    (name "ocaml-fmt")
    (version "0.8.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://erratique.ch/software/fmt/releases/fmt-"
                            version ".tbz"))
        (sha256 (base32
                  "1zj9azcxcn6skmb69ykgmi9z8c50yskwg03wqgh87lypgjdcz060"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)
       ("topkg" ,ocaml-topkg)))
    (propagated-inputs
     `(("result" ,ocaml-result)
       ("ocaml-uchar" ,ocaml-uchar)
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

(define-public ocaml4.02-fmt
  (package-with-ocaml4.02 ocaml-fmt))

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
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)
       ("topkg" ,ocaml-topkg)))
    (arguments
     `(#:tests? #f
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

(define-public ocaml4.02-astring
  (package-with-ocaml4.02 ocaml-astring))

(define-public ocaml-alcotest
  (package
    (name "ocaml-alcotest")
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mirage/alcotest/releases/"
                                  "download/" version "/alcotest-" version ".tbz"))
              (sha256
                (base32
                  "0szwjxvaahgynsx0apj81jxj3ki6yz4is9mh2wkcbx66qy7n6fvb"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "alcotest"
       #:test-target "."))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)))
    (propagated-inputs
     `(("ocaml-astring" ,ocaml-astring)
       ("ocaml-cmdliner" ,ocaml-cmdliner)
       ("ocaml-fmt" ,ocaml-fmt)
       ("ocaml-result" ,ocaml-result)
       ("ocaml-uuidm" ,ocaml-uuidm)))
    (home-page "https://github.com/mirage/alcotest")
    (synopsis "Lightweight OCaml test framework")
    (description "Alcotest exposes simple interface to perform unit tests.  It
exposes a simple TESTABLE module type, a check function to assert test
predicates and a run function to perform a list of unit -> unit test callbacks.
Alcotest provides a quiet and colorful output where only faulty runs are fully
displayed at the end of the run (with the full logs ready to inspect), with a
simple (yet expressive) query language to select the tests to run.")
    (license license:isc)))

(define-public ocaml4.02-alcotest
  (package-with-ocaml4.02 ocaml-alcotest))

(define-public ocaml-ppx-tools
  (package
    (name "ocaml-ppx-tools")
    (version "5.1+4.06.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/alainfrisch/ppx_tools/archive/"
                            version ".tar.gz"))
        (sha256 (base32
                  "0mncpy9v2mcjgnj7s2vqpp2b1ixv54djicfx66ic9wny9d202gj1"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases (delete 'configure))
       #:tests? #f))
    (properties
      `((ocaml4.02-variant . ,(delay ocaml4.02-ppx-tools))))
    (home-page "https://github.com/alainfrisch/ppx_tools")
    (synopsis "Tools for authors of ppx rewriters and other syntactic tools")
    (description "Tools for authors of ppx rewriters and other syntactic tools.")
    (license license:expat)))

(define-public ocaml4.02-ppx-tools
  (let ((base (package-with-ocaml4.02 (strip-ocaml4.02-variant ocaml-ppx-tools))))
    (package
      (inherit base)
      (version "5.0+4.02.0")
      (source
        (origin
          (method url-fetch)
          (uri (string-append "https://github.com/alainfrisch/ppx_tools/archive/"
                              version ".tar.gz"))
          (sha256 (base32
                    "0rjg4rngi8k9873z4zq95zn9hj8qyw1vcrf11y15aqasfpqq16rc")))))))

(define-public ocaml-react
  (package
    (name "ocaml-react")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://erratique.ch/software/react/releases/react-"
                            version ".tbz"))
        (sha256 (base32
                  "1aj8w79gdd9xnrbz7s5p8glcb4pmimi8jp9f439dqnf6ih3mqb3v"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)
       ("ocaml-topkg" ,ocaml-topkg)))
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
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

(define-public ocaml4.02-react
  (package-with-ocaml4.02 ocaml-react))

(define-public ocaml-ssl
  (package
    (name "ocaml-ssl")
    (version "0.5.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/savonet/ocaml-ssl.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32
                  "0fviq8xhp3qk7pmkl7knchywsipxgb7p0z489hj8qnf2sx8xzdmy"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list "OCAMLFIND_LDCONF=ignore")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda* (#:key #:allow-other-keys)
             (system* "sh" "bootstrap")
             (substitute* "src/OCamlMakefile"
               (("/bin/sh") (which "bash")))
             (substitute* "configure"
               (("/bin/sh") (which "bash")))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("which" ,which)))
    (propagated-inputs `(("openssl" ,openssl)))
    (home-page "https://github.com/savonet/ocaml-ssl/")
    (synopsis "OCaml bindings for OpenSSL")
    (description
     "OCaml-SSL is a set of bindings for OpenSSL, a library for communicating
through Transport Layer Security (@dfn{TLS}) encrypted connections.")
    (license license:lgpl2.1)))

(define-public ocaml4.02-ssl
  (package-with-ocaml4.02 ocaml-ssl))

(define-public ocaml-lwt
  (package
    (name "ocaml-lwt")
    (version "4.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ocsigen/lwt/archive/" version
                            ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256 (base32
                  "0mhh019bjkg5xfvpy1pxs4xdxb759fyydmgb6l4j0qww1qgr8klp"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f; require lwt_ppx
       #:jbuild? #t
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure
           (lambda _
             (invoke "ocaml" "src/util/configure.ml" "-use-libev" "true")
             #t)))))
    (native-inputs
     `(("ocaml-cppo" ,ocaml-cppo)
       ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
       ("pkg-config" ,pkg-config)
       ("ppx-tools-versioned" ,ocaml-ppx-tools-versioned)))
    (inputs
     `(("libev" ,libev)
       ("glib" ,glib)))
    (propagated-inputs
     `(("result" ,ocaml-result)
       ("ocaml-ssl" ,ocaml-ssl)
       ("ocaml-react" ,ocaml-react)))
    (home-page "https://github.com/ocsigen/lwt")
    (synopsis "Cooperative threads and I/O in monadic style")
    (description "Lwt provides typed, composable cooperative threads.  These
make it easy to run normally-blocking I/O operations concurrently in a single
process.  Also, in many cases, Lwt threads can interact without the need for
locks or other synchronization primitives.")
    (license license:lgpl2.1)))

(define-public ocaml4.02-lwt
  (package-with-ocaml4.02 ocaml-lwt))

(define-public ocaml-lwt-log
  (package
    (name "ocaml-lwt-log")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/aantron/lwt_log/archive/" version
                            ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256 (base32
                  "1lr62j2266pbsi54xmzsfvl2z7fi7smhak7fp1ybl8hssxwi6in2"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f; require lwt_ppx
       #:jbuild? #t))
    (propagated-inputs
     `(("lwt" ,ocaml-lwt)))
    (home-page "https://github.com/aantron/lwt_log")
    (synopsis "Logging library")
    (description "This package provides a deprecated logging component for
ocaml lwt.")
    (license license:lgpl2.1)))

(define-public ocaml4.02-lwt-log
  (package-with-ocaml4.02 ocaml-lwt-log))

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
    (arguments
     `(#:tests? #f
       #:build-flags (list "build" "--with-js_of_ocaml" "false")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)))
    (propagated-inputs
     `(("fmt" ,ocaml-fmt)
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

(define-public ocaml4.02-logs
  (package-with-ocaml4.02 ocaml-logs))

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
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)
       ("astring" ,ocaml-astring)))
    (home-page "http://erratique.ch/software/fpath")
    (synopsis "File system paths for OCaml")
    (description "Fpath is an OCaml module for handling file system paths with
POSIX or Windows conventions.  Fpath processes paths without accessing the
file system and is independent from any system library.")
    (license license:isc)))

(define-public ocaml4.02-fpath
  (package-with-ocaml4.02 ocaml-fpath))

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
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)
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

(define-public ocaml4.02-bos
  (package-with-ocaml4.02 ocaml-bos))

(define-public ocaml-xmlm
  (package
    (name "ocaml-xmlm")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/xmlm/releases/"
                                  "xmlm-" version ".tbz"))
              (sha256
                (base32
                  "1rrdxg5kh9zaqmgapy9bhdqyxbbvxxib3bdfg1vhw4rrkp1z0x8n"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("ocaml-topkg" ,ocaml-topkg)
       ("opam" ,opam)))
    (home-page "http://erratique.ch/software/xmlm")
    (synopsis "Streaming XML codec for OCaml")
    (description "Xmlm is a streaming codec to decode and encode the XML data
format.  It can process XML documents without a complete in-memory
representation of the data.")
    (license license:isc)))

(define-public ocaml4.02-xmlm
  (package-with-ocaml4.02 ocaml-xmlm))

(define-public ocaml-ulex
  (package
    (name "ocaml-ulex")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/whitequark/ulex/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
                (base32
                  "16gnbhqs6y2v89vw4igzvxdf2g8ybh5643636824aldcv8sscac0"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases (delete 'configure))
       #:tests? #f
       #:make-flags
       (list "all.opt"
             (string-append "OCAMLBUILD=ocamlbuild -byte-plugin "
                            "-cflags -I," (assoc-ref %build-inputs "camlp4")
                            "/lib/ocaml/site-lib/camlp4"))))
    (native-inputs
     `(("camlp4" ,camlp4)
       ("ocamlbuild" ,ocamlbuild)))
    (home-page "http://www.cduce.org/download.html#side")
    (synopsis "Lexer generator for Unicode and OCaml")
    (description "Lexer generator for Unicode and OCaml.")
    (license license:expat)))

(define-public ocaml4.02-ulex
  (package-with-ocaml4.02 ocaml-ulex))

(define-public ocaml-uchar
  (package
    (name "ocaml-uchar")
    (version "0.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ocaml/uchar/releases/download/v"
                            version "/uchar-" version ".tbz"))
        (sha256 (base32
                  "1w2saw7zanf9m9ffvz2lvcxvlm118pws2x1wym526xmydhqpyfa7"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "native=true" "native-dynlink=true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)))
    (home-page "https://github.com/ocaml/uchar")
    (synopsis "Compatibility library for OCaml's Uchar module")
    (description "The uchar package provides a compatibility library for the
`Uchar` module introduced in OCaml 4.03.")
    (license license:lgpl2.1)))

(define-public ocaml4.02-uchar
  (package-with-ocaml4.02 ocaml-uchar))

(define-public ocaml-uutf
  (package
    (name "ocaml-uutf")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/uutf/releases/"
                                  "uutf-" version ".tbz"))
              (sha256
                (base32
                  "1gp96dcggq7s84934vimxh89caaxa77lqiff1yywbwkilkkjcfqj"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)
       ("topkg" ,ocaml-topkg)))
    (propagated-inputs
     `(("uchar" ,ocaml-uchar)
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

(define-public ocaml4.02-uutf
  (package-with-ocaml4.02 ocaml-uutf))

(define-public ocaml-jsonm
  (package
    (name "ocaml-jsonm")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/jsonm/releases/"
                                  "jsonm-" version ".tbz"))
              (sha256
                (base32
                  "1176dcmxb11fnw49b7yysvkjh0kpzx4s48lmdn5psq9vshp5c29w"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)
       ("topkg" ,ocaml-topkg)))
    (propagated-inputs
     `(("uutf" ,ocaml-uutf)
       ("cmdliner" ,ocaml-cmdliner)))
    (home-page "http://erratique.ch/software/jsonm")
    (synopsis "Non-blocking streaming JSON codec for OCaml")
    (description "Jsonm is a non-blocking streaming codec to decode and encode
the JSON data format.  It can process JSON text without blocking on IO and
without a complete in-memory representation of the data.")
    (license license:isc)))

(define-public ocaml4.02-jsonm
  (package-with-ocaml4.02 ocaml-jsonm))

(define-public ocaml-ocurl
  (package
    (name "ocaml-ocurl")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ygrek.org.ua/p/release/ocurl/ocurl-"
                                  version ".tar.gz"))
              (sha256
                (base32
                  "1ax3xdlzgb1zg7d0wr9nwgmh6a45a764m0wk8p6mx07ad94hz0q9"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-/bin/sh
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure"
               (("-/bin/sh") (string-append "-" (which "bash")))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs `(("curl" ,curl)))
    (home-page "http://ocurl.forge.ocamlcore.org/")
    (synopsis "OCaml bindings for libcurl")
    (description "Client-side URL transfer library, supporting HTTP and a
multitude of other network protocols (FTP/SMTP/RTSP/etc).")
    (license license:isc)))

(define-public ocaml4.02-ocurl
  (package-with-ocaml4.02 ocaml-ocurl))

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
       ("ocamlbuild" ,ocamlbuild)
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

(define-public ocaml4.02-base64
  (package-with-ocaml4.02 ocaml-base64))

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
     `(#:ocaml ,ocaml-4.02
       #:findlib ,ocaml4.02-findlib
       #:phases
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
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.camlcity.org/download/"
                                  "omake-" version ".tar.gz"))
              (sha256
               (base32
                "07bdg1h5i7qnlv9xq81ad5hfypl10hxm771h4rjyl5cn8plhfcgz"))
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
    (properties
      `((ocaml4.02-variant . ,(delay ocaml4.02-omake))))
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

(define-public ocaml4.02-omake
  (let ((base (package-with-ocaml4.02 (strip-ocaml4.02-variant omake))))
    (package
      (inherit base)
      (version "0.10.2")
      (source (origin
                (method url-fetch)
                (uri (string-append "http://download.camlcity.org/download/"
                                    "omake-" version ".tar.gz"))
                (sha256
                 (base32
                  "1znnlkpz89hk44byvnl1pr92ym6hwfyyw2qm9clq446r6l2z4m64"))
                (patches (search-patches "omake-fix-non-determinism.patch")))))))

(define-public ocaml-batteries
  (package
    (name "ocaml-batteries")
    (version "2.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml-batteries-team/"
                                  "batteries-included/releases/download/v"
                                  version "/batteries-" version ".tar.gz"))
              (sha256
               (base32
                "0lkdmv2my5hirkswmci2cch341n6wkkj2q8apjhs83sg6528caxi"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("qtest" ,ocaml-qtest)
       ("bisect" ,ocaml-bisect)
       ("ounit" ,ocaml-ounit)))
    (propagated-inputs
     `(("ocaml-num" ,ocaml-num)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check) ; tests are run by the build phase
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((files
                     (map (lambda (str)
                            (substring str 0 (- (string-length str) 1)))
                          (append
                            (find-files "src" ".*.mliv")
                            (find-files "src" ".*.mlv")
                            (find-files "src" ".*.mlp")))))
               (apply invoke "ocamlbuild" "-no-links" "-use-ocamlfind" "-I" "num"
                      "-lflag" "-dllpath-all" files)
               (for-each (lambda (file)
                           (copy-file (string-append "_build/" file) file))
                         files))
             (invoke "ocamlbuild" "-no-links" "-use-ocamlfind" "-I" "num"
                     "-lflag" "-dllpath-all" "build/mkconf.byte")
             (copy-file "_build/build/mkconf.byte" "build/mkconf.byte")
             (invoke "make" "all")
             #t)))))
    (properties
      `((ocaml4.02-variant . ,(delay ocaml4.02-batteries))))
    (home-page "http://batteries.forge.ocamlcore.org/")
    (synopsis "Development platform for the OCaml programming language")
    (description "Define a standard set of libraries which may be expected on
every compliant installation of OCaml and organize these libraries into a
hierarchy of modules.")
    (license license:lgpl2.1+)))

(define-public ocaml4.02-batteries
  (let ((base (package-with-ocaml4.02 (strip-ocaml4.02-variant ocaml-batteries))))
    (package
      (inherit base)
      (version "2.5.3")
      (source (origin
                (method url-fetch)
                (uri (ocaml-forge-uri "batteries" version 1650))
                (sha256
                 (base32
                  "1a97w3x2l1jr5x9kj5gqm1x6b0q9fjqdcsvls7arnl3bvzgsia0n"))))
      (propagated-inputs '()))))

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
                        (string-append stubs "/dllpcre_stubs.so")))
             #t)))))
    (native-inputs
     `(("batteries" ,ocaml-batteries)
       ("ocamlbuild" ,ocamlbuild)
       ("pcre:bin" ,pcre "bin")))
    (propagated-inputs `(("pcre" ,pcre)))
    (home-page "https://mmottl.github.io/pcre-ocaml")
    (synopsis "Bindings to the Perl Compatibility Regular Expressions library")
    (description "Pcre-ocaml offers library functions for string pattern
matching and substitution, similar to the functionality offered by the Perl
language.")
    (license license:lgpl2.1+))); with the OCaml link exception

(define-public ocaml4.02-pcre
  (package-with-ocaml4.02 ocaml-pcre))

(define-public ocaml-expect
  (package
    (name "ocaml-expect")
    (version "0.0.6")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1736))
              (sha256
               (base32
                "098qvg9d4yrqzr5ax291y3whrpax0m3sx4gi6is0mblc96r9yqk0"))))
    (arguments
     `(#:tests? #f))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("ocaml-num" ,ocaml-num)
       ("ocaml-pcre" ,ocaml-pcre)
       ("ounit" ,ocaml-ounit)))
    (propagated-inputs `(("batteries" ,ocaml-batteries)))
    (home-page "https://forge.ocamlcore.org/projects/ocaml-expect/")
    (synopsis "Simple implementation of expect")
    (description "Help building unitary testing of interactive program.  You
can match the question using a regular expression or a timeout.")
    (license license:lgpl2.1+))) ; with the OCaml static compilation exception

(define-public ocaml4.02-fileutils
  (package
    (name "ocaml4.02-fileutils")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1728))
              (sha256
               (base32
                "1rc4cqlvdhbs55i85zfbfhz938fsy4fj6kwlkfm3ra7bpwn8bmpd"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:ocaml ,ocaml-4.02
       #:findlib ,ocaml4.02-findlib))
    (native-inputs
     `(("ounit" ,ocaml4.02-ounit)))
    (home-page "http://ocaml-fileutils.forge.ocamlcore.org")
    (synopsis "Pure OCaml functions to manipulate real file and filename")
    (description "Library to provide pure OCaml functions to manipulate real
file (POSIX like) and filename.")
    (license license:lgpl2.1+))) ; with the OCaml static compilation exception

(define-public ocaml-oasis
  (package
    (name "ocaml-oasis")
    (version "0.4.11")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1757))
              (sha256
               (base32
                "0bn13mzfa98dq3y0jwzzndl55mnywaxv693z6f1rlvpdykp3vdqq"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                (substitute* "test/test-main/Test.ml"
                  ;; most of these tests fail because ld cannot find crti.o, but according
                  ;; to the log file, the environment variables {LD_,}LIBRARY_PATH
                  ;; are set correctly when LD_LIBRARY_PATH is defined beforhand.
                  (("TestBaseCompat.tests;") "")
                  (("TestExamples.tests;") "")
                  (("TestFull.tests;") "")
                  (("TestPluginDevFiles.tests;") "")
                  (("TestPluginInternal.tests;") "")
                  (("TestPluginOCamlbuild.tests;") "")
                  (("TestPluginOMake.tests;") ""))
                #t))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("ocamlify" ,ocamlify)
       ("ocamlmod" ,ocamlmod)))
    (home-page "https://oasis.forge.ocamlcore.org")
    (synopsis "Integrates a configure, build, install system in OCaml projects")
    (description "OASIS is a tool to integrate a configure, build and install
system in your OCaml projects.  It helps to create standard entry points in your
build system and allows external tools to analyse your project easily.")
    (license license:lgpl2.1+))) ; with ocaml static compilation exception

(define-public ocaml4.02-oasis
  (package-with-ocaml4.02 ocaml-oasis))

(define-public ocaml-js-build-tools
  (package
    (name "ocaml-js-build-tools")
    (version "113.33.06")
    (source (janestreet-origin "js-build-tools" version
              "0r8z4fz8iy5y6hkdlkpwf6rk4qigcr3dzyv35585xgg2ahf12zy6"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("ocamlbuild" ,ocamlbuild)
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

(define-public ocaml4.02-js-build-tools
  (package-with-ocaml4.02 ocaml-js-build-tools))

(define-public ocaml4.02-bin-prot
  (package
    (name "ocaml4.02-bin-prot")
    (version "113.33.03")
    (source (janestreet-origin "bin_prot" version
               "1ws8c017z8nbj3vw92ndvjk9011f71rmp3llncbv8r5fc76wqv3l"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/bin_prot/")
    (synopsis "Binary protocol generator")
    (description "This library contains functionality for reading and writing
OCaml-values in a type-safe binary protocol.  It is extremely efficient,
typically supporting type-safe marshalling and unmarshalling of even highly
structured values at speeds sufficient to saturate a gigabit connection.  The
protocol is also heavily optimized for size, making it ideal for long-term
storage of large amounts of data.")
    (license license:asl2.0)))

(define-public ocaml4.02-fieldslib
  (package
    (name "ocaml4.02-fieldslib")
    (version "113.33.03")
    (source (janestreet-origin "fieldslib" version
               "1rm3bn54bzk2hlq8f3w067ak8s772w4a8f78i3yh79vxfzq8ncvv"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/fieldslib/")
    (synopsis "Syntax extension to record fields")
    (description "Syntax extension to define first class values representing
record fields, to get and set record fields, iterate and fold over all fields
of a record and create new record values.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-core
  (package
    (name "ocaml4.02-ppx-core")
    (version "113.33.03")
    (source (janestreet-origin "ppx_core" version
               "0f69l4czhgssnhb5ds2j9dbqjyz8dp1y3i3x0i4h6pxnms20zbxa"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (inputs `(("ppx-tools" ,ocaml4.02-ppx-tools)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
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

(define-public ocaml4.02-ppx-optcomp
  (package
    (name "ocaml4.02-ppx-optcomp")
    (version "113.33.03")
    (source (janestreet-origin "ppx_optcomp" version
               "13an8p2r7sd0d5lv54mlzrxdni47408bwqi3bjcx4m6005170q30"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_optcomp/")
    (synopsis "Optional compilation for OCaml")
    (description "Ppx_optcomp stands for Optional Compilation.  It is a tool
used to handle optional compilations of pieces of code depending of the word
size, the version of the compiler, ...")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-driver
  (package
    (name "ocaml4.02-ppx-driver")
    (version "113.33.03")
    (source (janestreet-origin "ppx_driver" version
              "011zzr45897j49b7iiybb29k7pspcx36mlnp7nh6pxb8b0ga76fh"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)
       ("ppx-optcomp" ,ocaml4.02-ppx-optcomp)))
    (propagated-inputs
     `(("ppx-optcomp" ,ocaml4.02-ppx-optcomp)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
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
    (version "1.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/mjambon/cppo/archive/v" version
                            ".tar.gz"))
        (sha256 (base32
                  "1dkm3d5h6h56y937gcdk2wixlpzl59vv5pmiafglr89p20kf7gqf"))
        (file-name (string-append name "-" version ".tar.gz"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "--profile" "release")))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)))
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

(define-public ocaml4.02-cppo
  (package-with-ocaml4.02 ocaml-cppo))

;; this package is not reproducible. This is related to temporary filenames
;; such as findlib_initxxxxx where xxxxx is random.
(define-public ocaml4.02-ppx-deriving
  (package
    (name "ocaml4.02-ppx-deriving")
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
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("cppo" ,ocaml4.02-cppo)
       ("ounit" ,ocaml4.02-ounit)
       ("opam" ,opam)))
    (propagated-inputs
     `(("result" ,ocaml4.02-result)
       ("ppx-tools" ,ocaml4.02-ppx-tools)))
    (arguments
     `(#:ocaml ,ocaml-4.02
       #:findlib ,ocaml4.02-findlib
       #:phases
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

(define-public ocaml4.02-ppx-type-conv
  (package
    (name "ocaml4.02-ppx-type-conv")
    (version "113.33.03")
    (source
      (janestreet-origin "ppx_type_conv" version
        "1sp602ads2f250nv4d4rgw54d14k7flyhb4w8ff084f657hhmxv2"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-deriving" ,ocaml4.02-ppx-deriving)
       ("ppx-core" ,ocaml4.02-ppx-core)
       ("ppx-driver" ,ocaml4.02-ppx-driver)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_type_conv/")
    (synopsis "Support Library for type-driven code generators")
    (description "The type_conv library factors out functionality needed by
different preprocessors that generate code from type specifications.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-inline-test
  (package
    (name "ocaml4.02-ppx-inline-test")
    (version "113.33.03")
    (source (janestreet-origin "ppx_inline_test" version
              "0859ni43fl39dd5g6cxfhq0prxmx59bd2bfq8jafp593ba4q0icq"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (propagated-inputs
      `(("ppx-driver" ,ocaml4.02-ppx-driver)
        ("ppx-tools" ,ocaml4.02-ppx-tools)
        ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_inline_test/")
    (synopsis "Syntax extension for writing in-line tests in ocaml code")
    (description "Syntax extension for writing in-line tests in ocaml code.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-bench
  (package
    (name "ocaml4.02-ppx-bench")
    (version "113.33.03")
    (source (janestreet-origin "ppx_bench" version
                   "1hky3y17qpb925rymf97wv54di9gaqdmkix7wpkjw14qzl512b68"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml4.02-ppx-driver)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-inline-test" ,ocaml4.02-ppx-inline-test)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_bench/")
    (synopsis "Syntax extension for writing in-line benchmarks in ocaml code")
    (description "Syntax extension for writing in-line benchmarks in ocaml code.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-compare
  (package
    (name "ocaml4.02-ppx-compare")
    (version "113.33.03")
    (source (janestreet-origin "ppx_compare" version
              "0bfhi33kq9l4q6zzc6svki2csracz5j4731c3npcy6cs73jynn0z"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml4.02-ppx-driver)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-core" ,ocaml4.02-ppx-core)
       ("ppx-type-conv" ,ocaml4.02-ppx-type-conv)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_compare/")
    (synopsis "Generation of comparison functions from types")
    (description "Generation of fast comparison functions from type expressions
and definitions.  Ppx_compare is a ppx rewriter that derives comparison functions
from type representations.  The scaffolded functions are usually much faster
than ocaml's Pervasives.compare.  Scaffolding functions also gives you more
flexibility by allowing you to override them for a specific type and more safety
by making sure that you only compare comparable values.")
    (license license:asl2.0)))

(define-public ocaml4.02-sexplib
  (package
    (name "ocaml4.02-sexplib")
    (version "113.33.03")
    (source (janestreet-origin "sexplib" version
              "1ffjmj8if9lyv965cgn2ld1xv7g52qsr8mqflbm515ck1i8l2ima"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/sexplib/")
    (synopsis "Library for serializing OCaml values to and from S-expressions")
    (description "Sexplib contains functionality for parsing and pretty-printing
s-expressions.")
    (license license:asl2.0)))

(define-public ocaml4.02-typerep
  (package
    (name "ocaml4.02-typerep")
    (version "113.33.03")
    (source (janestreet-origin "typerep" version
              "1b9v5bmi824a9d4sx0f40ixq0yfcbiqxafg4a1jx95xg9199zafy"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/typerep/")
    (synopsis "Typerep is a library for runtime types")
    (description "Typerep is a library for runtime types.")
    (license license:asl2.0)))

(define-public ocaml4.02-variantslib
  (package
    (name "ocaml4.02-variantslib")
    (version "113.33.03")
    (source (janestreet-origin "variantslib" version
              "05vp799vl38fvl98ga5miwbzh09cnnpapi6q6gdvwyqi6w7s919n"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/variantslib")
    (synopsis "OCaml variants as first class values")
    (description "OCaml variants as first class values.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-sexp-conv
  (package
    (name "ocaml4.02-ppx-sexp-conv")
    (version "113.33.03")
    (source (janestreet-origin "ppx_sexp_conv" version
              "1rbj6d5dl625gdxih34xcrdvikci6h8i2dl9x3wraa4qrgishiw7"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (propagated-inputs
     `(("sexplib" ,ocaml4.02-sexplib)
       ("ppx-core" ,ocaml4.02-ppx-core)
       ("ppx-type-conv" ,ocaml4.02-ppx-type-conv)
       ("ppx-tools" ,ocaml4.02-ppx-tools)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_sexp_conv")
    (synopsis "Generation of S-expression conversion functions from type definitions")
    (description "Generation of S-expression conversion functions from type
definitions.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-variants-conv
  (package
    (name "ocaml4.02-ppx-variants-conv")
    (version "113.33.03")
    (source (janestreet-origin "ppx_variants_conv" version
              "0vnn2l1118cj72413d3f7frlw6yc09l8f64jlzkzbgb9bxpalx34"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-core" ,ocaml4.02-ppx-core)
       ("variantslib" ,ocaml4.02-variantslib)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-type-conv" ,ocaml4.02-ppx-type-conv)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_variants_conv")
    (synopsis "Generation of accessor and iteration functions for ocaml variant
types")
    (description "Generation of accessor and iteration functions for ocaml
variant types.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-here
  (package
    (name "ocaml4.02-ppx-here")
    (version "113.33.03")
    (source (janestreet-origin "ppx_here" version
              "1ay8lfxi0qg3ib2zkwh4h0vqk3gjmxaz572gzab0bbxyqn3z86v7"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml4.02-ppx-driver)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_here")
    (synopsis "Expands [%here] into its location")
    (description "Expands [%here] into its location.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-assert
  (package
    (name "ocaml4.02-ppx-assert")
    (version "113.33.03")
    (source (janestreet-origin "ppx_assert" version
              "1k5kxmqkibp5fk25pgz81f3c1r4mgvb5byzf6bnmxd24y60wn46p"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-compare" ,ocaml4.02-ppx-compare)
       ("ppx-core" ,ocaml4.02-ppx-core)
       ("ppx-driver" ,ocaml4.02-ppx-driver)
       ("ppx-sexp-conv" ,ocaml4.02-ppx-sexp-conv)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-type-conv" ,ocaml4.02-ppx-type-conv)
       ("ppx-sexplib" ,ocaml4.02-sexplib)
       ("ppx-here" ,ocaml4.02-ppx-here)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_assert")
    (synopsis "Assert-like extension nodes that raise useful errors on failure")
    (description "Assert-like extension nodes that raise useful errors on failure.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-enumerate
  (package
    (name "ocaml4.02-ppx-enumerate")
    (version "113.33.03")
    (source (janestreet-origin "ppx_enumerate" version
              "15g7yfv9wg2h9r6k6q1zrhygmsl4xrfn25mrb0i4czjjivzmxjh4"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-type-conv" ,ocaml4.02-ppx-type-conv)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_enumerate")
    (synopsis "Generate a list containing all values of a finite type")
    (description "Ppx_enumerate is a ppx rewriter which generates a definition
for the list of all values of a type (for a type which only has finitely
many values).")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-let
  (package
    (name "ocaml4.02-ppx-let")
    (version "113.33.03")
    (source (janestreet-origin "ppx_let" version
              "0gd6d3gdaqfwjcs7gaw1qxc30i584q6a86ndaj1bx1q63xqd6yx9"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml4.02-ppx-driver)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_let")
    (synopsis "Monadic let-bindings")
    (description "A ppx rewriter for monadic and applicative let bindings,
match expressions, and if expressions.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-typerep-conv
  (package
    (name "ocaml4.02-ppx-typerep-conv")
    (version "113.33.03")
    (source (janestreet-origin "ppx_typerep_conv" version
              "0g0xqm9s1b2jjvxb8yp69281q2s3bwz6sibn10fvgcdawpa0rmrg"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-type-conv" ,ocaml4.02-ppx-type-conv)
       ("ppx-core" ,ocaml4.02-ppx-core)
       ("typerep" ,ocaml4.02-typerep)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_typerep_conv")
    (synopsis "Generation of runtime types from type declarations")
    (description "Automatic generation of runtime types from type definitions.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-sexp-value
  (package
    (name "ocaml4.02-ppx-sexp-value")
    (version "113.33.03")
    (source (janestreet-origin "ppx_sexp_value" version
              "0m3ag23mbqm0i2pv1dzilfks15ipa5q60mf57a0cd3p0pvarq10g"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml4.02-ppx-driver)
       ("ppx-here" ,ocaml4.02-ppx-here)
       ("ppx-sexp-conv" ,ocaml4.02-ppx-sexp-conv)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_sexp_value/")
    (synopsis "Simplify building s-expressions from ocaml values")
    (description "A ppx rewriter that simplifies building s-expressions from
ocaml values.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-pipebang
  (package
    (name "ocaml4.02-ppx-pipebang")
    (version "113.33.03")
    (source (janestreet-origin "ppx_pipebang" version
              "1965c7hymp26ncmjs0pfxi2s5jlj60z2c9b194lgcwxqiav56pcw"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml4.02-ppx-driver)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_pipebang/")
    (synopsis "Inline reverse application operators `|>` and `|!`")
    (description "A ppx rewriter that inlines reverse application operators
@code{|>} and @code{|!}.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-bin-prot
  (package
    (name "ocaml4.02-ppx-bin-prot")
    (version "113.33.03")
    (source (janestreet-origin "ppx_bin_prot" version
              "173kjv36giik11zgfvsbzwfbpr66dm2pcha9vf990jgzh8hqz39h"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("bin-prot" ,ocaml4.02-bin-prot)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-type-conv" ,ocaml4.02-ppx-type-conv)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_bin_prot/")
    (synopsis "Generation of bin_prot readers and writers from types")
    (description "Generation of binary serialization and deserialization
functions from type definitions.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-fail
  (package
    (name "ocaml4.02-ppx-fail")
    (version "113.33.03")
    (source (janestreet-origin "ppx_fail" version
              "1dwgad0f05gqp5rnwf9dcasidpfi7q3mrpazsw3a2vijjblbhjgn"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml4.02-ppx-driver)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-here" ,ocaml4.02-ppx-here)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_fail/")
    (synopsis "Add location to calls to failwiths")
    (description "Syntax extension that makes [failwiths] always include a
position.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-custom-printf
  (package
    (name "ocaml4.02-ppx-custom-printf")
    (version "113.33.03")
    (source (janestreet-origin "ppx_custom_printf" version
              "11jlx0n87g2j1vyyp343dibx7lvvwig5j5q0nq0b80kbsq0k6yr8"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-sexp-conv" ,ocaml4.02-ppx-sexp-conv)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-core" ,ocaml4.02-ppx-core)
       ("ppx-driver" ,ocaml4.02-ppx-driver)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_custom_printf/")
    (synopsis "Printf-style format-strings for user-defined string conversion")
    (description "Extensions to printf-style format-strings for user-defined
string conversion.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-sexp-message
  (package
    (name "ocaml4.02-ppx-sexp-message")
    (version "113.33.03")
    (source (janestreet-origin "ppx_sexp_message" version
              "084w1l3gnyw4ri9vbn7bv9b2xkw1520qczfxpxdarfivdrz8xr68"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml4.02-ppx-driver)
       ("ppx-here" ,ocaml4.02-ppx-here)
       ("ppx-sexp-conv" ,ocaml4.02-ppx-sexp-conv)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_sexp_message/")
    (synopsis "A ppx rewriter for easy construction of s-expressions")
    (description "Ppx_sexp_message aims to ease the creation of s-expressions
in OCaml.  This is mainly motivated by writing error and debugging messages,
where one needs to construct a s-expression based on various element of the
context such as function arguments.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-fields-conv
  (package
    (name "ocaml4.02-ppx-fields-conv")
    (version "113.33.03")
    (source (janestreet-origin "ppx_fields_conv" version
              "1vzbdz27g5qhhfs7wx6rjf979q4xyssxqbmp6sc1sxknbghslbdv"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)
       ("ppx-core" ,ocaml4.02-ppx-core)))
    (propagated-inputs
     `(("fieldslib" ,ocaml4.02-fieldslib)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-core" ,ocaml4.02-ppx-core)
       ("ppx-type-conv" ,ocaml4.02-ppx-type-conv)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_fields_conv/")
    (synopsis "Generation of accessor and iteration functions for ocaml records")
    (description "Ppx_fields_conv is a ppx rewriter that can be used to define
first class values representing record fields, and additional routines, to get
and set record fields, iterate and fold over all fields of a record and create
new record values.")
    (license license:asl2.0)))

(define-public ocaml-seq
  (package
    (name "ocaml-seq")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/c-cube/seq/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02lb2d9i12bxrz2ba5wygk2bycan316skqlyri0597q7j9210g8r"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((install-dir (string-append (assoc-ref outputs "out")
                                               "/lib/ocaml/site-lib/seq")))
               (mkdir-p install-dir)
               (with-output-to-file (string-append install-dir "/META")
                 (lambda _
                   (display "name=\"seq\"
version=\"[distributed with ocaml]\"
description=\"dummy package for compatibility\"
requires=\"\"")))
             #t))))))
    (properties
      `((ocaml4.02-variant . ,(delay ocaml4.02-seq))))
    (home-page "https://github.com/c-cube/seq")
    (synopsis "OCaml's standard iterator type")
    (description "This package is a compatibility package for OCaml's
standard iterator type starting from 4.07.")
    (license license:lgpl2.1+)))

(define-public ocaml4.02-seq
  (let ((base (package-with-ocaml4.02 (strip-ocaml4.02-variant ocaml-seq))))
    (package
      (inherit base)
      (arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib
         #:tests? #f; no tests
         #:phases (modify-phases %standard-phases (delete 'configure)))))))

(define-public ocaml-re
  (package
    (name "ocaml-re")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/ocaml-re//archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pdb0mr6z5ax6szblr3f5lbdnqq9grm97cmsfjmdma60yrx2rqhd"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "--profile" "release")))
    (propagated-inputs
     `(("ocaml-seq" ,ocaml-seq)))
    (native-inputs
     `(("ounit" ,ocaml-ounit)))
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

(define-public ocaml4.02-re
  (package-with-ocaml4.02 ocaml-re))

(define-public ocaml4.02-ppx-expect
  (package
    (name "ocaml4.02-ppx-expect")
    (version "113.33.03")
    (source (janestreet-origin "ppx_expect" version
              "03sbs4s5i8l9syr45v25f5hzy7msd2b47k2a9wsq9m43d4imgkrc"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("fieldslib" ,ocaml4.02-fieldslib)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("ppx-assert" ,ocaml4.02-ppx-assert)
       ("ppx-compare" ,ocaml4.02-ppx-compare)
       ("ppx-core" ,ocaml4.02-ppx-core)
       ("ppx-custom-printf" ,ocaml4.02-ppx-custom-printf)
       ("ppx-driver" ,ocaml4.02-ppx-driver)
       ("ppx-fields-conv" ,ocaml4.02-ppx-fields-conv)
       ("ppx-inline-test" ,ocaml4.02-ppx-inline-test)
       ("ppx-sexp-conv" ,ocaml4.02-ppx-sexp-conv)
       ("ppx-variants-conv" ,ocaml4.02-ppx-variants-conv)
       ("re" ,ocaml4.02-re)
       ("sexplib" ,ocaml4.02-sexplib)
       ("variantslib" ,ocaml4.02-variantslib)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_expect/")
    (synopsis "Cram like framework for OCaml")
    (description "Expect-test is a framework for writing tests in OCaml, similar
to Cram.  Expect-tests mimic the existing inline tests framework with the
let%expect_test construct.  The body of an expect-test can contain
output-generating code, interleaved with %expect extension expressions to denote
the expected output.")
    (license license:asl2.0)))

(define-public ocaml4.02-ppx-jane
  (package
    (name "ocaml4.02-ppx-jane")
    (version "113.33.03")
    (source (janestreet-origin "ppx_jane" version
              "0bjxkhmzgm6x9dcvjwybbccn34khbvyyjimcbaja30fp6qcqk5yl"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-assert" ,ocaml4.02-ppx-assert)
       ("ppx-bench" ,ocaml4.02-ppx-bench)
       ("ppx-bin-prot" ,ocaml4.02-ppx-bin-prot)
       ("ppx-compare" ,ocaml4.02-ppx-compare)
       ("ppx-custom-printf" ,ocaml4.02-ppx-custom-printf)
       ("ppx-deriving" ,ocaml4.02-ppx-deriving)
       ("ppx-enumerate" ,ocaml4.02-ppx-enumerate)
       ("ppx-expect" ,ocaml4.02-ppx-expect)
       ("ppx-fail" ,ocaml4.02-ppx-fail)
       ("ppx-fields-conv" ,ocaml4.02-ppx-fields-conv)
       ("ppx-here" ,ocaml4.02-ppx-here)
       ("ppx-inline-test" ,ocaml4.02-ppx-inline-test)
       ("ppx-let" ,ocaml4.02-ppx-let)
       ("ppx-pipebang" ,ocaml4.02-ppx-pipebang)
       ("ppx-sexp-conv" ,ocaml4.02-ppx-sexp-conv)
       ("ppx-sexp-message" ,ocaml4.02-ppx-sexp-message)
       ("ppx-sexp-value" ,ocaml4.02-ppx-sexp-value)
       ("ppx-typerep-conv" ,ocaml4.02-ppx-typerep-conv)
       ("ppx-variants-conv" ,ocaml4.02-ppx-variants-conv)))
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/ppx_jane/")
    (synopsis "Standard Jane Street ppx rewriters")
    (description "Ppx_jane is a ppx_driver including all standard ppx rewriters.")
    (license license:asl2.0)))

(define-public ocaml4.02-core-kernel
  (package
    (name "ocaml4.02-core-kernel")
    (version "113.33.03")
    (source (janestreet-origin "core_kernel" version
               "0fl23jrwivixawhxinbwaw9cabqnzn7fini7dxpxjjvkxdc8ip5y"))
    (native-inputs
     `(("js-build-tools" ,ocaml4.02-js-build-tools)
       ("ppx-jane" ,ocaml4.02-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("bin_prot" ,ocaml4.02-bin-prot)
       ("ppx-assert" ,ocaml4.02-ppx-assert)
       ("ppx-bench" ,ocaml4.02-ppx-bench)
       ("ppx-driver" ,ocaml4.02-ppx-driver)
       ("ppx-expect" ,ocaml4.02-ppx-expect)
       ("ppx-inline-test" ,ocaml4.02-ppx-inline-test)
       ("typerep" ,ocaml4.02-typerep)
       ("sexplib" ,ocaml4.02-sexplib)
       ("variantslib" ,ocaml4.02-variantslib)
       ("result" ,ocaml4.02-result)
       ("fieldslib" ,ocaml4.02-fieldslib)))
    (build-system ocaml-build-system)
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/core_kernel/")
    (synopsis "Portable standard library for OCaml")
    (description "Core is an alternative to the OCaml standard library.

Core_kernel is the system-independent part of Core.  It is aimed for cases when
the full Core is not available, such as in Javascript.")
    (license license:asl2.0)))

(define-public ocaml4.02-async-kernel
  (package
    (name "ocaml4.02-async-kernel")
    (version "113.33.03")
    (source (janestreet-origin "async_kernel" version
              "04bjsaa23j831r09r38x6xx9nhryvp0z5ihickvhxqa4fb2snyvd"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml4.02-js-build-tools)
       ("ppx-jane" ,ocaml4.02-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("core-kernel" ,ocaml4.02-core-kernel)))
    (build-system ocaml-build-system)
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/async_kernel/")
    (synopsis "Monadic concurrency library")
    (description "Async-kernel is a library for concurrent programming in OCaml.")
    (license license:asl2.0)))

(define-public ocaml4.02-async-rpc-kernel
  (package
    (name "ocaml4.02-async-rpc-kernel")
    (version "113.33.03")
    (source (janestreet-origin "async_rpc_kernel" version
             "0y97h9pkb00v7jpf87m8cbb0ffkclj9g26ph6sq97q8dpisnkjwh"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml4.02-js-build-tools)
       ("ppx-jane" ,ocaml4.02-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-kernel" ,ocaml4.02-async-kernel)))
    (build-system ocaml-build-system)
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/async_rpc_kernel/")
    (synopsis "Platform-independent core of the Async RPC library")
    (description "Async_rpc_kernel is the platform-independent core of
the Async RPC library.")
    (license license:asl2.0)))

(define-public ocaml4.02-core
  (package
    (name "ocaml4.02-core")
    (version "113.33.03")
    (source (janestreet-origin "core" version
              "1znll157qg56g9d3247fjibv1hxv3r9wxgr4nhy19j2vzdh6a268"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml4.02-js-build-tools)
       ("ppx-jane" ,ocaml4.02-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("core-kernel" ,ocaml4.02-core-kernel)))
    (build-system ocaml-build-system)
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/core/")
    (synopsis "Alternative to OCaml's standard library")
    (description "The Core suite of libraries is an alternative to OCaml's
standard library that was developed by Jane Street.")
    (license license:asl2.0)))

(define-public ocaml4.02-async-unix
  (package
    (name "ocaml4.02-async-unix")
    (version "113.33.03")
    (source (janestreet-origin "async_unix" version
              "1fwl0lfrizllcfjk8hk8m7lsz9ha2jg6qgk4gssfyz377qvpcq4h"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml4.02-js-build-tools)
       ("ppx-jane" ,ocaml4.02-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-kernel" ,ocaml4.02-async-kernel)
       ("core" ,ocaml4.02-core)))
    (build-system ocaml-build-system)
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/async_unix")
    (synopsis "Asynchronous execution library for Unix")
    (description "Async_unix is an asynchronous execution library for Unix.")
    (license license:asl2.0)))

(define-public ocaml4.02-async-extra
  (package
    (name "ocaml4.02-async-extra")
    (version "113.33.03")
    (source (janestreet-origin "async_extra" version
              "1si8jgiq5xh5sl9f2b7f9p17p7zx5h1pg557x2cxywi2x7pxqg4f"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml4.02-js-build-tools)
       ("ppx-jane" ,ocaml4.02-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-rpc-kernel" ,ocaml4.02-async-rpc-kernel)
       ("async-unix" ,ocaml4.02-async-unix)
       ("core" ,ocaml4.02-core)))
    (build-system ocaml-build-system)
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
    (home-page "https://github.com/janestreet/async_extra")
    (synopsis "Extra functionnalities for the async library")
    (description "Async_extra provides additional functionnalities for the
async library.")
    (license license:asl2.0)))

(define-public ocaml4.02-async
  (package
    (name "ocaml4.02-async")
    (version "113.33.03")
    (source (janestreet-origin "async" version
              "0210fyhcs12kpmmd26015bgivkfd2wqkyn3c5wd7688d0f872y25"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml4.02-js-build-tools)
       ("ppx-jane" ,ocaml4.02-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-extra" ,ocaml4.02-async-extra)))
    (build-system ocaml-build-system)
    (arguments
      (ensure-keyword-arguments janestreet-arguments
       `(#:ocaml ,ocaml-4.02
         #:findlib ,ocaml4.02-findlib)))
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
    (native-inputs
     `(("cppo" ,ocaml-cppo)
       ("ocamlbuild" ,ocamlbuild)))
    (home-page "https://github.com/OCamlPro/ocplib-endian")
    (synopsis "Optimised functions to read and write int16/32/64 from strings
and bigarrays")
    (description "Optimised functions to read and write int16/32/64 from strings
and bigarrays, based on new primitives added in version 4.01.  It works on
strings, bytes and bigstring (Bigarrys of chars), and provides submodules for
big- and little-endian, with their unsafe counter-parts.")
    (license license:lgpl2.1)))

(define-public ocaml4.02-ocplib-endian
  (package-with-ocaml4.02 ocaml-ocplib-endian))

(define-public ocaml4.02-cstruct
  (package
    (name "ocaml4.02-cstruct")
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
     `(#:ocaml ,ocaml-4.02
       #:findlib ,ocaml4.02-findlib
       #:configure-flags
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
     `(("ounit" ,ocaml4.02-ounit)
       ("ppx-tools" ,ocaml4.02-ppx-tools)
       ("camlp4" ,camlp4-4.02)))
    (propagated-inputs
     `(("ocplib-endian" ,ocaml4.02-ocplib-endian)
       ("lwt" ,ocaml4.02-lwt)
       ("async" ,ocaml4.02-async)
       ("sexplib" ,ocaml4.02-sexplib)))
    (home-page "https://github.com/mirage/ocaml-cstruct")
    (synopsis "Access C structures via a camlp4 extension")
    (description "Cstruct is a library and syntax extension to make it easier
to access C-like structures directly from OCaml.  It supports both reading and
writing to these structures, and they are accessed via the Bigarray module.")
    (license license:isc)))

(define-public ocaml4.02-hex
  (package
    (name "ocaml4.02-hex")
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
    (arguments
     `(#:ocaml ,ocaml-4.02
       #:findlib ,ocaml4.02-findlib))
    (propagated-inputs
     `(("cstruct" ,ocaml4.02-cstruct)))
    (home-page "https://github.com/mirage/ocaml-hex/")
    (synopsis "Minimal library providing hexadecimal converters")
    (description "Hex is a minimal library providing hexadecimal converters.")
    (license license:isc)))

(define-public ocaml4.02-ezjsonm
  (package
    (name "ocaml4.02-ezjsonm")
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
     `(("alcotest" ,ocaml4.02-alcotest)))
    (propagated-inputs
     `(("hex" ,ocaml4.02-hex)
       ("jsonm" ,ocaml4.02-jsonm)
       ("lwt" ,ocaml4.02-lwt)
       ("sexplib" ,ocaml4.02-sexplib)))
    (arguments
     `(#:configure-flags (list "--enable-lwt")
       #:ocaml ,ocaml-4.02
       #:findlib ,ocaml4.02-findlib))
    (home-page "https://github.com/mirage/ezjsonm/")
    (synopsis "Read and write JSON data")
    (description "Ezjsonm provides more convenient (but far less flexible) input
and output functions that go to and from [string] values than jsonm.  This avoids
the need to write signal code, which is useful for quick scripts that manipulate
JSON.")
    (license license:isc)))

(define-public ocaml4.02-uri
  (package
    (name "ocaml4.02-uri")
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
    (arguments
     `(#:ocaml ,ocaml-4.02
       #:findlib ,ocaml4.02-findlib))
    (native-inputs
     `(("ounit" ,ocaml4.02-ounit)))
    (propagated-inputs
     `(("ppx-sexp-conv" ,ocaml4.02-ppx-sexp-conv)
       ("re" ,ocaml4.02-re)
       ("ppx-deriving" ,ocaml4.02-ppx-deriving)
       ("sexplib" ,ocaml4.02-sexplib)
       ("stringext" ,ocaml4.02-stringext)))
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

(define-public ocaml4.02-easy-format
  (package-with-ocaml4.02 ocaml-easy-format))

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
     `(#:ocaml ,ocaml-4.02
       #:findlib ,ocaml4.02-findlib
       #:use-make? #t
       #:make-flags
       (list (string-append "BUILDFLAGS=\"-cflags -I,"
                            (assoc-ref %build-inputs "camlp4")
                            "/lib/ocaml/site-lib/camlp4/Camlp4Parsers\""))))
    (native-inputs `(("camlp4" ,camlp4-4.02)))
    (propagated-inputs `(("camlp4" ,camlp4-4.02)))
    (home-page "https://github.com/diml/optcomp")
    (synopsis "Optional compilation for OCaml")
    (description "Optcomp provides an optional compilation facility with
cpp-like directives.")
    (license license:bsd-3)))

(define-public ocaml-piqilib
  (package
    (name "ocaml-piqilib")
    (version "0.6.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alavrik/piqi/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1ssccnwqzfyf7syfq2fv4zyhwayxwd75rhq9y28mvq1w6qbww4l7"))
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
               (invoke "./configure" "--prefix" out "--ocaml-libdir"
                       (string-append out "/lib/ocaml/site-lib")))
             #t))
       (add-after 'build 'build-ocaml
         (lambda* (#:key outputs #:allow-other-keys)
           (invoke "make" "ocaml")
           #t))
       (add-after 'install 'install-ocaml
         (lambda* (#:key outputs #:allow-other-keys)
           (invoke "make" "ocaml-install")
           #t))
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
       ("easy-format" ,ocaml-easy-format)
       ("base64" ,ocaml-base64)))
    (home-page "http://piqi.org")
    (synopsis "Data serialization and conversion library")
    (description "Piqilib is the common library used by the piqi command-line
tool and piqi-ocaml.")
    (license license:asl2.0)))

(define-public ocaml4.02-piqilib
  (package-with-ocaml4.02 ocaml-piqilib))

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
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)))
    (propagated-inputs
     `(("cmdliner" ,ocaml-cmdliner)
       ("topkg" ,ocaml-topkg)))
    (home-page "http://erratique.ch/software/uuidm")
    (synopsis "Universally unique identifiers for OCaml")
    (description "Uuidm is an OCaml module implementing 128 bits universally
unique identifiers (UUIDs) version 3, 5 (named based with MD5, SHA-1 hashing)
and 4 (random based) according to RFC 4122.")
    (license license:isc)))

(define-public ocaml4.02-uuidm
  (package-with-ocaml4.02 ocaml-uuidm))

(define-public ocaml-graph
  (package
    (name "ocaml-graph")
    (version "1.8.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ocamlgraph.lri.fr/download/"
                                  "ocamlgraph-" version ".tar.gz"))
              (sha256
               (base32
                "0m9g16wrrr86gw4fz2fazrh8nkqms0n863w7ndcvrmyafgxvxsnr"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:install-target "install-findlib"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-shell
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CONFIG_SHELL" (string-append (assoc-ref inputs "bash")
                                                   "/bin/sh")))))))
    (inputs `(("lablgtk" ,lablgtk)))
    (properties `((upstream-name . "ocamlgraph")))
    (home-page "http://ocamlgraph.lri.fr/")
    (synopsis "Graph library for OCaml")
    (description "OCamlgraph is a generic graph library for OCaml.")
    (license license:lgpl2.1)))

(define-public ocaml4.02-graph
  (package-with-ocaml4.02 ocaml-graph))

(define-public ocaml-piqi
  (package
    (name "ocaml-piqi")
    (version "0.7.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alavrik/piqi-ocaml/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0da0v2v28aj65b0cwpvvmcqyrfvz94pp3j5zgfdr1jrl5s37c21g"))))
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
       ("camlp4" ,camlp4)
       ("protobuf" ,protobuf))) ; for tests
    (propagated-inputs
     `(("num" ,ocaml-num)
       ("piqilib" ,ocaml-piqilib)))
    (properties
      `((ocaml4.02-variant . ,(delay ocaml4.02-piqi))))
    (home-page "https://github.com/alavrik/piqi-ocaml")
    (synopsis "Protocol serialization system for OCaml")
    (description "Piqi is a multi-format data serialization system for OCaml.
It provides a uniform interface for serializing OCaml data structures to JSON,
XML and Protocol Buffers formats.")
    (license license:asl2.0)))

(define-public ocaml4.02-piqi
  (let ((base (package-with-ocaml4.02 (strip-ocaml4.02-variant ocaml-piqi))))
    (package
      (inherit base)
      (propagated-inputs
       `(("piqilib" ,ocaml4.02-piqilib))))))

(define-public bap
  (package
    (name "bap")
    (version "1.3.0")
    (home-page "https://github.com/BinaryAnalysisPlatform/bap")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "0v95v9mp7mg8fj25ry0w7566zd9xp6cs8nnqj4l38q54fb1hfav9"))
              (file-name (string-append name "-" version ".tar.gz"))))
   (build-system ocaml-build-system)
   (native-inputs
    `(("oasis" ,ocaml4.02-oasis)
      ("clang" ,clang-3.8)
      ("ounit" ,ocaml4.02-ounit)))
   (propagated-inputs
    `(("core-kernel" ,ocaml4.02-core-kernel)
      ("ppx-driver" ,ocaml4.02-ppx-driver)
      ("bin-prot" ,ocaml4.02-bin-prot)
      ("uri" ,ocaml4.02-uri)
      ("llvm" ,llvm-3.8)
      ("gmp" ,gmp)
      ("clang-runtime" ,clang-runtime)
      ("fileutils" ,ocaml4.02-fileutils)
      ("cmdliner" ,ocaml4.02-cmdliner)
      ("zarith" ,ocaml4.02-zarith)
      ("uuidm" ,ocaml4.02-uuidm)
      ("camlzip" ,ocaml4.02-camlzip)
      ("frontc" ,ocaml-frontc)
      ("ezjsonm" ,ocaml4.02-ezjsonm)
      ("ocurl" ,ocaml4.02-ocurl)
      ("piqi" ,ocaml4.02-piqi)
      ("ocamlgraph" ,ocaml4.02-graph)
      ("bitstring" ,ocaml4.02-bitstring)
      ("ppx-jane" ,ocaml4.02-ppx-jane)
      ("re" ,ocaml4.02-re)))
   (inputs `(("llvm" ,llvm-3.8)))
   (arguments
    `(#:ocaml ,ocaml-4.02
      #:findlib ,ocaml4.02-findlib
      #:use-make? #t
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (invoke "./configure" "--prefix"
                    (assoc-ref outputs "out")
                    "--libdir"
                    (string-append
                      (assoc-ref outputs "out")
                      "/lib/ocaml/site-lib")
                    "--with-llvm-version=3.8"
                    "--with-llvm-config=llvm-config"
                    "--enable-everything")
            (substitute* "plugins/objdump/objdump_main.ml"
              (("Re_perl") "Re.Perl"))
            (substitute* "oasis/objdump"
              (("re.pcre") "re.pcre, re.perl")))))))
   (synopsis "Binary Analysis Platform")
   (description "Binary Analysis Platform is a framework for writing program
analysis tools, that target binary files.  The framework consists of a plethora
of libraries, plugins, and frontends.  The libraries provide code reusability,
the plugins facilitate extensibility, and the frontends serve as entry points.")
   (license license:expat)))

(define-public ocaml-camomile
  (package
    (name "ocaml-camomile")
    (version "1.0.1")
    (home-page "https://github.com/yoriyuki/Camomile")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/releases/download/" version
                                  "/camomile-" version ".tbz"))
              (sha256
               (base32
                "01ssjrqz41jvrqh27jxnh9cx7ywi9b5sgsykd00i7z9nrcwhlfy2"))))
    (build-system dune-build-system)
    (native-inputs
     `(("camlp4" ,camlp4)))
    (arguments
     `(#:build-flags (list "--profile" "realease")
       #:tests? #f))
    (synopsis "Comprehensive Unicode library")
    (description "Camomile is a Unicode library for OCaml.  Camomile provides
Unicode character type, UTF-8, UTF-16, UTF-32 strings, conversion to/from about
200 encodings, collation and locale-sensitive case mappings, and more.  The
library is currently designed for Unicode Standard 3.2.")
    ;; with an exception for linked libraries to use a different license
    (license license:lgpl2.0+)))

(define-public ocaml4.02-camomile
  (package-with-ocaml4.02 ocaml-camomile))

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
                "1cy07pwvbrlysszs938yd74yyvvbgkffpb82qrjph77zf0h2gdi7"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:ocaml ,ocaml-4.02
       #:findlib ,ocaml4.02-findlib
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "PREFIX" out))
             #t)))))
    (native-inputs
     `(("menhir" ,ocaml4.02-menhir)))
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
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/diml/zed/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19m5vrj60vg1b63qfsv0aabdlzgn40cqmx65s3wafqi4fs9xp6jn"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _ (invoke "jbuilder" "build")))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "jbuilder" "install" "--prefix" out)))))))
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

(define-public ocaml4.02-zed
  (package-with-ocaml4.02 ocaml-zed))

(define-public ocaml-lambda-term
  (package
    (name "ocaml-lambda-term")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/diml/lambda-term/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hy5ryagqclgdm9lzh1qil5mrynlypv7mn6qm858hdcnmz9zzn0l"))))
    (build-system dune-build-system)
    (arguments
     `(#:build-flags (list "--profile" "release")
       #:tests? #f))
    (propagated-inputs
     `(("lwt" ,ocaml-lwt)
       ("lwt-log" ,ocaml-lwt-log)
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

(define-public ocaml4.02-lambda-term
  (package-with-ocaml4.02 ocaml-lambda-term))

(define-public ocaml-utop
  (package
    (name "ocaml-utop")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml-community/utop/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1414snwmqaxs1x8wbpjf6fn3jsl01hq0phrr7639xmb5vh15mgd4"))))
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
               (invoke "jbuilder" "install"
                       "--prefix" out
                       "--libdir" libdir)))))))
    (native-inputs
     `(("ocaml" ,ocaml)
       ("cppo" ,ocaml-cppo)
       ("jbuilder" ,ocaml-jbuilder)))
    (propagated-inputs
     `(("ocaml-findlib" ,ocaml-findlib)
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

(define-public ocaml-integers
  (package
    (name "ocaml-integers")
    (version "0.2.2")
    (home-page "https://github.com/ocamllabs/ocaml-integers")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page
                                  "/releases/download/v0.2.2/integers-"
                                  version ".tbz"))
              (file-name (string-append name "-" version ".tbz"))
              (sha256
               (base32
                "08b1ljw88ny3l0mdq6xmffjk8anfc77igryva5jz1p6f4f746ywk"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)))
    (inputs
     `(("topkg" ,ocaml-topkg)
       ("opam" ,opam)))
    (synopsis "Various signed and unsigned integer types for OCaml")
    (description "The ocaml-integers library provides a number of 8-, 16-, 32-
and 64-bit signed and unsigned integer types, together with aliases such as
long and size_t whose sizes depend on the host platform.")
    (license license:expat)))

(define-public ocaml-ctypes
  (package
   (name "ocaml-ctypes")
   (version "0.14.0")
   (home-page "https://github.com/ocamllabs/ocaml-ctypes")
   (source (origin
             (method url-fetch)
             (uri (string-append home-page "/archive/" version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "0zrsd42q2nciyg9375g2kydqax6ay299rhyfgms59qiw7d9ylyp9"))))
   (build-system ocaml-build-system)
   (arguments
    `(#:tests? #f; require an old lwt
      #:make-flags
      (list (string-append "INSTALL_HEADERS = $(wildcard $($(PROJECT).dir)/*.h)"))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (inputs
    `(("libffi" ,libffi)
      ("ounit" ,ocaml-ounit)
      ("integers" ,ocaml-integers)
      ("lwt" ,ocaml-lwt)
      ("topkg" ,ocaml-topkg)
      ("opam" ,opam)))
   (synopsis "Library for binding to C libraries using pure OCaml")
   (description "Ctypes is a library for binding to C libraries using pure
OCaml.  The primary aim is to make writing C extensions as straightforward as
possible.  The core of ctypes is a set of combinators for describing the
structure of C types -- numeric types, arrays, pointers, structs, unions and
functions.  You can use these combinators to describe the types of the
functions that you want to call, then bind directly to those functions -- all
without writing or generating any C!")
   (license license:expat)))

(define-public ocaml-ocb-stubblr
  (package
   (name "ocaml-ocb-stubblr")
   (version "0.1.1")
   (home-page "https://github.com/pqwy/ocb-stubblr")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   home-page "/releases/download/v0.1.1/ocb-stubblr-"
                   version ".tbz"))
             (file-name (string-append name "-" version ".tbz"))
             (sha256
              (base32
               "167b7x1j21mkviq8dbaa0nmk4rps2ilvzwx02igsc2706784z72f"))))
   (build-system ocaml-build-system)
   (arguments
    `(#:build-flags (list "build" "--tests" "true")
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))
   (inputs
    `(("topkg" ,ocaml-topkg)
      ("opam" ,opam)))
   (native-inputs
    `(("astring" ,ocaml-astring)
      ("ocamlbuild" ,ocamlbuild)))
   (synopsis "OCamlbuild plugin for C stubs")
   (description "Ocb-stubblr is about ten lines of code that you need to
repeat over, over, over and over again if you are using ocamlbuild to build
OCaml projects that contain C stubs.")
   (license license:isc)))

(define-public ocaml-tsdl
  (package
    (name "ocaml-tsdl")
    (version "0.9.1")
    (home-page "http://erratique.ch/software/tsdl")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/releases/tsdl-"
                                  version ".tbz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08bb97fhvz829fb0sgjn2p20mp7b04v98zy2qxpk2w390a6c4b34"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags '("build")
       #:tests? #f; tests require a display device
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("topkg" ,ocaml-topkg)
       ("result" ,ocaml-result)
       ("sdl2" ,sdl2)
       ("integers" ,ocaml-integers)
       ("ctypes" ,ocaml-ctypes)))
    (synopsis "Thin bindings to SDL for OCaml")
    (description "Tsdl is an OCaml library providing thin bindings to the
cross-platform SDL C library.")
    (license license:isc)))

(define-public dedukti
  (package
    (name "dedukti")
    (version "2.6.0")
    (home-page "https://deducteam.github.io/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deducteam/dedukti.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0frl3diff033i4fmq304b8wbsdnc9mvlhmwd7a3zd699ng2lzbxb"))))
    (inputs
     `(("menhir" ,ocaml-menhir)))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "make")
             #t))
         (replace 'check
           (lambda _
             (invoke "make" "tests")
             #t))
         (add-before 'install 'set-binpath
           ;; Change binary path in the makefile
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (substitute* "GNUmakefile"
                 (("BINDIR = (.*)$")
                  (string-append "BINDIR = " out "/bin"))))
             #t))
         (replace 'install
           (lambda _
             (invoke "make" "install")
             #t)))))
    (synopsis "Proof-checker for the λΠ-calculus modulo theory, an extension of
the λ-calculus")
    (description "Dedukti is a proof-checker for the λΠ-calculus modulo
theory.  The λΠ-calculus is an extension of the simply typed λ-calculus with
dependent types.  The λΠ-calculus modulo theory is itself an extension of the
λΠ-calculus where the context contains variable declaration as well as rewrite
rules.  This system is not designed to develop proofs, but to check proofs
developed in other systems.  In particular, it enjoys a minimalistic syntax.")
    (license license:cecill-c)))

(define-public emacs-dedukti-mode
  (let ((commit "d7c3505a1046187de3c3aeb144455078d514594e"))
    (package
      (name "emacs-dedukti-mode")
      (version (git-version "0" "0" commit))
      (home-page "https://github.com/rafoo/dedukti-mode")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "1842wikq24c8rg0ac84vb1qby9ng1nssxswyyni4kq85lng5lcrp"))
                (file-name (git-file-name name version))))
      (inputs
       `(("dedukti" ,dedukti)))
      (build-system emacs-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'patch-dkpath
             (lambda _
               (let ((dkcheck-path (which "dkcheck")))
                 (substitute* "dedukti-mode.el"
                   (("dedukti-path \"(.*)\"")
                    (string-append "dedukti-path \"" dkcheck-path "\"")))))))))
      (synopsis "Emacs major mode for Dedukti files")
      (description "This package provides an Emacs major mode for editing
Dedukti files.")
      (license license:cecill-b))))

(define-public emacs-flycheck-dedukti
  (let ((commit "3dbff5646355f39d57a3ec514f560a6b0082a1cd"))
    (package
      (name "emacs-flycheck-dedukti")
      (version (git-version "0" "0" commit))
      (home-page "https://github.com/rafoo/flycheck-dedukti")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "1ffpxnwl3wx244n44mbw81g00nhnykd0lnid29f4aw1av7w6nw8l"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (inputs
       `(("dedukti-mode" ,emacs-dedukti-mode)
         ("flycheck-mode" ,emacs-flycheck)))
      (synopsis "Flycheck integration for the dedukti language")
      (description "This package provides a frontend for Flycheck to perform
syntax checking on dedukti files.")
      (license license:cecill-b))))

(define-public ocaml-ppx-inline-test
  (package
    (name "ocaml-ppx-inline-test")
    (version "0.11.0")
    (home-page "https://github.com/janestreet/ppx_inline_test")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11n94fz1asjf5vqdgriv0pvsa5lbfpqcyk525c7816w23vskcvq6"))))
    (build-system dune-build-system)
    (arguments
     ;see home page README for further information
     `(#:tests? #f))
    (inputs
     `(("ocaml-base" ,ocaml-base)
       ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
       ("ocaml-compiler-libs" ,ocaml-compiler-libs)
       ("ocaml-sexplib0" ,ocaml-sexplib0)
       ("ocaml-stdio" ,ocaml-stdio)
       ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (synopsis "Syntax extension for writing in-line tests in ocaml code")
    (description "This package contains a syntax extension for writing
in-line tests in ocaml code.  It is part of Jane Street's PPX rewriters
collection.")
    (license license:expat)))

(define-public ocaml-bindlib
  (package
    (name "ocaml-bindlib")
    (version "5.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rlepigre/ocaml-bindlib.git")
             (commit (string-append "ocaml-bindlib_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1f8kr81w8vsi4gv61xn1qbc6zrzkjp8l9ix0942vjh4gjxc74v75"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:use-make? #t
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "make")))
         (replace 'install
           (lambda _
             (invoke "make" "install"))))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("ocaml-findlib" ,ocaml-findlib)))
    (home-page "https://rlepigre.github.io/ocaml-bindlib/")
    (synopsis "OCaml Bindlib library for bound variables")
    (description "Bindlib is a library allowing the manipulation of data
structures with bound variables.  It is particularly useful when writing ASTs
for programming languages, but also for manipulating terms of the λ-calculus
or quantified formulas.")
    (license license:gpl3+)))

(define-public ocaml-earley
  (package
    (name "ocaml-earley")
    (version "2.0.0")
    (home-page "https://github.com/rlepigre/ocaml-earley")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18k7bi7krc4bvqnhijz1q0pfr0nfahghfjifci8rh1q4i5zd0xz5"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (synopsis "Parsing library based on Earley Algorithm")
    (description "Earley is a parser combinator library base on Earley's
algorithm.  It is intended to be used in conjunction with an OCaml syntax
extension which allows the definition of parsers inside the language.  There
is also support for writing OCaml syntax extensions in a camlp4 style.")
    (license license:cecill-b)))

(define-public ocaml-timed
  (package
    (name "ocaml-timed")
    (version "1.0")
    (home-page "https://github.com/rlepigre/ocaml-timed")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append home-page ".git"))
                    (commit (string-append name "_" version))))
              (sha256
               (base32
                "0hfxz710faxy5yk97bkfnw87r732jcxxhmjppwrbfdb6pd0wks96"))
              (file-name (git-file-name name version))))
    (build-system ocaml-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "make")))
         (replace 'install
           (lambda _
             (invoke "make" "install")))
         (replace 'check
           (lambda _
             (invoke "make" "tests"))))))
    (synopsis "Timed references for imperative state")
    (description "Timed references for imperative state.  This module provides
an alternative type for references (or mutable cells) supporting undo/redo
operations.  In particular, an abstract notion of time is used to capture the
state of the references at any given point, so that it can be restored.  Note
that usual reference operations only have a constant time / memory overhead
(compared to those of the standard library).

Moreover, we provide an alternative implementation based on the references
of the standard library (Pervasives module).  However, it is less efficient
than the first one.")
    (license license:expat)))

(define-public ocaml-biniou
 (package
   (name "ocaml-biniou")
   (version "1.2.0")
   (home-page "https://github.com/mjambon/biniou")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mjpgwyfq2b2izjw0flmlpvdjgqpq8shs89hxj1np2r50csr8dcb"))))
   (build-system dune-build-system)
   (inputs
    `(("ocaml-easy-format" ,ocaml-easy-format)))
   (native-inputs
    `(("which" ,which)))
   (synopsis "Data format designed for speed, safety, ease of use and backward
compatibility")
   (description "Biniou (pronounced \"be new\" is a binary data format
designed for speed, safety, ease of use and backward compatibility as
protocols evolve.  Biniou is vastly equivalent to JSON in terms of
functionality but allows implementations several times faster (4 times faster
than yojson), with 25-35% space savings.")
   (license license:bsd-3)))

(define-public ocaml-yojson
  (package
    (name "ocaml-yojson")
    (version "1.4.1")
    (home-page "https://github.com/ocaml-community/yojson")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0nwsfkmqpyfab4rxq76q8ff7giyanghw08094jyrp275v99zdjr9"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (inputs
     `(("ocaml-biniou" ,ocaml-biniou)
       ("ocaml-easy-format" ,ocaml-easy-format)))
    (native-inputs
     `(("ocaml-cppo" ,ocaml-cppo)))
    (synopsis "Low-level JSON library for OCaml")
    (description "Yojson is an optimized parsing and printing library for the
JSON format.  It addresses a few shortcomings of json-wheel including 2x
speedup, polymorphic variants and optional syntax for tuples and variants.
@code{ydump} is a pretty printing command-line program provided with the
yojson package.  The program @code{atdgen} can be used to derive OCaml-JSON
serializers and deserializers from type definitions.")
    (license license:bsd-3)))
 
(define-public ocaml-craml
  (package
    (name "ocaml-craml")
    (version "1.0.0")
    (home-page "https://github.com/realworldocaml/craml")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "197xjp4vmzdymf2ndinw271ihpf45h04mx8gqj8ypspxdr5fj1a5"))))
    (build-system dune-build-system)
    (inputs
     `(("ocaml-fmt" ,ocaml-fmt)
       ("ocaml-astring" ,ocaml-astring)
       ("ocaml-logs" ,ocaml-logs)
       ("ocaml-cmdliner" ,ocaml-cmdliner)))
    (synopsis
     "CRAM-testing framework for testing command line applications")
    (description "CRAM is a is functional testing framework for command line
applications.  @code{craml} is freely inspired by the
Mercurial's @code{https://www.selenic.com/blog/?p=663, unified test
format}.  @code{craml} is released as a single binary (called @code{craml}).")
    (license license:isc)))

(define-public ocaml-merlin
  (package
    (name "ocaml-merlin")
    (version "3.2.2")
    (home-page "https://ocaml.github.io/merlin/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/merlin.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15ssgmwdxylbwhld9p1cq8x6kadxyhll5bfyf11dddj6cldna3hb"))))
    (build-system dune-build-system)
    (inputs
     `(("ocaml-biniou" ,ocaml-biniou)
       ("ocaml-yojson" ,ocaml-yojson)
       ("ocaml-easy-format" ,ocaml-easy-format)))
    (native-inputs
     `(("ocaml-findlib" ,ocaml-findlib)))
    (arguments
     '(#:tests? #f)) ;; Errors in tests in version 3.2.2
    (synopsis "Context sensitive completion for OCaml in Vim and Emacs")
    (description "Merlin is an editor service that provides modern IDE
features for OCaml.  Emacs and Vim support is provided out-of-the-box.
External contributors added support for Visual Studio Code, Sublime Text and
Atom.")
    (license license:expat)))

(define-public ocaml-gsl
  (package
    (name "ocaml-gsl")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/mmottl/gsl-ocaml/releases/download/"
         version "/gsl-" version ".tbz"))
       (sha256
        (base32
         "17vcswipliq1b2idbzx1z95kskn1a4q4s5v04igilg0f7lnkaarb"))))
    (build-system ocaml-build-system)
    (inputs
     `(("gsl" ,gsl)))
    (home-page "https://mmottl.github.io/gsl-ocaml")
    (synopsis "Bindings to the GNU Scientific Library")
    (description
     "GSL-OCaml is an interface to the @dfn{GNU scientific library} (GSL) for
the OCaml language.")
    (license license:gpl3+)))

(define-public cubicle
  (package
    (name "cubicle")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://cubicle.lri.fr/cubicle-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10kk80jdmpdvql88sdjsh7vqzlpaphd8vip2lp47aarxjkwjlz1q"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("automake" ,automake)
       ("ocaml" ,ocaml)
       ("which" ,(@@ (gnu packages base) which))))
    (propagated-inputs
     `(("ocaml-num" ,ocaml-num)
       ("z3" ,z3)))
    (arguments
     `(#:configure-flags (list "--with-z3")
       #:make-flags (list "QUIET=")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'configure-for-release
           (lambda _
             (substitute* "Makefile.in"
               (("SVNREV=") "#SVNREV="))
             #t))
         (add-before 'configure 'fix-/bin/sh
           (lambda _
             (substitute* "configure"
               (("-/bin/sh") (string-append "-" (which "sh"))))
             #t))
         (add-before 'configure 'fix-smt-z3wrapper.ml
           (lambda _
             (substitute* "Makefile.in"
               (("\\\\n") ""))
             #t))
         (add-before 'configure 'fix-ocaml-num
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("= \\$\\(FUNCTORYLIB\\)")
                (string-append "= -I "
                               (assoc-ref inputs "ocaml-num")
                               "/lib/ocaml/site-lib"
                               " $(FUNCTORYLIB)")))
             #t)))))
    (home-page "http://cubicle.lri.fr/")
    (synopsis "Model checker for array-based systems")
    (description "Cubicle is a model checker for verifying safety properties
of array-based systems.  This is a syntactically restricted class of
parametrized transition systems with states represented as arrays indexed by
an arbitrary number of processes.  Cache coherence protocols and mutual
exclusion algorithms are typical examples of such systems.")
    (license license:asl2.0)))

(define-public ocaml-sexplib0
  (package
    (name "ocaml-sexplib0")
    (version "0.11.0")
    (home-page "https://github.com/janestreet/sexplib0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07v3ggyss7xhfv14bjk1n87sr42iqwj4cgjiv2lcdfkqk49i2bmi"))))
    (build-system dune-build-system)
    (arguments
     '(#:tests? #f)) ;no tests
    (synopsis "Library containing the definition of S-expressions and some
base converters")
    (description "Part of Jane Street's Core library The Core suite of
libraries is an industrial strength alternative to OCaml's standard library
that was developed by Jane Street, the largest industrial user of OCaml.")
(license license:expat)))

(define-public ocaml-parsexp
  (package
    (name "ocaml-parsexp")
    (version "0.11.0")
    (home-page "https://github.com/janestreet/parsexp")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nyq23s5igd8cf3n4qxprjvhbmb6ighb3fy5mw7hxl0mdgsw5fvz"))))
    (build-system dune-build-system)
    (inputs
     `(("ocaml-sexplib0" ,ocaml-sexplib0)))
    (synopsis "S-expression parsing library")
    (description
     "This library provides generic parsers for parsing S-expressions from
strings or other medium.

The library is focused on performances but still provide full generic
parsers that can be used with strings, bigstrings, lexing buffers,
character streams or any other sources effortlessly.

It provides three different class of parsers:
@itemize
@item
the normal parsers, producing [Sexp.t] or [Sexp.t list] values
@item
the parsers with positions, building compact position sequences so
that one can recover original positions in order to report properly
located errors at little cost
@item
the Concrete Syntax Tree parsers, produce values of type
@code{Parsexp.Cst.t} which record the concrete layout of the s-expression
syntax, including comments
@end itemize

This library is portable and doesn't provide IO functions.  To read
s-expressions from files or other external sources, you should use
parsexp_io.")
    (license license:expat)))

(define-public ocaml-sexplib
  (package
    (name "ocaml-sexplib")
    (version "0.11.0")
    (home-page "https://github.com/janestreet/sexplib")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1qfl0m04rpcjvc4yw1hzh6r16jpwmap0sa9ax6zjji67dz4szpyb"))))
    (build-system dune-build-system)
    (propagated-inputs
     `(("ocaml-num" ,ocaml-num)
       ("ocaml-parsexp" ,ocaml-parsexp)
       ("ocaml-sexplib0" ,ocaml-sexplib0)))
    (synopsis
     "Library for serializing OCaml values to and from S-expressions")
    (description
     "This package is part of Jane Street's Core library. Sexplib contains
functionality for parsing and pretty-printing s-expressions.")
    (license license:expat)))

(define-public ocaml-base
  (package
    (name "ocaml-base")
    (version "0.11.1")
    (home-page "https://github.com/janestreet/base")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j6xb4265jr41vw4fjzak6yr8s30qrnzapnc6rl1dxy8bjai0nir"))))
    (build-system dune-build-system)
    (propagated-inputs
     `(("ocaml-sexplib0" ,ocaml-sexplib0)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           ;; make warnings non fatal (jbuilder behaviour)
           (lambda _
             (invoke "dune" "build" "@install" "--profile=release"))))))
  (synopsis
    "Full standard library replacement for OCaml")
  (description
    "Base is a complete and portable alternative to the OCaml standard
library.  It provides all standard functionalities one would expect
from a language standard library.  It uses consistent conventions
across all of its module.

Base aims to be usable in any context.  As a result system dependent
features such as I/O are not offered by Base.  They are instead
provided by companion libraries such as
@url{https://github.com/janestreet/stdio, ocaml-stdio}.")
  (license license:expat)))

(define-public ocaml-compiler-libs
  (package
    (name "ocaml-compiler-libs")
    (version "0.11.0")
    (home-page "https://github.com/janestreet/ocaml-compiler-libs")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03jds7bszh8wwpfwxb3dg0gyr1j1872wxwx1xqhry5ir0i84bg0s"))))
    (build-system dune-build-system)
    (arguments
     '(#:tests? #f)) ;no tests
    (synopsis "Compiler libraries repackaged")
    (description "This packaeg simply repackages the OCaml compiler libraries
so they don't expose everything at toplevel.  For instance, @code{Ast_helper}
is now @code{Ocaml_common.Ast_helper}.")
    (license license:expat)))

(define-public ocaml-stdio
  (package
    (name "ocaml-stdio")
    (version "0.11.0")
    (home-page "https://github.com/janestreet/stdio")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1facajqhvq34g2wrg368y0ajxd6lrj5b3lyzyj0jhdmraxajjcwn"))))
    (build-system dune-build-system)
    (propagated-inputs
     `(("ocaml-base" ,ocaml-base)
       ("ocaml-sexplib0" ,ocaml-sexplib0)))
    (arguments
     '(#:tests? #f)) ;no tests
    (synopsis "Standard IO library for OCaml")
    (description
     "Stdio implements simple input/output functionalities for OCaml.  It
re-exports the input/output functions of the OCaml standard libraries using
a more consistent API.")
    (license license:expat)))

(define-public ocaml-ppx-derivers
  (package
    (name "ocaml-ppx-derivers")
    (version "1.2")
    (home-page
     "https://github.com/ocaml-ppx/ppx_derivers")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0bnhihl1w31as5w2czly1v3d6pbir9inmgsjg2cj6aaj9v1dzd85"))))
    (build-system dune-build-system)
    (arguments
     '(#:tests? #f)) ;no tests
    (properties `((upstream-name . "ppx_derivers")))
    (synopsis "Shared @code{@@deriving} plugin registry")
    (description
     "Ppx_derivers is a tiny package whose sole purpose is to allow
ppx_deriving and ppx_type_conv to inter-operate gracefully when linked
as part of the same ocaml-migrate-parsetree driver.")
    (license license:bsd-3)))

(define-public ocaml-ppxlib
  (package
    (name "ocaml-ppxlib")
    (version "0.4.0")
    (home-page "https://github.com/ocaml-ppx/ppxlib")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nr4igf5m4prvigvv470dnhfdhdw0p6hz6zw8gnm5bzcv7s2lg5l"))))
    (build-system dune-build-system)
    (propagated-inputs
     `(("ocaml-base" ,ocaml-base)
       ("ocaml-compiler-libs" ,ocaml-compiler-libs)
       ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
       ("ocaml-ppx-derivers" ,ocaml-ppx-derivers)
       ("ocaml-stdio" ,ocaml-stdio)
       ("ocaml-result" ,ocaml-result)
       ("ocaml-sexplib0" ,ocaml-sexplib0)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-topfind
           (lambda* (#:key inputs #:allow-other-keys)
             ;; add the line #directory ".." at the top of each file
             ;; using #use "topfind";; to be able to find topfind
             (let* ((findlib-path (assoc-ref inputs "findlib"))
                    (findlib-libdir
                     (string-append findlib-path "/lib/ocaml/site-lib")))
               (substitute* '("test/base/test.ml"
                              "test/deriving/test.ml"
                              "test/driver/attributes/test.ml"
                              "test/driver/non-compressible-suffix/test.ml"
                              "test/driver/transformations/test.ml")
                 (("#use \"topfind\";;" all)
                  (string-append "#directory \"" findlib-libdir "\"\n"
                                 all))))
             #t)))))
    (synopsis
     "Base library and tools for ppx rewriters")
    (description
     "A comprehensive toolbox for ppx development.  It features:
@itemize
@item an OCaml AST / parser / pretty-printer snapshot, to create a full frontend
independent of the version of OCaml;
@item a library for library for ppx rewriters in general, and type-driven code
generators in particular;
@item
a feature-full driver for OCaml AST transformers;
@item a quotation mechanism allowing to write values representing the
OCaml AST in the OCaml syntax;
@item a generator of open recursion classes from type definitions.
@end itemize")
    (license license:expat)))

(define-public ocaml-ppx-compare
  (package
    (name "ocaml-ppx-compare")
    (version "0.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/ppx_compare.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06bq4m1bsm4jlx4g7wh5m99qky7xm4c2g52kaz6pv25hdn5agi2m"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_compare")))
    (home-page "https://github.com/janestreet/ppx_compare")
    (synopsis "Generation of comparison functions from types")
    (description "Generation of fast comparison functions from type expressions
and definitions.  Ppx_compare is a ppx rewriter that derives comparison functions
from type representations.  The scaffolded functions are usually much faster
than ocaml's Pervasives.compare.  Scaffolding functions also gives you more
flexibility by allowing you to override them for a specific type and more safety
by making sure that you only compare comparable values.")
    (license license:asl2.0)))

(define-public ocaml-fieldslib
  (package
    (name "ocaml-fieldslib")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version) "/files/"
                                  "fieldslib-v" version ".tar.gz"))
              (sha256
               (base32
                "12948pzxrl360lybm9fzyvplgcl87zjbn4m3sk1aw75zk85p1388"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "fieldslib")))
    (home-page "https://github.com/janestreet/fieldslib")
    (synopsis "Syntax extension to record fields")
    (description "Syntax extension to define first class values representing
record fields, to get and set record fields, iterate and fold over all fields
of a record and create new record values.")
    (license license:asl2.0)))

(define-public ocaml-variantslib
  (package
    (name "ocaml-variantslib")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/variantslib-v" version ".tar.gz"))
              (sha256
               (base32
                "1hsdwmkslvk4cznqr4lyyiy7vvk5spil226k0z2in26fxq6y0hf3"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "variantslib")))
    (home-page "https://github.com/janestreet/variantslib")
    (synopsis "OCaml variants as first class values")
    (description "The Core suite of libraries is an alternative to OCaml's
standard library.")
    (license license:asl2.0)))

(define-public ocaml-ppx-fields-conv
  (package
    (name "ocaml-ppx-fields-conv")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_fields_conv-v" version ".tar.gz"))
              (sha256
               (base32
                "07zrd3qky2ppbfl55gpm90rvqa5860xgwcsvihrjmkrw6d0jirkc"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-fieldslib" ,ocaml-fieldslib)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_fields_conv")))
    (home-page "https://github.com/janestreet/ppx_fields_conv")
    (synopsis "Generation of accessor and iteration functions for ocaml records")
    (description "Ppx_fields_conv is a ppx rewriter that can be used to define
first class values representing record fields, and additional routines, to get
and set record fields, iterate and fold over all fields of a record and create
new record values.")
    (license license:asl2.0)))

(define-public ocaml-ppx-sexp-conv
  (package
    (name "ocaml-ppx-sexp-conv")
    (version "0.11.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/ppx_sexp_conv.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pqwnqy1xp309wvdcaax4lg02yk64lq2w03mbgfvf6ps5ry4gis9"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_sexp_conv")))
    (home-page "https://github.com/janestreet/ppx_sexp_conv")
    (synopsis "Generation of S-expression conversion functions from type definitions")
    (description "This package generates S-expression conversion functions from type
definitions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-variants-conv
  (package
    (name "ocaml-ppx-variants-conv")
    (version "0.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/ppx_variants_conv.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yc0gsds5m2nv39zga8nnrca2n75rkqy5dz4xj1635ybz20hhbjd"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-variantslib" ,ocaml-variantslib)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "ppx_variants_conv")))
    (home-page
      "https://github.com/janestreet/ppx_variants_conv")
    (synopsis "Generation of accessor and iteration functions for OCaml variant types")
    (description
      "This package generates accessors and interation functions for OCaml
variant types.")
    (license license:asl2.0)))

(define-public ocaml-ppx-custom-printf
  (package
    (name "ocaml-ppx-custom-printf")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_custom_printf-v" version ".tar.gz"))
              (sha256
               (base32
                "11b73smf3g3bpd9lg014pr4rx285nk9mnk6g6464ph51jv0sqzhj"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_custom_printf")))
    (home-page "https://github.com/janestreet/ppx_custom_printf")
    (synopsis "Printf-style format-strings for user-defined string conversion")
    (description "Extensions to printf-style format-strings for user-defined
string conversion.")
    (license license:asl2.0)))

(define-public ocaml-bin-prot
  (package
    (name "ocaml-bin-prot")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/bin_prot-v" version ".tar.gz"))
              (sha256
               (base32
                "1rsd91gx36prj4whi76nsiz1bzpgal9nzyw3pxdz1alv4ilk2il6"))))
    (build-system dune-build-system)
    (inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-compare" ,ocaml-ppx-compare)
        ("ocaml-ppx-custom-printf" ,ocaml-ppx-custom-printf)
        ("ocaml-ppx-fields-conv" ,ocaml-ppx-fields-conv)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-ppx-variants-conv" ,ocaml-ppx-variants-conv)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)))
    (properties `((upstream-name . "bin_prot")))
    (home-page "https://github.com/janestreet/bin_prot")
    (synopsis "Binary protocol generator")
    (description "This library contains functionality for reading and writing
OCaml-values in a type-safe binary protocol.  It is extremely efficient,
typically supporting type-safe marshalling and unmarshalling of even highly
structured values at speeds sufficient to saturate a gigabit connection.  The
protocol is also heavily optimized for size, making it ideal for long-term
storage of large amounts of data.")
    (license (list
               license:asl2.0
               license:bsd-3))))

(define-public ocaml-octavius
  (package
    (name "ocaml-octavius")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml-doc/octavius")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gqfbwsh0zq4b9mvvlmg3zhpbfbvq8swqr31320aibbqydwf77hr"))))
    (build-system dune-build-system)
    (properties `((upstream-name . "octavius")))
    (home-page "https://github.com/ocaml-doc/octavius")
    (synopsis "Ocamldoc comment syntax parser")
    (description "Octavius is a library to parse the `ocamldoc` comment syntax.")
    (license license:isc)))

(define-public ocaml-ppx-hash
  (package
    (name "ocaml-ppx-hash")
    (version "0.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/ppx_hash.git")
                     (commit (string-append "v" version))))
              (sha256
               (base32
                "1p0ic6aijxlrdggpmycj12q3cy9xksbq2vq727215maz4snvlf5p"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-compare" ,ocaml-ppx-compare)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_hash")))
    (home-page "https://github.com/janestreet/ppx_hash")
    (synopsis "Generation of hash functions from type expressions and definitions")
    (description "This package is a collecton of ppx rewriters that generate
hash functions from type exrpessions and definitions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-enumerate
  (package
    (name "ocaml-ppx-enumerate")
    (version "0.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/ppx_enumerate.git")
                     (commit (string-append "v" version))))
              (sha256
               (base32
                "0spx9k1v7vjjb6sigbfs69yndgq76v114jhxvzjmffw7q989cyhr"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_enumerate")))
    (home-page "https://github.com/janestreet/ppx_enumerate")
    (synopsis "Generate a list containing all values of a finite type")
    (description "Ppx_enumerate is a ppx rewriter which generates a definition
for the list of all values of a type (for a type which only has finitely
many values).")
    (license license:asl2.0)))

(define-public ocaml-ppx-bench
  (package
    (name "ocaml-ppx-bench")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_bench-v" version ".tar.gz"))
              (sha256
               (base32
                "0ys4pblbcjbk9dn073rqiwm7r6rc7fah03j7riklkwnb5n44andl"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-ppx-inline-test" ,ocaml-ppx-inline-test)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_bench")))
    (home-page "https://github.com/janestreet/ppx_bench")
    (synopsis "Syntax extension for writing in-line benchmarks in ocaml code")
    (description "Syntax extension for writing in-line benchmarks in ocaml code.")
    (license license:asl2.0)))

(define-public ocaml-ppx-here
  (package
    (name "ocaml-ppx-here")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_here-v" version ".tar.gz"))
              (sha256
               (base32
                "0wxcak3ay4jpigm3pfdcpr65qw4hxfa8whhkryhcd8gy71x056z5"))))
    (build-system dune-build-system)
    (arguments
     ;; broken tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_here")))
    (home-page "https://github.com/janestreet/ppx_here")
    (synopsis "Expands [%here] into its location")
    (description
      "Part of the Jane Street's PPX rewriters collection.")
    (license license:asl2.0)))

(define-public ocaml-typerep
  (package
    (name "ocaml-typerep")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/typerep-v" version ".tar.gz"))
              (sha256
               (base32
                "1zi7hy0prpgzqhr4lkacr04wvlvbp21jfbdfvffhrm6cd400rb5v"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs `(("ocaml-base" ,ocaml-base)))
    (home-page "https://github.com/janestreet/typerep")
    (synopsis "Typerep is a library for runtime types")
    (description "Typerep is a library for runtime types.")
    (license license:asl2.0)))

(define-public ocaml-ppx-sexp-value
  (package
    (name "ocaml-ppx-sexp-value")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_sexp_value-v" version ".tar.gz"))
              (sha256
               (base32
                "1xnalfrln6k5khsyxvxkg6v32q8fpr4cqamsjqfih29jdv486xrs"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_sexp_value")))
    (home-page "https://github.com/janestreet/ppx_sexp_value")
    (synopsis "Simplify building s-expressions from ocaml values")
    (description "A ppx rewriter that simplifies building s-expressions from
ocaml values.")
    (license license:asl2.0)))

(define-public ocaml-ppx-sexp-message
  (package
    (name "ocaml-ppx-sexp-message")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_sexp_message-v" version ".tar.gz"))
              (sha256
               (base32
                "1yh440za0w9cvrbxbmqacir8715kdaw6sw24ys9xj80av9nqpiw7"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_sexp_message")))
    (home-page "https://github.com/janestreet/ppx_sexp_message")
    (synopsis "A ppx rewriter for easy construction of s-expressions")
    (description "Ppx_sexp_message aims to ease the creation of s-expressions
in OCaml.  This is mainly motivated by writing error and debugging messages,
where one needs to construct a s-expression based on various element of the
context such as function arguments.")
    (license license:asl2.0)))

(define-public ocaml-ppx-pipebang
  (package
    (name "ocaml-ppx-pipebang")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_pipebang-v" version ".tar.gz"))
              (sha256
               (base32
                "1wrrzlb4kdvkkcmzi01fw25jar38r2jlnyn0i6pn4z0lq4gpm9m0"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_pipebang")))
    (home-page "https://github.com/janestreet/ppx_pipebang")
    (synopsis "Inline reverse application operators `|>` and `|!`")
    (description "A ppx rewriter that inlines reverse application operators
@code{|>} and @code{|!}.")
    (license license:asl2.0)))

(define-public ocaml-ppx-optional
  (package
    (name "ocaml-ppx-optional")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_optional-v" version ".tar.gz"))
              (sha256
               (base32
                "1z8z2bga95k2vksljljfglg10vygkjd24kn1b37sk4z3nmp47x0h"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_optional")))
    (home-page "https://github.com/janestreet/ppx_optional")
    (synopsis "Pattern matching on flat options")
    (description
      "A ppx rewriter that rewrites simple match statements with an if then
else expression.")
    (license license:asl2.0)))

(define-public ocaml-ppx-optcomp
  (package
    (name "ocaml-ppx-optcomp")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_optcomp-v" version ".tar.gz"))
              (sha256
               (base32
                "1bb52p2j2h4s9f06vrcpla80rj93jinnzq6jzilapyx9q068929i"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-stdio" ,ocaml-stdio)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_optcomp")))
    (home-page "https://github.com/janestreet/ppx_optcomp")
    (synopsis "Optional compilation for OCaml")
    (description "Ppx_optcomp stands for Optional Compilation.  It is a tool
used to handle optional compilations of pieces of code depending of the word
size, the version of the compiler, ...")
    (license license:asl2.0)))

(define-public ocaml-ppx-let
  (package
    (name "ocaml-ppx-let")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_let-v" version ".tar.gz"))
              (sha256
               (base32
                "1wdfw6w4xbg97a35yg6bif9gggxniy9ddnrjfw1a0inkl2yamxkj"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_let")))
    (home-page "https://github.com/janestreet/ppx_let")
    (synopsis "Monadic let-bindings")
    (description "A ppx rewriter for monadic and applicative let bindings,
match expressions, and if expressions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-fail
  (package
    (name "ocaml-ppx-fail")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_fail-v" version ".tar.gz"))
              (sha256
               (base32
                "07plqsvljiwvngggfypwq55g46s5my55y45mvlmalrxyppzr03s8"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_fail")))
    (home-page "https://github.com/janestreet/ppx_fail")
    (synopsis "Add location to calls to failwiths")
    (description "Syntax extension that makes [failwiths] always include a
position.")
    (license license:asl2.0)))

(define-public ocaml-ppx-assert
  (package
    (name "ocaml-ppx-assert")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_assert-v" version ".tar.gz"))
              (sha256
               (base32
                "17kd311n0l9f72gblf9kv8i5rghr106w37x4f0m5qwh6nlgl0j9k"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-compare" ,ocaml-ppx-compare)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_assert")))
    (home-page "https://github.com/janestreet/ppx_assert")
    (synopsis "Assert-like extension nodes that raise useful errors on failure")
    (description "This package contains assert-like extension nodes that raise
useful errors on failure.")
    (license license:asl2.0)))

(define-public ocaml-ppx-expect
  (package
    (name "ocaml-ppx-expect")
    (version "0.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/ppx_expect.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bnz3cpj3vwdw409r6f8raakl8n364q5l7wy4i6gckr34a4vla69"))))
    (build-system dune-build-system)
    (arguments
     `(#:jbuild? #t))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-assert" ,ocaml-ppx-assert)
        ("ocaml-ppx-compare" ,ocaml-ppx-compare)
        ("ocaml-ppx-custom-printf" ,ocaml-ppx-custom-printf)
        ("ocaml-ppx-fields-conv" ,ocaml-ppx-fields-conv)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppx-inline-test" ,ocaml-ppx-inline-test)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-ppx-variants-conv" ,ocaml-ppx-variants-conv)
        ("ocaml-stdio" ,ocaml-stdio)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)
        ("ocaml-re" ,ocaml-re)))
    (properties `((upstream-name . "ppx_expect")))
    (home-page "https://github.com/janestreet/ppx_expect")
    (synopsis "Cram like framework for OCaml")
    (description "Expect-test is a framework for writing tests in OCaml, similar
to Cram.  Expect-tests mimics the existing inline tests framework with the
@code{let%expect_test} construct.  The body of an expect-test can contain
output-generating code, interleaved with @code{%expect} extension expressions
to denote the expected output.")
    (license license:asl2.0)))

(define-public ocaml-ppx-js-style
  (package
    (name "ocaml-ppx-js-style")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_js_style-v" version ".tar.gz"))
              (sha256
               (base32
                "0z3fc55jdjhhsblla6z4fqc13kljpcz29q79rvs5h2vsraqrldr2"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-octavius" ,ocaml-octavius)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_js_style")))
    (home-page "https://github.com/janestreet/ppx_js_style")
    (synopsis "Code style checker for Jane Street Packages")
    (description "This packages is a no-op ppx rewriter.  It is used as a
@code{lint} tool to enforce some coding conventions across all Jane Street
packages.")
    (license license:asl2.0)))

(define-public ocaml-ppx-typerep-conv
  (package
    (name "ocaml-ppx-typerep-conv")
    (version "0.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/ppx_typerep_conv.git")
                     (commit (string-append "v" version))))
              (sha256
               (base32
                "0a13dpfrrg0rsm8qni1bh7pqcda30l70z8r6yzi5a64bmwk7g5ah"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-typerep" ,ocaml-typerep)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_typerep_conv")))
    (home-page "https://github.com/janestreet/ppx_typerep_conv")
    (synopsis "Generation of runtime types from type declarations")
    (description "This package can automatically generate runtime types
from type definitions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-base
  (package
    (name "ocaml-ppx-base")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_base-v" version ".tar.gz"))
              (sha256
               (base32
                "0aq206pg330jmj7lhcagiiwm3a0b3gsqm801m8ajd4ysyw7idkym"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
      `(("ocaml-ppx-compare" ,ocaml-ppx-compare)
        ("ocaml-ppx-enumerate" ,ocaml-ppx-enumerate)
        ("ocaml-ppx-hash" ,ocaml-ppx-hash)
        ("ocaml-ppx-js-style" ,ocaml-ppx-js-style)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_base")))
    (home-page "https://github.com/janestreet/ppx_base")
    (synopsis "Base set of ppx rewriters")
    (description "Ppx_base is the set of ppx rewriters used for Base.

Note that Base doesn't need ppx to build, it is only used as a
verification tool.")
    (license license:asl2.0)))

(define-public ocaml-ppx-bin-prot
  (package
    (name "ocaml-ppx-bin-prot")
    (version "0.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/ppx_bin_prot.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h60i75bzvhna1axyn662gyrzhh441l79vl142d235i5x31dmnkz"))))
    (build-system dune-build-system)
    (arguments
     ;; Cyclic dependency with ocaml-ppx-jane
     `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-bin-prot" ,ocaml-bin-prot)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_bin_prot")))
    (home-page "https://github.com/janestreet/ppx_bin_prot")
    (synopsis "Generation of bin_prot readers and writers from types")
    (description "Generation of binary serialization and deserialization
functions from type definitions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-jane
  (package
    (name "ocaml-ppx-jane")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/ppx_jane-v" version ".tar.gz"))
              (sha256
               (base32
                "0lgppkw3aixrfnixihrsz2ipafv8fpvkdpy3pw8n0r615gg8x8la"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
      `(("ocaml-ppx-assert" ,ocaml-ppx-assert)
        ("ocaml-ppx-base" ,ocaml-ppx-base)
        ("ocaml-ppx-bench" ,ocaml-ppx-bench)
        ("ocaml-ppx-bin-prot" ,ocaml-ppx-bin-prot)
        ("ocaml-ppx-custom-printf" ,ocaml-ppx-custom-printf)
        ("ocaml-ppx-expect" ,ocaml-ppx-expect)
        ("ocaml-ppx-fail" ,ocaml-ppx-fail)
        ("ocaml-ppx-fields-conv" ,ocaml-ppx-fields-conv)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppx-inline-test" ,ocaml-ppx-inline-test)
        ("ocaml-ppx-let" ,ocaml-ppx-let)
        ("ocaml-ppx-optcomp" ,ocaml-ppx-optcomp)
        ("ocaml-ppx-optional" ,ocaml-ppx-optional)
        ("ocaml-ppx-pipebang" ,ocaml-ppx-pipebang)
        ("ocaml-ppx-sexp-message" ,ocaml-ppx-sexp-message)
        ("ocaml-ppx-sexp-value" ,ocaml-ppx-sexp-value)
        ("ocaml-ppx-typerep-conv" ,ocaml-ppx-typerep-conv)
        ("ocaml-ppx-variants-conv" ,ocaml-ppx-variants-conv)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_jane")))
    (home-page "https://github.com/janestreet/ppx_jane")
    (synopsis "Standard Jane Street ppx rewriters")
    (description "This package installs a ppx-jane executable, which is a ppx
driver including all standard Jane Street ppx rewriters.")
    (license license:asl2.0)))

(define-public ocaml-splittable-random
  (package
    (name "ocaml-splittable-random")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/splittable_random-v" version ".tar.gz"))
              (sha256
               (base32
                "0l1wbd881mymlnpzlq5q53mmdz3g5d7qjhyc7lfaq1x0iaccn5lc"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-jane" ,ocaml-ppx-jane)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)))
    (properties `((upstream-name . "splittable_random")))
    (home-page "https://github.com/janestreet/splittable_random")
    (synopsis "PRNG that can be split into independent streams")
    (description "This package provides a splittable
@acronym{PRNG,pseudo-random number generator} functions like a PRNG that can
be used as a stream of random values; it can also be split to produce a
second, independent stream of random values.

This library implements a splittable pseudo-random number generator that sacrifices
cryptographic-quality randomness in favor of performance.")
    (license license:asl2.0)))

(define-public ocaml-jane-street-headers
  (package
    (name "ocaml-jane-street-headers")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/jane-street-headers-v" version ".tar.gz"))
              (sha256
               (base32
                "0afhzm08l9v883fhpqqh2lmy7az609pxif40bp7x1sk8c0yszqsh"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (home-page "https://github.com/janestreet/jane-street-headers")
    (synopsis "Jane Street C header files")
    (description "This package provides C header files shared between the
various Jane Street packages.")
    (license license:asl2.0)))

(define-public ocaml-configurator
  (package
    (name "ocaml-configurator")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                                  (version-major+minor version)
                                  "/files/configurator-v" version ".tar.gz"))
              (sha256
               (base32
                "0kwgi3sh92v4n242dk5hgpwd85zzgnczgbkqi0q0kr6m93zgbf7p"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-stdio" ,ocaml-stdio)))
    (home-page "https://github.com/janestreet/configurator")
    (synopsis "Helper library for gathering system configuration")
    (description "Configurator is a small library that helps writing OCaml
scripts that test features available on the system, in order to generate config.h
files for instance.

Configurator allows one to:
@itemize
@item test if a C program compiles
@item query pkg-config
@item import #define from OCaml header files
@item generate config.h file
@end itemize")
    (license license:asl2.0)))

(define-public ocaml-spawn
  (package
    (name "ocaml-spawn")
    (version "0.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/spawn.git")
                     (commit (string-append "v" version))))
              (sha256
               (base32
                "0amgj7g9sjlbjivn1mg7yjdmxd21hgp4a0ak2zrm95dmm4gi846i"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (substitute* "test/tests.ml"
               (("/bin/pwd") (which "pwd"))
               (("/bin/echo") (which "echo")))
             #t)))))
    (native-inputs
     `(("ocaml-ppx-expect" ,ocaml-ppx-expect)))
    (home-page "https://github.com/janestreet/spawn")
    (synopsis "Spawning sub-processes")
    (description
      "Spawn is a small library exposing only one functionality: spawning sub-process.

It has three main goals:

@itemize
@item provide missing features of Unix.create_process such as providing a
working directory,
@item provide better errors when a system call fails in the
sub-process.  For instance if a command is not found, you get a proper
@code{Unix.Unix_error} exception,
@item improve performances by using vfork when available.  It is often
claimed that nowadays fork is as fast as vfork, however in practice
fork takes time proportional to the process memory while vfork is
constant time.  In application using a lot of memory, vfork can be
thousands of times faster than fork.
@end itemize")
    (license license:asl2.0)))

(define-public ocaml-core
  (package
    (name "ocaml-core")
    (version "0.11.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/core.git")
                     (commit (string-append "v" version))))
              (sha256
               (base32
                "0pzl8n09z4f3i7z2wq4cjxfqrr8mj6xcdp7rbg0nxap2zdhjgvrq"))))
    (build-system dune-build-system)
    (arguments
     `(#:jbuild? #t
       ;; Require a cyclic dependency: core_extended
       #:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-configurator" ,ocaml-configurator)
        ("ocaml-core-kernel" ,ocaml-core-kernel)
        ("ocaml-ppx-assert" ,ocaml-ppx-assert)
        ("ocaml-ppx-jane" ,ocaml-ppx-jane)
        ("ocaml-sexplib" ,ocaml-sexplib)
        ("ocaml-spawn" ,ocaml-spawn)
        ("ocaml-stdio" ,ocaml-stdio)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (home-page "https://github.com/janestreet/core")
    (synopsis "Alternative to OCaml's standard library")
    (description "The Core suite of libraries is an alternative to OCaml's
standard library that was developed by Jane Street.")
    ;; Also contains parts of OCaml, relicensed to asl2.0, as permitted
    ;; by OCaml's license for consortium members (see THIRD-PARTY.txt).
    (license license:asl2.0)))

(define-public ocaml-core-kernel
  (package
    (name "ocaml-core-kernel")
    (version "0.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/core_kernel.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dg7ygy7i64c5gaakb1cp1b26p9ks81vbxmb8fd7jff2q60j2z2g"))))
    (build-system dune-build-system)
    (arguments
     ;; Cyclic dependency with ocaml-core
     `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-bin-prot" ,ocaml-bin-prot)
        ("ocaml-configurator" ,ocaml-configurator)
        ("ocaml-fieldslib" ,ocaml-fieldslib)
        ("ocaml-jane-street-headers" ,ocaml-jane-street-headers)
        ("ocaml-ppx-assert" ,ocaml-ppx-assert)
        ("ocaml-ppx-base" ,ocaml-ppx-base)
        ("ocaml-ppx-hash" ,ocaml-ppx-hash)
        ("ocaml-ppx-inline-test" ,ocaml-ppx-inline-test)
        ("ocaml-ppx-jane" ,ocaml-ppx-jane)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-ppx-sexp-message" ,ocaml-ppx-sexp-message)
        ("ocaml-sexplib" ,ocaml-sexplib)
        ("ocaml-splittable-random" ,ocaml-splittable-random)
        ("ocaml-stdio" ,ocaml-stdio)
        ("ocaml-typerep" ,ocaml-typerep)
        ("ocaml-variantslib" ,ocaml-variantslib)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)))
    (properties `((upstream-name . "core_kernel")))
    (home-page "https://github.com/janestreet/core_kernel")
    (synopsis "Portable standard library for OCaml")
    (description "Core is an alternative to the OCaml standard library.

Core_kernel is the system-independent part of Core.  It is aimed for cases when
the full Core is not available, such as in Javascript.")
    (license (list
               ;; this package and parts of OCaml, relicensed by janestreet
               license:asl2.0
               ;; MLton and sjs
               license:expat))))
