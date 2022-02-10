;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2022 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Peter Kreye <kreyepr@gmail.com>
;;; Copyright © 2018, 2019 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 divoplade <d@divoplade.fr>
;;; Copyright © 2020, 2021, 2022 pukkamustard <pukkamustard@posteo.net>
;;; Copyright © 2021 aecepoglu <aecepoglu@fastmail.fm>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
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
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
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
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages node)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system dune)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module (guix download)
  #:use-module (guix gexp)
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

(define (janestreet-origin name version hash)
  (origin (method url-fetch)
          (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                              (version-major+minor version) "/files/"
                              name "-v" (version-major+minor+point version)
                              ".tar.gz"))
          (sha256 (base32 hash))))

(define-public camlboot
  (let ((commit "45045d0afa82f7e9b7ea07314aab08be2d3cd64b")
        (revision "1"))
    (package
      (name "camlboot")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Ekdohibs/camlboot")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1f5gl3hzvixbgk0v3kmxiyn432znyy3jh5fa65cfzcaxzgfv1i1c"))
                (modules '((guix build utils)))
                (snippet
                 `(begin
                    ;; Remove bootstrap binaries and pre-generated source files,
                    ;; to ensure we actually bootstrap properly.
                    (for-each delete-file (find-files "ocaml-src" "^.depend$"))
                    (delete-file "ocaml-src/boot/ocamlc")
                    (delete-file "ocaml-src/boot/ocamllex")
                    ;; Ensure writable
                    (for-each
                     (lambda (file)
                       (chmod file (logior (stat:mode (stat file)) #o200)))
                     (find-files "." "."))))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "_boot/ocamlc") ; build target
         #:tests? #f                        ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'no-autocompile
             (lambda _
               ;; prevent a guile warning
               (setenv "GUILE_AUTO_COMPILE" "0")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (mkdir-p bin)
                 (install-file "_boot/ocamlc" bin)
                 (rename-file "miniml/interp/lex.byte" "ocamllex")
                 (install-file "ocamllex" bin)))))))
      (native-inputs
       (list guile-3.0))
      (properties
       ;; 10 hours, mostly for arm, more than 1 expected even on x86_64
       `((max-silent-time . 36000)))
      (home-page "https://github.com/Ekdohibs/camlboot")
      (synopsis "OCaml source bootstrap")
      (description "OCaml is written in OCaml.  Its sources contain a pre-compiled
bytecode version of @command{ocamlc} and @command{ocamllex} that are used to
build the next version of the compiler.  Camlboot implements a bootstrap for
the OCaml compiler and provides a bootstrapped equivalent to these files.

It contains a compiler for a small subset of OCaml written in Guile Scheme,
an interpreter for OCaml written in that subset and a manually-written lexer
for OCaml.  These elements eliminate the need for the binary bootstrap in
OCaml and can effectively bootstrap OCaml 4.07.

This package produces a native @command{ocamlc} and a bytecode @command{ocamllex}.")
      (license license:expat))))

(define-public ocaml-4.13
  (package
    (name "ocaml")
    (version "4.13.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://caml.inria.fr/pub/distrib/ocaml-"
                    (version-major+minor version)
                    "/ocaml-" version ".tar.xz"))
              (sha256
               (base32
                "1s7xwqidpjwfhnpfma4nb93gxfr7g9jfn03s1j03iyavmpgph7ck"))))
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
     (list perl pkg-config))
    (inputs
     (list libx11 libiberty ;needed for objdump support
           zlib))                       ;also needed for objdump support
    (arguments
     `(#:configure-flags '("--enable-ocamltest")
       #:test-target "tests"
       #:make-flags '("world.opt")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((sh (search-input-file inputs "/bin/sh"))
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
                  (find-files "." "\\.ml$")))))))))
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

(define-public ocaml-4.09
  (package
    (inherit ocaml-4.13)
    (version "4.09.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://caml.inria.fr/pub/distrib/ocaml-"
                    (version-major+minor version)
                    "/ocaml-" version ".tar.xz"))
              (patches (search-patches "ocaml-4.09-multiple-definitions.patch"))
              (sha256
               (base32
                "1v3z5ar326f3hzvpfljg4xj8b9lmbrl53fn57yih1bkbx3gr3yzj"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((sh (search-input-file inputs "/bin/sh"))
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
                  (find-files "." "\\.ml$"))))))
         (replace 'build
           (lambda _
             (invoke "make" "-j" (number->string (parallel-job-count))
                     "world.opt")))
         (replace 'check
           (lambda _
             (with-directory-excursion "testsuite"
               (invoke "make" "all")))))))))

;; This package is a bootstrap package for ocaml-4.07. It builds from camlboot,
;; using the upstream sources for ocaml 4.07. It installs a bytecode ocamllex
;; and ocamlc, the bytecode interpreter ocamlrun, and generated .depend files
;; that we otherwise remove for bootstrap purposes.
(define ocaml-4.07-boot
  (package
    (inherit ocaml-4.09)
    (name "ocaml-boot")
    (version "4.07.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://caml.inria.fr/pub/distrib/ocaml-"
                    (version-major+minor version)
                    "/ocaml-" version ".tar.xz"))
              (sha256
               (base32
                "1f07hgj5k45cylj1q3k5mk8yi02cwzx849b1fwnwia8xlcfqpr6z"))
              (patches (search-patches "ocaml-multiple-definitions.patch"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; Remove bootstrap binaries and pre-generated source files,
                  ;; to ensure we actually bootstrap properly.
                  (for-each delete-file (find-files "." "^.depend$"))
                  (delete-file "boot/ocamlc")
                  (delete-file "boot/ocamllex")))))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'copy-bootstrap
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((camlboot (assoc-ref inputs "camlboot")))
               (copy-file (string-append camlboot "/bin/ocamllex") "boot/ocamllex")
               (copy-file (string-append camlboot "/bin/ocamlc") "boot/ocamlc")
               (chmod "boot/ocamllex" #o755)
               (chmod "boot/ocamlc" #o755))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (mandir (string-append out "/share/man")))
               (invoke "./configure"
                       "--prefix" out
                       "--mandir" mandir))))
         (replace 'build
           (lambda* (#:key parallel-build? #:allow-other-keys)
             (define* (make . args)
               (apply invoke "make"
                      (append (if parallel-build?
                                  `("-j" ,(number->string (parallel-job-count)))
                                  '())
                              args)))
             ;; create empty .depend files because they are included by various
             ;; Makefiles, and they have no rule to generate them.
             (invoke "touch" ".depend" "stdlib/.depend" "byterun/.depend"
                     "tools/.depend"  "lex/.depend" "asmrun/.depend"
                     "debugger/.depend" "ocamltest/.depend" "ocamldoc/.depend"
                     "ocamldoc/stdlib_non_prefixed/.depend"
                     "otherlibs/bigarray/.depend" "otherlibs/graph/.depend"
                     "otherlibs/raw_spacetime_lib/.depend" "otherlibs/str/.depend"
                     "otherlibs/systhreads/.depend" "otherlibs/threads/.depend"
                     "otherlibs/unix/.depend" "otherlibs/win32unix/.depend")
             ;; We cannot build ocamldep until we have created all the .depend
             ;; files, so replace it with ocamlc -depend.
             (substitute* "tools/Makefile"
               (("\\$\\(CAMLRUN\\) ./ocamldep") "../boot/ocamlc -depend"))
             (substitute* '("otherlibs/graph/Makefile"
                            "otherlibs/systhreads/Makefile"
                            "otherlibs/threads/Makefile"
                            "otherlibs/unix/Makefile")
               (("\\$\\(CAMLRUN\\) ../../tools/ocamldep")
                "../../boot/ocamlc -depend"))
             (substitute* '("otherlibs/bigarray/Makefile"
                            "otherlibs/raw_spacetime_lib/Makefile"
                            "otherlibs/str/Makefile"
                            "otherlibs/win32unix/Makefile")
               (("\\$\\(CAMLRUN\\) \\$\\(ROOTDIR\\)/tools/ocamldep")
                "../../boot/ocamlc -depend"))
             ;; Ensure we copy needed file, so we can generate a proper .depend
             (substitute* "ocamldoc/Makefile"
               (("include Makefile.unprefix")
                "include Makefile.unprefix
depend: $(STDLIB_MLIS) $(STDLIB_DEPS)"))
             ;; Generate required tools for `alldepend'
             (make "-C" "byterun" "depend")
             (make "-C" "byterun" "all")
             (copy-file "byterun/ocamlrun" "boot/ocamlrun")
             (make "ocamlyacc")
             (copy-file "yacc/ocamlyacc" "boot/ocamlyacc")
             (make "-C" "stdlib" "sys.ml")
             (make "-C" "stdlib" "CAMLDEP=../boot/ocamlc -depend" "depend")
             ;; Build and copy files later used by `tools'
             (make "-C" "stdlib" "COMPILER="
                   "CAMLC=../boot/ocamlc -use-prims ../byterun/primitives"
                   "all")
             (for-each
              (lambda (file)
                (copy-file file (string-append "boot/" (basename file))))
              (cons* "stdlib/stdlib.cma" "stdlib/std_exit.cmo" "stdlib/camlheader"
                     (find-files "stdlib" ".*.cmi$")))
             (symlink "../byterun/libcamlrun.a" "boot/libcamlrun.a")
             ;; required for ocamldoc/stdlib_non_prefixed
             (make "parsing/parser.mli")
             ;; required for dependencies
             (make "-C" "tools"
                   "CAMLC=../boot/ocamlc -nostdlib -I ../boot -use-prims ../byterun/primitives -I .."
                   "make_opcodes" "cvt_emit")
             ;; generate all remaining .depend files
             (make "alldepend"
                   (string-append "ocamllex=" (getcwd) "/boot/ocamlrun "
                                  (getcwd) "/boot/ocamllex")
                   (string-append "CAMLDEP=" (getcwd) "/boot/ocamlc -depend")
                   (string-append "OCAMLDEP=" (getcwd) "/boot/ocamlc -depend")
                   (string-append "ocamldep=" (getcwd) "/boot/ocamlc -depend"))
             ;; Build ocamllex
             (make "CAMLC=boot/ocamlc -nostdlib -I boot -use-prims byterun/primitives"
                   "ocamlc")
             ;; Build ocamlc
             (make "-C" "lex"
                   "CAMLC=../boot/ocamlc -strict-sequence -nostdlib -I ../boot -use-prims ../byterun/primitives"
                   "all")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (depends (string-append out "/share/depends")))
               (mkdir-p bin)
               (mkdir-p depends)
               (install-file "ocamlc" bin)
               (install-file "lex/ocamllex" bin)
               (for-each
                (lambda (file)
                  (let ((dir (string-append depends "/" (dirname file))))
                    (mkdir-p dir)
                    (install-file file dir)))
                (find-files "." "^\\.depend$"))))))))
    (native-inputs
     `(("camlboot" ,camlboot)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))))

(define-public ocaml-4.07
  (package
    (inherit ocaml-4.07-boot)
    (name "ocaml")
    (arguments
      (substitute-keyword-arguments (package-arguments ocaml-4.09)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'configure 'copy-bootstrap
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((ocaml (assoc-ref inputs "ocaml")))
                  (copy-file (string-append ocaml "/bin/ocamllex") "boot/ocamllex")
                  (copy-file (string-append ocaml "/bin/ocamlc") "boot/ocamlc")
                  (chmod "boot/ocamllex" #o755)
                  (chmod "boot/ocamlc" #o755)
                  (let ((rootdir (getcwd)))
                    (with-directory-excursion (string-append ocaml "/share/depends")
                      (for-each
                        (lambda (file)
                          (copy-file file (string-append rootdir "/" file)))
                        (find-files "." ".")))))))
            (replace 'configure
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (mandir (string-append out "/share/man")))
                  ;; Custom configure script doesn't recognize
                  ;; --prefix=<PREFIX> syntax (with equals sign).
                  (invoke "./configure"
                          "--prefix" out
                          "--mandir" mandir))))))))
    (native-inputs
     `(("ocaml" ,ocaml-4.07-boot)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))))

(define-public ocaml ocaml-4.13)

(define-public ocamlbuild
  (package
    (name "ocamlbuild")
    (version "0.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/ocamlbuild")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hb5mcdz4wv7sh1pj7dq9q4fgz5h3zg7frpiya6s8zd3ypwzq0kh"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:make-flags
       ,#~(list (string-append "OCAMLBUILD_PREFIX=" #$output)
                (string-append "OCAMLBUILD_BINDIR=" #$output "/bin")
                (string-append "OCAMLBUILD_LIBDIR=" #$output
                               "/lib/ocaml/site-lib")
                (string-append "OCAMLBUILD_MANDIR=" #$output "/share/man"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       ; some failures because of changes in OCaml's error message formating
       #:tests? #f))
    (home-page "https://github.com/ocaml/ocamlbuild")
    (synopsis "OCaml build tool")
    (description "OCamlbuild is a generic build tool, that has built-in rules
for building OCaml library and programs.")
    (license license:lgpl2.1+)))

(define-public camlidl
  (package
    (name "camlidl")
    (version "1.09")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xavierleroy/camlidl")
             (commit "camlidl109")))
       (sha256
        (base32 "0zrkaq7fk23b2b9vg6jwdjx7l0hdqp4synbbrw1zcg8gjf6n3c80"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; No test suite
       #:make-flags
       (list
        (string-append
         "BINDIR=" (assoc-ref %outputs "out") "/bin")
        (string-append
         "OCAMLLIB=" (assoc-ref %outputs "out") "/lib/ocaml/site-lib/camlidl"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (copy-file "config/Makefile.unix" "config/Makefile")
             ;; Note: do not pass '-jN' as this appears to not be
             ;; parallel-safe (race condition related to libcamlidl.a).
             (invoke "make" "all")
             #t))
         (add-before 'install 'create-target-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out"))))
               (mkdir-p
                (string-append out "/bin"))
               (mkdir-p
                (string-append out "/lib/ocaml/site-lib/camlidl/stublibs"))
               (mkdir-p
                (string-append out "/lib/ocaml/site-lib/camlidl/caml")))
             #t))
         (add-after 'install 'install-meta
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-output-to-file
                   (string-append out "/lib/ocaml/site-lib/camlidl/META")
                 (lambda _
                   (display
                    (string-append
                     "description = \"Stub code generator for OCaml/C interface\"
version = \"" ,version "\"
directory = \"^\"
archive(byte) = \"com.cma\"
archive(native) = \"com.cmxa\"")))))
             #t)))))
    (native-inputs
     (list ocaml))
    (home-page "https://github.com/xavierleroy/camlidl")
    (synopsis "Stub code generator for OCaml/C interface")
    (description
     "Camlidl is a stub code generator for Objective Caml.  It generates stub
code for interfacing Caml with C from an IDL description of the C functions.")
    (license license:lgpl2.1)))

(define-public ocaml-extlib
  (package
    (name "ocaml-extlib")
    (version "1.7.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ygrek.org/p/release/ocaml-extlib/"
                                  "extlib-" version ".tar.gz"))
              (sha256
               (base32
                "0npq4hq3zym8nmlyji7l5cqk6drx2rkcx73d60rxqh5g8dla8p4k"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
      (list ocaml-cppo))
    (home-page "https://github.com/ygrek/ocaml-extlib")
    (synopsis "Complete and small extension for OCaml standard library")
    (description "This library adds new functions to OCaml standard library
modules, modifies some functions in order to get better performances or
safety (tail-recursive) and also provides new modules which should be useful
for day to day programming.")
    ;; With static-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-cudf
  (package
    (name "ocaml-cudf")
    (version "0.9")
    (source
      (origin
        (method url-fetch)
        (uri "https://gforge.inria.fr/frs/download.php/36602/cudf-0.9.tar.gz")
        (sha256
          (base32
            "0771lwljqwwn3cryl0plny5a5dyyrj4z6bw66ha5n8yfbpcy8clr"))))
    (build-system ocaml-build-system)
    (propagated-inputs (list ocaml-extlib))
    (native-inputs
      (list perl ocamlbuild ocaml-ounit))
    (arguments
     `(#:make-flags
       ,#~(list
           "all" "opt"
           (string-append "BINDIR=" #$output "/bin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://www.mancoosi.org/cudf/")
    (synopsis "CUDF library (part of the Mancoosi tools)")
    (description "CUDF (for Common Upgradeability Description Format) is a
format for describing upgrade scenarios in package-based Free and Open Source
Software distribution.")
    ;; With static-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-mccs
  (package
    (name "ocaml-mccs")
    (version "1.1+13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/AltGr/ocaml-mccs")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15slv97jlvbaxj221wp4sjxk4lbdfyj9vcig0jwd4n54v89ksvib"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-cudf))
    (home-page "https://www.i3s.unice.fr/~cpjm/misc/")
    (synopsis "Upgrade path problem solver")
    (description "Mccs (Multi Criteria CUDF Solver) is a CUDF problem solver.
Mccs take as input a CUDF problem and computes the best solution according to
a set of criteria.  It relies on a Integer Programming solver or a
Pseudo Boolean solver to achieve its task.  Mccs can use a wide set of
underlying solvers like Cplex, Gurobi, Lpsolver, Glpk, CbC, SCIP or WBO.")
    (license (list
               license:bsd-3
               license:gpl3+
               ;; With static-linking exception
               license:lgpl2.1+))))

(define-public ocaml-dose3
  (package
    (name "ocaml-dose3")
    (version "5.0.1")
    (source (origin
              (method url-fetch)
              (uri "https://gforge.inria.fr/frs/download.php/file/36063/dose3-5.0.1.tar.gz")
              (sha256
               (base32
                "00yvyfm4j423zqndvgc1ycnmiffaa2l9ab40cyg23pf51qmzk2jm"))
              (patches
               (search-patches
                "ocaml-dose3-add-unix-dependency.patch"
                "ocaml-dose3-Fix-for-ocaml-4.06.patch"
                "ocaml-dose3-dont-make-printconf.patch"
                "ocaml-dose3-Install-mli-cmx-etc.patch"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:configure-flags
       ,#~(list (string-append "SHELL="
                               #+(file-append (canonical-package bash-minimal)
                                              "/bin/sh")))
       #:make-flags
       ,#~(list (string-append "LIBDIR=" #$output "/lib/ocaml/site-lib"))))
    (propagated-inputs
      (list ocaml-graph ocaml-cudf ocaml-extlib ocaml-re))
    (native-inputs
      `(("perl" ,perl)
        ("python" ,python-2) ; for a test script
        ("python2-pyyaml" ,python2-pyyaml) ; for a test script
        ("ocaml-extlib" ,ocaml-extlib)
        ("ocamlbuild" ,ocamlbuild)
        ("ocaml-cppo" ,ocaml-cppo)))
    (home-page "https://www.mancoosi.org/software/")
    (synopsis "Package distribution management framework")
    (description "Dose3 is a framework made of several OCaml libraries for
managing distribution packages and their dependencies.  Though not tied to
any particular distribution, dose3 constitutes a pool of libraries which
enable analyzing packages coming from various distributions.  Besides basic
functionalities for querying and setting package properties, dose3 also
implements algorithms for solving more complex problems such as monitoring
package evolutions, correct and complete dependency resolution and
repository-wide uninstallability checks.")
    ;; with static-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-down
  (package
    (name "ocaml-down")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://erratique.ch/software/down/releases/down-"
                            version ".tbz"))
        (sha256
         (base32
          "1nz2f5j17frgr2vrslcz9klmi6w9sm2vqwwwpi33ngcm3rgmsrlg"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:build-flags
       ,#~(list "build" "--lib-dir"
                (string-append #$output "/lib/ocaml/site-lib"))))
    (native-inputs
     (list ocaml-findlib ocamlbuild ocaml-topkg opam))
    (home-page "https://erratique.ch/software/down")
    (synopsis "OCaml toplevel (REPL) upgrade")
    (description "Down is an unintrusive user experience upgrade for the
@command{ocaml} toplevel (REPL).

Simply load the zero dependency @code{down} library in the @command{ocaml}
toplevel and you get line edition, history, session support and identifier
completion and documentation with @command{ocp-index}.

Add this to your @file{~/.ocamlinit}:

@example
#use \"down.top\"
@end example

You may also need to add this to your @file{~/.ocamlinit} and declare
the environment variable @code{OCAML_TOPLEVEL_PATH}:

@example
let () =
  try Topdirs.dir_directory (Sys.getenv \"OCAML_TOPLEVEL_PATH\")
  with Not_found -> ()
@end example

OR

@example
let () = String.split_on_char ':' (Sys.getenv \"OCAMLPATH\")
         |> List.filter (fun x -> Filename.check_suffix x \"/site-lib\")
         |> List.map (fun x -> x ^ \"/toplevel\")
         (* remove the line below if you don't want to see the text
            every time you start the toplevel *)
         |> List.map (fun x -> Printf.printf \"adding directory %s\\n\" x; x)
         |> List.iter Topdirs.dir_directory;;
@end example")
    (license license:isc)))

(define-public ocaml-opam-file-format
  (package
    (name "ocaml-opam-file-format")
    (version "2.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/opam-file-format")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fxhppdmrysr2nb5z3c448h17np48f3ga9jih33acj78r4rdblcs"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; No tests
       #:make-flags ,#~(list (string-append "LIBDIR=" #$output
                                            "/lib/ocaml/site-lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://opam.ocaml.org")
    (synopsis "Parser and printer for the opam file syntax")
    (description "This package contains a parser and a pretty-printer for
the opam file format.")
    ;; With static-linking exception
    (license license:lgpl2.1+)))

(define-public opam
  (package
    (name "opam")
    (version "2.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/opam")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mdr32mg63yaw89p44zx8b9dxp1167ckmlxkp8svd6fwgb3z49yx"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
           (lambda* (#:key inputs make-flags #:allow-other-keys)
             (let ((bash (assoc-ref inputs "bash"))
                   (bwrap (search-input-file inputs "/bin/bwrap")))
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
                 (("^add_sys_mounts /usr")
                  (string-append "add_sys_mounts "
                                 (%store-directory)
                                 " /run/current-system /usr")))
               (substitute* "src/client/opamInitDefaults.ml"
                 (("\"bwrap\"") (string-append "\"" bwrap "\""))))))
         (add-before 'check 'prepare-checks
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Opam tests need to run an isolated environment from a writable
             ;; home directory.
             (mkdir-p "test-home")
             (setenv "HOME" (string-append (getcwd) "/test-home"))

             ;; Opam tests require data from opam-repository. Instead of
             ;; downloading them with wget from the guix environment, copy the
             ;; content to the expected directory.
             (substitute* "tests/reftests/dune.inc"
               (("tar -C.*opam-archive-([0-9a-f]*)[^)]*" _ commit)
                (string-append "rmdir %{targets}) (run cp -r "
                               (assoc-ref inputs (string-append "opam-repo-" commit))
                               "/ %{targets}) (run chmod +w -R %{targets}"))
               (("wget[^)]*") "touch %{targets}")
               ;; Disable a failing test because of different line wrapping
               (("diff cli-versioning.test cli-versioning.out") "run true")
               ;; Disable a failing test because it tries to clone a git
               ;; repository from inside bwrap
               (("diff upgrade-format.test upgrade-format.out") "run true"))
             (substitute* "tests/reftests/dune"
               ;; Because of our changes to the previous file, we cannot check
               ;; it can be regenerated
               (("diff dune.inc dune.inc.gen") "run true"))
             ;; Ensure we can run the generated build.sh (no /bin/sh)
             (substitute* '("tests/reftests/legacy-local.test"
                            "tests/reftests/legacy-git.test")
               (("#! ?/bin/sh")
                (string-append "#!"
                               (search-input-file inputs "/bin/sh"))))
             (substitute* "tests/reftests/testing-env"
               (("OPAMSTRICT=1")
                (string-append "OPAMSTRICT=1\nLIBRARY_PATH="
                               (assoc-ref inputs "libc") "/lib"))))))))
    (native-inputs
      (let ((opam-repo (lambda (commit hash)
                         (origin
                           (method git-fetch)
                           (uri (git-reference
                                  (url "https://github.com/ocaml/opam-repository")
                                  (commit commit)))
                           (file-name (git-file-name "opam-repo" commit))
                           (sha256 (base32 hash))))))
       `(("dune" ,dune)
         ("ocaml-cppo" ,ocaml-cppo)

         ;; For tests.
         ("git" ,git-minimal)
         ("openssl" ,openssl)
         ("python" ,python-wrapper)
         ("rsync" ,rsync)
         ("unzip" ,unzip)
         ("which" ,which)

         ;; Data for tests
         ("opam-repo-009e00fa" ,(opam-repo "009e00fa86300d11c311309a2544e5c6c3eb8de2"
                                           "1wwy0rwrsjf4q10j1rh1dazk32fbzhzy6f7zl6qmndidx9b1bq7w"))
         ("opam-repo-ad4dd344" ,(opam-repo "ad4dd344fe5cd1cab49ced49d6758a9844549fb4"
                                           "1a1qj47kj8xjdnc4zc50ijrix1kym1n7k20n3viki80a7518baw8"))
         ("opam-repo-c1d23f0e" ,(opam-repo "c1d23f0e17ec83a036ebfbad1c78311b898a2ca0"
                                           "0j9abisx3ifzm66ci3p45mngmz4f0fx7yd9jjxrz3f8w5jffc9ii"))
         ("opam-repo-f372039d" ,(opam-repo "f372039db86a970ef3e662adbfe0d4f5cd980701"
                                           "0ld7fcry6ss6fmrpswvr6bikgx299w97h0gwrjjh7kd7rydsjdws")))))
    (inputs
     (list ocaml ncurses curl bubblewrap))
    (propagated-inputs
     (list ocaml-cmdliner ocaml-dose3 ocaml-mccs ocaml-opam-file-format
           ocaml-re))
    (home-page "http://opam.ocamlpro.com/")
    (synopsis "Package manager for OCaml")
    (description
     "OPAM is a tool to manage OCaml packages.  It supports multiple
simultaneous compiler installations, flexible package constraints, and a
Git-friendly development workflow.")

    ;; The 'LICENSE' file waives some requirements compared to LGPLv3.
    (license license:lgpl3)))

(define-public camlp5
  (package
    (name "camlp5")
    (version "8.00.02")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/camlp5/camlp5")
             (commit (string-append "rel" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03qm99h2380x0y69sppg10yz1mwx7smkscia7pa175wmilifx8vy"))))
    (build-system gnu-build-system)
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
    (inputs
     (list ocaml))
    (native-inputs
     (list perl))
    (home-page "https://camlp5.github.io/")
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
    (version "2.35")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://hevea.inria.fr/old/"
                                  "hevea-" version ".tar.gz"))
              (sha256
               (base32
                "1jwydkb9ldb1sx815c364dxgr569f2rbbzgxbn2kanrybpdbm2gi"))))
    (build-system gnu-build-system)
    (inputs
     (list ocaml))
    (native-inputs
     (list ocamlbuild))
    (arguments
     `(#:tests? #f                      ; no test suite
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
    (version "1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/num")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vzdnvpj5dbj3ifx03v25pj2jj1ccav072v4d29pk1czdba2lzfc"))))
    (build-system dune-build-system)
    (home-page "https://github.com/ocaml/num")
    (synopsis "Arbitrary-precision integer and rational arithmetic")
    (description "OCaml-Num contains the legacy Num library for
arbitrary-precision integer and rational arithmetic that used to be part of
the OCaml core distribution.")
    (license license:lgpl2.1+))); with linking exception

(define-public emacs-tuareg
  ;; Last upstream release on Sept., 14th, 2018, since then "Package cl
  ;; deprecated" or 'lexical-binding' and others had been fixed.
  (let ((commit "ccde45bbc292123ec20617f1af7f7e19f7481545")
        (revision "0"))
    (package
      (name "emacs-tuareg")
      (version (git-version "2.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ocaml/tuareg")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1yxv4bnqarilnpg5j7wywall8170hwvm0q4xx06yqjgcn8pq1lac"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("emacs" ,emacs-minimal)
         ("opam" ,opam)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'make-git-checkout-writable
             (lambda _
               (for-each make-file-writable (find-files "."))
               #t))
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
      (license license:gpl2+))))

(define-public ocaml-menhir
  (package
    (name "ocaml-menhir")
    (version "20211230")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/fpottier/menhir.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "177gfp23gxc84j5pv2mqkza1ggg6rskvfwwjg229ba1457inayzs"))))
    (build-system dune-build-system)
    (inputs
     (list ocaml))
    (arguments
     `(#:tests? #f)) ; No check target
    (properties `((ocaml4.07-variant . ,(delay (strip-ocaml4.07-variant ocaml-menhir)))))
    (home-page "http://gallium.inria.fr/~fpottier/menhir/")
    (synopsis "Parser generator")
    (description "Menhir is a parser generator.  It turns high-level grammar
specifications, decorated with semantic actions expressed in the OCaml
programming language into parsers, again expressed in OCaml.  It is based on
Knuth’s LR(1) parser construction technique.")
    ;; The file src/standard.mly and all files listed in src/mnehirLib.mlpack
    ;; that have an *.ml or *.mli extension are GPL licensed. All other files
    ;; are QPL licensed.
    (license (list license:gpl2+ license:qpl))))

(define-public ocaml-bigarray-compat
  (package
    (name "ocaml-bigarray-compat")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mirage/bigarray-compat")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hif5baiwswdblymyfbxh9066pfqynlz5vj3b2brpn0a12k6i5fq"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (home-page "https://github.com/mirage/bigarray-compat")
    (synopsis "OCaml compatibility library")
    (description "This package contains a compatibility library for
@code{Stdlib.Bigarray} in OCaml.")
    (license license:isc)))

(define-public lablgtk
  (package
    (name "lablgtk")
    (version "2.18.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/garrigue/lablgtk")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "179ipx0c6bpxm4gz0syxgqy09dp5p4x9qsdil7s9jlx8ffg1mm0w"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("findlib" ,ocaml-findlib)
       ("pkg-config" ,pkg-config)))
    ;; FIXME: Add inputs gtkgl-2.0, libpanelapplet-2.0, gtkspell-2.0,
    ;; and gtk+-quartz-2.0 once available.
    (inputs
     (list gtk+-2
           gtksourceview-2
           libgnomecanvas
           libgnomeui
           libglade
           librsvg))
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
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-lablgtk))))
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

(define-public ocaml4.07-lablgtk
  (package
    (inherit lablgtk)
    (name "ocaml4.07-lablgtk")
    (native-inputs
     `(("ocaml" ,ocaml-4.07)
       ("findlib" ,ocaml4.07-findlib)
       ("pkg-config" ,pkg-config)))
    (properties '())))

(define-public unison
  (package
    (name "unison")
    (version "2.51.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bcpierce00/unison")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bykiyc0dc5pkw8x370qkg2kygq9pq7yqzsgczd3y13b6ivm4sdq"))
              (patches (search-patches "unison-fix-ocaml-4.08.patch"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                  ; 1.9 MiB of documentation
    (native-inputs
     `(("ocaml" ,ocaml-4.09)
       ;; For documentation
       ("ghostscript" ,ghostscript)
       ("texlive" ,(texlive-updmap.cfg
                    (list texlive-fonts-ec texlive-dvips-l3backend)))
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
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.camlcity.org/download/"
                                  "findlib" "-" version ".tar.gz"))
              (sha256
               (base32
                "0hfcwamcvinmww59b5i4yxbf0kxyzkp5qv3d1c7ybn9q52vgq463"))))
    (build-system gnu-build-system)
    (native-inputs
     (list m4 ocaml))
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
                                               out "/lib/ocaml/site-lib"))))))))
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

(define-public ocaml4.07-findlib
  (package
    (inherit ocaml-findlib)
    (name "ocaml4.07-findlib")
    (native-inputs
     (list m4 ocaml-4.07))))

(define-public ocaml4.09-findlib
  (package
    (inherit ocaml-findlib)
    (name "ocaml4.09-findlib")
    (native-inputs
     (list m4 ocaml-4.09))))

(define-public ocaml-ounit2
  (package
    (name "ocaml-ounit2")
    (version "2.2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gildor478/ounit.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1frdfnal6zl46dq5vlxz1ks28bf9x57zgik2cc65izji0ymr7pis"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-lwt ocaml-stdlib-shims))
    (home-page "https://github.com/gildor478/ounit")
    (synopsis "Unit testing framework for OCaml")
    (description "OUnit2 is a unit testing framework for OCaml.  It is similar
to JUnit and other XUnit testing frameworks.")
    (license license:expat)))

;; note that some tests may hang for no obvious reason.
(define-public ocaml-ounit
  (package
    (inherit ocaml-ounit2)
    (name "ocaml-ounit")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda _
             (invoke "make" "install-ounit" ,(string-append "version="
                                                            (package-version ocaml-ounit2))))))))
    (propagated-inputs
     (list ocaml-ounit2))
    (home-page "http://ounit.forge.ocamlcore.org")
    (synopsis "Unit testing framework for OCaml")
    (description "Unit testing framework for OCaml.  It is similar to JUnit and
other XUnit testing frameworks.")
    (license license:expat)))

(define-public camlzip
  (package
    (name "camlzip")
    (version "1.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/xavierleroy/camlzip")
                     (commit (string-append
                               "rel"
                               (string-join (string-split version #\.) "")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16jnn3czxnvyjngnz167x5kw097k7izdqvkix8qvgvhdmgvqm89b"))))
    (build-system ocaml-build-system)
    (inputs
     (list zlib))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:install-target "install-findlib"
       #:make-flags
       ,#~(list "all" "allopt"
                (string-append "INSTALLDIR=" #$output "/lib/ocaml"))))
    (home-page "https://github.com/xavierleroy/camlzip")
    (synopsis "Provides easy access to compressed files")
    (description "Provides easy access to compressed files in ZIP, GZIP and
JAR format.  It provides functions for reading from and writing to compressed
files in these formats.")
    (license license:lgpl2.1+)))

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

(define-public ocaml-zarith
  (package
    (name "ocaml-zarith")
    (version "1.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/Zarith")
                     (commit (string-append "release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jslm1rv1j0ya818yh23wf3bb6hz7qqj9pn5fwl45y9mqyqa01s9"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list perl))
    (inputs
     (list gmp))
    (arguments
     `(#:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _ (invoke "./configure")))
         (add-after 'install 'move-sublibs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/ocaml/site-lib")))
               (mkdir-p (string-append lib "/stublibs"))
               (rename-file (string-append lib "/zarith/dllzarith.so")
                            (string-append lib "/stublibs/dllzarith.so"))))))))
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
    (version "4.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/BinaryAnalysisPlatform/FrontC")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mi1vh4qgscnb470qwidccaqd068j1bqlz6pf6wddk21paliwnqb"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "test.t"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _
             (for-each make-file-writable (find-files "." ".")))))))
    (native-inputs
     (list ocaml-menhir ocaml-odoc))
    (properties `((upstream-name . "FrontC")
                  (ocaml4.07-variant . ,(delay ocaml4.07-frontc))))
    (home-page "https://www.irit.fr/FrontC")
    (synopsis "C parser and lexer library")
    (description "FrontC is an OCAML library providing a C parser and lexer.
The result is a syntactic tree easy to process with usual OCAML tree management.
It provides support for ANSI C syntax, old-C K&R style syntax and the standard
GNU CC attributes.  It provides also a C pretty printer as an example of use.")
    (license license:lgpl2.1)))

(define-public ocaml4.07-frontc
  (package-with-ocaml4.07
    (package
      (inherit ocaml-frontc)
      (version "3.4.2")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/mirage/ocaml-base64")
                       (commit (string-append
                                 "V_" (string-join (string-split version #\.) "_")))))
                (file-name (git-file-name "ocaml-frontc" version))
                (sha256
                 (base32
                  "0k7jk9hkglnkk27s62xl493jyqc017gyvwqb1lyc0ywbb001s102"))))
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
         #:make-flags ,#~(list (string-append "PREFIX=" #$output)
                               "OCAML_SITE=$(LIB_DIR)/ocaml/")))
      (properties '()))))

(define-public ocaml-qcheck
  (package
    (name "ocaml-qcheck")
    (version "0.18.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/c-cube/qcheck")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g3r8lrw6fsdphgpnp08saxyxk1vd3chpmb564ir2qnsp716vz6z"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
     (list ocaml-alcotest ocaml-ounit))
    (native-inputs
     (list ocamlbuild))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-qcheck))))
    (home-page "https://github.com/c-cube/qcheck")
    (synopsis "QuickCheck inspired property-based testing for OCaml")
    (description "QuickCheck inspired property-based testing for OCaml. This
module checks invariants (properties of some types) over randomly
generated instances of the type. It provides combinators for generating
instances and printing them.")
    (license license:lgpl3+)))

(define-public ocaml4.07-qcheck
  (package-with-ocaml4.07
    (package
      (inherit ocaml-qcheck)
      (version "0.12")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/c-cube/qcheck")
                      (commit version)))
                (file-name (git-file-name "ocaml-qcheck" version))
                (sha256
                 (base32
                  "1llnfynhlndwyjig7wrayjnds2b3mggp5lw20dwxhn2i2lkkb22m"))))
      (properties '()))))

(define-public ocaml-qtest
  (package
    (name "ocaml-qtest")
    (version "2.11.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/vincent-hugot/qtest/")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04ghjshh6104xyglm0db9kv90m62qla5f4bfrlndv6dsvgw3rdjl"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "tests"))
    (propagated-inputs
     `(("ounit" ,ocaml-ounit)
       ("qcheck" ,ocaml-qcheck)))
    (home-page "https://github.com/vincent-hugot/qtest")
    (synopsis "Inline (Unit) Tests for OCaml")
    (description "Qtest extracts inline unit tests written using a special
syntax in comments.  Those tests are then run using the oUnit framework and the
qcheck library.  The possibilities range from trivial tests -- extremely simple
to use -- to sophisticated random generation of test cases.")
    (license license:lgpl3+)))

(define-public ocaml-stringext
  (package
    (name "ocaml-stringext")
    (version "1.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/rgrinberg/stringext")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m09cmn3vrk3gdm60fb730qsygcfyxsyv7gl9xfzck08q1x2x9qx"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
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

(define dune-bootstrap
  (package
    (name "dune")
    (version "2.9.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/dune")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b4rsqn6gf3cv46jgvrsq0xh9zfsaif810zpbvm0mv2bhphqfjk7"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; require odoc
       #:make-flags ,#~(list "release"
                             (string-append "PREFIX=" #$output)
                             (string-append "LIBDIR=" #$output
                                            "/lib/ocaml/site-lib"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p "src/dune")
             (invoke "./configure")
             #t)))))
    (home-page "https://github.com/ocaml/dune")
    (synopsis "OCaml build system")
    (description "Dune is a build system that was designed to simplify the
release of Jane Street packages.  It reads metadata from @file{dune} files
following a very simple s-expression syntax.")
    (license license:expat)))

(define ocaml4.09-dune-bootstrap
  (package-with-ocaml4.09 dune-bootstrap))

(define-public dune-configurator
  (package
    (inherit dune-bootstrap)
    (name "dune-configurator")
    (build-system dune-build-system)
    (arguments
     `(#:package "dune-configurator"
       #:dune ,dune-bootstrap
       ; require ppx_expect
       #:tests? #f))
    (propagated-inputs
     (list ocaml-csexp))
    (properties `((ocaml4.09-variant . ,(delay ocaml4.09-dune-configurator))))
    (synopsis "Dune helper library for gathering system configuration")
    (description "Dune-configurator is a small library that helps writing
OCaml scripts that test features available on the system, in order to generate
config.h files for instance.  Among other things, dune-configurator allows one to:

@itemize
@item test if a C program compiles
@item query pkg-config
@item import #define from OCaml header files
@item generate config.h file
@end itemize")))

(define-public ocaml4.09-dune-configurator
  (package
    (inherit dune-configurator)
    (name "ocaml4.09-dune-configurator")
    (arguments
     `(#:package "dune-configurator"
       #:tests? #f
       #:dune ,ocaml4.09-dune-bootstrap
       #:ocaml ,ocaml-4.09
       #:findlib ,ocaml4.09-findlib))
    (propagated-inputs
     `(("ocaml-csexp" ,ocaml4.09-csexp)))))

(define-public dune
  (package
    (inherit dune-bootstrap)
    (propagated-inputs
     (list dune-configurator))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-dune))
                  (ocaml4.09-variant . ,(delay ocaml4.09-dune))))))

(define-public ocaml4.09-dune
  (package
    (inherit ocaml4.09-dune-bootstrap)
    (propagated-inputs
     (list dune-configurator))))

(define-public ocaml4.07-dune
  (package
    (inherit (package-with-ocaml4.07 dune-bootstrap))
    (version "1.11.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/dune")
                     (commit version)))
              (file-name (git-file-name "dune" version))
              (sha256
               (base32
                "0l4x0x2fz135pljv88zj8y6w1ninsqw0gn1mdxzprd6wbxbyn8wr"))))))

(define-public ocaml-csexp
  (package
    (name "ocaml-csexp")
    (version "1.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml-dune/csexp")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v5y4x1a21193h8q536c0s0d8hv3hyyky4pgzm2dw9807v36s2x4"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f; FIXME: needs ppx_expect, but which version?
       #:dune ,dune-bootstrap
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'chmod
           (lambda _
             (for-each (lambda (file) (chmod file #o644)) (find-files "." ".*"))
             #t)))))
    (propagated-inputs
     (list ocaml-result))
    (properties `((ocaml4.09-variant . ,(delay ocaml4.09-csexp))))
    (home-page "https://github.com/ocaml-dune/csexp")
    (synopsis "Parsing and printing of S-expressions in Canonical form")
    (description "This library provides minimal support for Canonical
S-expressions.  Canonical S-expressions are a binary encoding of
S-expressions that is super simple and well suited for communication
between programs.

This library only provides a few helpers for simple applications.  If
you need more advanced support, such as parsing from more fancy input
sources, you should consider copying the code of this library given
how simple parsing S-expressions in canonical form is.

To avoid a dependency on a particular S-expression library, the only
module of this library is parameterised by the type of S-expressions.")
    (license license:expat)))

(define-public ocaml4.09-csexp
  (package
    (inherit ocaml-csexp)
    (name "ocaml4.09-csexp")
    (arguments
     `(#:ocaml ,ocaml-4.09
       #:findlib ,ocaml4.09-findlib
       ,@(substitute-keyword-arguments (package-arguments ocaml-csexp)
           ((#:dune _) ocaml4.09-dune-bootstrap))))
    (propagated-inputs
     `(("ocaml-result" ,ocaml4.09-result)))))

(define-public ocaml-migrate-parsetree
  (package
    (name "ocaml-migrate-parsetree")
    (version "2.3.0")
    (home-page "https://github.com/ocaml-ppx/ocaml-migrate-parsetree")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nqmhsc72xmgm865nn8q0hngklhvqav281hgnx1gf5ns49a0n3ig"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list ocaml-ppx-derivers ocamlbuild ocaml-result))
    (properties `((upstream-name . "ocaml-migrate-parsetree")
                  (ocaml4.07-variant . ,(delay ocaml4.07-migrate-parsetree))))
    (synopsis "OCaml parsetree converter")
    (description "This library converts between parsetrees of different OCaml
versions.  For each version, there is a snapshot of the parsetree and conversion
functions to the next and/or previous version.")
    (license license:lgpl2.1+)))

;; OCaml 4.07 packages require version 1.*
(define-public ocaml4.07-migrate-parsetree
  (package-with-ocaml4.07
    (package
      (inherit ocaml-migrate-parsetree)
      (name "ocaml-migrate-parsetree")
      (version "1.8.0")
      (home-page "https://github.com/ocaml-ppx/ocaml-migrate-parsetree")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "16x8sxc4ygxrr1868qpzfqyrvjf3hfxvjzmxmf6ibgglq7ixa2nq"))))
      (properties '((upstream-name . "ocaml-migrate-parsetree"))))))

(define-public ocaml4.07-ppx-tools-versioned
  (package-with-ocaml4.07
    (package
      (name "ocaml-ppx-tools-versioned")
      (version "5.4.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/ocaml-ppx/ppx_tools_versioned")
                       (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07lnj4yzwvwyh5fhpp1dxrys4ddih15jhgqjn59pmgxinbnddi66"))))
      (build-system dune-build-system)
      (arguments
       `(#:test-target "."
         #:package "ppx_tools_versioned"))
      (propagated-inputs
       (list ocaml-migrate-parsetree))
      (properties `((upstream-name . "ppx_tools_versioned")))
      (home-page "https://github.com/let-def/ppx_tools_versioned")
      (synopsis "Variant of ppx_tools")
      (description "This package is a variant of ppx_tools based on
ocaml-migrate-parsetree")
      (license license:expat))))

(define-public ocaml-bitstring
  (package
    (name "ocaml-bitstring")
    (version "4.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/xguerin/bitstring")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mghsl8b2zd2676mh1r9142hymhvzy9cw8kgkjmirxkn56wbf56b"))))
    (build-system dune-build-system)
    (native-inputs
     (list time autoconf automake))
    (propagated-inputs
     (list ocaml-stdlib-shims))
    (arguments
     `(#:package "bitstring"
       #:tests? #f; Tests fail to build
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'upgrade
           (lambda _
             (invoke "dune" "upgrade")
             #t)))))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-bitstring))))
    (home-page "https://github.com/xguerin/bitstring")
    (synopsis "Bitstrings and bitstring matching for OCaml")
    (description "Adds Erlang-style bitstrings and matching over bitstrings as
a syntax extension and library for OCaml.  You can use this module to both parse
and generate binary formats, files and protocols.  Bitstring handling is added
as primitives to the language, making it exceptionally simple to use and very
powerful.")
    (license license:isc)))

(define-public ocaml4.07-bitstring
  (package-with-ocaml4.07
    (package
      (inherit ocaml-bitstring)
      (version "3.1.0")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://bitbucket.org/thanatonauts/bitstring/"
                                    "get/v" version ".tar.gz"))
                (file-name (string-append "ocaml-bitsring-" version ".tar.gz"))
                (sha256
                 (base32
                  "15jjk2pq1vx311gl49s5ag6x5y0654x35w75z07g7kr2q334hqps"))))
      (propagated-inputs
       `(("ocaml-ppx-tools-versioned" ,ocaml4.07-ppx-tools-versioned)))
      (properties '()))))
 
(define-public ocaml-result
  (package
    (name "ocaml-result")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/result")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "166laj8qk7466sdl037c6cjs4ac571hglw4l5qpyll6df07h6a7q"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."
       #:dune ,dune-bootstrap))
    (properties `((ocaml4.09-variant . ,(delay ocaml4.09-result))))
    (home-page "https://github.com/janestreet/result")
    (synopsis "Compatibility Result module")
    (description "Uses the new result type defined in OCaml >= 4.03 while
staying compatible with older version of OCaml should use the Result module
defined in this library.")
    (license license:bsd-3)))

(define-public ocaml4.09-result
  (package
    (inherit ocaml-result)
    (name "ocaml4.09-result")
    (arguments
     `(#:test-target "."
       #:dune ,ocaml4.09-dune-bootstrap
       #:ocaml ,ocaml-4.09
       #:findlib ,ocaml4.09-findlib))))
 
(define-public ocaml-topkg
  (package
    (name "ocaml-topkg")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/topkg/releases/"
                                  "topkg-" version ".tbz"))
              (sha256
               (base32
                "1iyinmcfqpprk7k4cc51nqgypayprbj4larwcfqw86k5dri84825"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list opam ocamlbuild))
    (propagated-inputs
     `(("result" ,ocaml-result)))
    (arguments
     `(#:tests? #f
       #:build-flags '("build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://erratique.ch/software/topkg")
    (synopsis "Transitory OCaml software packager")
    (description "Topkg is a packager for distributing OCaml software. It
provides an API to describe the files a package installs in a given build
configuration and to specify information about the package's distribution,
creation and publication procedures.")
    (license license:isc)))
 
(define-public ocaml-rresult
  (package
    (name "ocaml-rresult")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/rresult/releases/"
                                  "rresult-" version ".tbz"))
              (sha256
               (base32
                "0h2mjyzhay1p4k7n0mzaa7hlc7875kiy6m1i3r1n03j6hddpzahi"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list opam ocamlbuild))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)))
    (arguments
     `(#:tests? #f
       #:build-flags '("build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://erratique.ch/software/rresult")
    (synopsis "Result value combinators for OCaml")
    (description "Handle computation results and errors in an explicit and
declarative manner, without resorting to exceptions.  It defines combinators
to operate on the result type available from OCaml 4.03 in the standard
library.")
    (license license:isc)))

(define-public ocaml-sqlite3
  (package
    (name "ocaml-sqlite3")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mmottl/sqlite3-ocaml")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ksm0a490315sf0yy8lmva5f3bgr0jnllffanyq89431grpj6x15"))))
    (build-system dune-build-system)
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-sqlite3))))
    (propagated-inputs
     (list dune-configurator ocaml-odoc))
    (native-inputs
     (list ocaml-ppx-inline-test pkg-config sqlite))
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

(define-public ocaml4.07-sqlite3
  (package-with-ocaml4.07
   (package
     (inherit ocaml-sqlite3)
     (version "5.0.2")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mmottl/sqlite3-ocaml")
               (commit version)))
        (file-name (git-file-name "ocaml-sqlite3" version))
        (sha256
         (base32
          "15mmq7ak5facpfawfrc6hjz211gli7jab52iqdsihfvh790xm55f"))))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
         (add-before 'build 'chmod
           (lambda _
             (for-each (lambda (file) (chmod file #o644)) (find-files "." ".*"))
             #t)))))
     (propagated-inputs
      `(("ocaml-odoc" ,ocaml-odoc)))
     (properties '()))))

(define-public ocaml-csv
  (package
    (name "ocaml-csv")
    (version "2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Chris00/ocaml-csv")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0y2hlqlmqs7r4y5mfzc5qdv7gdp3wxbwpz458vf7fj4593vg94cf"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "csv"
       #:test-target "."))
    (home-page "https://github.com/Chris00/ocaml-csv")
    (synopsis "Pure OCaml functions to read and write CSV")
    (description
     "@dfn{Comma separated values} (CSV) is a simple tabular format supported
by all major spreadsheets.  This library implements pure OCaml functions to
read and write files in this format as well as some convenience functions to
manipulate such data.")
    ;; This is LGPLv2.1 with an exception that allows packages statically-linked
    ;; against the library to be released under any terms.
    (license license:lgpl2.1)))

(define-public ocaml-mtime
  (package
    (name "ocaml-mtime")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/mtime/releases/"
                                  "mtime-" version ".tbz"))
              (sha256
               (base32
                "0syilgk4nzscacsswnvgwqlf0n0lhs221jss8gc8z9igw2x4sgsq"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list ocamlbuild opam))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)))
    (arguments
     `(#:tests? #f
       #:build-flags (list "build" "--with-js_of_ocaml" "false")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://erratique.ch/software/mtime")
    (synopsis "Monotonic wall-clock time for OCaml")
    (description "Access monotonic wall-clock time.  It measures time
spans without being subject to operating system calendar time adjustments.")
    (license license:isc)))

(define-public ocaml-calendar
  ;; No tags.
  ;; Commit from 2019-02-03.
  (let ((commit "a447a88ae3c1e9873e32d2a95d3d3e7c5ed4a7da")
        (revision "0"))
    (package
      (name "ocaml-calendar")
      ;; Makefile.in says 2.04.
      (version (git-version "2.04" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ocaml-community/calendar")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "09d9gyqm3zkf3z2m9fx87clqihx6brf8rnzm4yq7c8kf1p572hmc"))))
      (build-system gnu-build-system)
      (arguments
       '(#:test-target "tests"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'make-deterministic
             (lambda _
               (substitute* "Makefile.in"
                 (("`date`") "no date for reproducibility"))))
           (add-before 'install 'set-environment-variables
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (setenv "OCAMLFIND_DESTDIR"
                         (string-append out "/lib/ocaml/site-lib"))
                 (setenv "OCAMLFIND_LDCONF" "ignore")
                 (mkdir-p (string-append
                           out "/lib/ocaml/site-lib/calendar"))))))))
      (native-inputs
       (list autoconf automake))
      (propagated-inputs
       `(("ocaml" ,ocaml)
         ("ocamlfind" ,ocaml-findlib)))
      (home-page "https://github.com/ocaml-community/calendar")
      (synopsis "OCaml library for handling dates and times")
      (description "This package provides types and operations over
dates and times.")
      ;; With linking exception.
      (license license:lgpl2.1+))))

(define-public ocaml-cmdliner
  (package
    (name "ocaml-cmdliner")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://erratique.ch/software/cmdliner/releases/"
                                  "cmdliner-" version ".tbz"))
              (sha256
               (base32
                "1h04q0zkasd0mw64ggh4y58lgzkhg6yhzy60lab8k8zq9ba96ajw"))))
    (build-system ocaml-build-system)
    (inputs
     (list ocaml-result))
    (native-inputs
     (list ocamlbuild))
    (arguments
     `(#:tests? #f
       #:make-flags ,#~(list (string-append "LIBDIR=" #$output
                                            "/lib/ocaml/site-lib/cmdliner"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-source-file-order
           (lambda _
             (substitute* "build.ml"
               (("Sys.readdir dir")
                "let a = Sys.readdir dir in Array.sort String.compare a; a"))
             #t)))))
    (home-page "https://erratique.ch/software/cmdliner")
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
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://erratique.ch/software/fmt/releases/fmt-"
                            version ".tbz"))
        (sha256 (base32
                  "0q8j2in2473xh7k4hfgnppv9qy77f2ih89yp6yhpbp92ba021yzi"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)
       ("topkg" ,ocaml-topkg)))
    (propagated-inputs
     `(("cmdliner" ,ocaml-cmdliner)
       ("ocaml-stdlib-shims" ,ocaml-stdlib-shims)
       ("ocaml-uchar" ,ocaml-uchar)))
    (arguments `(#:tests? #f
                 #:build-flags (list "build" "--with-base-unix" "true"
                                     "--with-cmdliner" "true")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-fmt))))
    (home-page "https://erratique.ch/software/fmt")
    (synopsis "OCaml Format pretty-printer combinators")
    (description "Fmt exposes combinators to devise Format pretty-printing
functions.")
    (license license:isc)))

(define-public ocaml4.07-fmt
  (package-with-ocaml4.07
    (package
      (inherit ocaml-fmt)
      (version "0.8.9")
      (source (origin
                (method url-fetch)
                (uri (string-append "http://erratique.ch/software/fmt/releases/fmt-"
                                    version ".tbz"))
                (sha256
                 (base32
                  "0gkkkj4x678vxdda4xaw2dd44qjacavsvn5nx8gydfwah6pjbkxk"))))
      (properties '()))))

(define-public ocaml-astring
  (package
    (name "ocaml-astring")
    (version "0.8.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://erratique.ch/software/astring/releases/astring-"
                            version ".tbz"))
        (sha256 (base32
                  "1ykhg9gd3iy7zsgyiy2p9b1wkpqg9irw5pvcqs3sphq71iir4ml6"))))
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
    (home-page "https://erratique.ch/software/astring")
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
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/alcotest")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lgadc7p56677fi5gwl7r9bml4z87rzh50khhqaij7bwp37zrmpi"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "alcotest"
       #:test-target "."))
    (native-inputs
     (list ocamlbuild))
    (propagated-inputs
     (list ocaml-astring
           ocaml-cmdliner
           ocaml-fmt
           ocaml-re
           ocaml-stdlib-shims
           ocaml-uuidm
           ocaml-uutf))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-alcotest))))
    (home-page "https://github.com/mirage/alcotest")
    (synopsis "Lightweight OCaml test framework")
    (description "Alcotest exposes simple interface to perform unit tests.  It
exposes a simple TESTABLE module type, a check function to assert test
predicates and a run function to perform a list of unit -> unit test callbacks.
Alcotest provides a quiet and colorful output where only faulty runs are fully
displayed at the end of the run (with the full logs ready to inspect), with a
simple (yet expressive) query language to select the tests to run.")
    (license license:isc)))

(define-public ocaml4.07-alcotest
  (package-with-ocaml4.07
    (package
      (inherit ocaml-alcotest)
      (version "1.0.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mirage/alcotest")
                      (commit version)))
                (file-name (git-file-name "ocaml-alcotest" version))
                (sha256
                 (base32
                  "1frwi185z4aadmaf0vp8xk5227nyg7nmh28ijj5l7ncjr5slvhz8"))))
      (properties '()))))

(define-public ocaml-ppx-tools
  (package
    (name "ocaml-ppx-tools")
    (version "6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alainfrisch/ppx_tools")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15v7yfv6gyp8lzlgwi9garz10wpg34dk4072jdv19n6v20zfg7n1"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (native-inputs
     (list ocaml-cppo))
    (properties `((upstream-name . "ppx_tools")))
    (home-page "https://github.com/alainfrisch/ppx_tools")
    (synopsis "Tools for authors of ppx rewriters and other syntactic tools")
    (description
     "Ppx_tools provides tools for authors of ppx rewriters and other
syntactic tools.")
    (license license:expat)))

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
     (list ocamlbuild opam ocaml-topkg))
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://erratique.ch/software/react")
    (synopsis "Declarative events and signals for OCaml")
    (description "React is an OCaml module for functional reactive programming
(FRP).  It provides support to program with time varying values: declarative
events and signals.  React doesn't define any primitive event or signal, it
lets the client choose the concrete timeline.")
    (license license:bsd-3)))

(define-public ocaml-ssl
  (package
    (name "ocaml-ssl")
    (version "0.5.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/savonet/ocaml-ssl")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256 (base32
                  "1rszqiqayh67xlwd5411k8vib47x9kapdr037z1majd2c14z3kcb"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (native-inputs
     (list autoconf automake which))
    (propagated-inputs (list openssl))
    (home-page "https://github.com/savonet/ocaml-ssl/")
    (synopsis "OCaml bindings for OpenSSL")
    (description
     "OCaml-SSL is a set of bindings for OpenSSL, a library for communicating
through Transport Layer Security (@dfn{TLS}) encrypted connections.")
    (license license:lgpl2.1)))

(define-public ocaml-mmap
  (package
    (name "ocaml-mmap")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mirage/mmap")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jaismy5d1bhbbanysmr2k79px0yv6ya265dri3949nha1l23i60"))))
    (build-system dune-build-system)
    (home-page "https://github.com/mirage/mmap")
    (synopsis "File mapping for OCaml")
    (description "This project provides a @command{Mmap.map_file} function
for mapping files in memory.  This function is the same as the
@command{Unix.map_file} function added in OCaml >= 4.06.")
    (license (list license:qpl license:lgpl2.0))))

(define-public ocaml-lwt
  (package
    (name "ocaml-lwt")
    (version "5.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ocsigen/lwt")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32
                 "1jbjz2rsz3j56k8vh5qlmm87hhkr250bs2m3dvpy9vsri8rkzj9z"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "lwt"))
    (native-inputs
     (list ocaml-cppo pkg-config))
    (inputs
     (list glib))
    (propagated-inputs
     (list ocaml-mmap ocaml-ocplib-endian ocaml-result ocaml-seq libev))
    (home-page "https://github.com/ocsigen/lwt")
    (synopsis "Cooperative threads and I/O in monadic style")
    (description "Lwt provides typed, composable cooperative threads.  These
make it easy to run normally-blocking I/O operations concurrently in a single
process.  Also, in many cases, Lwt threads can interact without the need for
locks or other synchronization primitives.")
    (license license:lgpl2.1)))

(define-public ocaml-lwt-react
  (package
    (inherit ocaml-lwt)
    (name "ocaml-lwt-react")
    (version "1.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocsigen/lwt")
                     ;; Version from opam
                     (commit "5.5.0")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jbjz2rsz3j56k8vh5qlmm87hhkr250bs2m3dvpy9vsri8rkzj9z"))))
    (arguments
     `(#:package "lwt_react"))
    (properties `((upstream-name . "lwt_react")))
    (propagated-inputs
     (list ocaml-lwt ocaml-react))))

(define-public ocaml-lwt-log
  (package
    (name "ocaml-lwt-log")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/aantron/lwt_log")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1n12i1rmn9cjn6p8yr6qn5dwbrwvym7ckr7bla04a1xnq8qlcyj7"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)); require lwt_ppx
    (propagated-inputs
     `(("lwt" ,ocaml-lwt)))
    (properties `((upstream-name . "lwt_log")))
    (home-page "https://github.com/aantron/lwt_log")
    (synopsis "Logging library")
    (description "This package provides a deprecated logging component for
ocaml lwt.")
    (license license:lgpl2.1)))

(define-public ocaml-logs
  (package
    (name "ocaml-logs")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/logs/releases/"
                                  "logs-" version ".tbz"))
              (sha256
                (base32
                  "1jnmd675wmsmdwyb5mx5b0ac66g4c6gpv5s4mrx2j6pb0wla1x46"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build" "--with-js_of_ocaml" "false")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild opam))
    (propagated-inputs
     `(("fmt" ,ocaml-fmt)
       ("lwt" ,ocaml-lwt)
       ("mtime" ,ocaml-mtime)
       ("result" ,ocaml-result)
       ("cmdliner" ,ocaml-cmdliner)
       ("topkg" ,ocaml-topkg)))
    (home-page "https://erratique.ch/software/logs")
    (synopsis "Logging infrastructure for OCaml")
    (description "Logs provides a logging infrastructure for OCaml.  Logging is
performed on sources whose reporting level can be set independently.  Log
message report is decoupled from logging and is handled by a reporter.")
    (license license:isc)))

(define-public ocaml-fpath
  (package
    (name "ocaml-fpath")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/fpath/releases/"
                                  "fpath-" version ".tbz"))
              (sha256
                (base32
                  "03z7mj0sqdz465rc4drj1gr88l9q3nfs374yssvdjdyhjbqqzc0j"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild opam))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)
       ("astring" ,ocaml-astring)))
    (home-page "https://erratique.ch/software/fpath")
    (synopsis "File system paths for OCaml")
    (description "Fpath is an OCaml module for handling file system paths with
POSIX or Windows conventions.  Fpath processes paths without accessing the
file system and is independent from any system library.")
    (license license:isc)))

(define-public ocaml-bos
  (package
    (name "ocaml-bos")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/bos/releases/"
                                  "bos-" version ".tbz"))
              (sha256
                (base32
                  "0dwg7lpaq30rvwc5z1gij36fn9xavvpah1bj8ph9gmhhddw2xmnq"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild opam))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)
       ("astring" ,ocaml-astring)
       ("fmt" ,ocaml-fmt)
       ("fpath" ,ocaml-fpath)
       ("logs" ,ocaml-logs)
       ("rresult" ,ocaml-rresult)))
    (home-page "https://erratique.ch/software/bos")
    (synopsis "Basic OS interaction for OCaml")
    (description "Bos provides support for basic and robust interaction with
the operating system in OCaml.  It has functions to access the process
environment, parse command line arguments, interact with the file system and
run command line programs.")
    (license license:isc)))

(define-public ocaml-xmlm
  (package
    (name "ocaml-xmlm")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/xmlm/releases/"
                                  "xmlm-" version ".tbz"))
              (sha256
                (base32
                  "1ynrjba3wm3axscvggrfijfgsznmphhxnkffqch67l9xiqjm44h9"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild ocaml-topkg opam))
    (home-page "https://erratique.ch/software/xmlm")
    (synopsis "Streaming XML codec for OCaml")
    (description "Xmlm is a streaming codec to decode and encode the XML data
format.  It can process XML documents without a complete in-memory
representation of the data.")
    (license license:isc)))

(define-public ocaml-gen
  (package
    (name "ocaml-gen")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/c-cube/gen")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z5nw5wljvcqp8q07h336bbvf9paynia0jsdh4486hlkbmr1ask1"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "gen"
       #:test-target "."))
    (propagated-inputs
     (list ocaml-odoc ocaml-seq))
    (native-inputs
     (list ocaml-qtest ocaml-qcheck))
    (home-page "https://github.com/c-cube/gen/")
    (synopsis "Iterators for OCaml, both restartable and consumable")
    (description "Gen implements iterators of OCaml, that are both restartable
and consumable.")
    (license license:bsd-2)))

(define-public ocaml-sedlex
  (package
    (name "ocaml-sedlex")
    (version "2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocaml-community/sedlex")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "062a5dvrzvb81l3a9phljrhxfw9nlb61q341q0a6xn65hll3z2wy"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:package "sedlex"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/generator/data"
               ;; Newer versions of dune emit an error if files it wants to
               ;; build already exist. Delete the dune file so dune doesn't
               ;; complain.
               (delete-file "dune")
               (for-each
                (lambda (file)
                  (copy-file (assoc-ref inputs file) file))
                '("DerivedCoreProperties.txt" "DerivedGeneralCategory.txt"
                  "PropList.txt")))
             #t))
         (add-before 'build 'chmod
           (lambda _
             (for-each (lambda (file) (chmod file #o644)) (find-files "." ".*"))
             #t)))))
    (propagated-inputs
     (list ocaml-gen ocaml-ppxlib ocaml-uchar))
    ;; These three files are needed by src/generator/data/dune, but would be
    ;; downloaded using curl at build time.
    (inputs
     `(("DerivedCoreProperties.txt"
        ,(origin
           (method url-fetch)
           (uri "https://www.unicode.org/Public/12.1.0/ucd/DerivedCoreProperties.txt")
           (sha256
            (base32
             "0s6sn1yr9qmb2i6gf8dir2zpsbjv1frdfzy3i2yjylzvf637msx6"))))
       ("DerivedGeneralCategory.txt"
        ,(origin
           (method url-fetch)
           (uri "https://www.unicode.org/Public/12.1.0/ucd/extracted/DerivedGeneralCategory.txt")
           (sha256
            (base32
             "1rifzq9ba6c58dn0lrmcb5l5k4ksx3zsdkira3m5p6h4i2wriy3q"))))
       ("PropList.txt"
        ,(origin
           (method url-fetch)
           (uri "https://www.unicode.org/Public/12.1.0/ucd/PropList.txt")
           (sha256
            (base32
             "0gsb1jpj3mnqbjgbavi4l95gl6g4agq58j82km22fdfg63j3w3fk"))))))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-sedlex))))
    (home-page "https://www.cduce.org/download.html#side")
    (synopsis "Lexer generator for Unicode and OCaml")
    (description "Lexer generator for Unicode and OCaml.")
    (license license:expat)))

(define-public ocaml4.07-sedlex
  (package-with-ocaml4.07
   (package
     (inherit ocaml-sedlex)
     (name "ocaml-sedlex")
     (version "2.1")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/ocaml-community/sedlex")
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "05f6qa8x3vhpdz1fcnpqk37fpnyyq13icqsk2gww5idjnh6kng26"))))
     (propagated-inputs
      `(("ocaml-ppx-tools-versioned" ,ocaml4.07-ppx-tools-versioned)
        ,@(package-propagated-inputs ocaml-sedlex)))
     (properties '()))))

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
     (list ocamlbuild opam))
    (home-page "https://github.com/ocaml/uchar")
    (synopsis "Compatibility library for OCaml's Uchar module")
    (description "The uchar package provides a compatibility library for the
`Uchar` module introduced in OCaml 4.03.")
    (license license:lgpl2.1)))

(define-public ocaml-uutf
  (package
    (name "ocaml-uutf")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/uutf/releases/"
                                  "uutf-" version ".tbz"))
              (sha256
                (base32
                  "0s05r8ggp1g97zq4rnvbxzj22pv8ld0k5wsdw662jw0y7mhsawl7"))))
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
    (home-page "https://erratique.ch/software/uutf")
    (synopsis "Non-blocking streaming Unicode codec for OCaml")
    (description "Uutf is a non-blocking streaming codec to decode and encode
the UTF-8, UTF-16, UTF-16LE and UTF-16BE encoding schemes.  It can efficiently
work character by character without blocking on IO.  Decoders perform character
position tracking and support newline normalization.

Functions are also provided to fold over the characters of UTF encoded OCaml
string values and to directly encode characters in OCaml Buffer.t values.")
    (license license:isc)))

(define-public ocaml-uunf
  (package
    (name "ocaml-uunf")
    (version "14.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/uunf/releases/uunf-"
                           version".tbz"))
       (sha256
        (base32
         "17wv0nm3vvwcbzb1b09akw8jblmigyhbfmh1sy9lkb5756ni94a2"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags (list "build" "--tests" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'check 'check-data
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-file (assoc-ref inputs "NormalizationTest.txt")
                        "test/NormalizationTest.txt")
             #t)))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)
       ("topkg" ,ocaml-topkg)
       ;; Test data is otherwise downloaded with curl
       ("NormalizationTest.txt"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://www.unicode.org/Public/"
                               version
                               "/ucd/NormalizationTest.txt"))
           (file-name (string-append "NormalizationTest-" version ".txt"))
           (sha256
              (base32 "0c93pqdkksf7b7zw8y2w0h9i5kkrsdjmh2cr5clrrhp6mg10rcvw"))))))
    (propagated-inputs (list ocaml-uutf))
    (home-page "https://erratique.ch/software/uunf")
    (synopsis "Unicode text normalization for OCaml")
    (description
     "Uunf is an OCaml library for normalizing Unicode text.  It supports all
Unicode normalization forms.  The library is independent from any
IO mechanism or Unicode text data structure and it can process text
without a complete in-memory representation.")
    (license license:isc)))

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
    (home-page "https://erratique.ch/software/jsonm")
    (synopsis "Non-blocking streaming JSON codec for OCaml")
    (description "Jsonm is a non-blocking streaming codec to decode and encode
the JSON data format.  It can process JSON text without blocking on IO and
without a complete in-memory representation of the data.")
    (license license:isc)))
 
(define-public ocaml-ocp-indent
  (package
    (name "ocaml-ocp-indent")
    (version "1.8.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/OCamlPro/ocp-indent")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1dvcl108ir9nqkk4mjm9xhhj4p9dx9bmg8bnms54fizs1x3x8ar3"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "tests"))
    (propagated-inputs
     (list ocaml-cmdliner))
    (home-page "https://www.typerex.org/ocp-indent.html")
    (synopsis "Tool to indent OCaml programs")
    (description
      "Ocp-indent is based on an approximate, tolerant OCaml parser
and a simple stack machine.  Presets and configuration options are available,
with the possibility to set them project-wide.  It supports the most common
syntax extensions, and it is extensible for others.

This package includes:

@itemize
@item An indentor program, callable from the command-line or from within editors,
@item Bindings for popular editors,
@item A library that can be directly used by editor writers, or just for
      fault-tolerant and approximate parsing.
@end itemize")
    (license license:lgpl2.1)))
 
(define-public ocaml-ocp-index
  (package
    (name "ocaml-ocp-index")
    (version "1.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/OCamlPro/ocp-index")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1gbigw5s2cafkr82n9vkxbb892qfkykj0adj0hrdkrkw8j6rfl0j"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "ocp-index"))
    (propagated-inputs
     (list ocaml-ocp-indent ocaml-re ocaml-cmdliner))
    (native-inputs
     (list ocaml-cppo))
    (home-page "https://www.typerex.org/ocp-index.html")
    (synopsis "Lightweight completion and documentation browsing for OCaml libraries")
    (description "This package includes only the @code{ocp-index} library
and command-line tool.")
    ;; All files in libs/ are GNU lgpl2.1
    ;; For static linking, clause 6 of LGPL is lifted
    ;; All other files under GNU gpl3
    (license (list license:gpl3+
                   license:lgpl2.1+))))

(define-public ocaml-ocurl
  (package
    (name "ocaml-ocurl")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ygrek.org.ua/p/release/ocurl/ocurl-"
                                  version ".tar.gz"))
              (sha256
                (base32
                  "0qvpsqbq4qbd397n0nlv9cwlqfyjw7gfb5mmq1awvnklr0c9fdg0"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-/bin/sh
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure"
               (("-/bin/sh") (string-append "-" (which "bash")))))))))
    (native-inputs
     (list pkg-config))
    (inputs (list curl))
    (home-page "http://ocurl.forge.ocamlcore.org/")
    (synopsis "OCaml bindings for libcurl")
    (description "Client-side URL transfer library, supporting HTTP and a
multitude of other network protocols (FTP/SMTP/RTSP/etc).")
    (license license:isc)))

(define-public ocaml-base64
  (package
    (name "ocaml-base64")
    (version "3.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mirage/ocaml-base64")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "068hwdbpl7vx9jjpxdc6a10zqd8xa55j3xx7ga6fnwrlfsbs2pjj"))))
    (build-system dune-build-system)
    (native-inputs
     (list ocaml-alcotest ocaml-bos ocaml-rresult))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-base64))))
    (home-page "https://github.com/mirage/ocaml-base64")
    (synopsis "Base64 encoding for OCaml")
    (description "Base64 is a group of similar binary-to-text encoding schemes
that represent binary data in an ASCII string format by translating it into a
radix-64 representation.  It is specified in RFC 4648.")
    (license license:isc)))

(define-public ocaml4.07-base64
  (package-with-ocaml4.07
    (package
      (inherit ocaml-base64)
      (version "3.2.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/mirage/ocaml-base64")
                       (commit (string-append "v" version))))
                (file-name (git-file-name "ocaml-base64" version))
                (sha256
                 (base32
                  "1ilw3zj0w6cq7i4pvr8m2kv5l5f2y9aldmv72drlwwns013b1gwy"))))
      (arguments
       ;; Tests are likely incompatible with our recent alcotest
       `(#:tests? #f))
      (properties '()))))

(define-public ocamlify
  (package
    (name "ocamlify")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.ocamlcore.org/ocamlify/ocamlify/"
                           version "/ocamlify-" version ".tar.gz"))
       (sha256
        (base32 "1f0fghvlbfryf5h3j4as7vcqrgfjb4c8abl5y0y5h069vs4kp5ii"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           ;; This package uses pre-generated setup.ml by oasis, but is
           ;; a dependency of oasis.  the pre-generated setup.ml is broken
           ;; with recent versions of OCaml, so we perform a bootstrap instead.
           (lambda _
             (substitute* "src/OCamlifyConfig.ml.ab"
               (("$pkg_version") ,version))
             (rename-file "src/OCamlifyConfig.ml.ab" "src/OCamlifyConfig.ml")
             (with-directory-excursion "src"
               (invoke "ocamlc" "OCamlifyConfig.ml" "ocamlify.ml" "-o"
                       "ocamlify"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "src/ocamlify" bin)
               #t))))))
    (home-page "https://forge.ocamlcore.org/projects/ocamlify")
    (synopsis "Include files in OCaml code")
    (description "OCamlify creates OCaml source code by including
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
       ,#~(list (string-append "PREFIX=" #$output))
       #:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-makefile
                     (lambda* (#:key outputs #:allow-other-keys)
                       (substitute* "mk/osconfig_unix.mk"
                                    (("CC = cc") "CC = gcc")))))))
    (native-inputs (list hevea))
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

(define-public ocaml-benchmark
  (package
    (name "ocaml-benchmark")
    (version "1.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Chris00/ocaml-benchmark")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0d0vdfjgjzf1y6wkd714d8b0piv1z9qav5ahsapynqzk4b4ahhnp"))))
    (build-system dune-build-system)
    (arguments `(#:test-target "tests"))
    (home-page "https://github.com/Chris00/ocaml-benchmark")
    (synopsis "Benchmark running times of code")
    (description
      "This module provides a set of tools to measure the running times of
your functions and to easily compare the results.  A statistical test
is used to determine whether the results truly differ.")
    (license license:lgpl3+)))

(define-public ocaml-batteries
  (package
    (name "ocaml-batteries")
    (version "3.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml-batteries-team/batteries-included")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w2fb47vgifj4iws0s8r531n0p9khq92n7gwcs7caj5bbzfc5zdx"))))
    (build-system ocaml-build-system)
    (propagated-inputs (list ocaml-num))
    (native-inputs
     (list ocamlbuild ocaml-benchmark ocaml-qcheck ocaml-qtest))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _
             (for-each make-file-writable (find-files "." "."))))
         (add-before 'build 'fix-nondeterminism
           (lambda _
             (substitute* "setup.ml"
               (("Sys.readdir dirname")
                "let a = Sys.readdir dirname in Array.sort String.compare a; a"))
             #t)))))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-batteries))))
    (home-page "http://batteries.forge.ocamlcore.org/")
    (synopsis "Development platform for the OCaml programming language")
    (description "Define a standard set of libraries which may be expected on
every compliant installation of OCaml and organize these libraries into a
hierarchy of modules.")
    (license license:lgpl2.1+)))

(define-public ocaml4.07-batteries
  (package-with-ocaml4.07
    (package
      (inherit ocaml-batteries)
      (version "2.10.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/ocaml-batteries-team/batteries-included")
                       (commit (string-append "v" version))))
                (file-name (git-file-name "ocaml-batteries" version))
                (sha256
                 (base32
                  "02fxa1nkp7rpiwfp04n0sagdp9lad4dh9bvljp95xfshm1cx7y4q"))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'check) ; tests are run by the build phase
           (add-before 'build 'fix-nondeterminism
             (lambda _
               (substitute* "setup.ml"
                 (("Sys.readdir dirname")
                  "let a = Sys.readdir dirname in Array.sort String.compare a; a"))
               #t))
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
      (native-inputs
       `(("ocamlbuild" ,ocamlbuild)
         ("qtest" ,ocaml-qtest)))
      (properties '()))))

(define-public ocaml-pcre
  (package
    (name "ocaml-pcre")
    (version "7.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/mmottl/pcre-ocaml")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "048k1rl17fcml000yh8fnghk1a06h14lbyrnk9nbigxsymrz6cq2"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests.
     '(#:tests? #f))
    (propagated-inputs
     (list dune-configurator pcre))
    (native-inputs
     `(("pcre:bin" ,pcre "bin")))
    (home-page "https://mmottl.github.io/pcre-ocaml")
    (synopsis
      "Bindings to the Perl Compatibility Regular Expressions library")
    (description "Pcre-ocaml offers library functions for string
pattern matching and substitution, similar to the functionality
offered by the Perl language.")
    ;; With static linking exception
    (license license:lgpl2.1+)))

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
    (propagated-inputs
     `(("batteries" ,ocaml-batteries)))
    (home-page "https://forge.ocamlcore.org/projects/ocaml-expect/")
    (synopsis "Simple implementation of expect")
    (description "This package provides utilities for building unitary testing
of interactive program.  You can match the question using a regular expression
or a timeout.")
    (license license:lgpl2.1+))) ; with the OCaml static compilation exception

(define-public ocaml-stdlib-shims
  (package
    (name "ocaml-stdlib-shims")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/stdlib-shims")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gmg8w67j3ww17llk7hl4dx0vq7p50rn5s4ib9sy984k543rz59h"))))
    (build-system dune-build-system)
    (home-page "https://github.com/ocaml/stdlib-shims")
    (synopsis "OCaml stdlib features backport to older OCaml compilers")
    (description "This package backports some of the new stdlib features to
older compilers, such as the Stdlib module.  This allows projects that require
compatibility with older compiler to use these new features in their code.")
    ;; with ocaml-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-fileutils
  (package
    (name "ocaml-fileutils")
    (version "0.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/gildor478/ocaml-fileutils")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0aa7p5qymi8p7iqym42yk2akjd1ff81fvaks82nhjc533zl01pnf"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-stdlib-shims))
    (native-inputs
     (list ocaml-ounit))
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
     (list ocamlbuild ocamlify ocamlmod))
    (home-page "https://oasis.forge.ocamlcore.org")
    (synopsis "Integrates a configure, build, install system in OCaml projects")
    (description "OASIS is a tool to integrate a configure, build and install
system in your OCaml projects.  It helps to create standard entry points in your
build system and allows external tools to analyse your project easily.")
    (license license:lgpl2.1+))) ; with ocaml static compilation exception

(define-public ocaml-cppo
  (package
    (name "ocaml-cppo")
    (version "1.6.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mjambon/cppo")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256 (base32
                 "0xjb1dnj8lvdcccskrhrakknd9q4vfy5330sjbqpf4h95dsz90k9"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     (list ocamlbuild))
    (home-page "https://github.com/mjambon/cppo")
    (synopsis "Equivalent of the C preprocessor for OCaml programs")
    (description "Cppo is an equivalent of the C preprocessor for OCaml
programs.  It allows the definition of simple macros and file inclusion.  Cppo is:
@enumerate
@item more OCaml-friendly than @command{cpp}
@item easy to learn without consulting a manual
@item reasonably fast
@item simple to install and to maintain.
@end enumerate")
    (license license:bsd-3)))

(define-public ocaml-seq
  (package
    (name "ocaml-seq")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/c-cube/seq")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cjpsc7q76yfgq9iyvswxgic4kfq2vcqdlmxjdjgd4lx87zvcwrv"))))
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
    (home-page "https://github.com/c-cube/seq")
    (synopsis "OCaml's standard iterator type")
    (description "This package is a compatibility package for OCaml's
standard iterator type starting from 4.07.")
    (license license:lgpl2.1+)))

(define-public ocaml-re
  (package
    (name "ocaml-re")
    (version "1.10.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/ocaml-re")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lj94y3zj6qff1yizf311h5ww15djbmnb1g43wd0jz3afa9gk59m"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
     (list ocaml-seq))
    (native-inputs
     `(("ounit" ,ocaml-ounit)))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-re))))
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

(define-public ocaml4.07-re
  (package-with-ocaml4.07
    (package
      (inherit ocaml-re)
      (arguments
       `(#:test-target "."
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-dune-version
             (lambda _
               (delete-file "dune-project"))))))
      (properties '()))))

(define-public ocaml-ocplib-endian
  (package
    (name "ocaml-ocplib-endian")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/OCamlPro/ocplib-endian/")
                     (commit version)))
              (sha256
               (base32
                "1klj4g451s7m5r8bxmwc1rpvngpqdm40csnx9smgc06pwy2fax2c"))
              (file-name (git-file-name name version))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "tests"))
    (native-inputs
     `(("cppo" ,ocaml-cppo)))
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
    (version "6.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mirage/ocaml-cstruct")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gpyr3cf393j1ir7i2m2qhx75l21w6ad7imdd73xn0jy3pjg4wsj"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "cstruct"
       #:test-target "."))
    (propagated-inputs
     (list ocaml-bigarray-compat))
    (native-inputs
     (list ocaml-alcotest))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-cstruct))))
    (home-page "https://github.com/mirage/ocaml-cstruct")
    (synopsis "Access C structures via a camlp4 extension")
    (description "Cstruct is a library and syntax extension to make it easier
to access C-like structures directly from OCaml.  It supports both reading and
writing to these structures, and they are accessed via the Bigarray module.")
    (license license:isc)))

(define-public ocaml4.07-cstruct
  (package-with-ocaml4.07
    (package
      (inherit ocaml-cstruct)
      (version "5.1.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/mirage/ocaml-cstruct")
                       (commit (string-append "v" version))))
                (file-name (git-file-name "ocaml-cstruct" version))
                (sha256
                 (base32
                  "0jj3whs8r3jc524i9bb67rffh7y7r157hjgvws0bkxijxpjzwkbk"))))
      (properties '()))))

(define-public ocaml-hex
  (package
    (name "ocaml-hex")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mirage/ocaml-hex")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c8nhibcwy0ykzca4jn3gqb8ylq21ff88y82gl60wyzijr64rn0q"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
     `(("ocaml-bigarray-compat" ,ocaml-bigarray-compat)
       ("cstruct" ,ocaml-cstruct)))
    (home-page "https://github.com/mirage/ocaml-hex/")
    (synopsis "Minimal library providing hexadecimal converters")
    (description "Hex is a minimal library providing hexadecimal converters.")
    (license license:isc)))

(define-public ocaml4.07-ezjsonm
  (package
    (name "ocaml4.07-ezjsonm")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mirage/ezjsonm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "064j9pzy01p3dv947khqyn7fkjbs3jmrqsg8limb4abnlaqxxs2s"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "ezjsonm"
       #:test-target "."
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (native-inputs
     `(("ocaml-alcotest" ,(package-with-ocaml4.07 ocaml-alcotest))))
    (propagated-inputs
     `(("ocaml-hex" ,(package-with-ocaml4.07 ocaml-hex))
       ("ocaml-jsonm" ,(package-with-ocaml4.07 ocaml-jsonm))
       ("ocaml-sexplib" ,(package-with-ocaml4.07 ocaml-sexplib))))
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
    (version "4.2.0")
    (home-page "https://github.com/mirage/ocaml-uri")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1bgkc66cq00mgnkz3i535srwzwc4cpdsv0mly5dzvvq33451xwf0"))))
    (build-system dune-build-system)
    (arguments '(#:package "uri"
                 #:test-target "."))
    (propagated-inputs
     (list ocaml-stringext ocaml-angstrom))
    (native-inputs
     (list ocaml-ounit ocaml-ppx-sexp-conv))
    (properties `((upstream-name . "uri")
                  (ocaml4.07-variant ,(delay ocaml4.07-uri))))
    (synopsis "RFC3986 URI/URL parsing library")
    (description "OCaml-uri is a library for parsing URI/URL in the RFC3986 format.")
    (license license:isc)))

(define-public ocaml4.07-uri
  (package
    (name "ocaml4.07-uri")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mirage/ocaml-uri")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ppbav41mszpjcl0zi3fyg958cxyfs57i7kvha4ds9ydn89bjmrh"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'update-deprecated
           (lambda _
             (substitute* "lib/uri.ml"
               (("Re.get") "Re.Group.get")))))
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (native-inputs
     `(("ocaml-ounit" ,(package-with-ocaml4.07 ocaml-ounit))
       ("ocaml-ppx-sexp-conv" ,(package-with-ocaml4.07 ocaml-ppx-sexp-conv))))
    (propagated-inputs
     `(("ocaml-re" ,(package-with-ocaml4.07 ocaml-re))
       ("ocaml-sexplib0" ,(package-with-ocaml4.07 ocaml-sexplib0))
       ("ocaml-stringext" ,(package-with-ocaml4.07 ocaml-stringext))))
    (properties `((upstream-name . "uri")))
    (home-page "https://github.com/mirage/ocaml-uri")
    (synopsis "RFC3986 URI/URL parsing library")
    (description "OCaml-uri is a library for parsing URI/URL in the RFC3986 format.")
    (license license:isc)))

(define-public ocaml-easy-format
  (package
    (name "ocaml-easy-format")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mjambon/easy-format")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fc95q2ypck6m6rv3kiawwilh5ac93v2hcp823mj608d5kj79xkb"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "easy-format"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'make-writable
           (lambda _
             (for-each
               (lambda (file)
                 (chmod file #o644))
               (find-files "." "."))
             #t)))))
    (home-page "https://github.com/mjambon/easy-format")
    (synopsis "Interface to the Format module")
    (description "Easy-format is a high-level and functional interface to the
Format module of the OCaml standard library.")
    (license license:bsd-3)))

(define-public ocaml4.07-piqilib
  (package
    (name "ocaml4.07-piqilib")
    (version "0.6.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alavrik/piqi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v04hs85xv6d4ysqxyv1dik34dx49yab9shpi4x7iv19qlzl7csb"))))
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
             #t))))
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib))
    (native-inputs
     (list which))
    (propagated-inputs
     `(("ocaml-xmlm" ,(package-with-ocaml4.07 ocaml-xmlm))
       ("ocaml-sedlex" ,(package-with-ocaml4.07 ocaml-sedlex))
       ("ocaml-easy-format" ,(package-with-ocaml4.07 ocaml-easy-format))
       ("ocaml-base64" ,(package-with-ocaml4.07 ocaml-base64))))
    (home-page "http://piqi.org")
    (synopsis "Data serialization and conversion library")
    (description "Piqilib is the common library used by the piqi command-line
tool and piqi-ocaml.")
    (license license:asl2.0)))

(define-public ocaml-uuidm
  (package
    (name "ocaml-uuidm")
    (version "0.9.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/uuidm/"
                                  "releases/uuidm-" version ".tbz"))
              (sha256
               (base32
                "1ivxb3hxn9bk62rmixx6px4fvn52s4yr1bpla7rgkcn8981v45r8"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags
       (list "build" "--tests" "true" "--with-cmdliner" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild opam))
    (propagated-inputs
     `(("cmdliner" ,ocaml-cmdliner)
       ("topkg" ,ocaml-topkg)))
    (home-page "https://erratique.ch/software/uuidm")
    (synopsis "Universally unique identifiers for OCaml")
    (description "Uuidm is an OCaml module implementing 128 bits universally
unique identifiers (UUIDs) version 3, 5 (named based with MD5, SHA-1 hashing)
and 4 (random based) according to RFC 4122.")
    (license license:isc)))

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
             (setenv "CONFIG_SHELL"
                     (search-input-file inputs "/bin/sh")))))))
    (inputs (list lablgtk))
    (properties `((upstream-name . "ocamlgraph")))
    (home-page "http://ocamlgraph.lri.fr/")
    (synopsis "Graph library for OCaml")
    (description "OCamlgraph is a generic graph library for OCaml.")
    (license license:lgpl2.1)))

(define-public ocaml4.07-piqi
  (package
    (name "ocaml4.07-piqi")
    (version "0.7.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/alavrik/piqi-ocaml")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1913jpsb8mvqi8609j4g4sm5jhg50dq0xqxgy8nmvknfryyc89nm"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:make-flags
       ,#~(list (string-append "DESTDIR=" #$output)
                (string-append "SHELL="
                               #+(file-append (canonical-package bash-minimal)
                                              "/bin/sh")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (delete 'configure))
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib))
    (native-inputs
     (list which protobuf)) ; for tests
    (propagated-inputs
     `(("ocaml-num" ,(package-with-ocaml4.07 ocaml-num))
       ("ocaml-piqilib" ,ocaml4.07-piqilib)
       ("ocaml-stdlib-shims" ,(package-with-ocaml4.07 ocaml-stdlib-shims))))
    (home-page "https://github.com/alavrik/piqi-ocaml")
    (synopsis "Protocol serialization system for OCaml")
    (description "Piqi is a multi-format data serialization system for OCaml.
It provides a uniform interface for serializing OCaml data structures to JSON,
XML and Protocol Buffers formats.")
    (license license:asl2.0)))

(define-public bap
  (package
    (name "bap")
    (version "2.0.0")
    (home-page "https://github.com/BinaryAnalysisPlatform/bap")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url home-page)
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lb9xkfp67wjjqr75p6krivmjra7l5673236v9ny4gp0xi0755bk"))))
   (build-system ocaml-build-system)
   (native-inputs
    `(("ocaml-oasis" ,(package-with-ocaml4.07 ocaml-oasis))
      ("clang" ,clang-3.8)
      ("ocaml-ounit" ,(package-with-ocaml4.07 ocaml-ounit))))
   (propagated-inputs
    `(("camlzip" ,(package-with-ocaml4.07 camlzip))
      ("ocaml-bitstring" ,(package-with-ocaml4.07 ocaml-bitstring))
      ("ocaml-cmdliner" ,(package-with-ocaml4.07 ocaml-cmdliner))
      ("ocaml-core-kernel" ,ocaml4.07-core-kernel)
      ("ocaml-ezjsonm" ,ocaml4.07-ezjsonm)
      ("ocaml-fileutils" ,(package-with-ocaml4.07 ocaml-fileutils))
      ("ocaml-frontc" ,(package-with-ocaml4.07 ocaml-frontc))
      ("ocaml-graph" ,(package-with-ocaml4.07 ocaml-graph))
      ("ocaml-ocurl" ,(package-with-ocaml4.07 ocaml-ocurl))
      ("ocaml-piqi" ,ocaml4.07-piqi)
      ("ocaml-ppx-jane" ,ocaml4.07-ppx-jane)
      ("ocaml-utop" ,ocaml4.07-utop)
      ("ocaml-uuidm" ,(package-with-ocaml4.07 ocaml-uuidm))
      ("ocaml-uri" ,ocaml4.07-uri)
      ("ocaml-zarith" ,(package-with-ocaml4.07 ocaml-zarith))))
   (inputs
    (list gmp llvm-3.8 ncurses))
   (arguments
    `(#:use-make? #t
      #:phases
      (modify-phases %standard-phases
        (add-before 'configure 'fix-ncurses
          (lambda _
            (substitute* "oasis/llvm"
              (("-lcurses") "-lncurses"))
            #t))
        (replace 'configure
          (lambda* (#:key outputs inputs #:allow-other-keys)
            ;; add write for user, to prevent a failure in the install phase
            (for-each
              (lambda (file)
                (let ((stat (stat file)))
                  (chmod file (+ #o200 (stat:mode stat)))))
              (find-files "." "."))
            (invoke "./configure" "--prefix"
                    (assoc-ref outputs "out")
                    "--libdir"
                    (string-append
                      (assoc-ref outputs "out")
                      "/lib/ocaml/site-lib")
                    "--with-llvm-version=3.8"
                    "--with-llvm-config=llvm-config"
                    "--enable-everything"))))
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib))
   (synopsis "Binary Analysis Platform")
   (description "Binary Analysis Platform is a framework for writing program
analysis tools, that target binary files.  The framework consists of a plethora
of libraries, plugins, and frontends.  The libraries provide code reusability,
the plugins facilitate extensibility, and the frontends serve as entry points.")
   (license license:expat)))

(define-public ocaml-camomile
  (package
    (name "ocaml-camomile")
    (version "1.0.2")
    (home-page "https://github.com/yoriyuki/Camomile")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/releases/download/" version
                                  "/camomile-" version ".tbz"))
              (sha256
               (base32
                "0chn7ldqb3wyf95yhmsxxq65cif56smgz1mhhc7m0dpwmyq1k97h"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "camomile-test"
       #:tests? #f ; Tests fail, see https://github.com/yoriyuki/Camomile/issues/82
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-usr-share
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("Camomile/dune" "configure.ml")
               (("/usr/share") (string-append (assoc-ref outputs "out") "/share")))
             #t)))))
    (synopsis "Comprehensive Unicode library")
    (description "Camomile is a Unicode library for OCaml.  Camomile provides
Unicode character type, UTF-8, UTF-16, UTF-32 strings, conversion to/from about
200 encodings, collation and locale-sensitive case mappings, and more.  The
library is currently designed for Unicode Standard 3.2.")
    ;; with an exception for linked libraries to use a different license
    (license license:lgpl2.0+)))

(define-public ocaml-charinfo-width
  ;; Add LICENSE file and Dune tests
  (let ((commit "20aaaa6dca8f1e0b1ace55b6f2a8ba5e5910b620"))
    (package
      (name "ocaml-charinfo-width")
      (version (git-version "1.1.0" "1" commit))
      (home-page "https://github.com/kandu/charinfo_width/")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "04gil5hxm2jax9paw3i24d8zyzhyl5cphzfyryvy2lcrm3c485q0"))))
      (build-system dune-build-system)
      (propagated-inputs
       (list ocaml-result ocaml-camomile))
      (native-inputs
       (list ocaml-ppx-expect))
      (properties
       `((upstream-name . "charInfo_width")))
      (synopsis "Determine column width for a character")
      (description "This module implements purely in OCaml a character width
function that follows the prototype of POSIX's wcwidth.")
      (license license:expat))))

(define-public ocaml-zed
  (package
    (name "ocaml-zed")
    (version "3.1.0")
    (home-page "https://github.com/ocaml-community/zed")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04vr1a94imsghm98iigc35rhifsz0rh3qz2qm0wam2wvp6vmrx0p"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
     `(("ocaml-charInfo-width" ,ocaml-charinfo-width)
       ("ocaml-camomile" ,ocaml-camomile)
       ("ocaml-react" ,ocaml-react)))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-zed))))
    (synopsis "Abstract engine for text edition in OCaml")
    (description
     "This module provides an abstract engine for text edition.  It can be
used to write text editors, edition widgets, readlines, and more.  The module
Zed uses Camomile to fully support the Unicode specification, and implements
an UTF-8 encoded string type with validation, and a rope datastructure to
achieve efficient operations on large Unicode buffers.  Zed also features a
regular expression search on ropes.  To support efficient text edition
capabilities, Zed provides macro recording and cursor management facilities.")
    (license license:bsd-3)))

(define-public ocaml4.07-zed
  (package-with-ocaml4.07
   (package
     (inherit ocaml-zed)
     (version "2.0.3")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/ocaml-community/zed")
                     (commit version)))
               (file-name (git-file-name "ocaml4.07-zed" version))
               (sha256
                (base32
                 "0pa9awinqr0plp4b2az78dwpvh01pwaljnn5ydg8mc6hi7rmir55"))))
     (properties '()))))

(define-public ocaml-lambda-term
  (package
    (name "ocaml-lambda-term")
    (version "3.1.0")
    (home-page "https://github.com/ocaml-community/lambda-term")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k0ykiz0vhpyyj9fkss29ajas4fh1xh449j702xkvayqipzj1mkg"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
     (list ocaml-lwt
           ocaml-lwt-log
           ocaml-react
           ocaml-zed
           ocaml-camomile
           ocaml-lwt-react
           ocaml-mew-vi))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-lambda-term))))
    (synopsis "Terminal manipulation library for OCaml")
    (description "Lambda-Term is a cross-platform library for manipulating the
terminal.  It provides an abstraction for keys, mouse events, colors, as well as
a set of widgets to write curses-like applications.  The main objective of
Lambda-Term is to provide a higher level functional interface to terminal
manipulation than, for example, ncurses, by providing a native OCaml interface
instead of bindings to a C library.")
    (license license:bsd-3)))

(define-public ocaml4.07-lambda-term
  (package-with-ocaml4.07
   (package
     (inherit ocaml-lambda-term)
     (version "2.0.2")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/ocaml-community/lambda-term")
              (commit version)))
        (file-name (git-file-name "ocaml4.07-lambda-term" version))
        (sha256
         (base32 "0zcjy6fvf0d3i2ssz96asl889n3r6bplyzk7xvb2s3dkxbgcisyy"))))
     (propagated-inputs
      `(("ocaml-lwt" ,ocaml-lwt)
        ("ocaml-lwt-log" ,ocaml-lwt-log)
        ("ocaml-lwt-react" ,ocaml-lwt-react)
        ("ocaml-zed" ,ocaml-zed)))
     (properties '()))))

(define-public ocaml-utop
  (package
    (name "ocaml-utop")
    (version "2.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-community/utop")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mdpqc1b67p5rm2jsbwy0gjjgdlfqcakjyh1cwdj959ykz4zy9ld"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (native-inputs
     (list ocaml-cppo))
    (propagated-inputs
     (list ocaml-lambda-term ocaml-lwt ocaml-lwt-react ocaml-camomile
           ocaml-react))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-utop))))
    (home-page "https://github.com/ocaml-community/utop")
    (synopsis "Improved interface to the OCaml toplevel")
    (description "UTop is an improved toplevel for OCaml.  It can run in a
terminal or in Emacs.  It supports line editing, history, real-time and context
sensitive completion, colors, and more.")
    (license license:bsd-3)))

(define-public ocaml4.07-utop
  (package-with-ocaml4.07
   (package
     (inherit ocaml-utop)
     (version "2.4.3")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/ocaml-community/utop")
              (commit version)))
        (file-name (git-file-name "ocaml4.07-utop" version))
        (sha256
         (base32 "1bl4943qpi3qy152dbdm5glhx19zsiylmn4rcxi8l66g58hikyjp"))))
     (propagated-inputs
      `(("ocaml-lambda-term" ,ocaml-lambda-term)
        ("ocaml-lwt" ,ocaml-lwt)
        ("ocaml-react" ,ocaml-react)
        ("ocaml-camomile" ,ocaml-camomile)
        ("ocaml-zed" ,ocaml-zed)))
     (properties '()))))

(define-public ocaml-integers
  (package
    (name "ocaml-integers")
    (version "0.6.0")
    (home-page "https://github.com/ocamllabs/ocaml-integers")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07qk7dgxykn528qvhfwa0j402yr58y1y84ivpi1ji1lvk0qddxcs"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (propagated-inputs
     (list ocaml-stdlib-shims))
    (synopsis "Various signed and unsigned integer types for OCaml")
    (description "The ocaml-integers library provides a number of 8-, 16-, 32-
and 64-bit signed and unsigned integer types, together with aliases such as
long and size_t whose sizes depend on the host platform.")
    (license license:expat)))

(define-public ocaml-ctypes
  (package
   (name "ocaml-ctypes")
   (version "0.20.0")
   (home-page "https://github.com/ocamllabs/ocaml-ctypes")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url home-page)
                    (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "13mq6i08f217fj2lq55psji453nmqp136sjxhg25hjb2z0gv9gky"))))
   (build-system ocaml-build-system)
   (arguments
    `(#:make-flags
      (list (string-append "INSTALL_HEADERS = $(wildcard $($(PROJECT).dir)/*.h)"))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'make-writable
          (lambda _
            (for-each make-file-writable
                      (find-files "."))))
        (delete 'configure))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("ounit" ,ocaml-ounit)
      ("lwt" ,ocaml-lwt)))
   (propagated-inputs
    `(("bigarray-compat" ,ocaml-bigarray-compat)
      ("integers" ,ocaml-integers)))
   (inputs
    (list libffi))
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
        (delete 'configure)
        (add-before 'build 'fix-for-guix
          (lambda _
            (substitute* "src/ocb_stubblr.ml"
              ;; Do not fail when opam is not present or initialized
              (("error_msgf \"error running opam\"") "\"\"")
              ;; Guix doesn't have cc, but it has gcc
              (("\"cc\"") "\"gcc\""))
            #t)))))
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
    (version "0.9.7")
    (home-page "https://erratique.ch/software/tsdl")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/releases/tsdl-"
                                  version ".tbz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zwv0ixkigh1gzk5n49rwvz2f2m62jdkkqg40j7dclg4gri7691f"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags '("build")
       #:tests? #f; tests require a display device
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild ocaml-astring opam pkg-config))
    (inputs
     `(("topkg" ,ocaml-topkg)
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
             (url "https://github.com/deducteam/dedukti")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0frl3diff033i4fmq304b8wbsdnc9mvlhmwd7a3zd699ng2lzbxb"))))
    (inputs
     `(("menhir" ,ocaml-menhir)))
    (native-inputs
     (list ocamlbuild))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       ,#~(modify-phases %standard-phases
            (delete 'configure)
            (replace 'build
              (lambda _
                (invoke "make")))
            (replace 'check
              (lambda _
                (invoke "make" "tests")))
            (add-before 'install 'set-binpath
              ;; Change binary path in the makefile
              (lambda _
                (substitute* "GNUmakefile"
                  (("BINDIR = (.*)$")
                   (string-append "BINDIR = " #$output "/bin")))))
            (replace 'install
              (lambda _
                (invoke "make" "install"))))))
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
       (list dedukti))
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
       (list emacs-dedukti-mode emacs-flycheck))
      (synopsis "Flycheck integration for the dedukti language")
      (description "This package provides a frontend for Flycheck to perform
syntax checking on dedukti files.")
      (license license:cecill-b))))

(define-public ocaml-jst-config
  (package
    (name "ocaml-jst-config")
    (version "0.14.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/jst-config")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0sak7rrix8vd2bscbjxbxi1d9jyd27ik59dpybfxhzhsn9s1wllv"))))
    (build-system dune-build-system)
    (arguments '(#:tests? #f))           ; no tests
    (propagated-inputs
      (list ocaml-base ocaml-ppx-assert ocaml-stdio dune-configurator))
    (home-page "https://github.com/janestreet/jst-config")
    (synopsis "Compile-time configuration for Jane Street libraries")
    (description "Defines compile-time constants used in Jane Street libraries
such as Base, Core, and Async.  This package has an unstable interface; it is
intended only to share configuration between different packages from Jane
Street.  Future updates may not be backward-compatible, and we do not
recommend using this package directly.")
    (license license:expat)))

(define-public ocaml-jane-street-headers
  (package
    (name "ocaml-jane-street-headers")
    (version "0.14.0")
    (source
     (janestreet-origin
      "jane-street-headers" version
      "028yxb4h3iy025iy89v8653m5brh7flrjshghs4x99pd690pmfs7"))
    (build-system dune-build-system)
    (arguments '(#:tests? #f))           ; no tests
    (home-page "https://github.com/janestreet/jane-street-headers")
    (synopsis "Jane Street C header files")
    (description "C header files shared between the various Jane Street
packages.")
    (license license:expat)))

(define-public ocaml-time-now
  (package
    (name "ocaml-time-now")
    (version "0.14.0")
    (source
     (janestreet-origin
      "time_now" version
      "0hkn2jw4dz5gflnsblskl5wp6z7zbrahwjmaxmsskfviwjg82cqh"))
    (build-system dune-build-system)
    (arguments '(#:tests? #f))           ; no tests
    (propagated-inputs
     (list ocaml-base ocaml-jane-street-headers ocaml-jst-config
           ocaml-ppx-base ocaml-ppx-optcomp))
    (properties `((upstream-name . "time_now")))
    (home-page
     "https://github.com/janestreet/time_now")
    (synopsis "Reports the current time")
    (description
     "Provides a single function to report the current time in nanoseconds
since the start of the Unix epoch.")
    (license license:expat)))

(define-public ocaml-ppx-inline-test
  (package
    (name "ocaml-ppx-inline-test")
    (version "0.14.1")
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
         "1ajdna1m9l1l3nfigyy33zkfa3yarfr6s086jdw2pcfwlq1fhhl4"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ;see home page README for further information
    (propagated-inputs
     (list ocaml-base
           ocaml-migrate-parsetree
           ocaml-compiler-libs
           ocaml-sexplib0
           ocaml-stdio
           ocaml-ppxlib
           ocaml-time-now))
    (properties `((upstream-name . "ppx_inline_test")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-inline-test))))
    (synopsis "Syntax extension for writing in-line tests in ocaml code")
    (description "This package contains a syntax extension for writing
in-line tests in ocaml code.  It is part of Jane Street's PPX rewriters
collection.")
    (license license:expat)))

(define-public ocaml4.07-ppx-inline-test
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-inline-test)
     (name "ocaml-ppx-inline-test")
     (version "0.12.0")
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
          "0nyz411zim94pzbxm2l2v2l9jishcxwvxhh142792g2s18r4vn50"))))
    (propagated-inputs
     `(("ocaml-base" ,ocaml-base)
       ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
       ("ocaml-compiler-libs" ,ocaml-compiler-libs)
       ("ocaml-sexplib0" ,ocaml-sexplib0)
       ("ocaml-stdio" ,ocaml-stdio)
       ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_inline_test"))))))

(define-public ocaml-bindlib
  (package
    (name "ocaml-bindlib")
    (version "5.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rlepigre/ocaml-bindlib")
             (commit (string-append "ocaml-bindlib_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1f8kr81w8vsi4gv61xn1qbc6zrzkjp8l9ix0942vjh4gjxc74v75"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
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
     (list ocamlbuild ocaml-findlib))
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
    (version "3.0.0")
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
         "1vi58zdxchpw6ai0bz9h2ggcmg8kv57yk6qbx82lh47s5wb3mz5y"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
     (list ocaml-stdlib-shims))
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
   (version "1.2.1")
   (home-page "https://github.com/mjambon/biniou")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0x2kiy809n1j0yf32l7hj102y628jp5jdrkbi3z7ld8jq04h1790"))))
   (build-system dune-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-before 'build 'make-writable
          (lambda _ (for-each make-file-writable (find-files "." ".")))))))
   (inputs
    (list ocaml-easy-format))
   (native-inputs
    (list which))
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
    (version "1.7.0")
    (home-page "https://github.com/ocaml-community/yojson")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0zncsw8bgbkh1pfvfc7nh628hfj84lgx6jwhp9ashj3z1z0w3xjn"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
     (list ocaml-biniou ocaml-easy-format))
    (native-inputs
     (list ocaml-alcotest ocaml-cppo))
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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'upgrade
           (lambda _
             (invoke "dune" "upgrade")
             #t)))))
    (inputs
     (list ocaml-fmt ocaml-astring ocaml-logs ocaml-cmdliner))
    (synopsis
     "CRAM-testing framework for testing command line applications")
    (description "CRAM is a is functional testing framework for command line
applications.  @code{craml} is freely inspired by the
Mercurial's @code{https://www.selenic.com/blog/?p=663, unified test
format}.  @code{craml} is released as a single binary (called @code{craml}).")
    (license license:isc)))

(define-public ocaml-dot-merlin-reader
  (package
    (name "ocaml-dot-merlin-reader")
    (version "4.4-413")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/merlin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wijg1vh2q6yr46vkv34vvksligd0ajl4hv7m6qbz3ywqr8akg23"))))
    (build-system dune-build-system)
    (arguments '(#:package "dot-merlin-reader"
                 #:tests? #f))          ; no tests
    (inputs
     (list ocaml-yojson ocaml-csexp ocaml-result))
    (home-page "https://ocaml.github.io/merlin/")
    (synopsis "Reads config files for @code{ocaml-merlin}")
    (description "@code{ocaml-dot-merlin-reader} is an external reader for
@code{ocaml-merlin} configurations.")
    (license license:expat)))

(define-public ocaml-merlin
  (package
    (inherit ocaml-dot-merlin-reader)
    (name "ocaml-merlin")
    (arguments
     '(#:package "merlin"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "dune" "runtest" "-p" "merlin,dot-merlin-reader")))))))
    (inputs
     (list ocaml-yojson ocaml-csexp ocaml-result))
    (native-inputs
     (list ocaml-dot-merlin-reader ; required for tests
           ocaml-mdx jq))
    (synopsis "Context sensitive completion for OCaml in Vim and Emacs")
    (description "Merlin is an editor service that provides modern IDE
features for OCaml.  Emacs and Vim support is provided out-of-the-box.
External contributors added support for Visual Studio Code, Sublime Text and
Atom.")
    (license license:expat)))

(define-public ocaml-gsl
  (package
    (name "ocaml-gsl")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/mmottl/gsl-ocaml/releases/download/"
         version "/gsl-" version ".tbz"))
       (sha256
        (base32
         "1l5zkkkg8sglsihrbf10ivq9s8xzl1y6ag89i4jqpnmi4m43fy34"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-gsl-directory
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/config/discover.ml"
               (("/usr") (assoc-ref inputs "gsl"))))))))
    (inputs
     (list gsl))
    (propagated-inputs
     (list ocaml-base ocaml-stdio))
    (home-page "https://mmottl.github.io/gsl-ocaml")
    (synopsis "Bindings to the GNU Scientific Library")
    (description
     "GSL-OCaml is an interface to the @dfn{GNU scientific library} (GSL) for
the OCaml language.")
    (license license:gpl3+)))

(define-public ocaml4.07-gsl-1
  (package-with-ocaml4.07
    (package
      (inherit ocaml-gsl)
      (version "1.19.3")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/mmottl/gsl-ocaml"
                                    "/releases/download/v"
                                    version "/gsl-ocaml-" version ".tar.gz"))
                (sha256
                 (base32
                  "0nzp43hp8pbjqkrxnwp5lgjrabxayf61h18fjaydi0s5faq6f3xh"))))
      (build-system ocaml-build-system)
      (inputs
       `(("gsl" ,gsl-static)))
      (native-inputs
       `(("ocamlbuild" ,ocamlbuild)))
      (arguments '())
      (propagated-inputs '()))))

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
     (list automake ocaml
           (@@ (gnu packages base) which)))
    (propagated-inputs
     (list ocaml-num z3))
    (arguments
     `(#:configure-flags (list "--with-z3")
       #:make-flags (list "QUIET=")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'make-deterministic
           (lambda _
             (substitute* "Makefile.in"
               (("`date`") "no date for reproducibility"))))
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
               (("nums.cma") "num.cma num_core.cma")
               (("= \\$\\(FUNCTORYLIB\\)")
                (string-append "= -I "
                               (assoc-ref inputs "ocaml-num")
                               "/lib/ocaml/site-lib/num/core -I "
                               (assoc-ref inputs "ocaml-num")
                               "/lib/ocaml/site-lib/num"
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
    (version "0.14.0")
    (home-page "https://github.com/janestreet/sexplib0")
    (source
     (janestreet-origin "sexplib0" version
                        "0adrc0r1vvvr41dcpj8jwkzh1dfgqf0mks9xlnnskqfm3a51iavg"))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)) ;no tests
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-sexplib0))))
    (synopsis "Library containing the definition of S-expressions and some
base converters")
    (description "Part of Jane Street's Core library The Core suite of
libraries is an industrial strength alternative to OCaml's standard library
that was developed by Jane Street, the largest industrial user of OCaml.")
    (license license:expat)))

(define-public ocaml4.07-sexplib0
  (package-with-ocaml4.07
   (package
     (inherit ocaml-sexplib0)
     (name "ocaml-sexplib0")
     (version "0.11.0")
     (source
      (janestreet-origin "sexplib0" version
                         "1p06p2s7p9xsjn0z9qicniv1ai54d8sj11k8j633di2mm7jzxpin"))
     (arguments `(#:tests? #f))         ; no tests
     (properties '()))))

(define-public ocaml-parsexp
  (package
    (name "ocaml-parsexp")
    (version "0.14.2")
    (home-page "https://github.com/janestreet/parsexp")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url home-page)
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14yi0licf1cp6b7qny5pz6dmlalqdksx0m0kzcrwi6byjxwjkbi9"))))
    (build-system dune-build-system)
    (inputs
     (list ocaml-sexplib0 ocaml-base))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-parsexp))))
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

(define-public ocaml4.07-parsexp
  (package-with-ocaml4.07
   (package
     (inherit ocaml-parsexp)
     (name "ocaml-parsexp")
     (version "0.11.0")
     (source
      (janestreet-origin "parsexp" version
                         "11a30zkfgbi6pb4whq22k1zc8ghdp9bwxl5s5cdlmx1z8s4yxsf0"))
     (properties '()))))

(define-public ocaml-sexplib
  (package
    (name "ocaml-sexplib")
    (version "0.14.0")
    (home-page "https://github.com/janestreet/sexplib")
    (source
     (janestreet-origin "sexplib" version
                        "12rlnc6fcrjfdn3gs2agi418sj54ighhs6dfll37zcv7mgywblm2"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-num ocaml-parsexp ocaml-sexplib0))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-sexplib))))
    (synopsis
     "Library for serializing OCaml values to and from S-expressions")
    (description
     "This package is part of Jane Street's Core library.  Sexplib contains
functionality for parsing and pretty-printing s-expressions.")
    (license license:expat)))

(define-public ocaml4.07-sexplib
  (package-with-ocaml4.07
   (package
     (inherit ocaml-sexplib)
     (version "0.11.0")
     (source
      (janestreet-origin "sexplib" version
                         "0ksx62zsxhz8xmdrsn41n2hbc2qbyh3bxxc6946xisvgwh42h3q3"))
     (properties '()))))

(define-public ocaml-base
  (package
    (name "ocaml-base")
    (version "0.14.3")
    (home-page "https://github.com/janestreet/base")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/base")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1cqpdpvhr4zns1lpdm2w0p6q400mc1z91hd716mb062ng83n2nsf"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-sexplib0))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-base))))
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

(define-public ocaml4.07-base
  (package-with-ocaml4.07
   (package
     (inherit ocaml-base)
     (name "ocaml-base")
     (version "0.11.1")
     (source
      (origin
        ;; version 0.11.1 is not released on ocaml.janestreet.org.
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/janestreet/base.git")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0j6xb4265jr41vw4fjzak6yr8s30qrnzapnc6rl1dxy8bjai0nir"))))
     (properties '()))))

(define-public ocaml-compiler-libs
  (package
    (name "ocaml-compiler-libs")
    (version "0.12.4")
    (home-page "https://github.com/janestreet/ocaml-compiler-libs")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00if2f7j9d8igdkj4rck3p74y17j6b233l91mq02drzrxj199qjv"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)) ;no tests
    (properties `((upstream-name . "ocaml-compiler-libs")
                  (ocaml4.07-variant . ,(delay ocaml4.07-compiler-libs))))
    (synopsis "Compiler libraries repackaged")
    (description "This package simply repackages the OCaml compiler libraries
so they don't expose everything at toplevel.  For instance, @code{Ast_helper}
is now @code{Ocaml_common.Ast_helper}.")
    (license license:expat)))

(define-public ocaml4.07-compiler-libs
  (package-with-ocaml4.07
   (package
     (inherit ocaml-compiler-libs)
     (name "ocaml-compiler-libs")
     (version "0.11.0")
     (home-page "https://github.com/janestreet/ocaml-compiler-libs")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url home-page)
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "03jds7bszh8wwpfwxb3dg0gyr1j1872wxwx1xqhry5ir0i84bg0s"))))
     (properties `((upstream-name . "ocaml-compiler-libs"))))))

(define-public ocaml-stdio
  (package
    (name "ocaml-stdio")
    (version "0.14.0")
    (home-page "https://github.com/janestreet/stdio")
    (source
     (janestreet-origin "stdio" version
                        "1hj5hraprqy2i90a690l11yjszvb99j818q3d684ryx6p2lddk0l"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-sexplib0))
    (arguments `(#:tests? #f)) ;no tests
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-stdio))))
    (synopsis "Standard IO library for OCaml")
    (description
     "Stdio implements simple input/output functionalities for OCaml.  It
re-exports the input/output functions of the OCaml standard libraries using
a more consistent API.")
    (license license:expat)))

(define-public ocaml4.07-stdio
  (package-with-ocaml4.07
   (package
     (inherit ocaml-stdio)
     (version "0.11.0")
     (source
      (janestreet-origin "stdio" version
                         "0pqbp2wy5fgmc38irwvmj9nlcvclb1ix1mp4y7l39bgvvlz303h9"))
     (properties '()))))

(define-public ocaml-ppx-deriving
  (package
    (name "ocaml-ppx-deriving")
    (version "5.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-ppx/ppx_deriving")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wqcnw4wi6pfjjhixpakckm03dpj990259za432804471a6spm2j"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
     (list ocaml-ppx-derivers ocaml-ppxlib ocaml-result))
    (native-inputs
     (list ocaml-cppo ocaml-ounit2))
    (properties `((upstream-name . "ppx_deriving")))
    (home-page "https://github.com/ocaml-ppx/ppx_deriving")
    (synopsis "Type-driven code generation for OCaml")
    (description
     "Ppx_deriving provides common infrastructure for generating code based
on type definitions, and a set of useful plugins for common tasks.")
    (license license:expat)))

(define-public ocaml-ppx-derivers
  (package
    (name "ocaml-ppx-derivers")
    (version "1.2.1")
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
         "0yqvqw58hbx1a61wcpbnl9j30n495k23qmyy2xwczqs63mn2nkpn"))))
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
    (version "0.24.0")
    (home-page "https://github.com/ocaml-ppx/ppxlib")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nkkdvqifa36hxj6msd74ld4dfd749d6d9ygfj7zsm328rqvpqf2"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base
           ocaml-compiler-libs
           ocaml-migrate-parsetree
           ocaml-stdlib-shims
           ocaml-ppx-derivers
           ocaml-stdio
           ocaml-result
           ocaml-sexplib0))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-ppxlib))))
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

(define-public ocaml4.07-ppxlib
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppxlib)
     (name "ocaml-ppxlib")
     (version "0.6.0")
     (home-page "https://github.com/ocaml-ppx/ppxlib")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url home-page)
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0my9x7sxb329h0lzshppdaawiyfbaw6g5f41yiy7bhl071rnlvbv"))))
     (build-system dune-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'set-topfind
            (lambda* (#:key inputs #:allow-other-keys)
              ;; add the line #directory ".." at the top of each file
              ;; using #use "topfind";; to be able to find topfind
              (let* ((findlib-path (assoc-ref inputs "findlib"))
                     (findlib-libdir
                      (string-append findlib-path "/lib/ocaml/site-lib")))
                (substitute* '("test/base/test.ml"
                               "test/code_path/test.ml"
                               "test/deriving/test.ml"
                               "test/driver/attributes/test.ml"
                               "test/driver/non-compressible-suffix/test.ml"
                               "test/driver/transformations/test.ml")
                  (("#use \"topfind\";;" all)
                   (string-append "#directory \"" findlib-libdir "\"\n"
                                  all))))
              #t)))))
     (properties '()))))

(define-public ocaml-ppx-compare
  (package
    (name "ocaml-ppx-compare")
    (version "0.14.0")
    (source
     (janestreet-origin "ppx_compare" version
                        "0mqxa2s194nif7x4fjn1p5gd9i3bakr8nv27gf8x1g5nmi8q9pmp"))
    (build-system dune-build-system)
    (arguments
     ;; Tests are currenlty failing
     ;; (see https://github.com/janestreet/ppx_compare/issues/10)
     '(#:tests? #f))
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_compare")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-compare))))
    (home-page "https://github.com/janestreet/ppx_compare")
    (synopsis "Generation of comparison functions from types")
    (description "Generation of fast comparison functions from type expressions
and definitions.  Ppx_compare is a ppx rewriter that derives comparison functions
from type representations.  The scaffolded functions are usually much faster
than ocaml's Pervasives.compare.  Scaffolding functions also gives you more
flexibility by allowing you to override them for a specific type and more safety
by making sure that you only compare comparable values.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-compare
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-compare)
     (name "ocaml-ppx-compare")
     (version "0.11.1")
     (home-page "https://github.com/janestreet/ppx_compare")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url home-page)
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "06bq4m1bsm4jlx4g7wh5m99qky7xm4c2g52kaz6pv25hdn5agi2m"))))
     (properties `((upstream-name . "ppx_compare"))))))

(define-public ocaml-fieldslib
  (package
    (name "ocaml-fieldslib")
    (version "0.14.0")
    (source (janestreet-origin
             "fieldslib" version
             "10n5y376fb5jgqk9h8vq158rm1b36h9lzh6p11q33h6xgvb1v6n3"))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)) ; No tests
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "fieldslib")
                  (ocaml4.07-variant . ,(delay ocaml4.07-fieldslib))))
    (home-page "https://github.com/janestreet/fieldslib")
    (synopsis "Syntax extension to record fields")
    (description "Syntax extension to define first class values representing
record fields, to get and set record fields, iterate and fold over all fields
of a record and create new record values.")
    (license license:asl2.0)))

(define-public ocaml4.07-fieldslib
  (package-with-ocaml4.07
   (package
     (inherit ocaml-fieldslib)
     (version "0.11.0")
     (source (janestreet-origin
              "fieldslib" version
              "12948pzxrl360lybm9fzyvplgcl87zjbn4m3sk1aw75zk85p1388"))
     (properties `((upstream-name . "fieldslib"))))))

(define-public ocaml-variantslib
  (package
    (name "ocaml-variantslib")
    (version "0.14.0")
    (source
     (janestreet-origin "variantslib" version
              "11zp27gh282dx9ifbhcp6i7fkc97fvk8amaj58mf1g1hwklc0lm3"))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "variantslib")
                  (ocaml4.07-variant . ,(delay ocaml4.07-variantslib))))
    (home-page "https://github.com/janestreet/variantslib")
    (synopsis "OCaml variants as first class values")
    (description "The Core suite of libraries is an alternative to OCaml's
standard library.")
    (license license:asl2.0)))

(define-public ocaml4.07-variantslib
  (package-with-ocaml4.07
   (package
     (inherit ocaml-variantslib)
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
     (properties `((upstream-name . "variantslib"))))))

(define-public ocaml-ppx-fields-conv
  (package
    (name "ocaml-ppx-fields-conv")
    (version "0.14.2")
    (home-page "https://github.com/janestreet/ppx_fields_conv")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zwirwqry24b48bg7d4yc845hvcirxyymzbw95aaxdcck84d30n8"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-fieldslib ocaml-migrate-parsetree
           ocaml-ppxlib))
    (properties `((upstream-name . "ppx_fields_conv")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-fields-conv))))
    (synopsis "Generation of accessor and iteration functions for ocaml records")
    (description "Ppx_fields_conv is a ppx rewriter that can be used to define
first class values representing record fields, and additional routines, to get
and set record fields, iterate and fold over all fields of a record and create
new record values.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-fields-conv
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-fields-conv)
    (version "0.11.0")
    (source (janestreet-origin
             "ppx_fields_conv" version
             "07zrd3qky2ppbfl55gpm90rvqa5860xgwcsvihrjmkrw6d0jirkc"))
    (properties `((upstream-name . "ppx_fields_conv"))))))

(define-public ocaml-ppx-sexp-conv
  (package
    (name "ocaml-ppx-sexp-conv")
    (version "0.14.3")
    (home-page "https://github.com/janestreet/ppx_sexp_conv")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dbri9d00ydi0dw1cavswnqdmhjaaz80vap29ns2lr6mhhlvyjmj"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_sexp_conv")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-sexp-conv))))
    (synopsis "Generation of S-expression conversion functions from type definitions")
    (description "This package generates S-expression conversion functions from type
definitions.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-sexp-conv
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-sexp-conv)
     (name "ocaml-ppx-sexp-conv")
     (version "0.11.2")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/janestreet/ppx_sexp_conv")
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0pqwnqy1xp309wvdcaax4lg02yk64lq2w03mbgfvf6ps5ry4gis9"))))
     (properties `((upstream-name . "ppx_sexp_conv"))))))

(define-public ocaml-ppx-variants-conv
  (package
    (name "ocaml-ppx-variants-conv")
    (version "0.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/janestreet/ppx_variants_conv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1p11fiz4m160hs0xzg4g9rxchp053sz3s3d1lyciqixad1xi47a4"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-variantslib ocaml-migrate-parsetree
           ocaml-ppxlib))
    (properties
     `((upstream-name . "ppx_variants_conv")
       (ocaml4.07-variant . ,(delay ocaml4.07-ppx-variants-conv))))
    (home-page
     "https://github.com/janestreet/ppx_variants_conv")
    (synopsis "Generation of accessor and iteration functions for OCaml variant types")
    (description
     "This package generates accessors and iteration functions for OCaml
variant types.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-variants-conv
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-variants-conv)
     (name "ocaml-ppx-variants-conv")
     (version "0.11.1")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/janestreet/ppx_variants_conv")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1yc0gsds5m2nv39zga8nnrca2n75rkqy5dz4xj1635ybz20hhbjd"))))
    (properties `((upstream-name . "ppx_variants_conv"))))))

(define-public ocaml-ppx-custom-printf
  (package
    (name "ocaml-ppx-custom-printf")
    (version "0.14.1")
    (home-page "https://github.com/janestreet/ppx_custom_printf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0c1m65kn27zvwmfwy7kk46ga76yw2a3ik9jygpy1b6nn6pi026w9"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-ppx-sexp-conv ocaml-migrate-parsetree
           ocaml-ppxlib))
    (properties `((upstream-name . "ppx_custom_printf")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-custom-printf))))
    (synopsis "Printf-style format-strings for user-defined string conversion")
    (description "Extensions to printf-style format-strings for user-defined
string conversion.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-custom-printf
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-custom-printf)
     (version "0.11.0")
     (source
      (janestreet-origin
       "ppx_custom_printf" version
       "11b73smf3g3bpd9lg014pr4rx285nk9mnk6g6464ph51jv0sqzhj"))
     (properties `((upstream-name . "ppx_custom_printf"))))))

(define-public ocaml4.07-bin-prot
  (package
    (name "ocaml4.07-bin-prot")
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
      `(("ocaml-base" ,(package-with-ocaml4.07 ocaml-base))
        ("ocaml-ppx-compare" ,(package-with-ocaml4.07 ocaml-ppx-compare))
        ("ocaml-ppx-custom-printf" ,(package-with-ocaml4.07 ocaml-ppx-custom-printf))
        ("ocaml-ppx-fields-conv" ,(package-with-ocaml4.07 ocaml-ppx-fields-conv))
        ("ocaml-ppx-sexp-conv" ,(package-with-ocaml4.07 ocaml-ppx-sexp-conv))
        ("ocaml-ppx-variants-conv" ,(package-with-ocaml4.07 ocaml-ppx-variants-conv))
        ("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))))
    (arguments
     `(#:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
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
    (version "1.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml-doc/octavius")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c5m51xcn2jv42kjjpklr6g63sgx1k885wfdp1yr4wrmiaj9cbpx"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'make-writable
           (lambda _
             (for-each (lambda (file)
                         (chmod file #o644))
                       (find-files "." "."))
             #t)))))
    (properties `((upstream-name . "octavius")))
    (home-page "https://github.com/ocaml-doc/octavius")
    (synopsis "Ocamldoc comment syntax parser")
    (description "Octavius is a library to parse the `ocamldoc` comment syntax.")
    (license license:isc)))

(define-public ocaml-ppx-hash
  (package
    (name "ocaml-ppx-hash")
    (version "0.14.0")
    (source
     (janestreet-origin "ppx_hash" version
                "0x4wgdvhgd8a49bzari52jpkykxpv6ncgp5ncda3xgg0a9r49s8n"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-ppx-compare ocaml-ppx-sexp-conv
           ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_hash")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-hash))))
    (home-page "https://github.com/janestreet/ppx_hash")
    (synopsis "Generation of hash functions from type expressions and definitions")
    (description "This package is a collection of ppx rewriters that generate
hash functions from type exrpessions and definitions.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-hash
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-hash)
     (name "ocaml-ppx-hash")
     (home-page "https://github.com/janestreet/ppx_hash")
     (version "0.11.1")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url home-page)
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1p0ic6aijxlrdggpmycj12q3cy9xksbq2vq727215maz4snvlf5p"))))
     (properties `((upstream-name . "ppx_hash"))))))

(define-public ocaml-ppx-enumerate
  (package
    (name "ocaml-ppx-enumerate")
    (version "0.14.0")
    (source
     (janestreet-origin
      "ppx_enumerate" version
      "1ij6sffgqhnjwnj9brhrrw1c6xgxlh0s6r17x1qkgnyrc73gfsz8"))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ; no test suite
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_enumerate")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-enumerate))))
    (home-page "https://github.com/janestreet/ppx_enumerate")
    (synopsis "Generate a list containing all values of a finite type")
    (description "Ppx_enumerate is a ppx rewriter which generates a definition
for the list of all values of a type (for a type which only has finitely
many values).")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-enumerate
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-enumerate)
     (name "ocaml-ppx-enumerate")
     (version "0.11.1")
     (home-page "https://github.com/janestreet/ppx_enumerate")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url home-page)
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0spx9k1v7vjjb6sigbfs69yndgq76v114jhxvzjmffw7q989cyhr"))))
     (properties `((upstream-name . "ppx_enumerate"))))))

(define-public ocaml4.07-ppx-bench
  (package
    (name "ocaml4.07-ppx-bench")
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
     `(#:tests? #f
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
     `(("ocaml-ppx-inline-test" ,(package-with-ocaml4.07 ocaml-ppx-inline-test))
        ("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))
        ("ocaml-ppxlib" ,(package-with-ocaml4.07 ocaml-ppxlib))))
    (properties `((upstream-name . "ppx_bench")))
    (home-page "https://github.com/janestreet/ppx_bench")
    (synopsis "Syntax extension for writing in-line benchmarks in ocaml code")
    (description "Syntax extension for writing in-line benchmarks in ocaml code.")
    (license license:asl2.0)))

(define-public ocaml-ppx-here
  (package
    (name "ocaml-ppx-here")
    (version "0.14.0")
    (source
     (janestreet-origin "ppx_here" version
                        "0b444djy68v6ji0ypwv5l02pkl151qzrgg96lyhl8dxfrzvj1zkj"))
    (build-system dune-build-system)
    (arguments
     ;; broken tests
     `(#:tests? #f))
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_here")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-here))))
    (home-page "https://github.com/janestreet/ppx_here")
    (synopsis "Expands [%here] into its location")
    (description
      "Part of the Jane Street's PPX rewriters collection.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-here
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-here)
     (version "0.11.0")
     (source
      (janestreet-origin "ppx_here" version
                         "0wxcak3ay4jpigm3pfdcpr65qw4hxfa8whhkryhcd8gy71x056z5"))
     (properties `((upstream-name . "ppx_here"))))))

(define-public ocaml4.07-typerep
  (package
    (name "ocaml4.07-typerep")
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
     `(#:tests? #f
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs `(("ocaml-base" ,(package-with-ocaml4.07 ocaml-base))))
    (home-page "https://github.com/janestreet/typerep")
    (synopsis "Typerep is a library for runtime types")
    (description "Typerep is a library for runtime types.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-sexp-value
  (package
    (name "ocaml4.07-ppx-sexp-value")
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
    (arguments
     `(#:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
      `(("ocaml-base" ,(package-with-ocaml4.07 ocaml-base))
        ("ocaml-ppx-here" ,(package-with-ocaml4.07 ocaml-ppx-here))
        ("ocaml-ppx-sexp-conv" ,(package-with-ocaml4.07 ocaml-ppx-sexp-conv))
        ("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))
        ("ocaml-ppxlib" ,(package-with-ocaml4.07 ocaml-ppxlib))))
    (properties `((upstream-name . "ppx_sexp_value")))
    (home-page "https://github.com/janestreet/ppx_sexp_value")
    (synopsis "Simplify building s-expressions from ocaml values")
    (description "A ppx rewriter that simplifies building s-expressions from
ocaml values.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-sexp-message
  (package
    (name "ocaml4.07-ppx-sexp-message")
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
    (arguments
     `(#:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
      `(("ocaml-base" ,(package-with-ocaml4.07 ocaml-base))
        ("ocaml-ppx-here" ,(package-with-ocaml4.07 ocaml-ppx-here))
        ("ocaml-ppx-sexp-conv" ,(package-with-ocaml4.07 ocaml-ppx-sexp-conv))
        ("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))
        ("ocaml-ppxlib" ,(package-with-ocaml4.07 ocaml-ppxlib))))
    (properties `((upstream-name . "ppx_sexp_message")))
    (home-page "https://github.com/janestreet/ppx_sexp_message")
    (synopsis "Ppx rewriter for easy construction of s-expressions")
    (description "Ppx_sexp_message aims to ease the creation of s-expressions
in OCaml.  This is mainly motivated by writing error and debugging messages,
where one needs to construct a s-expression based on various element of the
context such as function arguments.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-pipebang
  (package
    (name "ocaml4.07-ppx-pipebang")
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
     `(#:tests? #f
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
      `(("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))
        ("ocaml-ppxlib" ,(package-with-ocaml4.07 ocaml-ppxlib))))
    (properties `((upstream-name . "ppx_pipebang")))
    (home-page "https://github.com/janestreet/ppx_pipebang")
    (synopsis "Inline reverse application operators `|>` and `|!`")
    (description "A ppx rewriter that inlines reverse application operators
@code{|>} and @code{|!}.")
    (license license:asl2.0)))

(define-public ocaml-ppx-optional
  (package
    (name "ocaml-ppx-optional")
    (version "0.14.0")
    (source
     (janestreet-origin
      "ppx_optional" version
      "1hh6ivlp1qpvyn8l0vhrahkkcp3scf7km254sgplprmk10wnyidz"))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)) ; No tests
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_optional")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-optional))))
    (home-page "https://github.com/janestreet/ppx_optional")
    (synopsis "Pattern matching on flat options")
    (description
      "A ppx rewriter that rewrites simple match statements with an if then
else expression.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-optional
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-optional)
     (version "0.11.0")
     (source
      (janestreet-origin
       "ppx_optional" version
       "1z8z2bga95k2vksljljfglg10vygkjd24kn1b37sk4z3nmp47x0h"))
     (properties `((upstream-name . "ppx_optional"))))))

(define-public ocaml-ppx-optcomp
  (package
    (name "ocaml-ppx-optcomp")
    (version "0.14.3")
    (home-page "https://github.com/janestreet/ppx_optcomp")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1iflgfzs23asw3k6098v84al5zqx59rx2qjw0mhvk56avlx71pkw"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-stdio ocaml-ppxlib))
    (properties `((upstream-name . "ppx_optcomp")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-optcomp))))
    (synopsis "Optional compilation for OCaml")
    (description "Ppx_optcomp stands for Optional Compilation.  It is a tool
used to handle optional compilations of pieces of code depending of the word
size, the version of the compiler, ...")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-optcomp
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-optcomp)
     (version "0.11.0")
     (source
      (janestreet-origin
       "ppx_optcomp" version
       "1bb52p2j2h4s9f06vrcpla80rj93jinnzq6jzilapyx9q068929i"))
     (properties `((upstream-name . "ppx_optcomp"))))))

(define-public ocaml-ppx-let
  (package
    (name "ocaml-ppx-let")
    (version "0.14.0")
    (source
     (janestreet-origin "ppx_let" version
                        "1qcrnd86pbr1di5m6z4ps4p15qawwa02jxwz3xfd82hdbjmdwf1s"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_let")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-let))))
    (home-page "https://github.com/janestreet/ppx_let")
    (synopsis "Monadic let-bindings")
    (description "A ppx rewriter for monadic and applicative let bindings,
match expressions, and if expressions.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-let
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-let)
     (version "0.11.0")
     (source
      (janestreet-origin "ppx_let" version
                         "1wdfw6w4xbg97a35yg6bif9gggxniy9ddnrjfw1a0inkl2yamxkj"))

     (properties `((upstream-name . "ppx_let"))))))

(define-public ocaml4.07-ppx-fail
  (package
    (name "ocaml4.07-ppx-fail")
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
    (arguments
     `(#:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
      `(("ocaml-base" ,(package-with-ocaml4.07 ocaml-base))
        ("ocaml-ppx-here" ,(package-with-ocaml4.07 ocaml-ppx-here))
        ("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))
        ("ocaml-ppxlib" ,(package-with-ocaml4.07 ocaml-ppxlib))))
    (properties `((upstream-name . "ppx_fail")))
    (home-page "https://github.com/janestreet/ppx_fail")
    (synopsis "Add location to calls to failwiths")
    (description "Syntax extension that makes [failwiths] always include a
position.")
    (license license:asl2.0)))

(define-public ocaml-ppx-cold
  (package
    (name "ocaml-ppx-cold")
    (version "0.14.0")
    (home-page "https://github.com/janestreet/ppx_cold")
    (source
     (janestreet-origin "ppx_cold" version
                        "1madfzhpir9amnxmg530n70vll0jrl59vyp71miji73i6b9sy6n2"))
    (build-system dune-build-system)
    (arguments `(#:test-target "tests"))
    (propagated-inputs
     (list ocaml-base ocaml-ppxlib))
    (properties `((upstream-name . "ppx_cold")))
    (synopsis "Syntax extension for indicating cold path")
    (description
     "This package contains an syntax extension to indicate that the code is
on the cold path and should be kept out of the way to avoid polluting the
instruction cache on the hot path.  See also
https://github.com/ocaml/ocaml/issues/8563.")
    (license license:expat)))

(define-public ocaml-ppx-assert
  (package
    (name "ocaml-ppx-assert")
    (version "0.14.0")
    (source
     (janestreet-origin "ppx_assert" version
                        "1l2rr4jz2q5b35ryn2z146z7m9v6k8krp5gpn8ilib66mnz5zx15"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base
           ocaml-ppx-cold
           ocaml-ppx-compare
           ocaml-ppx-here
           ocaml-ppx-sexp-conv
           ocaml-migrate-parsetree
           ocaml-ppxlib))
    (properties `((upstream-name . "ppx_assert")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-assert))))
    (home-page "https://github.com/janestreet/ppx_assert")
    (synopsis "Assert-like extension nodes that raise useful errors on failure")
    (description "This package contains assert-like extension nodes that raise
useful errors on failure.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-assert
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-assert)
     (version "0.11.0")
     (source
      (janestreet-origin "ppx_assert" version
                         "17kd311n0l9f72gblf9kv8i5rghr106w37x4f0m5qwh6nlgl0j9k"))
     (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-compare" ,ocaml-ppx-compare)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
     (properties `((upstream-name . "ppx_assert"))))))

(define-public ocaml-ppx-expect
  (package
    (name "ocaml-ppx-expect")
    (version "0.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/janestreet/ppx_expect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1v886rsl93wdmaw61z10q8nqshf8hvlznj9gym2ljrjz4cqyjsa4"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base
           ocaml-ppx-here
           ocaml-ppx-inline-test
           ocaml-stdio
           ocaml-ppxlib
           ocaml-migrate-parsetree
           ocaml-re))
    (properties `((upstream-name . "ppx_expect")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-expect))))
    (home-page "https://github.com/janestreet/ppx_expect")
    (synopsis "Cram like framework for OCaml")
    (description "Expect-test is a framework for writing tests in OCaml, similar
to Cram.  Expect-tests mimics the existing inline tests framework with the
@code{let%expect_test} construct.  The body of an expect-test can contain
output-generating code, interleaved with @code{%expect} extension expressions
to denote the expected output.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-expect
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-expect)
     (version "0.12.0")
     (source
      (janestreet-origin "ppx_expect" version
                         "1zpci8c49yn2ixchmwjx1kf9pwybv3dnn4l2dgnd6m36qnkralfk"))
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
     (properties `((upstream-name . "ppx_expect"))))))

(define-public ocaml-ppx-js-style
  (package
    (name "ocaml-ppx-js-style")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/janestreet/ppx_js_style")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "16ax6ww9h36xyn9acbm8zxv0ajs344sm37lgj2zd2bvgsqv24kxj"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)) ; No tests
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-octavius ocaml-ppxlib))
    (properties `((upstream-name . "ppx_js_style")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-js-style))))
    (home-page "https://github.com/janestreet/ppx_js_style")
    (synopsis "Code style checker for Jane Street Packages")
    (description "This package is a no-op ppx rewriter.  It is used as a
@code{lint} tool to enforce some coding conventions across all Jane Street
packages.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-js-style
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-js-style)
     (version "0.11.0")
     (source
      (janestreet-origin "ppx_js_style" version
                         "0z3fc55jdjhhsblla6z4fqc13kljpcz29q79rvs5h2vsraqrldr2"))
     (properties `((upstream-name . "ppx_js_style"))))))

(define-public ocaml4.07-ppx-typerep-conv
  (package
    (name "ocaml4.07-ppx-typerep-conv")
    (version "0.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/ppx_typerep_conv")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a13dpfrrg0rsm8qni1bh7pqcda30l70z8r6yzi5a64bmwk7g5ah"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
      `(("ocaml-base" ,(package-with-ocaml4.07 ocaml-base))
        ("ocaml-typerep" ,ocaml4.07-typerep)
        ("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))
        ("ocaml-ppxlib" ,(package-with-ocaml4.07 ocaml-ppxlib))))
    (properties `((upstream-name . "ppx_typerep_conv")))
    (home-page "https://github.com/janestreet/ppx_typerep_conv")
    (synopsis "Generation of runtime types from type declarations")
    (description "This package can automatically generate runtime types
from type definitions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-base
  (package
    (name "ocaml-ppx-base")
    (version "0.14.0")
    (source
     (janestreet-origin
      "ppx_base" version
      "0b7a3fmi90jk8paz0g36yzaq670fbnrbi1j8r5ibh9wbcfli7ji6"))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (propagated-inputs
     (list ocaml-ppx-compare
           ocaml-ppx-cold
           ocaml-ppx-enumerate
           ocaml-ppx-hash
           ocaml-ppx-js-style
           ocaml-ppx-sexp-conv
           ocaml-migrate-parsetree
           ocaml-ppxlib))
    (properties `((upstream-name . "ppx_base")
                  (ocaml4.07-variant . ,(delay ocaml4.07-ppx-base))))
    (home-page "https://github.com/janestreet/ppx_base")
    (synopsis "Base set of ppx rewriters")
    (description "Ppx_base is the set of ppx rewriters used for Base.

Note that Base doesn't need ppx to build, it is only used as a
verification tool.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-base
  (package-with-ocaml4.07
   (package
     (inherit ocaml-ppx-base)
     (version "0.11.0")
     (source
      (janestreet-origin
       "ppx_base" version
       "0aq206pg330jmj7lhcagiiwm3a0b3gsqm801m8ajd4ysyw7idkym"))
    (propagated-inputs
     `(("ocaml-ppx-compare" ,ocaml-ppx-compare)
       ("ocaml-ppx-enumerate" ,ocaml-ppx-enumerate)
       ("ocaml-ppx-hash" ,ocaml-ppx-hash)
       ("ocaml-ppx-js-style" ,ocaml-ppx-js-style)
       ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
       ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_base"))))))

(define-public ocaml4.07-ppx-bin-prot
  (package
    (name "ocaml4.07-ppx-bin-prot")
    (version "0.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/ppx_bin_prot")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h60i75bzvhna1axyn662gyrzhh441l79vl142d235i5x31dmnkz"))))
    (build-system dune-build-system)
    (arguments
     ;; Cyclic dependency with ocaml-ppx-jane
     `(#:tests? #f
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
      `(("ocaml-base" ,(package-with-ocaml4.07 ocaml-base))
        ("ocaml-bin-prot" ,ocaml4.07-bin-prot)
        ("ocaml-ppx-here" ,(package-with-ocaml4.07 ocaml-ppx-here))
        ("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))
        ("ocaml-ppxlib" ,(package-with-ocaml4.07 ocaml-ppxlib))))
    (properties `((upstream-name . "ppx_bin_prot")))
    (home-page "https://github.com/janestreet/ppx_bin_prot")
    (synopsis "Generation of bin_prot readers and writers from types")
    (description "Generation of binary serialization and deserialization
functions from type definitions.")
    (license license:asl2.0)))

(define-public ocaml4.07-ppx-jane
  (package
    (name "ocaml4.07-ppx-jane")
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
     `(#:test-target "."
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
     `(("ocaml-ppx-assert" ,(package-with-ocaml4.07 ocaml-ppx-assert))
       ("ocaml-ppx-base" ,(package-with-ocaml4.07 ocaml-ppx-base))
        ("ocaml-ppx-bench" ,ocaml4.07-ppx-bench)
        ("ocaml-ppx-bin-prot" ,ocaml4.07-ppx-bin-prot)
        ("ocaml-ppx-custom-printf" ,(package-with-ocaml4.07 ocaml-ppx-custom-printf))
        ("ocaml-ppx-expect" ,(package-with-ocaml4.07 ocaml-ppx-expect))
        ("ocaml-ppx-fail" ,ocaml4.07-ppx-fail)
        ("ocaml-ppx-fields-conv" ,(package-with-ocaml4.07 ocaml-ppx-fields-conv))
        ("ocaml-ppx-here" ,(package-with-ocaml4.07 ocaml-ppx-here))
        ("ocaml-ppx-inline-test" ,(package-with-ocaml4.07 ocaml-ppx-inline-test))
        ("ocaml-ppx-let" ,(package-with-ocaml4.07 ocaml-ppx-let))
        ("ocaml-ppx-optcomp" ,(package-with-ocaml4.07 ocaml-ppx-optcomp))
        ("ocaml-ppx-optional" ,(package-with-ocaml4.07 ocaml-ppx-optional))
        ("ocaml-ppx-pipebang" ,ocaml4.07-ppx-pipebang)
        ("ocaml-ppx-sexp-message" ,ocaml4.07-ppx-sexp-message)
        ("ocaml-ppx-sexp-value" ,ocaml4.07-ppx-sexp-value)
        ("ocaml-ppx-typerep-conv" ,ocaml4.07-ppx-typerep-conv)
        ("ocaml-ppx-variants-conv" ,(package-with-ocaml4.07 ocaml-ppx-variants-conv))
        ("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))
        ("ocaml-ppxlib" ,(package-with-ocaml4.07 ocaml-ppxlib))))
    (properties `((upstream-name . "ppx_jane")))
    (home-page "https://github.com/janestreet/ppx_jane")
    (synopsis "Standard Jane Street ppx rewriters")
    (description "This package installs a ppx-jane executable, which is a ppx
driver including all standard Jane Street ppx rewriters.")
    (license license:asl2.0)))

(define-public ocaml4.07-splittable-random
  (package
    (name "ocaml4.07-splittable-random")
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
    (arguments
     `(#:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
      `(("ocaml-base" ,(package-with-ocaml4.07 ocaml-base))
        ("ocaml-ppx-jane" ,ocaml4.07-ppx-jane)
        ("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))))
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

(define-public ocaml4.07-jane-street-headers
  (package
    (name "ocaml4.07-jane-street-headers")
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
     `(#:test-target "."
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (home-page "https://github.com/janestreet/jane-street-headers")
    (synopsis "Jane Street C header files")
    (description "This package provides C header files shared between the
various Jane Street packages.")
    (license license:asl2.0)))

(define-public ocaml4.07-configurator
  (package
    (name "ocaml4.07-configurator")
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
     `(#:tests? #f
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
      `(("ocaml-base" ,(package-with-ocaml4.07 ocaml-base))
        ("ocaml-stdio" ,(package-with-ocaml4.07 ocaml-stdio))))
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

(define-public ocaml4.07-spawn
  (package
    (name "ocaml4.07-spawn")
    (version "0.13.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/spawn")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w003k1kw1lmyiqlk58gkxx8rac7dchiqlz6ah7aj7bh49b36ppf"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (substitute* "test/tests.ml"
               (("/bin/pwd") (which "pwd"))
               (("/bin/echo") (which "echo")))
             #t)))
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (native-inputs
     `(("ocaml-ppx-expect" ,(package-with-ocaml4.07 ocaml-ppx-expect))))
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

(define-public ocaml4.07-core
  (package
    (name "ocaml4.07-core")
    (version "0.11.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/core")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pzl8n09z4f3i7z2wq4cjxfqrr8mj6xcdp7rbg0nxap2zdhjgvrq"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "core"
       #:tests? #f; Require a cyclic dependency: core_extended
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
      `(("ocaml-base" ,(package-with-ocaml4.07 ocaml-base))
        ("ocaml-configurator" ,ocaml4.07-configurator)
        ("ocaml-core-kernel" ,ocaml4.07-core-kernel)
        ("ocaml-ppx-assert" ,(package-with-ocaml4.07 ocaml-ppx-assert))
        ("ocaml-ppx-jane" ,ocaml4.07-ppx-jane)
        ("ocaml-sexplib" ,(package-with-ocaml4.07 ocaml-sexplib))
        ("ocaml-spawn" ,ocaml4.07-spawn)
        ("ocaml-stdio" ,(package-with-ocaml4.07 ocaml-stdio))
        ("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))
        ("ocaml-ppxlib" ,(package-with-ocaml4.07 ocaml-ppxlib))))
    (home-page "https://github.com/janestreet/core")
    (synopsis "Alternative to OCaml's standard library")
    (description "The Core suite of libraries is an alternative to OCaml's
standard library that was developed by Jane Street.")
    ;; Also contains parts of OCaml, relicensed to asl2.0, as permitted
    ;; by OCaml's license for consortium members (see THIRD-PARTY.txt).
    (license license:asl2.0)))

(define-public ocaml4.07-core-kernel
  (package
    (name "ocaml4.07-core-kernel")
    (version "0.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/core_kernel")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dg7ygy7i64c5gaakb1cp1b26p9ks81vbxmb8fd7jff2q60j2z2g"))))
    (build-system dune-build-system)
    (arguments
     ;; Cyclic dependency with ocaml-core
     `(#:tests? #f
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune))
    (propagated-inputs
      `(("ocaml-base" ,(package-with-ocaml4.07 ocaml-base))
        ("ocaml-bin-prot" ,ocaml4.07-bin-prot)
        ("ocaml-configurator" ,ocaml4.07-configurator)
        ("ocaml-fieldslib" ,(package-with-ocaml4.07 ocaml-fieldslib))
        ("ocaml-jane-street-headers" ,ocaml4.07-jane-street-headers)
        ("ocaml-ppx-assert" ,(package-with-ocaml4.07 ocaml-ppx-assert))
        ("ocaml-ppx-base" ,(package-with-ocaml4.07 ocaml-ppx-base))
        ("ocaml-ppx-hash" ,(package-with-ocaml4.07 ocaml-ppx-hash))
        ("ocaml-ppx-inline-test" ,(package-with-ocaml4.07 ocaml-ppx-inline-test))
        ("ocaml-ppx-jane" ,ocaml4.07-ppx-jane)
        ("ocaml-ppx-sexp-conv" ,(package-with-ocaml4.07 ocaml-ppx-sexp-conv))
        ("ocaml-ppx-sexp-message" ,ocaml4.07-ppx-sexp-message)
        ("ocaml-sexplib" ,(package-with-ocaml4.07 ocaml-sexplib))
        ("ocaml-splittable-random" ,ocaml4.07-splittable-random)
        ("ocaml-stdio" ,(package-with-ocaml4.07 ocaml-stdio))
        ("ocaml-typerep" ,ocaml4.07-typerep)
        ("ocaml-variantslib" ,(package-with-ocaml4.07 ocaml-variantslib))
        ("ocaml-migrate-parsetree"
         ,(package-with-ocaml4.07 ocaml-migrate-parsetree))))
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

(define-public ocaml-markup
  (package
    (name "ocaml-markup")
    (version "1.0.2")
    (home-page "https://github.com/aantron/markup.ml")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1kvqwrrcrys5d0kzdwxcj66jpi6sdhfas4pcg02pixx92q87vhqm"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "markup"))
    (propagated-inputs
     (list ocaml-bisect-ppx ocaml-uchar ocaml-uutf ocaml-lwt))
    (native-inputs
     (list ocaml-ounit2 pkg-config))
    (properties
     `((ocaml4.07-variant . ,(delay (package-with-ocaml4.07 ocaml-markup0.8.0)))))
    (synopsis "Error-recovering functional HTML5 and XML parsers and writers")
    (description "Markup.ml provides an HTML parser and an XML parser.  The
parsers are wrapped in a simple interface: they are functions that transform
byte streams to parsing signal streams.  Streams can be manipulated in various
ways, such as processing by fold, filter, and map, assembly into DOM tree
structures, or serialization back to HTML or XML.

Both parsers are based on their respective standards.  The HTML parser, in
particular, is based on the state machines defined in HTML5.

The parsers are error-recovering by default, and accept fragments.  This makes
it very easy to get a best-effort parse of some input.  The parsers can,
however, be easily configured to be strict, and to accept only full documents.

Apart from this, the parsers are streaming (do not build up a document in
memory), non-blocking (can be used with threading libraries), lazy (do not
consume input unless the signal stream is being read), and process the input in
a single pass.  They automatically detect the character encoding of the input
stream, and convert everything to UTF-8.")
    (license license:bsd-3)))

;; ocaml-markup 1.0.0 can not be built with old version of dune used in
;; package-with-ocaml4.07
(define-public ocaml-markup0.8.0
  (package
    (inherit ocaml-markup)
    (name "ocaml-markup")
    (version "0.8.0")
    (home-page "https://github.com/aantron/markup.ml")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0aif4abvfmi9xc1pvw5n5rbm6rzkkpsxyvdn0lanr33rjpvkwdlm"))))
    (native-inputs
     (list ocaml-ounit pkg-config))
    (properties '())))

(define-public ocaml-tyxml
  (package
    (name "ocaml-tyxml")
    (version "4.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocsigen/tyxml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0bh66wknc7sx2r63kscp0hg6h73dkv6qpkx0cdz2qp7p28pg2ixz"))))
    (build-system dune-build-system)
    (inputs
     (list ocaml-re ocaml-seq ocaml-uutf))
    (native-inputs
     (list ocaml-alcotest))
    (arguments `(#:package "tyxml"))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-tyxml))))
    (home-page "https://github.com/ocsigen/tyxml/")
    (synopsis "TyXML is a library for building correct HTML and SVG documents")
    (description "TyXML provides a set of convenient combinators that uses the
OCaml type system to ensure the validity of the generated documents.  TyXML can
be used with any representation of HTML and SVG: the textual one, provided
directly by this package, or DOM trees (@code{js_of_ocaml-tyxml}) virtual DOM
(@code{virtual-dom}) and reactive or replicated trees (@code{eliom}).  You can
also create your own representation and use it to instantiate a new set of
combinators.")
    (license license:lgpl2.1)))

(define-public ocaml4.07-tyxml
  (package-with-ocaml4.07
    (package
      (inherit ocaml-tyxml)
      (version "4.4.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/ocsigen/tyxml")
                       (commit version)))
                (file-name (git-file-name "ocaml-tyxml" version))
                (sha256
                 (base32
                  "1hw4phyadcfgywgh5sj87i76gp56qwxzwlcpfdwjbf6ggag9clmd"))))
      (properties '()))))

(define-public ocaml-bisect-ppx
  (package
    (name "ocaml-bisect-ppx")
    (version "2.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aantron/bisect_ppx")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "02sn1a9anyksd2lpd9xryrxsx76xg99c7mx1mq6b5q09r9b060d4"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-ppxlib ocaml-cmdliner))
    (arguments
     ;; Tests require ocamlformat which would lead to circular dependencies
     '(#:tests? #f))
    (properties `((upstream-name . "bisect_ppx")
                  (ocaml4.07-variant . ,(delay ocaml4.07-bisect-ppx))))
    (home-page "https://github.com/aantron/bisect_ppx")
    (synopsis "Code coverage for OCaml")
    (description "Bisect_ppx helps you test thoroughly.  It is a small
preprocessor that inserts instrumentation at places in your code, such as
if-then-else and match expressions.  After you run tests, Bisect_ppx gives a
nice HTML report showing which places were visited and which were missed.

Usage is simple - add package bisect_ppx when building tests, run your tests,
then run the Bisect_ppx report tool on the generated visitation files.")
    (license license:mpl2.0)))

(define-public ocaml4.07-bisect-ppx
  (package-with-ocaml4.07
    (package
      (inherit ocaml-bisect-ppx)
      (version "2.4.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aantron/bisect_ppx")
               (commit version)))
         (file-name (git-file-name "ocaml-bisect-ppx" version))
         (sha256
          (base32
           "1njs8xc108rrpx5am5zhhcn6vjva7rsphm8034qp5lgyvnhfgh7q"))))
      (propagated-inputs
       `(("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
         ("ocaml-ppx-tools-versioned" ,ocaml4.07-ppx-tools-versioned)
         ,@(package-propagated-inputs ocaml-bisect-ppx)))
      (native-inputs
       `(("ocaml-ounit2" ,ocaml-ounit2)))
      (arguments
       `(#:test-target "."
         ;; tests require git and network
         #:tests? #f))
      (properties '((upstream-name . "bisect_ppx"))))))

(define-public ocaml-odoc
  (package
    (name "ocaml-odoc")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/odoc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ycb468pc6vsvqj176j99bmbkrr9saxvyn9qhpazi01abbcq5d90"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test
           (lambda _
             ;; test results expects #!/bin/sh but gets a store path instead
             (substitute* "test/xref2/with.t/run.t"
               (("#!/bin/sh") (string-append "#!" (which "sh")))))))))
    (inputs
    (list ocaml-astring
          ocaml-bisect-ppx
          ocaml-cmdliner
          ocaml-fmt
          ocaml-fpath
          ocaml-logs
          ocaml-migrate-parsetree
          ocaml-odoc-parser
          ocaml-re
          ocaml-result
          ocaml-tyxml))
  (native-inputs
    (list ocaml-alcotest
          ocaml-bos
          ocaml-cppo
          ocaml-findlib
          ocaml-lwt
          ocaml-markup
          ocaml-ppx-expect
          ocaml-version
          ocaml-yojson
          jq))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-odoc))))
    (home-page "https://github.com/ocaml/odoc")
    (synopsis "OCaml documentation generator")
    (description "Odoc is a documentation generator for OCaml.  It reads
@emph{doc comments}, delimited with @code{(** ... *)}, and outputs
@acronym{HTML}.

Text inside doc comments is marked up in ocamldoc syntax.  Odoc's main
advantage over ocamldoc is an accurate cross-referencer, which handles the
complexity of the OCaml module system.")
    (license license:isc)))

(define-public ocaml-odoc-parser
  (package
    (name "ocaml-odoc-parser")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/ocaml-doc/odoc-parser")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
           "0mvwbnla0wd29bc7ckwpb494qv7p616cx8hnm0zg36np7v3zwy4w"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-astring ocaml-result))
    (native-inputs
      (list ocaml-ppx-expect))
    (home-page "https://github.com/ocaml-doc/odoc-parser")
    (synopsis "Parser for ocaml documentation comments")
    (description
     "This package provides a library for parsing the contents of OCaml
documentation comments, formatted using Odoc syntax, an extension of the
language understood by ocamldoc.")
    (license license:isc)))

;; version 1.5.2 requires ocaml-markdown 1.0.0 which does not compile
;; with old version of dune used in package-with-ocaml4.07
(define-public ocaml4.07-odoc
  (package-with-ocaml4.07
   (package
     (inherit ocaml-odoc)
     (name "ocaml-odoc")
     (version "1.5.1")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/ocaml/odoc")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0z2nisg1vb5xlk41hqw8drvj90v52wli7zvnih6a844cg6xsvvj2"))))
     (arguments '())
     (inputs
      `(("ocaml-alcotest" ,ocaml-alcotest)
        ("ocaml-markup" ,ocaml-markup)
        ("ocaml-sexplib" ,ocaml-sexplib)
        ("ocaml-re" ,ocaml-re)
        ("ocaml-uutf" ,ocaml-uutf)))
     (native-inputs
      `(("ocaml-astring" ,ocaml-astring)
        ("ocaml-cmdliner" ,ocaml-cmdliner)
        ("ocaml-cppo" ,ocaml-cppo)
        ("ocaml-fpath" ,ocaml-fpath)
        ("ocaml-result" ,ocaml-result)
        ("ocaml-tyxml" ,ocaml-tyxml)
        ("ocaml-bisect-ppx" ,ocaml-bisect-ppx)
        ("tidy-html" ,tidy-html)))
     (properties '()))))

(define-public ocaml-fftw3
  (package
    (name "ocaml-fftw3")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Chris00/fftw-ocaml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07ljbin9dsclsqh24p7haqjccz1w828sf5xfwlzl298d4a6zsbhs"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "tests"))
    (propagated-inputs
     (list fftw fftwf))
    (native-inputs
     (list ocaml-cppo ocaml-lacaml))
    (home-page
     "https://github.com/Chris00/fftw-ocaml")
    (synopsis
     "Bindings to FFTW3")
    (description
     "Bindings providing OCaml support for the seminal Fast Fourier Transform
library FFTW.")
    (license license:lgpl2.1))) ; with static linking exception.

(define-public ocaml-lacaml
  (package
    (name "ocaml-lacaml")
    (version "11.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mmottl/lacaml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "115535kphchh2a434b48b408x9794j8zzrsdmacsgqdsrgy3rck4"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ; No test target.
    (native-inputs
     (list openblas lapack ocaml-base ocaml-stdio))
    (home-page "https://mmottl.github.io/lacaml/")
    (synopsis
     "OCaml-bindings to BLAS and LAPACK")
    (description
     "Lacaml interfaces the BLAS-library (Basic Linear Algebra Subroutines) and
LAPACK-library (Linear Algebra routines).  It also contains many additional
convenience functions for vectors and matrices.")
    (license license:lgpl2.1)))

(define-public ocaml-cairo2
  (package
    (name "ocaml-cairo2")
    (version "0.6.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Chris00/ocaml-cairo")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04rp5y1rlq1pw592yywhvqd5x7csmyi9ad4f32g99nakb10s4nhx"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "tests"))
    (inputs
     `(("cairo" ,cairo)
       ("gtk+-2" ,gtk+-2)
       ("lablgtk" ,lablgtk)))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/Chris00/ocaml-cairo")
    (synopsis "Binding to Cairo, a 2D Vector Graphics Library")
    (description "Ocaml-cairo2 is a binding to Cairo, a 2D graphics library
with support for multiple output devices.  Currently supported output targets
include the X Window System, Quartz, Win32, image buffers, PostScript, PDF,
and SVG file output.")
    (license license:lgpl3+)))

(define-public ocaml-version
  (package
    (name "ocaml-version")
    (version "3.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocurrent/ocaml-version")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1c13zk1qna59m5z9jx7da3p7cxd48scpyk7zg7gi6gc6hx5hhjx8"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))          ; no tests
    (properties '((upstream-name . "ocaml-version")))
    (home-page
     "https://github.com/ocurrent/ocaml-version")
    (synopsis
     "Manipulate, parse and generate OCaml compiler version strings")
    (description
     "This library provides facilities to parse version numbers of the OCaml
compiler, and enumerates the various official OCaml releases and configuration
variants.")
    (license license:isc)))

(define-public ocaml-mdx
  (package
    (name "ocaml-mdx")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/realworldocaml/mdx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w1givvhwv9jzj9zbg4mmlpb35sqi75w83r99p2z50bdr69fdf57"))))
    (build-system dune-build-system)
    (inputs
     (list ocaml-fmt
           ocaml-astring
           ocaml-logs
           ocaml-cmdliner
           ocaml-re
           ocaml-result
           ocaml-odoc
           ocaml-odoc-parser
           ocaml-version))
    (native-inputs
     (list ocaml-cppo ocaml-lwt ocaml-alcotest))
    (home-page
     "https://github.com/realworldocaml/mdx")
    (synopsis
     "Executable code blocks inside markdown files")
    (description
     "@code{ocaml-mdx} executes code blocks inside markdown files.
There are (currently) two sub-commands, corresponding
to two modes of operations: pre-processing (@code{ocaml-mdx pp})
and tests (@code{ocaml-mdx test}]).

The pre-processor mode allows mixing documentation and code,
and to practice @dfn{literate programming} using markdown and OCaml.

The test mode ensures that shell scripts and OCaml fragments
in the documentation always stays up-to-date.

@code{ocaml-mdx} is released as two binaries called @code{ocaml-mdx} and
@code{mdx} which are the same, mdx being the deprecated name, kept for now for
compatibility.")
    (license license:isc)))

(define-public ocaml-mparser
  (package
    (name "ocaml-mparser")
    (version "1.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/murmour/mparser")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "16j19v16r42gcsii6a337zrs5cxnf12ig0vaysxyr7sq5lplqhkx"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests.
     '(#:package "mparser"
       #:tests? #f))
    (home-page "https://github.com/murmour/mparser")
    (synopsis "Simple monadic parser combinator library")
    (description
      "This library implements a rather complete and efficient monadic parser
combinator library similar to the Parsec library for Haskell by Daan Leijen and
the FParsec library for FSharp by Stephan Tolksdorf.")
    ;; With static linking exception.
    (license license:lgpl2.1+)))

(define-public ocaml-mparser-re
  (package
    (inherit ocaml-mparser)
    (name "ocaml-mparser-re")
    (arguments
     ;; No tests.
     '(#:package "mparser-re"
       #:tests? #f))
    (propagated-inputs
     (list ocaml-mparser ocaml-re))
    (synopsis "MParser plugin for RE-based regular expressions")
    (description "This package provides RE-based regular expressions
support for Mparser.")))

(define-public ocaml-mparser-pcre
  (package
    (inherit ocaml-mparser)
    (name "ocaml-mparser-pcre")
    (arguments
     ;; No tests.
     '(#:package "mparser-pcre"
       #:tests? #f))
    (propagated-inputs
     (list ocaml-mparser ocaml-pcre))
    (synopsis "MParser plugin for PCRE-based regular expressions")
    (description "This package provides PCRE-based regular expressions
support for Mparser.")))

(define-public lablgtk3
  (package
    (name "lablgtk")
    (version "3.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/garrigue/lablgtk")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11qfc39cmwfwfpwmjh6wh98zwdv6p73bv8hqwcsss869vs1r7gmn"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #t
       #:test-target "."
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'make-writable
           (lambda _
             (for-each (lambda (file)
                         (chmod file #o644))
                       (find-files "." "."))))
         (add-before 'build 'set-version
           (lambda _
             (substitute* "dune-project"
               (("\\(name lablgtk3\\)")
                (string-append "(name lablgtk3)\n(version " ,version ")"))))))))
    (propagated-inputs
     (list ocaml-cairo2))
    (inputs
     `(("camlp5" ,camlp5)
       ("gtk+" ,gtk+)
       ("gtksourceview-3" ,gtksourceview-3)
       ("gtkspell3" ,gtkspell3)))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/garrigue/lablgtk")
    (synopsis "OCaml interface to GTK+3")
    (description "LablGtk is an OCaml interface to GTK+ 1.2, 2.x and 3.x.  It
provides a strongly-typed object-oriented interface that is compatible with the
dynamic typing of GTK+.  Most widgets and methods are available.  LablGtk
also provides bindings to gdk-pixbuf, the GLArea widget (in combination with
LablGL), gnomecanvas, gnomeui, gtksourceview, gtkspell, libglade (and it can
generate OCaml code from .glade files), libpanel, librsvg and quartz.")
    ;; Version 2 only, with linking exception.
    (license license:lgpl2.0)))

(define-public ocaml-reactivedata
  ;; Future releases will use dune.
  (package
    (name "ocaml-reactivedata")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocsigen/reactiveData")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0l5z0fsckqkywjbn2nwy3s55h85yx8scc4hq9qzr9ig3hrq1mfb0"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-deprecated
           (lambda _
             (substitute*
                 "src/reactiveData.ml"
               (("Pervasives.compare") "compare"))
             #t))
         (add-before 'install 'forget-makefile
           ;; Ensure we use opam to install files
           (lambda _
             (delete-file "Makefile")
             #t)))))
    (build-system ocaml-build-system)
    (properties `((upstream-name . "reactiveData")))
    (native-inputs
     (list ocamlbuild opam))
    (propagated-inputs
     (list ocaml-react))
    (home-page "https://github.com/ocsigen/reactiveData")
    (synopsis "Declarative events and signals for OCaml")
    (description
     "React is an OCaml module for functional reactive programming (FRP).  It
provides support to program with time varying values: declarative events and
 signals.  React doesn't define any primitive event or signal, it lets the
client chooses the concrete timeline.")
    (license license:lgpl2.1+)))

(define-public ocaml-uucd
  (package
    (name "ocaml-uucd")
    (version "14.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/uucd/releases/"
                           "uucd-" version ".tbz"))
       (sha256
        (base32
         "0fc737v5gj3339jx4x9xr096lxrpwvp6vaiylhavcvsglcwbgm30"))))
    (build-system ocaml-build-system)
    (arguments
     '(#:build-flags '("build" "--tests" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (propagated-inputs
     (list ocaml-xmlm))
    (native-inputs
     (list opam ocaml-findlib ocamlbuild ocaml-topkg))
    (home-page "https://erratique.ch/software/uucd")
    (synopsis "Unicode character database decoder for OCaml")
    (description "Uucd is an OCaml module to decode the data of the Unicode
character database from its XML representation.  It provides high-level (but
not necessarily efficient) access to the data so that efficient
representations can be extracted.")
    (license license:isc)))

(define-public ocaml-uucp
  (package
    (name "ocaml-uucp")
    (version "14.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/uucp/releases/"
                           "uucp-" version ".tbz"))
       (sha256
        (base32
         "1yx9nih3d9prb9zizq8fzmmqylf24a6yifhf81h33znrj5xn1mpj"))))
    (build-system ocaml-build-system)
    (arguments
     '(#:build-flags '("build" "--tests" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list opam
           ocaml-findlib
           ocamlbuild
           ocaml-topkg
           ocaml-uucd
           ocaml-uunf
           ocaml-uutf))
    (home-page "https://erratique.ch/software/uucp")
    (synopsis "Unicode character properties for OCaml")
    (description "Uucp is an OCaml library providing efficient access to a
selection of character properties of the Unicode character database.")
    (license license:isc)))

(define-public ocaml-uuseg
  (package
    (name "ocaml-uuseg")
    (version "14.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/uuseg/releases/"
                           "uuseg-" version ".tbz"))
       (sha256
        (base32
         "1g9zyzjkhqxgbb9mh3cgaawscwdazv6y8kdqvmy6yhnimmfqv25p"))))
    (build-system ocaml-build-system)
    (arguments
     '(#:build-flags '("build" "--tests" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (propagated-inputs
     (list ocaml-uucp ocaml-uutf ocaml-cmdliner))
    (native-inputs
     (list opam ocaml-findlib ocamlbuild ocaml-topkg))
    (home-page "https://erratique.ch/software/uuseg")
    (synopsis "Unicode text segmentation for OCaml")
    (description "Uuseg is an OCaml library for segmenting Unicode text.  It
implements the locale independent Unicode text segmentation algorithms to
detect grapheme cluster, word and sentence boundaries and the Unicode line
breaking algorithm to detect line break opportunities.

The library is independent from any IO mechanism or Unicode text data
structure and it can process text without a complete in-memory
representation.")
    (license license:isc)))

(define-public ocaml-fix
  (package
    (name "ocaml-fix")
    (version "20220121")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://gitlab.inria.fr/fpottier/fix")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "15785v43jcbqsw1y653cnb89alrcnbdri1h0w6zl6p7769ja9rdj"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests.
     '(#:tests? #f))
    (home-page "https://gitlab.inria.fr/fpottier/fix")
    (synopsis "Facilities for memoization and fixed points")
    (description "This package provides helpers with various constructions
that involve memoization and recursion.")
    (license license:lgpl2.0)))

(define-public ocaml-dune-build-info
  (package
    (inherit dune)
    (name "ocaml-dune-build-info")
    (build-system dune-build-system)
    (arguments
     '(#:package "dune-build-info"
       ;; No separate test suite from dune.
       #:tests? #f))
    (propagated-inputs
     (list ocaml-odoc))
    (synopsis "Embed build information inside an executable")
    (description "This package allows one to access information about how the
executable was built, such as the version of the project at which it was built
or the list of statically linked libraries with their versions.  It supports
reporting the version from the version control system during development to
get an precise reference of when the executable was built.")))

(define-public ocaml-either
  (package
    (name "ocaml-either")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mirage/either")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32 "099p1m24vz5i0043zcfp88krzjsa2qbrphrm4bnx84gif5vgkxwm"))))
    (build-system dune-build-system)
    (arguments
     ;; no tests
     `(#:tests? #f))
    (home-page "https://github.com/mirage/either")
    (synopsis "Compatibility Either module")
    (description "This library is a compatibility module for the Either module
defined in OCaml 4.12.0.")
    (license license:expat)))

(define-public ocamlformat
  (package
    (name "ocamlformat")
    (version "0.20.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/ocaml-ppx/ocamlformat")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1q78gxsz763d6vbi1lyfmn7733l10qhq80bchdli9zw7sggs7nq1"))))
    (build-system dune-build-system)
    (arguments
     '(#:package "ocamlformat"
       ;; FIXME: The expected format is slightly different than what the
       ;; produced format is for test/cli/stdin.t
       #:tests? #f))
    (propagated-inputs
      (list ocaml-version
            ocaml-base
            ocaml-cmdliner
            ocaml-dune-build-info
            ocaml-either
            ocaml-fix
            ocaml-fpath
            ocaml-menhir
            ocaml-odoc
            ocaml-ppxlib
            ocaml-re
            ocaml-odoc-parser
            ocaml-stdio
            ocaml-uuseg
            ocaml-uutf))
    (native-inputs
      (list ocaml-alcotest ocaml-ocp-indent ocaml-bisect-ppx))
    (home-page "https://github.com/ocaml-ppx/ocamlformat")
    (synopsis "Auto-formatter for OCaml code")
    (description "OCamlFormat is a tool to automatically format OCaml code in
a uniform style.")
    (license license:expat)))

(define-public ocaml-bigstringaf
  (package
    (name "ocaml-bigstringaf")
    (version "0.8.0")
    (home-page "https://github.com/inhabitedtype/bigstringaf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0z69g3bpgw4p7z9h5j70w75rnxfrd8c7nlhqq6xvvkkgbxafm1ll"))))
    (build-system dune-build-system)
    (arguments
     '(#:test-target "."))
    (propagated-inputs
     (list ocaml-bigarray-compat))
    (native-inputs
     (list ocaml-alcotest pkg-config))
    (synopsis
     "Bigstring intrinsics and fast blits based on memcpy/memmove")
    (description
     "The OCaml compiler has a bunch of intrinsics for Bigstrings, but they're
not widely-known, sometimes misused, and so programs that use Bigstrings are
slower than they have to be.  And even if a library got that part right and
exposed the intrinsics properly, the compiler doesn't have any fast blits
between Bigstrings and other string-like types.  @code{bigstringaf} provides
these missing pieces.")
    (license license:bsd-3)))

(define-public ocaml-trie
  (package
    (name "ocaml-trie")
    (version "1.0.0")
    (home-page "https://github.com/kandu/trie/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s7p9swjqjsqddylmgid6cv263ggq7pmb734z4k84yfcrgb6kg4g"))))
    (build-system dune-build-system)
    (arguments
     '(#:tests? #f))                    ;no tests
    (synopsis "Strict impure trie tree")
    (description
     "This module implements strict impure trie tree data structure for
OCaml.")
    (license license:expat)))

(define-public ocaml-mew
  (package
    (name "ocaml-mew")
    (version "0.1.0")
    (home-page "https://github.com/kandu/mew")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0417xsghj92v3xa5q4dk4nzf2r4mylrx2fd18i7cg3nzja65nia2"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-result ocaml-trie))
    (native-inputs
     (list ocaml-ppx-expect))
    (synopsis "General modal editing engine generator")
    (description
     "This package provides the core modules of Modal Editing Witch, a general
modal editing engine generator.")
    (license license:expat)))

(define-public ocaml-mew-vi
  (package
    (name "ocaml-mew-vi")
    (version "0.5.0")
    (home-page "https://github.com/kandu/mew_vi")
    (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
        (sha256
          (base32 "0lihbf822k5zasl60w5mhwmdkljlq49c9saayrws7g4qc1j353r8"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-mew ocaml-react))
    (native-inputs
     (list ocaml-ppx-expect))
    (properties `((upstream-name . "mew_vi")))
    (synopsis "Modal editing VI-like editing engine generator")
    (description "This module provides a vi-like modal editing engine
generator.")
    (license license:expat)))

(define-public ocaml-syntax-shims
  (package
    (name "ocaml-syntax-shims")
    (version "1.0.0")
    (home-page "https://github.com/ocaml-ppx/ocaml-syntax-shims")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0l1i8z95qgb0lxlrv3yb5nkp391hqsiyi4r91p12k3xmggqixagf"))))
    (build-system dune-build-system)
    (properties
     `((upstream-name . "ocaml-syntax-shims")))
    (synopsis
     "Backport new syntax to older OCaml versions")
    (description
     "This package backports new language features such as @code{let+} to older
OCaml compilers.")
    (license license:expat)))

(define-public ocaml-angstrom
  (package
    (name "ocaml-angstrom")
    (version "0.15.0")
    (home-page "https://github.com/inhabitedtype/angstrom")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hmrkdcdlkwy7rxhngf3cv3sa61cznnd9p5lmqhx20664gx2ibrh"))))
    (build-system dune-build-system)
    (arguments
     ;; Only build the base angstrom package.
     '(#:package "angstrom"
       #:test-target "."))
    (propagated-inputs
     (list ocaml-bigstringaf))
    (native-inputs
     (list ocaml-alcotest ocaml-ppx-let ocaml-syntax-shims))
    (synopsis "Parser combinators built for speed and memory-efficiency")
    (description
     "Angstrom is a parser-combinator library that makes it easy to write
efficient, expressive, and reusable parsers suitable for high-performance
applications.  It exposes monadic and applicative interfaces for composition,
and supports incremental input through buffered and unbuffered interfaces.
Both interfaces give the user total control over the blocking behavior of
their application, with the unbuffered interface enabling zero-copy IO.
Parsers are backtracking by default and support unbounded lookahead.")
    (license license:bsd-3)))

(define-public ocaml-graphics
  (package
    (name "ocaml-graphics")
    (version "5.1.2")
    (home-page "https://github.com/ocaml/graphics")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1q20f8y6ijxbvzik2ns4yl3w54q5z8kd0pby8i8c64a04hvly08m"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list libx11))
    (synopsis "The OCaml graphics library")
    (description
     "The graphics library provides a set of portable drawing primitives.
Drawing takes place in a separate window that is created when
Graphics.open_graph is called.  This library used to be distributed with OCaml
up to OCaml 4.08.")
    (license license:lgpl2.1+)))

(define-public ocaml-uri-sexp
  (package
    (inherit ocaml-uri)
    (name "ocaml-uri-sexp")
    (arguments
     '(#:package "uri-sexp"
       #:test-target "."))
    (propagated-inputs
      (list ocaml-uri ocaml-ppx-sexp-conv ocaml-sexplib0))
    (native-inputs (list ocaml-ounit))
    (synopsis "RFC3986 URI/URL parsing library")
    (description "This package adds S-exp support to @code{ocaml-uri}.")))

(define-public ocaml-cohttp
  (package
    (name "ocaml-cohttp")
    (version "5.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/mirage/ocaml-cohttp")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "074xis3wmr76gadh1ffmfzjfx13mw4kr2s6rkwqwzcl6l85n9x2z"))))
    (build-system dune-build-system)
    (arguments
     '(#:package "cohttp"
       #:test-target "cohttp_test/src"))
    (propagated-inputs
      (list ocaml-re
            ocaml-uri
            ocaml-uri-sexp
            ocaml-sexplib0
            ocaml-ppx-sexp-conv
            ocaml-stringext
            ocaml-base64))
    (native-inputs
      (list ocaml-fmt ocaml-jsonm ocaml-alcotest))
    (home-page "https://github.com/mirage/ocaml-cohttp")
    (synopsis "OCaml library for HTTP clients and servers")
    (description
      "Cohttp is an OCaml library for creating HTTP daemons.  It has a portable
HTTP parser, and implementations using various asynchronous programming
libraries.")
    (license license:isc)))

(define-public js-of-ocaml
  (package
    (name "js-of-ocaml")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocsigen/js_of_ocaml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iyhl9z57j53j2jvyqcwmxhbvy23l6g80aa0abmlgwam14yskspf"))))
    (build-system dune-build-system)
    (arguments `(#:test-target "."))
    (propagated-inputs
     (list ocaml-ppxlib
           ocaml-uchar
           ocaml-menhir
           ocaml-reactivedata
           ocaml-cmdliner
           ocaml-lwt
           ocaml-tyxml
           ocaml-re
           ocaml-uutf
           ocaml-graphics
           ocaml-yojson))
    (native-inputs
     ;; for tests
     (list node ocaml-ppx-expect ocaml-num))
    (properties `((upstream-name . "js_of_ocaml")))
    (home-page "https://ocsigen.org/js_of_ocaml/")
    (synopsis "Compiler from OCaml bytecode to Javascript")
    (description "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript.
It makes it possible to run pure OCaml programs in JavaScript environment like
browsers and Node.js.")
    (license license:lgpl2.1+)))

(define-public ocaml-bibtex2html
  (package
    (name "ocaml-bibtex2html")
    (version "1.99")
    (source
      (origin
        (method url-fetch)
        (uri "https://www.lri.fr/~filliatr/ftp/bibtex2html/bibtex2html-1.99.tar.gz")
        (sha256 (base32 "07gzrs4lfrkvbn48cgn2gn6c7cx3jsanakkrb2irj0gmjzfxl96j"))))
    (build-system ocaml-build-system)
    (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'patch-/bin/sh
            (lambda _
              (substitute* "configure" (("/bin/sh") (which "bash")))
              (setenv "HOME" (getcwd)) ;; mktexfmt needs writable home directory
              #t)))))
    (native-inputs
     `(("which" ,which)
       ("texlive" ,(texlive-updmap.cfg
                    (list texlive-fonts-ec texlive-preprint
                          texlive-hyperref texlive-bibtex)))))
    (propagated-inputs
     (list hevea))
    (home-page "https://www.lri.fr/~filliatr/bibtex2html/")
    (synopsis "BibTeX to HTML translator")
    (description "This package allows you to produce, from a set of
bibliography files in BibTeX format, a bibliography in HTML format.")
    (license license:gpl2)))
