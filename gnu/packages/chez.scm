;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages chez)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:select (gpl2+ gpl3+ lgpl2.0+ lgpl2.1+ asl2.0 bsd-3 expat
                          public-domain))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define nanopass
  (let ((version "1.9"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/nanopass/nanopass-framework-scheme.git")
            (commit (string-append "v" version))))
      (sha256 (base32 "0lrngdna6w7v9vlp1a873hgwrwsz2p0pgkccswa4smzvdyhgfsri"))
      (file-name (git-file-name "nanopass" version)))))

(define stex
  (let ((version "1.2.1"))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/dybvig/stex/archive"
            "/v" version ".tar.gz"))
      (sha256 (base32 "03pl3f668h24dn51vccr1sj5lsba9zq3j37bnxjvdadcdaj4qy5z"))
      (file-name (string-append "stex-" version ".tar.gz")))))

(define-public chez-scheme
  (package
    (name "chez-scheme")
    (version "9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cisco/ChezScheme/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32 "135991hspq0grf26pvl2lkwhp92yz204h6rgiwyym0x6v0xzknd1"))
       (file-name (string-append "chez-scheme-" version ".tar.gz"))
       (modules '((guix build utils)))
       (snippet
        ;; Fix compilation with glibc >= 2.26, which removed xlocale.h.
        '(begin
           (substitute* "c/expeditor.c"
             (("xlocale\\.h") "locale.h"))
           #t))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)
       ("libx11" ,libx11)
       ("xorg-rgb" ,xorg-rgb)
       ("nanopass" ,nanopass)
       ("zlib" ,zlib)
       ("zlib:static" ,zlib "static")
       ("stex" ,stex)))
    (native-inputs
     `(("texlive" ,(texlive-union (list texlive-latex-oberdiek
                                        texlive-generic-epsf)))
       ("ghostscript" ,ghostscript)
       ("netpbm" ,netpbm)))
    (native-search-paths
     (list (search-path-specification
            (variable "CHEZSCHEMELIBDIRS")
            (files (list (string-append "lib/csv" version "-site"))))))
    (outputs '("out" "doc"))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:test-target "test"
       #:configure-flags
       (list ,(match (or (%current-target-system) (%current-system))
                ("x86_64-linux" '(list "--machine=ta6le"))
                ("i686-linux" '(list "--machine=ti3le"))
                ;; Let autodetection have its attempt on other architectures.
                (_
                 '())))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-processor-detection
           (lambda _ (substitute* "configure"
                       (("uname -a") "uname -m"))
                   #t))
         (add-after 'unpack 'patch-broken-documentation
           (lambda _
             ;; Work around an oversight in the 9.5 release tarball that causes
             ;; building the documentation to fail. This should be fixed in the
             ;; next one; see <https://github.com/cisco/ChezScheme/issues/209>.
             (substitute* "csug/copyright.stex"
               (("\\\\INSERTREVISIONMONTHSPACEYEAR" )
                "October 2017"))       ; tarball release date
             #t))
         ;; Adapt the custom 'configure' script.
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (nanopass (assoc-ref inputs "nanopass"))
                   (stex (assoc-ref inputs "stex"))
                   (zlib (assoc-ref inputs "zlib"))
                   (zlib-static (assoc-ref inputs "zlib:static"))
                   (unpack (assoc-ref %standard-phases 'unpack))
                   (patch-source-shebangs
                    (assoc-ref %standard-phases 'patch-source-shebangs)))
               (map (match-lambda
                      ((src orig-name new-name)
                       (with-directory-excursion "."
                         (apply unpack (list #:source src))
                         (apply patch-source-shebangs (list #:source src)))
                       (delete-file-recursively new-name)
                       (invoke "mv" orig-name new-name)))
                    `((,nanopass "source" "nanopass")
                      (,stex "stex-1.2.1" "stex")))
               ;; The Makefile wants to download and compile "zlib".  We patch
               ;; it to use the one from our 'zlib' package.
               (substitute* "configure"
                 (("rmdir zlib .*$") "echo \"using system zlib\"\n"))
               (substitute* (find-files "./c" "Mf-[a-zA-Z0-9.]+")
                 (("\\$\\{Kernel\\}: \\$\\{kernelobj\\} \\.\\./zlib/libz\\.a")
                  "${Kernel}: ${kernelobj}")
                 (("ld ([-a-zA-Z0-9_${} ]+) \\.\\./zlib/libz\\.a" all args)
                  (string-append "ld " args " " zlib-static "/lib/libz.a"))
                 (("\\(cd \\.\\./zlib; ([-a-zA-Z0-9=./ ]+))")
                  (which "true")))
               (substitute* (find-files "mats" "Mf-.*")
                 (("^[[:space:]]+(cc ) *") "\tgcc "))
               (substitute*
                   (find-files "." (string-append
                                    "("
                                    "Mf-[a-zA-Z0-9.]+"
                                    "|Makefile[a-zA-Z0-9.]*"
                                    "|checkin"
                                    "|stex\\.stex"
                                    "|newrelease"
                                    "|workarea"
                                    ;;"|[a-zA-Z0-9.]+\\.ms" ; guile can't read
                                    ")"))
                 (("/bin/rm") (which "rm"))
                 (("/bin/ln") (which "ln"))
                 (("/bin/cp") (which "cp")))
               (substitute* "makefiles/installsh"
                 (("/bin/true") (which "true")))
               (substitute* "stex/Makefile"
                 (("PREFIX=/usr") (string-append "PREFIX=" out)))
               (invoke "./configure" "--threads"
                       (string-append "--installprefix=" out)))))
         ;; Installation of the documentation requires a running "chez".
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((doc (string-append (assoc-ref outputs "doc")
                                       "/share/doc/" ,name "-" ,version)))
               (invoke "make" "docs")
               (with-directory-excursion "csug"
                 (substitute* "Makefile"
                   ;; The ‘installdir=’ can't be overruled on the command line.
                   (("/tmp/csug9") doc)
                   ;; $m is the ‘machine type’, e.g. ‘ta6le’ on x86_64, but is
                   ;; set incorrectly for some reason, e.g. to ‘a6le’ on x86_64.
                   ;; Avoid the whole mess by running the (machine-independent)
                   ;; ‘installsh’ script at its original location.
                   (("\\$m/installsh") "makefiles/installsh"))
                 (invoke "make" "install")
                 (install-file "csug.pdf" doc))
               (with-directory-excursion "release_notes"
                 (install-file "release_notes.pdf" doc))
               #t)))
         ;; The binary file name is called "scheme" as the one from MIT/GNU
         ;; Scheme.  We add a symlink to use in case both are installed.
         (add-after 'install 'install-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (name "chez-scheme"))
               (symlink (string-append bin "/scheme")
                        (string-append bin "/" name))
               (map (lambda (file)
                      (symlink file (string-append (dirname file)
                                                   "/" name ".boot")))
                    (find-files lib "scheme.boot"))
               #t)))
         (add-before 'reset-gzip-timestamps 'make-manpages-writable
           (lambda* (#:key outputs #:allow-other-keys)
             (map (lambda (file)
                    (make-file-writable file))
                  (find-files (string-append (assoc-ref outputs "out")
                                             "/share/man")
                              ".*\\.gz$"))
             #t)))))
    ;; According to the documentation MIPS is not supported.
    ;; Cross-compiling for the Raspberry Pi is supported, but not native ARM.
    (supported-systems (fold delete %supported-systems
                             '("mips64el-linux" "armhf-linux")))
    (home-page "http://www.scheme.com")
    (synopsis "R6RS Scheme compiler and run-time")
    (description
     "Chez Scheme is a compiler and run-time system for the language of the
Revised^6 Report on Scheme (R6RS), with numerous extensions.  The compiler
generates native code for each target processor, with support for x86, x86_64,
and 32-bit PowerPC architectures.")
    (license asl2.0)))

(define-public chez-srfi
  (package
    (name "chez-srfi")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fedeinthemix/chez-srfi.git")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1vgn984mj2q4w6r2q66h7qklp2hrh85wwh4k9yisga5fi0ps7myf"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     `(("chez-scheme" ,chez-scheme)))
    (arguments
     `(#:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)))
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "https://github.com/fedeinthemix/chez-srfi")
    (synopsis "SRFI libraries for Chez Scheme")
    (description
     "This package provides a collection of SRFI libraries for Chez Scheme.")
    (license expat)))

(define-public chez-web
  (let ((commit "5fd177fe53f31f466bf88720d03c95a3711a8bea")
        (revision "1"))
    (package
      (name "chez-web")
      ;; Release 2.0 is different and doesn't work.
      (version (git-version "2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arcfide/ChezWEB.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1dq25qygyncbfq4kwwqqgyyakfqjwhp5q23vrf3bff1p66nyfl3b"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("chez-scheme" ,chez-scheme)
         ("ghostscript" ,ghostscript)
         ("texlive" ,(texlive-union (list texlive-latex-oberdiek
                                          texlive-generic-epsf
                                          texlive-metapost
                                          texlive-fonts-charter
                                          texlive-generic-pdftex
                                          texlive-context-base
                                          texlive-fonts-cm
                                          texlive-tex-plain)))))
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" %output)
                            (string-append "DOCDIR=" %output "/share/doc/"
                                           ,name "-" ,version)
                            (string-append "LIBDIR=" %output "/lib/chezweb")
                            (string-append "TEXDIR=" %output "/share/texmf-local"))
                      #:tests? #f        ; no tests
                      #:phases
                      (modify-phases %standard-phases
                        (add-before 'build 'set-HOME
                          (lambda _
                            ;; FIXME: texlive-union does not find the built
                            ;; metafonts, so it tries to generate them in HOME.
                            (setenv "HOME" "/tmp")
                            #t))
                        ;; This package has a custom "bootstrap" script that
                        ;; is meant to be run from the Makefile.
                        (delete 'bootstrap)
                        (replace 'configure
                          (lambda* _
                            (copy-file "config.mk.template" "config.mk")
                            (substitute* "tangleit"
                              (("\\./cheztangle\\.ss" all)
                               (string-append "chez-scheme --program " all)))
                            (substitute* "weaveit"
                              (("mpost chezweb\\.mp")
                               "mpost --tex=tex chezweb.mp")
                              (("\\./chezweave" all)
                               (string-append "chez-scheme --program " all)))
                            (substitute* "installit"
                              (("-g \\$GROUP -o \\$OWNER") ""))
                            #t)))))
      (home-page "https://github.com/arcfide/ChezWEB")
      (synopsis "Hygienic Literate Programming for Chez Scheme")
      (description "ChezWEB is a system for doing Knuthian style WEB
programming in Scheme.")
      (license expat))))

(define-public chez-sockets
  (let ((commit "bce96881c06bd69a6757a6bff139744153924140")
        (revision "1"))
    (package
      (name "chez-sockets")
      (version (string-append "0.0-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arcfide/chez-sockets.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32 "1n5fbwwz51fdzvjackgmnsgh363g9inyxv7kmzi0469cwavwcx5m"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("chez-scheme" ,chez-scheme)
         ("chez-web" ,chez-web)
         ("texlive" ,(texlive-union (list texlive-generic-pdftex)))))
      (arguments
       `(#:tests? #f              ; no tests
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs inputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (chez-web (assoc-ref inputs "chez-web"))
                      (chez (assoc-ref inputs "chez-scheme"))
                      (chez-h (dirname (car (find-files chez "scheme\\.h")))))
                 (substitute* "Makefile"
                   (("(SCHEMEH=).*$" all var)
                    (string-append var chez-h)))
                 #t)))
           (add-before 'build 'tangle
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "TEXINPUTS"
                       (string-append
                        (getcwd) ":"
                        (assoc-ref inputs "chez-web") "/share/texmf-local/tex/generic:"
                        ":"))
               ;; just using "make" tries to build the .c files before
               ;; they are created.
               (and (invoke "make" "sockets")
                    (invoke "make"))))
           (replace 'build
             (lambda* (#:key outputs inputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (chez-site (string-append out "/lib/csv"
                                                ,(package-version chez-scheme)
                                                "-site/arcfide")))
                 ;; make sure Chez Scheme can find the shared libraries.
                 (substitute* "sockets.ss"
                   (("(load-shared-object) \"(socket-ffi-values\\.[sd][oy].*)\""
                     all cmd so)
                    (string-append cmd " \"" chez-site "/" so "\""))
                   (("sockets-stub\\.[sd][oy].*" all)
                    (string-append chez-site "/" all)))
                 ;; to compile chez-sockets, the .so files must be
                 ;; installed (because of the absolute path we
                 ;; inserted above).
                 (for-each (lambda (f d) (install-file f d))
                           '("socket-ffi-values.so" "sockets-stub.so")
                           (list chez-site chez-site))
                 (zero? (system "echo '(compile-file \"sockets.sls\")' | scheme -q")))))
           (replace 'install
             (lambda* (#:key outputs inputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib/chez-sockets"))
                      (doc (string-append out "/share/doc/" ,name "-" ,version))
                      (chez-site (string-append out "/lib/csv"
                                                ,(package-version chez-scheme)
                                                "-site/arcfide")))
                 (for-each (lambda (f d) (install-file f d))
                           '("sockets.pdf" "sockets.so")
                           (list doc chez-site))
                 #t))))))
      (home-page "https://github.com/arcfide/chez-sockets")
      (synopsis "Extensible sockets library for Chez Scheme")
      (description "Chez-sockets is an extensible sockets library for
Chez Scheme.")
      (license expat))))

;; Help function for Chez Scheme to add the current path to
;; CHEZSCHEMELIBDIRS.
(define chez-configure
  '(lambda _
     (let ((chez-env (getenv "CHEZSCHEMELIBDIRS")))
       (setenv "CHEZSCHEMELIBDIRS"
               (if chez-env
                   (string-append ".:" chez-env)
                   "."))
       #t)))

;; Help function to define make flags for some Chez Scheme custom make
;; files.
(define (chez-make-flags name version)
  `(let ((out (assoc-ref %outputs "out")))
     (list (string-append "PREFIX=" out)
           (string-append "DOCDIR=" out "/share/doc/"
                          ,name "-" ,version))))

(define-public chez-matchable
  (package
    (name "chez-matchable")
    (version "20160306")
    (home-page "https://github.com/fedeinthemix/chez-matchable")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (sha256
        (base32 "02qn7x348p23z1x5lwhkyj7i8z6mgwpzpnwr8dyina0yzsdkr71s"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     `(("chez-srfi" ,chez-srfi))) ; for tests
    (native-inputs
     `(("chez-scheme" ,chez-scheme)))
    (arguments
     `(#:make-flags ,(chez-make-flags name version)
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (replace 'configure ,chez-configure))))
    (synopsis "Portable hygienic pattern matcher for Scheme")
    (description "This package provides a superset of the popular Scheme
@code{match} package by Andrew Wright, written in fully portable
@code{syntax-rules} and thus preserving hygiene.")
    (license public-domain)))

(define-public chez-irregex
  (package
    (name "chez-irregex")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fedeinthemix/chez-irregex.git")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0jh6piylw545j81llay9wfivgpv6lcnwd81gm4w17lkasslir50q"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     `(("chez-matchable" ,chez-matchable))) ; for tests
    (propagated-inputs
     `(("chez-srfi" ,chez-srfi))) ; for irregex-utils
    (native-inputs
     `(("chez-scheme" ,chez-scheme)))
    (arguments
     `(#:make-flags ,(chez-make-flags name version)
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (replace 'configure ,chez-configure))))
    (home-page "https://github.com/fedeinthemix/chez-irregex")
    (synopsis "Portable regular expression library for Scheme")
    (description "This package provides a portable and efficient
R[4567]RS implementation of regular expressions, supporting both POSIX
syntax with various (irregular) PCRE extensions, as well as SCSH's SRE
syntax, with various aliases for commonly used patterns.")
    (license bsd-3)))

(define-public chez-fmt
  (package
    (name "chez-fmt")
    (version "0.8.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://synthcode.com/scheme/fmt/fmt-" version ".tar.gz"))
       (sha256
        (base32 "1zxqlw1jyg85yzclylh8bp2b3fwcy3l3xal68jw837n5illvsjcl"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("chez-srfi" ,chez-srfi))) ; for irregex-utils
    (native-inputs
     `(("chez-scheme" ,chez-scheme)))
    (arguments
     `(#:make-flags ,(chez-make-flags name version)
       #:test-target "chez-check"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure ,chez-configure)
         (replace 'build
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "chez-build" make-flags)))
         (replace 'install
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "chez-install" make-flags))))))
    (home-page "http://synthcode.com/scheme/fmt")
    (synopsis "Combinator formatting library for Chez Scheme")
    (description "This package provides a library of procedures for
formatting Scheme objects to text in various ways, and for easily
concatenating, composing and extending these formatters efficiently
without resorting to capturing and manipulating intermediate
strings.")
    (license bsd-3)))

(define-public chez-mit
  (package
    (name "chez-mit")
    (version "0.1")
    (home-page "https://github.com/fedeinthemix/chez-mit")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/archive/v" version ".tar.gz"))
       (sha256
        (base32 "1p11q061znwxzxrxg3vw4dbsnpv1dav12hjhnkrjnzyyjvvdm2kn"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs
     `(("chez-srfi" ,chez-srfi))) ; for tests
    (native-inputs
     `(("chez-scheme" ,chez-scheme)))
    (arguments
     `(#:make-flags ,(chez-make-flags name version)
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (replace 'configure ,chez-configure))))
    (synopsis "MIT/GNU Scheme compatibility library for Chez Scheme")
    (description "This package provides a set of MIT/GNU Scheme compatibility
libraries for Chez Scheme.  The main goal was to provide the functionality
required to port the program 'Scmutils' to Chez Scheme.")
    (license gpl3+)))

(define-public chez-scmutils
  (package
    (name "chez-scmutils")
    (version "0.1")
    (home-page "https://github.com/fedeinthemix/chez-scmutils")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/archive/v" version ".tar.gz"))
       (sha256
        (base32 "1a5j61pggaiwl1gl6m038rcy5n8r2sj5nyjmz86jydx97mm5i8hj"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs
     `(("chez-srfi" ,chez-srfi)))      ; for tests
    (native-inputs
     `(("chez-scheme" ,chez-scheme)))
    (propagated-inputs
     `(("chez-mit" ,chez-mit)
       ("chez-srfi" ,chez-srfi)))
    (arguments
     `(#:make-flags ,(chez-make-flags name version)
       #:tests? #f ; no test suite
       #:phases
       (modify-phases %standard-phases
         (replace 'configure ,chez-configure)
         ;; Since the documentation is lacking, we install the source
         ;; code.  For things to work correctly we have to replace
         ;; relative paths by absolute ones in 'include' forms.  This
         ;; in turn requires us to compile the files in the final
         ;; destination.
         (delete 'build)
         (add-after 'install 'install-src
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (zero? (apply system* "make" "install-src" make-flags))))
         (add-after 'install-src 'absolute-path-in-scm-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (file)
                           (substitute* file
                             (("include +\"\\./scmutils")
                              (string-append "include \"" (dirname file)))))
                         (find-files out "\\.sls"))
               (for-each (lambda (file)
                           (substitute* file
                             (("include +\"\\./scmutils/simplify")
                              (string-append "include \"" (dirname file)))))
                         (find-files out "fbe-syntax\\.scm"))
               #t)))
         (add-after 'absolute-path-in-scm-files 'build
           (lambda* (#:key outputs (make-flags '()) #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (mk-file (car (find-files out "Makefile"))))
               (with-directory-excursion (dirname mk-file)
                 (zero? (apply system* "make" "build" make-flags))))))
         (add-after 'build 'clean-up
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (for-each delete-file
                         (find-files out "Makefile|compile-all\\.ss"))))))))
    (synopsis "Port of MIT/GNU Scheme Scmutils to Chez Scheme")
    (description "This package provides a port of the MIT/GNU Scheme
Scmutils program to Chez Scheme.  The port consists of a set of
libraries providing most of the functionality of the original.")
    (license gpl3+)))
