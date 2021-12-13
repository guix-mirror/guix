;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021 jgart <jgart@dismail.de>
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

(define-module (gnu packages racket)
  #:use-module ((guix licenses)
                #:select (asl2.0 expat lgpl3+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages chez)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg))

;; Commentary:
;;
;; Here's how bootstrapping minimal Racket works:
;;
;;   - Racket BC [CGC] can be built with only a C compiler (except for
;;     one caveat discussed below).
;;   - Racket BC [3M] needs an existing Racket to run "xform",
;;     which transforms its own C source code to add additional annotations
;;     for the precise garbage collector.
;;   - Racket CS needs (bootfiles for) Racket's fork of Chez Scheme.
;;     It also needs an existing Racket to compile Racket-implemented
;;     parts of the runtime system to R6RS libraries.
;;   - Chez Scheme also needs bootfiles for itself, but Racket can simulate
;;     enough of Chez Scheme to load Racket's fork of the Chez Scheme compiler
;;     purely from source into Racket and apply the compiler to itself,
;;     producing the needed bootfiles (albeit very slowly).
;;     Any variant of Racket since version 7.1 can run the simulation.
;;
;; So, we build CGC to build 3M to build bootfiles and CS.
;;
;; One remaining bootstrapping limitation is that Racket's reader, module
;; system, and macro expander are implemented in Racket. For Racket CS,
;; they are compiled to R6RS libraries as discussed above. This note from the
;; README file applies to all such subsystems:
;;
;;     The Racket version must be practically the same as the current Racket
;;     verson, although it can be the Racket BC implementation (instead of
;;     the Racket CS implementation).
;;
;;     Unlike Chez Scheme boot files, the files generated in "schemified"
;;     are human-readable and -editable Scheme code. That provides a way
;;     out of bootstrapping black holes, even without BC.
;;
;; However, other Racket subsystems implemented in Racket for Racket CS
;; use older C implementations for Racket BC, whereas the reader, expander,
;; and module system were completely replaced with the Racket implementation
;; as of Racket 7.0.
;;
;; For Racket BC, the compiled "linklet" s-expressions (primitive modules)
;; are embeded in C as a static string constant. Eventually, they are further
;; compiled by the C-implemented Racket BC bytecode and JIT compilers.
;; (On platforms where Racket BC's JIT is not supported, yet another compiler
;; instead compiles the linklets to C code, but this is not a bootstrapping
;; issue.)
;;
;; Code:

(define cfg-flag:sh-for-rktio
  `(string-append "CPPFLAGS=-DGUIX_RKTIO_PATCH_BIN_SH="
                  (assoc-ref %build-inputs "sh")
                  "/bin/sh"))
(define cfg-flag:enable-lt
  `(string-append "--enable-lt="
                  (assoc-ref %build-inputs "libtool")
                  "/bin/libtool"))
(define cfg-flag:enable-racket
  `(let ((racket (assoc-ref %build-inputs "racket")))
     (string-append "--enable-racket="
                    racket
                    "/bin/racket")))

(define unpack-nanopass+stex
  ;; Copied from chez-scheme.
  ;; TODO: Eventually, we should refactor Chez Scheme
  ;; enough to share more directly, so that we can make
  ;; Racket's version of Chez avalable as a Guix package,
  ;; e.g. for architectures not supported upstream.
  ;; For now, we let Racket drive the Chez build process
  ;; other than this step.
  `(for-each (lambda (dep)
               (define src
                 (assoc-ref (or native-inputs inputs) dep))
               (copy-recursively src dep
                                 #:keep-mtime? #t))
             '("nanopass" "stex")))


(define-public racket-minimal
  (package
    (name "racket-minimal")
    (version "8.3")            ; note: remember to also update racket!
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/racket/racket")
             (commit (string-append "v" version))))
       (sha256
        "1i1jnv1wb0kanfg47hniafx2vhwjc33qqx66lq7wkf5hbmgsyws3")
       (file-name (git-file-name name version))
       (patches (search-patches "racket-minimal-sh-via-rktio.patch"))
       (modules '((guix build utils)))
       (snippet
        (with-imported-modules '((guix build utils))
          #~(begin
              ;; Unbundle Chez submodules.
              (with-directory-excursion "racket/src/ChezScheme"
                ;; Remove bundled libraries (copied from 'chez-scheme').
                (for-each delete-file-recursively
                          '("stex"
                            "nanopass"
                            "lz4"
                            "zlib")))
              ;; Unbundle libffi.
              (delete-file-recursively "racket/src/bc/foreign/libffi"))))))
    (inputs
     `(;; common to all racket-minimal variants:
       ("openssl" ,openssl)
       ("sqlite" ,sqlite)
       ("sh" ,bash-minimal)
       ;; only for CS
       ("zlib" ,zlib)
       ("zlib:static" ,zlib "static")
       ("lz4" ,lz4)
       ("lz4:static" ,lz4 "static")))
    (native-inputs
     `(("bootfiles" ,racket-bootstrap-chez-bootfiles)
       ,@(package-native-inputs racket-bootstrap-chez-bootfiles)))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-csonly"
             "--enable-libz"
             "--enable-liblz4"
             ,cfg-flag:enable-racket
             ,cfg-flag:sh-for-rktio)
       #:out-of-source? #true
       ;; Tests are in packages like racket-test-core and
       ;; main-distribution-test that aren't part of the main distribution.
       #:tests? #f
       #:modules ((ice-9 match)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-nanopass+stex
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (with-directory-excursion "racket/src/ChezScheme"
               ,unpack-nanopass+stex)
             #t))
         (add-after 'unpack-nanopass+stex 'unpack-bootfiles
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "racket/src/ChezScheme"
               (copy-recursively
                (string-append (assoc-ref inputs "bootfiles") "/boot")
                "boot"))
             #t))
         (add-before 'configure 'initialize-config.rktd
           (lambda* (#:key inputs #:allow-other-keys)
             (define (write-racket-hash alist)
               ;; inside must use dotted pair notation
               (display "#hash(")
               (for-each (match-lambda
                           ((k . v)
                            (format #t "(~s . ~s)" k v)))
                         alist)
               (display ")\n"))
             (mkdir-p "racket/etc")
             (with-output-to-file "racket/etc/config.rktd"
               (lambda ()
                 (write-racket-hash
                  `((lib-search-dirs
                     . (#f ,@(map (lambda (lib)
                                    (string-append (assoc-ref inputs lib)
                                                   "/lib"))
                                  '("openssl"
                                    "sqlite"))))
                    (build-stamp . "")
                    (catalogs
                     . (,(string-append
                          "https://download.racket-lang.org/releases/"
                          ,version
                          "/catalog/")
                        #f))))))
             #t))
         (add-before 'configure 'change-directory
           (lambda _
             (chdir "racket/src")
             #t))
         (add-after 'install 'remove-pkgs-directory
           ;; If the configured pkgs-dir exists, "pkgs.rktd" does not
           ;; exist, and a lock file does not exist, commands like
           ;; `raco pkg show` will try to create a lock file and fail
           ;; due to the read-only store.
           ;; Arguably this may be a bug in `pkg/private/lock`:
           ;; see <https://github.com/racket/racket/issues/3851>.
           ;; As a workaround, remove the directory.
           (lambda* (#:key outputs #:allow-other-keys)
             ;; rmdir because we want an error if it isn't empty
             (rmdir (string-append (assoc-ref outputs "out")
                                   "/share/racket/pkgs"))
             #t)))))
    (home-page "https://racket-lang.org")
    (synopsis "Racket without bundled packages such as DrRacket")
    (description
     "Racket is a general-purpose programming language in the Scheme family,
with a large set of libraries and a compiler based on Chez Scheme.  Racket is
also a platform for language-oriented programming, from small domain-specific
languages to complete language implementations.

The ``minimal Racket'' distribution includes just enough of Racket for you to
use @command{raco pkg} to install more.  Bundled packages, such as the
DrRacket IDE, are not included.")
    ;; https://download.racket-lang.org/license.html
    ;; The LGPL components are only used by Racket BC.
    (license (list asl2.0 expat))))


(define-public racket-minimal-bc-3m
  (hidden-package
   (package
     (inherit racket-minimal)
     (name "racket-minimal-bc-3m")
     (inputs
      (modify-inputs (package-inputs racket-minimal)
        (delete "zlib" "zlib:static" "lz4" "lz4:static")
        (prepend libffi ;; <- only for BC variants
                 )))
     (native-inputs
      `(("libtool" ,libtool)
        ("racket" ,(if (%current-target-system)
                       racket-minimal
                       racket-minimal-bc-cgc))))
     (arguments
      (substitute-keyword-arguments (package-arguments racket-minimal)
        ((#:configure-flags _ '())
         `(list "--enable-bconly"
                ,cfg-flag:enable-racket
                ,cfg-flag:enable-lt
                ,cfg-flag:sh-for-rktio))
        ((#:phases usual-phases)
         `(modify-phases ,usual-phases
            (delete 'unpack-nanopass+stex)
            (delete 'unpack-bootfiles)))))
     (synopsis "Minimal Racket with the BC [3M] runtime system")
     (description "The Racket BC (``before Chez'' or ``bytecode'')
implementation was the default before Racket 8.0.  It uses a compiler written
in C targeting architecture-independent bytecode, plus a JIT compiler on most
platforms.  Racket BC has a different C API and supports a slightly different
set of architectures than the current default runtime system, Racket CS (based
on ``Chez Scheme'').

This package is the normal implementation of Racket BC with a precise garbage
collector, 3M (``Moving Memory Manager'').")
     ;; https://download.racket-lang.org/license.html
     ;; The LGPL components are only used by Racket BC.
     (license (list lgpl3+ asl2.0 expat)))))


(define-public racket-minimal-bc-cgc
  (package
    (inherit racket-minimal-bc-3m)
    (name "racket-minimal-bc-cgc")
    (native-inputs
     (alist-delete "racket" (package-native-inputs racket-minimal-bc-3m)))
    (arguments
     (substitute-keyword-arguments (package-arguments racket-minimal-bc-3m)
       ((#:configure-flags _ '())
        `(list "--enable-cgcdefault"
               ,cfg-flag:enable-lt
               ,cfg-flag:sh-for-rktio))))
    (synopsis "Old Racket implementation used for bootstrapping")
    (description "This variant of the Racket BC (``before Chez'' or
``bytecode'') implementation is not recommended for general use.  It uses
CGC (a ``Conservative Garbage Collector''), which was succeeded as default in
PLT Scheme version 370 (which translates to 3.7 in the current versioning
scheme) by the 3M variant, which in turn was succeeded in version 8.0 by the
Racket CS implementation.

Racket BC [CGC] is primarily used for bootstrapping Racket BC [3M].  It may
also be used for embedding applications without the annotations needed in C
code to use the 3M garbage collector.")))


(define-public racket-bootstrap-chez-bootfiles
  (hidden-package
   (package
     (inherit racket-minimal)
     (name "racket-bootstrap-chez-bootfiles")
     (inputs `())
     (native-inputs
      `(("racket" ,(if (%current-target-system)
                       racket-minimal
                       racket-minimal-bc-3m))
        ("stex" ,@(assoc-ref (package-native-inputs chez-scheme) "stex"))
        ("nanopass" ,@(assoc-ref (package-native-inputs chez-scheme)
                                 "nanopass"))))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'unpack-nanopass+stex
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (with-directory-excursion "racket/src/ChezScheme"
                ,unpack-nanopass+stex)
              #t))
          (delete 'configure)
          (delete 'patch-generated-file-shebangs)
          (replace 'build
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (with-directory-excursion "racket/src/ChezScheme"
                (invoke (string-append (assoc-ref inputs "racket")
                                       "/bin/racket")
                        "rktboot/main.rkt"
                        "--dest" (assoc-ref outputs "out")))
              #t))
          (delete 'check)
          (delete 'install))))
     (synopsis "Chez Scheme bootfiles bootstrapped by Racket")
     (description "Chez Scheme is a self-hosting compiler: building it
requires ``bootfiles'' containing the Scheme-implemented portions compiled for
the current platform.  (Chez can then cross-compile bootfiles for all other
supported platforms.)

The Racket package @code{cs-bootstrap} (part of the main Racket Git
repository) implements enough of a Chez Scheme simulation to load the Chez
Scheme compiler purely from source into Racket and apply the compiler to
itself, thus bootstrapping Chez Scheme.  Bootstrapping takes about 10 times as
long as using an existing Chez Scheme, but @code{cs-bootstrap} supports Racket
7.1 and later, including the Racket BC variant.

Note that the generated bootfiles are specific to Racket's fork of Chez
Scheme, and @code{cs-bootstrap} does not currently support building upstream
Chez Scheme.")
     (license (list asl2.0)))))


(define %installer-mirrors
  ;; Source:
  ;; https://github.com/racket/racket-lang-org/blob/master/download/data.rkt#L58
  ;; Matthew Flatt says: "note that many are commented out"
  ;; INVARIANT: End with a trailing "/"!
  '("https://mirror.racket-lang.org/installers/"
    "https://www.cs.utah.edu/plt/installers/"
    "https://plt.cs.northwestern.edu/racket-mirror/"
    "https://mirror.csclub.uwaterloo.ca/racket/racket-installers/"
    ;; Universität Tübingen is using a self-signed HTTPS certificate:
    "http://mirror.informatik.uni-tuebingen.de/mirror/racket/"
    "https://racket.infogroep.be/"
    ))

(define %main-repo-main-distribution-pkgs
  ;; These are the packages developed in the main Racket Git repository
  ;; that are part of the main distribution.
  '("at-exp-lib"
    "base"
    "compiler-lib"
    ;; NOT "compiler-test"
    "compiler"
    "net-doc"
    "net-lib"
    ;; NOT "net-test"
    "net"
    ;; NOT "plt-services"
    ;; NOT "racket-benchmarks"
    ;; NOT "racket-build-guide"
    "racket-doc"
    "racket-index"
    "racket-lib"
    ;; NOT "racket-test-core"
    ;; NOT "racket-test-extra"
    ;; NOT "racket-test"
    "zo-lib"))


(define-public racket
  (package
    (inherit racket-minimal)
    (name "racket")
    (version (package-version racket-minimal)) ; needed for origin uri to work
    (source
     (origin
       (method url-fetch)
       (uri (map (lambda (base)
                   (string-append base version "/racket-src.tgz"))
                 %installer-mirrors))
       (sha256
        (base32
         "0jdr0y7scvv2a3sq456ifrgq0yfsbiwavdf2m86zmrapp481mby4"))
       (snippet
        #~(begin
            (use-modules (guix build utils)
                         (ice-9 match)
                         (ice-9 regex))
            ;; unbundle minimal Racket
            (for-each delete-file-recursively
                      '("collects"
                        "doc"
                        "etc"
                        "README"
                        "src"))
            ;; unbundle package sources included elsewhere
            (with-directory-excursion "share/pkgs"
              (for-each delete-file-recursively
                        '#+%main-repo-main-distribution-pkgs))
            #t))))
    (inputs
     `(("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("glib" ,glib)
       ("glu" ,glu)
       ("gmp" ,gmp)
       ("gtk+" ,gtk+)                   ; propagates gdk-pixbuf+svg
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("mesa" ,mesa)
       ("mpfr" ,mpfr)
       ("pango" ,pango)
       ("unixodbc" ,unixodbc)
       ("libedit" ,libedit)))
    (native-inputs
     `(("racket" ,racket-minimal)
       ("extend-layer" ,extend-layer)
       ("main-repo" ,(package-source racket-minimal))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'unpack-packages
           (let ((unpack (assoc-ref %standard-phases 'unpack)))
             (lambda* (#:key  native-inputs inputs outputs #:allow-other-keys)
               (let* ((racket (assoc-ref (or native-inputs inputs) "racket"))
                      (prefix (assoc-ref outputs "out"))
                      (pkgs-dir (string-append prefix "/share/racket/pkgs")))
                 (mkdir-p pkgs-dir)
                 (copy-recursively
                  "share/links.rktd"
                  (string-append prefix "/share/racket/links.rktd"))
                 (copy-recursively "share/pkgs" pkgs-dir)
                 ;; NOTE: unpack changes the working directory
                 (unpack #:source (assoc-ref (or native-inputs inputs)
                                             "main-repo"))
                 (for-each (lambda (pkg)
                             (define dest (string-append pkgs-dir "/" pkg))
                             (mkdir-p dest)
                             (copy-recursively (string-append "pkgs/" pkg)
                                               dest))
                           ',%main-repo-main-distribution-pkgs)
                 #t))))
         (replace 'configure
           (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
             (let ((racket (assoc-ref (or native-inputs inputs) "racket"))
                   (prefix (assoc-ref outputs "out")))
               (apply invoke
                      (string-append racket "/bin/racket")
                      (assoc-ref inputs "extend-layer")
                      racket
                      prefix
                      (map
                       (lambda (lib)
                         (string-append (assoc-ref inputs lib) "/lib"))
                       '("cairo"
                         "fontconfig"
                         "glib"
                         "glu"
                         "gmp"
                         "gtk+"
                         "libjpeg"
                         "libpng"
                         "libx11"
                         "mesa"
                         "mpfr"
                         "pango"
                         "unixodbc"
                         "libedit")))
               #t)))
         (replace 'build
           (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
             (invoke (string-append (assoc-ref (or native-inputs inputs)
                                               "racket")
                                    "/bin/racket")
                     "--config"
                     (string-append (assoc-ref outputs "out")
                                    "/etc/racket")
                     "-l"
                     "raco"
                     "setup")
             #t))
         (delete 'install))
       ;; we still don't have these:
       #:tests? #f))
    (synopsis "Programmable programming language in the Scheme family")
    (description
     "Racket is a general-purpose programming language in the Scheme family,
with a large set of libraries and a compiler based on Chez Scheme.  Racket is
also a platform for language-oriented programming, from small domain-specific
languages to complete language implementations.

The main Racket distribution comes with many bundled packages, including the
DrRacket IDE, libraries for GUI and web programming, and implementations of
languages such as Typed Racket, R5RS and R6RS Scheme, Algol 60, and Datalog.")))


(define extend-layer
  (scheme-file
   "extend-layer.rkt"
   `(module
     extend-layer racket/base
     (require racket/cmdline
              racket/match
              racket/file
              racket/list
              racket/pretty)
     (define config-file-pth
       "etc/racket/config.rktd")
     (define (build-path-string . args)
       (path->string (apply build-path args)))
     (define rx:racket
       ;; Guile's reader doesn't support #rx"racket"
       (regexp "racket"))
     (command-line
      #:args (parent-layer prefix . lib-dir*)
      (let* ([config
              (for/fold
               ([config (file->value (build-path parent-layer
                                                 config-file-pth))])
               ([spec (in-list
                       '((lib-dir lib-search-dirs "lib/racket")
                         (share-dir share-search-dirs "share/racket")
                         (links-file
                          links-search-files
                          "share/racket/links.rktd")
                         (pkgs-dir pkgs-search-dirs "share/racket/pkgs")
                         (bin-dir bin-search-dirs "bin")
                         (man-dir man-search-dirs "share/man")
                         (doc-dir doc-search-dirs "share/doc/racket")
                         (include-dir
                          include-search-dirs
                          "include/racket")))])
               (match-define (list main-key search-key pth) spec)
               (hash-set*
                config
                main-key
                (build-path-string prefix pth)
                search-key
                (list* #f
                       (hash-ref config
                                 main-key
                                 (build-path-string parent-layer pth))
                       (filter values (hash-ref config search-key null)))))]
             [config
              (hash-set config
                        'apps-dir
                        (build-path-string prefix "share/applications"))]
             [config
              ;; place new foreign lib-search-dirs before old
              ;; foreign dirs, but after Racket layers
              (let-values
                  ([(rkt extra)
                    (partition (lambda (pth)
                                 (or (not pth)
                                     (regexp-match? rx:racket pth)))
                               (hash-ref config 'lib-search-dirs))])
                (hash-set config
                          'lib-search-dirs
                          (append rkt
                                  lib-dir*
                                  extra)))]
             [bin-dir
              (hash-ref config 'bin-dir)]
             [config
              (hash-set* config
                         'config-tethered-console-bin-dir bin-dir
                         'config-tethered-gui-bin-dir bin-dir)]
             [new-config-pth
              (build-path prefix config-file-pth)])
        (make-parent-directory* new-config-pth)
        (call-with-output-file*
         new-config-pth
         (lambda (out)
           (pretty-write config out))))))))
