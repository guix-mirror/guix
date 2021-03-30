;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (gnu packages javascript)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages web)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system minify)
  #:use-module (guix utils))

(define-public cjson
  (package
    (name "cjson")
    (version "1.7.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DaveGamble/cJSON")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1rlnailhjm180zb6pc17jwphjwivw8kfpqgixjfgq4iyryq46sah"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DENABLE_CJSON_UTILS=On")))
    (home-page "https://github.com/DaveGamble/cJSON")
    (synopsis "JSON parser written in ANSI C")
    (description "This library provides a portable embeddable JSON parser.")
    (license license:expat)))

(define-public js-context-menu
  (package
    (name "js-context-menu")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/zorkow/context-menu")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1q063l6477z285j6h5wvccp6iswvlp0jmb96sgk32sh0lf7nhknh"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (chdir (assoc-ref %build-inputs "source"))
         (let ((target (string-append %output "/share/javascript/context-menu")))
           (apply invoke (string-append (assoc-ref %build-inputs "esbuild")
                                        "/bin/esbuild")
                  "--bundle"
                  "--tsconfig=tsconfig.json"
                  (string-append "--outdir=" target)
                  (find-files "ts" "\\.ts$"))))))
    (native-inputs
     `(("esbuild" ,esbuild)))
    (home-page "https://github.com/zorkow/context-menu")
    (synopsis "Generic context menu")
    (description "This package provides a reimplementation of the MathJax
context menu in TypeScript.")
    (license license:asl2.0)))

(define-public font-mathjax
  (package
    (name "font-mathjax")
    (version "2.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mathjax/MathJax")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "127j12g7v2hx6k7r00b8cp49s7nkrwhxy6l8p03pw34xpxbgbimm"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((install-directory (string-append %output "/share/fonts/mathjax")))
           (mkdir-p install-directory)
           (copy-recursively (string-append (assoc-ref %build-inputs "source")
                                            "/fonts")
                             install-directory)))))
    (home-page "https://www.mathjax.org/")
    (synopsis "Fonts for MathJax")
    (description "This package contains the fonts required for MathJax.")
    (license license:asl2.0)))

(define-public js-mathjax
  (package
    (inherit font-mathjax)
    (name "js-mathjax")
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match)
                      (ice-9 popen)
                      (ice-9 regex))
         (set-path-environment-variable
          "PATH" '("bin") (map (match-lambda
                                 ((_ . input)
                                  input))
                               %build-inputs))
         (set-path-environment-variable
          "GUIX_LOCPATH" '("lib/locale")
          (list (assoc-ref %build-inputs "glibc-utf8-locales")))
         (setenv "LANG" "en_US.UTF-8")
         (let ((install-directory (string-append %output "/share/javascript/mathjax")))
           (copy-recursively (string-append (assoc-ref %build-inputs "source") "/unpacked")
                             "MathJax-unpacked")
           (mkdir-p install-directory)
           (symlink (string-append (assoc-ref %build-inputs "font-mathjax")
                                   "/share/fonts/mathjax")
                    (string-append install-directory "/fonts"))

           (for-each
            (lambda (file)
              (let ((installed (string-append install-directory
                                              ;; remove prefix "./MathJax-unpacked"
                                              (string-drop file 18))))
                (format #t "~a -> ~a~%" file installed)
                (cond
                 ((string-match "\\.js$" file)
                  (mkdir-p (dirname installed))
                  (let ((minified (open-pipe* OPEN_READ "uglify-js" file)))
                    (call-with-output-file installed
                      (lambda (port)
                        (dump-port minified port)))

                    (let ((exit (close-pipe minified)))
                      (unless (zero? exit)
                        (error "dear, uglify-js failed" exit)))))
                 (else
                  (install-file file (dirname installed))))))
            (find-files "."))

           #t))))
    (native-inputs
     `(("font-mathjax" ,font-mathjax)
       ("glibc-utf8-locales" ,glibc-utf8-locales)
       ("uglify-js" ,uglify-js)
       ,@(package-native-inputs font-mathjax)))
    (synopsis "JavaScript display engine for LaTeX, MathML, and AsciiMath")
    (description "MathJax is a JavaScript display engine for LaTeX, MathML,
and AsciiMath notation that works in all modern browsers.  It requires no
plugins or software to be installed on the browser.  So the page author can
write web documents that include mathematics and be confident that readers will
be able to view it naturally and easily.")))

(define-public js-commander
  (package
    (name "js-commander")
    (version "6.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/tj/commander.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "126m25s6mxpxmdj4aw5awz06b47r8r798lcf1c5bnmmh39cik5i1"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (chdir (assoc-ref %build-inputs "source"))
         (let ((esbuild (string-append (assoc-ref %build-inputs "esbuild")
                                       "/bin/esbuild"))
               (target (string-append %output "/share/javascript/commander")))
           (invoke esbuild
                   "--bundle"
                   "--minify"
                   "--tsconfig=tsconfig.json"
                   "--platform=node"
                   (string-append "--outfile=" target "/index.min.js")
                   "index.js")))))
    (native-inputs
     `(("esbuild" ,esbuild)))
    (home-page "https://github.com/tj/commander.js")
    (synopsis "Library for node.js command-line interfaces")
    (description "Commander.js aims to be the complete solution for node.js
command-line interfaces.  ")
    (license license:expat)))

(define-public js-xmldom-sre
  ;; This commit corresponds to the untagged release 0.1.32
  (let ((commit "3c79325bc2c9e5d27e3ba44b680fa8c5dd6a381d")
        (revision "1"))
    (package
      (name "js-xmldom-sre")
      (version "0.1.32")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zorkow/xmldom/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0a88v0id3mjflpvjqhv8a28br0xvaaszxbf7alg6pxfbnkb69yyq"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (chdir (assoc-ref %build-inputs "source"))
           (let ((esbuild (string-append (assoc-ref %build-inputs "esbuild")
                                         "/bin/esbuild"))
                 (target (string-append %output "/share/javascript/xmldom-sre")))
             (invoke esbuild
                     "--bundle"
                     "--minify"
                     "--platform=node"
                     (string-append "--outfile=" target "/dom-parser.min.js")
                     "dom-parser.js")))))
      (native-inputs
       `(("esbuild" ,esbuild)))
      (home-page "https://github.com/zorkow/xmldom/")
      (synopsis "DOM parser and XML serializer")
      (description "This is a fork of the xmldom library.  It allows the use
of wicked-good-xpath together with xmldom.")
      ;; One of these licenses may be selected.
      (license (list license:expat
                     license:lgpl2.0)))))

(define-public js-respond
  (package
    (name "js-respond")
    (version "1.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/scottjehl/Respond")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00xid731rirc7sdy1gc8qal3v9g0agr2qx15hm4x97l1lcbylyn2"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("src/matchmedia.addListener.js"
                            "src/matchmedia.polyfill.js"
                            "src/respond.js")))
    (home-page "https://github.com/scottjehl/Respond")
    (synopsis "Polyfill for min/max-width CSS3 Media Queries")
    (description "The goal of this script is to provide a fast and lightweight
script to enable responsive web designs in browsers that don't support CSS3
Media Queries.")
    (license license:expat)))

(define-public js-html5shiv
  (package
    (name "js-html5shiv")
    (version "3.7.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/aFarkas/html5shiv")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y1c5nyq0brl9fjdihhax33vks4s1ij9iv113879sg3zflmgqpd0"))))
    (build-system minify-build-system)
    (home-page "https://github.com/aFarkas/html5shiv")
    (synopsis "Enable HTML5 sectioning elements in legacy browsers")
    (description "The HTML5 Shiv enables use of HTML5 sectioning elements in
legacy Internet Explorer and provides basic HTML5 styling for Internet
Explorer 6-9, Safari 4.x (and iPhone 3.x), and Firefox 3.x.")
    ;; From the file "MIT and GPL2 licenses.md":
    ;;
    ;;   This software is licensed under a dual license system (MIT or GPL
    ;;   version 2). This means you are free to choose with which of both
    ;;   licenses (MIT or GPL version 2) you want to use this library.
    (license (list license:expat license:gpl2))))

(define-public js-json2
  (let ((commit "031b1d9e6971bd4c433ca85e216cc853f5a867bd")
        (revision "1"))
    (package
      (name "js-json2")
      (version (string-append "2016-10-28." revision "-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/douglascrockford/JSON-js")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1fvb6b2y5sd3sqdgcj683sdxcbxdii34q0ysc9wg0dq1sy81l11v"))))
      (build-system minify-build-system)
      (arguments
       `(#:javascript-files '("json2.js"
                              "json_parse.js"
                              "json_parse_state.js"
                              "cycle.js")))
      (home-page "https://github.com/douglascrockford/JSON-js")
      (synopsis "JSON encoders and decoders")
      (description "The files in this collection implement JSON
encoders/decoders in JavaScript.

@code{json2.js}: This file creates a JSON property in the global object, if
there isn't already one, setting its value to an object containing a stringify
method and a parse method.  The @code{parse} method uses the @code{eval}
method to do the parsing, guarding it with several regular expressions to
defend against accidental code execution hazards.  On current browsers, this
file does nothing, preferring the built-in JSON object.

@code{json_parse.js}: This file contains an alternative JSON @code{parse}
function that uses recursive descent instead of @code{eval}.

@code{json_parse_state.js}: This files contains an alternative JSON
@code{parse} function that uses a state machine instead of @code{eval}.

@code{cycle.js}: This file contains two functions, @code{JSON.decycle} and
@code{JSON.retrocycle}, which make it possible to encode cyclical structures
and DAGs in JSON, and to then recover them.  This is a capability that is not
provided by ES5.  @code{JSONPath} is used to represent the links.")
      (license license:public-domain))))

(define-public js-strftime
  (package
    (name "js-strftime")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url"https://github.com/samsonjs/strftime")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "131nmlivazwxyba25kh9lda99749fq4xsyin6lzfalaaydviby4p"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("strftime.js")))
    (home-page "https://github.com/samsonjs/strftime")
    (synopsis "Implementation of strftime to JavaScript")
    (description "This is an implementation of the @code{strftime} procedure
for JavaScript.  It works in (at least) node.js and browsers.  It supports
localization and timezones.  Most standard specifiers from C are supported as
well as some other extensions from Ruby.")
    (license license:expat)))

(define-public js-highlight
  (package
    (name "js-highlight")
    (version "9.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/isagalaev/highlight.js")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12qz22qjpd6svj58pwgcwg2x2rzhihfdrxg6lgj39nfpaln6dris"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("src/highlight.js")))
    (home-page "https://github.com/isagalaev/highlight.js")
    (synopsis "Syntax highlighting for JavaScript")
    (description "Highlight.js is a syntax highlighter written in JavaScript.
It works in the browser as well as on the server.  It works with pretty much
any markup, doesn’t depend on any framework and has automatic language
detection.")
    (license license:bsd-3)))

(define-public js-datatables
  (package
    (name "js-datatables")
    (version "1.10.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://datatables.net/releases/DataTables-"
                                  version ".zip"))
              (sha256
               (base32
                "0cff8a1g7pjwbjdqq0yzqd963ar7pfi4splmm6rwdzganr77rkhb"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("media/js/dataTables.bootstrap.js"
                            "media/js/jquery.dataTables.js")))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://datatables.net")
    (synopsis "DataTables plug-in for jQuery")
    (description "DataTables is a table enhancing plug-in for the jQuery
Javascript library, adding sorting, paging and filtering abilities to plain
HTML tables with minimal effort.")
    (license license:expat)))

(define-public js-requirejs
  (package
    (name "js-requirejs")
    (version "2.3.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/requirejs/requirejs")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cvd5y2mb3h6yil3niqn3gjqrzixdsxcz4rvc2f0hg4kzp5y0w86"))))
    (build-system minify-build-system)
    (arguments `(#:javascript-files '("require.js")))
    (home-page "https://github.com/requirejs/requirejs/")
    (synopsis "File and module loader for JavaScript")
    (description "RequireJS loads plain JavaScript files as well as more
defined modules.  It is optimized for in-browser use, including in a Web
Worker, but it can be used in other JavaScript environments.")
    (license license:expat)))

(define-public js-selectize
  (package
    (name "js-selectize")
    (version "0.12.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/selectize/selectize.js")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15gichl8wi6yxag2ps723nxrgyan15976dzsnvw9h9py8sbyyzjn"))))
    (build-system minify-build-system)
    (arguments `(#:javascript-files '("src/selectize.js")))
    (home-page "https://selectize.github.io/selectize.js/")
    (synopsis "Hybrid widget between a textbox and <select> box")
    (description "Selectize is the hybrid of a textbox and @code{<select>}
box.  It's jQuery based and it has autocomplete and native-feeling keyboard
navigation; it is useful for tagging, contact lists, etc.")
    (license license:asl2.0)))

(define-public js-es5-shim
  (package
    (name "js-es5-shim")
    (version "4.5.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/es-shims/es5-shim")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "142w384fbyllq4yggv173g82lw3wix4jqcg6hkhx1ymq89vvnpmh"))))
    (build-system minify-build-system)
    (arguments `(#:javascript-files
                 '("es5-sham.js"
                   "es5-shim.js")))
    (home-page "https://github.com/es-shims/es5-shim")
    (synopsis "ECMAScript 5 compatibility shims for legacy JavaScript engines")
    (description "@code{es5-shim.js} patches a JavaScript context to contain
all ECMAScript 5 methods that can be faithfully emulated with a legacy
JavaScript engine.  @code{es5-sham.js} patches other ES5 methods as closely as
possible.  Many of these shams are intended only to allow code to be written
to ES5 without causing run-time errors in older engines.  In many cases, this
means that these shams cause many ES5 methods to silently fail.")
    (license license:expat)))

(define-public js-filesaver
  (package
    (name "js-filesaver")
    (version "1.3.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eligrey/FileSaver.js")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gvqk0hnr8fig0n4da7vj7q6z31bcyv52916xz3rbmdj3pgpiv1d"))))
    (build-system minify-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-uglification
           ;; Remove "export" keyword which prevents the file from being
           ;; uglified by uglify-js.  Moreover, that keyword is not present in
           ;; the minified version of the library some projects are using,
           ;; e.g.,
           ;; <https://github.com/jmoenig/Snap--Build-Your-Own-Blocks/blob/master/FileSaver.min.js>
           (lambda _
             (substitute* "src/FileSaver.js"
               (("export ") ""))
             #t))
         (add-after 'install 'install-unminified-version
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "src/FileSaver.js"
                           (string-append (assoc-ref outputs "out")
                                          "/share/javascript"))
             #t)))))
    (home-page
     "https://eligrey.com/blog/saving-generated-files-on-the-client-side/")
    (synopsis "HTML5 saveAs() FileSaver implementation")
    (description "@file{FileSaver.js} implements the @code{saveAs()}
FileSaver interface in browsers that do not natively support it.

@file{FileSaver.js} is the solution to saving files on the
client-side, and is perfect for webapps that need to generate files,
or for saving sensitive information that shouldn't be sent to an
external server.")
    (license license:expat)))

(define-public mujs
  (package
    (name "mujs")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ccxvii/mujs")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00j748hzf4azgjgykc8x9xalh70hj1dm8qvqzvldbf08zq0s3n5j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)  ; no configure script
         (replace 'install
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "install-shared" make-flags))))
       #:make-flags
       (list ,(string-append "VERSION=" version)
             ,(string-append "CC=" (cc-for-target))
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:tests? #f))                    ; no tests
    (inputs
     `(("readline" ,readline)))
    (home-page "https://mujs.com/")
    (synopsis "JavaScript interpreter written in C")
    (description "MuJS is a lightweight Javascript interpreter designed for
embedding in other software to extend them with scripting capabilities.  MuJS
was designed with a focus on small size, correctness, and simplicity.  It is
written in portable C and implements ECMAScript as specified by ECMA-262.  The
interface for binding with native code is designed to be as simple as possible
to use, and is very similar to Lua.  There is no need to interact with byzantine
C++ template mechanisms, or worry about marking and unmarking garbage collection
roots, or wrestle with obscure build systems.")
    (license license:isc)))

(define-public quickjs
  (package
    (name "quickjs")
    (version "2021-03-27")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bellard.org/quickjs/quickjs-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "06pywwpmfwjz225h59wf90q96a2fd66qfcw5xa6m6y9k9k7glnx4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "prefix="
             (string-append "DESTDIR=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           (lambda _
             (invoke "make" "microbench"))))))
    (home-page "https://bellard.org/quickjs/")
    (synopsis "Small embeddable Javascript engine")
    (description "QuickJS supports the ES2020 specification including modules,
asynchronous generators, proxies, BigInt and BigDecimal.  It can compile
Javascript sources to executables with no external dependency.  It includes a
command line interpreter with contextual colorization implemented in
Javascript and a small built-in standard library with C library wrappers.")
    (license license:expat)))

(define-public duktape
  (package
    (name "duktape")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://duktape.org/duktape-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "19szwxzvl2g65fw95ggvb8h0ma5bd9vvnnccn59hwnc4dida1x4n"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; No tests.
       #:make-flags (list "-f" "Makefile.sharedlibrary"
                          (string-append "INSTALL_PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://duktape.org/")
    (synopsis "Small embeddable Javascript engine")
    (description "Duktape is an embeddable Javascript engine, with a focus on
portability and compact footprint.  Duktape is easy to integrate into a C/C++
project: add @file{duktape.c}, @file{duktape.h}, and @file{duk_config.h} to
your build, and use the Duktape API to call ECMAScript functions from C code
and vice versa.")
    (license license:expat)))
