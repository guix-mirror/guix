;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages readline)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system minify))

(define-public font-mathjax
  (package
    (name "font-mathjax")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mathjax/MathJax/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1r72di4pg4i6pfhcskkxqmf1158m81ki6a7lbw6nz4zh7xw23hy4"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (set-path-environment-variable
          "PATH" '("bin") (map (match-lambda
                                 ((_ . input)
                                  input))
                               %build-inputs))
         (let ((install-directory (string-append %output "/share/fonts/mathjax")))
           (mkdir-p install-directory)
           (invoke "tar" "-C" install-directory "-xvf"
                   (assoc-ref %build-inputs "source")
                   ,(string-append "MathJax-" version "/fonts")
                   "--strip" "2")))))
    (native-inputs
     `(("gzip" ,gzip)
       ("tar" ,tar)))
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
           (invoke "tar" "xvf" (assoc-ref %build-inputs "source")
                   ,(string-append "MathJax-" (package-version font-mathjax)
                                   "/unpacked")
                   "--strip" "2")
           (mkdir-p install-directory)
           (symlink (string-append (assoc-ref %build-inputs "font-mathjax")
                                   "/share/fonts/mathjax")
                    (string-append install-directory "/fonts"))

           (for-each
            (lambda (file)
              (let ((installed (string-append install-directory
                                              ;; remove prefix "."
                                              (string-drop file 1))))
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

(define-public js-respond
  (package
    (name "js-respond")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/scottjehl/Respond/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ds1ya2a185jp93mdn07159c2x8zczwi960ykrawpp62bwk2n93d"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match)
                      (ice-9 popen)
                      (srfi srfi-26))
         (set-path-environment-variable
          "PATH" '("bin") (map (match-lambda
                                 ((_ . input)
                                  input))
                               %build-inputs))
         (let ((install-directory (string-append %output
                                                 "/share/javascript/respond/")))
           (invoke "tar" "xvf"
                   (assoc-ref %build-inputs "source")
                   "--strip" "1")
           (mkdir-p install-directory)
           (let* ((file "src/respond.js")
                  (installed (string-append install-directory "respond.min.js")))
             (let ((minified (open-pipe* OPEN_READ "uglify-js" file)))
               (call-with-output-file installed
                 (cut dump-port minified <>)))))
         #t)))
    (home-page "https://github.com/scottjehl/Respond")
    (native-inputs
     `(("uglify-js" ,uglify-js)
       ("source" ,source)
       ("gzip" ,gzip)
       ("tar" ,tar)))
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
              (method url-fetch)
              (uri (string-append "https://github.com/aFarkas/html5shiv/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0inlbpxpqzdyi24lqagzf7l24zxg0y02xcpqs2h4npjscazzw7hg"))))
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
                      (url "https://github.com/douglascrockford/JSON-js.git")
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
              (method url-fetch)
              (uri (string-append "https://github.com/samsonjs/strftime/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1iya43w7y26y2dp9l4d40bhjc4scb5a9mng5ng5c8hsqr82f1375"))))
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
              (method url-fetch)
              (uri (string-append "https://github.com/isagalaev/highlight.js/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jjn9mj7fwq4zpr6is438bscf03b3q8jkj0k5c3fc6pkmjnhw939"))))
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
    (version "1.10.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://datatables.net/releases/DataTables-"
                                  version ".zip"))
              (sha256
               (base32
                "1y9xqyqyz7x1ls3ska71pshl2hpiy3qnw1f7wygyslbhy4ssgf57"))))
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

(define-public js-selectize
  (package
    (name "js-selectize")
    (version "0.12.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/selectize/selectize.js/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0756p49aaz34mw2dx8k1gxf210mngfrri25vkba0j7wihd2af8gn"))))
    (build-system minify-build-system)
    (arguments `(#:javascript-files '("src/selectize.js")))
    (home-page "http://selectize.github.io/selectize.js/")
    (synopsis "Hybrid widget between a textbox and <select> box")
    (description "Selectize is the hybrid of a textbox and @code{<select>}
box.  It's jQuery based and it has autocomplete and native-feeling keyboard
navigation; it is useful for tagging, contact lists, etc.")
    (license license:asl2.0)))

(define-public js-es5-shim
  (package
    (name "js-es5-shim")
    (version "4.5.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/es-shims/es5-shim/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0yfndyijz0ykddzprpvfjb2453gzpn528klmwycwbqc1bqd3m1hl"))))
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
              (method url-fetch)
              (uri (string-append "https://github.com/eligrey/FileSaver.js/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1rkhfqs5plaj628kzj7qgm5qahy4v7ihygifidqr6g6265mil97h"))))
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
    (version "1.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.ghostscript.com/mujs.git")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0pkv26jxwgv5ax0ylfmi4h96h79hj4gvr95218ns8wngnmgr1ny6"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)  ; no configure
         (add-after 'install 'install-shared-library
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "build/release/libmujs.so"
                             (string-append out "/lib"))))))
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out"))
                          (string-append "CC=gcc"))
       #:tests? #f))                    ; no tests
    (inputs
     `(("readline" ,readline)))
    (home-page "https://artifex.com/mujs/")
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
