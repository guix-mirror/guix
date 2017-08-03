;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages lisp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system minify))

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
           (system* "tar" "xvf" (assoc-ref %build-inputs "source")
                    "MathJax-2.7.1/unpacked" "--strip" "2")
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
                        (dump-port minified port)))))
                 (else
                  (install-file file (dirname installed))))))
            (find-files "."))))))
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
           (system* "tar" "xvf"
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
