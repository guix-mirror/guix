;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module (guix build-system trivial))

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
