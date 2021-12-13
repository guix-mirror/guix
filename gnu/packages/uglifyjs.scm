;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Charles <charles.b.jackson@protonmail.com>
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

(define-module (gnu packages uglifyjs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system node)
  #:use-module (gnu packages node-xyz))

(define-public node-uglify-js
  (package
    (name "node-uglify-js")
    (version "3.13.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mishoo/UglifyJS")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q1f91xwwm829cl5v727d4qkxy4yh7wri3pgl89f0m3a4y1n7hi4"))))
    (build-system node-build-system)
    (native-inputs
     (list node-acorn node-semver))
    (home-page "https://lisperator.net/uglifyjs/")
    (synopsis "JavaScript parser / mangler / compressor / beautifier toolkit")
    (description "UglifyJS is a JavaScript compressor/minifier written in
JavaScript.  It also contains tools that allow one to automate working with
JavaScript code: parser, code generator, compressor, mangler, scope analyzer,
tree walker, and tree transformer.")
      (license license:bsd-2)))

(define-public uglifyjs node-uglify-js)
