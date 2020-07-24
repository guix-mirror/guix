;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
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

(define-module (gnu packages sml)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public polyml
  (package
    (name "polyml")
    (version "5.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/polyml/polyml")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ag7n55ym1zxksi89dvs17j6iaa58v4mg47s92zpa1b49k4fql1k"))))
    (build-system gnu-build-system)
    (inputs
     `(("gmp" ,gmp)
       ("lesstif" ,lesstif)
       ("libffi" ,libffi)
       ("libx11" ,libx11)
       ("libxt" ,libxt)))
    (arguments
     '(#:configure-flags
       (list "--with-system-libffi=yes"
             "--with-x=yes"
             "--with-threads=yes"
             "--with-gmp=yes")
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-compiler
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (define flags
               (if parallel-build?
                   (cons (format #f "-j~d" (parallel-job-count))
                         make-flags)
                   make-flags))
             (apply system* "make" (append flags (list "compiler"))))))))
    (home-page "https://www.polyml.org/")
    (synopsis "Standard ML implementation")
    (description "Poly/ML is a Standard ML implementation.  It is fully
compatible with the ML97 standard.  It includes a thread library, a foreign
function interface, and a symbolic debugger.")
    ;; Some source files specify 'or any later version'; some don't
    (license
     (list license:lgpl2.1
           license:lgpl2.1+))))
