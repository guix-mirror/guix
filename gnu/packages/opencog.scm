;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages opencog)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils))

(define-public cogutil
  ;; The last release was in 2016.  Other OpenCog packages require a later
  ;; version.
  (let ((commit "b07b41b2eaf01627c78b27f1f28bb09ef7086f8e")
        (revision "1"))
    (package
      (name "cogutil")
      (version (git-version "2.0.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/opencog/cogutil.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ymmcrinp0prlxsmxmwdjjl4kgaj7wzq39d5b1q2apgg94yfdhqb"))))
      (build-system cmake-build-system)
      (arguments
       `(#:test-target "tests"))
      (inputs
       `(("boost" ,boost)))
      (native-inputs
       `(("cxxtest" ,cxxtest)
         ("python" ,python-minimal)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/opencog/cogutil/")
      (synopsis "Low-level C++ programming utilities used by OpenCog components")
      (description "The OpenCog utilities is a miscellaneous collection of C++
utilities use for typical programming tasks in multiple OpenCog projects.")
      ;; Either of these licenses.
      (license (list license:agpl3 license:asl2.0)))))

(define-public atomspace
  ;; The last release was in 2016 and doesn't build with our Boost package.
  (let ((commit "86c848dfc7135b3c47deb581f8da54a60f6711c9")
        (revision "1"))
    (package
      (name "atomspace")
      (version (git-version "5.0.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/opencog/atomspace.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0vxzhszb0z8081li38hid07a5axzxyflsmq1mcn4b1k4z1j8ggch"))))
      (build-system cmake-build-system)
      (arguments
       `(#:test-target "tests"
         #:configure-flags
         (list (string-append "-DGUILE_INCLUDE_DIR="
                              (assoc-ref %build-inputs "guile")
                              "/include/guile/2.2/")
               (string-append "-DGUILE_SITE_DIR="
                              (assoc-ref %outputs "out")
                              "/share/guile/site/2.2/"))))
      (inputs
       `(("boost" ,boost)
         ("cogutil" ,cogutil)
         ("gmp" ,gmp)
         ("guile" ,guile-2.2)
         ("postgresql" ,postgresql)))
      (native-inputs
       `(("cxxtest" ,cxxtest)
         ("python" ,python-minimal)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/opencog/atomspace/")
      (synopsis "OpenCog hypergraph database, query system and rule engine")
      (description "The OpenCog AtomSpace is an in-RAM @dfn{knowledge
representation} (KR) database, an associated query engine and graph-re-writing
system, and a rule-driven inferencing engine that can apply and manipulate
sequences of rules to perform reasoning.  It is a layer that sits on top of
ordinary distributed (graph) databases, providing a large variety of advanced
features not otherwise available.")
      (license license:agpl3))))
