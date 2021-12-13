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
  #:use-module (gnu packages language)
  #:use-module (gnu packages linux)
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
                      (url "https://github.com/opencog/cogutil")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ymmcrinp0prlxsmxmwdjjl4kgaj7wzq39d5b1q2apgg94yfdhqb"))))
      (build-system cmake-build-system)
      (arguments
       `(#:test-target "tests"))
      (inputs
       (list boost))
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
                      (url "https://github.com/opencog/atomspace")
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
       (list boost cogutil gmp guile-2.2 postgresql))
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

(define-public cogserver
  ;; There are no releases.
  (let ((commit "ec5f3b9590db0f6a085b5d0320f5d3710e0f1635")
        (revision "2"))
    (package
      (name "cogserver")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/opencog/cogserver")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1h0vcxb6n5dc654xqinqcxc7dxwcs6bsywgir8rhrqiykk760mzl"))))
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
       (list atomspace boost cogutil gmp guile-2.2))
      (native-inputs
       `(("cxxtest" ,cxxtest)
         ("python" ,python-minimal)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/opencog/cogserver/")
      (synopsis "OpenCog network server")
      (description "The OpenCog Cogserver is a network and job server for the
OpenCog framework.")
      (license license:agpl3))))

(define-public attention
  ;; There are no releases.
  (let ((commit "87d43679280ce486cd6757765d2e1df6d502991d")
        (revision "1"))
    (package
      (name "attention")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/opencog/attention")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0sndslphicv6w9qpag168rqkxq5sf71l5qbfx6zhsd5bzlf5fhwv"))))
      (build-system cmake-build-system)
      (arguments
       `(#:test-target "tests"
         #:configure-flags
         (list
          (string-append "-DGUILE_INCLUDE_DIR="
                         (assoc-ref %build-inputs "guile")
                         "/include/guile/2.2/")
          (string-append "-DGUILE_SITE_DIR="
                         (assoc-ref %outputs "out")
                         "/share/guile/site/2.2/"))))
      (inputs
       (list atomspace
             boost
             cogserver
             cogutil
             gmp
             guile-2.2))
      (native-inputs
       `(("cxxtest" ,cxxtest)
         ("python" ,python-minimal)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/opencog/attention/")
      (synopsis "OpenCog attention allocation subsystem")
      (description "Attention Allocation is an OpenCog subsystem meant to
control the application of processing and memory resources to specific
tasks.")
      (license license:agpl3))))

(define-public opencog
  ;; There are no recent releases.
  (let ((commit "ceac90507610cb2d0ee98f97a2086865292b1204")
        (revision "1"))
    (package
      (name "opencog")
      (version (git-version "0.1.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/opencog/opencog")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1j8wv910fvrmph370wv5pv2f4bc2s9vl6i7bw3pkmwbdhxkhjbhm"))))
      (build-system cmake-build-system)
      (arguments
       `(#:test-target "tests"
         #:configure-flags
         (list
          (string-append "-DGUILE_INCLUDE_DIR="
                         (assoc-ref %build-inputs "guile")
                         "/include/guile/2.2/")
          (string-append "-DGUILE_SITE_DIR="
                         (assoc-ref %outputs "out")
                         "/share/guile/site/2.2/"))))
      (inputs
       `(("attention" ,attention)
         ("atomspace" ,atomspace)
         ("boost" ,boost)
         ("cogserver" ,cogserver)
         ("cogutil" ,cogutil)
         ("gmp" ,gmp)
         ("guile" ,guile-2.2)
         ("libuuid" ,util-linux "lib")
         ("link-grammar" ,link-grammar)))
      (native-inputs
       `(("cxxtest" ,cxxtest)
         ("python" ,python-minimal)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/opencog/attention/")
      (synopsis "Framework for integrated artificial intelligence")
      (description "OpenCog is a framework for developing AI systems,
especially appropriate for integrative multi-algorithm systems, and artificial
general intelligence systems.  It currently contains a functional core
framework, and a number of cognitive agents at varying levels of completion,
some already displaying interesting and useful functionalities alone and in
combination.")
      (license license:agpl3))))

(define-public agi-bio
  ;; There are no releases.
  (let ((commit "b5c6f3d99e8cca3798bf0cdf2c32f4bdb8098efb")
        (revision "1"))
    (package
      (name "agi-bio")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/opencog/agi-bio")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0c3q0nhmd03nvqm1ih10y28n596cjvfhkcfvzw2fmz1sn3ywdah4"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ; there are none
         #:configure-flags
         (list
          (string-append "-DGUILE_INCLUDE_DIR="
                         (assoc-ref %build-inputs "guile")
                         "/include/guile/2.2/")
          (string-append "-DGUILE_SITE_DIR="
                         (assoc-ref %outputs "out")
                         "/share/guile/site/2.2/"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-unqualified-load
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "bioscience/bioscience.scm"
                 (("\\(load \"bioscience/types/bioscience_types.scm\"\\)")
                  (format #f "(load \"~a/bioscience/types/bioscience_types.scm\")"
                          (string-append (assoc-ref outputs "out")
                                         "/share/guile/site/2.2/opencog"))))
               #t)))))
      (inputs
       (list atomspace cogutil gmp guile-2.2))
      (native-inputs
       `(("cxxtest" ,cxxtest)
         ("python" ,python-minimal)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/opencog/agi-bio")
      (synopsis "Genomic and proteomic data exploration and pattern mining")
      (description "This is a package for genomic and proteomic research using
the OpenCog toolset with Guile.  This includes experiments in applying pattern
mining and other OpenCog components.")
      (license license:agpl3))))
