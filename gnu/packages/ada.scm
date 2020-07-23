;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu packages ada)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public python2-langkit
  (let ((commit "fe0bc8bf60dbd2937759810df76ac420d99fc15f")
        (revision "0"))
    (package
      (name "python2-langkit")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/AdaCore/langkit")
                      (commit commit)))
                (sha256
                 (base32
                  "1abqgw2p8pb1pm54my5kkbbixfhc6l0bwajdv1xlzyrh31xki3wx"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python2-docutils" ,python2-docutils)
         ("python2-enum34" ,python2-enum34)
         ("python2-funcy" ,python2-funcy)
         ("python2-mako" ,python2-mako)))
      (arguments
       `(#:python ,python-2
         #:tests? #f))           ; Tests would requite gprbuild (Ada).
      (synopsis "Semantic analysis tool generator in Python")
      (description "@code{Langkit} is a tool whose purpose is to make it easy
to create syntactic and semantic analysis engines.  Write a language
specification in our Python DSL and Langkit will generate for you an
Ada library with bindings for the C and Python programming languages.")
      (home-page "https://github.com/AdaCore/langkit/")
      (license license:gpl3+))))   ; and gcc runtime library exception

(define-public python2-libadalang
  (let ((commit "9b205e9bacdd50a68117727332e16fbef5f6ac49")
        (revision "0"))
    (package
      (name "python2-libadalang")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/AdaCore/libadalang")
                      (commit commit)))
                (sha256
                 (base32
                  "06hsnzj2syqpq2yhg1bb0zil7ydbyqkdmkjbf8j9b5sdgkyh5xrp"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system python-build-system)
      (native-inputs
       `(("python2-langkit" ,python2-langkit)
         ("python2-quex" ,python2-quex-0.67.3)))
      (arguments
       `(#:python ,python-2
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda _
               (invoke "python2" "ada/manage.py" "generate")
               (invoke "python2" "ada/manage.py" "build")))
           (replace 'check
             (lambda _
               (invoke "python2" "ada/manage.py" "test")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (invoke "python2" "ada/manage.py" "install" out)))))))
      (synopsis "Semantic Analysis for Ada in Python")
      (description "@code{libadalang} provides a high-performance semantic
engine for the Ada programming language.")
      (home-page "https://github.com/AdaCore/libadalang")
      (license license:gpl3)))) ; and gcc runtime gcc lib exception
