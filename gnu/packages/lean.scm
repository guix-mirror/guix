;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Amin Bandali <bandali@gnu.org>
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

(define-module (gnu packages lean)
  #:use-module (gnu packages multiprecision)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download))

(define-public lean
  (package
    (name "lean")
    (version "3.5.0")
    (home-page "https://github.com/leanprover-community/lean")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fdblq8ckrv6wqxfl4ybcs3ybfq7y096c9f5j4j75ymb14r401lr"))))
    (build-system cmake-build-system)
    (inputs
     `(("gmp" ,gmp)))
    (arguments
     `(#:build-type "Release"           ; default upstream build type
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-tests-shebangs
           (lambda _
             (let ((sh (which "sh"))
                   (bash (which "bash")))
               (substitute* (find-files "tests/lean" "\\.sh$")
                 (("#![[:blank:]]?/bin/sh")
                  (string-append "#!" sh))
                 (("#![[:blank:]]?/bin/bash")
                  (string-append "#!" bash))
                 (("#![[:blank:]]?usr/bin/env bash")
                  (string-append "#!" bash)))
               #t)))
         (add-before 'configure 'chdir-to-src
           (lambda _ (chdir "src") #t)))))
    (synopsis "The Lean theorem prover and programming language")
    (description
     "Lean is a theorem prover and programming language with a small trusted
core based on dependent typed theory, aiming to bridge the gap between
interactive and automated theorem proving.")
    (license license:asl2.0)))
