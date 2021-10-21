;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Charles Jackson <charles.b.jackson@protonmail.com>
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

;;; This module only contains Common Lisp libraries related to code testing
;;; facilities.

(define-module (gnu packages lisp-check)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf))

(define-public sbcl-nst
  (let ((commit "6c0990f594abcf5887e8d80f1035e3b60454b61b")
        (revision "1"))
    (package
     (name "sbcl-nst")
     (version (git-version "4.1.2" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jphmrst/cl-nst")
             (commit commit)))
       (file-name (git-file-name "nst" version))
       (sha256
        (base32 "1hf3r6pqbnd9vsd1i24qmz928kia72hdgmiafiwb6jw1hmj3r6ga"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      `(("closer-mop" ,sbcl-closer-mop)
        ("org-sampler" ,sbcl-org-sampler)))
     (home-page "https://github.com/jphmrst/cl-nst")
     (synopsis "Unit testing for Common Lisp")
     (description
      "NST is a unit/regression testing system for Common Lisp.")
     (license license:llgpl))))

(define-public ecl-nst
  (sbcl-package->ecl-package sbcl-nst))

(define-public cl-nst
  (sbcl-package->cl-source-package sbcl-nst))

(define-public sbcl-should-test
  (let ((commit "48facb9f9c07aeceb71fc0c48ce17fd7d54a09d4")
        (revision "0"))
    (package
      (name "sbcl-should-test")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/vseloved/should-test")
               (commit commit)))
         (file-name (git-file-name "should-test" version))
         (sha256
          (base32 "1fqqa7lhf28qg60ji9libkylkcy747x576qpjn1y7c945j2fxmnm"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-ppcre" ,sbcl-cl-ppcre)
         ("local-time" ,sbcl-local-time)
         ("osicat" ,sbcl-osicat)
         ("rutils" ,sbcl-rutils)))
      (home-page "https://github.com/vseloved/should-test")
      (synopsis "Minimal yet feature-rich Common Lisp test framework")
      (description
       "SHOULD-TEST is a methodology-agnostic and non-opinionated Common Lisp
test framework, i.e. it doesn't care what kind of test approach you'd like to
take.")
      (license license:expat))))

(define-public cl-should-test
  (sbcl-package->cl-source-package sbcl-should-test))

(define-public ecl-should-test
  (sbcl-package->ecl-package sbcl-should-test))
