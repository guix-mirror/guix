;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages lisp-check)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf))

;;; Commentary:
;;;
;;; This module only contains Common Lisp libraries related to code testing
;;; facilities.

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

;;; lisp-check.scm ends here
