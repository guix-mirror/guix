;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages bison)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages flex)
  #:use-module (srfi srfi-1)
  #:export (bison))

(define bison
  (package
    (name "bison")
    (version "3.0.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/bison/bison-"
                          version ".tar.xz"))
      (sha256
       (base32
        "0g4gjan477lac18m51kv4xzcsp6wjfsfwvd2dxymcl6vid9fihx2"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))
    (inputs `(("flex" ,flex)))
    (propagated-inputs `(("m4" ,m4)))
    (home-page "http://www.gnu.org/software/bison/")
    (synopsis "Parser generator")
    (description
     "GNU Bison is a general-purpose parser generator.  It can build a
deterministic or generalized LR parser from an annotated, context-free
grammar.  It is versatile enough to have many applications, from parsers for
simple tools through complex programming languages.")
    (license gpl3+)))

(define-public bison-2.7
  (package (inherit bison)
    (version "2.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/bison/bison-"
                          version ".tar.xz"))
      (sha256
       (base32
        "1zd77ilmpv5mi3kr55jrj6ncqlcnyhpianhrwzak2q28cv2cbn23"))))))

