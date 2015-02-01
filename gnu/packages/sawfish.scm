;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
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

(define-module (gnu packages sawfish)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gdbm)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo))

(define-public librep
  (package
    (name "librep")
    (version "0.92.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.tuxfamily.org/" name "/"
                                  name "_" version ".tar.xz"))
              (sha256
               (base32
                "0297m24p2y8j3wavf8qqyriic7ls2392cmfn96y0pi83r5qckc25"))
              (patches (list (search-patch "librep-rules.mk.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("makeinfo"   ,texinfo)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gdbm"     ,gdbm)
       ("gmp"      ,gmp)
       ("libffi"   ,libffi)
       ("readline" ,readline)))
    (native-search-paths
     (list (search-path-specification
            (variable "REP_DL_LOAD_PATH")
            (files '("lib/rep")))))
    (home-page "http://sawfish.wikia.com/wiki/Librep")
    (synopsis "Lisp system for sawfish")
    (description
     "Librep is a dialect of Lisp, designed to be used both as an extension
language for applications and as a general purpose programming language.  It
was originally written to be mostly-compatible with Emacs Lisp, but has
subsequently diverged markedly.  Its aim is to combine the best features of
Scheme and Common Lisp and provide an environment that is comfortable for
implementing both small and large scale systems.")
    (license gpl2+)))
