;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro base)
  #:use-module (guix packages)
  #:use-module (guix http)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils))

;;; Commentary:
;;;
;;; A Guix-based distribution.
;;;
;;; Code:

(define-public hello
  (package
   (name "hello")
   (version "2.8")
   (source (source
            (method http-fetch)
            (uri "http://ftp.gnu.org/gnu/hello/hello-2.8.tar.gz")
            (sha256
             (nix-base32-string->bytevector  ; TODO: make conversion implicit
              "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6"))))
   (build-system gnu-build-system)
   (arguments '(#:configure-flags
                `("--disable-dependency-tracking"
                  ,(string-append "--with-gawk="  ; for illustration purposes
                                 (assoc-ref %build-inputs "gawk")))))
   (inputs `(("gawk" ,(nixpkgs-derivation "gawk"))))
   (description "GNU Hello")
   (long-description "Yeah...")
   (license "GPLv3+")))
