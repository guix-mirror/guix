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

(define-module (guix build-system trivial)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:export (trivial-build-system))

(define* (trivial-build store name source inputs
                        #:key outputs system builder (modules '()))
  "Run build expression BUILDER, an expression, for SYSTEM.  SOURCE is
ignored."
  (build-expression->derivation store name system builder inputs
                                #:outputs outputs
                                #:modules modules))

(define trivial-build-system
  (build-system (name 'trivial)
                (description
                 "Trivial build system, to run arbitrary Scheme build expressions")
                (build trivial-build)
                (cross-build trivial-build)))
