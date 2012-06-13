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

(define-module (guix gnu-build-system)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (srfi srfi-1)
  #:export (gnu-build))

;; Commentary:
;;
;; Standard build procedure for packages using the GNU Build System or
;; something compatible ("./configure && make && make install").
;;
;; Code:

(define %standard-inputs
  (map (lambda (name)
         (cons name (nixpkgs-derivation name)))
       '("gnutar" "gzip" "bzip2" "xz"
         "coreutils" "gnused" "gnugrep" "bash"
         "gcc" "binutils" "gnumake" "glibc")))

(define* (gnu-build store name source inputs
                    #:key (outputs '("out")) (configure-flags '())
                    (system (%current-system)))
  "Return a derivation called NAME that builds from tarball SOURCE, with
input derivation INPUTS, using the usual procedure of the GNU Build System."
  (define builder
    `(begin
       (use-modules (guix build gnu-build-system))
       (gnu-build ,(if (derivation-path? source)
                       (derivation-path->output-path source)
                       source)
                  %outputs
                  %build-inputs
                  #:configure-flags ',configure-flags)))

  (build-expression->derivation store name system
                                builder
                                (alist-cons "source" source
                                            (append inputs %standard-inputs))
                                #:outputs outputs
                                #:modules '((guix build gnu-build-system)
                                            (guix build utils))))
