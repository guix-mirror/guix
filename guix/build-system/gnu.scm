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

(define-module (guix build-system gnu)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:use-module (srfi srfi-1)
  #:export (gnu-build
            gnu-build-system))

;; Commentary:
;;
;; Standard build procedure for packages using the GNU Build System or
;; something compatible ("./configure && make && make install").
;;
;; Code:

(define %standard-inputs
  (compile-time-value
   (map (lambda (name)
          (list name (nixpkgs-derivation name)))
        '("gnutar" "gzip" "bzip2" "xz" "diffutils" "patch"
          "coreutils" "gnused" "gnugrep" "bash"
          "findutils"                             ; used by `libtool'
          "gawk"                                  ; used by `config.status'
          "gcc" "binutils" "gnumake" "glibc"))))

(define* (gnu-build store name source inputs
                    #:key (outputs '("out")) (configure-flags ''())
                    (make-flags ''())
                    (patches ''()) (patch-flags ''("--batch" "-p1"))
                    (out-of-source? #f)
                    (tests? #t)
                    (parallel-build? #t) (parallel-tests? #t)
                    (patch-shebangs? #t)
                    (phases '%standard-phases)
                    (system (%current-system))
                    (modules '((guix build gnu-build-system)
                               (guix build utils))))
  "Return a derivation called NAME that builds from tarball SOURCE, with
input derivation INPUTS, using the usual procedure of the GNU Build System."
  (define builder
    `(begin
       (use-modules ,@modules)
       (gnu-build #:source ,(if (derivation-path? source)
                                (derivation-path->output-path source)
                                source)
                  #:outputs %outputs
                  #:inputs %build-inputs
                  #:patches ,patches
                  #:patch-flags ,patch-flags
                  #:phases ,phases
                  #:configure-flags ,configure-flags
                  #:make-flags ,make-flags
                  #:out-of-source? ,out-of-source?
                  #:tests? ,tests?
                  #:parallel-build? ,parallel-build?
                  #:parallel-tests? ,parallel-tests?
                  #:patch-shebangs? ,patch-shebangs?)))

  (build-expression->derivation store name system
                                builder
                                `(("source" ,source)
                                  ,@inputs
                                  ,@%standard-inputs)
                                #:outputs outputs
                                #:modules modules))

(define gnu-build-system
  (build-system (name 'gnu)
                (description
                 "The GNU Build System—i.e., ./configure && make && make install")
                (build gnu-build)))             ; TODO: add `gnu-cross-build'
