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


(define-module (test-builders)
  #:use-module (guix http)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;; Test the higher-level builders.

(define %store
  (false-if-exception (open-connection)))

(define %bootstrap-inputs
  ;; Derivations taken from Nixpkgs, so that the initial tests don't
  ;; take forever.
  (and (file-exists? (%nixpkgs-directory))
       `(("make" ,(nixpkgs-derivation "gnumake"))
         ("diffutils" ,(nixpkgs-derivation "diffutils"))
         ,@(@@ (distro packages base) %bootstrap-inputs))))

(define %bootstrap-guile
  (@@ (distro packages base) %bootstrap-guile))


(test-begin "builders")

(test-assert "http-fetch"
  (let* ((url      "http://ftp.gnu.org/gnu/hello/hello-2.8.tar.gz")
         (hash     (nix-base32-string->bytevector
                    "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6"))
         (drv-path (http-fetch %store url 'sha256 hash))
         (out-path (derivation-path->output-path drv-path)))
    (and (build-derivations %store (list drv-path))
         (file-exists? out-path)
         (valid-path? %store out-path))))

(test-assert "gnu-build-system"
  (and (build-system? gnu-build-system)
       (eq? gnu-build (build-system-builder gnu-build-system))))

(test-skip (if (file-exists? (%nixpkgs-directory)) 1 0))

(test-assert "gnu-build"
  (let* ((url      "http://ftp.gnu.org/gnu/hello/hello-2.8.tar.gz")
         (hash     (nix-base32-string->bytevector
                    "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6"))
         (tarball  (http-fetch %store url 'sha256 hash))
         (build    (gnu-build %store "hello-2.8" tarball
                              %bootstrap-inputs
                              #:implicit-inputs? #f
                              #:guile %bootstrap-guile))
         (out      (derivation-path->output-path build)))
    (and (build-derivations %store (list (pk 'hello-drv build)))
         (valid-path? %store out)
         (file-exists? (string-append out "/bin/hello")))))

(test-end "builders")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; End:
