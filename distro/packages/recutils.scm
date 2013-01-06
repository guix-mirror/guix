;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (distro packages recutils)
  #:use-module (guix licenses)
  #:use-module (distro)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public recutils
  (package
   (name "recutils")
   (version "1.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/recutils/recutils-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1v2xzwwwhc5j5kmvg4sv6baxjpsfqh8ln7ilv4mgb1408rs7xmky"))))
   (build-system gnu-build-system)
   (inputs `(;; TODO: Enable optional deps when they're packaged.
             ;; ("curl" ,(nixpkgs-derivation "curl"))
             ;; ("emacs" ,(nixpkgs-derivation "emacs"))
             ;; ("check" ,(nixpkgs-derivation "check"))
             ;; ("bc" ,(nixpkgs-derivation "bc"))
             ("patch/gets"
              ,(search-patch "diffutils-gets-undeclared.patch"))))
   (arguments `(#:patches (list (assoc-ref %build-inputs "patch/gets"))))
   (synopsis "GNU recutils, tools and libraries to access human-editable,
text-based databases")
   (description
    "GNU recutils is a set of tools and libraries to access human-editable,
text-based databases called recfiles.  The data is stored as a sequence of
records, each record containing an arbitrary number of named fields.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/recutils/")))
