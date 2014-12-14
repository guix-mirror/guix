;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.org>
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

(define-module (gnu packages mg)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config))

(define-public mg
  (package
    (name "mg")
    (version "20050429")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://homepage.boetes.org/software/mg/mg-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "19kib0aha4a40izzds7r63qfb2akq4sily6k28fl0n0zdgq0cna1"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Makefile.in"
                    (("-Werror") "")
                    (("-lcurses") "-lncurses")
                    (("/usr/bin/install") "install -D")
                    (("/usr/bin/strip") "strip"))))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)))
    (arguments
     ;; No test suite available.
     '(#:tests? #f
       #:phases (alist-cons-before
                 'configure 'pre-configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (substitute* "Makefile.in"
                     (("(prefix=[[:blank:]]*)/usr/local" all prefix)
                      (string-append prefix (assoc-ref outputs "out")))))
                 %standard-phases)))
    (home-page "http://homepage.boetes.org/software/mg/")
    (synopsis "Microscopic GNU Emacs clone")
    (description
     "mg is Micro GNU Emacs; this is a portable version of the mg maintained
by the OpenBSD team.")
    (license public-domain)))
