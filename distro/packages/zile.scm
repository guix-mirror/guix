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

(define-module (distro packages zile)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (distro packages bdw-gc)
  #:use-module (distro packages perl)
  #:use-module (distro packages help2man)
  #:use-module (distro packages ncurses))

(define-public zile
  (package
    (name "zile")
    (version "2.4.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/zile/zile-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0j801c28ypm924rw3lqyb6khxyslg6ycrv16wmmwcam0mk3mj6f7"))))
    (build-system gnu-build-system)
    (inputs
     `(("boehm-gc" ,libgc)
       ("ncurses" ,ncurses)
       ("perl" ,perl)
       ("help2man" ,help2man)))
    (home-page "http://www.gnu.org/software/zile/")
    (synopsis "GNU Zile, a lightweight Emacs clone")
    (description
     "GNU Zile, which is a lightweight Emacs clone.  Zile is short
for Zile Is Lossy Emacs.  Zile has been written to be as
similar as possible to Emacs; every Emacs user should feel at
home.

Zile has all of Emacs's basic editing features: it is 8-bit
clean (though it currently lacks Unicode support), and the
number of editing buffers and windows is only limited by
available memory and screen space respectively.  Registers,
minibuffer completion and auto fill are available.  Function
and variable names are identical with Emacs's (except those
containing the word \"emacs\", which instead contain the word
\"zile\"!).

However, all of this is packed into a program which typically
compiles to about 130Kb.")
    (license "GPLv3+")))
