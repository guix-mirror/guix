;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 ng0 <ng0@n0.is>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
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

(define-module (gnu packages forth)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages m4))

(define-public gforth
  (package
    (name "gforth")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gforth/gforth-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1c1bahc9ypmca8rv2dijiqbangm1d9av286904yw48ph7ciz4qig"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f             ; XXX: parallel build fails
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-gforth.el
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out           (assoc-ref outputs "out"))
                    (emacs-sitedir (string-append
                                    out "/share/emacs/site-lisp")))
               ;; TODO: compile and autoload it.
               (install-file "gforth.el" emacs-sitedir)
               #t))))))
    (native-inputs
     `(("m4" ,m4)))
    (synopsis "Forth interpreter")
    (description
     "Gforth is a fast and portable implementation of the ANSI Forth language.
It includes an editing mode for Emacs and an interpreter featuring completion
and history.  A generic virtual machine environment, vmgen, is also
included.")
    (home-page "https://www.gnu.org/software/gforth/")
    (license license:gpl3+)))
