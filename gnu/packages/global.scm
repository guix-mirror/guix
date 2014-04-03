;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages global)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages))

(define-public global                             ; a global variable
  (package
    (name "global")
    (version "6.2.12")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/global/global-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "05jkhya1cs6yqhkf8nw5x56adkxxrqyga7sq7hx44dbf7alczwfa"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("libtool" ,libtool)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-ncurses="
                            (assoc-ref %build-inputs "ncurses")))

       #:phases (alist-cons-after
                 'install 'post-install
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Install the Emacs Lisp file in the right place.
                   (let* ((out  (assoc-ref outputs "out"))
                          (data (string-append out "/share/gtags"))
                          (lisp (string-append out "/share/emacs/site-lisp")))
                     (mkdir-p lisp)
                     (copy-file (string-append data "/gtags.el")
                                (string-append lisp "/gtags.el"))
                     (delete-file (string-append data "/gtags.el"))
                     #t))
                 %standard-phases)))
    (home-page "http://www.gnu.org/software/global/")
    (synopsis "Cross-environment source code tag system")
    (description
     "GLOBAL is a source code tagging system that functions in the same way
across a wide array of environments, such as different text editors, shells
and web browsers.  The resulting tags are useful for quickly moving around in
a large, deeply nested project.")
    (license gpl3+)))
