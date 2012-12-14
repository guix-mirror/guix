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

(define-module (distro packages readline)
  #:use-module (guix licenses)
  #:use-module (distro)
  #:use-module (distro packages ncurses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public readline
  (package
   (name "readline")
   (version "6.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/readline/readline-"
                                version ".tar.gz"))
            (sha256
             (base32
              "10ckm2bd2rkxhvdmj7nmbsylmihw0abwcsnxf8y27305183rd9kr"))))
   (build-system gnu-build-system)
   (propagated-inputs `(("ncurses" ,ncurses)))
   (inputs `(("patch/link-ncurses"
              ,(search-patch "readline-link-ncurses.patch"))))
   (arguments `(#:patches (list (assoc-ref %build-inputs
                                           "patch/link-ncurses"))
                #:patch-flags '("-p0")
                #:configure-flags
                (list (string-append "LDFLAGS=-Wl,-rpath -Wl,"
                                     (assoc-ref %build-inputs "ncurses")
                                     "/lib"))

                #:phases (alist-cons-after
                          'install 'post-install
                          (lambda* (#:key outputs #:allow-other-keys)
                            (let* ((out (assoc-ref outputs "out"))
                                   (lib (string-append out "/lib")))
                              ;; Make libraries writable so that `strip' can
                              ;; work.  Failing to do that, it bails out with
                              ;; "Permission denied".
                              (for-each (lambda (f) (chmod f #o755))
                                        (find-files lib "\\.so"))
                              (for-each (lambda (f) (chmod f #o644))
                                        (find-files lib "\\.a"))))
                          %standard-phases)))
   (synopsis "GNU Readline, a library for interactive line editing")
   (description
    "The GNU Readline library provides a set of functions for use by
applications that allow users to edit command lines as they are typed in.
Both Emacs and vi editing modes are available.  The Readline library includes
additional functions to maintain a list of previously-entered command lines,
to recall and perhaps reedit those lines, and perform csh-like history
expansion on previous commands.

The history facilites are also placed into a separate library, the History
library, as part of the build process.  The History library may be used
without Readline in applications which desire its capabilities.")
   (license gpl3+)
   (home-page "http://savannah.gnu.org/projects/readline/")))
