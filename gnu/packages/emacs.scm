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

(define-module (gnu packages emacs)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (distro)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages pkg-config))

(define-public emacs
  (package
    (name "emacs")
    (version "24.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/emacs/emacs-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "13wbjfjmz13qpjwssy44nw2230lllmkkgjsy0rqfm6am2cf87n3k"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-crt-dir=" (assoc-ref %build-inputs "libc")
                            "/lib"))
       #:patches (list (assoc-ref %build-inputs "patch/epaths"))
       #:phases (alist-cons-before
                 'configure 'fix-/bin/pwd
                 (lambda _
                   ;; Use `pwd', not `/bin/pwd'.
                   (substitute* (find-files "." "^Makefile\\.in$")
                     (("/bin/pwd")
                      "pwd")))
                 %standard-phases)))
    (inputs
     `(("pkg-config" ,pkg-config)
       ("gnutls" ,gnutls)
       ("texinfo" ,texinfo)
       ("ncurses" ,ncurses)

       ;; TODO: Add the optional dependencies.
       ;; ("xlibs" ,xlibs)
       ;; ("gtk+" ,gtk+)
       ;; ("libXft" ,libXft)
       ;; ("libtiff" ,libtiff)
       ;; ("libungif" ,libungif)
       ;; ("libjpeg" ,libjpeg)
       ;; ("libpng" ,libpng)
       ;; ("libXpm" ,libXpm)
       ;; ("libxml2" ,libxml2)
       ;; ("dbus-library" ,dbus-library)

       ("patch/epaths" ,(search-patch "emacs-configure-sh.patch"))
       ))
    (home-page "http://www.gnu.org/software/emacs/")
    (synopsis
     "GNU Emacs 24, the extensible, customizable text editor")
    (description
     "GNU Emacs is an extensible, customizable text editor—and more.  At its
core is an interpreter for Emacs Lisp, a dialect of the Lisp
programming language with extensions to support text editing.

The features of GNU Emacs include: content-sensitive editing modes,
including syntax coloring, for a wide variety of file types including
plain text, source code, and HTML; complete built-in documentation,
including a tutorial for new users; full Unicode support for nearly all
human languages and their scripts; highly customizable, using Emacs
Lisp code or a graphical interface; a large number of extensions that
add other functionality, including a project planner, mail and news
reader, debugger interface, calendar, and more.  Many of these
extensions are distributed with GNU Emacs; others are available
separately.")
    (license gpl3+)))
