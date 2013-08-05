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
  #:use-module (gnu packages)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages libtiff)
  #:use-module (gnu packages libpng)
  #:use-module ((gnu packages compression)
                #:renamer (symbol-prefix-proc 'compression:))
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib))

(define-public emacs
  (package
    (name "emacs")
    (version "24.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/emacs/emacs-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1385qzs3bsa52s5rcncbrkxlydkw0ajzrvfxgv8rws5fx512kakh"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-crt-dir=" (assoc-ref %build-inputs "libc")
                            "/lib")
             "--with-gif=no")                     ; XXX: add libungif
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
       ("xlibs" ,libx11)
       ("gtk+" ,gtk+)
       ("libXft" ,libxft)
       ("libtiff" ,libtiff)
       ;; ("libungif" ,libungif)
       ("libjpeg" ,libjpeg-8)

       ;; When looking for libpng `configure' links with `-lpng -lz', so we
       ;; must also provide zlib as an input.
       ("libpng" ,libpng)
       ("zlib" ,compression:zlib)

       ("libXpm" ,libxpm)
       ("libxml2" ,libxml2)
       ("dbus" ,dbus)

       ("patch/epaths" ,(search-patch "emacs-configure-sh.patch"))
       ))
    (home-page "http://www.gnu.org/software/emacs/")
    (synopsis "The extensible, customizable, self-documenting text editor")
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


;;;
;;; Emacs hacking.
;;;

(define-public geiser
  (package
    (name "geiser")
    (version "0.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/geiser/" version
                                 "/geiser-" version ".tar.gz"))
             (sha256
              (base32 "0ds7zk9b1839l9fsqfsgrby6manvy1cf5bjniiqhxl55h0cr6ijp"))))
    (build-system gnu-build-system)
    (inputs `(("guile" ,guile-2.0)
              ("emacs" ,emacs)))
    (home-page "http://nongnu.org/geiser/")
    (synopsis "Collection of Emacs modes for Guile and Racket hacking")
    (description
     "Geiser is a collection of Emacs major and minor modes that
conspire with one or more Scheme interpreters to keep the Lisp Machine
Spirit alive.  It draws inspiration (and a bit more) from environments
such as Common Lisp’s Slime, Factor’s FUEL, Squeak or Emacs itself, and
does its best to make Scheme hacking inside Emacs (even more) fun.

Or, to be precise, what i consider fun.  Geiser is thus my humble
contribution to the dynamic school of expression, and a reaction against
what i perceive as a derailment, in modern times, of standard Scheme
towards the static camp.  Because i prefer growing and healing to poking
at corpses, the continuously running Scheme interpreter takes the center
of the stage in Geiser.  A bundle of Elisp shims orchestrates the dialog
between the Scheme interpreter, Emacs and, ultimately, the schemer,
giving her access to live metadata.")
    (license bsd-3)))
