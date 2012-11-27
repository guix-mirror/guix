;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (distro packages nano)
  #:use-module ((distro packages gettext)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (distro packages ncurses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public nano
  (package
    (name "nano")
    (version "2.2.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/nano/nano-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0yp6pid67k8h7394spzw0067fl2r7rxm2b6kfccg87g8nlry2s5y"))))
    (build-system gnu-build-system)
    (inputs
     `(("gettext" ,guix:gettext)
       ("ncurses" ,ncurses)))
    (home-page "http://www.nano-editor.org/")
    (synopsis
     "A small, user-friendly console text editor")
    (description
     "GNU nano is designed to be a free replacement for the Pico text
editor, part of the Pine email suite from The University of
Washington. It aims to emulate Pico as closely as possible and perhaps
include extra functionality.")
    (license "GPLv3+"))) ; some files are under GPLv2+