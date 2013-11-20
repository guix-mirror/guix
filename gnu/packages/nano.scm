;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages nano)
  #:use-module (guix licenses)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ncurses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public nano
  (package
    (name "nano")
    (version "2.3.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/nano/nano-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1s3b21h5p7r8xafw0gahswj16ai6k2vnjhmd15b491hl0x494c7z"))))
    (build-system gnu-build-system)
    (inputs
     `(("gettext" ,gnu-gettext)
       ("ncurses" ,ncurses)))
    (home-page "http://www.nano-editor.org/")
    (synopsis "Small, user-friendly console text editor")
    (description
     "nano is a small and simple text editor.  In addition to basic
editing, it supports interactive search and replace, go to line and column
number, auto-indentation and more.")
    (license gpl3+))) ; some files are under GPLv2+
