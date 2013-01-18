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

(define-module (gnu packages ed)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public ed
  (package
    (name "ed")
    (version "1.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://gnu/ed/ed-"
            version
            ".tar.gz"))
      (sha256
       (base32
        "18gvhyhwpabmgv4lh21lg8vl3z7acdyhh2mr2kj9g75wksj39pcp"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("CC=gcc")))
    (home-page "http://www.gnu.org/software/ed/")
    (synopsis
     "GNU ed, an implementation of the standard Unix editor")
    (description
     "GNU ed is a line-oriented text editor.  It is used to create,
display, modify and otherwise manipulate text files, both
interactively and via shell scripts.  A restricted version of ed,
red, can only edit files in the current directory and cannot
execute shell commands.  Ed is the \"standard\" text editor in the
sense that it is the original editor for Unix, and thus widely
available.  For most purposes, however, it is superseded by
full-screen editors such as GNU Emacs or GNU Moe.")
    (license gpl3+)))
