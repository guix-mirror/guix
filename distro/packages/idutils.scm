;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (distro packages idutils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (distro))

(define-public idutils
  (package
    (name "idutils")
    (version "4.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/idutils/idutils-"
                          version ".tar.xz"))
      (sha256
       (base32
        "1hmai3422iaqnp34kkzxdnywl7n7pvlxp11vrw66ybxn9wxg90c1"))))
    (build-system gnu-build-system)
    (inputs `(;; TODO: Add Emacs as an input for byte-compilation.
              ;; ("emacs" ,emacs)
              ("patch/gets"
               ,(search-patch "diffutils-gets-undeclared.patch"))))
    (arguments `(#:patches (list (assoc-ref %build-inputs "patch/gets"))))
    (home-page "http://www.gnu.org/software/idutils/")
    (synopsis "GNU Idutils, a text searching utility")
    (description
     "An \"ID database\" is a binary file containing a list of file
names, a list of tokens, and a sparse matrix indicating which
tokens appear in which files.

With this database and some tools to query it, many
text-searching tasks become simpler and faster.  For example,
you can list all files that reference a particular `\\#include'
file throughout a huge source hierarchy, search for all the
memos containing references to a project, or automatically
invoke an editor on all files containing references to some
function or variable.  Anyone with a large software project to
maintain, or a large set of text files to organize, can benefit
from the ID utilities.

Although the name `ID' is short for `identifier', the ID
utilities handle more than just identifiers; they also treat
other kinds of tokens, most notably numeric constants, and the
contents of certain character strings.")
    (license gpl3+)))
