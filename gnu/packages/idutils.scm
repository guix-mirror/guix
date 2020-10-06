;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages idutils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages))

(define-public idutils
  (package
    (name "idutils")
    (version "4.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/idutils/idutils-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1hmai3422iaqnp34kkzxdnywl7n7pvlxp11vrw66ybxn9wxg90c1"))
             (patches (search-patches "diffutils-gets-undeclared.patch"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (substitute* (find-files "lib" "\\.c$")
                   (("#if defined _IO_ftrylockfile")
                    "#if defined _IO_EOF_SEEN"))
                 (substitute* "lib/stdio-impl.h"
                   (("^/\\* BSD stdio derived implementations")
                    (string-append "#if !defined _IO_IN_BACKUP && defined _IO_EOF_SEEN\n"
                                   "# define _IO_IN_BACKUP 0x100\n"
                                   "#endif\n\n"
                                   "/* BSD stdio derived implementations")))
                 #t))))
    (build-system gnu-build-system)
    (native-inputs `(("emacs" ,emacs-minimal)))
    (home-page "https://www.gnu.org/software/idutils/")
    (synopsis "Identifier database utilities")
    (description
     "The GNU idutils package includes tools to create an index of textual
tokens used in a list of file names and then to query that index.  Thus, it
allows the user to, for example, find all the uses of a particular function
in a large programming project.  In addition to handling textual tokens, it
can also handle numeric constants and the contents of character strings.")
    (license gpl3+)))
