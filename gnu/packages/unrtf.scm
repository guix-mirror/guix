;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages unrtf)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages base))

(define-public unrtf
  (package
    (name "unrtf")
    (version "0.21.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/unrtf/unrtf-"
                                 version ".tar.gz"))
             (patches (search-patches "unrtf-CVE-2016-10091.patch"))
             (sha256
              (base32
               "1pcdzf2h1prn393dkvg93v80vh38q0v817xnbwrlwxbdz4k7i8r2"))
             (modules '((guix build utils)))
             (snippet
              #~(begin
                  ;; The tarball includes site-specific generated files.
                  ;; Remove them.
                  (for-each delete-file '("config.log" "config.h"))
                  (for-each delete-file
                            (find-files "." "^Makefile$"))

                  ;; The config/ directory contains dangling symlinks to
                  ;; /usr/share/automake.
                  (for-each delete-file (find-files "config"))

                  ;; Regenerate the whole thing.
                  (setenv "PATH"
                          (string-append #$autoconf "/bin:"
                                         #$automake "/bin:"
                                         #$m4 "/bin:"
                                         #$grep "/bin:" #$sed "/bin:"
                                         #$coreutils "/bin:"
                                         (getenv "PATH")))
                  (invoke "autoreconf" "-vfi")))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/unrtf/")
    (synopsis "Convert Rich Text Format documents to other formats")
    (description
     "GNU UnRTF converts text documents from RTF to HTML, LaTeX, or troff.
It supports changes in font characteristics, underlines and strikethroughs,
superscripts and subscripts, and more.")
    (license gpl2+)))
