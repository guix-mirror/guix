;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages texinfo)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ncurses))

(define-public texinfo
  (package
    (name "texinfo")
    (version "5.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/texinfo/texinfo-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1njfwh2z34r2c4r0iqa7v24wmjzvsfyz4vplzry8ln3479lfywal"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("xz" ,xz)
              ("perl" ,perl)))
    (home-page "http://www.gnu.org/software/texinfo/")
    (synopsis "The GNU documentation format")
    (description
     "Texinfo is the official documentation format of the GNU project.  It
uses a single source file using explicit commands to produce a final document
in any of several supported output formats, such as HTML or PDF.  This
package includes both the tools necessary to produce Info documents from
their source and the command-line Info reader.  The emphasis of the language
is on expressing the content semantically, avoiding physical markup commands.")
    (license gpl3+)))

(define-public texinfo-4
  (package (inherit texinfo)
    (version "4.13a")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/texinfo/texinfo-"
                    version
                    ".tar.lzma"))
              (sha256
               (base32
                "1rf9ckpqwixj65bw469i634897xwlgkm5i9g2hv3avl6mv7b0a3d"))))
    (inputs `(("ncurses" ,ncurses) ("xz" ,xz)))))

(define-public texi2html
  (package
    (name "texi2html")
    (version "5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/" name "/" name "-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1yprv64vrlcbksqv25asplnjg07mbq38lfclp1m5lj8cw878pag8"))
              (patches
               (list (search-patch "texi2html-document-encoding.patch")))
              (snippet
               ;; This file is modified by the patch above, but reset its
               ;; timestamp so we don't trigger the rule to update PO files,
               ;; which would require Gettext.
               ;; See <http://bugs.gnu.org/18247>.
               '(utime "texi2html.pl" 0 0 0 0))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (home-page "http://www.nongnu.org/texi2html/")
    (synopsis "Convert Texinfo to HTML")
    (description
     "Texi2HTML is a Perl script which converts Texinfo source files to HTML
output.  It now supports many advanced features, such as internationalization
and extremely configurable output formats.

Development of Texi2HTML moved to the GNU Texinfo repository in 2010, since it
was meant to replace the makeinfo implementation in GNU Texinfo.  The route
forward for authors is, in most cases, to alter manuals and build processes as
necessary to use the new features of the makeinfo/texi2any implementation of
GNU Texinfo.  The Texi2HTML maintainers (one of whom is the principal author
of the GNU Texinfo implementation) do not intend to make further releases of
Texi2HTML.")
    ;; Files in /lib under lgpl2.1+ and x11
    (license gpl2+)))
