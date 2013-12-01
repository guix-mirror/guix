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

(define-module (gnu packages unrtf)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public unrtf
  (package
    (name "unrtf")
    (version "0.19.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/unrtf/" version
                                 "/unrtf-" version ".tar.gz"))
             (sha256
              (base32
               "1c15miv4ni395wix6qxiqgjfspd6ssdk1ly6mm8srh0jgpidgjzf"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; No `configure' script; do it by hand.
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "Makefile"
                       (("/usr/local") out))))
                 %standard-phases)

       ;; There are test files, but apparently no automated test suite.
       #:tests? #f))
    (home-page "http://www.gnu.org/software/unrtf")
    (synopsis "Convert Rich Text Format documents to other formats")
    (description
     "GNU UnRTF converts text documents from RTF to HTML, LaTeX, or troff. 
It supports changes in font characteristics, underlines and strikethroughs,
superscripts and subscripts, and more.")
    (license gpl2+)))
