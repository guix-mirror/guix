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

(define-module (gnu packages noweb)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public noweb
  (package
    (name "noweb")
    (version "2.11b")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://www.eecs.harvard.edu/pub/nr/noweb-"
                                 version ".tgz"))
             (sha256
              (base32
               "10hdd6mrk26kyh4bnng4ah5h1pnanhsrhqa7qwqy6dyv3rng44y9"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'install 'pre-install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (mkdir-p (string-append out "/share/texmf/tex/latex"))
                     #t))
                 (alist-cons-after
                  'install 'post-install
                  (lambda* (#:key outputs inputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out"))
                          (cu  (assoc-ref inputs "coreutils"))
                          (du  (assoc-ref inputs "diffutils")))
                      (with-directory-excursion out
                        (for-each (lambda (prog)
                                    (substitute* prog
                                      (("nawk") (which "awk"))))
                                  (append (map (lambda (x)
                                                 (string-append "bin/" x))
                                               '("noweb" "nountangle"
                                                 "noroots" "noroff"
                                                 "noindex"))
                                          (map (lambda (x)
                                                 (string-append "lib/" x))
                                               '("btdefn" "emptydefn" "noidx"
                                                 "pipedocs" "toascii" "tohtml"
                                                 "toroff" "totex" "unmarkup"))))
                        (substitute* "bin/cpif"
                          (("^PATH=.*$")
                           (string-append "PATH=" cu "/bin:" du "/bin\n"))))
                      #t))
                  (alist-replace
                   'configure
                   (lambda _
                     ;; Jump in the source.
                     (chdir "src")

                     ;; The makefile reads "source: FAQ", but FAQ isn't
                     ;; available.
                     (substitute* "Makefile"
                       (("FAQ") "")))
                   %standard-phases)))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "BIN=" out "/bin")
                            (string-append "LIB=" out "/lib")
                            (string-append "MAN=" out "/share/man")
                            (string-append "TEXINPUTS=" out
                                           "/share/texmf/tex/latex")))
       #:tests? #f))                              ; no tests
    (home-page "http://www.cs.tufts.edu/~nr/noweb/")
    (synopsis "Literate programming tool")
    (description
     "noweb is designed to meet the needs of literate programmers while
remaining as simple as possible.  Its primary advantages are simplicity,
extensibility, and language-independence—especially noticeable when compared
with other literate-programming tools.  noweb uses 5 control sequences to
WEB's 27.  The noweb manual is only 4 pages; an additional page explains how
to customize its LaTeX output.  noweb works “out of the box” with any
programming language, and supports TeX, LaTeX, HTML, and troff back ends.")
    (license (fsf-free "http://www.cs.tufts.edu/~nr/noweb/#copyright"))))
