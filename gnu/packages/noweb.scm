;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages perl))

(define-public noweb
  (package
    (name "noweb")
    (version "2.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nrnrnr/noweb")
             (commit (string-append "v" (string-join (string-split version #\.)
                                                     "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1160i2ghgzqvnb44kgwd6s3p4jnk9668rmc15jlcwl7pdf3xqm95"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bind-early
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (substitute* (list "src/lib/nwmtime"
                                  "src/shell/htmltoc")
                 (("exec perl ")
                  (format #f "exec ~a " (which "perl"))))
               (substitute* "src/shell/noweb"
                 ((" cpif ")
                  (format #f " ~a/cpif " bin)))
               #t)))
         (add-before 'install 'pre-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share/texmf/tex/latex"))
               #t)))
         (add-after 'install 'post-install
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
               #t)))
         (replace 'configure
           (lambda _
             ;; Jump in the source.
             (chdir "src")
             #t)))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "BIN=" out "/bin")
                            (string-append "LIB=" out "/lib")
                            (string-append "MAN=" out "/share/man")
                            (string-append "TEXINPUTS=" out
                                           "/share/texmf/tex/latex")))
       #:tests? #f))                              ; no tests
    (inputs
     (list perl))
    (home-page "https://www.cs.tufts.edu/~nr/noweb/")
    (synopsis "Literate programming tool")
    (description
     "Noweb is designed to meet the needs of literate programmers while
remaining as simple as possible.  Its primary advantages are simplicity,
extensibility, and language-independence—especially noticeable when compared
with other literate-programming tools.  noweb uses 5 control sequences to
WEB's 27.  The noweb manual is only 4 pages; an additional page explains how
to customize its LaTeX output.  noweb works “out of the box” with any
programming language, and supports TeX, LaTeX, HTML, and troff back ends.")
    (license
     (list bsd-2                        ; dual-licenced under this and…
           (fsf-free "https://www.cs.tufts.edu/~nr/noweb/#copyright")))))
