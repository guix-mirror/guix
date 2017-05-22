;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages mes)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public nyacc
  (package
    (name "nyacc")
    (version "0.78.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xrnl2hl9rpl50n8cihvclcd951zj640fj5kpi3d6ihwcqjcdi0a"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("guile" ,guile-2.2)))
    (synopsis "LALR(1) Parser Generator in Guile")
    (description
     "NYACC is an LALR(1) parser generator implemented in Guile.
The syntax and nomenclature should be considered not stable.  It comes with
extensive examples, including parsers for the Javascript and C99 languages.")
    (home-page "https://savannah.nongnu.org/projects/nyacc")
    (license (list gpl3+ lgpl3+))))

(define-public mes
  (let ((commit "d4420bbcc9f994e2cce430cf156f383dc4092bca")
        (revision "0")
        (triplet "i686-unknown-linux-gnu")
        (version "0.6"))
    (package
      (name "mes")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/janneke/mes")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32 "0qqywk3siyhf08v7xac08lqldklrqfndlp495wgy6ii9fn93197k"))))
      (build-system gnu-build-system)
      (supported-systems '("i686-linux" "x86_64-linux"))
      (propagated-inputs
       `(("nyacc" ,nyacc)))
      (native-inputs
       `(("guile" ,guile-2.2)
         ,@(if (string-prefix? "x86_64-linux" (or (%current-target-system)
                                                  (%current-system)))
               ;; Use cross-compiler rather than #:system "i686-linux" to get
               ;; MesCC 64 bit .go files installed ready for use with Guile.
               `(("i686-linux-binutils" ,(cross-binutils triplet))
                 ("i686-linux-gcc" ,(cross-gcc triplet)))
               '())
         ("perl" ,perl)))               ;build-aux/gitlog-to-changelog
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'generate-changelog
             (lambda _
               (with-output-to-file "ChangeLog"
                 (lambda ()
                   (display "Please run
    build-aux/gitlog-to-changelog --srcdir=<git-checkout> > ChangeLog\n")))
               #t)))))
      (synopsis "Maxwell Equations of Software")
      (description
       "Mes aims to create full source bootstrapping for GuixSD.  It
consists of a mutual self-hosting [close to Guile-] Scheme interpreter
prototype in C and a Nyacc-based C compiler in [Guile] Scheme.")
      (home-page "https://gitlab.com/janneke/mes")
      (license gpl3+))))
