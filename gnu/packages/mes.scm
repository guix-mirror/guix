;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages man)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public nyacc-0.86
  (package
    (name "nyacc")
    (version "0.86.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/"
                                  name "-" version ".tar.gz"))
              (patches (search-patches "nyacc-binary-literals.patch"))
              (sha256
               (base32
                "0lkd9lyspvhxlfs0496gsllwinh62jk9wij6gpadvx9gwz6yavd9"))))
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

(define-public nyacc
  (package
    (inherit nyacc-0.86)
    (version "0.99.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hl5qxx19i4x1r0839sxm19ziqq65g4hy97yik81cc2yb9yvgyv3"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* (find-files "." "^Makefile\\.in$")
                    (("^SITE_SCM_DIR =.*")
                     "SITE_SCM_DIR = \
@prefix@/share/guile/site/@GUILE_EFFECTIVE_VERSION@\n")
                    (("^SITE_SCM_GO_DIR =.*")
                     "SITE_SCM_GO_DIR = \
@prefix@/lib/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache\n")
                    (("^INFODIR =.*")
                     "INFODIR = @prefix@/share/info\n")
                    (("^DOCDIR =.*")
                     "DOCDIR = @prefix@/share/doc/$(PACKAGE_TARNAME)\n"))
                  #t))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)))))

(define-public mes
  (package
    (name "mes")
    (version "0.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/mes/"
                                  "mes-" version ".tar.gz"))
              (sha256
               (base32
                "04pajp8v31na34ls4730ig5f6miiplhdvkmsb9ls1b8bbmw2vb4n"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (propagated-inputs
     `(("mescc-tools" ,mescc-tools)
       ("nyacc" ,nyacc)))
    (native-inputs
     `(("guile" ,guile-2.2)
       ,@(let ((target-system (or (%current-target-system)
                                  (%current-system))))
           (cond
            ((string-prefix? "x86_64-linux" target-system)
             ;; Use cross-compiler rather than #:system "i686-linux" to get
             ;; MesCC 64 bit .go files installed ready for use with Guile.
             `(("i686-linux-binutils" ,(cross-binutils "i686-unknown-linux-gnu"))
               ("i686-linux-gcc" ,(cross-gcc "i686-unknown-linux-gnu"))))
            (else
             '())))
       ("graphviz" ,graphviz)
       ("help2man" ,help2man)
       ("perl" ,perl)                 ; build-aux/gitlog-to-changelog
       ("texinfo" ,texinfo)))
    (arguments
     `(#:strip-binaries? #f))  ; binutil's strip b0rkes MesCC/M1/hex2 binaries
    (synopsis "Scheme interpreter and C compiler for full source bootstrapping")
    (description
     "GNU Mes--Maxwell Equations of Software--brings the Reduced Binary Seed
bootstrap to Guix and aims to help create full source bootstrapping for
GNU/Linux distributions.  It consists of a mutual self-hosting Scheme
interpreter in C and a Nyacc-based C compiler in Scheme and is compatible with
Guile.")
    (home-page "https://gnu.org/software/mes")
    (license gpl3+)))

(define-public mescc-tools
  (package
    (name "mescc-tools")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://git.savannah.nongnu.org/cgit/mescc-tools.git/snapshot/"
                    name "-Release_" version
                    ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06jpvq6xfjzn2al6b4rdwd3zv3h4cvilc4n9gqcnjr9cr6wjpw2n"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (synopsis "Tools for the full source bootstrapping process")
    (description
     "Mescc-tools is a collection of tools for use in a full source
bootstrapping process.  It consists of the M1 macro assembler, the hex2
linker, the blood-elf symbol table generator, the kaem shell, exec_enable and
get_machine.")
    (home-page "https://savannah.nongnu.org/projects/mescc-tools")
    (license gpl3+)))
