;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017,2018 Jan Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (gnu packages guile)
  #:use-module (gnu packages man)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public nyacc
  (package
    (name "nyacc")
    (version "0.83.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/"
                                  name "-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0120n0mdb6r58c4jc024dhwqy5s8a20waknijfhqjc59a884lrd6"))))
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

(define-public nyacc-for-mes
  (package
    (inherit nyacc)
    (version "0.80.42")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://gitlab.com/janneke/nyacc"
                                    "/-/archive/v" version
                                    "/nyacc-" version ".tar.gz"))
                (sha256
                 (base32
                  "0c8c8kxir0h2d4nxr131xbkfs7c80haipmkp2g6677sh14wn0b3y"))))))

(define-public mes
  (let ((triplet "i686-unknown-linux-gnu"))
    (package
      (name "mes")
      (version "0.16.1")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://gitlab.com/janneke/mes"
                                    "/-/archive/v" version
                                    "/mes-" version ".tar.gz"))
                (sha256
                 (base32
                  "0qghlbx2qn674q8vckxpzsd0p845kclg457bw6r25jpmslgm0bz2"))))
      (build-system gnu-build-system)
      (supported-systems '("i686-linux" "x86_64-linux"))
      (propagated-inputs
       `(("mescc-tools" ,mescc-tools)
         ("nyacc" ,nyacc-for-mes)))
      (native-inputs
       `(("guile" ,guile-2.2)
         ,@(if (not (string-prefix? "i686-linux" (or (%current-target-system)
                                                     (%current-system))))
               ;; Use cross-compiler rather than #:system "i686-linux" to get
               ;; MesCC 64 bit .go files installed ready for use with Guile.
               `(("i686-linux-binutils" ,(cross-binutils triplet))
                 ("i686-linux-gcc" ,(cross-gcc triplet)))
               '())
         ("help2man" ,help2man)
         ("perl" ,perl)                 ;build-aux/gitlog-to-changelog
         ("texinfo" ,texinfo)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'generate-changelog
             (lambda _
               (with-output-to-file "ChangeLog"
                 (lambda ()
                   (display "Please run
    build-aux/gitlog-to-changelog --srcdir=<git-checkout> > ChangeLog\n")))
               #t))
           (delete 'strip)))) ; binutil's strip b0rkes MesCC/M1/hex2 binaries
      (synopsis "Scheme interpreter and C compiler for full source bootstrapping")
      (description
       "Mes [Maxwell Equations of Software] aims to create full source
bootstrapping for GuixSD.  It consists of a mutual self-hosting [close to
Guile-] Scheme interpreter prototype in C and a Nyacc-based C compiler in
[Guile] Scheme.")
      (home-page "https://gitlab.com/janneke/mes")
      (license gpl3+))))

(define-public mescc-tools
  (package
    (name "mescc-tools")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/oriansj/mescc-tools/archive/Release_"
                    version
                    ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rsxbjc3bg0jl3h7ai4hndxx2iyyk8bvwj9nd3xv2vgz3bmypnah"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'install 'install-2
                    (lambda _
                      (let ((out (assoc-ref %outputs "out")))
                       (copy-file "bin/blood-elf" (string-append out "/bin/blood-elf"))))))))
    (synopsis "Tools for the full source bootstrapping process")
    (description
     "Mescc-tools is a collection of tools for use in a full source
bootstrapping process.  Currently consists of the M1 macro assembler and the
hex2 linker.")
    (home-page "https://github.com/oriansj/mescc-tools")
    (license gpl3+)))
