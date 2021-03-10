;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages cobol)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl))

(define-public gnucobol
  (package
    (name "gnucobol")
    (version "3.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://gnu/gnucobol/gnucobol-"
               version ".tar.xz"))
        (sha256
         (base32
          "0x15ybfm63g7c9340fc6712h9v59spnbyaz4rf85pmnp3zbhaw2r"))))
    (arguments
     '(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'place-cobol85-test-suite
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((newcob (assoc-ref inputs "newcob")))
               (copy-file newcob "tests/cobol85/newcob.val.Z"))))
         (add-before 'check 'set-TERM
           ;; Some tests expect a known terminal
           (lambda _ (setenv "TERM" "xterm-256color"))))
       #:test-target "checkall"))
    (native-inputs
     `(("perl" ,perl)))
    (inputs
     `(("bdb" ,bdb)
       ("gmp" ,gmp)
       ("ncurses" ,ncurses)
       ("newcob" ,(origin
                    (method url-fetch)
                    (uri "https://www.itl.nist.gov/div897/ctg/suites/newcob.val.Z")
                    (sha256
                     (base32
                      "1yb1plmv4firfnbb119r2vh1hay221w1ya34nyz0qwsxppfr56hy"))))))
    (build-system gnu-build-system)
    (home-page "https://savannah.gnu.org/projects/gnucobol/")
    (synopsis "A modern COBOL compiler")
    (description "GnuCOBOL is a free, modern COBOL compiler.  GnuCOBOL
implements a substantial part of the COBOL 85, COBOL 2002 and COBOL 2014
standards and X/Open COBOL, as well as many extensions included in other
COBOL compilers (IBM COBOL, MicroFocus COBOL, ACUCOBOL-GT and others).
GnuCOBOL translates COBOL into C and compiles the translated code using
a native C compiler.")
    (license gpl3+)))
