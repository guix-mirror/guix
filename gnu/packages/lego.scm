;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages lego)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex))

(define-public nqc
  (package
    (name "nqc")
    (version "3.1.r6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://bricxcc.sourceforge.net/nqc/release/"
                                  "nqc-" version ".tgz"))
              (sha256
               (base32
                "0rp7pzr8xrdxpv75c2mi8zszzz2ypli4vvzxiic7mbrryrafdmdz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (arguments
     '(#:tests? #f                      ;no tests
       #:make-flags (list (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'rm-generated
                    ;; Regenerating compiler/lexer.cpp avoids an 'undefined
                    ;; reference to `isatty(int)'' error.
                    (lambda _
                      (for-each delete-file
                                '("compiler/lexer.cpp"
                                  "compiler/parse.cpp"))
                      #t))
                  (add-after 'unpack 'deal-with-tarbomb
                    (lambda _
                      (chdir "..")      ;tarbomb
                      #t)))))
    (home-page "http://bricxcc.sourceforge.net/nqc/")
    (synopsis "C-like language for Lego's MINDSTORMS")
    (description
     "Not Quite C (NQC) is a simple language for programming several Lego
MINDSTORMS products.  The preprocessor and control structures of NQC are very
similar to C.  NQC is not a general purpose language -- there are many
restrictions that stem from limitations of the standard RCX firmware.")
    (license license:mpl1.0)))
