;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2015, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
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

(define-module (gnu packages bison)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages flex)
  #:use-module (srfi srfi-1))

(define-public bison
  (package
    (name "bison")
    (version "3.7.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/bison/bison-"
                          version ".tar.xz"))
      (sha256
       (base32
        "1kzkxrd3z4262k2sbxmyh9k5g5r2lakw0gv44l2hb4i1wbhqrmk7"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Building in parallel on many-core systems may cause an error such as
       ;; "mv: cannot stat 'examples/c/reccalc/scan.stamp.tmp': No such file or
       ;; directory".  See <https://bugs.gnu.org/36238>.
       #:parallel-build? #f
       ;; Similarly, when building tests in parallel, Make may produce this error:
       ;; "./examples/c/reccalc/scan.l:13:10: fatal error: parse.h: No such file
       ;; or directory".  Full log in <https://bugs.gnu.org/36238>.
       #:parallel-tests? #f))
    (native-inputs (list perl
                         ;; m4 is not present in PATH when cross-building.
                         m4))
    (inputs (list flex))
    (propagated-inputs (list m4))
    (home-page "https://www.gnu.org/software/bison/")
    (synopsis "Yacc-compatible parser generator")
    (description
     "GNU Bison is a general-purpose parser generator.  It can build a
deterministic or generalized LR parser from an annotated, context-free
grammar.  It is versatile enough to have many applications, from parsers for
simple tools through complex programming languages.

Bison also provides an implementation of @command{yacc}, as specified by POSIX.")
    (license gpl3+)))

(define-public bison-3.0
  (package
    (inherit bison)
    (version "3.0.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/bison/bison-"
                          version ".tar.xz"))
      (sha256
       (base32
        "0f7kjygrckkx8vas2nm673592jif0a9mw5g8207f6hj6h4pfyp07"))))))
