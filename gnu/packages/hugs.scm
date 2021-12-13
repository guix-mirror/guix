;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages hugs)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gcc))

(define-public hugs
  (package
    (name "hugs")
    (version "Sep2006")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.haskell.org/hugs/downloads/2006-09/"
                          name "98-plus-" version ".tar.gz"))
      (sha256
       (base32
        "1mdy4aq4campgmnpc2qwq7bsbfhaxfsqdghbyyz2wms4lnfcmyma"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-sh-n-cp
           (lambda _
             (substitute*
                 '("configure"
                   "libraries/configure"
                   "packages/time/configure"
                   "packages/base/configure"
                   "packages/X11/configure"
                   "packages/HGL/configure"
                   "packages/OpenAL/configure"
                   "packages/OpenGL/configure"
                   "packages/network/configure"
                   "packages/unix/configure"
                   "packages/Cabal/tests/HSQL/configure"
                   "packages/ALUT/configure"
                   "packages/GLUT/configure"
                   "packages/base/cbits/execvpe.c"
                   "packages/base/System/Process/Internals.hs"
                   "packages/Cabal/Distribution/attic"
                   "packages/Cabal/Distribution/Simple/Register.hs"
                   "packages/Cabal/Distribution/Simple/Hugs.hs"
                   "tools/hugs-hc"
                   "src/machdep.c"
                   "libraries/Makefile.in")
               (("/bin/sh") (which "sh")))
             (substitute* '("demos/Makefile.in"
                            "libraries/Makefile.in")
               (("/bin/cp") (which "cp")))
             #t)))
       #:tests? #f)) ; no test target
    ;; FIXME: Fails to build with GCC 5.
    (native-inputs (list gcc-4.9))
    (home-page "https://www.haskell.org/hugs/")
    (synopsis "Functional programming system based on Haskell 98")
    (description
     "Hugs 98 is an interpreter and programming environment for developing
Haskell programs.  It provides an almost complete implementation of Haskell
98, which includes expression and pattern syntax, primitives for monadic I/O,
with support for simple interactive programs, handle-based I/O, and exception
handling.  Hugs has a nearly complete implementation of the Haskell module
system and supports a number of advanced extensions.")
   (license (non-copyleft "file://License"
                          "See License in the distribution."))))
