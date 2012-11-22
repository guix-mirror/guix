;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro packages autotools)
  #:use-module (distro)
  #:use-module (distro packages perl)
  #:use-module (distro packages m4)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public autoconf
  (package
    (name "autoconf")
    (version "2.69")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/autoconf/autoconf-"
                          version ".tar.xz"))
      (sha256
       (base32
        "113nlmidxy9kjr45kg9x3ngar4951mvag1js2a3j8nxcz34wxsv4"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl)
       ("m4" ,m4)))
    ;; XXX: testsuite: 209 and 279 failed. The latter is an impurity. It
    ;; should use our own "cpp" instead of "/lib/cpp".
    (arguments `(#:tests? #f))
    (home-page
     "http://www.gnu.org/software/autoconf/")
    (synopsis
     "GNU Autoconf, a part of the GNU Build System")
    (description
     "GNU Autoconf is an extensible package of M4 macros that produce
shell scripts to automatically configure software source code
packages.  These scripts can adapt the packages to many kinds of
UNIX-like systems without manual user intervention.  Autoconf
creates a configuration script for a package from a template
file that lists the operating system features that the package
can use, in the form of M4 macro calls.")
    (license "GPLv3+"))) ; some files are under GPLv2+

(define-public automake
  (package
    (name "automake")
    (version "1.12.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/automake/automake-"
                          version ".tar.xz"))
      (sha256
       (base32
        "1k4pa3rmj626n5d39rc9041dc71lv8nzd341k53dw07iflkwinim"))))
    (build-system gnu-build-system)
    (inputs
     `(("autoconf" ,autoconf)
       ("perl" ,perl)))
    (home-page
     "http://www.gnu.org/software/automake/")
    (synopsis
     "GNU Automake, a GNU standard-compliant makefile generator")
    (description
     "GNU Automake is a tool for automatically generating
`Makefile.in' files compliant with the GNU Coding
Standards.  Automake requires the use of Autoconf.")
    (license "GPLv2+"))) ; some files are under GPLv3+

(define-public libtool
  (package
    (name "libtool")
    (version "2.4.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/libtool/libtool-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0649qfpzkswgcj9vqkkr9rn4nlcx80faxpyqscy2k1x9c94f93dk"))))
    (build-system gnu-build-system)
    (native-inputs `(("m4" ,m4)
                     ("perl" ,perl)))
    (arguments
     ;; TODO: Use `TESTSUITEFLAGS=-jN' for tests.
     `(#:patches (list (assoc-ref %build-inputs "patch/skip-tests"))))
    (inputs `(("patch/skip-tests"
               ,(search-patch "libtool-skip-tests.patch"))))
    (synopsis "GNU Libtool, a generic library support script")
    (description
     "GNU libtool is a generic library support script.  Libtool hides the
complexity of using shared libraries behind a consistent, portable interface.

To use libtool, add the new generic library building commands to your
Makefile, Makefile.in, or Makefile.am.  See the documentation for
details.")
    (license "GPLv3+")
    (home-page "http://www.gnu.org/software/libtool/")))
