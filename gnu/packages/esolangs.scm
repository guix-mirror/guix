;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Hendursaga <hendursaga@yahoo.com>
;;; Copyright © 2020 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (gnu packages esolangs)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public cfunge
  (package
    (name "cfunge")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/VorpalBlade/cfunge")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18ir0h10vxdb5jb57w5hjbgi8spjxg9x2148agadhhmbhsja02m7"))))
    (build-system cmake-build-system)
    (arguments
      ;; The tests are not designed to be run and evaluated automatically.
     '(#:tests? #f))
    (inputs
     (list ncurses))
    (home-page "https://github.com/VorpalBlade/cfunge")
    (synopsis "Fast conforming Befunge93/98/109 interpreter in C")
    (description "@command{cfunge} is a fast conforming Befunge93/98/109 interpreter
written in C.  It supports several @dfn{fingerprints} (opt-in language extensions
identified by unique ID codes).")
    (license license:gpl3)))

(define-public lolcode-lci
  (package
    (name "lolcode-lci")
    (version "0.11.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/justinmeza/lci")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0syw60b93iajgh91ffchirwwhm2kix2753ibx845kyrhzggmdh2l"))))
    (build-system cmake-build-system)
    (inputs
     (list readline))
    (native-inputs
     (list python-2))         ; for the tests
    (synopsis "LOLCODE interpreter written in C")
    (description
     "@code{lci} is a LOLCODE interpreter written in C and is designed to be
correct, portable, fast, and precisely documented.
@enumerate
@item correct: Every effort has been made to test lci's conformance to the
LOLCODE language specification.  Unit tests come packaged with the lci source code.
@item portable: lci follows the widely ported ANSI C specification allowing it
to compile on a broad range of systems.
@item fast: Much effort has gone into producing simple and efficient code
whenever possible to the extent that the above points are not compromized.
@end enumerate")
    (home-page "http://lolcode.org/")
    (license license:gpl3+)))

(define-public shakespeare-spl
  (package
    (name "shakespeare-spl")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/shakespearelang/spl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1lxpfhv01kcwy4l4xgx7j765g8g0v74jns0cr908nfw55h5fy1hj"))
              (patches
               (search-patches
                "shakespeare-spl-fix-grammar.patch"))))
    (build-system copy-build-system)
    (arguments
     `(#:imported-modules (,@%gnu-build-system-modules
                           (guix build copy-build-system))
       #:modules ((guix build copy-build-system)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  (guix build utils))
       #:install-plan '(("spl/bin" "bin")
                        ("spl/lib" "lib")
                        ("spl/include" "include"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'build
           (assoc-ref gnu:%standard-phases 'build)))))
    (native-inputs
     (list bison flex))
    (home-page "http://shakespearelang.sourceforge.net/")
    (synopsis "Write programs like Shakespearean plays")
    (description "Shakespeare is a programming language with the design goal
of making programs read like plays.  The characters in the play are variables.
If you want to assign a character, let's say Juliet, a positive value, you put
her and another character on the stage and let that character praise Juliet.")
    (license license:gpl2+)))
