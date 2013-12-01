;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages gprolog)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public gprolog
  (package
    (name "gprolog")
    (version "1.4.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gprolog/gprolog-" version
                          ".tar.gz"))
      (sha256
       (base32
        "13miyas47bmijmadm68cbvb21n4s156gjafz7kfx9brk9djfkh0q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-cons-before
                 'configure 'change-dir-n-fix-shells
                 (lambda _
                   (chdir "src")
                   (substitute* "configure"
                     (("-/bin/sh")  (string-append "-"  (which "sh")))
                     (("= /bin/sh") (string-append "= " (which "sh")))))
                 %standard-phases)))
    (home-page "https://www.gnu.org/software/gprolog/")
    (synopsis "Prolog compiler")
    (description
     "GNU Prolog is a standards-compliant Prolog compiler with constraint
solving over finite domains.  It accepts Prolog+ constraint programs and
produces a compiled, native binary which can function in a stand-alone
manner.  It also features an interactive interpreter.")
    (license (list gpl2+ lgpl3+))))
