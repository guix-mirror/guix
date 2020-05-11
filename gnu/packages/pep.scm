;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages pep)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix hg-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages xml))

(define-public yml2
  (package
    (name "yml2")
    (version "2.6.3")
    (source (origin
       (method hg-fetch)
       (uri (hg-reference
             (url "https://pep.foundation/dev/repos/yml2")
             (changeset version)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "10jjjyq1mz18zkzvxd62aba00h69gd9cglisqcvb81j67ml2v1bx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-lxml" ,python-lxml)))
    (home-page "https://fdik.org/yml/")
    (synopsis "Use a Domain Specific Language for XML without defining
a grammar")
    (description "The YML compiler is a small Python script.  It
provides the command line front end yml2c.  As default, it compiles
your script and outputs to stdout, that usually is the terminal.  Your
shell provides options to redirect the output into a pipe or a file.")
    (license license:gpl2)))
