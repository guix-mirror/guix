;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages parallel)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl))

(define-public parallel
  (package
    (name "parallel")
    (version "20140322")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/parallel/parallel-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1zk95pbdbx7dhcg8z168zg8i4r36b39rim6dffxlr6kghlch6sdl"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (home-page "http://www.gnu.org/software/parallel/")
    (synopsis "Build and execute command lines in parallel")
    (description
     "GNU Parallel is a tool for executing shell jobs in parallel using one
or more computers.  Jobs can consist of single commands or of scripts
and they are executed on lists of files, hosts, users or other items.")
    (license gpl3+)))
