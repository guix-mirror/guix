;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages which)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public which
  (package
    (name "which")
    (version "2.20")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/which/which-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1y2p50zadb36izzh2zw4dm5hvdiydqf3qa88l8kav20dcmfbc5yl"))))
    (build-system gnu-build-system)
    (home-page "https://gnu.org/software/which/")
    (synopsis "Find full path of shell commands")
    (description
     "GNU Which takes one or more arguments. For each of its arguments
it prints to stdout the full path of the executables that would have
been executed when this argument had been entered at the shell
prompt. It does this by searching for an executable or script in the
directories listed in the environment variable PATH using the same
algorithm as bash(1).")
    (license gpl3+))) ; some files are under GPLv2+
