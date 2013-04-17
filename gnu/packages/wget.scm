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

(define-module (gnu packages wget)
  #:use-module (guix licenses)
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages perl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public wget
  (package
    (name "wget")
    (version "1.14")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/wget/wget-"
                          version ".tar.xz"))
      (sha256
       (base32
        "0yqllj3nv9p3vqbdm6j4nvpjcwf1y19rq8sd966nrbd2qvvxfq8p"))))
    (build-system gnu-build-system)
    (inputs
     `(("gnutls" ,gnutls)
       ("perl" ,perl)
       ("gettext" ,guix:gettext)))
    (arguments
     '(#:phases
       (alist-cons-before 'build 'patch-/usr/bin/env
                          (lambda _
                            (for-each patch-shebang
                                      '("doc/texi2pod.pl" "tests/run-px")))
                          %standard-phases)))
    (home-page "http://www.gnu.org/software/wget/")
    (synopsis "Non-interactive command-line utility for downloading files")
    (description
     "GNU Wget is a free software package for retrieving files using HTTP,
HTTPS and FTP, the most widely-used Internet protocols.  It is a
non-interactive commandline tool, so it may easily be called from
scripts, cron jobs, terminals without X-Windows support, etc.")
    (license gpl3+))) ; some files are under GPLv2+
