;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
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

(define-module (gnu packages cpio)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public cpio
  (package
    (name "cpio")
    (version "2.13")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/cpio/cpio-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0vbgnhkawdllgnkdn6zn1f56fczwk0518krakz2qbwhxmv2vvdga"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; Remove superfluous declaration that trips GCC 10.
                 (substitute* "src/global.c"
                   (("char \\*program_name;")
                    ""))))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/cpio/")
    (synopsis "Manage cpio and tar file archives")
    (description
     "GNU cpio copies files into or out of cpio or tar archives.  Indeed,
many formats are supported, including legacy formats.  The format is
determined automatically by the program and is handled appropriately.
Furthermore, the location of the archive is not important.  It can be another
file on the drive, a tape, or data on a pipe.")
    (license gpl3+)))
