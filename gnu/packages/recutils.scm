;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages recutils)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gnupg))

(define-public recutils
  (package
   (name "recutils")
   (version "1.6")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/recutils/recutils-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0dxmz73n4qaasqymx97nlw6in98r6lnsfp0586hwkn95d3ll306s"))))
   (build-system gnu-build-system)
   (native-inputs `(("emacs" ,emacs)
                    ("bc" ,bc)))

   ;; TODO: Add more optional inputs.
   ;; FIXME: Our Bash doesn't have development headers (need for the 'readrec'
   ;; built-in command), but it's not clear how to get them installed.
   (inputs `(("curl" ,curl)
             ("libgcrypt" ,libgcrypt)
             ("check" ,check)))
   (synopsis "Manipulate plain text files as databases")
   (description
    "Recutils is a set of tools and libraries for creating and
manipulating text-based, human-editable databases.  Despite being text-based,
databases created with Recutils carry all of the expected features such as
unique fields, primary keys, time stamps and more. Many different field types
are supported, as is encryption.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/recutils/")))
