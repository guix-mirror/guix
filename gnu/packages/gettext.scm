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

(define-module (gnu packages gettext)
  #:use-module ((guix licenses) #:select (gpl3))
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml))

(define-public gettext
  (package
    (name "gettext")
    (version "0.18.3.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gettext/gettext-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0p940zmmw1lndvdhck2vrazikjhr02affwy47mmpfxqvacrrm3qd"))))
    (build-system gnu-build-system)
    (inputs
     `(("expat" ,expat)))
    (arguments
     `(#:phases (alist-cons-before
                 'configure 'link-expat
                 (lambda _
                   (substitute* "gettext-tools/configure"
                     (("LIBEXPAT=\"-ldl\"") "LIBEXPAT=\"-ldl -lexpat\"")
                     (("LTLIBEXPAT=\"-ldl\"") "LTLIBEXPAT=\"-ldl -lexpat\"")))
                (alist-cons-before
                 'check 'patch-tests
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((bash (which "sh")))
                     (substitute* (find-files "gettext-tools/tests"
                                              "^msgexec-[0-9]")
                       (("#![[:blank:]]/bin/sh")
                        (format #f "#!~a" bash)))
                     (substitute* (find-files "gettext-tools/gnulib-tests"
                                              "posix_spawn")
                       (("/bin/sh")
                        bash))))
                 %standard-phases))))
    (home-page "http://www.gnu.org/software/gettext/")
    (synopsis "Tools and documentation for translation")
    (description
     "gettext is a set of tools and documentation that provide a framework
for translating the textual output of programs into multiple languages.  It
provides translators with the means to create message catalogs, as well as an
Emacs mode to work with them, and a runtime library to load translated
messages from the catalogs.")
    (license gpl3))) ; some files are under GPLv2+
