;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages gcal)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public gcal
  (package
    (name "gcal")
    (version "4.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gcal/gcal-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1av11zkfirbixn05hyq4xvilin0ncddfjqzc4zd9pviyp506rdci"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; Adjust the bundled gnulib to work with glibc 2.28.  See e.g.
                 ;; "m4-gnulib-libio.patch".  This is a phase rather than patch
                 ;; or snippet to work around <https://bugs.gnu.org/32347>.
                 (substitute* (find-files "lib" "\\.c$")
                   (("#if defined _IO_ftrylockfile")
                    "#if defined _IO_EOF_SEEN"))
                 (substitute* "lib/stdio-impl.h"
                   (("^/\\* BSD stdio derived implementations")
                    (string-append "#if !defined _IO_IN_BACKUP && defined _IO_EOF_SEEN\n"
                                   "# define _IO_IN_BACKUP 0x100\n"
                                   "#endif\n\n"
                                   "/* BSD stdio derived implementations")))
                 #t))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/gcal/")
    (synopsis "Calculating and printing a wide variety of calendars")
    (description
     "Gcal is a program to calculate and print calendars on the
command-line.  Calendars can be printed in 1-month, 3-month or whole-year
views.  In addition, eternal holiday lists can be generated for many
countries, which can be complemented by user-made lists of fixed dates to
make an agenda.  Gcal can also calculate astronomical data, such as the
phases of the moon, and supports alternative calendar formats: Julian,
Gregorian, Islamic, Chinese and more.")
    (license gpl3+)))
