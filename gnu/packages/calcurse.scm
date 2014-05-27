;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages calcurse)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses))

(define-public calcurse
  (package
    (name "calcurse")
    (version "3.1.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://calcurse.org/files/calcurse-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1qwhffwhfg7bjxrviwlcrhnfw0976d39da8kfspq6dgd9nqv68a1"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://www.calcurse.org")
    (synopsis "Text-based calendar and scheduling")
    (description
     "Calcurse is a text-based calendar and scheduling application. It helps
keep track of events, appointments and everyday tasks. A configurable
notification system reminds user of upcoming deadlines, and the curses based
interface can be customized to suit user needs. All of the commands are
documented within an online help system.")
    (license bsd-2)))
