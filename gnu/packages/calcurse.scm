;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages ncurses))

(define-public calcurse
  (package
    (name "calcurse")
    (version "4.2.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://calcurse.org/files/calcurse-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0il0y06akdqgy0f9p40m4x6arn66nh7sr1w1i41bszycs7div266"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (native-inputs `(("tzdata" ,tzdata-2017a)))
    (arguments
     ;; The ical tests all want to create a ".calcurse" directory, and may
     ;; fail with "cannot create directory '.calcurse': File exists" if run
     ;; concurently.
     `(#:parallel-tests? #f
       ;; Since this tzdata is only used for tests and not referenced by the
       ;; built package, used the "fixed" obsolete version of tzdata and ensure
       ;; it does not sneak in to the closure.
       #:disallowed-references (,tzdata-2017a)
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'check-setup
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "TZDIR"   ;for test/ical-007.sh
                              (string-append (assoc-ref inputs "tzdata")
                                             "/share/zoneinfo")))))))
    (home-page "http://www.calcurse.org")
    (synopsis "Text-based calendar and scheduling")
    (description
     "Calcurse is a text-based calendar and scheduling application.  It helps
keep track of events, appointments and everyday tasks.  A configurable
notification system reminds user of upcoming deadlines, and the curses based
interface can be customized to suit user needs.  All of the commands are
documented within an online help system.")
    (license bsd-2)))
