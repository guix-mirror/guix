;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (distro packages xml)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public expat
  (package
    (name "expat")
    (version "2.1.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/expat/expat/"
                                 version "/expat-" version ".tar.gz"))
             (sha256
              (base32
               "11pblz61zyxh68s5pdcbhc30ha1b2vfjd83aiwfg4vc15x3hadw2"))))
    (build-system gnu-build-system)
    (home-page "http://www.libexpat.org/")
    (synopsis "A stream-oriented XML parser library written in C")
    (description
     "Expat is an XML parser library written in C.  It is a
stream-oriented parser in which an application registers handlers for
things the parser might find in the XML document (like start tags).")
    (license license:expat)))
