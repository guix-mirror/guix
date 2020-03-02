;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
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

(define-module (gnu packages acct)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public acct
  (package
    (name "acct")
    (version "6.6.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/acct/acct-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0gv6m8giazshvgpvwbng98chpas09myyfw1zr2y7hqxib0mvy5ac"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/acct/")
    (synopsis "Standard login and process accounting utilities")
    (description
     "GNU acct provides a means for system administrators to determine
system usage patterns.  It provides information on, for example, connections,
programs executed, and system resources used.")
    (license gpl3+)))
