;;; GNU Guix --- Functional package management for GNU
;;; Copyright 2014 John Darrington <jmd@gnu.org>
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

(define-module (gnu packages miscfiles)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public miscfiles
  (package
    (name "miscfiles")
    (version "1.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/" name "/" name "-"
                          version ".tar.gz"))
      (sha256
       (base32
        "005588vfrwx8ghsdv9p7zczj9lbc9a3r4m5aphcaqv8gif4siaka"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/miscfiles/")
    (synopsis
     "Data files for airport codes, zip codes, a dictionary, and more")
    (description
     "GNU Miscfiles is a collection of common data files.  They include, for
example, country abbreviations, names and capital cities; currency
abbreviations and names; a Best Current Practices index; a map of the ASCII
character set; a list of three-letter airport codes; and an English word
list.")
    (license gpl2+)))
