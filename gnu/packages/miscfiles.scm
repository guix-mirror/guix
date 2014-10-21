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
    (synopsis "Collection of miscellaneous text files")
    (description "Miscfiles is a collection of files not of crucial importance
for system administration or operation, but which have come to be common on
various systems over the years. It includes data files for country codes,
airport codes, currency information, and so on.")
    (license gpl2+)))
