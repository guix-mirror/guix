;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
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

(define-module (gnu packages visidata)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public visidata
  (package
    (name "visidata")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "visidata" version))
       (sha256
        (base32
         "10adfyn4gkisvciqawgh2lakkhhnjjxiyp7mzbgcwkq1b3sigpf1"))))
    (build-system python-build-system)
    ;; Tests disabled because they are not packaged with the source tarball.
    ;; Upstream suggests tests will be packaged with tarball around 2.0 release.
    (arguments '(#:tests? #f))
    (inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-fonttools" ,python-fonttools)
       ("python-h5py" ,python-h5py)
       ("python-lxml" ,python-lxml)
       ("python-openpyxl" ,python-openpyxl)
       ("python-pandas" ,python-pandas)
       ("python-psycopg2" ,python-psycopg2)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)
       ("python-xlrd" ,python-xlrd)))
    (synopsis "Terminal spreadsheet multitool for discovering and arranging data")
    (description
     "VisiData is an interactive multitool for tabular data.  It combines the
clarity of a spreadsheet, the efficiency of the terminal, and the power of
Python, into a lightweight utility which can handle millions of rows.")
    (home-page "https://www.visidata.org/")
    (license (list license:gpl3
                   license:expat)))) ;; visidata/vdtui.py
