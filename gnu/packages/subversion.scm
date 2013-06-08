;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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

(define-module (gnu packages subversion)
  #:use-module ((guix licenses) #:select (asl2.0))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libapr)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite))

(define-public subversion
  (package
    (name "subversion")
    (version "1.7.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://archive.apache.org/dist/subversion/subversion-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "11inl9n1riahfnbk1fax0dysm2swakzhzhpmm2zvga6fikcx90zw"))))
    (build-system gnu-build-system)
    (inputs
      `(("libapr" ,libapr)
        ("libaprutil" ,libaprutil)
        ("perl" ,perl)
        ("python" ,python)
        ("sqlite" ,sqlite)
        ("zlib" ,zlib)))
    (home-page "http://subversion.apache.org/")
    (synopsis "Subversion, a revision control system")
    (description
     "Subversion exists to be universally recognized and adopted as an
open-source, centralized version control system characterized by its
reliability as a safe haven for valuable data; the simplicity of its model and
usage; and its ability to support the needs of a wide variety of users and
projects, from individuals to large-scale enterprise operations.")
    (license asl2.0)))
