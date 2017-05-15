;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages sssd)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages))

(define-public ding-libs
  (package
    (name "ding-libs")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.pagure.org/SSSD/ding-libs/"
                                  "ding-libs-" version ".tar.gz"))
              (sha256
               (base32
                "1bczkvq7cblp75kqn6r2d7j5x7brfw6wxirzc6d2rkyb80gj2jkn"))))
    (build-system gnu-build-system)
    (home-page "https://pagure.io/SSSD/ding-libs/")
    (synopsis "Libraries for SSSD")
    (description
     "DING-LIBS (DING Is Not Glib) are a set of small, useful libraries that
the @dfn{System Security Services Daemon} (SSSD) uses and makes available to
other projects.  They include: libdhash, an implementation of a dynamic hash
table which will dynamically resize to achieve optimal storage and access time
properties; ini_config, a library for parsing and managing @code{INI} files;
path_utils, a library to manage UNIX paths and subsets of paths; collection, a
generic, hierarchical grouping mechanism for complex data sets; ref_array, a
dynamically-growing, reference-counted array; libbasicobjects, a set of
fundamental object types for C.")
    (license license:lgpl3+)))
