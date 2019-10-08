;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
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

(define-module (gnu packages sqlite)
  #:use-module (gnu packages)
  #:use-module (gnu packages readline)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

;;; Commentary:
;;;
;;; This module has been separated from (gnu packages databases) to reduce the
;;; number of module references for core packages.

(define-public sqlite
  (package
   (name "sqlite")
   (version "3.28.0")
   (source (origin
            (method url-fetch)
            (uri (let ((numeric-version
                        (match (string-split version #\.)
                          ((first-digit other-digits ...)
                           (string-append first-digit
                                          (string-pad-right
                                           (string-concatenate
                                            (map (cut string-pad <> 2 #\0)
                                                 other-digits))
                                           6 #\0))))))
                   (string-append "https://sqlite.org/2019/sqlite-autoconf-"
                                  numeric-version ".tar.gz")))
            (sha256
             (base32
              "1hxpi45crbqp6lacl7z611lna02k956m9bsy2bjzrbb2y23546yn"))))
   (build-system gnu-build-system)
   (inputs `(("readline" ,readline)))
   (arguments
    `(#:configure-flags
      ;; Add -DSQLITE_SECURE_DELETE, -DSQLITE_ENABLE_FTS3,
      ;; -DSQLITE_ENABLE_UNLOCK_NOTIFY and -DSQLITE_ENABLE_DBSTAT_VTAB
      ;; to CFLAGS.  GNU Icecat will refuse to use the system SQLite
      ;; unless these options are enabled.
      (list (string-append "CFLAGS=-O2 -DSQLITE_SECURE_DELETE "
                           "-DSQLITE_ENABLE_FTS3 "
                           "-DSQLITE_ENABLE_UNLOCK_NOTIFY "
                           "-DSQLITE_ENABLE_DBSTAT_VTAB"))))
   (home-page "https://www.sqlite.org/")
   (synopsis "The SQLite database management system")
   (description
    "SQLite is a software library that implements a self-contained, serverless,
zero-configuration, transactional SQL database engine.  SQLite is the most
widely deployed SQL database engine in the world.  The source code for SQLite
is in the public domain.")
   (license license:public-domain)))

;; This is used by Qt.
(define-public sqlite-with-column-metadata
  (package/inherit sqlite
    (name "sqlite-with-column-metadata")
    (arguments
     (substitute-keyword-arguments (package-arguments sqlite)
       ((#:configure-flags flags)
        `(list (string-append "CFLAGS=-O2 -DSQLITE_SECURE_DELETE "
                              "-DSQLITE_ENABLE_UNLOCK_NOTIFY "
                              "-DSQLITE_ENABLE_DBSTAT_VTAB "
                              "-DSQLITE_ENABLE_COLUMN_METADATA")))))))
