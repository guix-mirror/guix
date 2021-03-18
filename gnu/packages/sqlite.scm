;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (gnu packages hurd)
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
   (version "3.31.1")
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
                   (string-append "https://sqlite.org/2020/sqlite-autoconf-"
                                  numeric-version ".tar.gz")))
            (sha256
             (base32
              "1bj936svd8i5g25xd1bj52hj4zca01fgl3sqkj86z9q5pkz4wa32"))))
   (replacement sqlite/fixed)
   (build-system gnu-build-system)
   (inputs `(("readline" ,readline)))
   (native-inputs (if (hurd-target?)
                      ;; TODO move into origin on the next rebuild cycle.
                      `(("hurd-locking-mode.patch"
                         ,@(search-patches "sqlite-hurd.patch")))
                      '()))
   (outputs '("out" "static"))
   (arguments
    `(#:configure-flags
      ;; Add -DSQLITE_SECURE_DELETE, -DSQLITE_ENABLE_FTS3,
      ;; -DSQLITE_ENABLE_UNLOCK_NOTIFY and -DSQLITE_ENABLE_DBSTAT_VTAB
      ;; to CFLAGS.  GNU Icecat will refuse to use the system SQLite
      ;; unless these options are enabled.
      (list (string-append "CFLAGS=-O2 -DSQLITE_SECURE_DELETE "
                           "-DSQLITE_ENABLE_FTS3 "
                           "-DSQLITE_ENABLE_UNLOCK_NOTIFY "
                           "-DSQLITE_ENABLE_DBSTAT_VTAB "
                           ;; Column metadata is required by GNU Jami and Qt, et.al.
                           "-DSQLITE_ENABLE_COLUMN_METADATA"))
      #:phases (modify-phases %standard-phases
                 ;; TODO: remove in the next rebuild cycle
                 ,@(if (hurd-target?)
                       `((add-after 'unpack 'patch-sqlite/hurd
                           (lambda* (#:key inputs native-inputs
                                     #:allow-other-keys)
                             (let ((patch (assoc-ref
                                           (if ,(%current-target-system)
                                               native-inputs
                                               inputs)
                                           "hurd-locking-mode.patch")))
                               (invoke "patch" "-p1" "--force" "-i" patch)))))
                       '())
                 (add-after 'install 'move-static-library
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out    (assoc-ref outputs "out"))
                            (static (assoc-ref outputs "static"))
                            (source (string-append out "/lib/libsqlite3.a")))
                       (mkdir-p (string-append static "/lib"))
                       (link source (string-append static "/lib/libsqlite3.a"))
                       (delete-file source)

                       ;; Remove reference to the static library from the .la file
                       ;; so that Libtool looks for it in the usual places.
                       (substitute* (string-append out "/lib/libsqlite3.la")
                         (("^old_library=.*")
                          "old_library=''\n"))
                       #t))))))
   (home-page "https://www.sqlite.org/")
   (synopsis "The SQLite database management system")
   (description
    "SQLite is a software library that implements a self-contained, serverless,
zero-configuration, transactional SQL database engine.  SQLite is the most
widely deployed SQL database engine in the world.  The source code for SQLite
is in the public domain.")
   (license license:public-domain)))

(define-public sqlite/fixed
  (package
    (inherit sqlite)
    (version "3.32.3")
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
                     (string-append "https://sqlite.org/2020/sqlite-autoconf-"
                                    numeric-version ".tar.gz")))
              (sha256
               (base32
                "0rlbaq177gcgk5dswd3akbhv2nvvzljrbhgy18hklbhw7h90f5d3"))))))

;; Column metadata support was added to the regular 'sqlite' package with
;; commit fad5b1a6d8d9c36bea5785ae4fbc1beb37e644d7.
(define-public sqlite-with-column-metadata
  (deprecated-package "sqlite-with-column-metadata" sqlite))
