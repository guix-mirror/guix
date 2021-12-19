;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
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

(define-module (gnu packages ccache)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression))

(define-public ccache
  (package
    (name "ccache")
    (version "4.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ccache/ccache/releases/download/v"
                           version "/ccache-" version ".tar.xz"))
       (sha256
        (base32 "05wmflxdc8h3d00gr3kilr5dmrqxj6lcmq9ic575ydi60fz6w62i"))))
    (build-system cmake-build-system)
    (native-inputs (list perl ; for test/run
                         (@ (gnu packages base) which)))
    (inputs (list zlib
                  `(,zstd "lib")))
    (arguments
     (list #:configure-flags
           ;; The backend must be explicitly disabled to build without Redis.
           #~(list "-DREDIS_STORAGE_BACKEND=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'fix-shell
                 ;; Run early whilst we're still in the source directory.
                 (lambda _
                   (substitute* (list "test/run"
                                      "test/suites/base.bash"
                                      "unittest/test_hashutil.cpp")
                     (("compgen -e") "env | cut -d= -f1")
                     (("#!/bin/sh") (string-append "#!" (which "sh"))))))
               (add-before 'check 'set-home
                 ;; Tests require a writable HOME.
                 (lambda _
                   (setenv "HOME" (getenv "TMPDIR")))))))
    (home-page "https://ccache.dev/")
    (synopsis "Compiler cache")
    (description
     "Ccache is a compiler cache.  It speeds up recompilation by caching
previous compilations and detecting when the same compilation is being done
again.  Supported languages are C, C++, Objective-C and Objective-C++.")
    (license gpl3+)))
