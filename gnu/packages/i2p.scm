;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages i2p)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public i2pd
  (package
    (name "i2pd")
    (version "2.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PurpleI2P/i2pd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gx7y0vrg9lsl7m1r6c8xyyqmaqn900kms4g0941g0gd5zdb0mvz"))))
    (build-system cmake-build-system)
    (inputs `(("boost" ,boost)
              ("miniupnpc" ,miniupnpc)
              ("openssl" ,openssl)
              ("zlib" ,zlib)))
    (arguments '(#:configure-flags
                 (let ((source (assoc-ref %build-inputs "source")))
                   (list (string-append "-S" source "/build")
                         "-DWITH_PCH=OFF"
                         "-DWITH_STATIC=OFF"
                         "-DWITH_UPNP=ON"
                         "-DWITH_LIBRARY=ON"
                         "-DBUILD_SHARED_LIBS=ON"
                         "-DWITH_BINARY=ON"))
                 #:phases
                 (modify-phases %standard-phases
                   (replace 'check
                     (lambda* (#:key
                               (make-flags '())
                               (parallel-tests? #t)
                               #:allow-other-keys)
                       (let ((source (assoc-ref %build-inputs "source")))
                         (copy-recursively (string-append source "/tests")
                                           "./tests")
                         (with-directory-excursion "tests"
                           (substitute* "Makefile"
                             (("../libi2pd/") (string-append source "/libi2pd/")))
                           (apply invoke "make" "all"
                                  `(,@(if parallel-tests?
                                          `("-j" ,(number->string
                                                    (parallel-job-count)))
                                          '())
                                    ,@make-flags)))))))))
    (home-page "https://i2pd.website/")
    (synopsis "Router for an end-to-end encrypted and anonymous internet")
    (description "i2pd is a client for the anonymous I2P network, upon which
applications for file sharing, web browsing, instant messaging, and more are
built. i2pd allows people from all around the world to communicate and share
information securely without restrictions.")
    (license license:bsd-3)))
