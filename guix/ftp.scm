;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <ftp://www.gnu.org/licenses/>.

(define-module (guix ftp)
  #:use-module (ice-9 match)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module ((guix store) #:select (derivation-path?))
  #:use-module (guix utils)
  #:export (ftp-fetch))

;;; Commentary:
;;;
;;; Produce fixed-output derivations with data fetched over FTP.
;;;
;;; Code:

(define* (ftp-fetch store url hash-algo hash
                    #:optional name
                    #:key (system (%current-system)) guile)
  "Return the path of a fixed-output derivation in STORE that fetches URL,
which is expected to have hash HASH of type HASH-ALGO (a symbol).  By
default, the file name is the base name of URL; optionally, NAME can specify
a different file name."
  (define builder
    `(begin
       (use-modules (guix build ftp))
       (ftp-fetch ,url %output)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system))
      ((and (? string?) (? derivation-path?))
       guile)
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(distro packages base)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system)))))

  (build-expression->derivation store (or name (basename url)) system
                                builder '()
                                #:hash-algo hash-algo
                                #:hash hash
                                #:modules '((guix ftp-client)
                                            (guix build ftp)
                                            (guix build utils))
                                #:guile-for-build guile-for-build))

;;; ftp.scm ends here
