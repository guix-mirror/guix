;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages libunwind)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public libunwind
  (package
    (name "libunwind")
    (version "1.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/libunwind/libunwind-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "16nhx2pahh9d62mvszc88q226q5lwjankij276fxwrm8wb50zzlx"))))
    (build-system gnu-build-system)
    (arguments
     ;; FIXME: As of glibc 2.17, we get 3 out of 34 test failures.
     ;; Report them upstream.
     '(#:tests? #f))
    (home-page "http://www.nongnu.org/libunwind")
    (synopsis
     "A portable and efficient API to determine the call-chain of a program")
    (description
     "The primary goal of this project is to define a portable and efficient C
programming interface (API) to determine the call-chain of a program.  The API
additionally provides the means to manipulate the preserved (callee-saved)
state of each call-frame and to resume execution at any point in the
call-chain (non-local goto).  The API supports both local (same-process) and
remote (across-process) operation.  As such, the API is useful in a number of
applications.")
    (license x11)))
