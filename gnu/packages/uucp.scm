;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages uucp)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public uucp
  (package
    (name "uucp")
    (version "1.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/uucp/uucp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0b5nhl9vvif1w3wdipjsk8ckw49jj1w85xw1mmqi3zbcpazia306"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; The old 'configure' script doesn't support the arguments
                   ;; that we pass by default.
                   (setenv "CONFIG_SHELL" (which "sh"))
                   (let ((out (assoc-ref outputs "out")))
                     (zero? (system* "./configure"
                                     (string-append "--prefix=" out)
                                     (string-append "--infodir=" out
                                                    "/share/info")))))
                 %standard-phases)))
    (home-page "http://www.gnu.org/software/uucp/uucp.html")
    (synopsis "UUCP protocol implementation")
    (description
     "Taylor UUCP is the GNU implementation of UUCP (Unix-to-Unix Copy), a
set of utilities for remotely transferring files, email and net news
between computers.")
    (license gpl2+)))
