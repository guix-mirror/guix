;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
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

(define-module (gnu packages upnp)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public miniupnpc
  (package
    (name "miniupnpc")
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://miniupnp.tuxfamily.org/files/miniupnpc-"
             version ".tar.gz"))
       (sha256
        (base32 "0r24jdqcyf839n30ppimdna0hvybscyziaad7ng99fw0x19y88r9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)))
    (arguments
     ;; The build system does not use a configure script but depends on
     ;; `make'.  Hence we should pass parameters to `make' instead and remove
     ;; the configure phase.
     '(#:make-flags
       (list
        (string-append
         "SH=" (assoc-ref %build-inputs "bash") "/bin/sh")
        (string-append "INSTALLPREFIX=" (assoc-ref %outputs "out"))
        "CC=gcc")
       #:phases
       (alist-delete 'configure %standard-phases)))
    (home-page "http://miniupnp.free.fr/")
    (synopsis "Library implementing the client side UPnP protocol")
    (description
     "MiniUPnPc is a library is useful whenever an application needs to listen
for incoming connections but is run behind a UPnP enabled router or firewall.
Examples for such applications include: P2P applications, FTP clients for
active mode, IRC (for DCC) or IM applications, network games, any server
software.")
    (license
     (x11-style "file://LICENSE" "See 'LICENSE' file in the distribution"))))
