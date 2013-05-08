;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages bazaar)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix build utils)
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'guix:)))

(define-public bazaar
  (package
    (name "bazaar")
    (version "2.5.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://launchpad.net/bzr/2.5/" version
                          "/+download/bzr-" version ".tar.gz"))
      (sha256
       (base32
        "10krjbzia2avn09p0cdlbx2wya0r5v11w5ymvyl72af5dkx4cwwn"))))
    (build-system python-build-system)
    (inputs
     ;; Note: 'tools/packaging/lp-upload-release' and 'tools/weavemerge.sh'
     ;; require Zsh.
     `(("gettext" ,guix:gettext)))
    (arguments
     `(#:tests? #f)) ; no test target
    (home-page "https://gnu.org/software/bazaar")
    (synopsis "Distributed and central version control system")
    (description
     "GNU Bazaar is a distributed version control system, which supports both
central version control and distributed version control.  Developers can
organize their workspace in whichever way they want.  It is possible to work
from a command line or use a GUI application.")
    (license gpl2+)))