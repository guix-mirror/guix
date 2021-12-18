;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages microcom)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages readline)
  #:use-module (guix build-system gnu))

(define-public microcom
  (package
    (name "microcom")
    (version "2019.01.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.pengutronix.de/git/tools/microcom.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0rf4qdkbhknrrqn4rpb737l0km8yn5f498zapks4akf6yjh5aq8y"))))
    (build-system gnu-build-system)
    (inputs (list readline))
    (native-inputs (list automake autoconf))
    (home-page  "https://git.pengutronix.de/?p=tools/microcom.git")
    (synopsis "Minimalistic serial line terminal program")
 (description "Microcom is a minimalistic terminal program for accessing
devices via a serial connection.  It features connection via RS232 serial
interfaces (including setting of transfer rates) as well as in @code{telnetmode}
as specified in rfc2217 and a (Linux specific) CAN mode.")
    (license gpl2+)))
