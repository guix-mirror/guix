;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Leo Famulari <leo@famulari.name>
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

(define-module (gnu packages dvtm)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages ncurses))

(define-public dvtm
  (package
   (name "dvtm")
   (version "0.15")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.brain-dump.org/projects/dvtm/dvtm-"
                                 version ".tar.gz"))
            (sha256
             (base32
              "0475w514b7i3gxk6khy8pfj2gx9l7lv2pwacmq92zn1abv01a84g"))))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags (list "CC=gcc"
                         (string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases (modify-phases %standard-phases
                 (delete 'configure)
                 (delete 'check))))  ; no test suite
   (inputs (list ncurses))
   (synopsis "Tiling window management for the console")
   (description "dvtm brings the concept of tiling window management,
popularized by X11-window managers like dwm, to the console.  As a console
window manager it tries to make it easy to work with multiple console based
programs.")
   (home-page "https://www.brain-dump.org/projects/dvtm/")
   ;; "dvtm reuses some code of dwm and is released under the same MIT/X11
   ;; license. The terminal emulation part is licensed under the ISC license."
   ;; source: http://www.brain-dump.org/projects/dvtm/#license
   (license (list expat isc))))
