;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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

(define-module (gnu packages screen)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl))

(define-public screen
  (package
    (name "screen")
    (version "4.0.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/screen/screen-"
                                 version ".tar.gz"))
             (sha256
              (base32 "0xvckv1ia5pjxk7fs4za6gz2njwmfd54sc464n8ab13096qxbw3q"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses", ncurses)
       ("perl" ,perl)))
    (home-page "http://www.gnu.org/software/screen/")
    (synopsis "Full-screen window manager providing multiple terminals")
    (description
     "Screen is a terminal window manager that multiplexes a single terminal
between several processes.  The virtual terminals each provide features such
as a scroll-back buffer and a copy-and-paste mechanism.  Screen then manages
the different virtual terminals, allowing you to easily switch between them,
to detach them from the current session, or even splitting the view to show
two terminals at once.")
    (license gpl2+)))
