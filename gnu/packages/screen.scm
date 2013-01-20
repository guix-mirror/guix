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
             (uri (string-append "http://ftp.gnu.org/gnu/screen/screen-"
                                 version ".tar.gz"))
             (sha256
              (base32 "0xvckv1ia5pjxk7fs4za6gz2njwmfd54sc464n8ab13096qxbw3q"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses", ncurses)
       ("perl" ,perl)))
    (home-page "http://www.gnu.org/software/screen/")
    (synopsis "GNU Screen, a terminal multiplexer")
    (description
     "GNU screen is a full-screen window manager that multiplexes a physical
terminal between several processes, typically interactive shells. Each virtual
terminal provides the functions of the DEC VT100 terminal and, in addition,
several control functions from the ANSI X3.64 (ISO 6429) and ISO 2022 standards
(e.g., insert/delete line and support for multiple character sets). There is a
scrollback history buffer for each virtual terminal and a copy-and-paste
mechanism that allows the user to move text regions between windows. When
screen is called, it creates a single window with a shell in it (or the
specified command) and then gets out of your way so that you can use the
program as you normally would. Then, at any time, you can create new
(full-screen) windows with other programs in them (including more shells), kill
the current window, view a list of the active windows, turn output logging on
and off, copy text between windows, view the scrollback history, switch between
windows, etc. All windows run their programs completely independent of each
other. Programs continue to run when their window is currently not visible and
even when the whole screen session is detached from the users terminal.")
    (license gpl2+)))
