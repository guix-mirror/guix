;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages moe)
  #:use-module (guix licenses)
  #:use-module (gnu packages ncurses)
  #:use-module ((gnu packages compression) #:select (lzip))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public moe
  (package
    (name "moe")
    (version "1.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/moe/moe-"
                          version ".tar.lz"))
      (sha256
       (base32
        "1cfwi67sdl2qchqbdib4p6wxjpwz2kmn6vxn9hmh1zs0gg4xkbwc"))))
    (build-system gnu-build-system)
    (native-inputs `(("lzip" ,lzip)))
    (inputs `(("ncurses" ,ncurses)))
    (home-page "https://www.gnu.org/software/moe/moe.html")
    (synopsis "Modeless, multiple-buffer, user-friendly 8-bit text editor")
    (description
     "GNU Moe is a powerful-but-simple-to-use text editor.  It works in a
modeless manner, and features an intuitive set of key-bindings that
assign a degree of severity to each key; for example, key
combinations with the Alt key are for harmless commands like cursor
movements while combinations with the Control key are for commands
that will modify the text.  Moe features multiple windows, unlimited
undo/redo, unlimited line length, global search and replace, and
more.")
    (license gpl3+)))
