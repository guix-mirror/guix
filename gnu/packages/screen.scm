;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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
    (version "4.2.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/screen/screen-"
                                 version ".tar.gz"))
             (sha256
              (base32 "105hp6qdd8rl71p81klmxiz4mlb60kh9r7czayrx40g38x858s2l"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses", ncurses)
       ("perl" ,perl)))
    (arguments
     `(#:configure-flags
       ;; By default, man and info pages are put in PREFIX/{man,info},
       ;; but we want them in PREFIX/share/{man,info}.
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "--mandir=" out "/share/man")
               (string-append "--infodir=" out "/share/info")))))
    (home-page "http://www.gnu.org/software/screen/")
    (synopsis "Full-screen window manager providing multiple terminals")
    (description
     "GNU Screen is a terminal window manager that multiplexes a single
terminal between several processes.  The virtual terminals each provide
features such as a scroll-back buffer and a copy-and-paste mechanism.  Screen
then manages the different virtual terminals, allowing you to easily switch
between them, to detach them from the current session, or even splitting the
view to show two terminals at once.")
    (license gpl2+)))
