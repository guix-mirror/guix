;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages gkrellm)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))

(define-public gkrellm
  (package
    (name "gkrellm")
    (version "2.3.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://members.dslextreme.com/users/billw/gkrellm/gkrellm-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "12rc6zaa7kb60b9744lbrlfkxxfniprm6x0mispv63h4kh75navh"))))
    (build-system gnu-build-system)
    (inputs
     `(("gettext" ,gnu-gettext)
       ("gtk+" ,gtk+-2)
       ("libice" ,libice)
       ("libsm" ,libsm)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:tests? #f ; there is no check target
       #:phases
       (alist-delete
        'configure
        %standard-phases)
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "INSTALLROOT=" out)
               "CC=gcc"
               "X11_LIBS = -lX11 -lSM -lICE -lgmodule-2.0"))))
    (home-page "http://members.dslextreme.com/users/billw/gkrellm/gkrellm.html")
    (synopsis "System monitors")
    (description
     "GKrellM is a single process stack of system monitors which supports
applying themes to match its appearance to your window manager, Gtk, or any
other theme.")
    (license license:gpl3+)))
