;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages terminals)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages zip))

(define-public tilda
  (package
    (name "tilda")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/lanoxx/tilda/archive/"
                                  "tilda-" version ".zip"))
              (sha256
               (base32
                "06r7q0b1xjclagsnw0ilxhw5az5ddly2f7xc1lcwlrf337x0jrar"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                 (add-before 'patch-source-shebangs 'autogen
                  (lambda _       ; Avoid running ./configure.
                   (substitute* "autogen.sh"
                                (("^.*\\$srcdir/configure.*") ""))
                   (zero? (system* "sh" "autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gnu-gettext)
       ("pkg-config" ,pkg-config)
       ("unzip" ,unzip)))
    (inputs
     `(("glib" ,glib "bin")
       ("gtk+" ,gtk+)
       ("libconfuse" ,libconfuse)
       ("vte" ,vte-0.36))) ; TODO: try to use vte-0.38+ in 1.3
                           ; https://github.com/lanoxx/tilda/issues/94
    (synopsis "GTK+-based drop-down terminal")
    (description "Tilda is a terminal emulator similar to normal terminals like
gnome-terminal (GNOME) or Konsole (KDE), with the difference that it drops down
from the edge of a screen when a certain configurable hotkey is pressed.  This
is similar to the built-in consoles in some applications.  Tilda is highly
configureable through a graphical wizard.")
    (home-page "https://github.com/lanoxx/tilda")
    (license license:gpl2+)))
