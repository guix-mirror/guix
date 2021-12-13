;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages gxmessage)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages))

(define-public gxmessage
  (package
    (name "gxmessage")
    (version "3.4.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gxmessage/gxmessage-"
                                 version ".tar.gz"))
             (sha256
              (base32 "01ggvvp0nyn8xczh93icifnji468wsjgqzd1f1bixwsqziaicknv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "Makefile"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (inputs
     (list gtk+))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://www.gnu.org/software/gxmessage/")
    (synopsis "Open popup message window with buttons for return")
    (description "GNU gxmessage is a program that pops up dialog windows, which display
a message to the user and waits for their action.  The program then exits
with an exit code corresponding to the response.")
    (license gpl3+)))
