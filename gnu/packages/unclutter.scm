;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
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

(define-module (gnu packages unclutter)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages xorg))

(define-public unclutter
  (package
   (name "unclutter")
   (version "8")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://ftp.x.org/contrib/utilities/unclutter-"
                  version ".tar.Z"))
            (sha256
             (base32
              "0ahrr5z6wxqqfyihm112hnq0859zlxisrb3y5232zav58j6sfmdq"))))
   (build-system gnu-build-system)
   (arguments
    '(#:tests? #f  ; no check target
      #:phases (alist-delete
                'configure
                (alist-replace
                 'install
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out  (assoc-ref outputs "out"))
                          (bin  (string-append out "/bin"))
                          (man1 (string-append out "/share/man/man1")))
                     (mkdir-p bin)
                     (mkdir-p man1)
                     (zero?
                      (system* "make" "install" "install.man"
                               (string-append "BINDIR=" bin)
                               (string-append "MANDIR=" man1)))))
                 %standard-phases))))
   (inputs `(("libx11" ,libx11)))
   (home-page "http://ftp.x.org/contrib/utilities/")
   (synopsis "Hide idle mouse cursor")
   (description
    "Unclutter is a program which runs permanently in the background of an
X11 session.  It checks on the X11 pointer (cursor) position every few
seconds, and when it finds it has not moved (and no buttons are pressed
on the mouse, and the cursor is not in the root window) it creates a
small sub-window as a child of the window the cursor is in.  The new
window installs a cursor of size 1x1 but a mask of all 0, i.e. an
invisible cursor.  This allows you to see all the text in an xterm or
xedit, for example.  The human factors crowd would agree it should make
things less distracting.")
   (license public-domain)))
