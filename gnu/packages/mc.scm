;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 ng0 <ng0@infotropique.org>
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

(define-module (gnu packages mc)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages check)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression))

(define-public mc
  (package
    (name "mc")
    (version "4.8.16")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://ftp.midnight-commander.org/mc-"
                          version ".tar.xz"))
      (sha256
       (base32
        "083h5gwc0nny9b557way5dsmj71g2bzkiai4bn30y5nkjwqbxg5v"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("perl" ,perl)))
    (inputs `(("aspell" ,aspell)
              ("ncurses" ,ncurses)
              ("libssh2" ,libssh2)
              ("glib" ,glib)
              ("check" ,check)
              ("unzip" ,unzip)))
    (arguments
     `(#:configure-flags
       '("--with-screen=ncurses" "--enable-aspell")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'fix-absolutism
           (lambda _
             ;; Modify files that contain absolute file names.
             (substitute* "misc/mcedit.menu.in"
               (("#! /bin/sh") (string-append "#!" (which "sh")))
               (("/bin/bash") (which "bash")))
             (substitute* "misc/ext.d/misc.sh.in"
               (("/bin/cat") "cat"))
             #t)))))
    (home-page "http://www.midnight-commander.org")
    (synopsis "Graphical file manager")
    (description
     "GNU Midnight Commander is a command-line file manager laid out in a
common two-pane format.  In addition to standard file management tasks such as
copying and moving, Midnight Commander also supports viewing the contents of
RPM package files and other archives and managing files on other computers via
FTP or FISH.  It also includes a powerful text editor for opening text
files.")
    (license gpl3+)))
