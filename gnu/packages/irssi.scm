;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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

(define-module (gnu packages irssi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls))

(define-public irssi
  (package
    (name "irssi")
    (version "0.8.17")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.irssi.org/files/irssi-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "01v82q2pfiimx6lh271kdvgp8hl4pahc3srg04fqzxgdsb5015iw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-replace
        'configure
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out"))
                (ncurses (assoc-ref inputs "ncurses")))
            (setenv "CONFIG_SHELL" (which "bash"))
            (zero?
             (system* "./configure"
                      (string-append "--prefix=" out)
                      (string-append "--with-ncurses=" ncurses)))))
        %standard-phases)))
    (inputs
     `(("glib" ,glib)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)))
    (home-page "http://www.irssi.org/")
    (synopsis "Terminal-based IRC client")
    (description
     "Irssi is a terminal based IRC client for UNIX systems.  It also supports
SILC and ICB protocols via plugins.")
    (license license:gpl2+)))
