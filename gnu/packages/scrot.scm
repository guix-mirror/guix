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

(define-module (gnu packages scrot)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages image))

(define-public scrot
  (package
    (name "scrot")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://linuxbrit.co.uk/downloads/scrot-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1wll744rhb49lvr2zs6m93rdmiq59zm344jzqvijrdn24ksiqgb1"))))
    (build-system gnu-build-system)
    (arguments
     ;; By default, man and doc are put in PREFIX/{man,doc} instead of
     ;; PREFIX/share/{man,doc}.
     '(#:configure-flags
       (list (string-append "--mandir="
                            (assoc-ref %outputs "out")
                            "/share/man"))
       #:phases (alist-replace
                 'install
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (doc (string-append out "/share/doc/scrot")))
                     (mkdir-p doc)
                     (zero?
                      (system* "make" "install"
                               (string-append "docsdir=" doc)))))
                 %standard-phases)))
    (inputs
     `(("libx11" ,libx11)
       ("giblib" ,giblib)))
    (home-page "http://linuxbrit.co.uk/software/")
    (synopsis "Command-line screen capture utility for X Window System")
    (description
     "scrot allows to save a screenshot of a full screen, a window or a part
of the screen selected by mouse.")
    ;; This license removes a clause about X Consortium from the original
    ;; X11 license.
    (license (x11-style "file://COPYING"
                        "See 'COPYING' in the distribution."))))
