;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 ng0 <ng0@n0.is>
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

(define-module (gnu packages fvwm)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public fvwm
  (package
    (name "fvwm")
    (version "2.6.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/fvwmorg/fvwm/releases/download/"
                    version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hgkkdzcqjnaabvv9cnh0bz90nnjskbhjg9qnzpi2x0mbliwjdpv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-xsession
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions")))
               (mkdir-p xsessions)
               (with-output-to-file
                   (string-append xsessions "/fvwm2.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                    Name=FVWM~@
                    Comment=FVWM~@
                    Exec=~a/bin/fvwm~@
                    TryExec=~@*~a/bin/fvwm~@
                    Icon=~@
                    Type=Application~%"
                           out))))
             #t)))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("fribidi" ,fribidi)
       ("libpng" ,libpng)
       ("librsvg" ,librsvg)
       ("libxcursor" ,libxcursor)
       ("libxext" ,libxext)
       ("libxft" ,libxft)
       ("libxinerama" ,libxinerama)
       ("libxpm" ,libxpm)
       ("libxt" ,libxt)
       ("readline" ,readline)))
    (synopsis "Virtual window manager for X11")
    (description
     "FVWM is an extremely powerful ICCCM-compliant multiple virtual desktop
window manager for the X Window system.")
    (home-page "http://www.fvwm.org/")
    (license gpl2+)))
