;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu packages ratpoison)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (gnu packages)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages fontutils))

(define ratpoison.desktop
  (origin
    (method url-fetch)
    (uri (string-append "https://sources.gentoo.org/cgi-bin/viewvc.cgi/"
                        "gentoo-x86/x11-wm/ratpoison/files/ratpoison.desktop"
                        "?revision=1.1"))
    (file-name "ratpoison.desktop")
    (sha256
     (base32
      "1rh3f4c3rhn6q2hmkraam0831xqcqyj3qkqf019ahaxsxaan3553"))))

(define-public ratpoison
  (package
    (name "ratpoison")
    (version "1.4.9")
    (source
     (origin (method url-fetch)
             (uri (string-append "mirror://savannah/ratpoison/ratpoison-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1wfir1gvh5h7izgvx2kd1pr2k7wlncd33zq7qi9s9k2y0aza93yr"))
             (patches (search-patches "ratpoison-shell.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-xsession
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((rpd "ratpoison.desktop")
                            (dst (string-append (assoc-ref outputs "out")
                                                "/share/xsessions/")))
                        (mkdir-p dst)
                        (copy-file (assoc-ref inputs rpd)
                                   (string-append dst rpd))))))))
    (inputs
     `(("inputproto" ,inputproto)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libXft" ,libxft)
       ("libXi" ,libxi)
       ("libxrandr" ,libxrandr)
       ("libXpm" ,libxpm)
       ("libXt" ,libxt)
       ("libXtst" ,libxtst)
       ("libX11" ,libx11)
       ("readline" ,readline)
       ("xextproto" ,xextproto)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("ratpoison.desktop" ,ratpoison.desktop)))
    (home-page "https://www.nongnu.org/ratpoison/")
    (synopsis "Simple mouse-free tiling window manager")
    (description
     "Ratpoison is a simple window manager with no fat library
dependencies, no fancy graphics, no window decorations, and no
rodent dependence.  It is largely modelled after GNU Screen which
has done wonders in the virtual terminal market.

The screen can be split into non-overlapping frames.  All windows
are kept maximized inside their frames to take full advantage of
your precious screen real estate.

All interaction with the window manager is done through keystrokes.
Ratpoison has a prefix map to minimize the key clobbering that
cripples Emacs and other quality pieces of software.")
    (license gpl2+)))
