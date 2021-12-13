;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Alexandru-Sergiu Marton <brown121407@member.fsf.org>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
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

(define-module (gnu packages compton)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public compton
  (let ((upstream-version "0.1_beta2"))
    (package
      (name "compton")
      (version (string-filter (char-set-complement (char-set #\_))
                              upstream-version))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/chjj/compton")
               (commit (string-append "v" upstream-version))))
         (sha256
          (base32
           "0v65viilhnd2xgvmdpzc1srxszcg8kj1vhi5gy9292j48w0s2fx1"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (inputs
       (list dbus
             libconfig
             libx11
             libxcomposite
             libxdamage
             libxext
             libxfixes
             libxinerama
             libxrandr
             libxrender
             mesa
             xprop
             xwininfo))
      (native-inputs
       (list asciidoc libdrm pkg-config python xorgproto))
      (arguments
       `(#:make-flags (list
                       "CC=gcc"
                       "NO_REGEX_PCRE=1"          ; pcre makes build fail
                       (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:tests? #f                              ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))
      (home-page "https://github.com/chjj/compton")
      (synopsis "Compositor for X11")
      (description
       "Compton is a compositor for the Xorg display server and a for of
xcompmgr-dana, which implements some changes like:

@itemize
@item OpenGL backend (@command{--backend glx}), in addition to the old X Render
backend.
@item Inactive window transparency (@command{-i}) and dimming
(@command{--inactive-dim}).
@item Menu transparency (@command{-m}, thanks to Dana).
@item Shadows are now enabled for argb windows, e.g terminals with transparency
@item Removed serverside shadows (and simple compositing) to clean the code,
the only option that remains is clientside shadows.
@item Configuration files (see the man page for more details).
@item Colored shadows (@command{--shadow-[red/green/blue]}).
@item A new fade system.
@item VSync support (not always working).
@item Blur of background of transparent windows, window color inversion (bad in
performance).
@item Some more options...
@end itemize\n")
      (license license:expat))))

(define-public picom
  (package
    (name "picom")
    (version "8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yshui/picom")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0gjksayz2xpmgglvw17ppsan2imrd1fijs579kbf27xwp503xgfl"))
       (file-name (string-append "picom-" version))))
    (build-system meson-build-system)
    (inputs
     (list dbus
           libconfig
           libx11
           libxext
           libev
           mesa
           xprop
           xcb-util-renderutil
           xcb-util-image
           pixman
           uthash
           libxdg-basedir
           pcre))
    (native-inputs
     (list asciidoc pkg-config xorgproto))
    (arguments
     `(#:build-type "release"
       #:configure-flags '("-Dwith_docs=true")))
    (home-page "https://github.com/yshui/picom")
    (synopsis "Compositor for X11, forked from Compton")
    (description
     "Picom is a standalone compositor for Xorg, suitable for use
with window managers that do not provide compositing.

Picom is a fork of compton, which is a fork of xcompmgr-dana,
which in turn is a fork of xcompmgr.")
    (license (list license:expat      ; The original compton license.
                   license:mpl2.0)))) ; License used by new picom files.
