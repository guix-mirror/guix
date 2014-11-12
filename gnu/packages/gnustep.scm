;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages gnustep)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config))

(define-public windowmaker
  (package
    (name "windowmaker")
    (version "0.95.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://windowmaker.org/pub/source/release/WindowMaker-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1i3dw1yagsa3rs9x2na2ynqlgmbahayws0kz4vl00fla6550nns3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'pre-configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; 'wmaker' wants to invoke 'wmaker.inst' the first time,
                   ;; and the 'wmsetbg', so make sure it uses the right ones.
                   ;; We can't use a wrapper here because that would pollute
                   ;; $PATH in the whole session.
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin")))
                     (substitute* "src/main.c"
                       (("\"wmaker\\.inst")
                        (string-append "\"" bin "/wmaker.inst")))
                     (substitute* '("src/defaults.c" "WPrefs.app/Menu.c")
                       (("\"wmsetbg")
                        (string-append "\"" bin "/wmsetbg")))))
                 (alist-cons-after
                  'install 'wrap
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin")))
                      ;; In turn, 'wmaker.inst' wants to invoke 'wmmenugen'
                      ;; etc., so make sure everything is in $PATH.
                      (wrap-program (string-append bin "/wmaker.inst")
                                    `("PATH" ":" prefix (,bin)))))
                  %standard-phases))))
    (inputs
     `(("libxmu" ,libxmu)
       ("libxft" ,libxft)
       ("libx11" ,libx11)
       ("fontconfig" ,fontconfig)
       ("libjpeg" ,libjpeg)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://windowmaker.org/")
    (synopsis "NeXTSTEP-like window manager")
    (description
     "Window Maker is an X11 window manager originally designed to provide
integration support for the GNUstep Desktop Environment.  In every way
possible, it reproduces the elegant look and feel of the NeXTSTEP user
interface.  It is fast, feature rich, easy to configure, and easy to use.")

    ;; Artwork is distributed under the WTFPL.
    (license gpl2+)))
