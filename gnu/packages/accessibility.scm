;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Andrew Miloradovsky <andrew@interpretmath.pw>
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

(define-module (gnu packages accessibility)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb))

(define-public florence
  (package
    (name "florence")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/florence/florence/" version
                           "/florence-" version ".tar.bz2"))
       (sha256
        (base32
         "07h9qm22krlwayhzvc391lr23vicw81s48g7rirvx1fj0zyr4aa2"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list "--with-xtst"
                               "--without-docs"
                               "--with-notification")))
    (inputs
     `(("libxml2" ,libxml2)
       ("libglade" ,libglade)
       ("librsvg" ,librsvg)
       ("gstreamer" ,gstreamer)
       ("cairo" ,cairo)
       ("gtk+" ,gtk+)
       ("libxtst" ,libxtst)
       ("libxcomposite" ,libxcomposite)
       ("libnotify" ,libnotify)))
    (native-inputs
     `(("gettext-minimal" ,gettext-minimal)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://florence.sourceforge.net/")
    (synopsis "Extensible, scalable virtual keyboard for X11")
    (description
     "Florence is an extensible scalable virtual keyboard for X11.
It is useful for people who can't use a real hardware keyboard (for
example for people with disabilities), but you must be able to use
a pointing device (as a mouse, a trackball, a touchscreen or opengazer).

Florence stays out of your way when you don't need it: it appears on the
screen only when you need it.  A timer-based auto-click input method is
available to help to click.")
    ;; The documentation is under FDL1.2, but we do not install the
    ;; documentation.
    (license license:gpl2+)))

(define-public footswitch
  (let ((commit "ca43d53fc2002520cc825d119702afc124303e73")
        (revision "2"))
    (package
      (name "footswitch")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rgerganov/footswitch")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "14pyzc4ws1mj859xs9n4x83wzxxvd3bh5bdxzr6nv267xwx1mq68"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (inputs
       `(("hidapi" ,hidapi)))
      (arguments
       `(#:tests? #f ; no tests
         #:make-flags (list "CC=gcc")
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    ;; Install target in the Makefile does not work for Guix
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let ((bin (string-append (assoc-ref outputs "out")
                                                  "/bin")))
                          (install-file "footswitch" bin)
                          (install-file "scythe" bin)
                          #t))))))
      (home-page "https://github.com/rgerganov/footswitch")
      (synopsis "Command line utility for PCsensor foot switch")
      (description
       "Command line utility for programming foot switches sold by PCsensor.
It works for both single pedal devices and three pedal devices.  All supported
devices have vendorId:productId = 0c45:7403 or 0c45:7404.")
    (license license:expat))))

(define-public xmagnify
  (package
    (name "xmagnify")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/amiloradovsky/magnify.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ngnp5f5zl3v35vhbdyjpymy6mwrs0476fm5nd7dzkba7n841jdh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; none included
       #:make-flags
       (list "CC=gcc"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("libX11" ,libx11)))
    (home-page "https://gitlab.com/amiloradovsky/magnify")
    (synopsis "Tiny screen magnifier for X11")
    (description
     "This program magnifies a screen region by an integer positive factor and
draws the result on a window.  It is useful as an accessibility tool, which
works with every X Window System based GUI (depends only on libX11); or as an
assistant for graphic designers, who need to select individual pixels.")
    ;; Licensed either under Expat or GPLv2+.
    (license (list license:expat license:gpl2+))))
