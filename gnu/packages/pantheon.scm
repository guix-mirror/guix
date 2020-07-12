;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
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

(define-module (gnu packages pantheon)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system meson)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public granite
  (package
    (name "granite")
    (version "5.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/granite")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0acicv3f9gksb352v88lwap8ailjsxdrfknl2xql7blasbjzl2q0"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-icon-cache
           (lambda _
             (setenv "DESTDIR" "/")
             #t)))))
    (inputs
     `(("glib" ,glib)
       ("gtk" ,gtk+)
       ("libgee" ,libgee)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/granite")
    (synopsis "Library that extends GTK with common widgets and utilities")
    (description "Granite is a companion library for GTK+ and GLib.  Among other
things, it provides complex widgets and convenience functions designed for use
in apps built for the Pantheon desktop.")
    (license license:lgpl3+)))

(define-public pantheon-calculator
  (package
    (name "pantheon-calculator")
    (version "1.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/calculator")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1csxsr2c8qvl97xz9ahwn91z095nzgr0i1mbcb1spljll2sr9lkj"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-schema-cache-generation
           (lambda _
             (setenv "DESTDIR" "/")
             #t)))))
    (inputs
     `(("granite" ,granite)
       ("glib" ,glib)
       ("gtk" ,gtk+)
       ("libgee" ,libgee)))
    (native-inputs
     `(("cmake" ,cmake)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/calculator")
    (synopsis "Desktop calculator")
    (description "Calculator is an application for performing simple
arithmetic.  It is the default calculator application in the Pantheon
desktop.")
    (license license:gpl3)))

(define-public sideload
  (package
    (name "sideload")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/sideload")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mlc3nm2navzxm8k1rwpbw4w6mv30lmhqybm8jqxd4v8x7my73vq"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags (list (string-append "-Dflatpak="
                                              (assoc-ref %build-inputs "flatpak")
                                              "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'set-environment-variables
           (lambda _
             ;; Disable compiling schemas and updating desktop databases
             (setenv "DESTDIR" "/")
             #t))
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/io.elementary.sideload"))
                    (link (string-append out "/bin/sideload")))
               (symlink bin link)
               #t))))))
    (inputs
     `(("flatpak" ,flatpak)
       ("glib" ,glib)
       ("granite" ,granite)
       ("gtk" ,gtk+)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("libgee" ,libgee)
       ("libostree" ,libostree)
       ("libxml2" ,libxml2)))
    (propagated-inputs
     ;; Sideload needs these in the environment to fetch data securely from
     ;; Flatpak remotes.
     `(("gnupg" ,gnupg)
       ("gpgme" ,gpgme)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/sideload")
    (synopsis "Graphical application to side-load Flatpaks")
    (description "Sideload handles flatpakref files, like those you might find
on Flathub or another third-party website providing a Flatpak app for
download.")
    (license license:gpl3+)))
