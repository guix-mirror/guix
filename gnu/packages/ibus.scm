;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages ibus)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public ibus
  (package
   (name "ibus")
   (version "1.5.11")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/ibus/ibus/"
                                 "releases/download/"
                                 version "/ibus-" version ".tar.gz"))
             (sha256
              (base32
               "1g26llizd26h9sfz4xdq8krhz19hn08pirvfbkk3g89ri8lmm6a9"))))
   (build-system glib-or-gtk-build-system)
   (arguments
    `(#:tests? #f  ; tests fail because there's no connection to dbus
      #:make-flags
      (list "CC=gcc"
            (string-append "pyoverridesdir="
                           (assoc-ref %outputs "out")
                           "/lib/python2.7/site-packages/gi/overrides/")
            (string-append "py2overridesdir="
                           (assoc-ref %outputs "out")
                           "/lib/python2.7/site-packages/gi/overrides/"))
      #:phases
      (alist-cons-before
       'configure 'disable-dconf-update
       (lambda _
         (substitute* "data/dconf/Makefile.in"
           (("dconf update") "echo dconf update"))
         #t)
       (alist-cons-after
        'wrap-program 'wrap-with-additional-paths
        (lambda* (#:key outputs #:allow-other-keys)
          ;; Make sure 'ibus-setup' runs with the correct PYTHONPATH and
          ;; GI_TYPELIB_PATH.
          (let ((out (assoc-ref outputs "out")))
            (wrap-program (string-append out "/bin/ibus-setup")
              `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))
              `("GI_TYPELIB_PATH" ":" prefix
                (,(getenv "GI_TYPELIB_PATH")
                 ,(string-append out "/lib/girepository-1.0"))))))
        %standard-phases))))
   (inputs
    `(("dbus" ,dbus)
      ("dconf" ,dconf)
      ("gconf" ,gconf)
      ("gtk2" ,gtk+-2)
      ("intltool" ,intltool)
      ("libnotify" ,libnotify)
      ("iso-codes" ,iso-codes)
      ("pygobject2" ,python2-pygobject)
      ("python2" ,python-2)))
   (native-inputs
    `(("glib" ,glib "bin") ; for glib-genmarshal
      ("gobject-introspection" ,gobject-introspection) ; for g-ir-compiler
      ("pkg-config" ,pkg-config)))
   (native-search-paths
    (list (search-path-specification
           (variable "IBUS_COMPONENT_PATH")
           (files '("share/ibus/component")))))
   (synopsis "Input method framework")
   (description
    "IBus is an input framework providing a full-featured and user-friendly
input method user interface.  It comes with multilingual input support.  It
may also simplify input method development.")
   (home-page "http://ibus.googlecode.com/")
   (license lgpl2.1+)))
