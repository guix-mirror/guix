;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Sou Bunnbu <iyzsong@gmail.com>
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

(define-module (gnu packages wine)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages image)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public wine
  (package
    (name "wine")
    (version "1.7.31")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/wine/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "14747ihmyanxvv8mnrafbj3l6807h7zf1gcwidgm1f7s7g5n4viw"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Make.vars.in"
                  (("/bin/sh") "@SHELL@")))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gettext" ,gnu-gettext)
                     ("flex" ,flex)
                     ("bison" ,bison)
                     ("perl" ,perl)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gnutls" ,gnutls)
       ("lcms" ,lcms)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libgphoto2" ,libgphoto2)
       ("libmpg123" ,mpg123)
       ("libldap" ,openldap)
       ("libnetapi" ,samba)
       ("libsane" ,sane-backends)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg)
       ("libtiff" ,libtiff)
       ("libICE" ,libice)
       ("libX11" ,libx11)
       ("libXi" ,libxi)
       ("libXext" ,libxext)
       ("libXcursor" ,libxcursor)
       ("libXrender" ,libxrender)
       ("libXrandr" ,libxrandr)
       ("libXinerama" ,libxinerama)
       ("libXxf86vm" ,libxxf86vm)
       ("libXcomposite" ,libxcomposite)
       ("compositeproto" ,compositeproto)
       ("mesa" ,mesa)
       ("ncurses" ,ncurses)
       ("unixodbc" ,unixodbc)
       ("zlib" ,zlib)))
    (arguments
     `(;; The 64-bit build of Wine is reportedly less useful or even usable,
       ;; so force a 32-bit build (under the assumption that this package is
       ;; being used on an IA32-compatible architecture.)
       #:system "i686-linux"

       ;; XXX: There's a test suite, but it's unclear whether it's supposed to
       ;; pass.
       #:tests? #f

       #:phases
       (alist-cons-after
        'configure 'patch-dlopen-paths
        ;; Hardcode dlopened sonames to absolute paths.
        (lambda _
          (let* ((library-path (search-path-as-string->list
                                (getenv "LIBRARY_PATH")))
                 (find-so (lambda (soname)
                            (search-path library-path soname))))
            (substitute* "include/config.h"
              (("(#define SONAME_.* )\"(.*)\"" _ defso soname)
               (format #f "~a\"~a\"" defso (find-so soname))))))
        %standard-phases)))
    (home-page "http://www.winehq.org/")
    (synopsis "Implementation of the Windows API")
    (description
     "Wine (originally an acronym for \"Wine Is Not an Emulator\") is a
compatibility layer capable of running Windows applications.  Instead of
simulating internal Windows logic like a virtual machine or emulator, Wine
translates Windows API calls into POSIX calls on-the-fly, eliminating the
performance and memory penalties of other methods and allowing you to cleanly
integrate Windows applications into your desktop.")
    (license license:lgpl2.1+)

    ;; It really only supports IA32, but building on x86_64 will have the same
    ;; effect as building on i686 anyway.
    (supported-systems '("i686-linux" "x86_64-linux"))))
