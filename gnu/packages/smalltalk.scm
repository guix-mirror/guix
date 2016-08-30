;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages smalltalk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libsigsegv)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zip))

(define-public smalltalk
  (package
    (name "smalltalk")
    (version "3.2.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/smalltalk/smalltalk-"
                          version ".tar.xz"))
      (sha256
       (base32
        "1k2ssrapfzhngc7bg1zrnd9n2vyxp9c9m70byvsma6wapbvib6l1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("libffi" ,libffi)
       ("libltdl" ,libltdl)
       ("libsigsegv" ,libsigsegv)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("zip" ,zip)))
    (arguments
     `(#:phases (alist-cons-before
                 'configure 'fix-libc
                 (lambda _
                   (let ((libc (assoc-ref %build-inputs "libc")))
                     (substitute* "libc.la.in"
                       (("@LIBC_SO_NAME@") "libc.so")
                       (("@LIBC_SO_DIR@")  (string-append libc "/lib")))))
                %standard-phases)))
    (home-page "http://smalltalk.gnu.org/")
    (synopsis "Smalltalk environment")
    (description
     "GNU Smalltalk is a free implementation of the Smalltalk language.  It
implements the ANSI standard for the language and also includes extra classes
such as ones for networking and GUI programming.")
    (license license:gpl2+)))

(define-public squeak-vm
  (package
    (name "squeak-vm")
    (version "4.10.2.2614")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.squeakvm.org/unix/release/"
                           "Squeak-" version "-src.tar.gz"))
       (sha256
        (base32
         "0bpwbnpy2sb4gylchfx50sha70z36bwgdxraym4vrr93l8pd3dix"))
       (modules '((guix build utils)))
       (snippet
        ;; Make builds bit-reproducible.
        '(begin
           (substitute* "unix/cmake/verstamp"
             (("vm_date=.*")
              "vm_date = \"1970-01-01\";\n")
             (("ux_version=.*")
              "ux_version = \"GNU\";\n"))
           (substitute* "unix/vm/config.cmake"
             (("\\(VM_BUILD_STRING.*")
              "(VM_BUILD_STRING \\\"Built with GNU Guix\\\")"))))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("freetype" ,freetype)
       ("libffi" ,libffi)
       ("libxrender" ,libxrender)
       ("mesa" ,mesa)
       ("pulseaudio" ,pulseaudio)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-hardcoded-PATH
           (lambda _
             ;; Remove hard-coded FHS PATH entries.
             (substitute* '("unix/cmake/squeak.in"
                            "unix/cmake/squeak.sh.in")
               (("^PATH=.*") ""))
             #t))
         (add-after 'unpack 'create-build-dir
           (lambda _
             (mkdir "bld")
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "bld"
                 (zero?
                  (system* "../unix/cmake/configure"
                           (string-append "--prefix=" out)
                           "--without-quartz"))))))
         (replace 'build
           (lambda _
             (with-directory-excursion "bld"
               (zero? (system* "make"))))))))
    (synopsis "Smalltalk programming language and environment")
    (description "Squeak is a full-featured implementation of the Smalltalk
programming language and environment based on (and largely compatible with)
the original Smalltalk-80 system.  Squeak has very powerful 2- and 3-D
graphics, sound, video, MIDI, animation and other multimedia capabilities.  It
also includes a customisable framework for creating dynamic HTTP servers and
interactively extensible Web sites.")
    (home-page "http://www.squeakvm.org")
    (license license:x11)))
