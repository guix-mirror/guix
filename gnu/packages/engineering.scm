;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages engineering)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages linux) #:select (eudev)) ; FIXME: for pcb
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tcl)
  #:use-module (srfi srfi-1))

(define-public librecad
  (package
    (name "librecad")
    (version "2.0.6-rc")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/LibreCAD/LibreCAD/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1n1mh8asj6yrl5hi438dvizmrbqk1kni5xkizhi3pdmkg7z3hksm"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (alist-cons-after
        'unpack
        'patch-paths
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
            (substitute* "librecad/src/lib/engine/rs_system.cpp"
              (("/usr/share") (string-append out "/share")))))
        (alist-replace
         'configure
         (lambda* (#:key inputs #:allow-other-keys)
           (system* "qmake" (string-append "BOOST_DIR="
                                           (assoc-ref inputs "boost"))))
         (alist-replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (mkdir-p (string-append out "/bin"))
              (mkdir-p (string-append out "/share/librecad"))
              (copy-file "unix/librecad"
                         (string-append out "/bin/librecad"))
              (copy-recursively "unix/resources"
                                (string-append out "/share/librecad"))))
          %standard-phases)))))
    (inputs
     `(("boost" ,boost)
       ("muparser" ,muparser)
       ("freetype" ,freetype)
       ("qt" ,qt-4)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (home-page "http://librecad.org/")
    (synopsis "Computer-aided design (CAD) application")
    (description
     "LibreCAD is a 2D Computer-aided design (CAD) application for creating
plans and designs.")
    (license license:gpl2)))

(define-public geda-gaf
  (package
    (name "geda-gaf")
    (version "1.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ftp.geda-project.org/geda-gaf/stable/v"
                    (version-major+minor version) "/"
                    version "/geda-gaf-" version ".tar.gz"))
              (sha256
               (base32
                "08dpa506xk4gjbbi8vnxcb640wq4ihlgmhzlssl52nhvxwx7gx5v"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       ;; tests require a writable HOME
       (alist-cons-before
        'check 'set-home
        (lambda _
          (setenv "HOME" (getenv "TMPDIR")))
        %standard-phases)))
    (inputs
     `(("glib" ,glib)
       ("gtk" ,gtk+-2)
       ("guile" ,guile-2.0)
       ("desktop-file-utils" ,desktop-file-utils)
       ("shared-mime-info" ,shared-mime-info)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl))) ; for tests
    (home-page "http://geda-project.org/")
    (synopsis "Schematic capture, netlister, symbols, symbol checker, and utils")
    (description
     "gaf stands for “gschem and friends”.  It is a subset of the entire tool
suite grouped together under the gEDA name.  gEDA/gaf is a collection of tools
which currently includes: gschem, a schematic capture program; gnetlist, a
netlist generation program; gsymcheck, a syntax checker for schematic symbols;
gattrib, a spreadsheet programm that manipulates the properties of symbols of
a schematic; libgeda, libraries for gschem gnetlist and gsymcheck; gsch2pcb, a
tool to forward annotation from your schematic to layout using PCB; some minor
utilities.")
    (license license:gpl2+)))

(define-public pcb
  (package
    (name "pcb")
    (version "20140316")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ftp.geda-project.org/pcb/pcb-" version "/pcb-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0l6944hq79qsyp60i5ai02xwyp8l47q7xdm3js0jfkpf72ag7i42"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-cons-after
        'unpack 'use-wish8.6
        (lambda _
          (substitute* "configure"
            (("wish85") "wish8.6")))
        (alist-cons-after
         'install 'wrap
         (lambda* (#:key inputs outputs #:allow-other-keys)
           ;; FIXME: Mesa tries to dlopen libudev.so.0 and fails.  Pending a
           ;; fix of the mesa package we wrap the pcb executable such that
           ;; Mesa can find libudev.so.0 through LD_LIBRARY_PATH.
           (let* ((out (assoc-ref outputs "out"))
                  (path (string-append (assoc-ref inputs "udev") "/lib")))
             (wrap-program (string-append out "/bin/pcb")
               `("LD_LIBRARY_PATH" ":" prefix (,path)))))
         %standard-phases))))
    (inputs
     `(("dbus" ,dbus)
       ("mesa" ,mesa)
       ("udev" ,eudev) ;FIXME: required by mesa
       ("glu" ,glu)
       ("gd" ,gd)
       ("gtk" ,gtk+-2)
       ("gtkglext" ,gtkglext)
       ("desktop-file-utils" ,desktop-file-utils)
       ("shared-mime-info" ,shared-mime-info)
       ("tk" ,tk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("bison" ,bison)
       ("flex" ,flex)))
    (home-page "http://pcb.geda-project.org/")
    (synopsis "Design printed circuit board layouts")
    (description
     "GNU PCB is an interactive tool for editing printed circuit board
layouts.  It features a rats-nest implementation, schematic/netlist import,
and design rule checking.  It also includes an autorouter and a trace
optimizer; and it can produce photorealistic and design review images.")
    (license license:gpl2+)))
