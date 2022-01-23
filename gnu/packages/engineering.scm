;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2018, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018, 2019 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2018, 2019, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Tim Stahel <swedneck@swedneck.xyz>
;;; Copyright © 2019 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2019 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020,2021 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020, 2021 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020, 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2021 qblade <qblade@protonmail.com>
;;; Copyright © 2021 Gerd Heber <gerd.heber@gmail.com>
;;; Copyright © 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
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
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fpga)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)               ;FIXME: for pcb
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages openkinect)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public librecad
  (package
    (name "librecad")
    (version "2.2.0-rc2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/LibreCAD/LibreCAD")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08cl4935c9vznz9qdw1zgd86rn7hl64zpfayxl07x21bhf53pn24"))))
    (build-system qt-build-system)
    (arguments
     '(#:test-target "check"
       #:phases
       (modify-phases %standard-phases
         ;; Without this patch boost complains that "make_array" is not a
         ;; member of "boost::serialization".
         (add-after 'unpack 'patch-boost-error
           (lambda _
             (substitute* "librecad/src/lib/math/lc_quadratic.h"
               (("#include \"rs_vector.h\"" line)
                (string-append line
                               "\n#include <boost/serialization/array_wrapper.hpp>")))
             (substitute* "librecad/src/lib/math/rs_math.cpp"
               (("#include <boost/numeric/ublas/matrix.hpp>" line)
                (string-append "#include <boost/serialization/array_wrapper.hpp>\n"
                               line)))
             #t))
         ;; Fix build against Qt 5.11.
         (add-after 'unpack 'add-missing-headers
           (lambda _
             (substitute* "librecad/src/ui/generic/widgetcreator.cpp"
               (("#include <QPushButton>") "#include <QPushButton>
#include <QActionGroup>"))
             #t))
         (add-after 'unpack 'patch-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "librecad/src/lib/engine/rs_system.cpp"
                 (("/usr/share") (string-append out "/share"))))))
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (system* "qmake" (string-append "BOOST_DIR="
                                             (assoc-ref inputs "boost")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share/librecad")))
               (mkdir-p bin)
               (install-file "unix/librecad" bin)
               (mkdir-p share)
               (copy-recursively "unix/resources" share))
             #t)))))
    (inputs
     (list boost muparser freetype qtbase-5 qtsvg))
    (native-inputs
     (list pkg-config which))
    (home-page "https://librecad.org/")
    (synopsis "Computer-aided design (CAD) application")
    (description
     "LibreCAD is a 2D Computer-aided design (CAD) application for creating
plans and designs.")
    (license license:gpl2)))

(define-public geda-gaf
  (package
    (name "geda-gaf")
    (version "1.10.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ftp.geda-project.org/geda-gaf/stable/v"
                    (version-major+minor version) "/"
                    version "/geda-gaf-" version ".tar.gz"))
              (sha256
               (base32
                "19688b0671imy2i3jphcnq1120b8ymhr4wz2psiqylr82ljanqp8"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; tests require a writable HOME
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" (getenv "TMPDIR"))
             #t))
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "xorn/tests/Makefile.in"
               (("-Werror") ""))
             ;; This test returns its correct result in an unexpected order.
             (substitute* "libgeda/scheme/unit-tests/t0402-config.scm"
               (("\\(begin-config-test 'config-keys" m)
                (string-append "#;" m)))
             #t)))
       #:configure-flags
       (let ((pcb (assoc-ref %build-inputs "pcb")))
         (list (string-append "--with-pcb-datadir=" pcb "/share")
               (string-append "--with-pcb-lib-path="
                              pcb "/share/pcb/pcblib-newlib:"
                              pcb "/share/pcb/newlib")))))
    (inputs
     `(("gamin" ,gamin)
       ("glib" ,glib)
       ("gtk" ,gtk+-2)
       ("guile" ,guile-2.0)
       ("shared-mime-info" ,shared-mime-info)
       ("m4" ,m4)
       ("pcb" ,pcb)
       ("python" ,python-2))) ; for xorn
    (native-inputs
     (list groff pkg-config desktop-file-utils perl)) ; for tests
    (home-page "http://geda-project.org/")
    (synopsis "Schematic capture, netlister, symbols, symbol checker, and utils")
    (description
     "Gaf stands for “gschem and friends”.  It is a subset of the entire tool
suite grouped together under the gEDA name.  gEDA/gaf is a collection of tools
which currently includes: gschem, a schematic capture program; gnetlist, a
netlist generation program; gsymcheck, a syntax checker for schematic symbols;
gattrib, a spreadsheet programme that manipulates the properties of symbols of
a schematic; libgeda, libraries for gschem gnetlist and gsymcheck; gsch2pcb, a
tool to forward annotation from your schematic to layout using PCB; some minor
utilities.")
    (license license:gpl2+)))

(define-public lepton-eda
  ;; This is a fork of gEDA/gaf started in late 2016.  One of its goal is to
  ;; keep and to extend Guile support.
  (package
    (inherit geda-gaf)
    (name "lepton-eda")
    (version "1.9.14-20210407")
    (home-page "https://github.com/lepton-eda/lepton-eda")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (sha256
               (base32
                "0kyq0g6271vlwraw98637fn8bq2l6q4rll6748nn8rwsmfz71d0m"))
              (file-name (git-file-name name version))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("desktop-file-utils" ,desktop-file-utils)
       ("libtool" ,libtool)
       ("gettext" ,gettext-minimal)
       ("texinfo" ,texinfo)
       ("groff" ,groff)
       ("which" ,which)
       ,@(package-native-inputs geda-gaf)))
    (inputs
     `(("glib" ,glib)
       ("gtk" ,gtk+)
       ("gtksheet" ,gtksheet)
       ("guile" ,guile-3.0)
       ("shared-mime-info" ,shared-mime-info)
       ("m4" ,m4)
       ("pcb" ,pcb)))
    (arguments
     `(#:configure-flags
       (let ((pcb (assoc-ref %build-inputs "pcb")))
         ;; When running "make", the POT files are built with the build time as
         ;; their "POT-Creation-Date".  Later on, "make" notices that .pot
         ;; files were updated and goes on to run "msgmerge"; as a result, the
         ;; non-deterministic POT-Creation-Date finds its way into .po files,
         ;; and then in .gmo files.  To avoid that, simply make sure 'msgmerge'
         ;; never runs.  See <https://bugs.debian.org/792687>.
         (list "ac_cv_path_MSGMERGE=true"
               "--with-gtk3"
               (string-append "--with-pcb-datadir=" pcb "/share")
               (string-append "--with-pcb-lib-path="
                              pcb "/share/pcb/pcblib-newlib:"
                              pcb "/share/pcb/newlib")
               "CFLAGS=-fcommon"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-dynamic-link
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "libleptongui/scheme/schematic/ffi.scm.in"
               (("@LIBLEPTONGUI@")
                (string-append (assoc-ref outputs "out")
                               "/lib/libleptongui.so")))
             (substitute* '("libleptongui/scheme/schematic/ffi/gtk.scm.in"
                            "utils/attrib/lepton-attrib.scm")
               (("@LIBGTK@")
                (search-input-file inputs "/lib/libgtk-3.so")))
             (substitute* '("libleptongui/scheme/schematic/ffi/gobject.scm.in")
               (("@LIBGOBJECT@")
                (search-input-file inputs "/lib/libgobject-2.0.so")))
             (substitute* "liblepton/scheme/lepton/ffi.scm.in"
               (("@LIBLEPTON@")
                (string-append (assoc-ref outputs "out")
                               "/lib/liblepton.so")))
             (substitute* "utils/attrib/lepton-attrib.scm"
               (("@LIBLEPTONATTRIB@")
                (string-append (assoc-ref outputs "out")
                               "/lib/libleptonattrib.so")))
             (substitute* "liblepton/scheme/lepton/log.scm.in"
               (("@LIBGLIB@")
                (search-input-file inputs "/lib/libglib-2.0.so")))

             ;; For finding libraries when running tests before installation.
             (setenv "LIBLEPTONGUI"
                     (string-append (getcwd)
                                    "/libleptongui/src/.libs/libleptongui.so"))
             (setenv "LIBLEPTON"
                     (string-append (getcwd)
                                    "/libleptongui/src/.libs/liblepton.so"))
             (setenv "LD_LIBRARY_PATH"
                     (string-append (getcwd) "/libleptonattrib/src/.libs/:"
                                    (getenv "LIBRARY_PATH")))
             #t))
         (add-before 'bootstrap 'prepare
           (lambda _
             ;; Some of the scripts there are invoked by autogen.sh.
             (for-each patch-shebang (find-files "build-tools"))

             ;; Make sure 'msgmerge' can modify the PO files.
             (for-each (lambda (po)
                         (chmod po #o666))
                       (find-files "." "\\.po$"))

             ;; This would normally be created by invoking 'git', but it
             ;; doesn't work here.
             (call-with-output-file "version.h"
               (lambda (port)
                 (format port "#define PACKAGE_DATE_VERSION \"~a\"~%"
                         ,(string-drop version
                                       (+ 1 (string-index version #\-))))
                 (format port "#define PACKAGE_DOTTED_VERSION \"~a\"~%"
                         ,(string-take version
                                       (string-index version #\-)))
                 (format port "#define PACKAGE_GIT_COMMIT \"cabbag3\"~%")))
             #t))
         (add-after 'install 'compile-scheme-files
           (lambda* (#:key outputs #:allow-other-keys)
             (unsetenv "LIBLEPTONGUI")
             (unsetenv "LIBLEPTON")
             (unsetenv "LD_LIBRARY_PATH")
             (invoke "make" "precompile")
             #t)))))
    (description
     "Lepton EDA ia an @dfn{electronic design automation} (EDA) tool set
forked from gEDA/gaf in late 2016.  EDA tools are used for electrical circuit
design, schematic capture, simulation, prototyping, and production.  Lepton
EDA includes tools for schematic capture, attribute management, bill of
materials (BOM) generation, netlisting into over 20 netlist formats, analog
and digital simulation, and printed circuit board (PCB) layout, and many other
features.")))

(define-public pcb
  (package
    (name "pcb")
    (version "4.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/pcb/pcb/pcb-" version
                                  "/pcb-" version ".tar.gz"))
              (sha256
               (base32
                "1a7rilp75faidny0r4fdwdxkflyrqp6svxv9lbg7h868293962iz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-wish8.6
           (lambda _
             (substitute* "configure"
               (("wish85") "wish8.6"))
             #t))
         ;; It checks for "xhost", which we don't have.  This shouldn't
         ;; matter, because the test is supposed to be skipped, but it causes
         ;; "run_tests.sh" (and thus the "check" phase) to fail.
         (add-after 'unpack 'fix-check-for-display
           (lambda _
             (substitute* "tests/run_tests.sh"
               (("have_display=no") "have_display=yes"))
             #t))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; FIXME: Mesa tries to dlopen libudev.so.0 and fails.  Pending a
             ;; fix of the mesa package we wrap the pcb executable such that
             ;; Mesa can find libudev.so.0 through LD_LIBRARY_PATH.
             (let* ((out (assoc-ref outputs "out"))
                    (path (dirname
                           (search-input-file inputs "/lib/libudev.so"))))
               (wrap-program (string-append out "/bin/pcb")
                 `("LD_LIBRARY_PATH" ":" prefix (,path))))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             #t)))))
    (inputs
     `(("dbus" ,dbus)
       ("mesa" ,mesa)
       ("udev" ,eudev) ;FIXME: required by mesa
       ("glu" ,glu)
       ("gd" ,gd)
       ("gtk" ,gtk+-2)
       ("gtkglext" ,gtkglext)
       ("shared-mime-info" ,shared-mime-info)
       ("tk" ,tk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("bison" ,bison)
       ("desktop-file-utils" ,desktop-file-utils)
       ("flex" ,flex)
       ;; For tests
       ("imagemagick" ,imagemagick)
       ("gerbv" ,gerbv)
       ("ghostscript" ,ghostscript)
       ("xvfb" ,xorg-server-for-tests)))
    (home-page "http://pcb.geda-project.org/")
    (synopsis "Design printed circuit board layouts")
    (description
     "GNU PCB is an interactive tool for editing printed circuit board
layouts.  It features a rats-nest implementation, schematic/netlist import,
and design rule checking.  It also includes an autorouter and a trace
optimizer; and it can produce photorealistic and design review images.")
    (license license:gpl2+)))

(define-public pcb-rnd
  (package (inherit pcb)
    (name "pcb-rnd")
    (version "2.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://repo.hu/projects/pcb-rnd/releases/"
                                  "pcb-rnd-" version ".tar.gz"))
              (sha256
               (base32
                "06ylc2rd4yvzp3krk62q9dbi13h0yq1x257fbjkh10vfjn0ga5c2"))))
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'cc-is-gcc
           (lambda _ (setenv "CC" "gcc") #t))
         (replace 'configure
           ;; The configure script doesn't tolerate most of our configure flags.
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "sh" "configure"
                     (string-append "--prefix="
                                    (assoc-ref outputs "out"))))))))
    (home-page "http://repo.hu/projects/pcb-rnd/")
    (description "PCB RND is a fork of the GNU PCB circuit board editing tool
featuring various improvements and bug fixes.")))

(define-public fastcap
  (package
    (name "fastcap")
    (version "2.0-18Sep92")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "http://www.rle.mit.edu/cpg/codes/"
                                  name "-" version ".tgz"))
              (sha256
               (base32
                "0x37vfp6k0d2z3gnig0hbicvi0jp8v267xjnn3z8jdllpiaa6p3k"))
              (snippet
               ;; Remove a non-free file.
               '(begin
                  (delete-file "doc/psfig.sty")
                  #t))
              (patches (search-patches "fastcap-mulSetup.patch"
                                       "fastcap-mulGlobal.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     ;; FIXME: with texlive-tiny citation references are rendered as question
     ;; marks.  During the build warnings like these are printed:
     ;; LaTeX Warning: Citation `nabors91' on page 2 undefined on input line 3.
     `(("texlive" ,(texlive-updmap.cfg (list texlive-amsfonts)))
       ("ghostscript" ,ghostscript)))
    (arguments
     `(#:make-flags '("CC=gcc" "RM=rm" "SHELL=sh" "all")
       #:parallel-build? #f
       #:tests? #f ;; no tests-suite
       #:modules ((srfi srfi-1)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'make-doc
           (lambda _
             (invoke "make" "CC=gcc" "RM=rm" "SHELL=sh" "manual")))
         (add-before 'make-doc 'fix-doc
           (lambda _
             (substitute* "doc/Makefile" (("/bin/rm") (which "rm")))
             (substitute* (find-files "doc" "\\.tex")
               (("\\\\special\\{psfile=([^,]*),.*scale=([#0-9.]*).*\\}"
                 all file scale)
                (string-append "\\includegraphics[scale=" scale "]{"
                               file "}"))
               (("\\\\psfig\\{figure=([^,]*),.*width=([#0-9.]*in).*\\}"
                 all file width)
                (string-append "\\includegraphics[width=" width "]{"
                               file "}"))
               (("\\\\psfig\\{figure=([^,]*),.*height=([#0-9.]*in).*\\}"
                 all file height)
                (string-append "\\includegraphics[height=" height "]{"
                               file "}"))
               (("\\\\psfig\\{figure=([^,]*)\\}" all file)
                (string-append "\\includegraphics{" file "}")))
             (substitute* '("doc/mtt.tex" "doc/tcad.tex" "doc/ug.tex")
               (("^\\\\documentstyle\\[(.*)\\]\\{(.*)\\}"
                 all options class)
                (string-append "\\documentclass[" options "]{"
                               class "}\n"
                               "\\usepackage{graphicx}\n"
                               "\\usepackage{robinspace}"))
               (("\\\\setlength\\{\\\\footheight\\}\\{.*\\}" all)
                (string-append "%" all))
               (("\\\\setstretch\\{.*\\}" all)
                (string-append "%" all)))
             #t))
         (delete 'configure)
         (add-before 'install 'clean-bin
           (lambda _
             (delete-file (string-append (getcwd) "/bin/README"))
             #t))
         (add-before 'install 'make-pdf
           (lambda _
             (setenv "HOME" "/tmp")     ; FIXME: for texlive font cache
             (with-directory-excursion "doc"
               (and
                (for-each (lambda (file)
                            (invoke "dvips" file "-o"))
                          (find-files "." "\\.dvi"))
                (for-each (lambda (file)
                            (invoke "ps2pdf" file))
                          '("mtt.ps" "ug.ps" "tcad.ps"))
                (invoke "make" "clean")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (data (string-append out "/share"))
                    (bin (string-append out "/bin"))
                    (doc (string-append data "/doc/" ,name "-" ,version))
                    (examples (string-append doc "/examples")))
               (with-directory-excursion "bin"
                 (for-each (lambda (f)
                             (install-file f bin))
                           (find-files "." ".*")))
               (copy-recursively "doc" doc)
               (copy-recursively "examples" examples)
               #t))))))
    (home-page "https://www.rle.mit.edu/cpg/research_codes.htm")
    (synopsis "Multipole-accelerated capacitance extraction program")
    (description
     "Fastcap is a capacitance extraction program based on a
multipole-accelerated algorithm.")
    (license (license:non-copyleft #f "See fastcap.c."))))

(define-public fasthenry
  (package
    (name "fasthenry")
    (version "3.0-12Nov96")
    (source (origin
              (method url-fetch)
              (file-name (string-append name "-" version ".tar.gz"))
              (uri (string-append
                    "http://www.rle.mit.edu/cpg/codes/" name
                    "-" version ".tar.z"))
              (sha256
               (base32 "1a06xyyd40zhknrkz17xppl2zd5ig4w9g1grc8qrs0zqqcl5hpzi"))
              (patches (search-patches "fasthenry-spAllocate.patch"
                                       "fasthenry-spBuild.patch"
                                       "fasthenry-spUtils.patch"
                                       "fasthenry-spSolve.patch"
                                       "fasthenry-spFactor.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("CC=gcc" "RM=rm" "SHELL=sh" "all")
       #:parallel-build? #f
       #:tests? #f ;; no tests-suite
       #:modules ((srfi srfi-1)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (data (string-append out "/share"))
                           (bin (string-append out "/bin"))
                           (doc (string-append data "/doc/" ,name "-" ,version))
                           (examples (string-append doc "/examples")))
                      (with-directory-excursion "bin"
                        (for-each (lambda (f)
                                    (install-file f bin))
                                  (find-files "." ".*")))
                      (copy-recursively "doc" doc)
                      (copy-recursively "examples" examples)
                      #t))))))
    (home-page "https://www.rle.mit.edu/cpg/research_codes.htm")
    (synopsis "Multipole-accelerated inductance analysis program")
    (description
     "Fasthenry is an inductance extraction program based on a
multipole-accelerated algorithm.")
    (license (license:non-copyleft #f "See induct.c."))))

(define-public fritzing
  (package
    (name "fritzing")
    (version "0.9.3b")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fritzing/fritzing-app")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hpyc550xfhr6gmnc85nq60w00rm0ljm0y744dp0z88ikl04f4s3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "fritzing-parts-db")
                               "parts")
             ;; Make compatible with libgit2 > 0.24
             (substitute* "src/version/partschecker.cpp"
               (("error = git_remote_connect\\(remote, GIT_DIRECTION_FETCH, &callbacks\\)")
                "error = git_remote_connect(remote, GIT_DIRECTION_FETCH, &callbacks, NULL, NULL)"))

             ;; Use system libgit2 and boost.
             (substitute* "phoenix.pro"
               (("^LIBGIT2INCLUDE =.*")
                (string-append "LIBGIT2INCLUDE="
                               (assoc-ref inputs "libgit2") "/include\n"))
               (("^    LIBGIT2LIB =.*")
                (string-append "    LIBGIT2LIB="
                               (assoc-ref inputs "libgit2") "/lib\n")))
             ;; This file checks for old versions of Boost, insisting on
             ;; having us download the boost sources and placing them in the
             ;; build directory.
             (substitute* "pri/utils.pri"
               (("error\\(") "message("))

             (let ((out (assoc-ref outputs "out")))
               (invoke "qmake"
                       (string-append "QMAKE_LFLAGS_RPATH=-Wl,-rpath," out "/lib")
                       (string-append "PREFIX=" out)
                       "phoenix.pro")))))))
    (inputs
     `(("qtbase" ,qtbase-5)
       ("qtserialport" ,qtserialport)
       ("qtsvg" ,qtsvg)
       ("libgit2" ,libgit2)
       ("boost" ,boost)
       ("zlib" ,zlib)
       ("fritzing-parts-db"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/fritzing/fritzing-parts")
                 (commit version)))
           (file-name (git-file-name "fritzing-parts" version))
           (sha256
            (base32
             "1d2v8k7p176j0lczx4vx9n9gbg3vw09n2c4b6w0wj5wqmifywhc1"))))))
    (home-page "https://fritzing.org")
    (synopsis "Electronic circuit design")
    (description
     "The Fritzing application is @dfn{Electronic Design Automation} (EDA)
software with a low entry barrier, suited for the needs of makers and
hobbyists.  It offers a unique real-life \"breadboard\" view, and a parts
library with many commonly used high-level components.  Fritzing makes it very
easy to communicate about circuits, as well as to turn them into PCB layouts
ready for production.")
    ;; Documentation and parts are released under CC-BY-SA 3.0; source code is
    ;; released under GPLv3+.
    (license (list license:gpl3+ license:cc-by-sa3.0))))

(define-public qelectrotech
  (package
    (name "qelectrotech")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://git.tuxfamily.org/qet/qet.git/"
                           "snapshot/qet-" version ".tar.gz"))
       (sha256
        (base32 "0w70fqwhqqzga1kfp34v8z1xf9988nvvi3d5gwl2sg429p9mpsk2"))))
    (build-system qt-build-system)
    (arguments
     ;; XXX: tests are built for the CMake build option but it seems to be
     ;; broken in 0.8.0.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Patch hardcoded path before running qmake.
               (substitute* "qelectrotech.pro" (("\\/usr\\/local") out))
               (invoke "qmake")))))))
    (native-inputs
     (list pkg-config qttools))
    (inputs
     (list kcoreaddons kwidgetsaddons qtbase-5 qtsvg sqlite))
    (home-page "https://qelectrotech.org/")
    (synopsis "CAD/CAE editor focusing on schematics drawing features")
    (description "QElectroTech, or QET in short, is a desktop application to
create diagrams and schematics.  The software is primarily intended to create
electrical documentation but it can also be used to draw any kinds of diagrams,
such as those made in pneumatics, hydraulics, process industries, electronics,
and others.")
    (license license:gpl2+)))

(define-public gerbv
  (package
    (name "gerbv")
    (version "2.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gerbv/gerbv/gerbv-"
                                  version "/gerbv-" version ".tar.gz"))
              (sha256
               (base32
                "1d2k43k7i4yvbpi4sw1263a8d0q98z2n7aqhmpinpkih8a681vn5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("CFLAGS=-fcommon")))
    (native-inputs
     `(("glib:bin" ,glib "bin")         ; for glib-compile-schemas, etc.
       ("desktop-file-utils" ,desktop-file-utils)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("gtk" ,gtk+-2)))
    (home-page "http://gerbv.geda-project.org/")
    (synopsis "Gerber file viewer")
    (description
     "Gerbv is a viewer for files in the Gerber format (RS-274X only), which
is commonly used to represent printed circuit board (PCB) layouts.  Gerbv lets
you load several files on top of each other, do measurements on the displayed
image, etc.  Besides viewing Gerbers, you may also view Excellon drill files
as well as pick-place files.")
    (license license:gpl2+)))

(define-public translate2geda
  ;; There has been no formal release yet.
  (let ((commit "4c19e7eefa338cea8f1ee999ea8b37f8d0698169")
        (revision "1"))
    (package
      (name "translate2geda")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/erichVK5/translate2geda")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1h062bbpw8nk0jamkya1k4lsgaia796jyviiz2gkdi6k1bxhwgpa"))))
      (build-system ant-build-system)
      (arguments
       `(#:tests? #f ; there are no tests
         #:jar-name "translate2geda.jar"
         #:source-dir "."
         #:main-class "translate2geda"
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'install-bin
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (wrapper (string-append bin "/translate2geda")))
                 (mkdir-p bin)
                 (with-output-to-file wrapper
                   (lambda _
                     (format #t "#!/bin/sh~%exec ~a -jar ~a/share/java/translate2geda.jar"
                             (which "java") out)))
                 (chmod wrapper #o555))
               #t)))))
      (home-page "https://github.com/erichVK5/translate2geda")
      (synopsis "Utility for converting symbol and footprint formats to gEDA")
      (description
       "This package provides a utility for converting Kicad (@file{.mod},
@file{.lib}), Eagle (@file{.lbr}), gerber (@file{.gbr}, etc..),
BXL (@file{.bxl}), IBIS (@file{.ibs}), symdef, LT-Spice (@file{.asc}),
QUCS (@file{.sch}), and BSDL (@file{.bsd}) symbols and footprints and EggBot
fonts to gEDA.")
      (license license:gpl2+))))

(define-public libfive
  (let ((commit "8ca1b8685ef3fac7b64e66b10459b8421a3020c6")
        (revision "4"))
    (package
      (name "libfive")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/libfive/libfive")
                      (commit commit)))
                (sha256
                 (base32
                  "1c762cd70iv2b9av0l9lq0py9138y98wk3dirhdmil7jncdhvq98"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:test-target "libfive-test"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'remove-native-compilation
             (lambda _
               (substitute* "CMakeLists.txt" (("-march=native") ""))
               #t)))))
      (native-inputs
       (list pkg-config))
      (inputs
       (list boost libpng qtbase-5 eigen guile-3.0))
      (home-page "https://libfive.com")
      (synopsis "Tool for programmatic computer-aided design")
      (description
       "Libfive is a tool for programmatic computer-aided design (CAD).  In
libfive, solid models are defined as Scheme scripts, and there are no opaque
function calls into the geometry kernel: everything is visible to the user.
Even fundamental, primitive shapes are represented as code in the user-level
language.")
      (license (list license:mpl2.0               ;library
                     license:gpl2+))              ;Guile bindings and GUI

      ;; Mark as tunable to take advantage of SIMD code in Eigen.
      (properties '((tunable? . #t))))))

(define-public inspekt3d
  (let ((commit "703f52ccbfedad2bf5240bf8183d1b573c9d54ef")
        (revision "0"))
    (package
      (name "inspekt3d")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/kavalogic-inc/inspekt3d.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0lan6930g5a9z4ack9jj0zdd0mb2s6q2xzpiwcjdc3pvl9b1nbw4"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Allow builds with Guile 3.0.
                    (substitute* "configure.ac"
                      (("2\\.2") "3.0 2.2"))
                    #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-libfive-guile-location
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "inspekt3d/library.scm"
                 (("\"libfive-guile")
                  (string-append "\""
                                 (assoc-ref inputs "libfive")
                                 "/lib/libfive-guile")))
               #t)))))
      (native-inputs
       (list autoconf automake pkg-config))
      (inputs
       (list mesa guile-3.0))
      (propagated-inputs
       `(("libfive" ,libfive)
         ("guile-opengl" ,guile3.0-opengl)))
      (home-page "https://gitlab.com/kavalogic-inc/inspekt3d/")
      (synopsis "Lightweight 3D viewer for Libfive written in Guile Scheme")
      (description
       "Inspekt3d is a lightweight 3D viewer for Libfive written in Guile Scheme.
The viewer can be used interactively with a REPL (for example Geiser in
Emacs).")
      (license license:gpl3+))))

(define-public kicad
  (package
    (name "kicad")
    (version "5.1.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/kicad/code/kicad.git")
             (commit version)))
       (sha256
        (base32 "0kgikchqxds3mp71nkg307mr4c1dgv8akbmksz4w9x8jg4i1mfqq"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:out-of-source? #t
       #:tests? #f                      ; no tests
       #:build-type "Release"
       #:configure-flags
       ,#~(list
           "-DKICAD_SCRIPTING_PYTHON3=ON"
           "-DKICAD_SCRIPTING_WXPYTHON_PHOENIX=ON"
           "-DCMAKE_BUILD_WITH_INSTALL_RPATH=TRUE"
           (string-append "-DOCC_INCLUDE_DIR="
                          #$(this-package-input "opencascade-occt") "/include/opencascade"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-ngspice-detection
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "eeschema/CMakeLists.txt"
               (("NGSPICE_DLL_FILE=\"\\$\\{NGSPICE_DLL_FILE\\}\"")
                (string-append "NGSPICE_DLL_FILE=\""
                               (assoc-ref inputs "libngspice")
                               "/lib/libngspice.so\"")))))
         (add-after 'unpack 'fix-python-detection
           (lambda _
             (substitute* "CMakeModules/FindPythonLibs.cmake"
               (("_PYTHON3_VERSIONS 3\\.8 3\\.7")
                "_PYTHON3_VERSIONS 3.9 3.8 3.7"))))
         (add-after 'unpack 'add-missing-include
           (lambda _
             (substitute* "common/lib_tree_model.cpp"
               (("#include <eda_pattern_match.h>" all)
                (string-append "#include <algorithm>\n" all)))))
         (add-after 'install 'install-translations
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "kicad-i18n")
                               (assoc-ref outputs "out"))
             #t))
         (add-after 'install 'wrap-program
           ;; Ensure correct Python at runtime.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (python (assoc-ref inputs "python"))
                    (file (string-append out "/bin/kicad"))
                    (path (string-append
                           out
                           "/lib/python"
                           ,(version-major+minor
                             (package-version python))
                           "/site-packages:"
                           (getenv "GUIX_PYTHONPATH"))))
               (wrap-program file
                 `("GUIX_PYTHONPATH" ":" prefix (,path))
                 `("PATH" ":" prefix
                   (,(string-append python "/bin:")))))
             #t)))))
    (native-search-paths
     (list (search-path-specification
            (variable "KICAD")          ; to find kicad-doc
            (files '("")))
           (search-path-specification
            (variable "KICAD_TEMPLATE_DIR")
            (files '("share/kicad/template")))
           (search-path-specification
            (variable "KICAD_SYMBOL_DIR") ; symbol path
            (files '("share/kicad/library")))
           (search-path-specification
            (variable "KISYSMOD")       ; footprint path
            (files '("share/kicad/modules")))
           (search-path-specification
            (variable "KISYS3DMOD")     ; 3D model path
            (files '("share/kicad/modules/packages3d")))))
    (native-inputs
     `(("boost" ,boost)
       ("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("kicad-i18n" ,kicad-i18n)
       ("pkg-config" ,pkg-config)
       ("swig" ,swig)
       ("zlib" ,zlib)))
    (inputs
     `(("cairo" ,cairo)
       ("curl" ,curl)
       ("glew" ,glew)
       ("glm" ,glm)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("libngspice" ,libngspice)
       ("libsm" ,libsm)
       ("mesa" ,mesa)
       ("opencascade-occt" ,opencascade-occt)
       ("openssl" ,openssl)
       ("python" ,python-wrapper)
       ("wxwidgets" ,wxwidgets)
       ("wxpython" ,python-wxpython)))
    (home-page "https://www.kicad.org/")
    (synopsis "Electronics Design Automation Suite")
    (description "Kicad is a program for the formation of printed circuit
boards and electrical circuits.  The software has a number of programs that
perform specific functions, for example, pcbnew (Editing PCB), eeschema (editing
electrical diagrams), gerbview (viewing Gerber files) and others.")
    (license license:gpl3+)))

(define kicad-i18n
  (package
    (name "kicad-i18n")
    (version (package-version kicad))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/kicad/code/kicad-i18n.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y51l0r62cnxkvpc21732p3cx7pjvaqjih8193502hlv9kv1j9p6"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (home-page (package-home-page kicad))
    (synopsis "KiCad GUI translations")
    (description "This package contains the po files that are used for the GUI
translations for KiCad.")
    (license license:gpl3+)))

(define-public kicad-doc
  (package
    (name "kicad-doc")
    (version (package-version kicad))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/kicad/services/kicad-doc.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "026cz4zm903i75yhdvzha2nsnk4c0w07q3gd3xw3jmsmn18imgm3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_FORMATS=html")
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("gettext" ,gettext-minimal)
       ("git" ,git-minimal)
       ("perl" ,perl)
       ("perl-unicode-linebreak" ,perl-unicode-linebreak)
       ("perl-yaml-tiny" ,perl-yaml-tiny)
       ("po4a" ,po4a)
       ("source-highlight" ,source-highlight)))
    (home-page "https://kicad.org")
    (synopsis "KiCad official documentation")
    (description "This repository contains the official KiCad documentation.")
    (license license:gpl3+)))

(define-public kicad-symbols
  (package
    (name "kicad-symbols")
    (version (package-version kicad))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/kicad/libraries/kicad-symbols.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zdajim409570xzis53kmrbdcf7000v2vmc90f49h214lrx2zhr2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests exist
    (home-page (package-home-page kicad))
    (synopsis "Official KiCad schematic symbol libraries")
    (description "This package contains the official KiCad schematic symbol
libraries.")
    ;; TODO: Exception: "To the extent that the creation of electronic designs
    ;; that use 'Licensed Material' can be considered to be 'Adapted Material',
    ;; then the copyright holder waives article 3 of the license with respect to
    ;; these designs and any generated files which use data provided as part of
    ;; the 'Licensed Material'."
    ;; See <https://github.com/KiCad/kicad-symbols/blob/master/LICENSE.md>.
    (license license:cc-by-sa4.0)))

(define-public kicad-footprints
  (package
    (inherit kicad-symbols)
    (name "kicad-footprints")
    (version (package-version kicad))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/kicad/libraries/kicad-footprints.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qpii55dgv2gxqg1qq0dngdnbb9din790qi5qv0l6qqrzx843h5s"))))
    (synopsis "Official KiCad footprint libraries")
    (description "This package contains the official KiCad footprint libraries.")))

(define-public kicad-packages3d
  (package
    (inherit kicad-symbols)
    (name "kicad-packages3d")
    (version (package-version kicad))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/kicad/libraries/kicad-packages3D.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12w7m5nbk9kcnlnlg4sk1sd7xgb9i2kxfi0jcbd0phs89qyl7wjr"))))
    (synopsis "Official KiCad 3D model libraries")
    (description "This package contains the official KiCad 3D model libraries.")))

(define-public kicad-templates
  (package
    (inherit kicad-symbols)
    (name "kicad-templates")
    (version (package-version kicad))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/kicad/libraries/kicad-templates.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fbhn1l3j2rwc29aida9b408wif55i23bp9ddcs7dvf83smjm05g"))))
    (synopsis "Official KiCad project and worksheet templates")
    (description "This package contains the official KiCad project and
worksheet templates.")))

(define-public linsmith
  (package
    (name "linsmith")
    (version "0.99.31")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/linsmith/linsmith/linsmith-"
                    version "/linsmith-" version ".tar.gz"))
              (sha256
               (base32
                "13qj7n9826qc9shkkgd1p6vcpj78v4h9d67wbg45prg7rbnzkzds"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("CFLAGS=-fcommon")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gtk" ,gtk+-2)
       ("libgnome" ,libgnomeui)))
    (home-page "https://jcoppens.com/soft/linsmith/index.en.php")
    (synopsis "Smith Charting program")
    (description "LinSmith is a Smith Charting program, mainly designed for
educational use.  As such, there is an emphasis on capabilities that improve
the 'showing the effect of'-style of operation.")
    (license license:gpl2+)))

(define-public valeronoi
(package
  (name "valeronoi")
  (version "0.1.6")
  (source
   (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/ccoors/Valeronoi")
       (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1hpyh4mmjnxgkij7a6rynk2ril5413nkdvf8syn0lqvrmibdg7wv"))))
  (build-system cmake-build-system)
  (arguments
   `(#:phases
     (modify-phases %standard-phases
       (replace 'check
         (lambda* (#:key tests? #:allow-other-keys)
           (when tests?
             (invoke "./valeronoi-tests")))))))
  (inputs
   (list boost
         cgal
         gmp
         libxkbcommon
         mpfr
         openssl
         qtbase-5
         qtsvg))
  (home-page "https://github.com/ccoors/Valeronoi")
  (synopsis "WiFi mapping companion application for Valetudo")
  (description
   "Valeronoi (Valetudo + Voronoi) is a companion for Valetudo for generating
WiFi signal strength maps.  It visualizes them using a Voronoi diagram.")
  (license license:gpl3+)))

(define-public volk
  (package
    (name "volk")
    (version "2.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gnuradio/volk")
             (commit (string-append "v" version))
             (recursive? #t)))          ; for cpu_features git submodule
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mkqiw0i2fbbsk46zvk8yv5swl7ifhq6y1dlfphq8dsmkvxckqby"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-static-libraries
           ;; Remove libcpu_features.a (and any others that might appear).
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each delete-file (find-files lib "\\.a$"
                                                 #:fail-on-error? #t))
               #t)))
         (add-after 'install 'wrap-pythonpath
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (python (assoc-ref inputs "python"))
                    (file (string-append out "/bin/volk_modtool"))
                    (path (string-append
                           out
                           "/lib/python"
                           ,(version-major+minor
                             (package-version python))
                           "/site-packages:"
                           (getenv "GUIX_PYTHONPATH"))))
               (wrap-program file
                 `("GUIX_PYTHONPATH" ":" prefix (,path))
                 `("PATH" ":" prefix
                   (,(string-append python "/bin:")))))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("python" ,python-wrapper)
       ("python-mako" ,python-mako)))
    (home-page "https://www.libvolk.org/")
    (synopsis "Vector-Optimized Library of Kernels")
    (description
     "@acronym{VOLK, Vector-Optimized Library of Kernels} contains procedures
with machine-specific optimizations for mathematical functions.  It also
provides a machine-independent interface to select the best such procedures to
use on a given system.")
    (license license:gpl3+)))

(define-public libredwg
  (package
    (name "libredwg")
    (version "0.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/libredwg/libredwg-"
             version ".tar.xz"))
       (sha256
        (base32 "05v5k8fkx4z1p81x9kna7nlzmyx09dn686rj2zprnkf337qmg24i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-bindings")))
    (native-inputs
     `(("libxml2" ,libxml2)
       ("parallel" ,parallel)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("python-libxml2" ,python-libxml2)))
    (inputs
     (list pcre2))
    (home-page "https://www.gnu.org/software/libredwg/")
    (synopsis "C library to handle DWG (CAD-related) files")
    (description
     "GNU LibreDWG is a C library to handle DWG files.  It aims to be a free
replacement for the OpenDWG libraries.")
    (license license:gpl3+)))

(define-public minicom
  (package
    (name "minicom")
    (version "2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://salsa.debian.org/minicom-team/minicom.git")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0kfihxbh9qkjk9m1932ajyqx384c2aj3d9yaphh3i9i7y1shxlpx"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-lock-dir=/var/lock")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))))
         (replace 'bootstrap
           ;; autogen.sh needlessly hard-codes aclocal-1.14.
           (lambda _
             (invoke "autoreconf" "-vif")
             #t))
         (add-before 'configure 'patch-lock-check
           (lambda _
             (substitute* "configure"
               (("test -d [$]UUCPLOCK") "true"))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list ncurses))
    (home-page "https://salsa.debian.org/minicom-team/minicom")
    (synopsis "Serial terminal emulator")
    (description "@code{minicom} is a serial terminal emulator.")
    (license license:gpl2+)))

(define-public sterm
  (package
    (name "sterm")
    (version "20200306")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wentasah/sterm")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "031pd8yz2bfzqbari6za1c3xcqmw94ap4vbrjzb3v6izjcrca58c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases (delete 'configure))))
    (synopsis "Simple serial terminal")
    (description "This is a minimalist terminal program like minicom or cu.
The only thing it does is creating a bidirectional connection between
stdin/stdout and a terminal device (e.g. serial line).
It can also set serial line baudrate, manipulate DTR/RTS modem lines,
send break and throttle transmission speed.")
    (home-page "https://github.com/wentasah/sterm")
    (license license:gpl3+)))

(define-public harminv
  (package
    (name "harminv")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/stevengj/harminv/"
                              "releases/download/v" version "/"
                              name "-" version ".tar.gz"))
              (sha256
               (base32
                "0w1n4d249vlpda0hi6z1v13qp21vlbp3ykn0m8qg4rd5132j7fg1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-shared")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-tests
           (lambda _
             (substitute* "./sines-test.sh"
               ; change test frequency range - default fails
               (("0\\.15") "0.16"))
             #t)))))
    (native-inputs
     `(("fortran" ,gfortran)))
    (inputs
     (list lapack))
    (home-page "https://github.com/stevengj/harminv")
    (synopsis "Harmonic inversion solver")
    (description
     "Harminv is a free program (and accompanying library) to solve the problem of
harmonic inversion — given a discrete-time, finite-length signal that consists of a sum
of finitely-many sinusoids (possibly exponentially decaying) in a given bandwidth, it
determines the frequencies, decay constants, amplitudes, and phases of those sinusoids.")
    (license license:gpl2+)))

(define-public guile-libctl
  (package
    (name "guile-libctl")
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/NanoComp/libctl/releases/download/v"
                version "/libctl-" version ".tar.gz"))
              (sha256
               (base32
                "0x8r56lpfq83kfbq28vr25icl19xpfd6fjrxzcpdmv30l9pash83"))))
    (build-system gnu-build-system)
    (arguments
      `(#:configure-flags '("--enable-shared")))
    (native-inputs
     `(("fortran" ,gfortran)))
    (inputs
     (list guile-2.2))
    (home-page "http://ab-initio.mit.edu/wiki/index.php/Libctl")
    (synopsis "Flexible control files implementation for scientific simulations")
    (description
     "Libctl is a Guile-based library implementing flexible control files
for scientific simulations.")
    (license license:gpl2+)))

(define-public mpb
  (package
    (name "mpb")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/NanoComp/mpb/releases/download/v"
                version "/mpb-" version ".tar.gz"))
              (sha256
               (base32
                "1jgrb7dd6qs6j6y1gnxmdgrh79l2bvqa6nk60a4pw1annsks4brd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-libctl="
                            (assoc-ref %build-inputs "libctl")
                            "/share/libctl")
             "--enable-shared")))
    (native-inputs
     `(("fortran" ,gfortran)
       ("pkg-config" ,pkg-config)
       ("swig" ,swig)))
    (inputs
     `(("fftw" ,fftw)
       ("gsl" ,gsl)
       ("guile" ,guile-2.2)
       ("hdf5" ,hdf5)
       ("lapack" ,lapack)
       ("libctl" ,guile-libctl)
       ("readline" ,readline)
       ("zlib" ,zlib)))
    (home-page "http://ab-initio.mit.edu/wiki/index.php/MIT_Photonic_Bands")
    (synopsis "Computes band structures and electromagnetic modes of dielectric
structures")
    (description
     "MIT Photonic-Bands (MPB) computes definite-frequency eigenstates (harmonic modes)
of Maxwell's equations in periodic dielectric structures for arbitrary wavevectors, using
fully-vectorial and three-dimensional methods.")
    (license license:gpl2+)))

(define-public meep
  (package
    (name "meep")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/NanoComp/meep/releases/download/v"
                version "/meep-" version ".tar.gz"))
              (sha256
               (base32
                "14zyxmm3p80j5fz5b89sl7hgkgcisqjny5hjh4pi274ziqjqz8bm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-libctl="
                            (assoc-ref %build-inputs "libctl")
                            "/share/libctl"))))
    (native-inputs
     `(("fortran" ,gfortran)
       ("pkg-config" ,pkg-config)
       ("swig" ,swig)))
    (inputs
     `(("fftw" ,fftw)
       ("gsl" ,gsl)
       ("guile" ,guile-2.2)
       ("harminv" ,harminv)
       ("hdf5" ,hdf5)
       ("lapack" ,lapack)
       ("libctl" ,guile-libctl)
       ("mpb" ,mpb)
       ("zlib" ,zlib)))
    (home-page "http://ab-initio.mit.edu/wiki/index.php/Meep")
    (synopsis "Finite-difference time-domain (FDTD) simulation software")
    (description
     "Meep is a finite-difference time-domain (FDTD) simulation software package
developed at MIT to model electromagnetic systems.")
    (license license:gpl2+)))

(define-public adms
  (package
    (name "adms")
    (version "2.3.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Qucs/ADMS")
                    (commit (string-append "release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i37c9k6q1iglmzp9736rrgsnx7sw8xn3djqbbjw29zsyl3pf62c"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           bison
           flex
           libtool
           perl
           perl-xml-libxml))
    (home-page "https://github.com/Qucs/ADMS")
    (synopsis "Automatic device model synthesizer")
    (description
     "ADMS is a code generator that converts electrical compact device models
specified in high-level description language into ready-to-compile C code for
the API of spice simulators.  Based on transformations specified in XML
language, ADMS transforms Verilog-AMS code into other target languages.")
    (license license:gpl3)))

(define-public capstone
  (package
    (name "capstone")
    (version "3.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/aquynh/capstone")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dgf82kxj4rs45d6s8sr984c38sll1n5scpypjlyh21gh2yl4qfw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         ;; cstool's Makefile ‘+=’s LDFLAGS, so we cannot pass it as a make flag.
         (add-before 'build 'fix-cstool-ldflags
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "LDFLAGS"  (string-append "-Wl,-rpath="
                                               (assoc-ref outputs "out") "/lib"))
             #t)))))
    (home-page "https://www.capstone-engine.org")
    (synopsis "Lightweight multi-platform, multi-architecture disassembly framework")
    (description
     "Capstone is a lightweight multi-platform, multi-architecture disassembly
framework.  Capstone can disassemble machine code for many supported architectures
such as x86, x86_64, arm, arm64, mips, ppc, sparc, sysz and xcore.  It provides
bindings for Python, Java, OCaml and more.")
    (license license:bsd-3)))

;; FIXME: This package has a timestamp embedded in
;; lib/python3.5/site-packages/capstone/__pycache__/__iti__.cpython-35.pyc
(define-public python-capstone
  (package
    (inherit capstone)
    (name "python-capstone")
    (propagated-inputs
     (list capstone))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-and-fix-setup-py
           (lambda _
             (chdir "bindings/python")
             ;; Do not build the library again, because we already have it.
             (substitute* "setup.py" ((".*   build_libraries.*") ""))
             ;; This substitution tells python-capstone where to find the
             ;; library.
             (substitute* "capstone/__init__.py"
               (("pkg_resources.resource_filename.*")
                (string-append "'" (assoc-ref %build-inputs "capstone") "/lib',\n")))
             #t)))))))

(define-public python2-capstone
  (package-with-python2 python-capstone))


(define-public python-esptool-3.0
  (package
    (name "python-esptool")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "esptool" version))
       (sha256
        (base32
         "0d69rd9h8wrzjvfrc66vmz4qd5hly2fpdcwj2bdrlb7dbwikv5c7"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-ecdsa python-pyaes python-pyserial python-reedsolo
           python-cryptography python-bitstring))
    (home-page "https://github.com/espressif/esptool")
    (synopsis "Bootloader utility for Espressif ESP8266 & ESP32 chips")
    (description
     "@code{esptool.py} is a Python-based utility to communicate with the ROM
bootloader in Espressif ESP8266 & ESP32 series chips.")
    (license license:gpl2+)))

(define-public radare2
  (package
    (name "radare2")
    (version "5.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/radareorg/radare2")
                    (commit version)))
              (sha256
               (base32
                "0hv9x31iabasj12g8f04incr1rbcdkxi3xnqn3ggp8gl4h6pf2f3"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; tests require git and network access
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'mklibdir
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib"))
             #t)))
       #:configure-flags
       (list "--with-openssl"
             "--with-rpath"
             "--with-syscapstone"
             "--with-sysmagic"
             "--with-syszip"
             "--with-sysxxhash")
       #:make-flags
       (list "CC=gcc")))
    ;; TODO: Add gmp and libzip and make the build system actually find them.
    (inputs
     (list capstone libuv openssl zip))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     ;; In the Libs: section of r_hash.pc.
     (list xxhash))
    (home-page "https://radare.org/")
    (synopsis "Reverse engineering framework")
    (description
     "Radare2 is a complete framework for reverse-engineering, debugging, and
analyzing binaries.  It is composed of a set of small utilities that can be
used together or independently from the command line.

Radare2 is built around a scriptable disassembler and hexadecimal editor that
support a variety of executable formats for different processors and operating
systems, through multiple back ends for local and remote files and disk
images.

It can also compare (@dfn{diff}) binaries with graphs and extract information
like relocation symbols.  It is able to deal with malformed binaries, making
it suitable for security research and analysis.")
    (license license:lgpl3)))

(define-public asco
  (package
    (name "asco")
    (version "0.4.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/asco/asco/" version "/ASCO-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "119rbc2dc8xzwxvykgji0v0nrzvymjmlizr1bc2mihspj686kxsl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                                ; no tests
       #:make-flags '("all" "asco-mpi")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((coreutils (assoc-ref inputs "coreutils-minimal")))
               (substitute* '("errfunc.c" "asco.c")
                 (("cp ")
                  (string-append coreutils "/bin/cp "))
                 (("nice")
                  (string-append coreutils "/bin/nice")))
               (substitute* "Makefile"
                 (("<FULL_PATH_TO_MPICH>/bin/mpicc") (which "mpicc")))
               #t)))
         (replace 'install                        ; no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each (lambda (file)
                         (install-file file (string-append
                                             (assoc-ref outputs "out")
                                             "/bin")))
                       '("asco" "asco-mpi" "asco-test"
                         "tools/alter/alter" "tools/log/log"))
             #t)))))
    (native-inputs
     `(("mpi" ,openmpi)))
    (inputs
     (list coreutils-minimal))
    (home-page "http://asco.sourceforge.net/")
    (synopsis "SPICE circuit optimizer")
    (description
     "ASCO brings circuit optimization capabilities to existing SPICE simulators using a
high-performance parallel differential evolution (DE) optimization algorithm.")
    (license license:gpl2+)))

(define-public libngspice
  ;; Note: The ngspice's build system does not allow us to build both the
  ;; library and the executables in one go.  Thus, we have two packages.
  ;; See <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27344#236>.
  (package
    (name "libngspice")
    (version "35")
    (source
     (origin
       (method url-fetch)
       (uri (list
             (string-append "mirror://sourceforge/ngspice/ng-spice-rework/"
                            version "/ngspice-" version ".tar.gz")
             (string-append "mirror://sourceforge/ngspice/ng-spice-rework/"
                            "old-releases/" version
                            "/ngspice-" version ".tar.gz")))
       (sha256
        (base32 "1v3ra9p2sc6ash1bbjm6i4i3dd6ymxjgnyha7z5rlmyvfv1gbdy1"))))
    (build-system gnu-build-system)
    (arguments
     `(;; No tests for libngspice exist.
       ;; The transient tests for ngspice fail.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-timestamps
           (lambda _
             (substitute* "configure"
               (("`date`") "Thu Jan  1 00:00:01 UTC 1970"))))
         (add-after 'unpack 'delete-program-manuals
           (lambda _
             (substitute* "man/man1/Makefile.in"
               (("^man_MANS = ngspice\\.1 ngnutmeg\\.1 ngsconvert\\.1 ngmultidec\\.1")
                "man_MANS = "))))
         (add-after 'install 'delete-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively
              (string-append (assoc-ref outputs "out")
                             "/share/ngspice/scripts")))))
       #:configure-flags
       (list "--enable-openmp"
             "--enable-ciderlib"
             "--enable-xspice"
             "--with-ngshared"
             ;; Readline must be disabled to build KiCad with ngspice 34.  See
             ;; https://bugs.archlinux.org/task/70563 for reference.
             "--with-readline=no")))
    (native-inputs
     (list bison flex))
    (inputs
     `(("libxaw" ,libxaw)
       ("mpi" ,openmpi)))
    (home-page "http://ngspice.sourceforge.net/")
    (synopsis "Mixed-level/mixed-signal circuit simulator")
    (description
     "Ngspice is a mixed-level/mixed-signal circuit simulator.  It includes
@code{Spice3f5}, a circuit simulator, and @code{Xspice}, an extension that
provides code modeling support and simulation of digital components through
an embedded event driven algorithm.")
    (license (list license:lgpl2.0+ ; code in frontend/numparam
                   (license:non-copyleft "file:///COPYING") ; spice3 bsd-style
                   license:bsd-3 ; ciderlib
                   license:public-domain)))) ; xspice

(define-public ngspice
  ;; The ngspice executables (see libngpsice above.)
  (package (inherit libngspice)
    (name "ngspice")
    (arguments
     (substitute-keyword-arguments (package-arguments libngspice)
       ((#:configure-flags flags)
        `(delete "--with-ngshared" ,flags))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'delete-include-files
             (lambda _
               (substitute* "src/Makefile.in"
                 (("^SUBDIRS = misc maths frontend spicelib include/ngspice")
                  "SUBDIRS = misc maths frontend spicelib"))))
           (delete 'delete-program-manuals)
           (delete 'delete-script-files)))))
    (inputs
     (list libngspice readline))))

(define trilinos-serial-xyce
  ;; Note: This is a Trilinos containing only the packages Xyce needs, so we
  ;; keep it private.  See
  ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27344#248>.
  ;; TODO: Remove when we have modular Trilinos packages?
  (package
    (name "trilinos-serial-xyce")
    (version "12.12.1")
    (source
     (origin (method url-fetch)
             (uri (string-append "https://trilinos.org/oldsite/download/files/trilinos-"
                                 version "-Source.tar.gz"))
             (sha256
              (base32
               "1zgrcksrcbmyy79mbdv0j4j4sh0chpigxk8vcrrwgaxyxwxxhrvw"))))
    (build-system cmake-build-system)
    (arguments
     `(#:out-of-source? #t
       #:phases
       (modify-phases %standard-phases
         ;; Delete unneeded tribits(build system) directory which makes validate-runpath
         ;; phase to fail.
         (add-before 'validate-runpath 'delete-tribits
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively
              (string-append (assoc-ref outputs "out")
                             "/lib/cmake/tribits"))
             #t)))
       #:configure-flags
       (list "-DCMAKE_CXX_FLAGS=-O3 -fPIC"
             "-DCMAKE_C_FLAGS=-O3 -fPIC"
             "-DCMAKE_Fortran_FLAGS=-O3 -fPIC"
             "-DTrilinos_ENABLE_NOX=ON"
             "-DNOX_ENABLE_LOCA=ON"
             "-DTrilinos_ENABLE_EpetraExt=ON"
             "-DEpetraExt_BUILD_BTF=ON"
             "-DEpetraExt_BUILD_EXPERIMENTAL=ON"
             "-DEpetraExt_BUILD_GRAPH_REORDERINGS=ON"
             "-DTrilinos_ENABLE_TrilinosCouplings=ON"
             "-DTrilinos_ENABLE_Ifpack=ON"
             "-DTrilinos_ENABLE_Isorropia=ON"
             "-DTrilinos_ENABLE_AztecOO=ON"
             "-DTrilinos_ENABLE_Belos=ON"
             "-DTrilinos_ENABLE_Teuchos=ON"
             "-DTeuchos_ENABLE_COMPLEX=ON"
             "-DTrilinos_ENABLE_Amesos=ON"
             "-DAmesos_ENABLE_KLU=ON"
             "-DAmesos_ENABLE_UMFPACK=ON"
             "-DTrilinos_ENABLE_Sacado=ON"
             "-DTrilinos_ENABLE_Kokkos=OFF"
             "-DTrilinos_ENABLE_ALL_OPTIONAL_PACKAGES=OFF"
             "-DTPL_ENABLE_AMD=ON"
             "-DTPL_ENABLE_UMFPACK=ON"
             "-DTPL_ENABLE_BLAS=ON"
             "-DTPL_ENABLE_LAPACK=ON")))
    (native-inputs
     `(("fortran" ,gfortran)
       ("swig" ,swig)))
    (inputs
     `(("boost" ,boost)
       ("lapack" ,lapack)
       ("suitesparse" ,suitesparse)))
    (home-page "https://trilinos.org")
    (synopsis "Engineering and scientific problems algorithms")
    (description
     "The Trilinos Project is an effort to develop algorithms and enabling
technologies within an object-oriented software framework for the solution of
large-scale, complex multi-physics engineering and scientific problems.  A
unique design feature of Trilinos is its focus on packages.")
    (license (list license:lgpl2.1+
                   license:bsd-3))))

(define-public xyce-serial
  (package
    (name "xyce-serial")
    (version "6.8")
    (source
     (origin (method url-fetch)
             (uri (string-append "https://archive.org/download/Xyce-"
                                 version "/Xyce-" version ".tar.gz"))
             (sha256
              (base32
               "09flp1xywbb2laayd9rg8vd0fjsh115y6k1p71jacy0nrbdvvlcg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list
        "CXXFLAGS=-O3"
        (string-append "ARCHDIR="
                       (assoc-ref %build-inputs "trilinos")))))
    (native-inputs
     `(("bison" ,bison-3.0)                  ;'configure' fails with Bison 3.4
       ("flex" ,flex)
       ("fortran" ,gfortran)))
    (inputs
     `(("fftw" ,fftw)
       ("suitesparse" ,suitesparse)
       ("lapack" ,lapack)
       ("trilinos" ,trilinos-serial-xyce)))
    (home-page "https://xyce.sandia.gov/")
    (synopsis "High-performance analog circuit simulator")
    (description
     "Xyce is a SPICE-compatible, high-performance analog circuit simulator,
capable of solving extremely large circuit problems by supporting large-scale
parallel computing platforms.  It also supports serial execution.")
    (license license:gpl3+)))

(define trilinos-parallel-xyce
  (package (inherit trilinos-serial-xyce)
    (name "trilinos-parallel-xyce")
    (arguments
     `(,@(substitute-keyword-arguments (package-arguments trilinos-serial-xyce)
           ((#:configure-flags flags)
            `(append (list "-DTrilinos_ENABLE_ShyLU=ON"
                           "-DTrilinos_ENABLE_Zoltan=ON"
                           "-DTPL_ENABLE_MPI=ON")
                     ,flags)))))
    (inputs
     `(("mpi" ,openmpi)
       ,@(package-inputs trilinos-serial-xyce)))))

(define-public xyce-parallel
  (package (inherit xyce-serial)
    (name "xyce-parallel")
    (arguments
     `(,@(substitute-keyword-arguments (package-arguments xyce-serial)
           ((#:configure-flags flags)
            `(list "CXXFLAGS=-O3"
                   "CXX=mpiCC"
                   "CC=mpicc"
                   "F77=mpif77"
                   "--enable-mpi"
                   (string-append
                    "ARCHDIR="
                    (assoc-ref %build-inputs "trilinos")))))))
    (propagated-inputs
     `(("mpi" ,openmpi)))
    (inputs
     `(("trilinos" ,trilinos-parallel-xyce)
       ,@(alist-delete "trilinos"
                       (package-inputs xyce-serial))))))

(define-public freehdl
  (package
    (name "freehdl")
    (version "0.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://downloads.sourceforge.net/qucs/freehdl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "117dqs0d4pcgbzvr3jn5ppra7n7x2m6c161ywh6laa934pw7h2bz"))
              (patches
               (list (origin
                       ;; Fix build with GCC 7.  Patch taken from Arch Linux:
                       ;; https://github.com/archlinux/svntogit-community/tree/packages/freehdl/trunk
                       (method url-fetch)
                       (uri (string-append "https://raw.githubusercontent.com"
                                           "/archlinux/svntogit-community"
                                           "/3bb90d64dfe6883e26083cd1fa96226d0d59175a"
                                           "/trunk/build-fix.patch"))
                       (file-name "freehdl-c++-namespace.patch")
                       (sha256
                        (base32
                         "09df3c70rx81rnhlhry1wpdhji274nx9jb74rfprk06l4739zm08")))))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-pkg-config
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "freehdl/freehdl-config"
               (("pkg-config")
                (search-input-file inputs "/bin/pkg-config"))
               (("cat")
                (search-input-file inputs "/bin/cat")))))
         (add-after 'patch-pkg-config 'setenv
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CXX" (search-input-file inputs "/bin/g++"))
             (setenv "SYSTEM_LIBTOOL"
                     (search-input-file inputs "/bin/libtool"))))
         (add-after 'setenv 'patch-gvhdl
           (lambda _
             (substitute* "v2cc/gvhdl.in"
               (("--mode=link") "--mode=link --tag=CXX")
               (("-lm") "-lm FREEHDL/lib/freehdl/libieee.la"))
             #t))
         (add-after 'patch-gvhdl 'patch-freehdl-gennodes
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "freehdl/freehdl-gennodes.in"
               (("guile")
                (search-input-file inputs "/bin/guile"))
               (("\\(debug") ";(debug")
               (("\\(@ ") "(apply-emit")
               (("\\(@@ ") "(apply-mini-format"))
             #t))
         (add-after 'configure 'patch-freehdl-pc
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "freehdl.pc"
               (("=g\\+\\+")
                (string-append "=" (assoc-ref inputs "gcc-toolchain")
                               "/bin/g++"))
               (("=libtool")
                (string-append "=" (assoc-ref inputs "libtool")
                               "/bin/libtool")))
             #t))
         (add-after 'install-scripts 'make-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; 'gvhdl' invokes the C compiler directly, so hard-code its
               ;; file name.
               (wrap-program (string-append out "/bin/gvhdl")
                 `("CPLUS_INCLUDE_PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "gcc-toolchain")
                                    "/include")))
                 `("LIBRARY_PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "gcc-toolchain")
                                    "/lib")))
                 `("PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "gcc-toolchain")
                                    "/bin")
                    ,(string-append (assoc-ref inputs "coreutils")
                                    "/bin"))))
               (wrap-program (string-append out "/bin/freehdl-config")
                 `("PKG_CONFIG_PATH" ":" prefix (,(string-append out "/lib/pkgconfig")))))
             #t)))))
    (inputs
     (list coreutils
           gcc-toolchain
           guile-2.2
           perl
           pkg-config
           libtool))
    (native-inputs
     `(("pkg-config-native" ,pkg-config)
       ("libtool-native" ,libtool)))
    (home-page "http://www.freehdl.seul.org/")
    (synopsis "VHDL simulator")
    (description
     "FreeHDL is a compiler/simulator suite for the hardware description language VHDL.
  VHDL'93 as well as VHDL'87 standards are supported.")
    (license (list license:gpl2+
                   license:lgpl2.0+)))) ; freehdl's libraries

(define-public librepcb
  (package
    (name "librepcb")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.librepcb.org/releases/"
                           version "/librepcb-" version "-source.zip"))
       (sha256
        (base32 "0smp1p7wnrj0vh4rmz1cr2krfawc2lzx0pbzmgyay7xdp6jxympr"))))
    (build-system gnu-build-system)
    (inputs
     (list qtbase-5 qtsvg zlib))
    (native-inputs
     (list qttools ; for lrelease
           unzip))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p "build")
             (chdir "build")
             (let ((lrelease (search-input-file inputs "/bin/lrelease"))
                   (out (assoc-ref outputs "out")))
               (invoke "qmake"
                       (string-append "QMAKE_LRELEASE=" lrelease)
                       (string-append "PREFIX=" out)
                       "../librepcb.pro")))))))
    (home-page "https://librepcb.org/")
    (synopsis "Electronic Design Automation tool")
    (description "LibrePCB is @dfn{Electronic Design Automation} (EDA)
software to develop printed circuit boards.  It features human readable file
formats and complete project management with library, schematic and board
editors.")
    (license (list license:gpl3+
                   license:boost1.0 ; libs/clipper,
                                    ; libs/optional/tests/catch.hpp,
                                    ; libs/sexpresso/tests/catch.hpp
                   license:expat ; libs/delaunay-triangulation,
                                 ; libs/parseagle, libs/type_safe
                   license:asl2.0 ; libs/fontobene, libs/googletest,
                                  ; libs/parseagle
                   license:isc ; libs/hoedown
                   license:cc0 ; libs/optional, libs/sexpresso
                   license:bsd-2 ; libs/optional/tests/catch.hpp
                   license:lgpl2.1+)))) ; libs/quazip

(define-public gpx
  (package
    (name "gpx")
    (version "2.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/markwal/GPX")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yab269x8qyf7rd04vaxyqyjv4pzz9lp4sc4dwh927k23avr3rw5"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/markwal/GPX")
    (synopsis "Converting gcode to x3g files for 3D printing")
    (description
     "GPX is a post processing utility for converting gcode output from 3D
slicing software to x3g files for standalone 3D printing on common 3D
printers.")
    (license license:gpl2+)))

(define-public gnucap
  (package
    (name "gnucap")
    (version "20171003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://git.savannah.gnu.org/cgit/gnucap.git/snapshot/gnucap-"
                           version ".tar.gz"))
       (sha256
        (base32
         "16m09xa685qhj5fqq3bcgakrwnb74xhf5f7rpqkkf9fg8plzbb1g"))))
    (build-system gnu-build-system)
    (inputs
     (list readline))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Set correct rpath so that gnucap finds libgnucap.so.
               (substitute* (list "apps/configure" "lib/configure"
                                  "main/configure" "modelgen/configure")
                 (("LDFLAGS =")
                  (string-append "LDFLAGS = -Wl,-rpath=" out "/lib")))
               ;; gnucap uses a hand-written configure script that expects the
               ;; --prefix argument to be the first argument passed to it.
               (invoke "./configure" (string-append "--prefix=" out)))))
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (libpath "../lib/O:../apps/O"))
               (with-directory-excursion "tests"
                 ;; Make test return non-zero exit code when a test fails.
                 (substitute* "test"
                   (("/bin/sh") "/bin/sh -e")
                   (("\\|\\| echo \"\\*\\*\\*\\* \\$ii fails \\*\\*\\*\\*\"") ""))
                 ;; Fix expected plugin search path for test c_attach.1.gc
                 (substitute* "==out/c_attach.1.gc.out"
                   (("/usr/local/lib/gnucap")
                    (string-append libpath ":" out "/lib/gnucap")))
                 ;; Set library path so that gnucap can find libgnucap.so
                 ;; while running the tests.
                 (setenv "LD_LIBRARY_PATH" libpath)
                 (invoke "./test" "../main/O/gnucap" "" "test-output" "==out"))))))))
    (home-page "https://www.gnu.org/software/gnucap/")
    (synopsis "Mixed analog and digital circuit simulator")
    (description "GNUcap is a circuit analysis package.  It offers a general
purpose circuit simulator and can perform DC and transient analyses, fourier
analysis and AC analysis.  The engine is designed to do true mixed-mode
simulation.")
    (license license:gpl3+)))

(define-public radare2-for-cutter
  (package
    (inherit radare2)
    (name "radare2")
    (version "5.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/radareorg/radare2")
                    (commit version)))
              (sha256
               (base32
                "0aa7c27kd0l55fy5qfvxqmakp4pz6240v3hn84095qmqkzcbs420"))
              (file-name (git-file-name name version))))))

(define-public cutter
  (package
    (name "cutter")
    (version "1.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/radareorg/cutter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ljj3j3apbbw628n2nyrxpbnclixx20bqjxm0xwggqzz9vywsar0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (radare2 (assoc-ref inputs "radare2")))
               ;; Fix pkg-config detection ./src/lib_radare2.pri:PREFIX=/usr/lib
               ;; override `qmake PREFIX=`.
               (substitute* "./src/lib_radare2.pri"
                 (("PREFIX") "R2PREFIX")
                 (("R2PREFIX=/usr") (string-append "R2PREFIX=" radare2)))
               (invoke "qmake"
                       (string-append "PREFIX=" out)
                       "./src/Cutter.pro")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list qtbase-5
           qtsvg
           openssl
           ;; Depends on radare2 4.5.1 officially, builds and works fine with
           ;; radare2 5.0.0 but fails to build with radare2 5.1.1.
           radare2-for-cutter))
    (home-page "https://github.com/radareorg/cutter")
    (synopsis "GUI for radare2 reverse engineering framework")
    (description "Cutter is a GUI for radare2 reverse engineering framework.
Its goal is making an advanced andcustomizable reverse-engineering platform
while keeping the user experience at mind.  Cutter is created by reverse
engineers for reverse engineers.")
    (license (list license:cc-by-sa3.0  ;the "Iconic" icon set
                   license:gpl3+))))    ;everything else

(define-public lib3mf
  (package
    (name "lib3mf")
    (version "2.1.1")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference (url "https://github.com/3MFConsortium/lib3mf")
                          (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "1417xlxc1y5jnipixhbjfrrjgkrprbbraj8647sff9051m3hpxc3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DUSE_INCLUDED_ZLIB=0"
                               "-DUSE_INCLUDED_LIBZIP=0"
                               "-DUSE_INCLUDED_GTEST=0"
                               "-DUSE_INCLUDED_SSL=0")))
    (native-inputs
     (list googletest pkg-config))
    (inputs
     `(("libuuid" ,util-linux "lib")
       ("libzip" ,libzip)
       ("libressl" ,libressl)
       ("zlib" ,zlib)))
    (synopsis "Implementation of the 3D Manufacturing Format (3MF) file standard")
    (description
     "Lib3MF is a C++ implementation of the 3D Manufacturing Format (3MF) file
standard.  It offers a way to integrate 3MF reading and writing capabilities, as
well as conversion and validation tools for input and output data.  The
specification can be downloaded at @url{http://3mf.io/specification/}.")
    (home-page "https://3mf.io/")
    (license license:bsd-2)))

(define-public openscad
  (package
    (name "openscad")
    (version "2021.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.openscad.org/openscad-" version
                           ".src.tar.gz"))
       (sha256
        (base32
         "0n83szr88h8snccjrslr96mgw3f65x3sq726n6x5vxp5wybw4f6r"))))
    (build-system cmake-build-system)
    (inputs
     `(("boost" ,boost)
       ("cgal" ,cgal)
       ("double-conversion" ,double-conversion)
       ("eigen" ,eigen)
       ("fontconfig" ,fontconfig)
       ("glew" ,glew)
       ("gmp" ,gmp)
       ("harfbuzz" ,harfbuzz)
       ("lib3mf" ,lib3mf)
       ("libxml2" ,libxml2)
       ("libzip" ,libzip)
       ("mpfr" ,mpfr)
       ("opencsg" ,opencsg)
       ("qscintilla" ,qscintilla)
       ("qtbase" ,qtbase-5)
       ("qtmultimedia" ,qtmultimedia)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("which" ,which)
       ;; the following are only needed for tests
       ("imagemagick" ,imagemagick)
       ("ps" ,procps)
       ("python" ,python)
       ("xvfb" ,xorg-server-for-tests)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake"
                     (string-append "PREFIX=" (assoc-ref outputs "out")))
             #t))
         (replace 'check
           (lambda _
             (with-directory-excursion "tests"
               (invoke "cmake" ".")
               (invoke "make")
               (invoke "ctest" "--exclude-regex"
                       (string-join
                        (list
                         "astdumptest_allexpressions"
                         "echotest_function-literal-compare"
                         "echotest_function-literal-tests"
                         "echotest_allexpressions"
                         "lazyunion-*"
                         "pdfexporttest_centered"
                         "pdfexporttest_simple-pdf"

                         ;; Broken due since cgal@5.2 +
                         ;; https://github.com/CGAL/cgal/pull/5371 (security)
                         ;; FIXME: Investigate or wait for future releases to
                         ;; fix it.
                         ;; Unsure if wrong test-suite or wrong security
                         ;; patch.
                         "cgalpngtest_nef3_broken"
                         "opencsgtest_nef3_broken"
                         "csgpngtest_nef3_broken"
                         "throwntogethertest_nef3_broken")
                        "|")))
             ;; strip python test files since lib dir ends up in out/share
             (for-each delete-file
                       (find-files "libraries/MCAD" ".*\\.py"))
             #t)))))
    (synopsis "Script-based 3D modeling application")
    (description
     "OpenSCAD is a 3D Computer-aided Design (CAD) application.  Unlike an
interactive modeler, OpenSCAD generates 3D models from a script, giving you
full programmatic control over your models.")
    (home-page "https://www.openscad.org/")
    (license license:gpl2+)))

(define-public emacs-scad-mode
  (package
    (inherit openscad)
    (name "emacs-scad-mode")
    (native-inputs '())
    (inputs '())
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-elisp
           ;; Elisp directory is not in root of the source.
           (lambda _
             (chdir "contrib")
             #t)))))
    (synopsis "Emacs major mode for editing editing OpenSCAD code")
    (description "@code{scad-mode} provides an Emacs major mode for editing
OpenSCAD code.  It supports syntax highlighting, indenting and refilling of
comments.")))

(define-public freecad
  (package
    (name "freecad")
    (version "0.19.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FreeCAD/FreeCAD")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dkiwnqr6bhi2d90hz7ijqd872144c9n9xxpd1vbrmxr2x8cfl88"))
       (patches (search-patches "freecad-vtk9.patch"
                                "freecad-boost-serialization.patch"))))
    (build-system qt-build-system)
    (native-inputs
     (list doxygen
           graphviz
           qttools
           pkg-config
           python-pyside-2-tools
           swig))
    (inputs
     (list boost
           coin3D
           double-conversion
           eigen
           freetype
           gl2ps
           glew
           hdf5-1.10
           jsoncpp
           libarea
           libjpeg-turbo
           libmedfile
           libspnav
           libtheora
           libtiff
           libxi
           libxml++
           libxmu
           lz4
           netcdf
           opencascade-occt
           openmpi
           proj
           python-gitpython
           python-matplotlib
           python-pivy
           python-ply
           python-pyside-2
           python-pyyaml
           python-shiboken-2
           python-wrapper
           qtbase-5
           qtdeclarative
           qtsvg
           qtwebchannel
           qtwebengine
           qtx11extras
           qtxmlpatterns
           sqlite
           tbb
           vtk
           xerces-c
           zlib))
    (arguments
     `(#:tests? #f          ; Project has no tests
       #:configure-flags
       ,#~(list
           "-DBUILD_QT5=ON"
           "-DBUILD_FLAT_MESH:BOOL=ON"
           "-DBUILD_ENABLE_CXX_STD:STRING=C++17"
           (string-append "-DCMAKE_INSTALL_LIBDIR=" #$output "/lib")
           (string-append "-DPYSIDE2UICBINARY="
                          #$(this-package-native-input
                             "python-pyside-2-tools")
                          "/bin/uic")
           (string-append "-DPYSIDE2RCCBINARY="
                          #$(this-package-native-input
                             "python-pyside-2-tools")
                          "/bin/rcc")
           "-DPYSIDE_LIBRARY=PySide2::pyside2"
           (string-append
            "-DPYSIDE_INCLUDE_DIR="
            #$(this-package-input "python-pyside-2") "/include;"
            #$(this-package-input "python-pyside-2") "/include/PySide2;"
            #$(this-package-input "python-pyside-2") "/include/PySide2/QtCore;"
            #$(this-package-input "python-pyside-2") "/include/PySide2/QtWidgets;"
            #$(this-package-input "python-pyside-2") "/include/PySide2/QtGui;")
           "-DSHIBOKEN_LIBRARY=Shiboken2::libshiboken"
           (string-append "-DSHIBOKEN_INCLUDE_DIR="
                          #$(this-package-input "python-shiboken-2")
                          "/include/shiboken2"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'restore-pythonpath
           (lambda _
             (substitute* "src/Main/MainGui.cpp"
               (("_?putenv\\(\"PYTHONPATH=\"\\);") ""))))
         (add-after 'install 'wrap-pythonpath
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/FreeCAD")
                 (list "GUIX_PYTHONPATH"
                       'prefix (list (getenv "GUIX_PYTHONPATH"))))))))))
    (home-page "https://www.freecadweb.org/")
    (synopsis "Your Own 3D Parametric Modeler")
    (description
     "FreeCAD is a general purpose feature-based, parametric 3D modeler for
CAD, MCAD, CAx, CAE and PLM, aimed directly at mechanical engineering and
product design but also fits a wider range of uses in engineering, such as
architecture or other engineering specialties.  It is 100% Open Source (LGPL2+
license) and extremely modular, allowing for very advanced extension and
customization.")
    (license
     (list
      license:lgpl2.1+
      license:lgpl2.0+
      license:gpl3+
      license:bsd-3))))

(define-public libmedfile
  (package
    (name "libmedfile")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.salome-platform.org/Salome/other/med-"
             version ".tar.gz"))
       (sha256
        (base32
         "017h9p0x533fm4gn6pwc8kmp72rvqmcn6vznx72nkkl2b05yjx54"))))
    (build-system cmake-build-system)
    (inputs (list hdf5-1.10))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-test-output
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (delete-file-recursively
                (string-append out "/bin/testc"))
               #t))))))
    (home-page "https://www.salome-platform.org")
    (synopsis "Library to read and write MED files")
    (description
     "The purpose of the MED module is to provide a standard for storing and
recovering computer data associated to numerical meshes and fields, and to
facilitate the exchange between codes and solvers.

The persistent data storage is based upon HDF format (like CGNS, a standard
developed by Boeing and NASA in the area of Computational Fluid Dynamic).

MED also provides structures to hold data on meshes and fields.  These
structures are exchanged between solvers, hide the communication level (CORBA
or MPI), and offer persistence (read/write in .med files).

The main benefit of a common exchange format is reduced complexity of code
coupling.  It also allows sharing such high level functionalities as
computation of nodal connectivity of sub-elements (faces and edges),
arithmetic operations on fields, entity location functionalities, and
interpolation toolkit.")
    (license license:gpl3+)))

(define-public libarea
  (let ((revision "1")
        (commit "8f8bac811c10f1f01fda0d742a18591f61dd76ee"))
    (package
      (name "libarea")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url "https://github.com/Heeks/libarea")
                             (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0pvqz6cabxqdz5y26wnj6alkn8v5d7gkx0d3h8xmg4lvy9r3kh3g"))))
      (build-system gnu-build-system)
      (inputs (list boost python-wrapper))
      (native-inputs
       `(("cmake" ,cmake-minimal)))
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'configure 'cmake-configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (cmake (assoc-ref inputs "cmake")))
                 (mkdir-p "build")
                 (invoke "cmake"
                         (string-append "-DCMAKE_INSTALL_PREFIX=" out)))))
           (delete 'configure))))
      (home-page "https://github.com/Heeks/libarea")
      (synopsis
       "Library and python module for pocketing and profiling operations")
      (description
       "Area is a CAM-related software for pocketing operation.

This project provides library and associated python-module to compute pocket
operations.")
      (license (list
                license:bsd-3
                license:gpl3+)))))

(define-public libspnav
  (package
    (name "libspnav")
    (version "0.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FreeSpacenav/libspnav")
                    (commit (string-append "libspnav-" version))))
              (sha256
               (base32
                "098h1jhlj87axpza5zgy58prp0zn94wyrbch6x0s7q4mzh7dc8ba"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     (list libx11))
    (arguments `(#:tests? #f))
    (home-page "http://spacenav.sourceforge.net/")
    (synopsis
     "Library for communicating with spacenavd or 3dxsrv")
    (description
     "The libspnav library is provided as a replacement of the magellan
library.  It provides a cleaner, and more orthogonal interface.  libspnav
supports both the original X11 protocol for communicating with the driver, and
the new alternative non-X protocol.  Programs that choose to use the X11
protocol, are automatically compatible with either the free spacenavd driver
or the official 3dxserv, as if they were using the magellan SDK.

Also, libspnav provides a magellan API wrapper on top of the new API.  So, any
applications that were using the magellan library, can switch to libspnav
without any changes.  And programmers that are familiar with the magellan API
can continue using it with a free library without the restrictions of the
official SDK.")
    (license license:bsd-3)))

(define-public openctm
  (let ((revision 603))
    ;; Previous versions don't compile, they need to link libGL and libGLU.
    ;; Fixed in this revision.
    (package
      (name "openctm")
      (version (string-append "1.0.3." (number->string revision)))
      (source
       (origin
         (method svn-fetch)
         (uri (svn-reference
               (url "https://svn.code.sf.net/p/openctm/code/trunk")
               (revision revision)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32 "01wb70m48xh5gwhv60a5brv4sxl0i0rh038w32cgnlxn5x86s9f1"))))
      (build-system gnu-build-system)
      (native-inputs
       (list pkg-config))
      (inputs
       `(("mesa" ,mesa)
         ("glu" ,glu)
         ("glut" ,freeglut)
         ("gtk" ,gtk+-2)))
      (arguments
       `(#:tests? #f                              ;no tests
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (rename-file "Makefile.linux" "Makefile")
               (let ((out (assoc-ref outputs "out")))
                 ;; Create output directories.
                 (mkdir-p (string-append out "/lib"))
                 (mkdir-p (string-append out "/include"))
                 (mkdir-p (string-append out "/bin"))
                 ;; Fix rpath.
                 (substitute* "tools/Makefile.linux"
                   (("-rpath,\\.")
                    (string-append "-rpath," out "/lib/"))
                   (("/usr/local")
                    out))
                 ;; Set right output.
                 (substitute* "Makefile"
                   (("/usr/lib")
                    (string-append out "/lib"))
                   (("\\/usr\\/local")
                    out))
                 #t))))))
      (synopsis "3D triangle mesh format and related tools and libraries")
      (description "OpenCTM is a file format, a software library and a tool set
for compression of 3D triangle meshes.  The geometry is compressed to a
fraction of comparable file formats (3DS, STL, COLLADA...), and the format is
accessible through a simple API")
      (license license:zlib)
      (home-page "http://openctm.sourceforge.net/"))))

(define-public lib3ds
  (package
    (name "lib3ds")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://storage.googleapis.com/google-code-archive-downloads"
             "/v2/code.google.com/lib3ds/lib3ds-" version ".zip"))
       (sha256
        (base32 "1qr9arfdkjf7q11xhvxwzmhxqz3nhcjkyb8zzfjpz9jm54q0rc7m"))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip))
    (home-page "https://code.google.com/archive/p/lib3ds")
    (synopsis "3DS format file toolkit")
    (description "Lib3ds is a toolkit for handling the 3DS format for 3D
model files.  Its main goal is to simplify the creation of 3DS import and
export filters.")
    (license license:lgpl2.1+)))

(define-public meshlab
  (package
    (name "meshlab")
    (version "2020.06")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cnr-isti-vclab/meshlab")
                    (commit (string-append "Meshlab-" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1cgx24wxh2ah5pff51rcrk6x8qcdjpkxcdak7s4cfzmxvjlshydd"))))
    (build-system cmake-build-system)
    (inputs
     (list qtbase-5
           qtscript
           qtxmlpatterns
           mesa
           glu
           glew
           muparser
           gmp
           eigen
           libfreenect
           lib3ds
           openctm
           ;; FIXME: Compilation fails with system qhull:
           ;; https://github.com/cnr-isti-vclab/meshlab/issues/678
           ;; ("qhull" ,qhull)
           ))
    (arguments
     `(#:tests? #f                                ; Has no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'go-to-source-dir
           (lambda _ (chdir "src") #t))
         (add-after 'install 'move-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out")
                                       "/lib")))
               (rename-file
                (string-append lib "/meshlab/libmeshlab-common.so")
                (string-append lib "/libmeshlab-common.so"))
               #t))))))
    (synopsis "3D triangular mesh processing and editing software")
    (home-page "https://www.meshlab.net/")
    (description "MeshLab is a system for the processing and
editing of unstructured large 3D triangular meshes.  It is aimed to help the
processing of the typical not-so-small unstructured models arising in 3D
scanning, providing a set of tools for editing, cleaning, healing, inspecting,
rendering and converting this kind of meshes.  These tools include MeshLab
proper, a versatile program with a graphical user interface, and meshlabserver,
a program that can perform mesh processing tasks in batch mode, without a
GUI.")
    (license license:gpl3+)))

(define-public poke
  (package
    (name "poke")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/poke/poke-" version
                                  ".tar.gz"))
              (sha256
               (base32 "095a0qal1fwnqxnal0xb4mp0n4zy97j3ww1j04ij3jb0jpr4s1ff"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete files generated by Bison.
                  (delete-file "gl/parse-datetime.c")
                  (delete-file "gl/parse-datetime-gen.h")
                  (delete-file "jitter/example-vms/structured/structured-parser.c")
                  (delete-file "jitter/example-vms/structured/structured-parser.h")
                  (delete-file "jitter/jitterc/jitterc-parser.c")
                  (delete-file "jitter/jitterc/jitterc-parser.h")
                  (delete-file "jitter/jitter/jitter-routine-parser.c")
                  (delete-file "jitter/jitter/jitter-routine-parser.h")
                  (delete-file "libpoke/pkl-tab.c")
                  (delete-file "libpoke/pkl-tab.h")
                  (delete-file "poke/pk-map-tab.c")
                  (delete-file "poke/pk-map-tab.h")
                  ;; Delete files generated by flex.
                  (delete-file "jitter/example-vms/structured/structured-scanner.c")
                  (delete-file "jitter/example-vms/structured/structured-scanner.h")
                  (delete-file "jitter/jitterc/jitterc-scanner.c")
                  (delete-file "jitter/jitterc/jitterc-scanner.h")
                  (delete-file "jitter/jitter/jitter-routine-scanner.c")
                  (delete-file "jitter/jitter/jitter-routine-scanner.h")
                  (delete-file "libpoke/pkl-lex.c")
                  (delete-file "libpoke/pkl-lex.h")
                  (delete-file "poke/pk-map-lex.c")
                  (delete-file "poke/pk-map-lex.h")
                  ;; Other generated files:
                  (delete-file "jitter/example-vms/jitterlisp/jitterlispvm-vm1.c")
                  (delete-file "jitter/example-vms/jitterlisp/jitterlispvm-vm2.c")
                  (delete-file "jitter/example-vms/jitterlisp/jitterlispvm-vm.h")
                  (delete-file "jitter/example-vms/structured/structuredvm-vm1.c")
                  (delete-file "jitter/example-vms/structured/structuredvm-vm2.c")
                  (delete-file "jitter/example-vms/structured/structuredvm-vm.h")
                  (delete-file "jitter/example-vms/structured/structuredvm-vm-main.c")
                  (delete-file "jitter/example-vms/uninspired/uninspired-vm1.c")
                  (delete-file "jitter/example-vms/uninspired/uninspired-vm2.c")
                  (delete-file "jitter/example-vms/uninspired/uninspired-vm.h")
                  (delete-file "jitter/example-vms/uninspired/uninspired-vm-main.c")
                  (delete-file "libpoke/pvm-vm.h")
                  (delete-file "libpoke/pvm-vm1.c")
                  (delete-file "libpoke/pvm-vm2.c")))))
    (build-system gnu-build-system)
    ;; The GUI, which we elide, requires tcl and tk.
    (native-inputs (list bison dejagnu flex libtool pkg-config))
    ;; FIXME: Enable NBD support by adding `libnbd' (currently unpackaged).
    (inputs (list json-c libgc readline libtextstyle))
    (arguments
     ;; To build the GUI, add the `--enable-gui' configure flag.
     ;; To enable the "hyperlink server", add the `--enable-hserver' flag.
     `(#:configure-flags '("--enable-mi")))
    (home-page "https://www.gnu.org/software/poke/#documentation")
    (synopsis "Editing of arbitrary binary data")
    (description "GNU poke is an interactive, extensible editor for binary data.
Not limited to editing basic entities such as bits and bytes, it provides a
full-fledged procedural, interactive programming language designed to describe
data structures and to operate on them.")
    (license license:gpl3+)))

(define-public pcb2gcode
    (package
     (name "pcb2gcode")
     (version "2.1.0")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pcb2gcode/pcb2gcode")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0nzglcyh6ban27cc73j4l7w7r9k38qivq0jz8iwnci02pfalw4ry"))))
     (build-system gnu-build-system)
     (inputs
      (list boost
            geos
            gerbv
            glibmm
            gtkmm-2
            librsvg))
     (native-inputs
      (list autoconf automake libtool pkg-config))
     (home-page "https://github.com/pcb2gcode/pcb2gcode")
     (synopsis "Generate G-code for milling PCBs")
     (description "pcb2gcode is a command-line program for isolation routing
and drilling of PCBs.  It takes Gerber files as input and outputs G-code files
for the milling of PCBs.  It also includes an autoleveller for the automatic
dynamic calibration of the milling depth.")
     (license license:gpl3+)))

(define-public syscall-intercept
  ;; Upstream provides no tag. Also, last version update is 4 years old.
  (let ((commit "304404581c57d43478438d175099d20260bae74e")
        (revision "0"))
    (package
      (name "syscall-intercept")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/pmem/syscall_intercept/")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17sw78xp5wjzv25adpbq3khl8fi0avj7bgpi57q3jnvl3c68xy5z"))))
      (native-inputs
       (list perl pkg-config))
      (inputs
       (list capstone))
      (build-system cmake-build-system)
      (arguments
       `(#:build-type "Release"
         ;; FIXME: "syscall_format_logging" test fails.
         #:tests? #f))
      (home-page "https://github.com/pmem/syscall_intercept")
      (synopsis "System call intercepting library")
      (description
       "The system call intercepting library provides a low-level interface
for hooking Linux system calls in user space.  This is achieved by
hot-patching the machine code of the standard C library in the memory of
a process.")
      (license license:bsd-2))))

(define-public xfoil
  (package
    (name "xfoil")
    (version "6.99")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://web.mit.edu/drela/Public/web/xfoil/xfoil"
                           version ".tgz"))
       (sha256
        (base32
         "0h5y5v0qlyvi4qc943x394npz4779i8f52iksxshxkjj7xj500jw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'edit-files
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The instructions in orrs/README say that orrs/bin/osmap.f
             ;; should be edited, but that file is never used by XFOIL.
             ;; Instead, it is osrc/osmap.f that is used.
             (substitute* "osrc/osmap.f"
               (("^[ ]{6}DATA OSFILE / '/var/local/codes/orrs/osmap.dat' /")
                (let ((replacement (string-append (make-string 6 #\space)
                                                  "DATA OSFILE / '"
                                                  (assoc-ref outputs "out")
                                                  "/share/xfoil/osmap.dat' /")))
                  ;; In fixed form Fortran, lines cannot exceed 72 columns.
                  ;; The Guix store path exceeds this limit.
                  (string-append
                    (substring replacement 0 72) "\n"
                    (make-string 5 #\space) "&" (substring replacement 72)))))
             (substitute* "orrs/bin/Makefile_DP"
               (("^FC = ifort")
                "FC = gfortran")
               (("^FLG = -O -r8")
                "FLG = -O2 -fdefault-real-8"))
             (substitute* "plotlib/Makefile"
               (("^include ./config.make")
                "include ./config.make.gfortranDP"))
             (substitute* "bin/Makefile_gfortran"
               (("^BINDIR = /home/codes/bin/")
                (string-append "BINDIR = " (assoc-ref outputs "out") "/bin"))
               (("^CC = cc")
                "CC = gcc")
               (("^CFLAGS = -O -DUNDERSCORE")
                "CFLAGS = -O2 -DUNDERSCORE")
               (("^FFLAGS = -O \\$\\(CHK\\) \\$\\(DBL\\)")
                "FFLAGS = -O2 $(CHK) $(DBL)")
               (("^FFLOPT = -O \\$\\(CHK\\) \\$\\(DBL\\)")
                "FFLOPT = -O2 $(CHK) $(DBL)")
               ;; Separate the build stage from the install stage.
               (("\\$\\(INSTALLCMD\\) xfoil \\$\\(BINDIR\\)") "")
               (("\\$\\(INSTALLCMD\\) pxplot \\$\\(BINDIR\\)") "")
               (("\\$\\(INSTALLCMD\\) pplot \\$\\(BINDIR\\)") ""))))
         (replace 'build
           (lambda _
             (invoke "make" "-C" "orrs/bin" "-f" "Makefile_DP" "osgen")
             (with-directory-excursion "orrs"
               (invoke "bin/osgen" "osmaps_ns.lst"))
             (invoke "make" "-C" "plotlib")
             (substitute* "bin/Makefile_gfortran"
               (("^FFLAGS =(.*)$" _ suffix)
                (string-append "FFLAGS = -fallow-argument-mismatch "
                               suffix "\n")))
             (invoke "make" "-C" "bin" "-f" "Makefile_gfortran")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin-dir (string-append out "/bin"))
                    (data-dir (string-append out "/share/xfoil"))
                    (doc-dir (string-append out "/share/doc/xfoil")))
               (mkdir-p bin-dir)
               (invoke "make" "-C" "bin" "-f" "Makefile_gfortran" "install")
               (mkdir-p data-dir)
               (install-file "orrs/osmap.dat" data-dir)
               (mkdir-p doc-dir)
               (install-file "xfoil_doc.txt" doc-dir)))))
       #:tests? #f))
    (inputs
     (list libx11))
    (native-inputs
     (list gfortran))
    (home-page "https://web.mit.edu/drela/Public/web/xfoil/")
    (synopsis "Program for the design and analysis of subsonic airfoils")
    (description
     "XFOIL is an interactive program for the design and analysis of subsonic
isolated airfoils.  It consists of a collection of menu-driven routines which
perform various useful functions such as:
@itemize
@item Viscous (or inviscid) analysis of an existing airfoil
@item Airfoil design and redesign by interactive modification of surface speed
      distributions
@item Airfoil redesign by interactive modification of geometric parameters
@item Blending of airfoils
@item Writing and reading of airfoil coordinates and polar save files
@item Plotting of geometry, pressure distributions, and multiple polars
@end itemize")
    (license license:gpl2+)))

(define-public libigl
  (package
    (name "libigl")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libigl/libigl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "004a22ifq2vibgkgvrlyihqimpsfizvq5l448204kwfg3lkycajj"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DLIBIGL_USE_STATIC_LIBRARY=OFF"
         "-DLIBIGL_BUILD_TESTS=ON"
         "-DLIBIGL_BUILD_TUTORIALS=OFF"
         "-DLIBIGL_EXPORT_TARGETS=ON"
         ;; The following options disable tests for the corresponding libraries.
         ;; The options do not affect whether the libraries are linked to
         ;; libigl or not, they are used for tests.
         "-DLIBIGL_WITH_COMISO=OFF"
         "-DLIBIGL_WITH_CORK=OFF"
         "-DLIBIGL_WITH_MATLAB=OFF"
         "-DLIBIGL_WITH_MOSEK=OFF"
         "-DLIBIGL_WITH_TRIANGLE=OFF" ;; Undefined reference to "triangulate".
         "-DLIBIGL_WITH_OPENGL_GLFW_IMGUI=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-external
           (lambda _
             (setenv "HOME" (getcwd)) ;; cmake needs this to export modules
             (mkdir "external")
             (copy-recursively (assoc-ref %build-inputs "libigl-glad") "external/glad")
             (copy-recursively (assoc-ref %build-inputs "libigl-stb") "external/stb")
             (copy-recursively (assoc-ref %build-inputs "libigl-tetgen") "external/tetgen")
             (copy-recursively (assoc-ref %build-inputs "libigl-predicates") "external/predicates")))
         (add-after 'unpack-external 'patch-cmake
           (lambda _
             ;; Fix references to external libraries
             (substitute* "cmake/libigl.cmake"
               (("if\\(NOT TARGET Eigen3::Eigen\\)" all)
                (string-append "find_package(Eigen3 CONFIG REQUIRED)\n" all))
               (("if\\(NOT TARGET CGAL::CGAL\\)" all)
                (string-append "find_package(CGAL CONFIG COMPONENTS Core)\n" all))
               (("if\\(NOT TARGET tinyxml2\\)" all)
                (string-append "find_package(tinyxml2 CONFIG REQUIRED)\n"
                               "if (NOT TARGET tinyxml2::tinyxml2)"))
               (("if\\(NOT TARGET embree\\)" all)
                (string-append "find_package(embree 3 CONFIG REQUIRED)\n" all))
               (("if\\(NOT TARGET glfw\\)" all)
                (string-append "find_package(glfw3 CONFIG REQUIRED)\n" all))
               (("igl_download_glad\\(\\)" all) "")
               (("igl_download_stb\\(\\)" all) "")
               (("igl_download_tetgen\\(\\)" all) "")
               (("igl_download_triangle\\(\\)" all) "")
               (("igl_download_predicates\\(\\)" all) ""))
             (substitute* "tests/CMakeLists.txt"
               (("igl_download_test_data\\(\\)") "")
               (("set\\(IGL_TEST_DATA.*")
                (format #f "set(IGL_TEST_DATA ~a)\n"
                        (assoc-ref %build-inputs "libigl-test-data")))
               (("igl_download_catch2\\(\\)") "find_package(Catch2 CONFIG REQUIRED)")
               (("list\\(APPEND CMAKE_MODULE_PATH \\$\\{LIBIGL_EXTERNAL\\}/catch2/contrib\\)")
                "")
               (("add_subdirectory\\(\\$\\{LIBIGL_EXTERNAL\\}/catch2 catch2\\)") ""))
             ;; Install otherwise missing headers
             (substitute* "cmake/libigl.cmake"
               (("install_dir_files\\(copyleft\\)" all)
                (string-join (list all
                                   "install_dir_files(copyleft/cgal)"
                                   "install_dir_files(copyleft/opengl)"
                                   "install_dir_files(copyleft/tetgen)"
                                   "install_dir_files(embree)"
                                   "install_dir_files(opengl)"
                                   "install_dir_files(png)"
                                   "install_dir_files(predicates)"
                                   "install_dir_files(xml)")
                             "\n"))))))))
    ;; XXX: Inputs are currently only used to build tests.
    ;;      We would need to patch the CMake recipe to build a shared library
    ;;      with all of these.
    (inputs
     `(("boost" ,boost)
       ("catch2" ,catch-framework2)
       ("cgal" ,cgal)
       ("eigen" ,eigen)
       ("embree" ,embree)
       ("glfw" ,glfw)
       ("gmp" ,gmp)
       ("mesa" ,mesa)
       ("mpfr" ,mpfr)
       ("tinyxml2" ,tinyxml2)
       ;; When updating this package, update commit fields below according to
       ;; the hashes listed in "cmake/LibiglDownloadExternal.cmake".
       ("libigl-test-data"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/libigl/libigl-tests-data")
                 (commit "19cedf96d70702d8b3a83eb27934780c542356fe")))
           (file-name (git-file-name "libigl-test-data" version))
           (sha256 (base32 "1wxglrxw74xw4a4jmmjpm8719f3mnlbxbwygjb4ddfixxxyya4i2"))))
       ("libigl-glad"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/libigl/libigl-glad")
                 (commit "09b4969c56779f7ddf8e6176ec1873184aec890f")))
           (file-name (git-file-name "libigl-glad" version))
           (sha256 (base32 "0rwrs7513ylp6gxv7crjzflapcg9p7x04nzfvywgl665vl53rawk"))))
       ("libigl-stb"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/libigl/libigl-stb.git")
                 (commit "cd0fa3fcd90325c83be4d697b00214e029f94ca3")))
           (file-name (git-file-name "libigl-stb" version))
           (sha256 (base32 "0wwlb370z40y63ic3ny6q7lxibhixg2k1pjdkl4ymzv79zld28kj"))))
       ("libigl-predicates"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/libigl/libigl-predicates.git")
                 (commit "488242fa2b1f98a9c5bd1441297fb4a99a6a9ae4")))
           (file-name (git-file-name "libigl-predicates" version))
           (sha256 (base32 "13bd98g8lgcq37i3crj66433z09grnb2xjrcqpwqmyn147rp5wyh"))))
       ;; TODO: Package tetgen separately from <http://www.tetgen.org>
       ("libigl-tetgen"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/libigl/tetgen.git")
                 (commit "4f3bfba3997f20aa1f96cfaff604313a8c2c85b6")))
           (file-name (git-file-name "libigl-tetgen" version))
           (sha256 (base32 "1k724syssw37py7kwmibk3sfwkkgyjyy7qkijnhn6rjm91g8qxsg"))))))
    (home-page "https://libigl.github.io/")
    (synopsis "Simple C++ geometry processing library")
    (description "This library provides functionality for shape modelling,
visualization, matrix manipulation.")
    (license (list license:gpl3 license:mpl2.0))))

(define-public prusa-slicer
  (package
    (name "prusa-slicer")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/prusa3d/PrusaSlicer")
         (commit (string-append "version_" version))))
       (file-name (git-file-name name version))
       (sha256 (base32 "1mb7v0khrmsgy3inmh4mjn709jlhx422kvbnrhsqziph2wwak9bz"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Prusa slicer bundles a lot of dependencies in src/ directory.
           ;; Most of them contain prusa-specific modifications (e.g. avrdude),
           ;; but others do not. Here we replace the latter with Guix packages.
           ;; Remove bundled libraries that were not modified by Prusa Slicer developers.
           (delete-file-recursively "src/hidapi")
           (delete-file-recursively "src/eigen")
           (delete-file-recursively "src/libigl/igl")
           (substitute* "src/CMakeLists.txt"
             (("add_subdirectory\\(libigl\\)" all)
              (string-append
               all "\ninclude_directories(libigl INTERFACE libigl::core)"))
             (("add_subdirectory\\(hidapi\\)")
              "pkg_check_modules(HIDAPI REQUIRED hidapi-hidraw)")
             (("include_directories\\(hidapi/include\\)")
              "include_directories()"))
           (substitute* "src/slic3r/CMakeLists.txt"
             (("add_library\\(libslic3r_gui.*" all)
              (string-append
               all
               "\ntarget_include_directories(libslic3r_gui PUBLIC ${HIDAPI_INCLUDE_DIRS})\n"))
             (("\\bhidapi\\b") "${HIDAPI_LIBRARIES}"))))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DSLIC3R_FHS=1" ;; Use The Filesystem Hierarchy Standard.
         "-DSLIC3R_GTK=3" ;; Use GTK+
         ;; Use wxWidgets 3.0.x.x to prevent GUI crashes when adding support enforcers.
         "-DSLIC3R_WX_STABLE=1")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list boost
           cereal
           cgal
           curl
           dbus
           eigen
           expat
           glew
           glib
           gmp
           gtk+
           hidapi
           ilmbase
           libigl
           libpng
           mesa
           mpfr
           nlopt
           openvdb
           pango
           tbb
           eudev
           wxwidgets
           zlib))
    (home-page "https://www.prusa3d.com/prusaslicer/")
    (synopsis "G-code generator for 3D printers (RepRap, Makerbot, Ultimaker etc.)")
    (description "PrusaSlicer takes 3D models (STL, OBJ, AMF) and converts them into
G-code instructions for FFF printers or PNG layers for mSLA 3D printers.")
    (license license:agpl3)

    ;; Mark as tunable to take advantage of SIMD code in Eigen and in libigl.
    (properties '((tunable? . #t)))))
