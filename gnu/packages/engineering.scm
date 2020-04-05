;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018, 2019 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Tim Stahel <swedneck@swedneck.xyz>
;;; Copyright © 2019 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2019 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
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
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fpga)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
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
  #:use-module (gnu packages linux)               ;FIXME: for pcb
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public librecad
  (package
    (name "librecad")
    (version "2.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/LibreCAD/LibreCAD/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01nvc1g3si05r5np1pzn62ah9w84p8nxa32wqrjh6gdi17jfvi3l"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
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
             #t))
         ;; Ensure that icons are found at runtime
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (qt '("qtbase" "qtsvg")))
               (wrap-program (string-append out "/bin/librecad")
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/plugins/"))
                         qt)))
               #t))))))
    (inputs
     `(("boost" ,boost)
       ("muparser" ,muparser)
       ("freetype" ,freetype)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (home-page "https://librecad.org/")
    (synopsis "Computer-aided design (CAD) application")
    (description
     "LibreCAD is a 2D Computer-aided design (CAD) application for creating
plans and designs.")
    (license license:gpl2)))

(define-public geda-gaf
  (package
    (name "geda-gaf")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ftp.geda-project.org/geda-gaf/stable/v"
                    (version-major+minor version) "/"
                    version "/geda-gaf-" version ".tar.gz"))
              (sha256
               (base32
                "06ivgarvwbzjz2wigxzzkm8iszldi2p6x3a6jnlczjyrz4csddsy"))))
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
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("perl" ,perl))) ; for tests
    (home-page "http://geda-project.org/")
    (synopsis "Schematic capture, netlister, symbols, symbol checker, and utils")
    (description
     "Gaf stands for “gschem and friends”.  It is a subset of the entire tool
suite grouped together under the gEDA name.  gEDA/gaf is a collection of tools
which currently includes: gschem, a schematic capture program; gnetlist, a
netlist generation program; gsymcheck, a syntax checker for schematic symbols;
gattrib, a spreadsheet programm that manipulates the properties of symbols of
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
    (version "1.9.9-20191003")
    (home-page "https://github.com/lepton-eda/lepton-eda")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (sha256
               (base32
                "08cc3zfk84qq9mrkc9pp4r9jlavvm01wwy0yd9frql68w2zw6mip"))
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
       ("gtk" ,gtk+-2)
       ("guile" ,guile-2.2)
       ("shared-mime-info" ,shared-mime-info)
       ("m4" ,m4)
       ("pcb" ,pcb)))
    (arguments
     (substitute-keyword-arguments (package-arguments geda-gaf)
       ((#:configure-flags flags ''())
        ;; When running "make", the POT files are built with the build time as
        ;; their "POT-Creation-Date".  Later on, "make" notices that .pot
        ;; files were updated and goes on to run "msgmerge"; as a result, the
        ;; non-deterministic POT-Creation-Date finds its way into .po files,
        ;; and then in .gmo files.  To avoid that, simply make sure 'msgmerge'
        ;; never runs.  See <https://bugs.debian.org/792687>.
        `(cons "ac_cv_path_MSGMERGE=true" ,flags))
       ((#:phases phases '%standard-phases)
        `(modify-phases %standard-phases
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
               (invoke "make" "precompile")
               (for-each (lambda (program)
                           (wrap-program program
                             `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                               (,(string-append (assoc-ref outputs "out")
                                                "/share/lepton-eda/ccache/")))))
                         (find-files (string-append (assoc-ref outputs "out") "/bin")
                                     ".*"))
               #t))))))
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
                    (path (string-append (assoc-ref inputs "udev") "/lib")))
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
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://repo.hu/projects/pcb-rnd/releases/"
                                  "pcb-rnd-" version ".tar.gz"))
              (sha256
               (base32
                "0pycynla60b96jkb6fh6f4sx663pqbzjwnixhw5ym8sym2absm09"))))
    (arguments
     `(#:tests? #f ; no check target
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
     `(("texlive" ,(texlive-union (list texlive-fonts-amsfonts)))
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
    (home-page "http://www.rle.mit.edu/cpg/research_codes.htm")
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
    (home-page "http://www.rle.mit.edu/cpg/research_codes.htm")
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
                    (url "https://github.com/fritzing/fritzing-app.git")
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
     `(("qtbase" ,qtbase)
       ("qtserialport" ,qtserialport)
       ("qtsvg" ,qtsvg)
       ("libgit2" ,libgit2)
       ("boost" ,boost)
       ("zlib" ,zlib)
       ("fritzing-parts-db"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/fritzing/fritzing-parts.git")
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
                      (url "https://github.com/erichVK5/translate2geda.git")
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
  (let ((commit "6e39254e57c179459bb929df49ae96a6017a0ed6")
        (revision "3"))
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
                  "0ryv2hcbrwqc087w7rrs4a2irkcpmqync00g4dh8n7jn10w2jkim"))
                (file-name (git-file-name name version))
                (snippet
                 ;; Remove bundled catch since we provide our own.
                 '(begin
                    (delete-file "libfive/test/catch.hpp")
                    #t))))
      (build-system cmake-build-system)
      (arguments
       `(#:test-target "libfive-test"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'remove-native-compilation
             (lambda _
               (substitute* "CMakeLists.txt" (("-march=native") ""))
               #t))
           (add-after 'unpack 'find-catch
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-append (assoc-ref inputs "catch")
                                      "/include/catch"))
               #t)))))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (inputs
       `(("boost" ,boost)
         ("catch" ,catch-framework2)
         ("libpng" ,libpng)
         ("qtbase" ,qtbase)
         ("eigen" ,eigen)
         ("guile" ,guile-2.2)))
      (home-page "https://libfive.com")
      (synopsis "Tool for programmatic computer-aided design")
      (description
       "Libfive is a tool for programmatic computer-aided design (CAD).  In
libfive, solid models are defined as Scheme scripts, and there are no opaque
function calls into the geometry kernel: everything is visible to the user.
Even fundamental, primitive shapes are represented as code in the user-level
language.")
      (license (list license:mpl2.0               ;library
                     license:gpl2+)))))           ;Guile bindings and GUI

;; TODO Add doc https://gitlab.com/kicad/services/kicad-doc/-/tree/master
(define-public kicad
  (package
    (name "kicad")
    (version "5.1.5")
    (source
     (origin
       (method url-fetch)
       (file-name (string-append name "-" version ".tar.xz"))
       (uri (string-append
             "https://launchpad.net/kicad/" (version-major version)
             ".0/" version "/+download/kicad-" version ".tar.xz"))
       (sha256
        (base32 "0x3417f2pa7p65s9f7l49rqbnrzy8gz6i0n07mlbxqbnm0fmlql0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:out-of-source? #t
       #:tests? #f                      ; no tests
       #:build-type "Release"
       #:configure-flags
       (list "-DKICAD_SCRIPTING_PYTHON3=ON"
             "-DKICAD_SCRIPTING_WXPYTHON_PHOENIX=ON"
             "-DCMAKE_BUILD_WITH_INSTALL_RPATH=TRUE")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-translations
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "kicad-i18l")
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
                           (getenv "PYTHONPATH"))))
               (wrap-program file
                 `("PYTHONPATH" ":" prefix (,path))
                 `("PATH" ":" prefix
                   (,(string-append python "/bin:")))))
             #t)))))
    (native-search-paths
     (list (search-path-specification
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
       ("kicad-i18l" ,kicad-i18l)
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
       ("opencascade-oce" ,opencascade-oce)
       ("openssl" ,openssl)
       ("python" ,python-wrapper)
       ("wxwidgets" ,wxwidgets)
       ("wxpython" ,python-wxpython)))
    (home-page "https://kicad-pcb.org/")
    (synopsis "Electronics Design Automation Suite")
    (description "Kicad is a program for the formation of printed circuit
boards and electrical circuits.  The software has a number of programs that
perform specific functions, for example, pcbnew (Editing PCB), eeschema (editing
electrical diagrams), gerbview (viewing Gerber files) and others.")
    (license license:gpl3+)))

(define kicad-i18l
  (package
    (name "kicad-i18l")
    (version "5.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/kicad/code/kicad-i18n.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rfpifl8vky1gba2angizlb2n7mwmsiai3r6ip6qma60wdj8sbd3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (home-page "https://kicad-pcb.org/")
    (synopsis "KiCad GUI translations")
    (description "This package contains the po files that are used for the GUI
translations for KiCad.")
    (license license:gpl3+)))

(define-public kicad-symbols
  (package
    (name "kicad-symbols")
    (version "5.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KiCad/kicad-symbols.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "048b07ffsaav1ssrchw2p870lvb4rsyb5vnniy670k7q9p16qq6h"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests exist
    (home-page "https://kicad-pcb.org/")
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
    (version "5.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KiCad/kicad-footprints.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c4whgn14qhz4yqkl46w13p6rpv1k0hsc9s9h9368fxfcz9knb2j"))))
    (synopsis "Official KiCad footprint libraries")
    (description "This package contains the official KiCad footprint libraries.")))

(define-public kicad-packages3d
  (package
    (inherit kicad-symbols)
    (name "kicad-packages3d")
    (version "5.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KiCad/kicad-packages3d.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cff2ms1bsw530kqb1fr1m2pjixyxzwa81mxgac3qpbcf8fnpvaz"))))
    (synopsis "Official KiCad 3D model libraries")
    (description "This package contains the official KiCad 3D model libraries.")))

(define-public kicad-templates
  (package
    (inherit kicad-symbols)
    (name "kicad-templates")
    (version "5.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KiCad/kicad-templates.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cs3bm3zb5ngw5ldn0lzw5bvqm4kvcidyrn76438alffwiz2b15g"))))
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

(define-public volk
  (package
    (name "volk")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://libvolk.org/releases/volk-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1bz3ywc6y5wmz3i8p4z2wbzhns8bc0ywdkl9qnxpcvfcscarbdlh"))))
    (build-system cmake-build-system)
    (inputs
     `(("boost" ,boost)))
    (native-inputs
     `(("python-2" ,python-2)
       ("python2-cheetah" ,python2-cheetah)))
    (home-page "http://libvolk.org/")
    (synopsis "Vector-Optimized Library of Kernels")
    (description
     "@code{volk} contains procedures with machine-specific optimizations
for mathematical functions.  It also provides an machine-independent
interface to select the best such procedures to use on a given system.")
    (license license:gpl3+)))

(define-public minicom
  (package
    (name "minicom")
    (version "2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://alioth.debian.org/frs/download.php/"
                           "file/4215/" name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wa1l36fa4npd21xa9nz60yrqwkk5cq713fa3p5v0zk7g9mq6bsk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-lock-dir=/var/lock")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-lock-check
           (lambda _
             (substitute* "configure"
               (("test -d [$]UUCPLOCK") "true"))
             #t)))))
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page "https://alioth.debian.org/projects/minicom/")
    (synopsis "Serial terminal emulator")
    (description "@code{minicom} is a serial terminal emulator.")
    (license license:gpl2+)))

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
     `(("lapack" ,lapack)))
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
     `(("guile" ,guile-2.2)))
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
    (version "2.3.6")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "mirror://sourceforge/mot-adms/adms-source/"
                (version-major+minor version) "/adms-" version ".tar.gz"))
              (sha256
               (base32
                "1rn98l6jxcjhi6ai5f7p588khra9z80m0m0lql4n4sb7773fh1vk"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("flex" ,flex)
       ("bison" ,bison)))
    (home-page "https://sourceforge.net/projects/mot-adms")
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
     `(("capstone" ,capstone)))
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

(define-public radare2
  (package
    (name "radare2")
    (version "4.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/radareorg/radare2")
                    (commit version)))
              (sha256
               (base32
                "14b9433cgc2nabhz836zfgvgh2dwailcmvy05krsa0inmzbvx9fg"))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "libr/asm/p/Makefile"
                    (("LDFLAGS\\+=") "LDFLAGS+=-Wl,-rpath=$(LIBDIR) "))
                  (substitute* "libr/parse/p/Makefile"
                    (("LDFLAGS\\+=") "LDFLAGS+=-Wl,-rpath=$(LIBDIR) "))
                  (substitute* "libr/bin/p/Makefile"
                    (("LDFLAGS\\+=") "LDFLAGS+=-Wl,-rpath=$(LIBDIR) "))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; tests require git and network access
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'mklibdir
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref %outputs "out") "/lib"))
             #t)))
       #:configure-flags
       (list "--with-sysmagic" "--with-syszip" "--with-openssl"
             "--without-nonpic" "--with-rpath" "--with-syscapstone")
       #:make-flags
       (list "CC=gcc")))
    (inputs
     `(("openssl" ,openssl)
       ("zip" ,zip)
       ("gmp" ,gmp)
       ("capstone" ,capstone)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
     `(("coreutils-minimal" ,coreutils-minimal)))
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
    (version "28")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ngspice/ng-spice-rework/"
                                  version "/ngspice-" version ".tar.gz"))
              (sha256
               (base32
                "0rnz2rdgyav16w7wfn3sfrk2lwvvgz1fh0l9107zkcldijklz04l"))
              (modules '((guix build utils)))
              ;; We remove the non-free cider and build without it.
              (snippet
               '(begin
                  (delete-file-recursively "src/ciderlib")
                  (delete-file "src/ciderinit")
                  (substitute* "configure"
                    (("src/ciderlib/Makefile") "")
                    (("src/ciderlib/input/Makefile") "")
                    (("src/ciderlib/support/Makefile") "")
                    (("src/ciderlib/oned/Makefile") "")
                    (("src/ciderlib/twod/Makefile") ""))
                  #t))))
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
               (("`date`") "Thu Jan  1 00:00:01 UTC 1970"))
             #t))
         (add-after 'unpack 'delete-program-manuals
           (lambda _
             (substitute* "man/man1/Makefile.in"
               (("^man_MANS = ngspice\\.1 ngnutmeg\\.1 ngsconvert\\.1 ngmultidec\\.1")
                "man_MANS = "))
             #t))
         (add-after 'install 'delete-script-files
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively
              (string-append (assoc-ref outputs "out")
                             "/share/ngspice/scripts")))))
       #:configure-flags
       (list "--enable-openmp"
             "--enable-xspice"
             "--with-ngshared"
             "--with-readline=yes")))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("libxaw" ,libxaw)
       ("mpi" ,openmpi)
       ("readline" ,readline)))
    (home-page "http://ngspice.sourceforge.net/")
    (synopsis "Mixed-level/mixed-signal circuit simulator")
    (description
     "Ngspice is a mixed-level/mixed-signal circuit simulator.  It includes
@code{Spice3f5}, a circuit simulator, and @code{Xspice}, an extension that
provides code modeling support and simulation of digital components through
an embedded event driven algorithm.")
    (license (list license:lgpl2.0+ ; code in frontend/numparam
                   (license:non-copyleft "file:///COPYING") ; spice3 bsd-style
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
                  "SUBDIRS = misc maths frontend spicelib"))
               #t))
           (add-after 'install 'delete-cmpp-dlmain
             (lambda* (#:key outputs #:allow-other-keys)
               (for-each (lambda (file)
                           (delete-file
                            (string-append (assoc-ref outputs "out")
                                           file)))
                         '("/bin/cmpp" "/share/ngspice/dlmain.c"))
               #t))
           (delete 'delete-program-manuals)
           (delete 'delete-script-files)))))
    (inputs
     `(("libngspice" ,libngspice)
       ("readline" ,readline)))))

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
                "117dqs0d4pcgbzvr3jn5ppra7n7x2m6c161ywh6laa934pw7h2bz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-pkg-config
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "freehdl/freehdl-config"
               (("pkg-config")
                (string-append (assoc-ref inputs "pkg-config")
                               "/bin/pkg-config"))
               (("cat")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/cat")))
             #t))
         (add-after 'patch-pkg-config 'setenv
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CXX" (string-append (assoc-ref inputs "gcc")
                                          "/bin/g++"))
             (setenv "SYSTEM_LIBTOOL" (string-append (assoc-ref inputs "libtool")
                                                     "/bin/libtool"))
             #t))
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
                (string-append (assoc-ref inputs "guile") "/bin/guile"))
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
     `(("coreutils" ,coreutils)
       ("gcc-toolchain" ,gcc-toolchain)
       ("guile" ,guile-2.2)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("libtool" ,libtool)))
    (native-inputs
     `(("pkg-config-native" ,pkg-config)
       ("gcc" ,gcc-5)
       ("libtool-native" ,libtool)))
    (home-page "http://www.freehdl.seul.org/")
    (synopsis "VHDL simulator")
    (description
     "FreeHDL is a compiler/simulator suite for the hardware description language VHDL.
  VHDL'93 as well as VHDL'87 standards are supported.")
    (license (list license:gpl2+
                   license:lgpl2.0+)))) ; freehdl's libraries

(define-public qucs
  ;; Qucs 0.0.19 segfaults when using glibc-2.26. Temporarily build from git.
  ;; TODO: When qucs-0.0.20 is released, revert the commit that introduced this
  ;; comment and update the package.
  (let ((commit "b4f27d9222568066cd59e4c387c51a35056c99d8")
        (revision "0"))
    (package
      (name "qucs")
      (version (git-version "0.0.19" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Qucs/qucs")
                      (commit commit)))
                (sha256
                 (base32 "10bclay9xhkffmsh4j4l28kj1qpxx0pnxja5vx6305cllnq4r3gb"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'bootstrap 'patch-bootstrap
             (lambda _
               (for-each patch-shebang
                         '("bootstrap"
                           "qucs/bootstrap"
                           "qucs-doc/bootstrap"
                           "qucs-core/bootstrap"))
               #t))
           (add-before 'configure 'patch-configure
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "qucs/configure"
                 (("\\$QTDIR") (assoc-ref inputs "qt4")))
               #t))
           (add-after 'patch-configure 'patch-scripts
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* '("qucs/qucs/qucsdigi"
                              "qucs/qucs/qucsdigilib"
                              "qucs/qucs/qucsveri")
                 (("\\$BINDIR")
                  (string-append (assoc-ref outputs "out") "/bin"))
                 (("freehdl-config")
                  (string-append (assoc-ref inputs "freehdl") "/bin/freehdl-config"))
                 (("freehdl-v2cc")
                  (string-append (assoc-ref inputs "freehdl") "/bin/freehdl-v2cc"))
                 (("cp ")
                  (string-append (assoc-ref inputs "coreutils") "/bin/cp "))
                 (("glibtool")
                  (string-append (assoc-ref inputs "libtool") "/bin/libtool"))
                 (("sed")
                  (string-append (assoc-ref inputs "sed") "/bin/sed"))
                 (("iverilog")
                  (string-append (assoc-ref inputs "iverilog") "/bin/iverilog"))
                 (("vvp")
                  (string-append (assoc-ref inputs "iverilog") "/bin/vvp")))
               #t))
           (add-before 'check 'pre-check
             (lambda _
               ;; The test suite requires a running X server.
               (system "Xvfb :1 &")
               (setenv "DISPLAY" ":1")
               #t))
           (add-after 'install 'make-wrapper
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; 'qucs' directly invokes gcc, hence this wrapping.
                 (wrap-program (string-append out "/bin/qucs")
                   `("CPLUS_INCLUDE_PATH" ":" prefix
                     (,(string-append (assoc-ref inputs "gcc-toolchain")
                                      "/include")))
                   `("PATH" ":" prefix
                     (,(string-append (assoc-ref inputs "gcc-toolchain")
                                      "/bin")))
                   `("LIBRARY_PATH" ":" prefix
                     (,(string-append (assoc-ref inputs "gcc-toolchain")
                                      "/lib")))
                   `("ADMSXMLBINDIR" ":" prefix
                     (,(string-append (assoc-ref inputs "adms") "/bin")))
                   `("ASCOBINDIR" ":" prefix
                     (,(string-append (assoc-ref inputs "asco") "/bin")))
                   `("QUCS_OCTAVE" ":" prefix
                     (,(string-append (assoc-ref inputs "octave") "/bin/octave")))))
               #t)))
         #:parallel-build? #f ; race condition
         #:configure-flags '("--disable-doc"))) ; we need octave-epstk
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("bison" ,bison)
         ("flex" ,flex)
         ("gperf" ,gperf)
         ("libtool-native" ,libtool)
         ("pkg-config" ,pkg-config)
         ("python" ,python-2) ; for tests
         ("matplotlib" ,python2-matplotlib) ; for tests
         ("numpy" ,python2-numpy) ; for tests
         ("xorg-server" ,xorg-server-for-tests))) ; for tests
      (inputs
       `(("adms" ,adms)
         ("asco" ,asco)
         ("coreutils" ,coreutils)
         ("freehdl" ,freehdl)
         ("gcc-toolchain" ,gcc-toolchain)
         ("iverilog" ,iverilog)
         ("libtool" ,libtool)
         ("octave" ,octave-cli)
         ("qt4" ,qt-4)
         ("sed" ,sed)))
      (home-page "http://qucs.sourceforge.net/")
      (synopsis "Circuit simulator with graphical user interface")
      (description
       "Qucs is a circuit simulator with graphical user interface.  The software
aims to support all kinds of circuit simulation types---e.g. DC, AC,
S-parameter, transient, noise and harmonic balance analysis.  Pure digital
simulations are also supported.")
      (license license:gpl2+))))

(define-public qucs-s
  (package
    (name "qucs-s")
    (version "0.0.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ra3xdh/qucs_s/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12m1jwhb9qwvb141qzyskbxnw3wn1x22d02z4b4862p7xvccl5h7"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-scripts
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("qucs/qucsdigi"
                            "qucs/qucsdigilib"
                            "qucs/qucsveri")
               (("\\$BINDIR")
                (string-append (assoc-ref inputs "qucs") "/bin"))
               (("freehdl-config")
                (string-append (assoc-ref inputs "freehdl") "/bin/freehdl-config"))
               (("freehdl-v2cc")
                (string-append (assoc-ref inputs "freehdl") "/bin/freehdl-v2cc"))
               (("cp ")
                (string-append (assoc-ref inputs "coreutils") "/bin/cp "))
               (("glibtool")
                (string-append (assoc-ref inputs "libtool") "/bin/libtool"))
               (("sed")
                (string-append (assoc-ref inputs "sed") "/bin/sed"))
               (("iverilog")
                (string-append (assoc-ref inputs "iverilog") "/bin/iverilog"))
               (("vvp")
                (string-append (assoc-ref inputs "iverilog") "/bin/vvp")))
             #t))
         (add-after 'patch-scripts 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "qucs/main.cpp"
               (((string-append "QucsSettings\\.Qucsator = QucsSettings\\.BinDir "
                                "\\+ \"qucsator\" \\+ executableSuffix"))
                (string-append "}{ QucsSettings.Qucsator = \""
                               (assoc-ref inputs "qucs") "/bin/qucsator\""))
               (((string-append "QucsSettings\\.XyceExecutable = "
                                "\"/usr/local/Xyce-Release-6.8.0-OPENSOURCE/bin/Xyce"))
                (string-append "}{ QucsSettings.XyceExecutable = \""
                               (assoc-ref inputs "xyce-serial") "/bin/Xyce"))
               (((string-append "else QucsSettings\\.XyceParExecutable = "
                                "\"mpirun -np %p /usr/local"
                                "/Xyce-Release-6.8.0-OPENMPI-OPENSOURCE/bin/Xyce"))
                (string-append "QucsSettings.XyceParExecutable = \""
                               (assoc-ref inputs "mpi") "/bin/mpirun -np %p "
                               (assoc-ref inputs "xyce-parallel") "/bin/Xyce"))
               (("else QucsSettings\\.NgspiceExecutable = \"ngspice\"")
                (string-append "QucsSettings.NgspiceExecutable = " "\""
                               (assoc-ref inputs "ngspice") "/bin/ngspice\"")))
             (substitute* "qucs/extsimkernels/ngspice.cpp"
               (("share/qucs/xspice_cmlib") "share/qucs-s/xspice_cmlib"))
             (substitute* "qucs/qucs_actions.cpp"
               (("qucstrans")
                (string-append (assoc-ref inputs "qucs") "/bin/qucstrans"))
               (("qucsattenuator")
                (string-append (assoc-ref inputs "qucs") "/bin/qucsattenuator"))
               (("qucsrescodes")
                (string-append (assoc-ref inputs "qucs") "/bin/qucsrescodes")))
             #t))
         (add-after 'install 'install-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (for-each
              (lambda (script)
                (let ((file (string-append "../qucs_s-" ,version
                                           "/qucs/" script))
                      (out (assoc-ref outputs "out")))
                  (install-file file (string-append out "/bin"))
                  (chmod (string-append out "/bin/" script) #o555)))
              '("qucsdigi" "qucsdigilib" "qucsveri"))
             #t))
         (add-after 'install-scripts 'make-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (file (string-append out "/bin/qucs-s"))
                    (qucs (assoc-ref inputs "qucs"))
                    (qucsator (string-append qucs "/bin/qucsator")))
               (wrap-program file
                 `("CPLUS_INCLUDE_PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "gcc-toolchain")
                                    "/include")))
                 `("PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "gcc-toolchain")
                                    "/bin")))
                 `("LIBRARY_PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "gcc-toolchain")
                                    "/lib")))
                 `("QUCSATOR" ":" prefix (,qucsator))
                 `("QUCSCONV" ":" prefix (,(string-append qucsator "/bin/qucsconv")))
                 `("ADMSXMLBINDIR" ":" prefix (,(string-append (assoc-ref inputs "adms")
                                                               "/bin")))
                 `("ASCOBINDIR" ":" prefix (,(string-append (assoc-ref inputs "asco")
                                                            "/bin")))
                 `("QUCS_OCTAVE" ":" prefix (,(string-append (assoc-ref inputs "octave")
                                                             "/bin/octave"))))
               (symlink qucsator (string-append out "/bin/qucsator"))
               #t))))))
    (native-inputs
     `(("libtool-native" ,libtool)))
    (inputs
     `(("adms" ,adms)
       ("asco" ,asco)
       ("coreutils" ,coreutils)
       ("freehdl" ,freehdl)
       ("gcc-toolchain" ,gcc-toolchain)
       ("iverilog" ,iverilog)
       ("libtool" ,libtool)
       ("mpi" ,openmpi)
       ("ngspice" ,ngspice)
       ("octave" ,octave-cli)
       ("qt4" ,qt-4)
       ("qucs" ,qucs)
       ("sed" ,sed)
       ("xyce-serial" ,xyce-serial)
       ("xyce-parallel" ,xyce-parallel)))
    (home-page "https://ra3xdh.github.io/")
    (synopsis "Circuit simulator with graphical user interface")
    (description
     "Qucs-S is a spin-off of the Qucs cross-platform circuit simulator.
The S letter indicates SPICE.  The purpose of the Qucs-S subproject is to use
free SPICE circuit simulation kernels with the Qucs GUI.  It provides the
simulator backends @code{Qucsator}, @code{ngspice} and @code{Xyce}.")
    (license license:gpl2+)))

(define-public librepcb
  (package
    (name "librepcb")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.librepcb.org/releases/"
                           version "/librepcb-" version "-source.zip"))
       (sha256
        (base32 "1ich849dsx2hmcwlwbry4mkg374n940l3hy6srh4qms2rm7vd7x0"))))
    (build-system gnu-build-system)
    (inputs
     `(("qtbase" ,qtbase)
       ("zlib" ,zlib)))
    (native-inputs
     `(("qttools" ,qttools)             ; for lrelease
       ("unzip" ,unzip)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p "build")
             (chdir "build")
             (let ((lrelease (string-append (assoc-ref inputs "qttools")
                                            "/bin/lrelease"))
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
                    (url "https://github.com/markwal/GPX.git")
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
     `(("readline" ,readline)))
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

(define-public cutter
  (package
    (name "cutter")
    (version "1.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/radareorg/cutter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1gvsrcskcdd1hxrjpkpc657anmfs25f174vxk4wzvn385rnmrxd3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (radare2 (assoc-ref inputs "radare2")))
               ;; fix pkg-config detection ./src/lib_radare2.pri:PREFIX=/usr/lib
               ;; override `qmake PREFIX=`
               (substitute* "./src/lib_radare2.pri"
                 (("PREFIX") "R2PREFIX")
                 (("R2PREFIX=/usr") (string-append "R2PREFIX=" radare2)))
               (invoke "qmake"
                       (string-append "PREFIX=" out)
                       "./src/Cutter.pro")))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("openssl" ,openssl)
       ("radare2" ,radare2)))
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
    (version "1.8.1")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference (url "https://github.com/3MFConsortium/lib3mf.git")
                          (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "11wpk6n9ga2p57h1dcrp37w77mii0r7r6mlrgmykf7rvii1rzgqd"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("googletest-source" ,(package-source googletest))))
    (inputs
     `(("libuuid" ,util-linux)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-googletest
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "googletest-source")
                               "UnitTests/googletest")
             #t)))))
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
    (version "2019.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.openscad.org/openscad-" version
                           ".src.tar.gz"))
       (sha256
        (base32
         "0nbgk5q5pgnw53la0kccdcpz2f4xf6d6076rkn0q08z57hkc85ha"))))
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
       ("qtbase" ,qtbase)
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
               (invoke "ctest"))
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

(define-public freecad
  (package
    (name "freecad")
    (version "0.18.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FreeCAD/FreeCAD.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "170hk1kgrvsddrwykp24wyj0cha78zzmzbf50gn98x7ngqqs395s"))))
    (build-system qt-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("graphviz" ,graphviz)
       ("qttools" ,qttools)
       ("pkg-config" ,pkg-config)
       ("swig" ,swig)))
    (inputs
     `(("boost" ,boost)
       ("coin3D" ,coin3D)
       ("eigen" ,eigen)
       ("freetype" ,freetype)
       ("glew" ,glew)
       ("hdf5" ,hdf5-1.10)
       ("libarea" ,libarea)
       ("libmedfile" ,libmedfile)
       ("libspnav" ,libspnav)
       ("libxi" ,libxi)
       ("libxmu" ,libxmu)
       ("openmpi" ,openmpi)
       ("opencascade-occt" ,opencascade-occt)
       ("python-matplotlib" ,python-matplotlib)
       ("python-pyside-2" ,python-pyside-2)
       ("python-pyside-2-tools" ,python-pyside-2-tools)
       ("python-shiboken-2" ,python-shiboken-2)
       ("python-wrapper" ,python-wrapper)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ;; qtwebkit is optional. We remove it currently, because it takes
       ;; much time to compile and substitutes are often unavailable
       ;;("qtwebkit" ,qtwebkit)
       ("tbb" ,tbb)
       ("vtk" ,vtk)
       ("xerces-c" ,xerces-c)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list
        "-DBUILD_QT5=ON"
        (string-append "-DCMAKE_INSTALL_LIBDIR="
                       (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'restore-pythonpath
           (lambda _
             (substitute* "src/Main/MainGui.cpp"
               (("_?putenv\\(\"PYTHONPATH=\"\\);") ""))
             #t))
         (add-after 'install 'wrap-pythonpath
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/FreeCAD")
                 (list "PYTHONPATH"
                       'prefix (list (getenv "PYTHONPATH")))))
             #t)))))
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
    (inputs `(("hdf5" ,hdf5-1.10)))
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
         (uri (git-reference (url "https://github.com/Heeks/libarea.git")
                             (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0pvqz6cabxqdz5y26wnj6alkn8v5d7gkx0d3h8xmg4lvy9r3kh3g"))))
      (build-system gnu-build-system)
      (inputs `(("boost" ,boost)
                ("python-wrapper" ,python-wrapper)))
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
                    (url "https://github.com/FreeSpacenav/libspnav.git")
                    (commit (string-append "libspnav-" version))))
              (sha256
               (base32
                "098h1jhlj87axpza5zgy58prp0zn94wyrbch6x0s7q4mzh7dc8ba"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     `(("libx11" ,libx11)))
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
