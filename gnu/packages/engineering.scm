;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
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
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
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
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages wxwidgets)
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
    (home-page "http://librecad.org/")
    (synopsis "Computer-aided design (CAD) application")
    (description
     "LibreCAD is a 2D Computer-aided design (CAD) application for creating
plans and designs.")
    (license license:gpl2)))

(define-public geda-gaf
  (package
    (name "geda-gaf")
    (version "1.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ftp.geda-project.org/geda-gaf/unstable/v"
                    (version-major+minor version) "/"
                    version "/geda-gaf-" version ".tar.gz"))
              (sha256
               (base32
                "14mk45pfz11v54q66gafw2l68n1p5ssvvjmdm8ffgc8x1w5ajfrz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; tests require a writable HOME
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" (getenv "TMPDIR"))
             #t)))
       #:configure-flags
       (let ((pcb (assoc-ref %build-inputs "pcb")))
         (list (string-append "--with-pcb-datadir=" pcb "/share")
               (string-append "--with-pcb-lib-path="
                              pcb "/share/pcb/pcblib-newlib:"
                              pcb "/share/pcb/newlib")))))
    (inputs
     `(("glib" ,glib)
       ("gtk" ,gtk+-2)
       ("guile" ,guile-2.0)
       ("desktop-file-utils" ,desktop-file-utils)
       ("shared-mime-info" ,shared-mime-info)
       ("m4" ,m4)
       ("pcb" ,pcb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
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
       ("desktop-file-utils" ,desktop-file-utils)
       ("shared-mime-info" ,shared-mime-info)
       ("tk" ,tk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("bison" ,bison)
       ("flex" ,flex)
       ;; For tests
       ("imagemagick" ,imagemagick)
       ("gerbv" ,gerbv)
       ("ghostscript" ,ghostscript)
       ("xvfb" ,xorg-server)))
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
             (zero? (system* "sh" "configure"
                             (string-append "--prefix="
                                            (assoc-ref outputs "out")))))))))
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
               '(delete-file "doc/psfig.sty"))
              (patches (search-patches "fastcap-mulSetup.patch"
                                       "fastcap-mulGlobal.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     ;; FIXME: with texlive-tiny citation references are rendered as question
     ;; marks.  During the build warnings like these are printed:
     ;; LaTeX Warning: Citation `nabors91' on page 2 undefined on input line 3.
     `(("texlive" ,texlive-tiny)
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
             (zero? (system* "make" "CC=gcc" "RM=rm" "SHELL=sh"
                             "manual"))))
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
                (every (lambda (file)
                         (zero? (system* "dvips" file "-o")))
                       (find-files "." "\\.dvi"))
                (every (lambda (file)
                         (zero? (system* "ps2pdf" file)))
                       '("mtt.ps" "ug.ps" "tcad.ps"))
                (zero? (system* "make" "clean"))))))
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
    (version "0.9.2b")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/fritzing/"
                                  "fritzing-app/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15rwjp4xdj9w1z9f709rz9p0k2mi9k9idma9hvzkj5j8p04mg7yd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (and (zero? (system* "tar"
                                  "-xvf" (assoc-ref inputs "fritzing-parts-db")
                                  "-C" "parts"))
                  (zero? (system* "qmake"
                                  (string-append "PREFIX="
                                                 (assoc-ref outputs "out"))
                                  "phoenix.pro"))))))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtserialport" ,qtserialport)
       ("qtsvg" ,qtsvg)
       ("boost" ,boost)
       ("zlib" ,zlib)
       ("fritzing-parts-db"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://github.com/fritzing/"
                               "fritzing-parts/archive/" version ".tar.gz"))
           (file-name (string-append "fritzing-parts-" version ".tar.gz"))
           (sha256
            (base32
             "0jqr8yjg7177f3pk1fcns584r0qavwpr280nggsi2ff3pwk5wpsz"))))))
    (home-page "http://fritzing.org")
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
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gerbv/gerbv/gerbv-"
                                  version "/gerbv-" version ".tar.gz"))
              (sha256
               (base32
                "0v6ry0mxi5qym4z0y0lpblxsw9dfjpgxs4c4v2ngg7yw4b3a59ks"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoconf
           (lambda _
             ;; Build rules contain references to Russian translation, but the
             ;; needed files are missing; see
             ;; http://sourceforge.net/p/gerbv/bugs/174/
             (delete-file "po/LINGUAS")
             (substitute* "man/Makefile.am"
               (("PO_FILES= gerbv.ru.1.in.po") "")
               (("man_MANS = gerbv.1 gerbv.ru.1") "man_MANS = gerbv.1"))
             (zero? (system* "autoreconf" "-vfi")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("gettext" ,gettext-minimal)
       ("po4a" ,po4a)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("gtk" ,gtk+-2)
       ("desktop-file-utils" ,desktop-file-utils)))
    (home-page "http://gerbv.geda-project.org/")
    (synopsis "Gerber file viewer")
    (description
     "Gerbv is a viewer for files in the Gerber format (RS-274X only), which
is commonly used to represent printed circuit board (PCB) layouts.  Gerbv lets
you load several files on top of each other, do measurements on the displayed
image, etc.  Besides viewing Gerbers, you may also view Excellon drill files
as well as pick-place files.")
    (license license:gpl2+)))

(define-public ao
  (let ((commit "0bc2354b8dcd1a82a0fd6647706b126045e52734"))
    (package
      (name "ao-cad")            ;XXX: really "ao", but it collides with libao
      (version (string-append "0." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mkeeter/ao")
                      (commit commit)))
                (sha256
                 (base32
                  "0lm7iljklafs8dhlvaab2yhwx4xymrdjrqk9c5xvn59hlvbgl1j5"))
                (file-name (string-append name "-" version "-checkout"))
                (modules '((guix build utils)))
                (snippet
                 ;; Remove bundled libraries: Eigen, glm, and catch.  TODO:
                 ;; Unbundle efsw <https://github.com/diegostamigni/efsw>.
                 '(begin
                    (delete-file-recursively "vendor")

                    ;; Use #include <catch.hpp>.
                    (substitute* (find-files "." "\\.[ch]pp$")
                      (("catch/catch\\.hpp")
                       "catch.hpp"))))))
      (build-system cmake-build-system)
      (arguments
       `(;; Have the RUNPATH of libao.so point to $libdir, where libefsw.so
         ;; lives.
         #:configure-flags (list (string-append "-DCMAKE_SHARED_LINKER_FLAGS="
                                                "-Wl,-rpath="
                                                (assoc-ref %outputs "out")
                                                "/lib"))

         #:phases
         (modify-phases %standard-phases
           (add-before 'build 'add-eigen-to-search-path
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Allow things to find our own Eigen and Catch.
               (let ((eigen (assoc-ref inputs "eigen")))
                 (setenv "CPLUS_INCLUDE_PATH"
                         (string-append eigen "/include/eigen3:"
                                        (getenv "CPLUS_INCLUDE_PATH")))
                 #t)))
           (add-after 'install 'install-guile-bindings
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Install the Guile bindings (the build system only installs
               ;; libao.so.)
               (let* ((out    (assoc-ref outputs "out"))
                      (moddir (string-append out "/share/guile/site/2.0")))
                 (install-file "bind/libao.so"
                               (string-append out "/lib"))

                 ;; Go to the source directory.
                 (with-directory-excursion ,(string-append "../"
                                                           name "-" version
                                                           "-checkout")
                   (substitute* "bind/guile/ao/bind.scm"
                     (("\\(define libao \\(dynamic-link .*$")
                      (string-append "(define libao (dynamic-link \""
                                     out "/lib/libao\")) ;")))

                   (for-each (lambda (file)
                               (install-file file
                                             (string-append moddir
                                                            "/ao")))
                             (find-files "bind/guile" "\\.scm$"))

                   (substitute* "bin/ao-guile"
                     (("\\(add-to-load-path .*")
                      (string-append "(add-to-load-path \"" moddir "\")")))

                   (install-file "bin/ao-guile"
                                 (string-append out "/bin"))

                   ;; Allow Ao to dlopen the relevant GL libraries.  Otherwise
                   ;; it fails with:
                   ;;   Couldn't find current GLX or EGL context.
                   (let ((mesa (assoc-ref inputs "mesa")))
                     (wrap-program (string-append out "/bin/ao-guile")
                       `("LD_LIBRARY_PATH" ":" prefix
                         (,(string-append mesa "/lib")))))
                   #t)))))))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (inputs
       `(("boost" ,boost)
         ("catch" ,catch-framework)
         ("libpng" ,libpng)
         ("glfw" ,glfw)
         ("libepoxy" ,libepoxy)
         ("mesa" ,mesa)
         ("eigen" ,eigen)
         ("glm" ,glm)
         ("guile" ,guile-2.0)))
      (home-page "http://www.mattkeeter.com/projects/ao/")
      (synopsis "Tool for programmatic computer-aided design")
      (description
       "Ao is a tool for programmatic computer-aided design (CAD).  In Ao,
solid models are defined as Scheme scripts, and there are no opaque function
calls into the geometry kernel: everything is visible to the user.  Even
fundamental, primitive shapes are represented as code in the user-level
language.")
      (license (list license:lgpl2.1+             ;library
                     license:gpl2+)))))           ;Guile bindings

;; We use kicad from a git commit, because support for boost 1.61.0 has been
;; recently added.
(define-public kicad
  (let ((commit "5f4599fb56da4dd748845ab10abec02961d477f3")
        (revision "2"))
    (package
      (name "kicad")
      (version (string-append "4.0-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.launchpad.net/kicad")
               (commit commit)))
         (sha256
          (base32 "1833pln2975gmc5s18xf7s8m9vg834lmxxdjk0wlk3lq7bvjjnff"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system cmake-build-system)
      (arguments
       `(#:out-of-source? #t
         #:tests? #f ; no tests
         #:configure-flags
         (list "-DKICAD_STABLE_VERSION=ON"
               "-DKICAD_REPO_NAME=stable"
               ,(string-append "-DKICAD_BUILD_VERSION=4.0-"
                               (string-take commit 7))
               "-DCMAKE_BUILD_TYPE=Release"
               "-DKICAD_SKIP_BOOST=ON"; Use our system's boost library.
               (string-append "-DCMAKE_INSTALL_LIBDIR="
                              (assoc-ref %outputs "out") "/lib")
               "-DKICAD_SCRIPTING=ON"
               "-DKICAD_SCRIPTING_MODULES=ON"
               "-DKICAD_SCRIPTING_WXPYTHON=ON"
               ;; Has to be set explicitely, as we don't have the wxPython
               ;; headers in the wxwidgets store item, but in wxPython.
               (string-append "-DCMAKE_CXX_FLAGS=-I"
                              (assoc-ref %build-inputs "wxpython")
                              "/include/wx-3.0")
               "-DCMAKE_BUILD_WITH_INSTALL_RPATH=TRUE"
               "-DKICAD_SPICE=TRUE"
               ;; TODO: Enable this when CA certs are working with curl.
               "-DBUILD_GITHUB_PLUGIN=OFF")
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-program
             ;; Ensure correct Python at runtime.
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (python (assoc-ref inputs "python"))
                      (file (string-append out "/bin/kicad"))
                      (path (string-append
                             out
                             "/lib/python2.7/site-packages:"
                             (getenv "PYTHONPATH"))))
                 (wrap-program file
                   `("PYTHONPATH" ":" prefix (,path))
                   `("PATH" ":" prefix
                     (,(string-append python "/bin:")))))
               #t)))))
      (native-inputs
       `(("boost" ,boost)
         ("gettext" ,gnu-gettext)
         ("pkg-config" ,pkg-config)
         ("swig" ,swig)
         ("zlib" ,zlib)))
      (inputs
       `(("cairo" ,cairo)
         ("curl" ,curl)
         ("desktop-file-utils" ,desktop-file-utils)
         ("glew" ,glew)
         ("glm" ,glm)
         ("hicolor-icon-theme" ,hicolor-icon-theme)
         ("libngspice" ,libngspice)
         ("libsm" ,libsm)
         ("mesa" ,mesa)
         ("openssl" ,openssl)
         ("python" ,python-2)
         ("wxwidgets" ,wxwidgets-gtk2)
         ("wxpython" ,python2-wxpython)))
      (home-page "http://kicad-pcb.org/")
      (synopsis "Electronics Design Automation Suite")
      (description "Kicad is a program for the formation of printed circuit
boards and electrical circuits.  The software has a number of programs that
perform specific functions, for example, pcbnew (Editing PCB), eeschema (editing
electrical diagrams), gerbview (viewing Gerber files) and others.")
      (license license:gpl3+))))

(define-public kicad-library
  (let ((version "4.0.6"))
    (package
      (name "kicad-library")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "http://downloads.kicad-pcb.org/libraries/kicad-library-"
                      version ".tar.gz"))
                (sha256
                 (base32
                  "16f47pd6f0ddsdxdrp327nr9v05gl8c24d0qypq2aqx5hdjmkp7f"))))
      (build-system cmake-build-system)
      (arguments
       `(#:out-of-source? #t
         #:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'install-footprints ; from footprints tarball
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (zero? (system* "tar" "xvf"
                               (assoc-ref inputs "kicad-footprints")
                               "-C" (string-append (assoc-ref outputs "out")
                                                   "/share/kicad/modules")
                               "--strip-components=1"))))
           ;; We change the default global footprint file, which is generated if
           ;; it doesn't exist in user's home directory, from the one using the
           ;; github plugin, to the one using the KISYSMOD environment path.
           (add-after 'install-footprints 'use-pretty-footprint-table
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (template-dir (string-append out "/share/kicad/template"))
                      (fp-lib-table (string-append template-dir "/fp-lib-table")))
                 (delete-file fp-lib-table)
                 (copy-file (string-append fp-lib-table ".for-pretty")
                              fp-lib-table))
               #t)))))
      (native-search-paths
       (list (search-path-specification
              (variable "KISYSMOD") ; footprint path
              (files '("share/kicad/modules")))
             (search-path-specification
              (variable "KISYS3DMOD") ; 3D model path
              (files '("share/kicad/modules/packages3d")))))
      ;; Kicad distributes footprints in a separate tarball
      (native-inputs
       `(("kicad-footprints"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "http://downloads.kicad-pcb.org/libraries/kicad-footprints-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0vmgqhdw05k5fdnqv42grnvlz7v75g9md82jp2d3dvw2zw050lfb"))))))
      (home-page "http://kicad-pcb.org/")
      (synopsis "Libraries for kicad")
      (description "This package provides Kicad component, footprint and 3D
render model libraries.")
      (license license:lgpl2.0+))))

(define-public linsmith
  (package
    (name "linsmith")
    (version "0.99.30")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/linsmith/linsmith/linsmith-"
                    version "/linsmith-" version ".tar.gz"))
              (sha256
               (base32
                "18qslhr2r45rhpj4v6bjcqx189vs0bflvsj271wr7w8kvh69qwvn"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gtk" ,gtk+-2)
       ("libgnome" ,libgnomeui)))
    (home-page "http://jcoppens.com/soft/linsmith/index.en.php")
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
     `(("python-2", python-2)
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
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "http://ab-initio.mit.edu/harminv/harminv-"
                version ".tar.gz"))
              (sha256
               (base32
                "1pmm8d6fx9ahhnk7w12bfa6zx3afbkg4gkvlvgwhpjxbcrvrp3jk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
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
    (home-page "http://ab-initio.mit.edu/wiki/index.php/Harminv")
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
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "http://ab-initio.mit.edu/libctl/libctl-"
                version ".tar.gz"))
              (sha256
               (base32
                "1g7gqybq20jhdnw5vg18bgbj9jz0408gfmjvs8b4xs30pic8pgca"))))
    (build-system gnu-build-system)
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
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "http://ab-initio.mit.edu/mpb/mpb-"
                version ".tar.gz"))
              (sha256
               (base32
                "1mqb2d8jq957nksayjygq58iy8i42vjryzg9iy5fpfay31wzxsix"))))
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
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "http://ab-initio.mit.edu/meep/meep-"
                version ".tar.gz"))
              (sha256
               (base32
                "0f6lbw2hrksg7xscwdqs78jc9nmzx9fs8j0hz1y4i8qknkqiyk2n"))))
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
       ("guile" ,guile-2.0)             ; doesn't build with guile-2.2
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
    (version "3.0.5-rc2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aquynh/capstone/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cqms9r2p43aiwp5spd84zaccp16ih03r7sjhrv16nddahj0jz2q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "PREFIX=" %output)
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; cstool's Makefile overrides LDFLAGS, so we cannot pass it as a make flag.
         (add-before 'build 'fix-cstool-ldflags
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "cstool/Makefile"
               (("LDFLAGS =")
                (string-append "LDFLAGS = -Wl,-rpath=" (assoc-ref outputs "out")
                               "/lib")))
             #t)))))
    (home-page "http://www.capstone-engine.org")
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
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://radare.mikelloc.com/get/" version "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "16ggsk40zz6hyvclvqj1r4bh4hb78jf0d6ppry1jk4r0j30wm7cm"))
              (modules '((guix build utils)))
              (snippet
                '(begin
                  (substitute* "libr/asm/p/Makefile"
                    (("LDFLAGS\\+=") "LDFLAGS+=-Wl,-rpath=$(LIBDIR) "))
                  (substitute* "libr/parse/p/Makefile"
                    (("LDFLAGS\\+=") "LDFLAGS+=-Wl,-rpath=$(LIBDIR) "))
                  (substitute* "libr/bin/p/Makefile"
                    (("LDFLAGS\\+=") "LDFLAGS+=-Wl,-rpath=$(LIBDIR) "))))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f; tests require git and network access
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
    (synopsis "Portable reversing framework")
    (description
      "Radare project started as a forensics tool, a scriptable commandline
hexadecimal editor able to open disk files, but later support for analyzing
binaries, disassembling code, debugging programs, attaching to remote gdb
servers, ...")
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
    (version "26")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ngspice/ng-spice-rework/"
                                  version "/ngspice-" version ".tar.gz"))
              (sha256
               (base32
                "02019ndcl057nq9z41nxycqba7wxlb081ibvfj9jv010nz431qji"))
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
                    (("src/ciderlib/twod/Makefile") ""))))))
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
               (("`date`") "Do 1. Jan 00:00:00 UTC 1970"))
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
    (version "12.6.3")
    (source
     (origin (method url-fetch)
             (uri (string-append "https://trilinos.org/oldsite/download/files/trilinos-"
                                 version "-Source.tar.gz"))
             (sha256
              (base32
               "07jd1qpsbf31cmbyyngr4l67xzwyan24dyx5wlcahgbw7x6my3wn"))))
    (build-system cmake-build-system)
    (arguments
     `(#:out-of-source? #t
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
       ("lapack" ,lapack-3.5)
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
    (version "6.7")
    (source
     (origin (method url-fetch)
             (uri (string-append "https://archive.org/download/Xyce-"
                                 version "/Xyce-" version ".tar.gz"))
             (sha256
              (base32
               "02k952mnvrnc5kv7r65fdrn7khwq1lbyhwyvd7jznafzdpsvgm4x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list
        "CXXFLAGS=-O3 -std=c++11"
        (string-append "ARCHDIR="
                       (assoc-ref %build-inputs "trilinos")))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("fortran" ,gfortran)))
    (inputs
     `(("fftw" ,fftw)
       ("suitesparse" ,suitesparse)
       ("lapack" ,lapack-3.5)
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
            `(list "CXXFLAGS=-O3 -std=c++11"
                   "CXX=mpiCC"
                   "CC=mpicc"
                   "F77=mpif77"
                   "--enable-mpi"
                   "--enable-isorropia=no"
                   "--enable-zoltan=no"
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
       ("libtool-native" ,libtool)))
    (home-page "http://www.freehdl.seul.org/")
    (synopsis "VHDL simulator")
    (description
     "FreeHDL is a compiler/simulator suite for the hardware description language VHDL.
  VHDL'93 as well as VHDL'87 standards are supported.")
    (license (list license:gpl2+
                   license:lgpl2.0+)))) ; freehdl's libraries

(define-public qucs
  (package
    (name "qucs")
    (version "0.0.19")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://sourceforge.net/projects/qucs/files/qucs/" version
                "/qucs-" version ".tar.gz"))
              (sha256
               (base32
                "0giv9gfyfdizvjhq56x2pdncrlyv3k15lrsh6pk37i94vr7l7ij5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
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
     `(("gperf" ,gperf)
       ("libtool-native" ,libtool)
       ("python" ,python-2) ; for tests
       ("matplotlib" ,python2-matplotlib) ; for tests
       ("numpy" ,python2-numpy) ; for tests
       ("xorg-server" ,xorg-server))) ; for tests
    (inputs
     `(("adms" ,adms)
       ("asco" ,asco)
       ("coreutils" ,coreutils)
       ("freehdl" ,freehdl)
       ("gcc-toolchain" ,gcc-toolchain)
       ("iverilog" ,iverilog)
       ("libtool" ,libtool)
       ("octave" ,octave)
       ("qt4" ,qt-4)
       ("sed" ,sed)))
    (home-page "http://qucs.sourceforge.net/")
    (synopsis "Circuit simulator with graphical user interface")
    (description
     "Qucs is a circuit simulator with graphical user interface.  The software
aims to support all kinds of circuit simulation types---e.g. DC, AC,
S-parameter, transient, noise and harmonic balance analysis.  Pure digital
simulations are also supported.")
    (license license:gpl2+)))

(define-public qucs-s
  (package
    (name "qucs-s")
    (version "0.0.19S")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ra3xdh/qucs/releases/download/"
                                  version "/qucs-" version ".tar.gz"))
              (sha256
               (base32
                "1bhahvdqmayaw0306fxz1ghmjhd4fq05yk3rk7zi0z703w5imgjv"))))
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
               (((string-append "else QucsSettings\\.XyceExecutable = "
                                "\"/usr/local/Xyce-Release-6.2.0-OPENSOURCE/bin/runxyce"))
                (string-append "QucsSettings.XyceExecutable = \""
                               (assoc-ref inputs "xyce-serial") "/bin/Xyce"))
               (((string-append "else QucsSettings\\.XyceParExecutable = \"/usr/local"
                                "/Xyce-Release-6.2.0-OPENMPI-OPENSOURCE/bin/xmpirun"))
                (string-append "QucsSettings.XyceParExecutable = \""
                               (assoc-ref inputs "mpi") "/bin/mpirun"))
               (("%p")
                (string-append "%p "(assoc-ref inputs "xyce-parallel") "/bin/Xyce"))
               (("else QucsSettings\\.NgspiceExecutable = \"ngspice\"")
                (string-append "QucsSettings.NgspiceExecutable = " "\""
                               (assoc-ref inputs "ngspice") "/bin/ngspice\"")))
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
                (let ((file (string-append "../qucs-" ,version
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
       ("octave" ,octave)
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
