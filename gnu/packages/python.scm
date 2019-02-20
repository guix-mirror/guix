;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2017, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Omar Radwan <toxemicsquire4@gmail.com>
;;; Copyright © 2015 Pierre-Antoine Rault <par@rigelk.eu>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015, 2016 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2015, 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016, 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Daniel Pimentel <d4n1@d4n1.org>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016, 2017 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016 Dylan Jeffers <sapientech@sapientech@openmailbox.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2016 Dylan Jeffers <sapientech@sapientech@openmailbox.org>
;;; Copyright © 2016, 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2016, 2017, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016, 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016, 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017, 2018 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2017, 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017, 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Muriithi Frederick Muriuki <fredmanglis@gmail.com>
;;; Copyright © 2017 Brendan Tildesley <brendan.tildesley@openmailbox.org>
;;; Copyright © 2018 Ethan R. Jones <ethanrjones97@gmail.com
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Vijayalakshmi Vedantham <vijimay12@gmail.com>
;;; Copyright © 2018 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2018 Adam Massmann <massmannak@gmail.com>
;;; Copyright © 2016, 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018 Luther Thompson <lutheroto@gmail.com>
;;; Copyright © 2018 Vagrant Cascadian <vagrant@debian.org>
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

(define-module (gnu packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial))

(define-public python-2.7
  (package
    (name "python2")
    (version "2.7.15")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.python.org/ftp/python/"
                          version "/Python-" version ".tar.xz"))
      (sha256
       (base32
        "0x2mvz9dp11wj7p5ccvmk9s0hzjk2fa1m462p395l4r6bfnb3n92"))
      (patches (search-patches "python-2.7-search-paths.patch"
                               "python-2-deterministic-build-info.patch"
                               "python-2.7-site-prefixes.patch"
                               "python-2.7-source-date-epoch.patch"
                               "python-2.7-adjust-tests.patch"
                               "python2-CVE-2018-14647.patch"
                               "python2-CVE-2018-1000802.patch"))
      (modules '((guix build utils)))
      ;; suboptimal to delete failing tests here, but if we delete them in the
      ;; arguments then we need to make sure to strip out that phase when it
      ;; gets inherited by python and python-minimal.
      (snippet
       '(begin
          (for-each delete-file
                    '("Lib/test/test_compileall.py"
                      "Lib/test/test_ctypes.py" ; fails on mips64el
                      "Lib/test/test_distutils.py"
                      "Lib/test/test_import.py"
                      "Lib/test/test_shutil.py"
                      "Lib/test/test_socket.py"
                      "Lib/test/test_subprocess.py"))
          #t))))
    (outputs '("out"
               "tk"))                     ;tkinter; adds 50 MiB to the closure
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:configure-flags
       (list "--enable-shared"                    ;allow embedding
             "--with-system-ffi"                  ;build ctypes
             "--with-ensurepip=install"           ;install pip and setuptools
             "--enable-unicode=ucs4"
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       ;; With no -j argument tests use all available cpus, so provide one.
       #:make-flags
       (list (format #f "TESTOPTS=-j~d" (parallel-job-count)))

        #:modules ((ice-9 ftw) (ice-9 match)
                   (guix build utils) (guix build gnu-build-system))
        #:phases
        (modify-phases %standard-phases
          (add-before
           'configure 'patch-lib-shells
           (lambda _
             ;; Filter for existing files, since some may not exist in all
             ;; versions of python that are built with this recipe.
             (substitute* (filter file-exists?
                                  '("Lib/subprocess.py"
                                    "Lib/popen2.py"
                                    "Lib/distutils/tests/test_spawn.py"
                                    "Lib/test/support/__init__.py"
                                    "Lib/test/test_subprocess.py"))
               (("/bin/sh") (which "sh")))
             #t))
          (add-before 'configure 'do-not-record-configure-flags
            (lambda* (#:key configure-flags #:allow-other-keys)
              ;; Remove configure flags from the installed '_sysconfigdata.py'
              ;; and 'Makefile' so we don't end up keeping references to the
              ;; build tools.
              ;;
              ;; Preserve at least '--with-system-ffi' since otherwise the
              ;; thing tries to build libffi, fails, and we end up with a
              ;; Python that lacks ctypes.
              (substitute* "configure"
                (("^CONFIG_ARGS=.*$")
                 (format #f "CONFIG_ARGS='~a'\n"
                         (if (member "--with-system-ffi" configure-flags)
                             "--with-system-ffi"
                             ""))))
              #t))
          (add-before
           'check 'pre-check
           (lambda _
             ;; 'Lib/test/test_site.py' needs a valid $HOME
             (setenv "HOME" (getcwd))
             #t))
          (add-after
           'unpack 'set-source-file-times-to-1980
           ;; XXX One of the tests uses a ZIP library to pack up some of the
           ;; source tree, and fails with "ZIP does not support timestamps
           ;; before 1980".  Work around this by setting the file times in the
           ;; source tree to sometime in early 1980.
           (lambda _
             (let ((circa-1980 (* 10 366 24 60 60)))
               (ftw "." (lambda (file stat flag)
                          (utime file circa-1980 circa-1980)
                          #t))
               #t)))
          (add-after 'install 'remove-tests
            ;; Remove 25 MiB of unneeded unit tests.  Keep test_support.*
            ;; because these files are used by some libraries out there.
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (match (scandir (string-append out "/lib")
                                (lambda (name)
                                  (string-prefix? "python" name)))
                  ((pythonX.Y)
                   (let ((testdir (string-append out "/lib/" pythonX.Y
                                                 "/test")))
                     (with-directory-excursion testdir
                       (for-each delete-file-recursively
                                 (scandir testdir
                                          (match-lambda
                                            ((or "." "..") #f)
                                            ("support" #f)
                                            (file
                                             (not
                                              (string-prefix? "test_support."
                                                              file))))))
                       (call-with-output-file "__init__.py" (const #t))
                       #t)))))))
          (add-after 'remove-tests 'rebuild-bytecode
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                ;; Disable hash randomization to ensure the generated .pycs
                ;; are reproducible.
                (setenv "PYTHONHASHSEED" "0")
                (for-each
                 (lambda (opt)
                   (format #t "Compiling with optimization level: ~a\n"
                           (if (null? opt) "none" (car opt)))
                   (for-each (lambda (file)
                               (apply invoke
                                      `(,(string-append out "/bin/python")
                                        ,@opt
                                        "-m" "compileall"
                                        "-f" ; force rebuild
                                        ;; Don't build lib2to3, because it contains Python 3 code.
                                        "-x" "lib2to3/.*"
                                        ,file)))
                             (find-files out "\\.py$")))
                 (list '() '("-O") '("-OO")))
                #t)))
          (add-after 'install 'move-tk-inter
            (lambda* (#:key outputs #:allow-other-keys)
              ;; When Tkinter support is built move it to a separate output so
              ;; that the main output doesn't contain a reference to Tcl/Tk.
              (let ((out (assoc-ref outputs "out"))
                    (tk  (assoc-ref outputs "tk")))
                (when tk
                  (match (find-files out "tkinter.*\\.so")
                    ((tkinter.so)
                     ;; The .so is in OUT/lib/pythonX.Y/lib-dynload, but we
                     ;; want it under TK/lib/pythonX.Y/site-packages.
                     (let* ((len    (string-length out))
                            (target (string-append
                                     tk "/"
                                     (string-drop
                                      (dirname (dirname tkinter.so))
                                      len)
                                     "/site-packages")))
                       (install-file tkinter.so target)
                       (delete-file tkinter.so)))))
                #t))))))
    (inputs
     `(("bzip2" ,bzip2)
       ("gdbm" ,gdbm)
       ("libffi" ,libffi)                         ; for ctypes
       ("sqlite" ,sqlite)                         ; for sqlite extension
       ("openssl" ,openssl)
       ("readline" ,readline)
       ("zlib" ,zlib)
       ("tcl" ,tcl)
       ("tk" ,tk)))                               ; for tkinter
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (native-search-paths
     (list (search-path-specification
            (variable "PYTHONPATH")
            (files '("lib/python2.7/site-packages")))))
    (home-page "https://www.python.org")
    (synopsis "High-level, dynamically-typed programming language")
    (description
     "Python is a remarkably powerful dynamic programming language that
is used in a wide variety of application domains.  Some of its key
distinguishing features include: clear, readable syntax; strong
introspection capabilities; intuitive object orientation; natural
expression of procedural code; full modularity, supporting hierarchical
packages; exception-based error handling; and very high level dynamic
data types.")
    (license license:psfl)))

;; Current 2.x version.
(define-public python-2 python-2.7)

(define-public python2-called-python
  ;; Both 2.x and 3.x used to be called "python".  In commit
  ;; a7714d42de2c3082f3609d1e63c83d703fb39cf9 (March 2018), we renamed the
  ;; Python 2.x package to "python2".
  (package
    (inherit python-2)
    (name "python")
    (properties `((superseded . ,python-2)))))

(define-public python-3.7
  (package (inherit python-2)
    (name "python")
    (version "3.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (patches (search-patches
                        "python-fix-tests.patch"
                        "python-3-fix-tests.patch"
                        "python-3-deterministic-build-info.patch"
                        "python-3-search-paths.patch"))
              (patch-flags '("-p0"))
              (sha256
               (base32
                "1fzi9d2gibh0wzwidyckzbywsxcsbckgsl05ryxlifxia77fhgyq"))
              (snippet
               '(begin
                  (for-each delete-file
                            '(;; This test may hang and eventually run out of
                              ;; memory on some systems:
                              ;; <https://bugs.python.org/issue34587>
                              "Lib/test/test_socket.py"

                              ;; These tests fail on AArch64.
                              "Lib/ctypes/test/test_win32.py"
                              "Lib/test/test_fcntl.py"
                              "Lib/test/test_posix.py"))
                  #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-2)
       ((#:phases phases)
       `(modify-phases ,phases
          ;; Unset SOURCE_DATE_EPOCH while running the test-suite and set it
          ;; again afterwards.  See <https://bugs.python.org/issue34022>.
          (add-before 'check 'unset-SOURCE_DATE_EPOCH
            (lambda _ (unsetenv "SOURCE_DATE_EPOCH") #t))
          (add-after 'check 'reset-SOURCE_DATE_EPOCH
            (lambda _ (setenv "SOURCE_DATE_EPOCH" "1") #t))
           (replace 'rebuild-bytecode
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; Disable hash randomization to ensure the generated .pycs
                 ;; are reproducible.
                 (setenv "PYTHONHASHSEED" "0")
                 (for-each
                  (lambda (opt)
                    (format #t "Compiling with optimization level: ~a\n"
                            (if (null? opt) "none" (car opt)))
                    (for-each (lambda (file)
                                (apply invoke
                                       `(,(string-append out "/bin/python3")
                                         ,@opt
                                         "-m" "compileall"
                                         "-f" ; force rebuild
                                         ;; Don't build lib2to3, because it's Python 2 code.
                                         "-x" "lib2to3/.*"
                                         ,file)))
                              (find-files out "\\.py$")))
                  (list '() '("-O") '("-OO")))
                 #t)))))))
    (native-search-paths
     (list (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append "lib/python"
                                        (version-major+minor version)
                                        "/site-packages"))))))))

;; Current 3.x version.
(define-public python-3 python-3.7)

;; Current major version.
(define-public python python-3)

;; Minimal variants of Python, mostly used to break the cycle between Tk and
;; Python (Tk -> libxcb -> Python.)

(define-public python2-minimal
  (package/inherit python-2
    (name "python2-minimal")
    (outputs '("out"))

    ;; Keep zlib, which is used by 'pip' (via the 'zipimport' module), which
    ;; is invoked upon 'make install'.  'pip' also expects 'ctypes' and thus
    ;; libffi.
    (inputs `(("libffi" ,libffi)
              ("zlib" ,zlib)))))

(define-public python-minimal
  (package/inherit python
    (name "python-minimal")
    (outputs '("out"))

    ;; Build fails due to missing ctypes without libffi.
    ;; OpenSSL is a mandatory dependency of Python 3.x, for urllib;
    ;; zlib is required by 'zipimport', used by pip.
    (inputs `(("libffi" ,libffi)
              ("openssl" ,openssl)
              ("zlib" ,zlib)))))

(define-public python-debug
  (package/inherit python
    (name "python-debug")
    (outputs '("out" "debug"))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments python)
       ((#:configure-flags flags '())
        `(cons "--with-pydebug" ,flags))))
    (synopsis
     "High-level, dynamically-typed programming language (for debugging)")
    (description
     "This variant of Python provides an interpreter built with
@code{--with-pydebug} to help develop and debug extensions.  See
@url{https://pythonextensionpatterns.readthedocs.io/en/latest/debugging/debug.html},
for more information.")))

(define* (wrap-python3 python
                       #:optional
                       (name (string-append (package-name python) "-wrapper")))
  (package/inherit python
    (name name)
    (source #f)
    (build-system trivial-build-system)
    (outputs '("out"))
    (inputs `(("bash" ,bash)))
    (propagated-inputs `(("python" ,python)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
         (begin
           (use-modules (guix build utils))
           (let ((bin (string-append (assoc-ref %outputs "out") "/bin"))
                 (python (string-append (assoc-ref %build-inputs "python") "/bin/")))
                (mkdir-p bin)
                (for-each
                  (lambda (old new)
                    (symlink (string-append python old)
                             (string-append bin "/" new)))
                  `("python3" ,"pydoc3" ,"idle3" ,"pip3")
                  `("python"  ,"pydoc"  ,"idle"  ,"pip"))
                ;; python-config outputs search paths based upon its location,
                ;; use a bash wrapper to avoid changing its outputs.
                (let ((bash (string-append (assoc-ref %build-inputs "bash")
                                           "/bin/bash"))
                      (old  (string-append python "python3-config"))
                      (new  (string-append bin "/python-config")))
                  (with-output-to-file new
                    (lambda ()
                      (format #t "#!~a~%" bash)
                      (format #t "exec \"~a\" \"$@\"~%" old)
                      (chmod new #o755)
                      #t)))))))
    (synopsis "Wrapper for the Python 3 commands")
    (description
     "This package provides wrappers for the commands of Python@tie{}3.x such
that they can be invoked under their usual name---e.g., @command{python}
instead of @command{python3}.")))

(define-public python-wrapper (wrap-python3 python))
(define-public python-minimal-wrapper (wrap-python3 python-minimal))

(define-public micropython
  (package
    (name "micropython")
    (version "1.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/micropython/micropython/"
                            "releases/download/v" version
                            "/micropython-" version ".tar.gz"))
        (sha256
         (base32
          "1g1zjip3rkx6bp16qi1bag72wivnbh56fcsl3nffanrx4j5f4z90"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          (delete-file-recursively "ports/cc3200/FreeRTOS")
          (with-directory-excursion "lib"
            ;; TODO: Unbundle axtls and berkley-db-1.xx
            (for-each delete-file-recursively
                      '("libffi" "lwip" "stm32lib" "nrfx")))
          #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'preprare-build
           (lambda _
             (chdir "ports/unix")
             ;; see: https://github.com/micropython/micropython/pull/4246
             (substitute* "Makefile"
               (("-Os") "-Os -ffp-contract=off"))
             #t))
         (replace 'install-license-files
           ;; We don't build in the root directory so the file isn't found.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (dest (string-append out "/share/doc/" ,name "-" ,version "/")))
               (install-file "../../LICENSE" dest))
             #t))
         (delete 'configure)) ; no configure
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "V=1")
       #:test-target "test"))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("libffi" ,libffi)))
    (home-page "https://micropython.org/")
    (synopsis "Python implementation for microcontrollers and constrained systems")
    (description "MicroPython is a lean and efficient implementation of the
Python 3 programming language that includes a small subset of the Python
standard library and is optimised to run on microcontrollers and in constrained
environments.  MicroPython is packed full of advanced features such as an
interactive prompt, arbitrary precision integers, closures, list comprehension,
generators, exception handling and more.  Still it is compact enough to fit and
run within just 256k of code space and 16k of RAM.  MicroPython aims to be as
compatible with normal Python as possible to allow you to transfer code with
ease from the desktop to a microcontroller or embedded system.")
    (license license:expat)))
