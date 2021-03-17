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
;;; Copyright © 2015, 2016, 2017, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015, 2016 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2015, 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016, 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Daniel Pimentel <d4n1@d4n1.org>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 Dylan Jeffers <sapientech@sapientech@openmailbox.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2016 Dylan Jeffers <sapientech@sapientech@openmailbox.org>
;;; Copyright © 2016, 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2016, 2017, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016, 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016–2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017, 2018 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2017, 2018, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017, 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Muriithi Frederick Muriuki <fredmanglis@gmail.com>
;;; Copyright © 2017 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2018 Ethan R. Jones <ethanrjones97@gmail.com
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Vijayalakshmi Vedantham <vijimay12@gmail.com>
;;; Copyright © 2018 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2018 Adam Massmann <massmannak@gmail.com>
;;; Copyright © 2016, 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018, 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018 Luther Thompson <lutheroto@gmail.com>
;;; Copyright © 2018 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public python-2.7
  (package
    (name "python2")
    (replacement python-2.7/fixed)
    (version "2.7.17")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.python.org/ftp/python/"
                          version "/Python-" version ".tar.xz"))
      (sha256
       (base32
        "0hds28cg226m8j8sr394nm9yc4gxhvlv109w0avsf2mxrlrz0hsd"))
      (patches (search-patches "python-2.7-search-paths.patch"
                               "python-2-deterministic-build-info.patch"
                               "python-2.7-site-prefixes.patch"
                               "python-2.7-source-date-epoch.patch"
                               "python-2.7-adjust-tests.patch"
                               "python-cross-compile.patch"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; Ensure the bundled copies of these libraries are not used.
          (for-each delete-file-recursively
                    '("Modules/_ctypes/libffi" "Modules/expat" "Modules/zlib"))

          (substitute* "Modules/Setup.dist"
            ;; Link Expat instead of embedding the bundled one.
            (("^#pyexpat.*") "pyexpat pyexpat.c -lexpat\n"))

          ;; Suboptimal to delete failing tests here, but if we delete them in
          ;; the arguments then we need to make sure to strip out that phase
          ;; when it gets inherited by python and python-minimal.
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
             "--with-system-expat"                ;for XML support
             "--with-system-ffi"                  ;build ctypes
             "--with-ensurepip=install"           ;install pip and setuptools
             "--enable-unicode=ucs4"

             ;; Prevent the installed _sysconfigdata.py from retaining a reference
             ;; to coreutils.
             "INSTALL=install -c"
             "MKDIR_P=mkdir -p"

             ;; Disable runtime check failing if cross-compiling, see:
             ;; https://lists.yoctoproject.org/pipermail/poky/2013-June/008997.html
             ,@(if (%current-target-system)
                   '("ac_cv_buggy_getaddrinfo=no"
                     "ac_cv_file__dev_ptmx=no"
                     "ac_cv_file__dev_ptc=no")
                   '())
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       ;; With no -j argument tests use all available cpus, so provide one.
       #:make-flags
       (list (string-append
              (format #f "TESTOPTS=-j~d" (parallel-job-count))
              ;; Exclude the following tests as they fail
              ;; non-deterministically with "error: [Errno 104] Connection
              ;; reset by peer."  Python 3 seems unaffected.  A potential fix,
              ;; yet to be backported to Python 2, is available at:
              ;; https://github.com/python/cpython/commit/529525fb5a8fd9b96ab4021311a598c77588b918.
              " --exclude test_urllib2_localnet test_httplib"))

        #:modules ((ice-9 ftw) (ice-9 match)
                   (guix build utils) (guix build gnu-build-system))
        #:phases
        (modify-phases %standard-phases
          (add-before
           'configure 'patch-lib-shells
           (lambda _
             ;; This variable is used in setup.py to enable cross compilation
             ;; specific switches. As it is not set properly by configure
             ;; script, set it manually.
             ,@(if (%current-target-system)
                   '((setenv "_PYTHON_HOST_PLATFORM" ""))
                   '())
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
          ,@(if (hurd-system?)
                `((add-before 'build 'patch-regen-for-hurd
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((libc (assoc-ref inputs "libc")))
                        (substitute* "Lib/plat-generic/regen"
                          (("/usr/include/") (string-append libc "/include/")))
                        #t))))
                '())
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
                                      `(,,(if (%current-target-system)
                                              "python2"
                                              '(string-append out "/bin/python"))
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
       ("expat" ,expat)
       ("gdbm" ,gdbm)
       ("libffi" ,libffi)                         ; for ctypes
       ("sqlite" ,sqlite)                         ; for sqlite extension
       ("openssl" ,openssl)
       ("readline" ,readline)
       ("zlib" ,zlib)
       ("tcl" ,tcl)
       ("tk" ,tk)))                               ; for tkinter
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ;; When cross-compiling, a native version of Python itself is needed.
       ,@(if (%current-target-system)
             `(("python2" ,this-package)
               ("which" ,which))
             '())))
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
    (properties '((cpe-name . "python")))
    (license license:psfl)))

(define python-2.7/fixed
  (package
    (inherit python-2.7)
    (source (origin
              (inherit (package-source python-2.7))
              (patches (append (search-patches "python-2.7-CVE-2021-3177.patch")
                               (origin-patches (package-source python-2.7))))))))

;; Current 2.x version.
(define-public python-2 python-2.7)

(define-public python2-called-python
  ;; Both 2.x and 3.x used to be called "python".  In commit
  ;; a7714d42de2c3082f3609d1e63c83d703fb39cf9 (March 2018), we renamed the
  ;; Python 2.x package to "python2".
  (package/inherit python-2
    (name "python")
    (properties `((superseded . ,python-2)))))

(define-public python-3.8
  (package (inherit python-2)
    (name "python")
    (replacement python-3.8/fixed)
    (version "3.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (patches (search-patches
                        "python-CVE-2020-26116.patch"
                        "python-3-fix-tests.patch"
                        "python-3.8-fix-tests.patch"
                        "python-3-deterministic-build-info.patch"
                        "python-3-search-paths.patch"))
              (sha256
               (base32
                "1ps5v323cp5czfshqjmbsqw7nvrdpcbk06f62jbzaqik4gfffii6"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete the bundled copy of libexpat.
                  (delete-file-recursively "Modules/expat")
                  (substitute* "Modules/Setup"
                    ;; Link Expat instead of embedding the bundled one.
                    (("^#pyexpat.*") "pyexpat pyexpat.c -lexpat\n"))
                  #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-2)
       ((#:make-flags _)
        `(list (string-append
                (format #f "TESTOPTS=-j~d" (parallel-job-count))
                ;; test_mmap fails on low-memory systems.
                " --exclude test_mmap"
                ;; test_socket may hang and eventually run out of memory
                ;; on some systems: <https://bugs.python.org/issue34587>.
                " test_socket"
                ,@(if (hurd-target?)
                      '(" test_posix"      ;multiple errors
                        " test_time"
                        " test_pty"
                        " test_shutil"
                        " test_tempfile"   ;chflags: invalid argument:
                                           ;  tbv14c9t/dir0/dir0/dir0/test0.txt
                        " test_asyncio"    ;runs over 10min
                        " test_os"         ;stty: 'standard input':
                                           ;  Inappropriate ioctl for device
                        " test_openpty"    ;No such file or directory
                        " test_selectors"  ;assertEqual(NUM_FDS // 2, len(fds))
                                           ;  32752 != 4
                        " test_compileall" ;multiple errors
                        " test_poll"       ;list index out of range
                        " test_subprocess" ;runs over 10min
                        " test_asyncore"   ;multiple errors
                        " test_threadsignals"
                        " test_eintr"      ;Process return code is -14
                        " test_io"         ;multiple errors
                        " test_logging"
                        " test_signal"
                        " test_threading"  ;runs over 10min
                        " test_flags"      ;ERROR
                        " test_bidirectional_pty"
                        " test_create_unix_connection"
                        " test_unix_sock_client_ops"
                        " test_open_unix_connection"
                        " test_open_unix_connection_error"
                        " test_read_pty_output"
                        " test_write_pty")
                      '()))))
       ((#:phases phases)
       `(modify-phases ,phases
          ,@(if (hurd-system?)
                `((delete 'patch-regen-for-hurd)) ;regen was removed after 3.5.9
                '())
          ,@(if (hurd-target?)
                ;; The build system refuses to cross-compile for unknown targets
                ;; even though it works fine.  Add GNU/Hurd target.
                ;; TODO: Make it a patch in a future rebuild cycle.
                '((add-before 'configure 'support-hurd-cross-compile
                    (lambda _
                      (substitute* "configure"
                        (("\\*-\\*-vxworks.*" all)
                         (string-append "*-*-gnu)\nac_sys_system=GNU\n;;\n" all)))
                      #t)))
                '())
          (add-before 'check 'set-TZDIR
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              ;; test_email requires the Olson time zone database.
              (setenv "TZDIR"
                      (string-append (assoc-ref
                                      (or native-inputs inputs) "tzdata")
                                     "/share/zoneinfo"))
              #t))
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
                                       `(,,(if (%current-target-system)
                                               "python3"
                                               '(string-append out
                                                               "/bin/python3"))
                                          ,@opt
                                          "-m" "compileall"
                                          "-f" ; force rebuild
                                          ;; Don't build lib2to3, because it's Python 2 code.
                                          "-x" "lib2to3/.*"
                                          ,file)))
                              (find-files out "\\.py$")))
                  (list '() '("-O") '("-OO")))
                 #t)))
           ;; XXX: Apply patch on ARM platforms only to avoid a full rebuild.
           ;; Remove this phase in the next rebuild cycle.
           ,@(let ((system (or (%current-target-system)
                               (%current-system))))
               (if (any (cute string-prefix? <> system)
                        '("arm" "aarch64"))
                   '((add-after 'unpack 'apply-alignment-patch
                       (lambda* (#:key native-inputs inputs #:allow-other-keys)
                        (invoke "patch" "-p1" "--force" "--input"
                                (assoc-ref (or native-inputs inputs)
                                           "arm-alignment.patch")))))
                   '()))))))
    (native-inputs
     `(("tzdata" ,tzdata-for-tests)

       ;; Disable unaligned accesses in the sha3 module on ARM as
       ;; it causes a test failure when building 32-bit Python on a
       ;; 64-bit kernel.  See <https://bugs.python.org/issue36515>.
       ;; TODO: make this a regular patch in the next rebuild cycle.
       ,@(let ((system (or (%current-target-system)
                           (%current-system))))
           (if (any (cute string-prefix? <> system)
                    '("arm" "aarch64"))
               `(("arm-alignment.patch" ,(search-patch "python-3-arm-alignment.patch")))
               '()))

       ,@(if (%current-target-system)
             `(("python3" ,this-package))
             '())
       ,@(package-native-inputs python-2)))
    (native-search-paths
     (list (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append "lib/python"
                                        (version-major+minor version)
                                        "/site-packages"))))))))

(define python-3.8/fixed
  (package
    (inherit python-3.8)
    (source (origin
              (inherit (package-source python-3.8))
              (patches (append (search-patches "python-3.8-CVE-2021-3177.patch")
                               (origin-patches (package-source python-3.8))))))))

(define-public python-3.9
  (package (inherit python-3.8)
    (name "python-next")
    (version "3.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (patches (search-patches
                        "python-3.9-fix-tests.patch"
                        "python-3-deterministic-build-info.patch"
                        "python-3-search-paths.patch"))
              (sha256
               (base32
                "0z94vv5qhlwvcgc4sy9sdiqs0220s84wx3b62vslh5419z2k881w"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete the bundled copy of libexpat.
                  (delete-file-recursively "Modules/expat")
                  (substitute* "Modules/Setup"
                    ;; Link Expat instead of embedding the bundled one.
                    (("^#pyexpat.*") "pyexpat pyexpat.c -lexpat\n"))
                  #t))))))

;; Current 3.x version.
(define-public python-3 python-3.8)

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
    ;; libffi.  Expat is needed for XML support which is expected by a lot
    ;; of libraries out there.
    (inputs `(("expat" ,expat)
              ("libffi" ,libffi)
              ("zlib" ,zlib)))))

(define-public python-minimal
  (package/inherit python
    (name "python-minimal")
    (outputs '("out"))

    ;; Build fails due to missing ctypes without libffi.
    ;; OpenSSL is a mandatory dependency of Python 3.x, for urllib;
    ;; zlib is required by 'zipimport', used by pip.  Expat is needed
    ;; for XML support, which is generally expected to be available.
    (inputs `(("expat" ,expat)
              ("libffi" ,libffi)
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
instead of @command{python3} or @command{pip} instead of @command{pip3}.
To function properly, this package should not be installed together with the
@command{python} package.")))

(define-public python-wrapper (wrap-python3 python))
(define-public python-minimal-wrapper (wrap-python3 python-minimal))

(define-public micropython
  (package
    (name "micropython")
    (version "1.14")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/micropython/micropython/"
                            "releases/download/v" version
                            "/micropython-" version ".tar.xz"))
        (sha256
         (base32 "0k6ri3rxxnnmvcbi7z7x59r21f4vj9dcf9j64jhj1cgazmb62c4p"))
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
         (add-before 'build 'build-mpy-cross
           (lambda* (#:key make-flags #:allow-other-keys)
             (with-directory-excursion "mpy-cross"
               (apply invoke "make" make-flags))))
         (add-after 'build-mpy-cross 'prepare-build
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

(define-public pypy3
  (package
    (name "pypy3")
    (version "7.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/pypy/pypy/downloads/" ;
                                  "pypy3.6-v" version "-src.tar.bz2"))
              (sha256
               (base32
                "10zsk8jby8j6visk5mzikpb1cidvz27qq4pfpa26jv53klic6b0c"))
              (patches (search-patches "pypy3-7.3.1-fix-tests.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python-2" ,python-2)
       ("pkg-config" ,pkg-config)
       ("tar" ,tar)                     ; Required for package.py
       ("python2-pycparser" ,python2-pycparser)
       ("python2-hypothesis" ,python2-hypothesis)
       ("nss-certs" ,nss-certs)         ; For ssl tests
       ("gzip" ,gzip)))
    (inputs
     `(("libffi" ,libffi)
       ("zlib" ,zlib)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("expat" ,expat)
       ("bzip2" ,bzip2)
       ("sqlite" ,sqlite)
       ("gdbm" ,gdbm)
       ("tcl" ,tcl)
       ("tk" ,tk)
       ("glibc" ,glibc)
       ("bash-minimal" ,bash-minimal)   ; Used as /bin/sh
       ("xz" ,xz)))                     ; liblzma
    (arguments
     `(#:tests? #f     ;FIXME: Disabled for now, there are many tests failing.
       #:modules ((ice-9 ftw) (ice-9 match)
                  (guix build utils) (guix build gnu-build-system))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-source
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (substitute* '("rpython/rlib/clibffi.py")
                        ;; find_library does not work for libc
                        (("ctypes\\.util\\.find_library\\('c'\\)") "'libc.so'"))
                      (substitute* '("lib_pypy/cffi/_pycparser/ply/cpp.py")
                        ;; Make reproducible (XXX: unused?)
                        (("time\\.localtime\\(\\)") "time.gmtime(0)"))
                      (substitute* '("pypy/module/sys/version.py")
                        ;; Make reproducible
                        (("t\\.gmtime\\(\\)") "t.gmtime(0)"))
                      (substitute* '("lib_pypy/_tkinter/tklib_build.py")
                        ;; Link to versioned libtcl and libtk
                        (("linklibs = \\['tcl', 'tk'\\]")
                         "linklibs = ['tcl8.6', 'tk8.6']")
                        (("incdirs = \\[\\]")
                         (string-append "incdirs = ['"
                                        (assoc-ref inputs "tcl")
                                        "/include', '"
                                        (assoc-ref inputs "tk")
                                        "/include']")))
                      (substitute* '("lib_pypy/_curses_build.py")
                        ;; Find curses
                        (("/usr/local") (assoc-ref inputs "ncurses")))
                      (substitute* '("lib_pypy/_sqlite3_build.py")
                        ;; Always use search paths
                        (("sys\\.platform\\.startswith\\('freebsd'\\)") "True")
                        ;; Find sqlite3
                        (("/usr/local") (assoc-ref inputs "sqlite"))
                        (("libname = 'sqlite3'")
                         (string-append "libname = '"
                                        (assoc-ref inputs "sqlite")
                                        "/lib/libsqlite3.so.0'")))
                      (substitute* '("lib-python/3/subprocess.py")
                        ;; Fix shell path
                        (("/bin/sh")
                         (string-append (assoc-ref inputs "bash-minimal") "/bin/sh")))
                      (substitute* '("lib-python/3/distutils/unixccompiler.py")
                        ;; gcc-toolchain does not provide symlink cc -> gcc
                        (("\"cc\"") "\"gcc\""))
                      #t))
                  (add-after
                      'unpack 'set-source-file-times-to-1980
                    ;; copied from python package, required by zip testcase
                    (lambda _
                      (let ((circa-1980 (* 10 366 24 60 60)))
                        (ftw "." (lambda (file stat flag)
                                   (utime file circa-1980 circa-1980)
                                   #t))
                        #t)))
                  (replace 'build
                    (lambda* (#:key inputs #:allow-other-keys)
                      (with-directory-excursion "pypy/goal"
                        ;; Build with jit optimization.
                        (invoke "python2"
                                "../../rpython/bin/rpython"
                                (string-append "--make-jobs="
                                               (number->string (parallel-job-count)))
                                "-Ojit"
                                "targetpypystandalone"))
                      ;; Build c modules and package everything, so tests work.
                      (with-directory-excursion "pypy/tool/release"
                        (unsetenv "PYTHONPATH") ; Do not use the system’s python libs:
                                        ; AttributeError: module 'enum' has no
                                        ; attribute 'IntFlag'
                        (invoke "python2" "package.py"
                                "--archive-name" "pypy-dist"
                                "--builddir" (getcwd)))))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (begin
                            (setenv "HOME" "/tmp") ; test_with_pip tries to
                                        ; access ~/.cache/pip
                            ;; Run library tests only (no interpreter unit
                            ;; tests). This is what Gentoo does.
                            (invoke
                             "python2"
                             "pypy/test_all.py"
                             "--pypy=pypy/tool/release/pypy-dist/bin/pypy3"
                             "lib-python"))
                          (format #t "test suite not run~%"))
                      #t))
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (with-directory-excursion "pypy/tool/release"
                        ;; Delete test data.
                        (for-each
                         (lambda (x)
                           (delete-file-recursively (string-append
                                                     "pypy-dist/lib-python/3/" x)))
                         '("tkinter/test"
                           "test"
                           "sqlite3/test"
                           "lib2to3/tests"
                           "idlelib/idle_test"
                           "distutils/tests"
                           "ctypes/test"
                           "unittest/test"))
                        ;; Patch shebang referencing python2
                        (substitute* '("pypy-dist/lib-python/3/cgi.py"
                                       "pypy-dist/lib-python/3/encodings/rot_13.py")
                          (("#!.+/bin/python")
                           (string-append "#!" (assoc-ref outputs "out") "/bin/pypy3")))
                        (with-fluids ((%default-port-encoding "ISO-8859-1"))
                          (substitute* '("pypy-dist/lib_pypy/_md5.py"
                                         "pypy-dist/lib_pypy/_sha1.py")
                            (("#!.+/bin/python")
                             (string-append "#!" (assoc-ref outputs "out") "/bin/pypy3"))))
                        (copy-recursively "pypy-dist" (assoc-ref outputs "out")))
                      #t)))))
    (home-page "https://www.pypy.org/")
    (synopsis "Python implementation with just-in-time compilation")
    (description "PyPy is a faster, alternative implementation of the Python
programming language employing a just-in-time compiler.  It supports most
Python code natively, including C extensions.")
    (license (list license:expat        ; pypy itself; _pytest/
                   license:psfl ; python standard library in lib-python/
                   license:asl2.0 ; dotviewer/font/ and some of lib-python/
                   license:gpl3+ ; ./rpython/rlib/rvmprof/src/shared/libbacktrace/dwarf2.*
                   license:bsd-3 ; lib_pypy/cffi/_pycparser/ply/
                   (license:non-copyleft
                    "http://www.unicode.org/copyright.html")))))

