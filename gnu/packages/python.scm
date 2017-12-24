;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Omar Radwan <toxemicsquire4@gmail.com>
;;; Copyright © 2015 Pierre-Antoine Rault <par@rigelk.eu>
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015, 2016 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2015, 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2015, 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Daniel Pimentel <d4n1@d4n1.org>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016, 2017 ng0 <ng0@infotropique.org>
;;; Copyright © 2016 Dylan Jeffers <sapientech@sapientech@openmailbox.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2016 Dylan Jeffers <sapientech@sapientech@openmailbox.org>
;;; Copyright © 2016, 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2016, 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016, 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016, 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Muriithi Frederick Muriuki <fredmanglis@gmail.com>
;;; Copyright © 2017 Brendan Tildesley <brendan.tildesley@openmailbox.org>
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
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages search)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages bdw-gc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public python-2.7
  (package
    (name "python")
    (version "2.7.13")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.python.org/ftp/python/"
                          version "/Python-" version ".tar.xz"))
      (sha256
       (base32
        "0cgpk3zk0fgpji59pb4zy9nzljr70qzgv1vpz5hq5xw2d2c47m9m"))
      (patches (search-patches "python-2.7-search-paths.patch"
                               "python-2-deterministic-build-info.patch"
                               "python-2.7-site-prefixes.patch"
                               "python-2.7-source-date-epoch.patch"
                               "python-2.7-getentropy-on-old-kernels.patch"))
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
     `(;; 356 tests OK.
       ;; 6 tests failed:
       ;;     test_compileall test_distutils test_import test_shutil test_socket
       ;;     test_subprocess
       ;; 39 tests skipped:
       ;;     test_aepack test_al test_applesingle test_bsddb test_bsddb185
       ;;     test_bsddb3 test_cd test_cl test_codecmaps_cn test_codecmaps_hk
       ;;     test_codecmaps_jp test_codecmaps_kr test_codecmaps_tw test_curses
       ;;     test_dl test_gdb test_gl test_imageop test_imgfile test_ioctl
       ;;     test_kqueue test_linuxaudiodev test_macos test_macostools
       ;;     test_msilib test_ossaudiodev test_scriptpackages test_smtpnet
       ;;     test_socketserver test_startfile test_sunaudiodev test_timeout
       ;;     test_tk test_ttk_guionly test_urllib2net test_urllibnet
       ;;     test_winreg test_winsound test_zipfile64
       ;; 4 skips unexpected on linux2:
       ;;     test_bsddb test_bsddb3 test_gdb test_ioctl
       #:test-target "test"
       #:configure-flags
       (list "--enable-shared"                    ;allow embedding
             "--with-system-ffi"                  ;build ctypes
             "--with-ensurepip=install"           ;install pip and setuptools
             "--enable-unicode=ucs4"
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))

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
                                    "Lib/test/test_subprocess.py"))
               (("/bin/sh") (which "sh")))

             ;; Use zero as the timestamp in .pyc files so that builds are
             ;; deterministic.  TODO: Remove it when this variable is set in
             ;; gnu-build-system.scm.
             (setenv "SOURCE_DATE_EPOCH" "1")
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
                                            (file
                                             (not
                                              (string-prefix? "test_support."
                                                              file))))))
                       (call-with-output-file "__init__.py" (const #t))
                       #t)))))))
          (add-before 'strip 'make-libraries-writable
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Make .so files writable so they can be stripped.
              (let ((out (assoc-ref outputs "out")))
                (for-each (lambda (file)
                            (chmod file #o755))
                          (find-files (string-append out "/lib")
                                      "\\.so"))
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

(define-public python-3.5
  (package (inherit python-2)
    (version "3.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (patches (search-patches
                        "python-fix-tests.patch"
                        "python-3.5-fix-tests.patch"
                        "python-3.5-getentropy-on-old-kernels.patch"
                        "python-3-deterministic-build-info.patch"
                        "python-3-search-paths.patch"))
              (patch-flags '("-p0"))
              (sha256
               (base32
                "1c6v1n9nz4mlx9mw1125fxpmbrgniqdbbx9hnqx44maqazb2mzpf"))
              (snippet
               '(begin
                  (for-each delete-file
                            '("Lib/ctypes/test/test_win32.py" ; fails on aarch64
                              "Lib/test/test_fcntl.py"))
                  #t))))
    (arguments (substitute-keyword-arguments (package-arguments python-2)
                 ((#:tests? _) #t)))
    (native-search-paths
     (list (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append "lib/python"
                                        (version-major+minor version)
                                        "/site-packages"))))))))

;; Current 3.x version.
(define-public python-3 python-3.5)

;; Current major version.
(define-public python python-3)

;; Minimal variants of Python, mostly used to break the cycle between Tk and
;; Python (Tk -> libxcb -> Python.)

(define-public python2-minimal
  (package (inherit python-2)
    (name "python-minimal")
    (outputs '("out"))

    ;; Keep zlib, which is used by 'pip' (via the 'zipimport' module), which
    ;; is invoked upon 'make install'.  'pip' also expects 'ctypes' and thus
    ;; libffi.
    (inputs `(("libffi" ,libffi)
              ("zlib" ,zlib)))))

(define-public python-minimal
  (package (inherit python)
    (name "python-minimal")
    (outputs '("out"))

    ;; Build fails due to missing ctypes without libffi.
    ;; OpenSSL is a mandatory dependency of Python 3.x, for urllib;
    ;; zlib is required by 'zipimport', used by pip.
    (inputs `(("libffi" ,libffi)
              ("openssl" ,openssl)
              ("zlib" ,zlib)))))

(define* (wrap-python3 python
                       #:optional
                       (name (string-append (package-name python) "-wrapper")))
  (package (inherit python)
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

(define-public python-psutil
  (package
    (name "python-psutil")
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "psutil" version))
       (sha256
        (base32
         "1w4r09fvn6kd80m5mx4ws1wz100brkaq6hzzpwrns8cgjzjpl6c6"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: some tests does not return and times out.
     '(#:tests? #f))
    (home-page "https://www.github.com/giampaolo/psutil")
    (synopsis "Library for retrieving information on running processes")
    (description
     "psutil (Python system and process utilities) is a library for retrieving
information on running processes and system utilization (CPU, memory, disks,
network) in Python.  It is useful mainly for system monitoring, profiling and
limiting process resources and management of running processes.  It implements
many functionalities offered by command line tools such as: ps, top, lsof,
netstat, ifconfig, who, df, kill, free, nice, ionice, iostat, iotop, uptime,
pidof, tty, taskset, pmap.")
    (license license:bsd-3)))

(define-public python2-psutil
  (package-with-python2 python-psutil))

(define-public python-clyent
  (package
    (name "python-clyent")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "clyent" version))
       (sha256
        (base32
         "1r9987qmy1pz3hq54160bapqsywpq14waw4w9x3ly8hmq7kpgfbj"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)))
    (home-page "https://github.com/binstar/clyent")
    (synopsis "Command line client library")
    (description "Clyent is a Python command line utiliy library.  It is used
by @code{binstar}, @code{binstar-build} and @code{chalmers}.")
    (license license:bsd-3)))

(define-public python2-clyent
  (package-with-python2 python-clyent))

(define-public python-babel
  (package
    (name "python-babel")
    (version "2.3.4")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "Babel" version))
      (sha256
       (base32
        "0x98qqqw35xllpcama013a9788ly84z8dm1w2wwfpxh2710c8df5"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytz" ,python-pytz)))
    (arguments `(#:tests? #f)) ; no test target
    (home-page "http://babel.pocoo.org/")
    (synopsis
     "Tools for internationalizing Python applications")
    (description
     "Babel is composed of two major parts:
- tools to build and work with gettext message catalogs
- a Python interface to the CLDR (Common Locale Data Repository), providing
access to various locale display names, localized number and date formatting,
etc. ")
    (license license:bsd-3)))

(define-public python2-babel
  (package-with-python2 python-babel))

(define-public python2-backport-ssl-match-hostname
  (package
    (name "python2-backport-ssl-match-hostname")
    (version "3.5.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "https://pypi.python.org/packages/source/b/"
            "backports.ssl_match_hostname/backports.ssl_match_hostname-"
            version ".tar.gz"))
      (sha256
       (base32
        "1wndipik52cyqy0677zdgp90i435pmvwd89cz98lm7ri0y3xjajh"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f)) ; no test target
    (home-page "https://bitbucket.org/brandon/backports.ssl_match_hostname")
    (synopsis "Backport of ssl.match_hostname() function from Python 3.5")
    (description
     "This backport brings the ssl.match_hostname() function to users of
earlier versions of Python.  The function checks the hostname in the
certificate returned by the server to which a connection has been established,
and verifies that it matches the intended target hostname.")
    (license license:psfl)))

(define-public python-hdf4
  (package
   (name "python-hdf4")
   (version "0.9")
   (source
    (origin
      (method url-fetch)
      (uri (pypi-uri name version))
      (sha256
       (base32
        "1hjiyrxvxk9817qyqky3nar4y3fs4z8wxz0n884zzb5wi6skrjks"))))
   (build-system python-build-system)
   (native-inputs `(("nose" ,python-nose)))
   (propagated-inputs `(("numpy" ,python-numpy)))
   (inputs
    `(("hdf4" ,hdf4)
      ("libjpeg" ,libjpeg)
      ("zlib" ,zlib)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda _
            ;; The 'runexamples' script sets PYTHONPATH to CWD, then goes
            ;; on to import numpy. Somehow this works on their CI system.
            ;; Let's just manage PYTHONPATH here instead.
            (substitute* "runexamples.sh"
              (("export PYTHONPATH=.*") ""))
            (setenv "PYTHONPATH"
                    (string-append (getcwd) ":"
                                   (getenv "PYTHONPATH")))
            (and (zero? (system* "./runexamples.sh"))
                 (zero? (system* "nosetests" "-v"))))))))
   (home-page "https://github.com/fhs/python-hdf4")
   (synopsis "Python interface to the NCSA HDF4 library")
   (description
    "Python-HDF4 is a python wrapper around the NCSA HDF version 4 library,
which implements the SD (Scientific Dataset), VS (Vdata) and V (Vgroup) API’s.
NetCDF files can also be read and modified.  Python-HDF4 is a fork of
@url{http://hdfeos.org/software/pyhdf.php,pyhdf}.")
   (license license:expat)))

(define-public python2-hdf4
  (package-with-python2 python-hdf4))

(define-public python-h5py
  (package
    (name "python-h5py")
    (version "2.7.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "h5py" version))
      (sha256
       (base32
        "0433sdv6xc9p7v1xs1gvbxp7p152ywi3nplgjb258q9fvw9469br"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-hdf5-paths
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((prefix (assoc-ref inputs "hdf5")))
              (substitute* "setup_build.py"
                (("\\['/opt/local/lib', '/usr/local/lib'\\]")
                 (string-append "['" prefix "/lib" "']"))
                (("'/opt/local/include', '/usr/local/include'")
                 (string-append "'" prefix "/include" "'")))
              (substitute* "setup_configure.py"
                (("\\['/usr/local/lib', '/opt/local/lib'\\]")
                 (string-append "['" prefix "/lib" "']")))
              #t))))))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-numpy" ,python-numpy)))
    (inputs
     `(("hdf5" ,hdf5)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-pkgconfig" ,python-pkgconfig)))
    (home-page "http://www.h5py.org/")
    (synopsis "Read and write HDF5 files from Python")
    (description
     "The h5py package provides both a high- and low-level interface to the
HDF5 library from Python.  The low-level interface is intended to be a
complete wrapping of the HDF5 API, while the high-level component supports
access to HDF5 files, datasets and groups using established Python and NumPy
concepts.")
    (license license:bsd-3)))

(define-public python2-h5py
  (package-with-python2 python-h5py))

(define-public python-netcdf4
  (package
    (name "python-netcdf4")
    (version "1.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "netCDF4" version))
       (sha256
        (base32
         "1h6jq338amlbk0ilzvjyl7cck80i0bah9a5spn9in71vy2qxm7i5"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-cython" ,python-cython)))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
    (inputs
     `(("netcdf" ,netcdf)
       ("hdf4" ,hdf4)
       ("hdf5" ,hdf5)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "NO_NET" "1") ; disable opendap tests
             (with-directory-excursion "test"
               (setenv "PYTHONPATH" ; find and add the library we just built
                       (string-append
                        (car (find-files "../build" "lib.*"
                                         #:directories? #:t
                                         #:fail-on-error? #:t))
                        ":" (getenv "PYTHONPATH")))
               (zero? (system* "python" "run_all.py"))))))))
    (home-page
     "https://github.com/Unidata/netcdf4-python")
    (synopsis "Python/numpy interface to the netCDF library")
    (description "Netcdf4-python is a Python interface to the netCDF C
library.  netCDF version 4 has many features not found in earlier
versions of the library and is implemented on top of HDF5.  This module
can read and write files in both the new netCDF 4 and the old netCDF 3
format, and can create files that are readable by HDF5 clients.  The
API is modelled after @code{Scientific.IO.NetCDF}, and should be familiar
to users of that module.")
    ;; The software is mainly ISC, but includes some files covered
    ;; by the Expat license.
    (license (list license:isc license:expat))))

(define-public python2-netcdf4
  (package-with-python2 python-netcdf4))

(define-public python-lockfile
  (package
    (name "python-lockfile")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/l/lockfile/"
                           "lockfile-" version ".tar.gz"))
       (sha256
        (base32
         "16gpx5hm73ah5n1079ng0vy381hl802v606npkx4x8nb0gg05vba"))))
    (build-system python-build-system)
    (arguments '(#:test-target "check"))
    (native-inputs
     `(("python-pbr" ,python-pbr)))
    (home-page "https://launchpad.net/pylockfile")
    (synopsis "Platform-independent file locking module")
    (description
     "The lockfile package exports a LockFile class which provides a simple
API for locking files.")
    (license license:expat)))

(define-public python2-lockfile
  (package-with-python2 python-lockfile))

(define-public python-setuptools
  (package
    (name "python-setuptools")
    (version "31.0.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "setuptools" version))
      (sha256
       (base32
        "0ypybh4hx3bv4vhg2dc74xpj1g56ggnaffm87k4abhwjwq6wq608"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; Remove included binaries which are used to build self-extracting
          ;; installers for Windows.
          ;; TODO: Find some way to build them ourself so we can include them.
          (for-each delete-file (find-files "setuptools" "^(cli|gui).*\\.exe$"))
          #t))))
    (build-system python-build-system)
    ;; FIXME: Tests require pytest, which itself relies on setuptools.
    ;; One could bootstrap with an internal untested setuptools.
    (arguments
     `(#:tests? #f))
    (home-page "https://pypi.python.org/pypi/setuptools")
    (synopsis
     "Library designed to facilitate packaging Python projects")
    (description
     "Setuptools is a fully-featured, stable library designed to facilitate
packaging Python projects, where packaging includes:
Python package and module definitions,
distribution package metadata,
test hooks,
project installation,
platform-specific details,
Python 3 support.")
    ;; TODO: setuptools now bundles the following libraries:
    ;; packaging, pyparsing, six and appdirs. How to unbundle?
    (license (list license:psfl        ; setuptools itself
                   license:expat       ; six, appdirs, pyparsing
                   license:asl2.0      ; packaging is dual ASL2/BSD-2
                   license:bsd-2))))

(define-public python2-setuptools
  (package-with-python2 python-setuptools))

(define-public python-uniseg
  (package
    (name "python-uniseg")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bitbucket.org/emptypage/uniseg-python/"
                           "get/rel-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1df4gddnj2a0v8z35wb2ra5vvh1f1qyxs8fgd25c8g64031mna6x"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; The test suite requires network access.
    (home-page
     "https://bitbucket.org/emptypage/uniseg-python")
    (synopsis
     "Python library to determine Unicode text segmentations")
    (description
     "Uniseg is a Python package used to determine Unicode text segmentations.
Supported segmentations include:
@enumerate
@item @dfn{Code point} (any value in the Unicode codespace)
@item @dfn{Grapheme cluster} (user-perceived character made of a single or
multiple Unicode code points, e.g. \"G\" + acute-accent)
@item Word break
@item Sentence break
@item Line break
@end enumerate")
    (license license:expat)))

(define-public python2-uniseg
  (package-with-python2 python-uniseg))

(define-public python-humanfriendly
  (package
    (name "python-humanfriendly")
    (version "4.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "humanfriendly" version))
       (sha256
        (base32
         "0pisgizjql86785jchfjv217g0lsgk114g2lja5j4y3lsc3b9szi"))))
    (build-system python-build-system)
    (arguments
     `(;; XXX: Tests depend on coloredlogs, which in turn depends on humanfriendly.
       #:tests? #f))
    (propagated-inputs
     `(("python-monotonic" ,python-monotonic)))
    (home-page "https://humanfriendly.readthedocs.io")
    (synopsis "Human-friendly input and output in Python")
    (description
     "The functions and classes in @code{humanfriendly} can be used to make
text interfaces more user-friendly.  It includes tools to parse and format
numbers, file sizes, and timespans, timers for long-running operations, menus
to allow the user to choose from a list of options, and terminal interaction
helpers.")
    (license license:expat)))

(define-public python2-humanfriendly
  (package-with-python2 python-humanfriendly))

(define-public python-capturer
  (package
    (name "python-capturer")
    (version "2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "capturer" version))
       (sha256
        (base32
         "05d6ji4j8ipiq0br7bwam38qc6hd9l1djmfxlzrxx19ziyjl4089"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("python-humanfriendly" ,python-humanfriendly)))
    (home-page "https://capturer.readthedocs.io")
    (synopsis "Capture stdout and stderr streams of the current process")
    (description
     "The capturer package makes it easy to capture the stdout and stderr
streams of the current process and subprocesses.  Output can be relayed
to the terminal in real time but is also available to the Python program
for additional processing.")
    (license license:expat)))

(define-public python2-capturer
  (package-with-python2 python-capturer))

(define-public python-verboselogs
  (package
    (name "python-verboselogs")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "verboselogs" version))
       (sha256
        (base32
         "09z4d1jiasn7k1hs5af2ckmnrd0i1d1m04bhfjhv7z6svzfdwgg3"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-astroid" ,python-astroid)
       ("python-pylint" ,python-pylint)))
    (home-page "https://verboselogs.readthedocs.io")
    (synopsis "Verbose logging level for Python's logging module")
    (description
     "The @code{verboselogs} package extends Python's @code{logging} module to
add the log levels NOTICE, SPAM, SUCCESS and VERBOSE.")
    (license license:expat)))

(define-public python2-verboselogs
  (package-with-python2 python-verboselogs))

(define-public python-coloredlogs
  (package
    (name "python-coloredlogs")
    (version "7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "coloredlogs" version))
       (sha256
        (base32
         "1blcann6dyg5dhps9pg12rn0q0rjrlajpmmil0gy0j4cbvnl2il9"))))
    (build-system python-build-system)
    (arguments
     `(;Tests require some updated modules
       #:tests? #f))
    (propagated-inputs
     `(("python-capturer" ,python-capturer)))
    (home-page "https://coloredlogs.readthedocs.io")
    (synopsis "Colored stream handler for Python's logging module")
    (description
     "The @code{coloredlogs} package enables colored terminal output for
Python's logging module.  The @code{ColoredFormatter} class inherits from
@code{logging.Formatter} and uses ANSI escape sequences to render your logging
messages in color.")
    (license license:expat)))

(define-public python2-coloredlogs
  (package-with-python2 python-coloredlogs))

(define-public python-eventlet
  (package
    (name "python-eventlet")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "eventlet" version))
       (sha256
        (base32
         "0f3q55mq4n021wb7qa53pz3ix6i2py64sap66vsaqm2scjw83m9s"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-greenlet" ,python-greenlet)))
    (arguments
     ;; TODO: Requires unpackaged 'enum-compat'.
     '(#:tests? #f))
    (home-page "http://eventlet.net")
    (synopsis "Concurrent networking library for Python")
    (description
     "Eventlet is a concurrent networking library for Python that
allows you to change how you run your code, not how you write it.
It uses @code{epoll} or @code{libevent} for highly scalable non-blocking I/O.
Coroutines ensure that the developer uses a blocking style of programming
that is similar to threading, but provide the benefits of non-blocking I/O.
The event dispatch is implicit, which means you can easily use @code{Eventlet}
from the Python interpreter, or as a small part of a larger application.")
  (license license:expat)))

(define-public python2-eventlet
  (let ((base (package-with-python2
                (strip-python2-variant python-eventlet))))
    (package (inherit base)
      (propagated-inputs
       `(("python2-enum34" ,python2-enum34)
         ,@(package-propagated-inputs base))))))

(define-public python-six
  (package
    (name "python-six")
    (version "1.10.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "six" version))
      (sha256
       (base32
        "0snmb8xffb3vsma0z67i0h0w2g2dy0p3gsgh9gi4i0kgc5l8spqh"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-py" ,python-py)
       ("python-pytest" ,python-pytest)))
    (home-page "https://pypi.python.org/pypi/six/")
    (synopsis "Python 2 and 3 compatibility utilities")
    (description
     "Six is a Python 2 and 3 compatibility library.  It provides utility
functions for smoothing over the differences between the Python versions with
the goal of writing Python code that is compatible on both Python versions.
Six supports every Python version since 2.5.  It is contained in only one
Python file, so it can be easily copied into your project.")
    (license license:x11)))

(define-public python2-six
  (package-with-python2 python-six))

(define-public python-schedule
  (package
    (name "python-schedule")
    (version "0.4.3")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "schedule" version))
      (sha256
       (base32
        "0vplyjcbfrq50sphlwya749z8p2pcyi2nycw3518i0qpd9a6189i"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-mock" ,python-mock)))
    (home-page "https://github.com/dbader/schedule")
    (synopsis "Schedule periodic function calls in Python")
    (description
     "Schedule is an in-process scheduler for periodic jobs that uses the
builder pattern for configuration.  Schedule lets you run Python functions (or
any other callable) periodically at pre-determined intervals using a simple,
human-friendly syntax.")
    (license license:expat)))

(define-public python2-schedule
  (package-with-python2 python-schedule))

(define-public python-pandas
  (package
    (name "python-pandas")
    (version "0.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandas" version))
       (sha256
        (base32 "0540cnbwy2hc4hv2sxfs8i47xi91qzvzxfn80dl785ibiicly3vg"))
       (patches
        (search-patches "python-pandas-skip-failing-tests.patch"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pytz" ,python-pytz)
       ("python-dateutil" ,python-dateutil)))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-cython" ,python-cython)))
    (home-page "http://pandas.pydata.org")
    (synopsis "Data structures for data analysis, time series, and statistics")
    (description
     "Pandas is a Python package providing fast, flexible, and expressive data
structures designed to make working with structured (tabular,
multidimensional, potentially heterogeneous) and time series data both easy
and intuitive.  It aims to be the fundamental high-level building block for
doing practical, real world data analysis in Python.")
    (license license:bsd-3)))

(define-public python2-pandas
  (package-with-python2 python-pandas))

(define-public python2-mechanize
  (package
    (name "python2-mechanize")
    (version "0.2.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/m/mechanize/mechanize-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0rj7r166i1dyrq0ihm5rijfmvhs8a04im28lv05c0c3v206v4rrf"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; apparently incompatible with Python 3
       #:tests? #f))
         ;; test fails with message
         ;; AttributeError: 'module' object has no attribute 'test_pullparser'
         ;; (python-3.3.2) or
         ;; AttributeError: 'module' object has no attribute 'test_urllib2_localnet'
         ;; (python-2.7.5).
         ;; The source code is from March 2011 and probably not up-to-date
         ;; with respect to python unit tests.
    (home-page "http://wwwsearch.sourceforge.net/mechanize/")
    (synopsis
     "Stateful programmatic web browsing in Python")
    (description
     "Mechanize implements stateful programmatic web browsing in Python,
after Andy Lester’s Perl module WWW::Mechanize.")
    (license (license:non-copyleft
              "file://COPYING"
              "See COPYING in the distribution."))))


(define-public python-simplejson
  (package
    (name "python-simplejson")
    (version "3.10.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "simplejson" version))
      (sha256
       (base32
        "1qhwsykjlb85igb4cfl6v6gkprzbbg8gyqdd7zscc8w3x0ifcfwm"))))
    (build-system python-build-system)
    (home-page "http://simplejson.readthedocs.org/en/latest/")
    (synopsis
     "Json library for Python")
    (description
     "JSON (JavaScript Object Notation) is a subset of JavaScript
syntax (ECMA-262 3rd edition) used as a lightweight data interchange
format.

Simplejson exposes an API familiar to users of the standard library marshal
and pickle modules.  It is the externally maintained version of the json
library contained in Python 2.6, but maintains compatibility with Python 2.5
and (currently) has significant performance advantages, even without using
the optional C extension for speedups.  Simplejson is also supported on
Python 3.3+.")
    (license license:x11)))

(define-public python2-simplejson
  (package-with-python2 python-simplejson))


(define-public python-pyicu
  (package
    (name "python-pyicu")
    (version "1.9.8")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "PyICU" version))
      (sha256
       (base32
        "05nz4p2dpkhwj6y9kik24xbvmfxji39nl0xw0sc0nvp9fgzf6xnd"))))
    (build-system python-build-system)
    (inputs
     `(("icu4c" ,icu4c)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-six" ,python-six)))
    (home-page "https://github.com/ovalhub/pyicu")
    (synopsis "Python extension wrapping the ICU C++ API")
    (description
     "PyICU is a python extension wrapping the ICU C++ API.")
    (properties `((python2-variant . ,(delay python2-pyicu))))
    (license license:x11)))

(define-public python2-pyicu
  (let ((base (package-with-python2
                (strip-python2-variant python-pyicu))))
    (package
      (inherit base)
      (arguments
       `(,@(package-arguments base)
         #:phases
         (modify-phases %standard-phases
           (add-before 'check 'delete-failing-test
             (λ _
               ;; XXX: This fails due to Unicode issues unique to Python 2,
               ;; it seems: <https://github.com/ovalhub/pyicu/issues/61>.
               (delete-file "test/test_Script.py")
               #t))))))))

(define-public python2-dogtail
  ;; Python 2 only, as it leads to "TabError: inconsistent use of tabs and
  ;; spaces in indentation" with Python 3.
  (package
    (name "python2-dogtail")
    (version "0.9.9")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "dogtail" version))
             (sha256
              (base32
               "0p5wfssvzr9w0bvhllzbbd8fnp4cca2qxcpcsc33dchrmh5n552x"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2
                 #:tests? #f))                    ; invalid command "test"
    ;; Currently no offical homepage.
    (home-page "https://pypi.python.org/pypi/dogtail/")
    (synopsis "GUI test tool and automation framework written in Python")
    (description
     "Dogtail is a GUI test tool and automation framework written in Python.
It uses Accessibility (a11y) technologies to communicate with desktop
applications. dogtail scripts are written in Python and executed like any
other Python program.")
    (license license:gpl2+)))

(define-public python2-empy
  (package
    (name "python2-empy")
    (version "3.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.alcyone.com/software/empy/empy-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "01g8mmkfnvjdmlhsihwyx56lrg7r5m5d2fg6mnxsvy6g0dnl69f6"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "./test.sh")))))))
    (home-page "http://www.alcyone.com/software/empy/")
    (synopsis "Templating system for Python")
    (description
     "EmPy is a system for embedding Python expressions and statements in
template text; it takes an EmPy source file, processes it, and produces
output.  This is accomplished via expansions, which are special signals to the
EmPy system and are set off by a special prefix (by default the at sign, @@).
EmPy can expand arbitrary Python expressions and statements in this way, as
well as a variety of special forms.  Textual data not explicitly delimited in
this way is sent unaffected to the output, allowing Python to be used in
effect as a markup language.  Also supported are callbacks via hooks,
recording and playback via diversions, and dynamic, chainable filters.  The
system is highly configurable via command line options and embedded
commands.")
    (license license:lgpl2.1+)))

(define-public python2-element-tree
  (package
    (name "python2-element-tree")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://effbot.org/media/downloads/elementtree-"
                    version "-20050316.tar.gz"))
              (sha256
               (base32
                "016bphqnlg0l4vslahhw4r0aanw95bpypy65r1i1acyb2wj5z7dj"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2                       ; seems to be part of Python 3
       #:tests? #f))                            ; no 'test' sub-command
    (synopsis "Toolkit for XML processing in Python")
    (description
     "ElementTree is a Python library supporting lightweight XML processing.")
    (home-page "http://effbot.org/zone/element-index.htm")
    (license (license:x11-style
              "http://docs.python.org/2/license.html"
              "Like \"CWI LICENSE AGREEMENT FOR PYTHON 0.9.0 THROUGH 1.2\"."))))

(define-public python2-pybugz
  (package
    (name "python2-pybugz")
    (version "0.6.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://bits.liquidx.net/projects/pybugz/pybugz-"
                    version ".tar.gz"))
              (sha256
               (base32
                "17ni00p08gp5lkxlrrcnvi3x09fmajnlbz4da03qcgl9q21ym4jd"))
              (patches (search-patches "pybugz-stty.patch"
                                       "pybugz-encode-error.patch"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2                         ; SyntaxError with Python 3
       #:tests? #f))                              ; no 'test' sub-command
    (propagated-inputs
     `(("element-tree" ,python2-element-tree)))
    (synopsis "Python and command-line interface to Bugzilla")
    (description
     "PyBugz is a Python library and command-line tool to query the Bugzilla
bug tracking system.  It is meant as an aid to speed up interaction with the
bug tracker.")
    (home-page "http://www.liquidx.net/pybugz/")
    (license license:gpl2)))

(define-public python-enum34
  (package
    (name "python-enum34")
    (version "1.1.6")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "enum34" version))
      (sha256
       (base32
        "1cgm5ng2gcfrkrm3hc22brl6chdmv67b9zvva9sfs7gn7dwc9n4a"))))
    (build-system python-build-system)
    (home-page "https://pypi.python.org/pypi/enum34")
    (synopsis "Backported Python 3.4 Enum")
    (description
     "Enum34 is the new Python stdlib enum module available in Python 3.4
backported for previous versions of Python from 2.4 to 3.3.")
    (license license:bsd-3)))

(define-public python2-enum34
  (package-with-python2 python-enum34))

(define-public python-parse-type
  (package
    (name "python-parse-type")
    (version "0.3.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/p/"
                          "parse_type/parse_type-" version ".tar.gz"))
      (sha256
       (base32
        "0iv1c34npr4iynwpgv1vkjx9rjd18a85ir8c01gc5f7wp8iv7l1x"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "tests/test_parse_type_parse.py"
               ;; Newer Python versions don't have the problem this test tests.
               (("self[.]assertRaises[(]parse.TooManyFields, p.parse, ''[)]")
                ""))
             #t)))))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-parse" ,python-parse)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/jenisys/parse_type")
    (synopsis "Extended parse module")
    (description
     "Parse_type extends the python parse module.")
    (properties
     `((python2-variant . ,(delay python2-parse-type))))
    (license license:bsd-3)))

(define-public python2-parse-type
  (let ((base (package-with-python2
                (strip-python2-variant python-parse-type))))
    (package (inherit base)
      (propagated-inputs
       `(("python2-enum34" ,python2-enum34)
         ,@(package-propagated-inputs base))))))

(define-public python-parse
  (package
    (name "python-parse")
    (version "1.6.6")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "parse" version))
      (sha256
       (base32
        "0y31i3mwgv35qn0kzzjn9q8jqfdqmbi6sr6yfvn8rq4lqjm5lhvi"))
      (patches (search-patches "python-parse-too-many-fields.patch"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (zero? (system* "python" "test_parse.py")))))))
    (home-page "https://github.com/r1chardj0n3s/parse")
    (synopsis "Parse strings")
    (description
     "Parse strings using a specification based on the Python format()
syntax.")
    (license license:x11)))

(define-public python-polib
  (package
    (name "python-polib")
    (version "1.0.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "polib" version))
               (sha256
                (base32
                  "1pq2hbm3m2q0cjdszk8mc4qa1vl3wcblh5nfyirlfnzb2pcy7zss"))))
    (build-system python-build-system)
    (home-page "https://bitbucket.org/izi/polib/wiki/Home")
    (synopsis "Manipulate, create and modify gettext files")
    (description "Polib can manipulate any gettext format (po, pot and mo)
files.  It can be used to create po files from scratch or to modify
existing ones.")
    (license license:expat)))

(define-public python2-polib
  (let ((base (package-with-python2 (strip-python2-variant python-polib))))
    (package
      (inherit base)
      (arguments `(,@(package-arguments base)
                   ;; Tests don't work with python2.
                   #:tests? #f)))))

(define-public scons
  (package
    (name "scons")
    (version "3.0.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/scons/scons/" version
                                 "/scons-" version ".tar.gz"))
             (sha256
              (base32
               "0wzid419mlwqw9llrg8gsx4nkzhqy16m4m40r0xnh6cwscw5wir4"))))
    (build-system python-build-system)
    (arguments
     `(#:use-setuptools? #f                ; still relies on distutils
       #:tests? #f))                       ; no 'python setup.py test' command
    (home-page "http://scons.org/")
    (synopsis "Software construction tool written in Python")
    (description
     "SCons is a software construction tool.  Think of SCons as an improved,
cross-platform substitute for the classic Make utility with integrated
functionality similar to autoconf/automake and compiler caches such as ccache.
In short, SCons is an easier, more reliable and faster way to build
software.")
    (license license:x11)))

(define-public scons-python2
  (package
    (inherit (package-with-python2 scons))
    (name "scons-python2")))

(define-public python-extras
  (package
    (name "python-extras")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/e/extras/extras-"
             version ".tar.gz"))
       (sha256
        (base32
         "1h7zx4dfyclalg0fqnfjijpn0f793a9mx8sy3b27gd31nr6dhq3s"))))
    (build-system python-build-system)
    (arguments
     ;; error in setup.cfg: command 'test' has no such option 'buffer'
     '(#:tests? #f))
    (home-page "https://github.com/testing-cabal/extras")
    (synopsis "Useful extensions to the Python standard library")
    (description
     "Extras is a set of extensions to the Python standard library.")
    (license license:expat)))

(define-public python2-extras
  (package-with-python2 python-extras))

(define-public python-mimeparse
  (package
    (name "python-mimeparse")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/python-mimeparse/python-mimeparse-"
             version ".tar.gz"))
       (sha256
        (base32
         "1hyxg09kaj02ri0rmwjqi86wk4nd1akvv7n0dx77azz76wga4s9w"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; no setup.py test command
    (home-page
     "https://github.com/dbtsai/python-mimeparse")
    (synopsis "Python library for parsing MIME types")
    (description
     "Mimeparse provides basic functions for parsing MIME type names and
matching them against a list of media-ranges.")
    (license license:expat)))

(define-public python2-mimeparse
  (package-with-python2 python-mimeparse))

(define-public python-pafy
  (package
    (name "python-pafy")
    (version "0.5.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pafy" version))
       (sha256
        (base32
         "1a7dxi95m1043rxx1r5x3ngb66nwlq6aqcasyqqjzmmmjps4zrim"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Currently pafy can not find itself in the tests
    (propagated-inputs
     ;; Youtube-dl is a python package which is imported in the file
     ;; "backend_youtube_dl.py", therefore it needs to be propagated.
     `(("youtube-dl" ,youtube-dl)))
    (home-page "https://np1.github.io/pafy/")
    (synopsis "Retrieve YouTube content and metadata")
    (description
     "@code{pafy} is a python library to retrieve YouTube content and metadata.")
    (license license:lgpl3+)))

(define-public python-py
  (package
    (name "python-py")
    (version "1.4.32")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py" version))
       (sha256
        (base32
         "19s1pql9pq85h1qzsdwgyb8a3k1qgkvh33b02m8kfqhizz8rzf64"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: "ImportError: 'test' module incorrectly imported from
     ;; '/gnu/store/...-python-pytest-mimimal-3.0.5/lib/python3.5/site-packages'.
     ;; Expected '/tmp/guix-build-python-py-1.4.31.drv-0/py-1.4.31/py'.
     ;; Is this module globally installed?"
     '(#:tests? #f))
    (home-page "http://pylib.readthedocs.org/")
    (synopsis "Python library for parsing, I/O, instrospection, and logging")
    (description
     "Py is a Python library for file name parsing, .ini file parsing, I/O,
code introspection, and logging.")
    (license license:expat)))

(define-public python2-py
  (package-with-python2 python-py))

;; Recent versions of python-fixtures and python-testrepository need
;; python-pbr for packaging, which itself needs these two packages for
;; testing.
;; To fix this circular dependency, we use a build of python-pbr, based on the
;; same source, just without any test dependencies and with tests disabled.
;; python-pbr-minmal is then used to package python-fixtures and
;; python-testrepository.
;; Strictly speaking we currently could remove the test-requirements from the
;; normal python-pbr package (and save this package) since test are disabled
;; there anyway. But this may change in future.
(define-public python-pbr-minimal
  (package
    (name "python-pbr-minimal")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pbr" version))
       (sha256
        (base32
         "14fs5acnalnb3h62s7q7av239j541fk0n0z0lawh4h09b1s93s6p"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "http://docs.openstack.org/developer/pbr/")
    (synopsis "Minimal build of python-pbr used for bootstrapping")
    (description
     "Used only for bootstrapping python2-pbr, you should not need this.")
    (license license:asl2.0)))

(define-public python2-pbr-minimal
  (package-with-python2 python-pbr-minimal))

(define-public python-pbr
  (package
    (inherit python-pbr-minimal)
    (name "python-pbr")
    (arguments
     `(#:tests? #f)) ;; Most tests seem to use the Internet.
    (propagated-inputs
      `(("git" ,git))) ;; pbr actually uses the "git" binary.
    (native-inputs
      `(("python-fixtures" ,python-fixtures)
        ;; discover, coverage, hacking, subunit
        ("python-mock" ,python-mock)
        ("python-six" ,python-six)
        ("python-sphinx" ,python-sphinx)
        ("python-testrepository" ,python-testrepository)
        ("python-testresources" ,python-testresources)
        ("python-testscenarios" ,python-testscenarios)
        ("python-testtools" ,python-testtools)
        ("python-virtualenv" ,python-virtualenv)))
    (synopsis "Enhance the default behavior of Python’s setuptools")
    (description
      "Python Build Reasonableness (PBR) is a library that injects some useful
and sensible default behaviors into your setuptools run.  It will set
versions, process requirements files and generate AUTHORS and ChangeLog file
from git information.
")))

(define-public python2-pbr
  (package-with-python2 python-pbr))

(define-public python-exif-read
  (package
    (name "python-exif-read")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ExifRead" version))
              (sha256
               (base32
                "1b90jf6m9vxh9nanhpyvqdq7hmfx5iggw1l8kq10jrs6xgr49qkr"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; no tests
    (home-page "https://github.com/ianare/exif-py")
    (synopsis "Python library to extract EXIF data from image files")
    (description
     "ExifRead is a Python library to extract EXIF data from tiff and jpeg
files.")
    (license license:bsd-3)))

(define-public python2-exif-read
  (package-with-python2 python-exif-read))

(define-public python-pyld
  (package
    (name "python-pyld")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "PyLD" version))
              (sha256
               (base32
                "1m0fs6897vxfkf7awah5i66i7b7smm5fnywf1w50fpzyfbfhr156"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; no tests
    (home-page "https://github.com/digitalbazaar/pyld")
    (synopsis "Python implementation of the JSON-LD specification")
    (description
     "PyLD is an implementation of the JSON-LD specification.")
    (license license:bsd-3)))

(define-public python2-pyld
  (package-with-python2 python-pyld))

(define-public python-click
  (package
    (name "python-click")
    (version "6.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "click" version))
       (sha256
        (base32
         "02qkfpykbq35id8glfgwc38yc430427yd05z1wc5cnld8zgicmgi"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((glibc (assoc-ref inputs ,(if (%current-target-system)
                                                 "cross-libc" "libc"))))
               (substitute* "click/_unicodefun.py"
                 (("'locale'")
                  (string-append "'" glibc "/bin/locale'"))))
             #t))
         (replace 'check
           (lambda _
             (zero? (system* "make" "test")))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "http://click.pocoo.org")
    (synopsis "Command line library for Python")
    (description
     "Click is a Python package for creating command line interfaces in a
composable way with as little code as necessary.  Its name stands for
\"Command Line Interface Creation Kit\".  It's highly configurable but comes
with sensible defaults out of the box.")
    (license license:bsd-3)))

(define-public python2-click
  (package-with-python2 python-click))

(define-public python-wheel
  (package
    (name "python-wheel")
    (version "0.30.0a0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "wheel" version))
        (sha256
         (base32
          "1nm6mn8isny0hr86rhbfrpfj867c0phf001xgsd69xfp9ady1wwq"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-jsonschema" ,python-jsonschema)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://bitbucket.org/pypa/wheel/")
    (synopsis "Format for built Python packages")
    (description
     "A wheel is a ZIP-format archive with a specially formatted filename and
the @code{.whl} extension.  It is designed to contain all the files for a PEP
376 compatible install in a way that is very close to the on-disk format.  Many
packages will be properly installed with only the @code{Unpack} step and the
unpacked archive preserves enough information to @code{Spread} (copy data and
scripts to their final locations) at any later time.  Wheel files can be
installed with a newer @code{pip} or with wheel's own command line utility.")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-wheel))))))

(define-public python2-wheel
  (let ((wheel (package-with-python2
                (strip-python2-variant python-wheel))))
    (package (inherit wheel)
      (native-inputs `(("python2-functools32" ,python2-functools32)
                        ,@(package-native-inputs wheel))))))

(define-public python-vcversioner
  (package
    (name "python-vcversioner")
    (version "2.16.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vcversioner" version))
       (sha256
        (base32
         "16z10sm78jd7ca3jbkgc3q5i8a8q7y1h21q1li21yy3rlhbhrrns"))))
    (build-system python-build-system)
    (synopsis "Python library for version number discovery")
    (description "Vcversioner is a Python library that inspects tagging
information in a variety of version control systems in order to discover
version numbers.")
    (home-page "https://github.com/habnabit/vcversioner")
    (license license:isc)))

(define-public python2-vcversioner
  (package-with-python2 python-vcversioner))

(define-public python-jsonschema
  (package
    (name "python-jsonschema")
    (version "2.5.1")
    (source (origin
             (method url-fetch)
             (uri
              (string-append
               "https://pypi.python.org/packages/source/j/jsonschema/jsonschema-"
               version ".tar.gz"))
             (sha256
              (base32
               "0hddbqjm4jq63y8jf44nswina1crjs16l9snb6m3vvgyg31klrrn"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-vcversioner" ,python-vcversioner)))
    (home-page "https://github.com/Julian/jsonschema")
    (synopsis "Implementation of JSON Schema for Python")
    (description
     "Jsonschema is an implementation of JSON Schema for Python.")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-jsonschema))))))

(define-public python2-jsonschema
  (let ((jsonschema (package-with-python2
                     (strip-python2-variant python-jsonschema))))
    (package (inherit jsonschema)
             (native-inputs
              `(("python2-mock" ,python2-mock)
                ,@(package-native-inputs jsonschema)))
             (propagated-inputs
              `(("python2-functools32" ,python2-functools32))))))

(define-public python-schema
  (package
    (name "python-schema")
    (version "0.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "schema" version))
       (sha256
        (base32
         "1lw28j9w9vxyigg7vkfkvi6ic9lgjkdnfvnxdr7pklslqvzmk2vm"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/keleshev/schema")
    (synopsis "Simple data validation library")
    (description
     "@code{python-schema} is a library for validating Python data
structures, such as those obtained from config-files, forms, external
services or command-line parsing, converted from JSON/YAML (or
something else) to Python data-types.")
    (license license:psfl)))

(define-public python2-schema
  (package-with-python2 python-schema))

(define-public python-schema-0.5
  (package (inherit python-schema)
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "schema" version))
       (sha256
        (base32
         "10zqvpaky51kgb8nd42bk7jwl8cn2zvayxjpdc1wwmpybj92x67s"))))))

(define-public python2-schema-0.5
  (package-with-python2 python-schema-0.5))

(define-public python-kitchen
  (package
    (name "python-kitchen")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kitchen" version))
       (sha256
        (base32
         "0ggv3p4x8jvmmzhp0xm00h6pvh1g0gmycw71rjwagnrj8n23vxrq"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-chardet" ,python-chardet)))
    (home-page "https://github.com/fedora-infra/kitchen")
    (synopsis "Python API for snippets")
    (description "@code{kitchen} module provides a python API for all sorts of
little useful snippets of code that everybody ends up writing for their projects
but never seem big enough to build an independent release.  Use kitchen and stop
cutting and pasting that code over and over.")
    (license (list license:lgpl2.1+
                   ;; subprocess.py, test_subprocess.py,
                   ;; kitchen/pycompat25/defaultdict.py:
                   license:psfl))))

(define-public python2-kitchen
  (package-with-python2 python-kitchen))

(define-public python-unidecode
  (package
    (name "python-unidecode")
    (version "0.04.21")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "Unidecode" version))
             (sha256
              (base32
               "0lfhp9c5xrbpjvbpr12ji52g1lx04404bzzdg6pvabhzisw6l2i8"))))
    (build-system python-build-system)
    (home-page "https://pypi.python.org/pypi/Unidecode")
    (synopsis "ASCII transliterations of Unicode text")
    (description
     "Unidecode provides ASCII transliterations of Unicode text.  Unidecode is
useful when integrating with legacy code that doesn't support Unicode, or for
ease of entry of non-Roman names on a US keyboard, or when constructing ASCII
machine identifiers from human-readable Unicode strings that should still be
somewhat intelligible.")
    (license license:gpl2+)))

(define-public python2-unidecode
  (package-with-python2 python-unidecode))

(define-public python-pyjwt
  (package
    (name "python-pyjwt")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyJWT" version))
       (sha256
        (base32
         "1rxsg14i33vm2i6lz0my628108c81k43v10n4h3p0gx62xdyf2sh"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file-recursively
                     (find-files "." "\\.pyc$"))
           #t))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest-3.0)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/progrium/pyjwt")
    (synopsis "JSON Web Token implementation in Python")
    (description
     "PyJWT is a JSON Web Token implementation written in Python.")
    (license license:expat)))

(define-public python2-pyjwt
  (package-with-python2 python-pyjwt))

(define-public python-pykka
  (package
    (name "python-pykka")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pykka" version))
       (sha256
        (base32
         "049w3r0mdnnw7xv19jiq7rvls9k7xs73x05b4qs5d6z4vvmgyiz8"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nose" ,python-nose)
       ("python-gevent" ,python-gevent)
       ("python-eventlet" ,python-eventlet)))
    (home-page "https://www.pykka.org/")
    (synopsis "Pykka is a Python implementation of the actor model")
    (description
     "Pykka is a Python implementation of the actor model.
The actor model introduces some simple rules to control the sharing
of state and cooperation between execution units, which makes it
easier to build concurrent applications.")
    (license license:asl2.0)))

(define-public python2-pykka
  (package-with-python2 python-pykka))

(define-public python-itsdangerous
  (package
    (name "python-itsdangerous")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/i/itsdangerous/itsdangerous-"
             version ".tar.gz"))
       (sha256
        (base32
         "06856q6x675ly542ig0plbqcyab6ksfzijlyf1hzhgg3sgwgrcyb"))))
    (build-system python-build-system)
    (home-page "https://github.com/mitsuhiko/itsdangerous")
    (synopsis "Python library for passing data to/from untrusted environments")
    (description
     "Itsdangerous provides various helpers to pass trusted data to untrusted
environments and back.")
    (license license:bsd-3)))

(define-public python2-itsdangerous
  (package-with-python2 python-itsdangerous))

(define-public python-pyyaml
  (package
    (name "python-pyyaml")
    (version "3.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyYAML" version))
       (sha256
        (base32
         "1aqjl8dk9amd4zr99n8v2qxzgmr2hdvqfma4zh7a41rj6336c9sr"))))
    (build-system python-build-system)
    (inputs
     `(("libyaml" ,libyaml)))
    (home-page "http://pyyaml.org/wiki/PyYAML")
    (synopsis "YAML parser and emitter for Python")
    (description
     "PyYAML is a YAML parser and emitter for Python.  PyYAML features a
complete YAML 1.1 parser, Unicode support, pickle support, capable extension
API, and sensible error messages.  PyYAML supports standard YAML tags and
provides Python-specific tags that allow to represent an arbitrary Python
object.")
    (license license:expat)))

(define-public python2-pyyaml
  (package-with-python2 python-pyyaml))

(define-public python-virtualenv
  (package
    (name "python-virtualenv")
    (version "15.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "virtualenv" version))
       (sha256
        (base32
         "07cbajzk8l05k5zhlw0b9wbf2is65bl9v6zrn2a0iyn57w6pd73d"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Disable failing test.  See upstream bug report
             ;; https://github.com/pypa/virtualenv/issues/957
             (substitute* "tests/test_virtualenv.py"
               (("skipif.*") "skipif(True, reason=\"Guix\")\n"))
             (zero? (system* "py.test")))))))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://virtualenv.pypa.io/")
    (synopsis "Virtual Python environment builder")
    (description
     "Virtualenv is a tool to create isolated Python environments.")
    (license license:expat)))

(define-public python2-virtualenv
  (package-with-python2 python-virtualenv))

(define-public python-markupsafe
  (package
    (name "python-markupsafe")
    (version "0.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/M/MarkupSafe/MarkupSafe-"
             version ".tar.gz"))
       (sha256
        (base32
         "1hvip33wva3fnmvfp9x5klqri7hpl1hkgqmjbss18nmrb7zimv54"))))
    (build-system python-build-system)
    (home-page "https://github.com/mitsuhiko/markupsafe")
    (synopsis "XML/HTML/XHTML markup safe string implementation for Python")
    (description
     "Markupsafe provides an XML/HTML/XHTML markup safe string implementation
for Python.")
    (license license:bsd-3)))

(define-public python2-markupsafe
  (package-with-python2 python-markupsafe))

(define-public python-jinja2
  (package
    (name "python-jinja2")
    (version "2.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Jinja2" version))
       (sha256
        (base32
         "1zzrkywhziqffrzks14kzixz7nd4yh2vc0fb04a68vfd2ai03anx"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; These files cannot be built with Python < 3.6.  See
         ;; https://github.com/pallets/jinja/issues/655
         ;; FIXME: Remove this when the "python" package is upgraded.
         (add-after 'unpack 'delete-incompatible-files
           (lambda _
             (for-each delete-file
                       '("jinja2/asyncsupport.py"
                         "jinja2/asyncfilters.py"))
             #t)))))
    (propagated-inputs
     `(("python-markupsafe" ,python-markupsafe)))
    (home-page "http://jinja.pocoo.org/")
    (synopsis "Python template engine")
    (description
     "Jinja2 is a small but fast and easy to use stand-alone template engine
written in pure Python.")
    (license license:bsd-3)))

(define-public python2-jinja2
  (package-with-python2 python-jinja2))

(define-public python-pystache
  (package
    (name "python-pystache")
    (version "0.5.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pystache" version))
              (sha256
               (base32
                "0nmqsfmiw4arjxqkmf9z66ml950pcdjk6aq4gin4sywmzdjw5fzp"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; FIXME: Python 3 tests are failing.
    (home-page "http://defunkt.io/pystache/")
    (synopsis "Python logic-less template engine")
    (description
     "Pystache is a Python implementation of the framework agnostic,
logic-free templating system Mustache.")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-pystache))))))

(define-public python2-pystache
  (package (inherit (package-with-python2
                     (strip-python2-variant python-pystache)))
           (arguments
            `(#:python ,python-2
              #:phases
              (modify-phases %standard-phases
                (replace 'check
                  (lambda _
                    (zero? (system* "python" "test_pystache.py")))))))))

(define-public python-joblib
  (package
    (name "python-joblib")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "joblib" version))
              (sha256
               (base32
                "0787k919zlfmgymprz5bzv0v1df5bbirlf3awrghmjgvkrd9dci9"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove pre-compiled .pyc files from source.
           (for-each delete-file-recursively
                     (find-files "." "__pycache__" #:directories? #t))
           (for-each delete-file (find-files "." "\\.pyc$"))
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-failing-tests
           (lambda _
             ;; This numpydoc tests fails for unknown reasons
             (delete-file "doc/sphinxext/numpydoc/tests/test_docscrape.py")
             ;; This numpydoc test depends on matplotlib, which is not a
             ;; required input.
             (delete-file "doc/sphinxext/numpydoc/tests/test_plot_directive.py")
             ;; These tests fail to execute sys.executable
             (substitute* "joblib/test/test_parallel.py"
               (("import nose" line)
                (string-append "from nose.plugins.skip import SkipTest\n" line))
               (("def test_nested_parallel_warnings" line)
                (string-append "@SkipTest\n" line))
               (("def test_parallel_with_interactively_defined_functions" line)
                (string-append "@SkipTest\n" line)))
             #t)))))
    ;; Provide nose to enable tests command
    (native-inputs
     `(("python-nose"       ,python-nose)
       ("python-sphinx"     ,python-sphinx)
       ("python-docutils"   ,python-docutils)
       ("python-numpydoc"   ,python-numpydoc)))
    (home-page "http://pythonhosted.org/joblib/")
    (synopsis "Using Python functions as pipeline jobs")
    (description
     "Joblib is a set of tools to provide lightweight pipelining in Python.
In particular, joblib offers: transparent disk-caching of the output values
and lazy re-evaluation (memoize pattern), easy simple parallel computing
logging and tracing of the execution.")
    (license license:bsd-3)))

(define-public python2-joblib
  (package-with-python2 python-joblib))

(define-public python-docutils
  (package
    (name "python-docutils")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docutils" version))
       (sha256
        (base32
         "0x22fs3pdmr42kvz6c654756wja305qv6cx1zbhwlagvxgr4xrji"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; no setup.py test command
    (home-page "http://docutils.sourceforge.net/")
    (synopsis "Python Documentation Utilities")
    (description
     "Docutils is a modular system for processing documentation into useful
formats, such as HTML, XML, and LaTeX.  For input Docutils supports
reStructuredText.")
    ;; Most of the source code is public domain, but some source files are
    ;; licensed under the PFSL, BSD 2-clause, and GPLv3+ licenses.
    (license (list license:public-domain license:psfl license:bsd-2 license:gpl3+))))

(define-public python2-docutils
  (package-with-python2 python-docutils))

(define-public python-pygments
  (package
    (name "python-pygments")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pygments" version))
       (sha256
        (base32
         "1k78qdvir1yb1c634nkv6rbga8wv4289xarghmsbbvzhvr311bnv"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests require sphinx, which depends on this.
     '(#:tests? #f))
    (home-page "http://pygments.org/")
    (synopsis "Syntax highlighting")
    (description
     "Pygments is a syntax highlighting package written in Python.")
    (license license:bsd-2)))

(define-public python2-pygments
  (package-with-python2 python-pygments))

(define-public python-sphinxcontrib-websupport
  (package
    (name "python-sphinxcontrib-websupport")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-websupport" version))
              (sha256
               (base32
                "1f9f0wjpi9nhikbyaz6d19s7qvzdf1nq2g5dsh640fma4q9rd1bs"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-xapian-bindings" ,python-xapian-bindings)))
    ;; Needed for running the test suite
    (native-inputs
     `(("python-six" ,python-six)
       ("python-jinja2" ,python-jinja2)
       ("python-docutils" ,python-docutils)
       ("python-sphinx" ,python-sphinx)
       ("python-sqlalchemy" ,python-sqlalchemy)
       ("python-whoosh" ,python-whoosh)))
    (home-page "http://sphinx-doc.org/")
    (synopsis "Sphinx API for web applications")
    (description "This package provides a Python API to easily integrate
Sphinx documentation into your web application.  It provides tools to
integrate Sphinx documents in web templates and to handle searches.")
    (license license:bsd-3)))

(define-public python-sphinx
  (package
    (name "python-sphinx")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Sphinx" version))
       (sha256
        (base32
         "1i8p9idnli4gr0y4x67yakbdk5w6a0xjzhrg6bg51y9d1fi7fslf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Requires Internet access.
             (delete-file "tests/test_build_linkcheck.py")
             (zero? (system* "make" "test")))))))
    (propagated-inputs
     `(("python-imagesize" ,python-imagesize)
       ("python-sphinx-alabaster-theme"
        ,python-sphinx-alabaster-theme)
       ("python-babel" ,python-babel)
       ("python-snowballstemmer" ,python-snowballstemmer)
       ("python-docutils" ,python-docutils)
       ("python-jinja2" ,python-jinja2)
       ("python-pygments" ,python-pygments)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (native-inputs
     `(("graphviz" ,graphviz)
       ("python-html5lib" ,python-html5lib)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (home-page "http://sphinx-doc.org/")
    (synopsis "Python documentation generator")
    (description "Sphinx is a tool that makes it easy to create documentation
for Python projects or other documents consisting of multiple reStructuredText
sources.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-sphinx))))))

(define-public python-sphinx-1.6
  (package (inherit python-sphinx)
    (name "python-sphinx")
    (version "1.6.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Sphinx" version))
              (sha256
               (base32
                "1rj6f3i8hmrx2qlkshi5kp5xcy98dlynwlyl05yvflj5f66dp2xg"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Requires Internet access.
             (delete-file "tests/test_build_linkcheck.py")
             (substitute* "tests/test_build_latex.py"
               (("@pytest.mark.sphinx\\('latex', testroot='images'\\)")
                "@pytest.mark.skip()"))
             (zero? (system* "make" "test")))))))
    (propagated-inputs
     `(("python-sphinxcontrib-websupport" ,python-sphinxcontrib-websupport)
       ,@(package-propagated-inputs python-sphinx)))
    (native-inputs
     `(("python-pytest" ,python-pytest-3.0)
       ("imagemagick" ,imagemagick) ; for "convert"
       ,@(package-native-inputs python-sphinx)))
    (properties '())))

(define-public python-sphinx-1.5.3
  (package
    (inherit python-sphinx)
    (name "python-sphinx")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Sphinx" version))
       (sha256
        (base32
         "0kw1axswbvaavr8ggyf4qr6hnisnrzlbkkcdada69vk1x9xjassg"))))
    (native-inputs
     `(("python-pytest" ,python-pytest-3.0)
       ,@(package-native-inputs python-sphinx)))))

(define-public python2-sphinx
  (let ((base (package-with-python2 (strip-python2-variant python-sphinx))))
    (package
      (inherit base)
      (native-inputs `(("python2-mock" ,python2-mock)
                       ("python2-enum34" ,python2-enum34)
                       ,@(package-native-inputs base)))
      (propagated-inputs `(("python2-pytz" ,python2-pytz)
                       ,@(package-propagated-inputs base))))))

(define-public python-sphinx-rtd-theme
  (package
    (name "python-sphinx-rtd-theme")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx_rtd_theme" version))
       (sha256
        (base32
         "05rlhjzdyapr2w74jjs7mrm8hi69qskcr5vya9f9drpsys7lpxrd"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/snide/sphinx_rtd_theme/")
    (synopsis "ReadTheDocs.org theme for Sphinx")
    (description "A theme for Sphinx used by ReadTheDocs.org.")
    (license license:expat)))

(define-public python2-sphinx-rtd-theme
  (package-with-python2 python-sphinx-rtd-theme))

(define-public python-guzzle-sphinx-theme
  (package
    (name "python-guzzle-sphinx-theme")
    (version "0.7.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "guzzle_sphinx_theme" version))
        (sha256
         (base32
          "1rnkzrrsbnifn3vsb4pfaia3nlvgvw6ndpxp7lzjrh23qcwid34v"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/guzzle/guzzle_sphinx_theme")
    (synopsis "Sphinx theme used by Guzzle")
    (description "This package provides guzzle_sphinx_theme, a theme for the
Sphinx documentation system, used by @uref{http://docs.guzzlephp.org, Guzzle}
and several other projects.")
    (license license:expat)))

(define-public python2-guzzle-sphinx-theme
  (package-with-python2 python-guzzle-sphinx-theme))

(define-public python-rst.linker
  (package
    (name "python-rst.linker")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rst.linker" version))
       (sha256
        (base32
         "0bh4lnj2p1nh0wf5pgxgfbrp27xhb1rinahkb5j7s3qprq6qn0sr"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    ;; Test would require path.py, which would introduce a cyclic dependence.
    (arguments `(#:tests? #f))
    ;; Note: As of version 1.7 the documentation is not worth building.
    (home-page "https://github.com/jaraco/rst.linker")
    (synopsis "Sphinx plugin to add links and timestamps")
    (description "rst.linker allows to automatically replace text by a
reStructuredText external reference or timestamps.  It's primary purpose is to
augment the changelog, but it can be used for other documents, too.")
    (license license:expat)))

(define-public python2-rst.linker
  (package-with-python2 python-rst.linker))

(define-public python-feedgenerator
  (package
    (name "python-feedgenerator")
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "feedgenerator" version))
       (sha256
        (base32
         "01mirwkm7xfx539hmvj7g9da1j51gw5lsx74dr0glizskjm5vq2s"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove pre-compiled .pyc files from source.
           (for-each delete-file-recursively
                     (find-files "." "__pycache__" #:directories? #t))
           (for-each delete-file (find-files "." "\\.pyc$"))
           #t))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytz" ,python-pytz)
       ("python-six" ,python-six)))
    (home-page "https://github.com/getpelican/feedgenerator")
    (synopsis
     "Standalone version of Django's Atom/RSS feed generator")
    (description
     "Feedgenerator-py3k is a standalone version of Django's feedgenerator,
which can produce feeds in RSS 2.0, RSS 0.91, and Atom formats.")
    (license license:bsd-3)))

(define-public python2-feedgenerator
  (package-with-python2 python-feedgenerator))

(define-public python-blinker
  (package
    (name "python-blinker")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "blinker" version))
       (sha256
        (base32
         "1dpq0vb01p36jjwbhhd08ylvrnyvcc82yxx3mwjx6awrycjyw6j7"))))
    (build-system python-build-system)
    (home-page "http://pythonhosted.org/blinker/")
    (synopsis "Fast, simple object-to-object and broadcast signaling")
    (description
     "Blinker provides a fast dispatching system that allows any number of
interested parties to subscribe to events, or \"signals\".")
    (license license:expat)))

(define-public python2-blinker
  (package-with-python2 python-blinker))

(define-public pelican
  (package
    (name "pelican")
    (version "3.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pelican" version))
       (sha256
        (base32
         "1hn94rb4q3zmcq16in055xikal4dba5hfx3zznq7warllcgc9f8k"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-feedgenerator" ,python-feedgenerator)
       ("python-jinja2" ,python-jinja2)
       ("python-pygments" ,python-pygments)
       ("python-docutils" ,python-docutils)
       ("python-pytz" ,python-pytz)
       ("python-blinker" ,python-blinker)
       ("python-unidecode" ,python-unidecode)
       ("python-six" ,python-six)
       ("python-dateutil" ,python-dateutil)))
    (home-page "http://getpelican.com/")
    (arguments
     `(;; XXX Requires a lot more packages to do unit tests :P
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-before
                   'install 'adjust-requires
                   ;; Since feedgenerator is installed from git, it doesn't
                   ;; conform to the version requirements.
                   ;;
                   ;; We *do have* "feedgenerator >= 1.6", but strip off the
                   ;; version requirement so setuptools doesn't get confused.
                   (lambda _
                     (substitute* "setup.py"
                       (("['\"]feedgenerator.*?['\"]")
                        "'feedgenerator'")))))))
    (synopsis "Python-based static site publishing system")
    (description
     "Pelican is a tool to generate a static blog from reStructuredText,
Markdown input files, and more.  Pelican uses Jinja2 for templating
and is very extensible.")
    (license license:agpl3+)))

(define-public python-scikit-image
  (package
    (name "python-scikit-image")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/s/scikit-image/scikit-image-"
             version ".tar.gz"))
       (sha256
        (base32 "0jz416fqvpahqyffw8plmszzfj669w8wvf3y9clnr5lr6a7md3kn"))))
    (build-system python-build-system)
    (arguments
     ;; TODO: Some tests require running X11 server. Disable them?
     '(#:tests? #f))
    ;; See DEPENDS.txt for the list of build and run time requiremnts
    (propagated-inputs
     `(("python-matplotlib" ,python-matplotlib)
       ("python-networkx" ,python-networkx)
       ("python-scipy" ,python-scipy)
       ("python-pillow" ,python-pillow)))
    (native-inputs
     `(("python-numpy" ,python-numpy)
       ("python-cython" ,python-cython)
       ("python-six" ,python-six)))
    (home-page "http://scikit-image.org/")
    (synopsis "Image processing in Python")
    (description
     "Scikit-image is a collection of algorithms for image processing.")
    (license license:bsd-3)))

(define-public python2-scikit-image
  (package-with-python2 python-scikit-image))

(define-public python-cython
  (package
    (name "python-cython")
    (version "0.27")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Cython" version))
       (sha256
        (base32
         "02y0pp1nx77b8s1mpxc6da2dccl6wd31pp4ksi9via479qcvacmr"))))
    (build-system python-build-system)
    ;; we need the full python package and not just the python-wrapper
    ;; because we need libpython3.3m.so
    (inputs
     `(("python" ,python)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-HOME
           ;; some tests require access to "$HOME/.cython"
           (lambda _ (setenv "HOME" "/tmp") #t))
         (replace 'check
           (lambda _ (zero? (system* "python" "runtests.py" "-vv")))))))
    (home-page "http://cython.org/")
    (synopsis "C extensions for Python")
    (description "Cython is an optimising static compiler for both the Python
programming language and the extended Cython programming language.  It makes
writing C extensions for Python as easy as Python itself.")
    (license license:asl2.0)
    (properties `((python2-variant . ,(delay python2-cython))))))

(define-public python2-cython
  (package (inherit (package-with-python2
                     (strip-python2-variant python-cython)))
    (name "python2-cython")
    (inputs
     `(("python-2" ,python-2))))) ; this is not automatically changed

;; The RPython toolchain currently does not support Python 3.
(define-public python2-rpython
  (package
    (name "python2-rpython")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rpython" version))
       (sha256
        (base32
         "02z9cvxf0y41dcvwnvf2zn0albhhw1drvjjbq27m6i1piw1k6fc0"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (native-inputs
     `(("python2-pytest" ,python2-pytest))) ; needed for running tests
    (home-page "https://rpython.readthedocs.org")
    (synopsis "Framework for implementing interpreters and virtual machines")
    (description "RPython is a translation and support framework for
producing implementations of dynamic languages, emphasizing a clean separation
between language specification and implementation aspects.")
    (license license:expat)))

;; NOTE: when upgrading numpy please make sure that python-pandas and
;; python-scipy still build, as these three packages are often used together.
(define-public python-numpy
  (package
    (name "python-numpy")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/numpy/numpy/archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "025d4j4aakcp8w5i5diqh812cbbjgac7jszx1j56ivrbi1i8vv7d"))))
    (build-system python-build-system)
    (inputs
     `(("openblas" ,openblas)
       ("lapack" ,lapack)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-nose" ,python-nose)
       ("gfortran" ,gfortran)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure-blas-lapack
           (lambda* (#:key inputs #:allow-other-keys)
             (call-with-output-file "site.cfg"
               (lambda (port)
                 (format port
                         "[openblas]
libraries = openblas
library_dirs = ~a/lib
include_dirs = ~a/include

# backslash-n to make emacs happy
\n[lapack]
lapack_libs = lapack
library_dirs = ~a/lib
include_dirs = ~a/include
"
                         (assoc-ref inputs "openblas")
                         (assoc-ref inputs "openblas")
                         (assoc-ref inputs "lapack")
                         (assoc-ref inputs "lapack"))))
             #t))
         (add-before 'build 'fix-executable-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Make /gnu/store/...-bash-.../bin/sh the default shell,
             ;; instead of /bin/sh.
             (substitute* "numpy/distutils/exec_command.py"
               (("(os.environ.get\\('SHELL', ')(/bin/sh'\\))" match match-start match-end)
                (string-append match-start (assoc-ref inputs "bash") match-end)))
             ;; Use "gcc" executable, not "cc".
             (substitute* "numpy/distutils/system_info.py"
               (("c = distutils\\.ccompiler\\.new_compiler\\(\\)")
                "c = distutils.ccompiler.new_compiler(); c.set_executables(compiler='gcc',compiler_so='gcc',linker_exe='gcc',linker_so='gcc -shared')"))
             #t))
         ;; Tests can only be run after the library has been installed and not
         ;; within the source directory.
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; Make installed package available for running the tests
             (add-installed-pythonpath inputs outputs)
             (with-directory-excursion "/tmp"
               (zero? (system* "python" "-c"
                               "import numpy; numpy.test(verbose=2)"))))))))
    (home-page "http://www.numpy.org/")
    (synopsis "Fundamental package for scientific computing with Python")
    (description "NumPy is the fundamental package for scientific computing
with Python.  It contains among other things: a powerful N-dimensional array
object, sophisticated (broadcasting) functions, tools for integrating C/C++
and Fortran code, useful linear algebra, Fourier transform, and random number
capabilities.")
    (license license:bsd-3)))

(define-public python2-numpy
  (package-with-python2 python-numpy))

(define-public python-numpy-next
  (package (inherit python-numpy)
    (name "python-numpy-next")
    (version "1.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "numpy" version ".zip"))
       (sha256
        (base32
         "1fsgkhh1vdkhmlz8vmdgxnj9n9yaanckxxzz9s0b4p08fqvjic69"))))
    (native-inputs
     `(("unzip" ,unzip)
       ("python-cython" ,python-cython)
       ("python-nose" ,python-nose)
       ("gfortran" ,gfortran)))))

(define-public python2-numpy-next
  (package-with-python2 python-numpy-next))

;; NOTE: NumPy 1.8 is packaged only for Python 2 because it is of
;; interest only for legacy code going back to NumPy's predecessor
;; Numeric.
(define-public python2-numpy-1.8
  (package (inherit python2-numpy)
    (version "1.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/numpy/numpy/archive/v" version ".tar.gz"))
       (file-name (string-append "python2-numpy-" version ".tar.gz"))
       (sha256
        (base32
         "0sc20gz1b17xnyrkp5frca3ql5qfalpv916hfg2kqxpwr6jg0f1g"))))
    (arguments
     (substitute-keyword-arguments (package-arguments python2-numpy)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'configure-blas-lapack
             (lambda* (#:key inputs #:allow-other-keys)
               (call-with-output-file "site.cfg"
                 (lambda (port)
                   (format port
                           "[openblas]
libraries = openblas,lapack
library_dirs = ~a/lib:~a/lib
include_dirs = ~a/include:~a/include
"
                           (assoc-ref inputs "openblas")
                           (assoc-ref inputs "lapack")
                           (assoc-ref inputs "openblas")
                           (assoc-ref inputs "lapack"))))
               #t))))))
    (description "NumPy is the fundamental package for scientific computing
with Python.  It contains among other things: a powerful N-dimensional array
object, sophisticated (broadcasting) functions, tools for integrating C/C++
and Fortran code, useful linear algebra, Fourier transform, and random number
capabilities.  Version 1.8 is the last one to contain the numpy.oldnumeric API
that includes the compatibility layer numpy.oldnumeric with NumPy's predecessor
Numeric.")
    (license license:bsd-3)))

(define-public python-munch
  (package
    (name "python-munch")
    (version "2.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "munch" version))
        (sha256
         (base32
          "1cmqg91xnqx8gvnh4pmp0bfl1dfcm65d5p9mg73zz8pkjhx6h80l"))))
    (build-system python-build-system)
    (home-page "https://github.com/Infinidat/munch")
    (synopsis "Dot-accessible dictionary")
    (description "Munch is a dot-accessible dictionary similar to JavaScript
objects.")
    (license license:expat)))

(define-public python2-munch
  (package-with-python2 python-munch))

(define-public python-colormath
  (package
    (name "python-colormath")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "colormath" version))
       (sha256
        (base32
         "01wp5xwm0a89wdm1dc9rr1ij90idzdiiipxdj1yslhqzkhnjnfh0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-networkx" ,python-networkx)
       ("python-numpy" ,python-numpy)))
    (home-page "https://github.com/gtaylor/python-colormath")
    (synopsis "Color math and conversion library")
    (description
     "This is a Python library for color math and conversions.")
    (license license:bsd-3)))

(define-public python2-colormath
  (package-with-python2 python-colormath))

(define-public python-spectra
  (package
    (name "python-spectra")
    (version "0.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "spectra" version))
       (sha256
        (base32
         "0n87kzhpkml2s2q91rdkl8wz2kkv5b0bkrgww45lxa5vq34qh6w5"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "nosetests" "-v")))))))
    (propagated-inputs
     `(("python-colormath" ,python-colormath)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/jsvine/spectra")
    (synopsis "Color scales and color conversion")
    (description
     "This package provides a Python library intended to make color math,
color scales, and color space conversion easy.  It has support for:

@enumerate
@item Color scales
@item Color ranges
@item Color blending
@item Brightening/darkening colors
@item Saturating/desaturating colors
@item Conversion to/from multiple color spaces.
@end enumerate\n")
    (license license:expat)))

(define-public python2-spectra
  (package-with-python2 python-spectra))

(define-public python2-fastlmm
  (package
    (name "python2-fastlmm")
    (version "0.2.21")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fastlmm" version ".zip"))
       (sha256
        (base32
         "1q8c34rpmwkfy3r4d5172pzdkpfryj561897z9r3x22gq7813x1m"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2)) ; only Python 2.7 is supported
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)
       ("python2-scipy" ,python2-scipy)
       ("python2-matplotlib" ,python2-matplotlib)
       ("python2-pandas" ,python2-pandas)
       ("python2-scikit-learn" ,python2-scikit-learn)
       ("python2-pysnptools" ,python2-pysnptools)))
    (native-inputs
     `(("unzip" ,unzip)
       ("python2-cython" ,python2-cython)
       ("python2-mock" ,python2-mock)
       ("python2-nose" ,python2-nose)))
    (home-page "http://research.microsoft.com/en-us/um/redmond/projects/mscompbio/fastlmm/")
    (synopsis "Perform genome-wide association studies on large data sets")
    (description
     "FaST-LMM, which stands for Factored Spectrally Transformed Linear Mixed
Models, is a program for performing both single-SNP and SNP-set genome-wide
association studies (GWAS) on extremely large data sets.")
    (license license:asl2.0)))

(define-public python-numpy-documentation
  (package
    (name "python-numpy-documentation")
    (version (package-version python-numpy))
    (source (package-source python-numpy))
    (build-system python-build-system)
    (native-inputs
     `(("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)
       ("python-numpydoc" ,python-numpydoc)
       ("texlive" ,(texlive-union (list texlive-fonts-amsfonts
                                        texlive-fonts-ec
                                        texlive-generic-ifxetex
                                        texlive-generic-pdftex
                                        texlive-latex-amsfonts
                                        texlive-latex-capt-of
                                        texlive-latex-cmap
                                        texlive-latex-environ
                                        texlive-latex-eqparbox
                                        texlive-latex-etoolbox
                                        texlive-latex-expdlist
                                        texlive-latex-fancyhdr
                                        texlive-latex-fancyvrb
                                        texlive-latex-fncychap
                                        texlive-latex-float
                                        texlive-latex-framed
                                        texlive-latex-geometry
                                        texlive-latex-graphics
                                        texlive-latex-hyperref
                                        texlive-latex-mdwtools
                                        texlive-latex-multirow
                                        texlive-latex-oberdiek
                                        texlive-latex-parskip
                                        texlive-latex-preview
                                        texlive-latex-tabulary
                                        texlive-latex-threeparttable
                                        texlive-latex-titlesec
                                        texlive-latex-trimspaces
                                        texlive-latex-ucs
                                        texlive-latex-upquote
                                        texlive-latex-url
                                        texlive-latex-varwidth
                                        texlive-latex-wrapfig)))
       ("texinfo" ,texinfo)
       ("perl" ,perl)
       ("scipy-sphinx-theme"
        ,(origin ; The build script expects scipy-sphinx-theme as a git submodule
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/scipy/scipy-sphinx-theme.git")
                 (commit "c466764e2231ba132c09826b5b138fffa1cfcec3")))
           (sha256
            (base32
             "0q2y87clwlsgc7wvlsn9pzyssybcq10plwhq2w1ydykfsyyqbmkl"))))
       ,@(package-native-inputs python-numpy)))
    (arguments
     `(#:tests? #f ; we're only generating the documentation
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((data (string-append (assoc-ref outputs "out") "/share"))
                    (doc (string-append
                          data "/doc/" ,name "-"
                          ,(package-version python-numpy)))
                    (info-reader (string-append data "/info"))
                    (html (string-append doc "/html"))
                    (scipy-sphinx-theme "scipy-sphinx-theme")
                    (sphinx-theme-checkout (assoc-ref inputs scipy-sphinx-theme))
                    (pyver ,(string-append "PYVER=")))

               ;; FIXME: this is needed to for texlive-union to generate
               ;; fonts, which are not found.
               (setenv "HOME" "/tmp")

               (with-directory-excursion "doc"
                 (copy-recursively sphinx-theme-checkout scipy-sphinx-theme)
                 (mkdir-p html)
                 (system* "make" "html" pyver)
                 (system* "make" "latex" "PAPER=a4" pyver)
                 (system* "make" "-C" "build/latex"
                          "all-pdf" "PAPER=a4" pyver)
                 ;; FIXME: Generation of the info file fails.
                 ;; (system* "make" "info" pyver)
                 ;; (mkdir-p info)
                 ;; (copy-file "build/texinfo/numpy.info"
                 ;;            (string-append info "/numpy.info"))
                 (for-each (lambda (file)
                             (copy-file (string-append "build/latex" file)
                                        (string-append doc file)))
                           '("/numpy-ref.pdf" "/numpy-user.pdf"))
                 (with-directory-excursion "build/html"
                   (for-each (lambda (file)
                               (let* ((dir (dirname file))
                                      (tgt-dir (string-append html "/" dir)))
                                 (unless (equal? "." dir)
                                   (mkdir-p tgt-dir))
                                 (install-file file html)))
                             (find-files "." ".*")))))
             #t)))))
    (home-page (package-home-page python-numpy))
    (synopsis "Documentation for the python-numpy package")
    (description (package-description python-numpy))
    (license (package-license python-numpy))))

(define-public python2-numpy-documentation
  (let ((numpy-documentation (package-with-python2 python-numpy-documentation)))
    (package
      (inherit numpy-documentation)
      (native-inputs `(("python2-functools32" ,python2-functools32)
                       ,@(package-native-inputs numpy-documentation))))))

(define-public python-pygit2
  (package
    (name "python-pygit2")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pygit2" version))
       (sha256
        (base32
         "1cbc488ra3kg7r3qky17ms0szi3cda2d96qfkv1l9djsy9hnvw57"))
       (patches
        (search-patches "python-pygit2-disable-network-tests.patch"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-cffi" ,python-cffi)
       ("libgit2" ,libgit2)
       ("python-tox" ,python-tox)))
    (home-page "https://github.com/libgit2/pygit2")
    (synopsis "Python bindings for libgit2")
    (description "Pygit2 is a set of Python bindings to the libgit2 shared
library, libgit2 implements Git plumbing.")
    ;; GPL2.0 only, with linking exception.
    (license license:gpl2)))

(define-public python2-pygit2
  (package-with-python2 python-pygit2))

(define-public python-pyparsing
  (package
    (name "python-pyparsing")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pyparsing/pyparsing"
                           "/pyparsing-" version
                           "/pyparsing-" version ".tar.gz"))
       (sha256
        (base32
         "016b9gh606aa44sq92jslm89bg874ia0yyiyb643fa6dgbsbqch8"))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((doc (string-append (assoc-ref outputs "doc")
                                        "/share/doc/" ,name "-" ,version))
                    (html-doc (string-append doc "/html"))
                    (examples (string-append doc "/examples")))
               (mkdir-p html-doc)
               (mkdir-p examples)
               (for-each
                (lambda (dir tgt)
                  (map (lambda (file)
                         (install-file file tgt))
                       (find-files dir ".*")))
                (list "docs" "htmldoc" "examples")
                (list doc html-doc examples))
               #t))))))
    (home-page "http://pyparsing.wikispaces.com")
    (synopsis "Python parsing class library")
    (description
     "The pyparsing module is an alternative approach to creating and
executing simple grammars, vs. the traditional lex/yacc approach, or the use
of regular expressions.  The pyparsing module provides a library of classes
that client code uses to construct the grammar directly in Python code.")
    (license license:expat)))

(define-public python2-pyparsing
  (package-with-python2 python-pyparsing))

(define-public python-numpydoc
  (package
    (name "python-numpydoc")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/n/numpydoc/numpydoc-"
             version ".tar.gz"))
       (sha256
        (base32
         "0d4dnifaxkll50jx6czj05y8cb4ny60njd2wz299sj2jxfy51w4k"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Drop a test requiring matplotlib, which we cannot add as an
           ;; input since it would create a circular dependency: Extend the
           ;; test for Python 3, where it is already dropped, to Python 2.
           (substitute* "numpydoc/tests/test_plot_directive.py"
             (("3") "2"))))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://pypi.python.org/pypi/numpydoc")
    (synopsis
     "Numpy's Sphinx extensions")
    (description
     "Sphinx extension to support docstrings in Numpy format.")
    (license license:bsd-2)))

(define-public python2-numpydoc
  (package-with-python2 python-numpydoc))

(define-public python-numexpr
  (package
    (name "python-numexpr")
    (version "2.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "numexpr" version))
       (sha256
        (base32
         "1kpnbb5d5n927113zccfibn16z7gidjipyac6kbbhzs0lnizkgph"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))          ; no tests included
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
    (home-page "https://github.com/pydata/numexpr")
    (synopsis "Fast numerical expression evaluator for NumPy")
    (description
     "Numexpr is a fast numerical expression evaluator for NumPy.  With it,
expressions that operate on arrays are accelerated and use less memory than
doing the same calculation in Python.  In addition, its multi-threaded
capabilities can make use of all your cores, which may accelerate
computations, most specially if they are not memory-bounded (e.g. those using
transcendental functions).")
    (license license:expat)))

(define-public python2-numexpr
  (package-with-python2 python-numexpr))

(define-public python-cycler
  (package
    (name "python-cycler")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cycler" version))
              (sha256
               (base32
                "1n69n23fak1gjxlrbhqisi2b9pv3ckrfj98llx3p53953082syyd"))))
    (build-system python-build-system)
    (arguments
     ;; XXX: The current version requires 'coveralls' which we don't have.
     ;; Enable this for the next release which uses 'python-pytest'.
     '(#:tests? #f))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "http://matplotlib.org/cycler/")
    (synopsis "Composable keyword argument iterator")
    (description
     "When using @code{matplotlib} and plotting more than one line, it is
common to want to be able to want to be able to cycle over one or more artist
styles; but the plotting logic can quickly become involved.
To address this and enable easy cycling over arbitrary @code{kwargs}, the
@code{Cycler} class was developed.")
    (license license:bsd-3)))

(define-public python2-cycler
  (package-with-python2 python-cycler))

(define-public python-colorspacious
  (package
    (name "python-colorspacious")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/njsmith/colorspacious/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version))
        (sha256
         (base32 "1vflh5jm32qb0skza2i8pjacv09w6gq84fqpp2nj77s0rbmzgr4k"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "nosetests" "--all-modules" "-v" "colorspacious")))))))
    (home-page "https://github.com/njsmith/colorspacious")
    (synopsis "Python library for colorspace conversions")
    (description "@code{colorspacious} is a Python library that lets you
convert between colorspaces like sRGB, XYZ, CIEL*a*b*, CIECAM02, CAM02-UCS, etc.")
    (license license:expat)))

(define-public python2-colorspacious
  (package-with-python2 python-colorspacious))

(define-public python-matplotlib
  (package
    (name "python-matplotlib")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "matplotlib" version))
       (sha256
        (base32
         "1w8z2a1l7s72p1byfz7g03wqhygqxi8w82619dqb3a1lm97w9yqg"))))
    (build-system python-build-system)
    (propagated-inputs ; the following packages are all needed at run time
     `(("python-cycler" ,python-cycler)
       ("python-pyparsing" ,python-pyparsing)
       ("python-pygobject" ,python-pygobject)
       ("gobject-introspection" ,gobject-introspection)
       ("python-tkinter" ,python "tk")
       ("python-dateutil" ,python-dateutil)
       ("python-numpy" ,python-numpy)
       ("python-pillow" ,python-pillow)
       ("python-pytz" ,python-pytz)
       ("python-six" ,python-six)
       ;; The 'gtk+' package (and 'gdk-pixbuf', 'atk' and 'pango' propagated
       ;; from 'gtk+') provides the required 'typelib' files used by
       ;; 'gobject-introspection'. The location of these files is set with the
       ;; help of the environment variable GI_TYPELIB_PATH. At build time this
       ;; is done automatically by a 'native-search-path' procedure. However,
       ;; at run-time the user must set this variable as follows:
       ;;
       ;; export GI_TYPELIB_PATH=~/.guix-profile/lib/girepository-1.0
       ("gtk+" ,gtk+)
       ;; From version 1.4.0 'matplotlib' makes use of 'cairocffi' instead of
       ;; 'pycairo'. However, 'pygobject' makes use of a 'pycairo' 'context'
       ;; object. For this reason we need to import both libraries.
       ;; https://pythonhosted.org/cairocffi/cffi_api.html#converting-pycairo
       ("python-pycairo" ,python-pycairo)
       ("python-cairocffi" ,python-cairocffi)))
    (inputs
     `(("libpng" ,libpng)
       ("imagemagick" ,imagemagick)
       ("freetype" ,freetype)
       ("cairo" ,cairo)
       ("glib" ,glib)
       ;; FIXME: Add backends when available.
       ;("python-wxpython" ,python-wxpython)
       ("python-pyqt" ,python-pyqt)
       ("tcl" ,tcl)
       ("tk" ,tk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-nose" ,python-nose)
       ("python-mock" ,python-mock)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure-environment
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((cairo (assoc-ref inputs "cairo"))
                   (gtk+ (assoc-ref inputs "gtk+")))
               ;; Setting these directories in the 'basedirlist' of 'setup.cfg'
               ;; has not effect.
               (setenv "LD_LIBRARY_PATH"
                       (string-append cairo "/lib:" gtk+ "/lib"))
               (setenv "HOME" (getcwd))
               (call-with-output-file "setup.cfg"
                 (lambda (port)
                   (format port "[directories]~%
basedirlist = ~a,~a~%
 [rc_options]~%
backend = TkAgg~%"
                        (assoc-ref inputs "tcl")
                        (assoc-ref inputs "tk")))))
             #t)))))
    (home-page "http://matplotlib.org")
    (synopsis "2D plotting library for Python")
    (description
     "Matplotlib is a Python 2D plotting library which produces publication
quality figures in a variety of hardcopy formats and interactive environments
across platforms.  Matplotlib can be used in Python scripts, the python and
ipython shell, web application servers, and six graphical user interface
toolkits.")
    (license license:psfl)
    (properties `((python2-variant . ,(delay python2-matplotlib))))))

(define-public python2-matplotlib
  (let ((matplotlib (package-with-python2
                     (strip-python2-variant python-matplotlib))))
    (package (inherit matplotlib)
      ;; Make sure to use special packages for Python 2 instead
      ;; of those automatically rewritten by package-with-python2.
      (propagated-inputs
       `(("python2-pycairo" ,python2-pycairo)
         ("python2-functools32" ,python2-functools32)
         ("python2-pygobject-2" ,python2-pygobject-2)
         ("python2-subprocess32" ,python2-subprocess32)
         ("python2-tkinter" ,python-2 "tk")
         ,@(fold alist-delete (package-propagated-inputs matplotlib)
                 '("python-pycairo" "python-pygobject" "python-tkinter")))))))

(define-public python-matplotlib-documentation
  (package
    (name "python-matplotlib-documentation")
    (version (package-version python-matplotlib))
    (source (package-source python-matplotlib))
    (build-system python-build-system)
    (native-inputs
     `(("python-matplotlib" ,python-matplotlib)
       ("python-colorspacious" ,python-colorspacious)
       ("python-sphinx" ,python-sphinx)
       ("python-numpydoc" ,python-numpydoc)
       ("python-ipython" ,python-ipython)
       ("python-mock" ,python-mock)
       ("graphviz" ,graphviz)
       ("texlive" ,texlive)
       ("texinfo" ,texinfo)
       ,@(package-native-inputs python-matplotlib)))
    (arguments
     `(#:tests? #f ; we're only generating documentation
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (chdir "doc")
             ;; Produce pdf in 'A4' format.
             (substitute* "conf.py"
               (("latex_paper_size = 'letter'") "")
               ;; latex_paper_size is deprecated -> set paper size using
               ;; latex_elements
               (("latex_elements\\['pointsize'\\] = '11pt'" match)
                ;; insert at a point where latex_elements{} is defined:
                (string-append match "\nlatex_elements['papersize'] = 'a4paper'")))
             (zero? (system* "python" "make.py" "html" "latex" "texinfo"))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((data (string-append (assoc-ref outputs "out") "/share"))
                    (doc (string-append data "/doc/python-matplotlib-" ,version))
                    (info (string-append data "/info"))
                    (html (string-append doc "/html")))
               (mkdir-p html)
               (mkdir-p info)
               (copy-recursively "build/html" html)
               (symlink (string-append html "/_images")
                        (string-append info "/matplotlib-figures"))
               (with-directory-excursion "build/texinfo"
                 (substitute* "matplotlib.texi"
                   (("@image\\{([^,]*)" all file)
                    (string-append "@image{matplotlib-figures/" file)))
                 (symlink (string-append html "/_images")
                          "./matplotlib-figures")
                 (system* "makeinfo" "--no-split"
                          "-o" "matplotlib.info" "matplotlib.texi"))
               (copy-file "build/texinfo/matplotlib.info"
                          (string-append info "/matplotlib.info"))
               (copy-file "build/latex/Matplotlib.pdf"
                          (string-append doc "/Matplotlib.pdf")))
             #t)))))
    (home-page (package-home-page python-matplotlib))
    (synopsis "Documentation for the python-matplotlib package")
    (description (package-description python-matplotlib))
    (license (package-license python-matplotlib))))

(define-public python2-matplotlib-documentation
  (package-with-python2 python-matplotlib-documentation))

(define-public python2-pysnptools
  (package
    (name "python2-pysnptools")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pysnptools" version ".zip"))
       (sha256
        (base32
         "1wybggjzz8zw7aav4pjsg2h22xp17a1lghrprza1pxwlm7wf96y2"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2)) ; only Python 2.7 is supported
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)
       ("python2-scipy" ,python2-scipy)
       ("python2-pandas" ,python2-pandas)))
    (native-inputs
     `(("python2-cython" ,python2-cython)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://research.microsoft.com/en-us/um/redmond/projects/mscompbio/")
    (synopsis "Library for reading and manipulating genetic data")
    (description
     "PySnpTools is a library for reading and manipulating genetic data.  It
can, for example, efficiently read whole PLINK *.bed/bim/fam files or parts of
those files.  It can also efficiently manipulate ranges of integers using set
operators such as union, intersection, and difference.")
    (license license:asl2.0)))

(define-public python-rpy2
  (package
    (name "python-rpy2")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rpy2" version))
       (sha256
        (base32
         "0bqihjrdqwj5r1h86shvfb1p5hfr4a6klv1v54bzfr9r144w3rni"))))
    (build-system python-build-system)
    (arguments
     '(#:modules ((ice-9 ftw)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (guix build utils)
                  (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         ;; Without this phase the test loader cannot find the directories, in
         ;; which it is supposed to look for test files.
         (add-after 'unpack 'fix-tests
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "rpy/tests.py"
               (("loader.discover\\(")
                "loader.discover(rpy_root + '/' +"))
             #t))
         (replace 'check
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((cwd (getcwd)))
               (setenv "PYTHONPATH"
                       (string-append cwd "/build/"
                                      (find (cut string-prefix? "lib" <>)
                                            (scandir (string-append cwd "/build")))
                                      ":"
                                      (getenv "PYTHONPATH"))))
             ;; FIXME: Even when all tests pass, the check phase will fail.
             (system* "python" "-m" "rpy2.tests" "-v"))))))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-jinja2" ,python-jinja2)
       ("python-pytz" ,python-pytz)))
    (inputs
     `(("readline" ,readline)
       ("icu4c" ,icu4c)
       ("pcre" ,pcre)
       ("r-minimal" ,r-minimal)
       ("r-survival" ,r-survival)
       ("r-ggplot2" ,r-ggplot2)
       ("r-rsqlite" ,r-rsqlite)
       ("r-dplyr" ,r-dplyr)
       ("r-dbplyr" ,r-dbplyr)
       ("python-numpy" ,python-numpy)))
    (native-inputs
     `(("zlib" ,zlib)))
    (home-page "http://rpy.sourceforge.net/")
    (synopsis "Python interface to the R language")
    (description "rpy2 is a redesign and rewrite of rpy.  It is providing a
low-level interface to R from Python, a proposed high-level interface,
including wrappers to graphical libraries, as well as R-like structures and
functions.")
    ;; Any of these licenses can be picked for the R interface.  The whole
    ;; project is released under GPLv2+ according to the license declaration
    ;; in "setup.py".
    (license (list license:mpl2.0 license:gpl2+ license:lgpl2.1+))))

(define-public python-scipy
  (package
    (name "python-scipy")
    (version "0.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scipy" version))
       (sha256
        (base32
         "1rl411bvla6q7qfdb47fpdnyjhfgzl6smpha33n9ar1klykjr6m1"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-matplotlib" ,python-matplotlib)
       ("python-pyparsing" ,python-pyparsing)))
    (inputs
     `(("lapack" ,lapack)
       ("openblas" ,openblas)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-nose" ,python-nose)
       ("python-sphinx" ,python-sphinx)
       ("python-numpydoc" ,python-numpydoc)
       ("gfortran" ,gfortran)
       ("perl" ,perl)))
    (outputs '("out" "doc"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure-openblas
           (lambda* (#:key inputs #:allow-other-keys)
             (call-with-output-file "site.cfg"
               (lambda (port)
                 (format port
                         "[blas]
libraries = openblas
library_dirs = ~a/lib
include_dirs = ~a/include

# backslash-n to make emacs happy
\n[atlas]
library_dirs = ~a/lib
atlas_libs = openblas
"
                         (assoc-ref inputs "openblas")
                         (assoc-ref inputs "openblas")
                         (assoc-ref inputs "openblas"))))
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                    (doc (string-append data "/doc/" ,name "-" ,version))
                    (html (string-append doc "/html"))
                    (pyver ,(string-append "PYVER=")))
               ;; Make installed package available for building the
               ;; documentation
               (add-installed-pythonpath inputs outputs)
               (with-directory-excursion "doc"
                 ;; Fix generation of images for mathematical expressions.
                 (substitute* (find-files "source" "conf\\.py")
                   (("pngmath_use_preview = True")
                    "pngmath_use_preview = False"))
                 (mkdir-p html)
                 (system* "make" "html" pyver)
                 (with-directory-excursion "build/html"
                   (for-each (lambda (file)
                               (let* ((dir (dirname file))
                                      (tgt-dir (string-append html "/" dir)))
                                 (install-file file html)))
                             (find-files "." ".*")))))
             #t))
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "scipy/integrate/tests/test_quadpack.py"
               (("libm.so") "libm.so.6"))
             #t))
           ;; Tests can only be run after the library has been installed and not
           ;; within the source directory.
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (with-directory-excursion "/tmp"
               (zero? (system* "python" "-c"
                               "import scipy; scipy.test('full')")))
             #t)))))
    (home-page "https://www.scipy.org/")
    (synopsis "The Scipy library provides efficient numerical routines")
    (description "The SciPy library is one of the core packages that make up
the SciPy stack.  It provides many user-friendly and efficient numerical
routines such as routines for numerical integration and optimization.")
    (properties `((python2-variant . ,(delay python2-scipy))))
    (license license:bsd-3)))

(define-public python2-scipy
  (package-with-python2
   (strip-python2-variant python-scipy)))

(define-public python-socksipy-branch
  (package
    (name "python-socksipy-branch")
    (version "1.01")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "SocksiPy-branch" version))
       (sha256
        (base32
         "01l41v4g7fy9fzvinmjxy6zcbhgqaif8dhdqm4w90fwcw9h51a8p"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; There are no tests
    (home-page "https://code.google.com/archive/p/socksipy-branch/")
    (synopsis "Python SOCKS module")
    (description
     "SocksiPy - A Python SOCKS client module.  It provides a
socket-like interface that supports connections to any TCP
service through the use of a SOCKS4, SOCKS5 or HTTP proxy.
The original version was developed by Dan Haim, this is a
branch created by Mario Vilas to address some open issues,
as the original project seems to have been abandoned circa 2007.")
    (license license:bsd-3)))

(define-public python2-socksipy-branch
  (package-with-python2 python-socksipy-branch))

(define-public python-pycodestyle
  (package
    (name "python-pycodestyle")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pycodestyle" version))
        (sha256
          (base32
            "0rk78b66p57ala26mdldl9lafr48blv5s659sah9q50qnfjmc8k8"))))
    (build-system python-build-system)
    (home-page "https://pycodestyle.readthedocs.io/")
    (synopsis "Python style guide checker")
    (description "@code{pycodestyle} (formerly pep8) is a tool to check
Python code against some of the style conventions in
@url{http://www.python.org/dev/peps/pep-0008/,PEP 8}.")
    (license license:expat)))

(define-public python2-pycodestyle
  (package-with-python2 python-pycodestyle))

(define-public python-orderedmultidict
  (package
    (name "python-orderedmultidict")
    (version "0.7.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "orderedmultidict" version))
        (sha256
          (base32
            "0dls862ibm7qbq4fnvwx0xn1v9hwyzywbff8xjjdr42dd75208yw"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             ;; The package uses nosetest for running the tests.
             ;; Adding this initfile allows to run the test suite
             ;; without requiring nosetest.
             (zero? (system* "touch" "tests/__init__.py")))))))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (native-inputs
     `(("python-pycodestyle" ,python-pycodestyle)))
    (home-page "https://github.com/gruns/orderedmultidict")
    (synopsis "Python Ordered Multivalue Dictionary - omdict")
    (description "This package contains a library for ordered multivalue
dictionaries.  A multivalue dictionary is a dictionary that can store
multiple values for the same key.  An ordered multivalue dictionary is a
multivalue dictionary that retains the order of insertions and deletions.")
    (license license:unlicense)))

(define-public python2-orderedmultidict
  (package-with-python2 python-orderedmultidict))

(define-public python-autopep8
  (package
  (name "python-autopep8")
  (version "1.3.2")
  (source
   (origin
     (method url-fetch)
     (uri (pypi-uri "autopep8" version))
     (sha256
      (base32
       "1p9pa1ffg4iy96l918808jggg9a69iaka5awmj8xid36yc5mk0ky"))))
  (build-system python-build-system)
  (propagated-inputs
    `(("python-pycodestyle" ,python-pycodestyle)))
  (home-page "https://github.com/hhatto/autopep8")
  (synopsis "Format Python code according to the PEP 8 style guide")
  (description
    "@code{autopep8} automatically formats Python code to conform to
the PEP 8 style guide.  It uses the pycodestyle utility to determine
what parts of the code needs to be formatted.  @code{autopep8} is
capable of fixing most of the formatting issues that can be reported
by pycodestyle.")
  (license (license:non-copyleft
            "https://github.com/hhatto/autopep8/blob/master/LICENSE"))))

(define-public python2-autopep8
  (package-with-python2 python-autopep8))

(define-public python-distutils-extra
  (package
    (name "python-distutils-extra")
    (version "2.38")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://launchpad.net/python-distutils-extra/trunk/"
                          version "/+download/python-distutils-extra-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0lx15kcbby9zisx33p2h5hgakgwh2bvh0ibag8z0px4j6ifhs41x"))))
    (build-system python-build-system)
    (home-page "https://launchpad.net/python-distutils-extra/")
    (synopsis "Enhancements to Python's distutils")
    (description
     "The python-distutils-extra module enables you to easily integrate
gettext support, themed icons, and scrollkeeper-based documentation into
Python's distutils.")
    (license license:gpl2)))

(define-public python2-distutils-extra
  (package-with-python2 python-distutils-extra))

(define-public python2-elib.intl
  (package
    (name "python2-elib.intl")
    (version "0.0.3")
    (source
     (origin
       ;; This project doesn't tag releases or publish tarballs, so we take
       ;; source from a (semi-arbitrary, i.e. latest as of now) git commit.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dieterv/elib.intl.git")
             (commit "d09997cfef")))
       (sha256
        (base32
         "0y7vzff9xgbnaay7m0va1arl6g68ncwrvbgwl7jqlclsahzzb09d"))))
    (build-system python-build-system)
    (arguments
     ;; incompatible with Python 3 (exception syntax)
     `(#:python ,python-2
       #:tests? #f))
    (home-page "https://github.com/dieterv/elib.intl")
    (synopsis "Enhanced internationalization for Python")
    (description
     "The elib.intl module provides enhanced internationalization (I18N)
services for your Python modules and applications.")
    (license license:lgpl3+)))

(define-public python-olefile
  (package
    (name "python-olefile")
    (version "0.44")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/decalage2/olefile/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wmxbrhyqjry2000zx0zdhqdqxhgi06nz7sbzjlh222q2zjv1gpj"))))
    (build-system python-build-system)
    (home-page
     "https://www.decalage.info/python/olefileio")
    (synopsis "Read and write Microsoft OLE2 files.")
    (description
     "@code{olefile} can parse, read and write Microsoft OLE2 files (Structured
Storage or Compound Document, Microsoft Office).  It is an improved version of
the OleFileIO module from PIL, the Python Image Library.")
    (license license:bsd-3)))

(define-public python2-olefile
  (package-with-python2 python-olefile))

(define-public python-pillow
  (package
    (name "python-pillow")
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pillow" version))
       (sha256
        (base32
         "09xmn7rl6840sli2iz1k3fgxfgmri2nqz6vkinmb9mgg8ifp2z59"))
       (patch-flags '("-p1" "--binary"))
       (patches (search-patches "python-pillow-fix-failing-tests.patch"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose"       ,python-nose)))
    (inputs
     `(("freetype" ,freetype)
       ("lcms"     ,lcms)
       ("zlib"     ,zlib)
       ("libjpeg"  ,libjpeg)
       ("openjpeg" ,openjpeg)
       ("libtiff"  ,libtiff)
       ("libwebp"  ,libwebp)))
    (propagated-inputs
     `(("python-olefile" ,python-olefile)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after
                   'install 'check-installed
                   (lambda* (#:key outputs inputs #:allow-other-keys)
                     (begin
                       (setenv "HOME" (getcwd))
                       ;; Make installed package available for running the
                       ;; tests
                       (add-installed-pythonpath inputs outputs)
                       (and (zero? (system* "python" "selftest.py"
                                            "--installed"))
                            (zero? (system* "python" "test-installed.py"))))))
                 (delete 'check))))
    (home-page "https://pypi.python.org/pypi/Pillow")
    (synopsis "Fork of the Python Imaging Library")
    (description
     "The Python Imaging Library adds image processing capabilities to your
Python interpreter.  This library provides extensive file format support, an
efficient internal representation, and fairly powerful image processing
capabilities.  The core image library is designed for fast access to data
stored in a few basic pixel formats.  It should provide a solid foundation for
a general image processing tool.")
    (license (license:x11-style
              "http://www.pythonware.com/products/pil/license.htm"
              "The PIL Software License"))))

(define-public python2-pillow
  (package-with-python2 python-pillow))

(define-public python-pycparser
  (package
    (name "python-pycparser")
    (version "2.17")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "pycparser" version))
      (sha256
       (base32
        "1dkkjri0miidqb1zcqhqljfa34fcy9k5akasgwsv6k622zlk3b0a"))))
    (outputs '("out" "doc"))
    (build-system python-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (with-directory-excursion "tests"
               (zero? (system* "python" "all_tests.py")))))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                    (doc (string-append data "/doc/" ,name "-" ,version))
                    (examples (string-append doc "/examples")))
               (mkdir-p examples)
               (for-each (lambda (file)
                           (copy-file (string-append "." file)
                                      (string-append doc file)))
                         '("/README.rst" "/CHANGES" "/LICENSE"))
               (copy-recursively "examples" examples)))))))
    (home-page "https://github.com/eliben/pycparser")
    (synopsis "C parser in Python")
    (description
     "Pycparser is a complete parser of the C language, written in pure Python
using the PLY parsing library.  It parses C code into an AST and can serve as
a front-end for C compilers or analysis tools.")
    (license license:bsd-3)))

(define-public python2-pycparser
  (package-with-python2 python-pycparser))

(define-public python-xcffib
  (package
    (name "python-xcffib")
    (version "0.5.1")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "xcffib" version))
      (sha256
       (base32
        "09gbnmr5vn58mm8xi3fmd7fz6743cks6c46dphnxzwax6zsxmy60"))))
    (build-system python-build-system)
    (inputs
     `(("libxcb" ,libxcb)))
    (propagated-inputs
     `(("python-cffi" ,python-cffi) ; used at run time
       ("python-six" ,python-six)))
    (arguments
     `(;; FIXME: Tests need more work. See ".travis.yml" in the repository.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-libxcb-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libxcb (assoc-ref inputs "libxcb")))
               (substitute* '("xcffib/__init__.py")
                 (("^soname = \"") (string-append "soname = \"" libxcb "/lib/")))
               #t)))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc (string-append (assoc-ref outputs "out") "/share"
                                       "/doc/" ,name "-" ,version)))
               (mkdir-p doc)
               (copy-file "README.md"
                          (string-append doc "/README.md"))
               #t))))))
    (home-page "https://github.com/tych0/xcffib")
    (synopsis "XCB Python bindings")
    (description
     "Xcffib is a replacement for xpyb, an XCB Python bindings.  It adds
support for Python 3 and PyPy.  It is based on cffi.")
    (license license:expat)))

(define-public python2-xcffib
  (package-with-python2 python-xcffib))

(define-public python-cairocffi
  (package
    (name "python-cairocffi")
    (version "0.8.0")
    (source
     (origin
      (method url-fetch)
      ;; The archive on pypi is missing the 'utils' directory!
      (uri (string-append "https://github.com/Kozea/cairocffi/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "1rk2dvy3fxrga6bvvxc2fi5lbaynm5h4a0w0aaxyn3bc77rszjg9"))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("cairo" ,cairo)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)
       ("python-docutils" ,python-docutils)))
    (propagated-inputs
     `(("python-xcffib" ,python-xcffib))) ; used at run time
    (arguments
     `(;; FIXME: Tests cannot find 'libcairo.so.2'.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                    (doc (string-append data "/doc/" ,name "-" ,version))
                    (html (string-append doc "/html")))
               (setenv "LD_LIBRARY_PATH"
                       (string-append (assoc-ref inputs "cairo") "/lib" ":"
                                      (assoc-ref inputs "gdk-pixbuf") "/lib"))
               (setenv "LANG" "en_US.UTF-8")
               (mkdir-p html)
               (for-each (lambda (file)
                           (copy-file (string-append "." file)
                                      (string-append doc file)))
                         '("/README.rst" "/CHANGES" "/LICENSE"))
               (system* "python" "setup.py" "build_sphinx")
               (copy-recursively "docs/_build/html" html)
               #t))))))
    (home-page "https://github.com/Kozea/cairocffi")
    (synopsis "Python bindings and object-oriented API for Cairo")
    (description
     "Cairocffi is a CFFI-based drop-in replacement for Pycairo, a set of
Python bindings and object-oriented API for cairo.  Cairo is a 2D vector
graphics library with support for multiple backends including image buffers,
PNG, PostScript, PDF, and SVG file output.")
    (license license:bsd-3)))

(define-public python2-cairocffi
  (package-with-python2 python-cairocffi))

(define-public python-decorator
  (package
    (name "python-decorator")
    (version "4.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "decorator" version))
       (sha256
        (base32 "1d8npb11kxyi36mrvjdpcjij76l5zfyrz2f820brf0l0rcw4vdkw"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; no test target
    (home-page "https://pypi.python.org/pypi/decorator/")
    (synopsis "Python module to simplify usage of decorators")
    (description
      "The aim of the decorator module is to simplify the usage of decorators
for the average programmer, and to popularize decorators usage giving examples
of useful decorators, such as memoize, tracing, redirecting_stdout, locked,
etc.  The core of this module is a decorator factory.")
    (license license:expat)))

(define-public python2-decorator
  (package-with-python2 python-decorator))

(define-public python-drmaa
  (package
    (name "python-drmaa")
    (version "0.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "drmaa" version))
       (sha256
        (base32 "0xzqriqyvk5b8hszbavsyxd29wm3sxirm8zvvdm73rs2iq7w4hkx"))))
    (build-system python-build-system)
    ;; The test suite requires libdrmaa which is provided by the cluster
    ;; environment.  At runtime the environment variable DRMAA_LIBRARY_PATH
    ;; should be set to the path of the libdrmaa library.
    (arguments '(#:tests? #f))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://pypi.python.org/pypi/drmaa")
    (synopsis "Python bindings for the DRMAA library")
    (description
      "A Python package for Distributed Resource Management (DRM) job
submission and control.  This package is an implementation of the DRMAA 1.0
Python language binding specification.")
    (license license:bsd-3)))

(define-public python2-drmaa
  (package-with-python2 python-drmaa))

(define-public python-grako
  (package
    (name "python-grako")
    (version "3.99.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (pypi-uri "grako" version ".zip"))
       (sha256
        (base32
         "0r63i68wcnv63rfjkasq1ah81frz61a6mzbcnaxhrkdpx84p7hzw"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; Test file 'grako.ebnf' is missing from archive.
    (native-inputs
     `(("unzip" ,unzip)
       ("python-pytest" ,python-pytest-3.0)
       ("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://bitbucket.org/neogeny/grako")
    (synopsis "EBNF parser generator")
    (description
     "Grako takes a grammar in a variation of EBNF as input, and outputs a
memoizing PEG/Packrat parser in Python.")
    (license license:bsd-3)))

(define-public python2-grako
  (package-with-python2 python-grako))

(define-public python-gridmap
  (package
    (name "python-gridmap")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/pygridtools/gridmap/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gzjg2k6f14i1msm2b0ax8d9ds1hvk6qd5nlaivg8m4cxqp4cp1x"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; FIXME: Requires python-cherrypy.
    (propagated-inputs
     `(("python-psutil" ,python-psutil)
       ("python-drmaa" ,python-drmaa)
       ("python-pyzmq" ,python-pyzmq)))
    (home-page "https://github.com/pygridtools/gridmap")
    (synopsis "Create jobs on a cluster directly from Python")
    (description
      "Gridmap is a Python package to allow you to easily create jobs on the
cluster directly from Python.  You can directly map Python functions onto the
cluster without needing to write any wrapper code yourself.")
    (license license:gpl3+)))

(define-public python2-gridmap
  (package-with-python2 python-gridmap))

(define-public python-honcho
  (package
    (name "python-honcho")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/nickstenning/honcho/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zizn61n5z5hq421hkypk9pw8s6fpxw30f4hsg7k4ivwzy3gjw9j"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest-3.0)
       ("python-mock" ,python-mock)
       ("python-tox" ,python-tox)
       ("which" ,which))) ;for tests
    (propagated-inputs
     `(("python-jinja2" ,python-jinja2)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; fix honcho path in testsuite
             (substitute* "tests/conftest.py"
               (("'honcho'") (string-append "'" (assoc-ref outputs "out")
                                            "/bin/honcho" "'")))
             ;; It's easier to run tests after install.
             ;; Make installed package available for running the tests
             (add-installed-pythonpath inputs outputs)
             (zero? (system* "py.test" "-v")))))))
    (home-page "https://github.com/nickstenning/honcho")
    (synopsis "Manage Procfile-based applications")
    (description
      "A Procfile is a file which describes how to run an application
consisting of serveral processes. honcho starts all listed processes.
The output of all running processes is collected by honcho and
displayed.")
    (license license:expat)))

(define-public python2-honcho
  (package-with-python2 python-honcho))

(define-public python-pexpect
  (package
    (name "python-pexpect")
    (version "4.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pexpect" version))
       (sha256
        (base32 "14ls7k99pwvl21zqv65kzrhccv50j89m5ij1hf0slmsvlxjj84rx"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'prepare-tests
           (lambda _
             (substitute* (find-files "tests")
               (("/bin/ls") (which "ls"))
               (("/bin/echo") (which "echo"))
               (("/bin/which") (which "which"))
               ;; Many tests try to use the /bin directory which
               ;; is not present in the build environment.
               ;; Use one that's non-empty and unlikely to change.
               (("/bin'") "/dev'"))
             ;; XXX: Socket connection test gets "Connection reset by peer".
             ;; Why does it not work? Delete for now.
             (delete-file "tests/test_socket.py")
             #t))
         (replace 'check (lambda _ (zero? (system* "nosetests" "-v")))))))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-pytest" ,python-pytest-3.0)
       ("man-db" ,man-db)
       ("which" ,which)
       ("bash-full" ,bash)))                 ;full Bash for 'test_replwrap.py'
    (propagated-inputs
     `(("python-ptyprocess" ,python-ptyprocess)))
    (home-page "http://pexpect.readthedocs.org/")
    (synopsis "Controlling interactive console applications")
    (description
     "Pexpect is a pure Python module for spawning child applications;
controlling them; and responding to expected patterns in their output.
Pexpect works like Don Libes’ Expect.  Pexpect allows your script to spawn a
child application and control it as if a human were typing commands.")
    (license license:isc)))

(define-public python2-pexpect
  (package-with-python2 python-pexpect))

(define-public python-setuptools-scm
  (package
    (name "python-setuptools-scm")
    (version "1.15.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "setuptools_scm" version))
              (sha256
               (base32
                "0bwyc5markib0i7i2qlyhdzxhiywzxbkfiapldma8m91m82jvwfs"))))
    (build-system python-build-system)
    (home-page "https://github.com/pypa/setuptools_scm/")
    (synopsis "Manage Python package versions in SCM metadata")
    (description
     "Setuptools_scm handles managing your Python package versions in
@dfn{software configuration management} (SCM) metadata instead of declaring
them as the version argument or in a SCM managed file.")
    (license license:expat)))

(define-public python2-setuptools-scm
  (package-with-python2 python-setuptools-scm))

(define-public python-pathpy
  (package
    (name "python-pathpy")
    (version "8.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/p/"
                           "path.py/path.py-" version ".tar.gz"))
       (sha256
        (base32 "1p8s1l2vfkqhqxdhqlj0g1jjw4f1as2frr35sjcpjjpd5a89y41f"))))
    (outputs '("out" "doc"))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-appdirs" ,python-appdirs)))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)
       ("python-sphinx" ,python-sphinx)
       ("python-rst.linker" ,python-rst.linker)
       ("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-doc
           (lambda _
             (setenv "LANG" "en_US.UTF-8")
             (zero? (system* "python" "setup.py" "build_sphinx"))))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                    (doc (string-append data "/doc/" ,name "-" ,version))
                    (html (string-append doc "/html")))
               (mkdir-p html)
               (for-each (lambda (file)
                           (copy-file file (string-append doc "/" file)))
                         '("README.rst" "CHANGES.rst"))
               (copy-recursively "build/sphinx/html" html)))))))
    (home-page "https://github.com/jaraco/path.py")
    (synopsis "Python module wrapper for built-in os.path")
    (description
     "@code{path.py} implements path objects as first-class entities, allowing
common operations on files to be invoked on those path objects directly.")
    (license license:expat)))

(define-public python2-pathpy
  (package-with-python2 python-pathpy))

(define-public python-simplegeneric
  (package
    (name "python-simplegeneric")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/s/"
                           "simplegeneric/simplegeneric-" version ".zip"))
       (sha256
        (base32 "0wwi1c6md4vkbcsfsf8dklf3vr4mcdj4mpxkanwgb6jb1432x5yw"))))
    (build-system python-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://cheeseshop.python.org/pypi/simplegeneric")
    (synopsis "Python module for simple generic functions")
    (description
     "The simplegeneric module lets you define simple single-dispatch generic
functions, akin to Python’s built-in generic functions like @code{len()},
@code{iter()} and so on.  However, instead of using specially-named methods,
these generic functions use simple lookup tables, akin to those used by
e.g. @code{pickle.dump()} and other generic functions found in the Python
standard library.")
    (license license:zpl2.1)))

(define-public python2-simplegeneric
  (package-with-python2 python-simplegeneric))

(define-public python-ipython-genutils
  ;; TODO: This package is retired, check if can be removed, see description.
  (package
    (name "python-ipython-genutils")
    (version "0.1.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/i/"
                          "ipython_genutils/ipython_genutils-"
                          version ".tar.gz"))
      (sha256
       (base32 "19l2pp1c64ansr89l3cqh19jdi2ixhssdzx0vz4n6r52a6i281is"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; no tests
    (home-page "http://ipython.org")
    (synopsis "Vestigial utilities from IPython")
    (description
     "This package provides retired utilities from IPython.  No packages
outside IPython/Jupyter should depend on it.

This package shouldn't exist.  It contains some common utilities shared by
Jupyter and IPython projects during The Big Split.  As soon as possible, those
packages will remove their dependency on this, and this package will go
away.")
    (license license:bsd-3)))

(define-public python2-ipython-genutils
  (package-with-python2 python-ipython-genutils))

(define-public python-traitlets
  (package
    (name "python-traitlets")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "traitlets" version))
       (sha256
        (base32
         "1afy08sa5n9gnkvh3da49c16zkyv598vchv0p1hp7zzjy8895hz4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (propagated-inputs
     `(("python-ipython-genutils" ,python-ipython-genutils)
       ("python-decorator" ,python-decorator)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (home-page "http://ipython.org")
    (synopsis "Configuration system for Python applications")
    (description
     "Traitlets is a framework that lets Python classes have attributes with
type checking, dynamically calculated default values, and ‘on change’
callbacks.  The package also includes a mechanism to use traitlets for
configuration, loading values from files or from command line arguments.  This
is a distinct layer on top of traitlets, so you can use traitlets in your code
without using the configuration machinery.")
    (license license:bsd-3)))

(define-public python2-traitlets
  (package-with-python2 python-traitlets))

(define-public python-jupyter-core
  (package
    (name "python-jupyter-core")
    (version "4.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append (pypi-uri "jupyter_core" version)))
       (sha256
        (base32
         "1cy7inv218dgh4m1fbzbsiqpz733ylgjrj62jxqpfzs3r2cm7ic9"))))
    (build-system python-build-system)
    ;; FIXME: not sure how to run the tests
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-traitlets" ,python-traitlets)))
    (home-page "http://jupyter.org/")
    (synopsis "Jupyter base package")
    (description
     "Jupyter core is the base package on which Jupyter projects rely.")
    (license license:bsd-3)))

(define-public python2-jupyter-core
  (package-with-python2 python-jupyter-core))

(define-public python-jupyter-client
  (package
    (name "python-jupyter-client")
    (version "4.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_client" version))
       (sha256
        (base32
         "1vjjrpjw7k5sh982pbjnslv7byfbfazjw9g92jvs7dz5qbx556n9"))))
    (build-system python-build-system)
    ;; Tests fail because of missing native python kernel which I assume is
    ;; provided by the ipython package, which we cannot use because it would
    ;; cause a dependency cycle.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-pyzmq" ,python-pyzmq)
       ("python-traitlets" ,python-traitlets)
       ("python-jupyter-core" ,python-jupyter-core)))
    (home-page "http://jupyter.org/")
    (synopsis "Jupyter protocol implementation and client libraries")
    (description
     "The @code{jupyter_client} package contains the reference implementation
of the Jupyter protocol.  It also provides client and kernel management APIs
for working with kernels, and the @code{jupyter kernelspec} entrypoint for
installing @code{kernelspec}s for use with Jupyter frontends.")
    (license license:bsd-3)))

(define-public python2-jupyter-client
  (package-with-python2 python-jupyter-client))

(define-public python-ipykernel
  (package
    (name "python-ipykernel")
    (version "4.5.2")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "ipykernel" version))
      (sha256
       (base32 "0qllv0k6zzv1r1cj1x2ygxmlrrqhbslzj8rc6r6fg3kc1rgz4m2s"))))
    (build-system python-build-system)
    ;; The tests load a submodule of IPython.  However, IPython itself depends
    ;; on ipykernel.
    (arguments `(#:tests? #f))
    (propagated-inputs
     ;; imported at runtime during connect
     `(("python-jupyter-client" ,python-jupyter-client)))
    (home-page "http://ipython.org")
    (synopsis "IPython Kernel for Jupyter")
    (description
     "This package provides the IPython kernel for Jupyter.")
    (license license:bsd-3)))

(define-public python2-ipykernel
  (package-with-python2 python-ipykernel))

(define-public python-ipython
  (package
    (name "python-ipython")
    (version "5.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipython" version ".tar.gz"))
       (sha256
        (base32 "079wyjir4a9qx6kvx096b1asm63djbidk65z3ykcbnlngmg62pmz"))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (propagated-inputs
     `(("python-pyzmq" ,python-pyzmq)
       ("python-prompt-toolkit" ,python-prompt-toolkit)
       ("python-terminado" ,python-terminado)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-numpydoc" ,python-numpydoc)
       ("python-jinja2" ,python-jinja2)
       ("python-jupyter-console"
        ;; The python-ipython and python-jupyter-console require each
        ;; other. To get the functionality in both packages working, strip
        ;; down the python-jupyter-console package when using it as an input
        ;; to python-ipython.
        ,python-jupyter-console-minimal)
       ("python-mistune" ,python-mistune)
       ("python-pexpect" ,python-pexpect)
       ("python-pickleshare" ,python-pickleshare)
       ("python-simplegeneric" ,python-simplegeneric)
       ("python-jsonschema" ,python-jsonschema)
       ("python-traitlets" ,python-traitlets)
       ("python-ipykernel" ,python-ipykernel)
       ("python-nbformat" ,python-nbformat)
       ("python-pygments" ,python-pygments)))
    (inputs
     `(("readline" ,readline)
       ("which" ,which)))
    (native-inputs
     `(("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)
       ("python-requests" ,python-requests) ;; for tests
       ("python-testpath" ,python-testpath)
       ("python-nose" ,python-nose)
       ("python-sphinx" ,python-sphinx)
       ("python-shpinx-rtd-theme" ,python-sphinx-rtd-theme)
       ;; FIXME: It's possible that a smaller union would work just as well.
       ("texlive" ,(texlive-union (list texlive-fonts-amsfonts
                                        texlive-fonts-ec
                                        texlive-generic-ifxetex
                                        texlive-generic-pdftex
                                        texlive-latex-amsfonts
                                        texlive-latex-capt-of
                                        texlive-latex-cmap
                                        texlive-latex-environ
                                        texlive-latex-eqparbox
                                        texlive-latex-etoolbox
                                        texlive-latex-expdlist
                                        texlive-latex-fancyhdr
                                        texlive-latex-fancyvrb
                                        texlive-latex-fncychap
                                        texlive-latex-float
                                        texlive-latex-framed
                                        texlive-latex-geometry
                                        texlive-latex-graphics
                                        texlive-latex-hyperref
                                        texlive-latex-mdwtools
                                        texlive-latex-multirow
                                        texlive-latex-oberdiek
                                        texlive-latex-parskip
                                        texlive-latex-preview
                                        texlive-latex-tabulary
                                        texlive-latex-threeparttable
                                        texlive-latex-titlesec
                                        texlive-latex-trimspaces
                                        texlive-latex-ucs
                                        texlive-latex-upquote
                                        texlive-latex-url
                                        texlive-latex-varwidth
                                        texlive-latex-wrapfig)))
       ("texinfo" ,texinfo)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'install 'install-doc
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                   (doc (string-append data "/doc/" ,name "-" ,version))
                   (html (string-append doc "/html"))
                   (man1 (string-append data "/man/man1"))
                   (info (string-append data "/info"))
                   (examples (string-append doc "/examples"))
                   (python-arg (string-append "PYTHON=" (which "python"))))
              (setenv "LANG" "en_US.utf8")
              ;; Make installed package available for running the tests
              (add-installed-pythonpath inputs outputs)
              (with-directory-excursion "docs"
                ;; FIXME: pdf fails to build
                ;;(system* "make" "pdf" "PAPER=a4")
                (system* "make" python-arg "html")
                (system* "make" python-arg "info"))
              (copy-recursively "docs/man" man1)
              (copy-recursively "examples" examples)
              (copy-recursively "docs/build/html" html)
              ;; (copy-file "docs/build/latex/ipython.pdf"
              ;;            (string-append doc "/ipython.pdf"))
              (mkdir-p info)
              (copy-file "docs/build/texinfo/ipython.info"
                         (string-append info "/ipython.info"))
              (copy-file "COPYING.rst" (string-append doc "/COPYING.rst")))))
         ;; Tests can only be run after the library has been installed and not
         ;; within the source directory.
         (delete 'check)
         (add-after
          'install 'check
          (lambda* (#:key inputs outputs tests? #:allow-other-keys)
            (if tests?
                (with-directory-excursion "/tmp"
                  ;; Make installed package available for running the tests
                  (add-installed-pythonpath inputs outputs)
                  (setenv "HOME" "/tmp/") ;; required by a test
                  (zero? (system* (string-append (assoc-ref outputs "out")
                                                 "/bin/iptest"))))
                #t)))
         (add-before
          'install 'fix-tests
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "./IPython/utils/_process_posix.py"
              (("/usr/bin/env', 'which") (which "which")))
            (substitute* "./IPython/core/tests/test_inputtransformer.py"
              (("#!/usr/bin/env python")
               (string-append "#!" (which "python"))))
            ;; Disable 1 failing test
            (substitute* "./IPython/core/tests/test_magic.py"
              (("def test_dirops\\(\\):" all)
               (string-append "@dec.skipif(True)\n" all))))))))
    (home-page "http://ipython.org")
    (synopsis "IPython is a tool for interactive computing in Python")
    (description
     "IPython provides a rich architecture for interactive computing with:
Powerful interactive shells, a browser-based notebook, support for interactive
data visualization, embeddable interpreters and tools for parallel
computing.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-ipython))))))

(define-public python2-ipython
  (let ((ipython (package-with-python2 (strip-python2-variant python-ipython))))
    (package
      (inherit ipython)
      ;; FIXME: add pyreadline once available.
      (propagated-inputs
       `(("python2-backports-shutil-get-terminal-size"
          ,python2-backports-shutil-get-terminal-size)
         ("python2-pathlib2" ,python2-pathlib2)
         ,@(package-propagated-inputs ipython)))
      (native-inputs
       `(("python2-mock" ,python2-mock)
         ,@(package-native-inputs ipython))))))

(define-public python-urwid
  (package
    (name "python-urwid")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "urwid" version))
       (sha256
        (base32
         "18cnd1wdjcas08x5qwa5ayw6jsfcn33w4d9f7q3s29fy6qzc1kng"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Disable failing test. Bug filed upstream:
         ;; https://github.com/wardi/urwid/issues/164
         ;; TODO: check again for python-urwid > 1.3.1 or python > 3.4.3.
         (add-after 'unpack 'disable-failing-test
          (lambda _
            (substitute* "urwid/tests/test_event_loops.py"
              (("test_remove_watch_file")
                "disable_remove_watch_file")))))))
    (home-page "http://urwid.org")
    (synopsis "Console user interface library for Python")
    (description
     "Urwid is a curses-based UI/widget library for Python.  It includes many
features useful for text console applications.")
    (license license:lgpl2.1+)))

(define-public python2-urwid
  (let ((python2-urwid (package-with-python2 python-urwid)))
    (package
      (inherit python2-urwid)
      (arguments
       (append
        `(;; Explicitly using Python 2 is necessary due the argument list being
          ;; built from only the 'delete-test_vterm.py' phase and python-urwid's
          ;; package arguments, which by default assumes the use of Python 3.
          #:python ,python-2
          #:phases
          (modify-phases %standard-phases
            ;; Disable the vterm tests because of non-deterministic failures
            ;; with Python 2. See https://github.com/urwid/urwid/issues/230.
            (add-after 'unpack 'delete-test_vterm.py
              (delete-file "urwid/tests/test_vterm.py"))))
        (package-arguments python-urwid))))))

(define-public python-urwidtrees
  (package
    (name "python-urwidtrees")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        ;; package author intends on distributing via github rather than pypi:
        ;; https://github.com/pazz/alot/issues/877#issuecomment-230173331
        (uri (string-append "https://github.com/pazz/urwidtrees/archive/"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0d30lyd3s2a97rhqfax5w9ssqds2z6aydqx3c6j2c2lk3cb4ngvh"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (propagated-inputs `(("python-urwid" ,python-urwid)))
    (home-page "https://github.com/pazz/urwidtrees")
    (synopsis "Tree widgets for urwid")
    (description "Urwidtrees is a Widget Container API for the @code{urwid}
toolkit.  Use it to build trees of widgets.")
    (license license:gpl3+)))

(define-public python2-urwidtrees
  (package-with-python2 python-urwidtrees))

(define-public python-dbus
  (package
    (name "python-dbus")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://dbus.freedesktop.org/releases/dbus-python/dbus-python-"
             version ".tar.gz"))
       (sha256
        (base32 "1py62qir966lvdkngg0v8k1khsqxwk5m4s8nflpk1agk5f5nqb71"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before
          'check 'pre-check
          (lambda _
            ;; XXX: For the missing '/etc/machine-id'.
            (substitute* "test/run-test.sh"
              (("DBUS_FATAL_WARNINGS=1")
               "DBUS_FATAL_WARNINGS=0"))
            #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python)
       ("dbus-glib" ,dbus-glib)))
    (synopsis "Python bindings for D-bus")
    (description "python-dbus provides bindings for libdbus, the reference
implementation of D-Bus.")
    (home-page "http://www.freedesktop.org/wiki/Software/DBusBindings/")
    (license license:expat)))

(define-public python2-dbus
  (package (inherit python-dbus)
    (name "python2-dbus")
    (inputs `(("python" ,python-2)
              ,@(alist-delete "python"
                              (package-inputs python-dbus)
                              equal?)))
    ;; FIXME: on Python 2, the test_utf8 fails with:
    ;; "ValueError: unichr() arg not in range(0x10000) (narrow Python build)"
    (arguments `(#:tests? #f))))

(define-public python-lxml
  (package
    (name "python-lxml")
    (version "3.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "lxml" version))
        (sha256
         (base32
          "15nvf6n285n282682qyw3wihsncb0x5amdhyi4b83bfa2nz74vvk"))))
    (build-system python-build-system)
    (inputs
      `(("libxml2" ,libxml2)
        ("libxslt" ,libxslt)))
    (home-page "http://lxml.de/")
    (synopsis
      "Python XML processing library")
    (description
      "The lxml XML toolkit is a Pythonic binding for the C libraries
libxml2 and libxslt.")
    (license license:bsd-3))) ; and a few more, see LICENSES.txt

(define-public python2-lxml
  (package-with-python2 python-lxml))

;; beautifulsoup4 has a totally different namespace than 3.x,
;; and pypi seems to put it under its own name, so I guess we should too
(define-public python-beautifulsoup4
  (package
    (name "python-beautifulsoup4")
    (version "4.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "beautifulsoup4" version))
       (sha256
        (base32
         "0glaw1vyxnbp03fni7h5496n6iib0n5iim4gax1n0ngscs9s075j"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The Python 2 source is the definitive source of beautifulsoup4. We
         ;; must use this conversion script when building with Python 3. The
         ;; conversion script also runs the tests.
         ;; For more information, see the file 'convert-py3k' in the source
         ;; distribution.
         (replace 'check
           (lambda _ (zero? (system* "./convert-py3k")))))))
    (home-page
     "https://www.crummy.com/software/BeautifulSoup/bs4/")
    (synopsis
     "Python screen-scraping library")
    (description
     "Beautiful Soup is a Python library designed for rapidly setting up
screen-scraping projects.  It offers Pythonic idioms for navigating,
searching, and modifying a parse tree, providing a toolkit for
dissecting a document and extracting what you need.  It automatically
converts incoming documents to Unicode and outgoing documents to UTF-8.")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-beautifulsoup4))))))

(define-public python2-beautifulsoup4
  (package
    (inherit (package-with-python2
              (strip-python2-variant python-beautifulsoup4)))
    (arguments `(#:python ,python-2))))

(define-public python-netifaces
  (package
    (name "python-netifaces")
    (version "0.10.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://pypi.python.org/packages/source/n/netifaces/netifaces-"
              version
              ".tar.gz"))
        (sha256
          (base32
            "1plw237a4zib4z8s62g0mrs8gm3kjfrp5sxh6bbk9nl3rdls2mln"))))
    (build-system python-build-system)
    (home-page
      "https://bitbucket.org/al45tair/netifaces")
    (synopsis
      "Python module for portable network interface information")
    (description
      "Netifaces is a Python module providing information on network
interfaces in an easy and portable manner.")
    (license license:expat)))

(define-public python2-netifaces
  (package-with-python2 python-netifaces))

(define-public python-networkx
  (package
    (name "python-networkx")
    (version "1.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "networkx" version))
       (sha256
        (base32 "1f74s56xb4ggixiq0vxyfxsfk8p20c7a099lpcf60izv1php03hd"))))
    (build-system python-build-system)
    ;; python-decorator is needed at runtime
    (propagated-inputs
     `(("python-decorator" ,python-decorator)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "http://networkx.github.io/")
    (synopsis "Python module for creating and manipulating graphs and networks")
    (description
      "NetworkX is a Python package for the creation, manipulation, and study
of the structure, dynamics, and functions of complex networks.")
    (license license:bsd-3)))

(define-public python2-networkx
  (package-with-python2 python-networkx))

;; Define new package, because the current version of python-colormath does
;; not build against 2.0.
(define-public python-networkx2
  (package (inherit python-networkx)
    (name "python-networkx2")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "networkx" version ".zip"))
       (sha256
        (base32
         "1ajl2jp8qry9nyjzzkqpy0vmsr14d23z1qk7y0vr5iwjbpvzhpyd"))
       (patches
        (search-patches "python-networkx2-reproducible-build.patch"))))))

(define-public python2-networkx2
  (package-with-python2 python-networkx2))

(define-public snakemake
  (package
    (name "snakemake")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "snakemake" version))
       (sha256
        (base32 "0mgl44q152ws40zj2vicqark5szyd73vqy9pf26g6hk6dk0y0c79"))))
    (build-system python-build-system)
    (arguments
     ;; TODO: Package missing test dependencies.
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; For cluster execution Snakemake will call Python.  Since there is
         ;; no suitable PYTHONPATH set, cluster execution will fail.  We fix
         ;; this by calling the snakemake wrapper instead.
         (add-after 'unpack 'call-wrapper-not-wrapped-snakemake
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "snakemake/executors.py"
               (("\\{sys.executable\\} -m snakemake")
                (string-append (assoc-ref outputs "out")
                               "/bin/snakemake")))
             #t)))))
    (propagated-inputs
     `(("python-wrapt" ,python-wrapt)
       ("python-requests" ,python-requests)
       ("python-appdirs" ,python-appdirs)
       ("python-configargparse" ,python-configargparse)
       ("python-pyyaml" ,python-pyyaml)
       ("python-ratelimiter" ,python-ratelimiter)))
    (home-page "https://bitbucket.org/snakemake/snakemake/wiki/Home")
    (synopsis "Python-based execution environment for make-like workflows")
    (description
      "Snakemake aims to reduce the complexity of creating workflows by
providing a clean and modern domain specific specification language (DSL) in
Python style, together with a fast and comfortable execution environment.")
    (license license:expat)))

(define-public python-pyqrcode
  (package
    (name "python-pyqrcode")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyQRCode" version))
       (sha256
        (base32
         "1m9ln8k9v7dfbh1i81225hx5mdsh8mpf9g7r4wpbfmiyfcs7dgzx"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/mnooner256/pyqrcode")
    (synopsis "QR code generator")
    (description
     "Pyqrcode is a QR code generator written purely in Python with
SVG, EPS, PNG and terminal output.")
    (license license:bsd-3)))

(define-public python-seaborn
  (package
    (name "python-seaborn")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "seaborn" version))
       (sha256
        (base32 "0pawrqc3mxpwd5g9pvi9gba02637bh5c8ldpp8izfwpfn52469zs"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Tests requires a running X11 server.
    (propagated-inputs
     `(("python-pandas" ,python-pandas)
       ("python-matplotlib" ,python-matplotlib)
       ("python-scipy" ,python-scipy)))
    (home-page "http://stanford.edu/~mwaskom/software/seaborn/")
    (synopsis "Statistical data visualization")
    (description
     "Seaborn is a library for making attractive and informative statistical
graphics in Python.  It is built on top of matplotlib and tightly integrated
with the PyData stack, including support for numpy and pandas data structures
and statistical routines from scipy and statsmodels.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-seaborn))))))

(define-public python2-seaborn
  (let ((base (package-with-python2 (strip-python2-variant python-seaborn))))
    (package
      (inherit base)
      (propagated-inputs `(("python2-pytz" ,python2-pytz)
                           ,@(package-propagated-inputs base))))))

(define-public python-mpmath
  (package
  (name "python-mpmath")
  (version "0.19")
  (source (origin
            (method url-fetch)
            (uri (string-append "http://mpmath.org/files/mpmath-"
                                version ".tar.gz"))
            (sha256
             (base32
              "08ijsr4ifrqv3cjc26mkw0dbvyygsa99in376hr4b96ddm1gdpb8"))))
  (build-system python-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       (replace 'check
         (lambda _
           (zero?
            (system* "python" "mpmath/tests/runtests.py" "-local")))))))
  (home-page "http://mpmath.org")
  (synopsis "Arbitrary-precision floating-point arithmetic in python")
  (description
    "@code{mpmath} can be used as an arbitrary-precision substitute for
Python's float/complex types and math/cmath modules, but also does much
more advanced mathematics.")
  (license license:bsd-3)))

(define-public python2-mpmath
  (package-with-python2 python-mpmath))

(define-public python-sympy
  (package
    (name "python-sympy")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/sympy/sympy/releases/download/sympy-"
             version "/sympy-" version ".tar.gz"))
       (sha256
        (base32 "190n29sppw7g8ihilc5451y7jlfcaw56crqiqbf1jff43dlmfnxc"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-mpmath" ,python-mpmath)))
    (home-page "http://www.sympy.org/")
    (synopsis "Python library for symbolic mathematics")
    (description
     "SymPy is a Python library for symbolic mathematics.  It aims to become a
full-featured computer algebra system (CAS) while keeping the code as simple
as possible in order to be comprehensible and easily extensible.")
    (license license:bsd-3)))

(define-public python2-sympy
  (package-with-python2 python-sympy))

(define-public python-q
  (package
    (name "python-q")
    (version "2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "q" version))
       (sha256
        (base32
         "1mgfazh8fkizh6walra2zv885f3lcgr3nb02v1frfm4p8ddcy3yy"))))
    (build-system python-build-system)
    (home-page "https://github.com/zestyping/q")
    (synopsis "Quick-and-dirty debugging output for tired programmers")
    (description
     "q is a Python module for \"print\" style of debugging Python code.  It
provides convenient short API for print out of values, tracebacks, and
falling into the Python interpreter.")
    (license license:asl2.0)))

(define-public python2-q
  (package-with-python2 python-q))

(define-public python2-xlib
  (package
    (name "python2-xlib")
    (version "0.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/python-xlib/python-xlib"
                                  "/" version "/"
                                  "python-xlib-" version ".tar.gz"))
              (sha256
               (base32
                "1sv0447j0rx8cgs3jhjl695p5pv13ihglcjlrrz1kq05lsvb0wa7"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2                         ;Python 2 only
       #:tests? #f))                              ;no tests
    (home-page "http://python-xlib.sourceforge.net/")
    (synopsis "Python X11 client library")
    (description
     "The Python X Library is intended to be a fully functional X client
library for Python programs.  It is useful to implement low-level X clients.
It is written entirely in Python.")
    (license license:gpl2+)))

(define-public python-singledispatch
  (package
    (name "python-singledispatch")
    (version "3.4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "singledispatch" version))
       (sha256
        (base32
         "171b7ip0hsq5qm83np40h3phlr36ym18w0lay0a8v08kvy3sy1jv"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-six" ,python-six))) ; required for conversion, not at run-time
    (home-page
     "http://docs.python.org/3/library/functools.html#functools.singledispatch")
    (synopsis "Backport of singledispatch feature from Python 3.4")
    (description
     "This library brings functools.singledispatch from Python 3.4 to Python
2.6-3.3.")
    (license license:expat)))

(define-public python2-singledispatch
  (package-with-python2 python-singledispatch))

;; the python- version can be removed with python-3.5
(define-public python-backports-abc
  (package
    (name "python-backports-abc")
      (version "0.5")
      (source
        (origin
          (method url-fetch)
          (uri (pypi-uri "backports_abc" version))
          (sha256
           (base32
            "1pkv8d1zxj5f9i227dxbjczncbv7ks7ywnjwyxfjagm02i2yafq3"))))
    (build-system python-build-system)
    (home-page "https://github.com/cython/backports_abc")
    (synopsis "Backport of additions to the 'collections.abc' module")
    (description
     "Python-backports-abc provides a backport of additions to the
'collections.abc' module in Python-3.5.")
    (license license:psfl)))

(define-public python2-backports-abc
  (package-with-python2 python-backports-abc))

(define-public python-backports-csv
  (package
    (name "python-backports-csv")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "backports.csv" version))
       (sha256
        (base32
         "1imzbrradkfn8s2m1qcimyn74dn1mz2p3j381jljn166rf2i6hlc"))))
    (build-system python-build-system)
    (home-page "https://github.com/ryanhiebert/backports.csv")
    (synopsis "Backport of Python 3's csv module for Python 2")
    (description
     "Provides a  backport of Python 3's @code{csv} module for parsing
comma separated values.  The API of the @code{csv} module in Python 2
is drastically different from the @code{csv} module in Python 3.
This is due, for the most part, to the difference between str in
Python 2 and Python 3.")
    (license license:psfl)))

(define-public python2-backports-csv
  (package-with-python2 python-backports-csv))

(define-public python2-backports-shutil-get-terminal-size
  (package
    (name "python2-backports-shutil-get-terminal-size")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "backports.shutil_get_terminal_size" version))
       (sha256
        (base32
         "107cmn7g3jnbkp826zlj8rrj19fam301qvaqf0f3905f5217lgki"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "py.test" "-v")))))))
    (native-inputs
     `(("python2-pytest" ,python2-pytest)))
    (home-page "https://github.com/chrippa/backports.shutil_get_terminal_size")
    (synopsis "Backport of Python 3.3's @code{shutil.get_terminal_size}")
    (description
     "This package provides a backport of the @code{get_terminal_size
function} from Python 3.3's @code{shutil}.
Unlike the original version it is written in pure Python rather than C,
so it might be a tiny bit slower.")
    (license license:expat)))

(define-public python-waf
  (package
    (name "python-waf")
    (version "1.9.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://waf.io/"
                                  "waf-" version ".tar.bz2"))
              (sha256
               (base32
                "0wl4cnmp06lfxqjxaan58bqxn27smhydz0sg5prrfbl3bsw4gv6q"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (zero? (system* "python" "waf-light" "configure" "build"))))
         (replace 'check
           (lambda _
             (zero? (system* "python" "waf" "--version"))))
         (replace 'install
           (lambda _
             (copy-file "waf" %output))))))
    (home-page "https://waf.io/")
    (synopsis "Python-based build system")
    (description
     "Waf is a Python-based framework for configuring, compiling and installing
applications.")
    (license license:bsd-3)))

(define-public python2-waf
  (package-with-python2 python-waf))

(define-public python-pyzmq
  (package
    (name "python-pyzmq")
    (version "15.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyzmq" version))
       (sha256
        (base32 "13fhwnlvsvxv72kfhqbpn6qi7msh8mc8377mpabv32skk2cjfnxx"))))
    (build-system python-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--zmq=" (assoc-ref %build-inputs "zeromq")))
       ;; FIXME: You must build pyzmq with 'python setup.py build_ext
       ;; --inplace' for 'python setup.py test' to work.
       #:tests? #f))
    (inputs
     `(("zeromq" ,zeromq)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/zeromq/pyzmq")
    (synopsis "Python bindings for 0MQ")
    (description
     "PyZMQ is the official Python binding for the ZeroMQ messaging library.")
    (license license:bsd-4)))

(define-public python2-pyzmq
  (package-with-python2 python-pyzmq))

(define-public python-pep8
  (package
    (name "python-pep8")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pep8" version))
        (sha256
          (base32
            "002rkl4lsn6x2mxmf8ar00l0m8i3mzrc6pnzz77blyksmpsxa4x1"))))
    (build-system python-build-system)
    (home-page "http://pep8.readthedocs.org/")
    (synopsis "Python style guide checker")
    (description
     "This tools checks Python code against some of the style conventions in
PEP 8.")
    (license license:expat)))

(define-public python2-pep8
  (package-with-python2 python-pep8))

(define-public python-pyflakes
  (package
    (name "python-pyflakes")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyflakes" version))
        (sha256
          (base32
            "0qs2sgqszq7wcplis8509wk2ygqcrwzbs1ghfj3svvivq2j377pk"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/pyflakes/pyflakes")
    (synopsis "Passive checker of Python programs")
    (description
      "Pyflakes statically checks Python source code for common errors.")
    (license license:expat)))

(define-public python2-pyflakes
  (package-with-python2 python-pyflakes))

(define-public python-mccabe
  (package
    (name "python-mccabe")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mccabe" version))
        (sha256
          (base32
            "0yr08a36h8lqlif10l4xcikbbig7q8f41gqywir7rrvnv3mi4aws"))))
    (build-system python-build-system)
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/flintwork/mccabe")
    (synopsis "McCabe checker, plugin for flake8")
    (description
      "This package provides a Flake8 plug-in to compute the McCabe cyclomatic
complexity of Python source code.")
    (license license:expat)))

(define-public python2-mccabe
  (package-with-python2 python-mccabe))

(define-public python-mccabe-0.2.1
  (package (inherit python-mccabe)
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mccabe" version))
        (sha256
          (base32
            "0fi4a81kr5bcv5p4xgibqr595hyj5dafkqgsmfk96mfy8w71fajs"))))))

(define-public python2-mccabe-0.2.1
  (package-with-python2 python-mccabe-0.2.1))

;; Flake8 2.4.1 requires an older version of pep8.
;; This should be removed ASAP.
(define-public python-pep8-1.5.7
  (package (inherit python-pep8)
    (version "1.5.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/p/pep8/pep8-"
               version
               ".tar.gz"))
        (sha256
          (base32
           "12b9bbdbwnspxgak14xg58c130x2n0blxzlms5jn2dszn8qj3d0m"))))
    (arguments
     ;; XXX Tests not compatible with Python 3.5.
     '(#:tests? #f))))

(define-public python2-pep8-1.5.7
  (package-with-python2 python-pep8-1.5.7))

;; Flake8 2.4.1 requires an older version of pyflakes.
;; This should be removed ASAP.
(define-public python-pyflakes-0.8.1
  (package (inherit python-pyflakes)
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/p/pyflakes/pyflakes-"
               version
               ".tar.gz"))
        (sha256
          (base32
           "0sbpq6pqm1i9wqi41mlfrsc5rk92jv4mskvlyxmnhlbdnc80ma1z"))))
    (arguments
     ;; XXX Tests not compatible with Python 3.5.
     '(#:tests? #f))))

(define-public python2-pyflakes-0.8.1
  (package-with-python2 python-pyflakes-0.8.1))

(define-public python-flake8
  (package
    (name "python-flake8")
    (version "2.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flake8" version))
        (sha256
          (base32
            "0bs9cz4fr99r2rwig1b8jwaadl1nan7kgpdzqwj0bwbckwbmh7nc"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;; Remove pre-compiled .pyc files from source.
            (for-each delete-file-recursively
                      (find-files "." "__pycache__" #:directories? #t))
            (for-each delete-file (find-files "." "\\.pyc$"))
            #t))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pep8" ,python-pep8)
        ("python-pyflakes" ,python-pyflakes)
        ("python-mccabe" ,python-mccabe)))
    (native-inputs
      `(("python-mock" ,python-mock) ; TODO: only required for < 3.3
        ("python-nose" ,python-nose)))
    (home-page "https://gitlab.com/pycqa/flake8")
    (synopsis
      "The modular source code checker: pep8, pyflakes and co")
    (description
      "Flake8 is a wrapper around PyFlakes, pep8 and python-mccabe.")
    (license license:expat)))

(define-public python2-flake8
  (package-with-python2 python-flake8))

(define-public python-flake8-polyfill
  (package
    (name "python-flake8-polyfill")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flake8-polyfill" version))
       (sha256
        (base32
         "02gn2wxvh9vnf7m7dld7ca4l60mg5c370hv3swwppkngwaqmcw67"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append (getcwd) "/build/lib:"
                                    (getenv "PYTHONPATH")))
             (zero? (system* "py.test" "-v")))))))
    (native-inputs
     `(("python-flake8" ,python-flake8)
       ("python-mock" ,python-mock)
       ("python-pycodestyle" ,python-pycodestyle)
       ("python-pytest" ,python-pytest)))
    (home-page "https://gitlab.com/pycqa/flake8-polyfill")
    (synopsis "Polyfill package for Flake8 plugins")
    (description
     "This package that provides some compatibility helpers for Flake8
plugins that intend to support Flake8 2.x and 3.x simultaneously.")
    (license license:expat)))

(define-public python2-flake8-polyfill
  (package-with-python2 python-flake8-polyfill))

(define-public python-mistune
  (package
    (name "python-mistune")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mistune" version))
       (sha256
        (base32
         "04xpk1zvslhq3xpnf01g3ag0dy9wfv4z28p093r8k49vvxlyil11"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-cython" ,python-cython)))
    (home-page "https://github.com/lepture/mistune")
    (synopsis "Markdown parser in pure Python")
    (description "This package provides a fast markdown parser in pure
Python.")
    (license license:bsd-3)))

(define-public python2-mistune
  (package-with-python2 python-mistune))

(define-public python-markdown
  (package
    (name "python-markdown")
    (version "2.6.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Markdown" version))
       (sha256
        (base32
         "0cqfhr1km2s5d8jm6hbwgkrrj9hvkjf2gab3s2axlrw1clgaij0a"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
                  (lambda _
                    (zero? (system* "python" "run-tests.py")))))))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-pyyaml" ,python-pyyaml)))
    (home-page "https://pythonhosted.org/Markdown/")
    (synopsis "Python implementation of Markdown")
    (description
     "This package provides a Python implementation of John Gruber's
Markdown.  The library features international input, various Markdown
extensions, and several HTML output formats.  A command line wrapper
markdown_py is also provided to convert Markdown files to HTML.")
    (license license:bsd-3)))

(define-public python2-markdown
  (package-with-python2 python-markdown))

(define-public python-ptyprocess
  (package
    (name "python-ptyprocess")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/ptyprocess/ptyprocess-"
             version ".tar.gz"))
       (sha256
        (base32
         "19l1xrjn4l9gjz01s3vg92gn2dd9d8mw1v86ppkzlnr9m5iwwc05"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
                  (lambda _
                    (zero? (system* "nosetests")))))))
    (home-page "https://github.com/pexpect/ptyprocess")
    (synopsis "Run a subprocess in a pseudo terminal")
    (description
     "This package provides a Python library used to launch a subprocess in a
pseudo terminal (pty), and interact with both the process and its pty.")
    (license license:isc)))

(define-public python2-ptyprocess
  (package-with-python2 python-ptyprocess))

(define-public python-cram
  (package
    (name "python-cram")
    (version "0.7")
    (home-page "https://bitheap.org/cram/")
    (source (origin
              (method url-fetch)
              (uri (list (string-append home-page "cram-"
                                        version ".tar.gz")
                         (pypi-uri "cram" version)))
              (sha256
               (base32
                "0bvz6fwdi55rkrz3f50zsy35gvvwhlppki2yml5bj5ffy9d499vx"))))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* (find-files "cram" ".*\\.py$")
               ;; Replace default shell path.
               (("/bin/sh") (which "sh")))
             (substitute* (find-files "tests" ".*\\.t$")
               (("md5") "md5sum")
               (("/bin/bash") (which "bash"))
               (("/bin/sh") (which "sh")))
             (substitute* "cram/_test.py"
               ;; This hack works around a bug triggered by substituting
               ;; the /bin/sh paths. "tests/usage.t" compares the output of
               ;; "cram -h", which breaks the output at 80 characters. This
               ;; causes the line showing the default shell to break into two
               ;; lines, but the test expects a single line...
               (("env\\['COLUMNS'\\] = '80'")
                "env['COLUMNS'] = '160'"))
             #t))
         (delete 'check)
         (add-after 'install 'check
           ;; The test phase uses the built library and executable.
           ;; It's easier to run it after install since the build
           ;; directory contains version-specific PATH.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (setenv "PATH" (string-append (getenv "PATH") ":"
                                           (assoc-ref outputs "out") "/bin"))
             (zero? (system* "make" "test")))))))
    (build-system python-build-system)
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("which" ,which)))
    (synopsis "Simple testing framework for command line applications")
    (description
     "Cram is a functional testing framework for command line applications.
Cram tests look like snippets of interactive shell sessions.  Cram runs each
command and compares the command output in the test with the command’s actual
output.")
    (license license:gpl2+)))

(define-public python2-cram
  (package-with-python2 python-cram))

(define-public python-straight-plugin
  (package
    (name "python-straight-plugin")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "straight.plugin" version))
       (sha256
        (base32
         "069pjll4383p4kkgvcc40hgyvf79j2wdbpgwz77yigzxksh1gj62"))))
    (build-system python-build-system)
    (home-page "https://github.com/ironfroggy/straight.plugin")
    (synopsis "Simple namespaced plugin facility")
    (description "Straight Plugin provides a type of plugin you can create from
almost any existing Python modules, and an easy way for outside developers to
add functionality and customization to your projects with their own plugins.")
    (license license:expat)))

(define-public python2-straight-plugin
  (package-with-python2 python-straight-plugin))

(define-public python-fonttools
  (package
    (name "python-fonttools")
    (version "3.15.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "fonttools" version ".zip"))
              (sha256
               (base32
                "1hhj97izwliy0vybmza72d90l5d4mcn50y8akq7kyccfl82vdx4d"))))
    (build-system python-build-system)
    (arguments
     '(#:test-target "check"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-setuppy
           ;; Remove the undocumented "extra_path" argument, which adds an
           ;; intervening directories between site-packages and the package
           ;; directory.
           (lambda _
             (substitute* "setup.py"
               (("^[ \t]*extra_path *= *'FontTools',") ""))
             #t)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://github.com/behdad/fonttools")
    (synopsis "Tools to manipulate font files")
    (description
     "FontTools/TTX is a library to manipulate font files from Python.  It
supports reading and writing of TrueType/OpenType fonts, reading and writing
of AFM files, reading (and partially writing) of PS Type 1 fonts.  The package
also contains a tool called “TTX” which converts TrueType/OpenType fonts to and
from an XML-based format.")
    (license (license:non-copyleft
              "file://LICENSE.txt"
              "See LICENSE.txt in the distribution."))))

(define-public python2-fonttools
  (package-with-python2 python-fonttools))

(define-public python-ly
  (package
    (name "python-ly")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/57/4f/"
                           "889579244947368f28eda66b782331b1e75f83fd72e63f9ece93cd7a18f9"
                           "/python-ly-" version ".tar.gz"))
       (sha256
        (base32
         "0g6n288l83sfwavxh1aryi0aqvsr3sp7v6f903mckwqa4scpky62"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Some tests need network access.
     '(#:tests? #f))
    (synopsis "Tool and library for manipulating LilyPond files")
    (description "This package provides a Python library to parse, manipulate
or create documents in LilyPond format.  A command line program ly is also
provided that can be used to do various manipulations with LilyPond files.")
    (home-page "https://pypi.python.org/pypi/python-ly")
    (license license:gpl2+)))

(define-public python-appdirs
  (package
    (name "python-appdirs")
    (version "1.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "appdirs" version))
        (sha256
          (base32
            "14id6wxi12lgyw0mg3bcfnf888ad07jz9yj46gfzhn186z8rcn4y"))))
    (build-system python-build-system)
    (home-page "https://github.com/ActiveState/appdirs")
    (synopsis
      "Determine platform-specific dirs, e.g. a \"user data dir\"")
    (description
      "This module provides a portable way of finding out where user data
should be stored on various operating systems.")
    (license license:expat)))

(define-public python2-appdirs
  (package-with-python2 python-appdirs))

(define-public python-llfuse
  (package
    (name "python-llfuse")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://bitbucket.org/nikratio/python-llfuse/downloads/"
                    "llfuse-" version ".tar.bz2"))
              (sha256
               (base32
                "11hms1x68bf1bqbqy7w3wpffqsd3jkgricmzrc1hrnwkswfzzlr4"))))
    (build-system python-build-system)
    (inputs
     `(("fuse" ,fuse)
       ("attr" ,attr)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Python bindings for FUSE")
    (description
     "Python-LLFUSE is a set of Python bindings for the low level FUSE API.")
    (home-page "https://bitbucket.org/nikratio/python-llfuse/")
    (license license:lgpl2.0+)
    (properties `((python2-variant . ,(delay python2-llfuse))))))

(define-public python2-llfuse
  (package (inherit (package-with-python2
                 (strip-python2-variant python-llfuse)))
    (propagated-inputs `(("python2-contextlib2" ,python2-contextlib2)))))

;; For attic-0.16
(define-public python-llfuse-0.41
  (package (inherit python-llfuse)
    (version "0.41.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://bitbucket.org/nikratio/python-llfuse/downloads/"
                    "llfuse-" version ".tar.bz2"))
              (sha256
               (base32
                "1imlqw9b73086y97izr036f58pgc5akv4ihc2rrf8j5h75jbrlaa"))))
    ;; Python-LLFUSE < 0.42 includes underscore.js, which is MIT (expat)
    ;; licensed.  The rest of the package is licensed under LGPL2.0 or later.
    (license (list license:expat license:lgpl2.0+))))

(define-public python-msgpack
  (package
    (name "python-msgpack")
    (version "0.4.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "msgpack-python" version))
              (sha256
               (base32
                "11pqk5braa6wndpnr1dhg64js82vjgxnm0lzy73rwl831zgijaqs"))))
    (build-system python-build-system)
    (synopsis "MessagePack (de)serializer")
    (description "MessagePack is a fast, compact binary serialization format,
suitable for similar data to JSON.  This package provides CPython bindings for
reading and writing MessagePack data.")
    (home-page "https://pypi.python.org/pypi/msgpack-python/")
    (license license:asl2.0)))

(define-public python2-msgpack
  (package-with-python2 python-msgpack))

(define-public python-netaddr
  (package
    (name "python-netaddr")
    (version "0.7.19")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "netaddr" version))
       (sha256
         (base32
          "1zdfadvpq4lmcqzr383gywxn4xyn355kj1n3lk9q2l03vmyfrbiq"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ;; No tests.
    (home-page "https://github.com/drkjam/netaddr/")
    (synopsis "Pythonic manipulation of  network addresses")
    (description
      "A Python library for representing and manipulating IPv4, IPv6, CIDR, EUI
and MAC network addresses.")
    (license license:bsd-3)))

(define-public python2-netaddr
  (package-with-python2 python-netaddr))

(define-public python-wrapt
  (package
    (name "python-wrapt")
    (version "1.10.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "wrapt" version))
        (sha256
          (base32
            "0wrcm1mydvfivbkzz0h81ygzdchnscshi6xvy5n3r21r9s0px8af"))))
    (build-system python-build-system)
    (arguments
     ;; Tests are not included in the tarball, they are only available in the
     ;; git repository.
     `(#:tests? #f))
    (home-page "https://github.com/GrahamDumpleton/wrapt")
    (synopsis "Module for decorators, wrappers and monkey patching")
    (description
      "The aim of the wrapt module is to provide a transparent object proxy for
  Python, which can be used as the basis for the construction of function
  wrappers and decorator functions.")
    (license license:bsd-2)))

(define-public python2-wrapt
  (package-with-python2 python-wrapt))

(define-public python-xlrd
  (package
    (name "python-xlrd")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "xlrd" version))
              (sha256
               (base32
                "0s8hjiz01vbhy85xalrz0qlsmd9ypf36zjqrf97hh984spapvy0g"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Current test in setup.py does not work as of 1.0.0, so use nose to
         ;; run tests instead for now.
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (native-inputs `(("python-nose"       ,python-nose)))
    (home-page "http://www.python-excel.org/")
    (synopsis "Library for extracting data from Excel files")
    (description "This packages provides a library to extract data from
spreadsheets using Microsoft Excel proprietary file formats @samp{.xls} and
@samp{.xlsx} (versions 2.0 onwards).  It has support for Excel dates and is
Unicode-aware.  It is not intended as an end-user tool.")
    (license license:bsd-3)))

(define-public python2-xlrd
  (package-with-python2 python-xlrd))

(define-public python-prettytable
  (package
    (name "python-prettytable")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/P/PrettyTable/"
             "prettytable-" version ".tar.bz2"))
       (sha256
        (base32
         "0diwsicwmiq2cpzpxri7cyl5fmsvicafw6nfqf6p6p322dji2g45"))))
    (build-system python-build-system)
    (home-page "http://code.google.com/p/prettytable/")
    (synopsis "Display tabular data in an ASCII table format")
    (description
      "A library designed to represent tabular data in visually appealing ASCII
tables.  PrettyTable allows for selection of which columns are to be printed,
independent alignment of columns (left or right justified or centred) and
printing of sub-tables by specifying a row range.")
    (license license:bsd-3)))

(define-public python2-prettytable
  (package-with-python2 python-prettytable))

(define-public python-tables
  (package
    (name "python-tables")
    (version "3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tables" version))
       (sha256
        (base32
         "117s6w7s3yxafpmf3zz3svana7xfrsviw01va1xp7h8ylx8v6r1m"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove pre-compiled .pyc files from source.
           (for-each delete-file-recursively
                     (find-files "." "__pycache__" #:directories? #t))
           (for-each delete-file (find-files "." "\\.pyc$"))
           #t))))
    (build-system python-build-system)
    (arguments
     `(;; FIXME: python-build-system does not pass configure-flags to "build"
       ;; or "check", so we must override the build and check phases.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-gcc
           (lambda _
             (substitute* "setup.py"
               (("compiler = new_compiler\\(\\)" line)
                (string-append line
                               "\ncompiler.set_executables(compiler='gcc',"
                               "compiler_so='gcc',"
                               "linker_exe='gcc',"
                               "linker_so='gcc -shared')")))
             #t))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "python" "setup.py" "build"
                             (string-append "--hdf5="
                                            (assoc-ref inputs "hdf5"))))))
         (replace 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "python" "setup.py" "check"
                             (string-append "--hdf5="
                                            (assoc-ref inputs "hdf5")))))))))
    (propagated-inputs
     `(("python-numexpr" ,python-numexpr)
       ("python-numpy" ,python-numpy)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("hdf5" ,hdf5)
       ("bzip2" ,bzip2)
       ("zlib" ,zlib)))
    (home-page "http://www.pytables.org/")
    (synopsis "Hierarchical datasets for Python")
    (description "PyTables is a package for managing hierarchical datasets and
designed to efficiently cope with extremely large amounts of data.")
    (license license:bsd-3)))

(define-public python2-tables
  (package-with-python2 python-tables))

(define-public python-pyasn1
  (package
    (name "python-pyasn1")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyasn1" version))
       (sha256
        (base32
         "1b86yx23c1x74clai05a5ma8c8nfmhlx3j1mxq0ff657i2ylx33k"))))
    (build-system python-build-system)
    (home-page "http://pyasn1.sourceforge.net/")
    (synopsis "ASN.1 types and codecs")
    (description
     "This is an implementation of ASN.1 types and codecs in Python.  It is
suitable for a wide range of protocols based on the ASN.1 specification.")
    (license license:bsd-2)))

(define-public python2-pyasn1
  (package-with-python2 python-pyasn1))

(define-public python-pyasn1-modules
  (package
    (name "python-pyasn1-modules")
    (version "0.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyasn1-modules" version))
        (sha256
         (base32
          "0drqgw81xd3fxdlg89kgd79zzrabvfncvkbybi2wr6w2y4s1jmhh"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pyasn1" ,python-pyasn1)))
    (home-page "https://sourceforge.net/projects/pyasn1/")
    (synopsis "ASN.1 codec implementations")
    (description
     "Pyasn1-modules is a collection of Python modules providing ASN.1 types and
implementations of ASN.1-based codecs and protocols.")
    (license license:bsd-3)))

(define-public python2-pyasn1-modules
  (package-with-python2 python-pyasn1-modules))

(define-public python-ipaddress
  (package
    (name "python-ipaddress")
    (version "1.0.18")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ipaddress" version))
              (sha256
               (base32
                "1q8klj9d84cmxgz66073x1j35cplr3r77vx1znhxiwl5w74391ax"))))
    (build-system python-build-system)
    (home-page "https://github.com/phihag/ipaddress")
    (synopsis "IP address manipulation library")
    (description
      "This package provides a fast, lightweight IPv4/IPv6 manipulation library
 in Python.  This library is used to create, poke at, and manipulate IPv4 and
 IPv6 addresses and networks.  This is a port of the Python 3.3 ipaddress
 module to older versions of Python.")
    (license license:psfl)))

(define-public python2-ipaddress
  (package-with-python2 python-ipaddress))

(define-public python2-ipaddr
  (package
    (name "python2-ipaddr")
    (version "2.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipaddr" version))
       (sha256
        (base32 "1dwq3ngsapjc93fw61rp17fvzggmab5x1drjzvd4y4q0i255nm8v"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2                         ;version 2 only
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* _
             (zero? (system* "python" "ipaddr_test.py")))))))
    (home-page "https://github.com/google/ipaddr-py")
    (synopsis "IP address manipulation library")
    (description
     "Ipaddr is a Python@tie{}2 library for creating and manupilating IPv4 and
IPv6 addresses and networks.

For new implementations you may prefer to use the standard module
@code{ipaddress}, which was introduced in Python 3.3 and backported to older
versions of Python.")
    (license license:asl2.0)))

(define-public python-idna
  (package
    (name "python-idna")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "idna" version))
       (sha256
        (base32
         "1ara12a7k2zc69msa0arrvw00gn61a6i6by01xb3lkkc0h4cxd9w"))))
    (build-system python-build-system)
    (home-page "https://github.com/kjd/idna")
    (synopsis "Internationalized domain names in applications")
    (description
     "This is a library to support the Internationalised Domain Names in
Applications (IDNA) protocol as specified in RFC 5891.  This version of the
protocol is often referred to as “IDNA2008” and can produce different results
from the earlier standard from 2003.  The library is also intended to act as a
suitable drop-in replacement for the “encodings.idna” module that comes with
the Python standard library but currently only supports the older 2003
specification.")
    (license license:bsd-4)))

(define-public python2-idna
  (package-with-python2 python-idna))

(define-public python-pretend
  (package
    (name "python-pretend")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/p/"
                           "pretend/pretend-" version ".tar.gz"))
       (sha256
        (base32
         "0r5r7ygz9m6d2bklflbl84cqhjkc2q12xgis8268ygjh30g2q3wk"))))
    (build-system python-build-system)
    (home-page "https://github.com/alex/pretend")
    (synopsis "Library for stubbing in Python")
    (description
     "Pretend is a library to make stubbing with Python easier.  Stubbing is a
technique for writing tests.  You may hear the term mixed up with mocks,
fakes, or doubles.  Basically, a stub is an object that returns pre-canned
responses, rather than doing any computation.")
    (license license:bsd-3)))

(define-public python2-pretend
  (package-with-python2 python-pretend))

(define-public python-pip
  (package
    (name "python-pip")
    (version "9.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pip" version))
       (sha256
        (base32
         "03clr9c1dih5n9c00c592zzvf6r1ffimywkaq9agcqdllzhl7wh9"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))          ; there are no tests in the pypi archive.
    (home-page "https://pip.pypa.io/")
    (synopsis "Package manager for Python software")
    (description
     "Pip is a package manager for Python software, that finds packages on the
Python Package Index (PyPI).")
    (license license:expat)))

(define-public python2-pip
  (package-with-python2 python-pip))

(define-public python-tlsh
  (package
    (name "python-tlsh")
    (version "3.4.4")
    (home-page "https://github.com/trendmicro/tlsh")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/trendmicro/tlsh/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "00bhzjqrlh7v538kbkbn8lgx976j1138al3sdhklaizqjvpwyk4r"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (arguments
     '(#:out-of-source? #f
       #:phases (modify-phases %standard-phases
                  (replace
                   'install
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; Build and install the Python bindings.  The underlying
                     ;; C++ library is apparently not meant to be installed.
                     (let ((out (assoc-ref outputs "out")))
                       (with-directory-excursion "py_ext"
                         (and (system* "python" "setup.py" "build")
                              (system* "python" "setup.py" "install"
                                       (string-append "--prefix=" out))))))))))
    (inputs `(("python" ,python-wrapper)))        ;for the bindings
    (synopsis "Fuzzy matching library for Python")
    (description
     "Trend Micro Locality Sensitive Hash (TLSH) is a fuzzy matching library.
Given a byte stream with a minimum length of 256 bytes, TLSH generates a hash
value which can be used for similarity comparisons.  Similar objects have
similar hash values, which allows for the detection of similar objects by
comparing their hash values.  The byte stream should have a sufficient amount
of complexity; for example, a byte stream of identical bytes will not generate
a hash value.")
    (license license:asl2.0)))

(define-public python2-tlsh
  (package
    (inherit python-tlsh)
    (name "python2-tlsh")
    (inputs `(("python" ,python-2)))))

(define-public python-termcolor
  (package
    (name "python-termcolor")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "termcolor" version))
       (sha256
        (base32
         "0fv1vq14rpqwgazxg4981904lfyp84mnammw7y046491cv76jv8x"))))
    (build-system python-build-system)
    (arguments
     ;; There are no tests.
     `(#:tests? #f))
    (home-page "https://pypi.python.org/pypi/termcolor")
    (synopsis "ANSII Color formatting for terminal output")
    (description
     "This package provides ANSII Color formatting for output in terminals.")
    (license license:expat)))

(define-public python2-termcolor
  (package-with-python2 python-termcolor))

(define-public python-libarchive-c
  (package
    (name "python-libarchive-c")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "libarchive-c" version))
              (sha256
               (base32
                "0z4r7v3dhd6b3120mav05ff08srih176r2rg5k8kn7mjd9pslm2x"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before
                   'build 'reference-libarchive
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; Retain the absolute file name of libarchive.so.
                     (let ((libarchive (assoc-ref inputs "libarchive")))
                       (substitute* "libarchive/ffi.py"
                         (("find_library\\('archive'\\)")
                          (string-append "'" libarchive
                                         "/lib/libarchive.so'")))))))))
    (inputs
     `(("libarchive" ,libarchive)))
    (home-page "https://github.com/Changaco/python-libarchive-c")
    (synopsis "Python interface to libarchive")
    (description
     "This package provides Python bindings to libarchive, a C library to
access possibly compressed archives in many different formats.  It uses
Python's @code{ctypes} foreign function interface (FFI).")
    (license license:lgpl2.0+)))

(define-public python2-libarchive-c
  (package-with-python2 python-libarchive-c))

(define-public python-file
  (package
    (inherit file)
    (name "python-file")
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                                ;no tests
       #:configure-flags '("--single-version-externally-managed" "--root=/")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'change-directory
                    (lambda _
                      (chdir "python")
                      #t))
                  (add-before 'build 'set-library-file-name
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((file (assoc-ref inputs "file")))
                        (substitute* "magic.py"
                          (("find_library\\('magic'\\)")
                           (string-append "'" file "/lib/libmagic.so'")))
                        #t))))))
    (inputs `(("file" ,file)))
    (self-native-input? #f)
    (synopsis "Python bindings to the libmagic file type guesser.  Note that
this module and the python-magic module both provide a \"magic.py\" file;
these two modules, which are different and were developed separately, both
serve the same purpose: provide Python bindings for libmagic.")))

(define-public python2-file
  (package-with-python2 python-file))

(define-public python-debian
  (package
    (name "python-debian")
    (version "0.1.28")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32
         "0i15f0xzx679sd0ldq2sls9pnnps9fv6vhqvnv9dzf4qhma42i0y"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "http://packages.debian.org/sid/python-debian")
    (synopsis "Debian package related modules")
    (description
     ;; XXX: Use @enumerate instead of @itemize to work around
     ;; <http://bugs.gnu.org/21772>.
     "This package provides Python modules that abstract many formats of
Debian-related files, such as:

@enumerate
@item Debtags information;
@item @file{debian/changelog} files;
@item packages files, pdiffs;
@item control files of single or multiple RFC822-style paragraphs---e.g.
   @file{debian/control}, @file{.changes}, @file{.dsc};
@item Raw @file{.deb} and @file{.ar} files, with (read-only) access to
   contained files and meta-information.
@end enumerate\n")

    ;; Modules are either GPLv2+ or GPLv3+.
    (license license:gpl3+)))

(define-public python2-debian
  (package-with-python2 python-debian))

(define-public python-nbformat
  (package
    (name "python-nbformat")
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbformat" version))
       (sha256
        (base32
         "12s7j4qja8b5bs1kyw5dzmrqbjxxj8wk52cyasbiqbv7fblcrssz"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; no test target
    (propagated-inputs
     `(("python-ipython-genutils" ,python-ipython-genutils)
       ("python-jsonschema" ,python-jsonschema)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-traitlets" ,python-traitlets)))
    (home-page "http://jupyter.org")
    (synopsis "Jupyter Notebook format")
    (description "This package provides the reference implementation of the
Jupyter Notebook format and Python APIs for working with notebooks.")
    (license license:bsd-3)))

(define-public python2-nbformat
  (package-with-python2 python-nbformat))

(define-public python-bleach
  (package
    (name "python-bleach")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bleach" version))
       (sha256
        (base32
         "0jvg3jxrvnx7xmm9gj262v60ib452xlnwlb0navyp7jsvcd0d4qj"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-html5lib" ,python-html5lib-0.9)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/jsocol/bleach")
    (synopsis "Whitelist-based HTML-sanitizing tool")
    (description "Bleach is an easy whitelist-based HTML-sanitizing tool.")
    (license license:asl2.0)))

(define-public python2-bleach
  (package-with-python2 python-bleach))

(define-public python-entrypoints
  (package
    (name "python-entrypoints")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/takluyver/entrypoints/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0azqlkh3j0za080lsf5crnhaxx3c93k9dpv5ihkhf5cppgw5sjz5"))))
    (build-system python-build-system)
    ;; The package does not come with a setup.py file, so we have to generate
    ;; one ourselves.
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-setup.py
           (lambda _
             (call-with-output-file "setup.py"
               (lambda (port)
                 (format port "\
from setuptools import setup
setup(name='entrypoints', version='~a', py_modules=['entrypoints'])
" ,version))))))))
    (home-page "https://github.com/takluyver/entrypoints")
    (synopsis "Discover and load entry points from installed Python packages")
    (description "Entry points are a way for Python packages to advertise
objects with some common interface.  The most common examples are
@code{console_scripts} entry points, which define shell commands by
identifying a Python function to run.  The @code{entrypoints} module contains
functions to find and load entry points.")
    (license license:expat)))

(define-public python2-entrypoints
  (package-with-python2 python-entrypoints))

(define-public python-nbconvert
  (package
    (name "python-nbconvert")
    (version "5.0.0b1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbconvert" version))
       (sha256
        (base32
         "0brclbb18l4nmd5qy3dl9wn05rjdh1fz4rmzdlfqacj12rcdvdgp"))))
    (build-system python-build-system)
    (arguments
     `(;; The "bdist_egg" target is disabled by default, causing the installation
       ;; to fail.
       #:configure-flags (list "bdist_egg")
       ;; FIXME: 5 failures, 40 errors.
       #:tests? #f))
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (replace 'check
       ;;     (lambda _
       ;;       (zero? (system* "py.test" "-v")))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-bleach" ,python-bleach)
       ("python-entrypoints" ,python-entrypoints)
       ("python-jinja2" ,python-jinja2)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-mistune" ,python-mistune)
       ("python-nbformat" ,python-nbformat)
       ("python-pygments" ,python-pygments)
       ("python-traitlets" ,python-traitlets)))
    (home-page "http://jupyter.org")
    (synopsis "Converting Jupyter Notebooks")
    (description "The @code{nbconvert} tool, @{jupyter nbconvert}, converts
notebooks to various other formats via Jinja templates.  It allows you to
convert an @code{.ipynb} notebook file into various static formats including:

@enumerate
@item HTML
@item LaTeX
@item PDF
@item Reveal JS
@item Markdown (md)
@item ReStructured Text (rst)
@item executable script
@end enumerate\n")
    (license license:bsd-3)))

(define-public python2-nbconvert
  (package-with-python2 python-nbconvert))

(define-public python-notebook
  (package
    (name "python-notebook")
    (version "4.2.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "notebook" version))
              (sha256
               (base32
                "0laq5c2f21frq6xcdckgq7raqhznbjb0qs0357g612z87wyn1a9r"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; HOME must be set for tests
             (setenv "HOME" "/tmp")
             (zero? (system* "nosetests")))))))
    (propagated-inputs
     `(("python-jupyter-core" ,python-jupyter-core)
       ("python-nbformat" ,python-nbformat)
       ("python-nbconvert" ,python-nbconvert)
       ("python-ipython" ,python-ipython)))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-sphinx" ,python-sphinx)
       ("python-requests" ,python-requests)))
    (home-page "http://jupyter.org/")
    (synopsis "Web-based notebook environment for interactive computing")
    (description
     "The Jupyter HTML notebook is a web-based notebook environment for
interactive computing.")
    (properties `((python2-variant . ,(delay python2-notebook))))
    (license license:bsd-3)))

(define-public python2-notebook
  (let ((base (package-with-python2
                (strip-python2-variant python-notebook))))
    (package (inherit base)
      (native-inputs
       `(("python2-mock" ,python2-mock)
         ,@(package-native-inputs base)))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'check 'disable-test-case
              ;; The test requires network access to localhost. Curiously it
              ;; fails with Python 2 only. Simply make the test-case return
              ;; immediately.
              (lambda _
                (substitute*
                    "notebook/services/nbconvert/tests/test_nbconvert_api.py"
                  (("formats = self.nbconvert_api") "return #")))))))))))

(define-public python-widgetsnbextension
  (package
    (name "python-widgetsnbextension")
    (version "1.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "widgetsnbextension" version))
       (sha256
        (base32
         "0lff2mrwrgsa1mxmwx3phl9xvy0jqfpg6khbmxy53jbq56rwy666"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-notebook" ,python-notebook)))
    (native-inputs
     `(("python-certifi" ,python-certifi)
       ("python-nose" ,python-nose)))
    (home-page "http://ipython.org")
    (synopsis "IPython HTML widgets for Jupyter")
    (description "This package provides interactive HTML widgets for Jupyter
notebooks.")
    (license license:bsd-3)))

(define-public python2-widgetsnbextension
  (package-with-python2 python-widgetsnbextension))

(define-public python-ipywidgets
  (package
    (name "python-ipywidgets")
    (version "5.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipywidgets" version))
       (sha256
        (base32
         "1lk0qrr5l9a0z7qkkn30hv5832whxwxymf1l576fmmad0n7hkxms"))))
    (build-system python-build-system)
    ;; FIXME: it's not clear how to run the tests.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipython" ,python-ipython)
       ("python-traitlets" ,python-traitlets)
       ("python-widgetsnbextension" ,python-widgetsnbextension)))
    (home-page "http://ipython.org")
    (synopsis "IPython HTML widgets for Jupyter")
    (description "Ipywidgets are interactive HTML widgets for Jupyter
notebooks and the IPython kernel.  Notebooks come alive when interactive
widgets are used.  Users gain control of their data and can visualize changes
in the data.")
    (license license:bsd-3)))

(define-public python2-ipywidgets
  (package-with-python2 python-ipywidgets))

(define-public python-jupyter-console
  (package
    (name "python-jupyter-console")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_console" version))
       (sha256
        (base32
         "04acmkwsi99rcg3vb54c6n492zv35s92h2ahabc0w6wj976cipvx"))))
    (build-system python-build-system)
    ;; FIXME: it's not clear how to run the tests.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipython" ,python-ipython)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-prompt-toolkit" ,python-prompt-toolkit)
       ("python-pygments" ,python-pygments)))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter terminal console")
    (description "This package provides a terminal-based console frontend for
Jupyter kernels.  It also allows for console-based interaction with non-Python
Jupyter kernels such as IJulia and IRKernel.")
    (license license:bsd-3)))

(define-public python2-jupyter-console
  (package-with-python2 python-jupyter-console))

;; The python-ipython and python-jupyter-console require each other. To get
;; the functionality in both packages working, strip down the
;; python-jupyter-console package when using it as an input to python-ipython.
(define python-jupyter-console-minimal
  (package
    (inherit python-jupyter-console)
    (arguments
     (substitute-keyword-arguments
         (package-arguments python-jupyter-console)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'install 'delete-bin
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Delete the bin files, to avoid conflicts in profiles
               ;; where python-ipython and python-jupyter-console are
               ;; both present.
               (delete-file-recursively
                (string-append
                 (assoc-ref outputs "out") "/bin"))))))))
    ;; Remove the python-ipython propagated input, to avoid the cycle
    (propagated-inputs
     (alist-delete
      "python-ipython"
      (package-propagated-inputs python-jupyter-console)))))

(define-public jupyter
  (package
    (name "jupyter")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter" version))
       (sha256
        (base32
         "0pwf3pminkzyzgx5kcplvvbvwrrzd3baa7lmh96f647k30rlpp6r"))))
    (build-system python-build-system)
    ;; FIXME: it's not clear how to run the tests.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipywidgets" ,python-ipywidgets)
       ("python-jupyter-console" ,python-jupyter-console)
       ("python-nbconvert" ,python-nbconvert)
       ("python-notebook" ,python-notebook)))
    (home-page "http://jupyter.org")
    (synopsis "Web application for interactive documents")
    (description
     "The Jupyter Notebook is a web application that allows you to create and
share documents that contain live code, equations, visualizations and
explanatory text.  Uses include: data cleaning and transformation, numerical
simulation, statistical modeling, machine learning and much more.")
    (license license:bsd-3)))

(define-public python-chardet
  (package
    (name "python-chardet")
    (version "3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "chardet" version))
       (sha256
        (base32
         "1bpalpia6r5x1kknbk11p1fzph56fmmnp405ds8icksd3knr5aw4"))))
    (native-inputs
     `(("python-hypothesis" ,python-hypothesis)
       ("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (build-system python-build-system)
    (home-page "https://github.com/chardet/chardet")
    (synopsis "Universal encoding detector for Python 2 and 3")
    (description
     "This package provides @code{chardet}, a Python module that can
automatically detect a wide range of file encodings.")
    (license license:lgpl2.1+)))

(define-public python2-chardet
  (package-with-python2 python-chardet))

(define-public python-docopt
  (package
    (name "python-docopt")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       ;; The release on PyPI does not include tests.
       (uri (string-append
             "https://github.com/docopt/docopt/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16bf890xbdz3m30rsv2qacklh2rdn1zrfspfnwzx9g7vwz8yw4r1"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (zero? (system* "py.test")))))))
    (home-page "http://docopt.org")
    (synopsis "Command-line interface description language for Python")
    (description "This library allows the user to define a command-line
interface from a program's help message rather than specifying it
programatically with command-line parsers like @code{getopt} and
@code{argparse}.")
    (license license:expat)))

(define-public python2-docopt
  (package-with-python2 python-docopt))

(define-public python-pythondialog
  (package
    (name "python-pythondialog")
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pythondialog" version))
       (sha256
        (base32
         "1728ghsran47jczn9bhlnkvk5bvqmmbihabgif5h705b84r1272c"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((dialog (assoc-ref inputs "dialog")))
               ;; Since this library really wants to grovel the search path, we
               ;; must hardcode dialog's store path into it.
               (substitute* "dialog.py"
                 (("os.getenv\\(\"PATH\", \":/bin:/usr/bin\"\\)")
                  (string-append "os.getenv(\"PATH\")  + \":" dialog "/bin\"")))
               #t))))
       #:tests? #f)) ; no test suite
    (propagated-inputs
     `(("dialog" ,dialog)))
    (home-page "http://pythondialog.sourceforge.net/")
    (synopsis "Python interface to the UNIX dialog utility")
    (description "A Python wrapper for the dialog utility.  Its purpose is to
provide an easy to use, pythonic and comprehensive Python interface to dialog.
This allows one to make simple text-mode user interfaces on Unix-like systems")
    (license license:lgpl2.1)
    (properties `((python2-variant . ,(delay python2-pythondialog))))))

(define-public python2-pythondialog
  (let ((base (package-with-python2 (strip-python2-variant python-pythondialog))))
    (package
      (inherit base)
      (version (package-version python-pythondialog))
      (source (origin
                (method url-fetch)
                (uri (pypi-uri "python2-pythondialog" version))
                (sha256
                 (base32
                  "0d8k7lxk50imdyx85lv8j98i4c93a71iwpapnl1506rpkbm9qvd9")))))))

(define-public python-configobj
  (package
    (name "python-configobj")
    (version "5.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pypi.python.org/packages/source/c/configobj/"
                    "configobj-" version ".tar.gz"))
              (sha256
               (base32
                "00h9rcmws03xvdlfni11yb60bz3kxfvsj6dg6nrpzj71f03nbxd2"))
              ;; Patch setup.py so it looks for python-setuptools, which is
              ;; required to parse the keyword 'install_requires' in setup.py.
              (patches (search-patches "python-configobj-setuptools.patch"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (synopsis "Config file reading, writing and validation")
    (description "ConfigObj is a simple but powerful config file reader and
writer: an ini file round tripper.  Its main feature is that it is very easy to
use, with a straightforward programmer’s interface and a simple syntax for
config files.")
    (home-page "https://github.com/DiffSK/configobj")
    (license license:bsd-3)))

(define-public python2-configobj
  (package-with-python2 python-configobj))

(define-public python-configargparse
  (package
    (name "python-configargparse")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pypi.io/packages/source/C/ConfigArgParse/"
                    "ConfigArgParse-" version ".tar.gz"))
              (sha256
               (base32
                "0fgkiqh6r3rbkdq3k8c48m85g52k96686rw3a6jg4lcncrkpvk98"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pyyaml" ,python-pyyaml)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Bypass setuptools-shim because one test relies on "setup.py"
             ;; being the first argument passed to the python call.
             ;;
             ;; NOTE: Many tests do not run because they rely on Python's
             ;; built-in test.test_argparse, but we remove the unit tests from
             ;; our Python installation.
             (zero? (system* "python" "setup.py" "test")))))))
    (synopsis "Replacement for argparse")
    (description "A drop-in replacement for argparse that allows options to also
be set via config files and/or environment variables.")
    (home-page "https://github.com/bw2/ConfigArgParse")
    (license license:expat)))

(define-public python2-configargparse
  (package-with-python2 python-configargparse))

(define-public python-contextlib2
  (package
    (name "python-contextlib2")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "contextlib2" version))
       (sha256
        (base32
         "0cmp131dlh0d0zvw0aza1zd13glvngzk8lb4avks0hm7yxwdr9am"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (zero?
                      (system*
                       "python" "test_contextlib2.py" "-v")))))))
    (home-page "http://contextlib2.readthedocs.org/")
    (synopsis "Tools for decorators and context managers")
    (description "This module is primarily a backport of the Python
3.2 contextlib to earlier Python versions.  Like contextlib, it
provides utilities for common tasks involving decorators and context
managers.  It also contains additional features that are not part of
the standard library.")
    (license license:psfl)))

(define-public python2-contextlib2
  (package-with-python2 python-contextlib2))

(define-public python-texttable
  (package
    (name "python-texttable")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "texttable" version))
       (sha256
        (base32
         "1liiiydgkg37i46a418aw19fyf6z3ds51wdwwpyjbs12x0phhf4a"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; no tests
    (home-page "https://github.com/foutaise/texttable/")
    (synopsis "Python module for creating simple ASCII tables")
    (description "Texttable is a Python module for creating simple ASCII
tables.")
    (license license:lgpl2.1+)))

(define-public python2-texttable
  (package-with-python2 python-texttable))

(define-public python-atomicwrites
  (package
    (name "python-atomicwrites")
    (version "1.1.5")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "atomicwrites" version))
             (sha256
              (base32
               "11bm90fwm2avvf4f3ib8g925w7jr4m11vcsinn1bi6ns4bm32214"))))
    (build-system python-build-system)
    (synopsis "Atomic file writes in Python")
    (description "Library for atomic file writes using platform dependent tools
for atomic file system operations.")
    (home-page "https://github.com/untitaker/python-atomicwrites")
    (license license:expat)))

(define-public python2-atomicwrites
  (package-with-python2 python-atomicwrites))

(define-public python-click-threading
  (package
    (name "python-click-threading")
    (version "0.4.3")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "click-threading" version))
             (sha256
              (base32
               "0xs4bg2ws0zgyiplk312l049hi23c2zqf1g771rjhh5vr2msk4cg"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click" ,python-click)))
    (synopsis "Utilities for multithreading in Click")
    (description "This package provides utilities for multithreading in Click
applications.")
    (home-page "https://github.com/click-contrib/click-threading")
    (license license:expat)))

(define-public python-click-log
  (package
    (name "python-click-log")
    (version "0.2.0")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "click-log" version))
             (sha256
              (base32
               "1bjrfxji1yv4fj0g78ri2yfgn2wbivn8g69fxfinxvxpmighhshp"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click" ,python-click)))
    (synopsis "Logging for click applications")
    (description "This package provides a Python library for logging Click
applications.")
    (home-page "https://github.com/click-contrib/click-log")
    (license license:expat)))

(define-public python-apipkg
  (package
    (name "python-apipkg")
    (version "1.4")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "apipkg" version))
             (sha256
              (base32
               "1iks5701qnp3dlr3q1d9qm68y2plp2m029irhpz92a44psfkjf1f"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (synopsis "Namespace control and lazy-import mechanism")
    (description "With apipkg you can control the exported namespace of a Python
package and greatly reduce the number of imports for your users.  It is a small
pure Python module that works on virtually all Python versions.")
    (home-page "https://bitbucket.org/hpk42/apipkg")
    (license license:expat)))

(define-public python2-apipkg
  (package-with-python2 python-apipkg))

(define-public python-execnet
  (package
    (name "python-execnet")
    (version "1.4.1")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "execnet" version))
             (sha256
              (base32
               "1rpk1vyclhg911p3hql0m0nrpq7q7mysxnaaw6vs29cpa6kx8vgn"))))
    (build-system python-build-system)
    (arguments
     `(;; 2 failed, 275 passed, 670 skipped, 4 xfailed
       ;; The two test failures are caused by the lack of an `ssh` executable.
       ;; The test suite can be run with pytest after the 'install' phase.
       #:tests? #f))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
     `(("python-apipkg" ,python-apipkg)))
    (synopsis "Rapid multi-Python deployment")
    (description "Execnet provides a share-nothing model with
channel-send/receive communication for distributing execution across many
Python interpreters across version, platform and network barriers.  It has a
minimal and fast API targeting the following uses:
@enumerate
@item distribute tasks to (many) local or remote CPUs
@item write and deploy hybrid multi-process applications
@item write scripts to administer multiple environments
@end enumerate")
    (home-page "http://codespeak.net/execnet/")
    (license license:expat)))

(define-public python2-execnet
  (package-with-python2 python-execnet))

(define-public python-icalendar
  (package
    (name "python-icalendar")
    (version "4.0.0")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "icalendar" version))
             (sha256
              (base32
               "0jfp93x2pnpsbck92zw22dq7sl4pk0avv0gnb4x9vldrg6vlhyin"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-pytz" ,python-pytz)))
    (synopsis "Python library for parsing iCalendar files")
    (description "The icalendar package is a parser/generator of iCalendar
files for use with Python.")
    (home-page "https://github.com/collective/icalendar")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-newsfeed
  (package
    (name "python-sphinxcontrib-newsfeed")
    (version "0.1.4")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "sphinxcontrib-newsfeed" version))
             (sha256
              (base32
               "1d7gam3mn8v4in4p16yn3v10vps7nnaz6ilw99j4klij39dqd37p"))))
    (arguments '(#:tests? #f)) ; No tests.
    (build-system python-build-system)
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (synopsis "News Feed extension for Sphinx")
    (description "Sphinxcontrib-newsfeed is an extension for adding a simple
Blog, News or Announcements section to a Sphinx website.")
    (home-page "https://bitbucket.org/prometheus/sphinxcontrib-newsfeed")
    (license license:bsd-2)))

(define-public python-args
  (package
    (name "python-args")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "args" version))
              (sha256
               (base32
                "057qzi46h5dmxdqknsbrssn78lmqjlnm624iqdhrnpk26zcbi1d7"))))
    (build-system python-build-system)
    (home-page "https://github.com/kennethreitz/args")
    (synopsis "Command-line argument parser")
    (description
     "This library provides a Python module to parse command-line arguments.")
    (license license:bsd-3)))

(define-public python2-args
  (package-with-python2 python-args))

(define-public python-clint
  (package
    (name "python-clint")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "clint" version))
              (sha256
               (base32
                "1an5lkkqk1zha47198p42ji3m94xmzx1a03dn7866m87n4r4q8h5"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "py.test" "-v")))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-args" ,python-args)))
    (home-page "https://github.com/kennethreitz/clint")
    (synopsis "Command-line interface tools")
    (description
     "Clint is a Python module filled with a set of tools for developing
command-line applications, including tools for colored and indented
output, progress bar display, and pipes.")
    (license license:isc)))

(define-public python2-clint
  (package-with-python2 python-clint))

(define-public python-astor
  (package
    (name "python-astor")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "astor" version))
              (sha256
               (base32
                "1fdafq5hkis1fxqlmhw0sn44zp2ar46nxhbc22cvwg7hsd8z5gsa"))))
    (build-system python-build-system)
    (home-page "https://github.com/berkerpeksag/astor")
    (synopsis "Read and write Python ASTs")
    (description
     "Astor is designed to allow easy manipulation of Python source via the
Abstract Syntax Tree.")
    (license license:bsd-3)))

(define-public python2-astor
  (package-with-python2 python-astor))

(define-public python-rply
  (package
    (name "python-rply")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "rply" version))
              (sha256
               (base32
                "12rp1d9ba7nvd5rhaxi6xzx1rm67r1k1ylsrkzhpwnphqpb06cvj"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-appdirs" ,python-appdirs)))
    (home-page "https://github.com/alex/rply")
    (synopsis "Parser generator for Python")
    (description
     "This package provides a pure Python based parser generator, that also
works with RPython.  It is a more-or-less direct port of David Bazzley's PLY,
with a new public API, and RPython support.")
    (license license:bsd-3)))

(define-public python2-rply
  (package-with-python2 python-rply))

(define-public python-hy
  (package
    (name "python-hy")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hy" version))
              (sha256
               (base32
                "19sfymaksx9jhksfnb15ahid46mzrhdfzz6yy2craz2qnzvpmky8"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Tests require write access to HOME.
             (setenv "HOME" "/tmp")
             (zero? (system* "nosetests")))))))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-astor" ,python-astor)
       ("python-clint" ,python-clint)
       ("python-rply" ,python-rply)))
    (home-page "http://hylang.org/")
    (synopsis "Lisp frontend to Python")
    (description
     "Hy is a dialect of Lisp that's embedded in Python.  Since Hy transforms
its Lisp code into the Python Abstract Syntax Tree, you have the whole world of
Python at your fingertips, in Lisp form.")
    (license license:expat)))

(define-public python2-hy
  (package-with-python2 python-hy))

(define-public python2-functools32
  (package
    (name "python2-functools32")
    (version "3.2.3-2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "functools32" version))
        (sha256
         (base32
          "0v8ya0b58x47wp216n1zamimv4iw57cxz3xxhzix52jkw3xks9gn"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f)) ; no test target
    (home-page "https://github.com/MiCHiLU/python-functools32")
    (synopsis
     "Backport of the functools module from Python 3.2.3")
    (description
     "This package is a backport of the @code{functools} module from Python
3.2.3 for use with older versions of Python and PyPy.")
    (license license:expat)))

(define-public python2-subprocess32
  (package
    (name "python2-subprocess32")
    (version "3.2.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "subprocess32" version))
              (sha256
               (base32
                "14350dhhlhyz5gqzi3lihn9m6lvskx5mcb20srx1kgsk9i50li8y"))
              (patches
               (search-patches "python2-subprocess32-disable-input-test.patch"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (substitute* '("subprocess32.py"
                            "test_subprocess32.py")
               (("/bin/sh") (which "sh")))
             #t))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; For some reason this package fails to import
             ;; _posixsubprocess.so when PYTHONPATH is set to the build
             ;; directory. Running tests after install is easier.
             (add-installed-pythonpath inputs outputs)
             (zero? (system* "python" "test_subprocess32.py")))))))
    (home-page "https://github.com/google/python-subprocess32")
    (synopsis "Backport of the subprocess module from Python 3.2")
    (description
     "This is a backport of the @code{subprocess} standard library module
from Python 3.2 and 3.3 for use on Python 2.  It includes bugfixes and some
new features.  On POSIX systems it is guaranteed to be reliable when used
in threaded applications.  It includes timeout support from Python 3.3 but
otherwise matches 3.2’s API.")
    (license license:psfl)))

(define-public python2-futures
  (package
    (name "python2-futures")
    (version "3.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "futures" version))
        (sha256
         (base32
          "1pw1z4329xvlabdpwqa6b7v2fxf7hl64m4cgr22ckbym8m8m4hh5"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (home-page "https://github.com/agronholm/pythonfutures")
    (synopsis
     "Backport of the concurrent.futures package from Python 3.2")
    (description
     "The concurrent.futures module provides a high-level interface for
asynchronously executing callables.  This package backports the
concurrent.futures package from Python 3.2")
    (license license:bsd-3)))

(define-public python-promise
  (package
    (name "python-promise")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "promise" version))
        (sha256
         (base32
          "1k19ms8l3d5jzjh557rgkxb5sg4mqgfc315rn4hx1z3n8qq6lr3h"))))
    (build-system python-build-system)
    ;; Tests wants python-futures, which is a python2 only program, and
    ;; can't be found by python-promise at test time.
    (arguments `(#:tests? #f))
    (home-page "https://github.com/syrusakbary/promise")
    (synopsis "Promises/A+ implementation for Python")
    (description
     "Promises/A+ implementation for Python")
    (properties `((python2-variant . ,(delay python2-promise))))
    (license license:expat)))

(define-public python2-promise
  (let ((promise (package-with-python2
                   (strip-python2-variant python-promise))))
    (package (inherit promise)
      (arguments (substitute-keyword-arguments (package-arguments promise)
                   ((#:tests? _) #t)))
      (native-inputs
       `(("python2-futures" ,python2-futures)
         ("python2-pytest" ,python2-pytest)
         ,@(package-native-inputs promise))))))

(define-public python-colorama
  (package
   (name "python-colorama")
   (version "0.3.7")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "colorama" version))
     (sha256
      (base32
       "0avqkn6362v7k2kg3afb35g4sfdvixjgy890clip4q174p9whhz0"))))
   (build-system python-build-system)
   (synopsis "Colored terminal text rendering for Python")
   (description "Colorama is a Python library for rendering colored terminal
text.")
   (home-page "https://pypi.python.org/pypi/colorama")
   (license license:bsd-3)))

(define-public python2-colorama
  (package-with-python2 python-colorama))

(define-public python-rsa
  (package
   (name "python-rsa")
   (version "3.4.2")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "rsa" version))
     (sha256
      (base32
       "1dcxvszbikgzh99ybdc7jq0zb9wspy2ds8z9mjsqiyv3q884xpr5"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-pyasn1" ,python-pyasn1)))
   (synopsis "Pure-Python RSA implementation")
   (description "Python-RSA is a pure-Python RSA implementation.  It supports
encryption and decryption, signing and verifying signatures, and key
generation according to PKCS#1 version 1.5.  It can be used as a Python
library as well as on the command line.")
   (home-page "https://stuvel.eu/rsa")
   (license license:asl2.0)))

(define-public python2-rsa
  (package-with-python2 python-rsa))

(define-public python-pluggy
  (package
   (name "python-pluggy")
   (version "0.3.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "pluggy" version))
     (sha256
      (base32
       "18qfzfm40bgx672lkg8q9x5hdh76n7vax99aank7vh2nw21wg70m"))))
   (build-system python-build-system)
   (synopsis "Plugin and hook calling mechanism for Python")
   (description "Pluggy is an extraction of the plugin manager as used by
Pytest but stripped of Pytest specific details.")
   (home-page "https://pypi.python.org/pypi/pluggy")
   (license license:expat)))

(define-public python2-pluggy
  (package-with-python2 python-pluggy))

(define-public python-tox
  (package
   (name "python-tox")
   (version "2.8.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "tox" version))
     (sha256
      (base32
       "1drp6mwm8wdypjym15ia8lwjxbhcksb9vzxg4ay5dh4ji57by2ny"))))
   (build-system python-build-system)
   (arguments
    ;; FIXME: Tests require pytest-timeout, which itself requires
    ;; pytest>=2.8.0 for installation.
    '(#:tests? #f))
   (propagated-inputs
    `(("python-pluggy" ,python-pluggy) ; >=0.3.0,<0.4.0
      ("python-py" ,python-py)
      ("python-virtualenv" ,python-virtualenv)))
   (native-inputs
    `(; FIXME: Missing: ("python-pytest-timeout" ,python-pytest-timeout)
      ("python-pytest" ,python-pytest)  ; >= 2.3.5
      ("python-setuptools-scm" ,python-setuptools-scm)))
   (home-page "http://tox.testrun.org/")
   (synopsis "Virtualenv-based automation of test activities")
   (description "Tox is a generic virtualenv management and test command line
tool.  It can be used to check that a package installs correctly with
different Python versions and interpreters, or run tests in each type of
supported environment, or act as a frontend to continuous integration
servers.")
   (license license:expat)))

(define-public python2-tox
  (package-with-python2 python-tox))

(define-public python-jmespath
  (package
   (name "python-jmespath")
   (version "0.9.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "jmespath" version))
     (sha256
      (base32
       "0g9xvl69y7nr3w7ag4fsp6sm4fqf6vrqjw7504x2hzrrsh3ampq8"))))
   (build-system python-build-system)
   (native-inputs
    `(("python-nose" ,python-nose)))
   (synopsis "JSON Matching Expressions")
   (description "JMESPath (pronounced “james path”) is a Python library that
allows one to declaratively specify how to extract elements from a JSON
document.")
   (home-page "https://github.com/jmespath/jmespath.py")
   (license license:expat)))

(define-public python2-jmespath
  (package-with-python2 python-jmespath))

(define-public python-botocore
  (package
   (name "python-botocore")
   (version "1.7.9")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "botocore" version))
     (sha256
      (base32
       "02b1bw25r1wdjs5yppb1h9igf11wj092biriv2yg8hzp5r0wrkmg"))))
   (build-system python-build-system)
   (arguments
    ;; FIXME: Many tests are failing.
    '(#:tests? #f))
   (propagated-inputs
    `(("python-dateutil" ,python-dateutil)
      ("python-docutils" ,python-docutils)
      ("python-jmespath" ,python-jmespath)))
   (native-inputs
    `(("python-mock" ,python-mock)
      ("python-nose" ,python-nose)
      ("behave" ,behave)
      ("python-tox" ,python-tox)
      ("python-wheel" ,python-wheel)))
   (home-page "https://github.com/boto/botocore")
   (synopsis "Low-level interface to AWS")
   (description "Botocore is a Python library that provides a low-level
interface to the Amazon Web Services (AWS) API.")
   (license license:asl2.0)))

(define-public python2-botocore
  (package-with-python2 python-botocore))

(define-public python-xdo
  (package
    (name "python-xdo")
    (version "0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://http.debian.net/debian/pool/main/p/python-xdo/"
                    "python-xdo_" version ".orig.tar.gz"))
              (sha256
               (base32
                "1vqh1n5yy5dhnq312kwrl90fnck4v26is3lq3lxdvcn60vv19da0"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'patch-libxdo-path
           ;; Hardcode the path of dynamically loaded libxdo library.
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libxdo (string-append
                            (assoc-ref inputs "xdotool")
                            "/lib/libxdo.so")))
               (substitute* "xdo/_xdo.py"
                 (("find_library\\(\"xdo\"\\)")
                  (simple-format #f "\"~a\"" libxdo)))
               #t))))
       #:tests? #f))  ; no tests provided
    (propagated-inputs
     `(("python-six" ,python-six)))
    (inputs
     `(("xdotool" ,xdotool)
       ("libX11" ,libx11)))
    (home-page "https://tracker.debian.org/pkg/python-xdo")
    (synopsis "Python library for simulating X11 keyboard/mouse input")
    (description "Provides bindings to libxdo for manipulating X11 via simulated
input.  (Note that this is mostly a legacy library; you may wish to look at
python-xdo for newer bindings.)")
    (license license:bsd-3)))

(define-public python2-xdo
  (package-with-python2 python-xdo))

(define-public python-mako
  (package
    (name "python-mako")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Mako" version))
       (sha256
        (base32
         "03dyxgjknp4ffsv7vwfd28l5bbpzi0ylp20543wpg3iahyyrwma8"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-markupsafe" ,python-markupsafe)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nose" ,python-nose)
       ("python-pytest" ,python-pytest)))
    (home-page "http://www.makotemplates.org/")
    (synopsis "Templating language for Python")
    (description "Mako is a templating language for Python that compiles
templates into Python modules.")
    (license license:expat)))

(define-public python2-mako
  (package-with-python2 python-mako))

(define-public python-waitress
  (package
    (name "python-waitress")
    (version "0.8.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "waitress" version))
       (sha256
        (base32
         "017n9ra6vvmq9d5sfhdzyzr1mg15x2hj2dhm4pdlw98c1ypw2h3w"))))
    (build-system python-build-system)
    (home-page "https://github.com/Pylons/waitress")
    (synopsis "Waitress WSGI server")
    (description "Waitress is meant to be a production-quality pure-Python WSGI
server with very acceptable performance.")
    (license license:zpl2.1)))

(define-public python2-waitress
  (package-with-python2 python-waitress))

(define-public python-pyquery
  (package
    (name "python-pyquery")
    (version "1.2.17")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyquery" version))
       (sha256
        (base32
         "1xia20wm0vx5dk85kcwgh13bylz8qh47ffjxssd2586r60xi783a"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-webob" ,python-webob)
       ("python-webtest" ,python-webtest)))
    (propagated-inputs
     `(("python-lxml" ,python-lxml)
       ("python-cssselect" ,python-cssselect)))
    (home-page "https://github.com/gawel/pyquery")
    (synopsis "Make jQuery-like queries on xml documents")
    (description "pyquery allows you to make jQuery queries on xml documents.
The API is as much as possible the similar to jQuery.  pyquery uses lxml for
fast xml and html manipulation.")
    (license license:bsd-3)))

(define-public python2-pyquery
  (package-with-python2 python-pyquery))

(define-public python-anyjson
  (package
    (name "python-anyjson")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "anyjson" version))
       (sha256
        (base32
         "1fjph4alvcscsl5d4b6qpv1yh31jy05jxi1l0xff7lws7j32v09p"))))
    (build-system python-build-system)
    (arguments
     `(;; We could possibly get tests working, but on Python 3 it's not so easy.
       ;; Very strangely, 2to3 is run *during setup.py install* (or bdist, or
       ;; whatever) so this transformation needs to be done before the tests
       ;; can be run.  Maybe we could add a build step to transform beforehand
       ;; but it could be annoying/difficult.
       ;; We can enable tests for the Python 2 version, though, and do below.
       #:tests? #f))
    (home-page "https://bitbucket.org/runeh/anyjson/")
    (synopsis
     "Wraps best available JSON implementation in a common interface")
    (description
     "Anyjson loads whichever is the fastest JSON module installed
and provides a uniform API regardless of which JSON implementation is used.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-anyjson))))))

(define-public python2-anyjson
  (let ((anyjson (package-with-python2
                  (strip-python2-variant python-anyjson))))
    (package
      (inherit anyjson)
      (arguments `(;; Unlike the python 3 variant, we do run tests.  See above!
                   #:tests? #t
                   ,@(package-arguments anyjson)))
      (native-inputs `(("python2-nose" ,python2-nose))))))

(define-public python-amqp
  (package
    (name "python-amqp")
    (version "1.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "amqp" version))
       (sha256
        (base32
         "06n6q0kxhjnbfz3vn8x9yz09lwmn1xi9d6wxp31h5jbks0b4vsid"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-mock" ,python-mock)))
    (home-page "https://github.com/celery/py-amqp")
    (synopsis
     "Low-level AMQP client for Python (fork of amqplib)")
    (description
     "This is a fork of amqplib which was originally written by Barry Pederson.
It is maintained by the Celery project, and used by kombu as a pure python
alternative when librabbitmq is not available.")
    (license license:lgpl2.1+)
    (properties `((python2-variant . ,(delay python2-amqp))))))

(define-public python2-amqp
  (let ((amqp (package-with-python2
               (strip-python2-variant python-amqp))))
    (package
      (inherit amqp)
      (arguments `(;; Tries to run coverage tests with nose-cover3, which seems
                   ;; unmaintained.  Weirdly, does not do this on the python 3
                   ;; version?
                   #:tests? #f
                   ,@(package-arguments amqp))))))

(define-public python-kombu
  (package
    (name "python-kombu")
    (version "3.0.37")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kombu" version))
       (sha256
        (base32
         "0l16chb314gpq2v7fh94a22c30lcv6w3ylmhsa60bldlcq6a0r70"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-anyjson" ,python-anyjson)
       ("python-amqp" ,python-amqp)
       ("python-redis" ,python-redis)))
    (home-page "http://kombu.readthedocs.org")
    (synopsis "Message passing library for Python")
    (description "The aim of Kombu is to make messaging in Python as easy as
possible by providing an idiomatic high-level interface for the AMQ protocol,
and also provide proven and tested solutions to common messaging problems.
AMQP is the Advanced Message Queuing Protocol, an open standard protocol for
message orientation, queuing, routing, reliability and security, for which the
RabbitMQ messaging server is the most popular implementation.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-kombu))))))

(define-public python2-kombu
  (let ((kombu (package-with-python2
                (strip-python2-variant python-kombu))))
    (package
      (inherit kombu)
      (arguments `(;; FIXME: 'TestTransport.test_del_sync' fails on python2.
                   ;; It works fine on the python3 variant.
                   #:tests? #f
                   ,@(package-arguments kombu)))
      (native-inputs `(("python2-unittest2" ,python2-unittest2)
                ,@(package-native-inputs kombu))))))

(define-public python-billiard
  (package
    (name "python-billiard")
    (version "3.3.0.23")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "billiard" version))
       (sha256
        (base32
         "02wxsc6bhqvzh8j6w758kvgqbnj14l796mvmrcms8fgfamd2lak9"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/celery/billiard")
    (synopsis
     "Python multiprocessing fork with improvements and bugfixes")
    (description
     "Billiard is a fork of the Python 2.7 multiprocessing package.  The
multiprocessing package itself is a renamed and updated version of R Oudkerk's
pyprocessing package.  This standalone variant is intended to be compatible with
Python 2.4 and 2.5, and will draw its fixes/improvements from python-trunk.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-billiard))))))

(define-public python2-billiard
  (let ((billiard (package-with-python2
                   (strip-python2-variant python-billiard))))
    (package
      (inherit billiard)
      (native-inputs `(("python2-unittest2" ,python2-unittest2)
                       ("python2-mock" ,python2-mock)
                       ,@(package-native-inputs billiard))))))

(define-public python-celery
  (package
    (name "python-celery")
    (version "3.1.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "celery" version))
       (sha256
        (base32
         "0yh2prhdnx2dgkb67a5drj12hh2zvzx5f611p7mqqg01ydghif4r"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; These tests break with Python 3.5:
         ;; https://github.com/celery/celery/issues/2897#issuecomment-253066295
         (replace 'check
           (lambda _
             (zero?
               (system* "nosetests" "--exclude=^test_safe_to_remove.*")))))))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-pytz" ,python-pytz)
       ("python-billiard" ,python-billiard)
       ("python-kombu" ,python-kombu)))
    (home-page "http://celeryproject.org")
    (synopsis "Distributed Task Queue")
    (description "Celery is an asynchronous task queue/job queue based on
distributed message passing.  It is focused on real-time operation, but
supports scheduling as well.  The execution units, called tasks, are executed
concurrently on a single or more worker servers using multiprocessing,
Eventlet, or gevent.  Tasks can execute asynchronously (in the background) or
synchronously (wait until ready).")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-celery))))))

(define-public python2-celery
  (let ((celery (package-with-python2
                 (strip-python2-variant python-celery))))
    (package
      (inherit celery)
      (native-inputs `(("python2-unittest2" ,python2-unittest2)
                       ("python2-mock" ,python2-mock)
                       ,@(package-native-inputs celery))))))

(define-public python-translitcodec
  (package
    (name "python-translitcodec")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "translitcodec" version))
       (sha256
        (base32
         "10x6pvblkzky1zhjs8nmx64nb9jdzxad4bxhq4iwv0j4z2aqjnki"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))  ; no tests provided
    (home-page
     "https://github.com/claudep/translitcodec")
    (synopsis
     "Unicode to 8-bit charset transliteration codec")
    (description
     "This package contains codecs for transliterating ISO 10646 texts into
best-effort representations using smaller coded character sets (ASCII,
ISO 8859, etc.).")
    (license license:expat)))

(define-public python2-translitcodec
  (package-with-python2 python-translitcodec))

(define-public python-editor
  (package
  (name "python-editor")
  (version "0.5")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "python-editor" version))
      (sha256
        (base32
          "1ypnpgvzpkbwsg4rdvy4sy51j28b5xq9v8pnkwxncn07vqz06p7n"))))
  (build-system python-build-system)
  (home-page
    "https://github.com/fmoo/python-editor")
  (synopsis
    "Programmatically open an editor, capture the result")
  (description
    "python-editor is a library that provides the editor module for
programmatically interfacing with your system's $EDITOR.")
  (license license:asl2.0)))

(define-public python2-editor
  (package-with-python2 python-editor))

(define-public python-sphinxcontrib-programoutput
  (package
    (name "python-sphinxcontrib-programoutput")
    (version "0.10")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-programoutput" version))
              (sha256
               (base32
                "153hhnlbx4688zj9wd64819ps5znc2jlyp5crkgzvn5hxgy99vpx"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Many tests are failing and the upstream is gone.
     '(#:tests? #f))
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (synopsis "Sphinx extension to include program output")
    (description "A Sphinx extension to literally insert the output of arbitrary
commands into documents, helping you to keep your command examples up to date.")
    (home-page "https://github.com/lunaryorn/sphinxcontrib-programoutput")
    (license license:bsd-2)))

(define-public python2-sphinxcontrib-programoutput
  (package-with-python2 python-sphinxcontrib-programoutput))

(define-public python-sphinx-repoze-autointerface
  (package
    (name "python-sphinx-repoze-autointerface")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "repoze.sphinx.autointerface" version))
              (sha256
               (base32
                "08ycivzf7bh4a1zcyp31hbyqs1b2c9r26raa3vxjwwmbfqr3iw4f"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)
       ("python-zope-interface" ,python-zope-interface)))
    (synopsis "Auto-generate Sphinx API docs from Zope interfaces")
    (description "This package defines an extension for the Sphinx documentation
system.  The extension allows generation of API documentation by
introspection of @code{zope.interface} instances in code.")
    (home-page "https://github.com/repoze/repoze.sphinx.autointerface")
    (license license:repoze)))

(define-public python2-sphinx-repoze-autointerface
  (package-with-python2 python-sphinx-repoze-autointerface))

(define-public python-vobject
  (package
    (name "python-vobject")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "vobject" version))
              (sha256
               (base32
                "0hqjgf3ay1m5w1c0k00g5yfpdz1zni5qnr5rh9b8fg9hjvhwlmhg"))))
    (build-system python-build-system)
    (arguments
     '(;; The test suite relies on some non-portable Windows interfaces.
       #:tests? #f))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-pyicu" ,python-pyicu)))
    (synopsis "Parse and generate vCard and vCalendar files")
    (description "Vobject is intended to be a full featured Python package for
parsing and generating vCard and vCalendar files.  Currently, iCalendar files
are supported and well tested. vCard 3.0 files are supported, and all data
should be imported, but only a few components are understood in a sophisticated
way.")
    (home-page "http://eventable.github.io/vobject/")
    (license license:asl2.0)))

(define-public python2-vobject
  (package-with-python2 python-vobject))

(define-public python-munkres
  (package
    (name "python-munkres")
    (version "1.0.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "munkres" version))
              (sha256
               (base32
                "0mbspx4zv8id4x6pim6ybsa1xh96qwpbqj7skbqz4c9c9nf1lpqq"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; no test suite
    (home-page "http://software.clapper.org/munkres/")
    (synopsis "Implementation of the Munkres algorithm")
    (description "The Munkres module provides an implementation of the Munkres
algorithm (also called the Hungarian algorithm or the Kuhn-Munkres algorithm),
useful for solving the Assignment Problem.")
    (license license:bsd-3)))

(define-public python2-munkres
  (package-with-python2 python-munkres))

(define-public python-whoosh
  (package
    (name "python-whoosh")
    (version "2.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Whoosh" version))
       (sha256
        (base32
         "10qsqdjpbc85fykc1vgcs8xwbgn4l2l52c8d83xf1q59pwyn79bw"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://bitbucket.org/mchaput/whoosh")
    (synopsis "Full text indexing, search, and spell checking library")
    (description
     "Whoosh is a fast, pure-Python full text indexing, search, and spell
checking library.")
    (license license:bsd-2)))

(define-public python2-whoosh
  (let ((whoosh (package-with-python2 (strip-python2-variant python-whoosh))))
    (package (inherit whoosh)
      (propagated-inputs
       `(("python2-backport-ssl-match-hostname"
          ,python2-backport-ssl-match-hostname)
          ,@(package-propagated-inputs whoosh))))))

(define-public python-pathlib
  (package
    (name "python-pathlib")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pathlib" version))
              (sha256
               (base32
                "17zajiw4mjbkkv6ahp3xf025qglkj0805m9s41c45zryzj6p2h39"))))
    (build-system python-build-system)
    ;; The tests depend on the internal "test" module, which does not provide
    ;; a stable interface.
    (arguments `(#:tests? #f))
    (home-page "https://pathlib.readthedocs.org/")
    (synopsis "Object-oriented file system paths")
    (description "Pathlib offers a set of classes to handle file system paths.
It offers the following advantages over using string objects:

@enumerate
@item No more cumbersome use of os and os.path functions.  Everything can
be done easily through operators, attribute accesses, and method calls.
@item Embodies the semantics of different path types.  For example,
comparing Windows paths ignores casing.
@item Well-defined semantics, eliminating any inconsistencies or
ambiguities (forward vs. backward slashes, etc.).
@end enumerate

Note: In Python 3.4, pathlib is now part of the standard library.  For other
Python versions please consider python-pathlib2 instead, which tracks the
standard library module.  This module (python-pathlib) isn't maintained
anymore.")
    (license license:expat)))

(define-public python2-pathlib
  (package-with-python2 python-pathlib))

(define-public python2-pathlib2
  (package
    (name "python2-pathlib2")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pathlib2" version))
              (sha256
               (base32
                "0p050msg5c8d0kadv702jnfshaxrb0il765cpkgnhn6mq5hakcyy"))))
    (build-system python-build-system)
    ;; We only need the the Python 2 variant, since for Python 3 our minimum
    ;; version is 3.4 which already includes this package as part of the
    ;; standard library.
    (arguments
     `(#:python ,python-2))
    (native-inputs
     `(("python2-six" ,python2-six)))
    (home-page "https://pypi.python.org/pypi/pathlib2/")
    (synopsis "Object-oriented file system paths - backport of standard
pathlib module")
    (description "The goal of pathlib2 is to provide a backport of standard
pathlib module which tracks the standard library module, so all the newest
features of the standard pathlib can be used also on older Python versions.

Pathlib offers a set of classes to handle file system paths.  It offers the
following advantages over using string objects:

@enumerate
@item No more cumbersome use of os and os.path functions.  Everything can
be done easily through operators, attribute accesses, and method calls.
@item Embodies the semantics of different path types.  For example,
comparing Windows paths ignores casing.
@item Well-defined semantics, eliminating any inconsistencies or
ambiguities (forward vs. backward slashes, etc.).
@end enumerate")
    (license license:expat)))

(define-public python-jellyfish
  (package
    (name "python-jellyfish")
    (version "0.5.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jellyfish" version))
              (sha256
               (base32
                "1j9rplb16ba2prjj6mip46z0w9pnhnqpwgiwi0x93vnas14rlyl8"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/jamesturk/jellyfish")
    (synopsis "Approximate and phonetic matching of strings")
    (description "Jellyfish uses a variety of string comparison and phonetic
encoding algorithms to do fuzzy string matching.")
    (license license:bsd-2)
    (properties `((python2-variant . ,(delay python2-jellyfish))))))

(define-public python2-jellyfish
  (let ((jellyfish (package-with-python2
                     (strip-python2-variant python-jellyfish))))
    (package (inherit jellyfish)
      (native-inputs `(("python2-unicodecsv" ,python2-unicodecsv)
                       ,@(package-native-inputs jellyfish))))))

(define-public python2-unicodecsv
  (package
    (name "python2-unicodecsv")
    (version "0.14.1")
    (source (origin
             (method url-fetch)
             ;; The test suite is not included in the PyPi release.
             ;; https://github.com/jdunck/python-unicodecsv/issues/19
             (uri (string-append "https://github.com/jdunck/python-unicodecsv/"
                                 "archive/" version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "087nqanfcyp6mlfbbr5lva5f3w6iz1bybls9xlrb8icmc474wh4w"))))
    (build-system python-build-system)
    (arguments
     `(;; It supports Python 3, but Python 3 can already do Unicode CSV.
       #:python ,python-2))
    (native-inputs
     `(("python2-unittest2" ,python2-unittest2)))
    (home-page "https://github.com/jdunck/python-unicodecsv")
    (synopsis "Unicode CSV module for Python 2")
    (description "Unicodecsv is a drop-in replacement for Python 2.7's CSV
module, adding support for Unicode strings.")
    (license license:bsd-2)))

(define-public python-rarfile
  (package
    (name "python-rarfile")
    (version "2.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "rarfile" version))
              (sha256
               (base32
                "0qfad483kcbga0bn4qmcz953xjk16r52fahiy46zzn56v80y89ra"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Many tests fail, but the installation proceeds.
           (lambda _ (zero? (system* "make" "-C" "test" "test")))))))
    (native-inputs
     `(("which" ,which))) ; required for tests
    (propagated-inputs
     `(("libarchive" ,libarchive)))
    (home-page "https://github.com/markokr/rarfile")
    (synopsis "RAR archive reader for Python")
    (description "This is Python module for RAR archive reading.  The interface
is made as zipfile like as possible.")
    (license license:isc)))

(define-public python2-rarfile
  (package-with-python2 python-rarfile))

(define-public python-magic
  (package
    (name "python-magic")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ahupp/python-magic/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "17bgy92i7sb021f2s4mw1dcvpm6p1mi9jihridwy1pyn8mzvpjgk"))
       (file-name (string-append name "-" version "-checkout"))))
    (build-system python-build-system)
    (arguments
     ;; The tests are unreliable, so don't run them.  The tests fail
     ;; under Python3 because they were written for Python2 and
     ;; contain import statements that do not work in Python3.  One of
     ;; the tests fails under Python2 because its assertions are
     ;; overly stringent; it relies on comparing output strings which
     ;; are brittle and can change depending on the version of
     ;; libmagic being used and the system on which the test is
     ;; running.  In my case, under GuixSD 0.10.0, only one test
     ;; failed, and it seems to have failed only because the version
     ;; of libmagic that is packaged in Guix outputs a slightly
     ;; different (but not wrong) string than the one that the test
     ;; expected.
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
         ;; Replace a specific method call with a hard-coded
         ;; path to the necessary libmagic.so file in the
         ;; store.  If we don't do this, then the method call
         ;; will fail to find the libmagic.so file, which in
         ;; turn will cause any application using
         ;; python-magic to fail.
         (add-before 'build 'hard-code-path-to-libmagic
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((file (assoc-ref inputs "file")))
               (substitute* "magic.py"
                 (("ctypes.util.find_library\\('magic'\\)")
                  (string-append "'" file "/lib/libmagic.so'")))
           #t)))
         (add-before 'install 'disable-egg-compression
           (lambda _
             (let ((port (open-file "setup.cfg" "a")))
               (display "\n[easy_install]\nzip_ok = 0\n"
                        port)
               (close-port port)
               #t))))))
    (inputs
     ;; python-magic needs to be able to find libmagic.so.
     `(("file" ,file)))
    (home-page "https://github.com/ahupp/python-magic")
    (synopsis "File type identification using libmagic")
    (description
     "This module uses ctypes to access the libmagic file type
identification library.  It makes use of the local magic database and
supports both textual and MIME-type output.  Note that this module and
the python-file module both provide a \"magic.py\" file; these two
modules, which are different and were developed separately, both serve
the same purpose: to provide Python bindings for libmagic.")
    (license license:expat)))

(define-public python2-magic
  (package-with-python2 python-magic))

(define-public python2-s3cmd
  (package
    (name "python2-s3cmd")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/s3tools/s3cmd/" version "/"
                            "s3cmd-" version ".tar.gz"))
        (sha256
          (base32
            "0ki1rzhm5icvi9ry5jswi4b22yqwyj0d2wsqsgilwx6qhi7pjxa6"))))
    (build-system python-build-system)
    (arguments
     ;; s3cmd is written for python2 only and contains no tests.
     `(#:python ,python-2
       #:tests? #f))
    (propagated-inputs
     `(("python2-dateutil" ,python2-dateutil)
       ;; The python-file package also provides a magic.py module.
       ;; This is an unfortunate state of affairs; however, s3cmd
       ;; fails to install if it cannot find specifically the
       ;; python-magic package.  Thus we include it, instead of using
       ;; python-file.  Ironically, s3cmd sometimes works better
       ;; without libmagic bindings at all:
       ;; https://github.com/s3tools/s3cmd/issues/198
       ("python2-magic" ,python2-magic)))
    (home-page "http://s3tools.org/s3cmd")
    (synopsis "Command line tool for S3-compatible storage services")
    (description
     "S3cmd is a command line tool for uploading, retrieving and managing data
in storage services that are compatible with the Amazon Simple Storage
Service (S3) protocol, including S3 itself.  It supports rsync-like backup,
GnuPG encryption, and more.  It also supports management of Amazon's
CloudFront content delivery network.")
    (license license:gpl2+)))

(define-public python-pkgconfig
  (package
    (name "python-pkgconfig")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pkgconfig" version))
        (sha256
          (base32
            "1pw0kmvc57sjmaxi6c54fqsnihqj6hvhc9y1vaz36axafzqam7bh"))))
    (build-system python-build-system)
    (native-inputs
      `(("python-nose" ,python-nose)))
    (inputs
      `(("pkg-config" ,pkg-config)))
    (arguments
      `(;; Tests fail with "ValueError: _type_ 'v' not supported" on Python 3,
        ;; and on Python 2 they need the dl module deprecated since Python 2.6.
        #:tests? #f
        ;; Hard-code the path to pkg-config.
        #:phases
        (modify-phases %standard-phases
          (add-before
           'build 'patch
           (lambda _
             (substitute* "pkgconfig/pkgconfig.py"
               (("cmd = 'pkg-config")
                (string-append "cmd = '" (which "pkg-config"))))
             #t)))))
    (home-page "https://github.com/matze/pkgconfig")
    (synopsis "Python interface for pkg-config")
    (description "This module provides a Python interface to pkg-config.  It
can be used to find all pkg-config packages, check if a package exists,
check if a package meets certain version requirements, query CFLAGS and
LDFLAGS and parse the output to build extensions with setup.py.")
    (license license:expat)))

(define-public python2-pkgconfig
  (package-with-python2 python-pkgconfig))

(define-public python-bz2file
  (package
    (name "python-bz2file")
    (version "0.98")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bz2file" version))
       (sha256
        (base32
         "126s53fkpx04f33a829yqqk8fj4png3qwg4m66cvlmhmwc8zihb4"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Tests use deprecated python modules.
    (home-page "https://github.com/nvawda/bz2file")
    (synopsis "Read and write bzip2-compressed files")
    (description
     "Bz2file is a Python library for reading and writing bzip2-compressed
files.  It contains a drop-in replacement for the I/O interface in the
standard library's @code{bz2} module, including features from the latest
development version of CPython that are not available in older releases.")
    (license license:asl2.0)
    (properties `((python2-variant . ,(delay python2-bz2file))))))

(define-public python2-bz2file
  (let ((base (package-with-python2
               (strip-python2-variant python-bz2file))))
    (package
      (inherit base)
      (arguments
       `(#:python ,python-2
         #:phases
         (modify-phases %standard-phases
           ;; 'python setup.py test' does not work as of 0.98.
           ;; There is only the one test file, so we run it directly.
           (replace 'check
                    (lambda _ (zero? (system* "python"
                                              "test_bz2file.py"))))))))))

(define-public python-future
  (package
    (name "python-future")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "future" version))
       (sha256
        (base32
         "1nzy1k4m9966sikp0qka7lirh8sqrsyainyf8rk97db7nwdfv773"))))
    (build-system python-build-system)
    ;; Many tests connect to the network or are otherwise flawed.
    ;; https://github.com/PythonCharmers/python-future/issues/210
    (arguments
     `(#:tests? #f))
    (home-page "http://python-future.org")
    (synopsis "Single-source support for Python 3 and 2")
    (description
     "@code{python-future} is the missing compatibility layer between Python 2 and
Python 3.  It allows you to use a single, clean Python 3.x-compatible codebase
to support both Python 2 and Python 3 with minimal overhead.")
    (license license:expat)))

(define-public python2-future
  (package-with-python2 python-future))

(define-public python-cysignals
  (package
    (name "python-cysignals")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cysignals" version ".tar.bz2"))
        (sha256
          (base32
            "14cbyd9znlz6cxy1s3g6v6dv5jj45hn27pywkidd9b1zanaysqc6"))))
    (build-system python-build-system)
    (native-inputs
      `(("python-cython" ,python-cython)
        ("python-sphinx" ,python-sphinx)))
    (inputs
      `(("pari-gp" ,pari-gp)))
    (arguments
     `(#:modules ((guix build python-build-system)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  (guix build utils))
       ;; FIXME: Tests are executed after installation and currently fail
       ;; when not installing into standard locations; the author is working
       ;; on a fix.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before
          'build 'configure
          (assoc-ref gnu:%standard-phases 'configure)))))
    (home-page
      "https://github.com/sagemath/cysignals")
    (synopsis
      "Handling of interrupts and signals for Cython")
    (description
      "The cysignals package provides mechanisms to handle interrupts (and
other signals and errors) in Cython code, using two related approaches,
for mixed Cython/Python code or external C libraries and pure Cython code,
respectively.")
    (license license:lgpl3+)))

(define-public python2-cysignals
  (package-with-python2 python-cysignals))

(define-public python2-shedskin
 (package
  (name "python2-shedskin")
  (version "0.9.4")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "https://github.com/shedskin/shedskin/"
                          "releases/download/v" version
                          "/shedskin-" version ".tgz"))
      (sha256
        (base32
          "0nzwrzgw1ga8rw6f0ryq7zr9kkiavd1cqz5hzxkcbicl1dk7kz41"))))
  (build-system python-build-system)
  (arguments
   `(#:python ,python-2
     #:phases (modify-phases %standard-phases
               (add-after 'unpack 'fix-resulting-include-libs
                (lambda* (#:key inputs #:allow-other-keys)
                 (let ((libgc (assoc-ref inputs "libgc"))
                       (pcre (assoc-ref inputs "pcre")))
                  (substitute* "shedskin/makefile.py"
                   (("variable == 'CCFLAGS':[ ]*")
                    (string-append "variable == 'CCFLAGS':\n"
                                   "            line += ' -I " pcre "/include"
                                   " -I " libgc "/include'"))
                   (("variable == 'LFLAGS':[ ]*")
                    (string-append "variable == 'LFLAGS':\n"
                                   "            line += ' -L" pcre "/lib"
                                   " -L " libgc "/lib'")))
                  #t))))))
  (inputs `(("pcre" ,pcre)
            ("libgc" ,libgc)))
  (home-page "https://shedskin.github.io/")
  (synopsis "Experimental Python-2 to C++ Compiler")
  (description (string-append "This is an experimental compiler for a subset of
Python.  It generates C++ code and a Makefile."))
  (license (list license:gpl3 license:bsd-3 license:expat))))

(define-public python2-rope
  (package
    (name "python2-rope")
    (version "0.10.3")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "rope" version))
      (sha256
        (base32
         "18k5znhpwvrfck3yp0jmhd5j8r0f0s8bk1zh5yhs2cfgmfhbwigb"))))
    (arguments
     ;; Rope is currently python-2 only.
     ;; https://github.com/python-rope/rope/issues/57
     `(#:python ,python-2))
    (build-system python-build-system)
    (native-inputs
     `(("python2-unittest2" ,python2-unittest2)))
    (home-page "https://github.com/python-rope/rope")
    (synopsis "Refactoring library for Python")
    (description "Rope is a refactoring library for Python.  It facilitates
the renaming, moving and extracting of attributes, functions, modules, fields
and parameters in Python 2 source code.  These refactorings can also be applied
to occurrences in strings and comments.")
    (license license:gpl2)))

(define-public python-py3status
  (package
    (name "python-py3status")
    (version "3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py3status" version))
       (sha256
        (base32
         "0i283z1pivmir61z8kbiycigc94l61v33ygzkhczf1ifq7cppyds"))))
    (build-system python-build-system)
    (inputs
     `(("file" ,file)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; 'file' is used for detection of configuration file encoding
         ;; let's make link the dependency to particular input
         (add-before 'build 'patch-file-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((file-path (assoc-ref inputs "file")))
               (substitute* "py3status/parse_config.py"
                 (("check_output\\(\\['file'")
                  (string-append "check_output(['" file-path "/bin/file'")))
               #t))))
       #:tests? #f)) ; TODO: Requires many libraries not in Guix.
    (home-page "https://github.com/ultrabug/py3status")
    (synopsis "Extensible i3status wrapper written in Python")
    (description "py3status is an i3status wrapper which extends i3status
functionality in a modular way, allowing you to extend your panel with your
own code, responding to click events and updating clock every second.")
    (license license:bsd-3)))

(define-public python-tblib
  (package
    (name "python-tblib")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "tblib" version))
              (sha256 (base32
                       "02iahfkfa927hb4jq2bak36ldihwapzacfiq5lyxg8llwn98a1yi"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Upstream runs tests after installation and the package itself
             ;; resides in a subdirectory. Extend PYTHONPATH so it will be
             ;; found.
             (setenv "PYTHONPATH"
                     (string-append (getcwd) "/build/lib:"
                                    (getenv "PYTHONPATH")))
             (zero? (system* "py.test" "-vv" "tests" "README.rst")))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-six" ,python-six)))
    (home-page "https://github.com/ionelmc/python-tblib")
    (synopsis "Traceback serialization library")
    (description
     "Traceback serialization allows you to:

@enumerate
@item Pickle tracebacks and raise exceptions with pickled tracebacks in
different processes.  This allows better error handling when running code over
multiple processes (imagine multiprocessing, billiard, futures, celery etc).

@item Parse traceback strings and raise with the parsed tracebacks.
@end enumerate\n")
    (license license:bsd-3)))

(define-public python2-tblib
  (package-with-python2 python-tblib))

(define-public python-greenlet
  (package
    (name "python-greenlet")
    (version "0.4.11")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "greenlet" version))
              (sha256
               (base32
                "1xhik26j4f3kc4qw9xmj0c567rb5h1zryb4ijwqnqwwjvfhbv59h"))))
    (build-system python-build-system)
    (home-page "https://greenlet.readthedocs.io/")
    (synopsis "Lightweight in-process concurrent programming")
    (description
     "Greenlet package is a spin-off of Stackless, a version of CPython
that supports micro-threads called \"tasklets\".  Tasklets run
pseudo-concurrently (typically in a single or a few OS-level threads) and
are synchronized with data exchanges on \"channels\".")
    (license (list license:psfl license:expat))))

(define-public python2-greenlet
  (package-with-python2 python-greenlet))

(define-public python-gevent
  (package
    (name "python-gevent")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gevent" version))
              (sha256
               (base32
                "1smf3kvidpdiyi2c81alal74p2zm0clrm6xbyy6y1k9a3f2vkrbf"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; unbunding libev and c-ares
                  (for-each delete-file-recursively '("libev" "c-ares"))
                  ;; fixing testsuite
                  (call-with-output-file "greentest/__init__.py" noop)
                  (substitute* "greentest/testrunner.py"
                    (("import util") "from . import util")
                    (("from util import log") "from .util import log"))))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-greenlet" ,python-greenlet)))
    (native-inputs
     `(("python-six" ,python-six)))
    (inputs
     `(("c-ares" ,c-ares)
       ("libev" ,libev)))
    (home-page "http://www.gevent.org/")
    (synopsis "Coroutine-based network library")
    (description
     "gevent is a coroutine-based Python networking library that uses greenlet
to provide a high-level synchronous API on top of the libev event loop.")
    (license license:expat)))

(define-public python2-gevent
  (package-with-python2 python-gevent))

(define-public python-fastimport
  (package
    (name "python-fastimport")
    (version "0.9.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fastimport" version))
        (sha256
          (base32 "1aqjsin4rmqm7ln4j0p73fzxifws6c6ikgyhav7r137m2ixsxl43"))))
    (build-system python-build-system)
    (home-page "https://github.com/jelmer/python-fastimport")
    (synopsis "VCS fastimport parser and generator in Python")
    (description "This package provides a parser for and generator of the Git
@url{https://www.kernel.org/pub/software/scm/git/docs/git-fast-import.html,fastimport}
format.")
    (license license:gpl2+)))

(define-public python2-fastimport
  (package-with-python2 python-fastimport))

(define-public python-twisted
  (package
    (name "python-twisted")
    (version "17.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Twisted" version ".tar.bz2"))
              (sha256
               (base32
                "1p245mg15hkxp7hy5cyq2fgvlgjkb4cg0gwkwd148nzy1bbi3wnv"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; FIXME: Some tests are failing.
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (replace 'check
       ;;     (lambda _
       ;;       (zero? (system* "./bin/trial" "twisted")))))
    (propagated-inputs
     `(("python-zope-interface" ,python-zope-interface)
       ("python-incremental" ,python-incremental)
       ("python-constantly" ,python-constantly)
       ("python-automat" ,python-automat)))
    (home-page "https://twistedmatrix.com/")
    (synopsis "Asynchronous networking framework written in Python")
    (description
     "Twisted is an extensible framework for Python programming, with special
focus on event-based network programming and multiprotocol integration.")
    (license license:expat)))

(define-public python2-twisted
  (package-with-python2 python-twisted))

(define-public python-pika
  (package
    (name "python-pika")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pika" version))
        (sha256
         (base32
          "0nb4h08di432lv7dy2v9kpwgk0w92f24sqc2hw2s9vwr5b8v8xvj"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pyev" ,python-pyev)
       ("python-tornado" ,python-tornado)
       ("python-twisted" ,python-twisted)))
    (home-page "https://pika.readthedocs.org")
    (synopsis "Pure Python AMQP Client Library")
    (description
     "Pika is a pure-Python implementation of the AMQP (Advanced Message Queuing
Protocol) 0-9-1 protocol that tries to stay fairly independent of the underlying
network support library.")
    (license license:bsd-3)))

(define-public python2-pika
  (package-with-python2 python-pika))

(define-public python-ply
  (package
    (name "python-ply")
    (version "3.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ply" version))
        (sha256
          (base32
            "0gpl0yli3w03ipyqfrp3w5nf0iawhsq65anf5wwm2wf5p502jzhd"))))
    (build-system python-build-system)
    (home-page "http://www.dabeaz.com/ply/")
    (synopsis "Python Lex & Yacc")
    (description "PLY is a @code{lex}/@code{yacc} implemented purely in Python.
It uses LR parsing and does extensive error checking.")
    (license license:bsd-3)))

(define-public python2-ply
  (package-with-python2 python-ply))

(define-public python-tabulate
  (package
    (name "python-tabulate")
    (version "0.7.7")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "tabulate" version))
             (sha256
              (base32
               "1inqhspd4frxnp08c32yndr0lc4px1xfkqah184i5w09gkhvi843"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: The pypi release tarball is missing a 'test/common.py'
     ;; and the latest release is not tagged in the upstream repository.
     '(#:tests? #f))
    (home-page "https://bitbucket.org/astanin/python-tabulate")
    (synopsis "Pretty-print tabular data")
    (description
     "Tabulate is a library and command-line utility to pretty-print tabular
data in Python.")
    (license license:expat)))

(define-public python2-tabulate
  (package-with-python2 python-tabulate))

(define-public python-kazoo
  (package
    (name "python-kazoo")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kazoo" version))
       (sha256
        (base32
         "10pb864if9qi2pq9lfb9m8f7z7ss6rml80gf1d9h64lap5crjnjj"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; XXX: needs zookeeper
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://kazoo.readthedocs.org")
    (synopsis "High-level Zookeeper client library")
    (description
     "Kazoo is a Python client library for the Apache Zookeeper distributed
application service.  It is designed to be easy to use and to avoid common
programming errors.")
    (license license:asl2.0)))

(define-public python2-kazoo
  (package-with-python2 python-kazoo))

(define-public python-pykafka
  (package
    (name "python-pykafka")
    (version "2.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://pypi.python.org/packages/8b/3e/"
                     "384eeff406b06315738b62483fd2126c6e4f544167116b17cc04ea7d2a59/"
                     "pykafka-" version ".tar.gz"))
              (sha256
               (base32
                "1id6sr159p6aa13bxcqyr9gln8sqg1l0ddzns5iws8kk5q1p5cfv"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; XXX: needs zookeeper, kafka, etc.
    (propagated-inputs
     `(("python-gevent" ,python-gevent)
       ("python-kazoo" ,python-kazoo)
       ("python-tabulate" ,python-tabulate)))
    (inputs
     `(("librdkafka" ,librdkafka)))
    (home-page "https://pykafka.readthedocs.io/")
    (synopsis "Apache Kafka client for Python")
    (description
     "PyKafka is a client for the Apache Kafka distributed messaging system.
It includes Python implementations of Kafka producers and consumers, which
are optionally backed by a C extension built on librdkafka.")
    (license license:asl2.0)))

(define-public python2-pykafka
  (package-with-python2 python-pykafka))

(define-public python-wcwidth
 (package
  (name "python-wcwidth")
  (version "0.1.7")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "wcwidth" version))
      (sha256
        (base32
          "0pn6dflzm609m4r3i8ik5ni9ijjbb5fa3vg1n7hn6vkd49r77wrx"))))
  (build-system python-build-system)
  (home-page "https://github.com/jquast/wcwidth")
  (synopsis "Measure number of terminal column cells of wide-character codes")
  (description "Wcwidth measures the number of terminal column cells of
wide-character codes.  It is useful for those implementing a terminal emulator,
or programs that carefully produce output to be interpreted by one.  It is a
Python implementation of the @code{wcwidth} and @code{wcswidth} C functions
specified in POSIX.1-2001 and POSIX.1-2008.")
  (license license:expat)))

(define-public python2-wcwidth
  (package-with-python2 python-wcwidth))

(define-public python2-jsonrpclib
  (package
    (name "python2-jsonrpclib")
    (version "0.1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pypi.python.org/packages/source/j/jsonrpclib/"
                    "jsonrpclib-" version ".tar.gz"))
              (sha256
               (base32
                "02vgirw2bcgvpcxhv5hf3yvvb4h5wzd1lpjx8na5psdmaffj6l3z"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:python ,python-2))
    (home-page "https://github.com/joshmarshall/jsonrpclib/")
    (synopsis "Implementation of JSON-RPC specification for Python")
    (description
     "This library is an implementation of the JSON-RPC specification.
It supports both the original 1.0 specification, as well as the
new (proposed) 2.0 spec, which includes batch submission, keyword arguments,
etc.")
    (license license:asl2.0)))

(define-public python-chai
  (package
    (name "python-chai")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "chai" version))
              (sha256
               (base32
                "016kf3irrclpkpvcm7q0gmkfibq7jgy30a9v73pp42bq9h9a32bl"))))
    (build-system python-build-system)
    (home-page "https://github.com/agoragames/chai")
    (synopsis "Mocking framework for Python")
    (description
     "Chai provides an api for mocking, stubbing and spying your python
objects, patterned after the Mocha library for Ruby.")
    (license license:bsd-3)))

(define-public python2-chai
  (package-with-python2 python-chai))

(define-public python-inflection
  (package
    (name "python-inflection")
    (version "0.3.1")
    (source
     (origin (method url-fetch)
             (uri (pypi-uri "inflection" version))
             (sha256
              (base32
               "1jhnxgnw8y3mbzjssixh6qkc7a3afc4fygajhqrqalnilyvpzshq"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/jpvanhal/inflection")
    (synopsis "Python string transformation library")
    (description
     "Inflection is a string transformation library.  It singularizes
and pluralizes English words, and transforms strings from CamelCase to
underscored string.")
    (license license:expat)))

(define-public python2-inflection
  (package-with-python2 python-inflection))

(define-public python-pylev
  (package
    (name "python-pylev")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pylev" version))
              (sha256
               (base32
                "1hz1x9blsbxya1y9nnhnwwdnqmakxi9mc0jkwj0rn6b1h44i0f86"))))
    (build-system python-build-system)
    (home-page "https://github.com/toastdriven/pylev")
    (synopsis "Levenshtein distance implementation in Python")
    (description "Pure Python Levenshtein implementation, based off the
Wikipedia code samples at
@url{http://en.wikipedia.org/wiki/Levenshtein_distance}.")
    (license license:bsd-3)))

(define-public python2-pylev
  (package-with-python2 python-pylev))

(define-public python-cleo
  (package
    (name "python-cleo")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cleo" version))
              (sha256
               (base32
                "0q1cf0szr0d54am4pypzwdnm74zpladdsinad94c2fz5i06fdpf7"))))
    (build-system python-build-system)
    (native-inputs
     `(;; For testing
       ("python-mock" ,python-mock)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-backpack" ,python-backpack)
       ("python-pastel" ,python-pastel)
       ("python-pylev" ,python-pylev)))
    (home-page "https://github.com/sdispater/cleo")
    (synopsis "Command-line arguments library for Python")
    (description
     "Cleo allows you to create command-line commands with signature in
docstring and colored output.")
    (license license:expat)))

(define-public python2-cleo
  (package-with-python2 python-cleo))

(define-public python-lazy-object-proxy
  (package
    (name "python-lazy-object-proxy")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lazy-object-proxy" version))
              (sha256
               (base32
                "0s22aqqkdscyh8sjspyyax7qa1aiz8p4midrnyf39717fhfczm6x"))))
    (build-system python-build-system)
    (home-page "https://github.com/ionelmc/python-lazy-object-proxy")
    (synopsis "Lazy object proxy for python")
    (description
     "Lazy object proxy is an object that wraps a callable but defers the call
until the object is actually required, and caches the result of said call.")
    (license license:bsd-2)))

(define-public python2-lazy-object-proxy
  (package-with-python2 python-lazy-object-proxy))

(define-public python-dnspython
  (package
  (name "python-dnspython")
  (version "1.15.0")
  (source (origin
            (method url-fetch)
            (uri (string-append "http://www.dnspython.org/kits/"
                                version "/dnspython-" version ".tar.gz"))
            (sha256
             (base32
              "0jr4v2pd90i6l1xxbss2m05psbjaxvyvvvpq44wycijpfgjqln8i"))))
  (build-system python-build-system)
  (arguments '(#:tests? #f)) ; XXX: requires internet access
  (home-page "http://www.dnspython.org")
  (synopsis "DNS toolkit for Python")
  (description
   "dnspython is a DNS toolkit for Python.  It supports almost all record
types.  It can be used for queries, zone transfers, and dynamic updates.
It supports TSIG authenticated messages and EDNS0.")
  (license license:expat)))

(define-public python2-dnspython
  (package-with-python2 python-dnspython))

(define-public python-email-validator
  (package
    (name "python-email-validator")
    (version "1.0.2")
    (source
     (origin (method url-fetch)
             (uri (pypi-uri "email_validator" version))
             (sha256
              (base32
               "1ja9149l9ck5n45a72h3is7v476hjny5ybxbcamx1nw6iplsm7k6"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-dnspython
           (lambda _
             (substitute* "setup.py"
               (("dnspython3") "dnspython"))
             #t)))))
    (propagated-inputs
     `(("python-dnspython" ,python-dnspython)
       ("python-idna" ,python-idna)))
    (home-page "https://github.com/JoshData/python-email-validator")
    (synopsis "Email address validation library for Python")
    (description
     "This library validates email address syntax and deliverability.")
    (license license:cc0)))

(define-public python2-email-validator
  (package-with-python2 python-email-validator))

(define-public python-ukpostcodeparser
  (package
    (name "python-ukpostcodeparser")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "UkPostcodeParser" version))
              (sha256
               (base32
                "1jwg9z4rz51mcka1821rwgycsd0mcicyp1kiwjfa2kvg8bm9p2qd"))))
    (build-system python-build-system)
    (home-page "https://github.com/hamstah/ukpostcodeparser")
    (synopsis "UK Postcode parser for Python")
    (description
     "This library provides the @code{parse_uk_postcode} function for
parsing UK postcodes.")
    (license license:expat)))

(define-public python2-ukpostcodeparser
  (package-with-python2 python-ukpostcodeparser))

(define-public python-faker
  (package
  (name "python-faker")
  (version "0.7.9")
  (source (origin
            (method url-fetch)
            (uri (pypi-uri "Faker" version))
            (sha256
             (base32
              "1fh2p2yz0fsdr4fqwxgddwbvfb6qn6vp8yx0qwqzra27yq5d1wsm"))
            (patches
             (search-patches "python-faker-fix-build-32bit.patch"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                (for-each delete-file (find-files "." "\\.pyc$"))
                #t))))
  (build-system python-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       (replace 'check
         (lambda _
           (zero? (system* "python" "-m" "unittest" "-v" "tests")))))))
  (native-inputs
   `(;; For testing
     ("python-email-validator" ,python-email-validator)
     ("python-mock" ,python-mock)
     ("python-ukpostcodeparser" ,python-ukpostcodeparser)))
  (propagated-inputs
   `(("python-dateutil" ,python-dateutil)
     ("python-six" ,python-six)))
  (home-page "https://github.com/joke2k/faker")
  (synopsis "Python package that generates fake data")
  (description
   "Faker is a Python package that generates fake data such as names,
addresses, and phone numbers.")
  (license license:expat)
  (properties `((python2-variant . ,(delay python2-faker))))))

(define-public python2-faker
  (let ((base (package-with-python2 (strip-python2-variant
                                     python-faker))))
    (package
      (inherit base)
      (propagated-inputs
       `(("python2-ipaddress" ,python2-ipaddress)
         ,@(package-propagated-inputs base))))))

(define-public python-pyaml
  (package
    (name "python-pyaml")
    (version "17.7.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyaml" version))
              (sha256
               (base32
                "132grrw0ajq4nrappi3ldbkb952k7yn9b6c7csi2rmvzm1g6ppp2"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-unidecode" ,python-unidecode)))
    (propagated-inputs
     `(("python-pyyaml" ,python-pyyaml)))
    (home-page "https://github.com/mk-fg/pretty-yaml")
    (synopsis "YAML pretty-print library for Python")
    (description
     "pyaml is a PyYAML based python module to produce pretty and readable
YAML-serialized data.")
    (license (license:non-copyleft "http://www.wtfpl.net/txt/copying/"))))

(define-public python2-pyaml
  (package-with-python2 python-pyaml))

(define-public python-backpack
  (package
    (name "python-backpack")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "backpack" version))
       (sha256
        (base32
         "14rq1mvm0jda90lcx9gyyby9dvq4x3js2cmxvd6vl4686ixwyqh1"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-simplejson" ,python-simplejson)))
    (home-page "https://github.com/sdispater/backpack")
    (synopsis "Utilities for working with Python collections")
    (description "Backpack provides some useful utilities for working with
collections of data.")
    (license license:expat)))

(define-public python2-backpack
  (package-with-python2 python-backpack))

(define-public python-prompt-toolkit
 (package
  (name "python-prompt-toolkit")
  (version "1.0.15")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "prompt_toolkit" version ".tar.gz"))
      (sha256
        (base32
          "05v9h5nydljwpj5nm8n804ms0glajwfy1zagrzqrg91wk3qqi1c5"))))
  (build-system python-build-system)
  (arguments
   '(#:tests? #f)) ; The test suite uses some Windows-specific data types.
  (propagated-inputs
   `(("python-wcwidth" ,python-wcwidth)
     ("python-six" ,python-six)
     ("python-pygments" ,python-pygments)))
  (home-page "https://github.com/jonathanslenders/python-prompt-toolkit")
  (synopsis "Library for building command line interfaces in Python")
  (description
    "Prompt-Toolkit is a library for building interactive command line
interfaces in Python.  It's like GNU Readline but it also features syntax
highlighting while typing, out-of-the-box multi-line input editing, advanced
code completion, incremental search, support for Chinese double-width
characters, mouse support, and auto suggestions.")
  (license license:bsd-3)))

(define-public python2-prompt-toolkit
  (package-with-python2 python-prompt-toolkit))

(define-public python-jedi
  (package
    (name "python-jedi")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jedi" version))
        (sha256
          (base32
            "0c8x962ynpx001fdvp07m2q5jk4igkxbj3rmnydavphvlgxijk1v"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: One test fails (use "py.test" instead of 'setup.py test').
     '(#:tests? #f))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/davidhalter/jedi")
    (synopsis
      "Autocompletion for Python that can be used for text editors")
    (description
      "Jedi is an autocompletion tool for Python that can be used for text editors.")
    (license license:expat)))

(define-public python2-jedi
  (package-with-python2 python-jedi))

(define-public ptpython
  (package
    (name "ptpython")
    (version "0.34")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ptpython" version))
              (sha256
               (base32
                "1mmbiyzf0n8hm7z2a562x7w5cbl6jc0zsk6vp40q1z4cyblv1k13"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; FIXME: No tests in pypi tarball.
    (propagated-inputs
     `(("python-docopt" ,python-docopt)
       ("python-jedi" ,python-jedi)
       ("python-prompt-toolkit" ,python-prompt-toolkit)
       ("python-pygments" ,python-pygments)))
    (home-page "https://github.com/jonathanslenders/ptpython")
    (synopsis "Python Read-Eval-Print-Loop with nice IDE-like features")
    (description
     "ptpython is a Python read-eval-print loop with IDE-like features.
It supports syntax highlighting, multiline editing, autocompletion, mouse,
color schemes, bracketed paste, Vi and Emacs keybindings, Chinese characters
etc.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay ptpython-2))))))

(define-public ptpython-2
  (let ((base (package-with-python2 (strip-python2-variant ptpython))))
    (package
      (inherit base)
      (name "ptpython2"))))

(define-public python-stem
  (package
    (name "python-stem")
    (version "1.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stem" version))
       (sha256
        (base32
         "1j7pnblrn0yr6jmxvsq6y0ihmxmj5x50jl2n2606w67f6wq16j9n"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "./run_tests.py" "--unit")))))))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pep8" ,python-pep8)
       ("python-pyflakes" ,python-pyflakes)))
    (home-page "https://stem.torproject.org/")
    (synopsis
     "Python controller library that allows applications to interact with Tor")
    (description
     "Stem is a Python controller library for Tor.  With it you can use Tor's
control protocol to script against the Tor process and read descriptor data
relays publish about themselves.")
    (license license:lgpl3)))

(define-public python2-stem
  (package-with-python2 python-stem))

(define-public python-pyserial
  (package
    (name "python-pyserial")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyserial" version))
        (sha256
          (base32
            "0k1nfdrxxkdlv4zgaqsdv8li0pj3gbh2pyxw8q2bsg6f9490amyn"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; FIXME: 3/49 tests are failing.
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (replace 'check
       ;;     (lambda _
       ;;       (zero? (system* "python" "test/run_all_tests.py" "loop://")))))))
    (home-page
      "https://github.com/pyserial/pyserial")
    (synopsis "Python Serial Port Bindings")
    (description "@code{pyserial} provide serial port bindings for Python.  It
supports different byte sizes, stop bits, parity and flow control with RTS/CTS
and/or Xon/Xoff.  The port is accessed in RAW mode.")
    (license license:bsd-3)))

(define-public python2-pyserial
  (package-with-python2 python-pyserial))

(define-public python-kivy
  (package
    (name "python-kivy")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kivy" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0zk3g1j1z0lzcm9d0k1lprrs95zr8n8k5pdg3p5qlsn26jz4bg19"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f              ; Tests require many optional packages
       #:phases
       (modify-phases %standard-phases
         (replace 'build (lambda _ (zero? (system* "make" "force"))))
         (add-after 'patch-generated-file-shebangs 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "KIVY_SDL2_PATH"
                     (string-append (assoc-ref inputs "sdl-union")
                                    "/include/SDL2"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-cython" ,python-cython)))
    (inputs
     `(("gstreamer" ,gstreamer)
       ("mesa" ,mesa)
       ("sdl-union"
        ,(sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf)))))
    (home-page "http://kivy.org")
    (synopsis
     "Multitouch application framework")
    (description
     "A software library for rapid development of
hardware-accelerated multitouch applications.")
    (license license:expat)))

(define-public python2-kivy
  (package-with-python2 python-kivy))

(define-public python-kivy-next
  (let ((commit "a988c5e7a47da56263ff39514264a3de516ef2fe")
        (revision "1"))
    (package (inherit python-kivy)
      (name "python-kivy-next")
      (version (string-append "1.9.1-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kivy/kivy")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0jk92b4a8l7blkvkgkjihk171s0dfnq582cckff5srwc8kal5m0p")))))))

(define-public python2-kivy-next
  (package-with-python2 python-kivy-next))

(define-public python-binaryornot
  (package
    (name "python-binaryornot")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "binaryornot" version))
              (sha256
               (base32
                "1j4f51dxic39mdwf6alj7gd769wy6mhk916v031wjali51xkh3xb"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-chardet" ,python-chardet)
       ("python-hypothesis" ,python-hypothesis)))
    (home-page "https://github.com/audreyr/binaryornot")
    (synopsis "Package to check if a file is binary or text")
    (description "Ultra-lightweight pure Python package to check if a file is
binary or text.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-binaryornot))))))

(define-public python2-binaryornot
  (let ((base (package-with-python2 (strip-python2-variant python-binaryornot))))
    (package (inherit base)
      (propagated-inputs
       `(("python2-enum34" ,python2-enum34)
         ,@(package-propagated-inputs base))))))

(define-public python-nltk
  (package
    (name "python-nltk")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "nltk" version))
              (sha256
               (base32
                "0skxbhnymwlspjkzga0f7x1hg3y50fwpfghs8g8k7fh6f4nknlym"))))
    (build-system python-build-system)
    (arguments
     '(;; The tests require some extra resources to be downloaded.
       ;; TODO Try packaging these resources.
       #:tests? #f))
    (home-page "http://nltk.org/")
    (synopsis "Natural Language Toolkit")
    (description "It provides interfaces to over 50 corpora and lexical
resources such as WordNet, along with a suite of text processing libraries
for classification, tokenization, stemming, tagging, parsing, and semantic
reasoning, wrappers for natural language processing libraries.")
    (license license:asl2.0)))

(define-public python2-nltk
  (package-with-python2 python-nltk))

(define-public python-pymongo
  (package
    (name "python-pymongo")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pymongo" version))
              (sha256
               (base32
                "07mra6w86wjqy4lx5fvimidjhhfzd562gfjn8grsnbv2q8pk0i9x"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-certifi" ,python-certifi)))
    (home-page "https://github.com/mongodb/mongo-python-driver")
    (synopsis "Python driver for MongoDB")
    (description "Python driver for MongoDB.")
    (license license:asl2.0)))

(define-public python2-pymongo
  (package-with-python2 python-pymongo))

(define-public python-sh
  (package
    (name "python-sh")
    (version "1.11")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sh" version))
              (sha256
               (base32
                "192r0mpv6dmkysjzhc43ddffiwb5g7c76bgr1mb1z2xz9awbj3sr"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (home-page "https://github.com/amoffat/sh")
    (synopsis "Python subprocess interface")
    (description "Abstracts process invocation by providing a function
interface for programs.")
    (license license:expat)))

(define-public python2-sh
  (package-with-python2 python-sh))

(define-public python-consul
  (package
    (name "python-consul")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-consul" version))
        (sha256
         (base32
          "0rfyxcy4cr3x848vhx876ifalxd5ghq6l5x813m49h4vq2d4jiq8"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (home-page "https://github.com/cablehead/python-consul")
    (synopsis "Python client for Consul")
    (description
     "Python client for @url{http://www.consul.io/,Consul}, a tool for service
discovery, monitoring and configuration.")
    (license license:expat)))

(define-public python2-consul
  (package-with-python2 python-consul))

(define-public python-schematics
  (package
    (name "python-schematics")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://github.com/schematics/schematics/archive/v" version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "19v1i69bf3bzarfxmbv0v6ivpcn758x3shvbiy9l2hy0lvqwnp6l"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (arguments
     `(#:tests? #f)) ; requires a bunch of not very nice packages with fixed
                     ; version requirements (eg python-coveralls)
    (home-page "https://github.com/schematics/schematics")
    (synopsis "Python Data Structures for Humans")
    (description "Python Data Structures for Humans.")
    (license license:bsd-3)))

(define-public python2-schematics
  (package-with-python2 python-schematics))

(define-public python-odfpy
  (package
    (name "python-odfpy")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "odfpy" version))
              (sha256
               (base32
                "1a6ms0w9zfhhkqhvrnynwwbxrivw6hgjc0s5k7j06npc7rq0blxw"))))
    (arguments
     `(#:modules ((srfi srfi-1)
                  (guix build python-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; The test runner invokes python2 and python3 for test*.py.
           ;; To avoid having both in inputs, we replicate it here.
           (lambda _
             (every (lambda (test-file)
                      (zero? (system* "python" test-file)))
                    (find-files "tests" "^test.*\\.py$")))))))
    (build-system python-build-system)
    (home-page "https://github.com/eea/odfpy")
    (synopsis "Python API and tools to manipulate OpenDocument files")
    (description "Collection of libraries and utility programs written in
Python to manipulate OpenDocument 1.2 files.")
    (license
     ;; The software is mainly dual GPL2+ and ASL2.0, but includes a
     ;; number of files with other licenses.
     (list license:gpl2+ license:asl2.0 license:lgpl2.1+ license:cc-by-sa3.0))))

(define-public python2-odfpy
  (package-with-python2 python-odfpy))

(define-public python-natsort
  (package
    (name "python-natsort")
    (version "5.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "natsort" version))
              (sha256
               (base32
                "0bh6j0l8iapjnsgg3bs6q075cnzjl6zw1vlgqyv3qrygm2cxypkn"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-cachedir
           ;; Tests require write access to $HOME by default
           (lambda _ (setenv "PYTHON_EGG_CACHE" "/tmp") #t)))))
    (native-inputs
     `(("python-hypothesis" ,python-hypothesis)
       ("python-pytest-cache" ,python-pytest-cache)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-flakes" ,python-pytest-flakes)
       ("python-pytest-pep8" ,python-pytest-pep8)))
    (propagated-inputs ; TODO: Add python-fastnumbers.
     `(("python-pyicu" ,python-pyicu)))
    (home-page "https://github.com/SethMMorton/natsort")
    (synopsis "Natural sorting for python and shell")
    (description
     "Natsort lets you apply natural sorting on lists instead of
lexicographical.  If you use the built-in @code{sorted} method in python
on a list such as @code{['a20', 'a9', 'a1', 'a4', 'a10']}, it would be
returned as @code{['a1', 'a10', 'a20', 'a4', 'a9']}.  Natsort provides a
function @code{natsorted} that identifies numbers and sorts them separately
from strings.  It can also sort version numbers, real numbers, mixed types
and more, and comes with a shell command @command{natsort} that exposes this
functionality in the command line.")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-natsort))))))

(define-public python2-natsort
  (let ((base (package-with-python2 (strip-python2-variant python-natsort))))
    (package (inherit base)
             (native-inputs
              `(("python2-pathlib" ,python2-pathlib)
                ("python2-mock" ,python2-mock)
                ("python2-enum34" ,python2-enum34)
                ,@(package-native-inputs base))))))

(define-public python-glances
  (package
  (name "python-glances")
  (version "2.7.1")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "Glances" version))
      (sha256
        (base32
          "11jbq40g8alsbirnd4kiagznqg270247i0m8qhi48ldf2i5xppxg"))))
  (build-system python-build-system)
  (propagated-inputs
   `(("python-psutil" ,python-psutil)))
  (home-page
    "https://github.com/nicolargo/glances")
  (synopsis
    "A cross-platform curses-based monitoring tool")
  (description
    "Glances is a curses-based monitoring tool for a wide variety of platforms.
Glances uses the PsUtil library to get information from your system. It monitors
CPU, load, memory, network bandwidth, disk I/O, disk use, and more.")
  (license license:lgpl3+)))

(define-public python2-glances
  (package-with-python2 python-glances))

(define-public python-graphql-core
  (package
    (name "python-graphql-core")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "graphql-core" version))
        (sha256
         (base32
          "0rsaarx2sj4xnw9966rhh4haiqaapm4lm2mfqm48ywd51j5vh1a0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Tests require the unpackaged pytest-benchmark.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-hardcoded-version
           (lambda _ (substitute*
                       "setup.py"
                       (("'gevent==1.1rc1'") "'gevent'"))
             #t)))))
    (native-inputs
     `(("python-gevent" ,python-gevent)
       ("python-mock" ,python-mock)
       ("python-pytest-mock" ,python-pytest-mock)))
    (propagated-inputs
     `(("python-promise" ,python-promise)
       ("python-six" ,python-six)))
    (home-page "https://github.com/graphql-python/graphql-core")
    (synopsis "GraphQL implementation for Python")
    (description
     "GraphQL implementation for Python.  GraphQL is a data query language and
runtime designed and used to request and deliver data to mobile and web apps.
This library is a port of @url{https://github.com/graphql/graphql-js,graphql-js}
to Python.")
    (license license:expat)))

(define-public python2-graphql-core
  (package-with-python2 python-graphql-core))

(define-public python-graphql-relay
  (package
    (name "python-graphql-relay")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "graphql-relay" version))
        (sha256
         (base32
          "1nv5dxcj59zv31qvl8bd142njmxcmymny2dz3br1l2cpbljbf5i7"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-graphql-core" ,python-graphql-core)
       ("python-promise" ,python-promise)
       ("python-six" ,python-six)))
    (home-page "https://github.com/graphql-python/graphql-relay-py")
    (synopsis "Relay implementation for Python")
    (description
     "This is a library to allow the easy creation of Relay-compliant servers
using the GraphQL Python reference implementation of a GraphQL server.  It
should be noted that the code is a exact port of the original
@url{https://github.com/graphql/graphql-relay-js,graphql-relay js implementation}
from Facebook.")
    (license license:expat)))

(define-public python2-graphql-relay
  (package-with-python2 python-graphql-relay))

(define-public python-graphene
  (package
    (name "python-graphene")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "graphene" version))
        (sha256
         (base32
          "09zhac7igh9ixdz0ay6csy35b40l1jwbf2wrbxmgxwfhy51iy06q"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-graphql-core" ,python-graphql-core)
       ("python-graphql-relay" ,python-graphql-relay)
       ("python-iso8601" ,python-iso8601)
       ("python-promise" ,python-promise)
       ("python-six" ,python-six)))
    (arguments
     `(#:tests? #f))                    ; no tests/ in the PyPI tarball
    (home-page "http://graphene-python.org/")
    (synopsis "GraphQL Framework for Python")
    (description
     "Graphene is a Python library for building GraphQL schemas/types.
A GraphQL schema describes your data model, and provides a GraphQL server
with an associated set of resolve methods that know how to fetch data.")
    (properties `((python2-variant . ,(delay python2-graphene))))
    (license license:expat)))

(define-public python2-graphene
  (let ((base (package-with-python2
                (strip-python2-variant python-graphene))))
    (package (inherit base)
      (native-inputs
       `(("python2-sqlalchemy" ,python2-sqlalchemy)
         ,@(package-native-inputs base))))))

(define-public python-nautilus
  (package
    (name "python-nautilus")
    (version "0.4.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nautilus" version))
        (sha256
         (base32
          "01hwzjc1zshk4vvxrcghm398fpy4jls66dyz06g07mrwqif8878p"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; fails to import test modules
    (propagated-inputs
     `(("python-bcrypt" ,python-bcrypt)
       ("python-click" ,python-click)
       ("python-consul" ,python-consul)
       ("python-graphene" ,python-graphene)
       ("python-jinja2" ,python-jinja2)
       ("python-peewee" ,python-peewee)
       ("python-pika" ,python-pika)
       ("python-tornado" ,python-tornado)
       ("python-wtforms" ,python-wtforms)))
    (native-inputs
     `(("python-nose2" ,python-nose2)))
    (home-page "https://github.com/AlecAivazis/nautilus")
    (synopsis "Library for creating microservice applications")
    (description
     "Nautilus is a framework for flux based microservices that looks to
provide extendible implementations of common aspects of a cloud so that you can
focus on building massively scalable web applications.")
    (license license:expat)))

(define-public python-snowballstemmer
  (package
    (name "python-snowballstemmer")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "snowballstemmer" version))
              (sha256
               (base32
                "0a0idq4y5frv7qsg2x62jd7rd272749xk4x99misf5rcifk2d7wi"))))
    (build-system python-build-system)
    (arguments
     `(;; No tests exist
       #:tests? #f))
    (home-page "https://github.com/shibukawa/snowball_py")
    (synopsis "Snowball stemming library collection for Python")
    (description "This package provides 16 word stemmer algorithms generated
from Snowball algorithms.  It includes the 15 original ones plus the Poerter
English stemmer.")
    (license license:bsd-3)))

(define-public python2-snowballstemmer
  (package-with-python2 python-snowballstemmer))

(define-public python-sphinx-cloud-sptheme
  (package
    (name "python-sphinx-cloud-sptheme")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cloud_sptheme" version))
              (sha256
               (base32
                "1dniqb6a39yh786f86c4jn666rwnyi1jvzn4616zhcchb7sfdshd"))))
    (build-system python-build-system)
    ;; FIXME: The 'pypi' release archive does not contain tests.
    (arguments '(#:tests? #f))
    (native-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://bitbucket.org/ecollins/cloud_sptheme")
    (synopsis "'Cloud' theme for Sphinx documenter")
    (description "This package contains the \"Cloud\" theme for Sphinx and some
related extensions.")
    (license license:bsd-3)))

(define-public python2-sphinx-cloud-sptheme
  (package-with-python2 python-sphinx-cloud-sptheme))

(define-public python-sphinx-alabaster-theme
  (package
    (name "python-sphinx-alabaster-theme")
    (version "0.7.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "alabaster" version))
              (sha256
               (base32
                "027anxzcb951gjlcc43y3rbn9qrw36d16vj9wd2smv5410xx9bs7"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pygments" ,python-pygments)))
    (home-page "https://alabaster.readthedocs.io/")
    (synopsis "Configurable sidebar-enabled Sphinx theme")
    (description "Alabaster is a visually (c)lean, responsive, configurable
theme for the Sphinx documentation system.  It's the default theme of Sphinx.")
    (license license:bsd-3)))

(define-public python2-sphinx-alabaster-theme
  (package-with-python2 python-sphinx-alabaster-theme))

(define-public python-setproctitle
(package
  (name "python-setproctitle")
  (version "1.1.10")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "setproctitle" version))
      (sha256
        (base32
          "163kplw9dcrw0lffq1bvli5yws3rngpnvrxrzdw89pbphjjvg0v2"))))
  (build-system python-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
        (add-before 'check 'patch-Makefile
           ;; Stricly this is only required for the python2 variant.
           ;; But adding a phase in an inherited package seems to be
           ;; cumbersum. So we patch even for python3.
           (lambda _
             (let ((nose (assoc-ref %build-inputs "python2-nose")))
               (when nose
                 (substitute* "Makefile"
                   (("\\$\\(PYTHON\\) [^ ]which nosetests[^ ] ")
                    (string-append nose "/bin/nosetests "))))
               #t)))
        (replace 'check
           (lambda _
             (setenv "PYTHON" (or (which "python3") (which "python")))
             (setenv "PYCONFIG" (or (which "python3-config")
                                    (which "python-config")))
             (setenv "CC" "gcc")
             ;; No need to extend PYTHONPATH to find the built package, since
             ;; the Makefile will build anyway
             (zero? (system* "make" "check")))))))
  (native-inputs
   `(("procps" ,procps))) ; required for tests
  (home-page
    "https://github.com/dvarrazzo/py-setproctitle")
  (synopsis
   "Setproctitle implementation for Python to customize the process title")
  (description "The library allows a process to change its title (as displayed
by system tools such as ps and top).

Changing the title is mostly useful in multi-process systems, for
example when a master process is forked: changing the children's title
allows to identify the task each process is busy with.  The technique
is used by PostgreSQL and the OpenSSH Server for example.")
  (license license:bsd-3)
  (properties `((python2-variant . ,(delay python2-setproctitle))))))

(define-public python2-setproctitle
  (let ((base (package-with-python2
               (strip-python2-variant python-setproctitle))))
    (package
      (inherit base)
      (native-inputs `(("python2-nose" ,python2-nose)
                       ,@(package-native-inputs base))))))

(define-public python-validictory
  (package
    (name "python-validictory")
    (version "1.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "validictory" version))
      (sha256
       (base32
        "1zf1g9sw47xzp5f80bd94pb42j9yqv82lcrgcvdwr6nkaphfi37q"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           ;; Move the tests out of the package directory to avoid
           ;; packaging them.
           (lambda* _
             (rename-file "validictory/tests" "tests")
             (delete-file "tests/__init__.py")))
         (replace 'check
           (lambda _
             ;; Extend PYTHONPATH so the built package will be found.
             (setenv "PYTHONPATH"
                     (string-append (getcwd) "/build/lib:"
                                    (getenv "PYTHONPATH")))
             (zero? (system* "py.test" "-vv" )))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page
     "https://github.com/jamesturk/validictory")
    (synopsis "General purpose Python data validator")
    (description "It allows validation of arbitrary Python data structures.

The schema format is based on the JSON Schema
proposal (http://json-schema.org), so combined with json the library is also
useful as a validator for JSON data.")
  (license license:expat)))

(define-public python2-validictory
  (package-with-python2 python-validictory))

(define-public python-pyev
  (package
    (name "python-pyev")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyev" version))
        (sha256
         (base32
          "0rf603lc0s6zpa1nb25vhd8g4y337wg2wyz56i0agsdh7jchl0sx"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libev (string-append (assoc-ref inputs "libev")
                                         "/lib/libev.so.4")))
               (substitute* "setup.py"
                 (("libev_dll_name = find_library\\(\\\"ev\\\"\\)")
                  (string-append "libev_dll_name = \"" libev "\"")))))))))
    (inputs
     `(("libev" ,libev)))
    (home-page "http://pythonhosted.org/pyev/")
    (synopsis "Python libev interface")
    (description "Pyev provides a Python interface to libev.")
    (license license:gpl3)))

(define-public python2-pyev
  (package-with-python2 python-pyev))

(define-public python-imagesize
  (package
    (name "python-imagesize")
    (version "0.7.1")
    (source
      (origin
      (method url-fetch)
      (uri (pypi-uri "imagesize" version))
      (sha256
        (base32
          "0qk07k0z4241lkzzjji7z4da04pcvg7bfc4xz1934zlqhwmwdcha"))))
    (build-system python-build-system)
    (arguments
     '(;; Test files are not distributed on PyPi:
       ;; https://github.com/shibukawa/imagesize_py/issues/7
       #:tests? #f))
    (home-page "https://github.com/shibukawa/imagesize_py")
    (synopsis "Gets image size of files in variaous formats in Python")
    (description
      "This package allows determination of image size from
PNG, JPEG, JPEG2000 and GIF files in pure Python.")
    (license license:expat)))

(define-public python2-imagesize
 (package-with-python2 python-imagesize))

(define-public python-termstyle
  (package
    (name "python-termstyle")
    (version "0.1.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "termstyle" version))
        (sha256
          (base32
            "17wzkkcqy5zc0g68xlad3kcv66iw14d2pwqc0h9420gak0vbhx7g"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "python" "test3.py")))))))
    (home-page "https://github.com/gfxmonk/termstyle")
    (synopsis "Console text coloring for Python")
    (description "This package provides console text coloring for Python.")
    (license license:bsd-3)))

(define-public python-argcomplete
  (package
    (name "python-argcomplete")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "argcomplete" version))
        (sha256
          (base32
            "11bwiw6j0nilgz81xnw6f1npyga3prp8asjqrm87cdr3ria5l03x"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pexpect" ,python-pexpect)
       ("tcsh" ,tcsh)
       ("bash-full" ,bash)))             ;full Bash for 'test_file_completion'
    (home-page "https://github.com/kislyuk/argcomplete")
    (synopsis "Shell tab completion for Python argparse")
    (description "argcomplete provides extensible command line tab completion
of arguments and options for Python scripts using @code{argparse}.  It's
particularly useful for programs with many options or sub-parsers that can
dynamically suggest completions; for example, when browsing resources over the
network.")
    (license license:asl2.0)))

(define-public python2-argcomplete
  (package-with-python2 python-argcomplete))

(define-public python-xopen
  (package
    (name "python-xopen")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xopen" version))
        (sha256
          (base32
           "1wx6mylzcsyhjl19ycb83qq6iqpmr927lz62njfsar6ldsj0qcni"))
        (file-name (string-append name "-" version ".tar.gz"))))
    (build-system python-build-system)
    (home-page "https://github.com/marcelm/xopen/")
    (synopsis "Open compressed files transparently")
    (description "This module provides an @code{xopen} function that works like
Python's built-in @code{open} function, but can also deal with compressed files.
Supported compression formats are gzip, bzip2 and, xz, and are automatically
recognized by their file extensions.  The focus is on being as efficient as
possible on all supported Python versions.")
    (license license:expat)))

(define-public python2-xopen
  (package-with-python2 python-xopen))

(define-public python2-cheetah
  (package
    (name "python2-cheetah")
    (version "2.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Cheetah" version))
        (sha256
          (base32
            "0l5mm4lnysjkzpjr95q5ydm9xc8bv43fxmr79ypybrf1y0lq4c5y"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     `(("python2-markdown" ,python2-markdown)))
    (home-page "https://pythonhosted.org/Cheetah/")
    (synopsis "Template engine")
    (description "Cheetah is a text-based template engine and Python code
generator.

Cheetah can be used as a standalone templating utility or referenced as
a library from other Python applications.  It has many potential uses,
but web developers looking for a viable alternative to ASP, JSP, PHP and
PSP are expected to be its principle user group.

Features:
@enumerate
@item Generates HTML, SGML, XML, SQL, Postscript, form email, LaTeX, or any other
   text-based format.
@item Cleanly separates content, graphic design, and program code.
@item Blends the power and flexibility of Python with a simple template language
   that non-programmers can understand.
@item Gives template writers full access to any Python data structure, module,
   function, object, or method in their templates.
@item Makes code reuse easy by providing an object-orientated interface to
   templates that is accessible from Python code or other Cheetah templates.
   One template can subclass another and selectively reimplement sections of it.
@item Provides a simple, yet powerful, caching mechanism that can dramatically
   improve the performance of a dynamic website.
@item Compiles templates into optimized, yet readable, Python code.
@end enumerate")
    (license (license:x11-style "file://LICENSE"))))

(define-public python-dulwich
  (package
    (name "python-dulwich")
    (version "0.18.6")
    (source
      (origin
        (method url-fetch)
        (uri (list (string-append "https://www.dulwich.io/releases/"
                            "dulwich-" version ".tar.gz")
                   (pypi-uri "dulwich" version)))
        (sha256
          (base32
           "1aa1xfrxkc3j9s4xi0llhf5gndyi9ryprcxsqfa5fcb8ph34981q"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The tests use Popen with a custom environment which doesn't
             ;; include PATH.
             (substitute* "dulwich/tests/compat/utils.py"
               (("'git'") (string-append "'"
                                         (which "git")
                                         "'")))
             (substitute* '("dulwich/tests/test_repository.py"
                            "dulwich/tests/test_hooks.py")
               (("#!/bin/sh") (string-append "#!" (which "sh"))))
             (setenv "TEST_RUNNER" "unittest")
             (setenv "PYTHONHASHSEED" "random")
             #t)))))
    (propagated-inputs
     `(("python-fastimport" ,python-fastimport)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-geventhttpclient" ,python-geventhttpclient)
       ("git" ,git)))
    (home-page "https://www.dulwich.io/")
    (synopsis "Git implementation in Python")
    (description "Dulwich is an implementation of the Git file formats and
protocols written in pure Python.")
    ;; Can be used with either license.
    (license (list license:asl2.0 license:gpl2+))))

(define-public python2-dulwich
  (package-with-python2 python-dulwich))

(define-public python-pbkdf2
  (package
    (name "python-pbkdf2")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pbkdf2" version))
       (sha256
        (base32
         "0yb99rl2mbsaamj571s1mf6vgniqh23v98k4632150hjkwv9fqxc"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append (getcwd) "/build/lib:"
                                    (getenv "PYTHONPATH")))
             (zero? (system* "python" "test/test_pbkdf2.py")))))))
    (propagated-inputs
     `(("python-pycrypto" ,python-pycrypto)))  ; optional
    (home-page "https://www.dlitz.net/software/python-pbkdf2/")
    (synopsis "Password-based key derivation")
    (description "This module implements the password-based key derivation
function, PBKDF2, specified in RSA PKCS#5 v2.0.

PKCS#5 v2.0 Password-Based Key Derivation is a key derivation function which
is part of the RSA Public Key Cryptography Standards series.  The provided
implementation takes a password or a passphrase and a salt value (and
optionally a iteration count, a digest module, and a MAC module) and provides
a file-like object from which an arbitrarly-sized key can be read.")
    (license license:expat)))

(define-public python2-pbkdf2
  (package-with-python2 python-pbkdf2))

(define-public python-qrcode
  (package
    (name "python-qrcode")
    (version "5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "qrcode" version))
       (sha256
        (base32
         "0kljfrfq0c2rmxf8am57333ia41kd0snbm2rnqbdy816hgpcq5a1"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests require packaging 'pymaging'.
     '(#:tests? #f))
    (propagated-inputs
     `(("python-lxml" ,python-lxml)     ; for SVG output
       ("python-pillow" ,python-pillow) ; for PNG output
       ("python-six" ,python-six)))
    (home-page "https://github.com/lincolnloop/python-qrcode")
    (synopsis "QR Code image generator")
    (description "This package provides a pure Python QR Code generator
module.  It uses the Python Imaging Library (PIL) to allow for the generation
of QR Codes.

In addition this package provides a command line tool to generate QR codes and
either write these QR codes to a file or do the output as ascii art at the
console.")
    (license license:bsd-3)))

(define-public python2-qrcode
  (package-with-python2 python-qrcode))

(define-public python-rst2ansi
  (package
    (name "python-rst2ansi")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rst2ansi" version))
       (sha256
        (base32
         "0vzy6gd60l79ff750scl0sz48r1laalkl6md6dwzah4dcadgn5qv"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-docutils" ,python-docutils)))
    (home-page "https://github.com/Snaipe/python-rst-to-ansi")
    (synopsis "Convert RST to ANSI-decorated console output")
    (description
     "Python module dedicated to rendering RST (reStructuredText) documents
to ansi-escaped strings suitable for display in a terminal.")
    (license license:expat)))

(define-public python-ansi2html
  (package
    (name "python-ansi2html")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ansi2html" version))
       (sha256
        (base32
         "1wa00zffprb78w1mqq90dk47czz1knanys2a40zbw2vyapd5lp9y"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/ralphbean/ansi2html")
    (synopsis "Convert ANSI-decorated console output to HTML")
    (description
     "@command{ansi2html} is a Python library and command line utility for
convering text with ANSI color codes to HTML or LaTeX.")
    (license license:gpl3+)))

(define-public python2-ansi2html
  (package-with-python2 python-ansi2html))

(define-public python-ddt
  (package
    (name "python-ddt")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ddt" version))
       (sha256
        (base32
         "1c00ikkxr7lha97c81k938bzhgd4pbwamkjn0h4nkhr3xk00zp6n"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-pyyaml" ,python-pyyaml)))
    (home-page "https://github.com/txels/ddt")
    (synopsis "Data-Driven Tests")
    (description
     "DDT (Data-Driven Tests) allows you to multiply one test case by running
it with different test data, and make it appear as multiple test cases.")
    (license license:expat)))

(define-public python2-ddt
  (package-with-python2 python-ddt))

(define-public python-pycosat
  (package
    (name "python-pycosat")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycosat" version))
       (sha256
        (base32
         "1kl3wh1f47rc712n4bmwplbx3fqz3x9i1b587jrbpmvdva4c8f6l"))))
    ;; TODO: Unundle picosat. http://fmv.jku.at/picosat/
    (build-system python-build-system)
    (home-page "https://github.com/ContinuumIO/pycosat")
    (synopsis "Bindings to picosat (a SAT solver)")
    (description
     "This package provides efficient Python bindings to @code{picosat} on
the C level.  When importing pycosat, the @code{picosat} solver becomes part
of the Python process itself.  @code{picosat} is a @dfn{Boolean Satisfiability
Problem} (SAT) solver.")
    (license license:expat)))

(define-public python2-pycosat
  (package-with-python2 python-pycosat))

(define-public python2-ruamel.ordereddict
  (package
    (name "python2-ruamel.ordereddict")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ruamel.ordereddict" version))
       (sha256
        (base32
         "1xmkl8v9l9inm2pyxgc1fm5005yxm7fkd5gv74q7lj1iy5qc8n3h"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (zero? (system* "python" "test/testordereddict.py")))))))
    (home-page "https://bitbucket.org/ruamel/ordereddict")
    (synopsis "Version of dict that keeps keys in insertion order")
    (description
     "This is an implementation of an ordered dictionary with @dfn{Key
Insertion Order} (KIO: updates of values do not affect the position of the
key), @dfn{Key Value Insertion Order} (KVIO, an existing key's position is
removed and put at the back).  The standard library module @code{OrderedDict},
implemented later, implements a subset of @code{ordereddict} functionality.
Sorted dictionaries are also provided.  Currently only with @dfn{Key Sorted
Order} (KSO, no sorting function can be specified, but a transform can be
specified to apply on the key before comparison (e.g. @code{string.lower})).")
    (license license:expat)))

(define-public python-pypeg2
  (package
    (name "python-pypeg2")
    (version "2.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyPEG2" version))
       (sha256
        (base32
         "0v8ziaam2r637v94ra4dbjw6jzxz99gs5x4i585kgag1v204yb9b"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-lxml" ,python-lxml)))
    (arguments
     ;;https://bitbucket.org/fdik/pypeg/issues/36/test-failures-on-py35
     '(#:tests? #f))
    (home-page "https://fdik.org/pyPEG/")
    (synopsis "Parsering Expression Grammars in Python")
    (description "PyPEG is an intrinsic parser interpreter framework for
Python.  It is based on Parsing Expression Grammars, PEG.  With pyPEG you can
parse many formal languages.")
    (license license:gpl2)))

(define-public python-incremental
  (package
    (name "python-incremental")
    (version "17.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "incremental" version))
       (sha256
        (base32
         "1cylxdz1cnkm5g3pklna3h2n0i0rks95ir1pnpxfnvpkmab1cxbv"))))
    (build-system python-build-system)
    (home-page "https://github.com/hawkowl/incremental")
    (synopsis "Library for versioning Python projects")
    (description "Incremental is a small library that versions your Python
projects.")
    (license license:expat)))

(define-public python2-incremental
  (package-with-python2 python-incremental))

(define-public python-automat
  (package
    (name "python-automat")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Automat" version))
              (sha256
               (base32
                "1a7nsrljysfmdqmpn2apfa1gg6rfah4y9sizvns8gb08rx7d07rw"))))
    (build-system python-build-system)
    ;; We disable the tests because they require python-twisted, while
    ;; python-twisted depends on python-automat.  Twisted is optional, but the
    ;; tests fail if it is not available.  Also see
    ;; <https://github.com/glyph/automat/issues/71>.
    (arguments '(#:tests? #f))
    (native-inputs
     `(("python-m2r" ,python-m2r)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-graphviz" ,python-graphviz)))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-attrs" ,python-attrs)))
    (home-page "https://github.com/glyph/Automat")
    (synopsis "Self-service finite-state machines")
    (description "Automat is a library for concise, idiomatic Python
expression of finite-state automata (particularly deterministic finite-state
transducers).")
    (license license:expat)))

(define-public python2-automat
  (package-with-python2 python-automat))

(define-public python-m2r
  (package
    (name "python-m2r")
    (version "0.1.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "m2r" version))
              (sha256
               (base32
                "1axrwnf425sz4qz3c0qc7yhhki4myzb8rki7pczcsgzznzmqdyxd"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-docutils" ,python-docutils)
       ("python-mistune" ,python-mistune)))
    (native-inputs
     `(("python-pygments" ,python-pygments)
       ("python-mock" ,python-mock)))
    (home-page "https://github.com/miyakogi/m2r")
    (synopsis "Markdown to reStructuredText converter")
    (description "M2R converts a markdown file including reST markups to valid
reST format.")
    (license license:expat)))

(define-public python2-m2r
  (package-with-python2 python-m2r))

(define-public python-constantly
  (package
    (name "python-constantly")
    (version "15.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "constantly" version))
              (sha256
               (base32
                "0dgwdla5kfpqz83hfril716inm41hgn9skxskvi77605jbmp4qsq"))))
    (build-system python-build-system)
    (home-page "https://github.com/twisted/constantly")
    (synopsis "Symbolic constants in Python")
    (description "Constantly is a Python library that provides symbolic
constant support.  It includes collections and constants with text, numeric,
and bit flag values.")
    (license license:expat)))

(define-public python2-constantly
  (package-with-python2 python-constantly))

(define-public python-attrs
  (package
    (name "python-attrs")
    (version "17.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "attrs" version))
              (sha256
               (base32
                "04gx08ikpk26wnq22f7l42gapcvk8iz1512r927k6sadz6cinkax"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-hypothesis" ,python-hypothesis)
       ("python-zope-interface" ,python-zope-interface)
       ("python-six" ,python-six)))
    (home-page "https://github.com/python-attrs/attrs/")
    (synopsis "Attributes without boilerplate")
    (description "@code{attrs} is a Python package with class decorators that
ease the chores of implementing the most common attribute-related object
protocols.")
    (license license:expat)))

(define-public python2-attrs
  (package-with-python2 python-attrs))

(define-public python2-cliapp
  (package
    (name "python2-cliapp")
    (version "1.20170823")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://git.liw.fi/cgi-bin/cgit/cgit.cgi/cliapp/snapshot/cliapp-"
             version ".tar.gz"))
       (sha256
        (base32
         "1i9gik0xrj6jmi95s5w988jl1y265baz5xm5pbqdyvsh8h9ln6yq"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         ;; check phase needs to be run before the build phase. If not,
         ;; coverage-test-runner looks for tests for the built source files,
         ;; and fails.
         (delete 'check)
         (add-before 'build 'check
           (lambda _
             ;; Disable python3 tests
             (substitute* "check"
               (("python3") "# python3"))
             (zero? (system* "./check")))))))
    (native-inputs
     `(("python2-coverage-test-runner" ,python2-coverage-test-runner)
       ("python2-pep8" ,python2-pep8)))
    (propagated-inputs
     `(("python2-pyaml" ,python2-pyaml)))
    (home-page "https://liw.fi/cliapp/")
    (synopsis "Python framework for command line programs")
    (description "@code{python2-cliapp} is a python framework for
command line programs.  It contains the typical stuff such programs
need to do, such as parsing the command line for options, and
iterating over input files.")
    (license license:gpl2+)))

(define-public python2-ttystatus
  (package
    (name "python2-ttystatus")
    (version "0.35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://git.liw.fi/cgi-bin/cgit/cgit.cgi/ttystatus/snapshot/ttystatus-"
             version ".tar.gz"))
       (sha256
        (base32
         "0vivqbw7ddhsq1zj3g9cvvv4f0phl0pis2smsnwcr2szz2fk3hl6"))))
    (build-system python-build-system)
    (native-inputs
     `(("python2-coverage-test-runner" ,python2-coverage-test-runner)
       ("python2-pep8" ,python2-pep8)))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         ;; check phase needs to be run before the build phase. If not,
         ;; coverage-test-runner looks for tests for the built source files,
         ;; and fails.
         (delete 'check)
         (add-before 'build 'check
           (lambda _
             (zero? (system* "make" "check")))))))
    (home-page "https://liw.fi/ttystatus/")
    (synopsis "Python library for showing progress reporting and
status updates on terminals")
    (description "@code{python2-ttystatus} is a python library for
showing progress reporting and status updates on terminals, for
command line programs.  Output is automatically adapted to the width
of the terminal: truncated if it does not fit, and resized if the
terminal size changes.")
    (license license:gpl3+)))

(define-public python2-tracing
  (package
    (name "python2-tracing")
    (version "0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://git.liw.fi/cgi-bin/cgit/cgit.cgi/python-tracing/snapshot/tracing-"
             version ".tar.gz"))
       (sha256
        (base32
         "06cw4zg42fsvqy372vi2whj26w56vzg5axhzwdjc2bgwf03garbw"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (home-page "https://liw.fi/tracing/")
    (synopsis "Python debug logging helper")
    (description "@code{python2-tracing} is a python library for
logging debug messages.  It provides a way to turn debugging messages
on and off, based on the filename they occur in.  It is much faster
than using @code{logging.Filter} to accomplish the same thing, which
matters when code is run in production mode.  The actual logging still
happens using the @code{logging} library.")
    (license license:gpl3+)))

(define-public python2-larch
  (package
    (name "python2-larch")
    (version "1.20151025")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://git.liw.fi/cgi-bin/cgit/cgit.cgi/larch/snapshot/larch-"
             version ".tar.gz"))
       (patches (search-patches
                 "python2-larch-coverage-4.0a6-compatibility.patch"))
       (sha256
        (base32
         "1p4knkkavlqymgciz2wbcnfrdgdbafhg14maplnk4vbw0q8xs663"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         ;; check phase needs to be run before the build phase. If not,
         ;; coverage-test-runner looks for tests for the built source files,
         ;; and fails.
         (delete 'check)
         (add-before 'build 'check
           (lambda _
             (zero? (system* "make" "check")))))))
    (native-inputs
     `(("cmdtest" ,cmdtest)
       ("python2-coverage-test-runner" ,python2-coverage-test-runner)))
    (propagated-inputs
     `(("python2-tracing" ,python2-tracing)))
    (home-page "https://liw.fi/larch/")
    (synopsis "Python copy-on-write B-tree library")
    (description "@code{python2-larch} is an implementation of
particular kind of B-tree, based on research by Ohad Rodeh.  See
@url{http://liw.fi/larch/ohad-btrees-shadowing-clones.pdf} for details
on the data structure.

The distinctive feature of this B-tree is that a node is never
(conceptually) modified.  Instead, all updates are done by
copy-on-write.  This makes it easy to clone a tree, and modify only the
clone, while other processes access the original tree.")
    (license license:gpl3+)))

(define-public python-astroid
  (package
    (name "python-astroid")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/PyCQA/astroid/archive/astroid-"
             version ".tar.gz"))
       (sha256
        (base32
         "0isn5p7f9n48hmksgbrj7dkm9dyglnayzn5jngk37qywg8a74ngn"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-lazy-object-proxy" ,python-lazy-object-proxy)
       ("python-six" ,python-six)
       ("python-wrapt" ,python-wrapt)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
                  (lambda _
                    (zero? (system* "python" "-m" "unittest" "discover"
                                    "-p" "unittest*.py")))))))
    (home-page "https://github.com/PyCQA/astroid")
    (synopsis "Common base representation of python source code for pylint and
other projects")
    (description "@code{python-astroid} provides a common base representation
of python source code for projects such as pychecker, pyreverse, pylint, etc.

It provides a compatible representation which comes from the _ast module.  It
rebuilds the tree generated by the builtin _ast module by recursively walking
down the AST and building an extended ast.  The new node classes have
additional methods and attributes for different usages.  They include some
support for static inference and local name scopes.  Furthermore, astroid
builds partial trees by inspecting living objects.")
    (license license:lgpl2.1+)
    (properties `((python2-variant . ,(delay python2-astroid))))))

(define-public python2-astroid
  (let ((base (package-with-python2
               (strip-python2-variant python-astroid))))
    (package (inherit base)
             (propagated-inputs
              `(("python2-backports-functools-lru-cache"
                 ,python2-backports-functools-lru-cache)
                ("python2-enum34" ,python2-enum34)
                ("python2-singledispatch" ,python2-singledispatch)
                ,@(package-propagated-inputs base))))))

(define-public python-isort
  (package
    (name "python-isort")
    (version "4.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/timothycrosley/isort/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0zsrgkb0krn5476yncy5dd56k7dk34zqb4bnlvwy44ixgilyjmfh"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/timothycrosley/isort")
    (synopsis "Python utility/library to sort python imports")
    (description "@code{python-isort} is a python utility/library to sort
imports alphabetically, and automatically separated into sections.  It
provides a command line utility, a python library and plugins for various
editors.")
    (license license:expat)))

(define-public python2-isort
  (package-with-python2 python-isort))

(define-public python2-backports-functools-lru-cache
  (package
    (name "python2-backports-functools-lru-cache")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       ;; only the pypi tarballs contain the necessary metadata
       (uri (pypi-uri "backports.functools_lru_cache" version))
       (sha256
        (base32
         "158ysf2hb0q4p4695abfiym9x1ywg0dgh8a3apd7gqaaxjy22jj4"))))
    (build-system python-build-system)
    (native-inputs
     `(("python2-setuptools-scm" ,python2-setuptools-scm)))
    (arguments
     `(#:python ,python-2))
    (home-page "https://github.com/jaraco/backports.functools_lru_cache")
    (synopsis "Backport of functools.lru_cache from Python 3.3")
    (description "@code{python2-backports-functools-lru-cache} is a backport
of @code{functools.lru_cache} from python 3.3.")
    (license license:expat)))

(define-public python-configparser
  (package
    (name "python-configparser")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://bitbucket.org/ambv/configparser/get/"
             version ".tar.bz2"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0waq40as14abwzbb321hfz4vr1fi363nscy32ga14qvfygrg96wa"))))
    (build-system python-build-system)
    (home-page "http://docs.python.org/py3k/library/configparser.html")
    (synopsis "Backport of configparser from python 3.5")
    (description "@code{python-configparser} is a backport of
@code{configparser} from Python 3.5 so that it can be used directly
in other versions.")
    (license license:expat)))

(define-public python2-configparser
  (package-with-python2 python-configparser))

(define-public python-mando
  (package
  (name "python-mando")
  (version "0.5")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "mando" version))
      (sha256
        (base32
          "0q05h66439gqdmlk4jqm6xrwrzfdgs4mwk70barxhr2y83qbbdc0"))))
  (build-system python-build-system)
  (propagated-inputs
   `(("python-rst2ansi" ,python-rst2ansi)))
  (native-inputs
   `(("python-sphinx" ,python-sphinx-1.5.3)
     ("python-paramunittest" ,python-paramunittest)))
  (home-page "https://mando.readthedocs.org/")
  (synopsis
    "Wrapper around argparse, allowing creation of complete CLI applications")
  (description
    "This package is a wrapper around argparse, allowing you to write complete CLI
 applications in seconds while maintaining all the flexibility.")
  (license license:expat)))

(define-public python2-mando
  (package-with-python2 python-mando))

(define-public python-mando-0.3.1
  ;; python-radon (version 1.5.0) has a requirement
  ;; for mando<0.4,>=0.3
  (package
    (inherit python-mando)
    (name "python-mando")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rubik/mando/archive/v"
                           version
                           ".tar.gz"))
       (sha256
        (base32
         "17jlkdpqw22z1nyml5ybslilqkzmnk0dxxjml8bfghav1l5hbwd2"))))))

(define-public python-fudge
  (package
    (name "python-fudge")
    ;; 0.9.6 is the latest version suitable for testing the "fabric" Python 2
    ;; package, which is currently the only use of this package.
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fudge" version))
       (sha256
        (base32
         "185ia3vr3qk4f2s1a9hdxb8ci4qc0x0xidrad96pywg8j930qs9l"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))     ;XXX: Tests require the NoseJS Python package.
    (home-page "https://github.com/fudge-py/fudge")
    (synopsis "Replace real objects with fakes/mocks/stubs while testing")
    (description
     "Fudge is a Python module for using fake objects (mocks and stubs) to
test real ones.

In readable Python code, you declare the methods available on your fake object
and how they should be called.  Then you inject that into your application and
start testing.  This declarative approach means you don’t have to record and
playback actions and you don’t have to inspect your fakes after running code.
If the fake object was used incorrectly then you’ll see an informative
exception message with a traceback that points to the culprit.")
    (license license:expat)))

(define-public python2-fudge
  (package-with-python2 python-fudge))

(define-public python-mwclient
  (package
    (name "python-mwclient")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       ;; The PyPI version wouldn't contain tests.
       (uri (string-append "https://github.com/mwclient/mwclient/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32
         "1jj0yhilkjir00719fc7w133x7hdyhkxhk6xblla4asig45klsfv"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("python-requests-oauthlib"
        ,python-requests-oauthlib)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-pep8" ,python-pytest-pep8)
       ("python-pytest-cache" ,python-pytest-cache)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-responses" ,python-responses)))
    (home-page "https://github.com/btongminh/mwclient")
    (synopsis "MediaWiki API client")
    (description "This package provides a MediaWiki API client.")
    (license license:expat)))

(define-public python2-mwclient
  (package-with-python2 python-mwclient))

(define-public python-utils
  (package
    (name "python-utils")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python-utils" version))
              (sha256
               (base32
                "1mcsy6q5am4ya72rgkpb6kax6vv7c93cfkkas89xnpa4sj9zf28p"))))
    (build-system python-build-system)
    (native-inputs
     `(("pytest-runner" ,python-pytest-runner)
       ("pytest" ,python-pytest)
       ("six" ,python-six)))
    (home-page "https://github.com/WoLpH/python-utils")
    (synopsis "Convenient utilities not included with the standard Python install")
    (description
      "Python Utils is a collection of small Python functions and classes which
make common patterns shorter and easier.")
    (license license:bsd-2)))

(define-public python2-utils
  (package-with-python2 python-utils))

(define-public python-sphinx-me
  (package
    (name "python-sphinx-me")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-me" version))
       (sha256
        (base32
         "06jzgp213zihnvpcy2y5jy3ykid3apc2ncp2pg6a2g05lhiziglq"))))
    (build-system python-build-system)
    (home-page "https://github.com/stephenmcd/sphinx-me")
    (synopsis "Create a Sphinx documentation shell")
    (description
      "Create a Sphinx documentation shell for your project and include the
README file as the documentation index.  It handles extracting the required
meta data such as the project name, author and version from your project for
use in your Sphinx docs.")
    (license license:bsd-2)))

(define-public python2-sphinx-me
  (package-with-python2 python-sphinx-me))

(define-public python-diff-match-patch
  (package
    (name "python-diff-match-patch")
    (version "20121119")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "diff-match-patch" version))
        (sha256
         (base32
          "0k1f3v8nbidcmmrk65m7h8v41jqi37653za9fcs96y7jzc8mdflx"))))
    (build-system python-build-system)
    (home-page "https://code.google.com/p/google-diff-match-patch")
    (synopsis "Synchronize plain text")
    (description "Diff Match and Patch libraries offer robust algorithms to
perform the operations required for synchronizing plain text.")
    (license license:asl2.0)))

(define-public python2-diff-match-patch
  (package-with-python2 python-diff-match-patch))

(define-public python-dirsync
  (package
    (name "python-dirsync")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dirsync" version ".zip"))
        (sha256
         (base32
          "1hcdvmkwd5512zbxpin0k7bx5bkgzy3swjx7d0kj1y45af6r75v2"))))
    (build-system python-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (propagated-inputs
     `(("six" ,python-six)))
    (home-page "https://bitbucket.org/tkhyn/dirsync")
    (synopsis "Advanced directory tree synchronisation tool")
    (description "Advanced directory tree synchronisation tool.")
    (license license:expat)))

(define-public python2-dirsync
  (package-with-python2 python-dirsync))

(define-public python-levenshtein
  (package
    (name "python-levenshtein")
    (version "0.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "python-Levenshtein" version))
      (sha256
       (base32
        "1c9ybqcja31nghfcc8xxbbz9h60s9qi12b9hr4jyl69xbvg12fh3"))))
    (build-system python-build-system)
    (home-page "https://github.com/ztane/python-Levenshtein")
    (synopsis "Fast computation of Levenshtein distance and string similarity")
    (description
     "The Levenshtein Python C extension module contains functions for fast computation of
@enumerate
@item Levenshtein (edit) distance, and edit operations
@item string similarity
@item approximate median strings, and generally string averaging
@item string sequence and set similarity
@end enumerate
It supports both normal and Unicode strings.")
    (license license:gpl2+)))

(define-public python2-levenshtein
  (package-with-python2 python-levenshtein))

(define-public python-scandir
  (package
    (name "python-scandir")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scandir" version))
       (sha256
        (base32 "0yjrgp0mxp3d8bjkq2m1ac2ys8n76wykksvgyjrnil9gr3fx7a5d"))))
    (build-system python-build-system)
    (home-page "https://github.com/benhoyt/scandir")
    (synopsis "Directory iteration function")
    (description
     "Directory iteration function like os.listdir(), except that instead of
returning a list of bare filenames, it yields DirEntry objects that include
file type and stat information along with the name.  Using scandir() increases
the speed of os.walk() by 2-20 times (depending on the platform and file
system) by avoiding unnecessary calls to os.stat() in most cases.")
    (license license:bsd-3)))

(define-public python2-scandir
  (package-with-python2 python-scandir))

(define-public python2-stemming
  (package
    (name "python2-stemming")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stemming" version))
       (sha256
        (base32 "0ldwa24gnnxhniv0fhygkpc2mwgd93q10ag8rvzayv6hw418frsr"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (home-page "https://bitbucket.org/mchaput/stemming/overview")
    (synopsis "Python implementations of various stemming algorithms")
    (description
     "Python implementations of the Porter, Porter2, Paice-Husk, and Lovins
stemming algorithms for English.  These implementations are straightforward and
efficient, unlike some Python versions of the same algorithms available on the
Web.  This package is an extraction of the stemming code included in the Whoosh
search engine.")
    (license license:public-domain)))

(define-public python-factory-boy
  (package
    (name "python-factory-boy")
    (version "2.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "factory_boy" version))
       (sha256
        (base32 "1fvin6san5xsjh2c4y18caj2lnmwxlylyqm8mh1yc6rp38wlwr56"))))
    (build-system python-build-system)
    (arguments
     ;; Tests are not included in the tarball.
     `(#:tests? #f))
    (propagated-inputs
     `(("faker" ,python-faker)))
    (home-page "https://github.com/benhoyt/scandir")
    (synopsis "Versatile test fixtures replacement")
    (description
     "Factory_boy is a fixtures replacement based on thoughtbot’s factory_girl.

As a fixtures replacement tool, it aims to replace static, hard to maintain
fixtures with easy-to-use factories for complex object.

Instead of building an exhaustive test setup with every possible combination
of corner cases, factory_boy allows you to use objects customized for the
current test, while only declaring the test-specific fields")
    (license license:expat)))

(define-public python2-factory-boy
  (package-with-python2 python-factory-boy))

(define-public python-translate-toolkit
  (package
    (name "python-translate-toolkit")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "translate-toolkit" version ".tar.bz2"))
       (sha256
        (base32 "1vlkwrg83vb17jc36pmwh2b7jphwf390lz0jw8hakcg16qhwypvq"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-sphinx" ,python-sphinx)))
    (propagated-inputs
     `(("python-babel" ,python-babel)
       ("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-chardet" ,python-chardet)
       ("python-diff-match-patch" ,python-diff-match-patch)
       ("python-levenshtein" ,python-levenshtein)
       ("python-lxml" ,python-lxml)
       ("python-six" ,python-six)
       ("python-vobject" ,python-vobject)
       ("python-pyyaml" ,python-pyyaml)))
    (arguments
     ;; TODO: tests are not run, because they end with
     ;; TypeError: parse() missing 2 required positional arguments: 'tree' and
     ;; 'parse_funcs'
     ;; during test setup.
     `(#:tests? #f))
    (home-page "http://toolkit.translatehouse.org")
    (synopsis "Tools and API for translation and localization engineering")
    (description
     "Tools and API for translation and localization engineering.  It contains
several utilities, as well as an API for building localization tools.")
    (license license:gpl2+)))

(define-public python2-translate-toolkit
  (package-with-python2 python-translate-toolkit))

(define-public python-packaging
  (package
    (name "python-packaging")
    (version "16.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "packaging" version))
        (sha256
         (base32
          "17k1xbjshackwvbsnxqixbph8rbqhz4bf4g3al5xyzhavxgq6l2x"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pretend" ,python-pretend)
       ("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-pyparsing" ,python-pyparsing)
       ("python-six" ,python-six)))
    (home-page "https://github.com/pypa/packaging")
    (synopsis "Core utilities for Python packages")
    (description "Packaging is a Python module for dealing with Python packages.
It offers an interface for working with package versions, names, and dependency
information.")
    ;; From 'LICENSE': This software is made available under the terms of
    ;; *either* of the licenses found in LICENSE.APACHE or LICENSE.BSD.
    ;; Contributions to this software is made under the terms of *both* these
    ;; licenses.
    (license (list license:asl2.0 license:bsd-2))))

(define-public python2-packaging
  (package-with-python2 python-packaging))

(define-public python-relatorio
  (package
    (name "python-relatorio")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "relatorio" version))
       (sha256
        (base32
         "0lincq79mzgazwd9gh41dybjh9c3n87r83pl8nk3j79aihyfk84z"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-lxml" ,python-lxml)
       ("python-genshi" ,python-genshi)))
    (home-page "https://relatorio.tryton.org/")
    (synopsis "Templating library able to output ODT and PDF files")
    (description "Relatorio is a templating library which provides a way to
easily output ODT, ODS, PNG, SVG and several other kinds of files.  Support
for more filetypes can be easily added by creating plugins for them.")
    (license license:gpl3+)))

(define-public python2-relatorio
  (package-with-python2 python-relatorio))

(define-public python-radon
  (package
    (name "python-radon")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "radon" version))
       (sha256
        (base32
         "1h6jv36am0i827182a04ki6291lyx4kp957xfr5njgprj4nd0qsl"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-colorama" ,python-colorama)
       ("python-flake8-polyfill" ,python-flake8-polyfill)
       ("python-mando" ,python-mando-0.3.1)))
    (native-inputs
     `(("python-flake8" ,python-flake8)
       ("python-tox" ,python-tox)
       ("python-pytest" ,python-pytest)
       ("python-paramunittest" ,python-paramunittest)))
    (home-page "https://radon.readthedocs.org/")
    (synopsis "Code Metrics in Python")
    (description "Radon is a Python tool which computes various code metrics.
Supported metrics are:
@itemize @bullet
@item raw metrics: SLOC, comment lines, blank lines, &c.
@item Cyclomatic Complexity (i.e.  McCabe’s Complexity)
@item Halstead metrics (all of them)
@item the Maintainability Index (a Visual Studio metric)
@end itemize")
    (license license:expat)))

(define-public python2-radon
  (package-with-python2 python-radon))

(define-public python-sure
  (package
    (name "python-sure")
    (version "1.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sure" version))
       (sha256
        (base32
         "1iyqsy2d6radi88g1qf0lziy5b39h5cpb3g5jiqyb4xi46ig3x1z"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-mock" ,python-mock)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/gabrielfalcao/sure")
    (synopsis "Automated testing library in python for python")
    (description
     "Sure is a python library that leverages a DSL for writing assertions.
Sure is heavily inspired by @code{RSpec Expectations} and @code{should.js}.")
    (license license:gpl3+)))

(define-public python2-sure
  (package-with-python2 python-sure))

(define-public python2-couleur
  ;; This package does not seem to support python3 at all, hence,
  ;; only the python2 variant definition is provided.
  (package
    (name "python2-couleur")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "couleur" version))
       (sha256
        (base32
         "1qqaxyqz74wvid0cr119dhcwz0h0if5b5by44zl49pd5z65v58k1"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (home-page "https://github.com/gabrielfalcao/couleur")
    (synopsis
     "ANSI terminal tool for python, colored shell and other handy fancy features")
    (description
     "@code{Couleur} provides python programs a way to use the ANSI features in a unix
terminal such as coloured output in the shell, overwriting output, indentation, etc.")
    ;; README.md says ASL2.0, but all source code headers are LGPL3+.
    ;; https://github.com/gabrielfalcao/couleur/issues/11
    (license license:lgpl3+)))

(define-public python-misaka
  (package
    (name "python-misaka")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "misaka" version))
       (sha256
        (base32
         "1yqrq3a5rracirmvk52n28nn6ckdaz897gnigv89a9gmyn87sqw7"))))
    (build-system python-build-system)
    (arguments
     `(;; Line 37 of setup.py calls self.run_command('develop')
       ;; in the 'check' phase. This command seems to be trying
       ;; to write to
       ;; /gnu/store/...-python-<version>/lib/python<version>/site-packages/
       ;; for which it does not have the permission to write.
       #:tests? #f))
    (propagated-inputs
     `(("python-cffi" ,python-cffi)))
    (home-page "https://github.com/FSX/misaka")
    (synopsis "Python binding for Hoedown")
    (description
     "@code{Misaka} is a CFFI-based binding for @code{Hoedown}, a fast markdown processing
library written in C.  It features a fast HTML renderer and functionality to make custom
renderers (e.g. man pages or LaTeX).")
    (license license:expat)))

(define-public python2-misaka
  (package-with-python2 python-misaka))

(define-public python2-steadymark
  ;; This is forced into being a python2 only variant
  ;; due to its dependence on couleur that has no support
  ;; for python3
  (package
    (name "python2-steadymark")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "steadymark" version))
       (sha256
        (base32
         "1640i9g8dycql3cc8j0bky0jkzj0q39blfbp4lsgpkprkfgcdk8v"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-couleur" ,python2-couleur)
       ("python-sure" ,python2-sure)
       ("python-misaka" ,python2-misaka)))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-setup-py
           (lambda _
             ;; Update requirements from dependency==version
             ;; to dependency>=version
             (substitute* "setup.py"
               (("==") ">="))
             #t)))))
    (home-page "https://github.com/gabrielfalcao/steadymark")
    (synopsis "Markdown-based test runner for python")
    (description
     "@code{Steadymark} allows documentation to be written in github-flavoured
markdown.  The documentation may contain snippets of code surrounded by python
code blocks and @code{Steadymark} will find these snippets and run them, making
sure that there are no old malfunctional examples in the documentation examples.")
    (license license:expat)))

(define-public python-jsonpointer
  (package
    (name "python-jsonpointer")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jsonpointer" version))
       (sha256
        (base32
         "1cg0gvgqjysydv6p45v4jywg1jb3v48c7m3cbpi57zgf6nndr9cz"))))
  (build-system python-build-system)
  (home-page "https://github.com/stefankoegl/python-json-pointer")
  (synopsis "Identify specific nodes in a JSON document")
  (description "@code{jsonpointer} allows you to access specific nodes
by path in a JSON document (see RFC 6901).")
  (license license:bsd-3)))

(define-public python2-jsonpointer
  (package-with-python2 python-jsonpointer))

(define-public python-jsonpatch
  (package
    (name "python-jsonpatch")
    (version "1.16")
    (source
     (origin
       (method url-fetch)
       ;; pypi version lacks tests.js
       (uri (string-append "https://github.com/stefankoegl/python-json-patch/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "085ykisl8v7mv9h7hvhdy3l2fjzs4214gx32r5k6nx4f76hbv6y5"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-jsonpointer" ,python-jsonpointer)))
    (home-page "https://github.com/stefankoegl/python-json-patch")
    (synopsis "Applying JSON Patches in Python 2.6+ and 3.x")
    (description "@code{jsonpatch} is a library and program that allows
applying JSON Patches according to RFC 6902.")
    (license license:bsd-3)))

(define-public python2-jsonpatch
  (package-with-python2 python-jsonpatch))

(define-public python-jsonpatch-0.4
  (package (inherit python-jsonpatch)
    (name "python-jsonpatch")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/stefankoegl/python-json-patch/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0j0cd9z9zyp8kppp464jxrfgrnbgkzl1yi10i5gsv8yz6d95929d"))))))

(define-public python2-jsonpatch-0.4
  (package-with-python2 python-jsonpatch-0.4))

(define-public python-rfc3987
  (package
    (name "python-rfc3987")
    (version "1.3.7")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "rfc3987" version))
      (sha256
       (base32
        "192pclzs2y0yaywqkrlvd0x73740q310kvqvm6jldhi619mq59wi"))))
    (build-system python-build-system)
    (home-page "https://pypi.python.org/pypi/rfc3987")
    (synopsis "Parsing and validation of URIs (RFC 3986) and IRIs (RFC 3987)")
    (description "@code{rfc3987} provides routines for parsing and
validation of URIs (see RFC 3986) and IRIs (see RFC 3987).")
    (license license:gpl3+)))

(define-public python2-rfc3987
  (package-with-python2 python-rfc3987))

(define-public python-validate-email
  (package
    (name "python-validate-email")
    (version "1.3")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "validate_email" version))
      (sha256
       (base32
        "1bxffaf5yz2cph8ki55vdvdypbwkvn2xr1firlcy62vqbzf1jivq"))))
    (build-system python-build-system)
    (home-page "https://github.com/syrusakbary/validate_email")
    (synopsis "Verifies if an email address is valid and really exists")
    (description "@code{validate_email} can be used to verify if an email
address is valid and really exists.")
    (license license:lgpl3+)))

(define-public python2-validate-email
  (package-with-python2 python-validate-email))

(define-public python-flex
  (package
    (name "python-flex")
    (version "6.10.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "flex" version))
      (sha256
       (base32
        "00pamnwla3khk8nyla7y28dq9jnh69swd7f4jfsl7sn1izs8n8zk"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-iso8601" ,python-iso8601)
       ("python-jsonpointer" ,python-jsonpointer)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)
       ("python-rfc3987" ,python-rfc3987)
       ("python-six" ,python-six)
       ("python-validate-email" ,python-validate-email)))
    (home-page "https://github.com/pipermerriam/flex")
    (synopsis "Validates Swagger schemata")
    (description "@code{flex} can be used to validate Swagger schemata.")
    (license license:bsd-3)))

(define-public python2-flex
  (package-with-python2 python-flex))

(define-public python-marshmallow
  (package
    (name "python-marshmallow")
    (version "3.0.0b3")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "marshmallow" version))
      (sha256
       (base32
        "07mcrij1yvk85lvgx44wwr9pc80xryghvlgayb057g1cazcypysd"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-simplejson" ,python-simplejson)))
    (native-inputs
     `(("python-pytest-3.0" ,python-pytest-3.0)
       ("python-pytz" ,python-pytz)))
    (home-page "https://github.com/marshmallow-code/marshmallow")
    (synopsis "Convert complex datatypes to and from native
Python datatypes.")
    (description "@code{marshmallow} provides a library for converting
complex datatypes to and from native Python datatypes.")
    (license license:expat)))

(define-public python2-marshmallow
  (package-with-python2 python-marshmallow))

(define-public python-apispec
  (package
    (name "python-apispec")
    (version "0.25.3")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "apispec" version))
      (sha256
        (base32
          "0kxa8723zbisx10363yh4mmmn4higxrspymbjfz5zq8f644zagm9"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pyyaml" ,python-pyyaml)))
    (native-inputs
     `(("python-pytest-3.0" ,python-pytest-3.0)
       ("python-flask" ,python-flask)
       ("python-marshmallow" ,python-marshmallow)
       ("python-tornado" ,python-tornado)
       ("python-bottle" ,python-bottle)
       ("python-mock" ,python-mock)))
    (home-page "https://github.com/marshmallow-code/apispec")
    (synopsis "Swagger 2.0 API specification generator")
    (description "@code{python-apispec} is a pluggable API specification
generator. Currently supports the OpenAPI specification (f.k.a.
Swagger 2.0).")
    (license license:expat)))

(define-public python2-apispec
  (package-with-python2 python-apispec))

(define-public python-flasgger
  (package
    (name "python-flasgger")
    (version "0.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/rochacbruno/flasgger/archive/"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gqzlm0rb55fdpsy5ipkganlx9cnpi454fqyycr03jm22zql14ay"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("flake8 flasgger --ignore=F403")
                "flake8 flasgger --ignore=E731,F403"))
             (setenv "PYTHONPATH" (string-append (getcwd)
                                                 ":"
                                                 (getenv "PYTHONPATH")))
             (zero? (system* "py.test")))))))
    (propagated-inputs
     `(("python-flask" ,python-flask)
       ("python-pyyaml" ,python-pyyaml)
       ("python-jsonschema" ,python-jsonschema)
       ("python-mistune" ,python-mistune)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-decorator" ,python-decorator)
       ("python-flake8" ,python-flake8)
       ("python-flask-restful" ,python-flask-restful)
       ("python-flex" ,python-flex)
       ("python-pytest-3.0" ,python-pytest-3.0)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-marshmallow" ,python-marshmallow)
       ("python-apispec" ,python-apispec)))
    (home-page "https://github.com/rochacbruno/flasgger/")
    (synopsis "Extract Swagger specs from your Flask project")
    (description "@code{python-flasgger} allows extracting Swagger specs
from your Flask project.  It is a fork of Flask-Swagger.")
    (license license:expat)))

(define-public python2-flasgger
  (package-with-python2 python-flasgger))

(define-public python-swagger-spec-validator
  (package
    (name "python-swagger-spec-validator")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "swagger-spec-validator" version))
       (sha256
        (base32
         "13hkpn2lycwr0468yqhjb3kwszqf7hjwlq61w7vdxq1caz31k4nw"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-jsonschema" ,python-jsonschema)
       ("python-six" ,python-six)))
    (home-page
     "https://github.com/Yelp/swagger_spec_validator")
    (synopsis "Validation of Swagger specifications")
    (description "@code{swagger_spec_validator} provides a library for
validating Swagger API specifications.")
    (license license:asl2.0)))

(define-public python2-swagger-spec-validator
  (package-with-python2 python-swagger-spec-validator))

(define-public python-apache-libcloud
  (package
    (name "python-apache-libcloud")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "apache-libcloud" version))
        (sha256
          (base32
            "1a71z02ckcxld72k4qgmdnkjan52c4wczncs3p2mp5yafh7dsan7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-ssh
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libcloud/compute/ssh.py"
               (("'ssh'") (string-append "'" (assoc-ref inputs "openssh")
                                         "/bin/ssh" "'")))
             #t))
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "./libcloud/test/test_file_fixtures.py"
               ;; See <https://issues.apache.org/jira/browse/LIBCLOUD-923>.
               (("def _ascii") "def _raw_data(self, method, url, body, headers):
        return (httplib.OK,
                \"1234abcd\",
                {\"test\": \"value\"},
                httplib.responses[httplib.OK])
    def _ascii"))
             (substitute* "libcloud/test/compute/test_ssh_client.py"
               (("class ShellOutSSHClientTests")
                "@unittest.skip(\"Guix container doesn't have ssh service\")
class ShellOutSSHClientTests")
               ;; See <https://issues.apache.org/jira/browse/LIBCLOUD-924>.
               (("'.xf0.x90.x8d.x88'") "b'\\xF0\\x90\\x8D\\x88'")
               (("'.xF0', '.x90', '.x8D', '.x88'")
                "b'\\xF0', b'\\x90', b'\\x8D', b'\\x88'"))
             #t)))))
    (inputs
     `(("openssh" ,openssh)))
    (propagated-inputs
     `(("python-paramiko" ,python-paramiko)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-lockfile" ,python-lockfile)
       ("python-mock" ,python-mock)
       ("python-requests-mock" ,python-requests-mock)))
    (home-page "https://libcloud.apache.org/")
    (synopsis "Unified Cloud API")
    (description "@code{libcloud} is a Python library for interacting with
many of the popular cloud service providers using a unified API.")
    (license license:asl2.0)))

(define-public python2-apache-libcloud
  (package-with-python2 python-apache-libcloud))

(define-public python-smmap2
  (package
    (name "python-smmap2")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "smmap2" version))
       (sha256
        (base32
         "1hvn28p3zvxa98sbi9lrqvv2ps4q284j4jq9a619zw0m7yv0sly7"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nosexcover" ,python-nosexcover)))
    (home-page "https://github.com/Byron/smmap")
    (synopsis "Python sliding window memory map manager")
    (description "@code{smmap2} is a pure Python implementation of a sliding
window memory map manager.")
    (license license:bsd-3)))

(define-public python2-smmap2
  (package-with-python2 python-smmap2))

(define-public python-regex
  (package
    (name "python-regex")
    (version "2017.06.07")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "regex" version))
              (sha256
               (base32
                "06r6b7yigikbj3a72whl85r2b64pj1r0ypmw9yalmkm0wnxq8mz4"))))
    (build-system python-build-system)
    (home-page "https://bitbucket.org/mrabarnett/mrab-regex")
    (synopsis "Alternative regular expression module")
    (description "This regular expression implementation is backwards-
compatible with the standard @code{re} module, but offers additional
functionality like full case-folding for case-insensitive matches in Unicode.")
    (license license:psfl)))

(define-public python2-regex
  (package-with-python2 python-regex))

(define-public python2-pyopengl
  (package
   (name "python2-pyopengl")
   (version "3.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "PyOpenGL" version))
     (sha256
      (base32
       "1byxjj6a8rwzhxhjqlc588zdad2qwxdd7vlam2653ylll31waiwv"))))
   (arguments
     `(#:python ,python-2))
   (build-system python-build-system)
   (home-page "http://pyopengl.sourceforge.net")
   (synopsis "Standard OpenGL bindings for Python")
   (description
    "PyOpenGL is the most common cross platform Python binding to OpenGL and
related APIs.  The binding is created using the standard @code{ctypes}
library.")
   (license license:bsd-3)))

(define-public python2-pyopengl-accelerate
  (package
    (inherit python2-pyopengl)
    (name "python2-pyopengl-accelerate")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyOpenGL-accelerate" version))
       (sha256
        (base32
         "0464c1ifzk0k92lyndikmvzjgnx1y25r7bkkc8pnxm4kp1q4czwj"))))
    (synopsis "Acceleration code for PyOpenGL")
    (description
     "This is the Cython-coded accelerator module for PyOpenGL.")))

(define-public python-rencode
  (package
   (name "python-rencode")
   (version "1.0.3")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "rencode" version))
     (sha256
      (base32
       "08if5yax1xn5yfp8p3765ccjmfcv9di7i4m5jckgnwvdsgznwkbj"))))
   (build-system python-build-system)
   (native-inputs `(("pkg-config" ,pkg-config)
                    ("python-cython", python-cython)))
   (home-page "https://github.com/aresch/rencode")
   (synopsis "Serialization of heterogeneous data structures")
   (description
    "The @code{rencode} module is a data structure serialization library,
similar to @code{bencode} from the BitTorrent project.  For complex,
heterogeneous data structures with many small elements, r-encoding stake up
significantly less space than b-encodings.  This version of rencode is a
complete rewrite in Cython to attempt to increase the performance over the
pure Python module.")
   (license license:bsd-3)))

(define-public python2-rencode
  (package-with-python2 python-rencode))

(define-public python-xenon
  (package
    (name "python-xenon")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xenon" version))
       (sha256
        (base32
         "14kby2y48vp3sgwxqlm5d5789yibqwb1qli5fwcmdqg3iayrbklc"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-radon" ,python-radon)
       ("python-requests" ,python-requests)
       ("python-flake8" ,python-flake8)
       ("python-tox" ,python-tox)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-test-requirements
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Update requirements from dependency==version to
             ;; dependency>=version.
             (substitute* "requirements.txt"
               (("==") ">=")
               ((",<1.5.0") ""))
             ;; Remove httpretty dependency for tests.
             (substitute* "setup.py"
               (("httpretty") ""))
             #t)))))
    (home-page "https://xenon.readthedocs.org/")
    (synopsis "Monitor code metrics for Python on your CI server")
    (description
     "Xenon is a monitoring tool based on Radon.  It monitors code complexity.
Ideally, @code{xenon} is run every time code is committed.  Through command
line options, various thresholds can be set for the complexity of code.  It
will fail (i.e.  it will exit with a non-zero exit code) when any of these
requirements is not met.")
    (license license:expat)))

(define-public python2-xenon
  (package-with-python2 python-xenon))

(define-public python-pysocks
  (package
    (name "python-pysocks")
    (version "1.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PySocks" version))
       (sha256
        (base32
         "1krkiss578zqwcg4c8iqz1hwscwhsvy2djp3xyvps5gsgvr2j0yh"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "https://github.com/Anorov/PySocks")
    (synopsis "SOCKS client module")
    (description "@code{pysocks} is an updated and semi-actively maintained
version of @code{SocksiPy} with bug fixes and extra features.")
    (license license:bsd-3)))

(define-public python2-pysocks
  (package-with-python2 python-pysocks))

(define-public python-pydiff
  (package
    (name "python-pydiff")
    (version "0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pydiff" version))
        (sha256
          (base32
            "1als83h9w0gab24ipyna6khm390qmpnpkc5jksmdbs2xc8hp2z44"))))
    (build-system python-build-system)
    (home-page "https://github.com/myint/pydiff")
    (synopsis "Library to diff two Python files at the bytecode level")
    (description
      "@code{pydiff} makes it easy to look for actual code changes while
ignoring formatting changes.")
    (license license:expat)))

(define-public python2-pydiff
  (package-with-python2 python-pydiff))

(define-public python-tqdm
  (package
    (name "python-tqdm")
    (version "4.15.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "tqdm" version))
         (sha256
           (base32
             "0lwrmby8qz23gvqwkpivfrv4q8nfh90cz9ml6slwvwmcxxsdrhbf"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-flake8" ,python-flake8)
       ("python-nose" ,python-nose)
       ("python-nose-timer" ,python-nose-timer)
       ("python-coverage" ,python-coverage)
       ("python-virtualenv" ,python-virtualenv)))
    (home-page "https://github.com/tqdm/tqdm")
    (synopsis "Fast, extensible progress meter")
    (description
      "Make loops show a progress bar on the console by just wrapping any
iterable with @code{|tqdm(iterable)|}.  Offers many options to define
design and layout.")
    (license (list license:mpl2.0 license:expat))))

(define-public python2-tqdm
  (package-with-python2 python-tqdm))

(define-public python-pkginfo
  (package
    (name "python-pkginfo")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pkginfo" version))
        (sha256
          (base32
            "17pqjfpq3c6xzdmk8pski6jcjgjv78q00zjf2bgzb668pzm6l6mv"))))
    (build-system python-build-system)
    (arguments
     ;; The tests are broken upstream.
     '(#:tests? #f))
    (home-page
      "https://code.launchpad.net/~tseaver/pkginfo/trunk")
    (synopsis
      "Query metadatdata from sdists, bdists, and installed packages")
    (description
      "API to query the distutils metadata written in @file{PKG-INFO} inside a
source distriubtion (an sdist) or a binary distribution (e.g., created by
running bdist_egg).  It can also query the EGG-INFO directory of an installed
distribution, and the *.egg-info stored in a \"development checkout\" (e.g,
created by running @code{python setup.py develop}).")
    (license license:expat)))

(define-public python2-pkginfo
  (package-with-python2 python-pkginfo))

(define-public python-twine
  (package
    (name "python-twine")
    (version "1.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "twine" version))
        (sha256
          (base32
            "1ay1b6kdq6k4bfbjsvf6ymj41wrgpvinhxndb09355pwhxwmp96a"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-tqdm" ,python-tqdm)
       ("python-pkginfo", python-pkginfo)
       ("python-requests" ,python-requests)
       ("python-requests-toolbelt" ,python-requests-toolbelt)))
    (home-page "https://github.com/pypa/twine")
    (synopsis "Collection of utilities for interacting with PyPI")
    (description
      "@code{twine} currently supports registering projects and uploading
distributions.  It authenticates the user over HTTPS, allows them to pre-sign
their files and supports any packaging format (including wheels).")
    (license license:asl2.0)))

(define-public python2-twine
  (package-with-python2 python-twine))

(define-public python-linecache2
  (package
    (name "python-linecache2")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "linecache2" version))
        (sha256
          (base32
            "0z79g3ds5wk2lvnqw0y2jpakjf32h95bd9zmnvp7dnqhf57gy9jb"))))
    (build-system python-build-system)
    (arguments
     `(;; The tests depend on unittest2, and our version is a bit too old.
       #:tests? #f))
    (native-inputs
     `(("python-pbr" ,python-pbr)))
    (home-page
      "https://github.com/testing-cabal/linecache2")
    (synopsis "Backports of the linecache module")
    (description
      "The linecache module allows one to get any line from any file, while
attempting to optimize internally, using a cache, the common case where many
lines are read from a single file.")
    (license license:psfl)))

(define-public python2-linecache2
  (package-with-python2 python-linecache2))

(define-public python-traceback2
  (package
    (name "python-traceback2")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "traceback2" version))
        (sha256
          (base32
            "0c1h3jas1jp1fdbn9z2mrgn3jj0hw1x3yhnkxp7jw34q15xcdb05"))))
    (build-system python-build-system)
    (arguments
     `(;; python-traceback2 and python-unittest2 depend on one another.
       #:tests? #f))
    (native-inputs
     `(("python-pbr" ,python-pbr)))
    (propagated-inputs
      `(("python-linecache2" ,python-linecache2)))
    (home-page
      "https://github.com/testing-cabal/traceback2")
    (synopsis "Backports of the traceback module")
    (description
      "This module provides a standard interface to extract, format and print
stack traces of Python programs.  It exactly mimics the behavior of the Python
interpreter when it prints a stack trace.")
    (license license:psfl)))

(define-public python2-traceback2
  (package-with-python2 python-traceback2))

(define-public python-ratelimiter
  (package
    (name "python-ratelimiter")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ratelimiter" version))
       (sha256
        (base32
         "1dhz85mj5bqd2mij84ncs6pz32hgidr79hay4aqfmzaa4rbb497p"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))          ; There are no tests in the pypi archive.
    (home-page "https://github.com/RazerM/ratelimiter")
    (synopsis "Simple rate limiting object")
    (description
     "The @code{ratelimiter} module ensures that an operation will not be
executed more than a given number of times during a given period.")
    (license license:asl2.0)))

(define-public python2-ratelimiter
  (package-with-python2 python-ratelimiter))

(define-public python-dukpy
  (package
    (name "python-dukpy")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/kovidgoyal/dukpy/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32
         "0pj39rfwlzivqm5hkrsza7gssg6ggpxlq5ivc8f3h7x5pfgc6y6c"))))
    (build-system python-build-system)
    (home-page "https://github.com/kovidgoyal/dukpy")
    (synopsis "Run JavaScript in python")
    (description
     "dukpy is a JavaScript runtime environment for Python using the duktape
embeddable JavaScript engine.")
    ;; Dukpy is licensed under MIT like the embedded duktape library,
    ;; with 'errors.c' as GPL3.
    (license (list license:expat license:gpl3))))

(define-public python2-dukpy
  (package-with-python2 python-dukpy))

(define-public python-jsonrpclib-pelix
  (package
    (name "python-jsonrpclib-pelix")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jsonrpclib-pelix" version))
       (sha256
        (base32
         "1qs95vxplxwspbrqy8bvc195s58iy43qkf75yrjfql2sim8b25sl"))))
    (build-system python-build-system)
    (home-page "https://github.com/tcalmant/jsonrpclib/")
    (synopsis "JSON-RPC 2.0 client library for Python")
    (description
     "This library implements the JSON-RPC v2.0
specification (backwards-compatible) as a client library for Python.  This
version is a fork of jsonrpclib by Josh Marshall, usable with Pelix remote
services.")
    (license license:asl2.0)))

(define-public python2-jsonrpclib-pelix
  (package-with-python2 python-jsonrpclib-pelix))

(define-public python-setuptools-scm-git-archive
  (package
    (name "python-setuptools-scm-git-archive")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools_scm_git_archive" version))
       (sha256
        (base32
         "1nii1sz5jq75ilf18bjnr11l9rz1lvdmyk66bxl7q90qan85yhjj"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest-3.0" ,python-pytest-3.0)))
    (propagated-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/Changaco/setuptools_scm_git_archive/")
    (synopsis "Setuptools_scm plugin for git archives")
    (description
     "The setuptools_scm_git_archive package is a plugin to
setuptools_scm, which supports obtaining versions from git archives that
belong to tagged versions.")
    (license license:expat)))

(define-public python2-setuptools-scm-git-archive
  (package-with-python2 python-setuptools-scm-git-archive))

(define-public python-pyclipper
  (package
    (name "python-pyclipper")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyclipper" version ".zip"))
       (sha256
        (base32
         "1zpmwv3bya3j984y5cf9x9d5108kf6mxldcba68wiq0frv5qrssw"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)); 8 Tests fail, 37 succeed
    (propagated-inputs
     `(("python-setuptools-scm-git-archive" ,python-setuptools-scm-git-archive)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://github.com/greginvm/pyclipper")
    (synopsis "Wrapper for Angus Johnson's Clipper library")
    (description
     "Pyclipper is a Cython wrapper for the C++ translation of the
  Angus Johnson's polygon clipping Clipper library (ver. 6.2.1).")
    (license license:expat)))

(define-public python2-pyclipper
  (package-with-python2 python-pyclipper))

(define-public python2-booleanoperations
  (package
    (name "python2-booleanoperations")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "booleanOperations" version ".zip"))
       (sha256
        (base32
         "1hw42fazdpvsn77glx96hwsj9l17mvx37sc5707s08y5w6fx16mn"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (native-inputs
     `(("unzip" ,unzip)
       ("python2-pytest-3.0" ,python2-pytest-3.0)
       ("python2-pytest-runner" ,python2-pytest-runner)))
    (propagated-inputs
     `(("python-fonttools" ,python2-fonttools)
       ("python-pyclipper" ,python2-pyclipper)
       ("python-ufolib" ,python2-ufolib)))
    (home-page "https://github.com/typemytype/booleanOperations")
    (synopsis "Boolean operations on paths")
    (description
     "BooleanOperations provides a Python library that enables
boolean operations on paths.")
    (license license:expat)))

(define-public python-tempdir
  (package
    (name "python-tempdir")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tempdir" version))
       (sha256
        (base32
         "13msyyxqbicr111a294x7fsqbkl6a31fyrqflx3q7k547gnq15k8"))))
    (build-system python-build-system)
    (home-page "https://pypi.org/project/tempdir/")
    (arguments
     ;; the package has no tests
     '(#:tests? #f))
    (synopsis "Python library for managing temporary directories")
    (description
     "This library manages temporary directories that are automatically
deleted with all their contents when they are no longer needed.  It is
particularly convenient for use in tests.")
    (license license:expat)))

(define-public python2-tempdir
  (package-with-python2 python-tempdir))

(define-public python-activepapers
  (package
    (name "python-activepapers")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ActivePapers.Py" version))
       (sha256
        (base32
         "12wkhjh90ffipjzv10swndp2xv9hd7xrxvg6v0n4n3i411pj4xb8"))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((ice-9 ftw)
                  (srfi srfi-1)
                  (guix build utils)
                  (guix build python-build-system))

       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-python2-code
           (lambda _
             (for-each delete-file
                       '("lib/activepapers/builtins2.py"
                         "lib/activepapers/standardlib2.py"
                         "lib/activepapers/utility2.py"))))
         (replace 'check
           (lambda _
             ;; Deactivate the test cases that download files
             (setenv "NO_NETWORK_ACCESS" "1")
             ;; For some strange reason, some tests fail if nosetests runs all
             ;; test modules in a single execution. They pass if each test
             ;; module is run individually.
             (for-each (lambda (filename)
                         (invoke "nosetests"
                                 (string-append "tests/" filename)))
                       (scandir "tests"
                                (lambda (filename)
                                  (string-suffix? ".py" filename)))))))))
    (native-inputs
     `(("python-tempdir" ,python-tempdir)
       ("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-h5py" ,python-h5py)))
    (home-page "http://www.activepapers.org/")
    (synopsis "Executable papers for scientific computing")
    (description
     "ActivePapers is a tool for working with executable papers, which
combine data, code, and documentation in single-file packages,
suitable for publication as supplementary material or on repositories
such as figshare or Zenodo.")
    (properties `((python2-variant . ,(delay python2-activepapers))))
    (license license:bsd-3)))

(define-public python2-activepapers
  (let ((base (package-with-python2
               (strip-python2-variant python-activepapers))))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'delete-python2-code)
             (add-after 'unpack 'delete-python3-code
               (lambda _
                 (for-each delete-file
                           '("lib/activepapers/builtins3.py"
                             "lib/activepapers/standardlib3.py"
                             "lib/activepapers/utility3.py")))))))))))

(define-public python-semver
  (package
    (name "python-semver")
    (version "2.7.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "semver" version))
        (sha256
          (base32
            "0hhgqppchv59rqj0yzi1prdg2nfsywqmjsqy2rycyxm0hvxmbyqz"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-test-requirements
           (lambda _
             (substitute* "setup.py"
               ;; Our Python is new enough.
               (("'virtualenv<14\\.0\\.0'") "'virtualenv'"))
             #t)))))
    (native-inputs
     `(("python-tox" ,python-tox)
       ("python-virtualenv" ,python-virtualenv)))
    (home-page "https://github.com/k-bx/python-semver")
    (synopsis "Python helper for Semantic Versioning")
    (description "This package provides a Python library for
@url{Semantic Versioning, http://semver.org/}.")
    (license license:bsd-3)))

(define-public python2-semver
  (package-with-python2 python-semver))
