;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Omar Radwan <toxemicsquire4@gmail.com>
;;; Copyright © 2015 Pierre-Antoine Rault <par@rigelk.eu>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2015 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Kyle Meyer <kyle@kyleam.com>
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
  #:use-module ((guix licenses)
                #:select (asl2.0 bsd-4 bsd-3 bsd-2 non-copyleft cc0 x11 x11-style
                          gpl2 gpl2+ gpl3+ lgpl2.0+ lgpl2.1 lgpl2.1+ lgpl3+ agpl3+
                          isc psfl public-domain x11-style zpl2.1))
  #:use-module ((guix licenses) #:select (expat zlib) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages tcl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public python-2
  (package
    (name "python")
    (version "2.7.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.python.org/ftp/python/"
                          version "/Python-" version ".tar.xz"))
      (sha256
       (base32
        "1h7zbrf9pkj29hlm18b10548ch9757f75m64l47sy75rh43p7lqw"))
      (patches (map search-patch
                    '("python-2.7-search-paths.patch"
                      "python-2-deterministic-build-info.patch"
                      "python-2.7-source-date-epoch.patch")))))
    (outputs '("out"
               "tk"))                     ;tkinter; adds 50 MiB to the closure
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       ;; 268 tests OK.
       ;; 103 tests failed:
       ;;     test_distutils test_shutil test_signal test_site test_slice
       ;;     test_smtplib test_smtpnet test_socket test_socketserver
       ;;     test_softspace test_sort test_spwd test_sqlite test_ssl
       ;;     test_startfile test_stat test_str test_strftime test_string
       ;;     test_stringprep test_strop test_strptime test_strtod test_struct
       ;;     test_structmembers test_structseq test_subprocess test_sunau
       ;;     test_sunaudiodev test_sundry test_symtable test_syntax test_sys
       ;;     test_sys_setprofile test_sys_settrace test_sysconfig test_tarfile
       ;;     test_tcl test_telnetlib test_tempfile test_textwrap test_thread
       ;;     test_threaded_import test_threadedtempfile test_threading
       ;;     test_threading_local test_threadsignals test_time test_timeit
       ;;     test_timeout test_tk test_tokenize test_tools test_trace
       ;;     test_traceback test_transformer test_ttk_guionly test_ttk_textonly
       ;;     test_tuple test_typechecks test_ucn test_unary
       ;;     test_undocumented_details test_unicode test_unicode_file
       ;;     test_unicodedata test_univnewlines test_univnewlines2k test_unpack
       ;;     test_urllib test_urllib2 test_urllib2_localnet test_urllib2net
       ;;     test_urllibnet test_urlparse test_userdict test_userlist
       ;;     test_userstring test_uu test_uuid test_wait3 test_wait4
       ;;     test_warnings test_wave test_weakref test_weakset test_whichdb
       ;;     test_winreg test_winsound test_with test_wsgiref test_xdrlib
       ;;     test_xml_etree test_xml_etree_c test_xmllib test_xmlrpc
       ;;     test_xpickle test_xrange test_zipfile test_zipfile64
       ;;     test_zipimport test_zipimport_support test_zlib
       ;; 30 tests skipped:
       ;;     test_aepack test_al test_applesingle test_bsddb test_bsddb185
       ;;     test_bsddb3 test_cd test_cl test_codecmaps_cn test_codecmaps_hk
       ;;     test_codecmaps_jp test_codecmaps_kr test_codecmaps_tw test_crypt
       ;;     test_curses test_dl test_gdb test_gl test_idle test_imageop
       ;;     test_imgfile test_ioctl test_kqueue test_linuxaudiodev test_macos
       ;;     test_macostools test_msilib test_nis test_ossaudiodev
       ;;     test_scriptpackages
       ;; 6 skips unexpected on linux2:
       ;;     test_bsddb test_bsddb3 test_crypt test_gdb test_idle test_ioctl
       ;; One of the typical errors:
       ;; test_unicode
       ;; test test_unicode crashed -- <type 'exceptions.OSError'>: [Errno 2] No
       ;; such file or directory
       #:test-target "test"
       #:configure-flags
       (list "--enable-shared"                    ;allow embedding
             "--with-system-ffi"                  ;build ctypes
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
    (home-page "http://python.org")
    (synopsis "High-level, dynamically-typed programming language")
    (description
     "Python is a remarkably powerful dynamic programming language that
is used in a wide variety of application domains.  Some of its key
distinguishing features include: clear, readable syntax; strong
introspection capabilities; intuitive object orientation; natural
expression of procedural code; full modularity, supporting hierarchical
packages; exception-based error handling; and very high level dynamic
data types.")
    (license psfl)))

(define-public python
  (package (inherit python-2)
    (version "3.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (patches (map search-patch
                            '("python-fix-tests.patch"
                              ;; XXX Try removing this patch for python > 3.4.3
                              "python-disable-ssl-test.patch"
                              "python-3-deterministic-build-info.patch"
                              "python-3-search-paths.patch")))
              (patch-flags '("-p0"))
              (sha256
               (base32
                "1f4nm4z08sy0kqwisvv95l02crv6dyysdmx44p1mz3bn6csrdcxm"))))
    (arguments (substitute-keyword-arguments (package-arguments python-2)
                 ((#:tests? _) #t)))
    (native-search-paths
     (list (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append "lib/python"
                                        (version-major+minor version)
                                        "/site-packages"))))))))

;; Minimal variants of Python, mostly used to break the cycle between Tk and
;; Python (Tk -> libxcb -> Python.)

(define-public python2-minimal
  (package (inherit python-2)
    (name "python-minimal")
    (outputs '("out"))
    (arguments
     (substitute-keyword-arguments (package-arguments python-2)
       ((#:configure-flags cf)
        `(append ,cf '("--without-system-ffi")))))
    (inputs '())))                          ;none of the optional dependencies

(define-public python-minimal
  (package (inherit python)
    (name "python-minimal")
    (outputs '("out"))
    (arguments
     (substitute-keyword-arguments (package-arguments python)
       ((#:configure-flags cf)
        `(append ,cf '("--without-system-ffi")))))

    ;; OpenSSL is a mandatory dependency of Python 3.x, for urllib;
    ;; zlib is required by 'zipimport', used by pip.
    (inputs `(("openssl" ,openssl)
              ("zlib" ,zlib)))))

(define* (wrap-python3 python #:optional (name "python-wrapper"))
  (package (inherit python)
    (name name)
    (source #f)
    (build-system trivial-build-system)
    (outputs '("out"))
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
                  `("python3", "pydoc3", "idle3")
                  `("python",  "pydoc",  "idle"))))))
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
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/psutil/psutil-"
             version ".tar.gz"))
       (sha256
        (base32
         "00c8h1mzqysih99z8pnbmdv117d2naldf11yjy50dhykxsf3n89z"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://pypi.python.org/pypi/psutil/")
    (synopsis "Library for retrieving information on running processes")
    (description
     "psutil (Python system and process utilities) is a library for retrieving
information on running processes and system utilization (CPU, memory, disks,
network) in Python.  It is useful mainly for system monitoring, profiling and
limiting process resources and management of running processes.  It implements
many functionalities offered by command line tools such as: ps, top, lsof,
netstat, ifconfig, who, df, kill, free, nice, ionice, iostat, iotop, uptime,
pidof, tty, taskset, pmap.")
    (license bsd-3)))

(define-public python2-psutil
  (package-with-python2 python-psutil))

(define-public python-passlib
  (package
    (name "python-passlib")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/passlib/passlib-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0b9rd161b3mmiwd7nx1v599yh9sp07mlfwac65sjy9qn1l0gd1z9"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-setuptools" ,python-setuptools)))
    (inputs
     `(("python-py-bcrypt" ,python-py-bcrypt)))
    (arguments
     `(#:phases
       (alist-cons-before
        'check 'set-PYTHON_EGG_CACHE
        ;; some tests require access to "$HOME/.cython"
        (lambda* _ (setenv "PYTHON_EGG_CACHE" "/tmp"))
         %standard-phases)))
    (home-page "https://bitbucket.org/ecollins/passlib")
    (synopsis
     "Comprehensive password hashing framework")
    (description
     "Passlib is a password hashing library for Python 2 & 3, which provides
cross-platform implementations of over 30 password hashing algorithms, as well
as a framework for managing existing password hashes.  It's designed to be
useful for a wide range of tasks, from verifying a hash found in /etc/shadow,
to providing full-strength password hashing for multi-user application.")
    (license bsd-3)))

(define-public python2-passlib
  (package-with-python2 python-passlib))

(define-public python-py-bcrypt
  (package
    (name "python-py-bcrypt")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/py-bcrypt/py-bcrypt-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0y6smdggwi5s72v6p1nn53dg6w05hna3d264cq6kas0lap73p8az"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://code.google.com/p/py-bcrypt")
    (synopsis
     "Bcrypt password hashing and key derivation")
    (description
     "A python wrapper of OpenBSD's Blowfish password hashing code.  This
system hashes passwords using a version of Bruce Schneier's Blowfish block
cipher with modifications designed to raise the cost of off-line password
cracking and frustrate fast hardware implementation.  The computation cost of
the algorithm is parametised, so it can be increased as computers get faster.
The intent is to make a compromise of a password database less likely to
result in an attacker gaining knowledge of the plaintext passwords (e.g. using
John the Ripper).")
    ;; "sha2.c" is under BSD-3;
    ;; "blowfish.c" and "bcrypt.c" are under BSD-4;
    ;; the rest is under ISC.
    (license (list isc bsd-3 bsd-4))))

(define-public python2-py-bcrypt
  (package-with-python2 python-py-bcrypt))


(define-public python-paramiko
  (package
    (name "python-paramiko")
    (version "1.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/paramiko/paramiko-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0mbfzm9zlrz6mla9xakrm8wkll3x035f9rj3c5pbgjzfldqscmjg"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (propagated-inputs
     `(("python-pycrypto" ,python-pycrypto)))
    (inputs
     `(("python-ecdsa" ,python-ecdsa)))
    (home-page "http://www.paramiko.org/")
    (synopsis "SSHv2 protocol library")
    (description "Paramiko is a python implementation of the SSHv2 protocol,
providing both client and server functionality.  While it leverages a Python C
extension for low level cryptography (PyCrypto), Paramiko itself is a pure
Python interface around SSH networking concepts.")
    (license lgpl2.1+)))

(define-public python2-paramiko
  (package-with-python2 python-paramiko))


(define-public python-httplib2
  (package
    (name "python-httplib2")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/h/httplib2/httplib2-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1xc3clbrf77r0600kja71j7hk1218sjiq0gfmb8vjdajka8kjqxw"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page
     "https://github.com/jcgregorio/httplib2")
    (synopsis "Comprehensive HTTP client library")
    (description
     "A comprehensive HTTP client library supporting many features left out of
other HTTP libraries.")
    (license license:expat)))

(define-public python2-httplib2
  (package-with-python2 python-httplib2))

(define-public python-ecdsa
  (package
    (name "python-ecdsa")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/e/ecdsa/ecdsa-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1yj31j0asmrx4an9xvsaj2icdmzy6pw0glfpqrrkrphwdpi1xkv4"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (inputs
     `(("openssl" ,openssl)))
    (home-page
     "http://github.com/warner/python-ecdsa")
    (synopsis
     "ECDSA cryptographic signature library (pure python)")
    (description
     "This is an easy-to-use implementation of ECDSA cryptography (Elliptic
Curve Digital Signature Algorithm), implemented purely in Python.  With this
library, you can quickly create keypairs (signing key and verifying key), sign
messages, and verify the signatures.  The keys and signatures are very short,
making them easy to handle and incorporate into other protocols.")
    (license license:expat)))

(define-public python2-ecdsa
  (package-with-python2 python-ecdsa))

(define-public python-ccm
  (package
    (name "python-ccm")
    (version "2.0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/c/ccm/ccm-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "199jw221albs2iv6xczczq88fxnh0aw8hzmys8qkbzkd99dssng9"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-six" ,python-six)))
    (home-page "https://github.com/pcmanus/ccm")
    (synopsis "Cassandra Cluster Manager")
    (description "A script/library to create, launch and remove an Apache
Cassandra cluster on localhost.")
    (license asl2.0)))

(define-public python2-ccm
  (package-with-python2 python-ccm))

(define-public python-pytz
  (package
    (name "python-pytz")
    (version "2013b")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://launchpad.net/pytz/main/" version
                          "/+download/pytz-" version ".tar.bz2"))
      (sha256
       (base32
        "19giwgfcrg0nr1gdv49qnmf2jb2ilkcfc7qyqvfpz4dp0p64ksv5"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; no test target
    (home-page "https://launchpad.net/pytz")
    (synopsis "Python timezone library")
    (description
     "This library allows accurate and cross platform timezone calculations
using Python 2.4 or higher and provides access to the Olson timezone database.")
    (license x11)))

(define-public python2-pytz
  (package-with-python2 python-pytz))


(define-public python-babel
  (package
    (name "python-babel")
    (version "2.1.1")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "Babel" version))
      (sha256
       (base32
        "0j2jgfzj1a2m39pm2qc36fzr7a6p5ybwndi0xdzhi2p8zw7dbdkz"))))
    (build-system python-build-system)
    (inputs
     `(("python-pytz" ,python-pytz)
       ("python-setuptools" ,python-setuptools)))
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
    (license bsd-3)))

(define-public python2-babel
  (package-with-python2 python-babel))

(define-public python2-backport-ssl-match-hostname
  (package
    (name "python2-backport-ssl-match-hostname")
    (version "3.4.0.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "https://pypi.python.org/packages/source/b/"
            "backports.ssl_match_hostname/backports.ssl_match_hostname-"
            version ".tar.gz"))
      (sha256
       (base32
        "1bnn47ipvhy49n0m50v27lp4xj6sqdkdw676ypd7pawsn1zhwh87"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (inputs
     `(("python2-setuptools" ,python2-setuptools)))
    (home-page "https://pypi.python.org/pypi/backports.ssl_match_hostname")
    (synopsis "Backport of ssl.match_hostname() function from Python 3.4")
    (description
     "This backport brings the ssl.match_hostname() function to users of
earlier versions of Python.  The function checks the hostname in the
certificate returned by the server to which a connection has been established,
and verifies that it matches the intended target hostname.")
    (license psfl)))

(define-public python-h5py
  (package
    (name "python-h5py")
    (version "2.4.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/h/h5py/h5py-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0q4f9l8grf6pwp64xbv8bmyxx416s7h4522nnxac056ap3savbps"))))
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
     `(("python-numpy" ,python-numpy)))
    (inputs
     `(("hdf5" ,hdf5)))
    (native-inputs
     `(("python-cython" ,python-cython)))
    (home-page "http://www.h5py.org/")
    (synopsis "Read and write HDF5 files from Python")
    (description
     "The h5py package provides both a high- and low-level interface to the
HDF5 library from Python.  The low-level interface is intended to be a
complete wrapping of the HDF5 API, while the high-level component supports
access to HDF5 files, datasets and groups using established Python and NumPy
concepts.")
    (license bsd-3)))

(define-public python2-h5py
  (let ((h5py (package-with-python2 python-h5py)))
    (package (inherit h5py)
      (propagated-inputs
       `(("python2-numpy" ,python2-numpy)
         ,@(alist-delete
            "python-numpy"
            (package-propagated-inputs h5py)))))))

(define-public python-lockfile
  (package
    (name "python-lockfile")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/l/lockfile/"
                           "lockfile-" version ".tar.gz"))
       (sha256
        (base32
         "0iwif7i84gwpvrnpv4brshdk8j6l77smvknm8k3bg77mj6f5ini3"))))
    (build-system python-build-system)
    (arguments '(#:test-target "check"))
    (home-page "http://code.google.com/p/pylockfile/")
    (synopsis "Platform-independent file locking module")
    (description
     "The lockfile package exports a LockFile class which provides a simple
API for locking files.")
    (license license:expat)))

(define-public python2-lockfile
  (package-with-python2 python-lockfile))

(define-public python-mock
  (package
    (name "python-mock")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/m/mock/"
                           "mock-" version ".tar.gz"))
       (sha256
        (base32
         "0kzlsbki6q0awf89rc287f3aj8x431lrajf160a70z0ikhnxsfdq"))))
    (build-system python-build-system)
    (arguments '(#:test-target "check"))
    (home-page "http://code.google.com/p/mock/")
    (synopsis "Python mocking and patching library for testing")
    (description
     "Mock is a library for testing in Python.  It allows you to replace parts
of your system under test with mock objects and make assertions about how they
have been used.")
    (license license:expat)))

(define-public python2-mock
  (package-with-python2 python-mock))


(define-public python-setuptools
  (package
    (name "python-setuptools")
    (version "18.3.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/s/setuptools/setuptools-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0kc7rbav00ks6iaw14p38y81q12fx0lpkhgf5m97xc04f5r318ig"))))
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
    (license psfl)))

(define-public python2-setuptools
  (package-with-python2 python-setuptools))


(define-public python-pycrypto
  (package
    (name "python-pycrypto")
    (version "2.6.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/p/"
                          "pycrypto/pycrypto-" version ".tar.gz"))
      (sha256
       (base32
        "0g0ayql5b9mkjam8hym6zyg6bv77lbh66rv1fyvgqb17kfc1xkpj"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (inputs
     `(("python" ,python)
       ("gmp" ,gmp)))
    (arguments
     `(#:phases
       (alist-cons-before
        'build 'set-build-env
        ;; pycrypto runs an autoconf configure script behind the scenes
        (lambda _
          (setenv "CONFIG_SHELL" (which "bash")))
        %standard-phases)))
    (home-page "http://www.pycrypto.org/")
    (synopsis "Cryptographic modules for Python")
    (description
     "Pycrypto is a collection of both secure hash functions (such as SHA256
and RIPEMD160), and various encryption algorithms (AES, DES, RSA, ElGamal,
etc.).  The package is structured to make adding new modules easy.")
    (license public-domain)))

(define-public python2-pycrypto
  (let ((pycrypto (package-with-python2 python-pycrypto)))
    (package (inherit pycrypto)
      (inputs
       `(("python" ,python-2)
         ,@(alist-delete
            "python"
            (package-inputs pycrypto)))))))

(define-public python-keyring
  (package
    (name "python-keyring")
    (version "3.8")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/k/"
                          "keyring/keyring-" version ".zip"))
      (sha256
       (base32
        "1vxazfbcwggyfyramh55shkxs08skhpqrkm6lrrjnygnm8c1l2zg"))))
    (build-system python-build-system)
    (native-inputs
     `(("unzip" ,unzip)
       ("python-setuptools" ,python-setuptools)
       ("python-mock" ,python-mock)))
    (inputs
     `(("python-pycrypto" ,python-pycrypto)))
    (arguments
     `(#:tests? #f                      ;TODO: tests require pytest
       #:phases
       (alist-replace
        'unpack
        (lambda _
          (let ((unzip (string-append (assoc-ref %build-inputs "unzip")
                                      "/bin/unzip"))
                (source (assoc-ref %build-inputs "source")))
            (and (zero? (system* unzip source))
                 (chdir (string-append "keyring-" ,version)))))
        %standard-phases)))
    (home-page "http://bitbucket.org/kang/python-keyring-lib")
    (synopsis "Store and access your passwords safely")
    (description
     "The Python keyring lib provides a easy way to access the system keyring
service from python.  It can be used in any application that needs safe
password storage.")
    ;; "MIT" and PSF dual license
    (license x11)))

(define-public python2-keyring
  (let ((keyring (package-with-python2 python-keyring)))
    (package (inherit keyring)
      (inputs
       `(("python2-pycrypto" ,python2-pycrypto))))))

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
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://pypi.python.org/pypi/six/")
    (synopsis "Python 2 and 3 compatibility utilities")
    (description
     "Six is a Python 2 and 3 compatibility library.  It provides utility
functions for smoothing over the differences between the Python versions with
the goal of writing Python code that is compatible on both Python versions.
Six supports every Python version since 2.5.  It is contained in only one
Python file, so it can be easily copied into your project.")
    (license x11)))

(define-public python2-six
  (package-with-python2 python-six))

(define-public python-dateutil-2
  (package
    (name "python-dateutil")
    (version "2.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/p/"
                          name "/" name "-" version ".tar.gz"))
      (sha256
       (base32
        "0s74ad6r789810s10dxgvaf48ni6adac2icrdad34zxygqq6bj7f"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-six" ,python-six)))
    (home-page "http://labix.org/python-dateutil")
    (synopsis "Extensions to the standard datetime module")
    (description
     "The dateutil module provides powerful extensions to the standard
datetime module, available in Python 2.3+.")
    (license bsd-3)))

(define-public python2-dateutil-2
  (package-with-python2 python-dateutil-2))

(define-public python-dateutil
  (package
    (name "python-dateutil")
    (version "1.5") ; last version for python < 3
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://labix.org/download/python-dateutil/"
                          "python-dateutil-" version ".tar.gz"))
      (sha256
       (base32
        "0fqfglhy5khbvsipr3x7m6bcaqljh8xl5cw33vbfxy7qhmywm2n0"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://labix.org/python-dateutil")
    (synopsis "Extensions to the standard datetime module")
    (description
     "The dateutil module provides powerful extensions to the standard
datetime module, available in Python 2.3+.")
    (license psfl)))

(define-public python2-dateutil
  (package-with-python2 python-dateutil))

(define-public python-parsedatetime
  (package
    (name "python-parsedatetime")
    (version "1.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/p/"
                          "parsedatetime/parsedatetime-" version ".tar.gz"))
      (sha256
       (base32
        "1as0mm4ql3z0324nc9bys2s1ngh507i317p16b79rx86wlmvx9ix"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://github.com/bear/parsedatetime/")
    (synopsis
     "Parse human-readable date/time text")
    (description
     "Parse human-readable date/time text.")
    (license asl2.0)))

(define-public python2-parsedatetime
  (package-with-python2 python-parsedatetime))

(define-public python-pandas
  (package
    (name "python-pandas")
    (version "0.16.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/p/"
                          "pandas/pandas-" version ".tar.gz"))
      (sha256
       (base32 "1wfrp8dx1zcsry6f09ndza6qm1yr7f163211f4l9vjlnhxpxw4s0"))))
    (build-system python-build-system)
    (arguments
     `(;; Three tests fail:
       ;; - test_read_google
       ;; - test_read_yahoo
       ;; - test_month_range_union_tz_dateutil
       #:tests? #f))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pytz" ,python-pytz)
       ("python-dateutil" ,python-dateutil-2)))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-setuptools" ,python-setuptools)))
    (home-page "http://pandas.pydata.org")
    (synopsis "Data structures for data analysis, time series, and statistics")
    (description
     "Pandas is a Python package providing fast, flexible, and expressive data
structures designed to make working with structured (tabular,
multidimensional, potentially heterogeneous) and time series data both easy
and intuitive.  It aims to be the fundamental high-level building block for
doing practical, real world data analysis in Python.")
    (license bsd-3)))

(define-public python2-pandas
  (let ((pandas (package-with-python2 python-pandas)))
    (package (inherit pandas)
             (propagated-inputs
              `(("python2-numpy" ,python2-numpy)
                ,@(alist-delete "python-numpy"
                                (package-propagated-inputs pandas)))))))

(define-public python-tzlocal
  (package
    (name "python-tzlocal")
    (version "1.1.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/t/"
                          "tzlocal/tzlocal-" version ".zip"))
      (sha256
       (base32
        "1m3y918c3chf41fwg2bx4w42bqsjzn3dyvvcmwwy13c8gj6zssv9"))))
    (build-system python-build-system)
    (native-inputs
     `(("unzip" ,unzip)
       ("python-setuptools" ,python-setuptools)))
    (inputs `(("python-pytz" ,python-pytz)))
    (arguments
     `(#:phases
       (alist-replace
        'unpack
        (lambda _
          (let ((unzip (string-append (assoc-ref %build-inputs "unzip")
                                      "/bin/unzip"))
                (source (assoc-ref %build-inputs "source")))
            (and (zero? (system* unzip source))
                 (chdir (string-append "tzlocal-" ,version)))))
        %standard-phases)))
    (home-page "https://github.com/regebro/tzlocal")
    (synopsis
     "Local timezone information for Python")
    (description
     "Tzlocal returns a tzinfo object with the local timezone information.
This module attempts to fix a glaring hole in pytz, that there is no way to
get the local timezone information, unless you know the zoneinfo name, and
under several distributions that's hard or impossible to figure out.")
    (license cc0)))

(define-public python-pysam
  (package
    (name "python-pysam")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/p/pysam/pysam-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1fb6i6hbpzxaxb62kyyp5alaidwhj40f7c6gwbhr6njzlqd5l459"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; tests are excluded in the manifest
       #:phases
       (alist-cons-before
        'build 'set-flags
        (lambda _
          (setenv "LDFLAGS" "-lncurses")
          (setenv "CFLAGS" "-D_CURSES_LIB=1"))
        %standard-phases)))
    (inputs
     `(("python-cython"     ,python-cython)
       ("python-setuptools" ,python-setuptools)
       ("ncurses"           ,ncurses)
       ("zlib"              ,zlib)))
    (home-page "https://github.com/pysam-developers/pysam")
    (synopsis "Python bindings to the SAMtools C API")
    (description
     "Pysam is a Python module for reading and manipulating files in the
SAM/BAM format.  Pysam is a lightweight wrapper of the SAMtools C API.  It
also includes an interface for tabix.")
    (license license:expat)))

(define-public python2-pysam
  (package-with-python2 python-pysam))

(define-public python2-pysqlite
  (package
    (name "python2-pysqlite")
    (version "2.6.3a")                            ; see below
    (source
     (origin
      (method url-fetch)
      ;; During the switch from code.google.com to pypi.python.org, the 2.6.3
      ;; tarball was modified, but the version number was kept:
      ;; <https://lists.gnu.org/archive/html/guix-devel/2014-02/msg00077.html>.
      ;; Here we want to refer to the pypi-hosted 2.6.3 tarball.
      (uri (string-append
            "https://pypi.python.org/packages/source/p/pysqlite/pysqlite-"
            "2.6.3" ".tar.gz"))
      (sha256
       (base32
        "13djzgnbi71znjjyaw4nybg6smilgszcid646j5qav7mdchkb77y"))))
    (build-system python-build-system)
    (inputs
     `(("sqlite" ,sqlite)))
    (arguments
     `(#:python ,python-2 ; incompatible with Python 3
       #:tests? #f)) ; no test target
    (home-page "https://pypi.python.org/pypi/pysqlite")
    (synopsis "SQLite bindings for Python")
    (description
     "Pysqlite provides SQLite bindings for Python that comply to the
Database API 2.0T.")
    (license license:zlib)))


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
    (inputs
     `(("python2-setuptools" ,python2-setuptools)))
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
    (license (non-copyleft "file://COPYING"
                           "See COPYING in the distribution."))))


(define-public python-simplejson
  (package
    (name "python-simplejson")
    (version "3.3.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/s/simplejson/simplejson-"
                          version ".tar.gz"))
      (sha256
       (base32
        "07wsry5j44l5zzm74l4j2bvasiq8n5m32f31n2p7c68i5vc6p2ks"))))
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
    (license x11)))

(define-public python2-simplejson
  (package-with-python2 python-simplejson))


(define-public python2-pyicu
  (package
    (name "python2-pyicu")
    (version "1.8")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/P/PyICU/PyICU-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1y361x82lnh9k9srmdx3q92z5iag112z7r5fxm0n1sfwb349yjdw"))))
    (build-system python-build-system)
    (inputs
     `(("icu4c" ,icu4c)))
    (arguments
     `(#:python ,python-2 ; Python 3 works also, but needs special care for
                          ; linking with libpython3.3m
       #:tests? #f)) ; no check target
    (home-page "http://pyicu.osafoundation.org/")
    (synopsis "Python extension wrapping the ICU C++ API")
    (description
     "PyICU is a python extension wrapping the ICU C++ API.")
    (license x11)))

(define-public python2-dogtail
  ;; Python 2 only, as it leads to "TabError: inconsistent use of tabs and
  ;; spaces in indentation" with Python 3.
  (package
    (name "python2-dogtail")
    (version "0.8.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://fedorahosted.org/released/dogtail/dogtail-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1yc4cg7ip87z15gyd4wy2vzbywrjc52a3m8r8gqy2b50d65llcg1"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2
                 #:tests? #f))                    ; invalid command "test"
    (home-page "https://fedorahosted.org/dogtail/")
    (synopsis "GUI test tool and automation framework written in ​Python")
    (description
     "Dogtail is a GUI test tool and automation framework written in Python.
It uses Accessibility (a11y) technologies to communicate with desktop
applications. dogtail scripts are written in Python and executed like any
other Python program.")
    (license gpl2+)))

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
       #:phases (alist-replace
                 'check
                 (lambda _
                   (zero? (system* "./test.sh")))
                 %standard-phases)))
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
    (license lgpl2.1+)))

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
    (license (x11-style "http://docs.python.org/2/license.html"
                        "Like \"CWI LICENSE AGREEMENT FOR PYTHON \
0.9.0 THROUGH 1.2\"."))))

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
              (patches (map search-patch
                            (list "pybugz-stty.patch"
                                  "pybugz-encode-error.patch")))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2                         ; SyntaxError with Python 3
       #:tests? #f))                              ; no 'test' sub-command
    (inputs `(("element-tree" ,python2-element-tree)))
    (synopsis "Python and command-line interface to Bugzilla")
    (description
     "PyBugz is a Python library and command-line tool to query the Bugzilla
bug tracking system.  It is meant as an aid to speed up interaction with the
bug tracker.")
    (home-page "http://www.liquidx.net/pybugz/")
    (license gpl2)))

(define-public python-enum34
  (package
    (name "python-enum34")
    (version "1.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/e/"
                          "enum34/enum34-" version ".tar.gz"))
      (sha256
       (base32
        "0dg6mpg9n4g9diyrbnbb5vd9d1qw9f265zwhknqy0mxh0cvmjjrq"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (arguments
     `(#:phases
       (alist-replace
        'check
        (lambda _ (zero? (system* "python" "enum/test_enum.py")))
        %standard-phases)))
    (home-page "https://pypi.python.org/pypi/enum34")
    (synopsis "Backported Python 3.4 Enum")
    (description
     "Enum34 is the new Python stdlib enum module available in Python 3.4
backported for previous versions of Python from 2.4 to 3.3.")
    (license bsd-3)))

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
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-six" ,python-six)
       ("python-parse" ,python-parse)
       ("python-enum34" ,python-enum34))) ;required for python<3.4
    (arguments '(#:tests? #f))            ;TODO: tests require pytest
    (home-page "https://github.com/jenisys/parse_type")
    (synopsis "Extended parse module")
    (description
     "Parse_type extends the python parse module.")
    (license bsd-3)))

(define-public python-parse
  (package
    (name "python-parse")
    (version "1.6.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/p/"
                          "parse/parse-" version ".tar.gz"))
      (sha256
       (base32
        "0m30q64l6szl7s9mhvqy64w2fdhdn8lb91fmacjiwbv3479cmk57"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (alist-replace
        'check
        (lambda _ (zero? (system* "python" "test_parse.py")))
        %standard-phases)))
    (home-page "https://github.com/r1chardj0n3s/parse")
    (synopsis "Parse strings")
    (description
     "Parse strings using a specification based on the Python format()
syntax.")
    (license x11)))


(define-public scons
  (package
    (name "scons")
    (version "2.3.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/scons/scons-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0hdlci43wjz8maryj83mz04ir6rwcdrrzpd7cpzvdlzycqhdfmsb"))))
    (build-system python-build-system)
    (arguments
     ;; With Python 3.x, fails to build with a syntax error.
     `(#:python ,python-2
       #:tests? #f))                       ; no 'python setup.py test' command
    (home-page "http://scons.org/")
    (synopsis "Software construction tool written in Python")
    (description
     "SCons is a software construction tool.  Think of SCons as an improved,
cross-platform substitute for the classic Make utility with integrated
functionality similar to autoconf/automake and compiler caches such as ccache.
In short, SCons is an easier, more reliable and faster way to build
software.")
    (license x11)))

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
    (inputs
     `(("python-setuptools" ,python-setuptools)))
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
    (inputs
     `(("python-setuptools" ,python-setuptools)))
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

(define-public python-nose
  (package
    (name "python-nose")
    (version "1.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/n/nose/nose-"
               version ".tar.gz"))
        (sha256
          (base32
            "00qymfgwg4iam4xi0w9bnv7lcb3fypq1hzfafzgs1rfmwaj67g3n"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (arguments
     '(#:tests? #f)) ; FIXME: test suite fails
    (home-page "http://readthedocs.org/docs/nose/")
    (synopsis "Python testing library")
    (description
     "Nose extends the unittest library to make testing easier.")
    (license lgpl2.0+)))

(define-public python2-nose
  (package-with-python2 python-nose))

(define-public python-unittest2
  (package
    (name "python-unittest2")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/u/unittest2py3k/unittest2py3k-"
             version ".tar.gz"))
       (sha256
        (base32
         "00yl6lskygcrddx5zspkhr0ibgvpknl4678kkm6s626539grq93q"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://pypi.python.org/pypi/unittest2")
    (synopsis "Python unit testing library")
    (description
     "Unittest2 is a replacement for the unittest module in the Python
standard library.")
    (license psfl)))

(define-public python2-unittest2
  (package (inherit python-unittest2)
    (name "python2-unittest2")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/u/unittest2/unittest2-"
             version ".tar.gz"))
       (sha256
        (base32
         "0wbs4i4x3x7klr3v35ss6p9mcqz883i1xgcpkhvl7n2lyv6yhpda"))))
    (inputs
     `(("python2-setuptools" ,python2-setuptools)))
    (arguments
     `(#:python ,python-2
       #:tests? #f)))) ; no setup.py test command

(define-public python-py
  (package
    (name "python-py")
    (version "1.4.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/py/py-"
             version ".tar.gz"))
       (sha256
        (base32
         "1jkhffpai419v5rickm2vz86p9bkg3b3kcm2k4bi5wfajhw2m3xs"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://pylib.readthedocs.org/")
    (synopsis "Python library for parsing, I/O, instrospection, and logging")
    (description
     "Py is a Python library for file name parsing, .ini file parsing, I/O,
code introspection, and logging.")
    (license license:expat)))

(define-public python2-py
  (package-with-python2 python-py))

(define-public python-pytest
  (package
    (name "python-pytest")
    (version "2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pytest/pytest-"
             version ".tar.gz"))
       (sha256
        (base32
         "0g2w4p0n42wvz8rq4k6gnzpkakgz3g8sfanxk8jrsra9675snkcr"))
       (modules '((guix build utils)))
       (snippet
        ;; One of the tests involves the /usr directory, so it fails.
        '(substitute* "testing/test_argcomplete.py"
           (("def test_remove_dir_prefix\\(self\\):")
            "@pytest.mark.xfail\n    def test_remove_dir_prefix(self):")))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-py" ,python-py)
       ("python-nose" ,python-nose)
       ("python-mock" ,python-mock)))
    (home-page "http://pytest.org")
    (synopsis "Python testing library")
    (description
     "Pytest is a testing tool that provides auto-discovery of test modules
and functions, detailed info on failing assert statements, modular fixtures,
and many external plugins.")
    (license license:expat)))

(define-public python2-pytest
  (package-with-python2 python-pytest))

(define-public python-pytest-runner
  (package
    (name "python-pytest-runner")
    (version "2.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/p/"
                           "pytest-runner/pytest-runner-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1nwcqx0l3fv52kv8526wy8ypzghbq96c96di318d98d3wh7a8xg7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The fancy way of setting the version with setuptools_scm does not
         ;; seem to work here.
         (add-after 'unpack 'set-version
          (lambda _
            (substitute* "docs/conf.py"
              (("version = setuptools_scm\\.get_version\\(root='\\.\\.')")
               (string-append "version = \"" ,version "\"")))
            #t)))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools" ,python-setuptools)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://bitbucket.org/pytest-dev/pytest-runner")
    (synopsis "Invoke py.test as a distutils command")
    (description
     "This package provides a @command{pytest-runner} command that
@file{setup.py} files can use to run tests.")
    (license license:expat)))

(define-public python2-pytest-runner
  (package-with-python2 python-pytest-runner))

(define-public python-scripttest
  (package
    (name "python-scripttest")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/s/scripttest/scripttest-"
             version ".tar.gz"))
       (sha256
        (base32
         "0f4w84k8ck82syys7yg9maz93mqzc8p5ymis941x034v44jzq74m"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-pytest" ,python-pytest)))
    (home-page "http://pythonpaste.org/scripttest/")
    (synopsis "Python library to test command-line scripts")
    (description "Scripttest is a Python helper library for testing
interactive command-line applications.  With it you can run a script in a
subprocess and see the output as well as any file modifications.")
    (license license:expat)))

(define-public python2-scripttest
  (package-with-python2 python-scripttest))

(define-public python-testtools
  (package
    (name "python-testtools")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/t/testtools/testtools-"
             version ".tar.gz"))
       (sha256
        (base32
         "1dyml28ykpl5jb9khdmcdvhy1cxqingys6qvj2k04fzlaj6z3bbx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-mimeparse" ,python-mimeparse)))
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-extras" ,python-extras)))
    (home-page "https://github.com/testing-cabal/testtools")
    (synopsis
     "Extensions to the Python standard library unit testing framework")
    (description
     "Testtools extends the Python standard library unit testing framework to
provide matchers, more debugging information, and cross-Python
compatibility.")
    (license psfl)))

(define-public python2-testtools
  (package-with-python2 python-testtools))

(define-public python-testscenarios
  (package
    (name "python-testscenarios")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/t/testscenarios/testscenarios-"
             version ".tar.gz"))
       (sha256
        (base32
         "1671jvrvqlmbnc42j7pc5y6vc37q44aiwrq0zic652pxyy2fxvjg"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-testtools" ,python-testtools)
       ("python-mimeparse" ,python-mimeparse)))
    (home-page "https://launchpad.net/testscenarios")
    (synopsis "Pyunit extension for dependency injection")
    (description
     "Testscenarios provides clean dependency injection for Python unittest
style tests.")
    (license (list bsd-3 asl2.0)))) ; at the user's option

(define-public python2-testscenarios
  (package-with-python2 python-testscenarios))

(define-public python-testresources
  (package
    (name "python-testresources")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/t/testresources/testresources-"
             version ".tar.gz"))
       (sha256
        (base32
         "0cbj3plbllyz42c4b5xxgwaa7mml54lakslrn4kkhinxhdri22md"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://launchpad.net/testresources")
    (synopsis
     "Pyunit extension for managing test resources")
    (description
     "Testresources is an extension to Python's unittest to allow declarative
use of resources by test cases.")
    (license (list bsd-3 asl2.0)))) ; at the user's option

(define-public python2-testresources
  (package-with-python2 python-testresources))

(define-public python-subunit
  (package
    (name "python-subunit")
    (version "0.0.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/python-subunit/python-subunit-"
             version ".tar.gz"))
       (sha256
        (base32
         "1nkw9wfbvizmpajbj3in8ns07g7lwkiv8hip14jjlwk3cacls6jv"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-testtools" ,python-testtools)
       ("python-mimeparse" ,python-mimeparse)
       ("python-testscenarios" ,python-testscenarios)))
    (home-page "http://launchpad.net/subunit")
    (synopsis "Python implementation of the subunit protocol")
    (description
     "Python-subunit is a Python implementation of the subunit test streaming
protocol.")
    (license (list bsd-3 asl2.0)))) ; at the user's option

(define-public python2-subunit
  (package-with-python2 python-subunit))

;; Recent versions of python-fixtures need a recent version of python-pbr,
;; which needs a recent version of python-fixtures. To fix this circular
;; dependency, we keep old versions of python-fixtures and python-pbr to
;; bootstrap the whole thing:
;; - python-fixtures-0.3.16 is used to build python-pbr-0.11
;; - python-pbr-0.11 is used to build python-fixtures
;; - python-fixtures is used to build python-pbr
(define-public python-fixtures-0.3.16
  (package
    (name "python-fixtures")
    (version "0.3.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/f/fixtures/fixtures-"
             version ".tar.gz"))
       (sha256
        (base32
         "0x9r2gwilcig5g54k60bxzg96zabizq1855lrprlb4zckalp9asc"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (arguments
     '(#:tests? #f)) ; no setup.py test command
    (home-page "https://launchpad.net/python-fixtures")
    (synopsis "Python test fixture library")
    (description
     "Fixtures provides a way to create reusable state, useful when writing
Python tests.")
    (license (list bsd-3 asl2.0)))) ; at user's option

(define-public python2-fixtures-0.3.16
  (package-with-python2 python-fixtures-0.3.16))

(define-public python-pbr-0.11
  (package
    (name "python-pbr")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pbr/pbr-"
             version ".tar.gz"))
       (sha256
        (base32
         "0v9gb7gyqf7q9s99l0nnjj9ww9b0jvyqlwm4d56pcyinxydddw6p"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; Most tests seem to use the Internet.
    (inputs
      `(("python-fixtures-0.3.16" ,python-fixtures-0.3.16)
        ("python-pip" ,python-pip)
        ("python-setuptools" ,python-setuptools)))
    (home-page "https://launchpad.net/pbr")
    (synopsis "Change the default behavior of Python’s setuptools")
    (description
      "Python Build Reasonableness (PBR) is a library that injects some useful
and sensible default behaviors into your setuptools run.")
    (license asl2.0)))

(define-public python2-pbr-0.11
  (package-with-python2 python-pbr-0.11))

(define-public python-fixtures
  (package
    (name "python-fixtures")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/f/fixtures/fixtures-"
             version ".tar.gz"))
       (sha256
        (base32
         "1khpywdh91ijryhxjxiyyi5rmbimhl8hwbbf8lazhgzq6yxz6g5n"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-pbr-0.11" ,python-pbr-0.11)))
    (inputs
     `(("python-pip" ,python-pip)
       ("python-setuptools" ,python-setuptools)
       ;; Tests
       ("python-testtools" ,python-testtools)))
    (arguments
     '(#:tests? #f)) ; no setup.py test command
    (home-page "https://launchpad.net/python-fixtures")
    (synopsis "Python test fixture library")
    (description
     "Fixtures provides a way to create reusable state, useful when writing
Python tests.")
    (license (list bsd-3 asl2.0)))) ; at user's option

(define-public python2-fixtures
  (package-with-python2 python-fixtures))

(define-public python-testrepository
  (package
    (name "python-testrepository")
    (version "0.0.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/t/testrepository/testrepository-"
             version ".tar.gz"))
       (sha256
        (base32
         "1ssqb07c277010i6gzzkbdd46gd9mrj0bi0i8vn560n2k2y4j93m"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-fixtures-0.3.16" ,python-fixtures-0.3.16)
       ("python-testtools" ,python-testtools)))
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-subunit" ,python-subunit)
       ("python-mimeparse" ,python-mimeparse)))
    (home-page "https://launchpad.net/testrepository")
    (synopsis "Database for Python test results")
    (description "Testrepository provides a database of test results which can
be used as part of a developer's workflow to check things such as what tests
have failed since the last commit or what tests are currently failing.")
    (license (list bsd-3 asl2.0)))) ; at user's option

(define-public python2-testrepository
  (package-with-python2 python-testrepository))

(define-public python-coverage
  (package
    (name "python-coverage")
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/c/coverage/coverage-"
             version ".tar.gz"))
       (sha256
        (base32
         "0knlbq79g2ww6xzsyknj9rirrgrgc983dpa2d9nkdf31mb2a3bni"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://nedbatchelder.com/code/coverage")
    (synopsis "Code coverage measurement for Python")
    (description
     "Coverage measures code coverage, typically during test execution.  It
uses the code analysis tools and tracing hooks provided in the Python standard
library to determine which lines are executable, and which have been
executed.")
    (license bsd-3)))

(define-public python2-coverage
  (package-with-python2 python-coverage))

(define-public python-discover
  (package
    (name "python-discover")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/d/discover/discover-"
             version ".tar.gz"))
       (sha256
        (base32
         "0y8d0zwiqar51kxj8lzmkvwc3b8kazb04gk5zcb4nzg5k68zmhq5"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://pypi.python.org/pypi/discover/")
    (synopsis
     "Python test discovery for unittest")
    (description
     "Discover provides test discovery for unittest, a feature that has been
backported from Python 2.7 for Python 2.4+.")
    (license bsd-3)))

(define-public python2-discover
  (package-with-python2 python-discover))

(define-public behave
  (package
    (name "behave")
    (version "1.2.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://pypi.python.org/packages/source/b/"
                                 name "/" name "-" version ".tar.gz"))
             (sha256
              (base32
               "1v2rfy8xnf0rk7cj4cgr7lam4015d458i7bg0xqs9czfv6njlm14"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-six" ,python-six)
       ("python-enum43" ,python-enum34)
       ("python-parse" ,python-parse)
       ("python-parse-type" ,python-parse-type)))
    (arguments `(#:tests? #f))          ;TODO: tests require nose>=1.3 and
                                        ;PyHamcrest>=1.8
    (home-page "http://github.com/behave/behave")
    (synopsis "Python behavior-driven development")
    (description
     "Behave is a tool for behavior-driven development in python.
Behavior-driven development (or BDD) is an agile software development
technique that encourages collaboration between developers, QA and
non-technical or business participants in a software project.  Behave uses
tests written in a natural language style, backed up by Python code.")
    (license x11)))

(define-public python-exif-read
  (package
    (name "python-exif-read")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://pypi.python.org/packages/source/E/ExifRead/ExifRead-"
                version ".tar.gz"))
              (sha256
               (base32
                "17c627gcdmyc05hz4zk8qs4pjgw6rc68qzjzgz8gh1cmpsd7acf1"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (arguments `(#:tests? #f)) ; no tests
    (home-page "https://github.com/ianare/exif-py")
    (synopsis "Python library to extract EXIF data from image files")
    (description
     "ExifRead is a Python library to extract EXIF data from tiff and jpeg
files.")
    (license bsd-3)))

(define-public python2-exif-read
  (package-with-python2 python-exif-read))

(define-public python-pyld
  (package
    (name "python-pyld")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://pypi.python.org/packages/source/P/PyLD/PyLD-"
                version ".tar.gz"))
              (sha256
               (base32
                "1l9ymj85fsvayqplinzpk0kyiq6m74ps9xd3a9fhlxfn1rldf8x8"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (arguments `(#:tests? #f)) ; no tests
    (home-page "http://github.com/digitalbazaar/pyld")
    (synopsis "Python implementation of the JSON-LD specification")
    (description
     "PyLD is an implementation of the JSON-LD specification.")
    (license bsd-3)))

(define-public python2-pyld
  (package-with-python2 python-pyld))

(define-public python-certifi
  (package
    (name "python-certifi")
    (version "14.05.14")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://pypi.python.org/packages/source/c/certifi/certifi-"
                version ".tar.gz"))
              (sha256
               (base32
                "0s8vxzfz6s4m6fvxc7z25k9j35w0rh6jkw3wwcd1az1mssncn6qy"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (arguments `(#:tests? #f)) ; no tests
    (home-page "http://python-requests.org/")
    (synopsis "Python CA certificate bundle")
    (description
     "Certifi is a Python library that contains a CA certificate bundle, which
is used by the Requests library to verify HTTPS requests.")
    (license asl2.0)))

(define-public python2-certifi
  (package-with-python2 python-certifi))

(define-public python-click
  (package
    (name "python-click")
    (version "4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/c/click/click-"
             version ".tar.gz"))
       (sha256
        (base32 "0294x9g28w6zgswl0rsygkwi0wf6n480gf7fiiw5f9az3xhh77pl"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://click.pocoo.org")
    (synopsis "Command line library for Python")
    (description
     "Click is a Python package for creating command line interfaces in a
composable way with as little code as necessary.  Its name stands for
\"Command Line Interface Creation Kit\".  It's highly configurable but comes
with sensible defaults out of the box.")
    (license bsd-3)))

(define-public python2-click
  (package-with-python2 python-click))

(define-public python-requests
  (package
    (name "python-requests")
    (version "2.8.0")
    (source (origin
             (method url-fetch)
             (uri
              (string-append
               "https://pypi.python.org/packages/source/r/requests/requests-"
               version ".tar.gz"))
             (sha256
              (base32
               "0yrvj8hfnabrdxds59w6d6887sn5j0jlgpmcq04lk4k0kdc07w5j"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-certifi" ,python-certifi)))
    (arguments `(#:tests? #f)) ; no tests
    (home-page "http://python-requests.org/")
    (synopsis "Python HTTP library")
    (description
     "Requests is a Python HTTP client library.  It aims to be easier to use
than Python’s urllib2 library.")
    (license asl2.0)))

(define-public python2-requests
  (package-with-python2 python-requests))

(define-public python-jsonschema
  (package
    (name "python-jsonschema")
    (version "2.4.0")
    (source (origin
             (method url-fetch)
             (uri
              (string-append
               "https://pypi.python.org/packages/source/j/jsonschema/jsonschema-"
               version ".tar.gz"))
             (sha256
              (base32
               "1yik3031ziygvq66rj3mzfqdgxj29sg1bkfc46wsgi7lnbqs560j"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://github.com/Julian/jsonschema")
    (synopsis "Implementation of JSON Schema for Python")
    (description
     "Jsonschema is an implementation of JSON Schema for Python.")
    (license license:expat)))

(define-public python2-jsonschema
  (package-with-python2 python-jsonschema))

(define-public python-unidecode
  (package
    (name "python-unidecode")
    (version "0.04.16")
    (source (origin
             (method url-fetch)
             (uri
              (string-append
               "https://pypi.python.org/packages/source/U/Unidecode/Unidecode-"
               version ".tar.gz"))
             (sha256
              (base32
               "0yv56vc49rvippyxgxvcyz7jklc07ky38rcspax7p00sgmriiljc"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://pypi.python.org/pypi/Unidecode")
    (synopsis "ASCII transliterations of Unicode text")
    (description
     "Unidecode provides ASCII transliterations of Unicode text.  Unidecode is
useful when integrating with legacy code that doesn't support Unicode, or for
ease of entry of non-Roman names on a US keyboard, or when constructing ASCII
machine identifiers from human-readable Unicode strings that should still be
somewhat intelligeble.")
    (license gpl2+)))

(define-public python2-unidecode
  (package-with-python2 python-unidecode))

(define-public python-pyjwt
  (package
    (name "python-pyjwt")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyJWT" version))
       (sha256
        (base32
         "1556v2jppd8mjkkj66pxb5rcazm35jq81r233mdl8hfmz9n3icp1"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-pytest-runner" ,python-pytest-runner)))
    (arguments
     '(#:tests? #f)) ; test suite doesn't work
    (home-page "http://github.com/progrium/pyjwt")
    (synopsis "JSON Web Token implementation in Python")
    (description
     "PyJWT is a JSON Web Token implementation written in Python.")
    (license license:expat)))

(define-public python2-pyjwt
  (package-with-python2 python-pyjwt))

(define-public python-oauthlib
  (package
    (name "python-oauthlib")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "oauthlib" version))
              (sha256
               (base32
                "1bfrj70vdjxjw74khbyh6f0dksv7p5rh2346jnlrffyacd3gwjzg"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-coverage", python-coverage)
       ("python-nose" ,python-nose)
       ("python-mock" ,python-mock)))
    (inputs
     `(("python-blinker" ,python-blinker)
       ("python-cryptography" ,python-cryptography)
       ("python-pyjwt" ,python-pyjwt)))
    (home-page "https://github.com/idan/oauthlib")
    (synopsis "OAuth implementation for Python")
    (description
     "Oauthlib is a generic, spec-compliant, thorough implementation of the
OAuth request-signing logic.")
    (license bsd-3)))

(define-public python2-oauthlib
  (let ((base (package-with-python2 python-oauthlib)))
    (package
      (inherit base)
      (inputs
       `(("python2-unittest2" ,python2-unittest2)
         ("python2-cryptography" ,python2-cryptography)
         ,@(alist-delete "python-cryptography"
	                 (package-inputs base)))))))

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
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://github.com/mitsuhiko/itsdangerous")
    (synopsis "Python library for passing data to/from untrusted environments")
    (description
     "Itsdangerous provides various helpers to pass trusted data to untrusted
environments and back.")
    (license bsd-3)))

(define-public python2-itsdangerous
  (package-with-python2 python-itsdangerous))

(define-public python-pyyaml
  (package
    (name "python-pyyaml")
    (version "3.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/P/PyYAML/PyYAML-"
             version ".tar.gz"))
       (sha256
        (base32
         "1s26125vfnskng58ym37xhwv8v0mm95b2cwbjfag8prfhy596v63"))))
    (build-system python-build-system)
    (inputs
     `(("libyaml" ,libyaml)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
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
    (version "1.11.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/v/virtualenv/virtualenv-"
             version ".tar.gz"))
       (sha256
        (base32
         "1xq4prmg25n9cz5zcvbqx68lmc3kl39by582vd8pzs9f3qalqyiy"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
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
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://github.com/mitsuhiko/markupsafe")
    (synopsis "XML/HTML/XHTML markup safe string implementation for Python")
    (description
     "Markupsafe provides an XML/HTML/XHTML markup safe string implementation
for Python.")
    (license bsd-3)))

(define-public python2-markupsafe
  (package-with-python2 python-markupsafe))

(define-public python-jinja2
  (package
    (name "python-jinja2")
    (version "2.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/J/Jinja2/Jinja2-"
             version ".tar.gz"))
       (sha256
        (base32
         "1nwg9yfqgy421lncnm63k1zf9xkd1klc0jm0fr4p3dad01fsq91f"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-markupsafe" ,python-markupsafe)))
    (home-page "http://jinja.pocoo.org/")
    (synopsis "Python template engine")
    (description
     "Jinja2 is a small but fast and easy to use stand-alone template engine
written in pure Python.")
    (license bsd-3)))

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
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://defunkt.io/pystache/")
    (synopsis "Python logic-less template engine")
    (description
     "Pystache is a Python implementation of the framework agnostic,
logic-free templating system Mustache.")
    (license license:expat)))

(define-public python2-pystache
  (package-with-python2 python-pystache))

(define-public python-joblib
  (package
    (name "python-joblib")
    (version "0.9.0b4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pypi.python.org/packages/source/"
                                  "j/joblib/joblib-" version ".tar.gz"))
              (sha256
               (base32
                "1dvw3f8jgj6h0fxkghbgyclvdzc7l0ig7n0vis70awb5kczb9bs3"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-nose"       ,python-nose)))
    (home-page "http://pythonhosted.org/joblib/")
    (synopsis "Using Python functions as pipeline jobs")
    (description
     "Joblib is a set of tools to provide lightweight pipelining in Python.
In particular, joblib offers: transparent disk-caching of the output values
and lazy re-evaluation (memoize pattern), easy simple parallel computing
logging and tracing of the execution.")
    (license bsd-3)))

(define-public python2-joblib
  (package-with-python2 python-joblib))

(define-public python-docutils
  (package
    (name "python-docutils")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/d/docutils/docutils-"
             version ".tar.gz"))
       (sha256
        (base32
         "1ylnjnw1x4b2y7blr6x35ncdzn69k253kw4cdkv6asdb21w73ny7"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
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
    (license (list public-domain psfl bsd-2 gpl3+))))

(define-public python2-docutils
  (package-with-python2 python-docutils))

(define-public python-pygments
  (package
    (name "python-pygments")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/P/Pygments/Pygments-"
             version ".tar.gz"))
       (sha256
        (base32
         "1h11r6ss8waih51vcksfvzghfxiav2f8svc0812fa5kmyz5d97kr"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://pygments.org/")
    (synopsis "Syntax highlighting")
    (description
     "Pygments is a syntax highlighting package written in Python.")
    (license bsd-2)))

(define-public python2-pygments
  (package-with-python2 python-pygments))

(define-public python-sphinx
  (package
    (name "python-sphinx")
    (version "1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/S/Sphinx/Sphinx-"
             version ".tar.gz"))
       (sha256
        (base32
         "011xizm3jnmf4cvs5i6kgf6c5nn046h79i8j0vd0f27yw9j3p4wl"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-jinja2" ,python-jinja2)
       ("python-docutils" ,python-docutils)
       ("python-pygments" ,python-pygments)))
    (home-page "http://sphinx-doc.org/")
    (synopsis "Python documentation generator")
    (description "Sphinx is a tool that makes it easy to create documentation
for Python projects or other documents consisting of multiple reStructuredText
sources.")
    (license bsd-3)))

(define-public python2-sphinx
  (package-with-python2 python-sphinx))

(define-public python-sphinx-rtd-theme
  (package
    (name "python-sphinx-rtd-theme")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/s/"
                           "sphinx_rtd_theme/sphinx_rtd_theme-"
                           version ".tar.gz"))
       (sha256
        (base32
         "19nw3rn7awplcdrz63kg1njqwkbymfg9lwn7l2grhdyhyr2gaa8g"))))
    (build-system python-build-system)
    (arguments
     `(;; With standard flags, the install phase attempts to create a zip'd
       ;; egg file, and fails with an error: 'ZIP does not support timestamps
       ;; before 1980'
       #:configure-flags '("--single-version-externally-managed"
                           "--record=sphinx-rtd-theme.txt")))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (inputs
     `(("python-docutils" ,python-docutils)
       ("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/snide/sphinx_rtd_theme/")
    (synopsis "ReadTheDocs.org theme for Sphinx")
    (description "A theme for Sphinx used by ReadTheDocs.org.")
    (license license:expat)))

(define-public python2-sphinx-rtd-theme
  (package-with-python2 python-sphinx-rtd-theme))

(define-public python-feedgenerator
  (package
    (name "python-feedgenerator")
    (version "20150710.97185b7")
    (source
     ;; Using the git checkout for now because license file not added till
     ;; https://github.com/dmdm/feedgenerator-py3k/commit/97185b7566c240c4bf5ed80db7d6c271204dab39
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmdm/feedgenerator-py3k.git")
             (commit "97185b7566c240c4bf5ed80db7d6c271204dab39")))
       (sha256
        (base32
         "0dbd6apij5j1923ib905x0srgcyls4wlabqlwp4dzkwmksvnrr2a"))))
    (arguments
     `(;; With standard flags, the install phase attempts to create a zip'd
       ;; egg file, and fails with an error: 'ZIP does not support timestamps
       ;; before 1980'
       #:configure-flags '("--single-version-externally-managed"
                           "--record=feedgenerator.txt")))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-pytz" ,python-pytz)
       ("python-six" ,python-six)))
    (home-page
     "https://github.com/dmdm/feedgenerator-py3k.git")
    (synopsis
     "Standalone version of Django's Atom/RSS feed generator")
    (description
     "Feedgenerator-py3k is a standalone version of Django's feedgenerator,
which can produce feeds in RSS 2.0, RSS 0.91, and Atom formats.")
    (license bsd-3)))

(define-public python2-feedgenerator
  (package-with-python2 python-feedgenerator))

(define-public python-blinker
  (package
    (name "python-blinker")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/b/blinker/blinker-"
             version ".tar.gz"))
       (sha256
        (base32
         "0bvfxkmjx6bpa302pv7v2vw5rwr3dlzjzfdp3bj628i6144024b8"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    ;; No "test" command supplied to setuptools, so unless there's another way
    ;; to run tests, we're skipping them!
    (arguments '(#:tests? #f))
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
    (version "3.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pelican/pelican-"
             version ".tar.gz"))
       (sha256
        (base32
         "0lbkk902mqxpp452pp76n6qcjv6f99lq2zl204xmqyzcan9zr3ps"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (propagated-inputs
     `(("python-feedgenerator" ,python-feedgenerator)
       ("python-jinja2" ,python-jinja2)
       ("python-pygments" ,python-pygments)
       ("python-docutils" ,python-docutils)
       ("python-pytz" ,python-pytz)
       ("python-blinker" ,python-blinker)
       ("python-unidecode" ,python-unidecode)
       ("python-six" ,python-six)
       ("python-dateutil-2" ,python-dateutil-2)))
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
    (license agpl3+)))

(define-public python-scikit-learn
  (package
    (name "python-scikit-learn")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/scikit-learn/scikit-learn/archive/"
             version ".tar.gz"))
       (sha256
        (base32
         "140skabifgc7lvvj873pnzlwx0ni6q8qkrsyad2ccjb3h8rxzkih"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (alist-cons-before
        'check 'set-HOME
        ;; some tests require access to "$HOME"
        (lambda _ (setenv "HOME" "/tmp"))
        ;; Tests can only be run after the library has been installed and not
        ;; within the source directory.
        (alist-cons-after
         'install 'check
         (lambda _
           (with-directory-excursion "/tmp"
             ;; With Python 3 one test of 3334 fails
             ;; (sklearn.tests.test_common.test_transformers); see
             ;; https://github.com/scikit-learn/scikit-learn/issues/3693
             (system* "nosetests" "-v" "sklearn")))
         (alist-delete 'check %standard-phases)))))
    (inputs
     `(("openblas" ,openblas)
       ("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)))
    (home-page "http://scikit-learn.org/")
    (synopsis "Machine Learning in Python")
    (description
     "Scikit-learn provides simple and efficient tools for data
mining and data analysis.")
    (license bsd-3)))

(define-public python2-scikit-learn
  (let ((scikit (package-with-python2 python-scikit-learn)))
    (package (inherit scikit)
      (propagated-inputs
       `(("python2-numpy" ,python2-numpy)
         ("python2-scipy" ,python2-scipy)
         ,@(alist-delete
            "python-numpy"
            (alist-delete
             "python-scipy" (package-propagated-inputs scikit))))))))

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
    (propagated-inputs
     `(("python-matplotlib" ,python-matplotlib)
       ("python-networkx" ,python-networkx)
       ("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-six" ,python-six)
       ("python-pillow" ,python-pillow)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-setuptools" ,python-setuptools)))
    (home-page "http://scikit-image.org/")
    (synopsis "Image processing in Python")
    (description
     "Scikit-image is a collection of algorithms for image processing.")
    (license bsd-3)))

(define-public python2-scikit-image
  (let ((scikit-image (package-with-python2 python-scikit-image)))
    (package (inherit scikit-image)
      (native-inputs
       `(("python2-mock" ,python2-mock)
         ,@(package-native-inputs scikit-image)))
      (propagated-inputs
       `(("python2-pytz" ,python2-pytz)
         ("python2-matplotlib" ,python2-matplotlib)
         ("python2-numpy" ,python2-numpy)
         ("python2-scipy" ,python2-scipy)
         ,@(fold alist-delete (package-propagated-inputs scikit-image)
                 '("python-matplotlib" "python-numpy" "python-scipy")))))))

(define-public python-redis
  (package
    (name "python-redis")
    (version "2.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/r/redis/redis-"
             version ".tar.gz"))
       (sha256
        (base32 "1701qjwn4n05q90fdg4bsg96s27xf5s4hsb4gxhv3xk052q3gyx4"))))
    (build-system python-build-system)
    ;; Tests require a running Redis server
    (arguments '(#:tests? #f))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/andymccurdy/redis-py")
    (synopsis "Redis Python client")
    (description
     "This package provides a Python interface to the Redis key-value store.")
    (license license:expat)))

(define-public python2-redis
  (package-with-python2 python-redis))

(define-public python-rq
  (package
    (name "python-rq")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/r/rq/rq-"
             version ".tar.gz"))
       (sha256
        (base32 "0b0z5hn8wkfg300hx7816csgv3bcfamlr29fi3yzgqmpqxwj3fix"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-redis" ,python-redis)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://python-rq.org/")
    (synopsis "Simple job queues for Python")
    (description
     "RQ (Redis Queue) is a simple Python library for queueing jobs and
processing them in the background with workers.  It is backed by Redis and it
is designed to have a low barrier to entry.")
    (license bsd-2)))

(define-public python2-rq
  (package-with-python2 python-rq))

(define-public python-cython
  (package
    (name "python-cython")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cython.org/release/Cython-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0ddz2l2dvcy5hdkxx4xlfiwpccvwia7ixgcy4h0pdv46a4i4vxj3"))))
    (build-system python-build-system)
    ;; we need the full python package and not just the python-wrapper
    ;; because we need libpython3.3m.so
    (inputs
     `(("python" ,python)))
    (arguments
     `(#:phases
       (alist-cons-before
        'check 'set-HOME
        ;; some tests require access to "$HOME/.cython"
        (lambda* _ (setenv "HOME" "/tmp"))
        (alist-replace
         'check
         (lambda _ (zero? (system* "python" "runtests.py" "-vv")))
         %standard-phases))))
    (home-page "http://cython.org/")
    (synopsis "C extensions for Python")
    (description "Cython is an optimising static compiler for both the Python
programming language and the extended Cython programming language.  It makes
writing C extensions for Python as easy as Python itself.")
    (license asl2.0)))

(define-public python2-cython
  (package (inherit (package-with-python2 python-cython))
    (name "python2-cython")
    (inputs
     `(("python-2" ,python-2))))) ; this is not automatically changed

;; This version of numpy is missing the documentation and is only used to
;; build matplotlib which is required to build numpy's documentation.
(define python-numpy-bootstrap
  (package
    (name "python-numpy-bootstrap")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/numpy"
                           "/numpy-" version ".tar.gz"))
       (sha256
        (base32
         "070ybfvpgfmiz2hs94x445hvkh9dh52nyi0m8jp5kdihgvhbnx80"))))
    (build-system python-build-system)
    (inputs
     `(("python-nose" ,python-nose)
       ("openblas" ,openblas)
       ("lapack" ,lapack)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (arguments
     `(#:phases
       (alist-cons-before
        'build 'set-environment-variables
        (lambda* (#:key inputs #:allow-other-keys)
          (call-with-output-file "site.cfg"
            (lambda (port)
              (format port
                      "[openblas]
libraries = openblas
library_dirs = ~a/lib
include_dirs = ~a/include

[lapack]
lapack_libs = lapack
library_dirs = ~a/lib
include_dirs = ~a/include
"
                      (assoc-ref inputs "openblas")
                      (assoc-ref inputs "openblas")
                      (assoc-ref inputs "lapack")
                      (assoc-ref inputs "lapack"))))
          ;; Use "gcc" executable, not "cc".
          (substitute* "numpy/distutils/system_info.py"
            (("c = distutils\\.ccompiler\\.new_compiler\\(\\)")
             "c = distutils.ccompiler.new_compiler(); c.set_executables(compiler='gcc',compiler_so='gcc',linker_exe='gcc',linker_so='gcc -shared')"))
          #t)
        ;; Tests can only be run after the library has been installed and not
        ;; within the source directory.
        (alist-cons-after
         'install 'check
         (lambda _
           (with-directory-excursion "/tmp"
             (zero? (system* "python" "-c"
                             "import numpy; numpy.test(verbose=2)"))))
         (alist-delete
          'check
          %standard-phases)))))
    (home-page "http://www.numpy.org/")
    (synopsis "Fundamental package for scientific computing with Python")
    (description "NumPy is the fundamental package for scientific computing
with Python.  It contains among other things: a powerful N-dimensional array
object, sophisticated (broadcasting) functions, tools for integrating C/C++
and Fortran code, useful linear algebra, Fourier transform, and random number
capabilities.")
    (license bsd-3)))

(define python2-numpy-bootstrap
  (package-with-python2 python-numpy-bootstrap))

(define-public python2-fastlmm
  (package
    (name "python2-fastlmm")
    (version "0.2.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/f/fastlmm"
             "/fastlmm-" version ".zip"))
       (sha256
        (base32
         "023sydkrc3yxad2bycar02jfswwlh4199kafzhf2bssyx2c3xa0l"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2)) ; only Python 2.7 is supported
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)
       ("python2-scipy" ,python2-scipy)
       ("python2-matplotlib" ,python2-matplotlib)
       ("python2-pandas" ,python2-pandas)
       ("python2-scikit-learn" ,python2-scikit-learn)
       ("python2-cython" ,python2-cython)
       ("python2-pysnptools" ,python2-pysnptools)))
    (native-inputs
     `(("unzip" ,unzip)
       ("python2-mock" ,python2-mock)
       ("python2-setuptools" ,python2-setuptools)))
    (home-page "http://research.microsoft.com/en-us/um/redmond/projects/mscompbio/fastlmm/")
    (synopsis "Perform genome-wide association studies on large data sets")
    (description
     "FaST-LMM, which stands for Factored Spectrally Transformed Linear Mixed
Models, is a program for performing both single-SNP and SNP-set genome-wide
association studies (GWAS) on extremely large data sets.")
    (license asl2.0)))

(define-public python-numpy
  (package (inherit python-numpy-bootstrap)
    (name "python-numpy")
    (outputs '("out" "doc"))
    (inputs
     `(("which" ,which)
       ("python-setuptools" ,python-setuptools)
       ("python-matplotlib" ,python-matplotlib)
       ("python-sphinx" ,python-sphinx)
       ("python-pyparsing" ,python-pyparsing)
       ("python-numpydoc" ,python-numpydoc)
       ,@(package-inputs python-numpy-bootstrap)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("texlive" ,texlive)
       ("texinfo" ,texinfo)
       ("perl" ,perl)
       ,@(package-native-inputs python-numpy-bootstrap)))
    (arguments
     `(,@(substitute-keyword-arguments
             (package-arguments python-numpy-bootstrap)
           ((#:phases phases)
            `(alist-cons-after
              'install 'install-doc
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                       (doc (string-append
                             data "/doc/" ,name "-"
                             ,(package-version python-numpy-bootstrap)))
                       (info (string-append data "/info"))
                       (html (string-append doc "/html"))
                       (pyver ,(string-append "PYVER=")))
                  (with-directory-excursion "doc"
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
                                (find-files "." ".*"))))))
              ,phases)))))))

(define-public python2-numpy
  (let ((numpy (package-with-python2 python-numpy)))
    (package (inherit numpy)
      ;; Make sure we use exactly PYTHON2-MATPLOTLIB, which is customized for
      ;; Python 2.
      (inputs `(("python2-matplotlib" ,python2-matplotlib)
                ,@(alist-delete "python-matplotlib"
                                (package-inputs numpy)))))))

(define-public python-pyparsing
  (package
    (name "python-pyparsing")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pyparsing"
                           "/pyparsing-" version ".tar.gz"))
       (sha256
        (base32
         "0kw4py7gn45j93q8r7bzajfrjdc3xlsn2yzln41lf9zmrghjkrq6"))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f ; no test target
       #:modules ((guix build python-build-system)
                  (guix build utils))
       #:phases
       (alist-cons-after
        'install 'install-doc
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
             (list doc html-doc examples))))
        %standard-phases)))
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
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-docutils" ,python-docutils)
       ("python-sphinx" ,python-sphinx)
       ("python-nose" ,python-nose)))
    (home-page "https://pypi.python.org/pypi/numpydoc")
    (synopsis
     "Numpy's Sphinx extensions")
    (description
     "Sphinx extension to support docstrings in Numpy format.")
    (license bsd-2)))

(define-public python2-numpydoc
  (package-with-python2 python-numpydoc))

(define-public python-numexpr
  (package
    (name "python-numexpr")
    (version "2.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/"
                           "n/numexpr/numexpr-" version ".tar.gz"))
       (sha256
        (base32
         "0nsnff5312fm38w6dm34bw7ghfqqy8vl9gig0al963h4mz8zm8nz"))))
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
  (let ((numexpr (package-with-python2 python-numexpr)))
    (package (inherit numexpr)
      ;; Make sure to use special packages for Python 2 instead
      ;; of those automatically rewritten by package-with-python2.
      (propagated-inputs
       `(("python2-numpy" ,python2-numpy)
         ,@(alist-delete "python-numpy"
                         (package-propagated-inputs numexpr)))))))

(define-public python-matplotlib
  (package
    (name "python-matplotlib")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/matplotlib"
                           "/matplotlib-" version ".tar.gz"))
       (sha256
        (base32
         "1dn05cvd0g984lzhh72wa0z93psgwshbbg93fkab6slx5m3l95av"))
       (patches (list (search-patch "matplotlib-setupext-tk.patch")))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (propagated-inputs ; the following packages are all needed at run time
     `(("python-pyparsing" ,python-pyparsing)
       ("python-pygobject" ,python-pygobject)
       ("gobject-introspection" ,gobject-introspection)
       ("python-tkinter" ,python "tk")
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
     `(("python-setuptools" ,python-setuptools)
       ("python-dateutil" ,python-dateutil-2)
       ("python-six" ,python-six)
       ("python-pytz" ,python-pytz)
       ("python-numpy" ,python-numpy-bootstrap)
       ("python-sphinx" ,python-sphinx)
       ("python-numpydoc" ,python-numpydoc)
       ("python-nose" ,python-nose)
       ("python-mock" ,python-mock)
       ("libpng" ,libpng)
       ("imagemagick" ,imagemagick)
       ("freetype" ,freetype)
       ("cairo" ,cairo)
       ("glib" ,glib)
       ("python-pillow" ,python-pillow)
       ;; FIXME: Add backends when available.
       ;("python-wxpython" ,python-wxpython)
       ;("python-pyqt" ,python-pyqt)
       ("tcl" ,tcl)
       ("tk" ,tk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("texlive" ,texlive)
       ("texinfo" ,texinfo)))
    (arguments
     `(#:phases
       (alist-cons-before
        'build 'configure-environment
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
                        (assoc-ref inputs "tk"))))))
        (alist-cons-after
         'install 'install-doc
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                  (doc (string-append data "/doc/" ,name "-" ,version))
                  (info (string-append data "/info"))
                  (html (string-append doc "/html")))
             (with-directory-excursion "doc"
               ;; Produce pdf in 'A4' format.
               (substitute* (find-files "." "conf\\.py")
                 (("latex_paper_size = 'letter'")
                  "latex_paper_size = 'a4'"))
               (mkdir-p html)
               (mkdir-p info)
               ;; The doc recommends to run the 'html' target twice.
               (system* "python" "make.py" "html")
               (system* "python" "make.py" "html")
               (copy-recursively "build/html" html)
               (system* "python" "make.py" "latex")
               (system* "python" "make.py" "texinfo")
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
                          (string-append doc "/Matplotlib.pdf")))))
        %standard-phases))))
    (home-page "http://matplotlib.org")
    (synopsis "2D plotting library for Python")
    (description
     "Matplotlib is a Python 2D plotting library which produces publication
quality figures in a variety of hardcopy formats and interactive environments
across platforms.  Matplotlib can be used in Python scripts, the python and
ipython shell, web application servers, and six graphical user interface
toolkits.")
    (license psfl)))

(define-public python2-matplotlib
  (let ((matplotlib (package-with-python2 python-matplotlib)))
    (package (inherit matplotlib)
      ;; Make sure to use special packages for Python 2 instead
      ;; of those automatically rewritten by package-with-python2.
      (propagated-inputs
       `(("python2-pycairo" ,python2-pycairo)
         ("python2-pygobject-2" ,python2-pygobject-2)
         ("python2-tkinter" ,python-2 "tk")
         ,@(fold alist-delete (package-propagated-inputs matplotlib)
                 '("python-pycairo" "python-pygobject" "python-tkinter")))))))

(define-public python2-pysnptools
  (package
    (name "python2-pysnptools")
    (version "0.2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pysnptools"
             "/pysnptools-" version ".zip"))
       (sha256
        (base32
         "1rzf5qvwfvd2pp84b14pb2gdvxdk5avnj7rb41ac8gndpkr9g6ib"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2)) ; only Python 2.7 is supported
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)
       ("python2-scipy" ,python2-scipy)
       ("python2-pandas" ,python2-pandas)
       ("python2-cython" ,python2-cython)))
    (native-inputs
     `(("unzip" ,unzip)
       ("python2-setuptools" ,python2-setuptools)))
    (home-page "http://research.microsoft.com/en-us/um/redmond/projects/mscompbio/")
    (synopsis "Library for reading and manipulating genetic data")
    (description
     "PySnpTools is a library for reading and manipulating genetic data.  It
can, for example, efficiently read whole PLINK *.bed/bim/fam files or parts of
those files.  It can also efficiently manipulate ranges of integers using set
operators such as union, intersection, and difference.")
    (license asl2.0)))

(define-public python-rpy2
  (package
    (name "python-rpy2")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/r/rpy2"
                           "/rpy2-" version ".tar.gz"))
       (sha256
        (base32
         "1dp4l8hpv0jpf4crz4wis6in3lvwk86cr5zvpw410y4a07rrbqjk"))))
    (build-system python-build-system)
    (inputs
     `(("python-six" ,python-six)
       ("readline" ,readline)
       ("icu4c" ,icu4c)
       ("pcre" ,pcre)
       ("r" ,r)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("zlib" ,zlib)))
    (home-page "http://rpy.sourceforge.net/")
    (synopsis "Python interface to the R language")
    (description "rpy2 is a redesign and rewrite of rpy.  It is providing a
low-level interface to R from Python, a proposed high-level interface,
including wrappers to graphical libraries, as well as R-like structures and
functions.")
    (license gpl3+)))

(define-public python2-rpy2
  (let ((rpy2 (package-with-python2 python-rpy2)))
    (package (inherit rpy2)
      (native-inputs
       `(("python2-singledispatch" ,python2-singledispatch)
         ,@(package-native-inputs rpy2))))))

(define-public python-scipy
  (package
    (name "python-scipy")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/scipy"
                           "/scipy-" version ".tar.xz"))
       (sha256
        (base32
         "0wa0a4skpda3gx7lb12yn19nhbairlyxrvda2lz2bcawk3x5qzz2"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-matplotlib" ,python-matplotlib)
       ("python-pyparsing" ,python-pyparsing)))
    (inputs
     `(("lapack" ,lapack)
       ("openblas" ,openblas)))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-sphinx" ,python-sphinx)
       ("python-numpydoc" ,python-numpydoc)
       ("gfortran" ,gfortran)
       ("texlive" ,texlive)
       ("perl" ,perl)))
    (outputs '("out" "doc"))
    (arguments
     `(#:phases
       (alist-cons-before
        'build 'configure-openblas
        (lambda* (#:key inputs #:allow-other-keys)
          (call-with-output-file "site.cfg"
            (lambda (port)
              (format port
                      "[blas]
libraries = openblas
library_dirs = ~a/lib
include_dirs = ~a/include
[atlas]
library_dirs = ~a/lib
atlas_libs = openblas
"
                      (assoc-ref inputs "openblas")
                      (assoc-ref inputs "openblas")
                      (assoc-ref inputs "openblas"))))
          #t)
        (alist-cons-after
         'install 'install-doc
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                  (doc (string-append data "/doc/" ,name "-" ,version))
                  (html (string-append doc "/html"))
                  (pyver ,(string-append "PYVER=")))
             (with-directory-excursion "doc"
               ;; Fix generation of images for mathematical expressions.
               (substitute* (find-files "source" "conf\\.py")
                 (("pngmath_use_preview = True")
                  "pngmath_use_preview = False"))
               (mkdir-p html)
               (system* "make" "html" pyver)
               (system* "make" "latex" "PAPER=a4" pyver)
               (system* "make" "-C" "build/latex" "all-pdf" "PAPER=a4" pyver)
               (copy-file "build/latex/scipy-ref.pdf"
                          (string-append doc "/scipy-ref.pdf"))
               (with-directory-excursion "build/html"
                 (for-each (lambda (file)
                             (let* ((dir (dirname file))
                                    (tgt-dir (string-append html "/" dir)))
                               (install-file file html)))
                           (find-files "." ".*"))))))
         ;; Tests can only be run after the library has been installed and not
         ;; within the source directory.
         (alist-cons-after
          'install 'check
          (lambda _
            (with-directory-excursion "/tmp"
              (zero? (system* "python" "-c" "import scipy; scipy.test()"))))
          (alist-delete
           'check
           (alist-cons-after
            'unpack 'fix-tests
            (lambda _
              (substitute* "scipy/integrate/tests/test_quadpack.py"
                (("libm.so") "libm.so.6"))
              #t)
            %standard-phases)))))))
    (home-page "http://www.scipy.org/")
    (synopsis "The Scipy library provides efficient numerical routines")
    (description "The SciPy library is one of the core packages that make up
the SciPy stack.  It provides many user-friendly and efficient numerical
routines such as routines for numerical integration and optimization.")
    (license bsd-3)))

(define-public python2-scipy
  (let ((scipy (package-with-python2 python-scipy)))
    (package (inherit scipy)
      ;; Use packages customized for python-2.
      (propagated-inputs
       `(("python2-matplotlib" ,python2-matplotlib)
         ("python2-numpy" ,python2-numpy)
         ,@(alist-delete "python-matplotlib"
                         (alist-delete "python-numpy"
                                       (package-propagated-inputs scipy))))))))

(define-public python-sqlalchemy
  (package
    (name "python-sqlalchemy")
    (version "0.9.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/S/"
                          "SQLAlchemy/SQLAlchemy-" version ".tar.gz"))
      (sha256
       (base32
        "059ayifj5l08v6vv56anhyibyllscn10dlzr2fcw68gz1hfjdzsz"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-cython" ,python-cython) ;for c extensions
       ("python-pytest" ,python-pytest)
       ("python-mock"   ,python-mock))) ;for tests
    (arguments
     `(#:phases (alist-replace
                 'check
                 (lambda _ (zero? (system* "py.test")))
                 %standard-phases)))
    (home-page "http://www.sqlalchemy.org")
    (synopsis "Database abstraction library")
    (description
     "SQLAlchemy is the Python SQL toolkit and Object Relational Mapper that
gives application developers the full power and flexibility of SQL.  It
provides a full suite of well known enterprise-level persistence patterns,
designed for efficient and high-performing database access, adapted into a
simple and Pythonic domain language.")
    (license x11)))

(define-public python2-sqlalchemy
  (package-with-python2 python-sqlalchemy))

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
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://launchpad.net/python-distutils-extra/")
    (synopsis "Enhancements to Python's distutils")
    (description
     "The python-distutils-extra module enables you to easily integrate
gettext support, themed icons, and scrollkeeper-based documentation into
Python's distutils.")
    (license gpl2)))

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
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)))
    (arguments
     ;; incompatible with Python 3 (exception syntax)
     `(#:python ,python-2
       #:tests? #f
       ;; With standard flags, the install phase attempts to create a zip'd
       ;; egg file, and fails with an error: 'ZIP does not support timestamps
       ;; before 1980'
       #:configure-flags '("--single-version-externally-managed"
                           "--record=elib.txt")))
    (home-page "https://github.com/dieterv/elib.intl")
    (synopsis "Enhanced internationalization for Python")
    (description
     "The elib.intl module provides enhanced internationalization (I18N)
services for your Python modules and applications.")
    (license lgpl3+)))

(define-public python-pillow
  (package
    (name "python-pillow")
    (version "2.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/P/"
                           "Pillow/Pillow-" version ".tar.gz"))
       (sha256
        (base32
         "15n92axxph2s3kvg68bki9gv3nzwgq7130kp7wbblpi1l0cc2q47"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-nose"       ,python-nose)))
    (inputs
     `(("freetype" ,freetype)
       ("lcms"     ,lcms)
       ("zlib"     ,zlib)
       ("libjpeg"  ,libjpeg)
       ("openjpeg" ,openjpeg)
       ("libtiff"  ,libtiff)
       ("libwebp"  ,libwebp)))
    (propagated-inputs
     `(;; Used at runtime for pkg_resources
       ("python-setuptools" ,python-setuptools)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before
                   'install 'disable-egg-compression
                   (lambda _
                     ;; Leave the .egg uncompressed since compressing it would
                     ;; prevent the GC from identifying run-time dependencies.
                     ;; See <http://bugs.gnu.org/20765>.
                     (let ((port (open-file "setup.cfg" "a")))
                       (display "\n[easy_install]\nzip_ok = 0\n"
                                port)
                       (close-port port)
                       #t)))
                  (add-after
                   'install 'check-installed
                   (lambda _
                     (begin
                       (setenv "HOME" (getcwd))
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
    (license (x11-style
              "http://www.pythonware.com/products/pil/license.htm"
              "The PIL Software License"))))

(define-public python2-pillow
  (package-with-python2 python-pillow))

(define-public python-pycparser
  (package
    (name "python-pycparser")
    (version "2.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/p/"
                          "pycparser/pycparser-" version ".tar.gz"))
      (sha256
       (base32
        "0v5qfq03yvd1pi0dwlgfai0p3dh9bq94pydn19c4pdn0c6v9hzcm"))))
    (outputs '("out" "doc"))
    (build-system python-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-setuptools" ,python-setuptools)))
    (arguments
     `(#:phases
       (alist-replace
        'check
        (lambda _
          (with-directory-excursion "tests"
            (zero? (system* "python" "all_tests.py"))))
        (alist-cons-after
         'install 'install-doc
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                  (doc (string-append data "/doc/" ,name "-" ,version))
                  (examples (string-append doc "/examples")))
             (mkdir-p examples)
             (for-each (lambda (file)
                         (copy-file (string-append "." file)
                                    (string-append doc file)))
                       '("/README.rst" "/CHANGES" "/LICENSE"))
             (copy-recursively "examples" examples)))
         %standard-phases))))
    (home-page "https://github.com/eliben/pycparser")
    (synopsis "C parser in Python")
    (description
     "Pycparser is a complete parser of the C language, written in pure Python
using the PLY parsing library.  It parses C code into an AST and can serve as
a front-end for C compilers or analysis tools.")
    (license bsd-3)))

(define-public python2-pycparser
  (package-with-python2 python-pycparser))

(define-public python-cffi
  (package
    (name "python-cffi")
    (version "1.2.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/c/"
                          "cffi/cffi-" version ".tar.gz"))
      (sha256
       (base32 "0g8yfzinry1vsj6d1jlnd19338bh92lhhk207ksy4lm1n3g73dga"))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (inputs
     `(("libffi" ,libffi)))
    (propagated-inputs ; required at run-time
     `(("python-pycparser" ,python-pycparser)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)
       ("python-pytest" ,python-pytest)
       ("python-setuptools" ,python-setuptools)))
    (arguments
     `(#:phases
       (alist-cons-after
        'install 'install-doc
        (lambda* (#:key outputs #:allow-other-keys)
          (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                 (doc (string-append data "/doc/" ,name "-" ,version))
                 (html (string-append doc "/html")))
            (with-directory-excursion "doc"
              (system* "make" "html")
              (mkdir-p html)
              (copy-recursively "build/html" html))
            (copy-file "LICENSE" (string-append doc "/LICENSE"))))
        %standard-phases)))
    (home-page "http://cffi.readthedocs.org")
    (synopsis "Foreign function interface for Python")
    (description
     "Foreign Function Interface for Python calling C code.")
    (license license:expat)))

(define-public python2-cffi
  (package-with-python2 python-cffi))

(define-public python-xcffib
  (package
    (name "python-xcffib")
    (version "0.1.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/x/"
                          "xcffib/xcffib-" version ".tar.gz"))
      (sha256
       (base32
        "0655hzxv57h1a9ja9kwp0ichbkhf3djw32k33d66xp0q37dq2y81"))))
    (build-system python-build-system)
    (inputs
     `(("libxcb" ,libxcb)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (propagated-inputs
     `(("python-cffi" ,python-cffi))) ; used at run time
    (arguments
     `(#:phases
       (alist-cons-after
        'install 'install-doc
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((doc (string-append (assoc-ref outputs "out") "/share"
                                    "/doc/" ,name "-" ,version)))
            (mkdir-p doc)
            (copy-file "README.md"
                       (string-append doc "/README.md"))))
        %standard-phases)))
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
    (version "0.6")
    (source
     (origin
      (method url-fetch)
      ;; The archive on pypi is missing the 'utils' directory!
      (uri (string-append "https://github.com/SimonSapin/cairocffi/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "03w5p62sp3nqiccx864sbq0jvh7946277jqx3rcc3dch5xwfvv51"))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("cairo" ,cairo)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)
       ("python-docutils" ,python-docutils)
       ("python-setuptools" ,python-setuptools)))
    (propagated-inputs
     `(("python-xcffib" ,python-xcffib))) ; used at run time
    (arguments
     `(#:phases
       (alist-cons-after
        'install 'install-doc
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
            (copy-recursively "docs/_build/html" html)))
        %standard-phases)))
    (home-page "https://github.com/SimonSapin/cairocffi")
    (synopsis "Python bindings and object-oriented API for Cairo")
    (description
     "Cairocffi is a CFFI-based drop-in replacement for Pycairo, a set of
Python bindings and object-oriented API for cairo.  Cairo is a 2D vector
graphics library with support for multiple backends including image buffers,
PNG, PostScript, PDF, and SVG file output.")
    (license bsd-3)))

(define-public python2-cairocffi
  (package-with-python2 python-cairocffi))

(define-public python-decorator
  (package
    (name "python-decorator")
    (version "3.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/d/decorator/decorator-"
             version ".tar.gz"))
       (sha256
        (base32 "0i2bnlkh0p9gs76hb28mafandcrig2fmv56w9ai6mshxwqn0083k"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; no test target
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://pypi.python.org/pypi/decorator/")
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
    (version "0.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/d/drmaa/drmaa-"
             version ".tar.gz"))
       (sha256
        (base32 "0bzl9f9g34dlhwf09i3fdv7dqqzf2iq0w7d6c2bafx1nlap8qfbh"))))
    (build-system python-build-system)
    ;; The test suite requires libdrmaa which is provided by the cluster
    ;; environment.  At runtime the environment variable DRMAA_LIBRARY_PATH
    ;; should be set to the path of the libdrmaa library.
    (arguments '(#:tests? #f))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-setuptools" ,python-setuptools)))
    (home-page "https://pypi.python.org/pypi/drmaa")
    (synopsis "Python bindings for the DRMAA library")
    (description
      "A Python package for Distributed Resource Management (DRM) job
submission and control.  This package is an implementation of the DRMAA 1.0
Python language binding specification.")
    (license bsd-3)))

(define-public python2-drmaa
  (package-with-python2 python-drmaa))

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
    (inputs
     `(("python-psutil" ,python-psutil)
       ("python-drmaa" ,python-drmaa)
       ("python-pyzmq" ,python-pyzmq)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/pygridtools/gridmap")
    (synopsis "Create jobs on a cluster directly from Python")
    (description
      "Gridmap is a Python package to allow you to easily create jobs on the
cluster directly from Python.  You can directly map Python functions onto the
cluster without needing to write any wrapper code yourself.")
    (license gpl3+)))

(define-public python2-gridmap
  (package-with-python2 python-gridmap))

(define-public python-pexpect
  (package
    (name "python-pexpect")
    (version "3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/p/"
                           "pexpect/pexpect-" version ".tar.gz"))
       (sha256
        (base32 "1fp5gm976z7ghm8jw57463rj19cv06c8zw842prgyg788f6n3snz"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "http://pexpect.readthedocs.org/")
    (synopsis "Controlling interactive console applications")
    (description
     "Pexpect is a pure Python module for spawning child applications;
controlling them; and responding to expected patterns in their output.
Pexpect works like Don Libes’ Expect.  Pexpect allows your script to spawn a
child application and control it as if a human were typing commands.")
    (license isc)))

(define-public python2-pexpect
  (package-with-python2 python-pexpect))

(define-public python-setuptools-scm
  (package
    (name "python-setuptools-scm")
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "setuptools_scm" version))
              (sha256
               (base32
                "0y24bl893zk6nrklbvdrlmpkalf214zjn6k1xrglljd29rrn4wxi"))))
    (build-system python-build-system)
    (native-inputs `(("python-setuptools" ,python-setuptools)))
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
    (build-system python-build-system)
    (propagated-inputs
     `(("python-appdirs" ,python-appdirs)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (home-page "http://github.com/jaraco/path.py")
    (synopsis "Python module wrapper for built-in os.path")
    (description
     "@code{path.py} implements path objects as first-class entities, allowing
common operations on files to be invoked on those path objects directly.")
    (license license:expat)))

(define-public python2-pathpy
  (package-with-python2 python-pathpy))

(define-public python-pickleshare
  (package
    (name "python-pickleshare")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/p/"
                           "pickleshare/pickleshare-" version ".tar.gz"))
       (sha256
        (base32 "11ljr90j3p6qswdrbl7p4cjb2i93f6vn0vx9anzpshsx0d2mggn0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pathpy" ,python-pathpy)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/vivainio/pickleshare")
    (synopsis "Tiny key value database with concurrency support")
    (description
     "PickleShare is a small ‘shelve’-like datastore with concurrency support.
Like shelve, a PickleShareDB object acts like a normal dictionary.  Unlike
shelve, many processes can access the database simultaneously.  Changing a
value in database is immediately visible to other processes accessing the same
database.  Concurrency is possible because the values are stored in separate
files.  Hence the “database” is a directory where all files are governed by
PickleShare.")
    (license license:expat)))

(define-public python2-pickleshare
  (package-with-python2 python-pickleshare))

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
     `(("python-setuptools" ,python-setuptools)
       ("unzip" ,unzip)))
    (home-page "http://cheeseshop.python.org/pypi/simplegeneric")
    (synopsis "Python module for simple generic functions")
    (description
     "The simplegeneric module lets you define simple single-dispatch generic
functions, akin to Python’s built-in generic functions like @code{len()},
@code{iter()} and so on.  However, instead of using specially-named methods,
these generic functions use simple lookup tables, akin to those used by
e.g. @code{pickle.dump()} and other generic functions found in the Python
standard library.")
    (license zpl2.1)))

(define-public python2-simplegeneric
  (package-with-python2 python-simplegeneric))

(define-public python-ipython-genutils
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
     "This package provides retired utilities from IPython.")
    (license bsd-3)))

(define-public python2-ipython-genutils
  (package-with-python2 python-ipython-genutils))

(define-public python-traitlets
  (package
    (name "python-traitlets")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/t/"
                           "traitlets/traitlets-" version ".tar.gz"))
       (sha256
        (base32
         "0fr3w2xwb46c591dp7zw02bgf4d21mjy9g6rhwc9bwd4ji50n50b"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (propagated-inputs
     `(("python-ipython-genutils" ,python-ipython-genutils)
       ("python-decorator" ,python-decorator)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "http://ipython.org")
    (synopsis "Configuration system for Python applications")
    (description
     "Traitlets is a framework that lets Python classes have attributes with
type checking, dynamically calculated default values, and ‘on change’
callbacks.  The package also includes a mechanism to use traitlets for
configuration, loading values from files or from command line arguments.  This
is a distinct layer on top of traitlets, so you can use traitlets in your code
without using the configuration machinery.")
    (license bsd-3)))

(define-public python2-traitlets
  (package-with-python2 python-traitlets))

(define-public python-ipython
  (package
    (name "python-ipython")
    (version "3.2.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/i/"
                          "ipython/ipython-" version ".tar.gz"))
      (sha256
       (base32 "0xwin0sa9n0cabx4cq1ibf5ldsiw5dyimibla82kicz5gbpas4y9"))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (propagated-inputs
     `(("python-pyzmq" ,python-pyzmq)
       ("python-terminado" ,python-terminado)))
    (inputs
     `(("readline" ,readline)
       ("which" ,which)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-numpydoc" ,python-numpydoc)
       ("python-jinja2" ,python-jinja2)
       ("python-mistune" ,python-mistune)
       ("python-jsonschema" ,python-jsonschema)
       ("python-pygments" ,python-pygments)
       ("python-requests" ,python-requests) ;; for tests
       ("python-nose" ,python-nose)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)
       ("texlive" ,texlive)
       ("texinfo" ,texinfo)
       ("python-setuptools" ,python-setuptools)))
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
                   (examples (string-append doc "/examples")))
              (setenv "LANG" "en_US.utf8")
              (with-directory-excursion "docs"
                ;; FIXME: html and pdf fail to build
                ;; (system* "make" "html")
                ;; (system* "make" "pdf" "PAPER=a4")
                (system* "make" "info"))
              (copy-recursively "docs/man" man1)
              (copy-recursively "examples" examples)
              ;; (copy-recursively "docs/build/html" html)
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
          (lambda* (#:key outputs tests? #:allow-other-keys)
            (if tests?
                (with-directory-excursion "/tmp"
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
    (license bsd-3)))

(define-public python2-ipython
  (let ((ipython (package-with-python2 python-ipython)))
    (package
      (inherit ipython)
      ;; FIXME: some tests are failing
      (arguments
       `(#:tests? #f ,@(package-arguments ipython)))
      ;; Make sure we use custom python2-NAME packages.
      ;; FIXME: add pyreadline once available.
      (propagated-inputs
       `(("python2-terminado" ,python2-terminado)
         ,@(alist-delete "python-terminado"
                         (package-propagated-inputs ipython))))
      (inputs
       `(("python2-mock" ,python2-mock)
         ("python2-matplotlib" ,python2-matplotlib)
         ("python2-numpy" ,python2-numpy)
         ,@(fold alist-delete (package-inputs ipython)
                 '("python-matplotlib" "python-numpy")))))))

(define-public python-isodate
  (package
    (name "python-isodate")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://pypi.python.org/packages/source/i/isodate/isodate-"
              version
              ".tar.gz"))
        (sha256
          (base32
            "1yqjn0is0p64cmk9xhq4hc6q06jk86d60kg2jws58d78q0qysami"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (home-page
      "http://cheeseshop.python.org/pypi/isodate")
    (synopsis
      "Python date parser and formatter")
    (description
      "Python-isodate is a python module for parsing and formatting
ISO 8601 dates, time and duration.")
    (license bsd-3)))

(define-public python2-isodate
  (package-with-python2 python-isodate))

(define-public python-html5lib
  (package
    (name "python-html5lib")
    (version "1.0b3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://pypi.python.org/packages/source/h/html5lib/html5lib-"
              version
              ".tar.gz"))
        (sha256
          (base32
            "1l5i6xzckzx4hnh9qzv9q3kyhkgjx2hsi2k9srgci3qizjmvp6ln"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-six" ,python-six))) ; required to "import html5lib"
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (arguments
     `(#:test-target "check"))
    (home-page
      "https://github.com/html5lib/html5lib-python")
    (synopsis
      "Python HTML parser based on the WHATWG HTML specifcation")
    (description
      "Html5lib is an HTML parser based on the WHATWG HTML specifcation
and written in Python.")
    (license license:expat)))

(define-public python2-html5lib
  (package-with-python2 python-html5lib))

(define-public python-urwid
  (package
    (name "python-urwid")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/u/urwid/urwid-"
             version ".tar.gz"))
       (sha256
        (base32
         "18mb0yy94sjc434rd61m2sfnw27sa0nyrszpj5a9r9zh7fnlzw19"))))
    (build-system python-build-system)
    (native-inputs `(("python-setuptools" ,python-setuptools)))
    (home-page "http://urwid.org")
    (synopsis "Console user interface library for Python")
    (description
     "Urwid is a curses-based UI/widget library for Python.  It includes many
features useful for text console applications.")
    (license lgpl2.1+)))

(define-public python2-urwid
  (package-with-python2 python-urwid))

(define-public python-dbus
  (package
    (name "python-dbus")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://dbus.freedesktop.org/releases/dbus-python/dbus-python-"
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

(define-public python-apsw
  (package
    (name "python-apsw")
    (version "3.8.7.3-r1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://pypi.python.org/packages/source/a/apsw/apsw-"
              version
              ".tar.gz"))
        (sha256
          (base32
            "1rgxdypg7hym0qny15rx5khrghx9fkppfgsfa2s8lg917924mv7l"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools" ,python-setuptools)
        ("sqlite" ,sqlite)))
    (arguments
     `(#:phases
        ;; swap check and install phases
        (alist-cons-after
         'install 'check
         (assoc-ref %standard-phases 'check)
         (alist-delete
          'check
          %standard-phases))))
    (home-page "https://github.com/rogerbinns/apsw/")
    (synopsis "Another Python SQLite Wrapper")
    (description "APSW is a Python wrapper for the SQLite
embedded relational database engine.  In contrast to other wrappers such as
pysqlite it focuses on being a minimal layer over SQLite attempting just to
translate the complete SQLite API into Python.")
    (license license:zlib)))

(define-public python2-apsw
  (package-with-python2 python-apsw))

(define-public python-lxml
  (package
    (name "python-lxml")
    (version "3.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://pypi.python.org/packages/source/l/lxml/lxml-"
              version
              ".tar.gz"))
        (sha256
          (base32
            "0pd23qz8vms1mgm41p96h4vac5y91igs4wr9640gnvxgk019kmf7"))))
    (build-system python-build-system)
    (inputs
      `(("libxml2" ,libxml2)
        ("libxslt" ,libxslt)
        ("python-setuptools" ,python-setuptools)))
    (home-page "http://lxml.de/")
    (synopsis
      "Python XML processing library")
    (description
      "The lxml XML toolkit is a Pythonic binding for the C libraries
libxml2 and libxslt.")
    (license bsd-3))) ; and a few more, see LICENSES.txt

(define-public python2-lxml
  (package-with-python2 python-lxml))

(define-public python2-pil
  (package
    (name "python2-pil")
    (version "1.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "http://effbot.org/downloads/Imaging-"
              version ".tar.gz"))
        (sha256
          (base32
            "04aj80jhfbmxqzvmq40zfi4z3cw6vi01m3wkk6diz3lc971cfnw9"))
       (modules '((guix build utils)))
       (snippet
        ;; Adapt to newer freetype. As the package is unmaintained upstream,
        ;; there is no use in creating a patch and reporting it.
        '(substitute* "_imagingft.c"
           (("freetype/")
            "freetype2/")))))
    (build-system python-build-system)
    (inputs
      `(("freetype" ,freetype)
        ("libjpeg" ,libjpeg)
        ("libtiff" ,libtiff)
        ("python-setuptools" ,python-setuptools)
        ("zlib" ,zlib)))
    (arguments
     ;; Only the fork python-pillow works with Python 3.
     `(#:python ,python-2
       #:tests? #f ; no check target
       #:phases
         (alist-cons-before
          'build 'configure
          ;; According to README and setup.py, manual configuration is
          ;; the preferred way of "searching" for inputs.
          ;; lcms is not found, TCL_ROOT refers to the unavailable tkinter.
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((jpeg (assoc-ref inputs "libjpeg"))
                  (zlib (assoc-ref inputs "zlib"))
                  (tiff (assoc-ref inputs "libtiff"))
                  (freetype (assoc-ref inputs "freetype")))
              (substitute* "setup.py"
                (("JPEG_ROOT = None")
                 (string-append "JPEG_ROOT = libinclude(\"" jpeg "\")"))
                (("ZLIB_ROOT = None")
                 (string-append "ZLIB_ROOT = libinclude(\"" zlib "\")"))
                (("TIFF_ROOT = None")
                 (string-append "TIFF_ROOT = libinclude(\"" tiff "\")"))
                (("FREETYPE_ROOT = None")
                 (string-append "FREETYPE_ROOT = libinclude(\""
                                freetype "\")")))))
          %standard-phases)))
    (home-page "http://www.pythonware.com/products/pil/")
    (synopsis "Python Imaging Library")
    (description "The Python Imaging Library (PIL) adds image processing
capabilities to the Python interpreter.")
    (license (x11-style
               "file://README"
               "See 'README' in the distribution."))))

(define-public python2-cssutils
  (package
    (name "python2-cssutils")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://pypi.python.org/packages/source/c/cssutils/cssutils-"
              version
              ".zip"))
        (sha256
          (base32
            "1bwim1353r4hqiir73sn4sc43y7ymh09qx0kly7vj048blppc125"))))
    (build-system python-build-system)
    (native-inputs
      `(("python2-mock" ,python2-mock) ; for the tests
        ("unzip" ,unzip))) ; for unpacking the source
    (inputs
      `(("python2-setuptools" ,python2-setuptools)))
    (arguments
     `(#:python ,python-2 ; Otherwise tests fail with a syntax error.
       #:tests? #f ; The tests apparently download an external URL.
       ))
    (home-page "http://cthedot.de/cssutils/")
    (synopsis
      "CSS Cascading Style Sheets library for Python")
    (description
      "Cssutils is a Python package for parsing and building CSS
Cascading Style Sheets.  Currently it provides a DOM only and no rendering
options.")
    (license lgpl3+)))

(define-public python-cssselect
  (package
    (name "python-cssselect")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://pypi.python.org/packages/source/c/cssselect/cssselect-"
              version
              ".tar.gz"))
        (sha256
          (base32
            "10h623qnp6dp1191jri7lvgmnd4yfkl36k9smqklp1qlf3iafd85"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (arguments
     ;; tests fail with message
     ;; AttributeError: 'module' object has no attribute 'tests'
     `(#:tests? #f))
    (home-page
      "https://pythonhosted.org/cssselect/")
    (synopsis
      "CSS3 selector parser and translator to XPath 1.0")
    (description
      "Cssselect ia a Python module that parses CSS3 Selectors and translates
them to XPath 1.0 expressions.  Such expressions can be used in lxml or
another XPath engine to find the matching elements in an XML or HTML document.")
    (license bsd-3)))

(define-public python2-cssselect
  (package-with-python2 python-cssselect))

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
    (inputs
      `(("python-setuptools" ,python-setuptools)))
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
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/n/networkx/networkx-"
             version ".tar.gz"))
       (sha256
        (base32 "0n8wy0yq1kmdq4wh68mlhwhkndvwzx48lg41a1z0sxxms0wfp033"))))
    (build-system python-build-system)
    ;; python-decorator is needed at runtime
    (propagated-inputs
     `(("python-decorator" ,python-decorator)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-nose" ,python-nose)))
    (home-page "http://networkx.github.io/")
    (synopsis "Python module for creating and manipulating graphs and networks")
    (description
      "NetworkX is a Python package for the creation, manipulation, and study
of the structure, dynamics, and functions of complex networks.")
    (license bsd-3)))

(define-public python2-networkx
  (package-with-python2 python-networkx))

(define-public snakemake
  (package
    (name "snakemake")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/s/snakemake/snakemake-"
             version ".tar.gz"))
       (sha256
        (base32 "0fi4b63sj60hvi7rfydvmz2icl4wj74djw5sn2gl8hxd02qw4b91"))))
    (build-system python-build-system)
    (inputs `(("python-setuptools" ,python-setuptools)))
    (home-page "https://bitbucket.org/johanneskoester/snakemake")
    (synopsis "Python-based execution environment for make-like workflows")
    (description
      "Snakemake aims to reduce the complexity of creating workflows by
providing a clean and modern domain specific specification language (DSL) in
Python style, together with a fast and comfortable execution environment.")
    (license license:expat)))

(define-public python-seaborn
  (package
    (name "python-seaborn")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/s/seaborn/seaborn-"
             version ".tar.gz"))
       (sha256
        (base32 "1236abw18ijjglmv60q85ckqrvgf5qyy4zlq7nz5aqfg6q87z3wc"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pandas" ,python-pandas)
       ("python-matplotlib" ,python-matplotlib)
       ("python-scipy" ,python-scipy)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://stanford.edu/~mwaskom/software/seaborn/")
    (synopsis "Statistical data visualization")
    (description
     "Seaborn is a library for making attractive and informative statistical
graphics in Python.  It is built on top of matplotlib and tightly integrated
with the PyData stack, including support for numpy and pandas data structures
and statistical routines from scipy and statsmodels.")
    (license bsd-3)))

(define-public python2-seaborn
  (let ((seaborn (package-with-python2 python-seaborn)))
    (package (inherit seaborn)
      (propagated-inputs
       `(("python2-pytz" ,python2-pytz)
         ("python2-pandas" ,python2-pandas)
         ("python2-matplotlib" ,python2-matplotlib)
         ("python2-scipy" ,python2-scipy))))))

(define-public python-sympy
  (package
    (name "python-sympy")
    (version "0.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/sympy/sympy/releases/download/sympy-"
             version "/sympy-" version ".tar.gz"))
       (sha256
        (base32 "19yp0gy4i7p4g6l3b8vaqkj9qj7yqb5kqy0qgbdagpzgkdz958yz"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://www.sympy.org/")
    (synopsis "Python library for symbolic mathematics")
    (description
     "SymPy is a Python library for symbolic mathematics.  It aims to become a
full-featured computer algebra system (CAS) while keeping the code as simple
as possible in order to be comprehensible and easily extensible.")
    (license bsd-3)))

(define-public python2-sympy
  (package-with-python2 python-sympy))

(define-public python-testlib
  (package
    (name "python-testlib")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/t/testlib/testlib-"
             version ".zip"))
       (sha256
        (base32 "1mz26cxn4x8bbgv0rn0mvj2z05y31rkc8009nvdlb3lam5b4mj3y"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (native-inputs
     `(("unzip" ,unzip)))
    (arguments
     `(#:phases
       (alist-replace
        'unpack
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let ((unzip (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip"))
                (source (assoc-ref inputs "source")))
            (and (zero? (system* unzip source))
                 (chdir (string-append "testlib-" ,version)))))
        %standard-phases)))
    (synopsis "Python micro test suite harness")
    (description "A micro unittest suite harness for Python.")
    (home-page "https://github.com/trentm/testlib")
    (license license:expat)))

(define-public python2-testlib
  (package-with-python2 python-testlib))

(define-public python2-xlib
  (package
    (name "python2-xlib")
    (version "0.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/python-xlib/"
                                  "python-xlib-" version ".tar.gz"))
              (sha256
               (base32
                "1sv0447j0rx8cgs3jhjl695p5pv13ihglcjlrrz1kq05lsvb0wa7"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2                         ;Python 2 only
       #:tests? #f))                              ;no tests
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://python-xlib.sourceforge.net/")
    (synopsis "Python X11 client library")
    (description
     "The Python X Library is intended to be a fully functional X client
library for Python programs.  It is useful to implement low-level X clients.
It is written entirely in Python.")
    (license gpl2+)))

(define-public python-singledispatch
  (package
    (name "python-singledispatch")
    (version "3.4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/s/singledispatch/"
             "singledispatch-" version ".tar.gz"))
       (sha256
        (base32
         "171b7ip0hsq5qm83np40h3phlr36ym18w0lay0a8v08kvy3sy1jv"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page
     "http://docs.python.org/3/library/functools.html#functools.singledispatch")
    (synopsis "Backport of singledispatch feature from Python 3.4")
    (description
     "This library brings functools.singledispatch from Python 3.4 to Python
2.6-3.3.")
    (license license:expat)))

(define-public python2-singledispatch
  (package-with-python2 python-singledispatch))

(define-public python-tornado
  (package
    (name "python-tornado")
    (version "4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/t/tornado/"
             "tornado-" version ".tar.gz"))
       (sha256
        (base32 "0a12f00h277zbifibnj46wf14801f573irvf6hwkgja5vspd7awr"))))
    (build-system python-build-system)
    (inputs
     `(("python-certifi" ,python-certifi)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://www.tornadoweb.org/")
    (synopsis "Python web framework and asynchronous networking library")
    (description
     "Tornado is a Python web framework and asynchronous networking library,
originally developed at FriendFeed.  By using non-blocking network I/O,
Tornado can scale to tens of thousands of open connections, making it ideal
for long polling, WebSockets, and other applications that require a long-lived
connection to each user.")
    (license asl2.0)))

(define-public python2-tornado
  (let ((tornado (package-with-python2 python-tornado)))
    (package (inherit tornado)
      (inputs
       `(("python2-backport-ssl-match-hostname"
          ,python2-backport-ssl-match-hostname)
         ,@(package-inputs tornado))))))

(define-public python-waf
  (package
    (name "python-waf")
    (version "1.8.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://waf.io/"
                                  "waf-" version ".tar.bz2"))
              (sha256
               (base32
                "0b5q307fgn6a5d8yjia2d1l4bk1q3ilvc0w8k4isfrrx2gbcw8wn"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'build
                  (lambda _
                    (zero? (begin
                             (system* "python" "waf-light" "configure")
                             (system* "python" "waf-light" "build")))))
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
    (license bsd-3)))

(define-public python2-waf
  (package-with-python2 python-waf))

(define-public python-pyzmq
  (package
    (name "python-pyzmq")
    (version "14.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pyzmq/pyzmq-"
             version ".tar.gz"))
       (sha256
        (base32 "1frmbjykvhmdg64g7sn20c9fpamrsfxwci1nhhg8q7jgz5pq0ikp"))))
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
       ("python-nose" ,python-nose)
       ("python-setuptools" ,python-setuptools)))
    (home-page "http://github.com/zeromq/pyzmq")
    (synopsis "Python bindings for 0MQ")
    (description
     "PyZMQ is the official Python binding for the ZeroMQ messaging library.")
    (license bsd-4)))

(define-public python2-pyzmq
  (package-with-python2 python-pyzmq))

(define-public python-pep8
  (package
    (name "python-pep8")
    (version "1.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/p/pep8/pep8-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1zybkcdw1sx84dvkfss96nhykqg9bc0cdpwpl4k9wlxm61bf7dxq"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools" ,python-setuptools)))
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
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/p/pyflakes/pyflakes-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0pvawddspdq0y22dbraq5gld9qr6rwa7zhmpfhl2b7v9rqiiqs82"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools" ,python-setuptools)))
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
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/m/mccabe/mccabe-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "05ix3vdv5hjk4irl97n2n3c4g1vqvz7dbmkzs13f3bx97bxsczjz"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools" ,python-setuptools)))
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
            "12b9bbdbwnspxgak14xg58c130x2n0blxzlms5jn2dszn8qj3d0m"))))))

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
            "0sbpq6pqm1i9wqi41mlfrsc5rk92jv4mskvlyxmnhlbdnc80ma1z"))))))

(define-public python2-pyflakes-0.8.1
  (package-with-python2 python-pyflakes-0.8.1))

(define-public python-flake8
  (package
    (name "python-flake8")
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/f/flake8/flake8-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0dvmrpv7x98xkzffjz1z7lqr90sp5zdz16bdwckfd1cckpjvnzif"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools" ,python-setuptools)
        ("python-pep8" ,python-pep8-1.5.7)
        ("python-pyflakes" ,python-pyflakes-0.8.1)
        ("python-mccabe" ,python-mccabe)
        ("python-mock" ,python-mock)
        ("python-nose" ,python-nose)))
    (home-page "https://gitlab.com/pycqa/flake8")
    (synopsis
      "The modular source code checker: pep8, pyflakes and co")
    (description
      "Flake8 is a wrapper around PyFlakes, pep8 and python-mccabe.")
    (license license:expat)))

(define-public python2-flake8
  (package-with-python2 python-flake8))

;; This will only be needed by the python-hacking package and will not be
;; necessary once python-hacking > 0.10.2 is released.
(define-public python-flake8-2.2.4
  (package (inherit python-flake8)
    (inputs
      `(("python-setuptools" ,python-setuptools)
        ("python-pep8" ,python-pep8-1.5.7)
        ("python-pyflakes" ,python-pyflakes-0.8.1)
        ("python-mccabe" ,python-mccabe-0.2.1)
        ("python-mock" ,python-mock)
        ("python-nose" ,python-nose)))
    (version "2.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flake8" version))
        (sha256
          (base32
            "1r9wsry4va45h1rck5hxd3vzsg2q3y6lnl6pym1bxvz8ry19jwx8"))))))

(define-public python2-flake8-2.2.4
  (package-with-python2 python-flake8-2.2.4))

(define-public python-mistune
  (package
    (name "python-mistune")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/m/mistune/mistune-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "17zqjp9m4d1w3jf2rbbq5xshcw24q1vlcv24gkgfqqyyymajxahx"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-nose" ,python-nose)
       ("python-cython" ,python-cython)))
    (home-page "https://github.com/lepture/mistune")
    (synopsis "Markdown parser in pure Python")
    (description "This package provides a fast markdown parser in pure
Python.")
    (license bsd-3)))

(define-public python2-mistune
  (package-with-python2 python-mistune))

(define-public python-ptyprocess
  (package
    (name "python-ptyprocess")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/ptyprocess/ptyprocess-"
             version ".tar.gz"))
       (sha256
        (base32
         "0nggns5kikn32yyda2zrj1xdmh49pi3v0drggcdwljbv36r8zdyw"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-nose" ,python-nose)))
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
    (license isc)))

(define-public python2-ptyprocess
  (package-with-python2 python-ptyprocess))

(define-public python-terminado
  (package
    (name "python-terminado")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/t/terminado/terminado-"
             version ".tar.gz"))
       (sha256
        (base32
         "1dkmp1n8dj5v1jl9mfrq8lwyc7dsfrvcmz2bgkpg315sy7pr7s33"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-tornado" ,python-tornado)
       ("python-ptyprocess" ,python-ptyprocess)))
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-nose" ,python-nose)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
                  (lambda _
                    (zero? (system* "nosetests")))))))
    (home-page "https://github.com/takluyver/terminado")
    (synopsis "Terminals served to term.js using Tornado websockets")
    (description "This package provides a Tornado websocket backend for the
term.js Javascript terminal emulator library.")
    (license bsd-2)))

(define-public python2-terminado
  (let ((terminado (package-with-python2 python-terminado)))
    (package (inherit terminado)
             (propagated-inputs
              `(("python2-tornado" ,python2-tornado)
                ("python2-backport-ssl-match-hostname"
                 ,python2-backport-ssl-match-hostname)
                ,@(alist-delete "python-tornado"
                                (package-propagated-inputs terminado)))))))

(define-public python-fonttools
  (package
    (name "python-fonttools")
    (version "2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pypi.python.org/packages/source/F/FontTools/"
                    "fonttools-" version ".tar.gz"))
              (sha256
               (base32
                "08ay3x4ijarwhl60gqx2i9jzq6pxs20p4snc2d1q5jagh4rn39lb"))))
    (build-system python-build-system)
    (arguments '(#:test-target "check"))
    (propagated-inputs
     ;; XXX: module not found if setuptools is not available.
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://github.com/behdad/fonttools")
    (synopsis "Tools to manipulate font files")
    (description
     "FontTools/TTX is a library to manipulate font files from Python.  It
supports reading and writinfg of TrueType/OpenType fonts, reading and writing
of AFM files, reading (and partially writing) of PS Type 1 fonts.  The package
also contains a tool called “TTX” which converts TrueType/OpenType fonts to and
from an XML-based format.")
    (license (non-copyleft "file://LICENSE.txt"
                           "See LICENSE.txt in the distribution."))))

(define-public python2-fonttools
  (package-with-python2 python-fonttools))

(define-public python-ly
  (package
    (name "python-ly")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/python-ly/python-ly-"
             version ".tar.gz"))
       (sha256
        (base32
         "1bsjg4q9ihr8bfdclrcmb8yjcg8xm9dznh58f3zsyrkrjzwbhcd2"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (synopsis "Tool and library for manipulating LilyPond files")
    (description "This package provides a Python library to parse, manipulate
or create documents in LilyPond format.  A command line program ly is also
provided that can be used to do various manipulations with LilyPond files.")
    (home-page "https://pypi.python.org/pypi/python-ly")
    (license gpl2+)))

(define-public python-appdirs
  (package
    (name "python-appdirs")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/a/appdirs/appdirs-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1iddva7v3fq0aqzsahkazxr7vpw28mqcrsy818z4wyiqnkplbhlg"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (home-page "http://github.com/ActiveState/appdirs")
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
    (version "0.41")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://bitbucket.org/nikratio/python-llfuse/downloads/"
                    "llfuse-" version ".tar.bz2"))
              (sha256
               (base32
                "0yzy8ixpmxk00kdq6lx5vvwbs0n6s59qnja5q0js2ahbqyxiz2hb"))))
    (build-system python-build-system)
    (inputs
     `(("fuse" ,fuse)
       ("attr" ,attr)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-setuptools" ,python-setuptools)))
    (synopsis "Python bindings for FUSE")
    (description
     "Python-LLFUSE is a set of Python bindings for the low level FUSE API.")
    (home-page "https://bitbucket.org/nikratio/python-llfuse/")
    ;; Python-LLFUSE includes underscore.js, which is MIT (expat) licensed.
    ;; The rest of the package is licensed under LGPL2.0 or later.
    (license (list license:expat lgpl2.0+))))

(define-public python2-llfuse
  (package-with-python2 python-llfuse))

(define-public python-msgpack
  (package
    (name "python-msgpack")
    (version "0.4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pypi.python.org/packages/source/m/"
                    "msgpack-python/msgpack-python-" version ".tar.gz"))
              (sha256
               (base32
                "1527c76b6fn4zzkgfq5xvhh7x9a9686g7fjiz717rw5vklf5ik5z"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (synopsis "MessagePack (de)serializer")
    (description "MessagePack is a fast, compact binary serialization format,
suitable for similar data to JSON.  This package provides CPython bindings for
reading and writing MessagePack data.")
    (home-page "https://pypi.python.org/pypi/msgpack-python/")
    (license asl2.0)))

(define-public python2-msgpack
  (package-with-python2 python-msgpack))

(define-public python-netaddr
  (package
    (name "python-netaddr")
    (version "0.7.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/n/netaddr/netaddr-"
             version
             ".tar.gz"))
       (sha256
         (base32
          "06dxjlbcicq7q3vqy8agq11ra01kvvd47j4mk6dmghjsyzyckxd1"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ;; No tests.
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/drkjam/netaddr/")
    (synopsis "Pythonic manipulation of  network addresses")
    (description
      "A Python library for representing and manipulating IPv4, IPv6, CIDR, EUI
and MAC network addresses.")
    (license bsd-3)))

(define-public python2-netaddr
  (package-with-python2 python-netaddr))

(define-public python-wrapt
  (package
    (name "python-wrapt")
    (version "1.10.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/w/wrapt/wrapt-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0cq8rlpzkxzk48b50yrfhzn1d1hrq4gjcdqlrgq4v5palgiv9jwr"))))
    (build-system python-build-system)
    (arguments
     ;; Tests are not included in the tarball, they are only available in the
     ;; git repository.
     `(#:tests? #f))
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/GrahamDumpleton/wrapt")
    (synopsis "Module for decorators, wrappers and monkey patching")
    (description
      "The aim of the wrapt module is to provide a transparent object proxy for
  Python, which can be used as the basis for the construction of function
  wrappers and decorator functions.")
    (license bsd-2)))

(define-public python2-wrapt
  (package-with-python2 python-wrapt))

(define-public python-iso8601
  (package
  (name "python-iso8601")
  (version "0.1.10")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://pypi.python.org/packages/source/i/iso8601/iso8601-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1qf01afxh7j4gja71vxv345if8avg6nnm0ry0zsk6j3030xgy4p7"))))
  (build-system python-build-system)
  (inputs
    `(("python-setuptools" ,python-setuptools)))
  (home-page "https://bitbucket.org/micktwomey/pyiso8601")
  (synopsis "Module to parse ISO 8601 dates")
  (description
    "This module parses the most common forms of ISO 8601 date strings (e.g.
@code{2007-01-14T20:34:22+00:00}) into @code{datetime} objects.")
  (license license:expat)))

(define-public python2-iso8601
  (package-with-python2 python-iso8601))

(define-public python-monotonic
  (package
    (name "python-monotonic")
    (version "0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/m/monotonic/monotonic-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0yz0bcbwx8r2c01czzfpbrxddynxyk9k95jj8h6sgcb7xmfvl998"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/atdt/monotonic")
    (synopsis "Implementation of time.monotonic() for Python 2 & < 3.3")
    (description
      "This module provides a monotonic() function which returns the value (in
fractional seconds) of a clock which never goes backwards.")
    (license asl2.0)))

(define-public python2-monotonic
  (package-with-python2 python-monotonic))

(define-public python-webob
  (package
    (name "python-webob")
    (version "1.5.0b0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/W/WebOb/WebOb-"
             version ".tar.gz"))
       (sha256
        (base32
         "140b3iczclk1j0405rvw5gxshqfkhcc8254fj520z3m23cwbql4a"))))
    (build-system python-build-system)
    (inputs
      `(("python-nose" ,python-nose)
        ("python-setuptools" ,python-setuptools)))
    (home-page "http://webob.org/")
    (synopsis "WSGI request and response object")
    (description
      "WebOb provides wrappers around the WSGI request environment, and an
object to help create WSGI responses.")
    (license license:expat)))

(define-public python2-webob
  (package-with-python2 python-webob))

(define-public python-xlrd
  (package
    (name "python-xlrd")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pypi.python.org/packages/source/x/"
                                  "xlrd/xlrd-" version ".tar.gz"))
              (sha256
               (base32
                "0wpa55nvidmm5m2qr622dsh3cj46akdk0h3zjgzschcmydck73cf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Current test in setup.py does not work as of 0.9.4, so use nose to
         ;; run tests instead for now.
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (native-inputs `(("python-nose"       ,python-nose)
                     ("python-setuptools" ,python-setuptools)))
    (home-page "http://www.python-excel.org/")
    (synopsis "Library for extracting data from Excel files")
    (description "This packages provides a library to extract data from
spreadsheets using Microsoft Excel® proprietary file formats @samp{.xls} and
@samp{.xlsx} (versions 2.0 onwards).  It has support for Excel dates and is
Unicode-aware.  It is not intended as an end-user tool.")
    (license bsd-3)))

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
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (home-page "http://code.google.com/p/prettytable/")
    (synopsis "Display tabular data in an ASCII table format")
    (description
      "A library designed to represent tabular data in visually appealing ASCII
tables.  PrettyTable allows for selection of which columns are to be printed,
independent alignment of columns (left or right justified or centred) and
printing of sub-tables by specifying a row range.")
    (license bsd-3)))

(define-public python2-prettytable
  (package-with-python2 python-prettytable))

(define-public python-pyasn1
  (package
    (name "python-pyasn1")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/p/"
                           "pyasn1/pyasn1-" version ".tar.gz"))
       (sha256
        (base32
         "0iw31d9l0zwx35szkzq72hiw002wnqrlrsi9dpbrfngcl1ybwcsx"))))
    (build-system python-build-system)
    (home-page "http://pyasn1.sourceforge.net/")
    (synopsis "ASN.1 types and codecs")
    (description
     "This is an implementation of ASN.1 types and codecs in Python.  It is
suitable for a wide range of protocols based on the ASN.1 specification.")
    (license bsd-2)))

(define-public python2-pyasn1
  (package-with-python2 python-pyasn1))

(define-public python2-ipaddress
  (package
    (name "python2-ipaddress")
    (version "1.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/i/"
                           "ipaddress/ipaddress-" version ".tar.gz"))
       (sha256
        (base32
         "0givid4963n57nsjibms2fc347zmcs188q1hw9al1dkc9kj4nvr2"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; no tests
       #:python ,python-2))
    (home-page "https://github.com/phihag/ipaddress")
    (synopsis "IP address manipulation library")
    (description
     "This package provides a fast, lightweight IPv4/IPv6 manipulation library
in Python.  This library is used to create, poke at, and manipulate IPv4 and
IPv6 addresses and networks.  This is a port of the Python 3.3 ipaddress
module to older versions of Python.")
    (license psfl)))

(define-public python-idna
  (package
    (name "python-idna")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/i/"
                           "idna/idna-" version ".tar.gz"))
       (sha256
        (base32
         "0frxgmgi234lr9hylg62j69j4ik5zhg0wz05w5dhyacbjfnrl68n"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
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
    (license bsd-4)))

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
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/alex/pretend")
    (synopsis "Library for stubbing in Python")
    (description
     "Pretend is a library to make stubbing with Python easier.  Stubbing is a
technique for writing tests.  You may hear the term mixed up with mocks,
fakes, or doubles.  Basically, a stub is an object that returns pre-canned
responses, rather than doing any computation.")
    (license bsd-3)))

(define-public python2-pretend
  (package-with-python2 python-pretend))

(define-public python-cryptography-vectors
  (package
    (name "python-cryptography-vectors")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/c/"
                           "cryptography-vectors/cryptography_vectors-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0dx98kcypmarwwhi6rjwy30ridys2ja6mc6mjf0svd4nllkaljdq"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/pyca/cryptography")
    (synopsis "Test vectors for the cryptography package.")
    (description
      "This package contains test vectors for the cryptography package.")
    ;; Distributed under either BSD-3 or ASL2.0
    (license (list bsd-3 asl2.0))))

(define-public python2-cryptography-vectors
  (package-with-python2 python-cryptography-vectors))

(define-public python-cryptography
  (package
    (name "python-cryptography")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/c/"
                           "cryptography/cryptography-" version ".tar.gz"))
       (sha256
        (base32
         "1jmcidddbbgdavvnvjjc0pda4b9a5i9idsivchn69pqxx68x8k6n"))))
    (build-system python-build-system)
    (inputs
     `(("openssl" ,openssl)))
    (propagated-inputs
     `(("python-cffi" ,python-cffi)
       ("python-six" ,python-six)
       ("python-pyasn1" ,python-pyasn1)
       ("python-enum34" ,python-enum34)
       ("python-idna" ,python-idna)
       ("python-iso8601" ,python-iso8601)))
    (native-inputs
     `(("python-cryptography-vectors" ,python-cryptography-vectors)
       ("python-setuptools" ,python-setuptools)
       ("python-pretend" ,python-pretend)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/pyca/cryptography")
    (synopsis "Cryptographic recipes and primitives for Python")
    (description
      "cryptography is a package which provides cryptographic recipes and
primitives to Python developers.  It aims to be the “cryptographic standard
library” for Python.  The package includes both high level recipes, and low
level interfaces to common cryptographic algorithms such as symmetric ciphers,
message digests and key derivation functions.")
    ;; Distributed under either BSD-3 or ASL2.0
    (license (list bsd-3 asl2.0))))

(define-public python2-cryptography
  (let ((crypto (package-with-python2 python-cryptography)))
    (package (inherit crypto)
      (propagated-inputs
       `(("python2-ipaddress" ,python2-ipaddress)
         ,@(package-propagated-inputs crypto))))))

(define-public python-pyopenssl
  (package
    (name "python-pyopenssl")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/p/"
                           "pyOpenSSL/pyOpenSSL-" version ".tar.gz"))
       (sha256
        (base32
         "0wnnq15rhj7fhdcd8ycwiw6r6g3w9f9lcy6cigg8226vsrq618ph"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "OpenSSL/test/test_ssl.py"
              (("client\\.connect\\(\\('verisign\\.com', 443\\)\\)")
               "return True")
              ;; FIXME: disable broken test
              (("test_set_tmp_ecdh") "disabled__set_tmp_ecdh"))
            (substitute* "OpenSSL/test/test_crypto.py"
              (("command = b\"openssl \"")
               (string-append "command = b\""
                              (assoc-ref inputs "openssl")
                              "/bin/openssl" " \""))
              ;; FIXME: disable four broken tests
              (("test_der")             "disabled__der")
              (("test_digest")          "disabled__digest")
              (("test_get_extension")   "disabled__get_extension")
              (("test_extension_count") "disabled__extension_count"))
            #t)))))
    (propagated-inputs
     `(("python-cryptography" ,python-cryptography)
       ("python-six" ,python-six)))
    (inputs
     `(("openssl" ,openssl)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/pyca/pyopenssl")
    (synopsis "Python wrapper module around the OpenSSL library")
    (description
      "PyOpenSSL is a high-level wrapper around a subset of the OpenSSL
library.")
    (license asl2.0)))

(define-public python2-pyopenssl
  (let ((pyopenssl (package-with-python2 python-pyopenssl)))
    (package (inherit pyopenssl)
      (propagated-inputs
       `(("python2-cryptography" ,python2-cryptography)
         ,@(alist-delete "python-cryptography"
                         (package-propagated-inputs pyopenssl)))))))

(define-public python-pip
  (package
    (name "python-pip")
    (version "7.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pip/pip-"
             version ".tar.gz"))
       (sha256
        (base32
         "0xx4aypfgchxdknxq7gyqghd8wb221zrzyqlbabzm32jy237j16a"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools" ,python-setuptools)
        ("python-virtualenv" ,python-virtualenv)
        ;; Tests
        ("python-mock" ,python-mock)
        ("python-pytest" ,python-pytest)
        ("python-scripttest" ,python-scripttest)))
    (home-page "https://pip.pypa.io/")
    (synopsis
      "Package manager for Python software")
    (description
      "Pip is a package manager for Python software, that finds packages on the
Python Package Index (PyPI).")
    (license license:expat)))

(define-public python2-pip
  (package-with-python2 python-pip))

(define-public python-tlsh
  (package
    (name "python-tlsh")
    (version "3.4.1")                             ;according to CMakeLists.txt
    (home-page "https://github.com/trendmicro/tlsh")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    ;; This is a commit right after 3.4.1; see
                    ;; <https://github.com/trendmicro/tlsh/issues/9>.
                    (commit "3ae3f1f")))
              (sha256
               (base32
                "12cvnr5ndm5cg6i7lch93id90kgwgrigjgrj8f186nh3h4bf9chj"))
              (file-name (string-append name "-" version "-checkout"))))
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
    (license asl2.0)))

(define-public python2-tlsh
  (package
    (inherit python-tlsh)
    (name "python2-tlsh")
    (inputs `(("python" ,python-2)))))

(define-public python-libarchive-c
  (package
    (name "python-libarchive-c")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pypi.python.org/packages/source/l/libarchive-c/libarchive-c-"
                    version ".tar.gz"))
              (sha256
               (base32
                "089lrz6xyrfnk55v35vis6jyqyyl77w093057djyspnd2744wi2n"))))
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
                                         "/lib/libarchive.so'"))))

                     ;; Do not make a compressed egg (see
                     ;; <http://bugs.gnu.org/20765>).
                     (let ((port (open-file "setup.cfg" "a")))
                       (display "\n[easy_install]\nzip_ok = 0\n"
                                port)
                       (close-port port)
                       #t))))))
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("libarchive" ,libarchive)))
    (home-page "https://github.com/Changaco/python-libarchive-c")
    (synopsis "Python interface to libarchive")
    (description
     "This package provides Python bindings to libarchive, a C library to
access possibly compressed archives in many different formats.  It uses
Python's @code{ctypes} foreign function interface (FFI).")
    (license lgpl2.0+)))

(define-public python2-libarchive-c
  (package-with-python2 python-libarchive-c))

(define-public python-file
  (package
    (inherit file)
    (name "python-file")
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                                ;no tests
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
    (synopsis "Python bindings to the libmagic file type guesser")))

(define-public python2-file
  (package-with-python2 python-file))

(define-public python-debian
  (package
    (name "python-debian")
    (version "0.1.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/python-debian/python-debian-"
             version ".tar.gz"))
       (sha256
        (base32
         "193faznwnjc3n5991wyzim6h9gyq1zxifmfrnpm3avgkh7ahyynh"))))
    (build-system python-build-system)
    (inputs
     `(("python-six" ,python-six)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
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
    (license gpl3+)))

(define-public python2-debian
  (package-with-python2 python-debian))

(define-public python-chardet
  (package
    (name "python-chardet")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/c/chardet/chardet-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1ak87ikcw34fivcgiz2xvi938dmclh078az65l9x3rmgljrkhgp5"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/chardet/chardet")
    (synopsis "Universal encoding detector for Python 2 and 3")
    (description
     "This package provides @code{chardet}, a Python module that can
automatically detect a wide range of file encodings.")
    (license lgpl2.1+)))

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
     `(("python-pytest" ,python-pytest)
       ("python-setuptools" ,python-setuptools)))
    (arguments
     `(#:phases (alist-replace
                 'check
                 (lambda _ (zero? (system* "py.test")))
                 %standard-phases)))
    (home-page "http://docopt.org")
    (synopsis "Command-line interface description language for Python")
    (description "This library allows the user to define a command-line
interface from a program's help message rather than specifying it
programatically with command-line parsers like @code{getopt} and
@code{argparse}.")
    (license license:expat)))

(define-public python2-docopt
  (package-with-python2 python-docopt))

(define-public python-zope-event
  (package
    (name "python-zope-event")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/z"
                           "/zope.event/zope.event-" version ".tar.gz"))
       (sha256
        (base32
         "11p75zpfz3ffhz21nzx9wb23xs993ck5s6hkjcvhswwizni5jynw"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://pypi.python.org/pypi/zope.event")
    (synopsis "Event publishing system for Python")
    (description "Zope.event provides an event publishing API, intended for
use by applications which are unaware of any subscribers to their events.  It
is a simple event-dispatching system on which more sophisticated event
dispatching systems can be built.")
    (license zpl2.1)))

(define-public python2-zope-event
  (package-with-python2 python-zope-event))

(define-public python-zope-interface
  (package
    (name "python-zope-interface")
    (version "4.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/z"
                           "/zope.interface/zope.interface-" version ".tar.gz"))
       (sha256
        (base32
         "0ks8h73b2g4bkad821qbv0wzjppdrwys33i7ka45ik3wxjg1l8if"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope-event" ,python-zope-event)))
    (home-page "https://github.com/zopefoundation/zope.interface")
    (synopsis "Python implementation of the \"design by contract\"
methodology")
    (description "Zope.interface provides an implementation of \"object
interfaces\" for Python.  Interfaces are a mechanism for labeling objects as
conforming to a given API or contract.")
    (license zpl2.1)))

(define-public python2-zope-interface
  (package-with-python2 python-zope-interface))

(define-public python-zope-exceptions
  (package
    (name "python-zope-exceptions")
    (version "4.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/z"
                           "/zope.exceptions/zope.exceptions-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0zwxaaa66sqxg5k7zcrvs0fbg9ym1njnxnr28dfmchzhwjvwnfzl"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; circular dependency with zope.testrunner
    (propagated-inputs
     `(("python-zope-interface" ,python-zope-interface)))
    (home-page "http://cheeseshop.python.org/pypi/zope.exceptions")
    (synopsis "Zope exceptions")
    (description "Zope.exceptions provides general-purpose exception types
that have uses outside of the Zope framework.")
    (license zpl2.1)))

(define-public python2-zope-exceptions
  (package-with-python2 python-zope-exceptions))

(define-public python-zope-testing
  (package
    (name "python-zope-testing")
    (version "4.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/z"
                           "/zope.testing/zope.testing-" version ".tar.gz"))
       (sha256
        (base32
         "1yvglxhzvhl45mndvn9gskx2ph30zz1bz7rrlyfs62fv2pvih90s"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-zope-exceptions" ,python-zope-exceptions)))
    (propagated-inputs
     `(("python-zope-interface" ,python-zope-interface)))
    (home-page "http://pypi.python.org/pypi/zope.testing")
    (synopsis "Zope testing helpers")
    (description "Zope.testing provides a number of testing utilities for HTML
forms, HTTP servers, regular expressions, and more.")
    (license zpl2.1)))

(define-public python2-zope-testing
  (package-with-python2 python-zope-testing))

(define-public python-zope-testrunner
  (package
    (name "python-zope-testrunner")
    (version "4.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/z"
                           "/zope.testrunner/zope.testrunner-"
                           version ".zip"))
       (sha256
        (base32
         "1r7iqknhh55y45f64mz5hghgvzx34h1i11k350s0avx6q8gznja1"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-six" ,python-six)
       ("python-zope-exceptions" ,python-zope-exceptions)
       ("python-zope-testing" ,python-zope-testing)
       ("unzip" ,unzip)))
    (propagated-inputs
     `(("python-zope-interface" ,python-zope-interface)))
    (home-page "http://pypi.python.org/pypi/zope.testrunner")
    (synopsis "Zope testrunner script")
    (description "Zope.testrunner provides a script for running Python
tests.")
    (license zpl2.1)))

(define-public python2-zope-testrunner
  (let ((base (package-with-python2 python-zope-testrunner)))
    (package
      (inherit base)
      (native-inputs
       (append (package-native-inputs base)
               `(("python2-subunit" ,python2-subunit)
                 ("python2-mimeparse" ,python2-mimeparse)))))))

(define-public python-zope-i18nmessageid
  (package
    (name "python-zope-i18nmessageid")
    (version "4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/z"
             "/zope.i18nmessageid/zope.i18nmessageid-"
             version ".tar.gz"))
       (sha256
        (base32
         "1rslyph0klk58dmjjy4j0jxy21k03azksixc3x2xhqbkv97cmzml"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://pypi.python.org/pypi/zope.i18nmessageid")
    (synopsis "Message identifiers for internationalization")
    (description "Zope.i18nmessageid provides facilities for declaring
internationalized messages within program source text.")
    (license zpl2.1)))

(define-public python2-zope-i18nmessageid
  (package-with-python2 python-zope-i18nmessageid))

(define-public python-zope-schema
  (package
    (name "python-zope-schema")
    (version "4.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/z"
                           "/zope.schema/zope.schema-" version ".tar.gz"))
       (sha256
        (base32
         "1p943jdxb587dh7php4vx04qvn7b2877hr4qs5zyckvp5afhhank"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope-event" ,python-zope-event)
       ("python-zope-interface" ,python-zope-interface)))
    (native-inputs
     `(("python-zope-testing" ,python-zope-testing)))
    (home-page "http://pypi.python.org/pypi/zope.schema")
    (synopsis "Zope data schemas")
    (description "Zope.scheme provides extensions to zope.interface for
defining data schemas.")
    (license zpl2.1)))

(define-public python2-zope-schema
  (package-with-python2 python-zope-schema))

(define-public python-zope-configuration
  (package
    (name "python-zope-configuration")
    (version "4.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pypi.python.org/packages/source/z"
                                  "/zope.configuration/zope.configuration-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1x9dfqypgympnlm25p9m43xh4qv3p7d75vksv9pzqibrb4cggw5n"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope-i18nmessageid" ,python-zope-i18nmessageid)
       ("python-zope-schema" ,python-zope-schema)))
    (home-page "http://pypi.python.org/pypi/zope.configuration")
    (synopsis "Zope Configuration Markup Language")
    (description "Zope.configuration implements ZCML, the Zope Configuration
Markup Language.")
    (license zpl2.1)))

(define-public python2-zope-configuration
  (package-with-python2 python-zope-configuration))

(define-public python-zope-proxy
  (package
    (name "python-zope-proxy")
    (version "4.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/z"
                           "/zope.proxy/zope.proxy-" version ".tar.gz"))
       (sha256
        (base32
         "0pqwwmvm1prhwv1ziv9lp8iirz7xkwb6n2kyj36p2h0ppyyhjnm4"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope-interface" ,python-zope-interface)))
    (home-page "http://pypi.python.org/pypi/zope.proxy")
    (synopsis "Generic, transparent proxies")
    (description "Zope.proxy provides generic, transparent proxies for Python.
Proxies are special objects which serve as mostly-transparent wrappers around
another object, intervening in the apparent behavior of the wrapped object
only when necessary to apply the policy (e.g., access checking, location
brokering, etc.) for which the proxy is responsible.")
    (license zpl2.1)))

(define-public python2-zope-proxy
  (package-with-python2 python-zope-proxy))

(define-public python-zope-location
  (package
    (name "python-zope-location")
    (version "4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/z"
                           "/zope.location/zope.location-" version ".tar.gz"))
       (sha256
        (base32
         "1nj9da4ksiyv3h8n2vpzwd0pb03mdsh7zy87hfpx72b6p2zcwg74"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-zope-proxy" ,python-zope-proxy)
       ("python-zope-schema" ,python-zope-schema)))
    (home-page "http://pypi.python.org/pypi/zope.location/")
    (synopsis "Zope location library")
    (description "Zope.location implements the concept of \"locations\" in
Zope3, which are are special objects that have a structural location.")
    (license zpl2.1)))

(define-public python2-zope-location
  (package-with-python2 python-zope-location))

(define-public python-zope-security
  (package
    (name "python-zope-security")
    (version "4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/z"
                           "/zope.security/zope.security-" version ".tar.gz"))
       (sha256
        (base32
         "14zmf684amc0x32kq05yxnhfqd1cmyhafkw05gn81rn90zjv6ssy"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope-i18nmessageid" ,python-zope-i18nmessageid)
       ("python-zope-component" ,python-zope-component)
       ("python-zope-location" ,python-zope-location)
       ("python-zope-proxy" ,python-zope-proxy)
       ("python-zope-schema" ,python-zope-schema)
       ("python-zope-testrunner" ,python-zope-testrunner)
       ("python-zope-testing" ,python-zope-testing)))
    (home-page "http://pypi.python.org/pypi/zope.security")
    (synopsis "Zope security framework")
    (description "Zope.security provides a generic mechanism to implement
security policies on Python objects.")
    (license zpl2.1)))

(define-public python2-zope-security
  (let ((zope-security (package-with-python2 python-zope-security)))
    (package (inherit zope-security)
      (propagated-inputs
       `(("python2-zope-testrunner" ,python2-zope-testrunner)
         ,@(alist-delete
            "python-zope-testrunner"
            (package-propagated-inputs zope-security)))))))

(define-public python-zope-component
  (package
    (name "python-zope-component")
    (version "4.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/z"
                           "/zope.component/zope.component-" version ".tar.gz"))
       (sha256
        (base32
         "06pqr8m5jv12xjyy5b59hh9anl61cxkzhw9mka33r3nxalmi2b18"))))
    (build-system python-build-system)
    (arguments
     ;; Skip tests due to circular dependency with python-zope-security.
     '(#:tests? #f))
    (native-inputs
     `(("python-zope-testing" ,python-zope-testing)))
    (propagated-inputs
     `(("python-zope-event" ,python-zope-event)
       ("python-zope-interface" ,python-zope-interface)
       ("python-zope-i18nmessageid" ,python-zope-i18nmessageid)
       ("python-zope-configuration" ,python-zope-configuration)))
    (home-page "https://github.com/zopefoundation/zope.component")
    (synopsis "Zope Component Architecture")
    (description "Zope.component represents the core of the Zope Component
Architecture.  Together with the zope.interface package, it provides
facilities for defining, registering and looking up components.")
    (license zpl2.1)))

(define-public python2-zope-component
  (package-with-python2 python-zope-component))

(define-public python2-pythondialog
  (package
    (name "python2-pythondialog")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/p/"
                           "python2-pythondialog/python2-pythondialog-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1yhkagsh99bfi592ymczf8rnw8rk6n9hdqy3dd98m3yrx8zmjvry"))))
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
       #:python ,python-2
       #:tests? #f)) ; no test suite
    (propagated-inputs
     `(("dialog" ,dialog)))
    (home-page "http://pythondialog.sourceforge.net/")
    (synopsis "Python interface to the UNIX dialog utility")
    (description "A Python wrapper for the dialog utility.  Its purpose is to
provide an easy to use, pythonic and comprehensive Python interface to dialog.
This allows one to make simple text-mode user interfaces on Unix-like systems")
    (license lgpl2.1)))

(define-public python-pyrfc3339
  (package
    (name "python-pyrfc3339")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/p/"
                           "pyRFC3339/pyRFC3339-" version ".tar.gz"))
       (sha256
        (base32
         "1pp648xsjaw9h1xq2mgwzda5wis2ypjmzxlksc1a8grnrdmzy155"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytz" ,python-pytz)))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/kurtraschke/pyRFC3339")
    (synopsis "Python timestamp library")
    (description "Python library for generating and parsing RFC 3339-compliant
timestamps.")
    (license license:expat)))

(define-public python2-pyrfc3339
  (package-with-python2 python-pyrfc3339))

(define-public python-werkzeug
  (package
    (name "python-werkzeug")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/W/Werkzeug"
                           "/Werkzeug-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1gzwn1lkl90f3l1nzzxr7vjhm21qk8f837i8rvny5a209fcrhkzb"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "http://werkzeug.pocoo.org/")
    (synopsis "Utilities for WSGI applications")
    (description "One of the most advanced WSGI utility modules.  It includes a
powerful debugger, full-featured request and response objects, HTTP utilities to
handle entity tags, cache control headers, HTTP dates, cookie handling, file
uploads, a powerful URL routing system and a bunch of community-contributed
addon modules.")
    (license x11)))

(define-public python2-werkzeug
  (package-with-python2 python-werkzeug))

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
              (patches (list (search-patch "python-configobj-setuptools.patch")))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-six" ,python-six)))
    (synopsis "Config file reading, writing and validation")
    (description "ConfigObj is a simple but powerful config file reader and
writer: an ini file round tripper.  Its main feature is that it is very easy to
use, with a straightforward programmer’s interface and a simple syntax for
config files.")
    (home-page "https://github.com/DiffSK/configobj")
    (license bsd-3)))

(define-public python2-configobj
  (package-with-python2 python-configobj))

(define-public python-configargparse
  (package
    (name "python-configargparse")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pypi.python.org/packages/source/C/ConfigArgParse/"
                    "ConfigArgParse-" version ".tar.gz"))
              (sha256
               (base32
                "19wh919gbdbzxzpagg52q3lm62yicm95ddlcx77dyjc1slyshl1v"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Bug in test suite filed upstream:
     ;; https://github.com/bw2/ConfigArgParse/issues/32
     '(#:tests? #f))
    (synopsis "Replacement for argparse")
    (description "A drop-in replacement for argparse that allows options to also
be set via config files and/or environment variables.")
    (home-page "https://github.com/bw2/ConfigArgParse")
    (license license:expat)))

(define-public python2-configargparse
  (package-with-python2 python-configargparse))

(define-public python-ndg-httpsclient
  (package
    (name "python-ndg-httpsclient")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pypi.python.org/packages/source/n/ndg-httpsclient/"
                    "ndg_httpsclient-" version ".tar.gz"))
              (sha256
                (base32
                  "0x32ibixm3vv5m9xfk83xsqm8xcqw4dd0khbh6qbri6rxgymbhg8"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pyopenssl" ,python-pyopenssl)))
    (synopsis "HTTPS support for Python's httplib and urllib2")
    (description "This is a HTTPS client implementation for httplib and urllib2
based on PyOpenSSL.  PyOpenSSL provides a more fully featured SSL implementation
over the default provided with Python and importantly enables full verification
of the SSL peer.")
    (home-page "https://github.com/cedadev/ndg_httpsclient/")
    (license bsd-3)))

;; python2-openssl requires special care, so package-with-python2 is
;; insufficient.
(define-public python2-ndg-httpsclient
  (package (inherit python-ndg-httpsclient)
    (name "python2-ndg-httpsclient")
    (arguments `(#:python ,python-2))
    (propagated-inputs
     `(("python2-pyopenssl" ,python2-pyopenssl)))))
