;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
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

(define-module (gnu packages logging)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages autotools))

(define-public log4cpp
  (package
    (name "log4cpp")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/log4cpp/log4cpp-"
                                  (version-major+minor version) ".x%20%28new%29"
                                  "/log4cpp-" (version-major+minor version)
                                  "/log4cpp-" version ".tar.gz"))
              (sha256
               (base32
                "1l5yz5rfzzv6g3ynrj14mxfsk08cp5h1ssr7d74hjs0accrg7arm"))))
    (build-system gnu-build-system)
    (synopsis "Log library for C++")
    (description
     "Log4cpp is library of C++ classes for flexible logging to files, syslog,
IDSA and other destinations.  It is modeled after the Log4j Java library,
staying as close to their API as is reasonable.")
    (home-page "http://log4cpp.sourceforge.net/")
    (license license:lgpl2.1+)))

(define-public glog
  (package
    (name "glog")
    (version "0.3.5")
    (home-page "https://github.com/google/glog")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1q6ihk2asbx95a56kmyqwysq1x3grrw9jwqllafaidf0l84f903m"))
              (file-name (string-append name "-" version ".tar.gz"))
              (patches (search-patches "glog-gcc-5-demangling.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)                             ;for tests
       ("autoconf" ,(autoconf-wrapper))
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'add-automake-files
                    (lambda _
                      ;; The 'test-driver' file is a dangling symlink to
                      ;; /usr/share/automake; replace it.  We can't just run
                      ;; 'automake -ac' because it complains about version
                      ;; mismatch, so run the whole thing.
                      (delete-file "test-driver")
                      (delete-file "configure")   ;it's read-only
                      (zero? (system* "autoreconf" "-vfi")))))))
    (synopsis "C++ logging library")
    (description
     "Google glog is a library that implements application-level logging.
This library provides logging APIs based on C++-style streams and various
helper macros.  You can log a message by simply streaming things to log at a
particular severity level.  It allows logging to be controlled from the
command line.")
    (license license:bsd-3)))

(define-public tailon
  (package
    (name "tailon")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32
         "0wl2wm6p3pc0vkk33s7rzgcfvs9cwxfmlz997pdfhlw72r00l7s5"))))
    (build-system python-build-system)
    (inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-sockjs-tornado" ,python-sockjs-tornado)
       ("python-tornado-http-auth" ,python-tornado-http-auth)
       ("python-tornado" ,python-tornado)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-commands.py
                     (lambda args
                       (substitute* "tailon/commands.py"
                         (("self\\.first_in_path\\('grep'\\)")
                          (string-append"'" (which "grep") "'"))
                         (("self\\.first_in_path\\('gawk', 'awk'\\)")
                          (string-append"'" (which "gawk") "'"))
                         (("self\\.first_in_path\\('gsed', 'sed'\\)")
                          (string-append"'" (which "sed") "'"))
                         (("self\\.first_in_path\\('gtail', 'tail'\\)")
                          (string-append"'" (which "tail") "'")))
                       #t)))))
    (home-page "https://tailon.readthedocs.io/")
    (synopsis
     "Webapp for looking at and searching through log files")
    (description
     "Tailon provides a web interface around the tail, grep, awk and sed
commands, displaying the results via a web interface.")
    (license license:bsd-3)))

(define-public multitail
  (package
    (name "multitail")
    (version "6.4.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://vanheusden.com/multitail/multitail-"
                          version ".tgz"))
      (sha256
       (base32
        "1zd1r89xkxngl1pdrvsc877838nwkfqkbcgfqm3vglwalxc587dg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             "PREFIX="
             (string-append "DESTDIR="
                            (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-curses-lib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "mt.h"
                 (("ncursesw\\/panel.h") "panel.h")
                 (("ncursesw\\/ncurses.h") "ncurses.h")))
             #t))
         (delete 'configure))
       #:tests? #f)) ; no test suite (make check just runs cppcheck)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "https://vanheusden.com/multitail/")
    (synopsis "Monitor multiple logfiles")
    (description
     "MultiTail allows you to monitor logfiles and command output in multiple
windows in a terminal, colorize, filter and merge.")
    (license license:gpl2+)))
