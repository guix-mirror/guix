;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019 Meiyo Peng <meiyo@riseup.net>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages autotools))

(define-public log4cpp
  (package
    (name "log4cpp")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/log4cpp/log4cpp-"
                                  (version-major+minor version) ".x%20%28new%29"
                                  "/log4cpp-" (version-major+minor version)
                                  "/log4cpp-" version ".tar.gz"))
              (sha256
               (base32
                "07gmr3jyaf2239n9sp6h7hwdz1pv7b7aka8n06gmr2fnlmaymfrc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-call-stime
           (lambda _
             ;; Patch out use of 'stime' which was removed from glibc 2.31.
             ;; The test would not work in the build container anyway.
             (substitute* "tests/testDailyRollingFileAppender.cpp"
               (("if \\(stime\\(&now\\) == -1\\)")
                "if (1)"))
             #t)))))
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
    (version "0.4.0")
    (home-page "https://github.com/google/glog")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (sha256
               (base32
                "1xd3maiipfbxmhc9rrblc5x52nxvkwxp14npg31y5njqvkvzax9b"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'disable-signal-tests
                    (lambda _
                      ;; XXX: This test fails on non x86_64.  See e.g.
                      ;; https://github.com/google/glog/issues/219 and
                      ;; https://github.com/google/glog/issues/256.
                      (substitute* "Makefile"
                        (("\tsignalhandler_unittest_sh") "\t$(EMPTY)"))
                      #t)))))
    (native-inputs
     `(("perl" ,perl)                             ;for tests
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
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
    (version "6.5.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://vanheusden.com/multitail/multitail-"
                          version ".tgz"))
      (sha256
       (base32 "1vd9vdxyxsccl64ilx542ya5vlw2bpg6gnkq1x8cfqy6vxvmx7dj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX="
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
         (delete 'configure))           ; no configure script
       #:tests? #f)) ; no test suite (make check just runs cppcheck)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "https://vanheusden.com/multitail/")
    (synopsis "Monitor multiple log files")
    (description
     "MultiTail can monitor, color, filter, and merge log files and command
output in multiple windows in a terminal.")
    (license license:gpl2+)))

(define-public spdlog
  (package
    (name "spdlog")
    (version "1.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gabime/spdlog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "179krvg5sad6dviqpcjwg6czzknnilqszrg1d0fgp12h6sy66vqg"))))
    (build-system cmake-build-system)
    ;; TODO run benchmark. Currently not possible, as adding
    ;; (gnu packages benchmark) forms a dependency cycle
    (arguments
     '(#:configure-flags
       (list "-DSPDLOG_BUILD_BENCH=OFF"
             "-DSPDLOG_BUILD_TESTS=ON")))
    (home-page "https://github.com/gabime/spdlog")
    (synopsis "Fast C++ logging library")
    (description "Spdlog is a very fast header-only/compiled C++ logging
library.")
    ;; spdlog is under Expat license, but the bundled fmt library in
    ;; "include/spdlog/fmt/bundled" is under BSD 2 clause license.
    (license (list license:expat license:bsd-2))))
