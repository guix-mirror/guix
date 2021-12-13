;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (gnu packages libdaemon)
  #:use-module (gnu packages autotools)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libdaemon
  (package
    (name "libdaemon")
    (version "0.14")
    (source (origin
             (method url-fetch)
             (uri (list
                   (string-append
                    "mirror://debian/pool/main/libd/libdaemon/libdaemon_"
                    version ".orig.tar.gz")
                   "http://pkgs.fedoraproject.org/repo/pkgs/libdaemon/libdaemon-0.14.tar.gz/509dc27107c21bcd9fbf2f95f5669563/libdaemon-0.14.tar.gz"

                   ;; This used to be the canonical URL but it vanished.
                   ;; See <http://bugs.gnu.org/18639>.
                   ;; (string-append
                   ;;  "http://0pointer.de/lennart/projects/libdaemon/libdaemon-"
                   ;;  version ".tar.gz")
                   ))
             (sha256
              (base32
               "0d5qlq5ab95wh1xc87rqrh1vx6i8lddka1w3f1zcqvcqdxgyn8zx"))
             (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (native-inputs
     (if (and=> (%current-target-system) target-aarch64?)
         `(("config" ,config)) ; for config.sub
         '()))
    (arguments
     `(,@(if (%current-target-system)
             ;; The 'setpgrp' test cannot provide an answer when cross-compiling,
             ;; so provide the right one for glibc.
             `(#:configure-flags (list "ac_cv_func_setpgrp_void=yes"

                                       ;; TODO: Move this globally on the next
                                       ;; rebuild cycle.
                                       ;; Set a valid localstatedir for the
                                       ;; benefit of the default
                                       ;; 'daemon_pid_file_proc', used by the
                                       ;; Hurd's console client.
                                       "--localstatedir=/var"))
             '())
       ,@(if (and=> (%current-target-system) target-aarch64?)
             `(#:phases
               (modify-phases %standard-phases
                 (add-before 'configure 'update-config.sub
                   (lambda _
                     ;; Replace outdated config.sub such that aarch64
                     ;; will be recognised as an architecture.
                     (delete-file "config.sub")
                     (symlink (which "config.sub") "config.sub")))))
             '())))
    ;; XXX: Stale URL, missing replacement.  See <http://bugs.gnu.org/18639>.
    (home-page "http://0pointer.de/lennart/projects/libdaemon/")

    (synopsis "Lightweight C library that eases the writing of UNIX daemons")
    (description
     "Libdaemon is a lightweight C library that eases the writing of UNIX
daemons.  It consists of the following parts:

  • A wrapper around fork() which does the correct daemonization procedure of
    a process

  • A wrapper around syslog() for simpler and compatible log output to Syslog
    or STDERR

  • An API for writing PID files

  • An API for serializing UNIX signals into a pipe for usage with select() or
    poll()

  • An API for running subprocesses with STDOUT and STDERR redirected to
    syslog.

APIs like these are used in most daemon software available.  It is not that
simple to get it done right and code duplication is not a goal.")
    (license lgpl2.1+)))
