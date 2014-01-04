;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages lsh)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nettle)
  #:use-module ((gnu packages compression)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages guile))

(define-public liboop
  (package
    (name "liboop")
    (version "1.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://download.ofb.net/liboop/liboop-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0z6rlalhvfca64jpvksppc9bdhs7jwhiw4y35g5ibvh91xp3rn1l"))
      (patches (list (search-patch "liboop-mips64-deplibs-fix.patch")))))
    (build-system gnu-build-system)
    (home-page "http://www.lysator.liu.se/liboop/")
    (synopsis "Event loop library")
    (description "Liboop is a low-level event loop management library for
POSIX-based operating systems. It supports the development of modular,
multiplexed applications which may respond to events from several sources. It
replaces the \"select() loop\" and allows the registration of event handlers
for file and network I/O, timers and signals.  Since processes use these
mechanisms for almost all external communication, liboop can be used as the
basis for almost any application.")
    (license lgpl2.1+)))

(define-public lsh
  (package
    (name "lsh")
    (version "2.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/lsh/lsh-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1qqjy9zfzgny0rkb27c8c7dfsylvb6n0ld8h3an2r83pmaqr9gwb"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("m4" ,m4)
       ("guile" ,guile-2.0)
       ("gperf" ,gperf)
       ("psmisc" ,psmisc)))                       ; for `killall'
    (inputs
     `(("nettle" ,nettle)
       ("linux-pam" ,linux-pam)
       ("readline" ,readline)
       ("liboop" ,liboop)
       ("zlib" ,guix:zlib)
       ("gmp" ,gmp)))
    (arguments
     '(;; Skip the `configure' test that checks whether /dev/ptmx &
       ;; co. work as expected, because it relies on impurities (for
       ;; instance, /dev/pts may be unavailable in chroots.)
       #:configure-flags '("lsh_cv_sys_unix98_ptys=yes")

       ;; FIXME: Tests won't run in a chroot, presumably because
       ;; /etc/profile is missing, and thus clients get an empty $PATH
       ;; and nothing works.
       #:tests? #f

       #:phases
       (alist-cons-before
        'configure 'fix-test-suite
        (lambda _
          ;; Tests rely on $USER being set.
          (setenv "USER" "guix")

          (substitute* "src/testsuite/functions.sh"
            (("localhost")
             ;; Avoid host name lookups since they don't work in chroot
             ;; builds.
             "127.0.0.1")
            (("set -e")
             ;; Make tests more verbose.
             "set -e\nset -x"))

          (substitute* (find-files "src/testsuite" "-test$")
            (("localhost") "127.0.0.1"))

          (substitute* "src/testsuite/login-auth-test"
            (("/bin/cat")
             ;; Use the right path to `cat'.
             (which "cat"))))
        %standard-phases)))
    (home-page "http://www.lysator.liu.se/~nisse/lsh/")
    (synopsis "GNU implementation of the Secure Shell (ssh) protocols")
    (description
     "GNU lsh is a free implementation of the SSH version 2 protocol.  It is
used to create a secure line of communication between two computers,
providing shell access to the server system from the client.  It provides
both the server daemon and the client application, as well as tools for
manipulating key files.")
    (license gpl2+)))
