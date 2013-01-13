;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (distro packages mailutils)
  #:use-module (distro)
  #:use-module (distro packages linux)
  #:use-module (distro packages gnutls)
  #:use-module (distro packages gdbm)
  #:use-module (distro packages guile)
  #:use-module (distro packages ncurses)
  #:use-module (distro packages readline)
  #:use-module (distro packages dejagnu)
  #:use-module (distro packages m4)
  #:use-module (distro packages texinfo)
  #:use-module (distro packages mysql)
  #:use-module (distro packages autotools)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public mailutils
  (package
    (name "mailutils")
    (version "2.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/mailutils/mailutils-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0szbqa12zqzldqyw97lxqax3ja2adis83i7brdfsxmrfw68iaf65"))))
    (build-system gnu-build-system)
    (arguments
     '(;; TODO: Add `--with-sql'.
       #:patches (list (assoc-ref %build-inputs
                                  "patch/gets-undeclared"))
       #:phases (alist-cons-before
                 'build 'pre-build
                 (lambda _
                   ;; Use Guile 2.0's public API.
                   (substitute* "libmu_scm/mu_message.c"
                     (("scm_i_string_length")
                      "scm_c_string_length"))

                   ;; This file should be generated to use the right
                   ;; value of $(libdir) et al.
                   (delete-file "libmu_scm/mailutils.scm")

                   ;; Use the right file name for `cat'.
                   (substitute* "testsuite/lib/mailutils.exp"
                     (("/bin/cat")
                      (which "cat"))))
                 %standard-phases)
       #:parallel-tests? #f))
    (inputs
     `(("dejagnu" ,dejagnu)
       ("m4" ,m4)
       ("texinfo" ,texinfo)
       ("guile" ,guile-2.0)
       ("gnutls" ,gnutls)
       ("ncurses" ,ncurses)
       ("readline" ,readline)
       ("linux-pam" ,linux-pam)
       ("libtool" ,libtool)
       ("gdbm" ,gdbm)
       ("patch/gets-undeclared"
        ,(search-patch "m4-gets-undeclared.patch"))))
    (home-page "http://www.gnu.org/software/mailutils/")
    (synopsis "A rich and powerful protocol-independent mail framework")
    (description
     "GNU Mailutils is a rich and powerful protocol-independent mail
framework.  It contains a series of useful mail libraries, clients, and
servers.  These are the primary mail utilities for the GNU system.  The
central library is capable of handling electronic mail in various
mailbox formats and protocols, both local and remote.  Specifically,
this project contains a POP3 server, an IMAP4 server, and a Sieve mail
filter.  It also provides a POSIX `mailx' client, and a collection of
other handy tools.

The GNU Mailutils libraries supply an ample set of primitives for
handling electronic mail in programs written in C, C++, Python or
Scheme.

The utilities provided by Mailutils include imap4d and pop3d mail
servers, mail reporting utility comsatd, general-purpose mail delivery
agent maidag, mail filtering program sieve, and an implementation of MH
message handling system.")
    (license
     ;; Libraries are under LGPLv3+, and programs under GPLv3+.
     (list gpl3+ lgpl3+))))
