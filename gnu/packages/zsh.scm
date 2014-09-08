;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Kevin Lemonnier <lemonnierk@ulrar.net>
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

(define-module (gnu packages zsh)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages autotools)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public zsh
  (package
    (name "zsh")
    (version "5.0.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.zsh.org/pub/zsh-" version
                                 ".tar.gz"))
             (sha256
              (base32 "0f9y2lkv6xs5nxgj7ld7sbncy454sgamz21fm4109mxqlqa32fph"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags '("--with-tcsetpgrp" "--enable-pcre")
                 #:phases (alist-cons-before
                           'configure 'fix-sh
                           (lambda _
                             (substitute*
                                 '("configure"
                                   "configure.ac"
                                   "Src/exec.c"
                                   "Src/mkmakemod.sh"
                                   "Config/installfns.sh"
                                   "Config/defs.mk.in"
                                   "Test/E01options.ztst"
                                   "Test/A05execution.ztst"
                                   "Test/A01grammar.ztst"
                                   "Test/B02typeset.ztst"
                                   "Completion/Unix/Command/_init_d"
                                   "Util/preconfig")
                               (("/bin/sh") (which "sh"))))
                           %standard-phases)))
    (native-inputs `(("autoconf", autoconf)))
    (inputs `(("ncurses", ncurses)
              ("pcre", pcre)
              ("perl", perl)))
    (synopsis "Powerful shell for interactive use and scripting")
    (description "The Z shell (zsh) is a Unix shell that can be used
as an interactive login shell and as a powerful command interpreter
for shell scripting. Zsh can be thought of as an extended Bourne shell
with a large number of improvements, including some features of bash,
ksh, and tcsh.")
    (home-page "http://www.zsh.org/")

    ;; The whole thing is under an MIT/X11-style license, but there's one
    ;; command, 'Completion/Unix/Command/_darcs', which is under GPLv2+.
    (license gpl2+)))
