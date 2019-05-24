;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages gawk)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages libsigsegv)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public gawk
  (package
   (name "gawk")
   (version "5.0.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gawk/gawk-" version
                                ".tar.xz"))
            (sha256
             (base32 "01j409gharghgf7h0pjzywkimhz2ldrxf85nvf3sv1dl1vnr3w2h"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (add-before 'configure 'set-shell-file-name
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; Refer to the right shell.
                     (let ((bash (assoc-ref inputs "bash")))
                       (substitute* "io.c"
                         (("/bin/sh")
                          (string-append bash "/bin/sh")))

                       ;; When cross-compiling, remove dependencies on the
                       ;; `check-for-shared-lib-support' target, which tries
                       ;; to run the cross-built `gawk'.
                       ,@(if (%current-target-system)
                             '((substitute* "extension/Makefile.in"
                                 (("^.*: check-for-shared-lib-support" match)
                                  (string-append "### " match))))
                             '())

                       #t)))

                 (add-before 'check 'adjust-test-infrastructure
                   (lambda _
                     ;; Remove dependency on 'more' (from util-linux), which
                     ;; would needlessly complicate bootstrapping.
                     (substitute* "test/Makefile"
                       (("\\| more") ""))

                     ;; Adjust the shebang in that file since it is then diff'd
                     ;; against the actual test output.
                     (substitute* "test/watchpoint1.ok"
                       (("#! /usr/bin/gawk")
                        (string-append "#!" (which "gawk"))))
                     #t)))))

   (inputs `(("libsigsegv" ,libsigsegv)

             ,@(if (%current-target-system)
                   `(("bash" ,bash))
                   '())))

   (home-page "https://www.gnu.org/software/gawk/")
   (synopsis "Text scanning and processing language")
   (description
    "Gawk is the GNU implementation of Awk, a specialized programming
language for the easy manipulation of formatted text, such as tables of data.
Gawk features many extensions beyond the traditional implementation,
including network access, sorting, and large libraries.")
   (license gpl3+)))
