;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
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
  #:use-module (gnu packages bash)
  #:use-module (gnu packages libsigsegv)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public gawk
  (package
   (name "gawk")
   (version "4.1.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gawk/gawk-" version
                                ".tar.xz"))
            (sha256
             (base32 "1nz83vpss8xv7m475sv4qhhj40g74nvcw0y9kwq9ds8wzfmcdm7g"))))
   (build-system gnu-build-system)
   (arguments
    `(#:parallel-tests? #f                ; test suite fails in parallel

      #:phases (alist-cons-before
                'configure 'set-shell-file-name
                (lambda* (#:key inputs #:allow-other-keys)
                  ;; Refer to the right shell.
                  (let ((bash (assoc-ref inputs "bash")))
                    (substitute* "io.c"
                      (("/bin/sh")
                       (string-append bash "/bin/bash")))

                    ;; When cross-compiling, remove dependencies on the
                    ;; `check-for-shared-lib-support' target, which tries to
                    ;; run the cross-built `gawk'.
                    ,@(if (%current-target-system)
                          '((substitute* "extension/Makefile.in"
                              (("^.*: check-for-shared-lib-support" match)
                               (string-append "### " match))))
                          '())))

                (alist-cons-before
                 'check 'install-locales
                 (lambda _
                   ;; A bunch of tests require the availability of a UTF-8
                   ;; locale and otherwise fail.  Give them what they want.
                   (setenv "LOCPATH" (getcwd))
                   (zero? (system* "localedef" "--no-archive"
                                   "--prefix" (getcwd) "-i" "en_US"
                                   "-f" "UTF-8" "./en_US.UTF-8")))
                 %standard-phases))))
   (inputs `(("libsigsegv" ,libsigsegv)

             ,@(if (%current-target-system)
                   `(("bash" ,bash))
                   '())))

   (home-page "http://www.gnu.org/software/gawk/")
   (synopsis "Text scanning and processing language")
   (description
    "Gawk is the GNU implementation of Awk, a specialized programming
language for the easy manipulation of formatted text, such as tables of data.
Gawk features many extensions beyond the traditional implementation,
including network access, sorting, and large libraries.")
   (license gpl3+)))
