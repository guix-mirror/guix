;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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
   (version "4.1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gawk/gawk-" version
                                ".tar.xz"))
            (sha256
             (base32 "0hin2hswbbd6kd6i4zzvgciwpl5fba8d2s524z8y5qagyz3x010q"))))
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
                %standard-phases)))
   (inputs `(("libsigsegv" ,libsigsegv)

             ,@(if (%current-target-system)
                   `(("bash" ,bash))
                   '())))

   (home-page "http://www.gnu.org/software/gawk/")
   (synopsis "A text scanning and processing language")
   (description
    "Many computer users need to manipulate text files: extract and then
operate on data from parts of certain lines while discarding the rest, make
changes in various text files wherever certain patterns appear, and so on.
To write a program to do these things in a language such as C or Pascal is a
time-consuming inconvenience that may take many lines of code.  The job is
easy with awk, especially the GNU implementation: Gawk.

The awk utility interprets a special-purpose programming language that makes
it possible to handle many data-reformatting jobs with just a few lines of
code.")
   (license gpl3+)))
