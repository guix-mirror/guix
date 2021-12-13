;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages cook)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages compression)
  #:use-module (guix build-system gnu))

(define-public cook
  (package
    (name "cook")
    (version "2.34")
    (source
     (origin
       (method url-fetch)
       (uri "http://fossies.org/linux/misc/old/cook-2.34.tar.gz")
       (sha256
        (base32
         "104saqnqql1l7zr2pm3f718fdky3ds8j07c6xvwrs1rfkhrw58yw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f ; There are some nasty racy rules in the Makefile.
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-conf
           (lambda _
             (substitute* (append '("common/env.c")
                                  (find-files "test" "\\.sh"))
               (("/bin/sh") (which "sh")))

             ;; Guix's binutils (because it wants bit-reproducable builds) is
             ;; is configured with the  --enable-deterministic-archives flag.
             ;; This means the timestamp of files appended to an ar archive
             ;; are automatically and silently mutated to 00:00 1 Jan 1970
             ;; which plays havoc with this test, for which correct timestamps
             ;; are very important. Adding the U flag undoes the effect of
             ;; --enable-deterministic-archives and allows this test to work
             ;; again.
             (substitute* "test/00/t0077a.sh"
               (("ar qc") "ar qcU"))

             ;; Guix builds have LC_ALL set to "en_US.utf8", which causes
             ;; `date` to use a 12-hour clock instead of 24h, which in turn
             ;; makes t0217a.sh fail because of unexpected date output.
             (substitute* "test/02/t0217a.sh"
               (("export TZ")
                "export TZ\nLC_ALL=POSIX\nexport LC_ALL"))

             (setenv "SH" (which "sh"))
             #t)))))
    (native-inputs (list bison
                         ;; For building the documentation:
                         groff
                         ;; For the tests:
                         sharutils
                         ;; One test wants rsh.  However there is no rsh server
                         ;; running in the build environment and so far as I'm
                         ;; aware, it cannot be started without root.
                         ;; This test is therefore just skipped.
                         ;; ("inetutils" ,inetutils)
                         ed))
    (home-page (string-append "https://web.archive.org/web/20140727122520/"
                              "http://miller.emu.id.au/pmiller/software/cook/"))
    (synopsis "Tool for constructing files")
    (description "Cook is a tool for constructing files.  It is given a set of
files to create, and recipes of how to create them.  In any non-trivial program
there will be prerequisites to performing the actions necessary to creating
any file, such as include files.  Cook provides a mechanism to define these.")
    (license gpl3+)))
