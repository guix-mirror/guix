;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages lsof)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages perl))

(define-public lsof
  (package
    (name "lsof")
    (version "4.94.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lsof-org/lsof")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yxv2jg6rnzys49lyrz9yjb4knamah4xvlqj596y6ix3vm4k3chp"))
       (patches (search-patches "lsof-fatal-test-failures.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list groff ; for soelim
           perl))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (setenv "LSOF_CC" ,(cc-for-target))
             (setenv "LSOF_MAKE" "make")

             ;; By default, the makefile captures the output of 'uname -a'.
             ;; Provide a fixed output instead to make builds reproducible.
             (setenv "LSOF_SYSINFO"
                     (string-append "GNU/" (utsname:sysname (uname))
                                    " (GNU Guix)"))

             (invoke "./Configure" "linux")))
         (add-after 'configure 'patch-timestamps
           (lambda _
             (substitute* "Makefile"
               (("`date`") "`date --date=@1`"))))
         (add-after 'build 'build-man-page
           (lambda _
             (with-output-to-file "lsof.8"
               (lambda _ (invoke "soelim" "Lsof.8")))))
         (add-before 'check 'disable-failing-tests
           (lambda _
             (substitute* "tests/Makefile"
               ;; Fails with ‘ERROR!!! client gethostbyaddr() failure’.
               (("(STDTST=.*) LTsock" _ prefix) prefix)
               ;; LTnfs fails without access to a remote NFS server, and LTlock
               ;; fails when run on a Btrfs file system (see:
               ;; https://github.com/lsof-org/lsof/issues/152).
               (("OPTTST=[[:space:]]*LTbigf LTdnlc LTlock LTnfs")
                "OPTTST = LTbigf LTdnlc"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "tests"
                 ;; Tests refuse to run on ‘unvalidated’ platforms.
                 (make-file-writable "TestDB")
                 (invoke "./Add2TestDB")

                 ;; The ‘standard’ tests suggest running ‘optional’ ones as well.
                 (invoke "make" "standard" "optional")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "lsof" (string-append out "/bin"))
               (install-file "lsof.8" (string-append out "/share/man/man8"))))))))
    (synopsis "Display information about open files")
    (description
     "Lsof stands for LiSt Open Files, and it does just that.
It lists information about files that are open by the processes running
on the system.")
    (license (license:fsf-free
              "file://00FAQ"
              "License inspired by zlib, see point 1.9 of 00FAQ in the distribution."))
    (home-page "https://people.freebsd.org/~abe/")))
