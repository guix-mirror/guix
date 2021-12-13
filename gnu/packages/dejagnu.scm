;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flasher.co.il>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages dejagnu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages tcl))

(define-public dejagnu
  (package
    (name "dejagnu")
    (version "1.6.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/dejagnu/dejagnu-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0qfj2wd4qk1yn9yzam6g8nmyxfazcc0knjyyibycb2ainkhp21hd"))))
    (build-system gnu-build-system)
    (inputs (list expect))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Note: The test-suite *requires* /dev/pts among the
             ;; `build-chroot-dirs' of the build daemon when
             ;; building in a chroot.  See
             ;; <http://thread.gmane.org/gmane.linux.distributions.nixos/1036>
             ;; for details.
             (if (and (directory-exists? "/dev/pts")
                      (directory-exists? "/proc"))
                 (begin
                  ;; Provide `runtest' with a log name, otherwise it
                  ;; tries to run `whoami', which fails when in a chroot.
                  (setenv "LOGNAME" "guix-builder")

                  ;; The test-suite needs to have a non-empty stdin:
                  ;; <http://lists.gnu.org/archive/html/bug-dejagnu/2003-06/msg00002.html>.
                  (unless (zero? (system "make check < /dev/zero"))
                    (error "make check failed")))
                 (display "test suite cannot be run, skipping\n"))
             #t))
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Use the right `expect' binary.
             (let ((out    (assoc-ref outputs "out"))
                   (expect (assoc-ref inputs "expect")))
               (substitute* (string-append out "/bin/runtest")
                 (("^mypath.*$" all)
                  (string-append all
                                 "export PATH="
                                 expect "/bin:$PATH\n")))
               #t))))))
    (home-page
     "https://www.gnu.org/software/dejagnu/")
    (synopsis "GNU software testing framework")
    (description
     "DejaGnu is a framework for testing software.  In effect, it serves as
a front-end for all tests written for a program.  Thus, each program can have
multiple test suites, which are then all managed by a single harness.")
    (license gpl3+)))
