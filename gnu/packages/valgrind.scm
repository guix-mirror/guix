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

(define-module (gnu packages valgrind)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages))

(define-public valgrind
  (package
    (name "valgrind")
    (version "3.8.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://valgrind.org/downloads/valgrind-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1nsqk70ry3221sd62s4f0njcrncppszs4xxjcak13lxyfq2y0fs7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:patches (list (assoc-ref %build-inputs "patch/glibc-2.17"))
       #:phases (alist-cons-after
                 'install 'patch-suppression-files
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Don't assume the FHS.
                   (let* ((out (assoc-ref outputs "out"))
                          (dir (string-append out "/lib/valgrind")))
                     (substitute* (find-files dir "\\.supp$")
                       (("obj:/lib") "obj:*/lib")
                       (("obj:/usr/X11R6/lib") "obj:*/lib")
                       (("obj:/usr/lib") "obj:*/lib"))
                     #t))
                 %standard-phases)))
    (inputs `(;; GDB is needed to provide a sane default for `--db-command'.
              ("gdb" ,gdb)

              ("patch/glibc-2.17"
               ,(search-patch "valgrind-glibc.patch"))))
    (native-inputs `(("perl" ,perl)))
    (home-page "http://www.valgrind.org/")
    (synopsis "Debugging and profiling tool suite")
    (description
     "Valgrind is an instrumentation framework for building dynamic analysis
tools.  There are Valgrind tools that can automatically detect many memory
management and threading bugs, and profile your programs in detail. You can
also use Valgrind to build new tools.")
    (license gpl2+)))
