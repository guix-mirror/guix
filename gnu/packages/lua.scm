;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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

(define-module (gnu packages lua)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages readline))

(define-public lua
  (package
    (name "lua")
    (version "5.2.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.lua.org/ftp/lua-"
                                 version ".tar.gz"))
             (sha256
              (base32 "1rbv2ysq5fdksz7xg07dnrkl8i0gnx855hg4z6b324vng6l4sc34"))))
    (build-system gnu-build-system)
    (inputs `(("readline", readline)))
    (arguments
     '(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-1))
       #:test-target "test"
       #:phases (alist-replace
                 'build
                 (lambda _ (zero? (system* "make" "linux"))) ; XXX: Other OS.
                 (alist-replace
                  'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out")))
                      (zero? (system
                              (string-append "make install INSTALL_TOP=" out)))))
                  (alist-delete 'configure %standard-phases)))))
    (home-page "http://www.lua.org/")
    (synopsis "An embeddable scripting language.")
    (description
     "Lua is a powerful, fast, lightweight, embeddable scripting language.  Lua
combines simple procedural syntax with powerful data description constructs
based on associative arrays and extensible semantics. Lua is dynamically typed,
runs by interpreting bytecode for a register-based virtual machine, and has
automatic memory management with incremental garbage collection, making it ideal
for configuration, scripting, and rapid prototyping.")
    (license x11)))
