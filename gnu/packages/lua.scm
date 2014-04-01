;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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
                      (zero? (system* "make" "install"
                                      (string-append "INSTALL_TOP=" out)
                                      (string-append "INSTALL_MAN=" out
                                                     "/share/man/man1")))))
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

(define-public luajit
  (package
    (name "luajit")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://luajit.org/download/LuaJIT-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0ydxpqkmsn2c341j4r2v6r5r0ig3kbwv3i9jran3iv81s6r6rgjm"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ;luajit is distributed without tests
       #:phases (alist-delete 'configure %standard-phases)
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (home-page "http://www.luajit.org/")
    (synopsis "Just in time compiler for Lua programming language version 5.1")
    (description
     "LuaJIT is a Just-In-Time Compiler (JIT) for the Lua
programming language.  Lua is a powerful, dynamic and light-weight programming
language.  It may be embedded or used as a general-purpose, stand-alone
language.")
    (license x11)))
