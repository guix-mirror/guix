;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 doncatnip <gnopap@gmail.com>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk))

(define-public lua
  (package
    (name "lua")
    (version "5.2.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.lua.org/ftp/lua-"
                                 version ".tar.gz"))
             (sha256
              (base32 "0jwznq0l8qg9wh5grwg07b5cy3lzngvl5m2nl1ikp6vqssmf9qmr"))
             (patches (search-patches "lua-pkgconfig.patch"
                                      "lua52-liblua-so.patch"))))
    (build-system gnu-build-system)
    (inputs `(("readline" ,readline)))
    (arguments
     '(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:test-target "test"
       #:make-flags
       '("MYCFLAGS=-fPIC -DLUA_DL_DLOPEN"
         "linux")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "make" "install"
                               (string-append "INSTALL_TOP=" out)
                               (string-append "INSTALL_MAN=" out
                                              "/share/man/man1")))))))))
    (home-page "http://www.lua.org/")
    (synopsis "Embeddable scripting language")
    (description
     "Lua is a powerful, fast, lightweight, embeddable scripting language.  Lua
combines simple procedural syntax with powerful data description constructs
based on associative arrays and extensible semantics.  Lua is dynamically typed,
runs by interpreting bytecode for a register-based virtual machine, and has
automatic memory management with incremental garbage collection, making it ideal
for configuration, scripting, and rapid prototyping.")
    (license license:x11)))

(define-public lua-5.1
  (package (inherit lua)
    (version "5.1.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.lua.org/ftp/lua-"
                                 version ".tar.gz"))
             (sha256
              (base32 "0cskd4w0g6rdm2q8q3i4n1h3j8kylhs3rq8mxwl9vwlmlxbgqh16"))
             (patches (search-patches "lua51-liblua-so.patch"
                                      "lua-CVE-2014-5461.patch"
                                      "lua51-pkgconfig.patch"))))))

(define-public luajit
  (package
    (name "luajit")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://luajit.org/download/LuaJIT-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0zc0y7p6nx1c0pp4nhgbdgjljpfxsb5kgwp4ysz22l1p2bms83v2"))
              (patches (search-patches "luajit-symlinks.patch"
                                       "luajit-no_ldconfig.patch"))))
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
    (license license:x11)))

(define-public lua5.1-expat
  (package
    (name "lua5.1-expat")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://matthewwild.co.uk/projects/"
                                  "luaexpat/luaexpat-" version ".tar.gz"))
              (sha256
               (base32
                "1hvxqngn0wf5642i5p3vcyhg3pmp102k63s9ry4jqyyqc1wkjq6h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list "CC=gcc"
               (string-append "LUA_LDIR=" out "/share/lua/$(LUA_V)")
               (string-append "LUA_CDIR=" out "/lib/lua/$(LUA_V)")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           (lambda _
             (setenv "LUA_CPATH" "src/?.so;;")
             (setenv "LUA_PATH"  "src/?.lua;;")
             (and (zero? (system* "lua" "tests/test.lua"))
                  (zero? (system* "lua" "tests/test-lom.lua"))))))))
    (inputs
     `(("lua" ,lua-5.1)
       ("expat" ,expat)))
    (home-page "http://matthewwild.co.uk/projects/luaexpat/")
    (synopsis "SAX XML parser based on the Expat library")
    (description "LuaExpat is a SAX XML parser based on the Expat library.")
    (license (package-license lua-5.1))))

(define-public lua5.1-socket
  (package
    (name "lua5.1-socket")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://files.luaforge.net/releases/"
                                  "luasocket/luasocket/luasocket-"
                                  version "/luasocket-" version ".tar.gz"))
              (sha256
               (base32
                "19ichkbc4rxv00ggz8gyf29jibvc2wq9pqjik0ll326rrxswgnag"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "INSTALL_TOP_SHARE=" out "/share/lua/5.1")
               (string-append "INSTALL_TOP_LIB=" out "/lib/lua/5.1")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           (lambda _
             (setenv "LUA_CPATH" (string-append "src/?.so." ,version ";;"))
             (setenv "LUA_PATH"  "src/?.lua;;")
             (when (zero? (primitive-fork))
               (system* "lua" "test/testsrvr.lua"))
             (zero? (system* "lua" "test/testclnt.lua")))))))
    (inputs
     `(("lua" ,lua-5.1)))
    (home-page "http://www.tecgraf.puc-rio.br/~diego/professional/luasocket/")
    (synopsis "Socket library for Lua")
    (description "LuaSocket is a Lua extension library that is composed by two
parts: a C core that provides support for the TCP and UDP transport layers,
and a set of Lua modules that add support for functionality commonly needed by
applications that deal with the Internet.

Among the supported modules, the most commonly used implement the
SMTP (sending e-mails), HTTP (WWW access) and FTP (uploading and downloading
files) client protocols.  These provide a very natural and generic interface
to the functionality defined by each protocol.  In addition, you will find
that the MIME (common encodings), URL (anything you could possible want to do
with one) and LTN12 (filters, sinks, sources and pumps) modules can be very
handy.")
    (license (package-license lua-5.1))))

(define-public lua5.1-filesystem
  (package
    (name "lua5.1-filesystem")
    (version "1.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/keplerproject/"
                                  "luafilesystem/archive/v_"
                                  "1_6_3" ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s10ckxin0bysd6gaywqhxkpw3ybjhprr8m655b8cx3pxjwd49am"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("lua" ,lua-5.1)))
    (home-page "https://keplerproject.github.io/luafilesystem/index.html")
    (synopsis "File system library for Lua")
    (description "LuaFileSystem is a Lua library developed to complement the
set of functions related to file systems offered by the standard Lua
distribution.  LuaFileSystem offers a portable way to access the underlying
directory structure and file attributes.")
    (license (package-license lua-5.1))))

(define-public lua5.1-sec
  (package
    (name "lua5.1-sec")
    (version "0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/brunoos/luasec/archive/"
                                  "luasec-" version ".tar.gz"))
              (sha256
               (base32
                "0pgd1anzznl4s0h16wg8dlw9mgdb9h52drlcki6sbf5y31fa7wyf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list "linux"
               "CC=gcc"
               "LD=gcc"
               (string-append "LUAPATH=" out "/share/lua/5.1")
               (string-append "LUACPATH=" out "/lib/lua/5.1")))
       #:tests? #f ; no tests included
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("lua" ,lua-5.1)
       ("openssl" ,openssl)))
    (propagated-inputs
     `(("lua-socket" ,lua5.1-socket)))
    (home-page "https://github.com/brunoos/luasec/wiki")
    (synopsis "OpenSSL bindings for Lua")
    (description "LuaSec is a binding for OpenSSL library to provide TLS/SSL
communication.  It takes an already established TCP connection and creates a
secure session between the peers.")
    (license (package-license lua-5.1))))

(define-public lua-lgi
  (package
    (name "lua-lgi")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://github.com/pavouk/lgi/archive/"
              version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fmgdl5y4ph3yc6ycg865s3vai1rjkyda61cgqxk6zd13hmznw0c"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (add-before 'build 'set-env
           (lambda* (#:key inputs #:allow-other-keys)
             ;; we need to load cairo dynamically
             (let* ((cairo (string-append
                             (assoc-ref inputs "cairo") "/lib" )))
               (setenv "LD_LIBRARY_PATH" cairo)
               #t)))
         (add-before 'build 'set-lua-version
           (lambda _
             ;; lua version and therefore install directories are hardcoded
             ;; FIXME: This breaks when we update lua to >=5.3
             (substitute* "./lgi/Makefile"
               (("LUA_VERSION=5.1") "LUA_VERSION=5.2"))
             #t))
         (add-before 'check 'skip-test-gtk
           (lambda _
             ;; FIXME: Skip GTK tests:
             ;;   gtk3 - can't get it to run with the xorg-server config below
             ;;          and some non-gtk tests will also fail
             ;;   gtk2 - lots of functions aren't implemented
             ;; We choose gtk2 as the lesser evil and simply skip the test.
             ;; Currently, awesome is the only package that uses lua-lgi but
             ;; it doesn't need or interact with GTK using lua-lgi.
             (substitute* "./tests/test.lua"
               (("'gtk.lua',") "-- 'gtk.lua',"))
             #t))
         (add-before 'check 'start-xserver-instance
           (lambda* (#:key inputs #:allow-other-keys)
             ;; There must be a running X server during tests.
             (system (format #f "~a/bin/Xvfb :1 &"
                             (assoc-ref inputs "xorg-server")))
             (setenv "DISPLAY" ":1")
             #t)))))
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("glib" ,glib)
       ("pango", pango)
       ("gtk", gtk+-2)
       ("lua" ,lua)
       ("cairo" ,cairo)
       ("libffi" ,libffi)
       ("xorg-server", xorg-server)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/pavouk/lgi/")
    (synopsis "Lua bridge to GObject based libraries")
    (description
     "LGI is gobject-introspection based dynamic Lua binding to GObject
based libraries.  It allows using GObject-based libraries directly from Lua.
Notable examples are GTK+, GStreamer and Webkit.")
    (license license:expat)))
