;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 doncatnip <gnopap@gmail.com>
;;; Copyright © 2016, 2017, 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2016 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk))

(define-public lua
  (package
    (name "lua")
    (version "5.3.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://www.lua.org/ftp/lua-"
                                 version ".tar.gz"))
             (sha256
              (base32 "1b2qn2rv96nmbm6zab4l877bd4zq7wpwm8drwjiy2ih4jqzysbhc"))
             (patches (search-patches "lua-pkgconfig.patch"
                                      "lua-liblua-so.patch"))))
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
               (invoke "make" "install"
                       (string-append "INSTALL_TOP=" out)
                       (string-append "INSTALL_MAN=" out
                                      "/share/man/man1"))))))))
    (home-page "https://www.lua.org/")
    (synopsis "Embeddable scripting language")
    (description
     "Lua is a powerful, fast, lightweight, embeddable scripting language.  Lua
combines simple procedural syntax with powerful data description constructs
based on associative arrays and extensible semantics.  Lua is dynamically typed,
runs by interpreting bytecode for a register-based virtual machine, and has
automatic memory management with incremental garbage collection, making it ideal
for configuration, scripting, and rapid prototyping.")
    (license license:x11)))

(define-public lua-5.2
  (package (inherit lua)
           (version "5.2.4")
           (source
            (origin
              (method url-fetch)
              (uri (string-append "https://www.lua.org/ftp/lua-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0jwznq0l8qg9wh5grwg07b5cy3lzngvl5m2nl1ikp6vqssmf9qmr"))
              (patches (search-patches "lua-pkgconfig.patch"
                                       "lua-liblua-so.patch"))))))

(define-public lua-5.1
  (package (inherit lua)
    (version "5.1.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://www.lua.org/ftp/lua-"
                                 version ".tar.gz"))
             (sha256
              (base32 "0cskd4w0g6rdm2q8q3i4n1h3j8kylhs3rq8mxwl9vwlmlxbgqh16"))
             (patches (search-patches "lua51-liblua-so.patch"
                                      "lua-CVE-2014-5461.patch"
                                      "lua51-pkgconfig.patch"))))))

(define-public luajit
  (package
    (name "luajit")
    (version "2.1.0-beta3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://luajit.org/download/LuaJIT-"
                                  version ".tar.gz"))
              (sha256
               (base32 "1hyrhpkwjqsv54hnnx4cl8vk44h9d6c9w0fz1jfjz00w255y7lhs"))
              (patches (search-patches "luajit-no_ldconfig.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; luajit is distributed without tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'install 'create-luajit-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (with-directory-excursion bin
                 (symlink ,(string-append name "-" version)
                          ,name)
                 #t)))))
         #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (home-page "https://www.luajit.org/")
    (synopsis "Just in time compiler for Lua programming language version 5.1")
    (description
     "LuaJIT is a Just-In-Time Compiler (JIT) for the Lua
programming language.  Lua is a powerful, dynamic and light-weight programming
language.  It may be embedded or used as a general-purpose, stand-alone
language.")
    (license license:x11)))

(define (make-lua-expat name lua)
  (package
    (name name)
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
       (let ((out (assoc-ref %outputs "out"))
             (lua-version ,(version-major+minor (package-version lua))))
         (list "CC=gcc"
               (string-append "LUA_LDIR=" out "/share/lua/" lua-version)
               (string-append "LUA_CDIR=" out "/lib/lua/" lua-version)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           (lambda _
             (setenv "LUA_CPATH" "src/?.so;;")
             (setenv "LUA_PATH"  "src/?.lua;;")
             (invoke "lua" "tests/test.lua")
             (invoke "lua" "tests/test-lom.lua"))))))
    (inputs
     `(("lua" ,lua)
       ("expat" ,expat)))
    (home-page "http://matthewwild.co.uk/projects/luaexpat/")
    (synopsis "SAX XML parser based on the Expat library")
    (description "LuaExpat is a SAX XML parser based on the Expat library.")
    (license (package-license lua-5.1))))

(define-public lua5.1-expat
  (make-lua-expat "lua5.1-expat" lua-5.1))

(define-public lua5.2-expat
  (make-lua-expat "lua5.2-expat" lua-5.2))

(define (make-lua-socket name lua)
  (package
    (name name)
    (version "3.0-rc1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/diegonehab/luasocket")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1chs7z7a3i3lck4x7rz60ziwbf793gw169hpjdfca8y4yf1hzsxk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (lua-version ,(version-major+minor (package-version lua))))
         (list (string-append "INSTALL_TOP=" out)
               (string-append "LUAV?=" lua-version)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           (lambda _
             (setenv "LUA_CPATH" (string-append "src/?.so." ,version ";;"))
             (setenv "LUA_PATH"  "src/?.lua;;")
             (when (zero? (primitive-fork))
               (invoke "lua" "test/testsrvr.lua"))
             (invoke "lua" "test/testclnt.lua"))))))
    (inputs
     `(("lua" ,lua)))
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
    (license license:expat)))

(define-public lua5.1-socket
  (make-lua-socket "lua5.1-socket" lua-5.1))

(define-public lua5.2-socket
  (make-lua-socket "lua5.2-socket" lua-5.2))

(define (make-lua-filesystem name lua)
  (package
    (name name)
    (version "1.7.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/keplerproject/luafilesystem")
                     (commit (string-append "v"
                                            (string-join
                                              (string-split version #\.) "_")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zmprgkm9zawdf9wnw0v3w6ibaj442wlc6alp39hmw610fl4vghi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (lua-version ,(version-major+minor (package-version lua))))
         (list (string-append "PREFIX=" out)
               (string-append "LUA_LIBDIR=" out "/lib/lua/" lua-version)))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("lua" ,lua)))
    (home-page "https://keplerproject.github.io/luafilesystem/index.html")
    (synopsis "File system library for Lua")
    (description "LuaFileSystem is a Lua library developed to complement the
set of functions related to file systems offered by the standard Lua
distribution.  LuaFileSystem offers a portable way to access the underlying
directory structure and file attributes.")
    (license (package-license lua-5.1))))

(define-public lua-filesystem
  (make-lua-filesystem "lua-filesystem" lua))

(define-public lua5.1-filesystem
  (make-lua-filesystem "lua5.1-filesystem" lua-5.1))

(define-public lua5.2-filesystem
  (make-lua-filesystem "lua5.2-filesystem" lua-5.2))

(define (make-lua-sec name lua)
  (package
    (name name)
    (version "0.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/brunoos/luasec")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ssncgkggyr8i3z6zbvgrgsqj2q8676rnsikhpfwnk9n7sx4gwbl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (lua-version ,(version-major+minor (package-version lua))))
         (list "linux"
               "CC=gcc"
               "LD=gcc"
               (string-append "LUAPATH=" out "/share/lua/" lua-version)
               (string-append "LUACPATH=" out "/lib/lua/" lua-version)))
       #:tests? #f ; no tests included
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("lua" ,lua)
       ("openssl" ,openssl)))
    (propagated-inputs
     `(("lua-socket"
        ,(make-lua-socket
          (format #f "lua~a-socket"
                  (version-major+minor (package-version lua))) lua))))
    (home-page "https://github.com/brunoos/luasec/wiki")
    (synopsis "OpenSSL bindings for Lua")
    (description "LuaSec is a binding for OpenSSL library to provide TLS/SSL
communication.  It takes an already established TCP connection and creates a
secure session between the peers.")
    (license license:expat)))

(define-public lua5.1-sec
  (make-lua-sec "lua5.1-sec" lua-5.1))

(define-public lua5.2-sec
  (make-lua-sec "lua5.2-sec" lua-5.2))

(define-public lua-penlight
  (package
    (name "lua-penlight")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Tieske/Penlight.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qc2d1riyr4b5a0gnsmdw2lz5pw65s4ac60hc34w3mmk9l6yg6nl"))))
    (build-system trivial-build-system)
    (inputs
     `(("lua" ,lua)))
    (propagated-inputs
     `(("lua-filesystem" ,lua-filesystem)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (lua-version ,(version-major+minor (package-version lua)))
                (destination (string-append (assoc-ref %outputs "out")
                                            "/share/lua/" lua-version)))
           (mkdir-p destination)
           (with-directory-excursion source
             (copy-recursively "lua/" destination)))
         #t)))
    (home-page "http://tieske.github.io/Penlight/")
    (synopsis "Collection of general purpose libraries for the Lua language")
    (description "Penlight is a set of pure Lua libraries focusing on
input data handling (such as reading configuration files), functional
programming (such as map, reduce, placeholder expressions,etc), and OS
path management.  Much of the functionality is inspired by the Python
standard libraries.")
    (license license:expat)))

(define (make-lua-lgi name lua)
  (package
    (name name)
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pavouk/lgi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03rbydnj411xpjvwsyvhwy4plm96481d7jax544mvk7apd8sd5jj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'build 'set-env
           (lambda* (#:key inputs #:allow-other-keys)
             ;; We need to load cairo dynamically.
             (let* ((cairo (string-append (assoc-ref inputs "cairo") "/lib")))
               (setenv "LD_LIBRARY_PATH" cairo)
               #t)))
         (add-before 'build 'set-lua-version
           (lambda _
             ;; Lua version and therefore install directories are hardcoded.
             (substitute* "./lgi/Makefile"
               (("LUA_VERSION=5.1")
                (format #f
                        "LUA_VERSION=~a"
                        ,(version-major+minor (package-version lua)))))
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
    (native-inputs
     `(("dbus" ,dbus)                   ;tests use 'dbus-run-session'
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk" ,gtk+-2)
       ("libffi" ,libffi)
       ("lua" ,lua)
       ("pango" ,pango)
       ("xorg-server" ,xorg-server)))
    (home-page "https://github.com/pavouk/lgi/")
    (synopsis "Lua bridge to GObject based libraries")
    (description
     "LGI is gobject-introspection based dynamic Lua binding to GObject based
libraries.  It allows using GObject-based libraries directly from Lua.
Notable examples are GTK+, GStreamer and Webkit.")
    (license license:expat)))

(define-public lua-lgi
  (make-lua-lgi "lua-lgi" lua))

(define-public lua5.1-lgi
  (make-lua-lgi "lua5.1-lgi" lua-5.1))

(define-public lua5.2-lgi
  (make-lua-lgi "lua5.2-lgi" lua-5.2))

(define (make-lua-lpeg name lua)
  (package
    (name name)
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.inf.puc-rio.br/~roberto/lpeg/lpeg-"
                                  version ".tar.gz"))
              (sha256
               (base32 "1zjzl7acvcdavmcg5l7wi12jd4rh95q9pl5aiww7hv0v0mv6bmj8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; `make install` isn't available, so we have to do it manually
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (lua-version ,(version-major+minor (package-version lua))))
               (install-file "lpeg.so"
                             (string-append out "/lib/lua/" lua-version))
               (install-file "re.lua"
                             (string-append out "/share/lua/" lua-version))
               #t))))
       #:test-target "test"))
    (inputs `(("lua" ,lua)))
    (synopsis "Pattern-matching library for Lua")
    (description
     "LPeg is a pattern-matching library for Lua, based on Parsing Expression
Grammars (PEGs).")
    (home-page "http://www.inf.puc-rio.br/~roberto/lpeg")
    (license license:expat)))

(define-public lua-lpeg
  (make-lua-lpeg "lua-lpeg" lua))

(define-public lua5.1-lpeg
  (make-lua-lpeg "lua5.1-lpeg" lua-5.1))

(define-public lua5.2-lpeg
  (make-lua-lpeg "lua5.2-lpeg" lua-5.2))

(define (make-lua-luv name lua)
  (package
    (name name)
    (version "1.32.0-0")
    (source (origin
              ;; The release tarball includes the sources of libuv but does
              ;; not include the pkg-config files.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/luvit/luv.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c65c1lhbl0axnyks3910gjs0z0hw7w6jvl07g8kbpnbvfl4qajh"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; there are none
       #:configure-flags
       '("-DWITH_LUA_ENGINE=Lua"
         "-DWITH_SHARED_LIBUV=On"
         "-DBUILD_MODULE=Off"
         "-DBUILD_SHARED_LIBS=On"
         "-DLUA_BUILD_TYPE=System")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'copy-lua-compat
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "lua-compat")
                               "lua-compat")
             (setenv "CPATH"
                     (string-append (getcwd) "/lua-compat:"
                                    (or (getenv "CPATH") "")))
             #t)))))
    (inputs
     `(("lua" ,lua)
       ("libuv" ,libuv)))
    (native-inputs
     `(("lua-compat"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/keplerproject/lua-compat-5.3.git")
                 (commit "daebe77a2f498817713df37f0bb316db1d82222f")))
           (file-name "lua-compat-5.3-checkout")
           (sha256
            (base32
             "02a14nvn7aggg1yikj9h3dcf8aqjbxlws1bfvqbpfxv9d5phnrpz"))))))
    (home-page "https://github.com/luvit/luv/")
    (synopsis "Libuv bindings for Lua")
    (description
     "This library makes libuv available to Lua scripts.")
    (license license:asl2.0)))

(define-public lua-luv
  (make-lua-luv "lua-luv" lua))

(define-public lua5.1-luv
  (make-lua-luv "lua5.1-luv" lua-5.1))

(define-public lua5.2-luv
  (make-lua-luv "lua5.2-luv" lua-5.2))

;; Lua 5.3 is not supported.
(define (make-lua-bitop name lua)
  (package
    (name name)
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://bitop.luajit.org/download/"
                                  "LuaBitOp-" version ".tar.gz"))
              (sha256
               (base32
                "16fffbrgfcw40kskh2bn9q7m3gajffwd2f35rafynlnd7llwj1qj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list "INSTALL=install -pD"
             (string-append "INSTALLPATH=printf "
                            (assoc-ref %outputs "out")
                            "/lib/lua/"
                            ,(version-major+minor (package-version lua))
                            "/bit/bit.so"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs `(("lua" ,lua)))
    (home-page "https://bitop.luajit.org/index.html")
    (synopsis "Bitwise operations on numbers for Lua")
    (description
     "Lua BitOp is a C extension module for Lua which adds bitwise operations
on numbers.")
    (license license:expat)))

(define-public lua5.2-bitop
  (make-lua-bitop "lua5.2-bitop" lua-5.2))

(define-public lua5.1-bitop
  (make-lua-bitop "lua5.1-bitop" lua-5.1))

(define-public selene
  (package
    (name "selene")
    (version "2017.08.25")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jeremyong/Selene.git")
                    ;; The release is quite old.
                    (commit "ffe1ade2568d4cff5894552be8f43e63e379a4c9")))
              (file-name "Selene")
              (sha256
               (base32
                "1axrgv3rxxdsaf807lwvklfzicn6x6gpf35narllrnz9lg6hn508"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       ;; lua pc file in CMakeLists.txt is lua5.3.pc
       '("-DLUA_PC_CFG=lua;lua-5.3;lua-5.1")
       #:test-target "all"
       #:phases
       ;; This is a header only library
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((output (assoc-ref outputs "out"))
                    (source (assoc-ref inputs "source"))
                    (includedir (string-append output "/include")))
               (copy-recursively
                (string-append source "/include")
                includedir))
             #t))
         ;; The path of test files are hard coded.
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((output (assoc-ref outputs "out"))
                    (source (assoc-ref inputs "source"))
                    (builddir (getcwd))
                    (testdir  (string-append builddir "/test")))
               (copy-recursively (string-append source "/test") testdir)
               (invoke "make")
               (mkdir-p "runner")
               (copy-file "./test_runner" "./runner/test_runner")
               (chdir "./runner")
               (invoke "./test_runner")))))))
    (native-inputs
     `(("lua" ,lua)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/jeremyong/Selene")
    (synopsis "Lua C++11 bindings")
    (description
     "Selene is a simple C++11 header-only library enabling seamless
 interoperability between C++ and Lua programming language.")
    (license license:zlib)))
