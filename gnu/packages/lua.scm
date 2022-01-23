;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 doncatnip <gnopap@gmail.com>
;;; Copyright © 2016, 2017, 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2016 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Simon South <simon@simonsouth.net>
;;; Copyright © 2020 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2021 Vinícius dos Santos Oliveira <vini.ipsmaker@gmail.com>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2022 Brandon Lucas <br@ndon.dk>
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
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages re2c)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

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
    (inputs (list readline))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:test-target "test"
       #:make-flags
       (list "MYCFLAGS=-fPIC -DLUA_DL_DLOPEN"
             (string-append "CC=" ,(cc-for-target))
             (string-append "SYSLIBS=-L" (assoc-ref %build-inputs "readline")
                            "/lib")
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

(define-public lua-5.4
  (package (inherit lua)
           (version "5.4.3")
           (source (origin
                     (method url-fetch)
                     (uri (string-append "https://www.lua.org/ftp/lua-"
                                         version ".tar.gz"))
                     (sha256
                      (base32 "1yxvjvnbg4nyrdv10bq42gz6dr66pyan28lgzfygqfwy2rv24qgq"))
                     (patches (search-patches "lua-5.4-pkgconfig.patch"
                                              "lua-5.4-liblua-so.patch"))))))

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
    ;; On powerpc64le-linux, the build fails with an error: "No support for
    ;; PowerPC 64 bit mode (yet)".  See: https://issues.guix.gnu.org/49220
    (supported-systems (delete "powerpc64le-linux" %supported-systems))
    (description
     "LuaJIT is a Just-In-Time Compiler (JIT) for the Lua
programming language.  Lua is a powerful, dynamic and light-weight programming
language.  It may be embedded or used as a general-purpose, stand-alone
language.")
    (license license:x11)))

(define-public luajit-lua52-openresty
  (package
    (inherit luajit)
    (name "luajit-lua52-openresty")
    (version "2.1-20201229")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openresty/luajit2.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07haj27kbpbnkv836c2nd36h2xislrmri52w0zbpxvl68xk6g96p"))))
    (arguments
     `(#:tests? #f                      ;no test
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure script
         (add-after 'unpack 'enable-lua52-compat
           (lambda _
             (substitute* "src/Makefile"
               (("#(XCFLAGS\\+= -DLUAJIT_ENABLE_LUA52COMPAT)" _ flag) flag))
             #t)))))
    (home-page "https://github.com/openresty/luajit2")
    (synopsis "OpenResty's Branch of LuaJIT 2")
    (description
     "This is the official OpenResty branch of LuaJIT.  It is not to be
considered a fork, since changes are regularly synchronized from the upstream
LuaJIT project.  This package also enables the Lua 5.2 compat mode needed by
some projects.")))

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
         (list ,(string-append "CC=" (cc-for-target))
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
     (list lua expat))
    (home-page "https://matthewwild.co.uk/projects/luaexpat/")
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
     (list lua))
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
     (list lua))
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

(define (make-lua-ossl name lua)
  (package
    (name name)
    (version "20170903")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://25thandclement.com/~william/"
                                  "projects/releases/luaossl-" version ".tgz"))
              (sha256
               (base32
                "10392bvd0lzyibipblgiss09zlqh3a5zgqg1b9lgbybpqb9cv2k3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (lua-api-version ,(version-major+minor (package-version lua))))
         (list ,(string-append "CC=" (cc-for-target))
               "CFLAGS='-D HAVE_SYS_SYSCTL_H=0'" ; sys/sysctl.h is deprecated
               (string-append "prefix=" out)
               (string-append "LUA_APIS=" lua-api-version)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (lua-version ,(version-major+minor (package-version lua))))
               (setenv "LUA_CPATH"
                       (string-append out "/lib/lua/" lua-version "/?.so;;"))
               (setenv "LUA_PATH"
                       (string-append out "/share/lua/" lua-version "/?.lua;;"))
               (with-directory-excursion "regress"
                 (for-each (lambda (f)
                             (invoke "lua" f))
                           (find-files "." "^[0-9].*\\.lua$"))))
             #t)))))
    (inputs
     (list lua openssl))
    (home-page "https://25thandclement.com/~william/projects/luaossl.html")
    (synopsis "OpenSSL bindings for Lua")
    (description "The luaossl extension module for Lua provides comprehensive,
low-level bindings to the OpenSSL library, including support for certificate and
key management, key generation, signature verification, and deep bindings to the
distinguished name, alternative name, and X.509v3 extension interfaces.  It also
binds OpenSSL's bignum, message digest, HMAC, cipher, and CSPRNG interfaces.")
    (license license:expat)))

(define-public lua-ossl
  (make-lua-ossl "lua-ossl" lua))

(define-public lua5.1-ossl
  (make-lua-ossl "lua5.1-ossl" lua-5.1))

(define-public lua5.2-ossl
  (make-lua-ossl "lua5.2-ossl" lua-5.2))

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
               ,(string-append "CC=" (cc-for-target))
               "LD=gcc"
               (string-append "LUAPATH=" out "/share/lua/" lua-version)
               (string-append "LUACPATH=" out "/lib/lua/" lua-version)))
       #:tests? #f ; no tests included
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list lua openssl))
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

(define (make-lua-cqueues name lua lua-ossl)
  (package
    (name name)
    (version "20171014")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://25thandclement.com/~william/"
                                  "projects/releases/cqueues-" version ".tgz"))
              (sha256
               (base32
                "1dabhpn6r0hlln8vx9hxm34pfcm46qzgpb2apmziwg5z51fi4ksb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 string-fun))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (lua-api-version ,(version-major+minor (package-version lua))))
         (list ,(string-append "CC=" (cc-for-target))
               (string-append "LUA_APIS=" lua-api-version)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (replace 'install
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (apply invoke "make" "install"
                      (append make-flags
                              (list (string-append "DESTDIR=" out)
                                    "prefix="))))))
         (add-after 'install 'check
           (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
             (let*
                 ((lua-version ,(version-major+minor (package-version lua)))
                  (env-suffix (if (equal? lua-version "5.1")
                                  ""
                                  (string-append
                                   "_"
                                   (string-replace-substring lua-version "." "_"))))

                  (lua-ossl (assoc-ref inputs "lua-ossl"))
                  (out (assoc-ref outputs "out"))

                  (lua-cpath (lambda (p)
                               (string-append p "/lib/lua/" lua-version "/?.so")))
                  (lua-path (lambda (p)
                              (string-append p "/share/lua/" lua-version "/?.lua"))))
               ;; The test suite sets Lua-version-specific search-path variables
               ;; when available so we must do the same, as these take
               ;; precedence over the generic "LUA_CPATH" and "LUA_PATH"
               (setenv (string-append "LUA_CPATH" env-suffix)
                       (string-append
                        (string-join (map lua-cpath (list out lua-ossl)) ";")
                        ";;"))
               (setenv (string-append "LUA_PATH" env-suffix)
                       (string-append
                        (string-join (map lua-path (list out lua-ossl)) ";")
                        ";;"))

               ;; Skip regression tests we expect to fail
               (with-directory-excursion "regress"
                 (for-each (lambda (f)
                             (rename-file f (string-append f ".skip")))
                           (append
                            ;; Regression tests that require network
                            ;; connectivity
                            '("22-client-dtls.lua"
                              "30-starttls-completion.lua"
                              "62-noname.lua"
                              "153-dns-resolvers.lua")

                            ;; Regression tests that require LuaJIT
                            '("44-resolvers-gc.lua"
                              "51-join-defunct-thread.lua")

                            ;; Regression tests that require Lua 5.3
                            (if (not (equal? lua-version "5.3"))
                                '("152-thread-integer-passing.lua")
                                '()))))

               (apply invoke "make" "check" make-flags)))))))
    (native-inputs
     (list m4))
    (inputs
     (list lua openssl))
    (propagated-inputs
     (list lua-ossl))
    (home-page "https://25thandclement.com/~william/projects/cqueues.html")
    (synopsis "Event loop for Lua using continuation queues")
    (description "The cqueues extension module for Lua implements an event loop
that operates through the yielding and resumption of coroutines.  It is designed
to be non-intrusive, composable, and embeddable within existing applications.")
    (license license:expat)))

(define-public lua-cqueues
  (make-lua-cqueues "lua-cqueues" lua lua-ossl))

(define-public lua5.1-cqueues
  (make-lua-cqueues "lua5.1-cqueues" lua-5.1 lua5.1-ossl))

(define-public lua5.2-cqueues
  (make-lua-cqueues "lua5.2-cqueues" lua-5.2 lua5.2-ossl))

(define-public lua-penlight
  (package
    (name "lua-penlight")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Tieske/Penlight")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qc2d1riyr4b5a0gnsmdw2lz5pw65s4ac60hc34w3mmk9l6yg6nl"))))
    (build-system trivial-build-system)
    (inputs
     (list lua))
    (propagated-inputs
     (list lua-filesystem))
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

(define-public lua-ldoc
  (package
    (name "lua-ldoc")
    (version "1.4.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stevedonovan/LDoc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h0cf7bp4am54r0j8lhjs2l1c7q5vz74ba0jvw9qdbaqimls46g8"))))
    (build-system gnu-build-system)
    (inputs
     (list lua))
    (propagated-inputs
     (list lua-penlight))
    (arguments
     `(#:tests? #f                 ;tests must run after installation.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-installation-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (lua-version ,(version-major+minor (package-version lua))))
               (substitute* "makefile"
                 (("LUA=.*") "#\n")
                 (("(LUA_PREFIX=).*" _ prefix)
                  (string-append prefix out "\n"))
                 (("(LUA_BINDIR=).*" _ prefix)
                  (string-append prefix out "/bin\n"))
                 (("(LUA_SHAREDIR=).*" _ prefix)
                  (string-append prefix out "/share/lua/" lua-version "\n"))))
             #t))
         (delete 'configure)
         (add-before 'install 'create-bin-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
             #t)))))
    (home-page "http://stevedonovan.github.io/ldoc/")
    (synopsis "Lua documentation generator")
    (description
     "LDoc is a LuaDoc-compatible documentation generation system for
Lua source code.  It parses the declaration and documentation comments
in a set of Lua source files and produces a set of XHTML pages
describing the commented declarations and functions.")
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
       (list ,(string-append "CC=" (cc-for-target))
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
     (list dbus ;tests use 'dbus-run-session'
           pkg-config))
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
    (inputs (list lua))
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
                    (url "https://github.com/luvit/luv")
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
     (list lua libuv))
    (native-inputs
     `(("lua-compat"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/keplerproject/lua-compat-5.3")
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
    (inputs (list lua))
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
                    (url "https://github.com/jeremyong/Selene")
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
     (list lua pkg-config))
    (home-page "https://github.com/jeremyong/Selene")
    (synopsis "Lua C++11 bindings")
    (description
     "Selene is a simple C++11 header-only library enabling seamless
 interoperability between C++ and Lua programming language.")
    (license license:zlib)))

(define-public lua-resty-core
  (package
    (name "lua-resty-core")
    (version "0.1.18")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openresty/lua-resty-core")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c58hykwpg5zqbyhrcb703pzwbkih409v3bh2gady6z2kj9q32dw"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((luajit-major+minor ,(version-major+minor (package-version lua)))
                (package-lua-resty (lambda (input output)
                                     (mkdir-p (string-append output "/lib/lua"))
                                     (copy-recursively (string-append input "/lib/resty")
                                                       (string-append output "/lib/lua/resty"))
                                     (copy-recursively (string-append input "/lib/ngx")
                                                       (string-append output "/lib/ngx"))
                                     (symlink (string-append output "/lib/lua/resty")
                                              (string-append output "/lib/resty")))))
           (package-lua-resty (assoc-ref %build-inputs "source")
                              (assoc-ref %outputs "out")))
         #t)))
    (home-page "https://github.com/openresty/lua-resty-core")
    (synopsis "Lua API for NGINX")
    (description "This package provides a FFI-based Lua API for
@code{ngx_http_lua_module} or @code{ngx_stream_lua_module}.")
    (license license:bsd-2)))

(define-public lua-resty-lrucache
  (package
    (name "lua-resty-lrucache")
    (version "0.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openresty/lua-resty-lrucache")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bsc54v1rvxmkwg7a2c01p192lvw5g576f589is8fy1m1c6v4ap8"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((luajit-major+minor ,(version-major+minor (package-version lua)))
                (package-lua-resty (lambda (input output)
                                     (mkdir-p (string-append output "/lib/lua/" luajit-major+minor))
                                     (copy-recursively (string-append input "/lib/resty")
                                                       (string-append output "/lib/lua/" luajit-major+minor  "/resty"))
                                     (symlink (string-append output "/lib/lua/" luajit-major+minor "/resty")
                                              (string-append output "/lib/resty")))))
           (package-lua-resty (assoc-ref %build-inputs "source")
                              (assoc-ref %outputs "out")))
         #t)))
    (home-page "https://github.com/openresty/lua-resty-lrucache")
    (synopsis "Lua LRU cache based on the LuaJIT FFI")
    (description
     "This package provides Lua LRU cache based on the LuaJIT FFI.")
    (license license:bsd-2)))

(define-public lua-resty-signal
  (package
    (name "lua-resty-signal")
    (version "0.02")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openresty/lua-resty-signal")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13y1pqn45y49mhqwywasfdsid46d0c33yi6mrnracbnmvyxz1cif"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;TODO: Run the test suite.
       #:make-flags (list ,(string-append "CC=" (cc-for-target))
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'install-lua
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (use-modules (guix build utils))
             (let* ((luajit-major+minor ,(version-major+minor (package-version lua)))
                    (package-lua-resty (lambda (input output)
                                         (mkdir-p (string-append output "/lib/lua/" luajit-major+minor))
                                         (copy-recursively (string-append input "/lib/resty")
                                                           (string-append output "/lib/lua/" luajit-major+minor  "/resty"))
                                         (symlink (string-append output "/lib/lua/" luajit-major+minor "/resty")
                                                  (string-append output "/lib/resty")))))
               (package-lua-resty (assoc-ref inputs "source")
                                  (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://github.com/openresty/lua-resty-signal")
    (synopsis "Lua library for killing or sending signals to Linux processes")
    (description "This package provides Lua library for killing or sending
signals to Linux processes.")
    (license license:bsd-3)))

(define-public lua-tablepool
  (package
    (name "lua-tablepool")
    (version "0.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openresty/lua-tablepool")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03yjj3w6znvj6843prg84m0lkrn49l901f9hj9bgy3cj9s0awl6y"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((luajit-major+minor ,(version-major+minor (package-version lua)))
                (package-lua-resty (lambda (input output)
                                     (mkdir-p (string-append output "/lib/lua/" luajit-major+minor))
                                     (copy-recursively (string-append input "/lib")
                                                       (string-append output "/lib")))))
           (package-lua-resty (assoc-ref %build-inputs "source")
                              (assoc-ref %outputs "out")))
         #t)))
    (home-page "https://github.com/openresty/lua-tablepool")
    (synopsis "Lua table recycling pools for LuaJIT")
    (description "This package provides Lua table recycling pools for LuaJIT.")
    (license license:bsd-2)))

(define-public lua-resty-shell
  (package
    (name "lua-resty-shell")
    (version "0.03")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openresty/lua-resty-shell")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s6g04ip4hr97r2pd8ry3alq063604s9a3l0hn9nsidh81ps4dp7"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((luajit-major+minor ,(version-major+minor (package-version lua)))
                (package-lua-resty (lambda (input output)
                                     (mkdir-p (string-append output "/lib/lua/" luajit-major+minor))
                                     (copy-recursively (string-append input "/lib/resty")
                                                       (string-append output "/lib/lua/" luajit-major+minor  "/resty"))
                                     (symlink (string-append output "/lib/lua/" luajit-major+minor "/resty")
                                              (string-append output "/lib/resty")))))
           (package-lua-resty (assoc-ref %build-inputs "source")
                              (assoc-ref %outputs "out")))
         #t)))
    (home-page "https://github.com/openresty/lua-resty-shell")
    (synopsis "Lua module for nonblocking system shell command executions")
    (description "This package provides Lua module for nonblocking system
shell command executions.")
    (license license:bsd-3)))

(define-public emilua
  (package
   (name "emilua")
   (version "0.3.2")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://gitlab.com/emilua/emilua.git")
                  (commit (string-append "v" version))
                  ;; Current version requires Trial.Protocol and the HTTP lib
                  ;; developed as part of GSoC 2014 for Boost, and these are
                  ;; dependencies unlikely to be "unbundled" in future releases.
                  (recursive? #t)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1999bgrh52124a5g4qizav3x257ff2brjr855srpm1jv1nxzbygv"))))
   (build-system meson-build-system)
   (arguments
    `(;; Tests are disabled for now due to an issue that affecs guix:
      ;; <https://gitlab.com/emilua/emilua/-/issues/22>
      #:configure-flags
      (list "-Denable_http=true"
            "-Denable_tests=false"
            "-Denable_manpages=false"
            "-Dversion_suffix=-guix1")))
   (native-inputs
    (list luajit-lua52-openresty
          pkg-config
          re2c
          xxd))
   (inputs
    (list boost
          boost-static
          fmt-7
          ;; LuaJIT has a 2GiB addressing limit[1] that has been fixed on OpenResty
          ;; fork. Emilua is severely affected by this limit, so the upstream package
          ;; is avoided. Emilua also depends on the -DLUAJIT_ENABLE_LUA52COMPAT
          ;; configure flag[2] for some features to work (e.g. __pairs on HTTP
          ;; headers).
          ;;
          ;; [1] <http://hacksoflife.blogspot.com/2012/12/integrating-luajit-with-x-plane-64-bit.html>
          ;; [2] <http://luajit.org/extensions.html#lua52>
          luajit-lua52-openresty
          ncurses
          openssl))
   (native-search-paths
    (list
     (search-path-specification
      (variable "EMILUA_PATH")
      (files
       (list (string-append "lib/emilua-" (version-major+minor version)))))))
   (home-page "https://gitlab.com/emilua/emilua")
   (synopsis "Lua execution engine")
   (description
    "Emilua is a LuaJIT-based Lua execution engine that supports async IO,
fibers and actor-inspired threading.  The experimental builtin HTTP module is
enabled.")
   (license license:boost1.0)))

(define-public fennel
  ;; The 1.0.0 release had a bug where fennel installed under 5.4 no matter
  ;; what lua was used to compile it. There has since been an update that
  ;; corrects this issue, so we can rely on the version of the lua input to
  ;; determine where the fennel.lua file got installed to.
  (let ((commit "03c1c95f2a79e45a9baf607f96a74c693b8b70f4")
        (revision "0"))
    (package
      (name "fennel")
      (version (git-version "1.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.sr.ht/~technomancy/fennel")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1znp38h5q819gvcyl248zwvjsljfxdxdk8n82fnj6lyibiiqzgvx"))))
      (build-system gnu-build-system)
      (arguments
       '(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:tests? #t      ; even on cross-build
         #:test-target "test"
         #:phases
         (modify-phases %standard-phases
          (delete 'configure)
          (add-after 'build 'patch-fennel
           (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "fennel"
             (("/usr/bin/env .*lua")
              (search-input-file inputs "/bin/lua")))))
          (delete 'check)
          (add-after 'install 'check
           (assoc-ref %standard-phases 'check)))))
      (inputs (list lua))
      (home-page "https://fennel-lang.org/")
      (synopsis "Lisp that compiles to Lua")
      (description
       "Fennel is a programming language that brings together the speed,
simplicity, and reach of Lua with the flexibility of a Lisp syntax and macro
system.")
      (license license:expat))))

(define-public fnlfmt
  (package
    (name "fnlfmt")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~technomancy/fnlfmt")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rv0amqhy5ypi3pvxfaadn3k1cy4mjlc49wdzl2psz3i11w9gr36"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   ;; Use input fennel instead of bundled fennel.
                   (delete-file "fennel")
                   (delete-file "fennel.lua")
                   (substitute* "Makefile"
                     (("./fennel") "fennel"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'patch-makefile
         (lambda* (#:key native-inputs inputs #:allow-other-keys)
          (substitute* "Makefile"
           ;; Patch lua shebang that gets inserted to fnlfmt.
           (("/usr/bin/env lua")
            (search-input-file (or native-inputs inputs) "/bin/lua")))))
        (replace 'install
         ;; There is no install target; manually install the output file.
         (lambda* (#:key outputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (bin (string-append out "/bin")))
           (for-each (lambda (file)
                      (install-file file bin))
            (find-files "." "fnlfmt")))))
        (add-after 'install 'wrap
         (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
          (let* ((all-inputs (or native-inputs inputs))
                 (fnlfmt (assoc-ref outputs "out"))
                 (lua-version ,(version-major+minor (package-version lua)))
                 (fennel (assoc-ref all-inputs "fennel")))
           (wrap-program (string-append fnlfmt "/bin/fnlfmt")
            `("LUA_PATH" ";" suffix
              (,(format #f "~a/share/lua/~a/?.lua" fennel lua-version))))
           #t))))))
    (inputs (list bash-minimal))
    (native-inputs (list lua fennel))
    (home-page "https://git.sr.ht/~technomancy/fnlfmt")
    (synopsis "Automatic formatting of Fennel code")
    (description
     "Fnlfmt is a tool for automatically formatting Fennel code in a consistent
way, following established lisp conventions.")
    (license license:lgpl3+)))
