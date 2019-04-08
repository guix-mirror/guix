;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Corentin Bocquillon <corentin@nybble.fr>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
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

(define-module (gnu packages serialization)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages perl))

(define-public cereal
  (package
    (name "cereal")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/USCiLab/cereal/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kj32h3j2128anig0g9gzw82kfyd5xqfkwq6vdyv900jx8i1qckx"))))
    (build-system cmake-build-system)
    (arguments
     `(;; The only included tests are portability tests requiring
       ;; cross-compilation and boost.  Since we are building cereal on more
       ;; platforms anyway, there is no compelling reason to build the tests.
       #:tests? #f
       #:out-of-source? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
          (lambda _
            (substitute* "doc/doxygen.in"
              (("@CMAKE_CURRENT_SOURCE_DIR@") "."))
            (invoke "doxygen" "doc/doxygen.in")
            #t))
         ;; There is no "install" target, so we have to provide our own
         ;; "install" phase.
         (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out     (assoc-ref outputs "out"))
                   (doc     (string-append out "/share/cereal/docs"))
                   (include (string-append out "/include/cereal")))
              (mkdir-p doc)
              (mkdir-p include)
              (copy-recursively "include/cereal" include)
              (copy-recursively "doc/html" doc))
            #t)))))
    (native-inputs
     `(("doxygen" ,doxygen)))
    (home-page "http://uscilab.github.io/cereal/")
    (synopsis "C++11 library for serialization")
    (description
     "Cereal is a header-only C++11 serialization library.  Cereal takes
arbitrary data types and reversibly turns them into different representations,
such as compact binary encodings, XML, or JSON.")
    (license license:bsd-3)))

(define-public msgpack
  (package
    (name "msgpack")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/msgpack/msgpack-c/releases/download/"
         "cpp-" version "/msgpack-" version ".tar.gz"))
       (snippet
        '(let ((p (open-file "msgpack.pc.in" "a")))
           (display
            (string-append "Requires: " "zlib" "\n") p)
           (close-output-port p)
           #t))
       (sha256
        (base32
         "18hzmyfg3mvnp7ab03nqdzzvqagkl42gygjpi4zv4i7aca2dmwf0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("googletest" ,googletest)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("zlib" ,zlib))) ;; Msgpack installs two headers (zbuffer.h,
    ;; zbuffer.hpp) which #include <zlib.h>.  However, 'guix gc --references'
    ;; does not detect a store reference to zlib since these headers are not
    ;; compiled.
    (home-page "https://www.msgpack.org")
    (synopsis "Binary serialization library")
    (description "Msgpack is a library for C/C++ that implements binary
serialization.")
    (license license:boost1.0)))

(define-public libmpack
  (package
    (name "libmpack")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/tarruda/libmpack/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ml922gv8y99lbldqb9ykpjndla0hlprdjyl79yskkhwv2ai7sac"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("libtool" ,libtool)))
    (home-page "https://github.com/tarruda/libmpack")
    (synopsis "Small binary serialization library")
    (description "Libmpack is a small binary serialization and RPC library
that implements both the msgpack and msgpack-rpc specifications.")
    (license license:expat)))

(define-public lua-libmpack
  (package (inherit libmpack)
    (name "lua-libmpack")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libmpack/libmpack-lua/"
                                  "archive/" (package-version libmpack) ".tar.gz"))
              (file-name (string-append name "-" (package-version libmpack) ".tar.gz"))
              (sha256
               (base32
                "153zrrbyxhf71dgzjjhrk56rfwk3nisslpgcqyg44v8fnz1xpk6i"))))
    (build-system gnu-build-system)
    (arguments
     `(;; FIXME: tests require "busted", which is not yet available in Guix.
       #:tests? #f
       #:test-target "test"
       #:make-flags
       (let* ((lua-version ,(package-version lua))
              (lua-major+minor ,(version-major+minor (package-version lua))))
         (list "CC=gcc"
               "FETCH=echo"  ; don't fetch anything from the web
               "UNTGZ=echo"  ; and don't try to unpack it
               "USE_SYSTEM_LUA=yes"
               (string-append "MPACK_LUA_VERSION=" lua-version)
               (string-append "MPACK_LUA_VERSION_NOPATCH=" lua-major+minor)
               (string-append "PREFIX="
                              (assoc-ref %outputs "out"))
               (string-append "LUA_CMOD_INSTALLDIR="
                              (assoc-ref %outputs "out")
                              "/lib/lua/" lua-major+minor)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'unpack-mpack-sources
           (lambda* (#:key inputs #:allow-other-keys)
             ;; This is broken because mpack-src is not a file, but all
             ;; prerequisites are added to the inputs of the gcc invocation.
             (substitute* "Makefile"
               (("\\$\\(MPACK\\): mpack-src") "$(MPACK): "))
             (mkdir-p "mpack-src")
             (zero? (system* "tar" "-C" "mpack-src"
                             "--strip-components=1"
                             "-xvf" (assoc-ref inputs "libmpack"))))))))
    (inputs
     `(("lua" ,lua)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libmpack" ,(package-source libmpack))))
    (home-page "https://github.com/libmpack/libmpack-lua")
    (synopsis "Lua bindings for the libmpack binary serialization library")))

(define-public lua5.1-libmpack
  (package (inherit lua-libmpack)
    (name "lua5.1-libmpack")
    (arguments
     (substitute-keyword-arguments (package-arguments lua-libmpack)
       ((#:make-flags flags)
        `(let* ((lua-version ,(package-version lua-5.1))
                (lua-major+minor ,(version-major+minor (package-version lua-5.1))))
           (list "CC=gcc"
                 "USE_SYSTEM_LUA=yes"
                 (string-append "MPACK_LUA_VERSION=" lua-version)
                 (string-append "MPACK_LUA_VERSION_NOPATCH=" lua-major+minor)
                 (string-append "PREFIX="
                                (assoc-ref %outputs "out"))
                 (string-append "LUA_CMOD_INSTALLDIR="
                                (assoc-ref %outputs "out")
                                "/lib/lua/" lua-major+minor))))))
    (inputs
     `(("lua" ,lua-5.1)))))

(define-public lua5.2-libmpack
  (package (inherit lua-libmpack)
    (name "lua5.2-libmpack")
    (arguments
     (substitute-keyword-arguments (package-arguments lua-libmpack)
       ((#:make-flags flags)
        `(let* ((lua-version ,(package-version lua-5.2))
                (lua-major+minor ,(version-major+minor (package-version lua-5.2))))
           (list "CC=gcc"
                 "USE_SYSTEM_LUA=yes"
                 (string-append "MPACK_LUA_VERSION=" lua-version)
                 (string-append "MPACK_LUA_VERSION_NOPATCH=" lua-major+minor)
                 (string-append "PREFIX="
                                (assoc-ref %outputs "out"))
                 (string-append "LUA_CMOD_INSTALLDIR="
                                (assoc-ref %outputs "out")
                                "/lib/lua/" lua-major+minor))))))
    (inputs
     `(("lua" ,lua-5.2)))))

(define-public yaml-cpp
  (package
    (name "yaml-cpp")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/jbeder/yaml-cpp/archive/"
                    "yaml-cpp-" version ".tar.gz"))
              (sha256
               (base32
                "01gxn7kc8pzyh4aadjxxzq8cignmbwmm9rfrsmgqfg9w2q75dn74"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'dont-install-gtest-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion
                 (string-append out "/include")
                 (delete-file-recursively "gtest")
                 (delete-file-recursively "gmock"))
               (with-directory-excursion
                 (string-append out "/lib")
                 (for-each (lambda (file)
                             (delete-file file))
                           '("libgmock.so" "libgmock_main.so"
                             "libgtest.so" "libgtest_main.so"))))
             #t)))))
    (native-inputs
     `(("python" ,python)))
    (home-page "https://github.com/jbeder/yaml-cpp")
    (synopsis "YAML parser and emitter in C++")
    (description "YAML parser and emitter in C++ matching the YAML 1.2 spec.")
    (license license:bsd-3)))

(define-public jsoncpp
  (package
    (name "jsoncpp")
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/open-source-parsers/jsoncpp/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dpxk8hkni5dq4mdw8qbaj40jmid3a31d1gh8iqcnfwkw34ym7f4"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/open-source-parsers/jsoncpp")
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS:BOOL=YES")))
    (synopsis "C++ library for interacting with JSON")
    (description "JsonCpp is a C++ library that allows manipulating JSON values,
including serialization and deserialization to and from strings.  It can also
preserve existing comment in unserialization/serialization steps, making
it a convenient format to store user input files.")
    (license license:expat)))

;; Tensorflow does not build with jsoncpp 1.8.x.  It is built with commit
;; 4356d9bba191e1e16ce7a92073cbf3e63564e973, which lies between version 1.7.2
;; and 1.7.3.
(define-public jsoncpp-for-tensorflow
  (package (inherit jsoncpp)
    (name "jsoncpp")
    (version "1.7.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/open-source-parsers/jsoncpp.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1180ln8blrb0mwzpcf78k49hlki6di65q77rsvglf83kfcyh4d7z"))))))

(define-public capnproto
  (package
    (name "capnproto")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://capnproto.org/capnproto-c++-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0hfdnhlbskagzgvby8wy6lrxj53zfzpfqimbhga68c0ji2yw1969"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'do-not-require-/etc/services
           (lambda _
             ;; Workaround for test that tries to resolve port name from
             ;; /etc/services, which is not present in build environment.
             (substitute* "src/kj/async-io-test.c++" ((":http") ":80"))
             #t))
         (add-before 'check 'use-tmp-for-tempory-files
           (lambda _
             ;; Use /tmp for tempory files, as the default /var/tmp directory
             ;; doesn't exist.
             (substitute* "src/kj/filesystem-disk-test.c++"
               (("VAR\\_TMP \"/var/tmp\"")
                "VAR_TMP \"/tmp\""))
             #t)))))
    (home-page "https://capnproto.org")
    (synopsis "Capability-based RPC and serialization system")
    (description
     "Cap'n Proto is a very fast data interchange format and capability-based
RPC system.  Think JSON, except binary.  Or think Protocol Buffers, except faster.")
    (license license:expat)))

(define-public libbson
  (package
    (name "libbson")
    (version "1.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/mongodb/libbson/releases/"
                             "download/" version "/libbson-" version ".tar.gz"))
        (sha256
         (base32
          "1fj4554msq0rrz14snbj908dzqj46gh7jg9w9j0akn2b7q911m5a"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))
    (home-page "http://mongoc.org/libbson/current/index.html")
    (synopsis "C BSON library")
    (description "Libbson can create and parse BSON documents.  It can also
convert JSON documents to BSON and the opposite.  BSON stands for Binary JSON,
it is comparable to protobuf.")
    (license license:asl2.0)))

(define-public nlohmann-json-cpp
  (package
    (name "nlohmann-json-cpp")
    (version "2.1.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/nlohmann/json/"
                          "archive/v" version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "0lrh6cjd643c7kmvmwafbgq7dqj3b778483gjhjbvp6rc6z5xf2r"))))
    (build-system cmake-build-system)
    (home-page "https://nlohmann.github.io/json/")
    (synopsis "JSON library for C++")
    (description
     "JSON library for C++ trying to accomplish “Intuitive syntax”,
“Trivial integration”, and “Serious testing”.
However, “Memory efficiency” and “Speed” have not been primary goals.")
    (license license:expat)))

(define-public python-ruamel.yaml
  (package
    (name "python-ruamel.yaml")
    (version "0.15.83")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ruamel.yaml" version))
       (sha256
        (base32
         "0p4i8ad28cbbbjja8b9274irkhnphhvhap3aym6yb8xfp1d72kpw"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (arguments
     `(;; TODO: Tests require packaging "ruamel.std.pathlib".
       #:tests? #f))
    (home-page "https://bitbucket.org/ruamel/yaml")
    (synopsis "YAML 1.2 parser/emitter")
    (description
     "This package provides YAML parser/emitter that supports roundtrip
preservation of comments, seq/map flow style, and map key order.  It
is a derivative of Kirill Simonov’s PyYAML 3.11.  It supports YAML 1.2
and has round-trip loaders and dumpers.  It supports comments.  Block
style and key ordering are kept, so you can diff the source.")
    (license license:expat)))

(define-public python2-ruamel.yaml
  (package-with-python2 python-ruamel.yaml))

(define-public python-cbor
  (package
    (name "python-cbor")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cbor" version))
       (sha256
        (base32
         "1dmv163cnslyqccrybkxn0c9s1jk1mmafmgxv75iamnz5lk5l8hk"))))
    (build-system python-build-system)
    (home-page "https://bitbucket.org/bodhisnarkva/cbor")
    (synopsis "Implementation of the Concise Binary Object Representation")
    (description
     "Python-cbor provides an implementation of the Concise Binary Object
Representation (@dfn{CBOR}).  CBOR is comparable to JSON, has a superset of
JSON's ability, but serializes to a binary format which is smaller and faster
to generate and parse.  The two primary functions are @code{cbor.loads} and
@code{cbor.dumps}.")
    (license license:asl2.0)))

(define-public flatbuffers
  (package
    (name "flatbuffers")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/google/flatbuffers/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0z4swldxs0s31hnkqdhsbfmc8vx3p7zsvmqaw4l31r2iikdy651p"))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "Release"
       #:configure-flags
       (list (string-append "-DCMAKE_INSTALL_LIBDIR="
                            (assoc-ref %outputs "out") "/lib"))))
    (home-page "https://google.github.io/flatbuffers/")
    (synopsis "Memory-efficient serialization library")
    (description "FlatBuffers is a cross-platform serialization library for C++,
C#, C, Go, Java, JavaScript, PHP, and Python.  It was originally created for
game development and other performance-critical applications.")
    (license license:asl2.0)))

(define-public python-feather-format
  (package
    (name "python-feather-format")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "feather-format" version))
        (sha256
         (base32
          "1adivm5w5ji4qv7hq7942vqlk8l2wgw87bdlsia771z14z3zp857"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pandas" ,python-pandas)
       ("python-pyarrow" ,python-pyarrow)))
    (home-page "https://github.com/wesm/feather")
    (synopsis "Python wrapper to the Feather file format")
    (description "This package provides a Python wrapper library to the
Apache Arrow-based Feather binary columnar serialization data frame format.")
    (license license:asl2.0)))

(define-public python2-feather-format
  (package-with-python2 python-feather-format))
