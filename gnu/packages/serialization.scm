;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Corentin Bocquillon <corentin@nybble.fr>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2020 Alexandros Theodotou <alex@zrythm.org>
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
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages perl))

(define-public avro-cpp-1.9
  (package
    (name "avro-cpp")
    (version "1.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://archive.apache.org/dist/avro/avro-" version
                    "/avro-src-" version ".tar.gz"))
              (sha256
               (base32 "0i3fpm7r72yw397qc8yw9ybzk2mxjkv0yk5hnn00ylc1wbd0np73"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "lang/c++"))))))
    (inputs
     `(("boost" ,boost)
       ("snappy" ,snappy)))
    (home-page "https://avro.apache.org/")
    (synopsis "Data serialization system")
    (description "Apache Avro is a data serialization system.  Avro provides:
@enumerate
@item Rich data structures;
@item a compact, fast, binary data format;
@item a container file, to store persistent data;
@item remote procedure call (RPC); and
@item simple integration with dynamic languages.
@end enumerate

Code generation is not required to read or write data files nor to use or
implement RPC protocols.")
    (license license:asl2.0)))

(define-public avro-cpp-1.9-for-irods
  (package
    (inherit avro-cpp-1.9)
    (properties `((hidden? . #true)))
    (arguments
     `(#:configure-flags
       '("-DCMAKE_CXX_COMPILER=clang++"
         "-DCMAKE_CXX_FLAGS=-stdlib=libc++"
         "-DCMAKE_EXE_LINKER_FLAGS=-lc++abi -lz")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "lang/c++")))
         (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs  "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (cons* (string-append (assoc-ref inputs "libcxx+libcxxabi")
                                              "/include/c++/v1")
                               ;; Hide GCC's C++ headers so that they do not interfere with
                               ;; the Clang headers.
                               (delete (string-append gcc "/include/c++")
                                       (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                     #\:)))
                        ":"))
               (format #true
                       "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                       (getenv "CPLUS_INCLUDE_PATH"))))))))
    (inputs
     `(("boost" ,boost-for-irods)
       ("clang" ,clang-toolchain-6)
       ("libcxx+libcxxabi" ,libcxx+libcxxabi-6)
       ("libcxxabi" ,libcxxabi-6)
       ("snappy" ,snappy-with-clang6)
       ("zlib" ,zlib)))))

(define-public cereal
  (package
    (name "cereal")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/USCiLab/cereal")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hc8wh9dwpc1w1zf5lfss4vg5hmgpblqxbrpp1rggicpx9ar831p"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DSKIP_PORTABILITY_TEST=ON")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'skip-sandbox
           (lambda _
             (substitute* "CMakeLists.txt"
               (("add_subdirectory\\(sandbox\\)") ""))))
         (add-after 'install 'install-doc
           (lambda _
             (let ((doc (string-append %output "/share/doc/html")))
               (invoke "make" "doc")
               (mkdir-p doc)
               (copy-recursively "doc/html" doc)))))))
    (native-inputs
     `(("doxygen" ,doxygen)))
    (home-page "https://uscilab.github.io/cereal/")
    (synopsis "C++11 library for serialization")
    (description
     "Cereal is a header-only C++11 serialization library.  Cereal takes
arbitrary data types and reversibly turns them into different representations,
such as compact binary encodings, XML, or JSON.")
    (license license:bsd-3)))

(define-public msgpack
  (package
    (name "msgpack")
    (version "3.3.0")
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
        (base32 "0yzhq50ijvwrfkr97knhvn54lj3f4hr3zy39yq8wpf6xll94s4bf"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("googletest" ,googletest-1.8)
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tarruda/libmpack")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rai5djdkjz7bsn025k5489in7r1amagw1pib0z4qns6b52kiar2"))))
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
    (version "1.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libmpack/libmpack-lua")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ijvzgq5hvib03w5rghv31wi7byamwg7qdx5pawvhvnflaii8ivw"))))
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
             (copy-recursively (assoc-ref inputs "libmpack")
                               "mpack-src")
             #t)))))
    (inputs
     `(("lua" ,lua)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libmpack" ,(package-source libmpack))))
    (home-page "https://github.com/libmpack/libmpack-lua")
    (synopsis "Lua bindings for the libmpack binary serialization library")))

(define-public lua5.1-libmpack
  (package/inherit lua-libmpack
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
  (package/inherit lua-libmpack
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

(define-public libyaml
  (package
    (name "libyaml")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pyyaml.org/download/libyaml/yaml-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1x4fcw13r3lqy8ndydr3ili87wicplw2awbcv6r21qgyfndswhn6"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (home-page "https://pyyaml.org/wiki/LibYAML")
    (synopsis "YAML 1.1 parser and emitter written in C")
    (description
     "LibYAML is a YAML 1.1 parser and emitter written in C.")
    (license license:expat)))

(define-public libyaml+static
  (package
    (inherit libyaml)
    (name "libyaml+static")
    (arguments
     '(#:configure-flags '("--enable-static")))))

(define-public libcyaml
  (package
    (name "libcyaml")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tlsa/libcyaml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "libcyaml-libyaml-compat.patch"))
       (sha256
        (base32 "0428p0rwq71nhh5nzcbapsbrjxa0x5l6h6ns32nxv7j624f0zd93"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "CC=gcc"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'check
           (lambda _
             (setenv "CC" "gcc")
             (invoke "make" "test"))))))
    (inputs
     `(("libyaml" ,libyaml)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "C library for reading and writing YAML")
    (description
     "LibCYAML is a C library written in ISO C11 for reading and writing
structured YAML documents.  The fundamental idea behind CYAML is to allow
applications to construct schemas which describe both the permissible
structure of the YAML documents to read/write, and the C data structure(s)
in which the loaded data is arranged in memory.")
    (home-page "https://github.com/tlsa/libcyaml")
    (license license:isc)))

(define-public yaml-cpp
  (package
    (name "yaml-cpp")
    (version "0.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jbeder/yaml-cpp")
             (commit (string-append "yaml-cpp-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ykkxzxcwwiv8l8r697gyqh1nl582krpvi7m7l6b40ijnk4pw30s"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DYAML_BUILD_SHARED_LIBS=ON")))
    (native-inputs
     `(("python" ,python)))
    (home-page "https://github.com/jbeder/yaml-cpp")
    (synopsis "YAML parser and emitter in C++")
    (description "YAML parser and emitter in C++ matching the YAML 1.2 spec.")
    (license license:bsd-3)))

(define-public jsoncpp
  (package
    (name "jsoncpp")
    (version "1.9.2")
    (home-page "https://github.com/open-source-parsers/jsoncpp")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (patches (search-patches "jsoncpp-fix-inverted-case.patch"))
              (sha256
               (base32
                "037d1b1qdmn3rksmn1j71j26bv4hkjv7sn7da261k853xb5899sg"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS:BOOL=YES"
                           ,@(if (%current-target-system)
                                 `("-DJSONCPP_WITH_POST_BUILD_UNITTEST=OFF")
                                 '()))
       ,@(if (%current-target-system)
             '()
             `(#:cmake ,cmake-bootstrap))))
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
                    (url "https://github.com/open-source-parsers/jsoncpp")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1180ln8blrb0mwzpcf78k49hlki6di65q77rsvglf83kfcyh4d7z"))))))

(define-public capnproto
  (package
    (name "capnproto")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://capnproto.org/capnproto-c++-"
                    version ".tar.gz"))
              (sha256
               (base32
                "03f1862ljdshg7d0rg3j7jzgm3ip55kzd2y91q7p0racax3hxx6i"))))
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
         (add-before 'check 'use-tmp-for-temporary-files
           (lambda _
             ;; Use /tmp for temporary files, as the default /var/tmp directory
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
    (version "2.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/google/flatbuffers")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1zbf6bdpps8369r1ql00irxrp58jnalycc8jcapb8iqg654vlfz8"))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "Release"
       #:configure-flags
       (list "-DFLATBUFFERS_BUILD_SHAREDLIB=ON"
             (string-append "-DCMAKE_INSTALL_LIBDIR="
                            (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _ (for-each make-file-writable (find-files ".")))))))
    (home-page "https://google.github.io/flatbuffers/")
    (synopsis "Memory-efficient serialization library")
    (description "FlatBuffers is a cross-platform serialization library for C++,
C#, C, Go, Java, JavaScript, PHP, and Python.  It was originally created for
game development and other performance-critical applications.")
    (license license:asl2.0)))

(define-public python-feather-format
  (package
    (name "python-feather-format")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "feather-format" version))
        (sha256
         (base32
          "00w9hwz7sj3fkdjc378r066vdy6lpxmn6vfac3qx956k8lvpxxj5"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pandas" ,python-pandas)
       ("python-pyarrow" ,python-pyarrow)))
    (home-page "https://github.com/wesm/feather")
    (synopsis "Python wrapper to the Feather file format")
    (description "This package provides a Python wrapper library to the
Apache Arrow-based Feather binary columnar serialization data frame format.")
    (license license:asl2.0)))
