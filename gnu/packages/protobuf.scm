;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Daniel Pimentel <d4n1@d4n1.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
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

(define-module (gnu packages protobuf)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system ruby)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby))

(define-public fstrm
  (package
    (name "fstrm")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.farsightsecurity.com/dist/" name "/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "1i9y8a1712aj80p5a1kcp378bnjrg3s2127q7304hklhmjcrjl1d"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libevent))
    (home-page "https://github.com/farsightsec/fstrm")
    (synopsis "Implementation of the Frame Streams data transport protocol")
    (description
     "fstrm is an optimised implementation of Frame Streams as a C library and
several tools built on top of it.

@dfn{Frame Streams} is a light-weight, binary-clean protocol that allows for
the transport of arbitrarily-encoded data payload sequences with minimal
framing overhead---just four bytes per data frame.  It does not specify an
encoding format for these data frames and can be used with any data
serialisation format that produces byte sequences, such as Protocol Buffers,
XML, JSON, MessagePack, YAML, etc.

Frame Streams can be used either as a streaming transport over a reliable byte
stream socket (TCP sockets, TLS connections, @code{AF_UNIX} sockets, etc.) for
data in motion, or as a file format for data at rest.")
    (license (list license:asl2.0
                   (license:non-copyleft #f "See libmy/argv*")))))

(define-public protobuf
  (package
    (name "protobuf")
    (version "3.17.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/protobuf/releases/"
                                  "download/v" version "/protobuf-cpp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1jzqrklhj9grs6xbddyb5dyxfbgbgbyhl5zig8ml50wb22gwkkji"))))
    (build-system gnu-build-system)
    (inputs (list zlib))
    (outputs (list "out"
                   "static"))           ; ~12 MiB of .a files
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-broken-tests
           ;; The following tests fail on 32 bit architectures such as
           ;; i686-linux.
           (lambda _
             (let-syntax ((disable-tests
                           (syntax-rules ()
                             ((_ file test ...)
                              (substitute* file
                                ((test name)
                                 (string-append "DISABLED_" name)) ...)))))
               ;; See: https://github.com/protocolbuffers/protobuf/issues/8460.
               (disable-tests "src/google/protobuf/any_test.cc"
                              "TestPackFromSerializationExceedsSizeLimit")
               ;; See: https://github.com/protocolbuffers/protobuf/issues/8459.
               (disable-tests "src/google/protobuf/arena_unittest.cc"
                              "SpaceAllocated_and_Used"
                              "BlockSizeSmallerThanAllocation")
               ;; See: https://github.com/protocolbuffers/protobuf/issues/8082.
               (disable-tests "src/google/protobuf/io/zero_copy_stream_unittest.cc"
                              "LargeOutput"))))
         (add-after 'install 'move-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move static libraries to the "static" output.
             (let* ((out    (assoc-ref outputs "out"))
                    (lib    (string-append out "/lib"))
                    (static (assoc-ref outputs "static"))
                    (slib   (string-append static "/lib")))
               (mkdir-p slib)
               (for-each (lambda (file)
                           (install-file file slib)
                           (delete-file file))
                         (find-files lib "\\.a$"))))))))
    (home-page "https://github.com/google/protobuf")
    (synopsis "Data encoding for remote procedure calls (RPCs)")
    (description
     "Protocol Buffers are a way of encoding structured data in an efficient
yet extensible format.  Google uses Protocol Buffers for almost all of its
internal RPC protocols and file formats.")
    (license license:bsd-3)))

;; Tensorflow requires version 3.6 specifically.
(define-public protobuf-3.6
  (package
    (inherit protobuf)
    (version "3.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/protobuf/releases/"
                                  "download/v" version "/protobuf-cpp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0a955bz59ihrb5wg7dwi12xajdi5pmz4bl0g147rbdwv393jwwxk"))))))

;; The 3.5 series are the last versions that do not require C++ 11.
(define-public protobuf-3.5
  (package
    (inherit protobuf)
   (version "3.5.1")
   (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/protobuf/releases/"
                                  "download/v" version "/protobuf-cpp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14j0427ykjzrd9a66c2mpk0sjcccjlsx6q8ww6hzwb6sha3vm3f2"))))))

;; XXX Remove this old version when no other packages depend on it.
(define-public protobuf-2
  (package (inherit protobuf)
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/protobuf/releases/"
                                  "download/v" version "/protobuf-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "040rcs9fpv4bslhiy43v7dcrzakz4vwwpyqg4jp8bn24sl95ci7f"))))))

(define-public protobuf-c
  (package
    (name "protobuf-c")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/protobuf-c/protobuf-c/"
                                  "releases/download/v" version
                                  "/protobuf-c-" version ".tar.gz"))
              (sha256
               (base32
                "0y3yaanq97si7iyld06p8w20m0shpj7sf4xwzbhhvijhxw36d592"))))
    (build-system gnu-build-system)
    (inputs (list protobuf))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/protobuf-c/protobuf-c")
    (synopsis "Protocol Buffers implementation in C")
    (description
     "This is protobuf-c, a C implementation of the Google Protocol Buffers
data serialization format.  It includes @code{libprotobuf-c}, a pure C library
that implements protobuf encoding and decoding, and @code{protoc-c}, a code
generator that converts Protocol Buffer @code{.proto} files to C descriptor
code.")
    (license license:bsd-2)))

(define-public protozero
  (package
    (name "protozero")
    (version "1.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mapbox/protozero")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "052cq5mdjjgcsgk612zkqi8k08p3ikl22r59dk6i6fq41dxldja7"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/mapbox/protozero")
    (synopsis "Minimalistic protocol buffer decoder and encoder in C++")
    (description "Protozero is a minimalistic protocol buffer decoder and
encoder in C++.  The developer using protozero has to manually translate the
@file{.proto} description into code.")
    (license (list
              license:asl2.0            ; for folly
              license:bsd-2))))

(define-public python-protobuf
  (package
    (name "python-protobuf")
    (version "3.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "protobuf" version))
       (sha256
        (base32
         "0mj6z58aiw532s1mq48m9xdrm3gdyp2vv9cdinfb5wmnfpm5m7n9"))))
    (build-system python-build-system)
    (native-inputs
     (list python-wheel))
    (propagated-inputs
     (list python-six))
    (home-page "https://github.com/google/protobuf")
    (synopsis "Protocol buffers is a data interchange format")
    (description
     "Protocol buffers are a language-neutral, platform-neutral extensible
mechanism for serializing structured data.")
    (license license:bsd-3)))

(define-public python-pure-protobuf
  (package
    (name "python-pure-protobuf")
    (version "2.0.1")
    (source
     (origin
       ;; The PyPI tarball is broken: it has no tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eigenein/protobuf")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15dp5pvazd0jx4wzzh79080ah7hkpd3axh40al9vhzs2hf3v90hx"))))
    (build-system python-build-system)
    (native-inputs
     (list python-flake8 python-pytest python-pytest-cov python-isort))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "--cov-report" "term-missing" "--cov"
                     "pure_protobuf")
             (invoke "flake8" "pure_protobuf" "tests"
                     "--ignore=F541")
             (invoke "isort" "-rc" "-c" "pure_protobuf" "tests"))))))
    (home-page "https://pypi.org/project/pure-protobuf/")
    (synopsis "Protobuf implementation using dataclasses")
    (description
     "@code{python-pure-protobuf} takes advantage of the standard
dataclasses module to define message types.  Protocol buffers are a
language-neutral, platform-neutral extensible mechanism for serializing
structured data.")
    (license license:expat)))

(define-public python2-protobuf
  (package-with-python2 python-protobuf))

;; For tensorflow.
(define-public python-protobuf-3.6
  (package
    (inherit python-protobuf)
    (name "python-protobuf")
    (version (package-version protobuf-3.6) )
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "protobuf" version))
       (sha256
        (base32
         "04bqb12smlckzmgkj6vgmpbr3cby0n6726cmz33bqr7kn1vb728l"))))))

(define-public emacs-protobuf-mode
  (package
    (name "emacs-protobuf-mode")
    (version (package-version protobuf))
    (source (package-source protobuf))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'expand-load-path 'change-working-directory
           (lambda _ (chdir "editors") #t)))))
    (home-page "https://github.com/protocolbuffers/protobuf")
    (synopsis "Protocol buffers major mode for Emacs")
    (description
     "This package provides an Emacs major mode for editing Protocol Buffer
source files.")
    (license license:bsd-3)))

(define-public ruby-protobuf
  (package
    (name "ruby-protobuf")
    (version "3.10.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ruby-protobuf/protobuf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yzz7jgpp6qip5d6qhzbkf5gqaydfk3z3c1ngccwzp6w6wa75g8a"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-use-bundler-for-tests
           (lambda _
             (substitute* "spec/spec_helper.rb"
               (("Bundler\\.setup.*") ""))
             #t))
         (add-after 'unpack 'relax-version-requirements
           (lambda _
             (substitute* ((@@ (guix build ruby-build-system) first-gemspec))
               (("'rake',.*")
                "'rake'\n")
               (("\"rubocop\",.*")
                "'rubocop'\n")
               (("\"parser\",.*")
                "'parser'\n"))
             #t))
         (add-after 'unpack 'patch-protoc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((protoc (assoc-ref inputs "protobuf")))
               (substitute* "lib/protobuf/tasks/compile.rake"
                 (("\"protoc\"")
                  (string-append "\"" protoc "/bin/protoc" "\"")))
               #t)))
         (add-after 'unpack 'skip-failing-test
           ;; See: https://github.com/ruby-protobuf/protobuf/issues/419
           (lambda _
             (substitute* "spec/lib/protobuf/rpc/connectors/ping_spec.rb"
               (("expect\\(::IO\\)\\.to receive\\(:select\\).*" all)
                (string-append "        pending\n" all)))
             #t))
         (add-after 'replace-git-ls-files 'replace-more-git-ls-files
           (lambda _
             (substitute* ((@@ (guix build ruby-build-system) first-gemspec))
               (("`git ls-files -- \\{test,spec,features\\}/*`")
                "`find test spec features -type f | sort`")
               (("`git ls-files -- bin/*`")
                "`find bin -type f | sort`"))
             #t))
         (replace 'check
           (lambda _
             (invoke "rspec"))))))
    (native-inputs
     (list ruby-benchmark-ips
           ruby-ffi-rzmq
           ruby-parser
           ruby-pry-byebug
           ruby-pry-stack-explorer
           ruby-rake
           ruby-rspec
           ruby-rubocop
           ruby-ruby-prof
           ruby-simplecov
           ruby-timecop
           ruby-varint
           ruby-yard))
    (inputs
     (list protobuf))
    (propagated-inputs
     (list ruby-activesupport ruby-middleware ruby-thor ruby-thread-safe))
    (home-page "https://github.com/ruby-protobuf/protobuf")
    (synopsis "Implementation of Google's Protocol Buffers in Ruby")
    (description "Protobuf is an implementation of Google's Protocol Buffers
in pure Ruby.")
    (license license:expat)))

;;; This is a modified ruby-protobuf package used by ruby-cucumber-messages
;;; until https://github.com/ruby-protobuf/protobuf/pull/411 and
;;; https://github.com/ruby-protobuf/protobuf/pull/415 are merged upstream.
(define-public ruby-protobuf-cucumber
  (hidden-package
   (package
     (inherit ruby-protobuf)
     (name "ruby-protobuf-cucumber")
     (version "3.10.8")
     (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "protobuf-cucumber" version))
        (sha256
         (base32
          "1rd6naabhpfb1i5dr6fp5mqwaawsx0mqm73h5ycwkgbm1n2si872")))))))
