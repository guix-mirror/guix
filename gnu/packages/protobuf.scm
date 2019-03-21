;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Daniel Pimentel <d4n1@d4n1.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

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
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libevent" ,libevent)))
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
    (version "3.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/protobuf/releases/"
                                  "download/v" version "/protobuf-cpp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14j0427ykjzrd9a66c2mpk0sjcccjlsx6q8ww6hzwb6sha3vm3f2"))))
    (build-system gnu-build-system)
    (inputs `(("zlib" ,zlib)))
    (outputs (list "out"
                   "static"))           ; ~12 MiB of .a files
    (arguments
     `(#:phases
       (modify-phases %standard-phases
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
                         (find-files lib "\\.a$"))
               #t))))))
    (home-page "https://github.com/google/protobuf")
    (synopsis "Data encoding for remote procedure calls (RPCs)")
    (description
     "Protocol Buffers are a way of encoding structured data in an efficient
yet extensible format.  Google uses Protocol Buffers for almost all of its
internal RPC protocols and file formats.")
    (license license:bsd-3)))

(define-public protobuf-next
  (package (inherit protobuf)
    (name "protobuf")
    (version "3.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/protobuf/releases/"
                                  "download/v" version "/protobuf-cpp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0a955bz59ihrb5wg7dwi12xajdi5pmz4bl0g147rbdwv393jwwxk"))))))

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
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/protobuf-c/protobuf-c/"
                                  "releases/download/v" version
                                  "/protobuf-c-" version ".tar.gz"))
              (sha256
               (base32
                "0rr2kn7804cvhdm6lzz04gz76vy0fzj15dijbr17nv8x34x2sisi"))))
    (build-system gnu-build-system)
    (inputs `(("protobuf" ,protobuf)))
    (native-inputs `(("pkg-config" ,pkg-config)))
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
    (version "1.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mapbox/protozero.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ryvn3iwxiaih3mvyy45nbwxnhzfc8vby0xh9m6d6fpakhcpf6s3"))))
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
    (version "3.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "protobuf" version))
       (sha256
        (base32
         "1q4b1m55w4gvcbzklbk8iylaii98n4in41k27d94w8ypbwlrm1q9"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/google/protobuf")
    (synopsis "Protocol buffers is a data interchange format")
    (description
     "Protocol buffers are a language-neutral, platform-neutral extensible
mechanism for serializing structured data.")
    (license license:bsd-3)))

(define-public python2-protobuf
  (package-with-python2 python-protobuf))
