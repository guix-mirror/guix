;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages node)
  #:use-module ((guix licenses) #:select (expat))
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public node
  (package
    (name "node")
    (version "10.19.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.xz"))
              (sha256
               (base32
                "0sginvcsf7lrlzsnpahj4bj1f673wfvby8kaxgvzlrbb7sy229v2"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; Remove bundled software.
                  (for-each delete-file-recursively
                            '("deps/cares"
                              "deps/http_parser"
                              "deps/icu-small"
                              "deps/nghttp2"
                              "deps/openssl"
                              "deps/uv"
                              "deps/zlib"))
                  (substitute* "Makefile"
                    ;; Remove references to bundled software.
                    (("deps/http_parser/http_parser.gyp") "")
                    (("deps/uv/include/\\*.h") "")
                    (("deps/uv/uv.gyp") "")
                    (("deps/zlib/zlib.gyp") ""))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--shared-cares"
                           "--shared-http-parser"
                           "--shared-libuv"
                           "--shared-nghttp2"
                           "--shared-openssl"
                           "--shared-zlib"
                           "--without-snapshot"
                           "--with-intl=system-icu")
       ;; Run only the CI tests.  The default test target requires additional
       ;; add-ons from NPM that are not distributed with the source.
       #:test-target "test-ci-js"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-files
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Fix hardcoded /bin/sh references.
             (substitute* '("lib/child_process.js"
                            "lib/internal/v8_prof_polyfill.js"
                            "test/parallel/test-child-process-spawnsync-shell.js"
                            "test/parallel/test-stdio-closed.js"
                            "test/sequential/test-child-process-emfile.js")
               (("'/bin/sh'")
                (string-append "'" (which "sh") "'")))

             ;; Fix hardcoded /usr/bin/env references.
             (substitute* '("test/parallel/test-child-process-default-options.js"
                            "test/parallel/test-child-process-env.js"
                            "test/parallel/test-child-process-exec-env.js")
               (("'/usr/bin/env'")
                (string-append "'" (which "env") "'")))

             ;; FIXME: These tests fail in the build container, but they don't
             ;; seem to be indicative of real problems in practice.
             (for-each delete-file
                       '("test/parallel/test-cluster-master-error.js"
                         "test/parallel/test-cluster-master-kill.js"
                         ;; See also <https://github.com/nodejs/node/issues/25903>.
                         "test/sequential/test-performance.js"))

             ;; This requires a DNS resolver.
             (delete-file "test/parallel/test-dns.js")

             ;; FIXME: This test fails randomly:
             ;; https://github.com/nodejs/node/issues/31213
             (delete-file "test/parallel/test-net-listen-after-destroying-stdin.js")

             ;; FIXME: These tests fail on armhf-linux:
             ;; https://github.com/nodejs/node/issues/31970
             ,@(if (string-prefix? "arm" (%current-system))
                   '((for-each delete-file
                               '("test/parallel/test-zlib.js"
                                 "test/parallel/test-zlib-brotli.js"
                                 "test/parallel/test-zlib-brotli-flush.js"
                                 "test/parallel/test-zlib-brotli-from-brotli.js"
                                 "test/parallel/test-zlib-brotli-from-string.js"
                                 "test/parallel/test-zlib-convenience-methods.js"
                                 "test/parallel/test-zlib-random-byte-pipes.js"
                                 "test/parallel/test-zlib-write-after-flush.js")))
                   '())

             ;; These tests have an expiry date: they depend on the validity of
             ;; TLS certificates that are bundled with the source.  We want this
             ;; package to be reproducible forever, so remove those.
             ;; TODO: Regenerate certs instead.
             (for-each delete-file
                       '("test/parallel/test-tls-passphrase.js"
                         "test/parallel/test-tls-server-verify.js"))
             #t))
         (replace 'configure
           ;; Node's configure script is actually a python script, so we can't
           ;; run it with bash.
           (lambda* (#:key outputs (configure-flags '()) inputs
                     #:allow-other-keys)
             (let* ((prefix (assoc-ref outputs "out"))
                    (flags (cons (string-append "--prefix=" prefix)
                                 configure-flags)))
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)
               ;; Node's configure script expects the CC environment variable to
               ;; be set.
               (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
               (apply invoke
                      (string-append (assoc-ref inputs "python")
                                     "/bin/python")
                      "configure" flags))))
         (add-after 'patch-shebangs 'patch-npm-shebang
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((bindir (string-append (assoc-ref outputs "out")
                                           "/bin"))
                    (npm    (string-append bindir "/npm"))
                    (target (readlink npm)))
               (with-directory-excursion bindir
                 (patch-shebang target (list bindir))
                 #t)))))))
    (native-inputs
     `(("python" ,python-2)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("procps" ,procps)
       ("util-linux" ,util-linux)
       ("which" ,which)))
    (native-search-paths
     (list (search-path-specification
            (variable "NODE_PATH")
            (files '("lib/node_modules")))))
    (inputs
     `(("c-ares" ,c-ares)
       ("http-parser" ,http-parser)
       ("icu4c" ,icu4c)
       ("libuv" ,libuv/latest)
       ("nghttp2" ,nghttp2-1.40 "lib")
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (synopsis "Evented I/O for V8 JavaScript")
    (description "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (home-page "https://nodejs.org/")
    (license expat)
    (properties '((max-silent-time . 7200)     ;2h, needed on ARM
                  (timeout . 21600)))))        ;6h
