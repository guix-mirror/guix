;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mike Gerwitz <mtg@gnu.org>
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
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public node
  (package
    (name "node")
    (version "8.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.gz"))
              (sha256
               (base32
                "1qbiz7hgwlirhwpd71c8yzcbwsyi5bjlfp6lxb6v55j6rizinj9j"))
              ;; See https://github.com/nodejs/node/issues/16688
              ;; Remove this next update (>8.9.1).
              (patches
                (search-patches "node-test-http2-server-rst-stream.patch"))))
    (build-system gnu-build-system)
    (arguments
     ;; TODO: Purge the bundled copies from the source.
     '(#:configure-flags '("--shared-openssl"
                           "--shared-zlib"
                           "--shared-libuv"
                           "--shared-cares"
                           "--shared-http-parser"
                           "--without-snapshot")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-files
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Fix hardcoded /bin/sh references.
             (substitute* '("lib/child_process.js"
                            "lib/internal/v8_prof_polyfill.js"
                            "test/parallel/test-child-process-spawnsync-shell.js"
                            "test/parallel/test-stdio-closed.js")
               (("'/bin/sh'")
                (string-append "'" (which "sh") "'")))

             ;; Fix hardcoded /usr/bin/env references.
             (substitute* '("test/parallel/test-child-process-default-options.js"
                            "test/parallel/test-child-process-env.js"
                            "test/parallel/test-child-process-exec-env.js")
               (("'/usr/bin/env'")
                (string-append "'" (which "env") "'")))


             ;; test-make-doc needs doc-only target, which is inhibited below
             (for-each delete-file
                       '("test/doctool/test-make-doc.js"))
             ;; FIXME: This test seems to depends on files that are not
             ;; available in the bundled v8. See
             ;; https://github.com/nodejs/node/issues/13344
             (for-each delete-file
                       '("test/addons-napi/test_general/testInstanceOf.js"))
             ;; FIXME: These tests fail in the build container, but they don't
             ;; seem to be indicative of real problems in practice.
             (for-each delete-file
                       '("test/async-hooks/test-ttywrap.readstream.js"
                         "test/parallel/test-util-inspect.js"
                         "test/parallel/test-v8-serdes.js"
                         "test/parallel/test-dgram-membership.js"
                         "test/parallel/test-dns-cancel-reverse-lookup.js"
                         "test/parallel/test-dns-resolveany.js"
                         "test/parallel/test-cluster-master-error.js"
                         "test/parallel/test-cluster-master-kill.js"
                         "test/parallel/test-npm-install.js"
                         "test/parallel/test-regress-GH-746.js"
                         "test/sequential/test-child-process-emfile.js"
                         "test/sequential/test-benchmark-child-process.js"
                         "test/sequential/test-http-regr-gh-2928.js"))
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
               (zero? (apply system*
                             (string-append (assoc-ref inputs "python")
                                            "/bin/python")
                             "configure" flags)))))
         (add-before 'check 'skip-check-doc-only
           (lambda _
             (substitute* "Makefile"
               ;; requires js-yaml, which is not part of the distribution,
               ;; and falls back to using npm to download it
               (("\\$\\(MAKE\\) doc-only" all)
                (string-append "#" all)))
             #t))
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
       ("libuv" ,libuv)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (synopsis "Evented I/O for V8 JavaScript")
    (description "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (home-page "http://nodejs.org/")
    (license expat)
    (properties '((timeout . 3600))))) ; 1 h
