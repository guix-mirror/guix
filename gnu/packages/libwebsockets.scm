;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages libwebsockets)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses)
                #:select (lgpl2.1))
  #:use-module (gnu packages autotools)
  #:use-module ((gnu packages compression) #:select (zlib))
  #:use-module (gnu packages perl)
  #:use-module (gnu packages openssl))

(define-public libwebsockets
  (package
    (name "libwebsockets")
    (version "1.2")
    (source (origin
              ;; The project does not publish tarballs, so we have to take
              ;; things from Git.
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.libwebsockets.org/libwebsockets")
                    (commit (string-append "v" version
                                           "-chrome26-firefox18"))))
              (sha256
               (base32
                "1293hbz8qj4p27m1qjf8dn97r10xjyiwdpq491m87zi025s558cl"))
              (file-name (string-append name "-" version))))

    ;; The package has both CMake and GNU build systems, but the latter is
    ;; apparently better supported (CMake-generated makefiles lack an
    ;; 'install' target, for instance.)
    (build-system gnu-build-system)

    (arguments
     '(#:phases (alist-cons-before
                 'configure 'bootstrap
                 (lambda _
                   (chmod "libwebsockets-api-doc.html" #o666)
                   (zero? (system* "./autogen.sh")))
                 %standard-phases)))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool" ,libtool "bin")
                     ("perl" ,perl)))             ; to build the HTML doc
    (inputs `(("zlib" ,zlib)
              ("openssl" ,openssl)))
    (synopsis "WebSockets library written in C")
    (description
     "libwebsockets is a library that allows C programs to establish client
and server WebSockets connections---a protocol layered above HTTP that allows
for efficient socket-like bidirectional reliable communication channels.")
    (home-page "http://libwebsockets.org/")

    ;; This is LGPLv2.1-only with extra exceptions specified in 'LICENSE'.
    (license lgpl2.1)))
