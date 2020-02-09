;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
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

(define-module (gnu packages i2p)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages web)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public i2pd
  (package
    (name "i2pd")
    (version "2.29.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PurpleI2P/i2pd.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1issg3aidwikk4g12sa8q81zzp0hd0g8wdy2dx4899z8yrscl300"))))
    (build-system cmake-build-system)
    (inputs `(("boost" ,boost)
              ("miniupnpc" ,miniupnpc)
              ("openssl" ,openssl)
              ("websocketpp" ,websocketpp)
              ("zlib" ,zlib)))
    (arguments '(#:configure-flags
                 (let ((source (assoc-ref %build-inputs "source")))
                   (list (string-append "-S" source "/build")
                         "-DWITH_PCH=OFF"
                         "-DWITH_STATIC=OFF"
                         "-DWITH_UPNP=ON"
                         "-DWITH_WEBSOCKETS=ON"
                         "-DWITH_LIBRARY=ON"
                         "-DBUILD_SHARED_LIBS=ON"
                         "-DWITH_BINARY=ON"))
                 #:phases
                 (modify-phases %standard-phases
                   (replace 'check
                     (lambda* (#:key
                               (make-flags '())
                               (parallel-tests? #t)
                               #:allow-other-keys)
                       (let ((source (assoc-ref %build-inputs "source")))
                         (copy-recursively (string-append source "/tests")
                                           "./tests")
                         (with-directory-excursion "tests"
                           (substitute* "Makefile"
                             (("../libi2pd/") (string-append source "/libi2pd/"))
                             ;; Disable the x25519 test, which only compiles if
                             ;; openssl doesn't have X25519 support, but the
                             ;; version we use has it.
                             (("test-base-64 test-x25519 test-aeadchacha20poly1305")
                              "test-base-64 test-aeadchacha20poly1305"))
                           (apply invoke "make" "all"
                                  `(,@(if parallel-tests?
                                          `("-j" ,(number->string
                                                    (parallel-job-count)))
                                          '())
                                    ,@make-flags))))))
                   (add-after 'install 'install-headers
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((install-dir (assoc-ref outputs "out"))
                              (src-dir (string-append install-dir "/src"))
                              (include-dir
                               (string-append install-dir "/include")))
                         (mkdir-p include-dir)
                         ;; This is the only header file that's relevant to the
                         ;; public interface.
                         ;; <https://github.com/PurpleI2P/i2pd/issues/1378>
                         (install-file (string-append src-dir "/api.h")
                                       include-dir)
                         #t)))
                   (add-after 'install-headers 'remove-source
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((install-dir (assoc-ref outputs "out"))
                              (src-dir (string-append install-dir "/src")))
                         (delete-file-recursively src-dir)
                         (delete-file (string-append install-dir
                                                     "/LICENSE"))
                         #t))))))
    (home-page "https://i2pd.website/")
    (synopsis "Router for an end-to-end encrypted and anonymous internet")
    (description "i2pd is a client for the anonymous I2P network, upon which
applications for file sharing, web browsing, instant messaging, and more are
built. i2pd allows people from all around the world to communicate and share
information securely without restrictions.")
    (license license:bsd-3)))
