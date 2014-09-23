;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
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
  #:use-module ((guix licenses)
                #:select (expat))
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages which)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix download)
  #:use-module (guix build gnu-build-system)
  #:use-module (guix build-system gnu))

(define-public node
  (package
    (name "node")
    (version "0.10.29")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.gz"))
              (sha256
               (base32
                "0pdib215ldypc149ad03wlfj0i8fwdfydd4q2hd7ry35yw0rsds7"))))
    (native-inputs `(("python" ,python-2)
                     ("perl" ,perl)
                     ("gcc" ,gcc-4.9)
                     ("util-linux" ,util-linux)
                     ("which" ,which)))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-replace
        'configure
        ;; Node's configure script is actually a python script, so we can't
        ;; run it with bash.
        (lambda* (#:key outputs (configure-flags '()) inputs
                        #:allow-other-keys)
          (let* ((prefix (assoc-ref outputs "out"))
                 (flags `(,(string-append "--prefix=" prefix)
                          ,@configure-flags)))
            (format #t "build directory: ~s~%" (getcwd))
            (format #t "configure flags: ~s~%" flags)
            ;; Node's configure script expects the CC environment variable to
            ;; be set.
            (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
            (zero? (apply system*
                          (string-append (assoc-ref inputs "python")
                                         "/bin/python")
                          "./configure" flags))))
        %standard-phases)))
    (synopsis "Evented I/O for V8 javascript")
    (description "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (license expat)
    (home-page "http://nodejs.org/")))
