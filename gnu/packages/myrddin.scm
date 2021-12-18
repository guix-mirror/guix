;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Guix Together <jgart@dismail.de>
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

(define-module (gnu packages myrddin)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public myrddin
  (package
    (name "myrddin")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/oridb/mc")
         (commit (string-append "r" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0acqqz74ypmjvi1yqz7g1ymyk3mmkzwqgmdd3s7s287bdy4a72gc"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'build
                 (lambda* (#:key make-flags #:allow-other-keys)
                   (apply invoke "make" "bootstrap" make-flags)
                   (apply invoke "make" make-flags))))))
    (native-inputs
     (list bison pkg-config))
    (home-page "https://myrlang.org")
    (synopsis "Compiler and tools for the Myrddin programming language")
    (description
     "Myrddin is a programming language.  It features strong type checking,
generics, type inference, closures, and traits. It aims for C like low level
control.  This combination makes Myrddin suitable for anything ranging from
desktop applications, to embedded systems and potentially even kernel
development.")
    (license license:expat)))
