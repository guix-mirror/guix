;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages zwave)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xml))

(define-public open-zwave
  (package
    (name "open-zwave")
    (version "1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenZWave/open-zwave/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xgs4mmr0480c269wx9xkk67ikjzxkh8xcssrdx0f5xcl1lyd333"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Set RUNPATH on the 'MinOZW' executable.
                  (substitute* "cpp/examples/MinOZW/Makefile"
                    (("\\$\\(LDFLAGS\\)")
                     "$(LDFLAGS) -Wl,-rpath=$(PREFIX)/lib"))

                  ;; Delete the bundled TinyXML.
                  (delete-file-recursively "cpp/tinyxml")
                  (substitute* "cpp/build/Makefile"
                    (("LIBS \\+= -ludev")
                     "LIBS += -ludev -ltinyxml "))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure))            ;no 'configure' script

       #:make-flags (list "BUILD=debug"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          (string-append "pkgconfigdir="
                                         (assoc-ref %outputs "out")
                                         "/lib/pkgconfig"))

       ;; "make check" and "make fulltest" are only concerned with checking
       ;; the device XML database and it's not entirely clear what to get from
       ;; them.
       #:tests? #f))
    (native-inputs `(("which" ,which)
                     ("perl" ,perl)               ;for tests
                     ("perl-xml-simple" ,perl-xml-simple)))
    (inputs `(("tinyxml" ,tinyxml)
              ("eudev" ,eudev)))
    (home-page "http://www.openzwave.net/")
    (synopsis "Access Z-Wave devices from C++ programs")
    (description
     "OpenZWave (or OZW) is a C++ library that interfaces with selected Z-Wave
PC controllers.  It allows developers to create applications that manipulate
and respond to devices on a Z-Wave network, without requiring in-depth
knowledge of the Z-Wave protocol.")
    (license license:lgpl3+)))
