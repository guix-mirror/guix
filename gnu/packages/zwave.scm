;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
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
  #:use-module (guix build-system node)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
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
              (patches (search-patches "open-zwave-hidapi.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Set RUNPATH on the 'MinOZW' executable.
                  (substitute* "cpp/examples/MinOZW/Makefile"
                    (("\\$\\(LDFLAGS\\)")
                     "$(LDFLAGS) -Wl,-rpath=$(PREFIX)/lib"))

                  ;; XXX: There's a bundled TinyXML under cpp/tinyxml.  Keep
                  ;; it because using our own TinyXML leads to double-free
                  ;; down the road.

                  ;; Delete the bundled HIDAPI.
                  (delete-file-recursively "cpp/hidapi")
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
    (native-inputs (list which pkg-config perl ;for tests
                         perl-xml-simple))
    (inputs (list hidapi eudev))
    (home-page "http://www.openzwave.net/")
    (synopsis "Access Z-Wave devices from C++ programs")
    (description
     "OpenZWave (or OZW) is a C++ library that interfaces with selected Z-Wave
PC controllers.  It allows developers to create applications that manipulate
and respond to devices on a Z-Wave network, without requiring in-depth
knowledge of the Z-Wave protocol.")
    (license license:lgpl3+)))

(define-public node-openzwave-shared
  (package
    (name "node-openzwave-shared")
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenZWave/node-openzwave-shared")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1bqqy12dzqj05j9wsh50dmn84dddmhz0gjzvd3y20z4hpy1v8rsc"))))
    (inputs
     (list open-zwave node-nan))
    (native-inputs
     (list which python pkg-config))
    (build-system node-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           ;; For some reason, `npm install` doesn't build
           ;; the addon automatically, so we do it explicitly here.
           ;; We go through `npx` so the npmrc file sets the
           ;; configuration up properly.
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (invoke (search-input-file (or native-inputs inputs) "/bin/npx")
                     "--call"
                     (string-append
                      (search-input-file
                       (or native-inputs inputs)
                       "/lib/node_modules/npm/bin/node-gyp-bin/node-gyp")
                      " rebuild")))))))
    (home-page "https://github.com/OpenZWave/node-openzwave-shared")
    (synopsis "Node.js bindings for OpenZWave")
    (description
     "With the @code{node-openzwave-shared} package, you can easily control
and manage your Z-Wave devices (lights, dimmers, blinds, you name it) from
within Node.js applications.  This library also supports secure
devices (e.g. door locks) that require encryption.  All widely used Node.js
versions are supported with the help of @code{node-nan}.

This library is currently able to:
@itemize @bullet
@item
scan a Z-Wave network and report on connected devices;
@item
write values to Z-Wave nodes;
@item
monitor the network for changes;
@item
heal nodes and/or the network; and
@item
perform management tasks: add or remove nodes, replace failed nodes,
manage their group associations, etc.
@end itemize")
    (license license:isc)))
