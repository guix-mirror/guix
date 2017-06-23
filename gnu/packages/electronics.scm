;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Theodoros Foradis <theodoros.for@openmailbox.org>
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

(define-module (gnu packages electronics)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))


(define-public libserialport
  (package
    (name "libserialport")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/libserialport/libserialport-"
                    version ".tar.gz"))
              (sha256
               (base32
                "17ajlwgvyyrap8z7f16zcs59pksvncwbmd3mzf98wj7zqgczjaja"))))
    (build-system gnu-build-system)
    (home-page "http://sigrok.org/wiki/Libserialport")
    (synopsis "Library for using serial ports")
    (description "Libserialport is a minimal shared library written in C that is intended
to take care of the OS-specific details when writing software that uses serial ports.")
    (license license:lgpl3+)))

(define-public libsigrokdecode
  (package
    (name "libsigrokdecode")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/libsigrokdecode/libsigrokdecode-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1hfigfj1976qk11kfsgj75l20qvyq8c9p2h4mjw23d59rsg5ga2a"))))
    (outputs '("out" "doc"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-doc
           (lambda _
             (zero? (system* "doxygen"))))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "doxy/html-api"
                               (string-append (assoc-ref outputs "doc")
                                              "/share/doc/libsigrokdecode"))
             #t)))))
    (native-inputs
     `(("check" ,check)
       ("doxygen" ,doxygen)
       ("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)))
    ;; libsigrokdecode.pc lists "python" in Requires.private, and "glib" in Requires.
    (propagated-inputs
     `(("glib" ,glib)
       ("python" ,python)))
    (build-system gnu-build-system)
    (home-page "http://www.sigrok.org/wiki/Libsigrokdecode")
    (synopsis "Library providing (streaming) protocol decoding functionality")
    (description "Libsigrokdecode is a shared library written in C, which provides
(streaming) protocol decoding functionality.")
    (license license:gpl3+)))
