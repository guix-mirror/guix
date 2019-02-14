;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages rrdtool)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public rrdtool
  (package
    (name "rrdtool")
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://oss.oetiker.ch/rrdtool/pub/rrdtool-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1bhsg119j94xwykp2sbp01hhxcg78gzblfn7j98slrv9va77g6wq"))))
    (build-system gnu-build-system)
    (inputs
     `(("cairo" ,cairo)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("gtk" ,gtk+-2)
       ("libxml2" ,libxml2)
       ("pango" ,pango)
       ("python" ,python-2)))
    (native-inputs
     `(("groff" ,groff)
       ("pkg-config" ,pkg-config)

       ;; For tests.
       ("bc" ,bc)
       ("perl" ,perl)                   ; will also build Perl bindings
       ("tzdata" ,tzdata-for-tests)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda _
             (substitute* "libtool"
               (("/bin/sed") (which "sed")))
             #t))
         (add-before 'check 'prepare-test-environment
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZDIR"
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             #t))
         (add-after 'install 'remove-native-input-references
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (examples (string-append out "/share/rrdtool/examples")))
               ;; Drop shebangs from examples to avoid depending on native-input
               ;; perl.  It's clear from context and extension how to run them.
               (substitute* (find-files examples "\\.pl$")
                 (("^#!.*") ""))
               #t))))))
    (home-page "https://oss.oetiker.ch/rrdtool/")
    (synopsis "Time-series data storage and display system")
    (description
     "The Round Robin Database Tool (RRDtool) is a system to store and display
time-series data (e.g. network bandwidth, machine-room temperature, server
load average).  It stores the data in Round Robin Databases (RRDs), a very
compact way that will not expand over time.  RRDtool processes the extracted
data to enforce a certain data density, allowing for useful graphical
representation of data values.")
    (license license:gpl2+))) ; with license exception that allows combining
                              ; with many other licenses.
