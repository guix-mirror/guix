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

(define-module (gnu packages docbook)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:autoload   (gnu packages zip) (unzip))

(define-public docbook-xml
  (package
    (name "docbook-xml")
    (version "4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.docbook.org/xml/" version
                                  "/docbook-xml-" version ".zip"))
              (sha256
               (base32
                "1d671lcjckjri28xfbf6dq7y3xnkppa910w1jin8rjc35dx06kjf"))))
    (build-system trivial-build-system)
    (arguments
     '(#:builder (begin
                   (use-modules (guix build utils))

                   (let* ((unzip
                           (string-append (assoc-ref %build-inputs "unzip")
                                          "/bin/unzip"))
                          (source (assoc-ref %build-inputs "source"))
                          (out    (assoc-ref %outputs "out"))
                          (dtd    (string-append out "/xml/dtd/docbook")))
                     (mkdir-p dtd)
                     (with-directory-excursion dtd
                       (system* unzip source))))
       #:modules ((guix build utils))))
    (native-inputs `(("unzip" ,unzip)))
    (home-page "http://docbook.org")
    (synopsis "DocBook XML DTDs for document authoring")
    (description
     "DocBook is general purpose XML and SGML document type particularly well
suited to books and papers about computer hardware and software (though it is
by no means limited to these applications.)  This package provides XML DTDs.")
    (license (x11-style "" "See file headers."))))

(define-public docbook-xsl
  (package
    (name "docbook-xsl")
    (version "1.78.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/docbook/docbook-xsl-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0rxl013ncmz1n6ymk2idvx3hix9pdabk8xn01cpcv32wmfb753y9"))))
    (build-system trivial-build-system)
    (arguments
     `(#:builder (begin
                   (use-modules (guix build utils))

                   (let* ((bzip2  (assoc-ref %build-inputs "bzip2"))
                          (tar    (assoc-ref %build-inputs "tar"))
                          (source (assoc-ref %build-inputs "source"))
                          (out    (assoc-ref %outputs "out"))
                          (xsl    (string-append out "/xml/xsl")))
                     (setenv "PATH" (string-append bzip2 "/bin"))
                     (system* (string-append tar "/bin/tar") "xvf" source)

                     (mkdir-p xsl)
                     (copy-recursively (string-append ,name "-" ,version)
                                       (string-append xsl "/" ,name
                                                      "-" ,version))))
       #:modules ((guix build utils))))
    (native-inputs `(("bzip2" ,bzip2)
                     ("tar" ,tar)))
    (home-page "http://docbook.org")
    (synopsis "DocBook XSL style sheets for document authoring")
    (description
     "This package provides XSL style sheets for DocBook.")
    (license (x11-style "" "See 'COPYING' file."))))
