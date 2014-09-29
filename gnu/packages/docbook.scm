;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
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
                       (system* unzip source))
                     (substitute* (string-append out "/xml/dtd/docbook/catalog.xml")
                       (("uri=\"") 
                        (string-append 
                         "uri=\"file://" dtd "/")))))
                 #:modules ((guix build utils))))
    (native-inputs `(("unzip" ,unzip)))
    (home-page "http://docbook.org")
    (synopsis "DocBook XML DTDs for document authoring")
    (description
     "DocBook is general purpose XML and SGML document type particularly well
suited to books and papers about computer hardware and software (though it is
by no means limited to these applications.)  This package provides XML DTDs.")
    (license (x11-style "" "See file headers."))))

(define-public docbook-xml-4.4
  (package (inherit docbook-xml)
   (version "4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.docbook.org/xml/" version
                                  "/docbook-xml-" version ".zip"))
              (sha256
               (base32
                "141h4zsyc71sfi2zzd89v4bb4qqq9ca1ri9ix2als9f4i3mmkw82"))))))

(define-public docbook-xml-4.3
  (package (inherit docbook-xml)
   (version "4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.docbook.org/xml/" version
                                  "/docbook-xml-" version ".zip"))
              (sha256
               (base32
                "0r1l2if1z4wm2v664sqdizm4gak6db1kx9y50jq89m3gxaa8l1i3"))))))

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
     `(#:builder (let ((name-version (string-append ,name "-" ,version)))
                   (use-modules (guix build utils))

                   (let* ((bzip2  (assoc-ref %build-inputs "bzip2"))
                          (tar    (assoc-ref %build-inputs "tar"))
                          (source (assoc-ref %build-inputs "source"))
                          (out    (assoc-ref %outputs "out"))
                          (xsl    (string-append out "/xml/xsl")))
                     (setenv "PATH" (string-append bzip2 "/bin"))
                     (system* (string-append tar "/bin/tar") "xvf" source)

                     (mkdir-p xsl)
                     (copy-recursively name-version
                                       (string-append xsl "/" name-version))

                     (substitute* (string-append xsl "/" name-version "/catalog.xml")
                       (("rewritePrefix=\"./") 
                        (string-append "rewritePrefix=\"file://" xsl "/" name-version "/")))))
                 #:modules ((guix build utils))))
    (native-inputs `(("bzip2" ,bzip2)
                     ("tar" ,tar)))
    (home-page "http://docbook.org")
    (synopsis "DocBook XSL style sheets for document authoring")
    (description
     "This package provides XSL style sheets for DocBook.")
    (license (x11-style "" "See 'COPYING' file."))))

(define-public dblatex
  (package
    (name "dblatex")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/dblatex/dblatex-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0h3472n33pabrn8qwggsahkrjx8lybpwlc3zprby3w3w3x5i830f"))))
    (build-system python-build-system)
    ;; TODO: Add xfig/transfig for fig2dev utility
    (inputs
     `(("python-setuptools" ,python-setuptools)
       ("texlive" ,texlive)
       ("imagemagick" ,imagemagick)     ;for convert
       ("inkscape" ,inkscape)           ;for svg conversion
       ("docbook" ,docbook-xml)
       ("libxslt" ,libxslt)))           ;for xsltproc
    (arguments
     `(#:python ,python-2               ;'print' syntax
       #:tests? #f                      ;no 'test' command
       #:phases
       (alist-cons-after
        'wrap 'set-path
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
            ;; dblatex executes helper programs at runtime.
            (wrap-program (string-append out "/bin/dblatex")
                          `("PATH" ":" prefix
                            ,(map (lambda (input)
                                    (string-append (assoc-ref inputs input)
                                                   "/bin"))
                                  '("libxslt" "texlive"
                                    "imagemagick" "inkscape"))))))
        %standard-phases)))
    (home-page "http://dblatex.sourceforge.net")
    (synopsis "DocBook to LaTeX Publishing")
    (description
     "DocBook to LaTeX Publishing transforms your SGML/XML DocBook documents
to DVI, PostScript or PDF by translating them in pure LaTeX as a first
process.  MathML 2.0 markups are supported too.  It started as a clone of
DB2LaTeX.")
    ;; lib/contrib/which is under an X11 license
    (license gpl2+)))
