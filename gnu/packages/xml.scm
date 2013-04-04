;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl))

(define-public expat
  (package
    (name "expat")
    (version "2.1.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/expat/expat/"
                                 version "/expat-" version ".tar.gz"))
             (sha256
              (base32
               "11pblz61zyxh68s5pdcbhc30ha1b2vfjd83aiwfg4vc15x3hadw2"))))
    (build-system gnu-build-system)
    (home-page "http://www.libexpat.org/")
    (synopsis "A stream-oriented XML parser library written in C")
    (description
     "Expat is an XML parser library written in C.  It is a
stream-oriented parser in which an application registers handlers for
things the parser might find in the XML document (like start tags).")
    (license license:expat)))

(define-public libxml2
  (package
    (name "libxml2")
    (version "2.9.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://xmlsoft.org/libxml2/libxml2-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "10ib8bpar2pl68aqksfinvfmqknwnk7i35ibq6yjl8dpb0cxj9dd"))))
    (build-system gnu-build-system)
    (home-page "http://www.xmlsoft.org/")
    (synopsis "libxml2, a C parser for XML")
    (inputs `(("perl" ,perl)
              ("python" ,python)
              ("zlib" ,zlib)))
    (arguments
     `(#:phases
        (alist-replace
         'install
         (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
          (let ((install (assoc-ref %standard-phases 'install))
                (glibc (assoc-ref inputs "libc"))
                (out (assoc-ref outputs "out")))
            (apply install args)
            (chdir "python")
            (substitute* "setup.py" (("/opt/include") (string-append glibc "/include")))
            (system* "python" "setup.py" "install" (string-append "--prefix=" out))))
        %standard-phases)))
    (description
     "Libxml2 is the XML C parser and toolkit developed for the Gnome project
(but it is usable outside of the Gnome platform).")
    (license license:x11)))

(define-public libxslt
  (package
    (name "libxslt")
    (version "1.1.28")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://xmlsoft.org/libxslt/libxslt-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "13029baw9kkyjgr7q3jccw2mz38amq7mmpr5p3bh775qawd1bisz"))))
    (build-system gnu-build-system)
    (home-page "http://xmlsoft.org/XSLT/index.html")
    (synopsis "libxslt, a C library for applying XSLT stylesheets to XML documents")
    (inputs `(("libgcrypt" ,libgcrypt)
              ("libxml2" ,libxml2)
              ("python" ,python)
              ("zlib" ,zlib)))
    (arguments
      `(#:phases
         (alist-replace
          'configure
          (lambda* (#:key inputs #:allow-other-keys #:rest args)
            (let ((configure (assoc-ref %standard-phases 'configure))
                  (libxml2 (assoc-ref inputs "libxml2")))
              ;; FIXME: This should be done more centrally.
              (setenv "PYTHONPATH" (string-append libxml2 "/lib/python2.7/site-packages"))
              (apply configure args)))
         %standard-phases)))
    (description
     "Libxslt is an XSLT C library developed for the GNOME project. It is
based on libxml for XML parsing, tree manipulation and XPath support.")
    (license license:x11)))

(define-public perl-xml-parser
  (package
    (name "perl-xml-parser")
    (version "2.41")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/T/TO/TODDR/XML-Parser-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1sadi505g5qmxr36lgcbrcrqh3a5gcdg32b405gnr8k54b6rg0dl"))))
    (build-system perl-build-system)
    (arguments `(#:make-maker-flags
                 (let ((expat (assoc-ref %build-inputs "expat")))
                   (list (string-append "EXPATLIBPATH=" expat "/lib")
                         (string-append "EXPATINCPATH=" expat "/include")))))
    (inputs `(("expat" ,expat)))
    (license (package-license perl))
    (synopsis "Perl bindings to the Expat XML parsing library")
    (description
     "This module provides ways to parse XML documents.  It is built on top of
XML::Parser::Expat, which is a lower level interface to James Clark's expat
library.  Each call to one of the parsing methods creates a new instance of
XML::Parser::Expat which is then used to parse the document.  Expat options
may be provided when the XML::Parser object is created.  These options are
then passed on to the Expat object on each parse call.  They can also be given
as extra arguments to the parse methods, in which case they override options
given at XML::Parser creation time.")
    (home-page "http://search.cpan.org/~toddr/XML-Parser-2.41/Parser.pm")))

(define-public intltool
  (package
    (name "intltool")
    (version "0.50.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://launchpad.net/intltool/trunk/"
                                 version "/+download/intltool-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "01j4yd7i84n9nk4ccs6yifg84pp68nr9by57jdbhj7dpdxf5rwk7"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("perl" ,perl)
       ("perl-xml-parser" ,perl-xml-parser)))
    (arguments
      `(#:phases
         (alist-replace
          'configure
          (lambda* (#:key inputs #:allow-other-keys #:rest args)
            (let ((configure (assoc-ref %standard-phases 'configure))
                  (perl-xml-parser (assoc-ref inputs "perl-xml-parser")))
              ;; FIXME: This should be done more centrally.
              (setenv "PERL5LIB" (string-append perl-xml-parser "/lib/perl5/site_perl"))
              (apply configure args)))
         %standard-phases)))
    (home-page "https://launchpad.net/intltool/+download")
    (synopsis "Utility scripts for internationalising xml")
    (description
     "intltool automatically extracts translatable strings from oaf, glade,
bonobo ui, nautilus theme and other XML files into the po files.
It automatically merges translations from po files back into .oaf files
(encoding to be 7-bit clean). The merging mechanism can also be extended to
support other types of XML files.")
    (license license:gpl2+)))

