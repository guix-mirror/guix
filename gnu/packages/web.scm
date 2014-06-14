;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Aljosha Papsch <misc@rpapsch.de>
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

(define-module (gnu packages web)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'l:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages asciidoc)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages autotools)
  #:use-module ((gnu packages compression) #:select (zlib))
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages perl))

(define-public httpd
  (package
    (name "httpd")
    (version "2.4.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://apache/httpd/httpd-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1sig08xxq1kqxr2a42ndwr9g4mm6zdqnxldhxraym2y0xcjkd7yw"))))
    (build-system gnu-build-system)
    (inputs `(("apr" ,apr)
              ("apr-util" ,apr-util)
              ("openssl" ,openssl)
              ("pcre" ,pcre)))
    (arguments
     `(#:test-target "test"
       #:configure-flags (list "--enable-rewrite"
                               "--enable-userdir"
                               "--enable-vhost-alias"
                               "--enable-ssl"
                               "--enable-mime-magic"
                               (string-append "--sysconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc/httpd"))))
    (synopsis "Featureful HTTP server")
    (description
     "The Apache HTTP Server Project is a collaborative software development
effort aimed at creating a robust, commercial-grade, featureful, and
freely-available source code implementation of an HTTP (Web) server. The project
is jointly managed by a group of volunteers located around the world, using the
Internet and the Web to communicate, plan, and develop the server and its
related documentation.")
    (license l:asl2.0)
    (home-page "https://httpd.apache.org/")))

(define-public libwebsockets
  (package
    (name "libwebsockets")
    (version "1.2")
    (source (origin
              ;; The project does not publish tarballs, so we have to take
              ;; things from Git.
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.libwebsockets.org/libwebsockets")
                    (commit (string-append "v" version
                                           "-chrome26-firefox18"))))
              (sha256
               (base32
                "1293hbz8qj4p27m1qjf8dn97r10xjyiwdpq491m87zi025s558cl"))
              (file-name (string-append name "-" version))))

    ;; The package has both CMake and GNU build systems, but the latter is
    ;; apparently better supported (CMake-generated makefiles lack an
    ;; 'install' target, for instance.)
    (build-system gnu-build-system)

    (arguments
     '(#:phases (alist-cons-before
                 'configure 'bootstrap
                 (lambda _
                   (chmod "libwebsockets-api-doc.html" #o666)
                   (zero? (system* "./autogen.sh")))
                 %standard-phases)))

    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool" ,libtool "bin")
                     ("perl" ,perl)))             ; to build the HTML doc
    (inputs `(("zlib" ,zlib)
              ("openssl" ,openssl)))
    (synopsis "WebSockets library written in C")
    (description
     "libwebsockets is a library that allows C programs to establish client
and server WebSockets connections---a protocol layered above HTTP that allows
for efficient socket-like bidirectional reliable communication channels.")
    (home-page "http://libwebsockets.org/")

    ;; This is LGPLv2.1-only with extra exceptions specified in 'LICENSE'.
    (license l:lgpl2.1)))

(define-public perl-html-tagset
  (package
    (name "perl-html-tagset")
    (version "3.20")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/P/PE/PETDANCE/HTML-Tagset-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1qh8249wgr4v9vgghq77zh1d2zs176bir223a8gh3k9nksn7vcdd"))))
    (build-system perl-build-system)
    (license (package-license perl))
    (synopsis "Perl data tables useful in parsing HTML")
    (description
     "The HTML::Tagset module contains several data tables useful in various
kinds of HTML parsing operations.")
    (home-page "http://search.cpan.org/dist/HTML-Tagset/")))

(define-public perl-html-parser
  (package
    (name "perl-html-parser")
    (version "3.71")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/HTML-Parser-"
                   version ".tar.gz"))
             (sha256
              (base32
               "00nqzdgl7c3jilx7mil19k5jwcw3as14pvkjgxi97zyk94vqp4dy"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-html-tagset" ,perl-html-tagset)))
    (license (package-license perl))
    (synopsis "Perl HTML parser class")
    (description
     "Objects of the HTML::Parser class will recognize markup and separate
it from plain text (alias data content) in HTML documents.  As different
kinds of markup and text are recognized, the corresponding event handlers
are invoked.")
    (home-page "http://search.cpan.org/~gaas/HTML-Parser/")))

(define-public perl-http-date
  (package
    (name "perl-http-date")
    (version "6.02")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/HTTP-Date-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0cz357kafhhzw7w59iyi0wvhw7rlh5g1lh38230ckw7rl0fr9fg8"))))
    (build-system perl-build-system)
    (license (package-license perl))
    (synopsis "Perl date conversion routines")
    (description
     "The HTTP::Date module provides functions that deal with date formats
used by the HTTP protocol (and then some more).")
    (home-page "http://search.cpan.org/~gaas/HTTP-Date/")))

(define-public perl-uri
  (package
    (name "perl-uri")
    (version "1.60")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/URI-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0xr31mf7lfrwhyvlx4pzp6p7alls5gi4bj8pk5g89f5cckfd74hz"))))
    (build-system perl-build-system)
    (license (package-license perl))
    (synopsis "Perl Uniform Resource Identifiers (absolute and relative)")
    (description
     "The URI module implements the URI class.  Objects of this class
represent \"Uniform Resource Identifier references\" as specified in RFC 2396
(and updated by RFC 2732).")
    (home-page "http://search.cpan.org/~gaas/URI/")))

(define-public perl-lwp-mediatypes
  (package
    (name "perl-lwp-mediatypes")
    (version "6.02")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/LWP-MediaTypes-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0xmnblp962qy02akah30sji8bxrqcyqlff2w95l199ghql60ny8q"))))
    (build-system perl-build-system)
    (license (package-license perl))
    (synopsis "Perl module to guess the media type for a file or a URL")
    (description
     "The LWP::MediaTypes module provides functions for handling media (also
known as MIME) types and encodings.  The mapping from file extensions to
media types is defined by the media.types file.  If the ~/.media.types file
exists it is used instead.")
    (home-page "http://search.cpan.org/~gaas/LWP-MediaTypes/")))

(define-public perl-io-html
  (package
    (name "perl-io-html")
    (version "1.00")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/C/CJ/CJM/IO-HTML-"
                   version ".tar.gz"))
             (sha256
              (base32
               "06nj3a0xgp5jxwxx6ayglfk2v7npf5a7gwkqsjlkapjkybarzqh4"))))
    (build-system perl-build-system)
    (license (package-license perl))
    (synopsis "Perl module to open an HTML file with automatic charset detection")
    (description
     "IO::HTML provides an easy way to open a file containing HTML while
automatically determining its encoding.  It uses the HTML5 encoding sniffing
algorithm specified in section 8.2.2.1 of the draft standard.")
    (home-page "http://search.cpan.org/~cjm/IO-HTML/")))

(define-public perl-http-message
  (package
    (name "perl-http-message")
    (version "6.06")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/HTTP-Message-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0qxdrcak97azjvqyx1anpb2ky6vp6vc37x0wcfjdqfajkh09fzh8"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-http-date" ,perl-http-date)
       ("perl-uri" ,perl-uri)))
    (inputs
     `(("perl-io-html" ,perl-io-html)
       ("perl-lwp-mediatypes" ,perl-lwp-mediatypes)))
    (license (package-license perl))
    (synopsis "Perl HTTP style message")
    (description
     "An HTTP::Message object contains some headers and a content body.")
    (home-page "http://search.cpan.org/~gaas/HTTP-Message/")))

(define-public perl-http-cookies
  (package
    (name "perl-http-cookies")
    (version "6.01")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/HTTP-Cookies-"
                   version ".tar.gz"))
             (sha256
              (base32
               "087bqmg22dg3vj7gssh3pcsh9y1scimkbl5h1kc8jqyfhgisvlzm"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-http-message" ,perl-http-message)))
    (license (package-license perl))
    (synopsis "Perl HTTP cookie jars")
    (description
     "The HTTP::Cookies class is for objects that represent a cookie jar,
that is, a database of all the HTTP cookies that a given LWP::UserAgent
object knows about.")
    (home-page "http://search.cpan.org/~gaas/HTTP-Cookies/")))

(define-public perl-http-daemon
  (package
    (name "perl-http-daemon")
    (version "6.01")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/HTTP-Daemon-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1hmd2isrkilf0q0nkxms1q64kikjmcw9imbvrjgky6kh89vqdza3"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-http-message" ,perl-http-message)
       ("perl-lwp-mediatypes" ,perl-lwp-mediatypes)))
    (license (package-license perl))
    (synopsis "Perl simple http server class")
    (description
     "Instances of the HTTP::Daemon class are HTTP/1.1 servers that listen
on a socket for incoming requests.  The HTTP::Daemon is a subclass of
IO::Socket::INET, so you can perform socket operations directly on it too.")
    (home-page "http://search.cpan.org/~gaas/HTTP-Daemon/")))

(define-public perl-http-negotiate
  (package
    (name "perl-http-negotiate")
    (version "6.01")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/HTTP-Negotiate-"
                   version ".tar.gz"))
             (sha256
              (base32
               "05p053vjs5g91v5cmjnny7a3xzddz5k7vnjw81wfh01ilqg9qwhw"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-http-message" ,perl-http-message)))
    (license (package-license perl))
    (synopsis "Perl http content negotiation")
    (description
     "The HTTP::Negotiate module provides a complete implementation of the
HTTP content negotiation algorithm specified in
draft-ietf-http-v11-spec-00.ps chapter 12.  Content negotiation allows for
the selection of a preferred content representation based upon attributes
of the negotiable variants and the value of the various Accept* header
fields in the request.")
    (home-page "http://search.cpan.org/~gaas/HTTP-Negotiate/")))

(define-public perl-www-robotrules
  (package
    (name "perl-www-robotrules")
    (version "6.02")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/WWW-RobotRules-"
                   version ".tar.gz"))
             (sha256
              (base32
               "07m50dp5n5jxv3m93i55qvnd67a6g7cvbvlik115kmc8lbkh5da6"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-uri" ,perl-uri)))
    (license (package-license perl))
    (synopsis "Perl database of robots.txt-derived permissions")
    (description
     "The WWW::RobotRules module parses /robots.txt files as specified in
\"A Standard for Robot Exclusion\", at
<http://www.robotstxt.org/wc/norobots.html>.  Webmasters can use the
/robots.txt file to forbid conforming robots from accessing parts of
their web site.")
    (home-page "http://search.cpan.org/~gaas/WWW-RobotRules/")))

(define-public perl-net-http
  (package
    (name "perl-net-http")
    (version "6.06")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/Net-HTTP-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1m1rvniffadq99gsy25298ia3lixwymr6kan64jd3ylyi7nkqkhx"))))
    (build-system perl-build-system)
    (license (package-license perl))
    (synopsis "Perl low-level HTTP connection (client)")
    (description
     "The Net::HTTP class is a low-level HTTP client.  An instance of the
Net::HTTP class represents a connection to an HTTP server.  The HTTP protocol
is described in RFC 2616.  The Net::HTTP class supports HTTP/1.0 and
HTTP/1.1.")
    (home-page "http://search.cpan.org/~gaas/Net-HTTP/")))

(define-public perl-file-listing
  (package
    (name "perl-file-listing")
    (version "6.04")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/File-Listing-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1xcwjlnxaiwwpn41a5yi6nz95ywh3szq5chdxiwj36kqsvy5000y"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-http-date" ,perl-http-date)))
    (license (package-license perl))
    (synopsis "Perl directory listing parser")
    (description
     "The File::Listing module exports a single function called parse_dir(),
which can be used to parse directory listings.")
    (home-page "http://search.cpan.org/~gaas/File-Listing/")))

(define-public perl-encode-locale
  (package
    (name "perl-encode-locale")
    (version "1.03")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/Encode-Locale-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0m9d1vdphlyzybgmdanipwd9ndfvyjgk3hzw250r299jjgh3fqzp"))))
    (build-system perl-build-system)
    (license (package-license perl))
    (synopsis "Perl locale encoding determination")
    (description
     "The POSIX locale system is used to specify both the language
conventions requested by the user and the preferred character set to consume
and output.  The Encode::Locale module looks up the charset and encoding
(called a CODESET in the locale jargon) and arranges for the Encode module
to know this encoding under the name \"locale\".  It means bytes obtained
from the environment can be converted to Unicode strings by calling
Encode::encode(locale => $bytes) and converted back again with
Encode::decode(locale => $string).")
    (home-page "http://search.cpan.org/~gaas/Encode-Locale/")))

(define-public perl-libwww
  (package
    (name "perl-libwww")
    (version "6.05")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/libwww-perl-"
                   version ".tar.gz"))
             (sha256
              (base32
               "08wgwyz7748pv5cyngxia0xl6nragfnhrp4p9s78xhgfyygpj9bv"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-encode-locale" ,perl-encode-locale)
       ("perl-file-listing" ,perl-file-listing)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-http-cookies" ,perl-http-cookies)
       ("perl-http-daemon" ,perl-http-daemon)
       ("perl-http-negotiate" ,perl-http-negotiate)
       ("perl-net-http" ,perl-net-http)
       ("perl-www-robotrules" ,perl-www-robotrules)))
    (license (package-license perl))
    (synopsis "Perl modules for the WWW")
    (description
     "The libwww-perl collection is a set of Perl modules which provides a
simple and consistent application programming interface to the
World-Wide Web.  The main focus of the library is to provide classes
and functions that allow you to write WWW clients. The library also
contain modules that are of more general use and even classes that
help you implement simple HTTP servers.")
    (home-page "http://search.cpan.org/~gaas/libwww-perl/")))

(define-public tinyproxy
  (package
    (name "tinyproxy")
    (version "1.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.samba.org/~obnox/" name "/download/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0vl9igw7vm924rs6d6bkib7zfclxnlf9s8rmml1sfwj7xda9nmdy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        ;; For the log file, etc.
        "--localstatedir=/var")
       #:phases
       (alist-cons-before
        'build 'pre-build
        (lambda* (#:key inputs #:allow-other-keys #:rest args)
          ;; This stuff is needed, because without it, xmlint etc tries
          ;; to download docbookx.dtd and docbook.xsl from the net
          (let ((build (assoc-ref %standard-phases 'build))
                (docbook-xml (assoc-ref inputs "docbook-xml"))
                (docbook-xsl (assoc-ref inputs "docbook-xsl"))
                (our-catalog "/tmp/docbook-xml.xml"))
            (setenv "XML_CATALOG_FILES" our-catalog)
            (with-output-to-file our-catalog
              (lambda ()
                (display (string-append
                          "<?xml version=\"1.0\"?>
<!DOCTYPE catalog PUBLIC \"-//OASIS//DTD XML Catalogs V1.0//EN\"
\"file:///usr/share/xml/schema/xml-core/catalog.dtd\">
<catalog xmlns=\"urn:oasis:names:tc:entity:xmlns:xml:catalog\">
<system systemId=\"http://www.oasis-open.org/docbook/xml/4.5/docbookx.dtd\"
uri=\"file://" docbook-xml  "/xml/dtd/docbook/docbookx.dtd\"/>
<system systemId=\"http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl\"
uri=\"file://" docbook-xsl  "/xml/xsl/docbook-xsl-1.72.0/manpages/docbook.xsl\"/>
</catalog>\n"))))))
        %standard-phases)))
    ;; All of the below are used to generate the documentation
    ;; (Should they be propagated inputs of asciidoc ??)
    (native-inputs `(("asciidoc" ,asciidoc)
                     ("libxml2" ,libxml2) 
                     ("docbook-xml" ,docbook-xml)
                     ("docbook-xsl" ,docbook-xsl)
                     ("libxslt" ,libxslt)))
    (home-page "https://banu.com/tinyproxy/")
    (synopsis "Light-weight HTTP/HTTPS proxy daemon")
    (description "Tinyproxy is a light-weight HTTP/HTTPS proxy
daemon. Designed from the ground up to be fast and yet small, it is an ideal
solution for use cases such as embedded deployments where a full featured HTTP
proxy is required, but the system resources for a larger proxy are
unavailable.") 
    (license l:gpl2+)))
