;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Aljosha Papsch <misc@rpapsch.de>
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Raoul Jean Pierre Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2016, 2017, 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016, 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 Bake Timmons <b3timmons@speedymail.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
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
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix cvs-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system scons)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages check)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnu-doc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages base)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

(define-public httpd
  (package
    (name "httpd")
    (version "2.4.38")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://apache/httpd/httpd-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0jiriyyf3pm6axf4mrz6c2z08yhs21hb4d23viq87jclm5bmiikx"))))
    (build-system gnu-build-system)
    (native-inputs `(("pcre" ,pcre "bin")))       ;for 'pcre-config'
    (inputs `(("apr" ,apr)
              ("apr-util" ,apr-util)
              ("openssl" ,openssl)
              ("perl" ,perl))) ; needed to run bin/apxs
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
freely-available source code implementation of an HTTP (Web) server.  The
project is jointly managed by a group of volunteers located around the world,
using the Internet and the Web to communicate, plan, and develop the server
and its related documentation.")
    (license l:asl2.0)
    (home-page "https://httpd.apache.org/")))

(define-public mod-wsgi
  (package
    (name "mod-wsgi")
    (version "4.5.22")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/GrahamDumpleton/mod_wsgi.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q90xw2cbhka5gcd6yc69iir73x4gm7fm75qpkins2ryfl6w1q3f"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                 ; TODO: can't figure out if there are tests
       #:make-flags (list
                     (string-append "DESTDIR="
                                    (assoc-ref %outputs "out"))
                     "LIBEXECDIR=/modules")))
    (inputs
     `(("httpd" ,httpd)
       ("python" ,python-wrapper)))
    (synopsis "Apache HTTPD module for Python WSGI applications")
    (description
     "The mod_wsgi module for the Apache HTTPD Server adds support for running
applications that support the Python @acronym{WSGI, Web Server Gateway
Interface} specification.")
    (license l:asl2.0)
    (home-page "https://modwsgi.readthedocs.io/")))

(define-public nginx
  (package
    (name "nginx")
    ;; Track the ‘mainline’ branch.  Upstream considers it more reliable than
    ;; ’stable’ and recommends that “in general you deploy the NGINX mainline
    ;; branch at all times” (https://www.nginx.com/blog/nginx-1-6-1-7-released/)
    ;; Consider updating the nginx-documentation package together with this one.
    (version "1.15.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nginx.org/download/nginx-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hxfsz1117r91b9fb5hjddyrf1czvb36lh1z7zalqqdskfcbmkz4"))))
    (build-system gnu-build-system)
    (inputs `(("openssl" ,openssl)
              ("pcre" ,pcre)
              ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f                      ; no test target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-/bin/sh
           (lambda _
             (substitute* "auto/feature"
               (("/bin/sh") (which "sh")))
             #t))
         (replace 'configure
           ;; The configure script is hand-written, not from GNU autotools.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((flags
                    (list (string-append "--prefix=" (assoc-ref outputs "out"))
                          "--with-http_ssl_module"
                          "--with-http_v2_module"
                          "--with-pcre-jit"
                          "--with-debug"
                          ;; Even when not cross-building, we pass the
                          ;; --crossbuild option to avoid customizing for the
                          ;; kernel version on the build machine.
                          ,(let ((system "Linux")    ; uname -s
                                 (release "3.2.0")   ; uname -r
                                 ;; uname -m
                                 (machine (match (or (%current-target-system)
                                                     (%current-system))
                                            ("x86_64-linux"   "x86_64")
                                            ("i686-linux"     "i686")
                                            ("mips64el-linux" "mips64")
                                            ;; Prevent errors when querying
                                            ;; this package on unsupported
                                            ;; platforms, e.g. when running
                                            ;; "guix package --search="
                                            (_                "UNSUPPORTED"))))
                             (string-append "--crossbuild="
                                            system ":" release ":" machine)))))
               (setenv "CC" "gcc")
               (format #t "environment variable `CC' set to `gcc'~%")
               (format #t "configure flags: ~s~%" flags)
               (apply invoke "./configure" flags)
               #t)))
         (add-after 'install 'install-man-page
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (install-file "objs/nginx.8" (string-append man "/man8"))
               #t)))
         (add-after 'install 'fix-root-dirs
           (lambda* (#:key outputs #:allow-other-keys)
             ;; 'make install' puts things in strange places, so we need to
             ;; clean it up ourselves.
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/nginx")))
               ;; This directory is empty, so get rid of it.
               (rmdir (string-append out "/logs"))
               ;; Example configuration and HTML files belong in
               ;; /share.
               (mkdir-p share)
               (rename-file (string-append out "/conf")
                            (string-append share "/conf"))
               (rename-file (string-append out "/html")
                            (string-append share "/html"))
               #t))))))
    (home-page "https://nginx.org")
    (synopsis "HTTP and reverse proxy server")
    (description
     "Nginx (\"engine X\") is a high-performance web and reverse proxy server
created by Igor Sysoev.  It can be used both as a stand-alone web server
and as a proxy to reduce the load on back-end HTTP or mail servers.")
    ;; Almost all of nginx is distributed under the bsd-2 license.
    ;; The exceptions are:
    ;;   * The 'nginx-http-push' module is covered by the expat license.
    ;;   * The 'nginx-development-kit' module is mostly covered by bsd-3,
    ;;     except for two source files which are bsd-4 licensed.
    (license (list l:bsd-2 l:expat l:bsd-3 l:bsd-4))))

(define nginx-xslscript
  (let ((revision 11)
        (changeset "01dc9ba12e1b"))
    (package
      (name "nginx-xslscript")
      (version
       (simple-format #f "2014-03-31-~A-~A" revision changeset))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "http://hg.nginx.org/xslscript")
                      (changeset changeset)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "0am8zvdx3jmiwkg5q07qjaw5r26r4i2v5i4yr8a1k0jgib6ii08g"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f  ; No test suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out-bin (string-append
                               (assoc-ref outputs "out")
                               "/bin")))
                 (mkdir-p out-bin)
                 (copy-file "xslscript.pl"
                            (string-append
                             out-bin
                             "/xslscript.pl"))
                 #t))))))
      (home-page "http://hg.nginx.org/xslscript")
      (synopsis "XSLScript with NGinx specific modifications")
      (description
       "XSLScript is a terse notation for writing complex XSLT stylesheets.
This is modified version, specifically intended for use with the NGinx
documentation.")
      (license l:bsd-2))))

(define-public nginx-documentation
  ;; This documentation should be relevant for nginx@1.15.9.
  (let ((revision 2345)
        (changeset "7ef11708457e"))
    (package
      (name "nginx-documentation")
      (version
       (simple-format #f "2019-03-01-~A-~A" revision changeset))
      (source
       (origin (method hg-fetch)
               (uri (hg-reference
                     (url "http://hg.nginx.org/nginx.org")
                     (changeset changeset)))
               (file-name (string-append name "-" version))
               (sha256
                (base32
                 "15975jvh53mnsgi4hhgrwdwy3by23v4kxnhy2vnkziq8v7wkmy4y"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f                    ; no test suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (replace 'build
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((output (assoc-ref outputs "out")))
                 (substitute* "umasked.sh"
                   ((" /bin/sh") (string-append " " (which "sh"))))
                 ;; The documentation includes a banner, which makes sense on
                 ;; the NGinx website, but doesn't make much sense when
                 ;; viewing locally. Therefore, modify the CSS to remove the
                 ;; banner.
                 (substitute* "xslt/style.xslt"
                   (("#banner           \\{ background:     black;")
                    "#banner           { display:        none;"))
                 (invoke "make")
                 #t)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((output (assoc-ref outputs "out")))
                 (mkdir-p output)
                 (copy-recursively "libxslt" output)
                 #t))))))
      (native-inputs
       `(("libxml2" ,libxml2)
         ("libxslt" ,libxslt)
         ("nginx-xslscript" ,nginx-xslscript)))
      (home-page "https://nginx.org")
      (synopsis "Documentation for the nginx web server")
      (description
       "This package provides HTML documentation for the nginx web server.")
      (license l:bsd-2))))

(define-public fcgi
  (package
    (name "fcgi")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       ;; Upstream has disappeared.
       (uri (string-append "https://sources.archlinux.org/other/packages/fcgi/"
                           "fcgi-" version ".tar.gz"))
       (sha256
        (base32
         "1f857wnl1d6jfrgfgfpz3zdaj8fch3vr13mnpcpvy8bang34bz36"))
       (patches (search-patches "fcgi-2.4.0-poll.patch"
                                "fcgi-2.4.0-gcc44-fixes.patch"))))
    (build-system gnu-build-system)
    ;; Parallel building is not supported.
    (arguments `(#:parallel-build? #f))
    ;; This is an archived fork of the original home page, www.fastcgi.com.
    (home-page "https://fastcgi-archives.github.io/")
    (synopsis "Language-independent, high-performant extension to CGI")
    (description "FastCGI is a language-independent, scalable extension to CGI
that provides high performance without the limitations of server specific
APIs.")
    ;; This package is released under the Open Market License, a variant of
    ;; the Expat license, incompatible with the GPL.
    (license (l:non-copyleft "file://LICENSE.TERMS"))))

(define-public fcgiwrap
  (package
    (name "fcgiwrap")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gnosek/fcgiwrap.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ryw66h9aazi83amk8l7ha8k5g0c7qvk5g6jv376a1ws9xk2qw6f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests included
       #:make-flags (list "CC=gcc")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("fcgi" ,fcgi)))
    (home-page "https://nginx.localdomain.pl/wiki/FcgiWrap")
    (synopsis "Simple server for running CGI applications over FastCGI")
    (description "Fcgiwrap is a simple server for running CGI applications
over FastCGI.  It hopes to provide clean CGI support to Nginx (and other web
servers that may need it).")
    (license l:expat)))

(define-public starman
  (package
    (name "starman")
    (version "0.4014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Starman-" version ".tar.gz"))
       (sha256
        (base32
         "1sbb5rb3vs82rlh1fjkgkcmj5pj62b4y9si4ihh45sl9m8c2qxx5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-libwww" ,perl-libwww)
       ("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-data-dump" ,perl-data-dump)
       ("perl-http-date" ,perl-http-date)
       ("perl-http-message" ,perl-http-message)
       ("perl-http-parser-xs" ,perl-http-parser-xs)
       ("perl-net-server" ,perl-net-server)
       ("perl-plack" ,perl-plack)
       ("perl-test-tcp" ,perl-test-tcp)))
    (home-page "https://metacpan.org/release/Starman")
    (synopsis "PSGI/Plack web server")
    (description "Starman is a PSGI perl web server that has unique features
such as high performance, preforking, signal support, superdaemon awareness,
and UNIX socket support.")
    (license l:perl-license)))

(define-public icedtea-web
  (package
    (name "icedtea-web")
    (version "1.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://icedtea.wildebeest.org/download/source/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "004kwrngyxxlrlzby4vzxjr0xcyngcdc9dfgnvi61ffnjr006ryf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list  "--disable-plugin"         ;NPAPI plugins are obsolete nowadays.
             (string-append "BIN_BASH=" (assoc-ref %build-inputs "bash")
                            "/bin/bash")
             (string-append "--with-jdk-home=" (assoc-ref %build-inputs "jdk")))))
    (outputs '("out" "doc"))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("zip" ,zip)))
    (inputs
     `(("gtk+" ,gtk+)
       ("jdk" ,icedtea "jdk")))
    (home-page "http://icedtea.classpath.org/wiki/IcedTea-Web")
    (synopsis "Java Web Start")
    (description
     "IcedTea-Web is an implementation of the @dfn{Java Network Launching
Protocol}, also known as Java Web Start.  This package provides tools and
libraries for working with JNLP applets.")
    ;; The program is mainly GPL2+, with some individual files under LGPL2.1+
    ;; or dual licenses.
    (license l:gpl2+)))

(define-public jansson
  (package
    (name "jansson")
    (version "2.12")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "http://www.digip.org/jansson/releases/jansson-"
                             version ".tar.bz2"))
             (sha256
              (base32
               "1lp1mv8pjp5yziws66cy0dhpcam4bbjqhffk13v4vgdybp674pb4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (home-page "http://www.digip.org/jansson/")
    (synopsis "JSON C library")
    (description
     "Jansson is a C library for encoding, decoding and manipulating JSON
data.")
    (license l:expat)))

(define-public json-c
  (package
    (name "json-c")
    (version "0.13.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://s3.amazonaws.com/json-c_releases/releases/json-c-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0ws8dz9nk8q2c0gbf66kg2r6mrkl7kamd3gpdv9zsyrz9n6n0zmq"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; Somehow 'config.h.in' is older than
                 ;; 'aclocal.m4', which would trigger a rule to
                 ;; run 'autoheader'.
                 (set-file-time "config.h.in"
                                (stat "aclocal.m4"))
                 #t))))
    (build-system gnu-build-system)
    (home-page "https://github.com/json-c/json-c/wiki")
    (synopsis "JSON implementation in C")
    (description
     "JSON-C implements a reference counting object model that allows you to
easily construct JSON objects in C, output them as JSON-formatted strings and
parse JSON-formatted strings back into the C representation of JSON objects.
It aims to conform to RFC 7159.")
    (license l:x11)))

;; TODO: remove this old version when all dependents have been updated.
(define-public json-c-0.12
  (package
    (inherit json-c)
    (version "0.12.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://s3.amazonaws.com/json-c_releases/releases/json-c-"
                   version ".tar.gz"))
             (sha256
              (base32 "08qibrq29a5v7g23wi5icy6l4fbfw90h9ccps6vq0bcklx8n84ra"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; Somehow 'config.h.in' is older than
                 ;; 'aclocal.m4', which would trigger a rule to
                 ;; run 'autoheader'.
                 (set-file-time "config.h.in"
                                (stat "aclocal.m4"))

                 ;; Don't try to build with -Werror.
                 (substitute* (find-files "." "Makefile\\.in")
                   (("-Werror") ""))
                 #t))))))

(define-public json-parser
  (package
    (name "json-parser")
    (version "1.1.0")
    (source (origin
              ;; do not use auto-generated tarballs
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/udp/json-parser.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ls7z4fx0sq633s5bc0j1gh36sv087gmrgr7rza22wjq2d4606yf"))))
    ;; FIXME: we should build the python bindings in a separate package
    (build-system gnu-build-system)
    ;; the tests are written for the python bindings which are not built here
    (arguments '(#:tests? #f))
    (home-page "https://github.com/udp/json-parser")
    (synopsis "JSON parser written in ANSI C")
    (description "This package provides a very low footprint JSON parser
written in portable ANSI C.

@itemize
@item BSD licensed with no dependencies (i.e. just drop the C file into your
project)
@item Never recurses or allocates more memory than it needs
@item Very simple API with operator sugar for C++
@end itemize")
    (license l:bsd-2)))

(define-public qjson
  (package
    (name "qjson")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flavio/qjson.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f4wnxzx0qdmxzc7hqk28m0sva7z9p9xmxm6aifvjlp0ha6pmfxs"))))
    (build-system cmake-build-system)
    (arguments
     ;; The tests require a running X server.
     `(#:configure-flags '("-DQJSON_BUILD_TESTS=ON"
                           "-DCMAKE_CXX_FLAGS=-std=gnu++11 -fPIC")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-broken-test
           (lambda _
             ;; FIXME: One test fails.  See
             ;; https://github.com/flavio/qjson/issues/105
             (substitute* "tests/scanner/testscanner.cpp"
               (("QTest::newRow\\(\"too large exponential\"\\)" line)
                (string-append "//" line)))
             #t))
         (add-before 'check 'render-offscreen
           (lambda _ (setenv "QT_QPA_PLATFORM" "offscreen") #t)))))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "http://qjson.sourceforge.net")
    (synopsis "Library that maps JSON data to QVariant objects")
    (description "QJson is a Qt-based library that maps JSON data to
@code{QVariant} objects.  JSON arrays will be mapped to @code{QVariantList}
instances, while JSON's objects will be mapped to @code{QVariantMap}.")
    ;; Only version 2.1 of the license
    (license l:lgpl2.1)))

(define-public krona-tools
  (package
   (name "krona-tools")
   (version "2.7")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/marbl/Krona/releases/download/v"
                   version "/KronaTools-" version ".tar"))
             (sha256
              (base32
               "0wvgllcqscsfb4xc09y3fqhx8i38pmr4w55vjs5y79wx56n710iq"))))
   (build-system perl-build-system)
   (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; There is no configure or build steps.
         (delete 'configure)
         (delete 'build)
         ;; Install script "install.pl" expects the build directory to remain
         ;; after installation, creating symlinks etc., so re-implement it
         ;; here.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin   (string-append (assoc-ref outputs "out") "/bin"))
                   (perl  (string-append (assoc-ref outputs "out")
                                         "/lib/perl5/site_perl/krona-tools/lib")))
               (mkdir-p bin)
               (for-each
                (lambda (script)
                  (let* ((executable (string-append "scripts/" script ".pl")))
                    ;; Prefix executables with 'kt' as install script does.
                    (copy-file executable (string-append bin "/kt" script))))
                '("ClassifyBLAST"
                  "GetContigMagnitudes"
                  "GetLCA"
                  "GetTaxIDFromAcc"
                  "GetTaxInfo"
                  "ImportBLAST"
                  "ImportDiskUsage"
                  "ImportEC"
                  "ImportFCP"
                  "ImportGalaxy"
                  "ImportKrona"
                  "ImportMETAREP-BLAST"
                  "ImportMETAREP-EC"
                  "ImportMGRAST"
                  "ImportPhymmBL"
                  "ImportRDP"
                  "ImportRDPComparison"
                  "ImportTaxonomy"
                  "ImportText"
                  "ImportXML"))
               (for-each 
                (lambda (directory)
                  (copy-recursively directory
                                    (string-append perl "/../" directory)))
                (list "data" "img" "taxonomy" "src"))
               (install-file "lib/KronaTools.pm" perl))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (path (getenv "PERL5LIB")))
               (for-each
                (lambda (executable)
                  (wrap-program executable
                    `("PERL5LIB" ":" prefix
                      (,(string-append out "/lib/perl5/site_perl/krona-tools/lib")))))
                (find-files (string-append out "/bin/") ".*")))))
         (delete 'check)
         (add-after 'wrap-program 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (with-directory-excursion "data"
               (invoke (string-append (assoc-ref outputs "out") "/bin/ktImportText")
                       "ec.tsv")))))))
   (inputs
    `(("perl" ,perl)))
   (home-page "https://github.com/marbl/Krona/wiki")
   (synopsis "Hierarchical data exploration with zoomable HTML5 pie charts")
   (description
    "Krona is a flexible tool for exploring the relative proportions of
hierarchical data, such as metagenomic classifications, using a radial,
space-filling display.  It is implemented using HTML5 and JavaScript, allowing
charts to be explored locally or served over the Internet, requiring only a
current version of any major web browser.")
   (license l:bsd-3)))

(define-public rapidjson
  (package
    (name "rapidjson")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Tencent/rapidjson.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jixgb8w97l9gdh3inihz7avz7i770gy2j2irvvlyrq3wi41f5ab"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove code using the problematic JSON license (see
                  ;; <https://www.gnu.org/licenses/license-list.html#JSON>).
                  (delete-file-recursively "bin/jsonchecker")
                  #t))))
    (build-system cmake-build-system)
    (arguments
     (if (string-prefix? "aarch64" (or (%current-target-system)
                                       (%current-system)))
         '(#:phases
           (modify-phases %standard-phases
             (add-after 'unpack 'patch-aarch-march-detection
               (lambda _
                 (substitute* (find-files "." "^CMakeLists\\.txt$")
                   (("native") "armv8-a"))
                 #t))))
         ;; Disable CPU optimization for reproducibility.
         '(#:configure-flags '("-DRAPIDJSON_ENABLE_INSTRUMENTATION_OPT=OFF"))))
    (home-page "https://github.com/Tencent/rapidjson")
    (synopsis "JSON parser/generator for C++ with both SAX/DOM style API")
    (description
     "RapidJSON is a fast JSON parser/generator for C++ with both SAX/DOM
style API.")
    (license l:expat)))

(define-public libyajl
  (package
    (name "libyajl")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lloyd/yajl.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00yj06drb6izcxfxfqlhimlrb089kka0w0x8k27pyzyiq7qzcvml"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-tests
           (lambda _
             (substitute* "test/parsing/run_tests.sh"
               (("`which echo`") (which "echo")))
             #t)))))
    (home-page "https://lloyd.github.io/yajl/")
    (synopsis "C library for parsing JSON")
    (description
     "Yet Another JSON Library (YAJL) is a small event-driven (SAX-style) JSON
parser written in ANSI C and a small validating JSON generator.")
    (license l:isc)))

(define-public libwebsockets
  (package
    (name "libwebsockets")
    (version "1.3")
    (source (origin
              ;; The project does not publish tarballs, so we have to take
              ;; things from Git.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/warmcat/libwebsockets.git")
                    (commit (string-append "v" version
                                           "-chrome37-firefox30"))))
              (sha256
               (base32
                "12fqh2d2098mgf0ls19p9lzibpsqhv7mc5rn1yvrbfnazmcr40g4"))
              (file-name (string-append name "-" version))))

    (build-system cmake-build-system)
    (arguments
     ;; XXX: The thing lacks a 'make test' target, because CMakeLists.txt
     ;; doesn't use 'add_test', and it's unclear how to run the test suite.
     '(#:tests? #f))

    (native-inputs `(("perl" ,perl)))             ; to build the HTML doc
    (inputs `(("zlib" ,zlib)
              ("openssl" ,openssl)))
    (synopsis "WebSockets library written in C")
    (description
     "Libwebsockets is a library that allows C programs to establish client
and server WebSockets connections---a protocol layered above HTTP that allows
for efficient socket-like bidirectional reliable communication channels.")
    (home-page "http://libwebsockets.org/")

    ;; This is LGPLv2.1-only with extra exceptions specified in 'LICENSE'.
    (license l:lgpl2.1)))

(define-public libpsl
  (package
    (name "libpsl")
    (version "0.20.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rockdaboot/libpsl/"
                                  "releases/download/libpsl-" version
                                  "/libpsl-" version ".tar.gz"))
              (sha256
               (base32
                "03sn3fbcrmgl9x2f1gc6rbrdlbrnwbhrnkgi733gqb95cvmhmzgq"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libidn2" ,libidn2)
       ("libunistring" ,libunistring)
       ("python-2" ,python-2)))
    (home-page "https://github.com/rockdaboot/libpsl")
    (synopsis "C library for the Publix Suffix List")
    (description
     "A \"public suffix\" is a domain name under which Internet users can
directly register own names.

Browsers and other web clients can use it to avoid privacy-leaking
\"supercookies\", avoid privacy-leaking \"super domain\" certificates, domain
highlighting parts of the domain in a user interface, and sorting domain lists
by site.

Libpsl has built-in PSL data for fast access, allows to load PSL data from
files, checks if a given domain is a public suffix, provides immediate cookie
domain verification, finds the longest public part of a given domain, finds
the shortest private part of a given domain, works with international
domains (UTF-8 and IDNA2008 Punycode), is thread-safe, and handles IDNA2008
UTS#46.")
    (license l:x11)))

(define-public tidy
  (package
    (name "tidy")
    (version "20091223")
    (source (origin
              (method cvs-fetch)
              (uri (cvs-reference
                    (root-directory
                     ":pserver:anonymous@tidy.cvs.sourceforge.net:/cvsroot/tidy")
                    (module "tidy")
                    (revision "2009-12-23")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "14dsnmirjcrvwsffqp3as70qr6bbfaig2fv3zvs5g7005jrsbvpb"))
              (patches (search-patches "tidy-CVE-2015-5522+5523.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'bootstrap
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; configure.in and Makefile.am aren't in the root of the
                      ;; source tree.
                      (copy-recursively "build/gnuauto" ".")
                      (setenv "AUTOMAKE" "automake --foreign")
                      (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)
       ("libtool" ,libtool)))
    (synopsis "HTML validator and tidier")
    (description "HTML Tidy is a command-line tool and C library that can be
used to validate and fix HTML data.")
    (home-page "http://tidy.sourceforge.net/")
    (license (l:x11-style "file:///include/tidy.h"))))

(define-public tinyproxy
  (package
    (name "tinyproxy")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/tinyproxy/tinyproxy/"
                                  "releases/download/" version "/tinyproxy-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "10jnk6y2swld25mm47mjc0nkffyzsfysnsxwr7cs0ns1kil8ggjr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        ;; For the log file, etc.
        "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
           (lambda* (#:key inputs #:allow-other-keys #:rest args)
             ;; Uncommenting the next two lines may assist in debugging
             ;; (substitute* "docs/man5/Makefile" (("a2x") "a2x -v"))
             ;; (setenv "XML_DEBUG_CATALOG" "1")
             #t)))))
    (home-page "https://tinyproxy.github.io/")
    (synopsis "Light-weight HTTP/HTTPS proxy daemon")
    (description "Tinyproxy is a light-weight HTTP/HTTPS proxy
daemon.  Designed from the ground up to be fast and yet small, it is an ideal
solution for use cases such as embedded deployments where a full featured HTTP
proxy is required, but the system resources for a larger proxy are
unavailable.")
    (license l:gpl2+)))

(define-public polipo
  (package
    (name "polipo")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.pps.univ-paris-diderot.fr/~jch/software/files/polipo/polipo-"
             version ".tar.gz"))
       (sha256
        (base32
         "05g09sg9qkkhnc2mxldm1w1xkxzs2ylybkjzs28w8ydbjc3pand2"))))
    (native-inputs `(("texinfo" ,texinfo)))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)
                            (string-append "LOCAL_ROOT="
                                           out "/share/polipo/www")
                            "CC=gcc"))
       ;; No 'check' target.
       #:tests? #f))
    (home-page "http://www.pps.univ-paris-diderot.fr/~jch/software/polipo/")
    (synopsis "Small caching web proxy")
    (description
     "Polipo is a small caching web proxy (web cache, HTTP proxy, and proxy
server).  It was primarily designed to be used by one person or a small group
of people.")
    (license l:expat)))

(define-public websockify
  (package
    (name "websockify")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/novnc/websockify.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pcic8qs0gdwrfjgfaf893jyddaw97wcjm2mmvwn0xyhmy8mbmw1"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; FIXME: 2 out of 6 tests fail with "ImportError: No module
     ; named 'stubout'". The tests can be run by replacing the check phase with
     ; the command "python setup.py nosetests --verbosity=3".
    (native-inputs `(; Required for tests:
                     ("python-mox3" ,python-mox3)
                     ("python-nose" ,python-nose)))
    (propagated-inputs `(("python-numpy" ,python-numpy)))
    (home-page "https://github.com/novnc/websockify")
    (synopsis "WebSockets support for any application/server")
    (description "Websockify translates WebSockets traffic to normal socket
traffic.  Websockify accepts the WebSockets handshake, parses it, and then
begins forwarding traffic between the client and the target in both
directions.")
    (license l:lgpl3)))

(define-public wwwoffle
  (package
    (name "wwwoffle")
    (version "2.9j")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.gedanken.org.uk/software/"
                                  "wwwoffle/download/wwwoffle-"
                                  version ".tgz"))
              (sha256
               (base32
                "1ihil1xq9dp21hf108khxbw6f3baq0w5c0j3af038y6lkmad4vdi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-gnutls")
       #:tests? #f))                         ; no test target
    (native-inputs `(("flex" ,flex)))
    (inputs `(("gnutls" ,gnutls)
              ("libcrypt" ,libgcrypt)))
    (home-page "https://www.gedanken.org.uk/software/wwwoffle/")
    (synopsis "Caching web proxy optimized for intermittent internet links")
    (description "WWWOFFLE is a proxy web server that is especially good for
intermittent internet links.  It can cache HTTP, HTTPS, FTP, and finger
protocols, and supports browsing and requesting pages while offline, indexing,
modifying pages and incoming and outgoing headers, monitoring pages for
changes, and much more.")
    (license l:gpl2+)))

(define-public liboauth
  (package
    (name "liboauth")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/liboauth/liboauth-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "07w1aq8y8wld43wmbk2q8134p3bfkp2vma78mmsfgw2jn1bh3xhd"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-nss")))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("curl" ,curl)
       ("nss" ,nss)))
    (home-page "https://sourceforge.net/projects/liboauth")
    (synopsis "C library implementing the OAuth API")
    (description
     "liboauth is a collection of C functions implementing the OAuth API.
liboauth provides functions to escape and encode strings according to OAuth
specifications and offers high-level functionality built on top to sign
requests or verify signatures using either NSS or OpenSSL for calculating the
hash/signatures.")
    ;; Source code may be distributed under either license.
    (license (list l:expat l:gpl2+))))

(define-public libyaml
  (package
    (name "libyaml")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://pyyaml.org/download/libyaml/yaml-"
             version ".tar.gz"))
       (sha256
        (base32
         "0a87931cx5m14a1x8rbjix3nz7agrcgndf4h392vm62a4rby9240"))))
    (build-system gnu-build-system)
    (home-page "http://pyyaml.org/wiki/LibYAML")
    (synopsis "YAML 1.1 parser and emitter written in C")
    (description
     "LibYAML is a YAML 1.1 parser and emitter written in C.")
    (license l:expat)))

(define-public libquvi-scripts
  (package
    (name "libquvi-scripts")
    (version "0.4.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/quvi/" (version-major+minor version) "/"
             name "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0d0giry6bb57pnidymvdl7i5x9bq3ljk3g4bs294hcr5mj3cq0kw"))))
    (build-system gnu-build-system)
    (home-page "http://quvi.sourceforge.net/")
    (synopsis "Media stream URL parser")
    (description "This package contains support scripts called by libquvi to
parse media stream properties.")
    (license l:lgpl2.1+)))

(define-public libquvi
  (package
    (name "libquvi")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/quvi/" (version-major+minor version) "/" name "/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "00x9gbmzc5cns0gnfag0hsphcr3cb33vbbb9s7ppvvd6bxz2z1mm"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("cyrus-sasl" ,cyrus-sasl)
       ("libquvi-scripts" ,libquvi-scripts)
       ("lua" ,lua-5.1)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (arguments
     ;; Lua provides no .pc file, so add CFLAGS/LIBS manually.
     '(#:configure-flags
       (let ((lua (assoc-ref %build-inputs "lua")))
         (list
          (string-append "liblua_CFLAGS=-I" lua "/include")
          (string-append "liblua_LIBS=-L" lua "/libs -llua")))))
    (home-page "http://quvi.sourceforge.net/")
    (synopsis "Media stream URL parser")
    (description "libquvi is a library with a C API for parsing media stream
URLs and extracting their actual media files.")
    (license l:lgpl2.1+)))

(define-public quvi
  (package
    (name "quvi")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/" name "/"  (version-major+minor version)
             "/" name "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "09lhl6dv5zpryasx7yjslfrcdcqlsbwapvd5lg7w6sm5x5n3k8ci"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("libquvi" ,libquvi)))
    (home-page "http://quvi.sourceforge.net/")
    (synopsis "Media stream URL parser")
    (description "quvi is a command-line-tool suite to extract media files
from streaming URLs.  It is a command-line wrapper for the libquvi library.")
    (license l:lgpl2.1+)))

(define-public serf
  (package
    (name "serf")
    (version "1.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.apache.org/dist/serf/serf-"
                           version ".tar.bz2"))
       (sha256
        (base32 "1k47gbgpp52049andr28y28nbwh9m36bbb0g8p0aka3pqlhjv72l"))))
    (build-system scons-build-system)
    (propagated-inputs
     `(("apr" ,apr)
       ("apr-util" ,apr-util)
       ("openssl" ,openssl)))
    (inputs
     `(;; TODO: Fix build with gss.
       ;;("gss" ,gss)
       ("zlib" ,zlib)))
    (arguments
     `(#:scons ,scons-python2
       #:scons-flags (list (string-append "APR=" (assoc-ref %build-inputs "apr"))
                           (string-append "APU=" (assoc-ref %build-inputs "apr-util"))
                           (string-append "OPENSSL=" (assoc-ref %build-inputs "openssl"))
                           ;; (string-append "GSSAPI=" (assoc-ref %build-inputs "gss"))
                           (string-append "ZLIB=" (assoc-ref %build-inputs "zlib"))
                           (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scons-propagate-environment
                    (lambda _
                      ;; By design, SCons does not, by default, propagate
                      ;; environment variables to subprocesses.  See:
                      ;; <http://comments.gmane.org/gmane.linux.distributions.nixos/4969>
                      ;; Here, we modify the SConstruct file to arrange for
                      ;; environment variables to be propagated.
                      (substitute* "SConstruct"
                        (("^env = Environment\\(")
                         "env = Environment(ENV=os.environ, "))))
         (add-before 'check 'disable-broken-tests
           (lambda _
             ;; These tests rely on SSL certificates that expired 2017-04-18.
             ;; While there are newer certs available upstream, we don't want
             ;; this package to suddenly "expire" some time in the future.
             ;; https://bugs.gnu.org/26671
             (let ((broken-tests
                    '("test_ssl_trust_rootca"
                      "test_ssl_certificate_chain_with_anchor"
                      "test_ssl_certificate_chain_all_from_server"
                      "test_ssl_no_servercert_callback_allok"
                      "test_ssl_large_response"
                      "test_ssl_large_request"
                      "test_ssl_client_certificate"
                      "test_ssl_future_server_cert"
                      "test_setup_ssltunnel"
                      "test_ssltunnel_basic_auth"
                      "test_ssltunnel_basic_auth_server_has_keepalive_off"
                      "test_ssltunnel_basic_auth_proxy_has_keepalive_off"
                      "test_ssltunnel_basic_auth_proxy_close_conn_on_200resp"
                      "test_ssltunnel_digest_auth")))
               (for-each
                (lambda (test)
                  (substitute* "test/test_context.c"
                    (((string-append "SUITE_ADD_TEST\\(suite, " test "\\);")) "")))
                broken-tests)
               #t))))))
    (home-page "https://serf.apache.org/")
    (synopsis "High-performance asynchronous HTTP client library")
    (description
     "serf is a C-based HTTP client library built upon the Apache Portable
Runtime (APR) library.  It multiplexes connections, running the read/write
communication asynchronously.  Memory copies and transformations are kept to a
minimum to provide high performance operation.")
    ;; Most of the code is covered by the Apache License, Version 2.0, but the
    ;; bundled CuTest framework uses a different non-copyleft license.
    (license (list l:asl2.0 (l:non-copyleft "file://test/CuTest-README.txt")))))

(define-public libsass
  (package
    (name "libsass")
    (version "3.5.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sass/libsass.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0830pjcvhzxh6yixj82x5k5r1xnadjqzi16kp53213icbly0r9ma"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'set-LIBSASS_VERSION
           (lambda _
             (setenv "LIBSASS_VERSION" ,version)
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://sass-lang.com/libsass")
    (synopsis "SASS Compiler, implemented as a C/C++ library")
    (description
     "LibSass is a @acronym{SASS,Syntactically awesome style sheets} compiler
library designed for portability and efficiency.  To actually compile SASS
stylesheets, you'll need to use another program that uses this library,
@var{sassc} for example.")
    (license l:expat)))

(define-public sassc
  (package
    (name "sassc")
    (version "3.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sass/sassc.git")
                    (commit  version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jsfz1zg4gwk0dq8i92ll12axs3s70wsdsmdyi71zx8zmvib5nl6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       ;; I don't believe sassc contains any tests
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Makefile
           (lambda _
             (substitute* "Makefile"
               (("build-shared: \\$\\(RESOURCES\\) \\$\\(OBJECTS\\) \\$\\(LIB_SHARED\\)")
                "build-shared: $(RESOURCES) $(OBJECTS)")
               (("\\$\\(SASSC_EXE\\): libsass build")
                "$(SASSC_EXE): build")
               (("install: libsass-install-\\$\\(BUILD\\) \\\\")
                "install: \\"))
             #t))
         ;; This phase fails for some reason
         (delete 'bootstrap)
         ;; There is no configure script
         (delete 'configure)
         (add-before 'build 'setup-environment
           (lambda _
             (setenv "BUILD" "shared")
             (setenv "SASSC_VERSION" ,version)
             #t)))))
    (inputs
     `(("libsass" ,libsass)))
    (synopsis "CSS pre-processor")
    (description "SassC is a compiler written in C for the CSS pre-processor
language known as SASS.")
    (home-page "http://sass-lang.com/libsass")
    (license l:expat)))


(define-public perl-apache-logformat-compiler
  (package
    (name "perl-apache-logformat-compiler")
    (version "0.35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KA/KAZEBURO/"
                           "Apache-LogFormat-Compiler-" version ".tar.gz"))
       (sha256
        (base32 "06i70ydxk2wa2rcqn16842kra2qz3jwk0vk1abq8lah4180c0m0n"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-http-message" ,perl-http-message)
       ("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-mocktime" ,perl-test-mocktime)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-uri" ,perl-uri)))
    (propagated-inputs
     `(("perl-posix-strftime-compiler" ,perl-posix-strftime-compiler)))
    (arguments `(#:tests? #f))          ; TODO: Timezone test failures
    (home-page "https://metacpan.org/release/Apache-LogFormat-Compiler")
    (synopsis "Compile a log format string to perl-code")
    (description "This module provides methods to compile a log format string
to perl-code, for faster generation of access_log lines.")
    (license l:perl-license)))

(define-public perl-authen-sasl
  (package
    (name "perl-authen-sasl")
    (version "2.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GB/GBARR/"
                           "Authen-SASL-" version ".tar.gz"))
       (sha256
        (base32
         "02afhlrdq5hh5g8b32fa79fqq5i76qzwfqqvfi9zi57h31szl536"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           ;; Fix the build with Perl 5.26.0. Try removing this phase for later
           ;; versions of perl-authen-sasl.
           (lambda _ (setenv "PERL_USE_UNSAFE_INC" "1") #t)))))
    (propagated-inputs
     `(("perl-digest-hmac" ,perl-digest-hmac)
       ("perl-gssapi" ,perl-gssapi)))
    (home-page "https://metacpan.org/release/Authen-SASL")
    (synopsis "SASL authentication framework")
    (description "Authen::SASL provides an SASL authentication framework.")
    (license l:perl-license)))

(define-public perl-catalyst-action-renderview
  (package
    (name "perl-catalyst-action-renderview")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-Action-RenderView-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0j1rrld13cjk7ks92b5hv3xw4rfm2lvmksb4rlzd8mx0a0wj0rc5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-http-request-ascgi" ,perl-http-request-ascgi)
       ("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-data-visitor" ,perl-data-visitor)
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "https://metacpan.org/release/Catalyst-Action-RenderView")
    (synopsis "Sensible default Catalyst action")
    (description "This Catalyst action implements a sensible default end
action, which will forward to the first available view.")
    (license l:perl-license)))

(define-public perl-catalyst-action-rest
  (package
    (name "perl-catalyst-action-rest")
    (version "1.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/J/JJ/JJNAPIORK/"
                                  "Catalyst-Action-REST-" version ".tar.gz"))
              (sha256
               (base32
                "086bykggzalbjfk0islac4b48g9s2ypj7y81d6ns1lq0aax1py6c"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-requires" ,perl-test-requires)
       ("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-class-inspector" ,perl-class-inspector)
       ("perl-config-general" ,perl-config-general)
       ("perl-cpanel-json-xs" ,perl-cpanel-json-xs)
       ("perl-libwww" ,perl-libwww)
       ("perl-moose" ,perl-moose)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("perl-params-validate" ,perl-params-validate)
       ("perl-uri-find" ,perl-uri-find)
       ("perl-xml-simple" ,perl-xml-simple)))
    (home-page "https://metacpan.org/release/Catalyst-Action-REST")
    (synopsis "Automated REST Method Dispatching")
    (description "This Action handles doing automatic method dispatching for
REST requests.  It takes a normal Catalyst action, and changes the dispatch to
append an underscore and method name.  First it will try dispatching to an
action with the generated name, and failing that it will try to dispatch to a
regular method.")
    (license l:perl-license)))

(define-public perl-catalyst-authentication-store-dbix-class
  (package
    (name "perl-catalyst-authentication-store-dbix-class")
    (version "0.1506")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "Catalyst-Authentication-Store-DBIx-Class-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0i5ja7690fs9nhxcij6lw51j804sm8s06m5mvk1n8pi8jljrymvw"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-catalyst-plugin-authorization-roles"
        ,perl-catalyst-plugin-authorization-roles)
       ("perl-catalyst-plugin-session-state-cookie"
        ,perl-catalyst-plugin-session-state-cookie)
       ("perl-dbd-sqlite" ,perl-dbd-sqlite)
       ("perl-module-install" ,perl-module-install)
       ("perl-test-www-mechanize-catalyst" ,perl-test-www-mechanize-catalyst)))
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-catalyst-plugin-authentication"
        ,perl-catalyst-plugin-authentication)
       ("perl-dbix-class" ,perl-dbix-class)
       ("perl-catalyst-model-dbic-schema" ,perl-catalyst-model-dbic-schema)))
    (home-page
     "https://metacpan.org/release/Catalyst-Authentication-Store-DBIx-Class")
    (synopsis "Storage class for Catalyst authentication using DBIx::Class")
    (description "The Catalyst::Authentication::Store::DBIx::Class class
provides access to authentication information stored in a database via
DBIx::Class.")
    (license l:perl-license)))

(define-public perl-catalyst-component-instancepercontext
  (package
    (name "perl-catalyst-component-instancepercontext")
    (version "0.001001")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GR/GRODITI/"
                           "Catalyst-Component-InstancePerContext-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0wfj4vnn2cvk6jh62amwlg050p37fcwdgrn9amcz24z6w4qgjqvz"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-moose" ,perl-moose)))
    (home-page
     "https://metacpan.org/release/Catalyst-Component-InstancePerContext")
    (synopsis "Create only one instance of Moose component per context")
    (description "Catalyst::Component::InstancePerContext returns a new
instance of a component on each request.")
    (license l:perl-license)))

(define-public perl-catalyst-devel
  (package
    (name "perl-catalyst-devel")
    (version "1.39")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "Catalyst-Devel-" version ".tar.gz"))
       (sha256
        (base32
         "12m50bbkggjmpxihv3wnvr0g2qng0zwhlzi5ygppjz8wh2x73qxw"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-catalyst-action-renderview" ,perl-catalyst-action-renderview)
       ("perl-catalyst-plugin-configloader" ,perl-catalyst-plugin-configloader)
       ("perl-catalyst-plugin-static-simple" ,perl-catalyst-plugin-static-simple)
       ("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-config-general" ,perl-config-general)
       ("perl-file-changenotify" ,perl-file-changenotify)
       ("perl-file-copy-recursive" ,perl-file-copy-recursive)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-module-install" ,perl-module-install)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-emulate-class-accessor-fast"
        ,perl-moosex-emulate-class-accessor-fast)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-path-class" ,perl-path-class)
       ("perl-template-toolkit" ,perl-template-toolkit)))
    (home-page "https://metacpan.org/release/Catalyst-Devel")
    (synopsis "Catalyst Development Tools")
    (description "The Catalyst-Devel distribution includes a variety of
modules useful for the development of Catalyst applications, but not required
to run them.  Catalyst-Devel includes the Catalyst::Helper system, which
autogenerates scripts and tests; Module::Install::Catalyst, a Module::Install
extension for Catalyst; and requirements for a variety of development-related
modules.")
    (license l:perl-license)))

(define-public perl-catalyst-dispatchtype-regex
  (package
    (name "perl-catalyst-dispatchtype-regex")
    (version "5.90035")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MG/MGRIMES/"
                           "Catalyst-DispatchType-Regex-" version ".tar.gz"))
       (sha256
        (base32
         "06jq1lmpq88rmp9zik5gqczg234xac0hiyc3l698iif7zsgcyb80"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build) ;needs Module::Build >= 0.4004
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("perl-catalyst-runtime" ,perl-catalyst-runtime)))
    (propagated-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-text-simpletable" ,perl-text-simpletable)))
    (home-page "https://metacpan.org/release/Catalyst-DispatchType-Regex")
    (synopsis "Regex DispatchType for Catalyst")
    (description "Dispatch type managing path-matching behaviour using
regexes.  Regex dispatch types have been deprecated and removed from Catalyst
core.  It is recommend that you use Chained methods or other techniques
instead.  As part of the refactoring, the dispatch priority of Regex vs Regexp
vs LocalRegex vs LocalRegexp may have changed.  Priority is now influenced by
when the dispatch type is first seen in your application.")
    (license l:perl-license)))

(define-public perl-catalyst-model-dbic-schema
  (package
  (name "perl-catalyst-model-dbic-schema")
  (version "0.65")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://cpan/authors/id/G/GB/GBJK/"
                          "Catalyst-Model-DBIC-Schema-"
                          version ".tar.gz"))
      (sha256
        (base32
          "1spfjcjc0b9dv3k2gbanqj1m1cqzyxb32p76dhdwizzpbvpi3a96"))))
  (build-system perl-build-system)
  (native-inputs
   `(("perl-dbd-sqlite" ,perl-dbd-sqlite)
     ("perl-module-install" ,perl-module-install)
     ("perl-test-exception" ,perl-test-exception)
     ("perl-test-requires" ,perl-test-requires)))
  (propagated-inputs
   `(("perl-carp-clan" ,perl-carp-clan)
     ("perl-catalyst-component-instancepercontext"
      ,perl-catalyst-component-instancepercontext)
     ("perl-catalyst-runtime" ,perl-catalyst-runtime)
     ("perl-catalystx-component-traits" ,perl-catalystx-component-traits)
     ("perl-dbix-class" ,perl-dbix-class)
     ("perl-dbix-class-cursor-cached" ,perl-dbix-class-cursor-cached)
     ("perl-dbix-class-schema-loader" ,perl-dbix-class-schema-loader)
     ("perl-hash-merge" ,perl-hash-merge)
     ("perl-list-moreutils" ,perl-list-moreutils)
     ("perl-module-runtime" ,perl-module-runtime)
     ("perl-moose" ,perl-moose)
     ("perl-moosex-markasmethods" ,perl-moosex-markasmethods)
     ("perl-moosex-nonmoose" ,perl-moosex-nonmoose)
     ("perl-moosex-types" ,perl-moosex-types)
     ("perl-moosex-types-loadableclass" ,perl-moosex-types-loadableclass)
     ("perl-namespace-autoclean" ,perl-namespace-autoclean)
     ("perl-namespace-clean" ,perl-namespace-clean)
     ("perl-tie-ixhash" ,perl-tie-ixhash)
     ("perl-try-tiny" ,perl-try-tiny)))
  (home-page "https://metacpan.org/release/Catalyst-Model-DBIC-Schema")
  (synopsis "DBIx::Class::Schema Model Class")
  (description "This is a Catalyst Model for DBIx::Class::Schema-based
Models.")
  (license l:perl-license)))

(define-public perl-catalyst-plugin-accesslog
  (package
    (name "perl-catalyst-plugin-accesslog")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AR/ARODLAND/"
                           "Catalyst-Plugin-AccessLog-" version ".tar.gz"))
       (sha256
        (base32
         "0811rj45q4v2y8wka3wb9d5m4vbyhcmkvddf2wz4x69awzjbhgc7"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-datetime" ,perl-datetime)
       ("perl-moose" ,perl-moose)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-AccessLog")
    (synopsis "Request logging from within Catalyst")
    (description "This Catalyst plugin enables you to create \"access logs\"
from within a Catalyst application instead of requiring a webserver to do it
for you.  It will work even with Catalyst debug logging turned off.")
    (license l:perl-license)))

(define-public perl-catalyst-plugin-authentication
  (package
    (name "perl-catalyst-plugin-authentication")
    (version "0.10023")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-Plugin-Authentication-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0v6hb4r1wv3djrnqvnjcn3xx1scgqzx8nyjdg9lfc1ybvamrl0rn"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-catalyst-plugin-session" ,perl-catalyst-plugin-session)
       ("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-class-inspector" ,perl-class-inspector)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-emulate-class-accessor-fast"
        ,perl-moosex-emulate-class-accessor-fast)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("perl-string-rewriteprefix" ,perl-string-rewriteprefix)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-Authentication")
    (synopsis "Infrastructure plugin for the Catalyst authentication framework")
    (description "The authentication plugin provides generic user support for
Catalyst apps.  It is the basis for both authentication (checking the user is
who they claim to be), and authorization (allowing the user to do what the
system authorises them to do).")
    (license l:perl-license)))

(define-public perl-catalyst-plugin-authorization-roles
  (package
    (name "perl-catalyst-plugin-authorization-roles")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-Plugin-Authorization-Roles-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0l83lkwmq0lngwh8b1rv3r719pn8w1gdbyhjqm74rnd0wbjl8h7f"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-catalyst-plugin-authentication"
        ,perl-catalyst-plugin-authentication)
       ("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-set-object" ,perl-set-object)
       ("perl-universal-isa" ,perl-universal-isa)))
    (home-page
     "https://metacpan.org/release/Catalyst-Plugin-Authorization-Roles")
    (synopsis "Role-based authorization for Catalyst")
    (description "Catalyst::Plugin::Authorization::Roles provides role-based
authorization for Catalyst based on Catalyst::Plugin::Authentication.")
    (license l:perl-license)))

(define-public perl-catalyst-plugin-captcha
  (package
    (name "perl-catalyst-plugin-captcha")
    (version "0.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DI/DIEGOK/"
                           "Catalyst-Plugin-Captcha-" version ".tar.gz"))
       (sha256
        (base32
         "0llyj3v5nx9cx46jdbbvxf1lc9s9cxq5ml22xmx3wkb201r5qgaa"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-catalyst-plugin-session" ,perl-catalyst-plugin-session)
       ("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-gd-securityimage" ,perl-gd-securityimage)
       ("perl-http-date" ,perl-http-date)))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-Captcha")
    (synopsis "Captchas for Catalyst")
    (description "This plugin creates and validates Captcha images for
Catalyst.")
    (license l:perl-license)))

(define-public perl-catalyst-plugin-configloader
  (package
    (name "perl-catalyst-plugin-configloader")
    (version "0.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-Plugin-ConfigLoader-"
                           version ".tar.gz"))
       (sha256
        (base32
         "19j7p4v7mbx6wrmpvmrnd974apx7hdl2s095ga3b9zcbdrl77h5q"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-path-class" ,perl-path-class)
       ("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-config-any" ,perl-config-any)
       ("perl-data-visitor" ,perl-data-visitor)
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-ConfigLoader")
    (synopsis "Load config files of various types")
    (description "This module will attempt to load find and load configuration
files of various types.  Currently it supports YAML, JSON, XML, INI and Perl
formats.")
    (license l:perl-license)))

(define-public perl-catalyst-plugin-session
  (package
    (name "perl-catalyst-plugin-session")
    (version "0.41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JJ/JJNAPIORK/"
                           "Catalyst-Plugin-Session-" version ".tar.gz"))
       (sha256
        (base32 "0a451997zc2vjx7rvndgx1ldbrpic8sfbddyvncynh0zr8bhlqc5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-emulate-class-accessor-fast"
        ,perl-moosex-emulate-class-accessor-fast)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-object-signature" ,perl-object-signature)
       ("perl-test-www-mechanize-psgi" ,perl-test-www-mechanize-psgi)))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-Session")
    (synopsis "Catalyst generic session plugin")
    (description "This plugin links the two pieces required for session
management in web applications together: the state, and the store.")
    (license l:perl-license)))

(define-public perl-catalyst-plugin-session-state-cookie
  (package
    (name "perl-catalyst-plugin-session-state-cookie")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MS/MSTROUT/"
                           "Catalyst-Plugin-Session-State-Cookie-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1rvxbfnpf9x2pc2zgpazlcgdlr2dijmxgmcs0m5nazs0w6xikssb"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-catalyst-plugin-session" ,perl-catalyst-plugin-session)
       ("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-moose" ,perl-moose)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page
     "https://metacpan.org/release/Catalyst-Plugin-Session-State-Cookie")
    (synopsis "Maintain session IDs using cookies")
    (description "In order for Catalyst::Plugin::Session to work, the session
ID needs to be stored on the client, and the session data needs to be stored
on the server.  This plugin stores the session ID on the client using the
cookie mechanism.")
    (license l:perl-license)))

(define-public perl-catalyst-plugin-session-store-fastmmap
  (package
    (name "perl-catalyst-plugin-session-store-fastmmap")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-Plugin-Session-Store-FastMmap-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0x3j6zv3wr41jlwr6yb2jpmcx019ibyn11y8653ffnwhpzbpzsxs"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-cache-fastmmap" ,perl-cache-fastmmap)
       ("perl-catalyst-plugin-session" ,perl-catalyst-plugin-session)
       ("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-moosex-emulate-class-accessor-fast"
        ,perl-moosex-emulate-class-accessor-fast)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-path-class" ,perl-path-class)))
    (home-page
     "https://metacpan.org/release/Catalyst-Plugin-Session-Store-FastMmap")
    (synopsis "FastMmap session storage backend")
    (description "Catalyst::Plugin::Session::Store::FastMmap is a fast session
storage plugin for Catalyst that uses an mmap'ed file to act as a shared
memory interprocess cache.  It is based on Cache::FastMmap.")
    (license l:perl-license)))

(define-public perl-catalyst-plugin-stacktrace
  (package
    (name "perl-catalyst-plugin-stacktrace")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-Plugin-StackTrace-" version ".tar.gz"))
       (sha256
        (base32
         "1b2ksz74cpigxqzf63rddar3vfmnbpwpdcbs11v0ml89pb8ar79j"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-devel-stacktrace" ,perl-devel-stacktrace)
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-StackTrace")
    (synopsis "Stack trace on the Catalyst debug screen")
    (description "This plugin enhances the standard Catalyst debug screen by
including a stack trace of your application up to the point where the error
occurred.  Each stack frame is displayed along with the package name, line
number, file name, and code context surrounding the line number.")
    (license l:perl-license)))

(define-public perl-catalyst-plugin-static-simple
  (package
    (name "perl-catalyst-plugin-static-simple")
    (version "0.36")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "Catalyst-Plugin-Static-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "0m4l627p2fvzr4i6sgdxhdvsx4wpa6qmaibsbxlg5x5yjs7k7drn"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-mime-types" ,perl-mime-types)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-types" ,perl-moosex-types)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-Static-Simple")
    (synopsis "Simple serving of static pages")
    (description "The Static::Simple plugin is designed to make serving static
content in your application during development quick and easy, without
requiring a single line of code from you.  This plugin detects static files by
looking at the file extension in the URL (such as .css or .png or .js).  The
plugin uses the lightweight MIME::Types module to map file extensions to
IANA-registered MIME types, and will serve your static files with the correct
MIME type directly to the browser, without being processed through Catalyst.")
    (license l:perl-license)))

(define-public perl-catalyst-runtime
  (package
    (name "perl-catalyst-runtime")
    (version "5.90119")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Catalyst-Runtime-" version ".tar.gz"))
       (sha256
        (base32
         "1iw7x9rqk3sz2hm1bw01blz5vwm7zlljdf4xj3r8vz54f1yggzqr"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-cgi-simple" ,perl-cgi-simple)
       ("perl-cgi-struct" ,perl-cgi-struct)
       ("perl-class-c3-adopt-next" ,perl-class-c3-adopt-next)
       ("perl-class-date" ,perl-class-date)
       ("perl-class-load" ,perl-class-load)
       ("perl-data-dump" ,perl-data-dump)
       ("perl-http-body" ,perl-http-body)
       ("perl-http-message" ,perl-http-message)
       ("perl-json-maybexs" ,perl-json-maybexs)
       ("perl-libwww" ,perl-libwww)
       ("perl-module-pluggable" ,perl-module-pluggable)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-emulate-class-accessor-fast"
        ,perl-moosex-emulate-class-accessor-fast)
       ("perl-moosex-getopt" ,perl-moosex-getopt)
       ("perl-moosex-methodattributes" ,perl-moosex-methodattributes)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-path-class" ,perl-path-class)
       ("perl-plack" ,perl-plack)
       ("perl-plack-middleware-fixmissingbodyinredirect"
        ,perl-plack-middleware-fixmissingbodyinredirect)
       ("perl-plack-middleware-methodoverride"
        ,perl-plack-middleware-methodoverride)
       ("perl-plack-middleware-removeredundantbody"
        ,perl-plack-middleware-removeredundantbody)
       ("perl-plack-middleware-reverseproxy"
        ,perl-plack-middleware-reverseproxy)
       ("perl-plack-test-externalserver" ,perl-plack-test-externalserver)
       ("perl-safe-isa" ,perl-safe-isa)
       ("perl-string-rewriteprefix" ,perl-string-rewriteprefix)
       ("perl-text-simpletable" ,perl-text-simpletable)
       ("perl-tree-simple" ,perl-tree-simple)
       ("perl-tree-simple-visitorfactory" ,perl-tree-simple-visitorfactory)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-uri" ,perl-uri)
       ("perl-uri-ws" ,perl-uri-ws)))
    (home-page "https://metacpan.org/release/Catalyst-Runtime")
    (synopsis "The Catalyst Framework Runtime")
    (description "Catalyst is a modern framework for making web applications.
It is designed to make it easy to manage the various tasks you need to do to
run an application on the web, either by doing them itself, or by letting you
\"plug in\" existing Perl modules that do what you need.")
    (license l:perl-license)))

(define-public perl-catalyst-traitfor-request-proxybase
  (package
    (name "perl-catalyst-traitfor-request-proxybase")
    (version "0.000005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-TraitFor-Request-ProxyBase-"
                           version ".tar.gz"))
       (sha256
        (base32
         "02kir63d5cs2ipj3fn1qlmmx3gqi1xqzrxfr4pv5vjhjgsm0zgx7"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-catalystx-roleapplicator" ,perl-catalystx-roleapplicator)
       ("perl-http-message" ,perl-http-message)
       ("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("perl-uri" ,perl-uri)))
    (home-page
     "https://metacpan.org/release/Catalyst-TraitFor-Request-ProxyBase")
    (synopsis "Replace request base with value passed by HTTP proxy")
    (description "This module is a Moose::Role which allows you more
flexibility in your application's deployment configurations when deployed
behind a proxy.  Using this module, the request base ($c->req->base) is
replaced with the contents of the X-Request-Base header.")
    (license l:perl-license)))

(define-public perl-catalyst-view-download
  (package
    (name "perl-catalyst-view-download")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GA/GAUDEON/"
                           "Catalyst-View-Download-" version ".tar.gz"))
       (sha256
        (base32
         "1qgq6y9iwfbhbkbgpw9czang2ami6z8jk1zlagrzdisy4igqzkvs"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-module-install" ,perl-module-install)
       ("perl-test-simple" ,perl-test-simple)
       ("perl-test-www-mechanize-catalyst" ,perl-test-www-mechanize-catalyst)
       ("perl-text-csv" ,perl-text-csv)
       ("perl-xml-simple" ,perl-xml-simple)))
    (home-page "https://metacpan.org/release/Catalyst-View-Download")
    (synopsis "Download data in many formats")
    (description "The purpose of this module is to provide a method for
downloading data into many supportable formats.  For example, downloading a
table based report in a variety of formats (CSV, HTML, etc.).")
    (license l:perl-license)))

(define-public perl-catalyst-view-json
  (package
    (name "perl-catalyst-view-json")
    (version "0.36")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JJ/JJNAPIORK/"
                           "Catalyst-View-JSON-" version ".tar.gz"))
       (sha256
        (base32
         "0x943j1n2r0zqanyzdrs1xsnn8ayn2wqskn7h144xcqa6v6gcisl"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-yaml" ,perl-yaml)))
    (inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-json-maybexs" ,perl-json-maybexs)
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "https://metacpan.org/release/Catalyst-View-JSON")
    (synopsis "Catalyst JSON view")
    (description "Catalyst::View::JSON is a Catalyst View handler that returns
stash data in JSON format.")
    (license l:perl-license)))

(define-public perl-catalyst-view-tt
  (package
    (name "perl-catalyst-view-tt")
    (version "0.44")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Catalyst-View-TT-" version ".tar.gz"))
     (sha256
      (base32
       "06d1zg4nbb6kcyjbnyxrkf8z4zlscxr8650d94f7187jygfl8rvh"))))
  (build-system perl-build-system)
  (propagated-inputs
   `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
     ("perl-class-accessor" ,perl-class-accessor)
     ("perl-data-dump" ,perl-data-dump)
     ("perl-mro-compat" ,perl-mro-compat)
     ("perl-path-class" ,perl-path-class)
     ("perl-template-timer" ,perl-template-timer)
     ("perl-template-toolkit" ,perl-template-toolkit)))
  (home-page "https://metacpan.org/release/Catalyst-View-TT")
  (synopsis "Template View Class")
  (description "This module is a Catalyst view class for the Template
Toolkit.")
  (license l:perl-license)))

(define-public perl-catalystx-component-traits
  (package
    (name "perl-catalystx-component-traits")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RK/RKITOVER/"
                           "CatalystX-Component-Traits-" version ".tar.gz"))
       (sha256
        (base32
         "0iq4ci8m6g2c4g01fvdl568y7pjz28f3widk986v3pyhr7ll8j88"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-moosex-methodattributes" ,perl-moosex-methodattributes)))
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-class-load" ,perl-class-load)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-traits-pluggable" ,perl-moosex-traits-pluggable)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("perl-list-moreutils" ,perl-list-moreutils)))
    (home-page "https://metacpan.org/release/CatalystX-Component-Traits")
    (synopsis "Trait Loading and Resolution for Catalyst Components")
    (description "Adds a \"COMPONENT\" in Catalyst::Component method to your
Catalyst component base class that reads the optional \"traits\" parameter
from app and component config and instantiates the component subclass with
those traits using \"new_with_traits\" in MooseX::Traits from
MooseX::Traits::Pluggable.")
    (license l:perl-license)))

(define-public perl-catalystx-roleapplicator
  (package
    (name "perl-catalystx-roleapplicator")
    (version "0.005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HD/HDP/"
                           "CatalystX-RoleApplicator-" version ".tar.gz"))
       (sha256
        (base32
         "0vwaapxn8g5hs2xp63c4dwv9jmapmji4272fakssvgc9frklg3p2"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-relatedclassroles" ,perl-moosex-relatedclassroles)))
    (home-page "https://metacpan.org/release/CatalystX-RoleApplicator")
    (synopsis "Apply roles to Catalyst classes")
    (description "CatalystX::RoleApplicator applies roles to Catalyst
application classes.")
    (license l:perl-license)))

(define-public perl-catalystx-script-server-starman
  (package
    (name "perl-catalystx-script-server-starman")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AB/ABRAXXA/"
                           "CatalystX-Script-Server-Starman-"
                           version ".tar.gz"))
       (sha256
        (base32
         "08jvibq4v8xjj0c3cr93h0w8w0c88ajwjn37xjy7ygxl9krlffp6"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-test-www-mechanize-catalyst" ,perl-test-www-mechanize-catalyst)))
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-moose" ,perl-moose)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("starman" ,starman)))
    (home-page "https://metacpan.org/release/CatalystX-Script-Server-Starman")
    (synopsis "Catalyst development server with Starman")
    (description "This module provides a Catalyst extension to replace the
development server with Starman.")
    (license l:perl-license)))

(define-public perl-cgi
  (package
    (name "perl-cgi")
    (version "4.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEEJO/"
                           "CGI-" version ".tar.gz"))
       (sha256
        (base32
         "1m779315rzj4mpgscw209a2wk18iwg2n8zibn8aak4mv56jz8n4c"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-deep" ,perl-test-deep)
       ("perl-test-nowarnings" ,perl-test-nowarnings)
       ("perl-test-warn" ,perl-test-warn)))
    (propagated-inputs
     `(("perl-html-parser" ,perl-html-parser)))
    (home-page "https://metacpan.org/release/CGI")
    (synopsis "Handle Common Gateway Interface requests and responses")
    (description "CGI.pm is a stable, complete and mature solution for
processing and preparing HTTP requests and responses.  Major features include
processing form submissions, file uploads, reading and writing cookies, query
string generation and manipulation, and processing and preparing HTTP
headers.")
    (license l:perl-license)))

(define-public perl-cgi-formbuilder
  (package
    (name "perl-cgi-formbuilder")
    (version "3.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cpan.metacpan.org/authors/id/B/BI/BIGPRESH/"
             "CGI-FormBuilder-" version ".tar.gz"))
       (sha256
        (base32
         "163ixq9kninqq094z2rnkg9pv3bcmvjphlww4vksfrzhq3h9pjdf"))))
    (build-system perl-build-system)
    (inputs `(("perl-cgi" ,perl-cgi)))
    (home-page
     "https://metacpan.org/release/CGI-FormBuilder")
    (synopsis
     "Generate and process stateful forms")
    (description
     "@code{CGI::FormBuilder} provides an easy way to generate and process CGI
form-based applications.")
    (license l:perl-license)))

(define-public perl-cgi-session
  (package
    (name "perl-cgi-session")
    (version "4.48")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MA/MARKSTOS/CGI-Session-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1xsl2pz1jrh127pq0b01yffnj4mnp9nvkp88h5mndrscq9hn8xa6"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (inputs `(("perl-cgi" ,perl-cgi)))
    (home-page
     "https://metacpan.org/release/CGI-Session")
    (synopsis
     "Persistent session data in CGI applications")
    (description
     "@code{CGI::Session} provides modular session management system across
HTTP requests.")
    (license l:perl-license)))

(define-public perl-cgi-simple
  (package
    (name "perl-cgi-simple")
    (version "1.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MA/MANWAR/"
                           "CGI-Simple-" version ".tar.gz"))
       (sha256
        (base32 "1wzc2igs4khmj7zfahvs87c24p9ks8hnqhhsyviyiix53xx2y6sg"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-io-stringy" ,perl-io-stringy) ; for IO::Scalar
       ("perl-module-build" ,perl-module-build)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-test-nowarnings" ,perl-test-nowarnings)))
    (home-page "https://metacpan.org/release/CGI-Simple")
    (synopsis "CGI interface that is CGI.pm compliant")
    (description "CGI::Simple provides a relatively lightweight drop in
replacement for CGI.pm.  It shares an identical OO interface to CGI.pm for
parameter parsing, file upload, cookie handling and header generation.")
    (license l:perl-license)))

(define-public perl-cgi-struct
  (package
    (name "perl-cgi-struct")
    (version "1.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FU/FULLERMD/"
                           "CGI-Struct-" version ".tar.gz"))
       (sha256
        (base32
         "0v4xq2qpryr7i6jngw1wpn8yr2kiib10yxp4aih90vfdznkqsgfi"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-deep" ,perl-test-deep)))
    (home-page "https://metacpan.org/release/CGI-Struct")
    (synopsis "Build structures from CGI data")
    (description "This is a module for building structured data from CGI
inputs, in a manner reminiscent of how PHP does.")
    (license l:bsd-2)))

(define-public perl-datetime-format-http
  (package
    (name "perl-datetime-format-http")
    (version "0.42")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CK/CKRAS/"
                           "DateTime-Format-HTTP-" version ".tar.gz"))
       (sha256
        (base32
         "0h6qqdg1yzqkdxp7hqlp0qa7d1y64nilgimxs79dys2ryjfpcknh"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-http-date" ,perl-http-date)))
    (home-page "https://metacpan.org/release/DateTime-Format-HTTP")
    (synopsis "Date conversion routines")
    (description "This module provides functions that deal with the date
formats used by the HTTP protocol.")
    (license l:perl-license)))

(define-public perl-digest-md5-file
  (package
    (name "perl-digest-md5-file")
    (version "0.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DM/DMUEY/"
                           "Digest-MD5-File-" version ".tar.gz"))
       (sha256
        (base32
         "060jzf45dlwysw5wsm7av1wvpl06xgk415kwwpvv89r6wda3md5d"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-libwww" ,perl-libwww)))
    (home-page "https://metacpan.org/release/Digest-MD5-File")
    (synopsis "MD5 sums for files and urls")
    (description "Digest::MD5::File is a Perl extension for getting MD5 sums
for files and urls.")
    (license l:perl-license)))

(define-public perl-encode-locale
  (package
    (name "perl-encode-locale")
    (version "1.05")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/Encode-Locale-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1h8fvcdg3n20c2yp7107yhdkkx78534s9hnvn7ps8hpmf4ks0vqp"))))
    (build-system perl-build-system)
    (license l:perl-license)
    (synopsis "Perl locale encoding determination")
    (description
     "The POSIX locale system is used to specify both the language
conventions requested by the user and the preferred character set to
consume and output.  The Encode::Locale module looks up the charset and
encoding (called a CODESET in the locale jargon) and arranges for the
Encode module to know this encoding under the name \"locale\".  It means
bytes obtained from the environment can be converted to Unicode strings
by calling Encode::encode(locale => $bytes) and converted back again
with Encode::decode(locale => $string).")
    (home-page "https://metacpan.org/release/Encode-Locale")))

(define-public perl-feed-find
  (package
    (name "perl-feed-find")
    (version "0.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/B/BT/BTROTT/"
                                  "Feed-Find-" version ".tar.gz"))
              (sha256
               (base32
                "0sa33cm8ww55cymnl8j7b5yspi2y5xkkkgqqa4h6fs3wdqylz600"))))
    (build-system perl-build-system)
    (arguments
     ;; Tests expect to query files at http://stupidfool.org/perl/feeds/
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _ (setenv "PERL_USE_UNSAFE_INC" "1"))))))
    (inputs
     `(("perl-class-errorhandler" ,perl-class-errorhandler)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-libwww" ,perl-libwww)
       ("perl-uri" ,perl-uri)))
    (home-page "https://metacpan.org/release/Feed-Find")
    (synopsis "Syndication feed auto-discovery")
    (description "@code{Feed::Find} implements feed auto-discovery for finding
syndication feeds, given a URI.  It will discover the following feed formats:
RSS 0.91, RSS 1.0, RSS 2.0, Atom.")
    (license l:perl-license)))

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
    (license l:perl-license)
    (synopsis "Perl directory listing parser")
    (description
     "The File::Listing module exports a single function called parse_dir(),
which can be used to parse directory listings.")
    (home-page "https://metacpan.org/release/File-Listing")))

(define-public perl-finance-quote
  (package
   (name "perl-finance-quote")
   (version "1.38")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "https://cpan.metacpan.org/authors/id/E/EC/ECOCODE/"
                          "Finance-Quote-" version ".tar.gz"))
      (sha256
       (base32
        "0zhqb27y4vdxn476s2kwm9zl2f970yjcyyybnjm9b406krr2fm59"))
      (patches (search-patches
                "perl-finance-quote-unuse-mozilla-ca.patch"))))
   (build-system perl-build-system)
   (propagated-inputs
    `(("perl-cgi" ,perl-cgi)
      ("perl-datetime" ,perl-datetime)
      ("perl-html-parser" ,perl-html-parser)
      ("perl-html-tableextract" ,perl-html-tableextract)
      ("perl-html-tree" ,perl-html-tree)
      ("perl-http-cookies" ,perl-http-cookies)
      ("perl-http-message" ,perl-http-message)
      ("perl-json" ,perl-json)
      ("perl-libwww" ,perl-libwww)
      ("perl-lwp-protocol-https" ,perl-lwp-protocol-https)
      ("perl-uri" ,perl-uri)))
   (home-page "https://metacpan.org/release/Finance-Quote")
   (synopsis "Stock and mutual fund quotes")
   (description
    "Finance::Quote gets stock quotes from various internet sources, including
Yahoo! Finance, Fidelity Investments, and the Australian Stock Exchange.")
   (license l:gpl2)))

(define-public perl-gssapi
  (package
    (name "perl-gssapi")
    (version "0.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AG/AGROLMS/"
                           "GSSAPI-" version ".tar.gz"))
       (sha256
        (base32
         "1mkhwxjjlhr58pd770i9gnf7zy7jj092iv6jfbnb8bvnc5xjr3vx"))))
    (build-system perl-build-system)
    (inputs `(("gssapi" ,mit-krb5)))
    (arguments
     `(#:make-maker-flags
       `(,(string-append "--gssapiimpl=" (assoc-ref %build-inputs "gssapi")))))
    (home-page "https://metacpan.org/release/GSSAPI")
    (synopsis "Perl extension providing access to the GSSAPIv2 library")
    (description "This is a Perl extension for using GSSAPI C bindings as
described in RFC 2744.")
    (license l:perl-license)))

(define-public perl-html-element-extended
  (package
    (name "perl-html-element-extended")
    (version "1.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MS/MSISK/"
                           "HTML-Element-Extended-" version ".tar.gz"))
       (sha256
        (base32
         "0axknss8c368r5i082yhkfj8mq0w4nglfrpcxcayyzzj13qimvzk"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-html-tree" ,perl-html-tree)))
    (home-page "https://metacpan.org/release/HTML-Element-Extended")
    (synopsis "Manipulate tables of HTML::Element")
    (description
     "HTML::Element::Extended is a Perl extension for manipulating a table
composed of HTML::Element style components.")
    (license l:perl-license)))

(define-public perl-html-form
  (package
    (name "perl-html-form")
    (version "6.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GA/GAAS/"
                           "HTML-Form-" version ".tar.gz"))
       (sha256
        (base32
         "0dpwr7yz6hjc3bcqgcbdzjjk9l58ycdjmbam9nfcmm85y2a1vh38"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-html-parser" ,perl-html-parser)
       ("perl-html-tagset" ,perl-html-tagset)
       ("perl-http-message" ,perl-http-message)
       ("perl-lwp-mediatypes" ,perl-lwp-mediatypes)
       ("perl-uri" ,perl-uri)))
    (home-page "https://metacpan.org/release/HTML-Form")
    (synopsis "Perl class representing an HTML form element")
    (description "Objects of the HTML::Form class represents a single HTML
<form> ... </form> instance.")
    (license l:perl-license)))

(define-public perl-html-scrubber
  (package
    (name "perl-html-scrubber")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/N/NI/NIGELM/HTML-Scrubber-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "06p7w4zd42b2yh541mlzyqj40lwmvvn3fyqi8big4mf34la7m2jm"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-cpan-meta" ,perl-test-cpan-meta)
       ("perl-test-differences" ,perl-test-differences)
       ("perl-test-eol" ,perl-test-eol)
       ("perl-test-memory-cycle" ,perl-test-memory-cycle)
       ("perl-test-notabs" ,perl-test-notabs)))
    (inputs
     `(("perl-html-parser" ,perl-html-parser)))
    (home-page
     "https://metacpan.org/release/HTML-Scrubber")
    (synopsis
     "Perl extension for scrubbing/sanitizing html")
    (description
     "@code{HTML::Scrubber} Perl extension for scrubbing/sanitizing HTML.")
    (license l:perl-license)))

(define-public perl-html-lint
  (package
    (name "perl-html-lint")
    (version "2.32")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PETDANCE/"
                           "HTML-Lint-" version ".tar.gz"))
       (sha256
        (base32 "0lk02xpfxcg7ij4dvpsa4wjlzhmiizj0jfr3rwmdpbj69nvc93br"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-html-parser" ,perl-html-parser)
       ("perl-html-tagset" ,perl-html-tagset)
       ("perl-libwww" ,perl-libwww)))
    (home-page "https://metacpan.org/release/HTML-Lint")
    (synopsis "Check for HTML errors in a string or file")
    (description "HTML::Lint is a pure-Perl HTML parser and checker for
syntactic legitmacy.")
    (license l:artistic2.0)))

(define-public perl-html-tableextract
  (package
    (name "perl-html-tableextract")
    (version "2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cpan.metacpan.org/authors/id/M/MS/MSISK/"
                           "HTML-TableExtract-" version ".tar.gz"))
       (sha256
        (base32
         "01jimmss3q68a89696wmclvqwb2ybz6xgabpnbp6mm6jcni82z8a"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-html-element-extended" ,perl-html-element-extended)
       ("perl-html-parser" ,perl-html-parser)))
    (home-page "https://metacpan.org/release/HTML-TableExtract")
    (synopsis "Extract contents from HTML tables")
    (description
     "HTML::TableExtract is a Perl module for extracting the content contained
in tables within an HTML document, either as text or encoded element trees.")
    (license l:perl-license)))

(define-public perl-html-tree
  (package
    (name "perl-html-tree")
    (version "5.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KE/KENTNL/"
                           "HTML-Tree-" version ".tar.gz"))
       (sha256
        (base32
         "1gyvm4qlwm9y6hczkpnrdfl303ggbybr0nqxdjw09hii8yw4sdzh"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-html-parser" ,perl-html-parser)
       ("perl-html-tagset" ,perl-html-tagset)
       ("perl-libwww" ,perl-libwww)))
    (home-page "https://metacpan.org/release/HTML-Tree")
    (synopsis "Work with HTML in a DOM-like tree structure")
    (description "This distribution contains a suite of modules for
representing, creating, and extracting information from HTML syntax trees.")
    (license l:perl-license)))

(define-public perl-html-parser
  (package
    (name "perl-html-parser")
    (version "3.72")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/HTML-Parser-"
                   version ".tar.gz"))
             (sha256
              (base32
               "12v05ywlnsi9lc17z32k9jxx3sj1viy7y1wpl7n4az76v7hwfa7c"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-html-tagset" ,perl-html-tagset)
       ("perl-http-message" ,perl-http-message)))
    (license l:perl-license)
    (synopsis "Perl HTML parser class")
    (description
     "Objects of the HTML::Parser class will recognize markup and separate
it from plain text (alias data content) in HTML documents.  As different
kinds of markup and text are recognized, the corresponding event handlers
are invoked.")
    (home-page "https://metacpan.org/release/HTML-Parser")))

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
    (license l:perl-license)
    (synopsis "Perl data tables useful in parsing HTML")
    (description
     "The HTML::Tagset module contains several data tables useful in various
kinds of HTML parsing operations.")
    (home-page "https://metacpan.org/release/HTML-Tagset")))

(define-public perl-html-template
  (package
    (name "perl-html-template")
    (version "2.97")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/S/SA/SAMTREGAR/"
                                  "HTML-Template-" version ".tar.gz"))
              (sha256
               (base32
                "17qjw8swj2q4b1ic285pndgrkmvpsqw0j68nhqzpk1daydhsyiv5"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-cgi" ,perl-cgi)))
    (home-page "https://metacpan.org/release/HTML-Template")
    (synopsis "HTML-like templates")
    (description
     "This module attempts to make using HTML templates simple and natural.
It extends standard HTML with a few new HTML-esque tags: @code{<TMPL_VAR>},
@code{<TMPL_LOOP>}, @code{<TMPL_INCLUDE>}, @code{<TMPL_IF>},
@code{<TMPL_ELSE>} and @code{<TMPL_UNLESS>}.  The file written with HTML and
these new tags is called a template.  Using this module you fill in the values
for the variables, loops and branches declared in the template.  This allows
you to separate design from the data.")
    (license l:perl-license)))

(define-public perl-http-body
  (package
    (name "perl-http-body")
    (version "1.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GE/GETTY/"
                           "HTTP-Body-" version ".tar.gz"))
       (sha256
        (base32
         "15vj488i62mdp4ps9k77h39prj70i7anb6b0j8nm7l9vbdc2q3gw"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-deep" ,perl-test-deep)))
    (propagated-inputs
     `(("perl-file-temp" ,perl-file-temp)
       ("perl-http-message" ,perl-http-message))) ;For HTTP::Headers
    (home-page "https://metacpan.org/release/HTTP-Body")
    (synopsis "HTTP Body Parser")
    (description "HTTP::Body parses chunks of HTTP POST data and supports
application/octet-stream, application/json, application/x-www-form-urlencoded,
and multipart/form-data.")
    (license l:perl-license)))

(define-public perl-http-cookiejar
  (package
    (name "perl-http-cookiejar")
    (version "0.008")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "HTTP-CookieJar-" version ".tar.gz"))
       (sha256
        (base32
         "0rfw6avcralggs7bf7n86flvhaahxjnqzvpwszp0sk4z4wwy01wm"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-deep" ,perl-test-deep)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-time-mock" ,perl-time-mock)
       ("perl-uri" ,perl-uri)))
    (inputs
     `(("perl-time-local" ,perl-time-local)
       ("perl-http-date" ,perl-http-date)))
    (home-page "https://metacpan.org/release/HTTP-CookieJar")
    (synopsis "Minimalist HTTP user agent cookie jar")
    (description "This module implements a minimalist HTTP user agent cookie
jar in conformance with RFC 6265 <http://tools.ietf.org/html/rfc6265>.")
    (license l:asl2.0)))

(define-public perl-http-cookies
  (package
    (name "perl-http-cookies")
    (version "6.04")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/O/OA/OALDERS/HTTP-Cookies-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1m0kxcirbvbkrm2c59p1bkbvzlcdymg8fdpa7wlxijlx0xwz1iqc"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-http-message" ,perl-http-message)))
    (license l:perl-license)
    (synopsis "Perl HTTP cookie jars")
    (description
     "The HTTP::Cookies class is for objects that represent a cookie jar,
that is, a database of all the HTTP cookies that a given LWP::UserAgent
object knows about.")
    (home-page "https://metacpan.org/release/GAAS/HTTP-Cookies-6.01")))

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
    (license l:perl-license)
    (synopsis "Perl simple http server class")
    (description
     "Instances of the HTTP::Daemon class are HTTP/1.1 servers that listen
on a socket for incoming requests.  The HTTP::Daemon is a subclass of
IO::Socket::INET, so you can perform socket operations directly on it too.")
    (home-page "https://metacpan.org/release/HTTP-Daemon")))

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
    (license l:perl-license)
    (synopsis "Perl date conversion routines")
    (description
     "The HTTP::Date module provides functions that deal with date formats
used by the HTTP protocol (and then some more).")
    (home-page "https://metacpan.org/release/HTTP-Date")))

(define-public perl-http-message
  (package
    (name "perl-http-message")
    (version "6.15")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/O/OA/OALDERS/HTTP-Message-"
                   version ".tar.gz"))
             (sha256
              (base32
               "11fbvisyvi6bw8z9iq9fm9mraf69qyds09fblhl9gyvg7ccll93v"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-try-tiny" ,perl-try-tiny)))
    (propagated-inputs
     `(("perl-encode-locale" ,perl-encode-locale)
       ("perl-http-date" ,perl-http-date)
       ("perl-io-html" ,perl-io-html)
       ("perl-lwp-mediatypes" ,perl-lwp-mediatypes)
       ("perl-uri" ,perl-uri)))
    (license l:perl-license)
    (synopsis "Perl HTTP style message")
    (description
     "An HTTP::Message object contains some headers and a content body.")
    (home-page "https://metacpan.org/release/ETHER/HTTP-Message-6.11")))

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
    (license l:perl-license)
    (synopsis "Perl http content negotiation")
    (description
     "The HTTP::Negotiate module provides a complete implementation of the
HTTP content negotiation algorithm specified in
draft-ietf-http-v11-spec-00.ps chapter 12.  Content negotiation allows for
the selection of a preferred content representation based upon attributes
of the negotiable variants and the value of the various Accept* header
fields in the request.")
    (home-page "https://metacpan.org/release/HTTP-Negotiate")))

(define-public perl-http-parser
  (package
    (name "perl-http-parser")
    (version "0.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ED/EDECA/"
                           "HTTP-Parser-" version ".tar.gz"))
       (sha256
        (base32
         "0idwq3jk595xil65lmxz128ha7s3r2n5zknisddpgwnqrghs3igq"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-http-message" ,perl-http-message)
       ("perl-uri" ,perl-uri)))
    (home-page "https://metacpan.org/release/HTTP-Parser")
    (synopsis "Parse HTTP/1.1 requests")
    (description "This is an HTTP request parser.  It takes chunks of text as
received and returns a 'hint' as to what is required, or returns the
HTTP::Request when a complete request has been read.  HTTP/1.1 chunking is
supported.")
    (license l:perl-license)))

(define-public perl-http-parser-xs
  (package
    (name "perl-http-parser-xs")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KA/KAZUHO/"
                           "HTTP-Parser-XS-" version ".tar.gz"))
       (sha256
        (base32
         "02d84xq1mm53c7jl33qyb7v5w4372vydp74z6qj0vc96wcrnhkkr"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (home-page "https://metacpan.org/release/HTTP-Parser-XS")
    (synopsis "Fast HTTP request parser")
    (description "HTTP::Parser::XS is a fast, primitive HTTP request/response
parser.")
    (license l:perl-license)))

(define-public perl-http-request-ascgi
  (package
    (name "perl-http-request-ascgi")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FL/FLORA/"
                           "HTTP-Request-AsCGI-" version ".tar.gz"))
       (sha256
        (base32
         "1smwmiarwcgq7vjdblnb6ldi2x1s5sk5p15p7xvm5byiqq3znnwl"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-class-accessor" ,perl-class-accessor)
       ("perl-http-message" ,perl-http-message)))
    (home-page "https://metacpan.org/release/HTTP-Request-AsCGI")
    (synopsis "Set up a CGI environment from an HTTP::Request")
    (description "This module provides a convenient way to set up a CGI
environment from an HTTP::Request.")
    (license l:perl-license)))

(define-public perl-http-server-simple
  (package
    (name "perl-http-server-simple")
    (version "0.52")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BP/BPS/"
                           "HTTP-Server-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "0k6bg7k6mjixfzxdkkdrhqvaqmdhjszx0zsk8g0bimiby6j9z4yq"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-cgi" ,perl-cgi)))
    (arguments
     ;; See the discussion of a related tests issue at
     ;; https://lists.gnu.org/archive/html/guix-devel/2015-01/msg00346.html
     `(#:tests? #f

       #:phases (modify-phases %standard-phases
                   (add-before 'configure 'set-search-path
                     (lambda _
                       ;; Work around "dotless @INC" build failure.
                       (setenv "PERL5LIB"
                               (string-append (getcwd) ":"
                                              (getenv "PERL5LIB")))
                       #t)))))
    (home-page "https://metacpan.org/release/HTTP-Server-Simple")
    (synopsis "Lightweight HTTP server")
    (description "HTTP::Server::Simple is a simple standalone HTTP daemon with
no non-core module dependencies.  It can be used for building a standalone
http-based UI to your existing tools.")
    (license l:perl-license)))

(define-public perl-http-tiny
  (package
    (name "perl-http-tiny")
    (version "0.076")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "HTTP-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "11wkxxqj3ff84rgj9q2gzkdgscwp3fzj205846k9ycqinlpsmgfx"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-http-cookiejar" ,perl-http-cookiejar)
       ("perl-io-socket-ip" ,perl-io-socket-ip)
       ("perl-io-socket-ssl" ,perl-io-socket-ssl)
       ("perl-mozilla-ca" ,perl-mozilla-ca)
       ("perl-net-ssleay" ,perl-net-ssleay)))
    (home-page "https://metacpan.org/release/HTTP-Tiny")
    (synopsis "HTTP/1.1 client")
    (description "This is a very simple HTTP/1.1 client, designed for doing
simple requests without the overhead of a large framework like LWP::UserAgent.
It supports proxies and redirection.  It also correctly resumes after EINTR.")
    (license l:perl-license)))

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
    (license l:perl-license)
    (synopsis "Perl module to open an HTML file with automatic charset detection")
    (description
     "IO::HTML provides an easy way to open a file containing HTML while
automatically determining its encoding.  It uses the HTML5 encoding sniffing
algorithm specified in section 8.2.2.1 of the draft standard.")
    (home-page "https://metacpan.org/release/IO-HTML")))

(define-public perl-io-socket-ip
  (package
    (name "perl-io-socket-ip")
    (version "0.39")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PEVANS/"
                           "IO-Socket-IP-" version ".tar.gz"))
       (sha256
        (base32
         "15kv5g1yb4a345sk3r5wfr99f868lhfqkddzsgpqddvccfkhv58i"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/IO-Socket-IP")
    (synopsis "Family-neutral IP socket supporting both IPv4 and IPv6")
    (description "This module provides a protocol-independent way to use IPv4
and IPv6 sockets, intended as a replacement for IO::Socket::INET.")
    (license l:perl-license)))

(define-public perl-io-socket-ssl
  (package
    (name "perl-io-socket-ssl")
    (version "2.038")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/S/SU/SULLR/"
                                  "IO-Socket-SSL-" version ".tar.gz"))
              (sha256
               (base32
                "11fiifxyvn7njc9p52wgygyw24jz7rh7gnz2ikjphr4l4x9f03rx"))
              (patches (search-patches
                        "perl-io-socket-ssl-openssl-1.0.2f-fix.patch"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-net-ssleay" ,perl-net-ssleay)
       ;; for IDN support
       ("perl-uri" ,perl-uri)))
    (synopsis "Nearly transparent SSL encapsulation for IO::Socket::INET")
    (description
     "IO::Socket::SSL makes using SSL/TLS much easier by wrapping the
necessary functionality into the familiar IO::Socket interface and providing
secure defaults whenever possible.  This way existing applications can be made
SSL-aware without much effort, at least if you do blocking I/O and don't use
select or poll.")
    (license l:perl-license)
    (home-page "https://github.com/noxxi/p5-io-socket-ssl")))

(define-public perl-libwww
  (package
    (name "perl-libwww")
    (version "6.37")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/E/ET/ETHER/libwww-perl-"
                   version ".tar.gz"))
             (sha256
              (base32
               "04a24cx9gs070rvlwf5kanz03y7nnq9k2nmpr01plnm059iprvf6"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-needs" ,perl-test-needs)
       ("perl-test-requiresinternet" ,perl-test-requiresinternet)))
    (propagated-inputs
     `(("perl-encode-locale" ,perl-encode-locale)
       ("perl-file-listing" ,perl-file-listing)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-http-cookies" ,perl-http-cookies)
       ("perl-http-daemon" ,perl-http-daemon)
       ("perl-http-date" ,perl-http-date)
       ("perl-http-message" ,perl-http-message)
       ("perl-http-negotiate" ,perl-http-negotiate)
       ("perl-net-http" ,perl-net-http)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-uri" ,perl-uri)
       ("perl-www-robotrules" ,perl-www-robotrules)))
    (license l:perl-license)
    (synopsis "Perl modules for the WWW")
    (description
     "The libwww-perl collection is a set of Perl modules which provides a
simple and consistent application programming interface to the
World-Wide Web.  The main focus of the library is to provide classes
and functions that allow you to write WWW clients.  The library also
contains modules that are of more general use and even classes that
help you implement simple HTTP servers.")
    (home-page "https://metacpan.org/release/libwww-perl")))

(define-public perl-lwp-online
  (package
    (name "perl-lwp-online")
    (version "1.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AD/ADAMK/LWP-Online-"
             version ".tar.gz"))
       (sha256
        (base32
         "176f6vbk1018i0y7xj9d406ndbjgwzan2j9nihxnsahzg2vr2vz2"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-libwww" ,perl-libwww)
       ("perl-uri" ,perl-uri)))
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (home-page "https://metacpan.org/release/LWP-Online")
    (synopsis "Checks whether your process has access to the web")
    (description "This module attempts to answer, as accurately as it can, one
of the nastiest technical questions there is: am I on the internet?

A host of networking and security issues make this problem very difficult.
There are firewalls, proxies (both well behaved and badly behaved).  We might
not have DNS.  We might not have a network card at all!")
    (license l:perl-license)))

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
    (license l:perl-license)
    (synopsis "Perl module to guess the media type for a file or a URL")
    (description
     "The LWP::MediaTypes module provides functions for handling media (also
known as MIME) types and encodings.  The mapping from file extensions to
media types is defined by the media.types file.  If the ~/.media.types file
exists it is used instead.")
    (home-page "https://metacpan.org/release/LWP-MediaTypes")))

(define-public perl-lwp-protocol-https
  (package
    (name "perl-lwp-protocol-https")
    (version "6.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OA/OALDERS/"
                           "LWP-Protocol-https-" version ".tar.gz"))
       (sha256
        (base32
         "1rxrpwylfw1afah0nk96kgkwjbl2p1a7lwx50iipg8c4rx3cjb2j"))))
    (build-system perl-build-system)
    (native-inputs
     ;; For tests.
     `(("perl-test-requiresinternet" ,perl-test-requiresinternet)))
    (propagated-inputs
     `(("perl-io-socket-ssl" ,perl-io-socket-ssl)
       ("perl-libwww" ,perl-libwww)
       ;; Users should instead make sure SSL_ca_path is set properly.
       ;; ("perl-mozilla-ca" ,perl-mozilla-ca)
       ("perl-net-http" ,perl-net-http)))
    (home-page "https://metacpan.org/release/LWP-Protocol-https")
    (synopsis "HTTPS support for LWP::UserAgent")
    (description "The LWP::Protocol::https module provides support for using
https schemed URLs with LWP.")
    (license l:perl-license)))

(define-public perl-lwp-useragent-determined
  (package
    (name "perl-lwp-useragent-determined")
    (version "1.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AL/ALEXMV/"
                           "LWP-UserAgent-Determined-" version ".tar.gz"))
       (sha256
        (base32
         "0lyvbpjng7yfvyha9rp2y2c6liz5hhplmd2grc8jlsfkih7dbn06"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-libwww" ,perl-libwww)))
    (home-page "https://metacpan.org/release/LWP-UserAgent-Determined")
    (synopsis "Virtual browser that retries errors")
    (description "LWP::UserAgent::Determined works just like LWP::UserAgent,
except that when you use it to get a web page but run into a
possibly-temporary error (like a DNS lookup timeout), it'll wait a few seconds
and retry a few times.")
    (license l:perl-license)))

(define-public perl-net-amazon-s3
  (package
    (name "perl-net-amazon-s3")
    (version "0.60")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PF/PFIG/"
                           "Net-Amazon-S3-" version ".tar.gz"))
       (sha256
        (base32
         "10dcsq4s2kc9cb1vccx17r187c81drirc3s1hbxh3rb8489kg2b2"))
       (patches (search-patches
                 "perl-net-amazon-s3-moose-warning.patch"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-libwww" ,perl-libwww)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-data-stream-bulk" ,perl-data-stream-bulk)
       ("perl-datetime-format-http" ,perl-datetime-format-http)
       ("perl-digest-hmac" ,perl-digest-hmac)
       ("perl-digest-md5-file" ,perl-digest-md5-file)
       ("perl-file-find-rule" ,perl-file-find-rule)
       ("perl-http-date" ,perl-http-date)
       ("perl-http-message" ,perl-http-message)
       ("perl-lwp-useragent-determined" ,perl-lwp-useragent-determined)
       ("perl-mime-types" ,perl-mime-types)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-strictconstructor" ,perl-moosex-strictconstructor)
       ("perl-moosex-types-datetime-morecoercions"
        ,perl-moosex-types-datetime-morecoercions)
       ("perl-path-class" ,perl-path-class)
       ("perl-regexp-common" ,perl-regexp-common)
       ("perl-term-encoding" ,perl-term-encoding)
       ("perl-term-progressbar-simple" ,perl-term-progressbar-simple)
       ("perl-uri" ,perl-uri)
       ("perl-xml-libxml" ,perl-xml-libxml)))
    (home-page "https://metacpan.org/release/Net-Amazon-S3")
    (synopsis "Perl interface to Amazon S3")
    (description "This module provides a Perlish interface to Amazon S3.")
    (license l:perl-license)))

(define-public perl-net-http
  (package
    (name "perl-net-http")
    (version "6.18")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/O/OA/OALDERS/"
                   "Net-HTTP-" version ".tar.gz"))
             (sha256
              (base32
               "074mp9s37q1j290xa3qj1wwgalzla328i2zpnh73xkmdnwnxyhky"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-io-socket-ssl" ,perl-io-socket-ssl)
       ("perl-uri" ,perl-uri)))
    (license l:perl-license)
    (synopsis "Perl low-level HTTP connection (client)")
    (description
     "The Net::HTTP class is a low-level HTTP client.  An instance of the
Net::HTTP class represents a connection to an HTTP server.  The HTTP protocol
is described in RFC 2616.  The Net::HTTP class supports HTTP/1.0 and
HTTP/1.1.")
    (home-page "https://metacpan.org/release/Net-HTTP")))

(define-public perl-net-server
  (package
    (name "perl-net-server")
    (version "2.009")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RH/RHANDOM/"
                           "Net-Server-" version ".tar.gz"))
       (sha256
        (base32
         "0gw1k9gcw7habbkxvsfa2gz34brlbwcidk6khgsf1qjm0dbccrw2"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Net-Server")
    (synopsis "Extensible Perl server engine")
    (description "Net::Server is an extensible, generic Perl server engine.
It attempts to be a generic server as in Net::Daemon and NetServer::Generic.
It includes with it the ability to run as an inetd
process (Net::Server::INET), a single connection server (Net::Server or
Net::Server::Single), a forking server (Net::Server::Fork), a preforking
server which maintains a constant number of preforked
children (Net::Server::PreForkSimple), or as a managed preforking server which
maintains the number of children based on server load (Net::Server::PreFork).
In all but the inetd type, the server provides the ability to connect to one
or to multiple server ports.")
    (license l:perl-license)))

(define-public perl-net-smtp-ssl
  (package
    (name "perl-net-smtp-ssl")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Net-SMTP-SSL-" version ".tar.gz"))
       (sha256
        (base32
         "001a6dcfahf7kkyirqkc8jd4fh4fkal7n7vm9c4dblqrvmdc8abv"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-io-socket-ssl" ,perl-io-socket-ssl)))
    (home-page "https://metacpan.org/release/Net-SMTP-SSL")
    (synopsis "SSL support for Net::SMTP")
    (description "SSL support for Net::SMTP.")
    (license l:perl-license)))

(define-public perl-plack
  (package
    (name "perl-plack")
    (version "1.0033")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Plack-" version ".tar.gz"))
       (sha256
        (base32
         "081jg0xddzpg2anmqi9i6d7vs6c8z7k557bf8xl6vgb3h95pin5w"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-requires" ,perl-test-requires)
       ("perl-file-sharedir-install" ,perl-file-sharedir-install)))
    (propagated-inputs
     `(("perl-apache-logformat-compiler" ,perl-apache-logformat-compiler)
       ("perl-devel-stacktrace" ,perl-devel-stacktrace)
       ("perl-devel-stacktrace-ashtml" ,perl-devel-stacktrace-ashtml)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-hash-multivalue" ,perl-hash-multivalue)
       ("perl-http-body" ,perl-http-body)
       ("perl-http-message" ,perl-http-message)
       ("perl-http-tiny" ,perl-http-tiny)
       ("perl-libwww" ,perl-libwww)
       ("perl-stream-buffered" ,perl-stream-buffered)
       ("perl-test-tcp" ,perl-test-tcp)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-uri" ,perl-uri)))
    (home-page "https://metacpan.org/release/Plack")
    (synopsis "Perl Superglue for Web frameworks and servers (PSGI toolkit)")
    (description "Plack is a set of tools for using the PSGI stack.  It
contains middleware components, a reference server, and utilities for Web
application frameworks.  Plack is like Ruby's Rack or Python's Paste for
WSGI.")
    (license l:perl-license)))

(define-public perl-plack-middleware-fixmissingbodyinredirect
  (package
    (name "perl-plack-middleware-fixmissingbodyinredirect")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SW/SWEETKID/"
                           "Plack-Middleware-FixMissingBodyInRedirect-"
                           version ".tar.gz"))
       (sha256
        (base32
         "14dkrmccq7a5vpymx5dv8032gfcvhsw2i6v5sh3c4ym5ymlx08kc"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-html-parser" ,perl-html-parser) ;for HTML::Entities
       ("perl-http-message" ,perl-http-message)
       ("perl-plack" ,perl-plack)))     ;for Plack::Test
    (home-page
     "https://metacpan.org/release/Plack-Middleware-FixMissingBodyInRedirect")
    (synopsis "Plack::Middleware which sets body for redirect response")
    (description "This module sets the body in redirect response, if it's not
already set.")
    (license l:perl-license)))

(define-public perl-plack-middleware-methodoverride
  (package
    (name "perl-plack-middleware-methodoverride")
    (version "0.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Plack-Middleware-MethodOverride-"
                           version ".tar.gz"))
       (sha256
        (base32 "1wdmmav3rbhv49zpw311zrxxqmg1fz3f3q9src0ypgs8zcp5myyv"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-plack" ,perl-plack)))
    (home-page "https://metacpan.org/release/Plack-Middleware-MethodOverride")
    (synopsis "Override REST methods to Plack apps via POST")
    (description "This middleware allows for POST requests that pretend to be
something else: by adding either a header named X-HTTP-Method-Override to the
request, or a query parameter named x-tunneled-method to the URI, the client
can say what method it actually meant.")
    (license l:perl-license)))

(define-public perl-plack-middleware-removeredundantbody
  (package
    (name "perl-plack-middleware-removeredundantbody")
    (version "0.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SW/SWEETKID/"
                           "Plack-Middleware-RemoveRedundantBody-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1n3wm0zi8dnk54jx937asl951lslj3jvw0fry4jpzsibg4f6wrx0"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-plack" ,perl-plack)))
    (home-page
     "https://metacpan.org/release/Plack-Middleware-RemoveRedundantBody")
    (synopsis "Plack::Middleware which removes body for HTTP response")
    (description "This module removes the body in an HTTP response if it's not
required.")
    (license l:perl-license)))

(define-public perl-plack-middleware-reverseproxy
  (package
    (name "perl-plack-middleware-reverseproxy")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Plack-Middleware-ReverseProxy-"
                           version ".tar.gz"))
       (sha256
        (base32 "0a512n62pnk5ayj3zdzyj50iy1qi8nwh6ygks2h7nrh7gp9k2jc7"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-plack" ,perl-plack)))
    (home-page "https://metacpan.org/release/Plack-Middleware-ReverseProxy")
    (synopsis "Supports app to run as a reverse proxy backend")
    (description "Plack::Middleware::ReverseProxy resets some HTTP headers,
which are changed by reverse-proxy.  You can specify the reverse proxy address
and stop fake requests using 'enable_if' directive in your app.psgi.")
    (license l:perl-license)))

(define-public perl-plack-test-externalserver
  (package
    (name "perl-plack-test-externalserver")
    (version "0.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FL/FLORA/"
                           "Plack-Test-ExternalServer-" version ".tar.gz"))
       (sha256
        (base32
         "1dbg1p3rgvvbkkpvca5jlc2mzx8iqyiybk88al93pvbca65h1g7h"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-plack" ,perl-plack)))
    (home-page "https://metacpan.org/release/Plack-Test-ExternalServer")
    (synopsis "Run HTTP tests on external live servers")
    (description "This module allows your to run your Plack::Test tests
against an external server instead of just against a local application through
either mocked HTTP or a locally spawned server.")
    (license l:perl-license)))

(define-public perl-test-tcp
  (package
    (name "perl-test-tcp")
    (version "2.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOKUHIROM/"
                           "Test-TCP-" version ".tar.gz"))
       (sha256
        (base32 "14ahzklq3xgmwj58p9vdcfgpggrmh3nigq5mzqk4wakbb6fjs0fx"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-test-sharedfork" ,perl-test-sharedfork)))
    (arguments `(#:tests? #f))          ;related to signaling in t/05_sigint.t
    (home-page "https://metacpan.org/release/Test-TCP")
    (synopsis "Testing TCP programs")
    (description "Test::TCP is test utilities for TCP/IP programs.")
    (license l:perl-license)))

(define-public perl-test-www-mechanize
  (package
    (name "perl-test-www-mechanize")
    (version "1.52")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PETDANCE/"
                           "Test-WWW-Mechanize-" version ".tar.gz"))
       (sha256
        (base32 "1jsywlbxhqw39ij7s8vmgff5vys58vlfaq27072awacnxc65aal4"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-carp-assert-more" ,perl-carp-assert-more)
       ("perl-html-form" ,perl-html-form)
       ("perl-html-lint" ,perl-html-lint)
       ("perl-http-server-simple" ,perl-http-server-simple)
       ("perl-libwww" ,perl-libwww)
       ("perl-test-longstring" ,perl-test-longstring)
       ("perl-www-mechanize" ,perl-www-mechanize)))
    (home-page "https://metacpan.org/release/Test-WWW-Mechanize")
    (synopsis "Testing-specific WWW::Mechanize subclass")
    (description "Test::WWW::Mechanize is a subclass of the Perl module
WWW::Mechanize that incorporates features for web application testing.")
    (license l:artistic2.0)))

(define-public perl-test-www-mechanize-catalyst
  (package
    (name "perl-test-www-mechanize-catalyst")
    (version "0.62")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MS/MSTROUT/"
                           "Test-WWW-Mechanize-Catalyst-" version ".tar.gz"))
       (sha256
        (base32 "1cdc2q16vs6fb335pzaislz2rx1ph9acaxyp7v5hv9xbwwddwfqq"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-catalyst-plugin-session" ,perl-catalyst-plugin-session)
       ("perl-catalyst-plugin-session-state-cookie"
        ,perl-catalyst-plugin-session-state-cookie)
       ("perl-module-install" ,perl-module-install)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-utf8" ,perl-test-utf8)))
    (propagated-inputs
     `(("perl-catalyst-runtime" ,perl-catalyst-runtime)
       ("perl-class-load" ,perl-class-load)
       ("perl-libwww" ,perl-libwww)
       ("perl-moose" ,perl-moose)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-test-www-mechanize" ,perl-test-www-mechanize)
       ("perl-www-mechanize" ,perl-www-mechanize)))
    (home-page "https://metacpan.org/release/Test-WWW-Mechanize-Catalyst")
    (synopsis "Test::WWW::Mechanize for Catalyst")
    (description "The Test::WWW::Mechanize::Catalyst module meshes the
Test::WWW:Mechanize module and the Catalyst web application framework to allow
testing of Catalyst applications without needing to start up a web server.")
    (license l:perl-license)))

(define-public perl-test-www-mechanize-psgi
  (package
    (name "perl-test-www-mechanize-psgi")
    (version "0.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OA/OALDERS/"
                           "Test-WWW-Mechanize-PSGI-" version ".tar.gz"))
       (sha256
        (base32
         "0fsh2i05kf1kfavv2r9kmnjl7qlyqrd11ikc0qcqzzxsqzzjkg9r"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-pod" ,perl-test-pod)))
    (propagated-inputs
     `(("perl-plack" ,perl-plack)
       ("perl-test-www-mechanize" ,perl-test-www-mechanize)))
    (home-page "https://metacpan.org/release/Test-WWW-Mechanize-PSGI")
    (synopsis "Test PSGI programs using WWW::Mechanize")
    (description "PSGI is a specification to decouple web server environments
from web application framework code.  Test::WWW::Mechanize is a subclass of
WWW::Mechanize that incorporates features for web application testing.  The
Test::WWW::Mechanize::PSGI module meshes the two to allow easy testing of PSGI
applications.")
    (license l:perl-license)))

(define-public perl-uri
  (package
    (name "perl-uri")
    (version "1.73")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                                 "URI-" version ".tar.gz"))
             (sha256
              (base32
               "04z4xwiryrbxxi48bwbkgq9q9pwfgqry3wp0ramcrwv3dx5ap9yc"))))
    (build-system perl-build-system)
    (native-inputs
     ;; For tests.
     `(("perl-test-needs" ,perl-test-needs)))
    (license l:perl-license)
    (synopsis "Perl Uniform Resource Identifiers (absolute and relative)")
    (description
     "The URI module implements the URI class.  Objects of this class
represent \"Uniform Resource Identifier references\" as specified in RFC 2396
and updated by RFC 2732.")
    (home-page "https://metacpan.org/release/URI")))

(define-public perl-uri-fetch
  (package
    (name "perl-uri-fetch")
    (version "0.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                                  "URI-Fetch-" version ".tar.gz"))
              (sha256
               (base32
                "0rw6xiqm70s218aii9id3hf8j3pz6n22xnwd8v9m1ff2bnh63c0d"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f)) ; Tests require internet connection to succeed
    (inputs
     `(("perl-class-errorhandler" ,perl-class-errorhandler)
       ("perl-libwww" ,perl-libwww)
       ("perl-uri" ,perl-uri)))
    (home-page "https://metacpan.org/release/URI-Fetch")
    (synopsis "Smart URI fetching/caching")
    (description "@code{URI::Fetch} is a smart client for fetching HTTP pages,
notably syndication feeds (RSS, Atom, and others), in an intelligent, bandwidth-
and time-saving way.")
    (license l:perl-license)))

(define-public perl-uri-find
  (package
    (name "perl-uri-find")
    (version "20160806")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MS/MSCHWERN/"
                           "URI-Find-" version ".tar.gz"))
       (sha256
        (base32
         "1mk3jv8x0mcq3ajrn9garnxd0jc7sw4pkwqi88r5apqvlljs84z2"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-uri" ,perl-uri)))
    (home-page "https://metacpan.org/release/URI-Find")
    (synopsis "Find URIs in arbitrary text")
    (description "This module finds URIs and URLs (according to what URI.pm
considers a URI) in plain text.  It only finds URIs which include a
scheme (http:// or the like), for something a bit less strict, consider
URI::Find::Schemeless.  For a command-line interface, urifind is provided.")
    (license l:perl-license)))

(define-public perl-uri-ws
  (package
    (name "perl-uri-ws")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PL/PLICEASE/"
                           "URI-ws-" version ".tar.gz"))
       (sha256
        (base32
         "1vs1wm80sq685944g1l4a0fxcbccc00c0f9648yabdmcf90hwsvf"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-uri" ,perl-uri)))
    (home-page "https://metacpan.org/release/URI-ws")
    (synopsis "WebSocket support for URI package")
    (description "With this module, the URI package provides the same set of
methods for WebSocket URIs as it does for HTTP URIs.")
    (license l:perl-license)))

(define-public perl-uri-template
  (package
    (name "perl-uri-template")
    (version "0.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/B/BR/BRICAS/URI-Template-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1phibcmam2hklrddzj79l43va1gcqpyszbw21ynxq53ynmhjvbk8"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-uri" ,perl-uri)))
    (native-inputs
     `(("perl-test-pod-coverage" ,perl-test-pod-coverage)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-module-install" ,perl-module-install)
       ("perl-json" ,perl-json)))
    (home-page "https://metacpan.org/release/URI-Template")
    (synopsis "Object for handling URI templates")
    (description "This perl module provides a wrapper around URI templates as described in
RFC 6570.")
    (license l:perl-license)))

(define-public perl-www-curl
  (package
    (name "perl-www-curl")
    (version "4.17")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/S/SZ/SZBALINT/WWW-Curl-"
                    version".tar.gz"))
              (patches (search-patches "perl-www-curl-remove-symbol.patch"))
              (sha256
               (base32
                "1fmp9aib1kaps9vhs4dwxn7b15kgnlz9f714bxvqsd1j1q8spzsj"))))
    (build-system perl-build-system)
    (arguments
     '(#:tests? #f                          ;XXX: tests require network access

       #:phases (modify-phases %standard-phases
                   (add-before 'configure 'set-search-path
                     (lambda _
                       ;; Work around "dotless @INC" build failure.
                       (setenv "PERL5LIB"
                               (string-append (getcwd) ":"
                                              (getenv "PERL5LIB")))
                       #t)))))
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (inputs `(("curl" ,curl)))
    (synopsis "Perl extension interface for libcurl")
    (description
     "This is a Perl extension interface for the libcurl file downloading
library.")
    (license l:perl-license)
    (home-page "https://metacpan.org/release/WWW-Curl")))

(define-public perl-www-mechanize
  (package
    (name "perl-www-mechanize")
    (version "1.91")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OA/OALDERS/"
                           "WWW-Mechanize-" version ".tar.gz"))
       (sha256
        (base32 "0cb14m1vhaf0mgn2fqwi5hm72xhfi77hpq2g57swgy0w83x7m27b"))))
    (build-system perl-build-system)
    (native-inputs                      ;only for tests
     `(("perl-cgi" ,perl-cgi)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-output" ,perl-test-output)
       ("perl-test-warnings" ,perl-test-warnings)))
    (propagated-inputs
     `(("perl-html-form" ,perl-html-form)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-html-tree" ,perl-html-tree)
       ("perl-http-message" ,perl-http-message)
       ("perl-http-server-simple" ,perl-http-server-simple)
       ("perl-libwww" ,perl-libwww)
       ("perl-test-warn" ,perl-test-warn)
       ("perl-uri" ,perl-uri)))
    (home-page "https://metacpan.org/release/WWW-Mechanize")
    (synopsis "Web browsing in a Perl object")
    (description "WWW::Mechanize is a Perl module for stateful programmatic
web browsing, used for automating interaction with websites.")
    (license l:perl-license)))

(define-public perl-www-opensearch
  (package
    (name "perl-www-opensearch")
    (version "0.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/B/BR/BRICAS/"
                                  "WWW-OpenSearch-" version ".tar.gz"))
              (sha256
               (base32
                "1yxplx1q1qk2fvnzqrbk01lz26fy1lyhay51a3ky7q3jgh9p01rb"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-class-errorhandler" ,perl-class-errorhandler)
       ("perl-datetime" ,perl-datetime)
       ("perl-datetime-format-mail" ,perl-datetime-format-mail)
       ("perl-datetime-format-w3cdtf" ,perl-datetime-format-w3cdtf)
       ("perl-feed-find" ,perl-feed-find)
       ("perl-module-install" ,perl-module-install)
       ("perl-module-pluggable" ,perl-module-pluggable)
       ("perl-uri-fetch" ,perl-uri-fetch)
       ("perl-test-simple" ,perl-test-simple)
       ("perl-xml-atom" ,perl-xml-atom)
       ("perl-xml-rss" ,perl-xml-rss)))
    (inputs
     `(("perl-data-page" ,perl-data-page)
       ("perl-libwww" ,perl-libwww)
       ("perl-uri" ,perl-uri)
       ("perl-uri-template" ,perl-uri-template)
       ("perl-xml-feed" ,perl-xml-feed)
       ("perl-xml-libxml" ,perl-xml-libxml)))
    (home-page "https://metacpan.org/release/WWW-OpenSearch")
    (synopsis "Search A9 OpenSearch compatible engines")
    (description
     "@code{WWW::OpenSearch} is a module to search @url{A9's OpenSearch,
http://opensearch.a9.com} compatible search engines.")
    (license l:perl-license)))

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
    (license l:perl-license)
    (synopsis "Perl database of robots.txt-derived permissions")
    (description
     "The WWW::RobotRules module parses /robots.txt files as specified in
\"A Standard for Robot Exclusion\", at
<http://www.robotstxt.org/wc/norobots.html>.  Webmasters can use the
/robots.txt file to forbid conforming robots from accessing parts of
their web site.")
    (home-page "https://metacpan.org/release/WWW-RobotRules")))

(define-public python-feedparser
  (package
    (name "python-feedparser")
    (version "5.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "feedparser" version ".tar.bz2"))
       (sha256
        (base32
         "00hb4qg2am06g81mygfi1jsbx8830024jm45g6qp9g8fr6am91yf"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))
    (home-page
     "https://github.com/kurtmckee/feedparser")
    (synopsis "Parse feeds in Python")
    (description
     "Universal feed parser which handles RSS 0.9x, RSS 1.0, RSS 2.0,
CDF, Atom 0.3, and Atom 1.0 feeds.")
    (license (list l:bsd-2 ; source code
                   l:freebsd-doc)))) ; documentation

(define-public python2-feedparser
  (package-with-python2 python-feedparser))

(define-public gumbo-parser
  (package
    (name "gumbo-parser")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/"
                                  "gumbo-parser/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bgg2kbj311pqdzw2v33za7k66g1rv44kkvvnz2gnpaasi9k0ii8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))         ;tests require bundling googletest sources
    ;; The release tarball lacks the generated files.
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://github.com/google/gumbo-parser")
    (synopsis "HTML5 parsing library")
    (description
     "Gumbo is an implementation of the HTML5 parsing algorithm implemented as
a pure C99 library.")
    (license l:asl2.0)))

(define-public uwsgi
  (package
    (name "uwsgi")
    (version "2.0.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://projects.unbit.it/downloads/uwsgi-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10zmk4npknigmbqcq1wmhd461dk93159px172112vyq0i19sqwj9"))))
    (build-system gnu-build-system)
    (outputs '("out" "python"))
    (arguments
     '(;; XXX: The 'check' target runs cppcheck to do static code analysis.
       ;;      But there is no obvious way to run the real tests.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; Configuration is done by writing an ini file.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (bindir    (string-append out "/bin"))
                    (plugindir (string-append out "/lib/uwsgi")))
               ;; The build phase outputs files to these directories directly.
               (mkdir-p bindir)
               (mkdir-p plugindir)
               ;; XXX: Enable other plugins.
               (call-with-output-file "buildconf/guix.ini"
                 (lambda (port)
                   (format port "[uwsgi]
yaml = libyaml
bin_name = ~a/uwsgi
plugin_dir = ~a

inherit = base
plugins = cgi,python
embedded_plugins =
" bindir plugindir))))
             (setenv "PROFILE" "guix")
             #t))
         (replace 'install
           ;; Move plugins into their own output.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out           (assoc-ref outputs "out"))
                    (plugindir     (string-append out "/lib/uwsgi"))
                    (python-plugin (string-append
                                    plugindir "/python_plugin.so")))
               (install-file python-plugin
                             (string-append
                              (assoc-ref outputs "python") "/lib/uwsgi"))
               (delete-file python-plugin)
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("jansson" ,jansson)
       ("libxml2" ,libxml2)
       ("libyaml" ,libyaml)
       ("openssl" ,openssl)
       ("pcre" ,pcre)
       ("zlib" ,zlib)
       ;; For plugins.
       ("python" ,python)))
    (home-page "https://uwsgi-docs.readthedocs.org/")
    (synopsis "Application container server")
    (description
     "uWSGI presents a complete stack for networked/clustered web applications,
implementing message/object passing, caching, RPC and process management.
It uses the uwsgi protocol for all the networking/interprocess communications.")
    (license l:gpl2+))) ; with linking exception

(define-public jq
  (package
    (name "jq")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/stedolan/jq"
                                  "/releases/download/jq-" version
                                  "/jq-" version ".tar.gz"))
              (sha256
               (base32
                "1a76f46a652i2g333kfvrl6mp2w7whf6h1yly519izg4y967h9cn"))))
    (inputs
     `(("oniguruma" ,oniguruma)))
    (native-inputs
     `(;; TODO fix gems to generate documentation
       ;;("ruby" ,ruby)
       ;;("bundler" ,bundler)
       ("valgrind" ,valgrind)))
    (build-system gnu-build-system)
    (home-page "http://stedolan.github.io/jq/")
    (synopsis "Command-line JSON processor")
    (description "jq is like sed for JSON data – you can use it to slice and
filter and map and transform structured data with the same ease that sed, awk,
grep and friends let you play with text.  It is written in portable C.  jq can
mangle the data format that you have into the one that you want with very
little effort, and the program to do so is often shorter and simpler than
you'd expect.")
    (license (list l:expat l:cc-by3.0))))

(define-public uhttpmock
  (package
    (name "uhttpmock")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://tecnocode.co.uk/downloads/uhttpmock/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "163py4klka423x7li2b685gmg3a6hjf074mlff2ajhmi3l0lm8x6"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ;; For check phase.
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libsoup" ,libsoup)))
    (home-page "https://gitlab.com/groups/uhttpmock")
    (synopsis "Library for mocking web service APIs which use HTTP or HTTPS")
    (description
     "Uhttpmock is a project for mocking web service APIs which use HTTP or
HTTPS.  It provides a library, libuhttpmock, which implements recording and
playback of HTTP request/response traces.")
    (license l:lgpl2.1+)))

(define-public woof
  (package
    (name "woof")
    (version "2012-05-31")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.home.unix-ag.org/simon/woof-"
                    version ".py"))
              (sha256
               (base32
                "0wjmjhpg6xlid33yi59j47q2qadz20sijrqsjahj30vngz856hyq"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (out    (assoc-ref %outputs "out"))
                (bin    (string-append out "/bin"))
                (python (assoc-ref %build-inputs "python")))
           (mkdir-p bin)
           (with-directory-excursion bin
             (copy-file source "woof")
             (patch-shebang "woof" (list (string-append python "/bin")))
             (chmod "woof" #o555))
           #t))))
    (inputs `(("python" ,python-2)))
    (home-page "http://www.home.unix-ag.org/simon/woof.html")
    (synopsis "Single file web server")
    (description "Woof (Web Offer One File) is a small simple web server that
can easily be invoked on a single file.  Your partner can access the file with
tools they trust (e.g. wget).")
    (license l:gpl2+)))

(define netsurf-buildsystem
  (package
    (name "netsurf-buildsystem")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           "buildsystem-" version ".tar.gz"))
       (sha256
        (base32
         "1q23aaycv35ma5471l1gxib8lfq2s9kprrkaqgfc926d04rlbmhw"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build))))
    (home-page "http://www.netsurf-browser.org")
    (synopsis "Build system for the Netsurf project")
    (description
     "This package provides the shared build system for Netsurf project
libraries.")
    (license l:expat)))

(define netsurf-buildsystem-arguments
  `(#:make-flags `("COMPONENT_TYPE=lib-shared"
                   "CC=gcc" "BUILD_CC=gcc"
                   ,(string-append "PREFIX=" %output)
                   ,(string-append "NSSHARED="
                                   (assoc-ref %build-inputs
                                              "netsurf-buildsystem")
                                   "/share/netsurf-buildsystem"))
    #:test-target "test"
    #:phases (modify-phases %standard-phases
               (delete 'configure))))

(define-public libparserutils
  (package
    (name "libparserutils")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "1n2794y2l0c8nv8z2pxwfnbn882987ifmxjv60zdxkhcndhswarj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)
       ("pkg-config" ,pkg-config)
       ("perl" ,perl)))                 ;for test harness
    (arguments netsurf-buildsystem-arguments)
    (home-page "http://www.netsurf-browser.org/projects/libparserutils/")
    (synopsis "Parser building library")
    (description
     "LibParserUtils is a library for building efficient parsers, written in
C.  It is developed as part of the NetSurf project.")
    (license l:expat)))

(define-public hubbub
  (package
    (name "hubbub")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           "lib" name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "13yq1k96a7972x4r71i9bcsz9yiglj0yx7lj0ziq5r94w5my70ma"))
       (patches (search-patches "hubbub-sort-entities.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)
       ("pkg-config" ,pkg-config)
       ("doxygen" ,doxygen)
       ("json-c" ,json-c-0.12)      ; check whether json-c-0.12 can be removed
       ("perl" ,perl)))
    (propagated-inputs
     `(("libparserutils" ,libparserutils))) ;for libhubbub.pc
    (arguments netsurf-buildsystem-arguments)
    (home-page "http://www.netsurf-browser.org/projects/hubbub/")
    (synopsis "HTML5 compliant parsing library")
    (description
     "Hubbub is an HTML5 compliant parsing library, written in C, which can
parse both valid and invalid web content.  It is developed as part of the
NetSurf project.")
    (license l:expat)))

(define-public ikiwiki
  (package
    (name "ikiwiki")
    (version "3.20170111")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://snapshot.debian.org/archive/debian/"
                           "20170111T215449Z/pool/main/i/ikiwiki/ikiwiki_"
                           version ".tar.xz"))
       (sha256
        (base32
         "00d7yzv426fvqbhvzyafddv7fa6b4j2647b0wi371wd5yjj9j3sz"))))
    (build-system perl-build-system)
    (arguments
     `(;; Image tests fail
       ;;
       ;; Test Summary Report
       ;; -------------------
       ;; t/img.t                      (Wstat: 2304 Tests: 62 Failed: 9)
       ;;   Failed tests:  21, 27-28, 30-35
       ;;   Non-zero exit status: 9
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'include-PERL5LIB-in-wrapper
           (lambda _
             (substitute* "IkiWiki/Wrapper.pm"
               (("^@wrapper\\_hooks")
                (string-append
                 "@wrapper_hooks\n"
                 "        addenv(\"PERL5LIB\", \""
                 (getenv "PERL5LIB")
                 "\");")))))
         (add-after 'patch-source-shebangs 'patch-Makefile
           (lambda _
             (substitute* "Makefile.PL"
               (("SYSCONFDIR\\?=") "SYSCONFDIR?=$(PREFIX)"))
             #t))
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin/"))
                    (path (getenv "PERL5LIB")))
               (for-each (lambda (file)
                           (wrap-program file
                             `("PERL5LIB" ":" prefix (,path))))
                         (find-files bin))
               #t))))))
    (native-inputs
     `(("which" ,which)
       ("perl-html-tagset" ,perl-html-tagset)
       ("perl-timedate" ,perl-timedate)
       ("perl-xml-sax" ,perl-xml-sax)
       ("perl-xml-simple" ,perl-xml-simple)
       ("gettext" ,gettext-minimal)
       ("subversion" ,subversion)
       ("git" ,git)
       ("bazaar" ,bazaar)
       ("cvs" ,cvs)
       ("mercurial" ,mercurial)))
    (inputs
     `(("python" ,python-wrapper)
       ("perl-cgi-formbuilder" ,perl-cgi-formbuilder)
       ("perl-cgi-session" ,perl-cgi-session)
       ("perl-cgi-simple" ,perl-cgi-simple)
       ("perl-db-file" ,perl-db-file)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-html-scrubber" ,perl-html-scrubber)
       ("perl-html-template" ,perl-html-template)
       ("perl-image-magick" ,perl-image-magick)
       ("perl-json" ,perl-json)
       ("perl-text-markdown-discount" ,perl-text-markdown-discount)
       ("perl-uri" ,perl-uri)
       ("perl-yaml-libyaml" ,perl-yaml-libyaml)))
    (home-page "https://ikiwiki.info/")
    (synopsis "Wiki compiler, capable of generating HTML")
    (description
     "Ikiwiki is a wiki compiler, capable of generating a static set of web
pages, but also incorporating dynamic features like a web based editor and
commenting.")
    (license l:gpl2+)))

(define-public libwapcaplet
  (package
    (name "libwapcaplet")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "134pljlm8kby1yy49826f0ixnpig8iqak6xpyl3aivagnsjnxzy8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)
       ("pkg-config" ,pkg-config)
       ("check" ,check)))               ;for tests
    (arguments netsurf-buildsystem-arguments)
    (home-page "http://www.netsurf-browser.org/projects/libwapcaplet/")
    (synopsis "String internment library")
    (description
     "LibWapcaplet provides a reference counted string internment system
designed to store small strings and allow rapid comparison of them.  It is
developed as part of the Netsurf project.")
    (license l:expat)))

(define-public libcss
  (package
    (name "libcss")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "0pxdqbxn6brj03nv57bsvac5n70k4scn3r5msaw0jgn2k06lk81m"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)
       ("pkg-config" ,pkg-config)
       ("perl" ,perl)))
    (propagated-inputs                  ;needed for libcss.pc
     `(("libparserutils" ,libparserutils)
       ("libwapcaplet" ,libwapcaplet)))
    (arguments netsurf-buildsystem-arguments)
    (home-page "http://www.netsurf-browser.org/projects/libcss/")
    (synopsis "CSS parser and selection library")
    (description
     "LibCSS is a CSS (Cascading Style Sheet) parser and selection engine,
written in C.  It is developed as part of the NetSurf project.")
    (license l:expat)))

(define-public libdom
  (package
    (name "libdom")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "1919757mdl3gii2pl6kzm8b1cal0h06r5nqd2y0kny6hc5yrhsp0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)
       ("pkg-config" ,pkg-config)
       ("perl" ,perl)                   ;for test harness
       ("perl-libxml" ,perl-libxml)
       ("perl-switch" ,perl-switch)
       ("perl-xml-xpath" ,perl-xml-xpath)))
    (inputs
     `(("libparserutils" ,libparserutils)
       ("libwapcaplet" ,libwapcaplet)))
    (propagated-inputs
     `(("expat" ,expat)                 ;needed for headers and linking
       ("hubbub" ,hubbub)))             ;for libdom.pc
    (arguments
     `(#:tests? #f                 ;TODO: re-enable. tests take a looong time.
       ,@netsurf-buildsystem-arguments))
    (home-page "http://www.netsurf-browser.org/projects/libdom/")
    (synopsis "Implementation of the W3C DOM")
    (description
     "LibDOM is an implementation of the W3C DOM, written in C.  It is
developed as part of the NetSurf project.")
    (license l:expat)))

(define-public libsvgtiny
  (package
    (name "libsvgtiny")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "10bpkmvfpydj74im3r6kqm9vnvgib6afy0alx71q5n0w5yawy39c"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)
       ("pkg-config" ,pkg-config)
       ("gperf" ,gperf-3.0)))
    (inputs
     `(("libwapcaplet" ,libwapcaplet)))
    (propagated-inputs
     `(("libdom" ,libdom)))             ;for libsvgtiny.pc
    (arguments netsurf-buildsystem-arguments)
    (home-page "http://www.netsurf-browser.org/projects/libsvgtiny/")
    (synopsis "Library for parsing SVG files")
    (description
     "Libsvgtiny takes some SVG as input and returns a list of paths and texts
which can be rendered easily, as defined in
@url{http://www.w3.org/TR/SVGMobile/}.  It is developed as part of the NetSurf
project.")
    (license l:expat)))

(define-public libnsbmp
  (package
    (name "libnsbmp")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "0lib2m07d1i0k80m4blkwnj0g7rha4jbm5vrgd0wwbkyfa0hvk35"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)))
    (arguments netsurf-buildsystem-arguments)
    (home-page "http://www.netsurf-browser.org/projects/libnsbmp/")
    (synopsis "Decoding library for BMP and ICO files")
    (description
     "Libnsbmp is a decoding library for BMP and ICO image file formats,
written in C.  It is developed as part of the NetSurf project.")
    (license l:expat)))

(define-public libnsgif
  (package
    (name "libnsgif")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "0jwshypgmx16xlsbx3d8njk8a5khazlplca5mxd3rdbhrlsabbly"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)))
    (arguments netsurf-buildsystem-arguments)
    (home-page "http://www.netsurf-browser.org/projects/libnsgif/")
    (synopsis "Decoding library for GIF files")
    (description
     "Libnsgif is a decoding library for the GIF image file format, written in
C.  It is developed as part of the NetSurf project.")
    (license l:expat)))

(define-public libnsutils
  (package
    (name "libnsutils")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "09w1rixps1iiq6wirjwxmd6h87llvjzvw565rahjb3rlyhcplfqf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)))
    (arguments netsurf-buildsystem-arguments)
    (home-page "http://www.netsurf-browser.org/")
    (synopsis "Utility library for NetSurf")
    (description
     "Libnsutils provides a small number of useful utility routines.  It is
developed as part of the NetSurf project.")
    (license l:expat)))

(define-public libnspsl
  (package
    (name "libnspsl")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "1rsk1k2a495axxgv8060s0p1phhhcxrv75252kllbkvr8id5kqld"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)))
    (arguments netsurf-buildsystem-arguments)
    (home-page "http://www.netsurf-browser.org/")
    (synopsis "Library to generate a static Public Suffix List")
    (description
     "Libnspsl is a library to generate a static code representation of the
Public Suffix List.  It is developed as part of the NetSurf project.")
    (license l:expat)))

(define-public nsgenbind
  (package
    (name "nsgenbind")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "0v1cb1rz5fix9ql31nzmglj7sybya6d12b2fkaypm1avcca59xwj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)
       ("bison" ,bison)
       ("flex" ,flex)))
    (arguments
     (substitute-keyword-arguments netsurf-buildsystem-arguments
       ((#:make-flags flags)
        `(delete "COMPONENT_TYPE=lib-shared" ,flags))))
    (home-page "http://www.netsurf-browser.org/")
    (synopsis "Generate JavaScript to DOM bindings")
    (description
     "@code{nsgenbind} is a tool to generate JavaScript to DOM bindings from
w3c webidl files and a binding configuration file.")
    (license l:expat)))

(define-public netsurf
  (package
    (name "netsurf")
    (version "3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/netsurf/"
                           "releases/source/netsurf-" version "-src.tar.gz"))
       (sha256
        (base32
         "0hjm1h4m1i913y4mhkl7yqdifn8k70fwi58zdh6faypawzryc3m0"))
       (patches (search-patches "netsurf-system-utf8proc.patch"
                                "netsurf-y2038-tests.patch"
                                "netsurf-longer-test-timeout.patch"
                                "netsurf-message-timestamp.patch"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)
       ("nsgenbind" ,nsgenbind)
       ("libidn" ,libidn)               ;only for tests
       ("check" ,check)
       ("perl" ,perl)
       ("perl-html-parser" ,perl-html-parser)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("gtk+" ,gtk+-2)
       ("openssl" ,openssl)
       ("utf8proc" ,utf8proc)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg)
       ("libcss" ,libcss)
       ("libdom" ,libdom)
       ("libnsbmp" ,libnsbmp)
       ("libnsgif" ,libnsgif)
       ("libnspsl" ,libnspsl)
       ("libnsutils" ,libnsutils)
       ("libsvgtiny" ,libsvgtiny)
       ("miscfiles" ,miscfiles)))
    (arguments
     `(#:make-flags `("CC=gcc" "BUILD_CC=gcc"
                      ,(string-append "PREFIX=" %output)
                      ,(string-append "NSSHARED="
                                      (assoc-ref %build-inputs
                                                 "netsurf-buildsystem")
                                      "/share/netsurf-buildsystem"))
       #:test-target "test"
       #:modules ((ice-9 rdelim)
                  (ice-9 match)
                  (srfi srfi-1)
                  (sxml simple)
                  ,@%glib-or-gtk-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'adjust-welcome
           (lambda _
             ;; First, fix some unended tags and simple substitutions
             (substitute* "frontends/gtk/res/welcome.html"
               (("<(img|input)([^>]*)>" _ tag contents)
                (string-append "<" tag contents " />"))
               (("Licence") "License") ;prefer GNU spelling
               ((" open source") ", free software")
               (("web&nbsp;site") "website")
               ;; Prefer privacy-respecting default search engine
               (("www.google.co.uk") "www.duckduckgo.com/html")
               (("Google Search") "DuckDuckGo Search")
               (("name=\"btnG\"") ""))
             ;; Remove default links so it doesn't seem we're endorsing them
             (with-atomic-file-replacement "frontends/gtk/res/welcome.html"
               (lambda (in out)
                 ;; Leave the DOCTYPE header as is
                 (display (read-line in 'concat) out)
                 (sxml->xml
                  (let rec ((sxml (xml->sxml in)))
                    ;; We'd like to use sxml-match here, but it can't
                    ;; match against generic tag symbols...
                    (match sxml
                      (`(div (@ (class "links")) . ,rest)
                       '())
                      ((x ...)
                       (map rec x))
                      (x x)))
                  out)))
             #t))
         (add-before 'check 'patch-check
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("test/bloom.c" "test/hashtable.c")
               (("/usr/share/dict/words")
                (string-append (assoc-ref inputs "miscfiles") "/share/web2")))
             #t))
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (desktop (string-append out "/share/applications/"
                                            "netsurf.desktop")))
               (mkdir-p (dirname desktop))
               (copy-file "frontends/gtk/res/netsurf-gtk.desktop"
                          desktop)
               (substitute* desktop
                 (("netsurf-gtk") (string-append out "/bin/netsurf-gtk"))
                 (("netsurf.png") (string-append out "/share/netsurf/"
                                                 "netsurf.xpm")))
               (install-file "docs/netsurf-gtk.1"
                             (string-append out "/share/man/man1/"))
               #t))))))
    (home-page "http://www.netsurf-browser.org")
    (synopsis "Web browser")
    (description
     "NetSurf is a lightweight web browser that has its own layout and
rendering engine entirely written from scratch.  It is small and capable of
handling many of the web standards in use today.")
    (license l:gpl2+)))

(define-public surfraw
  (package
    (name "surfraw")
    (version "2.2.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://surfraw.alioth.debian.org/dist/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fy4ph5h9kp0jzj1m6pfylxnnmgdk0mmdppw76z9jhna4jndk5xa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-perl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((perl (assoc-ref inputs "perl")))
               (substitute* "surfraw.IN"
                 (("perl -e")
                  (string-append perl "/bin/perl -e")))
               #t)))
         (add-after 'install 'compress-elvi.1sr
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The manpages of the elvis are symlinks to elvi.1sr.gz
             ;; but elvi.1sr does not get compressed by our manpage phase.
             (let* ((out (assoc-ref %outputs "out"))
                    (man (string-append out "/share/man/man1")))
               (with-directory-excursion man
                 (invoke "gzip" "elvi.1sr"))))))))
    (inputs
     `(("perl" ,perl)
       ("perl-www-opensearch" ,perl-www-opensearch)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-libwww" ,perl-libwww)))
    (synopsis "Unix command line interface to the www")
    (description "Surfraw (Shell Users' Revolutionary Front Rage Against the Web)
provides a unix command line interface to a variety of popular www search engines
and similar services.")
    (home-page "https://surfraw.alioth.debian.org/")
    (license l:public-domain)))

(define-public darkhttpd
  (package
    (name "darkhttpd")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://unix4lyfe.org/darkhttpd/darkhttpd-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "0185wlyx4iqiwfigp1zvql14zw7gxfacncii3d15yaxk4av1f155"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("CC=gcc")
       #:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "darkhttpd"
                           (string-append (assoc-ref outputs "out")
                                          "/bin"))
             #t)))))
    (synopsis "Simple static web server")
    (description "darkhttpd is a simple static web server.  It is
standalone and does not need inetd or ucspi-tcp.  It does not need any
config files---you only have to specify the www root.")
    (home-page "https://unix4lyfe.org/darkhttpd/")
    (license l:isc)))

(define-public goaccess
  (package
    (name "goaccess")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://tar.goaccess.io/goaccess-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1w84y61f3ldg2f28q6qlyr1scn3mcx0bsbq3i5xi5w193wh3xa2q"))
              (modules '((guix build utils)))
              (snippet '(begin
                          (substitute* "src/error.h"
                            (("__DATE__") "\"1970-01-01\"")
                            (("__TIME__") "\"00:00:00\""))
                          #t))))
    (build-system gnu-build-system)
    (inputs
     ;; TODO: Add dependency on geoip-tools.
     `(("glib" ,glib)
       ("ncurses" ,ncurses)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://goaccess.io")
    (synopsis "Analyze Web server logs in real time")
    (description
     "GoAccess is a real-time web log analyzer and interactive viewer that
runs in a terminal or through your browser.  It provides fast and valuable
HTTP statistics for system administrators that require a visual server report
on the fly.")
    (license l:x11)))

(define-public httptunnel
  (package
    (name "httptunnel")
    (version "3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.nocrew.org/software/httptunnel/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mn5s6p68n32xzadz6ds5i6bp44dyxzkq68r1yljlv470jr84bql"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Remove non-free IETF RFC documentation.
                   (delete-file-recursively "doc")
                   #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The default configure phase tries to pass environment variables as
         ;; command-line arguments, which confuses the ./configure script.
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (invoke "./configure"
                       (string-append "--prefix=" out))))))))
    (home-page "http://www.nocrew.org/software/httptunnel.html")
    (synopsis "Tunnel data connections through HTTP requests")
    (description "httptunnel creates a bidirectional virtual data connection
tunnelled through HTTP (HyperText Transfer Protocol) requests.  This can be
useful for users behind restrictive firewalls.  As long as Web traffic is
allowed, even through a HTTP-only proxy, httptunnel can be combined with other
tools like SSH (Secure Shell) to reach the outside world.")
    (license l:gpl2+)))

(define-public stunnel
  (package
  (name "stunnel")
  (version "5.50")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "https://www.stunnel.org/downloads/stunnel-"
                          version ".tar.gz"))
      (sha256
       (base32 "0j811iakljjxw39qchmqf235jdkwixb0i4xxjyi55f08558947cm"))))
  (build-system gnu-build-system)
  (native-inputs
   ;; For tests.
   `(("iproute" ,iproute)
     ("netcat" ,netcat)
     ("procps" ,procps)))
  (inputs `(("openssl" ,openssl)))
  (arguments
   `(#:configure-flags
     (list (string-append "--with-ssl=" (assoc-ref %build-inputs "openssl")))
     #:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'patch-output-directories
         (lambda _
           ;; Some (not all) Makefiles have a hard-coded incorrect docdir.
           (substitute* (list "Makefile.in"
                              "doc/Makefile.in"
                              "tools/Makefile.in")
             (("/doc/stunnel")
              (string-append "/doc/" ,name "-" ,version)))
           #t))
       (add-before 'check 'patch-tests
         (lambda _
           (substitute* "tests/make_test"
             (("/bin/sh ")
              (string-append (which "sh") " ")))
           #t)))))
  (home-page "https://www.stunnel.org")
  (synopsis "TLS proxy for clients or servers")
  (description "Stunnel is a proxy designed to add TLS encryption
functionality to existing clients and servers without any changes in the
programs' code.  Its architecture is optimized for security, portability, and
scalability (including load-balancing), making it suitable for large
deployments.")
  (license l:gpl2+)))

(define-public varnish
  (package
    (name "varnish")
    (home-page "https://varnish-cache.org/")
    (version "6.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "_downloads/varnish-" version ".tgz"))
              (sha256
               (base32
                "0lwfk2gq99c653h5f51fs3j37r0gh2pf0p4w5z986nm2mi9z6yn3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib")
                               ;; Use absolute path of GCC so it's found at runtime.
                               (string-append "PTHREAD_CC="
                                              (assoc-ref %build-inputs "gcc")
                                              "/bin/gcc")
                               "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-absolute-file-names
           (lambda _
             (substitute* '("bin/varnishtest/vtc_varnish.c"
                            "bin/varnishtest/vtc_process.c"
                            "bin/varnishd/mgt/mgt_vcc.c")
               (("/bin/sh") (which "sh")))
             (substitute* "bin/varnishd/mgt/mgt_shmem.c"
               (("rm -rf") (string-append (which "rm") " -rf")))
             (substitute* "bin/varnishtest/vtc_main.c"
               (("/bin/rm") (which "rm")))
             #t))
         (add-before 'install 'patch-Makefile
           (lambda _
             (substitute* "Makefile"
               ;; Do not create /var/varnish during install.
               (("^install-data-am: install-data-local") "install-data-am: "))
             #t))
         (add-after 'install 'wrap-varnishd
           ;; Varnish uses GCC to compile VCL, so wrap it with required GCC
           ;; environment variables to avoid propagating them to profiles.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (varnishd (string-append out "/sbin/varnishd"))
                    (PATH (string-append (assoc-ref inputs "binutils") "/bin"))
                    (LIBRARY_PATH (string-append (assoc-ref inputs "libc") "/lib")))
               (wrap-program varnishd
                 ;; Add binutils to PATH so gcc finds the 'as' executable.
                 `("PATH" ":" prefix (,PATH))
                 ;; Make sure 'crti.o' et.al is found.
                 `("LIBRARY_PATH" ":" prefix (,LIBRARY_PATH)))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)
       ("rst2man" ,python-docutils)))
    (inputs
     `(("jemalloc" ,jemalloc)
       ("ncurses" ,ncurses)
       ("pcre" ,pcre)
       ("python" ,python-wrapper)
       ("readline" ,readline)))
    (synopsis "Web application accelerator")
    (description
     "Varnish is a high-performance HTTP accelerator.  It acts as a caching
reverse proxy and load balancer.  You install it in front of any server that
speaks HTTP and configure it to cache the contents through an extensive
configuration language.")
    (license (list l:bsd-2           ;main distribution
                   l:zlib            ;lib/libvgz/*
                   l:public-domain   ;bin/varnishncsa/as64.c, include/miniobj.h
                   l:bsd-3))))       ;include/vqueue.h, lib/libvarnishcompat/daemon.c

(define-public varnish-modules
  (package
    (name "varnish-modules")
    (home-page "https://github.com/varnish/varnish-modules")
    (version "0.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.varnish-software.com"
                                  "/varnish-modules/varnish-modules-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "09li9lqa1kb275w1rby2zldyg8r9cfcl4qyv53qyd9xbzilrz751"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python)
       ("varnish" ,varnish)))
    (synopsis "Collection of Varnish modules")
    (description
     "This package provides a collection of modules (@dfn{vmods}) for the Varnish
cache server, extending the @dfn{Varnish Configuration Language} (VCL) with
additional capabilities.")
    (license l:bsd-2)))

(define-public xinetd
  (package
    (name "xinetd")
    (version "2.3.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xinetd-org/xinetd.git")
             (commit (string-append "xinetd-"
                                    (string-join (string-split version #\.)
                                                 "-")))))
       (file-name (git-file-name name version))
       (patches (search-patches "xinetd-CVE-2013-4342.patch"
                                "xinetd-fix-fd-leak.patch"))
       (sha256
        (base32 "0wjai6qagcgxpa1khh639ih7kswgkryc7ll1i4hxhs29sc7irdcn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-loadavg")
       #:tests? #f))                    ; no tests
    (home-page "https://github.com/xinetd-org/xinetd")
    (synopsis "Internet services daemon")
    (description "@code{xinetd}, a more secure replacement for @code{inetd},
listens for incoming requests over a network and launches the appropriate
service for that request.  Requests are made using port numbers as identifiers
and xinetd usually launches another daemon to handle the request.  It can be
used to start services with both privileged and non-privileged port numbers.")
    (license (l:fsf-free "file://COPYRIGHT"))))

(define-public tidy-html
  (package
    (name "tidy-html")
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/htacg/tidy-html5/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0n29wcgw32rhnraj9j21ibhwi0xagmmcskhbaz8ihxly7nx3p9h8"))))
    (build-system cmake-build-system)
    (outputs '("out"
               "static"))               ; 1.0MiB of .a files
    (arguments
     `(#:tests? #f                      ; no tests available
       #:build-type "Release"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move static libraries to the "static" output.
             (let* ((out    (assoc-ref outputs "out"))
                    (lib    (string-append out "/lib"))
                    (static (assoc-ref outputs "static"))
                    (slib   (string-append static "/lib")))
               (mkdir-p slib)
               (for-each (lambda (file)
                           (install-file file slib)
                           (delete-file file))
                         (find-files lib "\\.a$"))
               #t))))))
    (native-inputs
     `(("libxslt" ,libxslt)))
    (home-page "http://www.html-tidy.org/")
    (synopsis "HTML Tidy with HTML5 support")
    (description
     "Tidy is a console application which corrects and cleans up
HTML and XML documents by fixing markup errors and upgrading
legacy code to modern standards.

Tidy also provides @code{libtidy}, a C static and dynamic library that
developers can integrate into their applications to make use of the
functions of Tidy.")
    (license l:bsd-3)))

(define-public hiawatha
  (package
    (name "hiawatha")
    (version "10.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.hiawatha-webserver.org/files/"
                           "hiawatha-" version ".tar.gz"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; We use packaged libraries, so delete the bundled copies.
                   (for-each delete-file-recursively
                             (list "extra/nghttp2.tgz" "mbedtls"))
                   #t))
       (sha256
        (base32 "1f2j2x1ziawz8ijg3s3izqpyzpiwfyhlsvbv0szxvhvj4a0l7pbl"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests included
       #:configure-flags (list (string-append "-DUSE_SYSTEM_MBEDTLS=on")
                               (string-append "-DENABLE_HTTP2=on")
                               (string-append "-DUSE_SYSTEM_NGHTTP2=on")
                               (string-append "-DENABLE_TOMAHAWK=on")
                               (string-append "-DLOG_DIR=/var/log/hiawatha")
                               (string-append "-DPID_DIR=/run")
                               (string-append "-DWEBROOT_DIR="
                                              (assoc-ref %outputs "out")
                                              "/share/hiawatha/html")
                               (string-append "-DWORK_DIR=/var/lib/hiawatha"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'install-no-empty-directories
           (lambda _
             (substitute* "CMakeLists.txt"
               (("install\\(DIRECTORY DESTINATION" match)
                (string-append "#" match)))
             #t))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'hiawatha' finds 'mbedtls'.
             (let* ((out (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin"))
                    (mbed (assoc-ref inputs "mbedtls-apache")))
               (wrap-program (string-append sbin "/hiawatha")
                 `("PATH" ":" prefix (,mbed)))))))))
    (inputs
     ;; TODO: package "hiawatha-monitor", an optional dependency of "hiawatha".
     `(("libxslt" ,libxslt)
       ("libxml2" ,libxml2)
       ("mbedtls-apache" ,mbedtls-for-hiawatha)
       ("nghttp2" ,nghttp2 "lib")
       ("zlib" ,zlib)))
    (home-page "https://www.hiawatha-webserver.org")
    (synopsis "Webserver with focus on security")
    (description
     "Hiawatha has been written with security in mind.
Features include the ability to stop SQL injections, XSS and CSRF attacks and
exploit attempts.")
    (license l:gpl2)))

(define-public python-httpbin
  (package
    (name "python-httpbin")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "httpbin" version))
       (sha256
        (base32
         "1dc92lnk846hpilslrqnr63x55cxll4qx88gif8fm521gv9cbyvr"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-decorator" ,python-decorator)
       ("python-flask" ,python-flask)
       ("python-itsdangerous" ,python-itsdangerous)
       ("python-markupsafe" ,python-markupsafe)
       ("python-six" ,python-six)))
    (home-page "https://github.com/Runscope/httpbin")
    (synopsis "HTTP request and response service")
    (description "Testing an HTTP Library can become difficult sometimes.
@code{RequestBin} is fantastic for testing POST requests, but doesn't let you control the
response.  This exists to cover all kinds of HTTP scenarios.  All endpoint responses are
JSON-encoded.")
    (license l:isc)))

(define-public python2-httpbin
  (package-with-python2 python-httpbin))

(define-public python-pytest-httpbin
  (package
    (name "python-pytest-httpbin")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-httpbin" version))
       (sha256
        (base32
         "1y0v2v7xpzpyd4djwp7ad8ifnlxp8r1y6dfbxg5ckzvllkgridn5"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-httpbin" ,python-httpbin)
       ("python-pytest" ,python-pytest)))
    (home-page
     "https://github.com/kevin1024/pytest-httpbin")
    (synopsis
     "Test your HTTP library against a local copy of httpbin")
    (description
     "@code{Pytest-httpbin} creates a @code{pytest} fixture that is dependency-injected
into your tests.  It automatically starts up a HTTP server in a separate thread running
@code{httpbin} and provides your test with the URL in the fixture.")
    (license l:expat)))

(define-public python2-pytest-httpbin
  (package-with-python2 python-pytest-httpbin))

(define-public http-parser
  (package
    (name "http-parser")
    (version "2.9.0")
    (home-page "https://github.com/nodejs/http-parser")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qa3rh9x4as2fc2p0y2hah83iqs7jr5106a8anv317359dgf3ssj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list (string-append "PREFIX="
                            (assoc-ref %outputs "out"))
             "CC=gcc" "library")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (synopsis "HTTP request/response parser for C")
    (description "This is a parser for HTTP messages written in C.  It parses
both requests and responses.  The parser is designed to be used in
high-performance HTTP applications.  It does not make any syscalls nor
allocations, it does not buffer data, it can be interrupted at anytime.
Depending on your architecture, it only requires about 40 bytes of data per
message stream (in a web server that is per connection).")
    (license l:expat)))

(define-public python2-httpretty
  (package
    (name "python2-httpretty")
    (version "0.8.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "httpretty" version))
       (sha256
        (base32
         "0vlp5qkyw3pxwwsg7xmdcfh1csvypvaz4m6abida8s4xmjxpdhc3"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-sphinx-rtd-theme" ,python2-sphinx-rtd-theme)
       ("python-sphinx" ,python2-sphinx)
       ("python-coverage" ,python2-coverage)
       ("python-tornado" ,python2-tornado)
       ("python-urllib3" ,python2-urllib3)
       ("python-sure" ,python2-sure)
       ("python-steadymark" ,python2-steadymark)
       ("python-requests" ,python2-requests)
       ("python-rednose" ,python2-rednose)
       ("python-nose-randomly" ,python2-nose-randomly)
       ("python-misaka" ,python2-misaka)
       ("python-pytest-httpbin" ,python2-pytest-httpbin)
       ("python-nose" ,python2-nose)))
    (arguments
     `(#:tests? #f
       ;; Requires mock>=1.3.0 which requires a more up-to-date
       ;; python-pbr. After updating these trying to build the
       ;; package leads to failures in python-flake8 and other
       ;; packages. The cascade of updates and failures this
       ;; leads to, seems to not be worth having the test run.
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-test-requirements
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Update requirements from dependecy==version
             ;; to dependency>=version
             (substitute* "development.txt"
               (("==") ">="))
             #t)))))
    (home-page "http://github.com/gabrielfalcao/httpretty")
    (synopsis "HTTP client mock for Python")
    (description "@code{httpretty} is a helper for faking web requests,
inspired by Ruby's @code{fakeweb}.")
    (license l:expat)))

(define-public jo
  (package
    (name "jo")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/jpmens/jo/releases/download/"
                           version "/jo-" version ".tar.gz"))
       (sha256
        (base32
         "1bmdck53jslrl3anqqpm6iyjdxrz445qzcc4fr37hr3wjg22zv1n"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/jpmens/jo")
    (synopsis "Output JSON from a shell")
    (description "jo is a command-line utility to create JSON objects or
arrays.  It creates a JSON string on stdout from words provided as
command-line arguments or read from stdin.")
    (license (list l:gpl2+
                   l:expat)))) ; json.c, json.h

(define-public python-internetarchive
  (package
    (name "python-internetarchive")
    (version "1.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/jjjake/internetarchive/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0sdbb2ag6vmybi8zmbjszi492a587giaaqxyy1p6gy03cb8mc512"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (setenv "PATH" (string-append (assoc-ref outputs "out") "/bin"
                                           ":" (getenv "PATH")))
             (invoke "py.test" "-v" "-k"
                     (string-append
                      ;; These tests attempt to make a connection to
                      ;; an external web service.
                      "not test_get_item_with_kwargs"
                      " and not test_ia")))))))
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("python-jsonpatch" ,python-jsonpatch-0.4)
       ("python-docopt" ,python-docopt)
       ("python-clint" ,python-clint)
       ("python-six" ,python-six)
       ("python-schema" ,python-schema-0.5)
       ("python-backports-csv" ,python-backports-csv)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-capturelog" ,python-pytest-capturelog)
       ("python-responses" ,python-responses)))
    (home-page "https://github.com/jjjake/internetarchive")
    (synopsis "Command-line interface to archive.org")
    (description "@code{ia} is a command-line tool for using
@url{archive.org} from the command-line.  It also emplements the
internetarchive python module for programatic access to archive.org.")
    (properties
     `((python2-variant . ,(delay python2-internetarchive))))
    (license l:agpl3+)))

(define-public python2-internetarchive
  (package-with-python2
   (strip-python2-variant python-internetarchive)))

(define-public python-clf
  (let ((commit-test-clf "d01d25923c599d3261910f79fb948825b4270d07")) ; 0.5.7
    (package
      (name "python-clf")
      (version "0.5.7")
      (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "clf" version))
         (sha256
          (base32
           "0zlkzqnpz7a4iavsq5vaz0nf5nr7qm5znpg1vlpz6rwnx6hikjdb"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-docopt" ,python-docopt)
         ("python-pygments" ,python-pygments)
         ("python-requests" ,python-requests)
         ("python-nose" ,python-nose)
         ("python-lxml" ,python-lxml)
         ("python-pyaml" ,python-pyaml)))
      (inputs
       `(("test-clf"
          ,(origin
             (method url-fetch)
             (uri (string-append "https://raw.githubusercontent.com"
                                 "/ncrocfer/clf/" commit-test-clf
                                 "/test_clf.py"))
             (sha256
              (base32
               "19lr5zdzsmxgkg7wrjq1yzkiahd03wi4k3dskssyhmjls8c10nqd"))))))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'get-tests
             (lambda _
               (copy-file (assoc-ref %build-inputs "test-clf") "test_clf.py")
               #t))
           (replace 'check
             (lambda _
               (invoke "nosetests"
                       ;; These tests require an Internet connection.
                       "--exclude=test_browse"
                       "--exclude=test_command"
                       "--exclude=test_search"))))))
      (home-page "https://github.com/ncrocfer/clf")
      (synopsis "Search code snippets on @url{https://commandlinefu.com}")
      (description "@code{clf} is a command line tool for searching code
snippets on @url{https://commandlinefu.com}.")
      (license l:expat))))

(define-public python2-clf
  (package-with-python2 python-clf))

(define-public rss-bridge
  (package
    (name "rss-bridge")
    (version "2019-01-13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/RSS-Bridge/rss-bridge")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1m0dq491954f0d7k4508ddlywk09whcz9j21rc4yk3lbwpf0nd4c"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let* ((out (assoc-ref %outputs "out"))
                (share-rss-bridge (string-append out "/share/rss-bridge")))
           (mkdir-p share-rss-bridge)
           (copy-recursively (assoc-ref %build-inputs "source") share-rss-bridge)
           #t))))
    (home-page "https://github.com/RSS-Bridge/rss-bridge")
    (synopsis "Generate Atom feeds for social networking websites")
    (description "rss-bridge generates Atom feeds for social networking
websites lacking feeds.  Supported websites include Facebook, Twitter,
Instagram and YouTube.")
    (license (list l:public-domain
                   l:expat)))) ;; vendor/simplehtmldom/simple_html_dom.php

(define-public linkchecker
  (package
    (name "linkchecker")
    (version "9.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linkchecker/linkchecker")
             (commit (string-append "v" version))))
       (patches
        (search-patches
         "linkchecker-mark-more-tests-that-require-the-network.patch"))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03ihjmc4bqxxqv71bb43r2f23sx0xnbq1k2fsg9fw05qa5s9x187"))))
    (build-system python-build-system)
    (inputs
     `(("python2-dnspython" ,python2-dnspython)
       ("python2-pyxdg" ,python2-pyxdg)
       ("python2-requests" ,python2-requests)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("python2-pytest" ,python2-pytest)
       ("python2-miniboa" ,python2-miniboa)
       ("python2-parameterized" ,python2-parameterized)))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         ;; Move the 'check phase to after 'install, so that the installed
         ;; library can be used
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Set PYTHONPATH so that the installed linkchecker is used
               (setenv "PYTHONPATH"
                       (string-append out "/lib/python2.7/site-packages"
                                      ":"
                                      (getenv "PYTHONPATH")))
               ;; Remove this directory to avoid it being used when running
               ;; the tests
               (delete-file-recursively "linkcheck")

               (invoke "py.test" "tests"))
             #t)))))
    (home-page "https://linkcheck.github.io/linkchecker")
    (synopsis "Check websites for broken links")
    (description "LinkChecker is a website validator.  It checks for broken
links in websites.  It is recursive and multithreaded providing output in
colored or normal text, HTML, SQL, CSV, XML or as a sitemap graph.  It
supports checking HTTP/1.1, HTTPS, FTP, mailto, news, nntp, telnet and local
file links.")
    (license (list l:gpl2+
                   l:bsd-2              ; linkcheck/better_exchook2.py
                   l:bsd-3              ; linkcheck/colorama.py
                   l:psfl               ; linkcheck/gzip2.py
                   l:expat))))          ; linkcheck/mem.py

(define-public cadaver
  (package
    (name "cadaver")
    (version "0.23.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.webdav.org/cadaver/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jizq69ifrjbjvz5y79wh1ny94gsdby4gdxwjad4bfih6a5fck7x"))))
    (build-system gnu-build-system)
    ;; TODO: Unbundle libneon and make build succeed with new neon.
    (arguments
     `(#:configure-flags (list "--with-ssl=openssl")
       #:tests? #f)) ;No tests included
    (native-inputs
     `(("gettext" ,gnu-gettext)
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("expat" ,expat)
       ("openssl" ,openssl)))
    (home-page "http://www.webdav.org/cadaver")
    (synopsis "Command-line WebDAV client")
    (description
     "Cadaver is a command-line WebDAV client for Unix. It supports
file upload, download, on-screen display, namespace operations (move/copy),
collection creation and deletion, and locking operations.")
    (license l:gpl2)))

(define-public python-py-ubjson
  (package
    (name "python-py-ubjson")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py-ubjson" version))
       (sha256
        (base32
         "03l9m9w5ip4hw0y69wlys5gzsfb7zcq3a77blj88grgiqhn5vm5n"))))
    (build-system python-build-system)
    (home-page "https://github.com/Iotic-Labs/py-ubjson")
    (synopsis "Universal Binary JSON encoder/decoder")
    (description
     "Py-ubjson is a Python module providing an Universal Binary JSON
encoder/decoder based on the draft-12 specification for UBJSON.")
    (license l:asl2.0)))

(define-public java-tomcat
  (package
    (name "java-tomcat")
    (version "8.5.38")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/tomcat/tomcat-8/v"
                                  version "/src/apache-tomcat-" version "-src.tar.gz"))
              (sha256
               (base32
                "13pbsyk39g1qph82nngp54mqycmg60rxlxwy4yszsssahrqnggb2"))
              (modules '((guix build utils)))
              ;; Delete bundled jars.
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (inputs
     `(("java-eclipse-jdt-core" ,java-eclipse-jdt-core)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (arguments
     `(#:build-target "package"
       #:tests? #f; requires downloading some files.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prevent-download
           (lambda _
             ;; This directory must exist
             (mkdir "downloads")
             ;; We patch build.xml so it doesn't download any dependency, because
             ;; we already have all of them.
             (substitute* "build.xml"
               (("download-compile,") "")
               (("depends=\"validate\"") "depends=\"build-prepare\"")
               ((",download-validate") ""))
             #t))
         (add-after 'unpack 'strip-timestamps
           (lambda _
             (substitute* "build.xml"
               (("<filter token=\"YEAR\" value=.*")
                "<filter token=\"YEAR\" value=\"1970\"/>")
               (("<filter token=\"VERSION_BUILT\" value=.*")
                "<filter token=\"VERSION_BUILT\" value=\"Jan 1 1970 00:00:00 UTC\"/>"))
             #t))
         (add-after 'unpack 'generate-properties
           (lambda _
             ;; This could have been passed to make-flags, but getcwd returns
             ;; a different directory then.
             (with-output-to-file "build.properties"
               (lambda _
                 (display
                   (string-append "base.path=" (getcwd) "/downloads\n"))))
             #t))
         (replace 'install
           (install-jars "output/build/lib")))))
    (home-page "https://tomcat.apache.org")
    (synopsis "Java Servlet, JavaServer Pages, Java Expression Language and Java
WebSocket")
    (description "Apache Tomcat is a free implementation of the Java
Servlet, JavaServer Pages, Java Expression Language and Java WebSocket
technologies.")
    (license l:asl2.0)))

(define-public java-eclipse-jetty-test-helper
  (package
    (name "java-eclipse-jetty-test-helper")
    (version "4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/eclipse/jetty.toolchain/"
                                  "archive/jetty-test-helper-" version ".tar.gz"))
              (sha256
               (base32
                "1jd6r9wc26fa11si4rn2gvy8ml8q4zw1nr6v04mjp8wvwpgvzwx5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "eclipse-jetty-test-helper.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-test-helper")
             #t))
         (add-before 'build 'fix-paths
           (lambda _
             ;; TODO:
             ;; This file assumes that the build directory is named "target"
             ;; but it is not the case with our ant-build-system. Once we have
             ;; maven though, we will have to rebuild this package because this
             ;; assumption is correct with maven-build-system.
             (substitute*
               "src/main/java/org/eclipse/jetty/toolchain/test/MavenTestingUtils.java"
               (("\"target\"") "\"build\"")
               (("\"tests\"") "\"test-classes\""))
             ;; Tests assume we are building with maven, so that the build
             ;; directory is named "target", and not "build".
             (with-directory-excursion "src/test/java/org/eclipse/jetty/toolchain/test"
               (substitute* '("FSTest.java" "OSTest.java" "TestingDirTest.java"
                              "MavenTestingUtilsTest.java")
                 (("target/tests") "build/test-classes")
                 (("\"target") "\"build")))
             #t)))))
    (inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-all)))
    (home-page "https://www.eclipse.org/jetty/")
    (synopsis "Helper classes for jetty tests")
    (description "This packages contains helper classes for testing the Jetty
Web Server.")
    ;; This program is licensed under both epl and asl.
    (license (list l:epl1.0 l:asl2.0))))

(define-public java-eclipse-jetty-perf-helper
  (package
    (inherit java-eclipse-jetty-test-helper)
    (name "java-eclipse-jetty-perf-helper")
    (arguments
     `(#:jar-name "eclipse-jetty-perf-helper.jar"
       #:source-dir "src/main/java"
       #:tests? #f; no tests
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-perf-helper")
             #t)))))
    (inputs
     `(("hdrhistogram" ,java-hdrhistogram)))))

(define-public java-eclipse-jetty-util
  (package
    (name "java-eclipse-jetty-util")
    (version "9.4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/eclipse/jetty.project/"
                                  "archive/jetty-" version ".v20170531.tar.gz"))
              (sha256
               (base32
                "0x7kbdvkmgr6kbsmbwiiyv3bb0d6wk25frgvld9cf8540136z9p1"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "eclipse-jetty-util.jar"
       #:source-dir "src/main/java"
       #:test-exclude
       (list "**/Abstract*.java"
             ;; requires network
             "**/InetAddressSetTest.java"
             ;; Assumes we are using maven
             "**/TypeUtilTest.java"
             ;; Error on the style of log
             "**/StdErrLogTest.java")
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-util")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-tomcat)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-all)
       ("perf-helper" ,java-eclipse-jetty-perf-helper)
       ("test-helper" ,java-eclipse-jetty-test-helper)))
    (home-page "https://www.eclipse.org/jetty/")
    (synopsis "Utility classes for Jetty")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides utility classes.")
    (license (list l:epl1.0 l:asl2.0))))

;; This version is required by maven-wagon
(define-public java-eclipse-jetty-util-9.2
  (package
    (inherit java-eclipse-jetty-util)
    (version "9.2.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/eclipse/jetty.project/"
                                  "archive/jetty-" version ".v20170606.tar.gz"))
              (sha256
               (base32
                "1i51qlsd7h06d35kx5rqpzbfadbcszycx1iwr6vz7qc9gf9f29la"))))
    (arguments
     `(#:jar-name "eclipse-jetty-util.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-exclude
       (list "**/Abstract*.java"
             ;; requires network
             "**/InetAddressSetTest.java"
             ;; Assumes we are using maven
             "**/TypeUtilTest.java"
             ;; We don't have an implementation for slf4j
             "**/LogTest.java"
             ;; Error on the style of log
             "**/StdErrLogTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-util")
             #t))
         (add-before 'build 'fix-test-sources
           (lambda _
             ;; We need to fix issues caused by changes in newer versions of
             ;; jetty-test-helper
             (let ((src "src/test/java/org/eclipse/jetty/util/resource"))
               (substitute* (string-append src "/AbstractFSResourceTest.java")
                 (("testdir.getDir\\(\\)") "testdir.getPath().toFile()")
                 (("testdir.getFile\\(\"foo\"\\)")
                  "testdir.getPathFile(\"foo\").toFile()")
                 (("testdir.getFile\\(name\\)")
                  "testdir.getPathFile(name).toFile()")))
             #t)))))))

(define-public java-eclipse-jetty-io
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-io")
    (arguments
     `(#:jar-name "eclipse-jetty-io.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-exclude (list "**/Abstract*.java"
                            ;; Abstract class
                            "**/EndPointTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-io")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-javaee-servletapi)
       ("util" ,java-eclipse-jetty-util)))
    (synopsis "Jetty :: IO Utility")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides IO-related utility classes.")))

(define-public java-eclipse-jetty-io-9.2
  (package
    (inherit java-eclipse-jetty-io)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("mockito" ,java-mockito-1)
       ("cglib" ,java-cglib)
       ("objenesis" ,java-objenesis)
       ("asm" ,java-asm)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-http
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-http")
    (arguments
     `(#:jar-name "eclipse-jetty-http.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-http")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "src/main/resources/" "build/classes/")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-javaee-servletapi)
       ("io" ,java-eclipse-jetty-io)
       ("util" ,java-eclipse-jetty-util)))
    (synopsis "Jetty :: Http Utility")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides HTTP-related utility classes.")))

(define-public java-eclipse-jetty-http-9.2
  (package
    (inherit java-eclipse-jetty-http)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ("io" ,java-eclipse-jetty-io-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-jmx
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-jmx")
    (arguments
     `(#:jar-name "eclipse-jetty-jmx.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; FIXME: requires com.openpojo.validation
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-jmx")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-javaee-servletapi)
       ("util" ,java-eclipse-jetty-util)))
    (synopsis "Jetty :: JMX Management")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides the JMX management.")))

(define-public java-eclipse-jetty-jmx-9.2
  (package
    (inherit java-eclipse-jetty-jmx)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))))

(define java-eclipse-jetty-http-test-classes
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-http-test-classes")
    (arguments
     `(#:jar-name "eclipse-jetty-http.jar"
       #:source-dir "src/test"
       #:tests? #f
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-http")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-tomcat)
       ("http" ,java-eclipse-jetty-http)
       ("io" ,java-eclipse-jetty-io)
       ("util" ,java-eclipse-jetty-util)))))

(define java-eclipse-jetty-http-test-classes-9.2
  (package
    (inherit java-eclipse-jetty-http-test-classes)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("http" ,java-eclipse-jetty-http-9.2)
       ,@(package-inputs java-eclipse-jetty-http-9.2)))))

(define-public java-eclipse-jetty-server
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-server")
    (arguments
     `(#:jar-name "eclipse-jetty-server.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; requires a mockito version we don't have
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-server")
             #t))
         (add-before 'build 'fix-source
           (lambda _
             ;; Explicit casts to prevent build failures
             (substitute* "src/main/java/org/eclipse/jetty/server/Request.java"
               (("append\\(LazyList")
                "append((CharSequence)LazyList"))
             (substitute*
               "src/main/java/org/eclipse/jetty/server/handler/ContextHandler.java"
               (((string-append
                   "Class<\\? extends EventListener> clazz = _classLoader==null"
                   "\\?Loader.loadClass\\(ContextHandler.class,className\\):"
                   "_classLoader.loadClass\\(className\\);"))
                (string-append "Class<? extends EventListener> clazz = "
                               "(Class<? extends EventListener>) "
                               "(_classLoader==null?Loader.loadClass("
                               "ContextHandler.class,className):"
                               "_classLoader.loadClass(className));")))
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-javaee-servletapi)
       ("http" ,java-eclipse-jetty-http)
       ("io" ,java-eclipse-jetty-io)
       ("jmx" ,java-eclipse-jetty-jmx)
       ("util" ,java-eclipse-jetty-util)))
    (native-inputs
     `(("test-classes" ,java-eclipse-jetty-http-test-classes)
       ,@(package-native-inputs java-eclipse-jetty-util)))
    (synopsis "Core jetty server artifact")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides the core jetty server
artifact.")))

(define-public java-eclipse-jetty-server-9.2
  (package
    (inherit java-eclipse-jetty-server)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ("jmx" ,java-eclipse-jetty-jmx-9.2)
       ("io" ,java-eclipse-jetty-io-9.2)
       ("http" ,java-eclipse-jetty-http-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("test-classes" ,java-eclipse-jetty-http-test-classes-9.2)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-security
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-security")
    (arguments
     `(#:jar-name "eclipse-jetty-security.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-exclude (list "**/ConstraintTest.*") ; This test fails
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-security")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-tomcat)
       ("http" ,java-eclipse-jetty-http)
       ("server" ,java-eclipse-jetty-server)
       ("util" ,java-eclipse-jetty-util)))
    (native-inputs
     `(("io" ,java-eclipse-jetty-io)
       ,@(package-native-inputs java-eclipse-jetty-util)))
    (synopsis "Jetty security infrastructure")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides the core jetty security
infrastructure")))

(define-public java-eclipse-jetty-security-9.2
  (package
    (inherit java-eclipse-jetty-security)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ("http" ,java-eclipse-jetty-http-9.2)
       ("server" ,java-eclipse-jetty-server-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("io" ,java-eclipse-jetty-io-9.2)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-servlet
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-servlet")
    (arguments
     `(#:jar-name "eclipse-jetty-servlet.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-servlet")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-tomcat)
       ("http" ,java-eclipse-jetty-http)
       ("http-test" ,java-eclipse-jetty-http-test-classes)
       ("io" ,java-eclipse-jetty-io)
       ("jmx" ,java-eclipse-jetty-jmx)
       ("security" ,java-eclipse-jetty-security)
       ("server" ,java-eclipse-jetty-server)
       ("util" ,java-eclipse-jetty-util)))
    (synopsis "Jetty Servlet Container")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides the core jetty servlet
container.")))

(define-public java-eclipse-jetty-servlet-9.2
  (package
    (inherit java-eclipse-jetty-servlet)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (arguments
     `(#:jar-name "eclipse-jetty-servlet.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; doesn't work
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-servlet")
             #t)))))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ("jmx" ,java-eclipse-jetty-jmx-9.2)
       ("io" ,java-eclipse-jetty-io-9.2)
       ("http" ,java-eclipse-jetty-http-9.2)
       ("security" ,java-eclipse-jetty-security-9.2)
       ("http-test" ,java-eclipse-jetty-http-test-classes-9.2)
       ("server" ,java-eclipse-jetty-server-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-xml
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-xml")
    (arguments
     `(#:jar-name "eclipse-jetty-xml.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; most tests require network
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-xml")
             #t)))))
    (inputs
     `(("java-eclipse-jetty-util" ,java-eclipse-jetty-util)))
    (native-inputs
     `(("java-eclipse-jetty-io" ,java-eclipse-jetty-io)
       ,@(package-native-inputs java-eclipse-jetty-util)))))

(define-public java-eclipse-jetty-xml-9.2
  (package
    (inherit java-eclipse-jetty-xml)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (arguments
     `(#:jar-name "eclipse-jetty-xml.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; most tests require network
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-xml")
             #t)))))
    (inputs
     `(("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("java-eclipse-jetty-io-9.2" ,java-eclipse-jetty-io-9.2)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-webapp
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-webapp")
    (arguments
     `(#:jar-name "eclipse-jetty-webapp.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       ;; One test fails
       #:test-exclude (list "**/WebAppContextTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-webapp")
             #t)))))
    (inputs
     `(("java-eclipse-jetty-util" ,java-eclipse-jetty-util)
       ("java-eclipse-jetty-http" ,java-eclipse-jetty-http)
       ("java-eclipse-jetty-server" ,java-eclipse-jetty-server)
       ("java-eclipse-jetty-servlet" ,java-eclipse-jetty-servlet)
       ("java-eclipse-jetty-security" ,java-eclipse-jetty-security)
       ("java-eclipse-jetty-xml" ,java-eclipse-jetty-xml)
       ("java-javaee-servletapi" ,java-javaee-servletapi)))
    (native-inputs
     `(("java-eclipse-jetty-io" ,java-eclipse-jetty-io)
       ,@(package-native-inputs java-eclipse-jetty-util)))))

(define-public java-eclipse-jetty-webapp-9.2
  (package
    (inherit java-eclipse-jetty-webapp)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (arguments
     `(#:jar-name "eclipse-jetty-webapp.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-exclude (list "**/WebAppContextTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-webapp")
             #t)))))
    (inputs
     `(("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclipse-jetty-http-9.2" ,java-eclipse-jetty-http-9.2)
       ("java-eclipse-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclipse-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)
       ("java-eclipse-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-eclipse-jetty-xml-9.2" ,java-eclipse-jetty-xml-9.2)
       ("java-tomcat" ,java-tomcat)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("java-eclipse-jetty-io-9.2" ,java-eclipse-jetty-io-9.2)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-jsoup
  (package
    (name "java-jsoup")
    (version "1.10.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jhy/jsoup/archive/jsoup-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0xbzw7rjv7s4nz1xk9b2cnin6zkpaldmc3svk71waa7hhjgp0a20"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jsoup.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (let ((classes-dir (string-append (getcwd) "/build/classes")))
               (with-directory-excursion "src/main/java"
                 (for-each (lambda (file)
                             (let ((dist (string-append classes-dir "/" file)))
                               (mkdir-p (dirname dist))
                               (copy-file file dist)))
                   (find-files "." ".*.properties"))))
             #t)))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-gson" ,java-gson)))
    (home-page "https://jsoup.org")
    (synopsis "HTML parser")
    (description "Jsoup is a Java library for working with real-world HTML.  It
provides a very convenient API for extracting and manipulating data, using the
best of DOM, CSS, and jQuery-like methods.")
    (license l:expat)))

(define-public tidyp
  (package
    (name "tidyp")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/downloads/petdance/tidyp/tidyp-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0f5ky0ih4vap9c6j312jn73vn8m2bj69pl2yd3a5nmv35k9zmc10"))))
    (build-system gnu-build-system)
    ;; ./test-thing.sh tries to run ./testall.sh, which is not included.
    (arguments `(#:tests? #f))
    (home-page "http://www.tidyp.com/")
    (synopsis "Validate HTML")
    (description "Tidyp is a program that can validate your HTML, as well as
modify it to be more clean and standard.  tidyp does not validate HTML 5.

libtidyp is the library on which the program is based.  It can be used by any
other program that can interface to it.  The Perl module @code{HTML::Tidy} is
based on this library, allowing Perl programmers to easily validate HTML.")
    ;; See htmldoc/license.html
    (license l:bsd-3)))

(define-public perl-html-tidy
  (package
    (name "perl-html-tidy")
    (version "1.60")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PETDANCE/HTML-Tidy-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1iyp2fd6j75cn1xvcwl2lxr8qpjxssy2360cyqn6g3kzd1fzdyxw"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tidyp-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile.PL"
               (("^my \\$inc = \"" line)
                (string-append line
                               "-I" (assoc-ref inputs "tidyp") "/include/tidyp "))
               (("-L/usr/lib")
                (string-append
                 "-L" (assoc-ref inputs "tidyp") "/lib")))
             #t)))))
    (inputs
     `(("perl-libwww" ,perl-libwww)
       ("tidyp" ,tidyp)))
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)))
    (home-page "https://metacpan.org/release/HTML-Tidy")
    (synopsis "(X)HTML validation in a Perl object")
    (description "@code{HTML::Tidy} is an HTML checker in a handy dandy
object.  It's meant as a replacement for @code{HTML::Lint}, which is written
in Perl but is not nearly as capable as @code{HTML::Tidy}.")
    (license l:artistic2.0)))

(define-public geomyidae
  (package
    (name "geomyidae")
    (version "0.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://r-36.net/geomyidae")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02afgrk36wkdkflyqr2xgh49v9zq6ma454jshk7igvhpxfb5l3ks"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:tests? #f                      ; no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "http://r-36.net/scm/geomyidae/file/README.html")
    (synopsis "Small Gopher server")
    (description
     "Geomyidae is a server for distributed hypertext protocol Gopher.  Its
features include:

@enumerate
@item Gopher menus (see @file{index.gph} for an example);
@item directory listings (if no @file{index.gph} was found);
@item CGI support (@file{.cgi} files are executed);
@item search support in CGI files;
@item logging with multiple log levels.
@end enumerate\n")
    (license l:expat)))

(define-public cat-avatar-generator
  (package
    (name "cat-avatar-generator")
    (version "1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://framagit.org/Deevad/cat-avatar-generator.git")
                     (commit "71c0c662742cafe8afd2d2d50ec84243113e35ad")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0s7b5whqsmfa57prbgl66ym551kg6ly0z14h5dgrlx4lqm70y2yw"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (srfi srfi-1)
                      (srfi srfi-26))
         (let ((source (assoc-ref %build-inputs "source"))
               (php-dir (string-append %output "/share/web/" ,name "/")))
           ;; The cache directory must not be in the store, but in a writable
           ;; location.  The webserver will give us this location.
           (copy-recursively source php-dir)
           (substitute* (string-append php-dir "/cat-avatar-generator.php")
             (("\\$cachepath = .*")
              "if(isset($_SERVER['CACHE_DIR']))
$cachepath = $_SERVER['CACHE_DIR'];
else
die('You need to set the CACHE_DIR variable first.');"))
           #t))))
    (home-page "https://framagit.org/Deevad/cat-avatar-generator")
    (synopsis "Random avatar generator")
    (description "Cat avatar generator is a generator of cat pictures optimised
to generate random avatars, or defined avatar from a \"seed\".  This is a
derivation by David Revoy from the original MonsterID by Andreas Gohr.")
    ;; expat for the code, CC-BY 4.0 for the artwork
    (license (list l:expat
                   l:cc-by4.0))))

(define-public nghttp2
  (package
    (name "nghttp2")
    (version "1.35.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/nghttp2/nghttp2/"
                           "releases/download/v" version "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0fi6qg2w82636wixwkqy7bclpgxslmvg82r431hs8h6aqc4mnzwv"))))
    (build-system gnu-build-system)
    (outputs (list "out"
                   "lib"))              ; only libnghttp2
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gcc" ,gcc-7)                   ; 1.35.0 requires GCC6 or later

       ;; Required by tests.
       ("cunit" ,cunit)
       ("tzdata" ,tzdata-for-tests)))
    (inputs
     ;; Required to build the tools (i.e. without ‘--enable-lib-only’).
     `(("c-ares" ,c-ares)
       ("jansson" ,jansson)             ; for HPACK tools
       ("jemalloc" ,jemalloc)           ; fight nghttpd{,x} heap fragmentation
       ("libev" ,libev)
       ("libxml2" ,libxml2)             ; for ‘nghttp -a’
       ("openssl" ,openssl)))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir=" (assoc-ref %outputs "lib") "/lib")
             "--enable-app"             ; build all the tools
             "--enable-hpack-tools"     ; ...all the tools
             "--disable-examples"
             "--disable-static")        ; don't bother building .a files
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'break-circular-reference
           ;; libnghttp2.pc by default retains a reference to the ‘out’ output,
           ;; which is not allowed.  Break this cycle.  While we could install
           ;; only the library to ‘out’ and move everything else to a separate
           ;; output, this would inconvenience the majority of (human) users.
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "lib/libnghttp2.pc.in"
               (("@prefix@")
                (assoc-ref outputs "lib")))
             #t))
         (add-before 'configure 'work-around-bug-30756
           (lambda _
             (for-each unsetenv '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH")) #t))
         (add-before 'check 'set-timezone-directory
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZDIR" (string-append (assoc-ref inputs "tzdata")
                                            "/share/zoneinfo"))
             #t)))))
    (home-page "https://nghttp2.org/")
    (synopsis "HTTP/2 protocol client, proxy, server, and library")
    (description
     "nghttp2 implements the Hypertext Transfer Protocol, version
2 (@dfn{HTTP/2}).

A reusable C library provides the HTTP/2 framing layer, with several tools built
on top of it:

@itemize
@item @command{nghttp}, a command-line HTTP/2 client.  It exposes many advanced
and low-level aspects of the protocol and is useful for debugging.
@item @command{nghttpd}, a fast, multi-threaded HTTP/2 static web server that
serves files from a local directory.
@item @command{nghttpx}, a fast, multi-threaded HTTP/2 reverse proxy that can be
deployed in front of existing web servers that don't support HTTP/2.
Both @command{nghttpd} and @command{nghttpx} can fall back to HTTP/1.1 for
backwards compatibilty with clients that don't speak HTTP/2.
@item @command{h2load} for benchmarking (only!) your own HTTP/2 servers.
@item HTTP/2 uses a header compression method called @dfn{HPACK}.
nghttp2 provides a HPACK encoder and decoder as part of its public API.
@item @command{deflatehd} converts JSON data or HTTP/1-style header fields to
compressed JSON header blocks.
@item @command{inflatehd} converts such compressed headers back to JSON pairs.
@end itemize\n")
    (license l:expat)))

(define-public hpcguix-web
  (let ((commit "53e09ea59ec0380b41a4cbda32df8bdb9a10004d")
        (revision "3"))
    (package
      (name "hpcguix-web")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/UMCUGenetics/hpcguix-web.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ah4pn9697vazhbvd45n4b1rrkx2nbhnw384cr0b941q3sz1dfyc"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-26)
                    (ice-9 popen)
                    (ice-9 rdelim))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'set-variables
             (lambda _
               ;; This prevents a few warnings
               (setenv "GUILE_AUTO_COMPILE" "0")
               (setenv "XDG_CACHE_HOME" (getcwd))
               #t))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out      (assoc-ref outputs "out"))
                      (guix     (assoc-ref inputs "guix"))
                      (guile    (assoc-ref inputs "guile"))
                      (gcrypt   (assoc-ref inputs "guile-gcrypt"))
                      (git      (assoc-ref inputs "guile-git"))
                      (bs       (assoc-ref inputs "guile-bytestructures"))
                      (json     (assoc-ref inputs "guile-json"))
                      (guile-cm (assoc-ref inputs
                                           "guile-commonmark"))
                      (deps (list guile gcrypt git bs guile-cm guix json))
                      (effective
                       (read-line
                        (open-pipe* OPEN_READ
                                    (string-append guile "/bin/guile")
                                    "-c" "(display (effective-version))")))
                      (path   (string-join
                               (map (cut string-append <>
                                         "/share/guile/site/"
                                         effective)
                                    deps)
                               ":"))
                      (gopath (string-join
                               (map (cut string-append <>
                                         "/lib/guile/" effective
                                         "/site-ccache")
                                    deps)
                               ":")))
                 (wrap-program (string-append out "/bin/run")
                   `("GUILE_LOAD_PATH" ":" prefix (,path))
                   `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,gopath)))

                 #t))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("uglify-js" ,uglify-js)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("guix" ,guix)))
      (propagated-inputs
       `(("guile" ,guile-2.2)
         ("guile-commonmark" ,guile-commonmark)
         ("guile-json" ,guile-json)))
    (home-page "https://github.com/UMCUGenetics/hpcguix-web")
      (synopsis "Web interface for cluster deployments of Guix")
      (description "Hpcguix-web provides a web interface to the list of packages
provided by Guix.  The list of packages is searchable and provides
instructions on how to use Guix in a shared HPC environment.")
    (license l:agpl3+))))
