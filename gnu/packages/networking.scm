;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2017, 2018, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017, 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2016 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016 Benz Schenk <benz.schenk@uzh.ch>
;;; Copyright © 2016, 2017 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2020, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Adam Van Ymeren <adam@vany.ca>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018, 2019 Tonton <tonton@riseup.net>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2018, 2020-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018, 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2019, 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019, 2020 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2019 Daniel Schaefer <git@danielschaefer.me>
;;; Copyright © 2019 Diego N. Barbato <dnbarbato@posteo.de>
;;; Copyright © 2020, 2021 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Jesse Dowell <jessedowell@gmail.com>
;;; Copyright © 2020 Hamzeh Nasajpour <h.nasajpour@pantherx.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Milkey Mouse <milkeymouse@meme.institute>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (gnu packages networking)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match))

(define-public usrsctp
  (package
    (name "usrsctp")
    (version "0.9.5.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/sctplab/usrsctp")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10ndzkip8blgkw572n3dicl6mgjaa7kygwn3vls80liq92vf1sa9"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           python-wrapper
           which))
    (home-page "https://github.com/sctplab/usrsctp/")
    (synopsis "SCTP user-land implementation")
    (description "UsrSCTP is a portable SCTP userland stack.  SCTP is a message
oriented, reliable transport protocol with direct support for multihoming that
runs on top of IP or UDP, and supports both v4 and v6 versions.")
    (license license:bsd-3)))

(define-public axel
  (package
    (name "axel")
    (version "2.17.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/axel-download-accelerator/axel/"
                           "releases/download/v" version "/"
                           "axel-" version ".tar.xz"))
       (sha256
        (base32 "1yfcsi0zv07bvhj8klgna3y1ycc4jhaija1b3rzzv0i4d4c2q2sq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list libressl))
    (home-page "https://github.com/axel-download-accelerator/axel")
    (synopsis "Light command line download accelerator")
    (description
     "Axel tries to accelerate the download process by using multiple
connections per file, and can also balance the load between different
servers.  It tries to be as light as possible, so it might be useful
on byte-critical systems.  It supports HTTP, HTTPS, FTP and FTPS
protocols.")
    (license license:gpl2+)))

;; This package does not have a release yet.
;; But this is required to provide a feature in PipeWire.
(define-public libcamera
  (package
    (name "libcamera")
    (version "0.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "git://linuxtv.org/libcamera.git")
         (commit "74c8b508338ccdd0780aa1e067a1e8fcb9ee326b")))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "0d9lp8b9gyxh4jwfh55kp8zl1xyyg32z684v3y29378zpksncss1"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dv4l2=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "test/meson.build"
               (("\\['list-cameras',                    'list-cameras.cpp'\\],")
                ""))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/doc")
                (string-append doc "/share/doc"))
               #t))))))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("sphinx" ,python-sphinx)
       ("yaml" ,python-pyyaml)))
    (inputs
     `(("boost" ,boost)
       ("glib" ,glib)
       ("gstreamer" ,gst-plugins-base)
       ("gnutls" ,gnutls)
       ("libtiff" ,libtiff)
       ("openssl" ,openssl)
       ("qt5" ,qtbase-5)
       ("udev" ,eudev)))
    (synopsis "Camera stack and framework")
    (description "LibCamera is a complex camera support library for GNU+Linux,
Android, and ChromeOS.")
    (home-page "https://libcamera.org/")
    (license license:lgpl2.1+)))

(define-public libnice
  ;; The latest release is old and randomly fails tests with GStreamer 1.18.5,
  ;; such as: "test-bind: ../libnice-0.1.18/stun/tests/test-bind.c:234:
  ;; bad_responses: Assertion `len >= 20' failed"
  (let ((revision "0")
        (commit "47a96334448838c43d7e72f4ef51b317befbfae1"))
    (package
      (name "libnice")
      (version (git-version "0.1.18" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/libnice/libnice")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "19ypjazslmsb9vqs2wyyzvi72h5jbn16dbng7pxh485djdrmgcg4"))))
      (build-system meson-build-system)
      (outputs '("out" "doc"))
      (arguments
       `(#:glib-or-gtk? #t           ; To wrap binaries and/or compile schemas
         #:configure-flags
         (list
          "-Dgtk_doc=enabled")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'disable-failing-tests
             (lambda _
               (substitute* "tests/meson.build"
                 ;; ‘test-set-port-range.c:66:main: assertion failed:
                 ;; (nice_agent_gather_candidates (agent, stream1))’
                 (("'test-set-port-range'" all)
                  (string-append "# " all))
                 ;; The following test is disabled as it fails in a
                 ;; nondeterministic fashion (see:
                 ;; https://gitlab.freedesktop.org/libnice/libnice/-/issues/151).
                 (("'test-bsd'" all)
                  (string-append "# " all)))
               (substitute* "stun/tests/meson.build"
                 ;; test-bind.c:234: bad_responses: Assertion `len >= 20'
                 ;; failed (see:
                 ;; https://gitlab.freedesktop.org/libnice/libnice/-/issues/150).
                 (("'bind', ")
                  ""))))
           (add-after 'install 'move-docs
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (doc (assoc-ref outputs "doc")))
                 (mkdir-p (string-append doc "/share"))
                 (rename-file
                  (string-append out "/share/gtk-doc")
                  (string-append doc "/share/gtk-doc"))))))))
      (native-inputs
       `(("glib:bin" ,glib "bin")
         ("gobject-introspection" ,gobject-introspection)
         ("graphviz" ,graphviz)
         ("gtk-doc" ,gtk-doc/stable)
         ("pkg-config" ,pkg-config)))
      (inputs
       (list gstreamer gst-plugins-base libnsl))
      (propagated-inputs
       (list glib glib-networking gnutls))
      (synopsis "GLib ICE implementation")
      (description "LibNice is a library that implements the Interactive
Connectivity Establishment (ICE) standard (RFC 5245 & RFC 8445).  It provides a
GLib-based library, libnice, as well as GStreamer elements to use it.")
      (home-page "https://libnice.freedesktop.org/")
      (license
       ;; This project is dual-licensed.
       (list
        license:lgpl2.1+
        license:mpl1.1)))))

(define-public rtmpdump
  ;; There are no tags in the repository, and the project is unlikely to
  ;; make new releases.  Take a recent commit for multiple security fixes
  ;; as well as GnuTLS compatibility.
  (let ((commit "c5f04a58fc2aeea6296ca7c44ee4734c18401aa3")
        (revision "0")
        (version "2.4"))                ;as mentioned in README and man pages
    (package
      (name "rtmpdump")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.ffmpeg.org/rtmpdump")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "07ias612jgmxpam9h418kvlag32da914jsnjsfyafklpnh8gdzjb"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no tests
         #:make-flags
         (list
          ;; The ‘validate-runpath’ phase fails to find librtmp.so.0.
          (string-append "LDFLAGS=-Wl,-rpath="
                         (assoc-ref %outputs "out") "/lib")
          (string-append "prefix=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'omit-static-library
             (lambda _
               (substitute* "librtmp/Makefile"
                 (("cp librtmp\\.a .*") ; don't install it
                  "")
                 (("librtmp\\.a ")      ; don't build it
                  ""))
               #t))
           (add-after 'unpack 'prefer-gnutls
             (lambda _
               (substitute* '("Makefile" "librtmp/Makefile")
                 (("CRYPTO=OPENSSL")
                  "#CRYPTO=OPENSSL")
                 (("#CRYPTO=GNUTLS")
                  "CRYPTO=GNUTLS"))))
           (delete 'configure))))
      (inputs
       (list gnutls zlib))
      (synopsis "Tools and library for handling RTMP streams")
      (description "RTMPdump is a toolkit for RTMP streams.  All forms of RTMP are
supported, including rtmp://, rtmpt://, rtmpe://, rtmpte://, and rtmps://.")
      (home-page "https://rtmpdump.mplayerhq.hu/")
      (license
       (list
        ;; Library.
        license:lgpl2.1+
        ;; Others.
        license:gpl2+)))))

(define-public slurm-monitor
  (package
    (name "slurm-monitor")
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/mattthias/slurm")
         (commit (string-append "upstream/" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n6pgrcs8gwrcq5fch1q3yk3jipjwrf21s9a13fbjrl903g5zzv9"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ;no tests
    (inputs (list ncurses))
    (synopsis "Network load monitor")
    (description
     "Slurm is a network load monitor.  It shows real-time traffic statistics
from any network device in any of three ASCII graph formats.")
    (home-page "https://github.com/mattthias/slurm")
    (license license:gpl2)))

(define-public srt
  (package
    (name "srt")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/Haivision/srt")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f60vlfxhh9bhafws82c3301whjlz5gy92jz9a9ymwfg5h53bv1j"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list
        (string-append "-DCMAKE_INSTALL_BINDIR="
                       (assoc-ref %outputs "out") "/bin")
        "-DCMAKE_INSTALL_INCLUDEDIR=include"
        "-DENABLE_STATIC=OFF"
        "-DENABLE_UNITTESTS=ON")))
    (native-inputs
     `(("gtest" ,googletest)
       ("pkg-config" ,pkg-config)
       ("tclsh" ,tcl)))
    (propagated-inputs
     (list openssl))
    (synopsis "Secure Reliable Transport")
    (description "SRT is a transport technology that optimizes streaming
performance across unpredictable networks, such as the Internet.")
    (home-page "https://www.srtalliance.org/")
    (license license:mpl2.0)))

;; FFmpeg, GStreamer, and VLC don't support SRT 1.4.2 yet.
(define-public srt-1.4.1
  (package
    (inherit srt)
    (name "srt")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/Haivision/srt")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01xaq44j95kbgqfl41pnybvqy0yq6wd4wdw88ckylzf0nzp977xz"))))))

(define-public lksctp-tools
  (package
    (name "lksctp-tools")
    (version "1.0.19")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/sctp/lksctp-tools")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jfq58j365mlgssavyw5wcal42n0xjkr40vmj9b8w265wgs28j20"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (synopsis
     "@acronym{SCTP, Stream Control Transmission Protocol} helpers for Linux")
    (description
     "The lksctp-tools project provides a user-space library for @acronym{SCTP,
the Stream Control Transmission Protocol} (@file{libsctp}) and C language header
files (@file{netinet/sctp.h}) for accessing SCTP-specific @acronym{APIs,
application programming interfaces} not provided by the standard sockets.
It also includes some SCTP-related helper utilities.")
    (home-page "http://lksctp.sourceforge.net/")
    (license
     (list
      ;; Library.
      license:lgpl2.1+
      ;; Others.
      license:gpl2+))))

(define-public python-pysctp
  (package
    (name "python-pysctp")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pysctp" version))
       (sha256
        (base32 "14h2qlmfi24bizhvvqkfqfa78pzm3911ibrzy9k94i97xy1978dy"))))
    (build-system python-build-system)
    (inputs
     (list lksctp-tools))
    (arguments
     `(#:tests? #f  ;; tests require network
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-setup.py
           (lambda _
             (substitute* "setup.py"
               (("include_dirs\\s*=.*")
                (string-append "include_dirs = ['.'] + '"
                               (getenv "C_INCLUDE_PATH") "'.split(':'),"))
               (("library_dirs\\s*=.*")
                (string-append "library_dirs = '"
                               (getenv "LIBRARY_PATH") "'.split(':'),"))))))))
    (home-page "https://github.com/p1sec/pysctp")
    (synopsis "Python module for the SCTP protocol stack and library")
    (description "@code{pysctp} implements the SCTP socket API.  You need a
SCTP-aware kernel (most are).")
    (license license:lgpl2.1+)))

(define-public knockd
  (package
    (name "knockd")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.zeroflux.org/proj/knock/files/knock-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1iv9h7a9l81ilbld3pi0dmzkizjss1755x1x3v5jxsi4asb8r3b9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))))
    (inputs
     (list libpcap))
    (home-page "https://www.zeroflux.org/projects/knock")
    (synopsis "Small port-knock daemon")
    (description "@command{knockd} is a port-knock daemon.  It listens to all traffic on
an ethernet or PPP interface, looking for special \"knock\" sequences of @dfn{port-hits}
(UDP/TCP packets sent to a server port).  This port need not be open, since knockd listens
at the link-layer level.")
    (license license:gpl2+)))

(define-public nng
  (package
    (name "nng")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nanomsg/nng")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a4jg8alh2h0rw6fb4dqpvk4hgl2a7h76mq7g34fy89qh9sgg1a4"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DNNG_ENABLE_COVERAGE=ON"
             "-DNNG_ENABLE_TLS=ON"
             "-DBUILD_SHARED_LIBS=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             ;; These tests require network access.
             (substitute* "tests/CMakeLists.txt"
               (("add_nng_test1\\(httpclient 60 NNG_SUPP_HTTP\\)") "")
               (("add_nng_test1\\(resolv 10 NNG_STATIC_LIB\\)") "")
               (("add_nng_test\\(tls 60\\)") ""))
             #t)))))
    (native-inputs
     `(("ksh" ,oksh)))
    (inputs
     `(("mbedtls" ,mbedtls-apache)))
    (synopsis "Lightweight messaging library")
    (description "NNG project is a rewrite of the scalability protocols library
known as libnanomsg, and adds significant new capabilities, while retaining
compatibility with the original.  It is a lightweight, broker-less library,
offering a simple API to solve common recurring messaging problems, such as
publish/subscribe, RPC-style request/reply, or service discovery.")
    (home-page "https://nng.nanomsg.org/")
    (license license:expat)))

(define-public nanomsg
  (package
    (name "nanomsg")
    (version "1.1.5")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/nanomsg/nanomsg")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01ddfzjlkf2dgijrmm3j3j8irccsnbgfvjcnwslsfaxnrmrq5s64"))))
    (build-system cmake-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "-DNN_ENABLE_COVERAGE=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share/doc"))
               (rename-file
                (string-append out "/share/doc/nanomsg")
                (string-append doc "/share/doc/nanomsg"))
               #t))))))
    (native-inputs
     `(("asciidoctor" ,ruby-asciidoctor)
       ("pkg-config" ,pkg-config)))
    (synopsis "Scalable socket library")
    (description "Nanomsg is a socket library that provides several common
communication patterns.  It aims to make the networking layer fast, scalable,
and easy to use.  Implemented in C, it works on a wide range of operating
systems with no further dependencies.")
    (home-page "https://nanomsg.org/")
    (license (license:non-copyleft "file:///COPYING"))))

(define-public blueman
  (package
    (name "blueman")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/blueman-project/blueman/releases"
                           "/download/" version "/blueman-" version ".tar.xz"))
       (sha256
        (base32 "1s86w4mklzr8hvbgl3nkg8jycl6grww533dhzw8gdn5glqfpkpbf"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list "--enable-polkit"
                               "--without-systemdsystemunitdir" ; Not required
                               "--without-systemduserunitdir")  ; Not required
       #:phases
       (modify-phases %standard-phases
         ;; Python references are not being patched in patch-phase of build,
         ;; despite using python-wrapper as input. So we patch them manually.
         (add-after 'unpack 'patch-python-references
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "apps"
               (substitute* '("blueman-adapters.in" "blueman-applet.in"
                              "blueman-manager.in" "blueman-mechanism.in"
                              "blueman-rfcomm-watcher.in" "blueman-sendto.in"
                              "blueman-services.in" "blueman-tray.in")
                 (("@PYTHON@")
                  (search-input-file inputs
                                     (string-append
                                      "/bin/python"
                                      ,(version-major+minor
                                        (package-version python)))))))))
         ;; Fix loading of external programs.
         (add-after 'unpack 'patch-external-programs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("blueman/main/NetConf.py"
                            "blueman/main/PPPConnection.py")
               (("/usr/sbin/bluetoothd")
                (search-input-directory inputs
                                        "/libexec/bluetooth/bluetoothd"))
               (("/sbin/iptables")
                (search-input-file inputs "/sbin/iptables"))
               (("/usr/sbin/pppd")
                (search-input-file inputs "/sbin/pppd")))))
         ;; Fix loading of pulseaudio libraries.
         (add-after 'unpack 'patch-pulseaudio-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((pulseaudio (assoc-ref inputs "pulseaudio"))
                    (pulse (string-append pulseaudio "/lib/libpulse.so.0"))
                    (pulse-glib (string-append pulseaudio
                                               "/lib/libpulse-mainloop-glib.so.0")))
               (with-directory-excursion "blueman/main"
                 (substitute* "PulseAudioUtils.py"
                   (("libpulse.so.0") pulse)
                   (("libpulse-mainloop-glib.so.0") pulse-glib))))))
         ;; Fix running of blueman programs.
         (add-after 'glib-or-gtk-wrap 'wrap-blueman-progs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/blueman-"))
                    (libexec (string-append out "/libexec/blueman-"))
                    (lib (string-append out "/lib/python"
                                        ,(version-major+minor
                                          (package-version python))
                                        "/site-packages")))
               (for-each
                (lambda (program)
                  (wrap-program program
                    `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH") ,lib))
                    `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))
                (append
                 (map (lambda (prog) (string-append bin prog))
                      '("adapters" "applet" "manager"
                        "sendto" "services" "tray"))
                 (map (lambda (prog) (string-append libexec prog))
                      '("mechanism" "rfcomm-watcher"))))))))))
    (native-inputs
     `(("cython" ,python-cython)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bluez" ,bluez)
       ("dbus" ,dbus)
       ("librsvg" ,librsvg)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("iproute2" ,iproute)
       ("iptables" ,iptables)
       ("net-tools" ,net-tools)
       ("pango" ,pango)
       ("polkit" ,polkit)
       ("ppp" ,ppp)
       ("pulseaudio" ,pulseaudio)
       ("pycairo" ,python-pycairo)
       ("pygobject" ,python-pygobject)
       ("python" ,python-wrapper)
       ("libappindicator" ,libappindicator)
       ("libnm" ,network-manager)))
    (synopsis "GTK+ Bluetooth manager")
    (description "Blueman is a Bluetooth management utility using the Bluez
D-Bus backend.  It is designed to be easy to use for most common Bluetooth
tasks.")
    (home-page "https://github.com/blueman-project/blueman")
    (license license:gpl3+)))

;; The gnu.org ‘home’ for this GNU project is a directory listing with 1.6.0 as
;; the latest version.  The author's git repository, mentioned in the 1.6.0
;; README and otherwise legit-looking, contains a proper 1.7.0 release tarball
;; with many OUI updates.  Use it, even though it's also several years old now.
(define-public macchanger
  (package
    (name "macchanger")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/alobbs/macchanger/"
                           "releases/download/" version "/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "1gs5m0jxyprdp00w2qkbnaqm3ilkjz0q1gqdg4nzdm8g4xy73qns"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/macchanger/")
    (synopsis "Viewing and manipulating MAC addresses of network interfaces")
    (description "GNU MAC Changer is a utility for viewing and changing MAC
addresses of networking devices.  New addresses may be set explicitly or
randomly.  They can include MAC addresses of the same or other hardware vendors
or, more generally, MAC addresses of the same category of hardware.")
    (license license:gpl2+)))

(define-public miredo
  (package
    (name "miredo")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.remlab.net/files/miredo/miredo-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0j9ilig570snbmj48230hf7ms8kvcwi2wblycqrmhh85lksd49ps"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-create-/run
           (lambda _
             (substitute* (find-files "src" "Makefile.*")
               (("^.+install_sh.+/run.+$")
                "\ttrue"))
             #t))
         (add-after 'unpack 'patch-iproute2
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((iproute (assoc-ref inputs "iproute"))
                    (ip (string-append iproute "/sbin/ip")))
               (substitute* "misc/client-hook.iproute"
                 (("/sbin/ip") ip))
               #t)))
         ;; The checkconf test in src/ requires network access.
         (add-before
          'check 'disable-checkconf-test
          (lambda _
            (substitute* "src/Makefile"
              (("^TESTS = .*") "TESTS = \n"))
            #t)))))
    (inputs
     `(("iproute" ,iproute)))
    (home-page "https://www.remlab.net/miredo/")
    (synopsis "Teredo IPv6 tunneling software")
    (description
     "Miredo is an implementation (client, relay, server) of the Teredo
specification, which provides IPv6 Internet connectivity to IPv6 enabled hosts
residing in IPv4-only networks, even when they are behind a NAT device.")
    (license license:gpl2+)))

(define-public ndisc6
  (package
    (name "ndisc6")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.remlab.net/files/ndisc6/ndisc6-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0lgkbnnll8nrr7h63ywd42sg8fiv6jhhymd7rnml8a3yqjgjz4rn"))))
    (build-system gnu-build-system)
    (home-page "https://www.remlab.net/ndisc6/")
    (synopsis "IPv6 diagnostic tools")
    (description
     "NDisc6 is a collection of tools for IPv6 networking diagnostics.
It includes the following programs:

@itemize
@item @command{ndisc6}: ICMPv6 Neighbor Discovery tool.
@item @command{rdisc6}: ICMPv6 Router Discovery tool.
@item @command{tcptraceroute6}: IPv6 traceroute over TCP.
@item @command{traceroute6}: IPv6 traceroute over UDP.
@item @command{rdnssd}: Recursive DNS Servers discovery daemon.
@end itemize")
    ;; The user can choose version 2 or 3 of the GPL, not later versions.
    (license (list license:gpl2 license:gpl3))))

(define-public parprouted
  (package
    (name "parprouted")
    (version "0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.hazard.maks.net/parprouted/"
                                  "parprouted-" version ".tar.gz"))
              (sha256
               (base32
                "1z6yg28i0pv20jivyy82pxb38hsryj95inhj27bs6ja1bp4l6dnn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'insert-absolute-iproute-reference
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((iproute (assoc-ref inputs "iproute"))
                             (ip (string-append iproute "/sbin/ip")))
                        (substitute* "parprouted.c"
                          (("/sbin/ip") ip))
                        #t)))
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (sbin (string-append out "/sbin"))
                             (man8 (string-append out "/share/man/man8")))
                        ;; No configure script; hijack the phase to make
                        ;; the necessary arrangements.
                        (setenv "CC" ,(cc-for-target))
                        (for-each mkdir-p (list sbin man8))
                        (substitute* "Makefile"
                          (("/usr/local/sbin") sbin)
                          (("/usr/local/man/man8") man8))
                        #t))))))
    (inputs
     `(("iproute" ,iproute)))
    (home-page "https://www.hazard.maks.net/parprouted/")
    (synopsis "Proxy ARP requests to other interfaces")
    (description
     "@command{parprouted} is a daemon for transparent IP (Layer@tie{}3)
proxy ARP bridging.  Unlike standard bridging, proxy ARP bridging can bridge
Ethernet networks behind wireless nodes.  Normal layer@tie{}2 bridging does
not work between wireless nodes because wireless does not know about MAC
addresses used in the wired Ethernet networks.  This daemon can also be
useful for making transparent firewalls.")
    (license license:gpl2)))

(define-public pproxy
  (package
    (name "pproxy")
    (version "2.7.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pproxy" version))
              (sha256
               (base32
                "1j4nv72i77i2j5nl9ymzpk4m98qih3naihfrqjghrc9b7g0krdzs"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (with-directory-excursion "tests"
                       (for-each (lambda (file)
                                   (invoke "python" file))
                                 ;; XXX: The api_ tests require network access
                                 ;; so we only run the cipher tests for now.
                                 (find-files "." "^cipher_.*\\.py$")))))))))
    (inputs
     (list python-asyncssh
           python-daemon
           python-pycryptodome
           python-uvloop))
    (home-page "https://github.com/qwj/python-proxy")
    (synopsis "Multi-protocol network proxy")
    (description
     "@command{pproxy} is an asynchronuous proxy server implemented with
Python 3 @code{asyncio}.  Among the supported protocols are HTTP, SOCKS
and SSH, and it can use both TCP and UDP as transport mechanisms.")
    (license license:expat)))

(define-public socat
  (package
    (name "socat")
    (version "1.7.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.dest-unreach.org/socat/download/socat-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1sbmqqvni3ss9wyay6ik5v81kxffkra80mh4ypgj74g82iba5b1z"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))          ; no test suite
    (inputs (list openssl))
    (home-page "http://www.dest-unreach.org/socat/")
    (synopsis
     "Open bidirectional communication channels from the command line")
    (description
     "socat is a relay for bidirectional data transfer between two independent
data channels---files, pipes, devices, sockets, etc.  It can create
\"listening\" sockets, named pipes, and pseudo terminals.

socat can be used, for instance, as TCP port forwarder, as a shell interface
to UNIX sockets, IPv6 relay, for redirecting TCP oriented programs to a serial
line, to logically connect serial lines on different computers, or to
establish a relatively secure environment (su and chroot) for running client
or server shell scripts with network connections.")
    (license license:gpl2)))

(define-public mbuffer
  (package
    (name "mbuffer")
    (version "20211018")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.maier-komor.de/software/mbuffer/mbuffer-"
                    version ".tgz"))
              (sha256
               (base32
                "1qxnbpyly00kml3sjan9iqg6pqacsi3yqq66x25w455cwkjc2h72"))))
    (build-system gnu-build-system)
    (native-inputs
     (list which))
    (inputs (list openssl))
    (home-page "http://www.maier-komor.de/mbuffer.html")
    (synopsis
     "Swiss army knife for data stream buffering (network aware)")
    (description
     "mbuffer is a tool for buffering data streams with a large set of features:

@itemize
@item direct support for TCP based network targets (IPv4 and IPv6)
@item ability to send to multiple targets in parallel (distribution mode)
@item support for multiple volumes
@item I/O rate limitation
@item high/low watermark based restart criteria
@item configurable buffer size
@item on the fly MD5 hash calculation
@item highly efficient, multi-threaded implementation
@end itemize")
    (license license:gpl3+)))

(define-public tcp-wrappers
  (package
    (name "tcp-wrappers")
    (version "7.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.porcupine.org/pub/security/tcp_wrappers_"
                    version ".tar.gz"))
              (sha256
               (base32
                "0p9ilj4v96q32klavx0phw9va21fjp8vpk11nbh6v2ppxnnxfhwm"))
              (modules '((guix build utils)))
              (snippet
               ;; 'sys_errlist' & co. are gone in glibc 2.33; work around it.
               '(substitute* "percent_m.c"
                  (("sys_errlist\\[errno\\]")
                   "strerror (errno)")
                  (("errno < sys_nerr")
                   "(1)")
                  (("errno >= sys_nerr")
                   "(0)")))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)  ; there is no configure script
         (delete 'check)      ; there are no tests
         (replace 'build
           (lambda _
             (chmod "." #o755)
             ;; Upstream doesn't generate a shared library.  So we have to do it.
             (setenv "CC" "gcc -fno-builtin -fPIC")
             (substitute* "Makefile"
               (("^(all[^\n]*)" line) (string-append line " libwrap.so\n
libwrap.so: $(LIB_OBJ)\n
\tgcc -shared $^ -o $@\n")))
             ;; Deal with some gcc breakage.
             (substitute* "percent_m.c"
               (("extern char .sys_errlist.*;") ""))
             (substitute* "scaffold.c"
               (("extern char .malloc.*;") ""))
             ;; This, believe it or not, is the recommended way to build!
             (invoke "make" "REAL_DAEMON_DIR=/etc" "linux")))
         ;; There is no make install stage, so we have to do it ourselves.
         (replace 'install
           (lambda _
             (let ((out (assoc-ref %outputs "out"))
                   (man-pages `("hosts_access.3"
                                "hosts_access.5"
                                "hosts_options.5"
                                "tcpd.8"
                                "tcpdchk.8"
                                "tcpdmatch.8"))
                   (libs  `("libwrap.a"
                            "libwrap.so"))
                   (headers `("tcpd.h"))
                   (bins `("safe_finger"
                           "tcpd"
                           "tcpdchk"
                           "tcpdmatch"
                           "try-from")))
               (for-each
                (lambda (x)
                  (install-file x (string-append out "/include")))
                headers)
               (for-each
                (lambda (x)
                  (install-file x (string-append out "/share/man/man"
                                                 (string-take-right x 1))))
                man-pages)
               (for-each
                (lambda (x)
                  (install-file x (string-append out "/lib/")))
                libs)
               (for-each
                (lambda (x)
                  (install-file x (string-append out "/bin/")))
                bins))
             #t)))))
    (home-page "http://www.porcupine.org")
    (synopsis  "Monitor and filter incoming requests for network services")
    (description "With this package you can monitor and filter incoming requests for
network services.  It includes a library which may be used by daemons to
transparently check connection attempts against an access control list.")
    (license (license:non-copyleft "file://DISCLAIMER"
                                   "See the file DISCLAIMER in the distribution."))))

(define-public zeromq
  (package
    (name "zeromq")
    (version "4.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/zeromq/libzmq/releases"
                           "/download/v" version "/zeromq-" version ".tar.gz"))
       (sha256
        (base32 "1rf3jmi36ms8jh2g5cvi253h43l6xdfq0r7mvp95va7mi4d014y5"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--disable-static"
                                     "--enable-drafts")))
    (home-page "https://zeromq.org")
    (synopsis "Library for message-based applications")
    (description
     "The 0MQ lightweight messaging kernel is a library which extends the
standard socket interfaces with features traditionally provided by specialized
messaging middle-ware products.  0MQ sockets provide an abstraction of
asynchronous message queues, multiple messaging patterns, message
filtering (subscriptions), seamless access to multiple transport protocols and
more.")
    (license license:lgpl3+)))

(define-public czmq
  (package
    (name "czmq")
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/zeromq/" name
                    "/releases/download/v" version
                    "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fdclvd7fcwixp0k57ccv7d159v3slasyhvndxfn8n1a9hh0lwjx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-drafts")))
    (inputs
     (list zeromq))
    (home-page "https://zeromq.org")
    (synopsis "High-level C bindings for ØMQ")
    (description
     "czmq provides bindings for the ØMQ core API that hides the differences
between different versions of ØMQ.")
    (license license:mpl2.0)))

(define-public cppzmq
  (package
    (name "cppzmq")
    (version "4.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zeromq/cppzmq")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "19acx2bzi4n6fdnfgkja1nds7m1bwg8lw5vfcijrx9fv75pa7m8h"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(;; FIXME: The test suite requires downloading Catch and custom
       ;; CMake targets, and refuses to use the system version.
       ;; See <https://github.com/zeromq/cppzmq/issues/334>.
       #:tests? #f
       #:configure-flags '("-DCPPZMQ_BUILD_TESTS=OFF")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list zeromq))
    (home-page "https://zeromq.org")
    (synopsis "C++ bindings for the ØMQ messaging library")
    (description
     "This package provides header-only C++ bindings for ØMQ.  The header
files contain direct mappings of the abstractions provided by the ØMQ C API.")
    (license license:expat)))

(define-public libnatpmp
  (package
    (name "libnatpmp")
    (version "20150609")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://miniupnp.free.fr/files/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c1n8n7mp0amsd6vkz32n8zj3vnsckv308bb7na0dg0r8969rap1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)) ; no tests
       #:make-flags
       (let* ((target ,(%current-target-system))
              (gcc (if target
                       (string-append target "-gcc")
                       "gcc")))
         (list
          (string-append "CC=" gcc)
          (string-append "INSTALLPREFIX=" (assoc-ref %outputs "out"))
          (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib")))))
    (home-page "http://miniupnp.free.fr/libnatpmp.html")
    (synopsis "C library implementing NAT-PMP")
    (description
     "@code{libnatpmp} is a portable and asynchronous implementation of
the Network Address Translation - Port Mapping Protocol (NAT-PMP)
written in the C programming language.")
    (license license:bsd-3)))

(define-public librdkafka
  (package
    (name "librdkafka")
    (version "1.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/edenhill/librdkafka")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05mgrdzacn9kdpr68r5j0cvsvl54s52glnsc1ww9rcxx6p7hq1ly"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; its custom configure script doesn't understand 'CONFIG_SHELL'.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; librdkafka++.so lacks RUNPATH for librdkafka.so
               (setenv "LDFLAGS"
                       (string-append "-Wl,-rpath=" out "/lib"))
               (invoke "./configure"
                       (string-append "--prefix=" out))))))))
    (native-inputs
     `(("python" ,python-wrapper)))
    (propagated-inputs
     (list zlib)) ; in the Libs.private field of rdkafka.pc
    (home-page "https://github.com/edenhill/librdkafka")
    (synopsis "Apache Kafka C/C++ client library")
    (description
     "librdkafka is a C library implementation of the Apache Kafka protocol,
containing both Producer and Consumer support.")
    (license license:bsd-2)))

(define-public libndp
  (package
    (name "libndp")
    (version "1.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://libndp.org/files/"
                                  "libndp-" version ".tar.gz"))
              (sha256
               (base32
                "0ay0n0d85254zdmv8znmn399gfiqpk6ga0jwdwa7ylpbw9pbdzw8"))))
    (build-system gnu-build-system)
    (home-page "https://libndp.org/")
    (synopsis "Library for Neighbor Discovery Protocol")
    (description
     "libndp contains a library which provides a wrapper for IPv6 Neighbor
Discovery Protocol.  It also provides a tool named ndptool for sending and
receiving NDP messages.")
    (license license:lgpl2.1+)))

(define-public ethtool
  (package
    (name "ethtool")
    (version "5.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/network/"
                                  "ethtool/ethtool-" version ".tar.xz"))
              (sha256
               (base32
                "0v8i592vwjypf111w0lfvaxdwhzybp6w600g28m9rm490c8xcvv8"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libmnl))
    (home-page "https://www.kernel.org/pub/software/network/ethtool/")
    (synopsis "Display or change Ethernet device settings")
    (description
     "ethtool can be used to query and change settings such as speed,
auto-negotiation and checksum offload on many network devices, especially
Ethernet devices.")
    (license license:gpl2)))

(define-public ifstatus
  (package
    (name "ifstatus")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ifstatus/ifstatus/"
                                  "ifstatus%20v" version "/ifstatus-v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "045cbsq9ps32j24v8y5hpyqxnqn9mpaf3mgvirlhgpqyb9jsia0c"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Main.h"
                    (("#include <stdio.h>")
                     "#include <stdio.h>\n#include <stdlib.h>"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                                ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)             ; no configure script
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin")))
                      (mkdir-p bin)
                      (copy-file "ifstatus"
                                 (string-append bin "/ifstatus")))
                    #t)))))
    (inputs (list ncurses))
    (home-page "http://ifstatus.sourceforge.net/graphic/index.html")
    (synopsis "Text based network interface status monitor")
    (description
     "IFStatus is a simple, easy-to-use program for displaying commonly
needed/wanted real-time traffic statistics of multiple network
interfaces, with a simple and efficient view on the command line.  It is
intended as a substitute for the PPPStatus and EthStatus projects.")
    (license license:gpl2+)))

(define-public iputils
  (package
    (name "iputils")
    (version "20190709")
    (home-page "https://github.com/iputils/iputils")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "s" version))))
              (file-name (git-file-name name version))
              (patches (search-patches "iputils-libcap-compat.patch"))
              (sha256
               (base32
                "04bp4af15adp79ipxmiakfp0ij6hx5qam266flzbr94pr8z8l693"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_RARPD=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-docbook-url
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((docbook-xsl (assoc-ref inputs "docbook-xsl"))
                    (uri (string-append docbook-xsl "/xml/xsl/docbook-xsl-"
                                        ,(package-version docbook-xsl))))
               (for-each
                (lambda (file)
                  (substitute* file
                    (("http://docbook\\.sourceforge\\.net/release/xsl-ns/current")
                     uri)))
                (cons "doc/meson.build"
                      (find-files "doc" "\\.xsl$")))
               #t))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml-5)
       ("libxml2" ,libxml2)          ;for XML_CATALOG_FILES
       ("xsltproc" ,libxslt)))
    (inputs
     (list libcap libidn2 openssl))
    (synopsis "Collection of network utilities")
    (description
     "This package contains a variety of tools for dealing with network
configuration, troubleshooting, or servers.  Utilities included are:

@itemize @bullet
@item @command{arping}: Ping hosts using the @dfn{Address Resolution Protocol}.
@item @command{clockdiff}: Compute time difference between network hosts
using ICMP TSTAMP messages.
@item @command{ninfod}: Daemon that responds to IPv6 Node Information Queries.
@item @command{ping}: Use ICMP ECHO messages to measure round-trip delays
and packet loss across network paths.
@item @command{rarpd}: Answer RARP requests from clients.
@item @command{rdisc}: Populate network routing tables with information from
the ICMP router discovery protocol.
@item @command{tftpd}: Trivial file transfer protocol server.
@item @command{tracepath}: Trace network path to an IPv4 or IPv6 address and
discover MTU along the way.
@end itemize")
    ;; The various utilities are covered by different licenses, see LICENSE
    ;; for details.
    (license (list license:gpl2+  ;arping, rarpd, tracepath
                   license:bsd-3  ;clockdiff, ninfod, ping, tftpd
                   (license:non-copyleft
                    "https://spdx.org/licenses/Rdisc.html"
                    "Sun Microsystems license, see rdisc.c for details")))))

(define-public nload
  (package
    (name "nload")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/nload/nload/" version
                                  "/nload-" version ".tar.gz"))
              (sha256
               (base32
                "1rb9skch2kgqzigf19x8bzk211jdfjfdkrcvaqyj89jy2pkm3h61"))))
    (build-system gnu-build-system)
    (inputs (list ncurses))
    (home-page "http://www.roland-riegel.de/nload/")
    (synopsis "Realtime console network usage monitor")
    (description
     "Nload is a console application which monitors network traffic and
bandwidth usage in real time.  It visualizes the in- and outgoing traffic using
two graphs, and provides additional info like total amount of transferred data
and min/max network usage.")
    (license license:gpl2+)))

(define-public iodine
  (package
    (name "iodine")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://code.kryo.se/" name "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gh17kcxxi37k65zm4gqsvbk3aw7yphcs3c02pn1c4s2y6n40axd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-ifconfig-path
           ;; This package works only with the net-tools version of ifconfig.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/tun.c"
               (("PATH=[^ ]* ")
                (string-append (assoc-ref inputs "net-tools") "/bin/")))
             #t))
         (add-before 'check 'delete-failing-tests
           ;; Avoid https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=802105.
           (lambda _
             (substitute* "tests/common.c"
               (("tcase_add_test\\(tc, \
test_parse_format_ipv(4(|_listen_all|_mapped_ipv6)|6)\\);")
                ""))
             #t)))
       #:make-flags (list ,(string-append "CC=" (cc-for-target))
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:test-target "test"))
    (inputs (list net-tools zlib))
    (native-inputs (list check-0.14 pkg-config))
    (home-page "https://code.kryo.se/iodine/")
    (synopsis "Tunnel IPv4 data through a DNS server")
    (description "Iodine tunnels IPv4 data through a DNS server.  This
can be useful in different situations where internet access is firewalled, but
DNS queries are allowed.  The bandwidth is asymmetrical, with limited upstream
and up to 1 Mbit/s downstream.")
    ;; src/md5.[ch] is released under the zlib license
    (license (list license:isc license:zlib))))

(define-public whois
  (package
    (name "whois")
    (version "5.5.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/rfc1036/whois")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wys0aixzq6mzvg7p6jb0d5rkkg23pjcgcsx86p7hjidxdvnbwzr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PKG_CONFIG=" ,(pkg-config-for-target))
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'build 'setenv
           (lambda _
             (setenv "HAVE_ICONV" "1")
             #t)))))
    (inputs
     (list libidn2))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (synopsis "Intelligent client for the WHOIS directory service")
    (description
      "whois searches for an object in a @dfn{WHOIS} (RFC 3912) database.
It is commonly used to look up the registered users or assignees of an Internet
resource, such as a domain name, an IP address block, or an autonomous system.
It can automatically select the appropriate server for most queries.

For historical reasons, this package also includes @command{mkpasswd}, which
encrypts passwords using @code{crypt(3)} and is unrelated to the Expect command
of the same name.")
    (home-page "https://github.com/rfc1036/whois")
    (license license:gpl2+)))

(define-public wireshark
  (package
    (name "wireshark")
    (version "3.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.wireshark.org/download/src/wireshark-"
                           version ".tar.xz"))
       (sha256
        (base32 "0f2sjbbwmmz9zr8vphxy0panfji5vv8vazm688mqxy3bzflfsd04"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-failing-test
           ;; Skip test suite failing with "Program reassemble_test is not
           ;; available" and alike errors.  Also skip test suite failing with
           ;; "AssertionError: Program extcap/sdjournal is not available"
           ;; error.'
           (lambda _
             (substitute* "CMakeLists.txt"
               (("suite_unittests" all) (string-append "# " all))
               (("suite_extcaps" all) (string-append "# " all))))))
       ;; Build process chokes during `validate-runpath' phase.
       ;;
       ;; Errors are like the following:
       ;; "/gnu/store/...wireshark-3.0.0/lib/wireshark/plugins/3.0/epan/ethercat.so:
       ;; error: depends on 'libwireshark.so.12', which cannot be found in
       ;; RUNPATH".  That is, "/gnu/store/...wireshark-3.0.0./lib" doesn't
       ;; belong to RUNPATH.
       ;;
       ;; That’s not a problem in practice because "ethercat.so" is a plugin,
       ;; so it’s dlopen’d by a process that already provides "libwireshark".
       ;; For now, we disable this phase.
       #:validate-runpath? #f))
    (inputs
     (list c-ares
           glib
           gnutls
           brotli
           libcap
           libgcrypt
           libnl
           libpcap
           libssh
           libxml2
           lz4
           lua-5.2                      ;Lua 5.3 unsupported
           mit-krb5
           `(,nghttp2 "lib")
           minizip
           qtbase-5
           qtmultimedia
           qtsvg
           sbc
           snappy
           zlib
           `(,zstd "lib")))
    (native-inputs
     (list bison
           doxygen
           flex
           gettext-minimal
           perl
           pkg-config
           python-wrapper
           qttools))
    (synopsis "Network traffic analyzer")
    (description "Wireshark is a network protocol analyzer, or @dfn{packet
sniffer}, that lets you capture and interactively browse the contents of
network frames.")
    (home-page "https://www.wireshark.org/")
    (license license:gpl2+)))

(define-public fping
  (package
    (name "fping")
    (version "5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://fping.org/dist/fping-"
                           version ".tar.gz"))
       (sha256
        (base32 "1f2prmii4fyl44cfykp40hp4jjhicrhddh9v3dfs11j6nsww0f7d"))))
    (build-system gnu-build-system)
    (home-page "https://fping.org/")
    (synopsis "Send ICMP ECHO_REQUEST packets to network hosts")
    (description
     "fping is a ping-like program which uses @acronym{ICMP, Internet Control
Message Protocol} echo requests to determine if a target host is responding.

@command{fping} differs from @command{ping} in that you can specify any number
of targets on the command line, or specify a file containing the lists of
targets to ping.  Instead of sending to one target until it times out or
replies, fping will send out a ping packet and move on to the next target in a
round-robin fashion.")
    (license license:expat)))

(define-public gandi.cli
  (package
    (name "gandi.cli")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32 "1h36jahbp7273wn3yd747kbiwjc0bm3sja67bcxdsd54ln0vyndg"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'embed-store-file-names
           (lambda _
             (substitute* (list "gandi/cli/modules/cert.py"
                                "gandi/cli/tests/commands/test_certificate.py")
               (("openssl") (which "openssl")))
             #t))
         (add-after 'install 'install-documentation
           ;; The included man page may be outdated but we install it anyway,
           ;; since it's mentioned in 'gandi --help' and better than nothing.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (man1 (string-append out "/share/man/man1")))
               (mkdir-p man1)
               (with-output-to-file (string-append man1 "/gandi.1")
                 (lambda _
                   (invoke "rst2man.py" "gandicli.man.rst")))
               #t))))))
    (native-inputs
     (list python-docutils ; for rst2man.py
           python-pytest python-pytest-cov python-tox))
    (propagated-inputs
     (list openssh))           ; used by gandi/cli/modules/iass.py
    (inputs
     (list openssl python-click python-ipy python-pyyaml python-requests))
    (home-page "https://cli.gandi.net")
    (synopsis "Command-line interface to the Gandi.net Web API")
    (description
     "This package provides a command-line client (@command{gandi}) to buy,
manage, and delete Internet resources from Gandi.net such as domain names,
virtual machines, and certificates.")
    (license license:gpl3+)))

(define-public go-netns
  (let ((commit "13995c7128ccc8e51e9a6bd2b551020a27180abd")
        (revision "1"))
    (package
      (name "go-netns")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/vishvananda/netns")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zk6w8158qi4niva5rijchbv9ixgmijsgqshh54wdaav4xrhjshn"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/vishvananda/netns"
         #:tests? #f))                  ;tests require root privileges
      (home-page "https://github.com/vishvananda/netns")
      (synopsis "Simple network namespace handling for Go")
      (description "The netns package provides a simple interface for
handling network namespaces in Go.")
      (license license:asl2.0))))

(define-public go-sctp
  ;; docker-libnetwork-cmd-proxy requires this exact commit.
  ;; This commit is mentioned in docker-libnetwork-cmd-proxy's vendor.conf.
  (let ((commit "6e2cb1366111dcf547c13531e3a263a067715847")
        (revision "2"))
    (package
      (name "go-sctp")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ishidawataru/sctp")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ba90fmpdwxa1ba4hrsjhi3gfy3pwmz7x8amw1p5dc9p5a7nnqrb"))))
      (build-system go-build-system)
      (arguments
       `(#:tests? #f    ; Test suite is flakey.
         #:import-path "github.com/ishidawataru/sctp"))
      (home-page "https://github.com/ishidawataru/sctp")
      (synopsis "SCTP library for the Go programming language")
      (description "This library provides methods for using the stream control
transmission protocol (SCTP) in a Go application.")
      (license license:asl2.0))))

(define-public httping
  (package
    (name "httping")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.vanheusden.com/httping/httping-"
                           version ".tgz"))
       (sha256
        (base32
         "1y7sbgkhgadmd93x1zafqc4yp26ssiv16ni5bbi9vmvvdl55m29y"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     (list fftw ncurses openssl))
    (arguments
     `(#:make-flags (list ,(string-append "CC=" (cc-for-target))
                          (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "PREFIX=")
       #:tests? #f)) ; no tests
    (home-page "https://www.vanheusden.com/httping/")
    (synopsis "Web server latency and throughput monitor")
    (description
     "httping measures how long it takes to connect to a web server, send an
HTTP(S) request, and receive the reply headers.  It is somewhat similar to
@command{ping}, but can be used even in cases where ICMP traffic is blocked
by firewalls or when you want to monitor the response time of the actual web
application stack itself.")
    (license license:gpl2)))        ; with permission to link with OpenSSL

(define-public httpstat
  (package
    (name "httpstat")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/reorx/httpstat")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cw8299a080m42slsimz31xs0gjnh833gpbj2dsr4hkcinrn4iyd"))))
    (build-system python-build-system)
    (inputs (list curl))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-curl-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "httpstat.py"
               (("ENV_CURL_BIN.get\\('curl'\\)")
                (string-append "ENV_CURL_BIN.get('"
                               (assoc-ref inputs "curl")
                               "/bin/curl')"))
               ;; "curl -w time_*" units seems to have
               ;; changed from seconds to nanoseconds.
               (("d\\[k\\] \\* 1000") "d[k] / 1000"))
             #t)))))
    (home-page "https://github.com/reorx/httpstat")
    (synopsis "Visualize curl statistics")
    (description
     "@command{httpstat} is a tool to visualize statistics from the
@command{curl} HTTP client.  It acts as a wrapper for @command{curl} and
prints timing information for each step of the HTTP request (DNS lookup,
TCP connection, TLS handshake and so on) in the terminal.")
    (license license:expat)))

(define-public squid
  (package
    (name "squid")
    (version "4.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.squid-cache.org/Versions/v4/squid-"
                           version ".tar.xz"))
       (sha256
        (base32 "060lwghn6q982bay11ia38c86kd8w6mjgy68n58v31kwik08m4nb"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; disable -march=native in build for reproducibility; see
       ;; https://wiki.squid-cache.org/KnowledgeBase/IllegalInstructionError
       (list "--disable-arch-native")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-true-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "test-suite/testheaders.sh"
               (("/bin/true")
                (search-input-file inputs "/bin/true"))))))))
    (inputs
     (list perl
           openldap
           linux-pam
           libcap
           cyrus-sasl
           expat
           libxml2
           openssl))
    (native-inputs
     (list cppunit pkg-config))
    (synopsis "Web caching proxy")
    (description "Squid is a caching proxy for the Web supporting HTTP, HTTPS,
FTP, and more.  It reduces bandwidth and improves response times by caching and
reusing frequently-requested web pages.")
    (home-page "http://www.squid-cache.org/")
    (license license:gpl2+)))

(define-public bwm-ng
  (package
    (name "bwm-ng")
    (version "0.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vgropp/bwm-ng")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gpp2l3w479h1w5skjra5xy0gxd24kvmk6i4psbkafnv2399la4k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-premature-./configure
           (lambda _
             (substitute* "autogen.sh"
               (("\\$srcdir/configure")
                "true"))
             #t)))))
    (native-inputs
     (list autoconf automake))
    (inputs
     (list ncurses))
    (synopsis "Console based live network and disk I/O bandwidth monitor")
    (description "Bandwidth Monitor NG is a small and simple console based
live network and disk I/O bandwidth monitor.")
    (home-page "https://www.gropp.org/?id=projects&sub=bwm-ng")
    (license license:gpl2)))

(define-public aircrack-ng
  (package
    (name "aircrack-ng")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.aircrack-ng.org/aircrack-ng-"
                           version ".tar.gz"))
       (sha256
        (base32 "0ix2k64qg7x3w0bzdsbk1m50kcpq1ws59g3zkwiafvpwdr4gs2sg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config which))
    (inputs
     (list libgcrypt
           libnl
           libpcap
           ethtool
           pcre
           sqlite
           zlib))
    (arguments
     `(#:configure-flags
       (list "CFLAGS=-fcommon"
             "--with-experimental=yes"  ; build wesside-ng, etc.
             "--with-gcrypt")           ; openssl's the default
       #:phases (modify-phases %standard-phases
                  (add-before 'bootstrap 'patch-evalrev
                    (lambda _
                      ;; Called by ./autogen.sh below, before the default
                      ;; ‘patch-shebangs’ phase has had a chance to run.
                      (substitute* "evalrev"
                        (("/bin/sh")
                         (which "sh")))
                      #t))
                  (add-after 'build 'absolutize-tools
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((ethtool (search-input-file inputs
                                                        "/sbin/ethtool")))
                        (substitute* "scripts/airmon-ng"
                          (("ethtool ")
                           (string-append ethtool " ")))
                        #t))))))
    (home-page "https://www.aircrack-ng.org")
    (synopsis "Assess WiFi network security")
    (description
     "Aircrack-ng is a complete suite of tools to assess WiFi network
security.  It focuses on different areas of WiFi security: monitoring,
attacking, testing, and cracking.  All tools are command-line driven, which
allows for heavy scripting.")
    (license (list license:gpl2+ license:bsd-3))))

(define-public pixiewps
  (package
    (name "pixiewps")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/wiire-a/pixiewps/releases/"
                    "download/v" version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "07nym6bqml0k9v29vnj003nrgnwrywgjvnljb7cdpsvnwilhbp64"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)) ; no configure script
       #:tests? #f)) ; there are no tests
    (home-page "https://github.com/wiire-a/pixiewps/")
    (synopsis "Offline brute-force tool for Wi-Fi Protected Setup")
    (description "Pixiewps implements the pixie-dust attack to brute
force the Wi-Fi Protected Setup (WPS) PIN by exploiting the low or
non-existing entropy of some access points.")
    (license license:gpl3+)))

(define-public reaver
  (package
    (name "reaver")
    (version "1.6.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/t6x/reaver-wps-fork-t6x/releases/"
                    "download/v" version "/reaver-" version ".tar.xz"))
              (sha256
               (base32
                "00k7mc81ifv0wma7k4v18mj498badbw5yls6c28qin3d1gda0ag3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ;; Save session files to current directory instead of /var.
       (list "--enable-savetocurrent"
             "--localstatedir=/tmp/dummy") ; prevent creating /var during install
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'change-directory
           (lambda _
             (chdir "src")
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir "../docs")
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version))
                    (man1 (string-append out "/share/man/man1")))
               (for-each (lambda (file) (install-file file doc))
                         (find-files "." "README.*"))
               (install-file "reaver.1" man1)
               #t))))
       #:tests? #f))                    ; there are no tests
    (inputs
     (list libpcap))
    (propagated-inputs
     (list aircrack-ng pixiewps))
    (home-page "https://github.com/t6x/reaver-wps-fork-t6x/")
    (synopsis "Attack tool for Wi-Fi Protected Setup")
    (description "Reaver performs a brute force attack against an access
point's Wi-Fi Protected Setup (WPS) PIN.  Once the PIN is found, the WPA
passphrase can be recovered and the AP's wireless settings can be
reconfigured.")
    (license license:gpl2+)))

(define-public perl-danga-socket
  (package
    (name "perl-danga-socket")
    (version "1.62")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NM/NML/"
                           "Danga-Socket-" version ".tar.gz"))
       (sha256
        (base32 "0x4bvirmf0kphks19jwgva00zz73zx344218dfaiv8gigrw3yg4m"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-tcp))
    (propagated-inputs
     (list perl-sys-syscall))
    (home-page "https://metacpan.org/release/Danga-Socket")
    (synopsis "Event loop and event-driven async socket base class")
    (description
     "Danga::Socket is an abstract base class for objects backed by a socket
which provides the basic framework for event-driven asynchronous IO, designed
to be fast.  Danga::Socket is both a base class for objects, and an event
loop.")
    (license license:perl-license)))

(define-public perl-data-validate-ip
  (package
    (name "perl-data-validate-ip")
    (version "0.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DR/DROLSKY/Data-Validate-IP-"
             version ".tar.gz"))
       (sha256
        (base32 "074adrlvkiahj1fdc9nvb95dpfyjzm2jzhi90m8xaw4bw5ipcbzy"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-requires))
    (propagated-inputs
     (list perl-netaddr-ip))
    (home-page "https://metacpan.org/release/Data-Validate-IP")
    (synopsis "IPv4 and IPv6 validation methods")
    (description
     "This module provides several IP address validation subroutines that both
validate and untaint their input.  This includes both basic validation
(@code{is_ipv4()} and @code{is_ipv6()}) and special cases like checking whether
an address belongs to a specific network or whether an address is public or
private (reserved).")
    (license license:perl-license)))

(define-public perl-net-dns
 (package
  (name "perl-net-dns")
  (version "1.31")
  (source
    (origin
      (method url-fetch)
      (uri
       (list
        (string-append "https://www.net-dns.org/download/Net-DNS-"
                       version ".tar.gz")
        (string-append "mirror://cpan/authors/id/N/NL/NLNETLABS/Net-DNS-"
                       version ".tar.gz")))
      (sha256
       (base32 "05f6rzvvmm6xd0p100k5y9kczdzqgala09ra8bccc18n6y74l0h0"))))
  (build-system perl-build-system)
  (inputs
    (list perl-digest-hmac))
  (home-page "https://www.net-dns.org/")
  (synopsis
    "Perl Interface to the Domain Name System")
  (description "Net::DNS is the Perl Interface to the Domain Name System.")
  (license license:x11)))

(define-public perl-socket6
 (package
  (name "perl-socket6")
  (version "0.29")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/U/UM/UMEMOTO/Socket6-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "054izici8klfxs8hr5rljib28plijpsfymy99xbzdp047bx1b2a6"))))
  (build-system perl-build-system)
  (arguments
   `(#:phases
     (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (args `("Makefile.PL"
                            ,(string-append "PREFIX=" out)
                            "INSTALLDIRS=site")))
               (setenv "CONFIG_SHELL" (which "sh"))
               (apply invoke "perl" args)))))))
  (home-page "https://metacpan.org/release/Socket6")
  (synopsis
    "IPv6 related part of the C socket.h defines and structure manipulators for Perl")
  (description "Socket6 binds the IPv6 related part of the C socket header
definitions and structure manipulators for Perl.")
  (license license:bsd-3)))

(define-public perl-net-dns-resolver-programmable
 (package
  (name "perl-net-dns-resolver-programmable")
  (version "0.003")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/J/JM/JMEHNLE/net-dns-resolver-programmable/"
             "Net-DNS-Resolver-Programmable-v" version ".tar.gz"))
      (sha256
        (base32
          "1v3nl2kaj4fs55n1617n53q8sa3mir06898vpy1rq98zjih24h4d"))
      (patches
       (search-patches "perl-net-dns-resolver-programmable-fix.patch"))))
  (build-system perl-build-system)
  (native-inputs
    (list perl-module-build))
  (inputs (list perl-net-dns))
  (home-page
    "https://metacpan.org/release/Net-DNS-Resolver-Programmable")
  (synopsis
    "Programmable DNS resolver class for offline emulation of DNS")
  (description "Net::DNS::Resolver::Programmable is a programmable DNS resolver for
offline emulation of DNS.")
  (license license:perl-license)))

(define-public perl-net-dns-resolver-mock
  (package
    (name "perl-net-dns-resolver-mock")
    (version "1.20171219")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "mirror://cpan/authors/id/M/MB/MBRADSHAW/"
                     "Net-DNS-Resolver-Mock-" version ".tar.gz"))
              (sha256
               (base32
                "0m3rxpkv1b9121srvbqkrgzg4m8mnydiydqv34in1i1ixwrl6jn9"))))
    (build-system perl-build-system)
    (inputs
     (list perl-net-dns))
    (home-page "https://metacpan.org/release/Net-DNS-Resolver-Mock")
    (synopsis "Mock DNS Resolver object for testing")
    (description
     "Net::DNS::Resolver::Mock is a subclass of Net::DNS::Resolver, but returns
static data from any provided DNS zone file instead of querying the network.
It is intended primarily for use in testing.")
    (license license:perl-license)))

(define-public perl-netaddr-ip
 (package
  (name "perl-netaddr-ip")
  (version "4.079")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/M/MI/MIKER/NetAddr-IP-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1rx0dinrz9fk9qcg4rwqq5n1dm3xv2arymixpclcv2q2nzgq4npc"))))
  (build-system perl-build-system)
  (arguments
    `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (args `("Makefile.PL"
                            ,(string-append "PREFIX=" out)
                            "INSTALLDIRS=site")))
               (setenv "CONFIG_SHELL" (which "sh"))
               (apply invoke "perl" args)))))))
  (home-page
    "https://metacpan.org/release/NetAddr-IP")
  (synopsis
    "Manages IPv4 and IPv6 addresses and subnets")
  (description "NetAddr::IP manages IPv4 and IPv6 addresses and subsets.")
  (license license:perl-license)))

(define-public perl-net-patricia
 (package
  (name "perl-net-patricia")
  (version "1.22")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/G/GR/GRUBER/Net-Patricia-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0ln5f57vc8388kyh9vhx2infrdzfhbpgyby74h1qsnhwds95m0vh"))))
  (build-system perl-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'dont-link-with-nsl ; Borrowed from Debian.
         (lambda _
           (substitute* "Makefile.PL"
             (("-lnsl") ""))
           #t)))))
  (inputs
    (list perl-net-cidr-lite perl-socket6))
  (home-page
    "https://metacpan.org/release/Net-Patricia")
  (synopsis
    "Patricia Trie Perl module for fast IP address lookups")
  (description
    "Net::Patricia does IP address lookups quickly in Perl.")
  ;; The bindings are licensed under GPL2 or later.
  ;; libpatricia is licensed under 2-clause BSD.
  (license (list license:gpl2+ license:bsd-2))))

(define-public perl-net-cidr-lite
 (package
  (name "perl-net-cidr-lite")
  (version "0.22")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/S/ST/STIGTSP/Net-CIDR-Lite-"
             version
             ".tar.gz"))
      (sha256
        (base32 "05w57db2lx4djb4vixzdr6qgrzyzkk047nl812g7nq8s6k5xh5s3"))))
  (build-system perl-build-system)
  (home-page "https://metacpan.org/release/Net-CIDR-Lite")
  (synopsis "Perl extension for merging IPv4 or IPv6 CIDR addresses")
  (description "Net::CIDR::Lite merges IPv4 or IPv6 CIDR addresses.")
  (license license:gpl1+)))

(define-public perl-io-socket-inet6
 (package
  (name "perl-io-socket-inet6")
  (version "2.72")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/S/SH/SHLOMIF/IO-Socket-INET6-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1fqypz6qa5rw2d5y2zq7f49frwra0aln13nhq5gi514j2zx21q45"))))
  (build-system perl-build-system)
  (native-inputs
    (list perl-module-build perl-test-pod perl-test-pod-coverage))
  (propagated-inputs (list perl-socket6))
  (arguments `(;; Need network socket API
               #:tests? #f))
  (home-page
    "https://metacpan.org/release/IO-Socket-INET6")
  (synopsis
    "Perl object interface for AF_INET/AF_INET6 domain sockets")
  (description "IO::Socket::INET6 is an interface for AF_INET/AF_INET6 domain
sockets in Perl.")
  (license license:perl-license)))

(define-public libproxy
  (package
    (name "libproxy")
    (version "0.4.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libproxy/libproxy/"
                                  "releases/download/" version "/libproxy-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "01cbgz6lc3v59sldqk96l1281kp2qxnsa2qwlf2ikvjlyr1gi2dw"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list dbus zlib))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "ctest" "-E" "url-test")))))))
    (synopsis "Library providing automatic proxy configuration management")
    (description "Libproxy handles the details of HTTP/HTTPS proxy
configuration for applications across all scenarios.  Applications using
libproxy only have to specify which proxy to use.")
    (home-page "https://libproxy.github.io/libproxy")
    (license license:lgpl2.1+)))

(define-public proxychains-ng
  (package
    (name "proxychains-ng")
    (version "4.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://ftp.barfooze.de/pub/sabotage/tarballs/"
                           "proxychains-ng-" version ".tar.xz"))
       (sha256
        (base32 "10ch6rmbw2lwrq1bc9w4glxkws7hvsy5ihasvzf3yg053xzsn1rj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-configure-script
           (lambda _
             ;; The configure script is very intolerant to unknown arguments,
             ;; such as "CONFIG_SHELL".
             (substitute* "configure"
               (("\\*\\) break ;;" line)
                (string-append "[A-Z]*) shift ;;\n"
                               line)))))
         (add-before 'configure 'set-up-environment
           (lambda _
             (setenv "CC" ,(cc-for-target)))))))
    (synopsis "Redirect any TCP connection through a proxy or proxy chain")
    (description "Proxychains-ng is a preloader which hooks calls to sockets
in dynamically linked programs and redirects them through one or more SOCKS or
HTTP proxies.")
    (home-page "https://github.com/rofl0r/proxychains-ng")
    (license license:gpl2+)))

(define-public enet
  (package
    (name "enet")
    (version "1.3.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://enet.bespin.org/download/"
                           "enet-" version ".tar.gz"))
       (sha256
        (base32 "1p6f9mby86af6cs7pv6h48032ip9g32c05cb7d9mimam8lchz3x3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (synopsis "Network communication layer on top of UDP")
    (description
     "ENet's purpose is to provide a relatively thin, simple and robust network
communication layer on top of UDP.  The primary feature it provides is optional
reliable, in-order delivery of packets.  ENet omits certain higher level
networking features such as authentication, server discovery, encryption, or
other similar tasks that are particularly application specific so that the
library remains flexible, portable, and easily embeddable.")
    (home-page "http://enet.bespin.org")
    (license license:expat)))

(define-public sslh
  (package
    (name "sslh")
    (version "1.21c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yrutschle/sslh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19h32dn0076p3s7dn35qi5yp2xvnxw9sqphppmn72vyb8caxvw1z"))))
    (build-system gnu-build-system)
    (native-inputs
     (list ;; Test dependencies.
           lcov
           perl
           perl-conf-libconfig
           perl-io-socket-inet6
           perl-socket6
           psmisc))             ; for ‘killall’
    (inputs
     (list libcap libconfig pcre tcp-wrappers))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'check 'fix-tests
           (lambda _
             (substitute* "./t"
               (("\"/tmp") "$ENV{\"TMPDIR\"} . \"")
               ;; The Guix build environment lacks ‘ip6-localhost’.
               (("ip6-localhost") "localhost"))
             #t))
         ;; Many of these files are mentioned in the man page. Install them.
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/sslh")))
               (install-file "README.md" doc)
               (for-each
                (lambda (file)
                  (install-file file (string-append doc "/examples")))
                (append (find-files "." "\\.cfg")
                        (find-files "scripts"))))
             #t)))
       #:make-flags (list ,(string-append "CC=" (cc-for-target))
                          "USELIBCAP=1"
                          "USELIBWRAP=1"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:test-target "test"))
    (home-page "https://www.rutschle.net/tech/sslh/README.html")
    (synopsis "Applicative network protocol demultiplexer")
    (description
     "sslh is a network protocol demultiplexer.  It acts like a switchboard,
accepting connections from clients on one port and forwarding them to different
servers based on the contents of the first received data packet.  Detection of
common protocols like HTTP(S), SSL, SSH, OpenVPN, tinc, and XMPP is already
implemented, but any other protocol that matches a regular expression can be
added.  sslh's name comes from its original application of serving both SSH and
HTTPS on port 443, allowing SSH connections from inside corporate firewalls
that block port 22.")
    (license (list license:bsd-2        ; tls.[ch]
                   license:gpl2+))))    ; everything else

(define-public iperf
  (package
    (name "iperf")
    (version "3.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/esnet/iperf")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0svkrmxki6ckn2a8xysh5x0bw68mqnvl3w64j6d45fxs97dca2vq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (synopsis "TCP, UDP and SCTP bandwidth measurement tool")
    (description
     "iPerf is a tool to measure achievable bandwidth on IP networks.  It
supports tuning of various parameters related to timing, buffers and
protocols (TCP, UDP, SCTP with IPv4 and IPv6).  For each test it reports
the bandwidth, loss, and other parameters.")
    (home-page "https://software.es.net/iperf/")
    (license (list license:bsd-3             ; Main distribution.
                   license:ncsa              ; src/{units,iperf_locale,tcp_window_size}.c
                   license:expat             ; src/{cjson,net}.[ch]
                   license:public-domain)))) ; src/portable_endian.h

(define-public nethogs
  (package
    (name "nethogs")
    (version "0.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/raboof/nethogs")
             (commit (string-append "v" version))))
       (hash
        (content-hash
         (base32 "0sn1sdp86akwlm4r1vmkxjjl50c0xaisk91bbz57z7kcsaphxna9")
         sha256))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     (list libpcap ncurses))
    (arguments
     `(#:make-flags `(,,(string-append "CC=" (cc-for-target))
                      ,(string-append "PREFIX=" %output)
                      ,(string-append "VERSION=" ,version))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no ./configure script.
    (home-page "https://github.com/raboof/nethogs")
    (synopsis "Per-process bandwidth monitor")
    (description "NetHogs is a small 'net top' tool for Linux.  Instead of
breaking the traffic down per protocol or per subnet, like most tools do, it
groups bandwidth by process.

NetHogs does not rely on a special kernel module to be loaded.  If there's
suddenly a lot of network traffic, you can fire up NetHogs and immediately see
which PID is causing this.  This makes it easy to identify programs that have
gone wild and are suddenly taking up your bandwidth.")
    (license license:gpl2+)))

(define-public nzbget
  (package
    (name "nzbget")
    (version "21.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/nzbget/nzbget/releases"
                           "/download/v" version
                           "/nzbget-" version "-src.tar.gz"))
       (sha256
        (base32 "09900x1k0yf4yi2cc0k093advvadyhrkm8rnd8nszhhdp2zc33sf"))))
    (arguments
     `(#:configure-flags
       (list
        (string-append "--with-libcurses-includes="
                       (assoc-ref %build-inputs "ncurses") "/include")
        (string-append "--with-libcurses-libraries="
                       (assoc-ref %build-inputs "ncurses") "/lib")
        (string-append "--with-tlslib=GnuTLS"))))
    (build-system gnu-build-system)
    (inputs (list gnutls libxml2 ncurses zlib))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/nzbget/nzbget")
    (synopsis "Usenet binary file downloader")
    (description
     "NZBGet is a binary newsgrabber, which downloads files from Usenet based
on information given in @code{nzb} files.  NZBGet can be used in standalone
and in server/client modes.  In standalone mode, you pass NZBGet @command{nzb}
files as command-line parameters and it downloads them and exits.  NZBGet also
contains a Web interface.  Its server can be controlled through remote
procedure calls (RPCs).")
    (license license:gpl2+)))

(define-public openvswitch
  (package
    (name "openvswitch")
    (version "2.16.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.openvswitch.org/releases/openvswitch-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1x0k0pw6jykrfgb8rq56bp2ghxd433d55pmr8mxy9gbzw1nc1vbi"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       '("--enable-shared"
         "--disable-static"        ; XXX still installs libopenvswitchavx512.a
         "--localstatedir=/var"
         "--with-dbdir=/var/lib/openvswitch")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-absolute-/bin/sh
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((bash (assoc-ref inputs "bash-minimal")))
               (substitute* "ovsdb/ovsdb-server.c"
                 (("/bin/sh") (string-append bash "/bin/sh"))))))
         (add-before 'check 'adjust-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((bash (assoc-ref inputs "bash-minimal"))
                    (/bin/sh (string-append bash "/bin/sh")))
               (with-fluids ((%default-port-encoding "ISO-8859-1"))
                 (substitute* (find-files "tests" ".*(run|testsuite)$")
                   (("#! /bin/sh")
                    (string-append "#! " /bin/sh))

                   ;; The tests use 'kill -0' to check whether a test has
                   ;; completed, but it does not work in the build container
                   ;; because zombies are not reaped automatically (PID 1 is
                   ;; the builder script).  Change to something that handles
                   ;; undead processes.
                   (("kill -0")
                    "kill-0")))
               (mkdir "/tmp/bin")
               (call-with-output-file "/tmp/bin/kill-0"
                 (lambda (port)
                   (format port "#!~a
ps --no-header -p $1 -o state= | grep -qv '^Z$'"
                           /bin/sh)))
               (chmod "/tmp/bin/kill-0" #o755)
               (setenv "PATH"
                       (string-append "/tmp/bin:" (getenv "PATH"))))))
         (replace 'install
           (lambda _
             (invoke "make"
                     ;; Don't try to create directories under /var.
                     "RUNDIR=/tmp"
                     "PKIDIR=/tmp"
                     "LOGDIR=/tmp"
                     "DBDIR=/tmp"
                     "install"))))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ;; for testing
       ("bash" ,bash)                   ;for 'compgen'
       ("procps" ,procps)
       ("util-linux" ,util-linux)))
    (inputs
     (list bash-minimal libcap-ng openssl))
    (synopsis "Virtual network switch")
    (home-page "https://www.openvswitch.org/")
    (description
     "Open vSwitch is a multilayer virtual switch.  It is designed to enable
massive network automation through programmatic extension, while still
supporting standard management interfaces and protocols (e.g. NetFlow, sFlow,
IPFIX, RSPAN, CLI, LACP, 802.1ag).")
    (license                            ; see debian/copyright for detail
     (list license:lgpl2.1              ; xenserver and utilities/bugtool
           license:gpl2                 ; datapath
           license:bsd-2 license:bsd-3
           license:asl2.0))))           ; all other

(define-public python-ipy
  (package
    (name "python-ipy")
    (version "1.00")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "IPy" version))
              (sha256
               (base32
                "08d6kcacj67mvh0b6y765ipccy6gi4w2ndd4v1l3im2qm1cgcarg"))))
    (build-system python-build-system)
    (home-page "https://github.com/autocracy/python-ipy/")
    (synopsis "Python class and tools for handling IP addresses and networks")
    (description "The @code{IP} class allows a comfortable parsing and
handling for most notations in use for IPv4 and IPv6 addresses and
networks.")
    (license license:bsd-3)))

(define-public python2-ipy
  (package-with-python2 python-ipy))

(define-public speedtest-cli
  (package
    (name "speedtest-cli")
    (version "2.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sivel/speedtest-cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10fazl4kwf41mk7pnwpfms16n0ii0kg9pf8r3mz9xwnl9y04mv9x"))))
    (build-system python-build-system)
    (home-page "https://github.com/sivel/speedtest-cli")
    (synopsis "Internet bandwidth tester")
    (description
     "Command line interface for testing internet bandwidth using
speedtest.net.")
    (license license:asl2.0)))

(define-public tftp-hpa
  (package
    (name "tftp-hpa")
    (version "5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/"
                                  "network/tftp/tftp-hpa/tftp-hpa-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "12vidchglhyc20znq5wdsbhi9mqg90jnl7qr9qs8hbvaz4fkdvmg"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no test target
           #:configure-flags
           #~(list "CFLAGS=-fcommon")))   ; XXX fix 5.2 build with GCC 10
    (synopsis "HPA's tftp client")
    (description
     "This is a tftp client derived from OpenBSD tftp with some extra options
added and bugs fixed.  The source includes readline support but it is not
enabled due to license conflicts between the BSD advertising clause and the GPL.")
    (home-page "https://git.kernel.org/cgit/network/tftp/tftp-hpa.git/about/")
    ;; Some source files are distributed under a 3-clause BSD license, and
    ;; others under a 4-clause BSD license. Refer to the files in the source
    ;; distribution for clarification.
    (license (list license:bsd-3 license:bsd-4))))

(define-public spiped
  (package
    (name "spiped")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.tarsnap.com/spiped/spiped-"
                                  version ".tgz"))
              (sha256
               (base32
                "04rpnc53whfky7pp2m9h35gwzwn6788pnl6c1qd576mpknbqjw4d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags (let* ((out (assoc-ref %outputs "out"))
                           (bindir (string-append out "/bin"))
                           (man1dir (string-append out "/share/man/man1")))
                      (list ,(string-append "CC=" (cc-for-target)) ; It tries to invoke `c99`.
                            (string-append "BINDIR=" bindir)
                            (string-append "MAN1DIR=" man1dir)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-command-invocations
           (lambda _
             (substitute* '("Makefile"
                            "libcperciva/cpusupport/Build/cpusupport.sh"
                            "libcperciva/POSIX/posix-cflags.sh"
                            "libcperciva/POSIX/posix-l.sh")
               (("command -p") ""))
             #t))
         (delete 'configure) ; No ./configure script.
         (add-after 'install 'install-more-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (misc (string-append out "/share/doc/spiped")))
               (install-file "DESIGN.md" misc)
               #t))))))
    (native-inputs
     (list procps)) ; `ps` is used by the test suite.
    (inputs
     (list openssl))
    (home-page "https://www.tarsnap.com/spiped.html")
    (synopsis "Create secure pipes between sockets")
    (description "Spiped (pronounced \"ess-pipe-dee\") is a utility for creating
symmetrically encrypted and authenticated pipes between socket addresses, so
that one may connect to one address (e.g., a UNIX socket on localhost) and
transparently have a connection established to another address (e.g., a UNIX
socket on a different system).  This is similar to 'ssh -L' functionality, but
does not use SSH and requires a pre-shared symmetric key.")
    (license license:bsd-2)))

(define-public quagga
  (package
    (name "quagga")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              ;; Use archived sources; see <http://issues.guix.gnu.org/47123>.
              (uri (string-append "https://fossies.org/linux/misc/"
                                  "quagga-" version ".tar.gz"))
              (sha256
               (base32
                "1lsksqxij5f1llqn86pkygrf5672kvrqn1kvxghi169hqf1c0r73"))
              (patches
               (search-patches "quagga-reproducible-build.patch"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config perl dejagnu))
    (inputs (list readline c-ares))
    (synopsis "Routing Software Suite")
    (description "Quagga is a routing software suite, providing implementations
of OSPFv2, OSPFv3, RIP v1 and v2, RIPng and BGP-4 for Unix platforms.

The Quagga architecture consists of a core daemon, @command{zebra}, which
acts as an abstraction layer to the underlying Unix kernel and presents the
Zserv API over a Unix or TCP stream to Quagga clients.  It is these Zserv
clients which typically implement a routing protocol and communicate routing
updates to the zebra daemon.")
    (home-page "https://www.nongnu.org/quagga/")
    (license license:gpl2+)))

(define-public thc-ipv6
  (let ((revision "0")
        (commit "4bb72573e0950ce6f8ca2800a10748477020029e"))
    (package
      (name "thc-ipv6")
      (version (git-version "3.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/vanhauser-thc/thc-ipv6")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1x5i6vbsddqc2yks7r1a2fw2fk16qxvd6hpzh1lykjfpkal8fdir"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:tests? #f ; No test suite.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; No ./configure script.
           (add-before 'build 'patch-paths
             (lambda _
               (substitute* "Makefile"
                 (("/bin/echo") "echo"))
               #t))
           (add-after 'install 'install-more-docs
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (doc (string-append out "/share/thc-ipv6/doc")))
                 (install-file "README" doc)
                 (install-file "HOWTO-INJECT" doc)
                 #t))))))
      ;; TODO Add libnetfilter-queue once packaged.
      (inputs
       (list libpcap openssl perl))
      (home-page "https://github.com/vanhauser-thc/thc-ipv6")
      (synopsis "IPv6 security research toolkit")
      (description "The THC IPv6 Toolkit provides command-line tools and a library
for researching IPv6 implementations and deployments.  It requires Linux 2.6 or
newer and only works on Ethernet network interfaces.")
      ;; AGPL 3 with exception for linking with OpenSSL. See the 'LICENSE' file in
      ;; the source distribution for more information.
      (license license:agpl3))))

(define-public bmon
  (package
    (name "bmon")
    (version "4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/tgraf/bmon/releases/download/v"
                           version "/bmon-" version ".tar.gz"))
       (sha256
        (base32
         "0ylzriv4pwh76344abzl1w219x188gshbycbna35gsyfp09c7z82"))))
    (build-system gnu-build-system)
    (inputs
     (list libconfuse libnl ncurses))
    (native-inputs
     (list pkg-config))
    (synopsis "Bandwidth monitor")
    (description "bmon is a monitoring and debugging tool to capture
networking-related statistics and prepare them visually in a human-friendly
way.  It features various output methods including an interactive curses user
interface and a programmable text output for scripting.")
    (home-page "https://github.com/tgraf/bmon")
    ;; README.md mentions both the 2-clause BSD and expat licenses, but all
    ;; the source files only have expat license headers. Upstream has been
    ;; contacted for clarification: https://github.com/tgraf/bmon/issues/59
    ;; Update the license field when upstream responds.
    (license (list license:bsd-2
                   license:expat))))

(define-public libnet
  (package
    (name "libnet")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libnet/libnet/releases/download"
                           "/v" version "/libnet-" version ".tar.gz"))
       (sha256
        (base32
         "19ys9vxk6fg70yzzdxsphfr0rwzgxxhr9b3ykhpg7rfray0qd96a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-doc
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "-C" "doc" "doc"
                    make-flags))))))
    (native-inputs
     (list ;; To build the documentation, Doxygen and Perl is required.
           doxygen perl))
    (home-page "https://github.com/libnet/libnet")
    (synopsis "Framework for low-level network packet construction")
    (description
     "Libnet provides a fairly portable framework for network packet
construction and injection.  It features portable packet creation interfaces
at the IP layer and link layer, as well as a host of supplementary
functionality.  Using libnet, quick and simple packet assembly applications
can be whipped up with little effort.")
    (license license:bsd-2)))

(define-public mtr
  (package
    (name "mtr")
    (version "0.95")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.bitwizard.nl/mtr/"
                           "mtr-" version ".tar.gz"))
       (sha256
        (base32 "0haanralbvd12pvkyihgkmx9ld74dnzm1s7mzparfandl416ibff"))))
    (build-system gnu-build-system)
    (inputs
     (list libcap ncurses))
    (arguments
     `(#:tests? #f))                    ; tests require network access
    (home-page "https://www.bitwizard.nl/mtr/")
    (synopsis "Network diagnostic tool")
    (description
     "@acronym{mtr, My TraceRoute} combines the functionality of the
@command{traceroute} and @command{ping} programs in a single network diagnostic
tool.  @command{mtr} can use several network protocols to detect intermediate
routers (or @dfn{hops}) between the local host and a user-specified destination.
It then continually measures the response time and packet loss at each hop, and
displays the results in real time.")
    (license license:gpl2+)))

(define-public amule
  (package
    (name "amule")
    (version "2.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/amule-project/amule")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nm4vxgmisn1b6l3drmz0q04x067j2i8lw5rnf0acaapwlp8qwvi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'bootstrap) ; bootstrap phase runs too early.
         (add-after 'patch-source-shebangs 'autogen
           (lambda _
             (invoke "sh" "autogen.sh")
             #t)))
       #:configure-flags
       '("--disable-rpath"
         "--enable-wxcas"
         "--enable-cas"
         "--enable-alc"
         "--enable-alcc"
         "--enable-xas"
         "--enable-amulecmd"
         "--enable-geoip"
         "--enable-ccache"
         "--enable-nls"
         "--enable-optimize"
         "--enable-amule-gui"
         "--enable-amule-daemon"
         "--enable-webserver"
         "--with-denoise-level=0")))
    (native-inputs
     (list autoconf automake gettext-minimal perl))
    (inputs
     (list zlib crypto++ libpng wxwidgets-gtk2))
    (home-page "https://amule.org/")
    (synopsis "Peer-to-peer client for the eD2K and Kademlia networks")
    (description
     "aMule is an eMule-like client for the eD2k and Kademlia peer-to-peer
file sharing networks.  It includes a graphical user interface (GUI), a daemon
allowing you to run a client with no graphical interface, and a Web GUI for
remote access.  The @command{amulecmd} command allows you to control aMule
remotely.")
    (license license:gpl2+)))

(define-public zyre
  (package
    (name "zyre")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/zeromq/zyre/releases/download/v"
                              version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "13596507ma1474cjqzxym5jlvcshvw7sjhw80rdz788gyz6kz90b"))))
    (build-system gnu-build-system)
    (inputs (list zeromq czmq libsodium))
    (synopsis "Framework for proximity-based peer-to-peer applications")
    (description "Zyre provides reliable group messaging over local area
networks using zeromq.  It has these key characteristics:

@itemize
@item Zyre needs no administration or configuration.
@item Peers may join and leave the network at any time.
@item Peers talk to each other without any central brokers or servers.
@item Peers can talk directly to each other.
@item Peers can join groups, and then talk to groups.
@item Zyre is reliable, and loses no messages even when the network is heavily loaded.
@item Zyre is fast and has low latency, requiring no consensus protocols.
@item Zyre is designed for WiFi networks, yet also works well on Ethernet networks.
@end itemize")
    (home-page "https://github.com/zeromq/zyre")
    (license license:mpl2.0)))

(define-public libsocketcan
  (package
    (name "libsocketcan")
    (version "0.0.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.pengutronix.de/cgit/tools/libsocketcan")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17z2y2r9xkixhr9bxr50m77fh710afl30s7jdhbxrvf56vmal2jr"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (home-page "https://git.pengutronix.de/cgit/tools/libsocketcan")
    (synopsis "SocketCAN user-space library")
    (description "This library allows controlling basic functions in SocketCAN
from user-space.  It requires a kernel built with SocketCAN support.")
    (license license:lgpl2.1+)))

(define-public can-utils
  (package
    (name "can-utils")
    (version "2020.02.04")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linux-can/can-utils")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a3j1mmnb7pvgc8r7zzp6sdp7903in2hna6bmpraxln7cwlzn4l6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No tests exist.
       #:make-flags (list ,(string-append "CC=" (cc-for-target))
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure))))
    (home-page "https://github.com/linux-can/can-utils")
    (synopsis "CAN utilities")
    (description "This package provides CAN utilities in the following areas:

@itemize
@item Basic tools to display, record, generate and replay CAN traffic
@item CAN access via IP sockets
@item CAN in-kernel gateway configuration
@item CAN bus measurement and testing
@item ISO-TP (ISO15765-2:2016 - this means messages with a body larger than
eight bytes) tools
@item Log file converters
@item Serial Line Discipline configuration for slcan driver
@end itemize")
    ;; Either BSD-3 or GPL-2 can be used.
    (license (list license:bsd-3 license:gpl2))))

(define-public asio
  (package
    (name "asio")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/asio/asio/"
                           version " (Stable)/asio-" version ".tar.bz2"))
       (sha256
        (base32 "0335kyxdnwnp96sh9p3jq1s87qnfmp5l7hzlcdxbbwfzrb9p8hr0"))))
    (build-system gnu-build-system)
    (inputs
     (list boost openssl))
    (arguments
     `(#:configure-flags
       (list
        (string-append "--with-boost=" (assoc-ref %build-inputs "boost"))
        (string-append "--with-openssl=" (assoc-ref %build-inputs "openssl")))))
    (home-page "https://think-async.com/Asio")
    (synopsis "C++ library for ASynchronous network I/O")
    (description "Asio is a cross-platform C++ library for network and
low-level I/O programming that provides developers with a consistent
asynchronous model using a modern C++ approach.")
    (license license:boost1.0)))

(define-public shadowsocks
  (package
    (name "shadowsocks")
    (version "2.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shadowsocks/shadowsocks")
             (commit version)))
       (sha256
        (base32 "02mp5905nz02d7amb4zc77rcrkxmvy8mf5rci7mvy58g24lvbw25"))
       (file-name (git-file-name name version))))
    (inputs
     (list openssl))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-crypto-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "shadowsocks/shell.py"
               (("config\\.get\\('libopenssl', None\\)")
                (format #f "config.get('libopenssl', ~s)"
                        (string-append
                         (assoc-ref inputs "openssl")
                         "/lib/libssl.so")))))))))
    (build-system python-build-system)
    (home-page "https://github.com/shadowsocks/shadowsocks")
    (synopsis "Fast tunnel proxy that helps you bypass firewalls")
    (description
     "This package is a fast tunnel proxy that helps you bypass firewalls.

Features:
@itemize
@item TCP & UDP support
@item User management API
@item TCP Fast Open
@item Workers and graceful restart
@item Destination IP blacklist
@end itemize")
    (license license:asl2.0)))

(define-public net-snmp
  (package
    (name "net-snmp")
    (version "5.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/net-snmp/net-snmp/"
                                  version "/net-snmp-" version ".tar.gz"))
              (sha256
               (base32
                "0gwcyi9qk707jgfsgmdr9w2w3r892fnqaam9v7zxpkg69njd8zzb"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Drop bundled libraries.
                  (delete-file-recursively "snmplib/openssl")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       ;; XXX: With parallel build enabled, Perl modules may not get linked with
       ;; libnetsnmp.  See e.g. <https://bugzilla.novell.com/show_bug.cgi?id=819497>.
       #:parallel-build? #f
       #:configure-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib")
             "--disable-static"
             "--with-logfile=/var/log/snmpd.log"
             (string-append "--with-openssl="
                            (assoc-ref %build-inputs "openssl")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "testing/fulltests/support/simple_TESTCONF.sh"
               (("NETSTAT=\"\"")
                (string-append "NETSTAT=\"" (which "netstat") "\"")))
             (substitute* '("testing/fulltests/default/T065agentextend_simple"
                            "testing/fulltests/default/T115agentxperl_simple")
               (("/usr/bin/env") (which "env")))
             (substitute* "testing/fulltests/default/T065agentextend_sh_simple"
               (("/bin/sh") (which "sh")))
             ;; These tests require network access.
             (for-each delete-file
                       '("testing/fulltests/default/T070com2sec_simple"
                         "testing/fulltests/default/T071com2sec6_simple"))
             #t))
         (add-after 'unpack 'patch-Makefile.PL
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("Makefile.PL -NET")
                (string-append "Makefile.PL PREFIX="
                               (assoc-ref outputs "out")
                               " INSTALLDIRS=site" " NO_PERLLOCAL=1"
                               " -NET")))
             #t)))))
    (inputs
     (list libnl ncurses ; for the ‘apps’
           openssl perl))
    (native-inputs
     (list pkg-config
           ;; For tests only.
           net-tools coreutils grep))
    (home-page "http://www.net-snmp.org/")
    (synopsis "Simple Network Management Protocol library and tools")
    (description "The @dfn{Simple Network Management Protocol} (SNMP) is a
widely used protocol for monitoring the health and welfare of network
equipment (e.g. routers), computer equipment and even devices like UPSs.
Net-SNMP is a suite of applications used to implement SNMP v1, SNMP v2c and
SNMP v3 using both IPv4 and IPv6.")
    ;; This only affects OpenBSD
    ;; https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2015-8100
    (properties `((lint-hidden-cve . ("CVE-2015-8100"))))
    (license (list license:bsd-3
                   (license:non-copyleft
                    "http://www.net-snmp.org/about/license.html"
                    "CMU/UCD copyright notice")))))

(define-public ubridge
  (package
    (name "ubridge")
    (version "0.9.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GNS3/ubridge")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jg66jhhpv4c9340fsdp64hf9h253i8r81fknxa0gq241ripp3jn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'install 'set-bindir
           (lambda* (#:key  inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (mkdir-p bin)
               (substitute* "Makefile"
                 (("\\$\\(BINDIR\\)") bin)
                 (("\tsetcap cap_net.*$") "")))
             #t)))))
    (inputs
     (list libpcap))
    (home-page "https://github.com/GNS3/ubridge/")
    (synopsis "Bridge for UDP tunnels, Ethernet, TAP and VMnet interfaces")
    (description "uBridge is a simple program to create user-land bridges
between various technologies.  Currently, bridging between UDP tunnels,
Ethernet and TAP interfaces is supported.  Packet capture is also supported.")
    (license license:gpl3+)))

(define-public hcxtools
  (package
    (name "hcxtools")
    (version "5.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ZerBea/hcxtools")
             (commit version)))
       (sha256
        (base32 "0k2qlq9hz5zc21nyc6yrnfqzga7hydn5mm0x3rpl2fhkwl81lxcn"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     (list curl libpcap openssl zlib))
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target))
             (string-append "INSTALLDIR=" (assoc-ref %outputs "out") "/bin"))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/ZerBea/hcxtools")
    (synopsis "Capture wlan traffic to hashcat and John the Ripper")
    (description
     "This package contains a small set of tools to capture and convert
packets from wireless devices for use with hashcat or John the Ripper.")
    (license license:expat)))

(define-public hcxdumptool
  (package
    (name "hcxdumptool")
    (version "6.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ZerBea/hcxdumptool")
             (commit version)))
       (sha256
        (base32 "1b4d543y64ib92w9gcmiyjn5hz2vyjqmxk3f3yr1zk04fhw16gmf"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target))
             (string-append "INSTALLDIR=" (assoc-ref %outputs "out") "/bin"))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list openssl))
    (home-page "https://github.com/ZerBea/hcxdumptool")
    (synopsis "Small tool to capture packets from wlan devices")
    (description
     "Small tool to capture packets from WLAN devices.  After capturing,
upload the \"uncleaned\" cap to @url{https://wpa-sec.stanev.org/?submit} to
see if the access point or the client is vulnerable to a dictionary attack.
Convert the cap file to hccapx format and/or to WPA-PMKID-PBKDF2
hashline (16800) with @command{hcxpcaptool} from the @code{hcxtools} package
and check if the WLAN key or the master key was transmitted unencrypted.")
    (license license:expat)))

(define-public dante
  (package
    (name "dante")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.inet.no/dante/files/dante-"
                           version ".tar.gz"))
       (sha256
        (base32 "0pbahkj43rx7rmv2x40mf5p3g3x9d6i2sz7pzglarf54w5ghd2j1"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: The dynamic socks library doesn't work with 'libc.so' (GNU ld
     ;; script).  When preloading is enabled, 'sockd' failed with:
     ;;    … Failed to open library "libc.so": …: invalid ELF header
     '(#:configure-flags '("--disable-preload")))
    (home-page "https://www.inet.no/dante/")
    (synopsis "SOCKS server and client")
    (description "Dante is a SOCKS client and server implementation.  It can
be installed on a machine with access to an external TCP/IP network and will
allow all other machines, without direct access to that network, to be relayed
through the machine the Dante server is running on.  The external network will
never see any machines other than the one Dante is running on.")
    (license (license:non-copyleft "file://LICENSE"))))

(define-public restbed
  (package
    (name "restbed")
    (version "4.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Corvusoft/restbed/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15j09x36i6zj6innl0w1mfzlc56qmjwrs82my8dsagqa2ikd08ya"))))
    (build-system cmake-build-system)
    (inputs
     (list asio catch-framework openssl))
    (arguments
     `(#:configure-flags
       '("-DBUILD_SSL=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'apply-patches-and-fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((asio (assoc-ref inputs "asio"))
                   (catch (assoc-ref inputs "catch"))
                   (openssl (assoc-ref inputs "openssl")))
               (substitute* "cmake/Findasio.cmake"
                 (("(find_path\\( asio_INCLUDE asio\\.hpp HINTS ).*$" all begin)
                  (string-append begin " \"" asio "/include\" )")))
               (substitute* "cmake/Findcatch.cmake"
                 (("(find_path\\( catch_INCLUDE catch\\.hpp HINTS ).*$" all begin)
                  (string-append begin " \"" catch "/include\" )")))
               (substitute* "cmake/Findopenssl.cmake"
                 (("(find_library\\( ssl_LIBRARY ssl ssleay32 HINTS ).*$" all begin)
                  (string-append begin " \"" openssl "/lib\" )"))
                 (("(find_library\\( crypto_LIBRARY crypto libeay32 HINTS ).*$" all begin)
                  (string-append begin " \"" openssl "/lib\" )"))
                 (("(find_path\\( ssl_INCLUDE openssl/ssl\\.h HINTS ).*$" all begin)
                  (string-append begin " \"" openssl "/include\" )")))))))))
    (synopsis "Asynchronous RESTful functionality to C++11 applications")
    (description "Restbed is a comprehensive and consistent programming
model for building applications that require seamless and secure
communication over HTTP.")
    (home-page "https://github.com/Corvusoft/restbed")
    (license license:agpl3+)))

(define-public restinio
  (package
    (name "restinio")
    (version "0.6.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Stiffstream/restinio")
                    (commit (string-append "v." version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j44mglsljwkw49583hcsrl5ck2g56n9srnm10kpbsz2dx5apx98"))))
    (build-system cmake-build-system)
    (inputs                             ; TODO: Need to force-keep references on some inputs, e.g. boost.
     (list zlib
           catch-framework2
           openssl
           boost
           pcre
           pcre2
           sobjectizer))
    (propagated-inputs
     (list asio fmt http-parser))
    (arguments
     `(#:configure-flags '("-DRESTINIO_INSTALL=on")
       #:tests? #f ; TODO: The tests are called from the root CMakelist, need RESTINIO_TEST=on.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "dev/restinio"))))))
    (home-page "https://stiffstream.com/en/products/restinio.html")
    (synopsis "C++14 library that gives you an embedded HTTP/Websocket server")
    (description "RESTinio is a header-only C++14 library that gives you an embedded
HTTP/Websocket server.  It is based on standalone version of ASIO
and targeted primarily for asynchronous processing of HTTP-requests.")
    (license license:bsd-3)))

(define-public opendht
  ;; The version/commit is kept in sync with what Jami uses in its daemon
  ;; contrib build system (see:
  ;; https://review.jami.net/plugins/gitiles/jami-daemon/+/refs/heads/master/contrib/src/opendht/rules.mak).
  (let ((commit "dbbfdaab0f4119abf79646313e0dbc52881dcd56")
        (revision "0"))
    (package
      (name "opendht")
      (version (git-version "2.3.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/savoirfairelinux/opendht")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07x8vw999qpfl6qwj5k5l2mcjy1vp32sd567f6imbsnh9vlx2bdv"))))
      (outputs '("out" "tools" "debug"))
      (build-system gnu-build-system)
      (arguments
       (list
        #:imported-modules `((guix build python-build-system) ;for site-packages
                             ,@%gnu-build-system-modules)
        #:modules '(((guix build python-build-system) #:prefix python:)
                    (guix build gnu-build-system)
                    (guix build utils))
        #:tests? #f                     ;tests require networking
        #:configure-flags
        #~(list "--enable-tests"
                "--enable-proxy-server"
                "--enable-push-notifications"
                "--enable-proxy-server-identity"
                "--enable-proxy-client")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-python-installation-prefix
              ;; Specify the installation prefix for the compiled Python module
              ;; that would otherwise attempt to installs itself to Python's own
              ;; site-packages directory.
              (lambda _
                (substitute* "python/Makefile.am"
                  (("--root=\\$\\(DESTDIR)/")
                   (string-append "--root=/ --single-version-externally-managed "
                                  "--prefix=" #$output)))))
            (add-after 'unpack 'specify-runpath-for-python-module
              (lambda _
                (substitute* "python/setup.py.in"
                  (("extra_link_args=\\[(.*)\\]" _ args)
                   (string-append "extra_link_args=[" args
                                  ", '-Wl,-rpath=" #$output "/lib']")))))
            (add-after 'install 'move-and-wrap-tools
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((tools (assoc-ref outputs "tools"))
                       (dhtcluster (string-append tools "/bin/dhtcluster"))
                       (site-packages (python:site-packages inputs outputs)))
                  (mkdir tools)
                  (rename-file (string-append #$output "/bin")
                               (string-append tools "/bin"))
                  ;; TODO: Contribute a patch to python/Makefile.am to
                  ;; automate this.
                  (copy-file "python/tools/dhtcluster.py" dhtcluster)
                  (chmod dhtcluster #o555)
                  (wrap-program dhtcluster
                    `("GUIX_PYTHONPATH" prefix (,site-packages)))))))))
      (inputs (list bash-minimal fmt readline))
      (propagated-inputs
       (list msgpack                    ;included in several installed headers
             restinio                   ;included in opendht/http.h
             ;; The following are listed in the 'Requires.private' field of
             ;; opendht.pc:
             argon2
             gnutls
             jsoncpp
             nettle
             openssl))                  ;required for the DHT proxy
      (native-inputs
       (list autoconf
             automake
             pkg-config
             python
             python-cython
             libtool
             cppunit))
      (home-page "https://github.com/savoirfairelinux/opendht/")
      (synopsis "Lightweight Distributed Hash Table (DHT) library")
      (description "OpenDHT provides an easy to use distributed in-memory data
store.  Every node in the network can read and write values to the store.
Values are distributed over the network, with redundancy.  It includes the
following features:
@itemize
@item Lightweight and scalable, designed for large networks and small devices;
@item High resilience to network disruption;
@item Public key cryptography layer providing optional data signature and
encryption (using GnuTLS);
@item IPv4 and IPv6 support;
@item Clean and powerful C++14 map API;
@item Bindings for C, Rust & Python 3;
@item REST API with an optional HTTP client and server with push notification
support.
@end itemize
The following tools are also included:
@table @command
@item dhtnode
A command line tool to run a DHT node and perform operations supported by the
library (get, put, etc.) with text values.
@item dhtchat
A very simple IM client working over the DHT.
@end table")
      (license license:gpl3+))))

(define-public frrouting
  (package
    (name "frrouting")
    (version "7.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FRRouting/frr/releases/"
                                  "download/frr-" version "/frr-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1r7gh5h27ii7d1d0z0x48wx7hs8vvympv3gqvy3cwzg05q5vk9xs"))))
    (build-system gnu-build-system)
    (inputs
     (list c-ares json-c libcap libyang readline))
    (native-inputs
     (list perl pkg-config python-wrapper python-pytest))
    (home-page "https://frrouting.org/")
    (synopsis "IP routing protocol suite")
    (description "FRRouting (FRR) is an IP routing protocol suite which includes
protocol daemons for BGP, IS-IS, LDP, OSPF, PIM, and RIP.")
    (license license:gpl2+)))

(define-public bird
  (package
    (name "bird")
    (version "2.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://bird.network.cz/pub/bird/bird-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1xp7f0im1v8pqqx3xqyfkd1nsxk8vnbqgrdrwnwhg8r5xs1xxlhr"))))
    (inputs
     (list libssh readline))
    (native-inputs
     (list bison flex))
    (arguments
     `(#:configure-flags '("--localstatedir=/var" "--enable-ipv6")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'dont-create-sysconfdir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile.in"
               ((" \\$\\(DESTDIR)/\\$\\(runstatedir)") "")))))))
    (build-system gnu-build-system)
    (home-page "http://bird.network.cz")
    (synopsis "Internet Routing Daemon")
    (description "BIRD is an Internet routing daemon with full support for all
the major routing protocols.  It allows redistribution between protocols with a
powerful route filtering syntax and an easy-to-use configuration interface.")
    (license license:gpl2+)))

(define-public iwd
  (package
    (name "iwd")
    (version "1.20")
    (source (origin
              ;; FIXME: We're using the bootstrapped sources because
              ;; otherwise using an external ell library is impossible.
              ;; How to bootstrap with Guix?
              (method url-fetch)
              (uri (string-append "https://www.kernel.org/pub/linux/network"
                                  "/wireless/iwd-" version ".tar.xz"))
              (sha256
               (base32
                "03q5scahyg86h4bdxqxm32shyssgpmfp5b3183j01ig7mg6f4lbx"))))
    (build-system gnu-build-system)
    (inputs
     (list dbus ell readline))
    (native-inputs
     `(("pkgconfig" ,pkg-config)
       ("python" ,python)
       ("rst2man" ,python-docutils)
       ("openssl" ,openssl)))
    (arguments
     `(#:configure-flags
       (let ((dbus (assoc-ref %outputs "out")))
         (list "--disable-systemd-service"
               "--enable-external-ell"
               "--enable-hwsim"
               "--enable-tools"
               "--enable-wired"
               "--localstatedir=/var"
               (string-append "--with-dbus-datadir=" dbus "/share/")
               (string-append "--with-dbus-busdir="
                              dbus "/share/dbus-1/system-services")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-Makefile
           (lambda _
             (substitute* "Makefile"
               ;; Don't try to 'mkdir /var'.
               (("\\$\\(MKDIR_P\\) -m 700") "true")))))))
    (home-page "https://git.kernel.org/pub/scm/network/wireless/iwd.git/")
    (synopsis "Internet Wireless Daemon")
    (description "iwd is a wireless daemon for Linux that aims to replace WPA
Supplicant.  It optimizes resource utilization by not depending on any external
libraries and instead utilizing features provided by the Linux kernel to the
maximum extent possible.")
    (license license:lgpl2.1+)))

(define-public libyang
  (package
    (name "libyang")
    (version "1.0.215")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/CESNET/libyang")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mrs2ppmq77z8sbqgm2w0rl9bfgybd6bcxanakfww4chih6cy0dw"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DENABLE_BUILD_TESTS=ON" "-DENABLE_LYD_PRIV=ON")))
    (propagated-inputs (list pcre))
    (native-inputs (list cmocka pkg-config))
    (home-page "https://github.com/CESNET/libyang")
    (synopsis "YANG data modelling language library")
    (description "libyang is a YANG data modelling language parser and toolkit
written (and providing API) in C.  Current implementation covers YANG 1.0 (RFC
6020) as well as YANG 1.1 (RFC 7950).")
    (license license:bsd-3)))

(define-public batctl
  (package
   (name "batctl")
   (version "2021.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://downloads.open-mesh.org/batman/releases/batman-adv-"
                         version "/batctl-" version ".tar.gz"))
     (sha256
      (base32 "1ryqz90av2p5pgmmpi1afmycd18zhpwz1i4f7r0s359jis86xndn"))))
   (inputs
    (list libnl))
   (native-inputs
    (list pkg-config))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      ;; Batctl only has a makefile. Thus we disable tests and
      ;; configuration, passing in a few make-flags.
      #:phases (modify-phases %standard-phases (delete 'configure))
      #:make-flags
      (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
            (string-append "PKG_CONFIG="
                           (search-input-file %build-inputs
                                              "/bin/pkg-config"))
            ,(string-append "CC=" (cc-for-target)))))
   (home-page "https://www.open-mesh.org/projects/batman-adv/wiki/Wiki")
   (synopsis "Management tool for the mesh networking BATMAN protocol")
   (description "This package provides a control tool for the
B.A.T.M.A.N. mesh networking routing protocol provided by the Linux kernel
module @code{batman-adv}, for Layer 2.")
   (license license:gpl2+)))

(define-public pagekite
  (package
    (name "pagekite")
    (version "1.5.2.200725")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pagekite/PyPagekite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lig1i42bn9isw848vnml5qhcaa04x1dr2hb075bm0a3439kv3rr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-man-page
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (invoke "make" "doc/pagekite.1")
               (install-file "doc/pagekite.1" (string-append man "/man1"))))))))
    (inputs
     (list python-six python-socksipychain))
    (home-page "https://pagekite.net/")
    (synopsis "Make localhost servers publicly visible")
    (description
     "PageKite implements a tunneled reverse proxy which makes it easy to make
a service (such as an HTTP or SSH server) on localhost visible to the wider
Internet, even behind NAT or restrictive firewalls.  A managed front-end relay
service is available at @url{https://pagekite.net/}, or you can run your own.")
    (license license:agpl3+)))

(define-public ipcalc
  (package
    (name "ipcalc")
    (version "0.41")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://jodies.de/ipcalc-archive/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "12if9sm8h2ac0pgwkw835cgyqjxm6h27k4kfn2vfas9krrqwbafx"))))
    (inputs `(("perl" ,perl)
              ("tar" ,tar)
              ("gzip" ,gzip)
              ("tarball" ,source)))
    (build-system trivial-build-system) ;no Makefile.PL
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (use-modules (srfi srfi-1))
         (let* ((source (assoc-ref %build-inputs "source"))
                (perl (string-append (assoc-ref %build-inputs "perl")
                                     "/bin"))
                (tar (assoc-ref %build-inputs "tar"))
                (gz  (assoc-ref %build-inputs "gzip"))
                (out (assoc-ref %outputs "out"))
                (bin (string-append out "/bin"))
                (doc (string-append out "/share/doc/ipcalc")))
           (setenv "PATH" (string-append gz "/bin"))
           (invoke (string-append tar "/bin/tar") "xvf" source)
           (chdir (string-append ,name "-" ,version))

           (install-file "ipcalc" bin)
           (patch-shebang (string-append bin "/ipcalc") (list perl))
           #t))))
    (synopsis "Simple IP network calculator")
    (description "ipcalc takes an IP address and netmask and calculates the
resulting broadcast, network, Cisco wildcard mask, and host range.  By giving
a second netmask, you can design subnets and supernets.  It is also intended
to be a teaching tool and presents the subnetting results as
easy-to-understand binary values.")
    (home-page "http://jodies.de/ipcalc")
    (license license:gpl2+)))

(define-public tunctl
  (package
    (name "tunctl")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tunctl/tunctl/" version "/"
                           "tunctl-" version ".tar.gz"))
       (sha256
        (base32 "1zsgn7w6l2zh2q0j6qaw8wsx981qcr536qlz1lgb3b5zqr66qama"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)            ;there is no configure.ac file
         (delete 'configure)            ;there is no configure script
         (delete 'check)                ;there are no tests
         (replace 'build
           (lambda _
             (setenv "CC" "gcc")
             (invoke "make" "tunctl")))
         ;; TODO: Requires docbook2x to generate man page from SGML.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "tunctl" bin))
             #t)))))
    (home-page "http://tunctl.sourceforge.net")
    (synopsis  "Utility to set up and maintain TUN/TAP network interfaces")
    (description "Tunctl is used to set up and maintain persistent TUN/TAP
network interfaces, enabling user applications to simulate network traffic.
Such interfaces are useful for VPN software, virtualization, emulation,
simulation, and a number of other applications.")
    (license license:gpl2)))

(define-public wol
  (package
    (name "wol")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/wake-on-lan/wol/"
                           version "/wol-" version ".tar.gz"))
       (sha256
        (base32 "08i6l5lr14mh4n3qbmx6kyx7vjqvzdnh3j9yfvgjppqik2dnq270"))))
    (build-system gnu-build-system)
    (home-page "https://sourceforge.net/projects/wake-on-lan/")
    (synopsis "Implements Wake On LAN functionality in a small program")
    (description "Tool to send a magic packet to wake another host on the
network.  This must be enabled on the target host, usually in the BIOS.")
    (license license:gpl2)))

(define-public traceroute
  (package
    (name "traceroute")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/traceroute/traceroute/"
                           "traceroute-" version "/traceroute-"
                           version ".tar.gz"))
       (sha256
        (base32 "1dh32vcfawkl1p9g4ral1p0camds4paqr8db1kaqxwyk6hmd4s9n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test suite
       #:make-flags
       (list (string-append "LIBRARY_PATH="
                            (assoc-ref %build-inputs "libc") "/lib")
             (string-append "CFLAGS=-I"
                            (assoc-ref %build-inputs "kernel-headers")
                            "/include")
             "LDFLAGS=-lm -L../libsupp"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-make
           (lambda _
             (substitute* "default.rules"
               ((" \\$\\(LIBDEPS\\)") "$(filter-out -l%,$(LIBDEPS))"))))
         (delete 'bootstrap)            ;no configure.ac file
         (delete 'configure))))         ;no configure script
    (home-page "http://traceroute.sourceforge.net/")
    (synopsis "Tracks the route taken by packets over an IP network")
    (description "This package provides a modern, but Linux-specific
implementation of the @command{traceroute} command that can be used to follow
the route taken by packets on an IP network on their way to a given host.  It
utilizes the IP protocol's time to live (TTL) field and attempts to elicit an
ICMP TIME_EXCEEDED response from each gateway along the path to the host.
Compared to other implementations, this @command{traceroute} command allows
some traces for unprivileged users.")
    (license (list license:gpl2+
                   license:lgpl2.1+)))) ;for the libsupp subdirectory

(define-public vde2
  (let ((commit "8c65ebc464b2f986d5f1f4e6ae829ef4480c9d5a")
        (revision "0"))
  (package
    (name "vde2")
    (version (git-version "2.3.2" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/virtualsquare/vde-2")
              (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l5xf71sv9zm5zw0wg8xgip58c0wh8zck2bazyc2a8gb67gc3s8y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f))           ; Build fails if #t.
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list python libpcap wolfssl))
    (home-page "https://github.com/virtualsquare/vde-2")
    (synopsis "Virtual Distributed Ethernet")
    (description "VDE is a set of programs to provide virtual software-defined
Ethernet network interface controllers across multiple virtual or
physical, local or remote devices.  The VDE architecture provides
virtual counterparts to hardware components such as switches and
cables.")
    (license (list license:gpl2
                   license:lgpl2.1       ; libvdeplug
                   (license:non-copyleft ; slirpvde
                    "file://COPYING.slirpvde"
                    "See COPYING.slirpvde in the distribution."))))))

(define-public haproxy
  (package
    (name "haproxy")
    (version "2.1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.haproxy.org/download/"
                                  (version-major+minor version)
                                  "/src/haproxy-" version ".tar.gz"))
              (sha256
               (base32
                "0fd3c1znid5a9w3gcf77b85hm2a2558w9s02c4b7xzkmivqnqbir"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let* ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)
               (string-append "DOCDIR=" out "/share/" ,name)
               "TARGET=linux-glibc"
               "USE_LUA=1"
               "USE_OPENSSL=1"
               "USE_ZLIB=1"
               "USE_PCRE_2=1"))
       #:tests? #f  ; there are only regression tests, using varnishtest
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list lua openssl pcre2 zlib))
    (home-page "https://www.haproxy.org/")
    (synopsis "Reliable, high performance TCP/HTTP load balancer")
    (description "HAProxy is a free, very fast and reliable solution offering
high availability, load balancing, and proxying for TCP and HTTP-based
applications.  It is particularly suited for web sites crawling under very
high loads while needing persistence or Layer7 processing.  Supporting tens of
thousands of connections is clearly realistic with today's hardware.")
    (license (list license:gpl2+
                   license:lgpl2.1
                   license:lgpl2.1+))))

(define-public lldpd
  (package
    (name "lldpd")
    (version "1.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://media.luffy.cx/files/lldpd/lldpd-"
                           version ".tar.gz"))
       (sha256
        (base32 "00a7v24qhxw80yk2v79wrkfn7br4r8pcajyrpz8j0xx2v1zq4ffn"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Drop bundled library.
           (delete-file-recursively "libevent")
           #t))))
    (arguments
     `(#:configure-flags
       (list
        "--with-privsep-user=nobody"
        "--with-privsep-group=nogroup"
        "--localstatedir=/var"
        "--enable-pie"
        "--disable-static"
        "--without-embedded-libevent"
        (string-append "--with-systemdsystemunitdir="
                       (assoc-ref %outputs "out")
                       "/lib/systemd/system"))))
    (build-system gnu-build-system)
    (inputs
     (list libevent libxml2 openssl readline))
    (native-inputs
     (list pkg-config))
    (home-page "https://vincentbernat.github.io/lldpd/")
    (synopsis "Locate neighbors of your network equipment")
    (description
     "The @dfn{Link Layer Discovery Protocol} (LLDP) is an industry standard
protocol designed to supplant proprietary Link-Layer protocols such as EDP or
CDP.  The goal of LLDP is to provide an inter-vendor compatible mechanism to
deliver Link-Layer notifications to adjacent network devices.  @code{lldpd} is
an implementation of LLDP.  It also supports some proprietary protocols.")
    (license license:isc)))

(define-public hashcash
  (package
    (name "hashcash")
    (version "1.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.hashcash.org/source/hashcash-"
                           version ".tgz"))
       (sha256
        (base32
         "15kqaimwb2y8wvzpn73021bvay9mz1gqqfc40gk4hj6f84nz34h1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; No tests available.
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((outdir (assoc-ref outputs "out"))
                    (bindir (string-append outdir "/bin"))
                    (mandir (string-append outdir "/share/man/man1"))
                    (docdir (string-append outdir "/share/doc/hashcash-" ,version)))
               ;; Install manually, as we don't need the `sha1' binary
               (install-file "hashcash" bindir)
               (install-file "hashcash.1" mandir)
               (install-file "README" docdir)
               (install-file "LICENSE" docdir)
               (install-file "CHANGELOG" docdir)
               #t))))))
    (home-page "https://www.hashcash.org/")
    (synopsis "Denial-of-service countermeasure")
    (description "Hashcash is a proof-of-work algorithm, which has been used
as a denial-of-service countermeasure technique in a number of systems.

A hashcash stamp constitutes a proof-of-work which takes a parametrizable
amount of work to compute for the sender.  The recipient can verify received
hashcash stamps efficiently.

This package contains a command-line tool for computing and verifying hashcash
stamps.")
    (license license:public-domain)))

(define-public nbd
  (package
    (name "nbd")
    (version "3.23")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/nbd/nbd/" version
                            "/nbd-" version ".tar.xz"))
        (sha256
         (base32 "1d2phi0m9x32p9zddv9fpkhj1rbhlvq93wsn9niy7i3aavn71x6y"))))
    (build-system gnu-build-system)
    (inputs
     (list glib))
    (native-inputs
     (list bison pkg-config which))
    (home-page "https://nbd.sourceforge.io/")
    (synopsis "NBD client and server")
    (description "This package provides the NBD (Network Block Devices)
client and server.  It allows you to use remote block devices over a TCP/IP
network.")
    (license license:gpl2)))

(define-public yggdrasil
  (package
    (name "yggdrasil")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/yggdrasil-network/yggdrasil-go")
         (commit (string-append "v" version))
         (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ajhn0z58ap9jldip7mqj78drmgg4645zfsxsy004cfkm60fasnx"))
       (patches (search-patches "yggdrasil-extra-config.patch"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/yggdrasil-network/yggdrasil-go"
       ;; TODO: figure out how tests are run
       #:tests? #f
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path directory))
               (list "github.com/yggdrasil-network/yggdrasil-go/cmd/yggdrasil"
                     "github.com/yggdrasil-network/yggdrasil-go/cmd/yggdrasilctl"
                     "github.com/yggdrasil-network/yggdrasil-go/cmd/genkeys"))
             #t)))))
    ;; https://github.com/kardianos/minwinsvc is windows only
    (propagated-inputs
     (list ;;("go-golang-zx2c4-com-wireguard-windows"
           ;; ,go-golang-zx2c4-com-wireguard-windows)
           go-golang-zx2c4-com-wireguard
           go-golang-org-x-text
           go-golang-org-x-sys
           go-golang-org-x-net
           go-golang-org-x-crypto
           go-netns
           go-netlink
           go-github-com-mitchellh-mapstructure
           go-github-com-mattn-go-runewidth
           go-github-com-mattn-go-isatty
           go-github-com-kardianos-minwinsvc
           go-github-com-hjson-hjson-go
           go-github-com-hashicorp-go-syslog
           go-github-com-gologme-log
           go-github-com-fatih-color
           go-github-com-cheggaaa-pb-v3
           go-github-com-vividcortex-ewma
           go-github-com-arceliar-phony
           go-github-com-arceliar-ironwood))
    (home-page "https://yggdrasil-network.github.io/blog.html")
    (synopsis
     "Experiment in scalable routing as an encrypted IPv6 overlay network")
    (description
     "Yggdrasil is an early-stage implementation of a fully end-to-end encrypted
IPv6 network.  It is lightweight, self-arranging, supported on multiple
platforms and allows pretty much any IPv6-capable application to communicate
securely with other Yggdrasil nodes.  Yggdrasil does not require you to have
IPv6 Internet connectivity - it also works over IPv4.")
    (license
     ;; As a special exception to the GNU Lesser General Public License
     ;; version 3 ("LGPL3"), the copyright holders of this Library give you
     ;; permission to convey to a third party a Combined Work that links
     ;; statically or dynamically to this Library without providing any Minimal
     ;; Corresponding Source or Minimal Application Code as set out in 4d or
     ;; providing the installation information set out in section 4e, provided
     ;; that you comply with the other provisions of LGPL3 and provided that you
     ;; meet, for the Application the terms and conditions of the license(s)
     ;; which apply to the Application. Except as stated in this special
     ;; exception, the provisions of LGPL3 will continue to comply in full to
     ;; this Library. If you modify this Library, you may apply this exception
     ;; to your version of this Library, but you are not obliged to do so. If
     ;; you do not wish to do so, delete this exception statement from your
     ;; version. This exception does not (and cannot) modify any license terms
     ;; which apply to the Application, with which you must still comply
     license:lgpl3)))

(define-public netdiscover
  (package
   (name "netdiscover")
   (version "0.7")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/netdiscover-scanner/netdiscover")
            (commit version)))
      (sha256
       (base32 "0g8w8rlg16dsibxi4dnyn7v7r8wwi5ypd51c4w59j0ps2id0w8yj"))
      (file-name (string-append "netdiscover-" version))))
   (arguments
    `(#:tests? #f)) ;; no tests
   (build-system gnu-build-system)
   (inputs
    (list libnet libpcap))
   (native-inputs
    (list autoconf automake))
   (synopsis "Network address discovery tool")
   (description "Netdiscover is a network address discovery tool developed
mainly for wireless networks without a @acronym{DHCP} server.  It also works
on hub/switched networks.  It is based on @acronym{ARP} packets, it will send
@acronym{ARP} requests and sniff for replies.")
   (home-page "https://github.com/netdiscover-scanner/netdiscover")
   (license license:gpl3+)))

(define-public putty
  (package
    (name "putty")
    (version "0.76")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://the.earth.li/~sgtatham/putty/"
                                 version "/putty-" version ".tar.gz")
                  (string-append "http://www.putty.be/" version
                                 "/putty-" version ".tar.gz")))
       (sha256
        (base32 "0gvi8phabszqksj2by5jrjmshm7bpirhgavz0dqyz1xaimxdjz2l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "unix"))))))
    (inputs
     (list gtk+))
    (native-inputs
     (list pkg-config python)) ; for tests
    (synopsis "Graphical @acronym{SSH, Secure SHell} and telnet client")
    (description "PuTTY is a graphical text terminal client.  It supports
@acronym{SSH, Secure SHell}, telnet, and raw socket connections with good
terminal emulation.  It can authenticate with public keys and Kerberos
single-sign-on.  It also includes command-line @acronym{SFTP, Secure File
Transfer Protocol} and older @acronym{SCP, Secure Copy Protocol}
implementations.")
    (home-page "https://www.chiark.greenend.org.uk/~sgtatham/putty/")
    (license license:expat)))
