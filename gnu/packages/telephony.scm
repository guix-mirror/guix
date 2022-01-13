;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2016, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 Francesco Frassinelli <fraph24@gmail.com>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Adonay Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@hyperbola.info>
;;; Copyright © 2018 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2019 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 LibreMiami <packaging-guix@libremiami.org>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Demis Balbach <db@minikn.xyz>
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

(define-module (gnu packages telephony)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linphone)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system qt))

(define-public phonesim
  (package
    (name "phonesim")
    (version "1.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/network/ofono/phonesim")
             (commit "a7c844d45b047b2dae5b0877816c346fce4c47b9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rc1c2vr03dmi1dr3skj57v77ga9c22g29xs1qiphqms4isby9cq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-maintainer-mode"
             "CC=" ,(cc-for-target))))
    (native-inputs
     (list automake autoconf pkg-config))
    (inputs
     (list qtbase-5 qtdeclarative))
    (synopsis "Phone Simulator for modem testing")
    (description
     "Phonesim is a modem emulator that oFono uses for development and
testing.  This allows oFono to be used by any host without requiring special
GSM (or other) hardware.")
    (home-page "https://git.kernel.org/pub/scm/network/ofono/phonesim")
    (license license:gpl2+)))

(define-public libilbc
  (package
    (name "libilbc")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/TimothyGu/libilbc")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32
         "1j1pn1w1198qvdiq2hgv9hkyq2nqcvmfnwlgppac633idkjnjrqx"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; No target
    (native-inputs
     (list pkg-config))
    (synopsis "Libre iLBC codec")
    (description "LibiLBC is a packaging friendly copy of the iLBC codec from
the WebRTC project.  It provides a base for distribution packages and can be
used as drop-in replacement for the non-free code from RFC 3591.")
    (home-page "https://github.com/TimothyGu/libilbc")
    (license license:bsd-3)))

(define-public spandsp
  (package
    (name "spandsp")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri
        ;; The original upstream has been down since the end of March 2020.
        (string-append "https://web.archive.org/web/20180626203108/"
                       "https://www.soft-switch.org/downloads/" name "/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32 "0rclrkyspzk575v8fslzjpgp4y2s4x7xk3r55ycvpi4agv33l1fc"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc" "static"))   ;doc contains HTML documentation
    (arguments
     `(#:configure-flags '("--enable-doc=yes" "--enable-tests=yes")
       #:parallel-build? #f ;non-deterministic build failures may occur otherwise
       #:parallel-tests? #f ;fails removing the same the files twice otherwise
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-configure.ac
                    (lambda _
                      ;; spandsp looks at hard coded locations of the FHS to
                      ;; find libxml2.
                      (substitute* "configure.ac"
                        (("AC_MSG_CHECKING\\(for libxml/xmlmemory\\.h.*" all)
                         (string-append all
                                        "PKG_CHECK_MODULES(XML2, libxml-2.0)\n"
                                        "CPPFLAGS+=\" $XML2_CFLAGS\"\n")))
                      ;; Force a regeneration of the autotools build system.
                      (delete-file "autogen.sh")
                      (delete-file "configure")
                      #t))
                  (add-after 'unpack 'do-not-install-data-files
                    ;; The .tiff images produced for tests are not
                    ;; reproducible and it is not desirable to have those
                    ;; distributed.
                    (lambda _
                      (substitute* '("test-data/itu/fax/Makefile.am"
                                     "test-data/etsi/fax/Makefile.am")
                        (("nobase_data_DATA")
                         "noinst_DATA"))
                      #t))
                  (add-after 'install 'install-doc
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((doc (string-append (assoc-ref outputs "doc")
                                                "/share/doc/" ,name "-" ,version)))
                        (copy-recursively "doc/t38_manual" doc)
                        #t)))
                  (add-after 'install 'move-static-libraries
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (static (assoc-ref outputs "static")))
                        (mkdir-p (string-append static "/lib"))
                        (with-directory-excursion out
                          (for-each (lambda (file)
                                      (rename-file file
                                                   (string-append static "/"
                                                                  file)))
                                    (find-files "lib" "\\.a$")))
                        #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ;; For the tests
       ("fftw" ,fftw)
       ("libpcap" ,libpcap)
       ("libsndfile" ,libsndfile)
       ("libjpeg" ,libjpeg-turbo)      ;XXX: should be propagated from libtiff
       ("libtiff" ,libtiff)
       ("netpbm" ,netpbm)
       ("sox" ,sox)
       ;; For the documentation
       ("docbook-xml" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
       ("doxygen" ,doxygen)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)))
    (synopsis "DSP library for telephony")
    (description "SpanDSP is a library of DSP functions for telephony, in the
8000 sample per second world of E1s, T1s, and higher order PCM channels.  It
contains low level functions, such as basic filters.  It also contains higher
level functions, such as cadenced supervisory tone detection, and a complete
software FAX machine.")
    (home-page "https://web.archive.org/web/20180626203108/\
https://www.soft-switch.org/index.html")
    (license (list license:lgpl2.1+  ;for the library
                   license:gpl2+)))) ;for the test suites and support programs

(define-public commoncpp
  (package
   (name "commoncpp")
   (version "1.8.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/" name "/commoncpp2-"
                   version ".tar.gz"))
            (sha256 (base32
                     "0kmgr5w3b1qwzxnsnw94q6rqs0hr8nbv9clf07ca2a2fyypx9kjk"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda _
             (substitute* "src/applog.cpp"
               (("^// TODO sc.*") "#include <sys/types.h>\n#include <sys/stat.h>\n"))
             #t)))))
   (build-system gnu-build-system)
   (synopsis "(u)Common C++ framework for threaded applications")
   (description "GNU Common C++ is an portable, optimized class framework for
threaded applications, supporting concurrent synchronization, inter-process
communications via sockets, and various methods for data handling, such as
serialization and XML parsing.  It includes the uCommon C++ library, a smaller
reimplementation.")
   (license license:gpl2+) ; plus runtime exception
   (properties '((ftp-directory . "/gnu/commoncpp")
                 (upstream-name . "commoncpp2")))
   (home-page "https://www.gnu.org/software/commoncpp/")))

(define-public ucommon
  (package
   (name "ucommon")
   (version "7.0.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/commoncpp/" name "-"
                   version ".tar.gz"))
            (sha256 (base32
                     "1mv080rvrhyxyhgqiqr8r9jdqhg3xhfawjvfj5zgj47h59nggjba"))))
   (build-system gnu-build-system)
   (inputs (list gnutls))
   (synopsis "Common C++ framework for threaded applications")
   (description "GNU uCommon C++ is meant as a very light-weight C++ library
to facilitate using C++ design patterns even for very deeply embedded
applications, such as for systems using uclibc along with posix threading
support.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/commoncpp/")
   (properties '((ftp-directory . "/gnu/commoncpp")))))

(define-public ccrtp
  (package
   (name "ccrtp")
   (version "2.1.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/ccrtp/ccrtp-"
                   version ".tar.gz"))
            (sha256 (base32
                     "17ili8l7zqbbkzr1rcy4hlnazkf50mds41wg6n7bfdsx3c7cldgh"))))
   (build-system gnu-build-system)
   (inputs (list ucommon libgcrypt))
   (native-inputs (list pkg-config))
   (synopsis "Implementation of RTP (real-time transport protocol)")
   (description  "GNU ccRTP is an implementation of RTP, the real-time transport
protocol from the IETF.  It is suitable both for high capacity servers and
personal client applications.  It is flexible in its design, allowing it to
function as a framework for the framework, rather than just being a
packet-manipulation library.")
   (license license:gpl2+) ; plus runtime exception
   (home-page "https://www.gnu.org/software/ccrtp/")))

(define-public zrtpcpp
  (package
    (name "zrtpcpp")
    (version "4.6.6")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/wernerd/ZRTPCPP")
         (commit
          (string-append "V" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32
         "06vphvh4dgi7ah5qkq53wqvswv8l273x0xwbc447qmgvamm0x1vs"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; No target
    (native-inputs
     (list pkg-config))
    (inputs
     (list ccrtp ucommon))
    (synopsis "C++ Implementation of ZRTP protocol")
    (description  "GNU ZRTP C++ provides a library that adds ZRTP support to the
GNU ccRTP stack and serves as library for other RTP stacks
(PJSIP, GStreamer).")
    (home-page "https://www.gnu.org/software/ccrtp/zrtp")
    (license license:lgpl3+)))

(define-public osip
  (package
   (name "osip")
   (version "5.2.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/osip/libosip2-" version ".tar.gz"))
            (patches (search-patches "osip-CVE-2017-7853.patch"))
            (sha256
             (base32
              "1wibs2zs035ay7qvl5ai8drv6f0xw7iscb0frmpgax3pisy88dzf"))))
   (build-system gnu-build-system)

   (synopsis "Library implementing SIP (RFC-3261)")
   (description "GNU oSIP is an implementation of the SIP protocol.  It is
used to provide multimedia and telecom software developers with an interface
to initiate and control SIP sessions.")
   (license license:lgpl2.1+)
   (properties '((ftp-directory . "/gnu/osip")
                 (upstream-name . "libosip2")))
   (home-page "https://www.gnu.org/software/osip/")))


(define-public exosip
  (package
   (name "exosip")
   (version "4.1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://savannah/exosip/libeXosip2-"
                                version ".tar.gz"))
            (sha256 (base32
                     "17cna8kpc8nk1si419vgr6r42k2lda0rdk50vlxrw8rzg0xp2xrw"))))
   (build-system gnu-build-system)
   (inputs (list osip))
   (synopsis "Sip abstraction library")
   (description "EXosip is a library that hides the complexity of using the
SIP protocol for multimedia session establishment.  This protocol is mainly to
be used by VoIP telephony applications (endpoints or conference server) but
might be also useful for any application that wish to establish sessions like
multiplayer games.")
   (license license:gpl2+)
   ;; (plus OpenSSL linking exception)
   ;; http://git.savannah.gnu.org/cgit/exosip.git/plain/LICENSE.OpenSSL
    (home-page "https://savannah.nongnu.org/projects/exosip")))

(define-public sipwitch
  (package
   (name "sipwitch")
   (version "1.9.15")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/sipwitch/sipwitch-"
                   version ".tar.gz"))
            (sha256 (base32
                     "10lli9c703d7qbarzc0lgmz963ppncvnrklwrnri0s1zcmmahyia"))))
   (build-system gnu-build-system)
   ;; The configure.ac uses pkg-config but in a kludgy way which breaks when
   ;; cross-compiling.  Among other issues there the program name "pkg-config"
   ;; is hard coded instead of respecting the PKG_CONFIG environment variable.
   ;; Fortunately we can avoid the use of pkg-config and set the dependency
   ;; flags ourselves.
   (arguments `(#:configure-flags
                `("--without-pkg-config"
                  ,(string-append "UCOMMON_CFLAGS=-I"
                                  (assoc-ref %build-inputs "ucommon") "/include")
                  "UCOMMON_LIBS=-lusecure -lucommon -lrt -ldl -lpthread"
                  ,(string-append "LIBOSIP2_CFLAGS=-I"
                                  (assoc-ref %build-inputs "osip") "/include")
                  "LIBOSIP2_LIBS=-losipparser2 -losip2"
                  ,(string-append "--sysconfdir=" (assoc-ref %outputs "out")
                                  "/etc")
                  "EXOSIP2_LIBS=-leXosip2"
                  ,(string-append "EXOSIP2_CFLAGS=-I"
                                  (assoc-ref %build-inputs "exosip")
                                  "/include"))))
   (inputs (list ucommon exosip osip))
   (synopsis "Secure peer-to-peer VoIP server for the SIP protocol")
   (description "GNU SIP Witch is a peer-to-peer Voice-over-IP server that
uses the SIP protocol.  Calls can be made from behind NAT firewalls and
without the need for a service provider.  Its peer-to-peer design ensures that
there is no central point for media intercept or capture and thus it can be
used to construct a secure telephone system that operates over the public
internet.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/sipwitch/")))

(define-public libsrtp
  (package
    (name "libsrtp")
    (version "2.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cisco/libsrtp")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gswpjm4jacfxmgglbf8hxi3yzsag4drk4q943p0wkmv21zj8l78"))))
    (native-inputs
     (list psmisc ;some tests require 'killall'
           procps))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "runtest"
       #:phases (modify-phases %standard-phases
                  (add-after 'build 'build-shared
                    (lambda* (#:key (make-flags '()) #:allow-other-keys)
                      ;; Build the shared library separately because
                      ;; the test runner requires a static build.
                      (apply invoke "make" "shared_library" make-flags)
                      #t))
                  (add-after 'install 'remove-static-library
                    (lambda* (#:key outputs #:allow-other-keys)
                      (delete-file (string-append (assoc-ref outputs "out")
                                                  "/lib/libsrtp2.a"))
                      #t)))))
    (synopsis "Secure RTP (SRTP) Reference Implementation")
    (description
     "This package provides an implementation of the Secure Real-time Transport
Protocol (@dfn{SRTP}), the Universal Security Transform (@dfn{UST}), and a
supporting cryptographic kernel.")
    (home-page "https://github.com/cisco/libsrtp")
    (license license:bsd-3)))

(define-public libiax2
  (let ((commit "0e5980f1d78ce462e2d1ed6bc39ff35c8341f201"))
    ;; This is the commit used by the Ring Project.
    (package
      (name "libiax2")
      (version (string-append "0.0.0-1." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.savoirfairelinux.com/sflphone/libiax2.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0d269474kk1933c55hx4azw3sak5ycfrxkw6ida0sb2cm00kfich"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake libtool))
      (home-page "https://gitlab.savoirfairelinux.com/sflphone/libiax2")
      (synopsis "Inter-Asterisk-Protocol library")
      (description "LibIAX2 implements the Inter-Asterisk-Protocol for relaying
Voice-over-IP (VoIP) communications.")
      ;; The file 'src/md5.c' is released into the public domain by RSA Data
      ;; Security.  The files 'src/answer.h', 'src/miniphone.c',
      ;; 'src/options.c', 'src/options.h', 'src/ring10.h', 'src/winiphone.c' are
      ;; covered under the 'GPL'.
      ;; The package as a whole is distributed under the LGPL 2.0.
      (license (list license:lgpl2.0
                     license:public-domain
                     license:gpl2+)))))

(define-public seren
  (package
    (name "seren")
    (version "0.0.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://holdenc.altervista.org/"
                                  "seren/downloads/seren-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "06mams6bng7ib7p2zpfq88kdr4ffril9svzc9lprkb0wjgmkglk9"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))  ; no "check" target
    (inputs
     (list alsa-lib gmp libogg ncurses opus))
    (synopsis "Simple VoIP program to create conferences from the terminal")
    (description
     "Seren is a simple VoIP program based on the Opus codec that allows you
to create a voice conference from the terminal, with up to 10 participants,
without having to register accounts, exchange emails, or add people to contact
lists.  All you need to join an existing conference is the host name or IP
address of one of the participants.")
    (home-page "http://holdenc.altervista.org/seren/")
    (license license:gpl3+)))

(define-public mumble
  (package
    (name "mumble")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/mumble-voip/mumble/releases/download/"
                version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "14v0rgy1a5alxmz7ly95y38bdj0hx79yysgkcd8r8p9qqfzlwpv1"))
              (modules '((guix build utils)
                         (ice-9 ftw)
                         (srfi srfi-1)))
              (snippet
               `(begin
                  (let ((keep
                         '("arc4random-src"
                           "celt-0.7.0-build"
                           "celt-0.7.0-src"
                           "celt-0.11.0-build"
                           "celt-0.11.0-src"
                           "qqbonjour-src"
                           "rnnoise-build"
                           "rnnoise-src"
                           "smallft-src")))
	            (with-directory-excursion "3rdparty"
	              (for-each delete-file-recursively
			        (lset-difference string=?
                                                 (scandir ".")
                                                 (cons* "." ".." keep))))
                    #t)))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f  ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "qmake" "main.pro" "QMAKE_LRELEASE=lrelease"
                     (string-append "MUMBLE_PYTHON="
                                    (search-input-file inputs
                                                       "/bin/python3"))
                     (string-append "CONFIG+="
                                    (string-join
                                     ;; Options used are listed in the same order
                                     ;; as in the "INSTALL" file
                                     ;; (plus the final "packaged" and "release").
                                     (list "no-bundled-speex" ; in speex
                                           "no-bundled-opus" ; in opus
                                           "no-g15" ; not packaged
                                           "no-jackaudio" ; use pulse
                                           "no-oss" ; use pulse
                                           "no-alsa" ; use pulse
                                           "no-update"
                                           "no-embed-qt-translations"
                                           "no-ice" ; not packaged
                                           "packaged"
                                           "release")))
                     (string-append "DEFINES+="
                                    "PLUGIN_PATH="
                                    (assoc-ref outputs "out")
                                    "/lib/mumble"))))
         (add-before 'configure 'fix-libspeechd-include
           (lambda _
             (substitute* "src/mumble/TextToSpeech_unix.cpp"
               (("libspeechd.h") "speech-dispatcher/libspeechd.h"))
             #t))
         ;; disable statistic gathering by default. see <https://bugs.gnu.org/25201>
         (add-before 'configure 'fix-statistic-gathering-default
           (lambda _
             (substitute* "src/mumble/Settings.cpp"
               (("bUsage = true;") "bUsage = false;"))
             #t))
         (add-before 'configure 'fix-mumble-overlay
           (lambda* (#:key outputs #:allow-other-keys)
              (with-output-to-file "scripts/mumble-overlay"
                (lambda ()
                  (format #t "#!~a~%" (which "bash"))
                  (format #t "export LD_PRELOAD=\"~a $LD_PRELOAD\"~%"
                          (string-append (assoc-ref outputs "out")
                                         "/lib/mumble/libmumble.so.1"))
                  (format #t "exec \"${@}\"")))
              #t))
         (add-before 'install 'disable-murmur-ice
           (lambda _
             (substitute* "scripts/murmur.ini.system"
               (("^ice=") ";ice="))
             #t))
         (replace 'install ; install phase does not exist
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (etc (string-append out "/etc/murmur"))
                    (dbus (string-append out "/etc/dbus-1/system.d/"))
                    (bin (string-append out "/bin"))
                    (services (string-append out "/share/services"))
                    (applications (string-append out "/share/applications"))
                    (icons (string-append out "/share/icons/hicolor/scalable/apps"))
                    (man (string-append out "/share/man/man1"))
                    (lib (string-append out "/lib/mumble")))
               (install-file "release/mumble" bin)
               (install-file "scripts/mumble-overlay" bin)
               (install-file "scripts/mumble.protocol" services)
               (install-file "scripts/mumble.desktop" applications)
               (install-file "icons/mumble.svg" icons)
               (install-file "man/mumble-overlay.1" man)
               (install-file "man/mumble.1" man)
               (install-file "release/murmurd" bin)
               (install-file "scripts/murmur.ini.system" etc)
               (rename-file (string-append etc "/murmur.ini.system")
                            (string-append etc "/murmur.ini"))
               (install-file "scripts/murmur.conf" dbus)
               (install-file "man/murmurd.1" man)
               (for-each (lambda (file) (install-file file lib))
                         (find-files "." "\\.so\\."))
               (for-each (lambda (file) (install-file file lib))
                         (find-files "release/plugins" "\\.so$"))
               #t))))))
    (inputs
     (list avahi
           boost
           libsndfile
           libxi
           mesa ; avoid bundled
           openssl
           opus ; avoid bundled
           protobuf
           pulseaudio
           qtbase-5
           qtsvg
           speech-dispatcher
           speex ; avoid bundled
           speexdsp)) ; avoid bundled
    (native-inputs
     (list pkg-config python qttools))
    (synopsis "Low-latency, high quality voice chat software")
    (description
     "Mumble is an low-latency, high quality voice chat
software primarily intended for use while gaming.
Mumble consists of two applications for separate usage:
@code{mumble} for the client, and @code{murmur} for the server.")
    (home-page "https://wiki.mumble.info/wiki/Main_Page")
    (license (list license:bsd-3 ; mumble celt-0.7.0 qqbonjour rnnoise smallft
                   license:bsd-2 ; celt-0.11.0
                   license:isc)))) ; arc4random

(define-public twinkle
  (package
    (name "twinkle")
    (version "1.10.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/LubosD/twinkle")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (patches
        (search-patches "twinkle-bcg729.patch")) ; To support new BCG729 API.
       (sha256
        (base32
         "0s0gi03xwvzp02ah4q6j33r9jx9nbayr6dxlg2ck9pwbay1nq1hx"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f                      ; no test target
       #:configure-flags
       (list
        ;; FIX-ME: Make Twinkle compatible with libre version of iLBC.
        ;; "-DWITH_ILBC=On"                ; For iLBC Codec Support
        "-DWITH_ZRTP=On"                ; For ZRTP Support
        "-DWITH_G729=On"                ; For G729 Codec Support
        "-DWITH_SPEEX=On")))            ; For Speex Codec Support
    (native-inputs
     (list bison flex qttools))
    (inputs
     (list alsa-lib
           bcg729
           zrtpcpp
           ccrtp
           file
           libilbc
           libsndfile
           libxml2
           qtbase-5
           qtdeclarative
           qtquickcontrols
           readline
           speex
           speexdsp
           ucommon))
    (synopsis "Softphone for voice over IP and instant messaging")
    (description "Twinkle is a softphone for your voice over IP and instant
messaging communcations using the SIP protocol.  You can use it for direct
IP phone to IP phone communication or in a network using a SIP proxy to route
your calls and messages.")
    (home-page "http://twinkle.dolezel.info/")
    (license license:gpl2+)))

(define-public pjproject
  (package
    (name "pjproject")
    (version "2.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pjsip/pjproject")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "04s4bgr2d22ym2ajjk6q507hyqss1p59yp8avyyyf5f8032nbaws"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled libraries.
           (delete-file-recursively "third_party")
           (substitute* "aconfigure.ac"
             (("third_party/build/os-auto.mak") ""))
           (substitute* "Makefile"
             (("third_party/build") ""))))
       (patches (search-patches "pjproject-install-libpjsua2.patch"))))
    (build-system gnu-build-system)
    (outputs '("out" "debug" "static"))
    (arguments
     `(#:test-target "selftest"
       #:configure-flags
       (list "--enable-shared"
             "--with-external-speex"
             "--with-external-gsm"
             "--with-external-srtp"
             "--with-external-pa"
             ;; The following flag is Linux specific.
             ,@(if (string-contains (or (%current-system)
                                        (%current-target-system)) "linux")
                   '("--enable-epoll")
                   '())
             "--with-gnutls"            ;disable OpenSSL checks
             "--disable-libyuv"         ;TODO: add missing package
             "--disable-silk"           ;TODO: add missing package
             "--disable-libwebrtc"      ;TODO: add missing package
             "--disable-ilbc-codec"     ;cannot be unbundled
             "--disable-g7221-codec"    ;TODO: add missing package
             "--enable-libsamplerate"
             ;; -DNDEBUG is set to prevent pjproject from raising
             ;; assertions that aren't critical, crashing
             ;; applications as the result.
             "CFLAGS=-DNDEBUG"
             ;; Specify a runpath reference to itself, which is missing and
             ;; causes the validate-runpath phase to fail.
             (string-append "LDFLAGS=-Wl,-rpath=" (assoc-ref %outputs "out")
                            "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-dep
           (lambda _ (invoke "make" "dep")))
         ;; The check phases is moved after the install phase so to
         ;; use the installed shared libraries for the tests.
         (delete 'check)
         (add-after 'install 'move-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (s (string-append (assoc-ref outputs "static") "/lib")))
               (mkdir-p s)
               (with-directory-excursion out
                 (for-each (lambda (f)
                             (rename-file f (string-append s "/" (basename f))))
                           (find-files "." "\\.a$"))))))
         (add-after 'install 'check
           (assoc-ref %standard-phases 'check))
         (add-before 'patch-source-shebangs 'autoconf
           (lambda _
             (invoke "autoconf" "-v" "-f" "-i" "-o"
                     "aconfigure" "aconfigure.ac")))
         (add-before 'autoconf 'disable-some-tests
           (lambda _
             (substitute* "pjlib/src/pjlib-test/test.h"
               ;; Disable network tests which are slow and/or require an
               ;; actual network.
               (("#define GROUP_NETWORK.*")
                "#define GROUP_NETWORK 0\n"))
             (substitute* "self-test.mak"
               ;; Fails with: pjlib-util-test-x86_64-unknown-linux-gnu:
               ;; ../src/pjlib-util-test/resolver_test.c:1501: action2_1:
               ;; Assertio n `pj_strcmp2(&pkt->q[0].name, "_sip._udp."
               ;; "domain2.com")==0' failed.
               ((" pjlib_util_test ") ""))
             (substitute* "pjsip/src/test/test.h"
               ;; Fails with: Error: unable to acquire TCP transport:
               ;; [pj_status_t=120101] Network is unreachable.
               (("#define INCLUDE_TCP_TEST.*")
                "#define INCLUDE_TCP_TEST 0\n")
               ;; The TSX tests takes a very long time to run; skip them.
               (("#define INCLUDE_TSX_GROUP.*")
                "#define INCLUDE_TSX_GROUP 0\n"))
             (substitute* "pjsip/src/test/dns_test.c"
               ;; The round_robin_test fails non-deterministically (depending
               ;; on load); skip it (see:
               ;; https://github.com/pjsip/pjproject/issues/2500).
               (("round_robin_test(pool)") 0))
             (substitute* "pjmedia/src/test/test.h"
               ;; The following tests require a sound card.
               (("#define HAS_MIPS_TEST.*")
                "#define HAS_MIPS_TEST 0\n")
               (("#define HAS_JBUF_TEST.*")
                "#define HAS_JBUF_TEST 0\n"))
             (substitute* "Makefile"
               ;; Disable the pjnath and pjsua tests, which require an actual
               ;; network and an actual sound card, respectively.
               (("pjnath-test pjmedia-test pjsip-test pjsua-test")
                "pjmedia-test pjsip-test")))))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list bcg729
           gnutls
           gsm
           libsamplerate
           libsrtp
           opus
           portaudio
           speex
           speexdsp))
    (home-page "https://www.pjsip.org")
    (synopsis "Session Initiation Protocol (SIP) stack")
    (description "PJProject provides an implementation of the Session
Initiation Protocol (SIP) and a multimedia framework.")
    (license license:gpl2+)))

(define-public libtgvoip
  (package
    (name "libtgvoip")
    (version "2.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grishka/libtgvoip")
             (commit version)))
       (file-name (git-file-name name version))
       ;; Fix compilation on i686-linux architecture.
       ;; NOTE: Applying these patches is order-dependent!
       ;; The patch for WebRTC /must/ precede the patch for SSE2.
       (patches
        (search-patches "libtgvoip-disable-webrtc.patch"
                        "libtgvoip-disable-sse2.patch"))
       (sha256
        (base32
         "122kn3jx6v0kkldlzlpzvlwqxgp6pmzxsjhrhcxw12bx9c08sar5"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib openssl opus pulseaudio))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; libtgvoip wants to dlopen libpulse and libasound, so tell it where
         ;; they are.
         (add-after 'unpack 'patch-dlopen
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "os/linux/AudioPulse.cpp"
               (("libpulse\\.so")
                (search-input-file inputs "/lib/libpulse.so")))
             (substitute* '("os/linux/AudioInputALSA.cpp"
                            "os/linux/AudioOutputALSA.cpp")
               (("libasound\\.so")
                (search-input-file inputs "/lib/libasound.so"))))))))
    (synopsis "VoIP library for Telegram clients")
    (description "A collection of libraries and header files for implementing
telephony functionality into custom Telegram clients.")
    (home-page "https://github.com/zevlg/libtgvoip")
    (license license:unlicense)))
