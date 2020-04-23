;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2016, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 Francesco Frassinelli <fraph24@gmail.com>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Adonay Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@hyperbola.info>
;;; Copyright © 2018 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2019 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
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
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system qt))

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
   (inputs `(("gnutls" ,gnutls)))
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
   (inputs `(("ucommon" ,ucommon)
             ("libgcrypt" ,libgcrypt)))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (synopsis "Implementation of RTP (real-time transport protocol)")
   (description  "GNU ccRTP is an implementation of RTP, the real-time transport
protocol from the IETF.  It is suitable both for high capacity servers and
personal client applications.  It is flexible in its design, allowing it to
function as a framework for the framework, rather than just being a
packet-manipulation library.")
   (license license:gpl2+) ; plus runtime exception
   (home-page "https://www.gnu.org/software/ccrtp/")))


(define-public osip
  (package
   (name "osip")
   (version "5.1.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/osip/libosip2-" version ".tar.gz"))
            (patches (search-patches "osip-CVE-2017-7853.patch"))
            (sha256
             (base32
              "0kgnxgzf968kbl6rx3hjsfb3jsg4ydgrsf35gzj319i1f8qjifv1"))))
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
   (inputs `(("osip" ,osip)))
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
   (inputs `(("ucommon" ,ucommon)
             ("exosip" ,exosip)
             ("osip" ,osip)))
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
    (version "2.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cisco/libsrtp")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f7i3jdh1wzdv7zjlz7gs3xw5jqig9zw8z9awsqqcp54f94xdpvd"))))
    (native-inputs
     `(("psmisc" ,psmisc)               ;some tests require 'killall'
       ("procps" ,procps)))
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
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)))
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
     `(("alsa-lib" ,alsa-lib)
       ("gmp" ,gmp)
       ("libogg" ,libogg)
       ("ncurses" ,ncurses)
       ("opus" ,opus)))
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
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://mumble.info/snapshot/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "03dqg5yf6d7ilc1wydpshnv1ndssppcbadqcq20jm5j4fdaf53cs"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; Remove bundled software.  Keep arc4random, celt-0.7.0,
                  ;; celt-0.11.0, qqbonjour, rnnoise, smallft.
                  (for-each
                    delete-file-recursively
                    '("3rdparty/GL" ; in mesa
                      "3rdparty/mach-override-build" ; for macx
                      "3rdparty/mach-override-src"
                      "3rdparty/minhook-build" ; for win32
                      "3rdparty/minhook-src"
                      "3rdparty/opus-build" ; in opus
                      "3rdparty/opus-src"
                      "3rdparty/sbcelt-helper-build" ; not enabled
                      "3rdparty/sbcelt-lib-build"
                      "3rdparty/sbcelt-src"
                      "3rdparty/speex-build" ; in speex
                      "3rdparty/speex-src"
                      "3rdparty/speexdsp-src" ; in speexdsp
                      "3rdparty/xinputcheck-build" ; for win32
                      "3rdparty/xinputcheck-src"))
                  #t))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f  ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "qmake" "main.pro" "QMAKE_LRELEASE=lrelease"
                     (string-append "MUMBLE_PYTHON="
                                    (string-append (assoc-ref inputs "python")
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
                         (find-files "release/plugins" "\\.so$"))))))))
    (inputs
     `(("avahi" ,avahi)
       ("boost" ,boost)
       ("libsndfile" ,libsndfile)
       ("libxi" ,libxi)
       ("mesa" ,mesa) ; avoid bundled
       ("openssl" ,openssl)
       ("opus" ,opus) ; avoid bundled
       ("protobuf" ,protobuf)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("speech-dispatcher" ,speech-dispatcher)
       ("speex" ,speex) ; avoid bundled
       ("speexdsp" ,speexdsp))) ; avoid bundled
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)
       ("qttools" ,qttools)))
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
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LubosD/twinkle")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s0gi03xwvzp02ah4q6j33r9jx9nbayr6dxlg2ck9pwbay1nq1hx"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                                ; no test target
       #:configure-flags '("-DWITH_SPEEX=On")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/twinkle")
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/plugins"))
                         '("qtbase" "qtdeclarative")))
                 `("QML2_IMPORT_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/qml"))
                         '("qtdeclarative" "qtquickcontrols"))))
               #t))))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("readline" ,readline)
       ("file" ,file)
       ("ucommon" ,ucommon)
       ("ccrtp" ,ccrtp)
       ("libxml2" ,libxml2)
       ("speex" ,speex)
       ("speexdsp" ,speexdsp)
       ("libsndfile" ,libsndfile)
       ("alsa-lib" ,alsa-lib)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtquickcontrols" ,qtquickcontrols)))
    (home-page "http://twinkle.dolezel.info/")
    (synopsis "Softphone for voice over IP and instant messaging")
    (description "Twinkle is a softphone for your voice over IP and instant
messaging communcations using the SIP protocol.  You can use it for direct IP
phone to IP phone communication or in a network using a SIP proxy to route your
calls and messages")
    (license license:gpl2+)))

(define-public pjproject
  (package
    (name "pjproject")
    (version "2.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pjsip/pjproject.git")
             (commit "5dfa75be7d69047387f9b0436dd9492bbbf03fe4")))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (let ((third-party-directories
                  ;; Things we don't need:
                  ;; BaseClasses - contains libraries from Windows SDK
                  ;; we don't need it, at least not now.
                  (list "BaseClasses" "g7221" "ilbc" "milenage"
                        "speex" "threademulation" "yuv" "bdsound"
                        "gsm" "mp3" "resample" "srtp" "webrtc"
                        ;; Keep only resample, build and README.txt.
                        "build/baseclasses" "build/g7221" "build/gsm"
                        "build/ilbc" "build/milenage" "build/resample"
                        "build/samplerate" "build/speex" "build/srtp"
                        "build/webrtc" "build/yuv")))
             ;; Keep only Makefiles related to resample.
             (for-each (lambda (directory)
                         (delete-file-recursively
                          (string-append "third_party/" directory)))
                       third-party-directories)
             #t)
           (let ((third-party-dirs
                  (list "gsm" "ilbc" "speex" "g7221" "srtp"
                        "portaudio" "resample")))
             (for-each
              (lambda (dirs)
                (substitute* "third_party/build/os-linux.mak"
                  (((string-append "DIRS += " dirs)) "")))
              third-party-dirs))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ayj6n7zd5wvd1nzj2k9s57fb4ckc2fv92k5sjvhd87yg69k3393"))))
    (build-system gnu-build-system)
    (inputs
     `(("portaudio" ,portaudio)))
    (propagated-inputs
     ;; These packages are referenced in the Libs field of the pkg-config
     ;; file that will be installed by pjproject.
     `(("speex" ,speex)
       ("libsrtp" ,libsrtp)
       ("gnutls" ,gnutls)
       ("resample", resample)
       ("util-linux" ,util-linux "lib")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("libtool" ,libtool)))
    (arguments
     `(;; FIXME make: No rule to make target
       ;; 'pjlib-test-unknown-[something]-gnu'.
       #:tests? #f
       ;; #:test-target "selftest"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-dep
           (lambda _ (invoke "make" "dep")))
         (add-before 'patch-source-shebangs 'autoconf
           (lambda _
             (invoke "autoconf" "-v" "-f" "-i" "-o"
                     "aconfigure" "aconfigure.ac")))
         (add-before 'autoconf 'disable-some-tests
           ;; Three of the six test programs fail due to missing network
           ;; access.
           (lambda _
             (substitute* "Makefile"
               (("selftest: pjlib-test pjlib-util-test pjnath-test pjmedia-test pjsip-test pjsua-test")
                "selftest: pjlib-test pjlib-util-test pjmedia-test"))
             #t)))))
    (home-page "https://www.pjsip.org")
    (synopsis "Session Initiation Protocol (SIP) stack")
    (description "PJProject provides an implementation of the Session
Initiation Protocol (SIP) and a multimedia framework.")
    (license license:gpl2+)))

(define %jami-version "20191101.3.67671e7")

(define* (jami-source #:key without-daemon)
  (origin
    (method url-fetch)
    (uri (string-append "https://dl.jami.net/ring-release/tarballs/ring_"
                        %jami-version
                        ".tar.gz"))
    (modules '((guix build utils)))
    (snippet
     (if without-daemon
       '(begin
          (delete-file-recursively "daemon/contrib"))
       #f))
    (sha256
     (base32
      "0kw172w2ccyz438kf5xqw14nhfm4xk6a2libnzib9j2wvhlpf4q0"))))

(define-public pjproject-jami
  (package
    (inherit pjproject)
    (name "pjproject-jami")
    (native-inputs
     `(("savoir-faire-linux-patches" ,(jami-source))
       ,@(package-native-inputs pjproject)))
    (arguments
     `(#:tests? #f
       ;; See ring-project/daemon/contrib/src/pjproject/rules.mak.
       #:configure-flags
       (list "--disable-oss"
             "--disable-sound"
             "--disable-video"
             "--enable-ext-sound"
             "--disable-speex-aec"
             "--disable-g711-codec"
             "--disable-l16-codec"
             "--disable-gsm-codec"
             "--disable-g722-codec"
             "--disable-g7221-codec"
             "--disable-speex-codec"
             "--disable-ilbc-codec"
             "--disable-opencore-amr"
             "--disable-silk"
             "--disable-sdl"
             "--disable-ffmpeg"
             "--disable-v4l2"
             "--disable-openh264"
             "--disable-resample"
             "--disable-libwebrtc"
             "--with-gnutls"
             "--with-external-srtp"
             ;; We need -fPIC or else we get the following error when linking
             ;; against pjproject-jami:
             ;;   relocation R_X86_64_32S against `.rodata' can not be used when
             ;;   making a shared object;
             "CFLAGS=-fPIC"
             "CXXFLAGS=-fPIC")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-after 'unpack 'apply-patches
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((savoir-faire-linux-patches-directory "Savoir-faire Linux patches")
                   ;; Comes from
                   ;; "ring-project/daemon/contrib/src/pjproject/rules.mak".
                   ;; WARNING: These amount for huge changes in pjproject.
                   (savoir-faire-linux-patches
                    '("fix_turn_alloc_failure"
                      "rfc2466"
                      "ipv6"
                      "multiple_listeners"
                      "pj_ice_sess"
                      "fix_turn_fallback"
                      "fix_ioqueue_ipv6_sendto"
                      "add_dtls_transport"
                      "rfc6544"
                      "ice_config"
                      "sip_config"
                      "fix_first_packet_turn_tcp"
                      "fix_ebusy_turn"
                      "ignore_ipv6_on_transport_check"
                      "fix_turn_connection_failure"
                      ;; "uwp_vs" ; for windows
                      "disable_local_resolution")))
               (mkdir-p savoir-faire-linux-patches-directory)
               (invoke "tar" "-xvf" (assoc-ref inputs "savoir-faire-linux-patches")
                       "-C" savoir-faire-linux-patches-directory
                       "--strip-components=5"
                       "ring-project/daemon/contrib/src/pjproject")
               (for-each
                (lambda (file)
                  (invoke "patch" "--force" "-p1" "-i"
                          (string-append savoir-faire-linux-patches-directory "/"
                                         file ".patch")))
                savoir-faire-linux-patches))
             #t))
         ;; TODO: We could use substitute-keyword-arguments instead of
         ;; repeating the phases from pjproject, but somehow it does
         ;; not work.
         (add-before 'build 'build-dep
           (lambda _ (invoke "make" "dep")))
         (add-before 'patch-source-shebangs 'autoconf
           (lambda _
             (invoke "autoconf" "-v" "-f" "-i" "-o"
                     "aconfigure" "aconfigure.ac")))
         (add-before 'autoconf 'disable-some-tests
           ;; Three of the six test programs fail due to missing network
           ;; access.
           (lambda _
             (substitute* "Makefile"
               (("selftest: pjlib-test pjlib-util-test pjnath-test pjmedia-test pjsip-test pjsua-test")
                "selftest: pjlib-test pjlib-util-test pjmedia-test"))
             #t)))))))

(define-public libring
  (package
    (name "libring")
    (version %jami-version)
    (source (jami-source #:without-daemon #t))
    (build-system gnu-build-system)
    (inputs
     ;; Missing (optional?) dep: libnatpmp.
     `(("alsa-lib" ,alsa-lib)
       ("boost" ,boost)
       ("dbus-c++" ,dbus-c++)
       ("eudev" ,eudev)
       ("ffmpeg" ,ffmpeg)
       ("flac" ,flac)
       ("gmp" ,gmp)
       ("gsm" ,gsm)
       ("jack" ,jack-1)
       ("jsoncpp" ,jsoncpp)
       ("libogg" ,libogg)
       ("libva" ,libva)
       ("opendht" ,opendht)
       ("opus" ,opus)
       ("pcre" ,pcre)
       ("pulseaudio" ,pulseaudio)
       ("libsamplerate" ,libsamplerate)
       ("libsndfile" ,libsndfile)
       ("speex" ,speex)
       ("speexdsp" ,speexdsp)
       ("libupnp" ,libupnp)
       ("libvorbis" ,libvorbis)
       ("libx264" ,libx264)
       ("libvdpau" ,libvdpau)
       ("yaml-cpp" ,yaml-cpp)
       ("zlib" ,zlib)
       ("openssl" ,openssl)
       ("libsecp256k1" ,libsecp256k1)
       ("python" ,python)
       ("python-wrapper" ,python-wrapper)
       ("restinio" ,restinio)
       ("libx11" ,libx11)
       ("asio" ,asio)
       ;; TODO: Upstream seems to rely on a custom pjproject (a.k.a. pjsip) version.
       ;; See https://git.jami.net/savoirfairelinux/ring-daemon/issues/24.
       ("pjproject" ,pjproject-jami)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("which" ,which)
       ("cppunit" ,cppunit)
       ("perl" ,perl)))                 ; Needed for documentation.
    (arguments
     `(#:tests? #f         ; The tests fail to compile due to missing headers.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "daemon")
             #t))
         (add-before 'build 'add-lib-dir
           (lambda _
             (mkdir-p "src/lib")
             #t)))))
    (synopsis "Distributed multimedia communications platform")
    (description "Jami (formerly GNU Ring) is a secure and distributed voice,
video and chat communication platform that requires no centralized server and
leaves the power of privacy in the hands of the user.  It supports the SIP and
IAX protocols, as well as decentralized calling using P2P-DHT.

This package provides a library and daemon implementing the Jami core
functionality.")
    (home-page "https://jami.net/")
    (license license:gpl3+)))

(define-public libringclient
  (package
    (inherit libring)
    (name "libringclient")
    (build-system cmake-build-system)
    (propagated-inputs
     `(("libring" ,libring)     ; For 'dring'.
       ("qtbase" ,qtbase)       ; Qt is included in several installed headers.
       ("qttools" ,qttools)))
    (arguments
     `(#:tests? #f                      ; There is no testsuite.
       #:configure-flags
       (list (string-append "-DRING_BUILD_DIR="
                            (assoc-ref %build-inputs "libring") "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "lrc")
             #t))
         (add-before 'configure 'fix-dbus-interfaces-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{CMAKE_INSTALL_PREFIX\\}(/share/dbus-1/interfaces)" _ dbus-interfaces-path-suffix)
                (string-append (assoc-ref inputs "libring")
                               dbus-interfaces-path-suffix))))))))
    (synopsis "Distributed multimedia communications platform")
    (description "Jami (formerly GNU Ring) is a secure and distributed voice,
video and chat communication platform that requires no centralized server and
leaves the power of privacy in the hands of the user.  It supports the SIP and
IAX protocols, as well as decentralized calling using P2P-DHT.

This package provides a library common to all Jami clients.")
    (home-page "https://jami.net")
    (license license:gpl3+)))

(define-public jami
  (package
    (inherit libring)
    (name "jami")
    (build-system cmake-build-system)
    (inputs
     `(("libringclient" ,libringclient)
       ("gtk+" ,gtk+)
       ("qrencode" ,qrencode)
       ("libnotify" ,libnotify)
       ("clutter" ,clutter)
       ("clutter-gtk" ,clutter-gtk)
       ("libcanberra" ,libcanberra)
       ("webkitgtk" ,webkitgtk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("doxygen" ,doxygen)))
    (propagated-inputs
     `(("libring" ,libring) ; Contains `dring', the daemon, which is automatically by d-bus.
       ("adwaita-icon-theme" ,adwaita-icon-theme)
       ("evolution-data-server" ,evolution-data-server)))
    (arguments
     `(#:tests? #f                      ; There is no testsuite.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "client-gnome")
             #t)))))
    (synopsis "Distributed, privacy-respecting communication program")
    (description "Jami (formerly GNU Ring) is a secure and distributed voice,
video and chat communication platform that requires no centralized server and
leaves the power of privacy in the hands of the user.  It supports the SIP and
IAX protocols, as well as decentralized calling using P2P-DHT.

This package provides the Jami client for the GNOME desktop.")
    (home-page "https://jami.net")
    (license license:gpl3+)))

(define-public jami-client-gnome
  (deprecated-package "jami-client-gnome" jami))

(define-public libtgvoip
  (package
    (name "libtgvoip")
    (version "2.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grishka/libtgvoip.git")
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
     `(("alsa-lib" ,alsa-lib)
       ("libopusenc" ,libopusenc)
       ("openssl" ,openssl)
       ("pulseaudio" ,pulseaudio)))
    (synopsis "VoIP library for Telegram clients")
    (description "A collection of libraries and header files for implementing
telephony functionality into custom Telegram clients.")
    (home-page "https://github.com/zevlg/libtgvoip")
    (license license:unlicense)))
