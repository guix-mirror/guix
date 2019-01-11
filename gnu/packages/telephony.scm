;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 Francesco Frassinelli <fraph24@gmail.com>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Adonay Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@hyperbola.info>
;;; Copyright © 2018 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages file)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
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
  #:use-module (guix build-system gnu))

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
   (version "5.0.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/osip/libosip2-" version ".tar.gz"))
            (patches (search-patches "osip-CVE-2017-7853.patch"))
            (sha256
             (base32
              "00yznbrm9q04wgd4b831km8iwlvwvsnwv87igf79g5vj9yakr88q"))))
   (build-system gnu-build-system)

   (synopsis "Library implementing SIP (RFC-3261)")
   (description "GNU oSIP is an implementation of the SIP protocol.  It is
used to provide multimedia and telecom software developers with an interface
to initiate and control SIP sessions.")
   (license license:lgpl2.1+)
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
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cisco/libsrtp/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02x5l5h2nq6f9gq1bmgz5v9jmnqaab51p8aldglng1z7pjbp9za4"))))
    (native-inputs
     `(("psmisc" ,psmisc)               ;some tests require 'killall'
       ("procps" ,procps)))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "runtest"))
    (synopsis "Secure RTP (SRTP) Reference Implementation")
    (description
     "This package provides an implementation of the Secure Real-time Transport
Protocol (@dfn{SRTP}), the Universal Security Transform (@dfn{UST}), and a
supporting cryptographic kernel.")
    (home-page "https://github.com/cisco/libsrtp")
    (license license:bsd-3)))

(define-public bctoolbox
  (package
    (name "bctoolbox")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/linphone/bctoolbox/bctoolbox-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14ivv6bh6qywys6yyb34scy9w78d636xl1f7cyxm3gwx2qv71lx5"))))
    (build-system gnu-build-system)
    (arguments '(#:make-flags '("CFLAGS=-fPIC")))
    (native-inputs
     `(("cunit" ,cunit)))
    (inputs
     `(("mbedtls" ,mbedtls-apache)))
    (home-page "https://www.linphone.org")
    (synopsis "Utilities library for linphone software")
    (description "BCtoolbox is a utilities library used by Belledonne
Communications softwares like linphone.")
    (license license:gpl2+)))

(define-public ortp
  (package
    (name "ortp")
    (version "0.27.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.savannah.nongnu.org/"
                                  "releases/linphone/ortp/sources/ortp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1by0dqdqrj5avzcvjws30g8v5sa61wj12x00sxw0kn1smcrshqgb"))))
    (build-system gnu-build-system)
    (inputs
     `(("bctoolbox" ,bctoolbox)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://linphone.org/")
    (synopsis "Implementation of the Real-time transport protocol")
    (description "oRTP is a library implementing the Real-time transport
protocol (RFC 3550).")
    (license license:lgpl2.1+)))

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
    (version "1.2.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://mumble.info/snapshot/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s60vaici3v034jzzi20x23hsj6mkjlc0glipjq4hffrg9qgnizh"))
              (patches (search-patches "mumble-1.2.19-abs.patch"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; Remove bundled software.
                  (for-each delete-file-recursively '("3rdparty"
                                                      "speex"
                                                      "speexbuild"
                                                      "opus-build"
                                                      "opus-src"
                                                      "sbcelt-helper-build"
                                                      "sbcelt-lib-build"
                                                      "sbcelt-src"))
                  ;; TODO: Celt is still bundled. It has been merged into Opus
                  ;; and will be removed after 1.3.0.
                  ;; https://github.com/mumble-voip/mumble/issues/1999
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake" "main.pro" "-recursive"
                     (string-append "CONFIG+="
                                    (string-join
                                     (list "no-update"
                                           "no-ice"
                                           "no-embed-qt-translations"
                                           "no-bundled-speex"
                                           "pch"
                                           "no-bundled-opus"
                                           "no-celt"
                                           "no-alsa"
                                           "no-oss"
                                           "no-portaudio"
                                           "speechd"
                                           "no-g15"
                                           "no-bonjour"
                                           "release")))
                     (string-append "DEFINES+="
                                    "PLUGIN_PATH="
                                    (assoc-ref outputs "out")
                                    "/lib/mumble"))))
         (add-before 'configure 'fix-libspeechd-include
           (lambda _
             (substitute* "src/mumble/TextToSpeech_unix.cpp"
               (("libspeechd.h") "speech-dispatcher/libspeechd.h"))))
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
       ("protobuf" ,protobuf)
       ("openssl" ,openssl)
       ("libsndfile" ,libsndfile)
       ("boost" ,boost)
       ("opus" ,opus)
       ("speex" ,speex)
       ("speexdsp" ,speexdsp)
       ("speech-dispatcher" ,speech-dispatcher)
       ("libx11" ,libx11)
       ("libxi" ,libxi)
       ("qt-4" ,qt-4)
       ("alsa-lib" ,alsa-lib)
       ("pulseaudio" ,pulseaudio)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Low-latency, high quality voice chat software")
    (description
     "Mumble is an low-latency, high quality voice chat
software primarily intended for use while gaming.
Mumble consists of two applications for separate usage:
@code{mumble} for the client, and @code{murmur} for the server.")
    (home-page "https://wiki.mumble.info/wiki/Main_Page")
    (license (list license:bsd-3
                   ;; The bundled celt is bsd-2. Remove after 1.3.0.
                   license:bsd-2))))

(define-public twinkle
  (let ((commit "02e1d1538af3337134bd7381dcd95f8d7775b30f")
        (revision "1"))
  (package
   (name "twinkle")
   (version (git-version "1.10.1" revision commit))
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/LubosD/twinkle")
                   (commit commit)))
             (patches
              (search-patches "twinkle-include-qregexpvalidator.patch"))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0ds4rp4vr1wagn4m4m7ldqbsx5vgmgbfcqqgyhn1wf6s1dm0020z"))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f ; no test target
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
   (license license:gpl2+))))

(define-public pjproject
  (package
    (name "pjproject")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.pjsip.org/release/" ;
             version "/" name "-" version ".tar.bz2"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (let ((third-party-directories
                  (list "BaseClasses" "bdsound" "bin" "g7221" "gsm"
                        "ilbc" "lib" "milenage" "mp3" "speex" "srtp"
                        "resample"
                        ;; Keep only resample, build and README.txt.
                        "build/baseclasses" "build/g7221" "build/gsm"
                        "build/ilbc" "build/milenage" "build/samplerate"
                        "build/speex" "build/srtp"
                        "build/resample" "build/yuv")))
             ;; Keep only Makefiles related to resample.
             (for-each (lambda (file)
                         (delete-file-recursively
                          (string-append "third_party/" file)))
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
       (sha256
        (base32
         "0wiph6g51wanzwjjrpwsz63amgvly8g08jz033gnwqmppa584b4w"))))
    (build-system gnu-build-system)
    (inputs
     `(("portaudio" ,portaudio)))
    (propagated-inputs
     ;; These packages are referenced in the Libs field of the pkg-config
     ;; file that will be installed by pjproject.
     `(("speex" ,speex)
       ("libsrtp" ,libsrtp)
       ("gnutls" ,gnutls)
       ("util-linux" ,util-linux)))
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
             (invoke "autoconf" "-vfi" "-o"
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

(define %jami-version "20190319.4.a16a99f")

(define* (jami-source #:key without-daemon)
  (origin
    (method url-fetch)
    (uri (string-append "http://dl.jami.net/ring-release/tarballs/ring_"
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
      "1c6n6sm7skw83v25g33g4jzbragz9j4przbzaz7asxw54jy33dwl"))))

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
             ;; "-fPIC" is required for libring.  Bug?
             "CFLAGS=-fPIC -DPJ_ENABLE_EXTRA_CHECK=1 -DPJ_ICE_MAX_CAND=256 -DPJ_ICE_MAX_CHECKS=1024 -DPJ_ICE_COMP_BITS=2 -DPJ_ICE_MAX_STUN=3 -DPJSIP_MAX_PKT_LEN=8000 -DPJ_ICE_ST_MAX_CAND=32"
             "CXXFLAGS=-fPIC -DPJ_ENABLE_EXTRA_CHECK=1 -DPJ_ICE_MAX_CAND=256 -DPJ_ICE_MAX_CHECKS=1024 -DPJ_ICE_COMP_BITS=2 -DPJ_ICE_MAX_STUN=3 -DPJSIP_MAX_PKT_LEN=8000 -DPJ_ICE_ST_MAX_CAND=32"
             ;; Now deviating from the rules.mak file.
             "--enable-ssl=gnutls"
             "--with-external-srtp")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'apply-patches
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((savoir-faire-linux-patches-directory "Savoir-faire Linux patches")
                   ;; Comes from
                   ;; "ring-project/daemon/contrib/src/pjproject/rules.mak".
                   ;; WARNING: These amount for huge changes in pjproject.
                   ;; Particularly, they add support for GnuTLS.
                   (savoir-faire-linux-patches
                    '("gnutls"
                      "rfc2466"
                      "ipv6"
                      "ice_config"
                      "multiple_listeners"
                      "pj_ice_sess"
                      "fix_turn_fallback"
                      "fix_ioqueue_ipv6_sendto"
                      "add_dtls_transport"
                      "rfc6062")))
               (mkdir-p savoir-faire-linux-patches-directory)
               (invoke "tar" "-xvf" (assoc-ref inputs "savoir-faire-linux-patches")
                       "-C" savoir-faire-linux-patches-directory "--strip-components=5" "ring-project/daemon/contrib/src/pjproject")
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
