;;; GNU Guix --- Functional package management for GNU
;;;
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
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

(define-module (gnu packages linphone)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu))

(define-public bcunit
  (package
    (name "bcunit")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0ylchj8w98ic2fkqpxc6yk4s6s0h0ql2zsz5n49jd7126m4h8dqk"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=NO")))         ; Not required
    (synopsis "Belledonne Communications Unit Testing Framework")
    (description "BCUnit is a fork of the defunct project CUnit,
with several fixes and patches applied.  It is an unit testing
framework for writing, administering, and running unit tests in C.")
    (home-page "https://gitlab.linphone.org/BC/public/bcunit")
    (license license:lgpl2.0+)))

(define-public bctoolbox
  (package
    (name "bctoolbox")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "1a1i70pb4hhnykkwyhhc7fv67q556l8kprny8xzgfqpj1nby2ms6"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=OFF")))        ; Not required
    (inputs
     `(("bcunit" ,bcunit)
       ("mbedtls" ,mbedtls-apache)))
    (synopsis "Belledonne Communications Tool Box")
    (description "BcToolBox is an utilities library used by Belledonne
Communications softwares like belle-sip, mediastreamer2 and linphone.")
    (home-page "https://gitlab.linphone.org/BC/public/bctoolbox")
    (license license:gpl2+)))

(define-public belr
  (package
    (name "belr")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "1fwv2cg3qy9vdc7dimcda7nqcqc1h2cdd7ikhk7ng7q4ys8m96c1"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=OFF")))        ; Not required
    (inputs
     `(("bctoolbox" ,bctoolbox)))
    (synopsis "Belledonne Communications Language Recognition Library")
    (description "Belr is Belledonne Communications' language recognition library,
written in C++11.  It parses text inputs formatted according to a language
defined by an ABNF grammar, such as the protocols standardized at IETF.")
    (home-page "https://gitlab.linphone.org/BC/public/belr")
    (license license:gpl3+)))

(define-public belcard
  (package
    (name "belcard")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0iiyrll1shnbb0561pkvdqcmx9b2cdr76xpsbaqdirc3s4xzcl0k"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=OFF")))        ; Not required
    (inputs
     `(("bctoolbox" ,bctoolbox)
       ("belr" ,belr)))
    (synopsis "Belledonne Communications VCard Library")
    (description "Belcard is a C++ library to manipulate VCard standard format.")
    (home-page "https://gitlab.linphone.org/BC/public/belcard")
    (license license:gpl3+)))

(define-public bcmatroska2
  (package
    (name "bcmatroska2")
    (version "0.23")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "1a0vlk4fhh189pfzrwbc3xbc5vyx6cnxy642d1h40045jz9y4h15"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=NO")))         ; Not required
    (synopsis "Belledonne Communications Media Container")
    (description "BcMatroska is a free and open standard multi-media
container format.  It can hold an unlimited number of video, audio,
picture, or subtitle tracks in one file. ")
    (home-page "https://gitlab.linphone.org/BC/public/bcmatroska2")
    (license
     (list
      ;; That license applies for Core C and LibEBML2.
      ;; https://www.matroska.org/node/47
      license:bsd-4
      ;; That license applies for LibMatroska2.
      ;; https://www.matroska.org/node/47
      license:lgpl2.1+))))

(define-public bcg729
  (package
    (name "bcg729")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "01y34ky7ykjgfnf8a9f59hg61fqfjiprfrzshdz06w0lz4gvy3qs"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=NO")))         ; Not required
    (synopsis "Belledonne Communications G729 Codec")
    (description "BcG729 is an implementation of both encoder and decoder of
the ITU G729 speech codec.  The library written in C 99 is fully portable and
can be executed on many platforms including both ARM and x86 processors.  It
supports concurrent channels encoding and decoding for multi call application
such as conferencing.")
    (home-page "https://gitlab.linphone.org/BC/public/belcard")
    (license license:gpl2+)))

(define-public ortp
  (package
    (name "ortp")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "016qg0lmdgmqh2kv19w9qhi4kkiyi5h1xp35g2s65b1j8ccm25d5"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=NO")))         ; Not required
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)))
    (inputs
     `(("bctoolbox" ,bctoolbox)))
    (synopsis "Belledonne Communications RTP Library")
    (description "oRTP is a C library implementing the RTP protocol.  It
implements the RFC 3550 standard.")
    (home-page "https://gitlab.linphone.org/BC/public/ortp")
    (license license:gpl2+)))

(define-public bzrtp
  (package
    (name "bzrtp")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "12y0kkh90pixaaxfyx26ca2brhy6nw57fsypp6vh8jk1illv0j5z"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=NO")))
    (inputs
     `(("bctoolbox" ,bctoolbox)
       ("sqlite3" ,sqlite)
       ("xml2" ,libxml2)))
    (synopsis "Belledonne Communications ZRTP Library")
    (description "BZRTP is an implementation of ZRTP keys exchange
protocol, written in C.  It is fully portable and can be executed on many
platforms including both ARM and x86.")
    (home-page "https://gitlab.linphone.org/BC/public/bzrtp")
    (license license:gpl2+)))

(define-public belle-sip
  (package
    (name "belle-sip")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0s55kggmgxap54dkw5856bgk4xg7yvbzialpxnjm0zhpic3hff1z"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; Requires network access
       #:configure-flags
       (list
        "-DENABLE_STATIC=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "src/CMakeLists.txt"
              ;; ANTLR would use multithreaded DFA generation
              ;; otherwise--which would not be reproducible.
              (("-Xmultithreaded ") ""))
             #t)))))
    (inputs
     `(("antlr3" ,antlr3-3.3)
       ("antlr3c" ,libantlr3c)
       ("bctoolbox" ,bctoolbox)
       ("java" ,icedtea)
       ("zlib" ,zlib)))
    (synopsis "Belledonne Communications SIP Library")
    (description "Belle-sip is a modern library implementing SIP transport,
transaction and dialog layers.  It is written in C, with an object-oriented
API.  It also comprises a simple HTTP/HTTPS client implementation.")
    (home-page "https://gitlab.linphone.org/BC/public/belle-sip")
    (license license:gpl2+)))

(define-public mediastreamer2
  (package
    (name "mediastreamer2")
    (version "2.16.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/"
                       "mediastreamer/mediastreamer-" version ".tar.gz"))
       (sha256
        (base32 "0whpqr69wz0pnzvragkpfblxhd0rds8k06c3mw5a0ag216a1yd9k"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=NO"            ; Not required
        "-DENABLE_STRICT=NO"            ; Would otherwise treat warnings as err
        "-DENABLE_BV16=NO"              ; Not available
        "-DCMAKE_C_FLAGS=-DMS2_GIT_VERSION=\\\"unknown\\\""
        "-DCMAKE_CXX_FLAGS=-DMS2_GIT_VERSION=\\\"unknown\\\"")))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("python" ,python)))
    (inputs
     `(("alsa" ,alsa-lib)
       ("bcg729" ,bcg729)
       ("bcmatroska2" ,bcmatroska2)
       ("bctoolbox" ,bctoolbox)
       ("ffmpeg" ,ffmpeg)
       ("glew" ,glew)
       ("glu" ,glu)
       ("glx" ,mesa-utils)
       ("gsm" ,gsm)
       ("opengl" ,mesa)
       ("opus" ,opus)
       ("ortp" ,ortp)
       ("pcap" ,libpcap)
       ("portaudio" ,portaudio)
       ("pulseaudio" ,pulseaudio)
       ("spandsp" ,spandsp)
       ("speex" ,speex)
       ("speexdsp" ,speexdsp)
       ("srtp" ,libsrtp)
       ("theora" ,libtheora)
       ("turbojpeg" ,libjpeg-turbo)
       ("v4l" ,v4l-utils)
       ("vpx" ,libvpx)
       ("x11" ,libx11)
       ("xv" ,libxv)
       ("zrtp", bzrtp)))
    (synopsis "Belledonne Communications Streaming Engine")
    (description "Mediastreamer is a powerful and lightweight streaming
engine for telephony applications.  This media processing and streaming toolkit
is responsible for receiving and sending all multimedia streams in Linphone,
including media capture, encoding and decoding, and rendering.")
    (home-page "https://gitlab.linphone.org/BC/public/mediastreamer2")
    (license license:gpl2+)))

(define-public liblinphone
  (package
    (name "liblinphone")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/linphone"
                       "/linphone-" version ".tar.gz"))
       (sha256
        (base32 "0phhkx55xdyg28d4wn8l8q4yvsmdgzmjiw584d4s190sq1azm91x"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        (string-append "-DGTK2_GDKCONFIG_INCLUDE_DIR="
                       (string-append (assoc-ref %build-inputs "gtk2")
                                      "/lib/gtk-2.0/include"))
        (string-append "-DGTK2_GLIBCONFIG_INCLUDE_DIR="
                       (string-append (assoc-ref %build-inputs "glib")
                                      "/lib/glib-2.0/include"))
        "-DENABLE_STATIC=NO"            ; Not required
        "-DENABLE_GTK_UI=YES")          ; For Legacy UI
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "gtk/main.c"
               (("#include \"liblinphone_gitversion.h\"")
                ""))
             #t)))))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("gettext" ,gettext-minimal)
       ("iconv" ,libiconv)
       ("python" ,python)
       ("xml2" ,libxml2)
       ("zlib" ,zlib)))
    (inputs
     `(("bctoolbox" ,bctoolbox)
       ("belcard" ,belcard)
       ("bellesip" ,belle-sip)
       ("bzrtp", bzrtp)
       ("glib" ,glib)
       ("gtk2" ,gtk+-2)
       ("mediastreamer2" ,mediastreamer2)
       ("notify" ,libnotify)
       ("ortp" ,ortp)
       ("pystache" ,python-pystache)
       ("six" ,python-six)
       ("sqlite" ,sqlite)
       ("udev" ,eudev)))
    (propagated-inputs
     `(("murrine" ,murrine)))           ; Required for GTK UI
    (synopsis "Belledonne Communications Softphone Library")
    (description "Liblinphone is a high-level SIP library integrating
all calling and instant messaging features into an unified
easy-to-use API.  It is the cross-platform VoIP library on which the
Linphone application is based on, and that anyone can use to add audio
and video calls or instant messaging capabilities to an application.")
    (home-page "https://gitlab.linphone.org/BC/public/liblinphone")
    (license license:gpl2+)))
