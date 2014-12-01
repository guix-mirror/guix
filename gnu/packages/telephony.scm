;;; GNU Guix --- Functional package management for GNU
;;; Copyright 2014 John Darrington <jmd@gnu.org>
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
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
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
       (alist-cons-before
        'configure 'pre-configure
        (lambda _
          (substitute* "src/applog.cpp"
            (("^// TODO sc.*") "#include <sys/types.h>\n#include <sys/stat.h>\n")))
        %standard-phases)))
   (build-system gnu-build-system)
   (synopsis "(u)Common C++ framework for threaded applications")
   (description "GNU Common C++ is an portable, optimized class framework for
threaded applications, supporting concurrent synchronization, inter-process
communications via sockets, and various methods for data handling, such as
serialization and XML parsing.  It includes the uCommon C++ library, a smaller
reimplementation.")
   (license gpl2+) ; plus runtime exception
   (home-page "http://www.gnu.org/software/commoncpp")))

(define-public ucommon
  (package
   (name "ucommon")
   (version "6.2.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/commoncpp/" name "-"
                   version ".tar.gz"))
            (sha256 (base32
                     "1apk1k877knvh2k1yqspsln5wm81a4ly4w97ang1qhi21ydwgjnn"))))
   (build-system gnu-build-system)
   (synopsis "Common C++ framework for threaded applications")
   (description "GNU uCommon C++ is meant as a very light-weight C++ library
to facilitate using C++ design patterns even for very deeply embedded
applications, such as for systems using uclibc along with posix threading
support.")
   (license gpl2+) ; plus runtime exception
   (home-page "http://www.gnu.org/software/commoncpp")))

(define-public ccrtp
  (package
   (name "ccrtp")
   (version "2.1.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/ccrtp/ccrtp-"
                   version ".tar.gz"))
            (sha256 (base32
                     "1p1pk2m7v75rdrh05rizpqcd5p08g3n541rw0kssyfzd805fb90d"))))
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
   (license gpl2+) ; plus runtime exception
   (home-page "http://www.gnu.org/software/ccrtp")))


(define-public osip
  (package
   (name "osip")
   (version "4.1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/osip/libosip2-" version ".tar.gz"))
            (sha256 (base32
                     "014503kqv7z63az6lgxr5fbajlrqylm5c4kgbf8p3a0n6cva0slr"))))
   (build-system gnu-build-system)

   (synopsis "Library implementing SIP (RFC-3261)")
   (description "GNU oSIP is an implementation of the SIP protocol.  It is
used to provide multimedia and telecom software developers with an interface
to initiate and control SIP sessions.")
   (license lgpl2.1+)
   (home-page "http://www.gnu.org/software/osip")))


(define-public exosip
  (package
   (name "exosip")
   (version "4.1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append 
                  "http://download.savannah.gnu.org/releases/exosip/libeXosip2-"
                  version ".tar.gz"))
            (sha256 (base32
                     "17cna8kpc8nk1si419vgr6r42k2lda0rdk50vlxrw8rzg0xp2xrw"))))
   (build-system gnu-build-system)
   (inputs `(("osip" ,osip)))
   (synopsis "Sip abstraction library")
   (description "EXosip is a library that hides the complexity of using the
SIP protocol for mutlimedia session establishement.  This protocol is mainly to
be used by VoIP telephony applications (endpoints or conference server) but
might be also usefull for any application that wish to establish sessions like
multiplayer games.")
   (license gpl2+) 
   ;; (plus OpenSSL linking exception)
   ;; http://git.savannah.gnu.org/cgit/exosip.git/plain/LICENSE.OpenSSL
    (home-page "http://savannah.nongnu.org/projects/exosip")))

(define-public sipwitch
  (package
   (name "sipwitch")
   (version "1.9.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/sipwitch/sipwitch-"
                   version ".tar.gz"))
            (sha256 (base32
                     "1iyh390rmxqrks7rypl8ql7fhd3pmy2ckqnp1p0llzrx67jh2q91"))))
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
   (license gpl3+)
   (home-page "http://www.gnu.org/software/sipwitch")))

