;;; GNU Guix --- Functional package management for GNU
;;;
;;; Copyright © 2020, 2021 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages crypto)
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
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
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
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu))

(define-public bcunit
  (let ((commit "74021cc7cb20a4e177748dd2948173e1f9c270ae")
        (revision "0"))
    (package
      (name "bcunit")
      (version (git-version "3.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://git.linphone.org/bcunit")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0npdwvanjkfg9vrqs5yi8vh6wliv50ycdli8pzavir84nb31nq1b"))))
      (build-system cmake-build-system)
      (outputs '("out" "doc"))
      (arguments
       `(#:configure-flags (list "-DENABLE_STATIC=NO"
                                 "-DENABLE_CURSES=ON"
                                 "-DENABLE_DOC=ON"
                                 "-DENABLE_EXAMPLES=ON"
                                 "-DENABLE_TEST=ON"
                                 "-DENABLE_MEMTRACE=ON")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-source
             (lambda _
               ;; Include BCunit headers for examples.
               (substitute* "Examples/CMakeLists.txt"
                 (("\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}")
                  (string-append "${CMAKE_CURRENT_SOURCE_DIR} "
                                 "${PROJECT_SOURCE_DIR}/BCUnit/Headers "
                                 "${CMAKE_BINARY_DIR}/BCUnit/Headers")))
               ;; Link bcunit and bcunit_tests libraries.
               (substitute* "BCUnit/Sources/CMakeLists.txt"
                 (("target_include_directories\\(bcunit_test PUBLIC Test\\)")
                  (string-append
                   "target_include_directories(bcunit_test PUBLIC Test)\n"
                   "target_link_libraries(bcunit_test bcunit)")))))
           (replace 'check
             (lambda _
               (with-directory-excursion "BCUnit/Sources/Test"
                 (invoke "./test_bcunit"))))
           (add-after 'install 'move-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (doc (assoc-ref outputs "doc")))
                 (for-each mkdir-p
                           `(,(string-append doc "/share/doc")
                             ,(string-append doc "/share/BCUnit")))
                 (rename-file
                  (string-append out "/share/doc/BCUnit")
                  (string-append doc "/share/doc/BCUnit"))
                 (rename-file
                  (string-append out "/share/BCUnit/Examples")
                  (string-append doc "/share/BCUnit/Examples"))))))))
      (inputs
       `(("ncurses" ,ncurses)))
      (synopsis "Belledonne Communications Unit Testing Framework")
      (description "BCUnit is a fork of the defunct project CUnit, with
several fixes and patches applied.  It is a unit testing framework for
writing, administering, and running unit tests in C.")
      (home-page "https://gitlab.linphone.org/BC/public/bcunit")
      (license license:lgpl2.0+))))

(define-public bctoolbox
  (package
    (name "bctoolbox")
    (version "4.4.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/bctoolbox.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bfswwvvdshaahg4jd2j10f0sci8809s4khajd0m6b059zwc7y25"))))
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:configure-flags '("-DENABLE_STATIC=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-cmake
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Fix decaf dependency (see:
             ;; https://gitlab.linphone.org/BC/public/bctoolbox/-/issues/3).
             (let* ((decaf (assoc-ref inputs "decaf")))
               (substitute* (find-files "." "CMakeLists.txt")
                 (("find_package\\(Decaf CONFIG\\)")
                  "set(DECAF_FOUND 1)")
                 (("\\$\\{DECAF_INCLUDE_DIRS\\}")
                  (string-append decaf "/include/decaf"))
                 (("\\$\\{DECAF_TARGETNAME\\}")
                  "decaf")))))
         (add-after 'unpack 'skip-problematic-tests
           (lambda _
             ;; The following test relies on networking; disable it.
             (substitute* "tester/port.c"
               (("[ \t]*TEST_NO_TAG.*bctbx_addrinfo_sort_test\\)")
                ""))))
         (add-after 'unpack 'fix-installed-resource-directory-detection
           (lambda _
             ;; There's some broken logic in tester.c that checks if CWD, or
             ;; if its parent exist, and if so, sets the prefix where the test
             ;; resources are looked up to; disable it (see:
             ;; https://gitlab.linphone.org/BC/public/bctoolbox/-/issues/4).
             (substitute* "src/tester.c"
               (("if \\(file_exists\\(\".\"\\)\\)")
                "if (NULL)")
               (("if \\(file_exists\\(\"..\"\\)\\)")
                "if (NULL)"))))
         (replace 'check
           (lambda _
             (with-directory-excursion "tester"
               (invoke "./bctoolbox_tester")))))))
    (inputs
     `(("bcunit" ,bcunit)
       ("decaf" ,libdecaf)
       ("mbedtls" ,mbedtls-apache)))
    (synopsis "Belledonne Communications Tool Box")
    (description "BcToolBox is an utilities library used by Belledonne
Communications software like belle-sip, mediastreamer2 and linphone.")
    (home-page "https://gitlab.linphone.org/BC/public/bctoolbox")
    (license license:gpl3+)))

(define-public belr
  (package
    (name "belr")
    (version "4.4.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/belr.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w2canwwm0qb99whnangvaybvjzq8xg6vksqxykgr8fbx7clw03h"))))
    (build-system cmake-build-system)
    (outputs '("out" "debug" "tester"))
    (arguments
     `(#:configure-flags '("-DENABLE_STATIC=OFF")
       #:phases
       (modify-phases %standard-phases
         (delete 'check)                ;moved after the install phase
         (add-after 'install 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((tester (assoc-ref outputs "tester"))
                    (belr_tester (string-append tester "/bin/belr_tester"))
                    (tester-share (string-append tester "/share/belr_tester")))
               (invoke belr_tester))))
         (add-after 'install 'move-tester
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (tester (assoc-ref outputs "tester")))
               (for-each mkdir-p
                         (list (string-append tester "/bin")
                               (string-append tester "/share")))
               (rename-file
                (string-append out "/bin/belr_tester")
                (string-append tester "/bin/belr_tester"))
               (rename-file
                (string-append out "/share/belr-tester")
                ;; The detect_res_prefix procedure in bctoolbox's tester.c
                ;; resolves the resource path based on the executable path and
                ;; name, so have it match.
                (string-append tester "/share/belr_tester"))))))))
    (inputs
     `(("bctoolbox" ,bctoolbox)))
    (synopsis "Belledonne Communications Language Recognition Library")
    (description "Belr is Belledonne Communications' language recognition
library, written in C++11.  It parses text inputs formatted according to a
language defined by an ABNF grammar, such as the protocols standardized at
IETF.")
    (home-page "https://gitlab.linphone.org/BC/public/belr")
    (license license:gpl3+)))

(define-public belcard
  (package
    (name "belcard")
    (version "4.4.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/belcard.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16x2xp8d0a115132zhy1kpxkyj86ia7vrsnpjdg78fnbvmvysc8m"))))
    (build-system cmake-build-system)
    (outputs '("out" "debug" "tester"))
    (arguments
     `(#:tests? #t
       #:configure-flags '("-DENABLE_STATIC=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-vcard-grammar-location
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (vcard-grammar
                     (string-append out "/share/belr/grammars/vcard_grammar")))
               (substitute* "include/belcard/vcard_grammar.hpp"
                 (("define VCARD_GRAMMAR \"vcard_grammar\"")
                  (format #f "define VCARD_GRAMMAR ~s" vcard-grammar))))))
         (add-after 'install 'install-tester
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (tester (assoc-ref outputs "tester"))
                   (test-name (string-append ,name "_tester")))
               (for-each mkdir-p
                         (list (string-append tester "/bin")
                               (string-append tester "/share")))
               (rename-file (string-append out "/bin/" test-name)
                            (string-append tester "/bin/" test-name))
               (rename-file (string-append out "/share/" test-name)
                            (string-append tester "/share/" test-name)))))
         (delete 'check)
         (add-after 'install-tester 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (let* ((tester (assoc-ref outputs "tester"))
                      (belcard_tester (string-append tester
                                                     "/bin/belcard_tester")))
                 (invoke belcard_tester))))))))
    (inputs
     `(("bctoolbox" ,bctoolbox)
       ("belr" ,belr)))
    (synopsis "Belledonne Communications VCard Library")
    (description "Belcard is a C++ library to manipulate VCard standard
format.")
    (home-page "https://gitlab.linphone.org/BC/public/belcard")
    (license license:gpl3+)))

(define-public bcmatroska2
  (package
    (name "bcmatroska2")
    (version "0.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/bcmatroska2.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1avl9w18kh4dxm3g8j0bkw39bksd7bz3nfxvyibqqnz63ds8vfi2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                                     ; No test target
       #:configure-flags (list "-DENABLE_STATIC=NO"))) ; Not required
    (synopsis "Belledonne Communications Media Container")
    (description "BcMatroska is a free and open standard multi-media container
format.  It can hold an unlimited number of video, audio, picture, or subtitle
tracks in one file.  This project provides a convenient distribution of the
Matroska multimedia container format.")
    (home-page "https://gitlab.linphone.org/BC/public/bcmatroska2")
    (license (list license:gpl2+        ;for this package (build system files)
                   license:bsd-4        ;for Core C and LibEBML2
                   license:lgpl2.1+)))) ;for LibMatroska2

(define-public bcg729
  (package
    (name "bcg729")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://git.linphone.org/bcg729")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hal6b3w6f8y5r1wa0xzj8sj2jjndypaxyw62q50p63garp2h739"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DENABLE_STATIC=NO"
                               "-DENABLE_TESTS=YES")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'copy-inputs
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((test-patterns (assoc-ref inputs "test-patterns"))
                   (dest (string-append "test/bcg729-patterns.zip")))
               (copy-recursively test-patterns dest))))
         (replace 'check
           (lambda _
             (with-directory-excursion "test"
               (invoke "unzip" "bcg729-patterns.zip")
               (for-each
                (lambda (test-name)
                  (invoke "./testCampaign" "-s" test-name))
                (list "fixedCodebookSearch"
                      "postProcessing"
                      "adaptativeCodebookSearch"
                      "computeLP"
                      "computeAdaptativeCodebookGain"
                      "postFilter"
                      "decoder"
                      "LPSynthesisFilter"
                      "decodeLSP"
                      ;; "encoder"
                      ;; "LSPQuantization"
                      "preProcessing"
                      "decodeFixedCodeVector"
                      "CNGdecoder"
                      ;; "LP2LSPConversion"
                      "gainQuantization"
                      "findOpenLoopPitchDelay"
                      "decodeGains"
                      "computeWeightedSpeech"
                      "interpolateqLSPAndConvert2LP"
                      "decodeAdaptativeCodeVector"))))))))
    (native-inputs
     `(("perl" ,perl)
       ("test-patterns"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://www.belledonne-communications.com/"
                               "bc-downloads/bcg729-patterns.zip"))
           (sha256
            (base32 "1kivarhh3izrl9sg0szs6x6pbq2ap0y6xsraw0gbgspi4gnfihrh"))))
       ("unzip" ,unzip)))
    (synopsis "Belledonne Communications G729 Codec")
    (description "BcG729 is an implementation of both encoder and decoder of
the ITU G729 speech codec.  The library written in C 99 is fully portable and
can be executed on many platforms including both ARM and x86 processors.  It
supports concurrent channels encoding and decoding for multi call application
such as conferencing.")
    (home-page "https://linphone.org/technical-corner/bcg729")
    (license license:gpl3+)))

(define-public ortp
  (package
    (name "ortp")
    (version "4.4.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/ortp.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r1kvjzyfvkf66in4p51wi87balzg3sw3aq6r4xr609mz86spi5m"))))
    (build-system cmake-build-system)
    (outputs '("out""tester"
               "doc"))                  ;1.5 MiB of HTML doc
    (arguments
     `(#:tests? #f                      ;requires networking
       #:configure-flags (list "-DENABLE_STATIC=NO"
                               "-DENABLE_TESTS=YES")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-version-strings
           ;; See: https://gitlab.linphone.org/BC/public/ortp/-/issues/5.
           (lambda _
             (substitute* "CMakeLists.txt"
               (("VERSION [0-9]+\\.[0-9]+\\.[0-9]+")
                (string-append "VERSION " ,version))
               (("\\$\\{ORTP_DOC_VERSION\\}")
                ,version))))
         (add-after 'install 'separate-outputs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc"))
                    (doc-src (string-append out "/share/doc/ortp-" ,version))
                    (doc-dest (string-append doc "/share/doc/ortp-" ,version))
                    (tester (assoc-ref outputs "tester")))
               (for-each mkdir-p (list (string-append doc "/share/doc")
                                       (string-append tester "/bin")))
               (rename-file doc-src doc-dest)
               (rename-file (string-append out "/bin")
                            (string-append tester "/bin"))))))))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)))
    (inputs
     `(("bctoolbox" ,bctoolbox)))
    (synopsis "Belledonne Communications RTP Library")
    (description "oRTP is a C library implementing the RTP protocol.  It
implements the RFC 3550 standard.")
    (home-page "https://linphone.org/technical-corner/ortp")
    (license license:gpl3+)))

(define-public bzrtp
  (package
    (name "bzrtp")
    (version "4.4.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/bzrtp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yjmsbqmymzl4r7sba6w4a2yld8m6hzafr6jf7sj0syhwpnc3zv6"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list
        "-DENABLE_STATIC=NO"
        "-DENABLE_TESTS=YES")))
    (inputs
     `(("bctoolbox" ,bctoolbox)
       ("sqlite3" ,sqlite)
       ("xml2" ,libxml2)))
    (synopsis "Belledonne Communications ZRTP Library")
    (description "BZRTP is an implementation of ZRTP keys exchange protocol,
written in C.  It is fully portable and can be executed on many platforms
including both ARM and x86.")
    (home-page "https://gitlab.linphone.org/BC/public/bzrtp")
    (license license:gpl3+)))

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
       (list "-DENABLE_STATIC=NO")      ; Not required
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "src/CMakeLists.txt"
               ;; ANTLR would use multithreaded DFA generation otherwise,
               ;; which would not be reproducible.
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
    (version "4.4.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/mediastreamer2.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0989h3d0h7qrx4kjx8gg09j8c5hvvi3h8qi1iq1dqbppwbaxbz8c"))))
    (outputs '("out" "doc" "tester"))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags (list "-DENABLE_STATIC=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-version
           (lambda _
             (substitute* "CMakeLists.txt"
               (("VERSION [0-9]+\\.[0-9]+\\.[0-9]+")
                (string-append "VERSION " ,version)))))
         (add-after 'install 'separate-outputs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (tester (assoc-ref outputs "tester"))
                    (tester-name (string-append ,name "_tester"))
                    (doc (assoc-ref outputs "doc"))
                    (doc-name (string-append ,name "-" ,version)))
               (for-each mkdir-p
                         (list (string-append tester "/bin")
                               (string-append tester "/share")
                               (string-append doc "/share/doc")))
               ;; Copy the tester executable.
               (rename-file (string-append out "/bin/" tester-name)
                            (string-append tester "/bin/" tester-name))
               ;; Copy the tester data files.
               (rename-file (string-append out "/share/" tester-name)
                            (string-append tester "/share/" tester-name))
               ;; Copy the HTML documentation.
               (rename-file (string-append out "/share/doc/" doc-name)
                            (string-append doc "/share/doc/" doc-name))))))))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("python" ,python-wrapper)))
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
       ("zrtp" ,bzrtp)))
    (synopsis "Belledonne Communications Streaming Engine")
    (description "Mediastreamer2 is a powerful and lightweight streaming engine
for telephony applications.  This media processing and streaming toolkit is
responsible for receiving and sending all multimedia streams in Linphone,
including media capture, encoding and decoding, and rendering.")
    (home-page "https://linphone.org/technical-corner/mediastreamer2")
    (license license:gpl3+)))

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
    (outputs '("out" "doc" "tester"))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list (string-append "-DGTK2_GDKCONFIG_INCLUDE_DIR="
                            (string-append (assoc-ref %build-inputs "gtk2")
                                           "/lib/gtk-2.0/include"))
             (string-append "-DGTK2_GLIBCONFIG_INCLUDE_DIR="
                            (string-append (assoc-ref %build-inputs "glib")
                                           "/lib/glib-2.0/include"))
             "-DENABLE_STATIC=NO"       ; Not required
             "-DENABLE_STRICT=NO"
             "-DENABLE_GTK_UI=YES")     ; for legacy UI
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build glib-or-gtk-build-system))
       #:modules ((guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "gtk/main.c"
               (("#include \"liblinphone_gitversion.h\"")
                ""))
             #t))
         (add-after 'install 'separate-outputs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc"))
                    (tester (assoc-ref outputs "tester"))
                    (tester-name (string-append ,name "_tester")))
               ;; Copy the tester executable.
               (mkdir-p (string-append tester "/bin"))
               (rename-file (string-append out "/bin/" tester-name)
                            (string-append tester "/bin/" tester-name))
               ;; Copy the tester data files.
               (mkdir-p (string-append tester "/share/"))
               (rename-file (string-append out "/share/" tester-name)
                            (string-append tester "/share/" tester-name))
               ;; Copy the HTML and XML documentation.
               (copy-recursively
                (string-append out "/share/doc/linphone-" ,version)
                (string-append doc "/share/doc/" ,name "-" ,version))
               (delete-file-recursively
                (string-append out "/share/doc/linphone-" ,version))
               #t)))
         (add-after 'install 'install-man-pages
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1")))
               (for-each (lambda (file)
                           (install-file file man))
                         (find-files ".." ".*.1$"))
               #t)))
         (add-after 'separate-outputs 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("udev" ,eudev)                  ;for libudev.h
       ;; For generating the C++ wrappers.
       ("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("python" ,python)
       ("pystache" ,python-pystache)
       ("six" ,python-six)))
    (inputs
     `(("bctoolbox" ,bctoolbox)
       ("belcard" ,belcard)
       ("bellesip" ,belle-sip)
       ("bzrtp" ,bzrtp)
       ("iconv" ,libiconv)
       ("glib" ,glib)
       ("gtk2" ,gtk+-2)
       ("mediastreamer2" ,mediastreamer2)
       ("notify" ,libnotify)
       ("ortp" ,ortp)
       ("sqlite" ,sqlite)
       ("xml2" ,libxml2)
       ("zlib" ,zlib)))
    (synopsis "Belledonne Communications Softphone Library")
    (description "Liblinphone is a high-level SIP library integrating
all calling and instant messaging features into an unified
easy-to-use API.  It is the cross-platform VoIP library on which the
Linphone application is based on, and that anyone can use to add audio
and video calls or instant messaging capabilities to an application.")
    (home-page "https://gitlab.linphone.org/BC/public/liblinphone")
    (license license:gpl2+)))

(define-public linphoneqt
  (package
    (name "linphoneqt")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "1g2zrr9li0g1hgs6vys06vr98h5dx36z22hx7a6ry231536c002a"))
       (patches (search-patches "linphoneqt-tabbutton.patch"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-cmake-error
           (lambda _
             ;; This is fixed in commit efed2fd8 of the master branch.
             (substitute* "CMakeLists.txt"
               (("js)\\$\"")
                "js$\""))
             #t))
         (add-after 'unpack 'set-version-string
           (lambda _
             (substitute* "src/app/AppController.cpp"
               (("LINPHONE_QT_GIT_VERSION")
                (format #f "~s" ,version)))
             #t))
         (add-after 'install 'extend-shared-resources
           ;; Not using the FHS exposes an issue where the client refers to
           ;; its own "share" directory, which lacks sound files installed by
           ;; liblinphone.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((liblinphone (assoc-ref inputs "linphone"))
                    (out (assoc-ref outputs "out")))
               (symlink (string-append liblinphone "/share/sounds")
                        (string-append out "/share/sounds"))
               #t))))))
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("bctoolbox" ,bctoolbox)
       ("belcard" ,belcard)
       ("bellesip" ,belle-sip)
       ("linphone" ,liblinphone)
       ("mediastreamer2" ,mediastreamer2)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qtsvg" ,qtsvg)))
    (synopsis "Desktop client for the Linphone SIP softphone")
    (description "Linphone is a SIP softphone for voice and video over IP calling
(VoIP) and instant messaging.  Amongst its features are:
@itemize
@item High Definition (HD) audio and video calls
@item Multiple call management (pause and resume)
@item Call transfer
@item Audio conferencing (merge calls into a conference call)
@item Call recording and replay (audio only)
@item Instant Messaging with message delivery status (IMDN)
@item Picture and file sharing
@item Echo cancellation
@item Secure user authentication using TLS client certificates
@item SRTP, zRTP and SRTP-DTLS voice and video encryption
@item Telephone tone (DTMF) support using SIP INFO or RFC 4733
@item Audio codecs: opus, speex, g711, g729, gsm, iLBC, g722, SILK, etc.
@item Video codecs: VP8, H.264 and H.265 with resolutions up to 1080P, MPEG4
@end itemize")
    (home-page "https://gitlab.linphone.org/BC/public/linphone-desktop")
    (license license:gpl2+)))

(define-public msopenh264
  (package
    (name "msopenh264")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/plugins/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0rdxgazm52560g52pp6mp3mwx6j1z3h2zyizzfycp8y8zi92fqm8"))
       (patches
        (list
         ;; For support for OpenH264 version >= 2.
         (origin
           (method url-fetch)
           (uri
            (string-append "https://gitlab.linphone.org/BC/public/msopenh264/"
                           "commit/493d147d28c9a0f788ba4e50b47a1ce7b18bf326"
                           ".diff"))
           (file-name "msopenh264-openh264.patch")
           (sha256
            (base32
             "0mmd7nz5n9ian4rcwn200nldmy5j0dpdrna7r32rqnaw82bx3kdb")))))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list "-DENABLE_STATIC=NO")))    ; Not required
    (inputs
     `(("mediastreamer2" ,mediastreamer2)
       ("openh264" ,openh264)
       ("ortp" ,ortp)))
    (synopsis "Media Streamer H.264 Codec")
    (description "MsOpenH264 is an  H.264 encoder/decoder plugin for
mediastreamer2 based on the openh264 library.")
    (home-page "https://gitlab.linphone.org/BC/public/msopenh264")
    (license license:gpl2+)))

(define-public mssilk
  (package
    (name "mssilk")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/plugins/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "07ip0vd29d1n98lnqs5wpimcsmpm65yl7g5vk4hbqghcbsjw94lj"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list "-DENABLE_STATIC=NO")))    ; Not required
    (inputs
     `(("mediastreamer2" ,mediastreamer2)
       ("ortp" ,ortp)))
    (synopsis "Media Streamer SILK Codec")
    (description "MSSILK is a plugin of MediaStreamer, adding support for AMR
codec.  It is based on the Skype's SILK implementation.")
    (home-page "https://gitlab.linphone.org/BC/public/mssilk")
    (license license:gpl2+)))

(define-public mswebrtc
  (package
    (name "mswebrtc")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/plugins/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "1wj28hl9myhshqmn64xg0jf07aw75gmnilb5rff6rcbdxim87mqr"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=NO")))         ; Not required
    (inputs
     `(("bctoolbox" ,bctoolbox)
       ("mediastreamer2" ,mediastreamer2)
       ("ortp" ,ortp)))
    (synopsis "Media Streamer WebRTC Codec")
    (description "MSWebRTC is a plugin of MediaStreamer, adding support for
WebRTC codec.  It includes features from WebRTC, such as, iSAC and AECM.")
    (home-page "https://gitlab.linphone.org/BC/public/mswebrtc")
    (license license:gpl2+)))

(define-public msamr
  (package
    (name "msamr")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/plugins/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "16c9f3z4wnj73k7y8gb0fgpr4axsm7b5zrbjvy8vsgz9gyg3agm5"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list "-DENABLE_STATIC=NO"       ; Not required
             "-DENABLE_WIDEBAND=YES")))
    (inputs
     `(("mediastreamer2" ,mediastreamer2)
       ("opencoreamr" ,opencore-amr)
       ("ortp" ,ortp)
       ("voamrwbenc" ,vo-amrwbenc)))
    (synopsis "Media Streamer AMR Codec")
    (description "MSAMR is a plugin of MediaStreamer, adding support for AMR
codec.  It is based on the opencore-amr implementation.")
    (home-page "https://gitlab.linphone.org/BC/public/msamr")
    (license license:gpl3+)))
