;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 qblade <qblade@protonmail.com>
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

(define-module (gnu packages speech)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)            ;for 'which'
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils))

(define-public flite
  (package
    (name "flite")
    (version "2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/festvox/flite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n0p81jzndzc1rzgm66kw9ls189ricy5v1ps11y0p2fk1p56kbjf"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX:
     ;; There numerous issues with the testsuite.
     ;; Enable all of them once they are fixed in upstream.
     `(#:tests? #f
       #:configure-flags
       (list
        "--enable-shared"
        (string-append "LDFLAGS=-Wl,-rpath="
                       (assoc-ref %outputs "out")
                       "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-rpath
           (lambda _
             (substitute* "main/Makefile"
              (("flite_LIBS_flags \\+= -Wl,-rpath [^ ]*")
               "flite_LIBS_flags +="))
             #t))
         (delete 'check)
         ;; Modifying testsuite/Makefile is not done in favor of
         ;; overriding 'check.
         ;; The path not taken would be:
         ;; test:\n\t$(foreach x,$(subst tris1,,$(subst dcoffset_wave,,$(subst flite_test,,$(subst by_word,,$(subst bin2ascii,,$(subst lpc_resynth,,$(subst rfc,,$(subst compare_wave,,$(subst record_in_noise,,$(subst combine_waves,,$(patsubst play_%,,$(subst record_wave,,$(subst lex_lookup,,$(patsubst lpc_test%,,$(patsubst asciiS2U%,,$(patsubst asciiU2S%,,$(ALL))))))))))))))))),echo TEST $x && ./$x data.one && ) true
         (add-after 'install 'check
           (lambda _
             (invoke "make" "-C" "testsuite")
             (with-directory-excursion "testsuite"
               (invoke "./token_test")
               (invoke "./hrg_test")
               (invoke "./regex_test")
               (invoke "./nums_test")
               (invoke "./lex_test")
               (invoke "./utt_test")
               (invoke "./multi_thread"))
             #t))
         (add-after 'install 'remove-static-libs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (for-each delete-file
                         (find-files out "\\.a$"))
               #t))))))
    (native-inputs
     `(("perl" ,perl)))
    (inputs
     `(("alsa" ,alsa-lib)))
    (synopsis "Speech synthesis system")
    (description "Flite (festival-lite) is a small, fast run-time text to speech
synthesis engine developed at CMU and primarily designed for small embedded
machines and/or large servers.  It is designed as an alternative text to speech
synthesis engine to Festival for voices built using the FestVox suite of voice
building tools.")
    (home-page "http://www.festvox.org/flite/index.html")
    (license (license:non-copyleft "file:///COPYING"))))

(define-public espeak
  (package
    (name "espeak")
    (version "1.48.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/espeak/espeak/"
                                  "espeak-" (version-major+minor version)
                                  "/espeak-" version "-source.zip"))
              (sha256
               (base32
                "0n86gwh9pw0jqqpdz7mxggllfr8k0r7pc67ayy7w5z6z79kig6mz"))
              (modules '((guix build utils)))
              (snippet
               ;; remove prebuilt binaries
               '(begin
                  (delete-file-recursively "linux_32bit")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          (string-append "DATADIR="
                                         (assoc-ref %outputs "out")
                                         "/share/espeak-data")
                          (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out")
                                         "/lib")
                          ;; The package fails to build with newer C++ standards.
                          "CXXFLAGS=-std=c++98"
                          "AUDIO=pulseaudio")
       #:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (chdir "src")
             ;; We use version 19 of the PortAudio library, so we must copy the
             ;; corresponding file to be sure that espeak compiles correctly.
             (copy-file "portaudio19.h" "portaudio.h")
             (substitute* "Makefile"
               (("/bin/ln") "ln"))
             #t)))))
       (inputs
        `(("portaudio" ,portaudio)
          ("pulseaudio" ,pulseaudio)))
       (native-inputs `(("unzip" ,unzip)))
       (home-page "http://espeak.sourceforge.net/")
       (synopsis "Software speech synthesizer")
       (description "eSpeak is a software speech synthesizer for English and
other languages.  eSpeak uses a \"formant synthesis\" method.  This allows many
languages to be provided in a small size.  The speech is clear, and can be used
at high speeds, but is not as natural or smooth as larger synthesizers which are
based on human speech recordings.")
       (license license:gpl3+)))

(define-public espeak-ng
  (package
    (name "espeak-ng")
    (version "1.50")
    (home-page "https://github.com/espeak-ng/espeak-ng")
    ;; Note: eSpeak NG publishes release tarballs, but the 1.50 tarball is
    ;; broken: <https://github.com/espeak-ng/espeak-ng/issues/683>.
    ;; Download the raw repository to work around it; remove 'native-inputs'
    ;; below when switching back to the release tarball.
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0jkqhf2h94vbqq7mg7mmm23bq372fa7mdk941my18c3vkldcir1b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")
       ;; Building in parallel triggers a race condition in 1.49.2.
       #:parallel-build? #f
       ;; XXX: Some tests require an audio device.
       #:tests? #f))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("which" ,which)))
    (inputs
     `(("libcap" ,libcap)
       ("pcaudiolib" ,pcaudiolib)))
    (synopsis "Software speech synthesizer")
    (description
     "eSpeak NG is a software speech synthesizer for more than 100 languages.
It is based on the eSpeak engine and supports spectral and Klatt formant
synthesis, and the ability to use MBROLA voices.")
    (license license:gpl3+)))

(define-public mitlm
  (package
    (name "mitlm")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mitlm/mitlm/releases/"
                                  "download/v" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "09fv4fcpmw9g1j0zml0k5kk1lgjw2spr8gn51llbkaaph6v8d62a"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (synopsis "The MIT Language Modeling toolkit")
    (description "The MIT Language Modeling (MITLM) toolkit is a set of
tools designed for the efficient estimation of statistical n-gram language
models involving iterative parameter estimation.  It achieves much of its
efficiency through the use of a compact vector representation of n-grams.")
    (home-page "https://github.com/mitlm/mitlm")
    (license license:expat)))

(define-public speech-dispatcher
  (package
    (name "speech-dispatcher")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/brailcom/speechd/releases"
                                  "/download/" version "/speech-dispatcher-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "16bg52hnkrsrs7kgbzanb34b9zb6fqxwj0a9bmsxmj1skkil1h1p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static"

                           ;; Disable support for proprietary TTS engines.
                           "--with-ibmtts=no"
                           "--with-kali=no" "--with-baratinoo=no")))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dotconf" ,dotconf)
       ("espeak" ,espeak-ng)
       ("glib" ,glib)
       ("libltdl" ,libltdl)
       ("libsndfile" ,libsndfile)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python)))
    (synopsis "Common interface to speech synthesizers")
    (description "The Speech Dispatcher project provides a high-level
device independent layer for access to speech synthesis through a simple,
stable and well documented interface.")
    (home-page "https://devel.freebsoft.org/speechd")
    ;; The software is distributed under GPL2+, but includes a number
    ;; of files covered by other licenses.  Note: in practice, this
    ;; is linked against dotconf, which is LGPL 2.1 only.
    (license (list license:gpl2+
                   license:fdl1.2+ ; Most files in doc/ are dual gpl2+/fdl1.2+.
                   license:lgpl2.1+
                   (license:non-copyleft
                    ;; festival_client.{c,h} carries an expat-style license.
                    "See src/modules/festival_client.c in the distribution.")
                   license:gpl3+)))) ; doc/texinfo.tex -- with TeX exception.

(define-public sonic
  (package
    (name "sonic")
    (version "0.2.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/waywardgeek/sonic")
                    (commit (string-append "release-" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "08xwnpw9cnaix1n1i7gvpq5hrfrqc2z1snjhjapfam506hrc77g4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'respect-LDFLAGS
           (lambda _
             (substitute* "Makefile"
               ((" -o sonic " match)
                (string-append " $(LDFLAGS)" match)))
             #t))
         (delete 'configure))))        ; no ./configure script
    (synopsis "Speed up or slow down speech")
    (description "Sonic implements a simple algorithm for speeding up or slowing
down speech.  However, it's optimized for speed ups of over 2X, unlike previous
algorithms for changing speech rate.  Sonic is a C library designed to be easily
integrated into streaming voice applications such as text-to-speech (TTS) back
ends.

The primary motivation behind Sonic is to enable the blind and visually impaired
to improve their productivity with speech engines, like eSpeak.  Sonic can also
be used by the sighted.")
    (home-page "https://github.com/waywardgeek/sonic")
    (license license:asl2.0)))

(define-public festival
  (package
    (name "festival")
    (version "2.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://festvox.org/packed/festival/"
                                  (version-major+minor version)
                                  "/festival-" version "-release.tar.gz"))
              (sha256
               (base32
                "1d5415nckiv19adxisxfm1l1xxfyw88g87ckkmcr0lhjdd10g42c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there is no test target
       #:make-flags
       (list (string-append "RM="
                            (assoc-ref %build-inputs "coreutils")
                            "/bin/rm")
             (string-append "ECHO_N="
                            (assoc-ref %build-inputs "coreutils")
                            "/bin/printf \"%s\""))
       #:parallel-build? #f ; not supported
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-and-patch-speech-tools
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "-C" ".."
                     "-xf" (assoc-ref inputs "speech-tools"))
             (with-directory-excursion "../speech_tools"
               (substitute* '("config/rules/modules.mak"
                              "config/rules/test_make_rules.mak"
                              "config/make_system.mak")
                 (("/bin/sh") (which "sh"))))
             #t))
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (substitute* '("config/test_make_rules"
                            "config/make_system.mak")
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'build 'build-speech-tools
           (lambda* (#:key configure-flags make-flags #:allow-other-keys)
             (with-directory-excursion "../speech_tools"
               (apply invoke "sh" "configure"
                      (string-append "CONFIG_SHELL=" (which "sh"))
                      (string-append "SHELL=" (which "sh"))
                      configure-flags)
               (apply invoke "make" make-flags))))
         (add-after 'build 'build-documentation
           (lambda _
             (with-directory-excursion "doc"
               (invoke "make" "festival.info"))))
         (add-after 'unpack 'set-installation-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "config/project.mak"
                 (("^FTLIBDIR.*")
                  (string-append "FTLIBDIR=" out "/share/festival/lib")))
               (substitute* "config/systems/default.mak"
                 (("^INSTALL_PREFIX.*")
                  (string-append "INSTALL_PREFIX=" out)))
               #t)))
         (add-after 'install 'actually-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Install Speech Tools first
               (with-directory-excursion "../speech_tools"
                 ;; Target directories
                 (for-each (lambda (dir)
                             (mkdir-p (string-append out dir)))
                           '("/bin"
                             "/lib"
                             "/include/speech_tools/"
                             "/include/speech_tools/instantiate"
                             "/include/speech_tools/ling_class"
                             "/include/speech_tools/rxp"
                             "/include/speech_tools/sigpr"
                             "/include/speech_tools/unix"))
                 ;; Install binaries
                 (for-each (lambda (file)
                             (install-file file (string-append out "/bin")))
                           (find-files "bin" ".*"))
                 (for-each (lambda (file)
                             (delete-file (string-append out "/bin/" file)))
                           '("est_gdb" "est_examples" "est_program"))
                 ;; Install libraries
                 (for-each (lambda (file)
                             (install-file file (string-append out "/lib")))
                           (find-files "lib" "lib.*\\.so.*"))

                 ;; Install headers
                 (for-each
                  (lambda (dir)
                    (for-each
                     (lambda (header)
                       (install-file header
                                     (string-append out "/include/speech_tools/" dir)))
                     (find-files (string-append "include/" dir)
                                 "\\.h$")))
                  '("." "instantiate" "ling_class" "rxp" "sigpr" "unix")))

               ;; Unpack files that will be installed together with the
               ;; Festival libraries.
               (invoke "tar" "--strip-components=1"
                       "-xvf" (assoc-ref inputs "festvox-cmu"))
               (invoke "tar" "--strip-components=1"
                       "-xvf" (assoc-ref inputs "festvox-poslex"))
               (invoke "tar" "--strip-components=1"
                       "-xvf" (assoc-ref inputs "default-voice"))

               ;; Install Festival
               (let ((bin (string-append out "/bin"))
                     (incdir (string-append out "/include/festival"))
                     (share (string-append out "/share/festival"))
                     (info (string-append out "/share/info")))
                 (for-each (lambda (executable)
                             (install-file executable bin))
                           '("src/main/festival"
                             "src/main/festival_client"
                             "examples/benchmark"))
                 (let ((scripts '("examples/dumpfeats"
                                  "examples/durmeanstd"
                                  "examples/latest"
                                  "examples/make_utts"
                                  "examples/powmeanstd"
                                  "examples/run-festival-script"
                                  "examples/saytime"
                                  "examples/scfg_parse_text"
                                  "examples/text2pos"
                                  "examples/text2wave")))
                   (substitute* scripts
                     (("exec /tmp/guix-build.*/bin/festival")
                      (string-append "exec " bin "/festival")))
                   (for-each (lambda (script)
                               (install-file script bin))
                             scripts))

                 ;; Documentation
                 (for-each (lambda (file)
                             (install-file file info))
                           (find-files "doc/info/" "festival.info.*"))

                 ;; Headers
                 (mkdir-p incdir)
                 (for-each (lambda (header)
                             (install-file header
                                           (string-append incdir "/"
                                                          (dirname header))))
                           (find-files "src/include" "\\.h$"))

                 ;; Data
                 (mkdir-p share)
                 (for-each (lambda (file)
                             (install-file file
                                           (string-append share "/"
                                                          (dirname file))))
                           (find-files "lib" ".*"))
                 (for-each delete-file
                           (append (find-files share "Makefile")
                                   (find-files bin "Makefile")))))
             #t))
         (add-after 'actually-install 'install-emacs-mode
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((emacs-dir (string-append (assoc-ref outputs "out")
                                             "/share/emacs/site-lisp")))
               (install-file "lib/festival.el" emacs-dir)
               (emacs-generate-autoloads ,name emacs-dir)
               #t)))
         ;; Rebuild the very old configure script that is confused by extra
         ;; arguments.
         (add-before 'configure 'bootstrap
           (lambda _ (invoke "autoreconf" "-vif"))))))
    (inputs
     `(("ncurses" ,ncurses)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("texinfo" ,texinfo)
       ("emacs" ,emacs-minimal)
       ("festvox-cmu"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://festvox.org/packed/festival/"
                               (version-major+minor version)
                               "/festlex_CMU.tar.gz"))
           (sha256
            (base32
             "01vwidqhhg2zifvk1gby91mckg1z2pv2mj6lihvdaifakf8k1561"))))
       ("festvox-poslex"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://festvox.org/packed/festival/"
                               (version-major+minor version)
                               "/festlex_POSLEX.tar.gz"))
           (sha256
            (base32
             "18wywilxaqwy63lc47p5g5529mpxhslibh1bjij0snxx5mjf7ip7"))))
       ("default-voice"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://festvox.org/packed/festival/"
                               (version-major+minor version)
                               "/voices/festvox_kallpc16k.tar.gz"))
           (sha256
            (base32
             "136hmsyiwnlg2qwa508dy0imf19mzrb5r3dmb2kg8kcyxnslm740"))))
       ("speech-tools"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://festvox.org/packed/festival/"
                               (version-major+minor version)
                               "/speech_tools-" version "-release.tar.gz"))
           (sha256
            (base32
             "1k2xh13miyv48gh06rgsq2vj25xwj7z6vwq9ilsn8i7ig3nrgzg4"))))))
    (home-page "https://www.cstr.ed.ac.uk/projects/festival/")
    (synopsis "Speech synthesis system")
    (description "Festival offers a general framework for building speech
synthesis systems as well as including examples of various modules.  As a
whole it offers full text to speech through a number APIs: from shell level,
though a Scheme command interpreter, as a C++ library, from Java, and an Emacs
interface.  Festival is multi-lingual though English is the most advanced.
The system is written in C++ and uses the Edinburgh Speech Tools Library for
low level architecture and has a Scheme (SIOD) based command interpreter for
control.")
    (license (license:non-copyleft "file://COPYING"))))

(define-public ekho
  (package
    (name "ekho")
    (version "8.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/e-guidedog/Ekho/"
                       version "/ekho-" version ".tar.xz"))
       (sha256
        (base32 "1v476kpw09ljj8mavasj4hya2w11jwlx7q22rh1lsn9jkkam5i2a"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("espeak-ng" ,espeak-ng)
       ("libsndfile" ,libsndfile)
       ("pulseaudio" ,pulseaudio)))
    (build-system gnu-build-system)
    (native-search-paths
     (list (search-path-specification
            (variable "EKHO_DATA_PATH")
            (files '("share/ekho-data")))))
    (home-page "https://eguidedog.net/ekho.php")
    (synopsis "Chinese text-to-speech software")
    (description
     "Ehko is a Text-To-Speech (TTS) software.  It supports Cantonese,
Mandarin, Toisanese, Zhaoan Hakka, Tibetan, Ngangien and Korean (in trial).
It can also speak English through eSpeak or Festival.")
    (license (list license:gpl2+
                   ;; libmusicxml
                   license:mpl2.0))))

(define-public sphinxbase
  (package
    (name "sphinxbase")
    (version "5prealpha")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/cmusphinx/"
                           "sphinxbase/" version "/"
                           "sphinxbase-" version ".tar.gz"))
       (sha256
        (base32 "0vr4k8pv5a8nvq9yja7kl13b5lh0f9vha8fc8znqnm8bwmcxnazp"))
       (patches (search-patches "sphinxbase-fix-doxygen.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-tests? #f))           ;tests fail otherwise
    (native-inputs
     `(("bison" ,bison)
       ("doxygen" ,doxygen)
       ("perl" ,perl)                   ;for tests
       ("python" ,python)
       ("swig" ,swig)))
    (inputs
     `(("pulseaudio" ,pulseaudio)))
    (home-page "https://cmusphinx.github.io/")
    (synopsis "Support library required by Pocketsphinx and Sphinxtrain")
    (description "This package contains the basic libraries shared by
the CMU Sphinx trainer and all the Sphinx decoders (Sphinx-II,
Sphinx-III, and PocketSphinx), as well as some common utilities for
manipulating acoustic feature and audio files.")
    (license license:bsd-4)))

(define-public pocketsphinx
  (package
    (name "pocketsphinx")
    (version "5prealpha")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/cmusphinx/"
                           "pocketsphinx/" version "/"
                           "pocketsphinx-" version ".tar.gz"))
       (sha256
        (base32 "1n9yazzdgvpqgnfzsbl96ch9cirayh74jmpjf7svs4i7grabanzg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)                   ;for tests
       ("python" ,python)
       ("swig" ,swig)))
    (inputs
     `(("gstreamer" ,gstreamer)
       ("libcap" ,libcap)
       ("pulseaudio" ,pulseaudio)
       ("sphinxbase" ,sphinxbase)))
    (home-page "https://cmusphinx.github.io/")
    (synopsis "Recognizer library written in C")
    (description "PocketSphinx is one of Carnegie Mellon University's
large vocabulary, speaker-independent continuous speech recognition
engine.")
    (license license:bsd-2)))
