;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2018, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018, 2019, 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 André Batista <nandre@riseup.net>
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

(define-module (gnu packages tor)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages w3m))

(define-public tor
  (package
    (name "tor")
    (version "0.4.5.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://dist.torproject.org/tor-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0x7hhl0svfc4yh9xvq7kkzgmwjcw1ak9i0794wjg4biy2fmclzs4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-lzma"
             "--enable-zstd")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'skip-practracker
           ;; This is a style linter.  It doesn't get to throw fatal errors.
           (lambda _
             (setenv "TOR_DISABLE_PRACTRACKER" "set")
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)))             ; for tests
    (inputs
     `(("libevent" ,libevent)
       ("libseccomp" ,libseccomp)
       ("openssl" ,openssl)
       ("xz" ,xz)
       ("zlib" ,zlib)
       ("zstd" ,zstd "lib")))
    (home-page "https://www.torproject.org/")
    (synopsis "Anonymous network router to improve privacy on the Internet")
    (description
     "Tor protects you by bouncing your communications around a distributed
network of relays run by volunteers all around the world: it prevents
somebody watching your Internet connection from learning what sites you
visit, and it prevents the sites you visit from learning your physical
location.  Tor works with many of your existing applications, including
web browsers, instant messaging clients, remote login, and other
applications based on the TCP protocol.

This package is the full featured @code{tor} which is needed for running
relays, bridges or directory authorities. If you just want to access the Tor
network or to setup an onion service you may install @code{tor-client}
instead.")
    (license license:bsd-3)))

(define-public tor-client
  (package
    (inherit tor)
    (name "tor-client")
    (arguments
     (substitute-keyword-arguments (package-arguments tor)
       ((#:configure-flags flags)
        (append flags
                '("--disable-module-relay")))))
    (synopsis "Client to the anonymous Tor network")
    (description
     "Tor protects you by bouncing your communications around a distributed
network of relays run by volunteers all around the world: it prevents
somebody watching your Internet connection from learning what sites you
visit, and it prevents the sites you visit from learning your physical
location.  Tor works with many of your existing applications, including
web browsers, instant messaging clients, remote login, and other
applications based on the TCP protocol.

To @code{torify} applications (to take measures to ensure that an application,
which has not been designed for use with Tor such as ssh, will use only Tor for
internet connectivity, and also ensures that there are no leaks from DNS, UDP or
the application layer) you need to install @code{torsocks}.

This package only provides a client to the Tor Network.")))

(define-public torsocks
  (package
    (name "torsocks")
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://people.torproject.org/~dgoulet/"
                                  "torsocks/torsocks-" version ".tar.xz"))
              (sha256
               (base32
                "08inrkap29gikb6sdmb58z43hw4abwrfw7ny40c4xzdkss0vkwdr"))))
    (build-system gnu-build-system)
    (inputs
     `(("libcap" ,libcap)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'build 'absolutize
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "src/bin/torsocks"
                        (("getcap=.*")
                         (string-append "getcap=" (which "getcap") "\n")))
                      #t)))))
    (home-page "https://www.torproject.org/")
    (synopsis "Use socks-friendly applications with Tor")
    (description
     "Torsocks allows you to use most socks-friendly applications in a safe
way with Tor.  It ensures that DNS requests are handled safely and explicitly
rejects UDP traffic from the application you're using.")

    ;; All the files explicitly say "version 2 only".
    (license license:gpl2)))

(define-public privoxy
  (package
    (name "privoxy")
    (version "3.0.32")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/ijbswa/Sources/"
                                 version "%20%28stable%29/privoxy-"
                                 version "-stable-src.tar.gz"))
             (sha256
              (base32
               "1mzfxwnvnf1jkvfcrsivm6mjwdzjrc3h89qziz0mwi32ih0f87f6"))))
    (build-system gnu-build-system)
    (arguments
     '(;; The default 'sysconfdir' is $out/etc; change that to
       ;; $out/etc/privoxy.
       #:configure-flags (list (string-append "--sysconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc/privoxy")
                               "--localstatedir=/var"
                               "--with-brotli"
                               "--with-openssl")
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-default-logging
           (lambda _
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               ;; Do not create /var/run nor /var/log/privoxy/logfile.
               (substitute* "GNUmakefile.in"
                 (("(logfile \\|\\| exit )1" _ match)
                  (string-append match "0"))
                 (("(\\$\\(DESTDIR\\)\\$\\(SHARE_DEST\\)) \\\\" _ match)
                  match)
                 ((".*\\$\\(LOG_DEST\\) \\$\\(DESTDIR\\)\\$\\(PID_DEST\\).*")
                  ""))
               ;; Disable logging in the default configuration to allow for
               ;; non-root users using it as is.
               (substitute* "config"
                 (("^logdir") "#logdir")
                 (("^logfile") "#logfile")))
             #t)))))
    (inputs
     `(("brotli" ,brotli)
       ("openssl" ,openssl)
       ("pcre" ,pcre)
       ("w3m" ,w3m)
       ("zlib" ,zlib)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (home-page "https://www.privoxy.org")
    (synopsis "Web proxy with advanced filtering capabilities for enhancing privacy")
    (description
     "Privoxy is a non-caching web proxy with advanced filtering capabilities
for enhancing privacy, modifying web page data and HTTP headers, controlling
access, and removing ads and other obnoxious Internet junk.  Privoxy has a
flexible configuration and can be customized to suit individual needs and
tastes.  It has application for both stand-alone systems and multi-user
networks.")
    (license license:gpl2+)))

(define-public onionshare-cli
  (package
    (name "onionshare-cli")
    (version "2.3.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/micahflee/onionshare")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1llvnvb676s2cs6a4y7isxdj75ddfvskw1p93v5m35vsw7f72kqz"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (inputs
     ;; TODO: obfs4proxy
     `(("python-click" ,python-click)
       ("python-eventlet" ,python-eventlet)
       ("python-flask" ,python-flask)
       ("python-flask-httpauth" ,python-flask-httpauth)
       ("python-flask-socketio" ,python-flask-socketio)
       ("python-psutil" ,python-psutil)
       ("python-pycryptodome" ,python-pycryptodome)
       ("python-pysocks" ,python-pysocks)
       ("python-requests" ,python-requests)
       ("python-stem" ,python-stem)
       ("python-unidecode" ,python-unidecode)
       ("python-urllib3" ,python-urllib3)
       ("tor" ,tor)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bake-tor
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (list "cli/onionshare_cli/common.py"
                                "desktop/src/onionshare/gui_common.py")
               (("shutil\\.which\\(\\\"tor\\\"\\)")
                (string-append "\"" (which "tor") "\"")))
             #t))
         (add-before 'build 'change-directory
           (lambda _ (chdir "cli") #t))
         (replace 'check
           (lambda _
             (setenv "HOME" "/tmp")
             ;; Greendns is not needed for testing, and if eventlet tries to
             ;; load it, an OSError is thrown when getprotobyname is called.
             ;; Thankfully there is an environment variable to disable the
             ;; greendns import, so use it:
             (setenv "EVENTLET_NO_GREENDNS" "yes")
             (invoke "pytest" "-v" "./tests"))))))
    (home-page "https://onionshare.org/")
    (synopsis "Securely and anonymously share files")
    (description "OnionShare lets you securely and anonymously share files,
host websites, and chat with friends using the Tor network.

This package contains @code{onionshare-cli}, a command-line interface to
OnionShare.")
    ;; Bundled, minified jquery and socket.io are expat licensed.
    (license (list license:gpl3+ license:expat))))

(define-public onionshare
  (package (inherit onionshare-cli)
    (name "onionshare")
    (arguments
     (substitute-keyword-arguments (package-arguments onionshare-cli)
      ((#:phases phases)
       `(modify-phases ,phases
         (replace 'change-directory
           (lambda _ (chdir "desktop/src") #t))
         (add-after 'unpack 'patch-tests
           (lambda _
             ;; Disable tests that require starting servers, which will hang
             ;; during build:
             ;; - test_autostart_and_autostop_timer_mismatch
             ;; - test_autostart_timer
             ;; - test_autostart_timer_too_short
             ;; - test_autostop_timer_too_short
             (substitute* "desktop/tests/test_gui_share.py"
               (("( *)def test_autost(art|op)_(timer(_too_short)?|and_[^(]*)\\(" & >)
                (string-append > "@pytest.mark.skip\n" &)))
             ;; - test_13_quit_with_server_started_should_warn
             (substitute* "desktop/tests/test_gui_tabs.py"
               (("( *)def test_13" & >)
                (string-append > "@pytest.mark.skip\n" &)))
             ;; Remove multiline load-path adjustment, so that onionshare-cli
             ;; modules are loaded from input
             (use-modules (ice-9 regex)
                          (ice-9 rdelim))
             (with-atomic-file-replacement "desktop/tests/conftest.py"
               (let ((start-rx (make-regexp "^# Allow importing")))
                 (lambda (in out)
                   (let loop ()
                     (let ((line (read-line in 'concat)))
                       (if (regexp-exec start-rx line)
                           (begin      ; slurp until closing paren
                             (let slurp ()
                               (let ((line (read-line in 'concat)))
                                 (if (string=? line ")\n")
                                     (dump-port in out) ; done
                                     (slurp)))))
                           (begin
                             (display line out)
                             (loop))))))))))
         (replace 'check
           (lambda _
             ;; Some tests need a writable homedir:
             (setenv "HOME" "/tmp")
             ;; Ensure installed modules can be found:
             (setenv "PYTHONPATH"
                     (string-append %output "/lib/python"
                                    ,(version-major+minor (package-version python))
                                    "/site-packages:"
                                    (getenv "PYTHONPATH")))
             ;; Avoid `getprotobyname` issues:
             (setenv "EVENTLET_NO_GREENDNS" "yes")
             ;; Make Qt render "offscreen":
             (setenv "QT_QPA_PLATFORM" "offscreen")
             ;; Must be run from "desktop" dir:
             (chdir "..")
             (invoke "./tests/run.sh")))
         (add-after 'install 'install-data
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share")))
               (install-file "org.onionshare.OnionShare.svg"
                             (string-append share "/icons/hicolor/scalable/apps"))
               (install-file "org.onionshare.OnionShare.desktop"
                             (string-append share "/applications"))
               #t)))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (inputs
     ;; TODO: obfs4proxy
     `(("onionshare-cli" ,onionshare-cli)
       ("python-shiboken-2" ,python-shiboken-2)
       ("python-pyside-2" ,python-pyside-2)
       ("python-qrcode" ,python-qrcode)
       ;; The desktop client uses onionshare-cli like a python module.  But
       ;; propagating onionshare-cli's inputs is not great, since a user would
       ;; not expect to have those installed when using onionshare-cli as a
       ;; standalone utility.  So add onionshare-cli's inputs here.
       ,@(package-inputs onionshare-cli)))
    (description "OnionShare lets you securely and anonymously share files,
host websites, and chat with friends using the Tor network.")))

(define-public nyx
  (package
    (name "nyx")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32
         "02rrlllz2ci6i6cs3iddyfns7ang9a54jrlygd2jw1f9s6418ll8"))))
    (build-system python-build-system)
    (inputs
     `(("python-stem" ,python-stem)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-man-page
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (install-file "nyx.1" (string-append man "/man1"))
               #t)))
         (add-after 'install 'install-sample-configuration
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "web/nyxrc.sample" doc)
               #t))))
       ;; XXX The tests seem to require more of a real terminal than the build
       ;; environment provides:
       ;;   _curses.error: setupterm: could not find terminal
       ;; With TERM=linux, the tests try to move the cursor and still fail:
       ;;   _curses.error: cbreak() returned ERR
       #:tests? #f))
    (home-page "https://nyx.torproject.org/")
    (synopsis "Tor relay status monitor")
    (description
     "Nyx monitors the performance of relays participating in the
@uref{https://www.torproject.org/, Tor anonymity network}.  It displays this
information visually and in real time, using a curses-based terminal interface.
This makes Nyx well-suited for remote shell connections and servers without a
graphical display.  It's like @command{top} for Tor, providing detailed
statistics and status reports on:

@enumerate
@item connections (with IP address, hostname, fingerprint, and consensus data),
@item bandwidth, processor, and memory usage,
@item the relay's current configuration,
@item logged events,
@item and much more.
@end enumerate

Potential client and exit connections are scrubbed of sensitive information.")
    (license license:gpl3+)))
