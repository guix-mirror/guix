;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 ng0 <ng0@infotropique.org>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
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
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages w3m))

(define-public tor
  (package
    (name "tor")
    (version "0.3.1.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://dist.torproject.org/tor-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "09ixizsr635qyshvrn1m5asjkaz4fm8dx80lc3ajyy0fi7vh86vf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-gcc-hardening"
                               "--enable-linker-hardening")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-2)))  ; for tests
    (inputs
     `(("zlib" ,zlib)
       ("openssl" ,openssl)
       ("libevent" ,libevent)
       ("libseccomp" ,libseccomp)
       ("xz" ,xz)
       ("zstd" ,zstd)))
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

To @code{torify} applications (to take measures to ensure that an application,
which has not been designed for use with Tor such as ssh, will use only Tor for
internet connectivity, and also ensures that there are no leaks from DNS, UDP or
the application layer) you need to install @code{torsocks}.")
    (license license:bsd-3)))

(define-public torsocks
  (package
    (name "torsocks")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://people.torproject.org/~dgoulet/"
                                  name "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0byr9ga9w79qz4vp0m11sbmspad7fsal9wm67r4znzb7zb7cis19"))))
    (build-system gnu-build-system)
    (inputs
     `(("which" ,which)
       ("libcap" ,libcap)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'build 'absolutize
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "src/bin/torsocks"
                        (("getcap=`.*`")
                         (string-append "getcap=" (which "getcap")))
                        (("`which")
                         (string-append "`" (which "which"))))
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
    (version "3.0.26")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/ijbswa/Sources/"
                                 version "%20%28stable%29/privoxy-"
                                 version "-stable-src.tar.gz"))
             (sha256
              (base32
               "1n4wpxmahl8m2y3d1azxa8lrdbpaad007k458skxrpz57ss1br2p"))))
    (build-system gnu-build-system)
    (arguments
     '(;; The default 'sysconfdir' is $out/etc; change that to
       ;; $out/etc/privoxy.
       #:configure-flags (list (string-append "--sysconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc/privoxy"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoconf
           (lambda _
             ;; Unfortunately, this is not a tarball produced by
             ;; "make dist".
             (zero? (system* "autoreconf" "-vfi")))))
       #:tests? #f))
    (inputs
     `(("w3m" ,w3m)
       ("pcre" ,pcre)
       ("zlib" ,zlib)
       ("autoconf" ,autoconf)
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

(define-public onionshare
  (package
    (name "onionshare")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/micahflee/onionshare/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "02iv7dg15da57gy3zvfchnwwpr21n1gva7mqwpwr958ni2034smk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out        (assoc-ref outputs "out"))
                    (onionshare (string-append out "/share/onionshare")))
               (substitute* "onionshare/strings.py"
                 ;; correct the locale directory
                 (("helpers.get_resource_path\\('locale'\\)")
                  (string-append "'" onionshare "/locale'")))
               (substitute* "onionshare/helpers.py"
                 ;; correct the location of version.txt
                 (("get_resource_path\\('version.txt'\\)")
                  (string-append "'" onionshare "/version.txt'"))
                 (("get_resource_path\\('wordlist.txt'\\)")
                  (string-append "'" onionshare "/wordlist.txt'")))
               (substitute* "onionshare/web.py"
                 ;; fix the location of the html files
                 (("helpers.get_resource_path\\('html/denied.html'\\)")
                  (string-append "'" onionshare "/html/denied.html'"))
                 (("helpers.get_resource_path\\('html/404.html'\\)")
                  (string-append "'" onionshare "/html/404.html'"))
                 (("helpers.get_resource_path\\('html/index.html'\\)")
                  (string-append "'" onionshare "/html/index.html'")))
               (substitute* "onionshare_gui/file_selection.py"
                 ;; fancy box image in the GUI
                 (("helpers.get_resource_path\\('images/drop_files.png'\\)")
                  (string-append "'" onionshare "/images/drop_files.png'")))
               (substitute* "onionshare_gui/server_status.py"
                 (("helpers.get_resource_path\\('images/server_stopped.png'\\)")
                  (string-append "'" onionshare "/images/server_stopped.png'"))
                 (("helpers.get_resource_path\\('images/server_working.png'\\)")
                  (string-append "'" onionshare "/images/server_working.png'"))
                 (("helpers.get_resource_path\\('images/server_started.png'\\)")
                  (string-append "'" onionshare "/images/server_started.png'")))
               (substitute* "onionshare_gui/onionshare_gui.py"
                  ;; for the icon on the GUI
                 (("helpers.get_resource_path\\('images/logo.png'\\)")
                  (string-append "'" onionshare "/images/logo.png'")))
               (substitute* '("setup.py" "onionshare/helpers.py")
                 (("sys.prefix,") (string-append "'" out "',")))
               (substitute* "setup.py"
                 ;; for the nautilus plugin
                 (("/usr/share/nautilus") "share/nautilus"))
             #t)))
         (delete 'check)
         (add-before 'strip 'tests
           ;; After all the patching we run the tests after installing.
           ;; This is also a known issue:
           ;; https://github.com/micahflee/onionshare/issues/284
           (lambda _ (zero? (system* "nosetests" "test")))))))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (inputs
     `(("python-flask" ,python-flask)
       ("python-nautilus" ,python-nautilus)
       ("python-sip" ,python-sip)
       ("python-stem" ,python-stem)
       ("python-pyqt" ,python-pyqt)))
    (home-page "https://onionshare.org/")
    (synopsis "Securely and anonymously share files")
    (description "OnionShare lets you securely and anonymously share files of
any size.  It works by starting a web server, making it accessible as a Tor
hidden service, and generating an unguessable URL to access and download the
files.  It doesn't require setting up a server on the internet somewhere or
using a third party filesharing service.  You host the file on your own computer
and use a Tor hidden service to make it temporarily accessible over the
internet.  The other user just needs to use Tor Browser to download the file
from you.")
    (license (list license:gpl3+
                   license:bsd-3))))    ; onionshare/socks.py

(define-public nyx
  ;; The last ‘arm’ relase was 5 years ago.  Meanwhile, python3 support has
  ;; been added and the software was renamed to ‘nyx’.
  (let ((commit "fea209127484d9b304b908a4711c9528b1d065bc")
        (revision "1"))                 ; Guix package revision
    (package
      (name "nyx")
      (version (string-append "1.9-"
                              revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (file-name (string-append name "-" version "-checkout"))
         (uri (git-reference
               (url "https://git.torproject.org/nyx.git")
               (commit commit)))
         (sha256
          (base32
           "1g0l4988076xg5gs0x0nxzlg58rfx5g5agmklvyh4yp03vxncdb9"))))
      (build-system python-build-system)
      (native-inputs
       `(("python-mock" ,python-mock)
         ("python-pep8" ,python-pep8)
         ("python-pyflakes" ,python-pyflakes)))
      (inputs
       `(("python-stem" ,python-stem)))
      (arguments
       `(#:configure-flags
         (list (string-append "--man-page="
                              (assoc-ref %outputs "out")
                              "/share/man/man1/nyx.1")
               (string-append "--sample-path="
                              (assoc-ref %outputs "out")
                              "/share/doc/nyx/nyxrc.sample"))
         #:use-setuptools? #f           ; setup.py still uses distutils
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda _
               (zero? (system* "./run_tests.py" "--unit")))))))
      ;; A Nyx home page is ‘being worked on’.  Use Arm's for now, which at
      ;; least mentions the new source repository:
      (home-page "http://www.atagar.com/arm/")
      (synopsis "Tor relay status monitor")
      (description "Nyx (formerly Anonymizing Relay Monitor or \"arm\")
monitors the performance of relays participating in the
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
      (license license:gpl3+))))
