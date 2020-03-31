;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages ntp)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public chrony
  (package
    (name "chrony")
    (version "3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.tuxfamily.org/chrony/"
                           "chrony-" version ".tar.gz"))
       (sha256
        (base32 "1d9r2dhslll4kzdmxrj0qfgwq1b30d4l3s5cwr8yr93029dpj0jf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((srfi srfi-26)
                  (guix build utils)
                  (guix build gnu-build-system))
       #:configure-flags
       (list "--enable-scfilter"
             "--with-sendmail=sendmail"
             "--with-user=chrony")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'stay-inside-out
           ;; Simply setting CHRONYVARDIR to something nonsensical at install
           ;; time would result in nonsense file names in man pages.
           (lambda _
             (substitute* "Makefile.in"
               (("mkdir -p \\$\\(DESTDIR\\)\\$\\(CHRONYVARDIR\\)") ":"))
             #t))
         (add-after 'install 'install-more-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (for-each (cut install-file <> doc)
                         (list "README" "FAQ"))
               (copy-recursively "examples" (string-append doc "/examples"))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libcap" ,libcap)
       ("libseccomp" ,libseccomp)
       ("nettle" ,nettle)))
    (home-page "https://chrony.tuxfamily.org/")
    (synopsis "System clock synchronisation service that speaks NTP")
    (description
     "Chrony keeps your system time accurate.  It synchronises your computer's
clock with @acronym{NTP, Network Time Protocol} servers, reference clocks such
as GPS receivers, or even manual input of the correct time from a wristwatch.

Chrony will determine the rate at which the computer gains or loses time, and
compensate for it.  It can also operate as an NTPv4 (RFC 5905) server and peer
to tell time to other computers on the network.

It's designed to perform well even under adverse conditions: congested
networks, unreliable clocks drifting with changes in temperature, and devices
or virtual machines that are frequently turned off and connect to the Internet
for only a few minutes at a time.

Typical accuracy when synchronised over the Internet is several milliseconds.
On a local network this can reach tens of microseconds.  With hardware
time-stamping or reference clock, sub-microsecond accuracy is possible.")
    (license l:gpl2)))

(define-public ntp
  (package
   (name "ntp")
   (version "4.2.8p14")
   (source
     (origin
       (method url-fetch)
       (uri (list (string-append
                    "http://archive.ntp.org/ntp4/ntp-"
                    (version-major+minor version)
                    "/ntp-" version ".tar.gz")
                  (string-append
                    "https://www.eecis.udel.edu/~ntp/ntp_spool/ntp4/ntp-"
                    (version-major+minor version)
                    "/ntp-" version ".tar.gz")))
       (sha256
        (base32 "1dsfbrad5adwjnm3k0y0ip8dzs7r2nmw66vjil8gvapnh7qf8q0r"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove the bundled copy of libevent, but we must keep
           ;; sntp/libevent/build-aux since configure.ac contains
           ;; AC_CONFIG_AUX_DIR([sntp/libevent/build-aux])
           (rename-file "sntp/libevent/build-aux"
                        "sntp/libevent:build-aux")
           (delete-file-recursively "sntp/libevent")
           (mkdir "sntp/libevent")
           (rename-file "sntp/libevent:build-aux"
                        "sntp/libevent/build-aux")
           #t))))
   (native-inputs `(("which" ,which)
                    ("pkg-config" ,pkg-config)))
   (inputs
    `(("openssl" ,openssl)
      ("libevent" ,libevent)
      ;; Build with POSIX capabilities support on GNU/Linux.  This allows 'ntpd'
      ;; to run as non-root (when invoked with '-u'.)
      ,@(if (string-suffix? "-linux"
                            (or (%current-target-system) (%current-system)))
            `(("libcap" ,libcap))
            '())))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'disable-network-test
                   (lambda _
                     (substitute* "tests/libntp/Makefile.in"
                       (("test-decodenetnum\\$\\(EXEEXT\\) ") ""))
                     #t)))))
   (build-system gnu-build-system)
   (synopsis "Real time clock synchronization system")
   (description "NTP is a system designed to synchronize the clocks of
computers over a network.")
   (license (l:x11-style
             "https://www.eecis.udel.edu/~mills/ntp/html/copyright.html"
             "A non-copyleft free licence from the University of Delaware"))
   (home-page "https://www.ntp.org")))

(define-public openntpd
  (package
    (name "openntpd")
    (version "6.2p3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://openbsd/OpenNTPD/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fn12i4kzsi0zkr4qp3dp9bycmirnfapajqvdfx02zhr4hanj0kv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags `( "--with-privsep-user=ntpd"
                            "--localstatedir=/var"
                            ,(string-append "--with-cacert="
                                            (assoc-ref %build-inputs "libressl")
                                            "/etc/ssl/cert.pem"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'modify-install-locations
           (lambda _
             ;; Don't try to create /var/run or /var/db
             (substitute* "src/Makefile.in"
               (("DESTDIR\\)\\$\\(localstatedir") "TMPDIR"))
             #t)))))
    (inputs
     `(("libressl" ,libressl))) ; enable TLS time constraints. See ntpd.conf(5).
    (home-page "http://www.openntpd.org/")
    (synopsis "NTP client and server by the OpenBSD Project")
    (description "OpenNTPD is the OpenBSD Project's implementation of a client
and server for the Network Time Protocol.  Its design goals include being
secure, easy to configure, and accurate enough for most purposes, so it's more
minimalist than ntpd.")
    ;; A few of the source files are under bsd-3.
    (license (list l:isc l:bsd-3))))

(define-public tlsdate
  (package
    (name "tlsdate")
    (version "0.0.13")
    (home-page "https://github.com/ioerror/tlsdate")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (commit (string-append "tlsdate-" version))
                    (url home-page)))
              (sha256
               (base32
                "0w3v63qmbhpqlxjsvf4k3zp90k6mdzi8cdpgshan9iphy1f44xgl"))
              (file-name (string-append name "-" version "-checkout"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Disable seccomp when it's not supported--e.g., on aarch64.  See
       ;; 'src/seccomp.c' for the list of supported systems.
       #:configure-flags ,(if (any (lambda (system)
                                     (string-contains (or
                                                       (%current-target-system)
                                                       (%current-system))
                                                      system))
                                   '("x86_64" "i686" "arm"))
                              ''()
                              ''("--disable-seccomp-filter"))

       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'autogen
                    (lambda _
                      ;; The ancestor of 'SOURCE_DATE_EPOCH'; it contains the
                      ;; date that is recorded in binaries.  It must be a
                      ;; "recent date" since it is used to detect bogus dates
                      ;; received from servers.
                      (setenv "COMPILE_DATE" (number->string 1530144000))
                      (invoke "sh" "autogen.sh"))))))
    (inputs `(("openssl" ,openssl-1.0)
              ("libevent" ,libevent)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool" ,libtool)))
    (synopsis "Extract remote time from TLS handshakes")
    (description
     "@command{tlsdate} sets the local clock by securely connecting with TLS
to remote servers and extracting the remote time out of the secure handshake.
Unlike ntpdate, @command{tlsdate} uses TCP, for instance connecting to a
remote HTTPS or TLS enabled service, and provides some protection against
adversaries that try to feed you malicious time information.")
    (license l:bsd-3)))
