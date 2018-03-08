;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015, 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Peter Feigl <peter.feigl@nexoid.at>
;;; Copyright © 2016 John J. Foerch <jjfoerch@earthlink.net>
;;; Copyright © 2016, 2017 ng0 <contact.ng0@cryptolab.net>
;;; Copyright © 2016, 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2017 Ethan R. Jones <doubleplusgood23@gmail.com>
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages admin)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages mcrypt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages man)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xml))

(define-public aide
  (package
    (name "aide")
    (version "0.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/aide/aide/"
                                  version "/aide-" version ".tar.gz"))
              (sha256
               (base32
                "0ibkv4z2gk14fn014kq13rp2ysiq6nn2cflv2q5i7zf466hm6758"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("libgpg-error" ,libgpg-error)
       ("libmhash" ,libmhash)
       ("pcre" ,pcre)
       ("zlib" ,zlib)))
    (synopsis "File and directory integrity checker")
    (description
     "AIDE (Advanced Intrusion Detection Environment) is a file and directory
integrity checker.  It creates a database from the regular expression rules
that it finds from its configuration files.  Once this database is initialized
it can be used to verify the integrity of the files.  It has several message
digest algorithms that are used to check the integrity of files.  All of the
usual file attributes can be checked for inconsistencies.")
    (home-page "http://aide.sourceforge.net/")
    (license license:gpl2+)))

(define-public progress
  (package
    (name "progress")
    (version "0.13.1")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/Xfennec/"
                          name "/archive/v" version ".tar.gz"))
      (sha256
       (base32 "199rk6608q9m6l0fbjm0xl2w1c5krf8245dqnksdp4rqp7l9ak06"))
      (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("ncurses" ,ncurses)))
    (arguments
     `(#:tests? #f ; There is no test suite.
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; There's no configure phase.
    (home-page "https://github.com/Xfennec/progress")
    (synopsis "Program to view the progress of the coreutils commands")
    (description "A program that looks for coreutils basic commands (cp, mv,
dd, tar, gzip/gunzip, cat, etc.) currently running on your system and displays
the percentage of copied data.  It can also show estimated time and throughput,
and provides a \"top-like\" mode (monitoring).")
    (license license:gpl3+)))

(define-public shepherd
  (package
    (name "shepherd")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://alpha.gnu.org/gnu/dmd/shepherd-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "174q1qg7yg6w1hfvlfv720hr6hid4h5xzw15y3ycfpspllzldhcb"))
              (patches (search-patches "shepherd-close-fds.patch"
                                       "shepherd-herd-status-sorted.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--localstatedir=/var")))
    (native-inputs
     `(("pkg-config" ,pkg-config)

       ;; This is the Guile we use as a cross-compiler...
       ("guile" ,guile-2.2)))
    (inputs
     ;; ... and this is the one that appears in shebangs when cross-compiling.
     `(("guile" ,guile-2.2)

       ;; The 'shepherd' command uses Readline when used interactively.  It's
       ;; an unusual use case though, so we don't propagate it.
       ("guile-readline" ,guile-readline)))
    (synopsis "System service manager")
    (description
     "The GNU Shepherd is a daemon-managing daemon, meaning that it supervises
the execution of system services, replacing similar functionality found in
typical init systems.  It provides dependency-handling through a convenient
interface and is based on GNU Guile.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/shepherd/")
    (properties '((ftp-server . "alpha.gnu.org")))))

(define-public daemontools
  (package
    (name "daemontools")
    (version "0.76")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cr.yp.to/" name "/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "07scvw88faxkscxi91031pjkpccql6wspk4yrlnsbrrb5c0kamd5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; No tests as far as I can tell.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir ,(string-append  name "-" version))))
         (delete 'configure)
         (add-before 'build 'patch
           (lambda _
             (substitute* "src/error.h"
               (("extern int errno;")
                "#include <errno.h>"))))
         (replace 'build
           (lambda _
             (invoke "package/compile")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "command"))))))))
    (synopsis "Tools for managing UNIX style services")
    (description
     "@code{daemontools} is a collection of tools for managing UNIX
services.")
    (license license:public-domain)
    (home-page "https://cr.yp.to/daemontools.html")))

(define-public dfc
  (package
   (name "dfc")
   (version "3.0.4")
   (source
    (origin
     (method url-fetch)
      (uri (string-append
            "http://projects.gw-computing.net/attachments/download/79/dfc-"
            version ".tar.gz"))
      (sha256
       (base32
        "0zk1ppx93ijimf4sbgqilxxikpsa2gmpbynknyh41xy7jbdjxp0b"))))
   (build-system cmake-build-system)
   (arguments '(#:tests? #f)) ; There are no tests.
   (native-inputs `(("gettext" ,gettext-minimal)))
   (home-page "http://projects.gw-computing.net/projects/dfc")
   (synopsis "Display file system space usage using graphs and colors")
   (description
    "dfc (df color) is a modern version of df.  It uses colors, draws pretty
graphs and can export its output to different formats.")
   (license license:bsd-3)))

(define-public htop
  (package
   (name "htop")
   (version "2.1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://hisham.hm/htop/releases/"
                  version "/htop-" version ".tar.gz"))
            (sha256
             (base32
              "0j07z0xm2gj1vzvbgh4323k4db9mr7drd7gw95mmpqi61ncvwq1j"))
            (patches (search-patches "htop-fix-process-tree.patch"))))
   (build-system gnu-build-system)
   (inputs
    `(("ncurses" ,ncurses)))
   (native-inputs
    `(("python" ,python-minimal-wrapper))) ; for scripts/MakeHeader.py
   (home-page "https://hisham.hm/htop/")
   (synopsis "Interactive process viewer")
   (description
    "This is htop, an interactive process viewer.  It is a text-mode
application (for console or X terminals) and requires ncurses.")
   (license license:gpl2)))

(define-public pies
  (package
    (name "pies")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/pies/pies-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "12r7rjjyibjdj08dvwbp0iflfpzl4s0zhn6cr6zj3hwf9gbzgl1g"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'patch-/bin/sh
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Use the right shell when executing user-provided
                      ;; shell commands.
                      (let ((bash (assoc-ref inputs "bash")))
                        (substitute* "src/progman.c"
                          (("\"/bin/sh\"")
                           (string-append "\"" bash "/bin/sh\"")))
                        #t))))))
    (home-page "https://www.gnu.org/software/pies/")
    (synopsis "Program invocation and execution supervisor")
    (description
     "GNU pies is a program that supervises the invocation and execution of
other programs.  It reads the list of programs to be started from its
configuration file, executes them, and then monitors their status,
re-executing them as necessary.")
    (license license:gpl3+)))

(define-public inetutils
  (package
    (name "inetutils")
    (version "1.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/inetutils/inetutils-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "05n65k4ixl85dc6rxc51b1b732gnmm8xnqi424dy9f1nz7ppb3xy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--localstatedir=/var"

                           ;; Make sure 'PATH_PROCNET_DEV' gets defined when
                           ;; cross-compiling (by default it does not.)
                           ,@(if (%current-target-system)
                                 '("--with-path-procnet-dev=/proc/net/dev")
                                 '()))
       ;; On some systems, 'libls.sh' may fail with an error such as:
       ;; "Failed to tell switch -a apart from -A".
       #:parallel-tests? #f))
    (inputs `(("ncurses" ,ncurses)
              ("readline" ,readline)))        ;for 'ftp'
    (native-inputs `(("netstat" ,net-tools))) ;for tests
    (home-page "https://www.gnu.org/software/inetutils/")
    (synopsis "Basic networking utilities")
    (description
     "Inetutils is a collection of common network programs, such as an ftp
client and server, a telnet client and server, an rsh client and server, and
hostname.")
    (license license:gpl3+)))

(define-public shadow
  (package
    (name "shadow")
    (version "4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/shadow-maint/shadow/releases/"
                    "download/" version "/shadow-" version ".tar.xz"))
              (sha256
               (base32
                "0hdpai78n63l3v3fgr3kkiqzhd0awrpfnnzz4mf7lmxdh61qb37w"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Assume System V `setpgrp (void)', which is the default on GNU
       ;; variants (`AC_FUNC_SETPGRP' is not cross-compilation capable.)
       #:configure-flags
       '("--with-libpam" "ac_cv_func_setpgrp_void=yes")

       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-nscd-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Use the right file name for nscd.
             (let ((libc (assoc-ref inputs
                                    ,(if (%current-target-system)
                                         "cross-libc"
                                         "libc"))))
               (substitute* "lib/nscd.c"
                 (("/usr/sbin/nscd")
                  (string-append libc "/sbin/nscd"))))))
         (add-after 'install 'remove-groups
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Remove `groups', which is already provided by Coreutils.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man")))
               (delete-file (string-append bin "/groups"))
               (for-each delete-file (find-files man "^groups\\."))
               #t))))))

    (inputs  (if (string-contains (or (%current-target-system)
                                      (%current-system))
                                  "-linux")
                 `(("linux-pam" ,linux-pam))
                 '()))
    (home-page "http://pkg-shadow.alioth.debian.org/")
    (synopsis "Authentication-related tools such as passwd, su, and login")
    (description
     "Shadow provides a number of authentication-related tools, including:
login, passwd, su, groupadd, and useradd.")

    ;; The `vipw' program is GPLv2+.
    ;; libmisc/salt.c is public domain.
    (license license:bsd-3)))

(define-public mingetty
  (package
    (name "mingetty")
    (version "1.08")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/mingetty/mingetty/"
                                 version "/mingetty-" version ".tar.gz"))
             (sha256
              (base32
               "05yxrp44ky2kg6qknk1ih0kvwkgbn9fbz77r3vci7agslh5wjm8g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (man8   (string-append out "/share/man/man8"))
                    (sbin   (string-append out "/sbin"))
                    (shadow (assoc-ref inputs "shadow"))
                    (login  (string-append shadow "/bin/login")))
               (substitute* "Makefile"
                 (("^SBINDIR.*")
                  (string-append "SBINDIR = " out
                                 "/sbin\n"))
                 (("^MANDIR.*")
                  (string-append "MANDIR = " out
                                 "/share/man/man8\n")))

               ;; Pick the right 'login' by default.
               (substitute* "mingetty.c"
                 (("\"/bin/login\"")
                  (string-append "\"" login "\"")))

               (mkdir-p sbin)
               (mkdir-p man8))
             #t)))
       #:tests? #f))                              ; no tests
    (inputs `(("shadow" ,shadow)))

    (home-page "https://sourceforge.net/projects/mingetty")
    (synopsis "Getty for the text console")
    (description
     "Small console getty that is started on the Linux text console,
asks for a login name and then transfers over to 'login'.  It is extended to
allow automatic login and starting any app.")
    (license license:gpl2+)))

(define-public net-base
  (package
    (name "net-base")
    (version "5.3")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://debian/pool/main/n/netbase/netbase_"
                   version ".tar.xz"))
             (sha256
              (base32
               "12xqjwg3p4rzmmh2iib6sigm9l29y3dgk74mmnw64k84jnbwdxl1"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let* ((source (assoc-ref %build-inputs "source"))
                          (tar    (assoc-ref %build-inputs "tar"))
                          (xz     (assoc-ref %build-inputs "xz"))
                          (output (assoc-ref %outputs "out"))
                          (etc    (string-append output "/etc")))
                     (setenv "PATH" (string-append xz "/bin"))
                     (system* (string-append tar "/bin/tar") "xvf"
                              source)
                     (chdir ,(string-append "netbase-" version))
                     (mkdir-p etc)
                     (for-each copy-file
                               '("etc-services" "etc-protocols" "etc-rpc")
                               (map (cut string-append etc "/" <>)
                                    '("services" "protocols" "rpc")))
                     #t))))
    (native-inputs `(("tar" ,tar)
                     ("xz" ,xz)))
    (synopsis "IANA protocol, port, and RPC number assignments")
    (description
     "This package provides the /etc/services, /etc/protocols, and /etc/rpc
files, which contain information about the IANA-assigned port, protocol, and
ONC RPC numbers.")
    (home-page "http://packages.debian.org/sid/netbase")
    (license license:gpl2)))

(define-public netcat
  (package
    (name "netcat")
    (version "0.7.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/netcat/netcat/" version
                                 "/netcat-" version ".tar.bz2"))
             (sha256
              (base32
               "1frjcdkhkpzk0f84hx6hmw5l0ynpmji8vcbaxg8h5k2svyxz0nmm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ;; By default, man and info pages are put in PREFIX/{man,info},
       ;; but we want them in PREFIX/share/{man,info}.
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "--mandir=" out "/share/man")
               (string-append "--infodir=" out "/share/info")))))
    (home-page "http://netcat.sourceforge.net")
    (synopsis "Read and write data over TCP/IP")
    (description
     "Netcat is a featured networking utility which reads and writes data
across network connections, using the TCP/IP protocol.  It is designed to be a
reliable \"back-end\" tool that can be used directly or easily driven by other
programs and scripts.  At the same time, it is a feature-rich network debugging
and exploration tool, since it can create almost any kind of connection you
would need and has several interesting built-in capabilities.")
    (license license:gpl2+)))

(define-public sipcalc
  (package
    (name "sipcalc")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.routemeister.net/projects"
                           "/sipcalc/files/sipcalc" "-" version ".tar.gz"))
       (sha256
        (base32
         "0mv3wndj4z2bsshh2k8d5sy3j8wxzgf8mzmmkvj1k8gpcz37dm6g"))))
    (build-system gnu-build-system)
    (home-page "http://www.routemeister.net/projects/sipcalc/")
    (synopsis "Command-line IP subnet calculator")
    (description
     "Sipcalc is an advanced command-line IP subnet calculator.  It can take
multiple forms of input (IPv4/IPv6/interface/hostname) and output a multitude
of information about a given subnet.

Features include:

@itemize @bullet
@item IPv4
@itemize
@item Retrieving of address information from interfaces.
@item Classfull and CIDR output.
@item Multiple address and netmask input and output formats (dotted quad, hex,
number of bits).
@item Output of broadcast address, network class, Cisco wildcard,
hosts/range, network range.
@item The ability to split a network based on a smaller netmask, now also with
recursive runs on the generated subnets.  (also IPv6)
@end itemize
@item IPv6
@itemize
@item Compressed and expanded input and output addresses.
@item Standard IPv6 network output.
@item v4 in v6 output.
@item Reverse DNS address generation.
@end itemize
@end itemize\n")
    (license license:bsd-3)))

(define-public alive
  (package
    (name "alive")
    (version "2.0.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/alive/alive-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1vrzg51ai68x9yld7vbgl58sxaw5qpx8rbakwcxn4cqq6vpxj38j"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("alive_cv_nice_ping=yes")))
    (inputs `(("guile" ,guile-2.0)
              ("inetutils" ,inetutils)))
    (home-page "https://www.gnu.org/software/alive/")
    (synopsis "Autologin and keep-alive daemon")
    (description
     "GNU Alive sends periodic pings to a server, generally to keep a
connection alive.")
    (license license:gpl3+)))

(define-public isc-dhcp
  (let* ((bind-major-version "9")
         (bind-minor-version "9")
         (bind-patch-version "11")
         (bind-release-type "-P")         ; for patch release, use "-P"
         (bind-release-version "1")      ; for patch release, e.g. "6"
         (bind-version (string-append bind-major-version
                                      "."
                                      bind-minor-version
                                      "."
                                      bind-patch-version
                                      bind-release-type
                                      bind-release-version)))
    (package
      (name "isc-dhcp")
      (version "4.3.6-P1")
      (source (origin
                (method url-fetch)
                (uri (string-append "http://ftp.isc.org/isc/dhcp/"
                                    version "/dhcp-" version ".tar.gz"))
                (sha256
                 (base32
                  "1hx3az6ckvgvybr1ag4k9kqr8zfcpzcww4vpw5gz0mi8y2z7gl9g"))))
      (build-system gnu-build-system)
      (arguments
       `(#:parallel-build? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'replace-bundled-bind
             (lambda* (#:key inputs #:allow-other-keys)
               (delete-file "bind/bind.tar.gz")
               (copy-file (assoc-ref inputs "bind-source-tarball")
                          "bind/bind.tar.gz")
               (chmod "bind/bind.tar.gz" #o644)
               (substitute* "bind/version.tmp"
                 (("^MAJORVER=.*")
                  (format #f "MAJORVER=~a\n" ,bind-major-version))
                 (("^MINORVER=.*")
                  (format #f "MINORVER=~a\n" ,bind-minor-version))
                 (("^PATCHVER=.*")
                  (format #f "PATCHVER=~a\n" ,bind-patch-version))
                 (("^RELEASETYPE=.*")
                  (format #f "RELEASETYPE=~a\n" ,bind-release-type))
                 (("^RELEASEVER=.*")
                  (format #f "RELEASEVER=~a\n" ,bind-release-version)))
               #t))
           (add-after 'configure 'post-configure
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Point to the right client script, which will be
               ;; installed in a later phase.
               (substitute* "includes/dhcpd.h"
                 (("#define[[:blank:]]+_PATH_DHCLIENT_SCRIPT.*")
                  (let ((out (assoc-ref outputs "out")))
                    (string-append "#define _PATH_DHCLIENT_SCRIPT \""
                                   out "/libexec/dhclient-script"
                                   "\"\n"))))

               ;; During the 'build' phase, 'bind.tar.gz' is extracted, so
               ;; we must patch shebangs in there and make sure the right
               ;; shell is used.
               (with-directory-excursion "bind"
                 (substitute* "Makefile"
                   (("\\./configure")
                    (let ((sh (which "sh")))
                      (string-append "./configure CONFIG_SHELL="
                                     sh " SHELL=" sh))))

                 (let ((bind-directory (string-append "bind-" ,bind-version)))
                   (system* "tar" "xf" "bind.tar.gz")
                   (for-each patch-shebang
                             (find-files bind-directory ".*"))
                   (zero? (system* "tar" "cf" "bind.tar.gz"
                                   bind-directory
                                   ;; avoid non-determinism in the archive
                                   "--sort=name"
                                   "--mtime=@0"
                                   "--owner=root:0"
                                   "--group=root:0"))))))
           (add-after 'install 'post-install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Install the dhclient script for GNU/Linux and make sure
               ;; if finds all the programs it needs.
               (let* ((out       (assoc-ref outputs "out"))
                      (libexec   (string-append out "/libexec"))
                      (coreutils (assoc-ref inputs "coreutils"))
                      (inetutils (assoc-ref inputs "inetutils"))
                      (net-tools (assoc-ref inputs "net-tools"))
                      (sed       (assoc-ref inputs "sed")))
                 (substitute* "client/scripts/linux"
                   (("/sbin/ip")
                    (string-append (assoc-ref inputs "iproute")
                                   "/sbin/ip")))

                 (mkdir-p libexec)
                 (copy-file "client/scripts/linux"
                            (string-append libexec "/dhclient-script"))

                 (wrap-program
                     (string-append libexec "/dhclient-script")
                   `("PATH" ":" prefix
                     ,(map (lambda (dir)
                             (string-append dir "/bin:"
                                            dir "/sbin"))
                           (list inetutils net-tools coreutils sed))))))))))

      (native-inputs `(("perl" ,perl)))

      (inputs `(("inetutils" ,inetutils)
                ("net-tools" ,net-tools)
                ("iproute" ,iproute)

                ;; XXX isc-dhcp bundles a copy of bind that has security
                ;; flaws, so we use a newer version.
                ("bind-source-tarball"
                 ,(origin
                    (method url-fetch)
                    (uri (string-append "http://ftp.isc.org/isc/bind9/"
                                        bind-version
                                        "/bind-" bind-version ".tar.gz"))
                    (sha256
                     (base32
                      "1a4g6nzzrbmhngdgvgv1jjq4fm06m8fwc2a0gskkchplxl7dva20"))))

                ;; When cross-compiling, we need the cross Coreutils and sed.
                ;; Otherwise just use those from %FINAL-INPUTS.
                ,@(if (%current-target-system)
                      `(("coreutils" ,coreutils)
                        ("sed" ,sed))
                      '())))

      (home-page "http://www.isc.org/products/DHCP/")
      (synopsis "Dynamic Host Configuration Protocol (DHCP) tools")
      (description
       "ISC's Dynamic Host Configuration Protocol (DHCP) distribution provides a
reference implementation of all aspects of DHCP, through a suite of DHCP
tools: server, client, and relay agent.")
      (license license:isc)
      (properties '((cpe-name . "dhcp"))))))

(define-public libpcap
  (package
    (name "libpcap")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.tcpdump.org/release/libpcap-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "07jlhc66z76dipj4j5v3dig8x6h3k6cb36kmnmpsixf3zmlvqgb7"))))
    (build-system gnu-build-system)
    (native-inputs `(("bison" ,bison) ("flex" ,flex)))
    (arguments '(#:configure-flags '("--with-pcap=linux")
                 #:tests? #f))                    ; no 'check' target
    (home-page "http://www.tcpdump.org")
    (synopsis "Network packet capture library")
    (description
     "libpcap is an interface for user-level packet capture.  It provides a
portable framework for low-level network monitoring.  Applications include
network statistics collection, security monitoring, network debugging, etc.")

    ;; fad-*.c and a couple other files are BSD-4, but the rest is BSD-3.
    (license license:bsd-3)))

(define-public tcpdump
  (package
    (name "tcpdump")
    (version "4.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.tcpdump.org/release/tcpdump-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ygy0layzqaj838r5xd613iraz09wlfgpyh7pc6cwclql8v3b2vr"))))
    (build-system gnu-build-system)
    (inputs `(("libpcap" ,libpcap)
              ("openssl" ,openssl)))
    (native-inputs `(("perl" ,perl)))        ; for tests
    (home-page "http://www.tcpdump.org/")
    (synopsis "Network packet analyzer")
    (description
     "Tcpdump is a command-line tool to analyze network traffic passing
through the network interface controller.")
    (license license:bsd-3)))

(define-public jnettop
  (package
    (name "jnettop")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://jnettop.kubs.info/dist/jnettop-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1855np7c4b0bqzhf1l1dyzxb90fpnvrirdisajhci5am6als31z9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("ncurses" ,ncurses)
       ("libpcap" ,libpcap)))
    (home-page "http://jnettop.kubs.info/")
    (synopsis "Visualize network traffic by bandwidth use")
    (description
     "Jnettop is a traffic visualiser, which captures traffic going
through the host it is running from and displays streams sorted
by bandwidth they use.")
    (license license:gpl2+)))

(define-public clusterssh
  (package
    (name "clusterssh")
    (version "3.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/clusterssh/"
                                  "1.%20ClusterSSH%20Series%203/" version
                                  "/clusterssh-" version ".tar.gz"))
              (sha256
               (base32
                "1bwggpvaj2al5blg1ynapviv2kpydffpzq2zkhi81najnvzc1rr7"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (propagated-inputs `(("xterm" ,xterm)
                         ("perl-tk" ,perl-tk)
                         ("perl-x11-protocol" ,perl-x11-protocol)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'set-load-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Put the perl-tk and perl-x11-protocol modules in the perl inc
             ;; path for PROG
             (let* ((out  (assoc-ref outputs "out"))
                    (prog (string-append out "/bin/cssh"))
                    (perl-ver ,(package-version perl))
                    (x11-inc (string-append
                              (assoc-ref inputs "perl-x11-protocol")
                              "/lib/perl5/site_perl/" perl-ver))
                    (tk-inc (string-append
                             (assoc-ref inputs "perl-tk")
                             "/lib/perl5/site_perl/" perl-ver
                             "/x86_64-linux")))
               (wrap-program
                   prog
                 `("PERL5LIB" ":" prefix (,x11-inc ,tk-inc))))
             #t)))))
    ;; The clusterssh.sourceforge.net address requires login to view
    (home-page "https://sourceforge.net/projects/clusterssh/")
    (synopsis "Secure concurrent multi-server terminal control")
    (description
     "ClusterSSH controls a number of xterm windows via a single graphical
console window to allow commands to be interactively run on multiple servers
over ssh connections.")
    (license license:gpl2+)))

(define-public rename
  (package
    (name "rename")
    (version "0.20")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/R/RM/RMBARKER/File-Rename-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1cf6xx2hiy1xalp35fh8g73j67r0w0g66jpcbc6971x9jbm7bvjy"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (home-page "https://metacpan.org/pod/distribution/File-Rename/rename.PL")
    (synopsis "Perl extension for renaming multiple files")
    (description
     "This package provides a Perl interface (@code{Perl::Rename}) as well
as a command-line utility (@command{rename}) that can rename multiple files
at once based on a Perl regular expression.")
    (license license:perl-license)))

(define-public rottlog
  (package
    (name "rottlog")
    (version "0.72.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/rottlog/rottlog-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0751mb9l2f0jrk3vj6q8ilanifd121dliwk0c34g8k0dlzsv3kd7"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Makefile.in"
                  (("-o \\$\\{LOG_OWN\\} -g \\$\\{LOG_GROUP\\}")
                   ;; Don't try to chown root.
                   "")
                  (("mkdir -p \\$\\(ROTT_STATDIR\\)")
                   ;; Don't attempt to create /var/lib/rottlog.
                   "true")))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list "ROTT_ETCDIR=/etc/rottlog" ;rc file location
                               "--localstatedir=/var")

       ;; Install example config files in OUT/etc.
       #:make-flags (list (string-append "ROTT_ETCDIR="
                                         (assoc-ref %outputs "out")
                                         "/etc"))

       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-paths
                    (lambda _
                      (substitute* "rc/rc"
                        (("/usr/sbin/sendmail") "sendmail"))
                      #t))
                  (add-after 'build 'set-packdir
                    (lambda _
                      ;; Set a default location for archived logs.
                      (substitute* "rc/rc"
                        (("packdir=\"\"")
                         "packdir=\"/var/log\""))
                      #t))
                  (add-before 'install 'tweak-rc-weekly
                    (lambda _
                      (substitute* "rc/weekly"
                        (("/bin/kill")
                         (which "kill"))
                        (("syslogd\\.pid")
                         ;; The file is called 'syslog.pid' (no 'd').
                         "syslog.pid"))
                      #t))
                  (add-after 'install 'install-info
                    (lambda _
                      (zero? (system* "make" "install-info")))))))
    (native-inputs `(("texinfo" ,texinfo)
                     ("util-linux" ,util-linux))) ; for 'cal'
    (home-page "https://www.gnu.org/software/rottlog/")
    (synopsis "Log rotation and management")
    (description
     "GNU Rot[t]log is a program for managing log files.  It is used to
automatically rotate out log files when they have reached a given size or
according to a given schedule.  It can also be used to automatically compress
and archive such logs.  Rot[t]log will mail reports of its activity to the
system administrator.")
    (license license:gpl3+)))

(define-public sudo
  (package
    (name "sudo")
    (version "1.8.22")
    (source (origin
              (method url-fetch)
              (uri
               (list (string-append "https://www.sudo.ws/sudo/dist/sudo-"
                                    version ".tar.gz")
                     (string-append "ftp://ftp.sudo.ws/pub/sudo/OLD/sudo-"
                                    version ".tar.gz")))
              (sha256
               (base32
                "00pxp74xkwdcmrjwy55j0k8p684jk1zx3nzdc11v30q8q8kwnmkj"))
              (modules '((guix build utils)))
              (snippet
               '(delete-file-recursively "lib/zlib"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--with-logpath=/var/log/sudo.log"
             "--with-rundir=/var/run/sudo"    ;must be cleaned up at boot time
             "--with-vardir=/var/db/sudo"
             "--with-iologdir=/var/log/sudo-io"

             ;; 'visudo.c' expects _PATH_MV to be defined, but glibc doesn't
             ;; provide it.
             (string-append "CPPFLAGS=-D_PATH_MV='\""
                            (assoc-ref %build-inputs "coreutils")
                            "/bin/mv\"'"))

       ;; Avoid non-determinism; see <http://bugs.gnu.org/21918>.
       #:parallel-build? #f

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda _
             (substitute* "src/sudo_usage.h.in"
               ;; Do not capture 'configure' arguments since we would
               ;; unduly retain references, and also because the
               ;; CPPFLAGS above would close the string literal
               ;; prematurely.
               (("@CONFIGURE_ARGS@") "\"\""))
             (substitute* (find-files "." "Makefile\\.in")
               (("-o [[:graph:]]+ -g [[:graph:]]+")
                ;; Allow installation as non-root.
                "")
               (("^install: (.*)install-sudoers(.*)" _ before after)
                ;; Don't try to create /etc/sudoers.
                (string-append "install: " before after "\n"))
               (("\\$\\(DESTDIR\\)\\$\\(rundir\\)")
                ;; Don't try to create /run/sudo.
                "$(TMPDIR)/dummy")
               (("\\$\\(DESTDIR\\)\\$\\(vardir\\)")
                ;; Don't try to create /var/db/sudo.
                "$(TMPDIR)/dummy"))
             #t)))

       ;; XXX: The 'testsudoers' test series expects user 'root' to exist, but
       ;; the chroot's /etc/passwd doesn't have it.  Turn off the tests.
       #:tests? #f))
    (inputs
     `(("groff" ,groff)
       ("linux-pam" ,linux-pam)
       ("zlib" ,zlib)
       ("coreutils" ,coreutils)))
    (home-page "https://www.sudo.ws/")
    (synopsis "Run commands as root")
    (description
     "Sudo (su \"do\") allows a system administrator to delegate authority to
give certain users (or groups of users) the ability to run some (or all)
commands as root or another user while providing an audit trail of the
commands and their arguments.")

    ;; See <http://www.sudo.ws/sudo/license.html>.
    (license license:x11)))

(define-public wpa-supplicant-minimal
  (package
    (name "wpa-supplicant-minimal")
    (version "2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://w1.fi/releases/wpa_supplicant-"
                    version
                    ".tar.gz"))
              (patches (search-patches "wpa-supplicant-CVE-2017-13082.patch"
                                       "wpa-supplicant-fix-key-reuse.patch"
                                       "wpa-supplicant-fix-zeroed-keys.patch"
                                       "wpa-supplicant-fix-nonce-reuse.patch"
                                       "wpa-supplicant-krack-followups.patch"))
              (sha256
               (base32
                "0l0l5gz3d5j9bqjsbjlfcv4w4jwndllp9fmyai4x9kg6qhs6v4xl"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir "wpa_supplicant")
             (copy-file "defconfig" ".config")
             (let ((port (open-file ".config" "al")))
               (display "
      CONFIG_DEBUG_SYSLOG=y

      # Choose GnuTLS (the default is OpenSSL.)
      CONFIG_TLS=gnutls

      CONFIG_DRIVER_NL80211=y
      CFLAGS += $(shell pkg-config libnl-3.0 --cflags)
      CONFIG_LIBNL32=y
      CONFIG_READLINE=y\n" port)
               (close-port port))))
         (add-after 'install 'install-man-pages
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (man  (string-append out "/share/man"))
                    (man5 (string-append man "/man5"))
                    (man8 (string-append man "/man8")))
               (define (copy-man-page target)
                 (lambda (file)
                   (install-file file target)))

               (mkdir-p man5) (mkdir man8)
               (for-each (copy-man-page man5)
                         (find-files "doc/docbook" "\\.5"))
               (for-each (copy-man-page man8)
                         (find-files "doc/docbook" "\\.8"))
               #t))))

      #:make-flags (list "CC=gcc"
                         (string-append "BINDIR=" (assoc-ref %outputs "out")
                                        "/sbin")
                         (string-append "LIBDIR=" (assoc-ref %outputs "out")
                                        "/lib"))
      #:tests? #f))
    (inputs
     `(("readline" ,readline)
       ("libnl" ,libnl)
       ("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt)))                 ;needed by crypto_gnutls.c
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://w1.fi/wpa_supplicant/")
    (synopsis "Connecting to WPA and WPA2-protected wireless networks")
    (description
     "wpa_supplicant is a WPA Supplicant with support for WPA and WPA2 (IEEE
802.11i / RSN).  Supplicant is the IEEE 802.1X/WPA component that is used in
the client stations.  It implements key negotiation with a WPA Authenticator
and it controls the roaming and IEEE 802.11 authentication/association of the
WLAN driver.

This package provides the 'wpa_supplicant' daemon and the 'wpa_cli' command.")

    ;; In practice, this is linked against Readline, which makes it GPLv3+.
    (license license:bsd-3)))

(define-public wpa-supplicant
  (package (inherit wpa-supplicant-minimal)
    (name "wpa-supplicant")
    (inputs `(("dbus" ,dbus)
              ,@(package-inputs wpa-supplicant-minimal)))
    (arguments
     (substitute-keyword-arguments (package-arguments wpa-supplicant-minimal)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'configure 'configure-for-dbus
             (lambda _
               (let ((port (open-file ".config" "al")))
                 (display "
      CONFIG_CTRL_IFACE_DBUS=y
      CONFIG_CTRL_IFACE_DBUS_NEW=y
      CONFIG_CTRL_IFACE_DBUS_INTRO=y\n" port)
                 (close-port port))
               #t))
          (add-after 'install-man-pages 'install-dbus-conf
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (dir (string-append out "/etc/dbus-1/system.d")))
                (mkdir-p dir)
                (copy-file "dbus/dbus-wpa_supplicant.conf"
                           (string-append dir "/wpa_supplicant.conf")))
              #t))))))))

(define-public wakelan
  (package
    (name "wakelan")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.gwdg.de/pub/linux/metalab/system/network/misc/wakelan-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0vydqpf44146ir6k87gmqaq6xy66xhc1gkr3nsd7jj3nhy7ypx9x"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/share/man/man1"))

               ;; It's an old configure script that doesn't understand
               ;; the extra options we pass.
               (setenv "CONFIG_SHELL" (which "bash"))
               (zero?
                (system* "./configure"
                         (string-append "--prefix=" out)
                         (string-append "--mandir=" out
                                        "/share/man")))))))
       #:tests? #f))
    (home-page "http://kernel.org")               ; really, no home page
    (synopsis "Send a wake-on-LAN packet")
    (description
     "WakeLan broadcasts a properly formatted UDP packet across the local area
network, which causes enabled computers to power on.")
    (license license:gpl2+)))

(define-public dmidecode
  (package
    (name "dmidecode")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://savannah/dmidecode/dmidecode-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1h0sg0lxa15nzf8s7884p6q7p6md9idm0c79wyqmk32l4ndwwrnp"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases (delete 'configure))
       #:tests? #f                                ; no 'check' target
       #:make-flags (list (string-append "prefix="
                                         (assoc-ref %outputs "out")))))
    (home-page "https://www.nongnu.org/dmidecode/")
    (synopsis "Read hardware information from the BIOS")
    (description
     "Dmidecode reports information about your system's hardware as described
in your system BIOS according to the SMBIOS/DMI standard.  This typically
includes system manufacturer, model name, serial number, BIOS version, asset
tag as well as a lot of other details of varying level of interest and
reliability depending on the manufacturer.  This will often include usage
status for the CPU sockets, expansion slots (e.g. AGP, PCI, ISA) and memory
module slots, and the list of I/O ports (e.g. serial, parallel, USB).")
    (license license:gpl2+)))

(define-public acpica
  (package
    (name "acpica")
    (version "20180209")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://acpica.org/sites/acpica/files/acpica-unix2-"
                    version ".tar.gz"))
              (sha256
               (base32
                "04hyc5s9iiyiznvspx7q73r6ns98d51wrv8zfvqbqv52gqq8hzdh"))))
    (build-system gnu-build-system)
    (native-inputs `(("flex" ,flex)
                     ("bison" ,bison)))
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" %output)
                          "CC=gcc"
                          "HOST=_LINUX"
                          "OPT_CFLAGS=-Wall -fno-strict-aliasing")
       #:tests? #f  ; no 'check' target.
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (home-page "https://acpica.org/")
    (synopsis "Tools for the development and debug of ACPI tables")
    (description
     "The ACPI Component Architecture (@dfn{ACPICA}) project provides an
OS-independent reference implementation of the Advanced Configuration and
Power Interface Specification (@dfn{ACPI}).  ACPICA code contains those portions
of ACPI meant to be directly integrated into the host OS as a kernel-resident
subsystem, and a small set of tools to assist in developing and debugging ACPI
tables.  This package contains only the user-space tools needed for ACPI table
development, not the kernel implementation of ACPI.")
    (license license:gpl2)))  ; Dual GPLv2/ACPICA Licence

(define-public stress
  (package
    (name "stress")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/main/s/stress/stress_"
                                  version ".orig.tar.gz"))
              (sha256
               (base32
                "0nw210jajk38m3y7h8s130ps2qsbz7j75wab07hi2r3hlz14yzh5"))))
    (build-system gnu-build-system)
    (home-page "https://packages.debian.org/sid/stress")
    (synopsis "Impose load on and stress test a computer system")
    (description
     "Stress is a tool that imposes a configurable amount of CPU, memory, I/O,
or disk stress on a POSIX-compliant operating system and reports any errors it
detects.

Stress is not a benchmark.  It is a tool used by system administrators to
evaluate how well their systems will scale, by kernel programmers to evaluate
perceived performance characteristics, and by systems programmers to expose
the classes of bugs which only or more frequently manifest themselves when the
system is under heavy load.")
    (license license:gpl2+)))

(define-public detox
  (package
    (name "detox")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/detox/detox/" version
                                  "/detox-" version ".tar.bz2"))
              (sha256
               (base32
                "1y6vvjqsg54kl49cry73jbfhr04s7wjs779vrr9zrq6kww7dkymb"))))
    (build-system gnu-build-system)
    ;; Both flex and popt are used in this case for their runtime libraries
    ;; (libfl and libpopt).
    (inputs
     `(("flex" ,flex)
       ("popt" ,popt)))
    (arguments
     `(#:configure-flags `(,(string-append "--with-popt="
                                           (assoc-ref %build-inputs "popt")))
       #:tests? #f))                    ;no 'check' target
    (home-page "http://detox.sourceforge.net")
    (synopsis "Clean up file names")
    (description
     "Detox is a program that renames files to make them easier to work with
under Unix and related operating systems.  Spaces and various other unsafe
characters (such as \"$\") get replaced with \"_\".  ISO 8859-1 (Latin-1)
characters can be replaced as well, as can UTF-8 characters.")
    (license license:bsd-3)))

(define-public testdisk
  (package
    (name "testdisk")
    (version "7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.cgsecurity.org/testdisk-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0ba4wfz2qrf60vwvb1qsq9l6j0pgg81qgf7fh22siaz649mkpfq0"))))
    (build-system gnu-build-system)
    (inputs
     `(("ntfs-3g" ,ntfs-3g)
       ("util-linux" ,util-linux)
       ("openssl" ,openssl)
       ;; FIXME: add reiserfs
       ("zlib" ,zlib)
       ("e2fsprogs" ,e2fsprogs)
       ("libjpeg" ,libjpeg)
       ("ncurses" ,ncurses)))
    (home-page "https://www.cgsecurity.org/wiki/TestDisk")
    (synopsis "Data recovery tool")
    (description
     "TestDisk is a program for data recovery, primarily designed to help
recover lost partitions and/or make non-booting disks bootable again.")
    (license license:gpl2+)))

(define-public tree
  (package
    (name "tree")
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://mama.indstate.edu/users/ice/tree/src/tree-"
                    version ".tgz"))
              (sha256
               (base32 "04kviw799qxly08zb8n5mgxfd96gyis6x69q2qiw86jnh87c4mv9"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases (delete 'configure))
       #:tests? #f                      ; no check target
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                               (list (string-append "prefix=" out)))))
    (synopsis "Recursively list the contents of a directory")
    (description
     "Tree is a recursive directory listing command that produces a depth
indented listing of files, which is colorized ala dircolors if the LS_COLORS
environment variable is set and output is to tty.")
    (home-page "http://mama.indstate.edu/users/ice/tree/")
    (license license:gpl2+)))

(define-public direvent
  (package
    (name "direvent")
    (version "5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/direvent/direvent-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1nwvjmx7kb14ni34c0b8x9a3791pc20gvhj7xaj66d8q4h6n0qf4"))
              (modules '((guix build utils)))
              (snippet '(substitute* "tests/testsuite"
                          (("#![[:blank:]]?/bin/sh")
                           "#!$SHELL")))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-/bin/sh
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Use the right shell when executing the watcher and
             ;; user-provided shell commands.
             (let ((bash (assoc-ref inputs "bash")))
               (substitute* '("src/direvent.c" "src/progman.c")
                 (("\"/bin/sh\"")
                  (string-append "\"" bash "/bin/sh\"")))

               ;; Adjust the 'shell.at' test accordingly.
               (substitute* "tests/testsuite"
                 (("SHELL=/bin/sh")
                  (string-append "SHELL=" bash "/bin/sh")))

               #t))))))
    (home-page "https://www.gnu.org/software/direvent/")
    (synopsis "Daemon to monitor directories for events such as file removal")
    (description
     "A daemon that monitors directories for events, such as creating,
deleting or modifying files.  It can monitor different sets of directories for
different events.  When an event is detected, direvent calls a specified
external program with information about the event, such as the location
within the file system where it occurred.  Thus, \"direvent\" provides an
easy way to react immediately if given files undergo changes, for example, to
track changes in important system configuration files.")
    (license license:gpl3+)))

(define-public libcap-ng
  (package
    (name "libcap-ng")
    (version "0.7.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://people.redhat.com/sgrubb/libcap-ng/libcap-ng-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0a0k484kwv0zilry2mbl9k56cnpdhsjxdxin17jas6kkyfy345aa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--without-python")))
    (home-page "https://people.redhat.com/sgrubb/libcap-ng/")
    (synopsis "Library for more easily working with POSIX capabilities")
    (description
     "The libcap-ng library is intended to make programming with POSIX
capabilities easier than the traditional libcap library.  It includes
utilities that can analyse all currently running applications and print out
any capabilities and whether or not it has an open ended bounding set.  The
included utilities are designed to let admins and developers spot apps from
various ways that may be running with too much privilege.")
    ;; The library is lgpl2.1+, but also ships some utils which are gpl2+.
    (license (list license:lgpl2.1+ license:gpl2+))))

(define-public smartmontools
  (package
    (name "smartmontools")
    (version "6.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/smartmontools/smartmontools/"
                    version "/smartmontools-" version ".tar.gz"))
              (sha256
               (base32
                "0m1hllbb78rr6cxkbalmz1gqkl0psgq8rrmv4gwcmz34n07kvx2i"))))
    (build-system gnu-build-system)
    (inputs `(("libcap-ng" ,libcap-ng)))
    (home-page "https://www.smartmontools.org/")
    (synopsis "S.M.A.R.T. harddisk control and monitoring tools")
    (description
     "The smartmontools package contains utility programs to control and
monitor storage systems using the Self-Monitoring, Analysis and Reporting
Technology System (@dfn{S.M.A.R.T.}) built into most modern ATA and SCSI hard
disks.  In many cases, these utilities will provide advanced warning of disk
degradation and failure.")
    (license license:gpl2+)))

(define-public fdupes
  (package
    (name "fdupes")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/adrianlopezroche/fdupes/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1sj9pa40pbz6xdwbxfwhdhkvhdf1xc5gvggk9mdq26c41gdnyswx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:tests? #f ; no 'check' target
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))))
    (home-page "https://github.com/adrianlopezroche/fdupes")
    (synopsis "Identify duplicate files")
    (description
     "fdupes is a program for identifying duplicate files residing within
specified directories.")
    (license license:expat)))

(define-public graphios
  (package
   (name "graphios")
   (version "2.0.3")
   (source
    (origin
      (method url-fetch)
      (uri (string-append
            "https://pypi.python.org/packages/source/g/graphios/graphios-"
            version ".tar.gz"))
      (sha256
       (base32
        "1h87hvc315wg6lklbf4l7csd3n5pgljwrfli1p3nasdi0izgn66i"))))
   (build-system python-build-system)
   (arguments
    ;; Be warned: Building with Python 3 succeeds, but the build process
    ;; throws a syntax error that is ignored.
    `(#:python ,python-2
      #:phases
      (modify-phases %standard-phases
        (add-before 'build 'fix-setup.py
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Fix hardcoded, unprefixed file names.
            (let ((out (assoc-ref outputs "out")))
              (substitute* '("setup.py")
                (("/etc") (string-append out "/etc"))
                (("/usr") out)
                (("distro_ver = .*") "distro_ver = ''"))
              #t))))))
   (home-page "https://github.com/shawn-sterling/graphios")
   (synopsis "Emit Nagios metrics to Graphite, Statsd, and Librato")
   (description
    "Graphios is a script to emit nagios perfdata to various upstream metrics
processing and time-series systems.  It's currently compatible with Graphite,
Statsd, Librato and InfluxDB.  Graphios can emit Nagios metrics to any number
of supported upstream metrics systems simultaneously.")
   (license license:gpl2+)))

(define-public ansible
  (package
    (name "ansible")
    (version "2.4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ansible" version))
       (sha256
        (base32
         "0n3n9py4s3aykiii31xq8g4wmd6693jvby0424pjrg0bna01apri"))
       (patches (search-patches "ansible-wrap-program-hack.patch"))))
    (build-system python-build-system)
    (native-inputs
     `(("python2-pycrypto" ,python2-pycrypto)
       ("python2-httplib2" ,python2-httplib2)
       ("python2-passlib" ,python2-passlib)
       ("python2-nose" ,python2-nose)
       ("python2-mock" ,python2-mock)
       ("python2-jinja2" ,python2-jinja2)
       ("python2-pyyaml" ,python2-pyyaml)
       ("python2-paramiko" ,python2-paramiko)))
    (inputs
     `(("python2-pycrypto" ,python2-pycrypto)
       ("python2-jinja2" ,python2-jinja2)
       ("python2-pyyaml" ,python2-pyyaml)
       ("python2-paramiko" ,python2-paramiko)))
    (arguments
     `(#:python ,python-2)) ; incompatible with Python 3
    (home-page "https://www.ansible.com/")
    (synopsis "Radically simple IT automation")
    (description "Ansible is a radically simple IT automation system.  It
handles configuration management, application deployment, cloud provisioning,
ad hoc task execution, and multinode orchestration---including trivializing
things like zero-downtime rolling updates with load balancers.")
    (license license:gpl3+)))

(define-public cpulimit
  (package
    (name "cpulimit")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/opsengine/cpulimit/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1nn2w849xd5bw4y5sqnll29nxdwl5h0cv4smc7dwmpb9qnd2ycb4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace
                   'build
                   (lambda _
                     (zero? (system* "make" "CC=gcc" "-Csrc"))))
                  (replace
                   'check
                   (lambda _
                     (zero? (system* "make" "CC=gcc" "-Ctests"))))
                  (replace
                   'install
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (bin (string-append out "/bin")))
                       (install-file "src/cpulimit" bin)))))))
    (home-page "https://github.com/opsengine/cpulimit")
    (synopsis "Limit CPU usage")
    (description
     "Cpulimit limits the CPU usage of a process.  It does not change the nice
value or other scheduling priority settings, but the real CPU usage, and is
able to adapt itself dynamically to the overall system load.  Children
processes and threads of the specified process may optionally share the same
limits.")
    (license license:gpl2+)))

(define-public autojump
  (package
    (name "autojump")
    (version "22.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/wting/autojump/archive/"
                           "release-v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "113rcpr37ngf2xs8da41qdarq5qmj0dwx8ggqy3lhlb0kvqq7g9z"))))
    (build-system gnu-build-system)
    (native-inputs                      ;for tests
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (inputs
     `(("python" ,python-wrapper)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build)
                  (replace 'check
                    (lambda _
                      (zero?
                       (system* "python" "tests/unit/autojump_utils_test.py"))))
                  (replace 'install
                    ;; The install.py script doesn't allow system installation
                    ;; into an arbitrary prefix, so do our own install.
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (share (string-append out "/share/autojump"))
                             (py (string-append out "/lib/python"
                                                ,(version-major+minor
                                                  (package-version python-wrapper))
                                                "/site-packages"))
                             (man (string-append out "/share/man/man1")))
                        (install-file "bin/autojump" bin)
                        (for-each (λ (f) (install-file f py))
                                  (find-files "bin" "\\.py$"))
                        (for-each (λ (f) (install-file f share))
                                  (find-files "bin" "autojump\\..*$"))
                        (substitute* (string-append share "/autojump.sh")
                          (("/usr/local") out))
                        (install-file "docs/autojump.1" man)
                        (wrap-program (string-append bin "/autojump")
                          `("PYTHONPATH" ":" prefix (,py)))
                        #t))))))
    (home-page "https://github.com/wting/autojump")
    (synopsis "Shell extension for file system navigation")
    (description
     "Autojump provides a faster way to navigate your file system, with a \"cd
command that learns\".  It works by maintaining a database of the directories
you use the most from the command line and allows you to \"jump\" to
frequently used directories by typing only a small pattern.")
    (license license:gpl3+)))

(define-public iftop
  (package
    (name "iftop")
    (version "1.0pre4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.ex-parrot.com/~pdw/iftop/download"
                                  "/iftop-" version ".tar.gz"))
              (sha256
               (base32
                "15sgkdyijb7vbxpxjavh5qm5nvyii3fqcg9mzvw7fx8s6zmfwczp"))))
    (build-system gnu-build-system)
    (inputs
      `(("libpcap" ,libpcap)
        ("ncurses" ,ncurses)))
    (synopsis "Monitor network usage")
    (description "Iftop does for network usage what @command{top} does
for CPU usage.  It listens to network traffic on a named interface and
displays a table of current bandwidth usage by pairs of hosts.")
    (home-page "http://www.ex-parrot.com/~pdw/iftop/")
    (license license:gpl2+)))

(define-public munge
  (package
    (name "munge")
    (version "0.5.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/dun/munge/releases/"
                                  "download/munge-" version "/munge-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1nj486bbg1adfg298zck96vgx57kchcypc1zdz1n7w540vyksxcr"))))
    (inputs
     `(("openssl" ,openssl)
       ("libgcrypt" ,libgcrypt)))
    (build-system gnu-build-system)
    (home-page "https://dun.github.io/munge/")
    (synopsis "Cluster computing authentication service")
    (description
     "Munge is an authentication service for creating and validating
credentials.  It allows a process to authenticate the UID and GID of another
local or remote process within a group of hosts having common users and
groups.  These hosts form a security realm that is defined by a shared
cryptographic key.  Clients within this security realm can create and validate
credentials without the use of root privileges, reserved ports, or
platform-specific methods.")
    (license license:gpl3+)))

(define-public audit
  (package
    (name "audit")
    (version "2.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://people.redhat.com/sgrubb/audit/"
                                  "audit-" version ".tar.gz"))
              (sha256
               (base32
                "1q1q51dvxscbi4kbakmd4bn0xrvwwaiwvaya79925cbrqwzxsg77"))))
    (build-system gnu-build-system)
    (home-page "http://people.redhat.com/sgrubb/audit/")
    (arguments
     `(#:configure-flags (list "--with-python=no")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             ;; In the build environmnte /etc/passwd does not contain an entry
             ;; for root/0, so we have to patch the expected value.
             (substitute* "auparse/test/auparse_test.ref"
               (("=0 \\(root\\)") "=0 (unknown(0))"))
             #t)))))
    (inputs
     `(("openldap" ,openldap)
       ("gnutls" ,gnutls)
       ("sasl" ,cyrus-sasl)))
    (synopsis "User-space component to the Linux auditing system")
    (description
     "auditd is the user-space component to the Linux auditing system, which
allows logging of system calls made by user-land processes.  It's responsible
for writing audit records to the disk.  Viewing the logs is done with the
@code{ausearch} or @code{aureport} utilities.  Configuring the audit rules is
done with the @code{auditctl} utility.")
    (license license:gpl2+)))

(define-public nmap
  (package
    (name "nmap")
    (version "7.60")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nmap.org/dist/nmap-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "08bga42ipymmbxd7wy4x5sl26c0ir1fm3n9rc6nqmhx69z66wyd8"))
              (modules '((guix build utils)))
              (snippet
               '(map delete-file-recursively
                 ;; Remove bundled lua, pcap, and pcre libraries.
                 ;; FIXME: Remove bundled liblinear once packaged.
                 '("liblua"
                   "libpcap"
                   "libpcre"
                   ;; Remove pre-compiled binares.
                   "mswin32")))))
    (build-system gnu-build-system)
    (inputs
     `(("openssl" ,openssl)
       ("libpcap" ,libpcap)
       ("pcre" ,pcre)
       ("lua" ,lua)
       ;; For 'ndiff'.
       ("python" ,python-2)))

    ;; TODO Add zenmap output.
    (outputs '("out" "ndiff"))
    (arguments
     '(#:configure-flags '("--without-zenmap")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-Makefile
           (lambda _
             (substitute* "Makefile"
               ;; Do not attempt to build lua.
               (("build-dnet build-lua") "build-dnet"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (define (make out . args)
               (unless (zero? (apply system* "make"
                                     (string-append "prefix=" out)
                                     args))
                 (error "make failed")))
             (define (python-path dir)
               (string-append dir "/lib/python2.7/site-packages"))
             (let ((out (assoc-ref outputs "out"))
                   (ndiff (assoc-ref outputs "ndiff")))
               (for-each mkdir-p (list out ndiff))
               (make out
                 "install-nmap"
                 "install-nse"
                 "install-ncat"
                 "install-nping")
               (make ndiff "install-ndiff")
               (wrap-program (string-append ndiff "/bin/ndiff")
                 `("PYTHONPATH" prefix
                   (,(python-path ndiff)))))))
         ;; These are the tests that do not require network access.
         (replace 'check
           (lambda _ (zero? (system* "make"
                                     "check-nse"
                                     "check-ndiff"
                                     "check-dns")))))
       ;; Nmap can't cope with out-of-source building.
       #:out-of-source? #f))
    (home-page "https://nmap.org/")
    (synopsis "Network discovery and security auditing tool")
    (description
     "Nmap (\"Network Mapper\") is a network discovery and security auditing
tool.  It is also useful for tasks such as network inventory, managing service
upgrade schedules, and monitoring host or service uptime.  It also provides an
advanced netcat implementation (ncat), a utility for comparing scan
results (ndiff), and a packet generation and response analysis tool (nping).")
    ;; This package uses nmap's bundled versions of libdnet and liblinear, which
    ;; both use a 3-clause BSD license.
    (license (list license:nmap license:bsd-3))))

(define-public dstat
  (package
    (name "dstat")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/dagwieers/dstat/archive/"
                    version ".tar.gz"))
              (file-name (string-append "dstat-" version ".tar.gz"))
              (sha256
               (base32
                "16286z3y2lc9nsq8njzjkv6k2vyxrj9xiixj1k3gnsbvhlhkirj6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; no make check
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "DESTDIR=" out)
                            "prefix=/"))
       ;; no configure script
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (inputs `(("python-2" ,python-2)))
    (synopsis "Versatile resource statistics tool")
    (description "Dstat is a versatile replacement for @command{vmstat},
@command{iostat}, @command{netstat}, and @command{ifstat}.  Dstat overcomes
some of their limitations and adds some extra features, more counters and
flexibility.  Dstat is handy for monitoring systems during performance tuning
tests, benchmarks or troubleshooting.

Dstat allows you to view all of your system resources in real-time, you can,
e.g., compare disk utilization in combination with interrupts from your IDE
controller, or compare the network bandwidth numbers directly with the disk
throughput (in the same interval).")
    (home-page "http://dag.wiee.rs/home-made/dstat/")
    (license license:gpl2+)))

(define-public thefuck
  (package
    (name "thefuck")
    (version "3.25")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/nvbn/thefuck/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "088bn2l1376qlndbpnjya4q1x3913nj3yj3wc7s2w3bz66d23skk"))
              (patches (search-patches "thefuck-test-environ.patch"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Tests look for installed package
             (add-installed-pythonpath inputs outputs)
             ;; Some tests need write access to $HOME.
             (setenv "HOME" "/tmp")
             (zero? (system* "py.test" "-v")))))))
    (propagated-inputs
     `(("python-colorama" ,python-colorama)
       ("python-decorator" ,python-decorator)
       ("python-psutil" ,python-psutil)
       ("python-pyte" ,python-pyte)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-mock" ,python-pytest-mock)))
    (home-page "https://github.com/nvbn/thefuck")
    (synopsis "Correct mistyped console command")
    (description
     "The Fuck tries to match a rule for a previous, mistyped command, creates
a new command using the matched rule, and runs it.")
    (license license:x11)))

(define-public di
  (package
    (name "di")
    (version "4.44")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gentoo.com/di/di-" version ".tar.gz"))
       (sha256
        (base32 "0803lp8kd3mp1jcm17i019xiqxdy85hhs6xk67zib8gmvg500gcn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; Obscure test failures.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'setup-environment
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             (setenv "prefix" (assoc-ref outputs "out"))
             #t)))
       #:make-flags (list "--environment-overrides")))
    (home-page "https://www.gentoo.com/di/")
    (synopsis "Advanced df like disk information utility")
    (description
     "'di' is a disk information utility, displaying everything that your
@code{df} command does and more.  It features the ability to display your disk
usage in whatever format you prefer.  It is designed to be highly portable and
produce uniform output across heterogeneous networks.")
    (license license:zlib)))

(define-public cbatticon
  (package
    (name "cbatticon")
    (version "1.6.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/valr/"
                                  name "/archive/" version ".tar.gz"))
              (sha256
               (base32
                "1s2n49ydh7pznnf02fak4yy0wqkgi9ag7yiw1zg1lhp4m0h37hyh"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure script
    (inputs
     `(("gtk+" ,gtk+)
       ("gettext" ,gettext-minimal)
       ("libnotify" ,libnotify)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Lightweight battery icon for the system tray")
    (description "cbatticon is a lightweight battery icon that displays
the status of your battery in the system tray.")
    (home-page "https://github.com/valr/cbatticon")
    (license license:gpl2+)))

(define-public interrobang
  (let ((revision "1")
        (commit "896543735e1c99144765fdbd7b6e6b5afbd8b881"))
    (package
      (name "interrobang")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/TrilbyWhite/interrobang")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "1n13m70p1hfba5dy3i8hfclbr6k9q3d9dai3dg4jvhdhmxcpjzdf"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)) ; no configure script
         #:make-flags (list (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))))
      (inputs
       `(("libx11" ,libx11)))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (synopsis "Scriptable launcher menu")
      (description "Interrobang is a scriptable launcher menu with a customizable
shortcut syntax and completion options.")
      (home-page "https://github.com/TrilbyWhite/interrobang")
      (license license:gpl3+))))

(define-public pam-krb5
  (package
    (name "pam-krb5")
    (version "4.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://archives.eyrie.org/software/kerberos/" name "-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1qjp8i1s9bz7g6kiqrkzzkxn5pfspa4sy53b6z40fqmdf9przdfb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-tests
           (lambda _
             ;; The build container seems to interfere with some tests.
             (substitute* "tests/TESTS"
               (("module/basic\n")  ""))
             (substitute* "tests/TESTS"
               (("pam-util/vector\n")  ""))
             #t)))))
    (inputs
     `(("linux-pam" ,linux-pam)
       ("mit-krb5" ,mit-krb5)))
    (native-inputs
     `(("perl" ,perl)
       ("perl-test-pod" ,perl-test-pod))) ; required for tests
    (synopsis "Kerberos PAM module")
    (description
     "Pam-krb5 is a Kerberos PAM module for either MIT Kerberos or Heimdal.
It supports ticket refreshing by screen savers, configurable
authorization handling, authentication of non-local accounts for network
services, password changing, and password expiration, as well as all the
standard expected PAM features.  It works correctly with OpenSSH, even
with @code{ChallengeResponseAuthentication} and @code{PrivilegeSeparation}
enabled, and supports extensive configuration either by PAM options or in
krb5.conf or both.  PKINIT is supported with recent versions of both MIT
Kerberos and Heimdal and FAST is supported with recent MIT Kerberos.")
    (home-page "http://www.eyrie.org/~eagle/software/pam-krb5")
    ;; Dual licenced under  a homebrew non-copyleft OR GPL (any version)
    ;; However, the tarball does not contain a copy of the GPL,  so unless
    ;; we put one in, we cannot distribute it under GPL without violating
    ;; clause requiring us to give all recipients a copy.
    (license license:gpl1+)))

(define-public sunxi-tools
  (package
    (name "sunxi-tools")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/linux-sunxi/"
                           "sunxi-tools/archive/v" version ".tar.gz"))
       (sha256
        (base32 "08iqwj95qw2s7ilhrdi2lkbc8dx64zk5lzz1qk587jr0lla81x41"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove binaries contained in the tarball which are only for the
        ;; target and can be regenerated anyway.
        '(delete-file-recursively "bin"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("cross-gcc" ,(cross-gcc "arm-linux-gnueabihf"
                                #:xbinutils (cross-binutils "arm-linux-gnueabihf")
                                #:libc (cross-libc "arm-linux-gnueabihf")))
       ("cross-libc" ,(cross-libc "arm-linux-gnueabihf")) ; header files
       ("cross-libc-static" ,(cross-libc "arm-linux-gnueabihf") "static")))
    (inputs
     `(("libusb" ,libusb)))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests exist
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          (string-append "CROSS_COMPILE="
                                         "arm-linux-gnueabihf-")
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'set-environment-up
           (lambda* (#:key make-flags #:allow-other-keys)
             (define (cross? x)
               (string-contains x "cross-arm-linux"))
             (define (filter-environment! filter-predicate
                                          environment-variable-names)
               (for-each
                (lambda (env-name)
                  (let* ((env-value (getenv env-name))
                         (search-path (search-path-as-string->list env-value))
                         (new-search-path (filter filter-predicate
                                                  search-path))
                         (new-env-value (list->search-path-as-string
                                         new-search-path ":")))
                    (setenv env-name new-env-value)))
                environment-variable-names))
             (setenv "CROSS_C_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))
             (setenv "CROSS_CPLUS_INCLUDE_PATH" (getenv "CPLUS_INCLUDE_PATH"))
             (setenv "CROSS_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
             (filter-environment! cross?
              '("CROSS_C_INCLUDE_PATH" "CROSS_CPLUS_INCLUDE_PATH"
                "CROSS_LIBRARY_PATH"))
             (filter-environment! (lambda (e) (not (cross? e)))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
             #t))
         (replace 'build
           (lambda* (#:key make-flags #:allow-other-keys)
             (zero? (apply system* "make" "tools" "misc" make-flags))))
         (add-after 'build 'build-armhf
           (lambda* (#:key make-flags #:allow-other-keys)
             (setenv "LIBRARY_PATH" #f)
             (zero? (apply system* "make" "target-tools" make-flags))))
         (replace 'install
           (lambda* (#:key make-flags #:allow-other-keys)
             (zero? (apply system* "make" "install-all" "install-misc"
                           make-flags)))))))
    (home-page "https://github.com/linux-sunxi/sunxi-tools")
    (synopsis "Hardware management tools for Allwinner computers")
    (description "This package contains tools for Allwinner devices:
@enumerate
@item @command{sunxi-fexc}, @command{bin2fex}, @command{fex2bin}: Compile
a textual description of a board (.fex) to a binary representation (.bin).
@item @command{sunxi-fel}: Puts an Allwinner device into FEL mode which
makes it register as a special USB device (rather than USB host).
You can then connect it to another computer and flash it from there.
@item @command{sunxi-nand-part}: Partitions NAND flash.
@item @command{sunxi-bootinfo}: Reads out boot0 and boot1 (Allwinner
bootloader) parameters.
@item @command{sunxi-pio}: Sets GPIO parameters and oscillates a GPIO
in order to be able to find it.
@item @command{sunxi-meminfo}: Prints memory bus settings.
@item @command{sunxi-nand-image-builder}: Prepares raw NAND images.
@end enumerate")
    (license license:gpl2+)))

(define-public sedsed
  (package
    (name "sedsed")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/aureliojargas/sedsed/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0139jkqvm8ipiwfj7k69ry2f9b1ffgpk79arpz4r7w9kf6h23bnh"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; No tests.
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sed-in
           (lambda _
             (substitute* "sedsed.py"
               (("sedbin = 'sed'")
                (string-append "sedbin = '" (which "sed") "'")))
             #t))
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               ;; Just one file to copy around
               (install-file "sedsed.py" bin)
               #t)))
         (add-after 'install 'symlink
           ;; Create 'sedsed' symlink to "sedsed.py".
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (sed (string-append bin "/sedsed"))
                    (sedpy (string-append bin "/sedsed.py")))
               (symlink  sedpy sed)
               #t))))))
    (home-page "http://aurelio.net/projects/sedsed")
    (synopsis "Sed sed scripts")
    (description
     "@code{sedsed} can debug, indent, tokenize and HTMLize your sed(1) script.

In debug mode it reads your script and add extra commands to it.  When
executed you can see the data flow between the commands, revealing all the
magic sed does on its internal buffers.

In indent mode your script is reformatted with standard spacing.

In tokenize mode you can see the elements of every command you use.

In HTMLize mode your script is converted to a beautiful colored HTML file,
with all the commands and parameters identified for your viewing pleasure.

With sedsed you can master any sed script.  No more secrets, no more hidden
buffers.")
    (license license:expat)))

(define-public intel-gpu-tools
  (package
    (name "intel-gpu-tools")
    (version "1.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cgit.freedesktop.org/xorg/app/"
                                  "intel-gpu-tools/snapshot/"
                                  "intel-gpu-tools-" version ".tar.gz"))
              (sha256
               (base32
                "1xfy4cgimyyn5qixlrfkadgnl9qwbk30vw8k80g8vjnrcc4hx986"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; many of the tests try to load kernel modules
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _
             ;; Don't run configure in this phase.
             (setenv "NOCONFIGURE" "1")
             (invoke "sh" "autogen.sh"))))))
    (inputs
     `(("util-macros" ,util-macros)
       ("libdrm" ,libdrm)
       ("libpciaccess" ,libpciaccess)
       ("kmod" ,kmod)
       ("procps" ,procps)
       ("cairo" ,cairo)
       ("libunwind" ,libunwind)
       ("libxrandr" ,libxrandr)
       ("glib" ,glib)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://cgit.freedesktop.org/xorg/app/intel-gpu-tools/")
    (synopsis "Tools for development and testing of the Intel DRM driver")
    (description "Intel GPU Tools is a collection of tools for development and
testing of the Intel DRM driver.  There are many macro-level test suites that
get used against the driver, including xtest, rendercheck, piglit, and
oglconform, but failures from those can be difficult to track down to kernel
changes, and many require complicated build procedures or specific testing
environments to get useful results.  Therefore, Intel GPU Tools includes
low-level tools and tests specifically for development and testing of the
Intel DRM Driver.")
    (license license:expat)))

(define-public fabric
  (package
    (name "fabric")
    (version "1.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Fabric" version))
       (sha256
        (base32
         "0k944dxr41whw7ib6380q9x15wyskx7fqni656icdn8rzshn9bwq"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))             ; Python 2 only
    (native-inputs
     `(("python2-fudge" ,python2-fudge)
       ("python2-jinja2" ,python2-jinja2)
       ("python2-nose" ,python2-nose)))
    (propagated-inputs
     `(("python2-paramiko" ,python2-paramiko)))
    (home-page "http://fabfile.org")
    (synopsis "Simple Pythonic remote execution and deployment tool")
    (description
     "Fabric is designed to upload files and run shell commands on a number of
servers in parallel or serially.  These commands are grouped in tasks (which
are regular Python functions) and specified in a @dfn{fabfile}.

It is similar to Capistrano, except it's implemented in Python and doesn't
expect you to be deploying Rails applications.  Fabric is a simple, Pythonic
tool for remote execution and deployment.")
    (license license:bsd-2)))

(define-public neofetch
  (package
    (name "neofetch")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/dylanaraps/neofetch/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15p69q0jchfms1fpb4i7kq8b28w2xpgh2zmynln618qxv1myf228"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:make-flags
       (list (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-target-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("\\$\\(DESTDIR\\)/etc/")
                  "$(PREFIX)/etc/"))
               (substitute* "neofetch"
                 (("\"/etc/neofetch")
                  (string-append "\"" out "/etc/neofetch"))
                 (("\"/usr/share/neofetch")
                  (string-append "\"" out "/share/neofetch"))))
             #t))
         (delete 'configure)            ; no configure script
         (replace 'install
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version))
                    (etc (string-append doc "/examples/etc")))
               (zero? (apply system* `("make" ,@make-flags
                                       ,(string-append "SYSCONFDIR=" etc)
                                       "install")))))))))
    (home-page "https://github.com/dylanaraps/neofetch")
    (synopsis "System info script")
    (description "Neofetch is a CLI system information tool written in Bash.
Neofetch displays information about your system next to an image, your OS
logo, or any ASCII file of your choice.  The main purpose of Neofetch is to be
used in screenshots to show other users what operating system or distribution
you are running, what theme or icon set you are using, etc.")
    (license license:expat)))

(define-public nnn
  (package
    (name "nnn")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jarun/nnn/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zswf8lb29zr1z642i1d0zi1y2mxal8qjqdrpdiqjh197jamj3zm"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("readline" ,readline)))
    (arguments
     '(#:tests? #f ; no tests
       #:phases
       ;; We do not provide `ncurses.h' within an `ncursesw'
       ;; sub-directory, so patch the source accordingly.  See
       ;; <http://bugs.gnu.org/19018>.
       ;; Thanks to gtypist maintainer.
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-curses-lib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("-lncursesw")
                  "-lncurses"))
               (substitute* "nnn.c"
                 (("ncursesw\\/curses.h")
                  "ncurses.h")))
             #t))
         (delete 'configure))
       #:make-flags
       (list
        (string-append "PREFIX="
                       (assoc-ref %outputs "out"))
        (string-append "-Wl,-rpath="
                       %output "/lib")
        "CC=gcc")))
    (home-page "https://github.com/jarun/nnn")
    (synopsis "Terminal file browser")
    (description "@command{nnn} is a fork of @command{noice}, a blazing-fast
lightweight terminal file browser with easy keyboard shortcuts for
navigation, opening files and running tasks.  There is no config file and
mime associations are hard-coded.  The incredible user-friendliness and speed
make it a perfect utility on modern distros.")
    (license license:bsd-2)))

(define-public thermald
  (package
    (name "thermald")
    (version "1.7.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/01org/thermal_daemon/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32
               "0isgmav3z3nb5bsdya8m3haqhzj1lyfjx7i812cqfjrh2a9msin4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'autogen.sh-and-fix-paths
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out")))
                       ;; XXX this can probably be removed after version 1.7.1.
                       ;; upstartconfir is hardcoded to /etc/init and the build
                       ;; system tries to mkdir that.  We don't even need upstart
                       ;; files at all; this is a fast and kludgy workaround
                       (substitute* "data/Makefile.am"
                         (("upstartconfdir = /etc/init")
                          (string-append "upstartconfdir = "
                                         out "/etc/init")))
                       ;; Now run autogen
                       (invoke "sh" "autogen.sh")
                       #t))))
       #:configure-flags
       (let ((out      (assoc-ref %outputs "out")))
         (list (string-append "--sysconfdir="
                              out "/etc")
               (string-append "--with-udev-dir="
                              out "/lib/udev")
               (string-append "--with-dbus-sys-dir="
                              out "/etc/dbus-1/system.d")
               "--localstatedir=/var"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("libxml2" ,libxml2)))
    (home-page "https://01.org/linux-thermal-daemon/")
    (synopsis "CPU scaling for thermal management")
    (description "The Linux Thermal Daemon helps monitor and control temperature
on systems running the Linux kernel.")
    ;; arm and aarch64 don't have cpuid.h
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:gpl2+)))

(define-public masscan
  (package
    (name "masscan")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/robertdavidgraham/masscan"
                                  "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1y9af345g00z83rliv6bmlqg37xwc7xpnx5xqdgmjikzcxgk9pji"))))
    (build-system gnu-build-system)
    (inputs
     `(("libpcap" ,libpcap)))
    (arguments
     '(#:test-target "regress"
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no ./configure script
         (add-after 'unpack 'patch-path
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pcap (assoc-ref inputs "libpcap")))
               (substitute* "src/rawsock-pcap.c"
                 (("libpcap.so") (string-append pcap "/lib/libpcap.so")))
               #t))))))
    (synopsis "TCP port scanner")
    (description "MASSCAN is an asynchronous TCP port scanner.  It can detect
open ports, and also complete the TCP connection and interact with the remote
application, collecting the information received.")
    (home-page "https://github.com/robertdavidgraham/masscan")
        ;; 'src/siphash24.c' is the SipHash reference implementation, which
        ;; bears a CC0 Public Domain Dedication.
    (license license:agpl3+)))

(define-public hungrycat
  (package
    (name "hungrycat")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jwilk/hungrycat/"
                                  "releases/download/" version "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "03fc1zsrf99lvxa7b4ps6pbi43304wbxh1f6ci4q0vkal370yfwh"))))
    (build-system gnu-build-system)
    (native-inputs
     ;; For tests.
     `(("python" ,python-wrapper)
       ("python-nose" ,python-nose)))
    (arguments
     `(#:test-target "test"))
    (synopsis "A single tool that combines @command{cat} & @command{rm}")
    (description
     "hungrycat prints the contents of a file to standard output, while
simultaneously freeing the disk space it occupied.  It is useful if you need
to process a large file, don't have enough space to store both the input and
output files, and don't need the input file afterwards.
While similar in principle to running @command{cat} immediately followed by
@command{rm}, @command{hungrycat} actually frees blocks as soon as they are
printed instead of after the entire file has been read, which is often too
late.")
    (home-page "https://jwilk.net/software/hungrycat")
    (license license:expat)))
