;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
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
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages python)
  #:use-module (gnu packages man))

(define-public dmd
  (package
    (name "dmd")
    (version "0.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://alpha.gnu.org/gnu/dmd/dmd-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "10fl4k96f17gqx2fv8iw9c61ld26gsk4bbrlfqckdmiimz1k175z"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--localstatedir=/var")))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("guile" ,guile-2.0)))
    (synopsis "Daemon managing daemons")
    (description
     "GNU DMD is a daemon-managing daemon, meaning that it manages the
execution of system services, replacing similar functionality found in
typical init systems.  It provides dependency-handling through a convenient
interface and is based on GNU Guile.")
    (license license:gpl3+)
    (home-page "http://www.gnu.org/software/dmd/")))

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
   (native-inputs `(("gettext" ,gnu-gettext)))
   (home-page "http://projects.gw-computing.net/projects/dfc")
   (synopsis "Display file system space usage using graphs and colors")
   (description
    "dfc (df color) is a modern version of df.  It uses colors, draws pretty
graphs and can export its output to different formats.")
   (license license:bsd-3)))

(define-public htop
  (package
   (name "htop")
   (version "1.0.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://hisham.hm/htop/releases/"
                  version "/htop-" version ".tar.gz"))
            (sha256
             (base32
              "0a8qbpsifzjwc4f45xfwm48jhm59g6q5hlib4bf7z13mgy95fp05"))))
   (build-system gnu-build-system)
   (inputs
    `(("ncurses" ,ncurses)))
   (home-page "http://htop.sourceforge.net/")
   (synopsis "Interactive process viewer")
   (description
    "This is htop, an interactive process viewer.  It is a text-mode
application (for console or X terminals) and requires ncurses.")
   (license license:gpl2)))

(define-public pies
  (package
    (name "pies")
    (version "1.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/pies/pies-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "18w0dbg77i56cx1bwa789w0qi3l4xkkbascxcv2b6gbm0zmjg1g6"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/pies/")
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
    (arguments `(;; FIXME: `tftp.sh' relies on `netstat' from utils-linux,
                 ;; which is currently missing.
                 #:tests? #f))
    (inputs `(("ncurses" ,ncurses)
              ("readline" ,readline)))            ; for 'ftp'
    (home-page "http://www.gnu.org/software/inetutils/")
    (synopsis "Basic networking utilities")
    (description
     "Inetutils is a collection of common network programs, such as an ftp
client and server, a telnet client and server, and an rsh client and server.")
    (license license:gpl3+)))

(define-public shadow
  (package
    (name "shadow")
    (version "4.1.5.1")
    (source (origin
             ;; Shadow has no real upstream, and not even tarballs.
             ;; See <https://lists.gnu.org/archive/html/guix-devel/2014-03/msg00233.html>.
             (method git-fetch)
             (uri (git-reference
                   (url "git://git.debian.org/git/pkg-shadow/shadow")
                   (commit (string-append "upstream/" version))))
             (sha256
              (base32
               "1xx85d83kmacmjzqbamgydcjkwsqd5fi1s2wgwx6myq5wa39qx0n"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Assume System V `setpgrp (void)', which is the default on GNU
       ;; variants (`AC_FUNC_SETPGRP' is not cross-compilation capable.)
       #:configure-flags '("--with-libpam" "ac_cv_func_setpgrp_void=yes")

       #:phases (alist-cons-before
                 'build 'set-nscd-file-name
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Use the right file name for nscd.
                   (let ((libc (assoc-ref inputs "libc")))
                     (substitute* "lib/nscd.c"
                       (("/usr/sbin/nscd")
                        (string-append libc "/sbin/nscd")))))
                 (alist-cons-after
                  'install 'remove-groups
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; Remove `groups', which is already provided by Coreutils.
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin"))
                           (man (string-append out "/share/man/man1")))
                      (delete-file (string-append bin "/groups"))
                      (for-each delete-file (find-files man "^groups\\."))
                      #t))
                  %standard-phases))))

    (inputs (if (string-suffix? "-linux"
                                (or (%current-target-system)
                                    (%current-system)))
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
             (uri (string-append "mirror://sourceforge/mingetty/mingetty-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "05yxrp44ky2kg6qknk1ih0kvwkgbn9fbz77r3vci7agslh5wjm8g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-replace 'configure
                               (lambda* (#:key inputs outputs
                                         #:allow-other-keys)
                                 (let* ((out    (assoc-ref outputs "out"))
                                        (man8   (string-append
                                                 out "/share/man/man8"))
                                        (sbin   (string-append out "/sbin"))
                                        (shadow (assoc-ref inputs "shadow"))
                                        (login  (string-append shadow
                                                               "/bin/login")))
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
                                   (mkdir-p man8)))
                               %standard-phases)
       #:tests? #f))                              ; no tests
    (inputs `(("shadow" ,shadow)))

    (home-page "http://sourceforge.net/projects/mingetty")
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
             (uri (string-append "mirror://sourceforge/netcat/netcat-"
                                 version ".tar.bz2"))
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
    (home-page "http://www.gnu.org/software/alive/")
    (synopsis "Autologin and keep-alive daemon")
    (description
     "GNU Alive sends periodic pings to a server, generally to keep a
connection alive.")
    (license license:gpl3+)))

(define-public isc-dhcp
  (package
    (name "isc-dhcp")
    (version "4.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.isc.org/isc/dhcp/"
                                  version "/dhcp-" version ".tar.gz"))
              (sha256
               (base32
                "1w4s7sni1m9223ya8m2a64lr62845c6xlraprjf8zfx6lylbqv16"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-after
                 'configure 'post-configure
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

                     (system* "tar" "xf" "bind.tar.gz")
                     (for-each patch-shebang
                               (find-files "bind-9.9.5-P1" ".*"))
                     (zero? (system* "tar" "cf" "bind.tar.gz"
                                     "bind-9.9.5-P1"
                                     ;; avoid non-determinism in the archive
                                     "--mtime=@0"
                                     "--owner=root:0"
                                     "--group=root:0"))))
                 (alist-cons-after
                  'install 'post-install
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
                                (list inetutils net-tools coreutils sed))))))
                  %standard-phases))))

    (native-inputs `(("perl" ,perl)))

    (inputs `(("inetutils" ,inetutils)
              ("net-tools" ,net-tools)
              ("iproute" ,iproute)

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
    (license license:isc)))

(define-public libpcap
  (package
    (name "libpcap")
    (version "1.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.tcpdump.org/release/libpcap-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14wyjywrdi1ikaj6yc9c72m6m2r64z94lb0gm7k1a3q6q5cj3scs"))))
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
    (version "4.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.tcpdump.org/release/tcpdump-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15hb7zkzd66nag102qbv100hcnf7frglbkylmr8adwr8f5jkkaql"))))
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
                                  "clusterssh-" version ".tar.gz"))
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
       (alist-cons-after
        'install 'set-load-paths
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
             `("PERL5LIB" ":" prefix (,x11-inc ,tk-inc)))))
        %standard-phases)))
    ;; The clusterssh.sourceforge.net address requires login to view
    (home-page "http://sourceforge.net/projects/clusterssh/")
    (synopsis "Secure concurrent multi-server terminal control")
    (description
     "ClusterSSH controls a number of xterm windows via a single graphical
console window to allow commands to be interactively run on multiple servers
over ssh connections.")
    (license license:gpl2+)))

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
     '(#:configure-flags (list (string-append "ROTT_ETCDIR="
                                              (assoc-ref %outputs "out")
                                              "/etc")
                               "--localstatedir=/var")
       #:phases (alist-cons-after
                 'install 'install-info
                 (lambda _
                   (zero? (system* "make" "install-info")))
                 %standard-phases)))
    (native-inputs `(("texinfo" ,texinfo)
                     ("util-linux" ,util-linux))) ; for 'cal'
    (home-page "http://www.gnu.org/software/rottlog/")
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
    (version "1.8.10p3")
    (source (origin
              (method url-fetch)
              (uri
               (list (string-append "http://www.sudo.ws/sudo/dist/sudo-"
                                    version ".tar.gz")
                     (string-append "ftp://ftp.sudo.ws/pub/sudo/OLD/sudo-"
                                    version ".tar.gz")))
              (sha256
               (base32
                "002l6h27pnhb77b65frhazbhknsxvrsnkpi43j7i0qw1lrgi7nkf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-logpath=/var/log/sudo.log"
                           "--with-rundir=/run/sudo"
                           "--with-vardir=/var/db/sudo"
                           "--with-iologdir=/var/log/sudo-io")
       #:phases (alist-cons-before
                 'configure 'pre-configure
                 (lambda _
                   (substitute* "configure"
                     ;; Refer to the right executables.
                     (("/usr/bin/mv") (which "mv"))
                     (("/usr/bin/sh") (which "sh")))
                   (substitute* (find-files "." "Makefile\\.in")
                     (("-O [[:graph:]]+ -G [[:graph:]]+")
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
                      "$(TMPDIR)/dummy")))
                 %standard-phases)

       ;; XXX: The 'testsudoers' test series expects user 'root' to exist, but
       ;; the chroot's /etc/passwd doesn't have it.  Turn off the tests.
       #:tests? #f))
    (inputs
     `(("groff" ,groff)
       ("linux-pam" ,linux-pam)
       ("coreutils" ,coreutils)))
    (home-page "http://www.sudo.ws/")
    (synopsis "Run commands as root")
    (description
     "Sudo (su \"do\") allows a system administrator to delegate authority to
give certain users (or groups of users) the ability to run some (or all)
commands as root or another user while providing an audit trail of the
commands and their arguments.")

    ;; See <http://www.sudo.ws/sudo/license.html>.
    (license license:x11)))

(define-public wpa-supplicant-light
  (package
    (name "wpa-supplicant-light")
    (version "2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://w1.fi/releases/wpa_supplicant-"
                    version
                    ".tar.gz"))
              (sha256
               (base32
                "08li21q1wjn5chrv289w666il9ah1w419y3dkq2rl4wnq0rci385"))
              (patches
               (map search-patch '("wpa-supplicant-CVE-2015-1863.patch"
                                   "wpa-supplicant-2015-2-fix.patch"
                                   "wpa-supplicant-2015-3-fix.patch"
                                   "wpa-supplicant-2015-4-fix-pt1.patch"
                                   "wpa-supplicant-2015-4-fix-pt2.patch"
                                   "wpa-supplicant-2015-4-fix-pt3.patch"
                                   "wpa-supplicant-2015-4-fix-pt4.patch"
                                   "wpa-supplicant-2015-4-fix-pt5.patch"
                                   "wpa-supplicant-2015-5-fix.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-replace
                 'configure
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
                     (close-port port)))

                 (alist-cons-after
                  'install 'install-man-pages
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out  (assoc-ref outputs "out"))
                           (man  (string-append out "/share/man"))
                           (man5 (string-append man "/man5"))
                           (man8 (string-append man "/man8")))
                      (define (copy-man-page target)
                        (lambda (file)
                          (copy-file file
                                     (string-append target "/"
                                                    (basename file)))))

                      (mkdir-p man5) (mkdir man8)
                      (for-each (copy-man-page man5)
                                (find-files "doc/docbook" "\\.5"))
                      (for-each (copy-man-page man8)
                                (find-files "doc/docbook" "\\.8"))
                      #t))
                  %standard-phases))

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
  (package (inherit wpa-supplicant-light)
    (name "wpa-supplicant")
    (inputs `(("dbus" ,dbus)
              ,@(package-inputs wpa-supplicant-light)))
    (arguments
     (substitute-keyword-arguments (package-arguments wpa-supplicant-light)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after
            'configure 'configure-for-dbus
            (lambda _
              (let ((port (open-file ".config" "al")))
                (display "
      CONFIG_CTRL_IFACE_DBUS=y
      CONFIG_CTRL_IFACE_DBUS_NEW=y
      CONFIG_CTRL_IFACE_DBUS_INTRO=y\n" port)
                (close-port port))
              #t))
           (add-after
            'install-man-pages 'install-dbus-conf
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (conf-dir (string-append out "/etc/dbus-1/system.d"))
                     (service-dir
                      (string-append out "/share/dbus-1/system-services")))
                (mkdir-p conf-dir)
                (copy-file "dbus/dbus-wpa_supplicant.conf"
                           (string-append conf-dir "/wpa_supplicant.conf"))
                (mkdir-p service-dir)
                (for-each (lambda (file)
                            (copy-file
                             file
                             (string-append service-dir "/" (basename file))))
                          (find-files "dbus" "\\.service$"))
                #t)))))))))

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
     '(#:phases (alist-replace
                 'configure
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
                                              "/share/man")))))
                 %standard-phases)
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
    (version "2.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://savannah/dmidecode/dmidecode-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "122hgaw8mpqdfra159lfl6pyk3837giqx6vq42j64fjnbl2z6gwi"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-delete 'configure %standard-phases)
       #:tests? #f                                ; no 'check' target
       #:make-flags (list (string-append "prefix="
                                         (assoc-ref %outputs "out")))))
    (home-page "http://www.nongnu.org/dmidecode/")
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
    (version "20150410")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://acpica.org/sites/acpica/files/acpica-unix2-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0q1fjwkyw9x6gsva6fd0zbn7ly4fx0ha4853f416np9kf2irillw"))))
    (build-system gnu-build-system)
    (native-inputs `(("flex" ,flex)
                     ("bison" ,bison)))
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" %output)
                          "HOST=_LINUX"
                          "OPT_CFLAGS=-Wall -fno-strict-aliasing")
       #:tests? #f  ; no 'check' target.
       #:phases (alist-delete 'configure %standard-phases)))
    (home-page "http://acpica.org/")
    (synopsis "Tools for the development and debug of ACPI tables")
    (description
     "The ACPI Component Architecture (ACPICA) project provides an
OS-independent reference implementation of the Advanced Configuration and
Power Interface Specification (ACPI).  ACPICA code contains those portions of
ACPI meant to be directly integrated into the host OS as a kernel-resident
subsystem, and a small set of tools to assist in developing and debugging ACPI
tables.  This package contains only the user-space tools needed for ACPI table
development, not the kernel implementation of ACPI.")
    (license license:gpl2)))  ; Dual GPLv2/ACPICA Licence

(define-public stress
  (package
    (name "stress")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/main/s/stress/stress_"
                                  version ".orig.tar.gz"))
              (sha256
               (base32
                "1v9vnzlihqfjsxa93hdbrq72pqqk00dkylmlg8jpxhm7s1w9qfl1"))))
    (build-system gnu-build-system)
    (home-page "http://packages.debian.org/wheezy/stress")
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
              (uri (string-append "mirror://sourceforge/detox/detox-"
                                  version ".tar.bz2"))
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
    (version "6.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.cgsecurity.org/testdisk-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0v1jap83f5h99zv01v3qmqm160d36n4ysi0gyq7xzb3mqgmw75x5"))))
    (build-system gnu-build-system)
    (inputs
     `(;; ("ntfs" ,ntfs)
       ("util-linux" ,util-linux)
       ("openssl" ,openssl)
       ("zlib" ,zlib)
       ("e2fsprogs" ,e2fsprogs)
       ("libjpeg" ,libjpeg)
       ("ncurses" ,ncurses)))
    (home-page "http://www.cgsecurity.org/wiki/TestDisk")
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
     '(#:phases (alist-delete 'configure %standard-phases)
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
    (version "5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/direvent/direvent-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1i14131y6m8wvirz6piw4zxz2q1kbpl0lniv5kl55rx4k372dg8z"))
              (modules '((guix build utils)))
              (snippet '(substitute* "tests/testsuite"
                          (("#![[:blank:]]?/bin/sh")
                           "#!$SHELL")))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'build 'patch-/bin/sh
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Use the right shell when executing the watcher.
                   (let ((bash (assoc-ref inputs "bash")))
                     (substitute* "src/direvent.c"
                       (("\"/bin/sh\"")
                        (string-append "\"" bash "/bin/sh\"")))))
                 %standard-phases)))
    (home-page "http://www.gnu.org/software/direvent/")
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
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://people.redhat.com/sgrubb/libcap-ng/libcap-ng-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0ssvnh4cvhya0c1j6k6192zvqcq7nc0x01fb5nwhr0prfqr0i8j8"))))
    (build-system gnu-build-system)
    (inputs `(("python" ,python)))
    (home-page "http://people.redhat.com/sgrubb/libcap-ng/")
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
    (version "6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/smartmontools/smartmontools/"
                    version "/smartmontools-" version ".tar.gz"))
              (sha256
               (base32
                "06gy71jh2d3gcfmlbbrsqw7215knkfq59q3j6qdxfrar39fhcxx7"))))
    (build-system gnu-build-system)
    (inputs `(("libcap-ng" ,libcap-ng)))
    (home-page "http://www.smartmontools.org/")
    (synopsis "S.M.A.R.T. harddisk control and monitoring tools")
    (description
     "The smartmontools package contains utility programs to control and
monitor storage systems using the Self-Monitoring, Analysis and Reporting
Technology System (S.M.A.R.T.) built into most modern ATA and SCSI harddisks.
In many cases, these utilities will provide advanced warning of disk
degradation and failure.")
    (license license:gpl2+)))

(define-public fdupes
  (package
    (name "fdupes")
    (version "1.51")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/adrianlopezroche/fdupes/archive/fdupes-"
             version ".tar.gz"))
       (sha256
        (base32
         "11j96vxl9vg3jsnxqxskrv3gad6dh7hz2zpyc8n31xzyxka1c7kn"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-delete 'configure %standard-phases)
       #:tests? #f ; no 'check' target
       #:make-flags (list (string-append "PREFIX="
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
   (inputs
    `(("python-setuptools" ,python2-setuptools)))
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
    (version "1.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/a/ansible/ansible-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "007fzgsqaahb0y4gjdxxmir9kcni7wph2z14jhqgpz88idrz8pn2"))))
    (build-system python-build-system)
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)
       ("python2-pycrypto" ,python2-pycrypto)
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
    (home-page "http://ansible.com/")
    (synopsis "Radically simple IT automation")
    (description "Ansible is a radically simple IT automation system.  It
handles configuration-management, application deployment, cloud provisioning,
ad-hoc task-execution, and multinode orchestration - including trivializing
things like zero downtime rolling updates with load balancers.")
    (license license:gpl3+)))
