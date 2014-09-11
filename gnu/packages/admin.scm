;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
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
  #:use-module ((gnu packages compression)
                #:renamer (symbol-prefix-proc 'c:))
  #:use-module ((gnu packages openssl)
                #:renamer (symbol-prefix-proc 'o:))
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
  #:use-module (gnu packages xorg))

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
    (license gpl3+)
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
   (license bsd-3)))

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
   (license gpl2)))

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
    (license gpl3+)))

(define-public inetutils
  (package
    (name "inetutils")
    (version "1.9.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/inetutils/inetutils-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "04wrm0v7l4890mmbaawd6wjwdv08bkglgqhpz0q4dkb0l50fl8q4"))))
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
    (license gpl3+)))

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
    (license bsd-3)))

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
    (license gpl2+)))

(define-public net-base
  (package
    (name "net-base")
    (version "5.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://debian/pool/main/n/netbase/netbase_"
                   version ".tar.gz"))
             (sha256
              (base32
               "01rkvqrg7krkx8b432nz6bpi8w3s4cm5q5r891k23cdrc9nsaayn"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let* ((source (assoc-ref %build-inputs "source"))
                          (tar    (assoc-ref %build-inputs "tar"))
                          (gzip   (assoc-ref %build-inputs "gzip"))
                          (output (assoc-ref %outputs "out"))
                          (etc    (string-append output "/etc")))
                     (setenv "PATH" (string-append gzip "/bin"))
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
                     ("gzip" ,c:gzip)))
    (synopsis "IANA protocol, port, and RPC number assignments")
    (description
     "This package provides the /etc/services, /etc/protocols, and /etc/rpc
files, which contain information about the IANA-assigned port, protocol, and
ONC RPC numbers")
    (home-page "http://packages.debian.org/sid/netbase")
    (license gpl2)))

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
programs and scripts. At the same time, it is a feature-rich network debugging
and exploration tool, since it can create almost any kind of connection you
would need and has several interesting built-in capabilities.")
    (license gpl2+)))

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
    (license gpl3+)))

(define-public isc-dhcp
  (package
    (name "isc-dhcp")
    (version "4.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.isc.org/isc/dhcp/"
                                  version "/dhcp-" version ".tar.gz"))
              (sha256
               (base32
                "12mydvj6x3zcl3gla06bywfkkrgg03g66fijs94mwb7kbiym3dm7"))))
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
                               (find-files "bind-9.9.5" ".*"))
                     (zero? (system* "tar" "cf" "bind.tar.gz"
                                     "bind-9.9.5"))))
                 (alist-cons-after
                  'install 'post-install
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    ;; Install the dhclient script for GNU/Linux and make sure
                    ;; if finds all the programs it needs.
                    (let* ((out       (assoc-ref outputs "out"))
                           (libexec   (string-append out "/libexec"))
                           (coreutils (assoc-ref inputs "coreutils"))
                           (net-tools (assoc-ref inputs "net-tools"))
                           (sed       (assoc-ref inputs "sed")))
                      (substitute* "client/scripts/linux"
                        (("/sbin/ip")
                         (string-append (assoc-ref inputs "iproute")
                                        "/sbin/ip")))

                      (mkdir-p libexec)
                      (copy-file "client/scripts/linux"
                                 (string-append libexec "/dhclient-script"))

                      (wrap-program (string-append libexec "/dhclient-script")
                                    `("PATH" ":" prefix
                                      ,(map (lambda (dir)
                                              (string-append dir "/bin:"
                                                             dir "/sbin"))
                                            (list net-tools coreutils sed))))))
                  %standard-phases))))

    (native-inputs `(("perl" ,perl)))

    (inputs `(("net-tools" ,net-tools)
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
    (license isc)))

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
    (arguments '(#:tests? #f))                    ; no 'check' target
    (home-page "http://www.tcpdump.org")
    (synopsis "Network packet capture library")
    (description
     "libpcap is an interface for user-level packet capture.  It provides a
portable framework for low-level network monitoring.  Applications include
network statistics collection, security monitoring, network debugging, etc.")

    ;; fad-*.c and a couple other files are BSD-4, but the rest is BSD-3.
    (license bsd-3)))

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
              ("openssl" ,o:openssl)))
    (native-inputs `(("perl" ,perl)))        ; for tests
    (home-page "http://www.tcpdump.org/")
    (synopsis "Network packet analyzer")
    (description
     "Tcpdump is a command-line tool to analyze network traffic passing
through the network interface controller.")
    (license bsd-3)))

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
    (license gpl2+)))

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
    (license gpl2+)))

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
    (license gpl3+)))

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
     `(#:configure-flags '("--with-logpath=/var/log/sudo.log")
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
                      (string-append "install: " before after "\n"))))
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
    (license x11)))

(define-public wpa-supplicant
  (package
    (name "wpa-supplicant")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://hostap.epitest.fi/releases/wpa_supplicant-"
                    version
                    ".tar.gz"))
              (sha256
               (base32
                "1vf8jc4yyksbxf86narvsli3vxfbm8nbnim2mdp66nd6d3yvin70"))))
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
      CONFIG_CTRL_IFACE_DBUS=y
      CONFIG_CTRL_IFACE_DBUS_NEW=y
      CONFIG_CTRL_IFACE_DBUS_INTRO=y
      CONFIG_DRIVER_NL80211=y
      CFLAGS += $(shell pkg-config libnl-3.0 --cflags)
      CONFIG_LIBNL32=y
      CONFIG_READLINE=y\n" port)
                     (close-port port)))
                 %standard-phases)

      #:make-flags (list "CC=gcc"
                         (string-append "BINDIR=" (assoc-ref %outputs "out")
                                        "/sbin")
                         (string-append "LIBDIR=" (assoc-ref %outputs "out")
                                        "/lib"))
      #:tests? #f))
    (inputs
     `(("readline" ,readline)
       ("libnl" ,libnl)
       ("dbus" ,dbus)
       ("openssl" ,o:openssl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://hostap.epitest.fi/wpa_supplicant/")
    (synopsis "Connecting to WPA and WPA2-protected wireless networks")
    (description
     "wpa_supplicant is a WPA Supplicant with support for WPA and WPA2 (IEEE
802.11i / RSN).  Supplicant is the IEEE 802.1X/WPA component that is used in
the client stations.  It implements key negotiation with a WPA Authenticator
and it controls the roaming and IEEE 802.11 authentication/association of the
WLAN driver.

This package provides the 'wpa_supplicant' daemon and the 'wpa_cli' command.")

    ;; In practice, this is linked against Readline, which makes it GPLv3+.
    (license bsd-3)))

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
    (license gpl2+)))

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
    (license gpl2+)))

(define-public flashrom
  (package
    (name "flashrom")
    (version "0.9.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://download.flashrom.org/releases/flashrom-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1s9pc4yls2s1gcg2ar4q75nym2z5v6lxq36bl6lq26br00nj2mas"))
              (patches (list (search-patch "flashrom-use-libftdi1.patch")))))
    (build-system gnu-build-system)
    (inputs `(("dmidecode" ,dmidecode)
              ("pciutils" ,pciutils)
              ("libusb" ,libusb)
              ("libftdi" ,libftdi)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     '(#:make-flags (list "CC=gcc" (string-append "PREFIX=" %output))
       #:tests? #f   ; no 'check' target
       #:phases
       (alist-delete
        'configure
        (alist-cons-before
         'build 'patch-exec-paths
         (lambda* (#:key inputs #:allow-other-keys)
           (substitute* "dmi.c"
             (("\"dmidecode\"")
              (format #f "~S"
                      (string-append (assoc-ref inputs "dmidecode")
                                     "/sbin/dmidecode")))))
         %standard-phases))))
    (home-page "http://flashrom.org/")
    (synopsis "Identify, read, write, erase, and verify ROM/flash chips")
    (description
     "flashrom is a utility for identifying, reading, writing,
verifying and erasing flash chips.  It is designed to flash
BIOS/EFI/coreboot/firmware/optionROM images on mainboards,
network/graphics/storage controller cards, and various other
programmer devices.")
    (license gpl2)))

(define-public acpica
  (package
    (name "acpica")
    (version "20140724")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://acpica.org/sites/acpica/files/acpica-unix2-"
                    version ".tar.gz"))
              (sha256
               (base32
                "01vdgrh7dsxrrvg5yd8sxm63cw8210pnsi5qg9g15ac53gn243ac"))))
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
    (synopsis "ACPICA tools for the development and debug of ACPI tables")
    (description
     "The ACPI Component Architecture (ACPICA) project provides an
OS-independent reference implementation of the Advanced Configuration and
Power Interface Specification (ACPI).  ACPICA code contains those portions of
ACPI meant to be directly integrated into the host OS as a kernel-resident
subsystem, and a small set of tools to assist in developing and debugging ACPI
tables.  This package contains only the user-space tools needed for ACPI table
development, not the kernel implementation of ACPI.")
    (license gpl2)))  ; Dual GPLv2/ACPICA Licence

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
    (synopsis "A tool to impose load on and stress test a computer system")
    (description
     "'stress' is a tool that imposes a configurable amount of CPU, memory, I/O,
or disk stress on a POSIX-compliant operating system and reports any errors it
detects.

'stress' is not a benchmark.  It is a tool used by system administrators to
evaluate how well their systems will scale, by kernel programmers to evaluate
perceived performance characteristics, and by systems programmers to expose
the classes of bugs which only or more frequently manifest themselves when the
system is under heavy load.")
    (license gpl2+)))

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
    (synopsis "Clean up filenames")
    (description
     "Detox is a program that renames files to make them easier to work with
under Unix and related operating systems.  Spaces and various other unsafe
characters (such as \"$\") get replaced with \"_\".  ISO 8859-1 (Latin-1)
characters can be replaced as well, as can UTF-8 characters.")
    (license bsd-3)))

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
       ("openssl" ,o:openssl)
       ("zlib" ,c:zlib)
       ("e2fsprogs" ,e2fsprogs)
       ("libjpeg" ,libjpeg)
       ("ncurses" ,ncurses)))
    (home-page "http://www.cgsecurity.org/wiki/TestDisk")
    (synopsis "Data recovery tool")
    (description
     "TestDisk is a program for data recovery, primarily designed to help
recover lost partitions and/or make non-booting disks bootable again.")
    (license gpl2+)))

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
within the file system where it occurred.  Thus, \"direvent\" provides an easy
way to react immediately if given files undergo changes, for example, to
track changes in important system configuration files.")
    (license gpl3+)))
