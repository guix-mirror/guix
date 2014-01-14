;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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
  #:use-module ((gnu packages base)
                #:select (tar))
  #:use-module ((gnu packages compression)
                #:select (gzip))
  #:use-module (gnu packages pkg-config))

(define-public dmd
  (package
    (name "dmd")
    (version "0.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://alpha.gnu.org/gnu/dmd/dmd-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "07mddw0p62fcphwjzgb6rfa0pjz5sy6jzbha0sm2vc3rqf459jxg"))
             (patches (list (search-patch "dmd-getpw.patch")))))
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
   (version "3.0.3")
   (source
    (origin
     (method url-fetch)
      (uri (string-append
            "http://projects.gw-computing.net/attachments/download/78/dfc-"
            version ".tar.gz"))
      (sha256
       (base32
        "1b4hfqv23l87cb37fxwzfk2sgspkyxpr3ig2hsd23hr6mm982j7z"))))
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
   (version "1.0.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/htop/"
                  version "/htop-" version ".tar.gz"))
            (sha256
             (base32
              "18fqrhvnm7h4c3939av8lpiwrwxbyw6hcly0jvq0vkjf0ixnaq7f"))))
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
             (method url-fetch)
             (uri (string-append
                   "http://pkg-shadow.alioth.debian.org/releases/shadow-"
                   version ".tar.bz2"))
             (sha256
              (base32
               "1yvqx57vzih0jdy3grir8vfbkxp0cl0myql37bnmi2yn90vk6cma"))))
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
    (version "5.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://ftp.de.debian.org/debian/pool/main/n/netbase/netbase_"
                   version ".tar.gz"))
             (sha256
              (base32
               "17l8xk2x632id5f9x9v5fs9wqc650hldd2lf3dh90r1zisj1ya8d"))))
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
                     ("gzip" ,gzip)))
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
    (version "4.3.0a1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.isc.org/isc/dhcp/"
                                  version "/dhcp-" version ".tar.gz"))
              (sha256
               (base32
                "0001n26m4488nl95h53wg60sywbli4d246vz2h8lpv70jlrq9q1p"))))
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
                               (find-files "bind-9.9.5b1" ".*"))
                     (zero? (system* "tar" "cf" "bind.tar.gz"
                                     "bind-9.9.5b1"))))
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

    ;; Even Coreutils and sed are needed here in case we're cross-compiling.
    (inputs `(("coreutils" ,coreutils)
              ("sed" ,sed)
              ("net-tools" ,net-tools)
              ("iproute" ,iproute)))

    (home-page "http://www.isc.org/products/DHCP/")
    (synopsis "Dynamic Host Configuration Protocol (DHCP) tools")
    (description
     "ISC's Dynamic Host Configuration Protocol (DHCP) distribution provides a
reference implementation of all aspects of DHCP, through a suite of DHCP
tools: server, client, and relay agent.")
    (license (bsd-style "http://www.isc.org/sw/dhcp/dhcp-copyright.php"))))
