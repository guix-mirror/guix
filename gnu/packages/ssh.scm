;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2018 Manuel Graf <graf@init.at>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
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

(define-module (gnu packages ssh)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:autoload   (gnu packages boost) (boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:autoload   (gnu packages protobuf) (protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public libssh
  (package
    (name "libssh")
    (version "0.8.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://git.libssh.org/projects/libssh.git")
                     (commit (string-append "libssh-" version))))
              (sha256
               (base32
                "1iqik1ba0g008k1mb1n85iih1azi7giy0c485jnlmsrjxik4q3j2"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     '(#:configure-flags '("-DWITH_GCRYPT=ON")

       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'avoid-werror
                    (lambda _
                      ;; Avoid '-Werror'.  Presumably this works fine with
                      ;; gcc@8 on x86_64 but leads to errors with our older
                      ;; compiler.
                      (substitute* "CompilerChecks.cmake"
                        (("-Werror=") "-W"))
                      #t)))

       ;; TODO: Add 'CMockery' and '-DWITH_TESTING=ON' for the test suite.
       #:tests? #f))
    (inputs `(("zlib" ,zlib)
              ("libgcrypt" ,libgcrypt)))
    (synopsis "SSH client library")
    (description
     "libssh is a C library implementing the SSHv2 and SSHv1 protocol for client
and server implementations.  With libssh, you can remotely execute programs,
transfer files, and use a secure and transparent tunnel for your remote
applications.")
    (home-page "https://www.libssh.org")
    (license license:lgpl2.1+)))

(define-public libssh2
  (package
   (name "libssh2")
   (version "1.8.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "https://www.libssh2.org/download/libssh2-"
                   version ".tar.gz"))
            (sha256
             (base32
              "1m3n8spv79qhjq4yi0wgly5s5rc8783jb1pyra9bkx1md0plxwrr"))
            (patches
             (search-patches "libssh2-fix-build-failure-with-gcrypt.patch"))))
   (build-system gnu-build-system)
   ;; The installed libssh2.pc file does not include paths to libgcrypt and
   ;; zlib libraries, so we need to propagate the inputs.
   (propagated-inputs `(("libgcrypt" ,libgcrypt)
                        ("zlib" ,zlib)))
   (arguments `(#:configure-flags `("--with-libgcrypt")
                #:phases (modify-phases %standard-phases
                           (replace 'bootstrap
                             (lambda _
                               (invoke "autoreconf" "-v"))))))
   (native-inputs `(("autoconf" ,autoconf)
                    ("automake" ,automake)))
   (synopsis "Client-side C library implementing the SSH2 protocol")
   (description
    "libssh2 is a library intended to allow software developers access to
the SSH-2 protocol in an easy-to-use self-contained package.  It can be built
into an application to perform many different tasks when communicating with
a server that supports the SSH-2 protocol.")
   (license license:bsd-3)
   (home-page "https://www.libssh2.org/")))

(define-public openssh
  (package
   (name "openssh")
   (version "7.9p1")
   (source (origin
             (method url-fetch)
             (uri (string-append "mirror://openbsd/OpenSSH/portable/"
                                 name "-" version ".tar.gz"))
             (patches (search-patches "openssh-CVE-2018-20685.patch"))
             (sha256
              (base32
               "1b8sy6v0b8v4ggmknwcqx3y1rjcpsll0f1f8f4vyv11x4ni3njvb"))))
   (build-system gnu-build-system)
   (native-inputs `(("groff" ,groff)
                    ("pkg-config" ,pkg-config)))
   (inputs `(("libedit" ,libedit)
             ("openssl" ,openssl)
             ("pam" ,linux-pam)
             ("mit-krb5" ,mit-krb5)
             ("zlib" ,zlib)
             ("xauth" ,xauth)))                   ;for 'ssh -X' and 'ssh -Y'
   (arguments
    `(#:test-target "tests"
      ;; Otherwise, the test scripts try to use a nonexistent directory and
      ;; fail.
      #:make-flags '("REGRESSTMP=\"$${BUILDDIR}/regress\"")
      #:configure-flags  `("--sysconfdir=/etc/ssh"

                           ;; Default value of 'PATH' used by sshd.
                          "--with-default-path=/run/current-system/profile/bin"

                          ;; configure needs to find krb5-config
                          ,(string-append "--with-kerberos5="
                                          (assoc-ref %build-inputs "mit-krb5")
                                          "/bin")

                          ;; libedit needed for sftp completion
                          "--with-libedit"

                          ;; Enable PAM support in sshd.
                          "--with-pam")

      #:phases
      (modify-phases %standard-phases
        (add-after 'configure 'reset-/var/empty
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (substitute* "Makefile"
               (("PRIVSEP_PATH=/var/empty")
                (string-append "PRIVSEP_PATH=" out "/var/empty")))
             #t)))
        (add-before 'check 'patch-tests
         (lambda _
           ;; remove 't-exec' regress target which requires user 'sshd'
           (substitute* "regress/Makefile"
             (("^(REGRESS_TARGETS=.*) t-exec(.*)" all pre post)
              (string-append pre post)))
           #t))
        (replace 'install
         (lambda* (#:key outputs (make-flags '()) #:allow-other-keys)
           ;; install without host keys and system configuration files
           (apply invoke "make" "install-nosysconf" make-flags)
           (install-file "contrib/ssh-copy-id"
                         (string-append (assoc-ref outputs "out")
                                        "/bin/"))
           (chmod (string-append (assoc-ref outputs "out")
                                 "/bin/ssh-copy-id") #o555)
           (install-file "contrib/ssh-copy-id.1"
                         (string-append (assoc-ref outputs "out")
                                        "/share/man/man1/"))
           #t)))))
   (synopsis "Client and server for the secure shell (ssh) protocol")
   (description
    "The SSH2 protocol implemented in OpenSSH is standardised by the
IETF secsh working group and is specified in several RFCs and drafts.
It is composed of three layered components:

The transport layer provides algorithm negotiation and a key exchange.
The key exchange includes server authentication and results in a
cryptographically secured connection: it provides integrity, confidentiality
and optional compression.

The user authentication layer uses the established connection and relies on
the services provided by the transport layer.  It provides several mechanisms
for user authentication.  These include traditional password authentication
as well as public-key or host-based authentication mechanisms.

The connection layer multiplexes many different concurrent channels over the
authenticated connection and allows tunneling of login sessions and
TCP-forwarding.  It provides a flow control service for these channels.
Additionally, various channel-specific options can be negotiated.")
   (license (license:non-copyleft "file://LICENSE"
                               "See LICENSE in the distribution."))
   (home-page "https://www.openssh.com/")))

(define-public guile-ssh
  (package
    (name "guile-ssh")
    (version "0.11.3")
    (home-page "https://github.com/artyom-poptsov/guile-ssh")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03bv3hwp2s8f0bqgfjaan9jx4dyab0abv27n2zn2g0izlidv0vl6"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; libssh >= 0.8.0 no longer provides libssh_threads: see
                  ;; <https://github.com/artyom-poptsov/guile-ssh/issues/9>.
                  (substitute* "libguile-ssh/Makefile.am"
                    (("-lssh_threads") ""))

                  ;; This test would wrongfully pick DSS keys when running on
                  ;; libssh >= 0.8.0, which fails:
                  ;; <https://github.com/artyom-poptsov/guile-ssh/issues/10>.
                  (substitute* "tests/server.scm"
                    (("= %libssh-minor-version 7")
                     ">= %libssh-minor-version 7"))
                  #t))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (arguments
     '(;; It makes no sense to build libguile-ssh.a.
       #:configure-flags '("--disable-static")

       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'autoreconf
                    (lambda* (#:key inputs #:allow-other-keys)
                      (invoke "autoreconf" "-vfi")))
                  (add-before 'build 'fix-libguile-ssh-file-name
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Build and install libguile-ssh.so so that we can use
                      ;; its absolute file name in .scm files, before we build
                      ;; the .go files.
                      (let* ((out (assoc-ref outputs "out"))
                             (lib (string-append out "/lib")))
                        (invoke "make" "install"
                                "-C" "libguile-ssh"
                                "-j" (number->string
                                      (parallel-job-count)))
                        (substitute* (find-files "." "\\.scm$")
                          (("\"libguile-ssh\"")
                           (string-append "\"" lib "/libguile-ssh\"")))
                        #t)))
                  (add-after 'install 'remove-bin-directory
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (examples (string-append
                                        out "/share/guile-ssh/examples")))
                        (mkdir-p examples)
                        (rename-file (string-append bin "/ssshd.scm")
                                     (string-append examples "/ssshd.scm"))
                        (rename-file (string-append bin "/sssh.scm")
                                     (string-append examples "/sssh.scm"))
                        (delete-file-recursively bin)
                        #t))))
       ;; Tests are not parallel-safe.
       #:parallel-tests? #f))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("texinfo" ,texinfo)
                     ("pkg-config" ,pkg-config)
                     ("which" ,which)))
    (inputs `(("guile" ,guile-2.2)
              ("libssh" ,libssh)
              ("libgcrypt" ,libgcrypt)))
    (synopsis "Guile bindings to libssh")
    (description
     "Guile-SSH is a library that provides access to the SSH protocol for
programs written in GNU Guile interpreter.  It is a wrapper to the underlying
libssh library.")
    (license license:gpl3+)))

(define-public guile2.2-ssh
  (deprecated-package "guile2.2-ssh" guile-ssh))

(define-public guile2.0-ssh
  (package
    (inherit guile-ssh)
    (name "guile2.0-ssh")
    (inputs `(("guile" ,guile-2.0)
              ,@(alist-delete "guile" (package-inputs guile-ssh))))))

(define-public corkscrew
  (package
    (name "corkscrew")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       ;; The agroman.net domain name expired on 2017-03-23, and the original
       ;; "http://www.agroman.net/corkscrew/corkscrew-2.0.tar.gz" now returns
       ;; bogus HTML.  Perhaps it will yet return.  Until then, use a mirror.
       (uri (string-append "https://downloads.openwrt.org/sources/"
                           "corkscrew-" version ".tar.gz"))
       (sha256 (base32
                "1gmhas4va6gd70i2x2mpxpwpgww6413mji29mg282jms3jscn3qd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; Replace configure phase as the ./configure script does not like
           ;; CONFIG_SHELL and SHELL passed as parameters
           (lambda* (#:key outputs build target #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bash  (which "bash"))
                    ;; Set --build and --host flags as the provided config.guess
                    ;; is not able to detect them
                    (flags `(,(string-append "--prefix=" out)
                             ,(string-append "--build=" build)
                             ,(string-append "--host=" (or target build)))))
               (setenv "CONFIG_SHELL" bash)
               (apply invoke bash "./configure" flags))))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "README" doc)
               #t))))))
    (home-page "http://www.agroman.net/corkscrew")
    (synopsis "SSH tunneling through HTTP(S) proxies")
    (description
     "Corkscrew tunnels SSH connections through most HTTP and HTTPS proxies.
Proxy authentication is only supported through the plain-text HTTP basic
authentication scheme.")
    (license license:gpl2+)))

(define-public mosh
  (package
    (name "mosh")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://mosh.org/mosh-" version ".tar.gz"))
              (sha256
               (base32
                "05hjhlp6lk8yjcy59zywpf0r6s0h0b9zxq0lw66dh9x8vxrhaq6s"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure 'mosh' can find 'mosh-client' and
             ;; 'mosh-server'.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (wrap-program (string-append bin "/mosh")
                             `("PATH" ":" prefix (,bin)))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("openssl" ,openssl)
       ("perl" ,perl)
       ("perl-io-tty" ,perl-io-tty)
       ("zlib" ,zlib)
       ("ncurses" ,ncurses)
       ("protobuf" ,protobuf)
       ("boost-headers" ,boost)))
    (home-page "https://mosh.org/")
    (synopsis "Remote shell tolerant to intermittent connectivity")
    (description
     "Remote terminal application that allows roaming, supports intermittent
connectivity, and provides intelligent local echo and line editing of user
keystrokes.  Mosh is a replacement for SSH.  It's more robust and responsive,
especially over Wi-Fi, cellular, and long-distance links.")
    (license license:gpl3+)))

(define-public et
  (package
    (name "et")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/MisterTea/EternalTCP/archive/et-v"
             version ".tar.gz"))
       (sha256
        (base32 "1n2w2kqbshdmbb0gz4yizyw9gqfls6qm2dnwx1d9c2hz7hmi7521"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs `(("glog" ,glog)
              ("gflags" ,gflags)
              ("libsodium" ,libsodium)
              ("protobuf" ,protobuf)))
    (synopsis "Remote shell that automatically reconnects")
    (description
     "Eternal Terminal (ET) is a remote shell that automatically reconnects
without interrupting the session.  Unlike SSH sessions, ET sessions will
survive even network outages and IP changes.  ET uses a custom protocol over
TCP, not the SSH protocol.")
    (home-page "https://mistertea.github.io/EternalTCP/")
    (license license:asl2.0)))

(define-public dropbear
  (package
    (name "dropbear")
    (version "2018.76")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://matt.ucc.asn.au/" name "/releases/"
                    name "-" version ".tar.bz2"))
              (patches (search-patches "dropbear-CVE-2018-15599.patch"))
              (sha256
               (base32
                "0rgavbzw7jrs5wslxm0dnwx2m409yzxd9hazd92r7kx8xikr3yzj"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ; there is no "make check" or anything similar
    ;; TODO: Investigate unbundling libtommath and libtomcrypt or at least
    ;; cherry-picking important bug fixes from them. See <bugs.gnu.org/24674>
    ;; for more information.
    (inputs `(("zlib" ,zlib)))
    (synopsis "Small SSH server and client")
    (description "Dropbear is a relatively small SSH server and
client.  It runs on a variety of POSIX-based platforms.  Dropbear is
particularly useful for embedded systems, such as wireless routers.")
    (home-page "https://matt.ucc.asn.au/dropbear/dropbear.html")
    (license (license:x11-style "" "See file LICENSE."))))

(define-public liboop
  (package
    (name "liboop")
    (version "1.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://ftp.lysator.liu.se/pub/liboop/"
                          name "-" version ".tar.gz"))
      (sha256
       (base32
        "1q0p1l72pq9k3bi7a366j2rishv7dzzkg3i6r2npsfg7cnnidbsn"))))
    (build-system gnu-build-system)
    (home-page "http://www.lysator.liu.se/liboop/")
    (synopsis "Event loop library")
    (description "Liboop is a low-level event loop management library for
POSIX-based operating systems.  It supports the development of modular,
multiplexed applications which may respond to events from several sources.  It
replaces the \"select() loop\" and allows the registration of event handlers
for file and network I/O, timers and signals.  Since processes use these
mechanisms for almost all external communication, liboop can be used as the
basis for almost any application.")
    (license license:lgpl2.1+)))

(define-public lsh
  (package
    (name "lsh")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/lsh/lsh-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1qqjy9zfzgny0rkb27c8c7dfsylvb6n0ld8h3an2r83pmaqr9gwb"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "src/testsuite/functions.sh"
                    (("localhost")
                     ;; Avoid host name lookups since they don't work in
                     ;; chroot builds.
                     "127.0.0.1")
                    (("set -e")
                     ;; Make tests more verbose.
                     "set -e\nset -x"))

                  (substitute* (find-files "src/testsuite" "-test$")
                    (("localhost") "127.0.0.1"))

                  (substitute* "src/testsuite/login-auth-test"
                    (("/bin/cat") "cat"))
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("m4" ,m4)
       ("guile" ,guile-2.0)
       ("gperf" ,gperf)
       ("psmisc" ,psmisc)))                       ; for `killall'
    (inputs
     `(("nettle" ,nettle-2)
       ("linux-pam" ,linux-pam)

       ;; 'rl.c' uses the 'CPPFunction' type, which is no longer in
       ;; Readline 6.3.
       ("readline" ,readline-6.2)

       ("liboop" ,liboop)
       ("zlib" ,zlib)
       ("gmp" ,gmp)

       ;; The server (lshd) invokes xauth when X11 forwarding is requested.
       ;; This adds 24 MiB (or 27%) to the closure of lsh.
       ("xauth" ,xauth)))
    (arguments
     '(;; Skip the `configure' test that checks whether /dev/ptmx &
       ;; co. work as expected, because it relies on impurities (for
       ;; instance, /dev/pts may be unavailable in chroots.)
       #:configure-flags '("lsh_cv_sys_unix98_ptys=yes"

                           ;; Use glibc's argp rather than the bundled one.
                           "--with-system-argp"

                           ;; 'lsh_argp.h' checks HAVE_ARGP_PARSE but nothing
                           ;; defines it.
                           "CPPFLAGS=-DHAVE_ARGP_PARSE")

       ;; FIXME: Tests won't run in a chroot, presumably because
       ;; /etc/profile is missing, and thus clients get an empty $PATH
       ;; and nothing works.
       #:tests? #f

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((nettle    (assoc-ref inputs "nettle"))
                    (sexp-conv (string-append nettle "/bin/sexp-conv")))
               ;; Remove argp from the list of sub-directories; we don't want
               ;; to build it, really.
               (substitute* "src/Makefile.in"
                 (("^SUBDIRS = argp")
                  "SUBDIRS ="))

               ;; Make sure 'lsh' and 'lshd' pick 'sexp-conv' in the right place
               ;; by default.
               (substitute* "src/environ.h.in"
                 (("^#define PATH_SEXP_CONV.*")
                  (string-append "#define PATH_SEXP_CONV \""
                                 sexp-conv "\"\n")))

               ;; Same for the 'lsh-authorize' script.
               (substitute* "src/lsh-authorize"
                 (("=sexp-conv")
                  (string-append "=" sexp-conv)))

               ;; Tell lshd where 'xauth' lives.  Another option would be to
               ;; hardcode "/run/current-system/profile/bin/xauth", thereby
               ;; reducing the closure size, but that wouldn't work on foreign
               ;; distros.
               (with-fluids ((%default-port-encoding "ISO-8859-1"))
                 (substitute* "src/server_x11.c"
                   (("define XAUTH_PROGRAM.*")
                    (string-append "define XAUTH_PROGRAM \""
                                   (assoc-ref inputs "xauth")
                                   "/bin/xauth\"\n")))))

             ;; Tests rely on $USER being set.
             (setenv "USER" "guix"))))))
    (home-page "http://www.lysator.liu.se/~nisse/lsh/")
    (synopsis "GNU implementation of the Secure Shell (ssh) protocols")
    (description
     "GNU lsh is a free implementation of the SSH version 2 protocol.  It is
used to create a secure line of communication between two computers,
providing shell access to the server system from the client.  It provides
both the server daemon and the client application, as well as tools for
manipulating key files.")
    (license license:gpl2+)))

(define-public sshpass
  (package
    (name "sshpass")
    (version "1.06")
    (synopsis "Non-interactive password authentication with SSH")
    (home-page "https://sourceforge.net/projects/sshpass/")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sshpass/sshpass/"
                           version "/sshpass-" version ".tar.gz"))
       (sha256
        (base32
         "0q7fblaczb7kwbsz0gdy9267z0sllzgmf0c7z5c9mf88wv74ycn6"))))
    (build-system gnu-build-system)
    (description "sshpass is a tool for non-interactivly performing password
authentication with SSH's so-called @dfn{interactive keyboard password
authentication}.")
    (license license:gpl2+)))

(define-public autossh
  (package
    (name "autossh")
    (version "1.4g")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.harding.motd.ca/autossh/autossh-"
             version ".tgz"))
       (sha256
        (base32 "0xqjw8df68f4kzkns5gcah61s5wk0m44qdk2z1d6388w6viwxhsz"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ; There is no "make check" or anything similar
    (inputs `(("openssh" ,openssh)))
    (synopsis "Automatically restart SSH sessions and tunnels")
    (description "autossh is a program to start a copy of @command{ssh} and
monitor it, restarting it as necessary should it die or stop passing traffic.")
    (home-page "https://www.harding.motd.ca/autossh/")
    (license
     ;; Why point to a source file?  Well, all the individual files have a
     ;; copy of this license in their headers, but there's no separate file
     ;; with that information.
     (license:non-copyleft "file://autossh.c"))))

(define-public pdsh
  (package
    (name "pdsh")
    (version "2.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/chaos/pdsh/"
                           "releases/download/pdsh-" version
                           "/pdsh-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bwlkl9inj66iwvafg00pi3sk9n673phdi0kcc59y9nn55s0hs3k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--with-ssh")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (substitute* '("tests/t0006-pdcp.sh"
                            "tests/t0004-module-loading.sh"
                            "tests/t2001-ssh.sh"
                            "tests/t1003-slurm.sh"
                            "tests/t6036-long-output-lines.sh"
                            "tests/aggregate-results.sh"
                            "tests/t2000-exec.sh"
                            "tests/t0002-internal.sh"
                            "tests/t1002-dshgroup.sh"
                            "tests/t5000-dshbak.sh"
                            "tests/t0001-basic.sh"
                            "tests/t0005-rcmd_type-and-user.sh"
                            "tests/test-lib.sh"
                            "tests/t2002-mrsh.sh"
                            "tests/t0003-wcoll.sh"
                            "tests/test-modules/pcptest.c")
               (("/bin/sh") (which "bash")))
             #t))
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "tests/t6036-long-output-lines.sh"
               (("which") (which "which")))
             #t)))))
    (inputs
     `(("openssh" ,openssh)
       ("mit-krb5" ,mit-krb5)
       ("perl" ,perl)))
    (native-inputs
     `(("which" ,which)))
    (home-page "https://github.com/chaos/pdsh")
    (synopsis "Parallel distributed shell")
    (description "Pdsh is a an efficient, multithreaded remote shell client
which executes commands on multiple remote hosts in parallel.  Pdsh implements
dynamically loadable modules for extended functionality such as new remote
shell services and remote host selection.")
    (license license:gpl2+)))

(define-public clustershell
  (package
    (name "clustershell")
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cea-hpc/clustershell/archive/v"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "1qyf6zp5ikk8rk7zvx5ssbgr9si2bqv3a3415590kd07s7i16nmd"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system python-build-system)
    (inputs `(("openssh" ,openssh)))
    (propagated-inputs `(("python-pyyaml" ,python-pyyaml)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'build 'record-openssh-file-name
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((ssh (assoc-ref inputs "openssh")))
                        (substitute* "lib/ClusterShell/Worker/Ssh.py"
                          (("info\\(\"ssh_path\"\\) or \"ssh\"")
                           (string-append "info(\"ssh_path\") or \""
                                          ssh "/bin/ssh\"")))
                        #t))))))
    (home-page "https://cea-hpc.github.io/clustershell/")
    (synopsis "Scalable event-driven Python framework for cluster administration")
    (description
     "ClusterShell is an event-driven Python framework, designed to run local
or distant commands in parallel on server farms or on large GNU/Linux
clusters.  It will take care of common issues encountered on HPC clusters,
such as operating on groups of nodes, running distributed commands using
optimized execution algorithms, as well as gathering results and merging
identical outputs, or retrieving return codes.  ClusterShell takes advantage
of existing remote shell facilities such as SSH.")
    (license license:lgpl2.1+)))
