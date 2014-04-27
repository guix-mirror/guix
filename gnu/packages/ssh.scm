;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ncurses)
  #:autoload   (gnu packages protobuf) (protobuf)
  #:autoload   (gnu packages boost) (boost)
  #:use-module (gnu packages which)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake))

(define-public libssh
  (package
    (name "libssh")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://red.libssh.org/attachments/download/87/libssh-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1jyaj9h1iglvn02hrvcchbx8ycjpj8b91h8mi459k7q5jp2xgd9b"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DWITH_GCRYPT=ON")

       ;; TODO: Add 'CMockery' and '-DWITH_TESTING=ON' for the test suite.
       #:tests? #f))
    (inputs `(("zlib" ,zlib)
               ;; Link against an older gcrypt, because libssh tries to access
               ;; fields of 'gcry_thread_cbs' that are now private:
               ;; src/threads.c:72:26: error: 'struct gcry_thread_cbs' has no member named 'mutex_init'
              ("libgcrypt", libgcrypt-1.5)))
    (native-inputs `(("patchelf" ,patchelf)))
    (synopsis "SSH client library")
    (description
     "libssh is a C library implementing the SSHv2 and SSHv1 protocol for
client and server implementations.  With libssh, you can remotely execute
programs, transfer files, and use a secure and transparent tunnel for your
remote applications.")
    (home-page "http://www.libssh.org")
    (license license:lgpl2.1+)))

(define libssh-0.5                                ; kept private
  (package (inherit libssh)
    (version "0.5.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://red.libssh.org/attachments/download/51/libssh-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "17cfdff4hc0ijzrr15biq29fiabafz0bw621zlkbwbc1zh2hzpy0"))
              (patches (list (search-patch "libssh-CVE-2014-0017.patch")))))))

(define-public libssh2
  (package
   (name "libssh2")
   (version "1.4.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "http://www.libssh2.org/download/libssh2-"
                   version ".tar.gz"))
            (sha256 (base32
                     "0vdr478dbhbdgnniqmirawjb7mrcxckn4slhhrijxnzrkmgziipa"))))
   (build-system gnu-build-system)
   ;; The installed libssh2.pc file does not include paths to libgcrypt and
   ;; zlib libraries, so we need to propagate the inputs.
   (propagated-inputs `(("libgcrypt" ,libgcrypt)
                        ("zlib" ,zlib)))
   (arguments '(#:configure-flags `("--with-libgcrypt")))
   (synopsis "libssh2, a client-side C library implementing the SSH2 protocol")
   (description
    "libssh2 is a library intended to allow software developers access to
the SSH-2 protocol in an easy-to-use self-contained package. It can be built
into an application to perform many different tasks when communicating with
a server that supports the SSH-2 protocol.")
   (license license:bsd-3)
   (home-page "http://www.libssh2.org/")))

(define-public openssh
  (package
   (name "openssh")
   (version "6.6p1")
   (source (origin
            (method url-fetch)
            (uri (let ((tail (string-append name "-" version ".tar.gz")))
                   (list (string-append "ftp://ftp.fr.openbsd.org/pub/OpenBSD/OpenSSH/portable/"
                                        tail)
                         (string-append "ftp://ftp2.fr.openbsd.org/pub/OpenBSD/OpenSSH/portable/"
                                        tail))))
            (sha256 (base32
                     "1fq3w86q05y5nn6z878wm312k0svaprw8k007188fd259dkg1ha8"))))
   (build-system gnu-build-system)
   (inputs `(("groff" ,groff)
             ("openssl" ,openssl)
             ("zlib" ,zlib)))
   (arguments
    `(#:test-target "tests"
      #:phases
       (alist-cons-after
        'configure 'reset-/var/empty
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
            (substitute* "Makefile"
              (("PRIVSEP_PATH=/var/empty")
               (string-append "PRIVSEP_PATH=" out "/var/empty")))))
       (alist-cons-before
        'check 'patch-tests
        (lambda _
          ;; remove tests that require the user sshd
          (substitute* "regress/Makefile"
            (("t10 t-exec") "t10")))
       (alist-replace
        'install
        (lambda* (#:key (make-flags '()) #:allow-other-keys)
          ;; install without host keys and system configuration files
          (zero? (apply system* "make" "install-nosysconf" make-flags)))
       %standard-phases)))))
   (synopsis "OpenSSH, a client and server for the secure shell (ssh) protocol")
   (description
    "The SSH2 protocol implemented in OpenSSH is standardised by the
IETF secsh working group and is specified in several RFCs and drafts.
It is composed of three layered components:

The transport layer provides algorithm negotiation and a key exchange.
The key exchange includes server authentication and results in a
cryptographically secured connection: it provides integrity, confidentiality
and optional compression.

The user authentication layer uses the established connection and relies on
the services provided by the transport layer. It provides several mechanisms
for user authentication. These include traditional password authentication
as well as public-key or host-based authentication mechanisms.

The connection layer multiplexes many different concurrent channels over the
authenticated connection and allows tunneling of login sessions and
TCP-forwarding. It provides a flow control service for these channels.
Additionally, various channel-specific options can be negotiated.")
   (license (license:bsd-style "file://LICENSE"
                               "See LICENSE in the distribution."))
   (home-page "http://www.openssh.org/")))

(define-public guile-ssh
  (package
    (name "guile-ssh")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/artyom-poptsov/libguile-ssh/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "1v4y5vrwg0g6804pzbr160zahlqvj7k7iwys2bdpfzp7m2i47siq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'autoreconf
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "ssh/Makefile.am"
                     (("-lssh_threads" match)
                      (string-append "-L" (assoc-ref inputs "libssh")
                                     "/lib " match)))

                   (zero? (system* "autoreconf" "-vfi")))
                 (alist-cons-after
                  'install 'fix-libguile-ssh-file-name
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out      (assoc-ref outputs "out"))
                           (libdir   (string-append out "/lib"))
                           (guiledir (string-append out
                                                    "/share/guile/site/2.0")))
                      (substitute* (find-files guiledir ".scm")
                        (("\"libguile-ssh\"")
                         (string-append "\"" libdir "/libguile-ssh\"")))

                      ;; Make sure it works.
                      (setenv "GUILE_LOAD_PATH" guiledir)
                      (setenv "GUILE_LOAD_COMPILED_PATH" guiledir)
                      (system* "guile" "-c" "(use-modules (ssh session))")))
                  %standard-phases))
       #:configure-flags (list (string-append "--with-guilesitedir="
                                              (assoc-ref %outputs "out")
                                              "/share/guile/site/2.0"))

       ;; Building the .go requires building libguile-ssh.so first.
       #:parallel-build? #f

       ;; Tests are not parallel-safe.
       #:parallel-tests? #f))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool" ,libtool "bin")
                     ("texinfo" ,texinfo)
                     ("pkg-config" ,pkg-config)
                     ("which" ,which)))
    (inputs `(("guile" ,guile-2.0)
              ("libssh" ,libssh-0.5)))
    (synopsis "Guile bindings to libssh")
    (description
     "Guile-SSH is a library that provides access to the SSH protocol for
programs written in GNU Guile interpreter.  It is a wrapper to the underlying
libssh library.")
    (home-page "https://github.com/artyom-poptsov/libguile-ssh")
    (license license:gpl3+)))

(define-public corkscrew
  (package
    (name "corkscrew")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.agroman.net/corkscrew/corkscrew-"
                           version ".tar.gz"))
       (sha256 (base32
                "1gmhas4va6gd70i2x2mpxpwpgww6413mji29mg282jms3jscn3qd"))))
    (build-system gnu-build-system)
    (arguments
     ;; Replace configure phase as the ./configure script does not link
     ;; CONFIG_SHELL and SHELL passed as parameters
     '(#:phases
       (alist-replace
        'configure
        (lambda* (#:key outputs inputs system target
                        #:allow-other-keys #:rest args)
          (let* ((configure (assoc-ref %standard-phases 'configure))
                 (prefix (assoc-ref outputs "out"))
                 (bash   (which "bash"))
                 ;; Set --build and --host flags as the provided config.guess
                 ;; is not able to detect them
                 (flags `(,(string-append "--prefix=" prefix)
                          ,(string-append "--build=" system)
                          ,(string-append "--host="
                                          (or target system)))))
            (setenv "CONFIG_SHELL" bash)
            (zero? (apply system* bash
                          (string-append "." "/configure")
                          flags))))
        %standard-phases)))
    (home-page "http://www.agroman.net/corkscrew")
    (synopsis "A tool for tunneling SSH through HTTP proxies")
    (description
     "Corkscrew allows creating TCP tunnels through HTTP proxies.  WARNING:
At the moment only plain text authentication is supported, should you require
to use it with your HTTP proxy.  Digest based authentication may be supported
in future and NTLM based authentication is most likey never be supported.")
    (license license:gpl2+)))

(define-public mosh
  (package
    (name "mosh")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://mosh.mit.edu/mosh-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0inzfmqrab3n97m7rrmhd4xh3hjz0xva2sfl5m06w11668r0skg7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-after
                 'install 'wrap
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Make sure 'mosh' can find 'mosh-client' and
                   ;; 'mosh-server'.
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin")))
                     (wrap-program (string-append bin "/mosh")
                                   `("PATH" ":" prefix (,bin)))))
                 %standard-phases)))
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
    (home-page "http://mosh.mit.edu/")
    (synopsis "Remote shell tolerant to intermittent connectivity")
    (description
     "Remote terminal application that allows roaming, supports intermittent
connectivity, and provides intelligent local echo and line editing of user
keystrokes.  Mosh is a replacement for SSH.  It's more robust and responsive,
especially over Wi-Fi, cellular, and long-distance links.")
    (license license:gpl3+)))
