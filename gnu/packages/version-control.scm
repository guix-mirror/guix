;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages version-control)
  #:use-module ((guix licenses)
                #:select (asl2.0 gpl1+ gpl2 gpl2+ gpl3+ x11-style))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build utils)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages gettext)
;;   #:use-module (gnu packages gnutls)
  #:use-module (gnu packages nano)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages))

(define-public bazaar
  (package
    (name "bazaar")
    (version "2.5.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://launchpad.net/bzr/2.5/" version
                          "/+download/bzr-" version ".tar.gz"))
      (sha256
       (base32
        "10krjbzia2avn09p0cdlbx2wya0r5v11w5ymvyl72af5dkx4cwwn"))))
    (build-system python-build-system)
    (inputs
     ;; Note: 'tools/packaging/lp-upload-release' and 'tools/weavemerge.sh'
     ;; require Zsh.
     `(("gettext" ,gnu-gettext)))
    (arguments
     `(#:tests? #f ; no test target
       #:python ,python-2   ; Python 3 apparently not yet supported, see
                            ; https://answers.launchpad.net/bzr/+question/229048
       #:phases (alist-cons-after
                 'unpack 'fix-mandir
                 (lambda _
                   (substitute* "setup.py"
                     (("man/man1") "share/man/man1")))
                 %standard-phases)))
    (home-page "https://gnu.org/software/bazaar")
    (synopsis "Version control system supporting both distributed and centralized workflows")
    (description
     "GNU Bazaar is a version control system that allows you to record
changes to project files over time. It supports both a distributed workflow
as well as the classic centralized workflow.")
    (license gpl2+)))

(define-public git
  (package
   (name "git")
   (version "1.8.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://git-core.googlecode.com/files/git-"
                                version ".tar.gz"))
            (sha256
             (base32
              "156bwqqgaw65rsvbb4wih5jfg94bxyf6p16mdwf0ky3f4ln55s2i"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("native-perl" ,perl)
      ("gettext" ,gnu-gettext)))
   (inputs
    `(("curl" ,curl)
      ("expat" ,expat)
      ("openssl" ,openssl)
      ("perl" ,perl)
      ("python" ,python-2) ; CAVEAT: incompatible with python-3 according to INSTALL
      ("zlib" ,zlib)

      ;; For 'git-svn'.
      ("subversion" ,subversion)

      ;; For 'git gui', 'gitk', and 'git citool'.
      ("tcl" ,tcl)
      ("tk" ,tk)))
   (outputs '("out"                               ; the core
              "svn"                               ; git-svn
              "gui"))                             ; gitk, git gui
   (arguments
    `(#:make-flags `("V=1") ; more verbose compilation
      #:test-target "test"
      #:tests? #f ; FIXME: Many tests are failing

      ;; The explicit --with-tcltk forces the build system to hardcode the
      ;; absolute file name to 'wish'.
      #:configure-flags (list (string-append "--with-tcltk="
                                             (assoc-ref %build-inputs "tk")
                                             "/bin/wish8.6")) ; XXX

      #:phases
       (alist-cons-after
        'configure 'patch-makefile-shebangs
        (lambda _
          (substitute* "Makefile"
            (("/bin/sh") (which "sh"))
            (("/usr/bin/perl") (which "perl"))
            (("/usr/bin/python") (which "python"))))
        (alist-cons-after
         'install 'split
         (lambda* (#:key inputs outputs #:allow-other-keys)
           ;; Split the binaries to the various outputs.
           (let* ((out      (assoc-ref outputs "out"))
                  (svn      (assoc-ref outputs "svn"))
                  (gui      (assoc-ref outputs "gui"))
                  (gitk     (string-append out "/bin/gitk"))
                  (gitk*    (string-append gui "/bin/gitk"))
                  (git-gui  (string-append out "/libexec/git-core/git-gui"))
                  (git-gui* (string-append gui "/libexec/git-core/git-gui"))
                  (git-cit  (string-append out "/libexec/git-core/git-citool"))
                  (git-cit* (string-append gui "/libexec/git-core/git-citool"))
                  (git-svn  (string-append out "/libexec/git-core/git-svn"))
                  (git-svn* (string-append svn "/libexec/git-core/git-svn"))
                  (git-sm   (string-append out
                                           "/libexec/git-core/git-submodule")))
             (mkdir-p (string-append gui "/bin"))
             (mkdir-p (string-append gui "/libexec/git-core"))
             (mkdir-p (string-append svn "/libexec/git-core"))

             (for-each (lambda (old new)
                         (copy-file old new)
                         (delete-file old)
                         (chmod new #o555))
                       (list gitk git-gui git-cit git-svn)
                       (list gitk* git-gui* git-cit* git-svn*))

             ;; Tell 'git-svn' where Subversion is.
             (wrap-program git-svn*
                           `("PATH" ":" prefix
                             (,(string-append (assoc-ref inputs "subversion")
                                              "/bin")))
                           `("PERL5LIB" ":" prefix
                             (,(string-append (assoc-ref inputs "subversion")
                                              "/lib/perl5/site_perl")))

                           ;; XXX: The .so for SVN/Core.pm lacks a RUNPATH, so
                           ;; help it find 'libsvn_client-1.so'.
                           `("LD_LIBRARY_PATH" ":" prefix
                             (,(string-append (assoc-ref inputs "subversion")
                                              "/lib"))))

             ;; Tell 'git-submodule' where Perl is.
             (wrap-program git-sm
                           `("PATH" ":" prefix
                             (,(string-append (assoc-ref inputs "perl")
                                              "/bin"))))

             ;; Tell 'git' to look for core programs in the user's profile.
             ;; This allows user to install other outputs of this package and
             ;; have them transparently taken into account.  There's a
             ;; 'GIT_EXEC_PATH' environment variable, but it's supposed to
             ;; specify a single directory, not a search path.
             (wrap-program (string-append out "/bin/git")
                           `("PATH" ":" prefix
                             ("$HOME/.guix-profile/libexec/git-core")))))
         %standard-phases))))
   (synopsis "Distributed version control system")
   (description
    "Git is a free distributed version control system designed to handle
everything from small to very large projects with speed and efficiency.")
   (license gpl2)
   (home-page "http://git-scm.com/")))

(define-public mercurial
  (package
    (name "mercurial")
    (version "2.7.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://mercurial.selenic.com/release/mercurial-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "121m8f7vmipmdg00cnzdz2rjkgydh28mwfirqkrbs5fv089vywl4"))))
    (build-system python-build-system)
    (arguments
     `(;; Restrict to Python 2, as Python 3 would require
       ;; the argument --c2to3.
       #:python ,python-2
       ;; FIXME: Disabled tests because they require the nose unit
       ;; testing framework: https://nose.readthedocs.org/en/latest/ .
       #:tests? #f))
    (home-page "http://mercurial.selenic.com")
    (synopsis "Decentralized version control system")
    (description
     "Mercurial is a free, distributed source control management tool.
It efficiently handles projects of any size
and offers an easy and intuitive interface.")
    (license gpl2+)))

(define-public neon
  (package
    (name "neon")
    (version "0.30.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.webdav.org/neon/neon-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1hlhg5w505jxdvaf7bq17057f6a48dry981g7lp2gwrhbp5wyqi9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxml2" ,libxml2)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (arguments
     `(;; FIXME: Add tests once reverse address lookup is fixed in glibc, see
       ;; https://sourceware.org/bugzilla/show_bug.cgi?id=16475
       #:tests? #f
       #:configure-flags '("--enable-shared"
                           ;; requires libgnutils-config, deprecated
                           ;; in gnutls 2.8.
                           ; "--with-ssl=gnutls")))
                           "--with-ssl=openssl")))
    (home-page "http://www.webdav.org/neon/")
    (synopsis "HTTP and WebDAV client library")
    (description "Neon is an HTTP and WebDAV client library, with a
C interface.  Features:
High-level wrappers for common HTTP and WebDAV operations (GET, MOVE,
DELETE, etc.);
low-level interface to the HTTP request/response engine, allowing the use
of arbitrary HTTP methods, headers, etc.;
authentication support including Basic and Digest support, along with
GSSAPI-based Negotiate on Unix, and SSPI-based Negotiate/NTLM on Win32;
SSL/TLS support using OpenSSL or GnuTLS, exposing an abstraction layer for
verifying server certificates, handling client certificates, and examining
certificate properties, smartcard-based client certificates are also
supported via a PKCS#11 wrapper interface;
abstract interface to parsing XML using libxml2 or expat, and wrappers for
simplifying handling XML HTTP response bodies;
WebDAV metadata support, wrappers for PROPFIND and PROPPATCH to simplify
property manipulation.")
    (license gpl2+))) ; for documentation and tests; source under lgpl2.0+

(define-public neon-0.29.6
  (package (inherit neon)
    (name "neon")
    (version "0.29.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.webdav.org/neon/neon-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0hzbjqdx1z8zw0vmbknf159wjsxbcq8ii0wgwkqhxj3dimr0nr4w"))))))

(define-public subversion
  (package
    (name "subversion")
    (version "1.7.14")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://archive.apache.org/dist/subversion/subversion-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "038jbcpwm083abp0rvk0fhnx65kp9mz1qvzs3f83ig8fxcvqzb64"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-after
                 'install 'instal-perl-bindings
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Follow the instructions from
                   ;; 'subversion/bindings/swig/INSTALL'.
                   (let ((out (assoc-ref outputs "out")))
                     (and (zero? (system* "make" "swig-pl-lib"))
                          ;; FIXME: Test failures.
                          ;; (zero? (system* "make" "check-swig-pl"))
                          (zero? (system* "make" "install-swig-pl-lib"))

                          ;; Set the right installation prefix.
                          (with-directory-excursion
                              "subversion/bindings/swig/perl/native"
                            (and (zero?
                                  (system* "perl" "Makefile.PL"
                                           (string-append "PREFIX=" out)))
                                 (zero?
                                  (system* "make" "install")))))))
                 %standard-phases)))
    (native-inputs
      `(("pkg-config" ,pkg-config)
        ;; For the Perl bindings.
        ("swig" ,swig)))
    (inputs
      `(("apr" ,apr)
        ("apr-util" ,apr-util)
        ("neon" ,neon-0.29.6)
        ("perl" ,perl)
        ("python" ,python-2) ; incompatible with Python 3 (print syntax)
        ("sqlite" ,sqlite)
        ("zlib" ,zlib)))
    (home-page "http://subversion.apache.org/")
    (synopsis "Subversion, a revision control system")
    (description
     "Subversion exists to be universally recognized and adopted as a
centralized version control system characterized by its
reliability as a safe haven for valuable data; the simplicity of its model and
usage; and its ability to support the needs of a wide variety of users and
projects, from individuals to large-scale enterprise operations.")
    (license asl2.0)))

(define-public rcs
  (package
    (name "rcs")
    (version "5.9.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/rcs/rcs-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0wdmmplga9k05d9k7wjqv4zb6xvvzsli8hmn206pvangki1g66k5"))))
    (build-system gnu-build-system)
    (native-inputs `(("ed" ,ed)))
    (home-page "http://www.gnu.org/software/rcs/")
    (synopsis "Per-file local revision control system")
    (description
     "RCS is the original Revision Control System.  It works on a
file-by-file basis, in contrast to subsequent version control systems such as
CVS, Subversion, and Git.  This can make it suitable for system
administration files, for example, which are often inherently local to one
machine.")
    (license gpl3+)))

(define-public cvs
  (package
    (name "cvs")
    (version "1.12.13")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://ftp.gnu.org/non-gnu/cvs/source/feature/"
                   version "/cvs-" version ".tar.bz2"))
             (sha256
              (base32
               "0pjir8cwn0087mxszzbsi1gyfc6373vif96cw4q3m1x6p49kd1bq"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: The test suite looks flawed, and the package is obsolete anyway.
     '(#:tests? #f))
    (inputs `(("zlib" ,zlib)
              ("nano" ,nano)))                    ; the default editor
    (home-page "http://cvs.nongnu.org")
    (synopsis "Historical centralized version control system")
    (description
     "CVS is a version control system, an important component of Source
Configuration Management (SCM).  Using it, you can record the history of
sources files, and documents.  It fills a similar role to the free software
RCS, PRCS, and Aegis packages.")
    (license gpl1+)))

(define-public vc-dwim
  (package
    (name "vc-dwim")
    (version "1.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/vc-dwim/vc-dwim-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "094pjwshvazlgagc254in2xvrp93vhcj0kb5ms17qs7sch99x9z2"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)
              ("inetutils" ,inetutils)     ; for `hostname', used in the tests
              ("emacs" ,emacs)))           ; for `ctags'
    (home-page "http://www.gnu.org/software/vc-dwim/")
    (synopsis "Version-control-agnostic ChangeLog diff and commit tool")
    (description
     "The vc-dwim package contains two tools, \"vc-dwim\" and \"vc-chlog\".
vc-dwim is a tool that simplifies the task of maintaining a ChangeLog and
using version control at the same time, for example by printing a reminder
when a file change has been described in the ChangeLog but the file has not
been added to the VC.  vc-chlog scans changed files and generates
standards-compliant ChangeLog entries based on the changes that it detects.")
    (license gpl3+)))

(define-public diffstat
  (package
    (name "diffstat")
    (version "1.58")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://invisible-island.net/diffstat/diffstat-"
                    version ".tgz"))
              (sha256
               (base32
                "14rpf5c05ff30f6vn6pn6pzy0k4g4is5im656ahsxff3k58i7mgs"))))
    (build-system gnu-build-system)
    (home-page "http://invisible-island.net/diffstat/")
    (synopsis "Make histograms from the output of 'diff'")
    (description
     "diffstat reads the output of 'diff' and displays a histogram of the
insertions, deletions, and modifications per-file.  It is useful for reviewing
large, complex patch files.")
    (license (x11-style "file://COPYING"))))


(define-public cssc
  (package
    (name "cssc")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/" name "/CSSC-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0bkw6fjh20ppvn54smv05461lm1vcwvn02avx941c4acafmkl1cm"))
              (patches (list (search-patch "cssc-gets-undeclared.patch")
                             (search-patch "cssc-missing-include.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-cons-before
                 'check 'precheck
                 (lambda _
                   (begin
                     (substitute* "tests/common/test-common"
                       (("/bin/pwd") (which "pwd")))

                     (substitute* "tests/prt/all-512.sh"
                       (("/bin/sh") (which "sh")))

                     ;; XXX: This test has no hope of passing until there is a "nogroup"
                     ;; entry (or at least some group to which the guix builder does
                     ;; not belong) in the /etc/group file of the build environment.
                     ;; Currently we do not have such a group.  Disable this test for now.
                     (substitute* "tests/Makefile"
                       (("test-delta ") ""))))
                 %standard-phases)))
    ;; These are needed for the tests
    (native-inputs `(("git" ,git)
                     ("cvs" ,cvs)))
    (home-page "http://www.gnu.org/software/cssc/")
    (synopsis "File-based version control like SCCS")
    (description  "GNU CSSC provides a replacement for the legacy Unix source
code control system SCCS.  This allows old code still under that system to be
accessed and migrated on modern systems.")
    (license gpl3+)))
