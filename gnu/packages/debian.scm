;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
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

(define-module (gnu packages debian)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wget))

(define-public debian-archive-keyring
  (package
    (name "debian-archive-keyring")
    (version "2021.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://salsa.debian.org/release-team/debian-archive-keyring.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0dcmv7y1k6j3a646kr0rkd2a0c4j2wrz868bh8j9zjx1npzns73q"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "verify-results"
       #:parallel-build? #f ; has race conditions
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apt (string-append out "/etc/apt/trusted.gpg.d/"))
                    (key (string-append out "/share/keyrings/")))
               (install-file "keyrings/debian-archive-keyring.gpg" key)
               (install-file "keyrings/debian-archive-removed-keys.gpg" key)
               (for-each (lambda (file)
                           (install-file file apt))
                         (find-files "trusted.gpg" "\\.gpg$")))
             #t)))))
    (native-inputs
     (list gnupg jetring))
    (home-page "https://packages.qa.debian.org/d/debian-archive-keyring.html")
    (synopsis "GnuPG archive keys of the Debian archive")
    (description
     "The Debian project digitally signs its Release files.  This package
contains the archive keys used for that.")
    (license (list license:public-domain ; the keys
                   license:gpl2+)))) ; see debian/copyright

(define-public debian-ports-archive-keyring
  (package
    (name "debian-ports-archive-keyring")
    (version "2021.12.30")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://debian/pool/main/d"
                            "/debian-ports-archive-keyring"
                            "/debian-ports-archive-keyring_" version ".tar.xz"))
        (sha256
         (base32
          "14f9hklr8gdlp782j5ijmm0nh061zcfw9vwpr8smb7rdfzk4wk70"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f              ; No test suite.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)    ; No configure script.
         (replace 'build
           (lambda _
             ;; gpg options derived from the debian/rules file.
             (let ((gpg-options (list "--no-options" "--no-default-keyring"
                                      "--no-auto-check-trustdb" "--no-keyring"
                                      "--import-options" "import-export"
                                      "--import")))
               (with-output-to-file "debian-ports-archive-keyring.gpg"
                 (lambda _
                   (apply invoke "gpg"
                          (append gpg-options (find-files "active-keys")))))
               (with-output-to-file "debian-ports-archive-keyring-removed.gpg"
                 (lambda _
                   (apply invoke "gpg"
                          (append gpg-options (find-files "removed-keys")))))
               (mkdir "trusted.gpg")
               (for-each
                 (lambda (key)
                   (with-output-to-file
                     (string-append "trusted.gpg/" (basename key ".key") ".gpg")
                     (lambda _
                       (apply invoke "gpg" (append gpg-options (list key))))))
                 (find-files "active-keys"))
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apt (string-append out "/etc/apt/trusted.gpg.d/"))
                    (key (string-append out "/share/keyrings/")))
               (install-file "debian-ports-archive-keyring.gpg" key)
               (install-file "debian-ports-archive-keyring-removed.gpg" key)
               (for-each (lambda (file)
                           (install-file file apt))
                         (find-files "trusted.gpg" "\\.gpg$")))
             #t)))))
    (native-inputs
     (list gnupg))
    (home-page "https://tracker.debian.org/pkg/debian-ports-archive-keyring")
    (synopsis "GnuPG archive keys of the Debian ports archive")
    (description
     "The Debian ports-archive digitally signs its Release files.  This package
contains the archive keys used for that.")
    ;; "The keys in the keyrings don't fall under any copyright."
    (license license:public-domain)))

(define-public ubuntu-keyring
  (package
    (name "ubuntu-keyring")
    (version "2021.03.26")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://launchpad.net/ubuntu/+archive/primary/"
                            "+files/" name "_" version ".tar.gz"))
        (sha256
         (base32
          "1ccvwh4s51viyhcg8gh189jmvbrhc5wv1bbp4minz3200rffsbj9"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((out (assoc-ref %outputs "out"))
                          (apt (string-append out "/etc/apt/trusted.gpg.d/"))
                          (key (string-append out "/share/keyrings/")))
                     (setenv "PATH" (string-append
                                      (assoc-ref %build-inputs "gzip") "/bin:"
                                      (assoc-ref %build-inputs "tar") "/bin"))
                     (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
                     (for-each (lambda (file)
                                 (install-file file apt))
                               (find-files "." "ubuntu-[^am].*\\.gpg$"))
                     (for-each (lambda (file)
                                 (install-file file key))
                               (find-files "." "ubuntu-[am].*\\.gpg$")))
                   #t)))
    (native-inputs
     (list tar gzip))
    (home-page "https://launchpad.net/ubuntu/+source/ubuntu-keyring")
    (synopsis "GnuPG keys of the Ubuntu archive")
    (description
     "The Ubuntu project digitally signs its Release files.  This package
contains the archive keys used for that.")
    (license (list license:public-domain ; the keys
                   license:gpl2+)))) ; see debian/copyright

(define-public debootstrap
  (package
    (name "debootstrap")
    (version "1.0.126")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://salsa.debian.org/installer-team/debootstrap.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0hfx6k86kby4xf0xqskpllq00g159j4khh66hfi6dhcdb91dgyd7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out    (assoc-ref outputs "out"))
                   (tzdata (assoc-ref inputs "tzdata"))
                   (debian (assoc-ref inputs "debian-keyring"))
                   (ubuntu (assoc-ref inputs "ubuntu-keyring")))
               (substitute* "Makefile"
                 (("/usr") "")
                 (("-o root -g root") "")
                 (("chown root.*") "\n"))
               (substitute* '("scripts/etch"
                              "scripts/potato"
                              "scripts/sarge"
                              "scripts/sid"
                              "scripts/woody"
                              "scripts/woody.buildd")
                 (("/usr") debian))
               (substitute* "scripts/gutsy"
                 (("/usr") ubuntu))
               (substitute* "debootstrap"
                 (("=/usr") (string-append "=" out)))
               ;; Ensure PATH works both in guix and within the debian chroot
               ;; workaround for: https://bugs.debian.org/929889
               (substitute* "functions"
                 (("PATH=/sbin:/usr/sbin:/bin:/usr/bin")
                  "PATH=$PATH:/sbin:/usr/sbin:/bin:/usr/bin"))
               (substitute* (find-files "scripts" ".")
                 (("/usr/share/zoneinfo") (string-append tzdata "/share/zoneinfo")))
               #t)))
         (add-after 'install 'install-man-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "debootstrap.8"
                             (string-append out "/share/man/man8"))
               #t)))
         (add-after 'install 'wrap-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((debootstrap (string-append (assoc-ref outputs "out")
                                               "/sbin/debootstrap"))
                   (path        (getenv "PATH")))
               (wrap-program debootstrap
                             `("PATH" ":" prefix (,path)))
               #t))))
       #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:tests? #f)) ; no tests
    (inputs
     `(("debian-keyring" ,debian-archive-keyring)
       ("ubuntu-keyring" ,ubuntu-keyring)
       ("tzdata" ,tzdata)

       ;; Called at run-time from various places, needs to be in PATH.
       ("gnupg" ,gnupg)
       ("wget" ,wget)))
    (native-inputs
     (list perl))
    (home-page "https://tracker.debian.org/pkg/debootstrap")
    (synopsis "Bootstrap a basic Debian system")
    (description "Debootstrap is used to create a Debian base system from
scratch, without requiring the availability of @code{dpkg} or @code{apt}.
It does this by downloading .deb files from a mirror site, and carefully
unpacking them into a directory which can eventually be chrooted into.")
    (license license:gpl2)))

(define-public debianutils
  (package
    (name "debianutils")
    (version "5.5-1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/debian/debianutils.git")
                    (commit (string-append "debian/" version))))
              (file-name (git-file-name "debianutils" version))
              (sha256
               (base32
                "1sbdjcb44g2s1zxjf9kxrp9drf9mmh6b49a9z3k428gmc6zsci4r"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake gettext-minimal po4a))
    (home-page "https://packages.debian.org/unstable/debianutils")
    (synopsis "Miscellaneous shell utilities")
    (description
     "This package provides a number of utilities which are mostly for use
in installation scripts of Debian packages.  The programs included are
@command{add-shell}, @command{installkernel}, @command{ischroot},
@command{remove-shell}, @command{run-parts}, @command{savelog},
@command{tempfile}, and @command{which}.")
    (license (list license:gpl2+
                   ;; The 'savelog' program is distributed under a
                   ;; GPL-compatible copyleft license.
                   (license:fsf-free "file://debian/copyright"
                                     "The SMAIL General Public License, see
debian/copyright for more information.")))))

(define-public apt-mirror
  (let ((commit "e664486a5d8947c2579e16dd793d762ea3de4202")
        (revision "1"))
    (package
      (name "apt-mirror")
      (version (git-version "0.5.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/apt-mirror/apt-mirror/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0qj6b7gldwcqyfs2kp6amya3ja7s4vrljs08y4zadryfzxf35nqq"))))
      (build-system gnu-build-system)
      (outputs '("out"))
      (arguments
       `(#:tests? #f
         ;; sysconfdir is not PREFIXed in the makefile but DESTDIR is
         ;; honored correctly; we therefore use DESTDIR for our
         ;; needs. A more correct fix would involve patching.
         #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                            "PREFIX=/")
         #:phases (modify-phases %standard-phases (delete 'configure))))
      (inputs
       (list wget perl))
      (home-page "http://apt-mirror.github.io/")
      (synopsis "Script for mirroring a Debian repository")
      (description
       "apt-mirror is a small tool that provides the ability to
selectively mirror Debian and Ubuntu GNU/Linux distributions or any
other apt sources typically provided by open source developers.")
      (license license:gpl2))))

(define-public dpkg
  (package
    (name "dpkg")
    (version "1.21.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.dpkg.org/git/dpkg/dpkg")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0g33cyd0qbyfdrphcw8m8ikj2hxqpjbyxbhvnp751515c8hgc4rx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'patch-version
           (lambda _
             (patch-shebang "get-version")
             (with-output-to-file ".dist-version"
               (lambda () (display ,version)))))
         (add-after 'unpack 'set-perl-libdir
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out"))
                   (perl (assoc-ref inputs "perl")))
               (setenv "PERL_LIBDIR"
                       (string-append out
                                      "/lib/perl5/site_perl/"
                                      ,(package-version perl)))))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("perl-io-string" ,perl-io-string)))
    (inputs
     (list bzip2
           libmd
           ncurses
           perl
           xz
           zlib))
    (home-page "https://wiki.debian.org/Teams/Dpkg")
    (synopsis "Debian package management system")
    (description "This package provides the low-level infrastructure for
handling the installation and removal of Debian software packages.")
    (license license:gpl2+)))

(define-public reprepro
  (package
    (name "reprepro")
    (version "5.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://salsa.debian.org/brlink/reprepro.git/")
               (commit (string-append name "-" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1kn7m5rxay6q2c4vgjgm4407xx2r46skkkb6rn33m6dqk1xfkqnh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; testtool not found
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
               (with-directory-excursion "tests"
                 (invoke (which "sh") "test.sh"))
               #t)))
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bash (string-append out "/etc/bash_completion.d/"))
                    (zsh  (string-append out "/share/zsh/site-fucnctions/")))
               (mkdir-p bash)
               (mkdir-p zsh)
               (copy-file "docs/reprepro.bash_completion"
                          (string-append bash "reprepro"))
               (copy-file "docs/reprepro.zsh_completion"
                          (string-append zsh "_reprepro"))
               #t))))))
    (inputs
     (list bdb
           bzip2
           gpgme
           libarchive
           xz
           zlib))
    (native-inputs
     (list autoconf automake))
    (home-page "https://salsa.debian.org/brlink/reprepro")
    (synopsis "Debian package repository producer")
    (description "Reprepro is a tool to manage a repository of Debian packages
(@code{.deb}, @code{.udeb}, @code{.dsc}, ...).  It stores files either being
injected manually or downloaded from some other repository (partially) mirrored
into one pool/ hierarchy.  Managed packages and files are stored in a Berkeley
DB, so no database server is needed.  Checking signatures of mirrored
repositories and creating signatures of the generated Package indices is
supported.")
    (license license:gpl2)))
