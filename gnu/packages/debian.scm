;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages perl))

(define-public debian-archive-keyring
  (package
    (name "debian-archive-keyring")
    (version "2019.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://salsa.debian.org/release-team/debian-archive-keyring.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0bphwji3ywk1zi5bq8bhqk7l51fwjy1idwsw7zfqnxca8m5wvw1g"))))
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
     `(("gnupg" ,gnupg)
       ("jetring" ,jetring)))
    (home-page "https://packages.qa.debian.org/d/debian-archive-keyring.html")
    (synopsis "GnuPG archive keys of the Debian archive")
    (description
     "The Debian project digitally signs its Release files.  This package
contains the archive keys used for that.")
    (license (list license:public-domain ; the keys
                   license:gpl2+)))) ; see debian/copyright

(define-public ubuntu-keyring
  (package
    (name "ubuntu-keyring")
    (version "2018.09.18.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://launchpad.net/ubuntu/+archive/primary/"
                            "+files/" name "_" version ".tar.gz"))
        (sha256
         (base32
          "0csx2n62rj9rxjv4y8qhby7l9rbybfwrb0406pc2cjr7f2yk91af"))))
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
     `(("tar" ,tar)
       ("gzip" ,gzip)))
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
    (version "1.0.123")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://salsa.debian.org/installer-team/debootstrap.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0fr5ir8arzisx71jybbk4xz85waz50lf2y052nfimzh6vv9dx54c"))))
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
     `(("perl" ,perl)))
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
    (version "4.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/debian/debianutils.git")
                    (commit (string-append "debian/" version))))
              (file-name (git-file-name "debianutils" version))
              (sha256
               (base32
                "1fmhzvymajack7kh8g4qjbwd9mq85z6rxl1psd1sm67s5695i9rc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'bootstrap 'create-translations
                    (lambda _
                      (with-directory-excursion "po4a"
                        (invoke "po4a" "--no-backups" "po4a.conf"))
                      #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("po4a" ,po4a)))
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
       `(("wget" ,wget)
         ("perl" ,perl)))
      (home-page "http://apt-mirror.github.io/")
      (synopsis "Script for mirroring a Debian repository")
      (description
       "apt-mirror is a small tool that provides the ability to
selectively mirror Debian and Ubuntu GNU/Linux distributions or any
other apt sources typically provided by open source developers.")
      (license license:gpl2))))
