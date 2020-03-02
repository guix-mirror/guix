;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
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
    (version "1.0.119")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://salsa.debian.org/installer-team/debootstrap.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0p0p8qmlsbvpfa0r7ifghr67zrrc96d83r9qwahzaxyxkvnhr4x4"))))
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
       ("tzdata" ,tzdata)))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "https://tracker.debian.org/pkg/debootstrap")
    (synopsis "Bootstrap a basic Debian system")
    (description "Debootstrap is used to create a Debian base system from
scratch, without requiring the availability of @code{dpkg} or @code{apt}.
It does this by downloading .deb files from a mirror site, and carefully
unpacking them into a directory which can eventually be chrooted into.")
    (license license:gpl2)))
