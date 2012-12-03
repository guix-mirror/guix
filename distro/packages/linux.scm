;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (distro packages flex)
  #:use-module (distro packages perl)
  #:use-module (distro packages ncurses)
  #:use-module (guix build-system gnu))

(define-public linux-libre-headers
  (let* ((version* "3.3.8")
         (build-phase
          '(lambda* (#:key outputs #:allow-other-keys)
             (setenv "ARCH" "x86_64")       ; XXX
             (and (zero? (system* "make" "defconfig"))
                  (zero? (system* "make" "mrproper" "headers_check")))))
         (install-phase
          `(lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (and (zero? (system* "make"
                                    (string-append "INSTALL_HDR_PATH=" out)
                                    "headers_install"))
                    (mkdir (string-append out "/include/config"))
                    (call-with-output-file
                        (string-append out
                                       "/include/config/kernel.release")
                      (lambda (p)
                        (format p "~a-default~%" ,version*))))))))
   (package
    (name "linux-libre-headers")
    (version version*)
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://linux-libre.fsfla.org/pub/linux-libre/releases/3.3.8-gnu/linux-libre-"
                   version "-gnu.tar.xz"))
             (sha256
              (base32
               "0jkfh0z1s6izvdnc3njm39dhzp1cg8i06jv06izwqz9w9qsprvnl"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases (alist-replace
                 'build ,build-phase
                 (alist-replace
                  'install ,install-phase
                  (alist-delete 'configure %standard-phases)))
       #:tests? #f))
    (synopsis "GNU Linux-Libre kernel headers")
    (description "Headers of the Linux-Libre kernel.")
    (license "GPLv2")
    (home-page "http://www.gnu.org/software/linux-libre/"))))

(define-public linux-pam
  (package
    (name "linux-pam")
    (version "1.1.6")
    (source
     (origin
      (method url-fetch)
      (uri (list (string-append "http://www.linux-pam.org/library/Linux-PAM-"
                                version ".tar.bz2")
                 (string-append "mirror://kernel.org/linux/libs/pam/library/Linux-PAM-"
                                version ".tar.bz2")))
      (sha256
       (base32
        "1hlz2kqvbjisvwyicdincq7nz897b9rrafyzccwzqiqg53b8gf5s"))))
    (build-system gnu-build-system)
    (inputs
     `(("flex" ,flex)

       ;; TODO: optional dependencies
       ;; ("libxcrypt" ,libxcrypt)
       ;; ("cracklib" ,cracklib)
       ))
    (arguments
     ;; XXX: Tests won't run in chroot, presumably because /etc/pam.d
     ;; isn't available.
     '(#:tests? #f))
    (home-page "http://www.linux-pam.org/")
    (synopsis "Pluggable authentication modules for Linux")
    (description
     "A *Free* project to implement OSF's RFC 86.0.
Pluggable authentication modules are small shared object files that can
be used through the PAM API to perform tasks, like authenticating a user
at login.  Local and dynamic reconfiguration are its key features")
    (license "BSD")))

(define-public psmisc
  (package
    (name "psmisc")
    (version "22.20")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/psmisc/psmisc/psmisc-"
                          version ".tar.gz"))
      (sha256
       (base32
        "052mfraykmxnavpi8s78aljx8w87hyvpx8mvzsgpjsjz73i28wmi"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://psmisc.sourceforge.net/")
    (synopsis
     "set of utilities that use the proc filesystem, such as fuser, killall, and pstree")
    (description
     "This PSmisc package is a set of some small useful utilities that
use the proc filesystem. We're not about changing the world, but
providing the system administrator with some help in common tasks.")
    (license "GPLv2+")))
