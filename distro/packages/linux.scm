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
  #:use-module (guix build-system gnu))

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
