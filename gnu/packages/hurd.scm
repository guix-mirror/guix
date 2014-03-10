;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
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

(define-module (gnu packages hurd)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages autotools))

(define-public gnumach-headers
  (package
    (name "gnumach-headers")
    (version "1.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnumach/gnumach-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0r371wsm7imx356p0xsls5hifb1gf9y90rm1phr0qkahbmfk9hlv"))))
    (build-system gnu-build-system)
    (arguments
    `(#:phases (alist-replace
                'install
                (lambda _
                  (zero?
                   (system* "make" "install-data")))
                (alist-delete
                 'build
                 %standard-phases))

      ;; GNU Mach supports only IA32 currently, so cheat so that we can at
      ;; least install its headers.
      #:configure-flags '("--build=i686-pc-gnu")

      #:tests? #f))
    (home-page "https://www.gnu.org/software/hurd/microkernel/mach/gnumach.html")
    (synopsis "GNU Mach kernel headers")
    (description
     "Headers of the GNU Mach kernel.")
    (license gpl2+)))

(define-public mig
  (package
    (name "mig")
    (version "1.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/mig/mig-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1jgzggnbp22sa8z5dilm43zy12vlf1pjxfb3kh13xrfhcay0l97b"))))
    (build-system gnu-build-system)
    (inputs `(("gnumach-headers" ,gnumach-headers)))
    (native-inputs
     `(("flex" ,flex)
       ("bison" ,bison)))
    (arguments `(#:tests? #f))
    (home-page "http://www.gnu.org/software/hurd/microkernel/mach/mig/gnu_mig.html")
    (synopsis "Mach 3.0 interface generator for the Hurd")
    (description
     "GNU MIG is the GNU distribution of the Mach 3.0 interface generator
MIG, as maintained by the GNU Hurd developers for the GNU project.
You need this tool to compile the GNU Mach and GNU Hurd distributions,
and to compile the GNU C library for the Hurd. Also,you will need it
for other software in the GNU system that uses Mach-based inter-process
communication.")
    (license gpl2+)))

(define-public hurd-headers
  (package
    (name "hurd-headers")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/hurd/hurd-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0lvkz3r0ngb4bsn2hzdc9vjpyrfa3ls36jivrvy1n7f7f55zan7q"))))
    (build-system gnu-build-system)
    (native-inputs
     `(;; Autoconf shouldn't be necessary but there seems to be a bug in the
       ;; build system triggering its use.
       ("autoconf" ,autoconf)

       ("mig" ,mig)))
    (arguments
     `(#:phases (alist-replace
                 'install
                 (lambda _
                   (zero? (system* "make" "install-headers" "no_deps=t")))
                 (alist-delete 'build %standard-phases))

       #:configure-flags '(;; Pretend we're on GNU/Hurd; 'configure' wants
                           ;; that.
                           "--host=i686-pc-gnu"

                           ;; Reduce set of dependencies.
                           "--without-parted")

       #:tests? #f))
    (home-page "http://www.gnu.org/software/hurd/hurd.html")
    (synopsis "GNU Hurd headers")
    (description
     "This package provides C headers of the GNU Hurd, used to build the GNU C
Library and other user programs.")
    (license gpl2+)))
