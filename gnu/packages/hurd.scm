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
  #:use-module (guix build-system gnu))

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
