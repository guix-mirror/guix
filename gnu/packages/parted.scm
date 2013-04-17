;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages parted)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages check)
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (gnu packages linux)
  #:use-module (gnu packages readline))

(define-public parted
  (package
    (name "parted")
    (version "3.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/parted/parted-"
                          version ".tar.xz"))
      (sha256
       (base32
        "05fa4m1bky9d13hqv91jlnngzlyn7y4rnnyq6d86w0dg3vww372y"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags '("--disable-device-mapper")
                 #:phases (alist-cons-before
                           'configure 'fix-mkswap
                           (lambda* (#:key inputs #:allow-other-keys)
                             (let ((util-linux (assoc-ref inputs
                                                          "util-linux")))
                               (substitute*
                                   "tests/t9050-partition-table-types.sh"
                                 (("mkswap")
                                  (string-append util-linux "/sbin/mkswap")))))
                           %standard-phases)))
    (inputs
     ;; XXX: add 'lvm2'.
     `(("check" ,check)
       ("gettext" ,guix:gettext)
       ("readline" ,readline)
       ("util-linux" ,util-linux)))
    (home-page "http://www.gnu.org/software/parted/")
    (synopsis "Disk partition editor")
    (description
     "GNU Parted is an industrial-strength package for creating, destroying,
resizing, checking and copying partitions, and the file systems on them.  This
is useful for creating space for new operating systems, reorganising disk
usage, copying data on hard disks and disk imaging.

It contains a library, libparted, and a command-line frontend, parted, which
also serves as a sample implementation and script backend.")
    (license gpl3+)))
