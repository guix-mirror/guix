;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu packages efi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public gnu-efi
  (package
    (name "gnu-efi")
    (version "3.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/gnu-efi/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32
         "1w3p4aqlc5j93q44la7dc8cr3hky20zvsd0h0k2lyzhwmrzfl5b7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; None exist.
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (synopsis "EFI toolchain")
    (description "This package provides an EFI (Extensible Firmware
Interface) toolchain for building programs that can run in the
environment presented by Intel's EFI.")
    (home-page "https://directory.fsf.org/wiki/GNU_EFI")
    ;; Distribution is allowed only when accepting all those licenses.
    (license (list license:bsd-2 license:bsd-3 license:bsd-4 license:expat))))
