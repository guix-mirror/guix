;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
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

(define-module (gnu packages u-boot)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex))

(define-public dtc
  (package
    (name "dtc")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.kernel.org/pub/software/utils/dtc/"
                    "dtc-" version ".tar.xz"))
              (sha256
               (base32
                "155v52palf5fwfcnq696s41whjk0a5dqx98b7maqzdn7xbc2m6bp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (arguments
     `(#:make-flags
       (list "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda _
             (substitute* "Makefile"
               (("/usr/bin/install") "install"))
             (substitute* "Makefile"
               (("PREFIX = \\$\\(HOME\\)") ""))))
         (delete 'configure))))
    (home-page "https://www.devicetree.org")
    (synopsis "Compiles device tree source files")
    (description "@command{dtc} compiles device tree source files to device
tree binary files. These are board description files used by Linux and BSD.")
    (license license:gpl2+)))
