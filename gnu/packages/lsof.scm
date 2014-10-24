;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages lsof)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl))

(define-public lsof
  (package
   (name "lsof")
   (version "4.87")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://lsof.itap.purdue.edu/pub/tools/unix/lsof/lsof_"
                                version ".tar.bz2"))
            (sha256 (base32
                     "0b6si72sml7gr9784ak491cxxbm9mx5bh174yg6rrirbv04kgpfz"))))
   (build-system gnu-build-system)
   (inputs `(("perl" ,perl)))
   (arguments
    `(#:tests? #f ; no test target
      #:phases
      (alist-replace
       'unpack
       (lambda* (#:key source #:allow-other-keys)
         (let ((unpack (assoc-ref %standard-phases 'unpack)))
           (unpack #:source source)
           (unpack #:source (car (find-files "." "\\.tar$")))))
      (alist-replace
       'configure
       (lambda _
         (setenv "LSOF_CC" "gcc")
         (setenv "LSOF_MAKE" "make")
         (system* "./Configure" "linux"))
      (alist-replace
       'install
       (lambda* (#:key outputs #:allow-other-keys)
         (let ((out (assoc-ref outputs "out")))
           (mkdir out)
           (mkdir (string-append out "/bin"))
           (copy-file "lsof" (string-append out "/bin/lsof"))
           (mkdir (string-append out "/share"))
           (mkdir (string-append out "/share/man"))
           (mkdir (string-append out "/share/man/man8"))
           (copy-file "lsof.8" (string-append out "/share/man/man8/lsof.8"))
         ))
       %standard-phases)))))
   (synopsis "Display information about open files")
   (description
    "Lsof stands for LiSt Open Files, and it does just that.
It lists information about files that are open by the processes running
on the system.")
   (license (license:fsf-free
             "file://00FAQ"
             "License inspired by zlib, see point 1.9 of 00FAQ in the distribution."))
   (home-page "http://people.freebsd.org/~abe/")))
