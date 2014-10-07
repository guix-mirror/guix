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

(define-module (gnu packages icu4c)
  #:use-module (gnu packages)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages perl)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public icu4c
  (package
   (name "icu4c")
   (version "53.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://download.icu-project.org/files/icu4c/"
                   version
                   "/icu4c-"
                   (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                   "-src.tgz"))
            (sha256
             (base32 "0a4sg9w054640zncb13lhrcjqn7yg1qilwd1mczc4w60maslz9vg"))
            (patches (list (search-patch "icu4c-test-date-format.patch")))))
   (build-system gnu-build-system)
   (inputs
    `(("patchelf" ,patchelf)
      ("perl" ,perl)))
   (arguments
    `(#:modules ((guix build gnu-build-system)
                 (guix build utils)
                 (guix build rpath)
                 (srfi srfi-26))
      #:imported-modules ((guix build gnu-build-system)
                          (guix build utils)
                          (guix build rpath))
      #:phases
      (alist-cons-after
       'unpack 'chdir-to-source
       (lambda _ (chdir "source"))
       (alist-cons-before
        'configure 'patch-configure
        (lambda _
          ;; patch out two occurrences of /bin/sh from configure script
          ;; that might have disappeared in a release later than 52.1
          (substitute* "configure"
            (("`/bin/sh")
             (string-append "`" (which "bash")))))
       (alist-cons-after
        'strip 'add-lib-to-runpath
        (lambda* (#:key outputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (lib (string-append out "/lib")))
            ;; Add LIB to the RUNPATH of all the binaries.
            (with-directory-excursion out
              (for-each (cut augment-rpath <> lib)
                        (append
                         (find-files "bin" ".*") 
                         (find-files "sbin" ".*"))))))
        %standard-phases)))))
   (synopsis "ICU, International Components for Unicode")
   (description
    "ICU is a set of C/C++ and Java libraries providing Unicode and
globalisation support for software applications. This package contains the
C/C++ part.")
   (license x11)
   (home-page "http://site.icu-project.org/")))
