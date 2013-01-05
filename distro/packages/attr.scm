;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (distro packages attr)
  #:use-module (guix licenses)
  #:use-module (distro packages perl)
  #:use-module ((distro packages gettext)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public attr
  (package
    (name "attr")
    (version "2.4.46")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://savannah/attr/attr-"
                          version ".src.tar.gz"))
      (sha256
       (base32
        "07qf6kb2zk512az481bbnsk9jycn477xpva1a726n5pzlzf9pmnw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-cons-after
        'configure 'patch-makefile-SHELL
        (lambda _
          (patch-makefile-SHELL "include/buildmacros"))
        (alist-replace
         'install
         (lambda _
           (zero? (system* "make"
                           "install"
                           "install-lib"
                           "install-dev")))
         (alist-replace
          'check
          (lambda _
            ;; Use the right shell.
            (substitute* "test/run"
              (("/bin/sh")
               (which "bash")))

            (system* "make" "tests" "-C" "test")

            ;; XXX: Ignore the test result since this is dependent on the
            ;; underlying file system.
            #t)
          %standard-phases)))))
    (inputs `(("perl" ,perl)
              ("gettext" ,guix:gettext)))
    (home-page
     "http://savannah.nongnu.org/projects/attr/")
    (synopsis
     "Library and tools for manipulating extended attributes")
    (description
     "Portable library and tools for manipulating extended attributes.")
    (license '(gpl2+ lgpl2.1+))))
