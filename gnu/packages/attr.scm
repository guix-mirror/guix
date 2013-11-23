;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages attr)
  #:use-module (guix licenses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages gettext)
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

         ;; When building natively, adjust the test cases.
         ,(if (%current-target-system)
              '%standard-phases
              '(alist-replace 'check
                              (lambda _
                                ;; Use the right shell.
                                (substitute* "test/run"
                                  (("/bin/sh")
                                   (which "bash")))

                                (system* "make" "tests" "-C" "test")

                                ;; XXX: Ignore the test result since this is
                                ;; dependent on the underlying file system.
                                #t)
                              %standard-phases))))))
    (inputs
     ;; Perl is needed to run tests; remove it from cross builds.
     (if (%current-target-system)
         '()
         `(("perl" ,perl))))
    (native-inputs
     `(("gettext" ,gnu-gettext)))

    (home-page "http://savannah.nongnu.org/projects/attr/")
    (synopsis "Library and tools for manipulating extended attributes")
    (description
     "Portable library and tools for manipulating extended attributes.")
    (license (list gpl2+ lgpl2.1+))))
