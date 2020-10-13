;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2020 Evan Hanson <evhan@foldling.org>
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
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

(define-module (gnu packages chicken)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix build-system chicken)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (gnu packages commencement)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public chicken
  (package
    (name "chicken")
    (version "5.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://code.call-cc.org/releases/"
                                  version "/chicken-" version ".tar.gz"))
              (sha256
               (base32
                "1yl0hxm9cirgcp8jgxp6vv29lpswfvaw3zfkh6rsj0vkrv44k4c1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))

       ;; No `configure' script; run "make check" after "make install" as
       ;; prescribed by README.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (add-after 'install 'check
           (assoc-ref %standard-phases 'check)))

       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "PLATFORM=linux"
                            (string-append "PREFIX=" out)
                            (string-append "VARDIR=" out "/var/lib")))

       ;; Parallel builds are not supported, as noted in README.
       #:parallel-build? #f))
    (native-search-paths
     (list (search-path-specification
            (variable "CHICKEN_REPOSITORY_PATH")
            ;; TODO extract binary version into a module level definition.
            (files (list "var/lib/chicken/11")))))
    (propagated-inputs `(("gcc-toolchain" ,gcc-toolchain)))
    (home-page "https://www.call-cc.org/")
    (synopsis "R5RS Scheme implementation that compiles native code via C")
    (description
     "CHICKEN is a compiler for the Scheme programming language.  CHICKEN
produces portable and efficient C, supports almost all of the R5RS Scheme
language standard, and includes many enhancements and extensions.")
    (license license:bsd-3)))

(define-public chicken-srfi-1
  (package
    (name "chicken-srfi-1")
    (version "0.5.1")
    (source
     (origin
       (method svn-fetch)
       (uri (svn-reference
             (url (string-append
                   "https://code.call-cc.org/svn/chicken-eggs/"
                   "release/5/srfi-1/tags/"
                   version))
             (revision 39055)
             (user-name "anonymous")
             (password "")))
       (file-name (string-append "chicken-srfi-1" version "-checkout"))
       (sha256
        (base32
         "02940zsjrmn7c34rnp1rllm2nahh9jvszlzrw8ak4pf31q09cmq1"))))
    (build-system chicken-build-system)
    (arguments '(#:egg-name "srfi-1"))
    (inputs
     `(("chicken-test" ,chicken-test)))
    (home-page "https://wiki.call-cc.org/eggref/5/srfi-1")
    (synopsis "SRFI-1 list library")
    (description
     "The list library defined in
@uref{https://srfi.schemers.org/srfi-1/srfi-1.html, SRFI-1} contains a lot of
useful list processing procedures for construction, examining, destructuring
and manipulating lists and pairs.")
    (license license:bsd-3)))

(define-public chicken-test
  (package
    (name "chicken-test")
    (version "1.1")
    (source
     (origin
       (method svn-fetch)
       (uri (svn-reference
             (url (string-append "https://code.call-cc.org/svn/chicken-eggs/"
                                 "release/5/test/tags/" version))
             (revision 39263)
             (user-name "anonymous")
             (password "")))
       (file-name (string-append "chicken-test-" version "-checkout"))
       (sha256
        (base32
         "14i91cxsn6hjkx6kqf7i9syck73cw71jik61jmhg87vpxx5kfnzx"))))
    (build-system chicken-build-system)
    (arguments '(#:egg-name "test"))
    (home-page "https://wiki.call-cc.org/eggref/5/test")
    (synopsis "Yet another testing utility")
    (description
     "This package provides a simple testing utility for CHICKEN Scheme.")
    (license license:bsd-3)))

