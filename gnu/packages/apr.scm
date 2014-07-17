;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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

(define-module (gnu packages apr)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages autotools))

(define-public apr
  (package
    (name "apr")
    (version "1.4.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://apache/apr/apr-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0884csfk3f530yscak0jlr6w929s3ys0n7fpwdg3dii1sgwd5f31"))
             (patches
              (list (search-patch "apr-skip-getservbyname-test.patch")))
             (patch-flags '("-p0"))))
    (build-system gnu-build-system)
    (arguments
     ;; Sometimes we end up with two processes concurrently trying to make
     ;; 'libmod_test.la': <http://hydra.gnu.org/build/60266/nixlog/2/raw>.
     ;; Thus, build sequentially.
     '(#:parallel-build? #f))
    (inputs `(("perl" ,perl)
              ("libtool" ,libtool)))
    (home-page "http://apr.apache.org/")
    (synopsis "The Apache Portable Runtime Library")
    (description
     "The mission of the Apache Portable Runtime (APR) project is to create and
maintain software libraries that provide a predictable and consistent interface
to underlying platform-specific implementations. The primary goal is to provide
an API to which software developers may code and be assured of predictable if
not identical behaviour regardless of the platform on which their software is
built, relieving them of the need to code special-case conditions to work
around or take advantage of platform-specific deficiencies or features.")
    (license asl2.0)))

(define-public apr-util
  (package
    (name "apr-util")
    (version "1.5.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://apache/apr/apr-util-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "19qjxpckb9p4j9pbk8kcirg6k5vqnjrqhnk9xx2c5m9964p3vkls"))))
    (build-system gnu-build-system)
    (inputs
      `(("apr" ,apr)))
    (arguments
     '(#:phases
       (alist-replace
        'configure
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out"))
                (apr (assoc-ref inputs "apr")))
            (setenv "CONFIG_SHELL" (which "bash"))
            (zero?
             (system* "./configure"
                      (string-append "--prefix=" out)
                      (string-append "--with-apr=" apr)))))
        %standard-phases)

       ;; There are race conditions during 'make check'.  Typically, the
       ;; 'testall' executable is not built yet by the time 'make check' tries
       ;; to run it.  See
       ;; <http://lists.gnu.org/archive/html/guix-devel/2014-03/msg00261.html>.
       #:parallel-tests? #f))
    (home-page "http://apr.apache.org/")
    (synopsis "One of the Apache Portable Runtime Library companions")
    (description
     "APR-util provides a number of helpful abstractions on top of APR.")
    (license asl2.0)))
