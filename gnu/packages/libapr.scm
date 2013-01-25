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

(define-module (gnu packages libapr)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages autotools))

(define-public libapr
  (package
    (name "libapr")
    (version "1.4.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://mirrors.ircam.fr/pub/apache//apr/apr-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1g0w9396akmhhrmjzmcwddny5ms43zvj2mrpdkyfcqxizrh5wqwv"))))
    (build-system gnu-build-system)
    (arguments
      `(#:patches (list (assoc-ref %build-inputs
                                   "patch/skip-test"))
        #:patch-flags '("-p0")))
    (inputs `(("perl" ,perl)
              ("libtool" ,libtool)
              ("patch/skip-test"
               ,(search-patch "libapr-skip-getservbyname-test.patch"))))
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

(define-public libaprutil
  (package
    (name "libaprutil")
    (version "1.5.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://apache.crihan.fr/dist//apr/apr-util-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0832cb90zd7zqhhdx0v3i8viw1rmn0d945qbk1zid3cnky9r0s19"))))
    (build-system gnu-build-system)
    (inputs
      `(("libapr" ,libapr)))
    (arguments
     '(#:phases
       (alist-replace
        'configure
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out"))
                (libapr (assoc-ref inputs "libapr")))
            (setenv "CONFIG_SHELL" (which "bash"))
            (zero?
             (system* "./configure"
                      (string-append "--prefix=" out)
                      (string-append "--with-apr=" libapr)))))
        %standard-phases)))
    (home-page "http://apr.apache.org/")
    (synopsis "One of the Apache Portable Runtime Library companions")
    (description
     "APR-util provides a number of helpful abstractions on top of APR.")
    (license asl2.0)))
