;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages check)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public check
  (package
    (name "check")
    (version "0.9.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/check/check/"
                          version "/check-" version ".tar.gz"))
      (sha256
       (base32
        "1jcahzrvxcnp5chdn2x46l0y4aba8d8yd70lljfin7h5knxrlyhs"))))
    (build-system gnu-build-system)
    (home-page "http://check.sourceforge.net/")
    (synopsis "Check, a unit testing framework for C")
    (description
     "Check is a unit testing framework for C. It features a simple
interface for defining unit tests, putting little in the way of the
developer.  Tests are run in a separate address space, so Check can
catch both assertion failures and code errors that cause segmentation
faults or other signals. The output from unit tests can be used within
source code editors and IDEs.")
    (license lgpl2.1+)))


(define-public cppunit
  (package
    (name "cppunit")
    (version "1.12.1")
    (source (origin
             (method url-fetch)
              (uri (string-append "mirror://sourceforge/cppunit/" name "/" 
                                  name "-"
                                  version ".tar.gz"))
             (sha256
              (base32
               "0jm49v5rmc5qw34vqs56gy8xja1dhci73bmh23cig4kcir6a0a5c"))))
    (build-system gnu-build-system)
    (home-page "http://sourceforge.net/projects/cppunit/")
    (synopsis "Unit testing framework for C++")
    (description "CppUnit is the C++ port of the famous JUnit framework for
unit testing. Test output is in XML for automatic testing and GUI based for
supervised tests.")
    (license lgpl2.1))) ; no copyright notices. LGPL2.1 is in the tarball


