;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Dave Love <fx@gnu.org>
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

(define-module (gnu packages fabric-management)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl))

;; Fixme: Done for the library, but needs support for running the daemon
;;        (shepherd definition).
;;        We should probably have a lib output, but that currently generates
;;        a cycle.
(define-public opensm
  (package
    (name "opensm")
    (version "3.3.20")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.openfabrics.org/downloads/management/opensm-"
                       version ".tar.gz"))
       (sha256 (base32 "162sg1w7kgy8ayl8a4dcbrfacmnfy2lr9a2yjyq0k65rmd378zg1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("flex" ,flex)
       ("bison" ,bison)))
    (inputs
     `(("rdma-core" ,rdma-core)))
    (arguments
     `(#:configure-flags '("--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((base (assoc-ref outputs "out"))
                    (doc  (string-append base "/share/doc/"
                                         ,name "-" ,version)))
               (for-each (lambda (file)
                           (install-file file doc))
                         (append (list "AUTHORS" "COPYING" "ChangeLog")
                                 (find-files "doc")))
               #t))))))
    (home-page "https://www.openfabrics.org/")
    (synopsis "OpenIB InfiniBand Subnet Manager and management utilities")
    (description "\
OpenSM is the OpenIB project's Subnet Manager for Infiniband networks.
The subnet manager is run as a system daemon on one of the machines in
the infiniband fabric to manage the fabric's routing state.  This package
also contains various tools for diagnosing and testing Infiniband networks
that can be used from any machine and do not need to be run on a machine
running the opensm daemon.")
    (license (list gpl2 bsd-2))))

(define-public infiniband-diags
  (package
    (name "infiniband-diags")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/linux-rdma/infiniband-diags/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ns9sjwvxnklhi47d6k5x8kxdk1n7f5362y45xwxqmr7gwfvpmwa"))))
    (build-system gnu-build-system)
    (inputs
     `(("rdma-core" ,rdma-core)
       ("opensm" ,opensm)
       ("glib" ,glib)))
    (outputs '("out" "lib"))
    (native-inputs
     ;; FIXME: needs rst2man for man pages
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (arguments
     '(#:configure-flags
       (list (string-append "CPPFLAGS=-I" (assoc-ref %build-inputs "opensm")
                            "/include/infiniband")
             (string-append "--with-perl-installdir=" (assoc-ref %outputs "lib")
                            "/lib/perl5/vendor_perl")
             "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autotools
           (lambda _
             (zero? (system "./autogen.sh"))))
         (add-after 'install 'licence
           (lambda _
             (let ((doc (string-append (assoc-ref %outputs "lib") "/share/doc")))
               (mkdir-p doc)
               (install-file "COPYING" doc))))
         (add-after 'install-file 'move-perl
           ;; Avoid perl in lib closure
           (lambda _
             (let ((perlout (string-append (assoc-ref %outputs "out") "/lib"))
                   (perlin (string-append (assoc-ref %outputs "lib")
                                          "/lib/perl5")))
               (mkdir-p perlout)
               (rename-file perlin perlout)
               #t))))))
    (home-page "https://github.com/linux-rdma/infiniband-diags")
    (synopsis "Infiniband diagnotic tools")
    (description "This is a set of command-line utilities to help configure,
debug, and maintain Infiniband (IB) fabrics.

In addition to the utilities, a sub-library, @file{libibnetdisc}, is provided
to scan an entire IB fabric and return data structures representing it.  The
interface to this library is not guaranteed to be stable.")
    (license (list gpl2 bsd-2)))) ; dual

(define-public ibutils
  (package
    (name "ibutils")
    (version "1.5.7-0.2.gbd7e502")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.openfabrics.org/downloads/ibutils/ibutils-"
                           version ".tar.gz"))
       (sha256
        (base32 "00x7v6cf8l5y6g9xwh1sg738ch42fhv19msx0h0090nhr0bv98v7"))))
    (build-system gnu-build-system)
    (inputs `(("graphviz" ,graphviz)
              ("tcl" ,tcl)
              ("tk" ,tk)
              ("infiniband-diags" ,infiniband-diags)
              ("rdma-core" ,rdma-core)
              ("opensm" ,opensm)
              ("perl" ,perl)))
    (native-inputs `(("swig" ,swig)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-osm="  (assoc-ref %build-inputs "opensm"))
             (string-append "--with-tk-lib=" (assoc-ref %build-inputs "tk") "/lib")
             "--disable-static")))
    (synopsis "InfiniBand network utilities")
    (description "These command-line utilities allow for diagnosing and
testing InfiniBand networks.")
    (home-page "https://www.openfabrics.org/downloads/ibutils/")
    (license bsd-2)))
