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
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages linux))

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
