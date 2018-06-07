;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
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

(define-module (gnu packages cluster)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls))

(define-public keepalived
  (package
    (name "keepalived")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.keepalived.org/software/keepalived-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0hp8i56zkf0398bmpi32a85f05cv2fy9wizkdfbxk7gav4z6yx18"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-configure
           (lambda _
             ;; XXX: The 'configure' script doesn't handle '-L' flags in the
             ;; output of 'pkg-config'.
             (substitute* "configure"
               (("PKG_CONFIG --libs") "PKG_CONFIG --libs-only-l"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)))
    (inputs
     `(("openssl" ,openssl)
       ("libnfnetlink" ,libnfnetlink)
       ("libnl" ,libnl)))
    (home-page "http://www.keepalived.org/")
    (synopsis "Loadbalancing and high-availability frameworks")
    (description
     "Keepalived provides frameworks for both load balancing and high
availability.  The load balancing framework relies on the Linux Virtual
Server (IPVS) kernel module.  High availability is achieved by the Virtual
Redundancy Routing Protocol (VRRP).  Each Keepalived framework can be used
independently or together to provide resilient infrastructures.")
    (license license:gpl2+)))
