;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls))

(define-public keepalived
  (package
    (name "keepalived")
    (version "2.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.keepalived.org/software/keepalived-"
                    version ".tar.gz"))
              (sha256
               (base32
                "021a7c1lq4aqx7dbwhlm5km6w039hapfzp5hf6wb5bfq79s25g38"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-info
           (lambda _
             (invoke "make" "-C" "doc" "texinfo")
             ;; Put images in a subdirectory as recommended by 'texinfo'.
             (install-file "doc/build/texinfo/software_design.png"
                           "doc/build/texinfo/keepalived-figures")
             (substitute* "doc/build/texinfo/keepalived.texi"
               (("@image\\{software_design,")
                "@image{keepalived-figures/software_design,"))
             (invoke "make" "-C" "doc/build/texinfo")))
         (add-after 'install 'install-info
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (infodir (string-append out "/share/info")))
               (install-file "doc/build/texinfo/keepalived.info" infodir)
               (install-file "doc/build/texinfo/software_design.png"
                             (string-append infodir "/keepalived-figures"))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)
       ("texinfo" ,texinfo)))
    (inputs
     `(("openssl" ,openssl)
       ("libnfnetlink" ,libnfnetlink)
       ("libnl" ,libnl)))
    (home-page "http://www.keepalived.org/")
    (synopsis "Load balancing and high-availability frameworks")
    (description
     "Keepalived provides frameworks for both load balancing and high
availability.  The load balancing framework relies on the Linux Virtual
Server (@dfn{IPVS}) kernel module.  High availability is achieved by the Virtual
Redundancy Routing Protocol (@dfn{VRRP}).  Each Keepalived framework can be used
independently or together to provide resilient infrastructures.")
    (license license:gpl2+)))
