;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages finance)
 #:use-module ((guix licenses) #:prefix license:)
 #:use-module (guix packages)
 #:use-module (guix download)
 #:use-module (guix build utils)
 #:use-module (guix build-system gnu)
 #:use-module (gnu packages boost)
 #:use-module (gnu packages databases)
 #:use-module (gnu packages linux)
 #:use-module (gnu packages pkg-config)
 #:use-module (gnu packages protobuf)
 #:use-module (gnu packages python)
 #:use-module (gnu packages qt)
 #:use-module (gnu packages tls)
 #:use-module (gnu packages upnp))

(define-public bitcoin-core
  (package
    (name "bitcoin-core")
    (version "0.11.2")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "https://bitcoin.org/bin/bitcoin-core-"
                             version "/bitcoin-"
                             version ".tar.gz"))
             (sha256
              (base32
               "1lwh0vhw1gf3h6zrhynvad9y9qbpmhc8cw1zvj11yzsz5rjbvlm4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-2) ; for the tests
       ("util-linux" ,util-linux))) ; provides the hexdump command for tests
    (inputs
     `(("bdb" ,bdb)
       ("boost" ,boost)
       ("miniupnpc" ,miniupnpc)
       ("openssl" ,openssl)
       ("protobuf" ,protobuf)
       ("qt" ,qt)))
    (arguments
     `(#:configure-flags
        (list
          ;; We use a bdb version newer than 4.8.
          "--with-incompatible-bdb"
          ;; Boost is not found unless specified manually.
          (string-append "--with-boost="
                         (assoc-ref %build-inputs "boost")))
       #:phases
        (modify-phases %standard-phases
          (add-before 'check 'set-home
           (lambda _
            (setenv "HOME" (getenv "TMPDIR"))))))) ; Tests write to $HOME.
    (home-page "https://bitcoin.org/en/")
    (synopsis "Bitcoin peer-to-peer client")
    (description
     "Bitcoin is a digital currency that enables instant payments to anyone
anywhere in the world.  It uses peer-to-peer technology to operate without
central authority: managing transactions and issuing money are carried out
collectively by the network.  Bitcoin Core is the reference implementation
of the bitcoin protocol.  This package provides the Bitcoin Core command
line client and a client based on Qt.")
    (license license:expat)))
