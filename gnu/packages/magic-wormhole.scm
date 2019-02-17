;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Leo Famulari <leo@famulari.name>
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

(define-module (gnu packages magic-wormhole)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

(define-public magic-wormhole-mailbox-server
  (package
    (name "magic-wormhole-mailbox-server")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "magic-wormhole-mailbox-server" version))
        (sha256
         (base32
          "1q6zhbx8fcpk7rchclm7yqcxdsc1x97hki2ji61sa544r5xvxv55"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
          (lambda _
            ;; This test requires network access.
            (substitute* "src/wormhole_mailbox_server/test/test_web.py"
              (("test_log_http") "disabled_test_log_http"))
            #t)))))
    (native-inputs
     `(("python-mock" ,python-mock)))
    (propagated-inputs
     `(("python-attrs" ,python-attrs)
       ("python-autobahn" ,python-autobahn)
       ("python-idna" ,python-idna)
       ("python-service-identity" ,python-service-identity)
       ("python-six" ,python-six)
       ("python-treq" ,python-treq)
       ("python-twisted" ,python-twisted)))
    (home-page "https://github.com/warner/magic-wormhole-mailbox-server")
    (synopsis "Magic-Wormhole central mailbox server")
    (description "This package provides the main server that Magic-Wormhole
clients connect to.  The server performs store-and-forward delivery for small
key-exchange and control messages.  Bulk data is sent over a direct TCP
connection, or through a transit-relay.")
   (license expat)))
