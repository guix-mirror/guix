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
  #:use-module (gnu packages check)
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

(define-public magic-wormhole-transit-relay
  (package
    (name "magic-wormhole-transit-relay")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "magic-wormhole-transit-relay" version))
        (sha256
         (base32
          "11w5gdc6am2ph5rns60x0694sx4zrlzxj540jljhn5cmnbx1ngxi"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (docs (string-append out "/share/doc/magic-wormhole-transit-relay")))
               (for-each (lambda (file)
                           (install-file file docs))
                         (find-files "docs/"))
               #t))))))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pyflakes" ,python-pyflakes)
       ("python-tox" ,python-tox)))
    (propagated-inputs
     `(("python-twisted" ,python-twisted)))
    (home-page
      "https://github.com/warner/magic-wormhole-transit-relay")
    (synopsis "Magic-Wormhole relay server")
    (description "This package provides the Magic-Wormhole Transit Relay
server, which helps clients establish bulk-data transit connections even when
both are behind NAT boxes.  Each side makes a TCP connection to this server and
presents a handshake.  Two connections with identical handshakes are glued
together, allowing them to pretend they have a direct connection.")
    (license expat)))

(define-public magic-wormhole
  (package
    (name "magic-wormhole")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "magic-wormhole" version))
        (sha256
         (base32
          "01fr4bi6kc6fz9n3c4qq892inrc3nf6p2djy65yvm7xkvdxncydf"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; XXX I can't figure out how to build the docs properly.
         ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34515#101
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1")))
               (install-file "docs/wormhole.1" man))
             #t)))))
    (native-inputs
     `(("python-mock" ,python-mock)
       ;; XXX These are required for the test suite but end up being referenced
       ;; by the built package.
       ;; https://bugs.gnu.org/25235
       ("magic-wormhole-mailbox-server" ,magic-wormhole-mailbox-server)
       ("magic-wormhole-transit-relay" ,magic-wormhole-transit-relay)))
    (propagated-inputs
     `(("python-autobahn" ,python-autobahn)
       ("python-click" ,python-click)
       ("python-hkdf" ,python-hkdf)
       ("python-humanize" ,python-humanize)
       ("python-pynacl" ,python-pynacl)
       ("python-spake2" ,python-spake2)
       ("python-tqdm" ,python-tqdm)
       ("python-twisted" ,python-twisted)
       ("python-txtorcon" ,python-txtorcon)))
    (home-page "https://github.com/warner/magic-wormhole")
    (synopsis "Securely transfer data between computers")
    (description "Magic-Wormhole is a library and a command-line tool named
wormhole, which makes it possible to securely transfer arbitrary-sized files and
directories (or short pieces of text) from one computer to another.  The two
endpoints are identified by using identical \"wormhole codes\": in general, the
sending machine generates and displays the code, which must then be typed into
the receiving machine.

The codes are short and human-pronounceable, using a phonetically-distinct
wordlist.  The receiving side offers tab-completion on the codewords, so usually
only a few characters must be typed.  Wormhole codes are single-use and do not
need to be memorized.")
    (license expat)))
