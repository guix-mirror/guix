;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (tests offload)
  #:use-module (guix scripts offload)
  #:use-module (srfi srfi-64))


(test-begin "offload")

(define-syntax-rule (expose-internal-definitions s1 s2 ...)
  (begin
    (define s1 (@@ (guix scripts offload) s1))
    (define s2 (@@ (guix scripts offload) s2)) ...))

(expose-internal-definitions machine-matches?
                             build-requirements-system
                             build-requirements-features
                             build-machine-system
                             build-machine-systems
                             %build-machine-system
                             %build-machine-systems
                             build-machine-features)

(define (deprecated-build-machine system)
  (build-machine
   (name "m1")
   (user "dummy")
   (host-key "some-key")
   (system system)))

(define (new-build-machine systems)
  (build-machine
   (name "m1")
   (user "dummy")
   (host-key "some-key")
   (systems systems)))

;;; Test that deprecated build-machine definitions still work.
(test-assert (machine-matches? (deprecated-build-machine "i686-linux")
                               (build-requirements
                                (system "i686-linux"))))


(test-assert (machine-matches? (new-build-machine '("i686-linux"))
                               (build-requirements
                                (system "i686-linux"))))

;;; A build machine can act as more than one system type, thanks to QEMU
;;; emulation.
(test-assert (machine-matches? (new-build-machine '("armhf-linux"
                                                    "aarch64-linux"
                                                    "i686-linux"
                                                    "x86_64-linux"))
                               (build-requirements
                                (system "armhf-linux"))))
