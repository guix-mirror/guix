;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (tests networking)
  #:use-module (gnu services networking)
  #:use-module (srfi srfi-64))

;;; Tests for the (gnu services networking) module.

(define ntp-server->string (@@ (gnu services networking) ntp-server->string))

(define %ntp-server-sample
  (ntp-server
   (type 'server)
   (address "some.ntp.server.org")
   (options `(iburst (version 3) (maxpoll 16) prefer))))

(test-begin "networking")

(test-equal "ntp-server->string"
  (ntp-server->string %ntp-server-sample)
  "server some.ntp.server.org iburst version 3 maxpoll 16 prefer")

(test-equal "ntp configuration servers deprecated form"
  (ntp-configuration-servers
   (ntp-configuration
    (servers (list (ntp-server
                    (type 'server)
                    (address "example.pool.ntp.org")
                    (options '()))))))
  (ntp-configuration-servers
   (ntp-configuration
    (servers (list "example.pool.ntp.org")))))

(test-end "networking")
