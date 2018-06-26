;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu services authentication)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (fprintd-configuration
            fprintd-configuration?
            fprintd-service-type))

(define-record-type* <fprintd-configuration>
  fprintd-configuration make-fprintd-configuration
  fprintd-configuration?
  (ntp      fprintd-configuration-fprintd
            (default fprintd)))

(define fprintd-service-type
  (service-type (name 'fprintd)
                (extensions
                 (list (service-extension dbus-root-service-type
                                          list)))
                (description
                 "Run fprintd, a fingerprint management daemon.")))
