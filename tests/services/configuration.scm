;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (tests services linux)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

;;; Tests for the (gnu services configuration) module.

(test-begin "services-configuration")


;;;
;;; define-configuration macro.
;;;

(define-configuration port-configuration
  (port (number 80) "The port number.")
  (no-serialization))

(test-equal "default value, no serialization"
  80
  (port-configuration-port (port-configuration)))

(define-configuration port-configuration-cs
  (port (number 80) "The port number." empty-serializer))

(test-equal "default value, custom serializer"
  80
  (port-configuration-cs-port (port-configuration-cs)))

(define serialize-number "")
(define-configuration port-configuration-ndv
  (port (number) "The port number."))

(test-equal "no default value, provided"
  55
  (port-configuration-ndv-port (port-configuration-ndv
                                (port 55))))

(test-assert "no default value, not provided"
  (guard (c ((configuration-error? c)
             #t))
    (port-configuration-ndv-port (port-configuration-ndv))))

(define (custom-number-serializer name value)
  (format #t "~a = ~a;" name value))

(define-configuration serializable-configuration
  (port (number 80) "The port number." custom-number-serializer))

(test-assert "serialize-configuration"
  (gexp?
   (let ((config (serializable-configuration)))
     (serialize-configuration config serializable-configuration-fields))))

(define-configuration serializable-configuration
  (port (number 80) "The port number." custom-number-serializer)
  (no-serialization))

(test-assert "serialize-configuration with no-serialization"
  ;; When serialization is disabled, the serializer is set to #f, so
  ;; attempting to use it fails with a 'wrong-type-arg' error.
  (not (false-if-exception
        (let ((config (serializable-configuration)))
          (serialize-configuration config serializable-configuration-fields)))))
