;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system mapped-devices)
  #:use-module (guix records)
  #:export (mapped-device
            mapped-device?
            mapped-device-source
            mapped-device-target
            mapped-device-type

            mapped-device-kind
            mapped-device-kind?
            mapped-device-kind-open
            mapped-device-kind-close))

;;; Commentary:
;;;
;;; This module supports "device mapping", a concept implemented by Linux's
;;; device-mapper.
;;;
;;; Code:

(define-record-type* <mapped-device> mapped-device
  make-mapped-device
  mapped-device?
  (source    mapped-device-source)                ;string
  (target    mapped-device-target)                ;string
  (type      mapped-device-type))                 ;<mapped-device-kind>

(define-record-type* <mapped-device-type> mapped-device-kind
  make-mapped-device-kind
  mapped-device-kind?
  (open      mapped-device-kind-open)             ;source target -> gexp
  (close     mapped-device-kind-close             ;source target -> gexp
             (default (const #~(const #f)))))

;;; mapped-devices.scm ends here
