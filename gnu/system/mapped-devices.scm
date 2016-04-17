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
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:autoload   (gnu packages cryptsetup) (cryptsetup)
  #:use-module (ice-9 match)
  #:export (mapped-device
            mapped-device?
            mapped-device-source
            mapped-device-target
            mapped-device-type

            mapped-device-kind
            mapped-device-kind?
            mapped-device-kind-open
            mapped-device-kind-close

            device-mapping-service-type
            device-mapping-service

            luks-device-mapping))

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


;;;
;;; Device mapping as a Shepherd service.
;;;

(define device-mapping-service-type
  (shepherd-service-type
   'device-mapping
   (match-lambda
     ((target open close)
      (shepherd-service
       (provision (list (symbol-append 'device-mapping- (string->symbol target))))
       (requirement '(udev))
       (documentation "Map a device node using Linux's device mapper.")
       (start #~(lambda () #$open))
       (stop #~(lambda _ (not #$close)))
       (respawn? #f))))))

(define (device-mapping-service target open close)
  "Return a service that maps device @var{target}, a string such as
@code{\"home\"} (meaning @code{/dev/mapper/home}).  Evaluate @var{open}, a
gexp, to open it, and evaluate @var{close} to close it."
  (service device-mapping-service-type
           (list target open close)))


;;;
;;; Common device mappings.
;;;

(define (open-luks-device source target)
  "Return a gexp that maps SOURCE to TARGET as a LUKS device, using
'cryptsetup'."
  #~(zero? (system* (string-append #$cryptsetup "/sbin/cryptsetup")
                    "open" "--type" "luks"
                    #$source #$target)))

(define (close-luks-device source target)
  "Return a gexp that closes TARGET, a LUKS device."
  #~(zero? (system* (string-append #$cryptsetup "/sbin/cryptsetup")
                    "close" #$target)))

(define luks-device-mapping
  ;; The type of LUKS mapped devices.
  (mapped-device-kind
   (open open-luks-device)
   (close close-luks-device)))

;;; mapped-devices.scm ends here
