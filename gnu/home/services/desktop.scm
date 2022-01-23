;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu home services desktop)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:autoload   (gnu packages xdisorg) (redshift)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (home-redshift-configuration
            home-redshift-configuration?

            home-redshift-service-type))


;;;
;;; Redshift.
;;;

(define (serialize-integer field value)
  (string-append (match field
                   ('daytime-temperature "temp-day")
                   ('nighttime-temperature "temp-night")
                   ('daytime-brightness "brightness-day")
                   ('nighttime-brightness "brightness-night")
                   ('latitude "lat")
                   ('longitude "lon")
                   (_ (symbol->string field)))
                 "=" (number->string value) "\n"))

(define (serialize-symbol field value)
  (string-append (symbol->string field)
                 "=" (symbol->string value) "\n"))

(define (serialize-string field value)
  (string-append (symbol->string field)
                 "=" value "\n"))

(define serialize-inexact-number serialize-integer)

(define (inexact-number? n)
  (and (number? n) (inexact? n)))
(define-maybe inexact-number)
(define-maybe string)

(define (serialize-raw-configuration-string field value)
  value)
(define raw-configuration-string? string?)

(define-configuration home-redshift-configuration
  (redshift
   (file-like redshift)
   "Redshift package to use.")

  (location-provider
   (symbol 'geoclue2)
   "Geolocation provider---@code{'manual} or @code{'geoclue2}.

In the former case, you must also specify the @code{latitude} and
@code{longitude} fields so Redshift can determine daytime at your place.  In
the latter case, the Geoclue system service must be running; it will be
queried for location information.")
  (adjustment-method
   (symbol 'randr)
   "Color adjustment method.")

  ;; Default values from redshift(1).
  (daytime-temperature
   (integer 6500)
   "Daytime color temperature (kelvins).")
  (nighttime-temperature
   (integer 4500)
   "Nighttime color temperature (kelvins).")

  (daytime-brightness
   (maybe-inexact-number 'disabled)
   "Daytime screen brightness, between 0.1 and 1.0.")
  (nighttime-brightness
   (maybe-inexact-number 'disabled)
   "Nighttime screen brightness, between 0.1 and 1.0.")

  (latitude
   (maybe-inexact-number 'disabled)
   "Latitude, when @code{location-provider} is @code{'manual}.")
  (longitude
   (maybe-inexact-number 'disabled)
   "Longitude, when @code{location-provider} is @code{'manual}.")

  (dawn-time
   (maybe-string 'disabled)
   "Custom time for the transition from night to day in the
morning---@code{\"HH:MM\"} format.  When specified, solar elevation is not
used to determine the daytime/nighttime period.")
  (dusk-time
   (maybe-string 'disabled)
   "Likewise, custom time for the transition from day to night in the
evening.")

  (extra-content
   (raw-configuration-string "")
   "Extra content appended as-is to the Redshift configuration file.  Run
@command{man redshift} for more information about the configuration file
format."))

(define (serialize-redshift-configuration config)
  (define location-fields
    '(latitude longitude))

  (define (location-field? field)
    (memq (configuration-field-name field) location-fields))

  (define (secondary-field? field)
    (or (location-field? field)
        (memq (configuration-field-name field)
              '(redshift extra-content))))

  #~(string-append
     "[redshift]\n"
     #$(serialize-configuration config
                                (remove secondary-field?
                                        home-redshift-configuration-fields))

     #$(home-redshift-configuration-extra-content config)

     "\n[manual]\n"
     #$(serialize-configuration config
                                (filter location-field?
                                        home-redshift-configuration-fields))))

(define (redshift-shepherd-service config)
  (define config-file
    (computed-file "redshift.conf"
                   #~(call-with-output-file #$output
                       (lambda (port)
                         (display #$(serialize-redshift-configuration config)
                                  port)))))

  (list (shepherd-service
         (documentation "Redshift program.")
         (provision '(redshift))
         ;; FIXME: This fails to start if Home is first activated from a
         ;; non-X11 session.
         (start #~(make-forkexec-constructor
                   (list #$(file-append redshift "/bin/redshift")
                         "-c" #$config-file)))
         (stop #~(make-kill-destructor)))))

(define home-redshift-service-type
  (service-type
   (name 'home-redshift)
   (extensions (list (service-extension home-shepherd-service-type
                                        redshift-shepherd-service)))
   (default-value (home-redshift-configuration))
   (description
    "Run Redshift, a program that adjusts the color temperature of display
according to time of day.")))
