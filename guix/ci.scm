;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix ci)
  #:use-module (guix http-client)
  #:autoload   (json parser) (json->scm)
  #:use-module (srfi srfi-9)
  #:export (build?
            build-id
            build-derivation
            build-system
            build-status
            build-timestamp

            %query-limit
            queued-builds
            latest-builds))

;;; Commentary:
;;;
;;; This module provides a client to the HTTP interface of the Hydra and
;;; Cuirass continuous integration (CI) tools.
;;;
;;; Code:

(define-record-type <build>
  (make-build id derivation system status timestamp)
  build?
  (id          build-id)                          ;integer
  (derivation  build-derivation)                  ;string | #f
  (system      build-system)                      ;string
  (status      build-status)                      ;integer
  (timestamp   build-timestamp))                  ;integer

(define %query-limit
  ;; Max number of builds requested in queries.
  1000)

(define (json-fetch url)
  (let* ((port (http-fetch url))
         (json (json->scm port)))
    (close-port port)
    json))

(define (json->build json)
  (make-build (hash-ref json "id")
              (hash-ref json "derivation")
              (hash-ref json "system")
              (hash-ref json "buildstatus")
              (hash-ref json "timestamp")))

(define* (queued-builds url #:optional (limit %query-limit))
  "Return the list of queued derivations on URL."
  (let ((queue (json-fetch (string-append url "/api/queue?nr="
                                          (number->string limit)))))
    (map json->build queue)))

(define* (latest-builds url #:optional (limit %query-limit))
  (let ((latest (json-fetch (string-append url "/api/latestbuilds?nr="
                                           (number->string limit)))))
    ;; Note: Hydra does not provide a "derivation" field for entries in
    ;; 'latestbuilds', but Cuirass does.
    (map json->build latest)))
