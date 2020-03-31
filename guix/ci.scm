;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix json)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (build?
            build-id
            build-derivation
            build-system
            build-status
            build-timestamp

            checkout?
            checkout-commit
            checkout-input

            evaluation?
            evaluation-id
            evaluation-spec
            evaluation-complete?
            evaluation-checkouts

            %query-limit
            queued-builds
            latest-builds
            latest-evaluations
            evaluations-for-commit))

;;; Commentary:
;;;
;;; This module provides a client to the HTTP interface of the Hydra and
;;; Cuirass continuous integration (CI) tools.
;;;
;;; Code:

(define-json-mapping <build> make-build build?
  json->build
  (id          build-id "id")                     ;integer
  (derivation  build-derivation)                  ;string | #f
  (system      build-system)                      ;string
  (status      build-status "buildstatus" )       ;integer
  (timestamp   build-timestamp))                  ;integer

(define-json-mapping <checkout> make-checkout checkout?
  json->checkout
  (commit      checkout-commit)                   ;string (SHA1)
  (input       checkout-input))                   ;string (name)

(define-json-mapping <evaluation> make-evaluation evaluation?
  json->evaluation
  (id          evaluation-id)                     ;integer
  (spec        evaluation-spec "specification")   ;string
  (complete?   evaluation-complete? "in-progress"
               (match-lambda
                 (0 #t)
                 (_ #f)))                         ;Boolean
  (checkouts   evaluation-checkouts "checkouts"   ;<checkout>*
               (lambda (checkouts)
                 (map json->checkout
                      (vector->list checkouts)))))

(define %query-limit
  ;; Max number of builds requested in queries.
  1000)

(define (json-fetch url)
  (let* ((port (http-fetch url))
         (json (json->scm port)))
    (close-port port)
    json))

(define* (queued-builds url #:optional (limit %query-limit))
  "Return the list of queued derivations on URL."
  (let ((queue (json-fetch (string-append url "/api/queue?nr="
                                          (number->string limit)))))
    (map json->build (vector->list queue))))

(define* (latest-builds url #:optional (limit %query-limit)
                        #:key evaluation system)
  "Return the latest builds performed by the CI server at URL.  If EVALUATION
is an integer, restrict to builds of EVALUATION.  If SYSTEM is true (a system
string such as \"x86_64-linux\"), restrict to builds for SYSTEM."
  (define* (option name value #:optional (->string identity))
    (if value
        (string-append "&" name "=" (->string value))
        ""))

  (let ((latest (json-fetch (string-append url "/api/latestbuilds?nr="
                                           (number->string limit)
                                           (option "evaluation" evaluation
                                                   number->string)
                                           (option "system" system)))))
    ;; Note: Hydra does not provide a "derivation" field for entries in
    ;; 'latestbuilds', but Cuirass does.
    (map json->build (vector->list latest))))

(define* (latest-evaluations url #:optional (limit %query-limit))
  "Return the latest evaluations performed by the CI server at URL."
  (map json->evaluation
       (vector->list
        (json->scm
         (http-fetch (string-append url "/api/evaluations?nr="
                                    (number->string limit)))))))


(define* (evaluations-for-commit url commit #:optional (limit %query-limit))
  "Return the evaluations among the latest LIMIT evaluations that have COMMIT
as one of their inputs."
  (filter (lambda (evaluation)
            (find (lambda (checkout)
                    (string=? (checkout-commit checkout) commit))
                  (evaluation-checkouts evaluation)))
          (latest-evaluations url limit)))
