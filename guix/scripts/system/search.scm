;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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

(define-module (guix scripts system search)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:export (service-type->recutils
            find-service-types
            guix-system-search))

;;; Commentary:
;;;
;;; Implement the 'guix system search' command, which searches among the
;;; available service types.
;;;
;;; Code:

(define service-type-name*
  (compose symbol->string service-type-name))

(define (service-type-default-shepherd-services type)
  "Return the list of Shepherd services created by default instances of TYPE,
provided TYPE has a default value."
  (match (guard (c ((service-error? c) #f))
           (service type))
    (#f '())
    ((? service? service)
     (let* ((extension (find (lambda (extension)
                               (eq? (service-extension-target extension)
                                    shepherd-root-service-type))
                             (service-type-extensions type)))
            (compute   (and extension (service-extension-compute extension))))
       (if compute
           (compute (service-value service))
           '())))))

(define (service-type-shepherd-names type)
  "Return the default names of Shepherd services created for TYPE."
  (append-map shepherd-service-provision
              (service-type-default-shepherd-services type)))

(define* (service-type->recutils type port
                                 #:optional (width (%text-width))
                                 #:key
                                 (extra-fields '())
                                 (hyperlinks? (supports-hyperlinks? port)))
  "Write to PORT a recutils record of TYPE, arranging to fit within WIDTH
columns.  When HYPERLINKS? is true, emit hyperlink escape sequences when
appropriate."
  (define width*
    ;; The available number of columns once we've taken into account space for
    ;; the initial "+ " prefix.
    (if (> width 2) (- width 2) width))

  (define (extensions->recutils extensions)
    (let ((list (string-join (map (compose service-type-name*
                                           service-extension-target)
                                  extensions))))
      (string->recutils
       (fill-paragraph list width*
                       (string-length "extends: ")))))

  ;; Note: Don't i18n field names so that people can post-process it.
  (format port "name: ~a~%" (service-type-name type))
  (format port "location: ~a~%"
          (or (and=> (service-type-location type)
                     (if hyperlinks? location->hyperlink location->string))
              (G_ "unknown")))

  (format port "extends: ~a~%"
          (extensions->recutils (service-type-extensions type)))

  ;; If possible, display the list of *default* Shepherd service names.  Note
  ;; that we may not always be able to do this (e.g., if the service type
  ;; lacks a default value); furthermore, it could be that the service
  ;; generates Shepherd services with different names if we give it different
  ;; parameters (this is the case, for instance, for
  ;; 'console-font-service-type'.)
  (match (service-type-shepherd-names type)
    (()    #f)
    (names (format port "shepherdnames:~{ ~a~}~%" names)))

  (when (service-type-description type)
    (format port "~a~%"
            (string->recutils
             (string-trim-right
              (parameterize ((%text-width width*))
                (texi->plain-text
                 (string-append "description: "
                                (or (and=> (service-type-description type) P_)
                                    ""))))
              #\newline))))

  (for-each (match-lambda
              ((field . value)
               (let ((field (symbol->string field)))
                 (format port "~a: ~a~%"
                         field
                         (fill-paragraph (object->string value) width*
                                         (string-length field))))))
            extra-fields)
  (newline port))

(define (service-type-description-string type)
  "Return the rendered and localised description of TYPE, a service type."
  (and=> (service-type-description type)
         (compose texi->plain-text P_)))

(define %service-type-metrics
  ;; Metrics used to estimate the relevance of a search result.
  `((,service-type-name* . 3)
    (,service-type-description-string . 2)
    (,(lambda (type)
        (match (and=> (service-type-location type) location-file)
          ((? string? file)
           (basename file ".scm"))
          (#f
           "")))
     . 1)))

(define (find-service-types regexps)
  "Return a list of service type/score pairs: service types whose name or
description matches REGEXPS sorted by relevance, and their score."
  (let ((matches (fold-service-types
                  (lambda (type result)
                    (match (relevance type regexps
                                      %service-type-metrics)
                      ((? zero?)
                       result)
                      (score
                       (cons (cons type score) result))))
                  '())))
    (sort matches
          (lambda (m1 m2)
            (match m1
              ((type1 . score1)
               (match m2
                 ((type2 . score2)
                  (if (= score1 score2)
                      (string>? (service-type-name* type1)
                                (service-type-name* type2))
                      (> score1 score2))))))))))


(define (guix-system-search . args)
  (with-error-handling
    (let* ((regexps (map (cut make-regexp* <> regexp/icase) args))
           (matches (find-service-types regexps)))
      (leave-on-EPIPE
       (display-search-results matches (current-output-port)
                               #:print service-type->recutils
                               #:command "guix system search")))))
