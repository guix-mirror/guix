;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
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

(define* (service-type->recutils type port
                                 #:optional (width (%text-width))
                                 #:key (extra-fields '()))
  "Write to PORT a recutils record of TYPE, arranging to fit within WIDTH
columns."
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
          (or (and=> (service-type-location type) location->string)
              (G_ "unknown")))

  (format port "extends: ~a~%"
          (extensions->recutils (service-type-extensions type)))

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
  "Return two values: the list of service types whose name or description
matches at least one of REGEXPS sorted by relevance, and the list of relevance
scores."
  (let ((matches (fold-service-types
                  (lambda (type result)
                    (match (relevance type regexps
                                      %service-type-metrics)
                      ((? zero?)
                       result)
                      (score
                       (cons (list type score) result))))
                  '())))
    (unzip2 (sort matches
                  (lambda (m1 m2)
                    (match m1
                      ((type1 score1)
                       (match m2
                         ((type2 score2)
                          (if (= score1 score2)
                              (string>? (service-type-name* type1)
                                        (service-type-name* type2))
                              (> score1 score2)))))))))))


(define (guix-system-search . args)
  (with-error-handling
    (let ((regexps (map (cut make-regexp* <> regexp/icase) args)))
      (leave-on-EPIPE
       (let-values (((services scores)
                     (find-service-types regexps)))
         (for-each (lambda (service score)
                     (service-type->recutils service
                                             (current-output-port)
                                             #:extra-fields
                                             `((relevance . ,score))))
                   services
                   scores))))))
