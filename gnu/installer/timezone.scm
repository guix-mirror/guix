;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer timezone)
  #:use-module (gnu installer utils)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:export (locate-children
            timezone->posix-tz
            timezone-has-child?
            zonetab->timezone-tree
            posix-tz->configuration))

(define %not-blank
  (char-set-complement char-set:blank))

(define (posix-tz->timezone tz)
  "Convert given TZ in Posix format like \"Europe/Paris\" into a list like
(\"Europe\" \"Paris\")."
  (string-split tz #\/))

(define (timezone->posix-tz timezone)
  "Convert given TIMEZONE like (\"Europe\" \"Paris\") into a Posix timezone
like \"Europe/Paris\"."
  (string-join timezone "/"))

(define (zonetab->timezones zonetab)
  "Parse ZONETAB file and return the corresponding list of timezones."

  (define (zonetab-line->posix-tz line)
    (let ((tokens (string-tokenize line %not-blank)))
      (match tokens
        ((code coordinates tz _ ...)
         tz))))

  (call-with-input-file zonetab
    (lambda (port)
      (let* ((lines (read-lines port))
             ;; Filter comment lines starting with '#' character.
             (tz-lines (filter (lambda (line)
                                 (not (eq? (string-ref line 0)
                                           #\#)))
                               lines)))
        (map (lambda (line)
               (posix-tz->timezone
                (zonetab-line->posix-tz line)))
             tz-lines)))))

(define (timezones->timezone-tree timezones)
  "Convert the list of timezones, TIMEZONES into a tree under the form:

	(\"America\" (\"North_Dakota\" \"New_Salem\" \"Center\"))

representing America/North_Dakota/New_Salem and America/North_Dakota/Center
timezones."

  (define (remove-first lists)
    "Remove the first element of every sublists in the argument LISTS."
    (map (lambda (list)
           (if (null? list) list (cdr list)))
         lists))

  (let loop ((cur-timezones timezones))
    (match cur-timezones
      (() '())
      (((region . rest-region) . rest-timezones)
       (if (null? rest-region)
           (cons (list region) (loop rest-timezones))
           (receive (same-region other-region)
               (partition (lambda (timezone)
                            (string=? (car timezone) region))
                          cur-timezones)
             (acons region
                    (loop (remove-first same-region))
                    (loop other-region))))))))

(define (locate-children tree path)
  "Return the children of the timezone indicated by PATH in the given
TREE. Raise a condition if the PATH could not be found."
  (let ((extract-proc (cut map car <>)))
    (match path
      (() (sort (extract-proc tree) string<?))
      ((region . rest)
       (or (and=> (assoc-ref tree region)
                  (cut locate-children <> rest))
           (raise
            (condition
             (&message
              (message
               (format #f (G_ "Unable to locate path: ~a.") path))))))))))

(define (timezone-has-child? tree timezone)
  "Return #t if the given TIMEZONE any child in TREE and #f otherwise."
  (not (null? (locate-children tree timezone))))

(define* (zonetab->timezone-tree zonetab)
  "Return the timezone tree corresponding to the given ZONETAB file."
  (timezones->timezone-tree (zonetab->timezones zonetab)))


;;;
;;; Configuration formatter.
;;;

(define (posix-tz->configuration timezone)
  "Return the configuration field for TIMEZONE."
  `((timezone ,timezone)))
