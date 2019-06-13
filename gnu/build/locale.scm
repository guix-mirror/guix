;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu build locale)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (build-locale
            normalize-codeset
            locale->name+codeset
            read-supported-locales))

(define locale-rx
  ;; Regexp matching a locale line in 'localedata/SUPPORTED'.
  (make-regexp
   "^[[:space:]]*([[:graph:]]+)/([[:graph:]]+)[[:space:]]*\\\\$"))

(define (read-supported-locales port)
  "Read the 'localedata/SUPPORTED' file from PORT.  That file is actually a
makefile snippet, with one locale per line, and a header that can be
discarded."
  (let loop ((locales '()))
    (define line
      (read-line port))

    (cond ((eof-object? line)
           (reverse locales))
          ((string-prefix? "#" (string-trim line)) ;comment
           (loop locales))
          ((string-contains line "=")            ;makefile variable assignment
           (loop locales))
          (else
           (match (regexp-exec locale-rx line)
             (#f
              (loop locales))
             (m
              (loop (alist-cons (match:substring m 1)
                                (match:substring m 2)
                                locales))))))))

(define (normalize-codeset codeset)
  "Compute the \"normalized\" variant of CODESET."
  ;; info "(libc) Using gettextized software", for the algorithm used to
  ;; compute the normalized codeset.
  (letrec-syntax ((-> (syntax-rules ()
                        ((_ proc value)
                         (proc value))
                        ((_ proc rest ...)
                         (proc (-> rest ...))))))
    (-> (lambda (str)
          (if (string-every char-set:digit str)
              (string-append "iso" str)
              str))
        string-downcase
        (lambda (str)
          (string-filter char-set:letter+digit str))
        codeset)))

(define* (build-locale locale
                       #:key
                       (localedef "localedef")
                       (directory ".")
                       (codeset "UTF-8")
                       (name (string-append locale "." codeset)))
  "Compute locale data for LOCALE and CODESET--e.g., \"en_US\" and
\"UTF-8\"--with LOCALEDEF, and store it in DIRECTORY under NAME."
  (format #t "building locale '~a'...~%" name)
  (invoke localedef "--no-archive" "--prefix" directory
          "-i" locale "-f" codeset
          (string-append directory "/" name)))

(define (locale->name+codeset locale)
  "Split a locale name such as \"aa_ER@saaho.UTF-8\" into two values: the
language/territory/modifier part, and the codeset."
  (match (string-rindex locale #\.)
    (#f  (values locale #f))
    (dot (values (string-take locale dot)
                 (string-drop locale (+ dot 1))))))
