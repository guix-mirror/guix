;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer locale)
  #:use-module (gnu installer utils)
  #:use-module (guix records)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (locale-language
            locale-territory
            locale-codeset
            locale-modifier

            locale->locale-string
            supported-locales->locales

            iso639->iso639-languages
            language-code->language-name

            iso3166->iso3166-territories
            territory-code->territory-name

            locale->configuration))


;;;
;;; Locale.
;;;

;; A glibc locale string has the following format:
;; language[_territory[.codeset][@modifier]].
(define locale-regexp "^([^_@]+)(_([^\\.@]+))?(\\.([^@]+))?(@([^$]+))?$")

;; LOCALE will be better expressed in a (guix record) that in an association
;; list. However, loading large files containing records does not scale
;; well. The same thing goes for ISO639 and ISO3166 association lists used
;; later in this module.
(define (locale-language assoc)
  (assoc-ref assoc 'language))
(define (locale-territory assoc)
  (assoc-ref assoc 'territory))
(define (locale-codeset assoc)
  (assoc-ref assoc 'codeset))
(define (locale-modifier assoc)
  (assoc-ref assoc 'modifier))

(define (locale-string->locale string)
  "Return the locale association list built from the parsing of STRING."
  (let ((matches (string-match locale-regexp string)))
    `((language  . ,(match:substring matches 1))
      (territory . ,(match:substring matches 3))
      (codeset   . ,(match:substring matches 5))
      (modifier  . ,(match:substring matches 7)))))

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

(define (locale->locale-string locale)
  "Reverse operation of locale-string->locale."
  (let ((language (locale-language locale))
        (territory (locale-territory locale))
        (codeset (locale-codeset locale))
        (modifier (locale-modifier locale)))
    (apply string-append
           `(,language
             ,@(if territory
                   `("_" ,territory)
                   '())
             ,@(if codeset
                   `("." ,(normalize-codeset codeset))
                   '())
             ,@(if modifier
                   `("@" ,modifier)
                   '())))))

(define (supported-locales->locales supported-locales)
  "Parse the SUPPORTED-LOCALES file from the glibc and return the matching
list of LOCALE association lists."
 (call-with-input-file supported-locales
    (lambda (port)
      (let ((lines (read-lines port)))
        (map (lambda (line)
               (match (string-split line #\ )
                 ((locale-string codeset)
                  (let ((line-locale (locale-string->locale locale-string)))
                    (assoc-set! line-locale 'codeset codeset)))))
             lines)))))


;;;
;;; Language.
;;;

(define (iso639-language-alpha2 assoc)
  (assoc-ref assoc 'alpha2))

(define (iso639-language-alpha3 assoc)
  (assoc-ref assoc 'alpha3))

(define (iso639-language-name assoc)
  (assoc-ref assoc 'name))

(define (supported-locale? locales alpha2 alpha3)
  "Find a locale in LOCALES whose alpha2 field matches ALPHA-2 or alpha3 field
matches ALPHA-3. The ISO639 standard specifies that ALPHA-2 is optional. Thus,
if ALPHA-2 is #f, only consider ALPHA-3. Return #f if not matching locale was
found."
  (find (lambda (locale)
          (let ((language (locale-language locale)))
            (or (and=> alpha2
                       (lambda (code)
                         (string=? language code)))
                (string=? language alpha3))))
        locales))

(define (iso639->iso639-languages locales iso639-3 iso639-5)
  "Return a list of ISO639 association lists created from the parsing of
ISO639-3 and ISO639-5 files."
  (call-with-input-file iso639-3
    (lambda (port-iso639-3)
      (call-with-input-file iso639-5
        (lambda (port-iso639-5)
          (filter-map
           (lambda (hash)
             (let ((alpha2 (hash-ref hash "alpha_2"))
                   (alpha3 (hash-ref hash "alpha_3"))
                   (name   (hash-ref hash "name")))
               (and (supported-locale? locales alpha2 alpha3)
                    `((alpha2 . ,alpha2)
                      (alpha3 . ,alpha3)
                      (name   . ,name)))))
           (append
            (hash-ref (json->scm port-iso639-3) "639-3")
            (hash-ref (json->scm port-iso639-5) "639-5"))))))))

(define (language-code->language-name languages language-code)
  "Using LANGUAGES as a list of ISO639 association lists, return the language
name corresponding to the given LANGUAGE-CODE."
  (let ((iso639-language
         (find (lambda (language)
                 (or
                  (and=> (iso639-language-alpha2 language)
                         (lambda (alpha2)
                           (string=? alpha2 language-code)))
                  (string=? (iso639-language-alpha3 language)
                            language-code)))
               languages)))
    (iso639-language-name iso639-language)))


;;;
;;; Territory.
;;;

(define (iso3166-territory-alpha2 assoc)
  (assoc-ref assoc 'alpha2))

(define (iso3166-territory-alpha3 assoc)
  (assoc-ref assoc 'alpha3))

(define (iso3166-territory-name assoc)
  (assoc-ref assoc 'name))

(define (iso3166->iso3166-territories iso3166)
  "Return a list of ISO3166 association lists created from the parsing of
ISO3166 file."
  (call-with-input-file iso3166
    (lambda (port)
      (map (lambda (hash)
             `((alpha2 . ,(hash-ref hash "alpha_2"))
               (alpha3 . ,(hash-ref hash "alpha_3"))
               (name   . ,(hash-ref hash "name"))))
           (hash-ref (json->scm port) "3166-1")))))

(define (territory-code->territory-name territories territory-code)
  "Using TERRITORIES as a list of ISO3166 association lists return the
territory name corresponding to the given TERRITORY-CODE."
  (let ((iso3166-territory
         (find (lambda (territory)
                 (or
                  (and=> (iso3166-territory-alpha2 territory)
                         (lambda (alpha2)
                           (string=? alpha2 territory-code)))
                  (string=? (iso3166-territory-alpha3 territory)
                            territory-code)))
               territories)))
    (iso3166-territory-name iso3166-territory)))


;;;
;;; Configuration formatter.
;;;

(define (locale->configuration locale)
  "Return the configuration field for LOCALE."
  `((locale ,locale)))
