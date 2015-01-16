;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system locale)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (srfi srfi-26)
  #:export (locale-definition
            locale-definition?
            locale-definition-name
            locale-definition-source
            locale-definition-charset

            locale-directory

            %default-locale-definitions))

;;; Commentary:
;;;
;;; Locale definitions, and compilation thereof.
;;;
;;; Code:

(define-record-type* <locale-definition> locale-definition
  make-locale-definition
  locale-definition?
  (name    locale-definition-name)                ;string--e.g., "fr_FR.utf8"
  (source  locale-definition-source)              ;string--e.g., "fr_FR"
  (charset locale-definition-charset              ;string--e.g., "UTF-8"
           (default "UTF-8")))

(define* (localedef-command locale
                            #:key (libc (canonical-package glibc)))
  "Return a gexp that runs 'localedef' from LIBC to build LOCALE."
  #~(begin
      (format #t "building locale '~a'...~%"
              #$(locale-definition-name locale))
      (zero? (system* (string-append #$libc "/bin/localedef")
                      "--no-archive" "--prefix" #$output
                      "-i" #$(locale-definition-source locale)
                      "-f" #$(locale-definition-charset locale)
                      (string-append #$output "/"
                                     #$(locale-definition-name locale))))))

(define* (locale-directory locales
                           #:key (libc (canonical-package glibc)))
  "Return a directory containing all of LOCALES compiled."
  (define build
    #~(begin
        (mkdir #$output)

        ;; 'localedef' executes 'gzip' to access compressed locale sources.
        (setenv "PATH" (string-append #$gzip "/bin"))

        (exit
         (and #$@(map (cut localedef-command <> #:libc libc)
                      locales)))))

  (gexp->derivation "locale" build
                    #:local-build? #t))

(define %default-locale-definitions
  ;; Arbitrary set of locales that are built by default.  They are here mostly
  ;; to facilitate first-time use to some people, while others may have to add
  ;; a specific <locale-definition>.
  (letrec-syntax ((utf8-locale (syntax-rules ()
                                 ((_ name*)
                                  (locale-definition
                                   (name (string-append name* ".utf8"))
                                   (source name*)
                                   (charset "UTF-8")))))
                  (utf8-locales (syntax-rules ()
                                  ((_ name ...)
                                   (list (utf8-locale name) ...)))))
    ;; Add "en_US.UTF-8" for compatibility with Guix 0.8.
    (cons (locale-definition
           (name "en_US.UTF-8")
           (source "en_US")
           (charset "UTF-8"))
          (utf8-locales "ca_ES"
                        "cs_CZ"
                        "da_DK"
                        "de_DE"
                        "el_GR"
                        "en_AU"
                        "en_CA"
                        "en_GB"
                        "en_US"
                        "es_AR"
                        "es_CL"
                        "es_ES"
                        "es_MX"
                        "fi_FI"
                        "fr_BE"
                        "fr_CA"
                        "fr_CH"
                        "fr_FR"
                        "ga_IE"
                        "it_IT"
                        "ja_JP"
                        "ko_KR"
                        "nb_NO"
                        "nl_NL"
                        "pl_PL"
                        "pt_PT"
                        "ro_RO"
                        "ru_RU"
                        "sv_SE"
                        "tr_TR"
                        "uk_UA"
                        "vi_VN"
                        "zh_CN"))))

;;; locale.scm ends here
