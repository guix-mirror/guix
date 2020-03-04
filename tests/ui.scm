;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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


(define-module (test-ui)
  #:use-module (guix ui)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module ((gnu packages) #:select (specification->package))
  #:use-module (guix tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 regex))

;; Test the (guix ui) module.

(define %paragraph
  "GNU Guile is an implementation of the Scheme programming language, with
support for many SRFIs, packaged for use in a wide variety of environments.
In addition to implementing the R5RS Scheme standard and a large subset of
R6RS, Guile includes a module system, full access to POSIX system calls,
networking support, multiple threads, dynamic linking, a foreign function call
interface, and powerful string processing.")

(define guile-1.8.8
  (manifest-entry
    (name "guile")
    (version "1.8.8")
    (item "/gnu/store/...")
    (output "out")))

(define guile-2.0.9
  (manifest-entry
    (name "guile")
    (version "2.0.9")
    (item "/gnu/store/...")
    (output "out")))


(test-begin "ui")

(test-assert "fill-paragraph"
  (every (lambda (column)
           (every (lambda (width)
                    (every (lambda (line)
                             (<= (string-length line) width))
                           (string-split (fill-paragraph %paragraph
                                                         width column)
                                         #\newline)))
                  '(15 30 35 40 45 50 60 70 80 90 100)))
   '(0 5)))

(test-assert "fill-paragraph, consecutive newlines"
  (every (lambda (width)
           (any (lambda (line)
                  (string-prefix? "When STR" line))
                (string-split
                 (fill-paragraph (procedure-documentation fill-paragraph)
                                 width)
                 #\newline)))
         '(15 20 25 30 40 50 60)))

(test-equal "fill-paragraph, large unbreakable word"
  '("Here is a" "very-very-long-word"
    "and that's" "it.")
  (string-split
   (fill-paragraph "Here is a very-very-long-word and that's it."
                   10)
   #\newline))

(test-equal "fill-paragraph, two spaces after period"
  "First line.  Second line"
  (fill-paragraph "First line.
Second line" 24))

(test-equal "package-description-string vs. Unicode"
  "b•ll•t\n\n"                                ;see <http://bugs.gnu.org/21536>
  (with-fluids ((%default-port-encoding "ISO-8859-1"))
    (package-description-string
     (dummy-package "foo" (description "b•ll•t")))))

(test-equal "package-specification->name+version+output"
  '(("guile" #f "out")
    ("guile" "2.0.9" "out")
    ("guile" #f "debug")
    ("guile" "2.0.9" "debug")
    ("guile-cairo" "1.4.1" "out"))
  (map (lambda (spec)
         (call-with-values
             (lambda ()
               (package-specification->name+version+output spec))
           list))
       '("guile"
         "guile@2.0.9"
         "guile:debug"
         "guile@2.0.9:debug"
         "guile-cairo@1.4.1")))

(test-equal "integer"
  '(1)
  (string->generations "1"))

(test-equal "comma-separated integers"
  '(3 7 1 4 6)
  (string->generations "3,7,1,4,6"))

(test-equal "closed range"
  '(4 5 6 7 8 9 10 11 12)
  (string->generations "4..12"))

(test-equal "closed range, equal endpoints"
  '(3)
  (string->generations "3..3"))

(test-equal "indefinite end range"
  '(>= 7)
  (string->generations "7.."))

(test-equal "indefinite start range"
  '(<= 42)
  (string->generations "..42"))

(test-equal "integer, char"
  #f
  (string->generations "a"))

(test-equal "comma-separated integers, consecutive comma"
  #f
  (string->generations "1,,2"))

(test-equal "comma-separated integers, trailing comma"
  #f
  (string->generations "1,2,"))

(test-equal "comma-separated integers, chars"
  #f
  (string->generations "a,b"))

(test-equal "closed range, start > end"
  #f
  (string->generations "9..2"))

(test-equal "closed range, chars"
  #f
  (string->generations "a..b"))

(test-equal "indefinite end range, char"
  #f
  (string->generations "a.."))

(test-equal "indefinite start range, char"
  #f
  (string->generations "..a"))

(test-equal "duration, 1 day"
  (make-time time-duration 0 (* 3600 24))
  (string->duration "1d"))

(test-equal "duration, 1 week"
  (make-time time-duration 0 (* 3600 24 7))
  (string->duration "1w"))

(test-equal "duration, 1 month"
  (make-time time-duration 0 (* 3600 24 30))
  (string->duration "1m"))

(test-equal "duration, 1 week == 7 days"
  (string->duration "1w")
  (string->duration "7d"))

(test-equal "duration, 1 month == 30 days"
  (string->duration "1m")
  (string->duration "30d"))

(test-equal "duration, 2 hours"
  7200
  (time-second (string->duration "2h")))

(test-equal "duration, 1 second"
  (make-time time-duration 0 1)
  (string->duration "1s"))

(test-equal "duration, integer"
  #f
  (string->duration "1"))

(test-equal "duration, char"
  #f
  (string->duration "d"))

(test-equal "size->number, bytes"
  42
  (size->number "42"))

(test-equal "size->number, MiB"
  (* 42 (expt 2 20))
  (size->number "42MiB"))

(test-equal "size->number, GiB"
  (* 3 (expt 2 30))
  (size->number "3GiB"))

(test-equal "size->number, 1.2GiB"
  (inexact->exact (round (* 1.2 (expt 2 30))))
  (size->number "1.2GiB"))

(test-equal "size->number, 1T"
  (expt 2 40)
  (size->number "1T"))

(test-equal "size->number, 1.M"
  (expt 2 20)
  (size->number "1.M"))

(test-assert "size->number, invalid unit"
  (catch 'quit
    (lambda ()
      (size->number "9X"))
    (lambda args
      #t)))

(test-equal "show-what-to-build, zero outputs"
  ""
  (with-store store
    (let ((drv (derivation store "zero" "/bin/sh" '()
                           #:outputs '())))
      (with-error-to-string
       (lambda ()
         ;; This should print nothing.
         (show-what-to-build store (list drv)))))))

(test-assert "show-manifest-transaction"
  (let* ((m (manifest (list guile-1.8.8)))
         (t (manifest-transaction (install (list guile-2.0.9)))))
    (with-store store
      (and (string-match "guile\t1.8.8 → 2.0.9"
                         (with-fluids ((%default-port-encoding "UTF-8"))
                           (with-error-to-string
                            (lambda ()
                              (show-manifest-transaction store m t)))))
           (string-match "guile\t1.8.8 -> 2.0.9"
                         (with-error-to-string
                           (lambda ()
                             ;; In Guile 2.2, %DEFAULT-PORT-ENCODING doesn't
                             ;; influence the encoding of string ports.
                             (set-port-encoding! (current-error-port)
                                                 "ISO-8859-1")
                             (show-manifest-transaction store m t))))))))

(test-assert "package-relevance"
  (let ((guile  (specification->package "guile"))
        (gcrypt (specification->package "guile-gcrypt"))
        (go     (specification->package "go"))
        (gnugo  (specification->package "gnugo"))
        (libb2  (specification->package "libb2"))
        (rx     (cut make-regexp <> regexp/icase))
        (>0     (cut > <> 0))
        (=0     zero?))
    (and (>0 (package-relevance guile
                                (map rx '("scheme"))))
         (>0 (package-relevance guile
                                (map rx '("scheme" "implementation"))))
         (>0 (package-relevance gcrypt
                                (map rx '("guile" "crypto"))))
         (=0 (package-relevance guile
                                (map rx '("guile" "crypto"))))
         (>0 (package-relevance go
                                (map rx '("go"))))
         (=0 (package-relevance go
                                (map rx '("go" "game"))))
         (>0 (package-relevance gnugo
                                (map rx '("go" "game"))))
         (>0 (package-relevance libb2
                                (map rx '("crypto" "library")))))))

(test-end "ui")
