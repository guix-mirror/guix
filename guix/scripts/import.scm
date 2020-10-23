;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2018 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix scripts import)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:export (%standard-import-options
            guix-import))


;;;
;;; Helper.
;;;

(define (newline-rewriting-port output)
  "Return an output port that rewrites strings containing the \\n escape
to an actual newline.  This works around the behavior of `pretty-print'
and `write', which output these as \\n instead of actual newlines,
whereas we want the `description' field to contain actual newlines
rather than \\n."
  (define (write-string str)
    (let loop ((chars (string->list str)))
      (match chars
        (()
         #t)
        ((#\\ #\n rest ...)
         (newline output)
         (loop rest))
        ((chr rest ...)
         (write-char chr output)
         (loop rest)))))

  (make-soft-port (vector (cut write-char <>)
                          write-string
                          (lambda _ #t)           ; flush
                          #f
                          (lambda _ #t)           ; close
                          #f)
                  "w"))


;;;
;;; Command line options.
;;;

(define %standard-import-options '())


;;;
;;; Entry point.
;;;

(define importers '("gnu" "nix" "pypi" "cpan" "hackage" "stackage" "elpa" "gem"
                    "go" "cran" "crate" "texlive" "json" "opam"))

(define (resolve-importer name)
  (let ((module (resolve-interface
                 `(guix scripts import ,(string->symbol name))))
        (proc (string->symbol (string-append "guix-import-" name))))
    (module-ref module proc)))

(define (show-help)
  (display (G_ "Usage: guix import IMPORTER ARGS ...
Run IMPORTER with ARGS.\n"))
  (newline)
  (display (G_ "IMPORTER must be one of the importers listed below:\n"))
  (newline)
  (format #t "~{   ~a~%~}" importers)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define-command (guix-import . args)
  (category packaging)
  (synopsis "import a package definition from an external repository")

  (match args
    (()
     (format (current-error-port)
             (G_ "guix import: missing importer name~%")))
    ((or ("-h") ("--help"))
     (show-help)
     (exit 0))
    ((or ("-V") ("--version"))
     (show-version-and-exit "guix import"))
    ((importer args ...)
     (if (member importer importers)
         (let ((print (lambda (expr)
                        (pretty-print expr (newline-rewriting-port
                                            (current-output-port))))))
           (match (apply (resolve-importer importer) args)
             ((and expr (or ('package _ ...)
                            ('let _ ...)))
              (print expr))
             ((? list? expressions)
              (for-each (lambda (expr)
                          (print expr)
                          (newline))
                        expressions))
             (x
              (leave (G_ "'~a' import failed~%") importer))))
         (leave (G_ "~a: invalid importer~%") importer)))))
