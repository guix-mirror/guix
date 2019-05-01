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

(define-module (guix scripts search)
  #:use-module (guix ui)
  #:use-module (guix scripts package)
  #:use-module (guix scripts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:export (guix-search))

(define (show-help)
  (display (G_ "Usage: guix search [OPTION] REGEXPS...
Search for packages matching REGEXPS."))
  (display (G_"
This is an alias for 'guix package -s'.\n"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix search")))))

(define (guix-search . args)
  (define (handle-argument arg result)
    ;; Treat all non-option arguments as regexps.
    (cons `(query search ,(or arg ""))
          result))

  (define opts
    (args-fold* args %options
                (lambda (opt name arg . rest)
                  (leave (G_ "~A: unrecognized option~%") name))
                handle-argument
                '()))

  (unless (assoc-ref opts 'query)
    (leave (G_ "missing arguments: no regular expressions to search for~%")))

  (guix-package* opts))
