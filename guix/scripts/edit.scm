;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2020, 2021 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (guix scripts edit)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module ((guix scripts build) #:select (%standard-build-options))
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:export (%editor
            guix-edit))

(define %options
  (list (find (lambda (option)
                (member "load-path" (option-names option)))
              %standard-build-options)
        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix edit")))))

(define (show-help)
  (display (G_ "Usage: guix edit PACKAGE...
Start $VISUAL or $EDITOR to edit the definitions of PACKAGE...\n"))
  (newline)
  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %editor
  ;; Nano is sensible default, as it is installed by base system.
  ;; For development, user can set custom value for $EDITOR.
  (make-parameter (or (getenv "VISUAL") (getenv "EDITOR") "nano")))

(define (search-path* path file)
  "Like 'search-path' but exit if FILE is not found."
  (let ((absolute-file-name (search-path path file)))
    (unless absolute-file-name
      ;; Shouldn't happen unless somebody fiddled with the 'location' field.
      (leave (G_ "file '~a' not found in search path ~s~%")
             file path))
    absolute-file-name))

(define (location->location-specification location)
  "Return the location specification for LOCATION for a typical editor command
line."
  (list (string-append "+"
                       (number->string
                        (location-line location)))
        (search-path* %load-path (location-file location))))


(define-command (guix-edit . args)
  (category packaging)
  (synopsis "view and edit package definitions")

  (define (parse-arguments)
    ;; Return the list of package names.
    (parse-command-line args %options (list (list))
                #:build-options? #f
                #:argument-handler cons))

  (with-error-handling
    (let* ((specs     (reverse (parse-arguments)))
           (locations (map specification->location specs)))

      (catch 'system-error
        (lambda ()
          (let ((file-names (append-map location->location-specification
                                        locations)))
            ;; Use `system' instead of `exec' in order to sanely handle
            ;; possible command line arguments in %EDITOR.
            (exit (system (string-join (cons (%editor) file-names))))))
        (lambda args
          (let ((errno (system-error-errno args)))
            (leave (G_ "failed to launch '~a': ~a~%")
                   (%editor) (strerror errno))))))))
