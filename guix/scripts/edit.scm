;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:export (%editor
            guix-edit))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix edit")))))

(define (show-help)
  (display (_ "Usage: guix edit PACKAGE...
Start $EDITOR to edit the definitions of PACKAGE...\n"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %editor
  (make-parameter (or (getenv "EDITOR") "emacsclient")))

(define (search-path* path file)
  "Like 'search-path' but exit if FILE is not found."
  (let ((absolute-file-name (search-path path file)))
    (unless absolute-file-name
      ;; Shouldn't happen unless somebody fiddled with the 'location' field.
      (leave (_ "file '~a' not found in search path ~s~%")
             file path))
    absolute-file-name))


(define (guix-edit . args)
  (with-error-handling
    (let* ((specs    (parse-command-line args %options '(())
                                         #:argument-handler cons))
           (packages (map specification->package specs)))
      (for-each (lambda (package)
                  (unless (package-location package)
                    (leave (_ "source location of package '~a' is unknown~%")
                           (package-full-name package))))
                packages)
      (apply execlp (%editor) (%editor)
             (append-map (lambda (package)
                           (let ((loc (package-location package)))
                             (list (string-append "+"
                                                  (number->string
                                                   (location-line loc)))
                                   (search-path* %load-path (location-file loc)))))
                         packages)))))
