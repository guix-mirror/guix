;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 David Thompson <davet@gnu.org>
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.lonestar.org>
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

(define-module (guix scripts deploy)
  #:use-module (gnu machine)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:export (guix-deploy))

;;; Commentary:
;;;
;;; This program provides a command-line interface to (gnu machine), allowing
;;; users to perform remote deployments through specification files.
;;;
;;; Code:



(define (show-help)
  (display (G_ "Usage: guix deploy [OPTION] FILE...
Perform the deployment specified by FILE.\n"))
  (show-build-options-help)
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         %standard-build-options))

(define %default-options
  '((system . ,(%current-system))
    (substitutes? . #t)
    (build-hook? . #t)
    (graft? . #t)
    (debug . 0)
    (verbosity . 1)))

(define (load-source-file file)
  "Load FILE as a user module."
  (let ((module (make-user-module '((gnu) (gnu machine) (gnu machine ssh)))))
    (load* file module)))

(define (guix-deploy . args)
  (define (handle-argument arg result)
    (alist-cons 'file arg result))
  (let* ((opts (parse-command-line args %options (list %default-options)
                                   #:argument-handler handle-argument))
         (file (assq-ref opts 'file))
         (machines (or (and file (load-source-file file)) '())))
    (with-store store
      (set-build-options-from-command-line store opts)
      (for-each (lambda (machine)
                  (info (G_ "deploying to ~a...") (machine-display-name machine))
                  (run-with-store store (deploy-machine machine)))
                machines))))
