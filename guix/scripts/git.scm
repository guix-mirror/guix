;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts git)
  #:use-module (ice-9 match)
  #:use-module (guix ui)
  #:export (guix-git))

(define (show-help)
  (display (G_ "Usage: guix git COMMAND ARGS...
Operate on Git repositories.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   authenticate    verify commit signatures and authorizations\n"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %sub-commands '("authenticate"))

(define (resolve-sub-command name)
  (let ((module (resolve-interface
                 `(guix scripts git ,(string->symbol name))))
        (proc (string->symbol (string-append "guix-git-" name))))
    (module-ref module proc)))

(define (guix-git . args)
  (with-error-handling
    (match args
      (()
       (format (current-error-port)
               (G_ "guix git: missing sub-command~%")))
      ((or ("-h") ("--help"))
       (show-help)
       (exit 0))
      ((or ("-V") ("--version"))
       (show-version-and-exit "guix git"))
      ((sub-command args ...)
       (if (member sub-command %sub-commands)
           (apply (resolve-sub-command sub-command) args)
           (format (current-error-port)
                   (G_ "guix git: invalid sub-command~%")))))))
