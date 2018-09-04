;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix inferior)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module ((guix utils) #:select (source-properties->location))
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (inferior?
            open-inferior
            close-inferior
            inferior-eval
            inferior-object?

            inferior-package?
            inferior-package-name
            inferior-package-version

            inferior-packages
            inferior-package-synopsis
            inferior-package-description
            inferior-package-home-page
            inferior-package-location))

;;; Commentary:
;;;
;;; This module provides a way to spawn Guix "inferior" processes and to talk
;;; to them.  It allows us, from one instance of Guix, to interact with
;;; another instance of Guix coming from a different commit.
;;;
;;; Code:

;; Inferior Guix process.
(define-record-type <inferior>
  (inferior pid socket version)
  inferior?
  (pid      inferior-pid)
  (socket   inferior-socket)
  (version  inferior-version))                    ;REPL protocol version

(define (inferior-pipe directory command)
  "Return an input/output pipe on the Guix instance in DIRECTORY.  This runs
'DIRECTORY/COMMAND repl' if it exists, or falls back to some other method if
it's an old Guix."
  (let ((pipe (with-error-to-port (%make-void-port "w")
                (lambda ()
                  (open-pipe* OPEN_BOTH
                              (string-append directory "/" command)
                              "repl" "-t" "machine")))))
    (if (eof-object? (peek-char pipe))
        (begin
          (close-pipe pipe)

          ;; Older versions of Guix didn't have a 'guix repl' command, so
          ;; emulate it.
          (open-pipe* OPEN_BOTH "guile"
                      "-L" (string-append directory "/share/guile/site/"
                                          (effective-version))
                      "-C" (string-append directory "/share/guile/site/"
                                          (effective-version))
                      "-C" (string-append directory "/lib/guile/"
                                          (effective-version) "/site-ccache")
                      "-c"
                      (object->string
                       `(begin
                          (primitive-load ,(search-path %load-path
                                                        "guix/scripts/repl.scm"))
                          ((@ (guix scripts repl) machine-repl))))))
        pipe)))

(define* (open-inferior directory #:key (command "bin/guix"))
  "Open the inferior Guix in DIRECTORY, running 'DIRECTORY/COMMAND repl' or
equivalent.  Return #f if the inferior could not be launched."
  (define pipe
    (inferior-pipe directory command))

  (cond-expand
    ((and guile-2 (not guile-2.2)) #t)
    (else (setvbuf pipe 'line)))

  (match (read pipe)
    (('repl-version 0 rest ...)
     (let ((result (inferior 'pipe pipe (cons 0 rest))))
       (inferior-eval '(use-modules (guix)) result)
       (inferior-eval '(use-modules (gnu)) result)
       (inferior-eval '(define %package-table (make-hash-table))
                      result)
       result))
    (_
     #f)))

(define (close-inferior inferior)
  "Close INFERIOR."
  (close-pipe (inferior-socket inferior)))

;; Non-self-quoting object of the inferior.
(define-record-type <inferior-object>
  (inferior-object address appearance)
  inferior-object?
  (address     inferior-object-address)
  (appearance  inferior-object-appearance))

(define (write-inferior-object object port)
  (match object
    (($ <inferior-object> _ appearance)
     (format port "#<inferior-object ~a>" appearance))))

(set-record-type-printer! <inferior-object> write-inferior-object)

(define (inferior-eval exp inferior)
  "Evaluate EXP in INFERIOR."
  (define sexp->object
    (match-lambda
      (('value value)
       value)
      (('non-self-quoting address string)
       (inferior-object address string))))

  (write exp (inferior-socket inferior))
  (newline (inferior-socket inferior))
  (match (read (inferior-socket inferior))
    (('values objects ...)
     (apply values (map sexp->object objects)))
    (('exception key objects ...)
     (apply throw key (map sexp->object objects)))))


;;;
;;; Inferior packages.
;;;

(define-record-type <inferior-package>
  (inferior-package inferior name version id)
  inferior-package?
  (inferior   inferior-package-inferior)
  (name       inferior-package-name)
  (version    inferior-package-version)
  (id         inferior-package-id))

(define (write-inferior-package package port)
  (match package
    (($ <inferior-package> _ name version)
     (format port "#<inferior-package ~a@~a ~a>"
             name version
             (number->string (object-address package) 16)))))

(set-record-type-printer! <inferior-package> write-inferior-package)

(define (inferior-packages inferior)
  "Return the list of packages known to INFERIOR."
  (let ((result (inferior-eval
                 '(fold-packages (lambda (package result)
                                   (let ((id (object-address package)))
                                     (hashv-set! %package-table id package)
                                     (cons (list (package-name package)
                                                 (package-version package)
                                                 id)
                                           result)))
                                 '())
                 inferior)))
    (map (match-lambda
           ((name version id)
            (inferior-package inferior name version id)))
         result)))

(define (inferior-package-field package getter)
  "Return the field of PACKAGE, an inferior package, accessed with GETTER."
  (let ((inferior (inferior-package-inferior package))
        (id       (inferior-package-id package)))
    (inferior-eval `(,getter (hashv-ref %package-table ,id))
                   inferior)))

(define* (inferior-package-synopsis package #:key (translate? #t))
  "Return the Texinfo synopsis of PACKAGE, an inferior package.  When
TRANSLATE? is true, translate it to the current locale's language."
  (inferior-package-field package
                          (if translate?
                              '(compose (@ (guix ui) P_) package-synopsis)
                              'package-synopsis)))

(define* (inferior-package-description package #:key (translate? #t))
  "Return the Texinfo description of PACKAGE, an inferior package.  When
TRANSLATE? is true, translate it to the current locale's language."
  (inferior-package-field package
                          (if translate?
                              '(compose (@ (guix ui) P_) package-description)
                              'package-description)))

(define (inferior-package-home-page package)
  "Return the home page of PACKAGE."
  (inferior-package-field package 'package-home-page))

(define (inferior-package-location package)
  "Return the source code location of PACKAGE, either #f or a <location>
record."
  (source-properties->location
   (inferior-package-field package
                           '(compose (lambda (loc)
                                       (and loc
                                            (location->source-properties
                                             loc)))
                                     package-location))))
