;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix diagnostics)
  #:use-module (guix colors)
  #:use-module (guix i18n)
  #:autoload   (guix utils) (<location>)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (warning
            info
            report-error
            leave

            location->string

            guix-warning-port
            program-name))

;;; Commentary:
;;;
;;; This module provides the tools to report diagnostics to the user in a
;;; consistent way: errors, warnings, and notes.
;;;
;;; Code:

(define-syntax highlight-argument
  (lambda (s)
    "Given FMT and ARG, expand ARG to a call that highlights it, provided FMT
is a trivial format string."
    (define (trivial-format-string? fmt)
      (define len
        (string-length fmt))

      (let loop ((start 0))
        (or (>= (+ 1 start) len)
            (let ((tilde (string-index fmt #\~ start)))
              (or (not tilde)
                  (case (string-ref fmt (+ tilde 1))
                    ((#\a #\A #\%) (loop (+ tilde 2)))
                    (else          #f)))))))

    ;; Be conservative: limit format argument highlighting to cases where the
    ;; format string contains nothing but ~a escapes.  If it contained ~s
    ;; escapes, this strategy wouldn't work.
    (syntax-case s ()
      ((_ "~a~%" arg)                          ;don't highlight whole messages
       #'arg)
      ((_ fmt arg)
       (trivial-format-string? (syntax->datum #'fmt))
       #'(%highlight-argument arg))
      ((_ fmt arg)
       #'arg))))

(define* (%highlight-argument arg #:optional (port (guix-warning-port)))
  "Highlight ARG, a format string argument, if PORT supports colors."
  (cond ((string? arg)
         ;; If ARG contains white space, don't highlight it, on the grounds
         ;; that it may be a complete message in its own, like those produced
         ;; by 'guix lint.
         (if (string-any char-set:whitespace arg)
             arg
             (highlight arg port)))
        ((symbol? arg)
         (highlight (symbol->string arg) port))
        (else arg)))

(define-syntax define-diagnostic
  (syntax-rules ()
    "Create a diagnostic macro (i.e., NAME), which will prepend PREFIX to all
messages."
    ((_ name (G_ prefix) colors)
     (define-syntax name
       (lambda (x)
         (syntax-case x ()
           ((name location (underscore fmt) args (... ...))
            (and (string? (syntax->datum #'fmt))
                 (free-identifier=? #'underscore #'G_))
            #'(begin
                (print-diagnostic-prefix prefix location
                                         #:colors colors)
                (format (guix-warning-port) (gettext fmt %gettext-domain)
                        (highlight-argument fmt args) (... ...))))
           ((name location (N-underscore singular plural n)
                  args (... ...))
            (and (string? (syntax->datum #'singular))
                 (string? (syntax->datum #'plural))
                 (free-identifier=? #'N-underscore #'N_))
            #'(begin
                (print-diagnostic-prefix prefix location
                                         #:colors colors)
                (format (guix-warning-port)
                        (ngettext singular plural n %gettext-domain)
                        (highlight-argument singular args) (... ...))))
           ((name (underscore fmt) args (... ...))
            (free-identifier=? #'underscore #'G_)
            #'(name #f (underscore fmt) args (... ...)))
           ((name (N-underscore singular plural n)
                  args (... ...))
            (free-identifier=? #'N-underscore #'N_)
            #'(name #f (N-underscore singular plural n)
                    args (... ...)))))))))

;; XXX: This doesn't work well for right-to-left languages.
;; TRANSLATORS: The goal is to emit "warning:" followed by a short phrase;
;; "~a" is a placeholder for that phrase.
(define-diagnostic warning (G_ "warning: ") %warning-color) ;emit a warning
(define-diagnostic info (G_ "") %info-color)
(define-diagnostic report-error (G_ "error: ") %error-color)

(define-syntax-rule (leave args ...)
  "Emit an error message and exit."
  (begin
    (report-error args ...)
    (exit 1)))

(define %warning-color (color BOLD MAGENTA))
(define %info-color (color BOLD))
(define %error-color (color BOLD RED))

(define* (print-diagnostic-prefix prefix #:optional location
                                  #:key (colors (color)))
  "Print PREFIX as a diagnostic line prefix."
  (define color?
    (color-output? (guix-warning-port)))

  (define location-color
    (if color?
        (cut colorize-string <> (color BOLD))
        identity))

  (define prefix-color
    (if color?
        (lambda (prefix)
          (colorize-string prefix colors))
        identity))

  (let ((prefix (if (string-null? prefix)
                    prefix
                    (gettext prefix %gettext-domain))))
    (if location
        (format (guix-warning-port) "~a: ~a"
                (location-color (location->string location))
                (prefix-color prefix))
        (format (guix-warning-port) "~:[~*~;guix ~a: ~]~a"
                (program-name) (program-name)
                (prefix-color prefix)))))

(define (location->string loc)
  "Return a human-friendly, GNU-standard representation of LOC."
  (match loc
    (#f (G_ "<unknown location>"))
    (($ <location> file line column)
     (format #f "~a:~a:~a" file line column))))


(define guix-warning-port
  (make-parameter (current-warning-port)))

(define program-name
  ;; Name of the command-line program currently executing, or #f.
  (make-parameter #f))
