;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Free Software Foundation, Inc.
;;; Copyright © 2018 Sahithi Yarlagadda <sahi@swecha.net>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix colors)
  #:use-module (guix memoization)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (color
            color?

            colorize-string
            highlight
            color-rules
            color-output?
            isatty?*))

;;; Commentary:
;;;
;;; This module provides tools to produce colored output using ANSI escapes.
;;;
;;; Code:

;; Record type for "colors", which are actually lists of color attributes.
(define-record-type <color>
  (make-color symbols ansi)
  color?
  (symbols  color-symbols)
  (ansi     color-ansi))

(define (print-color color port)
  (format port "#<color ~a>"
          (string-join (map symbol->string
                            (color-symbols color)))))

(set-record-type-printer! <color> print-color)

(define-syntax define-color-table
  (syntax-rules ()
    "Define NAME as a macro that builds a list of color attributes."
    ((_ name (color escape) ...)
     (begin
       (define-syntax color-codes
         (syntax-rules (color ...)
           ((_)
            '())
           ((_ color rest (... ...))
            `(escape ,@(color-codes rest (... ...))))
           ...))

       (define-syntax-rule (name colors (... ...))
         "Return a list of color attributes that can be passed to
'colorize-string'."
         (make-color '(colors (... ...))
                     (color-codes->ansi (color-codes colors (... ...)))))))))

(define-color-table color
  (CLEAR        "0")
  (RESET        "0")
  (BOLD         "1")
  (DARK         "2")
  (UNDERLINE    "4")
  (UNDERSCORE   "4")
  (BLINK        "5")
  (REVERSE      "6")
  (CONCEALED    "8")
  (BLACK       "30")
  (RED         "31")
  (GREEN       "32")
  (YELLOW      "33")
  (BLUE        "34")
  (MAGENTA     "35")
  (CYAN        "36")
  (WHITE       "37")
  (ON-BLACK    "40")
  (ON-RED      "41")
  (ON-GREEN    "42")
  (ON-YELLOW   "43")
  (ON-BLUE     "44")
  (ON-MAGENTA  "45")
  (ON-CYAN     "46")
  (ON-WHITE    "47"))

(define (color-codes->ansi codes)
  "Convert CODES, a list of color attribute codes, to a ANSI escape string."
  (match codes
    (()
     "")
    (_
     (string-append (string #\esc #\[)
                    (string-join codes ";" 'infix)
                    "m"))))

(define %reset
  (color RESET))

(define (colorize-string str color)
  "Return a copy of STR colorized using ANSI escape sequences according to
COLOR.  At the end of the returned string, the color attributes are reset such
that subsequent output will not have any colors in effect."
  (string-append (color-ansi color)
                 str
                 (color-ansi %reset)))

(define isatty?*
  (mlambdaq (port)
    "Return true if PORT is a tty.  Memoize the result."
    (isatty? port)))

(define (color-output? port)
  "Return true if we should write colored output to PORT."
  (and (not (getenv "INSIDE_EMACS"))
       (not (getenv "NO_COLOR"))
       (isatty?* port)))

(define %highlight-color (color BOLD))

(define* (highlight str #:optional (port (current-output-port)))
  "Return STR with extra ANSI color attributes to highlight it if PORT
supports it."
  (if (color-output? port)
      (colorize-string str %highlight-color)
      str))

(define (colorize-matches rules)
  "Return a procedure that, when passed a string, returns that string
colorized according to RULES.  RULES must be a list of tuples like:

  (REGEXP COLOR1 COLOR2 ...)

where COLOR1 specifies how to colorize the first submatch of REGEXP, and so
on."
  (lambda (str)
    (if (string-index str #\nul)
        str
        (let loop ((rules rules))
          (match rules
            (()
             str)
            (((regexp . colors) . rest)
             (match (regexp-exec regexp str)
               (#f (loop rest))
               (m  (let loop ((n 1)
                              (colors colors)
                              (result (list (match:prefix m))))
                     (match colors
                       (()
                        (string-concatenate-reverse
                         (cons (match:suffix m) result)))
                       ((first . tail)
                        (loop (+ n 1)
                              tail
                              (cons (colorize-string (match:substring m n)
                                                     first)
                                    result)))))))))))))

(define-syntax color-rules
  (syntax-rules ()
    "Return a procedure that colorizes the string it is passed according to
the given rules.  Each rule has the form:

  (REGEXP COLOR1 COLOR2 ...)

where COLOR1 specifies how to colorize the first submatch of REGEXP, and so
on."
    ((_ (regexp colors ...) ...)
     (colorize-matches `((,(make-regexp regexp) ,(color colors) ...)
                         ...)))))
