;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

;;; Commentary:
;;;
;;; This script updates package definitions so they use the "simplified" style
;;; for input lists, as in:
;;;
;;;  (package
;;;    ;; ...
;;;    (inputs (list foo bar baz)))
;;;
;;; Code:

(define-module (guix scripts style)
  #:autoload   (gnu packages) (specification->package fold-packages)
  #:use-module (guix scripts)
  #:use-module ((guix scripts build) #:select (%standard-build-options))
  #:use-module (guix combinators)
  #:use-module (guix ui)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:export (guix-style))


;;;
;;; Comment-preserving reader.
;;;

;; A comment.
(define-record-type <comment>
  (comment str margin?)
  comment?
  (str     comment->string)
  (margin? comment-margin?))

(define (read-with-comments port)
  "Like 'read', but include <comment> objects when they're encountered."
  ;; Note: Instead of implementing this functionality in 'read' proper, which
  ;; is the best approach long-term, this code is a later on top of 'read',
  ;; such that we don't have to rely on a specific Guile version.
  (let loop ((blank-line? #t)
             (return (const 'unbalanced)))
    (match (read-char port)
      ((? eof-object? eof)
       eof)                                       ;oops!
      (chr
       (cond ((eqv? chr #\newline)
              (loop #t return))
             ((char-set-contains? char-set:whitespace chr)
              (loop blank-line? return))
             ((memv chr '(#\( #\[))
              (let/ec return
                (let liip ((lst '()))
                  (liip (cons (loop (match lst
                                      (((? comment?) . _) #t)
                                      (_ #f))
                                    (lambda ()
                                      (return (reverse lst))))
                              lst)))))
             ((memv chr '(#\) #\]))
              (return))
             ((eq? chr #\')
              (list 'quote (loop #f return)))
             ((eq? chr #\`)
              (list 'quasiquote (loop #f return)))
             ((eq? chr #\,)
              (list (match (peek-char port)
                      (#\@
                       (read-char port)
                       'unquote-splicing)
                      (_
                       'unquote))
                    (loop #f return)))
             ((eqv? chr #\;)
              (unread-char chr port)
              (comment (read-line port 'concat)
                       (not blank-line?)))
             (else
              (unread-char chr port)
              (read port)))))))


;;;
;;; Comment-preserving pretty-printer.
;;;

(define* (pretty-print-with-comments port obj
                                     #:key
                                     (indent 0)
                                     (max-width 78)
                                     (long-list 5))
  (let loop ((indent indent)
             (column indent)
             (delimited? #t)                  ;true if comes after a delimiter
             (obj obj))
    (match obj
      ((? comment? comment)
       (if (comment-margin? comment)
           (begin
             (display " " port)
             (display (comment->string comment) port))
           (begin
             ;; When already at the beginning of a line, for example because
             ;; COMMENT follows a margin comment, no need to emit a newline.
             (unless (= column indent)
               (newline port)
               (display (make-string indent #\space) port))
             (display (comment->string comment) port)))
       (display (make-string indent #\space) port)
       indent)
      (('quote lst)
       (unless delimited? (display " " port))
       (display "'" port)
       (loop indent (+ column (if delimited? 1 2)) #t lst))
      (('quasiquote lst)
       (unless delimited? (display " " port))
       (display "`" port)
       (loop indent (+ column (if delimited? 1 2)) #t lst))
      (('unquote lst)
       (unless delimited? (display " " port))
       (display "," port)
       (loop indent (+ column (if delimited? 1 2)) #t lst))
      (('modify-inputs inputs clauses ...)
       ;; Special-case 'modify-inputs' to have one clause per line and custom
       ;; indentation.
       (let ((head "(modify-inputs "))
         (display head port)
         (loop (+ indent 4)
               (+ column (string-length head))
               #t
               inputs)
         (let* ((indent (+ indent 2))
                (column (fold (lambda (clause column)
                                (newline port)
                                (display (make-string indent #\space)
                                         port)
                                (loop indent indent #t clause))
                              indent
                              clauses)))
           (display ")" port)
           (+ column 1))))
      ((head tail ...)
       (unless delimited? (display " " port))
       (display "(" port)
       (let* ((new-column (loop indent (+ 1 column) #t head))
              (indent (+ indent (- new-column column)))
              (long?  (> (length tail) long-list)))
         (define column
           (fold2 (lambda (item column first?)
                    (define newline?
                      ;; Insert a newline if ITEM is itself a list, or if TAIL
                      ;; is long, but only if ITEM is not the first item.
                      (and (or (pair? item) long?)
                           (not first?) (not (comment? item))))

                    (when newline?
                      (newline port)
                      (display (make-string indent #\space) port))
                    (let ((column (if newline? indent column)))
                      (values (loop indent
                                    column
                                    (= column indent)
                                    item)
                              (comment? item))))
                  (+ 1 new-column)
                  #t                              ;first
                  tail))
         (display ")" port)
         (+ column 1)))
      (_
       (let* ((str (object->string obj))
              (len (string-length str)))
         (if (> (+ column 1 len) max-width)
             (begin
               (newline port)
               (display (make-string indent #\space) port)
               (display str port)
               (+ indent len))
             (begin
               (unless delimited? (display " " port))
               (display str port)
               (+ column (if delimited? 1 2) len))))))))

(define (object->string* obj indent)
  (call-with-output-string
    (lambda (port)
      (pretty-print-with-comments port obj
                                  #:indent indent))))


;;;
;;; Simplifying input expressions.
;;;

(define (label-matches? label name)
  "Return true if LABEL matches NAME, a package name."
  (or (string=? label name)
      (and (string-prefix? "python-" label)
           (string-prefix? "python2-" name)
           (string=? (string-drop label (string-length "python-"))
                     (string-drop name (string-length "python2-"))))))

(define* (simplify-inputs location package str inputs
                          #:key (label-matches? label-matches?))
  "Simplify the inputs field of PACKAGE (a string) at LOCATION; its current
value is INPUTS the corresponding source code is STR.  Return a string to
replace STR."
  (define (simplify-input-expression return)
    (match-lambda
      ((label ('unquote symbol)) symbol)
      ((label ('unquote symbol) output)
       (list 'quasiquote
             (list (list 'unquote symbol) output)))
      (_
       ;; Expression doesn't look like a simple input.
       (warning location (G_ "~a: complex expression, \
bailing out~%")
                package)
       (return str))))

  (define (simplify-input exp input return)
    (define package* package)

    (match input
      ((or ((? string? label) (? package? package))
           ((? string? label) (? package? package)
            (? string?)))
       ;; If LABEL doesn't match PACKAGE's name, then simplifying would incur
       ;; a rebuild, and perhaps it would break build-side code relying on
       ;; this specific label.
       (if (label-matches? label (package-name package))
           ((simplify-input-expression return) exp)
           (begin
             (warning location (G_ "~a: input label \
'~a' does not match package name, bailing out~%")
                      package* label)
             (return str))))
      (_
       (warning location (G_ "~a: non-trivial input, \
bailing out~%")
                package*)
       (return str))))

  (define (simplify-expressions exp inputs return)
    ;; Simplify the expressions in EXP, which correspond to INPUTS, and return
    ;; a list of expressions.  Call RETURN with a string when bailing out.
    (let loop ((result '())
               (exp exp)
               (inputs inputs))
      (match exp
        (((? comment? head) . rest)
         (loop (cons head result) rest inputs))
        ((head . rest)
         (match inputs
           ((input . inputs)
            ;; HEAD (an sexp) and INPUT (an input tuple) are correlated.
            (loop (cons (simplify-input head input return) result)
                  rest inputs))
           (()
            ;; If EXP and INPUTS have a different length, that
            ;; means EXP is a non-trivial input list, for example
            ;; with input-splicing, conditionals, etc.
            (warning location (G_ "~a: input expression is too short~%")
                     package)
            (return str))))
        (()
         ;; It's possible for EXP to contain fewer elements than INPUTS, for
         ;; example in the case of input splicing.  No bailout here.  (XXX)
         (reverse result)))))

  (define inputs-exp
    (call-with-input-string str read-with-comments))

  (match inputs-exp
    (('list _ ...)                                ;already done
     str)
    (('modify-inputs _ ...)                       ;already done
     str)
    (('quasiquote                                 ;prepending inputs
      (exp ...
           ('unquote-splicing
            ((and symbol (or 'package-inputs 'package-native-inputs
                             'package-propagated-inputs))
             arg))))
     (let/ec return
       (object->string*
        (let ((things (simplify-expressions exp inputs return)))
          `(modify-inputs (,symbol ,arg)
                          (prepend ,@things)))
        (location-column location))))
    (('quasiquote                                 ;replacing an input
      ((and exp ((? string? to-delete) ('unquote replacement)))
       ('unquote-splicing
        ('alist-delete (? string? to-delete)
                       ((and symbol
                             (or 'package-inputs 'package-native-inputs
                                 'package-propagated-inputs))
                        arg)))))
     (let/ec return
       (object->string*
        (let ((things (simplify-expressions (list exp)
                                            (list (car inputs))
                                            return)))
          `(modify-inputs (,symbol ,arg)
                          (replace ,to-delete ,replacement)))
        (location-column location))))

    (('quasiquote                                 ;removing an input
      (exp ...
           ('unquote-splicing
            ('alist-delete (? string? to-delete)
                           ((and symbol
                                 (or 'package-inputs 'package-native-inputs
                                     'package-propagated-inputs))
                            arg)))))
     (let/ec return
       (object->string*
        (let ((things (simplify-expressions exp inputs return)))
          `(modify-inputs (,symbol ,arg)
                          (delete ,to-delete)
                          (prepend ,@things)))
        (location-column location))))
    (('fold 'alist-delete                         ;removing several inputs
            ((and symbol
                  (or 'package-inputs 'package-native-inputs
                      'package-propagated-inputs))
             arg)
            ('quote ((? string? to-delete) ...)))
     (object->string*
      `(modify-inputs (,symbol ,arg)
                      (delete ,@to-delete))
      (location-column location)))
    (('quasiquote                    ;removing several inputs and adding others
      (exp ...
           ('unquote-splicing
            ('fold 'alist-delete
                   ((and symbol
                         (or 'package-inputs 'package-native-inputs
                             'package-propagated-inputs))
                    arg)
                   ('quote ((? string? to-delete) ...))))))
     (let/ec return
       (object->string*
        (let ((things (simplify-expressions exp inputs return)))
          `(modify-inputs (,symbol ,arg)
                          (delete ,@to-delete)
                          (prepend ,@things)))
        (location-column location))))
    (('quasiquote (exp ...))
     (let/ec return
       (object->string*
        `(list ,@(simplify-expressions exp inputs return))
        (location-column location))))
    (_
     (warning location (G_ "~a: unsupported input style, \
bailing out~%")
              package)
     str)))

(define* (simplify-package-inputs package
                                  #:key (policy 'silent))
  "Edit the source code of PACKAGE to simplify its inputs field if needed.
POLICY is a symbol that defines whether to simplify inputs; it can one of
'silent (change only if the resulting derivation is the same), 'safe (change
only if semantics are known to be unaffected), and 'always (fearlessly
simplify inputs!)."
  (for-each (lambda (field-name field)
              (match (field package)
                (()
                 #f)
                (inputs
                 (match (package-field-location package field-name)
                   (#f
                    ;; If the location of FIELD-NAME is not found, it may be
                    ;; that PACKAGE inherits from another package.
                    #f)
                   (location
                    (edit-expression
                     (location->source-properties location)
                     (lambda (str)
                       (define matches?
                         (match policy
                           ('silent
                            ;; Simplify inputs only when the label matches
                            ;; perfectly, such that the resulting derivation
                            ;; is unchanged.
                            label-matches?)
                           ('safe
                            ;; If PACKAGE has no arguments, labels are known
                            ;; to have no effect: this is a "safe" change, but
                            ;; it may change the derivation.
                            (if (null? (package-arguments package))
                                (const #t)
                                label-matches?))
                           ('always
                            ;; Assume it's gonna be alright.
                            (const #f))))

                       (simplify-inputs location
                                        (package-name package)
                                        str inputs
                                        #:label-matches? matches?))))))))
            '(inputs native-inputs propagated-inputs)
            (list package-inputs package-native-inputs
                  package-propagated-inputs)))

(define (package-location<? p1 p2)
  "Return true if P1's location is \"before\" P2's."
  (let ((loc1 (package-location p1))
        (loc2 (package-location p2)))
    (and loc1 loc2
         (if (string=? (location-file loc1) (location-file loc2))
             (< (location-line loc1) (location-line loc2))
             (string<? (location-file loc1) (location-file loc2))))))


;;;
;;; Options.
;;;

(define %options
  ;; Specification of the command-line options.
  (list (find (lambda (option)
                (member "load-path" (option-names option)))
              %standard-build-options)

        (option '(#\e "expression") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'expression arg result)))
        (option '("input-simplification") #t #f
                (lambda (opt name arg result)
                  (let ((symbol (string->symbol arg)))
                    (unless (memq symbol '(silent safe always))
                      (leave (G_ "~a: invalid input simplification policy~%")
                             arg))
                    (alist-cons 'input-simplification-policy symbol
                                result))))

        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix style")))))

(define (show-help)
  (display (G_ "Usage: guix style [OPTION]... [PACKAGE]...
Update package definitions to the latest style.\n"))
  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (display (G_ "
  -e, --expression=EXPR  consider the package EXPR evaluates to"))
  (display (G_ "
      --input-simplification=POLICY
                         follow POLICY for package input simplification, one
                         of 'silent', 'safe', or 'always'"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %default-options
  ;; Alist of default option values.
  '((input-simplification-policy . silent)))


;;;
;;; Entry point.
;;;

(define-command (guix-style . args)
  (category packaging)
  (synopsis "update the style of package definitions")

  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts     (parse-options))
         (packages (filter-map (match-lambda
                                 (('argument . spec)
                                  (specification->package spec))
                                 (('expression . str)
                                  (read/eval str))
                                 (_ #f))
                               opts))
         (policy   (assoc-ref opts 'input-simplification-policy)))
    (for-each (lambda (package)
                (simplify-package-inputs package #:policy policy))
              ;; Sort package by source code location so that we start editing
              ;; files from the bottom and going upward.  That way, the
              ;; 'location' field of <package> records is not invalidated as
              ;; we modify files.
              (sort (if (null? packages)
                        (fold-packages cons '() #:select? (const #t))
                        packages)
                    (negate package-location<?)))))
