;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021-2022 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:export (pretty-print-with-comments
            read-with-comments
            canonicalize-comment

            guix-style))


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

(define-syntax vhashq
  (syntax-rules (quote)
    ((_) vlist-null)
    ((_ (key (quote (lst ...))) rest ...)
     (vhash-consq key '(lst ...) (vhashq rest ...)))
    ((_ (key value) rest ...)
     (vhash-consq key '((() . value)) (vhashq rest ...)))))

(define %special-forms
  ;; Forms that are indented specially.  The number is meant to be understood
  ;; like Emacs' 'scheme-indent-function' symbol property.  When given an
  ;; alist instead of a number, the alist gives "context" in which the symbol
  ;; is a special form; for instance, context (modify-phases) means that the
  ;; symbol must appear within a (modify-phases ...) expression.
  (vhashq
   ('begin 1)
   ('lambda 2)
   ('lambda* 2)
   ('match-lambda 1)
   ('match-lambda* 2)
   ('define 2)
   ('define* 2)
   ('define-public 2)
   ('define*-public 2)
   ('define-syntax 2)
   ('define-syntax-rule 2)
   ('define-module 2)
   ('define-gexp-compiler 2)
   ('let 2)
   ('let* 2)
   ('letrec 2)
   ('letrec* 2)
   ('match 2)
   ('when 2)
   ('unless 2)
   ('package 1)
   ('origin 1)
   ('operating-system 1)
   ('modify-inputs 2)
   ('modify-phases 2)
   ('add-after '(((modify-phases) . 3)))
   ('add-before '(((modify-phases) . 3)))
   ('replace '(((modify-phases) . 2)))         ;different from 'modify-inputs'
   ('substitute* 2)
   ('substitute-keyword-arguments 2)
   ('call-with-input-file 2)
   ('call-with-output-file 2)
   ('with-output-to-file 2)
   ('with-input-from-file 2)))

(define %newline-forms
  ;; List heads that must be followed by a newline.  The second argument is
  ;; the context in which they must appear.  This is similar to a special form
  ;; of 1, except that indent is 1 instead of 2 columns.
  (vhashq
   ('arguments '(package))
   ('sha256 '(origin source package))
   ('base32 '(sha256 origin))
   ('git-reference '(uri origin source))
   ('search-paths '(package))
   ('native-search-paths '(package))
   ('search-path-specification '())))

(define (prefix? candidate lst)
  "Return true if CANDIDATE is a prefix of LST."
  (let loop ((candidate candidate)
             (lst lst))
    (match candidate
      (() #t)
      ((head1 . rest1)
       (match lst
         (() #f)
         ((head2 . rest2)
          (and (equal? head1 head2)
               (loop rest1 rest2))))))))

(define (special-form-lead symbol context)
  "If SYMBOL is a special form in the given CONTEXT, return its number of
arguments; otherwise return #f.  CONTEXT is a stack of symbols lexically
surrounding SYMBOL."
  (match (vhash-assq symbol %special-forms)
    (#f #f)
    ((_ . alist)
     (any (match-lambda
            ((prefix . level)
             (and (prefix? prefix context) (- level 1))))
          alist))))

(define (newline-form? symbol context)
  "Return true if parenthesized expressions starting with SYMBOL must be
followed by a newline."
  (match (vhash-assq symbol %newline-forms)
    (#f #f)
    ((_ . prefix)
     (prefix? prefix context))))

(define (escaped-string str)
  "Return STR with backslashes and double quotes escaped.  Everything else, in
particular newlines, is left as is."
  (list->string
   `(#\"
     ,@(string-fold-right (lambda (chr lst)
                            (match chr
                              (#\" (cons* #\\ #\" lst))
                              (#\\ (cons* #\\ #\\ lst))
                              (_   (cons chr lst))))
                          '()
                          str)
     #\")))

(define (string-width str)
  "Return the \"width\" of STR--i.e., the width of the longest line of STR."
  (apply max (map string-length (string-split str #\newline))))

(define (canonicalize-comment c)
  "Canonicalize comment C, ensuring it has the \"right\" number of leading
semicolons."
  (let ((line (string-trim-both
               (string-trim (comment->string c) (char-set #\;)))))
    (comment (string-append
              (if (comment-margin? c)
                  ";"
                  (if (string-null? line)
                      ";;"                        ;no trailing space
                      ";; "))
              line "\n")
             (comment-margin? c))))

(define* (pretty-print-with-comments port obj
                                     #:key
                                     (format-comment identity)
                                     (indent 0)
                                     (max-width 78)
                                     (long-list 5))
  "Pretty-print OBJ to PORT, attempting to at most MAX-WIDTH character columns
and assuming the current column is INDENT.  Comments present in OBJ are
included in the output.

Lists longer than LONG-LIST are written as one element per line.  Comments are
passed through FORMAT-COMMENT before being emitted; a useful value for
FORMAT-COMMENT is 'canonicalize-comment'."
  (let loop ((indent indent)
             (column indent)
             (delimited? #t)                  ;true if comes after a delimiter
             (context '())                    ;list of "parent" symbols
             (obj obj))
    (define (print-sequence context indent column lst delimited?)
      (define long?
        (> (length lst) long-list))

      (let print ((lst lst)
                  (first? #t)
                  (delimited? delimited?)
                  (column column))
        (match lst
          (()
           column)
          ((item . tail)
           (define newline?
             ;; Insert a newline if ITEM is itself a list, or if TAIL is long,
             ;; but only if ITEM is not the first item.  Also insert a newline
             ;; before a keyword.
             (and (or (pair? item) long?
                      (and (keyword? item)
                           (not (eq? item #:allow-other-keys))))
                  (not first?) (not delimited?)
                  (not (comment? item))))

           (when newline?
             (newline port)
             (display (make-string indent #\space) port))
           (let ((column (if newline? indent column)))
             (print tail #f
                    (comment? item)
                    (loop indent column
                          (or newline? delimited?)
                          context
                          item)))))))

    (define (sequence-would-protrude? indent lst)
      ;; Return true if elements of LST written at INDENT would protrude
      ;; beyond MAX-WIDTH.  This is implemented as a cheap test with false
      ;; negatives to avoid actually rendering all of LST.
      (find (match-lambda
              ((? string? str)
               (>= (+ (string-width str) 2 indent) max-width))
              ((? symbol? symbol)
               (>= (+ (string-width (symbol->string symbol)) indent)
                   max-width))
              ((? boolean?)
               (>= (+ 2 indent) max-width))
              (()
               (>= (+ 2 indent) max-width))
              (_                                  ;don't know
               #f))
            lst))

    (define (special-form? head)
      (special-form-lead head context))

    (match obj
      ((? comment? comment)
       (if (comment-margin? comment)
           (begin
             (display " " port)
             (display (comment->string (format-comment comment))
                      port))
           (begin
             ;; When already at the beginning of a line, for example because
             ;; COMMENT follows a margin comment, no need to emit a newline.
             (unless (= column indent)
               (newline port)
               (display (make-string indent #\space) port))
             (display (comment->string (format-comment comment))
                      port)))
       (display (make-string indent #\space) port)
       indent)
      (('quote lst)
       (unless delimited? (display " " port))
       (display "'" port)
       (loop indent (+ column (if delimited? 1 2)) #t context lst))
      (('quasiquote lst)
       (unless delimited? (display " " port))
       (display "`" port)
       (loop indent (+ column (if delimited? 1 2)) #t context lst))
      (('unquote lst)
       (unless delimited? (display " " port))
       (display "," port)
       (loop indent (+ column (if delimited? 1 2)) #t context lst))
      (('unquote-splicing lst)
       (unless delimited? (display " " port))
       (display ",@" port)
       (loop indent (+ column (if delimited? 2 3)) #t context lst))
      (('gexp lst)
       (unless delimited? (display " " port))
       (display "#~" port)
       (loop indent (+ column (if delimited? 2 3)) #t context lst))
      (('ungexp obj)
       (unless delimited? (display " " port))
       (display "#$" port)
       (loop indent (+ column (if delimited? 2 3)) #t context obj))
      (('ungexp-native obj)
       (unless delimited? (display " " port))
       (display "#+" port)
       (loop indent (+ column (if delimited? 2 3)) #t context obj))
      (('ungexp-splicing lst)
       (unless delimited? (display " " port))
       (display "#$@" port)
       (loop indent (+ column (if delimited? 3 4)) #t context lst))
      (('ungexp-native-splicing lst)
       (unless delimited? (display " " port))
       (display "#+@" port)
       (loop indent (+ column (if delimited? 3 4)) #t context lst))
      (((? special-form? head) arguments ...)
       ;; Special-case 'let', 'lambda', 'modify-inputs', etc. so the second
       ;; and following arguments are less indented.
       (let* ((lead    (special-form-lead head context))
              (context (cons head context))
              (head    (symbol->string head))
              (total   (length arguments)))
         (unless delimited? (display " " port))
         (display "(" port)
         (display head port)
         (unless (zero? lead)
           (display " " port))

         ;; Print the first LEAD arguments.
         (let* ((indent (+ column 2
                                  (if delimited? 0 1)))
                (column (+ column 1
                                  (if (zero? lead) 0 1)
                                  (if delimited? 0 1)
                                  (string-length head)))
                (initial-indent column))
           (define new-column
             (let inner ((n lead)
                         (arguments (take arguments (min lead total)))
                         (column column))
               (if (zero? n)
                   (begin
                     (newline port)
                     (display (make-string indent #\space) port)
                     indent)
                   (match arguments
                     (() column)
                     ((head . tail)
                      (inner (- n 1) tail
                             (loop initial-indent column
                                   (= n lead)
                                   context
                                   head)))))))

           ;; Print the remaining arguments.
           (let ((column (print-sequence
                          context indent new-column
                          (drop arguments (min lead total))
                          #t)))
             (display ")" port)
             (+ column 1)))))
      ((head tail ...)
       (let* ((overflow? (>= column max-width))
              (column    (if overflow?
                             (+ indent 1)
                             (+ column (if delimited? 1 2))))
              (newline?  (newline-form? head context))
              (context   (cons head context)))
         (if overflow?
             (begin
               (newline port)
               (display (make-string indent #\space) port))
             (unless delimited? (display " " port)))
         (display "(" port)

         (let* ((new-column (loop column column #t context head))
                (indent (if (or (>= new-column max-width)
                                (not (symbol? head))
                                (sequence-would-protrude?
                                 (+ new-column 1) tail)
                                newline?)
                            column
                            (+ new-column 1))))
           (when newline?
             ;; Insert a newline right after HEAD.
             (newline port)
             (display (make-string indent #\space) port))

           (let ((column
                  (print-sequence context indent
                                  (if newline? indent new-column)
                                  tail newline?)))
             (display ")" port)
             (+ column 1)))))
      (_
       (let* ((str (if (string? obj)
                       (escaped-string obj)
                       (object->string obj)))
              (len (string-width str)))
         (if (and (> (+ column 1 len) max-width)
                  (not delimited?))
             (begin
               (newline port)
               (display (make-string indent #\space) port)
               (display str port)
               (+ indent len))
             (begin
               (unless delimited? (display " " port))
               (display str port)
               (+ column (if delimited? 0 1) len))))))))

(define (object->string* obj indent . args)
  (call-with-output-string
    (lambda (port)
      (apply pretty-print-with-comments port obj
             #:indent indent
             args))))


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

(define (edit-expression/dry-run properties rewrite-string)
  "Like 'edit-expression' but display what would be edited without actually
doing it."
  (edit-expression properties
                   (lambda (str)
                     (unless (string=? (rewrite-string str) str)
                       (info (source-properties->location properties)
                             (G_ "would be edited~%")))
                     str)))

(define (absolute-location loc)
  "Replace the file name in LOC by an absolute location."
  (location (if (string-prefix? "/" (location-file loc))
                (location-file loc)
                (search-path %load-path (location-file loc)))
            (location-line loc)
            (location-column loc)))

(define* (simplify-package-inputs package
                                  #:key (policy 'silent)
                                  (edit-expression edit-expression))
  "Edit the source code of PACKAGE to simplify its inputs field if needed.
POLICY is a symbol that defines whether to simplify inputs; it can one of
'silent (change only if the resulting derivation is the same), 'safe (change
only if semantics are known to be unaffected), and 'always (fearlessly
simplify inputs!).  Call EDIT-EXPRESSION to actually edit the source of
PACKAGE."
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
                     (location->source-properties (absolute-location location))
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
                            (const #t))))

                       (simplify-inputs location
                                        (package-name package)
                                        str inputs
                                        #:label-matches? matches?))))))))
            '(inputs native-inputs propagated-inputs)
            (list package-inputs package-native-inputs
                  package-propagated-inputs)))


;;;
;;; Formatting package definitions.
;;;

(define* (format-package-definition package
                                    #:key policy
                                    (edit-expression edit-expression))
  "Reformat the definition of PACKAGE."
  (unless (package-definition-location package)
    (leave (package-location package)
           (G_ "no definition location for package ~a~%")
           (package-full-name package)))

  (edit-expression
   (location->source-properties
    (absolute-location (package-definition-location package)))
   (lambda (str)
     (let ((exp (call-with-input-string str
                  read-with-comments)))
       (object->string* exp
                        (location-column
                         (package-definition-location package))
                        #:format-comment canonicalize-comment)))))

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

        (option '(#\n "dry-run") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'dry-run? #t result)))
        (option '(#\e "expression") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'expression arg result)))
        (option '(#\S "styling") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'styling-procedure
                              (match arg
                                ("inputs" simplify-package-inputs)
                                ("format" format-package-definition)
                                (_ (leave (G_ "~a: unknown styling~%")
                                          arg)))
                              result)))
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
  -S, --styling=RULE     apply RULE, a styling rule"))
  (newline)
  (display (G_ "
  -n, --dry-run          display files that would be edited but do nothing"))
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
  `((input-simplification-policy . silent)
    (styling-procedure . ,format-package-definition)))


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
         (edit     (if (assoc-ref opts 'dry-run?)
                       edit-expression/dry-run
                       edit-expression))
         (style    (assoc-ref opts 'styling-procedure))
         (policy   (assoc-ref opts 'input-simplification-policy)))
    (with-error-handling
      (for-each (lambda (package)
                  (style package #:policy policy
                         #:edit-expression edit))
                ;; Sort package by source code location so that we start editing
                ;; files from the bottom and going upward.  That way, the
                ;; 'location' field of <package> records is not invalidated as
                ;; we modify files.
                (sort (if (null? packages)
                          (fold-packages cons '() #:select? (const #t))
                          packages)
                      (negate package-location<?))))))
