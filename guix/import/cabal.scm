;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix import cabal)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system base lalr)
  #:use-module (rnrs enums)
  #:use-module (guix utils)
  #:export (read-cabal
            eval-cabal
            
            cabal-custom-setup-dependencies

            cabal-package?
            cabal-package-name
            cabal-package-version
            cabal-package-revision
            cabal-package-license
            cabal-package-home-page
            cabal-package-source-repository
            cabal-package-synopsis
            cabal-package-description
            cabal-package-executables
            cabal-package-library
            cabal-package-test-suites
            cabal-package-flags
            cabal-package-eval-environment
            cabal-package-custom-setup

            cabal-source-repository?
            cabal-source-repository-use-case
            cabal-source-repository-type
            cabal-source-repository-location

            cabal-flag?
            cabal-flag-name
            cabal-flag-description
            cabal-flag-default
            cabal-flag-manual

            cabal-dependency?
            cabal-dependency-name
            cabal-dependency-version

            cabal-executable?
            cabal-executable-name
            cabal-executable-dependencies

            cabal-library?
            cabal-library-dependencies

            cabal-test-suite?
            cabal-test-suite-name
            cabal-test-suite-dependencies))

;; Part 1:
;;
;; Functions used to read a Cabal file.

;; Comment:
;;
;; The use of virtual closing braces VCCURLY and some lexer functions were
;; inspired from http://hackage.haskell.org/package/haskell-src

;; Object containing information about the structure of a block: (i) delimited
;; by braces or by indentation, (ii) minimum indentation.
(define-record-type  <parse-context>
  (make-parse-context mode indentation)
  parse-context?
  (mode parse-context-mode)                ; 'layout or 'no-layout
  (indentation parse-context-indentation)) ; #f for 'no-layout

;; <parse-context> mode set universe
(define-enumeration context (layout no-layout) make-context)

(define (make-stack)
  "Creates a simple stack closure.  Actions on the generated stack are
requested by calling it with one of the following symbols as the first
argument: 'empty?, 'push!, 'top, 'pop! and 'clear!.  The action 'push! is the
only one requiring a second argument corresponding to the object to be added
to the stack."
  (let ((stack '()))
    (lambda (msg . args)
      (cond ((eqv? msg 'empty?) (null? stack))
            ((eqv? msg 'push!) (set! stack (cons (first args) stack)))
            ((eqv? msg 'top) (if (null? stack) '() (first stack)))
            ((eqv? msg 'pop!) (match stack
                                ((e r ...) (set! stack (cdr stack)) e)
                                (_ #f)))
            ((eqv? msg 'clear!) (set! stack '()))
            (else #f)))))

;; Stack to track the structure of nested blocks and simple interface
(define context-stack (make-parameter (make-stack)))

(define (context-stack-empty?) ((context-stack) 'empty?))

(define (context-stack-push! e) ((context-stack) 'push! e))

(define (context-stack-top) ((context-stack) 'top))

(define (context-stack-pop!) ((context-stack) 'pop!))

(define (context-stack-clear!) ((context-stack) 'clear!))

;; Indentation of the line being parsed.
(define current-indentation (make-parameter 0))

;; Signal to reprocess the beginning of line, in case we need to close more
;; than one indentation level.
(define check-bol? (make-parameter #f))

;; Name of the file being parsed. Used in error messages.
(define cabal-file-name (make-parameter "unknowk"))

;; Specify the grammar of a Cabal file and generate a suitable syntax analyser.
(define (make-cabal-parser)
  "Generate a parser for Cabal files."
  (lalr-parser
   ;; --- token definitions
   (CCURLY VCCURLY OPAREN CPAREN TEST ID VERSION RELATION TRUE FALSE -ANY -NONE
           (right: IF FLAG EXEC TEST-SUITE CUSTOM-SETUP SOURCE-REPO BENCHMARK LIB OCURLY)
           (left: OR)
           (left: PROPERTY AND)
           (right: ELSE NOT))
   ;; --- rules
   (body        (properties sections)   : (append $1 $2))
   (sections    (sections flags)        : (append $1 $2)
                (sections source-repo)  : (append $1 (list $2))
                (sections executables)  : (append $1 $2)
                (sections test-suites)  : (append $1 $2)
                (sections custom-setup) : (append $1 $2)
                (sections benchmarks)   : (append $1 $2)
                (sections lib-sec)      : (append $1 (list $2))
                ()                      : '())
   (flags       (flags flag-sec)        : (append $1 (list $2))
                (flag-sec)              : (list $1))
   (flag-sec    (FLAG OCURLY properties CCURLY) : `(section flag ,$1 ,$3)
                (FLAG open properties close)    : `(section flag ,$1 ,$3)
                (FLAG)                          : `(section flag ,$1 '()))
   (source-repo (SOURCE-REPO OCURLY properties CCURLY)
                : `(section source-repository ,$1 ,$3)
                (SOURCE-REPO open properties close)
                : `(section source-repository ,$1 ,$3))
   (properties  (properties PROPERTY)   : (append $1 (list $2))
                (PROPERTY)              : (list $1))
   (executables (executables exec-sec)  : (append $1 (list $2))
                (exec-sec)              : (list $1))
   (exec-sec    (EXEC OCURLY exprs CCURLY) : `(section executable ,$1 ,$3)
                (EXEC open exprs close)    : `(section executable ,$1 ,$3))
   (test-suites (test-suites ts-sec)    : (append $1 (list $2))
                (ts-sec)                : (list $1))
   (ts-sec      (TEST-SUITE OCURLY exprs CCURLY) : `(section test-suite ,$1 ,$3)
                (TEST-SUITE open exprs close)    : `(section test-suite ,$1 ,$3))
   (custom-setup (CUSTOM-SETUP exprs) : (list `(section custom-setup ,$1 ,$2)))
   (benchmarks  (benchmarks bm-sec)     : (append $1 (list $2))
                (bm-sec)                : (list $1))
   (bm-sec      (BENCHMARK OCURLY exprs CCURLY) : `(section benchmark ,$1 ,$3)
                (BENCHMARK open exprs close)    : `(section benchmark ,$1 ,$3))
   (lib-sec     (LIB OCURLY exprs CCURLY) : `(section library ,$3)
                (LIB open exprs close)    : `(section library ,$3))
   (exprs       (exprs PROPERTY)         : (append $1 (list $2))
                (PROPERTY)               : (list $1)
                (exprs if-then-else)     : (append $1 (list $2))
                (if-then-else)           : (list $1)
                (exprs if-then)          : (append $1 (list $2))
                (if-then)                : (list $1))
   (if-then-else (IF tests OCURLY exprs CCURLY ELSE OCURLY exprs CCURLY)
                 : `(if ,$2 ,$4 ,$8)
                 (IF tests open exprs close ELSE OCURLY exprs CCURLY)
                 : `(if ,$2 ,$4 ,$8)
                 ;; The 'open' token after 'tests' is shifted after an 'exprs'
                 ;; is found.  This is because, instead of 'exprs' a 'OCURLY'
                 ;; token is a valid alternative.  For this reason, 'open'
                 ;; pushes a <parse-context> with a line indentation equal to
                 ;; the indentation of 'exprs'.
                 ;;
                 ;; Differently from this, without the rule above this
                 ;; comment, when an 'ELSE' token is found, the 'open' token
                 ;; following the 'ELSE' would be shifted immediately, before
                 ;; the 'exprs' is found (because there are no other valid
                 ;; tokens).  The 'open' would therefore create a
                 ;; <parse-context> with the indentation of 'ELSE' and not
                 ;; 'exprs', creating an inconsistency.  We therefore allow
                 ;; mixed style conditionals.
                 (IF tests open exprs close ELSE open exprs close)
                 : `(if ,$2 ,$4 ,$8))
   (if-then     (IF tests OCURLY exprs CCURLY) : `(if ,$2 ,$4 ())
                (IF tests open exprs close)    : `(if ,$2 ,$4 ()))
   (tests       (TEST OPAREN ID CPAREN)        : `(,$1 ,$3)
                (TRUE)                         : 'true
                (FALSE)                        : 'false
                (TEST OPAREN ID RELATION VERSION CPAREN)
                : `(,$1 ,(string-append $3 " " $4 " " $5))
                (TEST OPAREN ID -ANY CPAREN)
                : `(,$1 ,(string-append $3 " -any"))
                (TEST OPAREN ID -NONE CPAREN)
                : `(,$1 ,(string-append $3 " -none"))
                (TEST OPAREN ID RELATION VERSION AND RELATION VERSION CPAREN)
                : `(and (,$1 ,(string-append $3 " " $4 " " $5))
                        (,$1 ,(string-append $3 " " $7 " " $8)))
               (NOT tests)                     : `(not ,$2)
               (tests AND tests)               : `(and ,$1 ,$3)
               (tests OR tests)                : `(or ,$1 ,$3)
               (OPAREN tests CPAREN)           : $2)
   (open       () : (context-stack-push!
                                   (make-parse-context (context layout)
                                                       (current-indentation))))
   (close      (VCCURLY))))

(define (peek-next-line-indent port)
  "This function can be called when the next character on PORT is #\newline
and returns the indentation of the line starting after the #\newline
character.  Discard (and consume) empty and comment lines."
  (if (eof-object? (peek-char port))
      ;; If the file is missing the #\newline on the last line, add it and act
      ;; as if it were there. This is needed for proper operation of
      ;; indentation based block recognition (based on ‘port-column’).
      (begin (unread-char #\newline port) (read-char port) 0)
      (let ((initial-newline (string (read-char port))))
        (let loop ((char (peek-char port))
                   (word ""))
          (cond ((eqv? char #\newline) (read-char port)
                 (loop (peek-char port) ""))
                ((or (eqv? char #\space) (eqv? char #\tab))
                 (let ((c (read-char port)))
                   (loop (peek-char port) (string-append word (string c)))))
                ((comment-line port char) (loop (peek-char port) ""))
                (else
                 (let ((len (string-length word)))
                   (unread-string (string-append initial-newline word) port)
                   len)))))))

(define* (read-value port value min-indent #:optional (separator " "))
  "The next character on PORT must be #\newline.  Append to VALUE the
following lines with indentation larger than MIN-INDENT."
  (let loop ((val (string-trim-both value))
             (x (peek-next-line-indent port)))
    (if (> x min-indent)
        (begin
          (read-char port) ; consume #\newline
          (loop (string-append
                 val (if (string-null? val) "" separator)
                 (string-trim-both (read-delimited "\n" port 'peek)))
                (peek-next-line-indent port)))
        val)))

(define* (read-braced-value port)
  "Read up to a closing brace."
  (string-trim-both (read-delimited "}" port 'trim)))

(define (lex-white-space port bol)
  "Consume white spaces and comment lines on PORT.  If a new line is started return #t,
otherwise return BOL (beginning-of-line)."
  (let loop ((c (peek-char port))
             (bol bol))
    (cond
     ((and (not (eof-object? c))
           (or (char=? c #\space) (char=? c #\tab)))
      (read-char port)
      (loop (peek-char port) bol))
     ((and (not (eof-object? c)) (char=? c #\newline))
      (read-char port)
      (loop (peek-char port) #t))
     ((comment-line port c)
      (lex-white-space port bol))
     (else
      bol))))

(define (lex-bol port)
  "Process the beginning of a line on PORT: update current-indentation and
check the end of an indentation based context."
  (let ((loc (make-source-location (cabal-file-name) (port-line port)
                                   (port-column port) -1 -1)))
    (current-indentation (source-location-column loc))
    (case (get-offside port)
      ((less-than)
       (check-bol? #t) ; need to check if closing more than 1 indent level.
       (unless (context-stack-empty?) (context-stack-pop!))
       (make-lexical-token 'VCCURLY loc #f))
      (else
       (lex-token port)))))

(define (bol? port) (or (check-bol?) (= (port-column port) 0)))

(define (comment-line port c)
  "If PORT starts with a comment line, consume it up to, but not including
#\newline.  C is the next character on PORT."
  (cond ((and (not (eof-object? c)) (char=? c #\-))
         (read-char port)
         (let ((c2 (peek-char port)))
           (if (char=? c2 #\-)
               (read-delimited "\n" port 'peek)
               (begin (unread-char c port) #f))))
        (else #f)))

(define-enumeration ordering (less-than equal greater-than) make-ordering)

(define (get-offside port)
  "In an indentation based context return the symbol 'greater-than, 'equal or
'less-than to signal if the current column number on PORT is greater-, equal-,
or less-than the indentation of the current context."
  (let ((x (port-column port)))
    (match (context-stack-top)
      (($ <parse-context> 'layout indentation)
       (cond
        ((> x indentation) (ordering greater-than))
        ((= x indentation) (ordering equal))
        (else (ordering less-than))))
      (_ (ordering greater-than)))))
 
;; (Semi-)Predicates for individual tokens.

(define (is-relation? c)
  (and (char? c) (any (cut char=? c <>) '(#\< #\> #\=))))

(define* (make-rx-matcher pat #:optional (flag #f))
  "Compile PAT into a regular expression with FLAG and creates a function
matching a string against the created regexp."
  (let ((rx (if flag
                (make-regexp pat flag)
                (make-regexp pat))))
    (cut regexp-exec rx <>)))

(define is-layout-property (make-rx-matcher "([a-z0-9-]+)[ \t]*:[ \t]*(\\w?[^{}]*)$"
                                            regexp/icase))

(define is-braced-property (make-rx-matcher "([a-z0-9-]+)[ \t]*:[ \t]*\\{[ \t]*$"
                                            regexp/icase))

(define is-flag (make-rx-matcher "^flag +([a-z0-9_-]+)"
                                 regexp/icase))

(define is-src-repo
  (make-rx-matcher "^source-repository +([a-z0-9_-]+)"
                   regexp/icase))

(define is-exec (make-rx-matcher "^executable +([a-z0-9_-]+)"
                                 regexp/icase))

(define is-test-suite (make-rx-matcher "^test-suite +([a-z0-9_-]+)"
                                       regexp/icase))

(define is-custom-setup (make-rx-matcher "^(custom-setup)"
                                         regexp/icase))

(define is-benchmark (make-rx-matcher "^benchmark +([a-z0-9_-]+)"
                                      regexp/icase))

(define is-lib (make-rx-matcher "^library *" regexp/icase))

(define is-else (make-rx-matcher "^else" regexp/icase))

(define (is-if s) (string-ci=? s "if"))

(define (is-true s) (string-ci=? s "true"))

(define (is-false s) (string-ci=? s "false"))

(define (is-any s) (string-ci=? s "-any"))

(define (is-none s) (string-ci=? s "-none"))

(define (is-and s) (string=? s "&&"))

(define (is-or s) (string=? s "||"))

(define (is-id s port)
  (let ((cabal-reserved-words
         '("if" "else" "library" "flag" "executable" "test-suite" "custom-setup"
           "source-repository" "benchmark"))
        (spaces (read-while (cut char-set-contains? char-set:blank <>) port))
        (c (peek-char port)))
    (unread-string spaces port)
    (and (every (cut string-ci<> s <>) cabal-reserved-words)
         (and (not (char=? (last (string->list s)) #\:))
              (not (char=? #\: c))))))

(define (is-test s port)
  (let ((tests-rx (make-regexp "os|arch|flag|impl"))
        (spaces (read-while (cut char-set-contains? char-set:blank <>) port))
        (c (peek-char port)))
    (if (and (regexp-exec tests-rx s) (char=? #\( c))
        #t
        (begin (unread-string spaces port) #f))))

;; Lexers for individual tokens.

(define (lex-relation loc port)
  (make-lexical-token 'RELATION loc (read-while is-relation? port)))

(define (lex-version loc port)
  (make-lexical-token 'VERSION loc
                      (read-while (lambda (x)
                                    (or (char-numeric? x)
                                        (char=? x #\*)
                                        (char=? x #\.)))
                                  port)))

(define* (read-while is? port #:optional
                     (is-if-followed-by? (lambda (c) #f))
                     (is-allowed-follower? (lambda (c) #f)))
  "Read from PORT as long as: (i) either the read character satisfies the
predicate IS?, or (ii) it satisfies the predicate IS-IF-FOLLOWED-BY? and the
character immediately following it satisfies IS-ALLOWED-FOLLOWER?.  Returns a
string with the read characters."
  (let loop ((c (peek-char port))
             (res '()))
    (cond ((and (not (eof-object? c)) (is? c))
           (let ((c (read-char port)))
             (loop (peek-char port) (append res (list c)))))
          ((and (not (eof-object? c)) (is-if-followed-by? c))
           (let ((c (read-char port))
                 (c2 (peek-char port)))
             (if (and (not (eof-object? c2)) (is-allowed-follower? c2))
                 (loop c2 (append res (list c)))
                 (begin (unread-char c) (list->string res)))))
          (else (list->string res)))))

(define (lex-layout-property k-v-rx-res loc port)
  (let ((key (string-downcase (match:substring k-v-rx-res 1)))
        (value (match:substring k-v-rx-res 2)))
    (make-lexical-token
     'PROPERTY loc
     (list key `(,(read-value port value (current-indentation)))))))

(define (lex-braced-property k-rx-res loc port)
  (let ((key (string-downcase (match:substring k-rx-res 1))))
    (make-lexical-token
     'PROPERTY loc
     (list key `(,(read-braced-value port))))))

(define (lex-rx-res rx-res token loc)
  (let ((name (string-downcase (match:substring rx-res 1))))
    (make-lexical-token token loc name)))

(define (lex-flag flag-rx-res loc) (lex-rx-res flag-rx-res 'FLAG loc))

(define (lex-src-repo src-repo-rx-res loc)
  (lex-rx-res src-repo-rx-res 'SOURCE-REPO loc))

(define (lex-exec exec-rx-res loc) (lex-rx-res exec-rx-res 'EXEC loc))

(define (lex-test-suite ts-rx-res loc) (lex-rx-res ts-rx-res 'TEST-SUITE loc))

(define (lex-custom-setup ts-rx-res loc) (lex-rx-res ts-rx-res 'CUSTOM-SETUP loc))

(define (lex-benchmark bm-rx-res loc) (lex-rx-res bm-rx-res 'BENCHMARK loc))

(define (lex-lib loc) (make-lexical-token 'LIB loc #f))

(define (lex-else loc) (make-lexical-token 'ELSE loc #f))

(define (lex-if loc) (make-lexical-token 'IF loc #f))

(define (lex-true loc) (make-lexical-token 'TRUE loc #t))

(define (lex-false loc) (make-lexical-token 'FALSE loc #f))

(define (lex-any loc) (make-lexical-token '-ANY loc #f))

(define (lex-none loc) (make-lexical-token '-NONE loc #f))

(define (lex-and loc) (make-lexical-token 'AND loc #f))

(define (lex-or loc) (make-lexical-token 'OR loc #f))

(define (lex-id w loc) (make-lexical-token 'ID loc w))

(define (lex-test w loc) (make-lexical-token 'TEST loc (string->symbol w)))

;; Lexer for tokens recognizable by single char.

(define* (is-ref-char->token ref-char next-char token loc port
                         #:optional (hook-fn #f))
  "If the next character NEXT-CHAR on PORT is REF-CHAR, then read it,
execute HOOK-FN if it isn't #f and return a lexical token of type TOKEN with
location information LOC."
  (cond ((char=? next-char ref-char)
         (read-char port)
         (when hook-fn (hook-fn))
         (make-lexical-token token loc (string next-char)))
        (else #f)))

(define (is-ocurly->token c loc port)
  (is-ref-char->token #\{ c 'OCURLY loc port
                  (lambda ()
                    (context-stack-push! (make-parse-context
                                          (context no-layout) #f)))))

(define (is-ccurly->token c loc port)
  (is-ref-char->token #\} c 'CCURLY loc port (lambda () (context-stack-pop!))))

(define (is-oparen->token c loc port)
  (is-ref-char->token #\( c 'OPAREN loc port))

(define (is-cparen->token c loc port)
  (is-ref-char->token #\) c 'CPAREN loc port))

(define (is-not->token c loc port)
  (is-ref-char->token #\! c 'NOT loc port))

(define (is-version? c) (char-numeric? c))

;; Main lexer functions

(define (lex-single-char port loc)
  "Process tokens which can be recognised by peeking the next character on
PORT.  If no token can be recognized return #f.  LOC is the current port
location."
  (let* ((c (peek-char port)))
    (cond ((eof-object? c) (read-char port) '*eoi*)
          ((is-ocurly->token c loc port))
          ((is-ccurly->token c loc port))
          ((is-oparen->token c loc port))
          ((is-cparen->token c loc port))
          ((is-not->token c loc port))
          ((is-version? c) (lex-version loc port))
          ((is-relation? c) (lex-relation loc port))
          (else
           #f))))

(define (lex-word port loc)
  "Process tokens which can be recognized by reading the next word form PORT.
LOC is the current port location."
  (let* ((w (read-delimited " <>=()\t\n" port 'peek)))
    (cond ((is-if w) (lex-if loc))
          ((is-test w port) (lex-test w loc))
          ((is-true w) (lex-true loc))
          ((is-false w) (lex-false loc))
          ((is-any w) (lex-any loc))
          ((is-none w) (lex-none loc))
          ((is-and w) (lex-and loc))
          ((is-or w) (lex-or loc))
          ((is-id w port) (lex-id w loc))
          (else (unread-string w port) #f))))

(define (lex-line port loc)
  "Process tokens which can be recognised by reading a line from PORT.  LOC is
the current port location."
  (let* ((s (read-delimited "\n{}" port 'peek)))
    (cond
     ((is-flag s) => (cut lex-flag <> loc))
     ((is-src-repo s) => (cut lex-src-repo <> loc))
     ((is-exec s) => (cut lex-exec <> loc))
     ((is-test-suite s) => (cut lex-test-suite <> loc))
     ((is-custom-setup s) => (cut lex-custom-setup <> loc))
     ((is-benchmark s) => (cut lex-benchmark <> loc))
     ((is-lib s) (lex-lib loc))
     ((is-else s) (lex-else loc))
     (else (unread-string s port) #f))))

(define (lex-property port loc)
  (let* ((s (read-delimited "\n" port 'peek)))
    (cond
      ((is-braced-property s) => (cut lex-braced-property <> loc port))
      ((is-layout-property s) => (cut lex-layout-property <> loc port))
      (else #f))))

(define (lex-token port)
  (let* ((loc (make-source-location (cabal-file-name) (port-line port)
                                    (port-column port) -1 -1)))
    (or (lex-single-char port loc)
        (lex-word port loc)
        (lex-line port loc)
        (lex-property port loc))))

;; Lexer- and error-function generators

(define (errorp)
  "Generates the lexer error function."
  (let ((p (current-error-port)))
    (lambda (message . args)
      (format p "~a" message)
      (if (and (pair? args) (lexical-token? (car args)))
          (let* ((token (car args))
                 (source (lexical-token-source token))
                 (line (source-location-line source))
                 (column (source-location-column source)))
            (format p "~a " (or (lexical-token-value token)
                                 (lexical-token-category token)))
            (when (and (number? line) (number? column))
              (format p "(at line ~a, column ~a)" (1+ line) column)))
          (for-each display args))
      (format p "~%"))))

(define (make-lexer port)
  "Generate the Cabal lexical analyser reading from PORT."
  (let ((p port))
    (lambda ()
      (let ((bol (lex-white-space p (bol? p))))
        (check-bol? #f)
        (if bol (lex-bol p) (lex-token p))))))

(define* (read-cabal #:optional (port (current-input-port))
                     (file-name #f))
  "Read a Cabal file from PORT.  FILE-NAME is a string used in error messages.
If #f use the function 'port-filename' to obtain it."
  (let ((cabal-parser (make-cabal-parser)))
    (parameterize ((cabal-file-name
                    (or file-name (port-filename port) "standard input"))
                   (current-indentation 0)
                   (check-bol? #f)
                   (context-stack (make-stack)))
      (cabal-parser (make-lexer port) (errorp)))))

;; Part 2:
;;
;; Evaluate the S-expression returned by 'read-cabal'.

;; This defines the object and interface that we provide to access the Cabal
;; file information.  Note that this does not include all the pieces of
;; information of the Cabal file, but only the ones we currently are
;; interested in.
(define-record-type <cabal-package>
  (make-cabal-package name version revision license home-page source-repository
                      synopsis description
                      executables lib test-suites
                      flags eval-environment custom-setup)
  cabal-package?
  (name   cabal-package-name)
  (version cabal-package-version)
  (revision cabal-package-revision)
  (license cabal-package-license)
  (home-page cabal-package-home-page)
  (source-repository cabal-package-source-repository)
  (synopsis cabal-package-synopsis)
  (description cabal-package-description)
  (executables cabal-package-executables)
  (lib cabal-package-library) ; 'library' is a Scheme keyword
  (test-suites cabal-package-test-suites)
  (flags cabal-package-flags)
  (eval-environment cabal-package-eval-environment) ; alist
  (custom-setup cabal-package-custom-setup))

(set-record-type-printer! <cabal-package>
                          (lambda (package port)
                            (format port "#<cabal-package ~a@~a>"
                                      (cabal-package-name package)
                                      (cabal-package-version package))))

(define-record-type <cabal-source-repository>
  (make-cabal-source-repository use-case type location)
  cabal-source-repository?
  (use-case cabal-source-repository-use-case)
  (type cabal-source-repository-type)
  (location cabal-source-repository-location))

;; We need to be able to distinguish the value of a flag from the Scheme #t
;; and #f values.
(define-record-type <cabal-flag>
  (make-cabal-flag name description default manual)
  cabal-flag?
  (name cabal-flag-name)
  (description cabal-flag-description)
  (default cabal-flag-default) ; 'true or 'false
  (manual cabal-flag-manual))  ; 'true or 'false

(set-record-type-printer! <cabal-flag>
                          (lambda (package port)
                            (format port "#<cabal-flag ~a default:~a>"
                                      (cabal-flag-name package)
                                      (cabal-flag-default package))))

(define-record-type <cabal-dependency>
  (make-cabal-dependency name version)
  cabal-dependency?
  (name cabal-dependency-name)
  (version cabal-dependency-version))

(define-record-type <cabal-executable>
  (make-cabal-executable name dependencies)
  cabal-executable?
  (name cabal-executable-name)
  (dependencies cabal-executable-dependencies)) ; list of <cabal-dependency>

(define-record-type <cabal-library>
  (make-cabal-library dependencies)
  cabal-library?
  (dependencies cabal-library-dependencies)) ; list of <cabal-dependency>

(define-record-type <cabal-test-suite>
  (make-cabal-test-suite name dependencies)
  cabal-test-suite?
  (name cabal-test-suite-name)
  (dependencies cabal-test-suite-dependencies)) ; list of <cabal-dependency>

(define-record-type <cabal-custom-setup>
  (make-cabal-custom-setup name dependencies)
  cabal-custom-setup?
  (name cabal-custom-setup-name)
  (dependencies cabal-custom-setup-dependencies)) ; list of <cabal-dependency>

(define (cabal-flags->alist flag-list)
    "Retrun an alist associating the flag name to its default value from a
list of <cabal-flag> objects."
  (map (lambda (flag) (cons (cabal-flag-name flag) (cabal-flag-default flag)))
       flag-list))

(define (eval-cabal cabal-sexp env)
  "Given the CABAL-SEXP produced by 'read-cabal', evaluate all conditionals
and return a 'cabal-package' object.  The values of all tests can be
overwritten by specifying the desired value in ENV.  ENV must be an alist.
The accepted keys are: \"os\", \"arch\", \"impl\" and a name of a flag.  The
value associated with a flag has to be either \"true\" or \"false\".  The
value associated with other keys has to conform to the Cabal file format
definition."
  (define (os name)
    (let ((env-os (or (assoc-ref env "os") "linux")))
      (string-match env-os name)))
  
  (define (arch name)
    (let ((env-arch (or (assoc-ref env "arch") "x86_64")))
      (string-match env-arch name)))

  (define (comp-name+version haskell)
    "Extract the compiler name and version from the string HASKELL."
    (let* ((matcher-fn (make-rx-matcher "([a-zA-Z0-9_]+)-([0-9.]+)"))
           (name (or (and=> (matcher-fn haskell) (cut match:substring <> 1))
                     haskell))
           (version (and=> (matcher-fn haskell) (cut match:substring <> 2))))
      (values name version)))

  (define (comp-spec-name+op+version spec)
    "Extract the compiler specification from SPEC.  Return the compiler name,
the ordering operation and the version."
    (let* ((with-ver-matcher-fn (make-rx-matcher
                                 "([a-zA-Z0-9_-]+) *([<>=]+) *([0-9.]+) *"))
           (without-ver-matcher-fn (make-rx-matcher "([a-zA-Z0-9_-]+)"))
           (without-ver-matcher-fn-2 (make-rx-matcher "([a-zA-Z0-9_-]+) (-any|-none)"))
           (name (or (and=> (with-ver-matcher-fn spec)
                            (cut match:substring <> 1))
                     (and=> (without-ver-matcher-fn-2 spec)
                            (cut match:substring <> 1))
                     (match:substring (without-ver-matcher-fn spec) 1)))
           (operator (or (and=> (with-ver-matcher-fn spec)
                                (cut match:substring <> 2))
                         (and=> (without-ver-matcher-fn-2 spec)
                                (cut match:substring <> 2))))
           (version (or (and=> (with-ver-matcher-fn spec)
                               (cut match:substring <> 3))
                        (and=> (without-ver-matcher-fn-2 spec)
                               (cut match:substring <> 2)))))
      (values name operator version)))
  
  (define (impl haskell)
    (let*-values (((comp-name comp-ver)
                   (comp-name+version (or (assoc-ref env "impl") "ghc")))
                  ((spec-name spec-op spec-ver)
                   (comp-spec-name+op+version haskell)))
      (if (and spec-ver comp-ver)
          (cond
           ((not (string= spec-name comp-name)) #f)
           ((string= spec-op "==") (string= spec-ver comp-ver))
           ((string= spec-op ">=") (version>=? comp-ver spec-ver))
           ((string= spec-op ">") (version>? comp-ver spec-ver))
           ((string= spec-op "<=") (not (version>? comp-ver spec-ver)))
           ((string= spec-op "<") (not (version>=? comp-ver spec-ver)))
           ((string= spec-op "-any") #t)
           ((string= spec-op "-none") #f)
           (else
            (raise (condition
                    (&message (message "Failed to evaluate 'impl' test."))))))
          (string-match spec-name comp-name))))

  (define (cabal-flags)
    (make-cabal-section cabal-sexp 'flag))
  
  (define (flag name)
    (let ((value (or (assoc-ref env name)
                     (assoc-ref (cabal-flags->alist (cabal-flags)) name))))
      (if (eq? value 'false) #f #t)))
  (define (eval sexp)
    (match sexp
      (() '())
      ;; nested 'if'
      ((('if predicate true-group false-group) rest ...)
       (append (if (eval predicate)
                   (eval true-group)
                   (eval false-group))
               (eval rest)))
      (('if predicate true-group false-group)
       (if (eval predicate)
           (eval true-group)
           (eval false-group)))
      (('flag name) (flag name))
      (('os name) (os name))
      (('arch name) (arch name))
      (('impl name) (impl name))
      ('true #t)
      ('false #f)
      (('not name) (not (eval name)))
      ;; 'and' and 'or' aren't functions, thus we can't use apply
      (('and args ...) (fold (lambda (e s) (and e s)) #t (eval args)))
      (('or args ...) (fold (lambda (e s) (or e s)) #f (eval args)))
      ;; no need to evaluate flag parameters
      (('section 'flag name parameters)
       (list 'section 'flag name parameters))
      (('section 'custom-setup parameters)
       (list 'section 'custom-setup parameters))
      ;; library does not have a name parameter
      (('section 'library parameters)
       (list 'section 'library (eval parameters)))
      (('section type name parameters)
       (list 'section type name (eval parameters)))
      (((? string? name) values)
       (list name values))
      ((element rest ...)
       (cons (eval element) (eval rest)))
      (_ (raise (condition
                 (&message (message "Failed to evaluate Cabal file. \
See the manual for limitations.")))))))

  (define (cabal-evaluated-sexp->package evaluated-sexp)
    (let* ((name (lookup-join evaluated-sexp "name"))
           (version (lookup-join evaluated-sexp "version"))
           (revision (lookup-join evaluated-sexp "x-revision"))
           (license (lookup-join evaluated-sexp "license"))
           (home-page (lookup-join evaluated-sexp "homepage"))
           (home-page-or-hackage
            (if (string-null? home-page)
                (string-append "http://hackage.haskell.org/package/" name)
                home-page))
           (source-repository (make-cabal-section evaluated-sexp
                                                  'source-repository))
           (synopsis (lookup-join evaluated-sexp "synopsis"))
           (description (lookup-join evaluated-sexp "description"))
           (executables (make-cabal-section evaluated-sexp 'executable))
           (lib (make-cabal-section evaluated-sexp 'library))
           (test-suites (make-cabal-section evaluated-sexp 'test-suite))
           (flags (make-cabal-section evaluated-sexp 'flag))
           (eval-environment '())
           (custom-setup (match (make-cabal-section evaluated-sexp 'custom-setup)
                           ((x) x)
                           (_ #f))))
      (make-cabal-package name version revision license home-page-or-hackage
                          source-repository synopsis description executables lib
                          test-suites flags eval-environment custom-setup)))

  ((compose cabal-evaluated-sexp->package eval) cabal-sexp))

(define (make-cabal-section sexp section-type)
  "Given an SEXP as produced by 'read-cabal', produce a list of objects
pertaining to SECTION-TYPE sections.  SECTION-TYPE must be one of:
'executable, 'flag, 'test-suite, 'custom-setup, 'source-repository or
'library."
  (filter-map (cut match <>
                   (('section (? (cut equal? <> section-type)) name parameters)
                    (case section-type
                      ((test-suite) (make-cabal-test-suite
                                      name (dependencies parameters)))
                      ((custom-setup) (make-cabal-custom-setup
                                       name (dependencies parameters "setup-depends")))
                      ((executable) (make-cabal-executable
                                      name (dependencies parameters)))
                      ((source-repository) (make-cabal-source-repository
                                            name
                                            (lookup-join parameters "type")
                                            (lookup-join parameters "location")))
                      ((flag)
                       (let* ((default (lookup-join parameters "default"))
                              (default-true-or-false
                                (if (and default (string-ci=? "false" default))
                                    'false
                                    'true))
                              (description (lookup-join parameters "description"))
                              (manual (lookup-join parameters "manual"))
                              (manual-true-or-false
                               (if (and manual (string-ci=? "true" manual))
                                   'true
                                   'false)))
                         (make-cabal-flag name description
                                          default-true-or-false
                                          manual-true-or-false)))
                      (else #f)))
                   (('section (? (cut equal? <> section-type) lib) parameters)
                    (make-cabal-library (dependencies parameters)))
                   (_ #f))
              sexp))

(define* (lookup-join key-values-list key #:optional (delimiter " "))
  "Lookup and joint all values pertaining to keys of value KEY in
KEY-VALUES-LIST.  The optional DELIMITER is used to specify a delimiter string
to be added between the values found in different key/value pairs."
  (string-join 
   (filter-map (cut match <> 
                    (((? (lambda(x) (equal? x key))) value)
                     (string-join value delimiter))
                    (_ #f))
               key-values-list)
   delimiter))

(define dependency-name-version-rx
  (make-regexp "([a-zA-Z0-9_-]+) *(.*)"))

(define* (dependencies key-values-list #:optional (key "build-depends"))
  "Return a list of 'cabal-dependency' objects for the dependencies found in
KEY-VALUES-LIST."
  (let ((deps (string-tokenize (lookup-join key-values-list key ",")
                               (char-set-complement (char-set #\,)))))
    (map (lambda (d)
           (let ((rx-result (regexp-exec dependency-name-version-rx d)))
             (make-cabal-dependency
              (match:substring rx-result 1)
              (match:substring rx-result 2))))
         deps)))

;;; cabal.scm ends here
