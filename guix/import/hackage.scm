;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
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

(define-module (guix import hackage)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-1)
  #:use-module ((guix download) #:select (download-to-store))
  #:use-module ((guix utils) #:select (package-name->name+version))
  #:use-module (guix import utils)
  #:use-module (guix store)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module ((guix utils) #:select (call-with-temporary-output-file))
  #:export (hackage->guix-package))

;; Part 1:
;;
;; Functions used to read a Cabal file.

(define ghc-standard-libraries
  ;; List of libraries distributed with ghc (7.8.4). We include GHC itself as
  ;; some packages list it.
  '("ghc"
    "haskell98"
    "hoopl"
    "base"
    "transformers"
    "deepseq"
    "array"
    "binary"
    "bytestring"
    "containers"
    "time"
    "cabal"
    "bin-package-db"
    "ghc-prim"
    "integer-gmp"
    "integer-simple"
    "win32"
    "template-haskell"
    "process"
    "haskeline"
    "terminfo"
    "directory"
    "filepath"
    "old-locale"
    "unix"
    "old-time"
    "pretty"
    "xhtml"
    "hpc"))

(define package-name-prefix "ghc-")

(define key-value-rx
  ;; Regular expression matching "key: value"
  (make-regexp "([a-zA-Z0-9-]+):[ \t]*(\\w?.*)$"))

(define sections-rx
  ;; Regular expression matching a section "head sub-head ..."
  (make-regexp "([a-zA-Z0-9\\(\\)-]+)"))

(define comment-rx
  ;; Regexp matching Cabal comment lines.
  (make-regexp "^ *--"))

(define (has-key? line)
  "Check if LINE includes a key."
  (regexp-exec key-value-rx line))

(define (comment-line? line)
  "Check if LINE is a comment line."
  (regexp-exec comment-rx line))

(define (line-indentation+rest line)
  "Returns two results: The number of indentation spaces and the rest of the
line (without indentation)."
  (let loop ((line-lst (string->list line))
             (count 0))
    ;; Sometimes values are spread over multiple lines and new lines start
    ;; with a comma ',' with the wrong indentation.  See e.g. haddock-api.
    (if (or (null? line-lst)
            (not (or
                  (eqv? (first line-lst) #\space)
                  (eqv? (first line-lst) #\,) ; see, e.g., haddock-api.cabal
                  (eqv? (first line-lst) #\tab))))
        (values count (list->string line-lst))
        (loop (cdr line-lst) (+ count 1)))))

(define (multi-line-value lines seed)
  "Function to read a value split across multiple lines. LINES are the
remaining input lines to be read. SEED is the value read on the same line as
the key.  Return two values: A list with values and the remaining lines to be
processed."
  (define (multi-line-value-with-min-indent lines seed min-indent)
    (if (null? lines)
        (values '() '())
        (let-values (((current-indent value) (line-indentation+rest (first lines)))
                     ((next-line-indent next-line-value)
                      (if (null? (cdr lines))
                          (values #f "")
                          (line-indentation+rest (second lines)))))
          (if (or (not next-line-indent) (< next-line-indent min-indent)
                  (regexp-exec condition-rx next-line-value))
              (values (reverse (cons value seed)) (cdr lines))
              (multi-line-value-with-min-indent (cdr lines) (cons value seed)
                                                min-indent)))))

  (let-values (((current-indent value) (line-indentation+rest (first lines))))
    (multi-line-value-with-min-indent lines seed current-indent)))

(define (read-cabal port)
  "Parses a Cabal file from PORT.  Return a list of list pairs:

(((head1 sub-head1 ... key1) (value))
 ((head2 sub-head2 ... key2) (value2))
 ...).

We try do deduce the Cabal format from the following document:
https://www.haskell.org/cabal/users-guide/developing-packages.html 

Keys are case-insensitive.  We therefore lowercase them.  Values are
case-sensitive.  Currently only indentation-structured files are parsed.
Braces structured files are not handled." ;" <- make emacs happy.
  (define (read-and-trim-line port)
    (let ((line (read-line port)))
      (if (string? line)
          (string-trim-both line #\return)
          line)))

  (define (strip-insignificant-lines port)
    (let loop ((line (read-and-trim-line port))
               (result '()))
      (cond
       ((eof-object? line)
        (reverse result))
       ((or (string-null? line) (comment-line? line))
        (loop (read-and-trim-line port) result))
       (else
        (loop (read-and-trim-line port) (cons line result))))))

  (let loop
      ((lines (strip-insignificant-lines port))
       (indents  '()) ; only includes indents at start of section heads.
       (sections '())
       (result '()))
    (let-values
        (((current-indent line)
          (if (null? lines)
              (values 0 "")
              (line-indentation+rest (first lines))))
         ((next-line-indent next-line)
          (if (or (null? lines) (null? (cdr lines)))
              (values 0 "")
              (line-indentation+rest (second lines)))))
      (if (null? lines)
          (reverse result)
          (let ((rx-result (has-key? line)))
            (cond
             (rx-result
              (let ((key (string-downcase (match:substring rx-result 1)))
                    (value (match:substring rx-result 2)))
                (cond
                 ;; Simple single line "key: value".
                 ((= next-line-indent current-indent)
                  (loop (cdr lines) indents sections
                        (cons
                         (list (reverse (cons key sections)) (list value))
                         result)))
                 ;; Multi line "key: value\n value cont...".
                 ((> next-line-indent current-indent)
                  (let*-values (((value-lst lines)
                                 (multi-line-value (cdr lines)
                                                   (if (string-null? value)
                                                       '()
                                                       `(,value)))))
                    ;; multi-line-value returns to the first line after the
                    ;; multi-value.
                    (loop lines indents sections
                          (cons
                           (list (reverse (cons key sections)) value-lst)
                           result))))
                 ;; Section ended.
                 (else
                  ;; Indentation is reduced. Check by how many levels.
                  (let* ((idx (and=> (list-index
                                      (lambda (x) (= next-line-indent x))
                                      indents)
                                     (cut + <>
                                            (if (has-key? next-line) 1 0))))
                         (sec
                          (if idx
                              (drop sections idx)
                              (raise
                               (condition
                                (&message
                                 (message "unable to parse Cabal file"))))))
                         (ind (drop indents idx)))
                    (loop (cdr lines) ind sec
                          (cons 
                           (list (reverse (cons key sections)) (list value))
                           result)))))))
             ;; Start of a new section.
             ((or (null? indents)
                  (> current-indent (first indents)))
              (loop (cdr lines) (cons current-indent indents)
                    (cons (string-downcase line) sections) result))
             (else
              (loop (cdr lines) indents
                    (cons (string-downcase line) (cdr sections))
                    result))))))))

(define condition-rx
  ;; Regexp for conditionals.
  (make-regexp "^if +(.*)$"))

(define (split-section section)
  "Split SECTION in individual words with exception for the predicate of an
'if' conditional."
  (let ((rx-result (regexp-exec condition-rx section)))
    (if rx-result
        `("if" ,(match:substring rx-result 1))
        (map match:substring (list-matches sections-rx section)))))

(define (join-sections sec1 sec2)
  (fold-right cons sec2 sec1))

(define (pre-process-keys key)
  (match key
    (() '())
    ((sec1 rest ...)
     (join-sections (split-section sec1) (pre-process-keys rest)))))

(define (pre-process-entry-keys entry)
  (match entry
    ((key value)
     (list (pre-process-keys key) value))
    (() '())))

(define (pre-process-entries-keys entries)
  "ENTRIES is a list of list pairs, a keys list and a valules list, as
produced by 'read-cabal'.  Split each element of the keys list into individual
words.  This pre-processing is used to read flags."
  (match entries
    ((entry rest ...)
     (cons (pre-process-entry-keys entry)
           (pre-process-entries-keys rest)))
    (()
     '())))

(define (get-flags pre-processed-entries)
  "PRE-PROCESSED-ENTRIES is a list of list pairs, a keys list and a values
list, as produced by 'read-cabal' and pre-processed by
'pre-process-entries-keys'.  Return a list of pairs with the name of flags and
their default value (one of \"False\" or \"True\") as specified in the Cabal file:

((\"flag1-name\" . \"False-or-True\") ...)." ;" <- make emacs happy
  (match pre-processed-entries
    (() '())
    (((("flag" flag-name "default") (flag-val)) rest ...)
     (cons (cons flag-name  flag-val)
           (get-flags rest)))
    ((entry rest ... )
     (get-flags rest))
    (_ #f)))

;; Part 2:
;;
;; Functions to read information from the Cabal object created by 'read-cabal'
;; and convert Cabal format dependencies conditionals into equivalent
;; S-expressions.

(define tests-rx
  ;; Cabal test keywords
  (make-regexp "(os|arch|flag|impl) *\\(([ a-zA-Z0-9_.<>=-]+)\\)"))

(define parens-rx
  ;; Parentheses within conditions
  (make-regexp "\\((.+)\\)"))

(define or-rx
  ;; OR operator in conditions
  (make-regexp " +\\|\\| +"))

(define and-rx
  ;; AND operator in conditions
  (make-regexp " +&& +"))

(define not-rx
  ;; NOT operator in conditions
  (make-regexp "^!.+"))

(define (bi-op-args str match-lst)
  "Return a list with the arguments of (logic) bianry operators.  MATCH-LST
is the result of 'list-match' against a binary operator regexp on STR."
  (let ((operators (length match-lst)))
    (map (lambda (from to)
           (substring str from to))
         (cons 0 (map match:end match-lst))
         (append (map match:start match-lst) (list (string-length str))))))

(define (bi-op->sexp-like bi-op args)
  "BI-OP is a string with the name of a Scheme operator which in a Cabal file
is represented by a binary operator.  ARGS are the arguments of said operator.
Return a string representing an S-expression of the operator applied to its
arguments."
  (if (= (length args) 1)
      (first args)
      (string-append "(" bi-op
                     (fold (lambda (arg seed) (string-append seed " " arg))
                           "" args) ")")))

(define (not->sexp-like arg)
  "If the string ARG is prefixed by a Cabal negation operator, convert it to
an equivalent Scheme S-expression string."
  (if (regexp-exec not-rx arg)
      (string-append "(not "
                     (substring arg 1 (string-length arg))
                     ")")
      arg))

(define (parens-less-cond->sexp-like conditional)
  "Convert a Cabal CONDITIONAL string into a string with equivalent Scheme
syntax.  This procedure accepts only simple conditionals without parentheses."
  ;; The outher operation is the one with the lowest priority: OR
  (bi-op->sexp-like
   "or"
   ;; each OR argument may be an AND operation
   (map (lambda (or-arg)
          (let ((m-lst (list-matches and-rx or-arg)))
            ;; is there an AND operation?
            (if (> (length m-lst) 0)
                (bi-op->sexp-like
                 "and"
                 ;; expand NOT operators when there are ANDs
                 (map not->sexp-like (bi-op-args or-arg m-lst)))
                ;; ... and when there aren't.
                (not->sexp-like or-arg))))
        ;; list of OR arguments
        (bi-op-args conditional (list-matches or-rx conditional)))))

(define test-keyword-ornament "__")

(define (conditional->sexp-like conditional)
  "Convert a Cabal CONDITIONAL string into a string with equivalent Scheme
syntax."
  ;; First we substitute TEST-KEYWORD-ORNAMENT for parentheses around tests
  ;; keywords so that parentheses are only used to set precedences. This
  ;; substantially simplify parsing.
  (let ((conditional
         (regexp-substitute/global #f tests-rx conditional
                                   'pre 1 test-keyword-ornament 2
                                   test-keyword-ornament 'post)))
    (let loop ((sub-cond conditional))
      (let ((rx-result (regexp-exec parens-rx sub-cond)))
        (cond
         (rx-result
          (parens-less-cond->sexp-like
           (string-append
            (match:prefix rx-result)
            (loop (match:substring rx-result 1))
            (match:suffix rx-result))))
         (else
          (parens-less-cond->sexp-like sub-cond)))))))

(define (eval-flags sexp-like-cond flags)
  "SEXP-LIKE-COND is a string representing an S-expression conditional.  FLAGS
is a list of flag name and value pairs as produced by 'get-flags'.  Substitute
\"#t\" or \"#f\" according to the value of flags. (Default to \"True\")."
  (fold-right
   (lambda (flag sexp)
     (match flag
       ((name . value)
        (let ((rx (make-regexp
                   (string-append "flag" test-keyword-ornament name
                                  test-keyword-ornament))))
          (regexp-substitute/global
           #f rx sexp
           'pre (if (string-ci= value "False") "#f" "#t") 'post)))
       (_ sexp)))
   sexp-like-cond
   (cons '("[a-zA-Z0-9_-]+" . "True") flags)))

(define (eval-tests->sexp sexp-like-cond)
  "In the string SEXP-LIKE-COND substitute test keywords \"os(...)\" and
\"arch(...)\" with equivalent Scheme checks.  Retrun an S-expression."
  (with-input-from-string 
      (fold-right
       (lambda (test sexp)
         (match test
           ((type pre-match post-match)
            (let ((rx (make-regexp
                       (string-append type test-keyword-ornament "(\\w+)"
                                      test-keyword-ornament))))
              (regexp-substitute/global
               #f rx sexp
               'pre pre-match 2 post-match 'post)))
           (_ sexp)))
       sexp-like-cond
       ;; (%current-system) returns, e.g., "x86_64-linux" or "i686-linux".
       '(("(os|arch)" "(string-match \"" "\" (%current-system))")))
    read))

(define (eval-impl sexp-like-cond)
  "Check for the Cabal test \"impl(...)\" in the string SEXP-LIKE-COND.
Assume the module declaring the generated package includes a local variable
called \"haskell-implementation\" with a string value of the form NAME-VERSION
against which we compare."
  (with-output-to-string
    (lambda ()
      (write
       (with-input-from-string 
           (fold-right
            (lambda (test sexp)
              (match test
                ((pre-match post-match)
                 (let ((rx-with-version
                        (make-regexp
                         (string-append
                          "impl" test-keyword-ornament
                          "([a-zA-Z0-9_-]+) *([<>=]+) *([0-9.]+) *"
                          test-keyword-ornament)))
                       (rx-without-version
                        (make-regexp
                         (string-append "impl" test-keyword-ornament "(\\w+)"
                                        test-keyword-ornament))))
                   (if (regexp-exec rx-with-version sexp)
                       (regexp-substitute/global
                        #f rx-with-version sexp
                        'pre pre-match 2 " " post-match " \"" 1 "-" 3 "\")" 'post)
                       (regexp-substitute/global
                        #f rx-without-version sexp
                        'pre pre-match "-match \"" 1 "\" " post-match ")" 'post))))
                (_ sexp)))
            sexp-like-cond
            '(("(string" "haskell-implementation")))
         read)))))

(define (eval-cabal-keywords sexp-like-cond flags)
  ((compose eval-tests->sexp eval-impl (cut eval-flags <> flags))
   sexp-like-cond))

(define (key->values meta key)
  "META is the representation of a Cabal file as produced by 'read-cabal'.
Return the list of values associated with a specific KEY (a string)."
  (match meta
    (() '())
    (((((? (lambda(x) (equal? x key)))) v) r ...)
     v)
    (((k v) r ...)
     (key->values (cdr meta) key))
    (_ "key Not fount")))

(define (key-start-end->entries meta key-start-rx key-end-rx)
  "META is the representation of a Cabal file as produced by 'read-cabal'.
Return all entries whose keys list starts with KEY-START and ends with
KEY-END."
  (let ((pred
         (lambda (x)
           (and (regexp-exec key-start-rx (first x))
                (regexp-exec key-end-rx (last x))))))
           ;; (equal? (list key-start key-end) (list (first x) (last x))))))
    (match meta
      (() '())
      ((((? pred k) v) r ...)
       (cons `(,k ,v)
             (key-start-end->entries (cdr meta) key-start-rx key-end-rx)))
      (((k v) r ...)
       (key-start-end->entries (cdr meta) key-start-rx key-end-rx))
      (_ "key Not fount"))))

(define else-rx
  (make-regexp "^else$"))

(define (count-if-else rx-result-ls)
  (apply + (map (lambda (m) (if m 1 0)) rx-result-ls)))

(define (analyze-entry-cond entry)
  (let* ((keys (first entry))
         (vals (second entry))
         (rx-cond-result
          (map (cut regexp-exec condition-rx <>) keys))
         (rx-else-result
          (map (cut regexp-exec else-rx <>) keys))
         (cond-no (count-if-else rx-cond-result))
         (else-no (count-if-else rx-else-result))
         (cond-idx (list-index (lambda (rx) (if rx #t #f)) rx-cond-result))
         (else-idx (list-index (lambda (rx) (if rx #t #f)) rx-else-result))
         (key-cond
              (cond
               ((or (and cond-idx else-idx (< cond-idx else-idx))
                    (and cond-idx (not else-idx)))
                (match:substring
                 (receive (head tail)
                     (split-at rx-cond-result cond-idx) (first tail))))
               ((or (and cond-idx else-idx (> cond-idx else-idx))
                    (and (not cond-idx) else-idx))
                (match:substring
                 (receive (head tail)
                     (split-at rx-else-result else-idx) (first tail))))
               (else
                ""))))
    (values keys vals rx-cond-result
            rx-else-result cond-no else-no key-cond)))

(define (remove-cond entry cond)
  (match entry
    ((k v)
     (list (cdr (member cond k)) v))))

(define (group-and-reduce-level entries group group-cond)
  (let loop
      ((true-group group)
       (false-group '())
       (entries entries))
    (if (null? entries)
        (values (reverse true-group) (reverse false-group) entries)
        (let*-values (((entry) (first entries))
                      ((keys vals rx-cond-result rx-else-result
                             cond-no else-no key-cond)
                       (analyze-entry-cond entry)))
          (cond
           ((and (>= (+ cond-no else-no) 1) (string= group-cond key-cond))
            (loop (cons (remove-cond entry group-cond) true-group) false-group
                  (cdr entries)))
           ((and (>= (+ cond-no else-no) 1) (string= key-cond "else"))
            (loop true-group (cons (remove-cond entry "else") false-group)
                  (cdr entries)))
           (else
            (values (reverse true-group) (reverse false-group) entries)))))))

(define dependencies-rx
  (make-regexp "([a-zA-Z0-9_-]+) *[^,]*,?"))

(define (hackage-name->package-name name)
  (if (string-prefix? package-name-prefix name)
      (string-downcase name)
      (string-append package-name-prefix (string-downcase name))))

(define (split-and-filter-dependencies ls names-to-filter)
  "Split the comma separated list of dependencies LS coming from the Cabal
file, filter packages included in NAMES-TO-FILTER and return a list with
inputs suitable for the Guix package.  Currently the version information is
discarded."
  (define (split-at-comma-and-filter d)
    (fold
     (lambda (m seed)
       (let* ((name (string-downcase (match:substring m 1)))
              (pkg-name (hackage-name->package-name name)))
         (if (member name names-to-filter)
             seed
             (cons (list pkg-name (list 'unquote (string->symbol pkg-name)))
                   seed))))
     '()
     (list-matches dependencies-rx d)))
    
  (fold (lambda (d p) (append (split-at-comma-and-filter d) p)) '()  ls))

(define* (dependencies-cond->sexp meta #:key (include-test-dependencies? #t))
  "META is the representation of a Cabal file as produced by 'read-cabal'.
Return an S-expression containing the list of dependencies as expected by the
'inputs' field of a package.  The generated S-expressions may include
conditionals as defined in the cabal file.  During this process we discard the
version information of the packages."
  (define (take-dependencies meta)
    (let ((key-start-exe (make-regexp "executable"))
          (key-start-lib (make-regexp "library"))
          (key-start-tests (make-regexp "test-suite"))
          (key-end (make-regexp "build-depends")))
      (append
       (key-start-end->entries meta key-start-exe key-end)
       (key-start-end->entries meta key-start-lib key-end)
       (if include-test-dependencies?
           (key-start-end->entries meta key-start-tests key-end)
           '()))))

  (let ((flags (get-flags (pre-process-entries-keys meta)))
        (augmented-ghc-std-libs (append (key->values meta "name")
                                        ghc-standard-libraries)))
    (delete-duplicates
     (let loop ((entries (take-dependencies meta))
                (result '()))
       (if (null? entries)
           (reverse result)
           (let*-values (((entry) (first entries))
                         ((keys vals rx-cond-result rx-else-result
                                cond-no else-no key-cond)
                          (analyze-entry-cond entry)))
             (cond
              ((= (+ cond-no else-no) 0)
               (loop (cdr entries)
                     (append
                      (split-and-filter-dependencies vals
                                                     augmented-ghc-std-libs)
                      result)))
              (else
               (let-values (((true-group false-group entries)
                             (group-and-reduce-level entries '()
                                                     key-cond))
                            ((cond-final) (eval-cabal-keywords
                                           (conditional->sexp-like
                                            (last (split-section key-cond)))
                                           flags)))
                 (loop entries
                       (cond
                        ((or (eq? cond-final #t) (equal? cond-final '(not #f)))
                         (append (loop true-group '()) result))
                        ((or (eq? cond-final #f) (equal? cond-final '(not #t)))
                         (append (loop false-group '()) result))
                        (else
                         (let ((true-group-result (loop true-group '()))
                               (false-group-result (loop false-group '())))
                           (cond
                            ((and (null? true-group-result)
                                  (null? false-group-result))
                             result)
                            ((null? false-group-result)
                             (cons `(unquote-splicing
                                     (when ,cond-final ,true-group-result))
                                   result))
                            ((null? true-group-result)
                             (cons `(unquote-splicing
                                     (unless ,cond-final ,false-group-result))
                                   result))
                            (else
                             (cons `(unquote-splicing
                                     (if ,cond-final
                                         ,true-group-result
                                         ,false-group-result))
                                   result))))))))))))))))

;; Part 3:
;;
;; Retrive the desired package and its Cabal file from
;; http://hackage.haskell.org and construct the Guix package S-expression.

(define (hackage-fetch name-version)
  "Return the Cabal file for the package NAME-VERSION, or #f on failure.  If
the version part is omitted from the package name, then return the latest
version."
  (let*-values (((name version) (package-name->name+version name-version))
                ((url)
                 (if version
                     (string-append "http://hackage.haskell.org/package/"
                                    name "-" version "/" name ".cabal")
                     (string-append "http://hackage.haskell.org/package/"
                                    name "/" name ".cabal"))))
    (call-with-temporary-output-file
     (lambda (temp port)
       (and (url-fetch url temp)
            (call-with-input-file temp read-cabal))))))

(define string->license
  ;; List of valid values from
  ;; https://www.haskell.org
  ;; /cabal/release/cabal-latest/doc/API/Cabal/Distribution-License.html.
  (match-lambda
   ("GPL-2" 'gpl2)
   ("GPL-3" 'gpl3)
   ("GPL" "'gpl??")
   ("AGPL-3" 'agpl3)
   ("AGPL" "'agpl??")
   ("LGPL-2.1" 'lgpl2.1)
   ("LGPL-3" 'lgpl3)
   ("LGPL" "'lgpl??")
   ("BSD2" 'bsd-2)
   ("BSD3" 'bsd-3)
   ("MIT" 'expat)
   ("ISC" 'isc)
   ("MPL" 'mpl2.0)
   ("Apache-2.0" 'asl2.0)
   ((x) (string->license x))
   ((lst ...) `(list ,@(map string->license lst)))
   (_ #f)))

(define* (hackage-module->sexp meta #:key (include-test-dependencies? #t))
  "Return the `package' S-expression for a Cabal package.  META is the
representation of a Cabal file as produced by 'read-cabal'."

  (define name
    (first (key->values meta "name")))

  (define version
    (first (key->values meta "version")))
  
  (define description
    (let*-values (((description) (key->values meta "description"))
                  ((lines last)
                   (split-at description (- (length description) 1))))
      (fold-right (lambda (line seed) (string-append line "\n" seed))
                  (first last) lines)))
  
  (define source-url
    (string-append "http://hackage.haskell.org/package/" name
                   "/" name "-" version ".tar.gz"))

  ;; Several packages do not have an official home-page other than on Hackage.
  (define home-page
    (let ((home-page-entry (key->values meta "homepage")))
      (if (null? home-page-entry)
          (string-append "http://hackage.haskell.org/package/" name)
          (first home-page-entry))))
  
  (define (maybe-inputs input-type inputs)
    (match inputs
      (()
       '())
      ((inputs ...)
       (list (list input-type
                   (list 'quasiquote inputs))))))
  
  (let ((tarball (with-store store
                   (download-to-store store source-url))))
    `(package
       (name ,(hackage-name->package-name name))
       (version ,version)
       (source (origin
                 (method url-fetch)
                 (uri (string-append ,@(factorize-uri source-url version)))
                 (sha256
                  (base32
                   ,(if tarball
                        (bytevector->nix-base32-string (file-sha256 tarball))
                        "failed to download tar archive")))))
       (build-system haskell-build-system)
       ,@(maybe-inputs 'inputs
                       (dependencies-cond->sexp meta
                                                #:include-test-dependencies?
                                                include-test-dependencies?))
       (home-page ,home-page)
       (synopsis ,@(key->values meta "synopsis"))
       (description ,description)
       (license ,(string->license (key->values meta "license"))))))

(define* (hackage->guix-package module-name
                                #:key (include-test-dependencies? #t))
  "Fetch the Cabal file for PACKAGE-NAME from hackage.haskell.org, and return
the `package' S-expression corresponding to that package, or #f on failure."
  (let ((module-meta (hackage-fetch module-name)))
    (and=> module-meta (cut hackage-module->sexp <>
                            #:include-test-dependencies?
                            include-test-dependencies?))))

;;; cabal.scm ends here
