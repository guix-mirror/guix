;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2021 Julien Lepiller <julien@lepiller.eu>
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

(define-module (guix build po)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:export (read-po-file
            translate-cross-references))

;; A small parser for po files
(define-peg-pattern po-file body (* (or entry whitespace)))
(define-peg-pattern whitespace body (or " " "\t" "\n"))
(define-peg-pattern comment-chr body (range #\space #\頋))
(define-peg-pattern comment none (and "#" (* comment-chr) "\n"))
(define-peg-pattern flags all (and (ignore "#, ") (* comment-chr) (ignore "\n")))
(define-peg-pattern entry all
  (and (* (or flags comment (ignore (* whitespace))))
       (ignore "msgid ") msgid (ignore (* whitespace))
       (ignore "msgstr ") msgstr))
(define-peg-pattern escape body (or "\\\\" "\\\"" "\\n"))
(define-peg-pattern str-chr body (or " " "!" (and (ignore "\\") "\"")
                                     "\\n" (and (ignore "\\") "\\")
                                     (range #\# #\頋)))
(define-peg-pattern msgid all content)
(define-peg-pattern msgstr all content)
(define-peg-pattern content body
  (and (ignore "\"") (* str-chr) (ignore "\"")
       (? (and (ignore (* whitespace)) content))))

(define (interpret-newline-escape str)
  "Replace '\\n' sequences in STR with a newline character."
  (let loop ((str str)
             (result '()))
    (match (string-contains str "\\n")
      (#f (string-concatenate-reverse (cons str result)))
      (index
       (let ((prefix (string-take str index)))
         (loop (string-drop str (+ 2 index))
               (append (list "\n" prefix) result)))))))

(define (parse-tree->assoc parse-tree)
  "Converts a po PARSE-TREE to an association list, where the key is the msgid
and the value is the msgstr.  The result only contains non fuzzy strings."
  (define (comments->flags comments)
    (match comments
      (('flags flags)
       (map (lambda (flag) (string->symbol (string-trim-both flag #\space)))
            (string-split flags #\,)))
      ((? list? comments)
       (fold
         (lambda (comment res)
           (match comment
             ((? string? _) res)
             (flags
              (append (comments->flags flags)
                      res))))
         '()
         comments))))

  (match parse-tree
    (() '())
    ((entry . parse-tree)
     (match entry
       ((? string? entry)
        (parse-tree->assoc parse-tree))
       ;; empty msgid
       (('entry ('msgid ('msgstr msgstr)))
        (parse-tree->assoc parse-tree))
       ;; empty msgstr
       (('entry ('msgid msgid) 'msgstr)
        (parse-tree->assoc parse-tree))
       (('entry _ ('msgid msgid) 'msgstr)
        (parse-tree->assoc parse-tree))
       (('entry ('msgid msgid) ('msgstr msgstr))
        (acons (interpret-newline-escape msgid)
               (interpret-newline-escape msgstr)
               (parse-tree->assoc parse-tree)))
       (('entry ('msgid msgid) ('msgstr msgstr))
        (acons (interpret-newline-escape msgid)
               (interpret-newline-escape msgstr)
               (parse-tree->assoc parse-tree)))
       (('entry comments ('msgid msgid) ('msgstr msgstr))
        (if (member 'fuzzy (comments->flags comments))
            (parse-tree->assoc parse-tree)
            (acons (interpret-newline-escape msgid)
                   (interpret-newline-escape msgstr)
                   (parse-tree->assoc parse-tree))))))))

(define (read-po-file port)
  "Read a .po file from PORT and return an alist of msgid and msgstr."
  (let ((tree (peg:tree (match-pattern
                          po-file
                          (get-string-all port)))))
    (parse-tree->assoc tree)))

(define (canonicalize-whitespace str)
  "Change whitespace (newlines, etc.) in STR to @code{#\\space}."
  (string-map (lambda (chr)
                (if (char-set-contains? char-set:whitespace chr)
                    #\space
                    chr))
              str))

(define xref-regexp
  ;; Texinfo cross-reference regexp.
  (make-regexp "@(px|x)?ref\\{([^,}]+)"))

(define (translate-cross-references texi pofile)
  "Translate the cross-references that appear in @var{texi}, the initial
translation of a Texinfo file, using the msgid/msgstr pairs from @var{pofile}."
  (define translations
    (call-with-input-file pofile read-po-file))

  (define content
    (call-with-input-file texi get-string-all))

  (define matches
    (list-matches xref-regexp content))

  (define translation-map
    (fold (match-lambda*
            (((msgid . str) result)
             (vhash-cons msgid str result)))
          vlist-null
          translations))

  (define translated
    ;; Iterate over MATCHES and replace cross-references with their
    ;; translation found in TRANSLATION-MAP.  (We can't use
    ;; 'substitute*' because matches can span multiple lines.)
    (let loop ((matches matches)
               (offset 0)
               (result '()))
      (match matches
        (()
         (string-concatenate-reverse
          (cons (string-drop content offset) result)))
        ((head . tail)
         (let ((prefix (match:substring head 1))
               (ref    (canonicalize-whitespace (match:substring head 2))))
           (define translated
             (string-append "@" (or prefix "")
                            "ref{"
                            (match (vhash-assoc ref translation-map)
                              (#f ref)
                              ((_ . str) str))))

           (loop tail
                 (match:end head)
                 (append (list translated
                               (string-take
                                (string-drop content offset)
                                (- (match:start head) offset)))
                         result)))))))

  (format (current-error-port)
          "translated ~a cross-references in '~a'~%"
          (length matches) texi)

  (call-with-output-file texi
    (lambda (port)
      (display translated port))))
