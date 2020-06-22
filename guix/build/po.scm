;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (ice-9 textual-ports)
  #:export (read-po-file))

;; A small parser for po files
(define-peg-pattern po-file body (* (or comment entry whitespace)))
(define-peg-pattern whitespace body (or " " "\t" "\n"))
(define-peg-pattern comment-chr body (range #\space #\頋))
(define-peg-pattern comment none (and "#" (* comment-chr) "\n"))
(define-peg-pattern entry all
  (and (ignore (* whitespace)) (ignore "msgid ") msgid
       (ignore (* whitespace)) (ignore "msgstr ") msgstr))
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
  "Converts a po PARSE-TREE to an association list."
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
       (('entry ('msgid msgid) ('msgstr msgstr))
        (acons (interpret-newline-escape msgid)
               (interpret-newline-escape msgstr)
               (parse-tree->assoc parse-tree)))))))

(define (read-po-file port)
  "Read a .po file from PORT and return an alist of msgid and msgstr."
  (let ((tree (peg:tree (match-pattern
                          po-file
                          (get-string-all port)))))
    (parse-tree->assoc tree)))
