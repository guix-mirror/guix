;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:export (directory-exists?
            with-directory-excursion
            set-path-environment-variable
            alist-cons-before
            alist-cons-after
            alist-replace
            substitute
            substitute*))


;;;
;;; Directories.
;;;

(define (directory-exists? dir)
  "Return #t if DIR exists and is a directory."
  (let ((s (stat dir #f)))
    (and s
         (eq? 'directory (stat:type s)))))

(define-syntax-rule (with-directory-excursion dir body ...)
  "Run BODY with DIR as the process's current directory."
  (let ((init (getcwd)))
   (dynamic-wind
     (lambda ()
       (chdir dir))
     (lambda ()
       body ...)
     (lambda ()
       (chdir init)))))


;;;
;;; Search paths.
;;;

(define (search-path-as-list sub-directories input-dirs)
  "Return the list of directories among SUB-DIRECTORIES that exist in
INPUT-DIRS.  Example:

  (search-path-as-list '(\"share/emacs/site-lisp\" \"share/emacs/24.1\")
                       (list \"/package1\" \"/package2\" \"/package3\"))
  => (\"/package1/share/emacs/site-lisp\"
      \"/package3/share/emacs/site-lisp\")

"
  (append-map (lambda (input)
                (filter-map (lambda (dir)
                              (let ((dir (string-append input "/"
                                                        dir)))
                                (and (directory-exists? dir)
                                     dir)))
                            sub-directories))
              input-dirs))

(define (list->search-path-as-string lst separator)
  (string-join lst separator))

(define* (set-path-environment-variable env-var sub-directories input-dirs
                                        #:key (separator ":"))
  "Look for each of SUB-DIRECTORIES in INPUT-DIRS.  Set ENV-VAR to a
SEPARATOR-separated path accordingly.  Example:

  (set-path-environment-variable \"PKG_CONFIG\"
                                 '(\"lib/pkgconfig\")
                                 (list package1 package2))
"
  (setenv env-var
          (list->search-path-as-string (search-path-as-list sub-directories
                                                            input-dirs)
                                       separator)))


;;;
;;; Phases.
;;;
;;; In (guix build gnu-build-system), there are separate phases (configure,
;;; build, test, install).  They are represented as a list of name/procedure
;;; pairs.  The following procedures make it easy to change the list of
;;; phases.
;;;

(define* (alist-cons-before reference key value alist
                            #:optional (key=? equal?))
  "Insert the KEY/VALUE pair before the first occurrence of a pair whose key
is REFERENCE in ALIST.  Use KEY=? to compare keys."
  (let-values (((before after)
                (break (match-lambda
                        ((k . _)
                         (key=? k reference)))
                       alist)))
    (append before (alist-cons key value after))))

(define* (alist-cons-after reference key value alist
                           #:optional (key=? equal?))
  "Insert the KEY/VALUE pair after the first occurrence of a pair whose key
is REFERENCE in ALIST.  Use KEY=? to compare keys."
  (let-values (((before after)
                (break (match-lambda
                        ((k . _)
                         (key=? k reference)))
                       alist)))
    (match after
      ((reference after ...)
       (append before (cons* reference `(,key . ,value) after)))
      (()
       (append before `((,key . ,value)))))))

(define* (alist-replace key value alist #:optional (key=? equal?))
  "Replace the first pair in ALIST whose car is KEY with the KEY/VALUE pair.
An error is raised when no such pair exists."
  (let-values (((before after)
                (break (match-lambda
                        ((k . _)
                         (key=? k key)))
                       alist)))
    (match after
      ((_ after ...)
       (append before (alist-cons key value after))))))


;;;
;;; Text substitution (aka. sed).
;;;

(define (substitute file pattern+procs)
  "PATTERN+PROCS is a list of regexp/two-argument procedure.  For each line
of FILE, and for each PATTERN that it matches, call the corresponding PROC
as (PROC MATCH OUTPUT-PORT)."
  (let* ((rx+proc  (map (match-lambda
                         (((? regexp? pattern) . proc)
                          (cons pattern proc))
                         ((pattern . proc)
                          (cons (make-regexp pattern regexp/extended)
                                proc)))
                        pattern+procs))
         (template (string-append file ".XXXXXX"))
         (out      (mkstemp! template)))
    (with-throw-handler #t
      (lambda ()
        (call-with-input-file file
          (lambda (in)
            (let loop ((line (read-line in)))
              (if (eof-object? line)
                  #t
                  (begin
                    (for-each (match-lambda
                               ((regexp . proc)
                                (cond ((regexp-exec regexp line)
                                       =>
                                       (lambda (m)
                                         (proc m out)))
                                      (else
                                       (display line out)
                                       (newline out)))))
                              rx+proc)
                    (loop (read-line in)))))))
        (close out)
        (rename-file template file))
      (lambda (key . args)
        (false-if-exception (delete-file template))))))


(define-syntax let-matches
  ;; Helper macro for `substitute*'.
  (syntax-rules (_)
    ((let-matches index match (_ vars ...) body ...)
     (let-matches (+ 1 index) match (vars ...)
                  body ...))
    ((let-matches index match (var vars ...) body ...)
     (let ((var (match:substring match index)))
       (let-matches (+ 1 index) match (vars ...)
                    body ...)))
    ((let-matches index match () body ...)
     (begin body ...))))

(define-syntax-rule (substitute* file
                                 ((regexp match-var ...) body ...)
                                 ...)
  "Substitute REGEXP in FILE by the string returned by BODY.  BODY is
evaluated with each MATCH-VAR bound to the corresponding positional regexp
sub-expression.  For example:

  (substitute* file
               ((\"hello\")
                \"good morning\\n\")
               ((\"foo([a-z]+)bar(.*)$\" all letters end)
                (string-append \"baz\" letter end)))

Here, anytime a line of FILE contains \"hello\", it is replaced by \"good
morning\".  Anytime a line of FILE matches the second regexp, ALL is bound to
the complete match, LETTERS is bound to the first sub-expression, and END is
bound to the last one.

When one of the MATCH-VAR is `_', no variable is bound to the corresponding
match substring."
  (substitute file
              (list (cons regexp
                          (lambda (m p)
                            (let-matches 0 m (match-var ...)
                                         (display (begin body ...) p))))
                    ...)))


;;; Local Variables:
;;; eval: (put 'call-with-output-file/atomic 'scheme-indent-function 1)
;;; eval: (put 'with-throw-handler 'scheme-indent-function 1)
;;; End:
