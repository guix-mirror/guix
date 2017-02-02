;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix build bournish)
  #:use-module (system base language)
  #:use-module (system base compile)
  #:use-module (system repl command)
  #:use-module (system repl common)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (%bournish-language))

;;; Commentary:
;;;
;;; This is a super minimal Bourne-like shell language for Guile.  It is meant
;;; to be used at the REPL as a rescue shell.  In a way, this is to Guile what
;;; eshell is to Emacs.
;;;
;;; Code:

(define (expand-variable str)
  "Return STR or code to obtain the value of the environment variable STR
refers to."
  ;; XXX: No support for "${VAR}".
  (if (string-prefix? "$" str)
      `(or (getenv ,(string-drop str 1)) "")
      str))

(define* (display-tabulated lst
                             #:key (columns 3)
                             (column-width (/ 78 columns)))
  "Display the list of string LST in COLUMNS columns of COLUMN-WIDTH
characters."
  (define len (length lst))
  (define pad
    (if (zero? (modulo len columns))
        0
        columns))
  (define items-per-column
    (quotient (+ len pad) columns))
  (define items (list->vector lst))

  (let loop ((indexes (unfold (cut >= <> columns)
                              (cut * <> items-per-column)
                              1+
                              0)))
    (unless (>= (first indexes) items-per-column)
      (for-each (lambda (index)
                  (let ((item (if (< index len)
                                  (vector-ref items index)
                                  "")))
                    (display (string-pad-right item column-width))))
                indexes)
      (newline)
      (loop (map 1+ indexes)))))

(define ls-command-implementation
  ;; Run-time support procedure.
  (case-lambda
    (()
     (display-tabulated (scandir ".")))
    (files
     (let ((files (filter (lambda (file)
                            (catch 'system-error
                              (lambda ()
                                (lstat file))
                              (lambda args
                                (let ((errno (system-error-errno args)))
                                  (format (current-error-port) "~a: ~a~%"
                                          file (strerror errno))
                                  #f))))
                          files)))
       (display-tabulated files)))))

(define (ls-command . files)
  `((@@ (guix build bournish) ls-command-implementation) ,@files))

(define (which-command program)
  `(search-path ((@@ (guix build bournish) executable-path))
                ,program))

(define (cat-command file)
  `(call-with-input-file ,file
     (lambda (port)
       ((@ (guix build utils) dump-port) port (current-output-port))
       *unspecified*)))

(define (rm-command . args)
  "Emit code for the 'rm' command."
  (cond ((member "-r" args)
         `(for-each (@ (guix build utils) delete-file-recursively)
                    (list ,@(delete "-r" args))))
        (else
         `(for-each delete-file (list ,@args)))))

(define (lines+chars port)
  "Return the number of lines and number of chars read from PORT."
  (let loop ((lines 0) (chars 0))
    (match (read-char port)
      ((? eof-object?)              ;done!
       (values lines chars))
      (#\newline                    ;recurse
       (loop (1+ lines) (1+ chars)))
      (_                            ;recurse
       (loop lines (1+ chars))))))

(define (file-exists?* file)
  "Like 'file-exists?' but emits a warning if FILE is not accessible."
  (catch 'system-error
    (lambda ()
      (stat file))
    (lambda args
      (let ((errno (system-error-errno args)))
        (format (current-error-port) "~a: ~a~%"
                file (strerror errno))
        #f))))

(define (wc-print file)
  (let-values (((lines chars)
                (call-with-input-file file lines+chars)))
              (format #t "~a ~a ~a~%" lines chars file)))

(define (wc-l-print file)
  (let-values (((lines chars)
                (call-with-input-file file lines+chars)))
              (format #t "~a ~a~%" lines file)))

(define (wc-c-print file)
  (let-values (((lines chars)
                (call-with-input-file file lines+chars)))
              (format #t "~a ~a~%" chars file)))

(define (wc-command-implementation . files)
  (for-each wc-print (filter file-exists?* files)))

(define (wc-l-command-implementation . files)
  (for-each wc-l-print (filter file-exists?* files)))

(define (wc-c-command-implementation . files)
  (for-each wc-c-print (filter file-exists?* files)))

(define (wc-command . args)
  "Emit code for the 'wc' command."
  (cond ((member "-l" args)
         `((@@ (guix build bournish) wc-l-command-implementation)
           ,@(delete "-l" args)))
        ((member "-c" args)
         `((@@ (guix build bournish) wc-c-command-implementation)
           ,@(delete "-c" args)))
        (else
         `((@@ (guix build bournish) wc-command-implementation) ,@args))))

(define (reboot-command . args)
  "Emit code for 'reboot'."
  ;; Normally Bournish is used in the initrd, where 'reboot' is provided
  ;; directly by (guile-user).  In other cases, just bail out.
  `(if (defined? 'reboot)
       (reboot)
       (begin
         (format (current-error-port)
                 "I don't know how to reboot, sorry about that!~%")
         #f)))

(define (help-command . _)
  (display "\
Hello, this is Bournish, a minimal Bourne-like shell in Guile!

The shell is good enough to navigate the file system and run commands but not
much beyond that.  It is meant to be used as a rescue shell in the initial RAM
disk and is probably not very useful apart from that.  It has a few built-in
commands such as 'ls' and 'cd'; it lacks globbing, pipes---everything.\n"))

(define %not-colon (char-set-complement (char-set #\:)))
(define (executable-path)
  "Return the search path for programs as a list."
  (match (getenv "PATH")
    (#f  '())
    (str (string-tokenize str %not-colon))))

(define %commands
  ;; Built-in commands.
  `(("echo"   ,(lambda strings `(list ,@strings)))
    ("cd"     ,(lambda (dir) `(chdir ,dir)))
    ("pwd"    ,(lambda () `(getcwd)))
    ("rm"     ,rm-command)
    ("cp"     ,(lambda (source dest) `(copy-file ,source ,dest)))
    ("help"   ,help-command)
    ("ls"     ,ls-command)
    ("which"  ,which-command)
    ("cat"    ,cat-command)
    ("wc"     ,wc-command)
    ("reboot" ,reboot-command)))

(define (read-bournish port env)
  "Read a Bournish expression from PORT, and return the corresponding Scheme
code as an sexp."
  (match (read-line port)
    ((? eof-object? eof)
     eof)
    ((= string-tokenize (command args ...))
     (match (assoc command %commands)
       ((command proc)                            ;built-in command
        (apply proc (map expand-variable args)))
       (#f
        (let ((command (if (string-prefix? "\\" command)
                           (string-drop command 1)
                           command)))
          `(system* ,command ,@(map expand-variable args))))))))

(define %bournish-language
  (let ((scheme (lookup-language 'scheme)))
    ;; XXX: The 'scheme' language lacks a "joiner", so we add one here.  This
    ;; allows us to have 'read-bournish' read one shell statement at a time
    ;; instead of having to read until EOF.
    (set! (language-joiner scheme)
      (lambda (exps env)
        (match exps
          (()   '(begin))
          ((exp) exp)
          (_    `(begin ,@exps)))))

    (make-language #:name 'bournish
                   #:title "Bournish"

                   ;; The reader does all the heavy lifting.
                   #:reader read-bournish
                   #:compilers `((scheme . ,(lambda (exp env options)
                                              (values exp env env))))
                   #:decompilers '()
                   #:evaluator (language-evaluator scheme)
                   #:printer (language-printer scheme)
                   #:make-default-environment
                   (language-make-default-environment scheme))))

;; XXX: ",L bournish" won't work unless we call our module (language bournish
;; spec), which is kinda annoying, so provide another meta-command.
(define-meta-command ((bournish guix) repl)
  "bournish
Switch to the Bournish language."
  (let ((current (repl-language repl)))
    (format #t "Welcome to ~a, a minimal Bourne-like shell!~%To switch back, type `,L ~a'.\n"
            (language-title %bournish-language)
            (language-name current))
    (current-language %bournish-language)
    (set! (repl-language repl) %bournish-language)))

;;; bournish.scm ends here
