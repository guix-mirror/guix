;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix monad-repl)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (ice-9 pretty-print)
  #:use-module (system repl repl)
  #:use-module (system repl common)
  #:use-module (system repl command)
  #:use-module (system base language)
  #:use-module (system base compile)
  #:use-module (srfi srfi-26)
  #:export (run-in-store
            enter-store-monad))

;;; Comment:
;;;
;;; This modules provides a couple of REPL meta-commands that make it easier
;;; to work with monadic procedures in the store monad.
;;;
;;; Code:

(define* (monad-language monad run #:optional (name 'monad))
  "Return a language with a special evaluator that causes monadic values
 to be \"run\" in MONAD using procedure RUN."
  (let ((scheme (lookup-language 'scheme)))
    (define (evaluate-monadic-expression exp env)
      (let ((mvalue (compile exp #:to 'value #:env env)))
        (run mvalue)))

    (make-language #:name name
                   #:title "Monad"
                   #:reader (language-reader scheme)
                   #:compilers (language-compilers scheme)
                   #:decompilers (language-decompilers scheme)
                   #:evaluator evaluate-monadic-expression
                   #:printer (language-printer scheme)
                   #:make-default-environment
                   (language-make-default-environment scheme))))

(define* (default-guile-derivation store #:optional (system (%current-system)))
  "Return the derivation of the default "
  (package-derivation store (default-guile) system))

(define (store-monad-language)
  "Return a compiler language for the store monad."
  (let* ((store (open-connection))
         (guile (or (%guile-for-build)
                    (default-guile-derivation store))))
    (monad-language %store-monad
                    (cut run-with-store store <>
                         #:guile-for-build guile)
                    'store-monad)))

(define-meta-command ((run-in-store guix) repl (form))
  "run-in-store EXP
Run EXP through the store monad."
  (with-store store
    (let* ((guile (or (%guile-for-build)
                      (default-guile-derivation store)))
           (value (run-with-store store (repl-eval repl form)
                                  #:guile-for-build guile)))
      (run-hook before-print-hook value)
      (pretty-print value))))

(define-meta-command ((enter-store-monad guix) repl)
  "enter-store-monad
Enter a REPL for values in the store monad."
  (let ((new (make-repl (store-monad-language))))
    ;; Force interpretation so that our specially-crafted language evaluator
    ;; is actually used.
    (repl-option-set! new 'interp #t)
    (run-repl new)))

;;; monad-repl.scm ends here
