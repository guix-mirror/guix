;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (build-self)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (build))

;;; Commentary:
;;;
;;; When loaded, this module returns a monadic procedure of at least one
;;; argument: the source tree to build.  It returns a derivation that
;;; builds it.
;;;
;;; Code:

;; Use our very own Guix modules.
(eval-when (compile load eval)
  (and=> (assoc-ref (current-source-location) 'filename)
         (lambda (file)
           (let ((dir (string-append (dirname file) "/..")))
             (set! %load-path (cons dir %load-path))))))

(define (date-version-string)
  "Return the current date and hour in UTC timezone, for use as a poor
person's version identifier."
  ;; XXX: Last resort when the Git commit id is missing.
  (date->string (current-date 0) "~Y~m~d.~H"))

(define-syntax parameterize*
  (syntax-rules ()
    "Like 'parameterize' but for regular variables (!)."
    ((_ ((var value) rest ...) body ...)
     (let ((old var)
           (new value))
       (dynamic-wind
         (lambda ()
           (set! var new))
         (lambda ()
           (parameterize* (rest ...) body ...))
         (lambda ()
           (set! var old)))))
    ((_ () body ...)
     (begin body ...))))

(define (pure-load-compiled-path)
  "Return %LOAD-COMPILED-PATH minus the directories containing .go files from
Guix."
  (define (purify path)
    (fold-right delete path
                (filter-map (lambda (file)
                              (and=> (search-path path file) dirname))
                            '("guix.go" "gnu.go"))))

  (let loop ((path %load-compiled-path))
    (let ((next (purify path)))
      (if (equal? next path)
          path
          (loop next)))))

;; The procedure below is our return value.
(define* (build source
                #:key verbose? (version (date-version-string))
                #:allow-other-keys
                #:rest rest)
  "Return a derivation that unpacks SOURCE into STORE and compiles Scheme
files."
  ;; Start by jumping into the target Guix so that we have access to the
  ;; latest packages and APIs.
  ;;
  ;; Our checkout in the store has mtime set to the epoch, and thus .go
  ;; files look newer, even though they may not correspond.
  (parameterize* ((%load-should-auto-compile #f)
                  (%fresh-auto-compile #f)

                  ;; Work around <https://bugs.gnu.org/29226>.
                  (%load-compiled-path (pure-load-compiled-path)))
    ;; FIXME: This is currently too expensive notably because it involves
    ;; compiling a number of the big package files such as perl.scm, which
    ;; takes lots of time and memory as of Guile 2.2.2.
    ;;
    ;; (let ((reload-guix (module-ref (resolve-interface '(guix self))
    ;;                                'reload-guix)))
    ;;   (reload-guix))           ;cross fingers!

    (let ((guix-derivation (module-ref (resolve-interface '(guix self))
                                       'guix-derivation)))
      (guix-derivation source version))))

;; This file is loaded by 'guix pull'; return it the build procedure.
build

;;; build-self.scm ends here
