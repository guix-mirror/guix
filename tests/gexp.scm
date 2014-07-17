;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen))

;; Test the (guix gexp) module.

(define %store
  (open-connection))

;; For white-box testing.
(define gexp-inputs (@@ (guix gexp) gexp-inputs))
(define gexp->sexp  (@@ (guix gexp) gexp->sexp))

(define guile-for-build
  (package-derivation %store %bootstrap-guile))

;; Make it the default.
(%guile-for-build guile-for-build)

(define (gexp->sexp* exp)
  (run-with-store %store (gexp->sexp exp)
                  #:guile-for-build guile-for-build))

(define-syntax-rule (test-assertm name exp)
  (test-assert name
    (run-with-store %store exp
                    #:guile-for-build guile-for-build)))


(test-begin "gexp")

(test-equal "no refs"
  '(display "hello!")
  (let ((exp (gexp (display "hello!"))))
    (and (gexp? exp)
         (null? (gexp-inputs exp))
         (gexp->sexp* exp))))

(test-equal "unquote"
  '(display `(foo ,(+ 2 3)))
  (let ((exp (gexp (display `(foo ,(+ 2 3))))))
    (and (gexp? exp)
         (null? (gexp-inputs exp))
         (gexp->sexp* exp))))

(test-assert "one input package"
  (let ((exp (gexp (display (ungexp coreutils)))))
    (and (gexp? exp)
         (match (gexp-inputs exp)
           (((p "out"))
            (eq? p coreutils)))
         (equal? `(display ,(derivation->output-path
                             (package-derivation %store coreutils)))
                 (gexp->sexp* exp)))))

(test-assert "one input origin"
  (let ((exp (gexp (display (ungexp (package-source coreutils))))))
    (and (gexp? exp)
         (match (gexp-inputs exp)
           (((o "out"))
            (eq? o (package-source coreutils))))
         (equal? `(display ,(derivation->output-path
                             (package-source-derivation
                              %store (package-source coreutils))))
                 (gexp->sexp* exp)))))

(test-assert "same input twice"
  (let ((exp (gexp (begin
                     (display (ungexp coreutils))
                     (display (ungexp coreutils))))))
    (and (gexp? exp)
         (match (gexp-inputs exp)
           (((p "out"))
            (eq? p coreutils)))
         (let ((e `(display ,(derivation->output-path
                              (package-derivation %store coreutils)))))
           (equal? `(begin ,e ,e) (gexp->sexp* exp))))))

(test-assert "two input packages, one derivation, one file"
  (let* ((drv (build-expression->derivation
               %store "foo" 'bar
               #:guile-for-build (package-derivation %store %bootstrap-guile)))
         (txt (add-text-to-store %store "foo" "Hello, world!"))
         (exp (gexp (begin
                      (display (ungexp coreutils))
                      (display (ungexp %bootstrap-guile))
                      (display (ungexp drv))
                      (display (ungexp txt))))))
    (define (match-input thing)
      (match-lambda
       ((drv-or-pkg _ ...)
        (eq? thing drv-or-pkg))))

    (and (gexp? exp)
         (= 4 (length (gexp-inputs exp)))
         (every (lambda (input)
                  (find (match-input input) (gexp-inputs exp)))
                (list drv coreutils %bootstrap-guile txt))
         (let ((e0 `(display ,(derivation->output-path
                               (package-derivation %store coreutils))))
               (e1 `(display ,(derivation->output-path
                               (package-derivation %store %bootstrap-guile))))
               (e2 `(display ,(derivation->output-path drv)))
               (e3 `(display ,txt)))
           (equal? `(begin ,e0 ,e1 ,e2 ,e3) (gexp->sexp* exp))))))

(test-assert "input list"
  (let ((exp   (gexp (display
                      '(ungexp (list %bootstrap-guile coreutils)))))
        (guile (derivation->output-path
                (package-derivation %store %bootstrap-guile)))
        (cu    (derivation->output-path
                (package-derivation %store coreutils))))
    (and (lset= equal?
                `((,%bootstrap-guile "out") (,coreutils "out"))
                (gexp-inputs exp))
         (equal? `(display '(,guile ,cu))
                 (gexp->sexp* exp)))))

(test-assert "input list splicing"
  (let* ((inputs  (list (list glibc "debug") %bootstrap-guile))
         (outputs (list (derivation->output-path
                         (package-derivation %store glibc)
                         "debug")
                        (derivation->output-path
                         (package-derivation %store %bootstrap-guile))))
         (exp     (gexp (list (ungexp-splicing (cons (+ 2 3) inputs))))))
    (and (lset= equal?
                `((,glibc "debug") (,%bootstrap-guile "out"))
                (gexp-inputs exp))
         (equal? (gexp->sexp* exp)
                 `(list ,@(cons 5 outputs))))))

(test-assertm "gexp->file"
  (mlet* %store-monad ((exp -> (gexp (display (ungexp %bootstrap-guile))))
                       (guile  (package-file %bootstrap-guile))
                       (sexp   (gexp->sexp exp))
                       (drv    (gexp->file "foo" exp))
                       (out -> (derivation->output-path drv))
                       (done   (built-derivations (list drv)))
                       (refs   ((store-lift references) out)))
    (return (and (equal? sexp (call-with-input-file out read))
                 (equal? (list guile) refs)))))

(test-assertm "gexp->derivation"
  (mlet* %store-monad ((file    (text-file "foo" "Hello, world!"))
                       (exp ->  (gexp
                                 (begin
                                   (mkdir (ungexp output))
                                   (chdir (ungexp output))
                                   (symlink
                                    (string-append (ungexp %bootstrap-guile)
                                                   "/bin/guile")
                                    "foo")
                                   (symlink (ungexp file)
                                            (ungexp output "2nd")))))
                       (drv     (gexp->derivation "foo" exp))
                       (out ->  (derivation->output-path drv))
                       (out2 -> (derivation->output-path drv "2nd"))
                       (done    (built-derivations (list drv)))
                       (refs    ((store-lift references) out))
                       (refs2   ((store-lift references) out2))
                       (guile   (package-file %bootstrap-guile "bin/guile")))
    (return (and (string=? (readlink (string-append out "/foo")) guile)
                 (string=? (readlink out2) file)
                 (equal? refs (list (dirname (dirname guile))))
                 (equal? refs2 (list file))))))

(test-assertm "gexp->derivation, composed gexps"
  (mlet* %store-monad ((exp0 -> (gexp (begin
                                        (mkdir (ungexp output))
                                        (chdir (ungexp output)))))
                       (exp1 -> (gexp (symlink
                                       (string-append (ungexp %bootstrap-guile)
                                                      "/bin/guile")
                                       "foo")))
                       (exp  -> (gexp (begin (ungexp exp0) (ungexp exp1))))
                       (drv     (gexp->derivation "foo" exp))
                       (out ->  (derivation->output-path drv))
                       (done    (built-derivations (list drv)))
                       (guile   (package-file %bootstrap-guile "bin/guile")))
    (return (string=? (readlink (string-append out "/foo"))
                      guile))))

(test-assertm "gexp->derivation, default system"
  ;; The default system should be the one at '>>=' time, not the one at
  ;; invocation time.  See <http://bugs.gnu.org/18002>.
  (let ((system (%current-system))
        (mdrv   (parameterize ((%current-system "foobar64-linux"))
                  (gexp->derivation "foo"
                                    (gexp
                                     (mkdir (ungexp output)))))))
    (mlet %store-monad ((drv mdrv))
      (return (string=? system (derivation-system drv))))))

(define shebang
  (string-append (derivation->output-path guile-for-build)
                 "/bin/guile --no-auto-compile"))

;; If we're going to hit the silly shebang limit (128 chars on Linux-based
;; systems), then skip the following test.
(test-skip (if (> (string-length shebang) 127) 1 0))

(test-assertm "gexp->script"
  (mlet* %store-monad ((n ->   (random (expt 2 50)))
                       (exp -> (gexp
                                (system*
                                 (string-append (ungexp %bootstrap-guile)
                                                "/bin/guile")
                                 "-c" (object->string
                                       '(display (expt (ungexp n) 2))))))
                       (drv    (gexp->script "guile-thing" exp
                                             #:guile %bootstrap-guile))
                       (out -> (derivation->output-path drv))
                       (done   (built-derivations (list drv))))
    (let* ((pipe  (open-input-pipe out))
           (str   (get-string-all pipe)))
      (return (and (zero? (close-pipe pipe))
                   (= (expt n 2) (string->number str)))))))

(test-assert "printer"
  (string-match "^#<gexp \\(string-append .*#<package coreutils.*\
 \"/bin/uname\"\\) [[:xdigit:]]+>$"
                (with-output-to-string
                  (lambda ()
                    (write
                     (gexp (string-append (ungexp coreutils)
                                          "/bin/uname")))))))

(test-assert "printer vs. ungexp-splicing"
  (string-match "^#<gexp .* [[:xdigit:]]+>$"
                (with-output-to-string
                  (lambda ()
                    ;; #~(begin #$@#~())
                    (write
                     (gexp (begin (ungexp-splicing (gexp ())))))))))

(test-equal "sugar"
  '(gexp (foo (ungexp bar) (ungexp baz "out")
              (ungexp (chbouib 42))
              (ungexp-splicing (list x y z))))
  '#~(foo #$bar #$baz:out #$(chbouib 42) #$@(list x y z)))

(test-end "gexp")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;; Local Variables:
;; eval: (put 'test-assertm 'scheme-indent-function 1)
;; End:
