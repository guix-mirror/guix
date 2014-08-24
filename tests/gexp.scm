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
  #:use-module (guix tests)
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
  (open-connection-for-tests))

;; For white-box testing.
(define gexp-inputs (@@ (guix gexp) gexp-inputs))
(define gexp-native-inputs (@@ (guix gexp) gexp-native-inputs))
(define gexp->sexp  (@@ (guix gexp) gexp->sexp))

(define* (gexp->sexp* exp #:optional target)
  (run-with-store %store (gexp->sexp exp
                                     #:target target)
                  #:guile-for-build (%guile-for-build)))

(define-syntax-rule (test-assertm name exp)
  (test-assert name
    (run-with-store %store exp
                    #:guile-for-build (%guile-for-build))))


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

(test-assert "ungexp + ungexp-native"
  (let* ((exp    (gexp (list (ungexp-native %bootstrap-guile)
                             (ungexp coreutils)
                             (ungexp-native glibc)
                             (ungexp binutils))))
         (target "mips64el-linux")
         (guile  (derivation->output-path
                  (package-derivation %store %bootstrap-guile)))
         (cu     (derivation->output-path
                  (package-cross-derivation %store coreutils target)))
         (libc   (derivation->output-path
                  (package-derivation %store glibc)))
         (bu     (derivation->output-path
                  (package-cross-derivation %store binutils target))))
    (and (lset= equal?
                `((,%bootstrap-guile "out") (,glibc "out"))
                (gexp-native-inputs exp))
         (lset= equal?
                `((,coreutils "out") (,binutils "out"))
                (gexp-inputs exp))
         (equal? `(list ,guile ,cu ,libc ,bu)
                 (gexp->sexp* exp target)))))

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

(test-assert "input list + ungexp-native"
  (let* ((target "mips64el-linux")
         (exp   (gexp (display
                       (cons '(ungexp-native (list %bootstrap-guile coreutils))
                             '(ungexp (list glibc binutils))))))
         (guile (derivation->output-path
                 (package-derivation %store %bootstrap-guile)))
         (cu    (derivation->output-path
                 (package-derivation %store coreutils)))
         (xlibc (derivation->output-path
                 (package-cross-derivation %store glibc target)))
         (xbu   (derivation->output-path
                 (package-cross-derivation %store binutils target))))
    (and (lset= equal?
                `((,%bootstrap-guile "out") (,coreutils "out"))
                (gexp-native-inputs exp))
         (lset= equal?
                `((,glibc "out") (,binutils "out"))
                (gexp-inputs exp))
         (equal? `(display (cons '(,guile ,cu) '(,xlibc ,xbu)))
                 (gexp->sexp* exp target)))))

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

(test-assert "input list splicing + ungexp-native-splicing"
  (let* ((inputs (list (list glibc "debug") %bootstrap-guile))
         (exp    (gexp (list (ungexp-native-splicing (cons (+ 2 3) inputs))))))
    (and (lset= equal?
                `((,glibc "debug") (,%bootstrap-guile "out"))
                (gexp-native-inputs exp))
         (null? (gexp-inputs exp))
         (equal? (gexp->sexp* exp)                ;native
                 (gexp->sexp* exp "mips64el-linux")))))

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

(test-assertm "gexp->derivation, cross-compilation"
  (mlet* %store-monad ((target -> "mips64el-linux")
                       (exp    -> (gexp (list (ungexp coreutils)
                                              (ungexp output))))
                       (xdrv      (gexp->derivation "foo" exp
                                                    #:target target))
                       (refs      ((store-lift references)
                                   (derivation-file-name xdrv)))
                       (xcu       (package->cross-derivation coreutils
                                                             target))
                       (cu        (package->derivation coreutils)))
    (return (and (member (derivation-file-name xcu) refs)
                 (not (member (derivation-file-name cu) refs))))))

(test-assertm "gexp->derivation, ungexp-native"
  (mlet* %store-monad ((target -> "mips64el-linux")
                       (exp    -> (gexp (list (ungexp-native coreutils)
                                              (ungexp output))))
                       (xdrv      (gexp->derivation "foo" exp
                                                    #:target target))
                       (drv       (gexp->derivation "foo" exp)))
    (return (string=? (derivation-file-name drv)
                      (derivation-file-name xdrv)))))

(test-assertm "gexp->derivation, ungexp + ungexp-native"
  (mlet* %store-monad ((target -> "mips64el-linux")
                       (exp    -> (gexp (list (ungexp-native coreutils)
                                              (ungexp glibc)
                                              (ungexp output))))
                       (xdrv      (gexp->derivation "foo" exp
                                                    #:target target))
                       (refs      ((store-lift references)
                                   (derivation-file-name xdrv)))
                       (xglibc    (package->cross-derivation glibc target))
                       (cu        (package->derivation coreutils)))
    (return (and (member (derivation-file-name cu) refs)
                 (member (derivation-file-name xglibc) refs)))))

(test-assertm "gexp->derivation, ungexp-native + composed gexps"
  (mlet* %store-monad ((target -> "mips64el-linux")
                       (exp0   -> (gexp (list 1 2
                                              (ungexp coreutils))))
                       (exp    -> (gexp (list 0 (ungexp-native exp0))))
                       (xdrv      (gexp->derivation "foo" exp
                                                    #:target target))
                       (drv       (gexp->derivation "foo" exp)))
    (return (string=? (derivation-file-name drv)
                      (derivation-file-name xdrv)))))

(define shebang
  (string-append "#!" (derivation->output-path (%guile-for-build))
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
              (ungexp-splicing (list x y z))
              (ungexp-native foo) (ungexp-native foo "out")
              (ungexp-native (chbouib 42))
              (ungexp-native-splicing (list x y z))))
  '#~(foo #$bar #$baz:out #$(chbouib 42) #$@(list x y z)
          #+foo #+foo:out #+(chbouib 42) #+@(list x y z)))

(test-end "gexp")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;; Local Variables:
;; eval: (put 'test-assertm 'scheme-indent-function 1)
;; End:
